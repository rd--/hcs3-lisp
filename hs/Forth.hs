module Forth where

import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Data.Char {- base -}
import Data.Hashable {- hashable -}
import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import System.IO {- base -}
import System.Exit {- base -}

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}

-- | A dictionary is a map of named instructions ('Forth's).
type Dict w a = M.Map String (Forth w a ())

-- | The machine is either interpreting or compiling.
data VM_Mode = Interpret | Compile deriving (Eq,Show)

-- | Function from a word (text) into an instruction.
type Reader w a = String -> Forth w a ()

-- | Class of values that can constitute a 'Forth'.
class Forth_Type a where
    ty_char :: a -> Char -- ^ Single character representaton of /a/.
    ty_string :: a -> String -- ^ String representation of /a/.
    ty_int :: a -> Int -- ^ Coercion, ie. for loop counters.
    ty_from_int :: Int -> a -- ^ Coercion
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @-1@ and @0@.

instance Forth_Type Integer where
    ty_int = fromInteger
    ty_char = toEnum . fromInteger
    ty_string = show
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

-- | A compilation word, for the compilation stack.
data CW w a = CW_Word String | CW_Forth (Forth w a ())

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a =
    VM {stack :: [a] -- ^ The data stack, /the/ stack.
       ,rstack :: [a] -- ^ The return stack.
       ,cstack :: [CW w a] -- ^ The compilation stack.
       ,strings :: M.Map Int String -- ^ The string store, addressed.
       ,threads :: M.Map Int ThreadId
       ,dict :: Dict w a -- ^ The dictionary.
       ,locals :: [Dict w a] -- ^ The stack of locals dictionaries.
       ,buffer :: String -- ^ The current line of input text.
       ,mode :: VM_Mode -- ^ Basic state of the machine.
       ,world :: w -- ^ The world, instance state.
       ,literal :: String -> Maybe a -- ^ Read function for literal values.
       ,dynamic :: Maybe (Reader w a) -- ^ Dynamic post-dictionary lookup.
       ,eol :: Bool -- ^ End of line, runs printer.
       ,input_port :: Maybe Handle
       }

-- | Signals (exceptions) from 'VM'.
data VM_Signal = VM_EOF | VM_No_Input | VM_Error String deriving (Eq,Show)

vm_pp :: Forth_Type a => VM w a -> String
vm_pp vm =
    concat ["\n DATA STACK: ",unwords (map ty_string (stack vm))
           ,"\n RETURN STACK: ",unwords (map ty_string (rstack vm))
           ,"\n COMPILE STACK DEPTH: ",show (length (cstack vm))
           ,"\n STRINGS: ",show (strings vm)
           ,"\n DICT: ",unwords (M.keys (dict vm))
           ,"\n LOCALS: ",intercalate "," (map (unwords . M.keys) (locals vm))
           ,"\n THREADS: ",intercalate "," (map show (M.keys (threads vm)))
           ,"\n MODE: ",show (mode vm)
           ,"\n BUFFER: ",buffer vm]

-- | An instruction, the implementation of a /word/.
type Forth w a r = ExceptT {- String -} VM_Signal (StateT (VM w a) IO) r

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving (Show,Eq)

-- | Tracer, levels are 0 = HIGH, 1 = MEDIUM, 2 = LOW
trace :: Int -> String -> Forth w a ()
trace k msg = when (k < 0) (liftIO (putStrLn msg))

-- | Function with 'VM'.
with_vm :: MonadState a m => (a -> (a,r)) -> m r
with_vm f = get >>= \vm -> let (vm',r) = f vm in put vm' >> return r

-- | Procedure with 'VM'.
do_with_vm :: MonadState a m => (a -> m a) -> m ()
do_with_vm f = get >>= \vm -> f vm >>= put

-- | Pretty print 'Expr'.
expr_pp :: Forth_Type a => Expr a -> String
expr_pp e =
    case e of
      Literal a -> ty_string a
      Word nm -> nm

throw_error :: MonadIO m => String -> ExceptT VM_Signal m a
throw_error = throwError . VM_Error

-- | Reader that raises an /unknown word/ error.
unknown_error :: Reader w a
unknown_error s = throw_error ("unknown word: '" ++ s ++ "'")

-- | Make an empty (initial) machine.
empty_vm :: w -> (String -> Maybe a) -> VM w a
empty_vm w lit =
    VM {stack = []
       ,rstack = []
       ,cstack = []
       ,strings = M.empty
       ,threads = M.empty
       ,buffer = ""
       ,mode = Interpret
       ,dict = M.empty
       ,locals = []
       ,world = w
       ,literal = lit
       ,dynamic = Nothing
       ,eol = False
       ,input_port = Nothing}

-- | Reset 'VM', on error.
vm_reset :: VM w a -> VM w a
vm_reset vm =
    vm {stack = []
       ,rstack = []
       ,cstack = []
       ,buffer = ""
       ,mode = Interpret
       ,locals = []
       ,eol = False}

-- | Push value onto 'stack'.
push :: a -> Forth w a ()
push x = modify (\vm -> vm {stack = x : stack vm})

-- | Push value onto 'rstack'.
pushr :: a -> Forth w a ()
pushr x = modify (\vm -> vm {rstack = x : rstack vm})

-- | Push value onto 'cstack'.
pushc :: CW w a -> Forth w a ()
pushc x = modify (\vm -> vm {cstack = x : cstack vm})

-- | Pop indicated 'VM' stack.
pop_vm_stack :: String -> (VM w a -> [r]) -> (VM w a -> [r] -> VM w a) -> Forth w a r
pop_vm_stack nm f g = do
  vm <- get
  case f vm of
    [] -> throw_error (nm ++ ": stack underflow")
    x:xs -> put (g vm xs) >> return x

-- | Remove value from 'stack'.
pop :: Forth w a a
pop = pop_vm_stack "DATA" stack (\vm s -> vm {stack = s})

-- | Remove value from 'rstack'.
popr :: Forth w a a
popr = pop_vm_stack "RETURN" rstack (\vm s -> vm {rstack = s})

-- | Remove value from 'cstack'.
popc :: Forth w a (CW w a)
popc = pop_vm_stack "COMPILE" cstack (\vm s -> vm {cstack = s})

-- | Change the world.
modify_world :: (w -> w) -> Forth w a ()
modify_world f = modify (\vm -> vm {world = f (world vm)})

next_string_id :: VM w a -> Int
next_string_id vm =
    let m = strings vm
    in if M.null m then 0 else fst (M.findMax m) + 1

get_string :: Forth_Type a => Forth w a (Maybe String)
get_string = do
  _ <- pop
  k <- fmap ty_int pop
  vm <- get
  return (M.lookup k (strings vm))

-- | Read until /x/ is seen, discarding /x/, RHS may be @[]@.
--
-- > break_on ')' "comment ) WORD" == ("comment "," WORD")
break_on :: Eq a => a -> [a] -> ([a],[a])
break_on x l =
    case break (== x) l of
      (lhs,[]) -> (lhs,[])
      (lhs,_ : rhs) -> (lhs,rhs)

scan_until :: Forth_Type a => Char -> Forth w a String
scan_until c = do
  vm <- get
  let (str,buffer') = break_on c (buffer vm)
  put vm {buffer = buffer'}
  return str

-- | 'snd' of 'break_on'.
delete_until :: Eq a => a -> [a] -> [a]
delete_until x = snd . break_on x

-- | Scan a token from 'buffer', ANS Forth type comments are
-- discarded.  Although 'buffer' is filled by 'hGetLine' it may
-- contain newline characters because we may include a file.
scan_token :: Forth w a (Maybe String)
scan_token = do
  vm <- get
  case buffer vm of
    [] -> return Nothing
    str ->
        case break isSpace (dropWhile isSpace str) of
          ("\\",r) -> put vm {buffer = delete_until '\n' r} >> scan_token
          ("(",r) -> put vm {buffer = delete_until ')' r} >> scan_token
          (e,r) -> put vm {buffer = r,eol = null r} >>
                   if null e then scan_token else return (Just e)

-- | Read line from 'input_port' to 'buffer'.  There are two
-- /exceptions/ thrown here, 'VM_EOF' if an input port is given but
-- returns EOF, and 'VM_No_Input' if there is no input port.
fw_refill :: Forth w a ()
fw_refill = do
  vm <- get
  case input_port vm of
    Nothing -> throwError VM_No_Input
    Just h -> do
      eof <- liftIO (hIsEOF h)
      when eof (throwError VM_EOF)
      x <- liftIO (hGetLine h)
      put (vm {buffer = x,eol = True})

-- | If 'scan_token' is 'Nothing', then 'fw_refill' and retry.
read_token :: Forth w a String
read_token = do
  r <- scan_token
  case r of
    Just str -> return str
    Nothing -> fw_refill >> read_token

-- | Dictionary lookup.
lookup_word :: String -> VM w a -> Maybe (Forth w a ())
lookup_word k vm =
    case locals vm of
      [] -> M.lookup k (dict vm)
      l:_ -> case M.lookup k l of
               Nothing -> M.lookup k (dict vm)
               r -> r

-- | Parse a token string to an expression.
parse_token :: String -> Forth w a (Expr a)
parse_token s = do
  vm <- get
  case lookup_word s vm of
    Just _  -> return (Word s)
    Nothing ->
        case literal vm s of
          Just l  -> return (Literal l)
          Nothing ->
              case dynamic vm of
                Just _ -> return (Word s) -- if there is an dynamic reader, defer...
                Nothing -> throw_error ("unknown word: '" ++ s ++ "'")

-- | 'parse_token' of 'read_token'.
read_expr :: Forth w a (Expr a)
read_expr = parse_token =<< read_token

-- | 'lookup_word' in the dictionary, if unknown try 'dynamic'.
interpret_word :: String -> Forth w a ()
interpret_word w = do
  vm <- get
  case lookup_word w vm of
    Just r -> r
    Nothing ->
        case dynamic vm of
          Just f -> f w
          Nothing -> throw_error ("unknown word: '" ++ w ++ "'")

-- | Either 'interpret_word' or 'push' literal.
interpret_expr :: Expr a -> Forth w a ()
interpret_expr e =
    case e of
      Word w -> interpret_word w
      Literal a -> push a

-- | 'interpret_expr' of 'read_expr'.
interpret :: Forth w a ()
interpret = read_expr >>= interpret_expr

-- | A loop ends when the two elements at the top of the rstack are equal.
loop_end :: Eq a => Forth w a Bool
loop_end = do
  vm <- get
  case rstack vm of
    p:q:_ -> return (p == q)
    _ -> throw_error "loop_end: illegal rstack"

-- | /code/ is the expressions between @do@ and @loop@.
interpret_do :: (Forth_Type a,Eq a) => Forth w a () -> Forth w a ()
interpret_do code = do
  start <- pop
  end <- pop
  pushr end
  pushr start
  let step = do
        code
        i <- popr
        let i' = ty_from_int (ty_int i + 1)
        pushr i'
  let loop = do
        r <- loop_end
        if not r then step >> loop else popr >> popr >> return ()
  loop

-- | Get instruction at 'CW' or raise an error.
cw_instr :: CW w a -> Forth w a ()
cw_instr cw =
    case cw of
      CW_Word w -> throw_error ("cw_instr: WORD: " ++ w)
      CW_Forth f -> f

-- | foldl1 of '>>'.
forth_block :: [Forth w a ()] -> Forth w a ()
forth_block = foldl1 (>>)

-- | Add a 'locals' frame.
begin_locals :: Forth w a ()
begin_locals = with_vm (\vm -> (vm {locals = M.empty : locals vm},()))

-- | Remove a 'locals' frame.
end_locals :: Forth w a ()
end_locals = with_vm (\vm -> (vm {locals = tail (locals vm)},()))

bracketed :: (a, a) -> [a] -> [a]
bracketed (l,r) x = l : x ++ [r]

-- | Compile ';' statement.  There is always a compile 'locals' frame to be removed.
end_compilation :: Forth w a ()
end_compilation = do
  vm <- get
  case reverse (cstack vm) of
    CW_Word nm : cw ->
        let instr = (map cw_instr cw)
            instr' = if M.null (head (locals vm))
                     then instr
                     else bracketed (begin_locals,end_locals) instr
            w = forth_block instr'
        in do trace 2 ("END DEFINITION: " ++ nm)
              put (vm {cstack = []
                      ,locals = tail (locals vm)
                      ,dict = M.insert nm w (dict vm)
                      ,mode = Interpret})
    _ -> throw_error "CSTACK"
  return ()

-- | Predicate to see if 'CW' is a particular 'CW_Word'.
cw_is_word :: String -> CW w a -> Bool
cw_is_word w cw =
    case cw of
      CW_Word w' -> w == w'
      _ -> False

-- | Unwind the 'cstack' to the indicated control word.  The result is
-- the code block, in sequence.  The control word is also removed from
-- the cstack.
unwind_cstack_to :: String -> Forth w a [CW w a]
unwind_cstack_to w = do
  with_vm (\vm -> let (r,c) = break (cw_is_word w) (cstack vm)
                  in (vm {cstack = tail c},reverse r))

-- | Compile @loop@ statement, end of do block.
end_do :: (Eq a,Forth_Type a) => Forth w a ()
end_do = do
  cw <- unwind_cstack_to "do"
  let w = forth_block (map cw_instr cw)
  pushc (CW_Forth (interpret_do w))

-- | Consult stack and select either /true/ or /false/.
interpret_if :: (Eq a,Forth_Type a) => (Forth w a (),Forth w a ()) -> Forth w a ()
interpret_if (t,f) = pop >>= \x -> if x /= ty_from_bool False then t else f

-- | Compile @then@ statement, end of @if@ block.
end_if :: (Eq a,Forth_Type a) => Forth w a ()
end_if = do
  cw <- unwind_cstack_to "if"
  let f = forth_block . map cw_instr
  case break (cw_is_word "else") cw of
    (tb,[]) -> pushc (CW_Forth (interpret_if (f tb,return ())))
    (tb,fb) -> pushc (CW_Forth (interpret_if (f tb,f (tail fb))))

-- | Function over current locals 'Dict'.
at_current_locals :: (Dict w a -> Dict w a) -> VM w a -> VM w a
at_current_locals f vm =
    case locals vm of
      l : l' -> vm {locals = f l : l'}
      _ -> error "at_current_locals"

-- | 'locals' is used both during compilation and interpretation.  In
-- compilation the RHS is undefined, it is used for name lookup and to
-- know if an interpreter 'locals' frame must be made.  In
-- interpretation, if required, it is a secondary dictionary,
-- consulted first.
def_locals :: Forth_Type a => Forth w a ()
def_locals = do
  let get_names r = do
               w <- read_token
               if w == "}" then return r else get_names (w : r)
  nm <- get_names []
  trace 0 ("DEFINE-LOCALS: " ++ intercalate " " nm)
  let locals' = M.fromList (zip nm (repeat undefined))
  with_vm (\vm -> (at_current_locals (M.union locals') vm,()))
  pushc (CW_Forth (forth_block (map fw_local' nm)))

compile_s_quote :: Forth_Type a => Forth w a ()
compile_s_quote = do
  str <- scan_until '"'
  vm <- get
  let k = next_string_id vm
  put vm {strings = M.insert k str (strings vm)}
  pushc (CW_Forth (push (ty_from_int k) >> push (ty_from_int (length str))))

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
compile :: (Eq a,Forth_Type a) => Forth w a ()
compile = do
  expr <- read_expr
  trace 2 ("COMPILE: " ++ expr_pp expr)
  case expr of
    Word ";" -> end_compilation
    Word ":" -> throw_error ": IN COMPILE CONTEXT"
    Word "do" -> pushc (CW_Word "do")
    Word "i" -> pushc (CW_Forth fw_i)
    Word "j" -> pushc (CW_Forth fw_j)
    Word "loop" -> end_do
    Word "if" -> pushc (CW_Word "if")
    Word "else" -> pushc (CW_Word "else")
    Word "then" -> end_if
    Word "{" -> def_locals
    Word "s\"" -> compile_s_quote
    e -> pushc (CW_Forth (interpret_expr e))

-- | Either 'interpret' or 'compile', depending on 'mode'.
execute :: (Eq a,Forth_Type a) => Forth w a ()
execute = do
  vm <- get
  case mode vm of
    Interpret -> interpret
    Compile -> compile

-- * Primitives

-- | Unary stack operation.
unary_op :: (a -> a) -> Forth w a ()
unary_op f = pop >>= push . f

-- | Binary stack operation.  The first value on the stack is the RHS.
binary_op :: (a -> a -> a) -> Forth w a ()
binary_op f = pop >>= \y -> pop >>= \x -> push (f x y)

-- | 'binary_op', /rep/ translates the result so it can be placed onto the stack.
comparison_op :: Forth_Type a => (a -> a -> Bool) -> Forth w a ()
comparison_op f = binary_op (\x y -> ty_from_bool (f x y))

-- | Put string and then space.
put_str_sp :: String -> IO ()
put_str_sp s = putStr s >> putChar ' '

-- * Forth words

-- | Variant on @(local)@, argument not on stack.
fw_local' :: String -> Forth w a ()
fw_local' nm = do
  vm <- get
  case stack vm of
    e : s' -> put vm {stack = s'
                     ,locals = case locals vm of
                                 [] -> error "NO LOCALS FRAME"
                                 l : l' -> M.insert nm (push e) l : l'}
    _ -> throw_error ("(LOCAL): STACK UNDERFLOW: " ++ nm)

execute_buffer :: (Forth_Type a, Eq a) => VM w a -> IO (VM w a)
execute_buffer vm = do
  (r,vm') <- runStateT (runExceptT execute) vm
  case r of
    Left err -> case err of
                  VM_No_Input -> return vm'
                  _ -> error (show err)
    Right () -> execute_buffer vm'

fw_evaluate' :: (Eq a,Forth_Type a) => String -> Forth w a ()
fw_evaluate' str = do
  vm <- get
  let buf = buffer vm
      ip = input_port vm
  vm' <- liftIO (execute_buffer (vm {buffer = str, input_port = Nothing}))
  put (vm' {buffer = buf, input_port = ip})

-- | Variant on @included@, argument not on stack.
fw_included' :: (Eq a,Forth_Type a) => FilePath -> Forth w a ()
fw_included' nm = liftIO (readFile nm) >>= fw_evaluate'

fw_included :: (Eq a,Forth_Type a) => Forth w a ()
fw_included = do
  r <- get_string
  case r of
    Nothing -> throw_error "INCLUDED"
    Just str -> fw_included' str

fw_i :: Forth w a ()
fw_i = popr >>= \x -> pushr x >> push x

-- | Forth word @j@.
fw_j :: Forth w a ()
fw_j = do {x <- popr; y <- popr; z <- popr
          ;pushr z; pushr y; pushr x
          ;push z}

-- | Enter compile phase, the word name is pushed onto the /empty/
-- 'cstack', and a 'locals' frame is added.
fw_colon :: Forth w a ()
fw_colon = do
  nm <- read_token
  let reserved = M.keys (core_dict :: Dict w Integer)
  trace 0 ("DEFINE: " ++ nm)
  let edit vm = do
        when (nm `elem` reserved) (throw_error ("':' RESERVED NAME: " ++ nm))
        when (not (null (cstack vm))) (throw_error ("':' CSTACK NOT EMPTY: " ++ nm))
        return (vm {mode = Compile
                   ,cstack = [CW_Word nm]
                   ,locals = M.empty : locals vm})
  do_with_vm edit

-- | Forth word @/mod@.
fw_div_mod :: Integral a => Forth w a ()
fw_div_mod = pop >>= \p -> pop >>= \q -> let (r,s) = q `divMod` p in push s >> push r

-- | dup:( p -- p p ) swap:( p q -- q p ) drop:( p -- ) over:( p q -- p q p )
fw_dup,fw_swap,fw_drop,fw_over,fw_rot,fw_2dup :: Forth w a ()
fw_dup = pop >>= \e -> push e >> push e
fw_swap = pop >>= \p -> pop >>= \q -> push p >> push q
fw_drop = pop >> return ()
fw_over = pop >>= \p -> pop >>= \q -> push q >> push p >> push q
fw_rot = pop >>= \p -> pop >>= \q -> pop >>= \r -> push q >> push p >> push r
fw_2dup = pop >>= \p -> pop >>= \q -> push q >> push p >> push q >> push p

-- | ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fw_pick :: Forth_Type a => Forth w a ()
fw_pick = do
  vm <- get
  case stack vm of
    n : s' -> let n' = ty_int n
                  e = s' !! n'
              in put vm {stack = e : s'}
    _ -> throw_error "PICK"

fw_emit,fw_dot :: Forth_Type a => Forth w a ()
fw_emit = liftIO . putChar . ty_char =<< pop
fw_dot = liftIO . put_str_sp . ty_string =<< pop

fw_dot_s :: Forth_Type a => Forth w a ()
fw_dot_s = do
  vm <- get
  let l = map ty_string (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  liftIO (putStr n >> mapM_ put_str_sp l)

fw_bye :: Forth w a ()
fw_bye = liftIO exitSuccess

fw_s_quote_interpet :: Forth_Type a => Forth w a ()
fw_s_quote_interpet = do
  vm <- get
  let (str,buffer') = break_on '"' (buffer vm)
  put vm {stack = ty_from_int (length str) : ty_from_int (-1) : stack vm
         ,strings = M.insert (-1) str (strings vm)
         ,buffer = buffer'}

fw_type :: Forth_Type a => Forth w a ()
fw_type = do
  r <- get_string
  case r of
    Nothing -> throw_error "TYPE: UNKNOWN STRING-ID"
    Just str -> liftIO (put_str_sp str)

fw_vmstat :: Forth_Type a => Forth w a ()
fw_vmstat = get >>= liftIO . putStrLn . vm_pp

fw_fork :: Forth_Type a => Forth w a ()
fw_fork = do
  nm <- read_token
  vm <- get
  case lookup_word nm vm of
    Just fw -> do th <- liftIO (forkIO (exec_err vm fw >> return ()))
                  let k = hash th :: Int
                  put vm {stack = ty_from_int k : stack vm
                         ,threads = M.insert k th (threads vm)}
    Nothing -> throw_error ("FORK: UNKNOWN WORD: " ++ nm)

fw_kill :: Forth_Type a => Forth w a ()
fw_kill = do
  k <- pop
  vm <- get
  let k' = ty_int k
      threads' = threads vm
  case M.lookup k' threads' of
    Nothing -> throw_error ("KILL: UNKNOWN THREAD: " ++ show k')
    Just th -> liftIO (killThread th) >> put vm {threads = M.delete k' threads'}

fw_kill_all :: Forth w a ()
fw_kill_all = do
  vm <- get
  let th = M.elems (threads vm)
  liftIO (mapM_ killThread th)
  put vm {threads = M.empty}

-- * Dictionaries

-- | 'Num' instance words.
num_dict :: Num n => Dict w n
num_dict = M.fromList
    [("+",binary_op (+))
    ,("*",binary_op (*))
    ,("-",binary_op (-))
    ,("negate",unary_op negate)
    ,("abs",unary_op abs)]

integral_dict :: Integral n => Dict w n
integral_dict = M.fromList
    [("mod",binary_op mod)
    ,("/",binary_op div)
    ,("/mod",fw_div_mod)]

ord_dict :: (Forth_Type a,Ord a) => Dict w a
ord_dict = M.fromList
    [("=",comparison_op (==))
    ,("<",comparison_op (<))
    ,("<=",comparison_op (<=))
    ,(">",comparison_op (>))
    ,(">=",comparison_op (>=))]

core_dict :: (Eq a,Forth_Type a) => Dict w a
core_dict =
    let err nm = throw_error (concat ["'",nm,"': compiler word in interpeter context"])
    in M.fromList
    [(":",fw_colon)
    ,(";",err ";")
    ,("s\"",fw_s_quote_interpet)
    ,("included",fw_included)
    ,("type",fw_type)
    ,("do",err "do")
    ,("i",err "i")
    ,("j",err "j")
    ,("loop",err "loop")
    ,("if",err "if")
    ,("else",err "else")
    ,("then",err "then")
    ,("{",err "{")
    ,("}",err "}")
    ,("fork",fw_fork)
    ,("kill",fw_kill)
    ,("killall",fw_kill_all)
    ,("bye",fw_bye)
    -- STACK
    ,("drop",fw_drop)
    ,("dup",fw_dup)
    ,("over",fw_over)
    ,("pick",fw_pick)
    ,("rot",fw_rot)
    ,("swap",fw_swap)
    ,("2dup",fw_2dup)
     -- IO
    ,("emit",fw_emit)
    ,(".",fw_dot)
    ,(".s",fw_dot_s)
    ,("key",liftIO getChar >>= \c -> push (ty_from_int (fromEnum c)))
    -- DEBUG
    ,("vmstat",fw_vmstat)]

-- * Operation

exec_err :: VM w a -> Forth w a () -> IO (VM w a)
exec_err vm fw = do
  (r,vm') <- runStateT (runExceptT fw) vm
  case r of
    Left err -> error (show err)
    Right () -> return vm'

-- | Read, evaluate, print, loop.  Prints @OK@ at end of line.  Prints
-- error message and runs 'vm_reset' on error.
repl :: (Eq a,Forth_Type a) => VM w a -> IO ()
repl vm = do
  (r,vm') <- runStateT (runExceptT execute) vm
  case r of
    Left err -> case err of
                  VM_EOF -> putStrLn "BYE" >> liftIO exitSuccess
                  VM_No_Input -> liftIO exitSuccess
                  VM_Error msg -> putStrLn (" ERROR: " ++ msg) >> repl (vm_reset vm)
    Right () -> when (eol vm') (putStrLn " OK") >> repl vm'

load_files :: (Eq a,Forth_Type a) => [String] -> VM w a -> IO (VM w a)
load_files nm vm =
    case nm of
      [] -> return vm
      f : nm' -> exec_err vm (fw_included' f) >>= load_files nm'
