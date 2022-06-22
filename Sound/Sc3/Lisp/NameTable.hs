module Sound.Sc3.Lisp.NameTable where

-- | Name table.
type NameTable = [(String, String)]

-- | Load table given name re-writing rules, one per line.
nameTableLoad :: FilePath -> IO NameTable
nameTableLoad fn = do
  txt <- readFile fn
  let parse x = case words x of
                  [lhs,rhs] -> (lhs,rhs)
                  _ -> error ("nameTableLoad: " ++ x)
  return (map parse (lines txt))

