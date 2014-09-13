; WHY SUPERCOLLIDER (JMCC) #0

(DEFINE WHY-SUPERCOLLIDER?
  (LET ((R (RESONZ (MUL (DUST AR 0.2) 50) (RAND 200 3200) 0.003))
        (S (MIX (CLONE 10 R)))
        (Z (DELAY-N S 0.048 0.048))
        (C (COMB-L Z 0.1 (MUL-ADD (LF-NOISE1 KR (RAND 0 0.1)) 0.04 0.05) 15))
        (Y (MIX (CLONE 7 C)))
        (F (LAMBDA (I) (ALLPASS-N I 0.05 (CLONE 2 (RAND 0 0.05)) 1)))
        (X ((FOLDL1 COMPOSE (REPLICATE 4 F)) Y)))
    (ADD S (MUL X 0.2))))

(HEAR WHY-SUPERCOLLIDER?)

;; ANALOG BUBBLES (JMCC) #1

(DEFINE ANALOG-BUBBLES
  (LET* ((O (MUL-ADD (LF-SAW KR (MCE2 8 7.23) 0) 3 80))
         (F (MUL-ADD (LF-SAW KR 0.4 0) 24 O))
         (S (MUL (SIN-OSC AR (MIDI-CPS F) 0) 0.04)))
    (COMB-N S 0.2 0.2 4)))

(HEAR ANALOG-BUBBLES)

;; LFO MODULATION (JMCC) #1

(DEFINE LFO-MODULATION
  (LET* ((O (MUL-ADD (F-SIN-OSC KR 0.05 0) 80 160))
         (P (MUL-ADD (F-SIN-OSC KR (MCE2 0.6 0.7) 0) 3600 4000))
         (S (RLPF (MUL (LF-PULSE AR O 0 0.4) 0.05) P 0.2)))
    (COMB-L S 0.3 (MCE2 0.2 0.25) 2)))

(HEAR LFO-MODULATION)

;; HELL IS BUSY (JMCC) #1

(DEFINE HELL-IS-BUSY
  (LET ((O (F-SIN-OSC AR (ADD 400 (RAND 0 2000)) 0))
        (A (MUL (LF-PULSE KR (ADD 1 (RAND 0 10.0)) 0 (RAND 0 0.7)) 0.04)))
    (PAN2 (MUL O A) (RAND -1 1) 1)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 4 8 +INF.0) HELL-IS-BUSY))

;; POND LIFE (JMCC) #1

(DEFINE POND-LIFE
  (LET* ((F0 (ADD 20 (RAND 0 30)))
         (F1 (MUL-ADD (F-SIN-OSC KR F0 0) (RAND 100 400) (LIN-RAND 500 2500 0)))
         (A (MUL (LF-PULSE KR (FDIV 3 (RAND 1 9)) 0 (RAND 0.2 0.5)) 0.04)))
    (PAN2 (SIN-OSC AR F1 0) (RAND -1 1) A)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 8 8 4 +INF.0) POND-LIFE))

;; ALIEN FROGGIES (JMCC) #1

(DEFINE ALIEN-FROGGIES
  (LAMBDA (R)
    (LET* ((R* (FOLD (MUL R (U:EXP (LIN-RAND -0.2 0.2 0))) 1 30))
           (O (FORMANT AR R* (EXP-RAND 200 3000) (MUL-ADD (RAND 0 9) R* R*))))
      (MUL O 0.05))))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 0.25 0.5 5 +INF.0) (ALIEN-FROGGIES 11)))

;; RANDOM SINE WAVES (JMCC) #1

(DEFINE RANDOM-SINE-WAVES
  (PAN2 (F-SIN-OSC AR (RAND 0 2000) 0) (RAND -1 1) 0.02))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 2 5 12 +INF.0) RANDOM-SINE-WAVES))

;; RANDOM PULSATIONS (JMCC) #1

(DEFINE RANDOM-PULSATIONS
  (LET* ((E (ENV-LINEN 2 5 2 0.02 (LIST)))
         (O1 (MUL (F-SIN-OSC AR (RAND 0 2000) 0) (ENV-GEN KR 1 1 0 1 DO-NOTHING E)))
         (O2 (SIN-OSC AR (LIN-RAND 8 88 0) 0))
         (O3 (MUL (SIN-OSC KR (RAND 0.3 0.8) (RAND 0 TWO-PI)) 0.7)))
    (PAN2 (AM-CLIP O1 O2) O3 1)))

(WITH-SC3 (SPAWN-U (LIST (/ 9 8) +INF.0) RANDOM-PULSATIONS))

;; MOTO REV (JMCC) #1

(DEFINE MOTO-REV
  (LET* ((F (MUL-ADD (SIN-OSC KR 0.2 0) 10 21))
         (S (LF-PULSE AR F (MCE2 0 0.1) 0.1)))
    (CLIP2 (RLPF S 100 0.1) 0.4)))

(HEAR MOTO-REV)

;; SCRATCHY (JMCC) #1

(DEFINE SCRATCHY
  (LET* ((N (MUL (CLONE 2 (BROWN-NOISE AR)) 0.5))
         (F (MUL (U:MAX (SUB N 0.49) 0) 20)))
    (RHPF F 5000 1)))

(HEAR SCRATCHY)

;; TREMULATE (JMCC) #1

(DEFINE TREMULATE
  (LET* ((F (RAND 500 900))
         (O (F-SIN-OSC AR (MUL F (MAKE-MCE '(1 1.2 1.5 1.8))) 0))
         (R (CLONE 4 (RAND 30 90)))
         (A (U:MAX 0 (MUL (LF-NOISE2 KR R) 0.1)))
         (L (CLONE 4 (RAND -1 1))))
    (MIX (PAN2 O L A))))

(DEFINE TREMULATE-PP
  (LAMBDA (I)
    (COMB-N I 0.1 0.1 1)))

(WITH-SC3*
 (LIST
  (POST-PROCESS-U 2 TREMULATE-PP)
  (XFADE-TEXTURE-U (LIST 0.5 2 +INF.0) TREMULATE)))

;; RESO-PULSE (JMCC) #1

(DEFINE RESO-PULSE
  (LET* ((F (MIDI-CPS (L-CHOOSE (LIST 25 30 34 37 41 42 46 49 53 54 58 61 63 66))))
         (F* (MUL-ADD 2 F (RAND2 0.5))))
    (MUL (ADD (LF-PULSE AR F 0 0.2) (LF-PULSE AR F* 0 0.2)) 0.02)))

(DEFINE RESO-PULSE-PP
  (LAMBDA (Z)
    (LET* ((LFO-FREQ 6)
           (LFO (MUL-ADD (LF-NOISE0 KR LFO-FREQ) 1000 1200))
           (X (MOUSE-X KR 0.2 0.02 EXPONENTIAL 0.2))
           (LEFT (RLPF Z LFO X))
           (DELAY-TIME (FDIV 2 LFO-FREQ))
           (RIGHT (DELAY-N LEFT DELAY-TIME DELAY-TIME)))
      (MCE2 LEFT RIGHT))))

(WITH-SC3*
 (LIST
  (POST-PROCESS-U 1 RESO-PULSE-PP)
  (OVERLAP-TEXTURE-U (LIST 4 2 4 +INF.0) RESO-PULSE)))

;; SPRINKLER (JMCC) #1

(DEFINE SPRINKLER
  (LET* ((F (MUL-ADD (LF-PULSE KR 0.09 0 0.16) 10 7))
         (T (MUL (LF-PULSE KR F 0 0.25) 0.1)))
    (BPZ2 (MUL (WHITE-NOISE AR) T))))

(HEAR SPRINKLER)

;; SPRINKLER MOUSE (JMCC) #1

(DEFINE SPRINKLER-MOUSE
  (LET* ((F (MOUSE-X KR 0.2 50 LINEAR 0.2))
         (T (MUL (LF-PULSE KR F 0 0.25) 0.1)))
    (BPZ2 (MUL (WHITE-NOISE AR) T))))

(HEAR SPRINKLER-MOUSE)

;; HARMONIC SWIMMING (JMCC) #1

(DEFINE HARMONIC-SWIMMING
  (LET* ((A 0.02)
         (F 50)
         (P 20)
         (Z 0)
         (L (LINE KR 0 (NEG A) 60 0))
         (O (LAMBDA (H)
              (LET* ((R (CLONE 2 (RAND 2 8)))
                     (N (LF-NOISE1 KR R))
                     (E (U:MAX 0 (MUL-ADD N A L))))
                (MUL (F-SIN-OSC AR (* F (+ H 1)) 0) E)))))
    (MIX-FILL P O)))

(HEAR HARMONIC-SWIMMING)

;; HARMONIC TUMBLING (JMCC) #1

(DEFINE HARMONIC-TUMBLING
  (LET* ((F 80)
         (P 10)
         (T (X-LINE KR (MCE2 10 11) 0.1 60 0))
         (O (LAMBDA (H)
              (LET* ((N (DUST KR T))
                     (R (RAND 0.25 0.5))
                     (E (DECAY2 (MUL N 0.02) 0.005 R)))
                (MUL (F-SIN-OSC AR (* F (+ H 1)) 0) E)))))
    (MIX-FILL P O)))

(HEAR HARMONIC-TUMBLING)

;; RAILS (JMCC) #2

(DEFINE RAILS
  (LET* ((N 20) ; RESONANT MODES
         (E (MUL (DUST AR 100) 0.04)) ; EXCITATION
         (F (X-LINE KR 3000 300 8 DO-NOTHING)) ; SWEEP FILTER DOWN
         (L (LINE KR (RAND2 1) (RAND2 1) 8 DO-NOTHING)) ; SWEEP PAN
         (R (CLONE N (ADD 200 (LIN-RAND 0 3000 0)))) ; RESONANT FREQUENCIES
         (A (MAKE-MCE (REPLICATE N 1)))
         (T (CLONE N (ADD 0.2 (RAND 0 1)))) ; RING TIMES
         (K (KLANK (RESONZ E F 0.2) 1 0 1 (KLANK-DATA-MCE R A T))))
    (PAN2 K L 1)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 3 2 4 +INF.0) RAILS))

;; BOUNCING OBJECTS (JMCC) #2

(DEFINE BOUNCING-OBJECTS
  (LET* ((IMP-FRQ (X-LINE KR (ADD 5 (RAND -2 2)) 600 4 DO-NOTHING))
         (IMP-AMP (X-LINE KR 0.09 0.000009 4 DO-NOTHING))
         (IMP (MUL (IMPULSE AR IMP-FRQ 0) IMP-AMP))
         (EXC (DECAY IMP 0.001))
         (FLT-FRQ (CLONE 4 (RAND 400 8400)))
         (FLT-AMP (CLONE 4 (RAND 0 1)))
         (FLT-RTM (CLONE 4 (RAND 0.01 0.11)))
         (FLT (KLANK EXC 1 0 1 (KLANK-DATA-MCE FLT-FRQ FLT-AMP FLT-RTM)))
         (LOC (PAN2 FLT (RAND -1 1) 1))
         (E (ENV '(1 1 0) '(3 0.001) (REPLICATE 2 'LINEAR) -1 -1)))
    (MUL LOC (ENV-GEN KR 1 1 0 1 REMOVE-SYNTH E))))

(WITH-SC3
 (SPAWN-U (LIST 0.6 +INF.0) (DELAY-N BOUNCING-OBJECTS 1 (RAND 0 1))))

;; LOTS-O-SINES (JMCC) #2

(DEFINE LOTS-O-SINES
  (LET* ((N 60)
         (Z (KLANG-DATA (REPLICATE-M N (LIN-RAND 40 10040 0))
                        (REPLICATE N 1)
                        (REPLICATE N 0)))
         (K (CLONE 2 (KLANG AR 1 0 Z))))
    (MUL K (FDIV 0.1 N))))

(WITH-SC3 (XFADE-TEXTURE-U (LIST 4 4 +INF.0) LOTS-O-SINES))

;; CLUSTERED SINES (JMCC) #2

(DEFINE CLUSTERED-SINES
  (LET* ((N 80)
         (F1 (RAND 100 1100))
         (F2 (MUL 4 F1))
         (Y (REPLICATE-M N (ADD F1 (RAND 0 F2))))
         (Z (KLANG-DATA Y (MAP (LAMBDA (E) (FDIV F1 E)) Y) (REPLICATE N 0)))
         (K (CLONE 2 (KLANG AR 1 0 Z))))
    (MUL K (FDIV 0.3 N))))

(WITH-SC3 (XFADE-TEXTURE-U (LIST 4 4 +INF.0) CLUSTERED-SINES))

;; RESONATORS HARMONIC SERIES (JMCC) #2

(DEFINE RESONATORS-HARMONIC-SERIES
  (LET* ((P 12)
         (NOISE (MUL (BROWN-NOISE AR) 0.001))
         (RAT (LIST 1.0 1.125 1.25 1.333 1.5 1.667 1.875 2.0 2.25 2.5 2.667 3.0 3.333 3.75 4.0))
         (FREQ (MUL (L-CHOOSE RAT) 120))
         (RES-FREQS (ZIP-WITH ADD ((SERIES* ADD) P FREQ FREQ) (REPLICATE-M P (RAND2 0.5))))
         (SPEC (KLANK-DATA
                RES-FREQS
                (MAP (LAMBDA (I) (FDIV 1 (ADD I 1))) (ENUM-FROM-TO 0 (- P 1)))
                (REPLICATE-M P (RAND 0.5 4.5)))))
    (CLONE 2 (KLANK NOISE 1 0 1 SPEC))))

(WITH-SC3 (XFADE-TEXTURE-U (LIST 1 7 +INF.0) RESONATORS-HARMONIC-SERIES))

;; SWEPT RESONANT NOISE (JMCC) #2

(DEFINE SRN
  (LET* ((P 10)
         (N (MUL (WHITE-NOISE AR) 0.007))
         (F (MIDI-CPS (MUL-ADD (F-SIN-OSC KR (RAND 0.1 0.3) 0) (RAND 0 24) (RAND 36 84))))
         (SWEEP (RESONZ N F 0.1))
         (SPEC-F (LAMBDA ()
                   (KLANK-DATA-MCE (LIN-RAND-N P 80 10080 0)
                                   (MAKE-MCE (REPLICATE P 1))
                                   (RAND-N P 0.5 2.5))))
         (SPEC (REPLICATE-M 2 SPEC-F)))
    (MAKE-MCE (REPLICATE-M 2 (KLANK SWEEP 1 0 1 (SPEC-F))))))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 4 5 +INF.0) SRN))

;; COOLANT (JMCC) #2

(DEFINE COOLANT
  (LET* ((P 10)
         (N (REPLICATE P 1))
         (SP (MCE2 (KLANK-DATA (REPLICATE-M P (RAND 40 2400)) N N)
                   (KLANK-DATA (REPLICATE-M P (RAND 40 2400)) N N)))
         (S (ONE-POLE (MUL (CLONE 2 (BROWN-NOISE AR)) 0.002) 0.95)))
    (KLANK S 1 0 1 (MCE-TRANSPOSE SP))))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 4 2 +INF.0) COOLANT))

;; PULSING BOTTLES (JMCC) #2

(DEFINE U
  (LAMBDA (_)
    (LET* ((N (WHITE-NOISE AR))
           (R0 (RAND 4 14))
           (R1 (RAND 0 0.7))
           (R2 (RAND 400 7400))
           (R (RESONZ (MUL3 N (LF-PULSE KR R0 0 0.25) R1) R2 0.01))
           (F (RAND 0.1 0.5))
           (P (RAND 0 (MUL PI 2)))
           (S (SIN-OSC KR F P)))
      (PAN2 R S 1))))

(DEFINE PULSING-BOTTLES (MIX-FILL 6 U))

(HEAR PULSING-BOTTLES)

;; WHAT WAS I THINKING? (JMCC) #2

(DEFINE WHAT-WAS-I-THINKING
  (LET* ((N0 (MUL-ADD (LF-NOISE1 KR 0.2) 2000 2400))
         (N1 (MUL-ADD (LF-NOISE1 KR 0.157) 0.4 0.5))
         (I (MUL3 (LF-PULSE AR 0.1 0 0.05) (IMPULSE AR 8 0) 500))
         (D (DECAY I 2))
         (F (U:MAX (ADD (SIN-OSC KR 4 0) 80) D))
         (P (MUL (PULSE AR F N1) 0.04))
         (Z (RLPF P N0 0.2))
         (C (LAMBDA (X)
              (LET* ((R (RAND 0 0.3))
                     (N (MUL-ADD (LF-NOISE1 KR R) 0.025 0.035)))
                (COMB-L X 0.06 N 1))))
         (Y (MUL Z 0.6)))
    (ADD Z (MCE2 (ADD (C Y) (C Y))
                 (ADD (C Y) (C Y))))))

(HEAR WHAT-WAS-I-THINKING)

;; NARROW BAND FILTERED CRACKLE NOISE (JMCC) #2

(DEFINE NBFCN
  (LET* ((E (ENV-LINEN 2 5 2 1 (LIST)))
         (RF1 (ADD (RAND 0 2000) 80))
         (RF2 (MUL-ADD (RAND2 0.2) RF1 RF1))
         (RF (X-LINE KR RF1 RF2 9 DO-NOTHING))
         (C (MUL (ADD (CRACKLE AR 1.97) (RAND 0 0.03)) 0.15)))
    (PAN2 (RESONZ C RF 0.2) (RAND2 1) (ENV-GEN AR 1 1 0 1 REMOVE-SYNTH E))))

(WITH-SC3 (SPAWN-U (LIST 2 +INF.0) NBFCN))

;; RESONANT DUST (JMCC) #2

(DEFINE RESONANT-DUST
  (LET* ((RF (LET* ((ST (RAND 80 2080))
                    (EN (ADD ST (MUL (RAND -0.5 0.5) ST))))
               (X-LINE KR ST EN 9 DO-NOTHING)))
         (D (MUL (DUST AR (RAND 50 850)) 0.3)))
    (PAN2 (RESONZ D RF 0.1) (RAND -1 1) 1)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 5 2 9 +INF.0) RESONANT-DUST))

;; POLICE STATE (JMCC) #2

(DEFINE NODE
  (LAMBDA (_)
    (LET* ((R0 (RAND 0.02 0.1))
           (R1 (RAND 0 (* PI 2)))
           (R2 (RAND 0 600))
           (R3 (RAND 700 1300))
           (R4 (RAND -1 1))
           (R5 (RAND 80 120))
           (N0 (LF-NOISE2 AR R5))
           (F (MUL-ADD (SIN-OSC KR R0 R1) R2 R3)))
      (PAN2 (MUL (SIN-OSC AR F 0) N0) R4 0.1))))

(DEFINE POLICE-STATE
  (LET* ((N0 (LF-NOISE2 KR (MCE2 0.4 0.4)))
         (N1 (LF-NOISE2 AR (MUL-ADD N0 90 620)))
         (N2 (LF-NOISE2 KR (MCE2 0.3 0.3)))
         (E (MUL N1 (MUL-ADD N2 0.15 0.18))))
    (COMB-L (ADD (MIX-FILL 4 NODE) E) 0.3 0.3 3)))

(HEAR POLICE-STATE)

;; UPLINK (JMCC) #2

(DEFINE UPLINK
  (LET* ((R (LAMBDA (N) (RAND 0 N)))
         (P0 (LF-PULSE KR (R 20) 0 (R 1)))
         (P1 (MUL-ADD (LF-PULSE KR (R 4) 0 (R 1)) (R 8000) (R 2000)))
         (F (MIX (CLONE 2 (MUL P0 P1)))))
    (PAN2 (MUL (LF-PULSE AR F 0 0.5) 0.04) (RAND -0.8 0.8) 1)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 1 5 +INF.0) UPLINK))

;; DATA SPACE (JMCC) #2

(DEFINE DATA-SPACE
  (LET* ((R (LAMBDA (N) (RAND 0 N)))
         (LP (LAMBDA (F M A) (MUL-ADD (LF-PULSE KR (R F) 0 (R 1)) M A)))
         (P0 (LP 200 1 0))
         (P1 (LP 40 (R 8000) (R 2000)))
         (P2 (LP 20 1 0))
         (P3 (LP 4 (R 8000) (R 2000)))
         (P4 (LP 20 1 0))
         (P5 (LP 4 (R 8000) (R 2000)))
         (F (ADD3 (MUL P0 P1) (MUL P2 P3) (MUL P4 P5)))
         (DT (RAND 0.15 0.35))
         (O (MUL (LF-PULSE AR F 0 0.5) 0.04))
         (L (MUL (LF-NOISE0 KR (R 3)) 0.8)))
    (COMB-L (PAN2 O L 1) DT DT 3)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 1 6 4 +INF.0) DATA-SPACE))

;; CYMBALISM (JMCC) #2

(DEFINE CYMBALISM
  (LET* ((P 15)
         (F1 (RAND 500 2500))
         (F2 (RAND 0 8000))
         (Y (LET ((F (REPLICATE-M P (ADD F1 (RAND 0 F2))))
                  (RT (REPLICATE-M P (RAND 1 5)))
                  (A (REPLICATE P 1)))
              (KLANK-DATA F A RT)))
         (Z (CLONE 2 Y))
         (T (IMPULSE AR (RAND 0.5 3.5) 0))
         (N (MUL (WHITE-NOISE AR) 0.03))
         (S (MUL (DECAY T 0.004) N)))
    (KLANK S 1 0 1 (MCE-TRANSPOSE Z))))

(WITH-SC3 (XFADE-TEXTURE-U (LIST 4 4 +INF.0) CYMBALISM))

;; CYMBALISM ACCELERANDO (JMCC) #2

(DEFINE CYMBALISM-ACCELERANDO
  (LET* ((P 15)
         (F1 (RAND 500 2500))
         (F2 (RAND 0 8000))
         (Y (LET ((F (REPLICATE-M P (ADD F1 (RAND 0 F2))))
                  (RT (REPLICATE-M P (RAND 1 5)))
                  (A (REPLICATE P 1)))
              (KLANK-DATA F A RT)))
         (Z (CLONE 2 Y))
         (TF (X-LINE KR (LIN-RAND 0.5 4.5 0) (RAND 0.5 35.5) 12 DO-NOTHING))
         (T (IMPULSE AR TF 0))
         (N (MUL (WHITE-NOISE AR) 0.02))
         (S (MUL (DECAY T 0.004) N)))
    (KLANK S 1 0 1 (MCE-TRANSPOSE Z))))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 4 4 +INF.0) CYMBALISM-ACCELERANDO))

;; RING MODULATED KLANK (JMCC) #2

(DEFINE RMK
  (LET* ((P 8)
         (Z (KLANK-DATA-MCE (RAND-N P 100 10000)
                            (CLONE P 1)
                            (RAND-N P 0.2 1)))
         (K (KLANK (MUL (DUST AR 20) 0.02) 1 0 1 Z))
         (F (MUL-ADD (LF-NOISE2 KR (RAND 0.1 0.4)) 200 (RAND 350 400))))
    (PAN2 (MUL (SIN-OSC AR F 0) K) (RAND -1 1) 1)))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 4 4 +INF.0) RMK))

;; ANALOGUE DAZE (JMCC) #3

(DEFINE ANALOGUE-DAZE
  (LET ((PATTERN (LIST 55 63 60 63 57 65 62 65))
        (SEQU (LAMBDA (S TR) (DEMAND TR 0 (DSEQ DINF (MAKE-MCE S)))))
        (F (LAMBDA (OCTAVE CLOCKRATE PWM-RATE FLT-RATE)
             (LET ((TRG (IMPULSE KR CLOCKRATE 0))
                   (PATTERN* (MAP (COMPOSE MIDI-CPS (ADD (MUL 12 OCTAVE))) PATTERN))
                   (SQ (SEQU PATTERN* TRG))
                   (PWM (MUL-ADD (SIN-OSC KR PWM-RATE (RAND 0 TWO-PI)) 0.4 0.5))
                   (CF (MUL-ADD (SIN-OSC KR FLT-RATE (RAND 0 TWO-PI)) 1400 2000)))
               (RLPF (MUL (LF-PULSE AR (LAG SQ 0.05) 0 PWM) 0.1) CF (FDIV 1 15)))))
        (A (MUL (LF-NOISE0 AR (MUL-ADD (LF-NOISE1 KR 0.3) 6000 8000)) (MCE2 0.07 0.08)))
        (X (MUL (DECAY (IMPULSE AR 2 0) 0.15) A))
        (G (ADD (MCE2 (F 1 8 0.31 0.2) (F 0 2 0.13 0.11)) X))
        (Z (MUL 0.4 (ADD (COMB-N G 0.375 0.375 5) (MCE-REVERSE G))))
        (E (ENV-LINEN 2 56 2 1 NIL)))
    (MUL Z (ENV-GEN KR 1 1 0 1 REMOVE-SYNTH E))))

(HEAR ANALOGUE-DAZE)

;; SYNTHETIC PIANO (JMCC) #3

(DEFINE SP
  (LAMBDA (_)
    (LET* ((N (I-RAND 36 90))
           (F (RAND 0.1 0.5))
           (PH (RAND 0 1))
           (S (MUL (IMPULSE AR F PH) 0.1))
           (E (DECAY2 S 0.008 0.04))
           (C
            (LAMBDA (I)
              (LET* ((N0 (LF-NOISE2 AR 3000))
                     (O (LIST-REF (LIST -0.05 0 0.04) I))
                     (DT (FDIV 1 (MIDI-CPS (ADD N O)))))
                (COMB-L (MUL N0 E) DT DT 6))))
           (L (SUB (FDIV (SUB N 36) 27) 1)))
      (PAN2 (MIX-FILL 3 C) L 1))))

(HEAR (MIX-FILL 6 SP))

;; REVERBERATED SINE PERCUSSION (JMCC) #3

(DEFINE RALLPASS
  (LAMBDA (I)
    (ALLPASS-N I 0.050 (CLONE 3 (RAND 0 0.05)) 1)))

(DEFINE CHAIN-OF
  (LAMBDA (N F)
    (FOLDL1 COMPOSE (REPLICATE N F))))

(DEFINE REVERBERATED-SINE-PERCUSSION
  (LET* ((D 10)
         (C 7)
         (A 4)
         (S (MIX-FILL
             D
             (LAMBDA (_)
               (RESONZ (MUL (DUST AR (/ 2 D)) 50)
                       (+ 200 (RAND 0 3000))
                       0.003))))
         (Z (DELAY-N S 0.048 0.48))
         (Y (MIX (COMB-L Z
                         0.1
                         (MUL-ADD (LF-NOISE1 KR (CLONE C (RAND 0 0.1)))
                                  0.04
                                  0.05)
                         15)))
         (X ((CHAIN-OF A RALLPASS) Y)))
    (ADD S (MUL 0.2 X))))

(HEAR REVERBERATED-SINE-PERCUSSION)

;; REVERBERATED NOISE BURSTS (JMCC) #3

(DEFINE USEQ (LAMBDA (N F) (FOLDL1 COMPOSE (REPLICATE N F))))

(DEFINE RNB
  (LET* ((S (MUL (DECAY (MUL (DUST AR 0.6) 0.2) 0.15) (PINK-NOISE AR)))
         (Z (DELAY-N S 0.048 0.048))
         (Y (MIX (COMB-L Z 0.1 (MUL-ADD (LF-NOISE1 KR (CLONE 6 (RAND 0 0.1))) 0.04 0.05) 15)))
         (F (USEQ 4 (LAMBDA (I) (ALLPASS-N I 0.050 (CLONE 2 (RAND 0 0.05)) 1)))))
    (ADD S (F Y))))

(HEAR RNB)

;; ANALOG BUBBLES {MOUSE} (JMCC) #1

(DEFINE ANALOG-BUBBLES
  (LET* ((Y (MOUSE-Y KR 0.1 10 EXPONENTIAL 0.2))
         (X (MOUSE-X KR 2 40 EXPONENTIAL 0.2))
         (O (MUL-ADD (LF-SAW KR X 0) -3 80))
         (F (MUL-ADD (LF-SAW KR Y 0) 24 O))
         (S (MUL (SIN-OSC AR (MIDI-CPS F) 0) 0.04)))
    (COMB-N S 0.2 0.2 4)))

(HEAR ANALOG-BUBBLES)

;; BERLIN 1977 (JMCC) #4

(DEFINE SEQU (LAMBDA (S TR) (DEMAND TR 0 (DSEQ DINF (MAKE-MCE S)))))

(DEFINE SEQU-R (LAMBDA (S TR) (DEMAND TR 0 (DSHUF DINF (MAKE-MCE S)))))

(DEFINE BERLIN-1977
  (LET* ((CLOCK-RATE (MOUSE-X KR 5 20 EXPONENTIAL 0.2))
         (CLOCK-TIME (FDIV 1 CLOCK-RATE))
         (CLOCK (IMPULSE KR CLOCK-RATE 0))
         (PATTERN-LIST (LIST 55 60 63 62 60 67 63 58))
         (NOTE (SEQU PATTERN-LIST CLOCK))
         (CLOCK-16 (PULSE-DIVIDER CLOCK 16 0))
         (NOTE* (ADD (SEQU-R (LIST -12 -7 -5 0 2 5) CLOCK-16) NOTE))
         (FREQ (MIDI-CPS NOTE*))
         (ENV (DECAY2 CLOCK (MUL 0.05 CLOCK-TIME) (MUL 2 CLOCK-TIME)))
         (AMP (MUL-ADD ENV 0.1 0.02))
         (FILT (MUL-ADD ENV (MUL (F-SIN-OSC KR 0.17 0) 800) 1400))
         (PW (MUL-ADD (SIN-OSC KR 0.08 (MCE2 0 HALF-PI)) 0.45 0.5))
         (S (MUL (PULSE AR FREQ PW) AMP)))
    (COMB-N (RLPF S FILT 0.15) 0.2 (MCE2 0.2 0.17) 1.5)))

(HEAR BERLIN-1977)

;; SAMPLE AND HOLD LIQUIDITIES (JMCC) #2

(DEFINE SAMPLE-AND-HOLD-LIQUIDITIES
  (LET* ((R (MOUSE-X KR 1 200 1 0.1))
         (T (RECIP R))
         (C (MUL (IMPULSE KR R 0) 0.4))
         (CF (MOUSE-Y KR 100 8000 1 0.1))
         (F (LATCH (MUL-ADD (WHITE-NOISE KR) (MUL CF 0.5) CF) C))
         (P (LATCH (WHITE-NOISE KR) C))
         (E (DECAY2 C (MUL 0.1 T) (MUL 0.9 T)))
         (I (PAN2 (MUL (SIN-OSC AR F 0) E) P 1)))
    (COMB-N I 0.3 0.3 2)))

(HEAR SAMPLE-AND-HOLD-LIQUIDITIES)

;; SWEEPY NOISE (JMCC) #6

(DEFINE SWEEPY-NOISE
  (LET* ((N (CLONE 2 (WHITE-NOISE AR)))
         (LFO-DEPTH (MOUSE-Y KR 200 8000 1 0.2))
         (LFO-RATE (MOUSE-X KR 4 60 1 0.2))
         (FREQ (MUL-ADD (LF-SAW KR LFO-RATE 0)
                        LFO-DEPTH
                        (MUL LFO-DEPTH 1.2)))
         (FILTERED (RLPF (MUL N 0.03) FREQ 0.1)))
    (ADD (COMB-N FILTERED 0.3 0.3 2) FILTERED)))

(HEAR SWEEPY-NOISE)

;; NOISE BURST SWEEP (JMCC) #6

(DEFINE NOISE-BURST-SWEEP
  (LET* ((N (CLONE 2 (WHITE-NOISE AR)))
         (LFO-RATE (MOUSE-X KR 10 60 1 0.2))
         (AMP (U:MAX 0 (LF-SAW KR LFO-RATE -1)))
         (CFREQ (MOUSE-Y KR 400 8000 1 0.2))
         (FREQ (MUL-ADD (SIN-OSC KR 0.2 0) CFREQ (MUL 1.05 CFREQ))))
    (RESONZ (MUL N AMP) FREQ 0.1)))

(HEAR NOISE-BURST-SWEEP)

;; ALIEN MEADOW (JMCC) #6

(DEFINE ALIEN-MEADOW
  (LET* ((A (RAND 0 20))
         (B (RAND 0 5000))
         (C (RAND 0 20))
         (P (RAND -1 1))
         (F (ADD (MUL3 (SIN-OSC AR A 0) B 0.1) B)))
    (PAN2 (SIN-OSC AR F 0) P (MUL-ADD (SIN-OSC AR C 0) 0.05 0.05))))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 2 6 6 +INF.0) ALIEN-MEADOW))

;; HARD SYNC SAWTOOTH WITH LFO (JMCC) #6

(DEFINE HSSWL
  (LET* ((F (MIDI-CPS (ADD 30 (I-RAND 0 50))))
         (O (MUL-ADD (SIN-OSC KR 0.2 (MCE2 0 (RAND 0 (* 2 PI)))) (MUL 2 F) (MUL 3 F))))
    (MUL (SYNC-SAW AR (MCE2 F (ADD F 0.2)) O) 0.05)))

(DEFINE HSSWL-PP
  (LAMBDA (Z)
    (ADD (COMB-N Z 0.3 0.3 4) (MCE-REVERSE Z))))

(WITH-SC3*
 (LIST
  (POST-PROCESS-U 2 HSSWL-PP)
  (OVERLAP-TEXTURE-U (LIST 4 4 4 +INF.0) HSSWL)))

;; ALEATORIC QUARTET (JMCC) #7

(DEFINE ALEATORIC-QUARTET
  (LET* ((AMP 0.4)
         (DENSITY (MOUSE-X KR 0.01 1 0 0.1))
         (DMUL (MUL (RECIP DENSITY) (MUL 0.5 AMP)))
         (DADD (ADD (NEG DMUL) AMP))
         (RAPF (LAMBDA (I)
                 (LET ((R (MCE2 (RAND 0 0.05) (RAND 0 0.05))))
                   (ALLPASS-N I 0.05 R 1))))
         (RAPFN (LAMBDA (N I)
                  ((FOLDL1 COMPOSE (REPLICATE N RAPF)) I)))
         (MK-F (LAMBDA ()
                 (LET* ((I0 (I-RAND 0 2))
                        (R0 (SELECT I0 (MCE3 1 0.5 0.25)))
                        (R1 (RAND -30 30))
                        (N0 (LF-NOISE0 KR R0))
                        (M (U:ROUND (MUL-ADD N0 7 (ADD 66 R1)) 1)))
                   (MIDI-CPS (LAG M 0.2)))))
         (MK-S (LAMBDA (_)
                 (LET* ((F (RECIP (MK-F)))
                        (R (RAND -1 1))
                        (N0 (PINK-NOISE AR))
                        (N1 (LF-NOISE1 KR 8))
                        (X (MUL N0 (U:MAX 0 (MUL-ADD N1 DMUL DADD)))))
                   (PAN2 (COMB-L X 0.02 F 3) R 1))))
         (G (MIX-FILL 4 MK-S)))
    (LEAK-DC (RAPFN 4 G) 0.995)))

(HEAR ALEATORIC-QUARTET)

;; MODAL SPACE (JMCC) #8

(DEFINE MODAL-SPACE
  (LET* ((B (AS-LOCAL-BUF (LIST 0 2 3.2 5 7 9 10)))
         (X (MOUSE-X KR 0 15 0 0.1))
         (K (DEGREE-TO-KEY B X 12))
         (C (LAMBDA (N R)
              (LET* ((O (MUL (SIN-OSC AR (MIDI-CPS (ADD3 R K N)) 0) 0.1))
                     (T (LF-PULSE AR (MIDI-CPS (MCE2 48 55)) 0.15 0.5))
                     (F (MIDI-CPS (MUL-ADD (SIN-OSC KR 0.1 0) 10 R)))
                     (D (MUL (RLPF T F 0.1) 0.1))
                     (M (ADD O D)))
                (ADD (COMB-N M 0.31 0.31 2) M))))
         (N (MUL (LF-NOISE1 KR (MCE2 3 3.05)) 0.04)))
    (MUL (ADD (C N 48) (C N 72)) 0.25)))

(HEAR MODAL-SPACE)

;; STRUMMABLE GUITAR (JMCC) #11

(DEFINE STRUMMABLE-GUITAR
  (LET* ((SCALE (LIST 52 57 62 67 71 76))
         (STR (LAMBDA (I)
                (LET* ((X (MOUSE-X KR 0 1 0 0.2))
                       (T (U:ABS (HPZ1 (GT X (ADD 0.25 (MUL I 0.1))))))
                       (E (DECAY T 0.05))
                       (N (MUL (PINK-NOISE AR) E))
                       (DT (RECIP (MIDI-CPS (LIST-REF SCALE I))))
                       (S (COMB-L N DT DT 4)))
                  (PAN2 S (SUB (MUL I 0.2) 0.5) 1))))
         (STRS (MIX-FILL (LENGTH SCALE) STR)))
    (LEAK-DC (LPF STRS 12000) 0.995)))

(HEAR STRUMMABLE-GUITAR)

;; BLIPS 001 (JMCC) #SC3D1.5

(DEFINE BLIPS-001
  (LET* ((C (COIN 0.8))
         (B (LAMBDA ()
              (LET ((F (X-LINE KR (EXP-RAND 0.25 400) (EXP-RAND 0.25 400) 4 0))
                    (NH (X-LINE KR (EXP-RAND 2 100) (EXP-RAND 2 100) 4 0)))
                (BLIP AR F NH)))))
    (MUL C (PAN2 (MUL (B) (B)) (LINE KR (RAND2 1) (RAND2 1) 4 0) 0.3))))

(DEFINE BLIPS-PP
  (LAMBDA (Z)
    (LET ((F (LAMBDA (X) (ALLPASS-N X 0.05 (MCE2 (RAND 0 0.05) (RAND 0 0.05)) 4))))
      (ITERATE 6 F (DISTORT Z)))))

(WITH-SC3*
 (LIST
  (POST-PROCESS-U 2 BLIPS-PP)
  (OVERLAP-TEXTURE-U (LIST 2 1 12 +INF.0) BLIPS-001)))

;; ZIZLE (JMCC) #SC3D1.5

(DEFINE RRAND RAND-RANGE)

(DEFINE ZIZLE
  (LET* ((A (LAMBDA (F)
              (LET ((FREQ (MUL F (MCE2 (RRAND 0.7 1.3) 1)))
                    (PH (MCE2 (RAND* TWO-PI) (RAND* TWO-PI))))
                (MIX (MUL (SIN-OSC AR FREQ PH) 0.1)))))
         (A1 (U:MAX (A (EXP-RAND 0.38 8)) 0))
         (A2 (U:ABS (A (EXP-RAND 6 24)))))
    (PAN2 (SIN-OSC AR (MIDI-CPS (RRAND 24 108)) (RAND* TWO-PI)) (RAND2 1) (MUL A1 A2))))

(WITH-SC3 (OVERLAP-TEXTURE-U (LIST 4 4 12 +INF.0) ZIZLE))

;; BABBLING BROOK (JMCC) #SC3

(DEFINE BABBLING-BROOK
  (LET* ((B (LAMBDA (F M A G)
              (LET* ((N1 (BROWN-NOISE AR))
                     (N2 (BROWN-NOISE AR))
                     (N3 (MUL-ADD (LPF N2 F) M A)))
                (MUL (RHPF (ONE-POLE N1 0.99) N3 0.03) G))))
         (X (CLONE 2 (B 14 400 500 0.024)))
         (Y (CLONE 2 (B 20 800 1000 0.040))))
    (ADD X Y)))

(HEAR BABBLING-BROOK)
