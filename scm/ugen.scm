; AUTO-GENERATED (see hsc3-db)

(define A2K
  (lambda (input)
    (mk-ugen (list "A2K" kr (list input) nil 1 nil nil))))

(define APF
  (lambda (input freq radius)
    (mk-ugen (list "APF" (list 0) (list input freq radius) nil 1 nil nil))))

(define AllpassC
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassC" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define AllpassL
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassL" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define AllpassN
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassN" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define AmpComp
  (lambda (rt freq root exp_)
    (mk-ugen (list "AmpComp" rt (list freq root exp_) nil 1 nil nil))))

(define AmpCompA
  (lambda (rt freq root minAmp rootAmp)
    (mk-ugen (list "AmpCompA" rt (list freq root minAmp rootAmp) nil 1 nil nil))))

(define Amplitude
  (lambda (rt input attackTime releaseTime)
    (mk-ugen (list "Amplitude" rt (list input attackTime releaseTime) nil 1 nil nil))))

(define BAllPass
  (lambda (input freq rq)
    (mk-ugen (list "BAllPass" (list 0) (list input freq rq) nil 1 nil nil))))

(define BBandPass
  (lambda (input freq bw)
    (mk-ugen (list "BBandPass" (list 0) (list input freq bw) nil 1 nil nil))))

(define BBandStop
  (lambda (input freq bw)
    (mk-ugen (list "BBandStop" (list 0) (list input freq bw) nil 1 nil nil))))

(define BHiPass
  (lambda (input freq rq)
    (mk-ugen (list "BHiPass" (list 0) (list input freq rq) nil 1 nil nil))))

(define BHiShelf
  (lambda (input freq rs db)
    (mk-ugen (list "BHiShelf" (list 0) (list input freq rs db) nil 1 nil nil))))

(define BLowPass
  (lambda (input freq rq)
    (mk-ugen (list "BLowPass" (list 0) (list input freq rq) nil 1 nil nil))))

(define BLowShelf
  (lambda (input freq rs db)
    (mk-ugen (list "BLowShelf" (list 0) (list input freq rs db) nil 1 nil nil))))

(define BPF
  (lambda (input freq rq)
    (mk-ugen (list "BPF" (list 0) (list input freq rq) nil 1 nil nil))))

(define BPZ2
  (lambda (input)
    (mk-ugen (list "BPZ2" (list 0) (list input) nil 1 nil nil))))

(define BPeakEQ
  (lambda (input freq rq db)
    (mk-ugen (list "BPeakEQ" (list 0) (list input freq rq db) nil 1 nil nil))))

(define BRF
  (lambda (input freq rq)
    (mk-ugen (list "BRF" (list 0) (list input freq rq) nil 1 nil nil))))

(define BRZ2
  (lambda (input)
    (mk-ugen (list "BRZ2" (list 0) (list input) nil 1 nil nil))))

(define Balance2
  (lambda (left right pos level)
    (mk-ugen (list "Balance2" (list 0 1) (list left right pos level) nil 2 nil nil))))

(define Ball
  (lambda (rt input g damp friction)
    (mk-ugen (list "Ball" rt (list input g damp friction) nil 1 nil nil))))

(define BeatTrack
  (lambda (rt chain lock)
    (mk-ugen (list "BeatTrack" rt (list chain lock) nil 4 nil nil))))

(define BeatTrack2
  (lambda (rt busindex numfeatures windowsize phaseaccuracy lock weightingscheme)
    (mk-ugen (list "BeatTrack2" rt (list busindex numfeatures windowsize phaseaccuracy lock weightingscheme) nil 6 nil nil))))

(define BiPanB2
  (lambda (rt inA inB azimuth gain)
    (mk-ugen (list "BiPanB2" rt (list inA inB azimuth gain) nil 3 nil nil))))

(define BinaryOpUGen
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 nil nil))))

(define Blip
  (lambda (rt freq numharm)
    (mk-ugen (list "Blip" rt (list freq numharm) nil 1 nil nil))))

(define BlockSize (mk-ugen (list "BlockSize" ir nil nil 1 nil nil)))

(define BrownNoise
  (lambda (rt)
    (mk-ugen (list "BrownNoise" rt nil nil 1 nil (unique-uid)))))

(define BufAllpassC
  (lambda (buf input delaytime decaytime)
    (mk-ugen (list "BufAllpassC" (list 1) (list buf input delaytime decaytime) nil 1 nil nil))))

(define BufAllpassL
  (lambda (buf input delaytime decaytime)
    (mk-ugen (list "BufAllpassL" (list 1) (list buf input delaytime decaytime) nil 1 nil nil))))

(define BufAllpassN
  (lambda (buf input delaytime decaytime)
    (mk-ugen (list "BufAllpassN" (list 1) (list buf input delaytime decaytime) nil 1 nil nil))))

(define BufChannels
  (lambda (rt bufnum)
    (mk-ugen (list "BufChannels" rt (list bufnum) nil 1 nil nil))))

(define BufCombC
  (lambda (buf input delaytime decaytime)
    (mk-ugen (list "BufCombC" (list 1) (list buf input delaytime decaytime) nil 1 nil nil))))

(define BufCombL
  (lambda (buf input delaytime decaytime)
    (mk-ugen (list "BufCombL" (list 1) (list buf input delaytime decaytime) nil 1 nil nil))))

(define BufCombN
  (lambda (buf input delaytime decaytime)
    (mk-ugen (list "BufCombN" (list 1) (list buf input delaytime decaytime) nil 1 nil nil))))

(define BufDelayC
  (lambda (buf input delaytime)
    (mk-ugen (list "BufDelayC" (list 1) (list buf input delaytime) nil 1 nil nil))))

(define BufDelayL
  (lambda (buf input delaytime)
    (mk-ugen (list "BufDelayL" (list 1) (list buf input delaytime) nil 1 nil nil))))

(define BufDelayN
  (lambda (buf input delaytime)
    (mk-ugen (list "BufDelayN" (list 1) (list buf input delaytime) nil 1 nil nil))))

(define BufDur
  (lambda (rt bufnum)
    (mk-ugen (list "BufDur" rt (list bufnum) nil 1 nil nil))))

(define BufFrames
  (lambda (rt bufnum)
    (mk-ugen (list "BufFrames" rt (list bufnum) nil 1 nil nil))))

(define BufRateScale
  (lambda (rt bufnum)
    (mk-ugen (list "BufRateScale" rt (list bufnum) nil 1 nil nil))))

(define BufRd
  (lambda (nc rt bufnum phase loop interpolation)
    (mk-ugen (list "BufRd" rt (list bufnum phase loop interpolation) nil nc nil nil))))

(define BufSampleRate
  (lambda (rt bufnum)
    (mk-ugen (list "BufSampleRate" rt (list bufnum) nil 1 nil nil))))

(define BufSamples
  (lambda (rt bufnum)
    (mk-ugen (list "BufSamples" rt (list bufnum) nil 1 nil nil))))

(define BufWr
  (lambda (bufnum phase loop inputArray)
    (mk-ugen (list "BufWr" (list 3) (list bufnum phase loop) inputArray 1 nil nil))))

(define COsc
  (lambda (rt bufnum freq beats)
    (mk-ugen (list "COsc" rt (list bufnum freq beats) nil 1 nil nil))))

(define CheckBadValues
  (lambda (input id_ post)
    (mk-ugen (list "CheckBadValues" (list 0) (list input id_ post) nil 1 nil nil))))

(define Clip
  (lambda (input lo hi)
    (mk-ugen (list "Clip" (list 0) (list input lo hi) nil 1 nil nil))))

(define ClipNoise
  (lambda (rt)
    (mk-ugen (list "ClipNoise" rt nil nil 1 nil (unique-uid)))))

(define CoinGate
  (lambda (prob input)
    (mk-ugen (list "CoinGate" (list 1) (list prob input) nil 1 nil (unique-uid)))))

(define CombC
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombC" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define CombL
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombL" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define CombN
  (lambda (input maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombN" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil nil))))

(define Compander
  (lambda (input control_ thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "Compander" (list 0) (list input control_ thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil nil))))

(define CompanderD
  (lambda (rt input thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "CompanderD" rt (list input thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil nil))))

(define ControlDur (mk-ugen (list "ControlDur" ir nil nil 1 nil nil)))

(define ControlRate (mk-ugen (list "ControlRate" ir nil nil 1 nil nil)))

(define Convolution
  (lambda (input kernel framesize)
    (mk-ugen (list "Convolution" ar (list input kernel framesize) nil 1 nil nil))))

(define Convolution2
  (lambda (input kernel trigger framesize)
    (mk-ugen (list "Convolution2" ar (list input kernel trigger framesize) nil 1 nil nil))))

(define Convolution2L
  (lambda (rt input kernel trigger framesize crossfade)
    (mk-ugen (list "Convolution2L" rt (list input kernel trigger framesize crossfade) nil 1 nil nil))))

(define Convolution3
  (lambda (rt input kernel trigger framesize)
    (mk-ugen (list "Convolution3" rt (list input kernel trigger framesize) nil 1 nil nil))))

(define Crackle
  (lambda (rt chaosParam)
    (mk-ugen (list "Crackle" rt (list chaosParam) nil 1 nil nil))))

(define CuspL
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspL" rt (list freq a b xi) nil 1 nil nil))))

(define CuspN
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspN" rt (list freq a b xi) nil 1 nil nil))))

(define DC
  (lambda (rt input)
    (mk-ugen (list "DC" rt (list input) nil 1 nil nil))))

(define Dbrown
  (lambda (length_ lo hi step)
    (mk-ugen (list "Dbrown" dr (list length_ lo hi step) nil 1 nil (unique-uid)))))

(define Dbufrd
  (lambda (bufnum phase loop)
    (mk-ugen (list "Dbufrd" dr (list bufnum phase loop) nil 1 nil (unique-uid)))))

(define Dbufwr
  (lambda (bufnum phase loop input)
    (mk-ugen (list "Dbufwr" dr (list bufnum phase loop input) nil 1 nil (unique-uid)))))

(define Dconst
  (lambda (sum_ input tolerance)
    (mk-ugen (list "Dconst" dr (list sum_ input tolerance) nil 1 nil (unique-uid)))))

(define Decay
  (lambda (input decayTime)
    (mk-ugen (list "Decay" (list 0) (list input decayTime) nil 1 nil nil))))

(define Decay2
  (lambda (input attackTime decayTime)
    (mk-ugen (list "Decay2" (list 0) (list input attackTime decayTime) nil 1 nil nil))))

(define DecodeB2
  (lambda (nc w x y orientation)
    (mk-ugen (list "DecodeB2" (list 0 1 2) (list w x y orientation) nil nc nil nil))))

(define DegreeToKey
  (lambda (bufnum input octave)
    (mk-ugen (list "DegreeToKey" (list 1) (list bufnum input octave) nil 1 nil nil))))

(define DelTapRd
  (lambda (buffer phase delTime interp)
    (mk-ugen (list "DelTapRd" (list 1) (list buffer phase delTime interp) nil 1 nil nil))))

(define DelTapWr
  (lambda (buffer input)
    (mk-ugen (list "DelTapWr" (list 1) (list buffer input) nil 1 nil nil))))

(define Delay1
  (lambda (input)
    (mk-ugen (list "Delay1" (list 0) (list input) nil 1 nil nil))))

(define Delay2
  (lambda (input)
    (mk-ugen (list "Delay2" (list 0) (list input) nil 1 nil nil))))

(define DelayC
  (lambda (input maxdelaytime delaytime)
    (mk-ugen (list "DelayC" (list 0) (list input maxdelaytime delaytime) nil 1 nil nil))))

(define DelayL
  (lambda (input maxdelaytime delaytime)
    (mk-ugen (list "DelayL" (list 0) (list input maxdelaytime delaytime) nil 1 nil nil))))

(define DelayN
  (lambda (input maxdelaytime delaytime)
    (mk-ugen (list "DelayN" (list 0) (list input maxdelaytime delaytime) nil 1 nil nil))))

(define Demand
  (lambda (trig_ reset demandUGens)
    (mk-ugen (list "Demand" (list 0) (list trig_ reset) demandUGens (length (mce-channels demandUGens)) nil nil))))

(define DemandEnvGen
  (lambda (rt level dur shape curve gate_ reset levelScale levelBias timeScale doneAction)
    (mk-ugen (list "DemandEnvGen" rt (list level dur shape curve gate_ reset levelScale levelBias timeScale doneAction) nil 1 nil nil))))

(define DetectIndex
  (lambda (bufnum input)
    (mk-ugen (list "DetectIndex" (list 1) (list bufnum input) nil 1 nil nil))))

(define DetectSilence
  (lambda (input amp time doneAction)
    (mk-ugen (list "DetectSilence" (list 0) (list input amp time doneAction) nil 1 nil nil))))

(define Dgeom
  (lambda (length_ start grow)
    (mk-ugen (list "Dgeom" dr (list length_ start grow) nil 1 nil (unique-uid)))))

(define Dibrown
  (lambda (length_ lo hi step)
    (mk-ugen (list "Dibrown" dr (list length_ lo hi step) nil 1 nil (unique-uid)))))

(define DiskIn
  (lambda (nc bufnum loop)
    (mk-ugen (list "DiskIn" ar (list bufnum loop) nil nc nil nil))))

(define DiskOut
  (lambda (bufnum input)
    (mk-ugen (list "DiskOut" ar (list bufnum) input 1 nil nil))))

(define Diwhite
  (lambda (length_ lo hi)
    (mk-ugen (list "Diwhite" dr (list length_ lo hi) nil 1 nil (unique-uid)))))

(define Done
  (lambda (src)
    (mk-ugen (list "Done" kr (list src) nil 1 nil nil))))

(define Dpoll
  (lambda (input label_ run trigid)
    (mk-ugen (list "Dpoll" dr (list input label_ run trigid) nil 1 nil (unique-uid)))))

(define Drand
  (lambda (repeats list_)
    (mk-ugen (list "Drand" dr (list repeats) list_ 1 nil (unique-uid)))))

(define Dreset
  (lambda (input reset)
    (mk-ugen (list "Dreset" dr (list input reset) nil 1 nil (unique-uid)))))

(define Dseq
  (lambda (repeats list_)
    (mk-ugen (list "Dseq" dr (list repeats) list_ 1 nil (unique-uid)))))

(define Dser
  (lambda (repeats list_)
    (mk-ugen (list "Dser" dr (list repeats) list_ 1 nil (unique-uid)))))

(define Dseries
  (lambda (length_ start step)
    (mk-ugen (list "Dseries" dr (list length_ start step) nil 1 nil (unique-uid)))))

(define Dshuf
  (lambda (repeats list_)
    (mk-ugen (list "Dshuf" dr (list repeats) list_ 1 nil (unique-uid)))))

(define Dstutter
  (lambda (n input)
    (mk-ugen (list "Dstutter" dr (list n input) nil 1 nil (unique-uid)))))

(define Dswitch
  (lambda (index list_)
    (mk-ugen (list "Dswitch" dr (list index) list_ 1 nil (unique-uid)))))

(define Dswitch1
  (lambda (index list_)
    (mk-ugen (list "Dswitch1" dr (list index) list_ 1 nil (unique-uid)))))

(define Dunique
  (lambda (source maxBufferSize protected)
    (mk-ugen (list "Dunique" dr (list source maxBufferSize protected) nil 1 nil (unique-uid)))))

(define Dust
  (lambda (rt density)
    (mk-ugen (list "Dust" rt (list density) nil 1 nil (unique-uid)))))

(define Dust2
  (lambda (rt density)
    (mk-ugen (list "Dust2" rt (list density) nil 1 nil (unique-uid)))))

(define Duty
  (lambda (rt dur reset doneAction level)
    (mk-ugen (list "Duty" rt (list dur reset doneAction level) nil 1 nil nil))))

(define Dwhite
  (lambda (length_ lo hi)
    (mk-ugen (list "Dwhite" dr (list length_ lo hi) nil 1 nil (unique-uid)))))

(define Dwrand
  (lambda (repeats weights list_)
    (mk-ugen (list "Dwrand" dr (list repeats weights) list_ 1 nil (unique-uid)))))

(define Dxrand
  (lambda (repeats list_)
    (mk-ugen (list "Dxrand" dr (list repeats) list_ 1 nil (unique-uid)))))

(define EnvGen
  (lambda (rt gate_ levelScale levelBias timeScale doneAction envelope_)
    (mk-ugen (list "EnvGen" rt (list gate_ levelScale levelBias timeScale doneAction) envelope_ 1 nil nil))))

(define ExpRand
  (lambda (lo hi)
    (mk-ugen (list "ExpRand" ir (list lo hi) nil 1 nil (unique-uid)))))

(define FBSineC
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineC" rt (list freq im fb a c xi yi) nil 1 nil nil))))

(define FBSineL
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineL" rt (list freq im fb a c xi yi) nil 1 nil nil))))

(define FBSineN
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineN" rt (list freq im fb a c xi yi) nil 1 nil nil))))

(define FFT
  (lambda (buffer input hop wintype active winsize)
    (mk-ugen (list "FFT" kr (list buffer input hop wintype active winsize) nil 1 nil nil))))

(define FOS
  (lambda (input a0 a1 b1)
    (mk-ugen (list "FOS" (list 0) (list input a0 a1 b1) nil 1 nil nil))))

(define FSinOsc
  (lambda (rt freq iphase)
    (mk-ugen (list "FSinOsc" rt (list freq iphase) nil 1 nil nil))))

(define Fold
  (lambda (input lo hi)
    (mk-ugen (list "Fold" (list 0) (list input lo hi) nil 1 nil nil))))

(define Formant
  (lambda (rt fundfreq formfreq bwfreq)
    (mk-ugen (list "Formant" rt (list fundfreq formfreq bwfreq) nil 1 nil nil))))

(define Formlet
  (lambda (input freq attacktime decaytime)
    (mk-ugen (list "Formlet" (list 0) (list input freq attacktime decaytime) nil 1 nil nil))))

(define Free
  (lambda (trig_ id_)
    (mk-ugen (list "Free" (list 0) (list trig_ id_) nil 1 nil nil))))

(define FreeSelf
  (lambda (input)
    (mk-ugen (list "FreeSelf" kr (list input) nil 1 nil nil))))

(define FreeSelfWhenDone
  (lambda (src)
    (mk-ugen (list "FreeSelfWhenDone" kr (list src) nil 1 nil nil))))

(define FreeVerb
  (lambda (input mix room damp)
    (mk-ugen (list "FreeVerb" (list 0) (list input mix room damp) nil 1 nil nil))))

(define FreeVerb2
  (lambda (input in2 mix room damp)
    (mk-ugen (list "FreeVerb2" (list 0) (list input in2 mix room damp) nil 2 nil nil))))

(define FreqShift
  (lambda (input freq phase)
    (mk-ugen (list "FreqShift" ar (list input freq phase) nil 1 nil nil))))

(define GVerb
  (lambda (input roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize)
    (mk-ugen (list "GVerb" (list 0) (list input roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize) nil 2 nil nil))))

(define Gate
  (lambda (input trig_)
    (mk-ugen (list "Gate" (list 0) (list input trig_) nil 1 nil nil))))

(define GbmanL
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanL" rt (list freq xi yi) nil 1 nil nil))))

(define GbmanN
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanN" rt (list freq xi yi) nil 1 nil nil))))

(define Gendy1
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy1" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil (unique-uid)))))

(define Gendy2
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c)
    (mk-ugen (list "Gendy2" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c) nil 1 nil (unique-uid)))))

(define Gendy3
  (lambda (rt ampdist durdist adparam ddparam freq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy3" rt (list ampdist durdist adparam ddparam freq ampscale durscale initCPs knum) nil 1 nil (unique-uid)))))

(define GrainBuf
  (lambda (nc trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains)
    (mk-ugen (list "GrainBuf" ar (list trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains) nil nc nil nil))))

(define GrainFM
  (lambda (nc trigger dur carfreq modfreq index pan envbufnum maxGrains)
    (mk-ugen (list "GrainFM" ar (list trigger dur carfreq modfreq index pan envbufnum maxGrains) nil nc nil nil))))

(define GrainIn
  (lambda (nc trigger dur input pan envbufnum maxGrains)
    (mk-ugen (list "GrainIn" ar (list trigger dur input pan envbufnum maxGrains) nil nc nil nil))))

(define GrainSin
  (lambda (nc trigger dur freq pan envbufnum maxGrains)
    (mk-ugen (list "GrainSin" ar (list trigger dur freq pan envbufnum maxGrains) nil nc nil nil))))

(define GrayNoise
  (lambda (rt)
    (mk-ugen (list "GrayNoise" rt nil nil 1 nil (unique-uid)))))

(define HPF
  (lambda (input freq)
    (mk-ugen (list "HPF" (list 0) (list input freq) nil 1 nil nil))))

(define HPZ1
  (lambda (input)
    (mk-ugen (list "HPZ1" (list 0) (list input) nil 1 nil nil))))

(define HPZ2
  (lambda (input)
    (mk-ugen (list "HPZ2" (list 0) (list input) nil 1 nil nil))))

(define Hasher
  (lambda (input)
    (mk-ugen (list "Hasher" (list 0) (list input) nil 1 nil nil))))

(define HenonC
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonC" rt (list freq a b x0 x1) nil 1 nil nil))))

(define HenonL
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonL" rt (list freq a b x0 x1) nil 1 nil nil))))

(define HenonN
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonN" rt (list freq a b x0 x1) nil 1 nil nil))))

(define Hilbert
  (lambda (input)
    (mk-ugen (list "Hilbert" (list 0) (list input) nil 2 nil nil))))

(define IEnvGen
  (lambda (rt index envelope_)
    (mk-ugen (list "IEnvGen" rt (list index) envelope_ 1 nil nil))))

(define IFFT
  (lambda (buffer wintype winsize)
    (mk-ugen (list "IFFT" ar (list buffer wintype winsize) nil 1 nil nil))))

(define IRand
  (lambda (lo hi)
    (mk-ugen (list "IRand" ir (list lo hi) nil 1 nil (unique-uid)))))

(define Impulse
  (lambda (rt freq phase)
    (mk-ugen (list "Impulse" rt (list freq phase) nil 1 nil nil))))

(define In
  (lambda (nc rt bus)
    (mk-ugen (list "In" rt (list bus) nil nc nil nil))))

(define InFeedback
  (lambda (nc bus)
    (mk-ugen (list "InFeedback" ar (list bus) nil nc nil nil))))

(define InRange
  (lambda (input lo hi)
    (mk-ugen (list "InRange" (list 0) (list input lo hi) nil 1 nil nil))))

(define InRect
  (lambda (rt x y rect)
    (mk-ugen (list "InRect" rt (list x y rect) nil 1 nil nil))))

(define InTrig
  (lambda (nc bus)
    (mk-ugen (list "InTrig" kr (list bus) nil nc nil nil))))

(define Index
  (lambda (bufnum input)
    (mk-ugen (list "Index" (list 1) (list bufnum input) nil 1 nil nil))))

(define IndexInBetween
  (lambda (bufnum input)
    (mk-ugen (list "IndexInBetween" (list 1) (list bufnum input) nil 1 nil nil))))

(define IndexL
  (lambda (bufnum input)
    (mk-ugen (list "IndexL" (list 1) (list bufnum input) nil 1 nil nil))))

(define InfoUGenBase
  (lambda (rt)
    (mk-ugen (list "InfoUGenBase" rt nil nil 1 nil nil))))

(define Integrator
  (lambda (input coef)
    (mk-ugen (list "Integrator" (list 0) (list input coef) nil 1 nil nil))))

(define K2A
  (lambda (input)
    (mk-ugen (list "K2A" ar (list input) nil 1 nil nil))))

(define KeyState
  (lambda (rt keycode minval maxval lag)
    (mk-ugen (list "KeyState" rt (list keycode minval maxval lag) nil 1 nil nil))))

(define KeyTrack
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyTrack" rt (list chain keydecay chromaleak) nil 1 nil nil))))

(define Klang
  (lambda (rt freqscale freqoffset specificationsArrayRef)
    (mk-ugen (list "Klang" rt (list freqscale freqoffset) specificationsArrayRef 1 nil nil))))

(define Klank
  (lambda (input freqscale freqoffset decayscale specificationsArrayRef)
    (mk-ugen (list "Klank" (list 0) (list input freqscale freqoffset decayscale) specificationsArrayRef 1 nil nil))))

(define LFClipNoise
  (lambda (rt freq)
    (mk-ugen (list "LFClipNoise" rt (list freq) nil 1 nil (unique-uid)))))

(define LFCub
  (lambda (rt freq iphase)
    (mk-ugen (list "LFCub" rt (list freq iphase) nil 1 nil nil))))

(define LFDClipNoise
  (lambda (rt freq)
    (mk-ugen (list "LFDClipNoise" rt (list freq) nil 1 nil (unique-uid)))))

(define LFDNoise0
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise0" rt (list freq) nil 1 nil (unique-uid)))))

(define LFDNoise1
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise1" rt (list freq) nil 1 nil (unique-uid)))))

(define LFDNoise3
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise3" rt (list freq) nil 1 nil (unique-uid)))))

(define LFGauss
  (lambda (rt duration width iphase loop doneAction)
    (mk-ugen (list "LFGauss" rt (list duration width iphase loop doneAction) nil 1 nil nil))))

(define LFNoise0
  (lambda (rt freq)
    (mk-ugen (list "LFNoise0" rt (list freq) nil 1 nil (unique-uid)))))

(define LFNoise1
  (lambda (rt freq)
    (mk-ugen (list "LFNoise1" rt (list freq) nil 1 nil (unique-uid)))))

(define LFNoise2
  (lambda (rt freq)
    (mk-ugen (list "LFNoise2" rt (list freq) nil 1 nil (unique-uid)))))

(define LFPar
  (lambda (rt freq iphase)
    (mk-ugen (list "LFPar" rt (list freq iphase) nil 1 nil nil))))

(define LFPulse
  (lambda (rt freq iphase width)
    (mk-ugen (list "LFPulse" rt (list freq iphase width) nil 1 nil nil))))

(define LFSaw
  (lambda (rt freq iphase)
    (mk-ugen (list "LFSaw" rt (list freq iphase) nil 1 nil nil))))

(define LFTri
  (lambda (rt freq iphase)
    (mk-ugen (list "LFTri" rt (list freq iphase) nil 1 nil nil))))

(define LPF
  (lambda (input freq)
    (mk-ugen (list "LPF" (list 0) (list input freq) nil 1 nil nil))))

(define LPZ1
  (lambda (input)
    (mk-ugen (list "LPZ1" (list 0) (list input) nil 1 nil nil))))

(define LPZ2
  (lambda (input)
    (mk-ugen (list "LPZ2" (list 0) (list input) nil 1 nil nil))))

(define Lag
  (lambda (input lagTime)
    (mk-ugen (list "Lag" (list 0) (list input lagTime) nil 1 nil nil))))

(define Lag2
  (lambda (input lagTime)
    (mk-ugen (list "Lag2" (list 0) (list input lagTime) nil 1 nil nil))))

(define Lag2UD
  (lambda (input lagTimeU lagTimeD)
    (mk-ugen (list "Lag2UD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil nil))))

(define Lag3
  (lambda (input lagTime)
    (mk-ugen (list "Lag3" (list 0) (list input lagTime) nil 1 nil nil))))

(define Lag3UD
  (lambda (input lagTimeU lagTimeD)
    (mk-ugen (list "Lag3UD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil nil))))

(define LagIn
  (lambda (nc bus lag)
    (mk-ugen (list "LagIn" kr (list bus lag) nil nc nil nil))))

(define LagUD
  (lambda (input lagTimeU lagTimeD)
    (mk-ugen (list "LagUD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil nil))))

(define LastValue
  (lambda (input diff)
    (mk-ugen (list "LastValue" (list 0) (list input diff) nil 1 nil nil))))

(define Latch
  (lambda (input trig_)
    (mk-ugen (list "Latch" (list 0 1) (list input trig_) nil 1 nil nil))))

(define LatoocarfianC
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianC" rt (list freq a b c d xi yi) nil 1 nil nil))))

(define LatoocarfianL
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianL" rt (list freq a b c d xi yi) nil 1 nil nil))))

(define LatoocarfianN
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianN" rt (list freq a b c d xi yi) nil 1 nil nil))))

(define LeakDC
  (lambda (input coef)
    (mk-ugen (list "LeakDC" (list 0) (list input coef) nil 1 nil nil))))

(define LeastChange
  (lambda (rt a b)
    (mk-ugen (list "LeastChange" rt (list a b) nil 1 nil nil))))

(define Limiter
  (lambda (input level dur)
    (mk-ugen (list "Limiter" (list 0) (list input level dur) nil 1 nil nil))))

(define LinCongC
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongC" rt (list freq a c m xi) nil 1 nil nil))))

(define LinCongL
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongL" rt (list freq a c m xi) nil 1 nil nil))))

(define LinCongN
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongN" rt (list freq a c m xi) nil 1 nil nil))))

(define LinExp
  (lambda (input srclo srchi dstlo dsthi)
    (mk-ugen (list "LinExp" (list 0) (list input srclo srchi dstlo dsthi) nil 1 nil nil))))

(define LinPan2
  (lambda (input pos level)
    (mk-ugen (list "LinPan2" (list 0) (list input pos level) nil 2 nil nil))))

(define LinRand
  (lambda (lo hi minmax)
    (mk-ugen (list "LinRand" ir (list lo hi minmax) nil 1 nil (unique-uid)))))

(define LinXFade2
  (lambda (inA inB pan level)
    (mk-ugen (list "LinXFade2" (list 0 1) (list inA inB pan level) nil 1 nil nil))))

(define Line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "Line" rt (list start end dur doneAction) nil 1 nil nil))))

(define Linen
  (lambda (gate_ attackTime susLevel releaseTime doneAction)
    (mk-ugen (list "Linen" kr (list gate_ attackTime susLevel releaseTime doneAction) nil 1 nil nil))))

(define LocalBuf
  (lambda (numChannels numFrames)
    (mk-ugen (list "LocalBuf" ir (list numChannels numFrames) nil 1 nil (unique-uid)))))

(define LocalIn
  (lambda (nc rt default_)
    (mk-ugen (list "LocalIn" rt nil default_ nc nil nil))))

(define LocalOut
  (lambda (input)
    (mk-ugen (list "LocalOut" (list 0) nil input 0 nil nil))))

(define Logistic
  (lambda (rt chaosParam freq init_)
    (mk-ugen (list "Logistic" rt (list chaosParam freq init_) nil 1 nil nil))))

(define LorenzL
  (lambda (rt freq s r b h xi yi zi)
    (mk-ugen (list "LorenzL" rt (list freq s r b h xi yi zi) nil 1 nil nil))))

(define Loudness
  (lambda (chain smask tmask)
    (mk-ugen (list "Loudness" kr (list chain smask tmask) nil 1 nil nil))))

(define MFCC
  (lambda (rt chain numcoeff)
    (mk-ugen (list "MFCC" rt (list chain numcoeff) nil 13 nil nil))))

(define MantissaMask
  (lambda (input bits)
    (mk-ugen (list "MantissaMask" (list 0) (list input bits) nil 1 nil nil))))

(define Median
  (lambda (length_ input)
    (mk-ugen (list "Median" (list 1) (list length_ input) nil 1 nil nil))))

(define MidEQ
  (lambda (input freq rq db)
    (mk-ugen (list "MidEQ" (list 0) (list input freq rq db) nil 1 nil nil))))

(define ModDif
  (lambda (x y mod_)
    (mk-ugen (list "ModDif" (list 0) (list x y mod_) nil 1 nil nil))))

(define MoogFF
  (lambda (input freq gain reset)
    (mk-ugen (list "MoogFF" (list 0) (list input freq gain reset) nil 1 nil nil))))

(define MostChange
  (lambda (a b)
    (mk-ugen (list "MostChange" (list 0 1) (list a b) nil 1 nil nil))))

(define MouseButton
  (lambda (rt minval maxval lag)
    (mk-ugen (list "MouseButton" rt (list minval maxval lag) nil 1 nil nil))))

(define MouseX
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseX" rt (list minval maxval warp lag) nil 1 nil nil))))

(define MouseY
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseY" rt (list minval maxval warp lag) nil 1 nil nil))))

(define NRand
  (lambda (lo hi n)
    (mk-ugen (list "NRand" ir (list lo hi n) nil 1 nil (unique-uid)))))

(define NodeID
  (lambda (rt)
    (mk-ugen (list "NodeID" rt nil nil 1 nil nil))))

(define Normalizer
  (lambda (input level dur)
    (mk-ugen (list "Normalizer" (list 0) (list input level dur) nil 1 nil nil))))

(define NumAudioBuses (mk-ugen (list "NumAudioBuses" ir nil nil 1 nil nil)))

(define NumBuffers (mk-ugen (list "NumBuffers" ir nil nil 1 nil nil)))

(define NumControlBuses (mk-ugen (list "NumControlBuses" ir nil nil 1 nil nil)))

(define NumInputBuses (mk-ugen (list "NumInputBuses" ir nil nil 1 nil nil)))

(define NumOutputBuses (mk-ugen (list "NumOutputBuses" ir nil nil 1 nil nil)))

(define NumRunningSynths (mk-ugen (list "NumRunningSynths" ir nil nil 1 nil nil)))

(define OffsetOut
  (lambda (bus input)
    (mk-ugen (list "OffsetOut" (list 1) (list bus) input 0 nil nil))))

(define OnePole
  (lambda (input coef)
    (mk-ugen (list "OnePole" (list 0) (list input coef) nil 1 nil nil))))

(define OneZero
  (lambda (input coef)
    (mk-ugen (list "OneZero" (list 0) (list input coef) nil 1 nil nil))))

(define Onsets
  (lambda (chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf)
    (mk-ugen (list "Onsets" kr (list chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf) nil 1 nil nil))))

(define Osc
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "Osc" rt (list bufnum freq phase) nil 1 nil nil))))

(define OscN
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "OscN" rt (list bufnum freq phase) nil 1 nil nil))))

(define Out
  (lambda (bus input)
    (mk-ugen (list "Out" (list 1) (list bus) input 0 nil nil))))

(define PSinGrain
  (lambda (rt freq dur amp)
    (mk-ugen (list "PSinGrain" rt (list freq dur amp) nil 1 nil nil))))

(define PV_Add
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Add" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_BinScramble
  (lambda (buffer wipe width trig_)
    (mk-ugen (list "PV_BinScramble" kr (list buffer wipe width trig_) nil 1 nil (unique-uid)))))

(define PV_BinShift
  (lambda (buffer stretch shift interp)
    (mk-ugen (list "PV_BinShift" kr (list buffer stretch shift interp) nil 1 nil nil))))

(define PV_BinWipe
  (lambda (bufferA bufferB wipe)
    (mk-ugen (list "PV_BinWipe" kr (list bufferA bufferB wipe) nil 1 nil nil))))

(define PV_BrickWall
  (lambda (buffer wipe)
    (mk-ugen (list "PV_BrickWall" kr (list buffer wipe) nil 1 nil nil))))

(define PV_ChainUGen
  (lambda (maxSize)
    (mk-ugen (list "PV_ChainUGen" kr (list maxSize) nil 1 nil nil))))

(define PV_ConformalMap
  (lambda (buffer areal aimag)
    (mk-ugen (list "PV_ConformalMap" kr (list buffer areal aimag) nil 1 nil nil))))

(define PV_Conj
  (lambda (buffer)
    (mk-ugen (list "PV_Conj" kr (list buffer) nil 1 nil nil))))

(define PV_Copy
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Copy" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_CopyPhase
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_CopyPhase" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_Diffuser
  (lambda (buffer trig_)
    (mk-ugen (list "PV_Diffuser" kr (list buffer trig_) nil 1 nil nil))))

(define PV_Div
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Div" kr (list bufferA bufferB) nil 1 nil nil))))

;(define PV_HainsworthFoote
;  (lambda (maxSize)
;    (mk-ugen (list "PV_HainsworthFoote" kr (list maxSize) nil 1 nil nil))))

;(define PV_JensenAndersen
;  (lambda (maxSize)
;    (mk-ugen (list "PV_JensenAndersen" kr (list maxSize) nil 1 nil nil))))

(define PV_LocalMax
  (lambda (buffer threshold)
    (mk-ugen (list "PV_LocalMax" kr (list buffer threshold) nil 1 nil nil))))

(define PV_MagAbove
  (lambda (buffer threshold)
    (mk-ugen (list "PV_MagAbove" kr (list buffer threshold) nil 1 nil nil))))

(define PV_MagBelow
  (lambda (buffer threshold)
    (mk-ugen (list "PV_MagBelow" kr (list buffer threshold) nil 1 nil nil))))

(define PV_MagClip
  (lambda (buffer threshold)
    (mk-ugen (list "PV_MagClip" kr (list buffer threshold) nil 1 nil nil))))

(define PV_MagDiv
  (lambda (bufferA bufferB zeroed)
    (mk-ugen (list "PV_MagDiv" kr (list bufferA bufferB zeroed) nil 1 nil nil))))

(define PV_MagFreeze
  (lambda (buffer freeze)
    (mk-ugen (list "PV_MagFreeze" kr (list buffer freeze) nil 1 nil nil))))

(define PV_MagMul
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_MagMul" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_MagNoise
  (lambda (buffer)
    (mk-ugen (list "PV_MagNoise" kr (list buffer) nil 1 nil nil))))

(define PV_MagShift
  (lambda (buffer stretch shift)
    (mk-ugen (list "PV_MagShift" kr (list buffer stretch shift) nil 1 nil nil))))

(define PV_MagSmear
  (lambda (buffer bins)
    (mk-ugen (list "PV_MagSmear" kr (list buffer bins) nil 1 nil nil))))

(define PV_MagSquared
  (lambda (buffer)
    (mk-ugen (list "PV_MagSquared" kr (list buffer) nil 1 nil nil))))

(define PV_Max
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Max" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_Min
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Min" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_Mul
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Mul" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_PhaseShift
  (lambda (buffer shift integrate)
    (mk-ugen (list "PV_PhaseShift" kr (list buffer shift integrate) nil 1 nil nil))))

(define PV_PhaseShift270
  (lambda (buffer)
    (mk-ugen (list "PV_PhaseShift270" kr (list buffer) nil 1 nil nil))))

(define PV_PhaseShift90
  (lambda (buffer)
    (mk-ugen (list "PV_PhaseShift90" kr (list buffer) nil 1 nil nil))))

(define PV_RandComb
  (lambda (buffer wipe trig_)
    (mk-ugen (list "PV_RandComb" kr (list buffer wipe trig_) nil 1 nil (unique-uid)))))

(define PV_RandWipe
  (lambda (bufferA bufferB wipe trig_)
    (mk-ugen (list "PV_RandWipe" kr (list bufferA bufferB wipe trig_) nil 1 nil (unique-uid)))))

(define PV_RectComb
  (lambda (buffer numTeeth phase width)
    (mk-ugen (list "PV_RectComb" kr (list buffer numTeeth phase width) nil 1 nil nil))))

(define PV_RectComb2
  (lambda (bufferA bufferB numTeeth phase width)
    (mk-ugen (list "PV_RectComb2" kr (list bufferA bufferB numTeeth phase width) nil 1 nil nil))))

(define Pan2
  (lambda (input pos level)
    (mk-ugen (list "Pan2" (list 0) (list input pos level) nil 2 nil nil))))

(define Pan4
  (lambda (rt input xpos ypos level)
    (mk-ugen (list "Pan4" rt (list input xpos ypos level) nil 4 nil nil))))

(define PanAz
  (lambda (nc input pos level width orientation)
    (mk-ugen (list "PanAz" (list 0) (list input pos level width orientation) nil nc nil nil))))

(define PanB
  (lambda (rt input azimuth elevation gain)
    (mk-ugen (list "PanB" rt (list input azimuth elevation gain) nil 4 nil nil))))

(define PanB2
  (lambda (input azimuth gain)
    (mk-ugen (list "PanB2" (list 0) (list input azimuth gain) nil 3 nil nil))))

(define PartConv
  (lambda (input fftsize irbufnum)
    (mk-ugen (list "PartConv" ar (list input fftsize irbufnum) nil 1 nil nil))))

(define Pause
  (lambda (gate_ id_)
    (mk-ugen (list "Pause" kr (list gate_ id_) nil 1 nil nil))))

(define PauseSelf
  (lambda (input)
    (mk-ugen (list "PauseSelf" kr (list input) nil 1 nil nil))))

(define PauseSelfWhenDone
  (lambda (src)
    (mk-ugen (list "PauseSelfWhenDone" kr (list src) nil 1 nil nil))))

(define Peak
  (lambda (input trig_)
    (mk-ugen (list "Peak" (list 0) (list input trig_) nil 1 nil nil))))

(define PeakFollower
  (lambda (input decay_)
    (mk-ugen (list "PeakFollower" (list 0) (list input decay_) nil 1 nil nil))))

(define Phasor
  (lambda (rt trig_ rate_ start end resetPos)
    (mk-ugen (list "Phasor" rt (list trig_ rate_ start end resetPos) nil 1 nil nil))))

(define PinkNoise
  (lambda (rt)
    (mk-ugen (list "PinkNoise" rt nil nil 1 nil (unique-uid)))))

(define Pitch
  (lambda (input initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar)
    (mk-ugen (list "Pitch" kr (list input initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar) nil 2 nil nil))))

(define PitchShift
  (lambda (input windowSize pitchRatio pitchDispersion timeDispersion)
    (mk-ugen (list "PitchShift" (list 0) (list input windowSize pitchRatio pitchDispersion timeDispersion) nil 1 nil nil))))

(define PlayBuf
  (lambda (nc rt bufnum rate_ trigger startPos loop doneAction)
    (mk-ugen (list "PlayBuf" rt (list bufnum rate_ trigger startPos loop doneAction) nil nc nil nil))))

(define Pluck
  (lambda (input trig_ maxdelaytime delaytime decaytime coef)
    (mk-ugen (list "Pluck" (list 0) (list input trig_ maxdelaytime delaytime decaytime coef) nil 1 nil nil))))

;(define Poll
;  (lambda (trig_ input trigid label_)
;    (mk-ugen (list "Poll" (list 1) (list trig_ input trigid label_) nil 1 nil nil))))

(define Pulse
  (lambda (rt freq width)
    (mk-ugen (list "Pulse" rt (list freq width) nil 1 nil nil))))

(define PulseCount
  (lambda (trig_ reset)
    (mk-ugen (list "PulseCount" (list 0) (list trig_ reset) nil 1 nil nil))))

(define PulseDivider
  (lambda (trig_ div_ start)
    (mk-ugen (list "PulseDivider" (list 0) (list trig_ div_ start) nil 1 nil nil))))

(define QuadC
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadC" rt (list freq a b c xi) nil 1 nil nil))))

(define QuadL
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadL" rt (list freq a b c xi) nil 1 nil nil))))

(define QuadN
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadN" rt (list freq a b c xi) nil 1 nil nil))))

(define RHPF
  (lambda (input freq rq)
    (mk-ugen (list "RHPF" (list 0) (list input freq rq) nil 1 nil nil))))

(define RLPF
  (lambda (input freq rq)
    (mk-ugen (list "RLPF" (list 0) (list input freq rq) nil 1 nil nil))))

(define RadiansPerSample (mk-ugen (list "RadiansPerSample" ir nil nil 1 nil nil)))

(define Ramp
  (lambda (input lagTime)
    (mk-ugen (list "Ramp" (list 0) (list input lagTime) nil 1 nil nil))))

(define Rand
  (lambda (lo hi)
    (mk-ugen (list "Rand" ir (list lo hi) nil 1 nil (unique-uid)))))

(define RandID
  (lambda (rt id_)
    (mk-ugen (list "RandID" rt (list id_) nil 0 nil nil))))

(define RandSeed
  (lambda (rt trig_ seed)
    (mk-ugen (list "RandSeed" rt (list trig_ seed) nil 0 nil nil))))

(define RecordBuf
  (lambda (rt bufnum offset recLevel preLevel run loop trigger doneAction inputArray)
    (mk-ugen (list "RecordBuf" rt (list bufnum offset recLevel preLevel run loop trigger doneAction) inputArray 1 nil nil))))

(define ReplaceOut
  (lambda (bus input)
    (mk-ugen (list "ReplaceOut" (list 1) (list bus) input 0 nil nil))))

(define Resonz
  (lambda (input freq bwr)
    (mk-ugen (list "Resonz" (list 0) (list input freq bwr) nil 1 nil nil))))

(define Ringz
  (lambda (input freq decaytime)
    (mk-ugen (list "Ringz" (list 0) (list input freq decaytime) nil 1 nil nil))))

(define Rotate2
  (lambda (x y pos)
    (mk-ugen (list "Rotate2" (list 0 1) (list x y pos) nil 2 nil nil))))

(define RunningMax
  (lambda (input trig_)
    (mk-ugen (list "RunningMax" (list 0) (list input trig_) nil 1 nil nil))))

(define RunningMin
  (lambda (input trig_)
    (mk-ugen (list "RunningMin" (list 0) (list input trig_) nil 1 nil nil))))

(define RunningSum
  (lambda (input numsamp)
    (mk-ugen (list "RunningSum" (list 0) (list input numsamp) nil 1 nil nil))))

(define SOS
  (lambda (input a0 a1 a2 b1 b2)
    (mk-ugen (list "SOS" (list 0) (list input a0 a1 a2 b1 b2) nil 1 nil nil))))

(define SampleDur (mk-ugen (list "SampleDur" ir nil nil 1 nil nil)))

(define SampleRate (mk-ugen (list "SampleRate" ir nil nil 1 nil nil)))

(define Sanitize
  (lambda (input replace)
    (mk-ugen (list "Sanitize" (list 0) (list input replace) nil 1 nil nil))))

(define Saw
  (lambda (rt freq)
    (mk-ugen (list "Saw" rt (list freq) nil 1 nil nil))))

(define Schmidt
  (lambda (input lo hi)
    (mk-ugen (list "Schmidt" (list 0) (list input lo hi) nil 1 nil nil))))

(define Select
  (lambda (which array)
    (mk-ugen (list "Select" (list 0 1) (list which) array 1 nil nil))))

(define SendTrig
  (lambda (input id_ value)
    (mk-ugen (list "SendTrig" (list 0) (list input id_ value) nil 0 nil nil))))

(define SetResetFF
  (lambda (trig_ reset)
    (mk-ugen (list "SetResetFF" (list 0 1) (list trig_ reset) nil 1 nil nil))))

(define Shaper
  (lambda (bufnum input)
    (mk-ugen (list "Shaper" (list 1) (list bufnum input) nil 1 nil nil))))

(define SinOsc
  (lambda (rt freq phase)
    (mk-ugen (list "SinOsc" rt (list freq phase) nil 1 nil nil))))

(define SinOscFB
  (lambda (rt freq feedback)
    (mk-ugen (list "SinOscFB" rt (list freq feedback) nil 1 nil nil))))

(define Slew
  (lambda (input up dn)
    (mk-ugen (list "Slew" (list 0) (list input up dn) nil 1 nil nil))))

(define Slope
  (lambda (input)
    (mk-ugen (list "Slope" (list 0) (list input) nil 1 nil nil))))

(define SpecCentroid
  (lambda (rt buffer)
    (mk-ugen (list "SpecCentroid" rt (list buffer) nil 1 nil nil))))

(define SpecFlatness
  (lambda (rt buffer)
    (mk-ugen (list "SpecFlatness" rt (list buffer) nil 1 nil nil))))

(define SpecPcile
  (lambda (rt buffer fraction interpolate)
    (mk-ugen (list "SpecPcile" rt (list buffer fraction interpolate) nil 1 nil nil))))

(define Spring
  (lambda (rt input spring damp)
    (mk-ugen (list "Spring" rt (list input spring damp) nil 1 nil nil))))

(define StandardL
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardL" rt (list freq k xi yi) nil 1 nil nil))))

(define StandardN
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardN" rt (list freq k xi yi) nil 1 nil nil))))

(define Stepper
  (lambda (trig_ reset min_ max_ step resetval)
    (mk-ugen (list "Stepper" (list 0) (list trig_ reset min_ max_ step resetval) nil 1 nil nil))))

(define StereoConvolution2L
  (lambda (rt input kernelL kernelR trigger framesize crossfade)
    (mk-ugen (list "StereoConvolution2L" rt (list input kernelL kernelR trigger framesize crossfade) nil 2 nil nil))))

(define SubsampleOffset (mk-ugen (list "SubsampleOffset" ir nil nil 1 nil nil)))

(define Sum3
  (lambda (in0 in1 in2)
    (mk-ugen (list "Sum3" (list 0 1 2) (list in0 in1 in2) nil 1 nil nil))))

(define Sum4
  (lambda (in0 in1 in2 in3)
    (mk-ugen (list "Sum4" (list 0 1 2 3) (list in0 in1 in2 in3) nil 1 nil nil))))

(define Sweep
  (lambda (trig_ rate_)
    (mk-ugen (list "Sweep" (list 0) (list trig_ rate_) nil 1 nil nil))))

(define SyncSaw
  (lambda (rt syncFreq sawFreq)
    (mk-ugen (list "SyncSaw" rt (list syncFreq sawFreq) nil 1 nil nil))))

(define T2A
  (lambda (input offset)
    (mk-ugen (list "T2A" ar (list input offset) nil 1 nil nil))))

(define T2K
  (lambda (input)
    (mk-ugen (list "T2K" kr (list input) nil 1 nil nil))))

(define TBall
  (lambda (rt input g damp friction)
    (mk-ugen (list "TBall" rt (list input g damp friction) nil 1 nil nil))))

(define TDelay
  (lambda (input dur)
    (mk-ugen (list "TDelay" (list 0) (list input dur) nil 1 nil nil))))

(define TDuty
  (lambda (rt dur reset doneAction level gapFirst)
    (mk-ugen (list "TDuty" rt (list dur reset doneAction level gapFirst) nil 1 nil nil))))

(define TExpRand
  (lambda (lo hi trig_)
    (mk-ugen (list "TExpRand" (list 2) (list lo hi trig_) nil 1 nil (unique-uid)))))

(define TGrains
  (lambda (nc trigger bufnum rate_ centerPos dur pan amp interp)
    (mk-ugen (list "TGrains" ar (list trigger bufnum rate_ centerPos dur pan amp interp) nil nc nil nil))))

(define TIRand
  (lambda (lo hi trig_)
    (mk-ugen (list "TIRand" (list 2) (list lo hi trig_) nil 1 nil (unique-uid)))))

(define TRand
  (lambda (lo hi trig_)
    (mk-ugen (list "TRand" (list 2) (list lo hi trig_) nil 1 nil (unique-uid)))))

(define TWindex
  (lambda (input normalize array)
    (mk-ugen (list "TWindex" (list 0) (list input normalize) array 1 nil (unique-uid)))))

(define Timer
  (lambda (trig_)
    (mk-ugen (list "Timer" (list 0) (list trig_) nil 1 nil nil))))

(define ToggleFF
  (lambda (trig_)
    (mk-ugen (list "ToggleFF" (list 0) (list trig_) nil 1 nil nil))))

(define Trig
  (lambda (input dur)
    (mk-ugen (list "Trig" (list 0) (list input dur) nil 1 nil nil))))

(define Trig1
  (lambda (input dur)
    (mk-ugen (list "Trig1" (list 0) (list input dur) nil 1 nil nil))))

(define TrigControl
  (lambda (rt values)
    (mk-ugen (list "TrigControl" rt (list values) nil 1 nil nil))))

(define TwoPole
  (lambda (input freq radius)
    (mk-ugen (list "TwoPole" (list 0) (list input freq radius) nil 1 nil nil))))

(define TwoZero
  (lambda (input freq radius)
    (mk-ugen (list "TwoZero" (list 0) (list input freq radius) nil 1 nil nil))))

(define UnaryOpUGen
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 nil nil))))

(define VDiskIn
  (lambda (nc bufnum rate_ loop sendID)
    (mk-ugen (list "VDiskIn" ar (list bufnum rate_ loop sendID) nil nc nil nil))))

(define VOsc
  (lambda (rt bufpos freq phase)
    (mk-ugen (list "VOsc" rt (list bufpos freq phase) nil 1 nil nil))))

(define VOsc3
  (lambda (rt bufpos freq1 freq2 freq3)
    (mk-ugen (list "VOsc3" rt (list bufpos freq1 freq2 freq3) nil 1 nil nil))))

(define VarLag
  (lambda (input time curvature warp start)
    (mk-ugen (list "VarLag" (list 0) (list input time curvature warp start) nil 1 nil nil))))

(define VarSaw
  (lambda (rt freq iphase width)
    (mk-ugen (list "VarSaw" rt (list freq iphase width) nil 1 nil nil))))

(define Vibrato
  (lambda (rt freq rate_ depth delay onset rateVariation depthVariation iphase trig_)
    (mk-ugen (list "Vibrato" rt (list freq rate_ depth delay onset rateVariation depthVariation iphase trig_) nil 1 nil (unique-uid)))))

(define Warp1
  (lambda (nc bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp)
    (mk-ugen (list "Warp1" ar (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp) nil nc nil nil))))

(define WhiteNoise
  (lambda (rt)
    (mk-ugen (list "WhiteNoise" rt nil nil 1 nil (unique-uid)))))

(define Wrap
  (lambda (input lo hi)
    (mk-ugen (list "Wrap" (list 0) (list input lo hi) nil 1 nil nil))))

(define WrapIndex
  (lambda (bufnum input)
    (mk-ugen (list "WrapIndex" (list 1) (list bufnum input) nil 1 nil nil))))

(define XFade2
  (lambda (inA inB pan level)
    (mk-ugen (list "XFade2" (list 0 1) (list inA inB pan level) nil 1 nil nil))))

(define XLine
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "XLine" rt (list start end dur doneAction) nil 1 nil nil))))

(define XOut
  (lambda (bus xfade input)
    (mk-ugen (list "XOut" (list 2) (list bus xfade) input 0 nil nil))))

(define ZeroCrossing
  (lambda (input)
    (mk-ugen (list "ZeroCrossing" (list 0) (list input) nil 1 nil nil))))

(define MaxLocalBufs
  (lambda (count)
    (mk-ugen (list "MaxLocalBufs" kr (list count) nil 1 nil nil))))

(define MulAdd
  (lambda (input mul add)
    (mk-ugen (list "MulAdd" (list 0 1 2) (list input mul add) nil 1 nil nil))))

(define SetBuf
  (lambda (buf offset length_ array)
    (mk-ugen (list "SetBuf" ir (list buf offset length_) array 1 nil nil))))

(define A2B
  (lambda (rt a b c d)
    (mk-ugen (list "A2B" rt (list a b c d) nil 4 nil nil))))

(define AY
  (lambda (rt tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype)
    (mk-ugen (list "AY" rt (list tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype) nil 1 nil nil))))

(define Allpass1
  (lambda (rt input freq)
    (mk-ugen (list "Allpass1" rt (list input freq) nil 1 nil nil))))

(define Allpass2
  (lambda (rt input freq rq)
    (mk-ugen (list "Allpass2" rt (list input freq rq) nil 1 nil nil))))

(define AmplitudeMod
  (lambda (rt input attackTime releaseTime)
    (mk-ugen (list "AmplitudeMod" rt (list input attackTime releaseTime) nil 1 nil nil))))

(define AnalogBassDrum
  (lambda (rt trig_ infsustain accent freq tone decay_ attackfm selffm)
    (mk-ugen (list "AnalogBassDrum" rt (list trig_ infsustain accent freq tone decay_ attackfm selffm) nil 1 nil nil))))

(define AnalogPhaser
  (lambda (input lfoinput skew feedback modulation stages)
    (mk-ugen (list "AnalogPhaser" (list 0) (list input lfoinput skew feedback modulation stages) nil 1 nil nil))))

(define AnalogPhaserMod
  (lambda (input skew modulation stages)
    (mk-ugen (list "AnalogPhaserMod" (list 0) (list input skew modulation stages) nil 1 nil nil))))

(define AnalogSnareDrum
  (lambda (rt trig_ infsustain accent freq tone decay_ snappy)
    (mk-ugen (list "AnalogSnareDrum" rt (list trig_ infsustain accent freq tone decay_ snappy) nil 1 nil nil))))

(define AnalogTape
  (lambda (input bias saturation drive oversample mode)
    (mk-ugen (list "AnalogTape" (list 0) (list input bias saturation drive oversample mode) nil 1 nil nil))))

(define AnalogVintageDistortion
  (lambda (input drivegain bias lowgain highgain shelvingfreq oversample)
    (mk-ugen (list "AnalogVintageDistortion" (list 0) (list input drivegain bias lowgain highgain shelvingfreq oversample) nil 1 nil nil))))

(define AnalyseEvents2
  (lambda (rt input bufnum threshold triggerid circular pitch)
    (mk-ugen (list "AnalyseEvents2" rt (list input bufnum threshold triggerid circular pitch) nil 1 nil nil))))

(define ArneodoCoulletTresser
  (lambda (rt freq alpha h xi yi zi)
    (mk-ugen (list "ArneodoCoulletTresser" rt (list freq alpha h xi yi zi) nil 3 nil nil))))

(define ArrayMax
  (lambda (array)
    (mk-ugen (list "ArrayMax" (list 0) nil array 2 nil nil))))

(define ArrayMin
  (lambda (array)
    (mk-ugen (list "ArrayMin" (list 0) nil array 2 nil nil))))

(define AtsAmp
  (lambda (rt atsbuffer partialNum filePointer)
    (mk-ugen (list "AtsAmp" rt (list atsbuffer partialNum filePointer) nil 1 nil nil))))

(define AtsBand
  (lambda (rt atsbuffer band filePointer)
    (mk-ugen (list "AtsBand" rt (list atsbuffer band filePointer) nil 1 nil nil))))

(define AtsFreq
  (lambda (rt atsbuffer partialNum filePointer)
    (mk-ugen (list "AtsFreq" rt (list atsbuffer partialNum filePointer) nil 1 nil nil))))

(define AtsNoiSynth
  (lambda (rt atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip)
    (mk-ugen (list "AtsNoiSynth" rt (list atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip) nil 1 nil nil))))

(define AtsNoise
  (lambda (rt atsbuffer bandNum filePointer)
    (mk-ugen (list "AtsNoise" rt (list atsbuffer bandNum filePointer) nil 1 nil nil))))

(define AtsParInfo
  (lambda (rt atsbuffer partialNum filePointer)
    (mk-ugen (list "AtsParInfo" rt (list atsbuffer partialNum filePointer) nil 2 nil nil))))

(define AtsPartial
  (lambda (rt atsbuffer partial filePointer freqMul freqAdd)
    (mk-ugen (list "AtsPartial" rt (list atsbuffer partial filePointer freqMul freqAdd) nil 1 nil nil))))

(define AtsSynth
  (lambda (rt atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd)
    (mk-ugen (list "AtsSynth" rt (list atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd) nil 1 nil nil))))

(define AtsUGen
  (lambda (rt maxSize)
    (mk-ugen (list "AtsUGen" rt (list maxSize) nil 1 nil nil))))

(define AttackSlope
  (lambda (rt input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged)
    (mk-ugen (list "AttackSlope" rt (list input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged) nil 6 nil nil))))

(define AudioMSG
  (lambda (input index)
    (mk-ugen (list "AudioMSG" (list 0) (list input index) nil 1 nil nil))))

(define AverageOutput
  (lambda (input trig_)
    (mk-ugen (list "AverageOutput" (list 0) (list input trig_) nil 1 nil nil))))

(define B2A
  (lambda (rt w x y z)
    (mk-ugen (list "B2A" rt (list w x y z) nil 4 nil nil))))

(define B2Ster
  (lambda (rt w x y)
    (mk-ugen (list "B2Ster" rt (list w x y) nil 2 nil nil))))

(define B2UHJ
  (lambda (rt w x y)
    (mk-ugen (list "B2UHJ" rt (list w x y) nil 2 nil nil))))

(define BBlockerBuf
  (lambda (rt freq bufnum startpoint)
    (mk-ugen (list "BBlockerBuf" rt (list freq bufnum startpoint) nil 9 nil nil))))

(define BFDecode1
  (lambda (rt w x y z azimuth elevation wComp)
    (mk-ugen (list "BFDecode1" rt (list w x y z azimuth elevation wComp) nil 1 nil nil))))

(define BFDecoder
  (lambda (rt maxSize)
    (mk-ugen (list "BFDecoder" rt (list maxSize) nil 1 nil nil))))

(define BFEncode1
  (lambda (rt input azimuth elevation rho gain wComp)
    (mk-ugen (list "BFEncode1" rt (list input azimuth elevation rho gain wComp) nil 4 nil nil))))

(define BFEncode2
  (lambda (rt input point_x point_y elevation gain wComp)
    (mk-ugen (list "BFEncode2" rt (list input point_x point_y elevation gain wComp) nil 4 nil nil))))

(define BFEncodeSter
  (lambda (rt l r azimuth width elevation rho gain wComp)
    (mk-ugen (list "BFEncodeSter" rt (list l r azimuth width elevation rho gain wComp) nil 4 nil nil))))

(define BFGrainPanner
  (lambda (rt maxSize)
    (mk-ugen (list "BFGrainPanner" rt (list maxSize) nil 1 nil nil))))

(define BFManipulate
  (lambda (rt w x y z rotate tilt tumble)
    (mk-ugen (list "BFManipulate" rt (list w x y z rotate tilt tumble) nil 4 nil nil))))

(define BFPanner
  (lambda (rt maxSize)
    (mk-ugen (list "BFPanner" rt (list maxSize) nil 1 nil nil))))

(define BLBufRd
  (lambda (rt bufnum phase ratio)
    (mk-ugen (list "BLBufRd" rt (list bufnum phase ratio) nil 1 nil nil))))

(define BLOsc
  (lambda (rt freq pulsewidth waveform)
    (mk-ugen (list "BLOsc" rt (list freq pulsewidth waveform) nil 1 nil nil))))

(define BMoog
  (lambda (input freq q mode saturation)
    (mk-ugen (list "BMoog" (list 0) (list input freq q mode saturation) nil 1 nil nil))))

(define Balance
  (lambda (rt input test hp stor)
    (mk-ugen (list "Balance" rt (list input test hp stor) nil 1 nil nil))))

(define BeatStatistics
  (lambda (rt fft leak numpreviousbeats)
    (mk-ugen (list "BeatStatistics" rt (list fft leak numpreviousbeats) nil 4 nil nil))))

(define BinData
  (lambda (rt buffer bin overlaps)
    (mk-ugen (list "BinData" rt (list buffer bin overlaps) nil 2 nil nil))))

(define BlitB3
  (lambda (rt freq)
    (mk-ugen (list "BlitB3" rt (list freq) nil 1 nil nil))))

(define BlitB3D
  (lambda (rt freq)
    (mk-ugen (list "BlitB3D" rt (list freq) nil 1 nil nil))))

(define BlitB3Saw
  (lambda (rt freq leak)
    (mk-ugen (list "BlitB3Saw" rt (list freq leak) nil 1 nil nil))))

(define BlitB3Square
  (lambda (rt freq leak)
    (mk-ugen (list "BlitB3Square" rt (list freq leak) nil 1 nil nil))))

(define BlitB3Tri
  (lambda (rt freq leak leak2)
    (mk-ugen (list "BlitB3Tri" rt (list freq leak leak2) nil 1 nil nil))))

(define Breakcore
  (lambda (rt bufnum capturein capturetrigger duration ampdropout)
    (mk-ugen (list "Breakcore" rt (list bufnum capturein capturetrigger duration ampdropout) nil 1 nil nil))))

(define Brusselator
  (lambda (rt reset rate_ mu gamma initx inity)
    (mk-ugen (list "Brusselator" rt (list reset rate_ mu gamma initx inity) nil 2 nil nil))))

(define BufGrain
  (lambda (rt trigger dur sndbuf rate_ pos interp)
    (mk-ugen (list "BufGrain" rt (list trigger dur sndbuf rate_ pos interp) nil 1 nil nil))))

(define BufGrainB
  (lambda (rt trigger dur sndbuf rate_ pos envbuf interp)
    (mk-ugen (list "BufGrainB" rt (list trigger dur sndbuf rate_ pos envbuf interp) nil 1 nil nil))))

(define BufGrainBBF
  (lambda (rt trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp)
    (mk-ugen (list "BufGrainBBF" rt (list trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp) nil 4 nil nil))))

(define BufGrainBF
  (lambda (rt trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp)
    (mk-ugen (list "BufGrainBF" rt (list trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp) nil 4 nil nil))))

(define BufGrainI
  (lambda (rt trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp)
    (mk-ugen (list "BufGrainI" rt (list trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp) nil 1 nil nil))))

(define BufGrainIBF
  (lambda (rt trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp)
    (mk-ugen (list "BufGrainIBF" rt (list trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp) nil 4 nil nil))))

(define BufMax
  (lambda (rt bufnum gate_)
    (mk-ugen (list "BufMax" rt (list bufnum gate_) nil 2 nil nil))))

(define BufMin
  (lambda (rt bufnum gate_)
    (mk-ugen (list "BufMin" rt (list bufnum gate_) nil 2 nil nil))))

(define CQ_Diff
  (lambda (rt in1 in2 databufnum)
    (mk-ugen (list "CQ_Diff" rt (list in1 in2 databufnum) nil 1 nil nil))))

(define Cepstrum
  (lambda (rt cepbuf fftchain)
    (mk-ugen (list "Cepstrum" rt (list cepbuf fftchain) nil 1 nil nil))))

(define Chen
  (lambda (rt speed a b c)
    (mk-ugen (list "Chen" rt (list speed a b c) nil 3 nil nil))))

(define Chromagram
  (lambda (rt fft fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize)
    (mk-ugen (list "Chromagram" rt (list fft fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize) nil 12 nil nil))))

(define CircleRamp
  (lambda (rt input lagTime circmin circmax)
    (mk-ugen (list "CircleRamp" rt (list input lagTime circmin circmax) nil 1 nil nil))))

(define Clipper32
  (lambda (rt input lo hi)
    (mk-ugen (list "Clipper32" rt (list input lo hi) nil 1 nil nil))))

(define Clipper4
  (lambda (rt input lo hi)
    (mk-ugen (list "Clipper4" rt (list input lo hi) nil 1 nil nil))))

(define Clipper8
  (lambda (rt input lo hi)
    (mk-ugen (list "Clipper8" rt (list input lo hi) nil 1 nil nil))))

(define Clockmus
  (lambda (rt)
    (mk-ugen (list "Clockmus" rt nil nil 1 nil nil))))

(define CombLP
  (lambda (rt input gate_ maxdelaytime delaytime decaytime coef)
    (mk-ugen (list "CombLP" rt (list input gate_ maxdelaytime delaytime decaytime coef) nil 1 nil nil))))

(define ComplexRes
  (lambda (input freq decay_)
    (mk-ugen (list "ComplexRes" (list 0) (list input freq decay_) nil 1 nil nil))))

(define Concat
  (lambda (rt control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore)
    (mk-ugen (list "Concat" rt (list control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore) nil 1 nil nil))))

(define Concat2
  (lambda (rt control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold)
    (mk-ugen (list "Concat2" rt (list control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold) nil 1 nil nil))))

(define Coyote
  (lambda (rt input trackFall slowLag fastLag fastMul thresh minDur)
    (mk-ugen (list "Coyote" rt (list input trackFall slowLag fastLag fastMul thresh minDur) nil 1 nil nil))))

(define Crest
  (lambda (rt input numsamps gate_)
    (mk-ugen (list "Crest" rt (list input numsamps gate_) nil 1 nil nil))))

(define CrossoverDistortion
  (lambda (input amp smooth)
    (mk-ugen (list "CrossoverDistortion" (list 0) (list input amp smooth) nil 1 nil nil))))

(define DCompressor
  (lambda (input sidechainIn sidechain ratio threshold attack release makeup automakeup)
    (mk-ugen (list "DCompressor" (list 0) (list input sidechainIn sidechain ratio threshold attack release makeup automakeup) nil 1 nil nil))))

(define DFM1
  (lambda (input freq res inputgain type_ noiselevel)
    (mk-ugen (list "DFM1" (list 0) (list input freq res inputgain type_ noiselevel) nil 1 nil nil))))

(define DNoiseRing
  (lambda (change chance shift numBits resetval)
    (mk-ugen (list "DNoiseRing" dr (list change chance shift numBits resetval) nil 1 nil (unique-uid)))))

(define DPW3Tri
  (lambda (rt freq)
    (mk-ugen (list "DPW3Tri" rt (list freq) nil 1 nil nil))))

(define DPW4Saw
  (lambda (rt freq)
    (mk-ugen (list "DPW4Saw" rt (list freq) nil 1 nil nil))))

(define DWGBowed
  (lambda (rt freq velb force gate_ pos release c1 c3 impZ fB)
    (mk-ugen (list "DWGBowed" rt (list freq velb force gate_ pos release c1 c3 impZ fB) nil 1 nil nil))))

(define DWGBowedSimple
  (lambda (rt freq velb force gate_ pos release c1 c3)
    (mk-ugen (list "DWGBowedSimple" rt (list freq velb force gate_ pos release c1 c3) nil 1 nil nil))))

(define DWGBowedTor
  (lambda (rt freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor)
    (mk-ugen (list "DWGBowedTor" rt (list freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor) nil 1 nil nil))))

(define DWGClarinet3
  (lambda (rt freq pm pc m gate_ release c1 c3)
    (mk-ugen (list "DWGClarinet3" rt (list freq pm pc m gate_ release c1 c3) nil 1 nil nil))))

(define DWGFlute
  (lambda (rt freq pm endr jetr jetRa gate_ release)
    (mk-ugen (list "DWGFlute" rt (list freq pm endr jetr jetRa gate_ release) nil 1 nil nil))))

(define DWGPlucked
  (lambda (rt freq amp gate_ pos c1 c3 inp release)
    (mk-ugen (list "DWGPlucked" rt (list freq amp gate_ pos c1 c3 inp release) nil 1 nil nil))))

(define DWGPlucked2
  (lambda (rt freq amp gate_ pos c1 c3 inp release mistune mp gc)
    (mk-ugen (list "DWGPlucked2" rt (list freq amp gate_ pos c1 c3 inp release mistune mp gc) nil 1 nil nil))))

(define DWGPluckedStiff
  (lambda (rt freq amp gate_ pos c1 c3 inp release fB)
    (mk-ugen (list "DWGPluckedStiff" rt (list freq amp gate_ pos c1 c3 inp release fB) nil 1 nil nil))))

(define DWGSoundBoard
  (lambda (inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8)
    (mk-ugen (list "DWGSoundBoard" (list 0) (list inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8) nil 1 nil nil))))

(define Dbrown2
  (lambda (rt lo hi step dist length_)
    (mk-ugen (list "Dbrown2" rt (list lo hi step dist length_) nil 1 nil nil))))

(define DbufTag
  (lambda (bufnum v axiom rules recycle mode)
    (mk-ugen (list "DbufTag" dr (list bufnum v axiom rules recycle mode) nil 1 nil (unique-uid)))))

(define Decimator
  (lambda (rt input rate_ bits)
    (mk-ugen (list "Decimator" rt (list input rate_ bits) nil 1 nil nil))))

(define DetaBlockerBuf
  (lambda (bufnum startpoint)
    (mk-ugen (list "DetaBlockerBuf" dr (list bufnum startpoint) nil 1 nil (unique-uid)))))

(define Dfsm
  (lambda (rules n rgen)
    (mk-ugen (list "Dfsm" dr (list rules n rgen) nil 1 nil (unique-uid)))))

(define Dgauss
  (lambda (rt lo hi length_)
    (mk-ugen (list "Dgauss" rt (list lo hi length_) nil 1 nil nil))))

(define DiodeRingMod
  (lambda (car mod_)
    (mk-ugen (list "DiodeRingMod" (list 0) (list car mod_) nil 1 nil nil))))

(define Disintegrator
  (lambda (input probability multiplier)
    (mk-ugen (list "Disintegrator" (list 0) (list input probability multiplier) nil 1 nil (unique-uid)))))

(define Dneuromodule
  (lambda (nc dt theta x weights)
    (mk-ugen (list "Dneuromodule" dr (list dt) theta x weights nc nil (unique-uid)))))

(define DoubleNestedAllpassC
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3)
    (mk-ugen (list "DoubleNestedAllpassC" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) nil 1 nil nil))))

(define DoubleNestedAllpassL
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3)
    (mk-ugen (list "DoubleNestedAllpassL" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) nil 1 nil nil))))

(define DoubleNestedAllpassN
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3)
    (mk-ugen (list "DoubleNestedAllpassN" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) nil 1 nil nil))))

(define DoubleWell
  (lambda (rt reset ratex ratey f w delta initx inity)
    (mk-ugen (list "DoubleWell" rt (list reset ratex ratey f w delta initx inity) nil 1 nil nil))))

(define DoubleWell2
  (lambda (rt reset ratex ratey f w delta initx inity)
    (mk-ugen (list "DoubleWell2" rt (list reset ratex ratey f w delta initx inity) nil 1 nil nil))))

(define DoubleWell3
  (lambda (rt reset rate_ f delta initx inity)
    (mk-ugen (list "DoubleWell3" rt (list reset rate_ f delta initx inity) nil 1 nil nil))))

(define DriveNoise
  (lambda (rt input amount multi)
    (mk-ugen (list "DriveNoise" rt (list input amount multi) nil 1 nil nil))))

(define DrumTrack
  (lambda (rt input lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode)
    (mk-ugen (list "DrumTrack" rt (list input lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode) nil 4 nil nil))))

(define Dtag
  (lambda (rt bufsize v axiom rules recycle mode)
    (mk-ugen (list "Dtag" rt (list bufsize v axiom rules recycle mode) nil 1 nil nil))))

(define EnvDetect
  (lambda (rt input attack release)
    (mk-ugen (list "EnvDetect" rt (list input attack release) nil 1 nil nil))))

(define EnvFollow
  (lambda (rt input decaycoeff)
    (mk-ugen (list "EnvFollow" rt (list input decaycoeff) nil 1 nil nil))))

(define FFTComplexDev
  (lambda (rt buffer rectify powthresh)
    (mk-ugen (list "FFTComplexDev" rt (list buffer rectify powthresh) nil 1 nil nil))))

(define FFTCrest
  (lambda (rt buffer freqlo freqhi)
    (mk-ugen (list "FFTCrest" rt (list buffer freqlo freqhi) nil 1 nil nil))))

(define FFTDiffMags
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "FFTDiffMags" rt (list bufferA bufferB) nil 1 nil nil))))

(define FFTFlux
  (lambda (rt buffer normalise)
    (mk-ugen (list "FFTFlux" rt (list buffer normalise) nil 1 nil nil))))

(define FFTFluxPos
  (lambda (rt buffer normalise)
    (mk-ugen (list "FFTFluxPos" rt (list buffer normalise) nil 1 nil nil))))

(define FFTMKL
  (lambda (rt buffer epsilon)
    (mk-ugen (list "FFTMKL" rt (list buffer epsilon) nil 1 nil nil))))

(define FFTPeak
  (lambda (rt buffer freqlo freqhi)
    (mk-ugen (list "FFTPeak" rt (list buffer freqlo freqhi) nil 2 nil nil))))

(define FFTPhaseDev
  (lambda (rt buffer weight powthresh)
    (mk-ugen (list "FFTPhaseDev" rt (list buffer weight powthresh) nil 1 nil nil))))

(define FFTPower
  (lambda (rt buffer square)
    (mk-ugen (list "FFTPower" rt (list buffer square) nil 1 nil nil))))

(define FFTSlope
  (lambda (rt buffer)
    (mk-ugen (list "FFTSlope" rt (list buffer) nil 1 nil nil))))

(define FFTSpread
  (lambda (rt buffer centroid)
    (mk-ugen (list "FFTSpread" rt (list buffer centroid) nil 1 nil nil))))

(define FFTSubbandFlatness
  (lambda (rt chain cutfreqs)
    (mk-ugen (list "FFTSubbandFlatness" rt (list chain cutfreqs) nil 1 nil nil))))

(define FFTSubbandFlux
  (lambda (rt chain cutfreqs posonly)
    (mk-ugen (list "FFTSubbandFlux" rt (list chain cutfreqs posonly) nil 1 nil nil))))

(define FFTSubbandPower
  (lambda (rt chain cutfreqs square scalemode)
    (mk-ugen (list "FFTSubbandPower" rt (list chain cutfreqs square scalemode) nil 1 nil nil))))

(define FM7
  (lambda (rt ctlMatrix modMatrix)
    (mk-ugen (list "FM7" rt nil ctlMatrix modMatrix 6 nil nil))))

(define FMGrain
  (lambda (trigger dur carfreq modfreq index)
    (mk-ugen (list "FMGrain" (list 0) (list trigger dur carfreq modfreq index) nil 1 nil nil))))

(define FMGrainB
  (lambda (trigger dur carfreq modfreq index envbuf)
    (mk-ugen (list "FMGrainB" (list 0) (list trigger dur carfreq modfreq index envbuf) nil 1 nil nil))))

(define FMGrainBBF
  (lambda (rt trigger dur carfreq modfreq index envbuf azimuth elevation rho wComp)
    (mk-ugen (list "FMGrainBBF" rt (list trigger dur carfreq modfreq index envbuf azimuth elevation rho wComp) nil 4 nil nil))))

(define FMGrainBF
  (lambda (rt trigger dur carfreq modfreq index azimuth elevation rho wComp)
    (mk-ugen (list "FMGrainBF" rt (list trigger dur carfreq modfreq index azimuth elevation rho wComp) nil 4 nil nil))))

(define FMGrainI
  (lambda (rt trigger dur carfreq modfreq index envbuf1 envbuf2 ifac)
    (mk-ugen (list "FMGrainI" rt (list trigger dur carfreq modfreq index envbuf1 envbuf2 ifac) nil 1 nil nil))))

(define FMGrainIBF
  (lambda (rt trigger dur carfreq modfreq index envbuf1 envbuf2 ifac azimuth elevation rho wComp)
    (mk-ugen (list "FMGrainIBF" rt (list trigger dur carfreq modfreq index envbuf1 envbuf2 ifac azimuth elevation rho wComp) nil 4 nil nil))))

(define FMHDecode1
  (lambda (rt w x y z r s t u v azimuth elevation)
    (mk-ugen (list "FMHDecode1" rt (list w x y z r s t u v azimuth elevation) nil 1 nil nil))))

(define FMHEncode0
  (lambda (rt input azimuth elevation gain)
    (mk-ugen (list "FMHEncode0" rt (list input azimuth elevation gain) nil 9 nil nil))))

(define FMHEncode1
  (lambda (rt input azimuth elevation rho gain wComp)
    (mk-ugen (list "FMHEncode1" rt (list input azimuth elevation rho gain wComp) nil 9 nil nil))))

(define FMHEncode2
  (lambda (rt input point_x point_y elevation gain wComp)
    (mk-ugen (list "FMHEncode2" rt (list input point_x point_y elevation gain wComp) nil 9 nil nil))))

(define FeatureSave
  (lambda (rt features trig_)
    (mk-ugen (list "FeatureSave" rt (list features trig_) nil 1 nil nil))))

(define Fhn2DC
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (mk-ugen (list "Fhn2DC" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil nil))))

(define Fhn2DL
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (mk-ugen (list "Fhn2DL" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil nil))))

(define Fhn2DN
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (mk-ugen (list "Fhn2DN" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil nil))))

(define FhnTrig
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (mk-ugen (list "FhnTrig" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil nil))))

(define FincoSprottL
  (lambda (rt freq a h xi yi zi)
    (mk-ugen (list "FincoSprottL" rt (list freq a h xi yi zi) nil 3 nil nil))))

(define FincoSprottM
  (lambda (rt freq a b h xi yi zi)
    (mk-ugen (list "FincoSprottM" rt (list freq a b h xi yi zi) nil 3 nil nil))))

(define FincoSprottS
  (lambda (rt freq a b h xi yi zi)
    (mk-ugen (list "FincoSprottS" rt (list freq a b h xi yi zi) nil 3 nil nil))))

(define FitzHughNagumo
  (lambda (rt reset rateu ratew b0 b1 initu initw)
    (mk-ugen (list "FitzHughNagumo" rt (list reset rateu ratew b0 b1 initu initw) nil 1 nil nil))))

(define FrameCompare
  (lambda (rt buffer1 buffer2 wAmount)
    (mk-ugen (list "FrameCompare" rt (list buffer1 buffer2 wAmount) nil 1 nil nil))))

(define Friction
  (lambda (rt input friction spring damp mass beltmass)
    (mk-ugen (list "Friction" rt (list input friction spring damp mass beltmass) nil 1 nil nil))))

(define Gammatone
  (lambda (input centrefrequency bandwidth)
    (mk-ugen (list "Gammatone" (list 0) (list input centrefrequency bandwidth) nil 1 nil nil))))

(define GaussClass
  (lambda (rt input bufnum gate_)
    (mk-ugen (list "GaussClass" rt (list input bufnum gate_) nil 1 nil nil))))

(define GaussTrig
  (lambda (rt freq dev)
    (mk-ugen (list "GaussTrig" rt (list freq dev) nil 1 nil nil))))

(define Gbman2DC
  (lambda (rt minfreq maxfreq x0 y0)
    (mk-ugen (list "Gbman2DC" rt (list minfreq maxfreq x0 y0) nil 1 nil nil))))

(define Gbman2DL
  (lambda (rt minfreq maxfreq x0 y0)
    (mk-ugen (list "Gbman2DL" rt (list minfreq maxfreq x0 y0) nil 1 nil nil))))

(define Gbman2DN
  (lambda (rt minfreq maxfreq x0 y0)
    (mk-ugen (list "Gbman2DN" rt (list minfreq maxfreq x0 y0) nil 1 nil nil))))

(define GbmanTrig
  (lambda (rt minfreq maxfreq x0 y0)
    (mk-ugen (list "GbmanTrig" rt (list minfreq maxfreq x0 y0) nil 1 nil nil))))

(define Gendy4
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy4" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil nil))))

(define Gendy5
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy5" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil nil))))

(define Getenv
  (lambda (rt key defaultval)
    (mk-ugen (list "Getenv" rt (list key defaultval) nil 1 nil nil))))

(define GlitchBPF
  (lambda (rt input freq rq)
    (mk-ugen (list "GlitchBPF" rt (list input freq rq) nil 1 nil nil))))

(define GlitchBRF
  (lambda (rt input freq rq)
    (mk-ugen (list "GlitchBRF" rt (list input freq rq) nil 1 nil nil))))

(define GlitchHPF
  (lambda (rt input freq)
    (mk-ugen (list "GlitchHPF" rt (list input freq) nil 1 nil nil))))

(define GlitchRHPF
  (lambda (rt input freq rq)
    (mk-ugen (list "GlitchRHPF" rt (list input freq rq) nil 1 nil nil))))

(define Goertzel
  (lambda (rt input bufsize freq hop)
    (mk-ugen (list "Goertzel" rt (list input bufsize freq hop) nil 2 nil nil))))

(define GrainBufJ
  (lambda (rt numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains)
    (mk-ugen (list "GrainBufJ" rt (list numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains) nil 1 nil nil))))

(define GrainFMJ
  (lambda (rt numChannels trigger dur carfreq modfreq index grainAmp pan envbufnum maxGrains)
    (mk-ugen (list "GrainFMJ" rt (list numChannels trigger dur carfreq modfreq index grainAmp pan envbufnum maxGrains) nil 1 nil nil))))

(define GrainInJ
  (lambda (rt numChannels trigger dur input grainAmp pan envbufnum maxGrains)
    (mk-ugen (list "GrainInJ" rt (list numChannels trigger dur input grainAmp pan envbufnum maxGrains) nil 1 nil nil))))

(define GrainSinJ
  (lambda (rt numChannels trigger dur freq grainAmp pan envbufnum maxGrains)
    (mk-ugen (list "GrainSinJ" rt (list numChannels trigger dur freq grainAmp pan envbufnum maxGrains) nil 1 nil nil))))

(define GravityGrid
  (lambda (rt reset rate_ newx newy bufnum)
    (mk-ugen (list "GravityGrid" rt (list reset rate_ newx newy bufnum) nil 1 nil nil))))

(define GravityGrid2
  (lambda (rt reset rate_ newx newy bufnum)
    (mk-ugen (list "GravityGrid2" rt (list reset rate_ newx newy bufnum) nil 1 nil nil))))

(define GreyholeRaw
  (lambda (in1 in2 damping delaytime diffusion feedback moddepth modfreq size)
    (mk-ugen (list "GreyholeRaw" (list 0 1) (list in1 in2 damping delaytime diffusion feedback moddepth modfreq size) nil 2 nil nil))))

(define HairCell
  (lambda (input spontaneousrate boostrate restorerate loss)
    (mk-ugen (list "HairCell" (list 0) (list input spontaneousrate boostrate restorerate loss) nil 1 nil nil))))

(define HarmonicOsc
  (lambda (rt freq firstharmonic amplitudes)
    (mk-ugen (list "HarmonicOsc" rt (list freq firstharmonic) amplitudes 1 nil nil))))

(define Henon2DC
  (lambda (rt minfreq maxfreq a b x0 y0)
    (mk-ugen (list "Henon2DC" rt (list minfreq maxfreq a b x0 y0) nil 1 nil nil))))

(define Henon2DL
  (lambda (rt minfreq maxfreq a b x0 y0)
    (mk-ugen (list "Henon2DL" rt (list minfreq maxfreq a b x0 y0) nil 1 nil nil))))

(define Henon2DN
  (lambda (rt minfreq maxfreq a b x0 y0)
    (mk-ugen (list "Henon2DN" rt (list minfreq maxfreq a b x0 y0) nil 1 nil nil))))

(define HenonTrig
  (lambda (rt minfreq maxfreq a b x0 y0)
    (mk-ugen (list "HenonTrig" rt (list minfreq maxfreq a b x0 y0) nil 1 nil nil))))

(define ICepstrum
  (lambda (rt cepchain fftbuf)
    (mk-ugen (list "ICepstrum" rt (list cepchain fftbuf) nil 1 nil nil))))

(define IIRFilter
  (lambda (input freq rq)
    (mk-ugen (list "IIRFilter" (list 0) (list input freq rq) nil 1 nil nil))))

(define InGrain
  (lambda (rt trigger dur input)
    (mk-ugen (list "InGrain" rt (list trigger dur input) nil 1 nil nil))))

(define InGrainB
  (lambda (rt trigger dur input envbuf)
    (mk-ugen (list "InGrainB" rt (list trigger dur input envbuf) nil 1 nil nil))))

(define InGrainBBF
  (lambda (rt trigger dur input envbuf azimuth elevation rho wComp)
    (mk-ugen (list "InGrainBBF" rt (list trigger dur input envbuf azimuth elevation rho wComp) nil 4 nil nil))))

(define InGrainBF
  (lambda (rt trigger dur input azimuth elevation rho wComp)
    (mk-ugen (list "InGrainBF" rt (list trigger dur input azimuth elevation rho wComp) nil 4 nil nil))))

(define InGrainI
  (lambda (rt trigger dur input envbuf1 envbuf2 ifac)
    (mk-ugen (list "InGrainI" rt (list trigger dur input envbuf1 envbuf2 ifac) nil 1 nil nil))))

(define InGrainIBF
  (lambda (rt trigger dur input envbuf1 envbuf2 ifac azimuth elevation rho wComp)
    (mk-ugen (list "InGrainIBF" rt (list trigger dur input envbuf1 envbuf2 ifac azimuth elevation rho wComp) nil 4 nil nil))))

(define InsideOut
  (lambda (rt input)
    (mk-ugen (list "InsideOut" rt (list input) nil 1 nil nil))))

(define Instruction
  (lambda (rt bufnum)
    (mk-ugen (list "Instruction" rt (list bufnum) nil 1 nil nil))))

(define JPverbRaw
  (lambda (in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60)
    (mk-ugen (list "JPverbRaw" (list 0) (list in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60) nil 2 nil nil))))

(define KMeansRT
  (lambda (rt bufnum inputdata k gate_ reset learn)
    (mk-ugen (list "KMeansRT" rt (list bufnum inputdata k gate_ reset learn) nil 1 nil nil))))

(define KeyClarity
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyClarity" rt (list chain keydecay chromaleak) nil 1 nil nil))))

(define KeyMode
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyMode" rt (list chain keydecay chromaleak) nil 1 nil nil))))

(define KmeansToBPSet1
  (lambda (rt freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum)
    (mk-ugen (list "KmeansToBPSet1" rt (list freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum) nil 1 nil nil))))

(define LADSPA
  (lambda (rt nChans id_ args)
    (mk-ugen (list "LADSPA" rt (list nChans id_ args) nil 0 nil nil))))

(define LFBrownNoise0
  (lambda (rt freq dev dist)
    (mk-ugen (list "LFBrownNoise0" rt (list freq dev dist) nil 1 nil (unique-uid)))))

(define LFBrownNoise1
  (lambda (rt freq dev dist)
    (mk-ugen (list "LFBrownNoise1" rt (list freq dev dist) nil 1 nil (unique-uid)))))

(define LFBrownNoise2
  (lambda (rt freq dev dist)
    (mk-ugen (list "LFBrownNoise2" rt (list freq dev dist) nil 1 nil (unique-uid)))))

(define LPCAnalyzer
  (lambda (input source n p testE delta windowtype)
    (mk-ugen (list "LPCAnalyzer" (list 0 1) (list input source n p testE delta windowtype) nil 1 nil nil))))

(define LPCError
  (lambda (rt input p)
    (mk-ugen (list "LPCError" rt (list input p) nil 1 nil nil))))

(define LPCSynth
  (lambda (buffer signal pointer)
    (mk-ugen (list "LPCSynth" ar (list buffer signal pointer) nil 1 nil nil))))

(define LPCVals
  (lambda (buffer pointer)
    (mk-ugen (list "LPCVals" ar (list buffer pointer) nil 3 nil nil))))

(define LPF1
  (lambda (rt input freq)
    (mk-ugen (list "LPF1" rt (list input freq) nil 1 nil nil))))

(define LPF18
  (lambda (rt input freq res dist)
    (mk-ugen (list "LPF18" rt (list input freq res dist) nil 1 nil nil))))

(define LPFVS6
  (lambda (rt input freq slope)
    (mk-ugen (list "LPFVS6" rt (list input freq slope) nil 1 nil nil))))

(define LPG
  (lambda (input controlinput controloffset controlscale vca resonance lowpassmode linearity)
    (mk-ugen (list "LPG" (list 0) (list input controlinput controloffset controlscale vca resonance lowpassmode linearity) nil 1 nil nil))))

(define LTI
  (lambda (rt input bufnuma bufnumb)
    (mk-ugen (list "LTI" rt (list input bufnuma bufnumb) nil 1 nil nil))))

(define Latoocarfian2DC
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (mk-ugen (list "Latoocarfian2DC" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil nil))))

(define Latoocarfian2DL
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (mk-ugen (list "Latoocarfian2DL" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil nil))))

(define Latoocarfian2DN
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (mk-ugen (list "Latoocarfian2DN" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil nil))))

(define LatoocarfianTrig
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (mk-ugen (list "LatoocarfianTrig" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil nil))))

(define ListTrig
  (lambda (rt bufnum reset offset numframes)
    (mk-ugen (list "ListTrig" rt (list bufnum reset offset numframes) nil 1 nil nil))))

(define ListTrig2
  (lambda (rt bufnum reset numframes)
    (mk-ugen (list "ListTrig2" rt (list bufnum reset numframes) nil 1 nil nil))))

(define Logger
  (lambda (rt inputArray trig_ bufnum reset)
    (mk-ugen (list "Logger" rt (list inputArray trig_ bufnum reset) nil 1 nil nil))))

(define LoopBuf
  (lambda (nc rt bufnum rate_ gate_ startPos startLoop endLoop interpolation)
    (mk-ugen (list "LoopBuf" rt (list bufnum rate_ gate_ startPos startLoop endLoop interpolation) nil nc nil nil))))

(define Lorenz2DC
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (mk-ugen (list "Lorenz2DC" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil nil))))

(define Lorenz2DL
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (mk-ugen (list "Lorenz2DL" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil nil))))

(define Lorenz2DN
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (mk-ugen (list "Lorenz2DN" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil nil))))

(define LorenzTrig
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (mk-ugen (list "LorenzTrig" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil nil))))

(define Lores
  (lambda (input freq res)
    (mk-ugen (list "Lores" (list 0) (list input freq res) nil 1 nil nil))))

(define LotkaVolterra
  (lambda (rt freq a b c d h xi yi)
    (mk-ugen (list "LotkaVolterra" rt (list freq a b c d h xi yi) nil 2 nil nil))))

(define MCLDChaosGen
  (lambda (rt maxSize)
    (mk-ugen (list "MCLDChaosGen" rt (list maxSize) nil 1 nil nil))))

(define MarkovSynth
  (lambda (rt input isRecording waitTime tableSize)
    (mk-ugen (list "MarkovSynth" rt (list input isRecording waitTime tableSize) nil 1 nil nil))))

(define MatchingP
  (lambda (rt dict input dictsize ntofind hop method)
    (mk-ugen (list "MatchingP" rt (list dict input dictsize ntofind hop method) nil 4 nil nil))))

(define MatchingPResynth
  (lambda (rt dict method trigger residual activs)
    (mk-ugen (list "MatchingPResynth" rt (list dict method trigger residual activs) nil 1 nil nil))))

;(define Max
;  (lambda (a b)
;    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 13 nil))))

(define Maxamp
  (lambda (rt input numSamps)
    (mk-ugen (list "Maxamp" rt (list input numSamps) nil 1 nil nil))))

(define MdaPiano
  (lambda (rt freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain)
    (mk-ugen (list "MdaPiano" rt (list freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain) nil 2 nil nil))))

(define MeanTriggered
  (lambda (rt input trig_ length_)
    (mk-ugen (list "MeanTriggered" rt (list input trig_ length_) nil 1 nil nil))))

(define Meddis
  (lambda (input)
    (mk-ugen (list "Meddis" (list 0) (list input) nil 1 nil nil))))

(define MedianSeparation
  (lambda (rt fft fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax)
    (mk-ugen (list "MedianSeparation" rt (list fft fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax) nil 2 nil nil))))

(define MedianTriggered
  (lambda (rt input trig_ length_)
    (mk-ugen (list "MedianTriggered" rt (list input trig_ length_) nil 1 nil nil))))

(define MembraneCircle
  (lambda (rt excitation tension loss)
    (mk-ugen (list "MembraneCircle" rt (list excitation tension loss) nil 1 nil nil))))

(define MembraneHexagon
  (lambda (rt excitation tension loss)
    (mk-ugen (list "MembraneHexagon" rt (list excitation tension loss) nil 1 nil nil))))

(define Metro
  (lambda (rt bpm numBeats)
    (mk-ugen (list "Metro" rt (list bpm numBeats) nil 1 nil nil))))

(define MiBraids
  (lambda (rt pitch timbre color model trig_ resamp decim bits ws)
    (mk-ugen (list "MiBraids" rt (list pitch timbre color model trig_ resamp decim bits ws) nil 1 nil nil))))

(define MiClouds
  (lambda (rt pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_ inputArray)
    (mk-ugen (list "MiClouds" rt (list pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_) inputArray 2 nil nil))))

(define MiElements
  (lambda (rt blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg)
    (mk-ugen (list "MiElements" rt (list blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg) nil 2 nil nil))))

(define MiMu
  (lambda (rt input gain bypass)
    (mk-ugen (list "MiMu" rt (list input gain bypass) nil 1 nil nil))))

(define MiOmi
  (lambda (rt audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate space)
    (mk-ugen (list "MiOmi" rt (list audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate space) nil 2 nil nil))))

(define MiPlaits
  (lambda (rt pitch engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour)
    (mk-ugen (list "MiPlaits" rt (list pitch engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour) nil 2 nil nil))))

(define MiRings
  (lambda (rt input trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass)
    (mk-ugen (list "MiRings" rt (list input trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass) nil 2 nil nil))))

(define MiRipples
  (lambda (input cf reson drive)
    (mk-ugen (list "MiRipples" (list 0) (list input cf reson drive) nil 1 nil nil))))

(define MiTides
  (lambda (rt freq shape slope smooth shift trig_ clock output_mode ramp_mode ratio rate_)
    (mk-ugen (list "MiTides" rt (list freq shape slope smooth shift trig_ clock output_mode ramp_mode ratio rate_) nil 4 nil nil))))

(define MiVerb
  (lambda (time drywet damp hp freeze diff inputArray)
    (mk-ugen (list "MiVerb" (list 6) (list time drywet damp hp freeze diff) inputArray 2 nil nil))))

(define MiWarps
  (lambda (rt carrier modulator lev1 lev2 algo timb osc pit easteregg)
    (mk-ugen (list "MiWarps" rt (list carrier modulator lev1 lev2 algo timb osc pit easteregg) nil 2 nil nil))))

(define MonoGrain
  (lambda (rt input winsize grainrate winrandpct)
    (mk-ugen (list "MonoGrain" rt (list input winsize grainrate winrandpct) nil 1 nil nil))))

(define MonoGrainBF
  (lambda (rt input winsize grainrate winrandpct azimuth azrand elevation elrand rho)
    (mk-ugen (list "MonoGrainBF" rt (list input winsize grainrate winrandpct azimuth azrand elevation elrand rho) nil 4 nil nil))))

(define MoogLadder
  (lambda (input ffreq res)
    (mk-ugen (list "MoogLadder" (list 0) (list input ffreq res) nil 1 nil nil))))

(define MoogVCF
  (lambda (input fco res)
    (mk-ugen (list "MoogVCF" (list 0) (list input fco res) nil 1 nil nil))))

(define NL
  (lambda (rt input bufnuma bufnumb guard1 guard2)
    (mk-ugen (list "NL" rt (list input bufnuma bufnumb guard1 guard2) nil 1 nil nil))))

(define NL2
  (lambda (rt input bufnum maxsizea maxsizeb guard1 guard2)
    (mk-ugen (list "NL2" rt (list input bufnum maxsizea maxsizeb guard1 guard2) nil 1 nil nil))))

(define NLFiltC
  (lambda (rt input a b d c l)
    (mk-ugen (list "NLFiltC" rt (list input a b d c l) nil 1 nil nil))))

(define NLFiltL
  (lambda (rt input a b d c l)
    (mk-ugen (list "NLFiltL" rt (list input a b d c l) nil 1 nil nil))))

(define NLFiltN
  (lambda (rt input a b d c l)
    (mk-ugen (list "NLFiltN" rt (list input a b d c l) nil 1 nil nil))))

(define NTube
  (lambda (rt input lossarray karray delaylengtharray)
    (mk-ugen (list "NTube" rt (list input lossarray karray delaylengtharray) nil 1 nil nil))))

(define NearestN
  (lambda (rt treebuf input gate_ num)
    (mk-ugen (list "NearestN" rt (list treebuf input gate_ num) nil 3 nil nil))))

(define NeedleRect
  (lambda (rt rate_ imgWidth imgHeight rectX rectY rectW rectH)
    (mk-ugen (list "NeedleRect" rt (list rate_ imgWidth imgHeight rectX rectY rectW rectH) nil 1 nil nil))))

(define NeoFormant
  (lambda (rt formantfreq carrierfreq phaseshift)
    (mk-ugen (list "NeoFormant" rt (list formantfreq carrierfreq phaseshift) nil 1 nil nil))))

(define NeoVarSawOsc
  (lambda (rt freq pw waveshape)
    (mk-ugen (list "NeoVarSawOsc" rt (list freq pw waveshape) nil 1 nil nil))))

(define NestedAllpassC
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2)
    (mk-ugen (list "NestedAllpassC" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) nil 1 nil nil))))

(define NestedAllpassL
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2)
    (mk-ugen (list "NestedAllpassL" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) nil 1 nil nil))))

(define NestedAllpassN
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2)
    (mk-ugen (list "NestedAllpassN" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) nil 1 nil nil))))

(define OSFold4
  (lambda (rt input lo hi)
    (mk-ugen (list "OSFold4" rt (list input lo hi) nil 1 nil nil))))

(define OSFold8
  (lambda (rt input lo hi)
    (mk-ugen (list "OSFold8" rt (list input lo hi) nil 1 nil nil))))

(define OSTrunc4
  (lambda (rt input quant)
    (mk-ugen (list "OSTrunc4" rt (list input quant) nil 1 nil nil))))

(define OSTrunc8
  (lambda (rt input quant)
    (mk-ugen (list "OSTrunc8" rt (list input quant) nil 1 nil nil))))

(define OSWrap4
  (lambda (rt input lo hi)
    (mk-ugen (list "OSWrap4" rt (list input lo hi) nil 1 nil nil))))

(define OSWrap8
  (lambda (rt input lo hi)
    (mk-ugen (list "OSWrap8" rt (list input lo hi) nil 1 nil nil))))

(define OnsetStatistics
  (lambda (rt input windowsize hopsize)
    (mk-ugen (list "OnsetStatistics" rt (list input windowsize hopsize) nil 3 nil nil))))

(define Oregonator
  (lambda (rt reset rate_ epsilon mu q initx inity initz)
    (mk-ugen (list "Oregonator" rt (list reset rate_ epsilon mu q initx inity initz) nil 3 nil nil))))

(define OscBank
  (lambda (rt freq gain saw8 square8 saw4 square4 saw2 square2 saw1)
    (mk-ugen (list "OscBank" rt (list freq gain saw8 square8 saw4 square4 saw2 square2 saw1) nil 1 nil nil))))

(define OteyPiano
  (lambda (rt freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type)
    (mk-ugen (list "OteyPiano" rt (list freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type) nil 1 nil nil))))

(define OteyPianoStrings
  (lambda (rt freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type)
    (mk-ugen (list "OteyPianoStrings" rt (list freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type) nil 1 nil nil))))

(define OteySoundBoard
  (lambda (inp c1 c3 mix)
    (mk-ugen (list "OteySoundBoard" (list 0) (list inp c1 c3 mix) nil 1 nil nil))))

(define PVInfo
  (lambda (rt pvbuffer binNum filePointer)
    (mk-ugen (list "PVInfo" rt (list pvbuffer binNum filePointer) nil 2 nil nil))))

(define PVSynth
  (lambda (rt pvbuffer numBins binStart binSkip filePointer freqMul freqAdd)
    (mk-ugen (list "PVSynth" rt (list pvbuffer numBins binStart binSkip filePointer freqMul freqAdd) nil 1 nil nil))))

(define PV_BinBufRd
  (lambda (buffer playbuf point binStart binSkip numBins clear)
    (mk-ugen (list "PV_BinBufRd" kr (list buffer playbuf point binStart binSkip numBins clear) nil 1 nil nil))))

(define PV_BinDelay
  (lambda (buffer maxdelay delaybuf fbbuf hop)
    (mk-ugen (list "PV_BinDelay" kr (list buffer maxdelay delaybuf fbbuf hop) nil 1 nil nil))))

(define PV_BinFilter
  (lambda (buffer start end)
    (mk-ugen (list "PV_BinFilter" kr (list buffer start end) nil 1 nil nil))))

(define PV_BinPlayBuf
  (lambda (buffer playbuf rate_ offset binStart binSkip numBins loop clear)
    (mk-ugen (list "PV_BinPlayBuf" kr (list buffer playbuf rate_ offset binStart binSkip numBins loop clear) nil 1 nil nil))))

(define PV_BufRd
  (lambda (buffer playbuf point)
    (mk-ugen (list "PV_BufRd" kr (list buffer playbuf point) nil 1 nil nil))))

(define PV_CommonMag
  (lambda (bufferA bufferB tolerance remove)
    (mk-ugen (list "PV_CommonMag" kr (list bufferA bufferB tolerance remove) nil 1 nil nil))))

(define PV_CommonMul
  (lambda (bufferA bufferB tolerance remove)
    (mk-ugen (list "PV_CommonMul" kr (list bufferA bufferB tolerance remove) nil 1 nil nil))))

(define PV_Compander
  (lambda (buffer thresh slopeBelow slopeAbove)
    (mk-ugen (list "PV_Compander" kr (list buffer thresh slopeBelow slopeAbove) nil 1 nil nil))))

(define PV_Cutoff
  (lambda (bufferA bufferB wipe)
    (mk-ugen (list "PV_Cutoff" kr (list bufferA bufferB wipe) nil 1 nil nil))))

(define PV_EvenBin
  (lambda (buffer)
    (mk-ugen (list "PV_EvenBin" kr (list buffer) nil 1 nil nil))))

(define PV_ExtractRepeat
  (lambda (buffer loopbuf loopdur memorytime which ffthop thresh)
    (mk-ugen (list "PV_ExtractRepeat" kr (list buffer loopbuf loopdur memorytime which ffthop thresh) nil 1 nil nil))))

(define PV_Freeze
  (lambda (buffer freeze)
    (mk-ugen (list "PV_Freeze" kr (list buffer freeze) nil 1 nil nil))))

(define PV_FreqBuffer
  (lambda (buffer databuffer)
    (mk-ugen (list "PV_FreqBuffer" kr (list buffer databuffer) nil 1 nil nil))))

(define PV_Invert
  (lambda (buffer)
    (mk-ugen (list "PV_Invert" kr (list buffer) nil 1 nil nil))))

(define PV_MagBuffer
  (lambda (buffer databuffer)
    (mk-ugen (list "PV_MagBuffer" kr (list buffer databuffer) nil 1 nil nil))))

(define PV_MagExp
  (lambda (buffer)
    (mk-ugen (list "PV_MagExp" kr (list buffer) nil 1 nil nil))))

(define PV_MagGate
  (lambda (buffer thresh remove)
    (mk-ugen (list "PV_MagGate" kr (list buffer thresh remove) nil 1 nil nil))))

(define PV_MagLog
  (lambda (buffer)
    (mk-ugen (list "PV_MagLog" kr (list buffer) nil 1 nil nil))))

(define PV_MagMap
  (lambda (buffer mapbuf)
    (mk-ugen (list "PV_MagMap" kr (list buffer mapbuf) nil 1 nil nil))))

(define PV_MagMinus
  (lambda (bufferA bufferB remove)
    (mk-ugen (list "PV_MagMinus" kr (list bufferA bufferB remove) nil 1 nil nil))))

(define PV_MagMulAdd
  (lambda (buffer)
    (mk-ugen (list "PV_MagMulAdd" kr (list buffer) nil 1 nil nil))))

(define PV_MagScale
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_MagScale" kr (list bufferA bufferB) nil 1 nil nil))))

(define PV_MagSmooth
  (lambda (buffer factor)
    (mk-ugen (list "PV_MagSmooth" kr (list buffer factor) nil 1 nil nil))))

(define PV_MagSubtract
  (lambda (bufferA bufferB zerolimit)
    (mk-ugen (list "PV_MagSubtract" kr (list bufferA bufferB zerolimit) nil 1 nil nil))))

(define PV_MaxMagN
  (lambda (buffer numbins)
    (mk-ugen (list "PV_MaxMagN" kr (list buffer numbins) nil 1 nil nil))))

(define PV_MinMagN
  (lambda (buffer numbins)
    (mk-ugen (list "PV_MinMagN" kr (list buffer numbins) nil 1 nil nil))))

(define PV_Morph
  (lambda (bufferA bufferB morph)
    (mk-ugen (list "PV_Morph" kr (list bufferA bufferB morph) nil 1 nil nil))))

(define PV_NoiseSynthF
  (lambda (buffer threshold numFrames initflag)
    (mk-ugen (list "PV_NoiseSynthF" kr (list buffer threshold numFrames initflag) nil 1 nil nil))))

(define PV_NoiseSynthP
  (lambda (buffer threshold numFrames initflag)
    (mk-ugen (list "PV_NoiseSynthP" kr (list buffer threshold numFrames initflag) nil 1 nil nil))))

(define PV_OddBin
  (lambda (buffer)
    (mk-ugen (list "PV_OddBin" kr (list buffer) nil 1 nil nil))))

(define PV_PartialSynthF
  (lambda (buffer threshold numFrames initflag)
    (mk-ugen (list "PV_PartialSynthF" kr (list buffer threshold numFrames initflag) nil 1 nil nil))))

(define PV_PartialSynthP
  (lambda (buffer threshold numFrames initflag)
    (mk-ugen (list "PV_PartialSynthP" kr (list buffer threshold numFrames initflag) nil 1 nil nil))))

(define PV_PitchShift
  (lambda (buffer ratio)
    (mk-ugen (list "PV_PitchShift" kr (list buffer ratio) nil 1 nil nil))))

(define PV_PlayBuf
  (lambda (buffer playbuf rate_ offset loop)
    (mk-ugen (list "PV_PlayBuf" kr (list buffer playbuf rate_ offset loop) nil 1 nil nil))))

(define PV_RecordBuf
  (lambda (buffer recbuf offset run loop hop wintype)
    (mk-ugen (list "PV_RecordBuf" kr (list buffer recbuf offset run loop hop wintype) nil 1 nil nil))))

(define PV_SoftWipe
  (lambda (bufferA bufferB wipe)
    (mk-ugen (list "PV_SoftWipe" kr (list bufferA bufferB wipe) nil 1 nil nil))))

(define PV_SpectralEnhance
  (lambda (buffer numPartials ratio strength)
    (mk-ugen (list "PV_SpectralEnhance" kr (list buffer numPartials ratio strength) nil 1 nil nil))))

(define PV_SpectralMap
  (lambda (buffer specBuffer floor_ freeze mode norm window)
    (mk-ugen (list "PV_SpectralMap" kr (list buffer specBuffer floor_ freeze mode norm window) nil 1 nil nil))))

(define PV_Whiten
  (lambda (chain trackbufnum relaxtime floor_ smear bindownsample)
    (mk-ugen (list "PV_Whiten" kr (list chain trackbufnum relaxtime floor_ smear bindownsample) nil 1 nil nil))))

(define PV_XFade
  (lambda (bufferA bufferB fade)
    (mk-ugen (list "PV_XFade" kr (list bufferA bufferB fade) nil 1 nil nil))))

(define PanX
  (lambda (rt numChans input pos level width)
    (mk-ugen (list "PanX" rt (list numChans input pos level width) nil 0 nil nil))))

(define PanX2D
  (lambda (rt numChansX numChansY input posX posY level widthX widthY)
    (mk-ugen (list "PanX2D" rt (list numChansX numChansY input posX posY level widthX widthY) nil 0 nil nil))))

(define PeakEQ2
  (lambda (rt input freq rs db)
    (mk-ugen (list "PeakEQ2" rt (list input freq rs db) nil 1 nil nil))))

(define PeakEQ4
  (lambda (rt input freq rs db)
    (mk-ugen (list "PeakEQ4" rt (list input freq rs db) nil 1 nil nil))))

(define Perlin3
  (lambda (rt x y z)
    (mk-ugen (list "Perlin3" rt (list x y z) nil 1 nil nil))))

(define PermMod
  (lambda (rt input freq)
    (mk-ugen (list "PermMod" rt (list input freq) nil 1 nil nil))))

(define PermModArray
  (lambda (rt input freq pattern)
    (mk-ugen (list "PermModArray" rt (list input freq pattern) nil 1 nil nil))))

(define PermModT
  (lambda (rt input outfreq infreq)
    (mk-ugen (list "PermModT" rt (list input outfreq infreq) nil 1 nil nil))))

(define PhasorModal
  (lambda (input freq decay_ damp amp phase)
    (mk-ugen (list "PhasorModal" (list 0) (list input freq decay_ damp amp phase) nil 1 nil nil))))

(define PlaneTree
  (lambda (rt treebuf input gate_)
    (mk-ugen (list "PlaneTree" rt (list treebuf input gate_) nil 1 nil nil))))

(define PluckSynth
  (lambda (rt freq amp gate_ pos c1 c3 release f m k r l ra rho)
    (mk-ugen (list "PluckSynth" rt (list freq amp gate_ pos c1 c3 release f m k r l ra rho) nil 1 nil nil))))

(define PosRatio
  (lambda (rt input period thresh)
    (mk-ugen (list "PosRatio" rt (list input period thresh) nil 1 nil nil))))

(define PrintVal
  (lambda (rt input numblocks id_)
    (mk-ugen (list "PrintVal" rt (list input numblocks id_) nil 1 nil nil))))

(define Qitch
  (lambda (rt input databufnum ampThreshold algoflag ampbufnum minfreq maxfreq)
    (mk-ugen (list "Qitch" rt (list input databufnum ampThreshold algoflag ampbufnum minfreq maxfreq) nil 2 nil nil))))

(define RLPFD
  (lambda (input ffreq res dist)
    (mk-ugen (list "RLPFD" (list 0) (list input ffreq res dist) nil 1 nil nil))))

(define RMAFoodChainL
  (lambda (rt freq a1 b1 d1 a2 b2 d2 k r h xi yi zi)
    (mk-ugen (list "RMAFoodChainL" rt (list freq a1 b1 d1 a2 b2 d2 k r h xi yi zi) nil 3 nil nil))))

(define RMEQ
  (lambda (input freq rq k)
    (mk-ugen (list "RMEQ" (list 0) (list input freq rq k) nil 1 nil nil))))

(define RMEQSuite
  (lambda (rt maxSize)
    (mk-ugen (list "RMEQSuite" rt (list maxSize) nil 1 nil nil))))

(define RMS
  (lambda (rt input lpFreq)
    (mk-ugen (list "RMS" rt (list input lpFreq) nil 1 nil nil))))

(define RMShelf
  (lambda (rt input freq k)
    (mk-ugen (list "RMShelf" rt (list input freq k) nil 1 nil nil))))

(define RMShelf2
  (lambda (rt input freq k)
    (mk-ugen (list "RMShelf2" rt (list input freq k) nil 1 nil nil))))

(define RegaliaMitraEQ
  (lambda (rt input freq rq k)
    (mk-ugen (list "RegaliaMitraEQ" rt (list input freq rq k) nil 1 nil nil))))

(define Resonator
  (lambda (input freq position resolution structure brightness damping)
    (mk-ugen (list "Resonator" (list 0) (list input freq position resolution structure brightness damping) nil 1 nil nil))))

(define Rongs
  (lambda (rt trigger sustain f0 structure brightness damping accent stretch position loss)
    (mk-ugen (list "Rongs" rt (list trigger sustain f0 structure brightness damping accent stretch position loss) nil 1 nil nil))))

(define RosslerL
  (lambda (rt freq a b c h xi yi zi)
    (mk-ugen (list "RosslerL" rt (list freq a b c h xi yi zi) nil 3 nil nil))))

(define RosslerResL
  (lambda (rt input stiff freq a b c h xi yi zi)
    (mk-ugen (list "RosslerResL" rt (list input stiff freq a b c h xi yi zi) nil 1 nil nil))))

(define Rotate
  (lambda (rt w x y z rotate)
    (mk-ugen (list "Rotate" rt (list w x y z rotate) nil 1 nil nil))))

(define SLOnset
  (lambda (rt input memorysize1 before after threshold hysteresis)
    (mk-ugen (list "SLOnset" rt (list input memorysize1 before after threshold hysteresis) nil 1 nil nil))))

(define SMS
  (lambda (input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum)
    (mk-ugen (list "SMS" (list 0) (list input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum) nil 2 nil nil))))

(define SOMAreaWr
  (lambda (rt bufnum inputdata coords netsize numdims nhood gate_)
    (mk-ugen (list "SOMAreaWr" rt (list bufnum inputdata coords netsize numdims nhood gate_) nil 1 nil nil))))

(define SOMRd
  (lambda (rt bufnum inputdata netsize numdims gate_)
    (mk-ugen (list "SOMRd" rt (list bufnum inputdata netsize numdims gate_) nil 2 nil nil))))

(define SOMTrain
  (lambda (rt bufnum inputdata netsize numdims traindur nhood gate_ initweight)
    (mk-ugen (list "SOMTrain" rt (list bufnum inputdata netsize numdims traindur nhood gate_ initweight) nil 3 nil nil))))

(define SVF
  (lambda (signal cutoff res lowpass bandpass highpass notch peak)
    (mk-ugen (list "SVF" (list 0) (list signal cutoff res lowpass bandpass highpass notch peak) nil 1 nil nil))))

(define SawDPW
  (lambda (rt freq iphase)
    (mk-ugen (list "SawDPW" rt (list freq iphase) nil 1 nil nil))))

(define SensoryDissonance
  (lambda (rt fft maxpeaks peakthreshold norm clamp)
    (mk-ugen (list "SensoryDissonance" rt (list fft maxpeaks peakthreshold norm clamp) nil 1 nil nil))))

(define Sieve1
  (lambda (rt bufnum gap alternate)
    (mk-ugen (list "Sieve1" rt (list bufnum gap alternate) nil 1 nil nil))))

(define SinGrain
  (lambda (rt trigger dur freq)
    (mk-ugen (list "SinGrain" rt (list trigger dur freq) nil 1 nil nil))))

(define SinGrainB
  (lambda (rt trigger dur freq envbuf)
    (mk-ugen (list "SinGrainB" rt (list trigger dur freq envbuf) nil 1 nil nil))))

(define SinGrainBBF
  (lambda (rt trigger dur freq envbuf azimuth elevation rho wComp)
    (mk-ugen (list "SinGrainBBF" rt (list trigger dur freq envbuf azimuth elevation rho wComp) nil 4 nil nil))))

(define SinGrainBF
  (lambda (rt trigger dur freq azimuth elevation rho wComp)
    (mk-ugen (list "SinGrainBF" rt (list trigger dur freq azimuth elevation rho wComp) nil 4 nil nil))))

(define SinGrainI
  (lambda (rt trigger dur freq envbuf1 envbuf2 ifac)
    (mk-ugen (list "SinGrainI" rt (list trigger dur freq envbuf1 envbuf2 ifac) nil 1 nil nil))))

(define SinGrainIBF
  (lambda (rt trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp)
    (mk-ugen (list "SinGrainIBF" rt (list trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp) nil 4 nil nil))))

(define SinTone
  (lambda (rt freq phase)
    (mk-ugen (list "SinTone" rt (list freq phase) nil 1 nil nil))))

(define SineShaper
  (lambda (input limit)
    (mk-ugen (list "SineShaper" (list 0) (list input limit) nil 1 nil nil))))

(define SkipNeedle
  (lambda (rt range rate_ offset)
    (mk-ugen (list "SkipNeedle" rt (list range rate_ offset) nil 1 nil nil))))

(define SmoothDecimator
  (lambda (rt input rate_ smoothing)
    (mk-ugen (list "SmoothDecimator" rt (list input rate_ smoothing) nil 1 nil nil))))

(define SoftClipAmp
  (lambda (input pregain)
    (mk-ugen (list "SoftClipAmp" (list 0) (list input pregain) nil 1 nil nil))))

(define SoftClipAmp4
  (lambda (input pregain)
    (mk-ugen (list "SoftClipAmp4" (list 0) (list input pregain) nil 1 nil nil))))

(define SoftClipAmp8
  (lambda (input pregain)
    (mk-ugen (list "SoftClipAmp8" (list 0) (list input pregain) nil 1 nil nil))))

(define SoftClipper4
  (lambda (rt input)
    (mk-ugen (list "SoftClipper4" rt (list input) nil 1 nil nil))))

(define SoftClipper8
  (lambda (rt input)
    (mk-ugen (list "SoftClipper8" rt (list input) nil 1 nil nil))))

(define SonLPC
  (lambda (rt buff input hop poles)
    (mk-ugen (list "SonLPC" rt (list buff input hop poles) nil 1 nil nil))))

(define SonLPCSynth
  (lambda (rt chain)
    (mk-ugen (list "SonLPCSynth" rt (list chain) nil 1 nil nil))))

(define SonLPCSynthIn
  (lambda (rt chain input)
    (mk-ugen (list "SonLPCSynthIn" rt (list chain input) nil 1 nil nil))))

(define SortBuf
  (lambda (rt bufnum sortrate reset)
    (mk-ugen (list "SortBuf" rt (list bufnum sortrate reset) nil 1 nil nil))))

(define SpectralEntropy
  (lambda (nc rt fft fftsize numbands)
    (mk-ugen (list "SpectralEntropy" rt (list fft fftsize numbands) nil nc nil nil))))

(define Spreader
  (lambda (rt input theta filtsPerOctave)
    (mk-ugen (list "Spreader" rt (list input theta filtsPerOctave) nil 2 nil nil))))

(define SpruceBudworm
  (lambda (rt reset rate_ k1 k2 alpha beta mu rho initx inity)
    (mk-ugen (list "SpruceBudworm" rt (list reset rate_ k1 k2 alpha beta mu rho initx inity) nil 2 nil nil))))

(define Squiz
  (lambda (input pitchratio zcperchunk memlen)
    (mk-ugen (list "Squiz" (list 0) (list input pitchratio zcperchunk memlen) nil 1 nil nil))))

(define Standard2DC
  (lambda (rt minfreq maxfreq k x0 y0)
    (mk-ugen (list "Standard2DC" rt (list minfreq maxfreq k x0 y0) nil 1 nil nil))))

(define Standard2DL
  (lambda (rt minfreq maxfreq k x0 y0)
    (mk-ugen (list "Standard2DL" rt (list minfreq maxfreq k x0 y0) nil 1 nil nil))))

(define Standard2DN
  (lambda (rt minfreq maxfreq k x0 y0)
    (mk-ugen (list "Standard2DN" rt (list minfreq maxfreq k x0 y0) nil 1 nil nil))))

(define StandardTrig
  (lambda (rt minfreq maxfreq k x0 y0)
    (mk-ugen (list "StandardTrig" rt (list minfreq maxfreq k x0 y0) nil 1 nil nil))))

(define StkBandedWG
  (lambda (rt freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_)
    (mk-ugen (list "StkBandedWG" rt (list freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_) nil 1 nil nil))))

(define StkBeeThree
  (lambda (rt freq op4gain op3gain lfospeed lfodepth adsrtarget trig_)
    (mk-ugen (list "StkBeeThree" rt (list freq op4gain op3gain lfospeed lfodepth adsrtarget trig_) nil 1 nil nil))))

(define StkBlowHole
  (lambda (rt freq reedstiffness noisegain tonehole register breathpressure)
    (mk-ugen (list "StkBlowHole" rt (list freq reedstiffness noisegain tonehole register breathpressure) nil 1 nil nil))))

(define StkBowed
  (lambda (rt freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate)
    (mk-ugen (list "StkBowed" rt (list freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate) nil 1 nil nil))))

(define StkClarinet
  (lambda (rt freq reedstiffness noisegain vibfreq vibgain breathpressure trig_)
    (mk-ugen (list "StkClarinet" rt (list freq reedstiffness noisegain vibfreq vibgain breathpressure trig_) nil 1 nil nil))))

(define StkFlute
  (lambda (rt freq jetDelay noisegain jetRatio)
    (mk-ugen (list "StkFlute" rt (list freq jetDelay noisegain jetRatio) nil 1 nil nil))))

(define StkGlobals
  (lambda (rt showWarnings printErrors rawfilepath)
    (mk-ugen (list "StkGlobals" rt (list showWarnings printErrors rawfilepath) nil 1 nil nil))))

(define StkInst
  (lambda (rt freq gate_ onamp offamp instNumber args)
    (mk-ugen (list "StkInst" rt (list freq gate_ onamp offamp instNumber) args 1 nil nil))))

(define StkMandolin
  (lambda (rt freq bodysize pickposition stringdamping stringdetune aftertouch trig_)
    (mk-ugen (list "StkMandolin" rt (list freq bodysize pickposition stringdamping stringdetune aftertouch trig_) nil 1 nil nil))))

(define StkModalBar
  (lambda (rt freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_)
    (mk-ugen (list "StkModalBar" rt (list freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_) nil 1 nil nil))))

(define StkMoog
  (lambda (rt freq filterQ sweeprate vibfreq vibgain gain trig_)
    (mk-ugen (list "StkMoog" rt (list freq filterQ sweeprate vibfreq vibgain gain trig_) nil 1 nil nil))))

(define StkPluck
  (lambda (rt freq decay_)
    (mk-ugen (list "StkPluck" rt (list freq decay_) nil 1 nil nil))))

(define StkSaxofony
  (lambda (rt freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_)
    (mk-ugen (list "StkSaxofony" rt (list freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_) nil 1 nil nil))))

(define StkShakers
  (lambda (rt instr energy decay_ objects resfreq)
    (mk-ugen (list "StkShakers" rt (list instr energy decay_ objects resfreq) nil 1 nil nil))))

(define StkVoicForm
  (lambda (rt freq vuvmix vowelphon vibfreq vibgain loudness_ trig_)
    (mk-ugen (list "StkVoicForm" rt (list freq vuvmix vowelphon vibfreq vibgain loudness_ trig_) nil 1 nil nil))))

(define Streson
  (lambda (input delayTime res)
    (mk-ugen (list "Streson" (list 0) (list input delayTime res) nil 1 nil nil))))

(define StringVoice
  (lambda (rt trig_ infsustain freq accent structure brightness damping)
    (mk-ugen (list "StringVoice" rt (list trig_ infsustain freq accent structure brightness damping) nil 1 nil nil))))

(define Summer
  (lambda (trig_ step reset resetval)
    (mk-ugen (list "Summer" (list 0) (list trig_ step reset resetval) nil 1 nil nil))))

(define SwitchDelay
  (lambda (input drylevel wetlevel delaytime delayfactor maxdelaytime)
    (mk-ugen (list "SwitchDelay" (list 0) (list input drylevel wetlevel delaytime delayfactor maxdelaytime) nil 1 nil nil))))

(define TBetaRand
  (lambda (lo hi prob1 prob2 trig_)
    (mk-ugen (list "TBetaRand" (list 4) (list lo hi prob1 prob2 trig_) nil 1 nil (unique-uid)))))

(define TBrownRand
  (lambda (lo hi dev dist trig_)
    (mk-ugen (list "TBrownRand" (list 4) (list lo hi dev dist trig_) nil 1 nil (unique-uid)))))

(define TGaussRand
  (lambda (lo hi trig_)
    (mk-ugen (list "TGaussRand" (list 2) (list lo hi trig_) nil 1 nil (unique-uid)))))

(define TGrains2
  (lambda (nc rt trigger bufnum rate_ centerPos dur pan amp att dec interp)
    (mk-ugen (list "TGrains2" rt (list trigger bufnum rate_ centerPos dur pan amp att dec interp) nil nc nil nil))))

(define TGrains3
  (lambda (nc rt trigger bufnum rate_ centerPos dur pan amp att dec window interp)
    (mk-ugen (list "TGrains3" rt (list trigger bufnum rate_ centerPos dur pan amp att dec window interp) nil nc nil nil))))

(define TPV
  (lambda (chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor)
    (mk-ugen (list "TPV" ar (list chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor) nil 1 nil nil))))

(define TScramble
  (lambda (rt trigger inputs)
    (mk-ugen (list "TScramble" rt (list trigger inputs) nil 1 nil nil))))

(define TTendency
  (lambda (rt trigger dist parX parY parA parB)
    (mk-ugen (list "TTendency" rt (list trigger dist parX parY parA parB) nil 1 nil nil))))

(define Tartini
  (lambda (rt input threshold n k overlap smallCutoff)
    (mk-ugen (list "Tartini" rt (list input threshold n k overlap smallCutoff) nil 2 nil nil))))

(define TermanWang
  (lambda (rt input reset ratex ratey alpha beta eta initx inity)
    (mk-ugen (list "TermanWang" rt (list input reset ratex ratey alpha beta eta initx inity) nil 1 nil nil))))

(define TextVU
  (lambda (rt trig_ input label_ width reset ana)
    (mk-ugen (list "TextVU" rt (list trig_ input label_ width reset ana) nil 1 nil nil))))

(define Tilt
  (lambda (rt w x y z tilt)
    (mk-ugen (list "Tilt" rt (list w x y z tilt) nil 1 nil nil))))

(define TrigAvg
  (lambda (rt input trig_)
    (mk-ugen (list "TrigAvg" rt (list input trig_) nil 1 nil nil))))

(define Tumble
  (lambda (rt w x y z tilt)
    (mk-ugen (list "Tumble" rt (list w x y z tilt) nil 1 nil nil))))

(define TwoTube
  (lambda (rt input k loss d1length d2length)
    (mk-ugen (list "TwoTube" rt (list input k loss d1length d2length) nil 1 nil nil))))

(define UHJ2B
  (lambda (rt ls rs)
    (mk-ugen (list "UHJ2B" rt (list ls rs) nil 3 nil nil))))

(define VBAP
  (lambda (nc rt input bufnum azimuth elevation spread)
    (mk-ugen (list "VBAP" rt (list input bufnum azimuth elevation spread) nil nc nil nil))))

(define VBFourses
  (lambda (rt smoother freqarray)
    (mk-ugen (list "VBFourses" rt (list smoother) freqarray 4 nil nil))))

(define VBJonVerb
  (lambda (input decay_ damping inputbw erfl tail_)
    (mk-ugen (list "VBJonVerb" (list 0) (list input decay_ damping inputbw erfl tail_) nil 2 nil nil))))

(define VBPVoc
  (lambda (rt numChannels bufnum playpos fftsize)
    (mk-ugen (list "VBPVoc" rt (list numChannels bufnum playpos fftsize) nil 1 nil nil))))

(define VBSlide
  (lambda (input slideup slidedown)
    (mk-ugen (list "VBSlide" (list 0) (list input slideup slidedown) nil 1 nil nil))))

(define VMScan2D
  (lambda (rt bufnum)
    (mk-ugen (list "VMScan2D" rt (list bufnum) nil 2 nil nil))))

(define VOSIM
  (lambda (rt trig_ freq nCycles decay_)
    (mk-ugen (list "VOSIM" rt (list trig_ freq nCycles decay_) nil 1 nil nil))))

(define VarShapeOsc
  (lambda (rt freq pw waveshape sync syncfreq)
    (mk-ugen (list "VarShapeOsc" rt (list freq pw waveshape sync syncfreq) nil 1 nil nil))))

(define VosimOsc
  (lambda (rt freq form1freq form2freq shape)
    (mk-ugen (list "VosimOsc" rt (list freq form1freq form2freq shape) nil 1 nil nil))))

(define WAmp
  (lambda (rt input winSize)
    (mk-ugen (list "WAmp" rt (list input winSize) nil 1 nil nil))))

(define WalshHadamard
  (lambda (rt input which)
    (mk-ugen (list "WalshHadamard" rt (list input which) nil 1 nil nil))))

(define WarpZ
  (lambda (nc rt bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart)
    (mk-ugen (list "WarpZ" rt (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart) nil nc nil nil))))

(define WaveLoss
  (lambda (rt input drop_ outof mode)
    (mk-ugen (list "WaveLoss" rt (list input drop_ outof mode) nil 1 nil nil))))

(define WaveTerrain
  (lambda (rt bufnum x y xsize ysize)
    (mk-ugen (list "WaveTerrain" rt (list bufnum x y xsize ysize) nil 1 nil nil))))

(define WaveletDaub
  (lambda (rt input n which)
    (mk-ugen (list "WaveletDaub" rt (list input n which) nil 1 nil nil))))

(define WeaklyNonlinear
  (lambda (rt input reset ratex ratey freq initx inity alpha xexponent beta yexponent)
    (mk-ugen (list "WeaklyNonlinear" rt (list input reset ratex ratey freq initx inity alpha xexponent beta yexponent) nil 1 nil nil))))

(define WeaklyNonlinear2
  (lambda (rt input reset ratex ratey freq initx inity alpha xexponent beta yexponent)
    (mk-ugen (list "WeaklyNonlinear2" rt (list input reset ratex ratey freq initx inity alpha xexponent beta yexponent) nil 1 nil nil))))

(define WrapSummer
  (lambda (rt trig_ step min_ max_ reset resetval)
    (mk-ugen (list "WrapSummer" rt (list trig_ step min_ max_ reset resetval) nil 1 nil nil))))

(define ZOsc
  (lambda (rt freq formantfreq shape mode)
    (mk-ugen (list "ZOsc" rt (list freq formantfreq shape mode) nil 1 nil nil))))

(define Neg
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 0 nil))))

(define Not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 1 nil))))

(define IsNil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 2 nil))))

(define NotNil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 3 nil))))

(define BitNot
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 4 nil))))

(define Abs
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 5 nil))))

(define AsFloat
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 6 nil))))

(define AsInt
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 7 nil))))

(define Ceil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 8 nil))))

(define Floor
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 9 nil))))

(define Frac
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 10 nil))))

(define Sign
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 11 nil))))

(define Squared
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 12 nil))))

(define Cubed
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 13 nil))))

(define Sqrt
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 14 nil))))

(define Exp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 15 nil))))

(define Recip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 16 nil))))

(define MIDICPS
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 17 nil))))

(define CPSMIDI
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 18 nil))))

(define MIDIRatio
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 19 nil))))

(define RatioMIDI
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 20 nil))))

(define DbAmp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 21 nil))))

(define AmpDb
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 22 nil))))

(define OctCPS
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 23 nil))))

(define CPSOct
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 24 nil))))

(define Log
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 25 nil))))

(define Log2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 26 nil))))

(define Log10
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 27 nil))))

(define Sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 28 nil))))

(define Cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 29 nil))))

(define Tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 30 nil))))

(define ArcSin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 31 nil))))

(define ArcCos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 32 nil))))

(define ArcTan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 33 nil))))

(define SinH
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 34 nil))))

(define CosH
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 35 nil))))

(define TanH
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 36 nil))))

(define Rand_
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 37 nil))))

(define Rand2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 38 nil))))

(define LinRand_
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 39 nil))))

(define BiLinRand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 40 nil))))

(define Sum3Rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 41 nil))))

(define Distort
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 42 nil))))

(define SoftClip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 43 nil))))

(define Coin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 44 nil))))

(define DigitValue
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 45 nil))))

(define Silence
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 46 nil))))

(define Thru
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 47 nil))))

(define RectWindow
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 48 nil))))

(define HanWindow
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 49 nil))))

(define WelchWindow
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 50 nil))))

(define TriWindow
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 51 nil))))

(define Ramp_
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 52 nil))))

(define SCurve
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" (list 0) (list a) nil 1 53 nil))))

(define Add
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 0 nil))))

(define Sub
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 1 nil))))

(define Mul
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 2 nil))))

(define IDiv
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 3 nil))))

(define FDiv
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 4 nil))))

(define Mod
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 5 nil))))

(define EQ
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 6 nil))))

(define NE
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 7 nil))))

(define LT
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 8 nil))))

(define GT
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 9 nil))))

(define LE
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 10 nil))))

(define GE
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 11 nil))))

(define Min
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 12 nil))))

(define Max
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 13 nil))))

(define BitAnd
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 14 nil))))

(define BitOr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 15 nil))))

(define BitXor
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 16 nil))))

(define LCM
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 17 nil))))

(define GCD
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 18 nil))))

(define Round
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 19 nil))))

(define RoundUp
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 20 nil))))

(define Trunc
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 21 nil))))

(define Atan2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 22 nil))))

(define Hypot
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 23 nil))))

(define Hypotx
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 24 nil))))

(define Pow
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 25 nil))))

(define ShiftLeft
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 26 nil))))

(define ShiftRight
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 27 nil))))

(define UnsignedShift
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 28 nil))))

(define Fill
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 29 nil))))

(define Ring1
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 30 nil))))

(define Ring2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 31 nil))))

(define Ring3
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 32 nil))))

(define Ring4
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 33 nil))))

(define DifSqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 34 nil))))

(define SumSqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 35 nil))))

(define SqrSum
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 36 nil))))

(define SqrDif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 37 nil))))

(define AbsDif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 38 nil))))

(define Thresh
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 39 nil))))

(define AMClip
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 40 nil))))

(define ScaleNeg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 41 nil))))

(define Clip2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 42 nil))))

(define Excess
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 43 nil))))

(define Fold2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 44 nil))))

(define Wrap2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 45 nil))))

(define FirstArg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 46 nil))))

(define RandRange
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 47 nil))))

(define ExpRandRange
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" (list 0 1) (list a b) nil 1 48 nil))))
(define RDustR
  (lambda (rt lo hi)
    (mk-ugen (list "RDustR" rt (list lo hi) nil 1 nil (unique-uid)))))

(define RExpRandN
  (lambda (nc lo hi)
    (mk-ugen (list "RExpRandN" ir (list lo hi) nil nc nil (unique-uid)))))

(define RIRandN
  (lambda (nc lo hi)
    (mk-ugen (list "RIRandN" ir (list lo hi) nil nc nil (unique-uid)))))

(define RLinRandN
  (lambda (nc lo hi minmax)
    (mk-ugen (list "RLinRandN" ir (list lo hi minmax) nil nc nil (unique-uid)))))

(define RRandN
  (lambda (nc lo hi)
    (mk-ugen (list "RRandN" ir (list lo hi) nil nc nil (unique-uid)))))

(define RBezier
  (lambda (rt haltAfter dx freq phase param)
    (mk-ugen (list "RBezier" rt (list haltAfter dx freq phase) param 1 nil nil))))

(define RDelayMap
  (lambda (bufnum input dynamic mapArray)
    (mk-ugen (list "RDelayMap" (list 1) (list bufnum input dynamic) mapArray 1 nil nil))))

(define RDelaySet
  (lambda (input setArray)
    (mk-ugen (list "RDelaySet" (list 0) (list input) setArray 1 nil nil))))

(define RDelaySetB
  (lambda (buffer input setArray)
    (mk-ugen (list "RDelaySetB" (list 1) (list buffer input) setArray 1 nil nil))))

(define RDL
  (lambda (nc inputs)
    (mk-ugen (list "RDL" ar nil inputs nc nil nil))))

(define RDX7
  (lambda (rt bufnum on off data vc mnn vel pw mw bc fc)
    (mk-ugen (list "RDX7" rt (list bufnum on off data vc mnn vel pw mw bc fc) nil 1 nil nil))))

(define RDX7Env
  (lambda (rt gate_ data r1 r2 r3 r4 l1 l2 l3 l4 ol)
    (mk-ugen (list "RDX7Env" rt (list gate_ data r1 r2 r3 r4 l1 l2 l3 l4 ol) nil 1 nil nil))))

(define RFreezer
  (lambda (bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops)
    (mk-ugen (list "RFreezer" ar (list bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops) nil 1 nil nil))))

(define RLagC
  (lambda (input timeUp curveUp timeDown curveDown)
    (mk-ugen (list "RLagC" (list 0) (list input timeUp curveUp timeDown curveDown) nil 1 nil nil))))

(define RPlayTrace
  (lambda (rt bufnum degree rate_ access)
    (mk-ugen (list "RPlayTrace" rt (list bufnum degree rate_ access) nil 1 nil nil))))

(define RPVDecayTbl
  (lambda (rt fft_buf decay_rate_buf history_buf)
    (mk-ugen (list "RPVDecayTbl" rt (list fft_buf decay_rate_buf history_buf) nil 1 nil nil))))

(define RObxdFilter
  (lambda (input cutoff resonance multimode bandpass fourpole)
    (mk-ugen (list "RObxdFilter" (list 0) (list input cutoff resonance multimode bandpass fourpole) nil 1 nil nil))))

(define RTExpRandN
  (lambda (nc lo hi trigger)
    (mk-ugen (list "RTExpRandN" (list 2) (list lo hi trigger) nil nc nil (unique-uid)))))

(define RTRandN
  (lambda (nc lo hi trigger)
    (mk-ugen (list "RTRandN" (list 2) (list lo hi trigger) nil nc nil (unique-uid)))))

(define RTScramble
  (lambda (trigger inputs)
    (mk-ugen (list "RTScramble" (list 0) (list trigger) inputs (length (mce-channels inputs)) nil (unique-uid)))))

(define RShufflerB
  (lambda (bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta)
    (mk-ugen (list "RShufflerB" ar (list bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta) nil 2 nil nil))))

(define RShufflerL
  (lambda (input fragmentSize maxDelay)
    (mk-ugen (list "RShufflerL" (list 0) (list input fragmentSize maxDelay) nil 1 nil nil))))

(define RSmplrIndex
  (lambda (rt buf size mnn)
    (mk-ugen (list "RSmplrIndex" rt (list buf size mnn) nil 2 nil nil))))

(define RTraceRd
  (lambda (rt bufnum degree index access)
    (mk-ugen (list "RTraceRd" rt (list bufnum degree index access) nil 1 nil nil))))

(define PV_Split
  (lambda (bufferA bufferB)
    (mk-ugen (list "PV_Split" kr (list bufferA bufferB) nil 2 nil nil))))

; Zero local buffer.
;
; ClearBuf does not copy the buffer number through so this is an MRG node.
;
; ClearBuf :: ugen -> ugen
(define ClearBuf
  (lambda (b)
    (mrg2 b (mk-ugen (list "ClearBuf" ir (list b) nil 1 nil nil)))))

(define PackFFT
  (lambda (b sz fr to z mp)
    (mk-ugen (list "PackFFT" kr (list b sz fr to z) mp 1 nil nil))))

(define string->ugen
  (lambda (s)
    (make-mce
     (cons (string-length s)
           (map char->integer (string->list s))))))

(define Poll
  (lambda (trig input trigid label_)
    (mk-ugen (list "Poll" (list 1) (list trig input trigid) (string->ugen label_) 1 nil nil))))

(define PV_HainsworthFoote
  (lambda (buf h f thr wt)
    (mk-ugen (list "PV_HainsworthFoote" ar (list buf h f thr wt) nil 1 nil nil))))

(define PV_JensenAndersen
  (lambda (buffer propsc prophfe prophfc propsf threshold waittime)
    (mk-ugen (list "PV_JensenAndersen" ar (list buffer propsc prophfe prophfc propsf threshold waittime) nil 1 nil nil))))

(define Unpack1FFT
  (lambda (c b bi wm)
    (mk-ugen (list "Unpack1FFT" dr (list c b bi wm) nil 1 nil nil))))
