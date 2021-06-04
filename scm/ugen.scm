(define A2K
  (lambda (input)
    (construct-ugen "A2K" kr (list input) nil 1 nil)))

(define APF
  (lambda (input freq radius)
    (construct-ugen "APF" (list 0) (list input freq radius) nil 1 nil)))

(define AllpassC
  (lambda (input maxdelaytime delaytime decaytime)
    (construct-ugen "AllpassC" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil)))

(define AllpassL
  (lambda (input maxdelaytime delaytime decaytime)
    (construct-ugen "AllpassL" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil)))

(define AllpassN
  (lambda (input maxdelaytime delaytime decaytime)
    (construct-ugen "AllpassN" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil)))

(define AmpComp
  (lambda (rt freq root exp)
    (construct-ugen "AmpComp" rt (list freq root exp) nil 1 nil)))

(define AmpCompA
  (lambda (rt freq root minAmp rootAmp)
    (construct-ugen "AmpCompA" rt (list freq root minAmp rootAmp) nil 1 nil)))

(define Amplitude
  (lambda (rt input attackTime releaseTime)
    (construct-ugen "Amplitude" rt (list input attackTime releaseTime) nil 1 nil)))

(define BAllPass
  (lambda (input freq rq)
    (construct-ugen "BAllPass" (list 0) (list input freq rq) nil 1 nil)))

(define BBandPass
  (lambda (input freq bw)
    (construct-ugen "BBandPass" (list 0) (list input freq bw) nil 1 nil)))

(define BBandStop
  (lambda (input freq bw)
    (construct-ugen "BBandStop" (list 0) (list input freq bw) nil 1 nil)))

(define BHiPass
  (lambda (input freq rq)
    (construct-ugen "BHiPass" (list 0) (list input freq rq) nil 1 nil)))

(define BHiShelf
  (lambda (input freq rs db)
    (construct-ugen "BHiShelf" (list 0) (list input freq rs db) nil 1 nil)))

(define BLowPass
  (lambda (input freq rq)
    (construct-ugen "BLowPass" (list 0) (list input freq rq) nil 1 nil)))

(define BLowShelf
  (lambda (input freq rs db)
    (construct-ugen "BLowShelf" (list 0) (list input freq rs db) nil 1 nil)))

(define BPF
  (lambda (input freq rq)
    (construct-ugen "BPF" (list 0) (list input freq rq) nil 1 nil)))

(define BPZ2
  (lambda (input)
    (construct-ugen "BPZ2" (list 0) (list input) nil 1 nil)))

(define BPeakEQ
  (lambda (input freq rq db)
    (construct-ugen "BPeakEQ" (list 0) (list input freq rq db) nil 1 nil)))

(define BRF
  (lambda (input freq rq)
    (construct-ugen "BRF" (list 0) (list input freq rq) nil 1 nil)))

(define BRZ2
  (lambda (input)
    (construct-ugen "BRZ2" (list 0) (list input) nil 1 nil)))

(define Balance2
  (lambda (left right pos level)
    (construct-ugen "Balance2" (list 0 1) (list left right pos level) nil 2 nil)))

(define Ball
  (lambda (rt input g damp friction)
    (construct-ugen "Ball" rt (list input g damp friction) nil 1 nil)))

(define BeatTrack
  (lambda (rt chain lock)
    (construct-ugen "BeatTrack" rt (list chain lock) nil 4 nil)))

(define BeatTrack2
  (lambda (rt busindex numfeatures windowsize phaseaccuracy lock weightingscheme)
    (construct-ugen "BeatTrack2" rt (list busindex numfeatures windowsize phaseaccuracy lock weightingscheme) nil 6 nil)))

(define BiPanB2
  (lambda (rt inA inB azimuth gain)
    (construct-ugen "BiPanB2" rt (list inA inB azimuth gain) nil 3 nil)))

(define BinaryOpUGen
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 nil)))

(define Blip
  (lambda (rt freq numharm)
    (construct-ugen "Blip" rt (list freq numharm) nil 1 nil)))

(define BlockSize (construct-ugen "BlockSize" ir nil nil 1 nil))

(define BrownNoise
  (lambda (rt)
    (construct-ugen "BrownNoise" rt nil nil 1 nil)))

(define BufAllpassC
  (lambda (buf input delaytime decaytime)
    (construct-ugen "BufAllpassC" (list 1) (list buf input delaytime decaytime) nil 1 nil)))

(define BufAllpassL
  (lambda (buf input delaytime decaytime)
    (construct-ugen "BufAllpassL" (list 1) (list buf input delaytime decaytime) nil 1 nil)))

(define BufAllpassN
  (lambda (buf input delaytime decaytime)
    (construct-ugen "BufAllpassN" (list 1) (list buf input delaytime decaytime) nil 1 nil)))

(define BufChannels
  (lambda (rt bufnum)
    (construct-ugen "BufChannels" rt (list bufnum) nil 1 nil)))

(define BufCombC
  (lambda (buf input delaytime decaytime)
    (construct-ugen "BufCombC" (list 1) (list buf input delaytime decaytime) nil 1 nil)))

(define BufCombL
  (lambda (buf input delaytime decaytime)
    (construct-ugen "BufCombL" (list 1) (list buf input delaytime decaytime) nil 1 nil)))

(define BufCombN
  (lambda (buf input delaytime decaytime)
    (construct-ugen "BufCombN" (list 1) (list buf input delaytime decaytime) nil 1 nil)))

(define BufDelayC
  (lambda (buf input delaytime)
    (construct-ugen "BufDelayC" (list 1) (list buf input delaytime) nil 1 nil)))

(define BufDelayL
  (lambda (buf input delaytime)
    (construct-ugen "BufDelayL" (list 1) (list buf input delaytime) nil 1 nil)))

(define BufDelayN
  (lambda (buf input delaytime)
    (construct-ugen "BufDelayN" (list 1) (list buf input delaytime) nil 1 nil)))

(define BufDur
  (lambda (rt bufnum)
    (construct-ugen "BufDur" rt (list bufnum) nil 1 nil)))

(define BufFrames
  (lambda (rt bufnum)
    (construct-ugen "BufFrames" rt (list bufnum) nil 1 nil)))

(define BufRateScale
  (lambda (rt bufnum)
    (construct-ugen "BufRateScale" rt (list bufnum) nil 1 nil)))

(define BufRd
  (lambda (nc rt bufnum phase loop interpolation)
    (construct-ugen "BufRd" rt (list bufnum phase loop interpolation) nil nc nil)))

(define BufSampleRate
  (lambda (rt bufnum)
    (construct-ugen "BufSampleRate" rt (list bufnum) nil 1 nil)))

(define BufSamples
  (lambda (rt bufnum)
    (construct-ugen "BufSamples" rt (list bufnum) nil 1 nil)))

(define BufWr
  (lambda (bufnum phase loop inputArray)
    (construct-ugen "BufWr" (list 3) (list bufnum phase loop) inputArray 1 nil)))

(define COsc
  (lambda (rt bufnum freq beats)
    (construct-ugen "COsc" rt (list bufnum freq beats) nil 1 nil)))

(define CheckBadValues
  (lambda (input id post)
    (construct-ugen "CheckBadValues" (list 0) (list input id post) nil 1 nil)))

(define Clip
  (lambda (input lo hi)
    (construct-ugen "Clip" (list 0) (list input lo hi) nil 1 nil)))

(define ClipNoise
  (lambda (rt)
    (construct-ugen "ClipNoise" rt nil nil 1 nil)))

(define CoinGate
  (lambda (prob input)
    (construct-ugen "CoinGate" (list 1) (list prob input) nil 1 nil)))

(define CombC
  (lambda (input maxdelaytime delaytime decaytime)
    (construct-ugen "CombC" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil)))

(define CombL
  (lambda (input maxdelaytime delaytime decaytime)
    (construct-ugen "CombL" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil)))

(define CombN
  (lambda (input maxdelaytime delaytime decaytime)
    (construct-ugen "CombN" (list 0) (list input maxdelaytime delaytime decaytime) nil 1 nil)))

(define Compander
  (lambda (input control_ thresh slopeBelow slopeAbove clampTime relaxTime)
    (construct-ugen "Compander" (list 0) (list input control_ thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil)))

(define CompanderD
  (lambda (rt input thresh slopeBelow slopeAbove clampTime relaxTime)
    (construct-ugen "CompanderD" rt (list input thresh slopeBelow slopeAbove clampTime relaxTime) nil 1 nil)))

(define ControlDur (construct-ugen "ControlDur" ir nil nil 1 nil))

(define ControlRate (construct-ugen "ControlRate" ir nil nil 1 nil))

(define Convolution
  (lambda (input kernel framesize)
    (construct-ugen "Convolution" ar (list input kernel framesize) nil 1 nil)))

(define Convolution2
  (lambda (input kernel trigger framesize)
    (construct-ugen "Convolution2" ar (list input kernel trigger framesize) nil 1 nil)))

(define Convolution2L
  (lambda (rt input kernel trigger framesize crossfade)
    (construct-ugen "Convolution2L" rt (list input kernel trigger framesize crossfade) nil 1 nil)))

(define Convolution3
  (lambda (rt input kernel trigger framesize)
    (construct-ugen "Convolution3" rt (list input kernel trigger framesize) nil 1 nil)))

(define Crackle
  (lambda (rt chaosParam)
    (construct-ugen "Crackle" rt (list chaosParam) nil 1 nil)))

(define CuspL
  (lambda (rt freq a b xi)
    (construct-ugen "CuspL" rt (list freq a b xi) nil 1 nil)))

(define CuspN
  (lambda (rt freq a b xi)
    (construct-ugen "CuspN" rt (list freq a b xi) nil 1 nil)))

(define DC
  (lambda (rt input)
    (construct-ugen "DC" rt (list input) nil 1 nil)))

(define Dbrown
  (lambda (length_ lo hi step)
    (construct-ugen "Dbrown" dr (list length_ lo hi step) nil 1 nil)))

(define Dbufrd
  (lambda (bufnum phase loop)
    (construct-ugen "Dbufrd" dr (list bufnum phase loop) nil 1 nil)))

(define Dbufwr
  (lambda (bufnum phase loop input)
    (construct-ugen "Dbufwr" dr (list bufnum phase loop input) nil 1 nil)))

(define Dconst
  (lambda (sum_ input tolerance)
    (construct-ugen "Dconst" dr (list sum_ input tolerance) nil 1 nil)))

(define Decay
  (lambda (input decayTime)
    (construct-ugen "Decay" (list 0) (list input decayTime) nil 1 nil)))

(define Decay2
  (lambda (input attackTime decayTime)
    (construct-ugen "Decay2" (list 0) (list input attackTime decayTime) nil 1 nil)))

(define DecodeB2
  (lambda (nc w x y orientation)
    (construct-ugen "DecodeB2" (list 0 1 2) (list w x y orientation) nil nc nil)))

(define DegreeToKey
  (lambda (bufnum input octave)
    (construct-ugen "DegreeToKey" (list 1) (list bufnum input octave) nil 1 nil)))

(define DelTapRd
  (lambda (buffer phase delTime interp)
    (construct-ugen "DelTapRd" (list 1) (list buffer phase delTime interp) nil 1 nil)))

(define DelTapWr
  (lambda (buffer input)
    (construct-ugen "DelTapWr" (list 1) (list buffer input) nil 1 nil)))

(define Delay1
  (lambda (input)
    (construct-ugen "Delay1" (list 0) (list input) nil 1 nil)))

(define Delay2
  (lambda (input)
    (construct-ugen "Delay2" (list 0) (list input) nil 1 nil)))

(define DelayC
  (lambda (input maxdelaytime delaytime)
    (construct-ugen "DelayC" (list 0) (list input maxdelaytime delaytime) nil 1 nil)))

(define DelayL
  (lambda (input maxdelaytime delaytime)
    (construct-ugen "DelayL" (list 0) (list input maxdelaytime delaytime) nil 1 nil)))

(define DelayN
  (lambda (input maxdelaytime delaytime)
    (construct-ugen "DelayN" (list 0) (list input maxdelaytime delaytime) nil 1 nil)))

(define Demand
  (lambda (trig_ reset demandUGens)
    (construct-ugen "Demand" (list 0) (list trig_ reset) demandUGens (length (mce-channels demandUGens)) nil)))

(define DemandEnvGen
  (lambda (rt level dur shape curve gate_ reset levelScale levelBias timeScale doneAction)
    (construct-ugen "DemandEnvGen" rt (list level dur shape curve gate_ reset levelScale levelBias timeScale doneAction) nil 1 nil)))

(define DetectIndex
  (lambda (bufnum input)
    (construct-ugen "DetectIndex" (list 1) (list bufnum input) nil 1 nil)))

(define DetectSilence
  (lambda (input amp time doneAction)
    (construct-ugen "DetectSilence" (list 0) (list input amp time doneAction) nil 1 nil)))

(define Dgeom
  (lambda (length_ start grow)
    (construct-ugen "Dgeom" dr (list length_ start grow) nil 1 nil)))

(define Dibrown
  (lambda (length_ lo hi step)
    (construct-ugen "Dibrown" dr (list length_ lo hi step) nil 1 nil)))

(define DiskIn
  (lambda (nc bufnum loop)
    (construct-ugen "DiskIn" ar (list bufnum loop) nil nc nil)))

(define DiskOut
  (lambda (bufnum input)
    (construct-ugen "DiskOut" ar (list bufnum) input 1 nil)))

(define Diwhite
  (lambda (length_ lo hi)
    (construct-ugen "Diwhite" dr (list length_ lo hi) nil 1 nil)))

(define Done
  (lambda (src)
    (construct-ugen "Done" kr (list src) nil 1 nil)))

(define Dpoll
  (lambda (input label_ run trigid)
    (construct-ugen "Dpoll" dr (list input label_ run trigid) nil 1 nil)))

(define Drand
  (lambda (repeats list_)
    (construct-ugen "Drand" dr (list repeats) list_ 1 nil)))

(define Dreset
  (lambda (input reset)
    (construct-ugen "Dreset" dr (list input reset) nil 1 nil)))

(define Dseq
  (lambda (repeats list_)
    (construct-ugen "Dseq" dr (list repeats) list_ 1 nil)))

(define Dser
  (lambda (repeats list_)
    (construct-ugen "Dser" dr (list repeats) list_ 1 nil)))

(define Dseries
  (lambda (length_ start step)
    (construct-ugen "Dseries" dr (list length_ start step) nil 1 nil)))

(define Dshuf
  (lambda (repeats list_)
    (construct-ugen "Dshuf" dr (list repeats) list_ 1 nil)))

(define Dstutter
  (lambda (n input)
    (construct-ugen "Dstutter" dr (list n input) nil 1 nil)))

(define Dswitch
  (lambda (index list_)
    (construct-ugen "Dswitch" dr (list index) list_ 1 nil)))

(define Dswitch1
  (lambda (index list_)
    (construct-ugen "Dswitch1" dr (list index) list_ 1 nil)))

(define Dunique
  (lambda (source maxBufferSize protected)
    (construct-ugen "Dunique" dr (list source maxBufferSize protected) nil 1 nil)))

(define Dust
  (lambda (rt density)
    (construct-ugen "Dust" rt (list density) nil 1 nil)))

(define Dust2
  (lambda (rt density)
    (construct-ugen "Dust2" rt (list density) nil 1 nil)))

(define Duty
  (lambda (rt dur reset doneAction level)
    (construct-ugen "Duty" rt (list dur reset doneAction level) nil 1 nil)))

(define Dwhite
  (lambda (length_ lo hi)
    (construct-ugen "Dwhite" dr (list length_ lo hi) nil 1 nil)))

(define Dwrand
  (lambda (repeats weights list_)
    (construct-ugen "Dwrand" dr (list repeats weights) list_ 1 nil)))

(define Dxrand
  (lambda (repeats list_)
    (construct-ugen "Dxrand" dr (list repeats) list_ 1 nil)))

(define EnvGen
  (lambda (rt gate_ levelScale levelBias timeScale doneAction envelope_)
    (construct-ugen "EnvGen" rt (list gate_ levelScale levelBias timeScale doneAction) envelope_ 1 nil)))

(define ExpRand
  (lambda (lo hi)
    (construct-ugen "ExpRand" ir (list lo hi) nil 1 nil)))

(define FBSineC
  (lambda (rt freq im fb a c xi yi)
    (construct-ugen "FBSineC" rt (list freq im fb a c xi yi) nil 1 nil)))

(define FBSineL
  (lambda (rt freq im fb a c xi yi)
    (construct-ugen "FBSineL" rt (list freq im fb a c xi yi) nil 1 nil)))

(define FBSineN
  (lambda (rt freq im fb a c xi yi)
    (construct-ugen "FBSineN" rt (list freq im fb a c xi yi) nil 1 nil)))

(define FFT
  (lambda (buffer input hop wintype active winsize)
    (construct-ugen "FFT" kr (list buffer input hop wintype active winsize) nil 1 nil)))

(define FOS
  (lambda (input a0 a1 b1)
    (construct-ugen "FOS" (list 0) (list input a0 a1 b1) nil 1 nil)))

(define FSinOsc
  (lambda (rt freq iphase)
    (construct-ugen "FSinOsc" rt (list freq iphase) nil 1 nil)))

(define Fold
  (lambda (input lo hi)
    (construct-ugen "Fold" (list 0) (list input lo hi) nil 1 nil)))

(define Formant
  (lambda (rt fundfreq formfreq bwfreq)
    (construct-ugen "Formant" rt (list fundfreq formfreq bwfreq) nil 1 nil)))

(define Formlet
  (lambda (input freq attacktime decaytime)
    (construct-ugen "Formlet" (list 0) (list input freq attacktime decaytime) nil 1 nil)))

(define Free
  (lambda (trig_ id_)
    (construct-ugen "Free" (list 0) (list trig_ id_) nil 1 nil)))

(define FreeSelf
  (lambda (input)
    (construct-ugen "FreeSelf" kr (list input) nil 1 nil)))

(define FreeSelfWhenDone
  (lambda (src)
    (construct-ugen "FreeSelfWhenDone" kr (list src) nil 1 nil)))

(define FreeVerb
  (lambda (input mix room damp)
    (construct-ugen "FreeVerb" (list 0) (list input mix room damp) nil 1 nil)))

(define FreeVerb2
  (lambda (input in2 mix room damp)
    (construct-ugen "FreeVerb2" (list 0) (list input in2 mix room damp) nil 2 nil)))

(define FreqShift
  (lambda (input freq phase)
    (construct-ugen "FreqShift" ar (list input freq phase) nil 1 nil)))

(define GVerb
  (lambda (input roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize)
    (construct-ugen "GVerb" (list 0) (list input roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize) nil 2 nil)))

(define Gate
  (lambda (input trig_)
    (construct-ugen "Gate" (list 0) (list input trig_) nil 1 nil)))

(define GbmanL
  (lambda (rt freq xi yi)
    (construct-ugen "GbmanL" rt (list freq xi yi) nil 1 nil)))

(define GbmanN
  (lambda (rt freq xi yi)
    (construct-ugen "GbmanN" rt (list freq xi yi) nil 1 nil)))

(define Gendy1
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (construct-ugen "Gendy1" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil)))

(define Gendy2
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c)
    (construct-ugen "Gendy2" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c) nil 1 nil)))

(define Gendy3
  (lambda (rt ampdist durdist adparam ddparam freq ampscale durscale initCPs knum)
    (construct-ugen "Gendy3" rt (list ampdist durdist adparam ddparam freq ampscale durscale initCPs knum) nil 1 nil)))

(define GrainBuf
  (lambda (nc trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains)
    (construct-ugen "GrainBuf" ar (list trigger dur sndbuf rate_ pos interp pan envbufnum maxGrains) nil nc nil)))

(define GrainFM
  (lambda (nc trigger dur carfreq modfreq index pan envbufnum maxGrains)
    (construct-ugen "GrainFM" ar (list trigger dur carfreq modfreq index pan envbufnum maxGrains) nil nc nil)))

(define GrainIn
  (lambda (nc trigger dur input pan envbufnum maxGrains)
    (construct-ugen "GrainIn" ar (list trigger dur input pan envbufnum maxGrains) nil nc nil)))

(define GrainSin
  (lambda (nc trigger dur freq pan envbufnum maxGrains)
    (construct-ugen "GrainSin" ar (list trigger dur freq pan envbufnum maxGrains) nil nc nil)))

(define GrayNoise
  (lambda (rt)
    (construct-ugen "GrayNoise" rt nil nil 1 nil)))

(define HPF
  (lambda (input freq)
    (construct-ugen "HPF" (list 0) (list input freq) nil 1 nil)))

(define HPZ1
  (lambda (input)
    (construct-ugen "HPZ1" (list 0) (list input) nil 1 nil)))

(define HPZ2
  (lambda (input)
    (construct-ugen "HPZ2" (list 0) (list input) nil 1 nil)))

(define Hasher
  (lambda (input)
    (construct-ugen "Hasher" (list 0) (list input) nil 1 nil)))

(define HenonC
  (lambda (rt freq a b x0 x1)
    (construct-ugen "HenonC" rt (list freq a b x0 x1) nil 1 nil)))

(define HenonL
  (lambda (rt freq a b x0 x1)
    (construct-ugen "HenonL" rt (list freq a b x0 x1) nil 1 nil)))

(define HenonN
  (lambda (rt freq a b x0 x1)
    (construct-ugen "HenonN" rt (list freq a b x0 x1) nil 1 nil)))

(define Hilbert
  (lambda (input)
    (construct-ugen "Hilbert" (list 0) (list input) nil 2 nil)))

(define IEnvGen
  (lambda (rt index envelope_)
    (construct-ugen "IEnvGen" rt (list index) envelope_ 1 nil)))

(define IFFT
  (lambda (buffer wintype winsize)
    (construct-ugen "IFFT" ar (list buffer wintype winsize) nil 1 nil)))

(define IRand
  (lambda (lo hi)
    (construct-ugen "IRand" ir (list lo hi) nil 1 nil)))

(define Impulse
  (lambda (rt freq phase)
    (construct-ugen "Impulse" rt (list freq phase) nil 1 nil)))

(define In
  (lambda (nc rt bus)
    (construct-ugen "In" rt (list bus) nil nc nil)))

(define InFeedback
  (lambda (nc bus)
    (construct-ugen "InFeedback" ar (list bus) nil nc nil)))

(define InRange
  (lambda (input lo hi)
    (construct-ugen "InRange" (list 0) (list input lo hi) nil 1 nil)))

(define InRect
  (lambda (rt x y rect)
    (construct-ugen "InRect" rt (list x y rect) nil 1 nil)))

(define InTrig
  (lambda (nc bus)
    (construct-ugen "InTrig" kr (list bus) nil nc nil)))

(define Index
  (lambda (bufnum input)
    (construct-ugen "Index" (list 1) (list bufnum input) nil 1 nil)))

(define IndexInBetween
  (lambda (bufnum input)
    (construct-ugen "IndexInBetween" (list 1) (list bufnum input) nil 1 nil)))

(define IndexL
  (lambda (bufnum input)
    (construct-ugen "IndexL" (list 1) (list bufnum input) nil 1 nil)))

(define InfoUGenBase
  (lambda (rt)
    (construct-ugen "InfoUGenBase" rt nil nil 1 nil)))

(define Integrator
  (lambda (input coef)
    (construct-ugen "Integrator" (list 0) (list input coef) nil 1 nil)))

(define K2A
  (lambda (input)
    (construct-ugen "K2A" ar (list input) nil 1 nil)))

(define KeyState
  (lambda (rt keycode minval maxval lag)
    (construct-ugen "KeyState" rt (list keycode minval maxval lag) nil 1 nil)))

(define KeyTrack
  (lambda (rt chain keydecay chromaleak)
    (construct-ugen "KeyTrack" rt (list chain keydecay chromaleak) nil 1 nil)))

(define Klang
  (lambda (rt freqscale freqoffset specificationsArrayRef)
    (construct-ugen "Klang" rt (list freqscale freqoffset) specificationsArrayRef 1 nil)))

(define Klank
  (lambda (input freqscale freqoffset decayscale specificationsArrayRef)
    (construct-ugen "Klank" (list 0) (list input freqscale freqoffset decayscale) specificationsArrayRef 1 nil)))

(define LFClipNoise
  (lambda (rt freq)
    (construct-ugen "LFClipNoise" rt (list freq) nil 1 nil)))

(define LFCub
  (lambda (rt freq iphase)
    (construct-ugen "LFCub" rt (list freq iphase) nil 1 nil)))

(define LFDClipNoise
  (lambda (rt freq)
    (construct-ugen "LFDClipNoise" rt (list freq) nil 1 nil)))

(define LFDNoise0
  (lambda (rt freq)
    (construct-ugen "LFDNoise0" rt (list freq) nil 1 nil)))

(define LFDNoise1
  (lambda (rt freq)
    (construct-ugen "LFDNoise1" rt (list freq) nil 1 nil)))

(define LFDNoise3
  (lambda (rt freq)
    (construct-ugen "LFDNoise3" rt (list freq) nil 1 nil)))

(define LFGauss
  (lambda (rt duration width iphase loop doneAction)
    (construct-ugen "LFGauss" rt (list duration width iphase loop doneAction) nil 1 nil)))

(define LFNoise0
  (lambda (rt freq)
    (construct-ugen "LFNoise0" rt (list freq) nil 1 nil)))

(define LFNoise1
  (lambda (rt freq)
    (construct-ugen "LFNoise1" rt (list freq) nil 1 nil)))

(define LFNoise2
  (lambda (rt freq)
    (construct-ugen "LFNoise2" rt (list freq) nil 1 nil)))

(define LFPar
  (lambda (rt freq iphase)
    (construct-ugen "LFPar" rt (list freq iphase) nil 1 nil)))

(define LFPulse
  (lambda (rt freq iphase width)
    (construct-ugen "LFPulse" rt (list freq iphase width) nil 1 nil)))

(define LFSaw
  (lambda (rt freq iphase)
    (construct-ugen "LFSaw" rt (list freq iphase) nil 1 nil)))

(define LFTri
  (lambda (rt freq iphase)
    (construct-ugen "LFTri" rt (list freq iphase) nil 1 nil)))

(define LPF
  (lambda (input freq)
    (construct-ugen "LPF" (list 0) (list input freq) nil 1 nil)))

(define LPZ1
  (lambda (input)
    (construct-ugen "LPZ1" (list 0) (list input) nil 1 nil)))

(define LPZ2
  (lambda (input)
    (construct-ugen "LPZ2" (list 0) (list input) nil 1 nil)))

(define Lag
  (lambda (input lagTime)
    (construct-ugen "Lag" (list 0) (list input lagTime) nil 1 nil)))

(define Lag2
  (lambda (input lagTime)
    (construct-ugen "Lag2" (list 0) (list input lagTime) nil 1 nil)))

(define Lag2UD
  (lambda (input lagTimeU lagTimeD)
    (construct-ugen "Lag2UD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil)))

(define Lag3
  (lambda (input lagTime)
    (construct-ugen "Lag3" (list 0) (list input lagTime) nil 1 nil)))

(define Lag3UD
  (lambda (input lagTimeU lagTimeD)
    (construct-ugen "Lag3UD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil)))

(define LagIn
  (lambda (nc bus lag)
    (construct-ugen "LagIn" kr (list bus lag) nil nc nil)))

(define LagUD
  (lambda (input lagTimeU lagTimeD)
    (construct-ugen "LagUD" (list 0) (list input lagTimeU lagTimeD) nil 1 nil)))

(define LastValue
  (lambda (input diff)
    (construct-ugen "LastValue" (list 0) (list input diff) nil 1 nil)))

(define Latch
  (lambda (input trig_)
    (construct-ugen "Latch" (list 0 1) (list input trig_) nil 1 nil)))

(define LatoocarfianC
  (lambda (rt freq a b c d xi yi)
    (construct-ugen "LatoocarfianC" rt (list freq a b c d xi yi) nil 1 nil)))

(define LatoocarfianL
  (lambda (rt freq a b c d xi yi)
    (construct-ugen "LatoocarfianL" rt (list freq a b c d xi yi) nil 1 nil)))

(define LatoocarfianN
  (lambda (rt freq a b c d xi yi)
    (construct-ugen "LatoocarfianN" rt (list freq a b c d xi yi) nil 1 nil)))

(define LeakDC
  (lambda (input coef)
    (construct-ugen "LeakDC" (list 0) (list input coef) nil 1 nil)))

(define LeastChange
  (lambda (rt a b)
    (construct-ugen "LeastChange" rt (list a b) nil 1 nil)))

(define Limiter
  (lambda (input level dur)
    (construct-ugen "Limiter" (list 0) (list input level dur) nil 1 nil)))

(define LinCongC
  (lambda (rt freq a c m xi)
    (construct-ugen "LinCongC" rt (list freq a c m xi) nil 1 nil)))

(define LinCongL
  (lambda (rt freq a c m xi)
    (construct-ugen "LinCongL" rt (list freq a c m xi) nil 1 nil)))

(define LinCongN
  (lambda (rt freq a c m xi)
    (construct-ugen "LinCongN" rt (list freq a c m xi) nil 1 nil)))

(define LinExp
  (lambda (input srclo srchi dstlo dsthi)
    (construct-ugen "LinExp" (list 0) (list input srclo srchi dstlo dsthi) nil 1 nil)))

(define LinPan2
  (lambda (input pos level)
    (construct-ugen "LinPan2" (list 0) (list input pos level) nil 2 nil)))

(define LinRand
  (lambda (lo hi minmax)
    (construct-ugen "LinRand" ir (list lo hi minmax) nil 1 nil)))

(define LinXFade2
  (lambda (inA inB pan level)
    (construct-ugen "LinXFade2" (list 0 1) (list inA inB pan level) nil 1 nil)))

(define Line
  (lambda (rt start end dur doneAction)
    (construct-ugen "Line" rt (list start end dur doneAction) nil 1 nil)))

(define Linen
  (lambda (gate_ attackTime susLevel releaseTime doneAction)
    (construct-ugen "Linen" kr (list gate_ attackTime susLevel releaseTime doneAction) nil 1 nil)))

(define LocalBuf
  (lambda (numChannels numFrames)
    (construct-ugen "LocalBuf" ir (list numChannels numFrames) nil 1 nil)))

(define LocalIn
  (lambda (nc rt default_)
    (construct-ugen "LocalIn" rt nil default_ nc nil)))

(define LocalOut
  (lambda (input)
    (construct-ugen "LocalOut" (list 0) nil input 0 nil)))

(define Logistic
  (lambda (rt chaosParam freq init_)
    (construct-ugen "Logistic" rt (list chaosParam freq init_) nil 1 nil)))

(define LorenzL
  (lambda (rt freq s r b h xi yi zi)
    (construct-ugen "LorenzL" rt (list freq s r b h xi yi zi) nil 1 nil)))

(define Loudness
  (lambda (chain smask tmask)
    (construct-ugen "Loudness" kr (list chain smask tmask) nil 1 nil)))

(define MFCC
  (lambda (rt chain numcoeff)
    (construct-ugen "MFCC" rt (list chain numcoeff) nil 13 nil)))

(define MantissaMask
  (lambda (input bits)
    (construct-ugen "MantissaMask" (list 0) (list input bits) nil 1 nil)))

(define Median
  (lambda (length_ input)
    (construct-ugen "Median" (list 1) (list length_ input) nil 1 nil)))

(define MidEQ
  (lambda (input freq rq db)
    (construct-ugen "MidEQ" (list 0) (list input freq rq db) nil 1 nil)))

(define ModDif
  (lambda (x y mod_)
    (construct-ugen "ModDif" (list 0) (list x y mod_) nil 1 nil)))

(define MoogFF
  (lambda (input freq gain reset)
    (construct-ugen "MoogFF" (list 0) (list input freq gain reset) nil 1 nil)))

(define MostChange
  (lambda (a b)
    (construct-ugen "MostChange" (list 0 1) (list a b) nil 1 nil)))

(define MouseButton
  (lambda (rt minval maxval lag)
    (construct-ugen "MouseButton" rt (list minval maxval lag) nil 1 nil)))

(define MouseX
  (lambda (rt minval maxval warp lag)
    (construct-ugen "MouseX" rt (list minval maxval warp lag) nil 1 nil)))

(define MouseY
  (lambda (rt minval maxval warp lag)
    (construct-ugen "MouseY" rt (list minval maxval warp lag) nil 1 nil)))

(define NRand
  (lambda (lo hi n)
    (construct-ugen "NRand" ir (list lo hi n) nil 1 nil)))

(define NodeID
  (lambda (rt)
    (construct-ugen "NodeID" rt nil nil 1 nil)))

(define Normalizer
  (lambda (input level dur)
    (construct-ugen "Normalizer" (list 0) (list input level dur) nil 1 nil)))

(define NumAudioBuses (construct-ugen "NumAudioBuses" ir nil nil 1 nil))

(define NumBuffers (construct-ugen "NumBuffers" ir nil nil 1 nil))

(define NumControlBuses (construct-ugen "NumControlBuses" ir nil nil 1 nil))

(define NumInputBuses (construct-ugen "NumInputBuses" ir nil nil 1 nil))

(define NumOutputBuses (construct-ugen "NumOutputBuses" ir nil nil 1 nil))

(define NumRunningSynths (construct-ugen "NumRunningSynths" ir nil nil 1 nil))

(define OffsetOut
  (lambda (bus input)
    (construct-ugen "OffsetOut" (list 1) (list bus) input 0 nil)))

(define OnePole
  (lambda (input coef)
    (construct-ugen "OnePole" (list 0) (list input coef) nil 1 nil)))

(define OneZero
  (lambda (input coef)
    (construct-ugen "OneZero" (list 0) (list input coef) nil 1 nil)))

(define Onsets
  (lambda (chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf)
    (construct-ugen "Onsets" kr (list chain threshold odftype relaxtime floor_ mingap medianspan whtype rawodf) nil 1 nil)))

(define Osc
  (lambda (rt bufnum freq phase)
    (construct-ugen "Osc" rt (list bufnum freq phase) nil 1 nil)))

(define OscN
  (lambda (rt bufnum freq phase)
    (construct-ugen "OscN" rt (list bufnum freq phase) nil 1 nil)))

(define Out
  (lambda (bus input)
    (construct-ugen "Out" (list 1) (list bus) input 0 nil)))

(define PSinGrain
  (lambda (rt freq dur amp)
    (construct-ugen "PSinGrain" rt (list freq dur amp) nil 1 nil)))

(define PV_Add
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Add" kr (list bufferA bufferB) nil 1 nil)))

(define PV_BinScramble
  (lambda (buffer wipe width trig_)
    (construct-ugen "PV_BinScramble" kr (list buffer wipe width trig_) nil 1 nil)))

(define PV_BinShift
  (lambda (buffer stretch shift interp)
    (construct-ugen "PV_BinShift" kr (list buffer stretch shift interp) nil 1 nil)))

(define PV_BinWipe
  (lambda (bufferA bufferB wipe)
    (construct-ugen "PV_BinWipe" kr (list bufferA bufferB wipe) nil 1 nil)))

(define PV_BrickWall
  (lambda (buffer wipe)
    (construct-ugen "PV_BrickWall" kr (list buffer wipe) nil 1 nil)))

(define PV_ChainUGen
  (lambda (maxSize)
    (construct-ugen "PV_ChainUGen" kr (list maxSize) nil 1 nil)))

(define PV_ConformalMap
  (lambda (buffer areal aimag)
    (construct-ugen "PV_ConformalMap" kr (list buffer areal aimag) nil 1 nil)))

(define PV_Conj
  (lambda (buffer)
    (construct-ugen "PV_Conj" kr (list buffer) nil 1 nil)))

(define PV_Copy
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Copy" kr (list bufferA bufferB) nil 1 nil)))

(define PV_CopyPhase
  (lambda (bufferA bufferB)
    (construct-ugen "PV_CopyPhase" kr (list bufferA bufferB) nil 1 nil)))

(define PV_Diffuser
  (lambda (buffer trig_)
    (construct-ugen "PV_Diffuser" kr (list buffer trig_) nil 1 nil)))

(define PV_Div
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Div" kr (list bufferA bufferB) nil 1 nil)))

;(define PV_HainsworthFoote
;  (lambda (maxSize)
;    (construct-ugen "PV_HainsworthFoote" kr (list maxSize) nil 1 nil)))

;(define PV_JensenAndersen
;  (lambda (maxSize)
;    (construct-ugen "PV_JensenAndersen" kr (list maxSize) nil 1 nil)))

(define PV_LocalMax
  (lambda (buffer threshold)
    (construct-ugen "PV_LocalMax" kr (list buffer threshold) nil 1 nil)))

(define PV_MagAbove
  (lambda (buffer threshold)
    (construct-ugen "PV_MagAbove" kr (list buffer threshold) nil 1 nil)))

(define PV_MagBelow
  (lambda (buffer threshold)
    (construct-ugen "PV_MagBelow" kr (list buffer threshold) nil 1 nil)))

(define PV_MagClip
  (lambda (buffer threshold)
    (construct-ugen "PV_MagClip" kr (list buffer threshold) nil 1 nil)))

(define PV_MagDiv
  (lambda (bufferA bufferB zeroed)
    (construct-ugen "PV_MagDiv" kr (list bufferA bufferB zeroed) nil 1 nil)))

(define PV_MagFreeze
  (lambda (buffer freeze)
    (construct-ugen "PV_MagFreeze" kr (list buffer freeze) nil 1 nil)))

(define PV_MagMul
  (lambda (bufferA bufferB)
    (construct-ugen "PV_MagMul" kr (list bufferA bufferB) nil 1 nil)))

(define PV_MagNoise
  (lambda (buffer)
    (construct-ugen "PV_MagNoise" kr (list buffer) nil 1 nil)))

(define PV_MagShift
  (lambda (buffer stretch shift)
    (construct-ugen "PV_MagShift" kr (list buffer stretch shift) nil 1 nil)))

(define PV_MagSmear
  (lambda (buffer bins)
    (construct-ugen "PV_MagSmear" kr (list buffer bins) nil 1 nil)))

(define PV_MagSquared
  (lambda (buffer)
    (construct-ugen "PV_MagSquared" kr (list buffer) nil 1 nil)))

(define PV_Max
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Max" kr (list bufferA bufferB) nil 1 nil)))

(define PV_Min
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Min" kr (list bufferA bufferB) nil 1 nil)))

(define PV_Mul
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Mul" kr (list bufferA bufferB) nil 1 nil)))

(define PV_PhaseShift
  (lambda (buffer shift integrate)
    (construct-ugen "PV_PhaseShift" kr (list buffer shift integrate) nil 1 nil)))

(define PV_PhaseShift270
  (lambda (buffer)
    (construct-ugen "PV_PhaseShift270" kr (list buffer) nil 1 nil)))

(define PV_PhaseShift90
  (lambda (buffer)
    (construct-ugen "PV_PhaseShift90" kr (list buffer) nil 1 nil)))

(define PV_RandComb
  (lambda (buffer wipe trig_)
    (construct-ugen "PV_RandComb" kr (list buffer wipe trig_) nil 1 nil)))

(define PV_RandWipe
  (lambda (bufferA bufferB wipe trig_)
    (construct-ugen "PV_RandWipe" kr (list bufferA bufferB wipe trig_) nil 1 nil)))

(define PV_RectComb
  (lambda (buffer numTeeth phase width)
    (construct-ugen "PV_RectComb" kr (list buffer numTeeth phase width) nil 1 nil)))

(define PV_RectComb2
  (lambda (bufferA bufferB numTeeth phase width)
    (construct-ugen "PV_RectComb2" kr (list bufferA bufferB numTeeth phase width) nil 1 nil)))

(define Pan2
  (lambda (input pos level)
    (construct-ugen "Pan2" (list 0) (list input pos level) nil 2 nil)))

(define Pan4
  (lambda (rt input xpos ypos level)
    (construct-ugen "Pan4" rt (list input xpos ypos level) nil 4 nil)))

(define PanAz
  (lambda (nc input pos level width orientation)
    (construct-ugen "PanAz" (list 0) (list input pos level width orientation) nil nc nil)))

(define PanB
  (lambda (rt input azimuth elevation gain)
    (construct-ugen "PanB" rt (list input azimuth elevation gain) nil 4 nil)))

(define PanB2
  (lambda (input azimuth gain)
    (construct-ugen "PanB2" (list 0) (list input azimuth gain) nil 3 nil)))

(define PartConv
  (lambda (input fftsize irbufnum)
    (construct-ugen "PartConv" ar (list input fftsize irbufnum) nil 1 nil)))

(define Pause
  (lambda (gate_ id_)
    (construct-ugen "Pause" kr (list gate_ id_) nil 1 nil)))

(define PauseSelf
  (lambda (input)
    (construct-ugen "PauseSelf" kr (list input) nil 1 nil)))

(define PauseSelfWhenDone
  (lambda (src)
    (construct-ugen "PauseSelfWhenDone" kr (list src) nil 1 nil)))

(define Peak
  (lambda (input trig_)
    (construct-ugen "Peak" (list 0) (list input trig_) nil 1 nil)))

(define PeakFollower
  (lambda (input decay_)
    (construct-ugen "PeakFollower" (list 0) (list input decay_) nil 1 nil)))

(define Phasor
  (lambda (rt trig_ rate_ start end resetPos)
    (construct-ugen "Phasor" rt (list trig_ rate_ start end resetPos) nil 1 nil)))

(define PinkNoise
  (lambda (rt)
    (construct-ugen "PinkNoise" rt nil nil 1 nil)))

(define Pitch
  (lambda (input initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar)
    (construct-ugen "Pitch" kr (list input initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar) nil 2 nil)))

(define PitchShift
  (lambda (input windowSize pitchRatio pitchDispersion timeDispersion)
    (construct-ugen "PitchShift" (list 0) (list input windowSize pitchRatio pitchDispersion timeDispersion) nil 1 nil)))

(define PlayBuf
  (lambda (nc rt bufnum rate_ trigger startPos loop doneAction)
    (construct-ugen "PlayBuf" rt (list bufnum rate_ trigger startPos loop doneAction) nil nc nil)))

(define Pluck
  (lambda (input trig_ maxdelaytime delaytime decaytime coef)
    (construct-ugen "Pluck" (list 0) (list input trig_ maxdelaytime delaytime decaytime coef) nil 1 nil)))

;(define Poll
;  (lambda (trig_ input trigid label_)
;    (construct-ugen "Poll" (list 1) (list trig_ input trigid label_) nil 1 nil)))

(define Pulse
  (lambda (rt freq width)
    (construct-ugen "Pulse" rt (list freq width) nil 1 nil)))

(define PulseCount
  (lambda (trig_ reset)
    (construct-ugen "PulseCount" (list 0) (list trig_ reset) nil 1 nil)))

(define PulseDivider
  (lambda (trig_ div_ start)
    (construct-ugen "PulseDivider" (list 0) (list trig_ div_ start) nil 1 nil)))

(define QuadC
  (lambda (rt freq a b c xi)
    (construct-ugen "QuadC" rt (list freq a b c xi) nil 1 nil)))

(define QuadL
  (lambda (rt freq a b c xi)
    (construct-ugen "QuadL" rt (list freq a b c xi) nil 1 nil)))

(define QuadN
  (lambda (rt freq a b c xi)
    (construct-ugen "QuadN" rt (list freq a b c xi) nil 1 nil)))

(define RHPF
  (lambda (input freq rq)
    (construct-ugen "RHPF" (list 0) (list input freq rq) nil 1 nil)))

(define RLPF
  (lambda (input freq rq)
    (construct-ugen "RLPF" (list 0) (list input freq rq) nil 1 nil)))

(define RadiansPerSample (construct-ugen "RadiansPerSample" ir nil nil 1 nil))

(define Ramp
  (lambda (input lagTime)
    (construct-ugen "Ramp" (list 0) (list input lagTime) nil 1 nil)))

(define Rand
  (lambda (lo hi)
    (construct-ugen "Rand" ir (list lo hi) nil 1 nil)))

(define RandID
  (lambda (rt id_)
    (construct-ugen "RandID" rt (list id_) nil 0 nil)))

(define RandSeed
  (lambda (rt trig_ seed)
    (construct-ugen "RandSeed" rt (list trig_ seed) nil 0 nil)))

(define RecordBuf
  (lambda (rt bufnum offset recLevel preLevel run loop trigger doneAction inputArray)
    (construct-ugen "RecordBuf" rt (list bufnum offset recLevel preLevel run loop trigger doneAction) inputArray 1 nil)))

(define ReplaceOut
  (lambda (bus input)
    (construct-ugen "ReplaceOut" (list 1) (list bus) input 0 nil)))

(define Resonz
  (lambda (input freq bwr)
    (construct-ugen "Resonz" (list 0) (list input freq bwr) nil 1 nil)))

(define Ringz
  (lambda (input freq decaytime)
    (construct-ugen "Ringz" (list 0) (list input freq decaytime) nil 1 nil)))

(define Rotate2
  (lambda (x y pos)
    (construct-ugen "Rotate2" (list 0 1) (list x y pos) nil 2 nil)))

(define RunningMax
  (lambda (input trig_)
    (construct-ugen "RunningMax" (list 0) (list input trig_) nil 1 nil)))

(define RunningMin
  (lambda (input trig_)
    (construct-ugen "RunningMin" (list 0) (list input trig_) nil 1 nil)))

(define RunningSum
  (lambda (input numsamp)
    (construct-ugen "RunningSum" (list 0) (list input numsamp) nil 1 nil)))

(define SOS
  (lambda (input a0 a1 a2 b1 b2)
    (construct-ugen "SOS" (list 0) (list input a0 a1 a2 b1 b2) nil 1 nil)))

(define SampleDur (construct-ugen "SampleDur" ir nil nil 1 nil))

(define SampleRate (construct-ugen "SampleRate" ir nil nil 1 nil))

(define Sanitize
  (lambda (input replace)
    (construct-ugen "Sanitize" (list 0) (list input replace) nil 1 nil)))

(define Saw
  (lambda (rt freq)
    (construct-ugen "Saw" rt (list freq) nil 1 nil)))

(define Schmidt
  (lambda (input lo hi)
    (construct-ugen "Schmidt" (list 0) (list input lo hi) nil 1 nil)))

(define Select
  (lambda (which array)
    (construct-ugen "Select" (list 0 1) (list which) array 1 nil)))

(define SendTrig
  (lambda (input id_ value)
    (construct-ugen "SendTrig" (list 0) (list input id_ value) nil 0 nil)))

(define SetResetFF
  (lambda (trig_ reset)
    (construct-ugen "SetResetFF" (list 0 1) (list trig_ reset) nil 1 nil)))

(define Shaper
  (lambda (bufnum input)
    (construct-ugen "Shaper" (list 1) (list bufnum input) nil 1 nil)))

(define SinOsc
  (lambda (rt freq phase)
    (construct-ugen "SinOsc" rt (list freq phase) nil 1 nil)))

(define SinOscFB
  (lambda (rt freq feedback)
    (construct-ugen "SinOscFB" rt (list freq feedback) nil 1 nil)))

(define Slew
  (lambda (input up dn)
    (construct-ugen "Slew" (list 0) (list input up dn) nil 1 nil)))

(define Slope
  (lambda (input)
    (construct-ugen "Slope" (list 0) (list input) nil 1 nil)))

(define SpecCentroid
  (lambda (rt buffer)
    (construct-ugen "SpecCentroid" rt (list buffer) nil 1 nil)))

(define SpecFlatness
  (lambda (rt buffer)
    (construct-ugen "SpecFlatness" rt (list buffer) nil 1 nil)))

(define SpecPcile
  (lambda (rt buffer fraction interpolate)
    (construct-ugen "SpecPcile" rt (list buffer fraction interpolate) nil 1 nil)))

(define Spring
  (lambda (rt input spring damp)
    (construct-ugen "Spring" rt (list input spring damp) nil 1 nil)))

(define StandardL
  (lambda (rt freq k xi yi)
    (construct-ugen "StandardL" rt (list freq k xi yi) nil 1 nil)))

(define StandardN
  (lambda (rt freq k xi yi)
    (construct-ugen "StandardN" rt (list freq k xi yi) nil 1 nil)))

(define Stepper
  (lambda (trig_ reset min_ max_ step resetval)
    (construct-ugen "Stepper" (list 0) (list trig_ reset min_ max_ step resetval) nil 1 nil)))

(define StereoConvolution2L
  (lambda (rt input kernelL kernelR trigger framesize crossfade)
    (construct-ugen "StereoConvolution2L" rt (list input kernelL kernelR trigger framesize crossfade) nil 2 nil)))

(define SubsampleOffset (construct-ugen "SubsampleOffset" ir nil nil 1 nil))

(define Sum3
  (lambda (in0 in1 in2)
    (construct-ugen "Sum3" (list 0 1 2) (list in0 in1 in2) nil 1 nil)))

(define Sum4
  (lambda (in0 in1 in2 in3)
    (construct-ugen "Sum4" (list 0 1 2 3) (list in0 in1 in2 in3) nil 1 nil)))

(define Sweep
  (lambda (trig_ rate_)
    (construct-ugen "Sweep" (list 0) (list trig_ rate_) nil 1 nil)))

(define SyncSaw
  (lambda (rt syncFreq sawFreq)
    (construct-ugen "SyncSaw" rt (list syncFreq sawFreq) nil 1 nil)))

(define T2A
  (lambda (input offset)
    (construct-ugen "T2A" ar (list input offset) nil 1 nil)))

(define T2K
  (lambda (input)
    (construct-ugen "T2K" kr (list input) nil 1 nil)))

(define TBall
  (lambda (rt input g damp friction)
    (construct-ugen "TBall" rt (list input g damp friction) nil 1 nil)))

(define TDelay
  (lambda (input dur)
    (construct-ugen "TDelay" (list 0) (list input dur) nil 1 nil)))

(define TDuty
  (lambda (rt dur reset doneAction level gapFirst)
    (construct-ugen "TDuty" rt (list dur reset doneAction level gapFirst) nil 1 nil)))

(define TExpRand
  (lambda (lo hi trig_)
    (construct-ugen "TExpRand" (list 2) (list lo hi trig_) nil 1 nil)))

(define TGrains
  (lambda (nc trigger bufnum rate_ centerPos dur pan amp interp)
    (construct-ugen "TGrains" ar (list trigger bufnum rate_ centerPos dur pan amp interp) nil nc nil)))

(define TIRand
  (lambda (lo hi trig_)
    (construct-ugen "TIRand" (list 2) (list lo hi trig_) nil 1 nil)))

(define TRand
  (lambda (lo hi trig_)
    (construct-ugen "TRand" (list 2) (list lo hi trig_) nil 1 nil)))

(define TWindex
  (lambda (input normalize array)
    (construct-ugen "TWindex" (list 0) (list input normalize) array 1 nil)))

(define Timer
  (lambda (trig_)
    (construct-ugen "Timer" (list 0) (list trig_) nil 1 nil)))

(define ToggleFF
  (lambda (trig_)
    (construct-ugen "ToggleFF" (list 0) (list trig_) nil 1 nil)))

(define Trig
  (lambda (input dur)
    (construct-ugen "Trig" (list 0) (list input dur) nil 1 nil)))

(define Trig1
  (lambda (input dur)
    (construct-ugen "Trig1" (list 0) (list input dur) nil 1 nil)))

(define TrigControl
  (lambda (rt values)
    (construct-ugen "TrigControl" rt (list values) nil 1 nil)))

(define TwoPole
  (lambda (input freq radius)
    (construct-ugen "TwoPole" (list 0) (list input freq radius) nil 1 nil)))

(define TwoZero
  (lambda (input freq radius)
    (construct-ugen "TwoZero" (list 0) (list input freq radius) nil 1 nil)))

(define UnaryOpUGen
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 nil)))

(define VDiskIn
  (lambda (nc bufnum rate_ loop sendID)
    (construct-ugen "VDiskIn" ar (list bufnum rate_ loop sendID) nil nc nil)))

(define VOsc
  (lambda (rt bufpos freq phase)
    (construct-ugen "VOsc" rt (list bufpos freq phase) nil 1 nil)))

(define VOsc3
  (lambda (rt bufpos freq1 freq2 freq3)
    (construct-ugen "VOsc3" rt (list bufpos freq1 freq2 freq3) nil 1 nil)))

(define VarLag
  (lambda (input time curvature warp start)
    (construct-ugen "VarLag" (list 0) (list input time curvature warp start) nil 1 nil)))

(define VarSaw
  (lambda (rt freq iphase width)
    (construct-ugen "VarSaw" rt (list freq iphase width) nil 1 nil)))

(define Vibrato
  (lambda (rt freq rate_ depth delay onset rateVariation depthVariation iphase trig_)
    (construct-ugen "Vibrato" rt (list freq rate_ depth delay onset rateVariation depthVariation iphase trig_) nil 1 nil)))

(define Warp1
  (lambda (nc bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp)
    (construct-ugen "Warp1" ar (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp) nil nc nil)))

(define WhiteNoise
  (lambda (rt)
    (construct-ugen "WhiteNoise" rt nil nil 1 nil)))

(define Wrap
  (lambda (input lo hi)
    (construct-ugen "Wrap" (list 0) (list input lo hi) nil 1 nil)))

(define WrapIndex
  (lambda (bufnum input)
    (construct-ugen "WrapIndex" (list 1) (list bufnum input) nil 1 nil)))

(define XFade2
  (lambda (inA inB pan level)
    (construct-ugen "XFade2" (list 0 1) (list inA inB pan level) nil 1 nil)))

(define XLine
  (lambda (rt start end dur doneAction)
    (construct-ugen "XLine" rt (list start end dur doneAction) nil 1 nil)))

(define XOut
  (lambda (bus xfade input)
    (construct-ugen "XOut" (list 2) (list bus xfade) input 0 nil)))

(define ZeroCrossing
  (lambda (input)
    (construct-ugen "ZeroCrossing" (list 0) (list input) nil 1 nil)))

(define MaxLocalBufs
  (lambda (count)
    (construct-ugen "MaxLocalBufs" kr (list count) nil 1 nil)))

(define MulAdd
  (lambda (input mul add)
    (construct-ugen "MulAdd" (list 0 1 2) (list input mul add) nil 1 nil)))

(define SetBuf
  (lambda (buf offset length_ array)
    (construct-ugen "SetBuf" ir (list buf offset length_) array 1 nil)))

(define Neg
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 0)))

(define Not
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 1)))

(define IsNil
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 2)))

(define NotNil
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 3)))

(define BitNot
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 4)))

(define Abs
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 5)))

(define AsFloat
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 6)))

(define AsInt
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 7)))

(define Ceil
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 8)))

(define Floor
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 9)))

(define Frac
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 10)))

(define Sign
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 11)))

(define Squared
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 12)))

(define Cubed
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 13)))

(define Sqrt
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 14)))

(define Exp
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 15)))

(define Recip
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 16)))

(define MIDICPS
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 17)))

(define CPSMIDI
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 18)))

(define MIDIRatio
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 19)))

(define RatioMIDI
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 20)))

(define DbAmp
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 21)))

(define AmpDb
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 22)))

(define OctCPS
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 23)))

(define CPSOct
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 24)))

(define Log
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 25)))

(define Log2
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 26)))

(define Log10
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 27)))

(define Sin
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 28)))

(define Cos
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 29)))

(define Tan
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 30)))

(define ArcSin
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 31)))

(define ArcCos
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 32)))

(define ArcTan
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 33)))

(define SinH
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 34)))

(define CosH
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 35)))

(define TanH
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 36)))

(define Rand_
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 37)))

(define Rand2
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 38)))

(define LinRand_
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 39)))

(define BiLinRand
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 40)))

(define Sum3Rand
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 41)))

(define Distort
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 42)))

(define SoftClip
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 43)))

(define Coin
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 44)))

(define DigitValue
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 45)))

(define Silence
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 46)))

(define Thru
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 47)))

(define RectWindow
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 48)))

(define HanWindow
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 49)))

(define WelchWindow
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 50)))

(define TriWindow
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 51)))

(define Ramp_
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 52)))

(define SCurve
  (lambda (a)
    (construct-ugen "UnaryOpUGen" (list 0) (list a) nil 1 53)))

(define Add
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 0)))

(define Sub
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 1)))

(define Mul
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 2)))

(define IDiv
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 3)))

(define FDiv
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 4)))

(define Mod
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 5)))

(define EQ
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 6)))

(define NE
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 7)))

(define LT
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 8)))

(define GT
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 9)))

(define LE
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 10)))

(define GE
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 11)))

(define Min
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 12)))

(define Max
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 13)))

(define BitAnd
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 14)))

(define BitOr
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 15)))

(define BitXor
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 16)))

(define LCM
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 17)))

(define GCD
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 18)))

(define Round
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 19)))

(define RoundUp
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 20)))

(define Trunc
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 21)))

(define Atan2
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 22)))

(define Hypot
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 23)))

(define Hypotx
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 24)))

(define Pow
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 25)))

(define ShiftLeft
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 26)))

(define ShiftRight
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 27)))

(define UnsignedShift
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 28)))

(define Fill
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 29)))

(define Ring1
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 30)))

(define Ring2
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 31)))

(define Ring3
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 32)))

(define Ring4
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 33)))

(define DifSqr
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 34)))

(define SumSqr
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 35)))

(define SqrSum
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 36)))

(define SqrDif
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 37)))

(define AbsDif
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 38)))

(define Thresh
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 39)))

(define AMClip
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 40)))

(define ScaleNeg
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 41)))

(define Clip2
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 42)))

(define Excess
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 43)))

(define Fold2
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 44)))

(define Wrap2
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 45)))

(define FirstArg
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 46)))

(define RandRange
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 47)))

(define ExpRandRange
  (lambda (a b)
    (construct-ugen "BinaryOpUGen" (list 0 1) (list a b) nil 1 48)))

(define A2B
  (lambda (rt a b c d)
    (construct-ugen "A2B" rt (list a b c d) nil 4 nil)))

(define AY
  (lambda (rt tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype)
    (construct-ugen "AY" rt (list tonea toneb tonec noise control_ vola volb volc envfreq envstyle chiptype) nil 1 nil)))

(define Allpass1
  (lambda (rt input freq)
    (construct-ugen "Allpass1" rt (list input freq) nil 1 nil)))

(define Allpass2
  (lambda (rt input freq rq)
    (construct-ugen "Allpass2" rt (list input freq rq) nil 1 nil)))

(define AmplitudeMod
  (lambda (rt input attackTime releaseTime)
    (construct-ugen "AmplitudeMod" rt (list input attackTime releaseTime) nil 1 nil)))

(define AnalogBassDrum
  (lambda (rt trig_ infsustain accent freq tone decay_ attackfm selffm)
    (construct-ugen "AnalogBassDrum" rt (list trig_ infsustain accent freq tone decay_ attackfm selffm) nil 1 nil)))

(define AnalogPhaser
  (lambda (input lfoinput skew feedback modulation stages)
    (construct-ugen "AnalogPhaser" (list 0) (list input lfoinput skew feedback modulation stages) nil 1 nil)))

(define AnalogPhaserMod
  (lambda (input skew modulation stages)
    (construct-ugen "AnalogPhaserMod" (list 0) (list input skew modulation stages) nil 1 nil)))

(define AnalogSnareDrum
  (lambda (rt trig_ infsustain accent freq tone decay_ snappy)
    (construct-ugen "AnalogSnareDrum" rt (list trig_ infsustain accent freq tone decay_ snappy) nil 1 nil)))

(define AnalogTape
  (lambda (input bias saturation drive oversample mode)
    (construct-ugen "AnalogTape" (list 0) (list input bias saturation drive oversample mode) nil 1 nil)))

(define AnalogVintageDistortion
  (lambda (input drivegain bias lowgain highgain shelvingfreq oversample)
    (construct-ugen "AnalogVintageDistortion" (list 0) (list input drivegain bias lowgain highgain shelvingfreq oversample) nil 1 nil)))

(define AnalyseEvents2
  (lambda (rt input bufnum threshold triggerid circular pitch)
    (construct-ugen "AnalyseEvents2" rt (list input bufnum threshold triggerid circular pitch) nil 1 nil)))

(define ArneodoCoulletTresser
  (lambda (rt freq alpha h xi yi zi)
    (construct-ugen "ArneodoCoulletTresser" rt (list freq alpha h xi yi zi) nil 3 nil)))

(define ArrayMax
  (lambda (array)
    (construct-ugen "ArrayMax" (list 0) nil array 2 nil)))

(define ArrayMin
  (lambda (array)
    (construct-ugen "ArrayMin" (list 0) nil array 2 nil)))

(define AtsAmp
  (lambda (rt atsbuffer partialNum filePointer)
    (construct-ugen "AtsAmp" rt (list atsbuffer partialNum filePointer) nil 1 nil)))

(define AtsBand
  (lambda (rt atsbuffer band filePointer)
    (construct-ugen "AtsBand" rt (list atsbuffer band filePointer) nil 1 nil)))

(define AtsFreq
  (lambda (rt atsbuffer partialNum filePointer)
    (construct-ugen "AtsFreq" rt (list atsbuffer partialNum filePointer) nil 1 nil)))

(define AtsNoiSynth
  (lambda (rt atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip)
    (construct-ugen "AtsNoiSynth" rt (list atsbuffer numPartials partialStart partialSkip filePointer sinePct noisePct freqMul freqAdd numBands bandStart bandSkip) nil 1 nil)))

(define AtsNoise
  (lambda (rt atsbuffer bandNum filePointer)
    (construct-ugen "AtsNoise" rt (list atsbuffer bandNum filePointer) nil 1 nil)))

(define AtsParInfo
  (lambda (rt atsbuffer partialNum filePointer)
    (construct-ugen "AtsParInfo" rt (list atsbuffer partialNum filePointer) nil 2 nil)))

(define AtsPartial
  (lambda (rt atsbuffer partial filePointer freqMul freqAdd)
    (construct-ugen "AtsPartial" rt (list atsbuffer partial filePointer freqMul freqAdd) nil 1 nil)))

(define AtsSynth
  (lambda (rt atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd)
    (construct-ugen "AtsSynth" rt (list atsbuffer numPartials partialStart partialSkip filePointer freqMul freqAdd) nil 1 nil)))

(define AtsUGen
  (lambda (rt maxSize)
    (construct-ugen "AtsUGen" rt (list maxSize) nil 1 nil)))

(define AttackSlope
  (lambda (rt input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged)
    (construct-ugen "AttackSlope" rt (list input windowsize peakpicksize leak energythreshold sumthreshold mingap numslopesaveraged) nil 6 nil)))

(define AudioMSG
  (lambda (input index)
    (construct-ugen "AudioMSG" (list 0) (list input index) nil 1 nil)))

(define AverageOutput
  (lambda (input trig_)
    (construct-ugen "AverageOutput" (list 0) (list input trig_) nil 1 nil)))

(define B2A
  (lambda (rt w x y z)
    (construct-ugen "B2A" rt (list w x y z) nil 4 nil)))

(define B2Ster
  (lambda (rt w x y)
    (construct-ugen "B2Ster" rt (list w x y) nil 2 nil)))

(define B2UHJ
  (lambda (rt w x y)
    (construct-ugen "B2UHJ" rt (list w x y) nil 2 nil)))

(define BBlockerBuf
  (lambda (rt freq bufnum startpoint)
    (construct-ugen "BBlockerBuf" rt (list freq bufnum startpoint) nil 9 nil)))

(define BFDecode1
  (lambda (rt w x y z azimuth elevation wComp)
    (construct-ugen "BFDecode1" rt (list w x y z azimuth elevation wComp) nil 1 nil)))

(define BFDecoder
  (lambda (rt maxSize)
    (construct-ugen "BFDecoder" rt (list maxSize) nil 1 nil)))

(define BFEncode1
  (lambda (rt input azimuth elevation rho gain wComp)
    (construct-ugen "BFEncode1" rt (list input azimuth elevation rho gain wComp) nil 4 nil)))

(define BFEncode2
  (lambda (rt input point_x point_y elevation gain wComp)
    (construct-ugen "BFEncode2" rt (list input point_x point_y elevation gain wComp) nil 4 nil)))

(define BFEncodeSter
  (lambda (rt l r azimuth width elevation rho gain wComp)
    (construct-ugen "BFEncodeSter" rt (list l r azimuth width elevation rho gain wComp) nil 4 nil)))

(define BFGrainPanner
  (lambda (rt maxSize)
    (construct-ugen "BFGrainPanner" rt (list maxSize) nil 1 nil)))

(define BFManipulate
  (lambda (rt w x y z rotate tilt tumble)
    (construct-ugen "BFManipulate" rt (list w x y z rotate tilt tumble) nil 4 nil)))

(define BFPanner
  (lambda (rt maxSize)
    (construct-ugen "BFPanner" rt (list maxSize) nil 1 nil)))

(define BLBufRd
  (lambda (rt bufnum phase ratio)
    (construct-ugen "BLBufRd" rt (list bufnum phase ratio) nil 1 nil)))

(define BLOsc
  (lambda (rt freq pulsewidth waveform)
    (construct-ugen "BLOsc" rt (list freq pulsewidth waveform) nil 1 nil)))

(define BMoog
  (lambda (input freq q mode saturation)
    (construct-ugen "BMoog" (list 0) (list input freq q mode saturation) nil 1 nil)))

(define Balance
  (lambda (rt input test hp stor)
    (construct-ugen "Balance" rt (list input test hp stor) nil 1 nil)))

(define BeatStatistics
  (lambda (rt fft leak numpreviousbeats)
    (construct-ugen "BeatStatistics" rt (list fft leak numpreviousbeats) nil 4 nil)))

(define BinData
  (lambda (rt buffer bin overlaps)
    (construct-ugen "BinData" rt (list buffer bin overlaps) nil 2 nil)))

(define BlitB3
  (lambda (rt freq)
    (construct-ugen "BlitB3" rt (list freq) nil 1 nil)))

(define BlitB3D
  (lambda (rt freq)
    (construct-ugen "BlitB3D" rt (list freq) nil 1 nil)))

(define BlitB3Saw
  (lambda (rt freq leak)
    (construct-ugen "BlitB3Saw" rt (list freq leak) nil 1 nil)))

(define BlitB3Square
  (lambda (rt freq leak)
    (construct-ugen "BlitB3Square" rt (list freq leak) nil 1 nil)))

(define BlitB3Tri
  (lambda (rt freq leak leak2)
    (construct-ugen "BlitB3Tri" rt (list freq leak leak2) nil 1 nil)))

(define Breakcore
  (lambda (rt bufnum capturein capturetrigger duration ampdropout)
    (construct-ugen "Breakcore" rt (list bufnum capturein capturetrigger duration ampdropout) nil 1 nil)))

(define Brusselator
  (lambda (rt reset rate_ mu gamma initx inity)
    (construct-ugen "Brusselator" rt (list reset rate_ mu gamma initx inity) nil 2 nil)))

(define BufGrain
  (lambda (rt trigger dur sndbuf rate_ pos interp)
    (construct-ugen "BufGrain" rt (list trigger dur sndbuf rate_ pos interp) nil 1 nil)))

(define BufGrainB
  (lambda (rt trigger dur sndbuf rate_ pos envbuf interp)
    (construct-ugen "BufGrainB" rt (list trigger dur sndbuf rate_ pos envbuf interp) nil 1 nil)))

(define BufGrainBBF
  (lambda (rt trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp)
    (construct-ugen "BufGrainBBF" rt (list trigger dur sndbuf rate_ pos envbuf azimuth elevation rho interp wComp) nil 4 nil)))

(define BufGrainBF
  (lambda (rt trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp)
    (construct-ugen "BufGrainBF" rt (list trigger dur sndbuf rate_ pos azimuth elevation rho interp wComp) nil 4 nil)))

(define BufGrainI
  (lambda (rt trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp)
    (construct-ugen "BufGrainI" rt (list trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac interp) nil 1 nil)))

(define BufGrainIBF
  (lambda (rt trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp)
    (construct-ugen "BufGrainIBF" rt (list trigger dur sndbuf rate_ pos envbuf1 envbuf2 ifac azimuth elevation rho interp wComp) nil 4 nil)))

(define BufMax
  (lambda (rt bufnum gate_)
    (construct-ugen "BufMax" rt (list bufnum gate_) nil 2 nil)))

(define BufMin
  (lambda (rt bufnum gate_)
    (construct-ugen "BufMin" rt (list bufnum gate_) nil 2 nil)))

(define CQ_Diff
  (lambda (rt in1 in2 databufnum)
    (construct-ugen "CQ_Diff" rt (list in1 in2 databufnum) nil 1 nil)))

(define Cepstrum
  (lambda (rt cepbuf fftchain)
    (construct-ugen "Cepstrum" rt (list cepbuf fftchain) nil 1 nil)))

(define Chen
  (lambda (rt speed a b c)
    (construct-ugen "Chen" rt (list speed a b c) nil 3 nil)))

(define Chromagram
  (lambda (rt fft fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize)
    (construct-ugen "Chromagram" rt (list fft fftsize n tuningbase octaves integrationflag coeff octaveratio perframenormalize) nil 12 nil)))

(define CircleRamp
  (lambda (rt input lagTime circmin circmax)
    (construct-ugen "CircleRamp" rt (list input lagTime circmin circmax) nil 1 nil)))

(define Clipper32
  (lambda (rt input lo hi)
    (construct-ugen "Clipper32" rt (list input lo hi) nil 1 nil)))

(define Clipper4
  (lambda (rt input lo hi)
    (construct-ugen "Clipper4" rt (list input lo hi) nil 1 nil)))

(define Clipper8
  (lambda (rt input lo hi)
    (construct-ugen "Clipper8" rt (list input lo hi) nil 1 nil)))

(define Clockmus
  (lambda (rt)
    (construct-ugen "Clockmus" rt nil nil 1 nil)))

(define CombLP
  (lambda (rt input gate_ maxdelaytime delaytime decaytime coef)
    (construct-ugen "CombLP" rt (list input gate_ maxdelaytime delaytime decaytime coef) nil 1 nil)))

(define ComplexRes
  (lambda (input freq decay_)
    (construct-ugen "ComplexRes" (list 0) (list input freq decay_) nil 1 nil)))

(define Concat
  (lambda (rt control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore)
    (construct-ugen "Concat" rt (list control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore) nil 1 nil)))

(define Concat2
  (lambda (rt control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold)
    (construct-ugen "Concat2" rt (list control_ source storesize seektime seekdur matchlength freezestore zcr lms sc st randscore threshold) nil 1 nil)))

(define Coyote
  (lambda (rt input trackFall slowLag fastLag fastMul thresh minDur)
    (construct-ugen "Coyote" rt (list input trackFall slowLag fastLag fastMul thresh minDur) nil 1 nil)))

(define Crest
  (lambda (rt input numsamps gate_)
    (construct-ugen "Crest" rt (list input numsamps gate_) nil 1 nil)))

(define CrossoverDistortion
  (lambda (input amp smooth)
    (construct-ugen "CrossoverDistortion" (list 0) (list input amp smooth) nil 1 nil)))

(define DCompressor
  (lambda (input sidechainIn sidechain ratio threshold attack release makeup automakeup)
    (construct-ugen "DCompressor" (list 0) (list input sidechainIn sidechain ratio threshold attack release makeup automakeup) nil 1 nil)))

(define DFM1
  (lambda (input freq res inputgain type_ noiselevel)
    (construct-ugen "DFM1" (list 0) (list input freq res inputgain type_ noiselevel) nil 1 nil)))

(define DNoiseRing
  (lambda (change chance shift numBits resetval)
    (construct-ugen "DNoiseRing" dr (list change chance shift numBits resetval) nil 1 nil)))

(define DPW3Tri
  (lambda (rt freq)
    (construct-ugen "DPW3Tri" rt (list freq) nil 1 nil)))

(define DPW4Saw
  (lambda (rt freq)
    (construct-ugen "DPW4Saw" rt (list freq) nil 1 nil)))

(define DWGBowed
  (lambda (rt freq velb force gate_ pos release c1 c3 impZ fB)
    (construct-ugen "DWGBowed" rt (list freq velb force gate_ pos release c1 c3 impZ fB) nil 1 nil)))

(define DWGBowedSimple
  (lambda (rt freq velb force gate_ pos release c1 c3)
    (construct-ugen "DWGBowedSimple" rt (list freq velb force gate_ pos release c1 c3) nil 1 nil)))

(define DWGBowedTor
  (lambda (rt freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor)
    (construct-ugen "DWGBowedTor" rt (list freq velb force gate_ pos release c1 c3 impZ fB mistune c1tor c3tor iZtor) nil 1 nil)))

(define DWGClarinet3
  (lambda (rt freq pm pc m gate_ release c1 c3)
    (construct-ugen "DWGClarinet3" rt (list freq pm pc m gate_ release c1 c3) nil 1 nil)))

(define DWGFlute
  (lambda (rt freq pm endr jetr jetRa gate_ release)
    (construct-ugen "DWGFlute" rt (list freq pm endr jetr jetRa gate_ release) nil 1 nil)))

(define DWGPlucked
  (lambda (rt freq amp gate_ pos c1 c3 inp release)
    (construct-ugen "DWGPlucked" rt (list freq amp gate_ pos c1 c3 inp release) nil 1 nil)))

(define DWGPlucked2
  (lambda (rt freq amp gate_ pos c1 c3 inp release mistune mp gc)
    (construct-ugen "DWGPlucked2" rt (list freq amp gate_ pos c1 c3 inp release mistune mp gc) nil 1 nil)))

(define DWGPluckedStiff
  (lambda (rt freq amp gate_ pos c1 c3 inp release fB)
    (construct-ugen "DWGPluckedStiff" rt (list freq amp gate_ pos c1 c3 inp release fB) nil 1 nil)))

(define DWGSoundBoard
  (lambda (inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8)
    (construct-ugen "DWGSoundBoard" (list 0) (list inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8) nil 1 nil)))

(define Dbrown2
  (lambda (rt lo hi step dist length_)
    (construct-ugen "Dbrown2" rt (list lo hi step dist length_) nil 1 nil)))

(define DbufTag
  (lambda (bufnum v axiom rules recycle mode)
    (construct-ugen "DbufTag" dr (list bufnum v axiom rules recycle mode) nil 1 nil)))

(define Decimator
  (lambda (rt input rate_ bits)
    (construct-ugen "Decimator" rt (list input rate_ bits) nil 1 nil)))

(define DetaBlockerBuf
  (lambda (bufnum startpoint)
    (construct-ugen "DetaBlockerBuf" dr (list bufnum startpoint) nil 1 nil)))

(define Dfsm
  (lambda (rules n rgen)
    (construct-ugen "Dfsm" dr (list rules n rgen) nil 1 nil)))

(define Dgauss
  (lambda (rt lo hi length_)
    (construct-ugen "Dgauss" rt (list lo hi length_) nil 1 nil)))

(define DiodeRingMod
  (lambda (car mod_)
    (construct-ugen "DiodeRingMod" (list 0) (list car mod_) nil 1 nil)))

(define Disintegrator
  (lambda (input probability multiplier)
    (construct-ugen "Disintegrator" (list 0) (list input probability multiplier) nil 1 nil)))

(define Dneuromodule
  (lambda (nc dt theta x weights)
    (construct-ugen "Dneuromodule" dr (list dt) theta x weights nc nil)))

(define DoubleNestedAllpassC
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3)
    (construct-ugen "DoubleNestedAllpassC" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) nil 1 nil)))

(define DoubleNestedAllpassL
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3)
    (construct-ugen "DoubleNestedAllpassL" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) nil 1 nil)))

(define DoubleNestedAllpassN
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3)
    (construct-ugen "DoubleNestedAllpassN" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) nil 1 nil)))

(define DoubleWell
  (lambda (rt reset ratex ratey f w delta initx inity)
    (construct-ugen "DoubleWell" rt (list reset ratex ratey f w delta initx inity) nil 1 nil)))

(define DoubleWell2
  (lambda (rt reset ratex ratey f w delta initx inity)
    (construct-ugen "DoubleWell2" rt (list reset ratex ratey f w delta initx inity) nil 1 nil)))

(define DoubleWell3
  (lambda (rt reset rate_ f delta initx inity)
    (construct-ugen "DoubleWell3" rt (list reset rate_ f delta initx inity) nil 1 nil)))

(define DriveNoise
  (lambda (rt input amount multi)
    (construct-ugen "DriveNoise" rt (list input amount multi) nil 1 nil)))

(define DrumTrack
  (lambda (rt input lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode)
    (construct-ugen "DrumTrack" rt (list input lock dynleak tempowt phasewt basswt patternwt prior kicksensitivity snaresensitivity debugmode) nil 4 nil)))

(define Dtag
  (lambda (rt bufsize v axiom rules recycle mode)
    (construct-ugen "Dtag" rt (list bufsize v axiom rules recycle mode) nil 1 nil)))

(define RDustR
  (lambda (rt iot_min iot_max)
    (construct-ugen "RDustR" rt (list iot_min iot_max) nil 1 nil)))

(define EnvDetect
  (lambda (rt input attack release)
    (construct-ugen "EnvDetect" rt (list input attack release) nil 1 nil)))

(define EnvFollow
  (lambda (rt input decaycoeff)
    (construct-ugen "EnvFollow" rt (list input decaycoeff) nil 1 nil)))

(define RExpRandN
  (lambda (numChannels lo hi)
    (construct-ugen "RExpRandN" ir (list lo hi) nil numChannels nil)))

(define FFTComplexDev
  (lambda (rt buffer rectify powthresh)
    (construct-ugen "FFTComplexDev" rt (list buffer rectify powthresh) nil 1 nil)))

(define FFTCrest
  (lambda (rt buffer freqlo freqhi)
    (construct-ugen "FFTCrest" rt (list buffer freqlo freqhi) nil 1 nil)))

(define FFTDiffMags
  (lambda (rt bufferA bufferB)
    (construct-ugen "FFTDiffMags" rt (list bufferA bufferB) nil 1 nil)))

(define FFTFlux
  (lambda (rt buffer normalise)
    (construct-ugen "FFTFlux" rt (list buffer normalise) nil 1 nil)))

(define FFTFluxPos
  (lambda (rt buffer normalise)
    (construct-ugen "FFTFluxPos" rt (list buffer normalise) nil 1 nil)))

(define FFTMKL
  (lambda (rt buffer epsilon)
    (construct-ugen "FFTMKL" rt (list buffer epsilon) nil 1 nil)))

(define FFTPeak
  (lambda (rt buffer freqlo freqhi)
    (construct-ugen "FFTPeak" rt (list buffer freqlo freqhi) nil 2 nil)))

(define FFTPhaseDev
  (lambda (rt buffer weight powthresh)
    (construct-ugen "FFTPhaseDev" rt (list buffer weight powthresh) nil 1 nil)))

(define FFTPower
  (lambda (rt buffer square)
    (construct-ugen "FFTPower" rt (list buffer square) nil 1 nil)))

(define FFTSlope
  (lambda (rt buffer)
    (construct-ugen "FFTSlope" rt (list buffer) nil 1 nil)))

(define FFTSpread
  (lambda (rt buffer centroid)
    (construct-ugen "FFTSpread" rt (list buffer centroid) nil 1 nil)))

(define FFTSubbandFlatness
  (lambda (rt chain cutfreqs)
    (construct-ugen "FFTSubbandFlatness" rt (list chain cutfreqs) nil 1 nil)))

(define FFTSubbandFlux
  (lambda (rt chain cutfreqs posonly)
    (construct-ugen "FFTSubbandFlux" rt (list chain cutfreqs posonly) nil 1 nil)))

(define FFTSubbandPower
  (lambda (rt chain cutfreqs square scalemode)
    (construct-ugen "FFTSubbandPower" rt (list chain cutfreqs square scalemode) nil 1 nil)))

(define FM7
  (lambda (rt ctlMatrix modMatrix)
    (construct-ugen "FM7" rt nil ctlMatrix modMatrix 6 nil)))

(define FMGrain
  (lambda (trigger dur carfreq modfreq index)
    (construct-ugen "FMGrain" (list 0) (list trigger dur carfreq modfreq index) nil 1 nil)))

(define FMGrainB
  (lambda (trigger dur carfreq modfreq index envbuf)
    (construct-ugen "FMGrainB" (list 0) (list trigger dur carfreq modfreq index envbuf) nil 1 nil)))

(define FMGrainBBF
  (lambda (rt trigger dur carfreq modfreq index envbuf azimuth elevation rho wComp)
    (construct-ugen "FMGrainBBF" rt (list trigger dur carfreq modfreq index envbuf azimuth elevation rho wComp) nil 4 nil)))

(define FMGrainBF
  (lambda (rt trigger dur carfreq modfreq index azimuth elevation rho wComp)
    (construct-ugen "FMGrainBF" rt (list trigger dur carfreq modfreq index azimuth elevation rho wComp) nil 4 nil)))

(define FMGrainI
  (lambda (rt trigger dur carfreq modfreq index envbuf1 envbuf2 ifac)
    (construct-ugen "FMGrainI" rt (list trigger dur carfreq modfreq index envbuf1 envbuf2 ifac) nil 1 nil)))

(define FMGrainIBF
  (lambda (rt trigger dur carfreq modfreq index envbuf1 envbuf2 ifac azimuth elevation rho wComp)
    (construct-ugen "FMGrainIBF" rt (list trigger dur carfreq modfreq index envbuf1 envbuf2 ifac azimuth elevation rho wComp) nil 4 nil)))

(define FMHDecode1
  (lambda (rt w x y z r s t u v azimuth elevation)
    (construct-ugen "FMHDecode1" rt (list w x y z r s t u v azimuth elevation) nil 1 nil)))

(define FMHEncode0
  (lambda (rt input azimuth elevation gain)
    (construct-ugen "FMHEncode0" rt (list input azimuth elevation gain) nil 9 nil)))

(define FMHEncode1
  (lambda (rt input azimuth elevation rho gain wComp)
    (construct-ugen "FMHEncode1" rt (list input azimuth elevation rho gain wComp) nil 9 nil)))

(define FMHEncode2
  (lambda (rt input point_x point_y elevation gain wComp)
    (construct-ugen "FMHEncode2" rt (list input point_x point_y elevation gain wComp) nil 9 nil)))

(define FeatureSave
  (lambda (rt features trig_)
    (construct-ugen "FeatureSave" rt (list features trig_) nil 1 nil)))

(define Fhn2DC
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (construct-ugen "Fhn2DC" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil)))

(define Fhn2DL
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (construct-ugen "Fhn2DL" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil)))

(define Fhn2DN
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (construct-ugen "Fhn2DN" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil)))

(define FhnTrig
  (lambda (rt minfreq maxfreq urate wrate b0 b1 i u0 w0)
    (construct-ugen "FhnTrig" rt (list minfreq maxfreq urate wrate b0 b1 i u0 w0) nil 1 nil)))

(define FincoSprottL
  (lambda (rt freq a h xi yi zi)
    (construct-ugen "FincoSprottL" rt (list freq a h xi yi zi) nil 3 nil)))

(define FincoSprottM
  (lambda (rt freq a b h xi yi zi)
    (construct-ugen "FincoSprottM" rt (list freq a b h xi yi zi) nil 3 nil)))

(define FincoSprottS
  (lambda (rt freq a b h xi yi zi)
    (construct-ugen "FincoSprottS" rt (list freq a b h xi yi zi) nil 3 nil)))

(define FitzHughNagumo
  (lambda (rt reset rateu ratew b0 b1 initu initw)
    (construct-ugen "FitzHughNagumo" rt (list reset rateu ratew b0 b1 initu initw) nil 1 nil)))

(define FrameCompare
  (lambda (rt buffer1 buffer2 wAmount)
    (construct-ugen "FrameCompare" rt (list buffer1 buffer2 wAmount) nil 1 nil)))

(define Friction
  (lambda (rt input friction spring damp mass beltmass)
    (construct-ugen "Friction" rt (list input friction spring damp mass beltmass) nil 1 nil)))

(define Gammatone
  (lambda (input centrefrequency bandwidth)
    (construct-ugen "Gammatone" (list 0) (list input centrefrequency bandwidth) nil 1 nil)))

(define GaussClass
  (lambda (rt input bufnum gate_)
    (construct-ugen "GaussClass" rt (list input bufnum gate_) nil 1 nil)))

(define GaussTrig
  (lambda (rt freq dev)
    (construct-ugen "GaussTrig" rt (list freq dev) nil 1 nil)))

(define Gbman2DC
  (lambda (rt minfreq maxfreq x0 y0)
    (construct-ugen "Gbman2DC" rt (list minfreq maxfreq x0 y0) nil 1 nil)))

(define Gbman2DL
  (lambda (rt minfreq maxfreq x0 y0)
    (construct-ugen "Gbman2DL" rt (list minfreq maxfreq x0 y0) nil 1 nil)))

(define Gbman2DN
  (lambda (rt minfreq maxfreq x0 y0)
    (construct-ugen "Gbman2DN" rt (list minfreq maxfreq x0 y0) nil 1 nil)))

(define GbmanTrig
  (lambda (rt minfreq maxfreq x0 y0)
    (construct-ugen "GbmanTrig" rt (list minfreq maxfreq x0 y0) nil 1 nil)))

(define Gendy4
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (construct-ugen "Gendy4" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil)))

(define Gendy5
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (construct-ugen "Gendy5" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) nil 1 nil)))

(define Getenv
  (lambda (rt key defaultval)
    (construct-ugen "Getenv" rt (list key defaultval) nil 1 nil)))

(define GlitchBPF
  (lambda (rt input freq rq)
    (construct-ugen "GlitchBPF" rt (list input freq rq) nil 1 nil)))

(define GlitchBRF
  (lambda (rt input freq rq)
    (construct-ugen "GlitchBRF" rt (list input freq rq) nil 1 nil)))

(define GlitchHPF
  (lambda (rt input freq)
    (construct-ugen "GlitchHPF" rt (list input freq) nil 1 nil)))

(define GlitchRHPF
  (lambda (rt input freq rq)
    (construct-ugen "GlitchRHPF" rt (list input freq rq) nil 1 nil)))

(define Goertzel
  (lambda (rt input bufsize freq hop)
    (construct-ugen "Goertzel" rt (list input bufsize freq hop) nil 2 nil)))

(define GrainBufJ
  (lambda (rt numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains)
    (construct-ugen "GrainBufJ" rt (list numChannels trigger dur sndbuf rate_ pos loop interp grainAmp pan envbufnum maxGrains) nil 1 nil)))

(define GrainFMJ
  (lambda (rt numChannels trigger dur carfreq modfreq index grainAmp pan envbufnum maxGrains)
    (construct-ugen "GrainFMJ" rt (list numChannels trigger dur carfreq modfreq index grainAmp pan envbufnum maxGrains) nil 1 nil)))

(define GrainInJ
  (lambda (rt numChannels trigger dur input grainAmp pan envbufnum maxGrains)
    (construct-ugen "GrainInJ" rt (list numChannels trigger dur input grainAmp pan envbufnum maxGrains) nil 1 nil)))

(define GrainSinJ
  (lambda (rt numChannels trigger dur freq grainAmp pan envbufnum maxGrains)
    (construct-ugen "GrainSinJ" rt (list numChannels trigger dur freq grainAmp pan envbufnum maxGrains) nil 1 nil)))

(define GravityGrid
  (lambda (rt reset rate_ newx newy bufnum)
    (construct-ugen "GravityGrid" rt (list reset rate_ newx newy bufnum) nil 1 nil)))

(define GravityGrid2
  (lambda (rt reset rate_ newx newy bufnum)
    (construct-ugen "GravityGrid2" rt (list reset rate_ newx newy bufnum) nil 1 nil)))

(define GreyholeRaw
  (lambda (in1 in2 damping delaytime diffusion feedback moddepth modfreq size)
    (construct-ugen "GreyholeRaw" (list 0 1) (list in1 in2 damping delaytime diffusion feedback moddepth modfreq size) nil 2 nil)))

(define HairCell
  (lambda (input spontaneousrate boostrate restorerate loss)
    (construct-ugen "HairCell" (list 0) (list input spontaneousrate boostrate restorerate loss) nil 1 nil)))

(define HarmonicOsc
  (lambda (rt freq firstharmonic amplitudes)
    (construct-ugen "HarmonicOsc" rt (list freq firstharmonic) amplitudes 1 nil)))

(define Henon2DC
  (lambda (rt minfreq maxfreq a b x0 y0)
    (construct-ugen "Henon2DC" rt (list minfreq maxfreq a b x0 y0) nil 1 nil)))

(define Henon2DL
  (lambda (rt minfreq maxfreq a b x0 y0)
    (construct-ugen "Henon2DL" rt (list minfreq maxfreq a b x0 y0) nil 1 nil)))

(define Henon2DN
  (lambda (rt minfreq maxfreq a b x0 y0)
    (construct-ugen "Henon2DN" rt (list minfreq maxfreq a b x0 y0) nil 1 nil)))

(define HenonTrig
  (lambda (rt minfreq maxfreq a b x0 y0)
    (construct-ugen "HenonTrig" rt (list minfreq maxfreq a b x0 y0) nil 1 nil)))

(define ICepstrum
  (lambda (rt cepchain fftbuf)
    (construct-ugen "ICepstrum" rt (list cepchain fftbuf) nil 1 nil)))

(define IIRFilter
  (lambda (input freq rq)
    (construct-ugen "IIRFilter" (list 0) (list input freq rq) nil 1 nil)))

(define InGrain
  (lambda (rt trigger dur input)
    (construct-ugen "InGrain" rt (list trigger dur input) nil 1 nil)))

(define InGrainB
  (lambda (rt trigger dur input envbuf)
    (construct-ugen "InGrainB" rt (list trigger dur input envbuf) nil 1 nil)))

(define InGrainBBF
  (lambda (rt trigger dur input envbuf azimuth elevation rho wComp)
    (construct-ugen "InGrainBBF" rt (list trigger dur input envbuf azimuth elevation rho wComp) nil 4 nil)))

(define InGrainBF
  (lambda (rt trigger dur input azimuth elevation rho wComp)
    (construct-ugen "InGrainBF" rt (list trigger dur input azimuth elevation rho wComp) nil 4 nil)))

(define InGrainI
  (lambda (rt trigger dur input envbuf1 envbuf2 ifac)
    (construct-ugen "InGrainI" rt (list trigger dur input envbuf1 envbuf2 ifac) nil 1 nil)))

(define InGrainIBF
  (lambda (rt trigger dur input envbuf1 envbuf2 ifac azimuth elevation rho wComp)
    (construct-ugen "InGrainIBF" rt (list trigger dur input envbuf1 envbuf2 ifac azimuth elevation rho wComp) nil 4 nil)))

(define InsideOut
  (lambda (rt input)
    (construct-ugen "InsideOut" rt (list input) nil 1 nil)))

(define Instruction
  (lambda (rt bufnum)
    (construct-ugen "Instruction" rt (list bufnum) nil 1 nil)))

(define JPverbRaw
  (lambda (in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60)
    (construct-ugen "JPverbRaw" (list 0) (list in1 in2 damp earlydiff highband highx lowband lowx mdepth mfreq midx size t60) nil 2 nil)))

(define KMeansRT
  (lambda (rt bufnum inputdata k gate_ reset learn)
    (construct-ugen "KMeansRT" rt (list bufnum inputdata k gate_ reset learn) nil 1 nil)))

(define KeyClarity
  (lambda (rt chain keydecay chromaleak)
    (construct-ugen "KeyClarity" rt (list chain keydecay chromaleak) nil 1 nil)))

(define KeyMode
  (lambda (rt chain keydecay chromaleak)
    (construct-ugen "KeyMode" rt (list chain keydecay chromaleak) nil 1 nil)))

(define KmeansToBPSet1
  (lambda (rt freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum)
    (construct-ugen "KmeansToBPSet1" rt (list freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft bufnum) nil 1 nil)))

(define LADSPA
  (lambda (rt nChans id_ args)
    (construct-ugen "LADSPA" rt (list nChans id_ args) nil 0 nil)))

(define LFBrownNoise0
  (lambda (rt freq dev dist)
    (construct-ugen "LFBrownNoise0" rt (list freq dev dist) nil 1 nil)))

(define LFBrownNoise1
  (lambda (rt freq dev dist)
    (construct-ugen "LFBrownNoise1" rt (list freq dev dist) nil 1 nil)))

(define LFBrownNoise2
  (lambda (rt freq dev dist)
    (construct-ugen "LFBrownNoise2" rt (list freq dev dist) nil 1 nil)))

(define LPCAnalyzer
  (lambda (input source n p testE delta windowtype)
    (construct-ugen "LPCAnalyzer" (list 0 1) (list input source n p testE delta windowtype) nil 1 nil)))

(define LPCError
  (lambda (rt input p)
    (construct-ugen "LPCError" rt (list input p) nil 1 nil)))

(define LPCSynth
  (lambda (buffer signal pointer)
    (construct-ugen "LPCSynth" ar (list buffer signal pointer) nil 1 nil)))

(define LPCVals
  (lambda (buffer pointer)
    (construct-ugen "LPCVals" ar (list buffer pointer) nil 3 nil)))

(define LPF1
  (lambda (rt input freq)
    (construct-ugen "LPF1" rt (list input freq) nil 1 nil)))

(define LPF18
  (lambda (rt input freq res dist)
    (construct-ugen "LPF18" rt (list input freq res dist) nil 1 nil)))

(define LPFVS6
  (lambda (rt input freq slope)
    (construct-ugen "LPFVS6" rt (list input freq slope) nil 1 nil)))

(define LPG
  (lambda (input controlinput controloffset controlscale vca resonance lowpassmode linearity)
    (construct-ugen "LPG" (list 0) (list input controlinput controloffset controlscale vca resonance lowpassmode linearity) nil 1 nil)))

(define LTI
  (lambda (rt input bufnuma bufnumb)
    (construct-ugen "LTI" rt (list input bufnuma bufnumb) nil 1 nil)))

(define Latoocarfian2DC
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (construct-ugen "Latoocarfian2DC" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil)))

(define Latoocarfian2DL
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (construct-ugen "Latoocarfian2DL" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil)))

(define Latoocarfian2DN
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (construct-ugen "Latoocarfian2DN" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil)))

(define LatoocarfianTrig
  (lambda (rt minfreq maxfreq a b c d x0 y0)
    (construct-ugen "LatoocarfianTrig" rt (list minfreq maxfreq a b c d x0 y0) nil 1 nil)))

(define ListTrig
  (lambda (rt bufnum reset offset numframes)
    (construct-ugen "ListTrig" rt (list bufnum reset offset numframes) nil 1 nil)))

(define ListTrig2
  (lambda (rt bufnum reset numframes)
    (construct-ugen "ListTrig2" rt (list bufnum reset numframes) nil 1 nil)))

(define Logger
  (lambda (rt inputArray trig_ bufnum reset)
    (construct-ugen "Logger" rt (list inputArray trig_ bufnum reset) nil 1 nil)))

(define LoopBuf
  (lambda (nc rt bufnum rate_ gate_ startPos startLoop endLoop interpolation)
    (construct-ugen "LoopBuf" rt (list bufnum rate_ gate_ startPos startLoop endLoop interpolation) nil nc nil)))

(define Lorenz2DC
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (construct-ugen "Lorenz2DC" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil)))

(define Lorenz2DL
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (construct-ugen "Lorenz2DL" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil)))

(define Lorenz2DN
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (construct-ugen "Lorenz2DN" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil)))

(define LorenzTrig
  (lambda (rt minfreq maxfreq s r b h x0 y0 z0)
    (construct-ugen "LorenzTrig" rt (list minfreq maxfreq s r b h x0 y0 z0) nil 1 nil)))

(define Lores
  (lambda (input freq res)
    (construct-ugen "Lores" (list 0) (list input freq res) nil 1 nil)))

(define LotkaVolterra
  (lambda (rt freq a b c d h xi yi)
    (construct-ugen "LotkaVolterra" rt (list freq a b c d h xi yi) nil 2 nil)))

(define MCLDChaosGen
  (lambda (rt maxSize)
    (construct-ugen "MCLDChaosGen" rt (list maxSize) nil 1 nil)))

(define MarkovSynth
  (lambda (rt input isRecording waitTime tableSize)
    (construct-ugen "MarkovSynth" rt (list input isRecording waitTime tableSize) nil 1 nil)))

(define MatchingP
  (lambda (rt dict input dictsize ntofind hop method)
    (construct-ugen "MatchingP" rt (list dict input dictsize ntofind hop method) nil 4 nil)))

(define MatchingPResynth
  (lambda (rt dict method trigger residual activs)
    (construct-ugen "MatchingPResynth" rt (list dict method trigger residual activs) nil 1 nil)))

(define Maxamp
  (lambda (rt input numSamps)
    (construct-ugen "Maxamp" rt (list input numSamps) nil 1 nil)))

(define MdaPiano
  (lambda (rt freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain)
    (construct-ugen "MdaPiano" rt (list freq gate_ vel decay_ release hard velhard muffle velmuff velcurve stereo tune random stretch sustain) nil 2 nil)))

(define MeanTriggered
  (lambda (rt input trig_ length_)
    (construct-ugen "MeanTriggered" rt (list input trig_ length_) nil 1 nil)))

(define Meddis
  (lambda (input)
    (construct-ugen "Meddis" (list 0) (list input) nil 1 nil)))

(define MedianSeparation
  (lambda (rt fft fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax)
    (construct-ugen "MedianSeparation" rt (list fft fftharmonic fftpercussive fftsize mediansize hardorsoft p medianormax) nil 2 nil)))

(define MedianTriggered
  (lambda (rt input trig_ length_)
    (construct-ugen "MedianTriggered" rt (list input trig_ length_) nil 1 nil)))

(define MembraneCircle
  (lambda (rt excitation tension loss)
    (construct-ugen "MembraneCircle" rt (list excitation tension loss) nil 1 nil)))

(define MembraneHexagon
  (lambda (rt excitation tension loss)
    (construct-ugen "MembraneHexagon" rt (list excitation tension loss) nil 1 nil)))

(define Metro
  (lambda (rt bpm numBeats)
    (construct-ugen "Metro" rt (list bpm numBeats) nil 1 nil)))

(define MiBraids
  (lambda (rt pitch timbre color model trig_ resamp decim bits ws)
    (construct-ugen "MiBraids" rt (list pitch timbre color model trig_ resamp decim bits ws) nil 1 nil)))

(define MiClouds
  (lambda (rt pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_ inputArray)
    (construct-ugen "MiClouds" rt (list pit pos size dens tex drywet in_gain spread rvb fb freeze mode lofi trig_) inputArray 2 nil)))

(define MiElements
  (lambda (rt blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg)
    (construct-ugen "MiElements" rt (list blow_in strike_in gate_ pit strength contour bow_level blow_level strike_level flow mallet bow_timb blow_timb strike_timb geom bright damp pos space model easteregg) nil 2 nil)))

(define MiMu
  (lambda (rt input gain bypass)
    (construct-ugen "MiMu" rt (list input gain bypass) nil 1 nil)))

(define MiOmi
  (lambda (rt audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate space)
    (construct-ugen "MiOmi" rt (list audio_in gate_ pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter_mode cutoff reson strength env rotate space) nil 2 nil)))

(define MiPlaits
  (lambda (rt pitch engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour)
    (construct-ugen "MiPlaits" rt (list pitch engine harm timbre morph trigger level fm_mod timb_mod morph_mod decay_ lpg_colour) nil 2 nil)))

(define MiRings
  (lambda (rt input trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass)
    (construct-ugen "MiRings" rt (list input trig_ pit struct bright damp pos model poly intern_exciter easteregg bypass) nil 2 nil)))

(define MiRipples
  (lambda (input cf reson drive)
    (construct-ugen "MiRipples" (list 0) (list input cf reson drive) nil 1 nil)))

(define MiTides
  (lambda (rt freq shape slope smooth shift trig_ clock output_mode ramp_mode ratio rate_)
    (construct-ugen "MiTides" rt (list freq shape slope smooth shift trig_ clock output_mode ramp_mode ratio rate_) nil 4 nil)))

(define MiVerb
  (lambda (time drywet damp hp freeze diff inputArray)
    (construct-ugen "MiVerb" (list 6) (list time drywet damp hp freeze diff) inputArray 2 nil)))

(define MiWarps
  (lambda (rt carrier modulator lev1 lev2 algo timb osc pit easteregg)
    (construct-ugen "MiWarps" rt (list carrier modulator lev1 lev2 algo timb osc pit easteregg) nil 2 nil)))

(define MonoGrain
  (lambda (rt input winsize grainrate winrandpct)
    (construct-ugen "MonoGrain" rt (list input winsize grainrate winrandpct) nil 1 nil)))

(define MonoGrainBF
  (lambda (rt input winsize grainrate winrandpct azimuth azrand elevation elrand rho)
    (construct-ugen "MonoGrainBF" rt (list input winsize grainrate winrandpct azimuth azrand elevation elrand rho) nil 4 nil)))

(define MoogLadder
  (lambda (input ffreq res)
    (construct-ugen "MoogLadder" (list 0) (list input ffreq res) nil 1 nil)))

(define MoogVCF
  (lambda (input fco res)
    (construct-ugen "MoogVCF" (list 0) (list input fco res) nil 1 nil)))

(define NL
  (lambda (rt input bufnuma bufnumb guard1 guard2)
    (construct-ugen "NL" rt (list input bufnuma bufnumb guard1 guard2) nil 1 nil)))

(define NL2
  (lambda (rt input bufnum maxsizea maxsizeb guard1 guard2)
    (construct-ugen "NL2" rt (list input bufnum maxsizea maxsizeb guard1 guard2) nil 1 nil)))

(define NLFiltC
  (lambda (rt input a b d c l)
    (construct-ugen "NLFiltC" rt (list input a b d c l) nil 1 nil)))

(define NLFiltL
  (lambda (rt input a b d c l)
    (construct-ugen "NLFiltL" rt (list input a b d c l) nil 1 nil)))

(define NLFiltN
  (lambda (rt input a b d c l)
    (construct-ugen "NLFiltN" rt (list input a b d c l) nil 1 nil)))

(define NTube
  (lambda (rt input lossarray karray delaylengtharray)
    (construct-ugen "NTube" rt (list input lossarray karray delaylengtharray) nil 1 nil)))

(define NearestN
  (lambda (rt treebuf input gate_ num)
    (construct-ugen "NearestN" rt (list treebuf input gate_ num) nil 3 nil)))

(define NeedleRect
  (lambda (rt rate_ imgWidth imgHeight rectX rectY rectW rectH)
    (construct-ugen "NeedleRect" rt (list rate_ imgWidth imgHeight rectX rectY rectW rectH) nil 1 nil)))

(define NeoFormant
  (lambda (rt formantfreq carrierfreq phaseshift)
    (construct-ugen "NeoFormant" rt (list formantfreq carrierfreq phaseshift) nil 1 nil)))

(define NeoVarSawOsc
  (lambda (rt freq pw waveshape)
    (construct-ugen "NeoVarSawOsc" rt (list freq pw waveshape) nil 1 nil)))

(define NestedAllpassC
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2)
    (construct-ugen "NestedAllpassC" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) nil 1 nil)))

(define NestedAllpassL
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2)
    (construct-ugen "NestedAllpassL" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) nil 1 nil)))

(define NestedAllpassN
  (lambda (input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2)
    (construct-ugen "NestedAllpassN" (list 0) (list input maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) nil 1 nil)))

(define OSFold4
  (lambda (rt input lo hi)
    (construct-ugen "OSFold4" rt (list input lo hi) nil 1 nil)))

(define OSFold8
  (lambda (rt input lo hi)
    (construct-ugen "OSFold8" rt (list input lo hi) nil 1 nil)))

(define OSTrunc4
  (lambda (rt input quant)
    (construct-ugen "OSTrunc4" rt (list input quant) nil 1 nil)))

(define OSTrunc8
  (lambda (rt input quant)
    (construct-ugen "OSTrunc8" rt (list input quant) nil 1 nil)))

(define OSWrap4
  (lambda (rt input lo hi)
    (construct-ugen "OSWrap4" rt (list input lo hi) nil 1 nil)))

(define OSWrap8
  (lambda (rt input lo hi)
    (construct-ugen "OSWrap8" rt (list input lo hi) nil 1 nil)))

(define OnsetStatistics
  (lambda (rt input windowsize hopsize)
    (construct-ugen "OnsetStatistics" rt (list input windowsize hopsize) nil 3 nil)))

(define Oregonator
  (lambda (rt reset rate_ epsilon mu q initx inity initz)
    (construct-ugen "Oregonator" rt (list reset rate_ epsilon mu q initx inity initz) nil 3 nil)))

(define OscBank
  (lambda (rt freq gain saw8 square8 saw4 square4 saw2 square2 saw1)
    (construct-ugen "OscBank" rt (list freq gain saw8 square8 saw4 square4 saw2 square2 saw1) nil 1 nil)))

(define OteyPiano
  (lambda (rt freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type)
    (construct-ugen "OteyPiano" rt (list freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type) nil 1 nil)))

(define OteyPianoStrings
  (lambda (rt freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type)
    (construct-ugen "OteyPianoStrings" rt (list freq vel t_gate rmin rmax rampl rampr rcore lmin lmax lampl lampr rho e zb zh mh k alpha p hpos loss detune hammer_type) nil 1 nil)))

(define OteySoundBoard
  (lambda (inp c1 c3 mix)
    (construct-ugen "OteySoundBoard" (list 0) (list inp c1 c3 mix) nil 1 nil)))

(define PVInfo
  (lambda (rt pvbuffer binNum filePointer)
    (construct-ugen "PVInfo" rt (list pvbuffer binNum filePointer) nil 2 nil)))

(define PVSynth
  (lambda (rt pvbuffer numBins binStart binSkip filePointer freqMul freqAdd)
    (construct-ugen "PVSynth" rt (list pvbuffer numBins binStart binSkip filePointer freqMul freqAdd) nil 1 nil)))

(define PV_BinBufRd
  (lambda (buffer playbuf point binStart binSkip numBins clear)
    (construct-ugen "PV_BinBufRd" kr (list buffer playbuf point binStart binSkip numBins clear) nil 1 nil)))

(define PV_BinDelay
  (lambda (buffer maxdelay delaybuf fbbuf hop)
    (construct-ugen "PV_BinDelay" kr (list buffer maxdelay delaybuf fbbuf hop) nil 1 nil)))

(define PV_BinFilter
  (lambda (buffer start end)
    (construct-ugen "PV_BinFilter" kr (list buffer start end) nil 1 nil)))

(define PV_BinPlayBuf
  (lambda (buffer playbuf rate_ offset binStart binSkip numBins loop clear)
    (construct-ugen "PV_BinPlayBuf" kr (list buffer playbuf rate_ offset binStart binSkip numBins loop clear) nil 1 nil)))

(define PV_BufRd
  (lambda (buffer playbuf point)
    (construct-ugen "PV_BufRd" kr (list buffer playbuf point) nil 1 nil)))

(define PV_CommonMag
  (lambda (bufferA bufferB tolerance remove)
    (construct-ugen "PV_CommonMag" kr (list bufferA bufferB tolerance remove) nil 1 nil)))

(define PV_CommonMul
  (lambda (bufferA bufferB tolerance remove)
    (construct-ugen "PV_CommonMul" kr (list bufferA bufferB tolerance remove) nil 1 nil)))

(define PV_Compander
  (lambda (buffer thresh slopeBelow slopeAbove)
    (construct-ugen "PV_Compander" kr (list buffer thresh slopeBelow slopeAbove) nil 1 nil)))

(define PV_Cutoff
  (lambda (bufferA bufferB wipe)
    (construct-ugen "PV_Cutoff" kr (list bufferA bufferB wipe) nil 1 nil)))

(define PV_EvenBin
  (lambda (buffer)
    (construct-ugen "PV_EvenBin" kr (list buffer) nil 1 nil)))

(define PV_ExtractRepeat
  (lambda (buffer loopbuf loopdur memorytime which ffthop thresh)
    (construct-ugen "PV_ExtractRepeat" kr (list buffer loopbuf loopdur memorytime which ffthop thresh) nil 1 nil)))

(define PV_Freeze
  (lambda (buffer freeze)
    (construct-ugen "PV_Freeze" kr (list buffer freeze) nil 1 nil)))

(define PV_FreqBuffer
  (lambda (buffer databuffer)
    (construct-ugen "PV_FreqBuffer" kr (list buffer databuffer) nil 1 nil)))

(define PV_Invert
  (lambda (buffer)
    (construct-ugen "PV_Invert" kr (list buffer) nil 1 nil)))

(define PV_MagBuffer
  (lambda (buffer databuffer)
    (construct-ugen "PV_MagBuffer" kr (list buffer databuffer) nil 1 nil)))

(define PV_MagExp
  (lambda (buffer)
    (construct-ugen "PV_MagExp" kr (list buffer) nil 1 nil)))

(define PV_MagGate
  (lambda (buffer thresh remove)
    (construct-ugen "PV_MagGate" kr (list buffer thresh remove) nil 1 nil)))

(define PV_MagLog
  (lambda (buffer)
    (construct-ugen "PV_MagLog" kr (list buffer) nil 1 nil)))

(define PV_MagMap
  (lambda (buffer mapbuf)
    (construct-ugen "PV_MagMap" kr (list buffer mapbuf) nil 1 nil)))

(define PV_MagMinus
  (lambda (bufferA bufferB remove)
    (construct-ugen "PV_MagMinus" kr (list bufferA bufferB remove) nil 1 nil)))

(define PV_MagMulAdd
  (lambda (buffer)
    (construct-ugen "PV_MagMulAdd" kr (list buffer) nil 1 nil)))

(define PV_MagScale
  (lambda (bufferA bufferB)
    (construct-ugen "PV_MagScale" kr (list bufferA bufferB) nil 1 nil)))

(define PV_MagSmooth
  (lambda (buffer factor)
    (construct-ugen "PV_MagSmooth" kr (list buffer factor) nil 1 nil)))

(define PV_MagSubtract
  (lambda (bufferA bufferB zerolimit)
    (construct-ugen "PV_MagSubtract" kr (list bufferA bufferB zerolimit) nil 1 nil)))

(define PV_MaxMagN
  (lambda (buffer numbins)
    (construct-ugen "PV_MaxMagN" kr (list buffer numbins) nil 1 nil)))

(define PV_MinMagN
  (lambda (buffer numbins)
    (construct-ugen "PV_MinMagN" kr (list buffer numbins) nil 1 nil)))

(define PV_Morph
  (lambda (bufferA bufferB morph)
    (construct-ugen "PV_Morph" kr (list bufferA bufferB morph) nil 1 nil)))

(define PV_NoiseSynthF
  (lambda (buffer threshold numFrames initflag)
    (construct-ugen "PV_NoiseSynthF" kr (list buffer threshold numFrames initflag) nil 1 nil)))

(define PV_NoiseSynthP
  (lambda (buffer threshold numFrames initflag)
    (construct-ugen "PV_NoiseSynthP" kr (list buffer threshold numFrames initflag) nil 1 nil)))

(define PV_OddBin
  (lambda (buffer)
    (construct-ugen "PV_OddBin" kr (list buffer) nil 1 nil)))

(define PV_PartialSynthF
  (lambda (buffer threshold numFrames initflag)
    (construct-ugen "PV_PartialSynthF" kr (list buffer threshold numFrames initflag) nil 1 nil)))

(define PV_PartialSynthP
  (lambda (buffer threshold numFrames initflag)
    (construct-ugen "PV_PartialSynthP" kr (list buffer threshold numFrames initflag) nil 1 nil)))

(define PV_PitchShift
  (lambda (buffer ratio)
    (construct-ugen "PV_PitchShift" kr (list buffer ratio) nil 1 nil)))

(define PV_PlayBuf
  (lambda (buffer playbuf rate_ offset loop)
    (construct-ugen "PV_PlayBuf" kr (list buffer playbuf rate_ offset loop) nil 1 nil)))

(define PV_RecordBuf
  (lambda (buffer recbuf offset run loop hop wintype)
    (construct-ugen "PV_RecordBuf" kr (list buffer recbuf offset run loop hop wintype) nil 1 nil)))

(define PV_SoftWipe
  (lambda (bufferA bufferB wipe)
    (construct-ugen "PV_SoftWipe" kr (list bufferA bufferB wipe) nil 1 nil)))

(define PV_SpectralEnhance
  (lambda (buffer numPartials ratio strength)
    (construct-ugen "PV_SpectralEnhance" kr (list buffer numPartials ratio strength) nil 1 nil)))

(define PV_SpectralMap
  (lambda (buffer specBuffer floor_ freeze mode norm window)
    (construct-ugen "PV_SpectralMap" kr (list buffer specBuffer floor_ freeze mode norm window) nil 1 nil)))

(define PV_Split
  (lambda (bufferA bufferB)
    (construct-ugen "PV_Split" kr (list bufferA bufferB) nil 2 nil)))

(define PV_Whiten
  (lambda (chain trackbufnum relaxtime floor_ smear bindownsample)
    (construct-ugen "PV_Whiten" kr (list chain trackbufnum relaxtime floor_ smear bindownsample) nil 1 nil)))

(define PV_XFade
  (lambda (bufferA bufferB fade)
    (construct-ugen "PV_XFade" kr (list bufferA bufferB fade) nil 1 nil)))

(define PanX
  (lambda (rt numChans input pos level width)
    (construct-ugen "PanX" rt (list numChans input pos level width) nil 0 nil)))

(define PanX2D
  (lambda (rt numChansX numChansY input posX posY level widthX widthY)
    (construct-ugen "PanX2D" rt (list numChansX numChansY input posX posY level widthX widthY) nil 0 nil)))

(define PeakEQ2
  (lambda (rt input freq rs db)
    (construct-ugen "PeakEQ2" rt (list input freq rs db) nil 1 nil)))

(define PeakEQ4
  (lambda (rt input freq rs db)
    (construct-ugen "PeakEQ4" rt (list input freq rs db) nil 1 nil)))

(define Perlin3
  (lambda (rt x y z)
    (construct-ugen "Perlin3" rt (list x y z) nil 1 nil)))

(define PermMod
  (lambda (rt input freq)
    (construct-ugen "PermMod" rt (list input freq) nil 1 nil)))

(define PermModArray
  (lambda (rt input freq pattern)
    (construct-ugen "PermModArray" rt (list input freq pattern) nil 1 nil)))

(define PermModT
  (lambda (rt input outfreq infreq)
    (construct-ugen "PermModT" rt (list input outfreq infreq) nil 1 nil)))

(define PhasorModal
  (lambda (input freq decay_ damp amp phase)
    (construct-ugen "PhasorModal" (list 0) (list input freq decay_ damp amp phase) nil 1 nil)))

(define PlaneTree
  (lambda (rt treebuf input gate_)
    (construct-ugen "PlaneTree" rt (list treebuf input gate_) nil 1 nil)))

(define PluckSynth
  (lambda (rt freq amp gate_ pos c1 c3 release f m k r l ra rho)
    (construct-ugen "PluckSynth" rt (list freq amp gate_ pos c1 c3 release f m k r l ra rho) nil 1 nil)))

(define PosRatio
  (lambda (rt input period thresh)
    (construct-ugen "PosRatio" rt (list input period thresh) nil 1 nil)))

(define PrintVal
  (lambda (rt input numblocks id_)
    (construct-ugen "PrintVal" rt (list input numblocks id_) nil 1 nil)))

(define Qitch
  (lambda (rt input databufnum ampThreshold algoflag ampbufnum minfreq maxfreq)
    (construct-ugen "Qitch" rt (list input databufnum ampThreshold algoflag ampbufnum minfreq maxfreq) nil 2 nil)))

(define RBezier
  (lambda (rt haltAfter dx freq phase param)
    (construct-ugen "RBezier" rt (list haltAfter dx freq phase param) nil 1 nil)))

(define RDX7
  (lambda (rt bufnum on off data_ vc mnn vel pw mw bc fc)
    (construct-ugen "RDX7" rt (list bufnum on off data_ vc mnn vel pw mw bc fc) nil 1 nil)))

(define RDelayMap
  (lambda (bufnum input dynamic spec)
    (construct-ugen "RDelayMap" (list 1) (list bufnum input dynamic spec) nil 1 nil)))

(define RDelaySet
  (lambda (rt input spec)
    (construct-ugen "RDelaySet" rt (list input spec) nil 1 nil)))

(define RDelaySetB
  (lambda (rt bufnum input spec)
    (construct-ugen "RDelaySetB" rt (list bufnum input spec) nil 1 nil)))

(define RFreezer
  (lambda (rt bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops)
    (construct-ugen "RFreezer" rt (list bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops) nil 1 nil)))

(define RLPFD
  (lambda (input ffreq res dist)
    (construct-ugen "RLPFD" (list 0) (list input ffreq res dist) nil 1 nil)))

(define RLoopSet
  (lambda (rt bufnum left right gain increment spec)
    (construct-ugen "RLoopSet" rt (list bufnum left right gain increment spec) nil 1 nil)))

(define RMAFoodChainL
  (lambda (rt freq a1 b1 d1 a2 b2 d2 k r h xi yi zi)
    (construct-ugen "RMAFoodChainL" rt (list freq a1 b1 d1 a2 b2 d2 k r h xi yi zi) nil 3 nil)))

(define RMEQ
  (lambda (input freq rq k)
    (construct-ugen "RMEQ" (list 0) (list input freq rq k) nil 1 nil)))

(define RMEQSuite
  (lambda (rt maxSize)
    (construct-ugen "RMEQSuite" rt (list maxSize) nil 1 nil)))

(define RMS
  (lambda (rt input lpFreq)
    (construct-ugen "RMS" rt (list input lpFreq) nil 1 nil)))

(define RMShelf
  (lambda (rt input freq k)
    (construct-ugen "RMShelf" rt (list input freq k) nil 1 nil)))

(define RMShelf2
  (lambda (rt input freq k)
    (construct-ugen "RMShelf2" rt (list input freq k) nil 1 nil)))

(define RObxdFilter
  (lambda (rt input cutoff resonance multimode bandpass fourpole)
    (construct-ugen "RObxdFilter" rt (list input cutoff resonance multimode bandpass fourpole) nil 1 nil)))

(define RPVDecayTbl
  (lambda (rt fft_buf decay_rate_buf history_buf)
    (construct-ugen "RPVDecayTbl" rt (list fft_buf decay_rate_buf history_buf) nil 1 nil)))

(define RPlayTrace
  (lambda (rt bufnum degree rate_ axis)
    (construct-ugen "RPlayTrace" rt (list bufnum degree rate_ axis) nil 1 nil)))

(define RShufflerB
  (lambda (bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta)
    (construct-ugen "RShufflerB" ar (list bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta) nil 2 nil)))

(define RShufflerL
  (lambda (rt input fragmentSize maxDelay)
    (construct-ugen "RShufflerL" rt (list input fragmentSize maxDelay) nil 1 nil)))

(define RTraceRd
  (lambda (rt bufnum degree index axis)
    (construct-ugen "RTraceRd" rt (list bufnum degree index axis) nil 1 nil)))

(define RRandN
  (lambda (numChannels lo hi)
    (construct-ugen "RRandN" ir (list lo hi) nil numChannels nil)))

(define RegaliaMitraEQ
  (lambda (rt input freq rq k)
    (construct-ugen "RegaliaMitraEQ" rt (list input freq rq k) nil 1 nil)))

(define Resonator
  (lambda (input freq position resolution structure brightness damping)
    (construct-ugen "Resonator" (list 0) (list input freq position resolution structure brightness damping) nil 1 nil)))

(define Rongs
  (lambda (rt trigger sustain f0 structure brightness damping accent stretch position loss)
    (construct-ugen "Rongs" rt (list trigger sustain f0 structure brightness damping accent stretch position loss) nil 1 nil)))

(define RosslerL
  (lambda (rt freq a b c h xi yi zi)
    (construct-ugen "RosslerL" rt (list freq a b c h xi yi zi) nil 3 nil)))

(define RosslerResL
  (lambda (rt input stiff freq a b c h xi yi zi)
    (construct-ugen "RosslerResL" rt (list input stiff freq a b c h xi yi zi) nil 1 nil)))

(define Rotate
  (lambda (rt w x y z rotate)
    (construct-ugen "Rotate" rt (list w x y z rotate) nil 1 nil)))

(define SLOnset
  (lambda (rt input memorysize1 before after threshold hysteresis)
    (construct-ugen "SLOnset" rt (list input memorysize1 before after threshold hysteresis) nil 1 nil)))

(define SMS
  (lambda (input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum)
    (construct-ugen "SMS" (list 0) (list input maxpeaks currentpeaks tolerance noisefloor freqmult freqadd formantpreserve useifft ampmult graphicsbufnum) nil 2 nil)))

(define SOMAreaWr
  (lambda (rt bufnum inputdata coords netsize numdims nhood gate_)
    (construct-ugen "SOMAreaWr" rt (list bufnum inputdata coords netsize numdims nhood gate_) nil 1 nil)))

(define SOMRd
  (lambda (rt bufnum inputdata netsize numdims gate_)
    (construct-ugen "SOMRd" rt (list bufnum inputdata netsize numdims gate_) nil 2 nil)))

(define SOMTrain
  (lambda (rt bufnum inputdata netsize numdims traindur nhood gate_ initweight)
    (construct-ugen "SOMTrain" rt (list bufnum inputdata netsize numdims traindur nhood gate_ initweight) nil 3 nil)))

(define SVF
  (lambda (signal cutoff res lowpass bandpass highpass notch peak)
    (construct-ugen "SVF" (list 0) (list signal cutoff res lowpass bandpass highpass notch peak) nil 1 nil)))

(define SawDPW
  (lambda (rt freq iphase)
    (construct-ugen "SawDPW" rt (list freq iphase) nil 1 nil)))

(define SensoryDissonance
  (lambda (rt fft maxpeaks peakthreshold norm clamp)
    (construct-ugen "SensoryDissonance" rt (list fft maxpeaks peakthreshold norm clamp) nil 1 nil)))

(define Sieve1
  (lambda (rt bufnum gap alternate)
    (construct-ugen "Sieve1" rt (list bufnum gap alternate) nil 1 nil)))

(define SinGrain
  (lambda (rt trigger dur freq)
    (construct-ugen "SinGrain" rt (list trigger dur freq) nil 1 nil)))

(define SinGrainB
  (lambda (rt trigger dur freq envbuf)
    (construct-ugen "SinGrainB" rt (list trigger dur freq envbuf) nil 1 nil)))

(define SinGrainBBF
  (lambda (rt trigger dur freq envbuf azimuth elevation rho wComp)
    (construct-ugen "SinGrainBBF" rt (list trigger dur freq envbuf azimuth elevation rho wComp) nil 4 nil)))

(define SinGrainBF
  (lambda (rt trigger dur freq azimuth elevation rho wComp)
    (construct-ugen "SinGrainBF" rt (list trigger dur freq azimuth elevation rho wComp) nil 4 nil)))

(define SinGrainI
  (lambda (rt trigger dur freq envbuf1 envbuf2 ifac)
    (construct-ugen "SinGrainI" rt (list trigger dur freq envbuf1 envbuf2 ifac) nil 1 nil)))

(define SinGrainIBF
  (lambda (rt trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp)
    (construct-ugen "SinGrainIBF" rt (list trigger dur freq envbuf1 envbuf2 ifac azimuth elevation rho wComp) nil 4 nil)))

(define SinTone
  (lambda (rt freq phase)
    (construct-ugen "SinTone" rt (list freq phase) nil 1 nil)))

(define SineShaper
  (lambda (input limit)
    (construct-ugen "SineShaper" (list 0) (list input limit) nil 1 nil)))

(define SkipNeedle
  (lambda (rt range rate_ offset)
    (construct-ugen "SkipNeedle" rt (list range rate_ offset) nil 1 nil)))

(define SmoothDecimator
  (lambda (rt input rate_ smoothing)
    (construct-ugen "SmoothDecimator" rt (list input rate_ smoothing) nil 1 nil)))

(define SoftClipAmp
  (lambda (input pregain)
    (construct-ugen "SoftClipAmp" (list 0) (list input pregain) nil 1 nil)))

(define SoftClipAmp4
  (lambda (input pregain)
    (construct-ugen "SoftClipAmp4" (list 0) (list input pregain) nil 1 nil)))

(define SoftClipAmp8
  (lambda (input pregain)
    (construct-ugen "SoftClipAmp8" (list 0) (list input pregain) nil 1 nil)))

(define SoftClipper4
  (lambda (rt input)
    (construct-ugen "SoftClipper4" rt (list input) nil 1 nil)))

(define SoftClipper8
  (lambda (rt input)
    (construct-ugen "SoftClipper8" rt (list input) nil 1 nil)))

(define SonLPC
  (lambda (rt buff input hop poles)
    (construct-ugen "SonLPC" rt (list buff input hop poles) nil 1 nil)))

(define SonLPCSynth
  (lambda (rt chain)
    (construct-ugen "SonLPCSynth" rt (list chain) nil 1 nil)))

(define SonLPCSynthIn
  (lambda (rt chain input)
    (construct-ugen "SonLPCSynthIn" rt (list chain input) nil 1 nil)))

(define SortBuf
  (lambda (rt bufnum sortrate reset)
    (construct-ugen "SortBuf" rt (list bufnum sortrate reset) nil 1 nil)))

(define SpectralEntropy
  (lambda (nc rt fft fftsize numbands)
    (construct-ugen "SpectralEntropy" rt (list fft fftsize numbands) nil nc nil)))

(define Spreader
  (lambda (rt input theta filtsPerOctave)
    (construct-ugen "Spreader" rt (list input theta filtsPerOctave) nil 2 nil)))

(define SpruceBudworm
  (lambda (rt reset rate_ k1 k2 alpha beta mu rho initx inity)
    (construct-ugen "SpruceBudworm" rt (list reset rate_ k1 k2 alpha beta mu rho initx inity) nil 2 nil)))

(define Squiz
  (lambda (input pitchratio zcperchunk memlen)
    (construct-ugen "Squiz" (list 0) (list input pitchratio zcperchunk memlen) nil 1 nil)))

(define Standard2DC
  (lambda (rt minfreq maxfreq k x0 y0)
    (construct-ugen "Standard2DC" rt (list minfreq maxfreq k x0 y0) nil 1 nil)))

(define Standard2DL
  (lambda (rt minfreq maxfreq k x0 y0)
    (construct-ugen "Standard2DL" rt (list minfreq maxfreq k x0 y0) nil 1 nil)))

(define Standard2DN
  (lambda (rt minfreq maxfreq k x0 y0)
    (construct-ugen "Standard2DN" rt (list minfreq maxfreq k x0 y0) nil 1 nil)))

(define StandardTrig
  (lambda (rt minfreq maxfreq k x0 y0)
    (construct-ugen "StandardTrig" rt (list minfreq maxfreq k x0 y0) nil 1 nil)))

(define StkBandedWG
  (lambda (rt freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_)
    (construct-ugen "StkBandedWG" rt (list freq instr bowpressure bowmotion integration modalresonance bowvelocity setstriking trig_) nil 1 nil)))

(define StkBeeThree
  (lambda (rt freq op4gain op3gain lfospeed lfodepth adsrtarget trig_)
    (construct-ugen "StkBeeThree" rt (list freq op4gain op3gain lfospeed lfodepth adsrtarget trig_) nil 1 nil)))

(define StkBlowHole
  (lambda (rt freq reedstiffness noisegain tonehole register breathpressure)
    (construct-ugen "StkBlowHole" rt (list freq reedstiffness noisegain tonehole register breathpressure) nil 1 nil)))

(define StkBowed
  (lambda (rt freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate)
    (construct-ugen "StkBowed" rt (list freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate) nil 1 nil)))

(define StkClarinet
  (lambda (rt freq reedstiffness noisegain vibfreq vibgain breathpressure trig_)
    (construct-ugen "StkClarinet" rt (list freq reedstiffness noisegain vibfreq vibgain breathpressure trig_) nil 1 nil)))

(define StkFlute
  (lambda (rt freq jetDelay noisegain jetRatio)
    (construct-ugen "StkFlute" rt (list freq jetDelay noisegain jetRatio) nil 1 nil)))

(define StkGlobals
  (lambda (rt showWarnings printErrors rawfilepath)
    (construct-ugen "StkGlobals" rt (list showWarnings printErrors rawfilepath) nil 1 nil)))

(define StkInst
  (lambda (rt freq gate_ onamp offamp instNumber args)
    (construct-ugen "StkInst" rt (list freq gate_ onamp offamp instNumber) args 1 nil)))

(define StkMandolin
  (lambda (rt freq bodysize pickposition stringdamping stringdetune aftertouch trig_)
    (construct-ugen "StkMandolin" rt (list freq bodysize pickposition stringdamping stringdetune aftertouch trig_) nil 1 nil)))

(define StkModalBar
  (lambda (rt freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_)
    (construct-ugen "StkModalBar" rt (list freq instrument stickhardness stickposition vibratogain vibratofreq directstickmix volume trig_) nil 1 nil)))

(define StkMoog
  (lambda (rt freq filterQ sweeprate vibfreq vibgain gain trig_)
    (construct-ugen "StkMoog" rt (list freq filterQ sweeprate vibfreq vibgain gain trig_) nil 1 nil)))

(define StkPluck
  (lambda (rt freq decay_)
    (construct-ugen "StkPluck" rt (list freq decay_) nil 1 nil)))

(define StkSaxofony
  (lambda (rt freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_)
    (construct-ugen "StkSaxofony" rt (list freq reedstiffness reedaperture noisegain blowposition vibratofrequency vibratogain breathpressure trig_) nil 1 nil)))

(define StkShakers
  (lambda (rt instr energy decay_ objects resfreq)
    (construct-ugen "StkShakers" rt (list instr energy decay_ objects resfreq) nil 1 nil)))

(define StkVoicForm
  (lambda (rt freq vuvmix vowelphon vibfreq vibgain loudness_ trig_)
    (construct-ugen "StkVoicForm" rt (list freq vuvmix vowelphon vibfreq vibgain loudness_ trig_) nil 1 nil)))

(define Streson
  (lambda (input delayTime res)
    (construct-ugen "Streson" (list 0) (list input delayTime res) nil 1 nil)))

(define StringVoice
  (lambda (rt trig_ infsustain freq accent structure brightness damping)
    (construct-ugen "StringVoice" rt (list trig_ infsustain freq accent structure brightness damping) nil 1 nil)))

(define Summer
  (lambda (trig_ step reset resetval)
    (construct-ugen "Summer" (list 0) (list trig_ step reset resetval) nil 1 nil)))

(define SwitchDelay
  (lambda (input drylevel wetlevel delaytime delayfactor maxdelaytime)
    (construct-ugen "SwitchDelay" (list 0) (list input drylevel wetlevel delaytime delayfactor maxdelaytime) nil 1 nil)))

(define TBetaRand
  (lambda (lo hi prob1 prob2 trig_)
    (construct-ugen "TBetaRand" (list 4) (list lo hi prob1 prob2 trig_) nil 1 nil)))

(define TBrownRand
  (lambda (lo hi dev dist trig_)
    (construct-ugen "TBrownRand" (list 4) (list lo hi dev dist trig_) nil 1 nil)))

(define RTExpRandN
  (lambda (numChannels lo hi trigger)
    (construct-ugen "RTExpRandN" (list 3) (list lo hi trigger) nil numChannels nil)))

(define TGaussRand
  (lambda (lo hi trig_)
    (construct-ugen "TGaussRand" (list 2) (list lo hi trig_) nil 1 nil)))

(define TGrains2
  (lambda (nc rt trigger bufnum rate_ centerPos dur pan amp att dec interp)
    (construct-ugen "TGrains2" rt (list trigger bufnum rate_ centerPos dur pan amp att dec interp) nil nc nil)))

(define TGrains3
  (lambda (nc rt trigger bufnum rate_ centerPos dur pan amp att dec window interp)
    (construct-ugen "TGrains3" rt (list trigger bufnum rate_ centerPos dur pan amp att dec window interp) nil nc nil)))

(define TPV
  (lambda (chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor)
    (construct-ugen "TPV" ar (list chain windowsize hopsize maxpeaks currentpeaks freqmult tolerance noisefloor) nil 1 nil)))

(define RTRandN
  (lambda (numChannels lo hi trigger)
    (construct-ugen "RTRandN" (list 3) (list lo hi trigger) nil numChannels nil)))

(define TScramble
  (lambda (rt trigger inputs)
    (construct-ugen "TScramble" rt (list trigger inputs) nil 1 nil)))

(define TTendency
  (lambda (rt trigger dist parX parY parA parB)
    (construct-ugen "TTendency" rt (list trigger dist parX parY parA parB) nil 1 nil)))

(define Tartini
  (lambda (rt input threshold n k overlap smallCutoff)
    (construct-ugen "Tartini" rt (list input threshold n k overlap smallCutoff) nil 2 nil)))

(define TermanWang
  (lambda (rt input reset ratex ratey alpha beta eta initx inity)
    (construct-ugen "TermanWang" rt (list input reset ratex ratey alpha beta eta initx inity) nil 1 nil)))

(define TextVU
  (lambda (rt trig_ input label_ width reset ana)
    (construct-ugen "TextVU" rt (list trig_ input label_ width reset ana) nil 1 nil)))

(define Tilt
  (lambda (rt w x y z tilt)
    (construct-ugen "Tilt" rt (list w x y z tilt) nil 1 nil)))

(define TrigAvg
  (lambda (rt input trig_)
    (construct-ugen "TrigAvg" rt (list input trig_) nil 1 nil)))

(define Tumble
  (lambda (rt w x y z tilt)
    (construct-ugen "Tumble" rt (list w x y z tilt) nil 1 nil)))

(define TwoTube
  (lambda (rt input k loss d1length d2length)
    (construct-ugen "TwoTube" rt (list input k loss d1length d2length) nil 1 nil)))

(define UHJ2B
  (lambda (rt ls rs)
    (construct-ugen "UHJ2B" rt (list ls rs) nil 3 nil)))

(define VBAP
  (lambda (nc rt input bufnum azimuth elevation spread)
    (construct-ugen "VBAP" rt (list input bufnum azimuth elevation spread) nil nc nil)))

(define VBFourses
  (lambda (rt smoother freqarray)
    (construct-ugen "VBFourses" rt (list smoother) freqarray 4 nil)))

(define VBJonVerb
  (lambda (input decay_ damping inputbw erfl tail_)
    (construct-ugen "VBJonVerb" (list 0) (list input decay_ damping inputbw erfl tail_) nil 2 nil)))

(define VBPVoc
  (lambda (rt bufnum playpos fftsize)
    (construct-ugen "VBPVoc" rt (list bufnum playpos fftsize) nil 1 nil)))

(define VBSlide
  (lambda (input slideup slidedown)
    (construct-ugen "VBSlide" (list 0) (list input slideup slidedown) nil 1 nil)))

(define VMScan2D
  (lambda (rt bufnum)
    (construct-ugen "VMScan2D" rt (list bufnum) nil 2 nil)))

(define VOSIM
  (lambda (rt trig_ freq nCycles decay_)
    (construct-ugen "VOSIM" rt (list trig_ freq nCycles decay_) nil 1 nil)))

(define VarShapeOsc
  (lambda (rt freq pw waveshape sync syncfreq)
    (construct-ugen "VarShapeOsc" rt (list freq pw waveshape sync syncfreq) nil 1 nil)))

(define VosimOsc
  (lambda (rt freq form1freq form2freq shape)
    (construct-ugen "VosimOsc" rt (list freq form1freq form2freq shape) nil 1 nil)))

(define WAmp
  (lambda (rt input winSize)
    (construct-ugen "WAmp" rt (list input winSize) nil 1 nil)))

(define WalshHadamard
  (lambda (rt input which)
    (construct-ugen "WalshHadamard" rt (list input which) nil 1 nil)))

(define WarpZ
  (lambda (nc rt bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart)
    (construct-ugen "WarpZ" rt (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp zeroSearch zeroStart) nil nc nil)))

(define WaveLoss
  (lambda (rt input drop_ outof mode)
    (construct-ugen "WaveLoss" rt (list input drop_ outof mode) nil 1 nil)))

(define WaveTerrain
  (lambda (rt bufnum x y xsize ysize)
    (construct-ugen "WaveTerrain" rt (list bufnum x y xsize ysize) nil 1 nil)))

(define WaveletDaub
  (lambda (rt input n which)
    (construct-ugen "WaveletDaub" rt (list input n which) nil 1 nil)))

(define WeaklyNonlinear
  (lambda (rt input reset ratex ratey freq initx inity alpha xexponent beta yexponent)
    (construct-ugen "WeaklyNonlinear" rt (list input reset ratex ratey freq initx inity alpha xexponent beta yexponent) nil 1 nil)))

(define WeaklyNonlinear2
  (lambda (rt input reset ratex ratey freq initx inity alpha xexponent beta yexponent)
    (construct-ugen "WeaklyNonlinear2" rt (list input reset ratex ratey freq initx inity alpha xexponent beta yexponent) nil 1 nil)))

(define WrapSummer
  (lambda (rt trig_ step min_ max_ reset resetval)
    (construct-ugen "WrapSummer" rt (list trig_ step min_ max_ reset resetval) nil 1 nil)))

(define ZOsc
  (lambda (rt freq formantfreq shape mode)
    (construct-ugen "ZOsc" rt (list freq formantfreq shape mode) nil 1 nil)))
; Zero local buffer.
;
; ClearBuf does not copy the buffer number through so this is an MRG node.
;
; ClearBuf :: ugen -> ugen
(define ClearBuf
  (lambda (b)
    (mrg2 b (construct-ugen "ClearBuf" ir (list b) nil 1 nil))))

(define PackFFT
  (lambda (b sz fr to z mp)
    (construct-ugen "PackFFT" kr (list b sz fr to z) mp 1 nil)))

(define string->ugen
  (lambda (s)
    (make-mce
     (cons (string-length s)
           (map char->integer (string->list s))))))

(define Poll
  (lambda (trig input trigid label_)
    (construct-ugen "Poll" (list 1) (list trig input trigid) (string->ugen label_) 1 nil)))

(define PV_HainsworthFoote
  (lambda (buf h f thr wt)
    (construct-ugen "PV_HainsworthFoote" ar (list buf h f thr wt) nil 1 nil)))

(define PV_JensenAndersen
  (lambda (buffer propsc prophfe prophfc propsf threshold waittime)
    (construct-ugen "PV_JensenAndersen" ar (list buffer propsc prophfe prophfc propsf threshold waittime) nil 1 nil)))

(define Unpack1FFT
  (lambda (c b bi wm)
    (construct-ugen "Unpack1FFT" dr (list c b bi wm) nil 1 nil)))
