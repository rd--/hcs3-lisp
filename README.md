Lisp Supercollider
------------------

<!-- American Primitive, Vol. 2 -->

`hsc3-lisp` is a simple
[Lisp](http://www-formal.stanford.edu/jmc/history/lisp/lisp.html)
interpreter.

The only data type is the
[SuperCollider](http://audiosynth.com/)
`Unit Generator`.

[Why](http://rohandrape.net/?t=hsc3-graphs&e=lib/sc/graph/jmcc-why-supercollider.scd) Lisp SuperCollider?

~~~~
(let* ((s (mixFill 10 (lambda (_) (Resonz (Mul (Dust 0.2) 50) (Rand 200 3200) 0.003))))
       (z (DelayN s 0.048 0.048))
       (y (mixFill 7 (lambda (_) (CombL z 0.1 (MulAdd (LFNoise1 (Rand 0 0.1)) 0.04 0.05) 15))))
       (x (iter 4 (lambda (i) (AllpassN i 0.05 (RandN 2 0 0.05) 1)) y)))
  (Add s (Mul x 0.2)))
~~~~

There is an OSX (10.9.3) binary
([hsc3-lisp](sw/hsc3-lisp/osx/hsc3-lisp.xz).[xz](http://tukaani.org/xz/)),
or to build type:

~~~~
cabal install
~~~~

An environment variable locates the hsc3-lisp library files:

~~~~
export HSC3_LISP_DIR=$HOME/sw/hsc3-lisp/scm
~~~~

To hear the above type `C-cC-a` in
[Emacs](http://www.gnu.org/software/emacs/).

<!--
Or in a shell type:

~~~~
hsc3-lisp ~/sw/rsc3/help/graph/jmcc-why-supercollider.scm
~~~~
-->

There is a terse [tutorial](http://rohandrape.net/?t=hsc3-lisp&e=help/tutorial.scm).

<!--
and [translations](?t=hsc3-lisp&e=help/jmcc.lisp) of graphs by [James
McCartney](http://audiosynth.com/autobio/emu.jpg) from the `SC2`
manual.
-->

<!-- hsc3-lisp is `case-insensitive`. -->

<!-- In emacs you can type `C-x C-l` to time travel. -->

hsc3-lisp follows the
[Haskell](http://haskell.org/)
SuperCollider
([hsc3](http://rohandrape.net/?t=hsc3))
rules.

hsc3-lisp requires `scsynth` be listening at the standard port (57110).

hsc3-lisp is Post-[ML](http://standardml.org), ie.

    > (equal? (map (+ 1) (list 1 2 3)) (list 2 3 4))
    RESULT: 1

hsc3-lisp is not [scheme](http://library.readscheme.org/standards.html).
Nonetheless, the following [rsc3](http://rohandrape.net/?t=rsc3) graphs are working:

- [adc-bit-reduction.scm](?t=rsc3&e=help/graph/adc-bit-reduction.scm)
- [adc-down-sample.scm](?t=rsc3&e=help/graph/adc-down-sample.scm)
- [adc-shepard-tones.scm](?t=rsc3&e=help/graph/adc-shepard-tones.scm)
- [cr-lucier.scm](?t=rsc3&e=help/graph/cr-lucier.scm)
- [dm-shift-register.scm](?t=rsc3&e=help/graph/dm-shift-register.scm)
- [es-tw-435684664200540161.scm](?t=rsc3&e=help/graph/es-tw-435684664200540161.scm)
- [f0-pkt-00.scm](?t=rsc3&e=help/graph/f0-pkt-00.scm)
- [f0-pkt-26.scm](?t=rsc3&e=help/graph/f0-pkt-26.scm)
- [f0-pkt-28.scm](?t=rsc3&e=help/graph/f0-pkt-28.scm)
- [f0-red-frik.scm](?t=rsc3&e=help/graph/f0-red-frik.scm)
- [f0-tw0020.scm](?t=rsc3&e=help/graph/f0-tw0020.scm)
- [f0-tw0028.scm](?t=rsc3&e=help/graph/f0-tw0028.scm)
- [f0-tw0030.scm](?t=rsc3&e=help/graph/f0-tw0030.scm)
- [f0-tw0041.scm](?t=rsc3&e=help/graph/f0-tw0041.scm)
- [f0-tw0045.scm](?t=rsc3&e=help/graph/f0-tw0045.scm)
- [f0-tw0049.scm](?t=rsc3&e=help/graph/f0-tw0049.scm)
- [f0-tw0077.scm](?t=rsc3&e=help/graph/f0-tw0077.scm)
- [f0-tw0120.scm](?t=rsc3&e=help/graph/f0-tw0120.scm)
- [f0-tw0121.scm](?t=rsc3&e=help/graph/f0-tw0121.scm)
- [f0-tw0125.scm](?t=rsc3&e=help/graph/f0-tw0125.scm)
- [f0-tw0134.scm](?t=rsc3&e=help/graph/f0-tw0134.scm)
- [f0-tw0220.scm](?t=rsc3&e=help/graph/f0-tw0220.scm)
- [f0-tw0224.scm](?t=rsc3&e=help/graph/f0-tw0224.scm)
- [f0-tw0225.scm](?t=rsc3&e=help/graph/f0-tw0225.scm)
- [f0-tw-0454598285861617665.scm](?t=rsc3&e=help/graph/f0-tw-0454598285861617665.scm)
- [f0-tw-0456384156159574016.scm](?t=rsc3&e=help/graph/f0-tw-0456384156159574016.scm)
- [f0-tw-0839296011982024704.scm](?t=rsc3&e=help/graph/f0-tw-0839296011982024704.scm)
- [f0-tw-1105496695637454848.scm](?t=rsc3&e=help/graph/f0-tw-1105496695637454848.scm)
- [f0-tw-1125557217086857216.scm](?t=rsc3&e=help/graph/f0-tw-1125557217086857216.scm)
- [f0-tw-1136928201886904320.scm](?t=rsc3&e=help/graph/f0-tw-1136928201886904320.scm)
- [f0-tw-1138498427241861122.scm](?t=rsc3&e=help/graph/f0-tw-1138498427241861122.scm)
- [f0-tw-1210118661022867458.scm](?t=rsc3&e=help/graph/f0-tw-1210118661022867458.scm)
- [f0-tw-1254441448327479299.scm](?t=rsc3&e=help/graph/f0-tw-1254441448327479299.scm)
- [f0-tw-1338987643057688579.scm](?t=rsc3&e=help/graph/f0-tw-1338987643057688579.scm)
- [f0-tw-1343283697840226307.scm](?t=rsc3&e=help/graph/f0-tw-1343283697840226307.scm)
- [jmcc-aleatoric-quartet.scm](?t=rsc3&e=help/graph/jmcc-aleatoric-quartet.scm)
- [jmcc-analog-bubbles.scm](?t=rsc3&e=help/graph/jmcc-analog-bubbles.scm)
- [jmcc-analog-bubbles-mouse.scm](?t=rsc3&e=help/graph/jmcc-analog-bubbles-mouse.scm)
- [jmcc-analogue-daze.scm](?t=rsc3&e=help/graph/jmcc-analogue-daze.scm)
- [jmcc-babbling-brook.scm](?t=rsc3&e=help/graph/jmcc-babbling-brook.scm)
- [jmcc-berlin-1977.scm](?t=rsc3&e=help/graph/jmcc-berlin-1977.scm)
- [jmcc-clustered-sines.scm](?t=rsc3&e=help/graph/jmcc-clustered-sines.scm)
- [jmcc-coolant.scm](?t=rsc3&e=help/graph/jmcc-coolant.scm)
- [jmcc-data-space.scm](?t=rsc3&e=help/graph/jmcc-data-space.scm)
- [jmcc-deep-trip.scm](?t=rsc3&e=help/graph/jmcc-deep-trip.scm)
- [jmcc-demanding-studies.scm](?t=rsc3&e=help/graph/jmcc-demanding-studies.scm)
- [jmcc-doppler.scm](?t=rsc3&e=help/graph/jmcc-doppler.scm)
- [jmcc-harmonic-swimming.scm](?t=rsc3&e=help/graph/jmcc-harmonic-swimming.scm)
- [jmcc-harmonic-tumbling.scm](?t=rsc3&e=help/graph/jmcc-harmonic-tumbling.scm)
- [jmcc-impulse-sequencer.scm](?t=rsc3&e=help/graph/jmcc-impulse-sequencer.scm)
- [jmcc-landon-rose.scm](?t=rsc3&e=help/graph/jmcc-landon-rose.scm)
- [jmcc-lfo-modulation.scm](?t=rsc3&e=help/graph/jmcc-lfo-modulation.scm)
- [jmcc-metal-plate.scm](?t=rsc3&e=help/graph/jmcc-metal-plate.scm)
- [jmcc-modal-space.scm](?t=rsc3&e=help/graph/jmcc-modal-space.scm)
- [jmcc-moto-rev.scm](?t=rsc3&e=help/graph/jmcc-moto-rev.scm)
- [jmcc-mridangam.scm](?t=rsc3&e=help/graph/jmcc-mridangam.scm)
- [jmcc-noise-burst-sweep.scm](?t=rsc3&e=help/graph/jmcc-noise-burst-sweep.scm)
- [jmcc-plucked-strings.scm](?t=rsc3&e=help/graph/jmcc-plucked-strings.scm)
- [jmcc-police-state.scm](?t=rsc3&e=help/graph/jmcc-police-state.scm)
- [jmcc-pulsing-bottles.scm](?t=rsc3&e=help/graph/jmcc-pulsing-bottles.scm)
- [jmcc-random-panning-sines.scm](?t=rsc3&e=help/graph/jmcc-random-panning-sines.scm)
- [jmcc-random-pulsations.scm](?t=rsc3&e=help/graph/jmcc-random-pulsations.scm)
- [jmcc-resonant-dust.scm](?t=rsc3&e=help/graph/jmcc-resonant-dust.scm)
- [jmcc-reverberated-noise-bursts.scm](?t=rsc3&e=help/graph/jmcc-reverberated-noise-bursts.scm)
- [jmcc-reverberated-sine-percussion.scm](?t=rsc3&e=help/graph/jmcc-reverberated-sine-percussion.scm)
- [jmcc-sample-and-hold-liquidities.scm](?t=rsc3&e=help/graph/jmcc-sample-and-hold-liquidities.scm)
- [jmcc-saucer-base.scm](?t=rsc3&e=help/graph/jmcc-saucer-base.scm)
- [jmcc-scratchy.scm](?t=rsc3&e=help/graph/jmcc-scratchy.scm)
- [jmcc-sidereal-time.scm](?t=rsc3&e=help/graph/jmcc-sidereal-time.scm)
- [jmcc-snare-909.scm](?t=rsc3&e=help/graph/jmcc-snare-909.scm)
- [jmcc-spe.scm](?t=rsc3&e=help/graph/jmcc-spe.scm)
- [jmcc-sprinkler.scm](?t=rsc3&e=help/graph/jmcc-sprinkler.scm)
- [jmcc-sprinkler-mouse.scm](?t=rsc3&e=help/graph/jmcc-sprinkler-mouse.scm)
- [jmcc-strummable-guitar.scm](?t=rsc3&e=help/graph/jmcc-strummable-guitar.scm)
- [jmcc-sweepy-noise.scm](?t=rsc3&e=help/graph/jmcc-sweepy-noise.scm)
- [jmcc-synthetic-piano.scm](?t=rsc3&e=help/graph/jmcc-synthetic-piano.scm)
- [jmcc-tank.scm](?t=rsc3&e=help/graph/jmcc-tank.scm)
- [jmcc-theremin.scm](?t=rsc3&e=help/graph/jmcc-theremin.scm)
- [jmcc-tsort.scm](?t=rsc3&e=help/graph/jmcc-tsort.scm)
- [jmcc-what-was-i-thinking.scm](?t=rsc3&e=help/graph/jmcc-what-was-i-thinking.scm)
- [jmcc-why-supercollider.scm](?t=rsc3&e=help/graph/jmcc-why-supercollider.scm)
- [jmcc-zizle.scm](?t=rsc3&e=help/graph/jmcc-zizle.scm)
- [jrhb-chain-saw.scm](?t=rsc3&e=help/graph/jrhb-chain-saw.scm)
- [jrhb-deep-sea.scm](?t=rsc3&e=help/graph/jrhb-deep-sea.scm)
- [jrhb-dial-history.scm](?t=rsc3&e=help/graph/jrhb-dial-history.scm)
- [jrhb-half-life.scm](?t=rsc3&e=help/graph/jrhb-half-life.scm)
- [lp-one-line.scm](?t=rsc3&e=help/graph/lp-one-line.scm)
- [mp-rm-octaver.scm](?t=rsc3&e=help/graph/mp-rm-octaver.scm.scm)
- [pj-forest-sounds.scm](?t=rsc3&e=help/graph/pj-forest-sounds.scm)
- [rb-hh-808.scm](?t=rsc3&e=help/graph/rb-hh-808.scm)
- [rd-ccomb.scm](?t=rsc3&e=help/graph/rd-ccomb.scm)
- [rd-chrd.scm](?t=rsc3&e=help/graph/rd-chrd.scm)
- [rd-cricket.scm](?t=rsc3&e=help/graph/rd-cricket.scm)
- [rd-crotale.scm](?t=rsc3&e=help/graph/rd-crotale.scm)
- [rd-cut-outs.scm](?t=rsc3&e=help/graph/rd-cut-outs.scm)
- [rd-diffraction.scm](?t=rsc3&e=help/graph/rd-diffraction.scm)
- [rd-discretion.scm](?t=rsc3&e=help/graph/rd-discretion.scm)
- [rd-eggcrate.scm](?t=rsc3&e=help/graph/rd-eggcrate.scm)
- [rd-f-lets.scm](?t=rsc3&e=help/graph/rd-f-lets.scm)
- [rd-fm-iter.scm](?t=rsc3&e=help/graph/rd-fm-iter.scm)
- [rd-fwalk.scm](?t=rsc3&e=help/graph/rd-fwalk.scm)
- [rd-h-chatter.scm](?t=rsc3&e=help/graph/rd-h-chatter.scm)
- [rd-implosion.scm](?t=rsc3&e=help/graph/rd-implosion.scm)
- [rd-klink.scm](?t=rsc3&e=help/graph/rd-klink.scm)
- [rd-k-ppr.scm](?t=rsc3&e=help/graph/rd-k-ppr.scm)
- [rd-lf-pulses.scm](?t=rsc3&e=help/graph/rd-lf-pulses.scm)
- [rd-lg-timed.scm](?t=rsc3&e=help/graph/rd-lg-timed.scm)
- [rd-lz-bf.scm](?t=rsc3&e=help/graph/rd-lz-bf.scm)
- [rd-mouse-clatter.scm](?t=rsc3&e=help/graph/rd-mouse-clatter.scm)
- [rd-oscillator-cluster.scm](?t=rsc3&e=help/graph/rd-oscillator-cluster.scm)
- [rd-prts.scm](?t=rsc3&e=help/graph/rd-prts.scm)
- [rd-rzblp.scm](?t=rsc3&e=help/graph/rd-rzblp.scm)
- [rd-s-chirp.scm](?t=rsc3&e=help/graph/rd-s-chirp.scm)
- [rd-shifting-pulses.scm](?t=rsc3&e=help/graph/rd-shifting-pulses.scm)
- [rd-slly-wlk.scm](?t=rsc3&e=help/graph/rd-slly-wlk.scm)
- [rd-sosc-lp.scm](?t=rsc3&e=help/graph/rd-sosc-lp.scm)
- [rd-tgb.scm](?t=rsc3&e=help/graph/rd-tgb.scm)
- [rd-three-cpsw.scm](?t=rsc3&e=help/graph/rd-three-cpsw.scm)
- [rd-tipnso.scm](?t=rsc3&e=help/graph/rd-tipnso.scm)
- [rd-trmlo.scm](?t=rsc3&e=help/graph/rd-trmlo.scm)
- [rd-trkl.scm](?t=rsc3&e=help/graph/rd-trkl.scm)
- [rd-tr-out.scm](?t=rsc3&e=help/graph/rd-tr-out.scm)
- [rd-wial.scm](?t=rsc3&e=help/graph/rd-wial.scm)
- [rd-xy-interference.scm](?t=rsc3&e=help/graph/rd-xy-interference.scm)
- [tm-drummer.scm](?t=rsc3&e=help/graph/tm-drummer.scm)

<!--
- [alien-froggies.scm](?t=rsc3&e=help/graph/alien-froggies.scm)
- [alien-meadow.scm](?t=rsc3&e=help/graph/alien-meadow.scm)
- [blips-001.scm](?t=rsc3&e=help/graph/blips-001.scm)
- [bottle.scm](?t=rsc3&e=help/graph/bottle.scm)
- [bouncing-objects.scm](?t=rsc3&e=help/graph/bouncing-objects.scm)
- [bowed-string.scm](?t=rsc3&e=help/graph/bowed-string.scm)
- [choip.scm](?t=rsc3&e=help/graph/choip.scm)
- [cymbalism-accelerando.scm](?t=rsc3&e=help/graph/cymbalism-accelerando.scm)
- [cymbalism.scm](?t=rsc3&e=help/graph/cymbalism.scm)
- [hard-sync-sawtooth-with-lfo.scm](?t=rsc3&e=help/graph/hard-sync-sawtooth-with-lfo.scm)
- [hell-is-busy.scm](?t=rsc3&e=help/graph/hell-is-busy.scm)
- [karplus-strong.scm](?t=rsc3&e=help/graph/karplus-strong.scm)
- [lots-o-sines.scm](?t=rsc3&e=help/graph/lots-o-sines.scm)
- [narrow-band-filtered-crackle-noise.scm](?t=rsc3&e=help/graph/narrow-band-filtered-crackle-noise.scm)
- [nharm.scm](?t=rsc3&e=help/graph/nharm.scm)
- [pattern-buffer.scm](?t=rsc3&e=help/graph/pattern-buffer.scm)
- [pond-life.scm](?t=rsc3&e=help/graph/pond-life.scm)
- [rails.scm](?t=rsc3&e=help/graph/rails.scm)
- [random-sine-waves.scm](?t=rsc3&e=help/graph/random-sine-waves.scm)
- [record-scratcher.scm](?t=rsc3&e=help/graph/record-scratcher.scm)
- [red-frik.scm](?t=rsc3&e=help/graph/red-frik.scm)
- [reset.scm](?t=rsc3&e=help/graph/reset.scm)
- [resonators-harmonic-series.scm](?t=rsc3&e=help/graph/resonators-harmonic-series.scm)
- [reso-pulse.scm](?t=rsc3&e=help/graph/reso-pulse.scm)
- [ring-modulated-klank.scm](?t=rsc3&e=help/graph/ring-modulated-klank.scm)
- [scritto.scm](?t=rsc3&e=help/graph/scritto.scm)
- [seqr.scm](?t=rsc3&e=help/graph/seqr.scm)
- [status.scm](?t=rsc3&e=help/graph/status.scm)
- [strtchd-scrmbld.scm](?t=rsc3&e=help/graph/strtchd-scrmbld.scm)
- [swept-resonant-noise.scm](?t=rsc3&e=help/graph/swept-resonant-noise.scm)
- [tgr-rpr.scm](?t=rsc3&e=help/graph/tgr-rpr.scm)
- [tremulate.scm](?t=rsc3&e=help/graph/tremulate.scm)
- [uplink.scm](?t=rsc3&e=help/graph/uplink.scm)
- [voscil.scm](?t=rsc3&e=help/graph/voscil.scm)
- [wind-metals.scm](?t=rsc3&e=help/graph/wind-metals.scm)
-->

There is also an haskell to lisp translator (October, 2014) that re-writes a
[subset](http://rohandrape.net/?t=hsc3-lisp&e=md/sexp.md) of haskell as lisp.

## Lisp

The largest part of the lisp (scheme) sources are directly copied from `rhs` and `rsc3`,
see the `Makefile` at the `scm` directory.  These files are marked `AUTOGENERATED`.

- [hsc3.scm](?t=hsc3-lisp&e=scm/hsc3.scm)
- [rhs.scm](?t=hsc3-lisp&e=scm/rhs.scm)
- [rhs.syntax.scm](?t=hsc3-lisp&e=scm/rhs.syntax.scm)
- [rsc3.scm](?t=hsc3-lisp&e=scm/rsc3.scm)
- [scheme.scm](?t=hsc3-lisp&e=scm/scheme.scm)
- [stdlib.scm](?t=hsc3-lisp&e=scm/stdlib.scm)
- [ugen.scm](?t=hsc3-lisp&e=scm/ugen.scm)

## Help

<!-- - [jmcc.scm](?t=hsc3-lisp&e=help/jmcc.scm) -->

- [tutorial.scm](?t=hsc3-lisp&e=help/tutorial.scm)
- [r5rs.scm](?t=hsc3-lisp&e=help/r5rs.scm)

initial announcement:
[[haskell-art](http://lurk.org/groups/haskell-art/messages/topic/5i1PSjCHQvVQwcAcLhUaE6/),
 [local](?t=hsc3-lisp&e=help/announce.text)]
2014-10-15 05:10:19 GMT

<!--
[bham](http://www.listarc.bham.ac.uk/lists/sc-users/msg42056.html)
[gmane](http://article.gmane.org/gmane.comp.lang.haskell.art/1026)
-->

© [rohan drape](http://rohandrape.net/), 2014-2024, [gpl](http://gnu.org/copyleft/).

* * *

```
$ make doctest
Examples: 102  Tried: 102  Errors: 0  Failures: 0
$
```
