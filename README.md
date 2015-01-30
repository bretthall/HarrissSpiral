# HarrissSpiral

This is a simple GUI program that renders the [Harriss Spiral][blogpost]. It renders the spiral one *generation* at a time so that one can follow the curves construction. If you just want a pretty curve then put 10 or higher in the *generation* box in the control panel (careful that you don't enter too high a number or you may be waiting a while for the spiral to render). 

[blogpost]:???

The GUI program requires [gtk2hs](http://projects.haskell.org/gtk2hs/) and [lenses](https://hackage.haskell.org/package/lens) in order to build. Once you have both of those packages installed you just issue 

   ghc --make HarrissSpiral

to build.