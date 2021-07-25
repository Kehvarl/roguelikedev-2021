# r/RoguelikeDev Does The Complete Roguelike Tutorial

![RoguelikeDev Does the Complete Roguelike Tutorial Event Logo](https://imgur.com/xSph4zw.png)

At [r/roguelikedev](https://www.reddit.com/r/roguelikedev/) we're doing a dev-along following [The Complete Roguelike Tutorial](http://rogueliketutorials.com/libtcod/1).
This version will be following the [Common Lisp Roguelike Tutorial (CL-RLTUT)](https://nwforrer.github.io/categories/roguelike-tutorial/).

## Roguelike 2021
By: _Kehvarl <Kehvarl@Kehvarl.com>_
This roguelike is going to follow the Common-Lisp + BearLibTerminal tutorial through at least Chapter 6.


## Part 0
### Project Setup
* Install Common Lisp (SBCL)
* Install the editor of your choice
  * In this instance I am using Atom with SLIMA
* Install Quicklisp (https://www.quicklisp.org/beta/)
* Install QuickProject `(ql:quickload :quickproject)`
* Create Lisp Project `(quickproject:make-project #p"~/roguelikedev-2021" :depends-on '(cl-blt))`
* Install BeatLibTerminal (https://github.com/tommyettinger/BearLibTerminal)
  * Download and unpack latest release
  * Copy libBearLibTerminal.so to /usr/lib/libBearLibTerminal.so
* Download cl-blt and add to quicklisp local-projects
  * `git clone https://github.com/sjl/cl-blt.git ~/quicklisp/local-projects/cl-blt`
* Symlink your new project into quicklisp's local-projects
  * `ln -s ~/roguelikedev-2021/ ~/quicklisp/local-projects/roguelikedev-2021`
* Attempt to load your package
  * `sbcl` ;; I used `rlwrap sbcl` to give me history in the repl
  * (ql:quickload :roguelikedev-2021)
  * (in-package :roguelikedev-2021)

**[Extensive Progress Log](Progress_Log.md)**
