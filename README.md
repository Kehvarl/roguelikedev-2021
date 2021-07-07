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


## Part 1
### Drawing the Screen
![Part 1.0](./screenshots/Part1.0.png?raw=true "Game Window")
* Initialize BearLibTerminal and display a window with title
* Capture keystrokes and check for game-over condition.
## Drawing the Player and Moving around
![Part 1.1](./screenshots/Part1.1.png?raw=true "Player On Screen")
* Update Drawing routine to show the player '@'
* Update Drawing routing to accept location to draw player
* Break out key-handler to its own function
* Update Main loop to track player location and feed to drawing routing
* Update key-handler to capture movement keys and feed back to main loop.

## Part 2
### The entity
![Part 2.1](./screenshots/Part2.1.png?raw=true "Entities and NPCs")
#### Defining the Entity
* Create a generic Entity object to track entities
  * Position
  * color
  * Symbol
#### Using the Entity
* Turn the Player object into an entity
* Update the Draw routine to use Entities.
* Update Main loop to use entities and new Draw routine.
#### Better Rendering
* Render many entities using our new Draw method for generic entities.
#### Smarter movement
* Move our movement routine out of the Main Loop and into our Entity
#### Adding a Friend
* Created a second Entity, not in control of the player.
#### Cleaning up our Main file
* Move all the new Entity methods to their own file
* Add the new Entity file to our ASD to make sure it gets loaded.
### The Map
![Part 2.2](./screenshots/Part2.2.png?raw=true "The Game Map")
#### Define Map Components
* Create a new game-map file and add it to our ASD so it will get loaded.
* Define a Tile which is a game-map space that may or may not be a wall
#### Define game-map
* Create a game-map object which holds an array of Tiles to represent our map
* Initialize an empty map and set a few walls for demonstration purposes
#### Rendering the game-map
* Create some colors to identify map tile states
* Update our render-all function to draw map Tiles
* Update our main loop with a new map (don't forget to initialize it)
#### Cleaning up our Main file
* Create a file to hold our render-functions
  * Dont' forget to put `(in-package #:roguelike-2021)` at the top
* Add our new file to our ASD for loading.
  * Make sure it comes after anything it needs and before anything that uses it
#### Fixing our movement
* We can walk through walls, let's fix that!
* Add a method to game-map to check if a given x/y position is a wall.
* Update the "move" section of our main loop to check for a wall before we walk into it

## Part 3
### Looping over Tiles
We will be looping over the entire map (or subsets of it) quite often.  To facilitate this, we will create a macro to build the looping functions we need.
* Cleanup our Initialize loop to concisely state what we need it to do
* Initialize all tiles to walls while we're at it.
* Create a macro that generates the tiles-loop we used for Initializing
* Re-implement our initialization in terms of our new macro
### Defining Rooms and Tunnels.
* Create a class to identify simple rectangular rooms.
* Draw a few on our map
* Create some mechanism to carve tunnels between our Rooms
