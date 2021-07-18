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
![Part 3.1](./screenshots/Part3.1.png?raw=true "Random Maps")
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
* Create-Room tool will carve a Rect into our game-map
* Create a sample map with two rooms and use that from our main loop.
### Improve our Game loop
* Split our loop out into a "game-tick" function
* Update Main to feed initial state to our game-tick function.
* Strip out our global *map*
* Make a map initialize itself
### More useful maps
* Time to connect the rooms!
* We will first define a couple of functions to carve out a line of Tiles
* Then carve a corridor into our map!
### Dungeon Generation
* If we don't already have a function for overlapping rects, then lets create it
* We can also use a way to find the center of any room.
* Now we'll generate random rooms, and try to place them on the map!
* While we're at it, let's draw some connecting corridors

## Part 4
### Field of View
![Part 4.1](./screenshots/Part4.1.png?raw=true "Our Field Of View")
#### Calculating
* Per the CL-RLTUT tutorial, we will be implementing a very simple FoV algorithm
  * We will draw 360 lines from the character out to our desired radius.
  * As we draw each line, we will mark each tile it touches as "lit".
  * If we encounter a wall, we will mark the wall as "lit" and stop following the line.
* To support our lit/unlit state, we will add a slot to our Tile class.
* Since tile is becoming more complicated, I am going to move it out of the game-map file and into its own.
#### Rendering
* To render our calculated field of view, we'll need to modify our rendering routines to know what to do
  * First, we'll define some more colors to differentiate lit and unlit Tiles
  * Then we'll modify the Render-All function to draw those lit tiles in our fancy new color
  * Of course, our rendering tools don't know what to do if we don't calculate the FoV, so let's add that to our Game-Tick
### The Fog of War
![Part 4.2](./screenshots/Part4.2.png?raw=true "The Fog Of War")
#### Exploration
Currently the player can see the entire map.  That's no challenge at all! So what we will do is hide the map and only show the player what's in their field of view.  Just to be nice, we will also show the parts of the map the player has seen before, but we won't show any monsters or treasures lurking in those remembered places.
* First step: Add an "Explored" slot to our tiles.  That way we know what's safe to render.
* Then we'll update our FoV tool to mark places as explored as we encounter them.
#### Enemies
* Update our Draw routine to only show enemies inside our FoV.

## Part 5
### Placing Enemies
![Part 5.1](./screenshots/Part5.1.png?raw=true "A Well-Stocked Dungeon")
Now that there's a map to explore, and some mystery to the map, it's time to populate the dungeon!  This is most easily done ruing map generation when we can easily add stuff to the rooms.
* First we'll add a utility function to our game-map that checks if there's already an entity at a chosen x/y point.
* Then we'll add a method to our game-map that accepts a room and some constraints, then populates that room with some number of entities fitting our constraints.
  * Part of placing entities is finding a random spot in a room to place them.  I decided that finding that random point made more sense hiding inside Rect than in our Game-Map
* And then we just update our make-map routine so we can drop those random entities into our rooms as we make them.
### Colliding with enemies
![Part 5.2](./screenshots/Part5.2.png?raw=true "Win Friends and Kick Enemies")
Currently we can walk right through all the enemies on the map.  Since they won't move out of our way, this is very useful.  Unfortunately it's not ideal for the future of our game.
* Modify the Entity to hold a "blocks" slot
* Update entity creation to make our various entities solid
* Update our movement routine to check for solid creatures before we can enter a space on our map
  * This would be easier if we had an easy way to see if a given x/y location has an entity that blocks movement.  So let's add that function to our Game Map.
* While we're at it, let's tell you what you bumped into!
  * Back to the entity to add a Name slot.  Make sure to set it on entity creation.
  * Then update your move (over in Game-Tick) to print a message when you bump into an entity.
### Taking Turns
Let's give all those entities we're spawning a chance to do things!
* Create type to track our allowed game states
  * It can either be the player's turn or the list of Enemies' Turn
* Modify our Game-Tick to swap between states as appropriate
  * If it's the player Turn, and the Player has done something,
    * If they quit the game:  Set game-state to :EXIT
    * Otherwise Do the thing and set the game-state to :Enemy-Turn
  * If it's Enemy-Turn
    * Go through each entity and perform a placeholder
    * Set the Game-State to :Player-Turn
  * If it's EXIT
    * Return the :EXIT condition
* Modify the "do" loop in our Main function to track and pass Game-State to our Tick function.

## Interlude
* Doing some cleanup and re-organization so I can find stuff more easily.

## Part 6
### Combat!
![Part 6.1](./screenshots/Part6.1.png?raw=true "With a Fightin' Heart and Immortal Skill")
#### Components
We'll be using components within our entities to identify various capabilities, the first of which is "Fighter".  The Fighter Component grants: HP, Attack, and Defense.
* Create a new Components file, make sure to add it to our ASD and also put the proper (in-package...) at the top!
* The first thing we'll define is actually a generic Component class.  This holds the information that all components must have, which is basically just a reference to the entity that this component belongs to.
* Next up the Fighter.  This is a Component with HP, Max-HP, Attack, and Defense.
* A monster with HP but no AI seems silly, so we'll plan for that too!
  * Monster AI is just a component with a "take-turn" method.
* Now that we have a couple of components, lets' modify Entity so  we can use them.
  * Create a slot for each Component type in Entity
  * Make sure we initialize any existing components with the reference back to its owner.
* Update our player instance to have a Fighter component.
* Update our random monsters to have Fighter and AI components to make them more interesting.
* Now we need to update our game loop (Game-Tick if you forgot) to make use of the AI on our creatures!  Basically we just call "Take-turn" on anything that has an AI component.
#### Basic Monster AI
Now we make our enemies do stuff!
* To start with we need some features built into our entity class. Specifically a move-towards method that will handle pathfinding, and a distance-to that will see how far we are from a goal.
