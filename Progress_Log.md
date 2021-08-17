
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
* Next we update our AI's "take-turn" method to move towards the player, and try to attack if the enemy is close enough to a target.
* Finally we update our Game-Tick to feed all the essential data to Take-Turn.
* Running the game now, we see our enemies give chase!  Though, with their simplistic movement, they can get stuck easily.
#### Player Improvements
* Monsters are a bit more maneuverable than we are, let's fix that!  8-way movement can be implemented by assigning some new movement keys, and usually we'd be using the vi keys (hjkl + yubn).  I personally never found these to be intuitive or ergonomic, so let's see how we can improve on them!
  * QWE/A D/ZXC ->  really awkward on the left hand, so let's not go there if we dont' have to.
  * UIO/J L/M<> -> this seems doable, I actually find myself reaching for NM<m, but that might just be me.   This also leaves K for some special purpose, and P:? under your pinky, ready to use.
  * Numpad ->  This would be ideal, except I'm on a tenkeyless keyboard, as are many people I know.
#### Monster movement
* Our monsters just move in a straight line, so it's easy to trap them in places they can't move.   To fix that let's implement the AStar pathfinding algorithm and have our creatures use that to make their way around the map.
* Once we have a workable pathfinding solution, let's update our Entity's "move-towards" function to use that to help find the way to our target.
* Don't forget to add Pathfinding to our ASD so it loads properly.
### Doing Damage
![Part 6.2](./screenshots/Part6.2.png?raw=true "WHAM! POW! BLAM!")

#### Damage and Combat
And now we move from trading insults to trading blows.
* First up is some necessary changes to our Fighter component.
  * We'll add a function to deal damage
  * And another for straight-forward attacks.
* Then we'll modify our game-tick to use this new attack feature when we bump into an enemy.
* For fairness, we'll update the enemies' AI to attack instead of insulting us.
#### Messages, Death, and Corpses
![Part 6.3](./screenshots/Part6.3.png?raw=true "Let the dead find their rest as a messy pile!")

We can hit things, things can hit us, and we can even brag about the relative amounts of damage!  Time to do something with all this.
* Instead of printing, our take-damage and attack methods should return some information about what happened, so the game logic can do useful things for us.
* With all these useful messages flowing back to our game loop, let's modify it to display our messages and do something when things die!
* Now that our game loop has some information to work with (and we're printing the messages that look exactly the same as before), let's decide what to do with these immortal dead things!
  * We'll create a new file (don't forget to in-package and add it to our ASD), and define some nice tools for dealing with bodies.
  * Kill-player will turn you Red and set some flags so we can end the game
  * kill-monster will Print a nice victory message, turn the monster Red, and make it stop trying to do stuff.
* With those helpful utilities in place, we'll update our game-tick function to use them when things die.

## Part 7 (UI Stuff)
![Part 7.1](./screenshots/Part7.1.png?raw=true "Showing off our HP!")

### Player Health
We can deal and take damage, but right now we have no way of knowing just how badly injured we are.   We're going to tackle this in stages!  First up, just a crude presentation of our current HP.
* We'll hack our Render-All function to accept the player and display the HP and Max-HP values from that entity.
### Emergency Recompile of our source libraries
Well that was an exciting diversion!  It turns out that cl-blt can't print with the pre-compiled version of BearLibTerminal.  The fix wasn't too hard, so here's some instructions if you're following along:
* `git clone https://github.com/tommyettinger/BearLibTerminal.git`  to grab the latest source code for BearLibTerminal
* `sudo apt install build-essential cmake freeglut3-dev`  This will give us the stuff we need to compile BearLibTerminal from scratch, including the openGL headers we will need.   NOTE:  This is on Ubuntu Linux 21.04.  Your own computer might be different.
* `cd BearLibTerminal` Or to wheresoever you put your cloned copy.
* `cmake .`  Build the things that tell it how to build the thing!
* `make` Now actually build the thing!
* `sudo cp Terminal/Output/Linux64/libBearLibTerminal.so /usr/lib/libBearLibTerminal.so` Put the newly crafted BearLibTerminal in the place that Lisp knows to look for it.
* Now, RESTART your lisp REPL.  I'm sure there's a better way, but for me this is the only thing that worked.
* `(ql:quickload :roguelikedev-2021)`  Load our project
* `(in-package :roguelikedev-2021)`    Work within our Project
* `(main)`  Make sure it runs now!
### Player Health Try 2. Or: Back to our code already in progress.
* It seems to be working now, though I missed a step earlier.  I'm never setting the Max-HP of my entities, so my display is wrong.   Easy fix though!
  * Pop into components, and create an initialize-instance function to set max-HP if it's not provided.
### Rendering Order
![Part 7.2](./screenshots/Part7.2.png?raw=true "Walking all over them!")

Everything just gets drawn on screen in the order it's added to the Entities list.  Unfortunately, Player comes first, so we get covered with all sorts of unpleasantness if we share a tile with a body.    To fix that, let's add a concept of render order and make sure the dead come last when we do that.
* First off, we'll create a render-order global in our rendering-functions file.
* We'll define 3 types of renderable entities:  Corpses, Items, and Actors.  Lower on the list means it gets drawn first, so it's covered up by other stuff.
* Entities need to know their render-order, so let's modify them to add a new slot.  We'll default everything to the "corpse" render layer.
* Set the various render orders, and remember that death changes the order.
* Create a utility to sort entities by their render order.
* Actually use our new sort function in our rendering pipeline.
* And we're done!  You can now walk _on top_ of corpses!
### Healthbar
![Part 7.3](./screenshots/Part7.3.png?raw=true "Healthbar: The New sensation!")

Our little HP tracker is somewhat boring.  To make it more exciting we're going to turn it into a health bar that shrinks as our Player loses HP. Rather than just draw this wherever, we're going to divide our screen into a series of "Panels" to simplify the display of information.
* First up, we'll create a new "UI" file (and register it in our ASD), then do some work to define this Panel concept.
  * While we're at it, we created a panel-component class to store things that go inside panels, and we created a quick little function to create panels for us.
* Now we can design a component for our healthbars.
  * In our UI file, create a class for "bar" which has a name, and some settings for what it holds.
  * While we're there, let's write up some tools to render panels and bars.   And make sure the panel renderer calls render on all its components.
* Let's draw some panels.  We'll start by creating our main panel and health bar over in Main, then pop into game-tick to handle more of what we do.  Mostly just passing everything along to Render-All to do the real magic.
### Interlude
Somewhere I have introduced a new bug!  Our corpses are disappearing instead of remaining on screen.  It worked fine before the introduction of the Panels, so I suspect it's a simple matter of a change in the renderer.

The bug only activates if the first creature I kill is a Troll. It sometimes stops activating after I kill a few Orcs.

Based on some work I did after setting a Break in my code, it appears that corpses just occasionally vanish from my entity list.  This bears further investigation.
### Messages
![Part 7.4](./screenshots/Part7.4.png?raw=true "We've got Fun and Games")

* We'll start out by creating some classes in our UI file (Though I really feel this should be its own file since there's so much going on)
  * First up a Message Log which is a Panel-Component just like our healthbar is.
  * Next a message to store in our log.
* Now that we have those, we need to manipulate them, so there's some useful tools as Well
  * A way to initialize our log.
  * A way to add messages to the log.
  * Some simple word-wrapping to make sure that text actually fits.
  * A render function that knows how to draw the contents of a log.
* Penultimately, we create a message log back in our Main function.
  * Let's add a test message too.
* ![Part 7.5](./screenshots/Part7.5.png?raw=true "Logging our Messages")  

* Finally finally, we replace our format statements with add-to-log statements
  * Make sure we pass our new message-log into game-tick, and use that when we append messages, otherwise we won't actually have a working solution.
### Entities under the cursor
![Part 7.6](./screenshots/Part7.6.png?raw=true "Look around with your mouse.")  

Let's use that silly mouse that all our players have on their desk!
* We'll start off by configuring BLT to watch the mouse movements.
* Then we create a function that will scan our list of entities and find everything under the mouse position.
* And lastly we update Render-All to print that list in our message box.

## Part 8
### Items and Inventory
![Part 8.1](./screenshots/Part8.1.png?raw=true "Bottles bottles everywhere!")  

Now that our player can explore the map, encounter strange and unusual creatures, fight them to the death, and be told about it in glorious messages, it's time to add some stuff for them to pick up
#### Placing Items
* For starters let's scatter some healing potions around the map.
* Alongside our place monsters, it's time to create a place-items function we'll call during map generation.
* While we're at it, let's move monsters out of "entities" and into their own generator.
#### Picking things up
* Now that we can scatter items like a crazed animal, let's find a way to pick them up!
* First we'll define a couple of new components:  Inventory and Item
* Then we'll add slots for these to our Entity.
* And initialize an Inventory component in our Player entity so we can eventually pick stuff up.
* Next up we'll make sure those potions we're dropping all over the place have an Item component so we can use them later.
* We'll give the player a "Get" key mapped to 'G', which sends the Pickup message to our game loop.
* The inventory class will need a way for us to add items to it, so let's implement that Now
##### Simplifying Game Tick
Our game-tick class is getting very complicated, so while we're hacking on the player code, let's go ahead and move that to its own function to streamline things a little.
* define Player-Turn in the file with Game-Tick
* Some of the changes don't make sense yet, but we'll implement them later!
#### Picking things up II
* With the groundwork laid, we can now actually grab those bottles.
* Modify Player-Turn with a "when pickup" block that adds the item to your inventory.
#### Backtracking
* The tutorial we've been following actually made a big change a few parts back, but they weren't well documented.
  * Instead of being an Enum, GameState is a class which tracks several things:
  * The current-turn/game-state
  * The previous state (for use with menus)
  * The list of entities
  * A flag to track if the game is Running
* Given how much this class does that is impacted by our current changes, it behooves us to implement it now and get things working with as little fuss as possible.
* First we'll create a new file (game-state.lisp), and be sure to in-package it and add it to our ASD right before the main file.
* Next up we define a class for game-state and some slots to track our important information
* When we initialize game-state, if we don't set the previous state, let's make sure to set it to our starting state.
### Inventory menu
#### Drawing the menu
![Part 8.2](./screenshots/Part8.2.png?raw=true "I've been down to the bottom of every bottle")

We have a working game again, and we can pick up items that are lying around!  Let's do something with those items.
* First off, create a new menus.lisp file and in-package it.  Add to our ASD tool
* In our menu, let's set up a quick menu rendering tool.
* Let's also create a function to make a menu using our inventory.
* Now that we can draw menus (in theory), let's set up a key to draw them.
* Update player-turn to handle that new key and do something useful.  (set our game-state to the inventory state)
* Now jump to our tick and decide what to do with that new state!
  * What we decided is that if you press "esc" while in the menu it exits the menu and resumes the game.
* For actually showing our menu, we'll pass our game-state to the renderer and let it decide what to show.
* We now nicely list items in our inventory!  Of course nothing happens when you try to choose one.  That's next!
#### Using the menu
![Part 8.3](./screenshots/Part8.3.png?raw=true "Making bad choices")

We can display neat menus and show off the player's inventory (as long as it's 26 items or less).  Now let's use those items!
* First up we'll move our key handler to a new file!
* Then modify our key handler based on our game-state.
* Now we'll create a new key-handler for the player-dead state.
* Next up we'll create a function to handle those pesky inventory keys
* With our key handler doing something useful and telling us what the player picked, let's jump back to the game-tick and do something with that value.
  * What we choose to do at the moment is print the item to the REPL.
#### Using the menu revisited
![Part 8.4](./screenshots/Part8.4.png?raw=true "Quaffing and Feeling Great.")

* To really use our items, let's put a use-item function into them!
  * We'll define a slot that can hold a function for later use, and another slot with arguments for that function.
  * As before, when things get complicated the complicated make more files!
  * Per the tutorial I should actually put the functions in a new file and leave the item component in components.  I'm not doing either of those things!  Item functions in the Components-Item file!
  * Now that we have a way to heal, let's apply it to those purple potions prominently placed!
#### Dropping Items
![Part 8.5](./screenshots/Part8.5.png?raw=true "Just put that down anywhere.")

* Next up we're going to add a way to make space in your inventory:  dropping the junk!
* We'll start off my taking another look at our key handler, and adding a "drop" command.
* With our keys handled for entering the state, let's add a clause to the player-tick to jump into an inventory-drop state.
* Now in the inventory-drop state, we'll make sure to re-use the show-inventory key handler since it already lets us pick any item from our carried stuff (Is it weird that we have 26 hands?).
* If our game-tick can't kick us out of drop-inventory, make sure that happens now.  It's one line that I added a bit ahead of schedule.
* Next up we hack our renderer to show the menu title with "drop" instead of "use".
* Our inventory needs to know how to drop items, so let's add a method for that. (Once more, I think I'll split to a new file.  More files more better, right?
* Now our game-tick can do something with that drop action, and put things back on the map.
* I actually had to jump back to the Entity definition and remember to set inventory/owner during initialization.
* Now we can scatter our wild oats!  Or at least those potion bottles we keep finding.

## Part 9
### Off on our own:  Or how I panicked when I didn't have any more tutorial to follow.
The cl-rltut ends with inventory.  Ideally we'd follow the libtcod tutorial here, and get things in place for magic, mayhem, and remote targeting.
Instead, I want to try to implement some new feature that I've teased people with on the reddit
#### The AI From Beyond The Grave
Instead of removing the AI from dead creatures, we're going to set up 2 new AIs, and some new action rules
* New action Rule: Each AI will have an "Active Range".  If it's within that radius of the player, then it can act, even if it can't see the player.
  * For this, let's dive into our Basic Monster ai and see how action is currently handled.
  * Take-Turn checks to see if the monster is currently in a square the player can see.  So let's change things a bit!
  * First we'll add an Activation Range slot to our AI and set it during creation.
  * Now we'll modify take-turn to wander if we're in range, but attack if we're in sight.
  * My wandering breaks things, but if I just always move towards the player there's a bit of unpleasantness.
* New AI: Dead
![Part 9.1](./screenshots/Part9.1.png?raw=true "Self-cleaning corpses.")

  * The Dead AI basically just counts down through some states of decay and ends with the monster being removed from the map.
  * Changes made to a large number of files for this one.
  * First we broke AI out into separate files
  * Then when a creature dies we create a new "Dead-Monster" AI component for it.
  * Each tick, if the monster is in range of the player, it slowly decays, moving through phases until it fades away.
* New AI:  Regenerating.   A Dead Troll has a chance each turn of gaining back 1 HP.  If it regains 5, it returns to life.
![Part 9.2](./screenshots/Part9.2.png?raw=true "The Walking Trolls.")

  * I just made it 100% certain that the troll would regain HP or decay every 5 turns.   with 5 HP it becomes a Risen Troll.
  * We now have generic Decay and Regenerate features for dead monsters.  And a Resurrect function in the Death functions.  
  * I may give Live trolls a regeneration feature too.  Might be time to strip some of this out of hard-code and into configurable settings.
  * Dead monsters track some of the state of their past life
  * On resurrect, we put those settings back into place
### Scent trails
![Part 9.3](./screenshots/Part9.3.png?raw=true "Getting Warmer.")

  As the player walks, they will lay down a scent trail which mostly follows a logical progression.   If they cross their own trail, the value resets so the trails are vaguely branching.
  * Update Tile to hold a Track value
  * Create a new Player class descending from Entity.  Which is an Entity with a Track score
  * Update the player Move to increment the player's Track and set the map tile's Track
  * Temporary renderer hack to display the current track using HSV values to make them look cool.

## AI Tracking
We have some interesting features in place, now let's make some more interesting AI.  I really have a few types I want, I think:
* Following:  This AI will try to follow the player, but never approach closer than some Distance
* Tracking:  This AI will try to follow the player.  
  * If the player is in sight, it will move towards them normally.
  * If the player is out of sight, and if the AI can find the scent trail, it will move towards the player
  * Otherwise the AI will move randomly
* TODO
  * Fleeing: This AI will attempt to evade the player.

### Definitely not AI Tracking, but in that branch
![Part A.1](./screenshots/PartA.1.png?raw=true "Vermin! Vermin everywhere!")

Dead Bodies are still a source of consternation and threat!  Some may rise again, gaining in power with each undeath.  Others will bring forth hordes of rats or other foul carrion eaters.
* Spawners now exist.  
  * Currently they just fill rooms with potions.
  * Added per-room limits on the number of items a spawner will produce
* Random corpses may become vermin spawners at any point in their decay (5 percent chance per decay state)
  * Decay state gets put on hold and.
  * Monster gets a spawning component
  * Will attempt to spawn vermin somewhere in the same Region

### Better Region Identification
* Rather than keeping a list of rooms around, assign every tile a number based on its Room-ness or Corridor-ness.
* Any creatures on a tile with a shared number are in the same room.
  * Implemented an entities-in-region method on Game-Map

### Refactor/Cleanup
* Did some slight refactor to use Region instead of room in places.
* Need to review modules and put components where they belong.
* Very late in development to change the approach to packaging and dependencies, but that might be worthwhile or at least good to check into for the rewrite/tutorial.

### Doors
![Part A.2](./screenshots/PartA.2.png?raw=true "Debugging Corridors and Doors")
* We will fiddle with implementing doors.
  * A Corridor tile with a Room on one side and a Corridor on the other is a door
  * A Door is an Entity in a tile.  With a Door Component
  * Doors may be one of
    * Opening - always allow passage
    * Secret - Act like a wall most of the time
    * Open - allow passage, but can be closed (should have special character)
    * Closed - Act like a wall, but can be opened (should have special character)
* When we carve our corridors, we will flag the doors.  A door will be the first and last tile carved that were not rooms.
* A door is an entity that might block passage.
![Part A.3](./screenshots/PartA.3.png?raw=true "Cell doors block passage but not vision.")
  * Right now they're yellow.  We need a brown color.
  * We also need a door-state state.  Maybe a door component.
  * Door Component tracks door state.  
  * Doors block all passage
  * Bumping into a closed door opens it.

### Scoring
* Implemented a counter for number of turns played.

## Ideas awaiting experimentation
* Dijkstra Map Stuff
  * Use to have creatures move towards goals
  * Useful to maintain distance from the player or other Entity
* Monster Spawners
* Monster mutation
* Creature mutation
  * Ritual circles or Matter transmuters that apply random effects to entities that stand in them

## Necessary Features
* Levels
  * Need to be able to descend deeper into the game with monsters becoming more difficult as you do.
  * Would be nice to ascend back to the same level you were in previously
* Saving
  * Need to store the entire game-state and recover it later
  * Game-state includes:
    * Player with all components and Inventory
    * All entities, their current status and inventory and goals
    * The game map
      * Might be worthwhile to divorce exploration and visibility from the actual map
    * The current game-state
    * All of the above for all levels
* More Enemies
  * We need more enemies to scatter around the Levels
  * Enemies need a per-level frequency
  * Special enemies (Bosses) need to be introduced
  * Enemies should be pulled out of hard-code and defined by data.
* More items
  * We need items that do more than just heal
  * Equipable items that grant continuous bonuses
    * Power
    * Damage
    * Bonus attacks
    * Bonus effects on wearer
    * Effects on Target
  * Ranged effects are needed along with ways to select the target.
* Intro screen
* Main Menu
* Menu states and transitions
* Advancement.
  * How do we improve the player to survive greater and greater threats
  * Do we allow XP and leveling
  * Do we add skills and train them automatically
  * Is equipment the only boost
  * Are their things in game that can grant permanent increases
* Scoring
  * How do we track player score
* Entity actions
  * Is everything either an item or an Enemy
  * Do enemies have different goals in regards to the player
  * Are some enemies hostile towards each other
  * Does the game continue to only simulate a radius around the player, or the entire level
