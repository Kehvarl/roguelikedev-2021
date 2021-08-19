;;;; roguelike-2021.asd

(asdf:defsystem #:roguelike-2021
  :description "Unnamed Roguelike for the 2021 r/RogueLikeDev Roguelike Tutorial"
  :author "Kehvarl <Kehvarl@Kehvarl.com>"
  :license  ""
  :version "0.0.2"
  :serial t
  :depends-on (#:cl-blt #:queues.priority-queue)
  :components ((:file "package")
               (:file "rect")
               (:file "tile")
               (:file "ui")
               (:file "menu")
               (:file "render-functions")
               (:file "pathfinding")
               (:file "game-map")
               (:file "map-placement")
               (:file "map-generator")
               (:file "entity")
               (:file "components")
               (:file "components-ai-basic")
               (:file "components-ai-dead")
               (:file "components-item")
               (:file "components-inventory")
               (:file "components-spawner")
               (:file "fov")
               (:file "game-states")
               (:file "death-functions")
               (:file "handle-keys")
               (:file "game-tick")
               (:file "roguelike-2021")))
