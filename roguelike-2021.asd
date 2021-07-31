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
               (:file "game-states")
               (:file "game-map")
               (:file "pathfinding")
               (:file "entity")
               (:file "components")
               (:file "components-item")
               (:file "map-generator")
               (:file "fov")
               (:file "ui")
               (:file "menu")
               (:file "render-functions")
               (:file "handle-keys")
               (:file "game-tick")
               (:file "death-functions")
               (:file "roguelike-2021")))
