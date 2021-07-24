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
               (:file "game-map")
               (:file "pathfinding")
               (:file "components")
               (:file "entity")
               (:file "map-generator")
               (:file "fov")
               (:file "render-functions")
               (:file "game-tick")
               (:file "death-functions")
               (:file "roguelike-2021")))
