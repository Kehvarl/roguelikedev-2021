;;;; roguelike-2021.asd

(asdf:defsystem #:roguelike-2021
  :description "Unnamed Roguelike for the 2021 r/RogueLikeDev Roguelike Tutorial"
  :author "Kehvarl <Kehvarl@Kehvarl.com>"
  :license  ""
  :version "0.0.2"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "rect")
               (:file "tile")
               (:file "game-map")
               (:file "map-generator")
               (:file "entity") ;; Entity now relies on game-map
               (:file "fov")
               (:file "render-functions")
               (:file "game-tick")
               (:file "roguelike-2021")))
