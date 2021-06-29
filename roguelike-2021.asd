;;;; roguelike-2021.asd

(asdf:defsystem #:roguelike-2021
  :description "Unnamed Roguelike for the 2021 r/RogueLikeDev Roguelike Tutorial"
  :author "Kehvarl <Kehvarl@Kehvarl.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt)
  :components ((:file "package")
               (:file "roguelike-2021")))
