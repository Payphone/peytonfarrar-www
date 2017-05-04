(load-templates "default")

(default "Peyton Farrar"
    (:div :class "content center"
          (:h2 "About")
          (:p "A site dedicated to nothing in particular")
          (:h2 "Pages of Interest")
          (:ul
           (:li (:a :href "/docs" "Documents") " - Miscellaneous rambling"))
          (:h2 "Contact")
          (:p "Email: " (:a :href "mailto:peyton@peytonfarrar.com"
                            "Peyton@peytonfarrar.com"))))
