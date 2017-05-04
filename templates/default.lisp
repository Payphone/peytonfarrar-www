(defmacro default (&optional (title "") &body body)
  `(html
    (:head
     (:meta :htt-equiv "Content-Type" :content "text/html; charset=utf-8")
     (:meta :name "viewport" :content (conc "width=device-width,"
                                            "height=device-height"
                                            "initial-scale=0.86"))
     (:title ,title)
     (:link :href "/css/main.css" :rel "stylesheet" :type "text/css"))
    (:body
     (:div :class "row center header"
           (:h2 :class "col c3" (:a :href "/docs" "Docs"))
           (:h2 :class "col c6" (:a :href "/" "Peyton Farrar"))
           (:h2 :class "col c3" (:a :href "https://github.com/Payphone"
                                    "Github")))
     ,@body)))
