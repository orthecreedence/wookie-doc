(in-package :wookie-doc)

(deflayout default (data :top-level t)
  (:html
    (:head
      (:title (str (conc (getf data :title) " | Wookie")))
      (:link :rel "stylesheet" :href "/css/template.css")
      (:script :src "/js/mootools-1.4.1.js")
      (:script :src "/js/wookie.js")
      ;; syntax highlighting
      (:link :rel "stylesheet" :href "/js/highlight-lisp/themes/wookie.css" :id "hl-style")
      (:script :src "/js/highlight-lisp/highlight-lisp.js"))
    (:body
      (:div :id "wookie" (:a :href "/" "&nbsp;"))
      (:div :id "container"
        (:header
          (:h1 (:a :href "/" "Wookie"))
          (:nav
            (:ul
              (:li (:a :href "/" "Home"))
              (:li (:a :href "/docs" "Documentation"))
              (:li (:a :href "/guide" "Guide"))
              ;(:li (:a :href "/photos" "Photos"))
              ;(:li (:a :href "/refresh-views" "(reset views)"))
              )))
        (:content
          (:div :class "gutter clear"
            (str (getf data :content))))
        
        (:footer
          (:ul
            (:li (:a :href "/" "Wookie home"))
            (:li (:a :href "https://github.com/orthecreedence/wookie" "github")))
          (:p "&copy;" (:a :href "http://lyonbros.com" "Lyon Bros. Enterprises, LLC")))))))


