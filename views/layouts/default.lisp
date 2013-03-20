(in-package :wookie-doc)

(deflayout default (data :top-level t)
  (:html
    (:head
      (:title (str (getf data :title)))
      (:link :rel "stylesheet" :href "/css/template.css")
      (:script :src "/js/mootools-1.4.1.js")
      (:script :src "/js/wookie.js")
      ;; syntax highlighting
      (:link :rel "stylesheet" :href "/js/syntaxhighlighter_3.0.83/styles/shCore.css")
      (:script :src "/js/syntaxhighlighter_3.0.83/scripts/shCore.js")
      (:script :src "/js/syntaxhighlighter_3.0.83/scripts/shLegacy.js")
      (:script :src "/js/syntaxhighlighter_3.0.83/scripts/shAutoloader.js")
      (:script :src "/js/syntaxhighlighter_3.0.83/scripts/shBrushLisp.js")
      ;(:link :rel "stylesheet" :href "/js/highlight.js/styles/zenburn.css")
      ;(:script :src "/js/highlight.js/highlight.pack.js")
      )
    (:body
      (:div :id "container"
        (:header
          (:h1 (:a :href "/" "Wookie"))
          (:nav
            (:ul
              (:li (:a :href "/" "Home"))
              (:li (:a :href "/docs" "Documentation"))
              (:li (:a :href "/photos" "Photos"))
              (:li (:a :href "/refresh-views" "(reset views)")))))
        (:content
          (:div :class "gutter"
            (str (getf data :content))))
        
        (:footer
          (:ul
            (:li (:a :href "/" "Wookie home"))
            (:li (:a :href "https://github.com/orthecreedence/wookie" "github")))
          (:p "&copy;" (:a :href "http://lyonbros.com" "Lyon Bros. Enterprises, LLC")))))))


