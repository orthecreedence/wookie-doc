(in-package :wookie-doc)

(deflayout default (data :top-level t)
  (:html
    (:head
      (:title (str (getf data :title)))
      (:link :rel "stylesheet" :href "/css/template.css")
      (:link :rel "stylesheet" :href "/js/highlight.js/styles/zenburn.css")
      (:script :src "/js/mootools-1.4.1.js")
      (:script :src "/js/highlight.js/highlight.pack.js")
      (:script :src "/js/wookie.js"))
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
          (:p "&copy; Lyon Bros. Enterprises, LLC"))))))


