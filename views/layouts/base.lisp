(in-package :wookie-doc)

(deflayout base (data :top-level t)
  (:html
    (:head
      (:title (str (conc (getf data :title) " | Wookie")))
      (:link :rel "stylesheet" :href "/css/template.css")
      (:link :rel "stylesheet" :href "/css/modal.css")
      (:link :rel "shortcut icon" :href "/favicon.png" :type "image/png")
      (:script :src "/js/mootools-1.4.1.js")
      (:script :src "/js/modal.js")
      (:script :src "/js/wookie.js")
      ;; syntax highlighting
      (:link :rel "stylesheet" :href "/js/highlight-lisp/themes/wookie.css" :id "hl-style")
      (:script :src "/js/highlight-lisp/highlight-lisp.js"))
    (:body :class (getf data :body-class)
      (:div :id "wookie" (:a :href "/" "&nbsp;"))
      (:div :id "container"
        (:header
          (:div :class "inner"
            (:h1 (:a :href "/" "Wookie"))
            (:nav
              (:ul
                (:li (:a :href "/docs" "Docs"))
                (:li (:a :href "/faq" "FAQ"))
                ;(:li (:a :href "/apps" "Apps"))
                (:li (:a :href "/about" "About"))
                (:li (:a :href "https://github.com/orthecreedence/wookie" "Github"))
                ;(:li (:a :href "/guide" "Guide"))
                ))))
        (str (getf data :content))
        
        (:footer
          (:ul
            (:li (:a :href "/" "Wookie home"))
            (:li (:a :href "https://github.com/orthecreedence/wookie" "github"))
            (:li "Powered by Wookie"))
          (:p "MIT Licensed | &copy;" (:a :href "http://lyonbros.com" "Lyon Bros. Enterprises, LLC"))))
      (:script
        "
  var _paq = _paq || [];
  _paq.push([\"trackPageView\"]);
  _paq.push([\"enableLinkTracking\"]);

  (function() {
    var u=((\"https:\" == document.location.protocol) ? \"https\" : \"http\") + \"://stats.killtheradio.net/\";
    _paq.push([\"setTrackerUrl\", u+\"piwik.php\"]);
    _paq.push([\"setSiteId\", \"7\"]);
    var d=document, g=d.createElement(\"script\"), s=d.getElementsByTagName(\"script\")[0]; g.type=\"text/javascript\";
    g.defer=true; g.async=true; g.src=u+\"piwik.js\"; s.parentNode.insertBefore(g,s);
  })();
        "))))


