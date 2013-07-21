(in-package :wookie-doc)

(deflayout default (data :top-level t)
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
    (:body
      (:div :id "wookie" (:a :href "/" "&nbsp;"))
      (:div :id "container"
        (:header
          (:h1 (:a :href "/" "Wookie"))
          (:nav
            (:ul
              (:li (:a :href "/" "Home"))
              (:li (:a :href "/docs" "Documentation"))
              (:li (:a :href "/best-practices" "Best practices"))
              (:li (:a :href "/about" "About"))
              ;(:li (:a :href "/guide" "Guide"))
              )))
        (:content
          (:div :class "gutter clear"
            (str (getf data :content))))
        
        (:footer
          (:ul
            (:li (:a :href "/" "Wookie home"))
            (:li (:a :href "https://github.com/orthecreedence/wookie" "github"))
            (:li "Powered by Wookie"))
          (:p "&copy;" (:a :href "http://lyonbros.com" "Lyon Bros. Enterprises, LLC"))))
      (:script
        "
<!-- Piwik -->
<script type=\"text/javascript\">
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
</script>
<!-- End Piwik Code -->
        "))))


