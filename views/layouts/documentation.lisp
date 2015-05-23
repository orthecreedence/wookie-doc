(in-package :wookie-doc)

(deflayout documentation (data)
  (parent-layout 'default data
    (:div :class "documentation clear"
      (:div :class "doc-nav"
        (:ul
          (:li (:a :href "/docs" "Getting started"))
          (:li (:a :href "/docs/listeners" "Listeners"))
          (:li (:a :href "/docs/routes" "Routes"))
          (:li (:a :href "/docs/request-handling" "Request handling"))
          (:li (:a :href "/docs/plugins" "Plugins"))
          (:li (:a :href "/docs/core-plugins" "Core plugins"))
          (:li (:a :href "/docs/hooks" "Hooks"))
          (:li (:a :href "/docs/config" "Configuration"))
          (:li (:a :href "/docs/error-handling" "Error handling"))
          (:li (:a :href "/docs/threading" "Threading guide"))
          (:li (:a :href "/docs/writing-plugins" "Writing plugins"))
          (:li (:a :href "/docs/helpers" "Helpers"))
          (:li (:a :href "/docs/best-practices" "Best practices"))))
      (:div :class "doc-content"
        ;(:a :style "float: right;" :href "/docs" "&laquo; Back to docs")
        (str (getf data :content))))))

