(in-package :wookie-doc)

(deflayout documentation (data)
  (parent-layout 'default data
    (:div :class "documentation"
      (:a :style "float: right;" :href "/docs" "&laquo; Back to docs")
      (str (getf data :content)))))

