(in-package :wookie-doc)

(deflayout documentation (data)
  (parent-layout 'default data
    (:div :class "documentation"
      (str (getf data :content)))))

