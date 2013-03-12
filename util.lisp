(in-package :wookie-doc)

(defun load-views ()
  "Load all view files in one swift stroke. Maybe not 'correct' but sure helps
   organize."
  (let* ((view-dir (format nil "~a/views" *root*))
         (files (cl-fad:list-directory view-dir)))
    (dolist (file files)
      (let ((file-str (namestring file)))
        (when (and (cl-fad:file-exists-p file)
                   (string= (subseq file-str (- (length file-str) 5)) ".lisp"))
          (load file))))))

(defun load-view (name data))

(defmacro defview ((name &key (template :default) (stream 's)) &body body)
  `(progn
     (defun ,(intern (string-upcase (format nil "view-~a" name)) :wookie-doc) (&optional data)
       (let ((content (cl-who:with-html-output-to-string (,stream nil :prologue nil :indent nil))))
         

(setf (cl-who:html-mode) :html5)
(cl-who:with-html-output-to-string (s nil :prologue t :indent nil)
  (:html
    (:head
      (:title "FUUUUCK"))
    (:body
      (:h1 "hai...")
      (:p "how r u"))))
