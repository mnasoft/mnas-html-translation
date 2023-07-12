;;;; mnas-html-translation.asd

(asdf:defsystem #:mnas-html-translation
  :description "Describe mnas-html-translation here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("plump"
               "cl-ppcre"
               ;;               "mnas-dict"
               )
  :components ((:file "package")
               (:file "mnas-html-translation")))
