;;;; package.lisp

(defpackage #:mnas-html-translation
  (:use #:cl)
  (:export has-tag-in-parent
           parent-tags )
  (:export get-ch
           deep-ch
           is-all-of-type
           deep-ch-loop
           find-nodes-by-tag
           find-nodes-in-deep
           prepare-text-to-translate
           parse-path
           extract-strings-by-tag
           get-html-string
           get-html-strings
           )
  (:export make-translation
           make-translations
           ))

(in-package :mnas-html-translation)

