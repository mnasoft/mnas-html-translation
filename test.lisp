;;;; mnas-html-translation.lisp

(in-package :mnas-html-translation)

(make-translation
 #P"D:/home/_namatv/en-us/help/act_script/act_script_demo_External_Model.html"
 mnas-dict:*dic*
 '("p"))

#P"D:/home/_namatv/en-us/help/act_script/act_script_examples_python_Property_Provider.html"


(defparameter *r*
 (make-translations
  '(#P"D:/home/_namatv/en-us/help/wb2_js/ContainerName60.html")
  mnas-dict:*en-ru*
  '("h1" "h2" "h3" "h4" "h5" "h6" "h7" "ul" "ol" "td" "p")
  :predicat #'mnas-html-translation::pre-code))

(make-translations
 '(#P"D:/home/_namatv/en-us/help/wb2_js/ContainerName16.html")
 mnas-dict:*en-ru*
 '("h1" "h2" "h3" "h4" "h5" "h6" "h7" "ul" "ol" "td" "p")
 :predicat #'mnas-html-translation::pre-code)

(plump:serialize  *r*)

(with-open-file (stream
                 "D:/home/_namatv/en-us/help/wb2_js/ContainerName60_ru.html"
                 :direction :output
                 :if-exists :supersede)
  (plump:serialize  *r* stream)
                        
  
  )

"
D:\home\_namatv\en-us\help\wb2_js\ContainerName57.html
"
