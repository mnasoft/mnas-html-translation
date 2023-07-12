;;;; mnas-html-translation.lisp

(in-package :mnas-html-translation)

(defun parent-tags (node)
  (do* ((rez nil) 
        (n node (plump:parent n)))
       ((eql (type-of n) 'plump-dom:root) rez)
    (push (plump:tag-name n) rez)))

#+nil
(defun get-ch (node)
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (get-ch (nth 0 *l*))
@end(code)
"
  (typecase node
    (plump-dom:element
     (let* ((ch-dren (plump:children node)))
       (loop :for i :from 0 :below (car (array-dimensions ch-dren))
             :collect
             (aref ch-dren i))))
    (plump-dom:text-node (list node))))

#+nil
(defun get-ch (node)
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (get-ch (nth 0 *l*))
@end(code)
"
  (typecase node
    (plump-dom:element
     (when (string/= "code" (plump:tag-name node))
       (let* ((ch-dren (plump:children node)))
         (loop :for i :from 0 :below (car (array-dimensions ch-dren))
               :collect
               (aref ch-dren i)))))
    (plump-dom:text-node (list node))))

(defun pre-code (item)
  (or (equal "pre"  item)
      (equal "code" item)))

(defun all-tags (item) nil)

(defun get-ch (node
               &key
                 (predicat #'all-tags))
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (get-ch (nth 0 *l*))
@end(code)
"
  (typecase node
    (plump-dom:element
     (unless (some predicat
;;;; #'pre-code
                   (parent-tags node))
       (let* ((ch-dren (plump:children node)))
         (loop :for i :from 0 :below (car (array-dimensions ch-dren))
               :collect
               (aref ch-dren i)))))
    (plump-dom:text-node (list node))))

(defun deep-ch (nodes
                &key
                 (predicat #'all-tags))
  (apply #'append
         (loop :for i :in nodes :collect (get-ch i :predicat predicat))))

(defun is-all-of-type (nodes &optional (type 'plump-dom:text-node))
  (loop :for i :in nodes :do
    (when (not (equal type (type-of i)))
      (return-from is-all-of-type nil)))
  t)

(defun deep-ch-loop (nodes
                     &key
                       (predicat #'all-tags))
  " @b(Описание:) функция @b(deep-ch-loop) возвращает список нодов,
который является результатом рекурсивного поиска наследников из списка
nodes. 
"
  (do ((ns (deep-ch nodes :predicat predicat)
           (deep-ch ns    :predicat predicat)))
      ((is-all-of-type ns) ns)))

(defun find-nodes-by-tag (node tag)
  (nreverse
   (plump-dom:get-elements-by-tag-name node tag)))

(defun find-nodes-in-deep (node tag
                           &key
                             (predicat #'all-tags))
  (deep-ch-loop
   (find-nodes-by-tag node tag)
   :predicat predicat))

(defun prepare-text-to-translate (txt)
  (let* ((s1 (ppcre:regex-replace-all "[©\\t\\n\\r\\s ]+" txt " "))
         (s2 (ppcre:regex-replace-all "–" s1 "-"))
         (s3 (ppcre:regex-replace-all "×" s2 "x")))
    s3))

(defun parse-path (path)
  (with-open-file (stream path)
    (plump:parse stream)))

(defun extract-strings-by-tag (path &optional (tag "p"))
  "@b(Описание:) функция @b(extract-strings-by-tag) список строк,
которые находятся в @(tag) и его подтегах.

 @b(Переменые:)
@begin(list)
 @item(path - путь к файлу с расширением html;)
 @item(tag - тег html файла: \"h1\" - \"h6\";
       \"p\"; \"a\"; \"ol\"; \"ul\"; \"td\;
       \"acrticle\"; \"body\")
@end(list)"
  (loop :for i :in (find-nodes-in-deep (parse-path path) tag)
        :collect (prepare-text-to-translate (plump:text i))))

(defun get-html-string (ht path &optional (tag "p"))
  "@b(Описание:) функция @b(get-html-string)"
  (loop :for txt :in (extract-strings-by-tag path tag)
        :do (setf (gethash txt ht) nil))
  ht)

(defun get-html-strings (ht path-lst &optional (tag "p"))
  "@b(Описание:) функция @b(get-html-strings) возвращает
модифицированную хеш-таблицу @b(ht), добавляя к ней строки, связанные
с определенным тегом @b(tag), извлекаемым из списка путей @b(path-lst).

 @b(Переменые:)
@begin(list)
 @item(ht - хешированная таблица;)
 @item(path-lst - список файлов с расширением html, из которых
 извлекаются строки для их последующего перевода;)
 @item(tag - имя тега.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((ht (make-hash-table :test #'equal))
        (path-lst (directory "~/*.html")))
   (get-html-strings (ht path-lst)))
@end(code)
"
  (loop :for path :in path-lst
        :for i :from 1 ;;;; :to 1000
        :do
           (get-html-string ht path tag))
  ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defmethod make-translation (html-file (dictionary mnas-dict:<dictionary>) tags)
  (let ((root (parse-path html-file)))
    (mapcar
     '(lambda (tag)
       (mapcar
        #'(lambda (node)
            (let ((txt (prepare-text-to-translate (plump:text node))))
              (multiple-value-bind (ru exist) (gethash txt *dic-en-ru*)
                (when exist (setf (plump:text node) ru)))))
        (find-nodes-in-deep root tag)))
     tags)
    (with-open-file (stream
                     #+nil (make-html-ru-path html-file)
                     html-file
                     :direction :output
                     :if-exists :supersede)
      (plump:serialize root stream))))

(defmethod make-translation (html-file (dictionary mnas-dict:<dictionary>) tags
                             &key
                               (predicat #'all-tags))
  (let ((root (parse-path html-file)))
    (mapcar
     #'(lambda (tag)
         (mapcar
          #'(lambda (node)
              (let ((txt (prepare-text-to-translate (plump:text node))))
                (multiple-value-bind (ru exist) (mnas-dict:translate txt dictionary)
                  (when exist (setf (plump:text node) ru)))))
          (find-nodes-in-deep root tag :predicat predicat)))
     tags)
    root))

(defmethod make-translations (html-files (dictionary mnas-dict:<dictionary>) tags
                              &key
                                (predicat #'all-tags))
  (loop :for html-file :in html-files :do
    (let ((root (make-translation html-file dictionary tags :predicat predicat)))
      (with-open-file (stream
                       #+nil (make-html-ru-path html-file)
                       html-file
                       :direction :output
                       :if-exists :supersede)
        (plump:serialize root stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
