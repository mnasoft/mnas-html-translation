;;;; mnas-html-translation.lisp

(in-package :mnas-html-translation)

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

(defun deep-ch (nodes)
  (apply #'append
         (loop :for i :in nodes :collect (get-ch i))))

(defun is-all-of-type (nodes &optional (type 'plump-dom:text-node))
  (loop :for i :in nodes :do
    (when (not (equal type (type-of i)))
      (return-from is-all-of-type nil)))
  t)

(defun deep-ch-loop (nodes)
  " @b(Описание:) функция @b(deep-ch-loop) возвращает список нодов,
который является результатом рекурсивного поиска наследников из списка
nodes. 
"
  (do ((ns (deep-ch nodes) (deep-ch ns)))
      ((is-all-of-type ns) ns)))

(defun find-nodes-by-tag (node tag)
  (nreverse
     (plump-dom:get-elements-by-tag-name node tag)))

(defun find-nodes-in-deep (node tag)
  (deep-ch-loop
   (find-nodes-by-tag node tag)))

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
  ""
  (loop :for path :in path-lst
        :for i :from 1 ;;;; :to 1000
        :do
           (get-html-string ht path tag))
  ht)
