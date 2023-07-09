(require 'usocket)

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (list)
	     (when list
	       (case (first list)
		 (#\% (cons (http-char (second list) (third list))
			    (f (cdddr list))))
		 (#\+ (cons #\space (f (rest list))))
		 (otherwise (cons (first list) (f (rest list))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond
      (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
		      (decode-param (subseq s  (1+ i1) i2)))
		(when i2 (parse-params (subseq s (1+ i2))))))
      ((equal s "") nil)
      (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x)  (parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

(defun doctype-top ()
  "<!DOCTYPE html><html><head><title>Title of the document</title></head><body>")

(defun doctype-bottom ()
  "</body></html>")

(defun serve (request-handler)
  (usocket:with-server-socket
      (socket (usocket:socket-listen "127.0.0.1" 8004))
    (print "Server started.")
    (loop
      (with-open-stream (stream (usocket:socket-stream (usocket:socket-accept socket)))
	(let* ((url (parse-url (read-line stream)))
	       (path (first url))
	       (header (get-header stream))
	       (params (append (rest url)
			       (get-content-params stream header)))
	       (*standard-output* stream))
	  (funcall request-handler path header params))))))

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (princ "Content-Type: text/html")
  (princ (doctype-top))
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (princ "<form>What is your name?<input name='name' /></form>")
	    (format t "<h1>Nice to meet you, ~a!</h1>" (rest name))))
      (princ "<h2>Sorry... I don't know that page.</h2>"))
  (princ (doctype-bottom)))

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (let ((response
	  (if (equal path "greeting")
	      (let ((name (assoc 'name params)))
		(if (not name)
		    (make-response 200 "<form>What is your name?<input name='name' /></form>")
		    (make-response 200 (format nil "<h1>Nice to meet you, ~a!</h1>" (rest name)))))
	      (make-response 404 "<h2>Sorry... I don't know that page.</h2>"))))
    (princ response)))

(defun make-response (code body)
  (format nil "HTTP/1.1 ~a ~a~%Content-Type: text/html~%~%~a~%~a~a"
	  code
	  (if (= code 200) "OK" "Not Found")
	  (doctype-top)
	  body
	  (doctype-bottom)))
