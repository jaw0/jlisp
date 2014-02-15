
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: autoload.jl,v 2.2 1997/05/30 00:27:56 jaw Exp $

(defmac autoload (func file (doc "An undocumented function"))
  "(autoload function file [docstr]) declare function to be autoloaded from file
the file will be automagically loaded in the event that the function is called
if func is already defined will do nothing"
  (if (definedp func)
      #f    ; already defined -- do nothing
    (set! doc (strcat "[Autoloaded] " doc))
    `(defmac ,func argl
       ,doc
       (,'backquote
	(progn
	  ;; we want the load to load into the env at the time of the autoload
	  ;; not when it is loaded...
	  (with-current-enviornment ',(cdr (current-enviornment))
				    ;; current-env is of the autoload macro
				    ;; cdr is calling env
				    (load ,file))
	  (eval (cons ',func (,'quote (,'unquote argl)))))))))

;(defmac autoload (func file &optional doc)
 ;  `(load ,file))



