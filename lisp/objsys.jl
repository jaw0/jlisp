
;;;; Copyright (c) 1994, 1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: objsys.jl,v 2.2 1997/05/30 00:28:10 jaw Exp $

;;; jlisp object system
;;; modelled roughly after C++

;;;; ToDo copy-class

#|
element attributes : static      -- only one per class
                   : read-only   -- may only be set in the ctor
                   : initial     -- (initial value) default ctor uses this to set the initial value
method attributes  : virtual     -- func is virtual 
                   : after       -- call func after calling base class func
                   : before      -- call func before calling base class func (both or neither of these can be set)
                   : all-bases   -- call func in all base classes, not just first one found
                   : gather      -- gather results of func into a list
                   : join        -- used with gather to join lists together
class attributes   : all-bases   -- call func in all base classes, not just first one found (pro'ly not a good idea...)
|#

#|
Impl. Details (subject to change at whim):

object consists of vector:
    #(unique-tag vptr base-vectors... elements...)

the prop-list on the unique-tag is an alist containing:
    name        the class name
    tag         the unique tag
    length      length of the vector
    attribs     list of attributes
    nbase       number of base classes
    nelem       number of elements
    statvec     vector of values for static elements
    base        list describing bases, consisting of:
             (name-of-base unique-tag-of-base offset-in-vector)

    elems       list describing the elements, consisting of:
             (name-of-element offset-in-vector attrib-list)

    methods     list describing methods, consisting of:
             (name-of-method virtual-p function attrib-list)

|#

(defvar *class:htable* (makevector 1021))
(defvar *class:pred-suffix* "-p"
  "*class:pred-suffix* prefered suffix for predicate function, typically -p or ?")

(defmac defclass (pname (eleml ()) (methl ()) &rest attrl)
  "(defclass name (elems...) (meths...) attributes...) define a class
name  : name | (name base...)
elems : elemnt-name | (element-name attributes...)
meths : (method-name defn attributes...)"

  (if (nconsp eleml)
      (progn
	(set! attrl (cons eleml attrl))
	(set! eleml ())))
  (if (nconsp methl)
      (progn
	(set! attrl (cons methl attrl))
	(set! methl ())))
  
  (let* ((name  (if (consp pname) (car pname) pname));name of class
	 (bases ())			             ;base classes
	 (nbase 0)
	 (elems ())			             ;element list
	 (nelem 0)
	 (statics ())				     ;static elements
	 (nstatic 0)
	 (methods ())				     ;method list
	 (nmeth 0)
	 (tag (inter (gensym)))
	 (alist ()) 
	 (predname (string->symbol (strcat (symbol->string name) *class:pred-suffix*)))
	 fnew fpred hv
	 slen btag)

    ;; a derived class?
    (if (consp pname)
	;; go thru base classes
	(dolist (b (cdr pname))
	  ;; find base class defn
	  (set! btag (cdr (assq 'tag
				(assq b (nth *class:htable*
					     (% (hashv b)
						(length *class:htable*)))))))
	  ;; (set! btag (cdr (assq 'tag (assq b *class:alist*))))
	  (if (not btag)
	      (error "defclass" b "is not a known class name"))
	  (set! bases (cons
		       (list b btag (+ 2 nbase))
		       bases))
	  (++ nbase)))

    (dolist (mth methl)
      ;; go thru methods

      (if (nconsp mth)
	  (error "defclass" mth "invalid method spec"))

      (let* ((mname (car mth))
	     (defn  #f)
	     (attr  ())
	     (el (cdr mth))
	     (e  (car el))
	     virt-p )

	(while (and (nnullp e) (nnullp (cdr el)))
	  ;; split into defn and attrs
	  (cond
	   ((symbolp e)
	    (set! attr (cons e attr)))
	   (#t
	    (error "defclass" e "invalid method attribute")))
	  (set! el (cdr el))
	  (set! e  (car el)))
	(set! defn (eval e))
	
	(set! virt-p (and (memq 'virtual attr) #t))
	(set! methods (cons
		       (list mname virt-p defn attr)
		       methods))
	(++ nmeth)))
    ;; RSN virtual-ness ought be inheirited from base class method ...

    
    (dolist (elm eleml)
      ;; go thru elements
      (cond

       ((symbolp elm)
	;; simple element
	(set! elems (cons
		     (list elm (+ nelem nbase 2) ())
		     elems))
	(++ nelem))

       ((consp elm)
	;; complex element
	(if (memq 'static (cdr elm))
	    ;; static element
	    (progn
	      (set! statics (cons
			     (list (car elm) nstatic (cdr elm)) ;name value attribs
			     statics))
	      (++ nstatic))
	  ;; non static complex element
	  (set! elems (cons
		       (list (car elm) (+ nelem nbase 2) (cdr elm))
		       elems))
	  (++ nelem)))
      (#t
       (error "defclass" elm "invalid element spec"))))
    
    (set! slen (+ nelem nbase 2))

    ;; create some default methods

    ;; function new
    (set! fnew (eval `(lambda (&optional derivedp)
		  (let* ((s (makestruct ,slen))
			 (pl (get-props ',tag))
			 (ct (assq 'ctor (cdr (assq 'methods pl))))
			 (bl (cdr (assq 'base pl)))
			 bt)
		    (if (nboundp derivedp) (set! derivedp s))
		    (set-nth! s 0 ',tag)
		    (set-nth! s 1 #f)	;no virtual fncs until after constructed
		    (dolist (b bl)
		      ;; instantiate bases
		      (set! bt (get-props (cadr b)))
		      (set-nth! s (caddr b)
				;; find and call base class fnc new
				((caddr (assq 'new (cdr (assq 'methods bt))))
				 derivedp)))
		    ;; call ctor
		    (if ct ((caddr ct) s))
		    (set-nth! s 1 derivedp)
		    s))))
    
    (set! fpred (eval `(lambda (obj)
			(and (structp obj) (eq (nth obj 0) ',tag)))))

    ;; attach default methods
    (set! methods (append
		   methods
		   (list
		    (list 'new       #f fnew ())  ; can not be virtual
		    (list 'pred      #t fpred ()) ; if not virtual, would always be #t
		    (list 'printel   #f class:printel '(before all-bases)) ; print elements
		    (list 'print     #f class:print ())  ; wrapper around printel
		    (list 'elements  #f class:elements '(gather join all-bases before))
		    (list 'ctor      #f class:ctor ())
		    (list 'copy      #f class:copy ())
		    (list 'get       #f class:get ())
		    (list 'set       #f class:set ()))))
    ;; attach conversions to base classes (named after that class)
    (dolist (b bases)
      (set! methods (append
		     methods
		     (list
		      (list (nth b 0) #f (eval `(lambda (obj)
					    (nth obj ,(nth b 2)))) ())))))

    ;; attach data to tag
    (set! alist (append (list
			 (cons 'name    name)
			 (cons 'tag     tag)
			 (cons 'attribs attrl)
			 (cons 'length  slen)
			 (cons 'nbase   nbase)
			 (cons 'base    bases)
			 (cons 'nelem   nelem)
			 (cons 'elems   elems)
			 (cons 'statics statics)
			 (cons 'statvec (makevector nstatic))
			 (cons 'methods methods))
			alist))

    (set! hv (% (hashv name) (length *class:htable*)))
    (set-nth! *class:htable* hv (acons name alist (nth *class:htable* hv)))
    ;; (set! *class:alist* (acons name alist *class:alist*))
    (set-props-nq! tag  alist)

    `(progn
       (define ,name ,fnew)
       (define ,predname ,fpred))))

;;; locate element in object
;;; returns (offset vector attriblist)
;;; or #f
;;; *** for internal use only ***
(defun class:locate-elem (this elem)
  (let* ((pl    (get-props (nth this 0)))
	 (eleml (cdr (assq 'elems pl)))
	 (basel (cdr (assq 'base pl)))
	 (statl (cdr (assq 'statics pl)))
	 v g)
    (cond
     ((set! v (assq elem statl))
      ;; it is a static
      (list (nth v 1) (cdr (assq 'statvec pl)) (nth v 2)))
     ((set! v (assq elem eleml))
      ;; it is an element of this
      (list (nth v 1) this (nth v 2)))
     (#t
      ;; check base classes
      (set! v (car basel))
      (while (and (nnullp v) (not
			      (set! g (class:locate-elem (nth this (nth v 2)) elem))))
	(set! basel (cdr basel))
	(set! v (car basel)))
      g))))

;;; default accessor function
;;; *** for internal use only ***
;;; use method get
(defun class:get (this elem)
  "(class:get obj element) get the value of an element"
  (let* ((loc (class:locate-elem this elem)))
    (if loc
	(nth (nth loc 1) (nth loc 0))
      (error "class:get" elem "cannot find such an element"))))

;;; default settor function
;;; *** for internal use only ***
;;; use method set
(defun class:set (this &rest argl)
  "(class:set obj {element value}) set elements to values"
  (while (nnullp argl)
    (let ((elem (car argl))
	  (value (cadr argl)))
      (let ((loc (class:locate-elem this elem)))
	(if loc
	    (if (and (nth this 1) (memq 'read-only (nth loc 2)))
		;; can set ro elem in ctor only
		(error "class:set" elem "read-only element")
	      (set-nth! (nth loc 1) (nth loc 0) value))
	  (error "class:set" elem "cannot find such an element"))))
    (set! argl (cddr argl))))

;;; default print
;;; *** for internal use only ***
;;; use method print
(defun class:print (this &optional port)
  "(class:print obj [port]) print the object"
  ;; in the form {name:: elem: val elem: val}
  (display "{" port)
  (display (cdr (assq 'name (get-props (nth this 0)))) port)
  (display "::" port)
  this,(printel port)
  (display "}" port))
  
;;; *** for internal use only ***
;;; used by class:print above
(defun class:printel (this (port #t))
  (let* ((pl (get-props (nth this 0)))
	(eleml (cdr (assq 'elems pl))))
    ;; a double indirect conditional format is not the fastest way to go...
    (format port " ~{~{~a: ~s~}~#[~:; ~]~}" (mapcar (lambda (x)
						    (list (nth x 0)
							  (nth this (nth x 1))))
						  eleml))))

;;; default copy/create
;;; *** for internal use only ***
;;; use method copy
(defun class:copy (this)
  "(class:copy obj) create a new obj equal to this one"
  (let* ((eleml this,(elements))
	 (that this,(new)))
    (dolist (e eleml)
      (let ((loc  (class:locate-elem that e))
	    (tval this,(get e)))
	(set-nth! (nth loc 1) (nth loc 0) tval)))
    that))
      
;;; default ctor
;;; set up initial values
;;; *** for internal use only ***
(defun class:ctor (this)
  (let* ((pl (get-props (nth this 0)))
	(eleml (cdr (assq 'elems pl))))

    (dolist (e eleml)
      (let* ((offset (nth e 1))
	     (attr   (nth e 2))
	     (init   (cadr (assq 'initial attr)))) ;should this get evalled ?
	(set-nth! this offset init)))))

;;; method dispathing and stuff...
;;; *** for internal use only ***
(defun class:do-method (do-vfp this mname argl)
  (let* ((mtl  (assq mname (cdr (assq 'methods (get-props (nth this 0))))))
	 (vfp  (and do-vfp (cadr mtl)))
	 (that (if vfp (nth this 1) this))
	 (thaa (get-props (nth that 0)))
	 (fnl  (assq mname (cdr (assq 'methods thaa))))
	 (fnc  (if fnl (nth fnl 2) #f)) ; function
	 (fna  (if fnl (nth fnl 3) ())) ; attributes

	 ;; how, when, and which bases methods to do
	 (before (and fnc (memq 'before fna)))
	 (after  (and fnc (memq 'after  fna)))
	 (gather (and fnc (memq 'gather fna)))
	 (join   (and fnc (memq 'join   fna)))
	 (do-bases (or before after (not fnc)))
	 (allbasep (memq 'all-bases (if fnc
					fna
				      (cdr (memq 'attribs thaa)))))
	 (bl (cdr (assq 'base thaa)))
	 (b (car bl))
	 (result ()))

    ;; default to before if nothing specified ?
    (if (and (not before) (not after) fnc)
	(set! before #t))

    ;; should before AND after be disallowed?
    ;; (if (and before after)
    ;;     (error "method" mname "before and after ?"))
    
    (if (and before fnc)
	(set! result (let ((r (apply fnc that argl)))
		       (if gather ((if join append cons) r result) r))))

    ;; call base methods
    (if do-bases 
	(dolist (b bl)
	  (if (or allbasep (not cdm:got))
	      (set! result (let ((r (class:do-method #f (nth that (nth b 2)) mname argl)))
			     (if gather
				 (append r result)
			       r))))))
    
    (if (and after fnc)
	(set! result (let ((r (apply fnc that argl)))
		       (if gather ((if join append cons) r result) r))))
    (if fnc
	(set! cdm:got #t))
    result))

;;; (elem elem elem elem) or ((elem (attr)) (elem (attr)) (elem (attr)))
(defun class:elements (this (attr #f))
  "(class:elements obj [attrp]) return list of elements, optionally with their attributes"
  (let* ((pl (get-props (nth this 0)))
	(eleml (cdr (assq 'elems pl))))
    (mapcar (lambda (x)
	      (if attr
		  (cons (nth x 0) (nth x 2))
		(nth x 0)))
	    eleml)))

;;; method dispatch
;;; non-objects redirect to (type:method this argl...) ie. 1,(foo) -> (method 1 'foo) -> (int:foo 1)
(defun method (this mname &rest argl)
  "(method object method-name args...) call a class method"
  (if (structp this)
      (let ((cdm:got #f))
	(class:do-method #t this mname argl))
      (apply (string->symbol (strcat (typeof this) ?: (symbol->string mname))) this argl)))

(defun add-method (clname mthname &rest argl)
  "(add-method classname methodname attributes... func) add a method to a class"
  (let* ((pl (cdr (assq clname (nth *class:htable*
			       (% (hashv clname)
				  (length *class:htable*))))))
	 (mtha (assq 'methods pl))
	 (meths (cdr mtha))
	 (defn #f)
	 (attr ())
	 (el argl)
	 (e (car el))
	 virt-p)
    
    (while (and (nnullp e) (nnullp (cdr el)))
	  ;; split into defn and attrs
	  (cond
	   ((symbolp e)
	    (set! attr (cons e attr)))
	   (#t
	    (error "add-method" e "invalid method attribute")))
	  (set! el (cdr el))
	  (set! e  (car el)))
	(set! defn (eval e))
	
	(set! virt-p (and (memq 'virtual attr) #t))
	(set-cdr! mtha (cons
		       (list mthname virt-p defn attr)
		       meths))))


;;;=======================================================================================
;;; test classes
#|
(defclass foo
  ((x (docstring "I am x of foo"))
   (y read-only (initial "Why?"))
   z
   (q static))
  ((proc (lambda (x) (display "foo::proc\n")))
   (func (lambda (x) (display "foo::func\n")))
   (vfnc virtual (lambda (x) (display "foo::vfnc\n")))))

(defclass (bar foo)
  (a
   (b read-only)
   (c ugly))
  ((proc (lambda (x) (display "bar::proc\n")))
   (vfnc virtual (lambda (x) (display "bar::vfnc\n")))
   (ctor (lambda (this)
	   (method this 'set 'a 0)
	   (method this 'set 'b "Glark!")
	   (method this 'set 'c 9)))))


(defclass Person
  ((name (initial "John Doe"))
   (ssn  (initial "000-00-0000")))
  ((print virtual (lambda (this &optional port)
		    (display (method this 'get 'name) port)))))

(defclass (Employee Person)
  ((wage  (initial 3.75))
   (hours (initial 0))
   (dept  (initial "MailRoom")))
  ((print virtual after (lambda (this &optional port)
			  (display " (" port)
			  (display (method this 'get 'dept) port)
			  (display ")" port )))))

(defclass (Manager Employee)
  ((peons ()))
  ((print virtual before (lambda (this &optional port)
			   (display "Mngr. " port)))))


|#
