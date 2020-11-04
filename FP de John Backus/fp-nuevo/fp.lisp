; El siguiente codigo fuente en LISP ha sido adaptado de:
;
; https://github.com/soemraws/parse-float/blob/master/parse-float.lisp
;
; https://github.com/jfacorro/fp-interpreter-in-lisp

(deftype valid-radix ()
  "A valid Common Lisp radix."
  `(integer 2 36))

(deftype bounding-index ()
  "A valid upper bound to a string."
  `(integer 0 ,array-total-size-limit))

(deftype string-index ()
  "A valid string index."
  `(integer 0 ,(1- array-total-size-limit)))

(declaim (inline sign-char-p))
(defun sign-char-p (character)
  "Predicate for testing if CHARACTER is a sign character (i.e. #\+ or #\-)."
  (declare (type character character))
  (or (char= #\+ character)
      (char= #\- character)))

(declaim (inline whitespace-char-p))
(defun whitespace-char-p (character)
  "Predicate for testing if CHARACTER is a whitespace character."
  (declare (type character character))
  (or (char= #\Space character)
      (char= #\Tab character)
      (char= #\Return character)
      (char= #\Newline character)
      (char= #\Linefeed character)
      (char= #\Page character)))

(declaim (inline skip-whitespaces))
(defun skip-whitespaces (string &key (start 0) end)
  "For the substring in STRING delimited by START and END, skip all
  the whitespace at the beginning and return the index of the first
  non-whitespace character, or END if no non-whitespace characters
  were found."
  (declare (type string string)
           (type string-index start)
           (type (or null bounding-index) end))
  (unless end
    (setf end (length string)))
  (loop for index from start upto end
        while (and (< index end)
                   (whitespace-char-p (char string index)))
        finally (return index)))

(defun parse-integer-only (string &key (start 0) (end (length string))
                    (radix 10) (allow-sign t))
  "Parse an integer from a string, without skipping whitespaces.
Returns three values: the integer, the position in the string that
ended the parsing, and a boolean which is T if the parsing ended due
to a whitespace or end of the string, and NIL otherwise.  If
allow-sign is NIL (T by default), also signs are not allowed in the
string (i.e. cannot start with #\+ or #\-)."
  (declare (type string string)
           (type string-index start)
           (type bounding-index end)
           (type valid-radix radix))
  (let ((index start))
    (declare (type string-index index))
    (if (>= index end)
        (values nil index t)
        (let ((char (char string index)))
          (if (or (and (not allow-sign) (sign-char-p char))
                  (whitespace-char-p char))
              (values nil index t)
              (multiple-value-bind (value position)
                  (parse-integer string
                                 :start index
                                 :end end
                                 :junk-allowed t
                                 :radix radix)
                (if (or (= position end)
                        (whitespace-char-p (char string position)))
                    (values value position t)
                    (values value position nil))))))))
                    
(defun parse-float (string &key (start 0) (end (length string))
                 (radix 10) (junk-allowed nil)
                 (decimal-character #\.) (exponent-character #\e)
                 (type *READ-DEFAULT-FLOAT-FORMAT*))
  "Similar to PARSE-INTEGER, but parses a floating point value and
  returns the value as the specified TYPE (by default
  *READ-DEFAULT-FLOAT-FORMAT*). The DECIMAL-CHARACTER (by default #\.)
  specifies the separator between the integer and decimal parts, and
  the EXPONENT-CHARACTER (by default #\e, case insensitive) specifies
  the character before the exponent. Note that the exponent is only
  parsed if RADIX is 10."
  (declare (type string string)
           (type valid-radix radix)
           (type string-index start)
           (type bounding-index end)
           (type character decimal-character exponent-character))
  (let* ((sign 1)                       ; sign of the float
         (digits 0)                     ; number of decimal digits
         (index (skip-whitespaces string ; index walking through string
                                  :start start
                                  :end end))
         (integer-part nil)             ; parts of the value
         (decimal-part 0)
         (exponent-part 0)
         (result nil))                   ; final result
    (declare (type string-index index)
             (type integer sign exponent-part)
             (type (or null integer) integer-part decimal-part))
    (labels ((parse-sign ()
               (if (= index end)
                   #'parse-finish
                   (let ((char (char string index)))
                     (cond
                       ((char= #\- char)
                        (if (>= (incf index) end)
                            #'parse-finish
                            (progn
                              (setf sign -1)
                              #'parse-integer-part)))
                       ((char= #\+ char)
                        (if (>= (incf index) end)
                            #'parse-finish
                            #'parse-integer-part))
                       (t #'parse-integer-part)))))

             (parse-integer-part ()
               (multiple-value-bind (value position finished)
                   (parse-integer-only string
                                       :start index
                                       :end end
                                       :radix radix
                                       :allow-sign nil)
                 (declare (type bounding-index position))
                 (setf integer-part value
                       index position)
                 (if finished
                     #'parse-finish
                     (let ((char (char string index)))
                       (cond
                         ((char= char decimal-character)
                          (incf index)
                          #'parse-decimal-part)
                         ((null integer-part)
                          #'parse-finish)
                         ((and (char-equal char exponent-character)
                               (= radix 10))
                          (setf index (+ 1 index)
                                decimal-part 0)
                          #'parse-exponent-part)
                         (t #'parse-finish))))))

             (parse-decimal-part ()
               (multiple-value-bind (value position finished)
                   (parse-integer-only string
                                       :start index
                                       :end end
                                       :radix radix
                                       :allow-sign nil)
                 (declare (type bounding-index position))
                 (setf decimal-part (or value 0)
                       digits (- position index)
                       index position)
                 (when (and decimal-part
                            (null integer-part))
                   (setf integer-part 0))
                 (if finished
                     #'parse-finish
                     (progn
                       (unless decimal-part
                         (setf decimal-part 0))
                       (if (and (= radix 10)
                                (char-equal (char string index) exponent-character))
                           (progn
                             (incf index)
                             #'parse-exponent-part)
                           #'parse-finish)))))

             (parse-exponent-part ()
               (multiple-value-bind (value position)
                   (parse-integer string
                                  :start index
                                  :end end
                                  :junk-allowed t)
                 (declare (type bounding-index position))
                 (setf exponent-part (or value 0)
                       index position)
                 #'parse-finish))

             (parse-finish ()
               (unless junk-allowed
                 (setf index (skip-whitespaces string :start index :end end)))
               (if integer-part
                   (if (or (= index end)
                           junk-allowed)
                       (setf result (let ((mantissa
                                           (* sign (+ (coerce integer-part type)
                                                      (coerce (* decimal-part
                                                                 (expt radix (- digits))) type)))))
                                      (if (minusp exponent-part)
                                        (/ mantissa (expt 10 (- exponent-part)))
                                        (* mantissa (expt 10 exponent-part)))))
                       (simple-parse-error "junk in string ~S." string))
                   (unless junk-allowed
                     (simple-parse-error "junk in string ~S." string)))
               nil))
      (declare (dynamic-extent #'parse-sign
                               #'parse-integer-part
                               #'parse-decimal-part
                               #'parse-exponent-part
                               #'parse-finish))

      (loop with parser = #'parse-sign
            while parser
            do (setf parser (funcall (the function parser)))
            finally (return (values result index))))))

;------------------------------------
; make-node
;------------------------------------
(defun make-node (element &rest children)
    "Returns a tree node with the specified chidlren nodes"
    (cons element children))

;------------------------------------
; children
;------------------------------------
(defun children (node) 
    "Gets the children from a tree node"
    (rest node))

;------------------------------------
; datum
;------------------------------------
(defun datum (node)
    "Gets the datum from a tree node"
    (first node))

;------------------------------------
; add-child
;------------------------------------
(defun add-child (node child)
    (cond ((atom (rest node)) (setf (rest node) (list child)))
          (t (setf (rest node) (append (rest node) (list child))))))

;;----------------------------------------------
;; string-split
;;----------------------------------------------
(defun string-split (str &rest delims)
    "Splits a given string using the specified delimiters"
    (let ((splitted-str (list str)))
        ; If there are delimiters then explode
         (if (not (null delims))
            ; For each delimiter explode the string 
            ; and each instance afterwards
             (dolist (delim delims)
                     (setf splitted-str (flatten (mapcar (explode-lambda delim nil) splitted-str)))))
         splitted-str))

;;----------------------------------------------
;; string-explode
;;----------------------------------------------
(defun string-explode (str &rest delims)
    "Explodes a given string using the specified delimiters"
    (let ((exploded-str (if (listp str) str (list str))))
        ; If there are delimiters then explode
         (if (not (null delims))
            ; For each delimiter explode the string 
            ; and each instance afterwards
             (dolist (delim delims)
                     (setf exploded-str (flatten (mapcar (explode-lambda delim) exploded-str)))))
         exploded-str))

;;----------------------------------------------
;; explode-lambda
;;----------------------------------------------

(defun explode-lambda (delim &optional (explode t))
    "Generates a function with the delimiter"
    (lambda (str) (string-explode-helper str delim explode)))

;;----------------------------------------------
;; string-explode-helper
;;----------------------------------------------
(defun string-explode-helper (str delim &optional (explode t))
    "Explodes or splits (based on 'explode' param value) a string using the delimiters specified"
    (let ((index (search delim str))
          (delim-length (length delim)))
        (if (null index)
            (if (empty-string? str) nil (list str))
            (let ((begin-str (subseq str 0 index))
                  (delim (if explode (list delim) nil))
                  (end-str (string-explode-helper (subseq str (+ index delim-length)) delim explode)))
                 (append
                    ; if delimiter is in the first position don't add empty string
                    (if (zerop index) nil (list begin-str))
                    delim
                    end-str)))))

;;----------------------------------------------
;; Flatten
;;----------------------------------------------
(defun flatten (l)
    (cond
        ((null l) nil)
        ((atom l) (list l))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

;;----------------------------------------------
;; empty-string
;;----------------------------------------------
(defun empty-string? (str)
    (string= "" str))

;;----------------------------------------------
;; string-replace
;;----------------------------------------------
(defun string-replace (str &rest substrs)
    "Replace a series of substrings for others by 
    exploding the whole string with the first one, 
    replacing and then concatenating"
    (let* ((substr1 (first substrs))
           (substr2 (second substrs))
           (expl-str (string-explode str substr1))
           (result-str nil))
        ; Iterate through the exploded list and replace substr1 for substr2
        (dolist (item expl-str)
                (setf result-str (append result-str (if (string= item substr1) `(,substr2) `(,item)))))
        ; Concatenate all loose string in the list
                (setf result-str (apply #'concatenate (append '(string) result-str)))
        ; If there are still replacements, keep going
                (setf substrs (cddr substrs))
                (if (not (null substrs))
                    (apply #'string-replace (append (list result-str) substrs))
                    result-str)))

;;----------------------------------------------
;; string-explode-sequentially
;;----------------------------------------------
(defun string-explode-sequentially (str &rest delims)
    "Explodes a string (or a list of strings) 
    searching sequentially for the first matching 
    delimiter"
    (cond
        ((listp str)
            (flatten 
                (mapcar 
                    (lambda (item) 
                        (apply #'string-explode-sequentially (append `(,item) delims))) str)))            
        ((stringp str)
            (let ((result nil)
                  (len (length str))
                  (pos 0)
                  (exploded nil)
                  (index nil)
                  (start nil))
                (loop (if (>= pos len) (return))
                    (setf exploded nil)
                    ; Run through every delimiter sequentially
                    (dolist (delim delims)
                        (setf index (search delim str))
                        ; If the delimiter begins in the current position
                        ; then explode and terminate the dolist
                        (cond
                            ((and (not (null index)) (= pos index))
                                (setf start (if (zerop index) nil (list (subseq str 0 pos))))
                                (setf result (append result start `(,delim)))
                                (setf str (subseq str (+ pos (length delim))))
                                (setf pos 0)
                                (setf len (length str))
                                (setf exploded t))))
                    (if (not exploded) (setf pos (1+ pos))))
                (append result (if (empty-string? str) nil `(,str)))))
        (t str)))

;;----------------------------------------------
;; string-insert-alt
;;----------------------------------------------
(defun insert-between (str delim)
    "Insert an element in bewteen a lists elements"
    (cond
        ((null str) nil)
        ((null (rest str)) (list (first str)))
        (t (append `(,(first str)) `(,delim) (insert-between (rest str) delim)))))

;;----------------------------------------------
;; contains?
;;----------------------------------------------
(defun contains? (substr str)
    (not (null (search substr str))))

;:--------------------------------------
;; List that contains all parsing rules
;;--------------------------------------
(defparameter *rules* nil)

;;--------------------------------------
;; add-rule
;;--------------------------------------
(defun add-rule (rule)
    "Adds a rule to the parsing rules list"
    (setf *rules* (append  *rules* (list rule))))

;;--------------------------------------
;; parse
;;--------------------------------------
(defun parse (expr)
    "Applies rules in the order they were added and returns the last result"
    (unless (stringp expr) (error "PARSE: expr should be a string"))
    (if (null *rules*) expr (apply-rules expr *rules*)))

;;--------------------------------------
;; apply-rules
;;--------------------------------------
(defun apply-rules (expr rules)
    (let* ((rule (car rules))
           (remain (rest rules))
           (changed-code (apply-rule rule expr)))
        (if (null remain) changed-code (apply-rules changed-code remain))))

;;--------------------------------------
;; apply-rule
;;--------------------------------------
(defun apply-rule (rule raw-code)
    (funcall (getf rule :function) raw-code))

;;--------------------------------------
;; make-rule
;;--------------------------------------
(defmacro make-rule (name body)
    "Creates a parsing rule"
    (let* ((arg (gensym))
           (body (substitute-recursive arg (intern "ARG") body)))
        `(list :name ,name :function (lambda (,arg) ,body))))

;;--------------------------------------
;; substitute-recursive
;;--------------------------------------
(defun substitute-recursive (s1 s2 lst)
    "Substitutes a symbol s2 for s1 in the list and its sublists recursively"
    (cond 
        ((listp lst)
            (setf lst (substitute s1 s2 lst))
            (mapcar (lambda (it) (substitute-recursive s1 s2 it)) lst))
        (t lst)))

;:--------------------------------------
;; Checks that the expression contains a :
;; which means it is either a user function
;; definition or a function call
;;--------------------------------------
(add-rule 
    (make-rule "Validate expression"
        (cond
            ((comment? arg) nil)
            (t (validate-expression arg)
               (generate-fn-env arg)))))

;:--------------------------------------
;; validate-expression
;;--------------------------------------
(defun validate-expression (arg)
    (unless (or (evaluation? arg) (definition? arg))
        (error "The expression must be a function definition or a function evaluation." )))

;:--------------------------------------
;; definition?
;;--------------------------------------
(defun definition? (arg)
    (let* ((arg (string-trim " " arg))
           (index-def (search "Def " arg)))
        (and (contains? "=" arg) 
             (zerop index-def)
             (= 1 (count #\= arg)))))

;:--------------------------------------
;; evaluation?
;;--------------------------------------
(defun evaluation? (arg)
    (and (contains? ":" arg) 
        (not (definition? arg))
        (= 1 (count #\: arg))))

;:--------------------------------------
;; Replace some expression for parser-friendly
;; new expressions
;;--------------------------------------
(add-rule 
    (make-rule "Replace strings for parser-friendly expressions"
        (let ((fn (getf arg :fn))
              (env (getf arg :env)))
            
            (list :fn (string-replace fn 
                                        ; construct
                                        "[" "( construct ("
                                        "]" "))"
                                        "," ")("
                                        ; condition
                                        "->" "->("
                                        ";" ")")
            :env (string-replace env "<" "("
                                        ">" ")")))))

;:--------------------------------------
;; Convert to uppercase
;;--------------------------------------
(add-rule 
    (make-rule "Convert to uppercase" 
        (let ((fn (getf arg :fn))
              (env (getf arg :env)))

            (list :fn (string-upcase fn) :env (string-upcase env)))))

;:--------------------------------------
;; Split by " " ;
;;--------------------------------------
(add-rule 
    (make-rule "Split by ' ' and ';'" 
        (let ((fn (getf arg :fn))
              (env (getf arg :env)))

            (list :fn (string-split fn ":" " " ";" "=") :env (string-split env "," " ") ))))

;:--------------------------------------
;; Explode by special characters
;;--------------------------------------
(add-rule 
    (make-rule "Explode by special characters"
        (let ((fn (getf arg :fn))
              (env (getf arg :env)))

            (list :fn (string-explode-sequentially fn "(" ")" "/" "->" "<>" ">" "<" "[" "]" "~" "+" "-" "*" "%" "@")
              :env (string-explode env "(" ")")))))

;:--------------------------------------
;; Convert parenthesis in sublists
;;--------------------------------------
(add-rule 
    (make-rule "Convert parenthesis in sublists" 
        (let ((fn (getf arg :fn))
              (env (getf arg :env)))

            (list :fn (listify fn) :env (listify env)))))

;:--------------------------------------
;; Build tree
;;--------------------------------------
(add-rule 
    (make-rule "Build tree" 
        (let ((fn (getf arg :fn))
              (env (getf arg :env)))

        (list :fn (build-tree fn) :env env))))

;:--------------------------------------
;; listify
;;--------------------------------------
(defun listify (expr &optional (lists nil))
    (if (atom expr) (first lists)
        (let* ((head (first expr))
               (tail (rest expr))
               (current (first lists))
               (next (second lists))
               (else (cddr lists)))
            (cond
                ((string= head "(") 
                    (listify tail (cons nil lists)))
                ((string= head ")")
                    (listify tail (append (list (append next (list current))) else)))
                (t (listify tail (append (list (append current `(,head))) (list next) else)))))))

;;----------------------------------------------
;; build-tree
;;----------------------------------------------
(defun build-tree (lst)
    (build-tree-helper lst nil nil))

;;----------------------------------------------
;; build-tree-helper
;;----------------------------------------------
(defun build-tree-helper (code operators operands)
    (let* ((token (first code))           
           (fn (get-function token)))
        ;(format t "  token: \"~a\"~%" token)
        (cond 
            ((null code)
                (if (null operators)
                    (car operands)
                    (handle-operator nil operators operands)))
            ((listp token) 
                    (let ((subexpr (build-tree-helper token nil nil)))
                        (setf operands (cons subexpr operands))
                        (build-tree-helper (rest code) operators operands)))
            ((operand? fn)
                (setf operands (cons (make-node token) operands))
                (build-tree-helper (rest code) operators operands))
            (t (let* ((last-op (first operators))
                       (last-fn (get-function last-op)))
                    (if (or (null operators)
                            (> (precedence fn) (precedence last-fn)))
                        (build-tree-helper (rest code) (cons token operators) operands)
                        (handle-operator code operators operands)))))))

;;----------------------------------------------
;; handle-operator
;;----------------------------------------------
(defun handle-operator (code operators operands)
    (let* ((operator (first operators))
           (fn (get-function operator))
           (nparams (num-params fn)))
        ; If the number of parameters specified for the function
        ; is a negative number then take all available operands
        (if (< nparams 0) (setf nparams (length operands)))
        (build-tree-helper
            code
            (rest operators)
            (cons 
                (apply #'make-node (cons operator (reverse (subseq operands 0 nparams))))
                (subseq operands nparams)))))

;;----------------------------------------------
;; generate-fn-env
;;----------------------------------------------
(defun generate-fn-env (arg)
    "Generates an alist with :fn and :env"
    (let* ((parts (string-split arg ":"))
            (fn (first parts))
            (env (second parts)))
        ; If it's a function definition don't assign any env
        (if (definition? arg)
            ; Function definition
            (list :fn arg :env nil)
            ; Function (or expression) evaluation
            (list :fn fn :env env))))

;;----------------------------------------------
;; comment?
;;----------------------------------------------
(defun comment? (arg)
    "Returns true if the arg starts with a ;"
    (let ((index (search ";" arg)))
        (and (not (null index)) (zerop index))))

;;------------------------------
;; Constante FP values
;;------------------------------
(defconstant *false-value* "F")
(defconstant *true-value* "T")
(defconstant *empty-list-value* "<>")

;;------------------------------
;; id
;;------------------------------
(defun id ()    
    (lambda (arg)
        arg))

;;------------------------------
; selector-right
;;------------------------------
(defun selector-right (n)
    (lambda (arg)
        (if (<= n (length arg))
            (nth (1- n) (reverse arg))
            (error (format nil "Wrong argument: ~a. Expected length: ~a~%" (desymbolize arg) n)))))

;;------------------------------
; tl (tail)
;;------------------------------
(defun tl ()    
    (lambda (arg) 
        (rest arg)))

;;------------------------------
; tlr (tail right)
;;------------------------------
(defun tlr ()
    (lambda (arg)
        (reverse (rest (reverse arg)))))

;;------------------------------
; atom
;;------------------------------
(defun fp-atom ()    
    (lambda (arg) 
        (get-truth-value (atom arg))))

;;------------------------------
; eq
;;------------------------------
(defun fp-eq ()
    (lambda (arg)
        (if (= 2 (length arg))
            (get-truth-value (equal (first arg) (second arg)))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; fp-null
;;------------------------------
(defun fp-null ()
    (lambda (arg) 
        (get-truth-value (null arg))))

;;------------------------------
; fp-reverse
;;------------------------------
(defun fp-reverse ()
  (lambda (arg) (reverse arg)))

;;------------------------------
; fp-distl
;;------------------------------
(defun distl ()
    (lambda (arg)
        (let ((l (cadr arg))
             (a (car arg)))
            (cond 
                ((null l) nil)
                (t (append (list (list a (car l))) (funcall (distl) (list a (cdr l)))))))))

;;------------------------------
; fp-distr
;;------------------------------
(defun distr ()
  (lambda (arg)
    (let ((l (car arg))
          (a (cadr arg)))
      (cond ((null l) nil)
            (t (append (list (list (car l) a)) (funcall (distr) (list (cdr l) a))))))))

;;------------------------------
; fp-length
;;------------------------------
(defun fp-length ()
    (lambda (arg) 
        (length arg)))

;;------------------------------
; +
;;------------------------------
(defun fp-+ ()
    (lambda (arg)
        (if (= 2 (length arg))
            (let ((res (+ (first arg) (second arg))))
                 (if (= res (floor res)) (floor res) res))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; -
;;------------------------------
(defun fp-- ()
    (lambda (arg)
        (if (= 2 (length arg))
            (let ((res (- (first arg) (second arg))))
                 (if (= res (floor res)) (floor res) res))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; *
;;------------------------------
(defun fp-* ()
    (lambda (arg)
        (if (= 2 (length arg))
            (let ((res (* (first arg) (second arg))))
                 (if (= res (floor res)) (floor res) res))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; % Division
;;------------------------------
(defun fp-% ()
    (lambda (arg)
        (if (= 2 (length arg))
            (let ((res (float (/ (first arg) (second arg)))))
                 (if (= res (floor res)) (floor res) res))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; <
;;------------------------------
(defun fp-< ()
    (lambda (arg)
        (if (= 2 (length arg))
            (get-truth-value (< (first arg) (second arg)))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; >
;;------------------------------
(defun fp-> ()
    (lambda (arg)
        (if (= 2 (length arg))
            (get-truth-value (> (first arg) (second arg)))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
; trans
;;------------------------------
(defun trans ()
    (lambda (arg)
        (cond 
            ((null (car arg)) nil)
            (t (append (list (mapcar #'car arg)) (funcall (trans) (mapcar #'cdr arg)))))))

;;------------------------------
; and
;;------------------------------
(defun fp-and ()
    (lambda (arg)
        (if (= 2 (length arg))
            (get-truth-value (eval (append '(and) (map-truth-values arg))))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
;; or
;;------------------------------
(defun fp-or ()
    (lambda (arg)
        (if (= 2 (length arg))
            (get-truth-value (eval (append '(or) (map-truth-values arg))))
            (error (format nil "Wrong argument: ~a. Expected length: 2~%" (desymbolize arg))))))

;;------------------------------
;; map-truth-values
;;------------------------------
(defun map-truth-values (arg)
    (mapcar #'get-lisp-truth-value arg))

;;------------------------------
;; get-lisp-truth-value
;;------------------------------
(defun get-lisp-truth-value (val)
    (equal val *true-value*))

;;------------------------------
;; get-truth-value
;;------------------------------
(defun get-truth-value (data)
    (if data *true-value* *false-value*))

;;------------------------------
;; not
;;------------------------------
(defun fp-not ()    
    (lambda (arg)
        (if (atom arg)
            (if (not (get-lisp-truth-value arg)) *true-value* *false-value*)
            (error (format nil "Wrong argument: ~a. Atom expected~%" (desymbolize arg))))))

;;------------------------------
; fp-apndl
;;------------------------------
(defun apndl ()
  (lambda (arg)
    (let ((a (car arg))
          (l (cadr arg)))
      (append (list a) l))))

;;------------------------------
; fp-apndr
;;------------------------------
(defun apndr ()
  (lambda (arg)
    (let ((l (first arg))
          (a (second arg)))
      (append l (list a)))))

;;------------------------------
; rotl
;;------------------------------
(defun rotl ()
    (lambda (arg)
        (append (cdr arg) (list (car arg)))))

;;------------------------------
; rotr
;;------------------------------
(defun rotr ()
    (lambda (arg)
        (append (last arg) (butlast arg))))

;;------------------------------
; fp-compose
;;------------------------------
(defun compose (f1 f2)
    (lambda (arg)
        (fp-funcall f1 (fp-funcall f2 arg))))

;;------------------------------
; fp-construct
;;------------------------------
(defun construct (&rest args)
    (lambda (arg) 
        (mapcar (lambda (f) 
            (fp-funcall f arg)) args)))

;;------------------------------
; fp-const
;;------------------------------
(defun const (const)
  (lambda (arg)
    arg    ; To avoid compiler warning
    const))

;;------------------------------
; fp-cond
;;------------------------------
(defun fp-cond (a b c)    
    (lambda (arg) 
        (if (get-lisp-truth-value (fp-funcall a arg)) (fp-funcall b arg) (fp-funcall c arg))))

;;------------------------------
; fp-insert
;;------------------------------
(defun insert (f)
    (lambda (arg) 
        (cond
            ((null arg) nil)
            ((= (length arg) 1) (car arg))
            ((= (length arg) 2) (fp-funcall f (list (car arg) (cadr arg))))
            (t (fp-funcall f (list (car arg) (fp-funcall (insert f) (cdr arg))))))))

;;------------------------------
; alpha
;;------------------------------
(defun alpha (f)
    (lambda (arg) 
        (mapcar (lambda (a) (fp-funcall f a)) arg)))

;;------------------------------
; Def
;;------------------------------
(defun def (name fn)
    "Creates a user defined function"
    (when (functionp name) 
        (error (concatenate 'string "The function already exists")))
    (lambda ()
        (add-function (make-function name (make-user-function fn)))
        (concatenate 'string "FUNCTION " name " DEFINED")))

;;------------------------------
; make fp user function
;;------------------------------
(defun make-user-function (fn)
    (lambda ()
        (lambda (arg) 
            (fp-funcall fn arg))))

;;----------------------------------------------
;; General hash table function
;;----------------------------------------------
(defun add-hash-item (hash-table key value)
    (setf (gethash key hash-table) value))

;;----------------------------------------------
;; Define functions hash
;;----------------------------------------------
(defparameter *functions* (make-hash-table :test 'equal))

;;----------------------------------------------
;; Add functions to hash-tables
;;----------------------------------------------
(defun add-function (fun)
    (add-hash-item *functions* (getf fun :name) fun))

;;----------------------------------------------
;; Get function
;;----------------------------------------------
(defun get-function (name)
    (when (stringp name)
        (gethash (string-upcase name) *functions*)))

;;----------------------------------------------
;; Resolves the inner function from a fp-function alist
;;----------------------------------------------
(defun resolve-operand (arg)
    (let ((fn (cond 
                    ((functionp arg) 
                        arg)
                    ((listp arg) 
                        (getf arg :function))
                    ((numberp arg) 
                        (lambda () (lambda (x) (if (<= arg (length x))
                                                   (nth (1- arg) x)
                                                   (error (format nil "Wrong argument: ~a. Expected length: ~a~%" (desymbolize x) arg))))))
                    ((stringp arg) 
                        (getf (get-function arg) :function)))))
        (cond 
            ((null fn)    (error (format nil "Could not resolve '~a' to a function.~%" arg)))
            ((functionp arg) arg)
            (t (funcall fn)))))

;;----------------------------------------------
;; precedence
;;----------------------------------------------
(defun precedence (fn)
    (getf fn :precedence))

;;----------------------------------------------
;; num-params
;;----------------------------------------------
(defun num-params (fn)
    (getf fn :nparam))

;; ----------------------------------------
;; make-function
;; ----------------------------------------
(defun make-function (name fn &key (precedence 0) (nparam 0))
    (list :name (string-upcase name) :function fn :precedence precedence :nparam nparam))

;;------------------------------------
;; noparams-p
;;------------------------------------
(defun operand? (fn)
    (or (null fn) (= (getf fn :nparam) 0)))

;;----------------------------------------------
;; init-functions
;;----------------------------------------------
(defun init-functions ()
    (add-function (make-function "id" #'id))
    (add-function (make-function "r" #'selector-right :precedence 3 :nparam 1))
    (add-function (make-function "tl" #'tl))
    (add-function (make-function "tlr" #'tlr))
    (add-function (make-function "atom" #'fp-atom))
    (add-function (make-function "eq" #'fp-eq))
    (add-function (make-function "null" #'fp-null))
    (add-function (make-function "reverse" #'fp-reverse))
    (add-function (make-function "distl" #'distl))
    (add-function (make-function "distr" #'distr))
    (add-function (make-function "length" #'fp-length))
    (add-function (make-function "-" #'fp--))
    (add-function (make-function "+" #'fp-+))
    (add-function (make-function "*" #'fp-*))
    (add-function (make-function "%" #'fp-%))
    (add-function (make-function "<" #'fp-<))
    (add-function (make-function ">" #'fp->))
    (add-function (make-function "trans" #'trans))
    (add-function (make-function "and" #'fp-and))
    (add-function (make-function "or" #'fp-or))
    (add-function (make-function "not" #'fp-not))
    (add-function (make-function "apndl" #'apndl))
    (add-function (make-function "apndr" #'apndr))
    (add-function (make-function "rotl" #'rotl))
    (add-function (make-function "rotr" #'rotr))
    ;;----------------------------------------------
    ;; Functional forms
    ;;----------------------------------------------
    (add-function (make-function "o" #'compose :precedence 1 :nparam 2))
    (add-function (make-function "construct" #'construct :precedence 0 :nparam -1))
    (add-function (make-function "~" #'const :precedence 2 :nparam 1))
    (add-function (make-function "->" #'fp-cond :precedence 0 :nparam 3))
    (add-function (make-function "/" #'insert :precedence 2 :nparam 1))
    (add-function (make-function "@" #'alpha :precedence 2 :nparam 1))
    ;;----------------------------------------------
    ;; def function, used to add user defined
    ;;----------------------------------------------
    (add-function (make-function "Def" #'def :precedence -1 :nparam 2)))

;;----------------------------------------------
;; reset-functions
;;----------------------------------------------
(defun reset-functions ()
    (defparameter *functions* (make-hash-table :test 'equal))
    (init-functions))

;;----------------------------------------------
;; Call init-functions
;;----------------------------------------------
(init-functions)

;;------------------------------
; fp-funcall
;;------------------------------
(defun fp-funcall (fn &rest args)
    "Enables lazy evaluation for user defined functions"
    (apply (resolve-operand fn) args))

;;------------------------------
; fp-apply
;;------------------------------
(defun fp-apply (fn args)
    "Enables lazy evaluation for user defined functions"
    (apply (resolve-operand fn) args))

;;----------------------------------------------
;; fp-interpret
;;----------------------------------------------
(defun interpret (code)
    (handler-case 
        (let* ((parse-result (parse code))
               (fn (getf parse-result :fn))
               (env (getf parse-result :env)))
            ; Evaluate parse-tree
            (setf fn (evaluate fn))
            ; Evaluate env
            (setf env (evaluate-env env))

            (unless (null fn) (desymbolize (fp-apply fn env))))
        (condition (c) (format t "~a~%" c))))

;;----------------------------------------
;; Evaluates the FP tree
;;----------------------------------------
(defun evaluate (node)
    (cond 
        ((null node) nil)
        (t (let* ((data (datum node))
                  (childs (children node))
                  (fn (get-function data)))
                (cond
                    ((null fn) 
                        (map-to-value data))
                    ((operand? fn)
                        data)
                    (t (apply (getf fn :function) (mapcar #'evaluate childs))))))))

;;----------------------------------------
;; Evaluates the FP env
;;---------------------------------------
(defun evaluate-env (env)
    (symbolize env))

;;----------------------------------------
;; Maps an FP string to a value
;;----------------------------------------
(defun map-to-value (data)
    "Maps a string to a value"
    (cond 
        ((string= data "<>") nil)
        ((numericp data) (if (= (parse-float data :junk-allowed t) (parse-integer data :junk-allowed t))
                             (parse-integer data :junk-allowed t)
                             (parse-float data :junk-allowed t)))
        (t (cond 
                ((equal *true-value* data) *true-value*)
                ((equal *false-value* data) *false-value*)
                ((equal *empty-list-value* data) *empty-list-value*)
                (t data)))))                    

;;----------------------------------------
;; Evaluates the FP tree
;;----------------------------------------
(defun map-to-fp-value (data)
    (cond 
        ((null data) *empty-list-value*)
        ((numberp data) (write-to-string data))
        (t (string data))))

;;----------------------------------------------
;; numericp
;;----------------------------------------------
(defun numericp (str)
    "Returns true if the string is a number"
    (not (null (parse-float str :junk-allowed t))))

;;----------------------------------------------
;; Symbolize
;;----------------------------------------------
(defun symbolize (data)
    "Converts all string elements into symbols or numbers"
    (cond 
        ((null data) data)
        ((atom data) (map-to-value data))
        (t (mapcar #'symbolize data))))

;;----------------------------------------------
;; desymbolize
;;----------------------------------------------
(defun desymbolize (data)
    (cond 
        ((null data) (map-to-fp-value data))
        ((atom data) (map-to-fp-value data))
        (t (let ((result nil))
                (setf result (mapcar #'desymbolize data))
                (setf result (insert-between result ", "))
                (setf result (append '("<") result '(">")))
                (setf result (flatten result))
                (setf result (apply #'concatenate (append '(string) result)))))))

;;----------------------------------------------------
;;;define commands
;;----------------------------------------------------
(defconstant *quit* "quit")
(defconstant *load* "load")
(defconstant *reset* "reset")
(defconstant *help* "help")

;;----------------------------------------------------
;;;fp-repl
;;----------------------------------------------------
(defun fp-repl ()
    (welcome-msg)
    (loop
        (show-prompt)
        (let ((expr (read-line)))
            (cond 
                ((quit? expr) (return "Good bye!"))
                (t (handle-input expr))))))

;;---------------------------------------------------
;;welcome-msg
;;---------------------------------------------------
(defun welcome-msg ()
    (format t "------------------------------~%")
    (format t "Welcome to the FP Interpreter!~%")
    (format t "------------------------------~%")
    (format t "You can type 'help' to get some directions.~%"))

;;---------------------------------------------------
;;show-prompt
;;---------------------------------------------------
(defun show-prompt ()
    (format t "FP> ")
    (finish-output nil))

;;---------------------------------------------------
;;handle-command
;;---------------------------------------------------
(defun handle-input (expr)
    (let* ((expl-expr (string-split expr " "))
            (cmd (first expl-expr)))
        (cond 
            ((empty-string? expr) nil)
            ((equal cmd *help*) (help))
            ((equal cmd *reset*) (reset))
            ((equal cmd *load*) (load-script expl-expr))
            (t (interpret-and-print expr)))))

;;---------------------------------------------------
;;quit?
;;---------------------------------------------------
(defun quit? (expr)
    (equal expr *quit*))

;;---------------------------------------------------
;; help
;;---------------------------------------------------
(defun help ()
    (format t "-----------------------------------~%")
    (format t "FP Interpreter (in LISP)~%")
    (format t "-----------------------------------~%")
    (format t "Type:~%")
    (format t "~%")
    (format t "  help             for this information.~%")
    (format t "~%")
    (format t "  load [filepath]  to load a file containing FP code into the environment.~%")
    (format t "~%")
    (format t "  reset            deletes all user defined functions.~%")
    (format t "~%")
    (format t "  quit             to exit the interpreter.~%")
    (format t "~%")
    (format t "  [expression]     to be evaluated. It can be:~%")
    (format t "                     - User function definition.~%")
    (format t "                     - FP expression with its argument.~%")
    (format t "~%"))

;;---------------------------------------------------
;; interpret-and-print
;;---------------------------------------------------
(defun interpret-and-print (expr)
    (let ((result (interpret expr)))
        (unless (null result) (format t "RESULT: ~a~%" result))))

;;---------------------------------------------------
;; load
;;---------------------------------------------------
(defun load-script (expr)
    (let ((file-path (second expr)))
        (format t "Loading fp script '~a' into environment...~%" file-path)
        (with-open-file (file file-path :direction :INPUT :if-does-not-exist nil)                        
            (cond 
                ((null file) (format t "The file '~a' does not exist.~%" file-path))
                (t (do ((line (read-line file nil) (read-line file nil)))
                        ((null line))
                        (format t "INPUT:  ~a~%" line)
                        (handle-input line))
                    (format t "Script loaded...~%"))))))

;;---------------------------------------------------
;; reset
;;---------------------------------------------------
(defun reset ()
    (format t "Environment reset...~%")
    (reset-functions))

;;----------------------------------------------------
;; Start the repl
;;----------------------------------------------------
(fp-repl)
