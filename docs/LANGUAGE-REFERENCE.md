# Common Lisp Language Reference — LITHP IDE

A concise reference for ANSI Common Lisp as used in the LITHP IDE (SBCL default).

---

## Program Structure

```lisp
;;; package & system definition (for larger projects)
(defpackage #:my-app
  (:use #:cl)
  (:export #:main))
(in-package #:my-app)

;;; Definitions
(defvar *count* 0)
(defparameter *name* "Ada")
(defconstant +max+ 100)

(defun main ()
  (format t "Hello~%"))

;;; Entry
(main)
```

---

## Data Types

| Type            | Examples                      | Predicate        |
| --------------- | ----------------------------- | ---------------- |
| `integer`       | `42`, `-7`, `0`               | `integerp`       |
| `ratio`         | `1/3`, `22/7`                 | `ratiop`         |
| `float`         | `3.14`, `1.0e10`              | `floatp`         |
| `complex`       | `#c(1.0 2.5)`                 | `complexp`       |
| `character`     | `#\A`, `#\Space`, `#\Newline` | `characterp`     |
| `string`        | `"hello"`                     | `stringp`        |
| `symbol`        | `'foo`, `:keyword`            | `symbolp`        |
| `list` / `cons` | `'(1 2)`, `(cons 1 2)`        | `listp`, `consp` |
| `vector`        | `#(1 2 3)`                    | `vectorp`        |
| `array`         | `(make-array '(3 3))`         | `arrayp`         |
| `hash-table`    | `(make-hash-table)`           | `hash-table-p`   |
| `function`      | `#'car`                       | `functionp`      |
| `boolean`       | `t`, `nil`                    | —                |
| `null`          | `nil`                         | `null`           |

---

## Variables and Binding

```lisp
(defvar *x* 0)         ; global, reset only if unbound
(defparameter *y* 1)   ; global, always reset on reload
(defconstant +PI+ 3.14159d0)

; Lexical binding
(let ((a 1) (b 2))
  (+ a b))                   ; => 3

(let* ((a 1) (b (+ a 1)))
  b)                         ; => 2

; Dynamic (special) binding
(defvar *debug* nil)
(let ((*debug* t))           ; temporarily override *debug*
  (do-something))

(setf *x* 42)               ; mutation
(setq *x* 42)               ; equivalent older form
```

---

## Functions

```lisp
; Basic
(defun add (a b) (+ a b))

; Optional arguments
(defun greet (name &optional (greeting "Hello"))
  (format t "~a, ~a!~%" greeting name))

; Rest (variadic)
(defun sum (&rest nums) (apply #'+ nums))

; Keyword arguments
(defun connect (&key (host "localhost") (port 80))
  (format t "Connecting to ~a:~a~%" host port))
(connect :port 8080)

; Multiple return values
(defun divmod (a b) (values (floor a b) (mod a b)))
(multiple-value-bind (q r) (divmod 10 3)
  (format t "~a rem ~a~%" q r))

; Lambda
(funcall (lambda (x) (* x x)) 5)   ; => 25
(mapcar (lambda (x) (* x x)) '(1 2 3))  ; => (1 4 9)

; Function reference
(mapcar #'sqrt '(1.0 4.0 9.0))     ; => (1.0 2.0 3.0)
(funcall #'+ 1 2)                  ; => 3
(apply #'+ '(1 2 3))               ; => 6
```

---

## Special Operators

| Operator                | Description                 |
| ----------------------- | --------------------------- |
| `quote` / `'`           | Suppress evaluation         |
| `if`                    | Basic conditional           |
| `progn`                 | Sequence, return last       |
| `let` / `let*`          | Local binding               |
| `setf`                  | Generalised assignment      |
| `lambda`                | Anonymous function          |
| `function` / `#'`       | Function reference          |
| `block` / `return-from` | Named block with early exit |
| `tagbody` / `go`        | Low-level goto              |
| `unwind-protect`        | Cleanup on exit             |
| `eval-when`             | Control evaluation time     |
| `locally`               | Local declarations          |

---

## Conditionals

```lisp
(if test then else)
(when test body...)       ; if without else; returns nil if false
(unless test body...)     ; inverted when
(cond (test1 e1) (test2 e2) (t default))
(case key
  ((val1 val2) expr1)
  (otherwise   default))

(and expr1 expr2)         ; short-circuit, returns last or nil
(or  expr1 expr2)         ; short-circuit, returns first true

; Typecase
(etypecase x
  (integer (print "int"))
  (string  (print "str")))
```

---

## Loops

```lisp
; Simple recursion — preferred style
(defun count-up (n)
  (when (< n 10)
    (print n)
    (count-up (+ n 1))))

; do — explicit loop
(do ((i 0 (1+ i)))
    ((>= i 5))
  (print i))

; dotimes / dolist
(dotimes (i 5) (print i))
(dolist (x '(a b c)) (print x))

; loop — powerful macro
(loop for i from 1 to 10 do (print i))
(loop for i from 1 to 10 collect (* i i))
(loop for x in '(1 2 3 4 5)
      when (oddp x)
      sum x)              ; => 9

; repeat
(loop repeat 3 do (print "hello"))
```

---

## Lists (Pairs)

```lisp
; Constructors
(cons 1 '(2 3))    ; => (1 2 3)
(list 1 2 3)       ; => (1 2 3)
(make-list 3 :initial-element 0)  ; => (0 0 0)

; Accessors
(car '(1 2 3))      ; => 1
(cdr '(1 2 3))      ; => (2 3)
(cadr x)  (caddr x) (cadddr x)  ; 2nd, 3rd, 4th
(nth 2 '(a b c d))  ; => c  (0-indexed)
(nthcdr 2 '(a b c d)) ; => (c d)
(last '(1 2 3))     ; => (3)  (the list)

; Queries
(length '(1 2 3))   ; => 3
(member 3 '(1 2 3 4))  ; => (3 4) or nil
(assoc 'b '((a 1)(b 2)(c 3)))  ; => (b 2)
(find 3 '(1 2 3 4))   ; => 3 or nil
(position 3 '(1 2 3)) ; => 2

; Manipulation
(append '(1 2) '(3 4))  ; => (1 2 3 4)  — non-destructive
(reverse '(1 2 3))       ; => (3 2 1)   — non-destructive
(sort (list 3 1 2) #'<)  ; => (1 2 3)
```

### Higher-Order
```lisp
(mapcar #'sqrt '(1.0 4.0 9.0))     ; => (1.0 2.0 3.0)
(mapc #'print '(1 2 3))            ; side effects, returns list
(remove-if #'oddp '(1 2 3 4 5))    ; => (2 4)
(remove-if-not #'oddp '(1 2 3 4 5)) ; => (1 3 5)
(reduce #'+ '(1 2 3 4 5))          ; => 15
(every #'numberp '(1 2 3))         ; => T
(some  #'zerop  '(1 0 3))          ; => T
(count-if #'oddp '(1 2 3 4 5))     ; => 3
```

---

## Strings

```lisp
(length "hello")           ; => 5
(char "hello" 0)           ; => #\h
(subseq "hello" 1 3)       ; => "el"
(string-upcase "hello")    ; => "HELLO"
(string-downcase "HELLO")  ; => "hello"
(string-append "foo" "bar")       ; => "foobar"
(concatenate 'string "a" "b" "c") ; => "abc"
(string= "abc" "abc")      ; => T  (case-sensitive)
(string-equal "abc" "ABC") ; => T  (case-insensitive)
(string->list "abc")   ; use (coerce "abc" 'list)
(format nil "~a + ~a = ~a" 1 2 3) ; => "1 + 2 = 3"
(parse-integer "42")       ; => 42
(write-to-string 42)       ; => "42"
```

---

## Vectors and Arrays

```lisp
(vector 1 2 3)              ; => #(1 2 3)
(make-array 5 :initial-element 0)
(make-array '(3 3) :element-type 'fixnum)
(aref v 0)                  ; element access
(setf (aref v 0) 99)        ; set element
(array-dimensions a)        ; => (3 3)
(length v)                  ; for 1-D
(fill v 0)                  ; fill in-place
```

---

## Hash Tables

```lisp
(defparameter *h* (make-hash-table))
(setf (gethash 'key *h*) 42)
(gethash 'key *h*)          ; => 42, T
(gethash 'missing *h* -1)   ; => -1, NIL  (default)
(remhash 'key *h*)
(maphash (lambda (k v) (format t "~a: ~a~%" k v)) *h*)
(hash-table-count *h*)
```

---

## CLOS (Object System)

```lisp
(defclass shape ()
  ((color :initarg :color :initform "black"
          :accessor shape-color)))

(defclass circle (shape)
  ((radius :initarg :radius :accessor circle-radius)))

(defmethod area ((c circle))
  (* pi (expt (circle-radius c) 2)))

(defmethod describe-object ((c circle) stream)
  (format stream "Circle r=~a color=~a"
          (circle-radius c) (shape-color c)))

(defparameter *c* (make-instance 'circle :radius 5 :color "red"))
(area *c*)           ; => 78.53...
(shape-color *c*)    ; => "red"
```

---

## Conditions (Exceptions)

```lisp
; Signal a condition
(error "Something went wrong: ~a" value)
(warn  "Proceed with caution")

; Handle
(handler-case (risky-operation)
  (division-by-zero (c)
    (format t "Caught: ~a~%" c)
    0)
  (error (c)
    (format t "General error: ~a~%" c)))

; Cleanup always runs
(unwind-protect
  (risky-operation)
  (cleanup))

; Establish restarts
(handler-bind ((error #'(lambda (c) (invoke-restart 'use-value 42))))
  (restart-case (error "oops")
    (use-value (v) v)))
```

---

## Macros

```lisp
(defmacro my-and (&rest forms)
  (cond ((null forms) t)
        ((null (cdr forms)) (car forms))
        (t `(if ,(car forms) (my-and ,@(cdr forms)) nil))))

; gensym — avoid variable capture
(defmacro my-when (test &body body)
  (let ((g (gensym)))
    `(let ((,g ,test))
       (when ,g ,@body))))
```

---

## I/O

```lisp
; Output
(print x)                  ; write x, preceding newline
(prin1 x)                  ; write in read-back form
(princ x)                  ; write human-readable
(format t "~a~%" x)        ; formatted output
(write-char #\A)

; Input
(read)                     ; read one Lisp expression
(read-line)                ; read a line of text

; File I/O
(with-open-file (stream "file.txt" :direction :input)
  (loop for line = (read-line stream nil)
        while line do (print line)))

(with-open-file (stream "out.txt"
                 :direction :output
                 :if-exists :supersede)
  (format stream "Hello ~a~%" "World"))
```

### format directives
| Directive | Meaning                         |
| --------- | ------------------------------- |
| `~a`      | Human-readable (princ)          |
| `~s`      | Machine-readable (prin1)        |
| `~d`      | Decimal integer                 |
| `~f`      | Float                           |
| `~e`      | Scientific notation             |
| `~%`      | Newline                         |
| `~&`      | Newline if not at start of line |
| `~t`      | Tab                             |
| `~r`      | Radix (e.g., `~16r` for hex)    |
| `~{~}`    | Iterate over list               |

---

*CLHS (Common Lisp HyperSpec): [lispworks.com/documentation/HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/)*
*Practical Common Lisp: [gigamonkeys.com/book](https://gigamonkeys.com/book/)*
