# Common Lisp Tutorial — LITHP IDE

This tutorial teaches Common Lisp using the LITHP IDE. Common Lisp is a powerful, multi-paradigm language with an interactive REPL, a rich standard library, and a macro system that lets you extend the language itself.

---

## Getting Started

Open LITHP (`./run.sh`). A **SBCL REPL** appears in the lower terminal panel with a `*` prompt. You can:

- Type expressions directly in the terminal.
- Write code in the editor, select it, and press **`Ctrl+Enter`** to send it to the REPL.
- Press **`Ctrl+L`** to load the current file into the REPL.
- Press **`Ctrl+C`** to interrupt a running computation.

---

## Lesson 1 — The REPL

Common Lisp uses **Read-Eval-Print Loop** (REPL). Type an expression, press Enter:

```lisp
* (+ 1 2)
3
* (* 6 7)
42
* "Hello, World!"
"Hello, World!"
* (format t "Hello, World!~%")
Hello, World!
NIL
```

**Everything is an expression.** The last evaluated form's value is returned. `format t` prints to stdout; `~%` is a newline.

---

## Lesson 2 — Atoms and Lists

```lisp
; Atoms
42           ; integer
3.14         ; float
"hello"      ; string
#\A          ; character
T            ; true
NIL          ; false / empty list
'hello       ; symbol (quoted = not evaluated)

; Lists (code AND data have the same syntax!)
'(1 2 3)            ; a list of 3 numbers
'(a b c)            ; a list of 3 symbols
(list 1 2 3)        ; => (1 2 3) — evaluated
```

---

## Lesson 3 — Variables

```lisp
; Global variable
(defvar *pi* 3.14159)
(defparameter *name* "Ada Lovelace")

; Local binding
(let ((x 10)
      (y 20))
  (+ x y))   ; => 30

; Sequential binding
(let* ((x 5)
       (y (* x 2)))
  y)          ; => 10

; Mutation
(setf *pi* 3.14159265)
```

> **Convention:** Global variables use `*earmuffs*` — `*like-this*`.

---

## Lesson 4 — Functions

```lisp
; Define a function
(defun square (n)
  (* n n))

(square 5)   ; => 25

; Optional and keyword arguments
(defun greet (name &optional (greeting "Hello"))
  (format t "~a, ~a!~%" greeting name))

(greet "Grace")         ; Hello, Grace!
(greet "Ada" "Hi")      ; Hi, Ada!

; Anonymous function (lambda)
(funcall (lambda (x) (* x x)) 6)  ; => 36
(mapcar #'square '(1 2 3 4 5))    ; => (1 4 9 16 25)
```

---

## Lesson 5 — Conditionals

```lisp
; if
(if (> 5 3) "yes" "no")   ; => "yes"

; cond (multi-branch)
(cond
  ((> x 100) "huge")
  ((> x 10)  "big")
  (t          "small"))   ; t is always truthy — default branch

; case
(case (mod n 2)
  (0 "even")
  (1 "odd"))

; when / unless (no else)
(when (> x 0) (print "positive"))
(unless (zerop x) (print "non-zero"))

; and / or (short-circuit, return last value)
(and 1 2 3)    ; => 3
(or nil nil 5) ; => 5
```

---

## Lesson 6 — Recursion

```lisp
; Factorial
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 10)  ; => 3628800

; Tail-recursive with accumulator
(defun fact (n &optional (acc 1))
  (if (<= n 1) acc (fact (- n 1) (* n acc))))

; Fibonacci
(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))
```

---

## Lesson 7 — Lists

Lists are fundamental to Lisp (the name stands for **LIS**t **P**rocessing):

```lisp
(car '(1 2 3))          ; => 1         (first element)
(cdr '(1 2 3))          ; => (2 3)     (rest of list)
(cadr '(1 2 3))         ; => 2         (second element)
(cons 0 '(1 2 3))       ; => (0 1 2 3) (prepend)
(append '(1 2) '(3 4))  ; => (1 2 3 4) (join)
(length '(1 2 3))       ; => 3
(nth 2 '(a b c d))      ; => c         (0-indexed)
(reverse '(1 2 3))      ; => (3 2 1)
(last '(1 2 3))         ; => (3)       (returns a list!)
(member 3 '(1 2 3 4))   ; => (3 4)     (sublist from first match)
```

### Higher-Order List Functions
```lisp
(mapcar #'sqrt '(1 4 9 16))        ; => (1.0 2.0 3.0 4.0)
(remove-if #'oddp '(1 2 3 4 5))    ; => (2 4)
(reduce #'+ '(1 2 3 4 5))         ; => 15
(every  #'numberp '(1 2 3))        ; => T
(some   #'zerop   '(1 0 3))        ; => T
```

---

## Lesson 8 — Structures and Classes

```lisp
; defstruct (simple record)
(defstruct person
  name
  age)

(defparameter *p* (make-person :name "Grace" :age 85))
(person-name *p*)   ; => "Grace"
(person-age  *p*)   ; => 85

; CLOS — Common Lisp Object System
(defclass animal ()
  ((name  :initarg :name  :accessor animal-name)
   (sound :initarg :sound :accessor animal-sound)))

(defmethod speak ((a animal))
  (format t "~a says ~a~%" (animal-name a) (animal-sound a)))

(defparameter *dog* (make-instance 'animal :name "Rex" :sound "Woof"))
(speak *dog*)  ; Rex says Woof
```

---

## Lesson 9 — Macros

Macros receive **code as data** and transform it:

```lisp
; Simple macro
(defmacro my-when (condition &body body)
  `(if ,condition (progn ,@body)))

(my-when (> 5 3)
  (print "five is bigger")
  (print "than three"))

; do loop macro
(do ((i 0 (1+ i)))
    ((>= i 5))
  (print i))  ; prints 0 1 2 3 4

; loop macro (powerful built-in)
(loop for i from 1 to 5
      collect (* i i))  ; => (1 4 9 16 25)

(loop for x in '(1 2 3 4 5)
      when (oddp x)
      sum x)  ; => 9
```

---

## Next Steps

- Load examples in LITHP with `Ctrl+L`.
- Read the [Common Lisp Language Reference](LANGUAGE-REFERENCE.md).
- Explore the `loop` macro, file I/O with `with-open-file`, and CLOS inheritance.
