;;; elisplisp.el --- a little lisp using elisp to do all the hard work

;; Copyright (C) 2014  Jordon Biondo

;; Author: Jordon Biondo <biondoj@mail.gvsu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(defvar ell:vars ({ })
  "Global variable lookup.")

(defvar ell:stack '()
  "List of stack frames to push and pop from. For local bindings.")

(defun ell:new-env ()
  "Not used yet."
  `((:globals . ,({ }))
    (:stack . nil)))


(defun ell:vars-get (var)
  "Get the global value of VAR."
  (if (<<? ell:vars var)
      (cons 'value (<< ell:vars var))
    (cons 'error "not found.")))


(defun ell:vars-set (var value &optional global create)
  "Set VAR to VALUE in the stack, or GLOBALLY, CREATE in the current frame if desired."
  (if (or global (= 0 (length ell:stack)))
      (>> ell:vars var value)
    (ell:stack-set var value nil nil create)))

(defun ell:stack-in ()
  "Push a new stack frame."
  (push ({ }) ell:stack))

(defun ell:stack-out ()
  "Pop out of the current stack frame."
  (pop ell:stack))

(defun ell:stack-wall ()
  "Push a wall into the stack."
  (push 'wall ell:stack))

(defun ell:stack-get (var &optional frame scanning) "
Scan up the stack frames to find VAR's value, if it is not found before hitting the end
of the stack or a wall, lookup in the global table."
  (let ((frame (or frame scanning ell:stack)))
    (if (hash-table-p (car-safe frame))
        (if (<<? (car frame) var)
            (cons 'value (<< (car frame) var))
          (ell:stack-get var (cdr frame) t))
      (ell:vars-get var))))

(defun ell:stack-set (var value &optional frame scanning create) "
Set VAR to VALUE at the first instance of VAR on the stack, or CREATE the binding at the current frame.
SCANNING is for internal use only."
  (let ((frame (or frame scanning ell:stack)))
    (if (and (not create) (hash-table-p (car-safe frame)))
        (if (<<? (car frame) var)
            (>> (car frame) var value)
          (ell:stack-set var value (cdr frame) t))
      (>> (car ell:stack) var value))))

;; (defun dump-frame (&optional n)
;;   (let ((stacky (reverse ell:stack)))
;;     (dotimes (x (or n 1))
;;       (when (> (length stacky) 0)
;;  (dolist (key (<<keys (car stacky)))
;;    (message "%s%s:    %s" (make-string x ?\t) key (<< (car stacky) key))
;;    (pop stacky))))))

(defun ell:get (var)
  "Get the value of VAR from the stack or global list."
  (ell:stack-get var))

(defun ell:set (var value &optional create)
  "Set VAR to VALUE in the nearest stack frame, if CREATE is non nil, create the var on the current frame."
  (ell:vars-set var value nil create))

(defun ell:error-p (obj) "
Is obj a ell error?"
  (and (consp obj) (equal (car obj) 'error)))

(defun ell:docall (expr)
  "evaluate an expression that is a funciton call in the form (func . args)."
  (let ((sym-val (ell:eval (car expr)))) ;; get function from symbol
    (if (functionp sym-val)
        (apply sym-val (mapcar 'ell:eval (cdr expr))) ;; evaluate arg exprs and pass into function call
      (error (format "Symbols has no value as a function: %s" (car expr))))))

(defun ell:func-info (func)
  "Returns an alist with the 'args and 'body of FUNC"
  `((args . ,(second (if (and func (symbolp func)) (symbol-function func) func)))
    (body . ,(cddr   (if (and func  (symbolp func)) (symbol-function func) func)))))

(defun ell:native-func-p (func)
  "Return non-nil if FUNC is a ell native function expression: (native . symbol)"
  (and (equal (car func) 'native)
       (symbolp (cdr func))))

(defun ell:docall (expr)
  (let ((func (ell:eval (car expr))))
    (if (ell:native-func-p func)
        (apply (cdr func) (mapcar 'ell:eval (cdr expr)))
      (let ((info (ell:func-info func)))))))

(defun ell:docall (expr)
  (let ((func (ell:eval (car expr))))
    (if (ell:native-func-p func)
        (apply (cdr func) (mapcar 'ell:eval (cdr expr)))
      (let* ((info (ell:func-info func))
             (args (cdr (assoc 'args info)))
             (body (cdr (assoc 'body info))))
        (assert (= (length args) (length (cdr expr))) nil 
                "incorrect number of args : FIX ME LATER")
        (ell:stack-in)
        (dolist (value (mapcar 'ell:eval (cdr expr)))
          (ell:stack-set (pop args) value ell:stack nil t))
        (let ((result (ell:eval (cons 'elldo body))))
          (ell:stack-out)
          result)))))

(defun ell:let (decs body)
  "Implemented let."
  (ell:stack-in) ;; push a new stack frame
  (dolist (dec decs) ;; bind the keys in decs to their evaluated values
    (ell:set (car dec) (ell:eval (cadr dec)) t))
  ;;(dump-frame 4)
  (let ((retval (first (last (mapcar 'ell:eval body))))) ;; eval each body expr
    (ell:stack-out) ;; pop stack frame
    retval)) ;; return value of the last body call



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top level evaluations, currently, functions evaluation is handled
;; by emacs lisp using apply, but variables and scope are all handled
;; by ell:lisp
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ell:eval-quote     (expr) (cadr expr))
(defun ell:eval-let       (expr) (ell:let (cadr expr) (cddr expr)))
(defun ell:eval-if        (expr) (ell:eval (if (ell:eval (cadr expr)) 
                                               (caddr expr)
                                             (cons 'elldo (cdddr expr)))))
(defun ell:eval-lambda    (expr) (eval expr))
(defun ell:eval-set       (expr) (ell:set (ell:eval (second expr)) (ell:eval (third expr))))
(defun ell:eval-setq      (expr) (ell:set (second expr) (ell:eval (third expr))))
(defun ell:eval-def       (expr) (ell:vars-set var value t create))
(defun ell:eval-elldo     (expr) (first (last (mapcar 'ell:eval (cdr expr)))))
(defun ell:eval-funcall   (expr) (ell:docall expr))
(defun ell:eval-string    (expr) expr)
(defun ell:eval-number    (expr) expr)
(defun ell:eval-symbol    (expr)
  (let ((value (ell:get expr)))
    (if (ell:error-p value)
        (error (format "undefined symbol %s" expr))
      (cdr value))))

(defun ell:eval (expr)
  "This is terrible for now."
  (cond
   ((listp expr) (case (car expr)
                   ;; special forms
                   ('quote    (ell:eval-quote expr))
                   ('let      (ell:eval-let expr))
                   ('if       (ell:eval-if expr))
                   ('lambda   (ell:eval-lambda expr))
                   ('set      (ell:eval-set expr))
                   ('setq     (ell:eval-setq expr))
                   ('def      (ell:eval-def expr))
                   ('elldo    (ell:eval-elldo expr))
                   (otherwise (ell:eval-funcall expr))))
   ((symbolp expr) (ell:eval-symbol expr))
   ((stringp expr) (ell:eval-string expr))
   ((numberp expr) (ell:eval-number expr))
   (t (error "Don't know what to do with %S" expr))))

(defmacro ell (&rest body)
  (declare (indent defun))
  `(ell:eval '(elldo ,@body)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "natively implemented functions" (emacs-lisp)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (sym '(+ - / * mod % and or print car cdr cons format
                 > < >= <=
                 first second third fourth fifth
                 sixth seventh eighth ninth tenth nth
                 caar caaar caaaar
                 cddr cdddr cddddr
                 cadr caddr cadddr
                 cdar cdaar cdaaar
                 concat format append list))
  (>> ell:vars sym (cons 'native sym)))


  (provide 'elisplisp)
;;; elisplisp.el ends here
