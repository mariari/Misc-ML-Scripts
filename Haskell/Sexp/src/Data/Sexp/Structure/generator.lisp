;;; Hello, this is code that generates the functions in Structure.hs
;;; Please ignore this file if you don't want to generate any more.
;;; However, if you want to generate more please provide a spec like

;; (generate-haskell "defunMatch" '("sexp" "argBody") ":defsig-match" :list-star t)

;; the first argument is the constructor name of your data type
;; the second argument is what types it takes in order
;; the third argument is the keyword in syntax if it has one
;;   Note nil implies there is none, and also that no metadata can be
;;        copied over

;; the last argument is an optional keyword that states if the list is
;; arbitrary long rather than a single slot

;; ***********************************************************
;; General Helpers
;; ***********************************************************

(defun repeat (n thing)
  "repeats THING N times"
  (loop for i from 0 to (1- n) collect thing))

(defun header (name)
  (let ((header "----------------------------------------"))
    (format t "~a~%-- ~a~%~a~%" header name header)))

(defun indent (n string)
  (concatenate 'string (apply #'concatenate 'string (repeat n " "))
               string))

(defun capitalize-first (string)
  (let ((new-str (copy-seq string)))
    (setf (elt new-str 0) (char-upcase (elt new-str 0)))
    new-str))

;; taken from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun sep-list-by-space (list)
  (format nil "~{~a~^ ~}" list))

(defun apply-on-last (f list)
  (append (butlast list)
          (list (funcall f (car (last list))))))

;; ***********************************************************
;; Haskell Helpers
;; ***********************************************************

(defun indent-new-lines-by (number str)
  (replace-all str (format nil "~%") (format nil "~%~a" (indent number ""))))

(defun sig (name rhs)
  "sig constructs a haskell signature"
  (indent-new-lines-by
   2
   (format nil "~a :: ~a" name rhs)))

(defun fun (name args rhs)
  "defines a haskell function which takes a name, a list of arguments
and a rhs that may contain a guard, so no = is assumed for the rhs"
  (indent-new-lines-by
   2
   (cond ((listp args)
          (format nil "~a ~{~a ~}~a" name args rhs))
         ((equalp (elt rhs 0) #\newline)
          (format nil "~a ~a~a" name args rhs))
         (t
          (format nil "~a ~a ~a" name args rhs)))))

(defun body (body &optional (new-line t))
  "constructs a haskell definition body"
  (if new-line
      (format nil "=~%~a" body)
      (format nil "= ~a" body)))

(defun case-of (name match-bodies &optional (new-line t))
  "case-of takes a name to match on and a pair of match bodies
(case-of name ((decon1 guard1 … guardn) body2 decon2 body2))"
  (assert (evenp (length match-bodies)))
  (labels ((deconstructor-name (x)
             (if (listp (car x)) (caar x) (car x)))
           (get-guards (x)
             (if (listp x) (cdr x) x))
           (rec (x)
             (if (null x)
                 ""
                 (format nil "~%~a~a~a"
                         (deconstructor-name x)
                         (indent-new-lines-by
                          2
                          (guards (get-guards (car x)) (cadr x) new-line))
                         (rec (cddr x))))))
    (indent-new-lines-by
     2
     (format nil "case ~a of~a" name (rec match-bodies)))))

(defun guards (guards body &optional (new-line t))
  "guards takes a guard body list that looks like
((gurd1 … guardn) body1)"
  (cond ((or (stringp guards) (null guards))
         (format nil (if new-line " ->~%~a" " -> ~a") body))

        ((not (cdr guards))
         (format nil
                 (if new-line
                     "~%| ~a ->~%  ~a"
                     "~%| ~a -> ~a")
                 (car guards)
                 (indent-new-lines-by 2 body)))
        (t
         (format nil
                 (if new-line
                     "~%| ~a,~%~{  ~a~^,~% ~} ->~%  ~a"
                     "~%| ~a,~%~{  ~a~^,~% ~} -> ~a")
                 (car guards)
                 (cdr guards)
                 (indent-new-lines-by 2 body)))))

(defun guard-body (guard body &optional (new-line t))
  "guard form of a defun, very simple, can only handle 1 pattern"
  (format nil "~%| ~a =~a"
          guard
          (indent-new-lines-by
           2
           (format nil (if new-line "~%~a" " ~a") body))))

(defun double-quote (form)
  "dobule-quote surrounds a string with extra quotes"
  (format nil "\"~a\"" form))

(defun -> (&rest args)
  "-> forms a haskell arrow on n arguments"
  (format nil "~{~a~^ -> ~}" args))

(defun maybe (arg)
  "maybe surrounds a value with a Haskell Maybe"
  (format nil "Maybe ~a" arg))

(defun deconstruct-constructor (con-name field-names)
  "generates a pattern match form of a constructor"
  (format nil "(~a ~a)" con-name (sep-list-by-space field-names)))

(defun type-class-of (type-class-name name &rest instances)
  (format nil
          "instance ~A ~A where~A"
          type-class-name
          name
          (indent-new-lines-by
           2
           (format nil "~%~{~A~^~%~}" instances))))

;; ***********************************************************
;; Generator Helpers
;; ***********************************************************

(defun from-name (name)
  (format nil "from~a" (capitalize-first name)))

(defun to-name (name)
  (format nil "to~a" (capitalize-first name)))

;; (defun ignore-sexp-args )

;; ***********************************************************
;; Generator
;; ***********************************************************


;; could be done a LOT better, but works well enough to generate haskell
(defun generate-haskell (con-name pat s-name &key (list-star nil) (un-grouped nil))
  (let ((arg-names
          (mapcar (lambda (x y) (format nil "~a~a" x y))
                  pat
                  (loop for i from 1 to (length pat) collect i)))
        ;; if the pattern is a star we need to apply a function that
        ;; makes it take n arguments
        ;; we worry about grouping, as it ruins constructors if they aren't there...
        (pat-to
          (if (or (not list-star) un-grouped (equal (car (last pat)) "sexp"))
              pat
              (apply-on-last (lambda (x) (format nil "~a `fromStarList`" x)) pat)))
        (pat-from
          (if (or (not list-star) un-grouped (equal (car (last pat)) "sexp"))
              pat
              (apply-on-last (lambda (x) (format nil "~a `toStarList`" x)) pat)))
        (form-name (format nil "name~a" con-name))
        (is-name   (format nil "is~a" con-name))
        (to-name   (to-name con-name))
        (from-name (from-name con-name)))
    (labels ((extra-guards (pat names)
               (mapcan (lambda (pat name)
                         (if (equalp pat "sexp")
                             nil
                             (list
                              (format nil "Just ~a <- ~a ~a" name (to-name pat) name))))
                       pat names))
             (match (names)
               (format nil "~{~a~^ Sexp.:> ~}"
                       (append (when s-name (list (format nil "_name~a" con-name)))
                               names
                               (unless list-star '("Sexp.Nil")))))
             (from-construction (pat names)
               (format nil "~{~a~^, ~}"
                       (append (when s-name
                                   (list (format nil "Sexp.atom ~a" form-name)))
                             (mapcar (lambda (p n)
                                       (if (equalp p "sexp")
                                           n
                                           (format nil "~a ~a" (from-name p) n)))
                                     pat
                                     names)))))
      (let* ((to-body
               (case-of "form"
                        (list (cons (match arg-names)
                                    (extra-guards pat-to arg-names))
                              (format nil "~a ~{~a~^ ~} |> Just"
                                      con-name
                                      arg-names)
                              "_"
                              "Nothing")))
             (to-guard
              (if s-name
                  (concatenate 'string
                               (guard-body (format nil "~a form" is-name)
                                           to-body)
                               (guard-body "otherwise" "Nothing"))
                  (body to-body)))
             (from-body
              (format nil "~a [~a]"
                      (if list-star "Sexp.listStar" "Sexp.list")
                      (from-construction pat-from arg-names))))
        (header con-name)
        (format t "~%")
        (format t "~{~a~^~%~}"
                (append
                 (when s-name
                   (list
                    ;; name<form>
                    (sig form-name "NameSymbol.T")
                    (fun form-name nil (body (double-quote s-name) nil))
                    ""
                    ;; is<Form>
                    (sig is-name (-> "Sexp.T" "Bool"))
                    (fun is-name
                         (deconstruct-constructor "Sexp.Cons" '("form" "_"))
                         (body (format nil "Sexp.isAtomNamed form ~a" form-name) nil))
                    (fun is-name "_" (body "False" nil))
                    ""))
                 (list
                  ;; to<Form>
                  (sig to-name (-> "Sexp.T" (maybe con-name)))
                  (fun to-name "form" to-guard)
                  ""
                  ;; from<From>
                  (sig from-name (-> con-name "Sexp.T"))
                  (fun from-name
                       (deconstruct-constructor con-name arg-names)
                       (body from-body))
                  ""
                  ;; Type class Instance
                  (type-class-of "Sexp.Serialize"
                                 con-name
                                 (fun "deserialize" nil (body (to-name con-name) nil))
                                 (fun "serialize" nil (body (from-name con-name) nil)))
                  "")))))))

(defun parsing-types ()
  (generate-haskell "Type" (repeat 3 "sexp") "type" :list-star t)

  (generate-haskell "LetType" (repeat 4 "sexp") ":let-type")

  (generate-haskell "Include" '("nameSymbol") ":include")

  (generate-haskell "Alias" '("nameSymbol" "nameSymbol") ":alias")

  (generate-haskell "Defun" '("sexp" "sexp" "sexp") ":defun")

  (generate-haskell "Signature" '("sexp" "sexp") ":defsig")

  (generate-haskell "LetSignature" (repeat 3 "sexp") ":let-sig")

  (generate-haskell "Let" (repeat 4 "sexp") "let")

  ;; here we assume predAns is not a list of sexps for the answer, is
  ;; this actually acurate. Future refactor plan
  (generate-haskell "PredAns" (repeat 2 "sexp") nil)

  (generate-haskell "Cond" '("predAns") ":cond" :list-star t)

  (generate-haskell "DeconBody" (repeat 2 "sexp") nil)

  (generate-haskell "Case" '("sexp" "deconBody") "case" :list-star t)

  (generate-haskell "Arrow" '("sexp" "sexp") "%<-")

  (generate-haskell "Lambda" '("sexp" "sexp") ":lambda")

  (generate-haskell "Punned" '("sexp") nil)

  (generate-haskell "NotPunned" '("sexp" "sexp") nil)

  (generate-haskell "NameUsage" (repeat 3 "sexp") nil)

  (generate-haskell "Record" '("nameBind") ":record" :list-star t)

  (generate-haskell "Infix" (repeat 3 "sexp") ":infix")

  (generate-haskell "OpenIn" (repeat 2 "sexp") ":open-in")

  (generate-haskell "Open" '("sexp") "open")

  (generate-haskell "Declare" '("sexp") "declare")

  (generate-haskell "Declaim" (repeat 2 "sexp") ":declaim")

  (generate-haskell "DefModule" (repeat 3 "sexp") ":defmodule" :list-star t)

  (generate-haskell "Do" '("sexp") ":do" :list-star t)

  (generate-haskell "LetModule" (repeat 4 "sexp") ":let-mod")

  (generate-haskell "Effect" (repeat 2 "sexp") ":defeff")

  (generate-haskell "DefHandler" (repeat 2 "sexp") ":defhandler")

  (generate-haskell "LetRet" (repeat 2 "sexp") ":defret")

  (generate-haskell "LetOp" (repeat 3 "sexp") ":defop")

  (generate-haskell "RecordDec" '("nameUsage") ":record-d" :list-star t)

  (generate-haskell "Primitive" '("sexp") ":primitive")

  (generate-haskell "Binder" '("nameSymbol" "sexp") ":<-")

  (generate-haskell "DoDeep" '("doBodyFull") ":do" :list-star t)

  (generate-haskell "DoPure" '("sexp") ":do-pure")

  (generate-haskell "DoOp" (repeat 2 "sexp") ":do-op")

  (generate-haskell "Via" (repeat 2 "sexp") ":via")

  (generate-haskell "Header" '("nameSymbol" "sexp") ":header" :list-star t))

(defun transition-types ()
  (generate-haskell "ArgBody" '("sexp" "sexp") nil)

  (generate-haskell "DefunMatch" '("sexp" "argBody") ":defun-match" :list-star t)

  (generate-haskell "If" (repeat 3 "sexp") "if")

  (generate-haskell "IfNoElse" (repeat 2 "sexp") "if")

  (generate-haskell "DefunSigMatch" '("sexp" "sexp" "argBody") ":defsig-match" :list-star t)

  (generate-haskell "LetMatch" '("sexp" "argBodys" "sexp") ":let-match")

  (generate-haskell "RecordNoPunned" '("notPunnedGroup") ":record-no-pun"
                    :list-star t
                    :un-grouped t)

  (generate-haskell "LambdaCase" '("argBody") ":lambda-case" :list-star t)

  (generate-haskell "LetHandler" (repeat 3 "sexp") ":lethandler")

  (generate-haskell "Handler" '("sexp" "letRet" "letOp") ":lethandler" :list-star t)

  (generate-haskell "SumCon" '("nameSymbol") ":sum-con")

  (generate-haskell "SumConFilled" '("nameSymbol" "sexp") ":sum-con-filled")

  (generate-haskell "InPackage" '("nameSymbol") ":in-package"))

(defun berlin-types ()
  (generate-haskell "Relocated" '("nameSymbol") ":relocated"))

(defun core-named-representation ()
  (generate-haskell "Star" '("integer") ":star")

  (generate-haskell "PrimTy" '("sexp") ":prim-ty")

  (generate-haskell "Prim" '("sexp") ":prim")

  (generate-haskell "Pi" '("binder" "sexp") ":pi")

  (generate-haskell "Binder" '("nameSymbol" "sexp" "sexp") nil)

  (generate-haskell "Lam" '("nameSymbol" "sexp") ":named-lambda")

  (generate-haskell "Sigma" '("binder" "sexp") ":sigma")

  (generate-haskell "Pair" (repeat 2 "sexp") ":pair")

  (generate-haskell "Let" '("binder" "sexp") ":named-let")

  (generate-haskell "Var" '("nameSymbol") nil)
  ;; do we need to generate this?
  (generate-haskell "App" (repeat 2 "sexp") nil)

  (generate-haskell "Ann" '("sexp" "sexp") ":")

  (generate-haskell "Meta" '("sexp" "integer") nil)

  (generate-haskell "Field" (list "nameSymbol" "sexp" "sexp") nil)

  (generate-haskell "RecordTy" (list "field") ":record-ty" :list-star t)

  (generate-haskell "Lookup" (list "sexp" "symbol") ":lookup" :list-star t)

  (generate-haskell "CatProduct" (repeat 2 "sexp") ":cat-product")

  (generate-haskell "CatProductIntro" (repeat 2 "sexp") ":cat-product-intro")

  (generate-haskell "CatProductElimLeft" (repeat 2 "sexp") ":cat-elim-left")

  (generate-haskell "CatProductElimRight" (repeat 2 "sexp") ":cat-elim-right")

  (generate-haskell "CatCoProduct" (repeat 2 "sexp") ":cat-coproduct")

  (generate-haskell "CatCoproductIntroLeft" (repeat 1 "sexp") ":cat-intro-left")

  (generate-haskell "CatCoproductIntroRight" (repeat 1 "sexp") ":cat-intro-right")

  (generate-haskell "CatCoproductElim" (repeat 5 "sexp") ":cat-coelim"))
