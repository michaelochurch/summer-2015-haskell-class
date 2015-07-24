(def list (lambda (&rest) &rest))

(def defn
  (lambda (fname args body)
     (list 'def fname (list 'lambda fname args body))))

(set-macro! 'defn)

(def defmacro
  (lambda (fname args body)
    (list 'do
          (list 'def fname (list 'lambda fname args body))
          (list 'set-macro! (list 'quote fname)))))

(set-macro! 'defmacro)

(defn dec (n) (- n 1))

(defn inc (n) (+ n 1))

(defn length (list)
  (if (eq list ()) 0 (inc (length (cdr list)))))

(defn caar (x) (car (car x)))

(defn cadr (x) (car (cdr x)))

(defmacro let (bindings body)
  (if (eq bindings ()) body
      (list (list 'lambda (list (car (car bindings))) (list 'let (cdr bindings) body))
            (car (cdr (car bindings))))))

(defmacro and (&rest)
  (if (eq &rest ()) #t
      (list 'if (car &rest) (cons 'and (cdr &rest)) #f)))

(defmacro or (&rest)
  (if (eq &rest ()) #f
    (let ((sym (gensym)))
      (list 'let (list (list sym (car &rest)))
             (list 'if sym sym (cons 'or (cdr &rest)))))))

(defmacro cond (&rest)
  (if (eq &rest ()) #f
    (list 'if (caar &rest) (cadr (car &rest))
          (cons 'cond (cdr &rest)))))

(defn factorial (n)
  (if (== n 0) 1 (* n (factorial (dec n)))))

(defn twice (f)
  (lambda (x) (f (f x))))

(defn compose (f g)
  (lambda (x) (f (g x))))

(defn ntimes (n f)
  (if (== n 0)
      (lambda (x) x)
      (compose f (ntimes (dec n) f))))

(defn church (n)
  (lambda (f) (ntimes n f)))

(defn unchurch (cn)
  ((cn inc) 0))

(defn range (start end)
  (if (>= start end) ()
    (cons start (range (inc start) end))))

(defn map (f list)
  (if (eq list ()) ()
    (cons (f (car list)) (map f (cdr list)))))

(defn reduce (f z list)
  (if (eq list ()) z
    (reduce f (f z (car list)) (cdr list))))

(defn flip (f)
  (lambda (x y) (f y x)))

(defn last (list)
  (if (eq list ())
    (error "last: applied to empty list")
    (reduce (lambda (x y) y) (car list) (cdr list))))

(defn butlast (list)
  (do
      (if (or (eq list ()) (eq (cdr list) ()))
          ()
          (cons (car list) (butlast (cdr list))))))

(defn append (list1 list2)
  (if (eq list1 ())
      list2
      (cons (car list1) (append (cdr list1) list2))))

(defn concat (lists)
  (reduce append () lists))

(defn apply (f &rest)
  (if (eq &rest ())
    (f)
    (eval (cons f (append (butlast &rest) (last &rest))))))
