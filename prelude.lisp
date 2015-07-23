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

(defmacro and (&rest)
  (if (eq &rest ()) #t
      (list 'if (car &rest) (cons 'and (cdr &rest)) #f)))
