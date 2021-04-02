;;;;; FUNKTION auf einer Liste anwenden (einfach)
(defun check (func list)
  (mapcar #'(lambda (e) (funcall func e)) list))


;;;;; FUNKTION auf einer Liste anwenden (geschachtelt)
(defun check (func lst)
  (mapcar #'(lambda (e) (if (consp e) 
                            (check func e) 
                            (funcall func e)))
          lst))

(defun check(funk argumentliste)
    (mapcar #'(lambda (x)
        (cond((listp x) (check funk x)) ; if
            ((atom x) (funcall funk x)) ; if-else
            (t nil)))                   ; else
            argumentliste)
)

;;;;; Summe auf eine Liste anwenden (einfach)
(defun summing(argumentliste)
    (let ((sum 0))
        (mapcar #'(lambda (x) (setf sum (+ x sum))) argumentliste)
    (print sum)
    )
)

(let ((l '(1 2 3 4 5)))
    (let ((sum 1))
        (mapcar #'(lambda (x) (setf sum (+ x sum))) l)
    (print sum)))

(defun sum1 (list)(cond ((null list) 0)(t (+ (first list) (sum (rest list))))))


;;;;; Summe auf eine Liste anwenden (geschachtelt) mit mapcar
(defun summingR(argumentliste)
    (let ((sum 0))
        (mapcar #'(lambda (x)
            (if(listp x) (setf sum (+ sum (summingR  x)))
                         (setf sum (+ x sum))))
            argumentliste)
    sum)
)


;;;;; Summe auf eine Liste anwenden (geschachtelt) mit Rekursion
(defun summingR (liste)
  (cond
    ((null liste) 0)
    ((listp (first liste)) (+ (summingR (first liste)) (summingR (rest liste))))
    (t (+ (first liste) (summingR (rest liste))))
  )
)


;;;;; Zählen von Elemente in einer Liste (einfach und geschachtelt)
(defun count-atoms (liste)
  (cond 
    ((null liste) '0)
    ((atom liste) '1)
    (t (apply #'+ (mapcar #'count-atoms liste)))
  )
)


;;;;; Zähle nur Zahlen aus einer Liste (einfach und geschachtelt)
(defun count-atoms (liste)
  (cond 
    ((null liste) '0)
    ((atom liste) (if (numberp liste) '1 '0))
    (t (apply #'+ (mapcar #'count-atoms liste)))
  )
)


;;;;; Vergleiche zwei Listen miteinander (einfach)
(defun vergleiche(liste1 liste2)
  (mapcar #'(lambda (x y) (equal x y)) liste1 liste2)
)


;;;;; Vergleiche zwei Listen miteinander (geschachtelt)
(defun vergleicheR(liste1 liste2)
    (if (or (and (consp liste1) (consp liste2)) (and (atom liste1) (atom liste2))) 
      (mapcar #'(lambda (x y)
            (cond 
              ((consp x) (vergleicheR x y))
              ((atom x) (equal x y))
              (t nil)))
                liste1 liste2))
)


;;;;; Funktion und Liste als Parameter - (anwenden 'first '((a b c) (d e f))) => (A D)
(defun anwenden(funk liste)
  (if (endp liste)
    ()              ; funcall: Parameter in passender Form übergeben, einzelübergabe
    (cons (funcall funk (first liste)) (anwenden funk (rest liste)))))


(defun anwenden (funk liste)
  (if (endp liste)
    ()            ; apply: Paramter als Liste, wird als Liste übergeben
    (cons (apply funk(list (first liste))) (anwenden funk (rest liste)))))


;;;;; Zählen von bestimmten Elementen (zwei Variationen) in eine Liste (einfach und geschachtelt) mit apply
(defun anz-atome (L)
  (cond ((null L) 0)
        ((atom L) 1)
  (t (apply '+ (mapcar 'anz-atome L)))))

(defun anz-atome (L)
  (cond ((null L) 0)
        ((symbolp L) 0)
        ((numberp L) 1)
  (t (apply '+ (mapcar 'anz-atome L)))))


;;;;; Entfernen von Elementen, wenn sie bestimmte Bedingung nicht erfüllen (einfache Liste):
(defun remove-if-not1(test liste)
  (mapcar #'(lambda (x) (if (funcall test x) x)) liste))        ; => Ersetzt nicht-zutreffende Elemente mit NIL

(defun remove-if-not2(test liste)
  (mapcan #'(lambda (x) (if (funcall test x) (list x))) liste)) ; => Entfernt nicht-zutreffende Elemente
(remove-if-not2 #'numberp '(a 6 (2 3) 99))

(defun entferne(liste)                                          ; => ohne mapcan / mapcar 
  (cond
    ((null liste) nil)
    ((numberp (first liste)) (entferne (rest liste)))
    ((listp (first liste)) (append (list (entferne (first liste))) (entferne (rest liste))))
    (t (append (list (first liste)) (entferne (rest liste))))
  )
)


;;;;; Berechnen der Schnittmenge
(defun intersect  (A B)
  (if (eq A ())
      A
      (if (member (car A) B)
          (cons (car A) (intersect (cdr A) B))
(intersect (cdr A) B))))


;;;;; Durchschnitt berechnen
(defun median (liste)
  (let ((sum 0))
    (mapcar #'(lambda(x) (setf sum (+ sum x))) liste)
    (print (/ sum 3))
  )
)


;;;;; Berechne das zweitgrößte Element aus einer Liste (Liste mit stets drei Elemente)
(defun median (liste)
  (second (sort liste #'<))
)


;;;;; Erzeugen von Paaren '(1 2 3 4 5 7) => '((1 2)(3 4)(5 6)(7 NIL))
(defun paar (liste)
    (cond ((null liste) nil)
    (t (cons (list (first liste) (second liste)) (paar (rest (rest liste)))))))


;;;;; Sortieren von Paaren (non-rekursiv) '((2 1)(3 1)(2 4)(2 2)) => '((1 2)(1 3)(2 4)(2 2))
(defun sortpair (liste)
  (mapcar #'(lambda(x) 
          (cond 
              ((> (first x) (second x)) (list (second x) (first x)))
              (t (list (first x) (second x))))) liste)
)


;;;;; Kombination mapcar + Rekursion: '((a 1) (b 2) (c 3))) => ((NIL T) (NIL T) (NIL T))
(defun anwenden-deep (funk lst)
  (mapcar #'(lambda (e) (if (consp e) (anwenden-deep funk e) (funcall funk e))) lst))


; Entferne Zahlen aus einer geschachtelten Liste (a (1 b) c) => (a (nil b) c) mit mapcar
(defun entferne (liste)
  (cond 
    ((null liste) 0)
    (t 
      (mapcar #'(lambda (e)
        (if (listp e) (entferne e)
          (if (symbolp e) nil e))
      )
    liste))
  )
)


;;;;; Entferne Zahlen aus einer geschachtelten Liste (a (2 e 4) b 3)) => (a (e) b) - ohne mapcar
(defun entferne(liste)
  (cond
    ((null liste) nil)
    ((numberp (first liste)) (entferne (rest liste)))
    ((listp (first liste)) (append (list (entferne (first liste))) (entferne (rest liste))))
    (t (append (list (first liste)) (entferne (rest liste))))
  )
)


;;;;; Summiere alle Zahlen aus einer Liste (einfach und geschachtelt)
(defun summingR (liste)
  (cond
    ((null liste) 0)
    ((listp (first liste)) (+ (summingR (first liste)) (summingR (rest liste))))
    (t (+ (first liste) (summingR (rest liste))))
  )
)


;;;;; Bilde Vereinigung aus zwei Mengen
(defun vereinige(menge1 menge2)
    (let ((ergebnis '()))
    (cond
      ((null menge1) nil)
      (cond ((member (first menge1)) (append ergebnis (vereinige (rest menge1) menge2))
      (t (vereinige (rest menge1) (menge2)))
      ))
  )
  )
)

;;;;; Entfernt aus einer Liste alle Atome, behält nur die Listen in der übergebene Liste
(defun entferneAtomeVonListe (lst)
  (if (null lst) 
      nil
      (if (not (listp (first lst))) 
          (entferneAtomeVonListe (rest lst))
          (cons (first lst) (entferneAtomeVonListe (rest lst))))))


;;;;; Entfernt aus einer Liste alle Atome, behält nur die Listen in der übergebene Liste und flacht diese Liste ab, 
;;;;; so dass am Ende nur noch eine Liste steht
(defun eliminateAtomsFromList-mapcan (lst)
  (mapcan #'(lambda (e) (if (null e) (list e) (if (listp e) e))) lst))


;;;;; Kehrt eine Liste rekursiv um (einfache Liste) 
(defun my-reverse(liste)
    (cond ((null liste) nil)
    (t (append (my-reverse(rest liste)) (list (first liste)))))
)


;;;;; Zählt Elemente in einer Liste rekursiv (einfach)
(defun my-reverse(liste)
    (cond ((null liste) nil)
    (t (append (my-reverse(rest liste)) (list (first liste)))))
)


;;;;; Zählt Elemente in einer Liste rekursiv (geschachtelt)
(defun nested-reverse(liste)
    (cond ((null liste) liste)
        ((consp (first liste))
            (append (nested-reverse (rest liste)) (list (nested-reverse(first liste)))))
    (t (append (nested-reverse (rest liste)) (list (first liste)))))
)