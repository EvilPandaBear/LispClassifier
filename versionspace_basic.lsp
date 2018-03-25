; Load-Exampleset 
; ---------------
; Liest eine Beispieldatei und trennt die Kopfzeile ab
; Wert von Load-Exampleset ist eine Liste (was sonst?)
; 1. Element : Eine Liste mit einer Liste aller Attributnamen und einem Bezeichner "Teacher"
; 2. Element : Eine Liste von Listen, die jeweils einem bewerteten Beispiel entsprechen
; Ein bewertetes Beispiel ist eine Liste der Attributwerte sowie eine Bewertung "ja" "nein"

(setq *path-to-vs* "C:/TEMP/ball_lernen.lsp")

(DEFUN  LOAD-EXAMPLESET (Filename)
   (LET ((STREAM (OPEN Filename :DIRECTION :INPUT)))
         (LET ((CATEGORYNAMES (READ STREAM NIL STREAM))
               (EXAMPLELIST   (READ-EXAMPLELIST STREAM)))
              (PROGN (CONS CATEGORYNAMES EXAMPLELIST)))))

(DEFUN READ-EXAMPLELIST (STREAM)
  (LET ((READLINE (READ STREAM NIL STREAM)))
       (COND ((EQ READLINE STREAM) (PROGN (CLOSE STREAM) NIL))
             (T (CONS READLINE 
                      (READ-EXAMPLELIST STREAM))))))

; Version Space

(setq *Star* '*Star*)
(setq *Floor* '*Floor*)

(defun generalize (Example Concept)
  (cond ((OR (NULL Example) (NULL Concept)) NIL)
        ((Equal (car Example) *Floor*) 
         (cons (car Concept) (generalize (cdr Example) (cdr Concept))))
        ((Equal (car Concept) *Floor*)
         (cons (car Example) (generalize (cdr Example) (cdr Concept))))
        ((Equal (car Example) (car Concept))
         (cons (car Example) (generalize (cdr Example) (cdr Concept))))
        (T (cons *Star* (generalize (cdr Example) (cdr Concept))))))

(defun includes (Hypothesis Example)
   (cond ((NULL Hypothesis) T)
         ((OR (Equal (car Hypothesis) (car Example))
              (Equal (car Hypothesis) *Star*))
          (includes (cdr Hypothesis) (cdr Example)))
         (T NIL)))

(defun specialize (NegativeExample S Hy)
  (cond ((null S) NIL)
        ((not (includes Hy NegativeExample)) (list Hy))
        (T (append (specialize-single NegativeExample (car S) Hy)
                   (specialize NegativeExample (cdr S) Hy)))))

(defun specialize-single (NegativeExample MostGeneralSpecialization Hypothesis)
  (specialize-help NegativeExample MostGeneralSpecialization Hypothesis NIL NIL))

; Voraussetzung: Hy umfasst das negative Beispiel !
;                MGS umfasst es nicht !

(defun specialize-help (NegE MGS Hy StartOfH ListOfHypothesis)
;  (print NegE)(print MGS) (print Hy) (print StartOfH) (print ListOfHypothesis)(terpri)
  (cond ((NULL Hy) ListOfHypothesis)
        ((OR (Equal (car NegE) (car Hy))(Equal (car Hy) (car MGS)))
         ; finde spätere Unterscheidung
         (specialize-help (cdr NegE) (cdr MGS) (cdr Hy) (append StartOfH (List (car Hy))) ListOfHypothesis))
        ((AND (Equal (car Hy)  *star*)(NOT (Equal (car MGS) (car NegE))))
         ; wähle alle Alternativen, die positive Beispiele umfassen 
         (specialize-help (cdr NegE) (cdr MGS) (cdr Hy) (append StartOfH (List (car Hy))) 
                                   (cons (append (append StartOfH (list (car MGS))) (cdr Hy))
                                         ListOfHypothesis)))
        ((Equal (car Hy) *star*) 
         (specialize-help (cdr NegE) (cdr MGS) (cdr Hy) (append StartOfH (List (car MGS)))
                          ListOfHypothesis))))
                

(defun is-included (Hy Hylist)
  (cond ((null hylist) nil)
        ((includes (car Hylist) Hy) T)
        (T (is-included Hy (cdr Hylist)))))
              
(defun remove-less-general (HyList)
  (cond ((null HyList) nil)
        ((is-included (car HyList) (cdr Hylist))
         (remove-less-general (cdr Hylist)))
        (T (cons (car Hylist)
                 (remove-less-general (cdr Hylist))))))

(defun remove-too-special (Hy ListOfHypotheses)
  (cond ((NULL ListOfHypotheses) NIL)
        ((includes (car ListOfHypotheses) Hy)
         (cons (car ListOfHypotheses)
               (remove-too-special Hy (cdr ListOfHypotheses))))
        (T (remove-too-special Hy (cdr ListOfHypotheses)))))


(defun remove-too-general (Hy ListOfHypotheses)
  (cond ((NULL ListOfHypotheses) NIL)
        ((includes Hy (car ListOfHypotheses)) 
         (cons (car ListOfHypotheses)
               (remove-too-general Hy (cdr ListOfHypotheses))))
        (T (remove-too-general Hy (cdr ListOfHypotheses)))))


(defun get-S (VS)
  (car VS))

(defun get-G (VS)
  (cadr VS))

(defun prune (l)
  (cond ((null l) nil)
        ((null (car l))
         (prune (cdr l)))
        (t (cons (car l) (prune (cdr l))))))

(defun is-concept-p (x)
  (cond ((null x) nil)
        ((listp x)
         (listp (car x)))))

(defun flatten (x)
  (cond ((null x) nil)
        (T (append (car x) 
                   (flatten (cdr x))))))

         
(defun version-space-step (example label VS)
  (cond ((OR (null (get-G VS))
             (null (get-S VS))) '(NIL NIL))
        ((equal label "ja")
         (list (mapcar (lambda (x) (generalize example x)) (get-S VS))
               (prune (mapcar (lambda (x) (if (includes x example) x nil)) (get-G VS)))))
        ((equal label "nein")
         (list (prune (mapcar (lambda (x) (if (includes x example) nil x)) (get-S VS)))
               (remove-less-general (flatten (mapcar (lambda (x) (specialize example (get-S VS) x)) (get-G VS))))))))



(defun initVS (number)
  (initVS-help number NIL NIL))

(defun initVS-help (number S G)
  (cond ((= number 0) (list (list S) (list G)))
        (T (initVS-help (- number 1) (cons '*Floor* S) 
                        (cons '*Star* G)))))

(defun get-header (dataSet)
  (eval (car dataSet)))

(defun get-examplelist (dataSet)
  (cond ((null dataSet) NIL)
        (T (cons (eval (car dataSet))
                 (get-examplelist (cdr dataSet))))))

(defun version-space (filename)
  (let ((exampleFile (load-exampleset filename)))
    (do ((examples (cdr (get-examplelist exampleFile)) 
                   (cdr examples))
         (VS (initVS (length (car (get-header exampleFile)))) 
             (version-space-step (caar examples) (cadar examples) VS))
         (n 0 (+ n 1)))
        ((NULL examples) VS)

(format T "S = ~S ~% G = ~S ~% New Example: ~2D - ~S ~% " (get-S VS) (get-G VS) n (car examples) )
)))



               




