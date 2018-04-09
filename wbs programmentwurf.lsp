; Load-Exampleset 
; ---------------
; Liest eine Beispieldatei und trennt die Kopfzeile ab
; Wert von Load-Exampleset ist eine Liste (was sonst?)
; 1. Element : Eine Liste mit einer Liste aller Attributnamen und einem Bezeichner "Teacher"
; 2. Element : Eine Liste von Listen, die jeweils einem bewerteten Beispiel entsprechen
; Ein bewertetes Beispiel ist eine Liste der Attributwerte sowie eine Bewertung "ja" "nein"

(setq *path-to-vs* "Wohnungskartei_TestA2\Wohnungskartei_A2_formatted.lsp")
(setq *path-to-testdata* "C:/TEMP/ball_lernen.lsp")

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
					  
; ---------------------------------------------------------------------
; --------------------------- Version Space ---------------------------
; ---------------------------------------------------------------------

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



; Checks if an example matches a hypothesis
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
                


; Checks if an example is included in a list of hypotheses 
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

(defun version-space-list (training-data)
    (do ((examples training-data                                          ; init var binding
            (cdr examples))                                             ; step var binding
        (vs (initvs (length (caar training-data)))                           ; init var binding
            (version-space-step (caar examples) (cadar examples) vs))   ; step var binding
        (n 0 (+ n 1)))                                                  ; init + step var binding
        ((null examples) vs)                                            ; (termaination condition) return value
        ; step action
        (format T "New Example: ~2D ~% " n )
    )
)

; ----------------------------------------------------------------------
; ---- Call (classify (learn-conept "A2 formatted") "TestA2 formatted") ----
; ----------------------------------------------------------------------

; gets the test objects out of a lisp file
; the calles the classify-objects mapping function
(defun classify (concept testdata)
	(let
		((test-objects (load-exampleset testdata))) ;hier muss logik hin zum lesen von lisp liste
		(classify-objects concept test-objects)
	)
)

; calls AQ algorithm with filtered list of positive and negative examples
(defun learn-concept (training-data-path)
	(let 
		((training-data (load-exampleset training-data-path)))
		;((training-data (load-exampleset training-data-path)))
		(AQ (filterPos (cdr (get-examplelist training-data))) (filterNeg (cdr (get-examplelist training-data))))
		;(filterNeg (cdr (get-examplelist training-data)))
		;(filterPos (cdr (get-examplelist training-data)))
		;(version-space-list training-data)
	)
)

; Gets the concept and test objects
; maps one object and if it is accepted or not to 
; each index of the test objects list itself
(defun classify-objects (concept test-objects)
	(mapcar
		(lambda (test-object) 
			(list 
				(eval test-object) (is-accepted concept test-object) 
			)
		) test-objects
	)
)

; checks if the test object matches the concept
; if true, the test object has an accepted flag
; if false, the test object has an rejected flag
(defun is-accepted (concept test-object)
	(cond
		((null concept) "rejected")
		((is-included (car (eval test-object)) (car concept)) "accepted")
		(T (is-accepted (cdr concept) test-object))
	)
)


; ----------------------------------------------------------------------
; ----------------------------- AQ Algorithm ---------------------------
; ----------------------------------------------------------------------

; Filters all positive data out of trainings dataset
(defun filterPos (testdata)
	(cond 
		((null testdata) NIL)
		(
			(eval (equal "ja" (cadar testdata))) 
			(cons (car testdata) (filterPos (cdr testdata)))
		)
		(T (filterPos (cdr testdata)))
	)
)


; Filters all negative data out of trainings dataset
(defun filterNeg (testdata)
	(cond 
		((null testdata) NIL)
		(
			(eval (equal "nein" (cadar testdata))) 
			(cons (car testdata) (filterNeg (cdr testdata)))
		)
		(T (filterNeg (cdr testdata)))
	)
)


; The recursive AQ algorithm
(defun AQ (bPos bNeg)
	(cond 
		((null bPos) NIL)
		(T
			(let 
				((star (make-star (car bPos) bNeg))) 
				(cons 
					star
					(AQ 
						(remove-bPos-covered 
							(cdr bPos)
							star
						) 
						bNeg
					)
				)
			)			
		)
	)
	;(format T "--" bPos)
)


; Makes a star out of a positive object and negative objects
(defun make-star (pos-object neg-objects)
	(get-G 
		(version-space-list 
			(cons pos-object neg-objects)
		)
	)
)


; Removes positive examples that are already covered
(defun remove-bPos-covered (bPos star)
	(cond
		((null bPos) NIL)
		;((includes star (caar bPos)) (remove-bPos-covered (cdr bPos) star))
		((is-included (caar bPos) star) (remove-bPos-covered (cdr bPos) star))
		(T 
			(cons
					(car bPos)
					(remove-bPos-covered (cdr bPos) star)
			)
		)
	)
	
	;(format T "-" bPos)
)