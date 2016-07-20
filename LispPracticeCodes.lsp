
"Q3"
(defun grade (M)
	(cond
		((or (> M 100) (< M 0)) "Invalid entry")
		((>= M 90) 'A)
		((and (>= M 80) (<= M 89)) 'B)
		((and (>= M 70) (<= M 79)) 'C)
		((and (>= M 60) (<= M 69)) 'D)
		((and (>= M 0)  (<= M 60)) 'F)
	)
)

"Q4"
(defun BpowE (B E)
	(BpowE1 B E 1)
)

(defun BpowE1 (B E A)
	(cond
	((= E 0) A)
	(T (BpowE1 B (- E 1) (* A B)))
	)
)

"Q5"
(defun union (L1 L2)
	(delete-duplicates (fastunion L1 L2))
)

(defun fastunion (L1 L2)
	(cond
		((null L1) L2)
		((null L2) L1)
   		((member (first L1) L2) (fastunion (rest L1) L2))
   		(T (cons (first L1) (fastunion (rest L1) L2)))
   	)
)

(defun difference (L1 L2)
	(delete-duplicates (fastdifference L1 L2))
)

(defun fastdifference (L1 L2)
	(cond
		((null L1) ())
   		((member (first L1) L2) (fastdifference (rest L1) L2))
   		((not (member (first L1) L2)) (cons (first L1) (fastdifference (rest L1) L2)))
   	)
)

"Q6"
(defun rotate(L e)
	(fastrotate L e)
)

(defun fastrotate(L A)
	(cond
	((null L) nill)
	((= A 0) (append L ()))
	(T (fastrotate (append (rest L) (first L)) (- A 1)))
	)
)
