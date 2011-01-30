
;; un vierme este lista de forma (segment segment segment ...)
;; un segment este de forma ((x1 y1) (x2 y2)) x,y coordonate
;; perechea (x1 y1) este primul perechea initial al segmentului 
;; perechea (x2 y2) este perechea finala a segmentului 
;; un segment incepe cu perechea finala a segmentului anterior 
;; daca un segment are lungimea de 1 este eliminat din vierme la deplasare

;; Rutine pentru segmente de vierme 
(defun citeste_x (coord)
  (first coord))

(defun citeste_y (coord)
  (second coord))

(defun mod_x (coord fn pas)
  (list (funcall fn (car coord) pas) (second coord)))

(defun mod_y (coord fn pas)
  (list (car coord) (funcall fn (second coord) pas)))


(defun tip (seg)
  (destructuring-bind ((x1 y1) (x2 y2)) seg
    (cond 
      ((and (eql x1 x2) (eql y1 y2)) 'simplu)
      ((eql x1 x2) 'vertical)
      ((eql y1 y2) 'orizontal)
      (t 'eroare))))
      
(defun sus (seg)
  (destructuring-bind (x1y1 x2y2) seg
    (case (tip seg)
      ('vertical 
       (if (< (citeste_y x2y2) (citeste_y x1y1))
           (list x1y1 (mod_y x2y2 '- 1)) ; vertical in sus
           seg))
      ('orizontal (values (list x2y2 (mod_y x2y2 '- 1))
                          seg))
      ('simplu (list x1y1 (mod_y x2y2 '- 1))))))
     

(defun jos (seg)
  (destructuring-bind (x1y1 x2y2) seg
    (case (tip seg)
      ('vertical
       (if (< (citeste_y x2y2) (citeste_y x1y1)) 
           seg
           (list x1y1 (mod_y x2y2 '+ 1)))) ; vertical in jos
      ('orizontal (values (list x2y2 (mod_y x2y2 '+ 1))
                        seg))
      ('simplu (list x1y1 (mod_y x2y2 '+ 1))))))


(defun dreapta (seg)
  (destructuring-bind (x1y1 x2y2) seg
    (case (tip seg)
      ('vertical (values (list x2y2 (mod_x x2y2 '+ 1))
                 seg))
      ('orizontal
       (if (< (citeste_x x2y2) (citeste_x x1y1))
           seg
           (list x1y1 (mod_x x2y2 '+ 1))))
      ('simplu (list x1y1 (mod_x x2y2 '+ 1))))))
  
(defun stanga (seg)
  (destructuring-bind (x1y1 x2y2) seg
    (case (tip seg)
      ('vertical (values (list x2y2 (mod_x x2y2 '- 1))
                       seg))
      ('orizontal
       (if (< (citeste_x x2y2) (citeste_x x1y1))
           (list x1y1 (mod_x x2y2 '- 1))
           seg))
      ('simplu (list x1y1 (mod_x x2y2 '- 1))))))


;; Rutine pentru vierme 

(defun deplasare (vierme fn)
  (multiple-value-bind (seg1 seg2)
      (funcall fn (car vierme))
    (if (not (equal seg1 (car vierme)))
        (taie_coada (if (null seg2)
                        (cons seg1 (cdr vierme))
                        (cons seg1 (cons seg2 (cdr vierme)))))
        vierme)))
  
  
;    (if (equal seg1 (car vierme))
 ;       vierme
   ;     (if (null seg2)
  ;          (taie_coada (list (append seg1 (cdr vierme))))
    ;        (taie_coada (

(defun taie_segment (seg)
  (destructuring-bind ((x1 y1) (x2 y2)) seg
    (case (tip seg)
      ('vertical 
       (let ((y1 (funcall (if (< y2 y1) '- '+) y1 1)))
         (list (list x1 y1) (car (cdr seg)))))
      ('orizontal
       (let ((x1 (funcall (if (< x2 x1) '- '+) x1 1)))
         (list (list x1 y1) (car (cdr seg)))))
      ('simplu  nil))))


(defun taie_coada (vierme)
  (cond 
    ((null (cdr vierme)) 
     (let ((seg (taie_segment (car vierme))))
       (if (eql (tip seg) 'simplu)
           nil
           (list seg))))
    (t (cons (car vierme) (taie_coada (cdr vierme))))))

(defparameter *vierme* '(((50 50) (50 60))))

