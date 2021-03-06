
(in-package :rnd)

; MACROS


(defmacro with-prob (p &body body)
  "
  executes body with probability p.
  "
  (with-gensyms (pname)
    `(let ((,pname ,p))
       (when (< (random 1d0) ,p)
         (list ,@body)))))


(defmacro prob (p a &optional b)
  `(if (< (rnd) ,p)
     ,a ,b))


(defmacro either (a b)
  `(prob 0.5d0 ,a ,b))


(defmacro rcond (&rest clauses)
  (with-gensyms (val)
    (let* ((tot 0d0)
           (clauses* (loop for (prob . body) in clauses
                           do (incf tot (math:dfloat prob))
                           collect `((< ,val ,tot) ,@body))))
    `(let ((,val (rnd ,tot)))
      (cond ,@clauses*)))))


; GENERIC


(defun lget (l)
  (nth (random (length l)) l))


(defun aget (l)
  (aref l (random (length l))))


(defun nrnd-u-from (n a)
  (let* ((a* (if (eql (type-of a) 'cons) (to-array a) a))
         (resind nil)
         (anum (length a*)))
    (when (> n anum)
      (error "not enough distinct elements in a."))
    (loop until (>= (hset:num (hset:make :init resind)) n)
          do (setf resind (nrndi n 0 anum)))
    (loop for i in resind collect (aref a* i))))


(defun nrnd-from (n a)
  (loop for i in (nrndi n 0 (length a)) collect (aref a i)))


(defun -init-gen-array (v)
  (let ((res (make-generic-array)))
    (array-push v res)
    res))

(defun array-split (arr p)
  (let ((res (make-generic-array)))

    (array-push (-init-gen-array (aref arr 0)) res)

    (loop for i from 1 below (length arr) do
      (prob p
        (array-push (-init-gen-array (aref arr i)) res)
        (array-push (aref arr i) (aref res (1- (length res))))))
    res))


; NUMBERS AND RANGES


(defun rndi (a &optional b)
  (declare (integer a))
  ;(declare (type (or integer nil) b))
  (if (not b)
    (random a)
    (+ a (random (- (math:int b) a)))))


(defun nrndi (n a &optional b)
  (loop repeat n collect (rndi a b)))


(defun rndi* (ab)
  (declare (list ab))
  (destructuring-bind (a b)
    ab
    (declare (integer a b))
    (+ a (random (- b a)))))


(defun nrndi* (n ab)
  (loop repeat n collect (rndi ab)))


(defun rnd (&optional (x 1.0d0))
  (declare (double-float x))
  (random x))


(defun nrnd (n &optional (x 1.0d0))
  (declare (integer n))
  (declare (double-float x))
  (loop repeat n collect (rnd x)))


; TODO: nnorm, with-norm
(defun norm (&key (mu 0.0d0) (sigma 1d0))
  "
  box-muller transform
  "
  (declare (double-float mu sigma))
  (let ((s (* sigma (sqrt (* -2d0 (log (rnd))))))
        (u (* 2d0 pi (rnd))))
    (values
      (+ mu (* s (cos u)))
      (+ mu (* s (sin u))))))


(defun rndbtwn (a b)
  (declare (double-float a b))
  (+ a (random (- b a))))


(defun nrndbtwn (n a b)
  (declare (integer n))
  (declare (double-float a b))
  (loop for i from 0 below n collect (rndbtwn a b)))


(defun rnd* (&optional (x 1.0d0))
  (declare (double-float x))
  (- x (* 2.0d0 (random x))))


(defun nrnd* (n &optional (x 1.0d0))
  (declare (integer n))
  (declare (double-float x))
  (loop repeat n collect (rnd* x)))


(defmacro with-rndspace ((n a b rn) &body body)
  (with-gensyms (a* b* d)
    `(destructuring-bind (,a* ,b*)
      (sort (list (math:dfloat ,a) (math:dfloat ,b)) #'<)
      (let ((,d (- ,b* ,a*)))
        (loop repeat ,n do
          (let ((,rn (+ ,a* (random ,d))))
            (progn ,@body)))))))


(defun rndspace (n a b &key order)
  (declare (integer n))
  (declare (double-float a b))
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (- b a)))
        (let ((res (math:nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


(defun rndspacei (n a b &key order)
  (declare (integer n a b))
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (- b a)))
        (let ((res (math:nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


(defun bernoulli (n p)
  (declare (integer n))
  (declare (double-float p))
  (loop repeat n collect
    (if (< (rnd:rnd) p)
      1d0
      0d0)))


; SHAPES


(defun -add-if (a xy)
  (if xy (vec:add a xy) a))


(defun on-circ (rad &key xy)
  (-add-if (vec:scale (vec:cos-sin (random PII)) rad) xy))


(defun non-circ (n rad &key xy)
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (on-circ rad :xy xy)))


(defmacro with-in-circ ((n rad v &key xy) &body body)
  (with-gensyms (rad* xy*)
    `(let ((,rad* ,rad)
           (,xy* ,xy))
      (loop repeat ,n do
        (let ((,v (in-circ ,rad* :xy ,xy*)))
          (progn ,@body))))))


(defun in-circ (rad &key xy)
  (declare (double-float rad))
  (-add-if
    (let ((a (random 1.0d0))
          (b (random 1.0d0)))
      (declare (double-float a b))
      (if (< a b)
        (vec:scale (vec:cos-sin (* PII (/ a b))) (* b rad))
        (vec:scale (vec:cos-sin (* PII (/ b a))) (* a rad))))
    xy))


(defun nin-circ (n rad &key xy)
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (in-circ rad :xy xy)))


(defun in-box (sx sy &key xy)
  (declare (double-float sx sy))
  (-add-if (vec:vec (rnd* sx) (rnd* sy)) xy))


(defun nin-box (n sx sy &key xy)
  (declare (integer n))
  (declare (double-float sx sy))
  (loop repeat n collect (in-box sx sy :xy xy)))


(defmacro with-on-line ((n a b rn) &body body)
  (with-gensyms (sub a*)
    `(let* ((,a* ,a)
            (,sub (vec:sub ,b ,a*)))
      (loop repeat ,n do
        (let ((,rn (vec:add ,a* (vec:scale ,sub (random 1d0)))))
          (progn ,@body))))))


(defun on-line (a b)
  (declare (vec:vec a b))
  (vec:add a (vec:scale (vec:sub b a) (random 1.0d0))))


(defun on-line* (ab)
  (declare (list ab))
  (destructuring-bind (a b)
    ab
    (on-line a b)))


(defun non-line (n a b)
  (declare (integer n))
  (declare (vec:vec a b))
  (loop repeat n collect (on-line a b)))


(defun non-line* (n ab)
  (declare (integer n))
  (declare (list ab))
  (destructuring-bind (a b)
    ab
    (non-line n a b)))


; WALKERS

(defun get-lin-stp (&optional (init 0.0d0))
  "
  random linear walker limited to (0 1)
  "
  (let ((x (math:dfloat init)))
    (lambda (stp)
      (setf x (math:inc x (rnd* stp))))))


(defun get-lin-stp* (&optional (init 0.0d0))
  "
  random linear walker
  "
  (let ((x (math:dfloat init)))
    (lambda (stp)
      (incf x (rnd* stp)))))


(defun get-acc-lin-stp (&optional (init-x 0.0d0) (init-a 0.0d0))
  "
  random accelerated linear walker limited to (0 1)
  "
  (let ((a (math:dfloat init-a))
        (x (math:dfloat init-x)))
    (lambda (s)
      (setf x (math:inc x (incf a (rnd* s)))))))


(defun get-acc-lin-stp* (&optional (init-x 0.0d0) (init-a 0.0d0))
  "
  random accelerated linear walker
  "
  (let ((a (math:dfloat init-a))
        (x (math:dfloat init-x)))
    (lambda (s)
      (incf x (incf a (rnd* s))))))


(defun get-circ-stp* (&optional (init (vec:zero)))
  (let ((xy (vec:copy init)))
    (lambda (stp)
      (setf xy (vec:add xy (in-circ stp))))))


(defun get-acc-circ-stp* (&optional (init (vec:zero))
                                    (init-a (vec:zero)))
  (let ((a (vec:copy init-a))
        (xy (vec:copy init)))
    (lambda (stp)
      (setf xy (vec:add xy (setf a (vec:add a (in-circ stp))))))))

