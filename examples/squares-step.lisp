#!/usr/bin/env sbcl --script

(load "../src/load")
(asdf:load-system "snek")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun main (size fn)
  (let ((grains 100)
        (itt 10000)
        (noise 0.0000030d0)
        (rep 1)
        (rad 300d0)
        (snk (snek:make))
        (sand (sandpaint:make size
                :fg (color:white 0.005)
                :bg (color:gray 0.1d0))))

    (loop for x in (math:linspace rep 300d0 300d0) for i from 0 do
      (loop for y in (math:linspace rep 300d0 300d0) for j from 0 do
        (let ((g (snek:add-grp! snk :type 'path :closed t)))
          (snek:add-polygon! snk 115 rad
                             :xy (vec:vec x y)
                             :g g))))

    (let ((grp-states (make-hash-table :test #'equal))
          (c 1))
      (snek:itr-grps (snk g)
        (incf c)
        (setf (gethash g grp-states)
              (list
                (lin-path:make (snek:get-grp-verts snk :g g))
                (rnd:get-acc-lin-stp* (rnd:rnd))
                (rnd:get-acc-lin-stp* (rnd:rnd))
                noise)))

      (loop for i from 0 to itt do
        (print-every i 1000)

        ;(snek:with (snk)
        ;  (snek:itr-grps (snk g)
        ;    (snek:itr-verts (snk v :g g)
        ;      (snek:move-vert? v (rnd:in-circ 0.3d0) :rel t))))

        (snek:itr-grps (snk g :collect nil)
          (destructuring-bind (path lina linb ns)
            (gethash g grp-states)
            ;(lin-path:move path (get-grp-vert-vals snk :g g) :rel nil) ;closed must be set in constr.
            (sandpaint:stroke sand
              (lin-path:pos* path (list (funcall linb ns)
                                        (funcall lina ns)))
              grains)))))

    (sandpaint:chromatic-aberration sand (vec:vec 500d0 500d0) :s 500d0 :noise 2d0)
    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

