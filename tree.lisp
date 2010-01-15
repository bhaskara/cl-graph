(in-package :cl-graph)

;; Trees are implemented as a particular kind of graph.  Edges go from parent to child.

(defun is-tree (g)
  "Return true iff G is a tree, i.e.

1) It has a unique root node that has no parent (incoming edge)
2) Each other node has exactly one incoming edge
3) There are no cycles

Takes O(ev^2) time"

  (let ((root nil))
    (dolist (n (node-list g))
      (let ((l (length (incoming-edges g n))))
	(if (= 0 l)
	    (if root
		(return-from is-tree (values nil `(multiple-roots ,root ,n)))
		(setq root n))
	    (unless (= l 1)
	      (return-from is-tree (values nil `(multiple-parents ,n)))))))
    
    (dolist (n (node-list g) t)
      (unless (eq n root)
	;; follow parents
	(let ((par (parent g n)))
	  (loop
	     (cond
	       ((eq par n) (return-from is-tree (values nil `(cycle ,n))))
	       ((eq par root) (return))
	       (t (setq par (parent g par))))))))))
		   
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjacency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
(defun parent (g n)
  "Return the unique parent node of N or signal an error."
  (let ((incoming (incoming-edges g n)))
    (cond
      ((rest incoming) (error "~a has multiple parent edges ~a" n incoming))
      ((null incoming) (error "~a has no parent" n))
      (t (from (edge-info g (first incoming)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-child (g n &key data edge-data)
  "Returns 1) id of the new node 2) id of the edge to it"
  (let ((i (add-node g :data data)))
    (values i (add-edge g n i :data edge-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-root (g n)
  (let ((l (incoming-edges g n)))
    (length-equals 1 l)))

(defun root (g)
  (check-not-null
   (find-if (partial is-root g) (node-list g))))

(defun path-from-root (g n)
  (labels ((helper (g n l)
	     (if (is-root g n)
		 (cons n l)
		 (helper g (parent n) (cons n l)))))
    (helper g n nil)))

(defun depth (g n)
  (labels ((helper (g n d)
	     (if (is-root g n)
		 d
		 (helper g (parent n) (1+ d)))))
    (helper g n 0)))