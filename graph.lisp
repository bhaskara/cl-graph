;; Minimal graph implementation using adjacency list
;; Edges and nodes have integer id's and can have associated data

(in-package :cl-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (graph (:conc-name nil) (:constructor create-graph) (:copier nil))
  nodes edges next-node-id next-edge-id)

(defstruct (node-info (:conc-name nil) (:constructor make-node-info (node-data adjacent-edges)) (:type list))
  node-data adjacent-edges)

(defstruct (edge-info (:conc-name nil) (:constructor make-edge-info (edge-data from to)) (:type list))
  edge-data from to)

(defun make-graph ()
  (create-graph :nodes (make-hash-table :test #'eql) 
		:edges (make-hash-table :test #'eql)
		:next-node-id 0	:next-edge-id 0))

(defun copy-graph (g)
  "Return a new graph that is a copy of G.  Edge and node data and ids are shallow copied."
  (let ((g2 (make-graph)))
    (dolist (n (node-list g))
      (add-node g2 :data (copy-alist (get-node-data g n)) :id n))
    (dolist (e (edge-list g))
      (dsbind (from to) (incident-nodes g e)
	(add-edge g2 from to :id e :data (copy-alist (get-edge-data g e)))))
    g2))
  

(defun make-undirected-graph (adj-specs)
  "adj-specs is list of elements of form (V E1 ... En) where V are node names Ei is either a node name or a pair (NODE-NAME . EDGE-DATA)"
  (let ((g (make-graph)))
    (dolist (spec adj-specs)
      (add-node g :id (first spec)))
    (dolist (spec adj-specs g)
      (destructuring-bind (v . edge-specs) spec
	(dolist (spec edge-specs)
	  (condlet (((consp spec) (v2 (car spec)) (data (cdr spec)))
		    (t (v2 spec) (data nil)))
	    (when (edge-between g v v2)
	      (error "Trying to add duplicate edge between ~a and ~a" v v2))
	    (add-edge g v v2 :data data)))))))
      

(defun node-info (g i)
  (declare (type graph g))
  (check-not-null (gethash i (nodes g)) "Node ~a unknown in ~a" i g))

(defun edge-info (g i)
  (declare (type graph g))
  (check-not-null (gethash i (edges g)) "Edge ~a unknown in ~a" i g))

(defun get-edge-data (g i)
  (edge-data (edge-info g i)))

(defun get-node-data (g i)
  (node-data (node-info g i)))

(defun lookup-edge-data (g i k)
  (lookup-alist k (get-edge-data g i)))

(defun lookup-node-data (g i k)
  (lookup-alist k (get-node-data g i)))

(defun node-list (g)
  (hash-keys (nodes g)))

(defun edge-list (g)
  (hash-keys (edges g)))

(defun lookup-node (g key val)
  (check-not-null
   (dolist (id (node-list g))
     (let ((data (node-data (node-info g id))))
       (when (aand (assoc key data) (equal (cdr it) val))
	 (return id))))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjacency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun incident-nodes (g e)
  (with-readers (from to) (edge-info g e)
    (list from to)))

(defun other-node (g e i)
  (let ((edge-info (edge-info g e)))
    (with-readers (from to) edge-info
      (cond
	((eql from i) to)
	((eql to i) from)
	(t (error "Edge ~a was not incident to node ~a" edge-info i))))))

(defun adjacent-edge-list (g i)
  (adjacent-edges (node-info g i)))

(defun edge-between (g i j)
  "Return (the id of) an edge between I and J if one exists, or nil otherwise."
  (dolist (e (adjacent-edge-list g i))
    (when (eql j (other-node g e i))
      (return-from edge-between e))))



(defun neighbors (g i)
  "Node ids of neighbors i in g, with duplicates removed"
  (let ((l nil))
    (dolist (e (adjacent-edge-list g i) l)
      (pushnew (other-node g e i) l))))

(defun incoming-edges (g n)
  (let ((l nil))
    (dolist (e (adjacent-edge-list g n) (nreverse l))
      (when (eql (to (edge-info g e)) n)
	(push e l)))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-node (g &key data id)
  "Add a node.  You can provide your own ID, so long as it's not an integer (those are reserved for the defaults)."
  (orf id (1- (incf (next-node-id g))))
  (with-readers (nodes) g
    (assert (not (hash-table-has-key nodes id)))
    (setf (gethash id nodes) (make-node-info data nil))
    id))


(defun add-edge (g from to &key id data)
  "Add an edge between two nodes.  You can provide your own ID, so long as its not an integer (those are reserved for the defaults)."
  (orf id (1- (incf (next-edge-id g))))
  (with-readers (edges) g
    (assert (not (hash-table-has-key edges id)))
    (let ((from-node (node-info g from))
	  (to-node (node-info g to)))
      (setf (gethash id edges) (make-edge-info data from to))
      (push id (adjacent-edges from-node))
      (push id (adjacent-edges to-node))
      id)))

(defun remove-edge (g id)
  (dolist (n (incident-nodes g id))
    (let ((info (node-info g n)))
      (setf (adjacent-edges info) (delete id (adjacent-edges info)))))
  (assert (remhash id (edges g)) nil "Attempted to delete nonexistent edge ~a" id))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-graph (&rest args)
  (bind-pprint-args (str g) args
    (pprint-logical-block (str nil :prefix "[" :suffix "]")
      (format str "Graph~2I")
      (dolist (id (node-list g))
	(let ((info (node-info g id)))
	  (format str "~:@_Node ~a~4I~:[~;~:@_Data: ~:*~a~]~:@_Edges: ~a~2I" 
		  id (node-data info) (adjacent-edges info))))
      (dolist (id (edge-list g))
	(let ((info (edge-info g id)))
	  (format str "~:@_Edge ~a.  ~a -> ~a.~4I~:[~;~:@_Data: ~:*~a~]~2I"
		  id (from info) (to info) (edge-data info)))))))

(set-pprint-dispatch 'graph #'pprint-graph)


