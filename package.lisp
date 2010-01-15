(defpackage :cl-graph
  (:use :cl-utils :extended-reals :pqueue :cl)
  (:export

   :make-graph :make-undirected-graph :copy-graph 
   :add-node :add-edge :remove-edge
   :node-list :edge-list
   :get-edge-data :get-node-data
   :adjacent-edge-list :neighbors :other-node :edge-between :incident-nodes
   :compute-navfn :extract-path :shortest-path

   ;; Trees
   :is-tree :parent

))
