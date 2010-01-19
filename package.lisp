(defpackage :cl-graph
  (:use :cl-utils :extended-reals :pqueue :cl)
  (:export

   :make-graph :make-undirected-graph :copy-graph 
   :node-list :edge-list

   :add-node :add-edge :remove-edge

   :get-edge-data :get-node-data :lookup-edge-data :lookup-node-data 
   :update-edge-data :update-node-data :memoize-edge-data :memoize-node-data

   :adjacent-edge-list :neighbors :other-node :edge-between :incident-nodes 
   :outgoing-edges :incoming-edges :head :tail


   :compute-navfn :extract-path :shortest-path :diameter

   ;; Trees
   :is-tree :parent :root :depth :add-child :children :is-root :is-leaf
   :print-local-tree :down :up :inspect-tree

))
