#lang racket
(provide (all-defined-out))

;1. check_bst
(define (check_bst tree)
  (if (null? tree) #t
      (letrec (
               [nodeVal (car tree)]
               [checkL (lambda (lc)
                 (if (null? lc) #t
                     (> nodeVal (car lc))))]
               [checkR (lambda (rc)
                 (if (null? rc) #t
                     (< nodeVal (car rc))))])

               (and (checkL (cadr tree)) (checkR (caddr tree)) 
                    (check_bst(cadr tree)) ;proceed to check left subtree
                    (check_bst(caddr tree)) ;proceed to check right subtree 
                )
       )
   )
)

;2. apply
(define (apply f tree)
  (if (null? tree) null
      (list(f (car tree)) (apply f (cadr tree)) (apply f (caddr tree)))))

;3. equals
(define (exist nodeVal tree)
  (if (null? tree)#f
      (or (= nodeVal (car tree)) (exist nodeVal (cadr tree)) (exist nodeVal (caddr tree)))))

(define (comp tree1 tree2)
  (if (null? tree1)#t
      (and (exist (car tree1) tree2) (comp (cadr tree1) tree2) (comp (caddr tree1) tree2))))

(define (equals t1 t2)
  (and (comp t1 t2)(comp t2 t1)))