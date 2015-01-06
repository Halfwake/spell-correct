#lang racket/base

(require racket/contract
         racket/dict
         racket/sequence
         racket/list
         racket/set
         racket/match
         racket/promise
         racket/string
         racket/port)

(define/contract (count-items items)
  (-> (listof any/c) dict?)
  (for/fold ([model (hash)])
            ([item items])
   (dict-set model item
             (add1 (dict-ref model item 0)))))

(module+ test
  (require rackunit)
  (check-equal? (hash "abc" 2 "bc" 1)
                (count-items (list "abc" "abc" "bc")))
  (check-equal? (hash) (count-items empty)))

(define/contract (split sequence)
  (-> sequence? (listof (listof (listof any/c))))
  (for/list ([i (in-range 0 (add1 (sequence-length sequence)))])
    (list (take sequence i) (drop sequence i))))

(module+ test
  (check-equal? (set (split '(1 2 3)))
                (set '((() (1 2 3))
                       ((1) (2 3))
                       ((1 2) (3))
                       ((1 2 3) ()))))
  (check-equal? (set (split '()))
                (set '((() ())))))

(define/contract (deletes sequence)
  (-> list? set?)
  (apply set
         (for/list ([pair (split sequence)]
                    #:when (not (null? (second pair))))
           (match-define (list first-half (list _ second-half ...)) pair)
           (append first-half second-half))))

(module+ test
  (check-equal? (deletes '(1 2 3 4))
                (set '(2 3 4) '(1 3 4) '(1 2 4) '(1 2 3)))
  (check-equal? (deletes '()) (set)))

(define/contract (transposes sequence)
  (-> list? set?)
  (apply set
         (for/list ([pair (split sequence)]
                    #:when (> (length (second pair)) 1))
           (match-define (list first-half (list one two rest ...)) pair)
           (append first-half (list two one) rest))))

(module+ test
  (check-equal? (transposes '(1 2 3 4))
                (set '(2 1 3 4) '(1 3 2 4) '(1 2 4 3)))
  (check-equal? (transposes '()) (set))
  (check-equal? (transposes '(1)) (set)))

(define alphabet "abcdefghijklmnopqrstuvwxyz")

(define/contract (replaces sequence)
  (-> list? set?)
  (apply set
         (for*/list ([letter alphabet]
                     [pair (split sequence)]
                     #:when (not (null? (second pair))))
           (match-define (list first-half (list _ rest ...)) pair)
           (append first-half (list letter) rest))))

(define/contract (inserts sequence)
  (-> list? set?)
  (apply set
         (for*/list ([letter alphabet]
                     [pair (split sequence)])
           (match-define (list first-half second-half) pair)
           (append first-half (list letter) second-half))))

(define/contract (edit-once word)
  (-> string? set?)
  (define char-list (string->list word))
  (apply set
         (set-map (set-union (deletes char-list)
                             (transposes char-list)
                             (replaces char-list)
                             (inserts char-list))
                  list->string)))
               

(define (edit-repeat word n)
  (match n
    [0 (set word)]
    [n (apply set-union
                   (for/list ([edit (edit-once word)])
                     (edit-repeat edit (sub1 n))))]))

(define/contract (known words known-words)
  (-> set? dict? set?)
  (apply set (for/list ([word words]
                        #:when (dict-has-key? known-words word))
               word)))

(define/contract (candidates word known-words)
  (-> string? dict? set?)
  ;; Delay candidates to avoid generating large unused sets.
  (define word-candidate
    (delay (known (set word)
                  known-words)))
  (define edit-once-candidate
    (delay (known (edit-once word)
                  known-words)))
  (define edit-twice-candidate
    (delay (known (edit-repeat word 2)
                  known-words)))
  (cond [(not (set-empty? (force word-candidate)))
         (force word-candidate)]
        [(not (set-empty? (force edit-once-candidate)))
         (force edit-once-candidate)]
        [else (set word)]))

(module+ main
  (define known-words
    (call-with-input-file "big.txt"
      (λ (in)
        (count-items (string-split (port->string in))))))
  (let loop ()
    (displayln (format "~a~%" (candidates (string-trim (read-line)) known-words)))
    (loop)))
