#lang racket/gui
(require racket/format)
(require string-constants)
;Data section
(define data-table (make-hash))
(hash-set! data-table "record-message" "")
(hash-set! data-table "status-message" "")
(hash-set! data-table "result-message" "")
(hash-set! data-table "default-setup" (make-hash (list  (cons "hard" (make-hash (list (cons "beads-max" 20) (cons "average-beads" 5) (cons "current-beads" 0) (cons "current-beads-cards" 0) (cons "others" 0) (cons "kaika" 0) (cons "tantou" 0) (cons "uchigatana" 0) (cons "tachi" 0) (cons "yari" 0) (cons "naginata" 0) (cons "loss" 0) (cons "cards-max" 48) (cons "beads-cards-max" 26) (cons "others-max" 4) (cons "kaika-max" 2) (cons "tantou-max" 2) (cons "uchigatana-max" 3) (cons "tachi-max" 3) (cons "yari-max" 3) (cons "naginata-max" 3) (cons "loss-max" 2) (cons "total-draws" 16) (cons "current-draw" 0) (cons "current-records" '())))) (cons "expert" (make-hash (list (cons "beads-max" 25) (cons "average-beads" 5) (cons "current-beads" 0) (cons "current-beads-cards" 0) (cons "others" 0) (cons "kaika" 0) (cons "tantou" 0) (cons "uchigatana" 0) (cons "tachi" 0) (cons "yari" 0) (cons "naginata" 0) (cons "loss" 0) (cons "cards-max" 70) (cons "beads-cards-max" 37) (cons "others-max" 6) (cons "kaika-max" 4) (cons "tantou-max" 4) (cons "uchigatana-max" 4) (cons "tachi-max" 4) (cons "yari-max" 4) (cons "naginata-max" 4) (cons "loss-max" 3) (cons "total-draws" 19) (cons "current-draw" 0) (cons "current-records" '())))))))
(hash-set! data-table "current-setup" (hash-copy (hash-ref (hash-ref data-table "default-setup") "hard")))
;Logic section
(define debug-display (lambda () (display (hash-ref data-table "current-setup"))))

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define kaika-bonus (lambda () (let ((kaika (hash-ref (hash-ref data-table "current-setup") "kaika"))) (cond
                      ((or (= kaika 0) (= kaika 2) (= kaika 4)) 1)
                      (else 2)))))

(define beads-odds (lambda () (/ (- (hash-ref (hash-ref data-table "current-setup") "beads-cards-max") (hash-ref (hash-ref data-table "current-setup") "current-beads-cards")) (- (hash-ref (hash-ref data-table "current-setup") "cards-max") (hash-ref (hash-ref data-table "current-setup") "current-draw")))))

(define average-beads (lambda () (hash-ref (hash-ref data-table "current-setup") "average-beads")))

(define loss-odds (lambda () (/ (- (hash-ref (hash-ref data-table "current-setup") "loss-max") (hash-ref (hash-ref data-table "current-setup") "loss")) (- (hash-ref (hash-ref data-table "current-setup") "cards-max") (hash-ref (hash-ref data-table "current-setup") "current-draw")))))

(define risk (lambda (s) (let ((max (hash-ref (hash-ref data-table "current-setup") (string-append s "-max")))
                              (current (hash-ref (hash-ref data-table "current-setup") s)))
                          (cond
                            ((or (= current 0) (= current max)) 0)
                             (else
                              (* (- 0 (hash-ref (hash-ref data-table "current-setup") "current-beads"))  (/ (- max current) (- (hash-ref (hash-ref data-table "current-setup") "cards-max") (hash-ref (hash-ref data-table "current-setup") "current-draw")))))))))

(define level-extra (lambda () (let ((total-draws (hash-ref (hash-ref data-table "current-setup") "total-draws"))
                                     (current-draw (hash-ref (hash-ref data-table "current-setup") "current-draw")))
                      (cond
                        ((and (= total-draws 16) (= current-draw 15)) 140)
                        ((and (= total-draws 19) (= current-draw 18)) 300)
                        (else 0)))))

(define calculate (lambda () 
  (exact->inexact (+ (* (kaika-bonus) (beads-odds) (average-beads)) (* (loss-odds) -4.5) (risk "tantou") (risk "uchigatana") (risk "tachi") (risk "yari") (risk "naginata")(level-extra)))))

(define update-gui-status (lambda (s) (hash-set! data-table "status-message" s)
                            (send status-message set-label s)))

(define tostr (lambda (a)
                        (cond
                          ((equal? a "tantou") "Tantou \n")
                          ((equal? a "uchigatana") "Uchigatana \n")
                          ((equal? a "tachi") "Tachi \n")
                          ((equal? a "yari") "Yari \n")
                          ((equal? a "naginata") "Naginata \n")
                          ((equal? a "kaika") "Kaika \n")
                          ((equal? a "others") "Others \n")
                          ((list? a) (cond
                          ((equal? (car a) "gain") (string-append "Gain: " (~a (car (cdr a)))))
                          ((equal? (car a) "lose") (string-append "Lose: " (~a (car (cdr a)))))))
                          (else ""))))

(define form-record-message (lambda () (letrec ((form-message (lambda (l) 
                                                     (cond
                                                       ((null? l) "")
                                                       ((null? (car l)) "")
                                                       (else (string-append (tostr (car l)) (form-message (cdr l))))))))
                              (string-append "Total: " (string-append  (~a (hash-ref (hash-ref data-table "current-setup") "current-draw"))(string-append "   " (form-message (hash-ref (hash-ref data-table "current-setup") "current-records"))))))))

(define update-gui (lambda ()
                     (hash-set! data-table "record-message" (form-record-message))
                     (send records-message set-label (form-record-message))
                     (hash-set! data-table "status-message" "")
                     (send status-message set-label (string-append "Current beads:" (~a (hash-ref (hash-ref data-table "current-setup") "current-beads"))))
                     (cond
                       ((>= (calculate) 0) (send result-message set-label (string-append "Go ahead. Expectation:" (~a (calculate)) )))
                       (else (send result-message set-label "Stop here.")))))

(define set-setup (lambda (d) (hash-set! data-table "current-setup" (hash-copy (hash-ref (hash-ref data-table "default-setup") d)))
                    (send average-beads-slider set-value (hash-ref (hash-ref data-table "current-setup") "average-beads"))))

(define has-draws-left (lambda () (< (hash-ref (hash-ref data-table "current-setup") "current-draw") (hash-ref (hash-ref data-table "current-setup") "total-draws"))))

(define has-cards-left (lambda (c) (< (hash-ref (hash-ref data-table "current-setup") c) (hash-ref (hash-ref data-table "current-setup") (string-append c "-max")))))

(define new-card (lambda (c)
                   (lambda (k d) (cond 
                                   ((equal? k c) (hash-set! (hash-ref data-table "current-setup") k  (+ 1 d)))
                                   ((equal? k  "current-draw") (hash-set! (hash-ref data-table "current-setup") k (+ 1 d)))
                                   ((equal? k "current-records") (hash-set! (hash-ref data-table "current-setup") k (cons  c d)))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

(define new-average-beads (lambda (a) 
                   (lambda (k d) (cond
                                   ((equal? k "average-beads") (hash-set! (hash-ref data-table "current-setup") k a))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

(define new-beads (lambda (a)
                   (lambda (k d) (cond
                                   ((equal? k "current-beads") (hash-set! (hash-ref data-table "current-setup") k (+ (* (kaika-bonus) a)  d)))
                                   ((equal? k "current-beads-cards") (hash-set! (hash-ref data-table "current-setup") k (+ 1 d)))
                                   ((equal? k "current-draw") (hash-set! (hash-ref data-table "current-setup") k (+ 1 d)))
                                   ((equal? k "current-records") (hash-set! (hash-ref data-table "current-setup") k (cons  (list "gain" a) d)))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

(define has-beads-available (lambda (a) (<= a (hash-ref (hash-ref data-table "current-setup") "current-beads"))))

(define new-beads-loss (lambda (a)
                   (lambda (k d) (cond
                                   ((equal? k "current-beads") (hash-set! (hash-ref data-table "current-setup") k (- d a)))
                                   ((equal? k "current-draw") (hash-set! (hash-ref data-table "current-setup") k (+ 1 d)))
                                   ((equal? k "current-records") (hash-set! (hash-ref data-table "current-setup") k (cons  (list "lose" a) d)))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

(define set-difficulty (lambda (d)
                         (set-setup d)
                         (update-gui)))

(define set-card (lambda (c)
                    (cond
                      ((and (has-draws-left) (has-cards-left c))
                       (hash-for-each (hash-ref data-table "current-setup") (new-card c))
                       (update-gui))
                      (else (update-gui-status "Illegal move")))))

(define set-average-beads (lambda (a)
                            (hash-for-each (hash-ref data-table "current-setup") (new-average-beads a))
                            (update-gui)))

(define beads-gain (lambda (a)
                            (hash-for-each (hash-ref data-table "current-setup") (new-beads a))
                            (update-gui)))

(define beads-lose (lambda (a)
                     (cond
                       ((has-beads-available a) (hash-for-each (hash-ref data-table "current-setup") (new-beads-loss a)) (update-gui))
                       (else (update-gui-status "Illegal move")))))
                    
(define undo (lambda () (let ((records (hash-ref (hash-ref data-table "current-setup") "current-records")))
                          (cond
                            ((null? records) (update-gui-status "Illegal move"))
                            ((atom? (car records)) (undo-set-card (car records)))
                            (else (cond
                                    ((equal? (car (car records)) "gain") (undo-beads-gain (car (cdr (car records)))))
                                    ((equal? (car (car records)) "lose") (undo-beads-lose (car (cdr (car records)))))))))))

(define undo-set-card (lambda (c)
                       (hash-for-each (hash-ref data-table "current-setup") (undo-new-card c))
                       (update-gui)))

(define undo-new-card (lambda (c)
  (lambda (k d) (cond 
                                   ((equal? k c) (hash-set! (hash-ref data-table "current-setup") k  (- d 1)))
                                   ((equal? k  "current-draw") (hash-set! (hash-ref data-table "current-setup") k (-  d 1)))
                                   ((equal? k "current-records") (hash-set! (hash-ref data-table "current-setup") k (cdr d)))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

(define undo-beads-gain (lambda (a)
                            (hash-for-each (hash-ref data-table "current-setup") (undo-new-beads a))
                            (update-gui)))

(define undo-beads-lose (lambda (a) (hash-for-each (hash-ref data-table "current-setup") (undo-new-beads-loss a)) (update-gui)))

(define undo-new-beads (lambda (a)
                   (lambda (k d) (cond
                                   ((equal? k "current-beads") (hash-set! (hash-ref data-table "current-setup") k (- d(* (kaika-bonus) a))))
                                   ((equal? k "current-beads-cards") (hash-set! (hash-ref data-table "current-setup") k (- d 1)))
                                   ((equal? k "current-draw") (hash-set! (hash-ref data-table "current-setup") k (- d 1)))
                                   ((equal? k "current-records") (hash-set! (hash-ref data-table "current-setup") k (cdr d)))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

(define undo-new-beads-loss (lambda (a)
                   (lambda (k d) (cond
                                   ((equal? k "current-beads") (hash-set! (hash-ref data-table "current-setup") k (+ d a)))
                                   ((equal? k "current-draw") (hash-set! (hash-ref data-table "current-setup") k (- d 1)))
                                   ((equal? k "current-records") (hash-set! (hash-ref data-table "current-setup") k (cdr d)))
                                   (else (hash-set! (hash-ref data-table "current-setup") k d))))))

;UI section
(define frame (new frame% [label "Challenge Calculator"]))
(define panel1 (new horizontal-panel% [parent frame]))
(define hard-button (new button% (parent panel1)
                         (label "Hard")
                         (callback (lambda (button event)
                                     (set-difficulty "hard")))))
(define expert-button (new button% (parent panel1)
                         (label "Expert")
                         (callback (lambda (button event)
                                     (set-difficulty "expert")))))
(define average-beads-slider (new slider%
                    (label "Average Beads")
                    (parent panel1)
                    (min-value 0)
                    (max-value 25)
                    (init-value 5)
                    (callback (lambda (button event)
                                (set-average-beads (send button get-value))))))
(define beads-slider (new slider%
                    (label "Beads")
                    (parent panel1)
                    (min-value 0)
                    (max-value 25)
                    (init-value 5)))
(define beads-button (new button% (parent panel1)
                         (label "Beads")
                         (callback (lambda (button event)
                                     (beads-gain (send beads-slider get-value))
                                    ))))
(define loss-slider (new slider%
                    (label "Beads")
                    (parent panel1)
                    (min-value 0)
                    (max-value 9)
                    (init-value 5)
                    (min-width 100)))
(define loss-button (new button% (parent panel1)
                         (label "Beads loss")
                         (callback (lambda (button event)
                                     (beads-lose (send loss-slider get-value))))))
(define tantou-button (new button% (parent panel1)
                         (label "Tantou")
                         (callback (lambda (button event)
                                     (set-card "tantou")
                                     ))))
(define uchigatana-button (new button% (parent panel1)
                         (label "Uchigatana")
                         (callback (lambda (button event)
                                     (set-card "uchigatana")))))
(define tachi-button (new button% (parent panel1)
                         (label "Tachi")
                         (callback (lambda (button event)
                                     (set-card "tachi")))))
(define yari-button (new button% (parent panel1)
                         (label "Yari")
                         (callback (lambda (button event)
                                     (set-card "yari")))))
(define naginata-button (new button% (parent panel1)
                         (label "Naginata")
                         (callback (lambda (button event)
                                     (set-card "naginata")))))
(define kaika-button (new button% (parent panel1)
                         (label "Kaika!!!")
                         (callback (lambda (button event)
                                     (set-card "kaika")))))
(define others-button (new button% (parent panel1)
                         (label "Others")
                         (callback (lambda (button event)
                                     (set-card "others")))))
(define undo-button (new button% (parent panel1)
                         (label "Undo")
                         (callback (lambda (button event) (undo)))
                         ))
(define panel2 (new horizontal-panel% [parent frame]))
(define group-box-panel (new group-box-panel%
                             (parent panel2)
                             (min-height 200)
                             (label "Records")))
(define records-message (new message%
                            (parent group-box-panel)
                            (min-width 200)
                            (label "No records.")))
(define panel-a (new panel%
                     (parent panel2)
                     (style (list 'border))))
(define status-message (new message%
     (parent panel-a)
     (min-width 200)
     (label "Status: ")))
(define panel-b (new panel%
                     (parent panel2)
                     (style (list 'border))))
(define result-message (new message%
     (parent panel-b)
     (min-width 200)
     (label "Result: ")))
(define menu-bar (new menu-bar%
                      (parent frame)))
(define about-menu (new menu%
                      (label "&About")
                      (parent menu-bar)
                      (demand-callback (lambda (m) (send about-frame show #t)))))
(define about-frame (new frame% [label "About"]))
(define about-message-1 (new message%
     (parent about-frame)
     (min-width 200)
     (label "©2015 arsdragonfly, Released under MIT License.")))
(define about-message-2 (new message%
     (parent about-frame)
     (min-width 200)
     (label "arsdragonfly@gmail.com http://github.com/arsdragonfly/")))
(send frame show #t)
