#lang typed/racket

(require racket/runtime-path)

(define-runtime-path ch1 "ch1/")
(define-runtime-path ch2 "ch2/")
(define-runtime-path ch3 "ch3/")
(define-runtime-path ch4 "ch4/")
(define-runtime-path ch5 "ch5/")

(struct exercise ([chapter : Integer] [number : Integer]) #:transparent #:type-name Exercise)
(struct (T) completed ([value : T]) #:type-name Completed)
(struct (T) in-progress ([value : T]) #:type-name InProgress)
(struct (T) todo ([value : T]) #:type-name Todo)

(define-type (TaskStatus T) (U (Completed T) (InProgress T) (Todo T)))

(: task-status-value (All (T) (TaskStatus T) -> T))
(define (task-status-value status)
  (match status
    [(completed x) x]
    [(in-progress x) x]
    [(todo x) x]
    [_ (error "invalid status")]))

(define chapters : (Listof Integer) '(1 2 3 4 5))

(: chapter-exercise-count (-> Integer Integer))
(define (chapter-exercise-count chapter)
  (match chapter [1 46] [2 97] [3 82] [4 79] [5 52] [_ (error "chapter not exists")]))


(: chapter-exercise-folder (-> Integer Path))
(define (chapter-exercise-folder chapter)
  (match chapter [1 ch1] [2 ch2] [3 ch3] [4 ch4] [5 ch5] [_ (error "chapter not exists")]))

(: list-exercises (-> Integer (Listof Exercise)))
(define (list-exercises chapter)
  (for/list : (Listof Exercise)
    ([number (in-range 1 (add1 (chapter-exercise-count chapter)))])
    (exercise chapter number)))

(: list-files (-> Integer (Listof String)))
(define (list-files chapter)
  (for/fold : (Listof String) ([result '()])
    ([file (in-directory (chapter-exercise-folder chapter))])
    (match (file-name-from-path file)
      [(? path? name) (cons (path->string name)  result)]
      [_ result])))

(: group-files-by-exercise (-> (Listof String) (HashTable Exercise (Listof String))))
(define (group-files-by-exercise files)
  (for/fold : (HashTable Exercise (Listof String))
    ([result : (HashTable Exercise (Listof String)) (hash)])
    ([file files])
    (define regex-match (regexp-match #px"ex\\.(\\d)\\.(\\d+)" file))
    (match regex-match
      [(list _ (? string? chapter-capture) (? string? number-capture))
       (let* ([chapter (cast (string->number chapter-capture) Integer)]
              [number (cast (string->number number-capture) Integer)]
              [ex (exercise chapter number)])
         (hash-update result ex
                      (λ ([lst : (Listof String)]) (cons file lst))
                      (λ () '())))]
      [_ result])))

(: lift-exercise-with-status (-> Exercise (HashTable Exercise (Listof String)) (TaskStatus Exercise)))
(define (lift-exercise-with-status exercise files-table)
  (define files : (Listof String) (hash-ref files-table exercise (λ () '())))
  (define contains-todo : Boolean (ormap (λ ([f : String]) (regexp-match? #px"todo" f)) files))

  (match* (files contains-todo)
    [('() _) (todo exercise)]
    [(_ #t) (in-progress exercise)]
    [(_ #f) (completed exercise)]))

(: load-exercises-with-status (-> Integer (Listof (TaskStatus Exercise))))
(define (load-exercises-with-status chapter)

  (: files-table (HashTable Exercise (Listof String)))
  (define files-table (group-files-by-exercise (list-files chapter)))

  (: exercises (Listof Exercise))
  (define exercises (list-exercises chapter))
  (map (λ ([ex : Exercise]) (lift-exercise-with-status ex files-table)) exercises))


(: chunk-list (All (T) (-> (Listof T) Integer (Listof (Listof T)))))
(define (chunk-list lst n)
  (cond [(empty? lst) empty]
        [(< (length lst) n) (list lst)]
        [else (cons (take lst n) (chunk-list (drop lst n) n))]))

(: exercise-emoji (-> (TaskStatus Exercise) String))
(define (exercise-emoji status)
  (match status
    [(todo _) "🔳"]
    [(in-progress _) "🚧"]
    [(completed _) "✅"]
    [_ (error "invalid status")]))

(: title-level-1 (-> String Void))
(define (title-level-1 title)
  (printf "# ~a\n\n" title))

(: title-level-2 (-> String [#:center Boolean] Void))
(define (title-level-2 title #:center [center #f])
  (if center
      (printf "\n<h2 align=\"center\">~a</h2>\n\n" title)
      (printf "\n## ~a\n\n" title)))

(: text (-> (Listof String) Void))
(define (text lines)
  (for ([str lines])
    (displayln str)))

(: code-block (-> String (Listof String) Void))
(define (code-block lang lines)
  (displayln (string-append "```" lang))
  (text lines)
  (displayln "```"))

(: indent (-> Integer String String))
(define (indent n str)
  (string-append (make-string n #\space) str))

(: indent-lines (-> Integer (Listof String) (Listof String)))
(define (indent-lines n lines)
  (for/list : (Listof String) ([line lines])
    (indent n line)))

(: make-exercise-table-cell (-> (TaskStatus Exercise) String))
(define (make-exercise-table-cell exercise)
  (format
   "<td>~a ~a.~a</td>"
   (exercise-emoji exercise)
   (exercise-chapter (task-status-value exercise))
   (exercise-number (task-status-value exercise))))

(: make-exercise-table-row (-> (Listof (TaskStatus Exercise)) (Listof String)))
(define (make-exercise-table-row exercises)
  `("<tr>"
    ,@(for/list : (Listof String) ([exercise exercises])
        (indent 2 (make-exercise-table-cell exercise)))
    "</tr>"))

(: make-exercise-table (-> (Listof (Listof (TaskStatus Exercise))) (Listof String)))
(define (make-exercise-table exercises)
  `("<table align=\"center\">"
    ,(indent 2 "<tbody>")
    ,@(cast (flatten
             (for/list : (Listof (Listof String)) ([row exercises])
               (indent-lines 4 (make-exercise-table-row row))))
            (Listof String))
    ,(indent 2 "</tbody>")
    "</table>"))

(: exercise-table (-> Integer Void))
(define (exercise-table chapter)
  (define exercises (load-exercises-with-status chapter))
  (define chunked-exercises : (Listof (Listof (TaskStatus Exercise))) (chunk-list exercises 8))
  (define table : (Listof String) (make-exercise-table chunked-exercises))
  (text table))

(module+ main
  (with-output-to-file "README.md" #:exists 'replace
    (λ ()
      (title-level-1 "SICP Exercises")
      (title-level-2 "Setup")
      (code-block "bash" '("raco pkg install ./akari-sicp/lib/"))
      (for ([chapter chapters])
        (title-level-2 (format "Chapter ~a" chapter) #:center #t)
        (exercise-table chapter)))))
