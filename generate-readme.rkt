#lang racket

(require racket/match)
(require racket/runtime-path)

(define-runtime-path ch1 "ch1/")
(define-runtime-path ch2 "ch2/")
(define-runtime-path ch3 "ch3/")
(define-runtime-path ch4 "ch4/")
(define-runtime-path ch5 "ch5/")

(define chapters '(1 2 3 4 5))

(define (chapter-exercise-count chapter)
  (match chapter [1 46] [2 97] [3 82] [4 79] [5 52] [_ 0]))

(define (chapter-exercise-folder chapter)
  (match chapter [1 ch1] [2 ch2] [3 ch3] [4 ch4] [5 ch5] [_ (error "chapter not exists")]))

(define all-exercise-files
  (apply append
         (map
          (lambda (ch)
            (map file-name-from-path (directory-list (chapter-exercise-folder ch))))
          chapters)))

(define (make-exercise-info chapter number todo?) (list 'exercise chapter number todo?))
(define exercise-info-chapter cadr)
(define exercise-info-number caddr)
(define exercise-info-todo? cadddr)

(define (extract-exercise-info filename)
  (match (regexp-match #px"ex\\.(\\d+)\\.(\\d+)(\\.todo)?.*" filename)
    [(list _ chapter number todo?)
     (make-exercise-info (string->number chapter)
                         (string->number number)
                         (not (eq? todo? #f)))]
    [_ '()]))

(define all-exists-exercises (filter (compose not null?)
                              (map extract-exercise-info all-exercise-files)))

(define all-exists-exercises-table
  (make-hash (map
              (lambda (ex-info)
                (cons (cons (exercise-info-chapter ex-info)
                            (exercise-info-number ex-info))
                      ex-info))
              all-exists-exercises)))

(define (exercise-status-emoji chapter number)
  (define key (cons chapter number))
  (define has-file? (hash-has-key? all-exists-exercises-table key))
  (define has-todo? (and has-file?
                         (exercise-info-todo?
                          (hash-ref all-exists-exercises-table key))))
  (match* (has-file? has-todo?)
    [(_ #t) ":red_square:"]
    [(#t _) ":white_check_mark:"]
    [(#f _) ":white_square_button:"]))

(define (chunk-list lst n)
  (cond [(empty? lst) empty]
        [(< (length lst) n) (list lst)]
        [else (cons (take lst n) (chunk-list (drop lst n) n))]))

(define (make-markdown-table chapter n-col)
  (define exercises (map (lambda (number) (cons chapter number))
                         (inclusive-range 1 (chapter-exercise-count chapter))))

  (define chunked-exercises (chunk-list exercises n-col))

  (define (make-td exercise)
    (let ([chapter (car exercise)]
          [number (cdr exercise)])
      (format "<td>~a ~a</td>"
              (exercise-status-emoji chapter number)
              (format "~a.~a" chapter number))))

  (define (make-tr exercises)
    (define tds (map (lambda (ex) (make-td ex)) exercises))
    (define td-string (string-join tds "\n      " #:before-first "      "))
    (format "    <tr>\n~a\n    </tr>" td-string))

  (define (make-trs chunked-exercises)
    (define trs (map (lambda (exercises) (make-tr exercises)) chunked-exercises))
    (string-join trs "\n"))

  (string-join (list "<table align=\"center\">"
                     "  <tbody>"
                     (make-trs chunked-exercises)
                     "  </tbody>"
                     "</table>")
               "\n"))

(define (make-h3 text) (format "<h3 align=\"center\">~a</h2>\n" text))

(module+ main
  (with-output-to-file "README.md"
    (lambda ()
      (display-lines
       (list "# SICP Solutions"
             ""
             "## Setup"
             ""
             "```bash"
             "raco pkg install ./support/"
             "```"
             ""
             "## Run Tests"
             ""
             "```bash"
             "raco test ."
             "```"
             ""
             "## Progress"
             ""))
      (for ([ch chapters])
        (displayln (make-h3 (format "Chapter ~a" ch)))
        (displayln (make-markdown-table ch 8))
        (newline)))
    #:exists 'replace))
