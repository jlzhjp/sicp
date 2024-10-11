#lang racket

(define chapters '(1 2 3 4 5))
(define (chapter-exercise-count chapter)
  (cond [(= chapter 1) 46]
        [(= chapter 2) 97]
        [(= chapter 3) 82]
        [(= chapter 4) 79]
        [(= chapter 5) 52]
        [else 0]))

(define (make-exercise chapter number) (list 'exercise chapter number))
(define exercise-chapter cadr)
(define exercise-number caddr)

(define (exercise->file-basename ex)
  (let ([ch (exercise-chapter ex)]
        [no (exercise-number ex)])
    (format "ch.~a/ex.~a.~a"
            ch
            ch
            (~a no #:min-width 2 #:align 'right #:left-pad-string "0"))))

(define (exercise->string ex)
  (let ([ch (exercise-chapter ex)]
        [no (exercise-number ex)])
    (format "~a.~a" ch no)))

(define accept-extensions '(".rkt" ".txt" ".tex" ".dot" ".md"))

(define (exercise-has-file? ex)
  (define basename (exercise->file-basename ex))
  (define accept-files
    (map (lambda (ext) (string-join (list basename ext) ""))
         accept-extensions))
  (ormap (lambda (file-path) (file-exists? file-path)) accept-files))

(define (exercise-status-emoji ex)
  (if (exercise-has-file? ex)
      ":white_check_mark:"
      ":white_square_button:"))


(define (chunk-list lst n)
  (cond [(empty? lst) empty]
        [(< (length lst) n) (list lst)]
        [else (cons (take lst n) (chunk-list (drop lst n) n))]))

(define (make-markdown-table exercises n-col)
  (define chunked-exercises (chunk-list exercises n-col))

  (define (make-td exercise)
    (format "<td>~a ~a</td>" (exercise-status-emoji exercise) (exercise->string exercise)))

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

(define (make-h1 text) (format "<h1 align=\"center\">~a</h1>\n" text))
(define (make-h2 text) (format "<h2 align=\"center\">~a</h2>\n" text))

(with-output-to-file "README.md"
  (lambda ()
    (displayln (make-h1 "SICP Solutions"))
    (for ([ch chapters])
      (define exercises (map (lambda (no)
                               (make-exercise ch no))
                             (inclusive-range 1 (chapter-exercise-count ch))))
      (displayln (make-h2 (format "Chapter ~a" ch)))
      (displayln (make-markdown-table exercises 8))))
  #:exists 'replace)


