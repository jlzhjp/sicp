#lang racket/base

(require racket/match
         racket/function
         syntax/parse/define
         (for-syntax racket/base))

;; Exercise 5.18: make register able to be traced
(define (make-register name)
  (define contents '*unassigned*)
  (define tracing #f)

  (define (trace-on) (set! tracing #t))
  (define (trace-off) (set! tracing #f))

  (define (set value)
    (set! contents value)
    (when tracing (printf "[register] ~a set to ~a" name value)))

  (match-lambda
    ['get contents]
    ['set set]
    ['trace-on (trace-on)]
    ['trace-off (trace-off)]))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))
(define (register-trace-on register) (register 'trace-on))
(define (register-trace-off register) (register 'trace-off))

;; [Exercise 5.10]
(define (analyze-instruction inst labels machine)
  (define inst-name (car inst))
  (define dispatch (hash-ref (instruction-dispatch-table) inst-name))
  (dispatch inst labels machine))

(define (analyze-const exp)
  (match exp
    [(list 'const val) (lambda () val)]
    [_ (error 'analyze-const "expect const, got ~a" exp)]))

(define (analyze-reg exp machine)
  (match exp
    [(list 'reg register-name)
     (lambda ()
       (if ((machine 'register-allocated) register-name)
           (let ([register ((machine 'get-register) register-name)])
             (get-contents register))
           (error 'analyze-reg "register used before assigning: ~a" register-name)))]
    [_ (error 'analyze-reg "expect reg, got ~a" exp)]))

(define (analyze-label exp labels)
  (match exp
    [(list 'label label)
     (define insts
       (cond [(assq label labels) => cdr]
             [else (error 'analyze-label "label not found ~a" label)]))
     (lambda () insts)]
    [else (error 'analyze-label "expect label, got ~a" exp)]))

;; [Exercise 5.9] Modify the expression-processing procedures to enforce the condition that operations
;; can be used only with registers and contants
(define (analyze-operation inst machine)
  (with-machine-states machine
    (match inst
      [(cons (list 'op prim) arg-exps)
       (let ([analyzed-args (map (curryr analyze-operation-argument machine) arg-exps)]
             [prim-proc (cond [(assq prim ops) => cadr]
                              [else (error 'analyze-operation "unknown primitive ~a" prim)])])
         (lambda ()
           (let ([args (map (lambda (f) (f)) analyzed-args)])
             (apply prim-proc args))))]
      [_ (error 'analyze-operation "~a is not allowed here" inst)])))

(define (analyze-operation-argument exp machine)
  (match exp
    [(cons 'const _) (analyze-const exp)]
    [(cons 'reg _) (analyze-reg exp machine)]
    [_ (error 'analyze-operation-argument "expect const or reg, got ~a" exp)]))

(define-syntax (with-machine-states stx)
  (syntax-parse stx
    [(_ machine:expr body:expr ...)
     (define ops-sym (datum->syntax #'machine 'ops))
     (define stack-sym (datum->syntax #'machine 'stack))
     (define flag-sym (datum->syntax #'machine 'flag))
     (define pc-sym (datum->syntax #'machine 'pc))
     #`(let ([#,ops-sym (machine 'operations)]
             [#,stack-sym (machine 'stack)]
             [#,flag-sym ((machine 'get-register) 'flag)]
             [#,pc-sym ((machine 'get-register) 'pc)])
         body ...)]))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; (assign <register-name> (reg <register-name>))
;; (assign <register-name> (const <constant-value>))
;; (assign <register-name> (op <operation-name> <input_1> ... <input_n>))
;; (assign <register-name> (label <label-name>)
(define (analyze-assign inst labels machine)
  (with-machine-states machine
    (define ((assign target) val)
      (set-register-contents! machine target val)
      (advance-pc pc))
    (match inst
      [(list 'assign target (and reg-exp (cons 'reg _)))
       (compose (assign target) (analyze-reg reg-exp machine))]
      [(list 'assign target (and const-exp (cons 'const _)))
       (compose (assign target) (analyze-const const-exp))]
      [(list 'assign target (and label-exp (cons 'label _)))
       (compose (assign target) (analyze-label label-exp labels))]
      [(cons 'assign (cons target (and op-exp (cons (list 'op _) _))))
       (compose (assign target) (analyze-operation op-exp machine))]
      [_ (error 'analyze-assign "invalid assign instruction: ~a" inst)])))

;; (test (op <operation-name>) <input_1> ... <input_n>)
;; (test (reg <register-name>))
(define (analyze-test inst labels machine)
  (with-machine-states machine
    (define (assign-flag val)
      (set-contents! flag val)
      (advance-pc pc))
    (match inst
      [(cons 'test (and op-exp (cons (list 'op _) _)))
       (compose assign-flag (analyze-operation op-exp machine))]
      [(list 'test (and reg-exp (cons 'reg _)))
       (compose assign-flag (analyze-reg reg-exp machine))]
      [_ (error 'analyze-test "invalid test instruction: ~a" inst)])))

;; (branch (label <label-name>))
(define (analyze-branch inst labels machine)
  (with-machine-states machine
    (match inst
      [(list 'branch (and label-exp (cons 'label _)))
       (let ([get-insts (analyze-label label-exp labels)])
         (lambda ()
           (if (get-contents flag)
               (set-contents! pc (get-insts))
               (advance-pc pc))))])))

;; (goto (label <label-name>))
(define (analyze-goto inst labels machine)
  (with-machine-states machine
    (match inst
      [(list 'goto (and label-exp (list 'label label)))
       (let ([get-insts (analyze-label label-exp labels)])
         (lambda () (set-contents! pc (get-insts))))])))

;; (perform (op <operation-name>) (input_1> ... <input_n>
(define (analyze-perform inst labels machine)
  (with-machine-states machine
    (match inst
      [(cons 'perform op-exps)
       (compose (lambda _ (advance-pc pc)) (analyze-operation op-exps machine))]
      [_ (error 'analyze-perform "invalid perform instruction ~a" inst)])))

;; (save <register-name>)
(define (analyze-save inst labels machine)
  (with-machine-states machine
    (match inst
      [(list 'save register-name)
       (lambda ()
         (if ((machine 'register-allocated) register-name)
             (let ([contents (get-register-contents machine register-name)])
               (push stack contents)
               (advance-pc pc))
             (error 'save "regsiter used before assigning to a value: ~a" register-name)))]
      [_ (error 'analyze-save "invalid save instruction ~a" inst)])))

;; (restore <register-name>)
(define (analyze-restore inst labels machine)
  (with-machine-states machine
    (match inst
      [(list 'restore register-name)
       (lambda ()
         (set-register-contents! machine register-name (pop stack))
         (advance-pc pc))]
      [_ (error 'analyze-restore "invalid restore instruction: ~a" inst)])))

(define instruction-dispatch-table
  (make-parameter
   (hash
    'assign analyze-assign
    'test analyze-test
    'branch analyze-branch
    'goto analyze-goto
    'perform analyze-perform
    'save analyze-save
    'restore analyze-restore)))

(define (make-stack)
  (define s '())
  (define number-pushes 0)
  (define max-depth 0)
  (define current-depth 0)

  (define (push x)
    (set! s (cons x s))
    (set! number-pushes (add1 number-pushes))
    (set! current-depth (add1 current-depth))
    (set! max-depth (max current-depth max-depth)))

  (define (pop)
    (if (null? s)
        (error 'pop "empty stack")
        (let ([top (car s)])
          (set! s (cdr s))
          (set! current-depth (sub1 current-depth))
          top)))

  (define (initialize)
    (set! s '())
    (set! number-pushes 0)
    (set! current-depth 0))

  (define (print-statistics)
    (newline)
    (display (list 'total-pushes '= number-pushes
                   'maximum-depth '= max-depth)))

  (match-lambda
    ['push push]
    ['pop (pop)]
    ['initialize (initialize)]
    ['print-statistics (print-statistics)]
    [otherwise (error 'stack "unknown requset" otherwise)]))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;; the machine instruction data structure simply pairs the instruction text with the corresponding
;; execution procedure. The execution procedure is not yet available when [extract-labels] constructs
;; the instruction, and is inserted later by [udpate-insts!]
(define (make-instruction text) (mcons text '()))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

;; construct an object whose local state consists of a stack, an initially empty instruction sequence,
;; a list of operations that initially contains two registers, named [flag] and [pc] (program counter)
(define (make-new-machine)
  ;; the [pc] register determines the sequencing of instruction as the machine runs
  (define pc (make-register 'pc))
  ;; [flag] register is used to control branching
  ;; [test] instructions set the contents of [flag] to the result of test
  (define flag (make-register 'flag))
  ;; [branch] instructions decide whether or not to branch by examining the contents of [flag]
  (define stack (make-stack))
  (define the-instruction-sequence '())
  (define the-ops (list (list 'initialize-stack (lambda () (stack 'initialize)))))
  (define register-table (list (list 'pc pc) (list 'flag flag)))

  (set-contents! flag #f)

  (define (allocate-register name)
    (if (assq name register-table)
        (error "multiply defined register: " name)
        (set! register-table
              (cons (list name (make-register name))
                    register-table))))

  (define (register-allocated name)
    (cond [(assq name register-table) #t]
          [else #f]))

  ;; looks up registers in the table
  (define (lookup-register name)
    (cond [(assq name register-table) => cadr]
          [else (error 'lookup-register "unknown register: ~a" name)]))

  (define (execute)
    (define insts (get-contents pc))
    (cond [(null? insts) 'done]
          [else ((instruction-execution-proc (car insts)))
                (execute)]))

  (define (start)
    (set-contents! pc the-instruction-sequence)
    (execute))

  (define (install-instruction-sequence seq)
    (set! the-instruction-sequence seq))

  (define (install-operations ops)
    (set! the-ops (append the-ops ops)))

  (match-lambda
    ['start (start)]
    ['install-instruction-sequence install-instruction-sequence]
    ['allocate-register allocate-register]
    ['get-register lookup-register]
    ['register-allocated register-allocated]
    ['install-operations install-operations]
    ['stack stack]
    ['operations the-ops]
    [otherwise (error 'machine "unknown request ~a" otherwise)]))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (when (not ((machine 'register-allocated) register-name))
    ((machine 'allocate-register) register-name))
  (set-contents! (get-register machine register-name) value))
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (make-machine ops controller-text)
  ;; [make-machine] begins by calling the procedure [make-new-machine] to construct the parts of the
  ;; machine model that are common to all register machines. the basic machine model constructed by
  ;; [make-new-machine] is essentially a container for some registers and a stack, together with an
  ;; execution mechanism that processes the controller instructions one by one
  (define machine (make-new-machine))

  ;; install the designated operations in the machine
  ((machine 'install-operations) ops)
  ;; use an [assembler] to transform the controller list into instructions for the new machine and
  ;; install these as the machine's instruction sequence
  ((machine 'install-instruction-sequence)
   (assemble controller-text machine))
  ;; return the modified machine model
  machine)

;; main entry to the assembler.
;; takes the controller text and the machine model as arguments
;; returns the instruction sequence to be stored in the model.
(define (assemble controller-text machine)
  ;; calls [extract-labels] to build the initial instruction list and label table from the supplied
  ;; controller text
  (extract-labels
   controller-text
   ;; procedure to be called to process these result
   (lambda (insts labels)
     ;; use [update-insts!] to generate the instruction execution procedures and insert them into the
     ;; instruction list, and returns the modified list
     (update-insts! insts labels machine)
     insts)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

;; [text]: the sequence of controller instruction expressions
;; [receive]: will be called with two values:
;;   (1) a list [insts] of instruction data structures, each containing an instruction from text.
;;   (2) a table called [labels], which assiocates each label from [text] with the position in the
;;       list [insts] with the position in the list [insts] that the label designates.
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ([next-inst (car text)])
           ;; if [next-inst] is a label
           (if (symbol? next-inst)
               ;; Exercise 5.8
               (if (memq next-inst (map car labels))
                   (error 'extract-labels "duplicate label: ~a" next-inst)
                   (receive insts (cons (make-label-entry next-inst insts) labels)))
               (receive (cons (make-instruction next-inst) insts) labels)))))))

;; modifies the instruction list, which initially contains only the text of the instructions, to
;; include the corresponding execution procedures
(define (update-insts! insts labels machine)
  (with-machine-states machine
    (for ([inst insts])
      (set-instruction-execution-proc!
       inst
       (analyze-instruction (instruction-text inst) labels machine)))))

(module+ test
  (require akari-sicp/lib/testing)

  (define test-machine (make-new-machine))

  ((test-machine 'install-operations)
   (list (list '+ +)))

  ((test-machine 'allocate-register) 'x)
  (set-register-contents! test-machine 'x 1)

  (run-tests
   (describe "test register machine"
     (describe "test extract-labels (exercise 5.8)"
       (it "no duplicate label"
         (extract-labels
          '(label1
            (assign r (op +) (const 1) (const 1))
            label2
            (assign r (op -) (const 1) (const 2)))
          (lambda (insts labels)
            (expect
             [insts
              => (map
                  make-instruction
                  '((assign r (op +) (const 1) (const 1))
                    (assign r (op -) (const 1) (const 2))))]
             [labels
              => `((label1
                    ,@(map
                       make-instruction
                       '((assign r (op +) (const 1) (const 1))
                         (assign r (op -) (const 1) (const 2)))))
                   (label2
                    ,@(map
                       make-instruction
                       '((assign r (op -) (const 1) (const 2))))))]))))
       (it "label used more than once"
         (expect
          [(extract-labels
            '(label1
              (assign r (op +) (const 1) (const 1))
              label1
              (assign r (op +) (const 1) (const 2)))
            values) =!> #rx"duplicate label"])))
     (describe "test [analyze-operation]"
       (it "op with const argument"
         (expect [((analyze-operation '((op +) (const 1) (const 2)) test-machine)) => 3]))
       (it "op with register argument"
         (expect [((analyze-operation '((op +) (reg x) (const 2)) test-machine)) => 3]))
       (it "op should reject label argument (exercise 5.9)"
         (expect [((analyze-operation '((op +) (const 1) (label y)) test-machine))
                  =!> #rx"expect const or reg"])))
     (describe "test [analyze-assign]"
       (it "should allocate register if register not exists (exercise 5.13)"
         (define machine (make-machine
                          (list (list '+ +))
                          '((assign x (const 2)))))
         (start machine)
         (expect [(get-contents ((machine 'get-register) 'x)) => 2]))
       (it "assign operation result"
         (define machine (make-machine
                          (list (list '+ +))
                          '((assign x (op +) (const 1) (const 2)))))
         (start machine)
         (expect [(get-contents ((machine 'get-register) 'x)) => 3]))

       (it "assign register contents"
         (define machine (make-machine
                          '()
                          '((assign x (const 1))
                            (assign y (reg x)))))
         (start machine)
         (expect [(get-contents ((machine 'get-register) 'x)) => 1]
                 [(get-contents ((machine 'get-register) 'y)) => 1]))
       (it "assign label"
         (define machine (make-machine
                          '()
                          '((assign x (label t))
                            t
                            (assign y (const 1)))))
         (start machine)
         (expect [(get-contents ((machine 'get-register) 'y)) => 1]
                 [(map instruction-text (get-contents ((machine 'get-register) 'x)))
                  => '((assign y (const 1)))])))
     (describe "test [analyze-perform]"
       (it "should perform side effect correctly"
         (define machine (make-machine
                          (list (list 'print display))
                          '((assign x (const 1))
                            (perform (op print) (reg x)))))
         (expect [(start machine) =$> '("1")])))
     (describe "test [analyze-test]"
       (it "test operation"
         (define machine (make-machine
                          (list (list '> >))
                          '((test (op >) (const 2) (const 1)))))
         (start machine)
         (expect [(get-register-contents machine 'flag) => #t])

         (define machine* (make-machine
                           (list (list '> >))
                           '((test (op >) (const 1) (const 2)))))
         (start machine*)
         (expect [(get-register-contents machine* 'flag) => #f]))
       (it "test register"
         (define machine (make-machine
                          '()
                          '((assign x (const #t))
                            (test (reg x)))))
         (start machine)
         (expect [(get-register-contents machine 'flag) => #t])))
     (describe "test [analyze-goto]"
       (it "should jump to correct location"
         (define machine (make-machine
                          '()
                          '((goto (label alt))
                            (assign x (const 1))
                            alt
                            (assign y (const 2)))))
         (start machine)
         (expect
          [((machine 'register-allocated) 'x) => #f]
          [((machine 'register-allocated) 'y) => #t]
          [(get-register-contents machine 'y) => 2])))
     (describe "test [analyze-branch]"
       (it "should jump when [flag] is #t"
         (define machine (make-machine
                          '()
                          '((branch (label alt))
                            (assign x (const no-jmp))
                            (goto (label end))
                            alt
                            (assign x (const jmp))
                            end)))
         ((machine 'allocate-register) 'x)
         (set-register-contents! machine 'flag #t)
         (start machine)
         (expect [(get-register-contents machine 'x) => 'jmp]))
       (it "should fall through when [flag] is #f"
         (define machine (make-machine
                          '()
                          '((branch (label alt))
                            (assign x (const no-jmp))
                            (goto (label end))
                            alt
                            (assign x (const jmp))
                            end)))
         ((machine 'allocate-register) 'x)
         (set-register-contents! machine 'flag #f)
         (start machine)
         (expect [(get-register-contents machine 'x) => 'no-jmp])))
     (describe "test [analyze-save] and [analyze-restore]"
       (it "should save and restore correctly"
         (define machine
           (make-machine
            '()
            '((assign x (const 1))
              (assign y (const 2))
              (assign z (const 3))
              (save x)
              (save y)
              (save z)
              (restore x)
              (restore y)
              (restore z))))

         (start machine)

         (expect [(get-register-contents machine 'x) => 3]
                 [(get-register-contents machine 'y) => 2]
                 [(get-register-contents machine 'z) => 1]))))))
