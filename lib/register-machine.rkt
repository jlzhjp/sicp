#lang racket/base


(define (make-machine register-names ops controller-text)
  ;; [make-machine] begins by calling the procedure [make-new-machine] to construct the parts of the
  ;; machine model that are common to all register machines. the basic machine mdoel constructed by
  ;; [make-new-machine] is essentially a container for some registers and a stack, together with an
  ;; execution machanism that processes the controller instructions one by one
  (let ([machine (make-new-machine)])
    ;; [make-machine] then extends this baisc model to include the registers, operations, and
    ;; controller of the particular machine being defined.

    ;; allocates a register in the new machine for each of the supplied register names
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ;; install the designated operations in the machine
    ((machine 'install-operations) ops)
    ;; use an [assembler to transform the controller list into instructions for the new machine and
    ;; install these as the machine's instruction sequence
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ;; return the modified machine model
    machine))

;; [make-register] creates a register that holds a value that can be accessed or changed
(define (make-register name)
  (let ([contents '*unassigned*])
    (define (dispatch message)
      (cond [(eq? message 'get) contents]
            [(eq? message 'set)
             (lambda (value) (set! contents value))]
            [else (error 'register "unknown request ~a" message)]))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; [make-stack] creates a stack whose local state consists of a list of the items on the stack
;; a stack accepts requests to push an item onto the stack, the [pop] the top item off the stack and
;; return it, and to [initialize] the stack to empty
(define (make-stack)
  (let ([s '()])
    (define (push x) (set! s (cons x s)))

    (define (pop)
      (if (null? s)
          (error 'pop "empty stack")
          (let ([top (car s)])
            (set! s (cdr s))
            top)))

    (define (initialize)
      (set! s '()))

    (define (dispatch message)
      (cond [(eq? message 'push) push]
            [(eq? message 'pop) (pop)]
            [(eq? message 'initialize) (initialize)]
            [else (error 'stack "unknown request" message)]))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;; construct an object whose local state consists of a stack, an initially empty instruction sequence,
;; a list of operations that initially contains two registers, named [flag] and [pc] (program counter)
(define (make-new-machine)
  ;; the [pc] register determines the sequencing of instruction as the machine runs
  (let ([pc (make-register 'pc)]
        ;; [flag] register is used to control branching
        ;; [test] instructions set the contents of [flag] to the result of test
        ;; [branch] instructions decide whether or not to branch by examing the contents of [flag]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()])
    (let ([the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize))))]
          [register-table
           (list (list 'pc pc) (list 'flag flag))])

      ;; adds new entries to the register table
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)

      ;; looks up registers in the table
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (error "unknown register: " name))))

      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))

      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq))]
              [(eq? message 'allocate-register)
               allocate-register]
              [(eq? message 'get-register)
               lookup-register]
              [(eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops)))]
              [(eq? message 'stack) stack]
              [(eq? message 'operations) the-ops]
              [else (error 'machine "unknown request" message)]))
      dispatch)))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

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
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

;; modifies the instruction list, which initially contains only the text of the instructions, to
;; include the corresponding execution procedures
(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))

;; the machine instruction data structure simply pairs the instruction text with the corresponding
;; execution procedure. The execution procedure is not yet available when [extract-labels] constructs
;; the instruction, and is inserted later by [udpate-insts!]
(define (make-instruction text) (mcons text '()))
(define (instruction-text inst) (mcar inst))
(define (instruction-execution-proc inst) (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ([val (assoc label-name labels)])
    (if val
        (cdr val)
        (error 'assemble "undefined label" label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond [(eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc)]
        [(eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc)]
        [(eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc)]
        [(eq? (car inst) 'goto)
         (make-goto inst machine labels pc)]
        [(eq? (car inst) 'save)
         (make-save inst machine stack pc)]
        [(eq? (car inst) 'restore)
         (make-restore inst machine stack pc)]
        [(eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc)]
        [else (error 'assemble "unknown instruction type" inst)]))

;; [make-assign] extracts the target register name (the second element of the instruction) and the
;; value expression (the rest of the list that forms the instruction) from the [assign] instruction
;; using the selectors
(define (make-assign inst machine labels operations pc)
  ;; the target register name (the second element of the instruction)
  (let ([target (get-register machine (assign-reg-name inst))]
        ;; the value expression (the rest of the list that forms the instruction)
        [value-exp (assign-value-exp inst)])
    (let ([value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))])
      (lambda () ; execution procedure for [assign]
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (caddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ([condition (test-condition inst)])
    (if (operation-exp? condition)
        (let ([condition-proc
               (make-operation-exp
                condition machine labels operations)])
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error 'assemble "bad test instruction ~a" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
        (let ([insts (lookup-label labels (label-exp-label dest))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error 'assemble "bad branch instruction: ~a" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond [(label-exp? dest)
           (let ([insts (lookup-label labels (label-exp-label dest))])
             (lambda () (set-contents! pc insts)))]
          [(register-exp? dest)
           (let ([reg (get-register machine (register-exp-reg dest))])
             (lambda () (set-contents! pc (get-contents reg))))]
          [else (error 'assemble "bad goto instruction ~a" inst)])))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
        (let ([action-proc
               (make-operation-exp
                action machine labels operations)])
          (lambda () (action-proc) (advance-pc pc)))
        (error 'assemble "bad perform instruction" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond [(constant-exp? exp)
         (let ([c (constant-exp-value exp)])
           (lambda () c))]
        [(label-exp? exp)
         (let ([insts (lookup-label labels (label-exp-label exp))])
           (lambda () insts))]
        [(register-exp? exp)
         (let ([r (get-register machine (register-exp-reg exp))])
           (lambda () (get-contents r)))]
        [error (error 'assemble "unknown expression type: ~a" exp)]))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))])
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ([val (assoc symbol operations)])
    (if val
        (cadr val)
        (error 'assemble "unknown operation ~a" symbol))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))