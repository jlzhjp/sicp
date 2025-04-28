#lang racket/base

(require
  racket/match
  syntax/parse/define
  (for-syntax racket/base))

(define machine-ops (make-parameter '()))
(define instructions (make-parameter '()))

(define-syntax (define/insts stx)
  (syntax-parse stx
    [(_ label insts ...)
     #'(instructions (append (instructions) `(label insts ...)))]))

(define-syntax (define/op stx)
  (syntax-parse stx
    [(_ (name:id params:id ...) body:expr ...+)
     #'(machine-ops (cons (list (quote name) (lambda (params ...) body ...)) (machine-ops)))]))

(define/insts begin-garbage-collection
  ;; The algorithm begins by relocating the pair pointed at by [root] to the begining of the new
  ;; memory. The pair is copied, the [root] pointer is adjusted to point the new location, and the
  ;; [free] pointer is incremented

  ;; The state of the garbage-collection process is controlled by maintaining two pointers: [free]
  ;; and [scan]
  (assign free (const 0))
  (assign scan (const 0))
  (assign old (reg root))
  (assign relocate-continue (label reassign-root))
  (goto (label relocate-old-result-in-new)))

(define/insts reassign-root
  (assign root (reg new))
  (goto (label gc-loop)))

(define/insts gc-loop
  ;; Determine whether there are any more objects to be scanned by testing whether the [scan] pointer
  ;; is coincident with the [free] pointer.
  ;; If the pointers are equal, then all accessible objects
  ;; have been relocated, and we branch to [gc-flip], which cleans things up so that we can continue
  ;; the interrupted computation.
  (test (op =) (reg scan) (reg free))
  (branch (label gc-flip))

  ;; If there are still pairs to be scanned, we call the relocate subroutine to relocate the [car] of
  ;; the next pair (by placing the [car] pointer in [old])
  (assign old (op vector-ref) (reg new-cars) (reg scan))

  ;; [The relocate-continue] is set up so that the subroutine will return to update the [car] pointer
  (assign relocate-continue (label update-car))
  (goto (label relocate-old-result-in-new)))

(define/insts update-car
  ;; At [update-car], we modify the [car] pointer of the pair being scanned, then proceed to relocate
  ;; the [cdr] of the pair.
  (perform (op vector-set!) (reg new-cars) (reg scan) (reg new))
  (assign relocate-continue (label update-cdr))
  (goto (label relocate-old-result-in-new)))

(define/insts update-cdr
  (perform (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
  (assign scan (op +) (reg scan) (const 1))
  (goto (label gc-loop)))

(define/insts gc-flip
  (assign temp (reg the-cdr))
  (assign the-cdrs (reg new-cdrs))
  (assign new-cdrs (reg temp))
  (assign temp (reg the-cars))
  (assign the-cars (reg new-cars))
  (assign new-cars (reg temp)))

(define/insts relocate-old-result-in-new
  ;; The subroutine [relocate-old-result-in-new] relocates objects as follows:
  ;; If the object to be located (pointed at by [old] is not a pair, then we return the same pointer
  ;;   to the object unchanged (in [new]) (e.g. we may be scanning a pair whose [car] is the number 4.
  ;;   If we represent the car by [n4], then we want the "relocated" [car] pointer to still be [n4].
  ;; Otherwise, we must perform the relocation. If the [car] position of the pair to be relocated
  ;;   contains a broken-heart tag, then the pair has in fact already be moved, so we retrieve the
  ;;   forwarding address (from the [cdr] position of the broken heart) and return this in [new].
  (test (op pointer-to-pair?) (reg old))
  (branch (label pair))
  (assign new (reg old))
  (goto (reg relocate-continue)))

(define/insts pair
  (assign oldcr (op vector-ref) (reg the-cars) (reg old))
  (test (op broken-heart?) (reg oldcr))
  (branch (label already-moved))
  (assign new (reg free)) ; new location for pair
  ;; Update [free] pointer
  (assign (free) (op +) (reg free) (const 1))
  ;; Copy the [car] and [cdr] to new memory.
  (perform (op vector-set!) (reg new-cars) (reg new) (reg oldcr))
  (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
  (perform (op vector-set!) (reg new-cdrs) (reg new) (reg oldcr))
  ;; Construct the broken heart
  (perform (op vector-set!) (reg the-cars) (reg old) (const broken-heart))
  (perform (op vector-set!) (reg the-cdrs) 9reg old) (reg new)
  (goto (reg relocate-continue)))

(define/insts already-moved
  (assign new (op vector-ref) (reg the-cdrs) (reg old))
  (goto (reg relocate-continue)))

(define/insts pair
  (assign oldcr (op vector-ref) (reg the-cars) (reg old)))

(define/op (self-evaluating? exp)
  (or (number? exp) (string? exp) (char? exp) (boolean? exp)))

(define/op (variable? exp)
  (symbol? exp))

(define/op (quoted? exp)
  (match exp
    [(cons 'quote _) #t]
    [_ #f]))

(define/op (assignment? exp)
  (match exp
    [(cons 'set! _) #t]
    [_ #f]))

(define/op (definition? exp)
  (match exp
    [(cons 'define _) #t]
    [_ #f]))

(define/op (if? exp)
  (match exp
    [(cons 'if _) #t]
    [_ #f]))

(define/op (lambda? exp)
  (match exp
    [(cons 'labmda _) #t]
    [_ #f]))

(define/op (begin? exp)
  (match exp
    [(cons 'begin _) #t]
    [_ #f]))

(define/op (application? exp)
  (list? exp))

(define (dispatch-on pred reg label)
  `((test (op ,pred) (reg ,reg))
    (branch (label ,label))))

(define/insts eval-dispatch
  ,@(dispatch-on 'self-evaluating? 'exp 'ev-self-eval)
  ,@(dispatch-on 'variable? 'exp 'ev-variable)
  ,@(dispatch-on 'quoted? 'exp 'ev-quoted)
  ,@(dispatch-on 'assignment? 'exp 'ev-assignment)
  ,@(dispatch-on 'definition? 'exp 'ev-definition)
  ,@(dispatch-on 'if? 'exp 'ev-if)
  ,@(dispatch-on 'begin? 'exp 'ev-begin)
  ,@(dispatch-on 'application? 'exp 'ev-application)
  (goto (label unknown-expression-type)))

(define/insts ev-self-eval
  (assign val (reg exp))
  (goto (reg continue)))

(define/insts ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue)))

(define/insts ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue)))

(define/insts ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body (reg exp)))
  (assgin val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue)))

(define/insts ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  ;; evaluate the operator to produce a procedure, which will later be applied to the evaluated
  ;; operands.
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch)))

(define/insts ev-appl-did-operator
  (restore unev)
  (resore env)
  (assign argl (op empty-arglist))
  ;; assign to the [proc] register the procedure that was produced by evaluating the oeprator
  (assign proc (reg val))
  ;; if there are not operands, we go directly to [apply-dispatch].
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  ;; otherwise we save [proc] on the stack and start the argument-evaluation loop
  (save proc))

(define/insts ev-appl-operand-loop
  ;; each cycle of the argument-evaluation loop evaluates an operand from the list in [unev] and
  ;; accumulate the result into [argl]
  (save argl)
  ;; to evaluate an operand, we place it in the [exp] register and go to eval-dispatch, after
  ;; setting [continue] so that execution will resume with the argument-accumulation phase.
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand? (reg unev)))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch)))

(define/insts ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  ;; when an operand has been evaluated, the value is accumulated into the list held in argl.
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  ;; the operand it then removed from the list of unevaluated operands in [unev]
  (assign unev (op rest-operands) (reg unev))
  ;; and the argument-evalution continues
  (goto (label ev-appl-operand-loop)))

(define/insts ev-appl-last-arg
  ;; Evaluation of the last argument is handled differently. There is no need to save the
  ;; environment or the list of unevaluated operands before going to [eval-dispatch], since they
  ;; will not be requried after the last operand is evaluated.
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))

  ev-appl-accum-last-arg
  ;; restores the argument list, accumulates the new argument, restores the saved procedure, and
  ;; goes off to perform the application
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch)))

(define/insts apply-dispatch
  (test (op primitive-procedure? (reg proc)))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type)))

(define/insts primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore (continue))
  (goto (reg continue)))

(define/insts primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore (continue))
  (goto (reg continue)))

(define/insts primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore (continue))
  (goto (reg continue)))

(define/insts compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  ;; [compound-apply] is the only place in the interpreter where the [env] register is ever assigned
  ;; a new value.
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment) (reg unev) (reg argl) (reg proc))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence)))

(define/insts ev-begin
  ;; Explicit [begin] expressions are evaluted by placing the sequence of expressions to be evaluated
  ;; [unev] saving [continue] on the stack, and jumping to [ev-sequence]
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence)))

(define/insts ev-seq
  ;; The entries at [ev-sequence] and [ev-sequence-continue] form a loop the successively evalutes
  ;; each expression in a sequence.uence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch)))

(define/insts ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg uenv))
  (goto (label ev-sequence)))

(define/insts ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch)))

(define/insts ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch)))

(define/insts ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent)))

(define/insts ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch)))

(define/insts ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch)))

(define/insts ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch)))

(define/insts ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue)))

(define/insts ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch)))

(define/insts ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue)))
