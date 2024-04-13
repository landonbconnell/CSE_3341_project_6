; call to start the interpreter
(define (plan program)
    (cond
        ; handles case where 'program' is only a <const>
        ((integer? program) program)

        ; traverses down binary tree until an integer or procedure name is
        ; the first item in the 'program' list
        ((not (null? (cdr program))) (plan (cdr program)))

        ; handles case where <const> is first element in the 'program' list
        ((integer? (car program)) (car program))

        ; handles calls to PLAN procedures
        ((equal? 'planadd (caar program)) (evalPlanAdd (cdar program)))
        ((equal? 'planmul (caar program)) (evalPlanMul (cdar program)))
        ((equal? 'plansub (caar program)) (evalPlanSub (cdar program)))
        ((equal? 'planif (caar program)) (evalPlanIf (cdar program)))
        ((equal? 'planlet (caar program)) (evalPlanLet (cdar program)))
    )
)

(define (evalPlanAdd program) (+ (plan (car program)) (plan (cdr program))))

(define (evalPlanMul program) (* (plan (car program)) (plan (cdr program))))

(define (evalPlanSub program) (- (plan (car program)) (plan (cdr program))))

(define (evalPlanIf program)
    (if (> 0 (plan (car program)))
        (plan (cadr program))
        (plan (caddr program))
    )
)

; (define (evalPlanLet program) ... )