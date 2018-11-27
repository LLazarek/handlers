#lang racket

(require racket/control
         (for-syntax syntax/parse))

(struct effect (id k args) #:transparent)

(define (ef id . args)
  (shift k
         (raise (effect id k args))))

(define-syntax (with-eff-handler/deep* stx)
  (syntax-parse stx
    [(_ ([eff-call-id handler-fn])
        body ...)
     ;; Name the current handler so that it can be re-applied to continuations
     #'(letrec ([run-with-handled-eff
                 (λ (thunk)
                   (with-handlers
                     ([effect?
                       (λ (eff)
                         (match-define (effect eff-id eff-k eff-args) eff)
                         ;; Only handle effects intended for this handler
                         (if (equal? eff-id 'eff-call-id)
                             ;; Handle by calling the user-specified handler
                             (handler-fn
                              ;; but wrap the continuation we provide it
                              ;; so that it is also handled by this handler
                              ;; when invoked
                              (λ (arg-intended-for-effect-k)
                                (run-with-handled-eff
                                 (λ _ (eff-k arg-intended-for-effect-k))))
                              eff-args)

                             ;; If intended for another handler:
                             ;; rethrow the effect, but reconstruct
                             ;; the continuation to include the
                             ;; portion between this handler and the
                             ;; next handler, as well as add the deep
                             ;; handling on the continuation of the
                             ;; current handler
                             (shift rethrow-k
                                    (raise (effect eff-id
                                                   (λ (k-arg)
                                                     (rethrow-k
                                                      (run-with-handled-eff
                                                       (λ _ (eff-k k-arg)))))
                                                   eff-args)))))])
                     (thunk)))])
         (run-with-handled-eff
          (λ _ (reset body ...))))]))

(define-syntax (with-eff-handler/shallow* stx)
  (syntax-parse stx
    [(_ ([eff-call-id handler-fn])
        body ...)
     ;; Name the current handler so that it can be re-applied to continuations
     #'(letrec ([run-with-handled-eff
                 (λ (thunk)
                   (with-handlers
                     ([effect?
                       (λ (eff)
                         (match-define (effect eff-id eff-k eff-args) eff)
                         ;; Only handle effects intended for this handler
                         (if (equal? eff-id 'eff-call-id)
                             ;; Handle by calling the user-specified handler
                             (handler-fn
                              ;; Don't wrap the continuation!
                              eff-k
                              eff-args)

                             ;; If intended for another handler:
                             ;; rethrow the effect, but reconstruct
                             ;; the continuation to include the
                             ;; portion between this handler and the
                             ;; next handler, as well as add the deep
                             ;; handling on the continuation of the
                             ;; current handler
                             (shift rethrow-k
                                    (raise (effect eff-id
                                                   (λ (k-arg)
                                                     (rethrow-k
                                                      (run-with-handled-eff
                                                       (λ _ (eff-k k-arg)))))
                                                   eff-args)))))])
                     (thunk)))])
         (run-with-handled-eff
          (λ _ (reset body ...))))]))
