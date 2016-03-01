
(define true #t)
(define false #f)
(define null 'null)

(define global
    (environment
        identity
        handler
        (record 'env-type 'immutable)
        'send send
        'apply apply
        ...))

