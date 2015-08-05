
(define sexy-modules (mkht))

(define (def-sexy-module path)
    (define has? (hte? sexy-modules path))
    (if has?
        #f
        (let ((expanded (read-expand-cache-prog path (local-env))))
            (hts! sexy-modules path 'loading)
            (sexy-eval-module expanded path))))

(define (sexy-eval-module program path)
    (define nop (lambda args 'null))
    (if (pair? program)
        (let ((mods (cdar program)))
            (map def-sexy-module mods)
            (hts!
                sexy-modules
                path
                (sexy-seq-subcontractor (cdr program) #t)))
        (hts! sexy-modules path nop)))

