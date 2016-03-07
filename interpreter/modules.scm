
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
        (let ((mods (find-modules program)))
            (map def-sexy-module mods)
            (hts!
                sexy-modules
                path
                (sexy-seq-subcontractor program #t)))
        (hts! sexy-modules path nop)))

(define (find-modules prog)
    (define (finder prog xs)
        (let loop ((form (car prog)) (rest (cdr prog)) (mods xs))
            (if (pair? form)
                (case (car form)
                    ((quote)
                        (if (pair? rest)
                            (finder rest mods)
                            mods))
                    ((use)
                        (let ((numods (cons (caddr form) mods)))
                            (if (pair? rest)
                                (loop (car rest) (cdr rest) numods)
                                numods)))
                    (else
                        (let ((numods (finder form mods)))
                            (if (pair? rest)
                                (finder rest numods)
                                numods))))
                (if (pair? rest)
                    (loop (car rest) (cdr rest) mods)
                    mods))))
    (delete-duplicates (finder prog '())))

