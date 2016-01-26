
(define (uri? str)
    (string-contains str ":"))

(define (absolute-path? path)
    (define idx0 (string-ref path 0))
    (or (equal? idx0 #\~) (equal? idx0 #\/)))

(define (get-uri uri)
    (define (reader port)
        (read-string #f port))
    (handle-exceptions exn
            not-found
            (call-with-input-request
                uri
                #f
                reader)))

(define (get-sexy-path f)
    (irregex-replace/all "[^a-zA-Z0-9_.]" f "_"))

(define (get-sexy-module-path f)
    (string-append sexy-mod-dir "/" (get-sexy-path f)))

(define (get-sexy-cached-path f)
    (string-append sexy-cache-dir "/" (get-sexy-path f)))

(define (find-file path)
    (define (fnf f)
        (debug "File not found!" f)
        (exit))
    (cond
        ((symbol? path)
            (niy))
        ((uri? path)
            (let ((module-path (get-sexy-module-path path)))
                (if (file-exists? module-path)
                    module-path
                    (let ((prog-text (get-uri path)))
                        (if (eq? prog-text not-found)
                            (fnf path)
                            (let ((mport (open-output-file module-path)))
                                (display prog-text mport)
                                (close-output-port mport)
                                module-path))))))
        ((file-exists? path)
            path)
        (else
            (fnf path))))

(define (file-newer? f1 f2)
    (> (file-modification-time f1) (file-modification-time f2)))

(define (make-module-absolute-path path)
    (cond
        ((symbol? path)
            (let ((str (symbol->string path)))
                (define xs (string-split str "/"))
                (define name (string->symbol (car xs)))
                (define the-rest (string-join (cdr xs) "/"))
                (define the-fun (lookup load-symbols-env name top-cont top-err))
                (if (not (and (hash-table? the-fun) (eq? (htr the-fun 'type) 'fn)))
                    (sexy-error path "No entry found in symbols.sxy for " name)
                    (sexy-apply the-fun (list the-rest) 'null top-cont top-err))))
        ((or (uri? path) (absolute-path? path)) path)
        (else (string-append *cwd* "/" path))))


(define (make-module-path-to path)
    (irregex-replace "(/.*)/.*$" path 1))

(define (sexy-read-expand path env)
    (sexy-expand
        (sexy-read-file
            (open-input-file path))
        env))

(define (read-expand-cache-prog path env)
    (define abs-path (make-module-absolute-path path))
    (define path-to (make-module-path-to abs-path))
    (define fpath (find-file abs-path))
    (define cpath (get-sexy-cached-path fpath))
    (define is-cached (and (file-exists? cpath) (file-newer? cpath fpath)))
    (if (and *use-cache* is-cached)
        (call-with-input-file
            cpath
            sexy-read)
        (let ((old-wd *cwd*))
            (set! *cwd* path-to)
            (set! *use-cache* #f)
            (let ((expanded (sexy-read-expand fpath env))
                  (fport (open-output-file cpath)))
                (define finished
                    (cons
                        (delete-duplicates (find-modules expanded))
                        expanded))
                (sexy-write finished fport)
                (close-output-port fport)
                (set! *cwd* old-wd)
                (set! *use-cache* #t)
                finished))))

(define (find-modules prog)
    (define (finder prog xs)
        (let loop ((form (car prog)) (rest (cdr prog)) (mods xs))
            (if (pair? form)
                (case (car form)
                    ((quote)
                        (if (pair? rest)
                            (finder rest mods)
                            mods))
                    ((load)
                        (let ((numods (cons (make-module-absolute-path (cadr form)) mods)))
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
    (cons 'modules (finder prog '())))

