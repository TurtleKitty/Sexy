
(define global-arg-pair
    (sexy-cli-args (command-line-arguments)))

(define sys-env
    (sexy-object
        (list
            'view
                (lambda () (sexy-view (get-environment-variables)))
            'uname    (system-information)
            'hostname (get-host-name)
            'get
                (lambda (x)
                    (define envt (get-environment-variables))
                    (define (try z)
                        (define p (assoc z envt))
                        (if p (cdr p) 'null))
                    (if (symbol? x)
                        (let ((y (symbol->string x)))
                            (try y))
                        (try x)))
            'set!
                (lambda (k v)
                    (if (symbol? k)
                        (setenv (symbol->string k) v)
                        (setenv k v))
                        v)
            'del!
                (lambda (k)
                    (if (symbol? k)
                        (unsetenv (symbol->string k))
                    (unsetenv k))
                    'null)
        )
        '(view) #f #f))

(define sys-fs
    (sexy-object
        (list
            'read       open-input-file
            'write      open-output-file
            'rm         (lambda (f) (delete-file* f) 'null)
            'cp         (lambda (old new) (file-copy old new))
            'mv         (lambda (old new) (file-move old new))
            'ln         (lambda (old new) (create-symbolic-link old new) 'null)
            'tmp        (lambda () (create-temporary-file))
            'pwd        (lambda () (current-directory))
            'ls         (lambda (dir) (directory dir #t))
            'cd         (lambda (dir) (change-directory dir) 'null)
            'chroot     (lambda (dir) (set-root-directory! dir) 'null)
            'mkdir      (lambda (dir) (create-directory dir #t) 'null)
            'rmdir      (lambda (dir) (delete-directory dir #t) 'null)
            'tmp-dir    (lambda () (create-temporary-directory))
            'stat       file-stat
            'exists?    file-exists?
            'dir?       directory?
            'symlink?   symbolic-link?
            'listen     sexy-fs-listener
            'connect
                (lambda (path)
                    (define-values (in out) (unix-connect path))
                    (sexy-fs-socket path in out))
            'socket-pair
                (lambda ()
                    (define-values (in1 out1 in2 out2) (unix-pair))
                    (cons (sexy-fs-socket '? in1 out1) (sexy-fs-socket '? in2 out2)))
            'from
                (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (call-with-input-file (car args)
                            (lambda (f)
                                (sexy-apply (cadr args) (list f) 'null cont err)))))
            'to (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (call-with-output-file (car args)
                            (lambda (f)
                                (sexy-apply (cadr args) (list f) 'null cont err)))))
        )
        '(pwd socket-pair tmp tmp-dir) #f #f))

(define sys-http
    (sexy-object
        (list
            'get (lambda (uri)
                     (define got (get-uri uri))
                     (if (eq? got not-found)
                         'null
                         got)))
        #f #f #f))

(define sys-tcp
    (sexy-object
        (list
            'connect (lambda (host port)
                (define-values (in out) (tcp-connect host port))
                (sexy-tcp-socket in out))
            'listen (lambda (host port)
                (sexy-tcp-listener host port))
        )
        #f #f #f))

(define sys-signal
    (sexy-object
        (list
            'hup   1
            'int   2
            'quit  3
            'ill   4
            'abrt  6
            'fpe   8
            'kill  9
            'usr1 10
            'segv 11
            'usr2 12
            'pipe 13
            'alrm 14
            'term 15
            'chld 17
            'cont 18
            'stop 19
            'send (lambda (pid sig) (process-signal pid sig))
            'mask (lambda (sig) (signal-mask! sig) 'null)
            'masked? (lambda (sig) (signal-masked? sig))
            'unmask (lambda (sig) (signal-unmask! sig) 'null)
            'handler (lambda (sig) (signal-handler sig))
            'handle (lambda (sig fn)
                        (set-signal-handler!
                            sig
                            (lambda (sig)
                                (sexy-apply fn (list sig) 'null top-cont top-err)))
                    'null)
        )
        #f #f #f))

(define sys-proc
    (sexy-object
        (list
            'pid (lambda () (current-process-id))
            'uid (lambda () (current-user-id))
            'gid (lambda () (current-group-id))
            'parent-pid (lambda () (parent-process-id))
            'process-gid (lambda (pid) (process-group-id pid))
            'program-name (lambda () (program-name))
            'run (lambda (cmd) (process-run cmd))
            'sleep (lambda (s) (sleep s))
            'fork (lambda (thunk)
                (process-fork
                    (lambda ()
                        (sexy-apply thunk '() 'null top-cont top-err))))
            'exit exit
        )
        '(pid uid gid parent-pid program-name) #f #f))

(define (norris-day)
    (inexact->exact (current-seconds)))

(define (fire-missile)
    (define (alert n)
        (display "Launching in ")
        (display n)
        (display "...")
        (newline)
        (sleep 1))
    (display "Are you sure you want to do that, cowboy?")
    (newline)
    (let ((response (read)))
        (let ((r (string-ref (symbol->string response) 0)))
            (if (or (eq? r #\y) (eq? r #\Y))
                (begin 
                    (display "Ok, mad hacker.  Hope you have a fallout shelter.")
                    (newline)
                    (let loop ((n 5))
                        (alert n)
                        (if (eq? n 1)
                            (begin
                                (display "Good luck...")
                                (newline)
                                (sleep 7)
                                'KABOOM)
                            (loop (- n 1)))))
                (begin
                    (display "Wise man.")
                    (newline)
                    'null)))))

(define sys
    (sexy-object
        (list
            'stdin    (current-input-port)
            'stdout   (current-output-port)
            'stderr   (current-error-port)
            'opt      (cdr global-arg-pair)
            'rest     (car global-arg-pair)
            'view     'operating-system-interface
            'env      sys-env
            'fs       sys-fs
            'signal   sys-signal
            'proc     sys-proc
            'net      (sexy-object (list 'http sys-http 'tcp sys-tcp) #f #f #f)
            'time     norris-day
            'ts       norris-day
            'srand
                (lambda (v)
                    (randomize v)
                    'null)
            'shell (lambda (cmd)
                (read-all (process cmd)))
            'read
                (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stdin
                            (lambda (in)
                                (cont (sexy-read in)))
                            err)))
            'write
                (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stdout
                            (lambda (out)
                                (sexy-write (car args) out)
                                (cont 'null))
                            err)))
            'print
                (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stdout
                            (lambda (out)
                                (sexy-print (car args) out)
                                (cont 'null))
                            err)))
            'say
                (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'print
                            (lambda (printer)
                                (sexy-apply printer args 'null
                                    (lambda (x)
                                        (newline)
                                        (cont 'null))
                                    err))
                            err)))
            'log
                (sexy-proc
                    primitive-type
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stderr
                            (lambda (stderr)
                                (sexy-write (car args) stderr)
                                (newline stderr)
                                (cont 'null))
                            err)))
            'test
                (lambda (test-name ok)
                    (sexy-send sys 'stdout
                        (lambda (out)
                            (sexy-write (list test-name (if ok 'ok 'FAIL)) out)
                            (newline out)
                            'null)
                        top-err))
            '64764 (lambda () (display "\n    **** COMMODORE 64 BASIC V2 ****\n\n 64K RAM SYSTEM  38911 BASIC BYTES FREE\n\n") 'READY.)
            'launch-the-missile fire-missile)
        '(ts 64764 launch-the-missile)
        #f
        #f))


