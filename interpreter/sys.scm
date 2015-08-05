
(define sys
    (sexy-object
        (list
            'stdin   (current-input-port)
            'stdout  (current-output-port)
            'stderr  (current-error-port)
            'env
                (sexy-object
                    (list
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
                    #f #f #f)
            'exit exit
            'srand
                (lambda (v)
                    (randomize v)
                    'null)
            'launch-the-missile
                (lambda ()
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
            'file
                (sexy-object
                    (list
                        'read  open-input-file
                        'write open-output-file
                        'from
                            (sexy-proc
                                'primitive-function
                                'sys
                                (lambda (args opts cont err)
                                    (call-with-input-file (car args)
                                        (lambda (f)
                                            (sexy-apply (cadr args) (list f) cont err)))))
                        'to (sexy-proc
                                'primitive-function
                                'sys
                                (lambda (args opts cont err)
                                    (call-with-output-file (car args)
                                        (lambda (f)
                                            (sexy-apply (cadr args) (list f) cont err)))))
                        'stat
                            (lambda (f)
                                (file-stat f))
                        'symlink?
                            (lambda (f)
                                (symbolic-link? f))
                        'rm (lambda (f) (delete-file* f))
                        'cp (lambda (old new) (file-copy old new))
                        'mv (lambda (old new) (file-move old new))
                        'ln (lambda (old new) (create-symbolic-link old new))
                        'tmp (lambda () (create-temporary-file))
                    )
                    '(tmp) #f #f)
            'dir
                (sexy-object
                    (list
                        'mk (lambda (dir) (create-directory dir #t))
                        'rm (lambda (dir) (delete-directory dir #t))
                        'tmp (lambda () (create-temporary-directory))
                    )
                    '(tmp) #f #f)
            'tcp
                (sexy-object
                    (list
                        'connect (lambda (host port)
                            (define-values (in out) (tcp-connect host port))
                            (sexy-socket in out))
                        'listen (lambda (host port)
                            (sexy-listener host port))
                    )
                    #f #f #f)
            'signal
                (sexy-object
                    (list
                        'send (lambda (pid sig) (process-signal pid sig))
                        'mask (lambda (sig) (signal-mask! sig))
                        'masked? (lambda (sig) (signal-masked? sig))
                        'unmask (lambda (sig) (signal-unmask! sig))
                        'handler (lambda (sig) (signal-handler sig))
                        'handle (lambda (sig fn)
                                    (set-signal-handler!
                                        sig
                                        (lambda (sig)
                                            (sexy-apply fn (list sig) top-cont top-err))))
                    )
                    #f #f #f)
            'proc
                (sexy-object
                    (list
                        'pid (lambda () (current-process-id))
                        'uid (lambda () (current-user-id))
                        'gid (lambda () (current-group-id))
                        'parent-pid (lambda () (parent-process-id))
                        'process-gid (lambda (pid) (process-group-id pid))
                        'run (lambda (cmd) (process-run cmd))
                        'fork (lambda (thunk) (process-fork thunk))
                    )
                    '(pid uid gid parent-pid process-gid) #f #f)
            '64764 (lambda () (display "\n    **** COMMODORE 64 BASIC V2 ****\n\n 64K RAM SYSTEM  38911 BASIC BYTES FREE\n\n") 'READY.)
            'ts (lambda () (inexact->exact (current-seconds)))
            'uname (system-information)
            'hostname (get-host-name)
            'sleep (lambda (s) (sleep s))
            'pwd (lambda () (current-directory))
            'chdir (lambda (dir) (change-directory dir))
            'chroot (lambda (dir) (set-root-directory! dir))
            'shell (lambda (cmd)
                (read-all (process cmd)))
            'read
                (sexy-proc
                    'primitive-function
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stdin
                            (lambda (in)
                                (cont (sexy-read in)))
                            err)))
            'write
                (sexy-proc
                    'primitive-function
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stdout
                            (lambda (out)
                                (sexy-write (car args) out)
                                (cont 'null))
                            err)))
            'print
                (sexy-proc
                    'primitive-function
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stdout
                            (lambda (out)
                                (sexy-print (car args) out)
                                (cont 'null))
                            err)))
            'carp
                (sexy-proc
                    'primitive-function
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'stderr
                            (lambda (stderr)
                                (sexy-print (car args) stderr)
                                (newline stderr)
                                (cont 'null))
                            err)))
            'say
                (sexy-proc
                    'primitive-function
                    'sys
                    (lambda (args opts cont err)
                        (sexy-send sys 'print
                            (lambda (printer)
                                (sexy-apply printer args
                                    (lambda (x)
                                        (newline)
                                        (cont 'null))
                                    err))
                            err)))
            'test
                (lambda (tname ok)
                    (debug tname (if ok 'ok 'FAIL))
                    'null))
        '(ts pwd exit 64764 launch-the-missile)
        #f
        #f))

