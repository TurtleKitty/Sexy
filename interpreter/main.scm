
; CHICKEN!

(use srfi-1)
(use srfi-13)
(use srfi-69)

(use http-client)
(use medea)
(use numbers)
(use openssl)
(use posix)
(use symbol-utils)
(use tcp)
(use unix-sockets)
(use utf8)
(use utils)
(use uuid)
(use vector-lib)


; start

(define usage-text #<<END

Usage:

sexy repl
sexy exec "<code string>"
sexy run <filename>
sexy check <filename>
sexy expand <filename>
sexy compile <filename>
sexy clean

END
)

(define (usage)
    (display usage-text)
    (newline)
    (exit))

(define top-cont identity)
(define top-err  (lambda (ex continue) (debug "Uncaught error: " ex) (exit)))

(define *cwd* (current-directory))
(define *use-cache* #t)
(define sexy-use-symbols "~/.sexy/symbols.sxy")
(define sexy-mod-dir     "~/.sexy/modules")
(define sexy-cache-dir   "~/.sexy/compiled")

(define genv #f)
(define g-has? #f)
(define g-get  #f)

(define load-symbols-env #f)
(define loaded-module-symbols)

(include "read_expand_cache")
(include "utils")
(include "reader")
(include "syntax_checker")
(include "objects")
(include "send")
(include "macro_expander")
(include "eval_apply")    
(include "compiler")
(include "sys")
(include "env")
(include "modules")
(include "repl")
(include "start")

(define global-arg-pair (sexy-cli-args (command-line-arguments)))


(start)


