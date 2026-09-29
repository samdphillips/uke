#lang racket

(require file/gunzip)

(provide gz-file->lines)

(define ((make-coding-reader coder [who #f]) input)
  (define-values (from-pipe to-pipe) (make-pipe))
  (define err #f)
  (define (save-err e) (set! err e))
  (define (do-read buf)
    (when err
      (raise err))
    (define read-size
      (read-bytes! buf from-pipe))
    read-size)
  (define (do-close)
    (close-input-port from-pipe)
    (kill-thread thd)
    (close-input-port input))
  (define thd
    (thread
     (lambda ()
       (with-handlers ([exn:fail? save-err])
         (coder input to-pipe)
         (close-output-port to-pipe)))))
  (make-input-port who do-read #f do-close))

(define gunzip-port (make-coding-reader gunzip-through-ports 'gunzip-port))

(define (gz-file->lines gz-file)
  (call-with-input-file gz-file
    (λ (inp-gz) (port->lines (gunzip-port inp-gz)))))

(error-print-width 160)