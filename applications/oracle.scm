(require "c:/dream/interfaces/windows/heap.scm")
;(require "linux/libc.scm")
(require "c:/dream/interfaces/oci.scm")
(define url "//xxx.xxx.xxx.xxx/SERVICENAME")
(define server #f)
(define service #f)
(define session #f)
(define (connect)
  (set! server (attach-server url))
  (if server
    (begin
      (set! service (make-service server))
      (if service
        (set! session (begin-session service "username" "password")))))
  service)

(define (disconnect)
  (if session (end-session service session))
    (if server (detach-server server)))

(if (connect)
  (begin
    ;;Select up to 100 chars for field1 and up to 50 for field2.
    (write (select service "select field1, field2 from table" 100 50)))
  (write (error-message)))
(disconnect)
