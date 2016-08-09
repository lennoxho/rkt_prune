#lang racket

(define (remove-trailing-spaces str)
  (let [(reg-match (regexp-match #px"^(.*\\S)\\s*$" str))]
    (if reg-match
        (list-ref reg-match 1)
        "")))

(define (write-line line hfile)
  (write-string (string-append line (string #\newline)) hfile))

(define (stringln-size str)
  (bytes-length (string->bytes/utf-8 (string-append str (string #\newline)))))

(define (prune-helper hrfile hwfile num-bytes)
  (let [(line (read-line hrfile 'any))]
    (if (not (eof-object? line))
        (let [(pruned-line (remove-trailing-spaces line))]
          (write-line pruned-line hwfile)
          (prune-helper hrfile hwfile (+ num-bytes (stringln-size pruned-line))))
        (- num-bytes (stringln-size "")))))

(define (prune hrfile hwfile old-filesize)
  (let [(new-filesize (prune-helper hrfile hwfile 0))]
    (printf "Number of bytes pruned = ~v\n" (- old-filesize new-filesize))
    (file-truncate hwfile new-filesize)))

(define (prune-file filename)
  (define hrfile (open-input-file filename))
  (define hwfile (open-output-file filename #:exists 'update))
  (prune hrfile hwfile (file-size filename))
  (close-input-port hrfile)
  (close-output-port hwfile))

(for-each prune-file (command-line
                   #:program "rkt_prune"
                   #:args (filename . rest)
                   (cons filename rest)))