#lang racket

; Constants
(define buffer-size 256)

; Pure functions
(define (remove-trailing-spaces str)
  (let [(reg-match (regexp-match #px"^(.*\\S)\\s*$" str))]
    (if reg-match
        (list-ref reg-match 1)
        "")))

(define (write-line line hfile)
  (write-string (string-append line (string #\newline)) hfile))

(define (stringnl-size str)
  (bytes-length (string->bytes/utf-8 (string-append str (string #\newline)))))

(define (remove-CR str)
  (string-replace str "\r" ""))

(define (split-endl str)
  (string-split str "\n"))

(define (all-but-last lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (all-but-last (cdr lst)))))

; Functions with side effects
(define (write-nl hfile line last?)
  (write-line line hfile)
  (if last?
      (bytes-length (string->bytes/utf-8 line))
      (stringnl-size line)))

(define (prune-helper hrfile hwfile prev-buffer num-bytes)
  (let [(fresh-buffer (read-string buffer-size hrfile))]
    (if (not (eof-object? fresh-buffer))
        (let [(curr-buffer-list (split-endl (remove-CR (string-append prev-buffer fresh-buffer))))]
          (prune-helper hrfile
                        hwfile
                        (last curr-buffer-list)
                        (+ num-bytes (apply +
                                            (map
                                             (λ (line) (write-nl hwfile (remove-trailing-spaces line) #f))
                                             (all-but-last curr-buffer-list))))))
        (+ num-bytes (write-nl hwfile (remove-trailing-spaces prev-buffer) #t)))))

(define (prune hrfile hwfile old-filesize)
  (let [(new-filesize (prune-helper hrfile hwfile "" 0))]
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