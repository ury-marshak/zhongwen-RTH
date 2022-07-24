#lang racket/base
(require racket/list)
(require  racket/string)

(require csv-reading)
(require csv-writing)

(require "rth-tsv.rkt")

(define IN-FILENAME "Remembering Traditional Hanzi 1+2a.txt")
(define OUT-FILENAME "RTH-cedict.txt")

(define USED-ELEMENTS-FIELD-NUM STORY-FIELD-NUM) ;; Change to ELEMENTS-FIELD-NUM when it's ready


(define (choose-tag tags)
  (let ([result (findf (lambda (t)
                         (member t tags))
                       '("RTH1" "RTH2" "RT1-8"))])
    (unless result
      (error "No useful tag in " tags))
    result))

(define *compile-stories* #t)


(define (process-row row)
  (let* ([trad (string-trim (list-ref row CHARACTER-FIELD-NUM))]
         [simp (string-trim (list-ref row SIMPLIFIED-ID-FIELD-NUM))]
         [rth-index (list-ref row RTH-ID-FIELD-NUM)]
         [keyword (list-ref row KEYWORD-FIELD-NUM)]
         [keyword-info (list-ref row KEYWORD-INFO-FIELD-NUM)]
         [elements (list-ref row USED-ELEMENTS-FIELD-NUM)]
         [primitive (list-ref row PRIMITIVE-FIELD-NUM)]
         [tags (string-split (list-ref row TAGS-FIELD-NUM))]
         [story (string-trim (list-ref row STORY-FIELD-NUM))]
         [cangjie5 (list-ref row CANGJIE5-FIELD-NUM)]
         [heading (if (or (string=? simp "") (string=? simp trad))
                      trad
                      (format "~a[~a]" simp trad))]
         [keyword-info-str (if (non-empty-string? keyword-info)
                       (string-append-immutable " (" keyword-info ")")
                       "")]
         [pinyin ""])
    (when (= (string-length simp) 0)
      (set! simp trad))
    (format "~a ~a [xx5] /<span class='RTH'>~a:~a</span> <span class='keyword'>~a</span>~a ~a~a/"
            trad simp
            (choose-tag tags)
            rth-index
            keyword keyword-info-str
            (if (and *compile-stories* (positive? (string-length primitive)))
                (format "/(Prim: ~A)" primitive)
                "")
            (if (and *compile-stories* (positive? (string-length story)))
                (format "/~A" story)
                ""))))



(define (process-file (fname IN-FILENAME))
  (let ((data (read-RTH IN-FILENAME)))
    (let* ((pleco-strings (map process-row data)))
      (call-with-output-file OUT-FILENAME
        (lambda (out)
          (for ([s pleco-strings])
            (write-string s out)
            (write-string "\n" out)))
        #:exists 'replace)
      (take pleco-strings 18))
    ))

(module+ main
  (process-file))
