#lang racket/base

(require csv-reading)
(require csv-writing)

(require "enum.rkt")

(provide read-RTH write-RTH write-RTH-csv
 CHARACTER-FIELD-NUM
 KEYWORD-FIELD-NUM
 KEYWORD-INFO-FIELD-NUM
 PRIMITIVE-FIELD-NUM
 PRIMITIVE-STORY-FIELD-NUM
 PINYIN-FIELD-NUM
 RTH-ID-FIELD-NUM
 STROKES-FIELD-NUM
 ELEMENTS-FIELD-NUM
 STORY-FIELD-NUM
 HINT-FIELD-NUM
 COMMENTS-FIELD-NUM
 SIMPLIFIED-ID-FIELD-NUM
 MEASURE-WORD-FIELD-NUM
 MEANING-FIELD-NUM
 CANGJIE5-FIELD-NUM
 STUDY-ORDER-FIELD-NUM
 OTHER-STORIES-FIELD-NUM
 TAGS-FIELD-NUM
)


(define IN-FILENAME "Flt. RTH Study.txt")
;; (define OUT-FILENAME "RTH-el-pleco.txt")


(enum
 CHARACTER-FIELD-NUM
 KEYWORD-FIELD-NUM
 KEYWORD-INFO-FIELD-NUM
 PRIMITIVE-FIELD-NUM
 PRIMITIVE-STORY-FIELD-NUM
 PINYIN-FIELD-NUM
 RTH-ID-FIELD-NUM
 STROKES-FIELD-NUM
 ELEMENTS-FIELD-NUM
 STORY-FIELD-NUM
 HINT-FIELD-NUM
 COMMENTS-FIELD-NUM
 SIMPLIFIED-ID-FIELD-NUM
 MEASURE-WORD-FIELD-NUM
 MEANING-FIELD-NUM
 CANGJIE5-FIELD-NUM
 STUDY-ORDER-FIELD-NUM
 OTHER-STORIES-FIELD-NUM
 TAGS-FIELD-NUM
)

;;; --- Reading the file
(define (make-reader [delimiter #\tab])
  (make-csv-reader-maker
   `((separator-chars              ,delimiter)
     (strip-leading-whitespace?  . #f)
     (strip-trailing-whitespace? . #f))))

(define (all-rows port)
  (define read-row ((make-reader) port))
  (for/list ([row (in-producer read-row '())])
      row))

(define (read-file infilename)
  (with-input-from-file infilename
    (lambda () (all-rows (current-input-port)))))


(define (read-RTH infilename)
  (read-file infilename))


;;; --- Writing

(define (write-RTH-csv outfilename data)
  (let ((printing-params (make-csv-printing-params
                          #:quotes-only-when-needed? #f)))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))


(define (write-RTH outfilename data)
  (let ((printing-params default-tsv-printing-params))
    (with-output-to-file outfilename
      (lambda () (display-table data (current-output-port) #:printing-params printing-params ))
      #:exists 'replace)))
