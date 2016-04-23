;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;                                        ;
(in-package :index-cedict)
(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))


#|

(defun read-file (filename)
  (with-open-file (infile filename :direction :input
                                   :external-format
                                              :utf-8)
    (loop for line = (read-line infile nil nil)
          while line
          ;; for i from 0 ;;below 50
          collect (split-sequence:split-sequence #\Space
                                                 (as-trim-string line)))))




(defun write-template (in-filename out-filename)
  (let ((subs (read-srt in-filename)))
    (with-open-file (f-out out-filename
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      (loop for sub in subs
            for start = (getf sub :time-start)
            do (write-line (concatenate 'string
                                        (format-time-ms start)
                                        "  ") f-out)))
    ))

 |#


(defparameter *compile-stories* t)

(defun trim-string (s)
  (string-trim '(#\Space #\Tab #\Newline) s))

(defun write-dict (data fname-out dictname)
  (with-open-file (f-out fname-out
                         :external-format :utf-8
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
    ;;(format f-out "~%")
    (loop for rec in data
          do (destructuring-bind  (&key rth-index trad simp keyword
                                        primitive story &allow-other-keys)
                 rec

               (when (zerop (length simp))
                 (setf simp trad))
               (unless primitive
                 (setf primitive ""))
               
               (format f-out
                       "~A ~A [xx5] /<span class='RTH'>~A:~A</span> <span class='keyword'>~A</span> ~A~A/~%"
                       trad simp
                       dictname
                       rth-index
                       keyword
                       (if (and *compile-stories* (plusp (length primitive)))
                           (format nil "/(Prim: ~A)" primitive)
                           "")
                       (if (and *compile-stories* (plusp (length story)))
                           (format nil "/~A" story)
                           ""))))))



(defun load-data (fname-in fields)
  (with-open-file (infile fname-in :direction :input
                                   :external-format
                                              :utf-8)
    (let ((data (loop for line = (read-line infile nil nil)
                      ;; repeat 100
                      while line
                      for seq = (split-sequence:split-sequence #\Tab line)
                      collect (loop for v in seq
                                    for k in fields
                                    collect k
                                    collect v))))
      data)))

(defun make-dict (fname-in fname-out fields dictname)
  (let ((data (load-data fname-in fields)))
      
    (write-dict data fname-out dictname)))



(defparameter +rth1fields+
  '(:rth-index :trad :simp :keyword :strokes :story :primitive
    :hint :reading :comments :connotation
    ))

(defun rth1-dict (fname-in fname-out)
  (make-dict fname-in fname-out +rth1fields+ "RTH1"))

(defparameter +rth2fields+
  '(:trad :keyword :keyword-info :primitive :reading 
    :rth-index :lesson :strokes :story :hint :comments :simp
    ))

(defun rth2-dict (fname-in fname-out)
  (make-dict fname-in fname-out +rth2fields+ "RTH2"))


(defparameter +rth-new-fields+
  '(:trad :keyword :keyword-info :primitive :reading
    :rth-index :lesson :strokes :story :hint :comments :simp
    ))

(defun rth-new-dict (fname-in fname-out)
  (make-dict fname-in fname-out +rth-new-fields+ "RTH"))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
              while pos)))


(defun make-pleco ()
  (let ((data1 (load-data "~/work/zhongwen-RTH/rth-cedict/RTH1notes.txt" +rth1fields+))
        (data2 (load-data "~/work/zhongwen-RTH/rth-cedict/RTH2notes.txt" +rth2fields+))
        (fname-out "~/work/zhongwen-RTH/rth-cedict/plecoRTH.txt"))
    
    (with-open-file (f-out fname-out
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      (labels ((replace-html (s)
                 (replace-all s "&nbsp;" " "))
               (write-data (data dictname)
                 (loop for rec in data
                       do (destructuring-bind  (&key rth-index trad simp keyword
                                                     primitive story &allow-other-keys)
                              rec

                            (setf trad (trim-string trad))
                            (setf simp (trim-string simp))
                          
                            (when (string= simp trad)
                              (setf simp ""))
                            ;; (unless (zerop (length simp))
                            ;;   (setf simp (format nil "[~A]" simp)))
                          
                            (unless primitive
                              (setf primitive ""))
                            (unless story
                              (setf story ""))

                            (setf trad (replace-html trad))
                            (setf simp (replace-html simp))
                            (setf keyword (replace-html keyword))
                          
                            (let ((headword trad))
                              (when (plusp (length simp))
                                (setf headword
                                      (format nil "~A[~A]" simp trad) ))
                              (format f-out
                                      "~A~C~C~C~A~C  ~A (~A)~%"
                                      headword #\Tab
                                      #\Tab ;; no pinyin
                                      #.(code-char #xEAB2) keyword #.(code-char #xEAB3)
                                      rth-index dictname
                                  
                                  
                                      ;; (if (and *compile-stories* (plusp (length primitive)))
                                      ;;     (format nil "/(Prim: ~A)" primitive)
                                      ;;     "")
                                      ;; (if (and *compile-stories* (plusp (length story)))
                                      ;;     (format nil "/~A" story)
                                      ;;     "")
                                      ))))))
        (write-data data1 "RTH1")
        (write-data data2 "RTH2"))
      
      ;;(format f-out "~%")
      )))


(defun make-tsv ()
  (let ((data1 (load-data "~/work/zhongwen-RTH/rth-cedict/RTH1notes.txt" +rth1fields+))
        (data2 (load-data "~/work/zhongwen-RTH/rth-cedict/RTH2notes.txt" +rth2fields+))
        (fname-out "~/work/zhongwen-RTH/rth-cedict/RTH.tsv"))
    
    (with-open-file (f-out fname-out
                           :external-format :utf-8
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
      (labels ((replace-html (s)
                 (replace-all s "&nbsp;" " "))
               (write-data (data dictname)
                 (loop for rec in data
                       do (destructuring-bind  (&key rth-index trad simp keyword
                                                     primitive story &allow-other-keys)
                              rec

                            (setf trad (trim-string trad))
                            (setf simp (trim-string simp))
                          
                            (when (string= simp trad)
                              (setf simp ""))
                            ;; (unless (zerop (length simp))
                            ;;   (setf simp (format nil "[~A]" simp)))
                          
                            (unless primitive
                              (setf primitive ""))
                            (unless story
                              (setf story ""))

                          
                            (setf trad (replace-html trad))
                            (setf simp (replace-html simp))
                            (setf keyword (replace-html keyword))
                          
                            (format f-out
                                    "~A~C~A~C~A~C~A~C~A~C~%"
                                    trad #\Tab simp #\Tab
                                    keyword #\Tab
                                    rth-index #\Tab dictname #\Tab
                                  
                                  
                                    ;; (if (and *compile-stories* (plusp (length primitive)))
                                    ;;     (format nil "/(Prim: ~A)" primitive)
                                    ;;     "")
                                    ;; (if (and *compile-stories* (plusp (length story)))
                                    ;;     (format nil "/~A" story)
                                    ;;     "")
                                    )))))
        (write-data data1 "RTH1")
        (write-data data2 "RTH2"))
      
      ;;(format f-out "~%")
      )))




#|
 (rth1-dict "~/work/zhongwen-RTH/rth-cedict/RTH1notes.txt"
         "~/work/zhongwen-RTH/rth-cedict/rth1dict.u8")

 (rth2-dict "~/work/zhongwen-RTH/rth-cedict/RTH2notes.txt"
         "~/work/zhongwen-RTH/rth-cedict/rth2dict.u8")

|#

#|
 (cl-user::load-asd "~/work/zhongwen-RTH/rth-cedict/index-cedict.asd")
|#
#|
 (rth-new-dict "~/work/zhongwen-RTH/rth-cedict/RTHplusGrades1-8.tsv"
         "~/work/zhongwen-RTH/rth-cedict/rthdict.u8")


|#
