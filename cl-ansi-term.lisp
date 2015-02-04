;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This file is part of cl-ansi-term.
;;;
;;; Copyright (c) 2015 Mark Karpov
;;;
;;; cl-ansi-term is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; cl-ansi-term is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(defpackage   :cl-ansi-term
  (:nicknames :term)
  (:use       #:common-lisp
              #:alexandria
              #:anaphora)
  (:export    #:*effects-enabled*
              #:*terminal-width*
              #:register-hook
              #:remove-hook
              #:update-style-sheet
              #:cat-print
              #:hr
              #:progress-bar
              #:u-list
              #:o-list
              #:table))

(in-package #:cl-ansi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                        Parameters and Constants                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *effects-enabled* t
  "If this variable is bound to non-NIL value, graphic rendition
effects (and other terminal-dependent effects) are enabled, otherwise they
are disabled.")

(defparameter *terminal-width* 80
  "Many functions use this value to output text nicely. Default value is
80. If you want to dynamically change this value, write and register
:BEFORE-PRINTING hook and reassign terminal width before printing takes
place.")

(defparameter *hooks* (make-hash-table)
  "This variable is bound to hash table that provides access to lists of
functions by given key. We use keywords as keys. Arguments for the functions
depend entirely on EVENT on which every function is called.")

(defparameter *style-sheet* (make-hash-table)
  "This hash table contains strings for various styles of terminal output,
defined with UPDATE-STYLE-SHEET.")

(defvar *style* :default
  "This variable is bound to currently set style. Styles are set with
SET-STYLE function.")

(defparameter *coloration* nil
  "Alist where CARs are indexes at which to insert ANSI escape sequences to
change graphical rendition and CDRs are keywords that denote style of the
rendition. This special variable can be used to affect PRINT-PARTIALLY.")

(defparameter +foreground-colors+
  '((:default  . 39)
    (:black    . 30)
    (:red      . 31)
    (:green    . 32)
    (:yellow   . 33)
    (:blue     . 34)
    (:magneta  . 35)
    (:cyan     . 36)
    (:white    . 37)
    (:black*   . 90)
    (:red*     . 91)
    (:green*   . 92)
    (:yellow*  . 93)
    (:blue*    . 94)
    (:magneta* . 95)
    (:cyan*    . 96)
    (:white*   . 97))
  "These are basic foreground terminal colors. Colors that are denoted by
keywords ending with asterisk are not in ANSI standard (high intensity
variants of 8 basic colors).")

(defparameter +background-colors+
  '((:b-default  . 49)
    (:b-black    . 40)
    (:b-red      . 41)
    (:b-green    . 42)
    (:b-yellow   . 43)
    (:b-blue     . 44)
    (:b-magneta  . 45)
    (:b-cyan     . 46)
    (:b-white    . 47)
    (:b-black*   . 100)
    (:b-red*     . 101)
    (:b-green*   . 102)
    (:b-yellow*  . 103)
    (:b-blue*    . 104)
    (:b-magneta* . 105)
    (:b-cyan*    . 106)
    (:b-white*   . 107))
  "These are basic background terminal colors. Colors that are denoted by
keywords ending with asterisk are not in ANSI standard (high intensity
variants of 8 basic colors).")

(defparameter +effects+
  '((:bold      . 1)
    (:faint     . 2)
    (:italic    . 3)
    (:underline . 4)
    (:blink     . 5)
    (:bad-blink . 6)
    (:inverse   . 7)
    (:hide      . 8)
    (:fraktur   . 20)
    (:normal    . 22)
    (:framed    . 51)
    (:encircled . 52)
    (:overlined . 53))
  "All supported rendition effects. Some of them are hardly ever
supported.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Hooks                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-hook (event function)
  "Register a hook. When predefined EVENT occurs FUNCTION will be
called. You can register many functions to call on the same event.

Acceptable values of EVENT:

:BEFORE-PRINTING - FUNCTION is invoked just before printing takes place, no
argument is passed to the function
:AFTER-PRINTING - FUNCTION is invoked after printing, no argument is passed
to the function
:ON-STYLE-CHANGE - FUNCTION is invoked before style changing escape sequence
in printed. One argument is passed to FUNCTION, name of the style, which is
a keyword."
  (push function (gethash event *hooks*))
  nil)

(defun remove-hook (event)
  "Remove all registered functions that are called on EVENT. Returns T if
there were any functions associated with EVENT and NIL otherwise."
  (remhash event *hooks*))

(defun perform-hook (event &rest args)
  "Execute functions corresponding to given EVENT. We use this function to
perform the hooks, so it's for internal use. Return T if there is at least
one function associated with EVENT and NIL otherwise."
  (awhen (gethash event *hooks*)
    (dolist (x it t)
      (apply x args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                   Graphic Rendition and Style Sheet                    ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansi-escape-seq (&optional tokens)
  "Convert list of rendition tokens into ANSI escape sequence that will
select appropriate parameters of rendition if 'printed' on ANSI-compatible
terminal. If TOKENS is empty, escape sequence that resets all rendition
parameters will be returned."
  (if (null tokens)
      (format nil "~c[0m" #\escape)
      (flet ((select-token (options default)
               (or (car (intersection tokens
                                      (mapcar #'car options)))
                   default)))
        (let ((foreground-color (select-token +foreground-colors+ :default))
              (background-color (select-token +background-colors+ :b-default))
              (effect           (select-token +effects+           :normal)))
          (format nil "~c[~d;~d;~dm" #\escape
                  (cdr (assoc foreground-color +foreground-colors+))
                  (cdr (assoc background-color +background-colors+))
                  (cdr (assoc effect           +effects+)))))))

(defun update-style-sheet (alist)
  "Update style sheet used by the application. Every item of ALIST must be a
list with CAR denoting name of style sheet entry and CDR representing
collection of tokens that define terminal rendition. Tokens can represent
various things: foreground color, background color, and effects. Every type
of token has its default value, so you can omit some tokens. However, if
there are more than one token of the same type (for example :RED and :GREEN
- both tokens represent foreground color), result is unpredictable and
depends on internal workings of Common Lisp implementation used. You cannot
redefine :DEFAULT style, it's always represent default parameters of
rendition."
  (dolist (entry alist)
    (destructuring-bind (style . tokens) entry
      (if tokens
          (setf (gethash style *style-sheet*) (ansi-escape-seq tokens))
          (remhash style *style-sheet*))))
  (setf (gethash :default *style-sheet*) (ansi-escape-seq))
  (values))

(declaim (inline effects-p))

(defun effects-p (stream)
  "Evaluates to T if STREAM has support for the effects and
*EFFECTS-ENABLED* is not NIL."
  (and *effects-enabled*
       (interactive-stream-p stream)))

(defun set-style (style &optional (stream *standard-output*))
  "Sets terminal rendition according to defined STYLE. It does nothing if
*EFFECTS-ENABLED* is NIL or output stream is not interactive (e.g.
redirected to a file)."
  (awhen (and (effects-p stream)
              (gethash style *style-sheet*))
    (perform-hook :on-style-change style)
    (princ it stream)
    (setf *style* style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                               Utilities                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-partially (text start end &optional (stream *standard-output*))
  "Partially print given TEXT starting from START character until END
character is reached. Output will be colorized if *COLORATION* is bound to
alist that describes how to colorize the output, see *COLORATION*. All
output goes to STREAM."
  (do ((i start (1+ i)))
      ((= i end))
    (when (and *coloration*
               (= i (caar *coloration*)))
      (set-style (cdr (pop *coloration*)) stream))
    (princ (char text i) stream)))

(defun print-white-space (width &optional (stream *standard-output*))
  "Print WIDTH white-spaces to STREAM."
  (dotimes (i width)
    (princ #\space stream)))

(defmacro with-reasonable-width (var &body body)
  "Rebind variable VAR, correcting its value in BODY. If VAR is not a
positive number, *TERMINAL-WIDTH* will be added to it to get positive
value that will be used."
  `(let ((,var (+ ,var (if (plusp ,var) 0 *terminal-width*))))
     ,@body))

(defun align-object (width align &optional (stream *standard-output*))
  "Print white-space to STREAM so object occupying WIDTH columns will be
aligned according to ALIGN if printed immediately after the white space."
  (print-white-space
   (case align
     (:right  (- *terminal-width* width))
     (:center (floor (- *terminal-width* width)
                     2))
     (t       0))
   stream))

(defun string* (object)
  "Converts printable object OBJECT to its aesthetic string representation."
  (format nil "~a" object))

(defun print-filler (filler width style &optional (stream *standard-output*))
  "Print WIDTH symbols of FILLER to STREAM. Use STYLE for graphic
rendition."
  (multiple-value-bind (rough rest)
      (floor width (length (string* filler)))
    (set-style style)
    (dotimes (i rough)
      (princ filler stream))
    (print-partially filler 0 rest stream)
    (set-style :default)))

(defun print-words (objects
                    &key
                      (base-style  :default)
                      (margin      0)
                      (fill-column 0)
                      (align       :left)
                      (stream      *standard-output*))
  "Print concatenation of OBJECTS using FILL-COLUMN so that line breaks
don't happen inside words, only between them. OBJECTS must be a list
designator. It can consist of printable objects and lists where CAR is a
printable object and CADR is a keyword that denotes style of the string
designator. Unspecified styles default to BASE-STYLE. MARGIN is not applied
for the first line. If FILL-COLUMN is not a positive number,
*TERMINAL-WIDTH* will be added to it to get positive FILL-COLUMN. Output can
be aligned with ALIGN parameter. Output goes to STREAM."
  (with-reasonable-width fill-column
    (let ((fill-column (- fill-column margin))
          (text (make-array 0
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t))
          (*coloration* nil)
          (len 0))
      (with-output-to-string (stream text)
        (flet ((proc-object (obj style)
                 (push (cons len style) *coloration*)
                 (incf len (length (string* obj)))
                 (princ obj stream)))
          (dolist (object (ensure-list objects))
            (if (consp object)
                (destructuring-bind (printable style)
                    object
                  (proc-object printable style))
                (proc-object object base-style)))))
      (setf *coloration* (nreverse *coloration*))
      (do* ((start 0)
            (end   (min (+ start fill-column) len)
                   (min (+ start fill-column) len)))
           ((= start end))
        (destructuring-bind (break-pos . new-start)
            (or (awhen (position #\newline text
                                 :start start
                                 :end   end)
                  (cons it (1+ it)))
                (when (< (- end start)
                         fill-column)
                  (cons end end))
                (awhen (position #\space   text
                                 :start    start
                                 :end      end
                                 :from-end t)
                  (cons it (1+ it)))
                (cons end end))
          (when (plusp start)
            (print-white-space margin stream))
          (align-object (+ (- break-pos start) margin)
                        align
                        stream)
          (print-partially text start break-pos stream)
          (push (cons new-start *style*) *coloration*)
          (set-style :default stream)
          (terpri stream)
          (setf start new-start))))))

(defun cat-print (objects
                  &key
                    (base-style  :default)
                    (margin      0)
                    (fill-column 0)
                    (align       :left)
                    (stream      *standard-output*))
  "Concatenate OBJECTS and print them. OBJECTS must be a list designator
that consists of printable objects and lists where CAR is a printable object
and CADR is a keyword that denotes style of the object. Unspecified styles
default to BASE-STYLE. MARGIN, FILL-COLUMN, and ALIGN control corresponding
parameters of output. Valid values for ALIGN are :LEFT (default), :CENTER,
and :RIGHT. Output goes to STREAM."
  (perform-hook :before-printing)
  (print-white-space margin stream)
  (print-words objects
               :base-style base-style
               :margin margin
               :fill-column fill-column
               :align align
               :stream stream)
  (finish-output stream)
  (perform-hook :after-printing)
  (values))

(defun hr (&key
             (filler #\-)
             (style  :default)
             (width  0)
             (align  :left)
             (stream *standard-output*))
  "Print a horizontal line. Characters in the line are created by repeating
given FILLER until WIDTH characters accumulated. If WIDTH is not a positive
number, *TERMINAL-WIDTH* will be added to it to get positive WIDTH. STYLE
controls graphic rendition. ALIGN should be a keyword: :LEFT, :RIGHT,
or :CENTER. Output goes to STREAM."
  (perform-hook :before-printing)
  (with-reasonable-width width
    (align-object width align)
    (print-filler filler width style stream))
  (terpri stream)
  (finish-output stream)
  (perform-hook :after-printing)
  (values))

(defun progress-bar (label progress
                     &key
                       (margin      0)
                       (label-style :default)
                       (filler      #\#)
                       (bar-style   :default)
                       (num-style   :default)
                       (bar-width   -40)
                       (stream      *standard-output*))
  "Print a progress bar. If PROGRESS is less than 100, move cursor to the
beginning of current line, so next invocation of PROGRESS-BAR will rewrite
it. This function doesn't print anything if PROGRESS is less than 100 and
output stream is not interactive or *EFFECTS-ENABLED* is NIL. Insert MARGIN
spaces, then LABEL (style for the label is set with LABEL-STYLE). Size of
progress bar is set by BAR-WIDTH. If BAR-WIDTH is not a positive number,
*TERMINAL-WIDTH* will be added to it to get positive BAR-WIDTH. BAR-STYLE is
the style that will be used for the bar itself, while NUM-STYLE will be used
for number of percents and some additional elements. Output goes to STREAM."
  (unless (and (< progress 100)
               (not (effects-p stream)))
    (perform-hook :before-printing)
    (with-reasonable-width bar-width
      (let* ((total-cells  (- bar-width 8))
             (filled-cells (floor (/ (* total-cells progress) 100)))
             (empty-cells  (- total-cells filled-cells)))
        (print-white-space margin stream)
        (set-style label-style stream)
        (princ label stream)
        (set-style :default)
        (print-white-space
         (- *terminal-width*
            bar-width
            margin
            (length label))
         stream)
        (set-style num-style stream)
        (princ #\[ stream)
        (print-filler filler
                      filled-cells
                      bar-style
                      stream)
        (print-white-space empty-cells stream)))
    (set-style num-style stream)
    (princ #\] stream)
    (set-style :default stream)
    (print-white-space 1 stream)
    (set-style num-style stream)
    (format stream "~3d" progress)
    (set-style :default stream)
    (print-white-space 1 stream)
    (set-style num-style stream)
    (princ #\% stream)
    (set-style :default stream)
    (if (< progress 100)
        (format stream "~c[0G" #\escape)
        (terpri stream))
    (finish-output stream)
    (perform-hook :after-printing))
  (values))

(defun u-list (tree
               &key
                 (bullet       "*-~^")
                 (mark-suffix  #\*)
                 (bullet-style :default)
                 (item-style   :default)
                 (mark-style   :default)
                 (margin       0)
                 (level-margin 2)
                 (fill-column  0)
                 (stream       *standard-output*))
  "Print an unordered list according to TREE. If we consider TREE a list,
every element must be either a printable object to print as a list item or a
list where CAR is the list item and CDR is sublist of the item. BULLET must
be a string designator, it will be converted to string if needed and its
characters will be used as bullets: zeroth character will be the bullet for
top level of the list, first character is the bullet for sublist, etc. If
there are more levels of nesting than characters in the string, it will be
cycled. BULLET-STYLE is used for bullets. It can be also a list, in this
case it's possible to specify different styles for different levels of
nesting. ITEM-STYLE is used to render the list items. MARK-STYLE is used for
items that end with MARK-SUFFIX (it can be any printable
object). LEVEL-MARGIN must be a positive integer that specifies how to
increase margin for every level of nesting, you can also use plain
MARGIN. FILL-COLUMN is used to split long items, if it's not a positive
number, *TERMINAL-WIDTH* will be added to it to get positive
FILL-COLUMN. Output goes to STREAM."
  (let ((bullet       (apply #'circular-list
                             (coerce (string bullet) 'list)))
        (bullet-style (apply #'circular-list
                             (ensure-cons bullet-style)))
        (mark-suffix  (string* mark-suffix)))
    (labels ((print-item (level item bullet bullet-style)
               (let ((margin (+ margin (* level level-margin))))
                 (print-white-space margin stream)
                 (set-style (car bullet-style) stream)
                 (princ (car bullet) stream)
                 (set-style :default stream)
                 (print-white-space (1- level-margin) stream)
                 (let* ((item (ensure-cons item))
                        (words (string* (car item))))
                   (print-words words
                                :base-style  (if (ends-with-subseq mark-suffix
                                                                   words)
                                                 mark-style
                                                 item-style)
                                :margin      (+ margin level-margin)
                                :fill-column fill-column
                                :stream stream)
                   (dolist (subitem (cdr item))
                     (print-item (1+ level)
                                 subitem
                                 (cdr bullet)
                                 (cdr bullet-style)))))))
      (perform-hook :before-printing)
      (dolist (item tree)
        (print-item 0 item bullet bullet-style))
      (finish-output stream)
      (perform-hook :after-printing)
      (values))))

(defun o-list (tree
               &key
                 (index        :arabic)
                 (delimiter    #\.)
                 (mark-suffix  #\*)
                 (index-style  :default)
                 (item-style   :default)
                 (mark-style   :default)
                 (margin       0)
                 (level-margin 3)
                 (fill-column  0)
                 (stream       *standard-output*))
  "Print ordered list according to TREE. If we consider TREE a list,
every element must be either a printable object to print as a list item or a
list where CAR is list item and CDR is sublist of the item. INDEX must be a
list designator, its elements should be keywords that denote how to
represent numeration. Acceptable values are:

:ARABIC  - indexes will be printed as arabic numerals (default value)
:ROMAN   - indexes will be printed as roman numerals
:LETTER  - indexes will be printed as letters of Latin alphabet
:CAPITAL - the same as :LETTER, but capital letters are used

If there are more levels of nesting than elements in the list, it will be
cycled. The same applies to DELIMITER, which must be a string
designator. INDEX-STYLE is used for indexes. It can be also list, in this
case it's possible to specify different styles for different levels of
nesting. ITEM-STYLE is used to render the list items. MARK-STYLE is used for
items that end with MARK-SUFFIX (it can be any printable
object). LEVEL-MARGIN must be a positive integer that specifies how to
increase margin for every level of nesting, you can also use plain
MARGIN. FILL-COLUMN is used to split long items, if it's not a positive
number, *TERMINAL-OUTPUT* will be added to it to get positive
FILL-COLUMN. Output goes to STREAM."
  (let ((index       (apply #'circular-list
                            (ensure-cons index)))
        (index-style (apply #'circular-list
                            (ensure-cons index-style)))
        (delimiter   (apply #'circular-list
                            (coerce (string delimiter) 'list)))
        (mark-suffix (string* mark-suffix)))
    (labels ((print-item (level i item index index-style delimiter)
               (let ((margin (+ margin (* level level-margin)))
                     (image  (case (car index)
                               (:roman   (format nil "~@r~c"
                                                 (1+ i)
                                                 (car delimiter)))
                               (:letter  (format nil "~c~c"
                                                 (code-char (+ 97 i))
                                                 (car delimiter)))
                               (:capital (format nil "~c~c"
                                                 (code-char (+ 65 i))
                                                 (car delimiter)))
                               (t        (format nil "~d~c"
                                                 (1+ i)
                                                 (car delimiter))))))
                 (print-white-space margin stream)
                 (set-style (car index-style) stream)
                 (princ image stream)
                 (set-style :default stream)
                 (print-white-space (- level-margin (length image)) stream)
                 (set-style item-style stream)
                 (let* ((item (ensure-cons item))
                        (words (string* (car item))))
                   (print-words words
                                :base-style  (if (ends-with-subseq mark-suffix
                                                                   words)
                                                 mark-style
                                                 item-style)
                                :margin      (+ margin level-margin)
                                :fill-column fill-column
                                :stream      stream)
                   (do ((subitems (cdr item) (cdr subitems))
                        (i 0 (1+ i)))
                       ((null subitems))
                     (print-item (1+ level)
                                 i
                                 (car subitems)
                                 (cdr index)
                                 (cdr index-style)
                                 (cdr delimiter)))))))
      (perform-hook :before-printing)
      (do ((items tree (cdr items))
           (i 0 (1+ i)))
          ((null items))
        (print-item 0 i (car items) index index-style delimiter))
      (finish-output stream)
      (perform-hook :after-printing)
      (values))))

(defun table (objects
              &key
                (mark-suffix  #\*)
                (border-chars "-|+")
                (border-style :default)
                (header-style :default)
                (cell-style   :default)
                (mark-style   :default)
                (col-header   nil)
                (margin       0)
                (column-width 10)
                (align        :left)
                (stream       *standard-output*))
  "Print a table filling cells with OBJECTS. OBJECTS must be a list of list
designators. If BORDER-STYLE is NIL, no border will be printed, otherwise
BORDER-STYLE is expected to be a keyword that denotes style in which borders
of the table should be printed. HEADER-STYLE will be applied to the first
row of the table (also to the first column if COL-HEADER is not NIL) and
CELL-STYLE will be applied to all other rows. Objects that end with
MARK-SUFFIX will be printed using MARK-STYLE. MARGIN, COLUMN-WIDTH, and
ALIGN can also be specified. Valid values of ALIGN are: :LEFT (default
value), :CENTER, and :RIGHT. Output goes to STREAM."
  (check-type column-width (integer 4))
  (perform-hook :before-printing stream)
  (let* ((objects (mapcar #'ensure-cons objects))
         (columns (length (extremum objects #'> :key #'length)))
         (width (1+ (* columns column-width)))
         (border-chars (string border-chars))
         (mark-suffix (string mark-suffix))
         (row-index 0))
    (labels ((align ()
               (print-white-space margin stream)
               (align-object (+ width margin) align stream))
             (h-border ()
               (when border-style
                 (align)
                 (set-style border-style stream)
                 (dotimes (i width)
                   (princ (char border-chars
                                (if (zerop (mod i column-width)) 2 0))
                          stream))
                 (set-style :default stream)
                 (terpri stream)))
             (v-border ()
               (when border-style
                 (set-style border-style stream)
                 (princ (char border-chars 1))
                 (set-style :default stream)))
             (print-row (index items)
               (align)
               (let ((i 0))
                 (dolist (cell items)
                   (let ((cell (string* cell)))
                     (v-border)
                     (set-style
                      (cond ((ends-with-subseq mark-suffix cell) mark-style)
                            ((zerop index) header-style)
                            ((and col-header (zerop i)) header-style)
                            (t cell-style))
                      stream)
                     (princ cell stream)
                     (set-style :default stream)
                     (print-white-space (- column-width
                                           (length cell)
                                           (if border-style 1 0))
                                        stream)
                     (incf i)))
                 (print-white-space (* (- columns i) column-width) stream)
                 (v-border)
                 (terpri))))
      (dolist (row objects)
        (h-border)
        (print-row row-index row)
        (incf row-index))
      (h-border)))
  (finish-output stream)
  (perform-hook :after-printing stream)
  (values))
