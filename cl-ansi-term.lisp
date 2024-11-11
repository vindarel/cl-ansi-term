;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This file is part of cl-ansi-term.
;;;
;;; Copyright © 2015–2018 Mark Karpov
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
              #:print
              #:hr
              #:vspace
              #:banner
              #:progress-bar
              #:u-list
              #:o-list
              #:table
              #:vtable
              #:plist-vtable
              #:plist-table
              #:plists-table
              #:plists-vtable)
  (:shadow    #:print))

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
  "Many functions use this value to output text nicely. The default value is 80.
If you want to dynamically change this variable, write and register
:BEFORE-PRINTING hook and reassign terminal width before printing takes
place.")

(defparameter *column-width* 10 "The maximum table cells' width.")

(defparameter *hooks* (make-hash-table)
  "This variable is bound to a hash table that provides access to lists of
functions by given key. We use keywords as keys. Arguments for the functions
depend entirely on EVENT on which every function is called.")

(defparameter *style-sheet* (make-hash-table)
  "This hash table contains strings for various styles of terminal output,
defined with `update-style-sheet'.")

(defvar *style* :default
  "This variable is bound to currently set style. Styles are set with
`set-style' function.")

(defparameter *coloration* nil
  "Alist where CARs are indexes at which to insert ANSI escape sequences to
change graphical rendition and CDRs are keywords that denote style of the
rendition. This special variable can be used to affect `print-partially'.")

(defparameter +foreground-colors+
  '((:default  . 39)
    (:black    . 30)
    (:red      . 31)
    (:green    . 32)
    (:yellow   . 33)
    (:blue     . 34)
    (:magenta  . 35)
    (:cyan     . 36)
    (:white    . 37)
    (:black*   . 90)
    (:red*     . 91)
    (:green*   . 92)
    (:yellow*  . 93)
    (:blue*    . 94)
    (:magenta* . 95)
    (:cyan*    . 96)
    (:white*   . 97))
  "These are the basic foreground terminal colors. Colors that are denoted
by keywords ending with an asterisk are not in the ANSI standard (high
intensity variants of 8 basic colors).")

(defparameter +background-colors+
  '((:b-default  . 49)
    (:b-black    . 40)
    (:b-red      . 41)
    (:b-green    . 42)
    (:b-yellow   . 43)
    (:b-blue     . 44)
    (:b-magenta  . 45)
    (:b-cyan     . 46)
    (:b-white    . 47)
    (:b-black*   . 100)
    (:b-red*     . 101)
    (:b-green*   . 102)
    (:b-yellow*  . 103)
    (:b-blue*    . 104)
    (:b-magenta* . 105)
    (:b-cyan*    . 106)
    (:b-white*   . 107))
  "These are the basic background terminal colors. Colors that are denoted
by keywords ending with an asterisk are not in ANSI standard (high intensity
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
  "All supported rendition effects. Some of them are hardly ever supported
by real-world terminals.")

(defvar *prefer-plists-in-tables* nil
  "If non-nil, if the TABLE function can't clearly distinguish
  between a list of plists and a list of regular lists,
  it will give precedence to displaying the data as plists.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Hooks                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-hook (event function)
  "Register a hook. When predefined EVENT occurs FUNCTION will be called.
You can register many functions to call on the same event.

Acceptable values of EVENT:

:BEFORE-PRINTING—FUNCTION is invoked just before printing takes place, no
argument is passed to the function
:AFTER-PRINTING—FUNCTION is invoked after printing, no argument is passed to
the function
:ON-STYLE-CHANGE—FUNCTION is invoked before style changing escape sequence
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
  "Convert list of rendition tokens into an ANSI escape sequence that will
select appropriate parameters of rendition if “printed” on an
ANSI-compatible terminal. If TOKENS is empty, escape sequence that resets
all rendition parameters will be returned."
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
there are more than one token of the same type (for example :RED
and :GREEN—both tokens represent foreground color), result is unpredictable
and depends on internal workings of Common Lisp implementation used. You
cannot redefine :DEFAULT style, it always represents default parameters of
rendition."
  (dolist (entry alist)
    (destructuring-bind (style . tokens) entry
      (if tokens
          (setf (gethash style *style-sheet*) (ansi-escape-seq tokens))
          (remhash style *style-sheet*))))
  (setf (gethash :default *style-sheet*) (ansi-escape-seq))
  nil)

(declaim (inline effects-p))

(defun effects-p (stream)
  "Evaluates to T if STREAM has support for the effects and
`*effects-enabled*' is not NIL."
  (and *effects-enabled*
       (interactive-stream-p stream)))

(defun set-style (style &optional (stream *standard-output*))
  "Sets terminal rendition according to defined STYLE. It does nothing if
`*effects-enabled*' is NIL or output stream is not interactive (e.g.
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
character is reached. Output will be colorized if `*coloration*' is bound to
alist that describes how to colorize the output, see `*coloration*'. All
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
positive number, `*terminal-width*' will be added to it to get positive
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

(defun largest-elt (lines)
  "Return the largest element of the given strings."
  (extremum lines #'> :key #'length))

(defun largest-length (lines)
  "Return the largest length of the given strings."
  (length (largest-elt lines)))
#+nil
(assert (equalp 8 (largest-length '("rst" "ldvvvvvv" nil))))

(defun ensure-circular-list (object)
  (apply #'circular-list
         (ensure-cons object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                               Functions                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
`*terminal-width*' will be added to it to get positive FILL-COLUMN. Output
can be aligned with ALIGN parameter. Output goes to STREAM."
  (with-reasonable-width fill-column
    (let ((fill-column (- fill-column margin))
          (text (make-array 0
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t))
          *coloration*
          (len 0))
      (with-output-to-string (stream text)
        (flet ((proc-object (obj style)
                 (let ((obj (string* obj)))
                   (unless (emptyp obj)
                     (push (cons len style) *coloration*)
                     (incf len (length (string* obj)))
                     (princ obj stream)))))
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
          (do () ((or (null *coloration*)
                      (<= new-start (caar *coloration*))))
            (set-style (cdr (pop *coloration*)) stream))
          (when (or (null *coloration*)
                    (< new-start (caar *coloration*)))
            (push (cons new-start *style*) *coloration*))
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
  nil)

(defun parse-control-string (string args)
  "Parse control string STRING according to the format described in
documentation for PRINT function. Return a list, suitable for passing to
`cat-print'."
  (labels ((positions (char str &optional (start 0) acc)
             (aif (position char str :start start :test #'char=)
                  (positions char str (1+ it) (cons it acc))
                  (nreverse acc)))
           (worker (opened closed &optional pending acc)
             (cond ((null closed) (nreverse acc))
                   ((and opened (< (car opened) (car closed)))
                    (worker (cdr opened)
                            closed
                            (cons (car opened) pending)
                            acc))
                   (t (worker opened
                              (cdr closed)
                              (cdr pending)
                              (cons (cons (car pending)
                                          (car closed))
                                    acc)))))
           (form-pairs (open close str)
             (worker (positions open  str)
                     (positions close str)))
           (prepare (str start end)
             (with-output-to-string (stream)
               (dotimes (i (- end start))
                 (let ((char (char str (+ start i))))
                   (princ (if (char= char #\~)
                              (format nil "~a" (pop args))
                              char)
                          stream))))))
    (let ((brackets (form-pairs #\[ #\] string))
          (parens (form-pairs #\( #\) string))
          (i 0)
          result)
      (dolist (item (mapcan (lambda (b)
                              (destructuring-bind (bs . be) b
                                (awhen (find-if (lambda (x)
                                                  (= (car x) (1+ be)))
                                                parens)
                                  (destructuring-bind (ps . pe) it
                                    (list (list (cons (1+ bs) be)
                                                (cons (1+ ps) pe)))))))
                            brackets))
        (destructuring-bind ((bs . be) (ps . pe)) item
          (when (< i (1- bs))
            (push (prepare string i (1- bs)) result))
          (push (list (prepare string bs be)
                      (intern (string-upcase (subseq string ps pe))
                              "KEYWORD"))
                result)
          (setf i (1+ pe))))
      (when (< i (length string))
        (push (prepare string i (length string)) result))
      (nreverse result))))

(defun print (control-string
              &key
                args
                (base-style  :default)
                (margin      0)
                (fill-column 0)
                (align       :left)
                (stream      *standard-output*))
  "Insert arguments from ARGS (list designator) into CONTROL-STRING
substituting tildes. Any region of text in CONTROL-STRING can be printed in
specified style following this pattern: [text](NAME-OF-STYLE). Where
NAME-OF-STYLE is downcased name of symbol (keyword) in style sheet. Style of
the rest of the output defaults to BASE-STYLE. MARGIN, FILL-COLUMN, and
ALIGN control corresponding parameters of output. Valid values for ALIGN
are :LEFT (default), :CENTER, and :RIGHT. Output goes to STREAM."
  (cat-print (parse-control-string control-string
                                   (ensure-list args))
             :base-style  base-style
             :margin      margin
             :fill-column fill-column
             :align       align
             :stream      stream))

(defun hr (&key
             (filler #\-)
             (style  :default)
             (width  0)
             (align  :left)
             (stream *standard-output*))
  "Print a horizontal line. Characters in the line are created by repeating
given FILLER until WIDTH characters accumulated. If WIDTH is not a positive
number, `*terminal-width*' will be added to it to get positive WIDTH. STYLE
controls graphic rendition. ALIGN should be a keyword: :LEFT, :RIGHT,
or :CENTER. Output goes to STREAM."
  (perform-hook :before-printing)
  (with-reasonable-width width
    (align-object width align)
    (print-filler filler width style stream))
  (terpri stream)
  (finish-output stream)
  (perform-hook :after-printing)
  nil)

(defun vspace (&key
                 (stream *standard-output*)
                 (space 3))
  "Print vertical space, aka a SPACE amount of newlines,
  to STREAM (standard output by default).

  Hooks are performed before and after printing."
  (perform-hook :before-printing)
  (format stream "~v&" space)
  (terpri stream)
  (finish-output stream)
  (perform-hook :after-printing))

(defun banner (title &key
                       (stream *standard-output*)
                       (style :default)
                       (width 0)
                       (space 1)
                       (left-space 5)
                       (align :left)
                       (filler #\-))
  "Print TITLE in-between two horizontal spaces (hr), with vertical space before and after.

  SPACE controls how many blank lines are added before and after the title,
  LEFT-SPACE helps to center the title a bit (defaults to 5)."
  (perform-hook :before-printing)
  (with-reasonable-width width
      (align-object width align)
    (vspace :space space)
    (print-filler filler width style stream)
    (format stream "~&~a" (make-string left-space :initial-element #\Space))
    (cat-print title)
    (print-filler filler width style stream)
    (vspace :space space))
  (terpri stream)
  (finish-output stream)
  (perform-hook :after-printing))

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
beginning of current line, so next invocation of `progress-bar' will rewrite
it. This function doesn't print anything if PROGRESS is less than 100 and
output stream is not interactive or `*effects-enabled*' is NIL. Insert
MARGIN spaces, then LABEL (style for the label is set with LABEL-STYLE).
Size of progress bar is set by BAR-WIDTH. If BAR-WIDTH is not a positive
number, `*terminal-width*' will be added to it to get positive BAR-WIDTH.
BAR-STYLE is the style that will be used for the bar itself, while NUM-STYLE
will be used for number of percents and some additional elements. Output
goes to STREAM."
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
  nil)

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
list where CAR is the list item and CDR is sublist of the item.

Example:

(term:u-list '((:one one-a (:one-b :one-b-1 :one-b-2)) :two))

* ONE
  - ONE-A
  - ONE-B
    ~ ONE-B-1
    ~ ONE-B-2
* TWO

BULLET is a string. Each character will be used, each time in a row,
as the list bullet. They can be cycled over.

Example:

(term:u-list '((:one one-a (:one-b :one-b-1 :one-b-2)) :two)
             :bullet #\+)
+ ONE
  + ONE-A
  + ONE-B
    + ONE-B-1
    + ONE-B-2
+ TWO

(term:u-list '((:one one-a (:one-b :one-b-1 :one-b-2)) :two)
                   :bullet \"+-\")
+ ONE
  - ONE-A
  - ONE-B
    + ONE-B-1
    + ONE-B-2
+ TWO


BULLET-STYLE is used for bullets. It can be also a list, in this
case it's possible to specify different styles for different levels of
nesting.

ITEM-STYLE is used to render the list items. MARK-STYLE is used for
items that end with MARK-SUFFIX (it can be any printable object).

LEVEL-MARGIN must be a positive integer that specifies how to increase
margin for every level of nesting, you can also use plain MARGIN.

FILL-COLUMN is used to split long items, if it's not a positive number,
`*terminal-width*' will be added to it to get positive FILL-COLUMN.

Output goes to STREAM."
  (let ((bullet (ensure-circular-list
                 (coerce (string bullet) 'list)))
        (bullet-style (ensure-circular-list bullet-style) )
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
      nil)))

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
  "Print an ordered list according to TREE. If we consider TREE a list,
every element must be either a printable object to print as a list item or a
list where CAR is list item and CDR is sublist of the item.

Example:

(term:o-list '((:one one-a (:one-b :one-b-1 :one-b-2)) :two))
1. ONE
   1. ONE-A
   2. ONE-B
      1. ONE-B-1
      2. ONE-B-2
2. TWO

INDEX must be a
list designator, its elements should be keywords that denote how to
represent numeration. Acceptable values are:

:ARABIC—indexes will be printed as arabic numerals (default value)
:ROMAN—indexes will be printed as roman numerals
:LETTER—indexes will be printed as letters of Latin alphabet
:CAPITAL—the same as :LETTER, but capital letters are used

If there are more levels of nesting than elements in the list, it will be
cycled. The same applies to DELIMITER, which must be a string designator.

INDEX-STYLE is used for indexes. It can be also list, in this case it's
possible to specify different styles for different levels of nesting.

ITEM-STYLE is used to render the list items.

MARK-STYLE is used for items
that end with MARK-SUFFIX (it can be any printable object). LEVEL-MARGIN
must be a positive integer that specifies how to increase margin for every
level of nesting, you can also use plain MARGIN.

FILL-COLUMN is used to
split long items, if it's not a positive number, `*terminal-output*' will be
added to it to get positive FILL-COLUMN.

Output goes to STREAM."
  (let ((index       (ensure-circular-list index))
        (index-style (ensure-circular-list index-style))
        (delimiter   (ensure-circular-list
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
      nil)))

;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;

(defun table-dispatch (one-or-many-objects
                       &rest keys
                       &key
                         (plists-p *prefer-plists-in-tables*)
                          &ALLOW-OTHER-KEYS
                         )
  "Call the appropriate TABLE-* function for the type of objects.

  ONE-OR-MANY-OBJECTS can be:

  - a singe plist
  - a singe hash-table
  - a list of hash-tables
  - a list of property lists: in that case, we can not be sure
   that the user manipulates a list of plists or a list of regular lists with
   symbols and keywords.
   Set the PLISTS-P key argument to T or call the PLISTS-TABLE function directly.
  - a list of lists.
  "
  (remf keys :plists-p) ; either add the key arg to all functions, either delete it. Destructive.
  (cond
    ;; One HT.
    ((hash-table-p one-or-many-objects)
     (apply #'ht-table one-or-many-objects keys))
    ;; A list of HTs.
    ((and (consp one-or-many-objects)
          (hash-table-p (first one-or-many-objects)))
     (apply #'hts-table one-or-many-objects keys))
    ;; One plist.
    ((property-list-p one-or-many-objects)
     (apply #'plist-table one-or-many-objects keys))
    ;; Can be distinguish between a list of plists and a list of lists?
    ;; Maybe the users manipulates regular lists with symbols and keywords,
    ;; without them being plists.
    ;;
    ;; For many plists:
    ;; - either set the :plists-p key argument to T
    ;; - either call the PLISTS-TABLE function directly.
    ((and (every #'property-list-p one-or-many-objects)
          plists-p)
     (apply #'plists-table one-or-many-objects keys))
    (t
     (apply #'table-lists one-or-many-objects keys))
    ))

(defun vtable-dispatch (one-or-many-objects
                       &rest args
                       &key
                         (plists-p *prefer-plists-in-tables*)
                          &ALLOW-OTHER-KEYS
                         )
  "Call the appropriate VTABLE-* function for the type of objects.

  ONE-OR-MANY-OBJECTS can be:

  - a singe plist
  - a singe hash-table
  - a list of hash-tables
  - a list of property lists: in that case, we can not be sure
   that the user manipulates a list of plists or a list of regular lists with
   symbols and keywords.
   Set the PLISTS-P key argument to T or call the PLISTS-TABLE function directly.
  - a list of lists.
  "
  (remf args :plists-p) ; either add the key arg to all functions, either delete it. Destructive.
  (cond
    ;; One HT.
    ((hash-table-p one-or-many-objects)
     (apply #'ht-vtable one-or-many-objects args))
    ;; A list of HTs.
    ((and (consp one-or-many-objects)
          (hash-table-p (first one-or-many-objects)))
     (apply #'hts-vtable one-or-many-objects args))
    ;; One plist.
    ((property-list-p one-or-many-objects)
     (apply #'plist-vtable one-or-many-objects args))
    ;; Can be distinguish between a list of plists and a list of lists?
    ;; Maybe the users manipulates regular lists with symbols and keywords,
    ;; without them being plists.
    ;;
    ;; For many plists:
    ;; - either set the :plists-p key argument to T
    ;; - either call the PLISTS-TABLE function directly.
    ((and (every #'property-list-p one-or-many-objects)
          plists-p)
     (apply #'plists-vtable one-or-many-objects args))
    (t
     (apply #'vtable-lists one-or-many-objects args))
    ))

(defun table (objects
              &key
                (plists-p *prefer-plists-in-tables*)
                (keys nil)
                (exclude nil)
                ;; common args:
                (mark-suffix  #\*)
                (border-chars "-|+")
                (border-style :default)
                (header-style :default)
                (cell-style   :default)
                (mark-style   :default)
                (col-header   nil)
                ;; (cols 1000)
                (margin       0)
                (column-width *column-width*)
                (align        :left)
                (stream       *standard-output*))
  "Print a table filling cells with OBJECTS.

  OBJECTS can be:

  - a list of lists of string designators with equal lengths
.   - generally, the first list is a list of headers.
  - a list of hash-tables
    - the table displays the first hash-table keys and all the hash-tables values.
    - see also HTS-TABLE
  - a list of property-lists
    - the table prints the keys and all the plists' values
    - to help the TABLE understand the arguments are plists
      and not regular lists, set the PLISTS-P key argument to T
      or the variable *prefer-plists-in-tables* to T.
    - see also PLISTS-TABLE
  - a single hash-table
  - a single plist.

  KEYS is a list of keys to display (only applicable for hash-tables and plists). The associated rows or columns will be displayed.

  EXCLUDE is a list of keys to NOT display (only applicable for hash-tables and plists).

Example:

   (table '((:A :B :C) (1 2 3)))

=>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+
    |10       |20       |30       |
    +---------+---------+---------+


See VTABLE to print the table vertically.

If BORDER-STYLE is NIL, no border will be
printed, otherwise BORDER-STYLE is expected to be a keyword that denotes
the style in which borders of the table should be printed.

HEADER-STYLE will be
applied to the first row of the table (also to the first column if
COL-HEADER is not NIL) and CELL-STYLE will be applied to all other rows. If
CELL-STYLE is a list, its elements will be used to differently render every
column.

Objects that end with MARK-SUFFIX will be printed using MARK-STYLE.

COLUMN-WIDTH is 10 by default (see `*column-width*'). It can be an integer that applies to
all columns, or a list designator to set a different
width for every column. A cell content is truncated to fit the width. See `str:*ellipsis*'
for the ellusion string, `(…)' by default.

ALIGN controls the alignmet inside a cell. It can take the values :LEFT (default value), :CENTER, and :RIGHT.

MARGIN, an integer, is the left margin of the whole table.

Output goes to STREAM."
  (table-dispatch objects
                  :plists-p plists-p
                  :keys keys
                  :exclude exclude
                  :mark-suffix mark-suffix
                  :border-chars border-chars
                  :border-style border-style
                  :header-style header-style
                  :cell-style cell-style
                  :mark-style mark-style
                  :col-header col-header
                  :margin margin
                  :column-width column-width
                  :align align
                  :stream stream))

(defun vtable (objects
               &key
                 (plists-p *prefer-plists-in-tables*)
                 (keys nil)
                 (exclude nil)
                 ;; common args:
                 (mark-suffix  #\*)
                 (border-chars "-|+")
                 (border-style :default)
                 (header-style :default)
                 (cell-style   :default)
                 (mark-style   :default)
                 (col-header   nil)
                 ;; (cols 1000)
                 (margin       0)
                 (column-width *column-width*)
                 (align        :left)
                 (stream       *standard-output*))
  "Print a vertical table.

  See TABLE for all options and the accepted OBJECTS types.

  Example:

   (vtable '((:A :B :C) (1 2 3)))

=>

    +---------+---------+
    |A        |1        |
    +---------+---------+
    |B        |2        |
    +---------+---------+
    |C        |3        |
    +---------+---------+
"
  (vtable-dispatch objects
                   :plists-p plists-p
                   :keys keys
                   :exclude exclude
                   ;; display options
                   :mark-suffix mark-suffix
                   :border-chars border-chars
                   :border-style border-style
                   :header-style header-style
                   :cell-style cell-style
                   :mark-style mark-style
                   :col-header col-header
                   :margin margin
                   :column-width column-width
                   :align align
                   :stream stream))


(defun table-lists (objects
                    &key
                      (mark-suffix  #\*)
                      (border-chars "-|+")
                      (border-style :default)
                      (header-style :default)
                      (cell-style   :default)
                      (mark-style   :default)
                      (col-header   nil)
                      ;; (cols 1000)
                      (margin       0)
                      (column-width *column-width*)
                      (align        :left)
                      (stream       *standard-output*)
                      ;; accept :keys and :exclude.
                    &ALLOW-OTHER-KEYS
                      )
  (perform-hook :before-printing stream)
  (let* ((objects (mapcar #'ensure-cons objects))
         (nb-columns (largest-length objects))
         (cell-style (ensure-circular-list cell-style))
         ;; the column width is made a circular list: use with POP.
         (column-width (ensure-circular-list column-width))
         (width (1+ (reduce #'+ (subseq column-width 0 nb-columns))))
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
                 (dolist (i (subseq column-width 0 nb-columns))
                   (check-type i (integer 1))
                   (princ (char border-chars 2) stream)
                   (dotimes (j (1- i))
                     (princ (char border-chars 0) stream)))
                 (princ (char border-chars 2) stream)
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
                   (let ((cell (string* cell))
                         (width (pop column-width)))
                     (v-border)
                     (set-style
                      (cond ((ends-with-subseq mark-suffix cell)
                             mark-style)
                            ((zerop index)
                             header-style)
                            ((and col-header (zerop i))
                             header-style)
                            (t (pop cell-style))))
                     (princ (str:shorten (- width
                                            (if border-style 1 0))
                                         cell)
                            stream)
                     (set-style :default stream)
                     (print-white-space (- width
                                           (length cell)
                                           (if border-style 1 0))
                                        stream)
                     (incf i)))
                 (v-border)
                 (terpri))))

      (dolist (row objects)
        (h-border)
        (print-row row-index row)
        (incf row-index))
      (h-border)))
  (finish-output stream)
  (perform-hook :after-printing stream)
  nil)

;; From trivial-types.
(defun property-list-p (object)
  "Returns true if OBJECT is a property list.

Examples:

    (property-list-p 1) => NIL
    (property-list-p '(1 2 3)) => NIL
    (property-list-p '(foo)) => NIL
    (property-list-p nil) => T
    (property-list-p '(foo 1)) => T
    (property-list-p '(:a 1 :b 2)) => T"
  ;; (declare (optimize . #.*standard-optimize-qualities*))
  (typecase object
    (null t)
    (cons
     (loop
       (if (null object)
           (return t)
           (let ((key (car object))
                 (next (cdr object)))
             (if (or (not (symbolp key))
                     (not (consp next)))
                 (return)
                 (setq object (cdr next)))))))))


(defun invert-plists-matrix (objects)
  "Transform a list of rows to a list of columns.

  (invert-plists-matrix '((a b c) (1 2 3)))
  ;; => ((A 1) (B 2) (C 3))
  "
  (loop :with content-length = (length (first objects))
        :with grid = (loop :repeat content-length :collect (make-list (length objects)))
        :for obj :in objects
        :for i := 0 :then (incf i)
        :do (loop :for j :from 0 :to (1- content-length)
                  :for row := (nth j grid)
                  :do  (setf (nth i row) (nth j obj)))
        :finally (return grid)))

;; copied from table-lists
(defun vtable-lists (objects &key
                               ;; (cols 1000)
                               (mark-suffix  #\*)
                               (border-chars "-|+")
                               (border-style :default)
                               (header-style :default)
                               (cell-style   :default)
                               (mark-style   :default)
                               (col-header   nil)
                               (margin       0)
                               (column-width *column-width*)
                               (align        :left)
                               (stream       *standard-output*)
                               &ALLOW-OTHER-KEYS)
  "Print a table where the headers are in the first column, not the firts row.

  Like TABLE, OBJECTS must be a list of string designators with the same length.

  Example output, where '(:A :B :C) is the list of headers:

    (vtable '((:A :B :C) (1 2 3) (10 20 30) (1.1 2.2 3.3)))

    +---------+---------+---------+---------+
    |A        |1        |10       |1.1      |
    +---------+---------+---------+---------+
    |B        |2        |20       |2.2      |
    +---------+---------+---------+---------+
    |C        |3        |30       |3.3      |
    +---------+---------+---------+---------+

  With :border-style set to NIL:

    A         1         10        1.1
    B         2         20        2.2
    C         3         30        3.3

"
  ;; first object is the keys,
  ;; the rest are lists of values.
  (table (invert-plists-matrix objects)
         :mark-suffix mark-suffix
         :border-chars border-chars
         :border-style border-style
         :header-style header-style
         :cell-style cell-style
         :mark-style mark-style
         :col-header col-header
         ;; :cols cols
         :margin margin
         :column-width column-width
         :align align
         :stream stream))

(defun plist-table (plist &key
                            (cols 1000)
                            (mark-suffix  #\*)
                            (border-chars "-|+")
                            (border-style :default)
                            (header-style :default)
                            (cell-style   :default)
                            (mark-style   :default)
                            (col-header   nil)
                            (margin       0)
                            (column-width *column-width*)
                            (align        :left)
                            (stream       *standard-output*)
                            )
  "Print PLIST as a table: the plist keys as the headers row, the plist values as one row below.

  COLS allows to limit the number of columns.

  Other arguments are passed to the TABLE function.

  Example:

    (plist-table '(:a 1 :b 2 :c 3))

  =>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+

    See also PLIST-VTABLE for headers in a column."
  (let ((keys (serapeum:plist-keys plist))
        (values (serapeum:plist-values plist)))
    (table (list (serapeum:take cols keys)
                 (serapeum:take cols values))
           :mark-suffix mark-suffix
           :border-chars border-chars
           :border-style border-style
           :header-style header-style
           :cell-style cell-style
           :mark-style mark-style
           :col-header col-header
           :margin margin
           :column-width column-width
           :align align
           :stream stream
           )))

(defun plist-vtable (plist
                     &key
                       (mark-suffix  #\*)
                       (border-chars "-|+")
                       (border-style :default)
                       (header-style :default)
                       (cell-style   :default)
                       (mark-style   :default)
                       (col-header   nil)
                       (margin       0)
                       (column-width *column-width*)
                       (align        :left)
                       (stream       *standard-output*)
                       )
  "Print PLIST as a table, where the first column is the keys, the second column is the value.

  Example:

    (plist-vtable '(:a 1 :b 2 :c 3))

  =>

    +---------+---------+
    |A        |1        |
    +---------+---------+
    |B        |2        |
    +---------+---------+
    |C        |3        |
    +---------+---------+

  See also PLIST-TABLE for headers in the first row."
  (table (serapeum:batches plist 2)
         :mark-suffix mark-suffix
         :border-chars border-chars
         :border-style border-style
         :header-style header-style
         :cell-style cell-style
         :mark-style mark-style
         :col-header col-header
         :margin margin
         :column-width column-width
         :align align
         :stream stream
         ))

(defun ht-table (ht &key
                      (cols 1000)
                      (mark-suffix  #\*)
                      (border-chars "-|+")
                      (border-style :default)
                      (header-style :default)
                      (cell-style   :default)
                      (mark-style   :default)
                      (col-header   nil)
                      (margin       0)
                      (column-width *column-width*)
                      (align        :left)
                      (stream       *standard-output*)
                      )
  "Print the hash-table HT as a table: the keys as the headers row, the values as one row below.

  COLS allows to limit the number of columns.

  Other arguments are passed to the TABLE function.

  Example:

    (ht-table (serapeum:dict :a 1 :b 2 :c 3))

  =>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+

    See also HT-VTABLE for headers in a column."
  ;; XXX: watch out we reverse the orders to try to have them in first-in first-out.
  ;;This could be a parameter.
  (let ((keys (reverse (alexandria:hash-table-keys ht)))
        (values (reverse (alexandria:hash-table-values ht))))
    (table (list (serapeum:take cols keys)
                 (serapeum:take cols values))
           :mark-suffix mark-suffix
           :border-chars border-chars
           :border-style border-style
           :header-style header-style
           :cell-style cell-style
           :mark-style mark-style
           :col-header col-header
           :margin margin
           :column-width column-width
           :align align
           :stream stream
           )))

(defun ht-vtable (ht
                     &key
                       (mark-suffix  #\*)
                       (border-chars "-|+")
                       (border-style :default)
                       (header-style :default)
                       (cell-style   :default)
                       (mark-style   :default)
                       (col-header   nil)
                       (margin       0)
                       (column-width *column-width*)
                       (align        :left)
                       (stream       *standard-output*)
                       )
  "Print the hash-table HT as a table, where the first column is the keys, the second column is the value.

  Example:

    (ht-vtable '(:a 1 :b 2 :c 3))

  =>

    +---------+---------+
    |A        |1        |
    +---------+---------+
    |B        |2        |
    +---------+---------+
    |C        |3        |
    +---------+---------+

  See also HT-TABLE for headers in the first row."
  (table (reverse (serapeum:batches
                   (alexandria:hash-table-plist ht)
                   2))
         :mark-suffix mark-suffix
         :border-chars border-chars
         :border-style border-style
         :header-style header-style
         :cell-style cell-style
         :mark-style mark-style
         :col-header col-header
         :margin margin
         :column-width column-width
         :align align
         :stream stream
         ))

(defun hts-table (ht-list &key
                            (cols 1000)
                            (mark-suffix  #\*)
                            (border-chars "-|+")
                            (border-style :default)
                            (header-style :default)
                            (cell-style   :default)
                            (mark-style   :default)
                            (col-header   nil)
                            (margin       0)
                            (column-width *column-width*)
                            (align        :left)
                            (stream       *standard-output*)
                            )
  "Print the list of hash-tables HT-LIST as a table: the keys as the first row, the values of each hash-table as one row below.

  Other arguments are passed to the TABLE function.

  Example:

    (hts-table (list (serapeum:dict :a 1 :b 2 :c 3)
                     (serapeum:dict :a 10 :b 20 :c 30)))

  =>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+
    |10       |20       |30       |
    +---------+---------+---------+

 See also HTS-VTABLE for a vertical table with keys displayed in the left column."
  (declare (ignorable cols))
  ;; XXX: watch out we reverse the orders to try to have them in first-in first-out.
  ;;This could be a parameter.
  (let* ((keys (reverse (alexandria:hash-table-keys (first ht-list))))
         (rows (collect-hash-tables-values keys ht-list)))
    (table (cons keys rows)
           :mark-suffix mark-suffix
           :border-chars border-chars
           :border-style border-style
           :header-style header-style
           :cell-style cell-style
           :mark-style mark-style
           :col-header col-header
           :margin margin
           :column-width column-width
           :align align
           :stream stream
           )))

(defun collect-hash-tables-values (keys ht-list)
  (loop for ht in ht-list
        collect (loop for key in keys
                      collect (gethash key ht))))

(defun hts-vtable (ht-list
                   &key
                     (mark-suffix  #\*)
                     (border-chars "-|+")
                     (border-style :default)
                     (header-style :default)
                     (cell-style   :default)
                     (mark-style   :default)
                     (col-header   nil)
                     ;; (cols 1000)
                     (margin       0)
                     (column-width *column-width*)
                     (align        :left)
                     (stream       *standard-output*)
                     )
  "Print the list of HASH-TABLES as a vertical table, where the first column is the keys,
  the other columns are the values of each hash-table.

  Example:

    (hts-vtable (list (dict :a 1 :b 2 :c 3)
                      (dict :a 10 :b 20 :c 30)))

  =>

    +---------+---------+---------+
    |A        |1        |10       |
    +---------+---------+---------+
    |B        |2        |20       |
    +---------+---------+---------+
    |C        |3        |30       |
    +---------+---------+---------+

  See also HTS-TABLE for a regular display with the keys as headers in the first row."
  (let* ((keys (reverse (alexandria:hash-table-keys (first ht-list))))
         (values (collect-hash-tables-values keys ht-list)))

    (vtable (cons keys values)
            :stream stream
            :mark-suffix mark-suffix
            :border-chars border-chars
            :border-style border-style
            :header-style header-style
            :cell-style cell-style
            :mark-style mark-style
            :col-header col-header
            :margin margin
            :column-width column-width
            :align align
            ;; :cols cols                  ; unused in table
            )))

(defun collect-plists-values (keys plists)
  (loop for plist in plists
        collect (loop for key in (uiop:ensure-list keys)
                      collect (getf plist key))))

(defun remove-keys (keys exclude)
  ;; don't use set-difference to preserve order.
  (remove-if (lambda (it) (find it exclude)) keys))

(defun plists-table (plist-list
                     &key
                       ;; specific options:
                       (keys nil)
                       (exclude nil)

                       ;; common args:
                       (mark-suffix  #\*)
                       (border-chars "-|+")
                       (border-style :default)
                       (header-style :default)
                       (cell-style   :default)
                       (mark-style   :default)
                       (col-header   nil)
                       ;; (cols 1000)
                       (margin       0)
                       (column-width *column-width*)
                       (align        :left)
                       (stream       *standard-output*)
                       )
  "Print a list of plists as a TABLE.

  The first row shows the plist keys, taken from the first plist object.
  All other rows show the values of all the plist objects.

  If KEYS is given, the table will show only the values, and as many columns, for these keys.
  If EXCLUDE is given, the plist values and associated columns are ignored.

  Example:

  (plists-table '((:A 1 :B 2 :C 3) (:A 10 :B 20 :C 30)))

  =>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+
    |10       |20       |30       |
    +---------+---------+---------+
"
  (let* ((keys (remove-keys
                (or (uiop:ensure-list keys)
                    (serapeum:plist-keys (first plist-list)))
                (uiop:ensure-list exclude)))
         (values (collect-plists-values keys plist-list)))

    (table (cons keys values)
           :stream stream
           :mark-suffix mark-suffix
           :border-chars border-chars
           :border-style border-style
           :header-style header-style
           :cell-style cell-style
           :mark-style mark-style
           :col-header col-header
           :margin margin
           :column-width column-width
           :align align
           ;; :cols cols                  ; unused in table
           )))

(defun plists-vtable (plist-list
                      &key
                        ;; specific argument:
                        (keys nil)
                        (exclude nil)
                        ;; common args:
                        (mark-suffix  #\*)
                        (border-chars "-|+")
                        (border-style :default)
                        (header-style :default)
                        (cell-style   :default)
                        (mark-style   :default)
                        (col-header   nil)
                        (margin       0)
                        (column-width *column-width*)
                        (align        :left)
                        (stream       *standard-output*)
                        )
  "Print a list of plists as a VTABLE.

  See also VTABLE with the :plists-p key argument and `*prefer-plists-in-tables*`.

  The first colum (and not row) shows the plist keys, taken from the first plist object.
  All other rows show the values of all the plist objects.

  If KEYS is given, the table will show only the values, and as many rows, for these keys.
  If EXCLUDE is given, the plist values and associated rows are ignored.

  Example:

  (plists-vtable '((:A 1 :B 2 :C 3) (:A 10 :B 20 :C 30)))

  =>

    +---------+---------+---------+
    |A        |1        |10       |
    +---------+---------+---------+
    |B        |2        |20       |
    +---------+---------+---------+
    |C        |3        |30       |
    +---------+---------+---------+
"
  (let* ((keys (remove-keys
                (or (uiop:ensure-list keys)
                    (serapeum:plist-keys (first plist-list)))
                (uiop:ensure-list exclude)))
         (values (collect-plists-values keys plist-list)))

    (vtable (cons keys values)
            :stream stream
            :mark-suffix mark-suffix
            :border-chars border-chars
            :border-style border-style
            :header-style header-style
            :cell-style cell-style
            :mark-style mark-style
            :col-header col-header
            :margin margin
            :column-width column-width
            :align align
            )))
