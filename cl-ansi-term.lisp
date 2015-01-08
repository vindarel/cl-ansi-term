;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; cl-ansi-term - library to output formatted text on ANSI-compliant
;;; terminals.
;;;
;;; Copyright (c) 2015 Mark Karpov
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
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
              #:set-style
              #:hr
              #:progress-bar))

(in-package #:cl-ansi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                        Parameters and Constants                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *effects-enabled* t
  "If this variable is bound to non-NIL value, graphic rendition effects are
enabled, otherwise they are disabled.")

(defparameter *terminal-width* 80
  "Many functions use this value to output text nicely. Default value is
80. If you want to dynamically change this value, write and register
:BEFORE-PRINTING hook and reassign it before printing takes place.")

(defparameter *hooks* (make-hash-table)
  "This variable is bound to hash table that provides access to lists of
functions by given key. We use keywords as keys. Arguments for the functions
depend entirely on EVENT on which every function is called.")

(defparameter *style-sheet* (make-hash-table)
  "This hash table contains strings for various styles of terminal output,
defined with UPDATE-STYLE-SHEET.")

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
  "All supported rendition effects. Some of them are hardly ever supported.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Hooks                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-hook (event function)
  "Register a hook. When predefined EVENT occurs FUNCTION will be
called. You can register many functions to call on the same event.

Possible values of EVENT:

:BEFORE-PRINTING - FUNCTION is invoked just before printing takes place.
:AFTER-PRINTING - FUNCTION is invoked after printing."
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
  "Updates style sheet used by the application. Every items of ALIST must be
a list with CAR denoting name of style sheet entry and the rest should be
collection of tokens that define terminal rendition. Tokens can represent
various things: foreground color, background color, and effects. Every type
of token has its defaults, so you can omit some tokens. However, if there
more than one token of the same type (for example :RED and :GREEN - both
tokens represent foreground color), result is unpredictable and depends on
internal workings of Common Lisp implementation used. You cannot
redefine :DEFAULT style, it's always represent default parameters of
rendition."
  (dolist (entry alist)
    (destructuring-bind (style . tokens) entry
      (if tokens
          (setf (gethash style *style-sheet*) (ansi-escape-seq tokens))
          (remhash style *style-sheet*))))
  (setf (gethash :default *style-sheet*) (ansi-escape-seq))
  (values))

(defun set-style (style &optional (stream *standard-output*))
  "Sets terminal rendition according to defined STYLE. It does nothing if
*EFFECTS-ENABLED* is NIL or output stream is not interactive (e.g.
redirected to a file). This is low-level function and it's not recommended
to use it directly."
  (awhen (and *effects-enabled*
              (interactive-stream-p stream)
              (gethash style *style-sheet*))
    (princ it stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                               Utilities                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-partially (text start end stream)
  "Partially print given TEXT starting from START character until END
character is reached. All output goes to STREAM."
  (do ((i start (1+ i)))
      ((= i end))
    (princ (char text i) stream)))

(defun hr (&key
             (filler #\-)
             (style  :default)
             (width  0)
             (align  :left)
             (stream *standard-output*))
  "Print horizontal line. Characters in the line are created by repeating
given FILLER (it must be a string designator) until WIDTH characters
accumulated. If WIDTH is not a positive number, *TERMINAL-WIDTH* will be
added to it to get positive WIDTH. STYLE controls graphic rendition if
output device is interactive. ALIGN should be a keyword: :LEFT, :RIGHT,
or :CENTER. Output goes to STREAM (*STANDARD-OUTPUT* is default)."
  (perform-hook :before-printing)
  (let* ((width (+ width (if (plusp width) 0 *terminal-width*)))
         (white-space-len
          (case align
            (:right  (- *terminal-width* width))
            (:center (floor (- *terminal-width* width)
                            2))
            (t       0))))
    (dotimes (i white-space-len)
      (princ #\space stream))
    (set-style style stream)
    (let* ((filler (string filler))
           (fillen (length filler)))
      (multiple-value-bind (rough rest)
          (floor width fillen)
        (dotimes (i rough)
          (princ filler stream))
        (print-partially filler 0 rest stream))))
  (set-style :default stream)
  (terpri stream)
  (finish-output stream)
  (perform-hook :after-printing)
  (values))

(defun progress-bar (label progress
                     &key
                       (margin 0)
                       (label-style :default)
                       (filler #\#)
                       (bar-style :default)
                       (num-style :default)
                       (bar-width -40)
                       (stream *standard-output*))
  "Print progress bar. If PROGRESS is less than 100, move cursor to the
beginning of current line, so next invocation of PROGRESS-BAR will rewrite
it. This function doesn't print anything if PROGRESS is less than 100 and
output stream is not interactive or *EFFECTS-ENABLED* is NIL. Insert MARGIN
spaces, then LABEL (style for the label is set with LABEL-STYLE). Size of
progress bar is set by BAR-WIDTH. If BAR-WIDTH is not a positive number, it
*TERMINAL-WIDTH* will be added to it to get positive BAR-WIDTH. BAR-STYLE is
the style that will be used for the bar itself, while NUM-STYLE will be used
to number of percents and some additional elements. Output goes to
STREAM (*STANDARD-OUTPUT* is default)."
  (unless (and (< progress 100)
               (or (not (interactive-stream-p stream))
                   (not *effects-enabled*)))
    (perform-hook :before-printing)
    (let* ((bar-width (+ bar-width (if (plusp bar-width) 0 *terminal-width*)))
           (total-cells (- bar-width 8))
           (filled-cells (floor (/ (* total-cells progress) 100)))
           (empty-cells (- total-cells filled-cells))
           (filler (string filler)))
      (dotimes (i margin)
        (princ #\space stream))
      (set-style label-style stream)
      (princ label stream)
      (set-style :default)
      (dotimes (i (- *terminal-width*
                     bar-width
                     margin
                     (length label)))
        (princ #\space stream))
      (set-style num-style stream)
      (princ #\[ stream)
      (set-style bar-style stream)
      (multiple-value-bind (rough rest)
          (floor filled-cells (length filler))
        (dotimes (i rough)
          (princ filler stream))
        (print-partially filler 0 rest stream))
      (set-style :default)
      (dotimes (i empty-cells)
        (princ #\space stream))
      (set-style num-style stream)
      (format stream "] ~3d %" progress)
      (set-style :default stream)
      (if (< progress 100)
          (format stream "~c[0G" #\escape)
          (terpri stream)))
    (finish-output stream)
    (perform-hook :after-printing))
    (values))

;; unordered lists (with complex structure)

;; ordered lists (with complex structure)

;; format (= paragraphs: fill-column, justification, line prefix and postfix)

;; tables (with borders (also invisible borders) and without them)



;;; testing...

(defun test-print (str style)
  (set-style style)
  (princ str)
  (set-style :default)
  (terpri)
  (finish-output))

(update-style-sheet
 '((:error :red :b-green)
   (:num :yellow :underline)
   (:foo :cyan)))

(defun bar-test ()
  (progress-bar "boo" 15)
  (sleep 1)
  (progress-bar "boo" 75)
  (sleep 1)
  (progress-bar "boo" 100))
