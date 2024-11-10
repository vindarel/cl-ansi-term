;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; ASDF system definition for cl-ansi-term—library to output formatted text
;;; on ANSI-compliant terminals.
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

(asdf:defsystem :cl-ansi-term
  :version      "0.1.3"
  :description  "library to output formatted text on ANSI-compliant terminals"
  :author       "Mark Karpov"
  :license      "GNU GPL, version 3"
  :components   ((:file "cl-ansi-term"))
  :depends-on   (:alexandria
                 :anaphora
                 :serapeum
                 :str))
