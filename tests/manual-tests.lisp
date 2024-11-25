(in-package :cl-ansi-term)


;; Normal lists
(progn
  ;; ANSI codes render ok in Slime's REPL with M-x slime-repl-ansi-color-mode,
  ;; but they don't render in the pretty-printing buffer (C-c C-p on the last paren).
  (setf *enable-effects-on-dumb-terminals* nil)

  (defparameter objects '(("pk" "title" "price")
                          (1 "lisp" 9.90)
                          (2 "common lisp" 100)
                          ))

  (banner "A single table")
  (table (first objects))

  (banner "A single table, in columns")
  (vtable (first objects))

  (banner "A list of lists")
  (table objects)

  (banner "A list of lists, in columns.")
  (vtable objects)

  (banner "same, ignoring the column pk")
  (vtable objects :exclude "pk")

  (update-style-sheet
   '((:color :cyan   :bold)
     (:danger :red :bold)
     (:green :green)
     (:default :green)
     ))

  (setf *enable-effects-on-dumb-terminals* t)
  (banner "print all cells with color")
  (table objects :cell-style :color)

  (banner "print in red prices that > 10, other prices in blue, other cells in green.")
  (table objects
         :cell-style (lambda (val header)
                       (when (equal "price" header)
                         (if (> val 10)
                             :danger
                             :color))))
  )

(progn

  (let ((*enable-effects-on-dumb-terminals* nil))
    (banner "Tables with large content, adapting to the terminal width." :space 1)
    ;; (cat-print "All tables should respect the same total width.")
    (table-lists '((pk "long title" "long story")
                   (1 "the unbelievable story of a Common Lisp programmen ep. 100010" "that line is quite long too in order to test")))

    (table-lists '(("pk" "title" "price")
                   (1 "lisp" 9.9) (2 "common lisp the cool prog language with a super nice cookbook contributed by yours truly because it's cool" 100)))

    (banner "This table sets a smaller max column width")
    (table-lists '(("pk" "title" "price")
                   (1 "lisp" 9.9) (2 "common lisp the cool prog language with a super nice cookbook contributed by yours truly because it's cool" 100))
                 :max-column-width 40)
    ))

;; plists
(progn
  (defparameter plist '(:a 1 :b 2 :c 3))

  (banner "A single plist")
  (table plist)

  (banner "A single plist, in columns")
  (vtable plist :plist t)

  (banner "A list of plists")
  (table (list plist plist plist) :plist t)

  (banner "A list of plists, in columns: we need the :plist t argument.")
  (vtable (list plist plist plist) :plist t)

  (banner "same, ignoring the column :b")
  (vtable (list plist plist plist) :plist t :exclude :b)
  )


;; HT
(progn
  (defparameter d (serapeum:dict :a 1.1 :b 2.2 :c 3.3))

  (banner "A single hash-table")
  (table d)

  (banner "A single hash-table, in columns")
  (vtable d)

  (banner "A single hash-table, ignoring column :B")
  (table d :exclude :b)

  (banner "A single hash-table, vertically ignoring column :B")
  (vtable d :exclude :b)

  (banner "A list of hash-tables")
  (table (list d d d))

  (banner "A list of hash-tables, ignoring column :B")
  (table (list d d d) :keys '(:a :c))

  (banner "A list of hash-tables, in columns")
  (vtable (list d d d))

  (banner "same, ignoring the column :b")
  (vtable (list d d d) :exclude :b)
  )

;; Association lists
(progn
  (defparameter alist '((a . 1) (b . 2) (c . 3)))

  (banner "A single alist")
  (table alist :alist t)

  (banner "A single alist, in columns")
  (vtable alist :alist t)

  (banner "A single alist, ignoring column :b => TODO")
  (vtable alist :exclude :b :alist t)

  (banner "A list of alists")
  (table (list alist alist alist) :alists t)

  (banner "A list of alists, in columns, with a bigger column width")
  (vtable (list alist alist alist) :COLUMN-WIDTH 20 :alists t)

  (banner "same, ignoring the column :b => TODO")
  (vtable (list alist alist alist)  :exclude :b :alists t)
  )
