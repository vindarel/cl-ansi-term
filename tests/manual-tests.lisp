(in-package :cl-ansi-term)


;; Normal lists
(progn
  (defparameter objects '(("pk" "title" "price")
                          (1 "lisp" "9.90")
                          (2 "common lisp" "100")
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
  )

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

  (banner "same, ignoring the column :b => TODO")
  (vtable (list d d d)  :exclude :b)
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
