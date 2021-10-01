;;; ***************************************************************************
;;; Scone Construction grammar engine loader
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(defvar rootpath "/Users/yangyang/Documents/scone/")

(ql:quickload :cl-ppcre)
(ql:quickload :Alexandria)

(load (concatenate 'string rootpath "scone-loader"))
(scone)
(load-kb "core")
(load (concatenate 'string rootpath "Scone-CxG/dictionary"))
(load (concatenate 'string rootpath "Scone-CxG/grammar"))
(load (concatenate 'string rootpath "Scone-CxG/morphology"))
(load (concatenate 'string rootpath "Scone-CxG/matcher"))
(load (concatenate 'string rootpath "Scone-CxG/engine"))