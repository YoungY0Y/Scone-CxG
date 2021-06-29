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
(load (concatenate 'string rootpath "nlu\ engine/dictionary"))
(load (concatenate 'string rootpath "nlu\ engine/grammar"))
(load (concatenate 'string rootpath "nlu\ engine/matcher"))
(load (concatenate 'string rootpath "nlu\ engine/engine"))