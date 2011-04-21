;; This code is placed under the Lesser GNU Public License (LGPL)
;; (see http://www.fsf.org/licenses/lgpl.html) as
;; clarified for Lisp by Franz when they released AllegroServe (see
;; http://allegroserve.sourceforge.net/license-allogroserve.txt)
;;
;; This software is "as is", and has no warranty of any kind.  The
;; author assumes no responsibility for the consequences of any use
;; of this software.

(defsystem :csv-parser
  :author "Alain Picard <alain.picard@memetrics.com>"
  :license "LLGPL"
  :description "CSV parsing/writing utilities, a la Microsoft Excel"
  :components
  ((:file "csv-parser")))
