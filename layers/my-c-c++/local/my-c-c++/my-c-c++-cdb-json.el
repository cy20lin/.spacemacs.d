(require 'cl-lib)
(require 'json)

(defun my-c-c++-cdb-json (command &rest args)
  (cl-case command
    (get-compile-options (my-c-c++-cdb-json--get-compile-options))
    (get-file-compile-options (my-c-c++-cdb-json--get-file-compile-options))))

;;
;; private functions
;;

(defun my-c-c++-cdb-json--get-compile-options ())

(provide 'my-c-c++-cdb-json)
