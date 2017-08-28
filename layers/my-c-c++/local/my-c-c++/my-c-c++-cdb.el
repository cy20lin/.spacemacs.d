(require 'my-c-c++-cdb-guess-header)
(require 'my-c-c++-cdb-guess)
(require 'my-c-c++-cdb-json)

(defun my-c-c++--make-realtive-paths-in-flags-absolute (flags working-dir &optional options)
  ""
  (let ((new-flags nil)
        (make-next-absolute nil)
        (new-flag nil)
        (options (or options '("-isystem" "-I" "-iqoute" "--sysroot="))))
    (dolist (flag flags)
      (setq new-flag
            (or (cond
                 (make-next-absolute
                  (setq make-next-absolute nil)
                  (if (not (f-absolute? flag)) (f-join working-dir flag) flag))
                 (t (dolist (option options)
                      (cond
                       ((equal flag option)
                        (setq make-next-absolute t)
                        (return))
                       ((s-starts-with? option flag)
                        (let ((file (s-chop-prefix option flag)))
                          (return (concat option (if (not (f-absolute? file)) (f-join working-dir file))))))
                       )))) flag))
      (setq new-flags (cons new-flag new-flags)))
    (reverse new-flags)))

;; (cy-cdb--make-realtive-paths-in-flags-absolute '("-I./a" "-b" "asdf" "-I" "a" "-Ixxx") "d:/iiiii")

(provide 'my-c-c++-cdb)
