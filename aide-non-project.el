;;; aide-non-porject.el

;; Copyright (C) 2017 ChienYu Lin

;; Author: ChienYu Lin <cy20lin@gmail.com>

;; This file is part of Aide.

;; Aide is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Aide is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aide. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;;###autoload
(cl-defun aide-register-non-project-type (non-project-type modes- &key properties hook modes commands configs compile test run test-suffix test-prefix)
  "Register a non-project-type."
  (let ((props properties)
        (modes (append modes- modes)))
    (when hook         (setq props (plist-put props 'hook hook)))
    (when modes        (setq props (plist-put props 'modes modes)))
    (when commands     (setq props (plist-put props 'commands commands)))
    (when configs      (setq props (plist-put props 'configs configs)))
    (when compile      (setq props (plist-put props 'compile-command compile)))
    (when test         (setq props (plist-put props 'test-command test)))
    (when run          (setq props (plist-put props 'run-command run)))
    (when test-suffix  (setq props (plist-put props 'test-suffix test-suffix)))
    (when test-prefix  (setq props (plist-put props 'test-prefix test-prefix)))
    (puthash non-project-type props aide-non-project-types)
    (aide-non-project-type--add-modes non-project-type modes)))

;;;###autoload
(defun aide-non-project-type-p (non-project-type)
  "Check wether current buffer is non-project and its type is `non-project-type'."
  (eq (aide-non-project-type) non-project-type))

;;;###autoload
(defun aide-non-project-type ()
  "Get current buffer's non-project-type, return `nil' if not found."
  (or (car (gethash major-mode aide-mode-to-non-project-type-map)) 'generic))

;;;###autoload
(defun aide-non-project-types ()
  "Return all registered non-project-types in a list."
  (hash-table-keys aide-non-project-types))

;;;###autoload
(defun aide-non-project-type-run (non-project-type keys &rest args)
  "Run the property in `non-project-type' with given accessing `keys'."
  (let ((value (aide-non-project-type-get non-project-type keys)))
    (apply #'aide-non-project--handle-run `(,keys ,value . ,args))))

;;;###autoload
(defun aide-non-project-type-get (non-project-type keys)
  "Get the property in `non-project-type' with given accessing `keys'."
  (cl-reduce #'plist-get keys :initial-value (gethash non-project-type aide-non-project-types)))

;;;###autoload
(defun aide-non-project-p ()
  "Check wether current buffer is a non-project."
  (not (projectile-project-p)))

;;;###autoload
(defun aide-non-project-run (keys &rest args)
  "Run the property in current buffer's `non-project-type' with given accessing `keys'."
  (apply #'aide-non-project-type-run `(,(aide-non-project-type) . ,args)))

;;;###autoload
(defun aide-non-project-get (keys)
  "Get the property in current buffer's `non-project-type' with given accessing `keys'."
  (aide-project-type-get (aide-project-type) keys))

(defun aide-non-project--handle-run (keys value &rest args)
  "Handle all run handlers defined in `aide-non-project-run-handlers'."
  (cl-dolist (handler aide-non-project-run-handlers)
    (pcase (apply handler `(,keys ,value . ,args))
      ('stop (return 'stop))
      (_ (return)))))

;;;###autoload
(defun aide-non-project-default-run-handler (keys value &rest args)
  "Default run handler."
  (cond
   ((typep value 'function) (funcall value))
   ((and value (typep value 'list)) (compile (mapconcat #'shell-quote-argument (mapcar #'eval value) " ")))
   ((typep value 'string) (compile value))
   (t nil))
  nil)

;;;###autoload
(defvar aide-non-project-run-handlers (list #'aide-non-project-default-run-handler)
  "Default run-handlers.")

(defun aide-non-project-type--add-modes (non-project-type modes)
  "Register `modes' for `non-project-type'."
  (dolist (mode modes)
    (let ((non-project-types (gethash mode aide-mode-to-non-project-type-map)))
      (add-to-list 'non-project-types non-project-type)
      (puthash mode non-project-types aide-mode-to-non-project-type-map))))

;;;###autoload
(defvar aide-mode-to-non-project-type-map (make-hash-table)
  "A hash table holding all major-mode corresponding to non-project-types.")

;;;###autoload
(defvar aide-non-project-types (make-hash-table)
  "A hash table holding all non-project-types with its properties.")

(provide 'aide-non-project)
;;; aide-non-project.el ends here
