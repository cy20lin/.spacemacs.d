;;; aide-mode.el --- Definitions for aide-mode

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

(require 'aide-buffer)

;;;###autoload
(defvar aide-mode-initialization-hook-for-major-mode
  (make-hash-table)
  "A hash table holding set of major-mode corresponding initialization-hooks, \
which are run at the beginning of aide-mode.")

;;;###autoload
(defvar aide-mode-finalization-hook-for-major-mode
  (make-hash-table)
  "A hash table holding set of major-mode corresponding finalization-hooks, \
which are run at the end of aide-mode.")

;;;###autoload
(defvar aide-mode-initialization-hook (list 'aide-mode-initialize-for-major-mode)
  "Initialization hook, which are run at the very beginning of aide-mode.")

;;;###autoload
(defvaralias 'aide-mode-finalization-hook 'aide-mode-hook
  "Finalization hook, which are run at the end of aide-mode.")

;;;###autoload
(defun aide-mode-initialize-for-major-mode (&optional major-mode-)
  "Run initialization hook for specified major-mode-, \
use current major-mode if not specified."
  (let ((hook (gethash (or major-mode- major-mode) aide-mode-initialization-hook-for-major-mode)))
    (run-hooks 'hook)))

;;;###autoload
(defun aide-mode-finalize-for-major-mode (&optional major-mode-)
  "Run initialization hook for specified major-mode-, \
use current major-mode if not specified."
  (let ((hook (gethash (or major-mode- major-mode) aide-mode-finalization-hook-for-major-mode)))
    (run-hooks 'hook)))

;;;###autoload
(define-minor-mode aide-mode
  "Aide minor mode."
  :init-value nil
  (run-hooks 'aide-mode-initialization-hook)
  (let ((hook (aide-buffer-get '(hook))))
    (run-hooks 'hook)))

(add-hook 'aide-mode-hook 'aide-mode-finalize-for-major-mode)

;;;###autoload
(define-globalized-minor-mode global-aide-mode aide-mode
  aide-mode-try-enable
  :init-value nil)

;;;###autoload
(defvar aide-global-modes t
  "Modes for which option `aide-mode' is turned on.

If t, Aide Mode is turned on for all major modes.  If a list,
Mode is turned on for all `major-mode' symbols in that
list.  If the `car' of the list is `not', Aide Mode is turned
on for all `major-mode' symbols _not_ in that list.  If nil,
Aide Mode is never turned on by command `global-aide-mode'.")

;;;###autoload
(defun aide-mode-may-enable-p-on-modes ()
  "Whether `major-mode' is disallowed by `aide-global-modes'."
  (and
   (pcase aide-global-modes
     (`t t)
     (`(not . ,modes) (not (memq major-mode modes)))
     (modes (memq major-mode modes)))))

;;;###autoload
(defun aide-mode-may-enable-p-if-not-minibuffer ()
  "Return non-nil if current buffer is not a minibuffer."
  (not (minibufferp)))

;;;###autoload
(defvar aide-mode-may-enable-p-handlers
  (list 'aide-mode-may-enable-p-on-modes
        'aide-mode-may-enable-p-if-not-minibuffer)
  "List of may-enable-p handlers.")

;;;###autoload
(defun aide-mode-may-enable-p ()
  "Check if `aide-mode' may be activated."
  (cl-every 'funcall aide-mode-may-enable-p-handlers))

;;;###autoload
(defun aide-mode-try-enable ()
  "Enable aide-mode if predicate funcion returns non-nil."
  (when (aide-mode-may-enable-p) (aide-mode)))

(provide 'aide-mode)

;;; aide-mode.el ends here
