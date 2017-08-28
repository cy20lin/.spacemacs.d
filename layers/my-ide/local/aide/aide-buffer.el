;;; aide-buffer.el

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

(require 'aide-project)
(require 'aide-non-project)

;;;###autoload
(defun aide-buffer-type ()
  "Get the current buffer-type"
  ;; TODO: Maybe this can be this can be customized by users
  (pcase (list (aide-project-type) (aide-non-project-type))
    (`(nil ,non-project-type) (list 'non-project-type non-project-type))
    (`(generic ,non-project-type) (list 'non-project-type non-project-type))
    (`(,project-type ,_) (list 'project-type project-type))))

;;;###autoload
(defun aide-buffer-type-run (buffer-type keys &rest args)
  "Run the property in `buffer-type' with given accessing `keys'."
  (pcase buffer-type
    (`(project ,type) (apply #'aide-project-type-run `(,type ,keys . ,args)))
    (`(non-project ,type) (apply #'aide-non-project-type-run `(,type ,keys . ,args)))))

;;;###autoload
(defun aide-buffer-type-get (buffer-type keys)
  "Get the property in `buffer-type' with given accessing `keys'."
  (pcase buffer-type
    (`(project ,type) (aide-project-type-get type keys))
    (`(non-project ,type) (aide-non-project-type-get type keys))))

;;;###autoload
(defun aide-buffer-run (keys &rest args)
  "Run the property in current `buffer-type' with given accessing `keys'."
  (apply #'aide-buffer-type-run `(,(aide-buffer-type) ,keys . ,args)))

;;;###autoload
(defun aide-buffer-get (keys)
  "Get the property in current `buffer-type' with given accessing `keys'."
  (aide-buffer-type-get (aide-buffer-type) keys))

(provide 'aide-buffer)
;;; aide-buffer.el ends here
