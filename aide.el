;;; aide.el --- Integrate developing tools with ease

;; Copyright (C) 2017 ChienYu Lin

;; Author: ChienYu Lin <cy20lin@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (projectile "0.14.0")))

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

;; Aide is a framework that help you to integrate and configure
;; developing tools with easier manners.

;; Aide provide a project based configurating method, for all buffers
;; within the same project. By registering a project-type, with proper
;; configurations and properties. Aide will load these configurations
;; whenever current buffer match the condition of that project-type.
;; All buffers with same project-type will share the same configurations
;; in the end.

;; Aide utilizes those feature provided by `projectile' to help Aide
;; to deal with project related operations. Also, Aide extends features
;; to `projectile' to provide project based properties and configurations.

;; For those files and buffers that are not inside a porject, Aide also
;; provided a way to configure them. That is to register a non-project
;; type to Aide type system, these registered non-project types work almost
;; the same as those project types, with the exception that the configurations
;; are loaded on a per-file basis. If the current buffer is not in a project
;; or is in a project but cannot found a proper project-type to handle the
;; configurating of this project (i.e. generic project-type), Aide will
;; fall back to apply the non-project-type configurations to that buffer.

;; Aide provide hooks so that you can add your custom operations before or
;; after the project or non-project configurating operations. It it also
;; possible to add hooks for certain major modes.

;; Enjoy :D

;;; Code:

(require 'aide-mode)

(provide 'aide)

;;; aide.el ends here
