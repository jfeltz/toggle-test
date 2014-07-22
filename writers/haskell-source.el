;;; haskell-source.el --- Source writer for haskell

;; Copyright (C) 2014  John P. Feltz

;; Author: John P. Feltz <jfeltz@gmail.com>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;   Much of this code is extracted and cleaned up from Chris Done's haskell-mode:
;;    https://github.com/haskell/haskell-mode/

(with-no-warnings (require 'cl))

(defun to-reversed-components (p)
  "Extract from the file path haskell module path elements, ordered in reverse."
  (loop for part in (reverse (split-string p "/"))
    while ; while provides early termination for loop
        ; turn off case sensitivity and run the matching predicate on 'part'.
        (let ((case-fold-search nil)) (string-match "^[A-Z]+" part))
        collect (replace-regexp-in-string "\\.l?hs$" "" part)
    )
)

(defun to-haskell-module-name (p)
  "Derive a module name by subpath recognition starting from the end of path."
   (mapconcat 'identity (reverse (to-reversed-components p)) ".")
  )

(defun haskell-source (&optional testpath)
  (insert 
    "-- | \n"
    "-- \n"
    (if testpath (concat "-- for test coverage see: \n-- " testpath "\n" ))
    "module " (to-haskell-module-name (buffer-file-name)) " where\n"
  )
  (goto-char (point-min))
  (forward-char 5)
)

(provide 'haskell-source)
