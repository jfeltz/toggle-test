;;; haskell-test-qc.el --- Test writer for haskell

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

;;; Code:
(require 'haskell-source)

(defun haskell-test-qc (src-path)
  "A test writer for a Haskell QuickCheck test module."
  (insert 
    "{-# LANGUAGE TemplateHaskell #-}\n\n"
    "module " (to-haskell-module-name buffer-file-name) " where\n"
    "import " (to-haskell-module-name src-path) " -- test subjects\n"
    "import QuickCheck\n"
    "import Test.QuickCheck\n"
    "import Test.QuickCheck.Property.Monad\n\n"
    "main :: IO ()\n"
    "main = $quickCheckAll\n"
  )
  (goto-char (point-min))
  (forward-char 5)
  )

(provide 'haskell-test-qc)
