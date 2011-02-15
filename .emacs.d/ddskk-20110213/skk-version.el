;;; skk-version.el --- version information for SKK

;; Copyright (C) 2000, 2001, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-version.el,v 1.47 2011/01/03 06:37:05 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2011/01/03 06:37:05 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(put 'skk-version 'product-name "Daredevil SKK")
(put 'skk-version 'version-string "14.2.50")
(put 'skk-version 'codename "Juso")

;;;###autoload
(defun skk-version (&optional without-codename)
  "Return SKK version with its codename.
If WITHOUT-CODENAME is non-nil, simply return SKK version without
the codename."
  (interactive "P")
  (if (interactive-p)
      (message "%s" (skk-version without-codename))
    (if without-codename
	(format "%s/%s"
		(get 'skk-version 'product-name)
		(get 'skk-version 'version-string))
      (format "%s/%s (%s)"
	      (get 'skk-version 'product-name)
	      (get 'skk-version 'version-string)
	      (get 'skk-version 'codename)))))

(provide 'skk-version)

;;; skk-version.el ends here
