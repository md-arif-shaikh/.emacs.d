;;; early-init.el --- Early initialization           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: init

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; Start full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; hide start-up message
(setq inhibit-startup-message t)
;; remove scroll bar and toolbar, fringe
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 0)
;; cusor
(setq-default cursor-type 'bar)

(provide 'early-init)
;;; early-init.el ends here
