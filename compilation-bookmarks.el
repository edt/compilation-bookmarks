;;; compilation-bookmarks.el --- bookmarks for compilation bookmarks
;;
;; Copyright (C) 2015 Edgar Thier
;;
;; Author: Edgar Thier <info * edgarthier.net>
;; Version: 0.1
;; Keywords: tools, processes
;; URL:
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

(require 'compile)


(defgroup compilation-bookmarks nil
  "compilation bookmarks groups"
  :group 'convenience
  :group 'tools
  :group 'processes)


(defvar compilation-bookmarks nil "List of available compilation bookmarks")

(defvar compilation-bookmarks-active-bookmark nil "Currently used compilation bookmark")

;; (defvar compilation-bookmarks-save-file "~/.emacs.d/compilation-bookmarks" "Location of saved compilation bookmarks")


(defcustom compilation-bookmarks-save-file
  (expand-file-name "compilation-bookmarks" user-emacs-directory)
  "Location of saved compilation bookmarks"
  :group 'compilation-bookmarks
  :type 'string)


(defvar compilation-bookmarks-require-recompilation nil)


(defcustom compilation-bookmarks-prefix-key (kbd "C-c c")
  "Compilation-Bookmarks keymap prefix"
  :group 'compilation-bookmarks
  :type 'string)


(defvar compilation-bookmarks-command-compile-map
  (let ((map (make-sparse-keymap)))
    map)
  "Submap for compilation commands")

(fset 'compilation-bookmarks-command-compile-map compilation-bookmarks-command-compile-map)


(defvar compilation-bookmarks-command-compile-once-map
  (let ((map (make-sparse-keymap)))

    map)
  "Submap for compilation commands that shall run only once")

(fset 'compilation-bookmarks-command-compile-once-map compilation-bookmarks-command-compile-once-map)


(defvar compilation-bookmarks-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'compilation-bookmarks-add-bookmark)
    (define-key map "r" 'compilation-bookmarks-remove-bookmark)
    (define-key map "v" 'compilation-bookmarks-compile)
    (define-key map "i" 'compilation-bookmarks-compile-once)
    (define-key map "c" 'compilation-bookmarks-command-compile-map)
    map)
  "")

(fset 'compilation-bookmarks-command-map compilation-bookmarks-command-map)

(defvar compilation-bookmarks-map
  (let ((map (make-sparse-keymap)))
    (define-key map compilation-bookmarks-prefix-key 'compilation-bookmarks-command-map)
    map)
  "Keymap for compilation-bookmarks-minor-mode")


(defun compilation-bookmarks-add-compile-keybinding (bookmark)
  "Add bookmark to compile submap"
  (define-key compilation-bookmarks-command-compile-map (kbd (cbm-get-key bookmark)) `(lambda ()
                                                                                  (interactive)
                                                                                  (compilation-bookmarks-compile ,(cbm-get-name bookmark)))))


(defun compilation-bookmarks-remove-compile-keybinding (bookmark)
  "Remove bookmark from compile submap"
  (define-key compilation-bookmarks-command-compile-map (kbd (cbm-get-key bookmark)) nil))


(defun compilation-bookmarks-add-compile-once-keybinding (bookmark)
  "Add bookmark to compile-once-submap"
  (define-key compilation-bookmarks-command-compile-once-map (kbd (cbm-get-key bookmark)) `(lambda ()
                                                                                             (interactive)
                                                                                             (compilation-bookmarks-compile-once ,(cbm-get-name bookmark)))))


(defun compilation-bookmarks-remove-compile-once-keybinding (bookmark)
  "Remove bookmark from compile submap"
  (define-key compilation-bookmarks-command-compile-once-map (kbd (cbm-get-key bookmark)) nil))


(defun compilation-bookmarks-set-keybindings ()
  "Add keybindings for all compilation-bookmarks"
  (dolist (bm compilation-bookmarks)
    (if (equal (cbm-get-key bm) nil)
        (progn)
      (progn
        (compilation-bookarks-add-keybinding bm)
        (compilation-bookmarks-add-compile-once-keybinding bm)))))


(defun cbm-get-name (cbm)
  ""
  (cdr (assoc 'name cbm)))

(defun cbm-get-directory (cbm)
  ""
  (cdr (assoc 'directory cbm)))

(defun cbm-get-command (cbm)
  ""
  (cdr (assoc 'command cbm)))

(defun cbm-get-key (cbm)
  ""
  (cdr (assoc 'key cbm)))


(defun compilation-bookmarks-get-names ()
  "Return the names of all known bookmarks"
  (interactive)
  (let (tmp-list '())
    (dolist (cbm compilation-bookmarks)
      (add-to-list 'tmp-list (cdr (assoc 'name cbm))))
    tmp-list))


(defun compilation-bookmarks-get-names-interactive ()
  "Let user select bookmark and return selected one"
  (let ((selection (completing-read "Select bookmark: " (compilation-bookmarks-get-names))))
    selection))


(defun compilation-bookmarks-get-bookmark (selected-name)
  "Retrieve bookmark by name"
  (let ((bm nil))
    (dolist (n compilation-bookmarks)
      (if (string= selected-name (cbm-get-name n))
          (setq bm n)))
    bm))


(defun compilation-bookmarks-get-bookmark-interactive ()
  "Select bookmark by name given by user and return it"
  (let ((bookmark-selection (completing-read "Compilation bookmark to use: " (compilation-bookmarks-get-names)))
        (bm nil))
    (compilation-bookmarks-get-bookmark bookmark-selection)))


(defun compilation-bookmarks-add-bookmark (_dir _command _name _key)
  "Add compilation bookmark"
  (interactive "DDirectory to use: \nMCommand to use: \nMName of your bookmark: \nSKey (empty if none): ")
  (if (not (equal (compilation-bookmarks-get-bookmark _name) nil))
      (error "Name already exists!"))
  (if (equal _name nil)
      (error "Name must not be empty!"))
  (let ((new_key (prin1-to-string _key)))
  (let ((new-compilation-bookmark `((name . ,_name) (command . ,_command) (directory . ,_dir) (key . ,new_key))))
    (add-to-list 'compilation-bookmarks new-compilation-bookmark)
    (compilation-bookmarks-add-compile-keybinding new-compilation-bookmark)
    (compilation-bookmarks-add-compile-once-keybinding new-compilation-bookmark))))


(defun compilation-bookmarks-remove-bookmark ()
  "Remove bookmark from collection"
  (interactive)
  (let ((to-delete (compilation-bookmarks-get-bookmark-interactive)))
    (compilation-bookmarks-remove-compile-keybinding to-delete)
    (compilation-bookmarks-remove-compile-once-keybinding to-delete)
    (setq compilation-bookmarks (delete to-delete compilation-bookmarks))))


(defun compilation-bookmarks-change-bookmark (&optional name)
  "Change settings of bookmark"
  (interactive)
  (if (equal name nil)
      (setq name (compilation-bookmarks-get-names-interactive)))
  (let ((to-change (compilation-bookmarks-get-bookmark name)))

    (let ((new-dir (completing-read "New directory to use: " nil nil (cbm-get-directory to-change)))
          (new-cmd (completing-read "New command to use: " nil nil (cbm-get-command to-change)))
          (new-name (completing-read "New name to use: " nil nil (cbm-get-name to-change)))
          (new-key (completing-read "New key to use: " nil nil (cbm-get-key to-change))))

      (setcdr (assq 'directory to-change) new-dir)
      (setcdr (assq 'command to-change) new-cmd)
      (setcdr (assq 'name to-change) new-name)
      (setcdr (assq 'key to-change) new-key)))
  ;; TODO change binding
  )


(defun compilation-bookmarks-delete-compilation-buffer ()
  "deletes the compilation buffer"
  (if (get-buffer "*compilation*") ; If old compilation window exists
      (progn
        (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
        (kill-buffer "*compilation*"))))


(defun compilation-bookmarks-compilation-once (&optional name)
  "Use the compilation bookmark just once"
  (interactive)
  (if (equal name nil)
      (setq name (compilation-bookmarks-get-names-interactive)))
  (let ((bm (compilation-bookmarks-get-bookmark name)))
    (let ((default-directory (cbm-get-directory bm))
          (compilation-read-command nil))
    (setq compilation-bookmarks-require-recompilation t)
    (compile (cbm-get-command bm)))))


(defun compilation-bookmarks-compile (&optional name)
  "Set compilation settings to the current compilation bookmark"
  (interactive)
  (compilation-bookmarks-delete-compilation-buffer)
  (if compilation-bookmarks-require-recompilation
      (progn
        (setq compilation-bookmarks-require-recompilation nil)
        (let ((default-directory (cbm-get-directory compilation-bookmarks-active-bookmark))
              (compilation-read-command nil))
          (compile (cbm-get-directory compilation-bookmarks-active-bookmark))))
    ;; new compilation -> ask for bookmark
    (progn
      (if (equal name nil)
          (setq name (compilation-bookmarks-get-names-interactive)))
      (let ((bookmark-selection (compilation-bookmarks-get-bookmark name)))
        (setq compilation-bookmarks-active-bookmark bookmark-selection)
        (let ((default-directory (cbm-get-directory compilation-bookmarks-active-bookmark))
              (compilation-read-command t))
          (compile (cbm-get-command compilation-bookmarks-active-bookmark)))))))


(defun compilation-bookmarks-recompile ()
  "Looks for compilation buffer to recompile or starts a new compilation"
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compilation window exists
        (progn
          (call-interactively 'recompile))
      (compilation-bookmarks-compile))))


(require 'recentf)

(defun compilation-bookmarks-save-bookmarks ()
  "Stores compilation-bookmarks in `compilation-bookmarks-save-file`"
  (interactive)
  (with-temp-buffer
    (erase-buffer)
    (set-buffer-file-coding-system 'emacs-mule)
    (recentf-dump-variable 'compilation-bookmarks)
    (write-file (expand-file-name compilation-bookmarks-save-file))))


(defun compilation-bookmarks-load-bookmarks ()
  "Load compilation-bookmarks from `compilation-bookmarks-save-file`"
  (interactive)
  (if (file-exists-p compilation-bookmarks-save-file)
      (load compilation-bookmarks-save-file)))


(define-minor-mode compilation-bookmarks-mode
  "Collection of compilation bookmarks"
  :global t
  :keymap compilation-bookmarks-map

  (if compilation-bookmarks-mode
      (progn
        (compilation-bookmarks-load-bookmarks)
        (add-hook 'kill-emacs-hook 'compilation-bookmarks-save-bookmarks)
        (compilation-bookmarks-set-keybindings))

    (progn
      (compilation-bookmarks-save-bookmarks)
      (setq compilation-bookmarks nil)
      (remove-hook 'kill-emacs-hook 'compile-bookmarks-save-bookmarks))))

(provide 'compilation-bookmarks-mode)
