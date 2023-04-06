;;; mandoura.el --- WORK-IN-PROGRESS Use MPV to play media files via Dired -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/mandoura
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use MPV to play media files via Dired.  WORK-IN-PROGRESS.
;;
;; The name of this package is a reference to a Greek musical
;; instrument from the island of Crete.  Though according to the
;; Oracle of Delphi, it is a cryptic message for future generations:
;; "MPV Access Needs Dired to Output User's Requested Audio" (yes,
;; this is another one of those backronyms of mine).

;;; Code:

;; TODO 2023-04-06 06:27 +0300: Save playlist to user predefined path
;; TODO 2023-04-06 06:27 +0300: completion to select from saved playlists
;; TODO 2023-04-06 06:27 +0300: Optionally replay last playlist

(require 'dired)

(defgroup mandoura ()
  "Play audio or video files with mpv using Dired."
  :group 'convenience
  :group 'dired)

(defcustom mandoura-default-args
  '("--audio-display=no" "--loop-playlist=inf" "--shuffle")
  "List of strings representing flags for mpv.
Used as a fallback value for `mandoura-with-args'."
  :group 'mandoura
  :type '(repeat string))

(defvar mandoura--process-name "mandoura"
  "Name of process made by `mandoura-play-playlist' or related.")

(defun mandoura--running-process-p ()
  "Return `mandoura--process-name' or nil."
  (get-process mandoura--process-name))

(defun mandoura-kill-running-process ()
  "Prompt to kill running process, if present."
  (interactive)
  (when-let* ((process (mandoura--running-process-p))
              ((yes-or-no-p "Running process found; kill it to continue?")))
    (kill-process process)))

(defvar mandoura-playlist-file-base "mandoura-playlist-"
  "Base of temporary file name created by Mandoura commands.")

(defun mandoura-with-args (file &rest args)
  "Use mpv to play back FILE with ARGS.
ARGS is a list of strings."
  `("mpv"
    ,(format "--playlist=%s" file)
    ,@(if args args mandoura-default-args)))

(defvar mandoura-last-playlist nil
  "Last playlist file.")

;;;###autoload
(defun mandoura-play-playlist (files)
  "Create a playlist out of FILES and play it with mpv.

When called interactively, FILES is either the file at point in a
Dired buffer, or the marked files.  Directories are covered as
well.  In Lisp, FILES is a list of strings representing file
system paths.

The playlist is stored in the variable `temporary-file-directory'
with a base bame of `mandoura-playlist-file-base'."
  (interactive (list (dired-get-marked-files)))
  (unless (executable-find "mpv")
    (error "Cannot find mpv executable; aborting"))
  (mandoura-kill-running-process)
  (let* ((playlist (make-temp-file mandoura-playlist-file-base))
         (buf (find-file-noselect playlist)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (mapconcat #'identity files "\n"))
      (save-buffer))
    (when (make-process
           :name "mandoura"
           :buffer (get-buffer-create "*mandoura*")
           :command (mandoura-with-args playlist)
           :stderr (get-buffer-create "*mandoura-error-output*"))
      (setq mandoura-last-playlist playlist))))

(provide 'mandoura)
;;; mandoura.el ends here
