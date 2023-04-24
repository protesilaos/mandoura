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

(require 'dired)
(require 'json)

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

(defcustom mandoura-saved-playlist-directory nil
  "File system path where saved playlists are stored.
A playlist is a file that contains paths to media files.  It does
not need an extension to be readable by mpv."
  :type '(file :must-match t)
  :group 'mandoura)

(defvar mandoura--process-name "mandoura"
  "Name of process made by `mandoura-play-files' or related.")

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

(defvar mandoura--mpv-socket nil
  "Last value of the mpv socket.")

(defun mandoura--return-mpv-socket ()
  "Return mpv socket."
  (or mandoura--mpv-socket
      (setq mandoura--mpv-socket (make-temp-file "mandoura-mpv-socket"))))

(defun mandoura-with-args (file &rest args)
  "Use mpv to play back FILE with ARGS.

ARGS is a list of strings.  If ARGS is nil use
`mandoura-default-args'.

Regardless of ARGS, always start mpv with --input-ipc-server."
  `("mpv"
    ,(format "--playlist=%s" file)
    ,@(if args args mandoura-default-args)
    ,(format "--input-ipc-server=%s" (mandoura--return-mpv-socket))))

(defvar mandoura-last-playlist nil
  "Last playlist file.")

(defun mandoura--return-playlist ()
  "Return a new temporaty file or prompt for the previous one."
  (if (and mandoura-last-playlist
           (file-exists-p mandoura-last-playlist)
           (yes-or-no-p "Playlist exists; replay it?"))
      mandoura-last-playlist
    (make-temp-file mandoura-playlist-file-base)))

;;;###autoload
(defun mandoura-play-files (files)
  "Create a playlist out of FILES and play it with mpv.

When called interactively, FILES is either the file at point in a
Dired buffer, or the marked files.  Directories are covered as
well.  In Lisp, FILES is a list of strings representing file
system paths.

FILES are compiled into a single list that is store in
`mandoura-last-playlist'.  That is a temporary file.  It is
located at the value of the variable `temporary-file-directory'
with a base name of `mandoura-playlist-file-base'.

If the playlist exists as a `mandoura-last-playlist' file, prompt
the user to replay it.  Else create a new temporary file."
  (interactive (list (dired-get-marked-files)))
  (unless (executable-find "mpv")
    (error "Cannot find mpv executable; aborting"))
  (mandoura-kill-running-process)
  (when-let* ((playlist (mandoura--return-playlist))
              (buf (find-file-noselect playlist)))
    (unless (equal playlist mandoura-last-playlist)
      (with-current-buffer buf
        (erase-buffer)
        (insert (mapconcat #'identity files "\n"))
        (save-buffer)))
    (when (make-process
           :name "mandoura"
           :buffer (get-buffer-create "*mandoura*")
           :command (mandoura-with-args playlist)
           :stderr (get-buffer-create "*mandoura-error-output*"))
      (setq mandoura-last-playlist playlist))))

(defun mandoura--return-files (dir)
  "Return list of files from DIR."
  (directory-files dir :full-path nil :no-sort))

(defun mandoura-playlist-prompt ()
  "Prompt for playlist file in `mandoura-saved-playlist-directory'."
  (when-let* ((dir mandoura-saved-playlist-directory)
              ((file-exists-p dir))
              ((file-directory-p dir)))
    (completing-read "Select playlist file: "
                     (mandoura--return-files dir)
                     nil
                     :require-match)))

;;;###autoload
(defun mandoura-play-playlist (playlist)
  "Like `mandoura-play-files' but with given PLAYLIST file."
  ;; (declare (interactive-only t))
  (interactive (list (mandoura-playlist-prompt)))
  (unless (executable-find "mpv")
    (error "Cannot find mpv executable; aborting"))
  (mandoura-kill-running-process)
  (make-process
   :name "mandoura"
   :buffer (get-buffer-create "*mandoura*")
   :command (mandoura-with-args playlist)
   :stderr (get-buffer-create "*mandoura-error-output*"))
  (setq mandoura-last-playlist playlist))

;;;; Communicate with the socket (--input-ipc-server)

(defun mandoura--get-from-mpv-socket (property)
  "Get PROPERTY from `mandoura--return-mpv-socket'."
  (unless (executable-find "socat")
    (error "Cannot find `socat'; aborting"))
  (shell-command-to-string
   (format
    "echo '{ %S: [%S, %S] }' | socat - %s"
    "command"
    "get_property"
    property
    (mandoura--return-mpv-socket))))

(defun mandoura--get-json-data (json)
  "Get `:data' from plist returned by JSON string."
  (let ((json-object-type 'plist))
    (plist-get (json-read-from-string json) :data)))

;; TODO 2023-04-06: How to convert seconds to minutes?
(defvar mandoura--mpv-properties
  '(("media-title" . "Title of current item")
    ("duration" . "Duration of current item")
    ("path" . "File system path")
    ("filename" . "File name")
    ("time-pos" . "Time position")
    ;; TODO 2023-04-24: Artist name
    ("time-remaining" . "Time remaining"))
  "Selection of mpv properties and their descriptions for use in Mandoura prompts.")

(defun mandoura--property-annotation (candidate)
  "Completion annotation function for CANDIDATE in `mandoura--mpv-properties'."
  (format " -- %s" (alist-get candidate mandoura--mpv-properties "" nil #'equal)))

(defun mandoura--property-prompt ()
  "Prompt for an mpv property."
  (let ((completion-extra-properties `(:annotation-function ,#'mandoura--property-annotation)))
    (completing-read "Select mpv property: " mandoura--mpv-properties)))

(defun mandoura-return-data (property)
  "Return data from the mpv socket matching PROPERTY."
  (interactive (list (mandoura--property-prompt)))
  (message "%s"
           (mandoura--get-json-data
            (mandoura--get-from-mpv-socket property))))

(provide 'mandoura)
;;; mandoura.el ends here
