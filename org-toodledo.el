;;; org-toodledo.el - Toodledo integration for Emacs Org mode
;;
;; (c) 2011 Christopher J. White (cjwhite)
;; GNU General Public License v2 (GNU GPL v2),
;; inspired by work from Sacha Chua
;;
;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary
;;
;; This package is adds the ability to sync org-mode tasks with
;; Toodledo, a powerful web-based todo list manager that welcomes 3rd
;; party integrations.  (See http://www.toodledo.com/)
;;
;; This version of `org-toodledo' utilizes version 2.0 of the Toodledo API. 
;;
;; INSTALLATION
;; ------------
;;
;; 1. Required emacs packages:
;;      * `w3m' or `w3mexcerpt' -- see Notes below
;;      * `http-post-simple' -- http://www.emacswiki.org/emacs/http-post-simple.el
;;
;; 2. Put this file in your load path, byte compile the file for best
;;    performance, see `byte-compile-file'.
;;
;; 3. Put the following in your .emacs:
;;
;;    (push "<path-to-this-file>" load-path)
;;    (require 'org-toodledo)
;;    (setq org-toodledo-userid "<toodledo-userid>")      << *NOT* your email!
;;    (setq org-toodledo-password "<toodled-password>")
;;
;;    ;; Useful key bindings for org-mode
;;    (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-unset-key "\C-o")
;;             (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
;;             (local-set-key "\C-os" 'org-toodledo-sync)
;;             )
;;           )
;;
;; SYNCHRONIZING FOR THE FIRST TIME
;; --------------------------------
;;
;; The first step in using org-toodledo is to initialize a file and
;; synchronize tasks.  Simply create a new file, change the mode to
;; `org-mode', then call `org-toodledo-initialize'.  This will create
;; a new heading called "TASKS" (by default) and will import all
;; non-deleted tasks from Toodledo as sub-headings beneath "TASKS".
;;
;; If you already have an existing list of tasks in org file, open the
;; org file first.  Move the cursor to the headling where you want
;; imported tasks from Toodledo to be inserted into the buffer.  Call
;; `org-toodledo-initialize'.  This will import all tasks from the
;; server as well as pushing existing tasks in the org file back to
;; the server.
;; 
;; Once an org-file has been initialized, the heading selected will
;; be given a few Toodledo specific properties that are used to track
;; the status of synchronization:
;;
;;   * TASKS 
;;     :PROPERTIES:
;;     :Toodledo-lastsync: 1315343842
;;     :Toodledo-lastedit_task: 1315337478
;;     :Toodledo-lastdelete_task: 1314972230
;;     :END:
;;
;; This is referred to as the 'base Toodledo entry'.
;;
;; SYNCHRONIZING TASKS
;; -------------------
;;
;; The local org-file can be synchronized with the server at any time
;; by calling `org-toodledo-sync'.  When called, the following steps
;; are performed:
;; 
;;   1. Tasks added to the server since the last sync are downloaded
;;      and inserted as sub-headings to the Toodledo base heading (has
;;      the `Toodledo-lastsync' property)
;;
;;   2. Tasks modified on the server are compared against the local
;;      copy.  If the local copy was not modified since the last sync,
;;      the local copy is updated.  If local copy was modified, the
;;      server copy is inserted *after* the local copy as a duplicate.
;;      The user must manually merge any changes
;;
;;   3. Tasks deleted on the server are removed entirely from the
;;      local org file.
;;
;;   4. Tasks modified locally are pushed to the server as edits.
;;
;;   5. Tasks created and not yet prseent on the server are pushed as
;;      new tasks.
;;
;;   6. Tasks marked for deletion are deleted from the server, and
;;      then purged from the local file.
;;
;; Changes to tasks are automatically detected by computing a hash of
;; the task fields.  This hash is computed and saved as a property of
;; the task on sync.  When the next sync occurs, the hash value is
;; compared and if it differs, the task is considered modified.  This
;; eliminates the need for the user to mark tasks as modified or
;; remembere which tasks have changed -- it's all automatic!
;;
;; Note that `org-toodledo-sync' scans the entire file for tasks, not
;; just subheadings of the base entry.
;;
;; ADDING NEW TASKS
;; ----------------
;;
;; To add a new task on the server, just create a new headline
;; anywhere in the org file and give the headline a TODO keyword.
;; When ready, call `org-toodledo-sync' to push new tasks to the
;; server.
;;
;; DELETING TASKS
;; --------------
;;
;; Tasks cannot simply be killed from the org-file like text if the
;; were already synced with the server since they will just come back
;; the next time `org-toodledo-sync' is called.  Instead, they must be
;; marked as deleted by calling `org-toodledo-mark-task-deleted'.  Call
;; this function from any point within the task.  At the next sync, 
;; the task will be deleted from the server and then killed from the 
;; local file.
;;
;; Note that it may not be necessary to delete tasks in this way.  Instead
;; complete the task and let Toodledo archive completed tasks.
;;
;; TOODLEDO FIELDS
;; ---------------
;;
;; The table lists the possible Toodledo fields and how they are
;; mapped to org-mode style tasks:
;;
;; | Toodledo Field | Org-mode                | Comments                                                  |
;; | id             | Property :Toodledo-ID:  | If present, this task was previoiusly synced              |
;; | title          | Heading                 | The one line heading minus TODO state, priority and tags  |
;; | status         | TODO state              | See `org-toodledo-status-to-org-map'                      |
;; | startdate      | SCHEDULED               |                                                           |
;; | duedate        | DEADLINE                |                                                           |
;; | completed      | CLOSED                  | Timestamp when the task was marked completed              |
;; | repeat         | Repeat interval         |                                                           |
;; | repeatfrom     |                         |                                                           |
;; | context        | Tag                     | Context string "Work" becomes a tag :@Work:               |
;; | modified       | Property :Modified:     | Timestamp when this task was last modifed (set by server) |
;; | folder         | Property :Folder:       |                                                           |
;; | goal           | Property :Goal:         |                                                           |
;; | priority       | Priority                | 3=>A, 2=>B, 1=>C, -1,0 => D                               |
;; | note           | Body                    | Body of the task minus the properties                     |
;; | length         | Effort                  |                                                           |
;;
;; TODO STATES
;; -----------
;;
;; The TODO states from Toodledo are mapped to org-mode states via the
;; `org-toodledo-status-to-org-map' alist.   This can be customized to
;; choose your own TODO states, but all 10 states from Toodledo should
;; be mapped, even if only a subset are used in org-mode.
;;
;; In order to cycle through all the states recognized by Toodledo,
;; put a line like the following somewhere in your org file:
;;
;;   #+SEQ_TODO: TODO(t) DELEGATED(g) SOMEDAY(s) WAITING(w) | DONE(d) CANCELLED(c) REFERENCE(r) 
;;
;; CONTEXTS
;; --------
;;
;; Toodledo 'Contexts' allow you to split tasks into contexts such as
;; Work and Home.  Contexts are mapped to org tags with the '@' keyword,
;; :@Work: and :@Home:.
;;
;; Currently only contexts already on the server are recognized.  Setting
;; the task context of :@Phone: when Phone is not a valid context will 
;; loose the context.
;; 
;; MISCELLANEOUS NOTES
;; -------------------
;;
;;  - Doesn't do lots of error trapping. Might be a good idea to
;;    version-control your Org file.
;;
;;  - Verify handling of other tags that are not context
;;  
;;  - The body of a task is stored as the Toodledo note.  May get
;;    confused by asterisks, so don't use any starting asterisks in
;;    your body text.  (or anything that looks like an Org headline).
;;
;;  - w3mexcerpt.el inlcudes things needed things from w3m (since w3m
;;    requires things which require things which require things which
;;    require an executable which is no longer readily
;;    available.). (sachac)
;;
;;  - By default, save will ask to sync with Toodledo.  This can
;;    behavior can be changed via `org-toodledo-sync-on-save'.
;;
;; FUTURE WORK
;; -----------
;;
;; ** TODO Feature Requests: highest priority at top
;; 
;; [ ] Properly create contexts that are missing on the server. (cjwhite)
;;
;; [ ] It'd be great to allow notes to contain asterisks.  Make
;;     "[CR]** " the special key?  I use multiple asterisks all the
;;     time in notes.  (stophlong)
;; 
;; [ ] access to toodledo via proxy would also be good for those
;;     inside proxy based firewalls. (stophlong)
;; 
;; [ ] How to deal with sub-tasks? the paid version of toodledo (which
;;     I don't have) has sub-tasks.  At some point, might try to deal
;;     with those. (stophlong)
;; 
;; [ ] http-post-simple is used to post JSON-encoded data in the body.
;;     HTTPS is used for getting a token, but couldn't get HTTPS
;;     working for synchronizing, not sure why. (cjwhite)
;;
;; [ ] Better handling of 'duplicate' tasks -- those modified locally and
;;     on the server.  Perhaps tag them with a property and then on sync
;;     check for them asking the user to resolve. (cjwhite)
;;
;; [ ] Add a 'purge-completed-tasks' function -- once these tasks have
;;     been synced to the server, kill them locally (since they are
;;     backed up on toodledo).  Alternatively, move them to an archive
;;     file.  (cjwhite)
;;
;; [ ] Option to restrict synchronization to just tasks under the the
;;     base Toodledo entry.  (cjwhite)
;;
;; [ ] Support tasks across all agenda files.  (cjwhite)
;;
;; CHANGES
;; -------
;;
;; 2011-09-07  (cjwhite)
;;     First release for general distribution based on API 2.0
;;

;;; Code:

(require 'org)
(unless (require 'w3m nil t)
  (require 'w3mexcerpt))

(require 'xml)
(require 'json)
(require 'http-post-simple)
(require 'url)
(require 'url-http)

;;
;; User customizable variables
;;

(defcustom org-toodledo-userid ""
  "UserID from Toodledo (not your e-mail address): http://www.toodledo.com/info/api_doc.php"
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-password ""
  "Password for Toodledo."
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-sync-on-save "ask"
  "Action on save of a orgfile with toodledo tasks in it:
     no    - nothing
     ask   - ask the user to sync
     yes   - always sync"
  :group 'org-toodledo
  :type 'string
  )

(defcustom org-toodledo-status-to-org-map
  '(
    ("Active" . "TODO")
    ("None" . "TODO")
    ("Next Action" . "TODO")
    ("Planning" . "TODO")
    ("Delegated" . "DELEGATED")
    ("Waiting" . "WAITING")
    ("Someday" . "SOMEDAY")
    ("Hold" . "SOMEDAY")
    ("Postponed" . "SOMEDAY")
    ("Canceled" . "CANCELED")
    ("Reference" . "REFERENCE")
    )
  "Map of Toodledo API 'status' names to org-mode TODO states."
  :group 'org-toodledo
  :type '(alist :key-type string :value-type string)
  )

;;
;; Internal variables for tracking org-toodledo state
;;
(defvar org-toodledo-token-expiry nil "Expiry time for authentication token.")
(defvar org-toodledo-token nil "Authentication token.")
(defvar org-toodledo-key nil "Authentication key.")

;; Registered application ID and token for Toodledo API 2.0
(defconst org-toodledo-appid "orgtoodledo2" "Toodledo registered appid for API 2.0")
(defconst org-toodledo-apptoken "api4e4fbf7454eeb" "Toodledo apptoken associated with appid for API 2.0")

(defconst org-toodledo-fields 
  '( 
    ;; Toodledo recongized fields
    "id" "title" "status" "completed" "repeat" "repeatfrom" "context" "duedate" 
    "startdate" "modified" "folder" "goal" "priority" "note" "length" 
    ;; org-toodledo only fields
    "sync" "hash")
  "All fields related to a task"
  )

(defconst org-toodledo-fields-dont-ask
  '( 
    ;; Fields that toodled always returns, thus cannot be asked for
    "id" "title" "modified" "completed" 
    ;; org-toodledo only fields
     "sync" "hash")
  "Fields that must not be asked for from the server, either because the server
returns them automatically, or because they are internal only fields"
  )

(defconst org-toodledo-fields-dont-send
  '( 
    ;; Toodledo automatically sets modified, so don't attempt to push it
    "modified" 
    ;; org-toodledo only fields
    "sync" "hash")
  "Fields that shouldn't be sent to the server"
  )

(defconst org-toodledo-hash-fields 
  '( "title" "status" "completed" "repeat" "repeatfrom" "context" "duedate" "startdate"
     "folder" "goal" "priority" "note" "length")
  "Fields that are used to compute the hash of a task for detecting when a task changed."
  )

(defvar org-toodledo-fields-ask
  (remove nil (mapcar (lambda (f) (if (member f org-toodledo-fields-dont-ask) nil f)) org-toodledo-fields))
  "Fields that can be asked for (fields minus org-toodldeo-fields-dont-ask)"
  )

(defvar org-toodledo-fields-send
  (remove nil (mapcar (lambda (f) (if (member f org-toodledo-fields-dont-send) nil f)) org-toodledo-fields))
  "Fields that should be encoded and sent for new/modified tasks (fields minus org-toodled-fields-dont-send)"
  )

(defconst org-toodledo-api-status-map
  '(("0" . "None")
    ("1" . "Next Action")
    ("2" . "Active")
    ("3" . "Planning")
    ("4" . "Delegated")
    ("5" . "Waiting")
    ("6" . "Hold")
    ("7" . "Postponed")
    ("8" . "Someday")
    ("9" . "Canceled")
    ("10" . "Reference")
    )
  "Map of Toodledo API 'status' field values to names for easy reference.  The possible
values represent the keys for use in org-toodledo-status-to-org-map"
  )

(defvar org-toodledo-tmp-ref 1 
  "Temporary ID used to tag new tasks when synced in bulk to the server.  These ids 
should only be used for the short period of time when a new task is ")


(defun org-toodledo-initialize ()
  "Setup current item in an org file with Toodledo tasks.  If not "
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (error "Toodledo initialization must be performed on an org-mode file"))

  (save-excursion
    (if (org-toodledo-find-base-entry t)
        (message "Org-toodled already initialized")
      (let (item)
        (condition-case nil
            (progn 
              (org-back-to-heading t)
              (setq item (read-from-minibuffer "Default heading for Toodledo tasks: " 
                                               (elt (org-heading-components) 4)))
              )
          (error 
           (setq item (read-from-minibuffer "Default heading for Toodledo tasks: " "TASKS"))))

        (when item
          (goto-char (point-min))
          (unless (re-search-forward (format "^\*+[ \t]* %s" (regexp-quote item)) nil t)
            (if (y-or-n-p (format "No heading found matching '%s', create? " item))
                (progn
                  (goto-char (point-min))
                  (insert (concat "* " item "\n"))
                  (forward-line -1)
                  )
              (error "Aborted")))

          (org-entry-put (point) "Toodledo-lastsync" "0")
          (org-toodledo-sync)
          (goto-char (point-min))
          (re-search-forward (format "^\*+[ \t]* %s" (regexp-quote item)))
          (org-overview)
          (org-content)
          )
        )
      )
    )
  )

;;
;; Token / key functions
;;

(defun org-toodledo-token-valid ()
  "Return if org-toodledo-token is both non-null and not expired."
  (and org-toodledo-token
       org-toodledo-token-expiry
       (time-less-p (current-time) org-toodledo-token-expiry)))

(defun org-toodledo-token ()
  "Retrieve authentication token valid for four hours.  This token is used for all 
interaction with the server.  If the token expires, a new token is automatically
retrieved. "
  (if (or (string= org-toodledo-userid "")
          (string= org-toodledo-password ""))
      (error "Please set 'org-toodledo-userid' and 'org-toodledo-password'"))

  (if (org-toodledo-token-valid)
      ;; Return cached token
      org-toodledo-token
    
    ;; Else retrieve a new token
    (let ((response
           (with-current-buffer
               (url-retrieve-synchronously
                (concat "https://api.toodledo.com/2/account/token.php?f=xml"
                        ";userid=" org-toodledo-userid
                        ";appid=" org-toodledo-appid
                        ";sig=" (md5 (concat org-toodledo-userid org-toodledo-apptoken))))
             (xml-parse-region (point-min) (point-max)))))
      (if (equal (car (car response)) 'error)
	  (progn
	    (setq org-toodledo-token nil
		  org-toodledo-key nil
		  org-toodledo-token-expiry nil)
	    (error "Could not log in to Toodledo: %s" (elt (car response) 2)))
	(setq org-toodledo-token
	      (elt (car response) 2))

        ;; Set the expiry time to 4 hours from now
        (setq org-toodledo-token-expiry
	      (seconds-to-time (+ (float-time) (* 60 60 4))))
        )
      org-toodledo-token)))

(defun org-toodledo-key ()
  "Return authentication key used for each request."
  (if (and (org-toodledo-token-valid)
           org-toodledo-key)
      ;; Return cached key
      org-toodledo-key
    ;; Recompute token and key
    (setq org-toodledo-key
          (md5 (concat (md5 org-toodledo-password)
                       org-toodledo-apptoken
                       (org-toodledo-token))))))

(defun org-toodledo-get-account-info ()
  "Return account information from server."
  (org-toodledo-convert-xml-result-to-alist
   (car (org-toodledo-call-method "account/get"))))

(defun org-toodledo-get-tasks (&optional params)
  "Retrieve tasks from server using PARAMS.
Return a list of task alists."
  (aput 'params "fields" (cons (mapconcat 'identity org-toodledo-fields-ask ",") 'plain))
  
  (mapcar
   'org-toodledo-convert-xml-result-to-alist
   (xml-get-children
    (car (org-toodledo-call-method "tasks/get" params))
    'task)))

(defun org-toodledo-get-deleted (&optional params)
  "Retrieve deleted tasks using PARAMS.
Return a list of task alists."
  (mapcar
   'org-toodledo-convert-xml-result-to-alist
   (xml-get-children
    (car (org-toodledo-call-method "tasks/deleted" params))
    'task)))

(defun org-toodledo-sync ()
  "Synchronize tasks with the server bidirectionally."
  (interactive)
  (save-excursion
    (let ((regexp (concat "^\\*+[ \t]+\\(" org-todo-regexp "\\)"))
          (account-info (org-toodledo-get-account-info))
          (columns-pos (if (and (boundp 'org-columns-begin-marker) org-columns-begin-marker)
                           (marker-position org-columns-begin-marker) nil))
          server-edit-tasks
          server-delete-tasks
          new-tasks
          edit-tasks
          delete-tasks
          (end nil) ;; Restrict to Toodledo Task heading only?  XXXCJ
          )
      (when columns-pos
        (org-columns-quit))

      ;; Check for edited tasks on the server
      (org-toodledo-find-base-entry)
      (let ((local-lastedit-task (or (org-entry-get (point) "Toodledo-lastedit_task") "0")) 
            (server-lastedit-task (cdr (assoc "lastedit_task" account-info)))
            params)
        (when (> (string-to-number server-lastedit-task)
                 (string-to-number local-lastedit-task))
          (aput 'params "modafter" local-lastedit-task) ;; limit to tasks edited since last sync
          (aput 'params "comp" "0")                  ;; only grab completed tasks
          (setq org-toodledo-last-server-edit-tasks-params params)
          (setq server-edit-tasks (org-toodledo-get-tasks params))
          (setq org-toodledo-last-server-edit-tasks server-edit-tasks)
          (mapc 'org-toodledo-process-task server-edit-tasks)
          )
        )
      
      ;; Check for deleted tasks on the server
      (org-toodledo-find-base-entry)
      (let ((local-lastdelete-task (or (org-entry-get (point) "Toodledo-lastdelete_task") "0")) 
            (server-lastdelete-task (cdr (assoc "lastdelete_task" account-info)))
            params)
        (when (> (string-to-number server-lastdelete-task)
                 (string-to-number local-lastdelete-task))
          (aput 'params "after" local-lastdelete-task) ;; limit to tasks deleted since last sync
          (setq server-delete-tasks (org-toodledo-get-deleted params))
          (mapc (lambda (task) (org-toodledo-delete-local-task (org-toodledo-task-id task))) server-delete-tasks)
          )
        )


      ;; Iterate overall TODO items in the buffer -- any item matching the todo-regexp
      (goto-char (point-min))
      (while (re-search-forward regexp end t)
        (let* ((task (org-toodledo-parse-current-task))
               (modified (org-toodledo-task-modified task))
               (sync (org-toodledo-task-sync task))
               (hash (org-entry-get (point) "Hash"))
               (computed-hash (org-toodledo-compute-hash))
               (deleted (org-entry-get (point) "Deleted"))
               )
          (cond 
           ;; Collect a "new" task
           ((null (org-toodledo-task-id task))
            ;; Assign a temporary id, send it to the server as "ref", it will be echoed 
            ;; back from the server result with a real toodledo-id
            (let ((tmp-ref (number-to-string (setq org-toodledo-tmp-ref (1+ org-toodledo-tmp-ref))))
                  (new-task (org-toodledo-limit-fields task))
                  )
              (org-entry-put (point) "Toodledo-tmp-ref" tmp-ref)
              (aput 'new-task "ref" tmp-ref)
              (setq new-tasks (append new-tasks (list new-task)))
              )
            )

           ;; Collect a "delete" task
           (deleted
            (setq delete-tasks (append delete-tasks (list (org-toodledo-task-id task)))))
           
           ;; Collect an "edit" task
           ((or (not (string= hash computed-hash))
                (> (string-to-number modified)
                   (string-to-number sync)))
            (when (org-toodledo-task-completed task)
              ;; XXXCJ - make sure completed is handled correctly:
              ;;   DONE state should set the CLOSED timestamp
              )
            
            (setq edit-tasks (append edit-tasks 
                                     (list (org-toodledo-limit-fields task))))

            )
           )
          )
        )

      ;; Issue a single call for new-tasks
      (when new-tasks
        (let ((result (org-toodledo-server-add-task
                       (list (cons "tasks" (json-encode-array new-tasks)))))
              )
          (dolist (m (cddar result))
            (let ((ref (caddr (assoc 'ref m)))
                  (id (caddr (assoc 'id m)))
                  (mod (caddr (assoc 'modified m))))
              (if (and ref (not (string= ref ""))
                       (org-toodledo-find-todo-entry ref nil "Toodledo-tmp-ref"))
                  (progn
                    (org-entry-put (point) "Toodledo-ID" id)
                    (org-entry-delete (point) "Toodledo-tmp-ref")
                    (org-entry-put (point) "Sync" (format "%d" (float-time)))
                    (org-entry-put (point) "Modified" mod)
                    (org-toodledo-compute-hash t)
                    (message "Successfully synced new task ID %s / ref %s" id ref)
                    )
                (message "Failed to update new task with reference %S with task ID %S" ref id))))))
      
      ;; Issue a single call for edit-tasks
      (when edit-tasks
        (let ((result (org-toodledo-server-edit-task
                       (list (cons "tasks" (json-encode-array edit-tasks)))))
              )
          (dolist (m (cddar result))
            (let ((id (caddr (assoc 'id m)))
                  (mod (caddr (assoc 'modified m))))
              (if (and id (not (string= id ""))
                       (org-toodledo-find-todo-entry id nil))
                  (progn
                    (org-entry-put (point) "Sync" (format "%d" (float-time))) ;
                    (org-entry-put (point) "Modified" mod)
                    (org-toodledo-compute-hash t)
                    (message "Successfully edited task ID %s" id)
                    )
                (message "Failed to update edited task ID %S" id)))        
            )
          )
        )
      
      ;; Issue a single call for delete-tasks
      (when delete-tasks
        (let ((result (org-toodledo-server-delete-task
                       (list (cons "tasks" (json-encode-array delete-tasks)))))
              )
          (dolist (m (cddar result))
            (let ((id (caddr m)))
              (if (and id (not (string= id ""))
                       (org-toodledo-find-todo-entry id nil))
                  (progn
                    (org-back-to-heading t)
                    (delete-region
                     (point)
                     (if (and (end-of-line)
                              (re-search-forward org-complex-heading-regexp nil t))
                         (match-beginning 0)
                       (org-end-of-subtree t t)
                       (point)))
                    (message "Successfully deleted task ID %s" id)
                    )
                (message "Failed to delete task ID %S" id)))
            )
          )
        )
      
      ;; Finally, update account info
      (org-toodledo-find-base-entry)
      (org-entry-put (point) "Toodledo-lastsync" (format "%.0f" (float-time)))
      (org-entry-put (point) "Toodledo-lastedit_task" (cdr (assoc "lastedit_task" account-info)))
      (org-entry-put (point) "Toodledo-lastdelete_task" (cdr (assoc "lastdelete_task" account-info)))

      (when columns-pos
        (goto-char columns-pos)
        (org-columns))

      (message (concat (format "Sync complete, %d changes: in [e%d d%d] out [n%d e%d d%d]"
                               (+ (length server-edit-tasks)
                                  (length server-delete-tasks)
                                  (length new-tasks)
                                  (length edit-tasks)
                                  (length delete-tasks))
                               (length server-edit-tasks)
                               (length server-delete-tasks)
                               (length new-tasks)
                               (length edit-tasks)
                               (length delete-tasks))))
      (when (> 0 (+ (length server-edit-tasks)
                                  (length server-delete-tasks)
                                  (length new-tasks)
                                  (length edit-tasks)
                                  (length delete-tasks)))
        (sleep-for 1))
      )
    )
  )

(defun org-toodledo-parse-current-task ()
  "Parse the org task at point and extract all toodledo related fields.  Retrun
an alist of the task fields."
  (save-excursion
    (org-back-to-heading t)
    (when (and (looking-at org-complex-heading-regexp)
               (match-string 2)) ;; the TODO keyword
      (let* (info
             (status (match-string-no-properties 2))
             (priority (match-string-no-properties 3))
             (title (match-string-no-properties 4))
             (tags (match-string-no-properties 5))
             (id (org-entry-get (point) "Toodledo-ID"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled (org-entry-get nil "SCHEDULED"))
             (closed (org-entry-get nil "CLOSED"))
             context)
        ;; (add-to-list 'info (cons "title" (match-string-no-properties 1)))
        (if id (add-to-list 'info (cons "id" id)))
        (if (and (string= status "DONE")
                 (null closed))
            (progn
              (org-add-planning-info 'closed (org-current-effective-time))
              (setq closed (org-entry-get nil "CLOSED"))))
          
        (when tags
          (setq tags
                (delq nil
                      (mapcar
                       (lambda (tag)
                         (if (> (length tag) 0)
                             (if (string-match (org-re "@\\([[:alnum:]_]+\\)") tag)
                                 (setq context (org-toodledo-context-to-id (match-string 1 tag)))
                               tag)))
                       (split-string tags ":")))))
        (setq info
              (list
               (cons "id" id)
               (cons "title" title)
               (cons "length" (org-entry-get (point) "Effort"))
               (cons "context" context) 
               (cons "tag" (mapconcat 'identity tags " "))
               (cons "completed" 
                     (if (equal status "DONE") 
                         (format "%.0f" (org-time-string-to-seconds closed)) "0"))
               (cons "modified" (org-entry-get (point) "Modified"))
               (cons "sync" (org-entry-get (point) "Sync"))
               (cons "status" (org-toodledo-map-status status))
               (cons "priority"
                     (cond
                      ((equal priority "[#A]") "3")
                      ((equal priority "[#B]") "2")
                      ((equal priority "[#C]") "1")
                      ((equal priority "[#D]") "0")
                      (t "2"))) ;; Force org-mode's no priority to be same as [#B] as is done in org-mode.
               (cons "note"
                     (org-toodledo-entry-note))))
        (when (org-entry-get nil "FOLDER")
          (setq info (cons (cons "folder" (org-toodledo-folder-to-id (org-entry-get nil "FOLDER"))) info)))
        (when (org-entry-get nil "GOAL")
          (setq info (cons (cons "goal" (org-toodledo-goal-to-id (org-entry-get nil "GOAL"))) info)))
        (when deadline
          (setq info (cons (cons "duedate" 
                                 (format "%.0f" (org-time-string-to-seconds deadline))) info))
          (let ((repeat (org-toodledo-org-to-repeat deadline)))
            (when repeat
              (setq info (cons (cons "repeat" (car repeat)) info))
              (setq info (cons (cons "repeatfrom" (cdr repeat)) info))
              )
            )
          )
        (when scheduled
          (setq info (cons (cons "startdate"
                                 (format "%.0f" (org-time-string-to-seconds scheduled))) info)))
        info))))

(defun org-toodledo-process-task (task)
  "Process TASK definition, comparing with all currently defined tasks.
  - if TASK is not yet known (by id), create a new task
  - if TASK is known but local copy is not modified, update the local task
  - if TASK is known and local copy was modified, insert TASK as a duplicate"
  (save-excursion
    (if (org-toodledo-find-todo-entry (org-toodledo-task-id task) t)

        ;; Found this entry already -- check local modified time vs server modified time
        (let* ((server-modified (org-toodledo-task-modified task))
               (local-modified (or (org-entry-get (point) "Modified") "0"))
               (local-lastsync (or (org-entry-get (point) "Sync") "0"))
               (hash (org-entry-get (point) "Hash"))
               (computed-hash (org-toodledo-compute-hash))
               (touched (or (not (string= hash computed-hash))
                            (> (string-to-number local-modified)
                               (string-to-number local-lastsync))))
               )
          (cond

           ;; Not touched locally, and server did modify it; delete and recreate
           ((and (not touched) 
                 (> (string-to-number server-modified) (string-to-number local-modified)))
            (delete-region (progn (org-back-to-heading t) (point))
                           (progn (goto-char (match-end 0))
                                  (if (re-search-forward org-complex-heading-regexp nil t)
                                      (goto-char (match-beginning 0))
                                    (org-end-of-subtree))))
            (org-toodledo-insert-new-task task (point))
            )
           
           ((and touched
                 (> (string-to-number server-modified) (string-to-number local-modified)))
            (message "Task %s was modified locally and on the server, both are saved" 
                     (org-toodledo-task-id task))
            (org-toodledo-insert-new-task task (point))
            ;; XXXCJ - how to communicate this "duplicate" back to the user?
            ;;   - on sync, give user a number of "dups"?
            ;;   - add a property or a tag indicating its a dup?
            ;;   - on sync, check for dups and don't sync until the user resolves all dups
            ;;   - have a special check for dups function, lets user pick one or the other or edit
            )
           )
          )

      ;; Not found, add as new
      (org-toodledo-insert-new-task task)
      )
    )
  )

(defun org-toodledo-insert-new-task (task &optional pos heading)
  (save-excursion
    (let ((heading-pos (if heading (org-find-exact-headline-in-buffer heading))))
      (if heading-pos
          (goto-char heading-pos)
        (org-toodledo-find-base-entry)
        (org-back-to-heading t)
        )
      )
    
    (let* ((components (org-heading-components))
           (level (1+ (elt components 0))))
      (if pos 
          (goto-char pos)
        (org-forward-same-level 1 t))
      (let* ((repeat (org-toodledo-repeat-to-org 
                      (org-toodledo-task-repeat task) (org-toodledo-task-repeatfrom task)))
             (priority (org-toodledo-task-priority task))
             (context (org-toodledo-task-context task))
             (note (org-toodledo-task-note task))
             (duedate (org-toodledo-task-duedate task))
             (startdate (org-toodledo-task-startdate task))
             )
        (insert (make-string (or level 2) ?*) " " )
        (setq pos (point-marker))
        (insert (concat
                 (org-toodledo-task-status-to-org task) " "
                 (cond
                  ((equal priority "-1") "[#D] ") 
                  ((equal priority "0")  "[#D] ")
                  ((equal priority "1")  "[#C] ") 
                  ((equal priority "2")  "[#B] ") 
                  ((equal priority "3")  "[#A] "))
                 (org-toodledo-task-title task)
                 (if (and context (not (equal context "0")))
                     (concat " :@" (org-toodledo-id-to-context context) ":") 
                   "")
                 "\n"))
        
        ;; note => becomes the task textual contents
        (if note
            (insert note "\n"))

        ;; duedate => "DEADLINE: <2011-08-21 Sun>" 
        ;; If a repeat string was found, it is added: "DEADLINE: <2011-08-21 Sun +1m>"
        (if (and duedate
                 (not (<= (string-to-number duedate) 0)))    
            (setq duedate (concat org-deadline-string " "
                                  (org-toodledo-format-date duedate repeat)))
          (setq duedate nil))
        
        ;; startdate => "SCHEDULED: <2011-08-21 Sun>" 
        ;; If a repeat string was found, it is added: "DEADLINE: <2011-08-21 Sun +1m>"
        (if (and startdate
                 (not (<= (string-to-number startdate) 0)))
            (setq startdate (concat (make-string (if duedate 1 (1+ (or level 2))) ? )
                                    org-scheduled-string " "
                                    (org-toodledo-format-date startdate repeat)))
          (setq startdate nil))
        
        (when (or duedate startdate)
          (insert (make-string (1+ (or level 2)) ? ))
          (if duedate (insert duedate))
          (if (and duedate startdate) (insert " "))
          (if startdate (insert startdate))
          (insert "\n"))

        ;; create a properties drawer for all details
        (goto-char pos)
        (org-entry-put (point) "Toodledo-ID" (org-toodledo-task-id task))
        (org-entry-put (point) "Modified" (org-toodledo-task-modified task))
        (if (and (not (equal (org-toodledo-task-folder task) "0"))
                 (not (equal (org-toodledo-task-folder task) "")))
            (org-entry-put (point) "Folder" (car (rassoc (org-toodledo-task-folder task) org-toodledo-folders))))
        (if (and (not (equal (org-toodledo-task-goal task) "0"))
                 (not (equal (org-toodledo-task-goal task) "")))
            (org-entry-put (point) "Goal" (car (rassoc (org-toodledo-task-goal task) org-toodledo-folders))))
        (org-entry-put (point) "Sync" (format "%d" (float-time (current-time))))
        (org-entry-put (point) "Effort" (org-toodledo-task-length task))

        (org-toodledo-compute-hash t)
        )
      )
    )
  )

(defun org-toodledo-delete-local-task (id)
  "Delete the task text for ID from the current buffer.  This
does no interaction with the server.  This is primarily used when
notified that a task on th server was deleted.

In most cases org-toodledo-mark-task-deleted is more appropriate."

  (if (and id (not (string= id ""))
           (org-toodledo-find-todo-entry id t))
      (progn
        (org-back-to-heading t)
        (delete-region
         (point)
         (if (and (end-of-line)
                  (re-search-forward org-complex-heading-regexp nil t))
             (match-beginning 0)
           (org-end-of-subtree t t)
           (point)))
        )
    )
  )

(defun org-toodledo-mark-task-deleted ()
  "Marks the current task as deleted.  It will be deleted from the server
and from the local org file on the next sync"
  (interactive "")
  (save-excursion
    (let ((start-pos (point))
          (columns-pos (if (and (boundp 'org-columns-begin-marker) org-columns-begin-marker)
                           (marker-position org-columns-begin-marker) nil))
          )
      (when columns-pos
        (org-columns-quit))
      
      (org-back-to-heading t)
      (let ((task (org-toodledo-parse-current-task))
            response)
        (if (> (length (org-toodledo-task-id task)) 0)
            (org-entry-put (point) "Deleted" "1"))
        )
      
      (when columns-pos
        (goto-char columns-pos)
        (org-columns))
      
      (goto-char start-pos)
      )
    )
  )

;;
;; Field related functions
;;

;; Create a convenience function "org-toodled-task-<field>" for each field
;; of a task
(mapc (lambda (field)
        (eval `(defun ,(intern (concat "org-toodledo-task-" field)) (task)
                 ,(concat "Return the task property '" field "' for TASK")
                 (cdr (assoc ,field task)))))
      org-toodledo-fields)

(defun org-toodledo-limit-fields (task &optional fields)
  (unless fields
    (setq fields org-toodledo-fields-send))
  (let (new-task)
    (mapc (lambda (key) (let ((elem (assoc key task)))
                          (when elem (setq new-task (append (list elem) new-task))))) fields)
    new-task
    )
  )

(defun org-toodledo-entry-note ()
  "Extract the note for this task."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (goto-char (match-end 0))
      (let ((text (buffer-substring-no-properties
                   (point)
                   (if (re-search-forward org-complex-heading-regexp nil t)
                       (match-beginning 0)
                     (org-end-of-subtree)))))
        (with-temp-buffer
          (insert text)

          ;; Pull out DEADLINE / SCHEDULED / CLOSED fields
          (dolist (str (list (regexp-quote org-deadline-string)
                             (regexp-quote org-scheduled-string)
                             (regexp-quote org-closed-string)))
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "\\<" str " +[<\[][^]>\n]+[]>][ \t]*") nil t)
              (replace-match "")))

          ;; Drop any empty lines
          (goto-char (point-min))
          (while (re-search-forward "\n\n+" nil t)
            (replace-match "\n"))

          ;; org-export-remove-or-extract-drawers removed an argument sometime around version 7
          (if (>= (string-to-number org-version) 7)
              (org-export-remove-or-extract-drawers org-drawers nil)
            (org-export-remove-or-extract-drawers org-drawers nil nil))

          ;; Trim leading/trailing empty lines, but preserve whitepace at the beginning of the line
          (let ((s (buffer-substring-no-properties (point-min)
                                                   (point-max))))
            (if (string-match "\\(\\`[ \t]*[\n\r]+\\)+" s)  (setq s (replace-match "" t t s)))
            (if (string-match "\\([\n\r]+[ \t]*\\)+\\'" s) (setq s (replace-match "" t t s)))
            s)
          )
        )
      )
    )
  )

;;
;; Status related functions
;;

(defun org-toodledo-map-status (status &optional to-org)
  (cond 
   (to-org
    (if (string-match "^[0-9]+" status)
        (setq status (cdr (assoc status org-toodledo-api-status-map))))
    (cdr (assoc status org-toodledo-status-to-org-map)))
   
   ((string= status "DONE")
    "0")

   (t
    (car (rassoc 
          (car (rassoc status org-toodledo-status-to-org-map))
          org-toodledo-api-status-map)))
   )
  )

(defun org-toodledo-task-status-to-org (task)
  (let ((comp (org-toodledo-task-completed task))
        (status (org-toodledo-task-status task)))
    (cond
     ((not (or (null comp) (equal comp "") (equal comp "0"))) "DONE")
     (t (org-toodledo-map-status status t))
     )))

;;
;; Repeat parsing and translation (ie. every 1 month)
;;

;; (assert (equal (org-toodledo-repeat-to-org nil) ""))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 week") "+1w"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 month") "+1m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 year") "+1y"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 day") "+1d"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 2 weeks") "+2w"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 2 months") "+2m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 6 months") "+6m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 3 months") "+3m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 3 months" 1) ".+3m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 week" 1) ".+1w"))

(defun org-toodledo-repeat-to-org (repeat &optional from)
  "Turn REPEAT string into org-mode style repeat sequence.  The second
argument FROM indicates if the repeat is from the due-date (0) or 
from the completion date (1). 

The format for REPEAT must be of the form \"Every X T\". Where X
is a number and T is a unit of time (day/week/month/year).

Examples: Every 3 days, Every 1 month, Every 2 years, Every 16 weeks.

Note the Toodlde 2.0 API supports 2 additional formats which are
not supported by this code: \"On the X D of each month\", and
\"Every W\".
"
  (if (not from) (setq from 0))
  (when (stringp from) (setq from (string-to-number from)))
  (cond
   ((null repeat) 
    "")
   ((string-match "Every \\([0-9]+\\) day" repeat)
    (concat (if (= from 0) "+" ".+") (match-string 1 repeat) "d"))
   ((string-match "Every \\([0-9]+\\) week" repeat)
    (concat (if (= from 0) "+" ".+")  (match-string 1 repeat) "w"))
   ((string-match "Every \\([0-9]+\\) month" repeat)
    (concat (if (= from 0) "+" ".+")  (match-string 1 repeat) "m"))
   ((string-match "Every \\([0-9]+\\) year" repeat)
    (concat (if (= from 0) "+" ".+") (match-string 1 repeat) "y"))
   (t 
    (message "Unsupported repeat string format: %s" repeat)
    "")
   )
  )

(defun org-toodledo-org-to-repeat (string)
  "Extract org-mode style repeat information from STRING and return
as a Toodledo style string.  Return nil if STRING has no repeat information"
  (if (string-match "\\(\\.?\\)\\+\\([0-9]+\\)\\([wmdy]\\)" string)
      (cons
       (format "Every %s %s" (match-string 2 string)
               (let ((interval (match-string 3 string)))
                 (cond ((string= interval "d") "day")
                       ((string= interval "w") "week")
                       ((string= interval "m") "month")
                       ((string= interval "y") "year"))))
       (format "%d" (length (match-string 1 string))))
    nil)
  )


;;
;; Date Handling
;;

;; (assert (equal (org-toodledo-format-date "2003-08-12") "<2003-08-12 Tue>"))

(defun org-toodledo-format-date (date &optional repeat)
  "Return yyyy-mm-dd day for DATE."
  (concat
   "<"
   (format-time-string
    "%Y-%m-%d %a"
    (cond
     ((listp date) date)
     ((numberp date) (seconds-to-time date))
     ((and (stringp date)
           (string-match "^[0-9]+$" date))
      (seconds-to-time (string-to-number date)))
     (t (apply 'encode-time (org-parse-time-string date)))))
   (if repeat (concat " " repeat) "")
   ">"))

;;
;; Finding TODO tasks
;;

(defun org-toodledo-find-todo-entry (id &optional noerror prop)
  "Find entry with property PROP equal to ID.  If PROP is not specified, defaults
to Toodledo-ID"
  (goto-char (point-min))
  (unless prop (setq prop "Toodledo-ID"))
  (if (re-search-forward (concat "^[ \t]*:" prop ":[ \t]*" id) nil noerror)
      (org-back-to-heading t)
    nil)
  )

(defun org-toodledo-find-base-entry (&optional noerror)
  "Find base entry with 'Toodledo-lastsync' property."
  (goto-char (point-min))
  (if (re-search-forward "^[ \t]*:Toodledo-lastsync:" nil noerror)
      (org-back-to-heading t)
    nil )
  )

;;
;; Hash Function
;;
(defun org-toodledo-compute-hash (&optional update)
  "Compute an md5 hash of all user modifyable fields of the current task."
  (let* ((task (org-toodledo-parse-current-task))
         (text (mapconcat (lambda (field) (cdr (assoc field task))) org-toodledo-hash-fields ""))
         hash)
    (setq hash (md5 text))
    ;;(message "org-toodledo-compute-hash: %s from %s" hash text)
    (when update
      (org-entry-put (point) "Hash" hash))
    hash)
  )

;;
;; Save Hook
;;
(defun org-toodledo-save-hook ()
  "Save hook called before saving a file.  If this is an org-mode file and 
this file has been synced with Toodledo, check for saving.  

See org-toodledo-sync-on-save."  
  (when (and (eq major-mode 'org-mode)
             (org-toodledo-find-base-entry t))
    (save-excursion
      (let ((sync
             (cond 
              ((string= org-toodledo-sync-on-save "ask")
               (y-or-n-p "Sync with Toodledo? "))
              ((string= org-toodledo-sync-on-save "yes") t)
              (t nil))))
        (when sync
          (org-toodledo-sync))))))

(add-hook 'before-save-hook 'org-toodledo-save-hook)

;;
;; Miscellaneous
;;

(defmacro org-toodledo-defun (function-name api-name description)
  `(defun ,function-name (params)
     ,description
     (org-toodledo-call-method ,api-name params)))

(org-toodledo-defun org-toodledo-server-add-task "tasks/add" "Add task with PARAMS.")
(org-toodledo-defun org-toodledo-server-edit-task "tasks/edit" "Edit task with PARAMS.")
(org-toodledo-defun org-toodledo-server-delete-task "tasks/delete" "Delete task with PARAMS.")

(defun org-toodledo-call-method (method-name &optional params)
  "Call METHOD-NAME with PARAMS and return the parsed XML."

  (aput 'params "unix" (cons "1" 'plain))
  (aput 'params "key" (cons (org-toodledo-key) 'plain))
  (aput 'params "f" "xml")

  ;; Convert "unix" to 'unix
  (setq params (mapcar (lambda (e) 
                       (let ((key (intern (car e)))
                             (value (cdr e)))
                         (when (listp value)
                           (setq value (car value)))
                         (cons key value))) params))

  (let ((response (http-post-simple 
                   (concat  "http://api.toodledo.com/2/" method-name ".php")
                   params)))
    (if (not (eq 200 (caddr response)))
        (error (concat "Call to " method-name " failed"))
      (with-temp-buffer
        (insert (car response))
        (xml-parse-region (point-min) (point-max))))))

(defmacro org-toodledo-make-lookup-function (name)
  "Create a lookup function and caching functions for NAME.

  variable:  org-toodledo-NAMEs
  functions: org-toodledo-get-NAMEs
             org-toodledo-NAME-to-id
             org-toodledo-id-to-NAME
"
  (let ((cache-var (concat "org-toodledo-" name "s"))
        (get-func (concat "org-toodledo-get-" name "s"))
        (add-method (concat name "s/add"))
        (get-method (concat name "s/get")))
    (list
     'progn
     `(defvar ,(intern cache-var) nil)
     `(defun ,(intern get-func) (&optional force)
        ,(concat "Store an alist of (title . id) in `" cache-var "'.
Reload if FORCE is non-nil.")
        (if (or force (null ,(intern cache-var)))
            (setq ,(intern cache-var)
                  (mapcar
                   (lambda (node)
                     (cons
                      (caddar (xml-get-children node 'name)) (caddar (xml-get-children node 'id))))
                   (xml-get-children (car
                                      (org-toodledo-call-method ,get-method)) (quote ,(intern name)))))
          ,(intern cache-var)))
     `(defun ,(intern (concat "org-toodledo-" name "-to-id")) (item) 
        "Return numeric ID for CONTEXT, creating if necessary."
        (let ((lookups ,(list (intern get-func))))
          (if (null (assoc item lookups))
              ;; Create it if it does not yet exist
              (let ((result
                     (org-toodledo-call-method
                      ,add-method
                      (list (cons "title" item)))))
                (if (eq (caar result) 'added)
                    (setq ,(intern cache-var)
                          (cons (cons item
                                      (elt (car result) 2))
                                ,(intern cache-var))
                          lookups ,(intern cache-var)))))
          (cdr (assoc item lookups))))
     `(defun ,(intern (concat "org-toodledo-id-to-" name)) (id) 
        "Return name for context by ID."
        (let ((lookups ,(list (intern get-func))))
          (if (null (rassoc id lookups))
              nil
            (car (rassoc id lookups))))
        )
     )
    )
  )

(org-toodledo-make-lookup-function "context")
(org-toodledo-make-lookup-function "folder")
(org-toodledo-make-lookup-function "goal")

(defun org-toodledo-convert-xml-result-to-alist (info)
  "Convert INFO to an alist."
  (delq nil
        (mapcar
         (lambda (item)
           (if (listp item)
               (cons (symbol-name (car item)) (elt item 2))))
         (xml-node-children (delete "\n\t" info)))))

(provide 'org-toodledo)
