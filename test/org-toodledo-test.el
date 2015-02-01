(require 'ert)
(require 'org-toodledo)

(defun org-toodledo-test-setup-buffer (name)
  (let ((buf (get-buffer-create name)))
    (set-buffer buf)
    (erase-buffer)
    (when (not (eq major-mode 'org-mode))
      (org-mode))
    (insert "* TASKS\n
** TODO [#D] Test1            :@work:\n
    DEADLINE: <2015-02-07 土 +1m>\n
    :PROPERTIES:\n
    :ToodledoID: 393655887\n
    :Hash:     ad0d9f84a6a204e051925805181ed137\n
    :Parent-id:\n
    :LAST_REPEAT: [2015-01-14 水 16:52]\n
    :END:\n
    :LOGBOOK:\n
    - State \"DONE\"       from  \"TODO\"       [2015-01-14 水 16:52]\n
    - State \"DONE\"       from \"TODO\"       [2014-12-18 木 14:39]\n
    :LAST_REPEAT: [2014-12-18 木 14:39]\n
    :END:\n
** WAITING [#D] Test2  :@work:\n
     :PROPERTIES:\n
     :ToodledoID: 393773069\n
     :Hash:     3da632c2b88319569f648c35506cf0ba\n
     :Parent-id:\n
     :END:\n
     :LOGBOOK:\n
     - State \"WAITING\"    from \"WAITING\"    [2015-01-23 金 15:50]\n
     :END:\n
     - State \"WAITING\"    from \"TODO\"       [2015-01-23 金 15:50]\n")
    ;;(save-buffer)
))

(ert-deftest org-toodledo-initialize-test ()
<<<<<<< HEAD
  (let ((buf (get-buffer-create "*test*")))
    (set-buffer buf)
    (erase-buffer)
    (when (not (eq major-mode 'org-mode))
      (org-mode))
    (insert "* TASKS\n
** TODO [#D] Test1            :@work:\n
    DEADLINE: <2015-02-07 土 +1m>\n
    :PROPERTIES:\n
    :ToodledoID: 393655887\n
    :Hash:     ad0d9f84a6a204e051925805181ed137\n
    :Parent-id:\n
    :LAST_REPEAT: [2015-01-14 水 16:52]\n
    :END:\n
    :LOGBOOK:\n
    - State \"DONE\"       from  \"TODO\"       [2015-01-14 水 16:52]\n
    - State \"DONE\"       from \"TODO\"       [2014-12-18 木 14:39]\n
    :LAST_REPEAT: [2014-12-18 木 14:39]\n
    :END:\n
** WAITING [#D] Test2  :@work:\n
     :PROPERTIES:\n
     :ToodledoID: 393773069\n
     :Hash:     3da632c2b88319569f648c35506cf0ba\n
     :Parent-id:\n
     :END:\n
     :LOGBOOK:\n
     - State \"WAITING\"    from \"WAITING\"    [2015-01-23 金 15:50]\n
     :END:\n
     - State \"WAITING\"    from \"TODO\"       [2015-01-23 金 15:50]\n")
    (setq expected '(("duetime" . "0")
                     ("duedate" . "0")
                     ("starttime" . "0")
                     ("startdate" . "0")
                     ("repeatfrom" . "0")
                     ("repeat" . "")
                     ("goal" . "0")
                     ("folder" . "0")
                     ("id")
                     ("title" . "Test2")
                     ("length" . "0")
                     ("context" . "1200627")
                     ("tag" . "")
                     ("completed" . "0")
                     ("status" . "5")
                     ("priority" . "0")
                     ("note" . "- State \"WAITING\"    from \"TODO\"       [2015-01-23 金 15:50]")))
    (setq actual (org-toodledo-parse-current-task))
    (should (equal expected actual))))
=======
  (org-toodledo-test-setup-buffer "*test*")
  (setq expected '(("duetime" . "0")
                   ("duedate" . "0")
                   ("starttime" . "0")
                   ("startdate" . "0")
                   ("repeatfrom" . "0")
                   ("repeat" . "")
                   ("goal" . "0")
                   ("folder" . "0")
                   ("id")
                   ("title" . "Test2")
                   ("length" . "0")
                   ("context" . "1200627")
                   ("tag" . "")
                   ("completed" . "0")
                   ("status" . "5")
                   ("priority" . "0")
                   ("note" . "- State \"WAITING\"    from \"TODO\"       [2015-01-23 金 15:50]")))
  (setq actual (org-toodledo-parse-current-task))
  (should (equal (buffer-name (current-buffer)) "*test*")))
>>>>>>> parent of 2c21e66... Fix test

