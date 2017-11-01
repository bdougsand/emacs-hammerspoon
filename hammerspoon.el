(require 'json)


(defcustom hammerspoon-cl-exec "hs"
  "The name of the Hammerspoon command line utility"
  :group 'hammerspoon)

(defcustom hammerspoon-port-name "emacsInterop"
  "The name of the remote port created by Hammerspoon."
  :group 'hammerspoon)

(defcustom hammerspoon-receive-hook nil
  "Hook run when emacs receives an object from Hammerspoon."
  :group 'hammerspoon
  :type 'hook)

(defvar hammerspoon--subprocess nil)

(defun hammerspoon--receive-json (object)
  ;; TODO More interesting handling of input
  (run-hook-with-args 'hammerspoon-receive-hook object))

(defvar hammerspoon--parse-marker (make-marker))

(defun hammerspoon--subprocess-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))

      ;; Now try to parse more JSON:
      (goto-char hammerspoon--parse-marker)
      (search-forward ">>" nil 't)
      ;; Keep reading in JSON until we encounter a parse error
      (condition-case err
          (progn
            (hammerspoon--receive-json (json-read))
            (set-marker hammerspoon--parse-marker (point-max)))

        (json-error nil)))))

(defun hammerspoon-connect ()
  (let ((proc (start-process "hammerspoon" "*hammerspoon*"
                             hammerspoon-cl-exec
                             "-n" "-q"
                             "-m" hammerspoon-port-name)))
    (setq hammerspoon--subprocess proc)
    (set-marker hammerspoon--parse-marker 0 (process-buffer proc))
    (set-process-filter proc 'hammerspoon--subprocess-filter)))

(defun hammerspoon--cleanup-process ()
  (when hammerspoon--subprocess
    (process-send-eof hammerspoon--subprocess)
    (kill-buffer (process-buffer hammerspoon--subprocess))))

(defun hammerspoon--get-or-connect ()
  (or (if (eq (process-status hammerspoon--subprocess) 'run)
          hammerspoon--subprocess
        (hammerspoon--cleanup-process))
      (hammerspoon--connect)))

(defun hammerspoon--send (message)
  (process-send-string hammerspoon--subprocess (concat message "\n")))

(defun hammerspoon--send-json (object)
  (hammerspoon--send (json-encode object)))

(defun hammerspoon-quit ()
  (hammerspoon--cleanup-process))

(defun hammerspoon--make-pomodoro-event (hook-symbol)
  (let ((event (make-hash-table)))
    (puthash :type (-> hook-symbol
                       (symbol-name)
                       (substring 4 -5)
                       (s-lower-camel-case))
              event)
    (puthash :time-remaining org-pomodoro-countdown event)
    (puthash :count org-pomodoro-count event)
    (puthash :hello "world")
    event))

(defun hammerspoon--attach-pomodoro-hook (hook-symbol)
  (add-hook hook-symbol (lambda () (hammerspoon--send-json
                                    (hammerspoon--make-pomodoro-event hook-symbol)))))

(hammerspoon-connect)
(add-hook 'hammerspoon-receive-hook (lambda (message)
                                      (print (concat "Got an object with response: "
                                                     (alist-get 'response message)))))

(with-eval-after-load 'org-pomodoro
  (--each '(org-pomodoro-started-hook
            org-pomodoro-finished-hook
            org-pomodoro-killed-hook
            org-pomodoro-break-finished-hook)
    (hammerspoon--attach-pomodoro-hook it)))

(provide 'hammerspoon)
