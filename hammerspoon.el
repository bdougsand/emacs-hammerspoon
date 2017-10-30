(require 'json)


(defcustom hammerspoon-cl-exec "hs"
  "The name of the Hammerspoon command line utility")

(defvar hammerspoon-port-name "emacsInterop")

(defvar hammerspoon--subprocess nil)

(setq hammerspoon--subprocess
      (start-process "hammerspoon" "*hammerspoon*" hammerspoon-cl-exec "-nq" "-m" hammerspoon-port))

(defun hammerspoon--receive-json (object)
  ;; TODO More interesting handling of input
  (print object)
  ;; TODO Run receive hooks
  )

(defun hammerspoon-connect ()
  (let ((proc (start-process "hammerspoon" "*hammerspoon*"
                             hammerspoon-cl-exec
                             "-n" "-q"
                             "-m" "emacsInterop"))
        (initialized nil)
        ;; keeps track of the furthest point successfully parsed
        (parse-marker (make-marker)))
    (set-marker parse-marker 0 (process-buffer proc))
    (setq hammerspoon--subprocess proc)
    (set-process-filter proc
                        (lambda (proc string)
                          (with-current-buffer (process-buffer proc)
                            (search-forward ">")
                            (save-excursion
                              (goto-char (process-mark proc))
                              (insert string)
                              (set-marker (process-mark proc) (point))

                              ;; Now try to parse more JSON:
                              (goto-char parse-marker)
                              ;; Keep reading in JSON until we encounter a parse error
                              (while (condition-case err
                                         (progn
                                           (hammerspoon--receive-json (json-read))
                                           (set-marker parse-marker (point))
                                           't)

                                       (json-end-of-file
                                        (goto-char parse-marker)
                                        false)))))))))

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
  (process-send-string (hammerspoon--get-or-connect) (concat message "\n")))

(defun hammerspoon--send-json (object)
  (hammerspoon--send (json-encode object)))

(defun hammerspoon-quit ()
  (hammerspoon--cleanup-process))

(hammerspoon-connect)
(hammerspoon--send-json '(:hello "world"))
;; (hammerspoon--send "really long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long messagereally long")


;; (add-hook 'org-pomodoro-started-hook
;;           (lambda () (hammerspoon)))

;; (with-current-buffer (process-buffer hammerspoon--subprocess)
;;   (goto-char 0)
;;   (goto-char (search-forward ">"))
;;   (print (point)))

;; (with-current-buffer (process-buffer hammerspoon--subprocess)
;;   (condition-case err
;;       (json-read)
;;     (json-end-of-file (message "okay, wait for more!"))))

