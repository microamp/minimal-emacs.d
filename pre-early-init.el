(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage %s."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done
           (if (= gcs-done 1) "collection" "collections")))

(add-hook 'emacs-startup-hook #'display-startup-time 100)
