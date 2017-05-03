;;; package --- Hydra templates

;;; Commentary:
;;; Hydras for enhanced command interaction.

;; :color meanings
"
|----------+-----------+-----------------------+-----------------|
| Body     | Head      | Executing NON-HEADS   | Executing HEADS |
| Color    | Inherited |                       |                 |
|          | Color     |                       |                 |
|----------+-----------+-----------------------+-----------------|
| amaranth | red       | Disallow and Continue | Continue        |
| teal     | blue      | Disallow and Continue | Quit            |
| pink     | red       | Allow and Continue    | Continue        |
| red      | red       | Allow and Quit        | Continue        |
| blue     | blue      | Allow and Quit        | Quit            |
|----------+-----------+-----------------------+-----------------|
"

;;; Code:

;; Helpers
(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers ()
  "Kill buffers that are not in view."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

;; Hydras
(defhydra hydra-kill-buffer
  ;; Implicit /body
  ;; See color chart for meaning.
  (:color teal
	  :columns 5)
  "Hydra Kill"
  ("k" kill-this-buffer "Kill Current Buffer")
  ("c" kill-buffer "Kill Buffer Cycle")
  ("a" kill-all-buffers "Kill All Buffers")
  ("o" kill-other-buffers "Kill Other Buffers")
  ("q" nil "Quit"))

(defhydra hydra-projectile (:color blue
				   :columns 4)
  "Hydra Projectile"
  ("a"   counsel-projectile-ag               "ag")
  ("f"   counsel-projectile-find-file        "Find File")
  ;; ("e"   projectile-recentf                  "Recent Files")
  ;; ("z"   projectile-cache-current-file       "Cache Current File")

  ("d"   counsel-projectile-find-dir         "Find Directory")
  ("b"   counsel-projectile-switch-to-buffer "Switch to Buffer")
  ;; ("c"   projectile-invalidate-cache         "Clear Cache")
  ;; ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

  ("o"   projectile-multi-occur              "Multi Occur")
  ("p"   counsel-projectile-switch-project   "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers")
  ("q"   nil                                 "Quit" :color blue))

(provide 'hydras)
;;; hydras.el ends here
