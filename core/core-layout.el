;;+-----------------------~------------------------~---------------------+
;;|                        Emacs's ecb custom layout                     |
;;+-----------------------~------------------------~---------------------+
(ecb-layout-define "babel-custom-speedbar" left
  "This function creates the following layout by babel"
  (ecb-set-speedbar-buffer)
  (select-window (next-window)))

(provide 'core-layout)
