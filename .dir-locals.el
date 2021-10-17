((nil
  (fill-column . 78)
  (tab-width   .  8)
  (sentence-end-double-space . t)

  (eval . (with-eval-after-load 'geiser-guile
            (let ((root-dir
                   (file-name-directory
                    (locate-dominating-file default-directory ".dir-locals.el")))
                  (env-paths (split-string (getenv "GUILE_LOAD_PATH") ":")))
              (when env-paths (setq-local geiser-guile-load-path env-paths))
              (unless (member root-dir geiser-guile-load-path)
                (setq-local geiser-guile-load-path
                            (cons root-dir geiser-guile-load-path)))))))

 (scheme-mode
  (indent-tabs-mode . nil)))
