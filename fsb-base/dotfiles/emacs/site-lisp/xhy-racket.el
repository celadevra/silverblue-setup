;;; xhy-racket.el: configs for writing racket in Emacs

(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)

(provide 'xhy-racket)
