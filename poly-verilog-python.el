(require 'polymode)
(require 'verilog-mode)

(define-hostmode poly-verilog-hostmode
  :mode 'verilog-mode)

(define-innermode poly-python-expr-verilog-innermode
  :mode 'python-mode
  :head-matcher "^[ \t]*- \\|#{"
  :tail-matcher "$\\|}"
  :head-mode 'host
  :tail-mode 'host
  )

(define-polymode poly-verilog-python-mode
  :hostmode 'poly-verilog-hostmode
  :innermodes '(poly-python-expr-verilog-innermode))

(provide 'poly-verilog-python)
