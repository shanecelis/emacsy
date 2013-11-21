(define-module (emacsy mode)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-26)
  #:use-module (string completion)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy event)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (rnrs base)
  #:export (<mode>
            mode-name
            mode-map))

(define-class <mode> ()
  (name #:getter mode-name #:init-keyword #:mode-name)
  (mode-map #:accessor mode-map 
            #:init-keyword #:mode-map 
            #:init-form (make-keymap)))
