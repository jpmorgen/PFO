; +
; $Id: pfo_sysvar__define.pro,v 1.2 2004/01/15 17:05:49 jpmorgen Exp jpmorgen $

; pfo_sysvar__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  So call explicitly with an
; argument if you need to have a default structure with different
; initial values, or as in the case here, store the value in a system
; variable.

;; This defines the !pfo system variable, which contains some handy
;; tokens for refering to function types, axis, etc. in the pfo
;; formalism.  The idea is to have code read in english, so unique
;; token IDs are not necessary.  If the numbering scheme changes, just
;; change the token values and everything still works.  Stick an
;; initialized pfo_parinfo record on the end for handy reference.

;; WARNING: pfo_funct assumes that !pfo.fnames contains names of
;; functions that can be used to construct IDL function calls at
;; runtime.  The functions names recorded in !pfo.fnames
;; (e.g. "myfunct") should be defined in the same way user function
;; are defined for mpfit, but called pfo_myfunct and stored in a file
;; pfo_myfunct.pro.


; -

pro pfo_sysvar__define
  ;; System variables cannot be redefined and named structures cannot
  ;; be changed once they are defined, so it is OK to check this right
  ;; off the bat
  defsysv, '!pfo', exists=pfo_exists
  if pfo_exists eq 1 then return
  
  pfo_parinfo__define, parinfo=pfo_parinfo
  pfo $
    = {pfo_sysvar, $
       debug	:	0, $
       null	:	0, $	;; IMPORTANT THAT NULL BE 0
       ytemplate:	double(0), $	;; Default Y-axis type and value
       not_used	:	0, $	;; status tokens
       inactive	:	0, $	;; status tokens
       not_pfo	:	0, $
       active	:	1, $
       delete	:	-1, $
       Xin	:	1, $	;; axis tokens
       Xaxis	:	2, $	;; Transformed X-axis
       Yaxis	:	3, $
       $ ;; fop tokens -- in the order they operate
       repl	:	1, $	;; Replace contents of target
       mult	:	2, $
       add	:	3, $
       convolve	:	4, $	;; See NOTE in pfo_funct
       poly	:	1, $	;; ftype tokens
       deltafn	:	2, $
       gauss	:	3, $
       voigt	:	4, $
       sso	:	5, $
       sso_funct:	5, $
       czt	:	6, $
       pow	:	7, $
       bgo	:	8, $
       mgm	:	9, $	;; Modified Gaussian
       last_fn	:	9, $
       $ ;; If you add functions, add to these.  0 parameters means unspecified
       fnames	:	['null', 'poly', 'deltafn', 'gauss', 'voigt', 'sso_funct', 'czt', 'pow', 'bgo', 'mgm'], $
       fnpars	:	[0,0,2,3,4,0,10,2,5,3], $ ;; Number of params per fn
       $ ;; Here are tokens for individual parameters.  Are these useful?
       $ ;; Names will have to be handled by the calling routines,
       $ ;; since these can have different meanings (e.g. area
       $ ;; vs. equivalent width)
       print	:	1,   $  ;; parameter printing options 1=/print
       ppname	:	2,   $  ;; parameter names only
       pmp	:	3,   $  ;; print all mpfit fields
       separator:	',', $	;; Separator used in printing parameters, names
       pname_width:	12,  $	;; formatted width of the pname field
       longnames:	1,   $  ;; Print long parameter names for clarity
       left	:	0,   $
       right	:	1,   $
       $ ;; mpfit iterproc/stop stuff
       iterproc	:	'pfo_iterproc', $
       iterstop	:	-1, $   ;; stop fit, keep values
       iterquit	:	-2, $   ;; stop fit, discard values
       plotproc	:	'',  $ ;; --> Write a simple pfo_plotproc
       plotwin	:	7,   $
       parinfo	:	pfo_parinfo}

  if N_elements(pfo.fnames) ne pfo.last_fn+1 or $
    N_elements(pfo.fnpars) ne pfo.last_fn+1 then $
    message, 'ERROR: pfo_sysvar definition is not consistent.  If you have added a function definition, make sure you update last_fn, fnames, and fnpars'

  defsysv, '!pfo', pfo

end
