; +
; $Id: pfo_sysvar__define.pro,v 1.9 2011/09/16 13:43:50 jpmorgen Exp $

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

  pfo $
    = {pfo_sysvar, $
       popt	:	'MPFIT', $ ; default parameter optimizer
       debug	:	0, $    ; debug level 0 =catch, 1=don't catch, 2=xmanager, catch=0
       quiet	:	0, $	; keeps track of pfo_quiet level
       null	:	0, $	; IMPORTANT THAT NULL BE 0
       $ ;; Initial value of Yaxis in pfo_parinfo_parse, /calc.  If NaN, the deviates and plotting system works better, but you need to make sure that the first function to operate in each ROI has parinfo.pfo.fp = !pfo.replace.  If 0d, you can just have all functions combine additively
       init_Yaxis:	!values.d_nan, $
       not_used	:	0, $	; status tokens
       not_pfo	:	0, $
       active	:	2^1, $
       inactive	:	2^2, $
       delete	:	2^3, $
       all_status:	2^1+2^2+2^3, $
       $ ;; Companion tokens for .fixed.  
       fixed	:	1, $    ; (see pfo_mode)
       free	:	0, $
       $ ;; Token for pfo.fixed_mode and pfo_mode query keyword
       permanent:	1, $
       non_permanent:	0, $
       indeterminate:	-1,$
       none	:	0, $	; axis tokens
       Xin	:	1, $
       Xaxis	:	2, $	; Transformed X-axis
       Yaxis	:	3, $
       $ ;; String representation of the fop tokens
       axis_string:	['', 'Xin', 'X', 'Y'], $
       widget_axis_string:['none', 'Xin', 'X', 'Y'], $
       $ ;; fop tokens -- in the order they operate
       noop	:	0, $    ; Useful for pfo_deriv
       repl	:	1, $	; Replace contents of target
       mult	:	2, $
       add	:	3, $
       convolve	:	4, $	; See NOTE in pfo_parinfo_parse
       $ ;; String representation of the fop tokens
       fop_string:	['', '', '*', '+', 'convol'], $
       widget_fop_string:['noop', 'repl', '*', '+', 'convol'], $
       $ ;; String representations of delimiters (free, limited, fixed)
       delimiters: ['.', '<', '|'], $
       $ ;; Indicator when we are pegged against a limit
       pegged: '*', $
       $;; obselete
       $;;poly	:	1,  $	; ftype tokens
       $;;deltafn	:	2,  $
       $;;gauss	:	3,  $
       $;;voigt	:	4,  $
       $;;sso	:	5,  $
       $;;sso_funct:	5,  $
       $;;czt	:	6,  $
       $;;pow	:	7,  $
       $;;scint	:	8,  $ ;; Generic scintillator
       $;;bgo	:	8,  $ ;; temporary for debugging
       $;;mgm	:	9,  $	;; Modified Gaussian
       $;;ROI	:	10, $
       $;;deriv	:	11, $ ;; derivative -- depends on link
       $;;last_fn	:	11, $
       $ ;; If you add functions, add to these.  0 parameters means unspecified
       fnames	:	['null', 'poly', 'deltafn', 'gauss', 'voigt', 'sso_funct', 'czt', 'pow', 'scint', 'mgm', 'ROI', 'deriv'], $
       fnpars	:	[0,0,2,3,4,0,10,2,5,3,2,1], $ ;; Number of params per fn
       $ ;; Tokens for printing
       print	:	1,   $  ;; parameter printing options 1=/print -- single-line concise print
       ppname	:	2,   $  ;; parameter names only
       pmp	:	3,   $  ;; print all mpfit fields
       pall	:	4,   $  ;; print all mpfit fields plus parameter numbers
       separator:	',', $	;; Separator used in printing parameters, names
       pname_width:	12,  $	;; formatted width of the pname field
       longnames:	1,   $  ;; Print long parameter names for clarity
       parnums	:	1,   $  ;; print parameter numbers at the beginning of each line in !pmp
       left	:	0,   $
       right	:	1,   $
       $ ;; tokens for pfo_link status
       $;;inactive:	0,   $  duplicate of pfo.status
       master	:	1, $
       slave	:	2, $
       intralink:	1, $
       interlink:	2, $
       $ ;; mpfit iterproc/stop stuff
       iterproc	:	'pfo_iterproc', $
       iterstop	:	-2, $   ;; stop fit, keep values
       iterquit	:	-3, $   ;; stop fit, discard values
       window_index:	 28, $  ;; default PFO plot window.  Should not be 0 (see pfo_plot_obj__define)
       $ ;; tokens for actions in pfo_funct to keep code clean
       $;;print	:	1, $ ;; duplicate above
       calc	:	2, $
       indices	:	4, $
       widget	:	8, $
       $ ;; tokens for pfo_multi_ROI_struct.  Note that defining
       $ ;; allROI this way puts any operations it does up front,
       $ ;; which short-circuits what you might try to do with fseq in
       $ ;; other ROIs.  If you want your all-encompassing ROI/spec
       $ ;; functions to operate on the result of other 
       $ ;; calculations, set all_[spec/ROI] = !tok.max_long
       allspec	:	-1, $
       allROI	:	-1, $
       $ ;; pfo_obj stuff
       objects_only:	0, $	;; if set to 1, makes sure PFO COMMON block is not used
       pfo_obj_ClassName: 'pfo_obj', $ ;; Default top-level object class name used by pfo_obj_new
       $ ;; String used in pfo_struct_append when no descr is returned from pfo_struct_new
       not_documented: 'Not documented', $
       $ ;; Default labels for plotting
       Xin_title	: 'Input X-axis units (Xin)', $
       Xaxis_title	: 'Calculated X-axis units (Xaxis)', $
       Yin_Xin_title	: 'Input Y-axis (Yin) units per Xin units', $
       Yin_Xaxis_title	: 'Input Y-axis (Yin) units per Xaxis units', $
       color_table	: 38, $ ;; rainbow18
       n_colors		: 18L, $ ;; work with !d.n_colors to index colors
       oplot_parinfo_color: 6, $ ;; a nice green color
       oplot_parinfo_thick_boost: 2, $
       oplot_ROI_thick_boost: 3, $
       oplot_ROI_allROI_color: 3, $ ;; a relaxing dark blue
       oplot_ROI_allROI_thick_boost: 1, $
       win_font		: 'fixedsys' $ ;; font to use for windows
      }


;; obselete
;;  if N_elements(pfo.fnames) ne pfo.last_fn+1 or $
;;    N_elements(pfo.fnpars) ne pfo.last_fn+1 then $
;;    message, 'ERROR: pfo_sysvar definition is not consistent.  If you have added a function definition, make sure you update last_fn, fnames, and fnpars'

  defsysv, '!pfo', pfo

end
