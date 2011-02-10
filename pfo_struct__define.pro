; +
; $Id: pfo_struct__define.pro,v 1.4 2011/02/10 22:31:36 jpmorgen Exp $

; pfo_struct__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; This is a modular part of pfo_parinfo__define.

;; Tag 	Value	Meaning
;; status 0	Indicates to all PFO packages that this parameter not
;; 		used (i.e. set status=0 to have just PFO skip this
;; 		parameter)
;;	  1	parameter in use
;;	 -1	parameter marked for deletion
;; ofixed	Old mpfit fixed value.  When you set status = 0, you
;; 		also have to set fixed = 1 or else MPFIT complains.
;; 		ofixed is a place to store the old value so that when
;; 		you go back to being active, you know if you were
;; 		fixed or not.  Use pfo_mode to sort all this stuff
;; 		out.
;; fixed_mode	see pfo_mode
;;	 0	parameter .fixed value can be changed
;;	 1	parameter .fixed value will not be changed ("permanent")
;; ID 	 0-n	Parameter (or function) ID, for use by calling
;; 		routine.  Ideally, the calling routine should append
;; 		its own well-documented structure onto the pfo_parinfo
;; 		structure that provides multiple handles for grabbing 
;; 		groups of parameters.  If the task is simple, however,
;; 		pfo.ID can be used.  In pfo_funct, this is the LAST
;; 		identifier used to differentiate between parameter
;; 		sets that are otherwise equivalent.
;; fseq	 0-n	indicates order in which groups of parameters should
;; 		be handled.  Individual packages may need more a
;; 		sophisticated handling of this
;; inaxis 	What set of points should be used as the input axis to
;; 		this function
;;	  0	independent of axis
;;	  1	input X-axis (usually pixels, e.g. for dispersion
;;	  	relation (outaxis=1) or pixel dependent detector
;;	  	effects (outaxis=2)
;;	  2	transformed X-axis (e.g. for Voigts
;;		parameterized in wavelength)
;;	  3	Y-axis
;; infunct	This is a string such as "alog10" that is a single-
;; 		argument function applied to the X-axis (calculated or
;; 		input, depending on inaxis value) before the axis is
;; 		used to calculate the function.  Allows easy
;; 		calculation of functions in log (or whatever) space
;; 		without having to rewrite primitives
;; outaxis 	Where do the results go?
;;	   0	output of function does not operate on any axis
;;	   1	NOT ALLOWED -- input X-axis is always preserved
;;	   2	transformed X-axis
;;	   3	Y-axis
;; outfunct	This, like infunct, is a string that is the name of a
;; 		function called by pfo_funct on the output axis
;; 		_after_ the functon has been calculated so that
;; 		calculations can be done in, e.g. Y log space (HINT:
;; 		use the inverse function and make sure to debug
;; 		thoroughly)
;; fop		How are the function results combined with the
;; 		existing values on that axis?
;;	  0	Does not output to an axis
;;	  1	Additive function (e.g. Voigt line profile)
;;	  2	Multiplicative function (e.g. instrument sensitivity polynomial)
;;	  3	Replacement (e.g. dispersion relation)
;; ftype  0	this parameter is not handled by a PFO function
;;	  non-zero  See pfo_sysvar__define.pro, in particular fnames,
;;	  for the list of supported pfo functions
;;
;; format and eformat: strings used to format the parameter and error
;; 	  during printing

function pfo_struct__init
  ;; Currently no non-null values in default
  return, {pfo_struct}
end

pro pfo_struct__define
  pfo_struct $
    = {pfo_struct, $
       status	: 0, $
       ofixed	: 0, $
       fixed_mode:0, $
       ID	: 0, $
       fseq	: 0, $
       inaxis	: 0, $
       infunct	: '',$
       outaxis	: 0, $
       outfunct	: '',$
       fop	: 0, $
       ftype 	: 0.,$
       format	: '',$
       eformat	: '' $
      } 
end
