;+
; NAME: pfo_struct__define
;
; PURPOSE: Define, initialize and work with the structure PFO_STRUCT.
;
; CATEGORY:
;
; CALLING SEQUENCE: use pfo_struct_new, pfo_struct_setget_tag,
; pfo_struct_update

; DESCRIPTION: 

;   PFO_STRUCT is a structure that is added as a tag, named "PFO," to
;   the MPFIT "parinfo" structure.  PFO_STRUCT contains tags that are
;   interpreted by pfo_funct and other PFO routines.  The tags in
;   PFO_STRUCT are what enable the PFO system to make the
;   transformation from code space to data structure space.

;   PFO_STRUCT is not meant to contain an exhaustive list of useful
;   tags for the PFO system.  Other <tag>_struct should be created
;   contain those tags.  You can use this file as a template in order
;   to expand the capabilities of PFO in a modular way.  The current
;   design has <tag>_struct managed by the pfo_struct system (sorry
;   for the degeneracy in the name).

;   For a gentle introduction to the pfo_struct system, read the
;   documentation for pfo_struct_new.

;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_struct__define.pro,v 1.7 2011/09/08 20:15:30 jpmorgen Exp $
;
; $Log: pfo_struct__define.pro,v $
; Revision 1.7  2011/09/08 20:15:30  jpmorgen
; Added fname to pfo structure
;
; Revision 1.6  2011/09/01 22:13:08  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.5  2011/08/01 18:36:03  jpmorgen
; First reasonably functional version of pfo_obj
;
;-

;; Old documentation

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
;; 		pfo.pfoID can be used.  In pfo_funct, this is the LAST
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


;; pfo_struct__update makes sure that all of the functions mentioned
;; in the parinfo have at least a <fname>__fdefine.pro file on disk.
;; It initializes any functions not already defined in this instance
;; of the pfo_finfo system (see pfo_finfo.pro).  It also synchronizes
;; the integer part of parinfo.pfo.ftype with the fnums stored in the
;; finfo system.  pfo_strct_good_idx returns all indices into parinfo
;; for which these operations were successful
pro pfo_struct__update, $
   parinfo, $
   pfo_obj=pfo_obj, $ ;; We use the pfo_finfo system, which, if we are in object-oriented mode, uses information in the pfo_obj
   completed_updates=completed_updates, $	;; (input/output) list of completed updates
   pfo_struct_good_idx=good_idx, $ ;; (output) list of indices into parinfo for which valid pfo functions have been found
   _REF_EXTRA=extra

  ;; Don't run more than once.  NOTE: this doesn't double check pfo_struct_good_idx
  if pfo_struct_updated(completed_updates) then $
     return

  ;; We don't depend on any other tag updates, so just do our work

  ;; Initialize our output
  good_idx = !tok.nowhere

  ;; Go through things parameter by parameter
  for ip=long(0), N_elements(parinfo)-1 do begin
     ;; Get our fname from the parinfo
     fname = parinfo[ip].pfo.fname

     if !pfo.debug le 0 then begin
        ;; Get ready to catch any errors
        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, /CONTINUE, 'WARNING: caught the above error.  Parameter ' + strtrim(ip, 2) + ' will cause an error unless you delete it from the parinfo or find the function that helps to define it!  The delete can be acccomplished with the pfo_struct_good_idx keyword like this: parinfo = parinfo[pfo_struct_good_idx] '
           CONTINUE
        endif ;; catching missing __fdefine
     endif ;; debugging

     ;; Make sure that the code need to run (or at least define) this
     ;; function exists in this instance of PFO
     resolve_routine, fname + '__fdefine', /no_recompile

     ;; Check to see what finfo thinks the fnum is for this
     ;; parameter's fname.  finfo should have gentle return values
     ;; (e.g. !tok.nowhere if fname not found) and not raise any
     ;; errors
     finfo_fnum = pfo_fnum(fname, pfo_obj=pfo_obj)
     ;; Check to see what the parinfo thinks its fnum is
     parinfo_fnum = floor(parinfo[ip].pfo.ftype)

     ;; If they are the same, we are done.  But double check for a
     ;; pathological case of parinfo_fnum < 0, which might trigger
     ;; equality at funfo_fnum eq !tok/nowhere
     if finfo_fnum eq parinfo_fnum and parinfo_fnum ge 0 then begin
        ;; Remember to append our ip to the list of good_idx
        pfo_array_append, good_idx, ip
        CONTINUE
     endif ;; finfo_fnum and parinfo_fnum match

     ;; If we made it here, we need to syncronize the integer part of
     ;; ftype and finfo_fnum.

     ;; Make sure fname is in the finfo system
     if finfo_fnum eq !tok.nowhere then begin
        call_procedure, fname + '__fdefine', pfo_obj=pfo_obj
        ;; Get the new finfo_fnum
        finfo_fnum = pfo_fnum(fname, pfo_obj=pfo_obj)
     endif ;; initializing

     ;; This is the meat of our code.  We need to make sure the
     ;; integer part of ftype is syncronized with the finfo system.
     ;; Prefer the finfo value, since it is already defined and harder
     ;; to change.
     parinfo[ip].pfo.ftype = pfo_frac(parinfo[ip].pfo.ftype) + finfo_fnum
     ;; Remember to append our ip to the list of good_idx
     pfo_array_append, good_idx, ip
        
  endfor ;; each parameter

  ;; Mark our update as complete regardless of how many errors we generated.
  pfo_struct_update_complete, completed_updates

end


;; A __get_tag routine is optional but recommended.  It lets you map
;; tagnames to keywords in a procedure call
;; (e.g. pfo_struct_setget_tag).  This is the analog of a get_property
;; routine.  Although it is tedious to list all of the keywords/tags,
;; doing it this way passes values by reference and ultimately saves
;; memory.  Also, keywords don't necessarily have to map to
;; top-level keywords in <tag>_struct.  In other words, your struct
;; can be quite complicated and a properly set-up __set/get_tag
;; routines help you get the most out of it.

pro pfo_struct__get_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_setget_tag
   strict= strict, $ ;; See pfo_setget_tag
   _REF_EXTRA   	= extra, $
   status    	= status, $
   ofixed    	= ofixed, $
   fixed_mode	= fixed_mode, $
   pfoID       	= pfoID, $
   fseq      	= fseq, $
   inaxis    	= inaxis, $ 
   infunct   	= infunct, $
   outaxis   	= outaxis, $
   outfunct  	= outfunct, $
   fop       	= fop, $
   ftype     	= ftype, $  
   fname	= fname, $  
   format    	= format, $
   eformat   	= eformat  

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our tags into the
  ;; keywords

  if arg_present(status    ) or N_elements(status    ) ne 0 then status     = parinfo[idx].pfo.status 	  
  if arg_present(ofixed    ) or N_elements(ofixed    ) ne 0 then ofixed     = parinfo[idx].pfo.ofixed 	  
  if arg_present(fixed_mode) or N_elements(fixed_mode) ne 0 then fixed_mode = parinfo[idx].pfo.fixed_mode
  if arg_present(pfoID     ) or N_elements(pfoID     ) ne 0 then pfoID      = parinfo[idx].pfo.pfoID 	  
  if arg_present(fseq      ) or N_elements(fseq      ) ne 0 then fseq       = parinfo[idx].pfo.fseq 	  
  if arg_present(inaxis    ) or N_elements(inaxis    ) ne 0 then inaxis     = parinfo[idx].pfo.inaxis 	  
  if arg_present(infunct   ) or N_elements(infunct   ) ne 0 then infunct    = parinfo[idx].pfo.infunct   
  if arg_present(outaxis   ) or N_elements(outaxis   ) ne 0 then outaxis    = parinfo[idx].pfo.outaxis   
  if arg_present(outfunct  ) or N_elements(outfunct  ) ne 0 then outfunct   = parinfo[idx].pfo.outfunct  
  if arg_present(fop       ) or N_elements(fop       ) ne 0 then fop        = parinfo[idx].pfo.fop 	  
  if arg_present(ftype     ) or N_elements(ftype     ) ne 0 then ftype      = parinfo[idx].pfo.ftype 	  
  if arg_present(fname     ) or N_elements(fname     ) ne 0 then fname      = parinfo[idx].pfo.fname 	  
  if arg_present(format    ) or N_elements(format    ) ne 0 then format     = parinfo[idx].pfo.format 	  
  if arg_present(eformat   ) or N_elements(eformat   ) ne 0 then eformat    = parinfo[idx].pfo.eformat   

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /get, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end 

;; A __set_tag routine is optional, but recommended.  It saves some
;; code in the __init "method" and makes it easy to convert from
;; keywords to tag assignments with pfo_struct_setget_tag.  This is
;; the analogy of a set_property routine
pro pfo_struct__set_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_setget_tag
   strict= strict, $ ;; See pfo_setget_tag
   _REF_EXTRA   	= extra, $
   status    	= status, $
   ofixed    	= ofixed, $
   fixed_mode	= fixed_mode, $
   pfoID       	= pfoID, $
   fseq      	= fseq, $
   inaxis    	= inaxis, $ 
   infunct   	= infunct, $
   outaxis   	= outaxis, $
   outfunct  	= outfunct, $
   fop       	= fop, $
   ftype     	= ftype, $  
   fname	= fname, $  
   format    	= format, $
   eformat   	= eformat  

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags

  if N_elements(status    ) ne 0 then parinfo[idx].pfo.status 	  = status
  if N_elements(ofixed    ) ne 0 then parinfo[idx].pfo.ofixed 	  = ofixed
  if N_elements(fixed_mode) ne 0 then parinfo[idx].pfo.fixed_mode = fixed_mode
  if N_elements(pfoID     ) ne 0 then parinfo[idx].pfo.pfoID 	  = pfoID
  if N_elements(fseq      ) ne 0 then parinfo[idx].pfo.fseq 	  = fseq
  if N_elements(inaxis    ) ne 0 then parinfo[idx].pfo.inaxis 	  = inaxis
  if N_elements(infunct   ) ne 0 then parinfo[idx].pfo.infunct 	  = infunct
  if N_elements(outaxis   ) ne 0 then parinfo[idx].pfo.outaxis 	  = outaxis
  if N_elements(outfunct  ) ne 0 then parinfo[idx].pfo.outfunct   = outfunct
  if N_elements(fop       ) ne 0 then parinfo[idx].pfo.fop 	  = fop
  if N_elements(ftype     ) ne 0 then parinfo[idx].pfo.ftype 	  = ftype
  if N_elements(fname     ) ne 0 then parinfo[idx].pfo.fnae 	  = fname
  if N_elements(format    ) ne 0 then parinfo[idx].pfo.format 	  = format
  if N_elements(eformat   ) ne 0 then parinfo[idx].pfo.eformat 	  = eformat

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /set, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end 

function pfo_struct__init, $
  descr=descr, $ ;; descr is a return keyword that contains the structure documentation
  _REF_EXTRA=extra ;; keyword parameters to pass by reference (to save memory) to our __set_tag routine

  ;; Create our struct initialized with generic IDL null values.  We
  ;; could also have called:
  ;; pfo_struct = create_struct(name='pfo_struct')
  pfo_struct = {pfo_struct}

  ;; Our default parinfo will be free (set at parameter optimization
  ;; level) and active (set here)
  pfo_struct.status = !pfo.active
  ;; Default input axis is xaxis, the derived "internal" axis.  In the
  ;; absence of an Xin to xaxis transformation, xaxis is just
  ;; initilized to Xin
  pfo_struct.inaxis = !pfo.xaxis
  ;; Default output axis is Y-axis
  pfo_struct.outaxis = !pfo.yaxis
  ;; Default function operation add to existing Y-axis
  pfo_struct.fop = !pfo.add
  ;; Default formats
  pfo_struct.format='(g12.6)'
  pfo_struct.eformat='(g12.6)'

  ;; Create our description
  descr = $
    {README	: 'Basic structure of the PFO system', $
     status	: '0 not used, 1, in use, -1 marked for deletion', $
     ofixed	: 'old mpfit fixed value.  See pfo_mode', $
     fixed_mode	: 'See pfo_mode; 0: .fixed can be changed, 1: .fixed not changed', $
     ID	: 'param/function ID.  In pfo_funct, this is the LAST identifier used to differentiate between parameter sets that are otherwise equivalent.', $
     fseq	: 'function sequence provides a way of making algebraic ordering in pfo_funct', $
     inaxis	: '0: independent of axis, 1: Xin, 2: Xaxis, 3: Y-axis', $
     infunct	: 'This is a string such as "alog10" that is a single-argument function applied to the X-axis (calculated or input, depending on inaxis value) before the axis is used to calculate the function.  Allows easy calculation of functions in log (or whatever) spacewithout having to rewrite primitives',$
     outaxis	: '0: no axis, 1: not allowed, 2: X-axis, 3: Y-axis', $
     outfunct: 'This, like infunct, is a string that is the name of a function called by pfo_funct on the output axis after_ the functon has been calculated so that calculations can be done in, e.g. Y log space (HINT: use the inverse function and make sure to debug thoroughly)',$
     fop	: '0: Does not output to an axis, 1: Additive function (e.g. Voigt line profile), 2: Multiplicative function (e.g. instrument sensitivity polynomial)', $
     ftype 	: 'real number indicating what function (integer part) and parameter number (decimal part) this parameter corresponds to.  For simple functions (e.g. Voigt), this works well.  For complex ones (e.g. SSO), you will want to add tags to the parinfo structure to help your function figure out which parameters are which',$
     fname	: 'function name (e.g. pfo_voigt).  This is intended only to help syncronize the integer part of ftype when combining parinfos from different sources.', $
     format	: 'format string for printing value',$
     eformat	: 'format string for printing error'}

  ;; The last thing we do is pass on any other keywords to our
  ;; __set_tag "method."  Do this with _STRICT_EXTRA to make sure that
  ;; no bogus keywords are passed.  Be careful with debugging and make
  ;; sure user gets the best effort case when we are trying to ignore
  ;; the error.
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /INFORMATIONAL, 'WARNING: the above error was produced.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting (not recommended).' 
        return, pfo_struct
     endif ;; CATCH
  endif ;; debugging
  pfo_struct__set_tag, pfo_struct, _STRICT_EXTRA=extra

  return, pfo_struct

end

;; Standard IDL named structure definition 
pro pfo_struct__define

  ;; Read in system variables for all routines in this file.
  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Define our named structure
  pfo_struct = $
    {pfo_struct, $
     status  : 0B, $
     ofixed  : 0B, $
     fixed_mode:0B, $
     pfoID   : 0, $
     fseq    : 0, $
     inaxis  : 0B, $
     infunct : '',$
     outaxis : 0B, $
     outfunct: '',$
     fop     : 0B, $
     ftype   : float(0),$
     fname   : '', $
     format  : '',$
     eformat : '' $
    }
end
