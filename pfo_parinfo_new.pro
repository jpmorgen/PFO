;+
; NAME: pfo_parinfo_new
;
; PURPOSE: create a new strand of parinfo corresponding to the
; instance of a particular function
;
; CATEGORY: PFO

; CALLING SEQUENCE: newparinfo = pfo_parinfo_new(fname,
; [keyword=keyword...], [parallel_taglist=parallel_taglist | 
; taglist_series=taglist_series, [strict=strict]])

; DESCRIPTION: Invokes the pfo_<fname>__fdefine and pfo_<fname>__finit
; system to create a new string of parinfo.  The tag values of the
; parinfo array can be optionally initialized to the user's
; specification in the sequence of keywords.  parallel_taglist,
; taglist_series, and strict control how these keywords are
; presented to the top-level tag _set_tag routines (if present).  See
; README.names for a detailed description of this system.

; After you have created a newparinfo, append it to an existing
; parinfo with pfo_array_append and then call pfo_parinfo_update on
; the whole parinfo to work out any interdependencies.

;
; INPUTS: 
; 	fname: string containing function name

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 

;       In general, keyword parameters are passed to
;       pfo_<fname>__finit using the _REF_EXTRA mechanism.
;       pfo_<fname>__finit can pass these parameters on to
;       <tagname>_struct__set_tag routines in the same way so that
;       tags in the substructures that make up the parinfo can be set
;       conveniently.  The keywords taglist_series (abreviated tag)
;       and parallel_taglist determine how these _REF_EXTRA keywords
;       are distributed among the <tagname>_struct__set_tag routines.

;       parallel_taglist: If parallel = 1 (/parallel), pass all
;       keywords to all participating top-level
;       <tagname>_struct__set_tag procedures.  If parallel = [vector
;       of] type string, only pass keywords to the named tags.

;       taglist_series: like parallel_taglist, but order is important.
;       As each <tagname>_struct__set_tag procedure is processed, the
;       tag names are "soaked up" and, if there are duplicate sub-tag
;       names (e.g. status is a popular one), they are not set in
;       subsequent tags.  This is the default.

;       strict: in conjunction with taglist_series, uses IDL's
;       _STRICT_EXTRA mechanism on the last call to the
;       <tagname>_struct__set_tag to make sure that there is strict
;       matching between supplied keywords and tags in the
;       substructures

;
; OUTPUTS:

;	parinfo array with appropriate tags for function fname

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
; $Id: pfo_parinfo_new.pro,v 1.2 2011/09/01 22:15:32 jpmorgen Exp $
;
; $Log: pfo_parinfo_new.pro,v $
; Revision 1.2  2011/09/01 22:15:32  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_parinfo_new, fname, pfo_obj=pfo_obj, _REF_EXTRA=extra

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /CONTINUE, 'USAGE: parinfo=pfo_parinfo_new(fname, [keyword args to function_name__init])'
        message, 'USAGE: where the file <function_name>__fdefine.pro contains the initialization (function <function_name>__init) and definition (pro <function_name>__fdefine) of the function, in that order.'
     endif
  endif ;; not debugging

  ;; When fname is not specified, return the current
  ;; parinfo_template.  Pass along all the _EXTRA, in case the caller
  ;; wants to interface with pfo_parinfo_template stuff.
  if NOT keyword_set(fname) then $
    return, pfo_parinfo_template(pfo_obj=pfo_obj, _EXTRA=extra)

  ;; Call the __define procedure, which compiles the file which should
  ;; have both the __define procedure and the __init function in it
  call_procedure, fname + '__fdefine', pfo_obj=pfo_obj

  ;; Use IDL's routine_info function to determine if we have keywords
  init_funct = fname + '__init'
  init_params = routine_info(fname + '__init', /functions, /parameters)
  if init_params.num_kw_args gt 0 then $
    return, call_function(init_funct, pfo_obj=pfo_obj, _EXTRA=extra) $
  else $
    return, call_function(init_funct)
end



