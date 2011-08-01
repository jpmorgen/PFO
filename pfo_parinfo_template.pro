;+
; NAME: pfo_parinfo_template
;
; PURPOSE: keep track of the default parinfo array element
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: parinfo1 =
; pfo_parinfo_template([required_tags=required_tags,] [set=parinfo,]
; [parinfo_descr=parinfo_descr,] [popt=popt])
;

; DESCRIPTION: Uses the parinfo_template and parinfo_descr variables
; either in the PFO COMMON block or in the pfo_obj's heap variables to
; keep track of the current default template to be used in creating
; new parinfo elements.

; INPUTS:
;
; OPTIONAL INPUTS:

; KEYWORD PARAMETERS: 

;	pfo_obj -- pfo_obj if in object-oriented mode

;	required_tags -- a list of tags that should be in the returned
;                        parinfo template.  PFO_STRUCT_NEW is used to
;                        create these tags and, optionally, their
;                        descriptions.

;       parinfo_descr -- a parallel structure to the current parinfo
;                        template which serves as documentation

;       set -- If you want to manually manipulate the template, use
;              this keyword to input a parinfo structure which will
;              become the new template.  Can be input as an array of
;              struct, only set[0] will be used as the template.
;              Note that required_tags keyword is preferred over set.

;	popt -- parameter optimizer (string).
;               pfo_struct_new(<popt>_parinfo_struct) is called to
;               create the top-level tags in the parinfo which are
;               needed for the parameter optimizer.  Default is MPFIT.

;
; OUTPUTS: the current parinfo template.
;
; OPTIONAL OUTPUTS:

; COMMON BLOCKS:  

;   PFO, if we are not using objects.  This is one case where a COMMON
;   block is handy.  We want to have some persistent variables
;   (e.g. parinfo_template, parinfo_descr) that change in type (from
;   undefined to structs of various sizes and shapes).  It is the
;   change in type that prevents us from putting this into something
;   like !pfo.parinfo_template, since the !pfo system variable struct
;   tags cannot change type.  In object-oriented mode, we can stop
;   using the COMMON block and put our template on the heap.  The heap
;   variable can be nicely managed with the object lifecycle.

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
; $Id: pfo_parinfo_template.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_parinfo_template.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_parinfo_template, popt=popt, required_tags=required_tags, $
  set=set_parinfo, parinfo_descr=parinfo_descr_in, pfo_obj=pfo_obj

  init = {pfo_sysvar}
  init = {tok_sysvar}

  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Put our information back into pfo_obj
        if N_elements(pfo_obj) ne 0 then begin
           pfo_obj->set_property, $
             parinfo_template=parinfo_template_local, $
             parinfo_descr=parinfo_descr_local, $
             /no_copy
        endif ;; putting info back into pfo_obj
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_parinfo_template[, popt=popt][, required_tags=required_tags][, set=set_parinfo][, parinfo_descr=parinfo_descr][, pfo_obj=pfo_obj]'
     endif
  endif ;; not debugging

  ;; Extract the parinfo_template stuff from pfo_obj, if specified
  if N_elements(pfo_obj) gt 0 then begin
     pfo_obj->get_property, $
       parinfo_template=parinfo_template_local, $
       parinfo_descr=parinfo_descr_local, $
       /no_copy
  endif else begin
     ;; Check to see if we are in object oriented-only mode but the
     ;; user forgot to pass pfo_obj
     if keyword_set(!pfo.objects_only) then $
       message, 'ERROR: !pfo.objects_only = ' + strtrim(!pfo.objects_only, 2) + '.  pfo_parinfo_template was called, but not passed an object.'

     ;; If we made it here, we were not handed an object and that is
     ;; OK.  Extract fstruct stuff from our PFO COMMON block.  Do it
     ;; in a way that there is no cross-talk between objects and
     ;; non-object mode, if the user is mixing and matching.
     COMMON PFO, pfo_fstruct_array, pfo_fstruct_descr, $
       parinfo_template, parinfo_descr

     if N_elements(parinfo_template) gt 0 then $
       parinfo_template_local = parinfo_template
     if N_elements(parinfo_descr) gt 0 then $
       parinfo_descr_local = parinfo_descr

  endelse ;; object-oriented

  ;; Check to see if we wanted to manually set the parinfo to
  ;; something.  If we stick with the required_tags keyword, this
  ;; should not be necessary
  if keyword_set(set_parinfo) then begin
     ;; Make sure that our parinfo_template is only a 1-element array
     parinfo_template_local = set_parinfo[0]
     if keyword_set(parinfo_descr_in) then $
       parinfo_descr_local = parinfo_descr_in
  endif ;; setting parinfo and optionally description

  ;; The very first parinfo_template.  Keep it as simple as possible:
  ;; just the basic parameter optimizer stuff
  if NOT keyword_set(parinfo_template_local) then begin
     ;; Default parameter optimizer is stored in !pfo.popt, but can be
     ;; overridden with the popt keyword argument to this routine
     if NOT keyword_set(popt) then $
       popt = !pfo.popt
     parinfo_template_local = pfo_struct_new(popt+'_parinfo_struct', $
                                             descr=parinfo_descr_local)
  endif ;; constructing the default parinfo_template

  ;; Do a sanity check on required_tags
  if N_elements(required_tags) gt 0 and $
    size(/type, required_tags) ne !tok.string then $
      message, 'ERROR: required_tags must be type string'

  ;; Let pfo_struct_append do the heavy lifting
  pfo_struct_append, parinfo_template_local, required_tags, descr=descr
  ;; check to see if the tag is playing the descr game
  if keyword_set(descr) then $
    pfo_struct_append, parinfo_descr_local, temporary(descr)

  ;; Put everything back into our object or COMMON block
  if N_elements(pfo_obj) ne 0 then begin
     ;; Don't do the /no_copy in this case, since we use them as
     ;; return value(s)
     pfo_obj->set_property, $
       parinfo_template=parinfo_template_local, $
       parinfo_descr=parinfo_descr_local
  endif else begin
     parinfo_template = parinfo_template_local
     parinfo_descr = parinfo_descr_local
  endelse

  ;; Check to see if we wanted to know what parinfo_descr was
  if arg_present(parinfo_descr_in) or N_elements(parinfo_descr_in) ne 0 then $
    parinfo_descr_in = temporary(parinfo_descr_local)

  return, parinfo_template_local

end
