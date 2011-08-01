;+
; NAME: pfo_fdefine
;
; PURPOSE: define basic attributes of a function in the PFO system
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: pfo_fdefine, fname=fname, fnpars=fnpars,
; fdescr=fdescr, fnum=fnum [, pfo_obj=pfo_obj]

; DESCRIPTION: This procedure performs a low-level initialization of
; the function "fname" in the PFO system.  This procedure is a
; primitive of the function pfo_parinfo_new.  If you want to create a
; new instance of a function in PFO, use pfo_parinfo_new.

; In detail, this procedure maps an fnum to each fname.  fnum is the
; integer part of the tag parinfo.pfo.ftype.  This tag is used by
; pfo_funct and other routines to "grab" function parameters and pass
; them off to the primitives which ultimately proces them.  These
; primitives are, for the most part, routines in the function defining
; code <fname>__define.pro

; In addition to mapping an fnum to fname, fnpars and fdescr are also
; stored (see keywords to this routine and tags in pfo_fstruct).

; INPUTS:

;	pfo_fstruct_array: in order to accommodate both
;	object-oriented and non objected-oriented operation, we use a
;	call_procedure system in the pfo_parinfo_obj to operate on the
;	pro_fstruct_array

;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;	fname: function name (string)

;	fnpars: number of parameters in function (0 = unknown)

;	fdescr: function documentation

;	pfo_obj: the pfo_obj in which this function will be used.
;	The pfo_fstruct_array, which stores the information necessary
;	for this routine to work will be placed into that pfo_obj.
;	Otherwise, it will be stored in the PFO COMMON block.

;	fnum: (return) assigned integer part of parinfo.pfo.ftype

;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  

;   PFO.  If we are not using objects.  This is one case where a
;   COMMON block is handy.  We want to have some persistent variables
;   (e.g. parinfo_template, parinfo_descr) that change in type (from
;   undefined to structs of various sizes and shapes).  It is the
;   change in type that prevents us from putting this into something
;   like !pfo.parinfo_template, since the !pfo system variable struct
;   tags cannot change type.  In object-oriented mode, we can stop
;   using the COMMON block and put our template on the heap.  The heap
;   variable can be nicely managed with the pfo_obj object lifecycle

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
; $Id: pfo_fdefine.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_fdefine.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_fdefine, $
  fname=fname, $ ;; function name
  fnum=fnum, $ ;; (return) assigned integer part of parinfo.pfo.ftype
  fnpars=fnpars, $ ;; number of parameters in function (0 = unknown)
  fdescr=fdescr, $ ;; function documentation
  pfo_obj=pfo_obj ;; pfo_obj

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        ;; Put our information back into pfo_obj
        if N_elements(pfo_obj) ne 0 then begin
           pfo_obj->set_property, $
             pfo_fstruct_array=fstruct_array, $
             pfo_fstruct_descr=fstruct_descr, $
             /no_copy
        endif ;; putting info back into pfo_obj
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_fdefine, fname=fname, fnpar=fnpar, fdescr=fdescr[, pfo_obj=pfo_obj]'
     endif
  endif ;; not debugging

  ;; Extract the fstruct stuff from pfo_obj, if specified
  if N_elements(pfo_obj) gt 0 then begin
     pfo_obj->get_property, $
       pfo_fstruct_array=fstruct_array, $
       pfo_fstruct_descr=fstruct_descr, $
       /no_copy
  endif else begin
     ;; Check to see if we are in object oriented-only mode but the
     ;; user forgot to pass pfo_obj
     if keyword_set(!pfo.objects_only) then $
       message, 'ERROR: !pfo.objects_only = ' + strtrim(!pfo.objects_only, 2) + '.  pfo_fdefine was called, but not passed an object.'

     ;; If we made it here, we were not handed an object and that is
     ;; OK.  Extract fstruct stuff from our PFO COMMON block.  Do it
     ;; in a way that there is no cross-talk between objects and
     ;; non-object mode, if the user is mixing and matching.
     COMMON PFO, pfo_fstruct_array, pfo_fstruct_descr, $
       parinfo_template, parinfo_descr

     if N_elements(pfo_fstruct_array) gt 0 then $
       fstruct_array = pfo_fstruct_array
     if N_elements(pfo_fstruct_descr) gt 0 then $
       fstruct_descr = pfo_fstruct_descr

  endelse ;; object-oriented

  if N_elements(fstruct_array) eq 0 then begin
     ;; Our first function.  Initilize the function and capture the
     ;; documentation of pfo_fstruct
     fstruct_array = $
       pfo_struct_new('pfo_fstruct', fname=fname, fnpars=fnpars, fdescr=fdescr, descr=fstruct_descr)
     ;; Set our return fnum
     fnum = 0
  endif else begin
     ;; We have an array up and running.  Check to see if our function
     ;; is already in the array.  Do this with our local copy of
     ;; pfo_fstruct_array rather than pfo_finfo or pfo_fnum, since we
     ;; have gutted pfo_obj of the necessary information with /no_copy.
     fnum = where(strupcase(fname) eq strupcase(fstruct_array.fname), count)
     if fnum eq !tok.nowhere then begin
        ;; Add our function to the end of pfo_fstruct_array
        pfo_array_append, $
          fstruct_array, $
          pfo_struct_new('pfo_fstruct', fname=fname, fnpars=fnpars, fdescr=fdescr)
        ;; Set our return fnum
        fnum = N_elements(fstruct_array)-1
     endif ;; found fname in pfo_fstruct_array
  endelse ;; have a pfo_fstruct_array

  ;; Put our working fstruct_array back
  if N_elements(pfo_obj) ne 0 then begin
     pfo_obj->set_property, $
       pfo_fstruct_array=fstruct_array, $
       pfo_fstruct_descr=fstruct_descr, $
       /no_copy
  endif else begin
     pfo_fstruct_array = fstruct_array
     pfo_fstruct_descr = fstruct_descr
  endelse
     

end
