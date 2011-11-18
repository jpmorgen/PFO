;+
; NAME: pfo_finfo
;
; PURPOSE: Return low-level initialization information about a
; function in the PFO system
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: pfo_finfo [, fnum=fnum] [, fname=fname] [,
; fnpars=fnpars] [, fdescr=fdescr] [, pfo_obj=pfo_obj]

; DESCRIPTION: Functions are assigned unique numbers in the PFO system
; by the pfo_fdefine procedure.  The name, number of parameters, and
; documentation of each function are stored in an array of type
; structure called pfo_fstruct_array (see pfo_fstruct__define for a
; detailed listing of all of the info stroed).  The position of a
; function in the pfo_fstruct_array determines its unique number, the
; all-important "fnum."  The "fnum" ends up being the integer part of
; the tag parinfo.pfo.ftype.  In this way, fnum helps the pfo system
; (pfo_funct, in particular) group parinfo elements together to form a
; function.

; The pfo_fstruct_array is stored either in the pfo_obj which stores
; the parinfo you are working with or in the PFO COMMON block, if you
; are not working with objects.

; This procedure helps extract information from the pfo_fstruct array.
; You might find it more convenient to use the higher-level routines
; pfo_fname, pfo_fnum, and pfo_fnpars if you want to in-line your code.

; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

;	fname (in/out): function name
;	fnum (in/out): integer part of the tag parinfo.pfo.ftype
;	fnpars (output): number of parameters in the function (0 = unknown)
;	fdescr (output): function documentation
;	pfo_fstruct_descr (output): documentation of the structure
;		that contains fname, fnum, and fdescr.

;	pfo_obj: pfo_obj that is storing the pfo_fstruct_array.  If
;	not defined, the PFO COMMON block is queried

;       fstruct_array (output): the array which stores the fname, fnum,
;       and fdescr can be returned on this keyword to ease import of
;       parinfos formed outside of a pfo_obj-enabled system


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
; $Id: pfo_finfo.pro,v 1.3 2011/11/18 16:16:48 jpmorgen Exp $
;
; $Log: pfo_finfo.pro,v $
; Revision 1.3  2011/11/18 16:16:48  jpmorgen
; Modify error behavior
;
; Revision 1.2  2011/08/02 18:27:51  jpmorgen
; Release to Tom
; Take out /no_copy stuff, enable pfo_obj to extract fstruct_array in
; case user started making parinfos outside of pfo_obj system.
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
pro pfo_finfo, $
   fname=fname, $
   fnum=fnum, $
   fnpars=fnpars, $
   fdescr=fdescr, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   pfo_obj=pfo_obj, $
   fstruct_array=fstruct_array

  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Errors should be pretty well documented.  Return to calling code
  ;; when a problem occurs
  ON_ERROR, !tok.return

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'USAGE: pfo_finfo[, fname=fname] [,fnum=fnum] [,fnpars=fnpars] [, fdescr=fdescr] [, pfo_obj=pfo_obj] [, pfo_fstruct_descr=pfo_fstruct_descr] [, fstruct_array=fstruct_array]'
     endif
  endif ;; not debugging

  ;; Extract the fstruct stuff from pfo_obj, if specified.
  if N_elements(pfo_obj) gt 0 then begin
     pfo_obj->get_property, $
       pfo_fstruct_array=fstruct_array, $
       pfo_fstruct_descr=fstruct_descr
  endif else begin
     ;; Check to see if we are in object oriented-only mode but the
     ;; user forgot to pass pfo_obj
     if keyword_set(!pfo.objects_only) then $
       message, 'ERROR: !pfo.objects_only = ' + strtrim(!pfo.objects_only, 2) + '.  pfo_finfo was called, but not passed an object.'

     ;; If we made it here, we were not handed an object and that is
     ;; OK.  Extract fstruct stuff from our PFO COMMON block.  Do it
     ;; in a way that there is no cross-talk between objects and
     ;; non-object mode, if the user is mixing and matching.  Also, we
     ;; have a name conflict with pfo_fstruct_descr as a return
     ;; keyword, so move our common block name for that out of the
     ;; way, temporarily.
     COMMON PFO, pfo_fstruct_array, common_pfo_fstruct_descr, $
       parinfo_template, parinfo_descr

     if N_elements(pfo_fstruct_array) gt 0 then $
       fstruct_array = pfo_fstruct_array
     if N_elements(common_pfo_fstruct_descr) gt 0 then $
       fstruct_descr = common_pfo_fstruct_descr

  endelse ;; object-oriented

  ;; Handle our fnum case first, since it is the fastest
  if N_elements(fnum) ne 0 then begin
     ;; Check to see if the user passed us a bogus fnum, since they
     ;; had a defined variable, but really want to differ to fname for
     ;; looking up fnum
     if fnum ne !tok.nowhere then begin

        ;; Defaults, in case we have an error or no match
        ;; Use the initialization in pfo_fstruct_it
        blank  = pfo_struct_new('pfo_fstruct')
        fname  = blank.fname
        fnpars = blank.fnpars
        fdescr = blank.fdescr

        ;; Check fnum to see if it is sensible.
        if 0 le fnum and fnum lt N_elements(fstruct_array) then begin

           ;; Sanity check consistency of fname and fnum, if both specified
           if keyword_set(fname) then $
             if strupcase(fstruct_array[fnum].fname) ne strupcase(fname) then $
               message, 'ERROR: you specified fnum=' + strtrim(fnum, 2) + ' and fname=' + strtrim(fname, 2) + ' but that fnum is assocated with ' + strtrim(fstruct_array[fnum].fname, 2) + '.  Maybe you should not use both or set fnum = ' + strtrim(!tok.nowhere, 2) + ' or fname to '''' to get straight which one you really want.'

           ;; If we made it here, we have a valid fnum.  Copy values
           ;; into each keyword the user has specifically asked for.
           if arg_present(fname            ) or N_elements(fname            ) ne 0 then fname  = fstruct_array[fnum].fname
           if arg_present(fnpars           ) or N_elements(fnpars           ) ne 0 then fnpars = fstruct_array[fnum].fnpars
           if arg_present(fdescr           ) or N_elements(fdescr           ) ne 0 then fdescr = fstruct_array[fnum].fdescr 
           if N_elements(fstruct_descr) ne 0 and (arg_present(pfo_fstruct_descr) or N_elements(pfo_fstruct_descr) ne 0) then pfo_fstruct_descr = fstruct_descr

        endif else begin
           ;; fnum out of range
           message, 'ERROR: fnum = ' + strtrim(fnum, 2) + ' not initialized in system'
        endelse

        ;; If we have no match, we will be quietly returning with
        ;; "blank" values as output

        return

     endif ;; fnum is not !tok.nowhere
  endif ;; fnum

  ;; Given fname, find everything else.
  if keyword_set(fname) then begin
     ;; Defaults, in case we find nothing
     fnum = !tok.nowhere
     ;; Use the initialization in pfo_fstruct_it as our definition
     ;; of "nothing"
     blank = pfo_struct_new('pfo_fstruct')
     fnpars = blank.fnpars
     fdescr = blank.fdescr 
     ;; Check to see if we have a valid pfo_fstruct_array yet.  If
     ;; not, quietly return.  Note that this return doesn't mess
     ;; up our pfo_parinfo_obj, since there is no info to copy back
     ;; into it.
     if N_elements(fstruct_array) eq 0 then $
       return

     ;; Make sure fname is a string
     if size(/type, fname) ne !tok.string then $
       message, 'ERROR: fname must be a string'

     ;; If we made it here, we might have a valid fnum-fname match.
     ;; WARNING: it is tempting to use pfo_fnum or pfo_finfo, but the
     ;; only copy of the information these routines rely on resides in
     ;; this routine!  Fortunately, we don't require much.
     fnum = where(strupcase(fname) eq strupcase(fstruct_array.fname), count)
     if count gt 1 then $
       message, 'ERROR: more than one function of name ' + strtrim(fname, 2) + ' found in pfo_fstruct_array.  THIS SHOULD NOT HAPPEN!'
     ;; Check for a proper match
     if count eq 1 then begin
        ;; Where returns a vector (one element), we want a scaler
        fnum = fnum[0]
        ;; Copy values into keywords, if the user has specifically
        ;; asked for it.
        if arg_present(fname            ) or N_elements(fname            ) ne 0 then fname  = fstruct_array[fnum].fname
        if arg_present(fnpars           ) or N_elements(fnpars           ) ne 0 then fnpars = fstruct_array[fnum].fnpars
        if arg_present(fdescr           ) or N_elements(fdescr           ) ne 0 then fdescr = fstruct_array[fnum].fdescr 
           if N_elements(fstruct_descr) ne 0 and (arg_present(pfo_fstruct_descr) or N_elements(pfo_fstruct_descr) ne 0) then pfo_fstruct_descr = fstruct_descr
     endif ;; proper match 

     ;; If we have no match, we will be quietly returning with "blank"
     ;; values as output

     return

  endif ;; fname

  ;; In order to allow smooth return of fstruct_array, don't
  ;; raise an error if we get here.

end
