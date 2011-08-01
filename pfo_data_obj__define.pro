;+
; NAME: pfo_data_obj
;
; PURPOSE: Create an object to store data used in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
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
; $Id: pfo_data_obj__define.pro,v 1.1 2011/08/01 19:18:16 jpmorgen Exp $
;
; $Log: pfo_data_obj__define.pro,v $
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-


;;-------------------------------------------------------------------
;; PUBLIC METHODS
;;-------------------------------------------------------------------

function pfo_data_obj::Xin, $
   no_copy=no_copy ;; Dangerous!  This guts this object of its property, so calls to methods referring to that property won't work
  self->get_property, Xin=Xin, no_copy=no_copy
  return, Xin
end

function pfo_data_obj::Yin, $
   no_copy=no_copy ;; Dangerous!  This guts this object of its property, so calls to methods referring to that property won't work
  self->get_property, Yin=Yin, no_copy=no_copy
  return, Yin
end

function pfo_data_obj::Yerr, $
   no_copy=no_copy ;; Dangerous!  This guts this object of its property, so calls to methods referring to that property won't work
  self->get_property, Yerr=Yerr, no_copy=no_copy
  return, Yerr
end


pro pfo_data_obj::get_property, $
   Xin=Xin, $	;; X-axis, usually in natural units
   Yin=Yin, $	;; data Y-axis
   Yerr=Yerr, $	;; 1-sigma error bars on data
   weights=weights, $ ;; weight(s) used to calculate deviates (used in preference to yerr)
   no_copy=no_copy ;; Dangerous!  This guts this object of its property, so calls to methods referring to that property won't work

  ;; Xin
  if arg_present(Xin) and $
    N_elements(*self.pXin) ne 0 then begin
     if keyword_set(no_copy) then $
       Xin = temporary(*self.pXin) $
     else $
       Xin = *self.pXin
  endif ;; Xin

  ;; Yin
  if arg_present(Yin) and $
    N_elements(*self.pYin) ne 0 then begin
     if keyword_set(no_copy) then $
       Yin = temporary(*self.pYin) $
     else $
       Yin = *self.pYin
  endif ;; Yin

  ;; Yerr
  if arg_present(Yerr) and $
    N_elements(*self.pYerr) ne 0 then begin
     if keyword_set(no_copy) then $
       Yerr = temporary(*self.pYerr) $
     else $
       Yerr = *self.pYerr
  endif ;; Yerr

  ;; weights
  if arg_present(weights) and $
    N_elements(*self.pweights) ne 0 then begin
     if keyword_set(no_copy) then $
       weights = temporary(*self.pweights) $
     else $
       weights = *self.pweights
  endif ;; weights

end

pro pfo_data_obj::set_property, $
   Xin=Xin, $	;; X-axis, usually in natural units
   Yin=Yin, $	;; data Y-axis
   Yerr=Yerr, $	;; 1-sigma error bars on data
   weights=weights, $ ;; weight(s) used to calculate deviates (used in preference to yerr)
   no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

  init = {tok_sysvar}

  ;; Xin
  if N_elements(Xin) gt 0 then begin
     if size(/type, Xin) ne !tok.double then $
        message, /INFORMATIONAL, 'NOTE: converting Xin to double precision'
     if keyword_set(no_copy) then $
       *self.pXin = double(temporary(Xin)) $
     else $
       *self.pXin = double(Xin)
  endif ;; Xin

  ;; Yin
  if N_elements(Yin) gt 0 then begin
     if size(/type, Yin) ne !tok.double then $
        message, /INFORMATIONAL, 'NOTE: converting Yin to double precision'
     if keyword_set(no_copy) then $
       *self.pYin = double(temporary(Yin)) $
     else $
       *self.pYin = double(Yin)
  endif ;; Yin

  ;; Yerr
  if N_elements(Yerr) gt 0 then begin
     if size(/type, Yerr) ne !tok.double then $
        message, /INFORMATIONAL, 'NOTE: converting Yerr to double precision'
     if keyword_set(no_copy) then $
       *self.pYerr = double(temporary(Yerr)) $
     else $
       *self.pYerr = double(Yerr)
  endif ;; Yerr

  ;; weights
  if N_elements(weights) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.pweights = double(temporary(weights)) $
     else $
       *self.pweights = double(weights)
  endif ;; weights

end

;; The new_data method works like init when it comes to using the
;; set_property method to do the real work.
pro pfo_data_obj::new_data, $
   p0, $	;; Xin or Yin
   p1, $	;; Yin
   p2, $	;; Yerr
   quiet=quiet, $ ;; don't reportchange
   _REF_EXTRA=extra ;; pass on to set_property method

  ;; We are called by routines which always mention p0, p1, p2, even
  ;; if they ar enot defined.  This means we cannot use N_params() to
  ;; see how the user has specified [[Xin,] Yin, [Yerr]].
  N_positional_params = 0
  if N_elements(p0) gt 0 then $
     N_positional_params += 1
  if N_elements(p1) gt 0 then $
     N_positional_params += 1
  if N_elements(p2) gt 0 then $
     N_positional_params += 1

  ;; Handle our positional and keyword parameters simultaneously with
  ;; set_property.  This allows the /no_copy keyword to operate on all
  ;; parameters and keywords simultaneously.  Let set_property handle
  ;; the type conversion to double and any attendant messaging
  case N_positional_params of
     0: begin
        ;; It is a bad idea to call the init method without providing
        ;; data, since all of the rest of the 
        if NOT keyword_set(quiet) then $
           message, /INFORMATIONAL, 'WARNING: no positional parameters provided to pfo_data__init.  Xin, Yin, and Yerr are not initalized!  Remember to initialize ALL THREE when it comes time to use the pfo_obj.  Use /quiet switch or pfo_quiet to supress this message.'
        self->set_property,_EXTRA=extra
     end
     1: begin
        ;; Assume y-axis was specified in the p0 slot.  Make Xin read
        ;; in channels starting from 0, of the same type as Yin
        pfo_idx, p0, idx=Xin, type=size(/type, p0)
        ;; Assume Yerr is Poisson.
        self->set_property, Xin=Xin, $
                            Yin=p0, $
                            Yerr=sqrt(p0), $
                            _EXTRA=extra
     end
     2: begin
        ;; Assume Xin and Yin are specified.  Assume Yerr is Poisson
        ;; --> Could use some options to set statistics
        self->set_property, Xin=p0, $
                            Yin=p1, $
                            Yerr=sqrt(p1), $
                            _EXTRA=extra
     end
     3: begin
        ;; No ambiguities
        self->set_property, Xin=p0, $
                            Yin=p1, $
                            Yerr=p2, $
                            _EXTRA=extra
     end
  endcase 

end

function pfo_data_obj::descr
  return, *self.pdata_obj_descr
end

pro pfo_data_obj::cleanup
  ptr_free, self.pdata_obj_descr
  ptr_free, self.pXin
  ptr_free, self.pYin
  ptr_free, self.pYerr
  ptr_free, self.pweights

end ;; pfo_data_obj::cleanup

;; Handle the initialization of the pfo_data_obj in such a way as we
;; always have
function pfo_data_obj::init, $
   p0, $	;; Xin or Yin
   p1, $	;; Yin
   p2, $	;; Yerr
   _REF_EXTRA=extra

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Object not properly initialized ', /CONTINUE
        return, 0
     endif
  endif ;; not debugging

  ;; Create our documentation string
  self.pdata_obj_descr $
     = ptr_new( $
     {README	: 'pfo_data_obj stores data (e.g. Xin, Yin, Yerr) used by the PFO system', $
      PROPERTY	: 'Xin, Yin, Yerr', $
      METHODS	: 'set_property, get_property, descr(), Xin(),  Yin(), Yerr()'} $
              )

  ;; Turn our null reference pointers into undefined variables
  self.pXin = ptr_new(/allocate_heap)
  self.pYin = ptr_new(/allocate_heap)
  self.pYerr = ptr_new(/allocate_heap)
  self.pweights = ptr_new(/allocate_heap)

  ;; Use new_data method to handle positional and keyword parameters
  self->new_data, p0, p1, p2, _EXTRA=extra

  return, 1

end

pro pfo_data_obj__define

  ;; Store arrays in the form of heap variables so that they can
  ;; change over the lifetime of this object.  This als makes sure
  ;; that large arrays sit in only one place in memory (the heap)
  objectClass = $
     {pfo_data_obj, $
      pdata_obj_descr:	ptr_new(), $   	;; Pointer to description structure
      pXin      :       ptr_new(), $ 	;; Pointer to input X-axis
      pYin      :       ptr_new(), $ 	;; Pointer to input Y-axis
      pYerr     :       ptr_new(), $ 	;; Pointer to input Y-axis errors
      pweights	:	ptr_new() $ 	;; Pointer to weight(s) used to calculate deviates (used in preference to yerr)
     }

end
