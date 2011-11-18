;+
; NAME: pfo_obj
;
; PURPOSE: Create the central object in the PFO system
;
; CATEGORY: PFO
;
; CALLING SEQUENCE: pfo_obj = pfo_obj_new([[Xin,] Yin [Yerr]], keywords)
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
; $Id: pfo_obj__define.pro,v 1.6 2011/11/18 15:52:50 jpmorgen Exp $
;
; $Log: pfo_obj__define.pro,v $
; Revision 1.6  2011/11/18 15:52:50  jpmorgen
; Edit method now uses pfo_fit
;
; Revision 1.5  2011/09/22 23:48:55  jpmorgen
; Add edit method.  Start of integrated pfo_fit widget interface!
;
; Revision 1.4  2011/09/08 20:07:08  jpmorgen
; Cleaned up/created update of widgets at pfo_parinfo_obj level
;
; Revision 1.3  2011/09/01 22:14:18  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:28:25  jpmorgen
; Release to Tom
; Improve descr
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-


;; Edit method brings up pfo_fit_widget
pro pfo_obj::edit, $
   _REF_EXTRA=extra
  pfo_fit, pfo_obj=self, _EXTRA=extra
end

;; Each inherited class should have a descr method.
function pfo_obj::descr

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.', /CONTINUE
        return, 'Not documented.'
     endif
  endif ;; not debugging

  descr = *self.ppfo_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

end

pro pfo_obj::cleanup
  ptr_free, self.ppfo_obj_descr
  self->pfo_mpfit_obj::cleanup
end

function pfo_obj::init, $
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
  self.ppfo_obj_descr $
     = ptr_new( $
     {README	: 'pfo_obj is the default top-level object of the PFO system', $
      SUPERCLASSES: 'pfo_mpfit_obj', $
      PROPERTY	: '', $
      METHODS	: ''} $
              )

  ;; Call our superclass init methods
  ok = self->pfo_mpfit_obj::init(p0, p1, p2, _EXTRA=extra)
  if NOT ok then $
     return, 0

  return, 1

end


pro pfo_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  objectClass = $
     {pfo_obj, $
      ppfo_obj_descr: 	ptr_new(), $	;; Pointer to description structure
      inherits pfo_mpfit_obj  $
      }
end
