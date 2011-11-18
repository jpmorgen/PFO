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
; $Id: pfo_data_obj__define.pro,v 1.4 2011/11/18 16:16:09 jpmorgen Exp $
;
; $Log: pfo_data_obj__define.pro,v $
; Revision 1.4  2011/11/18 16:16:09  jpmorgen
; Modified to handle different invocation order at init
;
; Revision 1.3  2011/09/01 22:29:16  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:22:14  jpmorgen
; Release to Tom
; Fix init method, add printing
;
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
   no_copy=no_copy, $ ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
   _EXTRA=extra ;; to ignore any extra keywords

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
   p0, $        ;; Xin or Yin
   p1, $        ;; Yin
   p2, $        ;; Yerr
   quiet=quiet, $ ;; don't report change
   no_copy=no_copy, $ ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
   _EXTRA=extra ;; pass on any extra keywords (e.g. /no_update) to subclassed set_property methods

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
     0: ;; Hope that the user initializes data with set_property
     1: begin
        ;; Assume y-axis was specified in the p0 slot.  Make Xin read
        ;; in channels starting from 0, of the same type as Yin
        pfo_idx, p0, Xin, type=size(/type, p0)
        ;; Assume Yerr is Poisson.
        self->set_property, $
           Xin=Xin, $
           Yin=p0, $
           Yerr=sqrt(p0), $
           no_copy=no_copy, $ ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
           _EXTRA=extra ;; to ignore any extra keywords
     end
     2: begin
        ;; Assume Xin and Yin are specified.  Assume Yerr is Poisson
        ;; --> Could use some options to set statistics
        self->set_property, $
           Xin=p0, $
           Yin=p1, $
           Yerr=sqrt(p1), $
           no_copy=no_copy, $  ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
           _EXTRA=extra ;; to ignore any extra keywords
     end
     3: begin
        ;; No ambiguities
        self->set_property, $
           Xin=p0, $
           Yin=p1, $
           Yerr=p2, $
           no_copy=no_copy, $ ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
           _EXTRA=extra ;; to ignore any extra keywords
     end
  endcase 

end

;; Print basic information about contents of property
pro pfo_data_obj::print, _EXTRA=extra
  print, 'Xin: ' + strtrim(N_elements(*self.pXin), 2) + ' points'
  print, 'Yin: ' + strtrim(N_elements(*self.pYin), 2) + ' points'
  print, 'Yerr: ' + strtrim(N_elements(*self.pYerr), 2) + ' points'
  print, 'Weights: ' + strtrim(N_elements(*self.pweights), 2) + ' points'
end


function pfo_data_obj::descr

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

  descr = *self.ppfo_data_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_data_obj::cleanup
  ptr_free, self.ppfo_data_obj_descr
  ptr_free, self.pXin
  ptr_free, self.pYin
  ptr_free, self.pYerr
  ptr_free, self.pweights

end ;; pfo_data_obj::cleanup

;; Handle the initialization of the pfo_data_obj.  Make sure we pull
;; down the keywords that we are prepared to deal with.
function pfo_data_obj::init, $
   p0, $        ;; Xin or Yin
   p1, $        ;; Yin
   p2, $        ;; Yerr
   Xin=Xin, $	;; X-axis, usually in natural units
   Yin=Yin, $	;; data Y-axis
   Yerr=Yerr, $	;; 1-sigma error bars on data
   weights=weights, $ ;; weight(s) used to calculate deviates (used in preference to yerr)
   no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

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
  self.ppfo_data_obj_descr $
     = ptr_new( $
     {README	: 'pfo_data_obj stores data (e.g. Xin, Yin, Yerr) used by the PFO system', $
      METHODS	: 'Xin(),  Yin(), Yerr(), print'} $
              )
  ;; Grab a decent guess at what our property is from the list of
  ;; keywords in our get_property method
  ri = routine_info('pfo_data_obj::get_property', /parameters)
  property = ri.kw_args
  good_idx = where(stregex(property, '_EXTRA') lt 0, count)
  if count ne 0 then $
     pfo_struct_append, *self.ppfo_data_obj_descr, $
                        {PROPERTY: property[good_idx]}

  ;; Turn our null reference pointers into undefined variables
  self.pXin = ptr_new(/allocate_heap)
  self.pYin = ptr_new(/allocate_heap)
  self.pYerr = ptr_new(/allocate_heap)
  self.pweights = ptr_new(/allocate_heap)

  ;; Use new_data method to handle positional parameters.  When we are
  ;; inherited into the pfo_obj system, we want to use the overrided
  ;; set_property (particularly calc), but we don't wan to to
  ;; call update, since that triggers to much code that is not
  ;; initialized yet.
  self->new_data, $
     p0, $        ;; Xin or Yin
     p1, $        ;; Yin
     p2, $        ;; Yerr
     no_copy=no_copy, $ ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.
     /no_update
  
  ;; Keyword parameters can override positional parameters.  Make sure
  ;; we use our local set_property routine to avoid nested
  ;; initialization problems with other inherited routines
  self->pfo_data_obj::set_property, $
     Xin=Xin, $ ;; X-axis, usually in natural units
     Yin=Yin, $ ;; data Y-axis
     Yerr=Yerr, $       ;; 1-sigma error bars on data
     weights=weights, $ ;; weight(s) used to calculate deviates (used in preference to yerr)
     no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

  return, 1

end

pro pfo_data_obj__define

  ;; Make sure our system variables are defined for all of the
  ;; routines in this object
  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; Store arrays in the form of heap variables so that they can
  ;; change over the lifetime of this object.  This als makes sure
  ;; that large arrays sit in only one place in memory (the heap)
  objectClass = $
     {pfo_data_obj, $
      ppfo_data_obj_descr:	ptr_new(), $   	;; Pointer to description structure
      pXin      :       ptr_new(), $ 	;; Pointer to input X-axis
      pYin      :       ptr_new(), $ 	;; Pointer to input Y-axis
      pYerr     :       ptr_new(), $ 	;; Pointer to input Y-axis errors
      pweights	:	ptr_new() $ 	;; Pointer to weight(s) used to calculate deviates (used in preference to yerr)
     }

end
