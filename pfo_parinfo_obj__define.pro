;+
; NAME: pfo_parinfo_obj__define
;
; PURPOSE: define the object that stores "parinfo," the data structure
; that defines the function in the PFO system
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
; $Id: pfo_parinfo_obj__define.pro,v 1.2 2011/08/02 18:15:59 jpmorgen Exp $
;
; $Log: pfo_parinfo_obj__define.pro,v $
; Revision 1.2  2011/08/02 18:15:59  jpmorgen
; Release to Tom
; Init bug fixes, improved messages, improved descr
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-

;; Print parinfo in full format
pro pfo_parinfo_obj::print, $
   _REF_EXTRA=extra

  print, pfo_parinfo_parse(/print, *self.pparinfo, pfo_obj=self, /full, _EXTRA=extra)

end

;; This is a handy hack that calls a procedure, whose name is passed
;; as a string, proc.  It allows external procedures to access
;; parinfo, which can be rather large, by reference.  Proc is called
;; with parinfo as its first positional parameter and optionally
;; parameters p1...p14 and keywords passed by the calling routine.  I
;; wish there was a more general way to handle positional parameters,
;; but I think this is nicer than requiring only keywords.  If you
;; need to do more fiddling with internal self tags other than
;; parinfo, write another method (e.g. parinfo_template)
pro pfo_parinfo_obj::parinfo_call_procedure, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, _REF_EXTRA=extra

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning without doing any additional calculation.', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; We are called by routines which always mention p1, p2, even
  ;; if they are not defined.  This means we cannot use N_params() to
  ;; see how the user has specified [[Xin,] Yin, [Yerr]].
  N_positional_params = 0
  if N_elements(p1) gt 0 then $
     N_positional_params += 1
  if N_elements(p2) gt 0 then $
     N_positional_params += 1
  if N_elements(p3) gt 0 then $
     N_positional_params += 1
  if N_elements(p4) gt 0 then $
     N_positional_params += 1

  case N_positional_params of 
     0: begin
        if N_elements(extra) eq 0 then begin
           call_procedure, proc, *self.pparinfo
        endif else begin
           call_procedure, proc, *self.pparinfo, _EXTRA=extra
        endelse
     end
     1: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1 $
        else $
          call_procedure, proc, *self.pparinfo, p1, _EXTRA=extra
     end
     2: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, _EXTRA=extra
     end
     3: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, _EXTRA=extra
     end
     4: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, _EXTRA=extra
     end
     5: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, _EXTRA=extra
     end
     6: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, _EXTRA=extra
     end
     7: begin
        if N_elements(extra) eq 0 then $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7 $
        else $
          call_procedure, proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7, _EXTRA=extra
     end

     else: message, 'ERROR: too many positional parameters: ' + strtrim(N_params(), 2)
  endcase 

end

function pfo_parinfo_obj::parinfo_call_function, proc, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, _REF_EXTRA=extra

  init = {pfo_sysvar}

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning NaN.', /CONTINUE
        return, !values.d_NAN
     endif
  endif ;; not debugging

  ;; We are called by routines which always mention p1, p2, even
  ;; if they are not defined.  This means we cannot use N_params() to
  ;; see how the user has specified [[Xin,] Yin, [Yerr]].
  N_positional_params = 0
  if N_elements(p1) gt 0 then $
     N_positional_params += 1
  if N_elements(p2) gt 0 then $
     N_positional_params += 1
  if N_elements(p3) gt 0 then $
     N_positional_params += 1
  if N_elements(p4) gt 0 then $
     N_positional_params += 1

  case N_positional_params of 
     0: begin
        if N_elements(extra) eq 0 then begin
           return, call_function(proc, *self.pparinfo)
        endif else begin
           return, call_function(proc, *self.pparinfo, _EXTRA=extra)
        endelse
     end
     1: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1) $
        else $
          return, call_function(proc, *self.pparinfo, p1, _EXTRA=extra)
     end
     2: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, _EXTRA=extra)
     end
     3: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, _EXTRA=extra)
     end
     4: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, _EXTRA=extra)
     end
     5: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, _EXTRA=extra)
     end
     6: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, _EXTRA=extra)
     end
     7: begin
        if N_elements(extra) eq 0 then $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7) $
        else $
          return, call_function(proc, *self.pparinfo, p1, p2, p3, p4, p5, p6, p7, _EXTRA=extra)
     end

     else: message, 'ERROR: too many positional parameters: ' + strtrim(N_params(), 2)
  endcase 

end

function pfo_parinfo_obj::parinfo, $
   no_copy=no_copy ;; Dangerous!  This guts the pfo_parinfo_obj of parinfo, so any additional calls to this object that require knowledge of parinfo will fail!

  self->get_property, parinfo_array=parinfo, no_copy=no_copy
  return, parinfo

end

pro pfo_parinfo_obj::get_property, $
   parinfo_array=parinfo, $
   parinfo_template=parinfo_template, $
   parinfo_descr=parinfo_descr, $
   pfo_fstruct_array=pfo_fstruct_array, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   no_copy=no_copy ;; Dangerous!  This guts the pfo_parinfo_obj of its property, so calls to methods referring to that property won't work

  ;; parinfo
  if (arg_present(parinfo) or N_elements(parinfo) ne 0) and $
    N_elements(*self.pparinfo) ne 0 then begin
     if keyword_set(no_copy) then $
       parinfo = temporary(*self.pparinfo) $
     else $
       parinfo = *self.pparinfo
  endif ;; parinfo

  ;; parinfo_template
  if (arg_present(parinfo_template) or N_elements(parinfo_template)) ne 0 and $
    N_elements(*self.pparinfo_template) ne 0 then begin
     if keyword_set(no_copy) then $
       parinfo_template = temporary(*self.pparinfo_template) $
     else $
       parinfo_template = *self.pparinfo_template
  endif ;; parinfo_template

  ;; parinfo_descr
  if (arg_present(parinfo_descr) or N_elements(parinfo_descr) ne 0) and $
    N_elements(*self.pparinfo_descr) ne 0 then begin
     if keyword_set(no_copy) then $
       parinfo_descr = temporary(*self.pparinfo_descr) $
     else $
       parinfo_descr = *self.pparinfo_descr
  endif ;; parinfo_descr

  ;; pfo_fstruct_array
  if (arg_present(pfo_fstruct_array) or N_elements(pfo_fstruct_array)) ne 0 and $
    N_elements(*self.ppfo_fstruct_array) ne 0 then begin
     if keyword_set(no_copy) then $
       pfo_fstruct_array = temporary(*self.ppfo_fstruct_array) $
     else $
       pfo_fstruct_array = *self.ppfo_fstruct_array
  endif ;; pfo_fstruct_array

  ;; pfo_fstruct_descr
  if (arg_present(pfo_fstruct_descr) or N_elements(pfo_fstruct_descr)) ne 0 and $
    N_elements(*self.ppfo_fstruct_descr) ne 0 then begin
     if keyword_set(no_copy) then $
       pfo_fstruct_descr = temporary(*self.ppfo_fstruct_descr) $
     else $
       pfo_fstruct_descr = *self.ppfo_fstruct_descr
  endif ;; pfo_fstruct_descr

end ;; get_property

pro pfo_parinfo_obj::set_property, $
  parinfo_array=parinfo, $
  parinfo_template=parinfo_template, $
  parinfo_descr=parinfo_descr, $
  pfo_fstruct_array=pfo_fstruct_array, $
  pfo_fstruct_descr=pfo_fstruct_descr, $
  no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

  init = {tok_sysvar}
  init = {pfo_sysvar}

  ;; parinfo
  if N_elements(parinfo) gt 0 then begin
     ;; We can't just copy the parinfo in here without checking
     ;; to see if the user has been registering its functions with our
     ;; *self.ppfo_fstruct_array
     if N_elements(*self.ppfo_fstruct_array) eq 0 then begin
        ;; Our best guess is that the user has been accumulating
        ;; things in PFO COMMON block
        message, /INFORMATIONAL, 'NOTE: reading associations between function numbers and names from PFO COMMON block'
        oobjects_only = !pfo.objects_only
        !pfo.objects_only = !tok.no
        pfo_finfo, fstruct_array=*self.ppfo_fstruct_array
        !pfo.objects_only = oobjects_only
     endif ;; setting up fstruct_array
     if keyword_set(no_copy) then $
       *self.pparinfo = temporary(parinfo) $
     else $
       *self.pparinfo = parinfo
  endif ;; parinfo

  ;; parinfo_template
  if N_elements(parinfo_template) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.pparinfo_template = temporary(parinfo_template) $
     else $
       *self.pparinfo_template = parinfo_template
  endif ;; parinfo_template

  ;; parinfo_descr
  if N_elements(parinfo_descr) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.pparinfo_descr = temporary(parinfo_descr) $
     else $
       *self.pparinfo_descr = parinfo_descr
  endif ;; parinfo_descr

  ;; pfo_fstruct_array
  if N_elements(pfo_fstruct_array) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.ppfo_fstruct_array = temporary(pfo_fstruct_array) $
     else $
       *self.ppfo_fstruct_array = pfo_fstruct_array
  endif ;; pfo_fstruct_array

  ;; pfo_fstruct_descr
  if N_elements(pfo_fstruct_descr) gt 0 then begin
     if keyword_set(no_copy) then $
       *self.ppfo_fstruct_descr = temporary(pfo_fstruct_descr) $
     else $
       *self.ppfo_fstruct_descr = pfo_fstruct_descr
  endif ;; pfo_fstruct_descr

end ;; set_property

function pfo_parinfo_obj::descr
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

  descr = *self.ppfo_parinfo_obj_descr
  if pfo_struct_tag_present(descr, 'superclasses') then begin
     for isc=0, N_elements(descr.superclasses)-1 do begin
        sc = descr.superclasses[isc]
        scd = call_method(sc+'::descr', self)
        pfo_struct_append, descr, create_struct(sc, scd)
     endfor ;; each superclass
  endif ;; any superclasses

  return, descr

end

pro pfo_parinfo_obj::cleanup
  ptr_free, self.ppfo_parinfo_obj_descr
  ptr_free, self.pparinfo	
  ptr_free, self.pparinfo_template
  ptr_free, self.pparinfo_descr
  ptr_free, self.ppfo_fstruct_array
  ptr_free, self.ppfo_fstruct_descr
end

function pfo_parinfo_obj::init, $
   parinfo_array=parinfo_array, $
   parinfo_template=parinfo_template, $
   parinfo_descr=parinfo_descr, $
   pfo_fstruct_array=pfo_fstruct_array, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.

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
  self.ppfo_parinfo_obj_descr $
     = ptr_new( $
     {README	: 'pfo_parinfo_obj stores the parinfo and associated information used by the PFO system', $
      METHODS	: 'parinfo, print, parinfo_call_procedure, parinfo_call_function'} $
              )
  ;; Grab a decent guess at what our property is from the list of
  ;; keywords in our get_property method
  ri = routine_info('pfo_parinfo_obj::get_property', /parameters)
  property = ri.kw_args
  good_idx = where(stregex(property, '_EXTRA') lt 0, count)
  if count ne 0 then $
     pfo_struct_append, *self.ppfo_parinfo_obj_descr, $
                        {PROPERTY: property[good_idx]}

  ;; Turn our null reference pointers into undefined variables
  self.pparinfo = ptr_new(/allocate_heap)
  self.pparinfo_template = ptr_new(/allocate_heap)
  self.pparinfo_descr = ptr_new(/allocate_heap)
  self.ppfo_fstruct_array = ptr_new(/allocate_heap)
  self.ppfo_fstruct_descr = ptr_new(/allocate_heap)

  ;; Call our set_property routine to convert any keywords to property
  self->set_property, $
   parinfo_array=parinfo_array, $
   parinfo_template=parinfo_template, $
   parinfo_descr=parinfo_descr, $
   pfo_fstruct_array=pfo_fstruct_array, $
   pfo_fstruct_descr=pfo_fstruct_descr, $
   no_copy=no_copy ;; Dangerous!  This makes the variables passed as keywords undefined in the calling routine.


  ;; If we made it here, our object should be initialized properly
  return, 1

end

pro pfo_parinfo_obj__define

  objectClass = $
    {pfo_parinfo_obj, $
     ppfo_parinfo_obj_descr:ptr_new(), $ ;; Pointer to description structure
     pparinfo	:	ptr_new(), $ ;; Pointer to parinfo array (function definition)
     pparinfo_template: ptr_new(), $ ;; Pointer to template for creating new parinfo records
     pparinfo_descr:	ptr_new(), $ ;; Pointer to structure that documents the parinfo structure
     ppfo_fstruct_array:ptr_new(), $ ;; Pointer to fstruct_array, which associates function names in parinfo to fnums (integer part of parinfo.pfo.ftype).  Also has number of parameters in function and function definition
     ppfo_fstruct_descr:ptr_new()  $ ;; Pointer to documentation of fstruct_array
     }
     
     
end
