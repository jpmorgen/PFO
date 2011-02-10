;+
; NAME: pfo_struct_new
;
; PURPOSE: create a new, structure variable initialized to
; (optionally) non-null values
;
; CATEGORY: structures
;
; CALLING SEQUENCE: my_struct = pfo_struct_new(struct_name, [keyword
; args to struct_name__init])
;
; DESCRIPTION: 

; IDL has the annoying habit of zeroing out the elements of a new
; structure when it is created implicitly (e.g. a = {my_struct}).
; This routine provides a standardized method for creating a new
; structure and initializing it.  It is designed to resemble the way
; objects are created and populated with the init() method.  This
; allows the user to keep all creation and initialization code
; together in one place.

; To use this system, create a file my_struct__define, just like you
; normally would, but before the my_struct__define procedure is
; defined, you define a function, my_struct__init.  pfo_struct_new
; calls the __define procedure, to make sure the structure is defined
; and then the __init function to initialize it.

; INPUTS: struct_name: a string containing the name of the structure.
; The structure might actually be anonymous

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
; EXAMPLE: parinfo = pfo_struct_new('mpfit_parinfo')
; See mpfit_parinfo for further discussion
;
; MODIFICATION HISTORY:
;
; $Id: pfo_struct_new.pro,v 1.1 2011/02/10 22:33:24 jpmorgen Exp $
;
; $Log: pfo_struct_new.pro,v $
; Revision 1.1  2011/02/10 22:33:24  jpmorgen
; Initial revision
;
;-
function pfo_struct_new, struct_name, _EXTRA=extra
  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, !error_state.msg, /CONTINUE
        message, /CONTINUE, 'USAGE: struct=pfo_struct_new(struct_name, [keyword args to my_struct__init])'
        message, 'USAGE: where the file struct_name__define.pro contains the initialization (function struct_name__init) and definition (pro struct_name__define) of struct_name, in that order.'
     endif
  endif ;; not debugging

  ;; Call the __define procedure, which compiles the file which should
  ;; have both the __define procedure and the __init function in it
  call_procedure, struct_name + '__define'

  ;; Give ourselves two tries
  CATCH, err
  if err ne 0 then begin
     CATCH, /CANCEL     
     return, call_function(struct_name + '__init')
  endif
  return, call_function(struct_name + '__init', _EXTRA=extra)
end
