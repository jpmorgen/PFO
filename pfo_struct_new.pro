;+
; NAME: pfo_struct_new
;
; PURPOSE: create a new, properly initialized structure variable
;
; CATEGORY: [pfo] structures
;
; CALLING SEQUENCE: my_struct = pfo_struct_new(struct_name,
; [keyword args to struct_name__init])
;
; DESCRIPTION: 

; IDL has the annoying habit of zeroing out the elements of a new
; structure when it is created implicitly (e.g. a = {my_struct}
; returns a with null values in all of the tags no matter what
; my_struct__define sets them to).  This routine provides a solution
; to this problem: a standardized method for creating and initializing
; a new structure.  It is designed to resemble the way objects are
; created and populated with object_new(obj), obj__define, and the
; obj-->init() method.  This ultimately allows the user to keep all
; creation and initialization code together in one place, which makes
; for better, more modular programs.  At least that is the idea.

; To use this system, create a file, my_struct__define, just like you
; normally would to define a structure, but before the
; my_struct__define procedure appears in your file, define a function,
; my_struct__init.  pfo_struct_new calls the __define procedure, to
; make sure the structure is defined (and my_struct__define.pro is
; compiled) and then calls the __init function to initialize it.

; This works equally well for named and anonymous structures, though
; you will need to adjust the code in your __define file accordingly
; (hint: for anonymous structures, the __define pro doesn't need to do
; anything).  That said, named structures are recommended.

; The system optionally goes on to mimic object-oriented programming
; practices.  You can create a pair of routines <my_struct>__set_tag
; and <my_struct>__get_tag, as per the example in
; pfo_struct__define.pro.  These routines are used by the
; pfo_struct_setget_tag routine to enable the transparent conversion
; of keywords to and from tag assignments.  Other methods are also
; possible, such as __update, which is called by pfo_struct_update

; USE WITH PARINFO, DOCUMENTED STRUCTURES: The routine
; pfo_struct_append has some handy features which use pfo_struct_new
; with a mind to maintaining the arrays of parinfo structures that are
; used in the PFO system.  pfo_struct_append also maintains a parallel
; structure of documentation for your struct.  Note that the naming
; convention in pfo_struct_append is slightly different than
; pfo_struct_new: <my_struct> becomes <tag>_struct.  In other words,
; you get to be lazy and refer to things by the top-level tag names in
; the parinfo.  Confused?  See the EXAMPLE.  The routine
; pfo_parinfo_template makes use of the documentation feature.

; INPUTS: struct_name: a string containing the name of the structure.
; The structure might actually be anonymous

; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:

; All keyword parameters are passed to the __init method via the
; _REF_EXTRA mechanism.  They can therefore be input or output.  Input
; only is recommended.

; OUTPUTS: returns initialized structure
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
; parinfo = pfo_struct_new('mpfit_parinfo_struct')
; help, parinfo, /stru
; pfo_struct_append, parinfo, 'PFO'
; help, parinfo, /stru

; mpfit_parinfo_struct__define.pro contains an example of how to make
; a set of top-level parinfo tags.  pfo_parinfo_struct__define.pro
; contains the example for a top-level tag that is a struct.

;
; MODIFICATION HISTORY:
;
; $Id: pfo_struct_new.pro,v 1.2 2011/08/01 18:39:37 jpmorgen Exp $
;
; $Log: pfo_struct_new.pro,v $
; Revision 1.2  2011/08/01 18:39:37  jpmorgen
; First reasonably functional version of pfo_obj
; Added resolve_routine
;
; Revision 1.1  2011/02/10 22:33:24  jpmorgen
; Initial revision
;
;-
function pfo_struct_new, struct_name, _REF_EXTRA=extra
  init = {pfo_sysvar}
  init = {tok_sysvar}
  if !pfo.debug le 0 then begin
     ;; Return to the calling routine with our error
     ON_ERROR, !tok.return
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /CONTINUE, 'USAGE: struct=pfo_struct_new(struct_name, [keyword args to my_struct__init])'
        message, 'USAGE: where the file struct_name__define.pro contains the initialization (function struct_name__init) and definition (pro struct_name__define) of struct_name, in that order.'
     endif
  endif ;; not debugging

  ;; Call the __define procedure, which compiles the file which should
  ;; have both the __define procedure and the __init function in it.

  ;; Allow keyword parameters to be passed to __define, so that we can
  ;; do things like switch between float and double, while still
  ;; keeping the same tags
  
  define_proc = struct_name + '__define'
  ;; Compile define_proc file so that routine_info can work
  resolve_routine, /no_recompile, define_proc
  ;; Use IDL's routine_info function to determine if we have keywords
  ri_params = routine_info(define_proc, /parameters)
  if ri_params.num_kw_args gt 0 then $
     call_procedure, define_proc, _EXTRA=extra $
  else $
     call_procedure, define_proc

  ;; Use IDL's routine_info function to determine if we have keywords
  init_funct = struct_name + '__init'
  ri_params = routine_info(init_funct, /functions, /parameters)
  if ri_params.num_kw_args gt 0 then $
    return, call_function(init_funct, _EXTRA=extra) $
  else $
    return, call_function(init_funct)
end
