; +
; $Id: pfo_fcreate.pro,v 1.4 2010/07/17 19:00:27 jpmorgen Exp $

; pfo_fcreate.pro 

;; Create an array of blank parinfo records for the desired pfo
;; function.  Pass a parinfo template if you want something other than
;; the default pfo parinfo structure.  Could have also been called
;; pfo_parinfo_create.  Only handle the command line parameters
;; necessary for defining a default structure.  Calls pfo_fmod with
;; the _EXTRA arguments to take care of the rest.

; -

function pfo_fcreate, ftype, parinfo_template=parinfo_template, $
  value=value, inaxis=inaxis, outaxis=outaxis, fop=fop, $
  format=format, eformat=eformat, _EXTRA=extra
                     
  init = {pfo_sysvar}

  ;; Make sure we have a parinfo template that has the pfo structure
  ;; in it.  If parinfo_template was specified, but has no pfo
  ;; structure, append one.
  if NOT keyword_set(parinfo_template) then $
    parinfo_template = !pfo.parinfo
  junk = where(tag_names(parinfo_template) eq 'PFO', ispfo)
  if ispfo eq 1 then begin
     parinfo = parinfo_template
  endif else begin
     pfo = {pfo : {pfo_struct}}
     ;; Make new parinfo an anonymous structure, in case we are
     ;; defining many such structures
     parinfo = struct_append(parinfo_template, pfo)
  endelse

  ;; Set the pfo fields of the new parinfo structure to the default
  ;; values
  struct_assign, !pfo.parinfo, parinfo, /nozero

  ;; Default command line parameters
  if N_elements(ftype) eq 0 then $
    ftype = !pfo.null
  if NOT keyword_set(inaxis) then $
    inaxis = !pfo.Xaxis
  if NOT keyword_set(outaxis) then $
    outaxis = !pfo.Yaxis
  if NOT keyword_set(fop) then $
    fop = !pfo.add
  ;; Make eformat the same as format, so columns come out nice.  For E
  ;; format types, this will be fine.  For F formats, users will
  ;; probably want something different, but they would have to specify
  ;; that anyway.
  if keyword_set(format) and NOT keyword_set(eformat) then $
    eformat = format

  if N_elements(ftype) gt 1 then begin
     message, 'ERROR: for now, you have to build functions one at a time.  If you can figure out a good way to build multiple functions, handling all the attendant possibilities with the other command line input parameters, here is where you would insert that code.'
  endif

  ;; CURRENTLY DEFINED FUNCTIONS
  if ftype lt !pfo.null or ftype gt !pfo.last_fn then $
    message, 'ERROR: function type ' + string(ftype) + ' not recognized.  Check the calling code and/or modify pfo_sysvar__define.pro and write your own primitive.'

  ;; Use the primitives to define themselves.  In case we are passing
  ;; things to the primitives (e.g. ptype= and ftype= for sso, pass
  ;; _EXTRA).  Put the results into the variable parinfo.  Use IDL's
  ;; ability to execute commands you build at runtime from strings.
  parinfo = call_function('pfo_' + !pfo.fnames[ftype], $
                          /create, parinfo=parinfo, _EXTRA=extra)

  ;; Set other fields
  
  ;; Default to active (also should be done in primitives except pfo_null)
  parinfo.pfo.status = !pfo.active

  ;; Copy in initial values if the user specified them on the command
  ;; line.  Don't complain about not enough supplied values, since
  ;; the primitives should define the functions with the parameters in
  ;; the order of importance and with reasonable default values.
  if N_elements(value) ne 0 then begin
     npfo = N_elements(parinfo)
     nval = N_elements(value)
     idx = lindgen(min([npfo, nval]))
     struct_array_assign, parinfo, idx, tagname='value', tagval=value
  endif

  pfo_fmod, parinfo, inaxis=inaxis, outaxis=outaxis, $
            fop=fop, format=format, eformat=eformat, _EXTRA=extra

  return, parinfo

end
