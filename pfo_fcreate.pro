; +
; $Id: pfo_fcreate.pro,v 1.2 2003/12/19 00:01:05 jpmorgen Exp $

; pfo_fcreate.pro 

;; Create an array of blank parinfo records for the desired pfo
;; function.  Pass a parinfo template if you want something other than
;; the default pfo parinfo structure.  Could have also been called
;; pfo_parinfo_create.

; -

function pfo_fcreate, ftype, parinfo_template=parinfo_template, ID=ID, $
  inaxis=inaxis, outaxis=outaxis, fseq=fseq, fop=fop, $
  value=value, format=format, eformat=eformat, _EXTRA=extra
                     
  init = {pfo_sysvar}

  ;; Defaults and sanity checks
  if NOT keyword_set(parinfo_template) then $
    parinfo_template = !pfo.parinfo
  junk = where(tag_names(parinfo_template) eq 'PFO', ispfo)
  if ispfo ne 1 then $
    message, 'ERROR: parinfo_template does not contain a PFO structure'

  ;; Default command line parameters
  if N_elements(ftype) eq 0 then $
    ftype = !pfo_null
  if NOT keyword_set(inaxis) then $
    inaxis = !pfo.Xaxis
  if NOT keyword_set(outaxis) then $
    outaxis = !pfo.Yaxis
  if NOT keyword_set(fop) then $
    fop = !pfo.add

  if N_elements(ftype) gt 1 then begin
     message, 'ERROR: for now, you have to build functions one at a time.  If you can figure out a good way to build multiple functions, handling all the attendant possibilities with the other command line input parameters, here is where you would insert that code.'
  endif

  ;; CURRENTLY DEFINED FUNCTIONS
  if ftype lt !pfo.null or ftype gt !pfo.last_fn then $
    message, 'ERROR: function type ' + string(ftype) + 'not recognized.  Use ftype=0 and declare something in your own structure if you are handling this function on your own, or modify pfo_sysvar__define.pro and write your own primitive.'

  ;; Use the primitives to define themselves.  Put the results into
  ;; the variable parinfo.  Use IDL's ability to execute commands you
  ;; build at runtime from strings.
  parinfo = call_function('pfo_' + !pfo.fnames[ftype], $
                          /create, parinfo=parinfo_template, _EXTRA=extra)

  ;; Set other fields
  
  ;; Default to active (also should be done in primitives except pfo_null)
  parinfo[*].pfo.status = !pfo.active
  if keyword_set(ID) then $
    parinfo[*].pfo.ID = ID
  if keyword_set(inaxis) then $
    parinfo[*].pfo.inaxis = inaxis
  if keyword_set(outaxis) then $
    parinfo[*].pfo.outaxis = outaxis
  if keyword_set(fop) then $
    parinfo[*].pfo.fop = fop
  if keyword_set(format) then begin
     parinfo[*].pfo.format = format
     ;; Make a the default eformat=format
     if NOT keyword_set(eformat) then eformat = format
  endif
  if keyword_set(eformat) then $
    parinfo[*].pfo.eformat = eformat

  ;; Copy in initial values, if the user specified them on the command
  ;; line.  Don't complain about not enough supplied values, since
  ;; functions should be initialized just fine with not enough
  ;; parameters
  if keyword_set(value) then begin
     npfo = N_elements(parinfo)
     nval = N_elements(value)
     idx = indgen(min([npfo, nval]))
     struct_array_assign, parinfo, idx, tagname='value', tagval=value
  endif

  return, parinfo

end
