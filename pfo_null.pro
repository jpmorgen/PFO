; +
; $Id: pfo_null.pro,v 1.3 2011/06/28 21:02:19 jpmorgen Exp $

; pfo_null.pro 

;; This is the pfo null function, which is a useful primitive for
;; creating a single parinfo record and printing the parameter and/or
;; parinfo values of several parinfo records.  If you insist on making
;; it calculate something, it returns Xin *0.

; -

function pfo_null, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                   create=create, print=print, widget=widget, $
                   ytemplate=ytemplate, _EXTRA=extra


  ;; Generic pfo system initialization
  init = {pfo_sysvar}
  init = {tok_sysvar}

  ;; Keep code general: use fn below and just change the token here
  fn = !pfo.null

  ;; /CREATE
  if keyword_set(create) then begin
     if N_elements(idx) ne 0 then $ 
       message, 'ERROR: idx keyword not implemented in /create option'

     ;; Create necessary parinfo structure for this function.  In this
     ;; case, parinfo, if supplied, is a template, possibly other than
     ;; the default !pfo.parinfo.  Other functions should use:
     ;;
     ;; new_parinfo = replicate(pfo_null(/create, parinfo=parinfo), $
     ;; 			!pfo.fnpars[fn])
     ;;
     ;; to create blank parinfo records and get the benefit of the
     ;; error checking here.

     ;; If no parinfo template was supplied, use !pfo.parinfo
     if NOT keyword_set(parinfo) then $
         parinfo = pfo_parinfo_template(required_tags='PFO')

     ;; Make sure new_parinfo is just one element
     new_parinfo = parinfo[0]

     ;; --> THIS IS NOT QUITE WHAT IS GOING ON ANY MORE.  I am doing a
     ;; bit of a change here with respect to the engineering of
     ;; parinfo.  Was "Zero out the pfo and mpfit section of
     ;; new_parinfo, but keep any other stuff.  Assuming
     ;; !pfo.parinfo.active = 0, this turns off the parameter as well.
     ;; Don't forget to turn it back on."
     struct_assign, pfo_parinfo_template(required_tags='PFO'), new_parinfo, /nozero

     ;; Yes, I know returning different types is bad for other
     ;; languages, but it is cool that IDL can do it.
     return, new_parinfo

  endif ;; /CREATE

  ;; /PRINT
  if keyword_set(print) then begin
     
     toprint = ''

     ;; Let IDL do all the error checking, but don't bomb, since it is
     ;; hard to debug.
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: problem with print formatting'
     endif
     
     ;; Handle the case of idx=-1 by returning the null string
     if N_elements(idx) eq 1 then $
       if idx eq -1 then $
       return, ''

     ;; Try to handle all the cases of params, no parinfo, parinfo, no
     ;; params, etc.
     if N_elements(params) eq 0 and N_elements(parinfo) eq 0 then begin
        ;; Allows for the case print, pfo_null(print="Hello world")
        if size(print, /type) eq !tok.string then $
          toprint = toprint + print
        return, toprint
     endif

     ;; Get the benefit of the error checking of pfo_funct_check
     f_idx = $
       pfo_funct_check(!pfo.null, Xin=Xin, params=params, $
                       parinfo=parinfo, idx=idx)
     pidx = where(parinfo[f_idx].mpprint ne 0, nprint)
     if nprint eq 0 then $
       return, toprint
     ;; Unnest indices
     pidx = f_idx[pidx]

     ;; --> I am not sure if this is worth it, since I am now doing
     ;; more printing at the pfo_funct level.  Leave the code in for
     ;; now just for the heck of it.

     ;; Handle the case where print gives detailed instructions on how
     ;; to print.  If print is a single string, it is assumed to be a
     ;; format statement for the parameters.  If it as 2 element array
     ;; of string, the first element is the format, the second is the
     ;; list of variables (e.g. parinfo[0].pfo.ID).  The user has to
     ;; know what they are doing, since I do no error checking.
     if size(print, /type) eq !tok.string then begin
        np = N_elements(print)
        if np eq 1 then begin
           ;; This loops though until all the parameters are printed,
           ;; which is probably not what is expected....
           toprint = toprint + string(format=print, params)
           return, toprint
        endif ;; np=1
        if np eq 2 then begin
           ;; Since I am building a parameter at runtime, I have to
           ;; use execute instead of call_function.  --> this fails in
           ;; the IDLVM case, but it is unlikely it will ever be used
           ;; there, since this is pretty esoteric
           command = 'toprint = toprint + string(format=print[0], ' + $
                     print[1] + ')'
           ;; Execute the command we have built and check for an error.
           if NOT execute(command) then begin
              message, command, /CONTINUE
              message, 'ERROR: command shown above failed.'
           endif
           return, toprint
        endif ;; np=2
        message, 'ERROR: print string has improper format'
     endif

     ;; Now handle the more generic printing options
     if print eq !pfo.print then begin
        ;; Just print the parameters according to their pfo.formats
        ;; (if specified)
        for ip=0, N_elements(pidx)-1 do begin
           if ip ne 0 then $
             toprint = toprint + !pfo.separator + ' '
           if keyword_set(parinfo[pidx[ip]].pfo.format) then $
             format = '(' + parinfo[pidx[ip]].pfo.format + ')'
           toprint = toprint + string(params[pidx[ip]], format=format)
        endfor
        return, toprint
     endif
     if print eq !pfo.ppname then begin
        ;; Could have used the ':' format code, but this is just
        ;; about as much pain to code up, since I am not sure how
        ;; many parinfo records I will have
        for ip=0, N_elements(pidx)-1 do begin
           if ip ne 0 then $
             toprint = toprint + ', '
           toprint = toprint + parinfo[pidx[ip]].parname
        endfor
        return, toprint
     endif
     if print ge !pfo.pmp then begin
        for ip=0, N_elements(pidx)-1 do begin
           ;; Parameter numbers
           if print ge !pfo.pall then $
             toprint = toprint + string(format='(i4)', pidx[ip])

           ;; parname = 
           ;; Build up a formatted 'parname = ' using
           ;; !pfo.pname_width as the runtime format specifier
           pnformat = '(' + 'a' + strtrim(string(!pfo.pname_width),2) + $
                      ')'
           toprint = toprint + string(format=pnformat, $
                                      parinfo[pidx[ip]].parname) + ' = '
           
           ;; param and limits format
           if keyword_set(parinfo[pidx[ip]].pfo.format) then $
             pformat = '(' + parinfo[pidx[ip]].pfo.format + ')'
           ;; Left limit.  I think I want to always print it and
           ;; indicate its use with the delimiters
           toprint = toprint + $
                     string(parinfo[pidx[ip]].limits[!pfo.left], format=pformat)
           
           toprint = toprint + $
                     pfo_delimiter(!pfo.left, params, parinfo, $
                                   pidx[ip], _EXTRA=extra)
           
           ;; params
           toprint = toprint + string(params[pidx[ip]], format=pformat)
           
           ;; +/- error
           toprint = toprint + ' +/- '
           if keyword_set(parinfo[pidx[ip]].pfo.eformat) then $
             eformat = '(' + parinfo[pidx[ip]].pfo.eformat + ')'
           toprint = toprint + $
                     string(parinfo[pidx[ip]].error, format=eformat)

           toprint = toprint + $
                     pfo_delimiter(!pfo.right, params, parinfo, $
                                   pidx[ip], _EXTRA=extra)
           
           toprint = toprint + $
                     string(parinfo[pidx[ip]].limits[!pfo.right], format=pformat)
           
           ;; Additional mpfit stuff
           if keyword_set(parinfo[pidx[ip]].step) then $
             toprint = toprint + ' step=' + string(parinfo[pidx[ip]].step)
           if keyword_set(parinfo[pidx[ip]].mpside) then $
             toprint = toprint + ' mpside=' + string(parinfo[pidx[ip]].mpside)
           if keyword_set(parinfo[pidx[ip]].mpmaxstep) then $
             toprint = toprint +' mpmaxstep=' + string(parinfo[pidx[ip]].mpmaxstep)
           if keyword_set(parinfo[pidx[ip]].tied) then $
             toprint = toprint + ' tied=' + parinfo[pidx[ip]].tied

           ;; Next line for next parameter or end of list
           toprint = toprint + !tok.newline
        endfor ;; each parameter
        return, toprint
     endif
     message, 'ERROR: unknown print type ' + string(print)

  endif ;; /PRINT

  ;; WIDGET
  if keyword_set(widget) then begin
     ;; Get the benefit of the error checking of pfo_funct_check
     f_idx = pfo_funct_check(!pfo.null, Xin=Xin, params=params, $
                             parinfo=parinfo, idx=idx, npar=npar, $
                             /any_status)
     if npar eq 0 then $
       message, 'ERROR: no parameters found.  Is idx set to a bogus value?'
     
     ;; Check to see if we are creating or refilling the widget and do
     ;; some basic setup.
     refillID = parinfo[f_idx[0]].pfo_widget.IDs[!pfo_widget.ID_idx]
     if keyword_set(refillID) then begin
        ;; We wre refilling an existing widget
        ;; Check to see the widget thinks the tlb is the same one we
        ;; were passed.  If there is a mismatch, something is screwy
        widget_control, refillID, get_uvalue=wstate
        if wstate.tlbID ne widget then $
          message, 'ERROR: mismatch between tlb recorded in widget and passed to pfo_null.  Are you getting mixed up with multiple instances of the widget editor for the same segment of parinfo?'
     endif else begin
        ;; Build our widget.  Get the tlb so we can install it into
        ;; the uvalues of all the subwidgets
        widget_control, widget, get_uvalue=wstate
     endelse

     ;; Save off the current value of !pfo_widget.ID_idx so that we
     ;; can reset to that value for each parameter
     first_ID_idx = !pfo_widget.ID_idx

     ;; Step through the parameters one by one.  One row = one
     ;; parameter
     for ip=0, npar-1 do begin
        ;; reset our pointer into pfo_widget.IDs
        !pfo_widget.ID_idx = first_ID_idx
        
        ;; Borrow some code from the printing section.  Set up formats
        ;; for the parameter name and parameter
        pnformat = '(' + 'a' + strtrim(string(!pfo.pname_width),2) + $
                   ')'
        if keyword_set(parinfo[f_idx[ip]].pfo.format) then $
          pformat = '(' + parinfo[f_idx[ip]].pfo.format + ')'

        ;; Parname.  In the generic function, this isn't user
        ;; editable.  Some of the other functions, like pfo_poly might
        ;; want to allow this to change.
        if keyword_set(refillID) then begin
           ;; not editable in pfu_null
        endif else begin
           ;; This is our first widget for this parameter.  Set up the
           ;; row.  Here we assume that widget=parinfo_containerID
           ;; from pfo_funct
           rowID = widget_base(widget, row=1)
           ;; Construct 'parname = '
           toprint = string(format=pnformat, $
                            parinfo[f_idx[ip]].parname) + ' = '
           uvalue = {tlbID: wstate.tlbID}
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] = $
             widget_label(rowID, value=toprint, uvalue=uvalue)
        endelse
        !pfo_widget.ID_idx += 1

        ;; Left limit
        toprint = string(parinfo[f_idx[ip]].limits[!pfo.left], format=pformat)
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=toprint
        endif else begin
           uvalue = {tlbID: wstate.tlbID, side : !pfo_widget.left}
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] = $
             widget_text(rowID, value=toprint, xsize=16, $
                         /editable, /all_events, /kbrd_focus_events, uvalue=uvalue, event_func='pfo_widget_value_event')
        endelse
        !pfo_widget.ID_idx += 1

        ;; Left delimiter
        value = $
          shift(!pfo_widget.delimiters, $
                -1 * pfo_widget_delimiter(!pfo.left, params, $
                                          parinfo, f_idx[ip], pegged=pegged, sensitive=sensitive))
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=value, sensitive=sensitive
        endif else begin 
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] $
             = widget_droplist(rowID, event_func='pfo_widget_delimiter_event', $
                              value=value, uvalue=uvalue, sensitive=sensitive)
        endelse
        !pfo_widget.ID_idx += 1

        ;; Left pegged
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=pegged
        endif else begin 
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] $
             = widget_label(rowID, value=pegged, uvalue=uvalue)
        endelse
        !pfo_widget.ID_idx += 1

        ;; Value
        toprint = string(params[f_idx[ip]], format=pformat)
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=toprint
        endif else begin
           uvalue = {tlbID: wstate.tlbID, side : !pfo_widget.value}
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] = $
             widget_text(rowID, value=toprint, xsize=16, $
                         /editable, /all_events, /kbrd_focus_events, uvalue=uvalue, event_func='pfo_widget_value_event')
        endelse
        !pfo_widget.ID_idx += 1

        ;; +/- we don't save the ID for this, since we are not
        ;;     going to modify it.
        if NOT keyword_set(refillID) then $
          junkID = widget_label(rowID, value='+/-')

        ;; Error
        if keyword_set(parinfo[f_idx[ip]].pfo.eformat) then $
          eformat = '(' + parinfo[f_idx[ip]].pfo.eformat + ')'
        toprint = string(parinfo[f_idx[ip]].error, format=eformat)
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=toprint
        endif else begin
           uvalue = {tlbID: wstate.tlbID}
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] = $
             widget_label(rowID, value=toprint, xsize=60, uvalue=uvalue)
        endelse
        !pfo_widget.ID_idx += 1

        ;; Right delimiter
        value = $
          shift(!pfo_widget.delimiters, $
                -1 * pfo_widget_delimiter(!pfo.right, params, $
                                          parinfo, f_idx[ip], pegged=pegged, sensitive=sensitive))
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=value, sensitive=sensitive
        endif else begin 
           uvalue = {tlbID: wstate.tlbID, side : !pfo_widget.right}
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] $
             = widget_droplist(rowID, event_func='pfo_widget_delimiter_event', $
                              value=value, uvalue=uvalue, sensitive=sensitive)
        endelse
        !pfo_widget.ID_idx += 1

        ;; Right pegged
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=pegged
        endif else begin 
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] $
             = widget_label(rowID, value=pegged, uvalue=uvalue)
        endelse
        !pfo_widget.ID_idx += 1

        ;; Right limit
        toprint = string(parinfo[f_idx[ip]].limits[!pfo.right], format=pformat)
        if keyword_set(refillID) then begin
           widget_control, parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx], $
                           set_value=toprint
        endif else begin
           uvalue = {tlbID: wstate.tlbID, side : !pfo_widget.right}
           parinfo[f_idx[ip]].pfo_widget.IDs[!pfo_widget.ID_idx] = $
             widget_text(rowID, value=toprint, xsize=16, $
                         /editable, /all_events, /kbrd_focus_events, uvalue=uvalue, event_func='pfo_widget_value_event')
        endelse
        !pfo_widget.ID_idx += 1

     endfor 

  endif ;; widget

  ;; CALCULATE

  ;; Get ready to return an axis of 0s with the same array structure
  ;; as Xin.  Since Xin can be integer, but y is usually real, make it
  ;; possible to specify a y-axis template
  if N_elements(ytemplate) eq 0 then $
    ytemplate = !pfo.ytemplate
  
  ;; No Xin means output 0 in whatever type ytemplate is (don't assume
  ;; user supplied 0 value for ytemplate)
  if N_elements(Xin) eq 0 then $
    return, ytemplate * 0

  ;; We have a non-trivial Xin.  Make yaxis the same dimensionality,
  ;; but with the type of ytemplate.  Beware scaler Xin
  if size(Xin, /dimensions) eq 0 then $
    return, 0*ytemplate[0]

  return, make_array(dimension=size(Xin, /dimensions), $
                     type=size(ytemplate, /type))

end

