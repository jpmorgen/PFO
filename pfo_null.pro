; +
; $Id: pfo_null.pro,v 1.2 2004/01/15 17:10:34 jpmorgen Exp $

; pfo_null.pro 

;; This is the pfo null function, which is a useful primitive for
;; creating a single parinfo record and printing the parameter and/or
;; parinfo values of several parinfo records.  If you insist on making
;; it calculate something, it returns Xin *0.

; -

function pfo_null, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                   create=create, print=print, $
                   ytemplate=ytemplate, _EXTRA=extra


  ;; Generic pfo system initialization
  init = {pfo_sysvar}

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
       parinfo = !pfo.parinfo
     ;; In the case of a user supplied parinfo, check it has a pfo section
     junk = where(tag_names(parinfo) eq 'PFO', ispfo)
     if ispfo ne 1 then $
       message, 'ERROR: user supplied parinfo template does not contain a PFO structure'

     ;; Make sure new_parinfo is just one element
     new_parinfo = parinfo[0]

     ;; Zero out the pfo and mpfit section of new_parinfo, but keep
     ;; any other stuff.  Assuming !pfo.parinfo.active = 0, this turns
     ;; off the parameter as well.  Don't forget to turn it back on.
     struct_assign, !pfo.parinfo, new_parinfo, /nozero

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
        if size(print, /type) eq 7 then $
          toprint = toprint + print
        return, toprint
     endif

     ;; Get the benefit of the error checking of pfo_funct_check
     f_idx = pfo_funct_check(!pfo.null, Xin=Xin, params=params, $
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
     if size(print, /type) eq 7 then begin
        np = N_elements(print)
        if np eq 1 then begin
           ;; This loops though until all the parameters are printed,
           ;; which is probably not what is expected....
           toprint = toprint + string(format=print, params)
           return, toprint
        endif ;; np=1
        if np eq 2 then begin
           ;; Since I am building a parameter at runtime, I have to
           ;; use execute instead of call_function
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
     if print eq !pfo.pmp then begin
        for ip=0, N_elements(pidx)-1 do begin
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
             toprint = toprint + ' step=' + string(parinfo[ip].step)
           if keyword_set(parinfo[pidx[ip]].mpside) then $
             toprint = toprint + ' mpside=' + string(parinfo[ip].mpside)
           if keyword_set(parinfo[pidx[ip]].mpmaxstep) then $
             toprint = toprint +' mpmaxstep=' + string(parinfo[ip].mpmaxstep)
           if keyword_set(parinfo[pidx[ip]].tied) then $
             toprint = toprint + ' tied=' + parinfo[ip].tied

           ;; Next line for next parameter or end of list
           toprint = toprint + !pfo.newline
        endfor ;; each parameter
        return, toprint
     endif
     message, 'ERROR: unknown print type ' + string(print)

  endif ;; /PRINT

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
  ;; but with the type of ytemplate
  return, make_array(dimension=size(Xin, /dimensions), $
                     type=size(ytemplate, /type))

end
