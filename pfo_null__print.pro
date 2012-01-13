;+
; NAME: pfo_null_print
;
; PURPOSE: Provide generic printing services for simple PFO functions
;
; CATEGORY: PFO functions
;
; CALLING SEQUENCE: this is called from within pfo_parinfo_parse
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
; $Id: pfo_null__print.pro,v 1.5 2012/01/13 21:05:46 jpmorgen Exp $
;
; $Log: pfo_null__print.pro,v $
; Revision 1.5  2012/01/13 21:05:46  jpmorgen
; Add parameter line printing for additional structures
;
; Revision 1.4  2011/09/16 13:47:51  jpmorgen
; Added col_head output and idx and tied printing
;
; Revision 1.3  2011/09/01 22:23:31  jpmorgen
; Significant improvements to parinfo editing widget, created plotwin
; widget, added pfo_poly function.
;
; Revision 1.2  2011/08/02 18:23:10  jpmorgen
; Release to Tom
; Change to getting Yaxis_init from pfo_obj or !pfo.Yaxis_init
;
; Revision 1.1  2011/08/01 19:18:16  jpmorgen
; Initial revision
;
;-
function pfo_null__print, $
   parinfo, $	  ;; parinfo array (whole array).  This ends up getting ignored in pfo_null__calc
   params=params, $ 	  ;; parameters (entire array).
   idx=idx, $     ;; idx into parinfo of our function
   fname=fname, $ ;; original fname of function in pfo_parinfo_parse
   first_funct=first_funct, $ ;; allows us to display some introductory material (e.g. column headings)
   param_names_only=param_names_only, $ ;; print parameter names only
   brief=brief, $ ;; print the function briefly on one line (just function name and parameters)
   full=full, $ ;; print function name and algebraic info on a line by itself and then one line per parameter
   include_mpstep=include_mpstep, $, ;; also include MPFIT autoderivative step stuff (usually defaults are fine, so these are just zero)
   no_preamble=no_preamble, $ ;; don't include the preamble for first_funct (used in pfo_parinfo_text_cw)
   col_head=col_head, $ ;; (return) column headings (used in, e.g.,  pfo_null__widget for pfo_parinfo_text_cw)
   pfo_obj=pfo_obj, $ ;; This ends up getting ignored in pfo_null__print
   _REF_EXTRA=extra ;; Extra keywords to pass on to *struct__print methods


  ;; Generic pfo system initialization
  init = {pfo_sysvar}
  init = {tok_sysvar}

  toprint = ''

  ;; Handle pfo_debug level.  CATCH errors if _not_ debugging
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning print string as prepared so far,', /CONTINUE
        return, toprint
     endif
  endif ;; not debugging

  ;; Do basic function consistency checking
  pfo_fcheck, parinfo, fname, params=params, idx=idx, pfo_obj=pfo_obj

  ;; Work out our default layout.
  if keyword_set(param_names_only) + keyword_set(brief) + keyword_set(full) eq 0 then $
     full = 1
  if keyword_set(param_names_only) + keyword_set(brief) + keyword_set(full) ne 1 then $
     message, 'ERROR: specify only one keyword: param_names_only, brief, or full.  Default is brief.'

  ;; Get our default Yaxis value from pfo_obj, if specified
  init_Yaxis = !pfo.init_Yaxis
  if obj_valid(pfo_obj) then $
     pfo_obj->get_property, init_Yaxis=init_Yaxis

  ;; Start with our standard parinfo function preamble.  We only print
  ;; this once.  This works out to: "X = Xin; Y = NaN" unless tokens have
  ;; been changed.
  preamble = ''
  if keyword_set(first_funct) then $
     preamble = !pfo.axis_string[!pfo.Xaxis] + ' = ' + !pfo.axis_string[!pfo.Xin] + $
               '; ' + !pfo.axis_string[!pfo.Yaxis] + ' = ' + strtrim(init_Yaxis, 2)

  ;; Check to see if we have anything to print (this is generally not used)
  junk = where(parinfo[idx].mpprint ne 0, count)
  if count eq 0 then $
     return, toprint

  ;; If we made it here, we have something else to print.  We will
  ;; deal with newlines below
  preamble += '; '
  
  ;; Create the equation that shows how this function is combined with
  ;; the rest of the functions in the parinfo e.g. Y = Y + pfo_voigt(...))

  ;; output axis string and equal sign
  oaxs = !pfo.axis_string[parinfo[idx[0]].pfo.outaxis]
  eqsign = ' = '
  ;; If we are not operating on any axis, delete mention of output
  ;; axis and equals sign
  if parinfo[idx[0]].pfo.fop eq !pfo.noop then begin
     oaxs = ''
     eqsign = ''
  endif ;; noop

  ;; Other side of equation (e.g. = Y + ...)
  oaxs2 = oaxs
  ;; If we are replacing the axis, delete its mention on the right
  ;; side of the equation
  if parinfo[idx[0]].pfo.fop eq !pfo.repl then $
     oaxs2 = ''

  ;; Operation
  fos = ' ' + !pfo.fop_string[parinfo[idx[0]].pfo.fop] + ' '
  ;; Remove excess padding in the case we have no operation
  if parinfo[idx[0]].pfo.fop eq !pfo.noop then $
     fos = ''

  ;; For functions handled by pfo_null, all of our input and output
  ;; axes are syncronized for the whole function (e.g. all parameters
  ;; act on X to produce some change in Y).  This is not the case for
  ;; some functions, like pfo_ROI

  ;; Input axis string (e.g. X)
  inaxs = !pfo.axis_string[parinfo[idx[0]].pfo.inaxis]
  infunct = parinfo[idx[0]].pfo.infunct
  ;; Consider the case where we have a transforming function on this
  ;; axis (e.g. exp so that we read in log space)
  if keyword_set(infunct) then $
     inaxs = infunct + '(' + inaxs + ')'
  
  ;; Construct our output function with the fname and its pfo ID.  The
  ;; ID should always be shared between the parameters function
  outfunct = fname + strtrim(parinfo[idx[0]].pfo.pfoID, 2)

  ;; Note open parenthesis, which we will need to close later
  if keyword_set(parinfo[idx[0]].pfo.outfunct) then $
     outfunct = parinfo[idx[0]].pfo.outfunct + '(' + outfunct

  ;; Construct the equation string that is the same for all layouts
  equation_string = oaxs + eqsign + oaxs2 + fos + $
            outfunct + '(' + inaxs + ')('

  ;; Clear out the preamble if the caller doesn't want it
  if keyword_set(no_preamble) then $
     preamble = ''

  ;; PARAM_NAMES_ONLY: just list them on one line them with no other
  ;; information.  List them the same width as their corresponding
  ;; values, so they line up in columns if we print param_values_only
  ;; below them
  if keyword_set(param_names_only) then begin
     ;; Keep everything on one line
     toprint += preamble + ' ' + equation_string
     for ip=0, N_elements(idx)-1 do begin
        if ip ne 0 then $
           toprint = toprint + !pfo.separator + ' '
        ;; Find the width of the parameter format.
        test = string(params[idx[ip]], format=parinfo[idx[ip]].pfo.format)
        format = '(a' + strtrim(strlen(test), 2) + ')'
        toprint += string(parinfo[idx[ip]].parname, format=format)
     endfor

     ;; If I wasn't trying to be fancy with widths, the
     ;; following could be useful
     ;; toprint = strjoin(string(format='(a16, :, ' + '"' + !pfo.separator + ' "' + ')', parinfo[idx].parname))
     ;; return, toprint

  end

  ;; PARAM_VALUES_ONLY: just lists the parameter values on one line
  if keyword_set(brief) then begin
     ;; Keep everything on one line
     toprint += preamble + ' ' + equation_string
     ;; Just print the parameters according to their pfo.formats
     ;; (if specified)
     for ip=0, N_elements(idx)-1 do begin
        if ip ne 0 then $
           toprint = toprint + !pfo.separator + ' '
        if keyword_set(parinfo[idx[ip]].pfo.format) then $
           format = '(' + parinfo[idx[ip]].pfo.format + ')'
        toprint += string(params[idx[ip]], format=format)
     endfor

  endif

  ;; FULL: Print one parameter per line
  if keyword_set(full) then begin

     ;; set up formats that are wide enough for idx and tied.  They
     ;; should be at least as wide as their column headings plus one
     idx_nchar = floor(alog10(max(idx)))+1 > 4
     idx_format = '(i' + strtrim(idx_nchar, 2) + ')'
     tied_format = '(a' + strtrim(max(strlen(parinfo[idx].tied))+2 > 5, 2) + ')'

     ;; We want to prepend column headings and other explanations to our preamble
     if keyword_set(first_funct) then begin
        ;; Assume all our values print to the same width as the first one
        test = string(parinfo[idx[0]].value, format=parinfo[idx[0]].pfo.format)
        val_col_format = 'a' + strtrim(strlen(test), 2)
        test = string(parinfo[idx[0]].value, format=parinfo[idx[0]].pfo.eformat)
        err_col_format = 'a' + strtrim(strlen(test), 2)
        
        idx_col_format = 'a' + strtrim(strlen(idx[0]), 2)
        col_head = string(format='(a' + strtrim(!pfo.pname_width, 2) + ', 3X, ' + $
                          val_col_format + ', " L  ", ' + val_col_format + ', 5X, ' + err_col_format + $
                          ', " L  ", '  + val_col_format + ', a' + strtrim(idx_nchar, 2) + ', ' + tied_format + ')' , $
                          'Param name', 'Left limit', 'Value', 'Error', 'Right limit', 'idx', 'tied')
        if keyword_set(include_mpstep) then $
           col_head += string(format='(2('  + val_col_format + '), a7, ' + val_col_format + ' )' , $
                          'Step', 'Relstep', 'MPside', 'MPmaxstep')
           
        ;; Call the col_head sections of any other print routines in
        ;; the parinfo structure
        pfo_struct_call_procedure, parinfo, 'print', col_head=col_head, idx=idx, _EXTRA=extra
        preamble = 'Expression; (L: . = free, | = fixed, < = limited, <* = pegged)' + !tok.newline + col_head + !tok.newline + preamble

     endif

     ;; Initialize our output variable
     toprint = ''

     ;; Put in our preamble, remembering to add a newline.  Skip this
     ;; (and thus any leading newline) if no_preamble is set
     if NOT keyword_set(no_preamble) then $
        toprint += preamble + !tok.newline

     ;; The code does not put a trailing newline on functions, so
     ;; subsequent functions after the first new a prepending newline
     if NOT keyword_set(first_funct) then $
        toprint += !tok.newline

     ;; Check to see if any of the parinfo structure tags want to
     ;; print anything on the equation line (e.g. pfo_ROI_struct)
     pfo_struct_call_procedure, parinfo, 'print', equation_string=equation_string, idx=idx, _EXTRA=extra

     toprint += equation_string + !tok.newline
     for ip=0, N_elements(idx)-1 do begin
        ;; parname = 
        ;; Build up a formatted 'parname = ' using
        ;; !pfo.pname_width as the runtime format specifier
        pnformat = '(' + 'a' + strtrim(!pfo.pname_width, 2) + $
                   ')'
        param_print = string(format=pnformat, $
                             parinfo[idx[ip]].parname) + ' = '
        
        ;; param and limits format
        if keyword_set(parinfo[idx[ip]].pfo.format) then $
           pformat = '(' + parinfo[idx[ip]].pfo.format + ')'
        ;; Left limit.  I think I want to always print it and
        ;; indicate its use with the delimiters
        param_print += string(parinfo[idx[ip]].limits[!pfo.left], format=pformat)
        
        param_print += pfo_delimiter(parinfo, !pfo.left, params, $
                                     idx[ip], _EXTRA=extra)

        ;; param
        param_print += string(params[idx[ip]], format=pformat)
        
        ;; +/- error
        param_print += ' +/- '
        if keyword_set(parinfo[idx[ip]].pfo.eformat) then $
           eformat = '(' + parinfo[idx[ip]].pfo.eformat + ')'
        param_print += string(parinfo[idx[ip]].error, format=eformat)

        ;; Right delimiter
        param_print += pfo_delimiter(parinfo, !pfo.right, params, $
                                     idx[ip], _EXTRA=extra)
        ;; Right limit
        param_print += string(parinfo[idx[ip]].limits[!pfo.right], format=pformat)

        ;; Parameter numbers
        param_print += string(idx[ip], format=idx_format)
        ;; linked
        param_print += string(parinfo[idx[ip]].tied, format=tied_format)
        
        ;; Additional mpfit stuff, if desired
        if keyword_set(include_mpstep) then begin
           param_print += string(parinfo[idx[ip]].step, format=pformat)
           param_print += string(parinfo[idx[ip]].relstep, format=pformat)
           param_print += string(parinfo[idx[ip]].mpside, format='(B7)')
           param_print += string(parinfo[idx[ip]].mpmaxstep, format=pformat)
        endif ;; Additional mpfit stuff, if desired

        ;; Check to see if any of the parinfo structure tags want to
        ;; print anything on the parameter line (e.g. pfo_ROI_struct)
        pfo_struct_call_procedure, parinfo, 'print', param_print=param_print, idx=idx[ip], _EXTRA=extra
        ;; Next line for next parameter or end of list
        toprint += param_print + !tok.newline
     endfor ;; each parameter
  endif ;; full


  ;; For all layouts, close the paranthesis of the parameter list
  toprint += ')'
  ;; Don't forget to close extra paranthesis from outfunct
  if keyword_set(parinfo[idx[0]].pfo.outfunct) then $
     toprint += ')'

  return, toprint

     
end

