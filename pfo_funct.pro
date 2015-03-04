; +
; $Id: pfo_funct.pro,v 1.3 2004/01/15 17:11:38 jpmorgen Exp jpmorgen $

; pfo_funct.pro 

;; This is the general workhorse of the PFO system.  You can use it to
;; create, print, and calculate PFO functions.  If you specify
;; /CREATE, it basically just calls pfo_fcreate with the surplus
;; arguments supplied to this function using IDL's _EXTRA construct.
;; So if you want to look at the command line options available for
;; function creating, see pfo_fcreate.

;; Given an X axis, return a Y axis calculated according to the model
;; specified in parinfo with the parameters params.  Facility is made
;; for calculating a separate X axis using params and the input X axis
;; (e.g. to get from pixels to wavelength).  This function is
;; basically just a wrapper that parses the pfo structure section of a
;; pfo_parinfo array.  The parinfo information tells pfo_funct what
;; order to call the functions in, what axes to use as input and
;; output, and how to combine the output with any existing values on
;; that axis (e.g. replace, add, multiply, or convolve).  I suggest
;; you read the code starting at ;; CALCULATING to figure out exactly
;; how it works, but its use should be more or less self explanatory.
;; Read the full documentation and see examples for how to use it.

;; NOTE: the implementation of convolution here is not the most
;; convenient to use.  You will have to write a special pfo_ function
;; whose output value is your X-axis independent kernel, which must be
;; shorter than the input axis (might be a problem with mpfit, since
;; it messes with the axis length).  My preference is actually to
;; implement convolution in a pfo_ primitive, which is why I made it
;; possible to pass all the axes from pfo_funct to the primitives.  If
;; you need more information, such as the unadulterated original axes,
;; you can pass them in from the top level with the functargs
;; structure.

;; If PRINT is specified, the function is not calculated, but the
;; parameters are printed in the order in which they are parsed and in
;; the format specified by the value of PRINT

;; If CREATE is specified, it is not necessary to supply any other
;; command line parameters, though you may want to use
;; ftype=!pfo.myfunct and parinfo=myparinfo.  As discussed above,
;; additional parameters are passed to pfo_fcreate.

; -

function pfo_funct, Xin, params, dparams, parinfo=parinfo, idx=idx, $
                    xaxis=xaxis, yaxis=yaxis, ytemplate=ytemplate, $
                    create=create, print=print, $
                    ftype=ftype, convol_center=convol_center, $
                    edge_wrap=edge_wrap, $
                    edge_truncate=edge_truncate, _EXTRA=extra

  init = {tok_sysvar}
  init = {pfo_sysvar}


  ;; We are a wrapper for functions that create, read, and write
  ;; parinfo records.  I debated how to handle ftype.  Decided to
  ;; handle it as a positional parameter rather than a keyword so that
  ;; ftype as a keyword could be passed to primitive functions
  ;; (e.g. sso).

  ;; /CREATE
  if keyword_set(create) then begin
     if N_elements(ftype) eq 0 then $
       ftype=!pfo.null
     return, pfo_fcreate(ftype, parinfo_template=parinfo, _EXTRA=extra)
  endif ;; /CREATE

  ;; Use the common error checking code to get use_idx.
  use_idx = pfo_funct_check(!pfo.null, Xin=Xin, params=params, $
                            parinfo=parinfo, idx=idx, npar=n_use)

  ;; --> Hopfully some day I will fix this.
  if N_params() gt 2 then $
    message, 'ERROR: analytic derivatives are not implemented anywhere in the PFO system yet'

  ;; Check for pathological cases and do some setup work
  if keyword_set(print) then begin
     ;; PRINTING
     ;; don't be verbose if we just want the parameters
     toprint = 'X = Xin; Y = 0' + !tok.newline
     firstprint = 1             ; flag for newline stuff
     if size(print, /type) ne 7 then begin
        if print eq 1 then $
          toprint = ''
     endif

     ;; Make sure at least one parameter wants to be printed.
     if n_use eq 0 then $
       return, toprint

     if total(parinfo[use_idx].mpprint) eq 0 then $
       return, toprint

  endif else begin
     ;; CALCULATING

     yaxis = pfo_null(Xin, params, dparams, parinfo=parinfo)
     if n_use eq 0 then $
       return, yaxis

     ;; Internal xaxis for calculations like dispersion relation.  Start
     ;; from Xin
     xaxis = Xin
     x = xaxis
     y = yaxis

  endelse  ;; printing vs calculating setup

  ;; Now untangle the various levels of fseq, axes, etc.  This is
  ;; common to both printing and calculating.

  ;; Call the functions by sequence.  Sorry for all the nested
  ;; indices.  Since structures are passed by reference anyway, this
  ;; is the fastest way to do things.  The rule is if you do a `where'
  ;; on an array, the resulting indices reference the indices of the
  ;; original array.  If you `where' a subarray, then the resulting
  ;; indices are for THAT subarray.  Hence, you need to nest to get
  ;; back to the original indices.  

  ;; Pick out just the sequence numbers (seqs) that are actually used.
  ;; I think histogram can be used to do this as well, but if the fseq
  ;; numbers are not consecutive, this is probably more efficient
  
  fseqs = parinfo[use_idx].pfo.fseq
  u_fseqs = uniq(fseqs, sort(fseqs))

  for iseq=0, N_elements(u_fseqs)-1 do begin
     ;; Save off the current fseq in a variable for easy use later.
     fseq = fseqs[u_fseqs[iseq]]
     ;; From a list of all sequence numbers, pick out the indices that
     ;; correspond to this particular sequence
     fsidx = where(fseqs eq fseq)

     ;; If order is important, it will have been specified with fseq.
     ;; Now we need to process through by in and out axis.  The index
     ;; nexting is going to get out of hand, so use a temporary
     ;; variable, tidx, to store them.
     tidx = use_idx[fsidx]

     ;; If we are printing, check to see if we can skip this time
     ;; through the loop.  Unfortunately, IDL doesn't opt out of an
     ;; if statement part way through even if it could already know it
     ;; it's result.
     if keyword_set(print) then $
       if total(parinfo[tidx].mpprint) eq 0 then $
       CONTINUE

     ;; Decend to the next level, etc.
     inaxes = parinfo[tidx].pfo.inaxis
     u_inaxes = uniq(inaxes, sort(inaxes))
     for iinax=0, N_elements(u_inaxes)-1 do begin
        inaxis = inaxes[u_inaxes[iinax]]
        inaxidx = where(inaxes eq inaxis)
        tidx = use_idx[fsidx[inaxidx]]
        if keyword_set(print) then $
          if total(parinfo[tidx].mpprint) eq 0 then $
          CONTINUE
        oaxes = parinfo[tidx].pfo.outaxis
        u_oaxis = uniq(oaxes, sort(oaxes))
        for ioax=0, N_elements(u_oaxis)-1 do begin
           oaxis = oaxes[u_oaxis[ioax]]
           oaxidx = where(oaxes eq oaxis)
           tidx = use_idx[fsidx[inaxidx[oaxidx]]]
           if keyword_set(print) then $
             if total(parinfo[tidx].mpprint) eq 0 then $
             CONTINUE
           fops = parinfo[tidx].pfo.fop
           u_fop = uniq(fops, sort(fops))
           for ifop=0, N_elements(u_fop)-1 do begin
              fop = fops[u_fop[ifop]]
              if fop eq !pfo.repl and N_elements(u_fop) gt 1 then $
                message, 'ERROR: more than one transforming/replacing operation in this sequence acting on the same axis combination.'
              ;; Recall that ftype is a real number, with the integer
              ;; determining the function type and the decimal the
              ;; parameter of that function.  To make nomenclature
              ;; clear here, call the general function (e.g. delta,
              ;; gauss), fn

              fopidx = where(fops eq fop)
              tidx = use_idx[fsidx[inaxidx[oaxidx[fopidx]]]]
              if keyword_set(print) then $
                if total(parinfo[tidx].mpprint) eq 0 then $
                CONTINUE
              fns = fix(parinfo[tidx].pfo.ftype)
              u_fns = uniq(fns, sort(fns))
              for ifn=0, N_elements(u_fns)-1 do begin
                 fn = fns[u_fns[ifn]]
                 fnidx = where(fns eq fn)

                 ;; As advertised, the last thing used for unique
                 ;; identification is ID.  If the user is careful to
                 ;; assign a unique ID to each set of parameters that
                 ;; represent a function, the parameter list can be
                 ;; totally randomized and we can put it back together
                 ;; again with `where'
                 tidx = use_idx[fsidx[inaxidx[oaxidx[fopidx[fnidx]]]]]
                 if keyword_set(print) then $
                   if total(parinfo[tidx].mpprint) eq 0 then $
                   CONTINUE
                 IDs = parinfo[tidx].pfo.ID
                 u_IDs = uniq(IDs, sort(IDs))
                 for iID=0, N_elements(u_IDs)-1 do begin
                    ID = IDs[u_IDs[iID]]
                    IDidx = where(IDs eq ID, nIDidx)
                    ;; Get the whole list of ftypes (not just the
                    ;; integer part).
                    tidx = use_idx[fsidx[inaxidx[oaxidx[fopidx[fnidx[IDidx]]]]]]
                    if keyword_set(print) then $
                      if total(parinfo[tidx].mpprint) eq 0 then $
                      CONTINUE


                    ;; If we don't know the number of parameters are
                    ;; function is supposed to have, our work is done.
                    ;; If you really wanted more than one function of
                    ;; a special type, use unique IDs
                    if !pfo.fnpars[fn] eq 0 then begin
                       nfns = 1
                       parray = make_array(nfns, nIDidx, /LONG)
                       parray[0,*] = lindgen(nIDidx)
                    endif else begin
                       ;; Even with all this work, it is still
                       ;; possible that we can have more than one set
                       ;; of parameters for this function type.  If
                       ;; this is the case, the best thing we can do
                       ;; is take the parameters in the order in which
                       ;; we find them to fill in the complete
                       ;; definition of all the functions.
                       nfns = nIDidx / !pfo.fnpars[fn]
                       if nIDidx mod !pfo.fnpars[fn] ne 0 then $
                         message, 'ERROR: incorrect number of parameters for function type ' + !pfo.fnames[fn]
                    
                       ftypes = parinfo[tidx].pfo.ftype
                       u_ftypes = uniq(ftypes, sort(ftypes))
                       nparams = N_elements(u_ftypes)

                       ;; Create an array to store IDidx indices to
                       ;; the parinfo records we are tying to sort
                       ;; into unique, properly defined functions.
                       parray = make_array(nfns, nparams, /LONG, value=-999)
                    
                       for iIDidx=long(0), nIDidx-1 do begin
                          ;; Find out which row (parameter number) of
                          ;; the array we belong in
                          iparam = where(ftypes[u_ftypes] eq ftypes[iIDidx])
                          ;; Find the first column (function number)
                          ;; not already occupied
                          unused_idx = where(parray[*,iparam] eq -999, count)
                          if count eq 0 then $
                            message, 'ERROR: too many parinfo records with ftype = ' + string(ftypes[iIDidx])
                          parray[unused_idx[0], iparam] = iIDidx
                       endfor ;; all parameters with this ID
                    endelse ;; possible mutiple functions

                    ;; Call or print the functions in parray one at a
                    ;; time.
                    for iIDfn=long(0), nfns-1 do begin
                       ;; Assemble parameters for this particular
                       ;; instance of the function
                       fidx = use_idx $
                              [fsidx $
                               [inaxidx $
                                [oaxidx $
                                 [fopidx $
                                  [fnidx $
                                   [parray[iIDfn,*]]]]]]]

                       if keyword_set(print) then begin 
                          ;; PRINTING
                          if total(parinfo[fidx].mpprint) eq 0 then $
                            CONTINUE

                          ;; Append a newline if we are going to add
                          ;; on more functions in the more verbose
                          ;; print options
                          if size(print, /type) ne 7 then begin
                             if print gt 1 then begin
                                if NOT keyword_set(firstprint) then begin
                                   toprint = toprint + !tok.newline
                                endif
                                firstprint = 0
                             endif
                          endif

                          ;; Assemble a nice one-line version of the
                          ;; parameters, complete with function name
                          ;; and axis operated on.
                          case oaxis of
                             0		: oaxs = '' 
                             !pfo.Xin	: message, 'ERROR: cannot write to input axis'
                             !pfo.Xaxis	: oaxs = 'X'
                             !pfo.Yaxis	: oaxs = 'Y'
                             else	: message, 'ERROR: unrecognized outaxis value ' + string(oaxis)
                          endcase
                          oaxs2 = oaxs
                          case fop of 
                             0		: fos = ''
                             !pfo.repl	: begin
                                fos = ' '
                                oaxs2 = ''
                             end
                             !pfo.add	: fos = ' + '
                             !pfo.mult	: fos = ' * '
                             !pfo.convol: fos = ' convolv '
                             else	: message, 'ERROR: unrecognized operation ' + string(fop)
                          endcase
                          case inaxis of
                             0		: inaxs = ''
                             !pfo.Xin	: inaxs = 'Xin'
                             !pfo.Xaxis	: inaxs = 'X'
                             !pfo.Yaxis	: inaxs = 'Y'
                             else	: message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                          endcase
                          toprint = toprint + oaxs + ' = ' + oaxs2 + fos + $
                                    !pfo.fnames[fn] + strtrim(ID, 2) + $
                                    '(' + inaxs + ')('

                          ;; If we are printing one parameter per
                          ;; line, put a newline after the '('
                          if size(print, /type) ne 7 then $
                            if print eq !pfo.pmp then $
                            toprint = toprint + !tok.newline
                          
                       endif else begin
                          ;; CALCULATING                          

                          ;; Make temporary variables x and y, which
                          ;; are the generic input and output of the
                          ;; function
                          case inaxis of
                             0		: ; no change in x
                             !pfo.Xin	: x = Xin
                             !pfo.Xaxis	: x = xaxis
                             !pfo.Yaxis	: x = yaxis
                             else	: message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                          endcase
                          case oaxis of
                             0		: ; no change in y
                             !pfo.Xin	: message, 'ERROR: cannot write to input axis'
                             !pfo.Xaxis	: y = xaxis
                             !pfo.Yaxis	: y = yaxis
                             else	: message, 'ERROR: unrecognized outaxis value ' + string(oaxis)
                          endcase
                       endelse  ;; calculating vs printing

                       ;; create a command that calls the pfo
                       ;; primitive
                       ;;
                       fname = 'pfo_' + !pfo.fnames[fn] 
                       ;; call function is apparently faster than
                       ;; execute and is appropriate if you don't have
                       ;; to construct arguments.  The CATCH in mpfit
                       ;; also works better this way.  In case
                       ;; functions need to use more than just the
                       ;; input axis (e.g. convlution by an X
                       ;; dependent instrument profile), pass all
                       ;; axes.  Make sure your pfo_ function has
                       ;; _EXTRA, so these can be safely ignored.
                       fy = call_function(fname, x, params, $
                                          parinfo=parinfo, idx=fidx, $
                                          print=print, $
                                          Xorig=Xin, xaxis=xaxis, yaxis=yaxis, $
                                          _EXTRA=extra)

                       if keyword_set(print) then begin
                          ;; PRINTING
                          toprint = toprint + fy + ')'
                       endif else begin
                          ;; CALCULATING
                          case fop of 
                             0		: ; no output
                             !pfo.repl	: y = fy
                             !pfo.add	: y = y + fy
                             !pfo.mult	: y = y * fy 
                             !pfo.convol: y $
                               = convol(y, fy, center=convol_center, $
                                        edge_wrap=edge_wrap, $
                                        edge_truncate=edge_truncate)
                             else	: message, 'ERROR: unrecognized operation ' + string(fop)
                          endcase
                          
                          ;; Put y into the desired output axis
                          case oaxis of
                             0		: ; no change in y
                             !pfo.Xin	: message, 'ERROR: cannot write to input axis'
                             !pfo.Xaxis	: xaxis = y
                             !pfo.Yaxis	: yaxis = y
                             else	: message, 'ERROR: unrecognized inaxis value ' + string(inaxis)
                          endcase
                       endelse ;; printing vs calculating

                    endfor ;; each function in parray (iIDfn)
                 endfor ;; iID
              endfor ;; ifn
           endfor ;; ifop
        endfor ;; ioax
     endfor ;; iinax
  endfor ;; iseq

  ;; Return value depends on whether we are printing or calculating
  if keyword_set(print) then begin
     return, toprint
  endif else begin
     return, yaxis
  endelse

end
