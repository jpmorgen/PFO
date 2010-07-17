;+
; NAME: pfo_widget
;
; PURPOSE: Create and manage basic PFO package widget stuff.
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
; $Id: pfo_widget.pro,v 1.1 2010/07/17 18:56:48 jpmorgen Exp $
;-
pro pfo_widget_cleanup, tlb
  ;; Get our state variable from the tlb uvalue
  widget_control, tlb, get_uvalue=state, /no_copy
  ;; Make sure we still have a state variable
  if N_elements(state) eq 0 then $
    return
  ;; Clear out our widget IDs
  (*state.pparinfo)[state.idx].pfo_widget.IDs = 0
end

pro pfo_widget_destroy, pparinfo, idx=idx
  ;; Get our tlb from the pparinfo
  tlbID = pfo_widget(pparinfo, idx=idx)
  ;; If there is none, our job is done
  if tlbID eq 0 then $
    return
  ;; Erase the widgetIDs from our pparinfo
  (*pparinfo)[idx].pfo_widget.IDs = 0

  ;; Send the windowing system the signal to destroy the widget.  This
  ;; causes a chain of events that eventually ends up calling
  ;; pfo_widget_cleanup
  widget_control, tlbID, /destroy
end

;; Return array coordinates of ID.  If ID is not specified, return
;; array coordinates of all non-zero IDs
function pfo_widget_idx, pparinfo, ID, idx=idx, n_widgets=n_widgets
  if NOT keyword_set(idx) then $
    idx = lindgen(N_elements(*pparinfo))
  if n_params() eq 1 then begin
     grouped_widget_idx = $
       where((*pparinfo)[idx].pfo_widget.IDs ne 0, n_widgets)
  endif else begin
     grouped_widget_idx = $
       where((*pparinfo)[idx].pfo_widget.IDs eq ID, n_widgets)
  endelse
  if n_widgets eq 0 then $
    return, -1

  ;; Work with the indices that are split between the parinfo
  ;; and pfo_widget.IDs.  widget_idx is the index into the
  ;; pfo_widget.IDs array
  widget_idx = grouped_widget_idx MOD !pfo_widget.num_widgets
  ;; parinfo_idx is the index into parinfo
  parinfo_idx = grouped_widget_idx / !pfo_widget.num_widgets
  ;; unwrap so that we return the oritinal index into parinfo
  parinfo_idx = idx[parinfo_idx]
  return, [parinfo_idx, widget_idx]
end

;; Event handler for the tlb
pro pfo_widget_event, event
  widget_control, event.top, get_uvalue=state, /no_copy
  event_name = tag_names(event, /structure_name)
  ;; Process button events
  if event_name eq 'WIDGET_BUTTON' then begin
     widget_control, event.ID, get_uvalue=button
     case button of
        'done' : begin
           pfo_widget_destroy, state.pparinfo
           ;; Return now so we don't get an error trying to
           ;; access a dead widget
           return
        end
        else : a=dialog_message('Feature not implemented yet', /information)
     endcase
  endif
  widget_control, event.top, set_uvalue=state, /no_copy

end

;; Redraw/recreate events of values in our table
function pfo_widget_redraw_event, event
  retval = 0L
  sname = tag_names(event, /structure_name)
  if sname ne 'REDRAW_EVENT' then $
    message, 'ERROR: I do not expect to process this event.  Event bubble up structure not handled prorperly or sub-widget defined without an event handler.'
  tags = tag_names(event)
  ;; Check to see if we were asked to redraw
  junk = where(tags eq 'REDRAW', count)
  if count ne 0 then begin
     ;; Get our top level state variable so we have access to pparinfo
     widget_control, event.top, get_uvalue=state, /no_copy

     ;; Use pfo_funct to redraw everything in the parinfo container
     junk = pfo_funct(parinfo=state.pparinfo, /widget)


     ;; Check to see if we have any event_pro or event_funct
     tags = tag_names(state)
     junk = where(tags eq 'EVENT_PRO', count)
     if count eq 1 then begin
        event_pro = state.event_pro
        ;; Put our state variable back so the routine(s) we call can
        ;; have access to it.
        widget_control, event.top, set_uvalue=state, /no_copy
        call_procedure, event_pro, event
     endif
     junk = where(tags eq 'EVENT_FUNC', count)
     if count eq 1 then begin
        event_func = state.event_func
        ;; Put our state variable back so the routine(s) we call can
        ;; have access to it.
        widget_control, event.top, set_uvalue=state, /no_copy
        retval = call_function(event_func, event)
     endif
     ;; Put out state variable back if we haven't already done so
     if keyword_set(state) then $
       widget_control, event.top, set_uvalue=state, /no_copy

  endif
  ;; --> put recreate code here
  return, retval
end

;; Event handlers for the widgets in the parinfo_containerID table.
;; There is one event handler per widget type.

function pfo_widget_value_event, event

  ;; Don't bother doing anything until the user leaves the widget or
  ;; presses return.  This depends on the widget_text(/editable,
  ;; /all_events, /kbrd_focus_events)
  case tag_names(event, /structure_name) of
     'WIDGET_KBRD_FOCUS': begin
        if event.enter ne 0 then $
          return, 0L
     end
     ;; 10 is carriage return, I guess.
     'WIDGET_TEXT_CH': if event.ch ne 10 then $
       return, 0L
     else : return, 0L
  endcase

  ;; If we made it here, the user is done playing with the field

  ;; Get our little widget "state variable," wstate and check to see
  ;; if the tlb in wstate is the same as the real tlb
  widget_control, event.ID, get_uvalue=wstate
  if event.top ne wstate.tlbID then $
    message, 'ERROR: this event exists in a widget hierarchy not recorded by the subwidget tlbID "state variable" value.  Are you confusing me with multiple instances of pfo_widget?'


  ;; Check to see if wstate was really generated properly (e.g. has a
  ;; SIDE tag)
  wtags = tag_names(wstate)
  junk = where(wtags eq 'SIDE', count)
  if count eq 0 then $
    message, 'ERROR: improperly formatted uvalue for a value event.  Expecting a structure with a "side" tag'

  ;; Get our top level state variable so we have access to pparinfo
  widget_control, event.top, get_uvalue=state, /no_copy
  
  ;; Find our ID in the pfo_widgetID array
  widget_idx = pfo_widget_idx(state.pparinfo, event.ID, n_widgets=n_widgets)
  if n_widgets ne 1 then $
    message, 'ERROR: this widgetID is not stored properly in state.pparinfo?  Are you confusing me with multiple instances of pfo_widget?'

  ;; Read our value from the widget
  widget_control, event.ID, get_value=value
  case wstate.side of
     !pfo_widget.left : $
       (*state.pparinfo)[widget_idx[0]].limits[!pfo.left] = value
     !pfo_widget.value : $
       (*state.pparinfo)[widget_idx[0]].value = value
     !pfo_widget.right : $
       (*state.pparinfo)[widget_idx[0]].limits[!pfo.right] = value
  endcase

  ;; Put our state variable back
  widget_control, event.top, set_uvalue=state, /no_copy

  ;; "Bubble up" to the next event handler, indicating that we need a
  ;; redraw of this field (to pick up formatting).
  return_event = {redraw_event, $
                  ID: event.ID, top:event.top, $
                  handler:0L, redraw:1}

  return, return_event

end

function pfo_widget_delimiter_event, event
  ;; Get our little widget "state variable," wstate and check to see
  ;; if the tlb in wstate is the same as the real tlb
  widget_control, event.ID, get_uvalue=wstate
  if event.top ne wstate.tlbID then $
    message, 'ERROR: this event exists in a widget hierarchy not recorded by the subwidget tlbID "state variable" value.  Are you confusing me with multiple instances of pfo_widget?'

  ;; Check to see if wstate was really generated properly (e.g. has a
  ;; SIDE tag)
  wtags = tag_names(wstate)
  junk = where(wtags eq 'SIDE', count)
  if count eq 0 then $
    message, 'ERROR: improperly formatted uvalue for a value event.  Expecting a structure with a "side" tag'

  ;; Get our top level state variable so we have access to pparinfo
  widget_control, event.top, get_uvalue=state, /no_copy
  
  ;; Find our ID in the pfo_widgetID array
  widget_idx = pfo_widget_idx(state.pparinfo, event.ID, n_widgets=n_widgets)
  if n_widgets ne 1 then $
    message, 'ERROR: this widgetID is not stored properly in state.pparinfo?  Are you confusing me with multiple instances of pfo_widget?'

  ;; Read our value from the widget
  widget_control, event.ID, get_value=value
  ;; Droplist events return index of selected item.
  case value[event.index] of
     !pfo_widget.delimiters[!pfo_widget.free]: begin
        (*state.pparinfo)[widget_idx[0]].fixed   = !tok.no
        (*state.pparinfo)[widget_idx[0]].limited = !tok.no
     end
     !pfo_widget.delimiters[!pfo_widget.fixed]: begin
        (*state.pparinfo)[widget_idx[0]].fixed   = !tok.yes
        ;; Don't mess with limited in this case
     end
     !pfo_widget.delimiters[!pfo_widget.limited]: begin
        (*state.pparinfo)[widget_idx[0]].fixed   = !tok.no
        (*state.pparinfo)[widget_idx[0]].limited[wstate.side] = !tok.yes
     end
     else : a=dialog_message('Feature not implemented yet', /information)
  endcase

  ;; Put our state variable back
  widget_control, event.top, set_uvalue=state, /no_copy

  ;; "Bubble up" to the next event handler, indicating that we need a
  ;; redraw, since fiddling with one side effects the other
  return_event = {redraw_event, $
                  ID: event.ID, top:event.top, $
                  handler:0L, redraw:1}

  return, return_event


end

;;*************************************************************
function pfo_widget, pparinfo, idx=idx, group_leader=group_leader, $
                     parinfo_containerID=parinfo_containerID, $
                     event_pro=event_pro, event_funct=event_funct
  init = {pfo_widget_sysvar}
  init = {tok_sysvar}
  if size(pparinfo, /type) ne !tok.pointer then $
    message, 'ERROR: you need to create a heap variable for parinfo and pass me a pointer to that memory location in order for me to work properly'

  ;; When we are up and running, pfo_widget returns the tlbID of the
  ;; current widget.  Otherwise, pfo_widget builds the widget in two
  ;; stages: starts it and initilizes the scroll window into which all
  ;; the parinfo widgets are put (parinfo_containerID), and then
  ;; finishes the job by realizing the widget and calling XMANAGER.

  ;; parinfo_containerID is set when we are in the middle of building
  ;; our widget.
  if NOT keyword_set(parinfo_containerID) then begin
     ;; If we made it here, we are either starting from scratch or
     ;; refilling an active pfo_widget.  Check to see if there are any
     ;; non-zero widget IDs in pparinfo
     widget_idx = pfo_widget_idx(pparinfo, idx=idx, n_widgets=n_widgets)
     if n_widgets gt 0 then begin
        ;; This is our "up and running" case.  Pick the first
        ;; pfo_widget.IDs to find the tlbID.  Indexing widget_idx like
        ;; this guarantees covering all shapes and sizes of its return
        ;; value.
        ID = (*pparinfo)[widget_idx[0]].pfo_widget.IDs[widget_idx[1]]
        widget_control, ID, get_uvalue=wstate
        return, wstate.tlbID
     endif

     ;; If we made it here, we need to create our tlb widget from
     ;; scratch.  This is a three-step process.  Here we create the
     ;; base widget.  pfo_funct and its primitives fill the widget
     ;; with their stuff.  Then we need to cycle back, realize the
     ;; widget and call xmanager (see below).  I use the default
     ;; event_pro, which is the name of this file with _event
     ;; appended.
     tlbID = widget_base(column=1, $
                         title='PFO_WIDGET', $
                         group_leader=group_leader)

     ;; Define a couple of buttons on the top of the widget
     doneID = widget_button(tlbID, value='Done', $
                            tooltip='Dismiss widget', $
                            uvalue='done')
     eraseID = widget_button(tlbID, value='Erase all', $
                             tooltip='Erase all parinfo entries', $
                             uvalue='erase')

     ;; Put in some column headers
     rowID = widget_base(tlbID, row=1)
     junkID = widget_label(rowID, value='Function def/                          bstatus:  . = free, | = fixed, < = bound, * = pegged, T = tied')
     ;; --> The value of the column header is going to need to change when we add things
     rowID = widget_base(tlbID, row=1)
     junkID = widget_label(rowID, value='Parname                left bound      bstatus        VALUE              ERROR       bstatus    right bound')

     ;; Make a base widget for the parinfo table that has scroll bars.
     ;; We make this an output value that is used in pfo_funct and
     ;; primitives so that they can put their widgets inside of it.
     ;; Make a specific event handler here in order to handle redraw
     ;; and recreate events bubbled up from the event handlers of the
     ;; sub-widgets,
     parinfo_containerID = $
       widget_base(tlbID, /scroll, x_scroll_size=!pfo_widget.x_scroll, $
                   y_scroll_size=!pfo_widget.y_scroll, column=1, $
                   uvalue={tlbID: tlbID}, event_func='pfo_widget_redraw_event')

     ;; Prepare our "state" variable to carry around in the tlb
     state = {tlbID        : tlbID, $
              pparinfo     : pparinfo, $
              $ ;; idx into pparinfo
              idx          : idx $ 
             }
     ;; Append to the state variable keyowrds that we want to carry
     ;; around
     if keyword_set(group_leader) then $
       state = struct_append(state, {group_leader : group_leader})
     if keyword_set(event_pro) and keyword_set(event_func) then $
       message, 'ERROR: both event_pro and event_func cannot be set'
     if keyword_set(event_pro) then $
       state = struct_append(state, {event_pro : event_pro})
     if keyword_set(event_func) then $
       state = struct_append(state, {event_func : event_func})
     widget_control, tlbID, set_uvalue=state, /no_copy

     ;; Initialize our per-parameter pointer.  Remember to
     ;; re-initilize this each time you start over with a parameter
     !pfo_widget.ID_idx = 0
     return, tlbID

  endif

  ;; If we made it here, we are likely being called at the end of
  ;; pfo_funct for the third part of our widget creation excercise,
  ;; sketched out above. when we need to wrap up our widget creation
  ;; stuff.

  ;; Grab our tlb from the pparinfo, since that is a handy place to
  ;; store it.  Yes, this is a recursive call.
  tlbID = pfo_widget(pparinfo, idx=idx)
  if tlbID eq 0 then $
    message, 'ERROR: no IDs were stored in the parinfo'
  widget_control, tlbID, /realize
  ;; Now that we have realized our tlb, we can no longer put any
  ;; widgets into it.
  parinfo_containerID = 0
  xmanager, 'PFO_WIDGET', tlbID, group_leader=group_leader, $
            cleanup='pfo_widget_cleanup'

  ;; Zero our per-parameter pointer to make it clear we won't be
  ;; adding anything
  !pfo_widget.ID_idx = 0

  ;; Return our tlbID, since that is what we do.
  return, tlbID

end
