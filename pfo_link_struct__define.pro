;+
; NAME: pfo_link_struct__define
;
; PURPOSE: Define the parinfo.pfo_link substructure that holds
; information that helps link parameters together in the MPFIT system
;
; CATEGORY: PFO, optional add-in
;
; CALLING SEQUENCE: Used in the pfo_struct_new system.  See EXAMPLES
;
; DESCRIPTION: MPFIT links parameters together by using the indices of
; the parameters in the parinfo array.  Hard-coding index values
; violates the "slice and splice" methods of PFO.  PFO_LINK helps
; bridge the gap.  The pfo_link_struct contains tags that help
; identify "master" parameters to which "slave" parameters are tied
; (sorry for the imagery).
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
; Unlike tied in MPFIT, no fancy arbitrary expressions are allowed.  If
; you need that, make a PFO function for it ;-).  
;
; IDLVM without a license won't work with linking: The MPFIT
; tied code uses the EXECUTE statement
;
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: pfo_link_struct__define.pro,v 1.8 2012/01/13 20:52:29 jpmorgen Exp $
;
; $Log: pfo_link_struct__define.pro,v $
; Revision 1.8  2012/01/13 20:52:29  jpmorgen
; Added widget
;
; Revision 1.7  2011/12/01 22:08:31  jpmorgen
; Minor change in documentation
;
; Revision 1.6  2011/11/21 15:25:01  jpmorgen
; Get intralink working, add hand_tied, calculate effect of precision on
; ftype rounding, improve documentation
;
; Revision 1.5  2011/11/18 15:49:58  jpmorgen
; Got working with basic pfo_scint function, still some work to do
;
; Revision 1.4  2011/09/22 23:47:05  jpmorgen
; Fix bug of no pfo_obj
;
; Revision 1.3  2011/09/16 13:46:02  jpmorgen
; Improved substantially so that this works with an __update method,
; etc. in the pfo_obj system
;
; Revision 1.2  2011/01/03 21:48:43  jpmorgen
; Fixed bug in pfo_funct
;
;-

;; Helper function to read link info from parinfo
function pfo_link_struct_cw_obj::get_link_info, $
   column, $ ;; column for which we want to get information
   sensitive ;; (return) whether or not widget should be sensitive

  ;; Read tags from the parinfo.  *self.pidx should have just one idx
  self.pfo_obj->parinfo_call_procedure, $
     /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
     taglist_series=['pfo_link'], $
     linkID       = linkID    , $
     link_status  = link_status, $
     to_ID        = to_ID , $
     to_ftype     = to_ftype, $
     auto_link	  = auto_link

  ;; pfo_link__update ignores any parameters that are marked as
  ;; hand_tied, so default to marking everything as insensitive if we
  ;; are hand_tied
  sensitive = link_status ne !pfo.hand_tied

  case column of
     self.auto  : begin
        ;; In order to avoid confusion, return '-' if not sensitive,
        ;; even we we are set to something else
        if sensitive eq 0 then $
           return, '-'
        return, !pfo.auto_link_string[auto_link]
     end
     self.status: begin
        ;; Start out by checking our auto status.  If we are set to
        ;; any form of auto, make status insensitive, unless....
        if auto_link ne !pfo.not_used then $
           sensitive = 0
        ;; ... we are hand tied.  In this case, always be sensitive,
        ;; since hand_tied turns off the auto_link droplist
        if link_status eq !pfo.hand_tied then $
           sensitive = 1
        ;; Even if we are auto, display our master/slave status, since
        ;; that helps the user figure out which parameters are in
        ;; charge.  link_status is a bitmask.  Convert that into an
        ;; array index
        if link_status gt 0 then $
           link_status = alog(link_status)/alog(2) + 1
        return, !pfo.link_status_string[link_status]
     end
     self.link: begin
        ;; In order to avoid confusion, return '-' if not sensitive
        if sensitive eq 0 then $
           return, '-'
        ;; --> For now, let the user change the linkID even if we are in
        ;; auto mode
        return, string(linkID)
     end
     self.toID  : begin
        ;; In order to avoid confusion, return '-' if not sensitive
        if sensitive eq 0 then $
           return, '-'
        ;; If we are using the auto_link system, it manages to_ID
        sensitive = auto_link ne !pfo.not_used
        return, string(to_ID)
     end
     self.to_par: begin
        ;; In order to avoid confusion, return '-' if not sensitive
        if sensitive eq 0 then $
           return, '-'
        ;; Always allow user to modify to_par (don't change sensitive)

        ;; Clear out our old to_ftype_list, making it an undefined variable
        ptr_free, self.pto_ftype_list
        self.pto_ftype_list = ptr_new(/allocate_heap)

        ;; Check to see if our parameter is being handled by the link
        ;; system.  If not, return, '-'
        if link_status eq !pfo.not_used or $
           link_status eq !pfo.hand_tied or $
           to_ID eq !tok.nowhere then $
              return, '-'

        ;; If we made it here, we have a parameter we are trying to
        ;; point to

        ;; Construct the full list of parameter names, with the
        ;; current one at the top.  Code similar to that found in
        ;; __print and __update methods

        ;;  Get our entire list of linkIDs
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, $
           taglist_series=['pfo_link'], $
           linkID       = all_linkIDs

        ID_idx = where(all_linkIDs eq to_ID, count)
        if count eq 0 then begin
           message, 'WARNING: pfo_link.linkID ' + strtrim(to_ID, 2) + ' not found.', /CONTINUE
           to_parlist = self.invalid_to_ID
        endif else begin
           ;;  Get the ftype and parname of the to_ID function(s).
           ;;  Limit this to ID_idx to save memory.  The ftype of our
           ;;  to_ID is going to become *self.pto_ftype_list
           self.pfo_obj->parinfo_call_procedure, $
              /no_update, 'pfo_struct_setget_tag', /get, idx=ID_idx, $
              taglist_series=['mpfit_parinfo', 'pfo'], $
              ftype=*self.pto_ftype_list, $
              parname=parname
           ;; Make sure we have just one parameter that we are trying
           ;; to point to.  Keep in mind ftype is a subset of ID_idx
           to_ftype_idx = where(pfo_frac(*self.pto_ftype_list, /round) eq $
                                pfo_frac(to_ftype, /round), count)
           case count of 
              0 : begin
                 ;; We didn't find a valid ftype, so prompt the
                 ;; user to find one from the list
                 to_parlist = 'Select'
                 ;; Append the rest of the parnames to the droplist
                 pfo_array_append, to_parlist, parname
              end
              1 : begin
                 ;; We found a parameter.  Don't unwrap, since we
                 ;; index into parname.  This sets the top element of
                 ;; to_parlist to our current parname
                 to_parlist = parname[to_ftype_idx]
                 ;; Append the rest of the parnames to the droplist
                 pfo_array_append, to_parlist, parname
              end
              else: begin
                 message, 'WARNING: found ' + strtrim(count, 2) + ' parameters with fractional ftype = pfo_link.to_ftype = ' + strtrim(to_ftype, 2) + ' Non-unique linkID?', /CONTINUE
                 to_parlist = self.invalid_linkID
                 ptr_free, self.pto_ftype_list
                 self.pto_ftype_list = ptr_new(/allocate_heap)
              end
           endcase
        endelse    ;; invalid/valid linkID
        return, to_parlist
     end ;; to_par column

  endcase ;; column

end

function pfo_link_struct_cw_obj::event, event, type=type
  ;; We will always swallow the event
  retval = !tok.nowhere

  sn = tag_names(event, /structure_name)

  case type of
     self.auto  : begin
        if sn ne 'WIDGET_DROPLIST' then $
           message, 'ERROR: unexpected event'
        ;; Our current value is always listed at the top.  If
        ;; we haven't selected something else, our job is done.
        if event.index eq 0 then $
           return, retval
        ;; Our event index is shifted up by 1 relative to the string
        ;; array of valid values because our current value is listed
        ;; first and repeated in the list
        if event.index gt N_elements(!pfo.auto_link_string) then $
           message, 'ERROR: unexpected event index: ' + strtrim(event.index)
        self.pfo_obj->parinfo_call_procedure, $
           'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series=['pfo_link'], $
           auto_link=event.index-1
     end
     self.status: begin
        if sn ne 'WIDGET_DROPLIST' then $
           message, 'ERROR: unexpected event'
        if event.index eq 0 then $
           return, retval
        if event.index gt N_elements(!pfo.link_status_string) then $
           message, 'ERROR: unexpected event index: ' + strtrim(event.index)
        ;; Our link_status is a bitmask.  Shift event.index by 2 to
        ;; get 2^ to line up properly.  This depends on 2^-1
        ;; evaluating to 0 = !pfo.not_used.
        self.pfo_obj->parinfo_call_procedure, $
           'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series=['pfo_link'], $
           link_status=2^(event.index-2)
     end
     self.link: begin
        ;; linkID.  This can range from -1 to any positive value.  It
        ;; needs to be the same for all parameters in the function

        ;; Check to see if we have changed our value
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
           taglist_series='pfo_link', linkID=linkID
        ;; Get our current value from the widget
        widget_control, self.linkID, get_value=w_value
        ;; No change means swallow the event
        if linkID eq w_value then $
           return, retval
        if w_value lt !tok.nowhere then begin
           ok = dialog_message('ERROR: linkID must be at least ' + strtrim(!tok.nowhere,2))
           ;; Put previous value back
           self->refresh
           return, retval
        endif ;; invalid entry

        ;; If we made it here, we have a valid change to linkID
        
        ;; Get all of the idx for this function so we can keep the
        ;; linkIDs synchronized
        idx = self.pfo_obj->indices(expand_idx=*self.pidx)

        self.pfo_obj->parinfo_call_procedure, $
           'pfo_struct_setget_tag', /set, idx=idx, $
           taglist_series=['pfo_link'], $
           linkID=w_value
     end
     self.toID  : begin
        ;; toID.  This can range from -1 to any positive value
        ;; Check to see if we have changed our value
        self.pfo_obj->parinfo_call_procedure, $
           /no_update, 'pfo_struct_setget_tag', /get, idx=*self.pidx, $
           taglist_series='pfo_link', to_ID=to_ID
        ;; Get our current value from the widget
        widget_control, self.toID_ID, get_value=w_value
        ;; No change means swallow the event
        if to_ID eq w_value then $
           return, retval
        if w_value lt !tok.nowhere then begin
           ok = dialog_message('ERROR: toID must be at least ' + strtrim(!tok.nowhere,2))
           ;; Put previous value back
           self->refresh
           return, retval
        endif ;; invalid entry

        ;; If we made it here, we have a valid change to toID

        self.pfo_obj->parinfo_call_procedure, $
           'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series=['pfo_link'], $
           toID=w_value
     end
     self.to_par: begin
        if sn ne 'WIDGET_DROPLIST' then $
           message, 'ERROR: unexpected event'
        if event.index eq 0 then $
           return, retval

        ;; If we made it here, we have a valid change to to_ftype

        self.pfo_obj->parinfo_call_procedure, $
           'pfo_struct_setget_tag', /set, idx=*self.pidx, $
           taglist_series=['pfo_link'], $
           to_ftype=(*self.pto_ftype_list)[event.index-1]
     end
     else : message, 'ERROR: unknown event type: ' + strtrim(type, 2)
  endcase

  return, retval
end

pro pfo_link_struct_cw_obj::refresh

  ;; Refresh our widgets.  Also called by populate
  widget_control, self.auto_linkID, $
                  set_value=[self->get_link_info(self.auto, sensitive), $
                             !pfo.auto_link_string], $
                  sensitive=sensitive
  widget_control, self.statusID, $
                  set_value=[self->get_link_info(self.status, sensitive), $
                             !pfo.link_status_string], $
                  sensitive=sensitive
  widget_control, self.linkID, $
                  set_value=self->get_link_info(self.link, sensitive), $
                  sensitive=sensitive
  widget_control, self.toID_ID, $
                  set_value=self->get_link_info(self.toID, sensitive), $
                  sensitive=sensitive
  widget_control, self.to_parID, $
                  set_value=self->get_link_info(self.to_par, sensitive), $
                  sensitive=sensitive

end

pro pfo_link_struct_cw_obj::populate, $
   _REF_EXTRA=extra ;; for now, swallow any extra keywords

  ;; Set up the basic widgets with generic values...
  self.auto_linkID = $
     widget_droplist(self.tlbID, value='-', $
                     xsize=0.75, units=!tok.inches, $
                     uvalue={method: 'event', obj:self, keywords:{type:self.auto}})
  self.statusID = $
     widget_droplist(self.tlbID, value='-', $
                     xsize=1, units=!tok.inches, $
                     uvalue={method: 'event', obj:self, keywords:{type:self.status}})

  ID = widget_base(self.tlbID, xsize=0.75, units=!tok.inches)
  self.linkID = $
     pfo_cw_field(ID, title='', xsize=5, $
                  value=self->get_link_info(self.link), /integer, $
                  /return_events, $
                  /kbrd_focus_events, $
                  uvalue={method:'event', obj:self, keywords:{type:self.link}})
  ID = widget_base(self.tlbID, xsize=0.75, units=!tok.inches)
  self.toID_ID = $
     pfo_cw_field(ID, title='', xsize=5, $
                  value=0, /integer, $
                  /return_events, $
                  /kbrd_focus_events, $
                  uvalue={method:'event', obj:self, keywords:{type:self.toID}})

  self.to_parID = $
     widget_droplist(self.tlbID, value=['-', ' '], $
                     xsize=1.25, units=!tok.inches, $
                     uvalue={method: 'event', obj:self, keywords:{type:self.to_par}})
  ;; ... use refresh method to put value in
  self->refresh

end

;; Cleanup method
pro pfo_link_struct_cw_obj::cleanup
  ;; Clean up our local heap variables
  ptr_free, self.pto_ftype_list
  ;; Call our inherited cleaup routines
  self->pfo_parinfo_cw_obj::cleanup
end

;; Init method for the compound widget (cw)
function pfo_link_struct_cw_obj::init, $
   parentID, $ ;; widgetID of parent widget
   _REF_EXTRA=extra ;; All other input parameters are passed to underlying routines via _REF_EXTRA

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

  ;; Call our inherited init routines.  This puts pfo_obj into self,
  ;; among other things.  Make the tlb a row base, so everything lines
  ;; up on the parameter line properly
  ok = self->pfo_parinfo_cw_obj::init(parentID, /row, _EXTRA=extra)
  if NOT ok then return, 0

  ;; Initialize our pointers
  self.pto_ftype_list = ptr_new(/allocate_heap)

  ;; Define tokens for faster CASE execution
  self.auto	= 0
  self.status	= 1
  self.link	= 2
  self.toID	= 3
  self.to_par	= 4

  ;; Define error messages
  invalid_to_ID  = 'Chose valid to_ID'
  invalid_linkID = 'Check linkIDs or ftypes'

  ;; Build our widget
  self->populate, _EXTRA=extra

  ;; If build is sucessful, we can register ourselves in the refresh
  ;; list
  self->register_refresh

  ;; If we made it here, we have successfully set up our widget.
  return, 1

end

;; Object class definition for the compound widget (cw)
pro pfo_link_struct_cw_obj__define
  objectClass = $
     {pfo_link_struct_cw_obj, $
      $ ;; widget IDs
      auto_linkID	: 0L, $
      statusID		: 0L, $
      linkID		: 0L, $
      toID_ID		: 0L, $
      to_parID		: 0L, $
      pto_ftype_list	: ptr_new() , $ ;; list ftypes in to_ID function
      $ ;; error messages to display in droplist widgets
      invalid_to_ID	: '', $
      invalid_linkID	: '', $
      $ ;; tags for columns
      auto		: 0B, $
      status		: 0B, $
      link		: 0B, $
      toID		: 0B, $
      to_par		: 0B, $
      inherits pfo_parinfo_cw_obj}
end

;; Widget method.  Display our fields in editable format
function pfo_link_struct__widget, $
   parinfo, $ ;; entire parinfo array passed by reference (usually from pfo_obj)
   parentID, $ ;; The rowID of our equation line
   col_head=col_head, $ ;; flag to trigger column head output
   parameter=parameter, $ ;; flag to trigger per parameter widgets
   _REF_EXTRA=extra

  ;; Initialize output
  cwID = !tok.nowhere

  ;; Check to see if caller wants the column headings.  Assume
  ;; parentID is a rowID in pfo_null__widget, or equivalent
  if  keyword_set(col_head) then begin
     ID = widget_label(parentID, value='auto_link', xsize=0.75, units=!tok.inches)
     ID = widget_label(parentID, value='link_status', xsize=1, units=!tok.inches)
     ID = widget_label(parentID, value='link_ID', xsize=0.75, units=!tok.inches)
     ID = widget_label(parentID, value='to_ID', xsize=0.75, units=!tok.inches)
     ID = widget_label(parentID, value='to_par', xsize=1.25, units=!tok.inches)
     return, parentID
  endif ;; column headings

  ;; If we are not on the equation line, we have nothing to do
  if NOT keyword_set(parameter) then $
     return, cwID

  ;; Create our controlling object
  cw_obj = obj_new('pfo_link_struct_cw_obj', parentID, _EXTRA=extra)

  ;; The init method creates the widget and stores its ID in self.tlb.
  ;; Use the getID method to access it.  We return this ID, since that
  ;; is what people expect when they call a widget creation function.
  ;; What people will probably really want is the object to do the
  ;; heavy-duty control.  Default to a nonsense widgetID unless the
  ;; object creation was sucessful.
  if obj_valid(cw_obj) then begin
     cwID = cw_obj->tlbID()
  endif ;; valid cw_obj

end


;; Print method
pro pfo_link_struct__print, $
   parinfo, $
   idx=idx, $
   $;;equation_string=equation_string
   col_head=col_head, $
   param_print=param_print

  ;; Save our col_head as a local variable so we can use it to
  ;; calculate padding in param_print case.  We will want to compute
  ;; our parname format (see pfo_null__print or code referenced
  ;; therein)
  pnformat = '(' + 'a' + strtrim(!pfo.pname_width, 2) + $
             ')'
  local_col_head = ' auto_link link_status linkID to_ID ' + string(format=pnformat, 'to_par')

  ;; Print header
  if keyword_set(col_head) then begin
     col_head += local_col_head
     return
  endif

  ;; Print information for each parameter
  if keyword_set(param_print) then begin
     ;; Save our param_print as a local variable so we can use it to
     ;; calculate padding in param_print case
     local_param_print = ''
     ;;  auto_link
     case parinfo[idx].pfo_link.auto_link of
        !pfo.not_used:  local_param_print += '          '
        !pfo.intralink: local_param_print += '     intra'
        !pfo.interlink: local_param_print += '     inter'
        else: message, 'ERROR: unrecognized pfo_link.auto_link value: ' + strtrim(parinfo[idx].pfo_link.auto_link, 2)
     endcase
     ;; link_status
     case parinfo[idx].pfo_link.link_status of
        !pfo.not_used:  begin
           ;; If the link_status is not used, our work is done.  Pad
           ;; the rest of the param_print, so other param_prints line
           ;; up.
           num_spaces = strlen(local_col_head) - strlen(local_param_print)
           param_print += local_param_print + string(format='(' + string(num_spaces) + '(a1))', " ") 
           return
        end
        !pfo.master:    local_param_print += '     master'
        !pfo.slave:     local_param_print += '      slave'
        !pfo.hand_tied: begin
           ;; If we are hand-tied, our work is done.  Pad the rest of
           ;; the param_print, so other param_prints line up.
           num_spaces = strlen(local_col_head) - strlen(local_param_print)
           param_print += local_param_print + string(format='(' + string(num_spaces) + '(a1))', " ") 
           return
        end
        else: message, 'ERROR: unrecognized pfo_link.link_status value: ' + string(parinfo[idx].pfo_link.link_status)
     endcase
     ;; link_ID
     local_param_print += string(format='(i7)', parinfo[idx].pfo_link.linkID)
     ;; If we are not a slave, our work is done.  Pad the rest of the
     ;; param_print, so other param_prints line up.
     if parinfo[idx].pfo_link.link_status ne !pfo.slave then begin
        num_spaces = strlen(local_col_head) - strlen(local_param_print)
        param_print += local_param_print + string(format='(' + string(num_spaces) + '(a1))', " ") 
        return
     endif ;; not a slave

     ;; to_ID
     local_param_print += string(format='(i6)', parinfo[idx].pfo_link.to_ID)

     ;; Translate to_ftype into parname.  See code and comments from __update
     ;; method
     ;; Get our to_ID and to_ftype
     to_ID = parinfo[idx].pfo_link.to_ID
     to_ftype = parinfo[idx].pfo_link.to_ftype
     ;; Make sure we can find our ID
     ID_idx = where(parinfo.pfo_link.linkID eq to_ID, count)
     if count eq 0 then begin
        message, 'WARNING: pfo_link.linkID ' + strtrim(to_ID, 2) + ' not found.  Skipping.', /CONTINUE
        local_param_print += '???'
        num_spaces = strlen(local_col_head) - strlen(local_param_print)
        param_print += local_param_print + string(format='(' + string(num_spaces) + '(a1))', " ") 
        return
     endif ;; to_ID points to missing linkID

     ;; Make sure we have just one parameter that we are trying to
     ;; point to.  
     to_ftype_idx = where(pfo_frac(parinfo[ID_idx].pfo.ftype, /round) eq $
                          pfo_frac(to_ftype, /round), count)
     if count ne 1 then begin
       message, 'WARNING: found ' + strtrim(count, 2) + ' parameters with fractional ftype = pfo_link.to_ftype = ' + strtrim(to_ftype, 2) + ' Non-unique linkID?  Skipping', /CONTINUE
        local_param_print += '???'
        num_spaces = strlen(local_col_head) - strlen(local_param_print)
        param_print += local_param_print + string(format='(' + string(num_spaces) + '(a1))', " ") 
        return
     endif ;; duplicate ftype (unlikely) or duplicate linkID (more likely)
     ;; unwrap
     to_ftype_idx = ID_idx[to_ftype_idx]
     local_param_print += string(format=pnformat, parinfo[to_ftype_idx].parname)     

     ;; Put everything back onto param_print
     param_print += local_param_print

     return

  endif ;; per parameter

end

;; Update method.  Transforms auto_link to link_status, and
;; link_status, linkID, to_ID and to_ftype into .tied.  Allows user to
;; have their own .tied
pro pfo_link_struct__update, $
   parinfo, $
   pfo_obj=pfo_obj, $ ;; We use the pfo_finfo system, which, if we are in object-oriented mode, uses information in the pfo_obj
   completed_updates=completed_updates, $       ;; (input/output) list of completed updates
   _REF_EXTRA=extra

  ;; Don't run more than once.
  if pfo_struct_updated(completed_updates) then $
     return

  ;; We need to make sure that fnums (integer part of ftype) are
  ;; defined when when look for identical functions in autolink
  pfo_parinfo_update, parinfo, required_tags='pfo', update_only_tags='pfo', $
                      completed_updates=completed_updates, $
                      pfo_obj=pfo_obj

  ;; Use only active parameters.  Also, don't use parameters "tied" by
  ;; hand.
  use_idx = where((parinfo.pfo.status AND !pfo.active) gt 0 and $
                  (parinfo.pfo_link.link_status AND !pfo.hand_tied) eq 0, npar)
  ;; Quietly return if we don't have any such parameters to manage
  if npar eq 0 then $
     return

  ;; Check for auto_links first
  auto_link_idx = where(parinfo[use_idx].pfo_link.auto_link ne !pfo.not_used, count) 
  if count gt 0 then begin
     ;; unwrap
     auto_link_idx = use_idx[auto_link_idx]

     ;; Clear away all link_status and to_IDs, since we are going to
     ;; assign them automatically
     parinfo[auto_link_idx].pfo_link.link_status = !pfo.not_used
     parinfo[auto_link_idx].pfo_link.to_ID = !tok.nowhere

     ;; Handle interlinks (links between functions) first, otherwise
     ;; intralinks in master function don't get set up properly.
     interlink_idx = where(parinfo[use_idx].pfo_link.auto_link eq !pfo.interlink, count)
     ;; We need to cycle through each kind of function
     while count gt 0 do begin
        ;; unwrap
        interlink_idx = use_idx[interlink_idx]
        ;; Find the first function and make it the master.  Making the
        ;; whole function master is why we have to do this first,
        ;; since intralinks may require some of these parameters to be
        ;; slaves
        master_idx = pfo_parinfo_parse(/indices, parinfo=parinfo, expand_idx=interlink_idx[0], pfo_obj=pfo_obj)
        ;; Don't clobber hand_tied status
        good_idx = where(parinfo[master_idx].pfo_link.link_status ne !pfo.hand_tied, master_count)
        if master_count gt 0 then begin
           ;; unwrap
           master_idx = master_idx[good_idx]
           parinfo[master_idx].pfo_link.link_status = !pfo.master
           ;; Check to make sure all parameters have the same ID
           ID = parinfo[master_idx].pfo_link.linkID
           if N_elements(uniq(ID, sort(ID))) gt 1 then begin
              message, /CONTINUE, 'WARNING: parinfo.pfo_link.linkID values are out of sync.  linkID should have the same value for all parameters in each function.  Reinitializing.'
              parinfo[master_idx].pfo_link.linkID = !tok.nowhere
           endif ;; non-syncronized linkIDs
           ;; --> having trouble with functions that already have linkID
           ;; set to the same value?
           ;; Check to see if we need to initialize ID
           if ID[0] lt 0 then begin
              parinfo[master_idx].pfo_link.linkID = max(parinfo.pfo_link.linkID) + 1
           endif ;; initializing ID
           ;; Now make local ID (possibly just initialized) just a scaler
           ID = parinfo[master_idx[0]].pfo_link.linkID
           ;; Find all of our master's slaves
           fnums = floor(parinfo.pfo.ftype)
           master_fnum = fnums[master_idx[0]]
           slave_idx = where(fnums[use_idx] eq master_fnum and $ ;; same functions
                             parinfo[use_idx].pfo_link.auto_link eq !pfo.interlink and $ ;; auto interlink
                             parinfo[use_idx].pfo_link.link_status eq !pfo.not_used, count) ;; not our master
           ;; Check to see if we found slaves.  If not, it is not a
           ;; problem to have marked our first and only function of this
           ;; type as the master, above.
           if count gt 0 then begin
              ;; unwrap
              slave_idx = use_idx[slave_idx]
              ;; We found some slaves.  Mark them as such, link it to the
              ;; master ID we found above.  The to_ftype should be
              ;; already assigned
              parinfo[slave_idx].pfo_link.link_status = !pfo.slave
              parinfo[slave_idx].pfo_link.to_ID = ID
           endif ;; found slaves
        endif ;; found non-hand_tied parameters for a master function

        ;; Get ready to handle our next interlinked function (if any)
        interlink_idx = where(parinfo[use_idx].pfo_link.auto_link eq !pfo.interlink and $ ;; interlinked functions
                              parinfo[use_idx].pfo_link.link_status ne !pfo.master and $ ;; not a master (which we would have just assigned)
                              parinfo[use_idx].pfo_link.to_ID eq !tok.nowhere, count) ;; slave status not assigned yet
     endwhile

     ;; Handle intralinks (links within one function) next.  This
     ;; enables us to set parameters to slaves that were
     ;; indiscriminately set to master in the interlink code.
     intralink_idx = where(parinfo[use_idx].pfo_link.auto_link eq !pfo.intralink, N_intralink)
     if N_intralink gt 0 then begin
        ;; unwrap
        intralink_idx = use_idx[intralink_idx]
        ; Mark intralinks as slaves
        parinfo[intralink_idx].pfo_link.link_status = !pfo.slave
     endif
     ;; Make sure intralink functions have a linkID and set the
     ;; intralink's to_ID to that value
     for ip=0, N_intralink-1 do begin
        f_idx = pfo_parinfo_parse(/indices, parinfo=parinfo, expand_idx=intralink_idx[ip], pfo_obj=pfo_obj)
        ;; Check to make sure all parameters have the same ID
        ID = parinfo[f_idx].pfo_link.linkID
        if N_elements(uniq(ID, sort(ID))) gt 1 then begin
           message, /CONTINUE, 'WARNING: parinfo.pfo_link.linkID values are out of sync.  linkID should have the same value for all parameters in each function.  Reinitializing.'
           parinfo[f_idx].pfo_link.linkID = !tok.nowhere
           ID = parinfo[f_idx].pfo_link.linkID
        endif ;; non-syncronized linkIDs
        ;; Check to see if we need to initialize ID
        if ID[0] lt 0 then begin
           ID = max(parinfo.pfo_link.linkID) + 1
           parinfo[f_idx].pfo_link.linkID = ID
        endif ;; initializing ID
        parinfo[intralink_idx[ip]].pfo_link.to_ID = ID[0]
     endfor ;; each intralinked parameter

  endif ;; auto_link

  ;; Now that we have our masters and slaves set up, populate the
  ;; MPFIT parinfo.tied tag

  ;; Clear away any old mentions of tied unless the user is
  ;; specifically marked them with !pfo.hand_tied.
  parinfo[use_idx].tied = ''  

  ;; Get slave indexes
  slave_idx = where(parinfo[use_idx].pfo_link.link_status eq !pfo.slave, nslaves)
  if nslaves gt 0 then begin
     ;; Unwrap
     slave_idx = use_idx[slave_idx]
  endif

  ;; This loop gets skipped if there are no slaves.  Too bad if someone
  ;; sets to_ID and to_ftype and forgets to set status=!pfo.slave
  for is=0,nslaves-1 do begin
     ;; Get our to_ID and to_ftype from the slave
     to_ID = parinfo[slave_idx[is]].pfo_link.to_ID
     to_ftype = parinfo[slave_idx[is]].pfo_link.to_ftype
     ;; Make sure we can find our ID
     ID_idx = where(parinfo.pfo_link.linkID eq to_ID, count)
     if count eq 0 then begin
        message, 'WARNING: pfo_link.linkID ' + strtrim(to_ID, 2) + ' not found.  Skipping.', /CONTINUE
        CONTINUE
     endif ;; to_ID points to missing linkID

     ;; Make sure we have just one parameter that we are trying to
     ;; point to.  
     to_ftype_idx = where(pfo_frac(parinfo[ID_idx].pfo.ftype, /round) eq $
                          pfo_frac(to_ftype, /round), count)
     if count ne 1 then begin
       message, 'WARNING: found ' + strtrim(count, 2) + ' parameters with fractional ftype = pfo_link.to_ftype = ' + strtrim(to_ftype, 2) + ' Non-unique linkID?  Skipping', /CONTINUE
       CONTINUE
    endif ;; duplicate ftype (unlikely) or duplicate linkID (more likely)
     ;; unwrap
     to_ftype_idx = ID_idx[to_ftype_idx]
     ;; OK, here we are finally ready to make the MPFIT tied assigment
     parinfo[slave_idx[is]].tied = 'P[' + strtrim(to_ftype_idx, 2) + ']'
     ;; While we are here, synchronize the values for the benefit of
     ;; pfo_funct calls independent of mpfit or tnmin
     parinfo[slave_idx[is]].value = parinfo[to_ftype_idx].value
     parinfo[slave_idx[is]].error = !values.d_nan
     ;; For now leave the rest of the tags alone, since just
     ;; specifying tied results in the proper calculation of free
     ;; parameters in mpfit and tnmin.  It also gives the user an
     ;; additional cache of those tags.     
  endfor ;; Each slave

  ;; Mark our update as complete regardless of how many errors we generated.
  pfo_struct_update_complete, completed_updates

  
end

pro pfo_link_struct__get_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_struct_setget_tag
   strict= strict, $ ;; See pfo_struct_setget_tag
   _REF_EXTRA   = extra, $
   linkID       = linkID    , $
   link_status     = link_status, $
   to_ID        = to_ID , $
   to_ftype     = to_ftype, $
   auto_link	= auto_link

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our tags into the
  ;; keywords

  if arg_present(linkID   ) or N_elements(linkID   ) ne 0 then linkID    = parinfo[idx].pfo_link.linkID   
  if arg_present(link_status ) or N_elements(link_status ) ne 0 then link_status  = parinfo[idx].pfo_link.link_status 
  if arg_present(to_ID    ) or N_elements(to_ID    ) ne 0 then to_ID     = parinfo[idx].pfo_link.to_ID    
  if arg_present(to_ftype ) or N_elements(to_ftype ) ne 0 then to_ftype  = parinfo[idx].pfo_link.to_ftype 
  if arg_present(auto_link) or N_elements(auto_link) ne 0 then auto_link = parinfo[idx].pfo_link.auto_link

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /get, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end 

;; This allows easy setting of tags in the pfo_link structure (a
;; top-level structure or as a pfo_link tag in a parinfo) from the
;; command line.  Alternately, you could just set the tags directly by
;; hand in your code.
pro pfo_link_struct__set_tag, $
   parinfo, $
   idx=idx, $
   taglist_series= taglist_series, $ ;; See pfo_struct_setget_tag
   strict= strict, $ ;; See pfo_struct_setget_tag
   _REF_EXTRA   	= extra, $
   linkID       = linkID    , $
   link_status     = link_status, $
   to_ID        = to_ID , $
   to_ftype     = to_ftype, $
   auto_link	= auto_link

  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: caught the above error.  Returning with what I have done so far ', /CONTINUE
        return
     endif
  endif ;; not debugging

  ;; Make sure idx exists
  pfo_idx, parinfo, idx

  ;; Put our struct into a tag, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; If we made it here, we are good to copy our keywords into the
  ;; tags

  if N_elements(linkID	) ne 0 then parinfo[idx].pfo_link.linkID   = linkID	
  if N_elements(link_status) ne 0 then parinfo[idx].pfo_link.link_status   = link_status	
  if N_elements(to_ID	) ne 0 then parinfo[idx].pfo_link.to_ID    = to_ID	
  if N_elements(to_ftype) ne 0 then parinfo[idx].pfo_link.to_ftype = to_ftype
  if N_elements(auto_link) ne 0 then parinfo[idx].pfo_link.auto_link   = auto_link	

  ;; Put our tag back on the top-level, if necessary
  pfo_struct_tagify, parinfo, tagified=tagified

  ;; Pass on keywords processed in series to the next top-level tag
  ;; listed in taglist_series.  
  pfo_struct_setget_tag, parinfo, idx=idx, /next, /set, $
                      taglist_series=taglist_series, $
                      strict=strict, $
                      _EXTRA=extra

end


;; Using the pfo_struct_new system, this is how the structure can be
;; initialized to non-null values, if necessary.  This also returns
;; the description of the structure for use in pfo_struct_append and
;; pfo_parinfo_template
function pfo_link_struct__init, descr=descr

  ;; Get IDL's version of null values
  pfo_link_struct = {pfo_link_struct}
  ;; Assign default values that we want
  pfo_link_struct.linkID = !tok.nowhere ;; invalid linkID
  pfo_link_struct.to_ID = !tok.nowhere ;; invalid linkID
  pfo_link_struct.to_ftype = !tok.nowhere ;; invalid ftype

  descr = $
    {README	: 'Structure to keep track of which parameters are linked to each other by the MPFIT "tied" system.  There are master parameters and slave parameters (sorry for the imagery).  Master parameters are the ones that are varied by MPFIT, slave parameters come along for the ride', $
     linkID	: 'Set this in the master function, one ID per function, the same ID for all the parameters, even for the ones that don''t participate', $
     link_status	: '0 = not linked, 1 = master, 2 = slave (token available in !pfo)', $
     to_ID	: 'set in the slave functions, indicating a master function ID', $
     to_ftype	: 'set in the slave functions indicating the _fractional_ ftype of the parameter in the master function (pfo_frac is helpful)', $
     auto_link  : 'Used to automatically assign master/slave relationships in identical functions.  0 = not_used, 1 (intralink)= link to parameter in this instance of the function, 2 (interlink) = link to parameter in another instance of this function' $
    }
  
  ;; The last thing we do is pass on any other keywords to our
  ;; __set_tag "method."  Do this with _STRICT_EXTRA to make sure that
  ;; no bogus keywords are passed.  Be careful with debugging and make
  ;; sure user gets the best effort case when we are trying to ignore
  ;; the error.
  if !pfo.debug le 0 then begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, /INFORMATIONAL, 'WARNING: the above error was produced.  Use pfo_debug to help fix error, pfo_quiet, to suppress reporting (not recommended).' 
        return, pfo_link_struct
     endif ;; CATCH
  endif ;; debugging
  pfo_link_struct__set_tag, pfo_link_struct, _STRICT_EXTRA=extra

  return, pfo_link_struct
end

;; Define the basic template for the structure.  This works with IDL's
;; implicit structure definition: ls = {pfo_link_struct}, but forces
;; initialization of the structure tags to IDL null values.
pro pfo_link_struct__define

  ;; Read in system variables for all routines in this file.
  init = {pfo_sysvar}
  init = {tok_sysvar}

  pfo_link_struct $
    = {pfo_link_struct, $
       link_status  : 0B, $ ;; [master]/slave/hand_tied status.  It is not an error to have a slave point to a slave.  Master designation is optional
       linkID       : 0, $ ;; Set this in the master function, one ID per function, the same ID for all the parameters, even for the ones that don't participate
       to_ID    : 0, $ ;; for slave functions: indicates linkID of master
       to_ftype : 0., $ ;; for slave functions: fractional ftype of master parameter
       auto_link: 0B $ ;; used in pfo_link_struct__update to help automatically assign master/slave to desired parameter(s)
      } 
end
