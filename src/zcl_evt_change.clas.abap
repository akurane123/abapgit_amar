class ZCL_EVT_CHANGE definition
  public
  final
  create public .

public section.

  interfaces /STTPEC/IF_BADI_WHS_EVT_O_CHG .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EVT_CHANGE IMPLEMENTATION.


  METHOD /sttpec/if_badi_whs_evt_o_chg~change_event.

    IF cs_event_data-bizstep EQ 'sap:att:activity:11'
    OR cs_event_data-bizstep EQ 'sap:att:activity:18'. " Trigger this Enhancement only for Shipping Event
      CHECK iv_action EQ 'OBSERVE'.
* This implementation is used to add Quantity into EPCIS message.
      READ TABLE it_objects INTO DATA(ls_obj) INDEX 1.
      IF sy-subrc IS INITIAL.
        CHECK cs_event_data-quantitylist[] IS INITIAL.
        CHECK ls_obj-quantity IS NOT INITIAL.
        APPEND INITIAL LINE TO cs_event_data-quantitylist ASSIGNING FIELD-SYMBOL(<lfs_qty_list>).
        <lfs_qty_list>-quantity_element = ls_obj-code_char.
        <lfs_qty_list>-quantity         = ls_obj-quantity.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
