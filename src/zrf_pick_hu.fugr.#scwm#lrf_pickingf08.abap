*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PICKINGF08.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form switch_fields_for_dd
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM switch_fields_for_dd USING is_who  TYPE /scwm/s_who_int
                                iv_line TYPE i
                                it_pickhu TYPE  /scwm/tt_rf_pick_hus
                          CHANGING ct_ordim_confirm TYPE  /scwm/tt_rf_ordim_confirm
                                   cs_ordim_confirm TYPE  /scwm/s_rf_ordim_confirm.
  DATA: ls_twho_dd        TYPE  /scwm/twho_dd,
        lv_show_hu_logpos.

  "Distribution Equipment: switch on/off dest log pos
  IF is_who-type <> wmegc_wcr_dd .
    RETURN.
  ENDIF.

  BREAK-POINT ID /scwm/dd_picking.


* dlogpos_ext_w: can be visible and available for verification
  IF cs_ordim_confirm-dlogpos_ext_wt IS INITIAL.
    "logpos_ext_wt is not predetermined -> hide the display and verification field
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                           gc_scr_elmnt_dlogpos_ext_wt ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                           gc_scr_elmnt_dlgpos_ext_wt_vrf ).
  ELSE.
    "logpos_ext_wt is predetermined, show the display field
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
                           gc_scr_elmnt_dlogpos_ext_wt ).
    "framework takes care of logpos_wt verification field
  ENDIF.

* logpos_ext hu: can be open for input
  IF "cs_ordim_confirm-logpos_ext IS INITIAL AND
     cs_ordim_confirm-dlogpos_ext_wt IS INITIAL.
    " case 1: logpos_ext_wt is not predetermined -> show HU logpos
    lv_show_hu_logpos = abap_true.
  ELSEIF cs_ordim_confirm-dlogpos_ext_wt IS NOT INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                           gc_scr_elmnt_logpos_ext ).
  ENDIF.

  IF lv_show_hu_logpos IS NOT INITIAL AND
     is_who-wcr IS NOT INITIAL.
* we check in the settings table, if LOGPOS_EXT (HU) should be hidden
    DATA(lo_dd_picking) = /scwm/cl_dd_picking=>get_instance( ).
    CALL METHOD lo_dd_picking->get_dd_settings
      EXPORTING
        is_who     = is_who
      RECEIVING
        es_twho_dd = ls_twho_dd.
    IF ls_twho_dd-hide_logpos_ext IS NOT INITIAL.
      CLEAR lv_show_hu_logpos.
    ENDIF.
  ENDIF.
  IF lv_show_hu_logpos IS INITIAL.
* switch off this field
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                           gc_scr_elmnt_logpos_ext ).
    else.
    " switch on this field
    /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                           gc_scr_elmnt_logpos_ext ).
    " check, if pick-hu has logpos_ext provided (e.g. rack-hu)
    DATA: lt_huident TYPE /scwm/tt_huident,
          lt_huhdr   TYPE /scwm/tt_huhdr_int.

    IF cs_ordim_confirm-huid IS NOT INITIAL.
      CALL METHOD lo_dd_picking->get_hu_by_huid(
        EXPORTING
          iv_lgnum = cs_ordim_confirm-lgnum
          iv_who   = cs_ordim_confirm-who
          iv_huid  = cs_ordim_confirm-huid
        IMPORTING
          et_huhdr = lt_huhdr ).
    ELSE.
      lt_huident = CORRESPONDING #( it_pickhu ).
      CALL FUNCTION '/SCWM/HU_READ_MULT'
        EXPORTING
          it_huident = lt_huident
          iv_lgnum   = cs_ordim_confirm-lgnum
        IMPORTING
          et_huhdr   = lt_huhdr
        EXCEPTIONS
          OTHERS     = 0.
    ENDIF.
    CALL METHOD /scwm/cl_rf_bll_srvc=>init_listbox
      EXPORTING
        iv_fieldname = '/SCWM/S_RF_ORDIM_CONFIRM-LOGPOS_EXT'.

    LOOP AT lt_huhdr ASSIGNING FIELD-SYMBOL(<ls_huhdr>) WHERE logpos_ext IS NOT INITIAL.
      /scwm/cl_rf_bll_srvc=>insert_listbox(
        iv_fieldname = '/SCWM/S_RF_ORDIM_CONFIRM-LOGPOS_EXT'
        iv_value = <ls_huhdr>-logpos_ext ).
    ENDLOOP.
  ENDIF.
ENDFORM.
