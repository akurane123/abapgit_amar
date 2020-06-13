*----------------------------------------------------------------------*
***INCLUDE LZRF_CLOSE_HUF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form confirm_open_task
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_WRKC_LGNUM
*&      --> CS_PACK_VLENR
*&---------------------------------------------------------------------*
FORM confirm_open_task  USING    p_lgnum
                                 p_vlenr.


  DATA: lv_severity TYPE bapi_mtype,
        lt_msg      TYPE bapi_msg.

  DATA: lt_src_hu_open_to TYPE /scwm/tt_ordim_o.

  CALL FUNCTION '/SCWM/TO_READ_SRC'
    EXPORTING
      iv_lgnum     = p_lgnum
      iv_huident   = p_vlenr
    IMPORTING
      et_ordim_o   = lt_src_hu_open_to
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      OTHERS       = 4.

  ASSIGN lt_src_hu_open_to[ tostat = abap_false ] TO FIELD-SYMBOL(<fs_open_to>).
  IF sy-subrc = 0.
* confirm the task
    CALL FUNCTION 'Z_RF_CONFIRM_WAREHOUSE_TASK'
      EXPORTING
        i_lgnum    = p_lgnum
        i_tanum    = <fs_open_to>-tanum
        i_uname    = sy-uname
        i_lock     = 'X'
      IMPORTING
        e_severity = lv_severity
        e_message  = lt_msg.

    IF lv_severity = wmegc_severity_err.
*      WT Confirmation failed
      MESSAGE e204(/scwm/rf_en).
    ENDIF.
    WAIT UP TO 2 SECONDS.
  ENDIF.

  CALL METHOD /scwm/cl_tm=>cleanup( ).

ENDFORM.
