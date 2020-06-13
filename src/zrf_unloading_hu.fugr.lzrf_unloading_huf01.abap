*----------------------------------------------------------------------*
***INCLUDE LZRF_UNLOADING_HUF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_sscc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_sscc CHANGING iv_rehu TYPE /scwm/de_rf_rfhu
                    lv_qty TYPE int4.


  DATA: lv_rfc_logsys       TYPE logsys,
        lv_valid            TYPE char1,
        lv_objcode          TYPE zobj_code,
        ls_obj_request      TYPE /sttpec/s_att_obj_request,
        ls_objdata_top      TYPE /sttpec/s_att_obj_response,
        ls_objdata          TYPE /sttpec/s_att_obj_response,
        lt_top_objstruc_qty TYPE /sttpec/t_att_obj_response,
        lt_return           TYPE bapiret2_t.

  CLEAR: gv_matnr, gv_datex, gv_lotno.
* Get Logical System data
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_rfc_logsys  "Logical System
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

** Populate Object Code into local variable.
*  CONCATENATE '(00)' iv_rehu+2(18) INTO lv_objcode.
  CLEAR ls_obj_request.
  ls_obj_request-code_type = 'C'.
  ls_obj_request-code_char = iv_rehu.
* Call FM to check whether scanned HU is valid object in ATTP or not.
  CALL FUNCTION 'ZFM_RFC_GET_OBJ_DETAILS' DESTINATION 'ATDCLNT100_ABAP'
    EXPORTING
      is_objcode          = ls_obj_request
      i_get_top_level_id  = abap_true
      i_get_top_id_hry    = abap_true
    IMPORTING
      es_objdata          = ls_objdata
      es_objdata_top      = ls_objdata_top
      et_top_objstruc_qty = lt_top_objstruc_qty
      ex_matnr            = gv_matnr
      ex_lotno            = gv_lotno
      ex_datex            = gv_datex
      et_return           = lt_return.

* When no Error Message
  IF lt_return IS INITIAL.
    IF ls_objdata_top-enc_type = 'SSCC'.
      DELETE lt_top_objstruc_qty WHERE quantity IS INITIAL.
      DESCRIBE TABLE  lt_top_objstruc_qty LINES gv_qty.
      iv_rehu = ls_objdata_top-sscc.
    ELSE.
      MESSAGE e015(/sttpec/whs_msg).
    ENDIF.
  ENDIF.

ENDFORM.
