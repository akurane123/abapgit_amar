*----------------------------------------------------------------------*
***INCLUDE LZRF_PICK_HUF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_sn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZRF_S_CHILD_SN_RFSN
*&---------------------------------------------------------------------*
FORM validate_sn  USING    p_rfsn
                  CHANGING ls_objdata TYPE /sttpec/s_att_obj_response
                           p_meinh TYPE marm-meinh.

  DATA: lv_rfc_logsys  TYPE logsys,
        ls_obj_request TYPE /sttpec/s_att_obj_request,
        lv_valid_flag  TYPE xfeld,
        lv_sysubrc     TYPE sy-subrc.

  DATA: lo_messages TYPE REF TO /sttpec/cl_messages,
        ls_context  TYPE /sttpec/s_att_obj_context.

  CONSTANTS: gc_decode_mode  TYPE /sttpec/e_id_mode_dec VALUE 1.


* Create log
  IF lo_messages IS NOT BOUND.
    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
  ENDIF.


* request Object
  ls_obj_request-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
  ls_obj_request-code_char = p_rfsn.

  ls_obj_request-decode_mode = gc_decode_mode.
  ls_context-activity = /sttpec/cl_whs_constants=>gcs_activity-not_specified.


* Call Method to validate HU number entered on screen
  /sttpec/cl_att_functions=>validate_activity(
        EXPORTING
          is_objcode            = ls_obj_request
          is_validation_context = ls_context
          io_messages           = lo_messages
        IMPORTING
          es_objdata            = ls_objdata
          ev_valid_flag         = lv_valid_flag ).

  IF lv_valid_flag IS INITIAL.
* Error Message
    MESSAGE e002(/sttpec/whs_msg).
  ENDIF.

  IF ls_objdata-enc_type = 'SSCC'.
* Error Message
    MESSAGE e003(/sttpec/whs_msg).
  ENDIF.


  IF ls_objdata-enc_type = 'SGTIN'.
    DATA(lv_gtin) =  ls_objdata-gtin.
    SHIFT lv_gtin LEFT DELETING LEADING '0'.

    SELECT SINGLE ean11, meinh INTO @DATA(ls_data)
      FROM marm
      WHERE ean11 = @lv_gtin.
    IF sy-subrc = 0.
      p_meinh = ls_data-meinh.
    ENDIF.
  ENDIF.


** Call Method to get heirarchy of HU number entered on screen
*  /sttpec/cl_att_functions=>get_hierarchy_structure(
*        EXPORTING
*          is_objcode            = ls_obj_request
*          io_messages           = lo_messages
*        IMPORTING
*          et_objstruc           = gt_objstruc
*          et_objstruc_qty       = gt_objstruc_qty
*          et_objdata            = gt_objdata
*          ev_rc                 = lv_sysubrc ).
*
*  IF lv_valid_flag IS INITIAL.
*    CLEAR: p_rfsn.
** Error Message
*    MESSAGE e002(/sttpec/whs_msg).
*  ENDIF.

ENDFORM.
