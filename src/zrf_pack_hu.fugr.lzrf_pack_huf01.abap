*----------------------------------------------------------------------*
***INCLUDE LZRF_PACK_HUF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_sscc_content
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_PACK_NLENR
*&      <-- LT_CHILD_SN
*&---------------------------------------------------------------------*
FORM get_sscc_content  USING    p_pack_nlenr
                                p_pack_vlenr
                       CHANGING pt_child_sn TYPE zrf_tt_child_sn.
  DATA: lv_rfc_logsys  TYPE logsys,
        ls_obj_request TYPE /sttpec/s_att_obj_request,
        lt_obj_request TYPE /sttpec/t_att_obj_request,
        ls_objdata     TYPE /sttpec/s_att_obj_response,
        lv_valid_flag  TYPE xfeld,
        lv_sysubrc     TYPE sy-subrc,
        lv_gtin        TYPE /sttpec/e_gs1_gtin,
        lv_matid       TYPE /scwm/de_matid,
        ls_child_sn    TYPE zrf_s_child_sn,
        lv_objcode     TYPE stringval,
        ls_mat_global  TYPE /scwm/s_material_global.

  DATA: gt_objstruc     TYPE /sttpec/t_att_obj_hierarchy,
        gt_objstruc_qty TYPE /sttpec/t_att_qty_hierarchy,
        gt_objdata      TYPE /sttpec/t_att_obj_response.

  DATA: lo_messages TYPE REF TO /sttpec/cl_messages,
        ls_context  TYPE /sttpec/s_att_obj_context.


  CONSTANTS:        gc_decode_mode  TYPE /sttpec/e_id_mode_dec VALUE 1.
* Create log
  IF lo_messages IS NOT BOUND.
    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
  ENDIF.


*  CLEAR: gv_matnr, gv_datex, gv_lotno.
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

* request Object
  ls_obj_request-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
  CONCATENATE '(00)' p_pack_vlenr+2(18) INTO ls_obj_request-code_char.
  CONCATENATE '(00)' p_pack_nlenr+2(18) INTO lv_objcode.

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
    RETURN.
*    CLEAR: p_pack_nlenr.
** Error Message
*    MESSAGE e002(/sttpec/whs_msg).
  ENDIF.

  IF ls_objdata-enc_type <> 'SSCC'.
    CLEAR: p_pack_nlenr.
* Error Message
    MESSAGE e003(/sttpec/whs_msg).
  ENDIF.

* Call Method to validate HU number entered on screen
  /sttpec/cl_att_functions=>get_hierarchy_structure(
        EXPORTING
          is_objcode            = ls_obj_request
          io_messages           = lo_messages
        IMPORTING
          et_objstruc           = gt_objstruc
          et_objstruc_qty       = gt_objstruc_qty
          et_objdata            = gt_objdata
          ev_rc                 = lv_sysubrc ).


  LOOP AT gt_objdata ASSIGNING FIELD-SYMBOL(<fs_objdata>) WHERE status_pack = 1.
    ls_child_sn-rfhu_parent1 = lv_objcode.
    ls_child_sn-rfhu_parent2 = ls_obj_request-code_char.
    ls_child_sn-rfsn = <fs_objdata>-code_urn.

    APPEND ls_child_sn TO pt_child_sn.
    CLEAR: ls_child_sn.
  ENDLOOP.


ENDFORM.
