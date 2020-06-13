*----------------------------------------------------------------------*
***INCLUDE LZRF_REAPACKING_SSCCF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_sscc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CS_PACK_RFHU
*&---------------------------------------------------------------------*
FORM validate_sscc  USING p_rfhu TYPE /scwm/de_rf_rfhu_long.
*
  DATA: lv_rfc_logsys  TYPE logsys,
        ls_obj_request TYPE /sttpec/s_att_obj_request,
        ls_objdata     TYPE /sttpec/s_att_obj_response,
        lv_valid_flag  TYPE xfeld,
        lv_sysubrc     TYPE sy-subrc,
        lv_gtin        TYPE /sttpec/e_gs1_gtin,
        lv_matid       TYPE /scwm/de_matid,
        ls_mat_global  TYPE /scwm/s_material_global.


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
  ls_obj_request-code_char = p_rfhu.

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
    CLEAR: p_rfhu.
* Error Message
    MESSAGE e002(/sttpec/whs_msg).
  ENDIF.

  IF ls_objdata-enc_type <> 'SSCC'.
    CLEAR: p_rfhu.
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

* get material number from GTIN
  DELETE gt_objdata WHERE gtin IS INITIAL.
  IF gt_objdata IS NOT INITIAL.
    ASSIGN gt_objdata[ 1 ] TO FIELD-SYMBOL(<fs_gs_object>).

    lv_gtin = <fs_gs_object>-gtin.
    /sttpec/cl_whs_md_access=>get_matnr_4_gtin(
          EXPORTING
            iv_gtin            = lv_gtin
          IMPORTING
            ev_matnr           = gv_matnr
            ev_meinh           = gv_uom ).
  ENDIF.

* get Material Description
  IF gv_matnr IS NOT INITIAL.
    /scwm/cl_lm_data_utility=>mat_read_single(
          EXPORTING
            iv_matnr = gv_matnr
            iv_lgnum = 'MWH1'
          IMPORTING
            es_mat_global = ls_mat_global
          ).
    gv_maktx = ls_mat_global-maktx.
    gv_uom = ls_mat_global-meins.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_child_sn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZS_CHILD_SN_RFSN
*&---------------------------------------------------------------------*
FORM validate_child_sn  USING p_rfsn TYPE zrf_s_child_sn-rfsn
                        CHANGING ls_objdata TYPE /sttpec/s_att_obj_response.

  DATA: lv_rfc_logsys  TYPE logsys,
        ls_obj_request TYPE /sttpec/s_att_obj_request,
        lv_valid_flag  TYPE xfeld.

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
    CLEAR: p_rfsn.
* Error Message
    MESSAGE e002(/sttpec/whs_msg).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form create_move
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZT_CHILD_SN
*&---------------------------------------------------------------------*
FORM create_move  USING    p_child_sn TYPE zrf_tt_child_sn.
  DATA: lv_huident_dest TYPE /scwm/huident,
        lv_huident_src  TYPE /scwm/huident,
        lv_stock_guid   TYPE /lime/guid_stock,
        ls_quantity     TYPE /scwm/s_quan.

  DATA: lo_pack TYPE REF TO /scwm/cl_wm_packing.


  DATA: lt_huitems_dest TYPE  /scwm/tt_huitm_int,
        ls_huhdr_dest   TYPE /scwm/s_huhdr_int,
        lt_huhdr_dest   TYPE /scwm/tt_huhdr_int.

  DATA: lt_huitems_src TYPE  /scwm/tt_huitm_int,
        ls_huhdr_src   TYPE /scwm/s_huhdr_int,
        lt_huhdr_src   TYPE /scwm/tt_huhdr_int.

  DATA: lt_create   TYPE /scwm/tt_to_create_int,
        ls_create   TYPE /scwm/s_to_create_int,
        lv_tanum    TYPE  /scwm/tanum,
        lt_ltap_vb  TYPE  /scwm/tt_ltap_vb,
        ls_ltap_vb  TYPE /scwm/ltap,
        lt_bapi_ret TYPE bapirettab,
        ls_bapi_ret TYPE bapiret2.


  DATA: lo_matid    TYPE REF TO /scwm/cl_ui_stock_fields,
        lv_matid    TYPE /scwm/de_matid,
        lv_batchid  TYPE /scwm/de_batchid,
        lv_entitled TYPE /scwm/de_entitled,
        lv_qty      TYPE /scwm/rl03tanfme,
        lv_matnr    TYPE /scwm/de_matnr,
        lv_charg    TYPE /scwm/de_charg,
        lv_severity TYPE bapi_mtype.



  CONSTANTS: lv_procty   TYPE /scwm/de_procty VALUE '9999',
             lv_pack_mat TYPE /scwm/de_ui_id VALUE 'EURO PALLET'.
* Intialize the quantity
  lv_qty = 0.

  LOOP AT p_child_sn ASSIGNING FIELD-SYMBOL(<fs_total>).
    lv_qty = lv_qty + <fs_total>-quantity.
  ENDLOOP.

******* get warehouse PID **************
  SELECT SINGLE lgnum FROM /scwm/user
    INTO gv_lgnum WHERE uname = sy-uname.
******set Warehouse Number
  CALL METHOD /scwm/cl_tm=>set_lgnum( gv_lgnum ).


  ASSIGN p_child_sn[ 1 ] TO FIELD-SYMBOL(<fs_child_sn>).
  IF sy-subrc = 0.

* Get Source HU details
    lv_huident_src    = <fs_child_sn>-rfhu_parent2+4(18).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_huident_src
      IMPORTING
        output = lv_huident_src.
    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = 'WME'
        iv_lgnum   = gv_lgnum
        iv_huident = lv_huident_src
      IMPORTING
        et_huitm   = lt_huitems_src
        es_huhdr   = ls_huhdr_src
        et_huhdr   = lt_huhdr_src
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.

    IF sy-subrc IS NOT INITIAL.
* error message
    ENDIF.

    lv_huident_dest    = <fs_child_sn>-rfhu_parent1+4(18).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_huident_dest
      IMPORTING
        output = lv_huident_dest.

* Get Destination HU details
    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = 'WME'
        iv_lgnum   = gv_lgnum
        iv_huident = lv_huident_dest
      IMPORTING
        et_huitm   = lt_huitems_dest
        es_huhdr   = ls_huhdr_dest
        et_huhdr   = lt_huhdr_dest
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.

* if Destination HU not found then create one
    IF sy-subrc = 1.
      CALL FUNCTION 'ZMV_CREATE_HU'
        EXPORTING
          iv_hu       = lv_huident_dest
          iv_pack_mat = lv_pack_mat
          iv_location = ls_huhdr_src-lgpla
        IMPORTING
          es_huhdr    = ls_huhdr_dest.
    ENDIF.


********** Repack Stock from one HU to Another
    ASSIGN lt_huitems_src[ 1 ] TO FIELD-SYMBOL(<fs_huitm_src>).
    IF sy-subrc = 0.

      ls_create-procty = lv_procty.
      ls_create-matid = <fs_huitm_src>-matid.
      ls_create-batchid = <fs_huitm_src>-batchid.
      ls_create-cat = <fs_huitm_src>-cat.
      ls_create-entitled = <fs_huitm_src>-entitled.
      ls_create-owner = <fs_huitm_src>-entitled.
      ls_create-owner_role = 'BP'.
      ls_create-entitled_role = 'BP'.
      ls_create-guid_stock = <fs_huitm_src>-guid_stock.
      ls_create-letyp = ls_huhdr_src-letyp.
      ls_create-anfme = lv_qty.
      ls_create-altme = <fs_huitm_src>-altme.
      ls_create-opunit = 'EA'.
      ls_create-kompl = 'X'.
      ls_create-vltyp = ls_huhdr_src-lgtyp.
      ls_create-vlpla = ls_huhdr_src-lgpla.
      ls_create-vlenr = lv_huident_src.
      ls_create-sguid_hu = ls_huhdr_src-guid_hu.
      ls_create-nlpla = ls_huhdr_dest-lgpla.
      ls_create-nlenr = lv_huident_dest.
      ls_create-single_to = 'X'.
      ls_create-seqno = 1.
      APPEND ls_create TO lt_create.

      CALL FUNCTION '/SCWM/TO_CREATE'
        EXPORTING
          iv_lgnum         = gv_lgnum
*         IV_UPDATE_TASK   = 'X'
*         IV_COMMIT_WORK   = 'X'
          iv_wtcode        = 'D'
          iv_bname         = sy-uname
*         IS_RFC_QUEUE     =
          it_create        = lt_create
*         IT_CREATE_EXC    =
          iv_processor_det = 'X'
        IMPORTING
          ev_tanum         = lv_tanum
          et_ltap_vb       = lt_ltap_vb
          et_bapiret       = lt_bapi_ret
          ev_severity      = lv_severity.

      PERFORM error_analysis
      USING lv_severity
            lt_bapi_ret.

      COMMIT WORK AND WAIT.
      IF sy-subrc IS NOT INITIAL.
*      WT creation failed
        MESSAGE e203(/scwm/rf_en).
      ENDIF.
      CALL METHOD /scwm/cl_tm=>cleanup( ).
      gv_tanum = lv_tanum.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form confirm_move
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZT_CHILD_SN
*&---------------------------------------------------------------------*
FORM confirm_move  USING    p_child_sn TYPE zrf_tt_child_sn.

  DATA: lv_severity TYPE bapi_mtype,
        lt_msg      TYPE bapi_msg.


  CALL FUNCTION 'Z_RF_CONFIRM_WAREHOUSE_TASK'
    EXPORTING
      i_lgnum    = gv_lgnum
      i_tanum    = gv_tanum
      i_uname    = sy-uname
      i_lock     = 'X'
    IMPORTING
      e_severity = lv_severity
      e_message  = lt_msg.


  IF lv_severity = wmegc_severity_err.
*      WT Confirmation failed
    MESSAGE e204(/scwm/rf_en).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form error_analysis
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SEVERITY
*&      --> LT_BAPI_RET
*&---------------------------------------------------------------------*
FORM error_analysis   USING  VALUE(iv_severity)       TYPE bapi_mtype
                                   it_bapiret         TYPE bapiret2_t .

  DATA:
    ls_bapiret TYPE bapiret2.


  CHECK iv_severity CA 'EAX'.  "wmegc_severity_ea ??
  LOOP AT it_bapiret INTO ls_bapiret.
    IF ls_bapiret-type CA wmegc_severity_ea.
      MESSAGE    ID     ls_bapiret-id
                 TYPE   ls_bapiret-type
                 NUMBER ls_bapiret-number
                 WITH   ls_bapiret-message_v1
                        ls_bapiret-message_v2
                        ls_bapiret-message_v3
                        ls_bapiret-message_v4.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form deconsolidate
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZT_CHILD_SN
*&---------------------------------------------------------------------*
FORM deconsolidate  USING    lt_child_sn TYPE zrf_tt_child_sn.


  DATA: lv_qty       TYPE /scwm/ltap_nista,
        iv_source_hu TYPE /scwm/huident,
        iv_dest_hu   TYPE /scwm/huident,
        lv_severity  TYPE  bapi_mtype,
        lv_message   TYPE  bapi_msg.


  ASSIGN lt_child_sn[ 1 ] TO FIELD-SYMBOL(<fs_child_data>).
  IF sy-subrc = 0 .
* Intialize the quantity
    lv_qty = 0.
    LOOP AT lt_child_sn ASSIGNING FIELD-SYMBOL(<fs_total>).
      lv_qty = lv_qty + <fs_total>-quantity.
    ENDLOOP.
    iv_source_hu = <fs_child_data>-rfhu_parent2+4(18).
    iv_dest_hu = <fs_child_data>-rfhu_parent1+4(18).

* Confirm the Deconsolidation Task
    CALL FUNCTION 'Z_RF_CONFIRM_DECON_WH_TASK1'
      EXPORTING
        iv_lgnum     = gv_lgnum
        iv_source_hu = iv_source_hu
        iv_dest_hu   = iv_dest_hu
        iv_qty       = lv_qty
      IMPORTING
        e_severity   = lv_severity
        e_message    = lv_message.

    IF lv_severity = wmegc_severity_err.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_confirm_open_task
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_LGNUM
*&      --> LV_SOURCE_HU
*&---------------------------------------------------------------------*
FORM check_confirm_open_task  USING    p_lgnum
                                       p_source_hu.
  DATA: lv_severity TYPE bapi_mtype,
        lt_msg      TYPE bapi_msg.

  DATA: lt_src_hu_open_to TYPE /scwm/tt_ordim_o.

  CALL FUNCTION '/SCWM/TO_READ_SRC'
    EXPORTING
      iv_lgnum     = p_lgnum
      iv_huident   = p_source_hu
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
