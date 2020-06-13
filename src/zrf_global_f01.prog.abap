*----------------------------------------------------------------------*
***INCLUDE /STTPEC/WHS_TEST_TOOLBOX_F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_VAL
*&---------------------------------------------------------------------*
*       Call validation logic for the active tab
*----------------------------------------------------------------------*
FORM user_command_val USING    iv_activetab     TYPE syucomm
                      CHANGING cs_tbox_fields   TYPE /sttpec/s_whs_test_tbox_flds
                               ct_tbox_resobj   TYPE /sttpec/t_whs_test_tbox_resobj.

  DATA: lv_rc             TYPE sysubrc,
        lv_existance      TYPE char1,
        lv_vld_parent     TYPE xfeld, " flag to validate the parent
        lt_return         TYPE bapiret2_t,
        ls_context_base   TYPE /sttpec/s_whsdat_bas,
        ls_context        TYPE /sttpec/s_whsdat_ctx,
        lt_context_sd	    TYPE /sttpec/t_whsdat_srcdest,
        ls_context_parent TYPE /sttpec/s_whsdat_ctx,
        ls_object         TYPE /sttpec/s_obj_data,
        ls_object_parent  TYPE /sttpec/s_obj_data,
        ls_correct_parent TYPE /sttpec/s_obj_data,
        ls_tbox_resobj    TYPE /sttpec/s_whs_test_tbox_resobj,
        ls_objcode        TYPE /sttpec/s_att_obj_request,
        ls_criteria       TYPE /sttpec/s_att_obj_crit_tgt,
        ls_objdata        TYPE /sttpec/s_att_obj_response,
        ls_objdata_parent TYPE /sttpec/s_att_obj_response,
        ls_doc_objects    TYPE /sttpec/s_whs_test_tbox_docobj,
        lv_doc_content    TYPE xfeld,
        lo_messages       TYPE REF TO /sttpec/cl_messages,
        ls_dummy_msg      TYPE  symsg,                      "#EC NEEDED
        lt_messages       TYPE  /sttpec/cl_messages=>tt_bal_msg,
        ls_messages       TYPE /sttpec/cl_messages=>ts_bal_msg.
  DATA: lv_rfc_logsys   TYPE logsys.
  FIELD-SYMBOLS:
    <ls_object>      TYPE /sttpec/s_att_obj_chk,
    <ls_doc_objects> TYPE /sttpec/s_whs_test_tbox_docobj,
    <ls_tbox_resobj> TYPE /sttpec/s_whs_test_tbox_resobj.
* ---------------------------------------------------------------------

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
  cs_tbox_fields-logsys = lv_rfc_logsys.
  cs_tbox_fields-plant = '2000'.
* Create log
  IF lo_messages IS NOT BOUND.
    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
  ENDIF.

* ---------------------------------------------------------------------
  " assume current year document for test UI
  IF cs_tbox_fields-docyear IS INITIAL AND cs_tbox_fields-docnum IS NOT INITIAL.
    cs_tbox_fields-docyear = sy-datum(4).
  ENDIF.

* fill in WHS context data
  ls_context_base-ctx_ref   = cs_tbox_fields-ctx_ref.
  ls_context_base-ctx_loc   = cs_tbox_fields-ctx_loc.
  ls_context_base-custfield = iv_activetab. "will be used to distinguish between tabs with the same BF

  IF ls_context_base-doctpe IS INITIAL.
    PERFORM map_tbox_to_doctpe USING    iv_activetab
                                        ls_context_base-docnum
                               CHANGING ls_context_base-doctpe.
  ENDIF.

  PERFORM map_tbox_to_busfunc USING    iv_activetab
                              CHANGING ls_context_base-busfunc.
  IF  ls_context_base-busfunc IS INITIAL
  AND iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-identcont.
*    "Business function could not be determined.
*    MESSAGE s003 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
* ---------------------------------------------------------------------
* Check Child object
  CLEAR ls_object.
  IF cs_tbox_fields-scan_obj IS NOT INITIAL.
    ls_object-code_char = cs_tbox_fields-scan_obj.
    ls_object-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
  ENDIF.

  IF  iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-identcont
  AND iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-count.
    PERFORM create_check_buffer USING    ls_object
                                         abap_false
                                CHANGING ls_context_base
                                         lv_existance
                                         lv_rc
                                         lo_messages.
    IF lv_rc <> 0.
      MESSAGE e010(zmsg_attp_ewm)."Error reading buffer
***      PERFORM log_display USING lo_messages.
      RETURN.
    ENDIF.

***    IF lv_existance IS NOT INITIAL.
***      "This object has been already validated
***      MESSAGE s001 DISPLAY LIKE 'E'.
***      RETURN.
***    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING cs_tbox_fields TO ls_context.          "#EC ENHOK
  IF iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-count.
    CLEAR ls_context-ctx_loc. "as there is no location change
  ENDIF.

* Check if the document content provided
  CLEAR:ls_doc_objects, lv_doc_content.
  IF iv_activetab EQ /sttpec/cl_whs_test_constants=>gcs_tbox_activity-grhu
  OR iv_activetab EQ /sttpec/cl_whs_test_constants=>gcs_tbox_activity-ship.
    READ TABLE gt_doc_objects INTO ls_doc_objects WITH KEY ctx_ref = ls_context_base-ctx_ref.
    IF sy-subrc IS INITIAL.
      lv_doc_content = abap_true.
    ENDIF.

    PERFORM get_context_srcdest USING    cs_tbox_fields-ctx_gln
                                CHANGING lt_context_sd.
  ENDIF.
* ---------------------------------------------------------------------
* Check Parent Object
  lv_vld_parent = abap_true. " by default

  IF cs_tbox_fields-scan_obj_parent IS NOT INITIAL.
    ls_object_parent-code_char = cs_tbox_fields-scan_obj_parent.
    ls_object_parent-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.

*   Check the existance of Parent object in this buffer
    IF ls_context_base-bufferid IS NOT INITIAL.
      PERFORM create_check_buffer USING    ls_object_parent
                                           abap_true
                                  CHANGING ls_context_base
                                           lv_existance
                                           lv_rc
                                           lo_messages.
      IF lv_rc <> 0.
        MESSAGE e010(zmsg_attp_ewm)."Error reading buffer
*        PERFORM log_display USING lo_messages.
*        RETURN.
      ENDIF.

      IF lv_existance IS NOT INITIAL.
        "This Parent object has been already validated
        lv_vld_parent = abap_false.
      ENDIF.
    ENDIF.

  ENDIF.

  ls_context_parent-sscc  = cs_tbox_fields-sscc_parent.
  ls_context_parent-unit  = cs_tbox_fields-unit_parent.
* ---------------------------------------------------------------------
  CASE iv_activetab.
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-grhu.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_receive_container
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
          ib_doc_check       = cs_tbox_fields-doc_check
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          ct_context_sd      = lt_context_sd
          cs_object          = ls_object
          ct_objects         = ls_doc_objects-objects. "document content

      IF ls_object-obj_ids IS NOT INITIAL.
*       Update the global table with the doc.content found in BF
        IF lv_doc_content IS NOT INITIAL.
          READ TABLE gt_doc_objects ASSIGNING <ls_doc_objects> WITH KEY ctx_ref = ls_context_base-ctx_ref.
          IF sy-subrc IS INITIAL.
            <ls_doc_objects>-objects =  ls_doc_objects-objects.
          ENDIF.

        ELSE.
          IF ls_doc_objects-objects IS NOT INITIAL.
            APPEND INITIAL LINE TO gt_doc_objects ASSIGNING <ls_doc_objects>.
            IF <ls_doc_objects> IS ASSIGNED.
              <ls_doc_objects>-ctx_ref = ls_context_base-ctx_ref.
              <ls_doc_objects>-objects = ls_doc_objects-objects.
            ENDIF.
          ENDIF.
        ENDIF.

*       Update the result list
        READ TABLE ct_tbox_resobj ASSIGNING <ls_tbox_resobj> WITH KEY obj_ids = ls_object-obj_ids.
        IF sy-subrc IS INITIAL.
          IF lv_rc IS INITIAL.
            <ls_tbox_resobj>-match = abap_true.
          ENDIF.
        ELSE.
          IF lv_rc IS INITIAL.
            READ TABLE ls_doc_objects-objects ASSIGNING <ls_object> WITH KEY obj_ids = ls_object-obj_ids.
            IF sy-subrc IS INITIAL.
              APPEND INITIAL LINE TO ct_tbox_resobj ASSIGNING <ls_tbox_resobj>.
              IF <ls_tbox_resobj> IS ASSIGNED.
                MOVE-CORRESPONDING <ls_object> TO <ls_tbox_resobj>.
                " we assume that no duplicates can exist in ERP even w/o docyear
                <ls_tbox_resobj>-docnum = cs_tbox_fields-docnum.
                <ls_tbox_resobj>-docpos = cs_tbox_fields-docpos.
                <ls_tbox_resobj>-match  = <ls_object>-status.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-putaway.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_putaway
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickitem
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickhu.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_pick
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
          ib_doc_check       = cs_tbox_fields-doc_check
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
      IF lv_rc IS INITIAL AND iv_activetab = /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickitem.
*       Update Result list
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          ls_tbox_resobj-matnr = ls_context-matnr.
          ls_tbox_resobj-unit  = ls_context-unit.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-create.

*     Means the creation of a new object in att
      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_create
        EXPORTING
          ib_use_buffer      = abap_true
          iv_snr_usetype     = cs_tbox_fields-usetype  "object type
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-decommiss.

*     Means the decommission of object and its dependent objects
      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_decommission
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pick2p.

*     if parent object has been already validated
      IF lv_vld_parent IS INITIAL.
        CLEAR: ls_context_parent, ls_object_parent.
      ENDIF.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_pick2pallet
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
          ib_doc_check       = cs_tbox_fields-doc_check
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context_source  = ls_context
          cs_context_target  = ls_context_parent
          cs_object_source   = ls_object
          cs_object_target   = ls_object_parent.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          ls_tbox_resobj-matnr = ls_context-matnr.
          ls_tbox_resobj-unit  = ls_context-unit.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packhu
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packuom.

      IF  iv_activetab = /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packhu.
        IF ls_object_parent IS INITIAL.
          lv_vld_parent = abap_false.
        ENDIF.
      ELSE.
*       prefill the material for SGTIN creation/validation
        ls_context_parent-matnr = ls_context-matnr(18).
      ENDIF.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_pack
        EXPORTING
          ib_use_buffer      = abap_true
          iv_vld_target      = lv_vld_parent  "flag to validate Parent object
          ib_skip_validation = cs_tbox_fields-skip_validation
          ib_doc_check       = cs_tbox_fields-doc_check
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context_source  = ls_context
          cs_context_target  = ls_context_parent
          cs_object_source   = ls_object
          cs_object_target   = ls_object_parent.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_ids IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          ls_tbox_resobj-matnr = ls_context-matnr.
          ls_tbox_resobj-unit  = ls_context-unit.
          IF ls_object_parent-code_urn IS NOT INITIAL.
            ls_tbox_resobj-scan_obj_parent = ls_object_parent-code_urn.
          ELSE.
            ls_tbox_resobj-scan_obj_parent = cs_tbox_fields-scan_obj_parent.
          ENDIF.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-unpack.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_unpack
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context_source  = ls_context_parent
          cs_context_target  = ls_context
          cs_object_source   = ls_object_parent
          cs_object_target   = ls_object.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_ids IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          ls_tbox_resobj-matnr = ls_context-matnr.
          ls_tbox_resobj-unit  = ls_context-unit.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-lothry.

      IF ls_object_parent IS INITIAL
      OR cs_tbox_fields-evtaction = /sttpec/cl_whs_constants=>gcs_evtaction-delete.
        lv_vld_parent = abap_false.
      ENDIF.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_lot_hierarchy
        EXPORTING
          ib_use_buffer      = abap_true
          iv_vld_target      = lv_vld_parent  "flag to validate Parent object
          is_whsdat_ctx_ctrl = cs_tbox_fields-ctx_ctrl
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context_source  = ls_context
          cs_context_target  = ls_context_parent
          cs_object_source   = ls_object
          cs_object_target   = ls_object_parent.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_ids IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          ls_tbox_resobj-matnr = ls_context-matnr.
          ls_tbox_resobj-unit  = ls_context-unit.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-load.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_load_container
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
          ib_doc_check       = cs_tbox_fields-doc_check
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-ship.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_ship_container
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = cs_tbox_fields-skip_validation
          ib_doc_check       = cs_tbox_fields-doc_check
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          ct_context_sd      = lt_context_sd
          cs_object          = ls_object
          ct_objects         = ls_doc_objects-objects.

      IF ls_object-obj_ids IS NOT INITIAL.
*       Update the global table with the doc.content found in BF
        IF lv_doc_content IS NOT INITIAL.
          READ TABLE gt_doc_objects ASSIGNING <ls_doc_objects> WITH KEY ctx_ref = ls_context_base-ctx_ref.
          IF sy-subrc IS INITIAL.
            <ls_doc_objects>-objects =  ls_doc_objects-objects.
          ENDIF.

        ELSE.
          IF ls_doc_objects-objects IS NOT INITIAL.
            APPEND INITIAL LINE TO gt_doc_objects ASSIGNING <ls_doc_objects>.
            IF <ls_doc_objects> IS ASSIGNED.
              <ls_doc_objects>-ctx_ref = ls_context_base-ctx_ref.
              <ls_doc_objects>-objects = ls_doc_objects-objects.
            ENDIF.
          ENDIF.
        ENDIF.

*       Update the result list
        READ TABLE ct_tbox_resobj ASSIGNING <ls_tbox_resobj> WITH KEY obj_ids = ls_object-obj_ids.
        IF sy-subrc IS INITIAL.
          IF lv_rc IS INITIAL.
            <ls_tbox_resobj>-match = abap_true.
          ENDIF.
        ELSE.
          IF lv_rc IS INITIAL.
            READ TABLE ls_doc_objects-objects ASSIGNING <ls_object> WITH KEY obj_ids = ls_object-obj_ids.
            IF sy-subrc IS INITIAL.
              APPEND INITIAL LINE TO ct_tbox_resobj ASSIGNING <ls_tbox_resobj>.
              IF <ls_tbox_resobj> IS ASSIGNED.
                MOVE-CORRESPONDING <ls_object> TO <ls_tbox_resobj>.
                <ls_tbox_resobj>-docnum = cs_tbox_fields-docnum.
                <ls_tbox_resobj>-docpos = cs_tbox_fields-docpos.
                <ls_tbox_resobj>-match  = <ls_object>-status.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-inspobj.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_inspect
        EXPORTING
          ib_use_buffer   = abap_true
        IMPORTING
          ev_rc           = lv_rc
          et_return       = lt_return
        CHANGING
          cs_context_base = ls_context_base
          cs_context      = ls_context
          cs_object       = ls_object.
      "do not update the result list only if activity Inspect is not valid for this object
      IF  lv_rc <> /sttpec/cl_whs_constants=>gcs_error_codes-no_decode
      AND lv_rc <> /sttpec/cl_whs_constants=>gcs_error_codes-no_data.
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-insphier.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_inspect_hierarchy
        EXPORTING
          ib_use_buffer        = abap_true
          iv_vld_source        = lv_vld_parent  "flag to validate Parent object
          is_whsdat_ctx_ctrl   = cs_tbox_fields-ctx_ctrl
        IMPORTING
          ev_rc                = lv_rc
          et_return            = lt_return
        CHANGING
          cs_context_base      = ls_context_base
          cs_context_source    = ls_context_parent
          cs_context_target    = ls_context
          cs_object_source     = ls_object_parent
          cs_object_source_cur = ls_correct_parent
          cs_object_target     = ls_object.

*     Update Result list even in case validation has failed
      IF ls_object-obj_enc IS NOT INITIAL.
        MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
        ls_tbox_resobj-qty   = ls_object-quantity.
        ls_tbox_resobj-matnr = ls_context-matnr.
        ls_tbox_resobj-unit  = ls_context-unit.

        IF lv_rc IS INITIAL.
          ls_tbox_resobj-belong_to_prnt = abap_true.
        ELSE.
          ls_tbox_resobj-belong_to_prnt = abap_false.
        ENDIF.

*       Add current Top Level Parent of scanned child to the result list
        IF  ls_correct_parent IS NOT INITIAL
        AND ls_object-obj_ids <> ls_correct_parent-obj_ids.
          ls_tbox_resobj-obj_ids_par = ls_correct_parent-obj_ids.
        ENDIF.

        APPEND ls_tbox_resobj TO ct_tbox_resobj.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-sample.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_sample
        EXPORTING
          ib_use_buffer      = abap_true
          is_whsdat_ctx_ctrl = cs_tbox_fields-ctx_ctrl
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          ls_tbox_resobj-qty   = ls_object-quantity.
          ls_tbox_resobj-matnr = ls_context-matnr.
          ls_tbox_resobj-unit  = ls_context-unit.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-identcont.
      IF cs_tbox_fields-top_lvl IS INITIAL.
        ls_criteria-parent_mode = /sttpec/cl_whs_constants=>gcs_parent_mode-direct.
      ELSE.
        ls_criteria-parent_mode = /sttpec/cl_whs_constants=>gcs_parent_mode-top.
      ENDIF.

      ls_objcode-obj_code    = ls_object-obj_code.
      ls_objcode-decode_mode = 1.  "Decode Code w/o DB

*     Find Parent object
      CALL METHOD /sttpec/cl_att_functions=>get_parent_object
        EXPORTING
          is_objcode        = ls_objcode
          is_criteria       = ls_criteria
          io_messages       = lo_messages
        IMPORTING
          es_objdata        = ls_objdata
          es_objdata_parent = ls_objdata_parent
          ev_rc             = lv_rc.
      IF lv_rc <> 0 OR ls_objdata_parent-obj_ids IS INITIAL.
        IF ls_objdata-obj_ids IS INITIAL.
          "Decoding (character) of code >&1< failed
          MESSAGE e012(/sttpec/whs_msg) WITH ls_objcode-code_char." INTO ls_dummy_msg.
          RETURN.
***          lo_messages->set_message( ).
        ELSEIF ls_objdata_parent-obj_ids IS INITIAL.
          "Failed to determine the parent object
          MESSAGE e015(/sttpec/whs_msg)." INTO ls_dummy_msg.
          RETURN.
***          lo_messages->set_message( ).
        ENDIF.
***        PERFORM log_display USING lo_messages.
      ELSE.
*       Update Result list
        IF ls_objdata-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_objdata TO ls_tbox_resobj.
          ls_tbox_resobj-obj_ids_par     = ls_objdata_parent-obj_ids.
          ls_tbox_resobj-status_pack_par = ls_objdata_parent-status_pack.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.

      RETURN.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-count.

      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_count
        EXPORTING
          ib_use_buffer   = abap_true
        IMPORTING
          ev_rc           = lv_rc
          et_return       = lt_return
        CHANGING
          cs_context_base = ls_context_base
          cs_context      = ls_context
          cs_object       = ls_object
          ct_objdata      = gt_objdata_counting
          ct_objstruc_qty = gt_objqty_counting.

**       Update Result list
*        IF ls_object-obj_ids IS NOT INITIAL.
*          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
*          ls_tbox_resobj-qty   = ls_object-quantity.
*          ls_tbox_resobj-matnr = ls_context-matnr.
*          ls_tbox_resobj-unit  = ls_context-unit.
*          IF ls_object_parent-code_urn IS NOT INITIAL.
*            ls_tbox_resobj-scan_obj_parent = ls_object_parent-code_urn.
*          ELSE.
*            ls_tbox_resobj-scan_obj_parent = cs_tbox_fields-scan_obj_parent.
*          ENDIF.
*          APPEND ls_tbox_resobj TO ct_tbox_resobj.
*        ENDIF.

      IF cs_tbox_fields-unit IS INITIAL.
        cs_tbox_fields-unit = ls_context-unit.
      ENDIF.


      IF lt_return IS NOT INITIAL.
        lo_messages->convert_bapi_2_log(
          EXPORTING
            it_bapiret  = lt_return
          IMPORTING
            et_messages = lt_messages ).
        lo_messages->set_messages( it_messages = lt_messages ).
***        PERFORM log_display USING lo_messages.
      ENDIF.

      RETURN.
* ---------------------------------------------------------------------
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-docrelation.

*     Means to add transation reference to the scanned objects
      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_docrelation
        EXPORTING
          ib_use_buffer      = abap_true
          is_whsdat_ctx_ctrl = cs_tbox_fields-ctx_ctrl
          ib_skip_validation = cs_tbox_fields-skip_validation
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.
      IF lv_rc IS INITIAL.
*       Update Result list
        IF ls_object-obj_enc IS NOT INITIAL.
          MOVE-CORRESPONDING ls_object TO ls_tbox_resobj.
          APPEND ls_tbox_resobj TO ct_tbox_resobj.
        ENDIF.
      ENDIF.
* ---------------------------------------------------------------------
    WHEN OTHERS.
      RETURN.
  ENDCASE.
* ---------------------------------------------------------------------
  IF lv_rc IS NOT INITIAL.
    IF lt_return IS NOT INITIAL.
      lo_messages->convert_bapi_2_log(
        EXPORTING
          it_bapiret  = lt_return
        IMPORTING
          et_messages = lt_messages ).
      lo_messages->set_messages( it_messages = lt_messages ).
      READ TABLE lt_messages INTO ls_messages WITH KEY msgty = 'E'.
      IF sy-subrc = 0.
        MESSAGE ID ls_messages-msgid TYPE ls_messages-msgty NUMBER ls_messages-msgno
          WITH ls_messages-msgv1 ls_messages-msgv2.
        RETURN.
      ENDIF.
***     PERFORM log_display USING lo_messages.
    ELSE.
      "Activity is not valid for this scanned object.
      MESSAGE s010(/sttpec/whs_msg) DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
*   Update the selection screen fields
    IF  ls_object_parent               IS NOT INITIAL
    AND cs_tbox_fields-scan_obj_parent IS INITIAL.
      cs_tbox_fields-scan_obj_parent = ls_object_parent-code_urn.
    ENDIF.

    IF ls_context-qty_req IS NOT INITIAL.
      cs_tbox_fields-qty_rem = ls_context-qty_rem.
    ENDIF.

    "Scanned object has been successfully validated.
***    MESSAGE s004.
  ENDIF.

ENDFORM.                    " USER_COMMAND_VAL
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_VAL_PAR
*&---------------------------------------------------------------------*
*       Validate Parent Object
*----------------------------------------------------------------------*
*FORM user_command_val_par USING    iv_activetab   TYPE syucomm
*                          CHANGING cs_tbox_fields TYPE /sttpec/s_whs_test_tbox_flds.
*
*  DATA: lv_rc             TYPE sysubrc,
*        lv_existance      TYPE char1,
*        lt_return         TYPE bapiret2_t,                  "#EC NEEDED
*        ls_context_base   TYPE /sttpec/s_whsdat_bas,
*        ls_context_parent TYPE /sttpec/s_whsdat_ctx,
*        ls_context        TYPE /sttpec/s_whsdat_ctx,
*        ls_object         TYPE /sttpec/s_obj_data,          "#EC NEEDED
*        ls_object_parent  TYPE /sttpec/s_obj_data,
*        lo_messages       TYPE REF TO /sttpec/cl_messages,
*        lt_messages       TYPE /sttpec/cl_messages=>tt_bal_msg.
*
** ---------------------------------------------------------------------
*  IF  iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pick2p
*  AND iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packhu
*  AND iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packuom
*  AND iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-insphier
*  AND iv_activetab NE /sttpec/cl_whs_test_constants=>gcs_tbox_activity-lothry.
*    RETURN.
*  ENDIF.
** ---------------------------------------------------------------------
*  " assume current year document for test UI
*  IF cs_tbox_fields-docyear IS INITIAL AND cs_tbox_fields-docnum IS NOT INITIAL.
*    cs_tbox_fields-docyear = sy-datum(4).
*  ENDIF.
*
** fill in WHS context data
*  ls_context_base-ctx_loc   = cs_tbox_fields-ctx_loc.
*  ls_context_base-ctx_ref   = cs_tbox_fields-ctx_ref.
*  ls_context_base-custfield = iv_activetab.
*
*  PERFORM map_tbox_to_busfunc USING    iv_activetab
*                              CHANGING ls_context_base-busfunc.
*  IF ls_context_base-busfunc IS INITIAL.
*    "Business function could not be determined.
*    MESSAGE s003 DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
** ---------------------------------------------------------------------
** Create log
*  IF lo_messages IS NOT BOUND.
*    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
*                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
*  ENDIF.
** ---------------------------------------------------------------------
*  IF cs_tbox_fields-scan_obj_parent IS NOT INITIAL.
*    ls_object_parent-code_char = cs_tbox_fields-scan_obj_parent.
*    ls_object_parent-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
*
**   Create/Get buffer ID and check if already scanned
*    PERFORM create_check_buffer USING    ls_object_parent
*                                         abap_true
*                                CHANGING ls_context_base
*                                         lv_existance
*                                         lv_rc
*                                         lo_messages.
*    IF lv_rc <> 0.
*      PERFORM log_display USING lo_messages.
*      RETURN.
*    ENDIF.
*
*    IF lv_existance IS NOT INITIAL.
*      "This object has been already validated
*      MESSAGE s001 DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*  ENDIF.
*
*  ls_context_parent-sscc  = cs_tbox_fields-sscc_parent.
*  ls_context_parent-matnr = cs_tbox_fields-matnr.
*  ls_context_parent-unit  = cs_tbox_fields-unit_parent.
** ---------------------------------------------------------------------
** Validate Parent
*  CASE iv_activetab.
*    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pick2p.
*
**     SSCC must already exist
*      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_pick2pallet
*        EXPORTING
*          ib_use_buffer      = abap_true
*          ib_skip_validation = cs_tbox_fields-skip_validation
*        IMPORTING
*          ev_rc              = lv_rc
*          et_return          = lt_return
*        CHANGING
*          cs_context_base    = ls_context_base
*          cs_context_target  = ls_context_parent
*          cs_object_source   = ls_object          "empty
*          cs_object_target   = ls_object_parent.
*
*    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packhu
*      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packuom.
*
*      IF ls_object_parent IS INITIAL.
*        ls_context-matnr = cs_tbox_fields-matnr.
*      ENDIF.
*
*      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_pack
*        EXPORTING
*          ib_use_buffer      = abap_true
*          iv_vld_target      = abap_true
*          ib_skip_validation = cs_tbox_fields-skip_validation
*        IMPORTING
*          ev_rc              = lv_rc
*          et_return          = lt_return
*        CHANGING
*          cs_context_base    = ls_context_base
*          cs_context_source  = ls_context
*          cs_context_target  = ls_context_parent
*          cs_object_source   = ls_object "empty
*          cs_object_target   = ls_object_parent.
*
*    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-insphier.
*
**     Parent SSCC must exist
*      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_inspect_hierarchy
*        EXPORTING
*          ib_use_buffer     = abap_true
*        IMPORTING
*          ev_rc             = lv_rc
*          et_return         = lt_return
*        CHANGING
*          cs_context_base   = ls_context_base
*          cs_context_source = ls_context_parent
*          cs_context_target = ls_context
*          cs_object_source  = ls_object_parent
*          cs_object_target  = ls_object.
*
*    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-lothry.
*
*      CALL METHOD /sttpec/cl_whs_bf_activities=>whs_lot_hierarchy
*        EXPORTING
*          ib_use_buffer      = abap_true
*          iv_vld_target      = abap_true
*          is_whsdat_ctx_ctrl = cs_tbox_fields-ctx_ctrl
*          ib_skip_validation = cs_tbox_fields-skip_validation
*        IMPORTING
*          ev_rc              = lv_rc
*          et_return          = lt_return
*        CHANGING
*          cs_context_base    = ls_context_base
*          cs_context_source  = ls_context
*          cs_context_target  = ls_context_parent
*          cs_object_source   = ls_object "empty
*          cs_object_target   = ls_object_parent.
*
*    WHEN OTHERS.
*
*      RETURN.
*  ENDCASE.
*
*  IF lv_rc IS NOT INITIAL.
*    IF lt_return IS NOT INITIAL.
*      lo_messages->convert_bapi_2_log(
*        EXPORTING
*          it_bapiret  = lt_return    " Table with BAPI Return Information
*        IMPORTING
*          et_messages = lt_messages ).   " Application Log Messages
*      lo_messages->set_messages( it_messages = lt_messages ).
*      PERFORM log_display USING lo_messages.
*    ELSE.
*      "Activity is not valid for this scanned object.
*      MESSAGE s010(/sttpec/whs_msg) DISPLAY LIKE 'E'.
*    ENDIF.
*  ELSE.
*    IF cs_tbox_fields-scan_obj_parent IS INITIAL.
*      cs_tbox_fields-scan_obj_parent = ls_object_parent-code_urn.
*    ENDIF.
*
*    "Scanned object has been successfully validated
*    MESSAGE s004.
*  ENDIF.
*ENDFORM.                    " USER_COMMAND_VAL_PAR
*&---------------------------------------------------------------------*
*&      Form  DELETE_BUFFER_ENTRIES
*&---------------------------------------------------------------------*
*       Delete buffer entries for the Active Tab
*----------------------------------------------------------------------*
*FORM delete_buffer_entries USING    iv_activetab     TYPE sy-ucomm
*                           CHANGING cs_tbox_fields   TYPE /sttpec/s_whs_test_tbox_flds
*                                    ct_tbox_resobj   TYPE /sttpec/t_whs_test_tbox_resobj.
*
*  DATA: ls_context_base TYPE  /sttpec/s_whsdat_bas,
*        lv_log_display  TYPE xfeld,
*        lo_exc          TYPE REF TO /sttpec/cx_whs_exception,
*        lo_messages     TYPE REF TO /sttpec/cl_messages,
*        lt_messages     TYPE  /sttpec/cl_messages=>tt_bal_msg,
*        lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid.
*
*  FIELD-SYMBOLS: <ls_bufferid> TYPE guid.
*
** ---------------------------------------------------------------------
** Create log
*  IF lo_messages IS NOT BOUND.
*    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
*                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
*  ENDIF.
** ---------------------------------------------------------------------
** fill in WHS context data
*  PERFORM map_tbox_to_busfunc USING    iv_activetab
*                              CHANGING ls_context_base-busfunc.
*  IF ls_context_base-busfunc IS INITIAL.
*    "Business function could not be determined.
*    MESSAGE s003 DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*
*  ls_context_base-custfield = iv_activetab.
** ---------------------------------------------------------------------
** Find all buffer IDs for the business function&tab
*  TRY.
*      CALL METHOD /sttpec/cl_whs_bf_buffer=>read
*        EXPORTING
*          iv_busfunc   = ls_context_base-busfunc
*          iv_custfield = ls_context_base-custfield
*        IMPORTING
*          et_bufferid  = lt_bufferid
*        CHANGING
*          co_messages  = lo_messages.
*    CATCH /sttpec/cx_whs_exception INTO lo_exc.
*      IF lo_exc->messages IS BOUND.
*        CLEAR lt_messages[].
*        lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
*        lo_messages->set_messages( it_messages = lt_messages ).
*        PERFORM log_display USING lo_messages.
*      ENDIF.
*      RETURN.
*  ENDTRY.
*
** Clear the complete buffer
*  LOOP AT lt_bufferid ASSIGNING <ls_bufferid>.
*    TRY.
*        CALL METHOD /sttpec/cl_whs_bf_buffer=>clear
*          EXPORTING
*            iv_bufferid = <ls_bufferid>
*          CHANGING
*            co_messages = lo_messages.
*      CATCH /sttpec/cx_whs_exception INTO lo_exc.
*        IF lo_exc->messages IS BOUND.
*          CLEAR lt_messages[].
*          lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
*          lo_messages->set_messages( it_messages = lt_messages ).
*          lv_log_display = abap_true.
*        ENDIF.
*        CONTINUE.
*    ENDTRY.
*  ENDLOOP.
*
*  IF lv_log_display = abap_true.
*    PERFORM log_display USING lo_messages.
*  ENDIF.
*
*  CLEAR: cs_tbox_fields,ct_tbox_resobj.
*
*  DELETE gt_doc_objects WHERE busfunc = ls_context_base-busfunc.
*
*ENDFORM.                    " DELETE_BUFFER_ENTRIES
*&---------------------------------------------------------------------*
*&      Form  LOG_DISPLAY
*&---------------------------------------------------------------------*
*      Save and display log messages
*----------------------------------------------------------------------*
FORM log_display  USING    io_messages TYPE REF TO /sttpec/cl_messages.
  DATA: lt_messages TYPE /sttpec/cl_messages=>tt_bal_msg.

  io_messages->get_messages( IMPORTING et_messages_total = lt_messages ).
  IF lt_messages IS INITIAL.
    RETURN.
  ENDIF.

  /sttpec/cl_message_ctrl=>save(  EXPORTING iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
                                            iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).

* display log
  io_messages->log_display_popup(  EXPORTING iv_title = TEXT-001 ).

ENDFORM.                    " LOG_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  UPDATE_REM_QUANTITY
*&---------------------------------------------------------------------*
*       Update Remaining Quantity from Buffer
*----------------------------------------------------------------------*
*FORM update_rem_quantity  CHANGING cs_tbox_fields TYPE /sttpec/s_whs_test_tbox_flds
*                                   ct_res_list    TYPE /sttpec/t_whs_test_tbox_resobj.
*
*  DATA: lv_total_quan   TYPE /sttpec/e_quantity.
*
*  FIELD-SYMBOLS:
*    <ls_res_list> TYPE /sttpec/s_whs_test_tbox_resobj.
*
*  IF cs_tbox_fields-unit  IS INITIAL
*  OR cs_tbox_fields-matnr IS INITIAL.
*    RETURN.
*  ENDIF.
** ---------------------------------------------------------------------
*  SORT ct_res_list BY matnr unit.
*  READ TABLE ct_res_list TRANSPORTING NO FIELDS WITH KEY matnr = cs_tbox_fields-matnr
*                                                         unit  = cs_tbox_fields-unit.
*  IF sy-subrc = 0.
*    LOOP AT ct_res_list ASSIGNING <ls_res_list> FROM sy-tabix.
*      IF <ls_res_list>-matnr <> cs_tbox_fields-matnr
*      OR <ls_res_list>-unit  <> cs_tbox_fields-unit.
*        EXIT.
*      ENDIF.
*
*      ADD <ls_res_list>-qty TO lv_total_quan.
*
*    ENDLOOP.
*  ENDIF.
*
*  IF cs_tbox_fields-qty_req LT lv_total_quan.
*    MESSAGE w002 DISPLAY LIKE 'I' .
*    cs_tbox_fields-qty_req = lv_total_quan.
*  ENDIF.
**
*  cs_tbox_fields-qty_rem  = cs_tbox_fields-qty_req - lv_total_quan.
*  cs_tbox_fields-unit_rem = cs_tbox_fields-unit.
*ENDFORM.                    " UPDATE_REM_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  MAP_TBOX_TO_DOCTPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM map_tbox_to_doctpe  USING    iv_activetab TYPE syucomm
                                  iv_docnum    TYPE char12
                         CHANGING cv_doctpe    TYPE /sttpec/e_doctpe.

  IF iv_docnum IS INITIAL.
    RETURN.
  ENDIF.

  CASE iv_activetab.
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-grhu.
      cv_doctpe = /sttpec/cl_whs_constants=>gcs_doctpe-inb_dlv.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickitem
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickhu
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pick2p
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packhu
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packuom
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-load
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-ship.
      cv_doctpe = /sttpec/cl_whs_constants=>gcs_doctpe-out_dlv.

    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDFORM.                    " MAP_TBOX_TO_DOCTPE
*&---------------------------------------------------------------------*
*&      Form  CREATE_BUFFER_AND_CHECK
*&---------------------------------------------------------------------*
*       Create/Get buffer ID and check object existance
*----------------------------------------------------------------------*
FORM create_check_buffer  USING    is_object        TYPE /sttpec/s_obj_data
                                   iv_parent        TYPE flag
                          CHANGING cs_context_base  TYPE /sttpec/s_whsdat_bas
                                   cv_existance     TYPE char1
                                   cv_rc            TYPE sysubrc
                                   co_messages      TYPE REF TO /sttpec/cl_messages.

  DATA:
    lo_exc      TYPE REF TO /sttpec/cx_whs_exception,
    lt_messages TYPE  /sttpec/cl_messages=>tt_bal_msg.

  CLEAR: cv_rc, cv_existance.

  IF cs_context_base-bufferid IS INITIAL.
    TRY.
        CALL METHOD /sttpec/cl_whs_bf_buffer=>create
          EXPORTING
            is_base_data_ref = cs_context_base-ctx_ref
            is_base_data_loc = cs_context_base-ctx_loc
            iv_custfield     = cs_context_base-custfield
          IMPORTING
            ev_bufferid      = cs_context_base-bufferid
          CHANGING
            co_messages      = co_messages.
      CATCH /sttpec/cx_whs_exception INTO lo_exc.
        IF lo_exc->messages IS BOUND.
          CLEAR lt_messages[].
          lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
          co_messages->set_messages( it_messages = lt_messages ).
        ENDIF.
        cv_rc = /sttpec/cl_constants=>gcs_rc-fail.
        RETURN.
    ENDTRY.
  ENDIF.

  IF is_object-obj_code IS NOT INITIAL.
    TRY.
        /sttpec/cl_whs_bf_buffer=>check_existence_object(
          EXPORTING
            iv_bufferid  = cs_context_base-bufferid
            is_object    = is_object
            iv_parent    = iv_parent
          IMPORTING
            eb_existance  = cv_existance
          CHANGING
            co_messages  = co_messages ).
      CATCH /sttpec/cx_whs_exception INTO lo_exc.
        IF lo_exc->messages IS BOUND.
          CLEAR lt_messages[].
          lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
          co_messages->set_messages( it_messages = lt_messages ).
        ENDIF.
        cv_rc = /sttpec/cl_constants=>gcs_rc-fail.
        RETURN.
    ENDTRY.
  ENDIF.
ENDFORM.                    " CREATE_BUFFER_AND_CHECK
*&---------------------------------------------------------------------*
*&      Form  MAP_TBOX_TO_BUSFUNC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM map_tbox_to_busfunc  USING    iv_activetab TYPE syucomm
                          CHANGING cv_busfunc   TYPE /sttpec/e_busfunc.
  CASE iv_activetab.
    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-grhu.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-receive_container.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-putaway.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-putaway.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickitem
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pickhu.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-pick.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-pick2p.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-pick2pallet.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-create.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-create.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-decommiss.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-decommission.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packhu
      OR /sttpec/cl_whs_test_constants=>gcs_tbox_activity-packuom.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-pack.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-unpack.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-unpack.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-lothry.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-lot_hry.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-identcont.
*     validation is not needed just data determination

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-load.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-load_container.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-ship.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-ship_container.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-inspobj.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-inspect.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-insphier.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-inspect_hry.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-sample.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-sample.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-count.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-count.

    WHEN /sttpec/cl_whs_test_constants=>gcs_tbox_activity-docrelation.
      cv_busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-docrelation.

    WHEN OTHERS.
      RETURN.
  ENDCASE.
ENDFORM.                    " MAP_TBOX_TO_BUSFUNC
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_RETRV
*&---------------------------------------------------------------------*
*       Retrieve Contents
*----------------------------------------------------------------------*
*FORM user_command_retrv  USING    iv_activetab     TYPE syucomm
*                         CHANGING cs_tbox_fields   TYPE /sttpec/s_whs_test_tbox_flds
*                                  ct_tbox_resobj   TYPE /sttpec/t_whs_test_tbox_resobj
*                                  ct_doc_objects   TYPE /sttpec/t_whs_test_tbox_docobj.
*
*  DATA: lv_rc          TYPE sysubrc,
*        lt_objdata     TYPE /sttpec/t_att_obj_response,
*        lt_objects     TYPE /sttpec/t_att_obj_chk,
*        ls_doccode     TYPE /sttpec/s_att_obj_request,
*        ls_doctype     TYPE /sttpec/s_att_obj_request,
*        ls_biztrn      TYPE /sttpec/s_proc_trnlist,
*        lv_gln         TYPE /sttpec/e_gs1_gln,
*        lv_gln_ext     TYPE /sttpec/e_gs1_gln_ext ##needed,
*        lo_exc         TYPE REF TO /sttpec/cx_whs_exception,
*        lv_obj_num     TYPE i,
*        ls_doc_objects TYPE /sttpec/s_whs_test_tbox_docobj,
*        ls_object      TYPE /sttpec/s_att_obj_chk,
*        lo_messages    TYPE REF TO /sttpec/cl_messages,
*        ls_dummy_msg   TYPE symsg.                          "#EC NEEDED
*
*  FIELD-SYMBOLS:
*    <ls_objdata>     TYPE /sttpec/s_att_obj_response,
*    <ls_object>      TYPE /sttpec/s_att_obj_chk,
*    <ls_resobj>      TYPE /sttpec/s_whs_test_tbox_resobj,
*    <ls_doc_objects> TYPE /sttpec/s_whs_test_tbox_docobj.
*
** ---------------------------------------------------------------------
** Create log
*  IF lo_messages IS NOT BOUND.
*    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
*                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
*  ENDIF.
** ---------------------------------------------------------------------
** determine the document type
*  PERFORM map_tbox_to_doctpe USING    iv_activetab
*                                      cs_tbox_fields-docnum
*                             CHANGING cs_tbox_fields-doctpe.
*
*  PERFORM map_tbox_to_busfunc USING    iv_activetab
*                              CHANGING cs_tbox_fields-busfunc.
*
**  get GLN data
*  /sttpec/cl_whs_md_access=>get_loc_sgln(
*    EXPORTING
*      is_whsdat_loc = cs_tbox_fields-ctx_loc
*    IMPORTING
*      ev_gln        = lv_gln
*      ev_gln_ext    = lv_gln_ext
*      ).
*
*  IF cs_tbox_fields-ctx_ref-docyear IS INITIAL.
*    cs_tbox_fields-ctx_ref-docyear = sy-datum(4).
*  ENDIF.
*
*  TRY.
*      CALL METHOD /sttpec/cl_whs_encoding=>encode_trn_data
*        EXPORTING
*          is_context_loc = cs_tbox_fields-ctx_loc
*          is_context_ref = cs_tbox_fields-ctx_ref
*          iv_gln         = lv_gln
*          iv_gln_ext     = lv_gln_ext
*        IMPORTING
*          es_biztrn      = ls_biztrn.
*    CATCH /sttpec/cx_whs_exception INTO lo_exc.
*      lo_messages->set_message( EXPORTING is_message = lo_exc->message ).
*      PERFORM log_display USING lo_messages.
*      RETURN.
*  ENDTRY.
*
*  ls_doccode-code_char = ls_biztrn-biztransactionid.
*  ls_doccode-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
*
** business transaction type
*  ls_doctype-code_char = ls_biztrn-biztransactiontype.
*  ls_doctype-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
*
** find the list of the natively assigned objects to the specified business transaction
*  CALL METHOD /sttpec/cl_att_functions=>get_document_contents
*    EXPORTING
*      is_doccode       = ls_doccode
*      is_doctype       = ls_doctype
*      iv_unpacked_only = abap_true
*      io_messages      = lo_messages
*    IMPORTING
*      et_objdata       = lt_objdata
*      ev_rc            = lv_rc.
*  IF lv_rc <> 0 OR lt_objdata IS INITIAL.
*    "No natively assigned objects found for the document &1.
*    MESSAGE e009 WITH cs_tbox_fields-ctx_ref-docnum INTO ls_dummy_msg.
*    lo_messages->set_message( ).
*    PERFORM log_display USING lo_messages.
*  ELSE.
*
*    READ TABLE ct_doc_objects INTO ls_doc_objects WITH KEY ctx_ref = cs_tbox_fields-ctx_ref.
*    IF sy-subrc <> 0.
*      CLEAR ls_doc_objects.
*    ENDIF.
*
*    DELETE ct_tbox_resobj WHERE docnum = cs_tbox_fields-docnum
*                            AND docpos = cs_tbox_fields-docpos.
**   Update Result list
*    LOOP AT lt_objdata ASSIGNING <ls_objdata>.
*      APPEND INITIAL LINE TO lt_objects ASSIGNING <ls_object>.
*      IF <ls_object> IS ASSIGNED.
*        <ls_object>-obj_ids = <ls_objdata>-obj_ids.
*        <ls_object>-obj_dat = <ls_objdata>-obj_dat.
*        <ls_object>-obj_enc = <ls_objdata>-obj_enc.
*      ENDIF.
*
*      APPEND INITIAL LINE TO ct_tbox_resobj ASSIGNING <ls_resobj>.
*      IF <ls_resobj> IS ASSIGNED.
*        MOVE-CORRESPONDING <ls_objdata> TO <ls_resobj>.
*        <ls_resobj>-docnum = cs_tbox_fields-docnum.
*        <ls_resobj>-docpos = cs_tbox_fields-docpos.
*        READ TABLE ls_doc_objects-objects INTO ls_object WITH KEY obj_ids = <ls_objdata>-obj_ids.
*        IF sy-subrc IS INITIAL.
*          <ls_resobj>-match = ls_object-status.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    READ TABLE ct_doc_objects ASSIGNING <ls_doc_objects> WITH KEY ctx_ref = cs_tbox_fields-ctx_ref.
*    IF sy-subrc = 0.
*      <ls_doc_objects>-objects = lt_objects.
*    ELSE.
*      APPEND INITIAL LINE TO ct_doc_objects ASSIGNING <ls_doc_objects>.
*      IF <ls_doc_objects> IS ASSIGNED.
*        <ls_doc_objects>-ctx_ref = cs_tbox_fields-ctx_ref.
*        <ls_doc_objects>-objects = lt_objects.
*      ENDIF.
*    ENDIF.
*
*    "&1 natively assigned objects found for the document &2.
*    DESCRIBE TABLE lt_objects LINES lv_obj_num.
*    MESSAGE s011 WITH lv_obj_num cs_tbox_fields-docnum.
*  ENDIF.
*ENDFORM.                    " USER_COMMAND_RETRV
*&---------------------------------------------------------------------*
*&      Form  GET_CONTEXT_SRCDEST
*&---------------------------------------------------------------------*
*       Get Source/Destination GLNs
*----------------------------------------------------------------------*
FORM get_context_srcdest  USING    is_ctx_gln    TYPE /sttpec/s_whsdat_trn_loc
                          CHANGING ct_context_sd TYPE /sttpec/t_whsdat_srcdest.

  DATA: ls_context_sd TYPE /sttpec/s_whsdat_srcdest.

* Initilization
  CLEAR ct_context_sd.

  IF is_ctx_gln IS INITIAL.
    RETURN.
  ENDIF.

* Sold-from GLN
  IF is_ctx_gln-sold_fr_loc IS NOT INITIAL.
    ls_context_sd-sd_cat      = /sttpec/cl_whs_constants=>gcs_sd_category-source.
    ls_context_sd-sd_typecode = /sttpec/cl_whs_constants=>gcs_sd_typecode-owning_party.
    ls_context_sd-sd_gln      = is_ctx_gln-sold_fr_loc.
    APPEND ls_context_sd TO ct_context_sd.
  ENDIF.

* Sold-to GLN
  IF is_ctx_gln-sold_to_loc IS NOT INITIAL.
    ls_context_sd-sd_cat      = /sttpec/cl_whs_constants=>gcs_sd_category-destination.
    ls_context_sd-sd_typecode = /sttpec/cl_whs_constants=>gcs_sd_typecode-owning_party.
    ls_context_sd-sd_gln      = is_ctx_gln-sold_to_loc.
    APPEND ls_context_sd TO ct_context_sd.
  ENDIF.

* Ship-from GLN
  IF is_ctx_gln-ship_fr_loc IS NOT INITIAL.
    ls_context_sd-sd_cat      = /sttpec/cl_whs_constants=>gcs_sd_category-source.
    ls_context_sd-sd_typecode = /sttpec/cl_whs_constants=>gcs_sd_typecode-location.
    ls_context_sd-sd_gln      = is_ctx_gln-ship_fr_loc.
    APPEND ls_context_sd TO ct_context_sd.
  ENDIF.

* Ship-to GLN
  IF is_ctx_gln-ship_to_loc IS NOT INITIAL.
    ls_context_sd-sd_cat      = /sttpec/cl_whs_constants=>gcs_sd_category-destination.
    ls_context_sd-sd_typecode = /sttpec/cl_whs_constants=>gcs_sd_typecode-location.
    ls_context_sd-sd_gln      = is_ctx_gln-ship_to_loc.
    APPEND ls_context_sd TO ct_context_sd.
  ENDIF.
ENDFORM.                    " GET_CONTEXT_SRCDEST
*&---------------------------------------------------------------------*
*&      Form  CREATE_AUTHENTICATION_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_authentication_request .
  DATA: lt_return                TYPE bapiret2_t ##needed,
        lv_object_code1          TYPE /sttpec/e_objcode,
        lv_object_code2          TYPE /sttpec/e_objcode,
        lo_exc                   TYPE REF TO /sttpec/cx_int_exception,
        ls_auth_request_res_list LIKE LINE OF gt_auth_request_res_list_full.

  lv_object_code1 = gs_tbox_auth_request-scan_obj.
  lv_object_code2 = gs_tbox_auth_request-scan_obj_parent.

  TRY.
      CALL METHOD /sttpec/cl_whs_test_auth_req=>call_authentication
        EXPORTING
          iv_system_name     = gs_tbox_auth_request-att_system_name
          iv_object_code1    = lv_object_code1
          iv_object_code2    = lv_object_code2
          iv_longitude       = gs_tbox_auth_request-longitude
          iv_latitude        = gs_tbox_auth_request-latitude
          iv_langu           = gs_tbox_auth_request-langu
        IMPORTING
          et_auth_req_return = gt_auth_request_res_list_full
          et_return          = lt_return.
    CATCH /sttpec/cx_int_exception INTO lo_exc.
      MESSAGE ID lo_exc->message-msgid TYPE 'S' NUMBER lo_exc->message-msgno
        DISPLAY LIKE lo_exc->message-msgty
        WITH lo_exc->message-msgv1 lo_exc->message-msgv2 lo_exc->message-msgv3 lo_exc->message-msgv4.
      RETURN.
  ENDTRY.
*
  CLEAR gt_auth_request_res_list.
  gt_auth_request_res_list[] = gt_auth_request_res_list_full[].
  IF gv_disp_objcode1 = abap_true.
    DELETE gt_auth_request_res_list WHERE object_code <> gs_tbox_auth_request-scan_obj.

  ELSE.
    DELETE gt_auth_request_res_list WHERE object_code <> gs_tbox_auth_request-scan_obj_parent.

  ENDIF.

  " Fill result of authentication request.
  READ TABLE gt_auth_request_res_list_full INTO ls_auth_request_res_list
                                           WITH KEY object_code = gs_tbox_auth_request-scan_obj.
  IF sy-subrc = 0.
    gs_tbox_auth_request-scan_obj_1_result = ls_auth_request_res_list-auth_result.
  ELSE.
    CLEAR gs_tbox_auth_request-scan_obj_1_result.
  ENDIF.

  READ TABLE gt_auth_request_res_list_full INTO ls_auth_request_res_list
                                           WITH KEY object_code = gs_tbox_auth_request-scan_obj_parent.
  IF sy-subrc = 0.
    gs_tbox_auth_request-scan_obj_2_result = ls_auth_request_res_list-auth_result.
  ELSE.
    CLEAR gs_tbox_auth_request-scan_obj_2_result.
  ENDIF.
ENDFORM.                    " CREATE_AUTHENTICATION_REQUEST
*&---------------------------------------------------------------------*
*&      Form  REQUEST_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_MAIN_TAB_PRESSED_TAB  text
*      <--P_GS_TBOX_REQUEST_CONTROL  text
*----------------------------------------------------------------------*
*FORM REQUEST_CONTROL  USING    iv_activetab     TYPE syucomm ##NEEDED
*                      CHANGING cs_tbox_fields   TYPE /sttpec/s_whs_test_tbox_flds.
*
*  data: lo_messages       TYPE REF TO /sttpec/cl_messages.
*  data: ls_context_base   TYPE /sttpec/s_whsdat_bas.
*  data: lv_rc             TYPE sysubrc.
*  data: lt_return         TYPE bapiret2_t.
*  data: ls_context        TYPE /sttpec/s_whsdat_ctx.
*  data: ls_object         TYPE /sttpec/s_obj_data.
*  data: lt_messages       TYPE /sttpec/cl_messages=>tt_bal_msg.
*
** ---------------------------------------------------------------------
** Create log
*  IF lo_messages IS NOT BOUND.
*    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
*                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
*  ENDIF.
** ---------------------------------------------------------------------
*  " scanned object
*  CLEAR ls_object.
*  IF cs_tbox_fields-scan_obj IS NOT INITIAL.
*    ls_object-code_char = cs_tbox_fields-scan_obj.
*    ls_object-code_type = /sttpec/cl_whs_constants=>gcs_code_type-character.
*  ENDIF.
*
*  CALL METHOD /sttpec/cl_whs_bf_activities=>whs_request_control
*    IMPORTING
*      ev_rc     = lv_rc
*      et_return = lt_return
*    CHANGING
*      cs_object = ls_object.
** ---------------------------------------------------------------------
*
*  IF lt_return IS NOT INITIAL.
*    lo_messages->convert_bapi_2_log(
*      EXPORTING
*        it_bapiret  = lt_return
*      IMPORTING
*        et_messages = lt_messages ).
*    lo_messages->set_messages( it_messages = lt_messages ).
*    PERFORM log_display USING lo_messages.
*  ELSE.
*    IF lv_rc IS NOT INITIAL.
*      " Failed to gain control for scanned object.
*      MESSAGE s032(/sttpec/whs_msg) DISPLAY LIKE 'E'.
*    ELSE.
*      " Request control for scanned object successful.
*      MESSAGE s033(/sttpec/whs_msg).
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " REQUEST_CONTROL
*&---------------------------------------------------------------------*
*&      Form  SEND_EVENT
*&---------------------------------------------------------------------*
*       Command on pressing 'Send Event' button
*----------------------------------------------------------------------*
FORM send_event  USING    iv_activetab     TYPE syucomm
                 CHANGING cs_tbox_fields   TYPE /sttpec/s_whs_test_tbox_flds
                          ct_tbox_resobj   TYPE /sttpec/t_whs_test_tbox_resobj.


  DATA: ls_context_base TYPE  /sttpec/s_whsdat_bas,
        lo_exc          TYPE REF TO /sttpec/cx_whs_exception,
        lv_rc           TYPE sysubrc,
        lv_rc_all       TYPE sysubrc,
        lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
        lt_messages     TYPE /sttpec/cl_messages=>tt_bal_msg,
        ls_messages     TYPE /sttpec/cl_messages=>ts_bal_msg,
        lo_messages     TYPE REF TO /sttpec/cl_messages.

  FIELD-SYMBOLS: <ls_bufferid> TYPE guid.

* Create log
  IF lo_messages IS NOT BOUND.
    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
  ENDIF.
* ---------------------------------------------------------------------
* fill in WHS context data
  PERFORM map_tbox_to_busfunc USING    iv_activetab
                              CHANGING ls_context_base-busfunc.
  IF ls_context_base-busfunc IS INITIAL.
*    "Business function could not be determined.
*    MESSAGE s003 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  ls_context_base-custfield = iv_activetab.
* ---------------------------------------------------------------------
* Find all buffer IDs for the business function&tab
  TRY.
      CALL METHOD /sttpec/cl_whs_bf_buffer=>read
        EXPORTING
          iv_busfunc   = ls_context_base-busfunc
          iv_custfield = ls_context_base-custfield
        IMPORTING
          et_bufferid  = lt_bufferid
        CHANGING
          co_messages  = lo_messages.
    CATCH /sttpec/cx_whs_exception INTO lo_exc.
      IF lo_exc->messages IS BOUND.
        CLEAR lt_messages[].
        lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
        lo_messages->set_messages( it_messages = lt_messages ).
        PERFORM log_display USING lo_messages.
      ENDIF.
      RETURN.
  ENDTRY.

  LOOP AT lt_bufferid ASSIGNING <ls_bufferid>.

    CALL METHOD /sttpec/cl_whs_bf_events=>compose_events_and_send
      EXPORTING
        iv_bufferid = <ls_bufferid>
      IMPORTING
        ev_rc       = lv_rc
      CHANGING
        co_messages = lo_messages.

    lv_rc_all = lv_rc_all + lv_rc.

*   Clear the buffer
    TRY.
        CALL METHOD /sttpec/cl_whs_bf_buffer=>clear
          EXPORTING
            iv_bufferid = <ls_bufferid>
          CHANGING
            co_messages = lo_messages.
      CATCH /sttpec/cx_whs_exception INTO lo_exc.
        IF lo_exc->messages IS BOUND.
          CLEAR lt_messages[].
          lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
          lo_messages->set_messages( it_messages = lt_messages ).
        ENDIF.
        CONTINUE.
    ENDTRY.
  ENDLOOP.

***  PERFORM log_display USING lo_messages.
  READ TABLE lt_messages INTO ls_messages WITH KEY msgty = 'E'.
  IF sy-subrc = 0.
    MESSAGE ID ls_messages-msgid TYPE ls_messages-msgty NUMBER ls_messages-msgno
      WITH ls_messages-msgv1 ls_messages-msgv2.
    RETURN.
  ENDIF.

  " always clear result list (as buffer is cleared too)
  CLEAR ct_tbox_resobj.
  " clear entry fields oly if no failure (allows restart)
  IF lv_rc_all IS INITIAL.
    CLEAR cs_tbox_fields.
  ENDIF.

  DELETE gt_doc_objects WHERE busfunc = ls_context_base-busfunc.

ENDFORM.                    " SEND_EVENT
*&---------------------------------------------------------------------*
*& Form gf_serialized
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GV_MATNR
*&      <-- GV_SERIAL
*&---------------------------------------------------------------------*
FORM gf_serialized  USING    p_matnr TYPE matnr
                    CHANGING p_serial TYPE /sttpec/e_syncactive.

  SELECT SINGLE /sttpec/syncact
    INTO @p_serial
    FROM mara
    WHERE matnr = @p_matnr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form send_pack
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZT_CHILD_SN
*&      --> CS_PACK
*&---------------------------------------------------------------------*
FORM  send_pack  USING    p_child_sn TYPE zrf_tt_child_sn.

  DATA: iv_activetab TYPE sy-ucomm.
  CLEAR: gs_tbox_packhu.
  REFRESH: gt_packhu_res_list.
  iv_activetab = 'PACKHU'.
  LOOP AT p_child_sn ASSIGNING FIELD-SYMBOL(<fs_child_sn1>).

    gs_tbox_packhu-scan_obj = <fs_child_sn1>-rfsn.
    gs_tbox_packhu-scan_obj_parent = <fs_child_sn1>-rfhu_parent1.
    gs_tbox_packhu-logsys = 'H01CLNT100'.
    gs_tbox_packhu-plant = '2000'.
    gs_tbox_packhu-sscc_parent = <fs_child_sn1>-rfhu_parent1+4(18).

    PERFORM user_command_val USING    iv_activetab
                             CHANGING gs_tbox_packhu
                                      gt_packhu_res_list.
  ENDLOOP.
  PERFORM send_event USING    iv_activetab
                     CHANGING gs_tbox_packhu
                              gt_packhu_res_list.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form create_sscc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_sscc CHANGING lv_parent_hu1 TYPE /scwm/de_rf_rfhu_long.

  TYPES: BEGIN OF lty_sscc,
           gcp       TYPE char12,
           ext_digit TYPE char1,
         END OF lty_sscc,

         BEGIN OF lty_encoded,
           obj_code TYPE /sttpec/e_objcode,
         END OF lty_encoded.

  DATA: lv_hu      TYPE /scwm/de_rf_huident,
        lv_size    TYPE /sttpec/e_serno_req,
        lv_encode  TYPE /sttpec/e_objcode,
        ls_sscc    TYPE lty_sscc,
        ls_range   TYPE gty_range,
        lt_encoded TYPE STANDARD TABLE OF lty_encoded,
        lt_return  TYPE bapiret2_t.

* Provide some default values to Exporting parameters of FM
  lv_size = 1.
  ls_sscc-gcp = gc_gcp.
  ls_sscc-ext_digit = gc_ext_digit.

* Call FM in ATTP to get the Serial Number
  CALL FUNCTION '/STTP/INT_SNR_REQ_SYNC' DESTINATION 'ATDCLNT100_ABAP'
    EXPORTING
      iv_system         = 'SAP_H01CLNT100'
      is_sscc           = ls_sscc
      iv_size           = lv_size
      iv_list_range     = 'R'
      iv_readyly_encode = '1'
      ib_only_exact     = abap_true
      iv_caller         = 'E'
      ib_commit         = 'X'
    IMPORTING
      es_range          = ls_range
    TABLES
      et_encoded        = lt_encoded
      et_return         = lt_return.
* Check for any errors while getting Serial Number
  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc IS NOT INITIAL.
    READ TABLE lt_encoded INTO lv_encode INDEX 1.
    lv_hu = lv_encode+4(20).
    lv_parent_hu1 = lv_encode.
    PERFORM post_event_commissioning USING ls_range lv_encode lv_hu.
  ELSE.
    MESSAGE e002(zmsg_attp_ewm).
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_event_commissioning
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RANGE
*&      --> LV_ENCODE
*&      --> LV_HU
*&---------------------------------------------------------------------*
FORM post_event_commissioning  USING pi_range  TYPE gty_range
                                    pi_encode TYPE /sttpec/e_objcode
                                    pi_hu     TYPE /scwm/de_rf_huident.

  DATA:lv_rfc_logsys   TYPE logsys,
       lv_plant        TYPE werks_d,
       lv_rc           TYPE sysubrc,
       lv_rc_all       TYPE sysubrc,
       lv_urn          TYPE string,
       lo_messages     TYPE REF TO /sttpec/cl_messages,
       ls_context_base TYPE /sttpec/s_whsdat_bas,
       lt_objects_src  TYPE /sttpec/t_obj_data,
       lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
       lt_messages     TYPE /sttpec/cl_messages=>tt_bal_msg.

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

* Populate Document type
  ls_context_base-busfunc    = /sttpec/cl_whs_constants=>gcs_busfunc-create. "Business Function (06)
  ls_context_base-custfield  = gc_custfield.
  ls_context_base-logsys     = lv_rfc_logsys.
  ls_context_base-plant      = gc_plant.        "Plant

* Get Buffer ID
  TRY.
      CALL METHOD /sttpec/cl_whs_bf_buffer=>create
        EXPORTING
          is_base_data_ref = ls_context_base-ctx_ref
          is_base_data_loc = ls_context_base-ctx_loc
          iv_custfield     = ls_context_base-custfield
        IMPORTING
          ev_bufferid      = ls_context_base-bufferid
        CHANGING
          co_messages      = lo_messages.
    CATCH /sttpec/cx_whs_exception INTO DATA(lo_exc).
      IF lo_exc->messages IS BOUND.
*        CLEAR lt_messages[].
*        lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
*        lo_messages->set_messages( it_messages = lt_messages ).
      ENDIF.
  ENDTRY.

* Check existance of Buffer ID
  CHECK  ls_context_base-bufferid IS NOT INITIAL.
  TRY.
      CALL METHOD /sttpec/cl_whs_bf_buffer=>get
        EXPORTING
          iv_bufferid      = ls_context_base-bufferid
        IMPORTING
          es_base_data_loc = DATA(ls_context_loc)
          es_context_src   = DATA(ls_context_src)
          es_ctrl_data     = DATA(ls_ctrl_data)
          et_objects_src   = lt_objects_src
          et_objects_tgt   = DATA(lt_obj_tgt)
        CHANGING
          co_messages      = lo_messages.
    CATCH /sttpec/cx_whs_exception INTO lo_exc.
  ENDTRY.

* Put Object Data in buffer for commisioning
  IF lt_objects_src IS INITIAL.
    APPEND INITIAL LINE TO lt_objects_src ASSIGNING FIELD-SYMBOL(<lfs_obj_src>).
    <lfs_obj_src>-code_type = gc_code_type.
    <lfs_obj_src>-code_char = pi_encode.
    <lfs_obj_src>-owner     = gc_gcp.
    <lfs_obj_src>-serial    = pi_range-number_from.
    <lfs_obj_src>-enc_type  = gc_enc_type.
    <lfs_obj_src>-sscc      = pi_hu.
    CONCATENATE 'urn:epc:id:sscc:031021.' pi_range-number_from INTO lv_urn.
    <lfs_obj_src>-code_urn  = lv_urn.
  ENDIF.
* Put SSCC related data required for comissioning
  IF ls_context_src IS INITIAL.
    ls_context_src-sscc =  pi_hu.
    ls_context_src-gcp  = gc_gcp.
  ENDIF.
* Put Location related data
  IF ls_context_loc IS INITIAL.
    ls_context_loc-plant = gc_plant.
  ENDIF.

  IF ls_ctrl_data-compose_obj_event IS INITIAL.
    ls_ctrl_data-compose_obj_event = 'X'.
  ENDIF.

* Set Buffer ID with comissioning data
  TRY.
      CALL METHOD /sttpec/cl_whs_bf_buffer=>set
        EXPORTING
          iv_bufferid      = ls_context_base-bufferid
          is_base_data_loc = ls_context_loc
          is_context_src   = ls_context_src
          is_ctrl_data     = ls_ctrl_data
          it_objects_src   = lt_objects_src
        CHANGING
          co_messages      = lo_messages.
    CATCH /sttpec/cx_whs_exception INTO lo_exc.
  ENDTRY.

* Find all buffer IDs for the Business function
  TRY.
      CALL METHOD /sttpec/cl_whs_bf_buffer=>read
        EXPORTING
          iv_busfunc   = ls_context_base-busfunc
          iv_custfield = ls_context_base-custfield
        IMPORTING
          et_bufferid  = lt_bufferid
        CHANGING
          co_messages  = lo_messages.

    CATCH /sttpec/cx_whs_exception INTO lo_exc.
      IF lo_exc->messages IS BOUND.
        CLEAR lt_messages[].
        lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
        lo_messages->set_messages( it_messages = lt_messages ).
      ENDIF.
      RETURN.
  ENDTRY.

* Post Event Comissioning for generated buffer id
  LOOP AT lt_bufferid ASSIGNING FIELD-SYMBOL(<ls_bufferid>).
    CALL METHOD /sttpec/cl_whs_bf_events=>compose_events_and_send
      EXPORTING
        iv_bufferid = <ls_bufferid>
      IMPORTING
        ev_rc       = lv_rc
      CHANGING
        co_messages = lo_messages.

    lv_rc_all = lv_rc_all + lv_rc.

*   Clear the buffer
    TRY.
        CALL METHOD /sttpec/cl_whs_bf_buffer=>clear
          EXPORTING
            iv_bufferid = <ls_bufferid>
          CHANGING
            co_messages = lo_messages.
      CATCH /sttpec/cx_whs_exception INTO lo_exc.
        IF lo_exc->messages IS BOUND.
          CLEAR lt_messages[].
          lo_exc->messages->get_messages( IMPORTING et_messages_total = lt_messages ).
          lo_messages->set_messages( it_messages = lt_messages ).
        ENDIF.
        CONTINUE.
    ENDTRY.
  ENDLOOP.

* Check for any errors
  READ TABLE lt_messages WITH KEY msgty = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc IS INITIAL.
    MESSAGE e003(zmsg_attp_ewm).
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form send_unpack
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_unpack USING lt_child_sn TYPE zrf_tt_child_sn.
  DATA: iv_activetab TYPE sy-ucomm.

  iv_activetab = 'UNPACK'.
  LOOP AT lt_child_sn ASSIGNING FIELD-SYMBOL(<fs_child_sn>).

    gs_tbox_unpack-scan_obj = <fs_child_sn>-rfsn.
    gs_tbox_unpack-scan_obj_parent = <fs_child_sn>-rfhu_parent2.
    gs_tbox_unpack-logsys = 'H01CLNT100'.
    gs_tbox_unpack-plant = '2000'.
    gs_tbox_unpack-sscc_parent = <fs_child_sn>-rfhu_parent2+4(18).

    PERFORM user_command_val USING    iv_activetab
                             CHANGING gs_tbox_unpack
                                      gt_unpack_res_list.
  ENDLOOP.
* Send Event
  PERFORM send_event USING    iv_activetab
                     CHANGING gs_tbox_unpack
                              gt_unpack_res_list.
  WAIT UP TO 1 SECONDS.
ENDFORM.
