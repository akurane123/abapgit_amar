*----------------------------------------------------------------------*
***INCLUDE LZRF_DECOMM_HU_LABELF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form DECOMMISSION_HU
*&---------------------------------------------------------------------*

FORM decommission_hu  USING  pi_rehu_hu TYPE  /scwm/s_rf_rehu_hu.

  DATA: lv_rfc_logsys   TYPE logsys,
        lv_plant        TYPE werks_d,
        lv_rc           TYPE sysubrc,
        lv_rc_all       TYPE sysubrc,
        lv_code_char    TYPE string,
        lv_urn          TYPE string,
        lv_valid_flag   TYPE xfeld,
        lo_messages     TYPE REF TO /sttpec/cl_messages,
        ls_objcode      TYPE /sttpec/s_att_obj_request,
        ls_objdata      TYPE /sttpec/s_att_obj_response,
        ls_context      TYPE /sttpec/s_att_obj_context,
        ls_context_base TYPE /sttpec/s_whsdat_bas,
        lt_objects_src  TYPE /sttpec/t_obj_data,
        lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
        lt_messages     TYPE /sttpec/cl_messages=>tt_bal_msg.

STATICS:so_messages      TYPE REF TO /sttpec/cl_messages.

* Validate HU
  ls_objcode-code_type = gc_code_type.
  CONCATENATE gc_mask pi_rehu_hu-huident+2(18) INTO lv_code_char.
  ls_objcode-code_char = lv_code_char.
  ls_objcode-decode_mode = gc_decode_mode.
  ls_context-activity    = gc_activity.

* Call Method to validate HU number entered on screen
  /sttpec/cl_att_functions=>validate_activity(
        EXPORTING
          is_objcode            = ls_objcode
          is_validation_context = ls_context
          io_messages           = so_messages
        IMPORTING
          es_objdata            = ls_objdata
          ev_valid_flag         = lv_valid_flag ).

* Validation is sucessfull
  IF lv_valid_flag IS NOT INITIAL.

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
    ls_context_base-busfunc    = /sttpec/cl_whs_constants=>gcs_busfunc-decommission. "Business Function (06)
    ls_context_base-custfield  = 'DECOMMISS'.
    ls_context_base-logsys     = lv_rfc_logsys. "Logical System
    ls_context_base-plant      = '2000'.        "Plant

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

* Put Object Data in buffer for decommisioning
    IF lt_objects_src IS INITIAL.
      APPEND INITIAL LINE TO lt_objects_src ASSIGNING FIELD-SYMBOL(<lfs_obj_src>).
      <lfs_obj_src>-code_type  = gc_code_type.
      <lfs_obj_src>-code_char  = lv_code_char.
      <lfs_obj_src>-owner      = ls_objdata-owner.
      <lfs_obj_src>-serial     = ls_objdata-serial.
      <lfs_obj_src>-enc_type   = ls_objdata-enc_type.
      <lfs_obj_src>-sscc       = ls_objdata-sscc.
      <lfs_obj_src>-code_urn   = ls_objdata-code_urn.
    ENDIF.
* Put SSCC related data required for decomissioning
    IF ls_context_src IS INITIAL.
      ls_context_src-sscc =  ls_objdata-sscc.
      ls_context_src-gcp  =  ls_objdata-owner.
    ENDIF.
* Put Location related data
    IF ls_context_loc IS INITIAL.
      ls_context_loc-plant = '2000'.
    ENDIF.

* Set Buffer ID with decomissioning data
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

* Post Event deComissioning for generated buffer id
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
    ELSE.
      /scwm/cl_rf_bll_srvc=>message( EXPORTING iv_msgid = 'ZMSG_ATTP_EWM'
                                               iv_msgty = 'S'
                                               iv_msgno = 004
                                               iv_msgv1 = pi_rehu_hu-huident+2(18) ).
    ENDIF.
  ELSE.
* HU validation failed
    MESSAGE e005(zmsg_attp_ewm) WITH pi_rehu_hu-huident+2(18).
  ENDIF.

ENDFORM.
