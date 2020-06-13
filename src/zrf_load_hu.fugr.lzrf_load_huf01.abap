*----------------------------------------------------------------------*
***INCLUDE LZRF_LOAD_HUF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form LOAD_HU
*&---------------------------------------------------------------------*

FORM load_hu  USING     iv_load    TYPE /scwm/s_rf_load
                        iv_hu_load TYPE /scwm/tt_rf_load.

  DATA: lv_objcode        TYPE zobj_code,
        lv_valid          TYPE char1,
        lv_qty            TYPE int4,
        lv_rc             TYPE sysubrc,
        lv_rc_all         TYPE sysubrc,
        lv_rfc_logsys     TYPE logsys,
        lv_rfc_dest       TYPE rfcdest,
        lv_matnr          TYPE matnr,
        lt_bufferid       TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
        lt_messages       TYPE /sttpec/cl_messages=>tt_bal_msg,
        lo_delivery       TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_messages       TYPE REF TO /sttpec/cl_messages,
        lt_hierarchy      TYPE ztt_object_hierarchy,
        lt_docid          TYPE /scwm/dlv_docid_item_tab,
        lt_objects_src    TYPE /sttpec/t_obj_data,
        ls_docid          TYPE /scwm/dlv_docid_item_str,
        ls_context_base   TYPE /sttpec/s_whsdat_bas,
        ls_context        TYPE /sttpec/s_whsdat_ctx,
        lt_context_sd	    TYPE /sttpec/t_whsdat_srcdest,
        ls_context_parent TYPE /sttpec/s_whsdat_ctx,
        ls_object         TYPE /sttpec/s_obj_data,
        ls_doc_objects    TYPE /sttpec/s_whs_test_tbox_docobj,
        lv_plant          TYPE werks_d.
*        lo_exc            TYPE REF TO /sttpec/cx_whs_exception..

* Loop at Each HU and fill buffer for all the HU's scanned

  LOOP AT iv_hu_load INTO DATA(ls_hu_load).
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

* Populate Object Code into local variable.
    CONCATENATE '(00)' ls_hu_load-huident+2(18) INTO lv_objcode.

* Call FM to get quantity
    CALL FUNCTION 'ZFM_RFC_ATTP_GET_QUANTITY' DESTINATION 'ATDCLNT100_ABAP'
      EXPORTING
        im_objcode           = lv_objcode
        im_only_validate     = abap_false
      IMPORTING
        ex_valid             = lv_valid
        ex_qty               = lv_qty
        ex_hierarchy         = lt_hierarchy
      EXCEPTIONS
        query_object_failure = 1
        OTHERS               = 2.
    IF sy-subrc IS NOT INITIAL OR lv_valid IS INITIAL.
* Scanned Object failed
      MESSAGE e012(/sttpec/whs_msg) WITH ls_hu_load-huident.
    ELSE.
* Create Delivery Object in order to use Query Method
      CREATE OBJECT lo_delivery.

      ls_docid-docid  = ls_hu_load-docid.
      ls_docid-doccat = ls_hu_load-doccat.
      APPEND ls_docid TO lt_docid.

* Get Refrence document details
      TRY .
          lo_delivery->query( EXPORTING iv_doccat = ls_docid-doccat
                                        it_docid  = lt_docid
                              IMPORTING et_items = DATA(lt_items) ).
        CATCH /scdl/cx_delivery INTO DATA(lo_exec).
      ENDTRY.
* Read Item table to get ERP refrence Inbound Delivery
      READ TABLE lt_items INTO DATA(ls_items) INDEX 1.
      IF sy-subrc IS INITIAL.
        READ TABLE ls_items-refdoc INTO DATA(ls_ref_doc) WITH KEY refdoccat = 'ERP'.
        IF sy-subrc IS INITIAL.
          ls_context_base-docnum  = ls_ref_doc-refdocno.           "Outbound Delivery
          ls_context_base-docpos  = ls_ref_doc-refitemno+6(4).     "Item Number
          ls_context_base-docyear = sy-datum(4).                   "Document Year
        ENDIF.
      ENDIF.
* Populate Document type and Business Function
      IF ls_context_base-docnum  IS NOT INITIAL.
        ls_context_base-doctpe   = /sttpec/cl_whs_constants=>gcs_doctpe-out_dlv. "Document Type (03)
      ENDIF.
      ls_context_base-busfunc    = /sttpec/cl_whs_constants=>gcs_busfunc-load_container. "Business Function (02)
      ls_context_base-custfield  = 'LOAD'.
      ls_context_base-logsys     = lv_rfc_logsys.                                 "Logical System
* Get Plant Details using Refrence Document Details
      SELECT SINGLE werks
             INTO lv_plant
             FROM lips
             WHERE vbeln EQ ls_ref_doc-refdocno
             AND   posnr EQ ls_ref_doc-refitemno.
      IF sy-subrc IS INITIAL.
        ls_context_base-plant      = lv_plant.                                     "Plant
      ENDIF.

* get buffer id
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
              et_objects_src   = lt_objects_src
              et_objects_tgt   = DATA(lt_obj_tgt)
            CHANGING
              co_messages      = lo_messages.
        CATCH /sttpec/cx_whs_exception INTO lo_exc.
      ENDTRY.

* Set Quantity inside the object
      IF lt_objects_src IS NOT INITIAL.
        READ TABLE lt_objects_src ASSIGNING FIELD-SYMBOL(<lfs_obj_src>) INDEX 1.
        IF <lfs_obj_src>-quantity IS INITIAL.
          <lfs_obj_src>-quantity  = lv_qty.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO lt_objects_src ASSIGNING <lfs_obj_src>.
        <lfs_obj_src>-code_char = lv_objcode.
        <lfs_obj_src>-quantity  = lv_qty.
        READ TABLE lt_hierarchy INTO DATA(ls_hierarchy) INDEX 1.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ls_hierarchy-matnr
          IMPORTING
            output = lv_matnr.
        <lfs_obj_src>-product   = lv_matnr.
* Put Object Data in buffer for Loading
        READ TABLE gt_obj_data INTO DATA(ls_objdata) WITH KEY sscc = ls_hu_load-huident+2(18).
        IF sy-subrc IS INITIAL.
          <lfs_obj_src>-code_type    = gc_code_type.
          <lfs_obj_src>-code_char    = lv_objcode.
          <lfs_obj_src>-owner        = ls_objdata-owner.
          <lfs_obj_src>-serial       = ls_objdata-serial.
          <lfs_obj_src>-enc_type     = ls_objdata-enc_type.
          <lfs_obj_src>-sscc         = ls_objdata-sscc.
          <lfs_obj_src>-status_pack  = ls_hierarchy-status_pack.
          <lfs_obj_src>-status_stock = ls_hierarchy-status_stock.
          <lfs_obj_src>-code_urn     = ls_objdata-code_urn.
        ENDIF.
      ENDIF.
* Put SSCC related data required for Loading
      IF ls_context_src IS INITIAL.
        ls_context_src-sscc =  ls_objdata-sscc.
        ls_context_src-gcp  =  ls_objdata-owner.
      ENDIF.
* Put Location related data
      IF ls_context_loc IS INITIAL.
        ls_context_loc-plant = '2000'.
      ENDIF.
    ENDIF.
*  ENDIF.

* Set Quantity inside Buffer
    TRY.
        CALL METHOD /sttpec/cl_whs_bf_buffer=>set
          EXPORTING
            iv_bufferid      = ls_context_base-bufferid
            is_base_data_loc = ls_context_loc
            is_context_src   = ls_context_src
*            is_ctrl_data     = ls_ctrl_data
            it_objects_src   = lt_objects_src
          CHANGING
            co_messages    = lo_messages.
      CATCH /sttpec/cx_whs_exception INTO lo_exc.
    ENDTRY.

    CLEAR:ls_hu_load.
  ENDLOOP.

* Find all buffer IDs for the business function&tab
  CLEAR: ls_context_base.
  ls_context_base-busfunc    = /sttpec/cl_whs_constants=>gcs_busfunc-load_container. "Business Function (02)
  ls_context_base-custfield  = 'LOAD'.

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

* Trigger EPCIS for each buffer
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
*& Form VALIDATE_HU
*&---------------------------------------------------------------------*

FORM validate_hu  USING  pi_load TYPE /scwm/s_rf_load
                  CHANGING pc_valid_flg TYPE xfeld.

  DATA:lv_rc           TYPE sysubrc,
       lv_rc_all       TYPE sysubrc,
       lv_code_char    TYPE string,
       lv_valid_flag   TYPE xfeld,
       ls_objcode      TYPE /sttpec/s_att_obj_request,
       ls_objdata      TYPE /sttpec/s_att_obj_response,
       ls_context      TYPE /sttpec/s_att_obj_context,
       ls_context_base TYPE /sttpec/s_whsdat_bas,
       lt_objects_src  TYPE /sttpec/t_obj_data,
       lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
       lt_messages     TYPE /sttpec/cl_messages=>tt_bal_msg,
       lo_messages     TYPE REF TO /sttpec/cl_messages,
       lo_exc          TYPE REF TO /sttpec/cx_whs_exception.

  STATICS:so_messages      TYPE REF TO /sttpec/cl_messages.

* Validate HU
  ls_objcode-code_type = gc_code_type.
  CONCATENATE gc_mask pi_load-huident+2(18) INTO lv_code_char.
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
          ev_valid_flag         = pc_valid_flg  ).

  APPEND ls_objdata TO gt_obj_data.

ENDFORM.
