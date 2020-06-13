*----------------------------------------------------------------------*
***INCLUDE LZRF_RECEIVING_HUSF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form VALIDATE_HU_ATTP
*&---------------------------------------------------------------------*
FORM validate_hu_attp  USING    iv_rehu    TYPE /scwm/s_rf_admin_rehu
                                iv_cs_rehu TYPE  /scwm/s_rf_rehu_hu.

  DATA: lv_objcode        TYPE zobj_code,
        lv_valid          TYPE char1,
        lv_qty            TYPE int4,
        lv_rfc_logsys     TYPE logsys,
        lv_rfc_dest       TYPE rfcdest,
        lo_delivery       TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_messages       TYPE REF TO /sttpec/cl_messages,
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

  " Get destination
*  CALL METHOD /sttpec/cl_helper_utilities=>get_rfc_destination
*    EXPORTING
*      iv_logsys  = lv_rfc_logsys
*    RECEIVING
*      rv_rfcdest = lv_rfc_dest.
*  IF lv_rfc_dest IS INITIAL.
*    MESSAGE e014(/sttpec/int_msg).
*  ENDIF.

* Populate Object Code into local variable.
  CONCATENATE '(00)' iv_cs_rehu-huident+2(18) INTO lv_objcode.

* Call FM to check whether scanned HU is valid object in ATTP or not.
  CALL FUNCTION 'ZFM_RFC_ATTP_GET_QUANTITY' DESTINATION 'ATDCLNT100_ABAP'
    EXPORTING
      im_objcode           = lv_objcode
      im_only_validate     = abap_false
    IMPORTING
      ex_valid             = lv_valid
      ex_qty               = lv_qty
*     EX_HIERARCHY         = lt_hierarchy
    EXCEPTIONS
      query_object_failure = 1
      OTHERS               = 2.
  IF sy-subrc IS NOT INITIAL OR lv_valid IS INITIAL.
* Scanned Object failed
*    MESSAGE e012(/sttpec/whs_msg) WITH iv_cs_rehu-huident.
  ELSE.
* Create Delivery Object in order to use Query Method
    CREATE OBJECT lo_delivery.

* Get the details of Inbound delivery
    READ TABLE iv_rehu-itms ASSIGNING FIELD-SYMBOL(<lfs_items>) INDEX 1.
    IF sy-subrc IS INITIAL.
      ls_docid-docid  = <lfs_items>-docid.
      ls_docid-itemid = <lfs_items>-itemid.
      ls_docid-doccat = <lfs_items>-doccat.
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
          ls_context_base-docnum  = ls_ref_doc-refdocno.           "Inbound Delivery
          ls_context_base-docpos  = ls_ref_doc-refitemno+6(4).     "Item Number
          ls_context_base-docyear = sy-datum(4).                   "Document Year
        ENDIF.
      ENDIF.

* Populate Document type and Business Function
      IF ls_context_base-docnum  IS NOT INITIAL.
        ls_context_base-doctpe   = /sttpec/cl_whs_constants=>gcs_doctpe-inb_dlv. "Document Type (03)
      ENDIF.
      ls_context_base-busfunc    = /sttpec/cl_whs_constants=>gcs_busfunc-receive_container. "Business Function (02)
      ls_context_base-custfield  = 'GRHU'.
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
              iv_bufferid    = ls_context_base-bufferid
            IMPORTING
              et_objects_src = lt_objects_src
              et_objects_tgt = DATA(lt_obj_tgt)
            CHANGING
              co_messages    = lo_messages.
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
      ENDIF.

* Set Quantity inside Buffer
      TRY.
          CALL METHOD /sttpec/cl_whs_bf_buffer=>set
            EXPORTING
              iv_bufferid    = ls_context_base-bufferid
              it_objects_src = lt_objects_src
            CHANGING
              co_messages    = lo_messages.
        CATCH /sttpec/cx_whs_exception INTO lo_exc.
      ENDTRY.
    ENDIF.
  ENDIF.

ENDFORM.                     "VALIDATE_HU_ATTP

*&---------------------------------------------------------------------*
*& Form SEND_EPCIS_TO_ATTP
*&---------------------------------------------------------------------*

FORM send_epcis_to_attp USING IV_rehu TYPE /scwm/s_rf_admin_rehu
                              IV_rehu_hu TYPE  /scwm/s_rf_rehu_hu.

  DATA:lv_rc           TYPE sysubrc,
       lv_rc_all       TYPE sysubrc,
       ls_context_base TYPE /sttpec/s_whsdat_bas,
       lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
       lt_messages     TYPE /sttpec/cl_messages=>tt_bal_msg,
       lo_messages     TYPE REF TO /sttpec/cl_messages,
       lo_exc          TYPE REF TO /sttpec/cx_whs_exception.

  ls_context_base-busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-receive_container. "Business Function (02)
  ls_context_base-custfield  = 'GRHU'.

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
      ENDIF.
      RETURN.
  ENDTRY.

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

  commit work and wait.
** Check for any errors
*  READ TABLE lt_messages WITH KEY msgty = 'E' TRANSPORTING NO FIELDS.
*  IF sy-subrc IS INITIAL.
*    MESSAGE e003(zmsg_attp_ewm).
*    RETURN.
*  ENDIF.

*  WAIT UP TO 4 SECONDS.
*  PERFORM SEND_EVENT_PUTAWAY USING IV_rehu
*                              IV_rehu_hu .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form send_event
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_event_PUTAWAY USING IV_cs_rehu TYPE /scwm/s_rf_admin_rehu
                              IV_cs_rehu_hu TYPE  /scwm/s_rf_rehu_hu.

  DATA:lv_rc           TYPE sysubrc,
       lv_rc_all       TYPE sysubrc,
       "ls_context_base TYPE /sttpec/s_whsdat_bas,
       lt_bufferid     TYPE /sttpec/cl_whs_bf_buffer=>tt_guid,
       lt_messages     TYPE /sttpec/cl_messages=>tt_bal_msg,
       lo_messages     TYPE REF TO /sttpec/cl_messages,
       lo_exc          TYPE REF TO /sttpec/cx_whs_exception,
       ls_context_base   TYPE /sttpec/s_whsdat_bas,
       ls_object         TYPE /sttpec/s_obj_data,
       lt_return         TYPE bapiret2_t,
       ls_context        TYPE /sttpec/s_whsdat_ctx,
       lv_existance      TYPE char1.

* Create log
  IF lo_messages IS NOT BOUND.
    lo_messages = /sttpec/cl_message_ctrl=>create( iv_object    = /sttpec/cl_whs_test_constants=>gc_tbox_balobj
                                                   iv_subobject = /sttpec/cl_whs_test_constants=>gc_tbox_subobj ).
  ENDIF.
  clear : ls_context_base, ls_object, ls_context.

  ls_context_base-busfunc = /sttpec/cl_whs_constants=>gcs_busfunc-putaway. "Business Function (02)
  ls_context_base-custfield  = 'PUTAWAY'.
  ls_context_base-logsys = 'H01CLNT100'.
  ls_context_base-plant  = '2000'.


  LS_OBJECT-CODE_TYPE = 'C'.
  CONCATENATE '(00)' iv_cs_rehu_HU-huident+2(18)  INTO LS_OBJECT-CODE_CHAR .
  ls_context-sscc = iv_cs_rehu_HU-huident+2(18).
  PERFORM create_check_buffer USING    ls_object
                                         abap_false
                                CHANGING ls_context_base
                                         lv_existance
                                         lv_rc
                                         lo_messages.
    IF lv_rc <> 0.
*      PERFORM log_display USING lo_messages.
*      RETURN.
    ENDIF.


   CALL METHOD /sttpec/cl_whs_bf_activities=>whs_putaway
        EXPORTING
          ib_use_buffer      = abap_true
          ib_skip_validation = ''
        IMPORTING
          ev_rc              = lv_rc
          et_return          = lt_return
        CHANGING
          cs_context_base    = ls_context_base
          cs_context         = ls_context
          cs_object          = ls_object.


************************************************************************************
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
      ENDIF.
      RETURN.
  ENDTRY.

  LOOP AT lt_bufferid ASSIGNING FIELD-SYMBOL(<ls_bufferid>).
    CALL METHOD /sttpec/cl_whs_bf_events=>compose_events_and_send
      EXPORTING
        iv_bufferid = <ls_bufferid>
      IMPORTING
        ev_rc       = lv_rc
      CHANGING
        co_messages = lo_messages.

    lv_rc_all = lv_rc_all + lv_rc.

** Clear the buffer
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
*& Form create_check_buffer
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_OBJECT
*&      --> ABAP_FALSE
*&      <-- LS_CONTEXT_BASE
*&      <-- LV_EXISTANCE
*&      <-- LV_RC
*&      <-- LO_MESSAGES
*&---------------------------------------------------------------------*
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
