*----------------------------------------------------------------------*
***INCLUDE LZRF_PRINT_HU_LABELF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form POST_EVENT_COMMISSIONING
*&---------------------------------------------------------------------*

FORM post_event_commissioning USING pi_range  TYPE gty_range
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
  ELSE.
    /scwm/cl_rf_bll_srvc=>message( EXPORTING iv_msgid = 'ZMSG_ATTP_EWM'
                                             iv_msgty = 'S'
                                             iv_msgno = 001
                                             iv_msgv1 = pi_hu ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_pack_hu
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_pack_hu USING p_hu TYPE /scwm/de_rf_huident
                          p_del TYPE /scwm/de_rf_dlvno
                          ls_docid_query TYPE /scwm/dlv_docid_item_str.


  DATA: lo_query        TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_pack         TYPE REF TO /scwm/cl_dlv_pack_ibdl,
        lo_message      TYPE REF TO  /scdl/cl_sp_message_box,
        lt_docid_query  TYPE /scwm/dlv_docid_item_tab,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        lv_lock         TYPE xfeld,
        ls_huhdr        TYPE /scwm/s_huhdr_int,
        lt_items        TYPE /scwm/dlv_item_out_prd_tab.

  DATA: ls_material TYPE /scwm/s_pack_stock,
        ls_quantity TYPE /scwm/s_quan.

  CONSTANTS: lv_matid TYPE /scwm/de_rf_pmatid VALUE '0050568C800D1ED9A293AB813EC6A0EF'.




  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_docid_query-doccat = wmegc_doccat_pdi.
  APPEND ls_docid_query TO lt_docid_query.

  ls_read_options-data_retrival_only = abap_true.

* check entered number
  IF ls_docid_query-docid IS INITIAL.
    MESSAGE e013(/scwm/rf_de) WITH p_del.
*   Delivery &1 does not exist.
  ENDIF.

  IF lo_query IS NOT BOUND.
    CREATE OBJECT lo_query.
  ENDIF.

  TRY.
      CALL METHOD lo_query->query
        EXPORTING
          it_docid        = lt_docid_query
          iv_whno         = gv_lgnum
          is_read_options = ls_read_options
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery .
  ENDTRY.


* PACKING
  CALL METHOD /scwm/cl_tm=>cleanup( ).

  IF lo_pack IS NOT BOUND.
    CREATE OBJECT lo_pack.
  ENDIF.


* might happen on concurrent packing there is a lock on the same
* item. So let's try to lock a bit later.
  DO 5 TIMES.
    CALL METHOD lo_pack->init_rf
      EXPORTING
        iv_lgnum        = gv_lgnum
        it_docid        = lt_docid_query
        iv_doccat       = wmegc_doccat_pdi
        iv_lock_dlv     = 'X'
      IMPORTING
        ev_foreign_lock = lv_lock.

*   Check the lock
    IF lv_lock IS INITIAL.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

* create empty HU withe the given huident
  CALL METHOD lo_pack->/scwm/if_pack_bas~create_hu
    EXPORTING
      iv_pmat    = lv_matid
      iv_huident = p_hu
    RECEIVING
      es_huhdr   = ls_huhdr
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  ls_material-qdoccat = ls_docid_query-doccat.
  ls_material-qdocid  = ls_docid_query-docid.
  ls_material-qitmid  = ls_docid_query-itemid.

  ls_quantity-quan = gv_qty.
  ls_quantity-unit = 'EA'.

  CALL METHOD lo_pack->/scwm/if_pack_bas~pack_stock
    EXPORTING
      iv_dest_hu  = ls_huhdr-guid_hu
      is_material = ls_material
      is_quantity = ls_quantity
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* save it
  CALL METHOD lo_pack->/scwm/if_pack_bas~save
    EXPORTING
      iv_commit = ' '
      iv_wait   = ' '
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  COMMIT WORK AND WAIT.
  CALL METHOD /scwm/cl_tm=>cleanup( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form dlvno_docid_get
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_DEL
*&      <-- LS_DOCID_QUERY
*&---------------------------------------------------------------------*
FORM dlvno_docid_get  USING    l_dlv TYPE /scwm/de_rf_dlvno
                      CHANGING ps_docid_query TYPE /scwm/dlv_docid_item_str.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*

  DATA: lv_dlvno      TYPE /scwm/de_rf_dlvno.

  DATA: ls_docno        TYPE /scwm/dlv_docno_str,
        ls_docid        TYPE /scwm/dlv_prd_map_str,
        ls_deliveries   TYPE /scwm/s_rf_unlo_docid,
        ls_dlv_sel      TYPE /scwm/dlv_selection_str,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_read_options TYPE /scwm/dlv_query_contr_str.

  DATA: lt_docno     TYPE /scwm/dlv_docno_tab,
        lt_docid     TYPE /scwm/dlv_prd_map_tab,
        lt_dlv_sel   TYPE /scwm/dlv_selection_tab,
        lt_prd_hdr   TYPE /scwm/dlv_header_out_prd_tab,
        ls_dlv_docid TYPE /scwm/dlv_docid_item_str,
        lt_dlv_docid TYPE /scwm/dlv_docid_item_tab,
        lt_headers   TYPE /scwm/dlv_header_out_prd_tab,
        ls_items     TYPE /scwm/dlv_item_out_prd_str,
        lt_items     TYPE /scwm/dlv_item_out_prd_tab.

  DATA: lo_dlv_prd TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_message TYPE REF TO  /scdl/cl_sp_message_box,
        lo_pack    TYPE REF TO /scwm/cl_dlv_pack_ibdl.

  CONSTANTS: gc_dl_logfname_whno_i TYPE /scdl/dl_logfname
                          VALUE /scwm/if_dl_logfname_c=>sc_whno_i,
             gc_dl_logfname_docno  TYPE /scdl/dl_logfname
                                     VALUE /scdl/if_dl_logfname_c=>sc_docno_h.

*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*

*fill up with zeros
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_dlv
    IMPORTING
      output = l_dlv.

  IF lo_dlv_prd IS NOT BOUND.
    CREATE OBJECT lo_dlv_prd.
  ENDIF.

  ls_dlv_sel-fieldname = gc_dl_logfname_whno_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = 'MWH1'.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.
*  DELIVERY
  ls_dlv_sel-fieldname = gc_dl_logfname_docno.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = l_dlv.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  ls_read_options-keys_only = 'X'.

  TRY.
      CALL METHOD lo_dlv_prd->query
        EXPORTING
          it_selection    = lt_dlv_sel
          iv_doccat       = wmegc_doccat_pdi
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = lt_prd_hdr.
    CATCH /scdl/cx_delivery.
  ENDTRY.

  IF lt_prd_hdr IS INITIAL.
    MESSAGE e013(/scwm/rf_de) WITH l_dlv.
*   Delivery does not exist
  ENDIF.

* prepare call
  MOVE: wmegc_doccat_pdi TO ls_docno-doccat,
        l_dlv  TO ls_docno-docno.

  APPEND ls_docno TO lt_docno.

* get guid for scanned delivery number
  CALL METHOD lo_dlv_prd->map_docno_to_docid
    EXPORTING
      it_docno   = lt_docno
    IMPORTING
      et_mapping = lt_docid.

  IF lt_docid IS INITIAL.
    MESSAGE e013(/scwm/rf_de) WITH l_dlv.
*   Delivery does not exist
  ENDIF.

  READ TABLE lt_docid INTO ls_docid INDEX 1.

  ls_dlv_docid-docid  = ls_docid-docid.
  APPEND ls_dlv_docid TO lt_dlv_docid.

* get document Items
  PERFORM get_dlv_items USING    lt_dlv_docid
                        CHANGING lt_headers
                                 lt_items.

  READ TABLE lt_items INTO ls_items INDEX 1.




* add delivery to delivery table (here only one)
*  ps_docid_query-dlvno = ls_deliveries-dlvno  = ls_docid-docno.
  ps_docid_query-docid = ls_deliveries-docid  = ls_docid-docid.
  ps_docid_query-doccat = ls_deliveries-doccat = ls_docid-doccat.
  ps_docid_query-itemid = ls_items-itemid.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_dlv_items
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DLV_DOCID
*&      <-- LT_HEADERS
*&      <-- LT_ITEMS
*&---------------------------------------------------------------------*
FORM get_dlv_items  USING    it_docid   TYPE /scwm/dlv_docid_item_tab
                    CHANGING ct_headers TYPE /scwm/dlv_header_out_prd_tab
                             ct_items   TYPE /scwm/dlv_item_out_prd_tab.

  DATA: ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_read_options TYPE /scwm/dlv_query_contr_str.

  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.


* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

* query deliveries

  ls_read_options-data_retrival_only = 'X'.
  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.

  ls_include_data-item_status = 'X'.
  ls_include_data-item_partyloc = 'X'.
  ls_include_data-head_status = 'X'.
  ls_include_data-head_status_dyn = 'X'.
  ls_include_data-head_partyloc = 'X'.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = it_docid
*         it_selection    = lt_dlv_sel
          is_read_options = ls_read_options
*         is_exclude_data = ls_excl_prd_dat
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = ct_headers
          et_items        = ct_items.

    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
* ????
  ENDTRY.



  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_sscc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CS_UNLO_RFHU
*&      <-- GV_QTY
*&---------------------------------------------------------------------*
FORM validate_sscc  CHANGING iv_rehu TYPE /scwm/de_rf_rfhu
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
