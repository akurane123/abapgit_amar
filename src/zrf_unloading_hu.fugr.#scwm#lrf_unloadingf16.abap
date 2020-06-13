*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF16 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_egr_exists_for_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_UNLO_ADMIN_LGNUM  text
*      -->P_CS_UNLO_PROD  text
*      -->P_CS_REHU_HU  text
*      <--P_LT_PRD_HDR  text
*      <--P_LT_ITEMS  text
*----------------------------------------------------------------------*
FORM check_egr_exists_for_item  USING    iv_lgnum     TYPE /scwm/lgnum
                                         is_unlo_prod TYPE /scwm/s_rf_unlo_prod
                                CHANGING cs_unlo_dlv   TYPE /scwm/s_rf_unlo_docid
                                         cs_admin_unlo TYPE /scwm/s_rf_admin_unlo
                                         ct_prd_hdr    TYPE /scwm/dlv_header_out_prd_tab
                                         ct_items     TYPE /scwm/dlv_item_out_prd_tab.

  DATA: ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_selection    TYPE /scwm/dlv_selection_str,
        lt_selection    TYPE /scwm/dlv_selection_tab,
        ls_read_options TYPE /scwm/dlv_query_contr_str.

  DATA: ls_t300_md TYPE /scwm/s_t300_md.

  DATA: lo_dlv         TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_egr_manager TYPE REF TO /scwm/if_egr_manager.

  DATA: ls_docid_dlv TYPE /scwm/dlv_docid_item_str,
        lt_docid_dlv TYPE /scwm/dlv_docid_item_tab,
        ls_dlv_sel   TYPE /scwm/dlv_selection_str,
        lt_dlv_sel   TYPE /scwm/dlv_selection_tab,
        lt_items     TYPE /scwm/dlv_item_out_prd_tab,
        ls_items     TYPE /scwm/dlv_item_out_prd_str.

  DATA: lv_id         TYPE bu_partner_guid,
        lv_matnr      TYPE /scwm/de_rf_matnr,
        ls_mat_global TYPE  /scwm/s_material_global.

  DATA ls_partyloc      TYPE /scdl/dl_partyloc_str.
  DATA ls_partner       TYPE /scmb/mdl_partner_key_str.
  DATA ls_deliveries    TYPE /scwm/s_rf_unlo_docid.

* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.
  DATA(lo_saf) = /scdl/cl_af_management=>get_instance( ).
  lo_egr_manager ?= lo_saf->get_service( EXPORTING iv_service = /scwm/if_egr_manager=>sc_service ).


  IF cs_unlo_dlv-partyno IS INITIAL.

*   delivery query, keys only option
    ls_docid_dlv-docid  =  cs_unlo_dlv-docid.
    ls_docid_dlv-doccat =  cs_unlo_dlv-doccat.
    APPEND ls_docid_dlv TO lt_docid_dlv.

*   warehouse number
    ls_dlv_sel-fieldname = gc_dl_logfname_whno_i.
    ls_dlv_sel-option    = wmegc_option_eq.
    ls_dlv_sel-low       = cs_admin_unlo-lgnum.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.

    CLEAR ls_read_options.
    CLEAR ls_include_data.
    ls_read_options-keys_only = 'X'.
    ls_include_data-item_partyloc      = abap_true.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_docid        = lt_docid_dlv
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = lt_items.
      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
    ENDTRY.

    IF lt_items IS NOT INITIAL.
      READ TABLE lt_items INTO ls_items INDEX 1.
    ELSE.
      RETURN.
    ENDIF.

*   delivery query, normal data read, with one item from the delivery
    CLEAR lt_docid_dlv.
    ls_docid_dlv-docid =  ls_items-docid.
    ls_docid_dlv-itemid = ls_items-itemid.
    APPEND ls_docid_dlv TO lt_docid_dlv.

    CLEAR ls_read_options.
    CLEAR ls_include_data.
    ls_read_options-mix_in_object_instances  =  /scwm/if_dl_c=>sc_mix_in_load_instance.
    ls_read_options-data_retrival_only = abap_true.
    ls_include_data-item_partyloc      = abap_true.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_docid        = lt_docid_dlv
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = lt_items.
      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
    ENDTRY.


    READ TABLE lt_items INTO ls_items INDEX 1.
    IF sy-subrc EQ 0.
      READ TABLE ls_items-partyloc INTO ls_partyloc WITH KEY party_role = 'VENDOR'.
      IF sy-subrc = 0.
        lv_id = ls_partyloc-partyid.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.

    TRY.
        CALL FUNCTION '/SCMB/MDL_PARTNER_READ'
          EXPORTING
            iv_id   = lv_id
          IMPORTING
            es_data = ls_partner.
      CATCH
        /scmb/cx_mdl_interface                          "#EC NO_HANDLER
        /scmb/cx_mdl_result_empty                       "#EC NO_HANDLER
        /scmb/cx_mdl_result_toomany                     "#EC NO_HANDLER
        /scmb/cx_mdl_result                             "#EC NO_HANDLER
        /scmb/cx_mdl.                                   "#EC NO_HANDLER
    ENDTRY.

    cs_unlo_dlv-partyno = ls_partner-partner.
    CLEAR ls_deliveries.
    ls_deliveries-docid = cs_unlo_dlv-docid.
    ls_deliveries-partyno = ls_partner-partner.
    MODIFY cs_admin_unlo-deliveries FROM ls_deliveries
          TRANSPORTING partyno
          WHERE docid = ls_deliveries-docid .

  ENDIF.

  CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
    EXPORTING
      iv_lgnum   = iv_lgnum
    IMPORTING
      es_t300_md = ls_t300_md
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Get MATNR from MATID because MATNR could be a GTIN of the product
  IF is_unlo_prod-matid IS NOT INITIAL.
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = is_unlo_prod-matid
            iv_lgnum      = cs_admin_unlo-lgnum
**            iv_entitled   = lv_entitled
          IMPORTING
            es_mat_global = ls_mat_global.

        lv_matnr = ls_mat_global-matnr.

      CATCH /scwm/cx_md.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
  ELSE.
    lv_matnr = is_unlo_prod-matnr.
  ENDIF.

  CLEAR ls_selection.
  ls_selection-fieldname = /scwm/if_dl_logfname_c=>sc_prodind_prdi_h.
  ls_selection-sign      = /scmb/cl_search=>sc_sign_i.
  ls_selection-option    = /scmb/cl_search=>sc_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = /scdl/if_dl_logfname_c=>sc_locationno_wh_h.
  ls_selection-sign      = /scmb/cl_search=>sc_sign_i.
  ls_selection-option    = /scmb/cl_search=>sc_eq.
  ls_selection-low       = ls_t300_md-sc_unit.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = 'PRODUCTNO_I'.
  ls_selection-sign      = /scmb/cl_search=>sc_sign_i.
  ls_selection-option    = /scmb/cl_search=>sc_eq.
  ls_selection-low       = lv_matnr.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = 'PARTYNO_SFPRT_H'.
  ls_selection-sign      = /scmb/cl_search=>sc_sign_i.
  ls_selection-option    = /scmb/cl_search=>sc_eq.
  ls_selection-low       = cs_unlo_dlv-partyno.
  APPEND ls_selection TO lt_selection.

  CLEAR ls_read_options.
  CLEAR ls_include_data.
  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = abap_true.
  ls_include_data-item_partyloc      = abap_true.

  TRY.
      CALL METHOD lo_egr_manager->query
        EXPORTING
          it_selection    = lt_selection
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = ct_prd_hdr
          et_items        = ct_items.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " check_egr_exists_for_item

*&---------------------------------------------------------------------*
*&      Form  create_item_to_ibd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CS_REHU_HU    text
*      -->CS_REHU_PROD  text
*      -->LT_PRD_HDR    text
*      -->LT_ITEMS      text
*----------------------------------------------------------------------*
FORM create_item_to_ibd  USING    is_dlv       TYPE /scwm/s_rf_unlo_docid
                                  is_egr       TYPE /scwm/s_rf_unlo_docid
                         CHANGING cs_unlo_prod TYPE /scwm/s_rf_unlo_prod.
*                                  is_items     TYPE /scwm/dlv_item_out_prd_str.
*                                  lt_items     TYPE /scwm/dlv_item_out_prd_tab.

  DATA: lt_item_egr TYPE /scdl/t_sp_k_item,
        ls_item_egr TYPE /scdl/s_sp_k_item,
        lt_item_pdi TYPE /scwm/t_dlv_egr2pdi_item_ass,
        ls_item_pdi TYPE /scwm/s_dlv_egr2pdi_item_ass,
        lt_message  TYPE /scdl/dm_message_tab,
        ls_message  TYPE /scdl/dm_message_str.

  DATA: lo_egr     TYPE REF TO /scwm/if_egr_manager,
        lo_dlv     TYPE REF TO /scdl/cl_sp_prd_inb,
        lo_message TYPE REF TO /scdl/cl_dm_message.

  DATA: lt_inrecords    TYPE /scdl/t_sp_a_item_quantity,
        ls_inrecords    TYPE /scdl/s_sp_a_item_quantity,
        lt_outrecords   TYPE /scdl/t_sp_a_item_quantity,
        ls_outrecords   TYPE /scdl/s_sp_a_item_quantity,
        lt_return_codes TYPE /scdl/t_sp_return_code.

  DATA: ls_k_head   TYPE /scdl/s_sp_k_head,
        lt_k_head   TYPE /scdl/t_sp_k_head,
        ev_rejected TYPE boole_d.


  IF lo_dlv IS NOT BOUND.
    CREATE OBJECT lo_dlv.
  ENDIF.


  IF lo_egr IS NOT BOUND.
    DATA(lo_saf) = /scdl/cl_af_management=>get_instance( ).
    lo_egr ?= lo_saf->get_service( EXPORTING iv_service = /scwm/if_egr_manager=>sc_service ).
  ENDIF.

  ls_k_head-docid = is_dlv-docid.
  APPEND ls_k_head TO lt_k_head.

  lo_dlv->lock(
      EXPORTING inkeys       = lt_k_head
                lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
                aspect       = /scdl/if_sp_c=>sc_asp_head
      IMPORTING rejected     = ev_rejected
                return_codes = lt_return_codes ).

  PERFORM raise_error_sp USING lo_dlv
                               ev_rejected
                               lt_return_codes.

  ls_item_egr-docid  = is_egr-docid.
  ls_item_egr-itemid = is_egr-item_id.
  APPEND ls_item_egr TO lt_item_egr.

  CALL METHOD lo_egr->create_item_for_pdi
    EXPORTING
      iv_docid_pdi = is_dlv-docid
      it_item_egr  = lt_item_egr
    IMPORTING
      et_item_pdi  = lt_item_pdi
      eo_message   = lo_message.

  IF lo_message IS BOUND.
    lt_message = lo_message->get_messages( ).
    READ TABLE lt_message INTO ls_message WITH KEY msgty = 'E'.
*     contains error.
    IF sy-subrc IS INITIAL.
      MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
          WITH ls_message-msgv1 ls_message-msgv2
               ls_message-msgv3 ls_message-msgv4.
    ENDIF.
  ENDIF.

  READ TABLE lt_item_pdi INTO ls_item_pdi INDEX 1.

  ls_inrecords-docid  = ls_item_pdi-docidpdi.
  ls_inrecords-itemid = ls_item_pdi-itemidpdi.
  ls_inrecords-qty    = cs_unlo_prod-nista.
  ls_inrecords-uom    = cs_unlo_prod-uom.
  APPEND ls_inrecords TO lt_inrecords.

  CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
    EXPORTING
      aspect       = /scdl/if_sp_c=>sc_asp_item_quantity
      inrecords    = lt_inrecords
    IMPORTING
      outrecords   = lt_outrecords
      rejected     = ev_rejected
      return_codes = lt_return_codes.

  PERFORM raise_error_sp USING lo_dlv
                               ev_rejected
                               lt_return_codes.

  CLEAR: ev_rejected, lt_return_codes.
  CALL METHOD lo_dlv->/scdl/if_sp1_transaction~before_save
    IMPORTING
      rejected = ev_rejected.

  PERFORM raise_error_sp USING lo_dlv
                               ev_rejected
                               lt_return_codes.

  CALL METHOD lo_dlv->/scdl/if_sp1_transaction~save
*  EXPORTING
*    synchronously = abap_false
    IMPORTING
      rejected = ev_rejected.

  PERFORM raise_error_sp USING lo_dlv
                               ev_rejected
                               lt_return_codes.

*  It is to early to make commit and cleanup. Some batch update
*  can take place in the FORM pack_item_to_delivery (include
*  /SCWM/LRF_UNLOADINGF15). While the delivery QUERY cannot
*  make instance in this form, dump OBJECT_OBJREF_NOT_ASSIGNED
*  can happen by ABAP operands
*      lo_item ?= lo_bo->get_item( ls_inrecords_qty-itemid  ).
*      ls_product = lo_item->get_product( ).

*  COMMIT WORK AND WAIT.
*  CALL METHOD /scwm/cl_tm=>cleanup( ).

* store the new delivery item
  cs_unlo_prod-item_id = ls_item_pdi-itemidpdi.
  cs_unlo_prod-docid   = ls_item_pdi-docidpdi.
ENDFORM.                    " create_item_to_ibd

*&---------------------------------------------------------------------*
*&    Form  handle_error_sp
*&---------------------------------------------------------------------*
*     handle_error_sp
*     Filters Relevant Message from Service Provider
*----------------------------------------------------------------------*
*     -->IO_DLV
*     -->IV_REJECT
*     -->IT_RETURN
*     <--CS_MESSAGE
*----------------------------------------------------------------------*
FORM handle_error_sp USING io_dlv          TYPE REF TO /scdl/cl_sp_prd_inb
                           iv_rejected     TYPE boole_d
                           it_return_codes TYPE /scdl/t_sp_return_code
                     CHANGING cs_message   TYPE bapiret2.

  DATA:
    lt_message     TYPE /scdl/dm_message_tab,
    lo_message_box TYPE REF TO /scdl/cl_sp_message_box.

  FIELD-SYMBOLS:
    <ls_message>           TYPE /scdl/dm_message_str.

  CLEAR cs_message.
  IF it_return_codes IS INITIAL AND
     iv_rejected IS INITIAL.
    RETURN.
  ENDIF.

  READ TABLE it_return_codes TRANSPORTING NO FIELDS
    WITH KEY failed = abap_true.
  IF sy-subrc IS INITIAL OR iv_rejected EQ abap_true.

    lo_message_box = io_dlv->get_message_box( ).
    lt_message = lo_message_box->get_messages( ).

    LOOP AT lt_message ASSIGNING <ls_message> WHERE msgty = 'E'.
*     The last error message in the table is the first
*     which has been thrown. This is the root of the
*     problem.
    ENDLOOP.
    IF sy-subrc IS INITIAL.
      cs_message-type       = <ls_message>-msgty.
      cs_message-id         = <ls_message>-msgid.
      cs_message-number     = <ls_message>-msgno.
      cs_message-message_v1 = <ls_message>-msgv1 .
      cs_message-message_v2 = <ls_message>-msgv2 .
      cs_message-message_v3 = <ls_message>-msgv3 .
      cs_message-message_v4 = <ls_message>-msgv4 .
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&    Form  handle_error_sp
*&---------------------------------------------------------------------*
*     raise_error_sp
*     Raise Message from Service Provider
*----------------------------------------------------------------------*
*     -->IO_DLV
*     -->IV_REJECT
*     -->IT_RETURN
*----------------------------------------------------------------------*
FORM raise_error_sp USING io_dlv          TYPE REF TO /scdl/cl_sp_prd_inb
                          iv_rejected     TYPE boole_d
                          it_return_codes TYPE /scdl/t_sp_return_code.

  DATA: ls_message        TYPE bapiret2.

  PERFORM handle_error_sp USING io_dlv
                                iv_rejected
                                it_return_codes
                          CHANGING ls_message.

  IF ls_message IS NOT INITIAL.
    MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
            WITH ls_message-message_v1 ls_message-message_v2
                 ls_message-message_v3 ls_message-message_v4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_dlv_items
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DOCID  text
*      <--P_LT_ITEMS  text
*----------------------------------------------------------------------*
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

ENDFORM.                    " get_dlv_items
