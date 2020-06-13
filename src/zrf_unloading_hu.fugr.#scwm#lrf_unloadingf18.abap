*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF18 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_TU_NUM_EXT
*&---------------------------------------------------------------------*
*       Determine and the TU number based on the scanned data
*----------------------------------------------------------------------*
*              USING    is_admin_unlo TYPE /scwm/s_rf_admin_unlo
*              CHANGING cs_unlo       TYPE /scwm/s_rf_unlo
*----------------------------------------------------------------------*
FORM get_tu_num_ext
        USING    is_admin_unlo TYPE /scwm/s_rf_admin_unlo
        CHANGING cs_unlo       TYPE /scwm/s_rf_unlo.

  DATA:
      ls_tunit      TYPE /scwm/tunit,
      lv_tu_num     TYPE /scwm/de_tu_num,
      lv_door       TYPE /scwm/de_door,
      lv_door_bin   TYPE /scwm/lgpla,

      ls_bapiret  TYPE bapiret2,
      lt_bapiret  TYPE bapirettab,
      lv_severity TYPE bapi_mtype.

  MOVE cs_unlo-id_tu_door TO cs_unlo-tu_num.

* Existing behaviour in older releases
  SELECT SINGLE * FROM /scwm/tunit INTO ls_tunit
                  WHERE tu_num_ext = cs_unlo-tu_num.        "#EC WARNOK
  IF sy-subrc = 0.
    cs_unlo-tu_num = ls_tunit-tu_num_ext.
  ELSE.
    IF strlen( cs_unlo-tu_num ) < 19.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_unlo-tu_num
        IMPORTING
          output = lv_tu_num.

      CLEAR ls_tunit.
      SELECT SINGLE * FROM /scwm/tunit INTO ls_tunit
                      WHERE tu_num = lv_tu_num.
      IF sy-subrc <> 0.

*       New logic: Scanned data is not a TU, then it might be a door
*       or a door bin
        MOVE cs_unlo-id_tu_door TO lv_door_bin.
        CALL FUNCTION '/SCWM/DOOR_GET_TU_NOW'
          EXPORTING
            iv_lgnum    = is_admin_unlo-lgnum
            iv_door_bin = lv_door_bin
          IMPORTING
            ev_tu_num   = lv_tu_num
            et_bapiret  = lt_bapiret
            ev_severity = lv_severity.
        IF lv_severity CA wmegc_severity_ea.
          MOVE cs_unlo-id_tu_door TO lv_door.
          CALL FUNCTION '/SCWM/DOOR_GET_TU_NOW'
            EXPORTING
              iv_lgnum    = is_admin_unlo-lgnum
              iv_door     = lv_door
            IMPORTING
              ev_tu_num   = lv_tu_num
              et_bapiret  = lt_bapiret
              ev_severity = lv_severity.
          IF lv_severity CA wmegc_severity_ea.
            READ TABLE lt_bapiret WITH KEY type = wmegc_severity_abort INTO ls_bapiret.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE lt_bapiret WITH KEY type = wmegc_severity_err INTO ls_bapiret.
            ENDIF.
            MESSAGE ID ls_bapiret-id TYPE wmegc_severity_err
               NUMBER ls_bapiret-number
               WITH ls_bapiret-message_v1 ls_bapiret-message_v2
                    ls_bapiret-message_v3 ls_bapiret-message_v4.
          ENDIF.
        ENDIF.
        CLEAR ls_tunit.
        SELECT SINGLE * FROM /scwm/tunit INTO ls_tunit
                        WHERE tu_num = lv_tu_num.
*       Prevent check on unique assignment later on as there can only
*       be one active TU at a door at one point in time
        IF sy-subrc = 0.
          cs_unlo-tu_num   = ls_tunit-tu_num_ext.
          cs_unlo-tsp_scac = ls_tunit-tsp.
        ELSE.
          CLEAR cs_unlo-tu_num.
        ENDIF.

      ELSE.
        cs_unlo-tu_num = ls_tunit-tu_num_ext.
      ENDIF.
    ELSE.
      CLEAR cs_unlo-tu_num.
    ENDIF.

  ENDIF.

ENDFORM.                    "get_tu_num_ext
*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_BC_BY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DLV_ITMS_ITEMTYPE  text
*      -->P_LS_DLV_DOCCAT  text
*      <--P_LS_ITEM_BC  text
*----------------------------------------------------------------------*
FORM get_item_bc_by_type  USING    iv_itemtype TYPE /scdl/dl_itemtype
                                   iv_doccat   TYPE /scdl/dl_doccat
                          CHANGING es_item_bc
                                       TYPE /scdl/dl_itype_detail_str.

  DATA: lo_service      TYPE REF TO /scdl/cl_af_management,
        lo_bc_access    TYPE REF TO /scdl/if_af_business_conf,
        ls_item_extkey  TYPE        /scdl/dl_itmtype_extkey_str.

  CLEAR es_item_bc.

  lo_service = /scdl/cl_af_management=>get_instance( ).
  IF lo_service IS NOT BOUND.
    RETURN.
  ENDIF.

  TRY.
      lo_bc_access ?= lo_service->get_service(
                        /scdl/if_af_management_c=>sc_business_conf ).
    CATCH /scdl/cx_af_management.                       "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* get business configuration
  ls_item_extkey-item_type = iv_itemtype.
  ls_item_extkey-category  = iv_doccat.

  TRY.
      lo_bc_access->get_item_bc_by_type(
        EXPORTING
          is_item_extkey = ls_item_extkey
        IMPORTING
          es_itype       = es_item_bc ).
    CATCH /scdl/cx_af_business_conf.                    "#EC NO_HANDLER
      RETURN.
  ENDTRY.

ENDFORM.                    " GET_ITEM_BC_BY_TYPE
*&---------------------------------------------------------------------*
*&      Form  GET_DLV_ITEM_BY_MATID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_UNLO  text
*      -->P_CS_UNLO_PROD_MATID  text
*      <--P_LS_DLV  text
*      <--P_LS_DLV_ITEM  text
*----------------------------------------------------------------------*
FORM get_dlv_item_by_matid  USING    is_admin_unlo  TYPE /scwm/s_rf_admin_unlo
                                     iv_matid TYPE /scwm/de_rf_matid
                            CHANGING cs_dlv   TYPE /scwm/s_rf_unlo_docid
                                     cs_dlv_item TYPE /scwm/s_dlv_item_auom_det.

  DATA: ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_selection    TYPE /scwm/dlv_selection_str,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_docid_dlv    TYPE /scwm/dlv_docid_item_str,
        ls_dlv_sel      TYPE /scwm/dlv_selection_str,
        ls_items        TYPE /scwm/dlv_item_out_prd_str,
        ls_headers      TYPE /scwm/dlv_header_out_prd_str,
        ls_partyloc     TYPE /scdl/dl_partyloc_str,
        ls_deliveries   TYPE /scwm/s_rf_unlo_docid,
        ls_t300_md      TYPE /scwm/s_t300_md.

  DATA: lt_selection    TYPE /scwm/dlv_selection_tab,
        lt_docid_dlv    TYPE /scwm/dlv_docid_item_tab,
        lt_dlv_sel      TYPE /scwm/dlv_selection_tab,
        lt_items        TYPE /scwm/dlv_item_out_prd_tab,
        lt_headers      TYPE /scwm/dlv_header_out_prd_tab.

  DATA: lo_dlv          TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_stock_fields TYPE REF TO /scwm/cl_ui_stock_fields.

  DATA: lv_id           TYPE bu_partner_guid.

  FIELD-SYMBOLS: <ls_unlo_dlv> TYPE /scwm/s_rf_unlo_docid.

* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

* Clear returning parameters
  CLEAR cs_dlv.
  CLEAR cs_dlv_item.

  LOOP AT is_admin_unlo-deliveries ASSIGNING <ls_unlo_dlv>.
*   delivery query, keys only option
    ls_docid_dlv-docid  =  <ls_unlo_dlv>-docid.
    ls_docid_dlv-doccat =  <ls_unlo_dlv>-doccat.
    APPEND ls_docid_dlv TO lt_docid_dlv.
  ENDLOOP.

*   warehouse number
  ls_dlv_sel-fieldname = gc_dl_logfname_whno_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = is_admin_unlo-lgnum.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  CLEAR ls_read_options.
  CLEAR ls_include_data.

  ls_read_options-mix_in_object_instances  =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = abap_true.
  ls_include_data-head_partyloc      = abap_true.
  ls_include_data-item_partyloc      = abap_true.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = lt_docid_dlv
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = lt_headers
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
  ENDTRY.

* Search item with same MATID; we take the first one
  READ TABLE lt_items INTO ls_items
    WITH KEY product-productid = iv_matid.
  IF sy-subrc EQ 0.
*   Read the corresponding header data
    READ TABLE lt_headers INTO ls_headers
      WITH KEY docid = ls_items-docid.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.

  READ TABLE ls_headers-partyloc INTO ls_partyloc
       WITH KEY party_role = /scdl/if_dl_partyloc_c=>sc_party_role_wh.
  IF sy-subrc = 0.
    MOVE ls_partyloc-locationid TO cs_dlv-wh_locationid.
    MOVE ls_partyloc-locationno TO cs_dlv-wh_locationno.
  ENDIF.
  READ TABLE ls_headers-partyloc INTO ls_partyloc
    WITH KEY party_role = 'SFPRT'.
  IF sy-subrc = 0.
    cs_dlv-partyno = ls_partyloc-partyno.
    cs_dlv-partytext = ls_partyloc-party_text.
  ENDIF.

  cs_dlv-dlvno = ls_headers-docno.
  cs_dlv-docid = ls_headers-docid.
  cs_dlv-doccat = ls_headers-doccat.
  cs_dlv-doctype = ls_headers-doctype.

  cs_dlv_item-docid = ls_items-docid.
  cs_dlv_item-itemid = ls_items-itemid.
  cs_dlv_item-doccat = ls_items-doccat.
  cs_dlv_item-itemtype = ls_items-itemtype.
  cs_dlv_item-productid = ls_items-product-productid.
  cs_dlv_item-productno = ls_items-product-productno.
  cs_dlv_item-qty = ls_items-qty-qty.
  cs_dlv_item-uom = ls_items-qty-uom.
  IF ls_items-batchid IS NOT INITIAL.
    IF lo_stock_fields IS NOT BOUND.
      CREATE OBJECT lo_stock_fields.
    ENDIF.
    CALL METHOD lo_stock_fields->get_batchno_by_id
      EXPORTING
        iv_batchid = ls_items-batchid
      RECEIVING
        ev_charg   = cs_dlv_item-batchno.
  ENDIF.
  cs_dlv_item-entitled = ls_items-sapext-entitled.
  cs_dlv_item-owner = ls_items-stock-stock_owner.
  cs_dlv_item-procty = ls_items-sapext-/scwm/procty.

ENDFORM.                    " GET_DLV_ITEM_BY_MATID
