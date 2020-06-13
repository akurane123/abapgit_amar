*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF15 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  matid_read_from_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_UNLO_PROD_MATNR  text
*      <--P_LV_MATID  text
*----------------------------------------------------------------------*
FORM matid_read_from_matnr  USING    iv_matnr TYPE /scwm/de_rf_matnr
                            CHANGING cv_matid TYPE /scwm/de_rf_matid
                                     cv_ean_gtin TYPE xfeld.

  DATA lv_matean TYPE /scwm/de_rf_ean11.

  CLEAR cv_matid.

  CALL FUNCTION '/SCWM/RF_PRODUCT_INPUT'
    EXPORTING
      input      = iv_matnr
    IMPORTING
      ev_matid   = cv_matid
      ev_matean  = lv_matean.

  IF lv_matean IS NOT INITIAL.
    cv_ean_gtin = abap_true.
  ENDIF.
ENDFORM.                    "matid_read_from_matnr
*&---------------------------------------------------------------------*
*&      Form  check_for_open_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_DOCID  text
*      <--P_LV_MATID  text
*      <--P_CS_UNLO_PROD  text
*      <--P_LT_ITEMS  text
*      <--P_LT_OPEN_ITEMS  text
*      <--P_LV_BATCH_REQ  text
*      <--P_LV_BATCHID  text
*----------------------------------------------------------------------*
FORM check_for_open_quantity  USING    iv_lgnum       TYPE /scwm/lgnum
                                       iv_entitled    TYPE /scwm/de_entitled
                              CHANGING ct_docid       TYPE /scwm/dlv_docid_item_tab
                                       cs_unlo_prod   TYPE  /scwm/s_rf_unlo_prod
                                       ct_open_items  TYPE /scwm/dlv_hu_prd_tab.



  DATA: lt_free        TYPE /scwm/dlv_hu_prd_tab,
        ls_free        TYPE /scwm/dlv_hu_prd_str,
        lt_docid       TYPE /scwm/dlv_docid_item_tab,
        ls_docid       TYPE /scwm/dlv_docid_item_str,
        lt_hu_dlv      TYPE /scwm/dlv_hu_prd_tab,
        ls_matid_charg TYPE /scwm/s_matid_charg.

  DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        lv_quan_conv    TYPE /scwm/de_quantity,
        lv_idx          TYPE sy-tabix,
        lv_quan         TYPE /scwm/de_quantity.

  DATA: lo_pack       TYPE REF TO /scwm/cl_dlv_pack_ibdl,
        lo_dlv        TYPE REF TO /scwm/cl_dlv_management_prd,
        lt_prd_hdr    TYPE /scwm/dlv_header_out_prd_tab,
        lo_prd2hum    TYPE REF TO /scwm/cl_dlv_prd2hum,
        lt_items      TYPE /scwm/dlv_item_out_prd_tab,
        ls_mat_global TYPE  /scwm/s_material_global,
        ls_dlv_sel    TYPE /scwm/dlv_selection_str,
        lt_dlv_sel    TYPE /scwm/dlv_selection_tab.


  FIELD-SYMBOLS: <fs_docid> TYPE /scwm/s_docid,
                 <docid>    TYPE /scwm/dlv_docid_item_str,
                 <fs_free>  TYPE /scwm/dlv_hu_prd_str,
                 <fs_items> TYPE /scwm/dlv_item_out_prd_str.
  FIELD-SYMBOLS: <fs_items_2> TYPE /scwm/dlv_item_out_prd_str.


  CLEAR: ct_open_items.

* preselection to replace the method lo_pack->init with lo_pack->init_rf
* here we select only the relevant items by matid
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_dlv_sel-fieldname = gc_dl_logfname_productid_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = cs_unlo_prod-matid.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  CLEAR ls_dlv_sel.
  LOOP AT ct_docid ASSIGNING <docid>.
    ls_dlv_sel-fieldname = gc_dl_logfname_docid.
    ls_dlv_sel-option    = wmegc_option_eq.
    ls_dlv_sel-low       = <docid>-docid.
    APPEND ls_dlv_sel TO lt_dlv_sel.
  ENDLOOP.

  ls_read_options-keys_only = 'X'.
  ls_read_options-item_part_select = 'X'.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_selection    = lt_dlv_sel
          is_read_options = ls_read_options
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.
  ENDTRY.

  LOOP AT lt_items ASSIGNING <fs_items>.
    ls_docid-docid = <fs_items>-docid.
    ls_docid-itemid = <fs_items>-itemid.
    APPEND ls_docid TO lt_docid.
  ENDLOOP.

  IF lt_docid IS NOT INITIAL.
    IF lo_pack IS NOT BOUND.
      CREATE OBJECT lo_pack.
    ENDIF.

    CALL METHOD lo_pack->init_rf
      EXPORTING
        iv_lgnum  = iv_lgnum
        it_docid  = lt_docid
        iv_doccat = wmegc_doccat_pdi.

    CALL METHOD lo_pack->get_free
      IMPORTING
        et_free = lt_free.

*   filter for the different material in the delivery
    DELETE lt_free WHERE stock-matid NE cs_unlo_prod-matid.
  ENDIF.

  TRY.
*  Convert quantity
      MOVE cs_unlo_prod-nista TO lv_quan.
    CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
      MESSAGE e605(/scwm/rf_en) WITH cs_unlo_prod-nista.
  ENDTRY.

* Loop over the items and check if the UoM is the same
* if not convert and at the end compare the QtY.
  LOOP AT lt_free ASSIGNING <fs_free>.
    lv_idx = sy-tabix.
*   UoM conversion if the Delivery UoM <> selected UoM
    IF <fs_free>-open_qty-uom <> cs_unlo_prod-uom.

      TRY .
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = cs_unlo_prod-matid
              iv_quan      = lv_quan
              iv_unit_from = cs_unlo_prod-uom
              iv_unit_to   = <fs_free>-open_qty-uom
              iv_batchid   = <fs_free>-stock-batchid
            IMPORTING
              ev_quan      = lv_quan_conv.
        CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
              /scwm/cx_md_internal_error
              /scwm/cx_md_batch_not_required
              /scwm/cx_md_material_exist.
      ENDTRY.
    ELSE.
      lv_quan_conv = cs_unlo_prod-nista.
    ENDIF.

*   if the requested quantity < than we need => delete
    IF <fs_free>-open_qty-qty < lv_quan_conv.
      DELETE lt_free INDEX lv_idx.
    ENDIF.

  ENDLOOP.

* later also used
  UNASSIGN : <fs_free>.

* read the material detail
  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_unlo_prod-matid
          iv_lgnum      = iv_lgnum
          iv_entitled   = iv_entitled
        IMPORTING
          es_mat_global = ls_mat_global.
    CATCH /scwm/cx_md.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  IF ls_mat_global-batch_req IS NOT INITIAL.
    ls_matid_charg-matid = cs_unlo_prod-matid.
    ls_matid_charg-charg = cs_unlo_prod-charg.
    TRY.
        CALL METHOD /scwm/cl_batch_appl=>get_batchid_by_no
          EXPORTING
            iv_entitled    = iv_entitled
            is_matid_charg = ls_matid_charg
          IMPORTING
            ev_batchid     = cs_unlo_prod-batchid.
      CATCH /scwm/cx_batch_precheck.
    ENDTRY.

*   filter for the different batch in the delivery
    DELETE lt_free WHERE stock-batchid NE cs_unlo_prod-batchid AND
                         stock-batchid IS NOT INITIAL.

*   check for inactive items from the delivery
    IF lt_free IS INITIAL.

      IF lo_dlv IS NOT BOUND.
        CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
          RECEIVING
            eo_instance = lo_dlv.
      ENDIF.

      CLEAR: ls_read_options, ls_include_data.
      ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
      ls_read_options-data_retrival_only = abap_true.
      ls_include_data-item_status        = abap_true.
      ls_include_data-head_status        = abap_true.
      ls_include_data-head_status_dyn    = abap_true.
      ls_include_data-item_hierarchy = 'X'.

      TRY.
          CALL METHOD lo_dlv->query
            EXPORTING
              it_docid        = ct_docid
              is_read_options = ls_read_options
              is_include_data = ls_include_data
            IMPORTING
              et_headers      = lt_prd_hdr
              et_items        = lt_items.
        CATCH /scdl/cx_delivery.                        "#EC NO_HANDLER
* ????
      ENDTRY.

      LOOP AT lt_items ASSIGNING <fs_items>
        WHERE product-productid = cs_unlo_prod-matid. "AND
        "qty-qty >= cs_unlo_prod-nista.

*       Check if the UoMs are the same => if not convert
        IF <fs_items>-qty-uom <> cs_unlo_prod-uom.
          TRY .
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = cs_unlo_prod-matid
                  iv_quan      = lv_quan
                  iv_unit_from = cs_unlo_prod-uom
                  iv_unit_to   = <fs_items>-qty-uom
                  iv_batchid   = <fs_items>-batchid
                IMPORTING
                  ev_quan      = lv_quan_conv.
            CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                  /scwm/cx_md_internal_error
                  /scwm/cx_md_batch_not_required
                  /scwm/cx_md_material_exist.
          ENDTRY.
        ELSE.
          lv_quan_conv = cs_unlo_prod-nista.
        ENDIF.

*       If the Del. quantity in the same Measure is lower
*       continue => not relevant item!!
        IF <fs_items>-qty-qty < lv_quan_conv.
          CONTINUE.
        ENDIF.

        READ TABLE <fs_items>-status WITH KEY status_type = gc_status_type_dbd
                                              status_value = abap_true
                                     TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          ls_free-prd_no       = <fs_items>-docno.
          ls_free-prd_id       = <fs_items>-docid.
          ls_free-doccat_prd   = <fs_items>-doccat.
          ls_free-item_no      = <fs_items>-itemno.
          ls_free-item_id      = <fs_items>-itemid.
          ls_free-open_qty-qty = <fs_items>-qty-qty.
          ls_free-open_qty-uom = <fs_items>-qty-uom.

          APPEND ls_free TO lt_free.
        ENDIF.
      ENDLOOP.

*     searching for already batch splitted items
*     they are at top of the hierarchy
*     if there exist a batch splitted item the delivery is not inactive any more
      LOOP AT lt_items ASSIGNING <fs_items>
      WHERE product-productid = cs_unlo_prod-matid.

        READ TABLE <fs_items>-hierarchy WITH KEY hierarchy_level = '0'
                                     TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          ls_free-prd_no     = <fs_items>-docno.
          ls_free-prd_id     = <fs_items>-docid.
          ls_free-doccat_prd = <fs_items>-doccat.
          ls_free-item_no    = <fs_items>-itemno.
          ls_free-item_id    = <fs_items>-itemid.
          ls_free-open_qty-qty = <fs_items>-qty-qty.
          ls_free-open_qty-uom = <fs_items>-qty-uom.

*         search for the already existing batch subitemms
*         quantity reduction
          LOOP AT lt_items ASSIGNING <fs_items_2>
            WHERE product-productid = cs_unlo_prod-matid.

            READ TABLE <fs_items_2>-hierarchy WITH KEY parent_object = <fs_items>-itemid
                                     TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              ls_free-open_qty-qty = ls_free-open_qty-qty - <fs_items_2>-qty-qty.
            ENDIF.
          ENDLOOP.
          IF ls_free-open_qty-qty < lv_quan_conv.
            CONTINUE.
          ENDIF.
          APPEND ls_free TO lt_free.
        ENDIF.
      ENDLOOP.

    ENDIF.

    ct_open_items = lt_free.
  ELSE.
*   remaining product available to pack
    ct_open_items = lt_free.
  ENDIF.

ENDFORM.                    " check_for_open_quantity
*&---------------------------------------------------------------------*
*&      Form  create_charg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_REHU_PROD  text
*----------------------------------------------------------------------*
FORM create_charg  CHANGING cs_unlo_prod  TYPE /scwm/s_rf_unlo_prod.

  DATA lv_ean_gtin  TYPE xfeld.

  DATA: lo_batch        TYPE REF TO /scwm/cl_batch_appl  .

  DATA: ls_matid_charg  TYPE /scwm/s_matid_charg.

  BREAK-POINT ID /scwm/rf_unloading.

  TRY.
      lo_batch = /scwm/cl_batch_appl=>get_instance(
          iv_productid = cs_unlo_prod-matid
          iv_batchno   = cs_unlo_prod-charg ).

    CATCH /scwm/cx_batch_management.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDTRY.

*The product number is requiered,
*the batch number is optional (internal numbering).
*This method has to be called before saving the batch.
*The batch data will be checked internally.

  TRY.
      lo_batch->before_save( ).

    CATCH /scwm/cx_batch_management.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDTRY.
*  depending on confi
  /scwm/cl_batch_appl=>save( ).

  COMMIT WORK AND WAIT.

  CALL METHOD /scwm/cl_tm=>cleanup( ).

  PERFORM matid_read_from_matnr
    USING cs_unlo_prod-matnr
    CHANGING cs_unlo_prod-matid
             lv_ean_gtin.


  ls_matid_charg-matid = cs_unlo_prod-matid.
  ls_matid_charg-charg = cs_unlo_prod-charg.

  TRY.
      CALL METHOD /scwm/cl_batch_appl=>get_batchid_by_no
        EXPORTING
          is_matid_charg = ls_matid_charg
*         IT_MATID_CHARG =
        IMPORTING
          ev_batchid     = cs_unlo_prod-batchid
*         ES_VERSION_ATTR =
*         ET_RESULT      =
        .
    CATCH /scwm/cx_batch_precheck .


  ENDTRY.

ENDFORM.                    " create_charg
*&---------------------------------------------------------------------*
*&      Form  pack_spec_for_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ITEMS_PRD_>MV_DOCTYPE  text
*      -->P_CS_ADMIN_UNLO_LGNUM  text
*      -->P_LV_QUANTITY  text
*      -->P_LS_ITEMS  text
*      <--P_LV_HUMATID  text
*----------------------------------------------------------------------*
FORM pack_spec_for_item USING    iv_lgnum TYPE /scwm/lgnum
                                 is_unlo_prod TYPE /scwm/s_rf_unlo_prod
                                 it_deliveries TYPE /scwm/tt_rf_unlo_docid
                        CHANGING cv_humatid TYPE /scwm/de_matid  .

  DATA: ls_ttype TYPE /scdl/tdtype.

  DATA: ls_fields         TYPE /scwm/pak_com_i,
        lv_procedure      TYPE /sapcnd/ctlist_name,
        ls_condition      TYPE /scwm/s_ps_cond,
        lt_packspec       TYPE  /scwm/tt_guid_ps,
        l_data            TYPE /scwm/dlv_docid_item_str,
        lv_pspec          TYPE /scwm/de_matid,
        ls_psp_hdr        TYPE /scwm/s_ps_header_int,
        lt_psp_content    TYPE  /scwm/tt_packspec_nested,
        ls_psp_content    TYPE  /scwm/s_packspec_nested,
        ls_levels         TYPE /scwm/s_ps_level_int,
        ls_content        TYPE /scwm/s_ps_content_int,
        ls_packspec_level TYPE  /scwm/s_ps_level_int,
        lt_elementgroup   TYPE  /scwm/tt_ps_elementgroup,
        ls_delivery       TYPE /scwm/s_rf_unlo_docid,
        lv_doctype        TYPE /scdl/dl_doctype.

  READ TABLE it_deliveries INTO ls_delivery
       WITH KEY docid = is_unlo_prod-docid.

  IF sy-subrc = 0.
    ls_fields-pak_locid = ls_delivery-wh_locationid.
  ENDIF.

  ls_fields-pak_matid = is_unlo_prod-matid.
*  MOVE-CORRESPONDING is_items TO l_data.
  ls_condition-quantity = is_unlo_prod-nista.
  ls_condition-unit_q   = is_unlo_prod-uom.

  IF ls_delivery-doctype IS INITIAL.
    lv_doctype = gc_doctype_inb. "INB
  ELSE.
    lv_doctype = ls_delivery-doctype.
  ENDIF.

  SELECT SINGLE * FROM /scdl/tdtype
    INTO ls_ttype
    WHERE type = lv_doctype AND
          category = wmegc_doccat_pdi.

  lv_procedure = ls_ttype-dlvap.
  l_data-docid  = is_unlo_prod-docid.
  l_data-itemid = is_unlo_prod-item_id.
  l_data-doccat = is_unlo_prod-doccat.

  CALL FUNCTION '/SCWM/PS_FIND_AND_EVALUATE'
    EXPORTING
      is_fields       = ls_fields
      iv_procedure    = lv_procedure
      is_condition    = ls_condition
      i_data          = l_data
    IMPORTING
      et_packspec     = lt_packspec
    EXCEPTIONS
      determine_error = 1
      read_error      = 2
      no_record_found = 3
      OTHERS          = 4.

  IF lt_packspec IS INITIAL.
    RETURN.                   " no packspec exists.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_packspec INTO lv_pspec INDEX 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
    EXPORTING
      iv_guid_ps          = lv_pspec
      iv_read_elements    = 'X'
    IMPORTING
      es_packspec_header  = ls_psp_hdr
      et_packspec_content = lt_psp_content
      et_elementgroup     = lt_elementgroup
      es_packspec_level   = ls_packspec_level
    EXCEPTIONS
      error               = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_psp_content INTO ls_psp_content INDEX 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE ls_psp_content-levels INTO ls_levels
    WITH KEY hu_create = abap_true.
  IF sy-subrc IS INITIAL.
    cv_humatid = ls_levels-hu_matid.
  ENDIF.

ENDFORM.                    " PACK_SPEC_FOR_ITEM
*&---------------------------------------------------------------------*
*&      Form  pack_item_to_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_UNLO_PMATID  text
*      -->P_LS_ITEMS_PRD_>MV_DOCTYPE  text
*      -->P_LV_BATCHID  text
*      -->P_CS_UNLO_PROD_CHARG  text
*      -->P_CS_ADMIN_UNLO_LGNUM  text
*      -->P_LV_QUANTITY  text
*      -->P_LS_ITEMS  text
*      <--P_LS_HUHDR  text
*----------------------------------------------------------------------*
FORM pack_item_to_delivery  USING    is_deliveries TYPE /scwm/s_rf_unlo_docid
                            CHANGING cs_unlo_prod  TYPE /scwm/s_rf_unlo_prod
                                     cs_unlo       TYPE /scwm/s_rf_unlo
                                     cs_admin_unlo TYPE /scwm/s_rf_admin_unlo.



  DATA: lo_pack    TYPE REF TO /scwm/cl_dlv_pack_ibdl,
        lo_message TYPE REF TO  /scdl/cl_sp_message_box,
        lo_query   TYPE REF TO /scwm/cl_dlv_management_prd.
*  DATA: lo_pack TYPE REF TO /scwm/cl_wm_packing.

  DATA: ls_material  TYPE /scwm/s_pack_stock,
        ls_quantity  TYPE /scwm/s_quan,
        ls_k_head    TYPE  /scdl/s_sp_k_head,
        lt_k_head    TYPE  /scdl/t_sp_k_head,
        lt_huitm     TYPE /scwm/tt_huitm_int,
        ls_huitm     TYPE /scwm/s_huitm_int,
        ls_huitm_out TYPE /scwm/s_huitm_int,
        lt_dlvkey    TYPE /scdl/af_hum_data_tab,
        ls_dlvkey    TYPE /scdl/af_hum_data_str,
        lv_rejected  TYPE  boole_d,
        lv_lock      TYPE xfeld,
        lv_batch_crea TYPE /scwm/dl_batchcrea,
        lv_badi_imp(30) TYPE c.


  DATA: ls_huhdr TYPE /scwm/s_huhdr_int,
        ls_hus   TYPE /scwm/s_rf_unlo_hus.

  DATA: lt_huref           TYPE /scwm/tt_huref_int,
        ls_huref           TYPE /scwm/s_huref_int,
        lt_itmkey_lock     TYPE /scdl/t_sp_k_item,
        ls_itmkey_lock     TYPE /scdl/s_sp_k_item,
        lt_inrecords_bbd   TYPE /scdl/t_sp_a_item_sapext_prdi,
        ls_inrecords_bbd   TYPE /scdl/s_sp_a_item_sapext_prdi,
        lt_outrecords_bbd  TYPE /scdl/t_sp_a_item_sapext_prdi,
        ls_inrecords       TYPE /scdl/s_sp_a_item_product,
        lt_inrecords       TYPE /scdl/t_sp_a_item_product,
        lt_outrecords_item TYPE /scdl/t_sp_a_item_product,  "#EC NEEDED
        lt_item_product    TYPE /scdl/t_sp_a_item_product,
        ls_item_product    TYPE /scdl/s_sp_a_item_product,
        lt_return_code     TYPE /scdl/t_sp_return_code,
        lt_bapiret         TYPE bapirettab,
        ls_bapiret         TYPE bapiret2,
        lv_severity        TYPE bapi_mtype,
        lt_message         TYPE /scdl/dm_message_tab,
        ls_message         TYPE /scdl/dm_message_str,
        lt_items           TYPE /scwm/dlv_item_out_prd_tab,
        ls_items           TYPE /scwm/dlv_item_out_prd_str,
        lt_create_hu       TYPE /scwm/tt_to_crea_hu,
        ls_create_hu       TYPE /scwm/s_to_crea_hu,
        ls_mat_global      TYPE /scwm/s_material_global,
        lt_docid_query     TYPE /scwm/dlv_docid_item_tab,
        ls_docid_query     TYPE /scwm/dlv_docid_item_str,
        ls_read_options    TYPE /scwm/dlv_query_contr_str,
        lt_ltap            TYPE /scwm/tt_ltap_vb,
        lv_fcode           TYPE /scwm/de_fcode,
        lv_timezone        TYPE tznzone,
        lv_bbdat           TYPE /scwm/de_rf_sp_bbdat,
        lv_procty          TYPE /scwm/de_procty.

  DATA ls_inrecords_qty   TYPE /scdl/s_sp_a_item_quantity.
  DATA lt_inrecords_qty   TYPE /scdl/t_sp_a_item_quantity.
  DATA lt_outrecords_qty  TYPE /scdl/t_sp_a_item_quantity.
  DATA ls_inrecords_prod  TYPE /scdl/s_sp_a_item_product.
  DATA lt_inrecords_prod  TYPE /scdl/t_sp_a_item_product.
  DATA lt_outrecords_prod TYPE /scdl/t_sp_a_item_product.
  DATA lt_item_key        TYPE /scdl/t_sp_k_item.
  DATA ls_item_key        TYPE /scdl/s_sp_k_item.
  DATA lt_outrecords      TYPE /scdl/t_sp_a_item.
  DATA ls_outrecords      TYPE /scdl/s_sp_a_item.
  DATA ls_action          TYPE /scdl/s_sp_act_action.
  DATA ls_context         TYPE /scdl/s_sp_act_item_split.
  DATA lv_new_item_id     TYPE /scdl/dl_itemid.
  DATA lv_quantity        TYPE /lime/quantity.

  DATA: lo_dlv     TYPE REF TO /scdl/cl_sp_prd_inb,
        lo_prd2hum TYPE REF TO /scwm/cl_dlv_prd2hum,
        lo_badi_crea TYPE REF TO /scwm/ex_dlv_batch_crea.

  DATA lo_bo      TYPE REF TO /scdl/if_bo.
  DATA lo_item    TYPE REF TO /scdl/cl_dl_item_write.
  DATA lo_bom     TYPE REF TO /scdl/cl_bo_management.
  DATA ls_product TYPE        /scdl/dl_product_str.
  DATA lo_batch   TYPE REF TO /scwm/cl_batch_appl.
  DATA ls_sapext  TYPE        /scdl/dl_sap_dr_item_str.

  DATA ls_batch_md            TYPE  /scwm/dlv_md_prod_batch_det.
  DATA lo_md_access TYPE REF TO /scwm/cl_dlv_md_access.
  DATA lv_entitled  TYPE /scwm/de_entitled.
  DATA lv_batch TYPE /scdl/dl_batchno.
  DATA lv_new_batch TYPE xfeld.

  DATA ls_item_qty_upd      TYPE /scdl/s_sp_a_item_quantity.
  DATA lt_item_qty_upd      TYPE /scdl/t_sp_a_item_quantity.
  DATA lt_item_qty_upd_out  TYPE /scdl/t_sp_a_item_quantity.
  DATA lv_update            TYPE xfeld.
  DATA ls_items_upd         TYPE /scwm/dlv_item_out_prd_str.
  DATA lv_quan              TYPE /scdl/dl_quantity.
  DATA ls_ltap              TYPE /scwm/ltap.

  CONSTANTS:
        wmelc_subitem_no TYPE i VALUE 1.                    "Number of subitems after item split

  FIELD-SYMBOLS    <fs_item>        TYPE /scwm/dlv_item_out_prd_str.
  FIELD-SYMBOLS    <ls_parameter>   TYPE  any.
  FIELD-SYMBOLS    <fs_hierarchy>   TYPE /scdl/dl_hierarchy_str.


*  IF lv_batch_req IS NOT INITIAL AND
*      cs_unlo_prod-batchid IS INITIAL.
*
*    CALL FUNCTION '/SCWM/RF_REHU_CRBA'
*      EXPORTING
*        cv_matnr   = cs_unlo_prod-matnr
*        cv_batch   = cs_unlo_prod-charg
*      IMPORTING
*        ev_batchid = cs_unlo_prod-batchid
*        eo_batch   = lo_batch.
*
*  ENDIF.

*  CLEAR gv_dlvlockusr.
* BBD rel?

* set data for printing
  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'.

  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_docid_query-docid = cs_unlo_prod-docid.
  ls_docid_query-itemid = cs_unlo_prod-item_id.
  ls_docid_query-doccat = is_deliveries-doccat.
  APPEND ls_docid_query TO lt_docid_query.
  IF gv_dlvlockusr IS INITIAL.
    ls_read_options-data_retrival_only = abap_true.
  ELSE.
    ls_read_options-data_retrival_only = abap_false.
    CLEAR gv_dlvlockusr.
*  To provide to see the just created item
    CALL METHOD /scwm/cl_tm=>cleanup( ).
  ENDIF.

  IF lo_query IS NOT BOUND.
    CREATE OBJECT lo_query.
  ENDIF.

  TRY.
      CALL METHOD lo_query->query
        EXPORTING
          it_docid        = lt_docid_query
          iv_whno         = cs_admin_unlo-lgnum
          is_read_options = ls_read_options
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery .
  ENDTRY.

  READ TABLE lt_items INTO ls_items INDEX 1.

  lo_bom = /scdl/cl_bo_management=>get_instance( ).
*
  lo_bo = lo_bom->get_bo_by_id( cs_unlo_prod-docid ).

  IF lo_dlv IS NOT BOUND.
    CREATE OBJECT lo_dlv.
  ENDIF.

  ls_k_head-docid = cs_unlo_prod-docid.
  APPEND ls_k_head TO lt_k_head.

  lo_dlv->lock(
      EXPORTING inkeys       = lt_k_head
                lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
                aspect       = /scdl/if_sp_c=>sc_asp_head
      IMPORTING rejected     = lv_rejected
                return_codes = lt_return_code ).

  IF NOT lv_rejected IS INITIAL.
    PERFORM delivlock_handle
      USING cs_unlo-pmatid.
    EXIT.
  ENDIF.

* error_codes should be handled in service provider => return_codes must be empty!
  IF lt_return_code IS NOT INITIAL.
    PERFORM delivlock_handle
      USING cs_unlo-pmatid.
    EXIT.
  ENDIF.

  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_unlo_prod-matid
          iv_lgnum      = cs_admin_unlo-lgnum
          iv_entitled   = ls_items-sapext-entitled
        IMPORTING
          es_mat_global = ls_mat_global.
    CATCH /scwm/cx_md.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.
******************************
* batch relevant?
*******************************
* check wether the batch is already exist
  IF ls_mat_global-batch_req IS NOT INITIAL.

    lv_batch =  cs_unlo_prod-charg.
    lv_entitled = ls_items-sapext-entitled.

    lo_md_access = /scwm/cl_dlv_md_access=>get_instance( ).
    TRY.
        ls_batch_md = lo_md_access->get_batch_detail(
            iv_productno         = ls_mat_global-matnr
            iv_batchno           = lv_batch
            iv_lgnum             = cs_admin_unlo-lgnum
            iv_entitled          = lv_entitled
            iv_no_classification = abap_false ).
      CATCH /scwm/cx_dlv_batch .
*     batch is not yet existing -> possible to create
        lo_item ?= lo_bo->get_item( cs_unlo_prod-item_id ).
        ls_sapext = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item ).
        ls_product = lo_item->get_product( ).
        GET BADI lo_badi_crea
          FILTERS
            lgnum = cs_admin_unlo-lgnum.

        CALL BADI lo_badi_crea->check_batch_create
          EXPORTING
            iv_lgnum      = cs_admin_unlo-lgnum
            iv_doccat     = lo_item->mv_doccat
            iv_doc_type   = lo_item->mv_doctype
            iv_itemtype   = lo_item->mv_itemtype
            iv_itemcat    = lo_item->mv_itemcat
            iv_productid  = ls_product-productid
            iv_entitled   = ls_sapext-entitled
            iv_docid      = lo_item->mv_docid
            iv_itemid     = lo_item->mv_itemid
          IMPORTING
            ev_batch_crea = lv_batch_crea.

        IF lv_batch_crea = /scwm/if_dlv_batch_c=>sc_crea_no.
          lv_badi_imp = cl_abap_classdescr=>get_class_name( lo_badi_crea->imp ).
            IF lv_badi_imp CS '/SCWM/CL_EI_DLV_BATCH_DEF'.
*             SAP default implementation, table /SCWM/TDLVBATCH used
*             -> standard message
              MESSAGE e006(/scwm/dlv_batch) WITH lo_item->mv_itemtype lo_item->mv_doctype cs_admin_unlo-lgnum.
            ELSE.
*            Customer implementation -> reason why creation id not allowed
*            is unknown -> generic message
             MESSAGE e007(/scwm/dlv_batch).
            ENDIF.
        ENDIF.

        lv_new_batch = 'X'.
        CALL FUNCTION '/SCWM/RF_REHU_CRBA'
          EXPORTING
            cv_matid    = cs_unlo_prod-matid
            cv_matnr    = cs_unlo_prod-matnr
            cv_batch    = cs_unlo_prod-charg
            iv_lgnum    = cs_admin_unlo-lgnum
            iv_entitled = lv_entitled
          IMPORTING
            ev_batchid  = cs_unlo_prod-batchid
            eo_batch    = lo_batch.
    ENDTRY.
  ENDIF.

* creating batch if batch does not exist
  IF cs_unlo_prod-batchid   IS NOT INITIAL AND
     ls_items-product-batchno IS INITIAL.

    lv_lock = 'X'.

    lv_quantity = cs_unlo_prod-nista.

    IF cs_unlo_prod-uom <> ls_items-qty-uom.
      PERFORM convert_quan
        USING
          cs_unlo_prod-matid
          cs_unlo_prod-uom
          ls_items-qty-uom
          cs_unlo_prod-batchid
        CHANGING
          lv_quantity.
    ENDIF.

*   if the quantity equal to the whole amount from
*   the delivery item tehn no batch split is necessary.
    IF lv_quantity LT ls_items-qty-qty.

      IF ls_mat_global-batch_req IS NOT INITIAL.

        CLEAR ls_docid_query.
        ls_docid_query-docid = cs_unlo_prod-docid.
        ls_docid_query-doccat = cs_unlo_prod-doccat.
        APPEND ls_docid_query TO lt_docid_query.
        ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
        ls_read_options-data_retrival_only = 'X'.

        TRY.
            CALL METHOD lo_query->query
              EXPORTING
                it_docid        = lt_docid_query
                iv_whno         = cs_admin_unlo-lgnum
                is_read_options = ls_read_options
              IMPORTING
                et_items        = lt_items.
          CATCH /scdl/cx_delivery .                     "#EC NO_HANDLER
            MESSAGE ID     sy-msgid
                    TYPE   sy-msgty
                    NUMBER sy-msgno
                    WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDTRY.

*       search for already batch splitted items
        LOOP AT lt_items INTO ls_items_upd WHERE product-batchno = cs_unlo_prod-charg AND
                   product-productid = cs_unlo_prod-matid AND
                   manual = 'X'.
          READ TABLE ls_items_upd-hierarchy ASSIGNING <fs_hierarchy> WITH KEY hierarchy_type = 'BSP'  parent_object = cs_unlo_prod-item_id.
          IF sy-subrc = 0.
            lv_update = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDIF.

      IF lv_update IS INITIAL.

*       item key
        CLEAR lt_item_key.
        ls_item_key-docid  = cs_unlo_prod-docid.
        ls_item_key-itemid = cs_unlo_prod-item_id.
        APPEND ls_item_key TO lt_item_key.

*       create a new batch subitem
        ls_context-hierarchy_type    = /scdl/if_dl_hierarchy_c=>sc_type_charge.
        ls_context-number_subitems   = wmelc_subitem_no.
        ls_action-action_code        = /scdl/if_bo_action_c=>sc_split_item.

        CREATE DATA ls_action-action_control TYPE ('/SCDL/S_SP_ACT_ITEM_SPLIT').
        ASSIGN ls_action-action_control->* TO <ls_parameter>.
        MOVE-CORRESPONDING ls_context TO <ls_parameter>.

        lo_dlv->execute(
          EXPORTING aspect       = /scdl/if_sp_c=>sc_asp_item
                    inkeys       = lt_item_key
                    inparam      = ls_action
                    action       = /scdl/if_sp_c=>sc_act_execute_action
          IMPORTING
                    outrecords   = lt_outrecords
                    rejected     = lv_rejected
                    return_codes = lt_return_code ).

        PERFORM raise_error_sp USING lo_dlv
                                     lv_rejected
                                     lt_return_code.

*       find the new subitem
        DELETE lt_outrecords WHERE itemid = ls_item_key-itemid.
        READ TABLE lt_outrecords INTO ls_outrecords WITH KEY docid = ls_items-docid.
        lv_new_item_id = ls_outrecords-itemid.
        cs_unlo_prod-item_id = lv_new_item_id.

*       update quantity
        ls_inrecords_qty-docid  = ls_items-docid.
        ls_inrecords_qty-itemid = lv_new_item_id.
        ls_inrecords_qty-qty    = lv_quantity.
        ls_inrecords_qty-uom    = ls_items-qty-uom.
        APPEND ls_inrecords_qty TO lt_inrecords_qty.

        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_quantity
            inrecords    = lt_inrecords_qty
          IMPORTING
            outrecords   = lt_outrecords_qty
            rejected     = lv_rejected
            return_codes = lt_return_code.

      PERFORM raise_error_sp USING lo_dlv
                                   lv_rejected
                                   lt_return_code.

        lo_item ?= lo_bo->get_item( ls_inrecords_qty-itemid  ).
        ls_product = lo_item->get_product( ).

      ELSE.

        ls_items = ls_items_upd.
        cs_unlo_prod-item_id = ls_items-itemid.
        MOVE cs_unlo_prod-nista TO lv_quan .

        PERFORM convert_quan
                    USING
                       cs_unlo_prod-matid
                       cs_unlo_prod-uom
                       ls_items-qty-uom
                       cs_unlo_prod-batchid
                    CHANGING
                       lv_quan.

        CLEAR: ls_item_qty_upd, lt_item_qty_upd.
        ls_item_qty_upd-docid = ls_items-docid.
        ls_item_qty_upd-itemid = ls_items-itemid.
        ls_item_qty_upd-qty = ls_items-qty-qty + lv_quan.
        ls_item_qty_upd-uom = ls_items-qty-uom.
        APPEND ls_item_qty_upd TO lt_item_qty_upd.

        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_quantity
            inrecords    = lt_item_qty_upd
          IMPORTING
            outrecords   = lt_item_qty_upd_out
            rejected     = lv_rejected
            return_codes = lt_return_code.

        PERFORM raise_error_sp USING lo_dlv
                                     lv_rejected
                                     lt_return_code.

        lo_item ?= lo_bo->get_item( ls_items-itemid  ).
        ls_product = lo_item->get_product( ).

      ENDIF.


    ELSE.

      lo_item ?= lo_bo->get_item( cs_unlo_prod-item_id  ).
      ls_product = lo_item->get_product( ).

    ENDIF.

*   update batch
    ls_inrecords_prod-docid          = ls_items-docid.
    ls_inrecords_prod-itemid         = cs_unlo_prod-item_id.
    ls_inrecords_prod-productid      = ls_items-product-productid.
    ls_inrecords_prod-productno      = ls_items-product-productno.
    ls_inrecords_prod-productno_ext  = ls_items-product-productno_ext.
    ls_inrecords_prod-batchno        = cs_unlo_prod-charg.
    ls_inrecords_prod-productent     = ls_items-product-productent.
    ls_inrecords_prod-product_text   = ls_items-product-product_text.
    APPEND ls_inrecords_prod TO lt_inrecords_prod.

    CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
      EXPORTING
        aspect       = /scdl/if_sp_c=>sc_asp_item_product
        inrecords    = lt_inrecords_prod
      IMPORTING
        outrecords   = lt_outrecords_prod
        rejected     = lv_rejected
        return_codes = lt_return_code.

    PERFORM raise_error_sp USING lo_dlv
                                 lv_rejected
                                 lt_return_code.

    ls_sapext = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item ).

*   Call Execute to trigger the batch determination (only if the batch already exists)
*   determination of batch attributes are necessary after batch update
    IF lv_new_batch IS INITIAL.
*     item key
      CLEAR lt_item_key.
      ls_item_key-docid  = cs_unlo_prod-docid.
      ls_item_key-itemid = cs_unlo_prod-item_id.
      APPEND ls_item_key TO lt_item_key.

      ls_action-action_code        = /scdl/if_bo_action_c=>sc_determine.
      CLEAR ls_action-action_control.

      lo_dlv->execute(
        EXPORTING aspect       = /scdl/if_sp_c=>sc_asp_item
                  inkeys       = lt_item_key
                  inparam      = ls_action
                  action       = /scdl/if_sp_c=>sc_act_execute_action
        IMPORTING
                  outrecords   = lt_outrecords
                  rejected     = lv_rejected
                  return_codes = lt_return_code ).

      PERFORM raise_error_sp USING lo_dlv
                                   lv_rejected
                                   lt_return_code.
    ENDIF.

*   update delivery for BBD if on the screen we added the BBD
    IF cs_unlo_prod-bbdat IS NOT INITIAL AND lv_new_batch IS NOT INITIAL.

      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = cs_admin_unlo-lgnum
        IMPORTING
          ev_tzone        = lv_timezone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CONVERT TIME STAMP ls_items-sapext-tsttobb TIME ZONE lv_timezone INTO DATE lv_bbdat.

      IF cs_unlo_prod-bbdat NE ls_batch_md-sled_bbd AND
         ls_batch_md-sled_bbd IS NOT INITIAL.
        cs_unlo_prod-bbdat = ls_batch_md-sled_bbd.
      ENDIF.

      IF cs_unlo_prod-bbdat <> lv_bbdat     AND
         cs_unlo_prod-bbdat IS NOT INITIAL  AND
         lv_bbdat IS NOT INITIAL.
        MESSAGE e436(/scwm/rf_en) WITH cs_unlo_prod-bbdat.
      ENDIF.

      IF ls_mat_global-batch_req IS NOT INITIAL.

        MOVE-CORRESPONDING ls_items-sapext TO  ls_inrecords_bbd.
        ls_inrecords_bbd-docid   = cs_unlo_prod-docid.
        ls_inrecords_bbd-itemid  = cs_unlo_prod-item_id.
        CONVERT DATE cs_unlo_prod-bbdat INTO TIME STAMP ls_inrecords_bbd-tstfrbb TIME ZONE lv_timezone.
        ls_inrecords_bbd-tsttobb = ls_inrecords_bbd-tstfrbb.
        ls_inrecords_bbd-tzonebb = lv_timezone.

        APPEND ls_inrecords_bbd TO lt_inrecords_bbd.

        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdi
            inrecords    = lt_inrecords_bbd
          IMPORTING
            outrecords   = lt_outrecords_bbd
            rejected     = lv_rejected
            return_codes = lt_return_code.

      PERFORM raise_error_sp USING lo_dlv
                                   lv_rejected
                                   lt_return_code.
      ENDIF.
    ENDIF.

    IF lv_new_batch IS NOT INITIAL.
*   update the valuation data
      TRY.
          IF lo_batch->mo_valuat_mng IS BOUND.
            /scwm/cl_dlv_batch_internal=>item_batch_valuate(
                iv_lgnum  = cs_admin_unlo-lgnum
                io_item   = lo_item
                io_batch  = lo_batch ).
          ENDIF.
        CATCH /scwm/cx_dlv_batch
              /scwm/cx_dlv_chval.
          MESSAGE e008(/scwm/batch).
      ENDTRY.

*   save the batch
      TRY.
          lo_batch->before_save( ).

        CATCH /scwm/cx_batch_management.
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDTRY.

      /scwm/cl_batch_appl=>save( ).
    ENDIF.

  ELSE.

    IF ls_mat_global-batch_req IS NOT INITIAL.

*     update delivery for BBD if on the screen we added the BBD
      IF cs_unlo_prod-bbdat      IS NOT INITIAL.

        CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
          EXPORTING
            iv_lgnum        = cs_admin_unlo-lgnum
          IMPORTING
            ev_tzone        = lv_timezone
          EXCEPTIONS
            interface_error = 1
            data_not_found  = 2
            OTHERS          = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CONVERT TIME STAMP ls_items-sapext-tsttobb TIME ZONE lv_timezone INTO DATE lv_bbdat.

        IF cs_unlo_prod-bbdat IS NOT INITIAL  AND
           lv_bbdat IS NOT INITIAL AND
           cs_unlo_prod-bbdat <> lv_bbdat.
          MESSAGE e436(/scwm/rf_en) WITH cs_unlo_prod-bbdat.
        ENDIF.

        lv_lock = 'X'.

        MOVE-CORRESPONDING ls_items-sapext TO  ls_inrecords_bbd.

        ls_inrecords_bbd-docid   = cs_unlo_prod-docid.
        ls_inrecords_bbd-itemid  = cs_unlo_prod-item_id.
        CONVERT DATE cs_unlo_prod-bbdat
              INTO TIME STAMP ls_inrecords_bbd-tstfrbb
              TIME ZONE lv_timezone.
        ls_inrecords_bbd-tsttobb = ls_inrecords_bbd-tstfrbb.
        APPEND ls_inrecords_bbd TO lt_inrecords_bbd.

*       update the BBD to the delivery item
        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdi
            inrecords    = lt_inrecords_bbd
          IMPORTING
            outrecords   = lt_outrecords_bbd
            rejected     = lv_rejected
            return_codes = lt_return_code.

      PERFORM raise_error_sp USING lo_dlv
                                   lv_rejected
                                   lt_return_code.
      ENDIF.
    ENDIF.
  ENDIF.

* if new item was created, inbound delivery must be saved as well
  IF ls_items-objchg = /scdl/if_dl_c=>sc_objchg_create.
    lv_lock = 'X'.
  ENDIF.

  IF lo_dlv IS BOUND AND lv_lock IS NOT INITIAL.

    CLEAR: lv_rejected, lt_return_code.
    CALL METHOD lo_dlv->/scdl/if_sp1_transaction~before_save
      IMPORTING
        rejected = lv_rejected.

    PERFORM raise_error_sp USING lo_dlv
                                 lv_rejected
                                 lt_return_code.

    CALL METHOD lo_dlv->/scdl/if_sp1_transaction~save
*  EXPORTING
*    synchronously = abap_false
      IMPORTING
        rejected = lv_rejected.

    PERFORM raise_error_sp USING lo_dlv
                                 lv_rejected
                                 lt_return_code.


    COMMIT WORK AND WAIT.
    CALL METHOD /scwm/cl_tm=>cleanup( ).
  ENDIF.

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
        iv_lgnum        = cs_admin_unlo-lgnum
        it_docid        = lt_docid_query
        iv_doccat       = is_deliveries-doccat
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
      iv_pmat    = cs_unlo-pmatid
      iv_huident = cs_unlo-huident
    RECEIVING
      es_huhdr   = ls_huhdr
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_material-qdoccat = is_deliveries-doccat.
  ls_material-qdocid  = cs_unlo_prod-docid.
  ls_material-qitmid  = cs_unlo_prod-item_id.

  ls_quantity-quan = cs_unlo_prod-nista.
  ls_quantity-unit = cs_unlo_prod-uom.


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


  IF cs_unlo_prod-bbdat IS NOT INITIAL.

    CALL METHOD lo_pack->/scwm/if_pack_bas~get_hu_item
      EXPORTING
        iv_guid_hu = ls_huhdr-guid_hu
*       iv_guid_stock =
      IMPORTING
        et_huitm   = lt_huitm
*       es_huitm   = ls_huitm
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_huitm INTO ls_huitm INDEX 1.

    ls_huitm-vfdat = cs_unlo_prod-bbdat.

    TRY.
        CALL METHOD lo_pack->/scwm/if_pack_bas~change_huitm
          EXPORTING
            is_huitm = ls_huitm
          IMPORTING
            es_huitm = ls_huitm_out.
      CATCH /scwm/cx_basics .
    ENDTRY.
  ENDIF.


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

  ls_hus-huident = ls_huhdr-huident.
  ls_hus-guid_hu = ls_huhdr-guid_hu.
  ls_hus-hutyp   = ls_huhdr-letyp.
  ls_hus-hzmt    = ls_huhdr-hzmt.
  ls_hus-lgtyp   = ls_huhdr-lgtyp.
  ls_hus-docid   = cs_unlo_prod-docid.
  ls_hus-doccat  = wmegc_doccat_pdi.
  APPEND ls_hus TO cs_admin_unlo-hus.

  cs_unlo-huident = ls_huhdr-huident.
  cs_unlo-rfhu = ls_huhdr-huident.

* refresh the HU (process control is filled or not)
  CALL FUNCTION '/SCWM/HUHEADER_READ'
    EXPORTING
      iv_huident  = ls_huhdr-huident
      iv_lock     = ' '
    IMPORTING
      es_huheader = ls_huhdr
    EXCEPTIONS
      not_found   = 1
      input       = 2
      error       = 3
      OTHERS      = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod(
                      /scwm/cl_rf_bll_srvc=>c_prmod_background ).
  /scwm/cl_rf_bll_srvc=>set_fcode('GTPCKP').

* simple or complex unloading?
  PERFORM simple_or_complex
    USING    cs_admin_unlo-lgnum
             ls_huhdr
    CHANGING lv_fcode.

* --> COMPLEX
  CHECK lv_fcode = gc_fcode_complex.

* Create WT for moving
  ls_create_hu-huident = ls_huhdr-huident.
  ls_create_hu-rdocrelevant = abap_true.
  APPEND ls_create_hu TO lt_create_hu.

  CALL FUNCTION '/SCWM/TO_CREA_HU_INT'
    EXPORTING
      iv_bname         = sy-uname
      it_create_hu_int = lt_create_hu
    IMPORTING
      et_ltap_vb       = lt_ltap
      et_bapiret       = lt_bapiret
      ev_severity      = lv_severity.

  IF lv_severity CA wmegc_severity_ea.
    READ TABLE lt_bapiret INTO ls_bapiret
         WITH KEY type = wmegc_severity_err.
    IF sy-subrc = 0.
      MESSAGE ID     ls_bapiret-id
              TYPE   ls_bapiret-type
              NUMBER ls_bapiret-number
              WITH   ls_bapiret-message_v1 ls_bapiret-message_v2
                     ls_bapiret-message_v3 ls_bapiret-message_v4.
*       message from bapiret
    ELSE.
      READ TABLE lt_bapiret INTO ls_bapiret
           WITH KEY type = wmegc_severity_abort.
      IF sy-subrc = 0.
        MESSAGE ID     ls_bapiret-id
                TYPE   ls_bapiret-type
                NUMBER ls_bapiret-number
                WITH   ls_bapiret-message_v1 ls_bapiret-message_v2
                       ls_bapiret-message_v3 ls_bapiret-message_v4.
*       message from bapiret
      ELSE.
        MESSAGE e037(/scwm/rf_de).
*       putaway not possible
      ENDIF.
    ENDIF.

  ELSE.
    CALL FUNCTION '/SCWM/TO_POST'
      EXPORTING
        iv_commit_work = ' '
      IMPORTING
        et_ltap_vb     = lt_ltap
        ev_severity    = lv_severity
        et_bapiret     = lt_bapiret.
    IF lv_severity CA 'EA'.
      LOOP AT lt_bapiret INTO ls_bapiret
             WHERE type = 'E' OR
                   type = 'A'.
        MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type
                NUMBER ls_bapiret-number
                  WITH ls_bapiret-message_v1 ls_bapiret-message_v2
                       ls_bapiret-message_v3 ls_bapiret-message_v4.
      ENDLOOP.
    ELSE.
      COMMIT WORK AND WAIT.
      CALL METHOD /scwm/cl_tm=>cleanup( ).
    ENDIF.

*   check if the WT confirmed automatically or not.
*   if confirmed, then send the control back to the HU input screen.
    READ TABLE lt_ltap INTO ls_ltap INDEX 1.
    IF sy-subrc IS INITIAL.
      IF ls_ltap-pquit IS NOT INITIAL.
        /scwm/cl_rf_bll_srvc=>set_fcode('SYEXC1').
        CLEAR: cs_unlo-huident,
               cs_unlo-rfhu.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " pack_item_to_delivery

*&---------------------------------------------------------------------*
*&    Form  CONVERT_QUAN
*&---------------------------------------------------------------------*
*     Convert the quantity between alternative unit of measure and
*     base unit of measure
*----------------------------------------------------------------------*
*     -->IV_MATID
*     -->IV_UNIT_FROM
*     -->IV_UNIT_TO
*     -->IV_BATCHID
*     <--CV_QUAN
*----------------------------------------------------------------------*
FORM convert_quan
     USING    iv_matid     TYPE /scwm/de_matid
              iv_unit_from TYPE /scwm/de_unit
              iv_unit_to   TYPE /scwm/de_unit
              iv_batchid   TYPE /scwm/de_batchid
     CHANGING cv_quan      TYPE /scwm/de_quantity.

  DATA: lv_quan   TYPE /scwm/de_quantity.

  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
        EXPORTING
          iv_matid     = iv_matid
          iv_quan      = cv_quan
          iv_unit_from = iv_unit_from
          iv_unit_to   = iv_unit_to
          iv_batchid   = iv_batchid
        IMPORTING
          ev_quan      = lv_quan.
    CATCH /scwm/cx_md_interface
          /scwm/cx_md_batch_required
          /scwm/cx_md_internal_error
          /scwm/cx_md_batch_not_required
          /scwm/cx_md_material_exist.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  MOVE lv_quan TO cv_quan.

ENDFORM.                    " convert_quan
*&---------------------------------------------------------------------*
*&      Form  DELIVLOCK_HANDLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delivlock_handle
   USING iv_pmatid   TYPE /scwm/de_rf_pmatid.

  gv_dlvlockusr = sy-msgv1.
  gv_pmatid     = iv_pmatid.
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_call_stack_optimizer
    EXPORTING
      iv_step = 'ULCRHU'.
  /scwm/cl_rf_bll_srvc=>set_prmod(
                   /scwm/cl_rf_bll_srvc=>c_prmod_background ).
  /scwm/cl_rf_bll_srvc=>set_fcode('GTLOCK').
ENDFORM.                    " DELIVLOCK_HANDLE
