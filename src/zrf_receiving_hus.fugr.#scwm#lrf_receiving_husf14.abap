*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF14 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fill_prod_data_from_huitem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_HUITM  text
*      <--CS_REHU_PROD  text
*      <--CT_REHU_PROD  text
*----------------------------------------------------------------------*
FORM fill_prod_data_from_huitem  USING    iv_lgnum      TYPE /scwm/lgnum
                                          it_huitm      TYPE /scwm/tt_huitm_int
                                          iv_huident    TYPE /scwm/de_rf_huident
                                 CHANGING cs_rehu_prod  TYPE /scwm/s_rf_rehu_prod
                                          ct_rehu_prod  TYPE /scwm/tt_rf_rehu_prod.

  DATA: lo_dlv     TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_msg     TYPE REF TO /scwm/cl_dm_message_no.      "#EC NEEDED

  DATA: lt_items        TYPE /scwm/dlv_item_out_prd_tab,
        lt_doc          TYPE /scwm/dlv_docid_item_tab.

  DATA: ls_items        TYPE /scwm/dlv_item_out_prd_str,
        ls_mat_global   TYPE  /scwm/s_material_global,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_doc          TYPE /scwm/dlv_docid_item_str,
        ls_mat_lgnum    TYPE /scwm/s_material_lgnum.

  DATA: lo_packing      TYPE REF TO /scwm/cl_wm_packing,
        lt_huhdr        TYPE        /scwm/tt_huhdr_int,
        ls_huhdr        TYPE        /scwm/s_huhdr_int.

  FIELD-SYMBOLS: <fs_huitm> type /scwm/s_huitm_int.


  IF lo_packing IS NOT BOUND.
    CREATE OBJECT lo_packing.
  ENDIF.

* read all necessary data: HU status
  CALL METHOD lo_packing->get_hu
    EXPORTING
      iv_huident = iv_huident
    IMPORTING
      et_huhdr   = lt_huhdr
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.
* ensures the only needed data is read:
*  ls_include_data-item_status = 'X'.

  LOOP AT it_huitm ASSIGNING <fs_huitm>.
    ls_doc-doccat = <fs_huitm>-qdoccat.
    ls_doc-docid  = <fs_huitm>-qdocid.
    ls_doc-itemid = <fs_huitm>-qitmid.
    APPEND ls_doc TO lt_doc.
  ENDLOOP.

  TRY.
*   read delivery
    CALL METHOD lo_dlv->query
      EXPORTING
        it_docid        = lt_doc
        is_read_options = ls_read_options
        is_include_data = ls_include_data
      IMPORTING
        et_items        = lt_items.
    CATCH /scdl/cx_delivery .                           "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  LOOP AT it_huitm ASSIGNING <fs_huitm>.

    READ TABLE lt_items INTO ls_items WITH KEY doccat = <fs_huitm>-qdoccat
                                               docid  = <fs_huitm>-qdocid
                                               itemid = <fs_huitm>-qitmid.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <fs_huitm> TO cs_rehu_prod.

    cs_rehu_prod-procty = ls_items-sapext-/scwm/procty.
    cs_rehu_prod-gmbin  = ls_items-sapext-/scwm/gmbin.
    cs_rehu_prod-charg  = ls_items-product-batchno.
    cs_rehu_prod-matnr  = ls_items-product-productno.
    cs_rehu_prod-itemno = ls_items-itemno.
    cs_rehu_prod-vsola  = <fs_huitm>-quana.
    cs_rehu_prod-rdocid = ls_items-docid.
    cs_rehu_prod-rdoccat = <fs_huitm>-qdoccat.
    cs_rehu_prod-ritmid = ls_items-itemid.
    cs_rehu_prod-guid_stock = <fs_huitm>-guid_stock.
    cs_rehu_prod-dlvno = ls_items-docno.

    READ TABLE lt_huhdr INTO ls_huhdr
         WITH KEY guid_hu = <fs_huitm>-guid_parent.

    IF sy-subrc = 0.
      cs_rehu_prod-huident = ls_huhdr-huident.
      cs_rehu_prod-pmat    = ls_huhdr-pmat.
    ELSE.
      cs_rehu_prod-huident = iv_huident.
    ENDIF.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = cs_rehu_prod-matid
            iv_lgnum      = iv_lgnum
            iv_entitled   = ls_items-sapext-entitled
          IMPORTING
            es_mat_global = ls_mat_global
            es_mat_lgnum  = ls_mat_lgnum.
      CATCH /scwm/cx_md_interface
      /scwm/cx_md_material_exist
      /scwm/cx_md_mat_lgnum_exist
      /scwm/cx_md_lgnum_locid
      /scwm/cx_md.                                      "#EC NO_HANDLER
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDTRY.

    cs_rehu_prod-maktx  = ls_mat_global-maktx.

    BREAK-POINT ID /scwm/suom.

    IF ls_mat_lgnum-puom_wh IS NOT INITIAL.
      cs_rehu_prod-ruom = ls_mat_lgnum-puom_wh.
    ELSE.
      cs_rehu_prod-ruom = ls_mat_global-puom.
    ENDIF.

    APPEND cs_rehu_prod TO ct_rehu_prod.
  ENDLOOP.

ENDFORM.                    " fill_prod_data_from_huitem
