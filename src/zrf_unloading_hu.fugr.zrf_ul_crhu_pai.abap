FUNCTION zrf_ul_crhu_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_UNLO) TYPE  /SCWM/S_RF_UNLO
*"     REFERENCE(CS_ADMIN_UNLO) TYPE  /SCWM/S_RF_ADMIN_UNLO
*"     REFERENCE(CS_UNLO_PROD) TYPE  /SCWM/S_RF_UNLO_PROD
*"     REFERENCE(CT_UNLO_DLV) TYPE  /SCWM/TT_RF_UNLO_DOCID
*"----------------------------------------------------------------------

  BREAK-POINT ID /scwm/rf_unloading.

  DATA: ls_deliveries   TYPE /scwm/s_rf_unlo_docid,
        ls_docid        TYPE /scwm/dlv_docid_item_str,
        lt_docid        TYPE /scwm/dlv_docid_item_tab,
        lt_items        TYPE /scwm/dlv_item_out_prd_tab,
        ls_items        TYPE /scwm/dlv_hu_prd_str,
        lt_open_items   TYPE /scwm/dlv_hu_prd_tab,
        ls_prod_dlv     TYPE /scwm/s_rf_unlo_prod,
        ls_huhdr        TYPE /scwm/s_huhdr_int,
        lv_index        TYPE i,
        lv_field(60)    TYPE c,
        lv_badi_imp(30) TYPE c,
        lv_fcode        TYPE /scwm/de_fcode,
        lv_new_batch    TYPE xfeld,
        lv_batch_crea   TYPE /scwm/dl_batchcrea,
        ls_mat_global   TYPE  /scwm/s_material_global,
        ls_product      TYPE /scdl/dl_product_str,
        lt_prd_hdr      TYPE /scwm/dlv_header_out_prd_tab,
*        ls_matid_charg TYPE /scwm/s_matid_charg .
        lt_mat_uom      TYPE /scwm/tt_material_uom.

  DATA: lv_humatid TYPE /scwm/de_matid,
        lv_lines   TYPE i,
        lv_char40  TYPE /scwm/de_rf_text.

  DATA lt_batch_attr TYPE /scwm/tt_batch_attr.
  DATA lo_log        TYPE REF TO /scwm/cl_log.

  DATA lv_bbd_req TYPE xfeld.
  DATA lv_batch    TYPE /scdl/dl_batchno.
  DATA ls_batch_md TYPE /scwm/dlv_md_prod_batch_det.
  DATA ls_sapext   TYPE /scdl/dl_sap_dr_item_str.
  DATA lv_act_field(60) TYPE c.

  DATA lv_ean_gtin  TYPE xfeld.

  DATA: lo_dlv       TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_md_access TYPE REF TO /scwm/cl_dlv_md_access,
        lo_badi_crea TYPE REF TO  /scwm/ex_dlv_batch_crea,
        lo_item      TYPE REF TO /scdl/cl_dl_item_write,
        lo_bo        TYPE REF TO /scdl/if_bo,
        lo_bom       TYPE REF TO /scdl/cl_bo_management.

  DATA: lv_failed    TYPE        boole_d,
        lv_answer(1) TYPE        c.

  FIELD-SYMBOLS : <fs_mat_uom> TYPE /scwm/s_material_uom.

  DATA: ls_dlv       TYPE /scwm/s_rf_unlo_docid,
        ls_dlv_item  TYPE /scwm/s_dlv_item_auom_det,
        ls_mat_lgnum TYPE /scwm/s_material_lgnum,
        ls_mat_uom   TYPE /scwm/s_material_uom,
        ls_unit      TYPE t006a,
        ls_item_bc   TYPE /scdl/dl_itype_detail_str,
        ls_tdprf_cat TYPE /scwm/tdprf_cat.
  DATA:
    lv_gtin      TYPE /scmb/mdl_gtin,
    lv_ship_from TYPE /scwm/de_ship_from,
    lv_qty       TYPE /scdl/dl_quantity,
    lv_uom       TYPE /scdl/dl_uom,
    lv_itemtype  TYPE /scdl/dl_itemtype,
    lv_vsola     TYPE /scdl/dl_quantity,
    lv_owner     TYPE /scwm/de_owner,
    lv_entitled  TYPE /scwm/de_entitled,
    lv_pref_uom  TYPE /scwm/de_puom,
    lv_uom_text  TYPE char30,
    lv_disp_prf  TYPE /scwm/de_disp_prf,
    lv_text_max  TYPE i,
    lv_severity  TYPE bapi_mtype,
    lv_msg_view  TYPE /scwm/de_msg_view,
    lv_msg       TYPE text256.
  DATA: lo_badi     TYPE REF TO /scwm/ex_rf_prd_vrf_pref.

  DATA: lv_quan_test TYPE /scwm/de_quantity,
        lv_buom      TYPE meins,
        lv_puom      TYPE /scwm/de_puom.

  STATICS: sv_msg_displayed TYPE xfeld.

  cs_unlo-huident = cs_unlo-rfhu.
  cs_unlo_prod-matnr = cs_unlo_prod-rfprod.
  cs_unlo_prod-charg = cs_unlo_prod-rfbatch.

  lv_field = /scwm/cl_rf_bll_srvc=>get_field( ).
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  /scwm/cl_rf_bll_srvc=>set_prmod( /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).

* Listbox for UoMs
  /scwm/cl_rf_bll_srvc=>init_listbox( '/SCWM/S_RF_UNLO_PROD-UOM' ).

  CALL FUNCTION '/SCWM/RF_QTY_POSITIVE_CHECK'
    EXPORTING
      iv_qty   = cs_unlo_prod-nista
    EXCEPTIONS
      negative = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    CLEAR cs_unlo_prod-nista.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  lv_act_field = /scwm/cl_rf_bll_srvc=>get_act_field( ).

  IF cs_unlo-huident IS NOT INITIAL.
    CALL FUNCTION '/SCWM/HUHEADER_READ'
      EXPORTING
        iv_appl     = wmegc_huappl_wme
        iv_huident  = cs_unlo-huident
      IMPORTING
        es_huheader = ls_huhdr
      EXCEPTIONS
        not_found   = 1
        input       = 2
        deleted     = 3
        OTHERS      = 99.
    IF ls_huhdr IS NOT INITIAL.
      CLEAR: cs_unlo-rfhu.
      MESSAGE e420(/scwm/rf_en) WITH cs_unlo-huident.
    ENDIF.
  ENDIF.
**********************************************************************
** validate SSCC in ATTP***

  IF lv_act_field = '/SCWM/S_RF_UNLO-RFHU'.
** Get the SSCC details
    PERFORM validate_sscc CHANGING cs_unlo-rfhu
                           gv_qty.
    SHIFT gv_matnr LEFT DELETING LEADING '0'.

    IF gv_qty IS NOT INITIAL.
      cs_unlo-huident = cs_unlo-rfhu.
      cs_unlo_prod-rfprod = cs_unlo_prod-matnr = gv_matnr.
      cs_unlo_prod-nista = gv_qty.
      cs_unlo_prod-charg = cs_unlo_prod-rfbatch = gv_lotno.
      cs_unlo_prod-bbdat = gv_datex.
    ELSE.
      CLEAR: cs_unlo-rfhu.
      MESSAGE e003(/sttpec/whs_msg).
    ENDIF.
  ENDIF.
**********************************************************************

  IF cs_unlo_prod-matnr IS INITIAL.
    IF lv_act_field EQ '/SCWM/S_RF_UNLO_PROD-RFPROD'.
      CLEAR: cs_unlo_prod-maktx,
             cs_unlo_prod-uom.
      CALL METHOD /scwm/cl_rf_bll_srvc=>init_listbox
        EXPORTING
          iv_fieldname = '/SCWM/S_RF_UNLO_PROD-UOM'.
      MESSAGE e067(/scwm/rf_de).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_field( '/SCWM/S_RF_UNLO_PROD-RFPROD' ).
      EXIT.
    ENDIF.
  ENDIF.

* check the material
  IF cs_unlo_prod-matnr IS NOT INITIAL.
    PERFORM matid_read_from_matnr
    USING cs_unlo_prod-matnr
    CHANGING cs_unlo_prod-matid
             lv_ean_gtin.

    IF lv_ean_gtin IS NOT INITIAL.
*     If user enters a GTIN with a UoM always this UoM
*       is taken. If user changes manually UoM, it is
*       changed back after a ENTER. Same logic as in Receiv. of HU
      CLEAR cs_unlo_prod-uom.
    ENDIF.

    IF cs_unlo_prod-matid IS INITIAL.
      CLEAR: cs_unlo_prod-maktx, cs_unlo_prod-bbdat,cs_unlo_prod-nista,
             cs_unlo_prod-uom.
      CALL METHOD /scwm/cl_rf_bll_srvc=>init_listbox
        EXPORTING
          iv_fieldname = '/SCWM/S_RF_UNLO_PROD-UOM'.
      CLEAR: cs_unlo_prod-rfprod, cs_unlo_prod-rfbatch.
      MESSAGE e002(/scwm/md).
    ENDIF.
  ENDIF.

*   Try to find right delivery item based on entered product
  PERFORM get_dlv_item_by_matid USING cs_admin_unlo
                                      cs_unlo_prod-matid
                             CHANGING ls_dlv
                                      ls_dlv_item.

  lv_entitled = ls_dlv_item-entitled.

  IF lv_entitled IS INITIAL.
    CALL FUNCTION '/SCWM/ENTITLED_FOR_LGNUM_READ'
      EXPORTING
        iv_lgnum         = cs_admin_unlo-lgnum
      IMPORTING
        ev_entitled_dflt = lv_entitled.
  ENDIF.


* read the material detail ( + UoMs )
  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_unlo_prod-matid
          iv_lgnum      = cs_admin_unlo-lgnum
          iv_entitled   = lv_entitled
        IMPORTING
          es_mat_global = ls_mat_global
          et_mat_uom    = lt_mat_uom.
    CATCH /scwm/cx_md.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

* Set the product's text
  cs_unlo_prod-maktx = ls_mat_global-maktx.

  LOOP AT lt_mat_uom ASSIGNING <fs_mat_uom>.
*   Set the text
    WRITE <fs_mat_uom>-meinh TO lv_char40.
*   Set the listbox
    /scwm/cl_rf_bll_srvc=>insert_listbox(
      iv_fieldname = '/SCWM/S_RF_UNLO_PROD-UOM'
      iv_value = <fs_mat_uom>-meinh
      iv_text = lv_char40 ).
  ENDLOOP.

* Check UoM
  IF cs_unlo_prod-uom IS INITIAL.
    BREAK-POINT ID /scwm/suom.

    PERFORM get_item_bc_by_type USING ls_dlv_item-itemtype
                                      ls_dlv-doccat
                             CHANGING ls_item_bc.

    lv_owner    = ls_dlv_item-owner.
    lv_vsola    = ls_dlv_item-qty.

    lv_ship_from = ls_dlv-partyno.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = cs_unlo_prod-matid
            iv_lgnum      = cs_admin_unlo-lgnum
            iv_entitled   = lv_entitled
          IMPORTING
            es_mat_global = ls_mat_global
            es_mat_lgnum  = ls_mat_lgnum.
      CATCH /scwm/cx_md.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    IF ls_mat_lgnum-puom_wh IS NOT INITIAL.
      lv_pref_uom = ls_mat_lgnum-puom_wh.
    ELSE.
      lv_pref_uom = ls_mat_global-puom.
    ENDIF.
    IF ls_item_bc-use_alt_uom IS INITIAL.
      lv_pref_uom = cs_unlo_prod-uom.
    ENDIF.

*   Frist check if product is GTIN and has a UoM
*   New logic: If MATNR is a GTIN we take the UoM from GTIN
    lv_gtin = cs_unlo_prod-matnr.
    READ TABLE lt_mat_uom ASSIGNING <fs_mat_uom>
      WITH KEY gtin = lv_gtin.
    IF sy-subrc = 0 AND lv_gtin NE 0.
      IF <fs_mat_uom>-meinh IS NOT INITIAL.
        cs_unlo_prod-uom = <fs_mat_uom>-meinh.
      ENDIF.
*     If UoM of GTIN is not the preferred one, give info to user
      IF cs_unlo_prod-uom <> lv_pref_uom AND
         lv_pref_uom IS NOT INITIAL        AND
        lv_fcode = gc_fcode_enter          AND
        sv_msg_displayed IS INITIAL.

*       Call BAdI to control message output
        TRY.
            GET BADI lo_badi
              FILTERS
                lgnum = cs_admin_unlo-lgnum.

            CALL BADI lo_badi->validate
              EXPORTING
                iv_lgnum     = cs_admin_unlo-lgnum
                iv_matnr     = cs_unlo_prod-matnr
                iv_matid     = cs_unlo_prod-matid
                iv_gtin      = lv_gtin
                iv_uom       = cs_unlo_prod-uom
                iv_puom      = lv_pref_uom
                iv_docid     = ls_dlv_item-docid
                iv_itemid    = ls_dlv_item-itemid
                iv_ship_from = lv_ship_from
              IMPORTING
                ev_severity  = lv_severity
                ev_msg_view  = lv_msg_view.

            IF lv_msg_view <> '1'.
              lv_msg_view = '0'.
            ENDIF.

          CATCH cx_badi.                                "#EC NO_HANDLER
        ENDTRY.

        IF lv_severity IS NOT INITIAL.
          sv_msg_displayed = 'X'.

          /scwm/cl_rf_bll_srvc=>message(
               iv_flg_continue_flow = ' '
               iv_msgid             = '/SCWM/RF_EN'
               iv_msgty             = lv_severity
               iv_msgno             = 602
               iv_msgv1             = cs_unlo_prod-uom
               iv_msgv2             = lv_pref_uom
               iv_msg_view          = lv_msg_view
                ).

          IF lv_severity CA 'EA'.
            /scwm/cl_rf_bll_srvc=>set_field( lv_field ).
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   If we didn't found a GTIN, we call the BadI for the operational UoM
    IF cs_unlo_prod-uom IS INITIAL.
*   change the UoM
      IF cs_unlo_prod-matnr <> ls_mat_global-matnr.
        DATA lv_opunit      TYPE /scwm/de_rf_opunit.
        PERFORM opunit_check USING    cs_unlo_prod
                                      cs_admin_unlo
                                      lv_entitled
                             CHANGING lv_opunit.
        IF lv_opunit IS NOT INITIAL.
          cs_unlo_prod-uom = lv_opunit.
        ENDIF.
      ENDIF.
    ENDIF.
*   If we didn't found an operational UoM, we call the BAdI for preferred UoM
    IF cs_unlo_prod-uom IS INITIAL AND
       ls_item_bc-use_alt_uom IS NOT INITIAL.
      cs_unlo_prod-uom = ls_mat_global-meins.

      ls_dlv_item-doccat = ls_dlv_item-doccat.
      ls_dlv_item-docid = ls_dlv_item-docid.
      ls_dlv_item-itemid = ls_dlv_item-itemid.
      ls_dlv_item-itemtype = ls_dlv_item-itemtype.
      ls_dlv_item-productid = ls_mat_global-matid.
      ls_dlv_item-productno = ls_mat_global-matnr.
      ls_dlv_item-qty = lv_vsola.
      ls_dlv_item-uom = ls_dlv_item-uom.
      ls_dlv_item-batchno = cs_unlo_prod-charg.
      ls_dlv_item-entitled = lv_entitled.
      ls_dlv_item-owner = lv_owner.
      ls_dlv_item-procty = ls_dlv_item-procty.

      CALL FUNCTION '/SCWM/AUOM_DET'
        EXPORTING
          iv_lgnum     = cs_admin_unlo-lgnum
          iv_ship_from = lv_ship_from
          is_dlv_item  = ls_dlv_item
        IMPORTING
          ev_qty       = lv_qty
          ev_uom       = lv_uom.

      IF lv_uom IS NOT INITIAL.
        cs_unlo_prod-uom = lv_uom.
        cs_unlo_prod-altme = lv_uom.
        IF cs_unlo_prod-uom IS NOT INITIAL.
*         Check UoM on the UI is correct
          READ TABLE lt_mat_uom
            WITH KEY meinh = cs_unlo_prod-uom
            TRANSPORTING NO FIELDS.

          IF sy-subrc <> 0.
            MESSAGE e700(/scwm/l1) WITH cs_unlo_prod-uom.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   If we still don't have a UoM then take the product base
    IF cs_unlo_prod-uom IS INITIAL.
      cs_unlo_prod-uom = ls_mat_global-meins.
    ENDIF.

* Refill the UoM F8 list again
*   Init listbox
    /scwm/cl_rf_bll_srvc=>init_listbox( '/SCWM/S_RF_UNLO_PROD-UOM' ).

    IF cs_unlo_prod-matid IS INITIAL.
      MESSAGE e002(/scwm/md).
*     Product does not exist
    ENDIF.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = cs_unlo_prod-matid
            iv_lgnum      = cs_admin_unlo-lgnum
            iv_entitled   = lv_entitled
          IMPORTING
            es_mat_global = ls_mat_global
            es_mat_lgnum  = ls_mat_lgnum
            et_mat_uom    = lt_mat_uom.
      CATCH /scwm/cx_md.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    IF ls_mat_lgnum-puom_wh IS NOT INITIAL.
      lv_pref_uom = ls_mat_lgnum-puom_wh.
    ELSE.
      lv_pref_uom = ls_mat_global-puom.
    ENDIF.
    IF ls_item_bc-use_alt_uom IS INITIAL.
      lv_pref_uom = cs_unlo_prod-uom.
    ENDIF.

*   Get the screen width
    lv_disp_prf = /scwm/cl_rf_bll_srvc=>get_disp_prf( ).

    SELECT SINGLE * FROM /scwm/tdprf_cat
                    INTO ls_tdprf_cat
                   WHERE disp_prf = lv_disp_prf.

*   Put preferred on top; sortation is given from //MATERIAL_READ_SINGLE
    READ TABLE lt_mat_uom INTO ls_mat_uom
      WITH KEY meinh = lv_pref_uom.
    IF sy-subrc = 0.
      DELETE lt_mat_uom INDEX sy-tabix.
      INSERT ls_mat_uom INTO lt_mat_uom INDEX 1.
    ENDIF.

*   Calculate max. text length '1.XXX-Text- X'
    lv_text_max = ls_tdprf_cat-width - 10.
    IF lv_text_max > 30. lv_text_max = 30. ENDIF.

*   UoMs to List box
    LOOP AT lt_mat_uom ASSIGNING <fs_mat_uom>.
*     Read the text of the UoM
      SELECT SINGLE * FROM t006a
                      INTO ls_unit
                     WHERE msehi = <fs_mat_uom>-meinh
                       AND spras = sy-langu.
*     Set the text
      WRITE <fs_mat_uom>-meinh TO lv_uom.
      IF ls_unit-msehl IS NOT INITIAL AND
         lv_text_max = 30.
        lv_uom_text = ls_unit-msehl.
      ELSE.
        lv_uom_text = ls_unit-mseht.
      ENDIF.

      IF <fs_mat_uom>-meinh = lv_pref_uom.
        CONCATENATE <fs_mat_uom>-meinh '-' lv_uom_text(lv_text_max) '- X' INTO lv_char40.
      ELSE.
        CONCATENATE <fs_mat_uom>-meinh '-' lv_uom_text(lv_text_max) INTO lv_char40.
      ENDIF.
*     Set the listbox
      /scwm/cl_rf_bll_srvc=>insert_listbox(
        iv_fieldname = '/SCWM/S_RF_UNLO_PROD-UOM'
        iv_value = <fs_mat_uom>-meinh
        iv_text = lv_char40 ).
    ENDLOOP.
  ELSE.
*   Check UoM on the UI is correct
    READ TABLE lt_mat_uom
      WITH KEY meinh = cs_unlo_prod-uom
      TRANSPORTING NO FIELDS.
*   The Typed UoM is not an Alternate UoM for the prod.
    IF sy-subrc <> 0.
      MESSAGE e700(/scwm/l1) WITH cs_unlo_prod-uom.
    ENDIF.

  ENDIF.

  lv_quan_test = cs_unlo_prod-nista.
* Check for integer values only if SUOM is active.
  CALL FUNCTION '/SCWM/MATERIAL_SUOM_RELEVANT'
    EXPORTING
      iv_lgnum   = cs_admin_unlo-lgnum
      iv_matid   = cs_unlo_prod-matid
    IMPORTING
      ev_buom    = lv_buom
      ev_puom    = lv_puom
      et_mat_uom = lt_mat_uom.
* Base unit of measurement is always returned.
* At least if we have 1 UOM of the product defined as SUOM,
* perform the integer check.
  IF lines( lt_mat_uom ) > 1.
    lv_quan_test = frac( lv_quan_test ).
    IF lv_quan_test IS NOT INITIAL.
      MESSAGE e604(/scwm/rf_en).
    ENDIF.
  ENDIF.

  IF ls_mat_global-sled_bbd IS NOT INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_bbdat ).
  ENDIF.

* If batch required then display the charg field.
  IF ls_mat_global-batch_req = abap_true.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_charg ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_charg ).
  ENDIF.

  IF cs_unlo_prod-nista IS INITIAL.
    IF lv_act_field EQ '/SCWM/S_RF_UNLO_PROD-NISTA'.
      MESSAGE e175(/scwm/rf_en).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_field( '/SCWM/S_RF_UNLO_PROD-NISTA' ).
      EXIT.
    ENDIF.
  ENDIF.

  IF ls_mat_global-batch_req = abap_true.
    IF cs_unlo_prod-charg IS INITIAL.
      IF lv_act_field EQ '/SCWM/S_RF_UNLO_PROD-RFBATCH'.
        MESSAGE e206(/scwm/dlv_batch) WITH cs_unlo_prod-matnr.
      ELSE.
        /scwm/cl_rf_bll_srvc=>set_field( '/SCWM/S_RF_UNLO_PROD-RFBATCH' ).
        EXIT.
      ENDIF.
    ELSE.
      lv_batch =  cs_unlo_prod-charg.

      IF lo_md_access IS NOT BOUND.
        lo_md_access = /scwm/cl_dlv_md_access=>get_instance( ).
      ENDIF.

      TRY.
          IF abap_true = lo_md_access->get_batch_existence(
                 iv_productid = cs_unlo_prod-matid
                 iv_batchno   = lv_batch
                 iv_lgnum     = cs_admin_unlo-lgnum
                 iv_entitled  = lv_entitled ).

            ls_batch_md = lo_md_access->get_batch_detail(
                iv_productno         = ls_mat_global-matnr
                iv_batchno           = lv_batch
                iv_lgnum             = cs_admin_unlo-lgnum
                iv_entitled          = lv_entitled
                iv_no_classification = abap_false ).
            IF ls_batch_md-sled_bbd IS NOT INITIAL.
              cs_unlo_prod-bbdat = ls_batch_md-sled_bbd.
            ENDIF.
          ELSE.
            /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_bbdat ).
            lv_new_batch = 'X'.
          ENDIF.
        CATCH /scwm/cx_dlv_batch .
      ENDTRY.
    ENDIF.
  ENDIF.

* If BBD was not entered yet then make it possible
  IF cs_unlo_prod-bbdat IS INITIAL.
    IF lv_act_field NE '/SCWM/S_RF_UNLO_PROD-BBDAT'.
      /scwm/cl_rf_bll_srvc=>set_field( '/SCWM/S_RF_UNLO_PROD-BBDAT' ).
      EXIT.
    ENDIF.
  ENDIF.

* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

* Quickenter check
*  IF lv_field <> gc_shortcut AND lv_fcode = gc_fcode_enter.
*    /scwm/cl_rf_bll_srvc=>set_field( space ).
*    RETURN.
*  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod(
                     /scwm/cl_rf_bll_srvc=>c_prmod_background ).

  LOOP AT cs_admin_unlo-deliveries INTO ls_deliveries.
    ls_docid-docid  = ls_deliveries-docid.
    ls_docid-doccat = wmegc_doccat_pdi.
    APPEND ls_docid TO lt_docid.
  ENDLOOP.

  PERFORM check_for_open_quantity
    USING    cs_admin_unlo-lgnum
             lv_entitled
    CHANGING lt_docid
             cs_unlo_prod
             lt_open_items.

********************************************
* TODO!
* check for EGR
********************************************
  SORT lt_open_items BY prd_id doccat_prd item_id open_qty-qty.
  DELETE ADJACENT DUPLICATES FROM lt_open_items COMPARING prd_id
                                                          doccat_prd.

  CASE lines( lt_open_items ) .
    WHEN 0.
*     Product not exist in the current deliveries with sufficien quantity.
      CLEAR: ct_unlo_dlv.
      ct_unlo_dlv = cs_admin_unlo-deliveries.

*     If the unloading contains only 1 delivery then precheck whether
*     EGR exist for this delivery.
      lv_lines = lines( ct_unlo_dlv ).
      IF lv_lines = 1.
        READ TABLE ct_unlo_dlv INTO ls_deliveries INDEX 1.

        PERFORM check_egr_exists_for_item
          USING cs_admin_unlo-lgnum
                cs_unlo_prod
          CHANGING ls_deliveries
                   cs_admin_unlo
                   lt_prd_hdr
                   lt_items.

        IF lt_items IS INITIAL.
          MESSAGE e491(/scwm/rf_en).
        ENDIF.
      ENDIF.

      CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
        EXPORTING
          iv_fcode = 'GTSLDL'.

    WHEN 1.
*     Product find in sufficient quantity in 1 delivery
      READ TABLE lt_open_items INTO ls_items INDEX 1.

      CLEAR: ls_deliveries.
      READ TABLE cs_admin_unlo-deliveries INTO ls_deliveries
          WITH KEY docid   = ls_items-prd_id
                   doccat  = ls_items-doccat_prd.

      IF sy-subrc = 0.
        ls_deliveries-dlvno   = ls_items-prd_no.
        ls_deliveries-item_id = ls_items-item_id.
        ls_deliveries-uom     = ls_items-open_qty-uom.
      ENDIF.

*     check remaining shelf life
      PERFORM check_min_rem_sl
      USING     cs_unlo_prod
                ls_deliveries
      CHANGING  lv_failed.

      IF lv_failed IS NOT INITIAL.
        lv_answer = /scwm/cl_rf_dynpro_srvc=>display_message(
          iv_msgid           = '/SCWM/RF_EN'
          iv_msgty           = /scwm/cl_rf_bll_srvc=>c_msgty_query
*         Raise message minimum remaining shelf life exceeded.
          iv_msgno           = '511' ).

        IF lv_answer = /scwm/cl_rf_bll_srvc=>c_answer_no.
          /scwm/cl_rf_bll_srvc=>set_prmod(
                /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).
          RETURN.
        ENDIF.
      ENDIF.

      cs_unlo_prod-docid   = ls_items-prd_id.
      cs_unlo_prod-doccat  = ls_items-doccat_prd.
      cs_unlo_prod-item_id = ls_items-item_id.
*      cs_unlo_prod-uom     = ls_items-open_qty-uom.
*     batch is not yet existing - check customizing if allowed to create
      IF lv_new_batch IS NOT INITIAL.
        lo_bom = /scdl/cl_bo_management=>get_instance( ).
        lo_bo = lo_bom->get_bo_by_id( cs_unlo_prod-docid ).
        lo_item ?= lo_bo->get_item( cs_unlo_prod-item_id ).
        ls_product = lo_item->get_product( ).
        ls_sapext = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item ).
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
      ENDIF.
      CLEAR cs_unlo-pmatid.

      PERFORM pack_spec_for_item
        USING    cs_admin_unlo-lgnum
                 cs_unlo_prod
                 cs_admin_unlo-deliveries
        CHANGING cs_unlo-pmatid.

*     If no packspec for the product, the add manually
      IF cs_unlo-pmatid IS INITIAL.
        READ TABLE cs_admin_unlo-deliveries INTO ls_deliveries
          WITH KEY docid  = cs_unlo_prod-docid
                   doccat = cs_unlo_prod-doccat.
*       Mark the delivery
        IF sy-subrc = 0.
          lv_index = sy-tabix.
          ls_deliveries-mark_sel = abap_true.
          MODIFY cs_admin_unlo-deliveries FROM ls_deliveries INDEX lv_index.
        ENDIF.

        CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
          EXPORTING
            iv_fcode = 'GTPMSL'.
        RETURN.
      ENDIF.

      ls_deliveries-docid   = ls_items-prd_id.
      ls_deliveries-doccat  = ls_items-doccat_prd.

      PERFORM pack_item_to_delivery
        USING    ls_deliveries
        CHANGING cs_unlo_prod
                 cs_unlo
                 cs_admin_unlo.

*     The name of the constant is misleading,
*     but in want to use the fcode SYEXC1.
      CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
        EXPORTING
          iv_fcode = gc_fcode_syexc_cb.

    WHEN OTHERS.
*     Product find with sufficient quantity in more deliveries
      CLEAR: ls_deliveries.
      CLEAR ct_unlo_dlv.
      LOOP AT lt_open_items INTO ls_items.
        READ TABLE cs_admin_unlo-deliveries INTO ls_deliveries
            WITH KEY docid   = ls_items-prd_id
                     doccat  = ls_items-doccat_prd.
*        ls_deliveries-docid   = ls_items-prd_id.
*        ls_deliveries-doccat  = ls_items-doccat_prd.
        IF sy-subrc = 0.
          ls_deliveries-dlvno   = ls_items-prd_no.
          ls_deliveries-item_id = ls_items-item_id.
          ls_deliveries-uom     = ls_items-open_qty-uom.
          APPEND ls_deliveries TO ct_unlo_dlv.
        ENDIF.
      ENDLOOP.

      CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
        EXPORTING
          iv_fcode = 'GTSLDL'.
  ENDCASE.



ENDFUNCTION.
