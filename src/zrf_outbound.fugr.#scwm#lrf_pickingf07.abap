*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PICKINGF07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_HU_QUAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_HU_VERIFIED  text
*      <--ORDIM_CONFIRM  text
*----------------------------------------------------------------------*
FORM set_partial_pick
  USING
      iv_hu_verified  TYPE xfeld
  CHANGING
      ordim_confirm   TYPE /scwm/s_rf_ordim_confirm.

  DATA:
    lt_huitm          TYPE /scwm/tt_huitm_int,
    ls_huitm          TYPE /scwm/s_huitm_int,
    ls_huhdr          TYPE /scwm/s_huhdr_int,
    lv_vlenr_verif_hu TYPE /scwm/de_huident,
    lv_quantity       TYPE /lime/quantity,
    lv_huitm_quan     TYPE /lime/quantity,
    lv_quan_altme     TYPE /lime/quantity,
    lv_quan_diff      TYPE /lime/quantity,
    lv_flg_mixed_hu   TYPE xfeld,
    ls_t331           TYPE /scwm/t331,
    lv_flg_suom       TYPE /scwm/de_flg_suom,
    lv_data_entry     TYPE /scwm/de_data_entry.


* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ordim_confirm-vlenr_verif
    IMPORTING
      output = lv_vlenr_verif_hu.

* Read HU and get guid
  CALL FUNCTION '/SCWM/HU_READ'
    EXPORTING
      iv_lgnum   = ordim_confirm-lgnum
      iv_huident = lv_vlenr_verif_hu
    IMPORTING
      ev_mix     = lv_flg_mixed_hu
      es_huhdr   = ls_huhdr
      et_huitm   = lt_huitm
    EXCEPTIONS
      deleted    = 1
      not_found  = 2
      error      = 3
      OTHERS     = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR lv_quantity.
  LOOP AT lt_huitm INTO ls_huitm
    WHERE guid_stock0 = ordim_confirm-guid_stock0.
    lv_quantity = lv_quantity + ls_huitm-quan - ls_huitm-resq.
  ENDLOOP.

  lv_huitm_quan = lv_quantity.

* convert UoM of HU into AUoM of WT, if necessary.
  IF lv_quantity IS NOT INITIAL.
    READ TABLE lt_huitm INTO ls_huitm
      WITH KEY guid_stock0 = ordim_confirm-guid_stock0.

    IF ls_huitm-meins <> ls_huitm-altme AND
       ordim_confirm-altme = ls_huitm-meins AND
       lv_data_entry IS INITIAL. " do not change the AUoM for PbV
*     check for SUoM - display ALTME of HUITM
      CALL FUNCTION '/SCWM/SUOM_CHECK'
        EXPORTING
          iv_lgnum    = ordim_confirm-lgnum
          iv_matid    = ls_huitm-matid
          iv_uom      = ls_huitm-altme
        IMPORTING
          ev_flg_suom = lv_flg_suom.
      IF lv_flg_suom IS NOT INITIAL.
        ordim_confirm-altme = ls_huitm-altme.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = ls_huitm-matid
                iv_quan      = ordim_confirm-vsola
                iv_unit_from = ls_huitm-meins
                iv_unit_to   = ordim_confirm-altme
                iv_batchid   = ls_huitm-batchid
              IMPORTING
                ev_quan      = ordim_confirm-vsola.

            ordim_confirm-vsola_chr = ordim_confirm-vsola.

          CATCH /scwm/cx_md.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDTRY.
      ENDIF.
    ENDIF.

    IF ls_huitm-meins <> ordim_confirm-altme.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ls_huitm-matid
              iv_quan      = lv_quantity
              iv_unit_from = ls_huitm-meins
              iv_unit_to   = ordim_confirm-altme
              iv_batchid   = ls_huitm-batchid
            IMPORTING
              ev_quan      = lv_quan_altme.
        CATCH /scwm/cx_md.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.
      lv_quantity = lv_quan_altme.
    ENDIF.
  ENDIF.

  IF lv_quantity <= 0.
*   need to clean the srcHU field in case of bulk scenario
    /scwm/cl_rf_bll_srvc=>set_clear_field( 'X' ).
    MESSAGE e050 WITH ordim_confirm-vlenr_verif.
  ENDIF.

* HU contains less qty than WT, and the scanned HU is a topHU
* then provide a partial confirmation.
  IF lv_quantity < ordim_confirm-vsola AND
     lv_quantity IS NOT INITIAL AND
     ( ls_huhdr-top IS NOT INITIAL OR
       iv_hu_verified IS NOT INITIAL ).

    IF lv_quan_altme IS NOT INITIAL.
*     convert back to BUoM to check if rounding issue can happen
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ls_huitm-matid
              iv_quan      = lv_quan_altme
              iv_unit_from = ordim_confirm-altme
              iv_unit_to   = ls_huitm-meins
              iv_batchid   = ls_huitm-batchid
            IMPORTING
              ev_quan      = lv_quantity.
        CATCH /scwm/cx_md_interface
              /scwm/cx_md_batch_required
              /scwm/cx_md_internal_error
              /scwm/cx_md_batch_not_required
              /scwm/cx_md_material_exist.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

*     check if there is difference
      lv_quan_diff = abs( lv_huitm_quan - lv_quantity ).
      lv_quan_diff = /qos/cl_qty_aux=>round_qty( lv_quan_diff ).
      IF lv_quan_diff IS NOT INITIAL.
*       patial confirmation in base UoM
        lv_quantity = lv_huitm_quan.
        ordim_confirm-altme = ordim_confirm-meins.
        ordim_confirm-vsola_chr = ordim_confirm-vsolm.
        ordim_confirm-vsolm = lv_quantity.
      ELSE.
        lv_quantity = lv_quan_altme.
      ENDIF.
    ENDIF.

    ordim_confirm-vsola = lv_quantity.
    ordim_confirm-vsola_chr = lv_quantity.
    ordim_confirm-parti     = gc_xfeld.
    ordim_confirm-nista = lv_quantity.

*   the partial picking update the qty, therefore we must
*   update the PbV related prompts also.
    IF lv_data_entry IS NOT INITIAL.
      PERFORM pbv_qty_update USING ordim_confirm.
    ENDIF.

*   If quantity of HU is less required quantity AND
*   HU is not nested or mixed we set the HU withdrawal flag
    IF lv_flg_mixed_hu IS INITIAL.
*     do not set HUENT if there is dest HU, because it would
*     create nested HU in the destination
      IF ordim_confirm-nlenr IS INITIAL AND
         ordim_confirm-pickhu IS INITIAL.
        ordim_confirm-huent = /scmb/cl_c=>boole_true.
      ENDIF.
    ENDIF.
  ENDIF.
* The destination bin does not accept HU then
* the HU withdrawal must not be set.
  IF ordim_confirm-huent = /scmb/cl_c=>boole_true.
    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ordim_confirm-lgnum
        iv_lgtyp  = ordim_confirm-nltyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF ls_t331-huobl = 'Y'.
      ordim_confirm-huent = /scmb/cl_c=>boole_false.
    ENDIF.
  ENDIF.

  ordim_confirm-vlenr = lv_vlenr_verif_hu.

ENDFORM.                    " SET_PARTIAL_PICK


FORM set_partial_combined_pick
  USING
      iv_hu_verified  TYPE xfeld
      iv_step         TYPE /scwm/de_step
  CHANGING
      ordim_confirm   TYPE /scwm/s_rf_ordim_confirm
      tt_ordim_confirm  TYPE /scwm/tt_rf_ordim_confirm.

  DATA:
    lt_huitm          TYPE /scwm/tt_huitm_int,
    ls_huitm          TYPE /scwm/s_huitm_int,
    ls_huhdr          TYPE /scwm/s_huhdr_int,
    lv_vlenr_verif_hu TYPE /scwm/de_huident,
    lv_quantity       TYPE /lime/quantity,
    lv_quantity_dis   TYPE /lime/quantity,
    lv_huitm_quan     TYPE /lime/quantity,
    lv_quan_altme     TYPE /lime/quantity,
    lv_quan_diff      TYPE /lime/quantity,
    lv_flg_mixed_hu   TYPE xfeld,
    ls_t331           TYPE /scwm/t331,
    lv_flg_suom       TYPE /scwm/de_flg_suom,
    lv_data_entry     TYPE /scwm/de_data_entry,
    ls_comb_tanum     TYPE /scwm/s_rf_comb_tanum.


  FIELD-SYMBOLS:
        <ordim_confirm> TYPE /scwm/s_rf_ordim_confirm.


* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ordim_confirm-vlenr_verif
    IMPORTING
      output = lv_vlenr_verif_hu.

* Read HU and get guid
  CALL FUNCTION '/SCWM/HU_READ'
    EXPORTING
      iv_lgnum   = ordim_confirm-lgnum
      iv_huident = lv_vlenr_verif_hu
    IMPORTING
      ev_mix     = lv_flg_mixed_hu
      es_huhdr   = ls_huhdr
      et_huitm   = lt_huitm
    EXCEPTIONS
      deleted    = 1
      not_found  = 2
      error      = 3
      OTHERS     = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR lv_quantity.
  LOOP AT lt_huitm INTO ls_huitm
    WHERE guid_stock0 = ordim_confirm-guid_stock0.
    lv_quantity = lv_quantity + ls_huitm-quan - ls_huitm-resq.
  ENDLOOP.

  lv_huitm_quan = lv_quantity.

* convert UoM of HU into AUoM of WT, if necessary.
  IF lv_quantity IS NOT INITIAL.
    READ TABLE lt_huitm INTO ls_huitm
      WITH KEY guid_stock0 = ordim_confirm-guid_stock0.
* lv_quantity is in ls_huitm-meins
* the result of convertion: everything goes to ls_huitm-altme.
    IF ls_huitm-meins <> ls_huitm-altme AND
       ordim_confirm-altme = ls_huitm-meins AND
       lv_data_entry IS INITIAL. " do not change the AUoM for PbV
*     check for SUoM - display ALTME of HUITM
      CALL FUNCTION '/SCWM/SUOM_CHECK'
        EXPORTING
          iv_lgnum    = ordim_confirm-lgnum
          iv_matid    = ls_huitm-matid
          iv_uom      = ls_huitm-altme
        IMPORTING
          ev_flg_suom = lv_flg_suom.
      IF lv_flg_suom IS NOT INITIAL.
        ordim_confirm-altme = ls_huitm-altme.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = ls_huitm-matid
                iv_quan      = ordim_confirm-combqty
                iv_unit_from = ls_huitm-meins
                iv_unit_to   = ordim_confirm-altme
                iv_batchid   = ls_huitm-batchid
              IMPORTING
                ev_quan      = ordim_confirm-combqty.

            ordim_confirm-combqty_chr = ordim_confirm-combqty.

          CATCH /scwm/cx_md.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDTRY.
      ENDIF.
    ENDIF.

    IF ls_huitm-meins <> ordim_confirm-altme.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ls_huitm-matid
              iv_quan      = lv_quantity
              iv_unit_from = ls_huitm-meins
              iv_unit_to   = ordim_confirm-altme
              iv_batchid   = ls_huitm-batchid
            IMPORTING
              ev_quan      = lv_quan_altme.
        CATCH /scwm/cx_md.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.
      lv_quantity = lv_quan_altme.
    ENDIF.
  ENDIF.

* HU contains less qty than combined quantity, and the scanned HU is a topHU
* then provide a partial confirmation.
  IF lv_quantity < ordim_confirm-combqty AND
     lv_quantity IS NOT INITIAL AND
     ( ls_huhdr-top IS NOT INITIAL OR
       iv_hu_verified IS NOT INITIAL ).

    IF lv_quan_altme IS NOT INITIAL.
*     convert back to BUoM to check if rounding issue can happen
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ls_huitm-matid
              iv_quan      = lv_quan_altme
              iv_unit_from = ordim_confirm-altme
              iv_unit_to   = ls_huitm-meins
              iv_batchid   = ls_huitm-batchid
            IMPORTING
              ev_quan      = lv_quantity.
        CATCH /scwm/cx_md_interface
              /scwm/cx_md_batch_required
              /scwm/cx_md_internal_error
              /scwm/cx_md_batch_not_required
              /scwm/cx_md_material_exist.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

*     check if there is difference
      lv_quan_diff = abs( lv_huitm_quan - lv_quantity ).
      lv_quan_diff = /qos/cl_qty_aux=>round_qty( lv_quan_diff ).
      IF lv_quan_diff IS NOT INITIAL.
*       patial confirmation in base UoM
        lv_quantity = lv_huitm_quan.
        ordim_confirm-altme = ordim_confirm-meins.
        ordim_confirm-vsola = ordim_confirm-vsolm.
      ELSE.
        lv_quantity = lv_quan_altme.
      ENDIF.
    ENDIF.

    ordim_confirm-combqty = lv_quantity.
    ordim_confirm-combqty_chr = lv_quantity.
    ordim_confirm-nista = lv_quantity.

    IF lv_quantity <= ordim_confirm-vsola.
      ordim_confirm-uncomb = abap_true.
*      CALL METHOD /scwm/cl_rf_comb_pick=>set_is_combined
*        EXPORTING
*          iv_is_combined = abap_false.
      IF lv_quantity < ordim_confirm-vsola.
        ordim_confirm-parti = gc_xfeld.
      ENDIF.
      ordim_confirm-vsola = lv_quantity.
      ordim_confirm-vsola_chr = lv_quantity.
      ordim_confirm-nista = ordim_confirm-vsola.
      ordim_confirm-vlenr = lv_vlenr_verif_hu.
      IF iv_step = step_pick_cpmt OR iv_step = step_pbv_cpmt OR
        iv_step = step_source_blcp OR iv_step = step_pbv_blcp.

* Set HU withdrawl flag before jumping back.

*   If quantity of HU is less required quantity AND
*   HU is not nested or mixed we set the HU withdrawal flag
        IF lv_flg_mixed_hu IS INITIAL.
*     do not set HUENT if there is dest HU, because it would
*     create nested HU in the destination
          IF ordim_confirm-nlenr IS INITIAL AND
             ordim_confirm-pickhu IS INITIAL.
            ordim_confirm-huent = /scmb/cl_c=>boole_true.
          ENDIF.
        ENDIF.
* The destination bin does not accept HU then
* the HU withdrawal must not be set.
        IF ordim_confirm-huent = /scmb/cl_c=>boole_true.
          CALL FUNCTION '/SCWM/T331_READ_SINGLE'
            EXPORTING
              iv_lgnum  = ordim_confirm-lgnum
              iv_lgtyp  = ordim_confirm-nltyp
            IMPORTING
              es_t331   = ls_t331
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          IF ls_t331-huobl = 'Y'.
            ordim_confirm-huent = /scmb/cl_c=>boole_false.
          ENDIF.
        ENDIF.

        MODIFY tt_ordim_confirm FROM ordim_confirm
            TRANSPORTING uncomb parti vsola vsola_chr nista vlenr huent WHERE tanum = ordim_confirm-tanum.

        IF iv_step = step_pbv_cpmt.
          /scwm/cl_rf_bll_srvc=>set_prmod(
                        /scwm/cl_rf_bll_srvc=>c_prmod_background ).
          /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_pick_mtto ).
        ELSEIF iv_step = step_pbv_blcp.
          /scwm/cl_rf_bll_srvc=>set_prmod(
                        /scwm/cl_rf_bll_srvc=>c_prmod_background ).
          /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_pick_blmt ).
        ENDIF.

        RETURN.
      ENDIF.
    ENDIF.

* Split is not reqd if HU qty fulfills the WT qty.
    IF ordim_confirm-combqty NE lv_quantity.
      ordim_confirm-parti     = gc_xfeld.
    ENDIF.
    ordim_confirm-combqty = lv_quantity.
    ordim_confirm-combqty_chr = lv_quantity.


* Distribute partial confirm for WT in combined group.
    lv_quantity_dis = lv_quantity.
    LOOP AT ordim_confirm-comb_tanums INTO ls_comb_tanum.
      READ TABLE tt_ordim_confirm ASSIGNING <ordim_confirm> WITH KEY tanum = ls_comb_tanum-comb_tanum.
      IF sy-subrc = 0.
        IF lv_quantity_dis > <ordim_confirm>-vsola.
          <ordim_confirm>-nista = <ordim_confirm>-vsola.
          lv_quantity_dis = lv_quantity_dis - <ordim_confirm>-vsola.
        ELSEIF lv_quantity_dis = <ordim_confirm>-vsola.
          <ordim_confirm>-nista = <ordim_confirm>-vsola.
          lv_quantity_dis = lv_quantity_dis - <ordim_confirm>-vsola.
        ELSEIF lv_quantity_dis > 0.
          IF ordim_confirm-combqty NE lv_quantity.
            <ordim_confirm>-parti = gc_xfeld.
          ENDIF.
          <ordim_confirm>-vsola = lv_quantity_dis.
          <ordim_confirm>-vsola_chr = lv_quantity_dis.
          <ordim_confirm>-nista = <ordim_confirm>-vsola.
          lv_quantity_dis = lv_quantity_dis - <ordim_confirm>-vsola.
        ELSE.
*          <ordim_confirm>-parti = gc_xfeld.
*          <ordim_confirm>-vsola = 0.
*          <ordim_confirm>-vsola_chr = <ordim_confirm>-vsola.
*          <ordim_confirm>-nista = 0.
          DELETE ordim_confirm-comb_tanums WHERE comb_tanum >= <ordim_confirm>-tanum.
          DESCRIBE TABLE ordim_confirm-comb_tanums LINES ordim_confirm-samsorce.
          CLEAR <ordim_confirm>-comb_tanums.
          CLEAR <ordim_confirm>-samsorce.
        ENDIF.
        IF <ordim_confirm>-tanum = ordim_confirm-tanum.
          ordim_confirm-parti = <ordim_confirm>-parti.
          ordim_confirm-vsola = <ordim_confirm>-vsola.
          ordim_confirm-vsola_chr = <ordim_confirm>-vsola_chr.
          ordim_confirm-nista = <ordim_confirm>-nista.
        ENDIF.
      ENDIF.
    ENDLOOP.

*   the partial picking update the qty, therefore we must
*   update the PbV related prompts also.
    IF lv_data_entry IS NOT INITIAL.
      PERFORM pbv_qty_update USING ordim_confirm.
    ENDIF.

*   If quantity of HU is less required quantity AND
*   HU is not nested or mixed we set the HU withdrawal flag
    IF lv_flg_mixed_hu IS INITIAL.
*     do not set HUENT if there is dest HU, because it would
*     create nested HU in the destination
      IF ordim_confirm-nlenr IS INITIAL AND
         ordim_confirm-pickhu IS INITIAL.
        ordim_confirm-huent = /scmb/cl_c=>boole_true.
      ENDIF.
    ENDIF.
  ENDIF.

* The destination bin does not accept HU then
* the HU withdrawal must not be set.
  IF ordim_confirm-huent = /scmb/cl_c=>boole_true.
    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ordim_confirm-lgnum
        iv_lgtyp  = ordim_confirm-nltyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF ls_t331-huobl = 'Y'.
      ordim_confirm-huent = /scmb/cl_c=>boole_false.
    ENDIF.
  ENDIF.

  ordim_confirm-vlenr = lv_vlenr_verif_hu.
  LOOP AT ordim_confirm-comb_tanums INTO ls_comb_tanum.
    READ TABLE tt_ordim_confirm ASSIGNING <ordim_confirm> WITH KEY tanum = ls_comb_tanum-comb_tanum.
    IF sy-subrc = 0.
      <ordim_confirm>-vlenr = lv_vlenr_verif_hu.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_PARTIAL_COMBINED_PICK



FORM pbv_qty_update USING is_ordim_confirm  TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_grammar    TYPE char256,
        lv_msg        TYPE string,

        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_unit       TYPE t006a,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

  FIELD-SYMBOLS <s_index_pbv>     TYPE /scwm/s_rf_index_pbv.

  lt_index_pbv  = /scwm/cl_rf_bll_srvc=>get_index_pbv( ).
  ls_screlm_pbv = /scwm/cl_rf_bll_srvc=>get_screlm_pbv( ).

  READ TABLE lt_index_pbv ASSIGNING <s_index_pbv>
    WITH KEY index_field    = 'QTY'
             property_type  = 1.
  IF sy-subrc IS INITIAL.
    WRITE is_ordim_confirm-vsola_chr TO lv_grammar.
*   update grammar with the new qty
    ls_screlm_pbv-grammar_6 = lv_grammar.
*   update field prompt qty
    <s_index_pbv>-msgv1 = lv_grammar.

    SELECT SINGLE * FROM t006a
                    INTO ls_unit
                    WHERE msehi = is_ordim_confirm-altme
                      AND spras = sy-langu.

    MESSAGE i554 WITH is_ordim_confirm-vsola_chr ls_unit-msehl INTO lv_msg.
    ls_screlm_pbv-fld_prompt_6 = lv_msg.

*   update the changes back to the RF Framework
    /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).
    /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).
  ENDIF.

ENDFORM.

FORM clear_fields CHANGING cs_ordim_confirm       TYPE /scwm/s_rf_ordim_confirm
                           ct_ordim_confirm       TYPE /scwm/tt_rf_ordim_confirm.

  DATA: lv_data_entry TYPE /scwm/de_data_entry,
        lv_line       TYPE i,

        ls_ordim_o    TYPE /scwm/ordim_o.


  lv_line = /scwm/cl_rf_bll_srvc=>get_line( ).
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  CALL FUNCTION '/SCWM/TO_READ_SINGLE'
    EXPORTING
      iv_lgnum     = cs_ordim_confirm-lgnum
      iv_tanum     = cs_ordim_confirm-tanum
      iv_flglock   = abap_false
    IMPORTING
      es_ordim_o   = ls_ordim_o
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      error        = 4
      OTHERS       = 5.

  IF sy-subrc IS INITIAL.
    CLEAR: cs_ordim_confirm-vlpla_verif,
           cs_ordim_confirm-aisle_verif,
           cs_ordim_confirm-stack_verif,
           cs_ordim_confirm-lvl_v_verif,
           cs_ordim_confirm-nista_verif,
           cs_ordim_confirm-pickhu_verif,
           cs_ordim_confirm-combqty_verif.

    IF ls_ordim_o-vlenr IS INITIAL.
      CLEAR: cs_ordim_confirm-vlenr,
             cs_ordim_confirm-vlenr_verif,
             cs_ordim_confirm-parti.
*     qty may adjusted after HU scanning
      IF cs_ordim_confirm-exc_tab IS INITIAL.
        cs_ordim_confirm-vsola     = ls_ordim_o-vsola.
        cs_ordim_confirm-vsola_chr = cs_ordim_confirm-vsola.
        IF lv_data_entry IS NOT INITIAL.
          PERFORM pbv_qty_update USING cs_ordim_confirm.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR: cs_ordim_confirm-vlenr_verif.
    ENDIF.
    IF ls_ordim_o-batchid IS INITIAL.
      CLEAR: cs_ordim_confirm-batch,
             cs_ordim_confirm-batchid,
             cs_ordim_confirm-rfbatch.
      /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                      gc_scr_elmnt_rfbatch ).
    ENDIF.

    MODIFY ct_ordim_confirm FROM cs_ordim_confirm INDEX lv_line.
    /scwm/cl_rf_bll_srvc=>set_field( space ).
  ENDIF.

ENDFORM.


FORM wt_refresh     USING    iv_line                TYPE i
                    CHANGING ct_ordim_confirm       TYPE /scwm/tt_rf_ordim_confirm.

  DATA: ls_ordim_o            TYPE /scwm/ordim_o.

  FIELD-SYMBOLS <ordim_confirm>     TYPE /scwm/s_rf_ordim_confirm.

  READ TABLE ct_ordim_confirm ASSIGNING <ordim_confirm> INDEX iv_line.
  IF <ordim_confirm> IS ASSIGNED.
    CHECK <ordim_confirm>-tanum IS NOT INITIAL.

    CALL FUNCTION '/SCWM/TO_READ_SINGLE'
    EXPORTING
      iv_lgnum     = <ordim_confirm>-lgnum
      iv_tanum     = <ordim_confirm>-tanum
      iv_flglock   = abap_false
    IMPORTING
      es_ordim_o   = ls_ordim_o
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      error        = 4
      OTHERS       = 5.

    IF sy-subrc IS INITIAL.
      CLEAR <ordim_confirm>.
      MOVE-CORRESPONDING ls_ordim_o TO <ordim_confirm>.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_TO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_comb_grp USING iv_lgnum    TYPE /scwm/de_rf_lgnum
                        iv_who      TYPE /scwm/de_rf_who
               CHANGING cv_comb_grp TYPE /scwm/de_comb_grp.

  DATA: lt_comb_grp TYPE TABLE OF /scwm/de_comb_grp,
        lv_comb_grp TYPE /scwm/de_comb_grp.

  SELECT combination_grp FROM /scwm/ordim_c INTO TABLE lt_comb_grp
                         WHERE lgnum = iv_lgnum
                         AND who   = iv_who.
  SORT lt_comb_grp DESCENDING.

* Get the bigges combination group
  READ TABLE lt_comb_grp INTO lv_comb_grp INDEX 1.
  lv_comb_grp = lv_comb_grp + 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_comb_grp
    IMPORTING
      output = cv_comb_grp.


ENDFORM.
