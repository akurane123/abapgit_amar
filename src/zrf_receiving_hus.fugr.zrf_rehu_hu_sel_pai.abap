FUNCTION zrf_rehu_hu_sel_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"     REFERENCE(CT_REHU_HU) TYPE  /SCWM/TT_RF_REHU_HU
*"     REFERENCE(CS_REHU) TYPE  /SCWM/S_RF_ADMIN_REHU
*"     REFERENCE(CS_REHU_PROD) TYPE  /SCWM/S_RF_REHU_PROD
*"     REFERENCE(CT_REHU_PROD) TYPE  /SCWM/TT_RF_REHU_PROD
*"     REFERENCE(CS_REHU_DLV) TYPE  /SCWM/S_RF_UNLO_DOCID
*"----------------------------------------------------------------------

  DATA: lv_fcode      TYPE /scwm/de_fcode.

  DATA: ls_huhdr  TYPE /scwm/s_huhdr_int,
        ls_k_item TYPE  /scdl/s_sp_k_item,
        lt_k_item TYPE  /scdl/t_sp_k_item.

  DATA: lt_huitm                   TYPE /scwm/tt_huitm_int,
        lv_exist                   TYPE xfeld,
        ev_rejected                TYPE boole_d,            "#EC NEEDED
        lt_rcode                   TYPE  /scdl/t_sp_return_code, "#EC NEEDED
        lv_exist_in_other_delivery TYPE xfeld,
        lv_field(60)               TYPE c.

  DATA: lt_hu_guids TYPE /scwm/tt_guid_hu,
        ls_hu_guids TYPE /scwm/s_guid_hu.

  DATA: lo_prd2hum TYPE REF TO /scwm/cl_dlv_prd2hum.

  DATA: lo_ewl_manager       TYPE REF TO /scwm/if_api_lm_ewl_manager,
        lv_timestamp_current TYPE timestamp.

  FIELD-SYMBOLS: <fs_huitm> TYPE /scwm/s_huitm_int.


*----------------------------------------------------------------------*
* Program logic                                                        *
*----------------------------------------------------------------------*

  BREAK-POINT ID /scwm/rf_receiving_hus.

  CLEAR: ct_rehu_prod, cs_rehu_prod, cs_rehu_dlv.

  /scwm/cl_api_factory=>get_service( IMPORTING eo_api = lo_ewl_manager ).


  lv_field = /scwm/cl_rf_bll_srvc=>get_field( ).
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  PERFORM barcode_decoding_hu
    CHANGING cs_rehu_hu.

  /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).

  CALL FUNCTION 'CONVERSION_EXIT_HUID_INPUT'
    EXPORTING
      input  = cs_rehu_hu-vlenr_verif
    IMPORTING
      output = cs_rehu_hu-huident.

  CALL METHOD /scwm/cl_rf_bll_srvc=>get_fcode
    RECEIVING
      rv_fcode = lv_fcode.

  GET TIME STAMP FIELD lv_timestamp_current.

  IF lv_fcode = gc_fcode_backf.

    IF lo_ewl_manager->is_standard_skipped( ) = abap_true OR
       cs_rehu-procs IS INITIAL OR
       cs_rehu-prr_id IS INITIAL.
      CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
        EXPORTING
          iv_fcode = gc_fcode_updbck.
      RETURN.
    ENDIF.

    "Close and save current EWL
    "function code 'back' means end of transaction
    "-> commit the EWL as well
    lo_ewl_manager->close( iv_end_actual = lv_timestamp_current ).

    lo_ewl_manager->save( ).
    COMMIT WORK AND WAIT.
    lo_ewl_manager->cleanup( ).

    CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
      EXPORTING
        iv_fcode = gc_fcode_updbck.
    RETURN.
  ENDIF.

  CASE lv_fcode .

    WHEN gc_fcode_gtnwhu.
      PERFORM check_hu_exists
        USING cs_rehu_hu-huident
        CHANGING lv_exist.

      IF lv_exist = 'X'.
        MESSAGE e420(/scwm/rf_en) WITH cs_rehu_hu-huident.
      ENDIF.
      IF cs_rehu_hu-huident IS NOT INITIAL.
        PERFORM hu_number_range_check
          USING cs_rehu_hu
                cs_rehu-lgnum.
      ENDIF.
      CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
        EXPORTING
          iv_fcode = gc_fcode_gtnwhu.
      IF lo_ewl_manager->is_standard_skipped( ) = abap_true OR
         cs_rehu-procs IS INITIAL OR
         cs_rehu-prr_id IS INITIAL.
        RETURN.
      ENDIF.
      "Close and save current EWL
      lo_ewl_manager->close(
        EXPORTING
          iv_end_actual = lv_timestamp_current
        IMPORTING
          eo_message = DATA(lo_message) ).
      "Open new EWL
      lo_ewl_manager->open(
      EXPORTING
        iv_lgnum        = cs_rehu-lgnum
        iv_procs        = cs_rehu-procs
        iv_prr_id       = cs_rehu-prr_id
        iv_start_actual = lv_timestamp_current ).
      RETURN.

    WHEN OTHERS.
** check if the Material is serialized
* Validate External HU from ATTP system
      PERFORM validate_hu_attp USING cs_rehu cs_rehu_hu.

  ENDCASE.

  READ TABLE cs_rehu-hus WITH KEY huident = cs_rehu_hu-huident
                         TRANSPORTING NO FIELDS.

  IF sy-subrc = 0.

    READ TABLE cs_rehu-hus INTO cs_rehu_hu
                           WITH KEY huident = cs_rehu_hu-huident
                                    .
    IF sy-subrc NE 0.
      MESSAGE e415(/scwm/rf_en).
    ENDIF.

*   CREATE OBJECT lo_prd2hum.
    IF lo_prd2hum IS NOT BOUND.
      CREATE OBJECT lo_prd2hum.
    ENDIF.

    TRY.
        CALL METHOD lo_prd2hum->check_cross_hu
          EXPORTING
            iv_guid_hu  = cs_rehu_hu-guid_hu
          IMPORTING
            ev_cross_hu = cs_rehu_hu-cross_hu.
      CATCH
        /scdl/cx_delivery.                              "#EC NO_HANDLER

    ENDTRY.

*   refresh HU memory for given HU because qty conversion could
*   lead to wrong data
    MOVE cs_rehu_hu-guid_hu TO ls_hu_guids-guid_hu.
    APPEND ls_hu_guids TO lt_hu_guids.

    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'
      EXPORTING
        it_hu_guids = lt_hu_guids.

    PERFORM hu_read_and_lock
      USING    cs_rehu_hu-huident
               cs_rehu-lgnum
               cs_rehu-deliveries
               abap_false
      CHANGING ls_huhdr
               lt_huitm
               cs_rehu_hu-hazmat_ind
               cs_rehu_hu-rdoccat
               cs_rehu_hu-rdocid.

    CLEAR: ct_rehu_prod, cs_rehu_prod.

    PERFORM fill_prod_data_from_huitem
      USING    cs_rehu-lgnum
               lt_huitm
               cs_rehu_hu-huident
      CHANGING cs_rehu_prod
               ct_rehu_prod.

    CALL METHOD /scwm/cl_rf_bll_srvc=>set_fcode
      EXPORTING
        iv_fcode = gc_fcode_gtexhu.

    IF lo_ewl_manager->is_standard_skipped( ) = abap_true OR
       cs_rehu-procs IS INITIAL OR
       cs_rehu-prr_id IS INITIAL.
      RETURN.
    ENDIF.

    "Close and save current EWL
    lo_ewl_manager->close(
      EXPORTING
        iv_end_actual = lv_timestamp_current
      IMPORTING
        eo_message = lo_message ).

    "Open new EWL
    lo_ewl_manager->open(
    EXPORTING
      iv_lgnum        = cs_rehu-lgnum
      iv_procs        = cs_rehu-procs
      iv_prr_id       = cs_rehu-prr_id
      iv_start_actual = lv_timestamp_current ).

    RETURN.

  ELSE.

    IF cs_rehu_hu-huident IS INITIAL.

      /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).

    ELSE.
* Validate External HU from ATTP system
      PERFORM validate_hu_attp USING cs_rehu cs_rehu_hu.

      PERFORM check_for_other_delivery
        USING cs_rehu_hu-huident
              cs_rehu-lgnum
        CHANGING lv_exist_in_other_delivery.

      IF lv_exist_in_other_delivery IS NOT INITIAL.
        MESSAGE e430(/scwm/rf_en) WITH cs_rehu_hu-huident.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_gtnwhu ).

      IF lo_ewl_manager->is_standard_skipped( ) = abap_true OR
         cs_rehu-procs IS INITIAL OR
         cs_rehu-prr_id IS INITIAL.
        RETURN.
      ENDIF.

      "Close and save current EWL
      lo_ewl_manager->close(
        EXPORTING
          iv_end_actual = lv_timestamp_current
        IMPORTING
          eo_message = lo_message ).

      "Open new EWL
      lo_ewl_manager->open(
      EXPORTING
        iv_lgnum        = cs_rehu-lgnum
        iv_procs        = cs_rehu-procs
        iv_prr_id       = cs_rehu-prr_id
        iv_start_actual = lv_timestamp_current ).

    ENDIF.

  ENDIF.

ENDFUNCTION.
