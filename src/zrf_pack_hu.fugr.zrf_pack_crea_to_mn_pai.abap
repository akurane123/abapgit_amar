FUNCTION zrf_pack_crea_to_mn_pai.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PACK) TYPE  /SCWM/S_RF_PACK
*"     REFERENCE(CS_WRKC) TYPE  /SCWM/TWORKST
*"--------------------------------------------------------------------
  DATA: lo_wm_pack TYPE REF TO /scwm/cl_wm_packing.

  DATA: ls_huhdr TYPE /scwm/s_huhdr_int,
        ls_quan  TYPE /scwm/s_quan..

  DATA: lv_pick_guid_hu TYPE /scwm/guid_hu,
        lv_ship_guid_hu TYPE /scwm/guid_hu,
        lv_nista        TYPE /scwm/s_quan-quan,
        lv_field        TYPE text60,
        lv_exccode      TYPE /scwm/de_exccode,              "#EC NEEDED
        lv_fcode        TYPE /scwm/de_fcode,
        ls_huitm        TYPE /scwm/s_huitm_int,
        lv_cwreq        TYPE char01,
        lt_bapiret      TYPE bapirettab,
        ls_bapiret      LIKE LINE OF lt_bapiret,
        lv_severity     TYPE bapi_mtype.

  DATA: lt_valid_prf TYPE /scwm/tt_valid_prf_ext.

  BREAK-POINT ID /scwm/rf_packing.

  cs_pack-nlenr = cs_pack-rfhu.



  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  lv_field = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).

  CALL FUNCTION '/SCWM/RF_QTY_POSITIVE_CHECK'
    EXPORTING
      iv_qty   = cs_pack-nista_verif
    EXCEPTIONS
      negative = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF ( lv_field = '/SCWM/S_RF_PACK-NISTA_VERIF' OR lv_fcode = gc_fcode_callcw )
     AND cs_pack-cwrel = abap_true.

    MOVE-CORRESPONDING cs_pack TO ls_huitm.
    CALL FUNCTION '/SCWM/CWINPUT_REPACK_WC'
      EXPORTING
        iv_lgnum       = cs_wrkc-lgnum
        iv_workstation = cs_wrkc-workstation
        is_huitm       = ls_huitm
      IMPORTING
        ev_input_req   = lv_cwreq
        et_bapiret     = lt_bapiret
        ev_severity    = lv_severity.


    IF lv_severity = 'E'.
      READ TABLE lt_bapiret INTO ls_bapiret WITH KEY type = 'E'.
    ELSEIF lv_severity = 'A'.
      READ TABLE lt_bapiret INTO ls_bapiret WITH KEY type = 'A'.
    ENDIF.
    IF ls_bapiret IS NOT INITIAL.
      MESSAGE   ID     ls_bapiret-id
                TYPE   ls_bapiret-type
                NUMBER ls_bapiret-number
                WITH   ls_bapiret-message_v1 ls_bapiret-message_v2
                       ls_bapiret-message_v3 ls_bapiret-message_v4.
    ENDIF.

    IF gv_cw_called = abap_false AND lv_cwreq = abap_true.

      " Move to mediator screen.
      /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
      /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_fcmed ).
      RETURN.
    ENDIF.
  ENDIF.

* Get verification profile
  lt_valid_prf = /scwm/cl_rf_bll_srvc=>get_valid_prf( ).



  LOOP AT lt_valid_prf TRANSPORTING NO FIELDS
    WHERE flg_disable NE 'C' AND
          flg_disable NE 'X'.
    EXIT.
  ENDLOOP.

* Special logic for CW
* If we have open verification fields and cursor is on quantiy and we are
*   CW relevant we jump to CW screen
  IF sy-subrc = 0 AND
     lv_field EQ '/SCWM/S_RF_PACK-NISTA_VERIF' AND
     cs_pack-cwrel = abap_true.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
    RETURN.
  ENDIF.

* New logic because of quick ENTER.
* If we have open verification fields we display the screen again.
  IF sy-subrc = 0.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
    RETURN.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
  /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_pashin ).

  PERFORM initialize_manual
           USING    cs_wrkc
           CHANGING lo_wm_pack.

  CALL METHOD lo_wm_pack->/scwm/if_pack_bas~get_hu
    EXPORTING
      iv_huident = cs_pack-nlenr
      iv_lock    = 'X'
    IMPORTING
      es_huhdr   = ls_huhdr
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
    /scwm/cl_pack_view=>msg_error( ).
  ELSE.
    MOVE ls_huhdr-guid_hu TO lv_ship_guid_hu.
  ENDIF.

* during packing the srcHU and destHU must be different
  IF cs_pack-vlenr = cs_pack-nlenr AND
     cs_pack-nlenr IS NOT INITIAL.
    CLEAR cs_pack-rfhu.
    MESSAGE e039(/scwm/rf_de) WITH cs_pack-nlenr.
  ENDIF.

  IF cs_pack-vlenr IS NOT INITIAL.

    CALL METHOD lo_wm_pack->/scwm/if_pack_bas~get_hu
      EXPORTING
        iv_huident = cs_pack-vlenr
      IMPORTING
        es_huhdr   = ls_huhdr
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ELSE.
      MOVE ls_huhdr-guid_hu TO lv_pick_guid_hu.
    ENDIF.

    IF ls_huhdr-vhi = wmegc_vhi_plan.
      MESSAGE e111(/scwm/ui_packing).
    ENDIF.

    CALL METHOD lo_wm_pack->/scwm/if_pack_bas~pack_hu
      EXPORTING
        iv_source_hu = lv_pick_guid_hu
        iv_dest_hu   = lv_ship_guid_hu
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ENDIF.

  ELSE.
* todo: Umpacken einer LP in eine Nach HU

    CASE cs_pack-iprcode.
      WHEN wmegc_iprcode_diff.
        MOVE cs_pack-nista TO lv_nista.
        MOVE cs_pack-exccode TO lv_exccode.
        MOVE lv_nista TO ls_quan-quan.
        MOVE cs_pack-altme TO ls_quan-unit.

        CALL METHOD lo_wm_pack->post_difference
          EXPORTING
            iv_guid_hu    = cs_pack-guid_parent
            iv_guid_stock = cs_pack-guid_stock
            is_quan       = ls_quan
            iv_exccode    = cs_pack-exccode
          EXCEPTIONS
            error         = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      WHEN OTHERS.
        MOVE cs_pack-vsola TO ls_quan-quan.
        MOVE cs_pack-altme TO ls_quan-unit.
        MOVE cs_pack-exccode TO lv_exccode.

    ENDCASE.


    CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'
      EXPORTING
        iv_workcenter = cs_wrkc-workstation
        iv_exccode    = lv_exccode.

    CALL METHOD lo_wm_pack->/scwm/if_pack_bas~repack_stock
      EXPORTING
        iv_dest_hu    = lv_ship_guid_hu
        iv_source_hu  = cs_pack-guid_parent
        iv_stock_guid = cs_pack-guid_stock
        is_quantity   = ls_quan
      EXCEPTIONS
        error         = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ENDIF.


  ENDIF.

* to do: prüfen ob die Nach HU abgeschlossen werden sollte
* /SCWM/TAAREA-MAXPA
* Erreichen des Füllgrades für Gewicht und Volumen

  CALL METHOD lo_wm_pack->/scwm/if_pack_bas~save
    EXPORTING
      iv_commit = 'X'
      iv_wait   = 'X'
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    /scwm/cl_pack_view=>msg_error( ).
  ENDIF.


*********************ATTP Unpack and Pack*****************************
  DATA: lt_child_sn TYPE  zrf_tt_child_sn.
  REFRESH: lt_child_sn.
* Get SSCC Contents
  PERFORM get_sscc_content USING    cs_pack-nlenr
                                    cs_pack-vlenr
                           CHANGING lt_child_sn.
  IF lt_child_sn IS NOT INITIAL.
* Unpack the child serial numbers from destination HU
    PERFORM send_unpack USING lt_child_sn.
* Pack the serial numbers to source HU
    PERFORM send_pack USING lt_child_sn.
  ENDIF.

**********************************************************************

  CLEAR cs_pack.

  PERFORM initialize_manual
           USING    cs_wrkc
           CHANGING lo_wm_pack.

* Set new started_at time (global in RF and global in LDP memory)
  GET TIME STAMP FIELD gv_started_at.
  /scwm/cl_core_rf_memory=>set_started_at( ).

ENDFUNCTION.
