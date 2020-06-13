FUNCTION zrf_pack_hu_close_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PACK) TYPE  /SCWM/S_RF_PACK
*"     REFERENCE(CS_WRKC) TYPE  /SCWM/TWORKST
*"----------------------------------------------------------------------
  DATA: lo_wm_pack TYPE REF TO /scwm/cl_wm_packing.

  DATA: lv_step    TYPE /scwm/de_step.

  DATA: ls_huhdr     TYPE /scwm/s_huhdr_int,
        ls_huhdr_out TYPE /scwm/s_huhdr_int.

  BREAK-POINT ID /scwm/rf_packing.

  PERFORM get_wc
         CHANGING cs_wrkc.

  cs_pack-vlenr = cs_pack-rfhu.

  lv_step = /scwm/cl_rf_bll_srvc=>get_step( ).

  IF cs_wrkc IS NOT INITIAL.
    PERFORM initialize
          USING    cs_wrkc
          CHANGING lo_wm_pack.

    CALL METHOD lo_wm_pack->/scwm/if_pack~get_hu
      EXPORTING
        iv_huident = cs_pack-vlenr
        iv_lock    = 'X'
      IMPORTING
        es_huhdr   = ls_huhdr
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ENDIF.

*   update attributes of HU
    IF lv_step = 'PAHDCH'.
      MOVE cs_pack-g_weight TO ls_huhdr-g_weight.
      MOVE cs_pack-m_weight TO ls_huhdr-max_weight.
      MOVE cs_pack-unit_gw TO ls_huhdr-unit_gw.
      MOVE cs_pack-g_volume TO ls_huhdr-g_volume.
      MOVE cs_pack-m_volume TO ls_huhdr-max_volume.
      MOVE cs_pack-unit_gv TO ls_huhdr-unit_gv.
      MOVE cs_pack-length TO ls_huhdr-length.
      MOVE cs_pack-width TO ls_huhdr-width.
      MOVE cs_pack-height TO ls_huhdr-height.
      MOVE cs_pack-unit_lwh TO ls_huhdr-unit_lwh.
      MOVE cs_pack-dstgrp TO ls_huhdr-dstgrp.
      MOVE cs_pack-logpos TO ls_huhdr-logpos.
      MOVE cs_pack-tolw TO ls_huhdr-tolw.
      MOVE cs_pack-tolv TO ls_huhdr-tolv.
      MOVE cs_pack-g_capa TO ls_huhdr-g_capa.

      CALL METHOD lo_wm_pack->/scwm/if_pack~change_huhdr
        EXPORTING
          is_huhdr = ls_huhdr
        IMPORTING
          es_huhdr = ls_huhdr_out
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.
        /scwm/cl_pack_view=>msg_error( ).
      ENDIF.

      DATA: lo_badi_huchng TYPE REF TO /scwm/ex_wrkc_ui_hu_changed.
      GET BADI lo_badi_huchng
        FILTERS
          lgnum = cs_wrkc-lgnum.

      CALL BADI lo_badi_huchng->hu_header_changed
        EXPORTING
          is_huhdr_old = ls_huhdr
          is_huhdr_new = ls_huhdr_out
          is_wrkc      = cs_wrkc
          iv_model     = lo_wm_pack
        EXCEPTIONS
          OTHERS       = 99.
      IF NOT sy-subrc IS INITIAL.
        /scwm/cl_pack_view=>msg_error( ).
      ENDIF.
    ENDIF.

    CALL METHOD lo_wm_pack->hu_process_completed
      EXPORTING
        iv_hu  = ls_huhdr-guid_hu
      EXCEPTIONS
        error  = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
*     in case of error, release the lock
      CALL FUNCTION 'DEQUEUE_/SCWM/EHU'
        EXPORTING
          mode_/scwm/s_huhdr_int = 'E'
          mandt                  = sy-mandt
          huident                = ls_huhdr-huident
          lgnum                  = cs_wrkc-lgnum
          _scope                 = '2'.

      /scwm/cl_pack_view=>msg_error( ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>message(
*          iv_msg_view          = gc_msg_view_scr
           iv_flg_continue_flow = gc_xfeld
           iv_msgid             = gc_rf_de_msgid
           iv_msgty             = gc_msgty_success
           iv_msgno             = '225'
           iv_msgv1             = ls_huhdr-huident ).
    ENDIF.

    CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'
      EXPORTING
        iv_workcenter = cs_wrkc-workstation.

    CALL METHOD lo_wm_pack->/scwm/if_pack~save
      EXPORTING
        iv_commit = 'X'
        iv_wait   = 'X'
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ELSE.
      CLEAR cs_pack-vlenr.
      CLEAR cs_pack-rfhu.
    ENDIF.

* get instance and initialize log
    PERFORM initialize_manual
           USING    cs_wrkc
           CHANGING lo_wm_pack.


****** ATTP send Picking Event
** check if material is serialized
*    IF gv_matnr IS NOT INITIAL.
*      CLEAR: gv_serial.
*      PERFORM gf_serialized USING gv_matnr
*                            CHANGING gv_serial.
*
*      IF gv_serial IS NOT INITIAL.
** custom code to validate and post pickhu event
*        DATA : iv_activetab   TYPE sy-ucomm,
*               cs_tbox_fields TYPE        /sttpec/s_whs_test_tbox_flds,
*               ct_tbox_resobj TYPE        /sttpec/t_whs_test_tbox_resobj,
*               lv_rdocid      TYPE /scwm/de_docid,
*               lv_docno       TYPE /scdl/dl_docno_int.
*
*
*        iv_activetab = 'PICKHU'.
*        CONCATENATE '(00)' ls_huhdr-huident+2(18) INTO cs_tbox_fields-scan_obj.
**        SELECT SINGLE rdocid FROM /scwm/ordim_o INTO lv_rdocid
**            WHERE tanum = ordim_confirm-tanum
**              AND lgnum = ordim_confirm-lgnum.
**        SELECT SINGLE docno FROM /scdl/db_proch_o INTO lv_docno
**            WHERE docid = lv_rdocid.
**        SHIFT lv_docno LEFT DELETING LEADING '0'.
**        cs_tbox_fields-docnum = lv_docno.
*
**cs_tbox_fields-docyear = '2020'.
**cs_tbox_fields-logsys = 'H01CLNT100'.
**cs_tbox_fields-plant = '2000'.
*        cs_tbox_fields-sscc = ls_huhdr-huident+2(18).
*
*        PERFORM user_command_val USING    iv_activetab
*                              CHANGING cs_tbox_fields
*                                       ct_tbox_resobj.
*        PERFORM send_event USING    iv_activetab
*                           CHANGING cs_tbox_fields
*                                    ct_tbox_resobj.
*      ENDIF.
*    ENDIF.
** confirm the warehosue task for HU
    PERFORM confirm_open_task USING cs_wrkc-lgnum
                                    cs_pack-vlenr.
  ENDIF.

ENDFUNCTION.
