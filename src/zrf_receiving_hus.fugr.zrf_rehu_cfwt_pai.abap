FUNCTION ZRF_REHU_CFWT_PAI.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"     REFERENCE(CT_REHU_HU) TYPE  /SCWM/TT_RF_REHU_HU
*"     REFERENCE(CS_REHU) TYPE  /SCWM/S_RF_ADMIN_REHU
*"     REFERENCE(CS_REHU_PROD) TYPE  /SCWM/S_RF_REHU_PROD
*"     REFERENCE(CT_REHU_PROD) TYPE  /SCWM/TT_RF_REHU_PROD
*"--------------------------------------------------------------------


  DATA: lv_severity TYPE  bapi_mtype.

  DATA: ls_ordim_o  TYPE /scwm/ordim_o,
        ls_exctab   TYPE /scwm/s_rf_exc,
        ls_conf_exc TYPE /scwm/s_conf_exc,
        ls_bapiret  TYPE bapiret2,
        ls_conf_hu  TYPE /scwm/to_conf.

  DATA: lt_conf_exc TYPE /scwm/tt_conf_exc,
        lt_conf_hu  TYPE /scwm/to_conf_tt,
        lt_bapiret  TYPE bapiret2_t.

  DATA lv_fcode     TYPE /scwm/de_fcode.

  FIELD-SYMBOLS: <hus> TYPE /scwm/s_rf_rehu_hu.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

* HU-TO confirm
* lock TO
  PERFORM to_read_lock
          USING    cs_rehu-lgnum
                   cs_rehu_hu-tanum
                   'X'
          CHANGING ls_ordim_o.

  IF lv_fcode = 'BACKF'.
*  The program flow comes here 2 times. After the WT confirmation as well
   IF ls_ordim_o-tanum IS NOT INITIAL. "WT not confirmed
*   Clear LM data set before in /SCWM/RF_REHU_CFWT_PBO
    CALL FUNCTION '/SCWM/RF_REHU_CLEAR_LM_ADMIN'
      EXPORTING
        CS_REHU_HU       = cs_rehu_hu
        CS_REHU          = cs_rehu
    .
   ENDIF.
   EXIT.
  ENDIF.

  MOVE-CORRESPONDING ls_ordim_o TO ls_conf_hu.              "#EC ENHOK

  MOVE: cs_rehu_hu-nlpla     TO ls_conf_hu-nlpla.
  MOVE: ls_ordim_o-vsola     TO ls_conf_hu-nista.

* set start time
  IF ls_conf_hu-started_at IS INITIAL.
    ls_conf_hu-started_at = cs_rehu-started_at.
  ENDIF.

  APPEND ls_conf_hu TO lt_conf_hu.

* fill out exception tab for later confirmation
  LOOP AT cs_rehu_hu-exc_tab INTO ls_exctab.
    MOVE: cs_rehu_hu-tanum      TO ls_conf_exc-tanum,
          ls_exctab-exccode   TO ls_conf_exc-exccode,
          ls_exctab-exec_step TO ls_conf_exc-exec_step,
          wmegc_buscon_tpt      TO ls_conf_exc-buscon,
          ls_conf_hu-papos         TO ls_conf_exc-papos.
    APPEND ls_conf_exc TO lt_conf_exc.
    CLEAR: ls_conf_exc.
  ENDLOOP.
  SORT lt_conf_exc.
  DELETE ADJACENT DUPLICATES FROM lt_conf_exc.

* set data for printing
  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'.

  CALL FUNCTION '/SCWM/TO_CONF_INT_SIMULATE'
    EXPORTING
      iv_lgnum    = cs_rehu-lgnum
      iv_wtcode   = wmegc_wtcode_rsrc
      it_conf     = lt_conf_hu
      it_conf_exc = lt_conf_exc
    IMPORTING
      et_bapiret  = lt_bapiret
      ev_severity = lv_severity.

  IF lv_severity CA wmegc_severity_ea.
    READ TABLE lt_bapiret INTO ls_bapiret
         WITH KEY type = wmegc_severity_err.
    IF sy-subrc = 0.
      MESSAGE ID     ls_bapiret-id
              TYPE   ls_bapiret-type
              NUMBER ls_bapiret-number
              WITH   ls_bapiret-message_v1 ls_bapiret-message_v2
                     ls_bapiret-message_v3 ls_bapiret-message_v4.
*     message from bapiret
    ELSE.
      MESSAGE e037(/scwm/rf_de).
*     putaway not possible
    ENDIF.
  ENDIF.


* confirm the WT related to the HU
  CALL FUNCTION '/SCWM/TO_CONFIRM'
    EXPORTING
      iv_lgnum                    = cs_rehu-lgnum
      iv_wtcode                   = wmegc_wtcode_rsrc
      iv_commit_work              = ' '
      it_conf                     = lt_conf_hu
      it_conf_exc                 = lt_conf_exc
      iv_processor_det = 'X'
   IMPORTING
      et_bapiret                  = lt_bapiret
      ev_severity                 = lv_severity.

  IF lv_severity CA wmegc_severity_ea.
    READ TABLE lt_bapiret INTO ls_bapiret
         WITH KEY type = wmegc_severity_err.
    IF sy-subrc = 0.
      MESSAGE ID     ls_bapiret-id
              TYPE   ls_bapiret-type
              NUMBER ls_bapiret-number
              WITH   ls_bapiret-message_v1 ls_bapiret-message_v2
                     ls_bapiret-message_v3 ls_bapiret-message_v4.
*     message from bapiret
    ELSE.
      MESSAGE e037(/scwm/rf_de).
*     putaway not possible
    ENDIF.
  ELSE.

*   update DB
    COMMIT WORK AND WAIT.

* Send EPCIS message

 PERFORM send_epcis_to_attp USING cs_rehu cs_rehu_hu.

    CALL METHOD /scwm/cl_tm=>cleanup( ).

      /scwm/cl_rf_bll_srvc=>set_fcode(
                            gc_fcode_backf ).

*   we are in complex mode
    IF cs_rehu_hu-unloaded IS INITIAL AND
       cs_rehu_hu-pgr IS INITIAL.
      cs_rehu-sumhu_proc  = cs_rehu-sumhu_proc + 1.
      READ TABLE cs_rehu-hus ASSIGNING <hus>
        WITH KEY huident = cs_rehu_hu-huident.
      IF sy-subrc IS INITIAL.
        <hus>-unloaded = 'X'.
        <hus>-pgr      = 'X'.
      ENDIF.
    ELSE.
*     Simple process mode
*     HU already on the final bin.
      cs_rehu-sumhu = cs_rehu-sumhu - 1.
    cs_rehu-sumhu_proc = cs_rehu-sumhu_proc - 1.

    DELETE cs_rehu-hus WHERE huident = cs_rehu_hu-huident.
    ENDIF.

  ENDIF.


ENDFUNCTION.
