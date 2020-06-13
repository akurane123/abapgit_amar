FUNCTION zrf_sh_dl_hu_sel_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_LOAD) TYPE  /SCWM/S_RF_LOAD
*"     REFERENCE(CS_ADMIN_LOAD) TYPE  /SCWM/S_RF_ADMIN_LOAD
*"     REFERENCE(CT_LOAD) TYPE  /SCWM/TT_RF_LOAD
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"----------------------------------------------------------------------

DATA: lv_valid_flag TYPE xfeld.

* HU read by barcode
  cs_load-huident = cs_load-rfhu.
*----------------------------------------------------------------------
*- Validate entered HU from ATTP.
*----------------------------------------------------------------------
  PERFORM validate_hu USING cs_load CHANGING lv_valid_flag.

* Validation is sucessfull
  IF lv_valid_flag IS NOT INITIAL.
    CALL FUNCTION '/SCWM/RF_SH_DL_HU_SEL_PAI'
      CHANGING
        cs_load       = cs_load
        cs_admin_load = cs_admin_load
        ct_load       = ct_load
        wme_verif     = wme_verif.
* Trigger Event to ATTP
    PERFORM load_hu USING cs_load ct_load.
  ELSE.
* HU validation failed
    MESSAGE e005(zmsg_attp_ewm) WITH cs_load-huident+2(18).

  ENDIF.

ENDFUNCTION.
