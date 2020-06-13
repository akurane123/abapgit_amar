FUNCTION zrf_ul_packmat_sel_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_UNLO) TYPE  /SCWM/S_RF_UNLO
*"     REFERENCE(CS_ADMIN_UNLO) TYPE  /SCWM/S_RF_ADMIN_UNLO
*"     REFERENCE(CS_UNLO_PROD) TYPE  /SCWM/S_RF_UNLO_PROD
*"     REFERENCE(CS_UNLO_DLV) TYPE  /SCWM/S_RF_UNLO_DOCID
*"     REFERENCE(CT_UNLO_DLV) TYPE  /SCWM/TT_RF_UNLO_DOCID
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*
  BREAK-POINT ID /scwm/rf_unloading.

* Initiate screen parameter
  /scwm/cl_rf_bll_srvc=>init_screen_param( ).
* hard-coded packaging Material
  cs_unlo-pmatid = '0050568C800D1ED9A293AB813EC6A0EF'.

* Introduce the parameters
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'CS_UNLO'.


  IF gv_dlvlockusr IS NOT INITIAL.
    CLEAR gv_pmatid.
    cs_unlo-pmatid = gv_pmatid.
    MESSAGE e601(mc) WITH gv_dlvlockusr.
  ENDIF.




ENDFUNCTION.
