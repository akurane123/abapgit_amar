FUNCTION zfm_rfc_decomm_hu_pai .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"----------------------------------------------------------------------

  DATA: lv_fcode      TYPE /scwm/de_fcode.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  CASE lv_fcode .
    WHEN 'ENTER'.
* Decomission HU
      PERFORM decommission_hu USING cs_rehu_hu.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
