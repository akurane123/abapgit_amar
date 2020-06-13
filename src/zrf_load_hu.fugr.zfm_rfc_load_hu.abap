FUNCTION ZFM_RFC_LOAD_HU.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"     REFERENCE(CS_REHU) TYPE  /SCWM/S_RF_ADMIN_REHU
*"----------------------------------------------------------------------

  DATA: lv_fcode      TYPE /scwm/de_fcode.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  CASE lv_fcode .
    WHEN 'ENTER'.
* Load HU
*      PERFORM load_hu USING cs_rehu cs_rehu_hu.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
