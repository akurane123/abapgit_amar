FUNCTION ZFM_RFC_DECOMM_HU_PBO.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CS_REHU_HU) TYPE  /SCWM/S_RF_REHU_HU
*"--------------------------------------------------------------------

CALL METHOD /scwm/cl_rf_bll_srvc=>init_screen_param.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = gc_param_cs_rehu_hu.



ENDFUNCTION.
