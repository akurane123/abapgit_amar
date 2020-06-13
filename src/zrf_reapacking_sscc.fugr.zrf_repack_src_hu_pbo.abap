FUNCTION ZRF_REPACK_SRC_HU_PBO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"----------------------------------------------------------------------

* Introduce the parameters
  CALL METHOD /scwm/cl_rf_bll_srvc=>init_screen_param.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZS_CHILD_SN'.



ENDFUNCTION.
