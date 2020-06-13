FUNCTION zrf_repack_child_sn_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"----------------------------------------------------------------------

* clear the field
  CLEAR zs_child_sn-rfsn.

* Introduce the parameters
  CALL METHOD /scwm/cl_rf_bll_srvc=>init_screen_param.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZS_CHILD_SN'.



ENDFUNCTION.
