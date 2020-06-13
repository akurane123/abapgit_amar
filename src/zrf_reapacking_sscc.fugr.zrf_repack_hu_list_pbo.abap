FUNCTION zrf_repack_hu_list_pbo .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"     REFERENCE(ZT_CHILD_SN) TYPE  ZRF_TT_CHILD_SN
*"----------------------------------------------------------------------


* Introduce the parameters
  CALL METHOD /scwm/cl_rf_bll_srvc=>init_screen_param.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZS_CHILD_SN'.
*
* Introduce the used parameter
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
    EXPORTING
      iv_param_name = 'ZT_CHILD_SN'.

* Transfer table name into RF framework
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_scr_tabname
    EXPORTING
      iv_scr_tabname = 'ZRF_TT_CHILD_SN'.


* Set displaying line number of table
  CALL METHOD /scwm/cl_rf_bll_srvc=>set_line
    EXPORTING
      iv_line = 1.

ENDFUNCTION.
