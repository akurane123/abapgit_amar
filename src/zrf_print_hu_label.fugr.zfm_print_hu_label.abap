FUNCTION ZFM_PRINT_HU_LABEL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CS_UNLO) TYPE  /SCWM/S_RF_UNLO
*"----------------------------------------------------------------------

DATA: lv_fcode      TYPE /scwm/de_fcode.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  CASE lv_fcode .
    WHEN 'ENTER'.
* Print HU
*      PERFORM print_hu_label USING cs_rehu_hu.

    WHEN OTHERS.
  ENDCASE.


ENDFUNCTION.
