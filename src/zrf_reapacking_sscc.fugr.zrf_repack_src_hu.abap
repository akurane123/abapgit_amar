FUNCTION zrf_repack_src_hu .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"----------------------------------------------------------------------

  DATA: lv_fcode      TYPE /scwm/de_fcode.
  DATA lv_act_field(60) TYPE c.



  lv_act_field = /scwm/cl_rf_bll_srvc=>get_act_field( ).
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).


  CASE lv_fcode.
    WHEN gc_fcode_comm.
* create SSCC
      PERFORM create_sscc CHANGING zs_child_sn-rfhu_parent1.

    WHEN gc_fcode_enter.
      IF zs_child_sn-rfhu_parent1 IS NOT INITIAL.
* validate Parent SSCC with ATTP
        PERFORM validate_sscc USING zs_child_sn-rfhu_parent1.

        zs_child_sn-matnr = gv_matnr.
        zs_child_sn-uom = gv_uom.
        zs_child_sn-maktx = gv_maktx.
      ELSE.
        CLEAR: zs_child_sn-rfhu_parent1.
        MESSAGE e002(/sttpec/whs_msg).
      ENDIF.

  ENDCASE.




ENDFUNCTION.
