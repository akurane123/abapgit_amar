FUNCTION ZRF_INQ_ZNWODE_PBO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(CS_INQ_TO) TYPE  /SCWM/S_RF_INQ_TO
*"     REFERENCE(CT_INQ_TO) TYPE  /SCWM/TT_RF_INQ_TO
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"     REFERENCE(ZT_CHILD_SN) TYPE  ZRF_TT_CHILD_SN
*"----------------------------------------------------------------------

  BREAK-POINT ID /scwm/rf_inquiry.

*  Initiate screen parameter
  /scwm/cl_rf_bll_srvc=>init_screen_param( ).

* Set screen parameter
  /scwm/cl_rf_bll_srvc=>set_screen_param('CT_INQ_TO').
  /scwm/cl_rf_bll_srvc=>set_screen_param('CS_INQ_TO').

* Transfer table name into RF framework
*  /scwm/cl_rf_bll_srvc=>set_screen_param('/SCWM/TT_RF_INQ_TO').

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_scr_tabname
    EXPORTING
      iv_scr_tabname = 'CT_INQ_TO'.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_line
    EXPORTING
      iv_line = 1.

if ZT_CHILD_SN[] is INITIAL.
  data : lv_qty type /SCWM/DE_QUANTITY,
         LV_SRC_QTY TYPE /SCWM/DE_QUANTITY.

  loop at ct_inq_to into cs_inq_to.
    CLEAR : LV_QTY.
    LV_SRC_QTY = cs_inq_to-VSOLA.
    CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
      EXPORTING
        IV_MATID                             = cs_inq_to-MATID
        IV_QUAN                              = LV_SRC_QTY
        IV_UNIT_FROM                         = cs_inq_to-ALTME
        IV_UNIT_TO                           = 'EA'
        IV_BATCHID                           = cs_inq_to-BATCHID
     IMPORTING
       EV_QUAN                              = LV_QTY
              .
    cs_inq_to-vsola = lv_qty.
    cs_inq_to-altme = 'EA'.
    modify ct_inq_to from cs_inq_to TRANSPORTING vsola altme.
  ENDLOOP.
endif.

ENDFUNCTION.
