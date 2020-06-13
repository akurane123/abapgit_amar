FUNCTION ZRF_INQ_ZNWODE_PAI.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(CS_INQ_TO) TYPE  /SCWM/S_RF_INQ_TO
*"     REFERENCE(CT_INQ_TO) TYPE  /SCWM/TT_RF_INQ_TO
*"     REFERENCE(CT_INQ_TO_CONT) TYPE  /SCWM/TT_RF_INQ_TO
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"     REFERENCE(ZT_CHILD_SN) TYPE  ZRF_TT_CHILD_SN
*"----------------------------------------------------------------------

  DATA:  lv_fcode    TYPE /scwm/de_fcode,
         ls_inq_to   TYPE /scwm/s_rf_inq_to,
         lv_lines    TYPE i.

  BREAK-POINT ID /scwm/rf_inquiry.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  IF lv_fcode = 'OPENTO'.
    READ TABLE ct_inq_to_cont INTO ls_inq_to
      WITH KEY tostat = ' '.
    IF sy-subrc = 0.
      CLEAR : ct_inq_to,
              cs_inq_to.

      LOOP AT ct_inq_to_cont INTO ls_inq_to WHERE tostat = ' '.
        APPEND ls_inq_to TO ct_inq_to.
      ENDLOOP.
      DESCRIBE TABLE ct_inq_to LINES lv_lines.
      LOOP AT ct_inq_to INTO ls_inq_to.
        ls_inq_to-numwt = lv_lines.
        MODIFY ct_inq_to FROM ls_inq_to.
      ENDLOOP.
    ELSE.
*     No open TOs exist for WO number &1
      MESSAGE e005(/scwm/rf_en) WITH selection-who.
    ENDIF.

  ELSEIF lv_fcode = 'CONFTO'.
    READ TABLE ct_inq_to_cont INTO ls_inq_to
      WITH KEY tostat = 'C'.
    IF sy-subrc = 0.
      CLEAR : ct_inq_to,
              cs_inq_to.

      LOOP AT ct_inq_to_cont INTO ls_inq_to WHERE tostat = 'C'.
        APPEND ls_inq_to TO ct_inq_to.
      ENDLOOP.
      DESCRIBE TABLE ct_inq_to LINES lv_lines.
      LOOP AT ct_inq_to INTO ls_inq_to.
        ls_inq_to-numwt = lv_lines.
        MODIFY ct_inq_to FROM ls_inq_to.
      ENDLOOP.
    ELSE.
*     No Confirmed TOs exist for WO number &1
      MESSAGE e073(/scwm/rf_en) WITH selection-who.
    ENDIF.

  ELSEIF lv_fcode = 'ALLTO'.
    CLEAR : ct_inq_to,
            cs_inq_to.

    LOOP AT ct_inq_to_cont INTO ls_inq_to .
      ls_inq_to-to_ind = 'ALL'.
      APPEND ls_inq_to TO ct_inq_to.

    ENDLOOP.
    DESCRIBE TABLE ct_inq_to LINES lv_lines.
    LOOP AT ct_inq_to INTO ls_inq_to.
      ls_inq_to-numwt = lv_lines.
      MODIFY ct_inq_to FROM ls_inq_to.
    ENDLOOP.

  ELSEIF lv_fcode = 'DETAIL'.
  ENDIF.
*******************************************
  DATA: lv_act_field(60) TYPE c,
        ls_objdata       TYPE /sttpec/s_att_obj_response,
        ls_rsrc          TYPE /scwm/rsrc,

        gv_lgnum        TYPE /scwm/lgnum,
        gc_fcode_enter        TYPE /scwm/de_fcode VALUE 'ENTER',
        gt_objstruc     TYPE /sttpec/t_att_obj_hierarchy,
        gt_objstruc_qty TYPE /sttpec/t_att_qty_hierarchy,
        gt_objdata      TYPE /sttpec/t_att_obj_response,
        gt_child_sn     TYPE zrf_tt_child_sn.
* get Warehouse Number and set the value
* set default values
  CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
    EXPORTING
      iv_uname = sy-uname
    CHANGING
      cs_rsrc  = ls_rsrc.

  IF sy-subrc = 0.
    gv_lgnum = ls_rsrc-lgnum.
* Set Warehouse Number
    CALL METHOD /scwm/cl_tm=>set_lgnum( ls_rsrc-lgnum ).
  ELSE.
    MESSAGE e002(/sttpec/whs_msg).
  ENDIF.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  CASE lv_fcode.
    WHEN gc_fcode_enter.
      lv_act_field = /scwm/cl_rf_bll_srvc=>get_act_field( ).
      IF lv_act_field = 'ZRF_S_CHILD_SN-RFSN'.
        IF zs_child_sn-rfsn IS NOT INITIAL.
* Get child serial number details
          PERFORM validate_child_sn USING zs_child_sn-rfsn
                                    CHANGING ls_objdata.

* validate scanned child serial number belongs to Source HU
          ASSIGN gt_objstruc_qty[ code_urn_child =  ls_objdata-code_urn ] TO FIELD-SYMBOL(<fs_objdata>).
* If successful update internal Table
          IF sy-subrc = 0.
            zs_child_sn-quantity = <fs_objdata>-quantity.

            /scwm/cl_rf_bll_srvc=>set_field( 'ZRF_S_CHILD_SN-RFSN').
* Update the internal Table
            ASSIGN gt_child_sn[ rfsn = zs_child_sn-rfsn ] TO FIELD-SYMBOL(<fs_child_data>).
            IF sy-subrc = 0.
              MESSAGE e025(/sttpec/whs_msg).
              RETURN.
            ENDIF.
            APPEND zs_child_sn TO gt_child_sn.
            APPEND zs_child_sn TO zt_child_sn.
            CLEAR: zs_child_sn-rfsn.
            DESCRIBE TABLE gt_child_sn LINES zs_child_sn-count.
* Introduce the parameters
            CALL METHOD /scwm/cl_rf_bll_srvc=>set_screen_param
              EXPORTING
                iv_param_name = 'ZS_CHILD_SN'.
          ELSE.
* Error Message
            MESSAGE e003(/sttpec/whs_msg).
          ENDIF.
        ELSE.
          MESSAGE e002(/sttpec/whs_msg).
        ENDIF.
      ENDIF.
    endcase.
ENDFUNCTION.
