FUNCTION zrf_repack_child_sn_pai .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZS_CHILD_SN) TYPE  ZRF_S_CHILD_SN
*"     REFERENCE(ZT_CHILD_SN) TYPE  ZRF_TT_CHILD_SN
*"----------------------------------------------------------------------
  DATA: lv_act_field(60) TYPE c,
        ls_objdata       TYPE /sttpec/s_att_obj_response,
        ls_rsrc          TYPE /scwm/rsrc.
  DATA: lv_fcode TYPE /scwm/de_fcode.

* check if material is serialized
  IF gv_matnr IS NOT INITIAL.
    CLEAR: gv_serial.
    PERFORM gf_serialized USING gv_matnr
                          CHANGING gv_serial.
  ENDIF.
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
** Validate Destination HU with ATTP
      IF lv_act_field = 'ZRF_S_CHILD_SN-RFHU_PARENT2'.
        IF zs_child_sn-rfhu_parent2 IS NOT INITIAL.
* validate Parent SSCC with ATTP
          PERFORM validate_sscc USING zs_child_sn-rfhu_parent2.
          zs_child_sn-matnr = gv_matnr.
          zs_child_sn-uom = gv_uom.
          zs_child_sn-maktx = gv_maktx.

          /scwm/cl_rf_bll_srvc=>set_field( 'ZRF_S_CHILD_SN-RFSN' ).
          /scwm/cl_rf_bll_srvc=>set_screlm_input_off( 'ZRF_S_CHILD_SN-RFHU_PARENT2').

        ELSE.
          MESSAGE e002(/sttpec/whs_msg).
        ENDIF.
      ENDIF.


** validate Child Serial Numbers
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
    WHEN gc_fcode_pack.
* Check if the Process Type Determination Indicator is set to AT
* to conclude the step in Process Determined Warehouse Type
      SELECT SINGLE matid
        INTO @DATA(lv_matid)
        FROM /sapapo/matkey
        WHERE matnr = @gv_matnr.

      IF sy-subrc = 0.
        SELECT SINGLE ptdetind
          INTO @DATA(lv_ptdetind)
          FROM /sapapo/matlwh
          WHERE matid = @lv_matid.
* Check the indicator
        IF sy-subrc = 0 AND lv_ptdetind = 'AT'.
* Deconsolidate the HU
          PERFORM deconsolidate USING  zt_child_sn.
* Unpack the child serial numbers from destination HU
          PERFORM send_unpack USING zt_child_sn.
* Pack the serial numbers to source HU
          PERFORM send_pack USING zt_child_sn.
        ELSE.
* Unpack the child serial numbers from destination HU
          PERFORM send_unpack USING zt_child_sn.
* Create EWM task for moving HU quantity to destination HU
          PERFORM create_move USING zt_child_sn.
* Pack the serial numbers to source HU
          PERFORM send_pack USING zt_child_sn.
** Confirm EWM task
          PERFORM confirm_move USING zt_child_sn.
        ENDIF.
      ENDIF.


** set next function code
      REFRESH:  zt_child_sn.
      CLEAR: zs_child_sn.
*      /scwm/cl_rf_bll_srvc=>set_fcode( 'BACK' ).
  ENDCASE.





ENDFUNCTION.
