FUNCTION zfm_rfc_print_hu_label.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     VALUE(CS_UNLO) TYPE  /SCWM/S_RF_UNLO
*"     VALUE(CS_UNLO_PROD) TYPE  /SCWM/S_RF_UNLO_PROD
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_sscc,
           gcp       TYPE char12,
           ext_digit TYPE char1,
         END OF lty_sscc,

         BEGIN OF lty_encoded,
           obj_code TYPE /sttpec/e_objcode,
         END OF lty_encoded.

  DATA: lv_hu       TYPE /scwm/de_rf_huident,
        lv_size     TYPE /sttpec/e_serno_req,
        lv_encode   TYPE /sttpec/e_objcode,
        ls_sscc     TYPE lty_sscc,
        ls_range    TYPE gty_range,
        lt_encoded  TYPE STANDARD TABLE OF lty_encoded,
        lt_return   TYPE bapiret2_t,
        lv_delivery TYPE /scwm/de_rf_dlvno.


  DATA lv_act_field(60) TYPE c.
* Provide some default values to Exporting parameters of FM
  lv_size = 1.
  ls_sscc-gcp = gc_gcp.
  ls_sscc-ext_digit = gc_ext_digit.

  lv_act_field = /scwm/cl_rf_bll_srvc=>get_act_field( ).
**********************************************************************
** validate SSCC in ATTP***

  IF lv_act_field = '/SCWM/S_RF_UNLO-DLVNO'.
    lv_delivery = cs_unlo-dlvno.

* read GUID for entered delivery
    PERFORM dlvno_docid_get
      USING    lv_delivery
      CHANGING gs_docid_query.

    /scwm/cl_rf_bll_srvc=>set_field( '/SCWM/S_RF_UNLO-RFHU' ).
  ENDIF.

  IF lv_act_field = '/SCWM/S_RF_UNLO-RFHU'.
* Get the SSCC details
    PERFORM validate_sscc CHANGING cs_unlo-rfhu
                           gv_qty.
    SHIFT gv_matnr LEFT DELETING LEADING '0'.

    IF gv_qty IS NOT INITIAL.
      cs_unlo-huident = cs_unlo-rfhu.
*      cs_unlo_prod-rfprod = cs_unlo_prod-matnr = gv_matnr.
*      cs_unlo_prod-nista = gv_qty.
*      cs_unlo_prod-charg = cs_unlo_prod-rfbatch = gv_lotno.
*      cs_unlo_prod-bbdat = gv_datex.
    ELSE.
      CLEAR: cs_unlo-rfhu.
      MESSAGE e003(/sttpec/whs_msg).
    ENDIF.
    lv_hu = cs_unlo-huident.
* Creatre HU and pack the the delivery
    PERFORM create_pack_hu USING lv_hu
                                 lv_delivery
                                 gs_docid_query.


*Clear the fields
    CLEAR:gs_docid_query, cs_unlo, cs_unlo_prod.
    /scwm/cl_rf_bll_srvc=>set_field( '/SCWM/S_RF_UNLO-DLVNO' ).
    MESSAGE s006(zmsg_attp_ewm).
  ENDIF.


**********************************************************************

** Call FM in ATTP to get the Serial Number
*  CALL FUNCTION '/STTP/INT_SNR_REQ_SYNC' DESTINATION 'ATDCLNT100_ABAP'
*    EXPORTING
*      iv_system         = 'SAP_H01CLNT100'
*      is_sscc           = ls_sscc
*      iv_size           = lv_size
*      iv_list_range     = 'R'
*      iv_readyly_encode = '1'
*      ib_only_exact     = abap_true
*      iv_caller         = 'E'
*      ib_commit         = 'X'
*    IMPORTING
*      es_range          = ls_range
*    TABLES
*      et_encoded        = lt_encoded
*      et_return         = lt_return.
** Check for any errors while getting Serial Number
*  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
*  IF sy-subrc IS NOT INITIAL.
*    READ TABLE lt_encoded INTO lv_encode INDEX 1.
*    lv_hu = lv_encode+4(20).
*
*    PERFORM post_event_commissioning USING ls_range lv_encode lv_hu.
*  ELSE.
*    MESSAGE e002(zmsg_attp_ewm).
*  ENDIF.




ENDFUNCTION.
