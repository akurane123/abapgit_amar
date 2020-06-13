*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF19 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_MIN_REM_SL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_UNLO_PROD  text
*      -->P_LS_DELIERIES  text
*      <--P_LV_FAILED  text
*----------------------------------------------------------------------*
FORM check_min_rem_sl  USING    cs_unlo_prod  TYPE /scwm/s_rf_unlo_prod
                                cs_unlo_dlv   TYPE /scwm/s_rf_unlo_docid
                       CHANGING ev_failed   TYPE boole_d.

  DATA: lt_docid        TYPE        /scwm/dlv_docid_item_tab,
        ls_docid        TYPE        /scwm/dlv_docid_item_str,
        lo_dlv          TYPE REF TO /scwm/cl_dlv_management_prd,
        ls_read_options TYPE        /scwm/dlv_query_contr_str,
        lt_prd_hdr      TYPE        /scwm/dlv_header_out_prd_tab,
        ls_prd_hdr      TYPE        /scwm/dlv_header_out_prd_str,
        lt_items        TYPE        /scwm/dlv_item_out_prd_tab,
        ls_item         TYPE        /scwm/dlv_item_out_prd_str,
        ls_date         TYPE        /scdl/dl_date_str,
        ls_status       TYPE        /scdl/dl_status_str,
        lv_batch        TYPE        /scdl/dl_batchno,
        ls_batch_md     TYPE        /scwm/dlv_md_prod_batch_det,
        lo_md_access    TYPE REF TO /scwm/cl_dlv_md_access,
        lv_vfdat        TYPE        /scwm/sled.

  CLEAR ev_failed.
  CHECK cs_unlo_prod-charg IS NOT INITIAL.

  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.

  ls_docid-docid  = cs_unlo_dlv-docid.
  ls_docid-itemid = cs_unlo_dlv-item_id.
  ls_docid-doccat = cs_unlo_dlv-doccat.

  APPEND ls_docid TO lt_docid.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = lt_docid
          is_read_options = ls_read_options
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  READ TABLE lt_items INTO ls_item INDEX 1.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  IF ls_item-product-batchno IS NOT INITIAL.
    READ TABLE ls_item-status INTO ls_status
      WITH KEY status_type = 'DSL'.

    IF sy-subrc = 0.         "DSL status exists
      IF ls_status-status_value = /scdl/if_dl_status_c=>sc_v_checked_not_ok.

        ev_failed = 'X'.
      ENDIF.
    ENDIF.

    RETURN.
  ELSE.
*   search for a suitable subitem
    CLEAR ls_docid.
    ls_docid-docid  = cs_unlo_dlv-docid.
    ls_docid-doccat = cs_unlo_dlv-doccat.

    APPEND ls_docid TO lt_docid.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_docid        = lt_docid
            is_read_options = ls_read_options
          IMPORTING
            et_headers      = lt_prd_hdr
            et_items        = lt_items.
      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    LOOP AT lt_items INTO ls_item WHERE product-batchno = cs_unlo_prod-charg AND
               product-productno = cs_unlo_prod-matnr.
      READ TABLE ls_item-hierarchy TRANSPORTING NO FIELDS
          WITH KEY hierarchy_type = 'BSP'
                   parent_object  = cs_unlo_dlv-item_id.
      IF sy-subrc = 0.
        READ TABLE ls_item-status INTO ls_status
          WITH KEY status_type = 'DSL'.

        IF sy-subrc = 0.         "DSL status exists
          IF ls_status-status_value = /scdl/if_dl_status_c=>sc_v_checked_not_ok.
            ev_failed = 'X'.
          ENDIF.
        ENDIF.

        RETURN.
      ENDIF.
    ENDLOOP.

*   subitem does not exists, do the check based on the customizing
    lv_batch =  cs_unlo_prod-charg.

    IF lo_md_access IS NOT BOUND.
      lo_md_access = /scwm/cl_dlv_md_access=>get_instance( ).
    ENDIF.

    TRY.
        IF abap_true = lo_md_access->get_batch_existence(
            iv_productid = cs_unlo_prod-matid
            iv_batchno   = lv_batch
            iv_lgnum     = ls_item-sapext-/scwm/whno
            iv_entitled  = ls_item-sapext-entitled ).

          ls_batch_md = lo_md_access->get_batch_detail(
              iv_productno         = cs_unlo_prod-matnr
              iv_batchno           = lv_batch
              iv_lgnum             = ls_item-sapext-/scwm/whno
              iv_entitled          = ls_item-sapext-entitled
              iv_no_classification = abap_false ).

          lv_vfdat = ls_batch_md-sled_bbd.

        ELSE.
          lv_vfdat = cs_unlo_prod-bbdat.
        ENDIF.
      CATCH /scwm/cx_dlv_batch .

    ENDTRY.

    READ TABLE lt_prd_hdr INTO ls_prd_hdr INDEX 1.

    READ TABLE ls_prd_hdr-dates INTO ls_date
         WITH KEY tsttype = /scdl/if_dl_date_c=>sc_tsttype_delivery
             tst_category = /scdl/if_dl_date_c=>sc_tstcat_actual.

    IF sy-subrc <> 0.
      ls_date-tsttype = /scdl/if_dl_date_c=>sc_tsttype_delivery.
      ls_date-tst_category = /scdl/if_dl_date_c=>sc_tstcat_actual.
      GET TIME STAMP FIELD ls_date-tstfr.
      ls_date-tstto = ls_date-tstfr.

      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = ls_item-sapext-/scwm/whno
        IMPORTING
          ev_tzone        = ls_date-tzone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.

      APPEND ls_date TO ls_prd_hdr-dates.
    ENDIF.

    /scwm/cl_dlv_batch_internal=>item_val_sledbbd(
      EXPORTING
        iv_lgnum       = ls_item-sapext-/scwm/whno
        iv_docno       = ls_prd_hdr-docno
        iv_itemno      = ls_item-itemno
        iv_doccat      = ls_prd_hdr-doccat
        iv_doctype     = ls_prd_hdr-doctype
        iv_docid       = ls_prd_hdr-docid
        iv_itemcat     = ls_item-itemcat
        iv_itemtype    = ls_item-itemtype
        iv_itemid      = ls_item-itemid
        iv_action      = /scwm/if_dlv_batch_c=>sc_act_gr
        it_dates       = ls_prd_hdr-dates
        iv_productid   = cs_unlo_prod-matid
        iv_vfdat       = lv_vfdat
        iv_entitled    = ls_item-sapext-entitled
      IMPORTING
        ev_failed      = ev_failed ).

  ENDIF.

ENDFORM.                    " CHECK_MIN_REM_SL
