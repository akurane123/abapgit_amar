*&---------------------------------------------------------------------*
*&      Form  dlv_ncts_check
*&---------------------------------------------------------------------*
*       delete HUs from list, if they are blocked for NCTS reason
*----------------------------------------------------------------------*
*      <--CT_HUS  text
*----------------------------------------------------------------------*
FORM dlv_ncts_check
     CHANGING ct_hus          TYPE /scwm/tt_rf_unlo_hus
              ct_deliveries   TYPE /scwm/tt_rf_unlo_docid
              ct_sdat_ini_dlv TYPE /scwm/tt_rf_unlo_docid.

  DATA: lt_dlv_headers    TYPE /scwm/dlv_header_out_prd_tab,
        lt_dlv_headers_bo TYPE /scwm/dlv_header_out_prd_tab,
        lt_docid          TYPE /scwm/dlv_docid_item_tab,
        lt_docid_bo       TYPE /scwm/dlv_docid_item_tab,
        lt_message        TYPE /scdl/dm_message_tab.

  DATA: ls_hus         TYPE /scwm/s_rf_unlo_hus,
        ls_status      TYPE /scdl/dl_status_str,
        ls_docid       TYPE /scwm/dlv_docid_item_str,
        lv_docid       TYPE /SCDL/DL_DOCID.

  DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_sdat_ini_dlv TYPE /scwm/s_rf_unlo_docid.

  FIELD-SYMBOLS: <fs_dlv_headers>  TYPE /scwm/dlv_header_out_prd_str,
                 <fs_header_dates> TYPE /scdl/dl_date_str,
                 <fs_docid>        TYPE /SCWM/S_RF_UNLO_DOCID.

  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_message TYPE REF TO /scwm/cl_dm_message_no.

  FIELD-SYMBOLS: <fs_msge>       TYPE /scdl/dm_message_str. "#EC *

* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  CLEAR: ct_sdat_ini_dlv.

* collect deliveries to be checked
  LOOP AT ct_hus INTO ls_hus.
    ls_docid-docid  = ls_hus-docid.
    ls_docid-doccat = ls_hus-doccat.

    IF lo_dlv->check_object_instance_exists( iv_docid = ls_hus-docid ) = abap_true.
      APPEND ls_docid TO lt_docid_bo.
    ELSE.
      APPEND ls_docid TO lt_docid.
    ENDIF.
  ENDLOOP.

* Check deliveries without HU to avoid the packing
* of unpacked items and unload
  LOOP AT ct_deliveries ASSIGNING <fs_docid>.
    ls_docid-docid  = <fs_docid>-docid.
    ls_docid-doccat = <fs_docid>-doccat.
    IF lo_dlv->check_object_instance_exists( iv_docid = <fs_docid>-docid ) = abap_true.
      APPEND ls_docid TO lt_docid_bo.
    ELSE.
      APPEND ls_docid TO lt_docid.
    ENDIF.
  ENDLOOP.

  SORT lt_docid.
  SORT lt_docid_bo.
  DELETE ADJACENT DUPLICATES FROM lt_docid.
  DELETE ADJACENT DUPLICATES FROM lt_docid_bo.

* ensures the only needed data is read:
  ls_include_data-head_date   = 'X'.
  ls_include_data-item_status = 'X'.
  ls_include_data-head_status = 'X'.

* read dlvs for those which have BO in the memory
  IF lt_docid_bo IS NOT INITIAL.
* read status for deliveries
    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_docid        = lt_docid_bo
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_headers      = lt_dlv_headers_bo
            eo_message      = lo_message.
      CATCH  /scdl/cx_delivery.                         "#EC NO_HANDLER
        CHECK lo_message IS NOT INITIAL.
        lt_message = lo_message->get_messages( ).

        READ TABLE lt_message
          INTO <fs_msge>
          WITH KEY msgty = wmegc_severity_err.

        IF sy-subrc = 0.
          MESSAGE ID     <fs_msge>-msgid
                  TYPE   <fs_msge>-msgty
                  NUMBER <fs_msge>-msgno
                  WITH   <fs_msge>-msgv1
                         <fs_msge>-msgv2
                         <fs_msge>-msgv3
                         <fs_msge>-msgv4.
        ENDIF.
    ENDTRY.
  ENDIF.

* read dlvs for those which are not in the memory
  IF lt_docid IS NOT INITIAL.
* ensures fast DB readwithout BO instantiation.
    ls_read_options-data_retrival_only = 'X'.
* read status for deliveries
    TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid   = lt_docid
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
            et_headers      = lt_dlv_headers
            eo_message      = lo_message.
      CATCH  /scdl/cx_delivery.                         "#EC NO_HANDLER
        CHECK lo_message IS NOT INITIAL.
        lt_message = lo_message->get_messages( ).

        READ TABLE lt_message
          ASSIGNING <fs_msge>
          WITH KEY msgty = wmegc_severity_err.

        IF sy-subrc = 0.
          MESSAGE ID     <fs_msge>-msgid
                  TYPE   <fs_msge>-msgty
                  NUMBER <fs_msge>-msgno
                  WITH   <fs_msge>-msgv1
                         <fs_msge>-msgv2
                         <fs_msge>-msgv3
                         <fs_msge>-msgv4.
        ENDIF.
    ENDTRY.
  ENDIF.


* concatenate the 2 table.
  APPEND LINES OF lt_dlv_headers_bo TO lt_dlv_headers.

* check the delivery
  LOOP AT lt_dlv_headers ASSIGNING <fs_dlv_headers>.

    READ TABLE <fs_dlv_headers>-status
         WITH  KEY status_type = gc_dl_status_type_ncts
         INTO  ls_status.

    IF sy-subrc = 0
       AND ls_status-status_value <> gc_dl_stat_val_ncts_not_rlvnt
       AND ls_status-status_value <> gc_dl_stat_val_ncts_released.

*     delete all HUs from List, where docid blocked
      lv_docid = <fs_dlv_headers>-docid.
      DELETE ct_hus WHERE docid = lv_docid.
      DELETE ct_deliveries WHERE docid = lv_docid.
      DELETE ct_sdat_ini_dlv WHERE docid = lv_docid.
    ENDIF.

*   collect those dlvs which the start date is initial
    READ TABLE <fs_dlv_headers>-dates ASSIGNING <fs_header_dates>
      WITH KEY tsttype      = gc_dl_date_type
               tst_category = gc_dl_date_cat_actual.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING <fs_dlv_headers> TO ls_sdat_ini_dlv.
      APPEND ls_sdat_ini_dlv TO ct_sdat_ini_dlv.
    ENDIF.

  ENDLOOP.

  IF ct_hus IS INITIAL AND ct_deliveries IS INITIAL.
     MESSAGE e332(/scwm/rf_de).
  ENDIF.


ENDFORM.                    " dlv_ncts_check
