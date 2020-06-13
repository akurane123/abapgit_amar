*&--------------------------------------------------------------------*
*&      Form  hu_haz_dlv_txt_read
*&--------------------------------------------------------------------*
*       Read Texts for HU
*        -if rdocid is filled - texts for dlv (incl. position texts)
*                               will be read
*        -if hazmat is filled - hazardous information of all HU-items
*                               will be read
*---------------------------------------------------------------------*
*      -->IV_LGNUM   text
*      -->IV_HUIDENT text
*      -->IV_HAZMAT  text
*      -->IV_RDOCCAT text
*      -->IV_RDOCID  text
*      -->IV_ACTTY   text
*      <--CV_TEXT_INDtext
*---------------------------------------------------------------------*
FORM hu_haz_dlv_txt_read
  USING    iv_lgnum    TYPE /scwm/lgnum
           iv_huident  TYPE /scwm/huident
           iv_hazmat   TYPE xfeld
           iv_rdoccat  TYPE /scwm/de_doccat
           iv_rdocid   TYPE /scwm/de_docid
           iv_actty    TYPE /scwm/de_actty
  CHANGING cv_text_ind TYPE /scwm/de_text_ind.

  CALL FUNCTION '/SCWM/RF_TEXT_GET_AND_SET'
    EXPORTING
      iv_lgnum        = iv_lgnum
      iv_actty        = iv_actty
      iv_rdoccat      = iv_rdoccat
      iv_rdocid       = iv_rdocid
      iv_huident      = iv_huident
      iv_hazmat       = iv_hazmat
    IMPORTING
      ev_text_ind     = cv_text_ind
    EXCEPTIONS
      interface_error = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " HU_HAZ_DLV_TXT_READ
*&--------------------------------------------------------------------*
*&      Form  create_hu_list
*&--------------------------------------------------------------------*
*  Create the list of available HUs for unloading.
*  This form fills out the cs_admin_unlo-hus and cs_admin_unlo-deliveries
*  tables
*---------------------------------------------------------------------*
*      <->CT_UNLO   Unloading Output Table
*      <->CS_ADMIN  Unloading Admin. Structure
*---------------------------------------------------------------------*
FORM create_hu_list
  CHANGING cs_unlo  TYPE /scwm/s_rf_unlo
           cs_admin TYPE /scwm/s_rf_admin_unlo.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
  DATA: lv_whse_tzone   TYPE tznzone,
        lv_secs         TYPE i,
        lv_stamp_low    TYPE tzntstmps,
        lv_stamp_high   TYPE tzntstmps.

  DATA: ls_incl_prd_dat TYPE /scwm/dlv_query_incl_str_prd,
        ls_dlv_sel      TYPE /scwm/dlv_selection_str,
        ls_deliveries   TYPE /scwm/s_rf_unlo_docid,
        ls_huhdr        TYPE /scwm/s_huhdr_int,
        ls_huref        TYPE /scwm/s_huref_int,
        ls_hus          TYPE /scwm/s_rf_unlo_hus,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_docid        TYPE /scwm/s_docid,
        ls_docno        TYPE /scwm/dlv_prd_map_str,
        ls_partyloc     TYPE /scdl/dl_partyloc_str,
        ls_head_status_dyn  TYPE        /scdl/dl_status_type.

  DATA: lt_prd_hdr TYPE /scwm/dlv_header_out_prd_tab,
        lt_prd_hdr_act TYPE /scwm/dlv_header_out_prd_tab,
        lt_dlv_sel TYPE /scwm/dlv_selection_tab,
        lt_huhdr   TYPE /scwm/tt_huhdr_int,
        lt_huref   TYPE /scwm/tt_huref_int,
        lt_docid        TYPE /scwm/tt_docid,
        lt_sdat_ini_dlv TYPE /scwm/tt_rf_unlo_docid,
        lt_docno        TYPE /scwm/dlv_prd_map_tab.

  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.

  FIELD-SYMBOLS:
        <fs_prd_hdr>    TYPE /scwm/dlv_header_out_prd_str,
        <ls_status>     TYPE /scdl/dl_status_str.
*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*
* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

* query deliveries
*  warehouse number
  ls_dlv_sel-fieldname = gc_dl_logfname_whno_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = cs_admin-lgnum.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.
*  door
  ls_dlv_sel-fieldname = gc_dl_logfname_door_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = cs_unlo-door.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

* DATE Selection
  CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
    EXPORTING
      iv_lgnum        = cs_admin-lgnum
    IMPORTING
      ev_tzone        = lv_whse_tzone
    EXCEPTIONS
      interface_error = 1
      data_not_found  = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* remember values
  gv_door_dd_fut  = cs_unlo-door_dd_fut.
  gv_door_dd_past = cs_unlo-door_dd_past.

* Lowest Date
  IF cs_unlo-door_dd_past IS INITIAL.
    CONVERT DATE cs_unlo-door_dlv_date
            TIME gc_time_low
            INTO TIME STAMP lv_stamp_low
            TIME ZONE lv_whse_tzone.
  ELSE.
    CONVERT DATE cs_unlo-door_dlv_date
            TIME gc_time_low
            INTO TIME STAMP lv_stamp_low
            TIME ZONE lv_whse_tzone.
    lv_secs = cs_unlo-door_dd_past * gc_day_in_sec * -1.
    TRY.
        CALL METHOD cl_abap_tstmp=>add
          EXPORTING
            tstmp   = lv_stamp_low
            secs    = lv_secs
          RECEIVING
            r_tstmp = lv_stamp_low.
      CATCH cx_parameter_invalid_range. "#EC NO_HANDLER
      CATCH cx_parameter_invalid_type. "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

  CLEAR: lv_secs.

* Hihgest Date
  IF cs_unlo-door_dd_fut IS INITIAL.
    CONVERT DATE cs_unlo-door_dlv_date
            TIME gc_time_high
            INTO TIME STAMP lv_stamp_high
            TIME ZONE lv_whse_tzone.
  ELSE.
    CONVERT DATE cs_unlo-door_dlv_date
            TIME gc_time_high
            INTO TIME STAMP lv_stamp_high
            TIME ZONE lv_whse_tzone.
    lv_secs = cs_unlo-door_dd_fut * gc_day_in_sec.
    TRY.
        CALL METHOD cl_abap_tstmp=>add
          EXPORTING
            tstmp   = lv_stamp_high
            secs    = lv_secs
          RECEIVING
            r_tstmp = lv_stamp_high.
      CATCH cx_parameter_invalid_range. "#EC NO_HANDLER
      CATCH cx_parameter_invalid_type.  "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

* not blocked
  ls_dlv_sel-fieldname = gc_dl_logfname_value_dbc_h.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = gc_dl_status_value_unblocked. "not blocked
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

*  status not "unloading finished"
  ls_dlv_sel-fieldname = gc_dl_logfname_value_dun_i.
  ls_dlv_sel-option    = wmegc_option_ne.
  ls_dlv_sel-low       = gc_dl_status_value_dlun.  "ungleich Entladen
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.
* D A T E
  ls_dlv_sel-fieldname = gc_dl_logfname_value_dlvpldat.
  ls_dlv_sel-option    = wmegc_option_bt.
  ls_dlv_sel-low       = lv_stamp_low.
  ls_dlv_sel-high      = lv_stamp_high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  CLEAR ls_incl_prd_dat.
  ls_incl_prd_dat-head_partyloc   = 'X'.
  ls_incl_prd_dat-head_date       = 'X'.
  ls_incl_prd_dat-head_status     = 'X'.
  ls_incl_prd_dat-head_status_dyn = 'Y'.
  ls_incl_prd_dat-head_status     = 'X'.

  ls_head_status_dyn = gc_dl_status_type_dsp.
  APPEND ls_head_status_dyn TO ls_incl_prd_dat-head_status_dyn_detail.

  ls_head_status_dyn = gc_dl_status_type_dun.
  APPEND ls_head_status_dyn TO ls_incl_prd_dat-head_status_dyn_detail.

  ls_read_options-data_retrival_only = 'X'.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_selection    = lt_dlv_sel
          iv_doccat       = cs_admin-doccat
          is_include_data = ls_incl_prd_dat
          is_read_options = ls_read_options
        IMPORTING
          et_headers      = lt_prd_hdr.

    CATCH /scdl/cx_delivery.
      MESSAGE e212(/scwm/rf_de) WITH cs_unlo-door.
*     No deliveries assigned to door &1
  ENDTRY.

* remove the planned status read and search for actual
  DELETE lt_dlv_sel INDEX 5.
* actual D A T E
  ls_dlv_sel-fieldname = gc_dl_logfname_value_dlvactdat.
  ls_dlv_sel-option    = wmegc_option_bt.
  ls_dlv_sel-low       = lv_stamp_low.
  ls_dlv_sel-high      = lv_stamp_high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_selection    = lt_dlv_sel
          iv_doccat       = cs_admin-doccat
          is_include_data = ls_incl_prd_dat
          is_read_options = ls_read_options
        IMPORTING
          et_headers      = lt_prd_hdr_act.

    CATCH /scdl/cx_delivery.
      MESSAGE e212(/scwm/rf_de) WITH cs_unlo-door.
*     No deliveries assigned to door &1
  ENDTRY.

  APPEND LINES OF lt_prd_hdr_act TO lt_prd_hdr.
  SORT lt_prd_hdr BY docid.
  DELETE ADJACENT DUPLICATES FROM lt_prd_hdr COMPARING docid.

* prepare call
  LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
    READ TABLE <fs_prd_hdr>-status ASSIGNING <ls_status>
        WITH KEY status_type = gc_dl_status_type_dun.

    IF sy-subrc = 0 AND <ls_status>-status_value = gc_dl_status_value_finished.
      READ TABLE <fs_prd_hdr>-status ASSIGNING <ls_status>
          WITH KEY status_type = gc_dl_status_type_dsp.

      IF sy-subrc = 0 AND
         <ls_status>-status_value <> gc_dl_status_value_not_started AND
         <ls_status>-status_value <> gc_dl_status_value_partly.

        DELETE lt_prd_hdr.
        CONTINUE.
      ENDIF.

    ENDIF.
    ls_docid-docid = <fs_prd_hdr>-docid.
    APPEND ls_docid TO lt_docid.
  ENDLOOP.

* get delivery number for delivery guid
  CALL METHOD lo_dlv->map_docid_to_docno
    EXPORTING
      iv_doccat  = cs_admin-doccat
      it_docid   = lt_docid
    IMPORTING
      et_mapping = lt_docno.

* prepare worklist (list of deliveries assigned to door)
  LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
    ls_deliveries-door   = cs_unlo-door.
    ls_deliveries-docid  = <fs_prd_hdr>-docid.
    ls_deliveries-doccat = <fs_prd_hdr>-doccat.
    ls_deliveries-doctype = <fs_prd_hdr>-doctype.

    READ TABLE lt_docno INTO ls_docno
         WITH KEY docid = <fs_prd_hdr>-docid.
    IF sy-subrc EQ 0.
      ls_deliveries-dlvno  = ls_docno-docno.
    ENDIF.

    READ TABLE <fs_prd_hdr>-partyloc INTO ls_partyloc
         WITH KEY party_role = /scdl/if_dl_partyloc_c=>sc_party_role_wh.
    IF sy-subrc = 0.
      MOVE ls_partyloc-locationid TO ls_deliveries-wh_locationid.
      MOVE ls_partyloc-locationno TO ls_deliveries-wh_locationno.
    ENDIF.

    APPEND ls_deliveries TO cs_admin-deliveries.
  ENDLOOP.

  SORT cs_admin-deliveries BY docid.
  DELETE ADJACENT DUPLICATES FROM cs_admin-deliveries COMPARING docid.

* collect the HUs for the deliveries
  CALL FUNCTION '/SCWM/DLV_GET_HUS_FOR_DELIVERY'
    EXPORTING
      iv_doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
      it_docid  = lt_docid
    IMPORTING
      et_huref  = lt_huref
      et_huhdr  = lt_huhdr
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* create table of HUs
  LOOP AT lt_huhdr INTO ls_huhdr.
    ls_hus-huident = ls_huhdr-huident.
    ls_hus-guid_hu = ls_huhdr-guid_hu.
    ls_hus-hutyp   = ls_huhdr-letyp.
    ls_hus-hzmt    = ls_huhdr-hzmt.
    ls_hus-lgtyp   = ls_huhdr-lgtyp.

    LOOP AT lt_huref INTO ls_huref
                     where GUID_HU = ls_hus-guid_hu..
      ls_hus-docid   = ls_huref-docid.
      ls_hus-doccat  = ls_huref-doccat.
      ls_hus-door     = cs_unlo-door.

      APPEND ls_hus TO cs_admin-hus.
      CLEAR: ls_hus, ls_huref.

    ENDLOOP.
    CLEAR: ls_huhdr.
  ENDLOOP.

  SORT cs_admin-hus BY guid_hu docid.
  DELETE ADJACENT DUPLICATES FROM cs_admin-hus COMPARING guid_hu docid.

  PERFORM dlv_ncts_check
          CHANGING  cs_admin-hus
                    lt_sdat_ini_dlv
                    lt_sdat_ini_dlv.

ENDFORM.
