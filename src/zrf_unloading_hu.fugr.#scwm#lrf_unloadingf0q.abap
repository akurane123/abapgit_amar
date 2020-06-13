*&--------------------------------------------------------------------*
*&      Form  hu_read_and_lock
*&--------------------------------------------------------------------*
*       Read, Lock and Check HU
*---------------------------------------------------------------------*
*      -->IV_HUIDENT     HU Identification
*      -->IV_DOCID       Document number (Delivery GUID)
*      -->IV_LGNUM       Warehouse Number
*      -->IT_DELIVERIES  Selected Deliveries (GUIDs)
*      -->CS_HUHDR       HU Header
*      -->CV_HAZMAT_IND  Hazardous Material Indicator
*---------------------------------------------------------------------*
FORM hu_read_and_lock
  USING    iv_lgnum       TYPE /scwm/lgnum
           it_deliveries  TYPE /scwm/tt_rf_unlo_docid
  CHANGING cs_unlo        TYPE /SCWM/S_RF_UNLO
           cs_huhdr       TYPE /scwm/s_huhdr_int
           cs_admin_unlo  TYPE /scwm/s_rf_admin_unlo.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
  DATA: lv_whse_tzone TYPE tznzone,
        lv_ltrans     TYPE /scwm/de_ltrans,
        lv_secs       TYPE i,
        lv_stamp_low  TYPE tzntstmps,
        lv_stamp_high TYPE tzntstmps.

  DATA: ls_huref        TYPE /scwm/s_huref_int,
        ls_deliveries   TYPE /scwm/s_rf_unlo_docid, "#EC NEEDED
        ls_dlv_sel      TYPE /scwm/dlv_selection_str,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_prd_hdr      TYPE /scwm/dlv_header_out_prd_str,
        ls_unlo_hu      TYPE /scwm/s_rf_unlo_hus,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_partyloc     TYPE /scdl/dl_partyloc_str.

  DATA: lt_huref   TYPE /scwm/tt_huref_int,
        lt_dlv_sel TYPE /scwm/dlv_selection_tab,
        lt_prd_hdr TYPE /scwm/dlv_header_out_prd_tab,
        lt_prd_hdr_act  TYPE /scwm/dlv_header_out_prd_tab,
        lt_sdat_ini_dlv TYPE /scwm/tt_rf_unlo_docid.

  DATA: lo_packing TYPE REF TO /scwm/cl_wm_packing,
        lo_dlv     TYPE REF TO /scwm/cl_dlv_management_prd.

  FIELD-SYMBOLS: <fs_prd_hdr> TYPE /scwm/dlv_header_out_prd_str.

*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*
  IF lo_packing IS NOT BOUND.
    CREATE OBJECT lo_packing.
  ENDIF.


* read all necessary data: HU status
  CALL METHOD lo_packing->get_hu
    EXPORTING
      iv_huident = cs_unlo-huident
*     IV_GUID_HU =
      iv_lock    = gc_lock
    IMPORTING
*     ET_HUITM   =
      es_huhdr   = cs_huhdr
*     ET_HUHDR   =
*     ET_HUTREE  =
      et_huref   = lt_huref
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

* Lock issue happened give the actual message
  IF sy-msgno = 074 AND sy-msgid = '/SCWM/HU_WM'.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2.
  ENDIF.

* does HU exist in given warehouse number and is HU top HU?
  IF  sy-subrc = 1
  OR cs_huhdr-top IS INITIAL
  OR cs_huhdr-lgnum <> iv_lgnum.
    CLEAR cs_unlo-rfhu.
    MESSAGE e014(/scwm/rf_de) WITH cs_unlo-huident.
*   Handling Unit &1 ist not allowed
  ENDIF.

* determine HU status (unloaded y/n)
  CALL FUNCTION 'CRM_STATUS_CHECK'
    EXPORTING
      objnr             = cs_huhdr-guid_hu
      status            = wmegc_hustat_unloaded
    EXCEPTIONS
      object_not_found  = 1
      status_not_active = 2
      OTHERS            = 3.

* sy-subrc 0  ---->    unloaded
  IF sy-subrc = 0.
*   HU already unloaded -> error message
*     CLEAR cs_unlo.
     MESSAGE e018(/scwm/rf_de) WITH cs_huhdr-huident.
  ENDIF.

  lv_ltrans = /scwm/cl_rf_bll_srvc=>get_ltrans( ).

  IF lv_ltrans EQ gc_ltrans_uldosi.
*   create delivery object
    IF lo_dlv IS NOT BOUND.
      CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
        RECEIVING
          eo_instance = lo_dlv.
    ENDIF.

*   query deliveries
*   warehouse number
    ls_dlv_sel-fieldname = gc_dl_logfname_whno_i.
    ls_dlv_sel-option    = wmegc_option_eq.
    ls_dlv_sel-low       = iv_lgnum.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.
*    door
    ls_dlv_sel-fieldname = gc_dl_logfname_door_i.
    ls_dlv_sel-option    = wmegc_option_eq.
    ls_dlv_sel-low       = cs_unlo-door.
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.

*   DATE Selection
    CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
      EXPORTING
        iv_lgnum        = iv_lgnum
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

*   remember values
    gv_door_dd_fut  = cs_unlo-door_dd_fut.
    gv_door_dd_past = cs_unlo-door_dd_past.

*   Lowest Date
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

*   Hihgest Date
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

*   not blocked
    ls_dlv_sel-fieldname = gc_dl_logfname_value_dbc_h.
    ls_dlv_sel-option    = wmegc_option_eq.
    ls_dlv_sel-low       = gc_dl_status_value_unblocked. "not blocked
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.

*    status not "unloading finished"
    ls_dlv_sel-fieldname = gc_dl_logfname_value_dun_i.
    ls_dlv_sel-option    = wmegc_option_ne.
    ls_dlv_sel-low       = gc_dl_status_value_dlun.  "ungleich Entladen
    CLEAR ls_dlv_sel-high.
    APPEND ls_dlv_sel TO lt_dlv_sel.
*   D A T E
    ls_dlv_sel-fieldname = gc_dl_logfname_value_dlvpldat.
    ls_dlv_sel-option    = wmegc_option_bt.
    ls_dlv_sel-low       = lv_stamp_low.
    ls_dlv_sel-high      = lv_stamp_high.
    APPEND ls_dlv_sel TO lt_dlv_sel.

    CLEAR ls_dlv_sel.
    LOOP AT lt_huref INTO ls_huref.
      ls_dlv_sel-fieldname = gc_dl_logfname_docid.
      ls_dlv_sel-option    = wmegc_option_eq.
      ls_dlv_sel-low       = ls_huref-docid.
      APPEND ls_dlv_sel TO lt_dlv_sel.
    ENDLOOP.

    ls_read_options-keys_only = 'X'.
    ls_include_data-head_partyloc = 'X'.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_selection    = lt_dlv_sel
            iv_doccat       = wmegc_doccat_pdi
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_headers      = lt_prd_hdr.

      CATCH /scdl/cx_delivery.
        MESSAGE e212(/scwm/rf_de) WITH cs_unlo-door.
*       No deliveries assigned to door &1
    ENDTRY.

*   remove the planned status read and search for actual
    DELETE lt_dlv_sel INDEX 5.
*   actual D A T E
    ls_dlv_sel-fieldname = gc_dl_logfname_value_dlvactdat.
    ls_dlv_sel-option    = wmegc_option_bt.
    ls_dlv_sel-low       = lv_stamp_low.
    ls_dlv_sel-high      = lv_stamp_high.
    APPEND ls_dlv_sel TO lt_dlv_sel.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_selection    = lt_dlv_sel
            iv_doccat       = wmegc_doccat_pdi
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_headers      = lt_prd_hdr_act.

      CATCH /scdl/cx_delivery.
        MESSAGE e212(/scwm/rf_de) WITH cs_unlo-door.
*       No deliveries assigned to door &1
    ENDTRY.

    APPEND LINES OF lt_prd_hdr_act TO lt_prd_hdr.
    SORT lt_prd_hdr BY docid.
    DELETE ADJACENT DUPLICATES FROM lt_prd_hdr COMPARING docid.

    IF LINES( lt_prd_hdr ) = 0.
      MESSAGE e219(/scwm/rf_de) WITH cs_unlo-door.
    ENDIF.

*   prepare worklist (list of deliveries assigned to door)
    LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
      ls_deliveries-door   = cs_unlo-door.
      ls_deliveries-docid  = <fs_prd_hdr>-docid.
      ls_deliveries-doccat = <fs_prd_hdr>-doccat.
      ls_deliveries-doctype = <fs_prd_hdr>-doctype.

      READ TABLE <fs_prd_hdr>-partyloc INTO ls_partyloc
           WITH KEY party_role = /scdl/if_dl_partyloc_c=>sc_party_role_wh.
      IF sy-subrc = 0.
        MOVE ls_partyloc-locationid TO ls_deliveries-wh_locationid.
        MOVE ls_partyloc-locationno TO ls_deliveries-wh_locationno.
      ENDIF.

      APPEND ls_deliveries TO cs_admin_unlo-deliveries.
    ENDLOOP.

    SORT cs_admin_unlo-deliveries BY docid.
    DELETE ADJACENT DUPLICATES FROM cs_admin_unlo-deliveries COMPARING docid.

    CLEAR ls_deliveries.

*   create table of HUs
    ls_unlo_hu-huident = cs_huhdr-huident.
    ls_unlo_hu-guid_hu = cs_huhdr-guid_hu.
    ls_unlo_hu-hutyp   = cs_huhdr-letyp.
    ls_unlo_hu-hzmt    = cs_huhdr-hzmt.
    ls_unlo_hu-lgtyp   = cs_huhdr-lgtyp.
    LOOP AT lt_huref INTO ls_huref
                     WHERE guid_hu = cs_huhdr-guid_hu.
      ls_unlo_hu-docid   = ls_huref-docid.
      ls_unlo_hu-doccat  = ls_huref-doccat.
      ls_unlo_hu-door    = cs_unlo-door.

      APPEND ls_unlo_hu TO cs_admin_unlo-hus.
      CLEAR ls_huref.
    ENDLOOP.

    SORT cs_admin_unlo-hus BY guid_hu docid.
    DELETE ADJACENT DUPLICATES FROM cs_admin_unlo-hus COMPARING guid_hu docid.

    CLEAR ls_unlo_hu.

    PERFORM dlv_ncts_check
           CHANGING cs_admin_unlo-hus
                    cs_admin_unlo-deliveries
                    lt_sdat_ini_dlv.

*   set delivery status to 'begin unload'
    PERFORM delivery_status_set
      USING cs_admin_unlo-deliveries
            gc_dl_begin_unload
            iv_lgnum
            lt_sdat_ini_dlv.
  ENDIF.

* is HU in predecessor document (delivery, door, TU, shipment)?
  SORT lt_huref BY guid_hu.
  READ TABLE lt_huref
       INTO ls_huref
       WITH KEY guid_hu = cs_huhdr-guid_hu
       BINARY SEARCH.

  lv_ltrans = /scwm/cl_rf_bll_srvc=>get_ltrans( ).

  READ TABLE it_deliveries
       INTO ls_deliveries
       WITH KEY docid = ls_huref-docid.

  IF sy-subrc <> 0.
    READ TABLE lt_prd_hdr
         INTO ls_prd_hdr
         WITH KEY docid = ls_huref-docid.
    IF sy-subrc <> 0 AND
       lv_ltrans <> gc_ltrans_ulsysg. " The error message is not relevant in system guided mode
      MESSAGE e014(/scwm/rf_de) WITH cs_unlo-huident.
*     Handling Unit &1 ist not allowed
    ENDIF.
  ENDIF.
  MOVE: ls_huref-doccat TO cs_unlo-rdoccat,
        ls_huref-docid TO cs_unlo-rdocid.

* does HU contain hazardous material?
  CALL FUNCTION '/SCWM/RF_HAZMAT_IND_READ'
    EXPORTING
      iv_hazmat     = cs_huhdr-hzmt
    IMPORTING
      ev_hazmat_ind = cs_unlo-hazmat_ind.

ENDFORM.                    "hu_read_and_lock
