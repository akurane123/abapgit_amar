FUNCTION z_rf_confirm_decon_wh_task1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_QNAME) TYPE  UNAME DEFAULT SY-UNAME
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_TANUM) TYPE  /SCWM/TANUM OPTIONAL
*"     VALUE(IV_SOURCE_HU) TYPE  /SCWM/HUIDENT
*"     VALUE(IV_DEST_HU) TYPE  /SCWM/HUIDENT
*"     VALUE(IV_QTY) TYPE  /SCWM/LTAP_NISTA
*"  EXPORTING
*"     VALUE(E_SEVERITY) TYPE  BAPI_MTYPE
*"     VALUE(E_MESSAGE) TYPE  BAPI_MSG
*"--------------------------------------------------------------------


  DATA: lt_ltap_vb  TYPE /scwm/tt_ltap_vb,
        lt_bapiret  TYPE bapirettab,
        lv_severity TYPE bapi_mtype,
        lv_ser_err  TYPE xfeld.

  DATA: ls_ordim_o TYPE  /scwm/ordim_o,
        lt_ordim_c TYPE  /scwm/tt_ordim_c.


  DATA: lt_huitems_dest   TYPE  /scwm/tt_huitm_int,
        ls_huhdr_dest     TYPE /scwm/s_huhdr_int,
        lt_huhdr_dest     TYPE /scwm/tt_huhdr_int,
        lt_huitems_source TYPE  /scwm/tt_huitm_int,
        ls_huhdr_source   TYPE /scwm/s_huhdr_int,
        lt_huhdr_source   TYPE /scwm/tt_huhdr_int.

  DATA: lt_conf   TYPE /scwm/to_conf_tt,
        ls_conf   TYPE /scwm/to_conf,
        lv_wtcode TYPE  /scwm/de_wtcode.

  DATA: lv_lgpla TYPE /scwm/lgpla.
  DATA: ls_rsrc TYPE /scwm/rsrc.

  DATA: ls_sprd TYPE  /scwm/s_rf_sprd,
        ls_wrkc TYPE /scwm/tworkst.

  DATA: gs_workstation TYPE /scwm/tworkst,
        gs_worksttyp   TYPE /scwm/twrktyp,
        ls_quan        TYPE /scwm/s_quan,
        pa_wrkst       TYPE /scwm/s_wrk_spread-workstation.

  DATA: lo_wm_pack TYPE REF TO /scwm/cl_wm_packing.
* get instance and initialize log
  DATA: ls_pack_controle TYPE /scwm/s_pack_controle.


  DATA: lt_ordim_prod TYPE /scwm/tt_ordim_o,
        lt_ordim_hu   TYPE /scwm/tt_ordim_o.

  CONSTANTS: cv_wtcode   TYPE /scwm/de_wtcode VALUE 'B',
             lv_pack_mat TYPE /scwm/de_ui_id VALUE 'EURO PALLET'.
*********************************************************************

* check if Source HU exist
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_source_hu
    IMPORTING
      output = iv_source_hu.

* Get Source HU details
  CALL FUNCTION '/SCWM/HU_READ'
    EXPORTING
      iv_appl    = 'WME'
      iv_lgnum   = iv_lgnum
      iv_huident = iv_source_hu
    IMPORTING
      et_huitm   = lt_huitems_source
      es_huhdr   = ls_huhdr_source
      et_huhdr   = lt_huhdr_source
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
* check if Deconsolidation task is confirmed
*     Get list of open TO for HU as source HU
    PERFORM check_confirm_open_task USING iv_lgnum
                                          iv_source_hu.


* set default values
    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_rsrc.

    IF ls_rsrc-lgnum IS INITIAL.
      ls_rsrc-lgnum = iv_lgnum.
    ENDIF.

    ls_sprd-lgnum = ls_rsrc-lgnum.
    ls_wrkc-lgnum = ls_rsrc-lgnum.
    ls_sprd-workstation = 'DEKO'.
* Set Warehouse Number
    CALL METHOD /scwm/cl_tm=>set_lgnum( ls_rsrc-lgnum ).


    CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
      EXPORTING
        iv_lgnum       = ls_sprd-lgnum
        iv_workstation = ls_sprd-workstation
      IMPORTING
        es_workst      = ls_wrkc
      EXCEPTIONS
        error          = 1
        not_found      = 2
        OTHERS         = 3.

    IF sy-subrc NE 0.
      MESSAGE e040(/scwm/rf_de).
    ENDIF.


    IF lo_wm_pack IS NOT BOUND.
      CALL METHOD /scwm/cl_wm_packing=>get_instance
        IMPORTING
          eo_instance = lo_wm_pack.
    ENDIF.

* Determine processor from sy-user in case LM is activ
    ls_pack_controle-processor_det = 'X'.
* Initialize the workstation for Close HU
    CALL METHOD lo_wm_pack->init_by_workstation
      EXPORTING
        is_workstation   = ls_wrkc
        is_pack_controle = ls_pack_controle
        iv_no_hu_by_wc   = 'X'
      EXCEPTIONS
        error            = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ENDIF.


* Close source HU for Process step to be completed
    ASSIGN lt_huhdr_source[ 1 ] TO FIELD-SYMBOL(<fs_huhdr_source>).
    IF sy-subrc = 0.
* Check if TO number is provided
      CALL FUNCTION '/SCWM/WC_WT_DECONS'
        EXPORTING
          iv_hu_high       = <fs_huhdr_source>-guid_hu
          is_workstation   = ls_wrkc
        IMPORTING
          et_ordim_tprod   = lt_ordim_prod
          et_ordim_thu     = lt_ordim_hu
        EXCEPTIONS
          error            = 1
          object_not_found = 2
          no_wt            = 3
          OTHERS           = 4.
      IF sy-subrc <> 0 AND sy-subrc <> 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.
      ASSIGN lt_ordim_prod[ 1 ] TO FIELD-SYMBOL(<fs_ordim_prod>).
      IF sy-subrc = 0.
        iv_tanum = <fs_ordim_prod>-tanum.
      ENDIF.

    ENDIF.



************************************************************
* check if destination HU is created
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_dest_hu
      IMPORTING
        output = iv_dest_hu.

* Get Destination HU details
    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = 'WME'
        iv_lgnum   = iv_lgnum
        iv_huident = iv_dest_hu
      IMPORTING
        et_huitm   = lt_huitems_dest
        es_huhdr   = ls_huhdr_dest
        et_huhdr   = lt_huhdr_dest
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.

* if Destination HU not found then create one
    IF sy-subrc = 1.
      lv_lgpla = 'DECON'.

      CALL FUNCTION 'ZMV_CREATE_HU'
        EXPORTING
          iv_hu       = iv_dest_hu
          iv_pack_mat = lv_pack_mat
          iv_location = lv_lgpla
        IMPORTING
          es_huhdr    = ls_huhdr_dest.

      IF sy-subrc = 0.
        APPEND ls_huhdr_dest TO lt_huhdr_dest.
      ENDIF.
    ENDIF.


    ls_quan-unit = 'EA'.
    ls_quan-quan = iv_qty.

    ASSIGN lt_huhdr_dest[ 1 ] TO FIELD-SYMBOL(<fs_huhdr_dest>).
    IF sy-subrc = 0.
      CALL METHOD lo_wm_pack->pack_by_to
        EXPORTING
          iv_tanum   = iv_tanum
          iv_hu_dest = <fs_huhdr_dest>-guid_hu
          is_quan    = ls_quan
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      IF sy-subrc = 0.

        CALL METHOD lo_wm_pack->/scwm/if_pack_bas~save
          EXPORTING
            iv_commit = 'X'
            iv_wait   = 'X'
          EXCEPTIONS
            error     = 1
            OTHERS    = 2.

        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
      ENDIF.
    ENDIF.

* Close HU's after processing
    CALL METHOD lo_wm_pack->hu_process_completed
      EXPORTING
        iv_hu  = <fs_huhdr_source>-guid_hu
      EXCEPTIONS
        error  = 1
        OTHERS = 2.


* Close Dest HU for Process step to be completed
    CALL METHOD lo_wm_pack->hu_process_completed
      EXPORTING
        iv_hu  = <fs_huhdr_dest>-guid_hu
      EXCEPTIONS
        error  = 1
        OTHERS = 2.


    CALL METHOD lo_wm_pack->/scwm/if_pack_bas~save
      EXPORTING
        iv_commit = 'X'
        iv_wait   = 'X'
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.


  ELSE.
    lv_severity = 'E'.
    CONCATENATE 'Source SU' iv_source_hu 'does not exist'
    INTO e_message SEPARATED BY space.
    TRANSLATE e_message TO UPPER CASE.
    e_severity = lv_severity.
    EXIT.
  ENDIF.

ENDFUNCTION.
