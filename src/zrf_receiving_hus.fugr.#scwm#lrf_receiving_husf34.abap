*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF34 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  hu_number_range_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_REHU_HU  text
*----------------------------------------------------------------------*
FORM hu_number_range_check  USING    cs_rehu_hu TYPE /scwm/s_rf_rehu_hu
                                     iv_lgnum TYPE /scwm/lgnum.


  DATA: lt_nriv TYPE TABLE OF inriv.
  DATA: lv_txt  TYPE string.
  DATA: ls_nriv TYPE inriv,
        lv_ok   TYPE xfeld VALUE abap_false.


* first check for SSCC
  CALL FUNCTION '/SCWM/SSCC_CHECK'
    EXPORTING
      iv_sscc         = cs_rehu_hu-huident
      iv_lgnum        = iv_lgnum
      iv_foreign_sscc = ' '
    EXCEPTIONS
      OTHERS          = 9.

* could be a foreign SSCC
  IF sy-subrc <> 0.
    CALL FUNCTION '/SCWM/SSCC_CHECK'
      EXPORTING
        iv_sscc                 = cs_rehu_hu-huident
        iv_lgnum                = iv_lgnum
        iv_foreign_sscc         = 'X'
      EXCEPTIONS
        iln_not_found           = 1
        invalid_call            = 2
        invalid_customizing     = 3
        invalid_iln             = 4
        invalid_no_out_of_range = 5
        invalid_sscc            = 6
        internal_error          = 7
        error_on_number_range   = 8
        OTHERS                  = 9.

*   not an SSCC number
    IF sy-subrc <> 0.

*...  read all intervalls
      CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
        EXPORTING
          object              = '/SCWM/HUID'
          subobject           = iv_lgnum
        TABLES
          interval            = lt_nriv
        EXCEPTIONS
          object_not_found    = 6
          subobject_not_found = 8
          OTHERS              = 99.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_txt.
      ENDIF.

      LOOP AT lt_nriv INTO ls_nriv.
        CHECK cs_rehu_hu-huident > ls_nriv-fromnumber AND
              cs_rehu_hu-huident < ls_nriv-tonumber.
        IF ls_nriv-externind IS INITIAL.  "internal intervall

          IF cs_rehu_hu-huident > ls_nriv-nrlevel.
            MESSAGE e043(/scwm/hu_wm) WITH cs_rehu_hu-huident .
          ENDIF.
          lv_ok = abap_true.
        ELSE.   "external intervall = OK
          lv_ok = abap_true.
        ENDIF.
        EXIT.
      ENDLOOP.
      CHECK lv_ok = abap_false.
* If there are no external interval maintained - all external numbers
* are allowed
        READ TABLE lt_nriv TRANSPORTING NO FIELDS
                       WITH KEY externind = 'X'.
        CHECK sy-subrc IS INITIAL.
      CLEAR cs_rehu_hu-vlenr_verif.
      MESSAGE e085(/scwm/hu_wm) WITH cs_rehu_hu-huident.
    ENDIF.
  ENDIF.




ENDFORM.                    " hu_number_range_check

FORM opunit_check USING    is_rehu_prod  TYPE /scwm/s_rf_rehu_prod
                           is_rehu       TYPE /scwm/s_rf_admin_rehu
                  CHANGING cv_opunit     TYPE /scwm/de_rf_opunit.

  DATA: lv_badiopunit       TYPE /scwm/de_opunit.

  DATA: ls_ltap             TYPE /scwm/ltap,
        ls_who              TYPE /scwm/s_who_int,
        ls_mat_hazard       TYPE /scwm/s_material_hazard,
        ls_mat_lgnum        TYPE /scwm/s_material_lgnum,
        ls_mat_global       TYPE /scwm/s_material_global,
        lt_mat_uom          TYPE /scwm/tt_material_uom,
        ls_t331             TYPE /scwm/t331.

  DATA: go_badi_opunit      TYPE REF TO /scwm/ex_core_rms_opunit.

  TRY.
      GET BADI go_badi_opunit
        FILTERS
          lgnum = is_rehu-lgnum.
    CATCH cx_badi.                                      "#EC NO_HANDLER
  ENDTRY.

  ls_ltap-matid         = is_rehu_prod-matid.
  ls_ltap-matnr         = is_rehu_prod-matnr.
  ls_ltap-charg         = is_rehu_prod-charg.
  ls_ltap-batchid       = is_rehu_prod-batchid.
  ls_ltap-altme         = is_rehu_prod-altme.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = ls_ltap-matnr
    IMPORTING
      output = ls_ltap-matnr.

  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = is_rehu_prod-matid
          iv_lgnum      = is_rehu-lgnum
        IMPORTING
          es_mat_global = ls_mat_global
          et_mat_uom    = lt_mat_uom.

    CATCH /scwm/cx_md.
  ENDTRY.

* set operational UOM via customer BAdi
  TRY.
      CALL BADI go_badi_opunit->opunit
        EXPORTING
          iv_call       = wmegc_call_wt
          is_ltap       = ls_ltap
          is_mat_global = ls_mat_global
          is_mat_lgnum  = ls_mat_lgnum
          is_mat_hazard = ls_mat_hazard
          it_mat_uom    = lt_mat_uom
          is_t331       = ls_t331
        IMPORTING
          ev_opunit     = cv_opunit.
    CATCH cx_badi.                                      "#EC NO_HANDLER
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_BC_BY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM GET_ITEM_BC_BY_TYPE  USING    iv_itemtype TYPE /scdl/dl_itemtype
                                   iv_doccat   TYPE /scdl/dl_doccat
                          CHANGING es_item_bc
                                       TYPE /scdl/dl_itype_detail_str.

  DATA: lo_service      TYPE REF TO /scdl/cl_af_management,
        lo_bc_access    TYPE REF TO /scdl/if_af_business_conf,
        ls_item_extkey  TYPE        /scdl/dl_itmtype_extkey_str.

  CLEAR es_item_bc.

  lo_service = /scdl/cl_af_management=>get_instance( ).
  IF lo_service IS NOT BOUND.
    RETURN.
  ENDIF.

  TRY.
      lo_bc_access ?= lo_service->get_service(
                        /scdl/if_af_management_c=>sc_business_conf ).
    CATCH /scdl/cx_af_management.                       "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* get business configuration
  ls_item_extkey-item_type = iv_itemtype.
  ls_item_extkey-category  = iv_doccat.

  TRY.
      lo_bc_access->get_item_bc_by_type(
        EXPORTING
          is_item_extkey = ls_item_extkey
        IMPORTING
          es_itype       = es_item_bc ).
    CATCH /scdl/cx_af_business_conf.                    "#EC NO_HANDLER
      RETURN.
  ENDTRY.

ENDFORM.                    " GET_ITEM_BC_BY_TYPE
