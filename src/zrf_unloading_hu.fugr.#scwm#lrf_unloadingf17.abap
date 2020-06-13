*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF17 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HU_UNLOADED_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LGNUM    text
*      -->CS_UNLO  text
*----------------------------------------------------------------------*
FORM hu_unloaded_check  USING    iv_lgnum   TYPE /scwm/lgnum
                        CHANGING cs_unlo TYPE /scwm/s_rf_unlo.

  DATA ls_huhdr TYPE /scwm/s_huhdr_int.

  CALL FUNCTION '/SCWM/HU_READ'
    EXPORTING
      iv_appl    = wmegc_huappl_wme
      iv_lgnum   = iv_lgnum
      iv_huident = cs_unlo-huident
    IMPORTING
      es_huhdr   = ls_huhdr
    EXCEPTIONS
      error      = 1
      OTHERS     = 99.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* determine HU status (unloaded y/n)
  CALL FUNCTION 'CRM_STATUS_CHECK'
    EXPORTING
      objnr             = ls_huhdr-guid_hu
      status            = wmegc_hustat_unloaded
    EXCEPTIONS
      object_not_found  = 1
      status_not_active = 2
      OTHERS            = 3.

* sy-subrc 0  ---->    unloaded

  IF sy-subrc = 0.
*   HU already unloaded -> error message
    CLEAR cs_unlo.
    MESSAGE e018(/scwm/rf_de) WITH ls_huhdr-huident.
  ENDIF.


ENDFORM.                    " HU_UNLOADED_CHECK

FORM opunit_check USING    is_unlo_prod  TYPE /scwm/s_rf_unlo_prod
                           is_admin_unlo TYPE /scwm/s_rf_admin_unlo
                           iv_entitled   TYPE /scwm/de_entitled
                  CHANGING cv_opunit     TYPE /scwm/de_rf_opunit.

  DATA: lv_badiopunit       TYPE /scwm/de_opunit.

  DATA: ls_ltap       TYPE /scwm/ltap,
        ls_who        TYPE /scwm/s_who_int,
        ls_mat_hazard TYPE /scwm/s_material_hazard,
        ls_mat_lgnum  TYPE /scwm/s_material_lgnum,
        ls_mat_global TYPE /scwm/s_material_global,
        lt_mat_uom    TYPE /scwm/tt_material_uom,
        ls_t331       TYPE /scwm/t331.

  DATA: go_badi_opunit      TYPE REF TO /scwm/ex_core_rms_opunit.

  TRY.
      GET BADI go_badi_opunit
        FILTERS
          lgnum = is_admin_unlo-lgnum.
    CATCH cx_badi.                                      "#EC NO_HANDLER
  ENDTRY.

  ls_ltap-matid         = is_unlo_prod-matid.
  ls_ltap-matnr         = is_unlo_prod-matnr.
  ls_ltap-charg         = is_unlo_prod-charg.
  ls_ltap-batchid       = is_unlo_prod-batchid.
  ls_ltap-altme         = is_unlo_prod-altme.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = ls_ltap-matnr
    IMPORTING
      output = ls_ltap-matnr.

  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = is_unlo_prod-matid
          iv_lgnum      = is_admin_unlo-lgnum
          iv_entitled   = iv_entitled
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
