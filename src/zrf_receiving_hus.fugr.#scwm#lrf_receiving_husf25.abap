*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF25 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_for_other_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->HUIDENT  text
*      <--CV_EXIST_IN_OTHER_DELIVERY  text
*----------------------------------------------------------------------*
FORM check_for_other_delivery  USING    iv_huident TYPE /scwm/de_rf_huident
                                        iv_lgnum   TYPE /scwm/lgnum
                               CHANGING cv_exist_in_other_delivery TYPE xfeld.


  DATA: ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_selection      TYPE /scwm/dlv_selection_str,
        lt_selection      TYPE /scwm/dlv_selection_tab,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        lt_prd_hdr  TYPE /scwm/dlv_header_out_prd_tab
        .

  DATA: ls_qdoccat_r  TYPE  rsdsselopt,
        lt_qdoccat_r  TYPE  rseloption,
        ls_huident_r  TYPE  rsdsselopt,
        lt_huident_r  TYPE  rseloption,
        lt_huitm      TYPE  /scwm/tt_huitm_int,
        ls_huitm      TYPE  /scwm/s_huitm_int.


  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.

  CLEAR  cv_exist_in_other_delivery.

  ls_huident_r-sign   = wmegc_sign_inclusive.
  ls_huident_r-option = wmegc_option_eq.
  ls_huident_r-low    = iv_huident.
  APPEND ls_huident_r TO lt_huident_r.


  ls_qdoccat_r-sign   = wmegc_sign_inclusive.
  ls_qdoccat_r-option = wmegc_option_eq.
  ls_qdoccat_r-low    = /scdl/if_dl_doc_c=>sc_doccat_inb_prd.
  APPEND ls_qdoccat_r TO lt_qdoccat_r.
  CALL FUNCTION '/SCWM/HU_SELECT_GEN'
    EXPORTING
      iv_lgnum     = iv_lgnum
      ir_huident   = lt_huident_r
      ir_qdoccat   = lt_qdoccat_r
    IMPORTING
      et_huitm     = lt_huitm
*        et_huref     = lt_huref
    EXCEPTIONS
      wrong_input  = 1
      not_possible = 2
      OTHERS       = 3.

  IF sy-subrc <> 0.                                         "#EC *
  ENDIF.

  READ TABLE lt_huitm INTO ls_huitm INDEX 1.

  IF sy-subrc <> 0.                     "#EC NEEDED
  ENDIF.


*XSEL_IBD I EQ  X
*HUNO I EQ  SZA01
*STATUS_VALUE_DGR_I I EQ  1
*STATUS_VALUE_DWA_I I EQ  1

  CLEAR ls_selection.
  ls_selection-fieldname =
    /scdl/if_dl_logfname_c=>sc_status_value_dgr_i.
  ls_selection-low    = /scdl/if_dl_status_c=>sc_v_not_started.
  ls_selection-sign   = wmegc_sign_inclusive.
  ls_selection-option = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname =
    /scdl/if_dl_logfname_c=>sc_status_value_dwa_i.
  ls_selection-low    = /scdl/if_dl_status_c=>sc_v_not_started.
  ls_selection-sign   = wmegc_sign_inclusive.
  ls_selection-option = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname =
    /scdl/if_dl_logfname_c=>sc_docid_h.
  ls_selection-low    = ls_huitm-qdocid.
  ls_selection-sign   = wmegc_sign_inclusive.
  ls_selection-option = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = /scwm/if_dl_logfname_c=>sc_prodind_prdi_h.
  ls_selection-sign      = wmegc_sign_inclusive.
  ls_selection-option    = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.


  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.
  ls_include_data-item_status = 'X'.
  ls_include_data-item_partyloc = 'X'.
  ls_include_data-head_status = 'X'.
  ls_include_data-head_status_dyn = 'X'.

* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_selection    = lt_selection
          iv_doccat       = wmegc_doccat_pdi
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = lt_prd_hdr.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
* ????
  ENDTRY.

  IF lt_prd_hdr IS NOT INITIAL.
    cv_exist_in_other_delivery = 'X'.
  ENDIF.


ENDFORM.                    " check_for_other_delivery
