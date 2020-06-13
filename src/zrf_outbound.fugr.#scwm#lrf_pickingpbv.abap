*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PICKINGPBV .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PBV_PIHUIN_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for pick-hu introduction screen
*         /SCWM/SAPLRF_PBV 0200
*----------------------------------------------------------------------*
*      -->IS_ORDIM_CONFIRM  WT application data
*      <--CT_RF_PICK_HUS    Pick-HU table
*      <--P_WHO             WO information
*----------------------------------------------------------------------*
FORM pbv_pihuin_pbo  USING    is_ordim_confirm TYPE /scwm/s_rf_ordim_confirm
                     CHANGING ct_rf_pick_hus   TYPE /scwm/tt_rf_pick_hus
                              cs_who           TYPE /scwm/s_who_int.

  DATA: lv_msg             TYPE string,
        lv_text_1          TYPE string,
        lv_text_2          TYPE string,
        lv_pmat_1          TYPE string,
        lv_pmat_2          TYPE string,
        lv_pmat_guid_1     TYPE /scwm/de_matid,
        lv_pmat_guid_2     TYPE /scwm/de_matid,
        lv_num_pmat_1      TYPE i,
        lv_num_pmat_2      TYPE i,
        lv_num_pmat_1_c(3) TYPE c,
        lv_num_pmat_2_c(3) TYPE c,
        lv_data_entry      TYPE /scwm/de_data_entry.
  DATA: ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_mat_global TYPE /scwm/s_material_global.
  DATA: lt_listbox   TYPE /scwm/tt_rf_listbox,
        lt_index_pbv TYPE /scwm/tt_rf_index_pbv.

  FIELD-SYMBOLS: <ls_listbox>     TYPE /scwm/s_rf_listbox,
                 <ls_rf_pick_hus> TYPE /scwm/s_rf_pick_hus.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice set the storage type
  IF cs_who-lgtyp IS INITIAL.
    cs_who-lgtyp = is_ordim_confirm-vltyp.
  ENDIF.

* On Pick by Voice set the prompt text
  CALL METHOD /scwm/cl_rf_bll_srvc=>get_listbox
    EXPORTING
      iv_field   = '/SCWM/S_RF_NESTED-PMAT'
    RECEIVING
      rt_listbox = lt_listbox.

  READ TABLE lt_listbox ASSIGNING <ls_listbox> INDEX 1.
  IF sy-subrc = 0.
*   Count how much we have of this PMat
    lv_pmat_1 = <ls_listbox>-value.
    LOOP AT ct_rf_pick_hus ASSIGNING <ls_rf_pick_hus>.
      IF <ls_rf_pick_hus>-pmat = <ls_listbox>-value.
        lv_pmat_guid_1 = <ls_rf_pick_hus>-pmat_guid.
        IF <ls_rf_pick_hus>-huident IS NOT INITIAL.
          ADD 1 TO lv_num_pmat_1.
        ELSE.
          DELETE ct_rf_pick_hus.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_listbox ASSIGNING <ls_listbox> INDEX 2.
  IF sy-subrc = 0.
*   Count how much we have of this PMat
    lv_pmat_2 = <ls_listbox>-value.
    LOOP AT ct_rf_pick_hus ASSIGNING <ls_rf_pick_hus>.
      IF <ls_rf_pick_hus>-pmat = <ls_listbox>-value.
        lv_pmat_guid_2 = <ls_rf_pick_hus>-pmat_guid.
        IF <ls_rf_pick_hus>-huident IS NOT INITIAL.
          ADD 1 TO lv_num_pmat_2.
        ELSE.
          DELETE ct_rf_pick_hus.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lv_num_pmat_1 IS NOT INITIAL.
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = lv_pmat_guid_1
            iv_langu      = sy-langu
            iv_entitled   = is_ordim_confirm-entitled
            iv_lgnum      = is_ordim_confirm-lgnum
          IMPORTING
            es_mat_global = ls_mat_global.

      CATCH /scwm/cx_md_interface.
*       one/more parameter(s) missing
        MESSAGE e004 WITH lv_pmat_guid_1.
      CATCH /scwm/cx_md_material_exist.
*       Material does not exist
        MESSAGE e004 WITH lv_pmat_guid_1.
      CATCH /scwm/cx_md_lgnum_locid.
*       Warehouse number is not assigned to an APO Location
        MESSAGE e004 WITH lv_pmat_guid_1.
      CATCH /scwm/cx_md.
        MESSAGE e004 WITH lv_pmat_guid_1.
    ENDTRY.

    lv_num_pmat_1_c = lv_num_pmat_1.
    CONDENSE lv_num_pmat_1_c.
    CONCATENATE lv_num_pmat_1_c ls_mat_global-maktx INTO lv_text_1
      SEPARATED BY space.
  ENDIF.
  IF lv_num_pmat_2 IS NOT INITIAL.
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = lv_pmat_guid_2
            iv_langu      = sy-langu
            iv_entitled   = is_ordim_confirm-entitled
            iv_lgnum      = is_ordim_confirm-lgnum
          IMPORTING
            es_mat_global = ls_mat_global.

      CATCH /scwm/cx_md_interface.
*       one/more parameter(s) missing
        MESSAGE e004 WITH lv_pmat_guid_1.
      CATCH /scwm/cx_md_material_exist.
*       Material does not exist
        MESSAGE e004 WITH lv_pmat_guid_1.
      CATCH /scwm/cx_md_lgnum_locid.
*       Warehouse number is not assigned to an APO Location
        MESSAGE e004 WITH lv_pmat_guid_1.
      CATCH /scwm/cx_md.
        MESSAGE e004 WITH lv_pmat_guid_1.
    ENDTRY.

    lv_num_pmat_2_c = lv_num_pmat_2.
    CONDENSE lv_num_pmat_2_c.
    CONCATENATE lv_num_pmat_2_c ls_mat_global-maktx INTO lv_text_2
      SEPARATED BY space.
  ENDIF.

* New order, storage type T010, take 2 europallet
  IF lv_num_pmat_1 IS INITIAL.
    MESSAGE i597 WITH cs_who-lgtyp INTO lv_msg.
  ELSE.
    MESSAGE i561 WITH cs_who-lgtyp lv_text_1 lv_text_2 INTO lv_msg.
  ENDIF.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'SCREEN' iv_sf = 'SCR_PROMPT' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
  ls_screlm_pbv-scr_prompt = lv_msg.

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIHUIN_PBO
*&---------------------------------------------------------------------*
*&      Form  PBV_PIHUTO_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for source HU-WT screen
*         /SCWM/SAPLRF_PBV 0502
*----------------------------------------------------------------------*
*      <--IS_ORDIM_CONFIRM  WT application data
*----------------------------------------------------------------------*
FORM pbv_pihuto_pbo  CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_msg        TYPE string,
        lv_data_entry TYPE /scwm/de_data_entry,
        lv_aisle_not  TYPE xfeld,
        lv_stack_not  TYPE xfeld,
        lv_lvl_v_not  TYPE xfeld,
        lv_min_digits TYPE i,
        lv_lgpla_prev TYPE /scwm/de_lgpla_prev,
        lv_lgtyp_prev TYPE /scwm/de_lgtyp_prev.
  DATA: ls_lagp       TYPE /scwm/lagp,
        ls_lagp_prev  TYPE /scwm/lagp,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_rsrc       TYPE /scwm/rsrc,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice fill aisle, stack and level
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = cs_ordim_confirm-vlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cs_ordim_confirm-aisle = ls_lagp-aisle.
  cs_ordim_confirm-stack = ls_lagp-stack.
* If level is not filled, we have to send user to complete bin
  IF ls_lagp-aisle IS INITIAL AND
     ls_lagp-lvl_v IS INITIAL.
    cs_ordim_confirm-lvl_v = ls_lagp-lgpla.
  ELSE.
    cs_ordim_confirm-lvl_v = ls_lagp-lvl_v.
  ENDIF.
  CONDENSE cs_ordim_confirm-aisle.
  CONDENSE cs_ordim_confirm-stack.
  CONDENSE cs_ordim_confirm-lvl_v.

  CALL FUNCTION '/SCWM/RF_PICK_GET_GLOBVAR'
    IMPORTING
      ev_lgpla_prev = lv_lgpla_prev
      ev_lgtyp_prev = lv_lgtyp_prev.

  IF lv_lgpla_prev IS NOT INITIAL.
    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_lgpla      = lv_lgpla_prev
      IMPORTING
        es_lagp       = ls_lagp_prev
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* Switch on input for all parts
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).

* Check the storage bin delta between prev. and actual storage bin
  IF ls_lagp_prev-lgtyp = ls_lagp-lgtyp.
    IF ls_lagp_prev-aisle = ls_lagp-aisle.
*     Switch off input for aisle
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
      lv_aisle_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
      lv_stack_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack AND
       ls_lagp_prev-lvl_v = ls_lagp-lvl_v AND
       ls_lagp_prev-lvl_v IS NOT INITIAL.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).
      lv_lvl_v_not = abap_true.
    ENDIF.
  ENDIF.

* Fill prompt text, grammar (validation values) and help text

* Clear previously set screen prompt.
  CLEAR ls_screlm_pbv-scr_prompt.

  IF lv_aisle_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp.
*     Go to aisle &1 in storage type &4
      MESSAGE i559
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
             ls_lagp-lgtyp              " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to aisle &1
      MESSAGE i550
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_1 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_1' ).

    ls_screlm_pbv-grammar_1 = cs_ordim_confirm-aisle.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'AISLE'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_1.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_stack_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
       lv_aisle_not IS NOT INITIAL.
*     Go to stack &2 in storage type &4
      MESSAGE i571
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to stack &2
      MESSAGE i551
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_2 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'GRAMMAR_2' iv_pt = wmegc_pt_grammar ).

    IF ls_lagp-lvl_v IS INITIAL.  "Stack filled but no level
      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
      ENDIF.
    ELSE.
      ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
    ENDIF.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'STACK'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_2.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'HELP_2' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_lvl_v_not IS INITIAL.
    IF ls_lagp-aisle IS INITIAL AND
       ls_lagp-lvl_v IS INITIAL.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to storage bin &1 in storage type &2
        MESSAGE i573 WITH cs_ordim_confirm-vlpla ls_lagp-lgtyp INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ELSE.
*       Go to storage bin &1
        MESSAGE i553 WITH cs_ordim_confirm-vlpla INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lgpla.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'VLPLA'
          iv_tablename = '/SCWM/ORDIM_O'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ELSE.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to level &3 in storage type &4
        MESSAGE i572
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ELSE.
*       Go to level &3
        MESSAGE i552
            WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
                 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lvl_v.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'LVL_V'
          iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ENDIF.
  ENDIF.

* Check if we have HU on the source bin
  IF cs_ordim_confirm-vlenr IS INITIAL AND
     gv_huobl = wmegc_huobl_all AND
     ls_lagp-anzle > 0.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-vlenr
        iv_lgpla      = ls_lagp-lgpla
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.

*   Enter handling unit
    MESSAGE i568 WITH lv_min_digits INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).
    ls_screlm_pbv-fld_prompt_4 = lv_msg.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'VLENR'
        iv_tablename = '/SCWM/ORDIM_O'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_4.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

  ENDIF.
  IF cs_ordim_confirm-vlenr IS NOT INITIAL.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-vlenr
        iv_lgpla      = ls_lagp-lgpla
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.

*   Search handling unit &1
*    MESSAGE i556 WITH cs_ordim_confirm-vlenr lv_min_digits cs_ordim_confirm-stack cs_ordim_confirm-lvl_v INTO lv_msg.
    MESSAGE i556 WITH cs_ordim_confirm-vlenr lv_min_digits INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).

    ls_screlm_pbv-fld_prompt_4 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'VLENR'
        iv_tablename = '/SCWM/ORDIM_O'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_4.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

  ENDIF.

* Into handling unit &1
  IF cs_ordim_confirm-pickhu IS NOT INITIAL.
    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_rsrc.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-pickhu
        iv_rsrc       = ls_rsrc-rsrc
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_5
        ev_min_digits = lv_min_digits.
  ENDIF.
  MESSAGE i555 WITH cs_ordim_confirm-pickhu lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_5' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_5 = lv_msg.

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'PICKHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_5.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_5' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_5' ).

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIHUTO_PBO
*&---------------------------------------------------------------------*
*&      Form  PBV_PIMTTO_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for source Product-WT screen
*         /SCWM/SAPLRF_PBV 0501
*----------------------------------------------------------------------*
*      <--IS_ORDIM_CONFIRM  WT application data
*----------------------------------------------------------------------*
FORM pbv_pimtto_pbo USING    is_ordim_o       TYPE /scwm/ordim_o
                    CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_msg        TYPE string,
        lv_data_entry TYPE /scwm/de_data_entry,
        lv_aisle_not  TYPE xfeld,
        lv_stack_not  TYPE xfeld,
        lv_lvl_v_not  TYPE xfeld,
        lv_min_digits TYPE i,
        lv_lgpla_prev TYPE /scwm/de_lgpla_prev,
        lv_lgtyp_prev TYPE /scwm/de_lgtyp_prev,
        lv_grammar    TYPE char256.
  DATA: ls_lagp       TYPE /scwm/lagp,
        ls_lagp_prev  TYPE /scwm/lagp,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_rsrc       TYPE /scwm/rsrc,
        ls_unit       TYPE t006a,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice fill aisle, stack and level
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = cs_ordim_confirm-vlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cs_ordim_confirm-aisle = ls_lagp-aisle.
  cs_ordim_confirm-stack = ls_lagp-stack.
* If level is not filled, we have to send user to complete bin
  IF ls_lagp-aisle IS INITIAL AND
     ls_lagp-lvl_v IS INITIAL.
    cs_ordim_confirm-lvl_v = ls_lagp-lgpla.
  ELSE.
    cs_ordim_confirm-lvl_v = ls_lagp-lvl_v.
  ENDIF.
  CONDENSE cs_ordim_confirm-aisle.
  CONDENSE cs_ordim_confirm-stack.
  CONDENSE cs_ordim_confirm-lvl_v.

  CALL FUNCTION '/SCWM/RF_PICK_GET_GLOBVAR'
    IMPORTING
      ev_lgpla_prev = lv_lgpla_prev
      ev_lgtyp_prev = lv_lgtyp_prev.

  IF lv_lgpla_prev IS NOT INITIAL.
    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_lgpla      = lv_lgpla_prev
      IMPORTING
        es_lagp       = ls_lagp_prev
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* Switch on input for all parts
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).

* Check the storage bin delta between prev. and actual storage bin
  IF ls_lagp_prev-lgtyp = ls_lagp-lgtyp.
    IF ls_lagp_prev-aisle = ls_lagp-aisle.
*     Switch off input for aisle
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
      lv_aisle_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
      lv_stack_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack AND
       ls_lagp_prev-lvl_v = ls_lagp-lvl_v AND
       ls_lagp_prev-lgpla = ls_lagp-lgpla.
*     Switch off input for level/bin
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).
      lv_lvl_v_not = abap_true.
    ENDIF.
  ENDIF.

* Fill prompt text, grammar (validation values) and help text for Pick by Voice
* Prompt text is the instruction for the worker what he has to do
* Grammar is the list of valid values
* Help text is given to the user on a false input

* Clear previously set screen prompt.
  CLEAR ls_screlm_pbv-scr_prompt.

  IF lv_aisle_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp.
*     Go to aisle &1 in storage type &4
      MESSAGE i559
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
             ls_lagp-lgtyp              " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to aisle &1
      MESSAGE i550
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_1 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_1' ).
    ls_screlm_pbv-grammar_1 = cs_ordim_confirm-aisle.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'AISLE'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_1.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_stack_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
       lv_aisle_not IS NOT INITIAL.
*     Go to stack &2 in storage type &4
      MESSAGE i571
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to stack &2
      MESSAGE i551
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_2 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'GRAMMAR_2' iv_pt = wmegc_pt_grammar ).

    IF ls_lagp-lvl_v IS INITIAL.  "Stack filled but no level
      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
      ENDIF.
    ELSE.
      ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
    ENDIF.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'STACK'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_2.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'HELP_2' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).


  ENDIF.

  IF lv_lvl_v_not IS INITIAL.
    IF ls_lagp-aisle IS INITIAL AND
       ls_lagp-lvl_v IS INITIAL.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to storage bin &1 in storage type &2
        MESSAGE i573 WITH cs_ordim_confirm-vlpla ls_lagp-lgtyp INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ELSE.
*       Go to storage bin &1
        MESSAGE i553 WITH cs_ordim_confirm-vlpla INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lgpla.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'VLPLA'
          iv_tablename = '/SCWM/ORDIM_O'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ELSE.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to level &3 in storage type &4
        MESSAGE i572
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ELSE.
*       Go to level &3
        MESSAGE i552
            WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
                 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lvl_v.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'LVL_V'
          iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ENDIF.
  ENDIF.

* Check if we have HU on the source bin
  IF ( cs_ordim_confirm-vlenr IS INITIAL OR
       is_ordim_o-vlenr IS INITIAL ) AND
     gv_huobl = wmegc_huobl_all AND
     ls_lagp-anzle > 0.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = is_ordim_o-vlenr
        iv_lgpla      = ls_lagp-lgpla
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.

*   Enter handling unit
    MESSAGE i568 WITH lv_min_digits INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).
    ls_screlm_pbv-fld_prompt_4 = lv_msg.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'VLENR'
        iv_tablename = '/SCWM/ORDIM_O'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_4.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

*   Filled Text
    MESSAGE i704 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_1' iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_1 = lv_msg.
  ENDIF.
  IF cs_ordim_confirm-vlenr IS NOT INITIAL.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-vlenr
        iv_lgpla      = ls_lagp-lgpla
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.

*   Search handling unit &1
*    MESSAGE i556 WITH cs_ordim_confirm-vlenr lv_min_digits cs_ordim_confirm-stack cs_ordim_confirm-lvl_v INTO lv_msg.
    MESSAGE i556 WITH cs_ordim_confirm-vlenr lv_min_digits INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).

    ls_screlm_pbv-fld_prompt_4 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'VLENR'
        iv_tablename = '/SCWM/ORDIM_O'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_4.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).


  ENDIF.

* Search batch &1
  MESSAGE i558 WITH cs_ordim_confirm-rfbatch INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_5' iv_use_symsg = 'X' ).

  ls_screlm_pbv-fld_prompt_5 = lv_msg.
  ls_screlm_pbv-grammar_5 = cs_ordim_confirm-rfbatch.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_5' ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'CHARG'
      iv_tablename = '/SCWM/ORDIM_O'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_5.

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_help iv_sf = 'HELP_5' iv_use_symsg = 'X' ).


* Take &1
  SELECT SINGLE * FROM t006a
                  INTO ls_unit
                  WHERE msehi = cs_ordim_confirm-altme
                    AND spras = sy-langu.

  IF cs_ordim_confirm-combqty > 0.
* for combine picking
    MESSAGE i598 WITH cs_ordim_confirm-combqty_chr ls_unit-msehl INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_sf = 'FLD_PROMPT_6' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).


    ls_screlm_pbv-fld_prompt_6 = lv_msg.
* Remove leading spaces and trailing zeroes
    WRITE cs_ordim_confirm-combqty_chr TO lv_grammar.
    ls_screlm_pbv-grammar_6 = lv_grammar.
    CLEAR ls_screlm_pbv-grammar_lnk_1.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_6' ).

* help text = prompt text
    MESSAGE i582 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_pt = wmegc_pt_help iv_sf = 'HELP_6' iv_use_symsg = 'X' ).
    ls_screlm_pbv-help_6 = lv_msg.

* Filled Text
    MESSAGE i703 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_2' iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_2 = lv_msg.
  ELSE.
    MESSAGE i554 WITH cs_ordim_confirm-vsola_chr ls_unit-msehl INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_6' iv_use_symsg = 'X' ).

    ls_screlm_pbv-fld_prompt_6 = lv_msg.
* Remove leading spaces and trailing zeroes
    WRITE cs_ordim_confirm-vsola_chr TO lv_grammar.
    ls_screlm_pbv-grammar_6 = lv_grammar.
    CLEAR ls_screlm_pbv-grammar_lnk_1.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_6' ).

* help text = prompt text
    MESSAGE i582 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_help iv_sf = 'HELP_6' iv_use_symsg = 'X' ).
    ls_screlm_pbv-help_6 = lv_msg.

* Filled Text
    MESSAGE i703 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_2' iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_2 = lv_msg.
  ENDIF.

* Enter remaining quantity
  MESSAGE i569 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_7' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_7 = lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_help iv_sf = 'HELP_7' iv_use_symsg = 'X' ).
  ls_screlm_pbv-help_7 = lv_msg.
* Filled Text
  MESSAGE i703 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_3' iv_use_symsg = 'X' ).
  ls_screlm_pbv-filled_3 = lv_msg.

  ls_screlm_pbv-grammar_lnk_2 = 'EWM_NUMERIC.JSGF'.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_grammarlnk iv_sf = 'GRAMMAR_LNK_2' ).

* Into handling unit &1
  IF cs_ordim_confirm-pickhu IS NOT INITIAL.
    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_rsrc.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-pickhu
        iv_rsrc       = ls_rsrc-rsrc
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_8
        ev_min_digits = lv_min_digits.
  ENDIF.
  MESSAGE i555 WITH cs_ordim_confirm-pickhu lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_8' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_8 = lv_msg.

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'PICKHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_8.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_8' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_8' ).

* Into position &1
  MESSAGE i570 WITH cs_ordim_confirm-hupos INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'LOGPOS' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_9' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_9 = lv_msg.

  ls_screlm_pbv-grammar_9 = cs_ordim_confirm-hupos.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'LOGPOS' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_9' ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'HUPOS'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_9.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'LOGPOS' iv_pt = wmegc_pt_help iv_sf = 'HELP_9' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIMTTO_PBO
*&---------------------------------------------------------------------*
*&      Form  PBV_PIPLHU_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for place HU-WT screen
*         /SCWM/SAPLRF_PBV 0301
*----------------------------------------------------------------------*
*      <--IS_ORDIM_CONFIRM  WT application data
*----------------------------------------------------------------------*
FORM pbv_piplhu_pbo  CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_data_entry TYPE /scwm/de_data_entry,
        lv_msg        TYPE string,
        lv_aisle_not  TYPE xfeld,
        lv_stack_not  TYPE xfeld,
        lv_lvl_v_not  TYPE xfeld,
        lv_min_digits TYPE i,
        lv_sumahu     TYPE i,
        lv_lgpla_prev TYPE /scwm/de_lgpla_prev,
        lv_lgtyp_prev TYPE /scwm/de_lgtyp_prev.
  DATA: ls_lagp       TYPE /scwm/lagp,
        ls_lagp_prev  TYPE /scwm/lagp,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice fill aisle, stack and level
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = cs_ordim_confirm-nlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cs_ordim_confirm-aisle = ls_lagp-aisle.
  cs_ordim_confirm-stack = ls_lagp-stack.
* If level is not filled, we have to send user to complete bin
  IF ls_lagp-aisle IS INITIAL AND
     ls_lagp-lvl_v IS INITIAL.
    cs_ordim_confirm-lvl_v = ls_lagp-lgpla.
  ELSE.
    cs_ordim_confirm-lvl_v = ls_lagp-lvl_v.
  ENDIF.

  CONDENSE cs_ordim_confirm-aisle.
  CONDENSE cs_ordim_confirm-stack.
  CONDENSE cs_ordim_confirm-lvl_v.

  DATA  lv_plpos  TYPE /scwm/lvs_plpos.
  DATA  lv_lgpla  TYPE /scwm/lgpla.

  CALL FUNCTION '/SCWM/RF_PICK_GET_GLOBVAR'
    IMPORTING
      ev_lgpla_prev = lv_lgpla_prev
      ev_lgtyp_prev = lv_lgtyp_prev.

  IF lv_lgpla_prev IS NOT INITIAL.
    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_lgpla      = lv_lgpla_prev
      IMPORTING
        es_lagp       = ls_lagp_prev
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      IF lv_lgpla_prev CA wmegc_subbin_separator.
*       The bin can be a deleted subbin
        CALL FUNCTION '/SCWM/L_PLATZ_POSITION_TRENNEN'
         EXPORTING
           LGNUM           = cs_ordim_confirm-lgnum
           LGTYP           = lv_lgtyp_prev
           LGPLA           = lv_lgpla_prev
         IMPORTING
           O_LGPLA         = lv_lgpla
           O_PLPOS         = lv_plpos
         EXCEPTIONS
           NOT_FOUND       = 1
           OVERFLOW        = 2
           OTHERS          = 3
        .
        IF SY-SUBRC <> 0.
*        Implement suitable error handling here
        ENDIF.
*       Read data from main bin, while the subbin was deleted
        CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
          EXPORTING
            iv_lgnum = cs_ordim_confirm-lgnum
            iv_lgpla = lv_lgpla
          IMPORTING
            es_lagp  = ls_lagp_prev
          EXCEPTIONS
           OTHERS   = 99.

        IF SY-SUBRC <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

* Switch on input for all parts
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).

* Check the storage bin delta between prev. and actual storage bin
  IF ls_lagp_prev-lgtyp = ls_lagp-lgtyp.
    IF ls_lagp_prev-aisle = ls_lagp-aisle.
*     Switch off input for aisle
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
      lv_aisle_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
      lv_stack_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack AND
       ls_lagp_prev-lvl_v = ls_lagp-lvl_v AND
       ls_lagp_prev-lgpla = ls_lagp-lgpla.
*     Switch off input for level/bin
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).
      lv_lvl_v_not = abap_true.
    ENDIF.
  ENDIF.

* Fill prompt text, grammar (validation values) and help text

* Clear previously set screen prompt.
  CLEAR ls_screlm_pbv-scr_prompt.

  IF lv_aisle_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp.
*     Go to aisle &1 in storage type &4
      MESSAGE i559
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
             ls_lagp-lgtyp              " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to aisle &1
      MESSAGE i550
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_1 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_1' ).

    ls_screlm_pbv-grammar_1 = cs_ordim_confirm-aisle.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'AISLE'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_1.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_stack_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
       lv_aisle_not IS NOT INITIAL.
*     Go to stack &2 in storage type &4
      MESSAGE i571
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to stack &2
      MESSAGE i551
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_2 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'GRAMMAR_2' iv_pt = wmegc_pt_grammar ).

    IF ls_lagp-lvl_v IS INITIAL.  "Stack filled but no level

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
      ENDIF.

    ELSE.
      ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
    ENDIF.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'STACK'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_2.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'HELP_2' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_lvl_v_not IS INITIAL.
    IF ls_lagp-aisle IS INITIAL AND
       ls_lagp-lvl_v IS INITIAL.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to storage bin &1 in storage type &2
        MESSAGE i573 WITH cs_ordim_confirm-nlpla ls_lagp-lgtyp INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ELSE.
*       Go to storage bin &1
        MESSAGE i553 WITH cs_ordim_confirm-nlpla INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lgpla.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'NLPLA'
          iv_tablename = '/SCWM/ORDIM_O'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ELSE.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to level &3 in storage type &4
        MESSAGE i572
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ELSE.
*       Go to level &3
        MESSAGE i552
            WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
                 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lvl_v.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'LVL_V'
          iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ENDIF.
  ENDIF.

* Search handling unit &1
  IF cs_ordim_confirm-pickhu IS NOT INITIAL.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-pickhu
        iv_rsrc       = cs_ordim_confirm-srsrc
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.
  ENDIF.
*  MESSAGE i556 WITH cs_ordim_confirm-pickhu lv_min_digits cs_ordim_confirm-stack cs_ordim_confirm-lvl_v INTO lv_msg.
  MESSAGE i556 WITH cs_ordim_confirm-pickhu lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).

  ls_screlm_pbv-fld_prompt_4 = lv_msg.

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'PICKHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_4.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

* Place x Handling Units
  lv_sumahu = cs_ordim_confirm-sumahu.
  MESSAGE i581 WITH lv_sumahu INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'SUMAHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_5' ).
  ls_screlm_pbv-fld_prompt_5 = lv_msg.

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'SUMAHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_5' ).
  ls_screlm_pbv-grammar_5 = lv_sumahu.

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'SUMAHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_5.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'SUMAHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_5' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIPLHU_PBO
*&---------------------------------------------------------------------*
*&      Form  PBV_PIPLMT_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for place Product-WT screen
*         /SCWM/SAPLRF_PBV 0302
*----------------------------------------------------------------------*
*      <--IS_ORDIM_CONFIRM  WT application data
*----------------------------------------------------------------------*
FORM pbv_piplmt_pbo  CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_data_entry TYPE /scwm/de_data_entry,
        lv_msg        TYPE string,
        lv_aisle_not  TYPE xfeld,
        lv_stack_not  TYPE xfeld,
        lv_lvl_v_not  TYPE xfeld,
        lv_min_digits TYPE i,
        lv_lgpla_prev TYPE /scwm/de_lgpla_prev,
        lv_lgtyp_prev TYPE /scwm/de_lgtyp_prev,
        lv_grammar    TYPE char256.
  DATA: ls_lagp       TYPE /scwm/lagp,
        ls_lagp_prev  TYPE /scwm/lagp,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_unit       TYPE t006a,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice fill aisle, stack and level
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = cs_ordim_confirm-nlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cs_ordim_confirm-aisle = ls_lagp-aisle.
  cs_ordim_confirm-stack = ls_lagp-stack.
* If level is not filled, we have to send user to complete bin
  IF ls_lagp-aisle IS INITIAL AND
     ls_lagp-lvl_v IS INITIAL.
    cs_ordim_confirm-lvl_v = ls_lagp-lgpla.
  ELSE.
    cs_ordim_confirm-lvl_v = ls_lagp-lvl_v.
  ENDIF.
  CONDENSE cs_ordim_confirm-aisle.
  CONDENSE cs_ordim_confirm-stack.
  CONDENSE cs_ordim_confirm-lvl_v.

  CALL FUNCTION '/SCWM/RF_PICK_GET_GLOBVAR'
    IMPORTING
      ev_lgpla_prev = lv_lgpla_prev
      ev_lgtyp_prev = lv_lgtyp_prev.

  IF lv_lgpla_prev IS NOT INITIAL.
    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_lgpla      = lv_lgpla_prev
      IMPORTING
        es_lagp       = ls_lagp_prev
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* Switch on input for all parts
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).

* Check the storage bin delta between prev. and actual storage bin
  IF ls_lagp_prev-lgtyp = ls_lagp-lgtyp.
    IF ls_lagp_prev-aisle = ls_lagp-aisle.
*     Switch off input for aisle
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
      lv_aisle_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
      lv_stack_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack AND
       ls_lagp_prev-lvl_v = ls_lagp-lvl_v AND
       ls_lagp_prev-lgpla = ls_lagp-lgpla.
*     Switch off input for level/bin
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).
      lv_lvl_v_not = abap_true.
    ENDIF.
  ENDIF.

* Fill prompt text, grammar (validation values) and help text

* Clear previously set screen prompt.
  CLEAR ls_screlm_pbv-scr_prompt.

  IF lv_aisle_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp.
*     Go to aisle &1 in storage type &4
      MESSAGE i559
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
             ls_lagp-lgtyp              " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to aisle &1
      MESSAGE i550
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_1 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_1' ).

    ls_screlm_pbv-grammar_1 = cs_ordim_confirm-aisle.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'AISLE'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_1.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_stack_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
       lv_aisle_not IS NOT INITIAL.
*     Go to stack &2 in storage type &4
      MESSAGE i571
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to stack &2
      MESSAGE i551
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_2 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'GRAMMAR_2' iv_pt = wmegc_pt_grammar ).

    IF ls_lagp-lvl_v IS INITIAL.  "Stack filled but no level

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
      ENDIF.

    ELSE.
      ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
    ENDIF.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'STACK'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_2.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'HELP_2' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_lvl_v_not IS INITIAL.
    IF ls_lagp-aisle IS INITIAL AND
       ls_lagp-lvl_v IS INITIAL.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to storage bin &1 in storage type &2
        MESSAGE i573 WITH cs_ordim_confirm-nlpla ls_lagp-lgtyp INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ELSE.
*       Go to storage bin &1
        MESSAGE i553 WITH cs_ordim_confirm-nlpla INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lgpla.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'NLPLA'
          iv_tablename = '/SCWM/ORDIM_O'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ELSE.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to level &3 in storage type &4
        MESSAGE i572
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ELSE.
*       Go to level &3
        MESSAGE i552
            WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
                 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lvl_v.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'LVL_V'
          iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ENDIF.
  ENDIF.

  IF cs_ordim_confirm-pickhu IS NOT INITIAL.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-pickhu
        iv_rsrc       = cs_ordim_confirm-srsrc
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.
  ENDIF.
*  MESSAGE i556 WITH cs_ordim_confirm-pickhu lv_min_digits cs_ordim_confirm-stack cs_ordim_confirm-lvl_v INTO lv_msg.
  MESSAGE i556 WITH cs_ordim_confirm-pickhu lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_4 = lv_msg.

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'PICKHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_4.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

* Search batch &1
  MESSAGE i558 WITH cs_ordim_confirm-rfbatch INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_5' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_5 = lv_msg.

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_5' ).
  ls_screlm_pbv-grammar_5 = cs_ordim_confirm-rfbatch.
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'CHARG'
      iv_tablename = '/SCWM/ORDIM_O'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_5.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_help iv_sf = 'HELP_5' iv_use_symsg = 'X' ).

* Take &1
  SELECT SINGLE * FROM t006a
                  INTO ls_unit
                  WHERE msehi = cs_ordim_confirm-altme
                    AND spras = sy-langu.

  MESSAGE i554 WITH cs_ordim_confirm-nista_chr ls_unit-msehl INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_6' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_6 = lv_msg.
* Remove leading spaces and trailing zeroes
  WRITE cs_ordim_confirm-nista_chr TO lv_grammar.
  ls_screlm_pbv-grammar_6 = lv_grammar.
  CLEAR ls_screlm_pbv-grammar_lnk_1.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_6' ).

* help text = prompt text
  MESSAGE i582 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_help iv_sf = 'HELP_6' iv_use_symsg = 'X' ).
  ls_screlm_pbv-help_6 = lv_msg.

* Filled Text
  MESSAGE i703 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_1' iv_use_symsg = 'X' ).
  ls_screlm_pbv-filled_1 = lv_msg.

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIPLMT_PBO
*&---------------------------------------------------------------------*
*&      Form  PBV_PIBLMT_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for source bulk Product-WT screen
*         /SCWM/SAPLRF_PBV 0503
*----------------------------------------------------------------------*
*      <--IS_ORDIM_CONFIRM  WT application data
*----------------------------------------------------------------------*
FORM pbv_piblmt_pbo  CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_msg        TYPE string,
        lv_data_entry TYPE /scwm/de_data_entry,
        lv_aisle_not  TYPE xfeld,
        lv_stack_not  TYPE xfeld,
        lv_lvl_v_not  TYPE xfeld,
        lv_min_digits TYPE i,
        lv_lgpla_prev TYPE /scwm/de_lgpla_prev,
        lv_lgtyp_prev TYPE /scwm/de_lgtyp_prev,
        lv_grammar    TYPE char256.
  DATA: ls_lagp       TYPE /scwm/lagp,
        ls_lagp_prev  TYPE /scwm/lagp,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_rsrc       TYPE /scwm/rsrc,
        ls_unit       TYPE t006a,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice fill aisle, stack and level
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = cs_ordim_confirm-vlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cs_ordim_confirm-aisle = ls_lagp-aisle.
  cs_ordim_confirm-stack = ls_lagp-stack.
* If level is not filled, we have to send user to complete bin
  IF ls_lagp-aisle IS INITIAL AND
     ls_lagp-lvl_v IS INITIAL.
    cs_ordim_confirm-lvl_v = ls_lagp-lgpla.
  ELSE.
    cs_ordim_confirm-lvl_v = ls_lagp-lvl_v.
  ENDIF.
  CONDENSE cs_ordim_confirm-aisle.
  CONDENSE cs_ordim_confirm-stack.
  CONDENSE cs_ordim_confirm-lvl_v.

  CALL FUNCTION '/SCWM/RF_PICK_GET_GLOBVAR'
    IMPORTING
      ev_lgpla_prev = lv_lgpla_prev
      ev_lgtyp_prev = lv_lgtyp_prev.

  IF lv_lgpla_prev IS NOT INITIAL.
    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_lgpla      = lv_lgpla_prev
      IMPORTING
        es_lagp       = ls_lagp_prev
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* Switch on input for all parts
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).

* Check the storage bin delta between prev. and actual storage bin
  IF ls_lagp_prev-lgtyp = ls_lagp-lgtyp.
    IF ls_lagp_prev-aisle = ls_lagp-aisle.
*     Switch off input for aisle
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
      lv_aisle_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
      lv_stack_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack AND
       ls_lagp_prev-lvl_v = ls_lagp-lvl_v AND
       ls_lagp_prev-lgpla = ls_lagp-lgpla.
*     Switch off input for level/bin
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).
      lv_lvl_v_not = abap_true.
    ENDIF.
  ENDIF.

* Fill prompt text, grammar (validation values) and help text for Pick by Voice
* Prompt text is the instruction for the worker what he has to do
* Grammar is the list of valid values
* Help text is given to the user on a false input

* Clear previously set screen prompt.
  CLEAR ls_screlm_pbv-scr_prompt.

  IF lv_aisle_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp.
*     Go to aisle &1 in storage type &4
      MESSAGE i559
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
             ls_lagp-lgtyp              " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to aisle &1
      MESSAGE i550
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_1 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_1' ).

    ls_screlm_pbv-grammar_1 = cs_ordim_confirm-aisle.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'AISLE'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_1.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_stack_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
       lv_aisle_not IS NOT INITIAL.
*     Go to stack &2 in storage type &4
      MESSAGE i571
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to stack &2
      MESSAGE i551
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_2 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'GRAMMAR_2' iv_pt = wmegc_pt_grammar ).

    IF ls_lagp-lvl_v IS INITIAL.  "Stack filled but no level

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
      ENDIF.

    ELSE.
      ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
    ENDIF.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'STACK'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_2.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'HELP_2' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_lvl_v_not IS INITIAL.
    IF ls_lagp-aisle IS INITIAL AND
       ls_lagp-lvl_v IS INITIAL.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to storage bin &1 in storage type &2
        MESSAGE i573 WITH cs_ordim_confirm-vlpla ls_lagp-lgtyp INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ELSE.
*       Go to storage bin &1
        MESSAGE i553 WITH cs_ordim_confirm-vlpla INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.
      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lgpla.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'VLPLA'
          iv_tablename = '/SCWM/ORDIM_O'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ELSE.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to level &3 in storage type &4
        MESSAGE i572
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ELSE.
*       Go to level &3
        MESSAGE i552
            WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
                 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lvl_v.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'LVL_V'
          iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ENDIF.
  ENDIF.

* Search HU
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = ls_lagp-lgpla
      iv_skip_empty = abap_true
    IMPORTING
      ev_msg        = ls_screlm_pbv-grammar_4
      ev_min_digits = lv_min_digits.

* Enter handling unit
  MESSAGE i568 WITH lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_4 = lv_msg.

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

* Filled Text
  MESSAGE i704 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_1' iv_use_symsg = 'X' ).
  ls_screlm_pbv-filled_1 = lv_msg.

* Search batch &1
  MESSAGE i558 WITH cs_ordim_confirm-rfbatch INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_5' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_5 = lv_msg.
  ls_screlm_pbv-grammar_5 = cs_ordim_confirm-rfbatch.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_5' ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'CHARG'
      iv_tablename = '/SCWM/ORDIM_O'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_5.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'CHARG' iv_pt = wmegc_pt_help iv_sf = 'HELP_5' iv_use_symsg = 'X' ).

* Take &1
  SELECT SINGLE * FROM t006a
                  INTO ls_unit
                  WHERE msehi = cs_ordim_confirm-altme
                    AND spras = sy-langu.

  IF cs_ordim_confirm-combqty > 0.
* for combine picking
    MESSAGE i598 WITH cs_ordim_confirm-combqty_chr ls_unit-msehl INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_sf = 'FLD_PROMPT_6' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).


    ls_screlm_pbv-fld_prompt_6 = lv_msg.
* Remove leading spaces and trailing zeroes
    WRITE cs_ordim_confirm-combqty_chr TO lv_grammar.
    ls_screlm_pbv-grammar_6 = lv_grammar.
    CLEAR ls_screlm_pbv-grammar_lnk_1.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_6' ).

* help text = prompt text
    MESSAGE i582 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_pt = wmegc_pt_help iv_sf = 'HELP_6' iv_use_symsg = 'X' ).
    ls_screlm_pbv-help_6 = lv_msg.

* Filled Text
    MESSAGE i703 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'COMBQTY' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_2' iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_2 = lv_msg.
  ELSE.
    MESSAGE i554 WITH cs_ordim_confirm-vsola_chr ls_unit-msehl INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_6' iv_use_symsg = 'X' ).
    ls_screlm_pbv-fld_prompt_6 = lv_msg.
* Remove leading spaces and trailing zeroes
    WRITE cs_ordim_confirm-vsola_chr TO lv_grammar.
    ls_screlm_pbv-grammar_6 = lv_grammar.
    CLEAR ls_screlm_pbv-grammar_lnk_1.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_6' ).

* help text = prompt text
    MESSAGE i582 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_help iv_sf = 'HELP_6' iv_use_symsg = 'X' ).
    ls_screlm_pbv-help_6 = lv_msg.

* Filled Text
    MESSAGE i703 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_1' iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_1 = lv_msg.
  ENDIF.
* Enter remaining quantity
  MESSAGE i569 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_7' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_7 = lv_msg.

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_help iv_sf = 'HELP_7' iv_use_symsg = 'X' ).
  ls_screlm_pbv-help_7 = lv_msg.
* Filled Text
  MESSAGE i703 INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_3' iv_use_symsg = 'X' ).
  ls_screlm_pbv-filled_3 = lv_msg.

  ls_screlm_pbv-grammar_lnk_2 = 'EWM_NUMERIC.JSGF'.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'KQUAN' iv_pt = wmegc_pt_grammarlnk iv_sf = 'GRAMMAR_LNK_2' ).

* Into handling unit &1
  IF cs_ordim_confirm-pickhu IS NOT INITIAL.
    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_rsrc.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-pickhu
        iv_rsrc       = ls_rsrc-rsrc
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_8
        ev_min_digits = lv_min_digits.
  ENDIF.
  MESSAGE i555 WITH cs_ordim_confirm-pickhu lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_8' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_8 = lv_msg.

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'PICKHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_8.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_8' iv_use_symsg = 'X' ).


  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_8' ).

* Into position &1
  MESSAGE i570 WITH cs_ordim_confirm-hupos INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'LOGPOS' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_9' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_9 = lv_msg.
  ls_screlm_pbv-grammar_9 = cs_ordim_confirm-hupos.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'LOGPOS' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_9' ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'HUPOS'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_9.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'LOGPOS' iv_pt = wmegc_pt_help iv_sf = 'HELP_9' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIBLMT_PBO
*&---------------------------------------------------------------------*
*&      Form  PBV_PIBLHU_PBO
*&---------------------------------------------------------------------*
*       Fill voice properties for source bulk HU-WT screen
*         /SCWM/SAPLRF_PBV 0504
*----------------------------------------------------------------------*
*      <--IS_ORDIM_CONFIRM  WT application data
*----------------------------------------------------------------------*
FORM pbv_piblhu_pbo  CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm.

  DATA: lv_msg        TYPE string,
        lv_data_entry TYPE /scwm/de_data_entry,
        lv_aisle_not  TYPE xfeld,
        lv_stack_not  TYPE xfeld,
        lv_lvl_v_not  TYPE xfeld,
        lv_min_digits TYPE i,
        lv_lgpla_prev TYPE /scwm/de_lgpla_prev,
        lv_lgtyp_prev TYPE /scwm/de_lgtyp_prev.
  DATA: ls_lagp       TYPE /scwm/lagp,
        ls_lagp_prev  TYPE /scwm/lagp,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        ls_rsrc       TYPE /scwm/rsrc,

        lt_index_pbv  TYPE /scwm/tt_rf_index_pbv.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

  CLEAR lt_index_pbv.
  /scwm/cl_rf_bll_srvc=>set_index_pbv( lt_index_pbv ).

* On Pick by Voice fill aisle, stack and level
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = cs_ordim_confirm-lgnum
      iv_lgpla      = cs_ordim_confirm-vlpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cs_ordim_confirm-aisle = ls_lagp-aisle.
  cs_ordim_confirm-stack = ls_lagp-stack.
* If level is not filled, we have to send user to complete bin
  IF ls_lagp-aisle IS INITIAL AND
     ls_lagp-lvl_v IS INITIAL.
    cs_ordim_confirm-lvl_v = ls_lagp-lgpla.
  ELSE.
    cs_ordim_confirm-lvl_v = ls_lagp-lvl_v.
  ENDIF.
  CONDENSE cs_ordim_confirm-aisle.
  CONDENSE cs_ordim_confirm-stack.
  CONDENSE cs_ordim_confirm-lvl_v.

  CALL FUNCTION '/SCWM/RF_PICK_GET_GLOBVAR'
    IMPORTING
      ev_lgpla_prev = lv_lgpla_prev
      ev_lgtyp_prev = lv_lgtyp_prev.

  IF lv_lgpla_prev IS NOT INITIAL.
    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_lgpla      = lv_lgpla_prev
      IMPORTING
        es_lagp       = ls_lagp_prev
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* Switch on input for all parts
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).

* Check the storage bin delta between prev. and actual storage bin
  IF ls_lagp_prev-lgtyp = ls_lagp-lgtyp.
    IF ls_lagp_prev-aisle = ls_lagp-aisle.
*     Switch off input for aisle
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-AISLE_VERIF' ).
      lv_aisle_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-STACK_VERIF' ).
      lv_stack_not = abap_true.
    ENDIF.
    IF ls_lagp_prev-aisle = ls_lagp-aisle AND
       ls_lagp_prev-stack = ls_lagp-stack AND
       ls_lagp_prev-lvl_v = ls_lagp-lvl_v AND
       ls_lagp_prev-lvl_v IS NOT INITIAL.
*     Switch off input for stack
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-LVL_V_VERIF' ).
      lv_lvl_v_not = abap_true.
    ENDIF.
  ENDIF.

* Fill prompt text, grammar (validation values) and help text
* Clear previously set screen prompt.
  CLEAR ls_screlm_pbv-scr_prompt.

  IF lv_aisle_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp.
*     Go to aisle &1 in storage type &4
      MESSAGE i559
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
             ls_lagp-lgtyp              " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to aisle &1
      MESSAGE i550
        WITH cs_ordim_confirm-aisle     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-stack     " EWM 9.40 - Change to provide all bin parameters
             cs_ordim_confirm-lvl_v     " EWM 9.40 - Change to provide all bin parameters
        INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'AISLE' iv_sf = 'FLD_PROMPT_1' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_1 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_1' ).

    ls_screlm_pbv-grammar_1 = cs_ordim_confirm-aisle.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'AISLE'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_1.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'AISLE' iv_sf = 'HELP_1' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_stack_not IS INITIAL.
    IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
       lv_aisle_not IS NOT INITIAL.
*     Go to stack &2 in storage type &4
      MESSAGE i571
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ELSE.
*     Go to stack &2
      MESSAGE i551
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters (now 2nd parameter)
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters
          INTO lv_msg.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'STACK' iv_sf = 'FLD_PROMPT_2' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
    ENDIF.
    ls_screlm_pbv-fld_prompt_2 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'GRAMMAR_2' iv_pt = wmegc_pt_grammar ).

    IF ls_lagp-lvl_v IS INITIAL.  "Stack filled but no level

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_2 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
      ENDIF.

    ELSE.
      ls_screlm_pbv-grammar_2 = cs_ordim_confirm-stack.
    ENDIF.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'STACK'
        iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_2.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'STACK' iv_sf = 'HELP_2' iv_pt = wmegc_pt_help iv_use_symsg = 'X' ).

  ENDIF.

  IF lv_lvl_v_not IS INITIAL.
    IF ls_lagp-aisle IS INITIAL AND
       ls_lagp-lvl_v IS INITIAL.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to storage bin &1 in storage type &2
        MESSAGE i573 WITH cs_ordim_confirm-vlpla ls_lagp-lgtyp INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ELSE.
*       Go to storage bin &1
        MESSAGE i553 WITH cs_ordim_confirm-vlpla INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_sf = 'FLD_PROMPT_3' iv_pt = wmegc_pt_prompt iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lgpla.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'VLPLA'
          iv_tablename = '/SCWM/ORDIM_O'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).


    ELSE.
      IF ls_lagp_prev-lgtyp <> ls_lagp-lgtyp AND
         lv_aisle_not IS NOT INITIAL AND
         lv_stack_not IS NOT INITIAL.
*       Go to level &3 in storage type &4
        MESSAGE i572
          WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
               cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
               ls_lagp-lgtyp            " EWM 9.40 - Change to provide all bin parameters (now 4th parameter)
               INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ELSE.
*       Go to level &3
        MESSAGE i552
            WITH cs_ordim_confirm-aisle   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-stack   " EWM 9.40 - Change to provide all bin parameters
                 cs_ordim_confirm-lvl_v   " EWM 9.40 - Change to provide all bin parameters (now 3rd parameter)
                 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
            EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_3' iv_use_symsg = 'X' ).
      ENDIF.
      ls_screlm_pbv-fld_prompt_3 = lv_msg.

      IF ls_lagp-pbv_verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-pbv_verif.  "Take PbV valid. value of bin
      ELSEIF ls_lagp-verif IS NOT INITIAL.
        ls_screlm_pbv-grammar_3 = ls_lagp-verif.  "Take RF valid. value of bin
      ELSE.
        ls_screlm_pbv-grammar_3 = ls_lagp-lvl_v.
      ENDIF.

      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_3' ).

      CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
        EXPORTING
          iv_fieldname = 'LVL_V'
          iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
        IMPORTING
          ev_msg       = ls_screlm_pbv-help_3.
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'LVL_V' iv_pt = wmegc_pt_help iv_sf = 'HELP_3' iv_use_symsg = 'X' ).

    ENDIF.
  ENDIF.

* Check if we have HU on the source bin
  IF cs_ordim_confirm-vlenr IS INITIAL AND
     gv_huobl = wmegc_huobl_all AND
     ls_lagp-anzle > 0.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-vlenr
        iv_lgpla      = ls_lagp-lgpla
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.

*   Enter handling unit
    MESSAGE i568 WITH lv_min_digits INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).
    ls_screlm_pbv-fld_prompt_4 = lv_msg.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'VLENR'
        iv_tablename = '/SCWM/ORDIM_O'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_4.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).


    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

*   Filled Text
    MESSAGE i704 INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_1' iv_use_symsg = 'X' ).
    ls_screlm_pbv-filled_1 = lv_msg.
  ENDIF.
  IF cs_ordim_confirm-vlenr IS NOT INITIAL.
    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-vlenr
        iv_lgpla      = ls_lagp-lgpla
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_4
        ev_min_digits = lv_min_digits.

*   Search handling unit &1
*    MESSAGE i556 WITH cs_ordim_confirm-vlenr lv_min_digits cs_ordim_confirm-stack cs_ordim_confirm-lvl_v INTO lv_msg.
    MESSAGE i556 WITH cs_ordim_confirm-vlenr lv_min_digits INTO lv_msg.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_4' iv_use_symsg = 'X' ).
    ls_screlm_pbv-fld_prompt_4 = lv_msg.

    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_4' ).

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
      EXPORTING
        iv_fieldname = 'VLENR'
        iv_tablename = '/SCWM/ORDIM_O'
      IMPORTING
        ev_msg       = ls_screlm_pbv-help_4.
    /scwm/cl_rf_bll_srvc=>build_index_pbv(
        EXPORTING iv_if = 'VLENR' iv_pt = wmegc_pt_help iv_sf = 'HELP_4' iv_use_symsg = 'X' ).

  ENDIF.

* Into handling unit &1
  IF cs_ordim_confirm-pickhu IS NOT INITIAL.
    CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        cs_rsrc  = ls_rsrc.

    CALL METHOD /scwm/cl_rf_bll_srvc=>build_grammar_for_hu
      EXPORTING
        iv_lgnum      = cs_ordim_confirm-lgnum
        iv_src_hu     = cs_ordim_confirm-pickhu
        iv_rsrc       = ls_rsrc-rsrc
      IMPORTING
        ev_msg        = ls_screlm_pbv-grammar_5
        ev_min_digits = lv_min_digits.
  ENDIF.
  MESSAGE i555 WITH cs_ordim_confirm-pickhu lv_min_digits INTO lv_msg.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_5' iv_use_symsg = 'X' ).
  ls_screlm_pbv-fld_prompt_5 = lv_msg.
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_help_text
    EXPORTING
      iv_fieldname = 'PICKHU'
      iv_tablename = '/SCWM/S_RF_ORDIM_CONFIRM'
    IMPORTING
      ev_msg       = ls_screlm_pbv-help_5.
  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_help iv_sf = 'HELP_5' iv_use_symsg = 'X' ).

  /scwm/cl_rf_bll_srvc=>build_index_pbv(
      EXPORTING iv_if = 'PICKHU' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_5' ).

  /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

  " EWM 9.40 Enhancements
  "---------------------------------------------------------------
  CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
  "---------------------------------------------------------------

ENDFORM.                    " PBV_PIBLHU_PBO
*&---------------------------------------------------------------------*
*&      Form  PIMTTO_CLOSE_FIELDS
*&---------------------------------------------------------------------*
*       Close input fields after exception Bin Denial Full
*----------------------------------------------------------------------*
FORM pimtto_close_fields .

  DATA lt_valid_prf TYPE /scwm/tt_valid_prf_ext.

  FIELD-SYMBOLS <valid_prf> TYPE /scwm/s_valid_prf_ext.

* Close Qty verification input field
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_nista_vrf ).

* Turn off MATID verification
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    gc_scr_elmnt_matid_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_matid_vrf ).

* Turn off Batch verification if needed
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                   gc_scr_elmnt_batch_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_batch_vrf ).

* Turn off RFBatch verification if needed
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                   gc_scr_elmnt_rfbatch ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_rfbatch ).

* Turn off VLENR verification
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    gc_scr_elmnt_vlenr_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_vlenr_vrf ).

* Turn off Pick-HU verification
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                   gc_scr_elmnt_pickhu_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_pickhu_vrf ).

* Turn off HUPOS verification
  /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                    gc_scr_elmnt_hupos_vrf ).
  /scwm/cl_rf_bll_srvc=>set_screlm_input_off(
                                    gc_scr_elmnt_hupos_vrf ).

* New logic because of the quick enter and CW. The verif fields should be
* all validated for the ENTER function key pass. Without the valid_prf update
* an extra ENTER key is requested before the confirmation call.
  lt_valid_prf = /scwm/cl_rf_bll_srvc=>get_valid_prf( ).

  LOOP AT lt_valid_prf ASSIGNING <valid_prf>
    WHERE flg_disable IS INITIAL.
    IF <valid_prf>-valinp_fldname NE 'VLPLA_VERIF' AND
       <valid_prf>-valinp_fldname NE 'AISLE_VERIF' AND
       <valid_prf>-valinp_fldname NE 'STACK_VERIF' AND
       <valid_prf>-valinp_fldname NE 'LVL_V_VERIF'.
      <valid_prf>-flg_disable = 'C'.
    ENDIF.
  ENDLOOP.

  /scwm/cl_rf_bll_srvc=>set_valid_prf( lt_valid_prf ).

*   Set Fcode to ENTER, continue with TO confirmation.
  /scwm/cl_rf_bll_srvc=>set_fcode(
                  /scwm/cl_rf_bll_srvc=>c_fcode_enter ).

ENDFORM.                    " PIMTTO_CLOSE_FIELDS
*&---------------------------------------------------------------------*
*&      Form  PBV_CHECK_FOR_COMMANDS_PRD
*&---------------------------------------------------------------------*
*    Check if the function code can be disabled for produt WTs
*----------------------------------------------------------------------*
FORM pbv_check_for_commands_prd
  USING is_ordim_confirm      TYPE /scwm/s_rf_ordim_confirm
        it_ordim_confirm      TYPE /scwm/tt_rf_ordim_confirm
        it_mat_uom            TYPE /scwm/tt_material_uom
        iv_buom               TYPE meins.

  DATA: lv_flg_suom           TYPE xfeld,
        lv_data_entry         TYPE /scwm/de_data_entry,

        ls_mat_uom            TYPE /scwm/s_material_uom.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

* SUoM check
  CLEAR lv_flg_suom.
  LOOP AT it_mat_uom INTO ls_mat_uom.
    IF ls_mat_uom-meinh <> iv_buom.

      CALL FUNCTION '/SCWM/SUOM_CHECK'
        EXPORTING
          iv_lgnum    = is_ordim_confirm-lgnum
          iv_matid    = is_ordim_confirm-matid
          iv_uom      = ls_mat_uom-meinh
        IMPORTING
          ev_flg_suom = lv_flg_suom.

      IF lv_flg_suom IS NOT INITIAL. " UoM is SUoM relevant
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lv_flg_suom IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_fcode_off( 'CHGUOM' ).
  ENDIF.

  PERFORM pbv_check_for_commands USING it_ordim_confirm.

ENDFORM.                    "PBV_CHECK_FOR_COMMANDS_PRD
*&---------------------------------------------------------------------*
*&      Form  PBV_CHECK_FOR_COMMANDS
*&---------------------------------------------------------------------*
*       Check if the function code can be disabled
*----------------------------------------------------------------------*
FORM pbv_check_for_commands
  USING it_ordim_confirm      TYPE /scwm/tt_rf_ordim_confirm.

  DATA: lv_lines_total        TYPE numc4,
        lv_data_entry         TYPE /scwm/de_data_entry.

* Check if we work with a PbV device
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry <> wmegc_data_entry_voice.
    RETURN.
  ENDIF.

* SkipWT check
  DESCRIBE TABLE it_ordim_confirm LINES lv_lines_total.
  IF lv_lines_total EQ 1.
    /scwm/cl_rf_bll_srvc=>set_fcode_off( 'SKIP' ).
  ENDIF.
ENDFORM.                    "PBV_CHECK_FOR_COMMANDS
