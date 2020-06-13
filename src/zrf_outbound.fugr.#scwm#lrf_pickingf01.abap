*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PICKINGF01 .
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  more_ind_read
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->IV_MORE     text
*      -->IV_MORE_IND text
*---------------------------------------------------------------------*
FORM more_ind_read
     USING    iv_more     TYPE xfeld
     CHANGING cv_more_ind TYPE /scwm/de_rf_more.

  DATA: ls_dfies TYPE dfies.

  CLEAR: cv_more_ind.

* hier stimmt irgendwas nocht nicht, da der Text nicht zurückgegeben
* wird

  IF iv_more IS NOT INITIAL.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = gc_fn_more
        langu          = sy-langu
        all_types      = 'X'
      IMPORTING
        dfies_wa       = ls_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

*   Fill in the Place Inventory indicator text
    IF sy-subrc = 0.
*     Move short text to Place Inventory indicator
      MOVE ls_dfies-scrtext_s TO cv_more_ind.
    ENDIF.

  ENDIF.

ENDFORM.                    "more_ind_read

*&---------------------------------------------------------------------*
*&      Form  check_hu_req_on_destination
*&---------------------------------------------------------------------*
*       Checks if Pick HU is necessary or not
*----------------------------------------------------------------------*
*      -->LGNUM             warehouse number
*      -->TT_ORDIM_CONFIRM  actual Wts
*      -->T_RF_PICK_HUS     pick HUs
*----------------------------------------------------------------------*
FORM check_hu_req_on_destination
      USING    iv_lgnum          TYPE /scwm/lgnum
               iv_resource       TYPE /scwm/de_rsrc
               ordim_confirm     TYPE /scwm/s_rf_ordim_confirm
               lt_rf_pick_hus    TYPE /scwm/tt_rf_pick_hus
               tt_nested_hu      TYPE /scwm/tt_rf_nested_hu.

  DATA ls_lgtyp   TYPE rsdsselopt.
  DATA ls_t331    TYPE /scwm/t331.
  DATA ls_ordim_o TYPE /scwm/ordim_o.
  DATA lt_pickhus TYPE /scwm/tt_huident.
  DATA lv_huent   TYPE /scwm/ltap_hu_huent.
  DATA lt_valid_prf TYPE /scwm/tt_valid_prf_ext.
  DATA ls_valid_prf TYPE /scwm/s_valid_prf_ext.
  DATA ls_exc_tab   TYPE /scwm/s_rf_exc.
  DATA lv_data_entry  TYPE /scwm/de_data_entry.

  CALL FUNCTION '/SCWM/T331_READ_SINGLE'
    EXPORTING
      iv_lgnum  = iv_lgnum
      iv_lgtyp  = ordim_confirm-nltyp
    IMPORTING
      es_t331   = ls_t331
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* check for HU necessarity
  IF ls_t331-huobl = 'X'.
* one of the following three conditions has to occur:
* 1. a destination handling unit is set
   IF  NOT ( ordim_confirm-nlenr  IS NOT INITIAL OR
* 2. a pick HU is used
             ordim_confirm-pickhu IS NOT INITIAL OR
* 3. a source handling unit is set and HU withrawal is used
             ordim_confirm-vlenr  IS NOT INITIAL AND
             ordim_confirm-huent  =  'X' ).

*     Check if we have nested HU(s) where HUwd (HUENT)flag is not set
      IF tt_nested_hu IS NOT INITIAL.
        READ TABLE tt_nested_hu
             WITH KEY huent = abap_false TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDIF.

*     In case of exception BIDU or BIDF, the destHU check not relevant
        LOOP AT ordim_confirm-exc_tab INTO ls_exc_tab
          WHERE iprcode = 'BIDF' OR
                iprcode = 'BIDU'.
        ENDLOOP.
        IF sy-subrc IS NOT INITIAL.
*       In case of a difference exception decrease the qty to zero
*       no destHU is required
          IF ordim_confirm-vsola_chr = 0.
            READ TABLE ordim_confirm-exc_tab TRANSPORTING NO FIELDS
              WITH KEY iprcode = 'DIFF'.
            IF sy-subrc IS INITIAL.
              EXIT.
            ENDIF.
          ENDIF.

        CALL METHOD /scwm/cl_rf_bll_srvc=>get_valid_prf
          RECEIVING
            rt_valid_prf = lt_valid_prf.

        READ TABLE lt_valid_prf
        WITH KEY valinp_fldname = 'PICKHU_VERIF'
             INTO ls_valid_prf .

        IF sy-subrc EQ 0.
*         when the PICKHU is for verifying then open verif field
          ls_valid_prf-flg_disable = ' '.

      MODIFY lt_valid_prf
        FROM ls_valid_prf
        TRANSPORTING flg_disable
        WHERE valinp_fldname EQ ls_valid_prf-valinp_fldname.

      CALL METHOD /scwm/cl_rf_bll_srvc=>set_valid_prf
        EXPORTING
          it_valid_prf = lt_valid_prf.

      /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
        iv_screlm_name = '/SCWM/S_RF_ORDIM_CONFIRM-PICKHU_VERIF' ).

*         check if PbV is used
          lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).
          IF lv_data_entry = wmegc_data_entry_voice.
*           with PbV just drop the error message
            MESSAGE e223(/scwm/rf_en).
          ELSE. "wmegc_data_entry_general
*           keep the original logic in case of general rfui because
*           we want to keep the verification field open
            /scwm/cl_rf_bll_srvc=>message(
               iv_msg_view = '1'
               iv_flg_continue_flow = 'X'
               iv_msgid           = '/SCWM/RF_EN'
               iv_msgty           = 'E'
               iv_msgno           = '223'
               iv_back            = 'X' ).
          ENDIF.
        ELSE.
*         no pickHU verification, open the PICKHU field
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
          iv_screlm_name = '/SCWM/S_RF_ORDIM_CONFIRM-PICKHU' ).

        MESSAGE e223(/scwm/rf_en).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_hu_req_on_destination
*&---------------------------------------------------------------------*
*&      Form  wo_rsrc_ty_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_WO_RSRC_TY_TEMP  text
*      <--P_LT_WO_RSRC_TY  text
*----------------------------------------------------------------------*
FORM wo_rsrc_ty_update
   USING
    it_wo_rsrc_ty_temp TYPE /scwm/tt_wo_rsrc_ty
   CHANGING
    ct_wo_rsrc_ty      TYPE /scwm/tt_wo_rsrc_ty
.

  FIELD-SYMBOLS  <ls_wo_rsrc_ty_temp> TYPE /scwm/wo_rsrc_ty.
  DATA:           ls_wo_rsrc_ty       TYPE /scwm/wo_rsrc_ty.
  DATA:           modif_index         TYPE sy-tabix.

  LOOP AT it_wo_rsrc_ty_temp ASSIGNING <ls_wo_rsrc_ty_temp>.
    READ TABLE ct_wo_rsrc_ty WITH KEY
       lgnum      = <ls_wo_rsrc_ty_temp>-lgnum
       who        = <ls_wo_rsrc_ty_temp>-who
*       rsrc_type  = <ls_wo_rsrc_ty_temp>-rsrc_type
     INTO ls_wo_rsrc_ty.
    modif_index = sy-tabix.
    IF sy-subrc IS INITIAL.
      ls_wo_rsrc_ty-status = <ls_wo_rsrc_ty_temp>-status.
      MODIFY ct_wo_rsrc_ty FROM ls_wo_rsrc_ty INDEX modif_index.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e075(/scwm/rf_de).
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE ct_wo_rsrc_ty WHERE status <> wmegc_wo_open AND
                             status <> wmegc_wo_in_process.

ENDFORM.                    " wo_rsrc_ty_update
*&---------------------------------------------------------------------*
*&      Form  ppp_move_hu
*&---------------------------------------------------------------------*
*       Move picked HU's from end-point of actual aarea to
*         start-point of next aarea
*----------------------------------------------------------------------*
*      -->IS_ORDIM_CONFIRM  text
*      -->IS_CONF  text
*      -->IS_WHO  text
*      -->IT_WHOHU  text
*      -->IS_RESOURCE  text
*----------------------------------------------------------------------*
FORM ppp_move_hu USING is_ordim_confirm TYPE /scwm/s_rf_ordim_confirm
                       is_conf          TYPE /scwm/to_conf
                       is_who           TYPE /scwm/s_who_int
                       it_whohu         TYPE /scwm/tt_whohu_int
                       is_resource      TYPE /scwm/s_rsrc.

  "this functionality is now part of class /SCWM/CL_RF_CONFIRM_PICKING

ENDFORM.                    " pick_pack_pass
*&---------------------------------------------------------------------*
*&      Form  ppp_change_dest
*&---------------------------------------------------------------------*
*       Change destination of HU-WT from resource to destnation
*         e.g. from GI-zone to end-point of actual aarea.
*       Create HU-WT for the unused pick-HU
*----------------------------------------------------------------------*
*      -->IS_ORDIM_CONFIRM  text
*      -->IS_WHO  text
*      -->IS_RESOURCE  text
*      -->IT_ORDIM_O  text
*      <--CS_CREA  text
*      <--CT_CREATE_HUTO  text
*----------------------------------------------------------------------*
FORM ppp_change_dest  USING    is_ordim_confirm
                                 TYPE /scwm/s_rf_ordim_confirm
                               is_who TYPE /scwm/s_who_int
                               is_resource TYPE /scwm/s_rsrc
                               it_ordim_o TYPE /scwm/tt_ordim_o
                               it_whohu TYPE /scwm/tt_whohu_int
                      CHANGING cs_crea TYPE /scwm/s_to_crea_hu
                               ct_create_huto TYPE /scwm/tt_to_crea_hu.

  "this functionality is now part of class /SCWM/CL_RF_CONFIRM_PICKING

ENDFORM.                    " ppp_change_dest
*&---------------------------------------------------------------------*
*&      Form  PPP_SRC_COMPLETED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WHO  text
*      -->P_RESOURCE  text
*----------------------------------------------------------------------*
FORM ppp_src_completed USING iv_nltyp TYPE /scwm/ltap_nltyp
                             iv_nlber TYPE /scwm/ltap_nlber
                             iv_nlpla TYPE /scwm/de_rf_nlpla
                             iv_procty TYPE /scwm/de_procty
                             is_who TYPE /scwm/s_who_int
                             is_resource TYPE /scwm/s_rsrc
                             it_ordim_confirm
                                TYPE /scwm/tt_rf_ordim_confirm.

  "this functionality is now part of class /SCWM/CL_RF_CONFIRM_PICKING

ENDFORM.                    " PPP_SRC_COMPLETED
*&---------------------------------------------------------------------*
*&      Form  LOST_PICKHU_SEARCH
*&---------------------------------------------------------------------*
*       Searches for the already created PickHUs which
*       are already located on the resource
*----------------------------------------------------------------------*
*      -->iv_LGNUM        Warehouse number
*      -->iv_WHO          WHO number
*      <--CT_RF_PICK_HUS  Pick HUs
*----------------------------------------------------------------------*
FORM lost_pickhu_search  USING    iv_lgnum       TYPE /scwm/lgnum
                                  iv_who         TYPE /scwm/de_who
                                  resource       TYPE /scwm/s_rsrc
                         CHANGING ct_rf_pick_hus TYPE /scwm/tt_rf_pick_hus.

  DATA ls_huhdr       TYPE /scwm/s_huhdr_int.
  DATA lt_huhdr       TYPE /scwm/tt_huhdr_int.
  DATA ls_rf_pick_hus TYPE /scwm/s_rf_pick_hus.
  DATA lt_ordim_c     TYPE /scwm/tt_ordim_c.
  DATA ls_mat_global  TYPE /scwm/s_material_global.
  DATA ls_huident     TYPE /scwm/s_huident.
  DATA lt_huident     TYPE /scwm/tt_huident.

  FIELD-SYMBOLS <ordim_c> TYPE /scwm/ordim_c.
  FIELD-SYMBOLS <huhdr>   TYPE /scwm/s_huhdr_int.

  CLEAR lt_huident.

  CALL FUNCTION '/SCWM/TO_READ_WHO'
    EXPORTING
      iv_lgnum     = iv_lgnum
      iv_who       = iv_who
    IMPORTING
      et_ordim_c   = lt_ordim_c
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      error        = 4
      OTHERS       = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Search for a confirmed WT where
* the source and the destination HU is different
  ls_huident-lgnum = iv_lgnum.
  LOOP AT lt_ordim_c ASSIGNING <ordim_c> WHERE nlenr IS NOT INITIAL.
    IF <ordim_c>-vlenr NE <ordim_c>-nlenr.

      ls_huident-huident = <ordim_c>-nlenr.
      APPEND ls_huident TO lt_huident.

    ENDIF.
  ENDLOOP.

  IF lt_huident IS NOT INITIAL.

    SORT lt_huident BY huident.
    DELETE ADJACENT DUPLICATES FROM lt_huident.

    CALL FUNCTION '/SCWM/HU_GT_FILL'
      EXPORTING
        it_huident = lt_huident
      IMPORTING
        et_huhdr   = lt_huhdr
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
*      Non-existing HU's. Not interesting to handle here!
    ENDIF.

    DELETE lt_huhdr WHERE top = ' '.
    DELETE lt_huhdr WHERE loc_type <> wmegc_rsrc.
    DELETE lt_huhdr WHERE rsrc <> resource-rsrc.

    LOOP AT lt_huhdr ASSIGNING <huhdr>.
      MOVE-CORRESPONDING <huhdr> TO ls_rf_pick_hus.

      IF NOT ls_rf_pick_hus-pmat_guid IS INITIAL.
*       read additional data (material master for description)
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
              EXPORTING
                iv_matid      = ls_rf_pick_hus-pmat_guid
                iv_langu      = sy-langu
                iv_lgnum      = iv_lgnum
              IMPORTING
                es_mat_global = ls_mat_global.
          CATCH /scwm/cx_md_interface.
*           one/more parameter(s) missing
            MESSAGE e004 WITH ls_rf_pick_hus-pmat_guid.
          CATCH /scwm/cx_md_material_exist.
*           Material does not exist
            MESSAGE e004 WITH ls_rf_pick_hus-pmat_guid.
          CATCH /scwm/cx_md_lgnum_locid.
*           Warehouse number is not assigned to an APO Location
            MESSAGE e004 WITH ls_rf_pick_hus-pmat_guid.
          CATCH /scwm/cx_md.
            MESSAGE e004 WITH ls_rf_pick_hus-pmat_guid.
        ENDTRY.

        ls_rf_pick_hus-pmat = ls_mat_global-matnr.

      ENDIF.

      APPEND ls_rf_pick_hus TO ct_rf_pick_hus.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " LOST_PICKHU_SEARCH
*&---------------------------------------------------------------------*
*&      Form  WHO_CLOSE_AT_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LGNUM  text
*      -->IV_WHO  text
*----------------------------------------------------------------------*
FORM who_close_at_error  USING  iv_lgnum TYPE /scwm/de_rf_lgnum
                                iv_who    TYPE /scwm/de_rf_who.

  "this functionality is now part of class /SCWM/CL_RF_CONFIRM_PICKING

ENDFORM.                    " WHO_CLOSE_AT_ERROR
