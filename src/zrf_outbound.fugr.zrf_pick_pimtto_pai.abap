FUNCTION zrf_pick_pimtto_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(T_RF_PICK_HUS) TYPE  /SCWM/TT_RF_PICK_HUS
*"     REFERENCE(CT_SERNR) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(CT_SERNR_DIFF) TYPE  /SCWM/TT_RF_SERNR
*"     REFERENCE(CS_SN) TYPE  /SCWM/S_RF_SN
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(CT_SERNR_LSCK) TYPE  /SCWM/TT_RF_SERNR
*"----------------------------------------------------------------------

* need to check data and to define next step according to customizing
  DATA: lv_line                TYPE i,
        lv_restart_transaction TYPE xfeld VALUE IS INITIAL,
        lv_lines               TYPE sy-tabix,
        lv_lowchk_inv          TYPE /scwm/de_lowchk_inv,
        lv_leave_trans         TYPE xfeld VALUE IS INITIAL,
        lv_rvseq_not_allowed   TYPE xfeld VALUE IS INITIAL,
        lv_fixpath             TYPE /scwm/de_fixpath,
        oref                   TYPE REF TO /scwm/cl_wm_packing,
        lv_applic              TYPE /scwm/de_applic,
        lv_pres_prf            TYPE /scwm/de_pres_prf,
        lv_ltrans              TYPE /scwm/de_ltrans,
        lv_step                TYPE /scwm/de_step,
        lv_fcode               TYPE /scwm/de_fcode,
        lv_state               TYPE /scwm/de_state,
        ls_who                 TYPE /scwm/who,
        lt_ordim_o             TYPE /scwm/tt_ordim_o,
        ls_ordim_o             TYPE /scwm/ordim_o,
        ls_ordim_confirm       TYPE /scwm/s_rf_ordim_confirm,
        ls_huhdr               TYPE /scwm/huhdr,
        ls_huhdr_int           TYPE /scwm/s_huhdr_int,
        lv_return              TYPE sy-subrc,
        lv_flg_nest_hu         TYPE xfeld VALUE IS INITIAL,
        lv_flg_mixed_hu        TYPE xfeld VALUE IS INITIAL,
        lv_flg_huent_ok        TYPE xfeld VALUE IS INITIAL,
        ls_ltap                TYPE /scwm/ltap,
        lv_field_act           TYPE text60,
        lt_valid_prf           TYPE /scwm/tt_valid_prf_ext,
        ls_huheader            TYPE /scwm/s_huhdr_int,
        ls_exc_tab             TYPE /scwm/s_rf_exc.

  DATA: lv_stock TYPE /scwm/de_ser_stock.
  DATA: lv_colsn    TYPE /scwm/de_rf_colsn,
        lv_qty_numc TYPE /scwm/ltap_vsola.
  DATA: lo_badi TYPE REF TO /scwm/ex_rf_prt_wo_hu.

  DATA  lv_hu_verif TYPE /scwm/de_vlenr_verif.

  DATA: lv_value       TYPE /scwm/de_attrib_value,
        lv_dlv_closed  TYPE char1,
        lv_no_split    TYPE char1,
        lv_subrc       TYPE sy-subrc,
        lv_buom        TYPE meins,
        lv_puom        TYPE /scwm/de_puom,
        lv_exccode_int TYPE /scwm/de_exccode_int,
        lv_lines_total TYPE i,
        lv_line_new    TYPE i,
        ls_lagp        TYPE /scwm/lagp,
        ls_exc         TYPE /scwm/s_rf_exc,
        ls_texcoie     TYPE /scwm/texcoie,
        ls_exccode     TYPE /scwm/s_iexccode,
        lt_mat_uom     TYPE /scwm/tt_material_uom.
  DATA: lo_excep TYPE REF TO data,
        lo_log   TYPE REF TO data.

  DATA: lv_data_entry TYPE /scwm/de_data_entry,
        lv_altme      TYPE string,
        lv_msg        TYPE string,
        lv_grammar    TYPE string,
        lv_altme_2    TYPE /scwm/de_rf_altme,
        lv_altme_3    TYPE /scwm/de_rf_altme,
        ls_screlm_pbv TYPE /scwm/s_rf_screlm_pbv,
        lt_listbox    TYPE /scwm/tt_rf_listbox.
  FIELD-SYMBOLS: <ls_listbox>  TYPE /scwm/s_rf_listbox,
                 <s_valid_prf> TYPE /scwm/s_valid_prf_ext.

  DATA  lv_picker_drv_repl TYPE xfeld.
  DATA  lv_execstep        TYPE /scwm/de_exec_step.
  DATA lv_flg_direct_stock TYPE xfeld.

  BREAK-POINT ID /scwm/rf_picking.

* Get actual fcode and line
  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  lv_line = /scwm/cl_rf_bll_srvc=>get_line( ).
  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

* Set process mode to foreground as default
  /scwm/cl_rf_bll_srvc=>set_prmod(
                             /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).

* batch field is EAN128 enable, need to update back the
* standard field
  ordim_confirm-batch = ordim_confirm-rfbatch.
  MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
    TRANSPORTING batch.

* Count number of planned pick-HUs
  DESCRIBE TABLE t_rf_pick_hus LINES lv_lines.
  ordim_confirm-sumphu = lv_lines.

  CASE lv_fcode.

    WHEN fcode_vrflop.
*     When receiving this FCODE it means that the PB for switching
*     from Pick-HU verification by HU logical position to HU scanning
*     and vice versa was enabled in the PBO of the step.

*     Disable Pick-HU verification by scanning function code VRFPHU.
      /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrflop ).
*     Enable logical position verification function code VRFLOP.
      /scwm/cl_rf_bll_srvc=>set_fcode_on( fcode_vrfphu ).
*     Turn off Pick-HU verification => HU scanning
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                      gc_scr_elmnt_pickhu_vrf ).
*     Turn on logical position verification
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
                                       gc_scr_elmnt_hupos_vrf ).
*     In case position is empty(not yet verified) the field is
*     open for input.
      IF ordim_confirm-hupos IS INITIAL.
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                     gc_scr_elmnt_hupos_vrf ).
      ENDIF.
*     Clear global field which was set when switching to Pick-HU
*     verification while logical position verification mode.
      CLEAR gv_hu_vrf_in_logpos_mode.
      EXIT.

    WHEN fcode_vrfphu.
*     When receiving those FCODE it means that the PB for switching
*     from Pick-HU verification by HU logical position to HU scanning
*     and vice versa was enabled in the PBO of the step.

*     Disable logical position verification function code VRFLOP.
      /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrfphu ).
*     Enable Pick-HU verification by scanning function code VRFPHU.
      /scwm/cl_rf_bll_srvc=>set_fcode_on( fcode_vrflop ).
*     Turn off logical position verification
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                      gc_scr_elmnt_hupos_vrf ).
*     Turn on Pick-HU verification => HU scanning
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
                                       gc_scr_elmnt_pickhu_vrf ).
*     In case HU not yet verified(empty) the field is
*     open for input.
      IF ordim_confirm-pickhu_verif IS INITIAL.
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                     gc_scr_elmnt_pickhu_vrf ).
      ENDIF.
*     Set global field when switching to Pick-HU verification
*     while logical position verification mode.
      gv_hu_vrf_in_logpos_mode = gc_xfeld.

      EXIT.

    WHEN fcode_print.

      lv_applic   = /scwm/cl_rf_bll_srvc=>get_applic( ).
      lv_pres_prf = /scwm/cl_rf_bll_srvc=>get_pres_prf( ).
      lv_ltrans   = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
      lv_step = /scwm/cl_rf_bll_srvc=>get_step( ).
      lv_state = /scwm/cl_rf_bll_srvc=>get_state( ).
      MOVE-CORRESPONDING who TO ls_who.

      CLEAR ls_ordim_o.
      REFRESH lt_ordim_o.
      LOOP AT tt_ordim_confirm INTO ls_ordim_confirm.
        MOVE-CORRESPONDING ls_ordim_confirm TO ls_ordim_o.
        APPEND ls_ordim_o  TO lt_ordim_o.
      ENDLOOP.

      IF NOT ordim_confirm-vlenr IS INITIAL.
*       create instance
        IF oref IS NOT BOUND.
          CREATE OBJECT oref.
        ENDIF.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

*       Initiate
*        oref->init(
*          EXPORTING
*            iv_lgnum = ordim_confirm-lgnum
*          EXCEPTIONS
*            OTHERS   = 99 ).

*       check HU exist.
        oref->/scwm/if_pack_bas~get_hu(
            EXPORTING
              iv_huident = ordim_confirm-vlenr
            IMPORTING
              es_huhdr   = ls_huhdr_int
            EXCEPTIONS
              not_found = 1 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
      TRY.
          GET BADI lo_badi
            FILTERS
              lgnum = ls_who-lgnum.

          MOVE-CORRESPONDING ls_huhdr_int TO ls_huhdr.

          CALL BADI lo_badi->print
            EXPORTING
              iv_lgnum    = ls_who-lgnum
              iv_applic   = lv_applic
              iv_pres_prf = lv_pres_prf
              iv_ltrans   = lv_ltrans
              iv_step     = lv_step
              iv_fcode    = lv_fcode
              iv_state    = lv_state
              is_who      = ls_who
              it_ordim_o  = lt_ordim_o
              is_huhdr    = ls_huhdr
              it_pick_hus = t_rf_pick_hus
            IMPORTING
              ev_return   = lv_return.

          IF lv_return <> 0.
            MESSAGE e053.
          ENDIF.

        CATCH cx_badi.
      ENDTRY.
      EXIT.

    WHEN fcode_backf.

      CALL FUNCTION '/SCWM/RF_PICK_BACKF_CHECK'
        CHANGING
          tt_ordim_confirm = tt_ordim_confirm
          lv_leave_trans   = lv_leave_trans.

      IF NOT lv_leave_trans IS INITIAL.
        CALL FUNCTION '/SCWM/RF_PICK_LEAVE_TRANS_CHCK'
          CHANGING
            who              = who
            resource         = resource
            ordim_confirm    = ordim_confirm
            tt_ordim_confirm = tt_ordim_confirm.
        EXIT.
      ELSE.
        EXIT.
      ENDIF.

    WHEN fcode_rvseq.

      CALL FUNCTION '/SCWM/FIXPATH_FLAG_READ'
        EXPORTING
          iv_lgnum    = ordim_confirm-lgnum
          iv_aarea    = ordim_confirm-aarea
          iv_actty    = ordim_confirm-act_type
*         IV_NOBUF    = 'X'
        IMPORTING
          ev_fixpath  = lv_fixpath
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        MESSAGE e048 WITH ordim_confirm-lgnum
                          ordim_confirm-aarea
                          ordim_confirm-act_type.
      ELSE.
        IF NOT lv_fixpath IS INITIAL.
          lv_rvseq_not_allowed = gc_xfeld.
        ENDIF.
      ENDIF.

      DATA: lo_badi2 TYPE REF TO /scwm/ex_rf_pick_wt_reseq.

      TRY.
          GET BADI lo_badi2
            FILTERS
              lgnum = who-lgnum.

          CALL BADI lo_badi2->resequence_allowed
            EXPORTING
              iv_lgnum               = who-lgnum
              is_who                 = who
              it_ordim_confirm       = tt_ordim_confirm
            CHANGING
              ev_resqnce_not_allowed = lv_rvseq_not_allowed.

        CATCH cx_badi.
      ENDTRY.

      IF NOT lv_rvseq_not_allowed IS INITIAL.
        MESSAGE e033.
      ELSE.

*       Change sort sequence.
        IF gv_pick_sort = gc_sort_ascending.
          gv_pick_sort = gc_sort_descending.
        ELSEIF gv_pick_sort = gc_sort_descending.
          gv_pick_sort = gc_sort_ascending.
        ENDIF.

*       Sorting TOs
        CALL FUNCTION '/SCWM/RF_PICK_WHO_TO_SORT'
          CHANGING
            ordim_confirm    = ordim_confirm
            tt_ordim_confirm = tt_ordim_confirm
            who              = who.

        /scwm/cl_rf_bll_srvc=>set_prmod(
                   /scwm/cl_rf_bll_srvc=>c_prmod_background ).

        /scwm/cl_rf_bll_srvc=>set_line( 1 ).

        CALL FUNCTION '/SCWM/RF_PICK_SET_STATE'
          CHANGING
            resource      = resource
            ordim_confirm = ordim_confirm.

        CALL FUNCTION '/SCWM/RF_PICK_SET_FCODE'
          CHANGING
            resource         = resource
            ordim_confirm    = ordim_confirm
            tt_ordim_confirm = tt_ordim_confirm.
      ENDIF.

      EXIT.

    WHEN fcode_huent.
*     Set HUENT according to user requirement.
      READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.
      IF ordim_confirm-huent = gc_xfeld.
        CLEAR ordim_confirm-huent.
      ELSE.
        IF ( ( ordim_confirm-vlenr IS INITIAL AND
               ordim_confirm-vlenr_verif IS INITIAL ) OR
              ( ordim_confirm-huent_not_allowed IS NOT INITIAL ) ).
          MESSAGE e170.
        ENDIF.
*       It is possible ,that verification field is not on the screen
*       In this case we will take over the value from source HU (VLENR).
        IF ordim_confirm-vlenr_verif IS INITIAL.
          lv_hu_verif = ordim_confirm-vlenr.
        ELSE.
          lv_hu_verif = ordim_confirm-vlenr_verif.
        ENDIF.

        CALL FUNCTION '/SCWM/RF_PICK_HUNEST_CHECK'
          EXPORTING
            iv_hu_verif        = lv_hu_verif
            iv_hu_parent       = ordim_confirm-vlenr
            iv_lgnum           = ordim_confirm-lgnum
            iv_rsrc            = ordim_confirm-srsrc
            iv_lgpla           = ordim_confirm-vlpla
          IMPORTING
            ev_flg_nest_ok     = lv_flg_nest_hu
            ev_flg_huent_ok    = lv_flg_huent_ok
            ev_flg_mixed_hu    = lv_flg_mixed_hu
**            ev_hu_verif        = ordim_confirm-vlenr_verif
          CHANGING
            ordim_confirm      = ordim_confirm
          EXCEPTIONS
            hu_not_in_location = 1
            OTHERS             = 2.
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              MESSAGE e044.
            WHEN OTHERS.
              MESSAGE ID sy-msgid
                    TYPE sy-msgty
                  NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2
                         sy-msgv3 sy-msgv4.
          ENDCASE.
        ENDIF.
        IF NOT lv_flg_huent_ok IS INITIAL AND
               lv_flg_mixed_hu IS INITIAL.
          IF tt_nested_hu[] IS INITIAL.
            ordim_confirm-huent = gc_xfeld.
          ELSE.
*         Raise query Do you want to clear inner HU
*         reporting for HU take over?
            IF /scwm/cl_rf_dynpro_srvc=>display_message(
                iv_msgid           = gc_picking_msgid
                iv_msgty           = 'Q' "gc_msgty_warning
                iv_msgno           = '058' ) =
                        /scwm/cl_rf_bll_srvc=>c_answer_yes.
              REFRESH tt_nested_hu.
              CLEAR gv_nested_save.
              ordim_confirm-huent = gc_xfeld.
            ENDIF.
          ENDIF.
        ELSEIF NOT lv_flg_huent_ok IS INITIAL AND
               NOT lv_flg_mixed_hu IS INITIAL.
*         HU is mixed hu.
          MESSAGE e059 WITH ordim_confirm-vlenr.
        ELSEIF lv_flg_mixed_hu IS INITIAL AND
               lv_flg_huent_ok IS INITIAL.
*         Enter pick-HU number or HU position
          MESSAGE e008 WITH ordim_confirm-vlenr.
        ENDIF.

      ENDIF.
*     if HUWD changed manually(set or removed) set flag to true
      gv_manual_no_huwd = abap_true.
      MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
             TRANSPORTING huent.
      EXIT.

    WHEN 'CHGUOM'.
*     If FCode = CHGUOM, switch field SUOM to input possible
      BREAK-POINT ID /scwm/suom.

      READ TABLE ordim_confirm-exc_tab TRANSPORTING NO FIELDS
        WITH KEY iprcode = wmegc_iprcode_diff.

      lv_subrc = sy-subrc.

*     Check if product has stock-relevant UoM; if not a change of the
*       UoM is not allowed
      IF lv_subrc <> 0.
        CALL FUNCTION '/SCWM/MATERIAL_SUOM_RELEVANT'
          EXPORTING
            iv_lgnum    = ordim_confirm-lgnum
            iv_matid    = ordim_confirm-matid
            iv_entitled = ordim_confirm-entitled
          IMPORTING
            ev_buom     = lv_buom
            ev_puom     = lv_puom
            et_mat_uom  = lt_mat_uom.

        IF lines( lt_mat_uom ) <= 1.
          MESSAGE e608.
        ENDIF.
      ENDIF.

*     Check if we have a reference to a delivery
      IF ordim_confirm-qdoccat <> wmegc_doccat_pdo.
        IF lv_subrc <> 0.  "No exception set
*         Set exception code based on internal exception
          CALL FUNCTION '/SCWM/TEXCOIE_READ_SINGLE'
            EXPORTING
              iv_lgnum       = ordim_confirm-lgnum
              iv_exccode_int = wmegc_exccodei_auom
            IMPORTING
              es_texcoie     = ls_texcoie
            EXCEPTIONS
              not_found      = 1
              wrong_input    = 2
              OTHERS         = 3.
          IF sy-subrc = 1 OR
             ls_texcoie-exccode IS INITIAL.
            MESSAGE e090(/scwm/exception) WITH wmegc_exccodei_auom.
          ENDIF.

          IF sy-subrc > 1.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF sy-subrc = 0 AND
             ls_texcoie-exccode IS NOT INITIAL.

*           read data for exception code
            MOVE-CORRESPONDING ordim_confirm TO ls_ltap.
            MOVE ls_texcoie-exccode TO ls_exccode-exccode.

*           Create instance of Exception object
            IF lo_excep IS NOT BOUND.
              CALL METHOD /scwm/cl_exception_appl=>create_exception_object
                RECEIVING
                  rp_excep = lo_excep.
            ENDIF.

            CASE ordim_confirm-trart.
              WHEN wmegc_trart_pick. "Picking
                MOVE wmegc_buscon_tpi TO gv_buscon.
              WHEN wmegc_trart_int. "Internal movement
                MOVE wmegc_buscon_tim TO gv_buscon.
              WHEN wmegc_trart_tr.  "Special case Posting change with movement
                MOVE wmegc_buscon_tim TO gv_buscon.
              WHEN OTHERS.
                MESSAGE e145 WITH ordim_confirm-trart.
            ENDCASE.

            IF lv_data_entry = wmegc_data_entry_voice.
              lv_execstep = gv_exec_step.
            ELSE.
              lv_execstep = wmegc_execstep_a0.
            ENDIF.
            CALL METHOD /scwm/cl_exception_appl=>verify_exception_code
              EXPORTING
                is_appl_item_data = ls_ltap
                iv_lgnum          = ordim_confirm-lgnum
                iv_buscon         = gv_buscon
                iv_execstep       = lv_execstep
                ip_excep          = lo_excep
                iv_dlv_closed     = lv_dlv_closed
                iv_no_split       = lv_no_split
              CHANGING
                cs_exccode        = ls_exccode
                cp_log            = lo_log.

*           Exception code is not allowed!
            IF ls_exccode-valid <> 'X'.
              MESSAGE e003(/scwm/exception) WITH ls_texcoie-exccode.
            ENDIF.

            IF ls_exccode-iprcode EQ wmegc_iprcode_diff.
              READ TABLE ordim_confirm-exc_tab TRANSPORTING NO FIELDS
                WITH KEY iprcode = wmegc_iprcode_diff.
              IF sy-subrc <> 0.
                MOVE: ls_exccode-iprcode TO ls_exc-iprcode,
                      ls_exccode-exccode TO ls_exc-exccode,
                      gv_exec_step TO ls_exc-exec_step.
                IF ls_exccode-iprcode IS INITIAL.
                  ls_exc-no_proc = 'X'.
                ENDIF.
                APPEND ls_exc TO ordim_confirm-exc_tab.

                ordim_confirm-difty = ls_exccode-difty.

                MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
                  TRANSPORTING exc_tab difty.
              ENDIF.
            ELSE.
              MESSAGE e077(/scwm/exception) WITH ls_texcoie-exccode.
            ENDIF.
          ENDIF.
        ENDIF.
*       Switch input on for quantity and AltME
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                                gc_scr_elmnt_nista_vrf ).

        CLEAR ordim_confirm-nista_verif.

        MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
          TRANSPORTING nista_verif altme.

        /scwm/cl_rf_bll_srvc=>set_screlm_input_on(
                              '/SCWM/S_RF_ORDIM_CONFIRM-ALTME' ).

        CALL METHOD /scwm/cl_rf_bll_srvc=>set_field
          EXPORTING
            iv_field = '/SCWM/S_RF_ORDIM_CONFIRM-ALTME'.

      ELSE.
        MESSAGE e607(/scwm/rf_en).
      ENDIF.

*     Set the PbV field properties
      IF lv_data_entry = wmegc_data_entry_voice.
        ls_screlm_pbv = /scwm/cl_rf_bll_srvc=>get_screlm_pbv( ).

*       PromptText: Actual Unit P C. Enter new Unit
        lv_altme = ordim_confirm-altme.
        lv_altme = /scwm/cl_rf_bll_srvc=>build_prompt_for_field( lv_altme ).
        MESSAGE i707 WITH lv_altme INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'UOM' iv_pt = wmegc_pt_prompt iv_sf = 'FLD_PROMPT_10' iv_use_symsg = 'X' ).
        ls_screlm_pbv-fld_prompt_10 = lv_msg.

*       HelpText: Invalid Unit of Measure
        MESSAGE i708 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'UOM' iv_pt = wmegc_pt_help iv_sf = 'HELP_10' iv_use_symsg = 'X' ).
        ls_screlm_pbv-help_10 = lv_msg.

*       FilledText: Unit of Measure x
        MESSAGE i709 INTO lv_msg.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'UOM' iv_pt = wmegc_pt_filled iv_sf = 'FILLED_5' iv_use_symsg = 'X' ).
        ls_screlm_pbv-filled_5 = lv_msg.

*       Grammar: PC | K06 | K08
        CALL METHOD /scwm/cl_rf_bll_srvc=>get_listbox
          EXPORTING
            iv_field   = '/SCWM/S_RF_ORDIM_CONFIRM-ALTME'
          RECEIVING
            rt_listbox = lt_listbox.
        LOOP AT lt_listbox ASSIGNING <ls_listbox>.
          lv_altme_2 = <ls_listbox>-value.
          WRITE lv_altme_2 TO lv_altme_3.  "We must convert the unit e.g. ST -> PC
          IF sy-tabix = 1.
            lv_grammar = lv_altme_3.
          ELSE.
            CONCATENATE lv_grammar lv_altme_3
              INTO lv_grammar
              SEPARATED BY ' | '.
          ENDIF.
        ENDLOOP.
        /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'UOM' iv_pt = wmegc_pt_grammar iv_sf = 'GRAMMAR_10' ).
        ls_screlm_pbv-grammar_10 = lv_grammar.

        /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).

        " EWM 9.40 Enhancements
        "---------------------------------------------------------------
        CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
        "---------------------------------------------------------------

      ENDIF.

      RETURN.

    WHEN 'DIFFPB' OR 'BINDPB'.   "PbV "Direct difference or Bin Denial triggered by pushbutton
      READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.

      IF lv_fcode = 'BINDPB'.
*       Check which kind of bin denial we have (Partial or Full)
        IF ordim_confirm-nista_verif IS NOT INITIAL.
          lv_exccode_int = wmegc_exccodei_bidp.
        ELSE.
          lv_exccode_int = wmegc_exccodei_bidf.
          ordim_confirm-nista_verif = 0.
          CALL METHOD /scwm/cl_rf_bll_srvc=>set_screlm_input_off
            EXPORTING
              iv_screlm_name = '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF'.
        ENDIF.
      ENDIF.
      IF lv_fcode = 'DIFFPB'.
        lv_exccode_int = wmegc_exccodei_diff.
        IF ordim_confirm-nista_verif IS INITIAL.
          ordim_confirm-nista_verif = 0.
          CALL METHOD /scwm/cl_rf_bll_srvc=>set_screlm_input_off
            EXPORTING
              iv_screlm_name = '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF'.
        ENDIF.
      ENDIF.
*     Read the assigned external exception
      CALL FUNCTION '/SCWM/TEXCOIE_READ_SINGLE'
        EXPORTING
          iv_lgnum       = ordim_confirm-lgnum
          iv_exccode_int = lv_exccode_int
        IMPORTING
          es_texcoie     = ls_texcoie
        EXCEPTIONS
          not_found      = 1
          wrong_input    = 2
          OTHERS         = 3.
      IF sy-subrc = 1 OR
         ls_texcoie-exccode IS INITIAL.
        MESSAGE e090(/scwm/exception) WITH lv_exccode_int.
      ENDIF.

      IF sy-subrc > 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF sy-subrc = 0 AND
        ls_texcoie-exccode IS NOT INITIAL.

*       read data for exception code
        MOVE-CORRESPONDING ordim_confirm TO ls_ltap.
        MOVE ls_texcoie-exccode TO ls_exccode-exccode.

*       Create instance of Exception object
        IF lo_excep IS NOT BOUND.
          CALL METHOD /scwm/cl_exception_appl=>create_exception_object
            RECEIVING
              rp_excep = lo_excep.
        ENDIF.

        CASE ordim_confirm-trart.
          WHEN wmegc_trart_pick. "Picking
            MOVE wmegc_buscon_tpi TO gv_buscon.
          WHEN wmegc_trart_int. "Internal movement
            MOVE wmegc_buscon_tim TO gv_buscon.
          WHEN wmegc_trart_tr.  "Special case Posting change with movement
            MOVE wmegc_buscon_tim TO gv_buscon.
          WHEN OTHERS.
            MESSAGE e145 WITH ordim_confirm-trart.
        ENDCASE.

        CALL METHOD /scwm/cl_exception_appl=>verify_exception_code
          EXPORTING
            is_appl_item_data = ls_ltap
            iv_lgnum          = ordim_confirm-lgnum
            iv_buscon         = gv_buscon
            iv_execstep       = gv_exec_step
            ip_excep          = lo_excep
            iv_dlv_closed     = lv_dlv_closed
            iv_no_split       = lv_no_split
          CHANGING
            cs_exccode        = ls_exccode
            cp_log            = lo_log.

*       Exception code is not allowed!
        IF ls_exccode-valid <> 'X'.
          MESSAGE e003(/scwm/exception) WITH ls_texcoie-exccode.
        ENDIF.

*       Adjust the quantities and set the correct exception
        IF ls_exccode-iprcode EQ wmegc_iprcode_diff OR
          ls_exccode-iprcode EQ wmegc_iprcode_bidf OR
          ls_exccode-iprcode EQ wmegc_iprcode_bifp.
          READ TABLE ordim_confirm-exc_tab TRANSPORTING NO FIELDS
            WITH KEY iprcode = wmegc_iprcode_diff.
          IF sy-subrc <> 0.
            MOVE: ls_exccode-iprcode TO ls_exc-iprcode,
                  ls_exccode-exccode TO ls_exc-exccode,
                  gv_exec_step TO ls_exc-exec_step.
            IF ls_exccode-iprcode IS INITIAL.
              ls_exc-no_proc = 'X'.
            ENDIF.
            APPEND ls_exc TO ordim_confirm-exc_tab.

            ordim_confirm-difty = ls_exccode-difty.
            IF lv_fcode = 'BINDPB'.
              IF ls_exccode-iprcode = wmegc_iprcode_bidf.
                CLEAR ordim_confirm-huent.
                ordim_confirm-ndifa = ordim_confirm-vsola.
                ordim_confirm-nista = 0.
                ordim_confirm-nista_verif = 0.
                PERFORM pimtto_close_fields.
              ELSE.
              ENDIF.
              ordim_confirm-bind = gc_xfeld.
            ENDIF.
*           in case of BIDP we checking the verification field on the screen
*           if only the qty verification field is open, then we can process the WT
            IF ( lv_fcode = 'BINDPB' AND ls_exccode-iprcode = wmegc_iprcode_bifp ) OR
               ( lv_fcode = 'DIFFPB' AND ls_exccode-iprcode = wmegc_iprcode_diff ).
              lt_valid_prf = /scwm/cl_rf_bll_srvc=>get_valid_prf( ).
              LOOP AT lt_valid_prf TRANSPORTING NO FIELDS
                    WHERE flg_disable NE 'C' AND
                          flg_disable NE 'X' AND
                          valid_obj <> 'QTY'.
              ENDLOOP.
              IF sy-subrc IS NOT INITIAL.
                READ TABLE lt_valid_prf ASSIGNING <s_valid_prf>
                  WITH KEY valid_obj = 'QTY'.
                IF sy-subrc IS INITIAL.
*                 close the verification field
                  <s_valid_prf>-flg_disable = 'X'.
                  /scwm/cl_rf_bll_srvc=>set_valid_prf( lt_valid_prf ).

                  ordim_confirm-nista = ordim_confirm-nista_verif.
                  ordim_confirm-ndifa = ordim_confirm-vsola - ordim_confirm-nista_verif.
                ENDIF.
              ENDIF.
            ENDIF.
            MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
          ENDIF.
        ELSE.
          MESSAGE e077(/scwm/exception) WITH ls_texcoie-exccode.
        ENDIF.
*       Set the global variable GV_BUSCON for later PbV specific confirmation
        CALL FUNCTION '/SCWM/RF_PICK_SET_GLOBVAR'
          EXPORTING
            iv_buscon    = gv_buscon
            iv_exec_step = gv_exec_step.

        IF ls_exccode-iprcode = wmegc_iprcode_bidf.
          gv_no_fup_wt = 'X'.
        ENDIF.
      ENDIF.

      ls_screlm_pbv = /scwm/cl_rf_bll_srvc=>get_screlm_pbv( ).
      CLEAR ls_screlm_pbv-grammar_6.
      ls_screlm_pbv-grammar_lnk_1 = 'EWM_NUMERIC.JSGF'.
      /scwm/cl_rf_bll_srvc=>set_screlm_pbv( ls_screlm_pbv ).
      /scwm/cl_rf_bll_srvc=>build_index_pbv(
          EXPORTING iv_if = 'QTY' iv_pt = wmegc_pt_grammarlnk iv_sf = 'GRAMMAR_LNK_1' ).

      " EWM 9.40 Enhancements
      "---------------------------------------------------------------
      CALL METHOD /scwm/cl_rf_bll_srvc=>build_pbv_prompt_via_msg.
      "---------------------------------------------------------------

    WHEN 'DIFF' OR 'DIFFBD'.   "PbV "Difference or Bin Denial triggered by exception
*     Exception, DIFTY and BIND are set in //RF_PICK_EXCEPTION
*     NDIFA is calculated in //RF_PICK_QTY_CHECK

*     Set the global variable GV_BUSCON for later PbV specific confirmation
      CALL FUNCTION '/SCWM/RF_PICK_SET_GLOBVAR'
        EXPORTING
          iv_buscon    = gv_buscon
          iv_exec_step = gv_exec_step.

    WHEN 'SKIP'.  "PbV "Jump to next entry
*      for combined picking
      DATA:lt_combined_lead TYPE /scwm/tt_rf_tanum,
           lv_tanum         TYPE /scwm/de_rf_tanum.
      CALL METHOD /scwm/cl_rf_comb_pick=>get_lead
        IMPORTING
          et_combined_lead = lt_combined_lead.

      IF lt_combined_lead IS NOT INITIAL.
        DESCRIBE TABLE lt_combined_lead LINES lv_lines_total.
        READ TABLE lt_combined_lead WITH KEY ordim_confirm-tanum TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_line = sy-tabix.
          IF lv_line = lv_lines_total.
            lv_line = 1.
          ELSE.
            lv_line = lv_line + 1.
          ENDIF.

          READ TABLE lt_combined_lead INTO lv_tanum INDEX lv_line.
          IF sy-subrc = 0.
            READ TABLE tt_ordim_confirm WITH KEY tanum = lv_tanum TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              lv_line_new = sy-tabix.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
*     First check if we can jump to next entry
        DESCRIBE TABLE tt_ordim_confirm LINES lv_lines_total.
        lv_line_new = lv_line.
        DO.
          CLEAR ls_ordim_confirm.
          IF lv_line_new = lv_lines_total.  "End of table reached
            lv_line_new = 1.
          ELSE.
            ADD 1 TO lv_line_new.
          ENDIF.

          READ TABLE tt_ordim_confirm INTO ls_ordim_confirm
              INDEX lv_line_new.
*       filter out the 2nd leg WTs
          IF ls_ordim_confirm-srsrc IS NOT INITIAL.
            CONTINUE.
          ENDIF.
          EXIT.
        ENDDO.
      ENDIF.

*     in case of PbV, the next task button should reset the WT
      IF lv_data_entry = wmegc_data_entry_voice.
        PERFORM wt_refresh USING    lv_line
                           CHANGING tt_ordim_confirm.
      ENDIF.

*     We can jump to next entry; Simulate PGDN
      CALL METHOD /scwm/cl_rf_bll_srvc=>set_line
        EXPORTING
          iv_line = lv_line_new.

*     Check the next WT choosed by PGUP-PGDN for REC
      READ TABLE tt_ordim_confirm INTO ls_ordim_confirm
          INDEX lv_line_new TRANSPORTING tanum.
      CALL FUNCTION '/SCWM/REC_WT_CHECK'
        EXPORTING
          iv_tanum            = ls_ordim_confirm-tanum
        EXCEPTIONS
          internal_error      = 1
          rec_area_is_engaged = 2
          OTHERS              = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*     Get next TO data.
      READ TABLE tt_ordim_confirm INTO ordim_confirm
           INDEX lv_line_new.

*     Fill container for application specific verification
      CALL FUNCTION '/SCWM/RF_FILL_WME_VERIF'
        EXPORTING
          iv_lgnum     = ordim_confirm-lgnum
          iv_procty    = ordim_confirm-procty
          iv_trart     = ordim_confirm-trart
          iv_act_type  = ordim_confirm-act_type
          iv_aarea     = ordim_confirm-aarea
        IMPORTING
          es_wme_verif = wme_verif.

      CALL METHOD /scwm/cl_rf_bll_srvc=>set_rebuild_vrf.

      CALL FUNCTION '/SCWM/RF_PICK_SET_STATE'
        CHANGING
          resource      = resource
          ordim_confirm = ordim_confirm.

      CALL FUNCTION '/SCWM/RF_PICK_SET_FCODE'
        CHANGING
          resource         = resource
          ordim_confirm    = ordim_confirm
          tt_ordim_confirm = tt_ordim_confirm.

      CALL METHOD /scwm/cl_rf_bll_srvc=>set_field
        EXPORTING
          iv_field = ''.

      RETURN.

    WHEN 'CLEARF'.

      PERFORM clear_fields CHANGING ordim_confirm
                                    tt_ordim_confirm.
      RETURN.

  ENDCASE.

  PERFORM check_hu_req_on_destination
              USING
                 ordim_confirm-lgnum
                 resource-rsrc
                 ordim_confirm
                 t_rf_pick_hus
                 tt_nested_hu.

* Calling the CW screen
  lv_field_act = /scwm/cl_rf_bll_srvc=>get_cursor_field( ).

  CALL FUNCTION '/SCWM/RF_QTY_POSITIVE_CHECK'
    EXPORTING
      iv_qty   = ordim_confirm-nista_verif
    EXCEPTIONS
      negative = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* The CW call determination (filling of ls_ltap-cwreq)
* runs independently from the cursor field position.
  IF ordim_confirm-cwrel = abap_true.
*   Fill actual quantity in base unit of measure in LS_LTAP
    DATA  lv_quantity TYPE /lime/quantity.
    CLEAR ls_ltap.
    IF  ordim_confirm-altme <> ordim_confirm-meins.
      lv_quantity = ordim_confirm-vsola_chr.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = ordim_confirm-matid
              iv_quan      = lv_quantity
              iv_unit_from = ordim_confirm-altme
              iv_unit_to   = ordim_confirm-meins
              iv_batchid   = ordim_confirm-batchid
            IMPORTING
              ev_quan      = lv_quantity.
        CATCH /scwm/cx_md_interface
              /scwm/cx_md_batch_required
              /scwm/cx_md_internal_error
              /scwm/cx_md_batch_not_required
              /scwm/cx_md_material_exist.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.
      ls_ltap-nistm = lv_quantity.
    ELSE.
      ls_ltap-nistm = ordim_confirm-vsola_chr.
    ENDIF.
    IF ordim_confirm-sguid_hu IS NOT INITIAL.

      MOVE-CORRESPONDING ordim_confirm TO ls_ltap.
      CALL FUNCTION '/SCWM/CWINPUT_WT'
        CHANGING
          cs_ltap         = ls_ltap
*         CT_LTAP         =
        EXCEPTIONS
          interface_error = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
*     Check CW input without source HU
      MOVE-CORRESPONDING ordim_confirm TO ls_ltap.
      CALL FUNCTION '/SCWM/RF_CWINPUT_WT'
        CHANGING
          cs_ltap         = ls_ltap
        EXCEPTIONS
          interface_error = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

*   Checking if full bin denial (BIDU, BIDF) is used
    LOOP AT ordim_confirm-exc_tab INTO ls_exc_tab.
      IF ls_exc_tab-iprcode = iprcode_bidf OR
         ls_exc_tab-iprcode = iprcode_bidu.
        CLEAR ls_ltap-cwreq.
        EXIT.
      ENDIF.
    ENDLOOP.

*   CW screen should be called by the quantity verification field
*   to keep the CW functionality of the old version (behaviour)
    IF gv_cw_called = abap_false AND ls_ltap-cwreq = abap_true AND
      ( lv_field_act = '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF' OR lv_fcode = gc_fcode_callcw ).
      " Move to mediator screen.
      /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
      /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_fcmed ).
*     Picking use TT_ORDIM_CONFIRM to display data but CW screen works from ORDIM_CONFIRM
      READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.
      RETURN.
    ENDIF.
  ENDIF.

  lt_valid_prf = /scwm/cl_rf_bll_srvc=>get_valid_prf( ).

  LOOP AT lt_valid_prf TRANSPORTING NO FIELDS
        WHERE flg_disable NE 'C' AND
              flg_disable NE 'X'.

*   Initiate screen parameter
    /scwm/cl_rf_bll_srvc=>init_screen_param( ).
*   Set screen parameter
    /scwm/cl_rf_bll_srvc=>set_screen_param('TT_ORDIM_CONFIRM').

    IF lv_line > lines( tt_ordim_confirm ).
      /scwm/cl_rf_bll_srvc=>set_line( 1 ).
    ENDIF.
    EXIT.
  ENDLOOP.

* Special logic for CW
* If we have open verification fields and cursor is on quantiy and we are
*   CW relevant we jump to CW screen
  IF sy-subrc = 0 AND
     lv_field_act EQ '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF' AND
     ordim_confirm-cwrel = abap_true.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
    RETURN.
  ENDIF.

* New logic because of quick ENTER.
* If we have open verification fields we display the screen again.
* If the return-fcode not inital it means the main screen has been finished
* already, but an additional step was executed (e.g low stock check), and
* no further check required on the main screen
  IF sy-subrc = 0 AND ordim_confirm-return_fcode IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
    RETURN.
  ENDIF.

* CW screen should be called if we have no quantity verification field or
* the verification field is filled after exception (value enter not possible)
  IF gv_cw_called = abap_false AND ls_ltap-cwreq = abap_true AND ls_ltap-nista IS NOT INITIAL.
    " Move to mediator screen.
    /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
    /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_fcmed ).
*     Picking use TT_ORDIM_CONFIRM to display data but CW screen works from ORDIM_CONFIRM
    READ TABLE tt_ordim_confirm INTO ordim_confirm INDEX lv_line.
    RETURN.
  ENDIF.

  READ TABLE ordim_confirm-exc_tab INTO ls_exc_tab INDEX 1.
* Special logic for Documentary Batch
* If the product is Documentary Batch relevant and the Batch field is
* initial we display the screen again
  IF ( ordim_confirm-dbind     IS NOT INITIAL OR
       ordim_confirm-batch_req IS NOT INITIAL ) AND ordim_confirm-batch IS INITIAL.
*   exept in case of BIDF, BIDU, DIFF with zero quantity
    IF NOT ( ls_exc_tab-iprcode = iprcode_bidf
          OR ls_exc_tab-iprcode = iprcode_bidu
          OR ls_exc_tab-iprcode = iprcode_bfrp
          OR ( ls_exc_tab-iprcode = iprcode_diff AND ordim_confirm-nista = 0 )
           ).
      /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_foregr ).
      RETURN.
    ENDIF.
  ENDIF.

* Check whether the source HU is nested HU or not.
* this check force the lower-level HU add even if the
* HU verification is hidden on the screen
  PERFORM nested_hu_chk CHANGING ordim_confirm
                                 lv_flg_nest_hu
                                 lv_flg_huent_ok
                                 lv_flg_direct_stock.

* If the HU is nested, and no lower level HU is chosen
* formerly and HUWD is not set, then do it now
  IF NOT lv_flg_nest_hu IS INITIAL AND
     ordim_confirm-huent IS INITIAL AND
     tt_nested_hu IS INITIAL.
*   But not with bin denial full.
    LOOP AT ordim_confirm-exc_tab INTO ls_exc_tab
         WHERE iprcode = iprcode_bidf OR
               iprcode = iprcode_bidu.

    ENDLOOP.
    IF sy-subrc IS NOT INITIAL AND
       lv_flg_direct_stock IS INITIAL.
      /scwm/cl_rf_bll_srvc=>set_prmod(
                   /scwm/cl_rf_bll_srvc=>c_prmod_background ).
      /scwm/cl_rf_bll_srvc=>set_fcode( fcode_nesthu ).
      RETURN.
    ENDIF.
  ENDIF.

*  IF ordim_confirm-matnr IS NOT INITIAL.
** check if it is serialized
*    CLEAR: gv_serial.
*    gv_matnr = ordim_confirm-matnr .
*    PERFORM gf_serialized USING gv_matnr
*                          CHANGING gv_serial.
*    IF gv_serial IS NOT INITIAL.
*
********************************
** custom code to validate and post pickhu event
*      DATA : iv_activetab   TYPE sy-ucomm,
*             cs_tbox_fields TYPE /sttpec/s_whs_test_tbox_flds,
*             ct_tbox_resobj TYPE /sttpec/t_whs_test_tbox_resobj,
*             lv_rdocid      TYPE /scwm/de_docid,
*             lv_docno       TYPE /scdl/dl_docno_int.
*
*
*      iv_activetab = 'PICKHU'.
*      CONCATENATE '(00)' ordim_confirm-nlenr+2(18) INTO cs_tbox_fields-scan_obj.
*      SELECT SINGLE rdocid FROM /scwm/ordim_o INTO lv_rdocid
*          WHERE tanum = ordim_confirm-tanum
*            AND lgnum = ordim_confirm-lgnum.
*      SELECT SINGLE docno FROM /scdl/db_proch_o INTO lv_docno
*          WHERE docid = lv_rdocid.
*      SHIFT lv_docno LEFT DELETING LEADING '0'.
*      cs_tbox_fields-docnum = lv_docno.
*
**cs_tbox_fields-docyear = '2020'.
**cs_tbox_fields-logsys = 'H01CLNT100'.
**cs_tbox_fields-plant = '2000'.
*      cs_tbox_fields-sscc = ordim_confirm-nlenr+2(18).
*
*      PERFORM user_command_val USING    iv_activetab
*                            CHANGING cs_tbox_fields
*                                     ct_tbox_resobj.
*      PERFORM send_event USING    iv_activetab
*                         CHANGING cs_tbox_fields
*                                  ct_tbox_resobj.
*      PERFORM validate_hu_attp.
*******************************************
*    ENDIF.
*  ENDIF.




  IF ( ordim_confirm-lowchk_inv IS INITIAL OR
     ordim_confirm-lowchk_inv = gc_lowchk_inv_cancel ).
    TRY.
        CALL FUNCTION '/SCWM/TO_CONF_INV_RF'
          EXPORTING
            iv_lgnum      = ordim_confirm-lgnum
            iv_tanum      = ordim_confirm-tanum
            iv_vsola      = ordim_confirm-vsola
            iv_ndifa      = ordim_confirm-ndifa
            iv_difty      = ordim_confirm-difty
          IMPORTING
            ev_lowchk_inv = lv_lowchk_inv.
      CATCH /scwm/cx_core.
    ENDTRY.


  ENDIF.

*     If low stock check is requ. but BIDF, BIDP, DIFF is posted before
*   we make an internal low stock check with remain. qty = 0 or
*   reject the low stock check on difference to source bin
  IF lv_lowchk_inv = gc_lowchk_inv_perform.
    LOOP AT ordim_confirm-exc_tab INTO ls_exc_tab.
      IF ls_exc_tab-iprcode = iprcode_bidf OR
         ls_exc_tab-iprcode = iprcode_bidp OR
         ls_exc_tab-iprcode = iprcode_bidu OR
         ls_exc_tab-iprcode = iprcode_bprp OR
         ls_exc_tab-iprcode = iprcode_bfrp OR
         ls_exc_tab-iprcode = iprcode_diff OR
         ordim_confirm-iprcode = iprcode_bidf OR
         ordim_confirm-iprcode = iprcode_bidp OR
         ordim_confirm-iprcode = iprcode_bidu OR
         ordim_confirm-iprcode = iprcode_bprp OR
         ordim_confirm-iprcode = iprcode_bfrp OR
         ordim_confirm-iprcode = iprcode_diff.

        IF ( ls_exc_tab-iprcode = iprcode_diff OR
             ordim_confirm-iprcode = iprcode_diff ) AND
           ( ordim_confirm-difty   = gc_difty_account OR
             ordim_confirm-difty   = gc_difty_bin ).
          CONTINUE.
        ENDIF.

        IF ordim_confirm-difty = gc_difty_bin OR
           ordim_confirm-difty = gc_difty_account.
          ordim_confirm-lowchk_inv = wmegc_lowchk_reject.
        ELSE.
          CLEAR ordim_confirm-resta.
          CLEAR ordim_confirm-resta_verif.
          ordim_confirm-lowchk_inv = gc_lowchk_inv_compl.
        ENDIF.
        MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
               TRANSPORTING lowchk_inv resta resta_verif.
        CLEAR lv_lowchk_inv.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Call low stock check step if needed.
  IF lv_lowchk_inv = gc_lowchk_inv_perform.
*    Set process mode to background to enable change of fcode
    /scwm/cl_rf_bll_srvc=>set_prmod(
                         /scwm/cl_rf_bll_srvc=>c_prmod_background ).
*   Set fcode
    /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_low_stck ).
*   Set return fcode
    ordim_confirm-return_fcode = gc_fcode_ret_pimtto.
    MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
  ELSE.
****   Check serial number handling
***    CALL FUNCTION '/SCWM/RF_SN_CHECK'
***      EXPORTING
***        iv_lgnum = ordim_confirm-lgnum
***        iv_matid = ordim_confirm-matid
***      IMPORTING
***        ev_stock = lv_stock.
***    IF ( lv_stock = wmegc_serial_bin ).   "Serial number required
****     Serial number collection NOT for whole HU movement.
***
****     Check if we have already collected the serial numbers
***      DESCRIBE TABLE ct_sernr LINES lv_colsn.
***      lv_qty_numc = ordim_confirm-nista_verif.
***
***      IF lv_colsn <> lv_qty_numc.
***        clear cs_sn.
****       Set process mode to background to enable change of fcode
***        /scwm/cl_rf_bll_srvc=>set_prmod(
***                          /scwm/cl_rf_bll_srvc=>c_prmod_background ).
****       Set fcode
***        /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_serial ).
****       Set last step
***        lv_step = /scwm/cl_rf_bll_srvc=>get_step( ).
***        ordim_confirm-last_step = lv_step.
***        ordim_confirm-sn_call = 'X'.
***        EXIT.
***      ENDIF.
***    ENDIF.

    DATA:  lv_ser_err TYPE xfeld.

    BREAK-POINT ID /scwm/suom.
*     If we have set DIFTY but difference quantity is initial
*       -> we delete the difference exception
    IF ordim_confirm-difty IS NOT INITIAL AND
       ordim_confirm-ndifa IS INITIAL.
      READ TABLE ordim_confirm-exc_tab TRANSPORTING NO FIELDS
        WITH KEY iprcode = wmegc_iprcode_diff.
      IF sy-subrc = 0.
        DELETE ordim_confirm-exc_tab INDEX sy-tabix.
        CLEAR ordim_confirm-difty.

        MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
          TRANSPORTING exc_tab difty.
      ENDIF.
    ENDIF.

    PERFORM picker_driven_rpl_chk
      USING
         ordim_confirm
         tt_ordim_confirm
      CHANGING
         lv_picker_drv_repl
    .
    IF lv_picker_drv_repl = 'X'.
*    The replenishment task should be confirmed
*    to the resource by picker driven replenishment.
      CLEAR: ordim_confirm-nlenr,ordim_confirm-pickhu.
      MODIFY tt_ordim_confirm FROM ordim_confirm INDEX 1.
    ENDIF.
* After data is checked call confirmation function.
    CALL FUNCTION '/SCWM/RF_PICK_ORDIM_CONFIRM'
      IMPORTING
        ev_restart_transaction = lv_restart_transaction
        ev_ser_err             = lv_ser_err
      CHANGING
        resource               = resource
        tt_ordim_confirm       = tt_ordim_confirm
        ordim_confirm          = ordim_confirm
        tt_nested_hu           = tt_nested_hu
        who                    = who
        ct_sernr               = ct_sernr
        ct_sernr_diff          = ct_sernr_diff
        ct_sernr_lsck          = ct_sernr_lsck
        t_rf_pick_hus          = t_rf_pick_hus.

*   If TO_CONFIRM says we need serial numbers -> we call the SN screen
    IF lv_ser_err IS NOT INITIAL.
*       Set process mode to background to enable change of fcode
      /scwm/cl_rf_bll_srvc=>set_prmod(
                        /scwm/cl_rf_bll_srvc=>c_prmod_background ).
*       Set fcode
      /scwm/cl_rf_bll_srvc=>set_fcode( fcode_go_to_serial ).
*       Set last step
      lv_step = /scwm/cl_rf_bll_srvc=>get_step( ).
      ordim_confirm-last_step = lv_step.
      ordim_confirm-sn_call = 'X'.
      ordim_confirm-return_fcode = gc_fcode_ret_pimtto.
      RETURN.
    ENDIF.

* If the posting was okay we check the navigation
    CALL FUNCTION '/SCWM/RF_PICK_NAVIGATION'
      EXPORTING
        iv_restart_transaction = lv_restart_transaction
      CHANGING
        tt_ordim_confirm       = tt_ordim_confirm
        ordim_confirm          = ordim_confirm
        resource               = resource
        selection              = selection
        who                    = who
        wme_verif              = wme_verif.
  ENDIF.



ENDFUNCTION.
