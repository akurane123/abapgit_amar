FUNCTION-POOL /scwm/rf_picking MESSAGE-ID /scwm/rf_en.


TYPE-POOLS: wmegc.

* Re-Used CONSTANTS:
*   fcode_enter=>use        /scwm/cl_rf_bll_srvc=>C_FCODE_ENTER
*   fcode_clear=>use        /scwm/cl_rf_bll_srvc=>C_FCODE_CLEAR
*   fcode_back=>use         /scwm/cl_rf_bll_srvc=>C_FCODE_BACK
*   fcode_updbck=>use       /scwm/cl_rf_bll_srvc=>C_FCODE_UPDATE_BACK
*   fcode_unknow=>use       /scwm/cl_rf_bll_srvc=>C_FCODE_UNKNOWN
*   fcode_PGUP=>use         /scwm/cl_rf_bll_srvc=>C_FCODE_PREV_PG
*   fcode_PGDN=>use         /scwm/cl_rf_bll_srvc=>C_FCODE_NEXT_PG
*   C_PRMOD_BACKGROUND=>use /scwm/cl_rf_bll_srvc=>C_PRMOD_BACKGROUND '1'
*   C_PRMOD_FOREGROUND=>use /scwm/cl_rf_bll_srvc=>C_PRMOD_FOREGROUND '2'
*   fcode_compl_trans=>use  /scwm/cl_rf_bll_srvc=>c_fcode_compl_ltrans


"The interface /SCWM/IF_RF_PICKING_CONSTANTS is the new home for constants of the RF picking.
"This allows the usage of the constants in new global classes that are not part of this function group.
"Define new constants directly in there.
"If you need one of the following constants outside this function group define it in the interface
"and initialize the local constant with the constant from the interface as it is already done for some of the constants.

CONSTANTS gc_scr_elmnt_vlpla_vrf  TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_vlpla_vrf.
CONSTANTS gc_scr_elmnt_nlpla_vrf  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-NLPLA_VERIF'.
CONSTANTS gc_scr_elmnt_vlenr_vrf   TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_vlenr_vrf.
CONSTANTS gc_scr_elmnt_vlenr       TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-VLENR'.
CONSTANTS gc_scr_elmnt_matid_vrf   TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_matid_vrf.
CONSTANTS gc_scr_elmnt_matnr       TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-MATNR'.
CONSTANTS gc_scr_elmnt_nista_vrf   TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_nista_vrf.
CONSTANTS gc_scr_elmnt_nista_chr   TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_NESTED_HU-NISTA_CHR'.
CONSTANTS gc_scr_elmnt_huent   TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_NESTED_HU-HUENT'.
CONSTANTS gc_scr_elmnt_huent2  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-HUENT'.
CONSTANTS gc_scr_elmnt_huident_verif   TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_NESTED_HU-HUIDENT_VERIF'.
CONSTANTS gc_scr_elmnt_batch_vrf TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_batch_vrf.
CONSTANTS gc_scr_elmnt_batch     TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-BATCH'.
CONSTANTS gc_scr_elmnt_rfbatch   TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-RFBATCH'.
CONSTANTS gc_scr_elmnt_pickhu_vrf  TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_pickhu_vrf.
CONSTANTS gc_scr_elmnt_hupos_vrf   TYPE /scwm/de_screlm_name VALUE
          /scwm/if_rf_picking_constants=>sc_scr_elmnt_hupos_vrf.
CONSTANTS gc_scr_elmnt_hupos   TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-HUPOS'.
CONSTANTS gc_scr_elmnt_kquan_chr   TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-KQUAN_CHR'.
CONSTANTS gc_scr_elmnt_kquan_verif TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-KQUAN_VERIF'.
CONSTANTS gc_scr_elmnt_phu_logpos  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_PICK_HUS-LOGPOS'.
CONSTANTS gc_scr_elmnt_logpos_ext  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-LOGPOS_EXT'.
CONSTANTS gc_scr_elmnt_dlogpos_ext_wt  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-DLOGPOS_EXT_WT'.
CONSTANTS gc_scr_elmnt_dlgpos_ext_wt_vrf  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-DLOGPOS_EXT_WT_VERIF'.
CONSTANTS gc_huident TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_PICK_HUS-HUIDENT'.
CONSTANTS gc_pmat TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_PICK_HUS-PMAT'.
CONSTANTS gc_huident_verif TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_PICK_HUS-HUIDENT_VERIF'.
CONSTANTS gc_logpos TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_PICK_HUS-LOGPOS'.
CONSTANTS gc_logpos_text TYPE /scwm/de_screlm_name VALUE
          'LOGPOS_TEXT'.
CONSTANTS gc_filled_pmat TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_NESTED-PMAT'.
CONSTANTS gc_filled_rfhu TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_NESTED-RFHU'.
CONSTANTS gc_filled_logpos TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_NESTED-LOGPOS'.
CONSTANTS gc_scr_elmnt_ndifa_vrf  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-NDIFA_VERIF'.
CONSTANTS gc_scr_elmnt_ndifa  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-NDIFA'.
CONSTANTS gc_scr_elmnt_vsola_chr TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-VSOLA_CHR'.
CONSTANTS gc_scr_elmnt_nlpla  TYPE /scwm/de_screlm_name VALUE
          '/SCWM/S_RF_ORDIM_CONFIRM-NLPLA'.

CONSTANTS iprcode_splt          TYPE /scwm/de_iprcode VALUE 'SPLT'.
CONSTANTS iprcode_repl          TYPE /scwm/de_iprcode VALUE 'REPL'.
CONSTANTS iprcode_diff          TYPE /scwm/de_iprcode VALUE 'DIFF'.
CONSTANTS iprcode_humi          TYPE /scwm/de_iprcode VALUE 'HUMI'.
CONSTANTS iprcode_next          TYPE /scwm/de_iprcode VALUE 'NEXT'.
CONSTANTS iprcode_nxsp          TYPE /scwm/de_iprcode VALUE 'NXSP'.
CONSTANTS iprcode_bidf          TYPE /scwm/de_iprcode VALUE 'BIDF'.
CONSTANTS iprcode_bidp          TYPE /scwm/de_iprcode VALUE 'BIDP'.
CONSTANTS iprcode_bidu          TYPE /scwm/de_iprcode VALUE 'BIDU'.
CONSTANTS iprcode_bfrp          TYPE /scwm/de_iprcode VALUE 'BFRP'.
CONSTANTS iprcode_bprp          TYPE /scwm/de_iprcode VALUE 'BPRP'.
CONSTANTS iprcode_invb          TYPE /scwm/de_iprcode VALUE 'INVB'.
CONSTANTS iprcode_invm          TYPE /scwm/de_iprcode VALUE 'INVM'.
CONSTANTS iprcode_chbn          TYPE /scwm/de_iprcode VALUE 'CHBD'.
CONSTANTS iprcode_chhu          TYPE /scwm/de_iprcode VALUE 'CHHU'.
CONSTANTS iprcode_chba          TYPE /scwm/de_iprcode VALUE 'CHBA'.
CONSTANTS iprcode_list          TYPE /scwm/de_iprcode VALUE 'LIST'.
CONSTANTS iprcode_lstc          TYPE /scwm/de_iprcode VALUE 'LSTC'.
CONSTANTS fcode_tosel           TYPE /scwm/de_fcode  VALUE 'TOSEL'.
CONSTANTS fcode_skipte          TYPE /scwm/de_fcode  VALUE 'SKIPTE'.
CONSTANTS fcode_rvseq           TYPE /scwm/de_fcode  VALUE 'RVSEQ'.
CONSTANTS fcode_invb            TYPE /scwm/de_fcode  VALUE 'INVB'.
CONSTANTS fcode_invm            TYPE /scwm/de_fcode  VALUE 'INVM'.
CONSTANTS fcode_next            TYPE /scwm/de_fcode  VALUE 'NEXT'.
CONSTANTS fcode_chng            TYPE /scwm/de_fcode  VALUE 'CHNG'.
CONSTANTS fcode_save            TYPE /scwm/de_fcode  VALUE 'SAVE'.
CONSTANTS fcode_detail          TYPE /scwm/de_fcode  VALUE 'DETAIL'.
CONSTANTS fcode_print           TYPE /scwm/de_fcode  VALUE 'PRINT'.
CONSTANTS fcode_exception       TYPE /scwm/de_fcode  VALUE 'EXCEPT'.
CONSTANTS fcode_diff            TYPE /scwm/de_fcode  VALUE 'DIFF'.
CONSTANTS fcode_diffhu          TYPE /scwm/de_fcode  VALUE 'DIFFHU'.
CONSTANTS fcode_init            TYPE /scwm/de_fcode  VALUE 'INIT'.
CONSTANTS fcode_diffbd          TYPE /scwm/de_fcode  VALUE 'DIFFBD'.
CONSTANTS fcode_repln           TYPE /scwm/de_fcode  VALUE 'REPLN'.
CONSTANTS fcode_hucr            TYPE /scwm/de_fcode  VALUE 'HUCR'.
CONSTANTS fcode_split           TYPE /scwm/de_fcode  VALUE 'SPLIT'.
CONSTANTS fcode_huent           TYPE /scwm/de_fcode  VALUE 'HUENT'.
CONSTANTS fcode_backf           TYPE /scwm/de_fcode  VALUE 'BACKF'.
CONSTANTS fcode_enterf          TYPE /scwm/de_fcode  VALUE 'ENTERF'.
CONSTANTS fcode_delhu           TYPE /scwm/de_fcode  VALUE 'DELHU'.
CONSTANTS fcode_emptyb          TYPE /scwm/de_fcode  VALUE 'EMPTYB'.
CONSTANTS fcode_scan_hu         TYPE /scwm/de_fcode  VALUE 'GTMULT'.
CONSTANTS fcode_rmnqty          TYPE /scwm/de_fcode  VALUE 'RMNQTY'.
CONSTANTS fcode_reject          TYPE /scwm/de_fcode  VALUE 'REJECT'.
CONSTANTS fcode_nesthu          TYPE /scwm/de_fcode  VALUE 'NESTHU'.
CONSTANTS fcode_nodhu           TYPE /scwm/de_fcode  VALUE 'NODHU'.
CONSTANTS fcode_vrflop          TYPE /scwm/de_fcode  VALUE 'VRFLOP'.
CONSTANTS fcode_vrfphu          TYPE /scwm/de_fcode  VALUE 'VRFPHU'.
CONSTANTS fcode_autosn          TYPE /scwm/de_fcode  VALUE 'AUTOSN'.
CONSTANTS fcode_list            TYPE /scwm/de_fcode  VALUE 'LIST'.
CONSTANTS gc_fcode_ret_pimtto   TYPE /scwm/de_fcode  VALUE 'RTMTTO'.
CONSTANTS gc_fcode_ret_picpmt   TYPE /scwm/de_fcode  VALUE 'RTCPMT'.
CONSTANTS gc_fcode_ret_piblmt   TYPE /scwm/de_fcode  VALUE 'RTBLMT'.
CONSTANTS gc_fcode_ret_piblcp   TYPE /scwm/de_fcode  VALUE 'RTBLCP'.
CONSTANTS gc_fcode_ret_diff     TYPE /scwm/de_fcode  VALUE 'RTDIFF'.
CONSTANTS gc_fcode_ret_piblhu   TYPE /scwm/de_fcode  VALUE 'RTBLHU'.
CONSTANTS gc_fcode_ret_pihuto   TYPE /scwm/de_fcode  VALUE 'RTHUTO'.
CONSTANTS gc_fcode_pistck       TYPE /scwm/de_fcode  VALUE 'RTSTCK'.
CONSTANTS gc_fcode_callcw TYPE /scwm/de_fcode VALUE 'FCCWCA'. "Call CW function
CONSTANTS gc_fcode_fcmed TYPE /scwm/de_fcode VALUE 'FCCWME'. "Mediator function for CW
CONSTANTS fcode_go_to_pick_mtto TYPE /scwm/de_fcode  VALUE 'GTMTTO'.
CONSTANTS fcode_go_to_pick_huto TYPE /scwm/de_fcode  VALUE 'GTHUTO'.
CONSTANTS fcode_go_to_pick_blmt TYPE /scwm/de_fcode  VALUE 'GTBLMT'.
CONSTANTS fcode_go_to_pick_blhu TYPE /scwm/de_fcode  VALUE 'GTBLHU'.
CONSTANTS fcode_go_to_pick_blcp TYPE /scwm/de_fcode  VALUE 'GTBLCP'.
CONSTANTS fcode_go_to_pick_cpmt TYPE /scwm/de_fcode  VALUE 'GTCPMT'.
CONSTANTS fcode_go_to_cmb_dtl   TYPE /scwm/de_fcode  VALUE 'GTCDMT'.
CONSTANTS fcode_go_to_place_mat TYPE /scwm/de_fcode  VALUE 'GTPLMT'.
CONSTANTS fcode_go_to_place_hu  TYPE /scwm/de_fcode  VALUE 'GTPLHU'.
CONSTANTS fcode_go_to_hu_int    TYPE /scwm/de_fcode  VALUE 'GTIHU'.
CONSTANTS fcode_go_to_hu_skip   TYPE /scwm/de_fcode  VALUE 'GTIHUS'.
CONSTANTS fcode_go_to_low_stck  TYPE /scwm/de_fcode  VALUE 'GTSTCK'.
CONSTANTS fcode_go_to_serial    TYPE /scwm/de_fcode  VALUE 'GTSNCL'.
CONSTANTS fcode_go_to_chng_dbin TYPE /scwm/de_fcode  VALUE 'GTCHBN'.
CONSTANTS fcode_go_to_bin_den   TYPE /scwm/de_fcode  VALUE 'GTCHBD'.
CONSTANTS fcode_go_to_chng_dhu  TYPE /scwm/de_fcode  VALUE 'GTCHHU'.
CONSTANTS fcode_go_to_chng_logpos_wt  TYPE /scwm/de_fcode  VALUE 'GTCHLP'.
CONSTANTS fcode_go_to_chng_btch TYPE /scwm/de_fcode  VALUE 'GTCHBA'.
CONSTANTS fcode_skip_slct_scr   TYPE /scwm/de_fcode  VALUE 'SKPSCR'.
CONSTANTS fcode_queue_inp_again TYPE /scwm/de_fcode  VALUE 'RESTRT'.
CONSTANTS ltrans_pick_by_hu     TYPE /scwm/de_ltrans VALUE 'PIBHU'.
CONSTANTS ltrans_pick_by_queue  TYPE /scwm/de_ltrans VALUE 'PIBQUE'.
CONSTANTS ltrans_pick_by_whr    TYPE /scwm/de_ltrans VALUE 'PIBWHR'.
CONSTANTS ltrans_pick_by_who    TYPE /scwm/de_ltrans VALUE 'PIBWHO'.
CONSTANTS ltrans_pick_by_system TYPE /scwm/de_ltrans VALUE 'PISYSG'.
CONSTANTS ltrans_work_by_system TYPE /scwm/de_ltrans VALUE 'WKSYSG'.
CONSTANTS ltrans_work_by_queue  TYPE /scwm/de_ltrans VALUE 'WKSYSQ'.
CONSTANTS ltrans_work_by_wo     TYPE /scwm/de_ltrans VALUE 'WKMNWO'.
CONSTANTS ltrans_work_by_hu     TYPE /scwm/de_ltrans VALUE 'WKMNHU'.
CONSTANTS ltrans_work_by_wr     TYPE /scwm/de_ltrans VALUE 'WKMNWR'.
CONSTANTS step_source_mtto      TYPE /scwm/de_step   VALUE 'PIMTTO'.
CONSTANTS step_source_huto      TYPE /scwm/de_step   VALUE 'PIHUTO'.
CONSTANTS step_source_blmt      TYPE /scwm/de_step   VALUE 'PIBLMT'.
CONSTANTS step_source_blcp      TYPE /scwm/de_step   VALUE 'PIBLCP'.
CONSTANTS step_source_blhu      TYPE /scwm/de_step   VALUE 'PIBLHU'.
CONSTANTS step_dest_plhu        TYPE /scwm/de_step   VALUE 'PIPLHU'.
CONSTANTS step_dest_plmt        TYPE /scwm/de_step   VALUE 'PIPLMT'.
CONSTANTS step_dest_mphu        TYPE /scwm/de_step   VALUE 'PIMPHU'.
CONSTANTS step_pick_by_system   TYPE /scwm/de_step   VALUE 'PIBSYS'.
CONSTANTS step_pick_by_user     TYPE /scwm/de_step   VALUE 'PIBUSR'.
CONSTANTS step_pick_huin        TYPE /scwm/de_step   VALUE 'PIHUIN'.
CONSTANTS step_pick_pilist      TYPE /scwm/de_step   VALUE 'PILIST'.
CONSTANTS step_pick_recovery    TYPE /scwm/de_step   VALUE 'PIRCVR'.
CONSTANTS step_pick_nesthu      TYPE /scwm/de_step   VALUE 'PINEHU'.
CONSTANTS step_pick_pichhu      TYPE /scwm/de_step   VALUE 'PICHHU'.
CONSTANTS step_pick_chbd        TYPE /scwm/de_step   VALUE 'PICHBD'.
CONSTANTS step_pick_cpmt        TYPE /SCWM/de_step   VALUE 'PICPMT'.
CONSTANTS step_pbv_cpmt        TYPE /SCWM/de_step   VALUE 'PVCPMT'.
CONSTANTS step_pbv_blcp        TYPE /SCWM/de_step   VALUE 'PVBLCP'.
CONSTANTS step_pick_cbdt        TYPE /scwm/de_step   VALUE 'PICDMT'.
CONSTANTS step_pick_diff        TYPE /scwm/de_step   VALUE 'DIFF'.
CONSTANTS step_pick_diff_split  TYPE /scwm/de_step   VALUE 'DIFFSP'.
CONSTANTS step_pick_pidiff        TYPE /scwm/de_step   VALUE 'PIDIFF'.
CONSTANTS step_pick_pidiff_split  TYPE /scwm/de_step   VALUE 'PIDISP'.
CONSTANTS step_pick_pistck      TYPE /scwm/de_step   VALUE 'PISTCK'.
CONSTANTS step_source_pvmtto    TYPE /scwm/de_step   VALUE 'PVMTTO'.
CONSTANTS step_source_pvblmt    TYPE /scwm/de_step   VALUE 'PVBLMT'.
CONSTANTS gc_xfeld              TYPE xfeld           VALUE 'X'.
CONSTANTS gc_huto               TYPE /scwm/de_state  VALUE 'HUTO'.
CONSTANTS gc_bulk_huto          TYPE /scwm/de_state  VALUE 'BLHU'.
CONSTANTS gc_mtto               TYPE /scwm/de_state  VALUE 'MTTO'.
CONSTANTS gc_dd_mtto            TYPE /scwm/de_state  VALUE 'DD'.
CONSTANTS gc_bulk_mtto          TYPE /scwm/de_state  VALUE 'BLMT'.
CONSTANTS gc_plhu               TYPE /scwm/de_state  VALUE 'PLHU'.
CONSTANTS gc_plmt               TYPE /scwm/de_state  VALUE 'PLMT'.
CONSTANTS gc_excpt_type_all     TYPE char4           VALUE 'ALL'.
CONSTANTS gc_excpt_type_diff    TYPE char4           VALUE 'DIFF'.
CONSTANTS gc_difty_inbound      TYPE /scwm/de_difty  VALUE '3'.
CONSTANTS gc_difty_account      TYPE /scwm/de_difty  VALUE '2'.
CONSTANTS gc_difty_bin          TYPE /scwm/de_difty  VALUE '1'.
CONSTANTS gc_sort_ascending     TYPE /scwm/de_rf_pick_sort VALUE 'A'.
CONSTANTS gc_sort_descending    TYPE /scwm/de_rf_pick_sort VALUE 'D'.
CONSTANTS gc_picking_msgid      TYPE symsgid     VALUE '/SCWM/RF_EN'.
CONSTANTS gc_msgty_error        TYPE symsgty             VALUE 'E'.
CONSTANTS gc_msgty_warning      TYPE symsgty             VALUE 'W'.
CONSTANTS gc_msgty_query        TYPE symsgty             VALUE 'Q'.
CONSTANTS gc_msg_view_scr       TYPE /scwm/de_msg_view   VALUE '0'.
CONSTANTS gc_msg_view_line      TYPE /scwm/de_msg_view   VALUE '1'.
CONSTANTS gc_lowchk_inv_perform TYPE /scwm/de_lowchk_inv VALUE '1'.
CONSTANTS gc_place_inv_perform  TYPE /scwm/de_place_inv  VALUE '1'.
CONSTANTS gc_lowchk_inv_compl   TYPE /scwm/de_lowchk_inv VALUE '3'.
CONSTANTS gc_place_inv_compl    TYPE /scwm/de_place_inv  VALUE '3'.
CONSTANTS gc_lowchk_inv_cancel  TYPE /scwm/de_lowchk_inv VALUE '0'.
CONSTANTS gc_place_inv_cancel   TYPE /scwm/de_place_inv  VALUE '0'.
CONSTANTS gc_lowchk_inv_reject  TYPE /scwm/de_lowchk_inv VALUE '4'.
CONSTANTS gc_non_postn_mng      TYPE /scwm/de_postn VALUE space.
CONSTANTS gc_manual_postn_mng   TYPE /scwm/de_postn VALUE '1'.
CONSTANTS gc_auto_postn_mng     TYPE /scwm/de_postn VALUE '2'.
CONSTANTS gc_buscon_pi2         TYPE /scwm/tbusid-buscon VALUE 'PI2'.
CONSTANTS gc_buscon_pi3         TYPE /scwm/tbusid-buscon VALUE 'PI3'.
CONSTANTS gc_buscon_pi4         TYPE /scwm/tbusid-buscon VALUE 'PI4'.
CONSTANTS gc_buscon_pi5         TYPE /scwm/tbusid-buscon VALUE 'PI5'.
CONSTANTS gc_fn_more TYPE ddobjname VALUE '/SCWM/DE_RF_MORE'.

CONSTANTS gc_huident_length     TYPE i VALUE 20.

CONSTANTS gc_rf_msgid_de        TYPE symsgid     VALUE '/SCWM/RF_DE'.
CONSTANTS gc_rf_msgid_en        TYPE symsgid     VALUE '/SCWM/RF_EN'.

CONSTANTS: gc_prmod_foregr TYPE /scwm/de_prmod VALUE '2',
           gc_prmod_backgr TYPE /scwm/de_prmod VALUE '1'.

CONSTANTS: gc_valid_obj_level TYPE /scwm/de_valid_obj VALUE 'LVL_V'.

* Tables and structures for screens defined in RF-Framework
* For examples: parameters catalog table /SCWM/TPARAM_CAT

* Global tables
*TABLES:
*        /scwm/s_rf_nested_hu,
*        /scwm/s_rf_ordim_confirm,
*        /scwm/s_rf_selection,
*        /scwm/rsrc,
*        /scwm/s_wo_det_out,
*        /scwm/s_agg_tos,
*        /scwm/s_who_int,
*        /scwm/s_rf_middle_hu,
*        /scwm/s_rf_pick_hus,
*        /scwm/s_rf_nested,
*        /scwm/s_rf_sn.

* Globale variables:
* DATA gt_rf_pick_hus TYPE /scwm/tt_rf_pick_hus.
DATA: gv_pick_sort           TYPE /scwm/de_rf_pick_sort,
      gv_who                 TYPE /scwm/de_who,
      gv_who_man_assign      TYPE /scwm/de_manassign,
      gv_postn_mngmnt        TYPE /scwm/de_postn,
      gv_recovery_on         TYPE xfeld VALUE IS INITIAL,
      gv_nested_save         TYPE xfeld VALUE IS INITIAL,
      gv_diff_save           TYPE xfeld VALUE IS INITIAL,
      gv_stock_save          TYPE xfeld VALUE IS INITIAL,
      gv_huobl             TYPE /scwm/de_hu_obligatory VALUE IS INITIAL,
      gt_nested_hu           TYPE /scwm/tt_rf_nested_hu,
      gt_nested_hu_pbo       TYPE /scwm/tt_rf_nested_hu,
      gt_middle_hu           TYPE /scwm/tt_rf_middle_hu,
      gv_buscon              TYPE /scwm/tbusid-buscon,
      gv_exec_step           TYPE /scwm/de_exec_step,
      gv_nested              TYPE xfeld VALUE IS INITIAL,
      gv_nested_diff         TYPE xfeld VALUE IS INITIAL,
      gv_nested_exc          TYPE /scwm/de_exccode,
      gv_chng_nlenr          TYPE xfeld VALUE IS INITIAL,
      gv_hu_vrf_in_logpos_mode TYPE xfeld VALUE IS INITIAL,
      gv_dest_before_quit    TYPE xfeld VALUE IS INITIAL,
      gv_last_step           TYPE /scwm/de_step,
      gv_vsola               TYPE /scwm/ltap_vsola,
      gv_altme               TYPE /scwm/de_rf_altme,
      gv_nista               TYPE /scwm/ltap_nista,
      gv_ndifa               TYPE /scwm/ltap_ndifa,
      gv_nested_line         TYPE i.
DATA: gv_vlenr_verif TYPE /scwm/de_vlenr_verif.
DATA: gv_cw_called TYPE xfeld.
*     Variable handles the situtaion, when NO follow-up
*     WT created after the exception. The concerned exc.
*     BIDF - Full pick denial
*     BIDU - Stock removal denial
*     BIDP - Partial pick denial for the whole quantity
*     BFRP - Full Pick Denial + Replenishment
*     BPRP - Partial Pick Denial + Replenishment
*     DIFW(DIFTY=1)- Diff. as Charges for Warehouse quan 0
DATA: gv_no_fup_wt           TYPE xfeld,
      gv_manual_no_huwd      TYPE xfeld,
      gv_wt_index            TYPE i
      .
DATA: gv_lgpla_prev          TYPE /scwm/de_lgpla_prev,
      gv_lgtyp_prev          TYPE /scwm/de_lgtyp_prev.

** BADI

* Macro
*DEFINE check_bapiret.
*
*  loop at lt_bapiret into ls_bapiret
*       where type = 'E' or
*             type = 'A'.
*
*    message id ls_bapiret-id type ls_bapiret-type
*            number ls_bapiret-number
*              with ls_bapiret-message_v1 ls_bapiret-message_v2
*                   ls_bapiret-message_v3 ls_bapiret-message_v4.
*  endloop.
*
*END-OF-DEFINITION.


TYPES: BEGIN OF /scwm/s_pmat_guid,
         pmat_guid TYPE /scwm/de_matid,
       END OF /scwm/s_pmat_guid,
       /scwm/t_pmat_guid TYPE /scwm/s_pmat_guid OCCURS 0.





TYPES:
       BEGIN OF ts_to,
         who  TYPE /scwm/who-who,
         count_to TYPE /scwm/s_wo_det_out-count_to,
         sum_weight TYPE /scwm/de_weight_sum,
         unit_w TYPE gewei,
         sum_volume TYPE /scwm/de_volum_sum,
         unit_v TYPE voleh,
       END OF ts_to,

       tt_to TYPE TABLE OF ts_to.
