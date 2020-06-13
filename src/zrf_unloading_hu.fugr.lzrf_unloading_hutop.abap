FUNCTION-POOL zrf_unloading_hu.             "MESSAGE-ID ..
TYPE-POOLS: wmegc, wmesr.

*TABLES: /scwm/s_rf_unlo.

* Global Variable
DATA: gv_door_dd_fut  TYPE /scwm/de_rf_asgn_future_days,
      gv_door_dd_past TYPE /scwm/de_rf_asgn_past_days,
      gv_tunum_int    TYPE /scwm/de_tu_num.
DATA: gv_started_at TYPE /scwm/de_started_dt.

* Handlles delivery lock conflict by more users
* The variable contains the username, who actually
* locks the delivery data
DATA  gv_dlvlockusr   TYPE sy-uname.
DATA  gv_pmatid       TYPE /scwm/de_rf_pmatid.

* Wait seconds for TO confirmation, if error occured
CONSTANTS:
  gc_wait_seconds TYPE f VALUE '4'.

* Time to Timestamp conversion
CONSTANTS:
  gc_time_low   TYPE uzeit VALUE '000000',
  gc_time_high  TYPE uzeit VALUE '235959',
  gc_day_in_sec TYPE i     VALUE '86400'.

* Max how many deliveries can updated synchronously
CONSTANTS:
  gc_sync_dlv   TYPE i VALUE 10.

* Parameters (Structures)
CONSTANTS:
  gc_param_cs_unlo      TYPE /scwm/de_param_name VALUE 'CS_UNLO',
  gc_param_cs_unlo_prod TYPE /scwm/de_param_name VALUE 'CS_UNLO_PROD',
  gc_param_ct_unlo      TYPE /scwm/de_param_name VALUE 'CT_UNLO'.

* FCodes for Unloading
CONSTANTS:
  gc_fcode_simple  TYPE /scwm/de_fcode VALUE 'SYSMPL',
  gc_fcode_complex TYPE /scwm/de_fcode VALUE 'SYCMPL',
  gc_fcode_crhu    TYPE /scwm/de_fcode VALUE 'GTNWHU',
  gc_fcode_ende    TYPE /scwm/de_fcode VALUE 'SYENDE',
  gc_fcode_goon    TYPE /scwm/de_fcode VALUE 'SYGOON',
  gc_fcode_yes     TYPE /scwm/de_fcode VALUE 'YES   ',
  gc_fcode_dlvsel  TYPE /scwm/de_fcode VALUE 'DLVSEL',
  gc_fcode_enter   TYPE /scwm/de_fcode VALUE 'ENTER',
  gc_fcode_cmpltx  TYPE /scwm/de_fcode VALUE 'CMPTRS', "leave TX
  gc_fcode_backf   TYPE /scwm/de_fcode VALUE 'BACKF'.

* FCodes for Exceptions
CONSTANTS:
  gc_fcode_syexc_cb TYPE /scwm/de_fcode VALUE 'SYEXC1', "Change Bin
  gc_fcode_syexc_mo TYPE /scwm/de_fcode VALUE 'SYEXC2', "More
  gc_fcode_syexc_le TYPE /scwm/de_fcode VALUE 'SYEXC3', "Less
  gc_fcode_syexc_cr TYPE /scwm/de_fcode VALUE 'SYEXC4'. "Less

* Transactions
CONSTANTS:
  gc_ltrans_uldosi TYPE /scwm/de_ltrans VALUE 'ULDOSI',
  gc_ltrans_ulsysg TYPE /scwm/de_ltrans VALUE 'ULSYSG'.

* Processing Mode
CONSTANTS:
  gc_prmod_fgrnd       TYPE /scwm/de_prmod VALUE
                             /scwm/cl_rf_bll_srvc=>c_prmod_foreground.

* States for Unloading
CONSTANTS:
  gc_state_ulst01 TYPE /scwm/de_state VALUE 'ULST01',  " BACK
  gc_state_ulst02 TYPE /scwm/de_state VALUE 'ULST02'.  " NO

* Change loading info of Hu to unload
CONSTANTS:
  gc_hu_laod_info_unlo TYPE char1 VALUE 'A'.

* Locking
CONSTANTS:
  gc_lock TYPE xfeld VALUE 'X'.

* Status
CONSTANTS:
  gc_dl_begin_unload             TYPE string
    VALUE /scwm/if_ui_shp_const=>sc_act_status_unload_start,
  gc_dl_compl_unload             TYPE string
    VALUE /scwm/if_ui_shp_const=>sc_act_status_unload_end,
  gc_dl_status_type_dun          TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_unloading,
  gc_dl_status_value_dlun        TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_finished,
  gc_dl_status_value_partly      TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed,
  gc_dl_status_value_finished    TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_finished,
  gc_dl_status_value_not_started TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_not_started,
  gc_dl_status_value_unblocked   TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_false,
  gc_dl_status_type_dsp          TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_split,
  gc_dl_status_value_dsp_partly  TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed,

  gc_dl_stat_val_ncts_not_rlvnt  TYPE /scdl/dl_status_value
    VALUE /scwm/if_dl_status_c=>sc_v_ncts_not_relevant,
  gc_dl_stat_val_ncts_released   TYPE /scdl/dl_status_value
    VALUE /scwm/if_dl_status_c=>sc_v_ncts_released,

* status type
  gc_dl_status_type_ncts         TYPE /scdl/dl_status_type
    VALUE /scwm/if_dl_status_c=>sc_t_ncts.

* Delivery logical field names
CONSTANTS:
  gc_dl_logfname_docno           TYPE /scdl/dl_logfname
      VALUE /scdl/if_dl_logfname_c=>sc_docno_h,
  gc_dl_logfname_docid           TYPE /scdl/dl_logfname
      VALUE /scdl/if_dl_logfname_c=>sc_docid_h,
  gc_dl_logfname_productid_i     TYPE /scdl/dl_logfname
       VALUE /scdl/if_dl_logfname_c=>sc_productid_i,
  gc_dl_logfname_whno_i          TYPE /scdl/dl_logfname
     VALUE /scwm/if_dl_logfname_c=>sc_whno_i,
  gc_dl_logfname_door_i          TYPE /scdl/dl_logfname
     VALUE /scwm/if_dl_logfname_c=>sc_door_i,
  gc_dl_logfname_refdocno_bol_h  TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_refdocno_bol_h,
  gc_dl_logfname_refdocno_asn_h  TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_refdocno_asn_h,
  gc_dl_logfname_value_dun_i     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dun_i,
  gc_dl_logfname_value_dbc_h     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dbc_h,
  gc_dl_logfname_value_dlvpldat  TYPE /scdl/dl_logfname
    VALUE /scdl/if_dl_logfname_c=>sc_tstfr_tdelivery_plan_h,
  gc_dl_logfname_value_dlvactdat TYPE /scdl/dl_logfname
   VALUE /scdl/if_dl_logfname_c=>sc_tstfr_tdelivery_actual_h,
  gc_dl_logfname_status_ncts     TYPE /scdl/dl_logfname
       VALUE /scwm/if_dl_logfname_c=>sc_status_type_dwn_h,
  gc_dl_logfname_value_dsp_i     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dsp_i,

* FieldNames in Structure
  gc_scr_elmnt_door_dd_past      TYPE /scwm/de_screlm_name VALUE
             '/SCWM/S_RF_UNLO-DOOR_DD_PAST',
  gc_scr_elmnt_door_dd_fut       TYPE /scwm/de_screlm_name VALUE
             '/SCWM/S_RF_UNLO-DOOR_DD_FUT',
  gc_scr_elmnt_pmatid            TYPE /scwm/de_screlm_name VALUE
            '/SCWM/S_RF_UNLO-PMATID',
  gc_scr_elmnt_charg             TYPE /scwm/de_screlm_name VALUE
            '/SCWM/S_RF_UNLO_PROD-CHARG',
  gc_scr_elmnt_bbdat             TYPE /scwm/de_screlm_name VALUE
            '/SCWM/S_RF_UNLO_PROD-BBDAT',
  gc_shortcut                    TYPE /scwm/de_screlm_name VALUE
            '/SCWM/S_RF_SCRELM-SHORTCUT',
  gc_scr_elmnt_nista(60)         TYPE c VALUE
            '/SCWM/S_RF_UNLO_PROD-NISTA',


  gc_dl_date_type                TYPE /scdl/dl_tsttype
        VALUE /scdl/if_dl_date_c=>sc_tsttype_sunld,
  gc_dl_date_cat_actual          TYPE /scdl/dl_tst_category
        VALUE /scdl/if_dl_date_c=>sc_tstcat_actual,

  gc_prmod_foreground            TYPE /scwm/de_prmod VALUE '2',
  gc_prmod_background            TYPE /scwm/de_prmod VALUE '1',

  gc_status_type_dbd             TYPE /scdl/dl_status_type VALUE 'DBD',
  gc_doctype_inb                 TYPE /scdl/dl_doctype VALUE 'INB'
  .

* Field check
CONSTANTS:   gc_numcs      TYPE string VALUE '0123456789.,'.

DATA: gv_qty   TYPE int4,
      gv_lotno TYPE /sttpec/e_lotno,
      gv_matnr TYPE /sttpec/e_matnr,
      gv_datex TYPE /sttpec/e_datex.


*Includes
INCLUDE /scwm/lrf_unloadingf0q.
INCLUDE /scwm/lrf_unloadingf0o.
INCLUDE /scwm/lrf_unloadingf0n.
INCLUDE /scwm/lrf_unloadingf0l.
INCLUDE /scwm/lrf_unloadingf0f.
INCLUDE /scwm/lrf_unloadingf05.
INCLUDE /scwm/lrf_unloadingf06.
INCLUDE /scwm/lrf_unloadingf0d.
INCLUDE /scwm/lrf_unloadingf0p.
INCLUDE /scwm/lrf_unloadingf08.

*** screen 8450
INCLUDE /scwm/lrf_unloadingf16.
INCLUDE /scwm/lrf_unloadingf15.
INCLUDE /scwm/lrf_unloadingf19.
INCLUDE /scwm/lrf_unloadingf18.
INCLUDE /scwm/lrf_unloadingf17.


INCLUDE /scwm/irf_sscr.
