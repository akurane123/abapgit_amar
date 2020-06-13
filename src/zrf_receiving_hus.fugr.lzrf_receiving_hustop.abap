FUNCTION-POOL zrf_receiving_hus.            "MESSAGE-ID ..

* INCLUDE LZRF_RECEIVING_HUSD...             " Local class definition

TYPE-POOLS: wmegc, wmesr.

INCLUDE /scwm/lrf_receiving_husf03. " Read and Lock
INCLUDE /scwm/lrf_receiving_husf07. " Barcode Decoding
INCLUDE /scwm/lrf_receiving_husf14. " Fill Prod data from HU Item
INCLUDE /scwm/lrf_receiving_husf15. "
INCLUDE /scwm/lrf_receiving_husf24. "
INCLUDE /scwm/lrf_receiving_husf25. "
INCLUDE /scwm/lrf_receiving_husf34. " HU Number range check

* Global Variable
DATA: gv_door_dd_fut   TYPE /scwm/de_rf_asgn_future_days,
      gv_door_dd_past  TYPE /scwm/de_rf_asgn_past_days,
      gv_tunum_int     TYPE /scwm/de_tu_num,
      gv_tu_sr_act_num TYPE /scwm/de_tu_sr_act_num.

* Time to Timestamp conversion
CONSTANTS:
  gc_time_low   TYPE uzeit  VALUE '000000',
  gc_time_high  TYPE uzeit  VALUE '235959',
  gc_day_in_sec TYPE i      VALUE '86400',
  gc_numcs      TYPE string VALUE '0123456789.,'.


* Status
CONSTANTS:
  gc_dl_status_type_dun         TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_unloading,
  gc_dl_status_value_dlun       TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_finished,
  gc_dl_status_type_dpt         TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_putaway,
  gc_dl_status_value_dptw       TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_finished,
  gc_dl_status_value_unblocked  TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_false,
  gc_dl_compl_unload            TYPE string
    VALUE /scwm/if_ui_shp_const=>sc_act_status_unload_end,
  gc_dl_status_type_dsp         TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_split,
  gc_dl_status_value_dsp_partly TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed,

  gc_shortcut(60)               TYPE c VALUE '/SCWM/S_RF_SCRELM-SHORTCUT'.

* Delivery logical field names
CONSTANTS:
  gc_dl_logfname_docno           TYPE /scdl/dl_logfname
      VALUE /scdl/if_dl_logfname_c=>sc_docno_h,
  gc_dl_logfname_whno_i          TYPE /scdl/dl_logfname
     VALUE /scwm/if_dl_logfname_c=>sc_whno_i,
  gc_dl_logfname_docid           TYPE /scdl/dl_logfname
      VALUE /scdl/if_dl_logfname_c=>sc_docid_h,
  gc_dl_logfname_productid_i     TYPE /scdl/dl_logfname
       VALUE /scdl/if_dl_logfname_c=>sc_productid_i,
  gc_dl_logfname_door_i          TYPE /scdl/dl_logfname
     VALUE /scwm/if_dl_logfname_c=>sc_door_i,
  gc_dl_logfname_refdocno_bol_h  TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_refdocno_bol_h,
  gc_dl_logfname_refdocno_asn_h  TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_refdocno_asn_h,
  gc_dl_logfname_value_dun_i     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dun_i,
  gc_dl_logfname_value_dpt_i     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dpt_i,


  gc_dl_logfname_value_dbc_h     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dbc_h,
  gc_dl_logfname_value_dlvpldat  TYPE /scdl/dl_logfname
    VALUE /scdl/if_dl_logfname_c=>sc_tstfr_tdelivery_plan_h,
  gc_dl_logfname_value_dlvactdat TYPE /scdl/dl_logfname
   VALUE /scdl/if_dl_logfname_c=>sc_tstfr_tdelivery_actual_h,

* FieldNames in Structure
  gc_scr_elmnt_door_dd_past      TYPE /scwm/de_screlm_name VALUE
             '/SCWM/S_RF_REHU_HU-DOOR_DD_PAST',
  gc_scr_elmnt_door_dd_fut       TYPE /scwm/de_screlm_name VALUE
             '/SCWM/S_RF_REHU_HU-DOOR_DD_FUT',

  gc_param_cs_rehu_hu            TYPE /scwm/de_param_name VALUE
             'CS_REHU_HU' ,
  gc_param_cs_rehu               TYPE /scwm/de_param_name VALUE
             'CS_REHU' ,
  gc_param_ct_rehu_hu            TYPE /scwm/de_param_name VALUE
             'CT_REHU_HU' ,
  gc_param_cs_rehu_prod          TYPE /scwm/de_param_name VALUE
             'CS_REHU_PROD' ,
  gc_param_ct_rehu_prod          TYPE /scwm/de_param_name VALUE
             'CT_REHU_PROD' ,
  gc_param_cs_rehu_dlv           TYPE /scwm/de_param_name VALUE
              'CS_REHU_DLV' ,
  gc_param_ct_rehu_dlv           TYPE /scwm/de_param_name VALUE
              'CT_REHU_DLV' .

CONSTANTS:
  gc_fcode_gtnwhu TYPE /scwm/de_fcode VALUE 'GTNWHU',
  gc_fcode_gtexhu TYPE /scwm/de_fcode VALUE 'GTEXHU',
  gc_fcode_gtegsl TYPE /scwm/de_fcode VALUE 'GTEGSL',
  gc_fcode_backf  TYPE /scwm/de_fcode VALUE 'BACKF',
  gc_fcode_enter  TYPE /scwm/de_fcode VALUE 'ENTER',
  gc_fcode_gtdlsl TYPE /scwm/de_fcode VALUE 'GTDLSL',
  gc_fcode_gtpmsl TYPE /scwm/de_fcode VALUE 'GTPMSL',
  gc_fcode_gthudt TYPE /scwm/de_fcode VALUE 'GTHUDT',
  gc_fcode_gtchpm TYPE /scwm/de_fcode VALUE 'GTCHPM',
  gc_fcode_gtchqy TYPE /scwm/de_fcode VALUE 'GTCHQY',
  gc_fcode_rhchqy TYPE /scwm/de_fcode VALUE 'RHCHQY',
  gc_fcode_rhchpm TYPE /scwm/de_fcode VALUE 'RHCHPM',
  gc_fcode_gtchbd TYPE /scwm/de_fcode VALUE 'GTCHBD',
  gc_fcode_gthumi TYPE /scwm/de_fcode VALUE 'GTHUMI',
  gc_fcode_gthuad TYPE /scwm/de_fcode VALUE 'GTHUAD',
  gc_fcode_next   TYPE /scwm/de_fcode VALUE 'NEXT',
  gc_fcode_updbck TYPE /scwm/de_fcode VALUE 'UPDBCK',
  gc_fcode_yes    TYPE /scwm/de_fcode VALUE 'YES   '.

CONSTANTS:
  gc_prmod_foregr TYPE /scwm/de_prmod VALUE '2',
  gc_prmod_backgr TYPE /scwm/de_prmod VALUE '1'.

CONSTANTS:
  gc_hu_laod_info_unlo TYPE char1 VALUE 'A',
  gc_huident_length    TYPE i VALUE 20,
  gc_xfeld             TYPE xfeld VALUE 'X'.
