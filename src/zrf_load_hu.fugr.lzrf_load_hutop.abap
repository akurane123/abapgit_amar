FUNCTION-POOL zrf_load_hu.                  "MESSAGE-ID ..
DATA: gt_hu_seq     TYPE /scwm/tt_hu_load_seq,
      gt_obj_data   TYPE STANDARD TABLE OF /sttpec/s_att_obj_response,
      gv_docid      TYPE /scwm/de_docid,
      gv_tu_num     TYPE /scwm/de_tu_num,
      so_prd        TYPE REF TO /scwm/cl_dlv_management_prd,
      so_packing TYPE REF TO /scwm/cl_wm_packing.
* INCLUDE LZRF_LOAD_HUD...                   " Local class definition
CONSTANTS: gc_fcode_complex      TYPE /scwm/de_fcode VALUE 'SYCMPL',
           gc_fcode_simple       TYPE /scwm/de_fcode VALUE 'SYSMPL',
           gc_fcode_immediately  TYPE /scwm/de_fcode VALUE 'SYIMED',
           gc_mask               TYPE char4 VALUE '(00)',
           gc_code_type          TYPE /sttpec/e_code_type VALUE 'C',
           gc_activity           TYPE /sttpec/e_cactivno VALUE '17',
           gc_lock               TYPE xfeld VALUE 'X',
           gc_sync_dlv           TYPE i VALUE 10,
           gc_dl_date_type       TYPE /scdl/dl_tsttype VALUE /scdl/if_dl_date_c=>sc_tsttype_sload,
           gc_dl_date_cat_actual TYPE /scdl/dl_tst_category  VALUE /scdl/if_dl_date_c=>sc_tstcat_actual,
           gc_decode_mode        TYPE /sttpec/e_id_mode_dec VALUE 1.
