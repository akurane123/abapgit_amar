FUNCTION-POOL zrf_reapacking_sscc MESSAGE-ID /scwm/rf_en.          "MESSAGE-ID ..

* INCLUDE LZRF_REAPACKING_SSCCD...           " Local class definition
INCLUDE /scwm/irf_sscr.



DATA: gt_objstruc     TYPE /sttpec/t_att_obj_hierarchy,
      gt_objstruc_qty TYPE /sttpec/t_att_qty_hierarchy,
      gt_objdata      TYPE /sttpec/t_att_obj_response,
      gt_child_sn     TYPE zrf_tt_child_sn,
*      gv_matnr        TYPE /sttpec/e_matnr,
      gv_maktx        TYPE /scwm/de_desc40,
      gv_uom          TYPE /sttpec/e_meinh,
      gv_tanum        TYPE /scwm/tanum,
      gv_lgnum        TYPE /scwm/lgnum,
      gt_create       TYPE /scwm/tt_to_create_int.

* FCodes for Repacking
CONSTANTS:
  gc_fcode_simple       TYPE /scwm/de_fcode VALUE 'SYSMPL',
  gc_fcode_complex      TYPE /scwm/de_fcode VALUE 'SYCMPL',
  gc_fcode_crhu         TYPE /scwm/de_fcode VALUE 'GTNWHU',
  gc_fcode_ende         TYPE /scwm/de_fcode VALUE 'SYENDE',
  gc_fcode_goon         TYPE /scwm/de_fcode VALUE 'SYGOON',
  gc_fcode_yes          TYPE /scwm/de_fcode VALUE 'YES   ',
  gc_fcode_dlvsel       TYPE /scwm/de_fcode VALUE 'DLVSEL',
  gc_fcode_enter        TYPE /scwm/de_fcode VALUE 'ENTER',
  gc_fcode_cmpltx       TYPE /scwm/de_fcode VALUE 'CMPTRS', "leave TX
  gc_fcode_pack         TYPE /scwm/de_fcode VALUE 'ZPACK',
  gc_fcode_backf        TYPE /scwm/de_fcode VALUE 'BACKF',
  gc_fcode_comm         TYPE /scwm/de_fcode VALUE 'ZCOMM',

  gc_lowchk_inv_perform TYPE /scwm/de_lowchk_inv VALUE '1',
  gc_lowchk_inv_compl   TYPE /scwm/de_lowchk_inv VALUE '3'.





INCLUDE zrf_global_top.
INCLUDE zrf_global_f01.
