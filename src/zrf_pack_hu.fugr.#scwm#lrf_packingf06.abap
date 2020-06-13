*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PACKINGF06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  initialize_manual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <-- CS_WRKC  text
*      <-->LO_WM_PACK  text
*----------------------------------------------------------------------*
 FORM initialize_manual
  USING    is_wrkc TYPE /scwm/tworkst
  CHANGING co_wm_pack TYPE REF TO /scwm/cl_wm_packing.

   DATA: ls_pack_controle TYPE /scwm/s_pack_controle.
*  it was necessary to name this variable cs_wrkc because of IMPORT FROM MEMORY ID
*  did not work with ls_wrkc.
   DATA  cs_wrkc          TYPE /scwm/tworkst.

   IF co_wm_pack IS NOT BOUND.
     CALL METHOD /scwm/cl_wm_packing=>get_instance
       IMPORTING
         eo_instance = co_wm_pack.
   ENDIF.

* cleanup the proposal based on planned shipping HUs
   /scwm/cl_pohu_propose=>cleanup( ).

   IMPORT cs_wrkc FROM MEMORY ID gc_pack01.

   IF cs_wrkc IS INITIAL.
* The customizing values of the Workcenter for "Check Consol Grp" are:
* SPACE - No check
* 1     - Check while repacking products
* 2     - Check while repacking HUs and products
*
     ls_pack_controle-cdstgrp_mat    = gc_xfeld.
     ls_pack_controle-cdstgrp_hu     = gc_xfeld.
     ls_pack_controle-chkpack_dstgrp = '2'.    "move later into top include as constant
     ls_pack_controle-processor_det  = abap_true.

     CALL METHOD co_wm_pack->init
       EXPORTING
         iv_lgnum         = is_wrkc-lgnum
         is_pack_controle = ls_pack_controle
       EXCEPTIONS
         error            = 1
         OTHERS           = 2.
     IF sy-subrc <> 0.
       /scwm/cl_pack_view=>msg_error( ).
     ENDIF.
   ELSE.
     PERFORM initialize
            USING    cs_wrkc
            CHANGING co_wm_pack.
   ENDIF.
 ENDFORM.                    " initialize_manual

*&---------------------------------------------------------------------*
*&      Form  get_internal_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CS_PACK  text
*      <--CS_WRKC  text
*      <-->CV_IPROC  text
*----------------------------------------------------------------------*
 FORM get_internal_step  USING
                         cs_wrkc TYPE /scwm/tworkst
                         CHANGING
                         cv_iproc TYPE /scwm/de_iproc.

   DATA: ls_tprocs TYPE /scwm/s_tprocs.


   CALL FUNCTION '/SCWM/TPROCS_READ_SINGLE'
     EXPORTING
       iv_procs  = cs_wrkc-procs
     IMPORTING
       es_tprocs = ls_tprocs
     EXCEPTIONS
       not_found = 1
       OTHERS    = 2.

   IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   ELSE.
     MOVE ls_tprocs-iproc TO cv_iproc.
   ENDIF.


 ENDFORM.                    " get_internal_step
*&---------------------------------------------------------------------*
*&    Form  CONVERT_QUAN
*&---------------------------------------------------------------------*
*     Convert the quantity between alternative unit of measure and
*     base unit of measure
*----------------------------------------------------------------------*
*     -->IV_MATID
*     -->IV_UNIT_FROM
*     -->IV_UNIT_TO
*     -->IV_BATCHID
*     <--CV_QUAN
*----------------------------------------------------------------------*
 FORM convert_quan
      USING    iv_matid     TYPE /scwm/de_matid
               iv_unit_from TYPE /scwm/de_unit
               iv_unit_to   TYPE /scwm/de_unit
               iv_batchid   TYPE /scwm/de_batchid
      CHANGING cv_quan      TYPE /scwm/de_quantity.

   DATA: lv_quan   TYPE /scwm/de_quantity.

   TRY.
       CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
         EXPORTING
           iv_matid     = iv_matid
           iv_quan      = cv_quan
           iv_unit_from = iv_unit_from
           iv_unit_to   = iv_unit_to
           iv_batchid   = iv_batchid
         IMPORTING
           ev_quan      = lv_quan.
     CATCH /scwm/cx_md_interface
           /scwm/cx_md_batch_required
           /scwm/cx_md_internal_error
           /scwm/cx_md_batch_not_required
           /scwm/cx_md_material_exist.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   ENDTRY.

   MOVE lv_quan TO cv_quan.

 ENDFORM.                    " convert_quan

*&---------------------------------------------------------------------*
*&      Form  opunit_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_PACK    text
*      -->IS_WRKC    text
*      -->CV_OPUNIT  text
*----------------------------------------------------------------------*
 FORM opunit_check USING   is_pack    TYPE /scwm/s_rf_pack
                           is_wrkc    TYPE /scwm/tworkst
                  CHANGING cv_opunit  TYPE /scwm/de_rf_opunit.

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
           lgnum = is_wrkc-lgnum.
     CATCH cx_badi.                                     "#EC NO_HANDLER
   ENDTRY.

   ls_ltap-matid         = is_pack-matid.
   ls_ltap-matnr         = is_pack-rfhu.
   ls_ltap-charg         = is_pack-charg.
   ls_ltap-batchid       = is_pack-batchid.
   ls_ltap-altme         = is_pack-altme.

   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
     EXPORTING
       input  = ls_ltap-matnr
     IMPORTING
       output = ls_ltap-matnr.

   TRY.
       CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
         EXPORTING
           iv_matid      = is_pack-matid
           iv_lgnum      = is_wrkc-lgnum
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
     CATCH cx_badi.                                     "#EC NO_HANDLER
   ENDTRY.
 ENDFORM.                    "opunit_check


 FORM rollback_for_repack_all USING   is_pack    TYPE /scwm/s_rf_pack
                            is_wrkc    TYPE /scwm/tworkst.

   DATA: lt_aquay  TYPE /scwm/tt_aquay.
   DATA: ls_aqua   TYPE /scwm/aquay.
   DATA: lo_wm_pack TYPE REF TO /scwm/cl_wm_packing.

   DATA: ls_huhdr_pick TYPE /scwm/s_huhdr_int.
   DATA: lt_huitm TYPE /scwm/tt_huitm_int,
         lt_huhdr TYPE /scwm/tt_huhdr_int,
         ls_huitm TYPE /scwm/s_huitm_int.

   CALL METHOD /scwm/cl_tm=>register_rollback.
   CALL METHOD /scwm/cl_tm=>cleanup.

   PERFORM initialize_manual
          USING    is_wrkc
          CHANGING lo_wm_pack.

   CALL METHOD lo_wm_pack->get_hu
     EXPORTING
       iv_huident = is_pack-vlenr
     IMPORTING
       es_huhdr   = ls_huhdr_pick
       et_huhdr   = lt_huhdr
       et_huitm   = lt_huitm
     EXCEPTIONS
       not_found  = 1
       OTHERS     = 2.
   IF sy-subrc <> 0.
     /scwm/cl_pack_view=>msg_error( ).
   ENDIF.

   LOOP AT lt_huitm INTO ls_huitm.
     CLEAR ls_aqua.
     ls_aqua-mandt     = sy-mandt.
     ls_aqua-flgmove   =  ls_huhdr_pick-flgmove.
     ls_aqua-vsi       = ls_huitm-vsi.
     ls_aqua-lgnum     = ls_huhdr_pick-lgnum.
     ls_aqua-loc_type  = ls_huhdr_pick-loc_type.
     ls_aqua-location  = ls_huhdr_pick-lgpla.
     ls_aqua-huident   = is_pack-vlenr.
     ls_aqua-vhi       = ls_huhdr_pick-vhi.
     ls_aqua-guid_stock  = ls_huitm-guid_stock.
     ls_aqua-quan      = ls_huitm-quan.

     APPEND ls_aqua TO lt_aquay.

     ls_aqua-vsi   =  wmegc_reserved_stock.
     APPEND ls_aqua TO lt_aquay.
   ENDLOOP.

   CALL FUNCTION '/SCWM/AQUAY_DELETE'
     CHANGING
       ct_aquay = lt_aquay.

 ENDFORM.
