*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PACKINGF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM initialize
     USING    cs_wrkc TYPE /scwm/tworkst
     CHANGING co_wm_pack TYPE REF TO /scwm/cl_wm_packing.

  DATA: ls_pack_controle TYPE /scwm/s_pack_controle.

  DATA: cv_iproc TYPE /scwm/de_iproc.

  DATA: lo_pack_stg TYPE REF TO /scwm/cl_wm_packing_stg.


  PERFORM get_internal_step
        USING     cs_wrkc
        CHANGING  cv_iproc.

  IF co_wm_pack IS NOT BOUND.

* create lo_pack
    IF cv_iproc = wmegc_iproc_stag.
      CREATE OBJECT lo_pack_stg.
      co_wm_pack = lo_pack_stg.
    ELSE.
      CALL METHOD /scwm/cl_wm_packing=>get_instance
        IMPORTING
          eo_instance = co_wm_pack.
    ENDIF.

  ENDIF.



  CASE cv_iproc.
    WHEN wmegc_iproc_pac OR wmegc_iproc_vas.

      ls_pack_controle-cdstgrp_mat = 'X'.
      ls_pack_controle-cdstgrp_hu = 'X'.

    WHEN wmegc_iproc_stag.


      ls_pack_controle-cdstgrp_mat = 'X'.
      ls_pack_controle-cdstgrp_hu = ' '.

  ENDCASE.

* Determine processor from sy-user in case LM is activ
  ls_pack_controle-processor_det = 'X'.

  CALL METHOD co_wm_pack->init_by_workstation
    EXPORTING
      is_workstation   = cs_wrkc
      is_pack_controle = ls_pack_controle
      iv_no_hu_by_wc   = 'X'
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    /scwm/cl_pack_view=>msg_error( ).
  ENDIF.

ENDFORM.                    " initialize
