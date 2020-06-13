FUNCTION zrf_pack_hu_create_pai.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CS_PACK) TYPE  /SCWM/S_RF_PACK
*"     REFERENCE(CS_WRKC) TYPE  /SCWM/TWORKST
*"--------------------------------------------------------------------
  DATA: lo_wm_pack TYPE REF TO /scwm/cl_wm_packing.

  DATA: ls_huhdr TYPE /scwm/s_huhdr_int.

  DATA: lv_ltrans TYPE /scwm/de_ltrans.


  DATA: lv_hu TYPE /scwm/de_rf_rfhu_long.

  BREAK-POINT ID /scwm/rf_packing.

* get instance and initialize log
  PERFORM initialize
         USING    cs_wrkc
         CHANGING lo_wm_pack.

  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'
    EXPORTING
      iv_workcenter = cs_wrkc-workstation.


  IF cs_pack-nlenr IS INITIAL.
** Commission serialized material
    PERFORM create_sscc CHANGING lv_hu.
    cs_pack-nlenr = lv_hu+4(20).
    CALL METHOD lo_wm_pack->/scwm/if_pack~create_hu
      EXPORTING
        iv_pmat    = cs_pack-pmatid
        iv_huident = cs_pack-nlenr
        i_location = cs_pack-nlpla
      RECEIVING
        es_huhdr   = ls_huhdr
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>message(
*          iv_msg_view          = gc_msg_view_scr
           iv_flg_continue_flow = gc_xfeld
           iv_msgid             = gc_rf_de_msgid
           iv_msgty             = gc_msgty_success
           iv_msgno             = '224'
           iv_msgv1             = ls_huhdr-huident ).
    ENDIF.
  ELSE.
    CALL METHOD lo_wm_pack->/scwm/if_pack~get_hu
      EXPORTING
        iv_huident = cs_pack-nlenr
      IMPORTING
        es_huhdr   = ls_huhdr
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    CASE sy-subrc.
      WHEN 0.
        EXIT.
      WHEN 1.
        PERFORM initialize
                USING    cs_wrkc
                CHANGING lo_wm_pack.
        CALL METHOD lo_wm_pack->/scwm/if_pack~create_hu
          EXPORTING
            iv_pmat    = cs_pack-pmatid
            iv_huident = cs_pack-nlenr
            i_location = cs_pack-nlpla
          RECEIVING
            es_huhdr   = ls_huhdr
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.
        IF sy-subrc <> 0.
          /scwm/cl_pack_view=>msg_error( ).
        ENDIF.

      WHEN 2.
        /scwm/cl_pack_view=>msg_error( ).
      WHEN OTHERS.
        /scwm/cl_pack_view=>msg_error( ).
    ENDCASE.
  ENDIF.
  IF cs_pack-dstgrp IS NOT INITIAL.
* update distribution group in HU header
    ls_huhdr-dstgrp = cs_pack-dstgrp.

    CALL METHOD lo_wm_pack->/scwm/if_pack~change_huhdr
      EXPORTING
        is_huhdr = ls_huhdr
      IMPORTING
        es_huhdr = ls_huhdr
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      /scwm/cl_pack_view=>msg_error( ).
    ENDIF.
  ENDIF.

  CALL METHOD lo_wm_pack->/scwm/if_pack~save
    EXPORTING
      iv_commit = 'X'
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    /scwm/cl_pack_view=>msg_error( ).
  ELSE.
    lv_ltrans = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
    IF lv_ltrans = gc_ltrans_pahucs.
      CLEAR cs_pack.
    ENDIF.
* initialize log
    PERFORM initialize
           USING    cs_wrkc
           CHANGING lo_wm_pack.
  ENDIF.
ENDFUNCTION.
