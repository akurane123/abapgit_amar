*----------------------------------------------------------------------*
***INCLUDE LZRF_OUTBOUNDF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  nested_hu_chk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nested_hu_chk CHANGING cs_ordim_confirm TYPE /scwm/s_rf_ordim_confirm
                            cv_flg_nest_hu  TYPE xfeld
                            cv_flg_huent_ok  TYPE xfeld
                            cv_direct_stock  TYPE xfeld.

  DATA lv_hu_verif     TYPE /scwm/de_vlenr_verif.
  DATA lv_flg_mixed_hu TYPE xfeld.
  DATA lv_flg_hu_okay  TYPE xfeld.

  IF cs_ordim_confirm-vlenr_verif IS INITIAL.
    lv_hu_verif = cs_ordim_confirm-vlenr.
  ELSE.
    lv_hu_verif = cs_ordim_confirm-vlenr_verif.
  ENDIF.

  CALL FUNCTION '/SCWM/RF_PICK_HUNEST_CHECK'
    EXPORTING
      iv_hu_verif        = lv_hu_verif
      iv_hu_parent       = cs_ordim_confirm-vlenr
      iv_lgnum           = cs_ordim_confirm-lgnum
      iv_rsrc            = cs_ordim_confirm-srsrc
      iv_lgpla           = cs_ordim_confirm-vlpla
    IMPORTING
      ev_flg_nest_ok     = cv_flg_nest_hu
      ev_flg_huent_ok    = cv_flg_huent_ok
      ev_direct_stock    = cv_direct_stock
    CHANGING
      ordim_confirm      = cs_ordim_confirm
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

ENDFORM.                    " nested_hu_chk
