*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF06 .
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  who_rsrc_allowed
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->IV_LGNUM   text
*      -->IV_RSRC    text
*      -->IV_RSRC_GRPtext
*      -->IV_WHO     text
*      -->EV_YES     text
*---------------------------------------------------------------------*
FORM who_rsrc_allowed
     USING   iv_lgnum    TYPE /scwm/lgnum
             iv_rsrc     TYPE /scwm/de_rsrc
             iv_rsrc_grp TYPE /scwm/de_rsrc_grp
             iv_queue    TYPE /scwm/de_queue
             iv_who      TYPE /scwm/de_who
     CHANGING ev_yes     TYPE xfeld.
  DATA: ls_rsrc    TYPE /scwm/rsrc,
        ls_wo_rsrc TYPE /scwm/wo_rsrc_ty,
        lt_wo_rsrc TYPE /scwm/tt_wo_rsrc_ty.

  MOVE: iv_lgnum    TO ls_rsrc-lgnum,
        iv_rsrc     TO ls_rsrc-rsrc,
        iv_rsrc_grp TO ls_rsrc-rsrc_grp,
        'X'         TO ls_rsrc-rfind.

  MOVE: iv_lgnum TO ls_wo_rsrc-lgnum,
        iv_queue TO ls_wo_rsrc-queue,
        iv_who   TO ls_wo_rsrc-who.

  APPEND ls_wo_rsrc TO lt_wo_rsrc.

  CALL FUNCTION '/SCWM/RSRC_QUALIF_QUEUE_CHECK'
    EXPORTING
      iv_man_wo_sel = 'X'
      iv_who        = iv_who
    CHANGING
      cs_rsrc       = ls_rsrc
      ct_wo_rsrc_ty = lt_wo_rsrc.

  READ TABLE lt_wo_rsrc TRANSPORTING NO FIELDS
                        WITH KEY who = iv_who.
  IF sy-subrc = 0.
    ev_yes = 'X'.
  ENDIF.
ENDFORM.                    "who_rsrc_allowed
