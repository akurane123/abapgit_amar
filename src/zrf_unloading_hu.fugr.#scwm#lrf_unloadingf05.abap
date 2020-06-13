*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_UNLOADINGF05 .
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  who_select_for_rsrc_allowed
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->IV_LGNUM   text
*      -->IV_RSRC    text
*      -->IV_RSRC_GRPtext
*      -->IV_QUEUE   text
*      -->CT_WHO     text
*---------------------------------------------------------------------*
FORM who_select_for_rsrc_allowed
     USING   iv_lgnum    TYPE /scwm/lgnum
             iv_rsrc     TYPE /scwm/de_rsrc
             iv_rsrc_grp TYPE /scwm/de_rsrc_grp
             iv_queue    TYPE /scwm/de_queue
    CHANGING ct_who      TYPE /scwm/tt_who_int.


  DATA: ls_rsrc    TYPE /scwm/rsrc,
        ls_wo_rsrc TYPE /scwm/wo_rsrc_ty,
        lt_wo_rsrc TYPE /scwm/tt_wo_rsrc_ty,
        ls_who     TYPE /scwm/s_who_int.

  MOVE: iv_lgnum    TO ls_rsrc-lgnum,
        iv_rsrc     TO ls_rsrc-rsrc,
        iv_rsrc_grp TO ls_rsrc-rsrc_grp,
        'X'         TO ls_rsrc-rfind.

  LOOP AT ct_who INTO ls_who.

    MOVE: iv_lgnum    TO ls_wo_rsrc-lgnum,
          iv_queue    TO ls_wo_rsrc-queue,
          ls_who-who  TO ls_wo_rsrc-who.
    APPEND ls_wo_rsrc TO lt_wo_rsrc.

  ENDLOOP.

  CALL FUNCTION '/SCWM/RSRC_QUALIF_QUEUE_CHECK'
    CHANGING
      cs_rsrc       = ls_rsrc
      ct_wo_rsrc_ty = lt_wo_rsrc.

  CLEAR: ls_wo_rsrc.
  CLEAR: ct_who.

  LOOP AT lt_wo_rsrc INTO ls_wo_rsrc.
    ls_who-who = ls_wo_rsrc-who.
    APPEND ls_who TO ct_who.
    CLEAR: ls_who, ls_wo_rsrc.
  ENDLOOP.

ENDFORM.                    "who_select_for_rsrc_allowed
