*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PICKINGF06.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PICKER_DRIVEN_RPL_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ORDIM_CONFIRM  text
*      -->P_TT_ORDIM_CONFIRM  text
*      <--P_LV_PICKER_DRV_REPL  text
*----------------------------------------------------------------------*
FORM PICKER_DRIVEN_RPL_CHK
  USING
    ls_ordim_confirm     TYPE /scwm/s_rf_ordim_confirm
    tt_ordim_confirm     TYPE /scwm/tt_rf_ordim_confirm
  CHANGING
    cv_picker_drv_repl   TYPE XFELD
.

    FIELD-SYMBOLS <ls_ordim_confirm> TYPE /scwm/s_rf_ordim_confirm.

    CLEAR cv_picker_drv_repl.
    IF ls_ordim_confirm-str_repl = wmegc_repl_emergency. "direct repl."
*     Find a picking WT, to distungish between picker driven
*     replenishment and the normal replenishment
      LOOP AT tt_ordim_confirm ASSIGNING <ls_ordim_confirm>.
       IF <ls_ordim_confirm>-str_repl IS INITIAL.
         cv_picker_drv_repl = 'X'.
         EXIT.
       ENDIF.
      ENDLOOP.
    ENDIF.
ENDFORM.                    " PICKER_DRIVEN_RPL_CHK
