*&---------------------------------------------------------------------*
*&      Form  wo_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_LGNUM  text
*      -->P_IV_WHO  text
*      <--P_LS_WO  text
*----------------------------------------------------------------------*
FORM wo_read
     USING    iv_lgnum  TYPE /scwm/lgnum
              iv_wo     TYPE /scwm/de_who
     CHANGING cs_wo     TYPE /scwm/s_who_int.

  TRY.
      CALL FUNCTION '/SCWM/WHO_SELECT'
        EXPORTING
          iv_lgnum = iv_lgnum
          iv_who   = iv_wo
        IMPORTING
          es_who   = cs_wo.

    CATCH /scwm/cx_core. "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " wo_read
