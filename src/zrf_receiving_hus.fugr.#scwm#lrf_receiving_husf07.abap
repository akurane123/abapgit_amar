*&---------------------------------------------------------------------*
*&      Form  to_read_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LGNUM  text
*      -->IV_TANUM  text
*      -->IV_LOCK  text
*      <--cs_ordim_o
*----------------------------------------------------------------------*
FORM to_read_lock
     USING    iv_lgnum   TYPE /scwm/lgnum
              iv_tanum   TYPE /scwm/tanum
              iv_lock    TYPE xfeld
     CHANGING cs_ordim_o TYPE /scwm/ordim_o.

  IF iv_tanum IS INITIAL.
    MESSAGE e037(/scwm/rf_de).
*   putaway not possible
  ENDIF.

  CALL FUNCTION '/SCWM/TO_READ_SINGLE'
    EXPORTING
      iv_lgnum     = iv_lgnum
      iv_tanum     = iv_tanum
      iv_flglock   = iv_lock
    IMPORTING
      es_ordim_o   = cs_ordim_o
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      error        = 4
      OTHERS       = 99.

  IF sy-subrc <> 0.                     "#EC NEEDED
*   not interested in exceptions
  ENDIF.

ENDFORM.                    " to_read_lock
