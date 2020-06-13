*&---------------------------------------------------------------------*
*&      Form  to_read_lock
*&---------------------------------------------------------------------*
*       Read and lock Transfer Order
*----------------------------------------------------------------------*
*      -->IV_LGNUM    Warehouse Number
*      -->IV_TANUM    Transfer Order Number
*      -->IV_LOCK     Flag: Lock TO
*      <->CS_ORDIM_O  TO Structure
*----------------------------------------------------------------------*
FORM to_read_lock
  USING    iv_lgnum   TYPE /scwm/lgnum
           iv_tanum   TYPE /scwm/tanum
           iv_lock    TYPE xfeld
  CHANGING cs_ordim_o TYPE /scwm/ordim_o.

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
      OTHERS       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID     sy-msgid
            TYPE   sy-msgty
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " to_read_lock
