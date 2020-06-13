*&--------------------------------------------------------------------*
*&      Form  to_for_hu_read
*&--------------------------------------------------------------------*
*       Read TO for a given HU
*---------------------------------------------------------------------*
*      -->IV_LGNUM    Warehouse Number
*      -->IV_HUIDENT  HU Identification
*      -->IV_IPROC    Internal Process
*      <->CV_TANUM    TO Number
*      <->CV_NLPLA    Destination Storage Bin
*---------------------------------------------------------------------*
FORM to_for_hu_read
  USING    iv_lgnum   TYPE /scwm/lgnum
           iv_huident TYPE /scwm/huident
           iv_iproc   TYPE /scwm/de_iproc
  CHANGING cv_tanum   TYPE /scwm/tanum
           cv_nlpla   TYPE /scwm/lgpla
           cv_actty   TYPE /scwm/de_actty.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*

  DATA: lt_ordim_o TYPE /scwm/tt_ordim_o,
        ls_ordim_o TYPE /scwm/ordim_o,
        ls_t331    TYPE /scwm/t331.

  FIELD-SYMBOLS: <fs_ordim_o> TYPE /scwm/ordim_o.

*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*

  CALL FUNCTION '/SCWM/TO_READ_SRC'
    EXPORTING
      iv_lgnum     = iv_lgnum
      iv_huident   = iv_huident
      iv_nobuf     = abap_true
      iv_no_waiting = 'X'
    IMPORTING
      et_ordim_o   = lt_ordim_o
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      OTHERS       = 99.

  IF sy-subrc <> 0.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF..

  IF lt_ordim_o IS INITIAL.
    CALL FUNCTION 'DEQUEUE_/SCWM/EHU'
      EXPORTING
        mode_/scwm/s_huhdr_int = 'E'
        mandt                  = sy-mandt
        huident                = iv_huident
        lgnum                  = iv_lgnum.

    MESSAGE e017(/scwm/rf_de) WITH iv_huident.
*   Transfer Order for HU &1 does not exist
  ENDIF.

* one HU-TO for the HU?
  SORT lt_ordim_o BY vlenr.
  READ TABLE lt_ordim_o ASSIGNING <fs_ordim_o>
       WITH KEY flghuto = 'X'.

  IF sy-subrc <> 0.
    MESSAGE e017(/scwm/rf_de) WITH iv_huident.
*   Transfer Order for HU &1 does not exist
  ENDIF.

* check for unload WT
  If <fs_ordim_o>-vltyp IS NOT INITIAL.
*   check for door
    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum        = <fs_ordim_o>-lgnum
        iv_lgtyp        = <fs_ordim_o>-vltyp
      IMPORTING
        es_t331         = ls_t331
      EXCEPTIONS
        others          = 0.
    IF ls_t331-st_role <> wmegc_strole_door.
      MESSAGE e017(/scwm/rf_de) WITH iv_huident.
*     (Unload) Transfer Order for HU &1 does not exist
    ENDIF.
  ELSEIF <fs_ordim_o>-stu_num IS INITIAL.
    MESSAGE e017(/scwm/rf_de) WITH iv_huident.
*   (Unload) Transfer Order for HU &1 does not exist
  ENDIF.

  MOVE: <fs_ordim_o>-tanum    TO cv_tanum,
        <fs_ordim_o>-nlpla    TO cv_nlpla,
        <fs_ordim_o>-act_type TO cv_actty.

ENDFORM.                    "to_for_hu_read
