*&--------------------------------------------------------------------*
*&      Form  simple_or_complex
*&--------------------------------------------------------------------*
*       Simple or Complex unloading (w/o or with HUs)?
*---------------------------------------------------------------------*
*      -->IV_LGNUM   Warehouse Number
*      -->IS_HUHDR   HU Header
*      <->CV_FCODE   Function Code
*---------------------------------------------------------------------*
FORM simple_or_complex
  USING    iv_lgnum TYPE /scwm/lgnum
           is_huhdr TYPE /scwm/s_huhdr_int
  CHANGING cv_fcode TYPE /scwm/de_fcode.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
  DATA: lv_severity TYPE bapi_mtype.

*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*

* does HU have storage process?
  IF is_huhdr-prces IS INITIAL.
*   --> simple unloading
    MOVE gc_fcode_simple TO cv_fcode.
  ELSE.
*   check: unloading in storage process profile
    CALL FUNCTION '/SCWM/IPROC_IN_PRCES_CHECK'
      EXPORTING
        iv_lgnum        = iv_lgnum
        iv_prces        = is_huhdr-prces
        iv_iproc        = wmegc_iproc_unlo
      IMPORTING
        ev_severity     = lv_severity
      EXCEPTIONS
        interface_error = 1
        OTHERS          = 99.

    IF sy-subrc <> 0.
*   not interested in exceptions
    ENDIF.

    IF lv_severity = wmegc_severity_suc.
*     --> complex unloading
      MOVE gc_fcode_complex TO cv_fcode.
    ELSE.
*     --> simple unloading
      MOVE gc_fcode_simple TO cv_fcode.
    ENDIF.

  ENDIF.

ENDFORM.                    "hu_read_and_lock
