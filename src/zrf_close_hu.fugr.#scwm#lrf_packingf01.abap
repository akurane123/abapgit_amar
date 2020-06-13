*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_PACKINGF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_wc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WRKC  text
*      <--P_PACK  text
*----------------------------------------------------------------------*

FORM get_wc
  CHANGING cs_wrkc TYPE /scwm/tworkst.

* to do: create new table for hostname assignment
*----------------------------------------------------------------------*
*  DATA DECLARATIONS                                                   *
*----------------------------------------------------------------------*

  DATA: lt_workstation TYPE TABLE OF /scwm/tworkst.

  DATA: lv_terminal    TYPE /scwm/de_wc_terminal.

*----------------------------------------------------------------------*
*  PROGRAM LOGIC                                                       *
*----------------------------------------------------------------------*

  IMPORT cs_wrkc FROM MEMORY ID gc_pack01.
  IMPORT gv_trtyp FROM MEMORY ID gc_pack02.

  IF cs_wrkc IS INITIAL.

    CALL FUNCTION 'TH_USER_INFO'
      IMPORTING
        terminal = lv_terminal.

    SELECT * FROM /scwm/tworkst
             INTO TABLE lt_workstation
             WHERE terminal = lv_terminal.

    IF NOT sy-subrc IS INITIAL.
*      MOVE text-002 TO sprd-text1.
      /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
      /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_fcwcin ).
      EXIT.
    ENDIF.

    READ TABLE lt_workstation TRANSPORTING NO FIELDS INDEX 2.
    IF sy-subrc IS INITIAL.
*      MOVE text-002 TO sprd-text1.
      /scwm/cl_rf_bll_srvc=>set_prmod( gc_prmod_backgr ).
      /scwm/cl_rf_bll_srvc=>set_fcode( gc_fcode_fcwcin ).
      EXIT.
    ENDIF.

    READ TABLE lt_workstation INTO cs_wrkc INDEX 1.  "#EC CI_NOORDER

  ELSE.
    CALL METHOD /scwm/cl_tm=>set_lgnum( cs_wrkc-lgnum ).
  ENDIF.


ENDFORM.                    " get_wc
