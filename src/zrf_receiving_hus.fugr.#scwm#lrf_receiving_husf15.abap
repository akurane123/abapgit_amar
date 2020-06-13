*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF15 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_hu_exists
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_REHU_HU_HUIDENT  text
*      <--P_LV_EXIST  text
*----------------------------------------------------------------------*
FORM check_hu_exists  USING    iv_huident TYPE /scwm/de_huident
                      CHANGING cv_exist TYPE xfeld.

  DATA: ls_huhdr TYPE /scwm/s_huhdr_int.        "#EC NEEDED

  DATA: lo_packing TYPE REF TO /scwm/cl_wm_packing.

  cv_exist = ' '.

  IF lo_packing IS NOT BOUND.
    CREATE OBJECT lo_packing.
  ENDIF.

* read all necessary data: HU status
  CALL METHOD lo_packing->get_hu
    EXPORTING
      iv_huident = iv_huident
*     IV_GUID_HU =
      iv_lock    = 'X'
    IMPORTING
*      et_huitm   = ct_huitm
      es_huhdr   = ls_huhdr
*     ET_HUHDR   =
*     ET_HUTREE  =
*      et_huref   = lt_huref
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
   cv_exist = 'X'.
  ENDIF.


ENDFORM.                    " check_hu_exists
