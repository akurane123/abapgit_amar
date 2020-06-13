*&--------------------------------------------------------------------*
*&      Form  hu_read_and_lock
*&--------------------------------------------------------------------*
*       Read, Lock and Check HU
*---------------------------------------------------------------------*
*      -->IV_HUIDENT     HU Identification
*      -->IV_DOCID       Document number (Delivery GUID)
*      -->IV_LGNUM       Warehouse Number
*      -->IT_DELIVERIES  Selected Deliveries (GUIDs)
*      -->IV_LOCK        Lock HU
*      -->CS_HUHDR       HU Header
*      -->CV_HAZMAT_IND  Hazardous Material Indicator
*---------------------------------------------------------------------*
FORM hu_read_and_lock
  USING    iv_huident     TYPE /scwm/de_huident
*          iv_docid       TYPE /scwm/de_docid
           iv_lgnum       TYPE /scwm/lgnum
           it_deliveries  TYPE /scwm/tt_rf_unlo_docid
           iv_lock        TYPE boole_d
  CHANGING cs_huhdr       TYPE /scwm/s_huhdr_int
           ct_huitm       TYPE /scwm/tt_huitm_int
           cv_hazmat_ind  TYPE char4
           cv_doccat       TYPE /scwm/de_doccat
           cv_docid       TYPE /scwm/de_docid.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
  DATA: ls_deliveries TYPE /scwm/s_rf_unlo_docid.           "#EC NEEDED

  DATA: lt_huref  TYPE /scwm/tt_huref_int,
        lv_exist TYPE xfeld.

  DATA: lo_packing TYPE REF TO /scwm/cl_wm_packing.

  FIELD-SYMBOLS <fs_huref> TYPE /scwm/s_huref_int.
*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*
  IF lo_packing IS NOT BOUND.
    CREATE OBJECT lo_packing.
  ENDIF.

* read all necessary data: HU status
  CALL METHOD lo_packing->get_hu
    EXPORTING
      iv_huident = iv_huident
      iv_lock    = iv_lock
    IMPORTING
      et_huitm   = ct_huitm
      es_huhdr   = cs_huhdr
      et_huref   = lt_huref
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

* does HU exist in given warehouse number and is HU top HU?
  IF  sy-subrc = 1
  OR cs_huhdr-top IS INITIAL
  OR cs_huhdr-lgnum <> iv_lgnum.
    MESSAGE e014(/scwm/rf_de) WITH iv_huident.
*   Handling Unit &1 ist not allowed
  ENDIF.

* is HU in predecessor document (delivery, door, TU, shipment)?
  SORT lt_huref BY guid_hu.

  CLEAR lv_exist.
  LOOP AT lt_huref ASSIGNING <fs_huref>
    WHERE  guid_hu = cs_huhdr-guid_hu.

    cv_doccat = <fs_huref>-doccat.
    cv_docid = <fs_huref>-docid.

    READ TABLE it_deliveries
         INTO ls_deliveries
         WITH KEY docid = <fs_huref>-docid.

    IF sy-subrc = 0.
      lv_exist = 'X'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF it_deliveries IS NOT INITIAL.
    IF lv_exist IS INITIAL.
      MESSAGE e014(/scwm/rf_de) WITH iv_huident.
*     Handling Unit &1 ist not allowed
    ELSE.
      MOVE: <fs_huref>-doccat TO cv_doccat,
            <fs_huref>-docid TO cv_docid.
    ENDIF.
  ENDIF.

* does HU contain hazardous material?
  CALL FUNCTION '/SCWM/RF_HAZMAT_IND_READ'
    EXPORTING
      iv_hazmat     = cs_huhdr-hzmt
    IMPORTING
      ev_hazmat_ind = cv_hazmat_ind.

ENDFORM.                    "hu_read_and_lock
