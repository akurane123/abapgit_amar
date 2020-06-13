*&---------------------------------------------------------------------*
*&      Form  delivery_status_set
*&---------------------------------------------------------------------*
*       Set delivery "status" by setting unloading start date/time
*----------------------------------------------------------------------*
*      -->IT_DELIVERIES  Selected Deliveries (GUIDs)
*      -->IV_DOCID       Delivery
*      -->IV_DOCCAT      Document Category (Inbound/Outbound)
*      -->IV_STATUS      Status to be set
*----------------------------------------------------------------------*
FORM delivery_status_set
  USING it_deliveries TYPE /scwm/tt_rf_unlo_docid
        iv_status     TYPE string
        iv_lgnum      TYPE /scwm/lgnum
        it_sdat_ini_dlv TYPE /scwm/tt_rf_unlo_docid.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
  DATA: lv_rejected TYPE boole_d,
        lv_qname    TYPE trfcqnam,
        lv_dlvs     TYPE i.

  DATA: ls_resource   TYPE /scwm/rsrc,
        ls_t_sr_gen   TYPE /scwm/t_sr_gen,
        lt_deliveries TYPE /scwm/tt_rf_unlo_docid.

*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*

* check the customize, whether start date should be stored or not
  CALL METHOD /scwm/cl_sr_customizing=>get_sr_gen
    EXPORTING
      iv_lgnum    = iv_lgnum
    RECEIVING
      es_t_sr_gen = ls_t_sr_gen.

  CHECK ls_t_sr_gen-begin_unlo_id = wmesr_upd_unlo_start_date.

* it_sdat_ini_dlv contains those dlvs which the startdate is initial
* if it_sdat_ini_dlv is initial, the use the got dlvs in it_deliveries
  IF it_sdat_ini_dlv IS INITIAL.
    lt_deliveries = it_deliveries.
  ELSE.
    lt_deliveries = it_sdat_ini_dlv.
  ENDIF.

  lv_dlvs = LINES( lt_deliveries ).

  IF lv_dlvs > gc_sync_dlv.
*   to much dlvs -> async update
    PERFORM async_dlv_status_set USING iv_lgnum
                                       iv_status
                                       lt_deliveries.
    COMMIT WORK AND WAIT.
    CALL METHOD /scwm/cl_tm=>cleanup( ).
  ELSE.
* try to update delivery synchronously
    CALL FUNCTION '/SCWM/RF_UL_STATUS_SET'
      EXPORTING
        iv_lgnum      = iv_lgnum
        iv_status     = iv_status
        iv_async      = ' '
      IMPORTING
        ev_rejected   = lv_rejected
      TABLES
        it_deliveries = lt_deliveries.

    IF lv_rejected IS NOT INITIAL.
*     sync update was not succesfull, do async update
      PERFORM async_dlv_status_set USING iv_lgnum
                                         iv_status
                                         lt_deliveries.
      COMMIT WORK AND WAIT.
      CALL METHOD /scwm/cl_tm=>cleanup( ).
    ELSE.
      COMMIT WORK AND WAIT.
      CALL METHOD /scwm/cl_tm=>cleanup( ).
    ENDIF.
  ENDIF.

ENDFORM.                    " delivery_status_set
*&---------------------------------------------------------------------*
*&      Form  async_dlv_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_LGNUM  text
*      -->P_IV_STATUS  text
*      -->P_LT_DELIVERIES  text
*----------------------------------------------------------------------*
FORM async_dlv_status_set  USING    iv_lgnum      TYPE /scwm/lgnum
                                    iv_status     TYPE string
                                    it_deliveries TYPE /scwm/tt_rf_unlo_docid.

  DATA: lv_qname    TYPE trfcqnam.

  DATA: ls_resource   TYPE /scwm/rsrc.


  CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
    EXPORTING
      iv_uname = sy-uname
    CHANGING
      cs_rsrc  = ls_resource.

*   get queue name by warehouse and resource
  CONCATENATE 'UL' iv_lgnum ls_resource-rsrc INTO lv_qname.
  CONDENSE lv_qname.

*   set queue name
  CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
    EXPORTING
      qname              = lv_qname
    EXCEPTIONS
      invalid_queue_name = 0.

*  set delivery status asynchronously
  CALL FUNCTION '/SCWM/RF_UL_STATUS_SET'
    IN BACKGROUND TASK AS SEPARATE UNIT
    EXPORTING
      iv_lgnum      = iv_lgnum
      iv_status     = iv_status
      iv_async      = 'X'
    TABLES
      it_deliveries = it_deliveries.

ENDFORM.                    " async_dlv_status_set
