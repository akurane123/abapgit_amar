*----------------------------------------------------------------------*
***INCLUDE LZRF_OUTBOUNDF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_hu_attp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_hu_attp .

data : iv_activetab TYPE sy-ucomm,
       cs_tbox_fields  TYPE        /sttpec/s_whs_test_tbox_flds,
       ct_tbox_resobj     TYPE        /sttpec/t_whs_test_tbox_resobj.

iv_activetab = 'PICKHU'.
cs_tbox_fields-scan_obj = '(00)703102100000004637'.
cs_tbox_fields-docnum = '4000001380'.
cs_tbox_fields-docyear = '2020'.
cs_tbox_fields-logsys = 'H01CLNT100'.
cs_tbox_fields-plant = '2000'.
cs_tbox_fields-sscc = '703102100000004637'.

perform user_command_val USING    iv_activetab
                      CHANGING cs_tbox_fields
                               ct_tbox_resobj.

ENDFORM.
**&---------------------------------------------------------------------*
**& Form user_command_val
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> IV_ACTIVETAB
**&      --> TYPE
**&      --> SYUCOMM
**&      <-- CS_TBOX_FIELDS
**&      <-- TYPE
**&      <-- /STTPEC/S_WHS_TEST_TBOX_FLDS
**&      <-- CT_TBOX_RESOBJ
**&      <-- TYPE
**&      <-- /STTPEC/T_WHS_TEST_TBOX_RESOBJ
**&---------------------------------------------------------------------*
*FORM user_command_val  USING    iv_activetab     TYPE syucomm
*                      CHANGING cs_tbox_fields   TYPE /sttpec/s_whs_test_tbox_flds
*                               ct_tbox_resobj   TYPE /sttpec/t_whs_test_tbox_resobj.
*
*ENDFORM.
