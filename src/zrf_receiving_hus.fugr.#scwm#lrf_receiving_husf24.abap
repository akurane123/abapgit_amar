*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF24 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  barcode_decoding_HU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_REHU_HU  text
*----------------------------------------------------------------------*
FORM barcode_decoding_hu  CHANGING cs_rehu_hu TYPE /scwm/s_rf_rehu_hu.

  CALL FUNCTION '/SCWM/RF_REHU_EAN128_SPLIT'
      EXPORTING
        input               = cs_rehu_hu-vlenr_verif
     CHANGING
       cs_rehu_hu          = cs_rehu_hu.

ENDFORM.                    " barcode_decoding_HU
