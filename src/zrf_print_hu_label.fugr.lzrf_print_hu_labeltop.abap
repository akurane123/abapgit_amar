FUNCTION-POOL zrf_print_hu_label.           "MESSAGE-ID ..

* INCLUDE LZRF_PRINT_HU_LABELD...            " Local class definition

TYPES: BEGIN OF gty_range,
         number_from TYPE char40,
         number_to   TYPE char40,
         time_req    TYPE dec15,
       END OF gty_range.

CONSTANTS: gc_gcp       TYPE char12 VALUE '031021',
           gc_ext_digit TYPE char1 VALUE '7',
           gc_plant     TYPE /sttpec/e_locno VALUE '2000',
           gc_custfield TYPE char20 VALUE'CREATE',
           gc_code_type TYPE char1 VALUE 'C',
           gc_enc_type  TYPE /sttpec/e_enc_type VALUE 'SSCC',
           gv_lgnum     TYPE /scwm/lgnum VALUE 'MWH1'.


DATA: gv_qty   TYPE int4,
      gv_lotno TYPE /sttpec/e_lotno,
      gv_matnr TYPE /sttpec/e_matnr,
      gv_datex TYPE /sttpec/e_datex.

DATA:  gs_docid_query TYPE /scwm/dlv_docid_item_str.
