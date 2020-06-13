FUNCTION-POOL zrf_decomm_hu_label.          "MESSAGE-ID ..

CONSTANTS: gc_code_type   TYPE /sttpec/e_code_type VALUE 'C',
           gc_activity    TYPE /sttpec/e_cactivno VALUE '02',
           gc_decode_mode TYPE /sttpec/e_id_mode_dec VALUE 1,
           gc_mask        TYPE char4 VALUE '(00)'.

DATA :gc_param_cs_rehu_hu  TYPE /scwm/de_param_name VALUE 'CS_REHU_HU' .
* INCLUDE LZRF_DECOMM_HU_LABELD...           " Local class definition
