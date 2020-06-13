FUNCTION-POOL zrf_pack_hu.                  "MESSAGE-ID ..

* INCLUDE LZRF_PACK_HUD...                   " Local class definition
TYPE-POOLS: wmegc.

DATA: gv_trtyp      TYPE /scwm/de_workst_trtyp,
      gv_cw_called  TYPE xfeld,
      gv_started_at TYPE /scwm/de_started_dt.

CONSTANTS:
  gc_fcode_fchuin     TYPE /scwm/de_fcode VALUE 'FCHUIN', "Input HU
  gc_fcode_fcwcin     TYPE /scwm/de_fcode VALUE 'FCWCIN', "Input workcenter
  gc_fcode_fcpmin     TYPE /scwm/de_fcode VALUE 'FCPMIN', "Input packmat
  gc_fcode_fcphin     TYPE /scwm/de_fcode VALUE 'FCPHIN', "Input putaway HU
  gc_fcode_fcnohu     TYPE /scwm/de_fcode VALUE 'FCNOHU', "Input packmat
  gc_fcode_fcexhu     TYPE /scwm/de_fcode VALUE 'FCEXHU', "Existing HU
  gc_fcode_fcexnr     TYPE /scwm/de_fcode VALUE 'FCEXNR', "External number
  gc_fcode_fcidin     TYPE /scwm/de_fcode VALUE 'FCIDIN', "Input ID
  gc_fcode_fcshin     TYPE /scwm/de_fcode VALUE 'FCSHIN', "Input spread HU
  gc_fcode_enter      TYPE /scwm/de_fcode VALUE 'ENTER',  "Enter
  gc_fcode_fcitdf     TYPE /scwm/de_fcode VALUE 'FCITDF', "HU Item Differen
  gc_fcode_fcitda     TYPE /scwm/de_fcode VALUE 'FCITDA', "Damaged Goods
  gc_fcode_fcsplt     TYPE /scwm/de_fcode VALUE 'FCSPLT', "HU Item Split
  gc_fcode_fccdst     TYPE /scwm/de_fcode VALUE 'FCCDST', "Change dest
  gc_fcode_fchucs     TYPE /scwm/de_fcode VALUE 'FCHUCS', "Create HU
  gc_fcode_fcitls     TYPE /scwm/de_fcode VALUE 'FCITLS', "HU item list
  gc_fcode_list       TYPE /scwm/de_fcode VALUE 'LIST',   "Exception code list quantity
  gc_fcode_diffhu     TYPE /scwm/de_fcode VALUE 'DIFFHU', "Exception code for HU differences
  gc_fcode_fcmed      TYPE /scwm/de_fcode VALUE 'FCCWME', "Mediator function for CW
  gc_fcode_callcw     TYPE /scwm/de_fcode VALUE 'FCCWCA', "Call CW function
  gc_fcode_pashin     TYPE /scwm/de_fcode VALUE 'PASHIN',
  gc_fcode_paidin     TYPE /scwm/de_fcode VALUE 'PAIDIN',
  gc_fcode_chng       TYPE /scwm/de_fcode VALUE 'CHNG',
  gc_fcode_save       TYPE /scwm/de_fcode VALUE 'SAVE',

  gc_step_pashin      TYPE /scwm/de_step VALUE 'PASHIN',
  gc_step_paidin      TYPE /scwm/de_step VALUE 'PAIDIN',
  gc_step_paphin      TYPE /scwm/de_step VALUE 'PAPHIN',
  gc_step_pahuin      TYPE /scwm/de_step VALUE 'PAHUIN',
  gc_step_papmin      TYPE /scwm/de_step VALUE 'PAPMIN',
  gc_step_patocr      TYPE /scwm/de_step VALUE 'PATOCR',
  gc_step_paitin      TYPE /scwm/de_step VALUE 'PAITIN',


  gc_ltrans_pahucs    TYPE /scwm/de_ltrans VALUE 'PAHUCS', "HU create aut
  gc_ltrans_pahumn    TYPE /scwm/de_ltrans VALUE 'PAHUMN', "HU create mn
  gc_ltrans_pahrpm    TYPE /scwm/de_ltrans VALUE 'PAHRPM', "HU repack mn
  gc_ltrans_pahrpa    TYPE /scwm/de_ltrans VALUE 'PAHRPA', "HU repack aut
  gc_ltrans_pairpa    TYPE /scwm/de_ltrans VALUE 'PAIRPA', "Itm repack aut
  gc_ltrans_pairpm    TYPE /scwm/de_ltrans VALUE 'PAIRPM', "Itm repack mn
  gc_ltrans_pairal    TYPE /scwm/de_ltrans VALUE 'PAIRAL', "Itm repack mn(all)

  gc_prmod_foregr     TYPE /scwm/de_prmod VALUE '2',
  gc_prmod_backgr     TYPE /scwm/de_prmod VALUE '1',

  gc_rf_de_msgid      TYPE symsgid VALUE '/SCWM/RF_DE',
  gc_msgty_success    TYPE symsgty VALUE 'S',

  gc_trtyp_stg        TYPE /scwm/de_workst_trtyp VALUE '7',
*   gc_trtyp_pac TYPE /scwm/de_workst_trtyp VALUE '1',

  gc_type_prod        TYPE /scwm/de_prosthu_type VALUE '01',
*   gc_type_ean TYPE /scwm/de_prosthu_type  VALUE '02',
  gc_type_stid        TYPE /scwm/de_prosthu_type VALUE '03',
  gc_type_shu         TYPE /scwm/de_prosthu_type  VALUE '04',

  gc_xfeld            TYPE xfeld   VALUE 'X',

  gc_scr_elmnt_pmatid TYPE /scwm/de_screlm_name VALUE
       '/SCWM/S_RF_PACK-PMATID',
  gc_scr_elmnt_ndifa  TYPE /scwm/de_screlm_name VALUE
        '/SCWM/S_RF_PACK-NDIFA',

  gc_huident_length   TYPE i VALUE 20,

  gc_pack01           TYPE char10 VALUE 'PACK', "Memory ID 1
  gc_pack02           TYPE char10 VALUE 'PACK2'. "Memory ID 2


INCLUDE /scwm/ui_desktop_type_include.

INCLUDE /scwm/lrf_packingf02.
INCLUDE /scwm/lrf_packingf06.
INCLUDE zrf_global_top.
INCLUDE zrf_global_f01.
