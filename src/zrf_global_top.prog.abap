*&---------------------------------------------------------------------*
*& Include /STTPEC/WHS_TEST_TOOLBOX_TOP     Module Pool      /STTPEC/WHS_TEST_TOOLBOX
*&
*&---------------------------------------------------------------------*
*& Description:  This program represents warehouse test UI.
*&---------------------------------------------------------------------*

CONTROLS tabstrip   TYPE TABSTRIP.

CONSTANTS:
  gc_cc_grhu         TYPE scrfname         VALUE 'GV_CC_GR',
  gc_cc_pick         TYPE scrfname         VALUE 'GV_CC_PICKITEM',
  gc_cc_pick2p       TYPE scrfname         VALUE 'GV_CC_PICK2P',
  gc_cc_create       TYPE scrfname         VALUE 'GV_CC_CREATEP',
  gc_cc_decommiss    TYPE scrfname         VALUE 'GV_CC_DECOMMISS',
  gc_cc_packhu       TYPE scrfname         VALUE 'GV_CC_PACKHU',
  gc_cc_packuom      TYPE scrfname         VALUE 'GV_CC_PACKUOM',
  gc_cc_unpack       TYPE scrfname         VALUE 'GV_CC_UNPACK',
  gc_cc_lothry       TYPE scrfname         VALUE 'GV_CC_LOTHRY',
  gc_cc_ship         TYPE scrfname         VALUE 'GV_CC_SHIP',
  gc_cc_identcont    TYPE scrfname         VALUE 'GV_CC_IDENTCONT',
  gc_cc_inspobj      TYPE scrfname         VALUE 'GV_CC_INSPECTOBJ',
  gc_cc_insphier     TYPE scrfname         VALUE 'GV_CC_INSPECTHIER',
  gc_cc_sample       TYPE scrfname         VALUE 'GV_CC_SAMPLING',
  gc_cc_counting     TYPE scrfname         VALUE 'GV_CC_COUNTING',
  gc_cc_transref     TYPE scrfname         VALUE 'GV_CC_TRANSREF',
  gc_cc_auth_request TYPE scrfname         VALUE 'GV_CC_AUTHREQUEST',
  gc_system_name     TYPE /sttpec/e_system_name VALUE 'X3NCLNT100',
  gc_res_list        TYPE tabname          VALUE '/STTPEC/S_WHS_TEST_TBOX_RESOBJ',


  BEGIN OF gc_tabstrip,
    tab_grhu            TYPE sy-ucomm VALUE 'GRHU',
    tab_putaway         TYPE sy-ucomm VALUE 'PUTAWAY',
    tab_pickitem        TYPE sy-ucomm VALUE 'PICKITEM',
    tab_pickhu          TYPE sy-ucomm VALUE 'PICKHU',
    tab_pick2p          TYPE sy-ucomm VALUE 'PICK2P',
    tab_createp         TYPE sy-ucomm VALUE 'CREATE',
    tab_decommiss       TYPE sy-ucomm VALUE 'DECOMMISS',
    tab_packhu          TYPE sy-ucomm VALUE 'PACKHU',
    tab_packuom         TYPE sy-ucomm VALUE 'PACKUOM',
    tab_load            TYPE sy-ucomm VALUE 'LOAD',
    tab_ship            TYPE sy-ucomm VALUE 'SHIP',
    tab_lothry          TYPE sy-ucomm VALUE 'LOTHRY',
    tab_unpack          TYPE sy-ucomm VALUE 'UNPACK',
    tab_identcont       TYPE sy-ucomm VALUE 'IDENTCONT',
    tab_count           TYPE sy-ucomm VALUE 'COUNT',
    tab_inspectobj      TYPE sy-ucomm VALUE 'INSPECTOBJ',
    tab_inspecthier     TYPE sy-ucomm VALUE 'INSPECTHIER',
    tab_sample          TYPE sy-ucomm VALUE 'SAMPLE',
    tab_transref        TYPE sy-ucomm VALUE 'DOCRELATION',
    tab_auth_request    TYPE sy-ucomm VALUE 'AUTHREQUEST',
    tab_request_control TYPE sy-ucomm VALUE 'CNTR',
  END OF gc_tabstrip.

TYPES:
  BEGIN OF ts_tree_nodes,
    code_urn TYPE string,
    node_key TYPE lvc_nkey,
  END OF ts_tree_nodes.

* global data declaration
DATA:
  BEGIN OF gs_main_tab,
    subscreen   TYPE sy-dynnr,
    prog        TYPE sy-repid VALUE sy-repid,
    pressed_tab TYPE sy-ucomm VALUE gc_tabstrip-tab_grhu,
  END OF gs_main_tab,

  gv_okcode                     TYPE        sy-ucomm,
* selection screen structures
  gs_tbox_fields                TYPE        /sttpec/s_whs_test_tbox_flds, "#EC NEEDED
  gs_tbox_grhu                  TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_putaway               TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_pickitem              TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_pickhu                TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_pick2p                TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_createp               TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_decommiss             TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_packhu                TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_packuom               TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_lothry                TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_unpack                TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_identcont             TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_load                  TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_ship                  TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_inspectobj            TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_inspecthier           TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_sampling              TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_transref              TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_auth_request          TYPE        /sttpec/s_whs_test_tbox_flds,
  gs_tbox_request_control       TYPE     /sttpec/s_whs_test_tbox_flds,
* Counting
  gs_tbox_counting              TYPE        /sttpec/s_whs_test_tbox_flds,
  gt_objdata_counting           TYPE        /sttpec/t_att_obj_insp, "#EC NEEDED
  gt_objqty_counting            TYPE        /sttpec/t_att_qty_hierarchy, "#EC NEEDED
* Document Lists
  gt_doc_objects                TYPE        /sttpec/t_whs_test_tbox_docobj, "#EC NEEDED
* ALV GRID
  go_grid_grhu                  TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_pick                  TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_pick2p                TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_create                TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_decommiss             TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_packhu                TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_packuom               TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_lothry                TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_unpack                TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_ship                  TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_identcont             TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_counting              TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_inspectobj            TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_inspecthier           TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_sample                TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_transref              TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
  go_grid_auth_request          TYPE REF TO cl_gui_alv_grid, "#EC NEEDED
* ALV TREE
  go_tree_counting              TYPE REF TO cl_gui_alv_tree, "#EC NEEDED
* GUI Custom Containers
  go_cc_grhu                    TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_pickitem                TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_pick2p                  TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_create                  TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_decommiss               TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_packhu                  TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_packuom                 TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_lothry                  TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_unpack                  TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_ship                    TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_identcont               TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_inspectobj              TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_inspecthier             TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_sampling                TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_counting                TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_transref                TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  go_cc_auth_request            TYPE REF TO cl_gui_custom_container, "#EC NEEDED
  gs_layout                     TYPE        lvc_s_layo,     "#EC NEEDED
  gt_fieldcat                   TYPE        lvc_t_fcat,     "#EC NEEDED
  gt_sort                       TYPE        lvc_t_sort,     "#EC NEEDED
  gs_variant_grhu               TYPE        disvariant,     "#EC NEEDED
  gs_variant_pick               TYPE        disvariant,     "#EC NEEDED
  gs_variant_pick2p             TYPE        disvariant,     "#EC NEEDED
  gs_variant_create             TYPE        disvariant,     "#EC NEEDED
  gs_variant_decommiss          TYPE        disvariant,     "#EC NEEDED
  gs_variant_pack               TYPE        disvariant,     "#EC NEEDED
  gs_variant_packuom            TYPE        disvariant,     "#EC NEEDED
  gs_variant_lothry             TYPE        disvariant,     "#EC NEEDED
  gs_variant_unpack             TYPE        disvariant,     "#EC NEEDED
  gs_variant_ship               TYPE        disvariant,     "#EC NEEDED
  gs_variant_identcont          TYPE        disvariant,     "#EC NEEDED
  gs_variant_counting           TYPE        disvariant,     "#EC NEEDED
  gs_variant_inspobj            TYPE        disvariant,     "#EC NEEDED
  gs_variant_insphier           TYPE        disvariant,     "#EC NEEDED
  gs_variant_sample             TYPE        disvariant,     "#EC NEEDED
  gs_variant_transref           TYPE        disvariant,     "#EC NEEDED
  gs_variant_auth_request       TYPE        disvariant,     "#EC NEEDED
  gs_variant_request_control    TYPE        disvariant,     "#EC NEEDED
* Item Lists
  gt_res_list                   TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_grhu_res_list              TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_pickitem_res_list          TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_pick2p_res_list            TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_create_res_list            TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_decommiss_res_list         TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_packhu_res_list            TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_packuom_res_list           TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_lothry_res_list            TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_unpack_res_list            TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_ship_res_list              TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_identcont_res_list         TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_counting_res_list          TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_inspobj_res_list           TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_insphier_res_list          TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_sampling_res_list          TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_transref_res_list          TYPE        /sttpec/t_whs_test_tbox_resobj, "#EC NEEDED
  gt_auth_request_res_list      TYPE TABLE OF     /sttpec/s_whs_test_authreq_ret, "#EC NEEDED
  gt_auth_request_res_list_full TYPE TABLE OF     /sttpec/s_whs_test_authreq_ret, "#EC NEEDED
  gv_disp_objcode1              TYPE flag,                  "#EC NEEDED
  gv_disp_objcode2              TYPE flag.                  "#EC NEEDED


* Local Data
DATA: gv_serial TYPE /sttpec/e_syncactive,
      gv_matnr  TYPE matnr.


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
           gc_enc_type  TYPE /sttpec/e_enc_type VALUE 'SSCC'.
