*----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_SSCRI02 .
**---------------------------------------------------------------------*
*  MODULE loop_input INPUT
*---------------------------------------------------------------------*
*  Save input made in step-loop
*---------------------------------------------------------------------*
MODULE loop_input INPUT.

  PERFORM loop_input.

ENDMODULE.                 " loop_input  INPUT
*&--------------------------------------------------------------------*
*&      Form  loop_input
*&--------------------------------------------------------------------*
FORM loop_input.

*- data
  DATA lv_index     TYPE i.
  DATA lv_line      TYPE i.
  DATA lv_tabname   TYPE tabname.   "#EC NEEDED
  DATA lv_fieldname TYPE fieldname.
  DATA lv_field(60) TYPE c.
  DATA lv_fcode     TYPE /scwm/de_fcode.   "#EC NEEDED
  DATA lv_loopc     TYPE i.

*- field-symbols
  FIELD-SYMBOLS: <lv>     TYPE ANY,
                 <lv_scr> TYPE ANY,
                 <ls_scr> TYPE ANY.

* Calculate index of current table line
  lv_index = /scwm/cl_rf_bll_srvc=>get_line( ) + sy-stepl - 1.
  READ TABLE <gt_scr> ASSIGNING <ls_scr> INDEX lv_index.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  LOOP AT SCREEN.
    CHECK screen-input = /scwm/cl_rf_dynpro_srvc=>c_attrib_on.
    ASSIGN (screen-name) TO <lv_scr>.
    SPLIT screen-name AT '-' INTO lv_tabname lv_fieldname.
    ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_scr> TO <lv>.
    <lv> = <lv_scr>.
  ENDLOOP.

* Check number of table lines
  lv_loopc = /scwm/cl_rf_dynpro_srvc=>get_loopc( ).
  if lv_loopc = 0.  "Step-loop but number of lines = 0 -> not good
    /scwm/cl_rf_dynpro_srvc=>set_loopc( sy-loopc ).
  endif.

* Save current line
  MODIFY <gt_scr> FROM <ls_scr> INDEX lv_index.

* Get cursor line
  GET CURSOR FIELD lv_field LINE lv_line.

  IF lv_field IS NOT INITIAL.
    /scwm/cl_rf_bll_srvc=>set_act_field( lv_field ).
  ENDIF.

  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode_setting( ).

  /scwm/cl_rf_bll_srvc=>set_cursor_line( lv_line ).

* Restore required attribute of the screen elements
  IF screen-group1 = /scwm/cl_rf_dynpro_srvc=>c_group1_required.
    screen-required = /scwm/cl_rf_dynpro_srvc=>c_attrib_on.
    ASSIGN (screen-name) TO <lv>.
    IF <lv> IS INITIAL AND
       /scwm/cl_rf_bll_srvc=>get_flg_input_required( ) =
       /scmb/cl_c=>boole_false.
      /scwm/cl_rf_bll_srvc=>set_flg_input_required( screen-name ).
    ENDIF.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                    "loop_input
