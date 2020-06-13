*FUNCTION-POOL zrf_pick_hu.                  "MESSAGE-ID ..

* INCLUDE LZRF_PICK_HUD...                   " Local class definition
INCLUDE /scwm/lrf_pickingtop.

INCLUDE /scwm/irf_sscr.
INCLUDE /scwm/lrf_pickingpbv.
INCLUDE /scwm/lrf_pickingf06.
INCLUDE /scwm/lrf_pickingf08.


INCLUDE zrf_global_top.
INCLUDE zrf_global_f01.


DATA: gt_objstruc     TYPE /sttpec/t_att_obj_hierarchy,
      gt_objstruc_qty TYPE /sttpec/t_att_qty_hierarchy,
      gt_objdata      TYPE /sttpec/t_att_obj_response.
