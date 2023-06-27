FUNCTION-POOL zfi02fg0033.                  "MESSAGE-ID ..

CONSTANTS: gc_rbukrs TYPE bukrs VALUE 'ADMF'.

*--------------------------------------------------------------------*

TYPES: BEGIN OF gty_unit_id,
         unit_id TYPE zfidt00315-unit_id_level_1,
       END OF gty_unit_id.

TYPES: gtt_return TYPE TABLE OF bapiret2,
       gtt_unit_id TYPE TABLE OF gty_unit_id.

*--------------------------------------------------------------------*

DATA: gd_subrc TYPE sy-subrc,
      gd_message type text255.

*--------------------------------------------------------------------*
* Define                                                             *
*--------------------------------------------------------------------*
DEFINE f_fill_range.
  &1-sign = &2.
  &1-option = &3.
  &1-low = &4.
  &1-high = &5.
  APPEND &1.
END-OF-DEFINITION.

* INCLUDE LZFI02FG0033D...                   " Local class definition
