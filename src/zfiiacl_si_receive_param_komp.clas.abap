class ZFIIACL_SI_RECEIVE_PARAM_KOMP definition
  public
  create public .

public section.

  interfaces ZFIIAII_SI_RECEIVE_PARAM_KOMP .
protected section.
private section.
ENDCLASS.



CLASS ZFIIACL_SI_RECEIVE_PARAM_KOMP IMPLEMENTATION.


METHOD zfiiaii_si_receive_param_komp~si_receive_param_komp_ib.

  DATA : lit_param  TYPE TABLE  OF zfist00168,
         lit_return TYPE TABLE OF bapiret2,
         lwa_return TYPE bapiret2.

*--------------------------------------------------------------------*

  MOVE-CORRESPONDING input-mt_receive_param_komp_req-data[] TO lit_param[].

  CALL FUNCTION 'ZFIFM_IB_RECEIVE_PARAM_KOMP'
    TABLES
      it_param  = lit_param
      it_return = lit_return.

*--------------------------------------------------------------------*

  MOVE-CORRESPONDING lit_return TO output-mt_receive_param_komp_resp-return[].

*  CALL METHOD lcl_fill_data=>zfiiamt_receive_param_komp_res
*    IMPORTING
*      out = output.

ENDMETHOD.
ENDCLASS.
