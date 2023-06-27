class ZFIIACL_SI_RECEIVE_REQ_KOMPARA definition
  public
  create public .

public section.

  interfaces ZFIIAII_SI_RECEIVE_REQ_KOMPARA .
protected section.
private section.
ENDCLASS.



CLASS ZFIIACL_SI_RECEIVE_REQ_KOMPARA IMPLEMENTATION.


METHOD zfiiaii_si_receive_req_kompara~si_receive_req_komparasi_ib.

  DATA : lwa_req    TYPE zfist00169,
         lit_return TYPE TABLE OF bapiret2,
         lwa_return TYPE bapiret2.

*--------------------------------------------------------------------*

  CLEAR lwa_req.
*  MOVE-CORRESPONDING input-mt_receive_req_komparasi_req-data TO lwa_req.
*  MOVE-CORRESPONDING input-mt_receive_req_komparasi_req TO lwa_req.

  lwa_req-bal_id = input-mt_receive_req_komparasi_req-bal_id.
  lwa_req-bal_level = input-mt_receive_req_komparasi_req-bal_level.
  lwa_req-bal_date = input-mt_receive_req_komparasi_req-bal_date.
  lwa_req-bal_request_no = input-mt_receive_req_komparasi_req-bal_request_no.
  MOVE-CORRESPONDING input-mt_receive_req_komparasi_req-item_bal_unit_detail[] TO lwa_req-item_bal_unit_detail[].
  MOVE-CORRESPONDING input-mt_receive_req_komparasi_req-item_unit_lvl[] TO lwa_req-item_unit_lvl[].
  lwa_req-bal_source = input-mt_receive_req_komparasi_req-bal_source.

  CALL FUNCTION 'ZFIFM_IB_RECEIVE_REQ_KOMPARASI'
    EXPORTING
      im_req    = lwa_req
    IMPORTING
      ex_return = lwa_return.

*--------------------------------------------------------------------*

  MOVE-CORRESPONDING lwa_return TO output-mt_receive_req_komparasi_resp-return.

*  CALL METHOD lcl_fill_data=>zfiiamt_receive_req_komparasi
*    IMPORTING
*      out = output.

ENDMETHOD.
ENDCLASS.
