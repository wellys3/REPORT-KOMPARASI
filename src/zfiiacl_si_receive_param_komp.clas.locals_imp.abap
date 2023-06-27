
class lcl_fill_data implementation.

  method ZFIIAMT_RECEIVE_PARAM_KOMP_REQ.
    call method ZFIIADT_RECEIVE_PARAM_KOMP_REQ
      importing
        out = out-MT_RECEIVE_PARAM_KOMP_REQ.
  endmethod.

  method ZFIIADT_RECEIVE_PARAM_KOMP_REQ.
    call method ZFIIADT_RECEIVE_PARAM_KOMP_TAB
      importing
        out = out-DATA.
  endmethod.

  method ZFIIADT_RECEIVE_PARAM_KOMP_TAB.
    data: ls_out like line of out.
    call method ZFIIADT_RECEIVE_PARAM_KOMP_RE2
      importing
        out = ls_out.
    do 3 times.
      append ls_out to out.
    enddo.
  endmethod.

  method ZFIIADT_RECEIVE_PARAM_KOMP_RE2.
    out-RECON_ID = `String 1`. "#EC NOTEXT
    out-LEVEL_1 = `String 2`. "#EC NOTEXT
    out-LEVEL_2 = `String 3`. "#EC NOTEXT
    out-LEVEL_3 = `String 4`. "#EC NOTEXT
  endmethod.

  method ZFIIAMT_RECEIVE_PARAM_KOMP_RES.
    call method ZFIIADT_RECEIVE_PARAM_KOMP_RE1
      importing
        out = out-MT_RECEIVE_PARAM_KOMP_RESP.
  endmethod.

  method ZFIIADT_RECEIVE_PARAM_KOMP_RE1.
    call method ZFIIADT_RECEIVE_PARAM_KOM_TAB1
      importing
        out = out-RETURN.
  endmethod.

  method ZFIIADT_RECEIVE_PARAM_KOM_TAB1.
    data: ls_out like line of out.
    call method ZFIIADT_RECEIVE_PARAM_KOMP_RES
      importing
        out = ls_out.
    do 3 times.
      append ls_out to out.
    enddo.
  endmethod.

  method ZFIIADT_RECEIVE_PARAM_KOMP_RES.
    out-TYPE = `S`. "#EC NOTEXT
    out-ID = `String 6`. "#EC NOTEXT
    out-NUMBER = `7 `. "#EC NOTEXT
    out-MESSAGE = `String 8`. "#EC NOTEXT
  endmethod.

endclass.

