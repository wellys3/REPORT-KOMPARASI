**
**class lcl_fill_data implementation.
**
**  method ZFIIAMT_RECEIVE_REQ_KOMPARASI1.
**    call method ZFIIADT_RECEIVE_REQ_KOMPARASI3
**      importing
**        out = out-MT_RECEIVE_REQ_KOMPARASI_REQ.
**  endmethod.
**
**  method ZFIIADT_RECEIVE_REQ_KOMPARASI3.
**    call method ZFIIADT_RECEIVE_REQ_KOMPARASI1
**      importing
**        out = out.
**  endmethod.
**
**  method ZFIIADT_RECEIVE_REQ_KOMPARASI1.
**    out-BAL_ID = `String 1`. "#EC NOTEXT
**    out-BAL_LEVEL = `2 `. "#EC NOTEXT
**    out-UNIT_ID_LVL1 = `String 3`. "#EC NOTEXT
**    out-UNIT_ID_LVL2 = `String 4`. "#EC NOTEXT
**    out-UNIT_ID_LVL3 = `String 5`. "#EC NOTEXT
**    out-BAL_SOURCE = `String 6`. "#EC NOTEXT
**    out-BAL_DATE = `String 7`. "#EC NOTEXT
**  endmethod.
**
**  method ZFIIAMT_RECEIVE_REQ_KOMPARASI.
**    call method ZFIIADT_RECEIVE_REQ_KOMPARASI2
**      importing
**        out = out-MT_RECEIVE_REQ_KOMPARASI_RESP.
**  endmethod.
**
**  method ZFIIADT_RECEIVE_REQ_KOMPARASI2.
**    call method ZFIIADT_RECEIVE_REQ_KOMPARASI
**      importing
**        out = out-RETURN.
**  endmethod.
**
**  method ZFIIADT_RECEIVE_REQ_KOMPARASI.
**    out-TYPE = `S`. "#EC NOTEXT
**    out-ID = `String 9`. "#EC NOTEXT
**    out-NUMBER = `10 `. "#EC NOTEXT
**    out-MESSAGE = `String 11`. "#EC NOTEXT
**  endmethod.
**
**endclass.
