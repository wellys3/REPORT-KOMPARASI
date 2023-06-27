FUNCTION zfifm_ob_send_komparasi_lv1_3.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_KOMPARASI TYPE  ZFITT00170
*"  CHANGING
*"     VALUE(CH_RETURN) TYPE  ZFIST00191
*"----------------------------------------------------------------------

  DATA: lcl_proxy        TYPE REF TO zfioaco_si_komparasi_level1_3, "Class sproxy outbond
        lcl_fault        TYPE REF TO cx_ai_system_fault,            "Class Application Integration: Technical Error

        lwa_data_proxy_o TYPE zfioamt_komparasi_level1_3_req,  "Local work area header output proxy
*        lwa_item_proxy_i TYPE zfioadt_komparasi_level1_3_re3, "Local work area item output proxy
*        lwa_item       TYPE zfioadt_komparasi_level1_3_req,   "Local work area item

        lwa_data_proxy_i TYPE zfioamt_komparasi_level1_3_ib,  "Local work area header input proxy

        ld_i             TYPE i.

*--------------------------------------------------------------------*

  CLEAR ch_return.

*--------------------------------------------------------------------*
*Prepare Data

*****  LOOP AT it_komparasi INTO DATA(lwa_komparasi).
*****
*****    CLEAR lwa_item.
*****    MOVE-CORRESPONDING lwa_komparasi TO lwa_item.
*****
*****    APPEND lwa_item TO lwa_item_proxy-data.
*****
*****  ENDLOOP.
*****
*****  lwa_data_proxy-mt_komparasi_level1_3_req = lwa_item_proxy.

*--------------------------------------------------------------------*

*****  MOVE-CORRESPONDING it_komparasi[] TO lwa_item_proxy-data[].
*****  lwa_data_proxy-mt_komparasi_level1_3_req = lwa_item_proxy.

*--------------------------------------------------------------------*
*Preparing data

  MOVE-CORRESPONDING it_komparasi[] TO lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_json_data-data[].

  READ TABLE it_komparasi INTO DATA(lwa_komparasi) INDEX 1.
  IF sy-subrc EQ 0.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_rekon_id = lwa_komparasi-bal_id.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_noreff = lwa_komparasi-bal_no_reff.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_ball_level = lwa_komparasi-bal_level.

    CLEAR ld_i.
    ld_i = lwa_komparasi-row_total_per_batch.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_jml_data = ld_i.

    CLEAR ld_i.
    ld_i = lwa_komparasi-batch_seq_no.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_batch_no = ld_i.

    CLEAR ld_i.
    ld_i = lwa_komparasi-batch_total.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_batch_top = ld_i.

    CLEAR ld_i.
    ld_i = lwa_komparasi-row_total.
    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_total_data = ld_i.

    lwa_data_proxy_o-mt_komparasi_level1_3_req-ass_source = lwa_komparasi-bal_source.
  ENDIF.

*--------------------------------------------------------------------*
*Call Proxy

  TRY .
      CREATE OBJECT lcl_proxy.
      CREATE OBJECT lcl_fault.

      CALL METHOD lcl_proxy->si_komparasi_level1_3_ob
        EXPORTING
          output = lwa_data_proxy_o
        IMPORTING
          input  = lwa_data_proxy_i.

      COMMIT WORK.

    CATCH cx_ai_system_fault.
      WRITE :/ lcl_fault->errortext.
  ENDTRY.

*--------------------------------------------------------------------*
*Get result from proxy

  ch_return-bal_id = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_rekon_id.
  ch_return-bal_no_reff = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_no_reff.
  ch_return-bal_level = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_ball_level.
  ch_return-batch_seq_no = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_batch_no.
  ch_return-bal_source = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_source.
  ch_return-received_no = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_no_received.
  ch_return-row_total_per_batch_received = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-assjml_data_terima.
  ch_return-row_total_per_batch = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-data-ass_jml_data_aktual.
  ch_return-message = lwa_data_proxy_i-mt_komparasi_level1_3_ib_resp-data-message.

ENDFUNCTION.
