*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_SPLIT_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_PRE_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GD_SUBRC
*&---------------------------------------------------------------------*
FORM f_pre_execute  CHANGING p_subrc.

  SELECT SINGLE * FROM tvarvc INTO @DATA(lwa_tvarvc)
    WHERE name EQ @gc_delay_per_batch_failed.
  IF sy-subrc EQ 0.
    gd_delay_batch_failed = lwa_tvarvc-low.
  ELSE.
    gd_delay_batch_failed = '002000'.
  ENDIF.

*--------------------------------------------------------------------*

  SELECT SINGLE * FROM tvarvc INTO lwa_tvarvc
    WHERE name EQ gc_delay_per_batch_success.
  IF sy-subrc EQ 0.
    gd_delay_batch_success = lwa_tvarvc-low.
  ELSE.
    gd_delay_batch_success = '000005'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_execute .

  DATA: ld_batch_seq_no          TYPE zfide01200,
        ld_batch_total           TYPE zfide01201,
*        ld_exist                 TYPE boolean,
        ld_succeed               TYPE boolean,
        ld_jobname               TYPE tbtco-jobname,
        ld_job_was_released      TYPE c,
        ld_batch_status          TYPE char20,

        lwa_zfist00191           TYPE zfist00191,
        lit_data_komparasi_lv1_3 TYPE TABLE OF zfist00170,
        lit_data_komparasi_lv4   TYPE TABLE OF zfist00171.

*--------------------------------------------------------------------*

  PERFORM f_start_timer.

  PERFORM f_get_data CHANGING ld_batch_total
                              git_zfidt00280[]
                              git_zfidt00281[].

  IF git_zfidt00280[] IS NOT INITIAL OR
     git_zfidt00281[] IS NOT INITIAL.

    CASE s_zlevel-low.
      WHEN 1 OR 2 OR 3.

        CLEAR lit_data_komparasi_lv1_3[].
        MOVE-CORRESPONDING git_zfidt00280[] TO lit_data_komparasi_lv1_3[].

        CALL FUNCTION 'ZFIFM_OB_SEND_KOMPARASI_LV1_3'
          TABLES
            it_komparasi = lit_data_komparasi_lv1_3[]
          CHANGING
            ch_return    = lwa_zfist00191.

        "*--------------------------------------------------------------------*
        "Internal Test Only, will be deleted soon

*        LOOP AT lit_data_komparasi_lv1_3 ASSIGNING FIELD-SYMBOL(<fs_data_komparasi_lvl_3>).
*          <fs_data_komparasi_lvl_3>-received_no = 'RN1'.
*        ENDLOOP.

        "*--------------------------------------------------------------------*

*        CLEAR ld_exist.
*        READ TABLE lit_data_komparasi_lv1_3 INTO DATA(lwa_data_komparasi_lv1_3) INDEX 1.
*        IF sy-subrc EQ 0.
*          IF lwa_data_komparasi_lv1_3-received_no IS NOT INITIAL.
*            ld_exist = 'X'.
*          ENDIF.
*        ENDIF.

*        IF lwa_zfist00191-received_no IS NOT INITIAL.
*          ld_exist = 'X'.
*        ENDIF.

        "*---
        "New Condition

        CLEAR ld_batch_status.
        IF lwa_zfist00191-message EQ 'OK'.
          ld_batch_status = 'NEXT BATCH'.
        ELSEIF lwa_zfist00191-message EQ 'jumlah data tidak sama'.
          ld_batch_status = 'REPEAT BATCH 1'.
        ELSE.
          ld_batch_status = 'REPEAT CURRENT BATCH'.
        ENDIF.

        "*--------------------------------------------------------------------*

        CLEAR gd_subrc.
        PERFORM f_update_status_zfidt00268    USING 'SENT'
                                                    sy-datum
                                                    sy-uzeit

*                                                    lwa_data_komparasi_lv1_3-received_no
                                                    lwa_zfist00191-received_no

                                                    s_rbukrs-low
                                                    p_datum
                                                    p_uzeit
                                                    s_recid-low
                                                    s_noref-low
                                                    s_zlevel-low
                                                    s_bsq-low
                                           CHANGING gd_subrc.

        "*--------------------------------------------------------------------*

        CLEAR ld_succeed.
*        IF ld_exist EQ 'X'.
        IF ld_batch_status EQ 'NEXT BATCH'.

          ld_succeed = 'X'.

          IF s_bsq-low = ld_batch_total.
            ld_batch_status = 'LAST BATCH'.
          ENDIF.

          CLEAR gd_message.
          CONCATENATE 'Batch Status:' ld_batch_status
            INTO gd_message
              SEPARATED BY space.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

        ELSE.

*          PERFORM f_progress_bar_single USING 'Received Number: <BLANK>' 'S' 'E'.

          "*--------------------------------------------------------------------*

          IF c_sw_bj EQ 'X'.

            "ld_batch_seq_no = s_bsq-low.
            "ld_batch_seq_no = 1.

            CASE ld_batch_status.
              WHEN 'REPEAT BATCH 1'.

                CLEAR gd_message.
                CONCATENATE 'Batch Status:' ld_batch_status
                  INTO gd_message
                    SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.

                PERFORM f_progress_bar_single USING 'Received Number: <BLANK>' 'S' 'E'.

                ld_batch_seq_no = 1.

                "Jika semisal ada 5 batch, pada batch ke-3 tidak terima 'Received No',
                "maka diulang dari awal yaitu batch ke-1
                PERFORM f_remove_received_no USING s_rbukrs-low
                                                   p_datum
                                                   p_uzeit
                                                   s_recid-low
                                                   s_noref-low
                                                   s_zlevel-low.

              WHEN 'REPEAT CURRENT BATCH'.

                CLEAR gd_message.
                CONCATENATE 'Batch Status:' ld_batch_status
                  INTO gd_message
                    SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.

                PERFORM f_progress_bar_single USING 'Received Number: <BLANK>' 'S' 'E'.

                ld_batch_seq_no = s_bsq-low.

            ENDCASE.

            PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                       p_uzeit

                                                       s_rbukrs-low
                                                       s_recid-low
                                                       s_zlevel-low
                                                       s_baldat-low
                                                       s_noref-low

                                                       ld_batch_seq_no
                                                       ld_batch_total

                                                       gd_delay_batch_failed
                                              CHANGING ld_job_was_released
                                                       ld_jobname.

            IF ld_job_was_released EQ 'X'.
              CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
              CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
              PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
            ELSE.
              CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
              PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
            ENDIF.

          ENDIF.

          "*--------------------------------------------------------------------*

          PERFORM f_stop_timer.
          "001: This program sucessfully executed! (Exec. Time & seconds)
          MESSAGE s071(zfimsg) WITH gd_run.

          "*--------------------------------------------------------------------*
          "Display to ALV & Log in SM37

          PERFORM f_display_data USING lit_data_komparasi_lv1_3[]
                                       lit_data_komparasi_lv4[].

          "*--------------------------------------------------------------------*

        ENDIF.

        IF ld_succeed EQ 'X'.

          CLEAR gd_message.
*          CONCATENATE 'Received Number:' lwa_data_komparasi_lv1_3-received_no INTO gd_message SEPARATED BY space.
          CONCATENATE 'Received Number:' lwa_zfist00191-received_no INTO gd_message SEPARATED BY space.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

          "*--------------------------------------------------------------------*

          LOOP AT lit_data_komparasi_lv1_3 ASSIGNING FIELD-SYMBOL(<lfs_data_komparasi_lv1_3>).
            <lfs_data_komparasi_lv1_3>-received_no = lwa_zfist00191-received_no.
          ENDLOOP.

          "*--------------------------------------------------------------------*

          IF s_bsq-low < ld_batch_total.

            IF c_sw_bj EQ 'X'.

              ld_batch_seq_no = s_bsq-low + 1.
              PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                         p_uzeit

                                                         s_rbukrs-low
                                                         s_recid-low
                                                         s_zlevel-low
                                                         s_baldat-low
                                                         s_noref-low

                                                         ld_batch_seq_no
                                                         ld_batch_total

                                                         gd_delay_batch_success
                                                CHANGING ld_job_was_released
                                                         ld_jobname.

              IF ld_job_was_released EQ 'X'.
                CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
                CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
              ELSE.
                CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
              ENDIF.

            ENDIF.

          ENDIF.

          "*--------------------------------------------------------------------*

          PERFORM f_stop_timer.
          "001: This program sucessfully executed! (Exec. Time & seconds)
          MESSAGE s071(zfimsg) WITH gd_run.

          "*--------------------------------------------------------------------*
          "Display to ALV & Log in SM37

          PERFORM f_display_data USING lit_data_komparasi_lv1_3[]
                                       lit_data_komparasi_lv4[].

          "*--------------------------------------------------------------------*

        ENDIF.

        "*--------------------------------------------------------------------*
        "*--------------------------------------------------------------------*
        "*--------------------------------------------------------------------*

      WHEN 4.

        "*--------------------------------------------------------------------*
        "*--------------------------------------------------------------------*
        "*--------------------------------------------------------------------*

        CLEAR lit_data_komparasi_lv4[].
        MOVE-CORRESPONDING git_zfidt00281[] TO lit_data_komparasi_lv4[].

        CALL FUNCTION 'ZFIFM_OB_SEND_KOMPARASI_LV4'
          TABLES
            it_komparasi = lit_data_komparasi_lv4[]
          CHANGING
            ch_return    = lwa_zfist00191.

        "*--------------------------------------------------------------------*

*        CLEAR ld_exist.
*        READ TABLE lit_data_komparasi_lv4 INTO DATA(lwa_data_komparasi_lv4) INDEX 1.
*        IF sy-subrc EQ 0.
*          IF lwa_data_komparasi_lv4-received_no IS NOT INITIAL.
*            ld_exist = 'X'.
*          ENDIF.
*        ENDIF.

*        IF lwa_zfist00191-received_no IS NOT INITIAL.
*          ld_exist = 'X'.
*        ENDIF.

        "*---
        "New Condition

        CLEAR ld_batch_status.
        IF lwa_zfist00191-message EQ 'OK'.
          ld_batch_status = 'NEXT BATCH'.
        ELSEIF lwa_zfist00191-message EQ 'jumlah data tidak sama'.
          ld_batch_status = 'REPEAT BATCH 1'.
        ELSE.
          ld_batch_status = 'REPEAT CURRENT BATCH'.
        ENDIF.

        "*--------------------------------------------------------------------*

        CLEAR gd_subrc.
        PERFORM f_update_status_zfidt00268    USING 'SENT'
                                                    sy-datum
                                                    sy-uzeit

*                                                    lwa_data_komparasi_lv4-received_no
                                                    lwa_zfist00191-received_no

                                                    s_rbukrs-low
                                                    p_datum
                                                    p_uzeit
                                                    s_recid-low
                                                    s_noref-low
                                                    s_zlevel-low
                                                    s_bsq-low
                                           CHANGING gd_subrc.

        "*--------------------------------------------------------------------*

        CLEAR ld_succeed.
*        IF ld_exist EQ 'X'.
        IF ld_batch_status EQ 'NEXT BATCH'.

          ld_succeed = 'X'.

          IF s_bsq-low = ld_batch_total.
            ld_batch_status = 'LAST BATCH'.
          ENDIF.

          CLEAR gd_message.
          CONCATENATE 'Batch Status:' ld_batch_status
            INTO gd_message
              SEPARATED BY space.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

        ELSE.

*          PERFORM f_progress_bar_single USING 'Received Number: <BLANK>' 'S' 'E'.

          "*--------------------------------------------------------------------*

          IF c_sw_bj EQ 'X'.

            "ld_batch_seq_no = s_bsq-low.
*              ld_batch_seq_no = 1.

            CASE ld_batch_status.
              WHEN 'REPEAT BATCH 1'.

                CLEAR gd_message.
                CONCATENATE 'Batch Status:' ld_batch_status
                  INTO gd_message
                    SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.

                PERFORM f_progress_bar_single USING 'Received Number: <BLANK>' 'S' 'E'.

                ld_batch_seq_no = 1.

                "Jika semisal ada 5 batch, pada batch ke-3 tidak terima 'Received No',
                "maka diulang dari awal yaitu batch ke-1
                PERFORM f_remove_received_no USING s_rbukrs-low
                                                   p_datum
                                                   p_uzeit
                                                   s_recid-low
                                                   s_noref-low
                                                   s_zlevel-low.

              WHEN 'REPEAT CURRENT BATCH'.

                CLEAR gd_message.
                CONCATENATE 'Batch Status:' ld_batch_status
                  INTO gd_message
                    SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.

                PERFORM f_progress_bar_single USING 'Received Number: <BLANK>' 'S' 'E'.

                ld_batch_seq_no = s_bsq-low.

            ENDCASE.

            PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                       p_uzeit

                                                       s_rbukrs-low
                                                       s_recid-low
                                                       s_zlevel-low
                                                       s_baldat-low
                                                       s_noref-low

                                                       ld_batch_seq_no
                                                       ld_batch_total

                                                       gd_delay_batch_failed
                                              CHANGING ld_job_was_released
                                                       ld_jobname.

            IF ld_job_was_released EQ 'X'.
              CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
              CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
              PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
            ELSE.
              CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
              PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
            ENDIF.

          ENDIF.

          "*--------------------------------------------------------------------*

          PERFORM f_stop_timer.
          "001: This program sucessfully executed! (Exec. Time & seconds)
          MESSAGE s071(zfimsg) WITH gd_run.

          "*--------------------------------------------------------------------*
          "Display to ALV & Log in SM37

          PERFORM f_display_data USING lit_data_komparasi_lv1_3[]
                                       lit_data_komparasi_lv4[].

          "*--------------------------------------------------------------------*

        ENDIF.

        "*--------------------------------------------------------------------*

        IF ld_succeed EQ 'X'.

          CLEAR gd_message.
*          CONCATENATE 'Received Number:' lwa_data_komparasi_lv1_3-received_no INTO gd_message SEPARATED BY space.
          CONCATENATE 'Received Number:' lwa_zfist00191-received_no INTO gd_message SEPARATED BY space.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

          "*--------------------------------------------------------------------*

          LOOP AT lit_data_komparasi_lv4 ASSIGNING FIELD-SYMBOL(<lfs_data_komparasi_lv4>).
            <lfs_data_komparasi_lv4>-received_no = lwa_zfist00191-received_no.
          ENDLOOP.

          "*--------------------------------------------------------------------*

          IF s_bsq-low < ld_batch_total.

            IF c_sw_bj EQ 'X'.

              ld_batch_seq_no = s_bsq-low + 1.
              PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                         p_uzeit

                                                         s_rbukrs-low
                                                         s_recid-low
                                                         s_zlevel-low
                                                         s_baldat-low
                                                         s_noref-low

                                                         ld_batch_seq_no
                                                         ld_batch_total

                                                         gd_delay_batch_success
                                                CHANGING ld_job_was_released
                                                         ld_jobname.

              IF ld_job_was_released EQ 'X'.
                CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
                CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
              ELSE.
                CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
              ENDIF.

            ENDIF.

          ENDIF.

          "*--------------------------------------------------------------------*

          PERFORM f_stop_timer.
          "001: This program sucessfully executed! (Exec. Time & seconds)
          MESSAGE s071(zfimsg) WITH gd_run.

          "*--------------------------------------------------------------------*
          "Display to ALV & Log in SM37

          PERFORM f_display_data USING lit_data_komparasi_lv1_3[]
                                       lit_data_komparasi_lv4[].

          "*--------------------------------------------------------------------*

        ENDIF.

    ENDCASE.

  ELSE.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_ZFIDT00280[]
*&      <-- GIT_ZFIDT00281[]
*&---------------------------------------------------------------------*
FORM f_get_data  CHANGING p_batch_total
                          p_git_zfidt00280 TYPE gtt_zfidt00280
                          p_git_zfidt00281 TYPE gtt_zfidt00281.

  CLEAR p_batch_total.

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

      CLEAR p_git_zfidt00280[].
      SELECT * FROM zfidt00280 INTO TABLE p_git_zfidt00280
        WHERE rbukrs IN s_rbukrs AND
              bal_id IN s_recid AND
              bal_date_b IN s_baldat AND
              bal_no_reff IN s_noref AND
              bal_level IN s_zlevel AND
              batch_seq_no IN s_bsq.

      READ TABLE p_git_zfidt00280 INTO DATA(lwa_zfidt00280) INDEX 1.
      IF sy-subrc EQ 0.
        p_batch_total = lwa_zfidt00280-batch_total.
      ENDIF.

    WHEN 4.

      CLEAR p_git_zfidt00281[].
      SELECT * FROM zfidt00281 INTO TABLE p_git_zfidt00281
        WHERE rbukrs IN s_rbukrs AND
              bal_id IN s_recid AND
              bal_date_b IN s_baldat AND
              bal_no_reff IN s_noref AND
              bal_level IN s_zlevel AND
              batch_seq_no IN s_bsq.

      READ TABLE p_git_zfidt00281 INTO DATA(lwa_zfidt00281) INDEX 1.
      IF sy-subrc EQ 0.
        p_batch_total = lwa_zfidt00281-batch_total.
      ENDIF.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_UPDATE_STATUS_ZFIDT00268
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_DATUM
*&      --> SY_UZEIT
*&---------------------------------------------------------------------*
FORM f_update_status_zfidt00268   USING    p_kind
                                           p_datum_system
                                           p_uzeit_system

                                           p_received_no

                                           p_rbukrs
                                           p_datum_param
                                           p_uzeit_param
                                           p_recon_id
                                           p_noref
                                           p_zlevel
                                           p_bsq
                                  CHANGING p_subrc.

  SELECT * FROM zfidt00268 INTO TABLE @DATA(lit_zfidt00268)
    WHERE rbukrs EQ @p_rbukrs AND
          date_entry EQ @p_datum_param AND
          time_entry EQ @p_uzeit_param AND
          recon_id EQ @p_recon_id AND
          bal_no_reff EQ @p_noref AND
          zlevel EQ @p_zlevel AND
          batch_seq_no EQ @p_bsq.

  IF lit_zfidt00268[] IS NOT INITIAL.

    LOOP AT lit_zfidt00268 ASSIGNING FIELD-SYMBOL(<fs_zfidt00268>).

      CASE p_kind.
        WHEN 'PROCESSED'.

          <fs_zfidt00268>-status_processed = 'DONE'.
          <fs_zfidt00268>-timestamp_processed = p_datum_system && p_uzeit_system.

        WHEN 'SENT'.

          <fs_zfidt00268>-status_sent = 'DONE'.
          <fs_zfidt00268>-timestamp_sent = p_datum_system && p_uzeit_system.
          <fs_zfidt00268>-received_no = p_received_no.

      ENDCASE.

    ENDLOOP.

    MODIFY zfidt00268 FROM TABLE lit_zfidt00268.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.

      CASE p_kind.
        WHEN 'PROCESSED'.
          PERFORM f_progress_bar_single USING 'PROCESSED: Update status successfully on table ZFIDT00268...' 'S' 'S'.
          p_subrc = 0.
        WHEN 'SENT'.
          PERFORM f_progress_bar_single USING 'SENT: Update status successfully on table ZFIDT00268...' 'S' 'S'.
          p_subrc = 0.
      ENDCASE.

    ELSE.

      CASE p_kind.
        WHEN 'PROCESSED'.
          PERFORM f_progress_bar_single USING 'PROCESSED: Update status failed on table ZFIDT00268...' 'S' 'E'.
          p_subrc = 1.
        WHEN 'SENT'.
          PERFORM f_progress_bar_single USING 'SENT: Update status failed on table ZFIDT00268...' 'S' 'E'.
          p_subrc = 1.
      ENDCASE.

    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CREATE_BJ_ZFI02R0032_SPLIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_DATUM
*&      --> P_UZEIT
*&      --> S_RBUKRS_LOW
*&      --> S_RECID_LOW
*&      --> S_ZLEVEL_LOW
*&      --> S_BALDAT_LOW
*&      --> S_NOREF_LOW
*&---------------------------------------------------------------------*
FORM f_create_bj_zfi02r0032_split  USING    p_datum
                                            p_uzeit

                                            p_rbukrs
                                            p_recid
                                            p_zlevel
                                            p_baldat
                                            p_noref

                                            p_bsq
                                            p_batch_total

                                            p_delay_batch
                                   CHANGING p_job_was_released
                                            p_jobname.

*--------------------------------------------------------------------*

  RANGES: lra_rbukrs FOR acdoca-rbukrs,
          lra_recon_id FOR zfidt00268-recon_id,
          lra_zlevel FOR zfidt00268-zlevel,
          lra_balance_date FOR zfidt00268-balance_date,
          lra_bal_no_reff FOR zfidt00268-bal_no_reff,
          lra_bsq FOR zfidt00280-batch_seq_no.

  DATA: ld_jobcount       TYPE tbtco-jobcount,
        ld_startdate      TYPE sy-datum,
        ld_starttime      TYPE sy-uzeit,
        ld_batch_first(2) TYPE n,
        ld_batch_last(2)  TYPE n,
        ld_addtime        TYPE sy-uzeit.

*--------------------------------------------------------------------*

  "Example Job Name: ZKOMP_20230321_050215_01/03

  ld_batch_first = p_bsq.
  ld_batch_last = p_batch_total.

  CLEAR gd_message.
  gd_message = ld_batch_first && '/' && ld_batch_last.

  CLEAR p_jobname.
  CONCATENATE 'ZKOMP'
              p_datum
              p_uzeit
*              p_recon_id
              gd_message
    INTO p_jobname
    SEPARATED BY '_'.

*--------------------------------------------------------------------*

  f_fill_range: lra_rbukrs 'I' 'EQ' p_rbukrs ''.
  f_fill_range: lra_recon_id 'I' 'EQ' p_recid ''.
  f_fill_range: lra_zlevel 'I' 'EQ' p_zlevel ''.
  f_fill_range: lra_balance_date 'I' 'EQ' p_baldat '00000000'.
  f_fill_range: lra_bal_no_reff 'I' 'EQ' p_noref ''.
  f_fill_range: lra_bsq 'I' 'EQ' p_bsq ''.

*--------------------------------------------------------------------*

  CLEAR: ld_jobcount.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*     DELANFREP        = ' '
      jobname          = p_jobname
    IMPORTING
      jobcount         = ld_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    "Implement suitable error handling here

    CLEAR gd_message.
    CONCATENATE 'Error Creating Job' p_jobname
      INTO gd_message SEPARATED BY space.
    MESSAGE s368(00) WITH gd_message sy-subrc.
    EXIT.
  ELSE.

    "Submit job with all the selection screen params...
    SUBMIT zfi02r0032_split
      WITH s_rbukrs IN lra_rbukrs
      WITH s_recid IN lra_recon_id
      WITH s_zlevel IN lra_zlevel
      WITH s_baldat IN lra_balance_date
*      WITH s_source IN lra_source
      WITH s_noref IN lra_bal_no_reff
      WITH s_bsq IN lra_bsq

      WITH p_datum EQ p_datum
      WITH p_uzeit EQ p_uzeit

      WITH c_sw_bj EQ 'X' "Checkbox Switch Background Job

        USER sy-uname
        VIA JOB p_jobname NUMBER ld_jobcount AND RETURN.

    IF sy-subrc = 0.

*      ld_addtime = '00' && p_delay_batch_failed && '00'.
      ld_addtime = p_delay_batch.

      CLEAR p_job_was_released.
      CALL FUNCTION 'EWU_ADD_TIME'
        EXPORTING
          i_starttime = sy-uzeit
          i_startdate = sy-datum
*         i_addtime   = '000005'
          i_addtime   = ld_addtime
        IMPORTING
          e_endtime   = ld_starttime
          e_enddate   = ld_startdate.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = ld_jobcount
          jobname              = p_jobname
          sdlstrtdt            = ld_startdate
          sdlstrttm            = ld_starttime
          strtimmed            = 'X'
        IMPORTING
          job_was_released     = p_job_was_released
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.

      IF syst-subrc <> 0.
        MESSAGE s162(00) WITH'An error occured while closing the background job.'.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_REMOVE_RECEIVED_NO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_RBUKRS_LOW
*&      --> P_DATUM
*&      --> P_UZEIT
*&      --> S_RECID_LOW
*&      --> S_NOREF_LOW
*&      --> S_ZLEVEL_LOW
*&---------------------------------------------------------------------*
FORM f_remove_received_no  USING    p_rbukrs
                                    p_datum_param
                                    p_uzeit_param
                                    p_recon_id
                                    p_noref
                                    p_zlevel.

  SELECT * FROM zfidt00268 INTO TABLE @DATA(lit_zfidt00268)
    WHERE rbukrs EQ @p_rbukrs AND
          date_entry EQ @p_datum_param AND
          time_entry EQ @p_uzeit_param AND
          recon_id EQ @p_recon_id AND
          bal_no_reff EQ @p_noref AND
          zlevel EQ @p_zlevel.
  IF lit_zfidt00268[] IS NOT INITIAL.

    LOOP AT lit_zfidt00268 ASSIGNING FIELD-SYMBOL(<lfs_zfidt00268>).
      <lfs_zfidt00268>-received_no = ''.
    ENDLOOP.

    MODIFY zfidt00268 FROM TABLE lit_zfidt00268.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDIF.

ENDFORM.
