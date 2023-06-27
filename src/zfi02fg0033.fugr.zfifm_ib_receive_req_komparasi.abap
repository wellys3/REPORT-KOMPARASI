FUNCTION zfifm_ib_receive_req_komparasi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_REQ) TYPE  ZFIST00169
*"  EXPORTING
*"     VALUE(EX_RETURN) LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: ld_datum            TYPE sy-datum,
        ld_uzeit            TYPE sy-uzeit,
        ld_datum_str        TYPE char10,
        ld_uzeit_str        TYPE char10,
        ld_job_was_released TYPE c,
        ld_jobname          TYPE tbtco-jobname,
        ld_bal_date         TYPE zfide01112,
        ld_seqno            TYPE zfide01205,

        lit_zfidt00268      TYPE TABLE OF zfidt00268,
        lwa_zfidt00268      TYPE zfidt00268,
        lit_zfidt00315      TYPE TABLE OF zfidt00315,
        lwa_zfidt00315      TYPE zfidt00315,

        lwa_unit_id_lvl     TYPE gty_unit_id,
        lit_unit_id_lvl_1   TYPE TABLE OF gty_unit_id,
        lit_unit_id_lvl_2   TYPE TABLE OF gty_unit_id,
        lit_unit_id_lvl_3   TYPE TABLE OF gty_unit_id.

*--------------------------------------------------------------------*

  CLEAR:  ld_datum,
          ld_uzeit,
          ld_datum_str,
          ld_uzeit_str,

          lit_zfidt00268[],
          lit_zfidt00315[],
          lit_unit_id_lvl_1[],
          lit_unit_id_lvl_2[],
          lit_unit_id_lvl_3[].

*--------------------------------------------------------------------*
* Generate time

  ld_datum = sy-datum.
  ld_uzeit = sy-uzeit.

  ld_bal_date = im_req-bal_date - 1.

  PERFORM f_convert_date_with_separator  USING ld_datum
                                               '.'
                                      CHANGING ld_datum_str.

  PERFORM f_convert_time_with_separator  USING ld_uzeit
                                               '.'
                                      CHANGING ld_uzeit_str.

*--------------------------------------------------------------------*

  CASE im_req-bal_level.
    WHEN 1.

      ADD 1 TO ld_seqno.

      CLEAR lwa_zfidt00268.
      lwa_zfidt00268-rbukrs = gc_rbukrs.
      lwa_zfidt00268-date_entry = ld_datum.
      lwa_zfidt00268-time_entry = ld_uzeit.
      lwa_zfidt00268-recon_id = im_req-bal_id.

*      TRY.
*          lwa_zfidt00268-guid = cl_system_uuid=>create_uuid_x16_static( ).
*        CATCH cx_uuid_error.
*      ENDTRY.

      lwa_zfidt00268-bal_no_reff = im_req-bal_request_no.
      lwa_zfidt00268-zlevel = im_req-bal_level.

*      lwa_zfidt00268-seqno = ld_seqno.

*      lwa_zfidt00268-unit_id_level_1 = 'NULL'.
*      lwa_zfidt00268-unit_id_level_2 = 'NULL'.
*      lwa_zfidt00268-unit_id_level_3 = 'NULL'.
      lwa_zfidt00268-source = im_req-bal_source.
      lwa_zfidt00268-balance_date_b = im_req-bal_date.
      lwa_zfidt00268-balance_date = ld_bal_date.

      APPEND lwa_zfidt00268 TO lit_zfidt00268.

      "*--------------------------------------------------------------------*

      CLEAR lwa_zfidt00315.
      lwa_zfidt00315-rbukrs = gc_rbukrs.
      lwa_zfidt00315-date_entry = ld_datum.
      lwa_zfidt00315-time_entry = ld_uzeit.
      lwa_zfidt00315-recon_id = im_req-bal_id.
      lwa_zfidt00315-bal_no_reff = im_req-bal_request_no.
      lwa_zfidt00315-zlevel = im_req-bal_level.
      lwa_zfidt00315-seqno = ld_seqno.

      lwa_zfidt00315-unit_id_level_1 = 'NULL'.
      lwa_zfidt00315-unit_id_level_2 = 'NULL'.
      lwa_zfidt00315-unit_id_level_3 = 'NULL'.
      APPEND lwa_zfidt00315 TO lit_zfidt00315.

    WHEN 2.


      LOOP AT im_req-item_bal_unit_detail INTO DATA(lwa_item_bal_unit_detail).

        ADD 1 TO ld_seqno.

        CLEAR lwa_zfidt00268.
        lwa_zfidt00268-rbukrs = gc_rbukrs.
        lwa_zfidt00268-date_entry = ld_datum.
        lwa_zfidt00268-time_entry = ld_uzeit.
        lwa_zfidt00268-recon_id = im_req-bal_id.

*        TRY.
*            lwa_zfidt00268-guid = cl_system_uuid=>create_uuid_x16_static( ).
*          CATCH cx_uuid_error.
*        ENDTRY.

        lwa_zfidt00268-bal_no_reff = im_req-bal_request_no.
        lwa_zfidt00268-zlevel = im_req-bal_level.

*        lwa_zfidt00268-seqno = ld_seqno.

*        lwa_zfidt00268-unit_id_level_1 = lwa_item_bal_unit_detail-unit_id_lvl1.
*        lwa_zfidt00268-unit_id_level_2 = 'NULL'.
*        lwa_zfidt00268-unit_id_level_3 = 'NULL'.

        lwa_zfidt00268-source = im_req-bal_source.
        lwa_zfidt00268-balance_date_b = im_req-bal_date.
        lwa_zfidt00268-balance_date = ld_bal_date.

        APPEND lwa_zfidt00268 TO lit_zfidt00268.

        "*--------------------------------------------------------------------*

        CLEAR lwa_zfidt00315.
        lwa_zfidt00315-rbukrs = gc_rbukrs.
        lwa_zfidt00315-date_entry = ld_datum.
        lwa_zfidt00315-time_entry = ld_uzeit.
        lwa_zfidt00315-recon_id = im_req-bal_id.
        lwa_zfidt00315-bal_no_reff = im_req-bal_request_no.
        lwa_zfidt00315-zlevel = im_req-bal_level.
        lwa_zfidt00315-seqno = ld_seqno.

        lwa_zfidt00315-unit_id_level_1 = lwa_item_bal_unit_detail-unit_id_lvl1.
        lwa_zfidt00315-unit_id_level_2 = 'NULL'.
        lwa_zfidt00315-unit_id_level_3 = 'NULL'.
        APPEND lwa_zfidt00315 TO lit_zfidt00315.

        "*--------------------------------------------------------------------*

        CLEAR lwa_unit_id_lvl.
        lwa_unit_id_lvl-unit_id = lwa_item_bal_unit_detail-unit_id_lvl1.
        APPEND lwa_unit_id_lvl TO lit_unit_id_lvl_1.

      ENDLOOP.

    WHEN 3.

      LOOP AT im_req-item_bal_unit_detail INTO lwa_item_bal_unit_detail.

        ADD 1 TO ld_seqno.

        CLEAR lwa_zfidt00268.
        lwa_zfidt00268-rbukrs = gc_rbukrs.
        lwa_zfidt00268-date_entry = ld_datum.
        lwa_zfidt00268-time_entry = ld_uzeit.
        lwa_zfidt00268-recon_id = im_req-bal_id.

*        TRY.
*            lwa_zfidt00268-guid = cl_system_uuid=>create_uuid_x16_static( ).
*          CATCH cx_uuid_error.
*        ENDTRY.

        lwa_zfidt00268-bal_no_reff = im_req-bal_request_no.
        lwa_zfidt00268-zlevel = im_req-bal_level.

*        lwa_zfidt00268-seqno = ld_seqno.

*        lwa_zfidt00268-unit_id_level_1 = lwa_item_bal_unit_detail-unit_id_lvl1.
*        lwa_zfidt00268-unit_id_level_2 = lwa_item_bal_unit_detail-unit_id_lvl2.
*        lwa_zfidt00268-unit_id_level_3 = 'NULL'.

        lwa_zfidt00268-source = im_req-bal_source.
        lwa_zfidt00268-balance_date_b = im_req-bal_date.
        lwa_zfidt00268-balance_date = ld_bal_date.

        APPEND lwa_zfidt00268 TO lit_zfidt00268.

        "*--------------------------------------------------------------------*

        CLEAR lwa_zfidt00315.
        lwa_zfidt00315-rbukrs = gc_rbukrs.
        lwa_zfidt00315-date_entry = ld_datum.
        lwa_zfidt00315-time_entry = ld_uzeit.
        lwa_zfidt00315-recon_id = im_req-bal_id.
        lwa_zfidt00315-bal_no_reff = im_req-bal_request_no.
        lwa_zfidt00315-zlevel = im_req-bal_level.
        lwa_zfidt00315-seqno = ld_seqno.

        lwa_zfidt00315-unit_id_level_1 = lwa_item_bal_unit_detail-unit_id_lvl1.
        lwa_zfidt00315-unit_id_level_2 = lwa_item_bal_unit_detail-unit_id_lvl2.
        lwa_zfidt00315-unit_id_level_3 = 'NULL'.
        APPEND lwa_zfidt00315 TO lit_zfidt00315.

        "*--------------------------------------------------------------------*

        CLEAR lwa_unit_id_lvl.
        lwa_unit_id_lvl-unit_id = lwa_item_bal_unit_detail-unit_id_lvl1.
        APPEND lwa_unit_id_lvl TO lit_unit_id_lvl_1.

        CLEAR lwa_unit_id_lvl.
        lwa_unit_id_lvl-unit_id = lwa_item_bal_unit_detail-unit_id_lvl2.
        APPEND lwa_unit_id_lvl TO lit_unit_id_lvl_2.

      ENDLOOP.

    WHEN 4.

      LOOP AT im_req-item_bal_unit_detail INTO lwa_item_bal_unit_detail.
*      LOOP AT im_req-item_unit_lvl INTO DATA(lwa_item_unit_lvl).

        ADD 1 TO ld_seqno.

        CLEAR lwa_zfidt00268.
        lwa_zfidt00268-rbukrs = gc_rbukrs.
        lwa_zfidt00268-date_entry = ld_datum.
        lwa_zfidt00268-time_entry = ld_uzeit.
        lwa_zfidt00268-recon_id = im_req-bal_id.

*        TRY.
*            lwa_zfidt00268-guid = cl_system_uuid=>create_uuid_x16_static( ).
*          CATCH cx_uuid_error.
*        ENDTRY.

        lwa_zfidt00268-bal_no_reff = im_req-bal_request_no.
        lwa_zfidt00268-zlevel = im_req-bal_level.

*        lwa_zfidt00268-seqno = ld_seqno.

*        lwa_zfidt00268-unit_id_level_1 = lwa_item_unit_lvl-unit_id_lvl1.
*        lwa_zfidt00268-unit_id_level_2 = lwa_item_unit_lvl-unit_id_lvl2.
*        lwa_zfidt00268-unit_id_level_3 = lwa_item_unit_lvl-unit_id_lvl3.

*        lwa_zfidt00268-unit_id_level_1 = lwa_item_bal_unit_detail-unit_id_lvl1.
*        lwa_zfidt00268-unit_id_level_2 = lwa_item_bal_unit_detail-unit_id_lvl2.
*        lwa_zfidt00268-unit_id_level_3 = lwa_item_bal_unit_detail-unit_id_lvl3.

        lwa_zfidt00268-source = im_req-bal_source.
        lwa_zfidt00268-balance_date_b = im_req-bal_date.
        lwa_zfidt00268-balance_date = ld_bal_date.

        APPEND lwa_zfidt00268 TO lit_zfidt00268.

        "*--------------------------------------------------------------------*

        CLEAR lwa_zfidt00315.
        lwa_zfidt00315-rbukrs = gc_rbukrs.
        lwa_zfidt00315-date_entry = ld_datum.
        lwa_zfidt00315-time_entry = ld_uzeit.
        lwa_zfidt00315-recon_id = im_req-bal_id.
        lwa_zfidt00315-bal_no_reff = im_req-bal_request_no.
        lwa_zfidt00315-zlevel = im_req-bal_level.
        lwa_zfidt00315-seqno = ld_seqno.

*        lwa_zfidt00315-unit_id_level_1 = lwa_item_unit_lvl-unit_id_lvl1.
*        lwa_zfidt00315-unit_id_level_2 = lwa_item_unit_lvl-unit_id_lvl2.
*        lwa_zfidt00315-unit_id_level_3 = lwa_item_unit_lvl-unit_id_lvl3.

        lwa_zfidt00315-unit_id_level_1 = lwa_item_bal_unit_detail-unit_id_lvl1.
        lwa_zfidt00315-unit_id_level_2 = lwa_item_bal_unit_detail-unit_id_lvl2.
        lwa_zfidt00315-unit_id_level_3 = lwa_item_bal_unit_detail-unit_id_lvl3.

        APPEND lwa_zfidt00315 TO lit_zfidt00315.

        "*--------------------------------------------------------------------*

        CLEAR lwa_unit_id_lvl.
*        lwa_unit_id_lvl-unit_id = lwa_item_unit_lvl-unit_id_lvl1.
        lwa_unit_id_lvl-unit_id = lwa_item_bal_unit_detail-unit_id_lvl1.
        APPEND lwa_unit_id_lvl TO lit_unit_id_lvl_1.

        CLEAR lwa_unit_id_lvl.
*        lwa_unit_id_lvl-unit_id = lwa_item_unit_lvl-unit_id_lvl2.
        lwa_unit_id_lvl-unit_id = lwa_item_bal_unit_detail-unit_id_lvl2.
        APPEND lwa_unit_id_lvl TO lit_unit_id_lvl_2.

        CLEAR lwa_unit_id_lvl.
*        lwa_unit_id_lvl-unit_id = lwa_item_unit_lvl-unit_id_lvl3.
        lwa_unit_id_lvl-unit_id = lwa_item_bal_unit_detail-unit_id_lvl3.
        APPEND lwa_unit_id_lvl TO lit_unit_id_lvl_3.

      ENDLOOP.

  ENDCASE.

*--------------------------------------------------------------------*

  IF lit_zfidt00268[] IS NOT INITIAL.

    MODIFY zfidt00315 FROM TABLE lit_zfidt00315.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      PERFORM f_insert_message_b    USING 'E'
                                          'Detail'
                                          im_req-bal_id
                                          'Failed to save'
                                          'on'
                                          ld_datum_str
                                          ld_uzeit_str
                                 CHANGING ex_return.
      EXIT.
    ENDIF.

    "*--------------------------------------------------------------------*

    MODIFY zfidt00268 FROM TABLE lit_zfidt00268.
    IF sy-subrc EQ 0.

      COMMIT WORK AND WAIT.

      PERFORM f_create_bj_zfi02r0032 USING ld_datum
                                           ld_uzeit
                                           gc_rbukrs
                                           im_req-bal_id
                                           im_req-bal_level
                                           im_req-bal_date
                                           im_req-bal_source
                                           im_req-bal_request_no
                                           lit_unit_id_lvl_1[]
                                           lit_unit_id_lvl_2[]
                                           lit_unit_id_lvl_3[]
                                  CHANGING ld_job_was_released
                                           ld_jobname.

      CLEAR gd_message.
      IF ld_job_was_released EQ 'X'.
        CONCATENATE ld_jobname 'successfully'
          INTO gd_message SEPARATED BY space.
        CONCATENATE 'Successfully saved and created Background Job' gd_message
          INTO gd_message SEPARATED BY space.
      ELSE.
        CONCATENATE 'Successfully saved, but failed to create Background Job' ld_jobname
          INTO gd_message SEPARATED BY space.
      ENDIF.

      PERFORM f_insert_message_b    USING 'S'
                                          'Header & Detail'
                                          im_req-bal_id
                                          gd_message
                                          'on'
                                          ld_datum_str
                                          ld_uzeit_str
                                 CHANGING ex_return.
*                                          it_return[].

    ELSE.

      PERFORM f_insert_message_b    USING 'E'
                                          'Header'
                                          im_req-bal_id
                                          'Failed to save'
                                          'on'
                                          ld_datum_str
                                          ld_uzeit_str
                                 CHANGING ex_return.

    ENDIF.

  ENDIF.

ENDFUNCTION.


*&---------------------------------------------------------------------*
*& Form F_CREATE_BJ_ZFI02R0032
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_DATUM
*&      --> LD_UZEIT
*&---------------------------------------------------------------------*
FORM f_create_bj_zfi02r0032  USING    p_datum
                                      p_uzeit
                                      p_rbukrs
                                      p_recon_id
                                      p_zlevel
                                      p_balance_date
                                      p_source
                                      p_bal_no_reff
                                      p_lit_unit_id_lvl_1 TYPE gtt_unit_id
                                      p_lit_unit_id_lvl_2 TYPE gtt_unit_id
                                      p_lit_unit_id_lvl_3 TYPE gtt_unit_id
                             CHANGING p_job_was_released
                                      p_jobname.

  RANGES: lra_rbukrs FOR acdoca-rbukrs,
          lra_recon_id FOR zfidt00268-recon_id,
          lra_zlevel FOR zfidt00268-zlevel,
          lra_balance_date FOR zfidt00268-balance_date,
          lra_source FOR zfidt00268-source,
          lra_bal_no_reff FOR zfidt00268-bal_no_reff,
          lra_unit_id_lvl_1 FOR zfidt00315-unit_id_level_1,
          lra_unit_id_lvl_2 FOR zfidt00315-unit_id_level_2,
          lra_unit_id_lvl_3 FOR zfidt00315-unit_id_level_3.

  DATA: ld_jobcount  TYPE tbtco-jobcount,
        ld_startdate TYPE sy-datum,
        ld_starttime TYPE sy-uzeit.

*--------------------------------------------------------------------*
  "Example: ZFI02R0032_20230321_050219_AR1
  "Example: ZKOMP_20230321_050219_AR1

  CLEAR p_jobname.
*  CONCATENATE 'ZFI02R0032'
  CONCATENATE 'ZKOMP'
              p_datum
              p_uzeit
              p_recon_id
    INTO p_jobname
    SEPARATED BY '_'.

*--------------------------------------------------------------------*

  f_fill_range: lra_rbukrs 'I' 'EQ' p_rbukrs ''.
  f_fill_range: lra_recon_id 'I' 'EQ' p_recon_id ''.
  f_fill_range: lra_zlevel 'I' 'EQ' p_zlevel ''.
  f_fill_range: lra_balance_date 'I' 'EQ' p_balance_date '00000000'.
  f_fill_range: lra_source 'I' 'EQ' p_source ''.
  f_fill_range: lra_bal_no_reff 'I' 'EQ' p_bal_no_reff ''.

  CASE p_zlevel.
    WHEN 2.

      LOOP AT p_lit_unit_id_lvl_1 INTO DATA(lwa_unit_id).
        f_fill_range: lra_unit_id_lvl_1 'I' 'EQ' lwa_unit_id-unit_id ''.
      ENDLOOP.

    WHEN 3.

      LOOP AT p_lit_unit_id_lvl_1 INTO lwa_unit_id.
        f_fill_range: lra_unit_id_lvl_1 'I' 'EQ' lwa_unit_id-unit_id ''.
      ENDLOOP.

      LOOP AT p_lit_unit_id_lvl_2 INTO lwa_unit_id.
        f_fill_range: lra_unit_id_lvl_2 'I' 'EQ' lwa_unit_id-unit_id ''.
      ENDLOOP.

    WHEN 4.

      LOOP AT p_lit_unit_id_lvl_1 INTO lwa_unit_id.
        f_fill_range: lra_unit_id_lvl_1 'I' 'EQ' lwa_unit_id-unit_id ''.
      ENDLOOP.

      LOOP AT p_lit_unit_id_lvl_2 INTO lwa_unit_id.
        f_fill_range: lra_unit_id_lvl_2 'I' 'EQ' lwa_unit_id-unit_id ''.
      ENDLOOP.

      LOOP AT p_lit_unit_id_lvl_3 INTO lwa_unit_id.
        f_fill_range: lra_unit_id_lvl_3 'I' 'EQ' lwa_unit_id-unit_id ''.
      ENDLOOP.
  ENDCASE.

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
    SUBMIT zfi02r0032
      WITH s_rbukrs IN lra_rbukrs
      WITH s_recid IN lra_recon_id
      WITH s_zlevel IN lra_zlevel
      WITH s_baldat IN lra_balance_date
      WITH s_source IN lra_source
      WITH s_noref IN lra_bal_no_reff

      WITH s_unlv1 IN lra_unit_id_lvl_1
      WITH s_unlv2 IN lra_unit_id_lvl_2
      WITH s_unlv3 IN lra_unit_id_lvl_3

      WITH c_sw_fm EQ 'X' "Checkbox Switch FM
      WITH c_sw_log EQ 'X' "Checkbox Switch Log
      WITH p_datum EQ p_datum
      WITH p_uzeit EQ p_uzeit

      WITH c_sw_spl EQ 'X' "Checkbox Switch Split Year

        USER sy-uname
        VIA JOB p_jobname NUMBER ld_jobcount AND RETURN.

    IF sy-subrc = 0.

      CLEAR p_job_was_released.
      CALL FUNCTION 'EWU_ADD_TIME'
        EXPORTING
          i_starttime = sy-uzeit
          i_startdate = sy-datum
          i_addtime   = '000005'
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
*& Form F_INSERT_MESSAGE_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LWA_PARAM_RECON_ID
*&      <-- IT_RETURN
*&---------------------------------------------------------------------*
FORM f_insert_message_b  USING    p_type
                                  p_kind
                                  p_recon_id
                                  p_message
                                  p_message_b
                                  p_message_c
                                  p_message_d
                         CHANGING p_return TYPE bapiret2.

  DATA: lwa_bapiret2 TYPE bapiret2.

*--------------------------------------------------------------------*

  CLEAR p_return.
  p_return-type = p_type.

  IF p_kind IS NOT INITIAL.
    CONCATENATE p_kind
                'Recon. ID'
                p_recon_id
                p_message
      INTO p_return-message SEPARATED BY space.
  ELSE.
    CONCATENATE 'Recon. ID'
                p_recon_id
                p_message
      INTO p_return-message SEPARATED BY space.
  ENDIF.

ENDFORM.
