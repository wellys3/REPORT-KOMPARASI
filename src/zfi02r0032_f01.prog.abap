*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_PRE_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pre_execute CHANGING p_subrc.

  FIELD-SYMBOLS: <lfs>,
                 <lfs2>.

  TYPES: BEGIN OF gty_field_ass,
           field_ass TYPE zfidt00266-field_ass,
         END OF gty_field_ass,

         BEGIN OF gty_field_value,
           field_value TYPE zfidt00315-unit_id_level_1,
         END OF gty_field_value.

  DATA : lit_field_ass_tmp   TYPE TABLE OF gty_field_ass,
         lit_field_map_tmp   TYPE TABLE OF gty_map_field,
         lit_field_value_tmp TYPE TABLE OF gty_field_value,

         ld_dref             TYPE REF TO data,
         ld_times(1)         TYPE n,
         ld_tabix(1)         TYPE n,
         ld_sum_lines        TYPE i,
         ld_fs(100),
         ld_fs2(100),
         ld_length           TYPE i,
         ld_counter          TYPE n.

*--------------------------------------------------------------------*
*Get Radio Button

  IF rb1 EQ 'X'.
    gd_rb = 'NO TRACING'.
  ELSEIF rb2 EQ 'X'.
    gd_rb = 'TRACING LEVEL 1'.
  ELSEIF rb3 EQ 'X'.
    gd_rb = 'TRACING LEVEL 2'.
*  ELSEIF rb4 EQ 'X'.
*    gd_rb = 'TRACING LEVEL 3'.
  ENDIF.

*--------------------------------------------------------------------*
*Get Table Maintain ZFIDT00265

  CLEAR             git_zfidt00265[].
  SELECT * FROM zfidt00265 INTO TABLE git_zfidt00265
    WHERE rbukrs IN s_rbukrs AND
          recon_id IN s_recid.
  IF git_zfidt00265[] IS NOT INITIAL.

    CLEAR gra_racct[].
    LOOP AT git_zfidt00265 INTO DATA(lwa_zfidt00265).

      IF lwa_zfidt00265-racct_from IS NOT INITIAL AND
         lwa_zfidt00265-racct_to IS INITIAL .

        f_fill_range: gra_racct 'I' 'EQ' lwa_zfidt00265-racct_from ''.

      ELSEIF lwa_zfidt00265-racct_from IS INITIAL AND
         lwa_zfidt00265-racct_to IS NOT INITIAL .

        f_fill_range: gra_racct 'I' 'EQ' lwa_zfidt00265-racct_to ''.

      ELSEIF lwa_zfidt00265-racct_from IS NOT INITIAL AND
         lwa_zfidt00265-racct_to IS NOT INITIAL .

        IF lwa_zfidt00265-racct_from EQ lwa_zfidt00265-racct_to.
          f_fill_range: gra_racct 'I' 'EQ' lwa_zfidt00265-racct_from ''.
        ELSE.
          f_fill_range: gra_racct 'I' 'BT' lwa_zfidt00265-racct_from lwa_zfidt00265-racct_to.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

*Get Table Maintain ZFIDT00266
  CLEAR git_zfidt00266[].
  SELECT * FROM zfidt00266 INTO TABLE git_zfidt00266
    WHERE rbukrs IN s_rbukrs AND
          recon_id IN s_recid.

*Get Table Maintain ZFIDT00267
  CLEAR gwa_zfidt00267.
  SELECT SINGLE * FROM zfidt00267 INTO gwa_zfidt00267
    WHERE rbukrs IN s_rbukrs AND
          valid_date_to EQ '99991231' AND
          recon_id IN s_recid.

*--------------------------------------------------------------------*
*Get All Level ZFIDT00267 and Separate

  CLEAR: git_field_sap_separate[],
         ld_counter.
  DO 3 TIMES.

    ADD 1 TO ld_counter.

    CONCATENATE 'GWA_ZFIDT00267' '-' 'LEVEL' '_' ld_counter INTO ld_fs.
    CONDENSE ld_fs NO-GAPS.
    ASSIGN (ld_fs) TO <lfs>.

    IF <lfs> IS ASSIGNED.

      CLEAR lit_field_ass_tmp[].
*      SPLIT <lfs> AT '+' INTO TABLE lit_field_ass_tmp.
      SPLIT <lfs> AT ',' INTO TABLE lit_field_ass_tmp.

      IF lit_field_ass_tmp[] IS NOT INITIAL.

        LOOP AT lit_field_ass_tmp INTO DATA(lwa_field_ass_tmp).

          READ TABLE git_zfidt00266 INTO DATA(lwa_zfidt00266)
            WITH KEY field_ass = lwa_field_ass_tmp-field_ass.
          IF sy-subrc EQ 0.

            SELECT SINGLE * FROM dd03l INTO @DATA(lwa_dd03l)
              WHERE fieldname EQ @lwa_zfidt00266-field_sap.
            IF sy-subrc EQ 0.
              "Do nothing
            ELSE.

              p_subrc = 1.

              CLEAR gd_message.
              gd_message = lwa_field_ass_tmp-field_ass && ')'.
              CONCATENATE '(Field ASS:' gd_message
                INTO gd_message SEPARATED BY space.
              CONCATENATE 'Field SAP:' lwa_zfidt00266-field_sap
                          'not valid in SAP!'
                          gd_message
                INTO gd_message SEPARATED BY space.
              MESSAGE gd_message TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.

            ENDIF.

            "*--------------------------------------------------------------------*

            CLEAR gwa_field_sap_separate.
            gwa_field_sap_separate-zlevel = ld_counter.
            gwa_field_sap_separate-field_sap = lwa_zfidt00266-field_sap.
            APPEND gwa_field_sap_separate TO git_field_sap_separate.

          ELSE.

            p_subrc = 1.

            CLEAR gd_message.
            CONCATENATE 'Field ASS:' lwa_field_ass_tmp-field_ass
                        'in table ZFIDT00266, not found!'
              INTO gd_message SEPARATED BY space.
            MESSAGE gd_message TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF p_subrc NE 0.
      EXIT.
    ENDIF.

  ENDDO.

  IF git_field_sap_separate[] IS NOT INITIAL.
    DELETE git_field_sap_separate WHERE zlevel > s_zlevel-low.
  ENDIF.

  CHECK p_subrc EQ 0.

*--------------------------------------------------------------------*
*Get All Level ZFIDT00267 and Combine

  CLEAR: git_field_sap_combine[],
         ld_counter.
  DO 3 TIMES.

    ADD 1 TO ld_counter.

    CONCATENATE 'GWA_ZFIDT00267' '-' 'LEVEL' '_' ld_counter INTO ld_fs.
    CONDENSE ld_fs NO-GAPS.
    ASSIGN (ld_fs) TO <lfs>.

    IF <lfs> IS ASSIGNED.

      CLEAR lit_field_ass_tmp[].
*      SPLIT <lfs> AT '+' INTO TABLE lit_field_ass_tmp.
      SPLIT <lfs> AT ',' INTO TABLE lit_field_ass_tmp.

      IF lit_field_ass_tmp[] IS NOT INITIAL.

        CLEAR gwa_field_sap_combine.
        gwa_field_sap_combine-zlevel = ld_counter.

        CLEAR gd_tabix.
        LOOP AT lit_field_ass_tmp INTO lwa_field_ass_tmp.

          ADD 1 TO gd_tabix.

          READ TABLE git_zfidt00266 INTO lwa_zfidt00266
            WITH KEY field_ass = lwa_field_ass_tmp-field_ass.
          IF sy-subrc EQ 0.

            IF gd_tabix EQ 1.
              gwa_field_sap_combine-field_sap = lwa_zfidt00266-field_sap.
            ELSE.
              CONCATENATE gwa_field_sap_combine-field_sap ',' lwa_zfidt00266-field_sap
                INTO gwa_field_sap_combine-field_sap.
            ENDIF.

          ENDIF.

        ENDLOOP.

        APPEND gwa_field_sap_combine TO git_field_sap_combine.

      ENDIF.

    ENDIF.

  ENDDO.

  IF git_field_sap_combine[] IS NOT INITIAL.
    DELETE git_field_sap_combine WHERE zlevel > s_zlevel-low.
  ENDIF.

*--------------------------------------------------------------------*
*Choose field from Selection Screen (S_ZLEVEL-LOW)

  CLEAR gd_times.
  CASE s_zlevel-low.
    WHEN 1.
      "Do nothing
    WHEN 2.
      ld_times = 1.
    WHEN 3.
      ld_times = 2.
    WHEN 4.
      ld_times = 3.
  ENDCASE.
  gd_times = ld_times.

  "*--------------------------------------------------------------------*
  "*Get Unit ID Level 1 until 3 into one table

  IF s_zlevel-low >= 2.

    DESCRIBE TABLE s_unlv1[] LINES DATA(ld_lines).
    DESCRIBE TABLE s_unlv2[] LINES DATA(ld_lines_b).
    DESCRIBE TABLE s_unlv3[] LINES DATA(ld_lines_c).

    IF ld_lines >= ld_lines_b.
      ld_lines = ld_lines.
    ELSE.
      ld_lines = ld_lines_b.
    ENDIF.

    IF ld_lines >= ld_lines_c.
      ld_lines = ld_lines.
    ELSE.
      ld_lines = ld_lines_c.
    ENDIF.

    DO ld_lines TIMES.

      TRY.
          CASE s_zlevel-low.
            WHEN 2.
              gwa_unit_level-unit_id_lvl_1 = s_unlv1[ sy-index ]-low.
            WHEN 3.
              gwa_unit_level-unit_id_lvl_1 = s_unlv1[ sy-index ]-low.
              gwa_unit_level-unit_id_lvl_2 = s_unlv2[ sy-index ]-low.
            WHEN 4.
              gwa_unit_level-unit_id_lvl_1 = s_unlv1[ sy-index ]-low.
              gwa_unit_level-unit_id_lvl_2 = s_unlv2[ sy-index ]-low.
              gwa_unit_level-unit_id_lvl_3 = s_unlv3[ sy-index ]-low.
          ENDCASE.
        CATCH cx_root INTO DATA(lcl_exc).
      ENDTRY.

      APPEND gwa_unit_level TO git_unit_level.

    ENDDO.

*    SORT git_unit_level ASCENDING BY unit_id_lvl_1.
*    DELETE ADJACENT DUPLICATES FROM git_unit_level COMPARING unit_id_lvl_1.
*
*    SORT git_unit_level ASCENDING BY unit_id_lvl_2.
*    DELETE ADJACENT DUPLICATES FROM git_unit_level COMPARING unit_id_lvl_2.
*
*    SORT git_unit_level ASCENDING BY unit_id_lvl_3.
*    DELETE ADJACENT DUPLICATES FROM git_unit_level COMPARING unit_id_lvl_3.

  ENDIF.

*--------------------------------------------------------------------*
*Recon Mapping Field ASS and SAP and Field Length

  CLEAR ld_sum_lines.
  CLEAR git_field_map[].
  IF ( gwa_zfidt00267 IS NOT INITIAL ) AND
     ( s_zlevel-low <= 4 AND s_zlevel-low >= 2 ).

    CLEAR ld_tabix.
    DO ld_times TIMES.

      ADD 1 TO ld_tabix.

      CONCATENATE 'GWA_ZFIDT00267' '-' 'LEVEL_' ld_tabix INTO ld_fs.
      CONDENSE ld_fs NO-GAPS.

      "*--------------------------------------------------------------------*

      CONCATENATE 'S_UNLV' ld_tabix '[]' INTO ld_fs2.
      CONDENSE ld_fs2 NO-GAPS.

      "*--------------------------------------------------------------------*

      ASSIGN (ld_fs) TO <lfs>.
      IF <lfs> IS ASSIGNED.

        IF <lfs> IS NOT INITIAL.

*          SPLIT <lfs> AT '+' INTO TABLE git_field_map.
*          SPLIT <lfs> AT '+' INTO TABLE lit_field_map_tmp.
          SPLIT <lfs> AT ',' INTO TABLE lit_field_map_tmp.
        ENDIF.

      ENDIF.

      "*--------------------------------------------------------------------*

      IF lit_field_map_tmp[] IS NOT INITIAL.

        LOOP AT lit_field_map_tmp ASSIGNING FIELD-SYMBOL(<fs_field_map>).

          READ TABLE git_zfidt00266 INTO lwa_zfidt00266
            WITH KEY field_ass = <fs_field_map>-field_ass.
          IF sy-subrc EQ 0.

            <fs_field_map>-field_sap = lwa_zfidt00266-field_sap.
            <fs_field_map>-flag_input_conversion = lwa_zfidt00266-flag_input_conversion.

            SELECT SINGLE * FROM dd03l INTO @lwa_dd03l
              WHERE fieldname EQ @lwa_zfidt00266-field_sap AND
                    rollname NE ''.
            IF sy-subrc EQ 0.
              <fs_field_map>-field_length = lwa_dd03l-leng.
              <fs_field_map>-field_sap_data_element = lwa_dd03l-rollname.
            ENDIF.

          ENDIF.

        ENDLOOP.

        APPEND LINES OF lit_field_map_tmp[] TO git_field_map[].

      ENDIF.

      "*--------------------------------------------------------------------*
      "*Assigning value from Selection Screen (S_UNLV1 until S_UNLV3) to Ranges Field (GRA_VALUE1 until GRA_VALUE?)

      IF lit_field_map_tmp[] IS NOT INITIAL.

        ASSIGN (ld_fs2) TO <lfs2>.

        IF <lfs2> IS ASSIGNED.

          IF <lfs2> IS NOT INITIAL.
            CLEAR gra_unit_level[].
            gra_unit_level[] = <lfs2>.
          ENDIF.

        ENDIF.

        "*--------------------------------------------------------------------*

        LOOP AT gra_unit_level INTO DATA(lwa_unit_level).

          CLEAR lit_field_value_tmp[].
          SPLIT lwa_unit_level-low AT ',' INTO TABLE lit_field_value_tmp.

          CLEAR ld_length.
          LOOP AT lit_field_map_tmp INTO DATA(lwa_field_map).

            gd_tabix = sy-tabix.

            CLEAR ld_dref.
            UNASSIGN <lfs>.

*            CASE lwa_field_map-field_sap.
*              WHEN 'ZUONR'.
*                lwa_field_map-field_sap = 'DZUONR'.
*            ENDCASE.

*            CREATE DATA ld_dref TYPE (lwa_field_map-field_sap).
            CREATE DATA ld_dref TYPE (lwa_field_map-field_sap_data_element).
            ASSIGN ld_dref->* TO <lfs>.

*            <lfs> = lwa_unit_level-low+ld_length(lwa_field_map-field_length).

            TRY .
                <lfs> = lit_field_value_tmp[ gd_tabix ]-field_value.
              CATCH cx_root INTO DATA(lv_exc).
            ENDTRY.

*            CHECK <lfs> IS NOT INITIAL.

            IF lwa_field_map-flag_input_conversion EQ abap_true.
              <lfs> = |{ <lfs> ALPHA = IN }|.
            ENDIF.

            IF ld_tabix >= 2.
              gd_tabix = gd_tabix + ld_sum_lines.
            ENDIF.

            CASE gd_tabix.
              WHEN 1.
                f_fill_range: gra_value1 'I' 'EQ' <lfs> ''.
              WHEN 2.
                f_fill_range: gra_value2 'I' 'EQ' <lfs> ''.
              WHEN 3.
                f_fill_range: gra_value3 'I' 'EQ' <lfs> ''.
              WHEN 4.
                f_fill_range: gra_value4 'I' 'EQ' <lfs> ''.
              WHEN 5.
                f_fill_range: gra_value5 'I' 'EQ' <lfs> ''.
            ENDCASE.

*            IF lwa_field_map-flag_input_conversion EQ abap_true.
*              ADD lwa_field_map-field_length TO ld_length.
*            ELSE.
*              DATA(ld_strlen) = strlen( <lfs> ).
*              ADD ld_strlen TO ld_length.
*            ENDIF.

          ENDLOOP.

        ENDLOOP.

*        DESCRIBE TABLE lit_field_map_tmp LINES DATA(ld_lines).
        DESCRIBE TABLE lit_field_map_tmp LINES ld_lines.
        ADD ld_lines TO ld_sum_lines.

      ENDIF.

      "*--------------------------------------------------------------------*

    ENDDO.

  ENDIF.

  SORT gra_value1[] ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM gra_value1[] COMPARING low.
  DELETE gra_value1 WHERE low EQ 'NULL'.

  SORT gra_value2[] ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM gra_value2[] COMPARING low.
  DELETE gra_value2 WHERE low EQ 'NULL'.

  SORT gra_value3[] ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM gra_value3[] COMPARING low.
  DELETE gra_value3 WHERE low EQ 'NULL'.

  SORT gra_value4[] ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM gra_value4[] COMPARING low.
  DELETE gra_value4 WHERE low EQ 'NULL'.

  SORT gra_value5[] ASCENDING BY low.
  DELETE ADJACENT DUPLICATES FROM gra_value5[] COMPARING low.
  DELETE gra_value5 WHERE low EQ 'NULL'.

*--------------------------------------------------------------------*

  SELECT SINGLE * FROM tvarvc INTO @DATA(lwa_tvarvc)
    WHERE name EQ @gc_numb_rows_per_batch.
  IF sy-subrc EQ 0.
    gd_max_row_per_batch = lwa_tvarvc-low.
  ELSE.
    gd_max_row_per_batch = 3000.
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

  DATA: ld_jobname                     TYPE tbtco-jobname,
        ld_job_was_released            TYPE c,
        ld_batch_total                 TYPE zfide01201,
        ld_row_total                   TYPE zfide01203,

        lit_data_detail_x_temp         TYPE TABLE OF gty_data_detail_x,
        lit_data_detail_x_lv4_temp     TYPE TABLE OF gty_data_detail_x_lv4,
        lit_data_pre_detail_x_temp     TYPE TABLE OF gty_data_detail_x,
        lit_data_pre_detail_x_lv4_temp TYPE TABLE OF gty_data_detail_x_lv4.

*--------------------------------------------------------------------*

  PERFORM f_start_timer.

  IF c_sw_spl EQ ''.

    PERFORM f_get_data    USING '' "Flag Per Year
                                s_baldat-low(4)
                       CHANGING git_zfidt00242[]
*                              git_zfidt00269[]
*                              git_acdoca_ra[]
                                git_acdoca_oi[]
*                              git_acdoca_bs[]
                                git_acdoca_pl[]
                                git_acdoca[]

                                git_zfidt00242_x[]
                                git_acdoca_oi_x[]
                                git_acdoca_pl_x[]
                                git_acdoca_x_lv4[].

    PERFORM f_combine_table USING git_zfidt00242[]
*                                git_zfidt00269[]
*                                git_acdoca_ra[]
                                  git_acdoca_oi[]
*                                git_acdoca_bs[]
                                  git_acdoca_pl[]
                                  git_acdoca[]

                                  git_zfidt00242_x[]
                                  git_acdoca_oi_x[]
                                  git_acdoca_pl_x[]
                                  git_acdoca_x_lv4[]

                         CHANGING git_data_detail[]
                                  git_data_detail_x[]
                                  git_data_detail_x_lv4[].

    CASE gd_rb.
      WHEN 'NO TRACING'.

        PERFORM f_process_summary USING git_data_detail[]
                                        git_data_detail_x[]
                                        git_data_detail_x_lv4[]
                               CHANGING git_data_pre_detail[]
                                        git_data_pre_detail_x[]
                                        git_data_pre_detail_x_lv4[].

        PERFORM f_prepare_alv USING git_data_pre_detail[]
                                    git_data_pre_detail_x[]
                                    git_data_pre_detail_x_lv4[]
                           CHANGING git_data_sum_final[]
                                    git_data_sum_final_b[].

        CASE c_sw_fm. "Checkbox Switch FM
          WHEN ''.

            CASE s_zlevel-low.
              WHEN 1 OR 2 OR 3.

                IF git_data_sum_final[] IS NOT INITIAL.

                  PERFORM f_split_and_save_log    USING s_zlevel-low
                                               CHANGING ld_batch_total
                                                        ld_row_total

                                                        git_data_sum_final[]
                                                        git_data_sum_final_b[].

                  PERFORM f_stop_timer.
                  "001: This program sucessfully executed! (Exec. Time & seconds)
                  MESSAGE s071(zfimsg) WITH gd_run.

                  PERFORM f_display_data USING git_data_detail[]
                                               git_data_pre_detail[]
                                               git_data_sum_final[]
                                               git_data_sum_final_b[].
                ELSE.
                  MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
                ENDIF.

              WHEN 4.

                IF git_data_sum_final_b[] IS NOT INITIAL.

                  PERFORM f_split_and_save_log    USING s_zlevel-low
                                               CHANGING ld_batch_total
                                                        ld_row_total

                                                        git_data_sum_final[]
                                                        git_data_sum_final_b[].

                  PERFORM f_stop_timer.
                  "001: This program sucessfully executed! (Exec. Time & seconds)
                  MESSAGE s071(zfimsg) WITH gd_run.

                  PERFORM f_display_data USING git_data_detail[]
                                               git_data_pre_detail[]
                                               git_data_sum_final[]
                                               git_data_sum_final_b[].
                ELSE.
                  MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
                ENDIF.

            ENDCASE.

          WHEN 'X'.

            CASE s_zlevel-low.
              WHEN 1 OR 2 OR 3.

                IF git_data_sum_final[] IS NOT INITIAL.

                  PERFORM f_split_and_save_log     USING s_zlevel-low
                                                CHANGING ld_batch_total
                                                         ld_row_total

                                                         git_data_sum_final[]
                                                         git_data_sum_final_b[].

                  "*--------------------------------------------------------------------*

                  PERFORM f_update_status_zfidt00268 USING 'PROCESSED'
                                                           sy-datum
                                                           sy-uzeit

                                                           s_rbukrs-low
                                                           p_datum
                                                           p_uzeit
                                                           s_recid-low
                                                           s_noref-low
                                                           s_zlevel-low.

                  WAIT UP TO 1 SECONDS.

                  "*--------------------------------------------------------------------*
                  "Log in ZFIDT00268 will be changes to as splitted result

                  PERFORM f_change_log USING  ld_batch_total
                                              ld_row_total

                                              s_rbukrs-low
                                              p_datum
                                              p_uzeit
                                              s_recid-low
                                              s_noref-low
                                              s_zlevel-low
                                              s_baldat-low.

                  "*--------------------------------------------------------------------*
                  "Create job to split data and send outbond to ASS

                  PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                             p_uzeit

                                                             s_rbukrs-low
                                                             s_recid-low
                                                             s_zlevel-low
                                                             s_baldat-low
                                                             s_noref-low

                                                             '1'
                                                             ld_batch_total
                                                    CHANGING ld_job_was_released
                                                             ld_jobname.

                  CLEAR gd_message.
                  IF ld_job_was_released EQ 'X'.
                    CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
                    CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
                    PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
                  ELSE.
                    CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
                    PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
                  ENDIF.

                  "*--------------------------------------------------------------------*

*              CALL FUNCTION 'ZFIFM_OB_SEND_KOMPARASI_LV1_3'
*                TABLES
*                  it_komparasi = git_data_sum_final[].

                  "*--------------------------------------------------------------------*

*              PERFORM f_update_status_zfidt00268 USING 'SENT'
*                                                       sy-datum
*                                                       sy-uzeit
*
*                                                       s_rbukrs-low
*                                                       p_datum
*                                                       p_uzeit
*                                                       s_recid-low
*                                                       s_noref-low.

                  "*--------------------------------------------------------------------*

                  PERFORM f_stop_timer.
                  "001: This program sucessfully executed! (Exec. Time & seconds)
                  MESSAGE s071(zfimsg) WITH gd_run.

                  PERFORM f_display_data USING git_data_detail[]
                                               git_data_pre_detail[]
                                               git_data_sum_final[]
                                               git_data_sum_final_b[].
                ELSE.
                  MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
                ENDIF.

              WHEN 4.

                IF git_data_sum_final_b[] IS NOT INITIAL.

                  PERFORM f_split_and_save_log    USING s_zlevel-low
                                               CHANGING ld_batch_total
                                                        ld_row_total

                                                        git_data_sum_final[]
                                                        git_data_sum_final_b[].

                  "*--------------------------------------------------------------------*

                  PERFORM f_update_status_zfidt00268 USING 'PROCESSED'
                                                           sy-datum
                                                           sy-uzeit

                                                           s_rbukrs-low
                                                           p_datum
                                                           p_uzeit
                                                           s_recid-low
                                                           s_noref-low
                                                           s_zlevel-low.

                  WAIT UP TO 1 SECONDS.

                  "*--------------------------------------------------------------------*
                  "Log in ZFIDT00268 will be changes to as splitted result

                  PERFORM f_change_log USING  ld_batch_total
                                              ld_row_total

                                              s_rbukrs-low
                                              p_datum
                                              p_uzeit
                                              s_recid-low
                                              s_noref-low
                                              s_zlevel-low
                                              s_baldat-low.

                  "*--------------------------------------------------------------------*

                  PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                             p_uzeit

                                                             s_rbukrs-low
                                                             s_recid-low
                                                             s_zlevel-low
                                                             s_baldat-low
                                                             s_noref-low

                                                             '1'
                                                             ld_batch_total
                                                    CHANGING ld_job_was_released
                                                             ld_jobname.

                  CLEAR gd_message.
                  IF ld_job_was_released EQ 'X'.
                    CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
                    CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
                    PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
                  ELSE.
                    CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
                    PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
                  ENDIF.

                  "*--------------------------------------------------------------------*

*              CALL FUNCTION 'ZFIFM_OB_SEND_KOMPARASI_LV4'
*                TABLES
*                  it_komparasi = git_data_sum_final_b[].

                  "*--------------------------------------------------------------------*

*              PERFORM f_update_status_zfidt00268 USING 'SENT'
*                                                       sy-datum
*                                                       sy-uzeit
*
*                                                       s_rbukrs-low
*                                                       p_datum
*                                                       p_uzeit
*                                                       s_recid-low
*                                                       s_noref-low.

                  "*--------------------------------------------------------------------*

                  PERFORM f_stop_timer.
                  "001: This program sucessfully executed! (Exec. Time & seconds)
                  MESSAGE s071(zfimsg) WITH gd_run.

                  PERFORM f_display_data USING git_data_detail[]
                                               git_data_pre_detail[]
                                               git_data_sum_final[]
                                               git_data_sum_final_b[].
                ELSE.
                  MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
                ENDIF.

            ENDCASE.

        ENDCASE.

      WHEN 'TRACING LEVEL 1'.

        PERFORM f_process_summary USING git_data_detail[]
                                        git_data_detail_x[]
                                        git_data_detail_x_lv4[]
                               CHANGING git_data_pre_detail[]
                                        git_data_pre_detail_x[]
                                        git_data_pre_detail_x_lv4[].


        IF git_data_pre_detail[] IS NOT INITIAL.

          PERFORM f_stop_timer.
          "001: This program sucessfully executed! (Exec. Time & seconds)
          MESSAGE s071(zfimsg) WITH gd_run.

          PERFORM f_display_data USING git_data_detail[]
                                       git_data_pre_detail[]
                                       git_data_sum_final[]
                                       git_data_sum_final_b[].
        ELSE.
          MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

      WHEN 'TRACING LEVEL 2'.

        IF git_data_detail[] IS NOT INITIAL.

          PERFORM f_stop_timer.
          "001: This program sucessfully executed! (Exec. Time & seconds)
          MESSAGE s071(zfimsg) WITH gd_run.

          PERFORM f_display_data USING git_data_detail[]
                                       git_data_pre_detail[]
                                       git_data_sum_final[]
                                       git_data_sum_final_b[].
        ELSE.
          MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

    ENDCASE.

  ELSE.

    PERFORM f_get_year CHANGING git_year.

    "*--------------------------------------------------------------------*

    LOOP AT git_year INTO gwa_year.

      PERFORM f_get_data    USING 'X' "Flag Per Year
                                  gwa_year-gjahr
                         CHANGING git_zfidt00242[]
                                  git_acdoca_oi[]
                                  git_acdoca_pl[]
                                  git_acdoca[]

                                  git_zfidt00242_x[]
                                  git_acdoca_oi_x[]
                                  git_acdoca_pl_x[]
                                  git_acdoca_x_lv4[].

      CLEAR: lit_data_detail_x_temp[],
             lit_data_detail_x_lv4_temp[].
      PERFORM f_combine_table USING git_zfidt00242[]
                                    git_acdoca_oi[]
                                    git_acdoca_pl[]
                                    git_acdoca[]

                                    git_zfidt00242_x[]
                                    git_acdoca_oi_x[]
                                    git_acdoca_pl_x[]
                                    git_acdoca_x_lv4[]

                           CHANGING git_data_detail[]
                                    lit_data_detail_x_temp[]
                                    lit_data_detail_x_lv4_temp[].

      CLEAR: lit_data_pre_detail_x_temp[],
             lit_data_pre_detail_x_lv4_temp[].
      PERFORM f_process_summary USING git_data_detail[]
                                      lit_data_detail_x_temp[]
                                      lit_data_detail_x_lv4_temp[]
                             CHANGING git_data_pre_detail[]
                                      lit_data_pre_detail_x_temp[]
                                      lit_data_pre_detail_x_lv4_temp[].

      "*--------------------------------------------------------------------*

      PERFORM f_append_summary USING  lit_data_pre_detail_x_temp[]
                                      lit_data_pre_detail_x_lv4_temp[]
                             CHANGING git_data_pre_detail_x[]
                                      git_data_pre_detail_x_lv4[].

    ENDLOOP.

    "*--------------------------------------------------------------------*

    PERFORM f_process_summary_v2 CHANGING git_data_pre_detail_x[]
                                          git_data_pre_detail_x_lv4[].

    PERFORM f_prepare_alv USING git_data_pre_detail[]
                                git_data_pre_detail_x[]
                                git_data_pre_detail_x_lv4[]
                       CHANGING git_data_sum_final[]
                                git_data_sum_final_b[].

    "*--------------------------------------------------------------------*

    CASE c_sw_fm. "Checkbox Switch FM
      WHEN ''.

        CASE s_zlevel-low.
          WHEN 1 OR 2 OR 3.

            IF git_data_sum_final[] IS NOT INITIAL.

              PERFORM f_split_and_save_log    USING s_zlevel-low
                                           CHANGING ld_batch_total
                                                    ld_row_total

                                                    git_data_sum_final[]
                                                    git_data_sum_final_b[].

              PERFORM f_stop_timer.
              "001: This program sucessfully executed! (Exec. Time & seconds)
              MESSAGE s071(zfimsg) WITH gd_run.

              PERFORM f_display_data USING git_data_detail[]
                                           git_data_pre_detail[]
                                           git_data_sum_final[]
                                           git_data_sum_final_b[].
            ELSE.
              MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
            ENDIF.

          WHEN 4.

            IF git_data_sum_final_b[] IS NOT INITIAL.

              PERFORM f_split_and_save_log    USING s_zlevel-low
                                           CHANGING ld_batch_total
                                                    ld_row_total

                                                    git_data_sum_final[]
                                                    git_data_sum_final_b[].

              PERFORM f_stop_timer.
              "001: This program sucessfully executed! (Exec. Time & seconds)
              MESSAGE s071(zfimsg) WITH gd_run.

              PERFORM f_display_data USING git_data_detail[]
                                           git_data_pre_detail[]
                                           git_data_sum_final[]
                                           git_data_sum_final_b[].
            ELSE.
              MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
            ENDIF.

        ENDCASE.

      WHEN 'X'.

        CASE s_zlevel-low.
          WHEN 1 OR 2 OR 3.

            IF git_data_sum_final[] IS NOT INITIAL.

              PERFORM f_split_and_save_log     USING s_zlevel-low
                                            CHANGING ld_batch_total
                                                     ld_row_total

                                                     git_data_sum_final[]
                                                     git_data_sum_final_b[].

              "*--------------------------------------------------------------------*

              PERFORM f_update_status_zfidt00268 USING 'PROCESSED'
                                                       sy-datum
                                                       sy-uzeit

                                                       s_rbukrs-low
                                                       p_datum
                                                       p_uzeit
                                                       s_recid-low
                                                       s_noref-low
                                                       s_zlevel-low.

              WAIT UP TO 1 SECONDS.

              "*--------------------------------------------------------------------*
              "Log in ZFIDT00268 will be changes to as splitted result

              PERFORM f_change_log USING  ld_batch_total
                                          ld_row_total

                                          s_rbukrs-low
                                          p_datum
                                          p_uzeit
                                          s_recid-low
                                          s_noref-low
                                          s_zlevel-low
                                          s_baldat-low.

              "*--------------------------------------------------------------------*
              "Create job to split data and send outbond to ASS

              PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                         p_uzeit

                                                         s_rbukrs-low
                                                         s_recid-low
                                                         s_zlevel-low
                                                         s_baldat-low
                                                         s_noref-low

                                                         '1'
                                                         ld_batch_total
                                                CHANGING ld_job_was_released
                                                         ld_jobname.

              CLEAR gd_message.
              IF ld_job_was_released EQ 'X'.
                CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
                CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
              ELSE.
                CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
              ENDIF.

              "*--------------------------------------------------------------------*

*              CALL FUNCTION 'ZFIFM_OB_SEND_KOMPARASI_LV1_3'
*                TABLES
*                  it_komparasi = git_data_sum_final[].

              "*--------------------------------------------------------------------*

*              PERFORM f_update_status_zfidt00268 USING 'SENT'
*                                                       sy-datum
*                                                       sy-uzeit
*
*                                                       s_rbukrs-low
*                                                       p_datum
*                                                       p_uzeit
*                                                       s_recid-low
*                                                       s_noref-low.

              "*--------------------------------------------------------------------*

              PERFORM f_stop_timer.
              "001: This program sucessfully executed! (Exec. Time & seconds)
              MESSAGE s071(zfimsg) WITH gd_run.

              PERFORM f_display_data USING git_data_detail[]
                                           git_data_pre_detail[]
                                           git_data_sum_final[]
                                           git_data_sum_final_b[].
            ELSE.
              MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
            ENDIF.

          WHEN 4.

            IF git_data_sum_final_b[] IS NOT INITIAL.

              PERFORM f_split_and_save_log    USING s_zlevel-low
                                           CHANGING ld_batch_total
                                                    ld_row_total

                                                    git_data_sum_final[]
                                                    git_data_sum_final_b[].

              "*--------------------------------------------------------------------*

              PERFORM f_update_status_zfidt00268 USING 'PROCESSED'
                                                       sy-datum
                                                       sy-uzeit

                                                       s_rbukrs-low
                                                       p_datum
                                                       p_uzeit
                                                       s_recid-low
                                                       s_noref-low
                                                       s_zlevel-low.

              WAIT UP TO 1 SECONDS.

              "*--------------------------------------------------------------------*
              "Log in ZFIDT00268 will be changes to as splitted result

              PERFORM f_change_log USING  ld_batch_total
                                          ld_row_total

                                          s_rbukrs-low
                                          p_datum
                                          p_uzeit
                                          s_recid-low
                                          s_noref-low
                                          s_zlevel-low
                                          s_baldat-low.

              "*--------------------------------------------------------------------*

              PERFORM f_create_bj_zfi02r0032_split USING p_datum
                                                         p_uzeit

                                                         s_rbukrs-low
                                                         s_recid-low
                                                         s_zlevel-low
                                                         s_baldat-low
                                                         s_noref-low

                                                         '1'
                                                         ld_batch_total
                                                CHANGING ld_job_was_released
                                                         ld_jobname.

              CLEAR gd_message.
              IF ld_job_was_released EQ 'X'.
                CONCATENATE ld_jobname 'successfully' INTO gd_message SEPARATED BY space.
                CONCATENATE 'Created Background Job' gd_message INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'S'.
              ELSE.
                CONCATENATE 'Failed to create Background Job' ld_jobname INTO gd_message SEPARATED BY space.
                PERFORM f_progress_bar_single USING gd_message 'S' 'E'.
              ENDIF.

              "*--------------------------------------------------------------------*

*              CALL FUNCTION 'ZFIFM_OB_SEND_KOMPARASI_LV4'
*                TABLES
*                  it_komparasi = git_data_sum_final_b[].

              "*--------------------------------------------------------------------*

*              PERFORM f_update_status_zfidt00268 USING 'SENT'
*                                                       sy-datum
*                                                       sy-uzeit
*
*                                                       s_rbukrs-low
*                                                       p_datum
*                                                       p_uzeit
*                                                       s_recid-low
*                                                       s_noref-low.

              "*--------------------------------------------------------------------*

              PERFORM f_stop_timer.
              "001: This program sucessfully executed! (Exec. Time & seconds)
              MESSAGE s071(zfimsg) WITH gd_run.

              PERFORM f_display_data USING git_data_detail[]
                                           git_data_pre_detail[]
                                           git_data_sum_final[]
                                           git_data_sum_final_b[].
            ELSE.
              MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
            ENDIF.

        ENDCASE.

    ENDCASE.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_DATA[]
*&---------------------------------------------------------------------*
FORM f_get_data    USING p_flag_peryear
                         p_year
                CHANGING p_git_zfidt00242 TYPE gtt_zfidt00242
*                         p_git_zfidt00269 TYPE gtt_zfidt00269
*                         p_git_acdoca_ra TYPE gtt_acdoca_ra
                         p_git_acdoca_oi TYPE gtt_acdoca_oi
*                         p_git_acdoca_bs TYPE gtt_acdoca_oi
                         p_git_acdoca_pl TYPE gtt_acdoca_oi
                         p_git_acdoca TYPE gtt_acdoca

                         p_git_zfidt00242_x TYPE gtt_data_detail_x
                         p_git_acdoca_oi_x TYPE gtt_data_detail_x
                         p_git_acdoca_pl_x TYPE gtt_data_detail_x

                         p_git_acdoca_x_lv4 TYPE gtt_data_detail_x_lv4.

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

*--------------------------------------------------------------------*
*Get Data ZFIDT00242

      PERFORM f_progress_bar_single USING 'Getting Data from ACDOCA 242...' 'S' 'S'.

      PERFORM f_get_data_from_table    USING 'ACDOCA 242'
                                             p_flag_peryear
                                             p_year
                                    CHANGING p_git_zfidt00242[]
*                                             p_git_zfidt00269[]
*                                             p_git_acdoca_ra[]
                                             p_git_acdoca_oi[]
*                                             p_git_acdoca_bs[]
                                             p_git_acdoca_pl[]
                                             p_git_acdoca[]

                                             p_git_zfidt00242_x[]
                                             p_git_acdoca_oi_x[]
                                             p_git_acdoca_pl_x[]
                                             p_git_acdoca_x_lv4[].

*--------------------------------------------------------------------*
*Get Data ZFIDT00269

*****      PERFORM f_progress_bar_single USING 'Getting Data from ZFIDT00269...'.
*****
*****      PERFORM f_get_data_from_table    USING 'ZFIDT00269'
*****                                    CHANGING p_git_zfidt00242[]
*****                                             p_git_zfidt00269[]
*****                                             p_git_acdoca_ra[]
*****                                             p_git_acdoca_oi[]
*****                                             p_git_acdoca_bs[]
*****                                             p_git_acdoca_pl[]
*****                                             p_git_acdoca[].

*--------------------------------------------------------------------*
*Get Data ACDOCA RECON ACCOUNT

*****      PERFORM f_progress_bar_single USING 'Getting Data from ACDOCA RECON ACCOUNT...'.
*****
*****      PERFORM f_get_data_from_table    USING 'ACDOCA RA'
*****                                    CHANGING p_git_zfidt00242[]
*****                                             p_git_zfidt00269[]
*****                                             p_git_acdoca_ra[]
*****                                             p_git_acdoca_oi[]
*****                                             p_git_acdoca_bs[]
*****                                             p_git_acdoca_pl[]
*****                                             p_git_acdoca[].

*--------------------------------------------------------------------*
*Get Data ACDOCA OPEN ITEM

      PERFORM f_progress_bar_single USING 'Getting Data from ACDOCA OPEN ITEM...' 'S' 'S'.

      PERFORM f_get_data_from_table    USING 'ACDOCA OI'
                                             p_flag_peryear
                                             p_year
                                    CHANGING p_git_zfidt00242[]
*                                             p_git_zfidt00269[]
*                                             p_git_acdoca_ra[]
                                             p_git_acdoca_oi[]
*                                             p_git_acdoca_bs[]
                                             p_git_acdoca_pl[]
                                             p_git_acdoca[]

                                             p_git_zfidt00242_x[]
                                             p_git_acdoca_oi_x[]
                                             p_git_acdoca_pl_x[]
                                             p_git_acdoca_x_lv4[].

*--------------------------------------------------------------------*
*Get Data ACDOCA BALANCE SHEET

*****      PERFORM f_progress_bar_single USING 'Getting Data from ACDOCA BALANCE SHEET...'.
*****
*****      PERFORM f_get_data_from_table    USING 'ACDOCA BS'
*****                                    CHANGING p_git_zfidt00242[]
*****                                             p_git_zfidt00269[]
*****                                             p_git_acdoca_ra[]
*****                                             p_git_acdoca_oi[]
*****                                             p_git_acdoca_bs[]
*****                                             p_git_acdoca_pl[]
*****                                             p_git_acdoca[].

*--------------------------------------------------------------------*
*Get Data ACDOCA PROFIT LOSS

      PERFORM f_progress_bar_single USING 'Getting Data from ACDOCA PROFIT LOSS...' 'S' 'S'.

      PERFORM f_get_data_from_table    USING 'ACDOCA PL'
                                             p_flag_peryear
                                             p_year
                                    CHANGING p_git_zfidt00242[]
*                                             p_git_zfidt00269[]
*                                             p_git_acdoca_ra[]
                                             p_git_acdoca_oi[]
*                                             p_git_acdoca_bs[]
                                             p_git_acdoca_pl[]
                                             p_git_acdoca[]

                                             p_git_zfidt00242_x[]
                                             p_git_acdoca_oi_x[]
                                             p_git_acdoca_pl_x[]
                                             p_git_acdoca_x_lv4[].

*--------------------------------------------------------------------*

    WHEN 4.

*--------------------------------------------------------------------*
*Get Data ACDOCA

      PERFORM f_progress_bar_single USING 'Getting Data from ACDOCA...' 'S' 'S'.

      PERFORM f_get_data_from_table    USING 'ACDOCA'
                                             p_flag_peryear
                                             p_year
                                    CHANGING p_git_zfidt00242[]
*                                             p_git_zfidt00269[]
*                                             p_git_acdoca_ra[]
                                             p_git_acdoca_oi[]
*                                             p_git_acdoca_bs[]
                                             p_git_acdoca_pl[]
                                             p_git_acdoca[]

                                             p_git_zfidt00242_x[]
                                             p_git_acdoca_oi_x[]
                                             p_git_acdoca_pl_x[]
                                             p_git_acdoca_x_lv4[].

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA_FROM_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- GIT_ZFIDT00242[]
*&---------------------------------------------------------------------*
FORM f_get_data_from_table  USING    p_kind
                                     p_flag_peryear
                                     p_year
                            CHANGING p_git_zfidt00242 TYPE gtt_zfidt00242
*                                     p_git_zfidt00269 TYPE gtt_zfidt00269
*                                     p_git_acdoca_ra TYPE gtt_acdoca_ra
                                     p_git_acdoca_oi TYPE gtt_acdoca_oi
*                                     p_git_acdoca_bs TYPE gtt_acdoca_oi
                                     p_git_acdoca_pl TYPE gtt_acdoca_oi
                                     p_git_acdoca TYPE gtt_acdoca

                                      p_git_zfidt00242_x TYPE gtt_data_detail_x
                                      p_git_acdoca_oi_x TYPE gtt_data_detail_x
                                      p_git_acdoca_pl_x TYPE gtt_data_detail_x
                                      p_git_acdoca_x_lv4 TYPE gtt_data_detail_x_lv4.

  DATA: ld_poper TYPE acdoca-poper.

*--------------------------------------------------------------------*

  CLEAR: gd_where,
         gd_where1,
         gd_where2,
         gd_where3,
         gd_where4,
         gd_where5,
         git_named_seltabs[].

  CASE p_kind.
    WHEN 'ACDOCA 242'. "Engine Yearly

      LOOP AT git_field_map INTO DATA(lwa_map_field).

        gd_tabix = sy-tabix.

        CLEAR: git_named_seltabs[].
        CASE gd_tabix.
          WHEN 1.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value1[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where1 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 2.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value2[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where2 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 3.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value3[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where3 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 4.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value4[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where4 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 5.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value5[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where5 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
        ENDCASE.

      ENDLOOP.

      "*--------------------------------------------------------------------*

      CLEAR: gwa_named_seltabs, git_named_seltabs[].
*      gwa_named_seltabs-name = 'SAKNR'.
      gwa_named_seltabs-name = 'RACCT'.
      gwa_named_seltabs-dref = REF #( gra_racct[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.

      TRY.
          gd_where = cl_shdb_seltab=>combine_seltabs(
            EXPORTING it_named_seltabs = git_named_seltabs[]
                      iv_client_field = 'RCLNT'
          ).
        CATCH cx_shdb_exception.
      ENDTRY.

*****      TRY.
*****          CLEAR p_git_zfidt00242[].
*****          CALL METHOD zficl_amdp_012=>get_zfidt00242
*****            EXPORTING
*****              im_mandt      = sy-mandt
*****              im_rldnr      = '0L'
*****              im_gjahr      = s_baldat-low(4)
*****              im_bukrs      = gc_rbukrs
*****              im_where      = gd_where
*****              im_where1     = gd_where1
*****              im_where2     = gd_where2
*****              im_where3     = gd_where3
*****              im_where4     = gd_where4
*****              im_where5     = gd_where5
*****            IMPORTING
*****              et_zfidt00242 = p_git_zfidt00242.
*****        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
*****      ENDTRY.

      IF gd_rb EQ 'NO TRACING'.

        TRY.
            CLEAR p_git_zfidt00242_x[].
            CALL METHOD zficl_amdp_012=>get_acdoca_x
              EXPORTING
                im_flag_peryear = p_flag_peryear
                im_kind   = 'ACDOCA 242'
                im_mandt  = sy-mandt
                im_rldnr  = '0L'
                im_bukrs  = gc_rbukrs
*               im_gjahr  = s_baldat-low(4)
                im_gjahr  = p_year
                im_poper  = '000'
                im_budat  = s_baldat-low
                im_augdt  = '00000000'
                im_where  = gd_where
                im_where1 = gd_where1
                im_where2 = gd_where2
                im_where3 = gd_where3
                im_where4 = gd_where4
                im_where5 = gd_where5
              IMPORTING
                et_acdoca = p_git_zfidt00242_x.
          CATCH cx_amdp_error. " Exceptions when calling AMDP methods
        ENDTRY.

*        SORT p_git_zfidt00242_x ASCENDING BY bktxt zuonr
*                                             zzku zzcp zzpr zzch zzpo zzcc zz07 zz08 zz09 zz10.

      ELSE.

        TRY.
            CLEAR p_git_zfidt00242[].
            CALL METHOD zficl_amdp_012=>get_acdoca
              EXPORTING
                im_flag_peryear = p_flag_peryear
                im_kind   = 'ACDOCA 242'
                im_mandt  = sy-mandt
                im_rldnr  = '0L'
                im_bukrs  = gc_rbukrs
*               im_gjahr  = s_baldat-low(4)
                im_gjahr  = p_year
                im_poper  = '000'
                im_budat  = s_baldat-low
                im_augdt  = '00000000'
                im_where  = gd_where
                im_where1 = gd_where1
                im_where2 = gd_where2
                im_where3 = gd_where3
                im_where4 = gd_where4
                im_where5 = gd_where5
              IMPORTING
                et_acdoca = p_git_zfidt00242.
          CATCH cx_amdp_error. " Exceptions when calling AMDP methods
        ENDTRY.

*        SORT p_git_zfidt00242 ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

      ENDIF.


*****    WHEN 'ZFIDT00269'. "Engine Monthly
*****
*****      LOOP AT git_field_map INTO lwa_map_field.
*****
*****        gd_tabix = sy-tabix.
*****
*****        CLEAR: git_named_seltabs[].
*****        CASE gd_tabix.
*****          WHEN 1.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value1[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where1 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 2.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value2[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where2 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 3.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value3[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where3 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 4.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value4[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where4 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 5.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value5[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where5 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****        ENDCASE.
*****
*****      ENDLOOP.
*****
*****      "*--------------------------------------------------------------------*
*****
*****      CLEAR: gwa_named_seltabs, git_named_seltabs[].
******      gwa_named_seltabs-name = 'SAKNR'.
*****      gwa_named_seltabs-name = 'RACCT'.
*****      gwa_named_seltabs-dref = REF #( gra_racct[] ).
*****      APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****      TRY.
*****          gd_where = cl_shdb_seltab=>combine_seltabs(
*****            EXPORTING it_named_seltabs = git_named_seltabs[]
*****                      iv_client_field = 'RCLNT'
*****          ).
*****        CATCH cx_shdb_exception.
*****      ENDTRY.
*****
*****      ld_poper = s_baldat-low+4(2).
*****
**********      TRY.
**********          CLEAR p_git_zfidt00269[].
**********          CALL METHOD zficl_amdp_012=>get_zfidt00269
**********            EXPORTING
**********              im_mandt      = sy-mandt
**********              im_rldnr      = '0L'
**********              im_gjahr      = s_baldat-low(4)
**********              im_poper      = ld_poper
**********              im_bukrs      = gc_rbukrs
**********              im_where      = gd_where
**********              im_where1     = gd_where1
**********              im_where2     = gd_where2
**********              im_where3     = gd_where3
**********              im_where4     = gd_where4
**********              im_where5     = gd_where5
**********            IMPORTING
**********              et_zfidt00269 = p_git_zfidt00269.
**********        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
**********      ENDTRY.
*****
*****      TRY.
*****          CLEAR p_git_zfidt00269[].
*****          CALL METHOD zficl_amdp_012=>get_acdoca
*****            EXPORTING
*****              im_kind   = 'ACDOCA 269'
*****              im_mandt  = sy-mandt
*****              im_rldnr  = '0L'
*****              im_bukrs  = gc_rbukrs
*****              im_gjahr  = s_baldat-low(4)
*****              im_poper  = ld_poper
*****              im_budat  = '00000000'
*****              im_augdt  = '00000000'
*****              im_where  = gd_where
*****              im_where1 = gd_where1
*****              im_where2 = gd_where2
*****              im_where3 = gd_where3
*****              im_where4 = gd_where4
*****              im_where5 = gd_where5
*****            IMPORTING
*****              et_acdoca = p_git_zfidt00269.
*****        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
*****      ENDTRY.
*****
*****      SORT p_git_zfidt00269 ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

*****    WHEN 'ACDOCA RA'. "ACDOCA RECON ACCOUNT
*****
*****      LOOP AT git_field_map INTO lwa_map_field.
*****
*****        gd_tabix = sy-tabix.
*****
*****        CLEAR: git_named_seltabs[].
*****        CASE gd_tabix.
*****          WHEN 1.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value1[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where1 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 2.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value2[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where2 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 3.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value3[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where3 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 4.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value4[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where4 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 5.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value5[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where5 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****        ENDCASE.
*****
*****      ENDLOOP.
*****
*****      "*--------------------------------------------------------------------*
*****
*****      CLEAR: gwa_named_seltabs, git_named_seltabs[].
*****      gwa_named_seltabs-name = 'RACCT'.
*****      gwa_named_seltabs-dref = REF #( gra_racct[] ).
*****      APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****      TRY.
*****          gd_where = cl_shdb_seltab=>combine_seltabs(
*****            EXPORTING it_named_seltabs = git_named_seltabs[]
*****                      iv_client_field = 'RCLNT'
*****          ).
*****        CATCH cx_shdb_exception.
*****      ENDTRY.
*****
*****      ld_poper = s_baldat-low+4(2).
*****
*****      TRY.
*****          CLEAR p_git_acdoca_ra[].
*****          CALL METHOD zficl_amdp_012=>get_acdoca
*****            EXPORTING
*****              im_kind   = 'ACDOCA RA'
*****              im_mandt  = sy-mandt
*****              im_rldnr  = '0L'
*****              im_bukrs  = gc_rbukrs
*****              im_gjahr  = s_baldat-low(4)
*****              im_poper  = ld_poper
*****              im_budat  = s_baldat-low
*****              im_augdt  = s_baldat-low
*****              im_where  = gd_where
*****              im_where1 = gd_where1
*****              im_where2 = gd_where2
*****              im_where3 = gd_where3
*****              im_where4 = gd_where4
*****              im_where5 = gd_where5
*****            IMPORTING
*****              et_acdoca = p_git_acdoca_ra.
*****        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
*****      ENDTRY.
*****
*****      SORT p_git_acdoca_ra ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

    WHEN 'ACDOCA OI'. "ACDOCA OPEN ITEM

      LOOP AT git_field_map INTO lwa_map_field.

        gd_tabix = sy-tabix.

        CLEAR: git_named_seltabs[].
        CASE gd_tabix.
          WHEN 1.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value1[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where1 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 2.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value2[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where2 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 3.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value3[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where3 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 4.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value4[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where4 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 5.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value5[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where5 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
        ENDCASE.

      ENDLOOP.

      "*--------------------------------------------------------------------*

      CLEAR: gwa_named_seltabs, git_named_seltabs[].
      gwa_named_seltabs-name = 'RACCT'.
      gwa_named_seltabs-dref = REF #( gra_racct[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.

      TRY.
          gd_where = cl_shdb_seltab=>combine_seltabs(
            EXPORTING it_named_seltabs = git_named_seltabs[]
                      iv_client_field = 'RCLNT'
          ).
        CATCH cx_shdb_exception.
      ENDTRY.

      ld_poper = s_baldat-low+4(2).

      IF gd_rb EQ 'NO TRACING'.

        TRY.
            CLEAR p_git_acdoca_oi_x[].
            CALL METHOD zficl_amdp_012=>get_acdoca_x
              EXPORTING
                im_flag_peryear = p_flag_peryear
                im_kind   = 'ACDOCA OI'
                im_mandt  = sy-mandt
                im_rldnr  = '0L'
                im_bukrs  = gc_rbukrs
*               im_gjahr  = s_baldat-low(4)
                im_gjahr  = p_year
                im_poper  = ld_poper
                im_budat  = s_baldat-low
                im_augdt  = s_baldat-low
                im_where  = gd_where
                im_where1 = gd_where1
                im_where2 = gd_where2
                im_where3 = gd_where3
                im_where4 = gd_where4
                im_where5 = gd_where5
              IMPORTING
                et_acdoca = p_git_acdoca_oi_x.
          CATCH cx_amdp_error. " Exceptions when calling AMDP methods
        ENDTRY.

*        SORT p_git_acdoca_oi_x ASCENDING BY bktxt zuonr
*                                            zzku zzcp zzpr zzch zzpo zzcc zz07 zz08 zz09 zz10.

      ELSE.

        TRY.
            CLEAR p_git_acdoca_oi[].
            CALL METHOD zficl_amdp_012=>get_acdoca
              EXPORTING
                im_flag_peryear = p_flag_peryear
                im_kind   = 'ACDOCA OI'
                im_mandt  = sy-mandt
                im_rldnr  = '0L'
                im_bukrs  = gc_rbukrs
*               im_gjahr  = s_baldat-low(4)
                im_gjahr  = p_year
                im_poper  = ld_poper
                im_budat  = s_baldat-low
                im_augdt  = s_baldat-low
                im_where  = gd_where
                im_where1 = gd_where1
                im_where2 = gd_where2
                im_where3 = gd_where3
                im_where4 = gd_where4
                im_where5 = gd_where5
              IMPORTING
                et_acdoca = p_git_acdoca_oi.
          CATCH cx_amdp_error. " Exceptions when calling AMDP methods
        ENDTRY.

*        SORT p_git_acdoca_oi ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

      ENDIF.


*****    WHEN 'ACDOCA BS'. "ACDOCA BALANCE SHEET
*****
*****      LOOP AT git_field_map INTO lwa_map_field.
*****
*****        gd_tabix = sy-tabix.
*****
*****        CLEAR: git_named_seltabs[].
*****        CASE gd_tabix.
*****          WHEN 1.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value1[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where1 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 2.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value2[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where2 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 3.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value3[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where3 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 4.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value4[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where4 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****          WHEN 5.
*****            CLEAR gwa_named_seltabs.
*****            gwa_named_seltabs-name = lwa_map_field-field_sap.
*****            gwa_named_seltabs-dref = REF #( gra_value5[] ).
*****            APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****            TRY.
*****                gd_where5 = cl_shdb_seltab=>combine_seltabs(
*****                  EXPORTING it_named_seltabs = git_named_seltabs[]
*****                ).
*****              CATCH cx_shdb_exception.
*****            ENDTRY.
*****        ENDCASE.
*****
*****      ENDLOOP.
*****
*****      "*--------------------------------------------------------------------*
*****
*****      CLEAR: gwa_named_seltabs, git_named_seltabs[].
*****      gwa_named_seltabs-name = 'RACCT'.
*****      gwa_named_seltabs-dref = REF #( gra_racct[] ).
*****      APPEND gwa_named_seltabs TO git_named_seltabs.
*****
*****      TRY.
*****          gd_where = cl_shdb_seltab=>combine_seltabs(
*****            EXPORTING it_named_seltabs = git_named_seltabs[]
*****                      iv_client_field = 'RCLNT'
*****          ).
*****        CATCH cx_shdb_exception.
*****      ENDTRY.
*****
*****      ld_poper = s_baldat-low+4(2).
*****
*****      TRY.
*****          CLEAR p_git_acdoca_bs[].
*****          CALL METHOD zficl_amdp_012=>get_acdoca
*****            EXPORTING
*****              im_kind   = 'ACDOCA BS'
*****              im_mandt  = sy-mandt
*****              im_rldnr  = '0L'
*****              im_bukrs  = gc_rbukrs
*****              im_gjahr  = s_baldat-low(4)
*****              im_poper  = ld_poper
*****              im_budat  = s_baldat-low
*****              im_augdt  = s_baldat-low
*****              im_where  = gd_where
*****              im_where1 = gd_where1
*****              im_where2 = gd_where2
*****              im_where3 = gd_where3
*****              im_where4 = gd_where4
*****              im_where5 = gd_where5
*****            IMPORTING
*****              et_acdoca = p_git_acdoca_bs.
*****        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
*****      ENDTRY.
*****
*****      SORT p_git_acdoca_bs ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

    WHEN 'ACDOCA PL'. "ACDOCA PROFIT LOSS

      LOOP AT git_field_map INTO lwa_map_field.

        gd_tabix = sy-tabix.

        CLEAR: git_named_seltabs[].
        CASE gd_tabix.
          WHEN 1.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value1[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where1 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 2.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value2[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where2 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 3.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value3[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where3 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 4.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value4[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where4 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 5.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value5[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where5 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
        ENDCASE.

      ENDLOOP.

      "*--------------------------------------------------------------------*

      CLEAR: gwa_named_seltabs, git_named_seltabs[].
      gwa_named_seltabs-name = 'RACCT'.
      gwa_named_seltabs-dref = REF #( gra_racct[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.

      TRY.
          gd_where = cl_shdb_seltab=>combine_seltabs(
            EXPORTING it_named_seltabs = git_named_seltabs[]
                      iv_client_field = 'RCLNT'
          ).
        CATCH cx_shdb_exception.
      ENDTRY.

      ld_poper = s_baldat-low+4(2).

      IF gd_rb EQ 'NO TRACING'.

        TRY.
            CLEAR p_git_acdoca_pl_x[].
            CALL METHOD zficl_amdp_012=>get_acdoca_x
              EXPORTING
                im_flag_peryear = p_flag_peryear
                im_kind   = 'ACDOCA PL'
                im_mandt  = sy-mandt
                im_rldnr  = '0L'
                im_bukrs  = gc_rbukrs
*               im_gjahr  = s_baldat-low(4)
                im_gjahr  = p_year
                im_poper  = ld_poper
                im_budat  = s_baldat-low
                im_augdt  = s_baldat-low
                im_where  = gd_where
                im_where1 = gd_where1
                im_where2 = gd_where2
                im_where3 = gd_where3
                im_where4 = gd_where4
                im_where5 = gd_where5
              IMPORTING
                et_acdoca = p_git_acdoca_pl_x.
          CATCH cx_amdp_error. " Exceptions when calling AMDP methods
        ENDTRY.

*        SORT p_git_acdoca_oi_x ASCENDING BY bktxt zuonr
*                                            zzku zzcp zzpr zzch zzpo zzcc zz07 zz08 zz09 zz10.

      ELSE.

        TRY.
            CLEAR p_git_acdoca_pl[].
            CALL METHOD zficl_amdp_012=>get_acdoca
              EXPORTING
                im_flag_peryear = p_flag_peryear
                im_kind   = 'ACDOCA PL'
                im_mandt  = sy-mandt
                im_rldnr  = '0L'
                im_bukrs  = gc_rbukrs
*               im_gjahr  = s_baldat-low(4)
                im_gjahr  = p_year
                im_poper  = ld_poper
                im_budat  = s_baldat-low
                im_augdt  = s_baldat-low
                im_where  = gd_where
                im_where1 = gd_where1
                im_where2 = gd_where2
                im_where3 = gd_where3
                im_where4 = gd_where4
                im_where5 = gd_where5
              IMPORTING
                et_acdoca = p_git_acdoca_pl.
          CATCH cx_amdp_error. " Exceptions when calling AMDP methods
        ENDTRY.

*        SORT p_git_acdoca_pl ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

      ENDIF.


    WHEN 'ACDOCA'. "ACDOCA

      LOOP AT git_field_map INTO lwa_map_field.

        gd_tabix = sy-tabix.

        CLEAR: git_named_seltabs[].
        CASE gd_tabix.
          WHEN 1.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value1[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where1 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 2.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value2[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where2 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 3.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value3[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where3 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 4.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value4[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where4 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
          WHEN 5.
            CLEAR gwa_named_seltabs.
            gwa_named_seltabs-name = lwa_map_field-field_sap.
            gwa_named_seltabs-dref = REF #( gra_value5[] ).
            APPEND gwa_named_seltabs TO git_named_seltabs.

            TRY.
                gd_where5 = cl_shdb_seltab=>combine_seltabs(
                  EXPORTING it_named_seltabs = git_named_seltabs[]
                ).
              CATCH cx_shdb_exception.
            ENDTRY.
        ENDCASE.

      ENDLOOP.

      "*--------------------------------------------------------------------*

      CLEAR: gwa_named_seltabs, git_named_seltabs[].
      gwa_named_seltabs-name = 'RACCT'.
      gwa_named_seltabs-dref = REF #( gra_racct[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.

      TRY.
          gd_where = cl_shdb_seltab=>combine_seltabs(
            EXPORTING it_named_seltabs = git_named_seltabs[]
                      iv_client_field = 'RCLNT'
          ).
        CATCH cx_shdb_exception.
      ENDTRY.

*****      TRY.
*****          CLEAR p_git_acdoca[].
*****          CALL METHOD zficl_amdp_012=>get_acdoca
*****            EXPORTING
*****              im_kind   = 'ACDOCA'
*****              im_mandt  = sy-mandt
*****              im_rldnr  = '0L'
*****              im_bukrs  = gc_rbukrs
*****              im_gjahr  = s_baldat-low(4)
*****              im_poper  = '000'
*****              im_budat  = s_baldat-low
*****              im_augdt  = '00000000'
*****              im_where  = gd_where
*****              im_where1 = gd_where1
*****              im_where2 = gd_where2
*****              im_where3 = gd_where3
*****              im_where4 = gd_where4
*****              im_where5 = gd_where5
*****            IMPORTING
*****              et_acdoca = p_git_acdoca.
*****        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
*****      ENDTRY.
*****
*****      SORT p_git_acdoca ASCENDING BY rldnr rbukrs gjahr poper belnr buzei.

      TRY.
          CLEAR p_git_acdoca_x_lv4[].
          CALL METHOD zficl_amdp_012=>get_acdoca_x_lv4
            EXPORTING
              im_flag_peryear = p_flag_peryear
              im_kind   = 'ACDOCA'
              im_mandt  = sy-mandt
              im_rldnr  = '0L'
              im_bukrs  = gc_rbukrs
*             im_gjahr  = s_baldat-low(4)
              im_gjahr  = p_year
              im_poper  = '000'
              im_budat  = s_baldat-low
              im_augdt  = '00000000'
              im_where  = gd_where
              im_where1 = gd_where1
              im_where2 = gd_where2
              im_where3 = gd_where3
              im_where4 = gd_where4
              im_where5 = gd_where5
            IMPORTING
              et_acdoca = p_git_acdoca_x_lv4.
        CATCH cx_amdp_error. " Exceptions when calling AMDP methods
      ENDTRY.

*      SORT p_git_acdoca_x_lv4 ASCENDING BY gjahr belnr budat
*                                           bktxt zuonr.
**                                             zzku zzcp zzpr zzch zzpo zzcc zz07 zz08 zz09 zz10.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_SUM_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_combine_table USING p_git_zfidt00242 TYPE gtt_zfidt00242
*                           p_git_zfidt00269 TYPE gtt_zfidt00269
*                           p_git_acdoca_ra TYPE gtt_acdoca_ra
                           p_git_acdoca_oi TYPE gtt_acdoca_oi
*                           p_git_acdoca_bs TYPE gtt_acdoca_oi
                           p_git_acdoca_pl TYPE gtt_acdoca_oi
                           p_git_acdoca TYPE gtt_acdoca

                           p_git_zfidt00242_x TYPE gtt_data_detail_x
                           p_git_acdoca_oi_x TYPE gtt_data_detail_x
                           p_git_acdoca_pl_x TYPE gtt_data_detail_x
                           p_git_acdoca_x_lv4 TYPE gtt_data_detail_x_lv4

                  CHANGING p_git_data_detail TYPE gtt_data_detail
                           p_git_data_detail_x TYPE gtt_data_detail_x
                           p_git_data_detail_x_lv4 TYPE gtt_data_detail_x_lv4.

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

      PERFORM f_progress_bar_single USING 'Combining data from data already collected before...' 'S' 'S'.

      IF gd_rb EQ 'NO TRACING'.

        APPEND LINES OF p_git_zfidt00242_x[] TO p_git_data_detail_x[].
        APPEND LINES OF p_git_acdoca_oi_x[] TO p_git_data_detail_x[].
        APPEND LINES OF p_git_acdoca_pl_x[] TO p_git_data_detail_x[].

      ELSE.

        APPEND LINES OF p_git_zfidt00242[] TO p_git_data_detail[].
*      APPEND LINES OF p_git_zfidt00269[] TO p_git_data_detail[].
*      APPEND LINES OF p_git_acdoca_ra[] TO p_git_data_detail[].
        APPEND LINES OF p_git_acdoca_oi[] TO p_git_data_detail[].
*      APPEND LINES OF p_git_acdoca_bs[] TO p_git_data_detail[].
        APPEND LINES OF p_git_acdoca_pl[] TO p_git_data_detail[].

        SORT p_git_data_detail ASCENDING BY tabname rldnr rbukrs gjahr poper belnr buzei.

      ENDIF.

*--------------------------------------------------------------------*

    WHEN 4.
*****      APPEND LINES OF p_git_acdoca[] TO p_git_data_detail[].
      APPEND LINES OF p_git_acdoca_x_lv4[] TO p_git_data_detail_x_lv4[].
  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PROCESS_SUMMARY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_DATA_DETAIL[]
*&      <-- GIT_DATA_FINAL[]
*&---------------------------------------------------------------------*
FORM f_process_summary  USING p_git_data_detail TYPE gtt_data_detail
                              p_git_data_detail_x TYPE gtt_data_detail_x
                              p_git_data_detail_x_lv4 TYPE gtt_data_detail_x_lv4
                     CHANGING p_git_data_pre_detail TYPE gtt_data_detail
                              p_git_data_pre_detail_x TYPE gtt_data_detail_x
                              p_git_data_pre_detail_x_lv4 TYPE gtt_data_detail_x_lv4.


  TYPES: BEGIN OF gty_field_sap,
           field_sap TYPE zfidt00266-field_sap,
         END OF gty_field_sap.

  DATA: ld_fields TYPE string,
        ld_groups TYPE string.

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

      "*--------------------------------------------------------------------*
      "Get dynamic fields for summary

      CLEAR: ld_fields,
             ld_groups.
      PERFORM f_get_dynamic_fields USING 'A'
                                         git_field_sap_separate[]
                                CHANGING ld_fields
                                         ld_groups.

      "*--------------------------------------------------------------------*
      "Summarize from Data Detail

      PERFORM f_progress_bar_single USING 'Summarizing data from data after combined...' 'S' 'S'.

      IF gd_rb EQ 'NO TRACING'.
        SELECT (ld_fields)
          FROM @p_git_data_detail_x AS a
          GROUP BY (ld_groups)
            INTO CORRESPONDING FIELDS OF TABLE @p_git_data_pre_detail_x
          ##db_feature_mode[itabs_in_from_clause]
          ##itab_key_in_select
          ##itab_db_select.

        DELETE p_git_data_pre_detail_x WHERE hsl IS INITIAL.
      ELSE.
        SELECT (ld_fields)
          FROM @p_git_data_detail AS a
          GROUP BY (ld_groups)
            INTO CORRESPONDING FIELDS OF TABLE @p_git_data_pre_detail
          ##db_feature_mode[itabs_in_from_clause]
          ##itab_key_in_select
          ##itab_db_select.

        DELETE p_git_data_pre_detail WHERE hsl IS INITIAL.
      ENDIF.

    WHEN 4.

      "*--------------------------------------------------------------------*
      "Get dynamic fields for summary

      CLEAR: ld_fields,
             ld_groups.
      PERFORM f_get_dynamic_fields USING 'B'
                                         git_field_sap_separate[]
                                CHANGING ld_fields
                                         ld_groups.

      "*--------------------------------------------------------------------*
      "Summarize from Data Detail

      PERFORM f_progress_bar_single USING 'Summarizing data from data after getting data...' 'S' 'S'.

*****      SELECT (ld_fields)
*****        FROM @p_git_data_detail AS a
*****        GROUP BY (ld_groups)
*****          INTO CORRESPONDING FIELDS OF TABLE @p_git_data_pre_detail
*****        ##db_feature_mode[itabs_in_from_clause]
*****        ##itab_key_in_select
*****        ##itab_db_select.

      SELECT (ld_fields)
        FROM @p_git_data_detail_x_lv4 AS a
        GROUP BY (ld_groups)
          INTO CORRESPONDING FIELDS OF TABLE @p_git_data_pre_detail_x_lv4
        ##db_feature_mode[itabs_in_from_clause]
        ##itab_key_in_select
        ##itab_db_select.

*      DELETE p_git_data_pre_detail_x_lv4 WHERE hsl IS INITIAL.
  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_APPEND_SUMMARY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_DATA_PRE_DETAIL_X_TEMP[]
*&      --> LIT_DATA_PRE_DETAIL_X_LV4_TEMP
*&      <-- GIT_DATA_PRE_DETAIL_X[]
*&      <-- GIT_DATA_PRE_DETAIL_X_LV4[]
*&---------------------------------------------------------------------*
FORM f_append_summary  USING    p_lit_data_pre_detail_x TYPE gtt_data_detail_x
                                p_lit_data_pre_detail_x_lv4 TYPE gtt_data_detail_x_lv4
                       CHANGING p_git_data_pre_detail_x TYPE gtt_data_detail_x
                                p_git_data_pre_detail_x_lv4 TYPE gtt_data_detail_x_lv4.

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

      APPEND LINES OF p_lit_data_pre_detail_x[] TO p_git_data_pre_detail_x[].

    WHEN 4.

      APPEND LINES OF p_lit_data_pre_detail_x_lv4[] TO p_git_data_pre_detail_x_lv4[].

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DYNAMIC_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_FIELD_SAP_SEPARATE[]
*&      <-- LD_FIELDS
*&      <-- LD_GROUPS
*&---------------------------------------------------------------------*
FORM f_get_dynamic_fields  USING    p_kind
                                    p_git_field_sap_separate TYPE gtt_field_sap_separate
                           CHANGING p_ld_fields
                                    p_ld_groups.

  DATA: ld_fields_b TYPE string VALUE ', RHCUR, SUM( HSL ) AS HSL',
        ld_group_b  TYPE string VALUE ', RHCUR',
        ld_temp     TYPE string,
        ld_temp_b   TYPE string.

*--------------------------------------------------------------------*

  CLEAR gd_tabix.
  LOOP AT p_git_field_sap_separate INTO DATA(lwa_field_sap_separate).

    gd_tabix = sy-tabix.

    IF gd_tabix EQ 1.
      p_ld_fields = lwa_field_sap_separate-field_sap.
      p_ld_groups = lwa_field_sap_separate-field_sap.
    ELSE.

      ld_temp = p_ld_fields && ','.

      CONCATENATE ld_temp
                  lwa_field_sap_separate-field_sap
        INTO p_ld_fields
        SEPARATED BY space.

      "*--------------------------------------------------------------------*

      ld_temp_b = p_ld_groups && ','.

      CONCATENATE ld_temp_b
                  lwa_field_sap_separate-field_sap
        INTO p_ld_groups
        SEPARATED BY space.

    ENDIF.

  ENDLOOP.

  CASE p_kind.
    WHEN 'A'.
      "Do nothing
    WHEN 'B'.

      ld_temp = p_ld_fields && ','.

      CONCATENATE ld_temp
                  'BELNR'
        INTO p_ld_fields
        SEPARATED BY space.

      ld_temp = p_ld_fields && ','.

      CONCATENATE ld_temp
                  'BUDAT'
        INTO p_ld_fields
        SEPARATED BY space.

      ld_temp = p_ld_fields && ','.

      CONCATENATE ld_temp
                  'GJAHR'
        INTO p_ld_fields
        SEPARATED BY space.

      "*--------------------------------------------------------------------*

      ld_temp_b = p_ld_groups && ','.

      CONCATENATE ld_temp_b
                  'BELNR'
        INTO p_ld_groups
        SEPARATED BY space.

      ld_temp_b = p_ld_groups && ','.

      CONCATENATE ld_temp_b
                  'BUDAT'
        INTO p_ld_groups
        SEPARATED BY space.

      ld_temp_b = p_ld_groups && ','.

      CONCATENATE ld_temp_b
                  'GJAHR'
        INTO p_ld_groups
        SEPARATED BY space.

  ENDCASE.

*--------------------------------------------------------------------*

  p_ld_fields = p_ld_fields && ld_fields_b.
  p_ld_groups = p_ld_groups && ld_group_b.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PREPARE_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_DATA_PRE_DETAIL[]
*&      <-- GIT_DATA_SUM_FINAL[]
*&---------------------------------------------------------------------*
FORM f_prepare_alv  USING    p_git_data_pre_detail TYPE gtt_data_detail
                             p_git_data_pre_detail_x TYPE gtt_data_detail_x
                             p_git_data_pre_detail_x_lv4 TYPE gtt_data_detail_x_lv4
                    CHANGING p_git_data_sum_final TYPE gtt_data_sum_final
                             p_git_data_sum_final_b TYPE gtt_data_sum_final_b.

  FIELD-SYMBOLS: <lfs>.

  TYPES: BEGIN OF gty_field_sap,
           field_sap TYPE zfidt00266-field_sap,
         END OF gty_field_sap.

  DATA: ld_fs(100),
        ld_value_field_sap_combine TYPE string,
        ld_field_sap               TYPE string,
        ld_where                   TYPE string,
*        ld_seqno                   TYPE zfide01199,
*        ld_batch                   TYPE zfide01201,

        lit_field_sap_tmp          TYPE TABLE OF gty_field_sap.

*--------------------------------------------------------------------*

*  CLEAR ld_seqno.

*--------------------------------------------------------------------*
*Preparing to show ALV with Data Summary

  PERFORM f_progress_bar_single USING 'Preparing data to show ALV...' 'S' 'S'.

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

      IF gd_rb EQ 'NO TRACING'.

        CLEAR: gd_percent, gd_lines.
        DESCRIBE TABLE p_git_data_pre_detail_x LINES gd_lines.

        LOOP AT p_git_data_pre_detail_x INTO DATA(lwa_data_pre_detail_x).

*        ADD 1 TO ld_seqno.

          PERFORM f_progress_bar USING 'Preparing data to show ALV...'
                                        sy-tabix
                                        gd_lines.

          CLEAR gwa_data_sum_final.
          gwa_data_sum_final-bal_id = s_recid-low.
          gwa_data_sum_final-bal_level = s_zlevel-low.

*        gwa_data_sum_final-seqno = ld_seqno.

          LOOP AT git_field_sap_combine INTO DATA(lwa_field_sap_combine).

            CLEAR lit_field_sap_tmp[].
*          SPLIT lwa_field_sap_combine-field_sap AT '+' INTO TABLE lit_field_sap_tmp.
            SPLIT lwa_field_sap_combine-field_sap AT ',' INTO TABLE lit_field_sap_tmp.

            CLEAR ld_value_field_sap_combine.
            LOOP AT lit_field_sap_tmp INTO DATA(lwa_field_sap_tmp).

              gd_tabix = sy-tabix.

              CONCATENATE 'LWA_DATA_PRE_DETAIL_X' '-' lwa_field_sap_tmp INTO ld_fs.
              CONDENSE ld_fs NO-GAPS.
              ASSIGN (ld_fs) TO <lfs>.

              IF gd_tabix EQ 1.
                ld_value_field_sap_combine = <lfs>.
              ELSE.
                ld_value_field_sap_combine = ld_value_field_sap_combine && ',' && <lfs>.
              ENDIF.

            ENDLOOP.

            CASE lwa_field_sap_combine-zlevel.
              WHEN 1.
                gwa_data_sum_final-bal_unit_id_lvl1 = ld_value_field_sap_combine.
              WHEN 2.
                gwa_data_sum_final-bal_unit_id_lvl2 = ld_value_field_sap_combine.
              WHEN 3.
                gwa_data_sum_final-bal_unit_id_lvl3 = ld_value_field_sap_combine.
            ENDCASE.

          ENDLOOP.

          IF gwa_data_sum_final-bal_unit_id_lvl1 IS INITIAL.
*          gwa_data_sum_final-bal_unit_id_lvl1 = 'NULL'.
            gwa_data_sum_final-bal_unit_id_lvl1 = ''.
          ENDIF.

          IF gwa_data_sum_final-bal_unit_id_lvl2 IS INITIAL.
*          gwa_data_sum_final-bal_unit_id_lvl2 = 'NULL'.
            gwa_data_sum_final-bal_unit_id_lvl2 = ''.
          ENDIF.

          IF gwa_data_sum_final-bal_unit_id_lvl3 IS INITIAL.
*          gwa_data_sum_final-bal_unit_id_lvl3 = 'NULL'.
            gwa_data_sum_final-bal_unit_id_lvl3 = ''.
          ENDIF.

          gwa_data_sum_final-bal_source = s_source-low.
          gwa_data_sum_final-bal_date_b = s_baldat-low.
          gwa_data_sum_final-bal_date = s_baldat-low - 1.

          IF lwa_data_pre_detail_x-hsl IS NOT INITIAL.
            gwa_data_sum_final-bal_currency = lwa_data_pre_detail_x-rhcur.
            gwa_data_sum_final-bal_currency_b = lwa_data_pre_detail_x-rhcur.

            WRITE lwa_data_pre_detail_x-hsl TO gwa_data_sum_final-bal_amount CURRENCY lwa_data_pre_detail_x-rhcur.
            REPLACE ALL OCCURRENCES OF '.' IN gwa_data_sum_final-bal_amount WITH ''.
            CONDENSE gwa_data_sum_final-bal_amount.

            IF lwa_data_pre_detail_x-hsl < 0.
              REPLACE ALL OCCURRENCES OF '-' IN gwa_data_sum_final-bal_amount WITH ''.
              gwa_data_sum_final-bal_amount = '-' && gwa_data_sum_final-bal_amount.
            ENDIF.

            gwa_data_sum_final-bal_amount_b = lwa_data_pre_detail_x-hsl.
          ELSE.
            gwa_data_sum_final-bal_currency = 'NULL'.
            gwa_data_sum_final-bal_currency_b = lwa_data_pre_detail_x-rhcur.
            gwa_data_sum_final-bal_amount = 'NULL'.
            gwa_data_sum_final-bal_amount_b = 0.
          ENDIF.

          gwa_data_sum_final-bal_no_reff = s_noref-low.
          APPEND gwa_data_sum_final TO p_git_data_sum_final.

          "*--------------------------------------------------------------------*
          "Add flag which data match with selection screen

          CASE s_zlevel-low.
            WHEN 2.

              READ TABLE git_unit_level INTO gwa_unit_level WITH KEY unit_id_lvl_1 = gwa_data_sum_final-bal_unit_id_lvl1.
              IF sy-subrc EQ 0.
                gwa_unit_level-flag_exist = 'X'.
                MODIFY git_unit_level FROM gwa_unit_level INDEX sy-tabix.
              ENDIF.

            WHEN 3.

              READ TABLE git_unit_level INTO gwa_unit_level WITH KEY unit_id_lvl_1 = gwa_data_sum_final-bal_unit_id_lvl1
                                                                     unit_id_lvl_2 = gwa_data_sum_final-bal_unit_id_lvl2.
              IF sy-subrc EQ 0.
                gwa_unit_level-flag_exist = 'X'.
                MODIFY git_unit_level FROM gwa_unit_level INDEX sy-tabix.
              ENDIF.

          ENDCASE.

          "*--------------------------------------------------------------------*

        ENDLOOP.

*****        "*--------------------------------------------------------------------*
*****        "Add data which is not existing in SAP
*****
*****        CASE s_zlevel-low.
*****          WHEN 2. "Level 2
*****
*****            LOOP AT git_unit_level INTO gwa_unit_level WHERE flag_exist EQ space.
*****
*****              CLEAR gwa_data_sum_final.
*****              gwa_data_sum_final-bal_id = s_recid-low.
*****              gwa_data_sum_final-bal_level = s_zlevel-low.
*****              gwa_data_sum_final-bal_unit_id_lvl1 = gwa_unit_level-unit_id_lvl_1.
******            gwa_data_sum_final-bal_unit_id_lvl2 = 'NULL'.
*****              gwa_data_sum_final-bal_unit_id_lvl2 = ''.
******            gwa_data_sum_final-bal_unit_id_lvl3 = 'NULL'.
*****              gwa_data_sum_final-bal_unit_id_lvl3 = ''.
*****              gwa_data_sum_final-bal_source = s_source-low.
*****              gwa_data_sum_final-bal_date_b = s_baldat-low.
*****              gwa_data_sum_final-bal_date = s_baldat-low - 1.
*****              gwa_data_sum_final-bal_currency = 'NULL'.
*****              gwa_data_sum_final-bal_currency_b = ''.
*****              gwa_data_sum_final-bal_amount = 'NULL'.
*****              gwa_data_sum_final-bal_amount_b = 0.
*****              gwa_data_sum_final-bal_no_reff = s_noref-low.
*****              APPEND gwa_data_sum_final TO p_git_data_sum_final.
*****
*****            ENDLOOP.
*****
*****          WHEN 3. "Level 3
*****
*****            LOOP AT git_unit_level INTO gwa_unit_level WHERE flag_exist EQ space.
*****
*****              CLEAR gwa_data_sum_final.
*****              gwa_data_sum_final-bal_id = s_recid-low.
*****              gwa_data_sum_final-bal_level = s_zlevel-low.
*****              gwa_data_sum_final-bal_unit_id_lvl1 = gwa_unit_level-unit_id_lvl_1.
*****              gwa_data_sum_final-bal_unit_id_lvl2 = gwa_unit_level-unit_id_lvl_2.
******            gwa_data_sum_final-bal_unit_id_lvl3 = 'NULL'.
*****              gwa_data_sum_final-bal_unit_id_lvl3 = ''.
*****              gwa_data_sum_final-bal_source = s_source-low.
*****              gwa_data_sum_final-bal_date_b = s_baldat-low.
*****              gwa_data_sum_final-bal_date = s_baldat-low - 1.
*****              gwa_data_sum_final-bal_currency = 'NULL'.
*****              gwa_data_sum_final-bal_currency_b = ''.
*****              gwa_data_sum_final-bal_amount = 'NULL'.
*****              gwa_data_sum_final-bal_amount_b = 0.
*****              gwa_data_sum_final-bal_no_reff = s_noref-low.
*****              APPEND gwa_data_sum_final TO p_git_data_sum_final.
*****
*****            ENDLOOP.
*****
*****        ENDCASE.

        "*--------------------------------------------------------------------*

        SORT p_git_data_sum_final ASCENDING BY bal_unit_id_lvl1 bal_unit_id_lvl2 bal_unit_id_lvl3.

      ELSE.

        CLEAR: gd_percent, gd_lines.
        DESCRIBE TABLE p_git_data_pre_detail LINES gd_lines.

        LOOP AT p_git_data_pre_detail INTO DATA(lwa_data_pre_detail).

*        ADD 1 TO ld_seqno.

          PERFORM f_progress_bar USING 'Preparing data to show ALV...'
                                        sy-tabix
                                        gd_lines.

          CLEAR gwa_data_sum_final.
          gwa_data_sum_final-bal_id = s_recid-low.
          gwa_data_sum_final-bal_level = s_zlevel-low.

*        gwa_data_sum_final-seqno = ld_seqno.

*          LOOP AT git_field_sap_combine INTO DATA(lwa_field_sap_combine).
          LOOP AT git_field_sap_combine INTO lwa_field_sap_combine.

            CLEAR lit_field_sap_tmp[].
*          SPLIT lwa_field_sap_combine-field_sap AT '+' INTO TABLE lit_field_sap_tmp.
            SPLIT lwa_field_sap_combine-field_sap AT ',' INTO TABLE lit_field_sap_tmp.

            CLEAR ld_value_field_sap_combine.
*            LOOP AT lit_field_sap_tmp INTO DATA(lwa_field_sap_tmp).
            LOOP AT lit_field_sap_tmp INTO lwa_field_sap_tmp.

              gd_tabix = sy-tabix.

              CONCATENATE 'LWA_DATA_PRE_DETAIL' '-' lwa_field_sap_tmp INTO ld_fs.
              CONDENSE ld_fs NO-GAPS.
              ASSIGN (ld_fs) TO <lfs>.

              IF gd_tabix EQ 1.
                ld_value_field_sap_combine = <lfs>.
              ELSE.
                ld_value_field_sap_combine = ld_value_field_sap_combine && ',' && <lfs>.
              ENDIF.

            ENDLOOP.

            CASE lwa_field_sap_combine-zlevel.
              WHEN 1.
                gwa_data_sum_final-bal_unit_id_lvl1 = ld_value_field_sap_combine.
              WHEN 2.
                gwa_data_sum_final-bal_unit_id_lvl2 = ld_value_field_sap_combine.
              WHEN 3.
                gwa_data_sum_final-bal_unit_id_lvl3 = ld_value_field_sap_combine.
            ENDCASE.

          ENDLOOP.

          IF gwa_data_sum_final-bal_unit_id_lvl1 IS INITIAL.
*          gwa_data_sum_final-bal_unit_id_lvl1 = 'NULL'.
            gwa_data_sum_final-bal_unit_id_lvl1 = ''.
          ENDIF.

          IF gwa_data_sum_final-bal_unit_id_lvl2 IS INITIAL.
*          gwa_data_sum_final-bal_unit_id_lvl2 = 'NULL'.
            gwa_data_sum_final-bal_unit_id_lvl2 = ''.
          ENDIF.

          IF gwa_data_sum_final-bal_unit_id_lvl3 IS INITIAL.
*          gwa_data_sum_final-bal_unit_id_lvl3 = 'NULL'.
            gwa_data_sum_final-bal_unit_id_lvl3 = ''.
          ENDIF.

          gwa_data_sum_final-bal_source = s_source-low.
          gwa_data_sum_final-bal_date_b = s_baldat-low.
          gwa_data_sum_final-bal_date = s_baldat-low - 1.

          IF lwa_data_pre_detail-hsl IS NOT INITIAL.
            gwa_data_sum_final-bal_currency = lwa_data_pre_detail-rhcur.
            gwa_data_sum_final-bal_currency_b = lwa_data_pre_detail-rhcur.

            WRITE lwa_data_pre_detail-hsl TO gwa_data_sum_final-bal_amount CURRENCY lwa_data_pre_detail-rhcur.
            REPLACE ALL OCCURRENCES OF '.' IN gwa_data_sum_final-bal_amount WITH ''.
            CONDENSE gwa_data_sum_final-bal_amount.

            IF lwa_data_pre_detail-hsl < 0.
              REPLACE ALL OCCURRENCES OF '-' IN gwa_data_sum_final-bal_amount WITH ''.
              gwa_data_sum_final-bal_amount = '-' && gwa_data_sum_final-bal_amount.
            ENDIF.

            gwa_data_sum_final-bal_amount_b = lwa_data_pre_detail-hsl.
          ELSE.
            gwa_data_sum_final-bal_currency = 'NULL'.
            gwa_data_sum_final-bal_currency_b = lwa_data_pre_detail-rhcur.
            gwa_data_sum_final-bal_amount = 'NULL'.
            gwa_data_sum_final-bal_amount_b = 0.
          ENDIF.

          gwa_data_sum_final-bal_no_reff = s_noref-low.
          APPEND gwa_data_sum_final TO p_git_data_sum_final.

          "*--------------------------------------------------------------------*
          "Add flag which data match with selection screen

          CASE s_zlevel-low.
            WHEN 2.

              READ TABLE git_unit_level INTO gwa_unit_level WITH KEY unit_id_lvl_1 = gwa_data_sum_final-bal_unit_id_lvl1.
              IF sy-subrc EQ 0.
                gwa_unit_level-flag_exist = 'X'.
                MODIFY git_unit_level FROM gwa_unit_level INDEX sy-tabix.
              ENDIF.

            WHEN 3.

              READ TABLE git_unit_level INTO gwa_unit_level WITH KEY unit_id_lvl_1 = gwa_data_sum_final-bal_unit_id_lvl1
                                                                     unit_id_lvl_2 = gwa_data_sum_final-bal_unit_id_lvl2.
              IF sy-subrc EQ 0.
                gwa_unit_level-flag_exist = 'X'.
                MODIFY git_unit_level FROM gwa_unit_level INDEX sy-tabix.
              ENDIF.

          ENDCASE.

          "*--------------------------------------------------------------------*

        ENDLOOP.

*****        "*--------------------------------------------------------------------*
*****        "Add data which is not existing in SAP
*****
*****        CASE s_zlevel-low.
*****          WHEN 2. "Level 2
*****
*****            LOOP AT git_unit_level INTO gwa_unit_level WHERE flag_exist EQ space.
*****
*****              CLEAR gwa_data_sum_final.
*****              gwa_data_sum_final-bal_id = s_recid-low.
*****              gwa_data_sum_final-bal_level = s_zlevel-low.
*****              gwa_data_sum_final-bal_unit_id_lvl1 = gwa_unit_level-unit_id_lvl_1.
******            gwa_data_sum_final-bal_unit_id_lvl2 = 'NULL'.
*****              gwa_data_sum_final-bal_unit_id_lvl2 = ''.
******            gwa_data_sum_final-bal_unit_id_lvl3 = 'NULL'.
*****              gwa_data_sum_final-bal_unit_id_lvl3 = ''.
*****              gwa_data_sum_final-bal_source = s_source-low.
*****              gwa_data_sum_final-bal_date_b = s_baldat-low.
*****              gwa_data_sum_final-bal_date = s_baldat-low - 1.
*****              gwa_data_sum_final-bal_currency = 'NULL'.
*****              gwa_data_sum_final-bal_currency_b = ''.
*****              gwa_data_sum_final-bal_amount = 'NULL'.
*****              gwa_data_sum_final-bal_amount_b = 0.
*****              gwa_data_sum_final-bal_no_reff = s_noref-low.
*****              APPEND gwa_data_sum_final TO p_git_data_sum_final.
*****
*****            ENDLOOP.
*****
*****          WHEN 3. "Level 3
*****
*****            LOOP AT git_unit_level INTO gwa_unit_level WHERE flag_exist EQ space.
*****
*****              CLEAR gwa_data_sum_final.
*****              gwa_data_sum_final-bal_id = s_recid-low.
*****              gwa_data_sum_final-bal_level = s_zlevel-low.
*****              gwa_data_sum_final-bal_unit_id_lvl1 = gwa_unit_level-unit_id_lvl_1.
*****              gwa_data_sum_final-bal_unit_id_lvl2 = gwa_unit_level-unit_id_lvl_2.
******            gwa_data_sum_final-bal_unit_id_lvl3 = 'NULL'.
*****              gwa_data_sum_final-bal_unit_id_lvl3 = ''.
*****              gwa_data_sum_final-bal_source = s_source-low.
*****              gwa_data_sum_final-bal_date_b = s_baldat-low.
*****              gwa_data_sum_final-bal_date = s_baldat-low - 1.
*****              gwa_data_sum_final-bal_currency = 'NULL'.
*****              gwa_data_sum_final-bal_currency_b = ''.
*****              gwa_data_sum_final-bal_amount = 'NULL'.
*****              gwa_data_sum_final-bal_amount_b = 0.
*****              gwa_data_sum_final-bal_no_reff = s_noref-low.
*****              APPEND gwa_data_sum_final TO p_git_data_sum_final.
*****
*****            ENDLOOP.
*****
*****        ENDCASE.

        "*--------------------------------------------------------------------*

        SORT p_git_data_sum_final ASCENDING BY bal_unit_id_lvl1 bal_unit_id_lvl2 bal_unit_id_lvl3.

      ENDIF.

    WHEN 4.

*****      IF gra_unit_level[] IS NOT INITIAL.
*****
*****        READ TABLE git_field_sap_combine INTO lwa_field_sap_combine
*****          WITH KEY zlevel = 3.
*****
*****        ld_field_sap = lwa_field_sap_combine-field_sap.
*****
*****        "*--------------------------------------------------------------------*
*****
*****        CLEAR: gd_percent, gd_lines.
*****        DESCRIBE TABLE gra_unit_level LINES gd_lines.
*****
*****        LOOP AT gra_unit_level INTO DATA(lwa_unit_level).
*****
*****          PERFORM f_progress_bar USING 'Preparing data to show ALV...'
*****                                        sy-tabix
*****                                        gd_lines.
*****
*****          CLEAR gwa_data_sum_final_b.
*****          gwa_data_sum_final_b-bal_id = s_recid-low.
*****          gwa_data_sum_final_b-bal_level = s_zlevel-low.
*****
*****          PERFORM f_get_where_clause    USING lwa_unit_level-low
*****                                              lwa_field_sap_combine
*****                                     CHANGING ld_where.
*****
*****          LOOP AT p_git_data_pre_detail INTO lwa_data_pre_detail
*****            WHERE (ld_where).
*****
*****            "*--------------------------------------------------------------------*
*****
*****            LOOP AT git_field_sap_combine INTO lwa_field_sap_combine.
*****
*****              CLEAR lit_field_sap_tmp[].
******              SPLIT lwa_field_sap_combine-field_sap AT '+' INTO TABLE lit_field_sap_tmp.
*****              SPLIT lwa_field_sap_combine-field_sap AT ',' INTO TABLE lit_field_sap_tmp.
*****
*****              CLEAR ld_value_field_sap_combine.
*****              LOOP AT lit_field_sap_tmp INTO lwa_field_sap_tmp.
*****
*****                gd_tabix = sy-tabix.
*****
*****                CONCATENATE 'LWA_DATA_PRE_DETAIL' '-' lwa_field_sap_tmp INTO ld_fs.
*****                CONDENSE ld_fs NO-GAPS.
*****                ASSIGN (ld_fs) TO <lfs>.
*****
******                ld_value_field_sap_combine = ld_value_field_sap_combine && <lfs>.
*****
*****                IF gd_tabix EQ 1.
*****                  ld_value_field_sap_combine = <lfs>.
*****                ELSE.
*****                  ld_value_field_sap_combine = ld_value_field_sap_combine && ',' && <lfs>.
*****                ENDIF.
*****
*****              ENDLOOP.
*****
*****              CASE lwa_field_sap_combine-zlevel.
*****                WHEN 1.
*****                  gwa_data_sum_final_b-bal_unit_id_lvl1 = ld_value_field_sap_combine.
*****                WHEN 2.
*****                  gwa_data_sum_final_b-bal_unit_id_lvl2 = ld_value_field_sap_combine.
*****                WHEN 3.
*****                  gwa_data_sum_final_b-bal_unit_id_lvl3 = ld_value_field_sap_combine.
*****              ENDCASE.
*****
*****            ENDLOOP.
*****
*****            "*--------------------------------------------------------------------*
*****
*****            gwa_data_sum_final_b-bal_source = s_source-low.
*****
*****            IF lwa_data_pre_detail-budat NE '00000000'.
******              PERFORM f_conv_date_with_separator    USING lwa_data_pre_detail-budat
******                                                          '.'
******                                                 CHANGING gwa_data_sum_final_b-budat.
*****              gwa_data_sum_final_b-budat = lwa_data_pre_detail-budat.
*****
*****              gwa_data_sum_final_b-budat_b = lwa_data_pre_detail-budat.
*****            ELSE.
*****              gwa_data_sum_final_b-budat = 'NULL'.
*****              gwa_data_sum_final_b-budat_b = lwa_data_pre_detail-budat.
*****            ENDIF.
*****
*****            gwa_data_sum_final_b-belnr = lwa_data_pre_detail-belnr.
*****            gwa_data_sum_final_b-gjahr = lwa_data_pre_detail-gjahr.
*****
*****            IF lwa_data_pre_detail-hsl IS NOT INITIAL.
*****              gwa_data_sum_final_b-currency = lwa_data_pre_detail-rhcur.
*****              gwa_data_sum_final_b-currency_b = lwa_data_pre_detail-rhcur.
*****
*****              WRITE lwa_data_pre_detail-hsl TO gwa_data_sum_final_b-amount CURRENCY lwa_data_pre_detail-rhcur.
*****              REPLACE ALL OCCURRENCES OF '.' IN gwa_data_sum_final_b-amount WITH ''.
*****              CONDENSE gwa_data_sum_final_b-amount.
*****
*****              IF lwa_data_pre_detail-hsl < 0.
*****                REPLACE ALL OCCURRENCES OF '-' IN gwa_data_sum_final_b-amount WITH ''.
*****                gwa_data_sum_final_b-amount = '-' && gwa_data_sum_final_b-amount.
*****              ENDIF.
*****
*****              gwa_data_sum_final_b-amount_b = lwa_data_pre_detail-hsl.
*****            ELSE.
*****              gwa_data_sum_final_b-currency = 'NULL'.
*****              gwa_data_sum_final_b-currency_b = ''.
*****              gwa_data_sum_final_b-amount = 'NULL'.
*****              gwa_data_sum_final_b-amount_b = 0.
*****            ENDIF.
*****
*****            gwa_data_sum_final_b-status = 'POSTED'.
*****            gwa_data_sum_final_b-bal_date_b = s_baldat-low.
*****            gwa_data_sum_final_b-bal_date = s_baldat-low - 1.
*****            gwa_data_sum_final_b-bal_no_reff = s_noref-low.
*****            APPEND gwa_data_sum_final_b TO p_git_data_sum_final_b.
*****
*****            "*--------------------------------------------------------------------*
*****            "Add flag which data match with selection screen
*****
*****            CASE s_zlevel-low.
*****
*****              WHEN 4.
*****
*****                READ TABLE git_unit_level INTO gwa_unit_level WITH KEY unit_id_lvl_1 = gwa_data_sum_final_b-bal_unit_id_lvl1
*****                                                                       unit_id_lvl_2 = gwa_data_sum_final_b-bal_unit_id_lvl2
*****                                                                       unit_id_lvl_3 = gwa_data_sum_final_b-bal_unit_id_lvl3.
*****                IF sy-subrc EQ 0.
*****                  gwa_unit_level-flag_exist = 'X'.
*****                  MODIFY git_unit_level FROM gwa_unit_level INDEX sy-tabix.
*****                ENDIF.
*****
*****            ENDCASE.
*****
*****            "*--------------------------------------------------------------------*
*****
*****          ENDLOOP.
*****
*****        ENDLOOP.
*****
*****        "*--------------------------------------------------------------------*
*****        "Add data which is not existing in SAP
*****
*****        CASE s_zlevel-low.
*****          WHEN 4. "Level 4
*****
*****            LOOP AT git_unit_level INTO gwa_unit_level WHERE flag_exist EQ space.
*****
*****              CLEAR gwa_data_sum_final_b.
*****              gwa_data_sum_final_b-bal_id = s_recid-low.
*****              gwa_data_sum_final_b-bal_level = s_zlevel-low.
*****
*****              gwa_data_sum_final_b-bal_unit_id_lvl1 = gwa_unit_level-unit_id_lvl_1.
*****              gwa_data_sum_final_b-bal_unit_id_lvl2 = gwa_unit_level-unit_id_lvl_2.
*****              gwa_data_sum_final_b-bal_unit_id_lvl3 = gwa_unit_level-unit_id_lvl_3.
*****              gwa_data_sum_final_b-bal_source = s_source-low.
*****
*****              gwa_data_sum_final_b-budat = 'NULL'.
*****              gwa_data_sum_final_b-budat_b = ''.
*****              gwa_data_sum_final_b-belnr = 'NULL'.
*****              gwa_data_sum_final_b-gjahr = 'NULL'.
*****              gwa_data_sum_final_b-gjahr_b = ''.
*****              gwa_data_sum_final_b-currency = 'NULL'.
*****              gwa_data_sum_final_b-currency_b = ''.
*****              gwa_data_sum_final_b-amount = 'NULL'.
*****              gwa_data_sum_final_b-amount_b = 0.
*****              gwa_data_sum_final_b-status = 'NOT YET'.
*****              gwa_data_sum_final_b-bal_date_b = s_baldat-low.
*****              gwa_data_sum_final_b-bal_date = s_baldat-low - 1.
*****              gwa_data_sum_final_b-bal_no_reff = s_noref-low.
*****              APPEND gwa_data_sum_final_b TO p_git_data_sum_final_b.
*****
*****            ENDLOOP.
*****
*****        ENDCASE.
*****
*****        "*--------------------------------------------------------------------*
*****
*****        SORT p_git_data_sum_final_b ASCENDING BY bal_unit_id_lvl1 bal_unit_id_lvl2 bal_unit_id_lvl3.
*****
*****      ENDIF.

      IF gra_unit_level[] IS NOT INITIAL.

        READ TABLE git_field_sap_combine INTO lwa_field_sap_combine
          WITH KEY zlevel = 3.

        ld_field_sap = lwa_field_sap_combine-field_sap.

        "*--------------------------------------------------------------------*

        CLEAR: gd_percent, gd_lines.
        DESCRIBE TABLE gra_unit_level LINES gd_lines.

        LOOP AT gra_unit_level INTO DATA(lwa_unit_level).

          PERFORM f_progress_bar USING 'Preparing data to show ALV...'
                                        sy-tabix
                                        gd_lines.

          CLEAR gwa_data_sum_final_b.
          gwa_data_sum_final_b-bal_id = s_recid-low.
          gwa_data_sum_final_b-bal_level = s_zlevel-low.

          PERFORM f_get_where_clause    USING lwa_unit_level-low
                                              lwa_field_sap_combine
                                     CHANGING ld_where.

          LOOP AT p_git_data_pre_detail_x_lv4 INTO DATA(lwa_data_pre_detail_x_lv4)
            WHERE (ld_where).

            "*--------------------------------------------------------------------*

            LOOP AT git_field_sap_combine INTO lwa_field_sap_combine.

              CLEAR lit_field_sap_tmp[].
*              SPLIT lwa_field_sap_combine-field_sap AT '+' INTO TABLE lit_field_sap_tmp.
              SPLIT lwa_field_sap_combine-field_sap AT ',' INTO TABLE lit_field_sap_tmp.

              CLEAR ld_value_field_sap_combine.
              LOOP AT lit_field_sap_tmp INTO lwa_field_sap_tmp.

                gd_tabix = sy-tabix.

                CONCATENATE 'LWA_DATA_PRE_DETAIL_X_LV4' '-' lwa_field_sap_tmp INTO ld_fs.
                CONDENSE ld_fs NO-GAPS.
                ASSIGN (ld_fs) TO <lfs>.

*                ld_value_field_sap_combine = ld_value_field_sap_combine && <lfs>.

                IF gd_tabix EQ 1.
                  ld_value_field_sap_combine = <lfs>.
                ELSE.
                  ld_value_field_sap_combine = ld_value_field_sap_combine && ',' && <lfs>.
                ENDIF.

              ENDLOOP.

              CASE lwa_field_sap_combine-zlevel.
                WHEN 1.
                  gwa_data_sum_final_b-bal_unit_id_lvl1 = ld_value_field_sap_combine.
                WHEN 2.
                  gwa_data_sum_final_b-bal_unit_id_lvl2 = ld_value_field_sap_combine.
                WHEN 3.
                  gwa_data_sum_final_b-bal_unit_id_lvl3 = ld_value_field_sap_combine.
              ENDCASE.

            ENDLOOP.

            "*--------------------------------------------------------------------*

            gwa_data_sum_final_b-bal_source = s_source-low.

            IF lwa_data_pre_detail_x_lv4-budat NE '00000000'.
*              PERFORM f_conv_date_with_separator    USING lwa_data_pre_detail-budat
*                                                          '.'
*                                                 CHANGING gwa_data_sum_final_b-budat.
              gwa_data_sum_final_b-budat = lwa_data_pre_detail_x_lv4-budat.

              gwa_data_sum_final_b-budat_b = lwa_data_pre_detail_x_lv4-budat.
            ELSE.
              gwa_data_sum_final_b-budat = 'NULL'.
              gwa_data_sum_final_b-budat_b = lwa_data_pre_detail_x_lv4-budat.
            ENDIF.

            gwa_data_sum_final_b-belnr = lwa_data_pre_detail_x_lv4-belnr.
            gwa_data_sum_final_b-gjahr = lwa_data_pre_detail_x_lv4-gjahr.

            IF lwa_data_pre_detail_x_lv4-hsl IS NOT INITIAL.
              gwa_data_sum_final_b-currency = lwa_data_pre_detail_x_lv4-rhcur.
              gwa_data_sum_final_b-currency_b = lwa_data_pre_detail_x_lv4-rhcur.

              WRITE lwa_data_pre_detail_x_lv4-hsl TO gwa_data_sum_final_b-amount CURRENCY lwa_data_pre_detail_x_lv4-rhcur.
              REPLACE ALL OCCURRENCES OF '.' IN gwa_data_sum_final_b-amount WITH ''.
              CONDENSE gwa_data_sum_final_b-amount.

              IF lwa_data_pre_detail_x_lv4-hsl < 0.
                REPLACE ALL OCCURRENCES OF '-' IN gwa_data_sum_final_b-amount WITH ''.
                gwa_data_sum_final_b-amount = '-' && gwa_data_sum_final_b-amount.
              ENDIF.

              gwa_data_sum_final_b-amount_b = lwa_data_pre_detail_x_lv4-hsl.
            ELSE.
              gwa_data_sum_final_b-currency = 'NULL'.
              gwa_data_sum_final_b-currency_b = ''.
              gwa_data_sum_final_b-amount = 'NULL'.
              gwa_data_sum_final_b-amount_b = 0.
            ENDIF.

            gwa_data_sum_final_b-status = 'POSTED'.
            gwa_data_sum_final_b-bal_date_b = s_baldat-low.
            gwa_data_sum_final_b-bal_date = s_baldat-low - 1.
            gwa_data_sum_final_b-bal_no_reff = s_noref-low.
            APPEND gwa_data_sum_final_b TO p_git_data_sum_final_b.

            "*--------------------------------------------------------------------*
            "Add flag which data match with selection screen

            CASE s_zlevel-low.

              WHEN 4.

                READ TABLE git_unit_level INTO gwa_unit_level WITH KEY unit_id_lvl_1 = gwa_data_sum_final_b-bal_unit_id_lvl1
                                                                       unit_id_lvl_2 = gwa_data_sum_final_b-bal_unit_id_lvl2
                                                                       unit_id_lvl_3 = gwa_data_sum_final_b-bal_unit_id_lvl3.
                IF sy-subrc EQ 0.
                  gwa_unit_level-flag_exist = 'X'.
                  MODIFY git_unit_level FROM gwa_unit_level INDEX sy-tabix.
                ENDIF.

            ENDCASE.

            "*--------------------------------------------------------------------*

          ENDLOOP.

        ENDLOOP.

        "*--------------------------------------------------------------------*
        "Add data which is not existing in SAP

        CASE s_zlevel-low.
          WHEN 4. "Level 4

            LOOP AT git_unit_level INTO gwa_unit_level WHERE flag_exist EQ space.

              CLEAR gwa_data_sum_final_b.
              gwa_data_sum_final_b-bal_id = s_recid-low.
              gwa_data_sum_final_b-bal_level = s_zlevel-low.

              gwa_data_sum_final_b-bal_unit_id_lvl1 = gwa_unit_level-unit_id_lvl_1.
              gwa_data_sum_final_b-bal_unit_id_lvl2 = gwa_unit_level-unit_id_lvl_2.
              gwa_data_sum_final_b-bal_unit_id_lvl3 = gwa_unit_level-unit_id_lvl_3.
              gwa_data_sum_final_b-bal_source = s_source-low.

              gwa_data_sum_final_b-budat = 'NULL'.
              gwa_data_sum_final_b-budat_b = ''.
              gwa_data_sum_final_b-belnr = 'NULL'.
              gwa_data_sum_final_b-gjahr = 'NULL'.
              gwa_data_sum_final_b-gjahr_b = ''.
              gwa_data_sum_final_b-currency = 'NULL'.
              gwa_data_sum_final_b-currency_b = ''.
              gwa_data_sum_final_b-amount = 'NULL'.
              gwa_data_sum_final_b-amount_b = 0.
              gwa_data_sum_final_b-status = 'NOT YET'.
              gwa_data_sum_final_b-bal_date_b = s_baldat-low.
              gwa_data_sum_final_b-bal_date = s_baldat-low - 1.
              gwa_data_sum_final_b-bal_no_reff = s_noref-low.
              APPEND gwa_data_sum_final_b TO p_git_data_sum_final_b.

            ENDLOOP.

        ENDCASE.

        "*--------------------------------------------------------------------*

        SORT p_git_data_sum_final_b ASCENDING BY bal_unit_id_lvl1 bal_unit_id_lvl2 bal_unit_id_lvl3.

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
FORM f_update_status_zfidt00268  USING    p_kind
                                          p_datum_system
                                          p_uzeit_system

                                          p_rbukrs
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

    LOOP AT lit_zfidt00268 ASSIGNING FIELD-SYMBOL(<fs_zfidt00268>).

      CASE p_kind.
        WHEN 'PROCESSED'.

          <fs_zfidt00268>-status_processed = 'DONE'.
          <fs_zfidt00268>-timestamp_processed = p_datum_system && p_uzeit_system.

        WHEN 'SENT'.

          <fs_zfidt00268>-status_sent = 'DONE'.
          <fs_zfidt00268>-timestamp_sent = p_datum_system && p_uzeit_system.

      ENDCASE.

    ENDLOOP.

    MODIFY zfidt00268 FROM TABLE lit_zfidt00268.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.

      CASE p_kind.
        WHEN 'PROCESSED'.
          PERFORM f_progress_bar_single USING 'PROCESSED: Update status successfully on table ZFIDT00268...' 'S' 'S'.
        WHEN 'SENT'.
          PERFORM f_progress_bar_single USING 'SENT: Update status successfully on table ZFIDT00268...' 'S' 'S'.
      ENDCASE.

    ELSE.

      CASE p_kind.
        WHEN 'PROCESSED'.
          PERFORM f_progress_bar_single USING 'PROCESSED: Update status failed on table ZFIDT00268...' 'S' 'E'.
        WHEN 'SENT'.
          PERFORM f_progress_bar_single USING 'SENT: Update status failed on table ZFIDT00268...' 'S' 'E'.
      ENDCASE.

    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_WHERE_CLAUSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_FIELD_SAP_COMBINE
*&      <-- LD_WHERE
*&---------------------------------------------------------------------*
FORM f_get_where_clause  USING    p_unit_level
                                  p_lwa_field_sap_combine TYPE gty_field_sap_combine
                         CHANGING p_where.

  FIELD-SYMBOLS: <lfs>.

  TYPES: BEGIN OF gty_field_sap,
           field_sap TYPE zfidt00266-field_sap,
         END OF gty_field_sap.

  DATA: lit_field_sap_tmp TYPE TABLE OF gty_field_sap,

        ld_length         TYPE i,
        ld_dref           TYPE REF TO data.

*--------------------------------------------------------------------*

  CLEAR lit_field_sap_tmp[].
*  SPLIT p_lwa_field_sap_combine-field_sap AT '+' INTO TABLE lit_field_sap_tmp.
  SPLIT p_lwa_field_sap_combine-field_sap AT ',' INTO TABLE lit_field_sap_tmp.

  "*--------------------------------------------------------------------*

  CLEAR ld_length.
  LOOP AT lit_field_sap_tmp INTO DATA(lwa_field_sap_tmp).

    gd_tabix = sy-tabix.

    "*--------------------------------------------------------------------*

    READ TABLE git_field_map INTO DATA(lwa_field_map)
      WITH KEY field_sap = lwa_field_sap_tmp-field_sap.
    IF sy-subrc EQ 0.

      CASE lwa_field_map-field_sap.
        WHEN 'ZUONR'.
          lwa_field_map-field_sap = 'DZUONR'.
      ENDCASE.

      CLEAR ld_dref.
      CREATE DATA ld_dref TYPE (lwa_field_map-field_sap).
      ASSIGN ld_dref->* TO <lfs>.

      <lfs> = p_unit_level+ld_length(lwa_field_map-field_length).

      CHECK <lfs> IS NOT INITIAL.

      IF lwa_field_map-flag_input_conversion EQ abap_true.
        <lfs> = |{ <lfs> ALPHA = IN }|.
      ENDIF.

      IF lwa_field_map-flag_input_conversion EQ abap_true.
        ADD lwa_field_map-field_length TO ld_length.
      ELSE.
        DATA(ld_strlen) = strlen( <lfs> ).
        ADD ld_strlen TO ld_length.
      ENDIF.

    ENDIF.

    "*--------------------------------------------------------------------*

    IF gd_tabix EQ 1.

      p_where = '''' && <lfs> && ''''.

      CONCATENATE lwa_field_sap_tmp-field_sap '=' p_where
        INTO p_where
          SEPARATED BY space.

    ELSE.

      p_where = '''' && <lfs> && ''''.

      CONCATENATE 'AND' lwa_field_sap_tmp-field_sap '=' p_where
        INTO p_where
          SEPARATED BY space.

    ENDIF.

    "*--------------------------------------------------------------------*

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_SPLIT_AND_SAVE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_DATA_SUM_FINAL[]
*&      --> GIT_DATA_SUM_FINAL_B[]
*&      <-- GD_MAXROW
*&      <-- GD_SEQNO
*&      <-- GD_BATCH_NUMBER
*&      <-- GD_BATCH_MAX
*&---------------------------------------------------------------------*
FORM f_split_and_save_log     USING p_zlevel
                           CHANGING p_batch_total
                                    p_row_total

                                    p_git_data_sum_final TYPE gtt_data_sum_final
                                    p_git_data_sum_final_b TYPE gtt_data_sum_final_b.

  DATA: lit_zfidt00280 TYPE TABLE OF zfidt00280,
        lit_zfidt00281 TYPE TABLE OF zfidt00281.

  DATA: ld_max_row_per_batch   TYPE i,
        ld_tabix               TYPE sy-tabix,

        ld_batch_seq_no        TYPE zfide01200 VALUE 1,
        ld_row_seq_no          TYPE zfide01199,
        ld_batch_total         TYPE zfide01201,
        ld_row_total_per_batch TYPE zfide01204,
        ld_row_total           TYPE zfide01203,

        ld_row_total_c         TYPE zfide01203,
        ld_batch_total_c       TYPE p DECIMALS 2,

        lit_data_sum_final     TYPE gtt_data_sum_final,
        lit_data_sum_final_c   TYPE gtt_data_sum_final,
        lit_data_sum_final_b   TYPE gtt_data_sum_final_b,
        lit_data_sum_final_b_c TYPE gtt_data_sum_final_b.

*--------------------------------------------------------------------*

  CLEAR ld_row_seq_no.

  CLEAR: lit_data_sum_final[], lit_data_sum_final_b[],
         lit_data_sum_final_c[], lit_data_sum_final_b_c[].

  lit_data_sum_final[] = p_git_data_sum_final[].
  lit_data_sum_final_b[] = p_git_data_sum_final_b[].

  CLEAR: p_git_data_sum_final[],
         p_git_data_sum_final_b[].

  ld_max_row_per_batch = gd_max_row_per_batch.

*--------------------------------------------------------------------*

  CASE p_zlevel.
    WHEN 1 OR 2 OR 3.

      DESCRIBE TABLE lit_data_sum_final LINES ld_row_total.
      p_row_total = ld_row_total.

      DESCRIBE TABLE lit_data_sum_final LINES ld_row_total_c.
      ld_batch_total_c = ( ld_row_total_c / gd_max_row_per_batch ).

      "*--------------------------------------------------------------------*

      PERFORM f_round USING '+'
                      CHANGING ld_batch_total_c.
      ld_batch_total = ld_batch_total_c.
      p_batch_total = ld_batch_total.

      "*--------------------------------------------------------------------*

      LOOP AT lit_data_sum_final ASSIGNING FIELD-SYMBOL(<lwa_data_sum_final>).

        gd_tabix = sy-tabix.
        ADD 1 TO ld_tabix.
        ADD 1 TO ld_row_seq_no.

        "*--------------------------------------------------------------------*

        <lwa_data_sum_final>-rbukrs = s_rbukrs-low.

        <lwa_data_sum_final>-batch_seq_no = ld_batch_seq_no.
        <lwa_data_sum_final>-batch_total = ld_batch_total.

        <lwa_data_sum_final>-row_seq_no = ld_row_seq_no.
        <lwa_data_sum_final>-row_total = ld_row_total.

        APPEND <lwa_data_sum_final> TO lit_data_sum_final_c.

        IF ld_row_total_c >= gd_max_row_per_batch.

          IF ( gd_tabix MOD ld_max_row_per_batch EQ 0 ).

            CLEAR ld_row_total_per_batch.
            DESCRIBE TABLE lit_data_sum_final_c LINES ld_row_total_per_batch.

            LOOP AT lit_data_sum_final_c ASSIGNING FIELD-SYMBOL(<fs_data_sum_final_c>).
              <fs_data_sum_final_c>-row_total_per_batch = ld_row_total_per_batch.
            ENDLOOP.

            "*--------------------------------------------------------------------*

            ADD 1 TO ld_batch_seq_no.

            APPEND LINES OF lit_data_sum_final_c[] TO p_git_data_sum_final[].

            CLEAR lit_data_sum_final_c[].
            CLEAR ld_tabix.

            SUBTRACT ld_max_row_per_batch FROM ld_row_total_c.

          ENDIF.

        ELSE.

          IF ld_tabix EQ ld_row_total_c.

            CLEAR ld_row_total_per_batch.
            DESCRIBE TABLE lit_data_sum_final_c LINES ld_row_total_per_batch.

            LOOP AT lit_data_sum_final_c ASSIGNING <fs_data_sum_final_c>.
              <fs_data_sum_final_c>-row_total_per_batch = ld_row_total_per_batch.
            ENDLOOP.

            "*--------------------------------------------------------------------*

            ADD 1 TO ld_batch_seq_no.

            APPEND LINES OF lit_data_sum_final_c TO p_git_data_sum_final[].

          ENDIF.

        ENDIF.

      ENDLOOP.

      "*--------------------------------------------------------------------*

      CHECK c_sw_log EQ 'X'.

      DELETE FROM zfidt00280
        WHERE rbukrs EQ s_rbukrs-low AND
              bal_id EQ s_recid-low AND
              bal_date_b EQ s_baldat-low AND
              bal_no_reff EQ s_noref-low AND
              bal_level EQ s_zlevel-low.

      CLEAR lit_zfidt00280[].
      MOVE-CORRESPONDING p_git_data_sum_final[] TO lit_zfidt00280[].

      MODIFY zfidt00280 FROM TABLE lit_zfidt00280[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    WHEN 4.

      DESCRIBE TABLE lit_data_sum_final_b LINES ld_row_total.
      p_row_total = ld_row_total.


      DESCRIBE TABLE lit_data_sum_final_b LINES ld_row_total_c.
      ld_batch_total_c = ( ld_row_total_c / gd_max_row_per_batch ).

      "*--------------------------------------------------------------------*

      PERFORM f_round USING '+'
                      CHANGING ld_batch_total_c.
      ld_batch_total = ld_batch_total_c.
      p_batch_total = ld_batch_total.

      "*--------------------------------------------------------------------*

      LOOP AT lit_data_sum_final_b ASSIGNING FIELD-SYMBOL(<lwa_data_sum_final_b>).

        gd_tabix = sy-tabix.
        ADD 1 TO ld_tabix.
        ADD 1 TO ld_row_seq_no.

        "*--------------------------------------------------------------------*

        <lwa_data_sum_final_b>-rbukrs = s_rbukrs-low.

        <lwa_data_sum_final_b>-batch_seq_no = ld_batch_seq_no.
        <lwa_data_sum_final_b>-batch_total = ld_batch_total.

        <lwa_data_sum_final_b>-row_seq_no = ld_row_seq_no.
        <lwa_data_sum_final_b>-row_total = ld_row_total.

        APPEND <lwa_data_sum_final_b> TO lit_data_sum_final_b_c.

        IF ld_row_total_c >= gd_max_row_per_batch.

          IF ( gd_tabix MOD ld_max_row_per_batch EQ 0 ).

            CLEAR ld_row_total_per_batch.
            DESCRIBE TABLE lit_data_sum_final_b_c LINES ld_row_total_per_batch.

            LOOP AT lit_data_sum_final_b_c ASSIGNING FIELD-SYMBOL(<fs_data_sum_final_b_c>).
              <fs_data_sum_final_b_c>-row_total_per_batch = ld_row_total_per_batch.
            ENDLOOP.

            "*--------------------------------------------------------------------*

            ADD 1 TO ld_batch_seq_no.

            APPEND LINES OF lit_data_sum_final_b_c[] TO p_git_data_sum_final_b[].

            CLEAR lit_data_sum_final_b_c[].
            CLEAR ld_tabix.

            SUBTRACT ld_max_row_per_batch FROM ld_row_total_c.

          ENDIF.

        ELSE.

          IF ld_tabix EQ ld_row_total_c.

            CLEAR ld_row_total_per_batch.
            DESCRIBE TABLE lit_data_sum_final_b_c LINES ld_row_total_per_batch.

            LOOP AT lit_data_sum_final_b_c ASSIGNING <fs_data_sum_final_b_c>.
              <fs_data_sum_final_b_c>-row_total_per_batch = ld_row_total_per_batch.
            ENDLOOP.

            "*--------------------------------------------------------------------*

            ADD 1 TO ld_batch_seq_no.

            APPEND LINES OF lit_data_sum_final_b_c TO p_git_data_sum_final_b[].

          ENDIF.

        ENDIF.

      ENDLOOP.

      "*--------------------------------------------------------------------*

      CHECK c_sw_log EQ 'X'.

      DELETE FROM zfidt00281
        WHERE rbukrs EQ s_rbukrs-low AND
              bal_id EQ s_recid-low AND
              bal_date_b EQ s_baldat-low AND
              bal_no_reff EQ s_noref-low AND
              bal_level EQ s_zlevel-low.

      CLEAR lit_zfidt00281[].
      MOVE-CORRESPONDING p_git_data_sum_final_b[] TO lit_zfidt00281[].

      MODIFY zfidt00281 FROM TABLE lit_zfidt00281[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_ROUND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LD_TOTAL_BATCH
*&---------------------------------------------------------------------*
FORM f_round  USING    p_sign
              CHANGING p_total_batch.

  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals      = 0
      input         = p_total_batch
      sign          = p_sign
    IMPORTING
      output        = p_total_batch
    EXCEPTIONS
      input_invalid = 1
      overflow      = 2
      type_invalid  = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF p_total_batch IS INITIAL.
      p_total_batch = '1'.
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
        ld_batch_last(2)  TYPE n.

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
*& Form F_CHANGE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_BATCH_TOTAL
*&      --> S_RBUKRS_LOW
*&      --> P_DATUM
*&      --> P_UZEIT
*&      --> S_RECID_LOW
*&      --> S_NOREF_LOW
*&      --> S_ZLEVEL_LOW
*&---------------------------------------------------------------------*
FORM f_change_log  USING    p_batch_total
                            p_row_total

                            p_rbukrs
                            p_datum_param
                            p_uzeit_param
                            p_recon_id
                            p_noref
                            p_zlevel
                            p_baldat.

  DATA: lit_zfidt00268_c TYPE TABLE OF zfidt00268,
        ld_batch_seq_no  TYPE zfide01200.

*--------------------------------------------------------------------*

  SELECT * FROM zfidt00268 INTO TABLE @DATA(lit_zfidt00268)
    WHERE rbukrs EQ @p_rbukrs AND
          date_entry EQ @p_datum_param AND
          time_entry EQ @p_uzeit_param AND
          recon_id EQ @p_recon_id AND
          bal_no_reff EQ @p_noref AND
          zlevel EQ @p_zlevel.

  IF lit_zfidt00268[] IS NOT INITIAL.

    DELETE FROM zfidt00268
      WHERE rbukrs EQ p_rbukrs AND
            date_entry EQ p_datum_param AND
            time_entry EQ p_uzeit_param AND
            recon_id EQ p_recon_id AND
            bal_no_reff EQ p_noref AND
            zlevel EQ p_zlevel.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.

    "*--------------------------------------------------------------------*

    CLEAR lit_zfidt00268_c[].
    DO p_batch_total TIMES.

      ld_batch_seq_no = sy-index.

      LOOP AT lit_zfidt00268 INTO DATA(lwa_zfidt00268).
        lwa_zfidt00268-batch_seq_no = ld_batch_seq_no.
        lwa_zfidt00268-batch_total = p_batch_total.
        lwa_zfidt00268-row_total = p_row_total.

        CASE p_zlevel.
          WHEN 1 OR 2 OR 3.

            SELECT COUNT( * )
              FROM zfidt00280
              INTO @DATA(ld_row_total_per_batch)
                WHERE rbukrs EQ @p_rbukrs AND
                      bal_id EQ @p_recon_id AND
                      bal_date_b EQ @p_baldat AND
                      bal_no_reff EQ @p_noref AND
                      bal_level EQ @p_zlevel AND
                      batch_seq_no EQ @ld_batch_seq_no.

          WHEN 4.

            SELECT COUNT( * )
              FROM zfidt00281
              INTO ld_row_total_per_batch
                WHERE rbukrs EQ p_rbukrs AND
                      bal_id EQ p_recon_id AND
                      bal_date_b EQ p_baldat AND
                      bal_no_reff EQ p_noref AND
                      bal_level EQ p_zlevel AND
                      batch_seq_no EQ ld_batch_seq_no.

        ENDCASE.

        lwa_zfidt00268-row_total_per_batch = ld_row_total_per_batch.

        APPEND lwa_zfidt00268  TO lit_zfidt00268_c.
      ENDLOOP.

    ENDDO.

    "*--------------------------------------------------------------------*

    IF lit_zfidt00268_c[] IS NOT INITIAL.
      MODIFY zfidt00268 FROM TABLE lit_zfidt00268_c[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

    "*--------------------------------------------------------------------*

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_YEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_YEAR
*&---------------------------------------------------------------------*
FORM f_get_year  CHANGING p_git_year TYPE gtt_year.

  DATA: ld_year TYPE gjahr.

*--------------------------------------------------------------------*

  ld_year = 2019. "Start from year 2019

  CLEAR git_year.
  CLEAR gwa_year.
  gwa_year-gjahr = ld_year.
  APPEND gwa_year TO p_git_year.

  DO.

    ADD 1 TO ld_year.
    gwa_year-gjahr = ld_year.
    APPEND gwa_year TO p_git_year.

    IF ld_year EQ sy-datum(4).
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PROCESS_SUMMARY_V2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_DATA_PRE_DETAIL_X[]
*&      <-- GIT_DATA_PRE_DETAIL_X_LV4[]
*&---------------------------------------------------------------------*
FORM f_process_summary_v2  CHANGING p_git_data_pre_detail_x TYPE gtt_data_detail_x
                                    p_git_data_pre_detail_x_lv4 TYPE gtt_data_detail_x_lv4.

  TYPES: BEGIN OF gty_field_sap,
           field_sap TYPE zfidt00266-field_sap,
         END OF gty_field_sap.

  DATA: ld_fields          TYPE string,
        ld_groups          TYPE string,

        lit_data_sum_x     TYPE gtt_data_detail_x,
        lit_data_sum_x_lv4 TYPE gtt_data_detail_x_lv4.

*--------------------------------------------------------------------*

  CASE s_zlevel-low.
    WHEN 1 OR 2 OR 3.

      "*--------------------------------------------------------------------*
      "Get dynamic fields for summary

      CLEAR: ld_fields,
             ld_groups.
      PERFORM f_get_dynamic_fields USING 'A'
                                         git_field_sap_separate[]
                                CHANGING ld_fields
                                         ld_groups.

      "*--------------------------------------------------------------------*
      "Summarize from Data Detail

      PERFORM f_progress_bar_single USING 'Summarizing data for all years...' 'S' 'S'.

      SELECT (ld_fields)
        FROM @p_git_data_pre_detail_x AS a
        GROUP BY (ld_groups)
          INTO CORRESPONDING FIELDS OF TABLE @lit_data_sum_x
        ##db_feature_mode[itabs_in_from_clause]
        ##itab_key_in_select
        ##itab_db_select.

      DELETE lit_data_sum_x WHERE hsl IS INITIAL.

      p_git_data_pre_detail_x[] = lit_data_sum_x[].

    WHEN 4.

      "*--------------------------------------------------------------------*
      "Get dynamic fields for summary

      CLEAR: ld_fields,
             ld_groups.
      PERFORM f_get_dynamic_fields USING 'B'
                                         git_field_sap_separate[]
                                CHANGING ld_fields
                                         ld_groups.

      "*--------------------------------------------------------------------*
      "Summarize from Data Detail

      PERFORM f_progress_bar_single USING 'Summarizing data for all years...' 'S' 'S'.

      SELECT (ld_fields)
        FROM @p_git_data_pre_detail_x_lv4 AS a
        GROUP BY (ld_groups)
          INTO CORRESPONDING FIELDS OF TABLE @lit_data_sum_x_lv4
        ##db_feature_mode[itabs_in_from_clause]
        ##itab_key_in_select
        ##itab_db_select.

      p_git_data_pre_detail_x_lv4[] = lit_data_sum_x_lv4[].

  ENDCASE.

ENDFORM.
