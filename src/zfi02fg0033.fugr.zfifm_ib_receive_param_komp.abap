FUNCTION zfifm_ib_receive_param_komp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_PARAM STRUCTURE  ZFIST00168
*"      IT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lwa_zfidt00267 TYPE zfidt00267,
        ld_date        TYPE sy-datum.

*--------------------------------------------------------------------*

  CLEAR: it_return[].

*--------------------------------------------------------------------*

  LOOP AT it_param INTO DATA(lwa_param).

    SELECT SINGLE * FROM zfidt00267 INTO @DATA(lwa_zfidt00267_c)
      WHERE rbukrs EQ @gc_rbukrs AND
            valid_date_to EQ '99991231' AND
            recon_id EQ @lwa_param-recon_id.
    IF sy-subrc EQ 0. "Found

      CLEAR lwa_zfidt00267.
      lwa_zfidt00267 = lwa_zfidt00267_c.
      lwa_zfidt00267-level_1 = lwa_param-level_1.
      lwa_zfidt00267-level_2 = lwa_param-level_2.
      lwa_zfidt00267-level_3 = lwa_param-level_3.
      MODIFY zfidt00267 FROM lwa_zfidt00267.
      IF sy-subrc NE 0.
        gd_subrc = sy-subrc.
      ENDIF.

      "*--------------------------------------------------------------------*

      IF gd_subrc EQ 0.

        ld_date = sy-datum - 1.

        SELECT SINGLE * FROM zfidt00267 INTO @DATA(lwa_zfidt00267_c2)
          WHERE rbukrs EQ @gc_rbukrs AND
                valid_date_to EQ @ld_date.
        IF sy-subrc EQ 0.
          DELETE FROM zfidt00267 WHERE rbukrs EQ gc_rbukrs AND
                                  valid_date_to EQ ld_date.
        ENDIF.

        "*--------------------------------------------------------------------*

        CLEAR lwa_zfidt00267.
        lwa_zfidt00267 = lwa_zfidt00267_c.
        lwa_zfidt00267-valid_date_to = ld_date.
        MODIFY zfidt00267 FROM lwa_zfidt00267.
        IF sy-subrc NE 0.
          gd_subrc = sy-subrc.
        ENDIF.

        "*--------------------------------------------------------------------*

        IF gd_subrc EQ 0.
          COMMIT WORK AND WAIT.

          PERFORM f_insert_message    USING 'S'
                                            lwa_param-recon_id
                                            'Successfully updated'
                                   CHANGING it_return[].

        ELSE.

          PERFORM f_insert_message    USING 'S'
                                            lwa_param-recon_id
                                            'Failed to update'
                                   CHANGING it_return[].

        ENDIF.

      ELSE.

        PERFORM f_insert_message    USING 'S'
                                          lwa_param-recon_id
                                          'Failed to update'
                                 CHANGING it_return[].

      ENDIF.

    ELSE. "Not Found

      CLEAR lwa_zfidt00267.
      lwa_zfidt00267-rbukrs = gc_rbukrs.
      lwa_zfidt00267-valid_date_to = '99991231'.
      lwa_zfidt00267-recon_id = lwa_param-recon_id.
      lwa_zfidt00267-level_1 = lwa_param-level_1.
      lwa_zfidt00267-level_2 = lwa_param-level_2.
      lwa_zfidt00267-level_3 = lwa_param-level_3.
      MODIFY zfidt00267 FROM lwa_zfidt00267.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.

        PERFORM f_insert_message    USING 'S'
                                          lwa_param-recon_id
                                          'Successfully saved'
                                 CHANGING it_return[].

      ELSE.

        PERFORM f_insert_message    USING 'E'
                                          lwa_param-recon_id
                                          'Failed to save'
                                 CHANGING it_return[].

      ENDIF.

    ENDIF.

  ENDLOOP.

*--------------------------------------------------------------------*

ENDFUNCTION.


*&---------------------------------------------------------------------*
*& Form F_INSERT_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LWA_PARAM_RECON_ID
*&      <-- IT_RETURN
*&---------------------------------------------------------------------*
FORM f_insert_message  USING    p_type
                                p_recon_id
                                p_message
                       CHANGING p_it_return TYPE gtt_return.

  DATA: lwa_bapiret2 TYPE bapiret2.

*--------------------------------------------------------------------*

  CLEAR lwa_bapiret2.
  lwa_bapiret2-type = p_type.
  CONCATENATE 'Recon. ID'
              p_recon_id
              p_message
    INTO lwa_bapiret2-message SEPARATED BY space.
  APPEND lwa_bapiret2 TO p_it_return.

ENDFORM.
