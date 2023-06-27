*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_F00
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_initialization .

  PERFORM set_text .

ENDFORM.


*&---------------------------------------------------------------------*
*& Form SET_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_text .

  text902 = 'Selection Area'.
  text903 = 'Selection Area 2 (Will you send outbond to ASS or not?)'.
  text906 = 'Run via FM purpose only!'.
  text908 = 'Don''t use if you use Selection Screen!'.
  text904 = 'Tracing'.
  text905 = 'Selection Area 3 (Will you save to Log or not?)'.

  %_s_rbukrs_%_app_%-text = 'Company Code'.
  %_s_recid_%_app_%-text = 'Recon. ID'.
  %_s_zlevel_%_app_%-text = 'Level'.
  %_s_baldat_%_app_%-text = 'Balance Date'.
  %_s_noref_%_app_%-text = 'Balance Request No.'.
  %_s_source_%_app_%-text = 'Balance Source'.
  %_s_unlv1_%_app_%-text = 'Unit ID Level 1'.
  %_s_unlv2_%_app_%-text = 'Unit ID Level 2'.
  %_s_unlv3_%_app_%-text = 'Unit ID Level 3'.

  %_p_datum_%_app_%-text = 'Date Entry'.
  %_p_uzeit_%_app_%-text = 'Time Entry'.

  text701 = 'Switch On / Off'.
  text702 = '(X = On | <BLANK> = Off)'.

  text601 = 'Switch On / Off'.
  text602 = '(X = On | <BLANK> = Off)'.

  text801 = 'No Tracing'.
  text802 = 'Tracing Level 1'.
  text803 = 'Tracing Level 2'.

  text907 = 'Selection Area 4 (Get data split year)'.
  text501 = 'Switch On / Off'.
  text502 = '(X = On | <BLANK> = Off)'.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_modify_screen .

  IF c_sw_fm EQ ''.

    LOOP AT SCREEN.

      CASE screen-name.
        WHEN 'S_ZLEVEL-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_RBUKRS-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_RECID-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_BALDAT-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
      ENDCASE.

      CASE screen-group1.
        WHEN 'P1'.
          screen-input = '0'.
          MODIFY SCREEN.
      ENDCASE.

    ENDLOOP.

  ELSE.

    LOOP AT SCREEN.

      CASE screen-name.
        WHEN 'S_ZLEVEL-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_RBUKRS-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_RECID-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_BALDAT-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
      ENDCASE.

      CASE screen-group1.
        WHEN 'P2'.
          screen-input = '0'.
          MODIFY SCREEN.
      ENDCASE.

    ENDLOOP.

    rb1 = 'X'.
    rb2 = ''.
    rb3 = ''.

  ENDIF.

  IF c_sw_log EQ 'X' or c_sw_spl eq 'X'.

    LOOP AT SCREEN.

      CASE screen-name.
        WHEN 'S_ZLEVEL-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_RBUKRS-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_RECID-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
        WHEN 'S_BALDAT-LOW'.
          screen-required = '2'.
          MODIFY SCREEN.
      ENDCASE.

      CASE screen-group1.
        WHEN 'P2'.
          screen-input = '0'.
          MODIFY SCREEN.
      ENDCASE.

    ENDLOOP.

    rb1 = 'X'.
    rb2 = ''.
    rb3 = ''.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_MANDATORY_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_mandatory_validation .

  IF sy-ucomm = 'ONLI'.

    IF s_zlevel-low IS INITIAL.
      SET CURSOR FIELD 'S_ZLEVEL-LOW'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

    IF s_rbukrs-low IS INITIAL.
      SET CURSOR FIELD 'S_RBUKRS-LOW'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

    IF s_recid-low IS INITIAL.
      SET CURSOR FIELD 'S_RECID-LOW'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

    IF s_baldat-low IS INITIAL.
      SET CURSOR FIELD 'S_BALDAT-LOW'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

  ENDIF.

ENDFORM.


FORM f_progress_bar_single USING p_value
                                 p_type
                                 p_display_like.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = p_value.

  MESSAGE p_value TYPE p_type DISPLAY LIKE p_display_like.

ENDFORM.


FORM f_progress_bar USING p_value
                          p_tabix
                          p_nlines.

  DATA: w_text(250),
        w_percentage      TYPE p,
        w_percent_char(3).

  w_percentage    = ( p_tabix / p_nlines ) * 100.
  w_percent_char  = w_percentage.

  SHIFT w_percent_char LEFT DELETING LEADING ' '.
  CONCATENATE w_percent_char '% complete' INTO w_text.
  CONCATENATE p_value w_text INTO w_text SEPARATED BY space.

*This check needs to be in, otherwise when looping around big tables
*SAP will re-display indicator too many times causing report to run
*very slow. (No need to re-display same percentage anyways)

  IF w_percentage GT gd_percent
      OR p_tabix  EQ 1.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = w_percentage
        text       = w_text.
    MESSAGE w_text TYPE 'S'.

    gd_percent = w_percentage.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_START_TIMER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_start_timer .
  "Record start time
  GET RUN TIME FIELD gd_start.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_STOP_TIMER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_stop_timer .
  "Record end time
  GET RUN TIME FIELD gd_stop.
  "Run time (milliseconds instead of seconds)
  gd_run = ( gd_stop - gd_start ) / 1000000.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATE_WITH_SEPARATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GWA_EXCEL_RAW_COL4  text
*      -->P_GWA_EXCEL_FIX_VALID_FROM  text
*----------------------------------------------------------------------*
FORM f_conv_date_with_separator  USING    p_input
                                          p_separator
                                 CHANGING p_output.
  CLEAR p_output.
  p_output = p_input+6(2) && p_separator &&
             p_input+4(2) && p_separator &&
             p_input(4).
ENDFORM.                    " F_CONVERT_DATE_WITH_SEPARATOR


*&---------------------------------------------------------------------*
*& Form F_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LD_ANSWER  text
*&---------------------------------------------------------------------*
FORM f_confirm USING p_word1
                     p_word2
                     p_button1
                     p_button2
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = p_word1
      text_question         = p_word2
      text_button_1         = p_button1
      text_button_2         = p_button2
      display_cancel_button = 'X'
    IMPORTING
      answer                = p_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    "Do nothing
  ENDIF.

ENDFORM.
