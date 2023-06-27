*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_SPLIT_F00
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

  text901 = 'Selection Area'.
  %_s_rbukrs_%_app_%-text = 'Company Code'.
  %_s_recid_%_app_%-text = 'Recon. ID'.
  %_s_zlevel_%_app_%-text = 'Level'.
  %_s_baldat_%_app_%-text = 'Balance Date'.
  %_s_noref_%_app_%-text = 'Balance Request No.'.
*  %_s_source_%_app_%-text = 'Balance Source'.
  %_s_bsq_%_app_%-text = 'Batch Sequence Number'.

  text902 = 'Selection Area 2'.
  %_p_datum_%_app_%-text = 'Date Entry'.
  %_p_uzeit_%_app_%-text = 'Time Entry'.

  text903 = 'Selection Area 3 (Create Background Job for next level process?)'.
  text911 = 'Switch On / Off'.
  text912 = '(X = On | <BLANK> = Off)'.

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

    IF s_noref-low IS INITIAL.
      SET CURSOR FIELD 'S_NOREF-LOW'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

    IF s_bsq-low IS INITIAL.
      SET CURSOR FIELD 'S_BSQ-LOW'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

    IF p_datum IS INITIAL.
      SET CURSOR FIELD 'P_DATUM'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

    IF p_uzeit IS INITIAL.
      SET CURSOR FIELD 'P_UZEIT'.
      MESSAGE 'Fill out all required entry fields' TYPE 'E'.
    ENDIF.

  ENDIF.

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
      WHEN 'S_NOREF-LOW'.
        screen-required = '2'.
        MODIFY SCREEN.
      WHEN 'S_BSQ-LOW'.
        screen-required = '2'.
        MODIFY SCREEN.
      WHEN 'P_DATUM'.
        screen-required = '2'.
        MODIFY SCREEN.
      WHEN 'P_UZEIT'.
        screen-required = '2'.
        MODIFY SCREEN.
    ENDCASE.

  ENDLOOP.

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
