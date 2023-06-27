*&---------------------------------------------------------------------*
*& Include          ZFI02R0033_HOME_F01
*&---------------------------------------------------------------------*


**&---------------------------------------------------------------------*
**& Form F_CHECK_AUTH
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      <-- GD_SUBRC
**&---------------------------------------------------------------------*
*FORM f_check_auth  CHANGING p_subrc.
*
*  SELECT SINGLE * FROM zfidt00306
*    INTO @DATA(lwa_zfidt00306)
*      WHERE bname EQ @sy-uname.
*  IF sy-subrc EQ 0.
*    p_subrc = 0.
*  ELSE.
*    p_subrc = 1.
*
*    CLEAR gd_message.
*    gd_message = '''' && sy-uname && ''''.
*    CONCATENATE 'Your User ID' gd_message 'are not authorized to access this program!'
*      INTO gd_message
*        SEPARATED BY space.
*    MESSAGE gd_message TYPE 'S' DISPLAY LIKE 'E'.
*  ENDIF.
*
*ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_execute .

  IF rb1 EQ 'X'.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'ZFIDT00265'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        maintenance_prohibited       = 14
        OTHERS                       = 15.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF rb2 EQ 'X'.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'ZFIDT00266'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        maintenance_prohibited       = 14
        OTHERS                       = 15.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF rb3 EQ 'X'.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'ZFIDT00267'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        maintenance_prohibited       = 14
        OTHERS                       = 15.
    IF sy-subrc <> 0.
      "Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF rb4 EQ 'X'.

*    CALL FUNCTION 'SE16N_INTERFACE'
*      EXPORTING
*        i_tab     = 'ZFIDT00268'
*        i_display = 'X'
*        i_max_lines = ''
*      EXCEPTIONS
*        no_values = 1
*        OTHERS    = 2.
*    IF sy-subrc <> 0.
*      IF sy-subrc EQ 1.
*        MESSAGE 'No values found in table ZFIDT00268' TYPE 'S' DISPLAY LIKE 'W'.
*      ENDIF.
*    ENDIF.

    SET PARAMETER ID 'DTB' FIELD 'ZFIDT00268'.
    CALL TRANSACTION 'SE16' AND SKIP FIRST SCREEN.

*    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
*      EXPORTING
*        table_name = 'ZFIDT00268'
*        action     = 'ANZE'
**       WITHOUT_SUBMIT           = ' '
**       GENERATION_FORCED        =
**       NEW_SEL    =
**       NO_STRUCTURE_CHECK       = ' '
**       DATA_EXIT  = ' '
** IMPORTING
**       PROGNAME   =
** TABLES
**       SELTAB     =
** EXCEPTIONS
**       TABLE_IS_STRUCTURE       = 1
**       TABLE_NOT_EXISTS         = 2
**       DB_NOT_EXISTS            = 3
**       NO_PERMISSION            = 4
**       NO_CHANGE_ALLOWED        = 5
**       TABLE_IS_GTT             = 6
**       OTHERS     = 7
*      .
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

  ELSEIF rb4_b EQ 'X'.

*    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
*      EXPORTING
*        table_name = 'ZFIDT00315'
*        action     = 'ANZE'
**       WITHOUT_SUBMIT           = ' '
**       GENERATION_FORCED        =
**       NEW_SEL    =
**       NO_STRUCTURE_CHECK       = ' '
**       DATA_EXIT  = ' '
** IMPORTING
**       PROGNAME   =
** TABLES
**       SELTAB     =
** EXCEPTIONS
**       TABLE_IS_STRUCTURE       = 1
**       TABLE_NOT_EXISTS         = 2
**       DB_NOT_EXISTS            = 3
**       NO_PERMISSION            = 4
**       NO_CHANGE_ALLOWED        = 5
**       TABLE_IS_GTT             = 6
**       OTHERS     = 7
*      .
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    SET PARAMETER ID 'DTB' FIELD 'ZFIDT00315'.
    CALL TRANSACTION 'SE16' AND SKIP FIRST SCREEN.

  ELSEIF rb5 EQ 'X'.

*    CALL FUNCTION 'SE16N_INTERFACE'
*      EXPORTING
*        i_tab     = 'ZFIDT00280'
*        i_display = 'X'
*        i_max_lines = ''
*      EXCEPTIONS
*        no_values = 1
*        OTHERS    = 2.
*    IF sy-subrc <> 0.
*      IF sy-subrc EQ 1.
*        MESSAGE 'No values found in table ZFIDT00280' TYPE 'S' DISPLAY LIKE 'W'.
*      ENDIF.
*    ENDIF.

*    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
*      EXPORTING
*        table_name = 'ZFIDT00280'
*        action     = 'ANZE'
**       WITHOUT_SUBMIT           = ' '
**       GENERATION_FORCED        =
**       NEW_SEL    =
**       NO_STRUCTURE_CHECK       = ' '
**       DATA_EXIT  = ' '
** IMPORTING
**       PROGNAME   =
** TABLES
**       SELTAB     =
** EXCEPTIONS
**       TABLE_IS_STRUCTURE       = 1
**       TABLE_NOT_EXISTS         = 2
**       DB_NOT_EXISTS            = 3
**       NO_PERMISSION            = 4
**       NO_CHANGE_ALLOWED        = 5
**       TABLE_IS_GTT             = 6
**       OTHERS     = 7
*      .
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    SET PARAMETER ID 'DTB' FIELD 'ZFIDT00280'.
    CALL TRANSACTION 'SE16' AND SKIP FIRST SCREEN.

  ELSEIF rb6 EQ 'X'.

*    CALL FUNCTION 'SE16N_INTERFACE'
*      EXPORTING
*        i_tab     = 'ZFIDT00281'
*        i_display = 'X'
*        i_max_lines = ''
*      EXCEPTIONS
*        no_values = 1
*        OTHERS    = 2.
*    IF sy-subrc <> 0.
*      IF sy-subrc EQ 1.
*        MESSAGE 'No values found in table ZFIDT00281' TYPE 'S' DISPLAY LIKE 'W'.
*      ENDIF.
*    ENDIF.

*    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
*      EXPORTING
*        table_name = 'ZFIDT00281'
*        action     = 'ANZE'
**       WITHOUT_SUBMIT           = ' '
**       GENERATION_FORCED        =
**       NEW_SEL    =
**       NO_STRUCTURE_CHECK       = ' '
**       DATA_EXIT  = ' '
** IMPORTING
**       PROGNAME   =
** TABLES
**       SELTAB     =
** EXCEPTIONS
**       TABLE_IS_STRUCTURE       = 1
**       TABLE_NOT_EXISTS         = 2
**       DB_NOT_EXISTS            = 3
**       NO_PERMISSION            = 4
**       NO_CHANGE_ALLOWED        = 5
**       TABLE_IS_GTT             = 6
**       OTHERS     = 7
*      .
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    SET PARAMETER ID 'DTB' FIELD 'ZFIDT00281'.
    CALL TRANSACTION 'SE16' AND SKIP FIRST SCREEN.

  ELSEIF rb11 EQ 'X'.

    SUBMIT zfi02r0032 VIA SELECTION-SCREEN AND RETURN.

  ELSEIF rb12 EQ 'X'.

    SUBMIT zfi02r0032_split VIA SELECTION-SCREEN AND RETURN.

  ENDIF.

ENDFORM.
