*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_F02
*&---------------------------------------------------------------------*


CLASS gcl_falv DEFINITION INHERITING FROM zcl_falv.
  PUBLIC SECTION.

  PROTECTED SECTION.
    "redefinition of event handler
    METHODS evf_user_command REDEFINITION.
    METHODS evf_top_of_page REDEFINITION.
*    METHODS evf_data_changed REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.


CLASS gcl_falv IMPLEMENTATION.

  METHOD evf_user_command.

    CASE e_ucomm.
*      WHEN zcl_falv_dynamic_status=>b_01.
*
*        me->check_changed_data( ).
*
*        PERFORM f_execute_smartforms USING 'PRINT_PREVIEW'
*                                           git_data[].
*
**        call function 'POPUP_DISPLAY_MESSAGE'
**          exporting
**            titel = 'Popup'   " Title
**            msgid = '00'
**            msgty = 'S'
**            msgno = '001'
**            msgv1 = 'Button 1 clicked'.
*
*      WHEN zcl_falv_dynamic_status=>b_02.
*
*        me->check_changed_data( ).
*
*        PERFORM f_execute_smartforms USING 'PRINT'
*                                           git_data[].
*
**        call function 'POPUP_DISPLAY_MESSAGE'
**          exporting
**            titel = 'Popup'   " Title
**            msgid = '00'
**            msgty = 'S'
**            msgno = '001'
**            msgv1 = 'Button 2 clicked'.
*
*      WHEN zcl_falv_dynamic_status=>b_03.
*
*        me->check_changed_data( ).
*
*        PERFORM f_execute_smartforms USING 'PDF'
*                                           git_data[].

      WHEN OTHERS.
        super->evf_user_command( e_ucomm ).
    ENDCASE.

  ENDMETHOD.

  METHOD evf_top_of_page.

    DATA: ld_text TYPE text255.

    e_dyndoc_id->add_text( text = 'PT Adira Dinamika Multifinance Tbk'
                           sap_emphasis = cl_dd_area=>strong ).

    "*--------------------------------------------------------------------*

    e_dyndoc_id->new_line( repeat = 0 ).

    "*--------------------------------------------------------------------*

    e_dyndoc_id->add_table(
      EXPORTING
        no_of_columns = 2
        border = '0'
      IMPORTING
        table               = DATA(lr_table)
    ).

    lr_table->add_column(
      EXPORTING
        width               = '10%'
      IMPORTING
        column              = DATA(lr_col1)
    ).
    lr_table->add_column(
      EXPORTING
        width               = '70%'
      IMPORTING
        column              = DATA(lr_col2)
    ).

    "*--------------------------------------------------------------------*

    CLEAR ld_text.
    ld_text = gc_report_title.
    lr_col1->add_text( text = 'Program Name:' sap_style = cl_dd_area=>standard ).
    CLEAR ld_text.
    ld_text = gc_report_title.
    lr_col2->add_text( text = ld_text sap_style = cl_dd_area=>standard ).

    "*--------------------------------------------------------------------*

    lr_table->new_row( ).
    CLEAR ld_text.
*    ld_text = gd_run. CONDENSE ld_text.
    WRITE gd_run TO ld_text. CONDENSE ld_text.
    CONCATENATE ld_text 'seconds' INTO ld_text SEPARATED BY space.
    lr_col1->add_text( text = 'Execution Time:' sap_style = cl_dd_area=>standard ).
    lr_col2->add_text( text = ld_text sap_style = cl_dd_area=>standard ).

    "*--------------------------------------------------------------------*

    e_dyndoc_id->merge_document( ).

  ENDMETHOD.

*  METHOD evf_data_changed.
*    me->layout->set_col_opt( iv_value = 'X' ).
*    me->layout->set_cwidth_opt( iv_value = 'X' ).
*    me->soft_refresh( ).
*  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*& Form F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_DATA
*&---------------------------------------------------------------------*
FORM f_display_data  USING  p_git_data_detail TYPE gtt_data_detail
                            p_git_data_pre_detail TYPE gtt_data_detail

                            p_git_data_sum_final TYPE gtt_data_sum_final
                            p_git_data_sum_final_b TYPE gtt_data_sum_final_b.

  "FALV creation with only table passed
*  DATA(falv) = zcl_falv=>create( CHANGING ct_table = p_git_data ).

  "creation of falv with local redefinition
  DATA lcl_falv TYPE REF TO gcl_falv.

  CASE gd_rb.
    WHEN 'NO TRACING'.

      CASE s_zlevel-low.
        WHEN 1 OR 2 OR 3.
          lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                        CHANGING ct_table = p_git_data_sum_final ) .
        WHEN 4.
          lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                        CHANGING ct_table = p_git_data_sum_final_b ) .
      ENDCASE.

    WHEN 'TRACING LEVEL 1'.

      lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                    CHANGING ct_table = p_git_data_pre_detail ) .

    WHEN 'TRACING LEVEL 2'.

      lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                    CHANGING ct_table = p_git_data_detail ) .

  ENDCASE.

*  IF sy-batch EQ ''.
*    lcl_falv->check_changed_data( ).
*  ENDIF.

  "Add title variable
  lcl_falv->title_v1 = gc_report_title.

*  "Set checkbox
*  lcl_falv->set_mark_field( 'SELECT' ).

  "Set zebra
  lcl_falv->layout->set_zebra( iv_value = 'X' ).

*  "Set checkbox editable
*  lcl_falv->column( 'SELECT' )->set_edit( abap_true ).

  "Set column optimization
*  lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ).
  lcl_falv->layout->set_col_opt( iv_value = 'X' ).
  lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
  lcl_falv->layout->set_totals_bef( 'X' ). "Sum on Top
  lcl_falv->layout->set_sel_mode( 'A' ).

  "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
*  lcl_falv->gui_status->fully_dynamic = abap_true.

  PERFORM f_modify_field USING lcl_falv.

*  "Add button
*  PERFORM f_add_button USING falv.

* "Change grid to edit mode
*  lcl_falv->set_editable( iv_modify = abap_true ).

  "Set size top of page
  lcl_falv->top_of_page_height = 75.

  "Display full screen grid
  lcl_falv->show_top_of_page( )->display( ).

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_MODIFY_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> FALV
*&---------------------------------------------------------------------*
FORM f_modify_field USING p_me TYPE REF TO gcl_falv.

  DATA: lit_sort TYPE lvc_t_sort,
        lwa_sort TYPE lvc_s_sort.

*--------------------------------------------------------------------*

  CLEAR lit_sort[].
  CASE gd_rb.
    WHEN 'NO TRACING'.

      CASE s_zlevel-low.
        WHEN 1 OR 2 OR 3.
          p_me->column( 'BAL_AMOUNT_B' )->set_cfieldname( 'BAL_CURRENCY_B' ).
          p_me->column( 'BAL_AMOUNT_B' )->set_do_sum( 'X' ).
        WHEN 4.
          p_me->column( 'AMOUNT_B' )->set_cfieldname( 'CURRENCY_B' ).
          p_me->column( 'AMOUNT_B' )->set_do_sum( 'X' ).
      ENDCASE.

    WHEN 'TRACING LEVEL 1'.

      p_me->column( 'RCLNT' )->set_no_out( 'X' ).
      p_me->column( 'HSL' )->set_cfieldname( 'RHCUR' ).
      p_me->column( 'HSL' )->set_do_sum( 'X' ).

      CLEAR lwa_sort.
      lwa_sort-spos = 1.
      lwa_sort-fieldname = 'TABNAME'.
      lwa_sort-subtot = 'X'.
      APPEND lwa_sort TO lit_sort.

      p_me->sort = lit_sort[].

    WHEN 'TRACING LEVEL 2'.

      p_me->column( 'RCLNT' )->set_no_out( 'X' ).
      p_me->column( 'HSL' )->set_cfieldname( 'RHCUR' ).
      p_me->column( 'HSL' )->set_do_sum( 'X' ).

      CLEAR lwa_sort.
      lwa_sort-spos = 1.
      lwa_sort-fieldname = 'TABNAME'.
      lwa_sort-subtot = 'X'.
      APPEND lwa_sort TO lit_sort.

      p_me->sort = lit_sort[].

  ENDCASE.

ENDFORM.
