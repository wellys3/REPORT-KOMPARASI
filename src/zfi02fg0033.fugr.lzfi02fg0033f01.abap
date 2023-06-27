*----------------------------------------------------------------------*
***INCLUDE LZFI02FG0033F01.
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_DATE_WITH_SEPARATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GWA_EXCEL_RAW_COL4  text
*      -->P_GWA_EXCEL_FIX_VALID_FROM  text
*----------------------------------------------------------------------*
FORM f_convert_date_with_separator  USING    p_input
                                             p_separator
                                    CHANGING p_output.

  CLEAR p_output.
  p_output = p_input+6(2) && p_separator &&
             p_input+4(2) && p_separator &&
             p_input(4) && p_separator.

ENDFORM.                    " F_CONVERT_DATE_WITH_SEPARATOR


*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_TIME_WITH_SEPARATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UZEIT  text
*      -->P_0225   text
*      <--P_LD_STR_TIME  text
*----------------------------------------------------------------------*
FORM f_convert_time_with_separator  USING p_input
                                          p_separator
                                 CHANGING p_output.

  p_output = p_input(2) && p_separator &&
             p_input+2(2) && p_separator &&
             p_input+4(2).

ENDFORM.                    " F_GET_TIME_WITH_SEPARATOR
