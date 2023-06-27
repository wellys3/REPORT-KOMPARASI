*&---------------------------------------------------------------------*
*& Report ZFI02R0032_SPLIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Description : Report Komparasi (Send Split Outbond)
*&
*& Module      : Financial Accounting
*& Functional  : - Yeremia Khristian Suherman (yeremia.suherman@equine.co.id)
*& FSD Loc.    : - SO2_MIME_REPOSITORY --> SAP --> PUBLIC --> ZFSD
*& FSD         : - 0019. ADMF-EQG.P2207.0047-FSD-FI-CR2-Report Komparasi PK IMBT 08082022 v.1.1.pdf
*& Developer   : Welly Sugiarto (welly.sugiarto@equine.co.id)
*& Date        : March 21st, 2023
*& Copyright   : © 2023 PT Equine Global
*&               © 2023 PT Adira Dinamika Multi Finance
*&
*& Transport Request History (Any changes of TR will be updated here future):
*& *  A4DK908697 SAPABAP EG-AB-FI CR2 - Report Komparasi #3
*& *  Changelog: * Initial Release
*& *  A4DK908782 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #4
*& *  Changelog: * Bug Fixed
*& *  A4DK908789 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #5
*& *  Changelog: * Add Table ZFIDT00315 - FI: Table Log Request Report Komparasi from ASS (Detail)
*&---------------------------------------------------------------------*


REPORT zfi02r0032_split.


*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE: zfi02r0032_split_top, "Types, Data, Constant Declaration & Selection-Screen.
         zfi02r0032_split_f00, "Other Function for whole this program
         zfi02r0032_split_f01, "Get Data
         zfi02r0032_split_f02. "Display Data
*----------------------------------------------------------------------*
* End - Includes                                                       *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_initialization.
*----------------------------------------------------------------------*
* End - Initialization                                                 *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR gd_subrc.
  PERFORM f_pre_execute CHANGING gd_subrc.

  IF sy-batch EQ 'X'.
    gd_answer = '1'.
  ELSE.

    gd_message = 'Are you sure to execute?'.
    PERFORM f_confirm    USING 'Are you sure?'
                               gd_message
                               'Yes'
                               'No'
                      CHANGING gd_answer.

  ENDIF.

  CHECK gd_answer EQ '1'.

  PERFORM f_execute.

END-OF-SELECTION.
*----------------------------------------------------------------------*
* End - Start-of-Selection                                             *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* At-Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM f_download_template.
  PERFORM f_mandatory_validation.
*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modify_screen.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM f_get_file_dir CHANGING p_file.
*----------------------------------------------------------------------*
* End - At-Selection-Screen                                            *
*----------------------------------------------------------------------*
