*&---------------------------------------------------------------------*
*& Report ZFI02R0032
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Description : Report Komparasi
*&
*& Module      : Financial Accounting
*& Functional  : - Yeremia Khristian Suherman (yeremia.suherman@equine.co.id)
*& FSD Loc.    : - SO2_MIME_REPOSITORY --> SAP --> PUBLIC --> ZFSD
*& FSD         : - 0019. ADMF-EQG.P2207.0047-FSD-FI-CR2-Report Komparasi PK IMBT 08082022 v.1.1.pdf
*& Developer   : Welly Sugiarto (welly.sugiarto@equine.co.id)
*& Date        : September 9th, 2022
*& Copyright   : © 2022 PT Equine Global
*&               © 2022 PT Adira Dinamika Multi Finance
*&
*& Transport Request History (Any changes of TR will be updated here future):
*& *  A4DK908156 SAPABAP EG-AB-FI CR2 - Report Komparasi #1
*& *  Changelog: * Initial Release
*& *  A4DK908562 SAPABAP EG-AB-FI CR2 - Report Komparasi #2
*& *  Changelog: * Fix SPROXY
*& *               http://adira.co.id/ZFIFM_IB_RECEIVE_PARAM_KOMP
*& *               http://adira.co.id/ZFIFM_IB_RECEIVE_REQ_KOMPARASI
*& *  A4DK908697 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #3
*& *  Changelog: * When send outbond set as batch
*& *  A4DK908782 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #4
*& *  Changelog: * Bug Fixed
*& *  A4DK908789 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #5
*& *  Changelog: * Add Table ZFIDT00315 - FI: Table Log Request Report Komparasi from ASS (Detail)
*& *  A4DK908805 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #6
*& *  Changelog: * Bug Fixed Report Komparasi
*& *  A4DK908808 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #7
*& *  Changelog: * Bug Fixed Report Komparasi
*& *  A4DK908820 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #8
*& *  Changelog: * Bug Fixed Report Komparasi
*& *  A4DK908838 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #9
*& *  Changelog: * Bug Fixed Report Komparasi
*& *  A4DK908840 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #10
*& *  Changelog: * Display amount NULL di Level 4
*& *  A4DK908847 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #11
*& *  Changelog: * Remove Sort
*& *  A4DK908855 SAPABAP EG-AB-FI CR2 - Report Komparasi WSU YKS #12
*& *  Changelog: * Add method get data flag to Per Year
*&---------------------------------------------------------------------*


REPORT zfi02r0032.


*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE: zfi02r0032_top, "Types, Data, Constant Declaration & Selection-Screen.
         zfi02r0032_f00, "Other Function for whole this program
         zfi02r0032_f01, "Get Data
         zfi02r0032_f02. "Display Data
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

  CHECK gd_subrc EQ 0.

*--------------------------------------------------------------------*

  CLEAR gd_answer.
  IF c_sw_fm EQ 'X'.

    IF sy-batch EQ 'X'.
      gd_answer = '1'.
    ELSE.
      gd_message = 'Are you sure to send outbond to ASS?'.
      PERFORM f_confirm    USING 'Are you sure to send?'
                                 gd_message
                                 'Yes'
                                 'No'
                        CHANGING gd_answer.
    ENDIF.

  ELSE.
    gd_answer = '1'.
  ENDIF.

  CHECK gd_answer EQ '1'.

*--------------------------------------------------------------------*

  CLEAR gd_answer.
  IF c_sw_log EQ 'X'.

    IF sy-batch EQ 'X'.
      gd_answer = '1'.
    ELSE.
      gd_message = 'Are you sure to save Report Komparasi to ZFIDT00280 / ZFIDT00281?'.
      PERFORM f_confirm    USING 'Are you sure to save?'
                                 gd_message
                                 'Yes'
                                 'No'
                        CHANGING gd_answer.
    ENDIF.

  ELSE.
    gd_answer = '1'.
  ENDIF.

  CHECK gd_answer EQ '1'.

*--------------------------------------------------------------------*

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
