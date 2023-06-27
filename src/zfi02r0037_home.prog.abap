*&---------------------------------------------------------------------*
*& Report ZFI02R0033_HOME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Description : Program TAC Auto Reclass
*&
*& Module      : Financial Accounting
*& Functional  : - Frits Bilery Aritonang (frits.aritonang@equine.co.id)
*&               - Danang Yoga Wijaya (danang.wijaya@equine.co.id)
*& FSD Loc.    : - SO2_MIME_REPOSITORY --> SAP --> PUBLIC --> ZFSD
*& FSD         : -
*& Developer   : Welly Sugiarto (welly.sugiarto@equine.co.id)
*& Date        : June 20th, 2023
*& Copyright   : © 2023 PT Equine Global
*&               © 2023 PT Adira Dinamika Multi Finance
*&
*& Transport Request History (Any changes of TR will be updated here future):
*& *  A4DK908903 SAPABAP EG-AB-FI CR18 - Program Auto Reclass WSU FBA #1
*&    Changelog: #1 Initial Release
*&---------------------------------------------------------------------*


REPORT zfi02r0037_home.


*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE zfi02r0037_home_top.  "Types, Data, Constant Declaration & Selection-Screen.
INCLUDE zfi02r0037_home_f00.  "Other Function for whole this program
INCLUDE zfi02r0037_home_f01.  "Get Data
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
  PERFORM f_check_auth CHANGING gd_subrc.

  CHECK gd_subrc EQ 0.

  PERFORM f_execute.

END-OF-SELECTION.
*----------------------------------------------------------------------*
* End - Start-of-Selection                                             *
