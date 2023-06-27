*&---------------------------------------------------------------------*
*& Include          ZFI02R0033_HOME_F00
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

  text904 = 'Selection Area'.

  text801 = 'FI: Table Maintain Recon. ID (ZFIDT00265)'.
  text802 = 'FI: Table Maintain Mapping ASS & SAP (ZFIDT00266)'.
  text803 = 'FI: Table Maintain Parameter Level 1 until Level 3 (ZFIDT00267)'.
  text804 = 'FI: Table Log Request Report Komparasi from ASS - Header (ZFIDT00268)'.
  text804b = 'FI: Table Log Request Report Komparasi from ASS - Detail (ZFIDT00315)'.
  text805 = 'FI: Table Staging Result Komparasi Level 1-3 (ZFIDT00280)'.
  text806 = 'FI: Table Staging Result Komparasi Level 4 (ZFIDT00281)'.

  text811 = 'Report Komparasi'.
  text812 = 'Report Komparasi (Send Split Outbond)'.

ENDFORM.
