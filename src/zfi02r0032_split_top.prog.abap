*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_SPLIT_TOP
*&---------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Tables                                                             *
*--------------------------------------------------------------------*
TABLES: zfidt00268,
        zfidt00280.
*--------------------------------------------------------------------*
* End - Tables                                                       *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Constants                                                   *
*--------------------------------------------------------------------*
CONSTANTS: gc_report_title            TYPE lvc_title VALUE 'Report Komparasi (Send Split Outbond)',
           gc_rbukrs                  TYPE bukrs VALUE 'ADMF',
           gc_delay_per_batch_failed  TYPE tvarvc-name VALUE 'ZFI02R0032_DELAY_PER_BATCH_F', "Delay Per Batch If Failed
           gc_delay_per_batch_success TYPE tvarvc-name VALUE 'ZFI02R0032_DELAY_PER_BATCH_S'. "Delay Per Batch If Success
*--------------------------------------------------------------------*
* End - Global Constants                                             *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Types                                                       *
*--------------------------------------------------------------------*
*Custom
TYPES: gtt_zfidt00280           TYPE TABLE OF zfidt00280,
       gtt_zfidt00281           TYPE TABLE OF zfidt00281,
       gtt_data_komparasi_lv1_3 TYPE TABLE OF zfist00170,
       gtt_data_komparasi_lv4   TYPE TABLE OF zfist00171.
*--------------------------------------------------------------------*
*Standard

*--------------------------------------------------------------------*
* End - Global Types                                                 *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Variable                                                    *
*--------------------------------------------------------------------*
*Custom
DATA: git_zfidt00280 TYPE TABLE OF zfidt00280,
      git_zfidt00281 TYPE TABLE OF zfidt00281.

DATA: gd_delay_batch_failed  TYPE sy-uzeit,
      gd_delay_batch_success TYPE sy-uzeit.

*--------------------------------------------------------------------*
*Standard

*---Variable Program - Single Value
DATA: gd_rb         TYPE char20,
      gd_line_excel TYPE i,
      gd_tabix      TYPE i,
      gd_subrc      TYPE sy-subrc,
      gd_message    TYPE text255,
      gd_times      TYPE i,
      gd_answer(1). "Variable for Popup Answer.

*---Variable Get Execution Time
DATA: gd_start TYPE p DECIMALS 3,
      gd_stop  TYPE p DECIMALS 3,
      gd_run   TYPE p DECIMALS 3.
*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Define                                                             *
*--------------------------------------------------------------------*
DEFINE f_fill_range.
  &1-sign = &2.
  &1-option = &3.
  &1-low = &4.
  &1-high = &5.
  APPEND &1.
END-OF-DEFINITION.

"Example: f_fill_range: lra_lptyp 'I' 'EQ' lwa_lptyp-lptyp ''.
*--------------------------------------------------------------------*
* End - Define                                                       *
*--------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a02 WITH FRAME TITLE text901.
SELECT-OPTIONS: s_rbukrs FOR zfidt00268-rbukrs MEMORY ID zfi02r0032_s_rbukrs2 NO-EXTENSION NO INTERVALS DEFAULT gc_rbukrs,
                s_recid FOR zfidt00268-recon_id MEMORY ID zfi02r0032_s_recid2 NO-EXTENSION NO INTERVALS,
                s_zlevel FOR zfidt00268-zlevel MEMORY ID zfi02r0032_s_zlevel2 NO-EXTENSION NO INTERVALS,
                s_baldat FOR zfidt00268-balance_date MEMORY ID zfi02r0032_s_baldat2 NO-EXTENSION NO INTERVALS,
                s_noref FOR zfidt00268-bal_no_reff MEMORY ID zfi02r0032_s_noref2 NO-EXTENSION NO INTERVALS,
                s_bsq FOR zfidt00280-batch_seq_no MEMORY ID zfi02r0032_s_bsq2 NO-EXTENSION NO INTERVALS. "Batch Sequence Number
SELECTION-SCREEN END OF BLOCK a02.

SELECTION-SCREEN BEGIN OF BLOCK a03 WITH FRAME TITLE text902.
PARAMETERS: p_datum TYPE sy-datum MEMORY ID zfi02r0032_p_datum2 MODIF ID p1,
            p_uzeit TYPE sy-uzeit MEMORY ID zfi02r0032_p_uzeit2 MODIF ID p1.
SELECTION-SCREEN END OF BLOCK a03.

SELECTION-SCREEN BEGIN OF BLOCK a04 WITH FRAME TITLE text903.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) text911.
PARAMETERS: c_sw_bj AS CHECKBOX. "Checkbox Switch Background Job
SELECTION-SCREEN COMMENT 36(30) text912.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a04.
*----------------------------------------------------------------------*
* End - Selection Screen                                               *
*----------------------------------------------------------------------*
