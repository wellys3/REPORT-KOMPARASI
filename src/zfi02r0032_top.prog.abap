*&---------------------------------------------------------------------*
*& Include          ZFI02R0032_TOP
*&---------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Type-Pools                                                         *
*--------------------------------------------------------------------*
*TYPE-POOLS: icon, truxs, col, fiehc.
*--------------------------------------------------------------------*
* End - Type-Pools                                                   *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Nodes                                                              *
*--------------------------------------------------------------------*
*NODES: peras.
*--------------------------------------------------------------------*
* End - Nodes                                                        *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Infotype
*--------------------------------------------------------------------*
*INFOTYPES: 0000, 0001, 2006 MODE N.
*INFOTYPES: 0000, 0001, 2006.
*--------------------------------------------------------------------*
* End Infotype
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Tables                                                             *
*--------------------------------------------------------------------*
TABLES: zfidt00268, zfidt00315.
*--------------------------------------------------------------------*
* End - Tables                                                       *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Constants                                                   *
*--------------------------------------------------------------------*
CONSTANTS: gc_report_title        TYPE lvc_title VALUE 'Report Komparasi',
           gc_rbukrs              TYPE bukrs VALUE 'ADMF',
           gc_numb_rows_per_batch TYPE tvarvc-name VALUE 'ZFI02R0032_NUMB_ROWS_PER_BATCH'.
*--------------------------------------------------------------------*
* End - Global Constants                                             *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Types                                                       *
*--------------------------------------------------------------------*
*Custom
*TYPES: BEGIN OF gty_data_sum_final,
*         bal_id           TYPE zfidt00267-recon_id,
*         bal_level        TYPE zfidt00268-zlevel,
*         bal_unit_id_lvl1 TYPE zfist00169-unit_id_lvl1,
*         bal_unit_id_lvl2 TYPE zfist00169-unit_id_lvl2,
*         bal_unit_id_lvl3 TYPE zfist00169-unit_id_lvl3,
*         bal_source       TYPE zfidt00268-source,
*         bal_date         TYPE zfidt00268-balance_date,
*         bal_currency     TYPE acdoca-rhcur,
*         bal_amount       TYPE zfidt00269-hsl,
*       END OF gty_data_sum_final.

TYPES: BEGIN OF gty_data_detail,
         rclnt          TYPE acdoca-rclnt,

         tabname        TYPE dd03l-tabname,
         rldnr          TYPE acdoca-rldnr,
         rbukrs         TYPE acdoca-rbukrs,
         gjahr          TYPE acdoca-gjahr,
         poper          TYPE acdoca-poper,
         belnr          TYPE acdoca-belnr,
         buzei          TYPE zfidt00269-buzei,

         bktxt          TYPE bkpf-bktxt,

         fstag          TYPE zfidt00269-fstag,
         zopenitem      TYPE zfidt00269-zopenitem,
         txt50          TYPE zfidt00269-txt50,

         rhcur          TYPE acdoca-rhcur,
         racct          TYPE acdoca-racct,
         glaccount_type TYPE acdoca-glaccount_type,
         ktopl          TYPE acdoca-ktopl,

         hsl            TYPE acdoca-hsl,

         budat          TYPE acdoca-budat,

         xopvw          TYPE acdoca-xopvw,
         mitkz          TYPE skb1-mitkz,

         augdt          TYPE acdoca-augdt,
         zuonr          TYPE acdoca-zuonr,

         zzku           TYPE acdoca-zzku,
         zzcp           TYPE acdoca-zzcp,
         zzpr           TYPE acdoca-zzpr,
         zzch           TYPE acdoca-zzch,
         zzpo           TYPE acdoca-zzpo,
         zzcc           TYPE acdoca-zzcc,
         zz07           TYPE acdoca-zz07,
         zz08           TYPE acdoca-zz08,
         zz09           TYPE acdoca-zz09,
         zz10           TYPE acdoca-zz10,

         kostl          TYPE zfidt00269-kostl,
         prctr          TYPE zfidt00269-prctr,
         erdat          TYPE zfidt00269-erdat,
         erzeit         TYPE zfidt00269-erzeit,
         ernam          TYPE zfidt00269-ernam,
       END OF gty_data_detail.

TYPES: BEGIN OF gty_data_detail_x,
         rclnt   TYPE acdoca-rclnt,

         tabname TYPE dd03l-tabname,

         bktxt   TYPE bkpf-bktxt,
         zuonr   TYPE acdoca-zuonr,

         zzku    TYPE acdoca-zzku,
         zzcp    TYPE acdoca-zzcp,
         zzpr    TYPE acdoca-zzpr,
         zzch    TYPE acdoca-zzch,
         zzpo    TYPE acdoca-zzpo,
         zzcc    TYPE acdoca-zzcc,
         zz07    TYPE acdoca-zz07,
         zz08    TYPE acdoca-zz08,
         zz09    TYPE acdoca-zz09,
         zz10    TYPE acdoca-zz10,

         rhcur   TYPE acdoca-rhcur,
         hsl     TYPE acdoca-hsl,
       END OF gty_data_detail_x.

TYPES: BEGIN OF gty_data_detail_x_lv4,
         rclnt   TYPE acdoca-rclnt,

         tabname TYPE dd03l-tabname,
         gjahr   TYPE acdoca-gjahr,
         belnr   TYPE acdoca-belnr,

         budat   TYPE acdoca-budat,

         bktxt   TYPE bkpf-bktxt,
         zuonr   TYPE acdoca-zuonr,

         zzku    TYPE acdoca-zzku,
         zzcp    TYPE acdoca-zzcp,
         zzpr    TYPE acdoca-zzpr,
         zzch    TYPE acdoca-zzch,
         zzpo    TYPE acdoca-zzpo,
         zzcc    TYPE acdoca-zzcc,
         zz07    TYPE acdoca-zz07,
         zz08    TYPE acdoca-zz08,
         zz09    TYPE acdoca-zz09,
         zz10    TYPE acdoca-zz10,

         rhcur   TYPE acdoca-rhcur,
         hsl     TYPE acdoca-hsl,
       END OF gty_data_detail_x_lv4.

TYPES: BEGIN OF gty_map_field,
         field_ass              TYPE zfidt00266-field_ass,
         field_sap              TYPE zfidt00266-field_sap,
         field_sap_data_element TYPE dd03l-rollname,
         field_length           TYPE dd03l-leng,
         flag_input_conversion  TYPE zfidt00266-flag_input_conversion,
       END OF gty_map_field.

TYPES: BEGIN OF gty_field_sap_separate,
         zlevel    TYPE zfidt00268-zlevel,
         field_sap TYPE zfidt00266-field_sap,
       END OF gty_field_sap_separate.

TYPES: BEGIN OF gty_field_sap_combine,
         zlevel    TYPE zfidt00268-zlevel,
         field_sap TYPE zfidt00267-level_1,
       END OF gty_field_sap_combine.

TYPES: BEGIN OF gty_unit_level,
         unit_id_lvl_1 TYPE zfidt00315-unit_id_level_1,
         unit_id_lvl_2 TYPE zfidt00315-unit_id_level_2,
         unit_id_lvl_3 TYPE zfidt00315-unit_id_level_3,
         flag_exist    TYPE boolean,
       END OF gty_unit_level.

TYPES: BEGIN OF gty_year,
         gjahr TYPE bkpf-gjahr,
       END OF gty_year.

TYPES: gtt_data_sum_final     TYPE TABLE OF zfist00170,
       gtt_data_sum_final_b   TYPE TABLE OF zfist00171,
       gtt_data_detail        TYPE TABLE OF gty_data_detail,
       gtt_zfidt00242         TYPE TABLE OF gty_data_detail,
*       gtt_zfidt00269         TYPE TABLE OF gty_data_detail,
*       gtt_acdoca_ra          TYPE TABLE OF gty_data_detail,
       gtt_acdoca_oi          TYPE TABLE OF gty_data_detail,
       gtt_acdoca             TYPE TABLE OF gty_data_detail,

       gtt_data_detail_x      TYPE TABLE OF gty_data_detail_x,
       gtt_data_detail_x_lv4  TYPE TABLE OF gty_data_detail_x_lv4,

       gtt_field_sap_separate TYPE TABLE OF gty_field_sap_separate,

       gtt_year type table of gty_year.

*--------------------------------------------------------------------*
*Standard
TYPES: BEGIN OF gty_named_dref,
         name TYPE string,
         dref TYPE REF TO data,
       END OF gty_named_dref.
*--------------------------------------------------------------------*
* End - Global Types                                                 *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Variable                                                    *
*--------------------------------------------------------------------*
*Custom

*---Variable Program - Table & Work Area
DATA: git_data_sum_final        TYPE TABLE OF zfist00170,
      gwa_data_sum_final        TYPE zfist00170,

      git_data_sum_final_b      TYPE TABLE OF zfist00171,
      gwa_data_sum_final_b      TYPE zfist00171,

      git_data_detail           TYPE TABLE OF gty_data_detail,
      git_data_detail_x         TYPE TABLE OF gty_data_detail_x,
      git_data_detail_x_lv4     TYPE TABLE OF gty_data_detail_x_lv4,
      git_data_pre_detail       TYPE TABLE OF gty_data_detail,
      git_data_pre_detail_x     TYPE TABLE OF gty_data_detail_x,
      git_data_pre_detail_x_lv4 TYPE TABLE OF gty_data_detail_x_lv4,

      git_zfidt00265            TYPE TABLE OF zfidt00265,
      git_zfidt00266            TYPE TABLE OF zfidt00266,
      gwa_zfidt00267            TYPE zfidt00267,

      git_field_map             TYPE TABLE OF gty_map_field,
      git_field_sap_separate    TYPE TABLE OF gty_field_sap_separate,
      gwa_field_sap_separate    TYPE gty_field_sap_separate,
      git_field_sap_combine     TYPE TABLE OF gty_field_sap_combine,
      gwa_field_sap_combine     TYPE gty_field_sap_combine,
      git_unit_level            TYPE TABLE OF gty_unit_level,
      gwa_unit_level            TYPE gty_unit_level,

      git_zfidt00242            TYPE TABLE OF gty_data_detail,
*      git_zfidt00269         TYPE TABLE OF gty_data_detail,
*      git_acdoca_ra          TYPE TABLE OF gty_data_detail,
      git_acdoca_oi             TYPE TABLE OF gty_data_detail,
*      git_acdoca_bs          TYPE TABLE OF gty_data_detail,
      git_acdoca_pl             TYPE TABLE OF gty_data_detail,
      git_acdoca                TYPE TABLE OF gty_data_detail,

      git_zfidt00242_x          TYPE TABLE OF gty_data_detail_x,
      git_acdoca_oi_x           TYPE TABLE OF gty_data_detail_x,
      git_acdoca_pl_x           TYPE TABLE OF gty_data_detail_x,
      git_acdoca_x_lv4          TYPE TABLE OF gty_data_detail_x_lv4,

      git_year                 TYPE TABLE OF gty_year,
      gwa_year                 TYPE gty_year.

DATA: gd_max_row_per_batch TYPE i.

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

*---For AMDP Class
DATA: gd_where          TYPE sxmsbody,
      gd_where1         TYPE sxmsbody,
      gd_where2         TYPE sxmsbody,
      gd_where3         TYPE sxmsbody,
      gd_where4         TYPE sxmsbody,
      gd_where5         TYPE sxmsbody,
      git_named_seltabs TYPE TABLE OF gty_named_dref,
      gwa_named_seltabs TYPE gty_named_dref.

*---For Refresh ALV
DATA: gwa_stable     TYPE lvc_s_stbl,
      gd_refreshmode TYPE salv_de_constant.

*---For Debugger
DATA: git_terminal          TYPE TABLE OF tvarvc WITH HEADER LINE,
      gd_opcode_usr_attr(1) TYPE x VALUE 5,
      gd_terminal           TYPE usr41-terminal,
      gd_zdebug             TYPE text255,
      gd_flag               TYPE text255.

*---For Status Progress
DATA: gd_percent TYPE i,
      gd_lines   TYPE i.

*---Variable Get Execution Time
DATA: gd_start TYPE p DECIMALS 3,
      gd_stop  TYPE p DECIMALS 3,
      gd_run   TYPE p DECIMALS 3.
*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Range                                                       *
*--------------------------------------------------------------------*
*Custom

*--------------------------------------------------------------------*

*Standard

RANGES: gra_racct FOR acdoca-racct,
        gra_unit_level FOR zfidt00315-unit_id_level_1,
        gra_value1 FOR bkpf-bktxt,
        gra_value2 FOR bkpf-bktxt,
        gra_value3 FOR bkpf-bktxt,
        gra_value4 FOR bkpf-bktxt,
        gra_value5 FOR bkpf-bktxt.
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
SELECTION-SCREEN BEGIN OF BLOCK a02 WITH FRAME TITLE text902.
SELECT-OPTIONS: s_rbukrs FOR zfidt00268-rbukrs MEMORY ID zfi02r0032_s_rbukrs NO-EXTENSION NO INTERVALS DEFAULT gc_rbukrs,
                s_recid FOR zfidt00268-recon_id MEMORY ID zfi02r0032_s_recid NO-EXTENSION NO INTERVALS,
                s_zlevel FOR zfidt00268-zlevel MEMORY ID zfi02r0032_s_zlevel NO-EXTENSION NO INTERVALS,
                s_baldat FOR zfidt00268-balance_date MEMORY ID zfi02r0032_s_baldat NO-EXTENSION NO INTERVALS,
                s_source FOR zfidt00268-source MEMORY ID zfi02r0032_s_source NO-EXTENSION NO INTERVALS DEFAULT 'SAP',
                s_noref FOR zfidt00268-bal_no_reff MEMORY ID zfi02r0032_s_noref NO-EXTENSION NO INTERVALS,
                s_unlv1 FOR zfidt00315-unit_id_level_1 MEMORY ID zfi02r0032_s_unlv1 NO INTERVALS,
                s_unlv2 FOR zfidt00315-unit_id_level_2 MEMORY ID zfi02r0032_s_unlv2 NO INTERVALS,
                s_unlv3 FOR zfidt00315-unit_id_level_3 MEMORY ID zfi02r0032_s_unlv3 NO INTERVALS.
SELECTION-SCREEN END OF BLOCK a02.

SELECTION-SCREEN BEGIN OF BLOCK a03 WITH FRAME TITLE text903.
*SELECTION-SCREEN BEGIN OF BLOCK a03.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (79) text906.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT (79) text907.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (79) text908.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) text701.
PARAMETERS: c_sw_fm AS CHECKBOX USER-COMMAND u1. "MODIF ID p1. "Checkbox Switch FM
*SELECTION-SCREEN COMMENT 31(30) text702.
SELECTION-SCREEN COMMENT 36(30) text702.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_datum TYPE sy-datum MODIF ID p1 DEFAULT sy-datum,
            p_uzeit TYPE sy-uzeit MODIF ID p1 DEFAULT sy-uzeit.
SELECTION-SCREEN END OF BLOCK a03.

SELECTION-SCREEN BEGIN OF BLOCK a05 WITH FRAME TITLE text905.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) text601.
PARAMETERS: c_sw_log AS CHECKBOX USER-COMMAND u2. "MODIF ID p1. "Checkbox Switch Log
SELECTION-SCREEN COMMENT 36(30) text602.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a05.

SELECTION-SCREEN BEGIN OF BLOCK a06 WITH FRAME TITLE text907.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) text501.
PARAMETERS: c_sw_spl AS CHECKBOX USER-COMMAND u2. "MODIF ID p1. "Checkbox Switch Split Year
SELECTION-SCREEN COMMENT 36(30) text502.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a06.

SELECTION-SCREEN BEGIN OF BLOCK a04 WITH FRAME TITLE text904.
SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS rb1 RADIOBUTTON GROUP rb1 USER-COMMAND rad1 DEFAULT 'X' modif id p2.
PARAMETERS rb1 RADIOBUTTON GROUP rb1 DEFAULT 'X' MODIF ID p2.
SELECTION-SCREEN COMMENT 4(30) text801.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 4(30) text802  MODIF ID p2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb3 RADIOBUTTON GROUP rb1  MODIF ID p2.
SELECTION-SCREEN COMMENT 4(30) text803.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS rb4 RADIOBUTTON GROUP rb1.
*SELECTION-SCREEN COMMENT 4(30) text804.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a04.
*----------------------------------------------------------------------*
* End - Selection Screen                                               *
*----------------------------------------------------------------------*
