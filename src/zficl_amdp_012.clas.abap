CLASS zficl_amdp_012 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .

    "**********************************************************************

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
*             rtcur     TYPE zfidt00269-rtcur,

             rhcur          TYPE acdoca-rhcur,
             racct          TYPE acdoca-racct,

             glaccount_type TYPE acdoca-glaccount_type,
             ktopl          TYPE acdoca-ktopl,

             hsl            TYPE acdoca-hsl,
*             tsl       TYPE zfidt00269-tsl,

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

    TYPES: "gtt_zfidt00242 TYPE TABLE OF gty_zfidt00242,
      gtt_zfidt00242   TYPE TABLE OF gty_data_detail,
      gtt_zfidt00269   TYPE TABLE OF gty_data_detail,
      gtt_acdoca       TYPE TABLE OF gty_data_detail,

      gtt_zfidt00242_x TYPE TABLE OF gty_data_detail_x,
      gtt_zfidt00269_x TYPE TABLE OF gty_data_detail_x,
      gtt_acdoca_x     TYPE TABLE OF gty_data_detail_x,
      gtt_acdoca_x_lv4 TYPE TABLE OF gty_data_detail_x_lv4.

    "**********************************************************************

    CLASS-METHODS:

      get_acdoca IMPORTING
                   VALUE(im_flag_peryear) TYPE char1
                   VALUE(im_kind)         TYPE char20
                   VALUE(im_mandt)        TYPE sy-mandt
                   VALUE(im_rldnr)        TYPE acdoca-rldnr
                   VALUE(im_bukrs)        TYPE acdoca-rbukrs
                   VALUE(im_gjahr)        TYPE acdoca-gjahr

                   VALUE(im_poper)        TYPE acdoca-poper
                   VALUE(im_budat)        TYPE acdoca-budat
                   VALUE(im_augdt)        TYPE acdoca-augdt

                   VALUE(im_where)        TYPE sxmsbody
                   VALUE(im_where1)       TYPE sxmsbody
                   VALUE(im_where2)       TYPE sxmsbody
                   VALUE(im_where3)       TYPE sxmsbody
                   VALUE(im_where4)       TYPE sxmsbody
                   VALUE(im_where5)       TYPE sxmsbody
                 EXPORTING
                   VALUE(et_acdoca)       TYPE gtt_acdoca
                 RAISING
                   cx_amdp_error,

      get_acdoca_x IMPORTING
                     VALUE(im_flag_peryear) TYPE char1

                     VALUE(im_kind)         TYPE char20
                     VALUE(im_mandt)        TYPE sy-mandt
                     VALUE(im_rldnr)        TYPE acdoca-rldnr
                     VALUE(im_bukrs)        TYPE acdoca-rbukrs
                     VALUE(im_gjahr)        TYPE acdoca-gjahr

                     VALUE(im_poper)        TYPE acdoca-poper
                     VALUE(im_budat)        TYPE acdoca-budat
                     VALUE(im_augdt)        TYPE acdoca-augdt

                     VALUE(im_where)        TYPE sxmsbody
                     VALUE(im_where1)       TYPE sxmsbody
                     VALUE(im_where2)       TYPE sxmsbody
                     VALUE(im_where3)       TYPE sxmsbody
                     VALUE(im_where4)       TYPE sxmsbody
                     VALUE(im_where5)       TYPE sxmsbody
                   EXPORTING
                     VALUE(et_acdoca)       TYPE gtt_acdoca_x
                   RAISING
                     cx_amdp_error,

      get_acdoca_x_lv4 IMPORTING
                         VALUE(im_flag_peryear) TYPE char1

                         VALUE(im_kind)         TYPE char20
                         VALUE(im_mandt)        TYPE sy-mandt
                         VALUE(im_rldnr)        TYPE acdoca-rldnr
                         VALUE(im_bukrs)        TYPE acdoca-rbukrs
                         VALUE(im_gjahr)        TYPE acdoca-gjahr

                         VALUE(im_poper)        TYPE acdoca-poper
                         VALUE(im_budat)        TYPE acdoca-budat
                         VALUE(im_augdt)        TYPE acdoca-augdt

                         VALUE(im_where)        TYPE sxmsbody
                         VALUE(im_where1)       TYPE sxmsbody
                         VALUE(im_where2)       TYPE sxmsbody
                         VALUE(im_where3)       TYPE sxmsbody
                         VALUE(im_where4)       TYPE sxmsbody
                         VALUE(im_where5)       TYPE sxmsbody
                       EXPORTING
                         VALUE(et_acdoca)       TYPE gtt_acdoca_x_lv4
                       RAISING
                         cx_amdp_error.

    "**********************************************************************

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zficl_amdp_012 IMPLEMENTATION.

  METHOD get_acdoca    BY DATABASE PROCEDURE FOR HDB
                           LANGUAGE SQLSCRIPT
                           OPTIONS READ-ONLY
                           USING zfivt00091.

    et_acdoca_temp = select * from zfivt00091
                     where hsl != '0.00';

    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where);

    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where1);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where2);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where3);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where4);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where5);

    IF im_flag_peryear = '' THEN

        IF :im_kind = 'ACDOCA RA' then

*            et_acdoca_temp_b = select
*                         a.rclnt,
*
*                         'ACDOCA RA' as tabname,
*                         a.rldnr,
*                         a.rbukrs,
*                         a.gjahr,
*                         a.poper,
*                         a.belnr,
*                         a.buzei,
*
*                         a.bktxt,
*
*                         '' as fstag,
*                         '' as zopenitem,
*                         '' as txt50,
**                         '' as rtcur,
*
*                         a.rhcur,
*                         a.racct,
*
*                         a.glaccount_type,
*                         a.ktopl,
*
*                         a.hsl,
**                         '' as tsl,
*
*                         a.budat,
*
*                         a.xopvw,
*                         a.mitkz,
*
*                         a.augdt,
*                         a.zuonr,
*
*                         a.zzku,
*                         a.zzcp,
*                         a.zzpr,
*                         a.zzch,
*                         a.zzpo,
*                         a.zzcc,
*                         a.zz07,
*                         a.zz08,
*                         a.zz09,
*                         a.zz10,
*
*                         '' as kostl,
*                         '' as prctr,
*                         '' as erdat,
*                         '' as erzeit,
*                         '' as ernam
*                             from :et_acdoca_temp as a
*                             where a.rclnt = :im_mandt and
*                                   a.rldnr = :im_rldnr and
*                                   a.rbukrs = :im_bukrs and
*                                   a.gjahr = :im_gjahr and
*
*                                   a.mitkz != '' and
*                                   a.poper = :im_poper and
*                                   a.budat < :im_budat and
*                                   (  a.augdt = '00000000' or a.augdt > :im_augdt );

        ELSEif :im_kind = 'ACDOCA OI' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA OI' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and

                                   a.xopvw != '' and
                                   a.poper = :im_poper and
                                   a.budat < :im_budat and
                                   (  a.augdt = '00000000' or a.augdt > :im_augdt );

        elseif :im_kind = 'ACDOCA BS' then

*            et_acdoca_temp_b = select
*                         a.rclnt,
*
*                         'ACDOCA BS' as tabname,
*                         a.rldnr,
*                         a.rbukrs,
*                         a.gjahr,
*                         a.poper,
*                         a.belnr,
*                         a.buzei,
*
*                         a.bktxt,
*
*                         '' as fstag,
*                         '' as zopenitem,
*                         '' as txt50,
**                         '' as rtcur,
*
*                         a.rhcur,
*                         a.racct,
*
*                         a.glaccount_type,
*                         a.ktopl,
*
*                         a.hsl,
**                         '' as tsl,
*
*                         a.budat,
*
*                         a.xopvw,
*                         a.mitkz,
*
*                         a.augdt,
*                         a.zuonr,
*
*                         a.zzku,
*                         a.zzcp,
*                         a.zzpr,
*                         a.zzch,
*                         a.zzpo,
*                         a.zzcc,
*                         a.zz07,
*                         a.zz08,
*                         a.zz09,
*                         a.zz10,
*
*                         '' as kostl,
*                         '' as prctr,
*                         '' as erdat,
*                         '' as erzeit,
*                         '' as ernam
*
*                             from :et_acdoca_temp as a
*                             where a.rclnt = :im_mandt and
*                                   a.rldnr = :im_rldnr and
*                                   a.rbukrs = :im_bukrs and
*                                   a.gjahr = :im_gjahr and
*
*                                   a.mitkz = '' and
*                                   a.xopvw = '' and
*                                   a.glaccount_type = 'X' and
*                                   a.poper = :im_poper and
*                                   a.budat < :im_budat;

        elseif :im_kind = 'ACDOCA PL' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA PL' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and

                                   a.glaccount_type = 'P' and
                                   a.poper = :im_poper and
                                   a.budat <= :im_budat;

        ELSEif :im_kind = 'ACDOCA' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.budat < :im_budat;

        ELSEif :im_kind = 'ACDOCA 242' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA 242' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
*                                   a.gjahr <= :im_gjahr and
                                   a.budat < :im_budat and
                                   a.bttype != 'RFBC';

        ELSEif :im_kind = 'ACDOCA 269' then

*            et_acdoca_temp_b = select
*                         a.rclnt,
*
*                         'ACDOCA 269' as tabname,
*                         a.rldnr,
*                         a.rbukrs,
*                         a.gjahr,
*                         a.poper,
*                         a.belnr,
*                         a.buzei,
*
*                         a.bktxt,
*
*                         '' as fstag,
*                         '' as zopenitem,
*                         '' as txt50,
**                         '' as rtcur,
*
*                         a.rhcur,
*                         a.racct,
*
*                         a.glaccount_type,
*                         a.ktopl,
*
*                         a.hsl,
**                         '' as tsl,
*
*                         a.budat,
*
*                         a.xopvw,
*                         a.mitkz,
*
*                         a.augdt,
*                         a.zuonr,
*
*                         a.zzku,
*                         a.zzcp,
*                         a.zzpr,
*                         a.zzch,
*                         a.zzpo,
*                         a.zzcc,
*                         a.zz07,
*                         a.zz08,
*                         a.zz09,
*                         a.zz10,
*
*                         '' as kostl,
*                         '' as prctr,
*                         '' as erdat,
*                         '' as erzeit,
*                         '' as ernam
*
*                             from :et_acdoca_temp as a
*                             where a.rclnt = :im_mandt and
*                                   a.rldnr = :im_rldnr and
*                                   a.rbukrs = :im_bukrs and
*                                   a.gjahr = :im_gjahr and
*                                   a.poper = :im_poper;

        END if;

    ELSE

        if :im_kind = 'ACDOCA OI' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA OI' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and

                                   a.xopvw != '' and
                                   a.poper = :im_poper and
                                   a.budat < :im_budat and
                                   (  a.augdt = '00000000' or a.augdt > :im_augdt );

        elseif :im_kind = 'ACDOCA PL' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA PL' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and

                                   a.glaccount_type = 'P' and
                                   a.poper = :im_poper and
                                   a.budat <= :im_budat;

        ELSEif :im_kind = 'ACDOCA' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and
                                   a.budat < :im_budat;

        ELSEif :im_kind = 'ACDOCA 242' then

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA 242' as tabname,
                         a.rldnr,
                         a.rbukrs,
                         a.gjahr,
                         a.poper,
                         a.belnr,
                         a.buzei,

                         a.bktxt,

                         '' as fstag,
                         '' as zopenitem,
                         '' as txt50,
*                         '' as rtcur,

                         a.rhcur,
                         a.racct,

                         a.glaccount_type,
                         a.ktopl,

                         a.hsl,
*                         '' as tsl,

                         a.budat,

                         a.xopvw,
                         a.mitkz,

                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         '' as kostl,
                         '' as prctr,
                         '' as erdat,
                         '' as erzeit,
                         '' as ernam

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and
                                   a.budat < :im_budat and
                                   a.bttype != 'RFBC';

        END if;

    END IF;

    et_acdoca = select * from :et_acdoca_temp_b
                where hsl != '0.00';

  ENDMETHOD.

  METHOD get_acdoca_x    BY DATABASE PROCEDURE FOR HDB
                         LANGUAGE SQLSCRIPT
                         OPTIONS READ-ONLY
                         USING zfivt00091.

    et_acdoca_temp = select * from zfivt00091
                     where hsl != '0.00';

    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where);

    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where1);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where2);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where3);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where4);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where5);

    if :im_kind = 'ACDOCA OI' then

        et_acdoca_temp_b = select
                     a.rclnt,

                     'ACDOCA OI' as tabname,
*                     a.rldnr,
*                     a.rbukrs,
*                     a.gjahr,
*                     a.poper,
*                     a.belnr,
*                     a.buzei,

                     a.bktxt,

*                     '' as fstag,
*                     '' as zopenitem,
*                     '' as txt50,
*                     '' as rtcur,

*                     a.rhcur,
*                     a.racct,

*                     a.glaccount_type,
*                     a.ktopl,

*                     a.hsl,
*                     '' as tsl,

*                     a.budat,

*                     a.xopvw,
*                     a.mitkz,

*                     a.augdt,
                     a.zuonr,

                     a.zzku,
                     a.zzcp,
                     a.zzpr,
                     a.zzch,
                     a.zzpo,
                     a.zzcc,
                     a.zz07,
                     a.zz08,
                     a.zz09,
                     a.zz10,

*                     '' as kostl,
*                     '' as prctr,
*                     '' as erdat,
*                     '' as erzeit,
*                     '' as ernam

                     a.rhcur,
                     sum( a.hsl ) as hsl

                         from :et_acdoca_temp as a
                         where a.rclnt = :im_mandt and
                               a.rldnr = :im_rldnr and
                               a.rbukrs = :im_bukrs and
                               a.gjahr = :im_gjahr and

                               a.xopvw != '' and
                               a.poper = :im_poper and
                               a.budat < :im_budat and
                               (  a.augdt = '00000000' or a.augdt > :im_augdt )
                     group by a.rclnt, a.bktxt, a.zuonr,
                              a.zzku, a.zzcp, a.zzpr, a.zzch, a.zzpo, a.zzcc, a.zz07, a.zz08, a.zz09, a.zz10,
                              a.rhcur;

    elseif :im_kind = 'ACDOCA PL' then

        et_acdoca_temp_b = select
                     a.rclnt,

                     'ACDOCA PL' as tabname,
*                     a.rldnr,
*                     a.rbukrs,
*                     a.gjahr,
*                     a.poper,
*                     a.belnr,
*                     a.buzei,

                     a.bktxt,

*                     '' as fstag,
*                     '' as zopenitem,
*                     '' as txt50,
*                     '' as rtcur,

*                     a.rhcur,
*                     a.racct,

*                     a.glaccount_type,
*                     a.ktopl,

*                     a.hsl,
*                     '' as tsl,

*                     a.budat,

*                     a.xopvw,
*                     a.mitkz,

*                     a.augdt,
                     a.zuonr,

                     a.zzku,
                     a.zzcp,
                     a.zzpr,
                     a.zzch,
                     a.zzpo,
                     a.zzcc,
                     a.zz07,
                     a.zz08,
                     a.zz09,
                     a.zz10,

*                     '' as kostl,
*                     '' as prctr,
*                     '' as erdat,
*                     '' as erzeit,
*                     '' as ernam,

                     a.rhcur,
                     sum( a.hsl ) as hsl

                         from :et_acdoca_temp as a
                         where a.rclnt = :im_mandt and
                               a.rldnr = :im_rldnr and
                               a.rbukrs = :im_bukrs and
                               a.gjahr = :im_gjahr and

                               a.glaccount_type = 'P' and
                               a.poper = :im_poper and
                               a.budat <= :im_budat
                     group by a.rclnt, a.bktxt, a.zuonr,
                              a.zzku, a.zzcp, a.zzpr, a.zzch, a.zzpo, a.zzcc, a.zz07, a.zz08, a.zz09, a.zz10,
                              a.rhcur;

    ELSEIF :im_kind = 'ACDOCA 242' THEN

        IF im_flag_peryear = '' THEN

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA 242' as tabname,
*                         a.rldnr,
*                         a.rbukrs,
*                         a.gjahr,
*                         a.poper,
*                         a.belnr,
*                         a.buzei,

                         a.bktxt,

*                         '' as fstag,
*                         '' as zopenitem,
*                         '' as txt50,
*                         '' as rtcur,

*                         a.rhcur,
*                         a.racct,

*                         a.glaccount_type,
*                         a.ktopl,

*                         a.hsl,
*                         '' as tsl,

*                         a.budat,

*                         a.xopvw,
*                         a.mitkz,

*                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

*                         '' as kostl,
*                         '' as prctr,
*                         '' as erdat,
*                         '' as erzeit,
*                         '' as ernam

                         a.rhcur,
                         sum( a.hsl ) as hsl

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
*                                   a.gjahr <= :im_gjahr and
                                   a.budat < :im_budat and
                                   a.bttype != 'RFBC'
                         group by a.rclnt, a.bktxt, a.zuonr,
                                  a.zzku, a.zzcp, a.zzpr, a.zzch, a.zzpo, a.zzcc, a.zz07, a.zz08, a.zz09, a.zz10,
                                  a.rhcur;

        ELSE

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA 242' as tabname,
*                         a.rldnr,
*                         a.rbukrs,
*                         a.gjahr,
*                         a.poper,
*                         a.belnr,
*                         a.buzei,

                         a.bktxt,

*                         '' as fstag,
*                         '' as zopenitem,
*                         '' as txt50,
*                         '' as rtcur,

*                         a.rhcur,
*                         a.racct,

*                         a.glaccount_type,
*                         a.ktopl,

*                         a.hsl,
*                         '' as tsl,

*                         a.budat,

*                         a.xopvw,
*                         a.mitkz,

*                         a.augdt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

*                         '' as kostl,
*                         '' as prctr,
*                         '' as erdat,
*                         '' as erzeit,
*                         '' as ernam

                         a.rhcur,
                         sum( a.hsl ) as hsl

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.gjahr = :im_gjahr and

                                   a.budat < :im_budat and
                                   a.bttype != 'RFBC'
                         group by a.rclnt, a.bktxt, a.zuonr,
                                  a.zzku, a.zzcp, a.zzpr, a.zzch, a.zzpo, a.zzcc, a.zz07, a.zz08, a.zz09, a.zz10,
                                  a.rhcur;

        end if;

    END if;

    et_acdoca = select * from :et_acdoca_temp_b
                where hsl != '0.00';

  ENDMETHOD.

  METHOD get_acdoca_x_lv4  BY DATABASE PROCEDURE FOR HDB
                           LANGUAGE SQLSCRIPT
                           OPTIONS READ-ONLY
                           USING zfivt00091.

    et_acdoca_temp = select * from zfivt00091
                     where hsl != '0.00';

    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where);

    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where1);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where2);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where3);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where4);
    et_acdoca_temp = APPLY_FILTER (:et_acdoca_temp, :im_where5);

    if :im_kind = 'ACDOCA' then

        IF im_flag_peryear = '' THEN

            et_acdoca_temp_b = select
                         a.rclnt,

                         'ACDOCA' as tabname,
                         a.gjahr,
                         a.belnr,
                         a.budat,

                         a.bktxt,
                         a.zuonr,

                         a.zzku,
                         a.zzcp,
                         a.zzpr,
                         a.zzch,
                         a.zzpo,
                         a.zzcc,
                         a.zz07,
                         a.zz08,
                         a.zz09,
                         a.zz10,

                         a.rhcur,
                         sum( a.hsl ) as hsl

                             from :et_acdoca_temp as a
                             where a.rclnt = :im_mandt and
                                   a.rldnr = :im_rldnr and
                                   a.rbukrs = :im_bukrs and
                                   a.budat < :im_budat
                         group by a.rclnt, a.gjahr, a.belnr, a.budat, a.bktxt, a.zuonr,
                                  a.zzku, a.zzcp, a.zzpr, a.zzch, a.zzpo, a.zzcc, a.zz07, a.zz08, a.zz09, a.zz10,
                                  a.rhcur;

        ELSE

             et_acdoca_temp_b = select
                                 a.rclnt,

                                 'ACDOCA' as tabname,
                                 a.gjahr,
                                 a.belnr,
                                 a.budat,

                                 a.bktxt,
                                 a.zuonr,

                                 a.zzku,
                                 a.zzcp,
                                 a.zzpr,
                                 a.zzch,
                                 a.zzpo,
                                 a.zzcc,
                                 a.zz07,
                                 a.zz08,
                                 a.zz09,
                                 a.zz10,

                                 a.rhcur,
                                 sum( a.hsl ) as hsl

                                     from :et_acdoca_temp as a
                                     where a.rclnt = :im_mandt and
                                           a.rldnr = :im_rldnr and
                                           a.rbukrs = :im_bukrs and
                                           a.gjahr = :im_gjahr and
                                           a.budat < :im_budat
                                 group by a.rclnt, a.gjahr, a.belnr, a.budat, a.bktxt, a.zuonr,
                                          a.zzku, a.zzcp, a.zzpr, a.zzch, a.zzpo, a.zzcc, a.zz07, a.zz08, a.zz09, a.zz10,
                                          a.rhcur;

        END IF;

    END if;

    et_acdoca = select * from :et_acdoca_temp_b
                where hsl != '0.00';

  ENDMETHOD.

ENDCLASS.
