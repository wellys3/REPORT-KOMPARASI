@AbapCatalog.sqlViewName: 'ZFIVT00093'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZFIDT00269 for Program ZFI02R0032'
define view ZFICD_zfidt00269
  as select from zfidt00269 as a
      left outer join ska1   as b on  b.ktopl = 'ADMF'
                                and a.saknr  = b.saknr
    left outer join skb1   as c on  a.bukrs = c.bukrs
                                and a.saknr  = c.saknr
{

  key a.mandt as rclnt,
  key a.rldnr,
  key a.gjahr,
  key a.poper,
  key a.bukrs,
  key a.saknr,
  key a.buzei,

      b.glaccount_type, 
      b.ktopl,

      a.fstag,
      a.zopenitem,

      @Semantics: { amount : {currencyCode: 'rhcur'} }
      a.hsl,

      @Semantics: { amount : {currencyCode: 'rtcur'} }
      a.tsl,

      a.txt50,
      a.rhcur,
      c.waers as rhcur_skb1,
      a.rtcur,

//      a.zzku,
//      a.zzcp,
//      a.zzpr,
//      a.zzch,
//      a.zzpo,
//      a.zz07,
//      a.zz08,
//      a.zz09,
//      a.zz10,

//      lpad ( a.zzku, 2, '0' ) as zzku,
//      lpad ( a.zzcp, 2, '0' ) as zzcp,
//      lpad ( a.zzpr, 3, '0' ) as zzpr,
//      lpad ( a.zzch, 4, '0' ) as zzch,
//      lpad ( a.zzpo, 5, '0' ) as zzpo,
//      lpad ( a.zzcc, 4, '0' ) as zzcc,
//      lpad ( a.zz07, 6, '0' ) as zz07,
//      lpad ( a.zz08, 6, '0' ) as zz08,
//      lpad ( a.zz09, 6, '0' ) as zz09,
//      lpad ( a.zz10, 6, '0' ) as zz10,

      case
        when a.zzku = '' then ''
        else lpad ( a.zzku, 2, '0' )
      end as zzku,
      
      case
        when a.zzcp = '' then ''
        else lpad ( a.zzcp, 2, '0' )
      end as zzcp,
      
      case
        when a.zzpr = '' then ''
        else lpad ( a.zzpr, 3, '0' )
      end as zzpr,
      
      case
        when a.zzch = '' then ''
        else lpad ( a.zzch, 4, '0' )
      end as zzch,
      
      case
        when a.zzpo = '' then ''
        else lpad ( a.zzpo, 5, '0' )
      end as zzpo,
      
      case
        when a.zzcc = '' then ''
        else lpad ( a.zzcc, 4, '0' )
      end as zzcc,
      
      case
        when a.zz07 = '' then ''
        else lpad ( a.zz07, 6, '0' )
      end as zz07,
      
      case
        when a.zz08 = '' then ''
        else lpad ( a.zz08, 6, '0' )
      end as zz08,
      
      case
        when a.zz09 = '' then ''
        else lpad ( a.zz09, 6, '0' )
      end as zz09,
      
      case
        when a.zz10 = '' then ''
        else lpad ( a.zz10, 6, '0' )
      end as zz10,

      a.kostl,
      a.prctr,
      a.erdat,
      a.erzeit,
      a.ernam
}
