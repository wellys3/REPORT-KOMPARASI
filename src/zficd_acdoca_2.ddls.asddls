@AbapCatalog.sqlViewName: 'ZFIVT00091'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ACDOCA for Program ZFI02R0032'
define view ZFICD_ACDOCA_2
  as select from    acdoca as a
    left outer join bkpf   as b on  a.rbukrs = b.bukrs
                                and a.belnr  = b.belnr
                                and a.gjahr  = b.gjahr
    left outer join skb1   as c on  a.rbukrs = c.bukrs
                                and a.racct  = c.saknr
//    left outer join ska1   as d on  a.ktopl = d.ktopl
//                                and a.racct  = d.saknr
{

  key a.rclnt,
  key a.rldnr,
  key a.rbukrs,
  key a.gjahr,
  key a.belnr,
  key a.docln,

      cast ( lpad ( a.docln, 10, '0'
                  ) as abap.numc(10)
           ) as buzei,
      
      b.bktxt,
      
      a.rhcur,
      
      c.waers as rhcur_skb1,
      
      a.racct,
      a.glaccount_type,
      a.ktopl,
      
      @Semantics: { amount : {currencyCode: 'rhcur'} }
      a.hsl,
      //      sum(a.hsl) as hsl,

      a.poper,
      a.budat,
      c.xopvw,
      
      c.mitkz,
      
//      d.glaccount_type,
      
      a.augdt,
      a.zuonr,
      
      a.bttype,

//      a.zzku,
//      a.zzcp,
//      a.zzpr,
//      a.zzch,
//      a.zzpo,
//      a.zzcc,
//      a.zz07,
//      a.zz08,
//      a.zz09,
//      a.zz10
      
//      lpad ( a.zzku, 2, '0' ) as zzku,
//      lpad ( a.zzcp, 2, '0' ) as zzcp,
//      lpad ( a.zzpr, 3, '0' ) as zzpr,
//      lpad ( a.zzch, 4, '0' ) as zzch,
//      lpad ( a.zzpo, 5, '0' ) as zzpo,
//      lpad ( a.zzcc, 4, '0' ) as zzcc,
//      lpad ( a.zz07, 6, '0' ) as zz07,
//      lpad ( a.zz08, 6, '0' ) as zz08,
//      lpad ( a.zz09, 6, '0' ) as zz09,
//      lpad ( a.zz10, 6, '0' ) as zz10
      
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
      end as zz10
}
