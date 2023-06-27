@AbapCatalog.sqlViewName: 'ZFIVT00102'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ACDOCA for Program ZFI02R0032'
define view ZFICD_ACDOCA_2_SUM
  //  with parameters
  //    im_zzku : abap.char( 1000 )
  as select from zfivt00091
  //  (
  //    im_zzku   : $parameters.im_zzku
  //  )
  as a

{

  key a.rclnt,

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

      @Semantics: { amount : {currencyCode: 'rhcur'} }
      sum(a.hsl) as hsl

}
group by
  a.rclnt,

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

  a.rhcur
