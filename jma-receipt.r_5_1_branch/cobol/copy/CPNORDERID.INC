      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    Æþ±¡CLAIM¹àÌÜID  COPY (CPNORDERID.INC)                *
      ************************************************************
      ************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ     ÆâÍÆ
      ************************************************************
       01  NORDERID-AREA.
           03  NORDERID-ORDER-NYUIN        PIC X(02) VALUE "01".
           03  NORDERID-ORDER-KYUSYOKU     PIC X(02) VALUE "02".
           03  NORDERID-ORDER-DELNYUIN     PIC X(02) VALUE "11".
           03  NORDERID-ORDER-ALLDELNYUIN  PIC X(02) VALUE "12".
      *
           03  NORDERID-VAL.
      *
               05  NORDERID-NYUIN-G.
                   07  NORDERID-NYUINCL1   PIC X(02) VALUE "01".
                   07  NORDERID-NYUINCL2   PIC X(03) VALUE "001".
                   07  NORDERID-NYUINID    PIC X(04) VALUE "0101".
                   07  NORDERID-NYUINMES1  PIC X(30) VALUE "Æþ±¡".
                   07  NORDERID-NYUINMES2  PIC X(20) VALUE "Æþ±¡".
      *
               05  NORDERID-TAIIN-G.
                   07  NORDERID-TAIINCL1   PIC X(02) VALUE "01".
                   07  NORDERID-TAIINCL2   PIC X(03) VALUE "002".
                   07  NORDERID-TAIINID    PIC X(04) VALUE "0102".
                   07  NORDERID-TAIINMES1  PIC X(30) VALUE "Âà±¡".
                   07  NORDERID-TAIINMES2  PIC X(20) VALUE "Âà±¡".
      *
               05  NORDERID-IDOBTU-G.
                   07  NORDERID-IDOBTUCL1  PIC X(02) VALUE "01".
                   07  NORDERID-IDOBTUCL2  PIC X(03) VALUE "003".
                   07  NORDERID-IDOBTUID   PIC X(04) VALUE "0103".
                   07  NORDERID-IDOBTUMES1 PIC X(30) VALUE "Å¾Åï".
                   07  NORDERID-IDOBTUMES2 PIC X(20) VALUE "Å¾Åï".
      *
               05  NORDERID-IDOKA-G.
                   07  NORDERID-IDOKACL1   PIC X(02) VALUE "01".
                   07  NORDERID-IDOKACL2   PIC X(03) VALUE "004".
                   07  NORDERID-IDOKAID    PIC X(04) VALUE "0104".
                   07  NORDERID-IDOKAMES1  PIC X(30) VALUE "Å¾²Ê".
                   07  NORDERID-IDOKAMES2  PIC X(20) VALUE "Å¾²Ê".
      *
               05  NORDERID-IDOBRM-G.
                   07  NORDERID-IDOBRMCL1  PIC X(02) VALUE "01".
                   07  NORDERID-IDOBRMCL2  PIC X(03) VALUE "005".
                   07  NORDERID-IDOBRMID   PIC X(04) VALUE "0105".
                   07  NORDERID-IDOBRMMES1 PIC X(30) VALUE "Å¾¼¼".
                   07  NORDERID-IDOBRMMES2 PIC X(20) VALUE "Å¾¼¼".
      *
               05  NORDERID-IDODR-G.
                   07  NORDERID-IDODRCL1   PIC X(02) VALUE "01".
                   07  NORDERID-IDODRCL2   PIC X(03) VALUE "008".
                   07  NORDERID-IDODRID    PIC X(04) VALUE "0106".
                   07  NORDERID-IDODRMES1  PIC X(30) VALUE "Ã´Åö°å".
                   07  NORDERID-IDODRMES2  PIC X(20) VALUE "Ã´Åö°å".
      *
               05  NORDERID-DELNYUIN-G.
                   07  NORDERID-DELNYUINCL1    PIC X(02) VALUE "11".
                   07  NORDERID-DELNYUINCL2    PIC X(03) VALUE "001".
                   07  NORDERID-DELNYUINID     PIC X(04) VALUE "0107".
                   07  NORDERID-DELNYUINMES1   PIC X(30)
                                           VALUE "Æþ±¡¼è¾Ã".
                   07  NORDERID-DELNYUINMES2   PIC X(20)
                                           VALUE "Æþ±¡¼è¾Ã".
      *
               05  NORDERID-ALLDELNYUIN-G.
                   07  NORDERID-ALLDELNYUINCL1  PIC X(02) VALUE "12".
                   07  NORDERID-ALLDELNYUINCL2  PIC X(03) VALUE "001".
                   07  NORDERID-ALLDELNYUINID   PIC X(04) VALUE "0108".
                   07  NORDERID-ALLDELNYUINMES1 PIC X(30)
                                           VALUE "Æþ±¡¼è¾Ã¡Ê²ñ·×´Þ¤à¡Ë".
                   07  NORDERID-ALLDELNYUINMES2 PIC X(20)
                                           VALUE "Æþ±¡¼è¾Ã¡Ê²ñ·×´Þ¤à¡Ë".
      *
               05  NORDERID-NYUIN2-G.
                   07  NORDERID-NYUIN2CL1       PIC X(02) VALUE "XX".
                   07  NORDERID-NYUIN2CL2       PIC X(03) VALUE "XXX".
                   07  NORDERID-NYUIN2ID        PIC X(04) VALUE "0109".
                   07  NORDERID-NYUIN2MES1      PIC X(30) VALUE "Æþ±¡".
                   07  NORDERID-NYUIN2MES2      PIC X(20) VALUE "Æþ±¡".
      *
               05  NORDERID-TAIIN2-G.
                   07  NORDERID-TAIIN2CL1   PIC X(02) VALUE "XX".
                   07  NORDERID-TAIIN2CL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-TAIIN2ID    PIC X(04) VALUE "0110".
                   07  NORDERID-TAIIN2MES1  PIC X(30) VALUE "Âà±¡".
                   07  NORDERID-TAIIN2MES2  PIC X(20) VALUE "Âà±¡".
      *
               05  NORDERID-HENKO-G.
                   07  NORDERID-HENKOCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-HENKOCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-HENKOID    PIC X(04) VALUE "0111".
                   07  NORDERID-HENKOMES1  PIC X(30) VALUE "ÊÑ¹¹".
                   07  NORDERID-HENKOMES2  PIC X(20) VALUE "ÊÑ¹¹".
      *
               05  NORDERID-IDOALL-G.
                   07  NORDERID-IDOALLCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-IDOALLCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-IDOALLID    PIC X(04) VALUE "0112".
                  07 NORDERID-IDOALLMES1 PIC X(30) VALUE "Å¾²ÊÅ¾ÅïÅ¾¼¼".
                  07 NORDERID-IDOALLMES2 PIC X(20) VALUE "Å¾²ÊÅ¾ÅïÅ¾¼¼".
      *
               05  NORDERID-CANNYUIN-G.
                   07  NORDERID-CANNYUINCL1    PIC X(02) VALUE "XX".
                   07  NORDERID-CANNYUINCL2    PIC X(03) VALUE "XXX".
                   07  NORDERID-CANNYUINID     PIC X(04) VALUE "0113".
                   07  NORDERID-CANNYUINMES1   PIC X(30)
                                           VALUE "Æþ±¡¼è¾Ã".
                   07  NORDERID-CANNYUINMES2   PIC X(20)
                                           VALUE "Æþ±¡¼è¾Ã".
      *
               05  NORDERID-ALLCANNYUIN-G.
                   07  NORDERID-ALLCANNYUINCL1  PIC X(02) VALUE "XX".
                   07  NORDERID-ALLCANNYUINCL2  PIC X(03) VALUE "XXX".
                   07  NORDERID-ALLCANNYUINID   PIC X(04) VALUE "0120".
                   07  NORDERID-ALLCANNYUINMES1 PIC X(30)
                                           VALUE "Æþ±¡¼è¾Ã¡Ê²ñ·×´Þ¤à¡Ë".
                   07  NORDERID-ALLCANNYUINMES2 PIC X(20)
                                           VALUE "Æþ±¡¡¦²ñ·×¼è¾Ã".
      *
               05  NORDERID-CANTAIIN-G.
                   07  NORDERID-CANTAIINCL1  PIC X(02) VALUE "XX".
                   07  NORDERID-CANTAIINL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-CANTAIINID   PIC X(04) VALUE "0115".
                   07  NORDERID-CANTAIINMES1 PIC X(30) VALUE "Âà±¡¼è¾Ã".
                   07  NORDERID-CANTAIINMES2 PIC X(20) VALUE "Âà±¡¼è¾Ã".
      *
               05  NORDERID-CANIDO-G.
                   07  NORDERID-CANIDOCL1    PIC X(02) VALUE "XX".
                   07  NORDERID-CANIDOL2     PIC X(03) VALUE "XXX".
                   07  NORDERID-CANIDOID     PIC X(04) VALUE "0116".
                   07  NORDERID-CANIDOMES1   PIC X(30) VALUE "°ÛÆ°¼è¾Ã".
                   07  NORDERID-CANIDOMES2   PIC X(20) VALUE "°ÛÆ°¼è¾Ã".
      *
               05  NORDERID-TEISEI-G.
                   07  NORDERID-TEISEICL1    PIC X(02) VALUE "XX".
                   07  NORDERID-TEISEICL2    PIC X(03) VALUE "XXX".
                   07  NORDERID-TEISEIID     PIC X(04) VALUE "0117".
                   07  NORDERID-TEISEIMES1   PIC X(30)
                                           VALUE "Æþ±¡¡ÊÄûÀµ¡Ë".
                   07  NORDERID-TEISEIMES2   PIC X(20)
                                           VALUE "Æþ±¡¡ÊÄûÀµ¡Ë".
      *
               05  NORDERID-GCANCEL-G.
                   07  NORDERID-GCANCELCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-GCANCELCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-GCANCELID    PIC X(04) VALUE "0200".
                   07  NORDERID-GCANCELMES1  PIC X(30) VALUE "³°Çñ¼è¾Ã".
                   07  NORDERID-GCANCELMES2  PIC X(20) VALUE "³°Çñ¼è¾Ã".
      *
               05  NORDERID-GAIHK-G.
                   07  NORDERID-GAIHKCL1   PIC X(02) VALUE "01".
                   07  NORDERID-GAIHKCL2   PIC X(03) VALUE "006".
                   07  NORDERID-GAIHKID    PIC X(04) VALUE "0201".
                   07  NORDERID-GAIHKMES1  PIC X(30) VALUE "³°Çñ".
                   07  NORDERID-GAIHKMES2  PIC X(20) VALUE "³°Çñ".
      *
               05  NORDERID-GCURE-G.
                   07  NORDERID-GCURECL1   PIC X(02) VALUE "XX".
                   07  NORDERID-GCURECL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-GCUREID    PIC X(04) VALUE "0202".
                   07  NORDERID-GCUREMES1  PIC X(30) 
                                           VALUE "¼£ÎÅ¤Î°Ù¤Î³°Çñ".
                   07  NORDERID-GCUREMES2  PIC X(20)
                                           VALUE "¼£ÎÅ¤Î°Ù¤Î³°Çñ".
      *
               05  NORDERID-GSENT-G.
                   07  NORDERID-GSENTCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-GSENTCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-GSENTID    PIC X(04) VALUE "0203".
                   07  NORDERID-GSENTMES1  PIC X(30) 
                                           VALUE "ÁªÄêÆþ±¡Ãæ¤Î³°Çñ".
                   07  NORDERID-GSENTMES2  PIC X(20)
                                           VALUE "ÁªÄêÆþ±¡Ãæ¤Î³°Çñ".
      *
               05  NORDERID-GBACK-G.
                   07  NORDERID-GBACKCL1   PIC X(02) VALUE "01".
                   07  NORDERID-GBACKCL2   PIC X(03) VALUE "007".
                   07  NORDERID-GBACKID    PIC X(04) VALUE "0205".
                   07  NORDERID-GBACKMES1  PIC X(30) VALUE "µ¢±¡".
                   07  NORDERID-GBACKMES2  PIC X(20) VALUE "µ¢±¡".
      *
               05  NORDERID-GPTST-G.
                   07  NORDERID-GPTSTCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-GPTSTCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-GPTSTID    PIC X(04) VALUE "0206".
                   07  NORDERID-GPTSTMES1  PIC X(30) VALUE "³°ÇñÅù".
                   07  NORDERID-GPTSTMES2  PIC X(20) VALUE "³°ÇñÅù".
      *
               05  NORDERID-TAIJUSHIN40-G.
                   07  NORDERID-TAIJUSHIN40CL1   PIC X(02) VALUE "XX".
                   07  NORDERID-TAIJUSHIN40CL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-TAIJUSHIN40ID    PIC X(04) VALUE "0207".
                   07  NORDERID-TAIJUSHIN40MES1  PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£´£°¡ó¸º»»".
                   07  NORDERID-TAIJUSHIN40MES2  PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£´£°¡ó¸º»»".
      *
               05  NORDERID-TAIJUSHIN10-G.
                   07  NORDERID-TAIJUSHIN10CL1   PIC X(02) VALUE "XX".
                   07  NORDERID-TAIJUSHIN10CL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-TAIJUSHIN10ID    PIC X(04) VALUE "0208".
                   07  NORDERID-TAIJUSHIN10MES1  PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£±£°¡ó¸º»»".
                   07  NORDERID-TAIJUSHIN10MES2  PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£±£°¡ó¸º»»".
      *
               05  NORDERID-TAIJUSHIN20-G.
                   07  NORDERID-TAIJUSHIN20CL1   PIC X(02) VALUE "XX".
                   07  NORDERID-TAIJUSHIN20CL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-TAIJUSHIN20ID    PIC X(04) VALUE "0209".
                   07  NORDERID-TAIJUSHIN20MES1  PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£²£°¡ó¸º»»".
                   07  NORDERID-TAIJUSHIN20MES2  PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£²£°¡ó¸º»»".
      *
               05  NORDERID-TAIJUSHIN15-G.
                   07  NORDERID-TAIJUSHIN15CL1   PIC X(02) VALUE "XX".
                   07  NORDERID-TAIJUSHIN15CL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-TAIJUSHIN15ID    PIC X(04) VALUE "0210".
                   07  NORDERID-TAIJUSHIN15MES1  PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£±£µ¡ó¸º»»".
                   07  NORDERID-TAIJUSHIN15MES2  PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£±£µ¡ó¸º»»".
      *
               05  NORDERID-JIKANGEN-G.
                   07  NORDERID-JIKANGENCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-JIKANGENCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-JIKANGENID    PIC X(04) VALUE "0211".
                   07  NORDERID-JIKANGENMES1  PIC X(30)
                               VALUE "ÆÃÄê»þ´ÖÂà±¡¸º»»".
                   07  NORDERID-JIKANGENMES2  PIC X(20)
                               VALUE "ÆÃÄê»þ´ÖÂà±¡¸º»»".
      *
               05  NORDERID-YOBIGEN-G.
                   07  NORDERID-YOBIGENCL1    PIC X(02) VALUE "XX".
                   07  NORDERID-YOBIGENCL2    PIC X(03) VALUE "XXX".
                   07  NORDERID-YOBIGENID     PIC X(04) VALUE "0212".
                   07  NORDERID-YOBIGENMES1   PIC X(30)
                               VALUE "ÆÃÄêÍËÆüÆþÂà±¡¸º»»".
                   07  NORDERID-YOBIGENMES2   PIC X(20)
                               VALUE "ÆÃÄêÍËÆüÆþÂà±¡¸º»»".
      *
               05  NORDERID-YOBI2GEN-G.
                   07  NORDERID-YOBI2GENCL1   PIC X(02) VALUE "XX".
                   07  NORDERID-YOBI2GENCL2   PIC X(03) VALUE "XXX".
                   07  NORDERID-YOBI2GENID    PIC X(04) VALUE "0213".
                   07  NORDERID-YOBI2GENMES1  PIC X(30)
                               VALUE "ÆÃÄêÍËÆüÆþÂà±¡¸º¡ÜÂ¾¼õ£±£°¡ó¸º".
                   07  NORDERID-YOBI2GENMES2  PIC X(20)
                               VALUE "ÅÚÆü¸º¡ÜÂ¾¼õ£±£°¡ó¸º".
      *
               05  NORDERID-H30TAIJUSHIN35-G.
                   07 NORDERID-H30TAIJUSHIN35CL1 PIC X(02) VALUE "XX".
                   07 NORDERID-H30TAIJUSHIN35CL2 PIC X(03) VALUE "XXX".
                   07 NORDERID-H30TAIJUSHIN35ID  PIC X(04) VALUE "0214".
                   07 NORDERID-H30TAIJUSHIN35MES1  PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£³£µ¡ó¸º»»".
                   07 NORDERID-H30TAIJUSHIN35MES2  PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£³£µ¡ó¸º»»".
      *
               05  NORDERID-H30TAIJUSHIN15-G.
                   07 NORDERID-H30TAIJUSHIN15CL1 PIC X(02) VALUE "XX".
                   07 NORDERID-H30TAIJUSHIN15CL2 PIC X(03) VALUE "XXX".
                   07 NORDERID-H30TAIJUSHIN15ID  PIC X(04) VALUE "0215".
                   07 NORDERID-H30TAIJUSHIN15MES1  PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£±£µ¡ó¸º»»".
                   07 NORDERID-H30TAIJUSHIN15MES2  PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£±£µ¡ó¸º»»".
      *
               05  NORDERID-H30TAIJUSHIN5-G.
                   07 NORDERID-H30TAIJUSHIN5CL1  PIC X(02) VALUE "XX".
                   07 NORDERID-H30TAIJUSHIN5CL2  PIC X(03) VALUE "XXX".
                   07 NORDERID-H30TAIJUSHIN5ID   PIC X(04) VALUE "0216".
                   07 NORDERID-H30TAIJUSHIN5MES1 PIC X(30)
                               VALUE "Â¾°åÎÅµ¡´Ø¼õ¿Ç£µ¡ó¸º»»".
                   07 NORDERID-H30TAIJUSHIN5MES2 PIC X(20)
                               VALUE "Â¾°å¼õ¿Ç£µ¡ó¸º»»".
      *
               05  NORDERID-H30YOBIGEN-G.
                   07 NORDERID-H30YOBIGENCL1  PIC X(02) VALUE "XX".
                   07 NORDERID-H30YOBIGENCL2  PIC X(03) VALUE "XXX".
                   07 NORDERID-H30YOBIGENID   PIC X(04) VALUE "0217".
                   07 NORDERID-H30YOBIGENMES1 PIC X(30)
                               VALUE "ÆÃÄêÍËÆüÆþÂà±¡¸º¡ÜÂ¾¼õ£µ¡ó¸º".
                   07 NORDERID-H30YOBIGENMES2 PIC X(20)
                               VALUE "ÅÚÆü¸º¡ÜÂ¾¼õ£µ¡ó¸º".
      *
               05  NORDERID-FNORL-G.
                   07  NORDERID-FNORLCL1   PIC X(02) VALUE "02".
                   07  NORDERID-FNORLCL2   PIC X(03) VALUE "009".
                   07  NORDERID-FNORLID    PIC X(04) VALUE "0301".
                   07  NORDERID-FNORLMES1  PIC X(30)
                                           VALUE "°ìÈÌ¿©".
                   07  NORDERID-FNORLMES2  PIC X(20)
                                           VALUE "°ìÈÌ¿©".
      *
               05  NORDERID-FSPCL-G.
                   07  NORDERID-FSPCLCL1   PIC X(02) VALUE "02".
                   07  NORDERID-FSPCLCL2   PIC X(03) VALUE "010".
                   07  NORDERID-FSPCLID    PIC X(04) VALUE "0302".
                   07  NORDERID-FSPCLMES1  PIC X(30)
                                           VALUE "ÆÃÊÌ¿©".
                   07  NORDERID-FSPCLMES2  PIC X(20) VALUE "ÆÃÊÌ¿©".
      *
               05  NORDERID-FSELE-G.
                   07  NORDERID-FSELECL1   PIC X(02) VALUE "02".
                   07  NORDERID-FSELECL2   PIC X(03) VALUE "011".
                   07  NORDERID-FSELEID    PIC X(04) VALUE "0303".
                   07  NORDERID-FSELEMES1  PIC X(30)
                                           VALUE "ÁªÂò¿©".
                   07  NORDERID-FSELEMES2  PIC X(20) VALUE "ÁªÂò¿©".
      *
               05  NORDERID-FSPSL-G.
                   07  NORDERID-FSPSLCL1   PIC X(02) VALUE "02".
                   07  NORDERID-FSPSLCL2   PIC X(03) VALUE "013".
                   07  NORDERID-FSPSLID    PIC X(04) VALUE "0304".
                   07  NORDERID-FSPSLMES1  PIC X(30)
                                       VALUE "ÆÃÊÌ¿©¡ÜÁªÂò¿©".
                   07  NORDERID-FSPSLMES2  PIC X(20)
                                       VALUE "ÆÃÊÌ¿©¡ÜÁªÂò¿©".
      *
               05  NORDERID-FSTOP-G.
                   07  NORDERID-FSTOPCL1   PIC X(02) VALUE "02".
                   07  NORDERID-FSTOPCL2   PIC X(03) VALUE "012".
                   07  NORDERID-FSTOPID    PIC X(04) VALUE "0305".
                   07  NORDERID-FSTOPMES1  PIC X(30) VALUE "¿©»ß¤á".
                   07  NORDERID-FSTOPMES2  PIC X(20) VALUE "¿©»ß¤á".
      *
               05  NORDERID-FOOD-G.
                   07  NORDERID-FOODCL1    PIC X(02) VALUE "XX".
                   07  NORDERID-FOODCL2    PIC X(03) VALUE "XXX".
                   07  NORDERID-FOODID     PIC X(04) VALUE "0306".
                   07  NORDERID-FOODMES1   PIC X(30)
                                           VALUE "¿©»ö".
                   07  NORDERID-FOODMES2   PIC X(20)
                                           VALUE "¿©»ö".
      *
               05  NORDERID-FLIQUID-G.
                   07  NORDERID-FLIQUIDCL1  PIC X(02) VALUE "02".
                   07  NORDERID-FLIQUIDCL2  PIC X(03) VALUE "021".
                   07  NORDERID-FLIQUIDID   PIC X(04) VALUE "0307".
                   07  NORDERID-FLIQUIDMES1 PIC X(30)
                                           VALUE "·Ð´É±ÉÍÜ".
                   07  NORDERID-FOODMES2   PIC X(20)
                                           VALUE "·Ð´É±ÉÍÜ".
      *
               05  NORDERID-PTEVAL-G.
                   07  NORDERID-PTEVALCL1  PIC X(02) VALUE "XX".
                   07  NORDERID-PTEVALCL2  PIC X(03) VALUE "XXX".
                   07  NORDERID-PTEVALID   PIC X(04) VALUE "0401".
                   07  NORDERID-PTEVALMES1 PIC X(30)
                                           VALUE "°åÎÅ¶èÊ¬¡¦£Á£Ä£ÌÅÀ¿ô".
                   07  NORDERID-PTEVALMES2 PIC X(20)
                                           VALUE "°åÎÅ¶èÊ¬¡¦£Á£Ä£ÌÅÀ¿ô".
      *
               05  NORDERID-BUNBEN-G.
                   07  NORDERID-BUNBENCL1  PIC X(02) VALUE "XX".
                   07  NORDERID-BUNBENCL2  PIC X(03) VALUE "XXX".
                   07  NORDERID-BUNBENID   PIC X(04) VALUE "0501".
                   07  NORDERID-BUNBENMES1 PIC X(30)
                                           VALUE "½Ð»º°é»ù°ì»þ¶â¾ðÊó".
                   07  NORDERID-BUNBENMES2 PIC X(20)
                                           VALUE "½Ð»º°é»ù°ì»þ¶â¾ðÊó".
      *
               05  NORDERID-SAGAKU-G.
                   07  NORDERID-SAGAKUCL1  PIC X(02) VALUE "XX".
                   07  NORDERID-SAGAKUCL2  PIC X(03) VALUE "XXX".
                   07  NORDERID-SAGAKUID   PIC X(04) VALUE "0601".
                   07  NORDERID-SAGAKUMES1 PIC X(30)
                                           VALUE "¼¼ÎÁº¹³Û".
                   07  NORDERID-SAGAKUMES2 PIC X(20)
                                           VALUE "¼¼ÎÁº¹³Û".
               05  NORDERID-SAGAKUCANCEL-G.
                   07  NORDERID-SAGAKUCANCELCL1  PIC X(02) VALUE "XX".
                   07  NORDERID-SAGAKUCANCELCL2  PIC X(03) VALUE "XXX".
                   07  NORDERID-SAGAKUCANCELID   PIC X(04) VALUE "0602".
                   07  NORDERID-SAGAKUCANCELMES1 PIC X(30)
                                           VALUE "¼¼ÎÁº¹³Û".
                   07  NORDERID-SAGAKUCANCELMES2 PIC X(20)
                                           VALUE "¼¼ÎÁº¹³Û".
      *
               05  NORDERID-CRENACCT-G.
                   07  NORDERID-CRENACCTCL1    PIC X(02) VALUE "XX".
                   07  NORDERID-CRENACCTCL2    PIC X(03) VALUE "XXX".
                   07  NORDERID-CRENACCTID     PIC X(04) VALUE "9901".
                   07  NORDERID-CRENACCTMES1   PIC X(30)
                                               VALUE "Æþ±¡²ñ·×ºîÀ®".
                   07  NORDERID-NACCTMES2 PIC X(20)
                                               VALUE "Æþ±¡²ñ·×ºîÀ®".
      *
           03  NORDERID-R  REDEFINES       NORDERID-VAL.
               05  NORDERID-OCC            OCCURS    44.
                   07  NORDERID-CLAIMID.
                       09  NORDERID-CLAIMIDH   PIC X(02).
                       09  NORDERID-CLAIMIDT   PIC X(03).
                   07  NORDERID-ORCAID         PIC X(04).
                   07  NORDERID-ORCAMES1       PIC X(30).
                   07  NORDERID-ORCAMES2       PIC X(20).
      *
           03  NORDERID-MAX                PIC 9(03) VALUE 44.
