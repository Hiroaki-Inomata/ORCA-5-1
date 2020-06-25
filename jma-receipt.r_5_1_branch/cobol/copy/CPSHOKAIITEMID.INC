      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *    患者照会項目ID  COPY (CPSHOKAIITEMID.INC)             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付     内容
      ************************************************************
       01  SHOKAIITEMID-AREA.
           03  SHKITEMID-STSRYYMD      PIC 9(05)   VALUE   001.
           03  SHKITEMID-EDSRYYMD      PIC 9(05)   VALUE   002.
           03  SHKITEMID-STBIRTHDAY    PIC 9(05)   VALUE   003.
           03  SHKITEMID-EDBIRTHDAY    PIC 9(05)   VALUE   004.
           03  SHKITEMID-STAGE         PIC 9(05)   VALUE   005.
           03  SHKITEMID-EDAGE         PIC 9(05)   VALUE   006.
           03  SHKITEMID-SEX           PIC 9(05)   VALUE   007.
           03  SHKITEMID-STLASTYMD     PIC 9(05)   VALUE   008.
           03  SHKITEMID-EDLASTYMD     PIC 9(05)   VALUE   009.
           03  SHKITEMID-NYUGAIKBN     PIC 9(05)   VALUE   010.
           03  SHKITEMID-SRYKA         PIC 9(05)   VALUE   011.
           03  SHKITEMID-STPTNUM       PIC 9(05)   VALUE   012.
           03  SHKITEMID-EDPTNUM       PIC 9(05)   VALUE   013.
           03  SHKITEMID-CHKTSTPT      PIC 9(05)   VALUE   014.
           03  SHKITEMID-POST01        PIC 9(05)   VALUE   015.
           03  SHKITEMID-POST02        PIC 9(05)   VALUE   016.
           03  SHKITEMID-POST03        PIC 9(05)   VALUE   017.
           03  SHKITEMID-POST04        PIC 9(05)   VALUE   018.
           03  SHKITEMID-POST05        PIC 9(05)   VALUE   019.
           03  SHKITEMID-JYUSHO01      PIC 9(05)   VALUE   020.
           03  SHKITEMID-JYUSHO02      PIC 9(05)   VALUE   021.
           03  SHKITEMID-JYUSHO03      PIC 9(05)   VALUE   022.
           03  SHKITEMID-JYUSHO04      PIC 9(05)   VALUE   023.
           03  SHKITEMID-JYUSHO05      PIC 9(05)   VALUE   024.
           03  SHKITEMID-KANANAME01    PIC 9(05)   VALUE   025.
           03  SHKITEMID-KANANAME02    PIC 9(05)   VALUE   026.
           03  SHKITEMID-KANANAME03    PIC 9(05)   VALUE   027.
           03  SHKITEMID-KANANAME04    PIC 9(05)   VALUE   028.
           03  SHKITEMID-KANANAME05    PIC 9(05)   VALUE   029.
           03  SHKITEMID-NAME01        PIC 9(05)   VALUE   030.
           03  SHKITEMID-NAME02        PIC 9(05)   VALUE   031.
           03  SHKITEMID-NAME03        PIC 9(05)   VALUE   032.
           03  SHKITEMID-NAME04        PIC 9(05)   VALUE   033.
           03  SHKITEMID-NAME05        PIC 9(05)   VALUE   034.
           03  SHKITEMID-CONDITION101  PIC 9(05)   VALUE   035.
           03  SHKITEMID-CONDITION102  PIC 9(05)   VALUE   036.
           03  SHKITEMID-CONDITION103  PIC 9(05)   VALUE   037.
           03  SHKITEMID-CONDITION201  PIC 9(05)   VALUE   038.
           03  SHKITEMID-CONDITION202  PIC 9(05)   VALUE   039.
           03  SHKITEMID-CONDITION203  PIC 9(05)   VALUE   040.
           03  SHKITEMID-CONDITION301  PIC 9(05)   VALUE   041.
           03  SHKITEMID-CONDITION302  PIC 9(05)   VALUE   042.
           03  SHKITEMID-CONDITION303  PIC 9(05)   VALUE   043.
           03  SHKITEMID-GENMEN01      PIC 9(05)   VALUE   044.
           03  SHKITEMID-GENMEN02      PIC 9(05)   VALUE   045.
           03  SHKITEMID-GENMEN03      PIC 9(05)   VALUE   046.
           03  SHKITEMID-COMMENT01     PIC 9(05)   VALUE   047.
           03  SHKITEMID-COMMENT02     PIC 9(05)   VALUE   048.
           03  SHKITEMID-COMMENT03     PIC 9(05)   VALUE   049.
           03  SHKITEMID-TEL-TO        PIC 9(05)   VALUE   050.
           03  SHKITEMID-TEL01         PIC 9(05)   VALUE   051.
           03  SHKITEMID-TEL02         PIC 9(05)   VALUE   052.
           03  SHKITEMID-TEL03         PIC 9(05)   VALUE   053.
           03  SHKITEMID-TSY           PIC 9(05)   VALUE   054.
           03  SHKITEMID-STTSYSKBT     PIC 9(05)   VALUE   055.
           03  SHKITEMID-STTSYYM       PIC 9(05)   VALUE   056.
           03  SHKITEMID-EDTSYYM       PIC 9(05)   VALUE   057.
           03  SHKITEMID-HONKZKKBN     PIC 9(05)   VALUE   058.
           03  SHKITEMID-HKNJANUM01    PIC 9(05)   VALUE   059.
           03  SHKITEMID-HKNJANUM02    PIC 9(05)   VALUE   060.
           03  SHKITEMID-HKNJANUM03    PIC 9(05)   VALUE   061.
           03  SHKITEMID-HKNJANUM04    PIC 9(05)   VALUE   062.
           03  SHKITEMID-HKNJANUM05    PIC 9(05)   VALUE   063.
           03  SHKITEMID-HKNNUM01      PIC 9(05)   VALUE   064.
           03  SHKITEMID-HKNNUM02      PIC 9(05)   VALUE   065.
           03  SHKITEMID-HKNNUM03      PIC 9(05)   VALUE   066.
           03  SHKITEMID-HKNNUM04      PIC 9(05)   VALUE   067.
           03  SHKITEMID-HKNNUM05      PIC 9(05)   VALUE   068.
           03  SHKITEMID-KIGO01        PIC 9(05)   VALUE   069.
           03  SHKITEMID-KIGO02        PIC 9(05)   VALUE   070.
           03  SHKITEMID-KIGO03        PIC 9(05)   VALUE   071.
           03  SHKITEMID-KIGO04        PIC 9(05)   VALUE   072.
           03  SHKITEMID-KIGO05        PIC 9(05)   VALUE   073.
           03  SHKITEMID-NUM01         PIC 9(05)   VALUE   074.
           03  SHKITEMID-NUM02         PIC 9(05)   VALUE   075.
           03  SHKITEMID-NUM03         PIC 9(05)   VALUE   076.
           03  SHKITEMID-NUM04         PIC 9(05)   VALUE   077.
           03  SHKITEMID-NUM05         PIC 9(05)   VALUE   078.
           03  SHKITEMID-RDHKNKOH      PIC 9(05)   VALUE   079.
           03  SHKITEMID-RDKOHNUM      PIC 9(05)   VALUE   080.
           03  SHKITEMID-KOHNUM01      PIC 9(05)   VALUE   081.
           03  SHKITEMID-KOHNUM02      PIC 9(05)   VALUE   082.
           03  SHKITEMID-KOHNUM03      PIC 9(05)   VALUE   083.
           03  SHKITEMID-KOHNUM04      PIC 9(05)   VALUE   084.
           03  SHKITEMID-KOHNUM05      PIC 9(05)   VALUE   085.
           03  SHKITEMID-FTNJANUM01    PIC 9(05)   VALUE   086.
           03  SHKITEMID-FTNJANUM02    PIC 9(05)   VALUE   087.
           03  SHKITEMID-FTNJANUM03    PIC 9(05)   VALUE   088.
           03  SHKITEMID-FTNJANUM04    PIC 9(05)   VALUE   089.
           03  SHKITEMID-FTNJANUM05    PIC 9(05)   VALUE   090.
           03  SHKITEMID-UTAGAIKBN     PIC 9(05)   VALUE   091.
           03  SHKITEMID-TENKIKBN      PIC 9(05)   VALUE   092.
           03  SHKITEMID-STTENKIYMD    PIC 9(05)   VALUE   093.
           03  SHKITEMID-EDTENKIYMD    PIC 9(05)   VALUE   094.
           03  SHKITEMID-RDBYOMEI      PIC 9(05)   VALUE   095.
           03  SHKITEMID-BYOMEICD01    PIC 9(05)   VALUE   096.
           03  SHKITEMID-BYOMEICD02    PIC 9(05)   VALUE   097.
           03  SHKITEMID-BYOMEICD03    PIC 9(05)   VALUE   098.
           03  SHKITEMID-BYOMEICD04    PIC 9(05)   VALUE   099.
           03  SHKITEMID-BYOMEICD05    PIC 9(05)   VALUE   100.
           03  SHKITEMID-BYOMEI01      PIC 9(05)   VALUE   101.
           03  SHKITEMID-BYOMEI02      PIC 9(05)   VALUE   102.
           03  SHKITEMID-BYOMEI03      PIC 9(05)   VALUE   103.
           03  SHKITEMID-BYOMEI04      PIC 9(05)   VALUE   104.
           03  SHKITEMID-BYOMEI05      PIC 9(05)   VALUE   105.
           03  SHKITEMID-RDSRYACT      PIC 9(05)   VALUE   106.
           03  SHKITEMID-SRYCD01       PIC 9(05)   VALUE   107.
           03  SHKITEMID-SRYCD02       PIC 9(05)   VALUE   108.
           03  SHKITEMID-SRYCD03       PIC 9(05)   VALUE   109.
           03  SHKITEMID-SRYCD04       PIC 9(05)   VALUE   110.
           03  SHKITEMID-SRYCD05       PIC 9(05)   VALUE   111.
           03  SHKITEMID-KMK01         PIC 9(05)   VALUE   112.
           03  SHKITEMID-KMK02         PIC 9(05)   VALUE   113.
           03  SHKITEMID-KMK03         PIC 9(05)   VALUE   114.
           03  SHKITEMID-RDIO01        PIC 9(05)   VALUE   115.
           03  SHKITEMID-RDIO02        PIC 9(05)   VALUE   116.
           03  SHKITEMID-RDIO03        PIC 9(05)   VALUE   117.
           03  SHKITEMID-DEATH         PIC 9(05)   VALUE   118.
           03  SHKITEMID-SRYKBN        PIC 9(05)   VALUE   119.
           03  SHKITEMID-SRYSYUKBN     PIC 9(05)   VALUE   120.
           03  SHKITEMID-SELCOMMENT    PIC 9(05)   VALUE   121.
           03  SHKITEMID-BIRTHMON      PIC 9(05)   VALUE   122.
           03  SHKITEMID-KYUSEIKBN     PIC 9(05)   VALUE   123.
           03  SHKITEMID-SYUBYOKBN     PIC 9(05)   VALUE   124.
           03  SHKITEMID-TOKKICD       PIC 9(05)   VALUE   125.
           03  SHKITEMID-TOKKINAME     PIC 9(05)   VALUE   126.
           03  SHKITEMID-RECECOM01     PIC 9(05)   VALUE   127.
           03  SHKITEMID-RECECOM02     PIC 9(05)   VALUE   128.
           03  SHKITEMID-RECECOM03     PIC 9(05)   VALUE   129.
           03  SHKITEMID-TKMK01        PIC 9(05)   VALUE   130.
           03  SHKITEMID-TKMK02        PIC 9(05)   VALUE   131.
           03  SHKITEMID-TKMK03        PIC 9(05)   VALUE   132.
           03  SHKITEMID-TRDIO01       PIC 9(05)   VALUE   133.
           03  SHKITEMID-TRDIO02       PIC 9(05)   VALUE   134.
           03  SHKITEMID-TRDIO03       PIC 9(05)   VALUE   135.
           03  SHKITEMID-TOTALTEN      PIC 9(05)   VALUE   136.
           03  SHKITEMID-SRYVAL1       PIC 9(05)   VALUE   137.
           03  SHKITEMID-SRYVAL2       PIC 9(05)   VALUE   138.
           03  SHKITEMID-SRYVAL3       PIC 9(05)   VALUE   139.
           03  SHKITEMID-SRYVAL4       PIC 9(05)   VALUE   140.
           03  SHKITEMID-SRYVAL5       PIC 9(05)   VALUE   141.
           03  SHKITEMID-SRYVALCOMP1   PIC 9(05)   VALUE   142.
           03  SHKITEMID-SRYVALCOMP2   PIC 9(05)   VALUE   143.
           03  SHKITEMID-SRYVALCOMP3   PIC 9(05)   VALUE   144.
           03  SHKITEMID-SRYVALCOMP4   PIC 9(05)   VALUE   145.
           03  SHKITEMID-SRYVALCOMP5   PIC 9(05)   VALUE   146.
           03  SHKITEMID-SRYVALITEM    PIC 9(05)   VALUE   147.
           03  SHKITEMID-SKMK01        PIC 9(05)   VALUE   148.
           03  SHKITEMID-SKMK02        PIC 9(05)   VALUE   149.
           03  SHKITEMID-SKMK03        PIC 9(05)   VALUE   150.
           03  SHKITEMID-SRDIO01       PIC 9(05)   VALUE   151.
           03  SHKITEMID-SRDIO02       PIC 9(05)   VALUE   152.
           03  SHKITEMID-SRDIO03       PIC 9(05)   VALUE   153.
           03  SHKITEMID-SAIGAIKBN     PIC 9(05)   VALUE   154.
           03  SHKITEMID-BYOCOMP1      PIC 9(05)   VALUE   155.
           03  SHKITEMID-BYOCOMP2      PIC 9(05)   VALUE   156.
           03  SHKITEMID-BYOCOMP3      PIC 9(05)   VALUE   157.
           03  SHKITEMID-BYOCOMP4      PIC 9(05)   VALUE   158.
           03  SHKITEMID-BYOCOMP5      PIC 9(05)   VALUE   159.
           03  SHKITEMID-JKNHKNJANUM   PIC 9(05)   VALUE   160.
           03  SHKITEMID-JKNHKNNUM     PIC 9(05)   VALUE   161.
           03  SHKITEMID-JKNKOHNUM     PIC 9(05)   VALUE   162.
           03  SHKITEMID-JKNBYOMEI     PIC 9(05)   VALUE   163.
           03  SHKITEMID-JKNSRYACT     PIC 9(05)   VALUE   164.
           03  SHKITEMID-TENKENRECE    PIC 9(05)   VALUE   165.
           03  SHKITEMID-STBYOYMD      PIC 9(05)   VALUE   166.
           03  SHKITEMID-EDBYOYMD      PIC 9(05)   VALUE   167.
           03  SHKITEMID-HOKATSUKBN    PIC 9(05)   VALUE   168.
           03  SHKITEMID-TSTPT         PIC 9(05)   VALUE   169.
           03  SHKITEMID-HKNEDYM       PIC 9(05)   VALUE   170.
           03  SHKITEMID-KOHEDYM       PIC 9(05)   VALUE   171.
           03  SHKITEMID-FTNRATE       PIC 9(05)   VALUE   172.
           03  SHKITEMID-AGREEMENT     PIC 9(05)   VALUE   173.
           03  SHKITEMID-KHSKBT        PIC 9(05)   VALUE   174.
           03  SHKITEMID-STKHYM        PIC 9(05)   VALUE   175.
           03  SHKITEMID-EDKHYM        PIC 9(05)   VALUE   176.
           03  SHKITEMID-KHHKNJANUM    PIC 9(05)   VALUE   177.
           03  SHKITEMID-KNSKBT        PIC 9(05)   VALUE   178.
           03  SHKITEMID-STKNYM        PIC 9(05)   VALUE   179.
           03  SHKITEMID-EDKNYM        PIC 9(05)   VALUE   180.
           03  SHKITEMID-KNJOTAI       PIC 9(05)   VALUE   181.
           03  SHKITEMID-DRCD          PIC 9(05)   VALUE   182.
