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
       01  QSUB04-LNK.
      *    空白:画面/1:API
           03  QSUB04-DATA-TYPE                     PIC X(1).
      *    0:診療日/1:基本情報/2:保険・公費/3:病名、診療行為
      *    4:介護/5:並び順/A:全部
           03  QSUB04-CHECK-ID                      PIC X(1).
           03  QSUB04-RESULT-TYPE                   PIC X(1).
           03  QSUB04-GMN-KIHON-AREA.
               05  QSUB04-GMN-SRYYMD-G.
                   07  QSUB04-GMN-STSRYYMD          PIC X(10).
                   07  QSUB04-GMN-EDSRYYMD          PIC X(10).
               05  QSUB04-GMN-BIRTHDAY-G.
                   07  QSUB04-GMN-STBIRTHDAY        PIC X(10).
                   07  QSUB04-GMN-EDBIRTHDAY        PIC X(10).
                   07  QSUB04-NAI-STBIRTHDAY        PIC X(8).
                   07  QSUB04-NAI-EDBIRTHDAY        PIC X(8).
               05  QSUB04-GMN-AGE-G.
                   07  QSUB04-GMN-STAGE             PIC X(3).
                   07  QSUB04-GMN-EDAGE             PIC X(3).
               05  QSUB04-GMN-KJNYMD-G.
                   07  QSUB04-GMN-KJNYMD            PIC X(10).
                   07  QSUB04-NAI-KJNYMD            PIC X(8).
               05  QSUB04-GMN-BIRTHMON              PIC X(2).
               05  QSUB04-GMN-SEX-G.
                   07  QSUB04-GMN-SEX               PIC X(1).
                   07  QSUB04-GMN-SEXFL             PIC X(1).
                   07  QSUB04-GMN-SEXMEI            PIC X(8).
               05  QSUB04-GMN-LASTYMD-G.
                   07  QSUB04-GMN-STLASTYMD         PIC X(10).
                   07  QSUB04-GMN-EDLASTYMD         PIC X(10).
                   07  QSUB04-NAI-STLASTYMD         PIC X(8).
                   07  QSUB04-NAI-EDLASTYMD         PIC X(8).
               05  QSUB04-GMN-NYUGAIKBN-G.
                   07  QSUB04-GMN-NYUGAIKBN         PIC X(1).
                   07  QSUB04-GMN-NYUGAIKBNFL       PIC X(1).
                   07  QSUB04-GMN-NYUGAIKBNMEI      PIC X(10).
               05  QSUB04-GMN-SRYKA-G.
                   07  QSUB04-GMN-SRYKA             PIC X(2).
                   07  QSUB04-GMN-SRYKAFL           PIC X(1).
                   07  QSUB04-GMN-SRYKAMEI          PIC X(20).
               05  QSUB04-GMN-DEATH-G.
                   07  QSUB04-GMN-DEATH             PIC X(1).
                   07  QSUB04-GMN-DEATHFL           PIC X(1).
                   07  QSUB04-GMN-DEATHMEI          PIC X(8).
               05  QSUB04-GMN-PTNUM-G.
                   07  QSUB04-GMN-STPTNUM           PIC X(20).
                   07  QSUB04-GMN-EDPTNUM           PIC X(20).
               05  QSUB04-GMN-AGREEMENT-G.
                   07  QSUB04-GMN-AGREEMENT         PIC X(1).
                   07  QSUB04-GMN-AGREEMENTFL       PIC X(1).
                   07  QSUB04-GMN-AGREEMENTMEI      PIC X(20).
               05  QSUB04-GMN-TSTPT-G.
                   07  QSUB04-GMN-TSTPT             PIC X(1).
                   07  QSUB04-GMN-TSTPTFL           PIC X(1).
                   07  QSUB04-GMN-TSTPTMEI          PIC X(30).
               05  QSUB04-GMN-NAME-G.
                   07  QSUB04-GMN-NAME              PIC X(100)  OCCURS 5.
               05  QSUB04-GMN-ADDRESS-G.
                   07  QSUB04-GMN-JYUSHO            PIC X(100)  OCCURS 5.
               05  QSUB04-GMN-TEL-TO-G.
                   07  QSUB04-GMN-TEL-TO            PIC X(1).
                   07  QSUB04-GMN-TEL-TOFL          PIC X(1).
                   07  QSUB04-GMN-TEL-TOMEI         PIC X(18).
               05  QSUB04-GMN-TEL-G.
                   07  QSUB04-GMN-TEL               PIC X(15)   OCCURS 3.
               05  QSUB04-GMN-CONDITION1-G.
                   07  QSUB04-GMN-CONDITION1-OCC    OCCURS 3.
                       09  QSUB04-GMN-CONDITION1    PIC X(2).
                       09  QSUB04-GMN-CONDITION1FL  PIC X(1).
                       09  QSUB04-GMN-CONDITION1MEI PIC X(20).
               05  QSUB04-GMN-CONDITION2-G.
                   07  QSUB04-GMN-CONDITION2-OCC    OCCURS 3.
                       09  QSUB04-GMN-CONDITION2    PIC X(2).
                       09  QSUB04-GMN-CONDITION2FL  PIC X(1).
                       09  QSUB04-GMN-CONDITION2MEI PIC X(20).
               05  QSUB04-GMN-CONDITION3-G.
                   07  QSUB04-GMN-CONDITION3-OCC    OCCURS 3.
                       09  QSUB04-GMN-CONDITION3    PIC X(2).
                       09  QSUB04-GMN-CONDITION3FL  PIC X(1).
                       09  QSUB04-GMN-CONDITION3MEI PIC X(20).
               05  QSUB04-GMN-SELCOMMENT-G.
                   07  QSUB04-GMN-SELCOMMENT        PIC X(1).
                   07  QSUB04-GMN-SELCOMMENTFL      PIC X(1).
                   07  QSUB04-GMN-SELCOMMENTMEI     PIC X(38).
               05  QSUB04-GMN-COMMENT-G.
                   07  QSUB04-GMN-COMMENT           PIC X(40)   OCCURS 3.
               05  QSUB04-GMN-GENMEN-G.
                   07  QSUB04-GMN-GENMEN-OCC        OCCURS 3.
                       09  QSUB04-GMN-GENMEN        PIC X(2).
                       09  QSUB04-GMN-GENMENFL      PIC X(1).
                       09  QSUB04-GMN-GENMENMEI     PIC X(20).
               05  QSUB04-GMN-TSY-G.
                   07  QSUB04-GMN-TSY               PIC X(1).
                   07  QSUB04-GMN-TSYFL             PIC X(1).
                   07  QSUB04-GMN-TSYMEI            PIC X(28).
               05  QSUB04-GMN-TSY-SKBT-G.
                   07  QSUB04-GMN-TSY-SKBT          PIC X(1).
                   07  QSUB04-GMN-TSY-SKBTFL        PIC X(1).
                   07  QSUB04-GMN-TSY-SKBTMEI       PIC X(8).
               05  QSUB04-GMN-TSY-TEKYMD-G.
                   07  QSUB04-GMN-TSY-TEKYMD        PIC X(10).
                   07  QSUB04-GMN-TSY-TEKYM         PIC X(07).
                   07  QSUB04-GMN-TSY-TEKYMD-FIRST  PIC X(10).
                   07  QSUB04-GMN-TSY-TEKYMD-LAST   PIC X(10).
                   07  QSUB04-NAI-TSY-TEKYMD        PIC X(08).
               05  QSUB04-GMN-TOKKICD-G.
                   07  QSUB04-GMN-TOKKICD           PIC X(2).
                   07  QSUB04-GMN-TOKKICDFL         PIC X(1).
                   07  QSUB04-GMN-TOKKICDMEI        PIC X(30).
               05  QSUB04-GMN-TOKKINAME             PIC X(12).
               05  QSUB04-GMN-TOKKIINFO-G.
                   07  QSUB04-GMN-TOKKIINFO         PIC X(2).
                   07  QSUB04-GMN-TOKKIINFOFL       PIC X(1).
                   07  QSUB04-GMN-TOKKIINFOMEI      PIC X(30).
               05  QSUB04-GMN-TENSU-G.
                   07  QSUB04-GMN-TENSU-FIRST       PIC X(10).
                   07  QSUB04-GMN-TENSU-LAST        PIC X(10).
               05  QSUB04-GMN-SAIGAIKBN-G.
                   07  QSUB04-GMN-SAIGAIKBN         PIC X(1).
                   07  QSUB04-GMN-SAIGAIKBNFL       PIC X(1).
                   07  QSUB04-GMN-SAIGAIKBNMEI      PIC X(20).
               05  QSUB04-GMN-TENKENRECE-G
                   07  QSUB04-TENKENRECE            PIC X(1).
                   07  QSUB04-TENKENRECEFL          PIC X(1).
                   07  QSUB04-TENKENRECEMEI         PIC X(20).
           03  QSUB04-GMN-HOKEN-AREA.
               05  QSUB04-GMN-HKNEDYM               PIC X(10).
               05  QSUB04-NAI-HKNEDYM               PIC X(06).
               05  QSUB04-GMN-HONKZKKBN             PIC X(1).
               05  QSUB04-GMN-FTNRATE               PIC X(1).
               05  QSUB04-GMN-HKNJANUM-G.
                   07  QSUB04-GMN-HKNJANUM-LTYPE    PIC X(1).
                   07  QSUB04-GMN-HKNJANUM          PIC X(8)    OCCURS  5.
               05  QSUB04-GMN-HKNNUM-G.
                   07  QSUB04-GMN-HKNNUM-LTYPE      PIC X(1).
                   07  QSUB04-GMN-HKNNUM            PIC X(3)    OCCURS  5.
               05  QSUB04-GMN-KIGO-G.
                   07  QSUB04-GMN-KIGO              PIC X(40)   OCCURS  5.
               05  QSUB04-GMN-NUM-G.
                   07  QSUB04-GMN-NUM               PIC X(40)   OCCURS  5.
           03  QSUB04-GMN-KOHI-AREA.
               05  QSUB04-GMN-KOHEDYM               PIC X(10).
               05  QSUB04-NAI-KOHEDYM               PIC X(06).
               05  QSUB04-GMN-KOHNUM-G.
                   07  QSUB04-GMN-KOHNUM-LTYPE      PIC X(1).
                   07  QSUB04-GMN-KOHNUM-OCC        OCCURS  5.
                       09  QSUB04-GMN-KOHNUM        PIC X(3).
                       09  QSUB04-GMN-PAYKBN        PIC X(1).
               05  QSUB04-GMN-FTNJANUM-G.
                   07  QSUB04-GMN-FTNJANUM          PIC X(8)    OCCURS  5.
           03  QSUB04-GMN-HOKEN-KOHI-LTYPE          PIC X(1).
           03  QSUB04-GMN-BYOMEI-AREA.
               05  QSUB04-GMN-STSRYYMD-G.
                   07  QSUB04-GMN-STSRYYMD-FIRST    PIC X(10).
                   07  QSUB04-GMN-STSRYYMD-LAST     PIC X(10).
                   07  QSUB04-NAI-STSRYYMD-FIRST    PIC X(08).
                   07  QSUB04-NAI-STSRYYMD-LAST     PIC X(08).
               05  QSUB04-GMN-TENKIKBN              PIC X(1).
               05  QSUB04-GMN-TENKIYMD-G.
                   07  QSUB04-GMN-TENKIYMD-FIRST    PIC X(10).
                   07  QSUB04-GMN-TENKIYMD-LAST     PIC X(10).
                   07  QSUB04-NAI-TENKIYMD-FIRST    PIC X(08).
                   07  QSUB04-NAI-TENKIYMD-LAST     PIC X(08).
               05  QSUB04-GMN-UTAGAIKBN             PIC X(1).
               05  QSUB04-GMN-KYUSEIKBN             PIC X(1).
               05  QSUB04-GMN-SYUBYOKBN             PIC X(1).
               05  QSUB04-GMN-BYOMEI-G.
                   07  QSUB04-GMN-BYOMEI-LTYPE      PIC X(1).
                   07  QSUB04-GMN-BYOMEI-OCC        OCCURS  5.
                       09  QSUB04-GMN-BYOMEI        PIC X(80).
                       09  QSUB04-GMN-COMPKBN       PIC X(1).
           03  QSUB04-GMN-SRYACT-AREA.
               05  QSUB04-GMN-SRYKBN                PIC X(2).
               05  QSUB04-GMN-SRYSBTKBN             PIC X(2).
               05  QSUB04-GMN-HOKATSUKBN            PIC X(1).
               05  QSUB04-GMN-SRYVALKBN             PIC X(1).
               05  QSUB04-GMN-SRYACT-LTYPE          PIC X(1).
               05  QSUB04-GMN-SRYACT-G.
                   07  QSUB04-GMN-SRYACT-OCC        OCCURS  5.
                       09  QSUB04-GMN-SRYCD         PIC X(20).
                       09  QSUB04-GMN-SRYCD-NOT     PIC X(01).
                       09  QSUB04-GMN-SRYVAL        PIC X(11).
                       09  QSUB04-GMN-SRYVALCOMPKBN PIC X(01).
           03  QSUB04-GMN-KAIGO-AREA.
               05  QSUB04-GMN-KH-SKBT               PIC X(1).
               05  QSUB04-GMN-KH-TEKYMD-G.
                   07  QSUB04-GMN-KH-TEKYMD         PIC X(10).
                   07  QSUB04-GMN-KH-TEKYM          PIC X(07).
                   07  QSUB04-GMN-KH-TEKYMD-FIRST   PIC X(10).
                   07  QSUB04-GMN-KH-TEKYMD-LAST    PIC X(10).
                   07  QSUB04-NAI-KH-TEKYMD         PIC X(08).
               05  QSUB04-GMN-KH-HKNJANUM           PIC X(06).
               05  QSUB04-GMN-KN-SKBT               PIC X(1).
               05  QSUB04-GMN-KN-TEKYMD-G.
                   07  QSUB04-GMN-KN-TEKYMD         PIC X(10).
                   07  QSUB04-GMN-KN-TEKYM          PIC X(07).
                   07  QSUB04-GMN-KN-TEKYMD-FIRST   PIC X(10).
                   07  QSUB04-GMN-KN-TEKYMD-LAST    PIC X(10).
                   07  QSUB04-NAI-KN-TEKYMD         PIC X(08).
                   07  QSUB04-GMN-KN-JOTAIKBN       PIC X(02).
           03  QSUB04-GMN-SORT-AREA.
               05  QSUB04-GMN-SORT-KEY-G.
                   07  QSUB04-GMN-SORT-KEY-OCC      OCCURS  3.
                       09  QSUB04-GMN-SORT-KEY-OCC2 OCCURS  3.
                           11  QSUB04-GMN-SORT-ITEM     PIC X(1).
                           11  QSUB04-GMN-SORT-ORDER    PIC X(1).
           03  QSUB04-GMN-ERRCD                         PIC X(04).
