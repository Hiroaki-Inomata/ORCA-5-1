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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBGS139.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 新保険別請求チェック表 総括
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/01/13    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求ファイル
           SELECT  BGSSKY-FILE     ASSIGN  ASS-BGSSKY
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSSKY-KEY
                                   FILE    STATUS  IS  STS-BGSSKY.
      *
      *    公費ファイル
           SELECT  BGSKOH-FILE     ASSIGN  ASS-BGSKOH
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSKOH-KEY
                                   FILE    STATUS  IS  STS-BGSKOH.
      *
      *    その他請求ファイル
           SELECT  BGSETC-FILE     ASSIGN  ASS-BGSETC
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSETC-KEY
                                   FILE    STATUS  IS  STS-BGSETC.
      *
      *    エラーファイル
           SELECT  BGS100ERR-FILE  ASSIGN  ASS-BGS100ERR
                                   FILE    STATUS  IS  STS-BGS100ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求ファイル
       FD  BGSSKY-FILE.
       01  BGSSKY-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSSKY-//.
      *
      *    公費ファイル
       FD  BGSKOH-FILE.
       01  BGSKOH-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSKOH-//.
      *
      *    その他請求ファイル
       FD  BGSETC-FILE.
       01  BGSETC-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSETC-//.
      *
      *    エラーファイル
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY  "HCGS139NV02.INC" REPLACING  //HCGS139NV02//
                                   BY         //HCGS139N//.
           COPY  "HCGS139GV02.INC" REPLACING  //HCGS139GV02//
                                   BY         //HCGS139G//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSSKY//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSKOH//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSETC//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGS100ERR//.
      *
       01  STS-AREA.
           03  STS-BGSSKY          PIC X(02).
           03  STS-BGSKOH          PIC X(02).
           03  STS-BGSETC          PIC X(02).
           03  STS-BGS100ERR       PIC X(02).
      *
       01  CNT-AREA.
           03  CNT-NYUIN           PIC 9(05).
           03  CNT-GAIRAI          PIC 9(05).
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-KENSU           PIC 9(05).
           03  CNT-LEN             PIC 9(05).
           03  CNT-CSV             PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-BGSSKY          PIC 9(01).
           03  FLG-BGSKOH          PIC 9(01).
           03  FLG-BGSETC          PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-NG              PIC 9(05).
           03  IDX-HKN             PIC 9(05).
           03  IDX-TTL             PIC 9(05).
           03  IDX-KOH             PIC 9(05).
           03  IDX-BASE            PIC 9(05).
           03  IDXP                PIC 9(05).
           03  IDX-RECEKA          PIC 9(05).
           03  IDXI                PIC 9(05).
           03  IDXL                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-ITEM-SIZE       PIC 9(05).
           03  WRK-ITEM            PIC X(100).
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-Z3              PIC Z(3).
           03  WRK-Z4              PIC Z(4).
           03  WRK-Z5              PIC Z(5).
           03  WRK-Z9              PIC Z(9).
           03  WRK-M9              PIC -(9).
           03  WRK-TTLID           PIC 9(5).
           03  WRK-CSV-SRYYM       PIC X(10).
           03  WRK-PRT-SRYYM       PIC X(16).
           03  WRK-PRT-SAKYMD      PIC X(22).
           03  WRK-KANACHK-STR     PIC X(500).
           03  WRK-SRYKANAME1      PIC X(24).
           03  WRK-SKYSBTNAME      PIC X(50).
           03  WRK-MAX             PIC 9(05).
           03  WRK-CSVDATA         PIC X(20000).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-NYUGAIKBNMEI    PIC X(20).
           03  WRK-PRTKBN          PIC X(01).
           03  WRK-PRTKBNMEI       PIC X(10).
           03  WRK-DATAKBN         PIC X(01).
           03  WRK-DATAKBNMEI      PIC X(50).
      *
       01  CSV-HEAD-AREA.
           03  CSV-H01-G.
               05  CSV-H01-VAL.
                   07  CSV-H01-PRTKBN          PIC X(50)
                           VALUE   "帳票区分".
                   07  CSV-H01-PRTKBNMEI       PIC X(50)
                           VALUE   "帳票区分名".
                   07  CSV-H01-DATAKBN         PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H01-DATAKBNMEI      PIC X(50)
                           VALUE   "データ区分名".
                   07  CSV-H01-SRYYMD          PIC X(50)
                           VALUE   "診療年月".
                   07  CSV-H01-NYUGAIKBN       PIC X(50)
                           VALUE   "入外区分".
                   07  CSV-H01-NYUGAIKBNMEI    PIC X(50)
                           VALUE   "入外区分名称".
                   07  CSV-H01-SKYSBTKBN       PIC X(50)
                           VALUE   "当月・当月外区分".
                   07  CSV-H01-SKYSBT          PIC X(50)
                           VALUE   "当月・当月外区分名称".
                   07  CSV-H01-SRYKA           PIC X(50)
                           VALUE   "診療科コード".
                   07  CSV-H01-SRYKAMEI        PIC X(50)
                           VALUE   "診療科名称".
                   07  CSV-H01-HKNKBN          PIC X(50)
                           VALUE   "保険区分".
                   07  CSV-H01-KENSU           PIC X(50)
                           VALUE   "件数".
                   07  CSV-H01-NISSU           PIC X(50)
                           VALUE   "日数".
                   07  CSV-H01-TENSU           PIC X(50)
                           VALUE   "点数".
                   07  CSV-H01-FTNMONEY        PIC X(50)
                           VALUE   "患者負担額".
                   07  CSV-H01-HKNMONEY        PIC X(50)
                           VALUE   "保険請求額".
                   07  CSV-H01-KOHMONEY        PIC X(50)
                           VALUE   "公費請求額".
                   07  CSV-H01-TTLMONEY        PIC X(50)
                           VALUE   "保険診療収益（ａ）".
                   07  CSV-H01-SKJKAISU        PIC X(50)
                           VALUE   "食事・生活回数".
                   07  CSV-H01-SKJTTLMONEY     PIC X(50)
                           VALUE   "食事・生活標準負担額".
                   07  CSV-H01-SKJHKNMONEY     PIC X(50)
                           VALUE   "食事・生活保険請求額".
                   07  CSV-H01-SKJKOHMONEY     PIC X(50)
                           VALUE   "食事・生活公費請求額".
                   07  CSV-H01-SKJFTNMONEY     PIC X(50)
                           VALUE   "食事・生活請求円（ｂ）".
                   07  CSV-H01-TTLTTLMONEY     PIC X(50)
                           VALUE   "（ａ）＋（ｂ）".
               05  CSV-H01-R   REDEFINES   CSV-H01-VAL.
                   07  CSV-H01-ITEM            PIC X(50)
                                               OCCURS  25.
               05  CSV-H01-MAX                 PIC 9(05)
                                               VALUE   25.
           03  CSV-H02-G.
               05  CSV-H02-VAL.
                   07  CSV-H02-PRTKBN          PIC X(50)
                           VALUE   "帳票区分".
                   07  CSV-H02-PRTKBNMEI       PIC X(50)
                           VALUE   "帳票区分名".
                   07  CSV-H02-DATAKBN         PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H02-DATAKBNMEI      PIC X(50)
                           VALUE   "データ区分名".
                   07  CSV-H02-SRYYMD          PIC X(50)
                           VALUE   "診療年月".
                   07  CSV-H02-NYUGAIKBN       PIC X(50)
                           VALUE   "入外区分".
                   07  CSV-H02-NYUGAIKBNMEI    PIC X(50)
                           VALUE   "入外区分名称".
                   07  CSV-H02-SKYSBTKBN       PIC X(50)
                           VALUE   "当月・当月外区分".
                   07  CSV-H02-SKYSBT          PIC X(50)
                           VALUE   "当月・当月外区分名称".
                   07  CSV-H02-SRYKA           PIC X(50)
                           VALUE   "診療科コード".
                   07  CSV-H02-SRYKAMEI        PIC X(50)
                           VALUE   "診療科名称".
                   07  CSV-H02-KOHNAME         PIC X(50)
                           VALUE   "公費件数計".
                   07  CSV-H02-KENSU           PIC X(50)
                           VALUE   "件数".
               05  CSV-H02-R   REDEFINES   CSV-H02-VAL.
                   07  CSV-H02-ITEM            PIC X(50)
                                               OCCURS  13.
               05  CSV-H02-MAX                 PIC 9(05)
                                               VALUE   13.
           03  CSV-H03-G.
               05  CSV-H03-VAL.
                   07  CSV-H03-PRTKBN          PIC X(50)
                           VALUE   "帳票区分".
                   07  CSV-H03-PRTKBNMEI       PIC X(50)
                           VALUE   "帳票区分名".
                   07  CSV-H03-DATAKBN         PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H03-DATAKBNMEI      PIC X(50)
                           VALUE   "データ区分名".
                   07  CSV-H03-SRYYMD          PIC X(50)
                           VALUE   "診療年月".
                   07  CSV-H03-NYUGAIKBN       PIC X(50)
                           VALUE   "入外区分".
                   07  CSV-H03-NYUGAIKBNMEI    PIC X(50)
                           VALUE   "入外区分名称".
                   07  CSV-H03-SKYSBTKBN       PIC X(50)
                           VALUE   "当月・当月外区分".
                   07  CSV-H03-SKYSBT          PIC X(50)
                           VALUE   "当月・当月外区分名称".
                   07  CSV-H03-SRYKA           PIC X(50)
                           VALUE   "診療科コード".
                   07  CSV-H03-SRYKAMEI        PIC X(50)
                           VALUE   "診療科名称".
                   07  CSV-H03-ETCNAME         PIC X(50)
                           VALUE   "その他計".
                   07  CSV-H03-JIHIMONEY       PIC X(50)
                           VALUE   "自費".
                   07  CSV-H03-GENMEN          PIC X(50)
                           VALUE   "減免".
                   07  CSV-H03-CHOSEI1         PIC X(50)
                           VALUE   "調整金１".
                   07  CSV-H03-CHOSEI2         PIC X(50)
                           VALUE   "調整金２".
                   07  CSV-H03-HASU            PIC X(50)
                           VALUE   "窓口請求端数処理額".
               05  CSV-H03-R   REDEFINES   CSV-H03-VAL.
                   07  CSV-H03-ITEM            PIC X(50)
                                               OCCURS  17.
               05  CSV-H03-MAX                 PIC 9(05)
                                               VALUE   17.
           03  CSV-H04-G.
               05  CSV-H04-VAL.
                   07  CSV-H04-PRTKBN          PIC X(50)
                           VALUE   "帳票区分".
                   07  CSV-H04-PRTKBNMEI       PIC X(50)
                           VALUE   "帳票区分名".
                   07  CSV-H04-DATAKBN         PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H04-DATAKBNMEI      PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H04-SRYYMD          PIC X(50)
                           VALUE   "診療年月".
                   07  CSV-H04-NYUGAIKBN       PIC X(50)
                           VALUE   "入外区分".
                   07  CSV-H04-NYUGAIKBNMEI    PIC X(50)
                           VALUE   "入外区分名称".
                   07  CSV-H04-SKYSBTKBN       PIC X(50)
                           VALUE   "当月・当月外区分".
                   07  CSV-H04-SKYSBT          PIC X(50)
                           VALUE   "当月・当月外区分名称".
                   07  CSV-H04-SRYKA           PIC X(50)
                           VALUE   "診療科コード".
                   07  CSV-H04-SRYKAMEI        PIC X(50)
                           VALUE   "診療科名称".
                   07  CSV-H04-HKNKBN          PIC X(50)
                           VALUE   "保険区分".
                   07  CSV-H04-KENSU           PIC X(50)
                           VALUE   "件数".
                   07  CSV-H04-NISSU           PIC X(50)
                           VALUE   "日数".
                   07  CSV-H04-TENSU           PIC X(50)
                           VALUE   "点数".
                   07  CSV-H04-FTNMONEY        PIC X(50)
                           VALUE   "患者負担額".
                   07  CSV-H04-HKNMONEY        PIC X(50)
                           VALUE   "保険請求額".
                   07  CSV-H04-KOHMONEY        PIC X(50)
                           VALUE   "公費請求額".
                   07  CSV-H04-TTLMONEY        PIC X(50)
                           VALUE   "保険診療収益".
               05  CSV-H04-R   REDEFINES   CSV-H04-VAL.
                   07  CSV-H04-ITEM            PIC X(50)
                                               OCCURS  19.
               05  CSV-H04-MAX                 PIC 9(05)
                                               VALUE   19.
      *
       01  KEY-AREA.
           03  KEY-SKYSBT          PIC X(01).
           03  KEY-RECEKA          PIC X(02).
      *
       01  THKN-AREA.
           03  THKN-NYUGAI-OCC     OCCURS  3.
               05  THKN-BODY-OCC   OCCURS  100.
                   07  THKN-KENSU          PIC 9(06).
                   07  THKN-NISSU          PIC 9(08).
                   07  THKN-TENSU          PIC 9(08).
                   07  THKN-FTNMONEY       PIC 9(09).
                   07  THKN-HKNMONEY       PIC 9(09).
                   07  THKN-KOHMONEY       PIC 9(09).
                   07  THKN-TTLMONEY       PIC 9(09).
                   07  THKN-SKJKAISU       PIC 9(06).
                   07  THKN-SKJTTLMONEY    PIC 9(09).
                   07  THKN-SKJFTNMONEY    PIC 9(09).
                   07  THKN-SKJHKNMONEY    PIC 9(09).
                   07  THKN-SKJKOHMONEY    PIC 9(09).
                   07  THKN-TTLTTLMONEY    PIC 9(09).
                   07  THKN-JIHIMONEY      PIC S9(08).
                   07  THKN-GENMEN         PIC S9(08).
                   07  THKN-CHOSEI1        PIC S9(08).
                   07  THKN-CHOSEI2        PIC S9(08).
                   07  THKN-HASU           PIC S9(09).
      *
       01  TKOH-AREA.
           03  TKOH-NYUGAI-OCC             OCCURS  3.
               05  TKOH-KENSU              PIC 9(06).
      *
       01  KEY-AERA.
           03  KEY-NYUGAIKBN               PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-H2605         PIC X(06)   VALUE "201405".
      *
           03  CONST-SYAHO         PIC 9(02) VALUE 1.
           03  CONST-KOKUHO        PIC 9(02) VALUE 2.
           03  CONST-KOUKI         PIC 9(02) VALUE 3.
           03  CONST-TOKURYO       PIC 9(02) VALUE 4.
           03  CONST-JIHIHKN       PIC 9(02) VALUE 5.
           03  CONST-CHIKEN        PIC 9(02) VALUE 6.
           03  CONST-ROUSAI        PIC 9(02) VALUE 7.
           03  CONST-JIBAI         PIC 9(02) VALUE 8.
           03  CONST-DAISAN        PIC 9(02) VALUE 9.
           03  CONST-KOUGAI        PIC 9(02) VALUE 10.
           03  CONST-SYAKOU9HEI    PIC 9(03) VALUE 1.
           03  CONST-SYAKOU9TAN    PIC 9(03) VALUE 2.
           03  CONST-SYAKOU7HEI    PIC 9(03) VALUE 3.
           03  CONST-SYAKOU7TAN    PIC 9(03) VALUE 4.
           03  CONST-SYAHONHEI     PIC 9(03) VALUE 5.
           03  CONST-SYAHONTAN     PIC 9(03) VALUE 6.
           03  CONST-SYAKZKHEI     PIC 9(03) VALUE 7.
           03  CONST-SYAKZKTAN     PIC 9(03) VALUE 8.
           03  CONST-SYAMISGKHEI   PIC 9(03) VALUE 9.
           03  CONST-SYAMISGKTAN   PIC 9(03) VALUE 10.
           03  CONST-SYAKOH        PIC 9(03) VALUE 11.
           03  CONST-SYATTL        PIC 9(03) VALUE 12.
           03  CONST-KOKIPNKOU9    PIC 9(03) VALUE 13.
           03  CONST-KOKIPNKOU7    PIC 9(03) VALUE 14.
           03  CONST-KOKIPNHON     PIC 9(03) VALUE 15.
           03  CONST-KOKIPNMISGK   PIC 9(03) VALUE 16.
           03  CONST-KOKTAIHON     PIC 9(03) VALUE 17.
           03  CONST-KOKTAIKZK     PIC 9(03) VALUE 18.
           03  CONST-KOKTAIMISGK   PIC 9(03) VALUE 19.
           03  CONST-KOKTTL        PIC 9(03) VALUE 20.
           03  CONST-KOUKOU9       PIC 9(03) VALUE 21.
           03  CONST-KOUKOU7       PIC 9(03) VALUE 22.
           03  CONST-KOUTTL        PIC 9(03) VALUE 23.
           03  CONST-HKNTTL        PIC 9(03) VALUE 24.
           03  CONST-TOKTTL        PIC 9(03) VALUE 25.
           03  CONST-JIHITTL       PIC 9(03) VALUE 26.
           03  CONST-CHIKENTTL     PIC 9(03) VALUE 27.
           03  CONST-ROUSAITTL     PIC 9(03) VALUE 28.
           03  CONST-JIBAITTL      PIC 9(03) VALUE 29.
           03  CONST-DAISANTTL     PIC 9(03) VALUE 30.
           03  CONST-KOUGAITTL     PIC 9(03) VALUE 31.
           03  CONST-ALLTTL        PIC 9(03) VALUE 32.
           03  CONST-THKN-MAX      PIC 9(03) VALUE 32.
      *
           03  CONST-GRP-SYA-KOU9HEI   PIC 9(02)   VALUE 1.
           03  CONST-GRP-SYA-KOU9TAN   PIC 9(02)   VALUE 2.
           03  CONST-GRP-SYA-KOU7HEI   PIC 9(02)   VALUE 3.
           03  CONST-GRP-SYA-KOU7TAN   PIC 9(02)   VALUE 4.
           03  CONST-GRP-SYA-HONHEI    PIC 9(02)   VALUE 5.
           03  CONST-GRP-SYA-HONTAN    PIC 9(02)   VALUE 6.
           03  CONST-GRP-SYA-KZKHEI    PIC 9(02)   VALUE 7.
           03  CONST-GRP-SYA-KZKTAN    PIC 9(02)   VALUE 8.
           03  CONST-GRP-SYA-MISGKHEI  PIC 9(02)   VALUE 9.
           03  CONST-GRP-SYA-MISGKTAN  PIC 9(02)   VALUE 10.
           03  CONST-GRP-SYA-KOHTAN    PIC 9(02)   VALUE 11.
           03  CONST-GRP-SYA-KOH2HEI   PIC 9(02)   VALUE 12.
           03  CONST-GRP-SYA-KOH3HEI   PIC 9(02)   VALUE 13.
      *
           03  CONST-GRP-KOK-IPNKOU9   PIC 9(02)   VALUE 1.
           03  CONST-GRP-KOK-IPNKOU7   PIC 9(02)   VALUE 2.
           03  CONST-GRP-KOK-IPN       PIC 9(02)   VALUE 3.
           03  CONST-GRP-KOK-IPNMISGK  PIC 9(02)   VALUE 4.
           03  CONST-GRP-KOK-TAI       PIC 9(02)   VALUE 5.
           03  CONST-GRP-KOK-TAIKZK    PIC 9(02)   VALUE 6.
           03  CONST-GRP-KOK-TAIMISGK  PIC 9(02)   VALUE 7.
      *
           03  CONST-GRP-KOU-KOU9      PIC 9(02)   VALUE 1.
           03  CONST-GRP-KOU-KOU7      PIC 9(02)   VALUE 2.
      *
           03  CONST-HKNMEI-G.
               05  CONST-HKNMEI01  PIC X(50)
                                   VALUE "社保高９公費併用".
               05  CONST-HKNMEI02  PIC X(50)
                                   VALUE "社保高９単独".
               05  CONST-HKNMEI03  PIC X(50)
                                   VALUE "社保高７公費併用".
               05  CONST-HKNMEI04  PIC X(50)
                                   VALUE "社保高７単独".
               05  CONST-HKNMEI05  PIC X(50)
                                   VALUE "社保本人公費併用".
               05  CONST-HKNMEI06  PIC X(50)
                                   VALUE "社保本人単独".
               05  CONST-HKNMEI07  PIC X(50)
                                   VALUE "社保家族公費併用".
               05  CONST-HKNMEI08  PIC X(50)
                                   VALUE "社保家族単独".
               05  CONST-HKNMEI09  PIC X(50)
                                   VALUE "社保６歳公費併用".
               05  CONST-HKNMEI10  PIC X(50)
                                   VALUE "社保６歳単独".
               05  CONST-HKNMEI11  PIC X(50)
                                   VALUE "社保公費単独・併用".
               05  CONST-HKNMEI12  PIC X(50)
                                   VALUE "社保計".
               05  CONST-HKNMEI13  PIC X(50)
                                   VALUE "国保一般高９".
               05  CONST-HKNMEI14  PIC X(50)
                                   VALUE "国保一般高７".
               05  CONST-HKNMEI15  PIC X(50)
                                   VALUE "国保一般".
               05  CONST-HKNMEI16  PIC X(50)
                                   VALUE "国保６歳".
               05  CONST-HKNMEI17  PIC X(50)
                                   VALUE "国保退職本人".
               05  CONST-HKNMEI18  PIC X(50)
                                   VALUE "国保退職家族".
               05  CONST-HKNMEI19  PIC X(50)
                                   VALUE "国保退職６歳".
               05  CONST-HKNMEI20  PIC X(50)
                                   VALUE "国保計".
               05  CONST-HKNMEI21  PIC X(50)
                                   VALUE "後期高齢者高９".
               05  CONST-HKNMEI22  PIC X(50)
                                   VALUE "後期高齢者高７".
               05  CONST-HKNMEI23  PIC X(50)
                                   VALUE "後期高齢者計".
               05  CONST-HKNMEI24  PIC X(50)
                                   VALUE "保険合計（社＋国＋後）".
               05  CONST-HKNMEI25  PIC X(50)
                                   VALUE "特別療養費計".
               05  CONST-HKNMEI26  PIC X(50)
                                   VALUE "自費計".
               05  CONST-HKNMEI27  PIC X(50)
                                   VALUE "治験計".
               05  CONST-HKNMEI28  PIC X(50)
                                   VALUE "労災計".
               05  CONST-HKNMEI29  PIC X(50)
                                   VALUE "自賠責計".
               05  CONST-HKNMEI30  PIC X(50)
                                   VALUE "自賠責（第三者行為）計".
               05  CONST-HKNMEI31  PIC X(50)
                                   VALUE "公害計".
               05  CONST-HKNMEI32  PIC X(50)
                                   VALUE "合計".
           03  CONST-HKNMEI-R  REDEFINES   CONST-HKNMEI-G.
               05  CONST-HKNMEI-OCC    OCCURS 32.
                   07  CONST-HKNMEI    PIC X(50).
      *
      *    集計領域
       01  TTLHKN-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //TTLHKN-//.
      *
       01  BGSF01-REC.
           COPY    "CPBGSF01.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  BGS100P-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBGS100PARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       BGS100P-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      STS-AREA
                                           CNT-AREA
                                           FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
      *
           MOVE    BGS100P-SKYFNAME    TO  ASS-BGSSKY
           MOVE    BGS100P-KOHFNAME    TO  ASS-BGSKOH
           MOVE    BGS100P-ETCFNAME    TO  ASS-BGSETC
           MOVE    BGS100P-ERRFNAME    TO  ASS-BGS100ERR
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "総括表作成中"
                                       TO  JOB-YOBI
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-PRT-SAKYMD
      *
           MOVE    BGS100P-SRYYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3(1:16)   TO  WRK-PRT-SRYYM
      *
           MOVE    BGS100P-SRYYM(1:4)  TO  WRK-CSV-SRYYM(1:4)
           MOVE    "/"                 TO  WRK-CSV-SRYYM(5:1)
           MOVE    BGS100P-SRYYM(5:2)  TO  WRK-CSV-SRYYM(6:2)
      *
           MOVE    SPACE               TO  WRK-ITEM
           INSPECT WRK-ITEM  TALLYING  WRK-ITEM-SIZE   FOR ALL " "
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( BGS100P-EDITKBN =   "1" )
               MOVE    "1"         TO  KEY-SKYSBT
               PERFORM 201-MAIN-SEC
               MOVE    "2"         TO  KEY-SKYSBT
               PERFORM 201-MAIN-SEC
           ELSE
               MOVE    SPACE       TO  KEY-SKYSBT
               PERFORM 201-MAIN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       201-MAIN-SEC                SECTION.
      *
           IF    ( BGS100P-SYUKA   =   1 )
               PERFORM VARYING IDX-RECEKA  FROM    1   BY  1
                       UNTIL ( IDX-RECEKA  >   BGS100P-RECEKA-MAX )
      *
                   MOVE    BGS100P-RECEKA (IDX-RECEKA)
                                   TO  KEY-RECEKA
      *
                   PERFORM 2001-MAIN-SEC
               END-PERFORM
               MOVE    SPACE       TO  KEY-RECEKA
               PERFORM 2001-MAIN-SEC
           ELSE
               MOVE    SPACE       TO  KEY-RECEKA
               PERFORM 2001-MAIN-SEC
           END-IF
      *
           .
       201-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       2001-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO      CNT-NYUIN
                                           CNT-GAIRAI
      *
           INITIALIZE                      THKN-AREA
                                           TKOH-AREA
      *
           MOVE    SPACE           TO      WRK-SKYSBTNAME
           IF    ( BGS100P-EDITKBN =   "1" )
               EVALUATE    KEY-SKYSBT
               WHEN    "1"
                   MOVE  "当月診療分"  TO      WRK-SKYSBTNAME
               WHEN    "2"
                   MOVE  "当月診療分以外（月遅れ請求、返戻分再請求）"
                                       TO      WRK-SKYSBTNAME
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO      WRK-SRYKANAME1
           IF    ( BGS100P-SYUKA   =   1 )
               EVALUATE    KEY-RECEKA
               WHEN    SPACE
                   MOVE    "全科合計"  TO  WRK-SRYKANAME1
               WHEN    "00"
                   MOVE    "主科設定なし"
                                       TO  WRK-SRYKANAME1
               WHEN    OTHER
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    KEY-RECEKA      TO  SYS-KBNCD
                   MOVE    BGS100P-SRYYMD  TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1
                                       TO  WRK-SRYKANAME1
               END-EVALUATE
           END-IF
      *
           PERFORM 2001-SKY-EDIT-SEC
      *
           PERFORM 2001-ETC-SKY-EDIT-SEC
      *
           PERFORM 2001-KOH-EDIT-SEC
      *
           IF    ( CNT-NYUIN   >   ZERO )
               MOVE    1       TO  IDX1
               PERFORM 2001-HCGS139N-EDIT-SEC
               PERFORM 2001-HCGS139N-CSV-SEC
           END-IF
      *
           IF    ( CNT-GAIRAI  >   ZERO )
               MOVE    2       TO  IDX1
               PERFORM 2001-HCGS139G-EDIT-SEC
               PERFORM 2001-HCGS139G-CSV-SEC
           END-IF
      *
           IF    ( CNT-NYUIN   >   ZERO )
            AND  ( CNT-GAIRAI  >   ZERO )
               MOVE    3       TO  IDX1
               PERFORM 2001-HCGS139N-EDIT-SEC
               PERFORM 2001-HCGS139N-CSV-SEC
           END-IF
      *
           .
       2001-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       2001-SKY-EDIT-SEC           SECTION.
      *
           OPEN    INPUT  BGSSKY-FILE
      *
           PERFORM 900-BGSSKY-READ-SEC
      *
           PERFORM UNTIL ( FLG-BGSSKY NOT =   ZERO )
      *
               PERFORM 20011-SKY-EDIT-SEC
      *
               PERFORM 900-BGSSKY-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BGSSKY-FILE
      *
           .
       2001-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       20011-SKY-EDIT-SEC           SECTION.
      *
           IF    ( BGSSKY-NYUGAIKBN =   "1" )
               MOVE    1           TO   IDX1
               PERFORM 200111-SKY-EDIT-SEC
           ELSE
               MOVE    2           TO   IDX1
               PERFORM 200111-SKY-EDIT-SEC
           END-IF
      *
           MOVE    3               TO   IDX1
           PERFORM 200111-SKY-EDIT-SEC
      *
           .
       20011-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報編集処理
      *****************************************************************
       200111-SKY-EDIT-SEC           SECTION.
      *
           EVALUATE    BGSSKY-HOKEN
           WHEN    CONST-SYAHO
               PERFORM 200111-SYAHO-SKY-EDIT-SEC
           WHEN    CONST-KOKUHO
               PERFORM 200111-KOKUHO-SKY-EDIT-SEC
           WHEN    CONST-KOUKI
               PERFORM 200111-KOUKI-SKY-EDIT-SEC
           WHEN    OTHER
               DISPLAY "HOKEN[" BGSSKY-HOKEN "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-HKNTTL    TO   IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           MOVE    CONST-ALLTTL    TO   IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    社保請求情報編集処理
      *****************************************************************
       200111-SYAHO-SKY-EDIT-SEC       SECTION.
      *
           EVALUATE    BGSSKY-GROUPID
      *
           WHEN    CONST-GRP-SYA-KOU9HEI
               MOVE    CONST-SYAKOU9HEI    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-KOU9TAN
               MOVE    CONST-SYAKOU9TAN    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-KOU7HEI
               MOVE    CONST-SYAKOU7HEI    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-KOU7TAN
               MOVE    CONST-SYAKOU7TAN    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-HONHEI
               MOVE    CONST-SYAHONHEI     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-HONTAN
               MOVE    CONST-SYAHONTAN     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-KZKHEI
               MOVE    CONST-SYAKZKHEI     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-KZKTAN
               MOVE    CONST-SYAKZKTAN     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-MISGKHEI
               MOVE    CONST-SYAMISGKHEI   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-MISGKTAN
               MOVE    CONST-SYAMISGKTAN   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-SYA-KOHTAN
           WHEN    CONST-GRP-SYA-KOH2HEI
           WHEN    CONST-GRP-SYA-KOH3HEI
               MOVE    CONST-SYAKOH        TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    OTHER
               DISPLAY "GROUPID[" BGSSKY-GROUPID "]" BGSSKY-REC
           END-EVALUATE
      *
               MOVE    CONST-SYATTL        TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-SYAHO-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    国保請求情報編集処理
      *****************************************************************
       200111-KOKUHO-SKY-EDIT-SEC      SECTION.
      *
           EVALUATE    BGSSKY-GROUPID
      *
           WHEN    CONST-GRP-KOK-IPNKOU9
               MOVE    CONST-KOKIPNKOU9    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOK-IPNKOU7
               MOVE    CONST-KOKIPNKOU7    TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOK-IPN
               MOVE    CONST-KOKIPNHON     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOK-IPNMISGK
               MOVE    CONST-KOKIPNMISGK   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOK-TAI
               MOVE    CONST-KOKTAIHON     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOK-TAIKZK
               MOVE    CONST-KOKTAIKZK     TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOK-TAIMISGK
               MOVE    CONST-KOKTAIMISGK   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    OTHER
               DISPLAY "GROUPID[" BGSSKY-GROUPID "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-KOKTTL            TO  IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-KOKUHO-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者請求情報編集処理
      *****************************************************************
       200111-KOUKI-SKY-EDIT-SEC       SECTION.
      *
           EVALUATE    BGSSKY-GROUPID
      *
           WHEN    CONST-GRP-KOU-KOU9
               MOVE    CONST-KOUKOU9   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    CONST-GRP-KOU-KOU7
               MOVE    CONST-KOUKOU7   TO  IDX2
               PERFORM 2001111-SKY-SUM-SEC
      *
           WHEN    OTHER
               DISPLAY "GROUPID[" BGSSKY-GROUPID "]" BGSSKY-REC
           END-EVALUATE
      *
           MOVE    CONST-KOUTTL        TO  IDX2
           PERFORM 2001111-SKY-SUM-SEC
      *
           .
       200111-KOUKI-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求情報集計処理
      *****************************************************************
       2001111-SKY-SUM-SEC         SECTION.
      *
           COMPUTE THKN-KENSU (IDX1 IDX2)
               =   THKN-KENSU (IDX1 IDX2)    +   BGSSKY-KENSU
           COMPUTE THKN-NISSU (IDX1 IDX2)
               =   THKN-NISSU (IDX1 IDX2)    +   BGSSKY-NISSU
           COMPUTE THKN-TENSU (IDX1 IDX2)
               =   THKN-TENSU (IDX1 IDX2)    +   BGSSKY-TENSU
           COMPUTE THKN-FTNMONEY (IDX1 IDX2)
               =   THKN-FTNMONEY (IDX1 IDX2)     + BGSSKY-FTNMONEY
           COMPUTE THKN-HKNMONEY (IDX1 IDX2)
               =   THKN-HKNMONEY (IDX1 IDX2)     + BGSSKY-HKNMONEY
           COMPUTE THKN-KOHMONEY (IDX1 IDX2)
               =   THKN-KOHMONEY (IDX1 IDX2)     + BGSSKY-KOHMONEY
           COMPUTE THKN-TTLMONEY (IDX1 IDX2)
               =   THKN-TTLMONEY (IDX1 IDX2)     + BGSSKY-TTLMONEY
      *
           COMPUTE THKN-SKJKAISU (IDX1 IDX2)
               =   THKN-SKJKAISU (IDX1 IDX2)     + BGSSKY-SKJKAISU
           COMPUTE THKN-SKJTTLMONEY (IDX1 IDX2)
               =   THKN-SKJTTLMONEY (IDX1 IDX2)  + BGSSKY-SKJTTLMONEY
           COMPUTE THKN-SKJFTNMONEY (IDX1 IDX2)
               =   THKN-SKJFTNMONEY (IDX1 IDX2)  + BGSSKY-SKJFTNMONEY
           COMPUTE THKN-SKJHKNMONEY (IDX1 IDX2)
               =   THKN-SKJHKNMONEY (IDX1 IDX2)  + BGSSKY-SKJHKNMONEY
           COMPUTE THKN-SKJKOHMONEY (IDX1 IDX2)
               =   THKN-SKJKOHMONEY (IDX1 IDX2)  + BGSSKY-SKJKOHMONEY
           COMPUTE THKN-TTLTTLMONEY (IDX1 IDX2)
               =   THKN-TTLTTLMONEY (IDX1 IDX2)  + BGSSKY-TTLTTLMONEY
      *
           EVALUATE    IDX1
           WHEN    1
               COMPUTE CNT-NYUIN   =   CNT-NYUIN   +   1
           WHEN    2
               COMPUTE CNT-GAIRAI  =   CNT-GAIRAI  +   1
           END-EVALUATE
      *
           .
       2001111-SKY-SUM-EXT.
           EXIT.
      *****************************************************************
      *    その他請求情報編集処理
      *****************************************************************
       2001-ETC-SKY-EDIT-SEC           SECTION.
      *
           OPEN    INPUT               BGSETC-FILE
      *
           PERFORM 900-BGSETC-READ-SEC
      *
           PERFORM UNTIL ( FLG-BGSETC  NOT =   ZERO )
      *
               IF    ( BGSETC-NYUGAIKBN    =   "1" )
                   MOVE    1           TO  IDX-NG
               ELSE
                   MOVE    2           TO  IDX-NG
               END-IF
      *
               EVALUATE    BGSETC-HOKEN
               WHEN    CONST-SYAHO
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-SYATTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3                 TO  IDX1
                   MOVE    CONST-SYATTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-KOKUHO
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-KOKTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3                 TO  IDX1
                   MOVE    CONST-KOKTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-KOUKI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-KOUTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3                 TO  IDX1
                   MOVE    CONST-KOUTTL      TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-TOKURYO
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-TOKTTL    TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-TOKTTL    TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-JIHIHKN
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-JIHITTL   TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-JIHITTL   TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-CHIKEN
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-CHIKENTTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-CHIKENTTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-ROUSAI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-ROUSAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-ROUSAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-JIBAI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-JIBAITTL  TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-JIBAITTL  TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-DAISAN
                   MOVE    IDX-NG          TO  IDX1
                   MOVE    CONST-DAISANTTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-DAISANTTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               WHEN    CONST-KOUGAI
                   MOVE    IDX-NG            TO  IDX1
                   MOVE    CONST-KOUGAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
                   MOVE    3               TO  IDX1
                   MOVE    CONST-KOUGAITTL TO  IDX2
                   PERFORM 20011-ETC-SKY-SUM-SEC
               END-EVALUATE
      *
               MOVE    IDX-NG            TO  IDX1
               MOVE    CONST-ALLTTL    TO  IDX2
               PERFORM 20011-ETC-SKY-SUM-SEC
               MOVE    3               TO  IDX1
               MOVE    CONST-ALLTTL    TO  IDX2
               PERFORM 20011-ETC-SKY-SUM-SEC
      *
               PERFORM 900-BGSETC-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BGSETC-FILE
      *
           .
       2001-ETC-SKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    その他請求情報集計処理
      *****************************************************************
       20011-ETC-SKY-SUM-SEC           SECTION.
      *
           IF    ( BGSETC-HOKEN    =   CONST-SYAHO
                                    OR CONST-KOKUHO
                                    OR CONST-KOUKI  )
               CONTINUE
           ELSE
               COMPUTE THKN-KENSU (IDX1 IDX2)
                   =   THKN-KENSU (IDX1 IDX2)
                   +   BGSETC-KENSU
      *
               IF    ( BGSETC-HOKEN    =   CONST-DAISAN )
                AND  ( IDX2            =   CONST-ALLTTL )
                   CONTINUE
               ELSE
      *
                   COMPUTE THKN-NISSU (IDX1 IDX2)
                       =   THKN-NISSU (IDX1 IDX2)
                       +   BGSETC-NISSU
      *
                   COMPUTE THKN-TENSU (IDX1 IDX2)
                       =   THKN-TENSU (IDX1 IDX2)
                       +   BGSETC-TENSU
      *
                   COMPUTE THKN-SKJKAISU (IDX1 IDX2)
                       =   THKN-SKJKAISU (IDX1 IDX2)
                       +   BGSETC-SKJKAISU
               END-IF
      *
               COMPUTE THKN-FTNMONEY (IDX1 IDX2)
                   =   THKN-FTNMONEY (IDX1 IDX2)
                   +   BGSETC-FTNMONEY
      *
               COMPUTE THKN-HKNMONEY (IDX1 IDX2)
                   =   THKN-HKNMONEY (IDX1 IDX2)
                   +   BGSETC-HKNMONEY
      *
               COMPUTE THKN-KOHMONEY (IDX1 IDX2)
                   =   THKN-KOHMONEY (IDX1 IDX2)
                   +   BGSETC-KOHMONEY
      *
               COMPUTE THKN-TTLMONEY (IDX1 IDX2)
                   =   THKN-TTLMONEY (IDX1 IDX2)
                   +   BGSETC-TTLMONEY
      *
               COMPUTE THKN-SKJTTLMONEY (IDX1 IDX2)
                   =   THKN-SKJTTLMONEY (IDX1 IDX2)
                   +   BGSETC-SKJTTLMONEY
      *
               COMPUTE THKN-SKJFTNMONEY (IDX1 IDX2)
                   =   THKN-SKJFTNMONEY (IDX1 IDX2)
                   +   BGSETC-SKJFTNMONEY
      *
               COMPUTE THKN-SKJHKNMONEY (IDX1 IDX2)
                   =   THKN-SKJHKNMONEY (IDX1 IDX2)
                   +   BGSETC-SKJHKNMONEY
      *
               COMPUTE THKN-SKJKOHMONEY (IDX1 IDX2)
                   =   THKN-SKJKOHMONEY (IDX1 IDX2)
                   +   BGSETC-SKJKOHMONEY
      *
               COMPUTE THKN-TTLTTLMONEY (IDX1 IDX2)
                   =   THKN-TTLTTLMONEY (IDX1 IDX2)
                   +   BGSETC-TTLTTLMONEY
      *
               IF    ( BGSETC-KENSU        NOT =   ZERO )
                   EVALUATE    BGSETC-NYUGAIKBN
                   WHEN    "1"
                       COMPUTE CNT-NYUIN   =   CNT-NYUIN   +   1
                   WHEN    "2"
                       COMPUTE CNT-GAIRAI  =   CNT-GAIRAI  +   1
                   END-EVALUATE
               END-IF
           END-IF
      *
               COMPUTE THKN-JIHIMONEY (IDX1 IDX2)
                   =   THKN-JIHIMONEY (IDX1 IDX2)
                   +   BGSETC-JIHIMONEY
      *
               COMPUTE THKN-GENMEN (IDX1 IDX2)
                   =   THKN-GENMEN (IDX1 IDX2)
                   +   BGSETC-GENMEN
      *
               COMPUTE THKN-CHOSEI1 (IDX1 IDX2)
                   =   THKN-CHOSEI1 (IDX1 IDX2)
                   +   BGSETC-CHOSEI1
      *
               COMPUTE THKN-CHOSEI2 (IDX1 IDX2)
                   =   THKN-CHOSEI2 (IDX1 IDX2)
                   +   BGSETC-CHOSEI2
      *
               COMPUTE THKN-HASU (IDX1 IDX2)
                   =   THKN-HASU (IDX1 IDX2)
                   +   BGSETC-HASU
      *
           .
       20011-ETC-SKY-SUM-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       2001-KOH-EDIT-SEC           SECTION.
      *
            OPEN    INPUT  BGSKOH-FILE
      *
            PERFORM 900-BGSKOH-READ-SEC
      *
            PERFORM UNTIL ( FLG-BGSKOH NOT =   ZERO )
      *
                PERFORM 20011-KOH-EDIT-SEC
      *
                PERFORM 900-BGSKOH-READ-SEC
      *
            END-PERFORM
      *
            CLOSE   BGSKOH-FILE
      *
           .
       2001-KOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       20011-KOH-EDIT-SEC          SECTION.
      *
           IF    ( BGSKOH-NYUGAIKBN    =  "1" )
               MOVE    1           TO  IDX1
           ELSE
               MOVE    2           TO  IDX1
           END-IF
      *
           COMPUTE TKOH-KENSU (IDX1)
               =   TKOH-KENSU (IDX1)   +   BGSKOH-KENSU
      *
           COMPUTE TKOH-KENSU (3)
               =   TKOH-KENSU (3)      +   BGSKOH-KENSU
      *
           .
       20011-KOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院総括表編集処理
      *****************************************************************
       2001-HCGS139N-EDIT-SEC          SECTION.
      *
           INITIALIZE                      HCGS139N
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS139N-SRYYM
           IF    ( IDX1                =   1 )
               MOVE    "入院"          TO  HCGS139N-NYUGAI
           ELSE
               MOVE    "入院・外来　総計"
                                       TO  HCGS139N-NYUGAI
           END-IF
           MOVE    WRK-PRT-SAKYMD      TO  HCGS139N-SYSYMD
           MOVE    WRK-SKYSBTNAME      TO  HCGS139N-SKYSBT
           MOVE    WRK-SRYKANAME1      TO  HCGS139N-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS139N-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-THKN-MAX )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               IF    ( KEY-RECEKA  NOT =   SPACE )
                AND  ( IDX2     =      CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-DAISANTTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
                   CONTINUE
               ELSE
      *
                   MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139N-KENSU (CNT-LINE)
      *
                   MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z5
                   MOVE    WRK-Z5  TO  HCGS139N-NISSU (CNT-LINE)
      *
                   MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-TENSU (CNT-LINE)
      *
                   MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-FTNMONEY (CNT-LINE)
      *
                   MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-HKNMONEY (CNT-LINE)
      *
                   MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-KOHMONEY (CNT-LINE)
      *
                   MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-TTLMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJKAISU (IDX1 IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5  TO  HCGS139N-SKJKAISU (CNT-LINE)
      *
                   MOVE    THKN-SKJTTLMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJTTLMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJFTNMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJFTNMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJHKNMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJHKNMONEY (CNT-LINE)
      *
                   MOVE    THKN-SKJKOHMONEY (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-SKJKOHMONEY (CNT-LINE)
      *
                   MOVE    THKN-TTLTTLMONEY (IDX1 IDX2) TO WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139N-TTLTTLMONEY (CNT-LINE)
               END-IF
      *
               IF    ( IDX2     =      CONST-SYATTL
                                 OR    CONST-KOKTTL
                                 OR    CONST-KOUTTL
                                 OR    CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-DAISANTTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
      *
                   IF    ( KEY-RECEKA      =   SPACE )
                       MOVE    THKN-JIHIMONEY  (IDX1 IDX2)  TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-JIHIMONEY (CNT-LINE)
                       MOVE    THKN-GENMEN    (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-GENMEN (CNT-LINE)
                       MOVE    THKN-CHOSEI1   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-CHOSEI1 (CNT-LINE)
                       MOVE    THKN-CHOSEI2   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-CHOSEI2 (CNT-LINE)
                       MOVE    THKN-HASU      (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139N-HASU    (CNT-LINE)
      *
                       DISPLAY IDX1 ":"IDX2 " "
                           "JIHIMONEY[" THKN-JIHIMONEY(IDX1 IDX2) "]"
                           "GENMEN["    THKN-GENMEN   (IDX1 IDX2) "]"
                           "CHOSEI1["   THKN-CHOSEI1  (IDX1 IDX2) "]"
                           "CHOSEI2["   THKN-CHOSEI2  (IDX1 IDX2) "]"
                           "HASU["      THKN-HASU     (IDX1 IDX2) "]"
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    "特別療養費(注)"    TO  HCGS139N-LBLTOKUYO
           MOVE    "労　　災 （注）"   TO  HCGS139N-LBLROUSAI
           MOVE    "自 賠 責 （注）"   TO  HCGS139N-LBLJIBAI
           MOVE    "公　　害 （注）"   TO  HCGS139N-LBLKOUGAI
      *
           IF    ( KEY-RECEKA          =   SPACE )
      *
               IF    ( BGS100P-ETCGETKBN   =   "0" )
                   MOVE    "特別療養費" TO HCGS139N-LBLTOKUYO
                   MOVE    "労　　災"  TO  HCGS139N-LBLROUSAI
                   MOVE    "自 賠 責"  TO  HCGS139N-LBLJIBAI
                   MOVE    "公　　害"  TO  HCGS139N-LBLKOUGAI
               END-IF
      *
               MOVE    SPACE           TO  HCGS139N-ETCHKNMSG
               STRING  "（注）実際のレセプト件数"
                                           DELIMITED BY SIZE
                       "・日数と一致しない場合があります。"
                                           DELIMITED BY SIZE
               INTO    HCGS139N-ETCHKNMSG
               END-STRING
           ELSE
               MOVE    SPACE           TO  HCGS139N-ETCHKNMSG
               STRING    "（注）全科合計に編集されます。"
                                           DELIMITED   BY  SIZE
               INTO    HCGS139N-ETCHKNMSG
               END-STRING
      *
               MOVE    SPACE   TO  HCGS139N-ETCMSG
               STRING  "その他計は全科合計に編集されます。"
                                       DELIMITED   BY  SIZE
               INTO    HCGS139N-ETCMSG
               END-STRING
           END-IF
      *
           MOVE    TKOH-KENSU (IDX1)   TO  WRK-Z4
           MOVE    WRK-Z4              TO  HCGS139N-KOHKENSU
      *
           IF    ( BGS100P-SRYYM   >=  CONST-H2605 )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L LBL0101 "      DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCGS139N-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L LBL0102 "      DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCGS139N-LOPTION
               END-STRING
           END-IF
      *
           PERFORM 230-HCGS139N-PRINT-SEC
      *
           .
       2001-HCGS139N-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院チェック表編集処理（ＣＳＶ）
      *****************************************************************
       2001-HCGS139N-CSV-SEC           SECTION.
      *
           MOVE    "9"             TO  WRK-PRTKBN
           MOVE    "総括"          TO  WRK-PRTKBNMEI
           IF    ( IDX1            =   1 )
               MOVE    "1"         TO  WRK-NYUGAIKBN
               MOVE    "入院"      TO  WRK-NYUGAIKBNMEI
           ELSE
               MOVE    " "         TO  WRK-NYUGAIKBN
               MOVE    "入院・外来　総計"
                                   TO  WRK-NYUGAIKBNMEI
           END-IF
      *
           MOVE    SPACE           TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   CSV-H01-MAX )
      *
               MOVE    CSV-H01-ITEM (IDXI)
                                   TO  WRK-ITEM
               IF    ( IDXI   >=   CSV-H01-MAX    )
                   PERFORM 802-CSVDATA-EDIT-SEC
               ELSE
                   PERFORM 801-CSVDATA-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE    "1"             TO  WRK-DATAKBN
           MOVE    "保険区分別"    TO  WRK-DATAKBNMEI
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-KOUTTL )
      *
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
           END-PERFORM
      *
           MOVE    SPACE           TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   CSV-H02-MAX )
      *
               MOVE    CSV-H02-ITEM (IDXI)
                                   TO  WRK-ITEM
               IF    ( IDXI   >=   CSV-H02-MAX    )
                   PERFORM 802-CSVDATA-EDIT-SEC
               ELSE
                   PERFORM 801-CSVDATA-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE  SPACE             TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
      *
           MOVE    "2"             TO  WRK-DATAKBN
           MOVE    CSV-H02-KOHNAME
                                   TO  WRK-DATAKBNMEI
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    CSV-H02-KOHNAME TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  HCGS139N-KOHKENSU TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE    SPACE           TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   CSV-H01-MAX )
      *
               MOVE    CSV-H01-ITEM (IDXI)
                                       TO  WRK-ITEM
               IF    ( WRK-ITEM    =   "保険区分" )
                   MOVE    "保険計"    TO  WRK-ITEM
               END-IF
               IF    ( IDXI   >=   CSV-H01-MAX    )
                   PERFORM 802-CSVDATA-EDIT-SEC
               ELSE
                   PERFORM 801-CSVDATA-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE    "3"             TO  WRK-DATAKBN
           MOVE    "保険計"        TO  WRK-DATAKBNMEI
      *
           MOVE    CONST-HKNTTL    TO  IDX2
           PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
           IF    ( KEY-RECEKA      =   SPACE )
      *
               MOVE    CONST-TOKTTL        TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-JIHITTL       TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-CHIKENTTL     TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-ROUSAITTL     TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-JIBAITTL      TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-DAISANTTL      TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-KOUGAITTL     TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    CONST-ALLTTL        TO  IDX2
               PERFORM 20011-HCGS139N-HKN-CSV-SEC
      *
               MOVE    SPACE           TO  WRK-CSVDATA
               MOVE    1               TO  IDXP
               PERFORM VARYING IDXI    FROM    1   BY  1
                       UNTIL ( IDXI    >   CSV-H03-MAX )
      *
                   MOVE    CSV-H03-ITEM (IDXI)
                                       TO  WRK-ITEM
                   IF    ( IDXI   >=   CSV-H03-MAX    )
                       PERFORM 802-CSVDATA-EDIT-SEC
                   ELSE
                       PERFORM 801-CSVDATA-EDIT-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 230-TOUKEICSV-SEC
      *
               MOVE    "4"         TO  WRK-DATAKBN
               MOVE    CSV-H03-ETCNAME
                                   TO  WRK-DATAKBNMEI
      *
               MOVE    CONST-SYATTL        TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-KOKTTL        TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-KOUTTL        TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-TOKTTL        TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-JIHITTL       TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-CHIKENTTL     TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-ROUSAITTL     TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-JIBAITTL      TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-DAISANTTL      TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-KOUGAITTL     TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
               MOVE    CONST-ALLTTL        TO  IDX2
               PERFORM 20011-HCGS139N-ETC-CSV-SEC
      *
           END-IF
      *
           .
       2001-HCGS139N-CSV-EXT.
           EXIT.
      *****************************************************************
      *    入院チェック表編集処理（ＣＳＶ）（保険情報）
      *****************************************************************
       20011-HCGS139N-HKN-CSV-SEC      SECTION.
      *
           MOVE  SPACE             TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
      *
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    CONST-HKNMEI (IDX2)
                                   TO  WRK-ITEM
           IF    ( BGS100P-SRYYM   >=  CONST-H2605 )
               INITIALIZE          ORCSSTRINGAREA
               MOVE    "sub"   TO  ORCSSTR-COMMAND
               MOVE    WRK-ITEM TO ORCSSTR-STRING1
               MOVE    "高９"  TO  ORCSSTR-STRING2
               MOVE    "高一"  TO  ORCSSTR-STRING3
               CALL    "orcsstring" USING  ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING1
                               TO  WRK-ITEM
           END-IF
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-KENSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-NISSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-TENSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-FTNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-HKNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-KOHMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-TTLMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-SKJKAISU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-SKJFTNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-SKJHKNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-SKJKOHMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-SKJTTLMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-TTLTTLMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           .
       20011-HCGS139N-HKN-CSV-EXT.
           EXIT.
      *****************************************************************
      *    入院チェック表編集処理（ＣＳＶ）（その他情報）
      *****************************************************************
       20011-HCGS139N-ETC-CSV-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-CSVDATA
           MOVE    1                   TO  IDXP
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    CONST-HKNMEI (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-JIHIMONEY (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-GENMEN (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-CHOSEI1 (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-CHOSEI2 (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139N-HASU  (IDX2)
                                       TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           .
       20011-HCGS139N-ETC-CSV-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶファイルＫＥＹ項目編集処理
      *****************************************************************
       20011-CSV-KEY-EDIT-SEC         SECTION.
      *
           MOVE  WRK-PRTKBN        TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-PRTKBNMEI     TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-DATAKBN       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-DATAKBNMEI    TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-CSV-SRYYM     TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-NYUGAIKBN     TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-NYUGAIKBNMEI
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  KEY-SKYSBT        TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-SKYSBTNAME    TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  KEY-RECEKA        TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-SRYKANAME1    TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           .
       20011-CSV-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS139N-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS139NV02.red" TO  SPRT-PRTID
           MOVE    HCGS139N       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS139N-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    外来総括表編集処理
      *****************************************************************
       2001-HCGS139G-EDIT-SEC          SECTION.
      *
           INITIALIZE                      HCGS139G
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS139G-SRYYM
           MOVE    "外来"              TO  HCGS139G-NYUGAI
           MOVE    WRK-PRT-SAKYMD      TO  HCGS139G-SYSYMD
           MOVE    WRK-SKYSBTNAME      TO  HCGS139G-SKYSBT
           MOVE    WRK-SRYKANAME1      TO  HCGS139G-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS139G-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-THKN-MAX )
      *
               COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
               IF    ( KEY-RECEKA  NOT =   SPACE )
                AND  ( IDX2     =      CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-DAISANTTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
                   CONTINUE
               ELSE
                   MOVE    THKN-KENSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139G-KENSU (CNT-LINE)
      *
                   MOVE    THKN-NISSU (IDX1 IDX2)     TO  WRK-Z4
                   MOVE    WRK-Z4  TO  HCGS139G-NISSU (CNT-LINE)
      *
                   MOVE    THKN-TENSU (IDX1 IDX2)     TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-TENSU (CNT-LINE)
      *
                   MOVE    THKN-FTNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-FTNMONEY (CNT-LINE)
      *
                   MOVE    THKN-HKNMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-HKNMONEY (CNT-LINE)
      *
                   MOVE    THKN-KOHMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-KOHMONEY (CNT-LINE)
      *
                   MOVE    THKN-TTLMONEY (IDX1 IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  HCGS139G-TTLMONEY (CNT-LINE)
               END-IF
      *
               IF    ( IDX2     =      CONST-SYATTL
                                 OR    CONST-KOKTTL
                                 OR    CONST-KOUTTL
                                 OR    CONST-TOKTTL
                                 OR    CONST-JIHITTL
                                 OR    CONST-CHIKENTTL
                                 OR    CONST-ROUSAITTL
                                 OR    CONST-JIBAITTL
                                 OR    CONST-DAISANTTL
                                 OR    CONST-KOUGAITTL
                                 OR    CONST-ALLTTL )
      *
                   IF    ( KEY-RECEKA      =   SPACE )
                       MOVE    THKN-JIHIMONEY  (IDX1 IDX2)  TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-JIHIMONEY (CNT-LINE)
                       MOVE    THKN-GENMEN    (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-GENMEN (CNT-LINE)
                       MOVE    THKN-CHOSEI1   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-CHOSEI1 (CNT-LINE)
                       MOVE    THKN-CHOSEI2   (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-CHOSEI2 (CNT-LINE)
                       MOVE    THKN-HASU      (IDX1 IDX2)   TO WRK-M9
                       MOVE    WRK-M9  TO  HCGS139G-HASU    (CNT-LINE)
      *
                       DISPLAY IDX1 ":"IDX2 " "
                           "JIHIMONEY[" THKN-JIHIMONEY(IDX1 IDX2) "]"
                           "GENMEN["    THKN-GENMEN   (IDX1 IDX2) "]"
                           "CHOSEI1["   THKN-CHOSEI1  (IDX1 IDX2) "]"
                           "CHOSEI2["   THKN-CHOSEI2  (IDX1 IDX2) "]"
                           "HASU["      THKN-HASU     (IDX1 IDX2) "]"
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    "特別療養費(注)"    TO  HCGS139G-LBLTOKUYO
           MOVE    "労　　災 （注）"   TO  HCGS139G-LBLROUSAI
           MOVE    "自 賠 責 （注）"   TO  HCGS139G-LBLJIBAI
           MOVE    "公　　害 （注）"   TO  HCGS139G-LBLKOUGAI
      *
           IF    ( KEY-RECEKA          =   SPACE )
      *
               IF    ( BGS100P-ETCGETKBN   =   "0" )
                   MOVE    "特別療養費" TO HCGS139G-LBLTOKUYO
                   MOVE    "労　　災"  TO  HCGS139G-LBLROUSAI
                   MOVE    "自 賠 責"  TO  HCGS139G-LBLJIBAI
                   MOVE    "公　　害"  TO  HCGS139G-LBLKOUGAI
               END-IF
      *
               MOVE    SPACE           TO  HCGS139G-ETCHKNMSG
               STRING  "（注）実際のレセプト件数"
                                           DELIMITED BY SIZE
                       "・日数と一致しない場合があります。"
                                           DELIMITED BY SIZE
               INTO    HCGS139G-ETCHKNMSG
               END-STRING
           ELSE
               MOVE    SPACE           TO  HCGS139G-ETCHKNMSG
               STRING    "（注）全科合計に編集されます。"
                                           DELIMITED   BY  SIZE
               INTO    HCGS139G-ETCHKNMSG
               END-STRING
      *
               MOVE    SPACE   TO  HCGS139G-ETCMSG
               STRING  "その他計は全科合計に編集されます。"
                                       DELIMITED   BY  SIZE
               INTO    HCGS139G-ETCMSG
               END-STRING
           END-IF
      *
           MOVE    TKOH-KENSU (IDX1)   TO  WRK-Z4
           MOVE    WRK-Z4              TO  HCGS139G-KOHKENSU
      *
           IF    ( BGS100P-SRYYM   >=  CONST-H2605 )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L LBL0101 "      DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCGS139G-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L LBL0102 "      DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCGS139G-LOPTION
               END-STRING
           END-IF
      *
           PERFORM 230-HCGS139G-PRINT-SEC
      *
           .
       2001-HCGS139G-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS139G-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           MOVE    "HCGS139GV02.red" TO  SPRT-PRTID
           MOVE    HCGS139G       TO  SPRT-PRTDATA
      *
           PERFORM 230-PRINT-SEC
      *
           .
       230-HCGS139G-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    外来チェック表編集処理（ＣＳＶ）
      *****************************************************************
       2001-HCGS139G-CSV-SEC           SECTION.
      *
           MOVE    "9"             TO  WRK-PRTKBN
           MOVE    "総括"          TO  WRK-PRTKBNMEI
           MOVE    "2"             TO  WRK-NYUGAIKBN
           MOVE    "外来"          TO  WRK-NYUGAIKBNMEI
      *
           MOVE    SPACE           TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   CSV-H04-MAX )
      *
               MOVE    CSV-H04-ITEM (IDXI)
                                   TO  WRK-ITEM
               IF    ( IDXI   >=   CSV-H04-MAX    )
                   PERFORM 802-CSVDATA-EDIT-SEC
               ELSE
                   PERFORM 801-CSVDATA-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE    "1"             TO  WRK-DATAKBN
           MOVE    "保険区分別"    TO  WRK-DATAKBNMEI
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CONST-KOUTTL )
      *
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
           END-PERFORM
      *
           MOVE    SPACE           TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   CSV-H02-MAX )
      *
               MOVE    CSV-H02-ITEM (IDXI)
                                   TO  WRK-ITEM
               IF    ( IDXI   >=   CSV-H02-MAX    )
                   PERFORM 802-CSVDATA-EDIT-SEC
               ELSE
                   PERFORM 801-CSVDATA-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE  SPACE             TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
      *
           MOVE    "2"             TO  WRK-DATAKBN
           MOVE    CSV-H02-KOHNAME
                                   TO  WRK-DATAKBNMEI
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    CSV-H02-KOHNAME TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  HCGS139G-KOHKENSU TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE    SPACE           TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
           PERFORM VARYING IDXI    FROM    1   BY  1
                   UNTIL ( IDXI    >   CSV-H04-MAX )
      *
               MOVE    CSV-H04-ITEM (IDXI)
                                       TO  WRK-ITEM
               IF    ( WRK-ITEM    =   "保険区分" )
                   MOVE    "保険計"    TO  WRK-ITEM
               END-IF
               IF    ( IDXI   >=   CSV-H04-MAX    )
                   PERFORM 802-CSVDATA-EDIT-SEC
               ELSE
                   PERFORM 801-CSVDATA-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           MOVE    "3"             TO  WRK-DATAKBN
           MOVE    "保険計"        TO  WRK-DATAKBNMEI
      *
           MOVE    CONST-HKNTTL    TO  IDX2
           PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
           IF    ( KEY-RECEKA      =   SPACE )
      *
               MOVE    CONST-TOKTTL        TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-JIHITTL       TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-CHIKENTTL     TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-ROUSAITTL     TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-JIBAITTL      TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-DAISANTTL     TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-KOUGAITTL     TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    CONST-ALLTTL        TO  IDX2
               PERFORM 20011-HCGS139G-HKN-CSV-SEC
      *
               MOVE    SPACE           TO  WRK-CSVDATA
               MOVE    1               TO  IDXP
               PERFORM VARYING IDXI    FROM    1   BY  1
                       UNTIL ( IDXI    >   CSV-H03-MAX )
      *
                   MOVE    CSV-H03-ITEM (IDXI)
                                       TO  WRK-ITEM
                   IF    ( IDXI   >=   CSV-H03-MAX    )
                       PERFORM 802-CSVDATA-EDIT-SEC
                   ELSE
                       PERFORM 801-CSVDATA-EDIT-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 230-TOUKEICSV-SEC
      *
               MOVE    "4"         TO  WRK-DATAKBN
               MOVE    CSV-H03-ETCNAME
                                   TO  WRK-DATAKBNMEI
      *
               MOVE    CONST-SYATTL        TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-KOKTTL        TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-KOUTTL        TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-TOKTTL        TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-JIHITTL       TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-CHIKENTTL     TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-ROUSAITTL     TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-JIBAITTL      TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-DAISANTTL      TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-KOUGAITTL     TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
               MOVE    CONST-ALLTTL        TO  IDX2
               PERFORM 20011-HCGS139G-ETC-CSV-SEC
      *
           END-IF
      *
           .
       2001-HCGS139G-CSV-EXT.
           EXIT.
      *****************************************************************
      *    外来チェック表編集処理（ＣＳＶ）（保険情報）
      *****************************************************************
       20011-HCGS139G-HKN-CSV-SEC      SECTION.
      *
           MOVE  SPACE             TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
      *
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    CONST-HKNMEI (IDX2)
                                   TO  WRK-ITEM
           IF    ( BGS100P-SRYYM   >=  CONST-H2605 )
               INITIALIZE          ORCSSTRINGAREA
               MOVE    "sub"   TO  ORCSSTR-COMMAND
               MOVE    WRK-ITEM TO ORCSSTR-STRING1
               MOVE    "高９"  TO  ORCSSTR-STRING2
               MOVE    "高一"  TO  ORCSSTR-STRING3
               CALL    "orcsstring" USING  ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING1
                               TO  WRK-ITEM
           END-IF
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-KENSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-NISSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-TENSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-FTNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-HKNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-KOHMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-TTLMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           .
       20011-HCGS139G-HKN-CSV-EXT.
           EXIT.
      *****************************************************************
      *    外来チェック表編集処理（ＣＳＶ）（その他情報）
      *****************************************************************
       20011-HCGS139G-ETC-CSV-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-CSVDATA
           MOVE    1                   TO  IDXP
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    CONST-HKNMEI (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-JIHIMONEY (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-GENMEN (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-CHOSEI1 (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-CHOSEI2 (IDX2)
                                       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS139G-HASU  (IDX2)
                                       TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           .
       20011-HCGS139G-ETC-CSV-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-PRINT-SEC                   SECTION.
      *
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    BGS100P-SRYYM   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    BGS100P-SHELLMSG TO SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    BGS100P-SRYYM   TO  STOUKEICSV-SRYYM
           MOVE    BGS100P-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    BGS100P-CSVDIR  TO  STOUKEICSV-TO-FOLDER
           MOVE    BGS100P-CSVFNAME TO STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA     TO  STOUKEICSV-CSVDATA
           MOVE    "保険別請求チェック表ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF    ( STOUKEICSV-RETURN   =   ZERO )
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           COMPUTE BGS100P-PAGE
               =   BGS100P-PAGE    +   CNT-PAGE
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BGS100ERR-FILE
           IF      STS-BGS100ERR     =   ZERO
               CLOSE   BGS100ERR-FILE
           ELSE
               OPEN    OUTPUT              BGS100ERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BGS100ERR-REC
               WRITE   BGS100ERR-REC
               CLOSE   BGS100ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  BGS100P-RC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           IF    ( WRK-ITEM        NOT =   SPACE )
               PERFORM 800-GET-ITEM-SIZE-SEC
               STRING  WRK-ITEM(IDXL:CNT-LEN)  DELIMITED   BY  SIZE
               INTO    WRK-CSVDATA
               WITH    POINTER   IDXP
               END-STRING
           END-IF
           .
      *
       800-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       801-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 800-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       802-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 800-CSVDATA-EDIT-SEC
      *    PERFORM 800-PUT-CR-SEC
           .
      *
       802-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXL
                                       CNT-LEN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-ITEM-SIZE )
                    OR   ( WRK-ITEM (IDX0:)    =  SPACE )
      *
               IF    ( IDXL = ZERO )
                   IF    ( WRK-ITEM (IDX0:1)   NOT =  SPACE )
                       MOVE    IDX0 TO IDXL
                       MOVE    1    TO CNT-LEN
                   END-IF
               ELSE
                   COMPUTE CNT-LEN  =  CNT-LEN + 1
               END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","   DELIMITED   BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"   DELIMITED BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-HIZUKE-EDIT-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"   USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3 REPLACING  ALL "  "  BY  "　"
           END-IF
      *
      *
           .
       800-HIZUKE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BGS100P-JOBID       TO  JOB-JOBID
           MOVE    BGS100P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCBGS100"         TO  JOB-PGID
           MOVE    BGS100P-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求ファイル読込処理
      *****************************************************************
       900-BGSSKY-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSSKY
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSSKY  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    BGSSKY-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSSKY
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   MOVE    BGSSKY-REC  TO  BGSF01-REC
                   PERFORM 9001-SKIP-CHK-SEC
      *
               END-READ
           END-PERFORM
      *
           .
      *
       900-BGSSKY-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費ファイル読込処理
      *****************************************************************
       900-BGSKOH-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSKOH
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSKOH  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    BGSKOH-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSKOH
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   MOVE    BGSKOH-REC  TO  BGSF01-REC
                   PERFORM 9001-SKIP-CHK-SEC
      *
               END-READ
           END-PERFORM
      *
           .
      *
       900-BGSKOH-READ-EXT.
           EXIT.
      *****************************************************************
      *    その他請求ファイル読込処理
      *****************************************************************
       900-BGSETC-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSETC
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSETC  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    BGSETC-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSETC
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   MOVE    BGSETC-REC  TO  BGSF01-REC
                   PERFORM 9001-SKIP-CHK-SEC
      *
               END-READ
           END-PERFORM
      *
           .
      *
       900-BGSETC-READ-EXT.
           EXIT.
      *****************************************************************
      *    読み飛ばしチェック処理
      *****************************************************************
       9001-SKIP-CHK-SEC           SECTION.
      *
           IF    ( BGS100P-NYUGAIKBN   =   "1" OR "2" )
               IF    ( BGS100P-NYUGAIKBN
                                   NOT =   BGSF01-NYUGAIKBN )
                   MOVE    ZERO    TO  FLG-LOOPEND
               END-IF
      *
           END-IF
      *
           IF    ( KEY-SKYSBT      NOT =   SPACE )
               IF   (( KEY-SKYSBT      =   "1" )
                AND  ( BGSF01-SKYSBT   =   "1" ))
                 OR (( KEY-SKYSBT      =   "2" )
                AND  ( BGSF01-SKYSBT   =   "2" OR "3" ))
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-LOOPEND
               END-IF
           END-IF
      *
           IF    ( KEY-RECEKA      NOT =   SPACE )
               IF    ( KEY-RECEKA  NOT =   BGSF01-RECEKA )
                   MOVE    ZERO    TO  FLG-LOOPEND
               END-IF
           END-IF
      *
           .
      *
       9001-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCBGS139 RC [" BGS100P-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
