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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBD010S.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 収納日報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/23    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    中間ファイル
           SELECT  BD01003-FILE   ASSIGN   BD01003PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  DYNAMIC
                                  RECORD  KEY     IS  BD01003-KEY
                                  FILE    STATUS  IS  STS-BD01003.
      *
           SELECT  BD01004-FILE   ASSIGN   BD01004PARA
                                  ORGANIZATION    IS  INDEXED
                                  ACCESS  MODE    IS  DYNAMIC
                                  RECORD  KEY     IS  BD01004-KEY
                                  FILE    STATUS  IS  STS-BD01004.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS IS  STS-RECEERR.
      *
       DATA                        DIVISION.
      *
       FILE                        SECTION.
      *
       FD  BD01003-FILE.
       01  BD01003-REC.
           COPY    "CPBD01003.INC".
      *
       FD  BD01004-FILE.
       01  BD01004-REC.
           COPY    "CPBD01004.INC".
           03  BD01004-KANANAME    PIC X(100).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
       01  STS-AREA.
           03  STS-BD01003                 PIC X(02).
           03  STS-BD01004                 PIC X(02).
           03  STS-RECEERR                 PIC X(02).
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01//
                                   BY          //BD01003//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING   //RECE01//
                                   BY          //BD01004//.
      *
           COPY    "CPRECEERR.INC".
      *
      *     COPY    "HCM51V03.INC"  REPLACING   //HCM51V03//
      *                             BY          //HCM51//.
           COPY    "HCM51.INC".
           COPY    "HCM51V02.INC".
           COPY    "HCM52V05.INC"  REPLACING   //HCM52V05//
                                   BY          //HCM52//.
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-BD01003                 PIC 9(01).
           03  FLG-BD01004                 PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-LINE                    PIC 9(05).
           03  CNT-NO                      PIC 9(05).
           03  CNT-SMEI                    PIC 9(05).
           03  CNT-KETA                    PIC 9(02).
           03  CNT-KETA2                   PIC 9(02).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX-SUM                     PIC 9(05).
           03  IDX-HOKEN                   PIC 9(05).
           03  IDX-JIHI                    PIC 9(05).
           03  IDX-SRH                     PIC 9(05).
           03  IDX-HOHO                    PIC 9(05).
           03  IDX-SYAHON                  PIC 9(02).
           03  IDX-SYAKZK                  PIC 9(02).
           03  IDX-SYARJN                  PIC 9(02).
           03  IDX-KOKIPN                  PIC 9(02).
           03  IDX-KOKRJN                  PIC 9(02).
           03  IDX-KOHTAN                  PIC 9(02).
           03  IDX-JIHHKN                  PIC 9(02).
           03  IDX-ROUSAI                  PIC 9(02).
           03  IDX-JIBAI                   PIC 9(02).
           03  IDX-KOUGAI                  PIC 9(02).
           03  IDX-KOUKI                   PIC 9(02).
           03  IDX-CHIKEN                  PIC 9(02).
           03  IDX-S02                     PIC 9(05).
           03  IDX-HS02                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-TOUKEIERR               PIC X(200).
           03  WRK-REC                     PIC X(20000).
           03  WRK-SOURCE                  PIC X(20000).
           03  WRK-PATTERN                 PIC X(64).
           03  WRK-REPLACEMENT             PIC X(64).
           03  WRK-FLAGS                   PIC X(64).
           03  WRK-HENSURYO                PIC X(10).
           03  WRK-SURYO                   PIC ---------9.
           03  WRK-SYSYMD                  PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-STDAILYKEY.
               05  WRK-STDAILYKEY-YMD      PIC X(08).
               05  WRK-STDAILYKEY-TIME     PIC X(04).
           03  WRK-EDDAILYKEY.
               05  WRK-EDDAILYKEY-YMD      PIC X(08).
               05  WRK-EDDAILYKEY-TIME     PIC X(04).
           03  WRK-SYOSAIKBN               PIC X(01).
           03  WRK-HS02-MAX                PIC 9(05).
           03  WRK-KANACHK-INPUT           PIC X(100).
           03  WRK-KANACHK-MAX             PIC 9(05).
           03  WRK-PRT-SAKYMD              PIC X(22).
           03  WRK-PRT-SRYYMD              PIC X(64).
           03  WRK-PRT-STYMD               PIC X(22).
           03  WRK-PRT-STTIME.
               05  WRK-PRT-STTIME-HH       PIC X(04).
               05  WRK-PRT-STTIME-MM       PIC X(04).
           03  WRK-PRT-EDYMD               PIC X(22).
           03  WRK-PRT-EDTIME.
               05  WRK-PRT-EDTIME-HH       PIC X(04).
               05  WRK-PRT-EDTIME-MM       PIC X(04).
           03  WRK-OUT-INPUT               PIC X(300).
           03  WRK-OUT-MAX                 PIC 9(05).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-ZZZZ9                   PIC ZZZZ9.
           03  WRK-ZZZZZZZ9                PIC ----,---,--9.
           03  WRK-ZZZZZZZZ                PIC ----,---,---.
           03  WRK-ZX99                    PIC ---------9.
           03  WRK-KONNYUKIN-G.
               05  WRK-KONNYUKIN           PIC ---,---,--9.
               05  WRK-KONNYUKINFL2        PIC X(01).
           03  WRK-CHOSEI                  PIC S9(9).
           03  WRK-9-X.
               05  WRK-9                   PIC 9(01).
           03  WRK-SAGAKU                  PIC S9(9).
           03  WRK-SKYTTL-G.
               05  WRK-SKYTTL-MAX          PIC 9(03).
               05  WRK-SKYTTL-OCC          OCCURS  15.
                   07  WRK-SKYTTLMSG       PIC X(12).
           03  WRK-NYUKIN                  PIC S9(10).
           03  WRK-LINE-MAX                PIC 9(05).
           03  WRK-PARA-NAME               PIC X(25).
           03  WRK-PARA                    PIC X(100).
           03  WRK-CSV-STSRYYMD            PIC X(10).
           03  WRK-CSV-EDSRYYMD            PIC X(10).
           03  WRK-CSV-STTIME              PIC X(05).
           03  WRK-CSV-EDTIME              PIC X(05).
           03  WRK-CSV-SAKYMD              PIC X(10).
           03  WRK-CSV-DATE                PIC X(10).
           03  WRK-CSV-TIME                PIC X(05).
           03  WRK-CSV-DASH                PIC X(02).
           03  WRK-TIME                    PIC X(04).
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
       01  TTL-AREA.
           03  TTL-SYOSIN              PIC     S9(10).
           03  TTL-SAISIN              PIC     S9(10).
           03  TTL-HKNTEN              PIC     S9(10).
           03  TTL-SKYMONEY            PIC     S9(10).
           03  TTL-SKYHOKEN            PIC     S9(10).
           03  TTL-SKYJIHI             PIC     S9(10).
           03  TTL-KONNYUKIN           PIC     S9(10).
           03  TTL-NYUKIN              PIC     S9(10).
           03  TTL-CHOSEI              PIC     S9(10).
           03  TTL-CHOSEI1             PIC     S9(10).
           03  TTL-CHOSEI2             PIC     S9(10).
           03  TTL-GENMEN              PIC     S9(10).
           03  TTL-MISYU               PIC     S9(10).
           03  TTL-ZENMISYU            PIC     S9(10).
      *
       01  CSV-AREA.
           03  CSV-SYOSIN              PIC X(10).
           03  CSV-SAISIN              PIC X(10).
           03  CSV-HKNTEN              PIC X(10).
           03  CSV-SKYMONEY            PIC X(10).
           03  CSV-SKYHOKEN            PIC X(10).
           03  CSV-SKYJIHI             PIC X(10).
           03  CSV-KONNYUKIN           PIC X(10).
           03  CSV-NYUKIN              PIC X(10).
           03  CSV-CHOSEI              PIC X(10).
           03  CSV-CHOSEI1             PIC X(10).
           03  CSV-CHOSEI2             PIC X(10).
           03  CSV-GENMEN              PIC X(10).
           03  CSV-ZENMISYU            PIC X(10).
           03  CSV-MISYU               PIC X(10).
           03  CSV-SRYYMD              PIC X(10).
       01  CSV-AREA2.
           03  CSV-DAI                 PIC X(14).
           03  CSV-GOKEI               PIC X(10).
           03  CSV-SKYMONEY2           PIC X(10)  OCCURS 15.
      *
       01  KEY-AREA.
           03  KEY-NYUGAIKBN                   PIC X(01).
           03  KEY-PTID                        PIC 9(10).
           03  KEY-DENPNUM                     PIC 9(07).
           03  KEY-SRYKA                       PIC X(02).
      *
       01  SRH-AREA.
           03  SRH-CD                          PIC X(08).
           03  SRH-IDX                         PIC 9(05).
           03  SRH-ITM-G.
               05  SRH-ITM                     PIC X(100).
               05  SRH-ITM2                    PIC X(100).
           03  SRH-1013-G.
               05  SRH-1013-OCC                OCCURS  2.
                   07  SRH-1013-JIHINAME       PIC X(10)
                                               OCCURS  10.
           03  SRH-1005-G.
               05  SRH-1005-MAX                PIC 9(05).
               05  SRH-1005-OCC                OCCURS  100.
                   07  SRH-1005-CD             PIC X(02).
                   07  SRH-1005-SRYKANAME1     PIC X(10).
                   07  SRH-1005-SRYKANAME3     PIC X(02).
      *
           03  SRH-1041-G.
               05  SRH-1041-MAX                PIC 9(05).
               05  SRH-1041-OCC                OCCURS  100.
                   07  SRH-1041-CD             PIC X(02).
                   07  SRH-1041-NYKN-TANNAME   PIC X(10).
      *
       01  TPARA-AREA.
           03  TPARA-FORM                      PIC X(01).
           03  TPARA-SHADE                     PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-H200401                   PIC X(08)
                                               VALUE "20080401".
           03  CONST-H190401                   PIC X(08)
                                               VALUE "20070401".
           03  CONST-LINE-FORM0                PIC 9(05) VALUE 28.
           03  CONST-LINE-FORM1                PIC 9(05) VALUE 50.
           03  CONST-ETCJIHI-MAX               PIC 9(05) VALUE 10.
           03  CONST-1005-MAX                  PIC 9(05) VALUE 100.
           03  CONST-1041-MAX                  PIC 9(05) VALUE 100.
           03  CONST-HOHO-HENKIN               PIC X(02) VALUE "@1".
           03  CONST-HOHO-HENKINMSG            PIC X(10)
                                               VALUE "返金".
           03  CONST-HOHO-TORIKESI             PIC X(02) VALUE "@2".
           03  CONST-HOHO-TORIKESIMSG          PIC X(10)
                                               VALUE "入金取消".
           03  CONST-TEISEI                    PIC X(01) VALUE "8".
           03  CONST-HENKIN                    PIC X(01) VALUE "4".
           03  CONST-TORIKESI                  PIC X(02) VALUE "6".
      *
           03  CONST-TTLSEIKYUREL              PIC 9(02) VALUE 1.
           03  CONST-TTLGENMEN                 PIC 9(02) VALUE 2.
           03  CONST-TTLCHOSEI1                PIC 9(02) VALUE 3.
           03  CONST-TTLCHOSEI2                PIC 9(02) VALUE 4.
           03  CONST-TTLSEIKYU                 PIC 9(02) VALUE 5.
           03  CONST-TTLNYUKIN                 PIC 9(02) VALUE 6.
           03  CONST-TTLKONNYUKIN              PIC 9(02) VALUE 7.
           03  CONST-TTLMISYU                  PIC 9(02) VALUE 8.
           03  CONST-TTLSYOSIN                 PIC 9(02) VALUE 9.
           03  CONST-TTLSAISIN                 PIC 9(02) VALUE 10.
           03  CONST-TTLSYOSAI                 PIC 9(02) VALUE 11.
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(15)   VALUE
                                                   "syuunou_nippou_".
               05  WRK-TO-DATA-KIKAN   PIC X(21).
           03  WRK-TO-DATA1.
               05  WRK-TO-DATA-YMD     PIC X(08).
               05  FILLER              PIC X(04)   VALUE   ".csv".
           03  WRK-TO-DATA2.
               05  WRK-TO-DATA-STRYMD  PIC X(08).
               05  FILLER              PIC X(01)   VALUE   "-".
               05  WRK-TO-DATA-ENDYMD  PIC X(08).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01A.
           03  FILLER              PIC X(08)   VALUE   "収納日報".
           03  CSV-NYUGAI          PIC X(08)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SRYKA           PIC X(10)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "診療日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-STHSRYYMD       PIC X(10).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-STTIME          PIC X(05).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-DASH            PIC X(02).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-EDHSRYYMD       PIC X(10).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-EDTIME          PIC X(05).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "作成日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SAKYMD          PIC X(10).
       01  CSV-HEAD-01B.
           03  FILLER              PIC X(08)   VALUE   "伝票番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SRYYMDMEI       PIC X(10)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SYOSAIMEI       PIC X(04)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療科名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "保険請求額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "自費請求額".
           03  FILLER              PIC X(01)   VALUE   ",".
       01  CSV-HEAD-011.
           03  FILLER              PIC X(06)   VALUE   "減免額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "調整金１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "調整金２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "前回未収額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "今回請求額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "入金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入金方法".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "今回未収額".
       01  CSV-HEAD-012.
           03  FILLER              PIC X(06)   VALUE   "調整金".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "今回請求額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "入金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入金方法".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "未収額".
      *    ヘッダー情報
       01  CSV-HEAD-02A.
           03  FILLER              PIC X(06)   VALUE   "合計表".
           03  CSV-NYUGAI2         PIC X(08)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SRYKA2          PIC X(10)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "診療日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-STHSRYYMD2      PIC X(10).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-STTIME2         PIC X(05).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-DASH2           PIC X(02).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-EDHSRYYMD2      PIC X(10).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-EDTIME2         PIC X(05).
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "作成日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-SAKYMD2         PIC X(10).
       01  CSV-HEAD-02B.
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "合計".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "社保本人".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "社保家族".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "国保".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "後期高齢者".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "公費単独".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "自費".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "労災".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "自賠責".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "公害".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    自費名称
           COPY    "CPSK1013.INC".
      *
      *    締め時刻情報
           COPY    "CPSK3005.INC".
      *
      *    入金方法
           COPY    "CPSK1041.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    収納明細編集サブ
           COPY    "CPORCBD010S02.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCSKANACHK2.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    初再診取得サブ
           COPY "CPORCSS003.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
      *
       01  LNK-PARA.
           COPY    "CPORCSPRTLNK.INC".
      *
           COPY    "CPORCBD010S.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   LNK-PARA
                                   SBD010-AREA
                                   SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      STS-AREA
                                           IDX-AREA
                                           CNT-AREA
                                           FLG-AREA
                                           WRK-AREA
                                           KEY-AREA
                                           SRH-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SBD010-JOBID    TO  JOB-JOBID
           MOVE    SBD010-SHELLID  TO  JOB-SHELLID
           IF    ( SBD010-SIMEKBN      =   ZERO )
               MOVE    "ORCBD010"  TO  JOB-PGID
           ELSE
               MOVE    "ORCBD010V02"
                                   TO  JOB-PGID
           END-IF
           MOVE    "収納日報"      TO  JOB-SHELLMSG
           MOVE    SBD010-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    SBD010-HOSPNUM TO BD01003PARA-HOSPNUM
           MOVE    "BD01003"       TO  BD01003PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BD01003PARA-TERMID
      *
           MOVE    SBD010-HOSPNUM TO BD01004PARA-HOSPNUM
           MOVE    "BD01004"       TO  BD01004PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  BD01004PARA-TERMID
      *
           MOVE    SBD010-ERRFLPARA TO RECEERR
      *

      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BD01003PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    BD01004PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (2)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (3)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BD01003PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  BD01004PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                   TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
      *
           IF    ( SBD010-SIMEKBN      =   ZERO )
      *
               MOVE    SBD010-SRYYMD   TO  WRK-STDAILYKEY-YMD
                                           WRK-EDDAILYKEY-YMD
               MOVE     ZERO           TO  WRK-STDAILYKEY-TIME
               MOVE     ALL "9"        TO  WRK-EDDAILYKEY-TIME
      *
               IF    ( SBD010-EDSRYYMD     NOT =   SPACE )
                   MOVE    SBD010-EDSRYYMD TO  WRK-EDDAILYKEY-YMD
               END-IF
      *
               MOVE    WRK-STDAILYKEY-YMD  TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD3         TO  WRK-PRT-STYMD
      *
               MOVE    WRK-EDDAILYKEY-YMD  TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( WRK-STDAILYKEY-YMD (1:4)
                                           =   WRK-EDDAILYKEY-YMD (1:4))
                   MOVE    WRK-EDTYMD3 (11:)
                                           TO  WRK-PRT-EDYMD
               ELSE
                   MOVE    WRK-EDTYMD3 (5:)
                                           TO  WRK-PRT-EDYMD
               END-IF
      *
               MOVE    SPACE               TO  WRK-PRT-SRYYMD
               IF    ( WRK-STDAILYKEY-YMD  =   WRK-EDDAILYKEY-YMD )
                   MOVE    WRK-PRT-STYMD   TO  WRK-PRT-SRYYMD
               ELSE
                   STRING  WRK-PRT-STYMD   DELIMITED   BY  SIZE
                           "〜"            DELIMITED   BY  SIZE
                           WRK-PRT-EDYMD   DELIMITED   BY  SPACE
                   INTO    WRK-PRT-SRYYMD
                   END-STRING
               END-IF
           ELSE
      *
               INITIALIZE                  SYS-3005-REC
               MOVE    "3005"          TO  SYS-3005-KANRICD
               MOVE    "*"             TO  SYS-3005-KBNCD
               MOVE    WRK-SYSYMD      TO  SYS-3005-STYUKYMD
                                           SYS-3005-EDYUKYMD
               MOVE    SYS-3005-REC    TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC        TO  SYS-3005-REC
                   MOVE    SYS-3005-STSRYYMD   TO  WRK-STDAILYKEY-YMD
                   MOVE    SYS-3005-STTIME     TO  WRK-STDAILYKEY-TIME
                   MOVE    SYS-3005-EDSRYYMD   TO  WRK-EDDAILYKEY-YMD
                   MOVE    SYS-3005-EDTIME     TO  WRK-EDDAILYKEY-TIME
      *
                   MOVE    WRK-STDAILYKEY-YMD  TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD3         TO  WRK-PRT-STYMD
      *
                   MOVE    WRK-STDAILYKEY-TIME TO  WRK-KANACHK-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-KANACHK-INPUT   TO  WRK-PRT-STTIME
      *
                   MOVE    WRK-EDDAILYKEY-YMD  TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   IF    ( WRK-STDAILYKEY-YMD (1:4)
                                           =   WRK-EDDAILYKEY-YMD (1:4))
                       MOVE    WRK-EDTYMD3 (11:)
                                           TO  WRK-PRT-EDYMD
                   ELSE
                       MOVE    WRK-EDTYMD3 (5:)
                                           TO  WRK-PRT-EDYMD
                   END-IF
      *
                   MOVE    WRK-EDDAILYKEY-TIME TO  WRK-KANACHK-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-KANACHK-INPUT   TO  WRK-PRT-EDTIME
      *
                   MOVE    SPACE               TO  WRK-PRT-SRYYMD
                   IF    ( WRK-STDAILYKEY-YMD  =   WRK-EDDAILYKEY-YMD )
                       STRING   WRK-PRT-STYMD      DELIMITED   BY  SIZE
                                WRK-PRT-STTIME-HH  DELIMITED   BY  SIZE
                                "："               DELIMITED   BY  SIZE
                                WRK-PRT-STTIME-MM  DELIMITED   BY  SIZE
                                "〜"               DELIMITED   BY  SIZE
                                WRK-PRT-EDTIME-HH  DELIMITED   BY  SIZE
                                "："               DELIMITED   BY  SIZE
                                WRK-PRT-EDTIME-MM  DELIMITED   BY  SIZE
                       INTO   WRK-PRT-SRYYMD
                       END-STRING
                   ELSE
                       STRING   WRK-PRT-STYMD      DELIMITED   BY  SIZE
                                WRK-PRT-STTIME-HH  DELIMITED   BY  SIZE
                                "："               DELIMITED   BY  SIZE
                                WRK-PRT-STTIME-MM  DELIMITED   BY  SIZE
                                "〜"               DELIMITED   BY  SIZE
                                WRK-PRT-EDYMD      DELIMITED   BY  SPACE
                                WRK-PRT-EDTIME-HH  DELIMITED   BY  SIZE
                                "："               DELIMITED   BY  SIZE
                                WRK-PRT-EDTIME-MM  DELIMITED   BY  SIZE
                       INTO   WRK-PRT-SRYYMD
                       END-STRING
                   END-IF
      *
               ELSE
                   MOVE    "締め処理にて対象期間を登録してください"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *
           MOVE    WRK-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-PRT-SAKYMD
      *
           IF    ( SBD010-NYUGAIKBN    =   "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  SBD010-NYUGAIKBN
           END-IF
      *
           IF    ( SBD010-SUMKBN       =   "1" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  SBD010-SUMKBN
           END-IF
      *
           IF    ( SBD010-MEISAIKBN    =   "1" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  SBD010-MEISAIKBN
           END-IF
      *
           IF    ( SBD010-TOTALPRTKBN  =   "1" )
               CONTINUE
           ELSE
               MOVE    "0"             TO  SBD010-TOTALPRTKBN
           END-IF
      *
           PERFORM 1001-SET-SRH-1005-SEC
      *
           PERFORM 1001-SET-SRH-1041-SEC
      *
           PERFORM 1001-SET-SRH-1013-SEC
      *
           PERFORM 1001-SET-SKYTTL-SEC
      *
      *    初診／再診判定サブ初期処理
           INITIALIZE                      SS003-AREA
           MOVE    ZERO            TO      SS003-SYORIKBN
           CALL    "ORCSS003"      USING   SS003-AREA
                                           SPA-AREA
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
           IF      WRK-STDAILYKEY-YMD  =   WRK-EDDAILYKEY-YMD
               MOVE    WRK-STDAILYKEY-YMD  TO  WRK-TO-DATA-YMD
               MOVE    WRK-TO-DATA1    TO  WRK-TO-DATA-KIKAN
           ELSE
               MOVE    WRK-STDAILYKEY-YMD  TO  WRK-TO-DATA-STRYMD
               MOVE    WRK-EDDAILYKEY-YMD  TO  WRK-TO-DATA-ENDYMD
               MOVE    WRK-TO-DATA2    TO  WRK-TO-DATA-KIKAN
           END-IF
      *
           PERFORM 100-OPTION-SELECT-SEC
      *
           IF    ( TPARA-FORM      =   ZERO )
               MOVE    CONST-LINE-FORM0
                                   TO  WRK-LINE-MAX
           ELSE
               MOVE    CONST-LINE-FORM1
                                   TO  WRK-LINE-MAX
           END-IF
      *
           IF    ( SBD010-SIMEKBN      =   ZERO )
               MOVE    WRK-STDAILYKEY-YMD  TO  WRK-SYMD
               PERFORM 800-CSV-DATE-SEC
               MOVE    WRK-CSV-DATE        TO  WRK-CSV-STSRYYMD
      *
               IF    ( SBD010-EDSRYYMD     NOT =   SPACE )
      *
                   MOVE    "〜"            TO  WRK-CSV-DASH
      *
                   MOVE    WRK-EDDAILYKEY-YMD  TO  WRK-SYMD
                   PERFORM 800-CSV-DATE-SEC
                   MOVE    WRK-CSV-DATE        TO  WRK-CSV-EDSRYYMD
               END-IF
      *
           ELSE
               MOVE    WRK-STDAILYKEY-YMD  TO  WRK-SYMD
               PERFORM 800-CSV-DATE-SEC
               MOVE    WRK-CSV-DATE        TO  WRK-CSV-STSRYYMD
      *
               MOVE    WRK-STDAILYKEY-TIME TO  WRK-TIME
               PERFORM 800-CSV-TIME-SEC
               MOVE    WRK-CSV-TIME        TO  WRK-CSV-STTIME
      *
               MOVE    "〜"                TO  WRK-CSV-DASH
      *
               MOVE    WRK-EDDAILYKEY-YMD  TO  WRK-SYMD
               PERFORM 800-CSV-DATE-SEC
               MOVE    WRK-CSV-DATE        TO  WRK-CSV-EDSRYYMD
      *
               MOVE    WRK-EDDAILYKEY-TIME TO  WRK-TIME
               PERFORM 800-CSV-TIME-SEC
               MOVE    WRK-CSV-TIME        TO  WRK-CSV-EDTIME
           END-IF
      *
           MOVE    WRK-SYSYMD          TO  WRK-SYMD
           PERFORM 800-CSV-DATE-SEC
           MOVE    WRK-CSV-DATE        TO  WRK-CSV-SAKYMD
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  TPARA-AREA
           MOVE    ZERO            TO  TPARA-FORM
           MOVE    "1"             TO  TPARA-SHADE
      *
      *    帳票様式
           MOVE    "FORM"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-FORM
           END-IF
      *
      *    網掛け
           MOVE    "SHADE"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-SHADE
           END-IF
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避処理
      *****************************************************************
       1001-SET-SRH-1005-SEC       SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    WRK-STDAILYKEY-YMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                     OR  ( SRH-1005-MAX       >=   CONST-1005-MAX )
      *
               COMPUTE SRH-1005-MAX    =   SRH-1005-MAX  + 1
               MOVE    SYS-1005-KBNCD  TO  SRH-1005-CD (SRH-1005-MAX)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SRH-1005-SRYKANAME1
                                                       (SRH-1005-MAX)
               MOVE    SYS-1005-SRYKANAME3
                                       TO  SRH-1005-SRYKANAME3
                                                       (SRH-1005-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SET-SRH-1005-EXT.
           EXIT.
      *****************************************************************
      *    入金方法退避処理
      *****************************************************************
       1001-SET-SRH-1041-SEC       SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1041"              TO  SYS-KANRICD
           MOVE    WRK-STDAILYKEY-YMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                     OR  ( SRH-1041-MAX       >=   CONST-1041-MAX )
      *
               COMPUTE SRH-1041-MAX    =   SRH-1041-MAX  + 1
               MOVE    SYS-1041-KBNCD  TO  SRH-1041-CD (SRH-1041-MAX)
               MOVE    SYS-1041-NYKN-TANNAME
                                           TO  WRK-KANACHK-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SRH-1041-NYKN-TANNAME
                                                       (SRH-1041-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SET-SRH-1041-EXT.
           EXIT.
      *****************************************************************
      *    自費名称退避処理
      *****************************************************************
       1001-SET-SRH-1013-SEC       SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1013"              TO  SYS-KANRICD
           MOVE    WRK-STDAILYKEY-YMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1013-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    ZERO        TO  IDX1
                                       IDX2
      *
               IF    ( SYS-1013-KBNCD(1:1)  =   "1" )
                   MOVE    1       TO  IDX1
               END-IF
               IF    ( SYS-1013-KBNCD(1:1)  =   "2" )
                   MOVE    2       TO  IDX1
               END-IF
      *
               EVALUATE    SYS-1013-KBNCD(2:1)
               WHEN    "0"
                   MOVE    10                  TO  IDX2
               WHEN    "1"     THRU    "9"
                   MOVE    SYS-1013-KBNCD(2:1) TO  WRK-9-X
                   MOVE    WRK-9               TO  IDX2
               END-EVALUATE
      *
               IF    ( IDX1        >   ZERO )
                AND  ( IDX2        >   ZERO )
                   MOVE    SYS-1013-JIHINAME   TO  SRH-1013-JIHINAME
                                                             (IDX1 IDX2)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1013-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SET-SRH-1013-EXT.
           EXIT.
      *****************************************************************
      *    請求合計見出し・振り分け先編集処理
      *****************************************************************
       1001-SET-SKYTTL-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-EDDAILYKEY-YMD      >=  CONST-H200401 )
      *
               MOVE    1                   TO  IDX-SYAHON
               MOVE    2                   TO  IDX-SYAKZK
               MOVE    3                   TO  IDX-KOKIPN
               MOVE    4                   TO  IDX-KOUKI
                                               IDX-SYARJN
                                               IDX-KOKRJN
               MOVE    5                   TO  IDX-KOHTAN
               MOVE    6                   TO  IDX-JIHHKN
               MOVE    7                   TO  IDX-ROUSAI
               MOVE    8                   TO  IDX-JIBAI 
               MOVE    9                   TO  IDX-KOUGAI
               MOVE    10                  TO  IDX-CHIKEN
      *
               MOVE    9                   TO  WRK-SKYTTL-MAX
      *
               MOVE    "社保本人"          TO  WRK-SKYTTLMSG(IDX-SYAHON)
               MOVE    "社保家族"          TO  WRK-SKYTTLMSG(IDX-SYAKZK)
               MOVE    "国　保"            TO  WRK-SKYTTLMSG(IDX-KOKIPN)
               MOVE    "後期高齢者"        TO  WRK-SKYTTLMSG(IDX-KOUKI)
               MOVE    "公費単独"          TO  WRK-SKYTTLMSG(IDX-KOHTAN)
               MOVE    "自　費"            TO  WRK-SKYTTLMSG(IDX-JIHHKN)
               MOVE    "労　災"            TO  WRK-SKYTTLMSG(IDX-ROUSAI)
               MOVE    "自賠責"            TO  WRK-SKYTTLMSG(IDX-JIBAI)
               MOVE    "公　害"            TO  WRK-SKYTTLMSG(IDX-KOUGAI)
               MOVE    "治　験"            TO  WRK-SKYTTLMSG(IDX-CHIKEN)
           WHEN  OTHER
      *
               MOVE    1                   TO  IDX-SYAHON
               MOVE    2                   TO  IDX-SYAKZK
               MOVE    3                   TO  IDX-SYARJN
               MOVE    4                   TO  IDX-KOKIPN
               MOVE    5                   TO  IDX-KOKRJN
               MOVE    6                   TO  IDX-KOHTAN
               MOVE    7                   TO  IDX-JIHHKN
               MOVE    8                   TO  IDX-ROUSAI
               MOVE    9                   TO  IDX-JIBAI 
               MOVE    10                  TO  IDX-KOUGAI
               MOVE    11                  TO  IDX-CHIKEN
      *
               MOVE    10                  TO  WRK-SKYTTL-MAX
      *
               MOVE    "社保本人"          TO  WRK-SKYTTLMSG(IDX-SYAHON)
               MOVE    "社保家族"          TO  WRK-SKYTTLMSG(IDX-SYAKZK)
               MOVE    "社保老人"          TO  WRK-SKYTTLMSG(IDX-SYARJN)
               MOVE    "国保一般"          TO  WRK-SKYTTLMSG(IDX-KOKIPN)
               MOVE    "国保老人"          TO  WRK-SKYTTLMSG(IDX-KOKRJN)
               MOVE    "公費単独"          TO  WRK-SKYTTLMSG(IDX-KOHTAN)
               MOVE    "自　費"            TO  WRK-SKYTTLMSG(IDX-JIHHKN)
               MOVE    "労　災"            TO  WRK-SKYTTLMSG(IDX-ROUSAI)
               MOVE    "自賠責"            TO  WRK-SKYTTLMSG(IDX-JIBAI)
               MOVE    "公　害"            TO  WRK-SKYTTLMSG(IDX-KOUGAI)
               MOVE    "治　験"            TO  WRK-SKYTTLMSG(IDX-CHIKEN)
           END-EVALUATE
      *
           .
       1001-SET-SKYTTL-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           IF    ( SBD010-SIMEKBN      =   ZERO )
               MOVE    "ORCBD010"      TO  ORCSPRGOPTION-PRGID
           ELSE
               MOVE    "ORCBD010V02"   TO  ORCSPRGOPTION-PRGID
           END-IF
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
      *
           EVALUATE    WRK-PARA-NAME
           WHEN    "FORM"
               MOVE    ORCSPRGOPTION-OT-PARA (1:1)
                                       TO  KANACHK-MAE-INPUT
           WHEN    OTHER
               MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  KANACHK-MAE-INPUT
           END-EVALUATE
      *
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
     *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-EDIT-SYUNOU-SEC
      *
           PERFORM 2001-EDIT-FORM-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       2001-EDIT-SYUNOU-SEC        SECTION.
      *
           OPEN    OUTPUT          BD01003-FILE
           CLOSE   BD01003-FILE
           OPEN    I-O             BD01003-FILE
      *
           OPEN    OUTPUT          BD01004-FILE
      *
           PERFORM 900-SYUMEI-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
      *
               MOVE    SMEI-NYUGAIKBN  TO  KEY-NYUGAIKBN
               MOVE    SMEI-PTID       TO  KEY-PTID
               MOVE    SMEI-DENPNUM    TO  KEY-DENPNUM
      *
               MOVE    ZERO            TO  CNT-SMEI
      *
               PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYUMEI      NOT =   ZERO )
                        OR   ( KEY-PTID        NOT =   SMEI-PTID )
                        OR   ( KEY-NYUGAIKBN   NOT =   SMEI-NYUGAIKBN )
                        OR   ( KEY-DENPNUM     NOT =   SMEI-DENPNUM )
      *
                   COMPUTE CNT-SMEI    =   CNT-SMEI    +   1
      *
                   IF    ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                    AND  ( CNT-SMEI                =   1 )
                       PERFORM 20011-EDIT-SYUNOU-SEC
                   END-IF
      *
                   PERFORM 900-SYUMEI-KEY17-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key17"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           CLOSE   BD01003-FILE
                   BD01004-FILE
      *
           .
       2001-EDIT-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       20011-EDIT-SYUNOU-SEC       SECTION.
      *
           INITIALIZE                  SBD010S02-AREA
           MOVE    WRK-STDAILYKEY  TO  SBD010S02-STDAILYKEY
           MOVE    WRK-EDDAILYKEY  TO  SBD010S02-EDDAILYKEY
           CALL    "ORCBD010S02"   USING
                                   SYUNOU-REC
                                   SBD010-AREA
                                   SBD010S02-AREA
                                   SPA-AREA
      *
           PERFORM VARYING IDX-S02 FROM    1   BY  1
                   UNTIL ( IDX-S02 >   SBD010S02-MAX )
      *
               MOVE    ZERO        TO  WRK-SYOSAIKBN
               IF    ( SYU-NYUGAIKBN       =   "1" )
                   IF    ( SYU-CREATEKBN   =   "1" )
                       MOVE    "3" TO  WRK-SYOSAIKBN
                   ELSE
                       MOVE    "4" TO  WRK-SYOSAIKBN
                   END-IF
               ELSE
                   IF    ( SBD010S02-LAST  (IDX-S02)       =   1 )
                       IF    ( SBD010S02-MIN-DENPEDANUM    =   1 )
                        AND  ( SYU-SRYYMD  >=  WRK-STDAILYKEY-YMD )
                        AND  ( SYU-SRYYMD  <=  WRK-EDDAILYKEY-YMD )
                           PERFORM 800-CALL-ORCSS003-SEC
                           MOVE    SS003-SYOSAISIN-KBN
                                       TO  WRK-SYOSAIKBN
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 2001-EDIT-BD01003-SEC
      *
               PERFORM 2001-EDIT-BD01004-SEC
      *
           END-PERFORM
      *
           .
       20011-EDIT-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル出力処理
      *****************************************************************
       2001-EDIT-BD01003-SEC           SECTION.
      *
           INITIALIZE                      BD01003-REC
           MOVE    SYU-NYUGAIKBN       TO  BD01003-SRT-NYUGAIKBN
           MOVE    SBD010S02-KEYSRYKA (IDX-S02)
                                       TO  BD01003-SRT-SRYKA
      *
           PERFORM 900-BD01003-READ-SEC
           IF    ( FLG-BD01003         =   ZERO )
               PERFORM 20011-EDIT-BD01003-SEC
               REWRITE BD01003-REC
           ELSE
               INITIALIZE                  BD01003-REC
               MOVE    SYU-NYUGAIKBN   TO  BD01003-SRT-NYUGAIKBN
               MOVE    SBD010S02-KEYSRYKA (IDX-S02)
                                       TO  BD01003-SRT-SRYKA
               PERFORM 20011-EDIT-BD01003-SEC
               WRITE   BD01003-REC
           END-IF
      *
           .
       2001-EDIT-BD01003-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル出力処理
      *****************************************************************
       20011-EDIT-BD01003-SEC          SECTION.
      *
           MOVE    SBD010S02-HOKEN (IDX-S02)
                                       TO  IDX-HOKEN
      *
           IF    ( IDX-HOKEN           =   IDX-CHIKEN )
               MOVE    IDX-JIHHKN      TO  IDX-HOKEN
           END-IF
      *
           COMPUTE BD01003-TENSU-TOTAL
               =   BD01003-TENSU-TOTAL
               +   SBD010S02-HKNTEN (IDX-S02)
      *
           COMPUTE BD01003-TENSU (IDX-HOKEN)
               =   BD01003-TENSU (IDX-HOKEN)
               +   SBD010S02-HKNTEN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLSEIKYUREL)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLSEIKYUREL)
               +   SBD010S02-SEIKYU (IDX-S02)
               -   SBD010S02-GENMEN (IDX-S02)
               -   SBD010S02-CHOSEI (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLSEIKYUREL IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLSEIKYUREL IDX-HOKEN)
               +   SBD010S02-HKNSEIKYU (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLSEIKYUREL IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLSEIKYUREL IDX-JIHHKN)
               +   SBD010S02-JIHSEIKYU (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLGENMEN)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLGENMEN)
               +   SBD010S02-GENMEN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLGENMEN IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLGENMEN IDX-HOKEN)
               +   SBD010S02-HKNGENMEN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLGENMEN IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLGENMEN IDX-JIHHKN)
               +   SBD010S02-JIHGENMEN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLSEIKYU)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLSEIKYU)
               +   SBD010S02-SEIKYU (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLSEIKYU IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLSEIKYU IDX-HOKEN)
               +   SBD010S02-HKNSEIKYU (IDX-S02)
               +   SBD010S02-HKNGENMEN (IDX-S02)
               +   SBD010S02-HKNCHOSEI (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLSEIKYU IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLSEIKYU IDX-JIHHKN)
               +   SBD010S02-JIHSEIKYU (IDX-S02)
               +   SBD010S02-JIHGENMEN (IDX-S02)
               +   SBD010S02-JIHCHOSEI (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLCHOSEI1)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLCHOSEI1)
               +   SBD010S02-CHOSEI1 (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLCHOSEI2)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLCHOSEI2)
               +   SBD010S02-CHOSEI2 (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLCHOSEI1 IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLCHOSEI1 IDX-HOKEN)
               +   SBD010S02-HKNCHOSEI1 (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLCHOSEI2 IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLCHOSEI2 IDX-HOKEN)
               +   SBD010S02-HKNCHOSEI2 (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLCHOSEI1 IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLCHOSEI1 IDX-JIHHKN)
               +   SBD010S02-JIHCHOSEI1 (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLCHOSEI2 IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLCHOSEI2 IDX-JIHHKN)
               +   SBD010S02-JIHCHOSEI2 (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLNYUKIN)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLNYUKIN)
               +   SBD010S02-NYUKIN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLNYUKIN IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLNYUKIN IDX-HOKEN)
               +   SBD010S02-HKNNYUKIN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLNYUKIN IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLNYUKIN IDX-JIHHKN)
               +   SBD010S02-JIHNYUKIN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLKONNYUKIN)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLKONNYUKIN)
               +   SBD010S02-KONNYUKIN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLKONNYUKIN IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLKONNYUKIN IDX-HOKEN)
               +   SBD010S02-KONHKNNYUKIN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLKONNYUKIN IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLKONNYUKIN IDX-JIHHKN)
               +   SBD010S02-KONJIHNYUKIN (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLMISYU)
               =   BD01003-SEIKYU-TOTAL (CONST-TTLMISYU)
               +   SBD010S02-KONMISYU (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLMISYU IDX-HOKEN)
               =   BD01003-SEIKYU (CONST-TTLMISYU IDX-HOKEN)
               +   SBD010S02-KONHKNMISYU (IDX-S02)
      *
           COMPUTE BD01003-SEIKYU (CONST-TTLMISYU IDX-JIHHKN)
               =   BD01003-SEIKYU (CONST-TTLMISYU IDX-JIHHKN)
               +   SBD010S02-KONJIHMISYU (IDX-S02)
      *
           EVALUATE    WRK-SYOSAIKBN
           WHEN    "1"
               COMPUTE BD01003-SEIKYU (CONST-TTLSYOSIN IDX-HOKEN)
                   =   BD01003-SEIKYU (CONST-TTLSYOSIN IDX-HOKEN)
                   +   1
               COMPUTE BD01003-SEIKYU (CONST-TTLSYOSAI IDX-HOKEN)
                   =   BD01003-SEIKYU (CONST-TTLSYOSAI IDX-HOKEN)
                   +   1
      *
               COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLSYOSIN)
                   =   BD01003-SEIKYU-TOTAL (CONST-TTLSYOSIN)
                   +   1
      *
               COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLSYOSAI)
               =       BD01003-SEIKYU-TOTAL (CONST-TTLSYOSAI)
                   +   1
           WHEN    "2"
               COMPUTE BD01003-SEIKYU (CONST-TTLSAISIN IDX-HOKEN)
                   =   BD01003-SEIKYU (CONST-TTLSAISIN IDX-HOKEN)
                   +   1
               COMPUTE BD01003-SEIKYU (CONST-TTLSYOSAI IDX-HOKEN)
                   =   BD01003-SEIKYU (CONST-TTLSYOSAI IDX-HOKEN)
                   +   1
      *
               COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLSAISIN)
                   =   BD01003-SEIKYU-TOTAL (CONST-TTLSAISIN)
                   +   1
      *
               COMPUTE BD01003-SEIKYU-TOTAL (CONST-TTLSYOSAI)
               =       BD01003-SEIKYU-TOTAL (CONST-TTLSYOSAI)
                   +   1
           END-EVALUATE
      *
           MOVE    SBD010S02-HOHO-MAX (IDX-S02)
                                       TO  WRK-HS02-MAX
           PERFORM VARYING IDX-HS02    FROM    1   BY  1
                   UNTIL ( IDX-HS02    >   WRK-HS02-MAX )
      *
               MOVE    11              TO  IDX-HOHO
      *
               EVALUATE    SBD010S02-HOHO (IDX-S02 IDX-HS02)
               WHEN    CONST-HOHO-HENKIN
                   MOVE    12          TO  IDX-HOHO
               WHEN    CONST-HOHO-TORIKESI
                   MOVE    13          TO  IDX-HOHO
               WHEN    OTHER
                   MOVE    SBD010S02-HOHO (IDX-S02 IDX-HS02)
                                       TO  SRH-CD
                   PERFORM 250-GET-1041-SEC
                   IF    ( SRH-IDX     >=  1 )
                    AND  ( SRH-IDX     <=  10 )
                       MOVE    SRH-IDX TO  IDX-HOHO
                   END-IF
               END-EVALUATE
      *
               COMPUTE BD01003-NYUKIN (IDX-HOHO)
                   =   BD01003-NYUKIN (IDX-HOHO)
                   +   SBD010S02-HOHOMONEY (IDX-S02 IDX-HS02)
      *
           END-PERFORM
      *
           COMPUTE BD01003-NYUKIN-TOTAL
               =   BD01003-NYUKIN-TOTAL
               +   SBD010S02-NYUKIN (IDX-S02)
      *
           PERFORM VARYING IDX-JIHI    FROM    1   BY  1
                   UNTIL ( IDX-JIHI    >   CONST-ETCJIHI-MAX )
      *
               COMPUTE BD01003-JIHI (IDX-JIHI)
                   =   BD01003-JIHI (IDX-JIHI)
                   +   SBD010S02-ETCJIHI (IDX-S02 IDX-JIHI)
      *
               COMPUTE BD01003-JIHI-TOTAL
                   =   BD01003-JIHI-TOTAL
                   +   SBD010S02-ETCJIHI (IDX-S02 IDX-JIHI)
           END-PERFORM
      *
           .
       20011-EDIT-BD01003-EXT.
           EXIT.
      *****************************************************************
      *   帳票元データ編集処理
      *****************************************************************
       2001-EDIT-BD01004-SEC           SECTION.
      *
           PERFORM 20011-EDIT-BD01004-COMMON-SEC
      *
           MOVE    SBD010S02-HKNTEN (IDX-S02)
                                       TO  BD01004-HOKENTEN
           MOVE    SBD010S02-SEIKYU (IDX-S02)
                                       TO  BD01004-SKYMONEY
           MOVE    SBD010S02-HKNSEIKYU (IDX-S02)
                                       TO  BD01004-FTNMONEY
           MOVE    SBD010S02-JIHSEIKYU (IDX-S02)
                                       TO  BD01004-JIHIMONEY
           MOVE    SBD010S02-CHOSEI (IDX-S02)
                                       TO  BD01004-CHOSEI
           MOVE    SBD010S02-CHOSEI1 (IDX-S02)
                                       TO  BD01004-CHOSEI1
           MOVE    SBD010S02-CHOSEI2 (IDX-S02)
                                       TO  BD01004-CHOSEI2
           MOVE    SBD010S02-KONNYUKIN (IDX-S02)
                                       TO  BD01004-KONNYUKIN
           MOVE    SBD010S02-GENMEN (IDX-S02)
                                       TO  BD01004-GENMEN
           MOVE    SBD010S02-ZENMISYU(IDX-S02)
                                       TO  BD01004-ZENMISYU
           PERFORM VARYING IDX-JIHI    FROM    1   BY  1
                   UNTIL ( IDX-JIHI    >   CONST-ETCJIHI-MAX )
               MOVE    SBD010S02-ETCJIHI (IDX-S02 IDX-JIHI)
                                       TO  BD01004-ETCJIHI (IDX-JIHI)
           END-PERFORM
           MOVE    WRK-SYOSAIKBN       TO  BD01004-SYOSAIKBN
      *
           MOVE    SBD010S02-HOHO-MAX (IDX-S02)
                                       TO  WRK-HS02-MAX
      *
           IF    ( WRK-HS02-MAX        >   ZERO )
      *
               PERFORM VARYING IDX-HS02    FROM    1   BY  1
                       UNTIL ( IDX-HS02    >   WRK-HS02-MAX )
      *
                   IF    ( IDX-HS02    >   1 )
                       PERFORM 20011-EDIT-BD01004-COMMON-SEC
                       MOVE    "1"     TO  BD01004-SKYNYUKBN
                   END-IF
                   MOVE    IDX-HS02    TO  BD01004-RENNUM2
      *
                   MOVE    SBD010S02-HOHO (IDX-S02 IDX-HS02)
                                       TO  BD01004-HOHO
                   MOVE    SBD010S02-HOHOMONEY (IDX-S02 IDX-HS02)
                                       TO  BD01004-NYUKIN
                   IF    ( IDX-HS02    =   WRK-HS02-MAX )
                       MOVE    "1"     TO  BD01004-DSPMISYUKBN
                       MOVE    SBD010S02-KONMISYU (IDX-S02)
                                       TO  BD01004-MISYU
                       MOVE    SBD010S02-MISYU (IDX-S02)
                                       TO  BD01004-TTLMISYU
                   END-IF
      *
                   WRITE   BD01004-REC
      *
               END-PERFORM
      *
           ELSE
               MOVE    1               TO  BD01004-RENNUM2
               MOVE    "1"             TO  BD01004-DSPMISYUKBN
               MOVE    SBD010S02-KONMISYU (IDX-S02)
                                       TO  BD01004-MISYU
               MOVE    SBD010S02-MISYU (IDX-S02)
                                       TO  BD01004-TTLMISYU
               WRITE   BD01004-REC
           END-IF
      *
           .
       2001-EDIT-BD01004-EXT.
           EXIT.
      *****************************************************************
      *   帳票元データ編集処理（請求・入金共通項目）
      *****************************************************************
       20011-EDIT-BD01004-COMMON-SEC   SECTION.
      *
           INITIALIZE                  BD01004-REC
           MOVE    SYU-NYUGAIKBN       TO  BD01004-SRT-NYUGAIKBN
           MOVE    SBD010S02-KEYSRYKA (IDX-S02)
                                       TO  BD01004-SRT-SRYKA
           MOVE    SYU-DENPNUM         TO  BD01004-DENPNUM
           MOVE    SBD010S02-DENPEDANUM (IDX-S02)
                                       TO  BD01004-DENPEDANUM
           MOVE    SBD010S02-SYURRKNUM  (IDX-S02)
                                       TO  BD01004-SYURRKNUM
           MOVE    SBD010S02-SYUEDANUM  (IDX-S02)
                                       TO  BD01004-SYUEDANUM
           MOVE    SBD010S02-RENNUM     (IDX-S02)
                                       TO  BD01004-RENNUM
           MOVE    SYU-PTID            TO  BD01004-PTID
           MOVE    PTNUM-PTNUM         TO  BD01004-PTNUM
           MOVE    PTINF-NAME          TO  BD01004-NAME
           MOVE    PTINF-KANANAME      TO  BD01004-KANANAME
           MOVE    SYU-SRYYMD          TO  BD01004-SRYYMD
           MOVE    SYU-DENPPRTYMD      TO  BD01004-DENPPRTYMD
           MOVE    SBD010S02-HOKEN (IDX-S02)
                                       TO  BD01004-HOKEN
           MOVE    SBD010S02-SRYKA (IDX-S02)
                                       TO  BD01004-SRYKA
           MOVE    ZERO                TO  BD01004-SKYNYUKBN
      *
           .
       20011-EDIT-BD01004-COMMON-EXT.
           EXIT.
      *****************************************************************
      *   帳票編集処理
      *****************************************************************
       2001-EDIT-FORM-SEC          SECTION.
      *
           OPEN    INPUT   BD01004-FILE
                           BD01003-FILE
      *
           PERFORM 900-BD01004-READ-SEC
      *
      *     IF      FLG-BD01004     =   ZERO

      *     END-IF
           PERFORM UNTIL ( FLG-BD01004 NOT =   ZERO )
      *
               MOVE    BD01004-SRT-NYUGAIKBN    TO  KEY-NYUGAIKBN
               MOVE    BD01004-SRT-SRYKA        TO  KEY-SRYKA
               MOVE    ZERO                     TO  CNT-NO
               INITIALIZE                           TTL-AREA
      *
      *        ＣＳＶ出力処理
               IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
                   MOVE    "（入院）"      TO  CSV-NYUGAI
                   MOVE    "伝票発行日"    TO  CSV-SRYYMDMEI
                   MOVE    "退定"          TO  CSV-SYOSAIMEI
               ELSE
                   MOVE    "（外来）"      TO  CSV-NYUGAI
                   MOVE    "診療年月日"    TO  CSV-SRYYMDMEI
                   MOVE    "初再"          TO  CSV-SYOSAIMEI
               END-IF
               MOVE    WRK-CSV-STSRYYMD    TO  CSV-STHSRYYMD
               MOVE    WRK-CSV-STTIME      TO  CSV-STTIME
               MOVE    WRK-CSV-DASH        TO  CSV-DASH
               MOVE    WRK-CSV-EDSRYYMD    TO  CSV-EDHSRYYMD
               MOVE    WRK-CSV-EDTIME      TO  CSV-EDTIME
               MOVE    WRK-CSV-SAKYMD      TO  CSV-SAKYMD
               IF    ( SBD010-SUMKBN   =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    BD01004-SRYKA   TO  SRH-CD
                   PERFORM 250-GET-1005-SEC
                   MOVE    SRH-ITM         TO  CSV-SRYKA
               END-IF
               MOVE    CSV-HEAD-01A    TO   WRK-SOURCE
               MOVE    " *, *"         TO   WRK-PATTERN
               MOVE    ","             TO   WRK-REPLACEMENT
               MOVE    "g"             TO   WRK-FLAGS
               PERFORM 800-REGEXP-REPLACE-SEC
               MOVE    WRK-SOURCE      TO   WRK-REC
               PERFORM 500-TOUKEICSV-SEC
      *
               INITIALIZE                   WRK-REC
               IF    ( TPARA-FORM      =   ZERO )
                   STRING  CSV-HEAD-01B    DELIMITED BY SIZE
                           CSV-HEAD-011    DELIMITED BY SIZE
                                           INTO  WRK-REC
                   END-STRING
               ELSE
                   STRING  CSV-HEAD-01B    DELIMITED BY SIZE
                           CSV-HEAD-012    DELIMITED BY SIZE
                                           INTO  WRK-REC
                   END-STRING
               END-IF
               PERFORM 500-TOUKEICSV-SEC
               PERFORM UNTIL ( FLG-BD01004 NOT =   ZERO )
                        OR   ( BD01004-SRT-NYUGAIKBN
                                           NOT =   KEY-NYUGAIKBN )
                        OR   ( BD01004-SRT-SRYKA
                                           NOT =   KEY-SRYKA )
      *
                   IF    ( TPARA-FORM      =   ZERO )
                       PERFORM 20011-EDIT-FORM-SEC
                   ELSE
                       PERFORM 20012-EDIT-FORM1-SEC
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SBD010-TOTALPRTKBN  =   "1" )
                   PERFORM 20011-EDIT-TTLFORM-SEC
               END-IF
      *
           END-PERFORM
      *
           CLOSE           BD01004-FILE
                           BD01003-FILE
      *
           .
       2001-EDIT-FORM-EXT.
           EXIT.
      *****************************************************************
      *   帳票編集処理
      *****************************************************************
       20011-EDIT-FORM-SEC          SECTION.
      *
           PERFORM 210-HEAD-SEC
      *
      *
           PERFORM UNTIL ( FLG-BD01004           NOT = ZERO )
                    OR   ( BD01004-SRT-NYUGAIKBN NOT = KEY-NYUGAIKBN  )
                    OR   ( BD01004-SRT-SRYKA     NOT = KEY-SRYKA )
                    OR   ( CNT-LINE                 >= WRK-LINE-MAX )
      *
               PERFORM 220-BODY-SEC
      *
               PERFORM 220-SUM-BODY-TOTAL-SEC
      *
               PERFORM 900-BD01004-READ-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-BD01004           NOT = ZERO )
            OR   ( BD01004-SRT-NYUGAIKBN NOT = KEY-NYUGAIKBN  )
            OR   ( BD01004-SRT-SRYKA     NOT = KEY-SRYKA )
      *
               PERFORM 2201-BODY-TOTAL-SEC
           END-IF
      *
           PERFORM 230-PRINT-SEC
      *
           .
       20011-EDIT-FORM-EXT.
           EXIT.
      *****************************************************************
      *   見出し編集処理
      *****************************************************************
       210-HEAD-SEC                SECTION.
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  HCM51
      *
           IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
               MOVE    "（入院）"      TO  HCM51-NYUGAI
           ELSE
               MOVE    "（外来）"      TO  HCM51-NYUGAI
           END-IF
      *
           MOVE    WRK-PRT-SRYYMD      TO  HCM51-HSRYYMD
      *
           IF    ( SBD010-SUMKBN   =   ZERO )
               CONTINUE
           ELSE
               MOVE    BD01004-SRYKA   TO  SRH-CD
               PERFORM 250-GET-1005-SEC
               MOVE    SRH-ITM         TO  HCM51-HSRYKA
           END-IF
      *
           MOVE    WRK-PRT-SAKYMD  TO  HCM51-SAKYMD
      *
           MOVE    CNT-PAGE        TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9       TO  HCM51-PAGE
      *
           IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
               MOVE    "伝票発行日"    TO  HCM51-SRYYMDMSG
               MOVE    "退定"          TO  HCM51-SYOSAIMSG
           ELSE
               MOVE    "診療年月日"    TO  HCM51-SRYYMDMSG
               MOVE    "初再"          TO  HCM51-SYOSAIMSG
           END-IF
      *
           IF    ( TPARA-SHADE         =   ZERO )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L shade "
                                           DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
                                           INTO    HCM51-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L line "
                                           DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
                                           INTO    HCM51-LOPTION
               END-STRING
           END-IF
      *
           .
       210-HEAD-EXT.
           EXIT.
      *****************************************************************
      *   明細編集処理
      *****************************************************************
       220-BODY-SEC                SECTION.
      *
           INITIALIZE              CSV-AREA
           COMPUTE CNT-LINE    =   CNT-LINE   +   1
      *
           IF    ( BD01004-RENNUM  =   1 )
            AND  ( BD01004-RENNUM2 =   1 )
               COMPUTE CNT-NO  =   CNT-NO     +   1
           END-IF
      *
           IF    ( CNT-LINE        =   1 )
            OR  (( BD01004-RENNUM  =   1 )
              AND( BD01004-RENNUM2 =   1 ))
               MOVE    CNT-NO          TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  HCM51-NO (CNT-LINE)
               MOVE    BD01004-DENPNUM     TO  HCM51-DENPNUM (CNT-LINE)
               IF    ( BD01004-PTNUM (11:)    =   SPACE )
                   MOVE    BD01004-PTNUM   TO  HCM51-PTNUM (CNT-LINE)
               ELSE
                   MOVE    BD01004-PTNUM   TO  HCM51-PTNUM2(CNT-LINE)
               END-IF
               MOVE    BD01004-NAME        TO  HCM51-NAME  (CNT-LINE)
               MOVE    HCM51-NAME  (CNT-LINE)
                                           TO  WRK-KANACHK-INPUT
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  HCM51-NAME  (CNT-LINE)
      *
               IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
                   IF    ( BD01004-DENPPRTYMD
                                           NOT =   WRK-EDDAILYKEY-YMD )
                       MOVE    BD01004-DENPPRTYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD1 TO  HCM51-SRYYMD(CNT-LINE)
                       MOVE    BD01004-DENPPRTYMD  TO  WRK-SYMD
                       MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
                       MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
                       MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
                       MOVE    WRK-HEN-YMD-OT  TO  CSV-SRYYMD
                   END-IF
               ELSE
                   IF    ( BD01004-SRYYMD  NOT =   WRK-EDDAILYKEY-YMD )
                       MOVE    BD01004-SRYYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD1 TO  HCM51-SRYYMD(CNT-LINE)
                       MOVE    BD01004-SRYYMD  TO  WRK-SYMD
                       MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
                       MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
                       MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
                       MOVE    WRK-HEN-YMD-OT  TO  CSV-SRYYMD
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    BD01004-SYOSAIKBN
           WHEN    "1"
               MOVE    "初診"          TO  HCM51-SYOSAI(CNT-LINE)
           WHEN    "2"
               MOVE    "再診"          TO  HCM51-SYOSAI(CNT-LINE)
           WHEN    "3"
               MOVE    "退院"          TO  HCM51-SYOSAI(CNT-LINE)
           WHEN    "4"
               MOVE    "定期"          TO  HCM51-SYOSAI(CNT-LINE)
           END-EVALUATE
      *
      *
           MOVE    BD01004-SRYKA   TO  SRH-CD
           PERFORM 250-GET-1005-SEC
           MOVE    SRH-ITM2                TO  HCM51-SRYKA (CNT-LINE)
      *
           MOVE    WRK-SKYTTLMSG (BD01004-HOKEN)
                                           TO  HCM51-HOKEN (CNT-LINE)
      *
           IF    ( BD01004-SKYNYUKBN       =   ZERO )
               MOVE    BD01004-HOKENTEN    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-HKNTEN(CNT-LINE)
               MOVE    BD01004-SKYMONEY    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-SKYMONEY(CNT-LINE)
               MOVE    BD01004-FTNMONEY    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-SKYHOKEN(CNT-LINE)
               MOVE    BD01004-JIHIMONEY   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-SKYJIHI(CNT-LINE)
               MOVE    BD01004-CHOSEI1     TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-CHOSEI1(CNT-LINE)
               MOVE    BD01004-CHOSEI2     TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-CHOSEI2(CNT-LINE)
               MOVE    BD01004-GENMEN      TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-GENMEN(CNT-LINE)
               MOVE    BD01004-ZENMISYU    TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-ZENMISYU(CNT-LINE)
           END-IF
      *
           MOVE    BD01004-NYUKIN          TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9            TO  HCM51-NYUKIN(CNT-LINE)
      *
           IF    ( SBD010-MEISAIKBN    =   ZERO )
               EVALUATE    BD01004-HOHO
               WHEN     CONST-HOHO-HENKIN
                   MOVE    CONST-HOHO-HENKINMSG
                                           TO HCM51-HOHO (CNT-LINE)
               WHEN     CONST-HOHO-TORIKESI
                   MOVE    CONST-HOHO-TORIKESIMSG
                                           TO HCM51-HOHO (CNT-LINE)
               WHEN    OTHER
                   MOVE    BD01004-HOHO    TO  SRH-CD
                   PERFORM 250-GET-1041-SEC
                   MOVE    SRH-ITM         TO  HCM51-HOHO (CNT-LINE)
               END-EVALUATE
           END-IF
      *
           IF    ( BD01004-DSPMISYUKBN     =   "1" )
               MOVE    BD01004-MISYU       TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM51-MISYU(CNT-LINE)
           END-IF
      *
           MOVE    BD01004-HOKENTEN    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-HKNTEN
           MOVE    BD01004-SKYMONEY    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYMONEY
           MOVE    BD01004-FTNMONEY    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYHOKEN
           MOVE    BD01004-JIHIMONEY   TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYJIHI
           MOVE    BD01004-NYUKIN      TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-NYUKIN
           MOVE    BD01004-KONNYUKIN   TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-KONNYUKIN
           MOVE    BD01004-CHOSEI      TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-CHOSEI
           MOVE    BD01004-CHOSEI1     TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-CHOSEI1
           MOVE    BD01004-CHOSEI2     TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-CHOSEI2
           MOVE    BD01004-GENMEN      TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-GENMEN
           MOVE    BD01004-MISYU       TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-MISYU
           MOVE    BD01004-ZENMISYU    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-ZENMISYU
      *
           INITIALIZE                  WRK-REC
           STRING  BD01004-DENPNUM     DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-PTNUM       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-NAME        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-KANANAME    DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SRYYMD          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51-SYOSAI(CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51-HOKEN (CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-SRYKA       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51-SRYKA (CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-HKNTEN          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SKYHOKEN        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SKYJIHI         DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-GENMEN          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-CHOSEI1         DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-CHOSEI2         DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-ZENMISYU        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SKYMONEY        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-NYUKIN          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51-HOHO (CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-MISYU           DELIMITED  BY  SPACE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       220-BODY-EXT.
           EXIT.
      *****************************************************************
      *    明細合計集計処理
      *****************************************************************
       220-SUM-BODY-TOTAL-SEC          SECTION.
      *
           IF    ( BD01004-SYOSAIKBN   =   "1" )
               COMPUTE TTL-SYOSIN
                   =   TTL-SYOSIN  +   1
           END-IF
      *
           IF    ( BD01004-SYOSAIKBN   =   "2" )
               COMPUTE TTL-SAISIN
                   =   TTL-SAISIN  +   1
           END-IF
      *
           COMPUTE TTL-HKNTEN
               =   TTL-HKNTEN  +   BD01004-HOKENTEN
           COMPUTE TTL-SKYMONEY
               =   TTL-SKYMONEY +  BD01004-SKYMONEY
           COMPUTE TTL-SKYHOKEN
               =   TTL-SKYHOKEN +  BD01004-FTNMONEY
           COMPUTE TTL-SKYJIHI
               =   TTL-SKYJIHI +   BD01004-JIHIMONEY
           COMPUTE TTL-NYUKIN
               =   TTL-NYUKIN  +   BD01004-NYUKIN
           COMPUTE TTL-KONNYUKIN
               =   TTL-KONNYUKIN + BD01004-KONNYUKIN
           COMPUTE TTL-CHOSEI
               =   TTL-CHOSEI  +   BD01004-CHOSEI
           COMPUTE TTL-CHOSEI1
               =   TTL-CHOSEI1 +   BD01004-CHOSEI1
           COMPUTE TTL-CHOSEI2
               =   TTL-CHOSEI2 +   BD01004-CHOSEI2
           COMPUTE TTL-GENMEN
               =   TTL-GENMEN  +   BD01004-GENMEN
           IF    ( TPARA-FORM      =   ZERO )
               COMPUTE TTL-MISYU
                   =   TTL-MISYU   +   BD01004-MISYU
           ELSE
               COMPUTE TTL-MISYU
                   =   TTL-MISYU   +   BD01004-TTLMISYU
           END-IF
           COMPUTE TTL-ZENMISYU
               =   TTL-ZENMISYU    +   BD01004-ZENMISYU
      *
           .
       220-SUM-BODY-TOTAL-EXT.
           EXIT.
      *****************************************************************
      *    明細合計行編集処理
      *****************************************************************
       2201-BODY-TOTAL-SEC             SECTION.
      *
           IF    ( KEY-NYUGAIKBN   =   "2" )
               MOVE    "初診"      TO  HCM51-TTL-SYOSINMSG
               MOVE    TTL-SYOSIN  TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9 TO HCM51-TTL-SYOSIN
               MOVE    "再診"      TO  HCM51-TTL-SAISINMSG
               MOVE    TTL-SAISIN  TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9 TO HCM51-TTL-SAISIN
           END-IF
      *
           MOVE    "合計"          TO  HCM51-TTL-TTLMSG
           MOVE    "保険点数"      TO  HCM51-TTL-HKNTENMSG
           MOVE    "今回請求額"    TO  HCM51-TTL-SKYMONEYMSG
           MOVE    "保険請求額"    TO  HCM51-TTL-SKYHOKENMSG
           MOVE    "自費請求額"    TO  HCM51-TTL-SKYJIHIMSG
           MOVE    "調整金１"      TO  HCM51-TTL-CHOSEI1MSG
           MOVE    "調整金２"      TO  HCM51-TTL-CHOSEI2MSG
           MOVE    "減免額"        TO  HCM51-TTL-GENMENMSG
           MOVE    "入金額"        TO  HCM51-TTL-NYUKINMSG
           MOVE    "（内今回入金額）"
                                   TO  HCM51-TTL-KONNYUKINMSG
           MOVE    "今回未収額"    TO  HCM51-TTL-MISYUMSG
           MOVE    "前回未収額"    TO  HCM51-TTL-ZENMISYUMSG
      *
           MOVE    TTL-HKNTEN      TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-HKNTEN
           MOVE    TTL-SKYMONEY    TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-SKYMONEY
           MOVE    TTL-SKYHOKEN    TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-SKYHOKEN
           MOVE    TTL-SKYJIHI     TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-SKYJIHI
           MOVE    TTL-NYUKIN      TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-NYUKIN
           INITIALIZE                  WRK-KONNYUKIN-G
           MOVE    TTL-KONNYUKIN   TO  WRK-KONNYUKIN
           MOVE    ")"             TO  WRK-KONNYUKINFL2
           PERFORM VARYING IDX0    FROM    7   BY  -1
                   UNTIL ( IDX0    <   1 )
               IF    ( IDX0        =   1 )
                  OR ( WRK-KONNYUKIN-G (1:IDX0)    =   SPACE )
                   MOVE    "("     TO  WRK-KONNYUKIN-G (IDX0:1)
                   MOVE    1       TO  IDX0
               END-IF
           END-PERFORM
           MOVE    WRK-KONNYUKIN-G TO  HCM51-TTL-KONNYUKIN
           MOVE    TTL-CHOSEI1     TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-CHOSEI1
           MOVE    TTL-CHOSEI2     TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-CHOSEI2
           MOVE    TTL-GENMEN      TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-GENMEN
           MOVE    TTL-MISYU       TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-MISYU
           MOVE    TTL-ZENMISYU    TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM51-TTL-ZENMISYU
      *
           .
       2201-BODY-TOTAL-EXT.
           EXIT.
      *****************************************************************
      *   印刷処理
      *****************************************************************
       230-PRINT-SEC               SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           PERFORM 230-PRINT-COMMON-SEC
      *     MOVE   "HCM51V03.red"   TO  SPRT-PRTID
           MOVE   "HCM51.red"      TO  SPRT-PRTID
           MOVE    "収納日報"      TO  SPRT-TITLE
           MOVE    HCM51           TO  SPRT-PRTDATA
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERROR (1) " CNT-PAGE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *   印刷パラメタ共通項目設定処理
      *****************************************************************
       230-PRINT-COMMON-SEC        SECTION.
      *
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
      *
           .
       230-PRINT-COMMON-EXT.
           EXIT.
      *****************************************************************
      *   帳票編集処理
      *****************************************************************
       20012-EDIT-FORM1-SEC          SECTION.
      *
           PERFORM 210-HEAD-FORM1-SEC
      *
      *
           PERFORM UNTIL ( FLG-BD01004           NOT = ZERO )
                    OR   ( BD01004-SRT-NYUGAIKBN NOT = KEY-NYUGAIKBN  )
                    OR   ( BD01004-SRT-SRYKA     NOT = KEY-SRYKA )
                    OR   ( CNT-LINE                 >= WRK-LINE-MAX )
      *
               PERFORM 220-BODY-FORM1-SEC
      *
               PERFORM 220-SUM-BODY-TOTAL-SEC
      *
               PERFORM 900-BD01004-READ-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-BD01004           NOT = ZERO )
            OR   ( BD01004-SRT-NYUGAIKBN NOT = KEY-NYUGAIKBN  )
            OR   ( BD01004-SRT-SRYKA     NOT = KEY-SRYKA )
      *
               PERFORM 2201-BODY-TOTAL-FORM1-SEC
           END-IF
      *
           PERFORM 230-PRINT-FORM1-SEC
      *
           .
       20012-EDIT-FORM1-EXT.
           EXIT.
      *****************************************************************
      *   見出し編集処理
      *****************************************************************
       210-HEAD-FORM1-SEC              SECTION.
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  HCM51V02
      *
           IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
               MOVE    "（入院）"      TO  HCM51V02-NYUGAI
           ELSE
               MOVE    "（外来）"      TO  HCM51V02-NYUGAI
           END-IF
      *
           MOVE    WRK-PRT-SRYYMD      TO  HCM51V02-HSRYYMD
      *
           IF    ( SBD010-SUMKBN   =   ZERO )
               CONTINUE
           ELSE
               MOVE    BD01004-SRYKA   TO  SRH-CD
               PERFORM 250-GET-1005-SEC
               MOVE    SRH-ITM         TO  HCM51V02-HSRYKA
           END-IF
      *
           MOVE    WRK-PRT-SAKYMD  TO  HCM51V02-SAKYMD
      *
           MOVE    CNT-PAGE        TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9       TO  HCM51V02-PAGE
      *
           IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
               MOVE    "伝票発行日"    TO  HCM51V02-SRYYMDMSG
               MOVE    "退定"          TO  HCM51V02-SYOSAIMSG
           ELSE
               MOVE    "診療年月日"    TO  HCM51V02-SRYYMDMSG
               MOVE    "初再"          TO  HCM51V02-SYOSAIMSG
           END-IF
      *
           MOVE    "未収額"            TO  HCM51V02-MISYUMSG
      *
           IF    ( TPARA-SHADE         =   ZERO )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L shade "
                                           DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
                                           INTO    HCM51V02-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L line "
                                           DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
                                           INTO    HCM51V02-LOPTION
               END-STRING
           END-IF
      *
           .
       210-HEAD-FORM1-EXT.
           EXIT.
      *****************************************************************
      *   明細編集処理
      *****************************************************************
       220-BODY-FORM1-SEC          SECTION.
      *
           INITIALIZE              CSV-AREA
           COMPUTE CNT-LINE    =   CNT-LINE   +   1
      *
           IF    ( BD01004-RENNUM  =   1 )
            AND  ( BD01004-RENNUM2 =   1 )
               COMPUTE CNT-NO  =   CNT-NO     +   1
           END-IF
      *
           IF    ( CNT-LINE        =   1 )
            OR  (( BD01004-RENNUM  =   1 )
              AND( BD01004-RENNUM2 =   1 ))
               MOVE    CNT-NO          TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  HCM51V02-NO (CNT-LINE)
               MOVE    BD01004-DENPNUM TO  HCM51V02-DENPNUM (CNT-LINE)
               IF    ( BD01004-PTNUM (11:)    =   SPACE )
                   MOVE    BD01004-PTNUM   TO  HCM51V02-PTNUM (CNT-LINE)
               ELSE
                   MOVE    BD01004-PTNUM   TO  HCM51V02-PTNUM2(CNT-LINE)
               END-IF
               MOVE    BD01004-NAME        TO  HCM51V02-NAME  (CNT-LINE)
               MOVE    HCM51V02-NAME  (CNT-LINE)
                                           TO  WRK-KANACHK-INPUT
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  HCM51V02-NAME  (CNT-LINE)
               IF    ( BD01004-SRT-NYUGAIKBN   =   "1" )
                   IF    ( BD01004-DENPPRTYMD
                                           NOT =   WRK-EDDAILYKEY-YMD )
                       MOVE    BD01004-DENPPRTYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD1 TO  HCM51V02-SRYYMD(CNT-LINE)
                       MOVE    BD01004-DENPPRTYMD  TO  WRK-SYMD
                       MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
                       MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
                       MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
                       MOVE    WRK-HEN-YMD-OT  TO  CSV-SRYYMD
                   END-IF
               ELSE
                   IF    ( BD01004-SRYYMD  NOT =   WRK-EDDAILYKEY-YMD )
                       MOVE    BD01004-SRYYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD1 TO  HCM51V02-SRYYMD(CNT-LINE)
                       MOVE    BD01004-SRYYMD  TO  WRK-SYMD
                       MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
                       MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
                       MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
                       MOVE    WRK-HEN-YMD-OT  TO  CSV-SRYYMD
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    BD01004-SYOSAIKBN
           WHEN    "1"
               MOVE    "初診"          TO  HCM51V02-SYOSAI(CNT-LINE)
           WHEN    "2"
               MOVE    "再診"          TO  HCM51V02-SYOSAI(CNT-LINE)
           WHEN    "3"
               MOVE    "退院"          TO  HCM51V02-SYOSAI(CNT-LINE)
           WHEN    "4"
               MOVE    "定期"          TO  HCM51V02-SYOSAI(CNT-LINE)
           END-EVALUATE
      *
      *
           MOVE    BD01004-SRYKA   TO  SRH-CD
           PERFORM 250-GET-1005-SEC
           MOVE    SRH-ITM2                TO  HCM51V02-SRYKA (CNT-LINE)
      *
           MOVE    WRK-SKYTTLMSG (BD01004-HOKEN)
                                           TO  HCM51V02-HOKEN (CNT-LINE)
      *
           IF    ( BD01004-SKYNYUKBN       =   ZERO )
               MOVE    BD01004-HOKENTEN TO  WRK-ZX99
               MOVE    WRK-ZX99     TO  HCM51V02-HKNTEN(CNT-LINE)
               MOVE    BD01004-SKYMONEY TO  WRK-ZX99
               MOVE    WRK-ZX99     TO  HCM51V02-SKYMONEY(CNT-LINE)
               MOVE    BD01004-FTNMONEY TO  WRK-ZX99
               MOVE    WRK-ZX99     TO  HCM51V02-SKYHOKEN(CNT-LINE)
               MOVE    BD01004-JIHIMONEY TO  WRK-ZX99
               MOVE    WRK-ZX99     TO  HCM51V02-SKYJIHI(CNT-LINE)
               COMPUTE WRK-CHOSEI
                    =  BD01004-CHOSEI
                    +  BD01004-GENMEN
               MOVE    WRK-CHOSEI          TO  WRK-ZX99
               MOVE    WRK-ZX99        TO  HCM51V02-CHOSEI(CNT-LINE)
      *
               IF    ( BD01004-GENMEN      NOT =   ZERO )
                   MOVE    "*"             TO  HCM51V02-CHOSEI-MARK
                                                           (CNT-LINE)
               END-IF
      *
           END-IF
      *
           MOVE    BD01004-NYUKIN          TO  WRK-ZX99
           MOVE    WRK-ZX99            TO  HCM51V02-NYUKIN(CNT-LINE)
      *
           IF    ( SBD010-MEISAIKBN    =   ZERO )
               EVALUATE    BD01004-HOHO
               WHEN     CONST-HOHO-HENKIN
                   MOVE    CONST-HOHO-HENKINMSG
                                           TO HCM51V02-HOHO (CNT-LINE)
               WHEN     CONST-HOHO-TORIKESI
                   MOVE    CONST-HOHO-TORIKESIMSG
                                           TO HCM51V02-HOHO (CNT-LINE)
               WHEN    OTHER
                   MOVE    BD01004-HOHO    TO  SRH-CD
                   PERFORM 250-GET-1041-SEC
                   MOVE    SRH-ITM         TO  HCM51V02-HOHO (CNT-LINE)
               END-EVALUATE
           END-IF
      *
           IF    ( BD01004-DSPMISYUKBN     =   "1" )
               MOVE    BD01004-TTLMISYU    TO  WRK-ZX99
               MOVE    WRK-ZX99        TO  HCM51V02-MISYU(CNT-LINE)
           END-IF
      *
           MOVE    BD01004-HOKENTEN    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-HKNTEN
           MOVE    BD01004-FTNMONEY    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYHOKEN
           MOVE    BD01004-JIHIMONEY   TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYJIHI
           MOVE    WRK-CHOSEI          TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-CHOSEI
           MOVE    BD01004-SKYMONEY    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYMONEY
           MOVE    BD01004-NYUKIN      TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-NYUKIN
           MOVE    BD01004-TTLMISYU    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-MISYU
      *
           INITIALIZE                  WRK-REC
           STRING  BD01004-DENPNUM     DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-PTNUM       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-NAME        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-KANANAME    DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SRYYMD          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51V02-SYOSAI(CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51V02-HOKEN (CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   BD01004-SRYKA       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51V02-SRYKA (CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-HKNTEN          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SKYHOKEN        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SKYJIHI         DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-CHOSEI          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-SKYMONEY        DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-NYUKIN          DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   HCM51V02-HOHO (CNT-LINE)
                                       DELIMITED  BY  SPACE
                   ","                 DELIMITED  BY  SIZE
                   CSV-MISYU           DELIMITED  BY  SPACE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       220-BODY-FORM1-EXT.
           EXIT.
      *****************************************************************
      *    明細合計行編集処理
      *****************************************************************
       2201-BODY-TOTAL-FORM1-SEC       SECTION.
      *
           IF    ( KEY-NYUGAIKBN   =   "2" )
               MOVE    "初診"      TO  HCM51V02-TTL-SYOSINMSG
               MOVE    TTL-SYOSIN  TO  WRK-ZX99
               MOVE    WRK-ZX99 TO HCM51V02-TTL-SYOSIN
               MOVE    "再診"      TO  HCM51V02-TTL-SAISINMSG
               MOVE    TTL-SAISIN  TO  WRK-ZX99
               MOVE    WRK-ZX99 TO HCM51V02-TTL-SAISIN
           END-IF
      *
           MOVE    "合計"          TO  HCM51V02-TTL-TTLMSG
           MOVE    "保険点数"      TO  HCM51V02-TTL-HKNTENMSG
           MOVE    "今回請求額"    TO  HCM51V02-TTL-SKYMONEYMSG
           MOVE    "保険請求額"    TO  HCM51V02-TTL-SKYHOKENMSG
           MOVE    "自費請求額"    TO  HCM51V02-TTL-SKYJIHIMSG
           MOVE    "調整金"        TO  HCM51V02-TTL-CHOSEIMSG
           MOVE    "入金額"        TO  HCM51V02-TTL-NYUKINMSG
           MOVE    "未収額"        TO  HCM51V02-TTL-MISYUMSG
      *
           MOVE    TTL-HKNTEN      TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-HKNTEN
           MOVE    TTL-SKYMONEY    TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-SKYMONEY
           MOVE    TTL-SKYHOKEN    TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-SKYHOKEN
           MOVE    TTL-SKYJIHI     TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-SKYJIHI
           MOVE    TTL-NYUKIN      TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-NYUKIN
           COMPUTE WRK-CHOSEI = TTL-CHOSEI + TTL-GENMEN
           MOVE    WRK-CHOSEI      TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-CHOSEI
           IF    ( TTL-GENMEN  NOT =   ZERO )
               MOVE    "*"         TO  HCM51V02-TTL-CHOSEI-MARK
           END-IF
           MOVE    TTL-MISYU       TO  WRK-ZX99
           MOVE    WRK-ZX99    TO  HCM51V02-TTL-MISYU
      *
           .
       2201-BODY-TOTAL-FORM1-EXT.
           EXIT.
      *****************************************************************
      *   印刷処理
      *****************************************************************
       230-PRINT-FORM1-SEC               SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           PERFORM 230-PRINT-COMMON-SEC
           MOVE   "HCM51V02.red"   TO  SPRT-PRTID
           MOVE    "収納日報"      TO  SPRT-TITLE
           MOVE    HCM51V02        TO  SPRT-PRTDATA
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERROR (1) " CNT-PAGE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       230-PRINT-FORM1-EXT.
           EXIT.
      *****************************************************************
      *   帳票編集処理
      *****************************************************************
       20011-EDIT-TTLFORM-SEC          SECTION.
      *
           INITIALIZE                      BD01003-REC
           MOVE    KEY-NYUGAIKBN       TO  BD01003-SRT-NYUGAIKBN
           MOVE    KEY-SRYKA           TO  BD01003-SRT-SRYKA
           PERFORM 900-BD01003-READ-SEC
           IF    ( FLG-BD01003         =   ZERO )
               PERFORM 211-HEAD-SEC
               PERFORM 221-BODY-SEC
               PERFORM 231-PRINT-SEC
           END-IF
      *
           .
       20011-EDIT-TTLFORM-EXT.
           EXIT.
      *****************************************************************
      *   見出し編集処理
      *****************************************************************
       211-HEAD-SEC                SECTION.
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
      *
           INITIALIZE                  HCM52
      *
           INITIALIZE                  WRK-REC
           IF    ( BD01003-SRT-NYUGAIKBN   =   "1" )
               MOVE    "（入院）"      TO  HCM52-NYUGAI
               INITIALIZE                  WRK-REC
               MOVE    "（入院）"      TO  CSV-NYUGAI2
           ELSE
               MOVE    "（外来）"      TO  HCM52-NYUGAI
               MOVE    "初診件数"      TO  HCM52-SYOSINMSG
               MOVE    "再診件数"      TO  HCM52-SAISINMSG
               MOVE    "初再診計"      TO  HCM52-SYOSAIMSG
               MOVE    "（外来）"      TO  CSV-NYUGAI2
           END-IF
      *
           MOVE    WRK-CSV-STSRYYMD    TO  CSV-STHSRYYMD2
           MOVE    WRK-CSV-STTIME      TO  CSV-STTIME2
           MOVE    WRK-CSV-DASH        TO  CSV-DASH2
           MOVE    WRK-CSV-EDSRYYMD    TO  CSV-EDHSRYYMD2
           MOVE    WRK-CSV-EDTIME      TO  CSV-EDTIME2
           MOVE    WRK-CSV-SAKYMD      TO  CSV-SAKYMD2
           IF    ( SBD010-SUMKBN   =   ZERO )
               CONTINUE
           ELSE
               MOVE    BD01003-SRT-SRYKA
                                       TO  SRH-CD
               PERFORM 250-GET-1005-SEC
               MOVE    SRH-ITM         TO  CSV-SRYKA2
           END-IF
      *
           MOVE    CSV-HEAD-02A    TO  WRK-SOURCE
           MOVE    " *, *"         TO  WRK-PATTERN
           MOVE    ","             TO  WRK-REPLACEMENT
           MOVE    "g"             TO  WRK-FLAGS
           PERFORM 800-REGEXP-REPLACE-SEC
           MOVE    WRK-SOURCE      TO  WRK-REC
           PERFORM 500-TOUKEICSV-SEC
      *
           MOVE    CSV-HEAD-02B    TO  WRK-REC
           PERFORM 500-TOUKEICSV-SEC
      *
           MOVE    WRK-PRT-SRYYMD      TO  HCM52-HSRYYMD
      *
           IF    ( SBD010-SUMKBN   =   ZERO )
               CONTINUE
           ELSE
               MOVE    BD01003-SRT-SRYKA
                                       TO  SRH-CD
               PERFORM 250-GET-1005-SEC
               MOVE    SRH-ITM         TO  HCM52-HSRYKA
           END-IF
      *
           MOVE    WRK-PRT-SAKYMD      TO  HCM52-SAKYMD
      *
      *
           MOVE    CNT-PAGE            TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9           TO  HCM52-PAGE
      *
           IF    ( TPARA-SHADE         =   ZERO )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L shade "
                                           DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
                                           INTO    HCM52-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L line "
                                           DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
                                           INTO    HCM52-LOPTION
               END-STRING
           END-IF
      *
           .
       211-HEAD-EXT.
           EXIT.
      *****************************************************************
      *   明細編集処理
      *****************************************************************
       221-BODY-SEC                SECTION.
      *
           IF    ( TPARA-FORM      =   ZERO )
               MOVE    "（内今回入金額計）"
                                   TO  HCM52-KONNYUKINMSG
               MOVE    "今回未収額計"
                                   TO  HCM52-KONMISYUMSG
           END-IF
      *
           INITIALIZE              CSV-AREA2
      *
           MOVE    "保険点数"      TO  CSV-DAI
      *
           MOVE    BD01003-TENSU-TOTAL
                                   TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  HCM52-TTLTENSU
           MOVE    BD01003-TENSU-TOTAL
                                   TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO    TO  CSV-GOKEI
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   WRK-SKYTTL-MAX )
      *
               MOVE    BD01003-TENSU (IDX2)
                                   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9 TO HCM52-TENSU (IDX2)
      *
               MOVE    BD01003-TENSU (IDX2)
                                   TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  CSV-SKYMONEY2(IDX2)
      *
           END-PERFORM
      *
           PERFORM 2211-BODY-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   11 )
      *
               MOVE    ZERO        TO  FLG-TARGET
               IF    ( BD01003-SRT-NYUGAIKBN   =   "1" )
                   IF    ( IDX1        =   CONST-TTLSEIKYUREL
                                        OR CONST-TTLGENMEN
                                        OR CONST-TTLCHOSEI1
                                        OR CONST-TTLCHOSEI2
                                        OR CONST-TTLSEIKYU
                                        OR CONST-TTLNYUKIN
                                        OR CONST-TTLKONNYUKIN
                                        OR CONST-TTLMISYU )
                       MOVE    1           TO  FLG-TARGET
                   END-IF
               ELSE
                       MOVE    1           TO  FLG-TARGET
               END-IF
      *
               IF    ( TPARA-FORM  =   "1" )
                   IF    ( IDX1        =   CONST-TTLKONNYUKIN
                                        OR CONST-TTLMISYU )
                       MOVE    ZERO    TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET  =   1 )
      *
                   INITIALIZE              CSV-AREA2
                   IF    ( IDX1        =   CONST-TTLKONNYUKIN )
                       INITIALIZE              WRK-KONNYUKIN-G
                       MOVE    BD01003-SEIKYU-TOTAL (IDX1)
                                           TO  WRK-KONNYUKIN
                       MOVE    ")"         TO  WRK-KONNYUKINFL2
                       PERFORM VARYING IDX0    FROM    7   BY  -1
                               UNTIL ( IDX0    <   1 )
                           IF    ( IDX0        =   1 )
                            OR   ( WRK-KONNYUKIN-G (1:IDX0)  = SPACE )
                               MOVE    "(" TO  WRK-KONNYUKIN-G (IDX0:1)
                               MOVE    1   TO  IDX0
                           END-IF
                       END-PERFORM
                       MOVE    WRK-KONNYUKIN-G
                                           TO  HCM52-GOKEI (IDX1)
                   ELSE
                       MOVE    BD01003-SEIKYU-TOTAL (IDX1)
                                           TO  WRK-ZZZZZZZ9
                       MOVE    WRK-ZZZZZZZ9 TO HCM52-GOKEI (IDX1)
                   END-IF
                   MOVE    BD01003-SEIKYU-TOTAL (IDX1)  TO  WRK-SURYO
                   PERFORM 400-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO        TO  CSV-GOKEI
      *
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   WRK-SKYTTL-MAX )
      *
                       IF    ( IDX1        =   1 )
                           MOVE   WRK-SKYTTLMSG (IDX2)
                                           TO  HCM52-SKYTTLMSG(IDX2)
                       END-IF
      *
                       IF    ( IDX1        =   CONST-TTLKONNYUKIN )
                           INITIALIZE              WRK-KONNYUKIN-G
                           MOVE    BD01003-SEIKYU(IDX1 IDX2)
                                               TO  WRK-KONNYUKIN
                           MOVE    ")"         TO  WRK-KONNYUKINFL2
                           PERFORM VARYING IDX0    FROM    7   BY  -1
                                   UNTIL ( IDX0    <   1 )
                               IF    ( IDX0        =   1 )
                                OR   ( WRK-KONNYUKIN-G (1:IDX0)
                                                   = SPACE )
                                   MOVE    "(" 
                                           TO  WRK-KONNYUKIN-G (IDX0:1)
                                   MOVE    1
                                           TO  IDX0
                               END-IF
                           END-PERFORM
                           MOVE    WRK-KONNYUKIN-G
                                           TO  HCM52-SKYMONEY(IDX1 IDX2)
                       ELSE
                           MOVE    BD01003-SEIKYU(IDX1 IDX2)
                                               TO  WRK-ZZZZZZZ9
                           MOVE    WRK-ZZZZZZZ9
                                           TO  HCM52-SKYMONEY(IDX1 IDX2)
                       END-IF
                       MOVE    BD01003-SEIKYU(IDX1 IDX2)
                                               TO  WRK-SURYO
                       PERFORM 400-ZHEN-SURYO-SET-SEC
                       MOVE    WRK-HENSURYO    TO  CSV-SKYMONEY2(IDX2)
                   END-PERFORM
      *
                   EVALUATE    IDX1
                       WHEN    CONST-TTLSEIKYUREL
                           MOVE    "今回請求額"    TO  CSV-DAI
                       WHEN    CONST-TTLGENMEN
                           MOVE    "減免額"    TO  CSV-DAI
                       WHEN    CONST-TTLCHOSEI1
                           MOVE    "調整金１"    TO  CSV-DAI
                       WHEN    CONST-TTLCHOSEI2
                           MOVE    "調整金２"    TO  CSV-DAI
                       WHEN    CONST-TTLSEIKYU
                           MOVE    "今回請求額計"  TO  CSV-DAI
                       WHEN    CONST-TTLNYUKIN
                           MOVE    "入金額計"  TO  CSV-DAI
                       WHEN    CONST-TTLKONNYUKIN
                           MOVE    "内今回入金額計"    TO  CSV-DAI
                       WHEN    CONST-TTLMISYU
                           MOVE    "今回未収額計"    TO  CSV-DAI
                       WHEN    CONST-TTLSYOSIN
                           MOVE    "初診件数"    TO  CSV-DAI
                       WHEN    CONST-TTLSAISIN
                           MOVE    "再診件数"    TO  CSV-DAI
                       WHEN    CONST-TTLSYOSAI
                           MOVE    "初再診件"    TO  CSV-DAI
                   END-EVALUATE
                   PERFORM 2211-BODY-SEC
               END-IF
      *
           END-PERFORM
      *
           INITIALIZE              CSV-AREA2
           MOVE    "合　計"                TO  HCM52-TTLHOHOMSG
           MOVE    BD01003-NYUKIN-TOTAL    TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9            TO  HCM52-TTLNYUKIN
           MOVE    BD01003-NYUKIN-TOTAL    TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-GOKEI
      *
           MOVE    ZERO                    TO  IDX-HOHO
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       8 )
                      OR ( IDX1    >       SRH-1041-MAX )
      *
               COMPUTE IDX-HOHO    =   IDX-HOHO    +   1
               MOVE    SRH-1041-NYKN-TANNAME (IDX1)
                                           TO  HCM52-HOHOMSG (IDX-HOHO)
               MOVE    BD01003-NYUKIN (IDX1)
                                           TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM52-NYUKIN  (IDX-HOHO)
               MOVE     BD01003-NYUKIN (IDX1)
                                           TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-SKYMONEY2(IDX-HOHO)
      *
           END-PERFORM
      *
           IF    ( BD01003-NYUKIN (09) NOT =   ZERO )
            OR   ( BD01003-NYUKIN (10) NOT =   ZERO )
            OR   ( BD01003-NYUKIN (11) NOT =   ZERO )
               COMPUTE IDX-HOHO    =   IDX-HOHO    +   1
               COMPUTE WRK-NYUKIN
                   =   BD01003-NYUKIN (09)
                   +   BD01003-NYUKIN (10)
                   +   BD01003-NYUKIN (11)
               MOVE    "その他"            TO  HCM52-HOHOMSG (IDX-HOHO)
               MOVE    WRK-NYUKIN          TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM52-NYUKIN  (IDX-HOHO)
               MOVE    WRK-NYUKIN          TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-SKYMONEY2(IDX-HOHO)
           END-IF
      *
           COMPUTE IDX-HOHO    =   IDX-HOHO    +   1
           MOVE    "返　金"            TO  HCM52-HOHOMSG (IDX-HOHO)
           MOVE    BD01003-NYUKIN (12) TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9        TO  HCM52-NYUKIN  (IDX-HOHO)
           MOVE    BD01003-NYUKIN (12) TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-SKYMONEY2(IDX-HOHO)
      *
           IF    ( BD01003-NYUKIN (13) NOT =   ZERO )
               COMPUTE IDX-HOHO    =   IDX-HOHO    +   1
               MOVE    "入金取消"          TO  HCM52-HOHOMSG (IDX-HOHO)
               MOVE    BD01003-NYUKIN (13) TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  HCM52-NYUKIN  (IDX-HOHO)
               MOVE    BD01003-NYUKIN (13) TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-SKYMONEY2(IDX-HOHO)
           END-IF
      *
           INITIALIZE              WRK-REC
           STRING  ","                 DELIMITED BY SIZE
                   "合計"              DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (1)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (2)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (3)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (4)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (5)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (6)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (7)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (8)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (9)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (10)  DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (11)  DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (12)  DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (13)  DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (14)  DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-HOHOMSG (15)  DELIMITED BY SPACE
                                 INTO  WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           INITIALIZE              WRK-REC
           STRING  "入金額計"          DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   CSV-GOKEI           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(1)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(2)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(3)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(4)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(5)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(6)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(7)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(8)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(9)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(10)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(11)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(12)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(13)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(14)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(15)   DELIMITED BY SPACE
                                 INTO  WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           INITIALIZE              CSV-AREA2
           MOVE    "合　計"                TO  HCM52-TTLJIHIMSG
           MOVE    BD01003-JIHI-TOTAL      TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9            TO  HCM52-TTLJIHI
           MOVE    BD01003-JIHI-TOTAL      TO  WRK-SURYO
           PERFORM 400-ZHEN-SURYO-SET-SEC
           MOVE    WRK-HENSURYO        TO  CSV-GOKEI
      *
           PERFORM VARYING IDX-JIHI    FROM    1   BY  1
                   UNTIL ( IDX-JIHI    >   CONST-ETCJIHI-MAX )
               IF    ( BD01003-SRT-NYUGAIKBN   =   "1" )
                   MOVE    SRH-1013-JIHINAME (1 IDX-JIHI)
                                           TO  HCM52-JIHIMSG (IDX-JIHI)
               ELSE
                   MOVE    SRH-1013-JIHINAME (2 IDX-JIHI)
                                           TO  HCM52-JIHIMSG (IDX-JIHI)
               END-IF
      *
               IF    ( HCM52-JIHIMSG (IDX-JIHI) =  SPACE )
                   MOVE    BD01003-JIHI (IDX-JIHI)
                                           TO  WRK-ZZZZZZZZ
                   MOVE    WRK-ZZZZZZZZ    TO  HCM52-JIHI    (IDX-JIHI)
      *             MOVE    BD01003-JIHI (IDX-JIHI)
      *                                     TO  CSV-SKYMONEY2(IDX-JIHI)
               ELSE
                   MOVE    BD01003-JIHI (IDX-JIHI)
                                           TO  WRK-ZZZZZZZ9
                   MOVE    WRK-ZZZZZZZ9    TO  HCM52-JIHI    (IDX-JIHI)
                   MOVE    BD01003-JIHI (IDX-JIHI)
                                           TO  WRK-SURYO
                   PERFORM 400-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO    TO  CSV-SKYMONEY2(IDX-JIHI)
               END-IF
      *
           END-PERFORM
           INITIALIZE              WRK-REC
           STRING  ","                 DELIMITED BY SIZE
                   "合　計"            DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (1)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (2)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (3)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (4)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (5)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (6)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (7)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (8)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (9)   DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   HCM52-JIHIMSG (10)  DELIMITED BY SPACE
                                 INTO  WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           INITIALIZE              WRK-REC
           STRING  "その他自費計"      DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   CSV-GOKEI           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(1)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(2)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(3)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(4)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(5)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(6)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(7)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(8)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(9)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(10)   DELIMITED BY SPACE
                                 INTO  WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       221-BODY-EXT.
           EXIT.
      *****************************************************************
      *   明細編集処理
      *****************************************************************
       2211-BODY-SEC               SECTION.
      *
           INITIALIZE              WRK-REC
           STRING  CSV-DAI             DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-GOKEI           DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(1)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(2)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(3)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(4)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(5)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(6)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(7)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(8)    DELIMITED BY SPACE
                   ","                 DELIMITED BY SIZE
                   CSV-SKYMONEY2(9)    DELIMITED BY SPACE
                                 INTO  WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       2211-BODY-EXT.
           EXIT.
      *****************************************************************
      *   印刷処理
      *****************************************************************
       231-PRINT-SEC               SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           PERFORM 230-PRINT-COMMON-SEC
           MOVE   "HCM52V05.red"       TO  SPRT-PRTID
           MOVE    "収納日報（合計）"  TO  SPRT-TITLE
           MOVE    HCM52               TO  SPRT-PRTDATA
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERROR (2) " CNT-PAGE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       231-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    診療科取得処理
      *****************************************************************
       250-GET-1005-SEC            SECTION.
      *
           INITIALIZE              SRH-IDX
                                   SRH-ITM-G
      *
           PERFORM VARYING IDX-SRH FROM    1   BY  1
                   UNTIL ( IDX-SRH >   SRH-1005-MAX )
                    OR   ( SRH-ITM NOT =   SPACE )
      *
               IF    ( SRH-1005-CD (IDX-SRH)   =   SRH-CD )
      *
                   MOVE    IDX-SRH TO  SRH-IDX
      *
                   MOVE    SRH-1005-SRYKANAME1 (IDX-SRH)
                                   TO  SRH-ITM
      *
                   MOVE    SRH-1005-SRYKANAME3 (IDX-SRH)
                                   TO  SRH-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       250-GET-1005-EXT.
           EXIT.
      *****************************************************************
      *    入金方法取得処理
      *****************************************************************
       250-GET-1041-SEC            SECTION.
      *
           INITIALIZE              SRH-IDX
                                   SRH-ITM-G
      *
           PERFORM VARYING IDX-SRH FROM    1   BY  1
                   UNTIL ( IDX-SRH >   SRH-1041-MAX )
                    OR   ( SRH-ITM NOT =   SPACE )
      *
               IF    ( SRH-1041-CD (IDX-SRH)   =   SRH-CD )
      *
                   MOVE    IDX-SRH TO  SRH-IDX
      *
                   MOVE    SRH-1041-NYKN-TANNAME (IDX-SRH)
                                   TO  SRH-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       250-GET-1041-EXT.
           EXIT.
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   10
               IF      WRK-SURYO(CNT-KETA2:1)    NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE    WRK-SURYO(CNT-KETA2:1)
                                           TO  WRK-HENSURYO(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF    ( STS-RECEERR         =   ZERO )
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SBD010-JOBID    TO  JOB-JOBID
               MOVE    SBD010-SHELLID  TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SBD010-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
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
           MOVE    SBD010-SRYYMD   TO  STOUKEICSV-SRYYMD
           MOVE    SBD010-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "収納日報ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBD010 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 800-DEL-FILE-SEC
      *
           IF    ( CNT-PAGE        =   ZERO )
               MOVE    "対象データがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SBD010-JOBID    TO  JOB-JOBID
           MOVE    SBD010-SHELLID  TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           MOVE    SBD010-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイル削除処理
      *****************************************************************
       800-DEL-FILE-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BD01003PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BD01004PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-DEL-FILE-EXT.
           EXIT.
      *****************************************************************
      *    CSV日付編集処理
      *****************************************************************
       800-CSV-DATE-SEC            SECTION.
      *
            MOVE    SPACE          TO  WRK-CSV-DATE
      *
            MOVE    WRK-SYMD(1:4)  TO  WRK-CSV-DATE(1:4)
            MOVE    "/"            TO  WRK-CSV-DATE(5:1)
            MOVE    WRK-SYMD(5:2)  TO  WRK-CSV-DATE(6:2)
            MOVE    "/"            TO  WRK-CSV-DATE(8:1)
            MOVE    WRK-SYMD(7:2)  TO  WRK-CSV-DATE(9:2)
      *
           .
       800-CSV-DATE-EXT.
           EXIT.
      *****************************************************************
      *    CSV時刻編集処理
      *****************************************************************
       800-CSV-TIME-SEC            SECTION.
      *
            MOVE    SPACE          TO  WRK-CSV-TIME
      *
            MOVE    WRK-TIME(1:2)  TO  WRK-CSV-TIME(1:2)
            MOVE    ":"            TO  WRK-CSV-TIME(3:1)
            MOVE    WRK-TIME(3:2)  TO  WRK-CSV-TIME(4:2)
      *
           .
       800-CSV-TIME-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-KANACHK-INPUT TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
               MOVE    KANACHK-MAX TO  WRK-KANACHK-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       800-ZENKAKU-CHK-SEC         SECTION.
      *
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  WRK-KANACHK-INPUT
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE  WRK-KANACHK-INPUT TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT
                                       TO  WRK-KANACHK-INPUT
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初再診チェックサブ呼出処理
      *****************************************************************
       800-CALL-ORCSS003-SEC           SECTION.
      *
           INITIALIZE                          SS003-AREA
           MOVE    "1"                 TO      SS003-SYORIKBN
           MOVE    SYU-HOSPNUM          TO      SS003-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO      SS003-NYUGAIKBN
           MOVE    SYU-PTID            TO      SS003-PTID
           MOVE    SYU-DENPNUM         TO      SS003-DENPNUM
           CALL    "ORCSS003"          USING   SS003-AREA
                                               SPA-AREA
      *
           .
       800-CALL-ORCSS003-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SBD010-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    文字列置換処理
      *****************************************************************
       800-REGEXP-REPLACE-SEC          SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    WRK-SOURCE      TO  PTSRH-VTEXT
           MOVE    WRK-PATTERN     TO  PTSRH-VSTR (1)
           MOVE    WRK-REPLACEMENT TO  PTSRH-VSTR (2)
           MOVE    WRK-FLAGS       TO  PTSRH-VSTR (3)
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "regexp_replace" TO MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTSRH-REC
               MOVE    PTSRH-SQL   TO  WRK-SOURCE
           END-IF
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "regexp_replace" TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       800-REGEXP-REPLACE-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                  SMEI-REC
           MOVE    SBD010-HOSPNUM   TO  SMEI-HOSPNUM
           IF    ( SBD010-NYUGAIKBN    =   "0" )
               MOVE    "%"         TO SMEI-NYUGAIKBN
           ELSE
               MOVE    SBD010-NYUGAIKBN
                                   TO SMEI-NYUGAIKBN
           END-IF
           MOVE    WRK-STDAILYKEY  TO  SMEI-STDAILYKEY
           MOVE    WRK-EDDAILYKEY  TO  SMEI-EDDAILYKEY
           MOVE    SMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key17"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SMEI-REC
           ELSE
               INITIALIZE              SMEI-REC
               MOVE    1           TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細FETCH処理
      *****************************************************************
       900-SYUMEI-KEY17-FET-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           MOVE    SMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key17"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SMEI-REC
           ELSE
               INITIALIZE                  SMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SBD010-HOSPNUM TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SBD010-HOSPNUM TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SMEI-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SMEI-PTID       TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SMEI-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SMEI-PTID       TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
      *
           MOVE    SMEI-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID       TO  SYU-PTID
           MOVE    SMEI-DENPNUM    TO  SYU-DENPNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル読込処理
      *****************************************************************
       900-BD01003-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-BD01003
      *
           READ    BD01003-FILE
           INVALID
               MOVE    1           TO  FLG-BD01003
           END-READ
      *
           .
       900-BD01003-READ-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル読込処理
      *****************************************************************
       900-BD01004-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-BD01004
      *
           READ    BD01004-FILE    NEXT
           AT  END
               MOVE    1           TO  FLG-BD01004
           END-READ
      *
           .
       900-BD01004-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
