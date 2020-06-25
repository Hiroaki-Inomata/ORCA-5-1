      ******************************************************************
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
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000D124.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 月計表
      *  管理者            : 
      *  作成日付    作業者       記述
      *  04/02/09    @@@@-半田    新規作成
      ******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/02/21  処理速度の改善
      *  01.00.03    @@@@-門間    12/02/29  インデント整理
      *                           12/03/09  CSVファイル作成機能追加
      *  01.00.04    @@@@-門間    12/11/29  病床数修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    15/04/02  保険点数が「０」の場合、平均点の計算から除く
      *  02.00.03    @@@@-門間    15/04/16  助成金額の一時領域を修正
      *  02.00.04    @@@@-門間    17/11/17  在院数合計を印字するよう修正
      *  02.00.05    @@@@-井上    19/01/22  明細部の桁数修正
      *  02.00.06    @@@@-井上    20/03/06  「シ　助成金」欄の集計処理を修正
      ******************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    基本台帳(入院月報)用対象患者ファイル
           SELECT  D124PT-FILE     ASSIGN  AD124PT
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     D124P-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
      *    基本台帳(入院月報)用対象患者ファイル
       FD  D124PT-FILE.
       01  D124P. 
           03    D124P-KEY.
             05  D124P-SRYYMD      PIC X(08).
             05  D124P-PTID        PIC 9(10).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
       01  D100IPARA.
           03  FILLER              PIC X(256) VALUE  SPACE.
      *
      *    診療日別受信患者ファイル
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //AD124PT//.
      *
      *    基本台帳(入院月報)DIA用コピー句
           COPY    "A00000D124.INC".
           COPY    "A00000D1241.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTNYUINRRK-M    PIC 9(01).
           03  FLG-PTNYUINRRK-M2   PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INVALID         PIC 9(01).
           03  FLG-820SYS          PIC X(04).
           03  INP-STS             PIC 9(04).
           03  FLG-JIHI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE          PIC 9(02).
           03  CNT-PAGE          PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY        PIC 9(02).
               05  SYS-MM        PIC 9(02).
               05  SYS-DD        PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  I1                PIC 9(04).
           03  I2                PIC 9(04).
           03  I3                PIC 9(02).
           03  I4                PIC 9(04).
           03  I5                PIC 9(04).
           03  I6                PIC 9(04).
           03  I7                PIC 9(04).
           03  I8                PIC 9(02). 
           03  IDX1              PIC 9(04).
           03  IDX2              PIC 9(04).
           03  IDX3              PIC 9(04).
           03  IDX4              PIC 9(04).
           03  IDX5              PIC 9(04).
           03  IDX6              PIC 9(04).
           03  IDX7G.    
             05    IDX7          PIC 9(02).
           03  IDX8G.
             05    IDX8          PIC 9(04).
           03  IDX               PIC 9(04).
           03  IDY               PIC 9(04).
           03  IDZ               PIC 9(04).
           03  IDX-S             PIC 9(07).
           03  IDX-N             PIC 9(07).
           03  IDX-MAX           PIC 9(07).
      *
           03  JIHIIDX           PIC 9(04).
           03  NG                PIC 9(04).
           03  IDX9               PIC 9(04).
      *
           03  IDX-MONTH         PIC 9(03).
           03  IDX-ST            PIC 9(03).
           03  IDX-ED            PIC 9(03).
           03  IDXP              PIC 9(05).
      *
           03  IDX-BED           PIC 9(02).
           03  IDX-BED2          PIC 9(02).
           03  IDX-BED-ST        PIC 9(02).
           03  IDX-BED-ED        PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-SRYYMD1           PIC X(22).
           03  WRK-SRYYMD2           PIC X(22).
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-BIRTHDAY.
               05 WRK-BIRTHY         PIC 9(04).
               05 WRK-BIRTHM         PIC 9(02).
               05 WRK-BIRTHD         PIC 9(02).
           03  WRK-NENREI            PIC 9(02).
      *  
           03  WRK-AZUKARIYMD.
               05  WRK-AZUKARIYY     PIC 9(04).
               05  WRK-AZUKARIMM     PIC 9(02).
               05  WRK-AZUKARIDD     PIC 9(02).
      *
           03  WRK-SYUHKN-KBN        PIC 9(01).
           03  WRK-INI-AZUKARICD     PIC X(09).
           03  WRK-NYUTENTTLKBN      PIC 9(03).
           03  WRK-BF-MISYU          PIC S9(09).
           03  WRK-BF-AZUKARI        PIC S9(09).
           03  WRK-TEN               PIC S9(09).
           03  WRK-MONEY             PIC S9(09).
           03  WRK-JIHI              PIC S9(09).
           03  WRK-SYUEKI            PIC S9(09).
           03  WRK-RATE              PIC 9(01)V9(03).
           03  WRK-RATE2             PIC 9(03).
           03  WRK-FTN               PIC S9(09).
      *
           03  WRK-JIHI2            PIC 9(08).
           03  WRK-TJIHI-G       OCCURS  10.
               05  WRK-TJIHI        PIC X(12).
               05  WRK-TJIHI-FLG    PIC 9(01).
      *
           03  WRK-4001-TENTANKA    PIC 9(02)V9(01).
           03  WRK-RSI-MONEY1       PIC S9(09)V9(01). 
           03  WRK-RSI-MONEY2       PIC S9(09)V9(01). 
      *
           03  WRK-RENNUM-X         PIC ZZ9.
           03  WRK-PAGE             PIC ZZ9.
           03  WRK-RATE-Z9          PIC ZZ9.  
           03  WRK-Z9               PIC ZZZZZZZZ9.
           03  WRK-ZZ               PIC ZZZZZZZZZ.
           03  WRK-S9               PIC ----------9.
           03  WRK-SZ               PIC --------Z.
           03  WRK-NYUGAIKBN        PIC X(01).
           03  WRK-SRYKA            PIC X(02).
           03  WRK-DRCD             PIC X(05).
           03  WRK-SRYYMD           PIC X(08).
           03  WRK-SEL-SRYKACD      PIC X(02).
           03  WRK-SEL-SRYKANAME    PIC X(20).
           03  WRK-SEL-DRCD         PIC X(04).
           03  WRK-SEL-DRNAME       PIC X(20).
           03  DEBUG-CNT            PIC 9(03).
           03  WRK-SYS-BEDSU        PIC 9(04).
           03  WRK-SYS-BEDSUIPN     PIC 9(04).
           03  WRK-FTNRATE          PIC 9(01)V9(02).
           03  WRK-KYFRATE          PIC 9(01)V9(02).
      *
           03  WRK-TMP-YMD.
            05 WRK-TMP-YY        PIC 9(04).
            05 WRK-TMP-MM        PIC 9(02).
            05 WRK-TMP-DD        PIC 9(02).
           03  WRK-TMP-LYMD.
            05 WRK-TMP-LYY       PIC 9(04).
            05 WRK-TMP-LMM       PIC 9(02).
            05 WRK-TMP-LDD       PIC 9(02).
           03  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
           03  WRK-SUM-SYOHI          PIC S9(07).
           03  WRK-START-MM           PIC 9(02).
      *
           03  WRK-SONISU            PIC 9(02).
           03  WRK-JINSUU            PIC 9(09).
           03  WRK-SOBYOSYO          PIC S9(11).
           03  WRK-SUM-NISU          PIC S9(09).
           03  WRK-CNT-MONTH         PIC 9(02).
           03  WRK-TMP1-YMD.
            05 WRK-TMP1-YY            PIC 9(04).
            05 WRK-TMP1-MM            PIC 9(02).
            05 WRK-TMP1-DD            PIC 9(02).
           03  WRK-TMP2-YMD.
            05 WRK-TMP2-YY            PIC 9(04).
            05 WRK-TMP2-MM            PIC 9(02).
            05 WRK-TMP2-DD            PIC 9(02).
           03  WRK-STARTYMD      PIC 9(08).
           03  WRK-LASTYMD       PIC 9(08).
           03  WRK-NYURRK-G.
               05  WRK-NYURRK-SRYKA    PIC X(02).
               05  WRK-NYURRK-DRCD     PIC X(04).
               05  WRK-NYURRK-NYUYMD   PIC 9(08).
               05  WRK-NYURRK-STUYMD   PIC 9(08).
           03  WRK-ACCT-SDAY     PIC 9(02).
           03  WRK-ACCT-EDAY     PIC 9(02).
           03  WRK-ACCT-IN       PIC 9(04).
           03  WRK-JIHIMONEY     PIC 9(10).
           03  WRK-AZUKARI       PIC 9(10).
           03  WRK-NYU-PTID      PIC 9(10).
           03  WRK-HKNNAI            PIC 9(11).
           03  WRK-JOSEI             PIC S9(11).
      *
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-NENDOYM1      PIC 9(06).
           03  WRK-NENDOYM2      PIC 9(06).
           03  WRK-NENDONAI      PIC 9(06).
      *
           03  WRK-HN-TBL               OCCURS  32.
             05  WRK-HNINZU               PIC 9(05).
      *
       01  SUM-AREA.
           03         SUM-SRYKAMAX      PIC  9(03).
           03         SUM-DRMAX         PIC  9(03).
           03         SUM-MONTHMAX      PIC  9(02).
           03         SUM-SRYKA-G.    
             05       SUM-SRYKACD       PIC  X(02).
             05       SUM-SRYKANAME     PIC  X(20).
           03         SUM-DR-G.    
             05       SUM-DRCD          PIC  X(04).
             05       SUM-DRNAME        PIC  X(20).
           03         SUM-BED-TBL       OCCURS   32.
             05         SUM-BEDSU       PIC  9(04).
           03         SUM-PRFLG         PIC  9(01).
           03         SUM-BF-MISYU      PIC S9(09).
           03         SUM-JITUMSG       PIC  X(06).
           03         SUM-TENKI-G       OCCURS   3.
             05       SUM-TENKI-MSG     PIC  X(22).
           03         SUM-MONTH-TBL     OCCURS   26.
             05       SUM-MONTH         PIC  X(06).
             05       SUM-SYUNOU-CNT    PIC  9(09).
             05       SUM-YOSO-TBL      OCCURS   50.
               07     SUM-YOSO          PIC S9(11).
      *
       01  KEY-AREA.
           03  KEY-PTID                 PIC 9(10).
           03  KEY-DENPNUM              PIC 9(07).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
           03  WRK-SYMD1               PIC X(08).
           03  WRK-TMP-LYMD1           PIC X(08).
           COPY    "CPSHELLTBL.INC".
      *
      *+
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE         PIC 9(03)     VALUE  100.
           03  WRK-ITEM              PIC X(100)    VALUE  SPACE.
           03  WRK-CSVDATA           PIC X(20000)  VALUE  SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-D001  PIC X(50)
                            VALUE  "項目名".
               07  CSV-HEAD-D002  PIC X(50)
                            VALUE  "（ア）患者数＿入院".
               07  CSV-HEAD-D003  PIC X(50)
                            VALUE  "（イ）患者数＿退院".
               07  CSV-HEAD-D004  PIC X(50)
                            VALUE  "（ウ）患者数＿在院".
               07  CSV-HEAD-D005  PIC X(50)
                            VALUE  "（エ）患者数＿病床数".
               07  CSV-HEAD-D006  PIC X(50)
                            VALUE  "（オ）患者数＿稼働率".
               07  CSV-HEAD-D007  PIC X(50)
                            VALUE  "（カ）保険内点数（ヌ〜ヨ）".
               07  CSV-HEAD-D008  PIC X(50)
                            VALUE  "（キ）請求数".
               07  CSV-HEAD-D009  PIC X(50)
                            VALUE  "（ク）平均点（カ／キ）".
               07  CSV-HEAD-D010  PIC X(50)
                            VALUE  "（ケ）保険内負担金".
               07  CSV-HEAD-D011  PIC X(50)
                            VALUE  "（コ）保険外金額（あ〜さ）".
               07  CSV-HEAD-D012  PIC X(50)
                            VALUE  "（サ）食事療養費・生活療養費".
               07  CSV-HEAD-D013  PIC X(50)
                            VALUE  "（シ）食事負担額・生活負担額".
               07  CSV-HEAD-D014  PIC X(50)
                            VALUE  "（ス）室料差額".
               07  CSV-HEAD-D015  PIC X(50)
                            VALUE  "（セ）減免金額".
               07  CSV-HEAD-D016  PIC X(50)
                            VALUE  "（ソ）助成金額".
               07  CSV-HEAD-D017  PIC X(50)
                            VALUE  "（タ）消費税".
               07  CSV-HEAD-D018  PIC X(50)
                            VALUE  "（チ）調整金".
               07  CSV-HEAD-D019  PIC X(50)
                            VALUE  "（ツ）窓口請求金額".
               07  CSV-HEAD-D020  PIC X(50)
                            VALUE  "（テ）過日未収金残高".
               07  CSV-HEAD-D021  PIC X(50)
                            VALUE  "（ト）入金額".
               07  CSV-HEAD-D022  PIC X(50)
                            VALUE  "（ナ）当日未収金".
               07  CSV-HEAD-D023  PIC X(50)
                            VALUE  "（ニ）当日預り金残高".
               07  CSV-HEAD-D024  PIC X(50)
                            VALUE  "（ヌ）診療区分＿初・再診料".
               07  CSV-HEAD-D025  PIC X(50)
                            VALUE  "（ネ）診療区分＿医学管理等".
               07  CSV-HEAD-D026  PIC X(50)
                            VALUE  "（ノ）診療区分＿在宅医療".
               07  CSV-HEAD-D027  PIC X(50)
                            VALUE  "（ハ）診療区分＿投薬".
               07  CSV-HEAD-D028  PIC X(50)
                            VALUE  "（ヒ）診療区分＿注射".
               07  CSV-HEAD-D029  PIC X(50)
                            VALUE  "（フ）診療区分＿処置".
               07  CSV-HEAD-D030  PIC X(50)
                            VALUE  "（ヘ）診療区分＿手術".
               07  CSV-HEAD-D031  PIC X(50)
                            VALUE  "（ホ）診療区分＿麻酔".
               07  CSV-HEAD-D032  PIC X(50)
                            VALUE  "（マ）診療区分＿検査".
               07  CSV-HEAD-D033  PIC X(50)
                            VALUE  "（ミ）診療区分＿画像診断".
               07  CSV-HEAD-D034  PIC X(50)
                            VALUE  "（ム）診療区分＿リハビリテーション".
               07  CSV-HEAD-D035  PIC X(50)
                            VALUE  "（メ）診療区分＿精神科専門療法".
               07  CSV-HEAD-D036  PIC X(50)
                            VALUE  "（モ）診療区分＿放射線治療".
               07  CSV-HEAD-D037  PIC X(50)
                            VALUE  "（ヤ）診療区分＿病理診断".
               07  CSV-HEAD-D038  PIC X(50)
                            VALUE  "（ユ）診療区分＿入院料等".
               07  CSV-HEAD-D039  PIC X(50)
                            VALUE  "（ヨ）診療区分＿療養担当手当".
               07  CSV-HEAD-D040  PIC X(50)
                            VALUE  "（あ）保険適用外金額".
               07  CSV-HEAD-D041-G.
      *
                 09  CSV-HEAD-JIHI01-ID   PIC X(18)
                              VALUE  "（い）保険外自費＿".
                 09  CSV-HEAD-JIHI01-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI02-ID   PIC X(18)
                              VALUE  "（う）保険外自費＿".
                 09  CSV-HEAD-JIHI02-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI03-ID   PIC X(18)
                              VALUE  "（え）保険外自費＿".
                 09  CSV-HEAD-JIHI03-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI04-ID   PIC X(18)
                              VALUE  "（お）保険外自費＿".
                 09  CSV-HEAD-JIHI04-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI05-ID   PIC X(18)
                              VALUE  "（か）保険外自費＿".
                 09  CSV-HEAD-JIHI05-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI06-ID   PIC X(18)
                              VALUE  "（き）保険外自費＿".
                 09  CSV-HEAD-JIHI06-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI07-ID   PIC X(18)
                              VALUE  "（く）保険外自費＿".
                 09  CSV-HEAD-JIHI07-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI08-ID   PIC X(18)
                              VALUE  "（け）保険外自費＿".
                 09  CSV-HEAD-JIHI08-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI09-ID   PIC X(18)
                              VALUE  "（こ）保険外自費＿".
                 09  CSV-HEAD-JIHI09-NAME PIC X(32).
      *
                 09  CSV-HEAD-JIHI10-ID   PIC X(18)
                              VALUE  "（さ）保険外自費＿".
                 09  CSV-HEAD-JIHI10-NAME PIC X(32).
      *
               07  CSV-HEAD-JIHI-R      REDEFINES  CSV-HEAD-D041-G.
                 09  CSV-HEAD-JIHI-OCC    OCCURS  10.
                   11  CSV-HEAD-JIHI-ID     PIC X(06).
                   11  CSV-HEAD-JIHI-NAME   PIC X(44).
      *
             05  CSV-HEAD-R   REDEFINES   CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM        PIC X(50)  OCCURS  50.
      *
             05  CSV-HEAD-MAX         PIC 9(05)  VALUE  50.
      *+
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災情報取得
           COPY  "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    請求管理マスタ
           COPY    "CPRCF010.INC".
      *
      *    患者入院履歴情報(入院履歴全枝番号用)
       01  PNR-M-REC.
           COPY    "CPPTNYUINRRK.INC"
           REPLACING  //PTNYUINRRK-// BY //PNR-M-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *+
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *+
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000D100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000D100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  INP-STS  =   ZERO
      *    正常主処理
             PERFORM 200-MAIN-SEC
           ELSE
      *    エラー処理
             PERFORM 210-ERR-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
           MOVE    D100LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    D100LNK-JOBID   TO  JOB-JOBID
           MOVE    D100LNK-SHELLID TO  JOB-SHELLID
           MOVE    "A00000D124"    TO  JOB-PGID
      *
           MOVE    "基本台帳(入院月報)"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AD124TP"                TO  AD124PT-FILE-ID
           MOVE    D100LNK-PRTKANRI-TERMID  TO  AD124PT-TERMID
           MOVE    D100LNK-HOSPNUM          TO  AD124PT-HOSPNUM
      *
           MOVE    AD124PT-BASENAME         TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  AD124PT-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    D100LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  WRK-SYSYMDWH 
      *
      *    集計対象診療年月日１
           INITIALIZE  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SRYYMD1
      *
      *    集計対象診療年月日２
           INITIALIZE  WRK-SYMD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SRYYMD2 
      *
           MOVE  D100LNK-YMD1(1:6)    TO  WRK-STARTYMD(1:6)
           MOVE  "01"                 TO  WRK-STARTYMD(7:2)
           MOVE  D100LNK-YMD2(1:6)    TO  WRK-LASTYMD(1:6)
           MOVE  "31"                 TO  WRK-LASTYMD(7:2)
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE  WRK-STARTYMD          TO  SYS-1001-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1001-STYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPNUM    TO  D100LNK-HOSPNUM
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    病床数の取得
           MOVE    D100LNK-YMD1(5:2)      TO  IDX-BED-ST
           MOVE    D100LNK-YMD2(5:2)      TO  IDX-BED-ED
      *
           PERFORM  VARYING  IDX-BED  FROM  IDX-BED-ST  BY  1
                    UNTIL    IDX-BED  >  IDX-BED-ED
             ADD     1                   TO  IDX-BED2
             MOVE    SPACE               TO  SYS-1001-REC
             INITIALIZE                  SYS-1001-REC
             MOVE    "1001"              TO  SYS-1001-KANRICD
             MOVE    "*"                 TO  SYS-1001-KBNCD
             MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
             MOVE    D100LNK-YMD1(1:4)   TO  SYS-1001-EDYUKYMD(1:4)
             MOVE    D100LNK-YMD2(1:4)   TO  SYS-1001-STYUKYMD(1:4)
             MOVE    IDX-BED             TO  SYS-1001-EDYUKYMD(5:2)
                                             SYS-1001-STYUKYMD(5:2)
             MOVE    "01"                TO  SYS-1001-EDYUKYMD(7:2)
             MOVE    "31"                TO  SYS-1001-STYUKYMD(7:2)
             MOVE    SYS-1001-REC        TO  MCPDATA-REC
             PERFORM 800-SYSKANRI-READ-SEC
             MOVE    SYS-1001-BEDSU      TO  SUM-BEDSU(IDX-BED2)
           END-PERFORM
      *
      *    画面入力パラメタ値のチェック
      *    集計対象か期間のチェック
           IF   D100LNK-YMD1    >   D100LNK-YMD2
                MOVE    10      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
           MOVE D100LNK-YMD1(1:6) TO WRK-NENDOYM1
           MOVE D100LNK-YMD2(1:6) TO WRK-NENDOYM2
           COMPUTE    WRK-NENDONAI    =
                      WRK-NENDOYM2  -  WRK-NENDOYM1
           IF   WRK-NENDONAI >= 100
                MOVE    11      TO  INP-STS
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF   D100LNK-YMD1   <   "20080401"   
                AND   D100LNK-YMD2   >  "20080331"
                MOVE    12      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT 
           END-IF
      *
      *    入外区分のチェック
           IF   D100LNK-NYUGAIKBN   NOT =   "1"
            AND D100LNK-NYUGAIKBN   NOT =   "2"
                MOVE    20      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    診療科コードのチェック
           IF  D100LNK-SRYKACD     NOT  =  SPACE
               INITIALIZE                      SYS-1005-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    D100LNK-SRYKACD     TO  SYS-1005-KBNCD
               MOVE    WRK-LASTYMD          TO  WRK-SYMD
               MOVE  WRK-STARTYMD          TO  SYS-1005-EDYUKYMD
               MOVE  WRK-LASTYMD           TO  SYS-1005-STYUKYMD
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 810-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI2        =  ZERO
                       MOVE  MCPDATA-REC   TO  SYS-1005-REC
                       MOVE  SYS-1005-KBNCD(1:2)
                                           TO  WRK-SEL-SRYKACD
                       MOVE  SYS-1005-SRYKANAME1
                                           TO  WRK-SEL-SRYKANAME
               ELSE
                       MOVE  30            TO  INP-STS
                       GO                  TO  110-PARA-HENSYU-EXT
               END-IF
           END-IF
      *
      *    医師コードのチェック
           IF  D100LNK-DRCD        NOT  =  SPACE
               INITIALIZE                      SYS-1010-REC
               MOVE  SPA-HOSPNUM           TO  SYS-1010-HOSPNUM
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    D100LNK-DRCD        TO  SYS-1010-KBNCD
               MOVE    WRK-LASTYMD          TO  WRK-SYSYMD
               MOVE  WRK-STARTYMD          TO  SYS-1010-EDYUKYMD
               MOVE  WRK-LASTYMD           TO  SYS-1010-STYUKYMD
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               PERFORM 810-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI2       =   ZERO
                 AND   D100LNK-DRCD(1:1)   NOT =   "0"
                       MOVE  MCPDATA-REC   TO  SYS-1010-REC
                       MOVE  SYS-1010-KBNCD(2:4)
                                           TO  WRK-SEL-DRCD
                       MOVE  SYS-1010-NAME
                                           TO  WRK-SEL-DRNAME
               ELSE
                       MOVE  40            TO  INP-STS
                       GO                  TO  110-PARA-HENSYU-EXT
               END-IF
           END-IF
      *
      *    入院自費情報取得
      *    区分21
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "11"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(1)
               MOVE  1                 TO  WRK-TJIHI-FLG(1)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(1)
           END-IF
      *  
      *    区分22
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "12"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(2)
               MOVE  1                 TO  WRK-TJIHI-FLG(2)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(2)
           END-IF
      *  
      *    区分23
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "13"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(3)
               MOVE  1                 TO  WRK-TJIHI-FLG(3)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(3)
           END-IF
      *  
      *    区分24
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "14"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(4)
               MOVE  1                 TO  WRK-TJIHI-FLG(4)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "15"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(5)
               MOVE  1                 TO  WRK-TJIHI-FLG(5)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(5)
           END-IF
      *
      *    区分16
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "16"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(6)
               MOVE  1                 TO  WRK-TJIHI-FLG(6)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "17"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(7)
               MOVE  1                 TO  WRK-TJIHI-FLG(7)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "18"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(8)
               MOVE  1                 TO  WRK-TJIHI-FLG(8)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "19"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(9)
               MOVE  1                 TO  WRK-TJIHI-FLG(9)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "10"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-1013-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(10)
               MOVE  1                 TO  WRK-TJIHI-FLG(10)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(10)
           END-IF                                     
      *
      *    労災医療機関情報
           MOVE    SPACE                TO  SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
           MOVE  SPA-HOSPNUM            TO  SYS-4001-HOSPNUM
           MOVE    "4001"               TO  SYS-4001-KANRICD
           MOVE    "*"                  TO  SYS-4001-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  WRK-STARTYMD          TO  SYS-4001-EDYUKYMD
           MOVE  WRK-LASTYMD           TO  SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC         TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
                   MOVE  MCPDATA-REC    TO  SYS-4001-REC
                   EVALUATE  SYS-4001-TENKBN
                       WHEN  "1"    
                             MOVE  11.5 TO  WRK-4001-TENTANKA
                       WHEN  "2"    
                             MOVE  12   TO  WRK-4001-TENTANKA
                       WHEN  OTHER    
                             MOVE  10   TO  WRK-4001-TENTANKA
                  END-EVALUATE
           ELSE
                  MOVE  10   TO  WRK-4001-TENTANKA
           END-IF
      *
           IF   WRK-SEL-SRYKACD   =    SPACE
                MOVE     1     TO   I1 
                MOVE "99"      
                          TO   SUM-SRYKACD  
                MOVE "全科合計"
                          TO   SUM-SRYKANAME
           ELSE
                MOVE     1     TO   I1 
                MOVE WRK-SEL-SRYKACD      
                          TO   SUM-SRYKACD  
                MOVE WRK-SEL-SRYKANAME
                          TO   SUM-SRYKANAME
           END-IF
      *
           PERFORM  110-DR-PARA-HENSYU-SEC
      *
      *    帳票テキスト入力
               IF    SUM-DRCD     NOT =  "9999"
                AND  SUM-SRYKACD      =  "99"
                    MOVE  "(医師変更を含む)"  
                                       TO  SUM-TENKI-MSG(1)
               END-IF
               IF    SUM-DRCD         =  "9999"
                AND  SUM-SRYKACD  NOT =  "99"
                    MOVE  "(転科を含む)"  
                                       TO  SUM-TENKI-MSG(1)
               END-IF
               IF    SUM-DRCD     NOT =  "9999"
                AND  SUM-SRYKACD  NOT =  "99"
                    MOVE  "(転科、医師変更を含む)"  
                                       TO  SUM-TENKI-MSG(1)
               END-IF
               MOVE  SUM-TENKI-MSG(1)  TO  SUM-TENKI-MSG(2)
               MOVE  SUM-TENKI-MSG(1)  TO  SUM-TENKI-MSG(3)
      *
      *    プログラムオプション取込み（預り金コード）
           INITIALIZE                      ORCSPRGOPTIONAREA
           MOVE    "A00000D100"        TO  ORCSPRGOPTION-PRGID
           MOVE    "AZUKARICD"         TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA(1:9)  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA(1:9)  TO  WRK-INI-AZUKARICD
           END-IF
      *
      *    点数マスタ読込み
           MOVE   WRK-INI-AZUKARICD TO     TNS-SRYCD
           MOVE    SPA-HOSPNUM      TO     TNS-HOSPNUM
           PERFORM   910-TENSU-READ-SEC
           MOVE   TNS-NYUTENTTLKBN  TO     WRK-NYUTENTTLKBN
      *
      *    基本台帳(入院月報)用対象患者ファイル初期化
           OPEN  OUTPUT D124PT-FILE
           CLOSE        D124PT-FILE  
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師名編集処理
      *****************************************************************
       110-DR-PARA-HENSYU-SEC                   SECTION.
      *
           IF   WRK-SEL-DRCD    =        SPACE
                MOVE "9999"              TO   SUM-DRCD  
                MOVE "全医師合計"        TO   SUM-DRNAME
           ELSE
                MOVE WRK-SEL-DRCD        TO   SUM-DRCD  
                MOVE WRK-SEL-DRNAME      TO   SUM-DRNAME
           END-IF
           PERFORM  110-SRYMD-PARA-HENSYU-SEC
      *
           .
       110-DR-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付編集処理
      *****************************************************************
       110-SRYMD-PARA-HENSYU-SEC                   SECTION.
      *
      *    印刷年月日取得
           INITIALIZE  WRK-SYMD
           MOVE     1              TO   I3 
           MOVE     WRK-STARTYMD   TO   WRK-TMP-YMD
      *
      *    開始日から終了日までループ または 15回ループ
           PERFORM  UNTIL  WRK-TMP-YMD(1:6) > WRK-LASTYMD(1:6)
               MOVE  WRK-TMP-YMD(1:6) TO   SUM-MONTH(I3) 
               ADD   1                TO   WRK-TMP-MM
               IF    WRK-TMP-MM    =    13
                   ADD    1    TO    WRK-TMP-YY
                   MOVE   1    TO    WRK-TMP-MM
               END-IF
               ADD   1                TO   I3
           END-PERFORM
           MOVE     "計"        TO   SUM-MONTH(I3)  
           MOVE     I3          TO   SUM-MONTHMAX   
      *
           .
       110-SRYMD-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    基本台帳(入院月報)用対象患者ファイルOPEN
           OPEN  I-O D124PT-FILE
      *
      *    入院履歴集計処理
           PERFORM 310-NYURRK-CNT-SEC
      *
      *    収納情報集計処理
           PERFORM 320-SYUNOU-CNT-SEC
      *
      *    基本台帳(入院月報)用対象患者ファイルCLOSE
           CLOSE     D124PT-FILE
      *
      *    帳票編集印刷処理
           PERFORM 330-TERM-HEN-SEC
           MOVE   1    TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値阿エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE                   D124
           INITIALIZE                   D1241
      *
           MOVE   "(入院月報)"    TO    D124-SUBTITLE
           MOVE    WRK-SYSYMDWH   TO    D124-SYSYMD
           MOVE   "(入院月報)"    TO    D1241-SUBTITLE
           MOVE    WRK-SYSYMDWH   TO    D1241-SYSYMD
      *
           EVALUATE INP-STS 
           WHEN 10
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D124-JOBMSG
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D1241-JOBMSG
      *
           WHEN 11
             MOVE
             "入力する集計対象期間の範囲は１年以内で指定をして下さい"
                                  TO    D124-JOBMSG
             MOVE
             "入力する集計対象期間の範囲は１年以内で指定をして下さい"
                                  TO    D1241-JOBMSG
      *
           WHEN 12
             MOVE
           "入力された集計対象期間の範囲指定に誤りがあります（改正日）"
                                  TO    D124-JOBMSG
             MOVE
           "入力された集計対象期間の範囲指定に誤りがあります（改正日）"
                                  TO    D1241-JOBMSG
      *
           WHEN 20
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    D124-JOBMSG
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    D1241-JOBMSG
      *
           WHEN 30
             MOVE
             "入力された診療科コードの値に誤りがあります"
                                  TO    D124-JOBMSG
             MOVE
             "入力された診療科コードの値に誤りがあります"
                                  TO    D1241-JOBMSG
      *
           WHEN 40
             MOVE
             "入力された医師コードの値に誤りがあります"
                                  TO    D124-JOBMSG
             MOVE
             "入力された医師コードの値に誤りがあります"
                                  TO    D1241-JOBMSG
           END-EVALUATE
      *
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴集計処理
      *****************************************************************
       310-NYURRK-CNT-SEC      SECTION.
      *
           MOVE     1         TO   I3
           MOVE WRK-STARTYMD  TO WRK-TMP-YMD
           MOVE WRK-TMP-YMD   TO WRK-TMP-LYMD
           MOVE  31           TO WRK-TMP-LDD
      *
           PERFORM  UNTIL  WRK-TMP-YMD  >   WRK-LASTYMD
                      OR   WRK-TMP-MM   >   12
             INITIALIZE                  PTNYUINRRK-REC
             MOVE  D100LNK-HOSPNUM   TO   PTNYUINRRK-HOSPNUM
             MOVE  WRK-TMP-LYMD      TO   PTNYUINRRK-NYUINYMD
             MOVE  WRK-TMP-YMD       TO   PTNYUINRRK-TAIINYMD
      *    入院履歴により入院数、退院数、在院数を取得する。
             PERFORM  900-NYURRK-SELECT-SEC
             PERFORM  UNTIL   FLG-PTNYUINRRK  =   1
      *         テスト患者処理
                INITIALIZE                   PTINF-REC
                MOVE     PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
                MOVE     PTNYUINRRK-PTID     TO  PTINF-PTID
                PERFORM  900-PTINF-READ-SEC
                IF  FLG-PTINF = ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT  =  "1"
      *
      *            履歴レコード初期化
                   INITIALIZE               WRK-NYURRK-G
                   INITIALIZE               PNR-M-REC
                   MOVE    PTNYUINRRK-PTID  
                                        TO  PNR-M-PTID
                  MOVE    D100LNK-HOSPNUM
                                        TO  PNR-M-HOSPNUM
                   MOVE    PTNYUINRRK-RRKNUM
                                        TO  PNR-M-RRKNUM
      *
                   PERFORM  900-NYURRK-M-SELECT-SEC
                   PERFORM  UNTIL   FLG-PTNYUINRRK-M  =   1
      *
                      IF  ( ( SUM-DRCD    = "9999"
                         OR   SUM-DRCD    = PNR-M-DRCD1(2:4) )
                        AND ( SUM-SRYKACD = "99"
                         OR   SUM-SRYKACD = PNR-M-NYUINKA ) )
      *
                          IF 0            = WRK-NYURRK-NYUYMD
                             MOVE PNR-M-TENNYUYMD 
                                            TO WRK-NYURRK-NYUYMD
                          END-IF
                          MOVE PNR-M-TENSTUYMD
                                            TO WRK-NYURRK-STUYMD
                      ELSE
      *                  カウント処理
                          PERFORM  310-NYURRK-CNT-SUB-SEC
      *
      *                  履歴レコード初期化
                         INITIALIZE       WRK-NYURRK-G
                      END-IF
                      PERFORM  900-NYURRK-M-FETCH-SEC
                   END-PERFORM
                   PERFORM 900-M-DBCLOSE-SEC
      *
      *           カウント処理
                  PERFORM  310-NYURRK-CNT-SUB-SEC
      *
                  IF     PTNYUINRRK-PTID  NOT =  WRK-NYU-PTID
      *               患者別に当日預り金を診療行為情報より取得する。
                      PERFORM  320-AZUKARI-GET-SEC
                  END-IF
                  MOVE   PTNYUINRRK-PTID  TO  WRK-NYU-PTID
      *
                END-IF
                PERFORM  900-NYURRK-FETCH-SEC
             END-PERFORM
             PERFORM   900-NYURRK-CLOSE-SEC
             MOVE  ZERO  TO  WRK-NYU-PTID
      *      集計月カウントアップ
             ADD   1     TO  WRK-TMP-MM
             IF    WRK-TMP-MM    =    13
               ADD    1    TO    WRK-TMP-YY
               MOVE   1    TO    WRK-TMP-MM
             END-IF
             MOVE WRK-TMP-YMD   TO WRK-TMP-LYMD
             MOVE  31           TO WRK-TMP-LDD
      *
           END-PERFORM
      *
           PERFORM  VARYING  I3     FROM 1  BY  1
                   UNTIL    I3     >=   SUM-MONTHMAX
      *        入院、退院、在院 合計
               ADD  SUM-YOSO(I3 2)
                    TO  SUM-YOSO (SUM-MONTHMAX 2)
               ADD  SUM-YOSO(I3 3)
                    TO  SUM-YOSO (SUM-MONTHMAX 3)
               ADD  SUM-YOSO(I3 4)
                    TO  SUM-YOSO (SUM-MONTHMAX 4)
      *
           END-PERFORM
      *
          .
       310-NYURRK-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴集計サブ処理
      *****************************************************************
       310-NYURRK-CNT-SUB-SEC      SECTION.
      *
           MOVE  WRK-TMP-YMD   TO  WRK-SRYYMD
           PERFORM  320-SRYYMD-GROUPING-SEC
           IF I3  <     SUM-MONTHMAX
               IF  FLG-GET = ZERO
                   MOVE   1   TO   FLG-GET
               END-IF
           END-IF
           IF      0     <   WRK-NYURRK-NYUYMD
      *
      *    入院
               IF    WRK-TMP-LYMD  >= WRK-NYURRK-NYUYMD   
                AND  WRK-TMP-YMD   <= WRK-NYURRK-NYUYMD
                   ADD   1      TO   SUM-YOSO(I3 2)
               END-IF
      *
      *    退院
               IF    WRK-TMP-LYMD  >= WRK-NYURRK-STUYMD   
                AND  WRK-TMP-YMD   <= WRK-NYURRK-STUYMD
                   ADD   1      TO   SUM-YOSO(I3 3)
               END-IF
      *
      *    在院
               IF WRK-NYURRK-NYUYMD <= D100LNK-YMD2  AND
                  WRK-NYURRK-STUYMD >= D100LNK-YMD1
                    
                 INITIALIZE  WRK-SYMD1
                 MOVE    WRK-TMP-YMD     TO     WRK-SYMD1(1:6)
                 PERFORM 31012-LASTDAY-GET-SEC
                 MOVE    WRK-SYMD1       TO     WRK-TMP-LYMD1
                 IF  WRK-NYURRK-NYUYMD <=  D100LNK-YMD1  AND
                     WRK-NYURRK-STUYMD >=  D100LNK-YMD1
                   IF  WRK-NYURRK-NYUYMD(1:6) =  D100LNK-YMD1(1:6)
                     IF WRK-TMP-YMD(1:6)   =   WRK-NYURRK-NYUYMD(1:6)
                         MOVE  D100LNK-YMD1   TO    WRK-TMP1-YMD
                     ELSE  
                       IF WRK-TMP-YMD(1:6) > WRK-NYURRK-NYUYMD(1:6)
                           MOVE  WRK-TMP-YMD    TO    WRK-TMP1-YMD
                       END-IF
                     END-IF
                   ELSE 
                     IF  WRK-TMP-YMD(1:6) =  D100LNK-YMD1(1:6)
                         MOVE  D100LNK-YMD1    TO    WRK-TMP1-YMD
                     ELSE
                         MOVE  WRK-TMP-YMD     TO    WRK-TMP1-YMD
                     END-IF
                   END-IF
                 END-IF
                 IF  WRK-NYURRK-NYUYMD >= D100LNK-YMD1 AND
                     WRK-NYURRK-STUYMD >= D100LNK-YMD1
                   IF  WRK-NYURRK-NYUYMD(1:6) = D100LNK-YMD1(1:6)
                     IF  WRK-TMP-YMD(1:6)  >  D100LNK-YMD1(1:6)
                         MOVE   WRK-TMP-YMD  TO   WRK-TMP1-YMD
                     ELSE
                         MOVE   WRK-NYURRK-NYUYMD  TO   WRK-TMP1-YMD
                     END-IF
                   ELSE
                     IF  WRK-NYURRK-NYUYMD(1:6)  =  WRK-TMP-YMD(1:6)
                         MOVE  WRK-NYURRK-NYUYMD  TO  WRK-TMP1-YMD
                     ELSE 
                       IF WRK-NYURRK-NYUYMD(1:6) < WRK-TMP-YMD(1:6)
                           MOVE  WRK-TMP-YMD       TO  WRK-TMP1-YMD
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 IF  WRK-NYURRK-STUYMD >= D100LNK-YMD1 AND
                   WRK-NYURRK-STUYMD >  D100LNK-YMD2
                   IF WRK-NYURRK-STUYMD(1:6) = D100LNK-YMD2(1:6)
                     IF  WRK-TMP-LYMD1(1:6) < D100LNK-YMD2(1:6)
                         MOVE  WRK-TMP-LYMD1    TO  WRK-TMP2-YMD
                     ELSE
                         MOVE  D100LNK-YMD2    TO    WRK-TMP2-YMD
                     END-IF
                   ELSE
                     IF  WRK-TMP-LYMD1(1:6) = D100LNK-YMD2(1:6)
                         MOVE  D100LNK-YMD2    TO  WRK-TMP2-YMD
                     ELSE
                         IF  WRK-TMP-LYMD1(1:6) < D100LNK-YMD2(1:6)
                             MOVE WRK-TMP-LYMD1  TO  WRK-TMP2-YMD
                         END-IF
                     END-IF
                   END-IF
                 END-IF
                 IF  WRK-NYURRK-STUYMD >= D100LNK-YMD1 AND
                     WRK-NYURRK-STUYMD <= D100LNK-YMD2
                   IF  WRK-NYURRK-STUYMD(1:6) = D100LNK-YMD2(1:6)
                     IF  D100LNK-YMD2(1:6) = WRK-TMP-LYMD1(1:6)
                         MOVE  WRK-NYURRK-STUYMD  TO  WRK-TMP2-YMD
                     ELSE
                         MOVE  WRK-TMP-LYMD1  TO  WRK-TMP2-YMD
                     END-IF
                   ELSE
                     IF  WRK-NYURRK-STUYMD(1:6) = WRK-TMP-LYMD1(1:6)
                         MOVE  WRK-NYURRK-STUYMD  TO  WRK-TMP2-YMD
                     ELSE
                       IF WRK-NYURRK-STUYMD(1:6) > WRK-TMP-LYMD1(1:6)
                           MOVE  WRK-TMP-LYMD1  TO  WRK-TMP2-YMD
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      
                 COMPUTE SUM-YOSO(I3 4) = SUM-YOSO(I3 4)
                                        + WRK-TMP2-DD + 1
                                        - WRK-TMP1-DD
               END-IF
      *
           END-IF
      *
           .                      
       310-NYURRK-CNT-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計処理
      *****************************************************************
       320-SYUNOU-CNT-SEC      SECTION.
      *
           PERFORM  910-DBSELECT-SEC
           PERFORM  UNTIL      FLG-SYUNOU  = 1
      *
      *     集計対象年月のデータであるかをチェックする。 
            IF      SYU-DENPPRTYMD         >=  WRK-STARTYMD
              AND   SYU-DENPPRTYMD         <=  WRK-LASTYMD
              AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
              AND   SYU-CREATEKBN       NOT =  "3"
              AND   SYU-DENPJTIKBN      NOT =  "3"
              AND   SYU-NYUGAIKBN           =  "1"
              AND ((WRK-SEL-SRYKACD         =  SPACE)
               OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                    SYU-SRYKA               =  WRK-SEL-SRYKACD))
              AND ((WRK-SEL-DRCD            =  SPACE)
               OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                    SYU-DRCD                =  WRK-SEL-DRCD))
      *
               MOVE  SYU-DENPPRTYMD   TO  WRK-SRYYMD
               PERFORM  320-SRYYMD-GROUPING-SEC
               IF I3  <     SUM-MONTHMAX
                  IF  FLG-GET = ZERO
                       MOVE   1   TO   FLG-GET
                  END-IF
                  PERFORM 320-SYUNOU-CNT-SUB-SEC
               END-IF
             END-IF
             PERFORM 910-DBFETCH-SEC
           END-PERFORM
           PERFORM 910-DBCLOSE-SEC
      *
           PERFORM  911-DBSELECT-SEC
           PERFORM  UNTIL      FLG-SYUNOU  = 1
      *
            INITIALIZE         PTINF-REC 
            MOVE SYU-HOSPNUM TO PTINF-HOSPNUM
            MOVE SYU-PTID   TO PTINF-PTID
            PERFORM 900-PTINF-READ-SEC
      *
               IF     SYU-DENPPRTYMD           <  WRK-STARTYMD
                AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
                AND   SYU-CREATEKBN       NOT =  "3"
                AND   SYU-DENPJTIKBN      NOT =  "3"
                AND   SYU-NYUGAIKBN           =  "1"
                AND ((WRK-SEL-SRYKACD         =  SPACE)
                 OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                      SYU-SRYKA               =  WRK-SEL-SRYKACD))
                AND ((WRK-SEL-DRCD            =  SPACE)
                 OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                      SYU-DRCD                =  WRK-SEL-DRCD))
                AND   FLG-PTINF               =  ZERO
                AND   PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***        範囲指定期間前未収金の集計
                  COMPUTE  WRK-MONEY    =
                           SYU-SKYMONEY -  SYU-NYUKIN-TOTAL
                  ADD   WRK-MONEY  TO  SUM-BF-MISYU
               END-IF
            PERFORM 911-DBFETCH-SEC
           END-PERFORM
           PERFORM 911-DBCLOSE-SEC
      *
           MOVE   ZERO  TO  FLG-SYUMEI
           PERFORM  320-SYUMEI-SELECT-SEC
           PERFORM  UNTIL  FLG-SYUMEI  = 1
              INITIALIZE         PTINF-REC 
              MOVE SMEI-HOSPNUM TO PTINF-HOSPNUM
              MOVE SMEI-PTID    TO PTINF-PTID
              PERFORM 900-PTINF-READ-SEC
              PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
              MOVE    SMEI-PTID    TO  KEY-PTID
              MOVE    SMEI-DENPNUM TO  KEY-DENPNUM
              PERFORM UNTIL ( FLG-SYUMEI  = 1 )
                       OR   ( SMEI-PTID      NOT = KEY-PTID )
                       OR   ( SMEI-DENPNUM   NOT = KEY-DENPNUM )
      *
                 IF     SYU-HOSPNUM             =  D100LNK-HOSPNUM
                  AND   SYU-CREATEKBN       NOT =  "3"
                  AND   SYU-DENPJTIKBN      NOT =  "3"
                  AND   SYU-NYUGAIKBN           =  "1"
                  AND ((WRK-SEL-SRYKACD         =  SPACE)
                   OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                      SYU-SRYKA               =  WRK-SEL-SRYKACD))
                  AND ((WRK-SEL-DRCD            =  SPACE)
                   OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                      SYU-DRCD                =  WRK-SEL-DRCD))
                  AND   FLG-PTINF               =  ZERO
                  AND   PTINF-TSTPTNUMKBN   NOT =  "1"
      *
                    IF     SMEI-NYUHEN-YMD <= WRK-LASTYMD
                      AND  SMEI-NYUHEN-YMD >= WRK-STARTYMD
                      MOVE   SMEI-NYUHEN-YMD(5:2)     TO    I3
                      MOVE   D100LNK-YMD1(5:2)        TO    I8
                      COMPUTE  I3  =  I3  -  I8  +  1
                      IF SMEI-NYUHEN-YMD(5:2)  <  D100LNK-YMD1(5:2)
                          COMPUTE  I3  =  12 - I3
                      END-IF
                      ADD  SMEI-NYUHEN-MONEY
                                       TO  SUM-YOSO(I3 21)
                      ADD  SMEI-NYUHEN-MONEY
                                       TO  SUM-YOSO(SUM-MONTHMAX 21)
                    END-IF
      *
                  END-IF
      *
                  PERFORM  320-SYUMEI-FETCH-SEC
              END-PERFORM
           END-PERFORM 
           PERFORM  320-SYUMEI-CLOSE-SEC
      *
      *    その他集計項目の加算
           PERFORM  320-ETC-CNT-SEC
      *
           .
       320-SYUNOU-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他項目集計処理
      *****************************************************************
       320-ETC-CNT-SEC      SECTION.
      *
           PERFORM  VARYING  I3     FROM 1  BY  1
                    UNTIL    I3     >=   SUM-MONTHMAX
      *
      *      病床数
             MOVE    SUM-BEDSU(I3)       TO  SUM-YOSO (I3 5)
             COMPUTE SUM-YOSO(SUM-MONTHMAX 5) =
                     SUM-YOSO(SUM-MONTHMAX 5) +
                     SUM-YOSO(I3 5)
      *
      *      総日数
             MOVE  D100LNK-YMD1          TO  WRK-TMP-YMD
             MOVE  D100LNK-YMD2          TO  WRK-TMP-LYMD
             MOVE    D100LNK-YMD2        TO   WRK-TMP-LYMD
             MOVE    D100LNK-YMD1        TO   WRK-TMP-YMD
             IF   D100LNK-YMD1(1:6)  =  D100LNK-YMD2(1:6)
               COMPUTE  WRK-SONISU  =  WRK-TMP-LDD  -  WRK-TMP-DD  +  1
             ELSE
               COMPUTE  WRK-TMP-MM  =  WRK-TMP-MM  +  I3  -  1
               IF  WRK-TMP-MM  >  12
                 ADD    1     TO   WRK-TMP-YY
                 COMPUTE  WRK-TMP-MM  =  WRK-TMP-MM  -  12
               END-IF
      *        対象年月最終日取得
               INITIALIZE                    WRK-SYMD1
               MOVE    WRK-TMP-YMD(1:6)      TO   WRK-SYMD1(1:6)
               PERFORM   31012-LASTDAY-GET-SEC
               MOVE    WRK-SYMD1             TO   WRK-TMP-LYMD
      *
               IF   I3  =  1
                 COMPUTE  WRK-SONISU  =  WRK-TMP-LDD  -  WRK-TMP-DD
                                      +  1
               ELSE
                 IF   WRK-TMP-LYMD(1:6)  =  D100LNK-YMD2(1:6)
                   MOVE    D100LNK-YMD2(7:2)   TO   WRK-SONISU
                 ELSE
                   MOVE    WRK-TMP-LDD         TO   WRK-SONISU
                 END-IF
               END-IF
             END-IF
      *
      *      総床数
             COMPUTE  WRK-SOBYOSYO =
                             SUM-YOSO (I3 5) * WRK-SONISU
      *
      *      稼働率
             INITIALIZE                WRK-RATE
                                       WRK-RATE2
             COMPUTE  WRK-RATE  =  SUM-YOSO (I3 4)  /  WRK-SOBYOSYO
             COMPUTE  WRK-RATE2 ROUNDED     =    WRK-RATE  *  100
             MOVE     WRK-RATE2    TO  WRK-RATE-Z9
             MOVE     WRK-RATE-Z9  TO  SUM-YOSO(I3 6)
      *
      *    請求数
             MOVE SUM-SYUNOU-CNT(I3)  TO  SUM-YOSO (I3 8)
             ADD  SUM-SYUNOU-CNT(I3)  TO  SUM-YOSO (SUM-MONTHMAX 8)
      *
      *    平均点
             INITIALIZE                WRK-TEN
             COMPUTE  WRK-TEN  ROUNDED    =   SUM-YOSO (I3 7) / 
                                              WRK-HNINZU(I3)
             MOVE     WRK-TEN  TO  SUM-YOSO (I3 9)
      *
      *    過日未収金残額
             COMPUTE  I4  =  I3   -  1
             IF    I3     =  1
                   MOVE      SUM-BF-MISYU  TO  WRK-MONEY   
             ELSE
                   COMPUTE   WRK-MONEY   =   SUM-YOSO (I4 20)
                                         +   SUM-YOSO (I4 22)
             END-IF
             MOVE  WRK-MONEY   TO   SUM-YOSO (I3 20)
      *
             EVALUATE WRK-NYUTENTTLKBN
               WHEN  1
                    MOVE 0   TO  SUM-YOSO(I3 40)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 40)
               WHEN  2                
                    MOVE 0   TO  SUM-YOSO(I3 41)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 41)
               WHEN  3                
                    MOVE 0   TO  SUM-YOSO(I3 42)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 42)
               WHEN  4                
                    MOVE 0   TO  SUM-YOSO(I3 43)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 43)
               WHEN  5                
                    MOVE 0   TO  SUM-YOSO(I3 44)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 44)
               WHEN  6                
                    MOVE 0   TO  SUM-YOSO(I3 45)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 45)
               WHEN  7                
                    MOVE 0   TO  SUM-YOSO(I3 46)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 46)
               WHEN  8                
                    MOVE 0   TO  SUM-YOSO(I3 47)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 47)
               WHEN  9                
                    MOVE 0   TO  SUM-YOSO(I3 48)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 48)
               WHEN  10                
                    MOVE 0   TO  SUM-YOSO(I3 49)
                    MOVE 0   TO  SUM-YOSO(SUM-MONTHMAX 49)
             END-EVALUATE
           END-PERFORM
      *
      *    平均点(計)
      *
           INITIALIZE        WRK-TEN
           COMPUTE  WRK-TEN  ROUNDED      =
                    SUM-YOSO (SUM-MONTHMAX 7) / 
                    WRK-HNINZU(SUM-MONTHMAX)
           MOVE     WRK-TEN
                    TO  SUM-YOSO (SUM-MONTHMAX 9)
      *
      *    過日未収金残額(計)
           MOVE  SUM-YOSO (SUM-MONTHMAX - 1 20) 
                    TO  SUM-YOSO (SUM-MONTHMAX 20)
      *
           .
       320-ETC-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計サブ処理
      *****************************************************************
       320-SYUNOU-CNT-SUB-SEC      SECTION.
      *
           INITIALIZE            FLG-INVALID     
           MOVE SYU-SRYYMD   TO  D124P-SRYYMD
           MOVE SYU-PTID     TO  D124P-PTID
           READ D124PT-FILE
                KEY      IS      D124P-KEY  
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      *
           IF   FLG-INVALID      =     1 
      *    診療月別対象患者ファイルに未処理のデータは患者数を集計処理を行う。
                INITIALIZE       FLG-INVALID     
                WRITE D124P
                       INVALID  KEY  
                       MOVE  1  TO FLG-INVALID
                END-WRITE
      *    当日預り金
      *    患者別に当日預り金を診療行為情報より取得する。
           END-IF
      *
      *    請求数のカウント
           ADD     1       TO   SUM-SYUNOU-CNT(I3)
      *
      *    各週マスタの読込
      *    保険組合情報
           INITIALIZE                           HKNCOMBI-REC
           MOVE    SYU-HOSPNUM               TO  COMB-HOSPNUM
           MOVE    SYU-PTID                 TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
      *    患者保険情報検索
           INITIALIZE                           PTHKNINF-REC
           MOVE    SYU-HOSPNUM               TO  PTHKN-HOSPNUM
           MOVE    SYU-PTID                 TO  PTHKN-PTID
           MOVE    COMB-HKNID               TO  PTHKN-HKNID
           PERFORM 900-PTHKNINF-READ-SEC
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =   1
      *    該当レコードが存在しない場合は単価を10円に設定する。
                MOVE 10     TO  HKN-TENTNK
           END-IF
           IF   SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
            OR  SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM
            OR  SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM
      *    患者労災情報検索
                INITIALIZE           PTRSIINF-REC
                MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
                MOVE SYU-PTID    TO  PTRSI-PTID
                MOVE COMB-HKNID  TO  PTRSI-HKNID
                PERFORM 900-PTRSIINF-READ-SEC
           END-IF
      *
      *    合計保険点数初期化 
                   MOVE   ZERO             TO  SYU-TOTAL-HKNTEN
      *    診療科保険点数            
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(I3 24)
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 24)
      **   合計点数に加算
                   ADD    SYU-SSU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    指導料保険点数            
                   ADD    SYU-SDO-HKNTEN
                          TO  SUM-YOSO (I3 25)
                   ADD    SYU-SDO-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 25)
      **   合計点数に加算
                   ADD    SYU-SDO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    在宅料保険点数            
                   ADD    SYU-ZTK-HKNTEN
                          TO  SUM-YOSO (I3 26)
                   ADD    SYU-ZTK-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 26)
      **   合計点数に加算
                   ADD    SYU-ZTK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    投薬料保険点数            
                   ADD    SYU-TYK-HKNTEN
                          TO  SUM-YOSO (I3 27)
                   ADD    SYU-TYK-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 27)
      **   合計点数に加算
                   ADD    SYU-TYK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    注射料保険点数            
                   ADD    SYU-CSY-HKNTEN
                          TO  SUM-YOSO (I3 28)
                   ADD    SYU-CSY-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 28)
      **   合計点数に加算FLG-SRYACT
                   ADD    SYU-CSY-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    処置料保険点数            
                   ADD    SYU-SYC-HKNTEN
                          TO  SUM-YOSO (I3 29)
                   ADD    SYU-SYC-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 29)
      **   合計点数に加算
                   ADD    SYU-SYC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    手術料保険点数            
                   ADD    SYU-SJT-HKNTEN
                          TO  SUM-YOSO (I3 30)
                   ADD    SYU-SJT-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 30)
      **   合計点数に加算
                   ADD    SYU-SJT-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    麻酔点数            
                   ADD    SYU-MSI-HKNTEN
                          TO  SUM-YOSO (I3 31)
                   ADD    SYU-MSI-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 31)
      **   合計点数に加算
                   ADD    SYU-MSI-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    検査料保険点数            
                   ADD    SYU-KNS-HKNTEN
                          TO  SUM-YOSO (I3 32)
                   ADD    SYU-KNS-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 32)
      **   合計点数に加算
                   ADD    SYU-KNS-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    X線料保険点数            
                   ADD    SYU-GZU-HKNTEN
                          TO  SUM-YOSO (I3 33)
                   ADD    SYU-GZU-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 33)
      **   合計点数に加算
                   ADD    SYU-GZU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    リハ点数            
                   ADD    SYU-ETC-HKNTEN
                          TO  SUM-YOSO (I3 34)
                   ADD    SYU-ETC-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 34)
      **   合計点数に加算
                   ADD    SYU-ETC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    精神科専門点数            
                   ADD    SYU-SSN-HKNTEN
                          TO  SUM-YOSO (I3 35)
                   ADD    SYU-SSN-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 35)
      **   合計点数に加算
                   ADD    SYU-SSN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    放射線治療点数            
                   ADD    SYU-HOU-HKNTEN
                          TO  SUM-YOSO (I3 36)
                   ADD    SYU-HOU-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 36)
      **   合計点数に加算
                   ADD    SYU-HOU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    病理診断点数            
                   ADD    SYU-BYR-HKNTEN
                          TO  SUM-YOSO (I3 50)
                   ADD    SYU-BYR-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 50)
      **   合計点数に加算
                   ADD    SYU-BYR-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    入院保険点数              
                   ADD    SYU-NYN-HKNTEN
                          TO  SUM-YOSO (I3 37)
                   ADD    SYU-NYN-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 37)
      **   合計点数に加算
                   ADD    SYU-NYN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    療養店頭手当点数
                   ADD    SYU-RYO-HKNTEN
                          TO  SUM-YOSO (I3 38)
                   ADD    SYU-RYO-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 38)
      **   合計点数に加算
                   ADD    SYU-RYO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    保険適用外金額            
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (SUM-MONTHMAX 39)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 39)
      *
      *    自費１            
                   ADD    SYU-JIHI-1       
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-1
                          TO  SUM-YOSO (SUM-MONTHMAX 40)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 40)
      *
      *    自費２            
                   ADD    SYU-JIHI-2       
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-2
                          TO  SUM-YOSO (SUM-MONTHMAX 41)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 41)
      *
      *    自費３            
                   ADD    SYU-JIHI-3       
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-3
                          TO  SUM-YOSO (SUM-MONTHMAX 42)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 42)
      *
      *    自費４            
                   ADD    SYU-JIHI-4       
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-4
                          TO  SUM-YOSO (SUM-MONTHMAX 43)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 43)
      *
      *    自費５            
                   ADD    SYU-JIHI-5       
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-5
                          TO  SUM-YOSO (SUM-MONTHMAX 44)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 44)
      *
      *    自費６            
                   ADD    SYU-JIHI-6       
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-6
                          TO  SUM-YOSO (SUM-MONTHMAX 45)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 45)
      *
      *    自費７            
                   ADD    SYU-JIHI-7       
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-7
                          TO  SUM-YOSO (SUM-MONTHMAX 46)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 46)
      *
      *    自費８            
                   ADD    SYU-JIHI-8       
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-8
                          TO  SUM-YOSO (SUM-MONTHMAX 47)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 47)
      *
      *    自費９            
                   ADD    SYU-JIHI-9       
                          TO  SUM-YOSO (I3 48)
                   ADD    SYU-JIHI-9
                          TO  SUM-YOSO (SUM-MONTHMAX 48)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (I3 48)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 48)
      *
      *    自費１０            
                   ADD    SYU-JIHI-10       
                          TO  SUM-YOSO (I3 49)
                   ADD    SYU-JIHI-10
                          TO  SUM-YOSO (SUM-MONTHMAX 49)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (I3 49)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (SUM-MONTHMAX 49)
      *
      *    保険内点数(合計保険点数)
                   ADD    SYU-TOTAL-HKNTEN
                          TO  SUM-YOSO (I3 7)
                   ADD    SYU-TOTAL-HKNTEN   
                          TO  SUM-YOSO(SUM-MONTHMAX 7)
      *
                   INITIALIZE     WRK-HKNNAI
      *
      *    保険内負担金              
                   IF   ( (SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
                       AND  SYU-PTFTNRATE  NOT =  ZERO )
                       OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
                       OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
                       AND  PTRSI-JIBAISEIKBN  NOT  =  2
      *            労災初診金額金額
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                        ADD    SYU-RSISHOSHIN-MONEY   TO   WRK-HKNNAI
      *
      *            労災再診金額金額
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                        ADD   SYU-RSISAISHIN-MONEY    TO   WRK-HKNNAI
      *
      *            労災指導金額金額
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                        ADD   SYU-RSISDO-MONEY    TO   WRK-HKNNAI
      *
      *            労災その他金額金額
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                        ADD    SYU-RSIETC-MONEY   TO   WRK-HKNNAI
      *
                        ADD  SYU-HKNTEKMONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-HKNTEKMONEY
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                        ADD    SYU-HKNTEKMONEY   TO   WRK-HKNNAI
                   ELSE
      *            主保険分負担金額
      *
                   IF      SYU-SYUCOMPTOTAL  NOT = ZERO 
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                           MOVE    SYU-SYUCOMPTOTAL    TO   WRK-HKNNAI
                   ELSE IF SYU-KOH1COMPTOTAL NOT = ZERO
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                           MOVE    SYU-KOH1COMPTOTAL   TO   WRK-HKNNAI
                   ELSE IF SYU-KOH2COMPTOTAL NOT = ZERO
                           ADD SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                           MOVE    SYU-KOH2COMPTOTAL   TO   WRK-HKNNAI
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                           MOVE    SYU-KOH3COMPTOTAL   TO   WRK-HKNNAI
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(SUM-MONTHMAX 10)
                           MOVE    SYU-KOH4COMPTOTAL   TO   WRK-HKNNAI
                   END-IF 
                   END-IF 
                   END-IF 
                   END-IF 
                   END-IF
      *
                   END-IF
      *
      *    保険外金額
                   COMPUTE WRK-JIHI    =
                           SYU-TOTAL-TGMONEY  
                       +   SYU-TOTAL-TGMONEY-TAX  
                       +   SYU-JIHI-TOTAL
                       +   SYU-JIHI-TOTAL-TAX  
                   ADD    WRK-JIHI
                          TO  SUM-YOSO(I3 11)
                   ADD    WRK-JIHI
                          TO  SUM-YOSO(SUM-MONTHMAX 11)
      *    食事負担額 12
                   COMPUTE WRK-FTN     =
                          SYU-RYOYOHI-SKJ
                      +   SYU-RYOYOHI-SKJ-JIHI
                      +   SYU-RYOYOHI-LIFE
                   ADD    WRK-FTN
                          TO  SUM-YOSO(I3 12)
                   ADD    WRK-FTN
                          TO  SUM-YOSO(SUM-MONTHMAX 12)
      *    標準負担額 13
                   COMPUTE WRK-FTN     =
                          SYU-SKYMONEY-SKJ-KEI
                      +   SYU-SKYMONEY-SKJ-JIHI-KEI
                      +   SYU-SKYMONEY-LIFE-KEI
                   ADD    WRK-FTN
                          TO  SUM-YOSO(I3 13)
                   ADD    WRK-FTN
                          TO  SUM-YOSO(SUM-MONTHMAX 13)
      *    室料差額 14
                   ADD    SYU-RMSAGAKU
                          TO  SUM-YOSO(I3 14)
                   ADD    SYU-RMSAGAKU
                          TO  SUM-YOSO(SUM-MONTHMAX 14)
      *    減免金額
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(I3 15)
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(SUM-MONTHMAX 15)
      *    助成金額の算出
                   COMPUTE  WRK-MONEY  =  WRK-HKNNAI
                                       +  SYU-CHOSEI  +  SYU-RMSAGAKU
                                       +  WRK-FTN
                   COMPUTE  WRK-JOSEI  =  WRK-MONEY
                                       -  SYU-DISCOUNT-MONEY
                   INITIALIZE  FLG-JIHI
                   IF   SYU-DISCOUNT-MONEY  =  ZERO
                     EVALUATE  SYU-SYUHKNNUM
      *    労災
                     WHEN  WRK-CONS-ROUSAI-HKNNUM
      *    労災（自費分）
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN3J
                       WHEN  1
                         COMPUTE  WRK-JIHI ROUNDED  =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10
                       WHEN  2
                         COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10
                       WHEN  3
                         IF   WRK-JIHI(9:1)  NOT =  0
                           COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                           COMPUTE  WRK-JIHI =  WRK-JIHI  *  10  +  10
                         END-IF
                       WHEN  0
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  +  WRK-JIHI
                         MOVE     1         TO  FLG-JIHI
                       END-EVALUATE
      *    労災（保険分）
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN3
                       WHEN  1
                         COMPUTE  WRK-JOSEI ROUNDED  =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10
                       WHEN  2
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10
                       WHEN  3
                         IF   WRK-JOSEI(11:1)  NOT =  0
                           COMPUTE  WRK-JOSEI =  WRK-JOSEI  /  10
                           COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10  +  10
                         END-IF
                       END-EVALUATE
      *    自賠責
                     WHEN  WRK-CONS-JIBAI-HKNNUM
      *    自賠責（自費分）
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN4J
                       WHEN  1
                         COMPUTE  WRK-JIHI ROUNDED  =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10
                       WHEN  2
                         COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10 
                       WHEN  3
                         IF   WRK-JIHI(9:1)  NOT =  0
                           COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                           COMPUTE  WRK-JIHI =  WRK-JIHI  *  10  +  10
                         END-IF
                       WHEN  0
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  +  WRK-JIHI
                         MOVE     1         TO  FLG-JIHI
                       END-EVALUATE
      *    自賠責（保険分）
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN4
                       WHEN  1
                         COMPUTE  WRK-JOSEI ROUNDED  =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10
                       WHEN  2
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10 
                       WHEN  3
                         IF   WRK-JOSEI(11:1)  NOT =  0
                           COMPUTE  WRK-JOSEI =  WRK-JOSEI  /  10
                           COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10  +  10
                         END-IF
                       END-EVALUATE
      *    公害（自費分）
                     WHEN  WRK-CONS-KOGAI-HKNNUM
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN5J
                       WHEN  1
                         COMPUTE  WRK-JIHI ROUNDED  =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10
                       WHEN  2
                         COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10 
                       WHEN  3
                         IF   WRK-JIHI(9:1)  NOT =  0
                           COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                           COMPUTE  WRK-JIHI =  WRK-JIHI  *  10  +  10
                         END-IF
                       END-EVALUATE
      *    医保
                     WHEN  OTHER
      *    医保（自費分）
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN2J
                       WHEN  1
                         COMPUTE  WRK-JIHI ROUNDED  =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10
                       WHEN  2
                         COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                         COMPUTE  WRK-JIHI =  WRK-JIHI  *  10 
                       WHEN  3
                         IF   WRK-JIHI(9:1)  NOT =  0
                           COMPUTE  WRK-JIHI =  WRK-JIHI  /  10
                           COMPUTE  WRK-JIHI =  WRK-JIHI  *  10  +  10
                         END-IF
                       WHEN  0
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  +  WRK-JIHI
                         MOVE     1         TO  FLG-JIHI
                       END-EVALUATE
      *    医保（保険分）
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN2
                       WHEN  1
                         COMPUTE  WRK-JOSEI ROUNDED  =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10
                       WHEN  2
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10 
                       WHEN  3
                         IF   WRK-JOSEI(11:1)  NOT =  0
                           COMPUTE  WRK-JOSEI =  WRK-JOSEI  /  10
                           COMPUTE  WRK-JOSEI =  WRK-JOSEI  *  10  +  10
                         END-IF
                       END-EVALUATE
                     END-EVALUATE
      *    減免有り
                   ELSE
                     EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                     WHEN  1
                       COMPUTE  WRK-JOSEI  ROUNDED  =  WRK-JOSEI  /  10
                       COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10
                     WHEN  2
                       COMPUTE  WRK-JOSEI  =  WRK-JOSEI
                                                 /  10
                       COMPUTE  WRK-JOSEI  =  WRK-JOSEI
                                                 *  10 
                     WHEN  3
                       IF   WRK-JOSEI(11:1)  NOT =  0
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10  +  10
                       END-IF
                     END-EVALUATE
                   END-IF
      *
                   IF  FLG-JIHI  NOT  =  1
                     COMPUTE  WRK-JOSEI  =  WRK-JOSEI  +  WRK-JIHI
                   END-IF
      *    助成金額
                   COMPUTE  WRK-JOSEI  =  WRK-JOSEI  -  SYU-SKYMONEY
                   ADD    WRK-JOSEI    TO   SUM-YOSO(I3 16) 
                   ADD    WRK-JOSEI    TO   SUM-YOSO(SUM-MONTHMAX 16)
      *    消費税              
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(I3 17)
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(SUM-MONTHMAX 17)
      *    調整金              
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(I3 18)
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(SUM-MONTHMAX 18)
      *    窓口請求金額              
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(I3 19)
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(SUM-MONTHMAX 19)
      * 入金額(当日の診療に対する入金額)   
      *             MOVE   ZERO  TO  FLG-SYUMEI
      *             PERFORM  320-SYUMEI-SELECT-SEC
      *             PERFORM  UNTIL  FLG-SYUMEI  = 1
      *                IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
      *                 AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
      *                    MOVE   SMEI-NYUHEN-YMD(5:2)     TO    I3
      *                    MOVE   D100LNK-YMD1(5:2)        TO    I8
      *                    COMPUTE  I3  =  I3  -  I8  +  1
      *                    IF SMEI-NYUHEN-YMD(5:2)  <  D100LNK-YMD1(5:2)
      *                        COMPUTE  I3  =  12 - I3
      *                    END-IF
      *                    ADD    SMEI-NYUHEN-MONEY
      *                           TO  SUM-YOSO(I3 21)
      *                    ADD    SMEI-NYUHEN-MONEY
      *                           TO  SUM-YOSO(SUM-MONTHMAX 21)
      *                END-IF
      *             PERFORM  320-SYUMEI-FETCH-SEC
      *             END-PERFORM 
      *             PERFORM  320-SYUMEI-CLOSE-SEC
      *    未収金残額              
                   COMPUTE     WRK-MONEY        =
                               SYU-SKYMONEY     -    SYU-NYUKIN-TOTAL
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(I3 22)
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(SUM-MONTHMAX 22)
      *
      *    入金額から当日預り金を引き去る
                   EVALUATE WRK-NYUTENTTLKBN
                      WHEN  1                
                            COMPUTE SUM-YOSO(I3 21) =
                                 SUM-YOSO(I3 21) -
                                 SYU-JIHI-1      -
                                 SYU-JIHI-1-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                 SUM-YOSO(SUM-MONTHMAX 21) -
                                 SYU-JIHI-1                -
                                 SYU-JIHI-1-TAX
                      WHEN  2                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-2      -
                                SYU-JIHI-2-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-2                -
                                SYU-JIHI-2-TAX
                      WHEN  3                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-3      -
                                SYU-JIHI-3-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-3                -
                                SYU-JIHI-3-TAX
                      WHEN  4                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-4      -
                                SYU-JIHI-4-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-4                -
                                SYU-JIHI-4-TAX
                      WHEN  5                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-5      -
                                SYU-JIHI-5-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-5                -
                                SYU-JIHI-5-TAX
                      WHEN  6                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-6      -
                                SYU-JIHI-6-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-6                -
                                SYU-JIHI-6-TAX
                      WHEN  7                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-7      -
                                SYU-JIHI-7-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-7                -
                                SYU-JIHI-7-TAX
                      WHEN  8                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-8      -
                                SYU-JIHI-8-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-8                -
                                SYU-JIHI-8-TAX
                      WHEN  9                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-9      -
                                SYU-JIHI-9-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-9                -
                                SYU-JIHI-9-TAX
                      WHEN  10                
                            COMPUTE SUM-YOSO(I3 21) =
                                SUM-YOSO(I3 21) -
                                SYU-JIHI-10      -
                                SYU-JIHI-10-TAX
                            COMPUTE SUM-YOSO(SUM-MONTHMAX 21) =
                                SUM-YOSO(SUM-MONTHMAX 21) -
                                SYU-JIHI-10                -
                                SYU-JIHI-10-TAX
                   END-EVALUATE
      *
           IF  SYU-TOTAL-HKNTEN  NOT =  ZERO
             ADD    1                    TO  WRK-HNINZU(I3)
             ADD    1                    TO  WRK-HNINZU(SUM-MONTHMAX)
           END-IF
      *             
           .
       320-SYUNOU-CNT-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金集計処理
      *****************************************************************
       320-SYUMEI-CNT-SUB-SEC      SECTION.
      *
           MOVE  ZERO              TO   WRK-SMEI-NYUHEN-MONEY
           MOVE  D100LNK-YMD1(5:2) TO   WRK-START-MM
           MOVE  ZERO              TO   I3
           PERFORM  320-SYUMEI-SELECT-SEC
           PERFORM   UNTIL  FLG-SYUMEI = 1
             IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
               AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
               MOVE  SMEI-NYUHEN-YMD(5:2)   TO  I3
               COMPUTE  I3 = I3 - WRK-START-MM + 1
                 IF SMEI-NYUHEN-YMD(5:2)  <  D100LNK-YMD1(5:2)
                   COMPUTE  I3  =  12 - I3
                 END-IF
               ADD   SMEI-NYUHEN-MONEY  TO  SUM-YOSO(I3 21)
               ADD   SMEI-NYUHEN-MONEY  TO  SUM-YOSO(SUM-MONTHMAX 21)
             END-IF
             PERFORM  320-SYUMEI-FETCH-SEC
           END-PERFORM.
           PERFORM  320-SYUMEI-CLOSE-SEC
      *
           .
       320-SYUMEI-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    入金  SELECT
      *****************************************************************
       320-SYUMEI-SELECT-SEC           SECTION.
      *
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    "1"                 TO  SMEI-NYUGAIKBN
           MOVE    D100LNK-YMD1        TO  SMEI-STDAILYKEY
           MOVE    "0000"              TO  SMEI-STDAILYKEY(9:4)
           MOVE    D100LNK-YMD2        TO  SMEI-EDDAILYKEY
           MOVE    "9999"              TO  SMEI-EDDAILYKEY(9:4)
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
                PERFORM  320-SYUMEI-FETCH-SEC
           ELSE
                MOVE  1                 TO  FLG-SYUMEI
           END-IF
           .
       320-SYUMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *   入金  FETCH
      *****************************************************************
       320-SYUMEI-FETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syumei"         TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    ZERO            TO  FLG-SYUMEI
             MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
             MOVE    1               TO  FLG-SYUMEI
           END-IF
           .
       320-SYUMEI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *  入金  CLOSE
      *****************************************************************
       320-SYUMEI-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syumei"         TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
            .
       320-SYUMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SUB-SEC      SECTION.
      *
           IF    FLG-JYURRK       = ZERO
            AND  JYURRK-NYUGAIKBN = "2"
                 MOVE JYURRK-SRYYMD(7:2)  TO  IDX7G
                 MOVE JYURRK-HOSPNUM       TO  ACCT-HOSPNUM  
                 MOVE JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                 MOVE JYURRK-PTID         TO  ACCT-PTID
                 MOVE JYURRK-SRYKA        TO  ACCT-SRYKA
                 MOVE JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                 MOVE "11"                TO  ACCT-SRYKBN
                 PERFORM 900-SRYACCT-READ-SEC
                 IF      FLG-SRYACCT      =   ZERO
                   AND   ACCT-DAY(1 IDX7) >   ZERO
      *    初診の件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 2)
                         ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 2)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 5)
                 ELSE
                      MOVE JYURRK-HOSPNUM TO  ACCT-HOSPNUM  
                      MOVE JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                      MOVE JYURRK-PTID         TO  ACCT-PTID
                      MOVE JYURRK-SRYKA        TO  ACCT-SRYKA
                      MOVE JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                      MOVE "12"                TO  ACCT-SRYKBN
                      PERFORM 900-SRYACCT-READ-SEC
                      IF  FLG-SRYACCT          =   ZERO
                      AND ACCT-DAY(1 IDX7)     >   ZERO
      *    再診の件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 3)
                         ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 3)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 5)
                      ELSE
      *    その他件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 4)
                         ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 4)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 5)
                      END-IF
                 END-IF
           END-IF
      *
      *    新患件数
           PERFORM  900-JYURRK-SELECT-SEC 
           IF       FLG-JYURRK       =   ZERO
             AND    JYURRK-SRYYMD    =   SYU-SRYYMD 
      *    新患件数としてカウントする。
                    ADD  1  TO  SUM-YOSO(I3 6)
                    ADD  1  TO  SUM-YOSO(SUM-MONTHMAX 6)
           END-IF 
           PERFORM  900-JYURRK-CLOSE-SEC 
      *
           .           
       320-JYURRK-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    預り金対象者抽出者処理
      *****************************************************************
       320-AZUKARI-GET-SEC      SECTION.
      *
      *    集計初期値
           INITIALIZE                      SRYACT-REC
           MOVE   PTNYUINRRK-HOSPNUM    TO  SRY-HOSPNUM  
           MOVE   "1"                  TO  SRY-NYUGAIKBN 
           MOVE   PTNYUINRRK-PTID      TO  SRY-PTID
           MOVE   WRK-INI-AZUKARICD    TO  SRY-SRYCD(1)
                                           SRY-SRYCD(2)
                                           SRY-SRYCD(3)
                                           SRY-SRYCD(4)
                                           SRY-SRYCD(5)
           MOVE   I3                   TO  WRK-ACCT-IN
      *    集計日範囲
           MOVE   WRK-TMP-YMD(1:6)     TO  SRY-SRYYM
           MOVE   WRK-TMP-YMD(7:2)     TO  WRK-ACCT-SDAY
           MOVE   WRK-TMP-LYMD(7:2)    TO  WRK-ACCT-EDAY
      *    
           PERFORM  911-SRY-SELECT-SEC
           PERFORM  UNTIL      FLG-SRYACT  = 1
               IF    FLG-SRYACT     =   ZERO
                AND  SRY-NYUGAIKBN  =   "1"
                AND (SUM-SRYKACD    =   SRY-SRYKA
                 OR  SUM-SRYKACD    =   "99")
                     PERFORM VARYING IDX3  FROM  1 BY 1
                             UNTIL   IDX3  >  5
                        IF   SRY-SRYCD(IDX3)    = WRK-INI-AZUKARICD
                         AND SRY-JIHIMONEY(IDX3) > ZERO
                             MOVE SRY-JIHIMONEY(IDX3)  
                                                TO  WRK-JIHIMONEY
                             INITIALIZE             SRYACCT-REC
                             MOVE SRY-HOSPNUM    TO  ACCT-HOSPNUM  
                             MOVE SRY-NYUGAIKBN TO  ACCT-NYUGAIKBN
                             MOVE SRY-PTID      TO  ACCT-PTID
                             MOVE SRY-SRYKA     TO  ACCT-SRYKA
                             MOVE SRY-SRYYM     TO  ACCT-SRYYM
                             MOVE SRY-SRYKBN    TO  ACCT-SRYKBN
                             MOVE SRY-ZAINUM    TO  ACCT-ZAINUM
                             PERFORM 900-SRYACCT-SELECT-SEC
                             PERFORM 900-SRYACCT-FETCH-SEC
                             PERFORM  UNTIL FLG-SRYACCT    =  1
                                 PERFORM 320-AZUKARI-GET-SUB-SEC
                                 PERFORM 900-SRYACCT-FETCH-SEC
                             END-PERFORM
                             PERFORM 900-SRYACCT-CLOSE-SEC
                        END-IF
                    END-PERFORM           
               END-IF
               PERFORM 911-SRY-FETCH-SEC
           END-PERFORM           
           PERFORM 911-SRY-CLOSE-SEC
      *
           .
       320-AZUKARI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    預り金加算処理
      *****************************************************************
       320-AZUKARI-GET-SUB-SEC      SECTION.
      *
           PERFORM VARYING  IDX4   FROM  WRK-ACCT-SDAY  BY 1
                      UNTIL IDX4    >    WRK-ACCT-EDAY
               IF  ACCT-DAY(1 IDX4) >    ZERO
                   PERFORM VARYING  IDX5  FROM  2  BY 4
                      UNTIL  IDX5   >    4
                        IF  ACCT-DAY(IDX5 IDX4) > 0
                            INITIALIZE          JYURRK-REC
                            MOVE ACCT-HOSPNUM     TO JYURRK-HOSPNUM
                            MOVE ACCT-PTID       TO JYURRK-PTID
                            MOVE ACCT-NYUGAIKBN  TO JYURRK-NYUGAIKBN
                            MOVE ACCT-SRYKA      TO JYURRK-SRYKA
                            MOVE ACCT-SRYYM      TO JYURRK-SRYYMD(1:6)
                            MOVE IDX4(3:2)       TO JYURRK-SRYYMD(7:2)
                            MOVE IDX5            TO IDX6
                            ADD      -1          TO IDX6
                            MOVE IDX6            TO JYURRK-RENNUM
                            MOVE ACCT-HKNCOMBI   TO JYURRK-HKNCOMBINUM
                            PERFORM 900-JYURRK-DRCD-READ-SEC
                            IF  ( WRK-SEL-DRCD     = SPACE
                             OR ( WRK-SEL-DRCD NOT = SPACE
                              AND WRK-SEL-DRCD     = JYURRK-DRCD(2:4)))
                             AND  FLG-JYURRK       = ZERO
                                  COMPUTE WRK-AZUKARI =
                                    ACCT-DAY(IDX5 IDX4) * WRK-JIHIMONEY
                                  ADD  WRK-AZUKARI
                                   TO  SUM-YOSO(WRK-ACCT-IN 23)
                                  ADD  WRK-AZUKARI
                                   TO  SUM-YOSO(SUM-MONTHMAX 23)
                            END-IF
                        END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       320-AZUKARI-GET-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療月インデックス判別処理
      *****************************************************************
       320-SRYYMD-GROUPING-SEC      SECTION.
      *
             MOVE  1             TO  I3
             MOVE  SUM-MONTHMAX    TO  I4
             PERFORM  UNTIL  I3  >   I4
                      IF   SUM-MONTH(I3)  =  WRK-SRYYMD(1:6)
                           MOVE  ZERO   TO I4
                      ELSE
                           ADD   1      TO I3
                      END-IF
             END-PERFORM
      *
           .
       320-SRYYMD-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    SYU-PTID          TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み       
               INITIALIZE                    PTHKNINF-REC
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
               MOVE    SYU-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID        TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF    
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3202-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    患者保険情報読み込み       
           INITIALIZE                    PTHKNINF-REC
           MOVE    PTNYUINRRK-HOSPNUM TO  PTHKN-HOSPNUM
           MOVE    PTNYUINRRK-PTID   TO  PTHKN-PTID
           MOVE    COMB-HKNID        TO  PTHKN-HKNID
           PERFORM 900-PTHKNINF-READ-SEC
           .
       3202-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集サブ処理                                           *
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           INITIALIZE                   D124
           INITIALIZE                   D1241
      *    帳票サブタイトル
           MOVE    "(入院月報)"         TO  D124-SUBTITLE
           MOVE    "(入院月報)"         TO  D1241-SUBTITLE
      *    印字対象診療年月
           MOVE    WRK-SRYYMD1(1:10)    TO  D124-SRYYM
           MOVE    WRK-SRYYMD1(1:10)    TO  D1241-SRYYM
      *    診療科名称
           MOVE    SUM-SRYKANAME        TO  D124-SRYKA
           MOVE    SUM-SRYKANAME        TO  D1241-SRYKA
      *    医師名称
           MOVE    SUM-DRNAME           TO  D124-DRNAME
           MOVE    SUM-DRNAME           TO  D1241-DRNAME
      *
      *    帳票出力年月日
           MOVE    WRK-SYSYMDWH         TO  D124-SYSYMD
           MOVE    WRK-SYSYMDWH         TO  D1241-SYSYMD
      *    転帰テキスト 
           MOVE    SUM-TENKI-MSG(1)     TO  D124-TENKI(1)
           MOVE    SUM-TENKI-MSG(2)     TO  D124-TENKI(2)
           MOVE    SUM-TENKI-MSG(3)     TO  D124-TENKI(3)
      * 
      *    入院の自費タイトル設定
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >  10
             MOVE  WRK-TJIHI(IDX2)        TO  D1241-TJIHI(IDX2)
           END-PERFORM
      *
           MOVE    1                    TO  I3
      *
           IF  SUM-MONTHMAX  >  13
             MOVE    12                   TO  I5
           ELSE
             MOVE    SUM-MONTHMAX         TO  I5
           END-IF
      *
      *    テーブル設定
           PERFORM  UNTIL  I3  >  SUM-MONTHMAX
      *
             PERFORM  VARYING  I6  FROM  1  BY  1
                      UNTIL    I6  >  I5
                      OR       I3  >  SUM-MONTHMAX
      *
               IF  I3  =  SUM-MONTHMAX
                 MOVE    "計"                   TO  D124-TDAY(13)
                                                    D1241-TDAY(13)
                                                    SUM-MONTH(I3)
               ELSE
                 MOVE    SUM-MONTH(I3)(5:2)     TO  D124-TDAY(I6)(1:2)
                 MOVE    "月"                   TO  D124-TDAY(I6)(3:2)
                 MOVE    SUM-MONTH(I3)(5:2)     TO  D1241-TDAY(I6)(1:2)
                 MOVE    "月"                   TO  D1241-TDAY(I6)(3:2)
      *
                 MOVE    D124-TDAY(I6)          TO  WRK-MAE-INPUT
                 PERFORM  2009-HENKAN-SEC
                 MOVE    WRK-OUT-INPUT          TO  D124-TDAY(I6)
                                                    D1241-TDAY(I6)
                                                    SUM-MONTH(I3)
               END-IF
      *
               PERFORM  VARYING  I4  FROM  2  BY  1
                        UNTIL    I4  >  50
      *
                 COMPUTE  I7  =  I4  -  1
      *
                 IF  I3  =  SUM-MONTHMAX
                   IF     I4 =  1
                     OR   I4 =  5
                     OR   I4 =  6
                     OR ( I4 = 40 AND WRK-TJIHI-FLG(1) = ZERO )
                     OR ( I4 = 41 AND WRK-TJIHI-FLG(2) = ZERO )
                     OR ( I4 = 42 AND WRK-TJIHI-FLG(3) = ZERO )
                     OR ( I4 = 43 AND WRK-TJIHI-FLG(4) = ZERO )
                     OR ( I4 = 44 AND WRK-TJIHI-FLG(5) = ZERO )
                     OR ( I4 = 45 AND WRK-TJIHI-FLG(6) = ZERO )
                     OR ( I4 = 46 AND WRK-TJIHI-FLG(7) = ZERO )
                     OR ( I4 = 47 AND WRK-TJIHI-FLG(8) = ZERO )
                     OR ( I4 = 48 AND WRK-TJIHI-FLG(9) = ZERO )
                     OR ( I4 = 49 AND WRK-TJIHI-FLG(10) = ZERO )
      *
                     IF  I7  >  37
                       IF  I7  =  49
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  SPACE  TO  D124-YOSO(13 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  37 
                         MOVE  SPACE  TO  D1241-YOSO(13 IDX9)
                       END-IF
                     ELSE
                       MOVE  SPACE  TO  D124-YOSO(I6 I7)
                     END-IF
      *
                   ELSE
      *
                     MOVE  SUM-YOSO(I3 I4)  TO  WRK-S9
                     IF  I7  >  37
                       IF  I7  =  49
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  WRK-S9  TO  D124-YOSO(13 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  37
                         MOVE  WRK-S9  TO  D1241-YOSO(13 IDX9)
                       END-IF
                     ELSE
                         MOVE  WRK-S9  TO  D124-YOSO(13 I7)
                     END-IF
      *
                   END-IF
      *
                 ELSE
      *
                   IF     I4 =  1
                     OR ( I4 = 40 AND WRK-TJIHI-FLG(1)  = ZERO )
                     OR ( I4 = 41 AND WRK-TJIHI-FLG(2)  = ZERO )
                     OR ( I4 = 42 AND WRK-TJIHI-FLG(3)  = ZERO )
                     OR ( I4 = 43 AND WRK-TJIHI-FLG(4)  = ZERO )
                     OR ( I4 = 44 AND WRK-TJIHI-FLG(5)  = ZERO )
                     OR ( I4 = 45 AND WRK-TJIHI-FLG(6)  = ZERO )
                     OR ( I4 = 46 AND WRK-TJIHI-FLG(7)  = ZERO )
                     OR ( I4 = 47 AND WRK-TJIHI-FLG(8)  = ZERO )
                     OR ( I4 = 48 AND WRK-TJIHI-FLG(9)  = ZERO )
                     OR ( I4 = 49 AND WRK-TJIHI-FLG(10) = ZERO )
      *
                     IF  I7  >  37
                       IF  I7  =  49
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  SPACE  TO  D124-YOSO(I6 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  37
                         MOVE  SPACE  TO  D1241-YOSO(I6 IDX9)
                       END-IF
                     ELSE
                       MOVE  SPACE  TO  D124-YOSO(I6 I7)
                     END-IF
      *
                   ELSE
      *
                     MOVE  SUM-YOSO(I3 I4)  TO  WRK-S9
                     IF  I7  >  37
                       IF  I7  =  49
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  WRK-S9  TO  D124-YOSO(I6 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  37
                         MOVE  WRK-S9  TO  D1241-YOSO(I6 IDX9)
                       END-IF
                     ELSE
                       MOVE  WRK-S9  TO  D124-YOSO(I6 I7)
                     END-IF
      *
                   END-IF
      *
                 END-IF
      *
               END-PERFORM
               ADD     1                    TO  I3
             END-PERFORM
      *
             PERFORM  340-PRINT-OUT-SEC
             MOVE    13                   TO  I5
             INITIALIZE                   D124-DATA
             INITIALIZE                   D1241-DATA
      *
           END-PERFORM 
      *
      *+
      *    CSV編集処理
           PERFORM  800-CSV-HEAD-SEC
           PERFORM  400-CSV-DATA-SEC
      *+
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理                                               *
      *****************************************************************
       340-PRINT-OUT-SEC          SECTION.
      *
      *    帳票ページ番号
           ADD     1                              TO  CNT-PAGE
           MOVE    CNT-PAGE                       TO  WRK-PAGE
           MOVE    WRK-PAGE                       TO  D124-PAGE
           ADD     1                              TO  CNT-PAGE
           MOVE    CNT-PAGE                       TO  WRK-PAGE
           MOVE    WRK-PAGE                       TO  D1241-PAGE
      *
           INITIALIZE                             ORCSPRTAREA
           MOVE    "INS"                          TO  SPRT-MODE
           MOVE    D100LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    D100LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    D100LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    D100LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    D100LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    D100LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    D100LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    D100LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
      *
           IF  D100LNK-YMD1  <  "20080401"
             MOVE    "A00000D124O.red"              TO  SPRT-PRTID
           ELSE
             MOVE    "A00000D124N.red"              TO  SPRT-PRTID
           END-IF
      *
           MOVE    "基本台帳(入院月報)"           TO  SPRT-TITLE
           MOVE    D124                           TO  SPRT-PRTDATA
           MOVE    D100LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    D100LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    D100LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                      USING
                                                  ORCSPRTAREA
                                                  SPA-AREA
      *
           IF  SPRT-RETURN  =  ZERO
             MOVE    "A00000D1241.red"            TO  SPRT-PRTID
             MOVE    "基本台帳(入院月報)"         TO  SPRT-TITLE
             MOVE    D1241                        TO  SPRT-PRTDATA
             MOVE    D100LNK-PRTKANRI-TERMID      TO  SPRT-TERMID
             MOVE    D100LNK-PRTKANRI-OPID        TO  SPRT-OPID
             MOVE    D100LNK-PRTKANRI-PRTNM       TO  SPRT-PRTNM
             CALL    "ORCSPRT"                    USING
                                                  ORCSPRTAREA
                                                  SPA-AREA
      *
             IF  SPRT-RETURN  =  ZERO
               CONTINUE
             ELSE
               MOVE    "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
               PERFORM  500-ERR-HENSYU-SEC
             END-IF
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *+
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                    TO  IDXP
      *
      *    印字対象診療年月
           INITIALIZE  WRK-ITEM
           MOVE    "対象年："           TO  WRK-ITEM(1:8)
           MOVE    WRK-SRYYMD1(1:10)    TO  WRK-ITEM(9:10)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科名称
           INITIALIZE  WRK-ITEM
           MOVE    "診療科："           TO  WRK-ITEM(1:8)
           MOVE    SUM-SRYKANAME        TO  WRK-ITEM(9:20)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    医師名称
           INITIALIZE  WRK-ITEM
           MOVE    "医師："             TO  WRK-ITEM(1:6)
           MOVE    SUM-DRNAME           TO  WRK-ITEM(7:30)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    帳票出力年月日
           INITIALIZE  WRK-ITEM
           MOVE    WRK-SYSYMDWH         TO  WRK-ITEM(1:22)
           MOVE    "  発行"             TO  WRK-ITEM(23:6)
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           MOVE    1    TO  IDX2
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  CSV-HEAD-MAX
      *
      *      行項目印字
             INITIALIZE  WRK-ITEM
      *
             IF  IDX4  <  41
               MOVE    CSV-HEAD-ITEM(IDX4)        TO  WRK-ITEM
             ELSE
               MOVE    CSV-HEAD-ITEM(IDX4)(1:18)  TO  WRK-ITEM(1:18)
               MOVE    WRK-TJIHI(IDX2)            TO  WRK-ITEM(19:)
               ADD     1                          TO  IDX2
             END-IF
      *
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *      行毎に帳票情報を印字
             PERFORM  VARYING  IDX-MONTH  FROM  1  BY  1
                      UNTIL    IDX-MONTH  >  SUM-MONTHMAX
      *
               INITIALIZE  WRK-ITEM
      *
               IF  SUM-MONTH(IDX-MONTH)  NOT =  SPACE
                 EVALUATE  IDX4
      *            列項目印字
                   WHEN   1
                     MOVE    SUM-MONTH(IDX-MONTH)         TO  WRK-ITEM
      *            計には印字しない
                   WHEN   4 THRU  6
                     IF  IDX-MONTH  =  SUM-MONTHMAX
                       CONTINUE
                     ELSE
                       MOVE    SUM-YOSO(IDX-MONTH IDX4)     TO  WRK-S9
                       MOVE    WRK-S9                       TO  WRK-ITEM
                     END-IF
      *            （ヤ）病理診断
                   WHEN  37
                     MOVE    SUM-YOSO(IDX-MONTH IDX4 + 13)TO  WRK-S9
                     MOVE    WRK-S9                       TO  WRK-ITEM
      *            病理診断以降、１つずれる
                   WHEN  38 THRU 50
                     MOVE    SUM-YOSO(IDX-MONTH IDX4 - 1) TO  WRK-S9
                     MOVE    WRK-S9                       TO  WRK-ITEM
                   WHEN  OTHER
                     MOVE    SUM-YOSO(IDX-MONTH IDX4)     TO  WRK-S9
                     MOVE    WRK-S9                       TO  WRK-ITEM
                 END-EVALUATE
      *
                 IF  IDX-MONTH  <  SUM-MONTHMAX
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             PERFORM  810-CSVDATA-EDIT-SEC
      *
             PERFORM  230-TOUKEICSV-SEC
      *
           END-PERFORM
      *
           .
      *
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       800-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
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
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                         ORCSTOUKEICSVAREA
           MOVE    "INS"                      TO  STOUKEICSV-MODE
           MOVE    D100LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
           MOVE    D100LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
           MOVE    D100LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
           MOVE    D100LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
           MOVE    D100LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
           MOVE    D100LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
           MOVE    ZERO                       TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"                TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM                DELIMITED  BY  SIZE
                   "kihondaichou_n_geppou_"   DELIMITED  BY  SIZE
                   D100LNK-YMD1               DELIMITED  BY  SIZE
                   "_"                        DELIMITED  BY  SIZE
                   D100LNK-YMD2               DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "基本台帳(入院月報)"       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
                                              ORCSTOUKEICSVAREA
                                              SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                                TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                         WRK-CSVDATA
           MOVE    1                          TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *
      *+
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    D100LNK-JOBID   TO  JOB-JOBID
               MOVE    D100LNK-SHELLID TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    D100LNK-JOBID   TO  JOB-JOBID
           MOVE    D100LNK-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                          STS-AREA-DAY
           INITIALIZE                          LNK-DAY2-AREA
           MOVE    "21"                TO      LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO      LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO      WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD1           TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO   WRK-SYMD1
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC             SECTION.
      *
           INITIALIZE                 SYUNOU-REC
      *
           MOVE    D100LNK-HOSPNUM      TO   SYU-HOSPNUM
           MOVE    "1"                  TO   SYU-NYUGAIKBN
           MOVE    "00000000"           TO   SYU-SRYYMD
           MOVE    "99999999"           TO   SYU-UPSRYYMD
      *
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "st_key1"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
           END-IF        
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SYUNOU
               MOVE    MCPDATA-REC  TO  SYUNOU-REC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
           END-IF  
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       911-DBSELECT-SEC             SECTION.
      *
           MOVE    ZERO              TO  FLG-SYUNOU
      *
           INITIALIZE                     SYUNOU-REC
           MOVE    D100LNK-HOSPNUM   TO   SYU-HOSPNUM
           MOVE    "1"               TO   SYU-NYUGAIKBN
           MOVE    D100LNK-YMD1      TO   SYU-SRYYMD
           MOVE    SYUNOU-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"        TO   MCP-FUNC
           MOVE    "tbl_syunou"      TO   MCP-TABLE
           MOVE    "key78"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 911-DBFETCH-SEC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
           END-IF
           .
       911-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       911-DBFETCH-SEC                SECTION.
      *
           MOVE    ZERO              TO  FLG-SYUNOU
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key78"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SYUNOU
               MOVE    MCPDATA-REC  TO  SYUNOU-REC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
           END-IF  
           .
       911-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       911-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key78"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           .
       911-DBCLOSE-EXT.
           EXIT.
      *
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
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
               ELSE
                   MOVE    1           TO  FLG-SYUNOU
                   INITIALIZE              SYUNOU-REC
               END-IF
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者労災マスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTRSIINF-REC
                   MOVE    ZERO        TO  FLG-PTRSIINF
               ELSE
                   MOVE    1           TO  FLG-PTRSIINF
                   INITIALIZE              PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                 TO  TNS-YUKOSTYMD
           MOVE    ALL "9"              TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_tensu"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           INITIALIZE                       JYURRK-REC
           MOVE    SYU-HOSPNUM          TO  JYURRK-HOSPNUM  
           MOVE    SYU-PTID             TO  JYURRK-PTID  
           MOVE    SYU-NYUGAIKBN        TO  JYURRK-NYUGAIKBN  
           MOVE    SYU-DENPNUM          TO  JYURRK-DENPNUM  
      *
           MOVE    JYURRK-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key6"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk"       TO  MCP-TABLE
               MOVE    "key6"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF  MCP-RC             =   ZERO
                   MOVE    MCPDATA-REC  TO  JYURRK-REC
                   MOVE    ZERO         TO  FLG-JYURRK
               ELSE
                   MOVE    1            TO  FLG-JYURRK
                   INITIALIZE               JYURRK-REC
               END-IF
           ELSE
               MOVE    1                TO  FLG-JYURRK
               INITIALIZE                   JYURRK-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key6"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       900-JYURRK-SELECT-SEC              SECTION.
      *
           INITIALIZE                       JYURRK-REC
           MOVE    SYU-HOSPNUM          TO  JYURRK-HOSPNUM  
           MOVE    SYU-PTID             TO  JYURRK-PTID  
           MOVE    SYU-NYUGAIKBN        TO  JYURRK-NYUGAIKBN  
           MOVE    "00000000"           TO  JYURRK-SRYYMD  
           MOVE    SYU-DENPPRTYMD       TO  JYURRK-UPSRYYMD  
      *
           MOVE    JYURRK-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key14"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =    ZERO
               PERFORM 900-JYURRK-FETCH-SEC
           ELSE
               MOVE    1           TO   FLG-JYURRK
           END-IF
           .
       900-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     受信履歴情報検索
      *****************************************************************
       900-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key14"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-JYURRK
               MOVE    MCPDATA-REC   TO  JYURRK-REC
           ELSE
               MOVE    1             TO  FLG-JYURRK
           END-IF
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-JYURRK-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key14"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索
      *****************************************************************
       900-NYURRK-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
                   PERFORM 900-NYURRK-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK
               MOVE    1           TO    FLG-END
           END-IF
           .
       900-NYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索
      *****************************************************************
       900-NYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK
               MOVE    1            TO  FLG-END
           END-IF
           .
       900-NYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-NYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-NYURRK-M-SELECT-SEC                 SECTION.
      *
           MOVE    PNR-M-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC           =   ZERO
               PERFORM 900-NYURRK-M-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK-M
               MOVE    1           TO    FLG-END
           END-IF
           .
       900-NYURRK-M-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-NYURRK-M-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK-M
               MOVE    MCPDATA-REC  TO  PNR-M-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK-M
               MOVE    1            TO  FLG-END
           END-IF
           .
       900-NYURRK-M-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
        900-M-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
            .
        900-M-DBCLOSE-EXT.
            EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       911-SRY-SELECT-SEC              SECTION.
      *
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =    ZERO
               PERFORM 911-SRY-FETCH-SEC
           ELSE
               MOVE    1           TO   FLG-SRYACT
           END-IF
           .
       911-SRY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為情報検索
      *****************************************************************
       911-SRY-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SRYACT
               MOVE    MCPDATA-REC   TO  SRYACT-REC
           ELSE
               MOVE    1             TO  FLG-SRYACT
           END-IF
           .
       911-SRY-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       911-SRY-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       911-SRY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                       HKNNUM-REC
           MOVE    SYU-HOSPNUM          TO  HKN-HOSPNUM  
           MOVE    SYU-SYUHKNNUM        TO  HKN-HKNNUM  
           MOVE    WRK-LASTYMD          TO  HKN-TEKSTYMD
           MOVE    ALL "9"              TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key5"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC          =  ZERO
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
                   MOVE    ZERO        TO  FLG-HKNNUM
               ELSE
                   MOVE    1           TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key3"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_sryacct"      TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SRYACCT-REC
                   MOVE    ZERO            TO  FLG-SRYACCT
               ELSE
                   MOVE    1               TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key3"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報  検索
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SRYACCT
           ELSE
               MOVE    1            TO  FLG-SRYACCT
               INITIALIZE               SRYACCT-REC
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報FETCH
      *****************************************************************
       900-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC   TO  SRYACCT-REC
               MOVE    ZERO                    TO  FLG-SRYACCT
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報CLOSE
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
       900-JYURRK-DRCD-READ-SEC         SECTION.
      *
           MOVE    JYURRK-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key19"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"      TO  MCP-FUNC
               MOVE    "tbl_jyurrk"             TO  MCP-TABLE
               MOVE    "key19"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
               IF  MCP-RC             =   ZERO
                   MOVE    MCPDATA-REC  TO  JYURRK-REC
                   MOVE    ZERO         TO  FLG-JYURRK
               ELSE
                   MOVE    1            TO  FLG-JYURRK
                   INITIALIZE               JYURRK-REC
               END-IF
           ELSE
               MOVE    1                TO  FLG-JYURRK
               INITIALIZE                   JYURRK-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key19"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-JYURRK-DRCD-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
