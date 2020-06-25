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
       PROGRAM-ID.             A00000D127.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 基本台帳(外来・医師別)
      *  管理者            : 
      *  作成日付    作業者       記述
      *  04/03/08    @@@@-半田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     初診、再診、その他の判定条件を変更
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/01/27  処理速度の改善
      *  01.00.03    @@@@-門間    11/08/04  0件印字指定を行った場合、医師の22人目以降が
      *                                     印字されない不具合を修正
      *  01.00.04    @@@@-門間    12/02/29  インデント整理
      *                           12/03/09  CSVファイル作成機能追加
      *  01.00.05    @@@@-門間    12/06/27  初診、再診、その他の判定条件を再変更
      *  01.00.06    @@@@-門間    14/07/17  医師情報処理時、診療日を参照するように変更
      *                                     0件印字が有効な場合、表示される医師の上限を変更
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    14/12/16  医師コード空白時の修正
      *  02.00.03    @@@@-門間    15/04/02  保険点数が「０」の場合、平均点の計算から除く
      *  02.00.04    @@@@-門間    15/04/16  助成金額の一時領域を修正
      *  02.00.05    @@@@-井上    20/03/06  「シ　助成金」欄の集計処理を修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    基本台帳(外来日報)用対象患者ファイル
           SELECT  D127PT-FILE     ASSIGN  AD127PT
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     D127P-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
      *    基本台帳(外来日報)用対象患者ファイル
       FD  D127PT-FILE.
       01  D127P. 
           03    D127P-KEY.
             05  D127P-SRYYMD      PIC X(08).
             05  D127P-PTID        PIC 9(10).
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
                                   BY         //AD127PT//.
      *
      *    基本台帳(外来・医師別)DIA用コピー句
           COPY    "A00000D127.INC".
           COPY    "A00000D1271.INC".
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
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INVALID         PIC 9(01).
           03  FLG-820SYS          PIC X(04).
           03  INP-STS             PIC 9(04).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-JIHI            PIC 9(01).
           03  FLG-ACCTEND         PIC 9(01).
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
           03  I3                PIC 9(04).
           03  I4                PIC 9(04).
           03  I5                PIC 9(04).
           03  I6                PIC 9(04).
           03  I7                PIC 9(04).
           03  I8                PIC 9(04).
           03  I9                PIC 9(04).
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
           03  IDX10             PIC 9(02).
           03  IDX11             PIC 9(02).
      *
           03  IDX-DRCD           PIC 9(03).
           03  IDX-ST            PIC 9(03).
           03  IDX-ED            PIC 9(03).
           03  IDXP              PIC 9(05).
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
           03  WRK-GAITENTTLKBN      PIC 9(03).
           03  WRK-BF-MISYU          PIC S9(11).
           03  WRK-BF-AZUKARI        PIC S9(11).
           03  WRK-TEN               PIC S9(11).
           03  WRK-MONEY             PIC S9(11).
           03  WRK-JIHI              PIC S9(11).
           03  WRK-SYUEKI            PIC S9(11).
           03  WRK-RATE              PIC 9(01)V9(03).
           03  WRK-RATE2             PIC 9(03).
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
           03  WRK-Z9               PIC ZZZZZZZZZZ9.
           03  WRK-ZZ               PIC ZZZZZZZZZZZ.
           03  WRK-S9               PIC ----------9.
           03  WRK-SZ               PIC ----------Z.
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
           03  WRK-STARTYMD      PIC 9(08).
           03  WRK-LASTYMD       PIC 9(08).
           03  WRK-ACCT-SDAY     PIC 9(02).
           03  WRK-ACCT-EDAY     PIC 9(02).
           03  WRK-ACCT-IN       PIC 9(04).
           03  WRK-JIHIMONEY     PIC 9(10).
           03  WRK-AZUKARI       PIC 9(10).
           03  WRK-SYOSINYMD         PIC 9(08).
           03  WRK-HKNNAI            PIC 9(11).
           03  WRK-JOSEI             PIC S9(11).
      *
           03  WRK-DRCD-TBL     OCCURS  1000.
             05  WRK-DRFLG2      PIC 9(01).
             05  WRK-DRCD2       PIC X(04).
             05  WRK-DRNAME2     PIC X(18).
      *
           03  WRK-HN-TBL               OCCURS  1000.
             05  WRK-HNINZU               PIC 9(05).
      *
       01  SUM-AREA.
           03         SUM-DRMAX         PIC  9(03).
           03         SUM-SRYKA-G.    
             05       SUM-SRYKACD       PIC  X(02).
             05       SUM-SRYKANAME     PIC  X(20).
           03         SUM-PRFLG         PIC  9(01).
           03         SUM-DR-TBL        OCCURS  1000.
             05       SUM-DRCD          PIC  X(04).
             05       SUM-DRNAME        PIC  X(18).
             05       SUM-BF-MISYU      PIC S9(11).
             05       SUM-YOSO-TBL      OCCURS   48.
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
      *
           COPY    "CPSHELLTBL.INC".
      *
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
                            VALUE  "（ア）患者数＿初診".
               07  CSV-HEAD-D003  PIC X(50)
                            VALUE  "（イ）患者数＿再診".
               07  CSV-HEAD-D004  PIC X(50)
                            VALUE  "（ウ）患者数＿その他".
               07  CSV-HEAD-D005  PIC X(50)
                            VALUE  "（エ）患者数＿合計".
               07  CSV-HEAD-D006  PIC X(50)
                            VALUE  "（オ）患者数＿新患".
               07  CSV-HEAD-D007  PIC X(50)
                            VALUE  "（カ）患者数＿新患来院率".
               07  CSV-HEAD-D008  PIC X(50)
                            VALUE  "（キ）保険内点数".
               07  CSV-HEAD-D009  PIC X(50)
                            VALUE  "（ク）平均点".
               07  CSV-HEAD-D010  PIC X(50)
                            VALUE  "（ケ）保険内負担金".
               07  CSV-HEAD-D011  PIC X(50)
                            VALUE  "（コ）保険外金額".
               07  CSV-HEAD-D012  PIC X(50)
                            VALUE  "（サ）減免金額".
               07  CSV-HEAD-D013  PIC X(50)
                            VALUE  "（シ）助成金額".
               07  CSV-HEAD-D014  PIC X(50)
                            VALUE  "（ス）消費税".
               07  CSV-HEAD-D015  PIC X(50)
                            VALUE  "（セ）調整金".
               07  CSV-HEAD-D016  PIC X(50)
                            VALUE  "（ソ）窓口請求金額".
               07  CSV-HEAD-D017  PIC X(50)
                            VALUE  "（タ）期間外未収金残額".
               07  CSV-HEAD-D018  PIC X(50)
                            VALUE  "（チ）入金額".
               07  CSV-HEAD-D019  PIC X(50)
                            VALUE  "（ツ）当日未収金".
               07  CSV-HEAD-D020  PIC X(50)
                            VALUE  "（テ）当日預り金残額".
               07  CSV-HEAD-D021  PIC X(50)
                            VALUE  "（ト）".
               07  CSV-HEAD-D022  PIC X(50)
                            VALUE  "（ナ）診療区分＿初・再診料".
               07  CSV-HEAD-D023  PIC X(50)
                            VALUE  "（ニ）診療区分＿医学管理等".
               07  CSV-HEAD-D024  PIC X(50)
                            VALUE  "（ヌ）診療区分＿在宅医療".
               07  CSV-HEAD-D025  PIC X(50)
                            VALUE  "（ネ）診療区分＿投薬".
               07  CSV-HEAD-D026  PIC X(50)
                            VALUE  "（ノ）診療区分＿注射".
               07  CSV-HEAD-D027  PIC X(50)
                            VALUE  "（ハ）診療区分＿処置".
               07  CSV-HEAD-D028  PIC X(50)
                            VALUE  "（ヒ）診療区分＿手術".
               07  CSV-HEAD-D029  PIC X(50)
                            VALUE  "（フ）診療区分＿麻酔".
               07  CSV-HEAD-D030  PIC X(50)
                            VALUE  "（ヘ）診療区分＿検査".
               07  CSV-HEAD-D031  PIC X(50)
                            VALUE  "（ホ）診療区分＿画像診断".
               07  CSV-HEAD-D032  PIC X(50)
                            VALUE  "（マ）診療区分＿リハビリテーション".
               07  CSV-HEAD-D033  PIC X(50)
                            VALUE  "（ミ）診療区分＿精神科専門療法".
               07  CSV-HEAD-D034  PIC X(50)
                            VALUE  "（ム）診療区分＿放射線治療".
               07  CSV-HEAD-D035  PIC X(50)
                            VALUE  "（メ）診療区分＿病理診断".
               07  CSV-HEAD-D036  PIC X(50)
                            VALUE  "（モ）診療区分＿入院料等".
               07  CSV-HEAD-D037  PIC X(50)
                            VALUE  "（ヤ）診療区分＿療養担当手当".
               07  CSV-HEAD-D038  PIC X(50)
                            VALUE  "（あ）保険適用外金額".
               07  CSV-HEAD-D039-G.
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
               07  CSV-HEAD-JIHI-R      REDEFINES  CSV-HEAD-D039-G.
                 09  CSV-HEAD-JIHI-OCC    OCCURS  10.
                   11  CSV-HEAD-JIHI-ID     PIC X(06).
                   11  CSV-HEAD-JIHI-NAME   PIC X(44).
      *
             05  CSV-HEAD-R   REDEFINES   CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM        PIC X(50)  OCCURS  48.
      *
             05  CSV-HEAD-MAX         PIC 9(05)  VALUE  48.
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
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
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
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    初再診取得サブ
           COPY    "CPORCSS003.INC".
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
      ***    正常主処理
             PERFORM 200-MAIN-SEC
           ELSE
      ***    エラー処理
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
           MOVE    "A00000D127"    TO  JOB-PGID
      *
           MOVE    "基本台帳(外来・医師別)"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AD127TP"                TO  AD127PT-FILE-ID
           MOVE    D100LNK-PRTKANRI-TERMID  TO  AD127PT-TERMID
           MOVE    D100LNK-HOSPNUM          TO  AD127PT-HOSPNUM
      *
           MOVE    AD127PT-BASENAME         TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  AD127PT-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    初診／再診判定サブ初期処理
           INITIALIZE                      SS003-AREA
           MOVE    ZERO            TO      SS003-SYORIKBN
           CALL    "ORCSS003"      USING   SS003-AREA
                                           SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
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
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE  D100LNK-YMD1          TO  SYS-1001-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1001-STYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPNUM     TO  D100LNK-HOSPNUM
               MOVE    SYS-1001-BEDSU      TO  WRK-SYS-BEDSU
               MOVE    SYS-1001-BEDSUIPN   TO  WRK-SYS-BEDSUIPN
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
      *
      *    画面入力パラメタ値のチェック
      *    集計対象か期間のチェック
           IF   D100LNK-YMD1    >   D100LNK-YMD2
                MOVE    10      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF   D100LNK-YMD1(1:4)   NOT =   D100LNK-YMD2(1:4)
            IF  D100LNK-YMD1(5:4)   <=      D100LNK-YMD2(5:4)  
                MOVE    11      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
            END-IF
           END-IF
      *
      *    入外区分のチェック
           IF   D100LNK-NYUGAIKBN   NOT =   "1"
            AND D100LNK-NYUGAIKBN   NOT =   "2"
                MOVE    20      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF   D100LNK-YMD1(1:6)   NOT =   D100LNK-YMD2(1:6)
                MOVE    21      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    診療科コードのチェック
           IF  D100LNK-SRYKACD     NOT  =  SPACE
               INITIALIZE                      SYS-1005-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    D100LNK-SRYKACD     TO  SYS-1005-KBNCD
               MOVE    D100LNK-YMD2        TO  WRK-SYMD
               MOVE  D100LNK-YMD1          TO  SYS-1005-EDYUKYMD
               MOVE  D100LNK-YMD2          TO  SYS-1005-STYUKYMD
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
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    D100LNK-DRCD        TO  SYS-1010-KBNCD
               MOVE    D100LNK-YMD2        TO  WRK-SYSYMD
               MOVE  D100LNK-YMD1          TO  SYS-1010-EDYUKYMD
               MOVE  D100LNK-YMD2          TO  SYS-1010-STYUKYMD
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               PERFORM 810-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI2       =   ZERO
                 AND   D100LNK-DRCD(1:1)   =   "1"
                 AND   SYS-1010-KBNCD(1:1) =   "1"
                       MOVE  MCPDATA-REC   TO  SYS-1010-REC
                       MOVE  SYS-1010-KBNCD(2:4)
                                           TO  WRK-SEL-DRCD
                       MOVE  SYS-1010-NAME TO  WRK-SEL-DRNAME
               ELSE
                       MOVE 40             TO  INP-STS
                       GO                  TO  110-PARA-HENSYU-EXT
               END-IF                                     
           END-IF                                     
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "21"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "22"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "23"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "24"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "25"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
      *    区分26
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "26"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
      *    区分27
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "27"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
      *    区分28
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "28"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
      *    区分29
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "29"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
      *    区分20
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "20"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
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
           MOVE    SPA-HOSPNUM          TO  SYS-4001-HOSPNUM
           MOVE    "4001"               TO  SYS-4001-KANRICD
           MOVE    "*"                  TO  SYS-4001-KBNCD
           MOVE    D100LNK-YMD2         TO  WRK-SYMD
           MOVE  D100LNK-YMD1           TO  SYS-4001-EDYUKYMD
           MOVE  D100LNK-YMD2           TO  SYS-4001-STYUKYMD
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
           MOVE   SPA-HOSPNUM       TO     TNS-HOSPNUM
           PERFORM   910-TENSU-READ-SEC
           MOVE   TNS-GAITENTTLKBN  TO     WRK-GAITENTTLKBN
      *
      *    基本台帳(外来日報)用対象患者ファイル初期化
           OPEN  OUTPUT D127PT-FILE
           CLOSE        D127PT-FILE  
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
             MOVE  "1010"             TO  FLG-820SYS
             INITIALIZE                   SYS-1010-REC
             MOVE  D100LNK-HOSPNUM    TO  SYS-1010-HOSPNUM
             MOVE  "1010"             TO  SYS-1010-KANRICD
             MOVE  D100LNK-YMD2       TO  WRK-SYSYMD
             MOVE  D100LNK-YMD1       TO  SYS-1010-EDYUKYMD
             MOVE  D100LNK-YMD2       TO  SYS-1010-STYUKYMD
             PERFORM 820-SYSKANRI-DBSELECT-SEC
             MOVE     1               TO  I3 
      *
             PERFORM  UNTIL  FLG-SYSKANRI3 NOT = ZERO
               IF  SYS-1010-KBNCD(1:1)      = "1"
                 IF  D100LNK-ZERO  =  1
                   MOVE SYS-1010-KBNCD(2:4)  TO   WRK-DRCD2(I3)
                   MOVE SYS-1010-NAME        TO   WRK-DRNAME2(I3)
                 ELSE
                   MOVE SYS-1010-KBNCD(2:4)  TO   SUM-DRCD(I3)
                   MOVE SYS-1010-NAME        TO   SUM-DRNAME(I3)
                 END-IF
                 ADD  1            TO   I3
               END-IF
               PERFORM  820-SYSKANRI-DBFETCH-SEC
             END-PERFORM
             PERFORM  820-SYSKANRI-DBCLOSE-SEC
      *
             IF  D100LNK-ZERO  =  1
               MOVE    SPACE            TO   WRK-DRCD2(I3)
               MOVE    "該当医師なし"   TO   WRK-DRNAME2(I3)
             ELSE
               MOVE    SPACE            TO   SUM-DRCD(I3)
               MOVE    "該当医師なし"   TO   SUM-DRNAME(I3)
             END-IF
             ADD     1                TO   I3
      *
             IF  D100LNK-ZERO  =  1
               PERFORM  910-DBSELECT-SEC
               PERFORM  UNTIL      FLG-SYUNOU  = 1
      *
                 IF     D100LNK-YMD1  <=  SYU-SRYYMD
                   AND  D100LNK-YMD2  >=  SYU-SRYYMD
                   IF       D100LNK-SRYKACD      =  SPACE
                       AND  D100LNK-DRCD         =  SPACE
                     PERFORM  110-ZERO-SEC
                   END-IF
      *
                   IF       D100LNK-SRYKACD  NOT =  SPACE
                       AND  D100LNK-DRCD     NOT =  SPACE
                     IF     D100LNK-SRYKACD      =  SYU-SRYKA
                       AND  D100LNK-DRCD(2:4)    =  SYU-DRCD
                       PERFORM  110-ZERO-SEC
                     END-IF
                   END-IF
      *
                   IF       D100LNK-SRYKACD  NOT =  SPACE
                       AND  D100LNK-DRCD         =  SPACE
                     IF     D100LNK-SRYKACD      =  SYU-SRYKA
                       PERFORM  110-ZERO-SEC
                     END-IF
                   END-IF
      *
                   IF       D100LNK-DRCD     NOT =  SPACE
                       AND  D100LNK-SRYKACD      =  SPACE
                     IF     D100LNK-DRCD(2:4)    =  SYU-DRCD
                       PERFORM  110-ZERO-SEC
                     END-IF
                   END-IF
                 END-IF
                 PERFORM 910-DBFETCH-SEC
               END-PERFORM
               PERFORM 910-DBCLOSE-SEC
      *
               MOVE  1  TO  I3
               PERFORM  VARYING  I8  FROM  1  BY  1
                        UNTIL    I8  >     998
                 IF  WRK-DRFLG2(I8)  =  1
                   MOVE   WRK-DRCD2(I8)     TO   SUM-DRCD(I3)
                   MOVE   WRK-DRNAME2(I8)   TO   SUM-DRNAME(I3)
                   ADD    1                 TO   I3
                 END-IF
               END-PERFORM
             END-IF
           ELSE
             MOVE WRK-SEL-DRCD        TO   SUM-DRCD(1)
             MOVE WRK-SEL-DRNAME      TO   SUM-DRNAME(1)
             MOVE 2                   TO   I3
           END-IF
      *
           MOVE    ALL "9"          TO   SUM-DRCD(I3)
           MOVE    "計"             TO   SUM-DRNAME(I3)
           MOVE    I3               TO   SUM-DRMAX
      *
           .
       110-DR-PARA-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ０件印字処理
      *****************************************************************
       110-ZERO-SEC                SECTION.
      *
           PERFORM  VARYING  I9  FROM  1  BY  1
                    UNTIL    I9  >     998
             IF  SYU-DRCD  =  WRK-DRCD2(I9)
               MOVE  1  TO  WRK-DRFLG2(I9)
               MOVE  999  TO  I9
             END-IF
           END-PERFORM
           .
       110-ZERO-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    基本台帳(外来日報)用対象患者ファイルOPEN
           OPEN  I-O D127PT-FILE
      *
      *    収納情報集計処理
           PERFORM 320-SYUNOU-CNT-SEC
      *
      *    基本台帳(外来日報)用対象患者ファイルCLOSE
           CLOSE     D127PT-FILE
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
      *    画面入力値エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE                   D127
           INITIALIZE                   D1271
      *
           MOVE   "(外来・医師別)"      TO    D127-SUBTITLE
           MOVE   WRK-SYSYMDWH          TO    D127-SYSYMD
           MOVE   "(外来・医師別)"      TO    D1271-SUBTITLE
           MOVE   WRK-SYSYMDWH          TO    D1271-SYSYMD
      *
           EVALUATE INP-STS 
           WHEN 10
             MOVE
           "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D127-JOBMSG
             MOVE
           "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D1271-JOBMSG
      *
           WHEN 11
             MOVE
           "入力する集計対象期間の範囲は１２ヶ月内で指定をして下さい"
                                  TO    D127-JOBMSG
             MOVE
           "入力する集計対象期間の範囲は１２ヶ月内で指定をして下さい"
                                  TO    D1271-JOBMSG
      *
           WHEN 20
             MOVE
           "入力された入外区分の値に誤りがあります"
                                  TO    D127-JOBMSG
             MOVE
           "入力された入外区分の値に誤りがあります"
                                  TO    D1271-JOBMSG
      *
           WHEN 21
             MOVE
             "入力する集計対象期間の範囲は同年月内で指定をして下さい"
                                  TO    D127-JOBMSG
             MOVE
             "入力する集計対象期間の範囲は同年月内で指定をして下さい"
                                  TO    D1271-JOBMSG
      *
           WHEN 30
             MOVE
           "入力された診療科コードの値に誤りがあります"
                                  TO    D127-JOBMSG
             MOVE
           "入力された診療科コードの値に誤りがあります"
                                  TO    D1271-JOBMSG
      *
           WHEN 40
             MOVE
           "入力された医師コードの値に誤りがあります"
                                  TO    D127-JOBMSG
             MOVE
           "入力された医師コードの値に誤りがあります"
                                  TO    D1271-JOBMSG
           END-EVALUATE
      *
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
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
      *    集計対象年月のデータであるかをチェックする。 
           IF      SYU-SRYYMD             >=  D100LNK-YMD1
             AND   SYU-SRYYMD             <=  D100LNK-YMD2
             AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
             AND   SYU-CREATEKBN       NOT =  "3"
             AND   SYU-DENPJTIKBN      NOT =  "3"
             AND   SYU-NYUGAIKBN           =  "2"
             AND ((WRK-SEL-SRYKACD         =  SPACE)
              OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                   SYU-SRYKA               =  WRK-SEL-SRYKACD))
             AND ((WRK-SEL-DRCD            =  SPACE)
              OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                   SYU-DRCD                =  WRK-SEL-DRCD))
      *
              MOVE  SYU-DRCD     TO   WRK-DRCD
              PERFORM  320-DR-GROUPING-SEC
              IF I3  <     SUM-DRMAX
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
            MOVE SYU-PTID    TO PTINF-PTID
            PERFORM 900-PTINF-READ-SEC
      *
               IF     SYU-SRYYMD              <  D100LNK-YMD1
                AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
                AND   SYU-CREATEKBN       NOT =  "3"
                AND   SYU-DENPJTIKBN      NOT =  "3"
                AND   SYU-NYUGAIKBN           =  "2"
                AND ((WRK-SEL-SRYKACD         =  SPACE)
                 OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                      SYU-SRYKA               =  WRK-SEL-SRYKACD))
                AND ((WRK-SEL-DRCD            =  SPACE)
                 OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                      SYU-DRCD                =  WRK-SEL-DRCD))
                AND   FLG-PTINF               =  ZERO
                AND   PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***         範囲指定期間前未収金の集計
                  MOVE     SYU-DRCD     TO   WRK-DRCD
                  PERFORM  320-DR-GROUPING-SEC
      *
                  IF I3    <   SUM-DRMAX
                    COMPUTE  WRK-MONEY    =
                             SYU-SKYMONEY -  SYU-NYUKIN-TOTAL
                    ADD   WRK-MONEY  TO  SUM-BF-MISYU(I3)
                  END-IF
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
                  AND   SYU-NYUGAIKBN           =  "2"
                  AND ((WRK-SEL-SRYKACD         =  SPACE)
                   OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                      SYU-SRYKA               =  WRK-SEL-SRYKACD))
                  AND ((WRK-SEL-DRCD            =  SPACE)
                   OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                      SYU-DRCD                =  WRK-SEL-DRCD))
                  AND   FLG-PTINF               =  ZERO
                  AND   PTINF-TSTPTNUMKBN   NOT =  "1"
                   MOVE  SYU-DRCD     TO   WRK-DRCD
                   PERFORM  320-DR-GROUPING-SEC
                   IF I3  <     SUM-DRMAX
      *
                     IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
                       AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
                       ADD  SMEI-NYUHEN-MONEY
                                        TO  SUM-YOSO(I3 18)
                       ADD  SMEI-NYUHEN-MONEY
                                        TO  SUM-YOSO(SUM-DRMAX 18)
                     END-IF
      *
                   END-IF
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
                    UNTIL    I3     >=   SUM-DRMAX
      *
      *    新患来院率
                    INITIALIZE                WRK-RATE
                                              WRK-RATE2
                    COMPUTE  WRK-RATE =
                             SUM-YOSO(I3 6) / SUM-YOSO(I3 5)
                    COMPUTE  WRK-RATE2 ROUNDED     =
                             WRK-RATE  *  100
                    MOVE     WRK-RATE2    TO  WRK-RATE-Z9
                    MOVE     WRK-RATE-Z9  TO  SUM-YOSO(I3 7)
      *
      *    平均点
                    INITIALIZE                WRK-TEN
                    COMPUTE  WRK-TEN  ROUNDED      =
                             SUM-YOSO (I3 8) / 
                             WRK-HNINZU(I3)
                    MOVE     WRK-TEN  TO  SUM-YOSO (I3 9)
      *
      *    過日未収金残額
                    MOVE      SUM-BF-MISYU(I3)
                          TO  SUM-YOSO (I3 17)
                    ADD       SUM-BF-MISYU(I3) 
                          TO  SUM-YOSO (SUM-DRMAX 17)  
      *
      *    入金額
      *    入金額から当日預り金を引き去る              
                   COMPUTE SUM-YOSO(I3 18) =
                   SUM-YOSO(I3 18) -
                   SUM-YOSO(I3 20)
                   COMPUTE SUM-YOSO(SUM-DRMAX 18) =
                   SUM-YOSO(SUM-DRMAX 18) -
                   SUM-YOSO(I3 20)
      *
      *    自費金額から当日預り金を引き去る
                   EVALUATE WRK-GAITENTTLKBN
      *
                      WHEN  1                
                            COMPUTE SUM-YOSO(I3 38) =
                            SUM-YOSO(I3 38) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 38) =
                            SUM-YOSO(SUM-DRMAX 38) -
                            SUM-YOSO(I3 20)
                      WHEN  2                
                            COMPUTE SUM-YOSO(I3 39) =
                            SUM-YOSO(I3 39) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 39) =
                            SUM-YOSO(SUM-DRMAX 39) -
                            SUM-YOSO(I3 20)
                      WHEN  3                
                            COMPUTE SUM-YOSO(I3 40) =
                            SUM-YOSO(I3 40) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 40) =
                            SUM-YOSO(SUM-DRMAX 40) -
                            SUM-YOSO(I3 20)
                      WHEN  4                
                            COMPUTE SUM-YOSO(I3 41) =
                            SUM-YOSO(I3 41) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 41) =
                            SUM-YOSO(SUM-DRMAX 41) -
                            SUM-YOSO(I3 20)
                      WHEN  5                
                            COMPUTE SUM-YOSO(I3 42) =
                            SUM-YOSO(I3 42) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 42) =
                            SUM-YOSO(SUM-DRMAX 42) -
                            SUM-YOSO(I3 20)
                      WHEN  6                
                            COMPUTE SUM-YOSO(I3 43) =
                            SUM-YOSO(I3 43) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 43) =
                            SUM-YOSO(SUM-DRMAX 43) -
                            SUM-YOSO(I3 20)
                      WHEN  7                
                            COMPUTE SUM-YOSO(I3 44) =
                            SUM-YOSO(I3 44) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 44) =
                            SUM-YOSO(SUM-DRMAX 44) -
                            SUM-YOSO(I3 20)
                      WHEN  8                
                            COMPUTE SUM-YOSO(I3 45) =
                            SUM-YOSO(I3 45) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 45) =
                            SUM-YOSO(SUM-DRMAX 45) -
                            SUM-YOSO(I3 20)
                      WHEN  9                
                            COMPUTE SUM-YOSO(I3 46) =
                            SUM-YOSO(I3 46) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 46) =
                            SUM-YOSO(SUM-DRMAX 46) -
                            SUM-YOSO(I3 20)
                      WHEN  10                
                            COMPUTE SUM-YOSO(I3 47) =
                            SUM-YOSO(I3 47) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-DRMAX 47) =
                            SUM-YOSO(SUM-DRMAX 47) -
                            SUM-YOSO(I3 20)
                   END-EVALUATE
      *
           END-PERFORM
      *
      *    新患来院率(計)
           INITIALIZE                WRK-RATE
                                     WRK-RATE2
           COMPUTE  WRK-RATE =
                    SUM-YOSO(SUM-DRMAX 6) / 
                    SUM-YOSO(SUM-DRMAX 5)
           COMPUTE  WRK-RATE2 ROUNDED     =
                    WRK-RATE     *   100
           MOVE     WRK-RATE2    TO  WRK-RATE-Z9
           MOVE     WRK-RATE-Z9  TO  SUM-YOSO(SUM-DRMAX 7)
      *
      *    平均点(計)
      *
           INITIALIZE        WRK-TEN
           COMPUTE  WRK-TEN  ROUNDED      =
                    SUM-YOSO (SUM-DRMAX 8) / 
                    WRK-HNINZU(SUM-DRMAX)
           MOVE     WRK-TEN
                    TO  SUM-YOSO (SUM-DRMAX 9)
      *
      *    過日未収金残額(計)
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
           MOVE SYU-SRYYMD   TO  D127P-SRYYMD
           MOVE SYU-PTID     TO  D127P-PTID
           READ D127PT-FILE
                KEY      IS      D127P-KEY  
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      *
           IF   FLG-INVALID      =     1 
      *    診療日別対象患者ファイルに未処理のデータは患者数を集計処理を行う。
                INITIALIZE       FLG-INVALID     
                WRITE D127P
                       INVALID  KEY  
                       MOVE  1  TO FLG-INVALID
                END-WRITE
      *    新患件数
                PERFORM  900-JYURRK-SELECT-SEC
                IF       FLG-JYURRK       =   ZERO
                  AND    JYURRK-SRYYMD    =   SYU-SRYYMD 
      *    新患件数としてカウントする。
                  INITIALIZE                 WRK-SYOSINYMD
                                             FLG-SRYKARRK
                  MOVE    SYU-SRYYMD         TO   WRK-SYOSINYMD
                  PERFORM  900-SRYKARRK-SELECT-SEC
                  PERFORM  UNTIL  FLG-SRYKARRK  =  1
                    IF   KARRK-SYOSINYMD1  <  WRK-SYOSINYMD
                        AND  KARRK-SYOSINYMD1  NOT =  SPACE
                      MOVE    KARRK-SYOSINYMD1    TO   WRK-SYOSINYMD
                    END-IF
                    PERFORM  900-SRYKARRK-FETCH-SEC
                  END-PERFORM
                  PERFORM  900-SRYKARRK-CLOSE-SEC
                  IF   WRK-SYOSINYMD  =  SYU-SRYYMD  
                    ADD    1        TO   SUM-YOSO(I3 6)
                    ADD    1        TO   SUM-YOSO(SUM-DRMAX 6)
                  END-IF
                END-IF
                PERFORM  900-JYURRK-CLOSE-SEC
           END-IF
      *
      *    受診履歴情報により初診、再診、その他件数を取得する。
           PERFORM 320-JYURRK-CNT-SUB-SEC
      *
      *    当日預り金
      *    患者別に当日預り金を診療行為情報より取得する。
           PERFORM   910-JYURRK-READ-SEC
           MOVE    JYURRK-RENNUM     TO   IDX4
           PERFORM  UNTIL  FLG-JYURRK  =  1
             PERFORM  320-AZUKARI-GET-SEC
             PERFORM   910-JYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   910-JYURRK-CLOSE-SEC
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
      *         該当レコードが存在しない場合は単価を10円に設定する。
                MOVE 10     TO  HKN-TENTNK
           END-IF
           IF   SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
            OR  SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM
            OR  SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM
      *         患者労災情報検索
                INITIALIZE           PTRSIINF-REC
                MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
                MOVE SYU-PTID    TO  PTRSI-PTID
                MOVE COMB-HKNID  TO  PTRSI-HKNID
                PERFORM 900-PTRSIINF-READ-SEC
           END-IF
      *
      *    合計保険点数初期化 
                   MOVE   ZERO     TO  SYU-TOTAL-HKNTEN
      *    診療科保険点数            
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(I3 22)
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 22)
      **   合計点数に加算
                   ADD    SYU-SSU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    指導料保険点数            
                   ADD    SYU-SDO-HKNTEN
                          TO  SUM-YOSO (I3 23)
                   ADD    SYU-SDO-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 23)
      **   合計点数に加算
                   ADD    SYU-SDO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    在宅料保険点数            
                   ADD    SYU-ZTK-HKNTEN
                          TO  SUM-YOSO (I3 24)
                   ADD    SYU-ZTK-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 24)
      **   合計点数に加算
                   ADD    SYU-ZTK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    投薬料保険点数            
                   ADD    SYU-TYK-HKNTEN
                          TO  SUM-YOSO (I3 25)
                   ADD    SYU-TYK-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 25)
      **   合計点数に加算
                   ADD    SYU-TYK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    注射料保険点数            
                   ADD    SYU-CSY-HKNTEN
                          TO  SUM-YOSO (I3 26)
                   ADD    SYU-CSY-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 26)
      **   合計点数に加算FLG-SRYACT
                   ADD    SYU-CSY-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    処置料保険点数            
                   ADD    SYU-SYC-HKNTEN
                          TO  SUM-YOSO (I3 27)
                   ADD    SYU-SYC-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 27)
      **   合計点数に加算
                   ADD    SYU-SYC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    手術料保険点数            
                   ADD    SYU-SJT-HKNTEN
                          TO  SUM-YOSO (I3 28)
                   ADD    SYU-SJT-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 28)
      **   合計点数に加算
                   ADD    SYU-SJT-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    麻酔点数            
                   ADD    SYU-MSI-HKNTEN
                          TO  SUM-YOSO (I3 29)
                   ADD    SYU-MSI-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 29)
      **   合計点数に加算
                   ADD    SYU-MSI-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    検査料保険点数            
                   ADD    SYU-KNS-HKNTEN
                          TO  SUM-YOSO (I3 30)
                   ADD    SYU-KNS-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 30)
      **   合計点数に加算
                   ADD    SYU-KNS-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    X線料保険点数            
                   ADD    SYU-GZU-HKNTEN
                          TO  SUM-YOSO (I3 31)
                   ADD    SYU-GZU-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 31)
      **   合計点数に加算
                   ADD    SYU-GZU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    リハ点数            
                   ADD    SYU-ETC-HKNTEN
                          TO  SUM-YOSO (I3 32)
                   ADD    SYU-ETC-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 32)
      **   合計点数に加算
                   ADD    SYU-ETC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    精神科専門点数            
                   ADD    SYU-SSN-HKNTEN
                          TO  SUM-YOSO (I3 33)
                   ADD    SYU-SSN-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 33)
      **   合計点数に加算
                   ADD    SYU-SSN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    放射線治療点数            
                   ADD    SYU-HOU-HKNTEN
                          TO  SUM-YOSO (I3 34)
                   ADD    SYU-HOU-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 34)
      **   合計点数に加算
                   ADD    SYU-HOU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *    病理診断点数            
                   ADD    SYU-BYR-HKNTEN
                          TO  SUM-YOSO (I3 48)
                   ADD    SYU-BYR-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 48)
      **   合計点数に加算
                   ADD    SYU-BYR-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    入院料保険点数            
                   ADD    SYU-NYN-HKNTEN
                          TO  SUM-YOSO (I3 35)
                   ADD    SYU-NYN-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 35)
      **   合計点数に加算
                   ADD    SYU-NYN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    療養担当手当点数            
                   ADD    SYU-RYO-HKNTEN
                          TO  SUM-YOSO (I3 36)
                   ADD    SYU-RYO-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 36)
      **   合計点数に加算
                   ADD    SYU-RYO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *    保険適用外金額            
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (I3 37)
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (SUM-DRMAX 37)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (I3 37)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (SUM-DRMAX 37)
      *
      *    自費１            
                   ADD    SYU-JIHI-1       
                          TO  SUM-YOSO (I3 38)
                   ADD    SYU-JIHI-1
                          TO  SUM-YOSO (SUM-DRMAX 38)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (I3 38)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (SUM-DRMAX 38)
      *
      *    自費２            
                   ADD    SYU-JIHI-2       
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-JIHI-2
                          TO  SUM-YOSO (SUM-DRMAX 39)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (SUM-DRMAX 39)
      *
      *    自費３            
                   ADD    SYU-JIHI-3       
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-3
                          TO  SUM-YOSO (SUM-DRMAX 40)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (SUM-DRMAX 40)
      *
      *    自費４            
                   ADD    SYU-JIHI-4       
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-4
                          TO  SUM-YOSO (SUM-DRMAX 41)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (SUM-DRMAX 41)
      *
      *    自費５            
                   ADD    SYU-JIHI-5       
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-5
                          TO  SUM-YOSO (SUM-DRMAX 42)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (SUM-DRMAX 42)
      *
      *    自費６            
                   ADD    SYU-JIHI-6       
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-6
                          TO  SUM-YOSO (SUM-DRMAX 43)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (SUM-DRMAX 43)
      *
      *    自費７            
                   ADD    SYU-JIHI-7       
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-7
                          TO  SUM-YOSO (SUM-DRMAX 44)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (SUM-DRMAX 44)
      *
      *    自費８            
                   ADD    SYU-JIHI-8       
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-8
                          TO  SUM-YOSO (SUM-DRMAX 45)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (SUM-DRMAX 45)
      *
      *    自費９            
                   ADD    SYU-JIHI-9       
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-9
                          TO  SUM-YOSO (SUM-DRMAX 46)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (SUM-DRMAX 46)
      *
      *    自費１０            
                   ADD    SYU-JIHI-10       
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-10
                          TO  SUM-YOSO (SUM-DRMAX 47)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (SUM-DRMAX 47)
      *
      *    保険内点数(合計保険点数)
                   ADD    SYU-TOTAL-HKNTEN
                          TO  SUM-YOSO (I3 8)
                   ADD    SYU-TOTAL-HKNTEN   
                          TO  SUM-YOSO(SUM-DRMAX 8)
      *
                   INITIALIZE     WRK-HKNNAI
      * 保険内負担金              
      *
                   IF   ( (SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
                       AND  SYU-PTFTNRATE  NOT =  ZERO )
                       OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
                       OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
                       AND  PTRSI-JIBAISEIKBN  NOT  =  2
      *            労災初診金額金額
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(SUM-DRMAX 10)
                        ADD    SYU-RSISHOSHIN-MONEY   TO   WRK-HKNNAI
      *
      *            労災再診金額金額
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(SUM-DRMAX 10)
                        ADD   SYU-RSISAISHIN-MONEY    TO   WRK-HKNNAI
      *
      *            労災指導金額金額
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(SUM-DRMAX 10)
                        ADD   SYU-RSISDO-MONEY    TO   WRK-HKNNAI
      *
      *            労災その他金額金額
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(SUM-DRMAX 10)
                        ADD    SYU-RSIETC-MONEY   TO   WRK-HKNNAI
      *
                        ADD  SYU-HKNTEKMONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-HKNTEKMONEY
                               TO  SUM-YOSO(SUM-DRMAX 10)
                        ADD    SYU-HKNTEKMONEY   TO   WRK-HKNNAI
                   ELSE
      *            主保険分負担金額
      *
                   IF      SYU-SYUCOMPTOTAL  NOT = ZERO 
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(SUM-DRMAX 10)
                           MOVE    SYU-SYUCOMPTOTAL    TO   WRK-HKNNAI
                   ELSE IF SYU-KOH1COMPTOTAL NOT = ZERO
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(SUM-DRMAX 10)
                           MOVE    SYU-KOH1COMPTOTAL   TO   WRK-HKNNAI
                   ELSE IF SYU-KOH2COMPTOTAL NOT = ZERO
                           ADD SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(SUM-DRMAX 10)
                           MOVE    SYU-KOH2COMPTOTAL   TO   WRK-HKNNAI
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(SUM-DRMAX 10)
                           MOVE    SYU-KOH3COMPTOTAL   TO   WRK-HKNNAI
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(SUM-DRMAX 10)
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
                          TO  SUM-YOSO(SUM-DRMAX 11)
      *    減免金額
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(I3 12)
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(SUM-DRMAX 12)
      *    助成金額の算出
                   COMPUTE  WRK-MONEY  =  WRK-HKNNAI
                                       +  SYU-CHOSEI
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
                         IF   WRK-JIHI(11:1)  NOT =  0
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
                         IF   WRK-JIHI(11:1)  NOT =  0
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
                         IF   WRK-JIHI(11:1)  NOT =  0
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
                         IF   WRK-JIHI(11:1)  NOT =  0
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
                       COMPUTE  WRK-JOSEI  =  WRK-JOSEI  /  10
                       COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10 
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
                   ADD    WRK-JOSEI    TO   SUM-YOSO(I3 13) 
                   ADD    WRK-JOSEI    TO   SUM-YOSO(SUM-DRMAX 13)
      *
      *    消費税              
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(I3 14)
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(SUM-DRMAX 14)
      *    調整金              
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(I3 15)
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(SUM-DRMAX 15)
      *    窓口請求金額              
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(I3 16)
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(SUM-DRMAX 16)
      *    未収金残額              
                   COMPUTE     WRK-MONEY        =
                               SYU-SKYMONEY     -    SYU-NYUKIN-TOTAL
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(I3 19)
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(SUM-DRMAX 19)
      *
           IF  SYU-TOTAL-HKNTEN  NOT =  ZERO
             ADD    1                    TO  WRK-HNINZU(I3)
             ADD    1                    TO  WRK-HNINZU(SUM-DRMAX)
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
           PERFORM  320-SYUMEI-SELECT-SEC
      *
           PERFORM   UNTIL  FLG-SYUMEI = 1
             IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
               AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
               COMPUTE  WRK-SMEI-NYUHEN-MONEY = 
                        WRK-SMEI-NYUHEN-MONEY + SMEI-NYUHEN-MONEY
             ELSE
               COMPUTE  WRK-SMEI-NYUHEN-MONEY =
                        WRK-SMEI-NYUHEN-MONEY + 0
             END-IF
             PERFORM  320-SYUMEI-FETCH-SEC
           END-PERFORM.
      *
           PERFORM  320-SYUMEI-CLOSE-SEC
           ADD  WRK-SMEI-NYUHEN-MONEY  TO  SUM-YOSO(I3 18)
           ADD   WRK-SMEI-NYUHEN-MONEY  TO  SUM-YOSO(SUM-DRMAX 18)
           .
       320-SYUMEI-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    入金  SELECT
      *****************************************************************
       320-SYUMEI-SELECT-SEC           SECTION.
      *
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    "2"                 TO  SMEI-NYUGAIKBN
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
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SUB-SEC      SECTION.
      *
      *    初診／再診判定サブ
           INITIALIZE                          SS003-AREA
           MOVE    "1"                 TO      SS003-SYORIKBN
           MOVE    SYU-HOSPNUM         TO      SS003-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO      SS003-NYUGAIKBN
           MOVE    SYU-PTID            TO      SS003-PTID
           MOVE    SYU-DENPNUM         TO      SS003-DENPNUM
           CALL    "ORCSS003"          USING   SS003-AREA
                                               SPA-AREA
      *
           EVALUATE      SS003-SYOSAISIN-KBN
      *    初診件数カウント
           WHEN    "1"
             ADD    1                    TO  SUM-YOSO(I3 2)
                                             SUM-YOSO(SUM-DRMAX 2)
                                             SUM-YOSO(I3 5)
                                             SUM-YOSO(SUM-DRMAX 5)
      *    再診件数カウント
           WHEN    "2"
             ADD    1                    TO  SUM-YOSO(I3 3)
                                             SUM-YOSO(SUM-DRMAX 3)
                                             SUM-YOSO(I3 5)
                                             SUM-YOSO(SUM-DRMAX 5)
      *    その他件数カウント
           WHEN    OTHER
             ADD    1                    TO  SUM-YOSO(I3 4)
                                             SUM-YOSO(SUM-DRMAX 4)
                                             SUM-YOSO(I3 5)
                                             SUM-YOSO(SUM-DRMAX 5)
           END-EVALUATE
      *
           .
       320-JYURRK-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    預り金対象者抽出者処理
      *****************************************************************
       320-AZUKARI-GET-SEC      SECTION.
      *
           INITIALIZE                      SRYACT-REC
           MOVE   SYU-HOSPNUM           TO  SRY-HOSPNUM  
           MOVE   SYU-NYUGAIKBN        TO  SRY-NYUGAIKBN 
           MOVE   SYU-PTID             TO  SRY-PTID
           MOVE   WRK-INI-AZUKARICD    TO  SRY-SRYCD(1)
                                           SRY-SRYCD(2)
                                           SRY-SRYCD(3)
                                           SRY-SRYCD(4)
                                           SRY-SRYCD(5)
           MOVE   I3                   TO  WRK-ACCT-IN
      *    集計日範囲
           MOVE   SYU-SRYYMD(1:6)      TO  SRY-SRYYM
           MOVE   SYU-SRYYMD(7:2)      TO  WRK-ACCT-SDAY
           MOVE   SYU-SRYYMD(7:2)      TO  WRK-ACCT-EDAY
      *    
           PERFORM  911-SRY-SELECT-SEC
           PERFORM  UNTIL      FLG-SRYACT  = 1
               IF    FLG-SRYACT     =   ZERO
                AND  SRY-NYUGAIKBN  =   "2"
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
           IF  ( D100LNK-DRCD  =  SPACE
               OR ( D100LNK-DRCD NOT =  SPACE
               AND  D100LNK-DRCD  =  JYURRK-DRCD))
               AND  FLG-JYURRK  =  ZERO
             PERFORM  VARYING  IDX6  FROM 1  BY 1
                      UNTIL  IDX6  >  15
               IF  JYURRK-ZAINUM(IDX6)   =   ACCT-ZAINUM
                 COMPUTE  IDX5   =   IDX4   +   1
                 COMPUTE  WRK-AZUKARI  =  ACCT-DAY(IDX5 WRK-ACCT-SDAY)
                                       *  WRK-JIHIMONEY
                 ADD   WRK-AZUKARI   TO   SUM-YOSO(WRK-ACCT-IN 20)
                 ADD   WRK-AZUKARI   TO   SUM-YOSO(SUM-DRMAX 20)
               END-IF
             END-PERFORM
           END-IF
           .
       320-AZUKARI-GET-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師インデックス判別処理
      *****************************************************************
       320-DR-GROUPING-SEC      SECTION.
      *
             MOVE  1             TO  I3
             MOVE  SUM-DRMAX     TO  I4
             PERFORM  UNTIL  I3  >   I4
                      IF   SUM-DRCD(I3) =  WRK-DRCD
                           MOVE  ZERO   TO I4
                      ELSE
                           ADD   1      TO I3
                      END-IF
             END-PERFORM
      *
           .
       320-DR-GROUPING-EXT.
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
           INITIALIZE                   D127
           INITIALIZE                   D1271
      *    帳票サブタイトル
           MOVE    "(外来・医師別)"     TO  D127-SUBTITLE
           MOVE    "(外来・医師別)"     TO  D1271-SUBTITLE
      *    対象範囲期間
           MOVE    WRK-SRYYMD1          TO  D127-FRYMD
           MOVE    WRK-SRYYMD2          TO  D127-TOYMD
           MOVE    WRK-SRYYMD1          TO  D1271-FRYMD
           MOVE    WRK-SRYYMD2          TO  D1271-TOYMD
      *    診療科名称
           MOVE    SUM-SRYKANAME        TO  D127-SRYKA
           MOVE    SUM-SRYKANAME        TO  D1271-SRYKA
      *    帳票出力年月日
           MOVE    WRK-SYSYMDWH         TO  D127-SYSYMD
           MOVE    WRK-SYSYMDWH         TO  D1271-SYSYMD
      * 
      *    外来の自費タイトル設定
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >  10 
             MOVE    WRK-TJIHI(IDX2)      TO  D1271-TJIHI(IDX2)
           END-PERFORM
      *
           MOVE    1                    TO  I3
      *
           IF  SUM-DRMAX  >  8
             MOVE    8                    TO  I5
           ELSE
             MOVE    SUM-DRMAX            TO  I5
           END-IF
      *
           PERFORM  UNTIL  I3  >  SUM-DRMAX
      *
             PERFORM  VARYING  I6  FROM  1  BY  1
                      UNTIL    I6  >  I5
                      OR       I3  >  SUM-DRMAX
      *
               IF  I3  =  SUM-DRMAX
                 MOVE    "計"                  TO  D127-TDRNAME(8)
                                                   D1271-TDRNAME(8)
                                                   SUM-DRNAME(I3)
               ELSE
                 MOVE    SUM-DRNAME(I3)(1:18)  TO  D127-TDRNAME(I6)
                                                   D1271-TDRNAME(I6)
                                                   SUM-DRNAME(I3)
               END-IF
      *
               PERFORM VARYING  I4  FROM  2  BY  1
                       UNTIL    I4  >  48
      *
                 COMPUTE  I7  =  I4  -  1
      *
                 IF  I3  =  SUM-DRMAX
      *
                   IF     I4 =  1
                     OR   I4 = 21
                     OR ( I4 = 38 AND WRK-TJIHI-FLG(1)  = ZERO )
                     OR ( I4 = 39 AND WRK-TJIHI-FLG(2)  = ZERO )
                     OR ( I4 = 40 AND WRK-TJIHI-FLG(3)  = ZERO )
                     OR ( I4 = 41 AND WRK-TJIHI-FLG(4)  = ZERO )
                     OR ( I4 = 42 AND WRK-TJIHI-FLG(5)  = ZERO )
                     OR ( I4 = 43 AND WRK-TJIHI-FLG(6)  = ZERO )
                     OR ( I4 = 44 AND WRK-TJIHI-FLG(7)  = ZERO )
                     OR ( I4 = 45 AND WRK-TJIHI-FLG(8)  = ZERO )
                     OR ( I4 = 46 AND WRK-TJIHI-FLG(9)  = ZERO )
                     OR ( I4 = 47 AND WRK-TJIHI-FLG(10) = ZERO )
      *
                     IF  I7  >  35
                       IF  I7  =  47
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  SPACE  TO  D127-YOSO(8 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  35
                         MOVE  SPACE  TO  D1271-YOSO(8 IDX9)
                       END-IF
                     ELSE
                       MOVE  SPACE  TO  D127-YOSO(8 I7)
                     END-IF
      *
                   ELSE
      *
                     MOVE  SUM-YOSO(I3 I4)  TO  WRK-S9
                     IF  I7  >  35
                       IF  I7  =  47
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  WRK-S9  TO  D127-YOSO(8 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  35
                         MOVE  WRK-S9  TO  D1271-YOSO(8 IDX9)
                       END-IF
                     ELSE
                       MOVE  WRK-S9  TO  D127-YOSO(8 I7)
                     END-IF
      *
                   END-IF
      *
                 ELSE
      *
                   IF     I4 =  1
                     OR   I4 = 21
                     OR ( I4 = 38 AND WRK-TJIHI-FLG(1)  = ZERO )
                     OR ( I4 = 39 AND WRK-TJIHI-FLG(2)  = ZERO )
                     OR ( I4 = 40 AND WRK-TJIHI-FLG(3)  = ZERO )
                     OR ( I4 = 41 AND WRK-TJIHI-FLG(4)  = ZERO )
                     OR ( I4 = 42 AND WRK-TJIHI-FLG(5)  = ZERO )
                     OR ( I4 = 43 AND WRK-TJIHI-FLG(6)  = ZERO )
                     OR ( I4 = 44 AND WRK-TJIHI-FLG(7)  = ZERO )
                     OR ( I4 = 45 AND WRK-TJIHI-FLG(8)  = ZERO )
                     OR ( I4 = 46 AND WRK-TJIHI-FLG(9)  = ZERO )
                     OR ( I4 = 47 AND WRK-TJIHI-FLG(10) = ZERO )
      *
                     IF  I7  >  35
                       IF  I7  =  47
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  SPACE  TO  D127-YOSO(I6 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  35
                         MOVE  SPACE  TO  D1271-YOSO(I6 IDX9)
                       END-IF
                     ELSE
                       MOVE  SPACE  TO  D127-YOSO(I6 I7)
                     END-IF
      *
                   ELSE
      *
                     MOVE  SUM-YOSO(I3 I4)  TO  WRK-S9
                     IF  I7  >  35
                       IF  I7  =  47
                         COMPUTE  IDX9  =  I7  -  11
                         MOVE  WRK-S9  TO  D127-YOSO(I6 IDX9)
                       ELSE
                         COMPUTE  IDX9  =  I7  -  35
                         MOVE  WRK-S9  TO  D1271-YOSO(I6 IDX9)
                       END-IF
                     ELSE
                       MOVE  WRK-S9  TO  D127-YOSO(I6 I7)
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
             MOVE    8                      TO  I5
             INITIALIZE                     D127-DATA
             INITIALIZE                     D1271-DATA
      *
           END-PERFORM 
      *
      *    CSV編集処理
           PERFORM  800-CSV-HEAD-SEC
           PERFORM  400-CSV-DATA-SEC
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
           MOVE    WRK-PAGE                       TO  D127-PAGE
      *
           ADD     1                              TO  CNT-PAGE
           MOVE    CNT-PAGE                       TO  WRK-PAGE
           MOVE    WRK-PAGE                       TO  D1271-PAGE
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
             MOVE    "A00000D127O.red"              TO  SPRT-PRTID
           ELSE
             MOVE    "A00000D127N.red"              TO  SPRT-PRTID
           END-IF
      *
           MOVE    "基本台帳(外来・医師別)"       TO  SPRT-TITLE
           MOVE    D127                           TO  SPRT-PRTDATA
           MOVE    D100LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    D100LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    D100LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"                      USING
                                                  ORCSPRTAREA
                                                  SPA-AREA
      *
           IF  SPRT-RETURN  =  ZERO
             MOVE    "A00000D1271.red"              TO  SPRT-PRTID
             MOVE    "基本台帳(外来・医師別)"       TO  SPRT-TITLE
             MOVE    D1271                          TO  SPRT-PRTDATA
             MOVE    D100LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
             MOVE    D100LNK-PRTKANRI-OPID          TO  SPRT-OPID
             MOVE    D100LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
             CALL    "ORCSPRT"                      USING
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
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                    TO  IDXP
      *
      *    印字対象診療年月
           INITIALIZE  WRK-ITEM
           MOVE    "対象年月日："       TO  WRK-ITEM(1:12)
           MOVE    WRK-SRYYMD1(1:22)    TO  WRK-ITEM(13:22)
           MOVE    "〜"                 TO  WRK-ITEM(35:2)
           MOVE    WRK-SRYYMD2(1:22)    TO  WRK-ITEM(37:22)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科名称
           INITIALIZE  WRK-ITEM
           MOVE    "診療科："           TO  WRK-ITEM(1:8)
           MOVE    SUM-SRYKANAME        TO  WRK-ITEM(9:20)
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
      *    項目名称セット
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  CSV-HEAD-MAX
      *
             INITIALIZE  WRK-ITEM
             IF  IDX4  <  39
               MOVE    CSV-HEAD-ITEM(IDX4)        TO  WRK-ITEM
             ELSE
               MOVE    CSV-HEAD-ITEM(IDX4)(1:18)  TO  WRK-ITEM(1:18)
               MOVE    WRK-TJIHI(IDX2)            TO  WRK-ITEM(19:)
               ADD     1                          TO  IDX2
             END-IF
             PERFORM  800-CSVDATA-EDIT-SEC
      *
             PERFORM  VARYING  IDX-DRCD  FROM  1  BY  1
                      UNTIL    IDX-DRCD  >  SUM-DRMAX
               INITIALIZE  WRK-ITEM
               IF  SUM-DRNAME(IDX-DRCD)  NOT =  SPACE
                 EVALUATE  IDX4
                   WHEN   1
                     MOVE    SUM-DRNAME(IDX-DRCD)         TO  WRK-ITEM
                   WHEN  21
                     CONTINUE
                   WHEN  35
                     MOVE    SUM-YOSO(IDX-DRCD IDX4 + 13) TO  WRK-S9
                     MOVE    WRK-S9                       TO  WRK-ITEM
                   WHEN  36 THRU 48
                     MOVE    SUM-YOSO(IDX-DRCD IDX4 - 1)  TO  WRK-S9
                     MOVE    WRK-S9                       TO  WRK-ITEM
                   WHEN  OTHER
                     MOVE    SUM-YOSO(IDX-DRCD IDX4)      TO  WRK-S9
                     MOVE    WRK-S9                       TO  WRK-ITEM
                 END-EVALUATE
                 IF  IDX-DRCD  <  SUM-DRMAX
                   PERFORM  800-CSVDATA-EDIT-SEC
                 END-IF
               END-IF
             END-PERFORM
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
                   "kihondaichou_g_dr_"       DELIMITED  BY  SIZE
                   D100LNK-YMD1               DELIMITED  BY  SIZE
                   "_"                        DELIMITED  BY  SIZE
                   D100LNK-YMD2               DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "基本台帳(外来・医師別)"   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
                                              ORCSTOUKEICSVAREA
                                              SPA-AREA
           DISPLAY "STOUKEICSV-TITLE " STOUKEICSV-TITLE
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
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"          TO      LNK-DAY7-IRAI
           MOVE    WRK-SYMD      TO      LNK-DAY7-YM
           CALL    "ORCSDAY"     USING   STS-AREA-DAY
                                         LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO      WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC             SECTION.
      *
           INITIALIZE                 SYUNOU-REC
      *
           MOVE    D100LNK-HOSPNUM   TO   SYU-HOSPNUM
           MOVE    "2"              TO   SYU-NYUGAIKBN
           MOVE    "00000000"       TO   SYU-SRYYMD
           MOVE    D100LNK-YMD2     TO   SYU-UPSRYYMD
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "st_key1"                  TO  MCP-PATHNAME
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
      *    収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "st_key1"                  TO  MCP-PATHNAME
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
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "st_key1"                  TO  MCP-PATHNAME
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
           MOVE    "2"               TO   SYU-NYUGAIKBN
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
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
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
      *    管理マスタ検索
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           EVALUATE  FLG-820SYS
               WHEN  "1005"
                     MOVE    SYS-1005-REC TO  MCPDATA-REC  
               WHEN  "1010"
                     MOVE    SYS-1010-REC TO  MCPDATA-REC
           END-EVALUATE
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
           END-IF        
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               EVALUATE  FLG-820SYS
                   WHEN  "1005"
                         MOVE    MCPDATA-REC  TO  SYS-1005-REC
                   WHEN  "1010"
                         MOVE    MCPDATA-REC  TO  SYS-1010-REC
               END-EVALUATE
               MOVE    ZERO         TO  FLG-SYSKANRI3
           ELSE
               MOVE    1            TO  FLG-SYSKANRI3
           END-IF  
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_ptinf"             TO  MCP-TABLE
             MOVE    "key"                  TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
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
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
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
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
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
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報SELECT処理
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           INITIALIZE                 JYURRK-REC
           MOVE    SYU-HOSPNUM        TO   JYURRK-HOSPNUM
           MOVE    SYU-PTID           TO   JYURRK-PTID
           MOVE    SYU-NYUGAIKBN      TO   JYURRK-NYUGAIKBN
           MOVE    SYU-DENPNUM        TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO                TO   FLG-JYURRK
             PERFORM  910-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                TO   FLG-JYURRK
             INITIALIZE                    JYURRK-REC
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    受診履歴情報FETCH処理
      *****************************************************************
       910-JYURRK-FETCH-SEC         SECTION.
      *
           MOVE   "DBFETCH"         TO   MCP-FUNC
           MOVE   "tbl_jyurrk"      TO   MCP-TABLE
           MOVE   "key6"            TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC    TO   JYURRK-REC
           ELSE
             MOVE    1              TO   FLG-JYURRK
             INITIALIZE             JYURRK-REC
           END-IF
      *
           .
       910-JYURRK-FETCH-EXT.
           EXIT.
      *      
      *****************************************************************
      *    受診履歴情報CLOSE処理
      *****************************************************************
       910-JYURRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       900-JYURRK-SELECT-SEC              SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SYU-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    SYU-PTID        TO  JYURRK-PTID  
           MOVE    SYU-NYUGAIKBN   TO  JYURRK-NYUGAIKBN  
           MOVE    "00000000"      TO  JYURRK-SRYYMD  
           MOVE    SYU-SRYYMD      TO  JYURRK-UPSRYYMD  
      *
           MOVE    JYURRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key14"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
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
           MOVE    "DBFETCH"        TO  MCP-FUNC
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key14"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
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
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key14"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       911-SRY-SELECT-SEC              SECTION.
      *
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
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
           MOVE    "DBFETCH"        TO  MCP-FUNC
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
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
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
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
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
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
           INITIALIZE                 HKNNUM-REC
           MOVE    SYU-HOSPNUM     TO  HKN-HOSPNUM  
           MOVE    SYU-SYUHKNNUM  TO  HKN-HKNNUM  
           MOVE    D100LNK-YMD2   TO  HKN-TEKSTYMD
           MOVE    ALL "9"        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hknnum"             TO  MCP-TABLE
               MOVE    "key5"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
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
           MOVE    SRYACCT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
               MOVE    "tbl_sryacct"             TO  MCP-TABLE
               MOVE    "key3"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SRYACCT-REC
                   MOVE    ZERO          TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
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
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC               SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-SRYACT
           ELSE
               MOVE    1           TO  FLG-SRYACT
           END-IF        
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報FETCH
      *****************************************************************
       900-SRYACT-FETCH-SEC                SECTION.
      *
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-SRYACT
               MOVE    MCPDATA-REC  TO  SRYACT-REC
           ELSE
               MOVE    1            TO  FLG-SRYACT
               INITIALIZE               SRYACT-REC
           END-IF  
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報 ＤＢ　クローズ処理
      *****************************************************************
       900-SRYACT-CLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
       900-JYURRK-DRCD-READ-SEC         SECTION.
      *
           MOVE    JYURRK-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key19"                  TO  MCP-PATHNAME
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key19"                  TO  MCP-PATHNAME
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
      *    診療会計情報  検索
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           MOVE    SRYACCT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                        SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SRYACCT
           ELSE
               MOVE    1            TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
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
      *    診療科履歴情報検索処理
      *****************************************************************
       900-SRYKARRK-SELECT-SEC            SECTION.
      *
           INITIALIZE               SRYKARRK-REC
      *
           MOVE    SYU-HOSPNUM      TO   KARRK-HOSPNUM
           MOVE    SYU-PTID         TO   KARRK-PTID
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM  900-SRYKARRK-FETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       900-SRYKARRK-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"         TO   MCP-FUNC
           MOVE    "tbl_srykarrk"    TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO   SRYKARRK-REC 
             MOVE    ZERO            TO   FLG-SRYKARRK
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       900-SRYKARRK-CLOSE-SEC            SECTION.
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBCLOSECURSOR"  TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       900-SRYKARRK-CLOSE-EXT.
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
