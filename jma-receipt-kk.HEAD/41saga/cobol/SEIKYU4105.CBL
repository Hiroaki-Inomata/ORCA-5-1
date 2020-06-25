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
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU4105.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 佐賀・複写式レセプト
      *                      (181)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/21    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者	日付		内容
      *  01.00.01    有村    05/05/06    医療機関情報取得条件修正
      *  01.00.02    内田    07/03/08    転帰が正しく出るよう修正
      *                                  プレビュー時患者番号を表示
      *  01.00.03    内田    07/09/30    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  長期上位（974）は長期と同様の
      *                                  処理をする
      *  01.00.04    門間    08/11/20    SPA-AREA初期化
      *  01.00.05    吉川    10/01/13    区切り線がある場合、傷病名欄と
      *                                  転帰欄の番号がずれる不具合を修正
      *  01.00.06    吉川    10/03/03    乳幼児の食事欄は
      *                                  空白ではなくゼロを印字する
      *  01.00.07    吉川    10/03/15    転帰欄の番号がずれる不具合を修正
      *  01.00.08    吉川    10/04/13    外来様式で摘要欄の下部にゼロが
      *                                  印字されるのを修正
      *  01.00.09    吉川    10/08/18    受給者番号の全角入力対応
      *  01.00.10    吉川    12/02/15    主病名が一つのみの場合
      *                                  区切り線の次の番号を正しくする
      *  01.00.11    吉川    13/02/07    医療機関情報編集対応
      *  01.00.12    吉川    14/10/27    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.13    萩野    19/02/07    修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF101-FILE   ASSIGN    MF101PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF101-KEY
                               FILE    STATUS  IS  STS-MF101.
      *
      *    レセプト０３１データ
           SELECT  RECE031-FILE     ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF101-FILE.
       01  MF101-REC.
           COPY  "SEI4105.INC".
      *
      *    レセプト０３１データ
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF101//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE031//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    MONPE用コピー句
           COPY  "SC4105.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101       PIC X(02).
           03  STS-RECE02      PIC X(02).
           03  STS-RECEERR     PIC X(02).
           03  STS-RECE        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF       PIC 9(01).
           03  FLG-END         PIC 9(01).
           03  FLG-MF101       PIC 9(01).
           03  FLG-PRINT       PIC 9(01).
           03  FLG-SYSKANRI    PIC 9(01).
           03  FLG-SYSKANRI3   PIC 9(01).
           03  FLG-ERR         PIC 9(01).
           03  FLG-SYUNOU      PIC 9(01).
           03  FLG-RECE10      PIC 9(01).
           03  FLG-PTKOHINF    PIC 9(01).
           03  FLG-RECE031     PIC 9(01).
           03  FLG-HKNCOMBI    PIC 9(01).
           03  FLG-TENKI       PIC 9(01).
           03  FLG-KUGIRI      PIC 9(01).
           03  FLG-BTPARA      PIC 9(01).
           03  FLG-SYORI       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *    患者病名編集用パラメタ領域
       01  WRK-PARA-LINK.
           05  WRK-PARA-LINK-SRYYM   PIC  X(06).
           05  WRK-PARA-LINK-TERMID  PIC  X(16).
           05  WRK-PARA-LINK-SYSYMD  PIC  X(08).
           05  WRK-PARA-LINK-JOBID   PIC  9(07).
           05  WRK-PARA-LINK-SHELLID PIC  X(08).
           05  WRK-PARA-LINK-HOSPNUM  PIC  9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z1                PIC Z.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z29               PIC Z9.
           03  WRK-Z79               PIC ZZZ,ZZ9.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY           PIC 9(04).
               05  WRK-NMM           PIC 9(02).
               05  WRK-NDD           PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY           PIC 9(04).
               05  WRK-TMM           PIC 9(02).
               05  WRK-TDD           PIC 9(02).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-PATH              PIC X(64).
           03  WRK-PRTID             PIC X(20).
           03  WRK-KOHNUM            PIC X(03).
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
           03  WRK-HENKAN            PIC X(20).
      *
           03  WRK-KOHID               PIC 9(10).
           03  WRK-FTNMONEY            PIC 9(08).
           03  WRK-FTNMONEY-G          PIC 9(08).
      *
      *    医療機関情報
           03  WRK-HOSPNAME          PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1     PIC X(50).
               05  WRK-HOSPNAME2     PIC X(50).
           03  WRK-HOSPADRS          PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1     PIC X(50).
               05  WRK-HOSPADRS2     PIC X(50).
           03  WRK-HOSPTEL           PIC X(15).
      *
      *    病名退避テーブル
           03  WRK-TAIHI-TBL.
                05  WRK-TAIHI-BYOMEI   PIC X(54)   OCCURS  5 TIMES.
                05  WRK-TAIHI-SRYYMD   PIC X(24)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TENKI1   PIC X(04)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TENKI2   PIC X(04)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TENKI3   PIC X(04)   OCCURS  5 TIMES.
                05  WRK-TAIHI-TEKIYO   PIC X(52)   OCCURS  35 TIMES.
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
           03  WRK-EDIT-AREA.
      *        転記用カウント
               05  WRK-CNT-TENKI1              PIC 9(01).
               05  WRK-CNT-TENKI2              PIC 9(01).
               05  WRK-CNT-TENKI3              PIC 9(01).
      *        傷病名連番     
               05  WRK-LINE                    PIC 9(02).
      *        編集傷病名文字数 
               05  WRK-CLM                     PIC 9(03).
      *        ワーク編集傷病名文字数 
               05  WRK-WCLM                    PIC 9(03).
      *        病名文字数         
               05  WRK-MOJISU                  PIC 9(03).
      *        編集開始位置         
               05  WRK-STCLM                   PIC 9(03).
      *        傷病名欄編集行         
               05  WRK-LINEBYO                 PIC 9(02).
      *        適用欄編集行         
               05  WRK-LINETEK                 PIC 9(02).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
      *    傷病名編集
           03  WRK-BYOMEI                      PIC X(150).
           03  WRK-BYOMEIW                     PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1                     PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2                     PIC X(01). 
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1                     PIC X(54).
           03  WRK-BYOMEI1-W   REDEFINES       WRK-BYOMEI1.
               05  WRK-BYOMEI1-NO              PIC X(06).
               05  WRK-BYOMEI1-NAME            PIC X(48).
               
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2                     PIC X(52).
           03  WRK-BYOMEI2-W   REDEFINES       WRK-BYOMEI2.
               05  WRK-BYOMEI2-NO              PIC X(08).
               05  WRK-BYOMEI2-NAME            PIC X(44).
               05  WRK-BYOMEI2-NAMEW   REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(20).
                   07  WRK-BYOMEI2-ELSE.
                       09  WRK-BYOMEI2-YMD     PIC X(20).
                       09  WRK-BYOMEI2-TENKI   PIC X(04).
               05  WRK-BYOMEI2R-NAMEW  REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(14).
                   07  WRK-BYOMEI2R-ELSE.
                       09  WRK-BYOMEI2R-YMD    PIC X(20).
                       09  WRK-BYOMEI2R-TENKI  PIC X(04).
                       09  WRK-BYOMEI2R-TENKIYMD
                                               PIC X(06).
      *
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   48.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   44.
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   35.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.       
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "−".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(22)   VALUE
                                   "＝＝＝＝＝＝＝　以下　".
               05  WRK-KUGIRI2-KOHNAME         PIC X(10).
               05  FILLER          PIC X(20)   VALUE
                                   "　適用分　＝＝＝＝＝".
           03  WRK-KUGIRI3         PIC X(52)   VALUE
               ALL "−".
      *
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF101   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-TENKI1  PIC 9(02).
           03  CNT-TENKI2  PIC 9(02).
           03  CNT-TENKI3  PIC 9(02).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
               USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
      *
           DISPLAY "START"
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    地方公費対象患者用病名ファイル作成
           PERFORM 1001-BYOMEI-HEN-SEC
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU4105"    TO  JOB-PGID
           MOVE    "複写式レセプト" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
           MOVE    "RECE031"                   TO  RECE031PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID         TO  RECE031PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM              TO  RECE031PARA-HOSPNUM
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF101PARA-HOSPNUM
           MOVE  "MF101ZZ"            TO  MF101PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF101PARA-TERMID
      *
           MOVE  MF101PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECE031PARA-BASENAME TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF101PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECE031PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-1001-STYUKYMD(1:6)
                                       SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-1001-STYUKYMD(7:2)
                                       SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =  ZERO
               MOVE    MCPDATA-REC TO  SYS-1001-REC
           ELSE
               DISPLAY "*** SEIKYU4105 SYS 1001 ERR ***"
               MOVE  1             TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-1002-STYUKYMD(1:6)
                                       SYS-1002-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-1002-STYUKYMD(7:2)
                                       SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1002-REC
           ELSE
               DISPLAY "*** SEIKYU4105 SYS 1001 ERR ***"
               MOVE    1           TO  FLG-END
           END-IF
      *
      *    レセプト編集区分情報
           INITIALIZE                   SYS-2005-REC
           MOVE    "2005"           TO  SYS-2005-KANRICD
           MOVE    "*"              TO  SYS-2005-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-2005-STYUKYMD(1:6)
                                       SYS-2005-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-2005-STYUKYMD(7:2)
                                       SYS-2005-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-2005-HOSPNUM
           MOVE  SYS-2005-REC       TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN
           END-IF
           IF      SYS-2005-TENKIHENKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF 
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費対象患者用病名ファイル作成
      *****************************************************************
       1001-BYOMEI-HEN-SEC         SECTION.
      *
           INITIALIZE  WRK-PARA-LINK
           MOVE  LNK-PRTKANRI-SRYYM   TO   WRK-PARA-LINK-SRYYM
           MOVE  LNK-PRTKANRI-TERMID  TO   WRK-PARA-LINK-TERMID
           MOVE  LNK-PRTKANRI-SKYYMD  TO   WRK-PARA-LINK-SYSYMD
           MOVE  WRK-PARA-JOBID       TO   WRK-PARA-LINK-JOBID
           MOVE  WRK-PARA-SHELLID     TO   WRK-PARA-LINK-SHELLID
           MOVE  WRK-PARA-HOSPNUM     TO   WRK-PARA-LINK-HOSPNUM
      *
           DISPLAY "編集１開始"
           CALL  "SEIKYU41BYOMEI1"    USING  WRK-PARA-LINK
           DISPLAY "編集２開始"
           CALL  "SEIKYU41BYOMEI2"    USING  WRK-PARA-LINK
           DISPLAY "病名ファイル作成終了"
      *
           .
       1001-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN     OUTPUT  MF101-FILE
           CLOSE            MF101-FILE
           OPEN     I-O     MF101-FILE
           OPEN    INPUT    RECE031-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF101-FILE
      *
           IF       CNT-RECE02   >       0
                    PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           DISPLAY "2001 START"
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               IF      RECE02-KOHNUM (IDZ) = "181"
                   PERFORM 20011-SYUKEI-SEC
                   MOVE    4           TO  IDZ
               END-IF
           END-PERFORM
      *
      *
           PERFORM  900-KOHSKY-READ-SEC
           DISPLAY "2001 END"
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF101-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF101-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF101-PTNUM
           MOVE  RECE02-RECEKA       TO  MF101-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF101-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF101-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF101-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF101-SRYYM
           MOVE  RECE02-TEISYUTUSAKI TO  MF101-TEISYUTUSAKI
           MOVE  RECE02-RECESYUBETU  TO  MF101-RECESYUBETU
           MOVE  RECE02-HOJOKBN-KEY  TO  MF101-HOJOKBN-KEY
      *
           MOVE  RECE02-PTID         TO  MF101-PTID
           MOVE  RECE02-NAME         TO  MF101-NAME
           MOVE  RECE02-KANANAME     TO  MF101-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF101-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF101-SEX
           MOVE  RECE02-AGE          TO  MF101-AGE
           MOVE  RECE02-HKN          TO  MF101-HKN
           MOVE  RECE02-RJNHKN       TO  MF101-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF101-ZAITAKU
      *
      *    長期判定
           PERFORM 900-SEIKYU-READ-SEC
           IF      RECE10-ETC-KOHNUM(1)  =   "972"
           OR      RECE10-ETC-KOHNUM(2)  =   "972"
           OR      RECE10-ETC-KOHNUM(3)  =   "972"
           OR      RECE10-ETC-KOHNUM(4)  =   "972"
               MOVE    1       TO  MF101-TYOKI
           END-IF
      *    長期上位
           IF      RECE10-ETC-KOHNUM(1)  =   "974"
           OR      RECE10-ETC-KOHNUM(2)  =   "974"
           OR      RECE10-ETC-KOHNUM(3)  =   "974"
           OR      RECE10-ETC-KOHNUM(4)  =   "974"
               MOVE    2       TO  MF101-TYOKI
           END-IF
      *
      *    月途中で公費を切替えていない場合は請求管理情報より
      *    全国公費の情報を取得する
      *    切替えている場合は２個目の公費を第２公費として扱う
           IF  IDZ         =   1
               AND     RECE02-KOHNUM(IDZ + 1)  =  SPACE
               PERFORM 200121-ETC-KOH-HEN-SEC
           ELSE
               PERFORM 200122-ETC-KOH-HEN-SEC
           END-IF
      *
      *    全角→半角へ
           DISPLAY "患者番号=" MF101-PTNUM
                   "変換前1=" MF101-EKOHJKYSNUM(1)
                   "変換前2=" MF101-EKOHJKYSNUM(2)
           PERFORM  VARYING  IDX1  FROM  1   BY  1
                    UNTIL    IDX1  >     2
             MOVE     SPACE    TO    WRK-HENKAN
             MOVE     ZERO     TO    IDY
             PERFORM  VARYING  IDX2  FROM  1   BY  2
                      UNTIL    IDX2  >     20
                      OR       MF101-EKOHJKYSNUM(IDX1)(IDX2:2) = SPACE
               EVALUATE  MF101-EKOHJKYSNUM(IDX1)(IDX2:2)
               WHEN "１"
                 ADD      1    TO    IDY
                 MOVE    "1"   TO    WRK-HENKAN(IDY:1)
               WHEN "２"
                 ADD      1    TO    IDY
                 MOVE    "2"   TO    WRK-HENKAN(IDY:1)
               WHEN "３"
                 ADD      1    TO    IDY
                 MOVE    "3"   TO    WRK-HENKAN(IDY:1)
               WHEN "４"
                 ADD      1    TO    IDY
                 MOVE    "4"   TO    WRK-HENKAN(IDY:1)
               WHEN "５"
                 ADD      1    TO    IDY
                 MOVE    "5"   TO    WRK-HENKAN(IDY:1)
               WHEN "６"
                 ADD      1    TO    IDY
                 MOVE    "6"   TO    WRK-HENKAN(IDY:1)
               WHEN "７"
                 ADD      1    TO    IDY
                 MOVE    "7"   TO    WRK-HENKAN(IDY:1)
               WHEN "８"
                 ADD      1    TO    IDY
                 MOVE    "8"   TO    WRK-HENKAN(IDY:1)
               WHEN "９"
                 ADD      1    TO    IDY
                 MOVE    "9"   TO    WRK-HENKAN(IDY:1)
               WHEN "０"
                 ADD      1    TO    IDY
                 MOVE    "0"   TO    WRK-HENKAN(IDY:1)
               END-EVALUATE
             END-PERFORM
      *
             IF  WRK-HENKAN  NOT  =  SPACE
               MOVE    WRK-HENKAN    TO    MF101-EKOHJKYSNUM(IDX1)
             END-IF
      *
           END-PERFORM
           DISPLAY "変換後1=" MF101-EKOHJKYSNUM(1)
           DISPLAY "変換後2=" MF101-EKOHJKYSNUM(2)
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *
           IF      FLG-ERR     =   ZERO
               WRITE   MF101-REC
               ADD     1   TO  CNT-MF101
           ELSE
               DISPLAY "対象外患者=" RECE02-PTNUM
                       "負担者番号１=" MF101-EKOHJKYSNUM(1)
                       "負担者番号２=" MF101-EKOHJKYSNUM(2)
           END-IF
      *
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    RECE02-KOHFTNJANUM(IDZ)(3:5)  TO  WRK-HKNJANUM(1:5)
           DISPLAY "保険者番号=" WRK-HKNJANUM
      *
           IF      WRK-PARA-SYORIFLG   =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
      *
                   IF      (WRK-HKNJANUM(1:5)  =
                                       TBL-PARA-HKNJANUM(IDXX)(1:5))
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM 
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の切替えが無い場合の処理
      *****************************************************************
       200121-ETC-KOH-HEN-SEC                SECTION.
      *
      *
           MOVE    RECE02-KOHFTNJANUM (IDZ)  TO  MF101-FTNJANUM
           MOVE    RECE02-KOHJKYSNUM(IDZ)  TO  MF101-JKYSNUM
           MOVE    RECE02-KOHINF(IDZ)      TO  MF101-KOHINF
           COMPUTE IDY =   IDZ +   1
           MOVE    RECE02-JNISSU(1)        TO  MF101-JNISSU(1)
           MOVE    RECE02-TOTALTEN(1)      TO  MF101-TOTALTEN(1)
           MOVE    RECE02-FTNMONEY(1)      TO  MF101-FTNMONEY(1)
           MOVE    RECE02-SHOKUJIINF(1)    TO  MF101-SHOKUJIINF(1)
      *
      *    月途中で公費を切替えていない場合、請求管理情報より取得
           IF      RECE10-KOHNUM(1) NOT  =  SPACE
             MOVE    RECE10-JNISSU(2)    TO  MF101-JNISSU(2)
             MOVE    RECE10-TOTALTEN(2)  TO  MF101-TOTALTEN(2)
             MOVE    RECE10-FTNMONEY(2)  TO  MF101-FTNMONEY(2)
             MOVE    RECE10-SHOKUJINISSU(2)
                                    TO  MF101-SHOKUJINISSU(2)
             MOVE    RECE10-SHOKUJIRYOYOHI(2)
                                    TO  MF101-SHOKUJIRYOYOHI(2)
             MOVE    RECE10-SHOKUJIFTN(2) 
                                    TO  MF101-SHOKUJIFTN(2)
             MOVE    RECE10-KOHNUM(1)    TO  WRK-KOHNUM
                                             MF101-EKOHNUM(1)
      *    患者公費情報より負担者番号、受給者番号取得
             INITIALIZE                      PTKOHINF-REC
             MOVE    RECE10-HOSPNUM      TO  PTKOH-HOSPNUM
             MOVE    RECE10-PTID         TO  PTKOH-PTID
             MOVE    RECE10-KOHNUM (1)   TO  PTKOH-KOHNUM
      *
             MOVE    RECE10-SRYYM        TO  PTKOH-TEKSTYMD(1:6)
             MOVE    "99"                TO  PTKOH-TEKSTYMD(7:2)
             MOVE    RECE10-SRYYM        TO  PTKOH-TEKEDYMD(1:6)
             MOVE    "01"                TO  PTKOH-TEKEDYMD(7:2)
             MOVE    PTKOHINF-REC        TO  MCPDATA-REC
             PERFORM 930-PTKOHINF-SET-SEC
      *
             MOVE    PTKOH-FTNJANUM      TO  MF101-EKOHFTNJANUM(1)
             MOVE    PTKOH-JKYSNUM       TO  MF101-EKOHJKYSNUM(1)
      *    点数と日数と食事療養は地方公費と同じ値の時は記載を省略する
             IF      MF101-JNISSU(2)     =   MF101-JNISSU(1)
               MOVE  ZERO                TO  MF101-JNISSU(2)
             END-IF
      *
             IF      MF101-TOTALTEN(2)   =   MF101-TOTALTEN(1)
               MOVE  ZERO                    TO  MF101-TOTALTEN(2)
             END-IF
      *
             IF      MF101-SHOKUJINISSU(2)
                                         =   MF101-SHOKUJINISSU(1)
               MOVE  ZERO                TO  MF101-SHOKUJINISSU(2)
             END-IF
      *
             IF      MF101-SHOKUJIRYOYOHI(2)
                                        =   MF101-SHOKUJIRYOYOHI(1)
               MOVE  ZERO               TO  MF101-SHOKUJIRYOYOHI(2)
             END-IF
      *
             IF      MF101-SHOKUJIFTN(2)
                                        =   MF101-SHOKUJIFTN(1)
               MOVE  ZERO               TO  MF101-SHOKUJIFTN(2)
             END-IF
      *
             MOVE    RECE02-FTNMONEY(2) TO  MF101-FTNMONEY(3)
             MOVE    RECE02-KOHFTNJANUM(1)
                                        TO  MF101-EKOHFTNJANUM(2)
             MOVE    RECE02-KOHJKYSNUM(1)
                                        TO  MF101-EKOHJKYSNUM(2)
           ELSE
             MOVE    RECE02-KOHFTNJANUM(1)
                                        TO  MF101-EKOHFTNJANUM(1)
             MOVE    RECE02-KOHJKYSNUM(1)
                                        TO  MF101-EKOHJKYSNUM(1)
             MOVE    RECE02-FTNMONEY(2) TO  MF101-FTNMONEY(2)
           END-IF
           DISPLAY "負担者番号1=" MF101-EKOHFTNJANUM(1)
                   " 2=" MF101-EKOHFTNJANUM(2)
      *
           .
       200121-ETC-KOH-HEN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費切替え対応
      *****************************************************************
       200122-ETC-KOH-HEN-SEC                SECTION.
      *
           MOVE    RECE02-KOHFTNJANUM (IDZ)  TO  MF101-FTNJANUM
           MOVE    RECE02-KOHJKYSNUM  (IDZ)  TO  MF101-JKYSNUM
           MOVE    RECE02-KOHINF      (IDZ)  TO  MF101-KOHINF
           MOVE    RECE02-JNISSU      (1)  TO  MF101-JNISSU(1)
           MOVE    RECE02-TOTALTEN    (1)  TO  MF101-TOTALTEN(1)
           MOVE    RECE02-FTNMONEY(1)      TO  MF101-FTNMONEY(1)
           MOVE    RECE02-SHOKUJIINF  (1)  TO  MF101-SHOKUJIINF(1)
      *
      *    月途中で公費を切替えている場合、２個目を第２公費欄に記載する
           MOVE    RECE02-KOHFTNJANUM(IDZ)
                                       TO  MF101-EKOHFTNJANUM(1)
           MOVE    RECE02-KOHJKYSNUM(IDZ)
                                       TO  MF101-EKOHJKYSNUM(1)
           COMPUTE IDY     =   IDZ +   1
           MOVE    RECE02-JNISSU      (IDY)
                                       TO  MF101-JNISSU(2)
           MOVE    RECE02-TOTALTEN    (IDY)
                                       TO  MF101-TOTALTEN(2)
           MOVE    RECE02-FTNMONEY    (IDY)
                                       TO  MF101-FTNMONEY(2)
           MOVE    RECE02-SHOKUJIINF  (IDY)
                                       TO  MF101-SHOKUJIINF(2)
      *
           MOVE    RECE02-KOHFTNJANUM(IDZ + 1)
                                       TO  MF101-EKOHFTNJANUM(2)
           MOVE    RECE02-KOHJKYSNUM(IDZ + 1)
                                       TO  MF101-EKOHJKYSNUM(2)
           COMPUTE IDY     =   IDZ +   2
           MOVE    RECE02-JNISSU      (IDY)
                                       TO  MF101-JNISSU(3)
           MOVE    RECE02-TOTALTEN    (IDY)
                                       TO  MF101-TOTALTEN(3)
           MOVE    RECE02-FTNMONEY    (IDY)
                                       TO  MF101-FTNMONEY(3)
           MOVE    RECE02-SHOKUJIINF  (IDY)
                                       TO  MF101-SHOKUJIINF(3)
      *
           .
       200122-ETC-KOH-HEN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN    INPUT       MF101-FILE
      *
           MOVE    ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
      *
           PERFORM     UNTIL   FLG-PRINT   =  1
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *
               IF      MF101-NYUGAIKBN =   "1"
                   MOVE    "SC4106.red"    TO  WRK-PRTID
               ELSE
                   MOVE    "SC4105.red"    TO  WRK-PRTID
               END-IF
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    MF101-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理(入院用)
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SC05
      *
      *    診療年月
           MOVE    MF101-SRYYM             TO  WRK-SYMD (1:6)
           MOVE    "01"                    TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO  SC05-PRTYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN        TO  SC05-HOSPCDN
      *
      *    住所
           MOVE  WRK-HOSPADRS              TO  SC05-ADRS
           INSPECT SC05-ADRS               REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME              TO  SC05-HOSPNAME
           INSPECT SC05-HOSPNAME           REPLACING ALL "  " BY "　"
      *
      *    管理者名
           MOVE  SYS-1001-KAISETUNAME      TO  SC05-KANRINAME
      *
      *    保険区分
           EVALUATE    MF101-RECESYUBETU(2:1)
               WHEN    "1"
                   MOVE   "◯"            TO  SC05-HKNKBN(1)
               WHEN    "2"
                   MOVE   "◯"            TO  SC05-HKNKBN(2)
               WHEN    "3"
                   MOVE   "◯"            TO  SC05-HKNKBN(3)
               WHEN    "4"
                   MOVE   "◯"            TO  SC05-HKNKBN(4)
           END-EVALUATE
      *
      *    単独併用区分
           IF    MF101-EKOHNUM(1)     =  SPACE
               MOVE  "○"  TO  SC05-HEIKBN (1)
           ELSE
               MOVE  "○"  TO  SC05-HEIKBN (2)
           END-IF
      *
      *    本人家族区分
           EVALUATE    MF101-RECESYUBETU(4:1)
               WHEN    "1"
               WHEN    "2"
                   MOVE    "◯"    TO  SC05-HONKZKKBN (1)
               WHEN    "3"
               WHEN    "4"
                   MOVE    "◯"    TO  SC05-HONKZKKBN (2)
               WHEN    "5"
               WHEN    "6"
                   MOVE    "◯"    TO  SC05-HONKZKKBN (3)
               WHEN    "7"
               WHEN    "8"
                   MOVE    "◯"    TO  SC05-HONKZKKBN (4)
               WHEN    "9"
               WHEN    "0"
                   MOVE    "◯"    TO  SC05-HONKZKKBN (5)
           END-EVALUATE
      *
      *    記号
           MOVE  MF101-KIGO          TO  SC05-KIGO
      *    番号
           MOVE  MF101-NUM           TO  SC05-BANGO
      *    患者番号
           MOVE  MF101-PTNUM         TO  SC05-PTNUM
      *
      *    性別
           IF      MF101-SEX       =   "1"
               MOVE    "◯"        TO    SC05-SEX(1)
           ELSE
               MOVE    "◯"        TO    SC05-SEX(2)
           END-IF
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY2-EDTYMD1(1:1)
               WHEN  "M"
                   MOVE  "○"  TO  SC05-NENGO (1)
               WHEN  "T"
                   MOVE  "○"  TO  SC05-NENGO (2)
               WHEN  "S"
                   MOVE  "○"  TO  SC05-NENGO (3)
               WHEN  "H"
                   MOVE  "○"  TO  SC05-NENGO (4)
           END-EVALUATE
           MOVE    LNK-DAY2-EDTYMD1(2:8)   TO  SC05-BIRTHDAY 
      *
      *    特記事項
           IF  MF101-TYOKI  =  1
               MOVE  "０２長"  TO  SC05-TOKKI
           END-IF
           IF  MF101-TYOKI  =  2
               MOVE  "１６長２"    TO  SC05-TOKKI
           END-IF
      *
      *    氏名
           MOVE    MF101-NAME          TO  SC05-NAME
           MOVE    MF101-KANANAME      TO  SC05-KANANAME
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理(入院用)
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    保険者番号
           MOVE  MF101-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           IF     MF101-HKNNUM  =  "001"
             MOVE  WRK-OUT-INPUT(1:8) TO  SC05-HKNJANUM(9:8)
           ELSE
             MOVE  WRK-OUT-INPUT      TO  SC05-HKNJANUM
           END-IF
      *
      *    負担者番号、受給者番号設定
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
      *    負担者番号
               MOVE    MF101-EKOHFTNJANUM(IDX) TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC05-FTNJANUM(IDX)
      *
      *    受給者番号
               MOVE    MF101-EKOHJKYSNUM(IDX)  TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO  SC05-JKYSNUM(IDX)
           END-PERFORM
      *
      *
      *    点数、日数等
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
      *
      *    点数
               MOVE    MF101-TOTALTEN(IDX) TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC05-TENSU(IDX)
      *
      *    日数
               MOVE    MF101-JNISSU(IDX)   TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC05-NISSU(IDX)
      *
      *    一部負担金
               MOVE    MF101-FTNMONEY(IDX) TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC05-KOHFTN(IDX)
      *
               IF  MF101-NYUGAIKBN  =  "1"
               IF  (MF101-EKOHNUM(1)  =  SPACE
                    AND  IDX  =  2)
               OR  (MF101-EKOHNUM(1)  NOT =  SPACE
                    AND  IDX  =  3)
      *    食事日数
                 MOVE    MF101-SHOKUJINISSU(IDX)
                                           TO  WRK-Z29
                 MOVE    WRK-Z29           TO  SC05-SNISSU(IDX)
      *    食事療養費
                 MOVE    MF101-SHOKUJIRYOYOHI(IDX)
                                           TO  WRK-Z79
                 MOVE    WRK-Z79           TO  SC05-SKINGK(IDX)
      *    食事標準負担額
                 MOVE    MF101-SHOKUJIFTN(IDX)
                                           TO  WRK-Z79
                 MOVE    WRK-Z79           TO  SC05-SFTNGK(IDX)
               ELSE
      *    食事日数
                 MOVE    MF101-SHOKUJINISSU(IDX)
                                           TO  WRK-Z2
                 MOVE    WRK-Z2            TO  SC05-SNISSU(IDX)
      *
      *    食事療養費
                 MOVE    MF101-SHOKUJIRYOYOHI(IDX)
                                           TO  WRK-Z7
                 MOVE    WRK-Z7            TO  SC05-SKINGK(IDX)
      *
      *    食事標準負担額
                 MOVE    MF101-SHOKUJIFTN(IDX)
                                           TO  WRK-Z7
                 MOVE    WRK-Z7            TO  SC05-SFTNGK(IDX)
               END-IF
               END-IF
      *
           END-PERFORM 
      *
      *    傷病名編集処理
           INITIALIZE  WRK-TAIHI-TBL
           PERFORM 3201-BYOMEI-HEN-SEC
           MOVE    WRK-TAIHI-TBL            TO  SC05-BYOMEI-TBL
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理(入院用)
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
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
           MOVE    WRK-PRTID           TO  SPRT-PRTID
           MOVE    MF101-PTID          TO  SPRT-PTID
           MOVE    "複写式レセプト"
                                    TO  SPRT-TITLE
           MOVE    SC05             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       3201-BYOMEI-HEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    ZERO                TO  CNT-TENKI1
                                           CNT-TENKI2
                                           CNT-TENKI3
           MOVE    SPACE               TO  WRK-BYOMEI1       
      *
           INITIALIZE                     RECE031-REC
           MOVE    MF101-HOSPNUM      TO  RECE031-HOSPNUM
           MOVE    MF101-SRYYM        TO  RECE031-SRYYM
           MOVE    MF101-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    MF101-PTID         TO  RECE031-PTID
           MOVE    MF101-SRYKA        TO  RECE031-RECEKA
           MOVE    MF101-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    MF101-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    MF101-HKNJANUM-KEY TO  RECE031-HKNJANUM
           MOVE    MF101-HOJOKBN-KEY  TO  RECE031-HOJOKBN
           MOVE    MF101-TEKSTYMD     TO  RECE031-TEKSTYMD
      *
           START   RECE031-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE031
               NOT INVALID
                   PERFORM 990-RECE031-NEXT-SEC
           END-START
           DISPLAY "フラグ="      FLG-RECE031
           DISPLAY "診療年月1="   MF101-SRYYM
           DISPLAY "入外区分1="   MF101-NYUGAIKBN
           DISPLAY "患者ID1="     MF101-PTID
           DISPLAY "保険者番号1=" MF101-HKNJANUM-KEY
           DISPLAY "レセプト科1=" MF101-SRYKA
           DISPLAY "摘要開始日1=" MF101-TEKSTYMD
           DISPLAY "提出先1="     MF101-TEISYUTUSAKI
           DISPLAY "レセ種別1="   MF101-RECESYUBETU
           DISPLAY "補助区分1="   MF101-HOJOKBN-KEY
           DISPLAY "---------------------------"
           DISPLAY "診療年月2="   RECE031-SRYYM
           DISPLAY "入外区分2="   RECE031-NYUGAIKBN
           DISPLAY "患者ID2="     RECE031-PTID
           DISPLAY "保険者番号2=" RECE031-HKNJANUM
           DISPLAY "レセプト科2=" RECE031-RECEKA
           DISPLAY "摘要開始日2=" RECE031-TEKSTYMD
           DISPLAY "提出先2="     RECE031-TEISYUTUSAKI
           DISPLAY "レセ種別2="   RECE031-RECESYUBETU
           DISPLAY "補助区分2="   RECE031-HOJOKBN
           DISPLAY "---------------------------"
      *
      *
           PERFORM
               UNTIL  ( FLG-RECE031          =   1                    )
               OR     ( MF101-HOSPNUM    NOT =   RECE031-HOSPNUM      )
               OR     ( MF101-SRYYM      NOT =   RECE031-SRYYM        )
               OR     ( MF101-NYUGAIKBN  NOT =   RECE031-NYUGAIKBN    )
               OR     ( MF101-PTID       NOT =   RECE031-PTID         )
               OR     ( MF101-SRYKA      NOT =   RECE031-RECEKA       )
               OR     ( MF101-TEISYUTUSAKI
                                         NOT =   RECE031-TEISYUTUSAKI )
               OR     ( MF101-RECESYUBETU
                                         NOT =   RECE031-RECESYUBETU  )
               OR     ( MF101-HKNJANUM-KEY
                                         NOT =   RECE031-HKNJANUM     )
               OR     ( MF101-HOJOKBN-KEY
                                         NOT =   RECE031-HOJOKBN      )
               OR     ( MF101-TEKSTYMD   NOT =   RECE031-TEKSTYMD     )
      *    
                   DISPLAY "病名編集開始"
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
           END-PERFORM
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
                   MOVE    1             TO  FLG-TENKI
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    ZERO          TO  FLG-TENKI
               END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           DISPLAY "SYS2005-SYUBYOHENFLG =" SYS-2005-SYUBYOHENFLG
                   "主病名フラグ=" WRK-BO-SYUBYOFLG
           IF    ( SYS-2005-SYUBYOHENFLG   =   "3" )
           AND   ( WRK-BO-SYUBYOFLG        =   "1" )
               IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    WRK-KUGIRI3 
                           TO WRK-TAIHI-BYOMEI(WRK-LINEBYO)(2:52)
               ELSE
                   IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF 
                   MOVE    1           TO  FLG-KUGIRI
                   PERFORM 34110-KUGIRI-LINE-SEC
                   MOVE    ZERO        TO  FLG-KUGIRI   
               END-IF
           END-IF
      *
      *    ３番までは番号、年月日を印字する
           MOVE  WRK-LINE       TO    IDX2
           ADD   1              TO    IDX2
           PERFORM  VARYING  IDX1  FROM  IDX2  BY  1
                    UNTIL    IDX1  >    3
             ADD     1               TO  WRK-LINEBYO
             MOVE  IDX1              TO  WRK-Z1
             INITIALIZE              ORCSKANACHKAREA
             MOVE    "2"             TO  KANACHK-SYORI 
             MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
             MOVE    WRK-Z1          TO  KANACHK-MAE-INPUT (2:1)
             MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-TAIHI-BYOMEI(WRK-LINEBYO)
                                           WRK-TAIHI-SRYYMD(WRK-LINEBYO)
             END-IF
             MOVE  "　　年　　月　　日"  
                                   TO  WRK-TAIHI-SRYYMD(WRK-LINEBYO)(7:)
           END-PERFORM
      *
      *
           .
       3201-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       3210-BYOMEI-SET-SEC  SECTION.
      *
      *    連結表示のとき
           IF      SYS-2005-BYOMEIHENKBN    =   "0"
      *        前のデータと同一主病名フラグ・診療開始日・転帰区分
               IF  ( RECE031-SYUBYOFLG    =   WRK-BO-SYUBYOFLG )  AND
                   ( RECE031-SRYYMD       =   WRK-BO-SRYYMD    )  AND
                   ( RECE031-TENKIKBN     =   WRK-BO-TENKIKBN  )  AND 
                   ( RECE031-TENKIYMD     =   WRK-BO-TENKIYMD  )   
      *
      *            病名をつなげると何桁になるかを計算（，＋病名）
                   IF  WRK-CLM           =   ZERO
                       MOVE    WRK-CLM   TO  WRK-WCLM
                   ELSE
                       COMPUTE WRK-WCLM  =   WRK-CLM   +   2
                   END-IF
                   COMPUTE     WRK-WCLM  =
                              ( RECE031-BYOMEIMOJI  *   2 )
                                                    +   WRK-WCLM
      *
                   EVALUATE    TRUE
      *                傷病名欄に編集
                       WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                            前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
      *                適用欄に編集
                       WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                        前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
                   END-EVALUATE
               ELSE
      *            主病名フラグ・診療開始日・転帰区分 が異なる場合
                   IF      WRK-BYO-OLD        =   LOW-VALUE
                       CONTINUE
                   ELSE
                       IF  WRK-LINEBYO       <  WRK-BYOMEI-LINE
                           IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           END-IF
                       ELSE
                           IF  WRK-BYOMEI2   NOT =   SPACE
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI 
                           END-IF
                       END-IF
                   END-IF    
               END-IF
           ELSE
                IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
                    IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                        PERFORM 3221-BYOMEI1-OUT-SEC
                    END-IF
                ELSE
                    IF  WRK-BYOMEI2   NOT =   SPACE
                        MOVE    1             TO  FLG-TENKI
                        PERFORM 3222-BYOMEI2-OUT-SEC
                        MOVE    ZERO          TO  FLG-TENKI
                    END-IF
                END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           PERFORM 3211-SYUBYOFLG-HENSYU-SEC
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM         =   WRK-CLM   +   1
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE031-BYOMEIMOJI   *   2
           MOVE    RECE031-BYOMEI      TO  WRK-BYOMEI
      *
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE
                       ELSE
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE    
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI(45:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *     
           MOVE    RECE031-SYUBYOFLG     TO  WRK-BO-SYUBYOFLG
           MOVE    RECE031-SRYYMD        TO  WRK-BO-SRYYMD
           MOVE    RECE031-TENKIKBN      TO  WRK-BO-TENKIKBN
           MOVE    RECE031-TENKIYMD      TO  WRK-BO-TENKIYMD
      *
           PERFORM 990-RECE031-NEXT-SEC
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *      主病名と副病名の区切り線の編集
      ****************************************************************
       3211-SYUBYOFLG-HENSYU-SEC         SECTION.
      *
      *    主病名編集フラグが区切り有のときのみ処理を行う
           IF      SYS-2005-SYUBYOHENFLG   =   "1" OR  "3"
      *        主病名との区切り線（副傷病名があるときのみ）     
               IF      WRK-BYO-OLD             =   LOW-VALUE
                   CONTINUE
               ELSE
                   IF    ( FLG-RECE031             =   ZERO            )
                   AND   ( RECE031-SYUBYOFLG   NOT =   WRK-BO-SYUBYOFLG)
                   AND   ( WRK-BO-SYUBYOFLG        =   "1"             )
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           ADD     1           TO  WRK-LINEBYO
                           MOVE    WRK-KUGIRI3 TO
                                  WRK-TAIHI-BYOMEI(WRK-LINEBYO)(2:52)
                       ELSE
                           IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           MOVE    1           TO  FLG-KUGIRI
                           PERFORM 34110-KUGIRI-LINE-SEC
                           MOVE    ZERO        TO  FLG-KUGIRI   
                       END-IF
                   END-IF
               END-IF
           END-IF        
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *     傷病名１出力処理
      *      WRK-CONTINUE = 1 の場合、２行編集の２行目
      ****************************************************************
       3221-BYOMEI1-OUT-SEC        SECTION.
      *
      *    傷病名欄の行カウント
           ADD     1               TO  WRK-LINEBYO
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
               MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
               MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI1-NO 
               END-IF    
      *        診療開始日
               MOVE    WRK-BYOMEI1-NO      TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-TAIHI-SRYYMD
                                                    (WRK-LINEBYO)(7:)
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   ADD     1               TO  CNT-TENKI1
                                               CNT-TENKI2
                                               CNT-TENKI3
               ELSE
                   MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
                   MOVE    WRK-SDD         TO  WRK-Z2
                   ADD     1               TO  CNT-TENKI1
                                               CNT-TENKI2
                                               CNT-TENKI3
                   EVALUATE    WRK-BO-TENKIKBN
                   WHEN    "1"
                       MOVE    CNT-TENKI1 TO  WRK-Z2
                       STRING  "(" WRK-Z2 ")" DELIMITED BY SIZE
                                   INTO    WRK-TAIHI-TENKI1(WRK-LINEBYO)
                       END-STRING
                   WHEN    "2"
                       MOVE    CNT-TENKI2 TO  WRK-Z2
                       STRING  "(" WRK-Z2 ")" DELIMITED BY SIZE
                                   INTO    WRK-TAIHI-TENKI2(WRK-LINEBYO)
                       END-STRING
                   WHEN    "3"
                       MOVE    CNT-TENKI3 TO  WRK-Z2
                       STRING  "(" WRK-Z2 ")" DELIMITED BY SIZE
                                   INTO    WRK-TAIHI-TENKI3(WRK-LINEBYO)
                       END-STRING
                   END-EVALUATE
               END-IF
           END-IF
      *    病名
           MOVE    WRK-BYOMEI1     TO  WRK-TAIHI-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名２出力処理
      *      WRK-CONTINUE = 0 の場合、２行編集の１行目
      ****************************************************************
       3222-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     1               TO  WRK-LINETEK
      
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               IF  WRK-LINE            <   10
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               ELSE    
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (1:2)  TO  KANACHK-MAE-INPUT (2:2)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (4:1)
               END-IF
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI2-NO 
               END-IF    
           END-IF    
      *
      *    診療開始日・転帰を印字するスペースが有る場合
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               IF  WRK-BYOMEI2-ELSE    =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           ELSE
               IF  WRK-BYOMEI2R-ELSE   =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           END-IF    
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    WRK-BYOMEI2 TO  WRK-TAIHI-TEKIYO (WRK-LINETEK)
      *
      *    最後の病名に印字するスペースがなかったとき 
           IF       FLG-TENKI              =   1     
      *        診療年月日・転帰　編集処理
               MOVE    SPACE           TO  WRK-BYOMEI2
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
      *        適用欄の行カウント
               IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
               MOVE    WRK-BYOMEI2 TO  WRK-TAIHI-TEKIYO  (WRK-LINETEK)

           END-IF
      *
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日・転帰　編集処理
      *****************************************************************
       32221-SRYYMD-TENKI-HEN-SEC    SECTION.
      *
      *    転帰日を編集しないとき
           IF      SYS-2005-TENKIHENKBN    =   ZERO
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2-TENKI
               END-EVALUATE
           ELSE
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2R-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2R-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2R-TENKI
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                               TO  WRK-BYOMEI2R-TENKIYMD
               END-IF                                
           END-IF                                     
      *
           .
       32221-SRYYMD-TENKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           EVALUATE    FLG-KUGIRI
           WHEN    1
               MOVE    WRK-KUGIRI1 TO  WRK-TAIHI-TEKIYO  (WRK-LINETEK)
           END-EVALUATE
      *
           .
       34110-KUGIRI-LINE-EXT.
           EXIT.
      *
      *
      ******************************************************************
      **    改ページ処理
      ******************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *
      *    帳票印刷処理
           MOVE    WRK-TAIHI-TBL            TO  SC05-BYOMEI-TBL
           PERFORM 330-PRINT-OUT-SEC
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HEAD-HEN-SEC
           INITIALIZE  WRK-TAIHI-TBL
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      **
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR  TO  RECEERR-REC
             WRITE RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE                       RECE031-FILE
      *
      *
           CLOSE  MF101-FILE
           MOVE   CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU4105 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU4105 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU4105 END ***"
      *
           IF  CNT-MF101  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF101-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE031-NEXT-SEC           SECTION.
      *
           READ    RECE031-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE031
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE031
                   IF      RECE031-TENKIKBN     =   "8"
                       GO  TO  990-RECE031-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE031-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF      RECE02-KOHNUM (1)  =  "181"
               OR      RECE02-KOHNUM (2)  =  "181"
               OR      RECE02-KOHNUM (3)  =  "181"
               OR      RECE02-KOHNUM (4)  =  "181"
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF   
      *
      *        個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO                TO  FLG-ERR
                           MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                         END-IF
                     END-PERFORM 
                   END-IF
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY              TO  RECE10-KEY
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_seikyu"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE10
                   INITIALIZE                  RECE10-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
               INITIALIZE                  RECE10-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_seikyu"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key6"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
               MOVE    "key6"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key6"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       930-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE  "tbl_syskanri"          TO  MCP-TABLE
               MOVE  "key10"                 TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"             USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
               IF  MCP-RC  =  ZERO
                   MOVE  ZERO  TO  FLG-SYSKANRI
               ELSE
                   MOVE  1  TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE                   TO  SYS-1900-REC
           INITIALIZE                          SYS-1900-REC
           MOVE    "1900"                  TO  SYS-1900-KANRICD
           MOVE    "*"                     TO  SYS-1900-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-1900-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-STYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-EDYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1901-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1902-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
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
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
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
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
