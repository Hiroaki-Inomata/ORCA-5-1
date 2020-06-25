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
       PROGRAM-ID.             SOKATU3810.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・診療報酬請求書
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/04/14    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    有村    05/05/06    医療機関情報取得条件修正
      *  01.00.02    内田    06/09/17    食事日数の修正
      *  01.00.03    内田    06/11/21    食事がない場合でも生活療養費を印字
      *  01.00.04    内田    06/12/20    在総診が印字されない場合の修正
      *  01.00.05    内田    07/01/31    食事療養の印字判定修正
      *  01.00.06    内田    07/07/04    長期上位を長期へ集計する
      *  01.00.07    内田    07/09/02    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.08    内田    07/10/21    一般等の７０歳未満の
      *                                  一部負担金欄は印字しない
      *  01.00.09    長屋    08/04/29    帳票様式に伴う修正
      *  01.00.10    内田    08/05/04    国組の年齢判定修正
      *  01.00.11    内田    08/05/09    食事桁数修正
      *                                  診療年月の表示修正
      *  01.00.12    内田    08/06/20    ３月以前の返戻分を診療年月毎でなく
      *                                  まとめて印字する
      *  01.00.13    吉川    08/09/12    SPA-AREA初期化
      *  01.00.14    吉川    10/05/13    オンライン返戻対応
      *  01.00.15    吉川    11/01/12    特別療養費国保対応
      *  01.00.16    吉川    12/04/27    在宅はまとめて集計するように修正
      *  01.00.17    吉川    12/05/21    一般等の７０歳未満の
      *                                  一部負担金欄は印字する
      *  01.00.18    吉川    13/02/07    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.19    吉川    14/01/15    プログラムオプション対応
      *  01.00.20    吉川    14/05/14    前期高齢者２割対応
      *  01.00.21    吉川    14/08/06    返戻時、再印刷画面のタイトル修正
      *  01.00.22    吉川    14/10/27    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.23    吉川    15/12/10    医療機関編集情報への開設者名追加対応
      *  01.00.24    門間    17/05/15    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.25    萩野    18/03/01    SITEKBN変更（動作に変更なし）
      *  01.00.26    門間    18/09/06    不要コメントを削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル
           SELECT  SKT3810TMP-FILE ASSIGN    SKT3810TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT3810TMP-KEY
                                   FILE    STATUS  IS  STS-SKT3810.
      *    請求書一時ファイル
           SELECT  SKT3811TMP-FILE ASSIGN    SKT3811TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT3811TMP-KEY
                                   FILE    STATUS  IS  STS-SKT3811.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル
       FD  SKT3810TMP-FILE.
           COPY    "CPSKT3810TMP.INC".
      *
      *    編集済請求書一時ファイル
       FD  SKT3811TMP-FILE.
           COPY    "CPSKT3810TMP.INC"  REPLACING  //SKT3810TMP//
                                       BY         //SKT3811TMP//.
      *
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求書一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT3810TP//.
      *    請求書一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT3811TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT3820.INC".
           COPY    "SKT3830.INC".
           COPY    "SKT3825.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT3810         PIC X(02).
           03  STS-SKT3811         PIC X(02).
           03  STS-YUSEN           PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT3810TMP      PIC 9(01).
           03  FLG-SKT3811TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KOH             PIC 9(01).
           03  FLG-ZAIISO          PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-KOH1            PIC 9(01).
           03  FLG-KOH2            PIC 9(01).
           03  FLG-KUMIAI          PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(01).
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
      *
      *##  パラメタ領域（地方公費仕様）
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMWH2        PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-PAYKBN          PIC 9(02).
           03  WRK-RATE            PIC 9(03).
           03  WRK-CNT             PIC 9(02).
           03  WRK-ITBFTN          PIC 9(10).
           03  WRK-KOHNUM          PIC X(03).
      *
           03  WRK-TENHYO          PIC 99.
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13             PIC Z,ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2           PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-YUSEN           PIC X(06).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
           03  CNT-KOH             PIC 9(02).
      *
      *     TMPファイル編集エリア
       01  WRK-TMP-REC.
           03  WRK-TMP-KEY.
               05  WRK-TMP-SYUTURYOKU          PIC  9(01).
               05  WRK-TMP-HKNJANUM            PIC  X(06).
               05  WRK-TMP-KYURATE             PIC  X(03).
               05  WRK-TMP-ZAISOU              PIC  X(01).
           03  WRK-TMP-PAGE-FLG1               PIC  9(01).
           03  WRK-TMP-PAGE-FLG2               PIC  9(01).
           03  WRK-TMP-SEIKYU-TBL     OCCURS  9.
               05  WRK-TMP-NYUGAI-TBL  OCCURS  2.
                   07  WRK-TMP-KENSU            PIC  9(06).
                   07  WRK-TMP-NISSU            PIC  9(06).
                   07  WRK-TMP-TENSU            PIC  9(10).
                   07  WRK-TMP-ITBFTN           PIC  9(10).
               05  WRK-TMP-S-KENSU              PIC  9(06).
               05  WRK-TMP-S-NISSU              PIC  9(06).
               05  WRK-TMP-S-KINGAKU            PIC  9(10).
               05  WRK-TMP-S-FTNGAKU            PIC  9(10).
           03  WRK-TMP-RSEIKYU-TBL     OCCURS  2.
               05  WRK-TMP-RNYUGAI-TBL  OCCURS  2.
                   07  WRK-TMP-RKENSU            PIC  9(06).
                   07  WRK-TMP-RNISSU            PIC  9(06).
                   07  WRK-TMP-RTENSU            PIC  9(10).
                   07  WRK-TMP-RITBFTN           PIC  9(10).
               05  WRK-TMP-S-RKENSU              PIC  9(06).
               05  WRK-TMP-S-RNISSU              PIC  9(06).
               05  WRK-TMP-S-RKINGAKU            PIC  9(10).
               05  WRK-TMP-S-RFTNGAKU            PIC  9(10).
           03  WRK-TMP-KSEIKYU-TBL     OCCURS  2.
               05  WRK-TMP-KNYUGAI-TBL  OCCURS  2.
                   07  WRK-TMP-KKENSU            PIC  9(06).
                   07  WRK-TMP-KNISSU            PIC  9(06).
                   07  WRK-TMP-KTENSU            PIC  9(10).
                   07  WRK-TMP-KITBFTN           PIC  9(10).
               05  WRK-TMP-S-KKENSU              PIC  9(06).
               05  WRK-TMP-S-KNISSU              PIC  9(06).
               05  WRK-TMP-S-KKINGAKU            PIC  9(10).
               05  WRK-TMP-S-KFTNGAKU            PIC  9(10).
           03  WRK-TMP-TSEIKYU-TBL.
               05  WRK-TMP-TNYUGAI-TBL  OCCURS  2.
                   07  WRK-TMP-TKENSU            PIC  9(06).
                   07  WRK-TMP-TNISSU            PIC  9(06).
                   07  WRK-TMP-TTENSU            PIC  9(10).
                   07  WRK-TMP-TITBFTN           PIC  9(10).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
           03  WRK-ROUJIN-FLG          PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *　　保険組合せ情報
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
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
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
           COPY    "CPRCF010.INC".
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
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
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
           INITIALIZE                  WRK-TMP-REC
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *###（地方公費仕様）
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-TOKUBETU
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU3810"    TO  JOB-PGID
           MOVE    "国保請求書"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT3810"           TO  SKT3810TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT3810TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT3810TP-HOSPNUM
           MOVE    "SKT3811"           TO  SKT3811TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT3811TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT3811TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT3810TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  SKT3811TP-BASENAME      TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (3)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (4)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT3810TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  SKT3811TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (4)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
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
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           OPEN    OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
           END-EVALUATE
      *
           PERFORM 900-SEIKYU-SELECT-SEC
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
           MOVE    "SOKATU3810"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    優先保険者番号
           MOVE    "YUSEN"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:6)        TO  WRK-YUSEN
           DISPLAY "優先保険者番号="  WRK-YUSEN
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1    >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT3810TMP-FILE
           CLOSE            SKT3810TMP-FILE
           OPEN     I-O     SKT3810TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT3810TMP-FILE
      *
           IF       FLG-UPD   =       1
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了    処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      CNT-RECE10          =   ZERO
                   CONTINUE
           ELSE
                   IF      FLG-UPD     =   1
                           PERFORM     310-SEIKYU-UPD-SEC
                   END-IF
           END-IF
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU3810 IN  "  CNT-RECE10
           DISPLAY "*** SOKATU3810 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA

           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC              SECTION.
      *
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT3810TMP-REC
      *    保険者番号
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  SKT3810TMP-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  SKT3810TMP-HKNJANUM(1:6)
           END-IF
      *    平成２０年３月以前老人保健判定
           IF (RECE10-SRYYM  <  WRK-CONS-SRYYM-200804)
             AND  (RECE10-RJNHKNNUM  NOT =  SPACE)
               MOVE  RECE10-SKYYM          TO  SKT3810TMP-SRYYM
               MOVE  1                     TO  WRK-ROUJIN-FLG
           END-IF
      *    組合
           IF  SKT3810TMP-HKNJANUM(3:1) = "3"
             IF  RECE10-RJNHKNNUM  =  SPACE
               IF   RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
               AND  RECE10-FLG (1)  =  1
                 CONTINUE
               ELSE
                 IF  (RECE10-AGE  >  2)  AND  (RECE10-AGE  <  70)
                   IF  RECE10-KYURATE  NOT =  7
                       MOVE   RECE10-KYURATE  TO  SKT3810TMP-KYURATE
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *    印刷順設定
           IF    WRK-YUSEN  =  SKT3810TMP-HKNJANUM
             MOVE          1   TO    SKT3810TMP-SYUTURYOKU
           ELSE
             MOVE          2   TO    SKT3810TMP-SYUTURYOKU
           END-IF
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
           PERFORM  210-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " 診療年月＝"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " レセ種別＝"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    入外区分
           IF  RECE10-NYUGAIKBN  =  1
             MOVE  1  TO  IDX2
           ELSE
             MOVE  2  TO  IDX2
           END-IF
      *    老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
             EVALUATE  RECE10-KYURATE
               WHEN  9       MOVE  1  TO  IDX1
               WHEN  OTHER   MOVE  2  TO  IDX1
             END-EVALUATE
      *
             PERFORM  2102-RJN-SYUKEI-SEC
      *
             MOVE     1     TO     SKT3810TMP-PAGE-FLG2
           ELSE
      *    一般／退職者
             IF  RECE10-HKNNUM  =  "060" OR "068"
      *    年齢チェック
               IF  RECE10-AGE  >=  70
                 EVALUATE  RECE10-KYURATE
                   WHEN  9       MOVE  1  TO  IDX1
                   WHEN  8
                     IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                         MOVE    1    TO    IDX1
                     ELSE
                         MOVE    2    TO    IDX1
                     END-IF
                   WHEN  OTHER   MOVE  2  TO  IDX1
                 END-EVALUATE
               ELSE
                IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                AND  (RECE10-FLG (1)   =  1)
                    MOVE  4     TO  IDX1
                ELSE
      *    ３歳未満チェック
                 IF  RECE10-AGE > 2
      *    割合チェック
                   MOVE  3        TO    IDX1
                 ELSE
                   MOVE  4        TO    IDX1
                 END-IF
                END-IF
               END-IF
             ELSE
      *    年齢チェック
               IF  RECE10-HONKZKKBN  =  "1"
                 IF  RECE10-AGE  >  69
                   IF  RECE10-KYURATE  =  9
      *   70以上９割は退職本人へ
                     MOVE  5   TO  IDX1
                   ELSE
      *   70以上７割は退職被扶養者へ
                     MOVE  6   TO  IDX1
                   END-IF
                 ELSE
                   MOVE    5   TO  IDX1
                 END-IF
               ELSE
                IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                AND  (RECE10-FLG (1)   =  1)
                     MOVE  7     TO  IDX1
                ELSE
                 IF  RECE10-AGE  >  2
      *     家族も７０歳判定を行う
                   IF  RECE10-AGE > 69
                     IF  RECE10-KYURATE  =  9
                       MOVE  5   TO  IDX1
                     ELSE
                       MOVE  6   TO  IDX1
                     END-IF
                   ELSE
                    MOVE  6   TO  IDX1
                   END-IF
                 ELSE
                   MOVE  7   TO  IDX1
                 END-IF
                END-IF
               END-IF
             END-IF
      *
             PERFORM  2102-SYUKEI-SEC
      *
             MOVE     1     TO     SKT3810TMP-PAGE-FLG1
           END-IF
      *
      *
      *    公費併用チェック
           IF  RECE10-KOHNUM(1)  NOT =  SPACE
             PERFORM  220-KOH-SYUKEI-SEC
             MOVE     1     TO     SKT3810TMP-PAGE-FLG2
           ELSE
             IF  RECE10-ETC-KOHNUM(1) NOT = SPACE
               PERFORM 220-TYK-SYUKEI-SEC
               MOVE     1     TO     SKT3810TMP-PAGE-FLG2
             END-IF
           END-IF
      *
           IF  FLG-SKT3810TMP  =  1
               WRITE    SKT3810TMP-REC
           ELSE
               REWRITE  SKT3810TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・集計処理
      *****************************************************************
       2102-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                   TO  SKT3810TMP-KENSU (IDX1 IDX2)
      *    療養の給付：日数
           ADD   RECE10-JNISSU(1)    TO  SKT3810TMP-NISSU (IDX1 IDX2)
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT3810TMP-TENSU (IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT3810TMP-ITBFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養：件数
             ADD   1                 TO  SKT3810TMP-S-KENSU (IDX1)
           END-IF
      *    食事療養：日数
           ADD   RECE10-SHOKUJINISSU(1) TO  SKT3810TMP-S-NISSU (IDX1)
      *    食事療養：金額
           ADD   RECE10-SHOKUJIRYOYOHI (1)
                                     TO  SKT3810TMP-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
           ADD   RECE10-SHOKUJIFTN (1)
                                     TO  SKT3810TMP-S-FTNGAKU(IDX1)
      *
           MOVE    1               TO  FLG-UPD
           .
       2102-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人保健・集計処理
      *****************************************************************
       2102-RJN-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                   TO  SKT3810TMP-RKENSU (IDX1 IDX2)
      *    療養の給付：日数
           ADD   RECE10-JNISSU(1)    TO  SKT3810TMP-RNISSU (IDX1 IDX2)
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT3810TMP-RTENSU (IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT3810TMP-RITBFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養：件数
             ADD   1                 TO  SKT3810TMP-S-RKENSU (IDX1)
           END-IF
      *    食事療養：日数
           ADD  RECE10-SHOKUJINISSU(1) TO SKT3810TMP-S-RNISSU (IDX1)
      *    食事療養：金額
           ADD   RECE10-SHOKUJIRYOYOHI (1)
                                     TO  SKT3810TMP-S-RKINGAKU(IDX1)
      *    食事療養：標準負担額
           ADD   RECE10-SHOKUJIFTN (1)
                                     TO  SKT3810TMP-S-RFTNGAKU(IDX1)
      *
           MOVE    1               TO  FLG-UPD
           .
       2102-RJN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
      *
           MOVE    ZERO    TO    FLG-PASS
      *
           IF  RECE10-RJNHKNNUM  = SPACE
               MOVE  1   TO    IDX1
           ELSE
               MOVE  2   TO    IDX1
           END-IF
      *
      *    療養の給付は公費に該当する項目を淡々と集計する
      *    全国公費を持っている３併用の場合は、地方公費分の日数、点数は
      *    ０になるので(分点している時は別)ダブルカウントにならない
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR  RECE10-KOHNUM(IDX3) =   SPACE
      *    公費番号毎に件数をカウントする
               ADD     1               TO  SKT3810TMP-KKENSU(IDX1 IDX2)

      *    療養の給付：日数
               ADD     RECE10-JNISSU(IDX3 + 1)
                                       TO  SKT3810TMP-KNISSU(IDX1 IDX2)
      *
      *    療養の給付：点数
               ADD     RECE10-TOTALTEN(IDX3 + 1)
                                       TO  SKT3810TMP-KTENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
               ADD     RECE10-FTNMONEY(IDX3 + 1)
                                       TO  SKT3810TMP-KITBFTN(IDX1 IDX2)
      *    食事日数で判断に変更
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  > 0
                   ADD     1           TO  SKT3810TMP-S-KKENSU(IDX1)
               END-IF
      *    食事療養：金額
               ADD     RECE10-SHOKUJIRYOYOHI (IDX3 + 1)
                                       TO  SKT3810TMP-S-KKINGAKU(IDX1)
      *    食事療養：日数
               ADD     RECE10-SHOKUJINISSU(IDX3 + 1)
                                       TO  SKT3810TMP-S-KNISSU (IDX1)
      *    食事療養：標準負担額
               ADD     RECE10-SHOKUJIFTN (IDX3 + 1)
                                       TO  SKT3810TMP-S-KFTNGAKU(IDX1)
      *
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-TYK-SYUKEI-SEC        SECTION.
      *
           IF  RECE10-RJNHKNNUM  = SPACE
               MOVE  1   TO    IDX1
           ELSE
               MOVE  2   TO    IDX1
           END-IF
      *
      *    療養の給付：件数
           IF  RECE10-ETC-KOHNUM(1) = "972" OR "974"
           OR  RECE10-ETC-KOHNUM(2) = "972" OR "974"
           OR  RECE10-ETC-KOHNUM(3) = "972" OR "974"
           OR  RECE10-ETC-KOHNUM(4) = "972" OR "974"
             ADD  1                 TO  SKT3810TMP-TKENSU(IDX1)
      *    療養の給付：日数
             ADD   RECE10-JNISSU(1) TO  SKT3810TMP-TNISSU (IDX1)
      *
      *    "９７２"は請求管理テーブルの記載外公費に落ちるが、
      *    相対する記載外合計点数にはデータが落ちて来ないので、
      *    とりあえず合計点１をセットする。
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(1)
                                    TO  SKT3810TMP-TTENSU(IDX1)
           END-IF
      *
      *
           .
       220-TYK-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
006900*    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           PERFORM  20021-HENSYU-SEC
      *
           OPEN     INPUT       SKT3811TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     1           TO       WRK-CNT
      *
      *    編集済一時ファイル読込
           PERFORM  901-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    一般国保・退職国保分
      *    帳票編集<見出し>処理
      *
      *    データの無い場合は印刷しない
             IF     SKT3810TMP-PAGE-FLG1  =  1
               PERFORM 311-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 321-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 331-PRINT-OUT-SEC
             END-IF
      *
      *    老人保険・公費分
      *    データの無い場合は印刷しない
             IF     SKT3810TMP-PAGE-FLG2  =  1
               IF SKT3810TMP-SRYYM  = ZERO
      *    帳票編集<見出し>処理
                   PERFORM 313-HEAD-HEN-SEC
      *    帳票編集<明細>処理
                   PERFORM 323-BODY-HEN-SEC
      *    帳票印刷処理
                   PERFORM 333-PRINT-OUT-SEC
                ELSE
      *         IF SKT3810TMP-SRYYM NOT = ZERO
      *    月遅れ老人帳票編集<見出し>処理
                   PERFORM 314-HEAD-HEN-SEC
      *    月遅れ老人帳票編集<明細>処理
                   PERFORM 324-BODY-HEN-SEC
      *    月遅れ老人帳票印刷処理
                   PERFORM 334-PRINT-OUT-SEC
               END-IF
             END-IF
      *
      *    編集済一時ファイル読み込み
             PERFORM  901-TMP-READ-SEC
             MOVE  1  TO  WRK-CNT
           END-PERFORM
      *
           CLOSE    SKT3811TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル編集・出力処理
      *****************************************************************
       20021-HENSYU-SEC                SECTION.
      *
           OPEN     INPUT       SKT3810TMP-FILE
           OPEN     OUTPUT      SKT3811TMP-FILE
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
           MOVE     SKT3810TMP-REC    TO    WRK-TMP-REC
           MOVE     ZERO              TO    FLG-KUMIAI
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
             DISPLAY "チェック保険者番号1="  SKT3810TMP-HKNJANUM
             DISPLAY "チェック保険者番号2="  WRK-TMP-HKNJANUM
             DISPLAY "チェック3="  SKT3810TMP-ZAISOU
             DISPLAY "チェック4="  SKT3810TMP-KENSU(3 1)
             DISPLAY "チェック5="  SKT3810TMP-KENSU(3 2)
             DISPLAY "チェック6="  WRK-TMP-KYURATE
             DISPLAY "フラグ="  FLG-PRINT
      *    保険者番号が同じで、７割のデータがない場合は編集する
             IF     SKT3810TMP-HKNJANUM    =  WRK-TMP-HKNJANUM  AND
                    WRK-TMP-KENSU(3 1)     =  ZERO              AND
                    WRK-TMP-KENSU(3 2)     =  ZERO              AND
                    WRK-TMP-KYURATE        =  SPACE             AND
                    FLG-PRINT              =  ZERO
      *    ７割のデータがない場合は法定外給付割合分を加算
               PERFORM  20022-HENSYU-SEC
               INITIALIZE                     SKT3811TMP-REC
               MOVE     WRK-TMP-REC        TO SKT3811TMP-REC
               WRITE    SKT3811TMP-REC
               MOVE     1                  TO FLG-KUMIAI
             ELSE
               INITIALIZE                  SKT3811TMP-REC
               MOVE     WRK-TMP-REC        TO SKT3811TMP-REC
               WRITE    SKT3811TMP-REC
               MOVE     SKT3810TMP-REC     TO WRK-TMP-REC
             END-IF
      *    一時ファイル読込
             PERFORM    900-TMP-READ-SEC
      *
             IF         FLG-KUMIAI    =    1     AND
                        FLG-PRINT     =    ZERO
               MOVE     ZERO               TO FLG-KUMIAI
               INITIALIZE                     WRK-TMP-REC
               MOVE     SKT3810TMP-REC     TO WRK-TMP-REC
               PERFORM  900-TMP-READ-SEC
             END-IF
      *
           END-PERFORM
      *
           IF     FLG-KUMIAI     =     ZERO
             INITIALIZE                         SKT3811TMP-REC
             MOVE         WRK-TMP-REC        TO SKT3811TMP-REC
             WRITE        SKT3811TMP-REC
           END-IF
      *
           CLOSE    SKT3810TMP-FILE
           CLOSE    SKT3811TMP-FILE
      *
           .
       20021-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    法定内給付割合データへの加算処理
      *****************************************************************
       20022-HENSYU-SEC                SECTION.
      *
      *    ７割のデータがない場合は法定外給付割合を設定
               MOVE    SKT3810TMP-KYURATE     TO WRK-TMP-KYURATE
      *    印刷判定フラグを編集
               IF      WRK-TMP-PAGE-FLG1      =  ZERO
                 MOVE  SKT3810TMP-PAGE-FLG1   TO WRK-TMP-PAGE-FLG1
               END-IF
               IF      WRK-TMP-PAGE-FLG2      =  ZERO
                 MOVE  SKT3810TMP-PAGE-FLG2   TO WRK-TMP-PAGE-FLG2
               END-IF
      *    法定外給付割合のデータを法定内給付割合のデータに加算
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
                 ADD     SKT3810TMP-KENSU(3 IDX2)  TO
                                           WRK-TMP-KENSU(3 IDX2)
                 ADD     SKT3810TMP-NISSU(3 IDX2)  TO
                                           WRK-TMP-NISSU(3 IDX2)
                 ADD     SKT3810TMP-TENSU(3 IDX2)  TO
                                           WRK-TMP-TENSU(3 IDX2)
      *
                 ADD     SKT3810TMP-KKENSU(1 IDX2)  TO
                                           WRK-TMP-KKENSU(1 IDX2)
                 ADD     SKT3810TMP-KNISSU(1 IDX2)  TO
                                           WRK-TMP-KNISSU(1 IDX2)
                 ADD     SKT3810TMP-KTENSU(1 IDX2)  TO
                                           WRK-TMP-KTENSU(1 IDX2)
                 ADD     SKT3810TMP-KITBFTN(1 IDX2) TO
                                           WRK-TMP-KITBFTN(1 IDX2)
               END-PERFORM
      *
               ADD     SKT3810TMP-S-KENSU(3)  TO
                                         WRK-TMP-S-KENSU(3)
               ADD     SKT3810TMP-S-NISSU(3)  TO
                                         WRK-TMP-S-NISSU(3)
               ADD     SKT3810TMP-S-KINGAKU(3)  TO
                                         WRK-TMP-S-KINGAKU(3)
               ADD     SKT3810TMP-S-FTNGAKU(3)  TO
                                         WRK-TMP-S-FTNGAKU(3)
      *
               ADD     SKT3810TMP-S-KKENSU(1)  TO
                                         WRK-TMP-S-KKENSU(1)
               ADD     SKT3810TMP-S-KNISSU(1)  TO
                                         WRK-TMP-S-KNISSU(1)
               ADD     SKT3810TMP-S-KKINGAKU(1)  TO
                                         WRK-TMP-S-KKINGAKU(1)
               ADD     SKT3810TMP-S-KFTNGAKU(1)  TO
                                         WRK-TMP-S-KFTNGAKU(1)
      *
               ADD     SKT3810TMP-TKENSU(1)  TO
                                         WRK-TMP-TKENSU(1)
               ADD     SKT3810TMP-TNISSU(1)  TO
                                         WRK-TMP-TNISSU(1)
               ADD     SKT3810TMP-TTENSU(1)  TO
                                         WRK-TMP-TTENSU(1)
      *
           .
       20022-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（一般）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT3820
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3820-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT3820-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT3820-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3820-HOSPCD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3820-HOSPADRS
           INSPECT SKT3820-HOSPADRS      REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3820-HOSPNAME
           INSPECT SKT3820-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT3820-KAISETU
      *
      *    保険者番号
           MOVE    SKT3810TMP-HKNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3820-HKNJANUM
      *
      *    保険者名
           PERFORM  920-HKNJAINF-SET-SEC
           STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                   "  "                DELIMITED BY SIZE
                   "殿"                DELIMITED BY SIZE
                   INTO                SKT3820-HKNJANAME
           END-STRING
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"       TO  SKT3820-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（一般）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     7
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT3810TMP-KENSU (IDX1 IDX2)   TO  WRK-Z6
               MOVE  WRK-Z6   TO  SKT3820-KENSU(IDX1 IDX2)
      *    療養の給付：日数
               MOVE  SKT3810TMP-NISSU (IDX1 IDX2)   TO  WRK-Z6
               MOVE  WRK-Z6   TO  SKT3820-NISSU(IDX1 IDX2)
      *    療養の給付：点数
               MOVE  SKT3810TMP-TENSU (IDX1 IDX2)   TO  WRK-Z10
               MOVE  WRK-Z10   TO  SKT3820-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3810TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3820-ITBFTN(IDX1 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3810TMP-S-KENSU (IDX1)        TO  WRK-Z6
             MOVE  WRK-Z6   TO  SKT3820-S-KENSU(IDX1)
      *    食事療養：日数
             MOVE  SKT3810TMP-S-NISSU (IDX1)        TO  WRK-Z6
             MOVE  WRK-Z6   TO  SKT3820-S-NISSU(IDX1)
      *    食事療養：金額
             MOVE  SKT3810TMP-S-KINGAKU (IDX1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3820-S-MONEY(IDX1)
      *    食事療養：標準負担額
             MOVE  SKT3810TMP-S-FTNGAKU (IDX1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3820-S-FTNGAKU(IDX1)
           END-PERFORM
      *
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（一般）
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           MOVE    "SKT3820.red"       TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3820             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
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
       331-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（老人・公費）
      *****************************************************************
       313-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT3830
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3830-NO
                                             SKT3825-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT3830-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT3830-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3830-HOSPCD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3830-HOSPADRS
           INSPECT SKT3830-HOSPADRS      REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3830-HOSPNAME
           INSPECT SKT3830-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT3830-KAISETU
      *
      *    保険者番号
           MOVE    SKT3810TMP-HKNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3830-HKNJANUM
      *
      *    保険者名
           PERFORM  920-HKNJAINF-SET-SEC
           STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                   "  "                DELIMITED BY SIZE
                   "殿"                DELIMITED BY SIZE
                   INTO                SKT3830-HKNJANAME
           END-STRING
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"       TO  SKT3830-TOKUBETU
           END-IF
      *
           .
       313-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（老人・公費）
      *****************************************************************
       323-BODY-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     2
      *    公費編集
      *    療養の給付：件数
               MOVE  SKT3810TMP-KKENSU (1 IDX1)   TO  WRK-Z6
               MOVE  WRK-Z6   TO  SKT3830-KKENSU(IDX1)
      *    療養の給付：日数
               MOVE  SKT3810TMP-KNISSU (1 IDX1)   TO  WRK-Z6
               MOVE  WRK-Z6   TO  SKT3830-KNISSU(IDX1)
      *    療養の給付：点数
               MOVE  SKT3810TMP-KTENSU (1 IDX1)   TO  WRK-Z10
               MOVE  WRK-Z10   TO  SKT3830-KTENSU(IDX1)
      *    療養の給付：一部負担金
               MOVE  SKT3810TMP-KITBFTN (1 IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3830-KITBFTN(IDX1)
           END-PERFORM
      *    食事療養：件数
             MOVE  SKT3810TMP-S-KKENSU (1)        TO  WRK-Z6
             MOVE  WRK-Z6   TO  SKT3830-S-KKENSU
      *    食事療養：件数
             MOVE  SKT3810TMP-S-KNISSU (1)        TO  WRK-Z6
             MOVE  WRK-Z6   TO  SKT3830-S-KNISSU
      *    食事療養：金額
             MOVE  SKT3810TMP-S-KKINGAKU (1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3830-S-KMONEY
      *    食事療養：標準負担額
             MOVE  SKT3810TMP-S-KFTNGAKU (1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3830-S-KFTNGAKU
      *
           .
       323-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（公費）
      *****************************************************************
       333-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           MOVE    "SKT3830.red"       TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3830             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
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
       333-PRINT-OUT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（老人・公費）
      *****************************************************************
       314-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT3825
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3825-NO
      *
      *    診療年月
           MOVE    SKT3810TMP-SRYYM      TO  WRK-SYMD (1:6)
           MOVE    "01"                  TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)     TO  WRK-SRYYMWH2
           MOVE    WRK-SRYYMWH2          TO  SKT3825-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT3825-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3825-HOSPCD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3825-ADRS
           INSPECT SKT3825-ADRS          REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3825-HOSPNAME
           INSPECT SKT3825-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT3825-KAISETUNAME
      *
      *    保険者番号
           MOVE    "  "                    TO  WRK-MAE-INPUT(1:2)
           MOVE    SKT3810TMP-HKNJANUM     TO  WRK-MAE-INPUT(3:)
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3825-HKNJANUM
      *
      *    保険者名
           PERFORM  920-HKNJAINF-SET-SEC
           STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                   "  "                DELIMITED BY SIZE
                   "殿"                DELIMITED BY SIZE
                   INTO                SKT3825-HKNJANAME
           END-STRING
      *
           .
       313-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（老人・公費）
      *****************************************************************
       324-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     2
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *
      *    老人編集
      *    療養の給付：件数
               MOVE  SKT3810TMP-RKENSU (IDX1 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3825-KENSU(IDX1 IDX2)
      *    療養の給付：日数
               MOVE  SKT3810TMP-RNISSU (IDX1 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3825-NISSU(IDX1 IDX2)
      *    療養の給付：点数
               MOVE  SKT3810TMP-RTENSU (IDX1 IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3825-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3810TMP-RITBFTN (IDX1 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3825-ITBFTN(IDX1 IDX2)
             END-PERFORM
      *
      ***  食事療養：老人編集
      *    食事療養：件数
             MOVE  SKT3810TMP-S-RKENSU (IDX1)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3825-S-KENSU(IDX1)
      *    食事療養：日数
             MOVE  SKT3810TMP-S-RNISSU (IDX1)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3825-S-NISSU(IDX1)
      *    食事療養：金額
             MOVE  SKT3810TMP-S-RKINGAKU (IDX1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3825-S-KINGK(IDX1)
      *    食事療養：標準負担額
             MOVE  SKT3810TMP-S-RFTNGAKU (IDX1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3825-S-FTNGK(IDX1)
      *
      *    公費編集
      *    療養の給付：件数
             MOVE  SKT3810TMP-KKENSU (2 IDX1)   TO  WRK-Z5
             MOVE  WRK-Z5    TO  SKT3825-KKENSU(IDX1)
      *    療養の給付：日数
             MOVE  SKT3810TMP-KNISSU (2 IDX1)   TO  WRK-Z5
             MOVE  WRK-Z5    TO  SKT3825-KNISSU(IDX1)
      *    療養の給付：点数
             MOVE  SKT3810TMP-KTENSU (2 IDX1)   TO  WRK-Z9
             MOVE  WRK-Z9    TO  SKT3825-KTENSU(IDX1)
      *    療養の給付：一部負担金
             MOVE  SKT3810TMP-KITBFTN (2 IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9    TO  SKT3825-KITBFTN(IDX1)
           END-PERFORM
      *
      ***  食事療養：公費編集
      *    食事療養：件数
           MOVE  SKT3810TMP-S-KKENSU (2)        TO  WRK-Z5
           MOVE  WRK-Z5   TO  SKT3825-S-KKENSU
      *    食事療養：件数
           MOVE  SKT3810TMP-S-KNISSU (2)        TO  WRK-Z5
           MOVE  WRK-Z5   TO  SKT3825-S-KNISSU
      *    食事療養：金額
           MOVE  SKT3810TMP-S-KKINGAKU (2)      TO  WRK-Z9
           MOVE  WRK-Z9   TO  SKT3825-S-KKINGK
      *    食事療養：標準負担額
           MOVE  SKT3810TMP-S-KFTNGAKU (2)      TO  WRK-Z9
           MOVE  WRK-Z9   TO  SKT3825-S-KFTNGK
           .
       323-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（公費）
      *****************************************************************
       334-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           MOVE    "SKT3825.red"       TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE    "国保請求書"    TO  SPRT-TITLE
             WHEN  "2"
               MOVE    "国保請求書［返戻分］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3825             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
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
       333-PRINT-OUT-EXT.
           EXIT.
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
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
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
      *   請求管理ＤＢ更新処理
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
      *
      *###（地方公費仕様）
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC               TO  MCPDATA-REC
                   MOVE    "DBUPDATE"               TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY
                       MOVE  "*** SOKATU3810 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY
                   MOVE  "*** SOKATU3810 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                   MOVE    RECE10-KEY      TO  WRK-RECEERR(33:)
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
             MOVE  WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                          RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000              TO  RECE10-SRYYM
             MOVE  999999              TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
               ADD     1                   TO  CNT-RECE10
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT3810TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT3810TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT3810TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT3810TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           DISPLAY "*****保険者番号*****" SKT3810TMP-HKNJANUM
           DISPLAY "*****給付割合*****"   SKT3810TMP-KYURATE
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集済一時ファイル読込   04/01/22 INS
      *****************************************************************
       901-TMP-READ-SEC                SECTION.
      *
           READ  SKT3811TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT  AT  END
                   INITIALIZE     SKT3810TMP-REC
                   MOVE     SKT3811TMP-REC     TO     SKT3810TMP-REC
           END-READ
           .
       901-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
      *###（地方公費仕様）
      *
           MOVE    "DBSELECT"                     TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                      TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"                    TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                   TO  HKNJA-HOSPNUM
           MOVE  SKT3810TMP-HKNJANUM           TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                  TO  MCPDATA-REC
           MOVE  "DBSELECT"                    TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"                 TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
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
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
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
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
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
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
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
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
