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
       PROGRAM-ID.             A00000M700.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 月計表（診療費請求明細）
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/03/01    @@@@-半田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/07  コメント履歴削除
      *                                     初診、再診、その他の判定条件を変更
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/04/18  処理速度の改善、コメント履歴削除
      *  01.00.03    @@@@-田中    11/10/14  CSVファイル作成機能追加
      *  01.00.04    @@@@-田中    12/02/02  公務災害（健保準拠）時の点数単価不具合を修正
      *  01.00.05    @@@@-門間    12/07/03  初診、再診、その他の判定条件を再度変更
      *  01.00.06    @@@@-門間    12/12/06  公費単独の場合、項目「自費負担額」に
      *                                     公費１計算負担金を参照するように修正
      *  01.00.07    @@@@-門間    13/09/30  プレビュー画面の表示修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    15/09/30  ソート処理を修正
      *  02.00.03    門間         16/02/01  一時ディレクトリ対応(エラーファイル)
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000M700.INC".
           COPY    "A00000M7001.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  DEBUG-SW            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK2         PIC 9(01).
           03  FLG-ACCTEND         PIC 9(01).
           03  FLG-TOTAL           PIC 9(01).
      *
      *  件数用フラグ領域
           03  FLG-KENSUU.
             05  FLG-HKN           PIC 9(01).
             05  FLG-SYUEKI        PIC 9(01).
             05  FLG-SEIKYU        PIC 9(01).
             05  FLG-RYOSYU        PIC 9(01).
             05  FLG-JIHI-G.
               07  FLG-JIHI        PIC 9(01)  OCCURS  5.
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-S               PIC 9(05).
           03  IDX-N               PIC 9(05).
           03  IDX-MAX             PIC 9(05).
           03  IDX7G.
              05  IDX7             PIC 9(02).
           03  IDX8                PIC 9(04).
      *
           03  IDX1G.
              05  IDX1             PIC 9(02).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(02).
           03  IDX5                PIC 9(02).
           03  IDX6                PIC 9(03).
           03  IDX10               PIC 9(02).
           03  IDX11               PIC 9(02).
           03  JIHIIDX             PIC 9(04).
           03  IDXP                PIC 9(05).
           03  IDX-ST              PIC 9(03).
           03  IDX-ED              PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PRTYM      PIC X(06).
           03  WRK-PARA-PRTSYMD    PIC X(08).
           03  WRK-PARA-PRTLYMD    PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-PARA-PRTYMWH      PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
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
            05 WRK-BIRTHY            PIC 9(04).
            05 WRK-BIRTHM            PIC 9(02).
            05 WRK-BIRTHD            PIC 9(02).
           03  WRK-NENREI            PIC 9(02).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-PTID              PIC 9(10).
           03  WRK-FTNRATE           PIC 9(01)V9(02).
           03  WRK-KYFRATE           PIC 9(01)V9(02).
      *
           03  WRK-SYUHKN-KBN        PIC 9(01).
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
           03  WRK-JIHI4             PIC 9(08).
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-4001-KSNMONEY     PIC 9(08).
           03  WRK-4001-KOMUFLG      PIC X(01).
           03  WRK-4001-KOMUTENTANKA PIC 9(02).
           03  WRK-TENTNK            PIC 9(02)V9(01).
           03  WRK-RSI-MONEY1        PIC S9(09)V9(01).
           03  WRK-RSI-MONEY2        PIC S9(09)V9(01).
      *
      *    入院用印刷診療科件数
           03  WRK-PRNSRKA-CNT       PIC 9(03).
      *    外来用印刷診療科件数
           03  WRK-PRGSRKA-CNT       PIC 9(03).
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9-5              PIC ZZZZ9.
           03  WRK-Z9-9              PIC ZZZZZZZZ9.
           03  WRK-Z9-10             PIC ZZZZZZZZZ9.
           03  WRK-S9-10             PIC ---------9.
           03  WRK-SZ-10             PIC ---------Z.
      *
           03  WRK-FORMID            PIC X(30).
           03  WRK-FORMID1           PIC X(30).
           03  WRK-DENPNUM           PIC 9(07).
      *
       01  CONST-AREA.
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
      *
      *    診療科別加算エリア
       01  SUM-AREA.
           05  SUM-JYUSHINKEN       PIC 9(05)  OCCURS  32.
           05  SUM-SHKNKEN          PIC 9(05)  OCCURS  32.
           05  SUM-RHKNKEN          PIC 9(05)  OCCURS  32.
           05  SUM-OHKNKEN          PIC 9(05)  OCCURS  32.
           05  SUM-HKNTEN           PIC 9(09)  OCCURS  32.
           05  SUM-TEN-TBL          OCCURS  16.
               07  SUM-TEN          PIC 9(09)  OCCURS  32.
           05  SUM-HKNFTN           PIC S9(10) OCCURS  32.
           05  SUM-JIHIFTN          PIC S9(10) OCCURS  32.
           05  SUM-SEIKYU           PIC S9(10) OCCURS  32.
           05  SUM-RYOSYU           PIC S9(10) OCCURS  32.
      *
      *    入外区分別全診療科加算エリア
       01  SUM-AREA-GK.
           05  SUM-JYUSHINKEN-GK    PIC 9(05)  OCCURS  32.
           05  SUM-SHKNKEN-GK       PIC 9(05)  OCCURS  32.
           05  SUM-RHKNKEN-GK       PIC 9(05)  OCCURS  32.
           05  SUM-OHKNKEN-GK       PIC 9(05)  OCCURS  32.
           05  SUM-HKNTEN-GK        PIC 9(09)  OCCURS  32.
           05  SUM-TEN-TBL-GK       OCCURS  16.
               07  SUM-TEN-GK       PIC 9(09)  OCCURS  32.
           05  SUM-HKNFTN-GK        PIC S9(10) OCCURS  32.
           05  SUM-JIHIFTN-GK       PIC S9(10) OCCURS  32.
           05  SUM-SEIKYU-GK        PIC S9(10) OCCURS  32.
           05  SUM-RYOSYU-GK        PIC S9(10) OCCURS  32.
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  50000.
               05  WRK-SMEI-HOSPNUM       PIC 9(02).
               05  WRK-SMEI-SORT-KEY.
                 07  WRK-SMEI-NYUGAIKBN   PIC X(01).
                 07  WRK-SMEI-SRYKA       PIC X(02).
                 07  WRK-SMEI-NYUHEN-YMD  PIC X(08).
                 07  WRK-SMEI-PTID        PIC 9(10).
                 07  WRK-SMEI-DENPNUM     PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM  PIC 9(02).
               05  WRK-SMEI-SKYMONEY      PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
      *
           03  WRK-SV-SMEI-TBL            PIC X(46).
           03  WRK-SV-SMEI-DENPNUM        PIC 9(07).
      *
       01   WRK-JYUSHIN-AREA.
         03   WRK-JYUSHIN-PTID-TBL        OCCURS 10000.
           05   WRK-JYUSHIN-PTID          PIC  9(10).
           05   WRK-NYUHEN-YMD            PIC  X(02).
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
      *    CSV
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)  VALUE 100.
           03  WRK-ITEM                PIC X(100)  VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-KENSU1     PIC X(50)  VALUE SPACE.
               07  CSV-HEAD-KENSU2     PIC X(50)  VALUE SPACE.
               07  CSV-HEAD-KENSU3     PIC X(50)  VALUE SPACE.
               07  CSV-HEAD-KENSU4     PIC X(50)  VALUE SPACE.
               07  CSV-HEAD-HKNTEN     PIC X(50)  VALUE "保険点数".
               07  CSV-HEAD-SSU        PIC X(50)  VALUE "初・再診料".
               07  CSV-HEAD-SDO        PIC X(50)  VALUE "医学管理等".
               07  CSV-HEAD-ZTK        PIC X(50)  VALUE "在宅医療".
               07  CSV-HEAD-TYK        PIC X(50)  VALUE "投薬".
               07  CSV-HEAD-CSY        PIC X(50)  VALUE "注射".
               07  CSV-HEAD-SYC        PIC X(50)  VALUE "処置".
               07  CSV-HEAD-SJT        PIC X(50)  VALUE "手術".
               07  CSV-HEAD-MSI        PIC X(50)  VALUE "麻酔".
               07  CSV-HEAD-KNS        PIC X(50)  VALUE "検査".
               07  CSV-HEAD-GZU        PIC X(50)  VALUE "画像診断".
               07  CSV-HEAD-ETC        PIC X(50)
                                       VALUE "リハビリテーション".
               07  CSV-HEAD-SSN        PIC X(50)
                                       VALUE "精神科専門療法".
               07  CSV-HEAD-HOU        PIC X(50)  VALUE "放射線治療".
               07  CSV-HEAD-BYR        PIC X(50)  VALUE "病理診断".
               07  CSV-HEAD-NYN        PIC X(50)  VALUE "入院料".
               07  CSV-HEAD-RYO        PIC X(50)  VALUE "療養担当手当".
               07  CSV-HEAD-HKNFTN     PIC X(50)  VALUE "保険負担額".
               07  CSV-HEAD-JIHIFTN    PIC X(50)  VALUE "自費負担額".
               07  CSV-HEAD-SKYMONEY   PIC X(50)  VALUE "窓口請求額".
               07  CSV-HEAD-RSYMONEY   PIC X(50)  VALUE "窓口領収額".
             05  CSV-HEAD-R            REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM       PIC X(50) OCCURS 25.
             05  CSV-HEAD-MAX          PIC 9(05) VALUE 25.
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
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災医療機関情報
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
      *    診療行為マスタ
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
      *
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
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SUM-AREA-GK
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  WRK-JYUSHIN-AREA
           INITIALIZE                  RECEERR
      *
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
                                               RECEERR
                                               WRK-PARA-PRTYM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000M700"    TO  JOB-PGID
           MOVE    "月計表（診療費請求明細）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    初診／再診判定サブ初期処理
           INITIALIZE                      SS003-AREA
           MOVE    ZERO            TO      SS003-SYORIKBN
           CALL    "ORCSS003"      USING   SS003-AREA
                                           SPA-AREA
      *
           INITIALIZE                     SGETTEMP-AREA
           MOVE    RECEERR                TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"          USING  SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)  TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
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
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH
      *
      *    対象年月最初日設定
           MOVE    WRK-PARA-PRTYM      TO  WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                TO  WRK-PARA-PRTSYMD(7:2)
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM      TO  WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO  WRK-PARA-PRTLYMD
      *
      *    対象年月日編集
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PARA-PRTYMWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE  WRK-PARA-PRTSYMD      TO  SYS-1001-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO  SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPNUM     TO  WRK-PARA-HOSPNUM
           ELSE
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    労災医療機関情報
           MOVE    SPACE                   TO  SYS-4001-REC
           INITIALIZE                          SYS-4001-REC
           MOVE    "4001"                  TO  SYS-4001-KANRICD
           MOVE    "*"                     TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD        TO  WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD          TO  SYS-4001-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD          TO  SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC            TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE    MCPDATA-REC    TO  SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5       TO  WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12         TO  WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10         TO  WRK-4001-TENTANKA
             END-EVALUATE
      *
             IF  SYS-4001-KOMUKBN  =  "2"
      *    公務災害(健保基準)時の点数単価設定
               MOVE    SYS-4001-KOMUTENTANKA-X TO  WRK-4001-KOMUTENTANKA
      *    公務災害区分が健保基準の場合、１を格納
               MOVE    "1"        TO  WRK-4001-KOMUFLG
             END-IF
           ELSE
             MOVE  10   TO  WRK-4001-TENTANKA
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM  UNTIL   FLG-END     =   1
      *    収納情報集計処理
               PERFORM 320-BODY-HEN-SEC
           END-PERFORM
      *
      *    帳票編集<診療科合計ページ>印刷処理
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                    IF  WRK-PRNSRKA-CNT  >   1
                        PERFORM 330-TERM-KA-PRINT-SEC
                    END-IF
               WHEN "2"
                    IF  WRK-PRGSRKA-CNT  >   1
                        PERFORM 330-TERM-KA-PRINT-SEC
                    END-IF
           END-EVALUATE
      *
      *    帳票編集<合計ページ>印刷処理
           PERFORM 330-TERM-GK-PRINT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選定処理
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE    ZERO        TO  IDX-S
      *
           PERFORM  910-DBSELECT-SEC
           PERFORM  UNTIL   FLG-SYUMEI     =   1
             PERFORM   900-SYUNOU-READ-SEC
      *
             IF      FLG-SYUNOU             =  ZERO
               AND   SYU-DENPJTIKBN     NOT =  "3"
               AND ((SMEI-JOUTAIKBN     NOT =  "8"
               AND   SMEI-JOUTAIKBN     NOT =  "3")
               OR    SMEI-SKYMONEY      NOT =  0
               OR    SMEI-NYUHEN-MONEY  NOT =  0)
      *
               ADD   1                  TO  IDX-S
               MOVE  SMEI-HOSPNUM       TO  WRK-SMEI-HOSPNUM(IDX-S)
               MOVE  SMEI-NYUGAIKBN     TO  WRK-SMEI-NYUGAIKBN(IDX-S)
               MOVE  SMEI-SRYKA         TO  WRK-SMEI-SRYKA(IDX-S)
               MOVE  SMEI-PTID          TO  WRK-SMEI-PTID(IDX-S)
               MOVE  SMEI-DENPNUM       TO  WRK-SMEI-DENPNUM(IDX-S)
               MOVE  SMEI-DENPEDANUM    TO  WRK-SMEI-DENPEDANUM(IDX-S)
               MOVE  SMEI-SKYMONEY      TO  WRK-SMEI-SKYMONEY(IDX-S)
               MOVE  SMEI-NYUHEN-MONEY  TO  WRK-SMEI-NYUHEN-MONEY(IDX-S)
               MOVE  SMEI-NYUHEN-YMD    TO  WRK-SMEI-NYUHEN-YMD(IDX-S)
             END-IF
             PERFORM   910-DBFETCH-SEC
           END-PERFORM
           PERFORM   910-DBCLOSE-SEC
      *
      *    ソート処理
           MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX     >    1
            MOVE  1  TO  FLG-GET
            MOVE  ZERO    TO   FLG-END
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
              PERFORM  UNTIL IDX-N  >  IDX-MAX
                IF WRK-SMEI-SORT-KEY(IDX-S) > WRK-SMEI-SORT-KEY(IDX-N)
                  MOVE  WRK-SMEI-TBL(IDX-N)  TO  WRK-SV-SMEI-TBL
                  MOVE  WRK-SMEI-TBL(IDX-S)  TO  WRK-SMEI-TBL(IDX-N)
                  MOVE  WRK-SV-SMEI-TBL      TO  WRK-SMEI-TBL(IDX-S)
                END-IF
                ADD 1  TO  IDX-N
              END-PERFORM
              ADD 1  TO  IDX-S
              COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
           ELSE
             IF   IDX-MAX     =     ZERO
               MOVE  ZERO    TO   FLG-GET
               MOVE  1            TO   FLG-END
             ELSE
               MOVE  1            TO  FLG-GET
               MOVE  ZERO    TO   FLG-END
             END-IF
           END-IF
      *
           MOVE 1     TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票集計処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                        TO  CNT-LINE
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
      *    入院診療科数カウントアップ
                    MOVE   1    TO WRK-PRNSRKA-CNT
              WHEN "2"
      *    外来診療科数カウントアップ
                    MOVE   1    TO WRK-PRGSRKA-CNT
           END-EVALUATE
      *
           PERFORM   UNTIL   FLG-END     =   1
      *    件数用フラグの初期化
            INITIALIZE   FLG-KENSUU
      *
      *    収納テーブル再読み込み
            PERFORM 901-SYUNOU-READ-SEC
            IF    FLG-SYUNOU = ZERO
      *
      *    入外区分ブレイクチェック処理
            IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *    合計カウンターに加算
                PERFORM  VARYING     IDX1   FROM 1   BY 1
                         UNTIL       IDX1   >    32
                         ADD  SUM-SHKNKEN(IDX1)
                                     TO     SUM-SHKNKEN-GK(IDX1)
                         ADD  SUM-RHKNKEN(IDX1)
                                     TO     SUM-RHKNKEN-GK(IDX1)
                         ADD  SUM-OHKNKEN(IDX1)
                                     TO     SUM-OHKNKEN-GK(IDX1)
                         ADD  SUM-HKNTEN(IDX1)
                                     TO     SUM-HKNTEN-GK(IDX1)
                         PERFORM VARYING    IDX2   FROM  1  BY 1
                                 UNTIL      IDX2   >    16
                         ADD SUM-TEN(IDX2 IDX1)
                                     TO     SUM-TEN-GK(IDX2 IDX1)
                         END-PERFORM
                         ADD  SUM-HKNFTN(IDX1)
                                     TO     SUM-HKNFTN-GK(IDX1)
                         ADD  SUM-JIHIFTN(IDX1)
                                     TO     SUM-JIHIFTN-GK(IDX1)
                         ADD  SUM-SEIKYU(IDX1)
                                     TO     SUM-SEIKYU-GK(IDX1)
                         ADD  SUM-RYOSYU(IDX1)
                                     TO     SUM-RYOSYU-GK(IDX1)
                END-PERFORM
      *
      *    入院の印刷診療科数が２科以上の場合に科別集計頁を印字する。
                IF   WRK-NYUGAIKBN   =      "1"
                 AND WRK-PRNSRKA-CNT >      1
                     PERFORM 330-TERM-KA-PRINT-SEC
                END-IF
      *
                PERFORM 330-TERM-GK-PRINT-SEC
                INITIALIZE                  SUM-AREA-GK
                INITIALIZE                  SUM-AREA
      *
      *    ブレイクキー項目設定
                MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *
      *    PTIDブレイクキー項目の設定
                INITIALIZE  WRK-PTID
      *
                EVALUATE WRK-NYUGAIKBN
                    WHEN "1"
      *    入院診療科数カウントアップ
                         ADD   1    TO WRK-PRNSRKA-CNT
                    WHEN "2"
      *    外来診療科数カウントアップ
                         ADD   1    TO WRK-PRGSRKA-CNT
                END-EVALUATE
      *
            END-IF
      *
      *    診療科ブレイクチェック処理
            IF  WRK-SMEI-SRYKA(IDX-S)  NOT = WRK-SRYKA
                PERFORM 330-TERM-KA-PRINT-SEC
      *    合計カウンターに加算
                PERFORM  VARYING     IDX1   FROM 1   BY 1
                         UNTIL       IDX1   >    32
                         ADD  SUM-SHKNKEN(IDX1)
                                     TO     SUM-SHKNKEN-GK(IDX1)
                         ADD  SUM-RHKNKEN(IDX1)
                                     TO     SUM-RHKNKEN-GK(IDX1)
                         ADD  SUM-OHKNKEN(IDX1)
                                     TO     SUM-OHKNKEN-GK(IDX1)
                         ADD  SUM-HKNTEN(IDX1)
                                     TO     SUM-HKNTEN-GK(IDX1)
                         PERFORM VARYING    IDX2   FROM  1  BY 1
                                 UNTIL      IDX2   >    16
                         ADD SUM-TEN(IDX2 IDX1)
                                     TO     SUM-TEN-GK(IDX2 IDX1)
                         END-PERFORM
                         ADD  SUM-HKNFTN(IDX1)
                                     TO     SUM-HKNFTN-GK(IDX1)
                         ADD  SUM-JIHIFTN(IDX1)
                                     TO     SUM-JIHIFTN-GK(IDX1)
                         ADD  SUM-SEIKYU(IDX1)
                                     TO     SUM-SEIKYU-GK(IDX1)
                        ADD  SUM-RYOSYU(IDX1)
                                     TO     SUM-RYOSYU-GK(IDX1)
                END-PERFORM
                INITIALIZE                  SUM-AREA
      *
      *    ブレイクキー項目設定
                MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *
      *    PTIDブレイクキー項目の設定
                 INITIALIZE  WRK-PTID
      *
                EVALUATE WRK-NYUGAIKBN
                    WHEN "1"
      *    入院診療科数カウントアップ
                         ADD   1    TO WRK-PRNSRKA-CNT
                    WHEN "2"
      *    外来診療科数カウントアップ
                         ADD   1    TO WRK-PRGSRKA-CNT
                END-EVALUATE
      *
            END-IF
      *
      *    診療日別インデックス値の設定
            MOVE  WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2) TO  IDX1G
      *
      *    伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
             IF   WRK-SMEI-DENPEDANUM(IDX-S)   =     1
               IF  FLG-SYUNOU  = ZERO
                   PERFORM 320-BODY-HEN-SUB-SEC
      *    受診履歴集計
                IF SYU-NYUGAIKBN = "1"
                   PERFORM 320-NYUIN-CNT-SEC
                ELSE
      *    外来の場合は初診、再診別に件数を集計する。
                  PERFORM  320-GAIRAI-CNT-SEC
                END-IF
               END-IF
             END-IF
      *
      *    請求金額
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-SKYMONEY(IDX-S)
                           TO   SUM-SEIKYU(IDX1)
                                SUM-SEIKYU(32)
                   MOVE    1    TO   FLG-SEIKYU
             END-IF
      *    領収金額
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                           TO  SUM-RYOSYU(IDX1)
                               SUM-RYOSYU(32)
                   MOVE    1    TO   FLG-RYOSYU
             END-IF
      *
            END-IF
      *    収納テーブル収納テーブル再読み込み
      *
      *    配列インデックスのカウントアップ
             ADD  1     TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S   >   IDX-MAX
                MOVE    1   TO   FLG-END
             END-IF
      *
           END-PERFORM
      *
      *    合計カウンターに加算
           PERFORM  VARYING  IDX1  FROM  1  BY 1
                    UNTIL  IDX1  >  32
             ADD  SUM-SHKNKEN(IDX1)   TO  SUM-SHKNKEN-GK(IDX1)
             ADD  SUM-RHKNKEN(IDX1)   TO  SUM-RHKNKEN-GK(IDX1)
             ADD  SUM-OHKNKEN(IDX1)   TO  SUM-OHKNKEN-GK(IDX1)
             ADD  SUM-HKNTEN(IDX1)    TO  SUM-HKNTEN-GK(IDX1)
             PERFORM  VARYING  IDX2  FROM  1  BY 1
                      UNTIL  IDX2  >  16
               ADD  SUM-TEN(IDX2 IDX1)    TO  SUM-TEN-GK(IDX2 IDX1)
             END-PERFORM
             ADD  SUM-HKNFTN(IDX1)    TO  SUM-HKNFTN-GK(IDX1)
             ADD  SUM-JIHIFTN(IDX1)   TO  SUM-JIHIFTN-GK(IDX1)
             ADD  SUM-SEIKYU(IDX1)    TO  SUM-SEIKYU-GK(IDX1)
             ADD  SUM-RYOSYU(IDX1)    TO  SUM-RYOSYU-GK(IDX1)
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計カウント>サブ処理
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    患者情報読み込み
           INITIALIZE                           PTINF-REC
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTINF-HOSPNUM
           MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *    保険組合情報
           INITIALIZE                           HKNCOMBI-REC
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  COMB-HOSPNUM
           MOVE    WRK-SMEI-PTID(IDX-S)     TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
      *    患者保険情報検索
           INITIALIZE                           PTHKNINF-REC
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTHKN-HOSPNUM
           MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTHKN-PTID
           MOVE    COMB-HKNID               TO  PTHKN-HKNID
           PERFORM 900-PTHKNINF-READ-SEC
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =   1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE    10     TO  HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO  TO  SYU-TOTAL-HKNTEN
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL  IDZ  >  16
             ADD  SYU-HKNTEN(IDZ)    TO  SYU-TOTAL-HKNTEN
             ADD  SYU-HKNTEN(IDZ)    TO  SUM-TEN(IDZ IDX1)
                                         SUM-TEN(IDZ 32)
           END-PERFORM
           ADD  SYU-TOTAL-HKNTEN     TO  SUM-HKNTEN(IDX1)
                                         SUM-HKNTEN(32)
           MOVE    1                 TO  FLG-HKN
      *
      *    保険種別
           EVALUATE  SYU-SYUHKNNUM
             WHEN  SPACE
      *    公費単独
               MOVE    3            TO  WRK-SYUHKN-KBN
             WHEN  "980" THRU "989"
             WHEN  "900"  THRU  "919"
      *    自費
               MOVE    4            TO  WRK-SYUHKN-KBN
             WHEN  WRK-CONS-ROUSAI-HKNNUM
      **   労災
               MOVE    5            TO  WRK-SYUHKN-KBN
      *    患者労災情報検索
               INITIALIZE           PTRSIINF-REC
               MOVE    SYU-HOSPNUM  TO  PTRSI-HOSPNUM
               MOVE    SYU-PTID     TO  PTRSI-PTID
               MOVE    COMB-HKNID   TO  PTRSI-HKNID
               PERFORM  900-PTRSIINF-READ-SEC
             WHEN  WRK-CONS-JIBAI-HKNNUM
      *    自賠責
               MOVE    6            TO  WRK-SYUHKN-KBN
      *    患者労災情報検索
               INITIALIZE           PTRSIINF-REC
               MOVE    SYU-HOSPNUM  TO  PTRSI-HOSPNUM
               MOVE    SYU-PTID     TO  PTRSI-PTID
               MOVE    COMB-HKNID   TO  PTRSI-HKNID
               PERFORM 900-PTRSIINF-READ-SEC
      *    公害
             WHEN  WRK-CONS-KOGAI-HKNNUM
               MOVE    7            TO  WRK-SYUHKN-KBN
      *    後期高齢者
             WHEN  "039"
             WHEN  "040"
               MOVE    8            TO  WRK-SYUHKN-KBN
             WHEN  "060"
             WHEN  "067"
             WHEN  "068"
             WHEN  "069"
      *    国保
               MOVE    1            TO  WRK-SYUHKN-KBN
             WHEN  OTHER
      *    社保
               MOVE    2            TO  WRK-SYUHKN-KBN
           END-EVALUATE
      *
      *    主保険患者負担割合、主保険給付割合の算出
      *
           IF   WRK-SYUHKN-KBN  =  1
               OR  WRK-SYUHKN-KBN  =  2
      *    年齢の算出
             MOVE    SYU-SRYYMD         TO  WRK-SYSYMD
             MOVE    PTINF-BIRTHDAY     TO  WRK-BIRTHDAY
             COMPUTE  WRK-NENREI  =   WRK-SYSYY  -  WRK-BIRTHY
             IF   WRK-SYSMM  <  WRK-BIRTHM
               COMPUTE  WRK-NENREI  =  WRK-NENREI  -  1
             ELSE
              IF   WRK-SYSMM  =  WRK-BIRTHM
                  AND WRK-SYSDD  <  WRK-BIRTHD
                   COMPUTE WRK-NENREI =   WRK-NENREI - 1
              END-IF
             END-IF
      *
             IF   SYU-SYUHKNNUM  =  "060"
               EVALUATE  PTHKN-HOJOKBN
                 WHEN  ZERO
                   MOVE    ZERO   TO  WRK-FTNRATE
                 WHEN  1
                   MOVE    0.1    TO  WRK-FTNRATE
                 WHEN  2
                   MOVE    0.2    TO  WRK-FTNRATE
                 WHEN  3
                   MOVE    0.3    TO  WRK-FTNRATE
                 WHEN  OTHER
                   MOVE    0.3    TO  WRK-FTNRATE
               END-EVALUATE
             ELSE
               EVALUATE  PTHKN-HOJOKBN
                 WHEN  SPACE
                   MOVE    0.3    TO  WRK-FTNRATE
                 WHEN  8
                   MOVE    0.2    TO  WRK-FTNRATE
                 WHEN  9
                   MOVE    0.1    TO  WRK-FTNRATE
                 WHEN  OTHER
                   MOVE    ZERO   TO  WRK-FTNRATE
               END-EVALUATE
             END-IF
      *
      *    三歳未満、負担割合３割以上のチェック
             IF  WRK-NENREI  <=  2
                 AND  WRK-FTNRATE  >  0.2
               MOVE    0.2        TO  WRK-FTNRATE
             END-IF
             IF  SYU-SYUHKNNUM  =  "068"  OR  "069"
               MOVE    1          TO  WRK-FTNRATE
             END-IF
             COMPUTE WRK-KYFRATE   =  1 - WRK-FTNRATE
           END-IF
      *
      *    保険負担額
           IF   WRK-SYUHKN-KBN  =  1
               OR  WRK-SYUHKN-KBN  =  2
               OR  WRK-SYUHKN-KBN  =  5
               OR  WRK-SYUHKN-KBN  =  6
               OR  WRK-SYUHKN-KBN  =  7
               OR  WRK-SYUHKN-KBN  =  8
             EVALUATE  WRK-SYUHKN-KBN
               WHEN  1 THRU 2
               WHEN  8
      *    国保、社保の場合
                 IF   SYU-KOH-HKNNUM(1)  =  "027"
                   COMPUTE  WRK-MONEY  =  (SYU-TOTAL-HKNTEN * 10)
                                       -  SYU-KOH-COMPFTN(1)
                 ELSE
                   COMPUTE  WRK-MONEY  =  (SYU-TOTAL-HKNTEN * 10)
                                       -  SYU-SYUCOMPFTN
                 END-IF
      *
                 COMPUTE  WRK-MONEY  =  WRK-MONEY
                                     +  SYU-RYOYOHI-SKJ
                                     +  SYU-RYOYOHI-LIFE
                                     -  SYU-SKYMONEY-SKJ-KEI
                                     -  SYU-SKYMONEY-LIFE-KEI
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                 IF  HKN-RECEFTNDSPKBN  =  "1"
                   IF  WRK-MONEY(8:1)  >  4
                     MOVE    ZERO       TO  WRK-MONEY(8:1)
                     ADD  10            TO  WRK-MONEY
                   ELSE
                     MOVE    ZERO       TO  WRK-MONEY(8:1)
                   END-IF
                 END-IF
               WHEN  5
      *    労災の場合
      *    患者労災保険情報取得
                 PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
                 IF  PTRSI-HKNKBN  =  "5"
                     AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                   MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
                 ELSE
                   MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
                 END-IF
      *
                 IF  SYU-PTFTNRATE NOT =  ZERO
                   MOVE     ZERO       TO  WRK-MONEY
                 ELSE
                   COMPUTE  WRK-MONEY  =
                            (SYU-TOTAL-HKNTEN  *  WRK-TENTNK)
                   COMPUTE  WRK-MONEY  =  WRK-MONEY
                                       +  SYU-RSISHOSHIN-MONEY
                                       +  SYU-RSISAISHIN-MONEY
                                       +  SYU-RSISDO-MONEY
                                       +  SYU-RSIETC-MONEY
                                       +  SYU-RYOYOHI-SKJ
                                       +  SYU-RYOYOHI-LIFE
                 END-IF
               WHEN  6
      *    自賠の場合
                 IF  SYU-PTFTNRATE  NOT =  ZERO
                   MOVE    ZERO      TO  WRK-MONEY
                 ELSE
                   COMPUTE  WRK-MONEY  =  SYU-RSIJIBAI-SKYMONEY
                                       -  SYU-JIHI-TOTAL
                                       -  SYU-JIHI-TOTAL-TAX
                                       -  SYU-TOTAL-TGMONEY
                                       -  SYU-TOTAL-TGMONEY-TAX
                                       -  SYU-CHOSEI
                                       -  SYU-YKZ-FTN
                                       -  SYU-SKYMONEY-SKJ-JIHI-KEI
                                       -  SYU-SKYMONEY-LIFE-JIHI-KEI
                                       -  SYU-RMSAGAKU
                 END-IF
               WHEN   7
      *    公害の場合
                 COMPUTE  WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                     +  SYU-TOTAL-TGMONEY-TAX
                                     +  SYU-JIHI-TOTAL
                                     +  SYU-JIHI-TOTAL-TAX
                                     +  SYU-RMSAGAKU
                                     +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                     +  SYU-SKYMONEY-LIFE-JIHI-KEI
                 IF  SYU-SRYYMD  <  "20070401"
                   COMPUTE  WRK-JIHI4  =  WRK-JIHI4
                                       +  SYU-JIHI-TAX
                 END-IF
                 COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                     -   WRK-JIHI4
             END-EVALUATE
             ADD  WRK-MONEY          TO  SUM-HKNFTN(IDX1)
                                         SUM-HKNFTN(32)
           END-IF
      *
      *    自費負担額
           IF   WRK-SYUHKN-KBN  =  1
               OR  WRK-SYUHKN-KBN  =  2
               OR  WRK-SYUHKN-KBN  =  3
               OR  WRK-SYUHKN-KBN  =  4
               OR  WRK-SYUHKN-KBN  =  5
               OR  WRK-SYUHKN-KBN  =  6
               OR  WRK-SYUHKN-KBN  =  8
             EVALUATE  WRK-SYUHKN-KBN
               WHEN  1  THRU  2
               WHEN  8
                 IF   SYU-KOH-HKNNUM(1)  =  "027"
                   MOVE  SYU-KOH-COMPFTN(1)   TO  WRK-MONEY
                 ELSE
                   MOVE  SYU-SYUCOMPFTN   TO  WRK-MONEY
                 END-IF
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                 IF  WRK-MONEY(8:1) >   4
                     MOVE   ZERO    TO  WRK-MONEY(8:1)
                     ADD    10      TO  WRK-MONEY
                 ELSE
                     MOVE   ZERO    TO  WRK-MONEY(8:1)
                 END-IF
               WHEN 3
      *    公費単独の場合
                 MOVE    SYU-KOH-COMPFTN(1)     TO  WRK-MONEY
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                 IF  WRK-MONEY(8:1) >   4
                     MOVE   ZERO    TO  WRK-MONEY(8:1)
                     ADD    10      TO  WRK-MONEY
                 ELSE
                     MOVE   ZERO    TO  WRK-MONEY(8:1)
                 END-IF
               WHEN  4
                 MOVE    SYU-SKYMONEY      TO  WRK-MONEY
               WHEN  5
                 PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
                 IF  PTRSI-HKNKBN  =  "5"
                     AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                   MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
                 ELSE
                   MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
                 END-IF
      *
                 IF  SYU-PTFTNRATE  =  ZERO
                   COMPUTE  WRK-MONEY  =  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                                       +  SYU-RMSAGAKU
      *
                 ELSE
                   COMPUTE  WRK-MONEY  =
                           (SYU-TOTAL-HKNTEN  *  WRK-TENTNK)
                   COMPUTE  WRK-MONEY  =  WRK-MONEY
                                       +  SYU-RSISHOSHIN-MONEY
                                       +  SYU-RSISAISHIN-MONEY
                                       +  SYU-RSISDO-MONEY
                                       +  SYU-RSIETC-MONEY
                                       +  SYU-RYOYOHI-SKJ
                                       +  SYU-RYOYOHI-LIFE
                                       +  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                 END-IF
               WHEN 6
                 IF  SYU-PTFTNRATE  =  ZERO
                   COMPUTE  WRK-MONEY  =  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                                       +  SYU-RMSAGAKU
      *
                 ELSE
                   MOVE     SYU-RSIJIBAI-SKYMONEY    TO  WRK-MONEY
                 END-IF
             END-EVALUATE
      *
             IF  WRK-SYUHKN-KBN  NOT =  4  AND  5  AND  6
      *    自費,労災、自賠責以外の場合
      *    自費分の集計
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI  =  SYU-TOTAL-TGMONEY
                                   +  SYU-TOTAL-TGMONEY-TAX
                                   +  SYU-JIHI-TOTAL
                                   +  SYU-JIHI-TOTAL-TAX
                                   +  SYU-JIHI-TAX
               ELSE
                 COMPUTE  WRK-JIHI  =  SYU-TOTAL-TGMONEY
                                    +  SYU-TOTAL-TGMONEY-TAX
                                    +  SYU-JIHI-TOTAL
                                    +  SYU-JIHI-TOTAL-TAX
               END-IF
               COMPUTE  WRK-MONEY  =  WRK-MONEY
                                   +  WRK-JIHI
                                   +  SYU-SKYMONEY-SKJ-KEI
                                   +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                   +  SYU-SKYMONEY-LIFE-KEI
                                   +  SYU-SKYMONEY-LIFE-JIHI-KEI
                                   +  SYU-RMSAGAKU
             END-IF
             ADD   WRK-MONEY      TO  SUM-JIHIFTN(IDX1)
                                      SUM-JIHIFTN(32)
           END-IF
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院件数集計処理
      *****************************************************************
       320-NYUIN-CNT-SEC                    SECTION.
      *
      *    保険点数件数カウント
           IF  FLG-HKN = 1
               ADD   1  TO   SUM-RHKNKEN(IDX1)
                             SUM-RHKNKEN(32)
           END-IF
           .
       320-NYUIN-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来件数集計処理
      *****************************************************************
       320-GAIRAI-CNT-SEC      SECTION.
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
             IF  FLG-HKN = 1
               ADD    1       TO   SUM-SHKNKEN(IDX1)
                                   SUM-SHKNKEN(32)
             END-IF
      *    再診件数カウント
           WHEN    "2"
             IF  FLG-HKN  =  1
               ADD    1       TO   SUM-RHKNKEN(IDX1)
                                   SUM-RHKNKEN(32)
             END-IF
      *    その他件数カウント
           WHEN    OTHER
             IF  FLG-HKN = 1
               ADD    1       TO   SUM-OHKNKEN(IDX1)
                                   SUM-OHKNKEN(32)
             END-IF
           END-EVALUATE
      *
           MOVE     1        TO      IDX3
           PERFORM  UNTIL    IDX3    >      10000
             IF  (WRK-SMEI-PTID(IDX-S) =  WRK-JYUSHIN-PTID(IDX3))
             AND (WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                         =  WRK-NYUHEN-YMD(IDX3))
      *
                 MOVE  10000   TO      IDX3
             END-IF
             IF  WRK-JYUSHIN-PTID(IDX3)  =  ZERO
                 AND  WRK-NYUHEN-YMD(IDX3) = SPACE
                 AND  (IDX3 NOT = 10000)
      *
                 MOVE  WRK-SMEI-PTID(IDX-S)
                               TO  WRK-JYUSHIN-PTID(IDX3)
                 MOVE  WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                               TO  WRK-NYUHEN-YMD(IDX3)
                 ADD   1       TO      SUM-JYUSHINKEN-GK(IDX1)
                                       SUM-JYUSHINKEN-GK(32)
                 MOVE  10000   TO      IDX3
             END-IF
             ADD 1           TO      IDX3
           END-PERFORM
      *
           .
       320-GAIRAI-CNT-EXT.
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
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
               MOVE    SYU-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票<診療科合計>印刷処理
      *****************************************************************
      *
       330-TERM-KA-PRINT-SEC         SECTION.
      *
           INITIALIZE                       M700
           INITIALIZE                       M7001
      *
           MOVE    WRK-SYSYMDWH         TO  M700-SYSYMD
           MOVE    WRK-PARA-PRTYMWH     TO  M700-PRTYM
           MOVE    WRK-SYSYMDWH         TO  M7001-SYSYMD
           MOVE    WRK-PARA-PRTYMWH     TO  M7001-PRTYM
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"          TO  M700-NYUGAIKBN
                   MOVE "入院"          TO  M7001-NYUGAIKBN
                   MOVE  "請求件数"     TO  M700-KENSU2
              WHEN "2"
                   MOVE "外来"          TO  M700-NYUGAIKBN
                   MOVE "外来"          TO  M7001-NYUGAIKBN
                   MOVE  "その他件数"   TO  M700-KENSU3
                   MOVE  "再診件数"     TO  M700-KENSU2
                   MOVE  "初診件数"     TO  M700-KENSU1
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                       SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD     TO  WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD       TO  SYS-1005-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD       TO  SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC        TO  SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME TO  M700-SRYKA
               MOVE  SYS-1005-SRYKANAME TO  M7001-SRYKA
           ELSE
               MOVE  SPACE              TO  M700-SRYKA
               MOVE  SPACE              TO  M7001-SRYKA
           END-IF
      *
           IF  FLG-GET = ZERO
      *    対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M700-JOBMSG
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M7001-JOBMSG
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
           ELSE
      *    対象データが１件以上存在した場合
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
      *
      *    月末最終日の設定
             MOVE WRK-PARA-PRTLYMD(7:2)      TO  IDX7G
             PERFORM VARYING   IDX1    FROM  1   BY  1
                     UNTIL     IDX1    >     IDX7
      *
      *    日別
      *    保険点数、件数合計
              MOVE    SUM-HKNTEN(IDX1)   TO  WRK-Z9-9
              MOVE    WRK-Z9-9           TO  M700-GHKNTEN(IDX1)
              IF WRK-NYUGAIKBN =   "2"
                 MOVE    SUM-SHKNKEN(IDX1)   TO  WRK-Z9-5
                 MOVE    WRK-Z9-5            TO  M700-SHKNKEN(IDX1)
                 MOVE    SUM-OHKNKEN(IDX1)   TO  WRK-Z9-5
                 MOVE    WRK-Z9-5            TO  M700-OHKNKEN(IDX1)
              END-IF
              MOVE    SUM-RHKNKEN(IDX1)  TO  WRK-Z9-5
              MOVE    WRK-Z9-5           TO  M700-RHKNKEN(IDX1)
      *
      *    診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
                      UNTIL   IDX   >    16
                      MOVE    SUM-TEN (IDX IDX1)
                                         TO   WRK-Z9-9
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX IDX1)
              END-PERFORM
      *
      *    金額合計
              MOVE  SUM-HKNFTN(IDX1)     TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-HKNFTN(IDX1)
              MOVE  SUM-JIHIFTN(IDX1)    TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-JIHIFTN(IDX1)
              MOVE  SUM-SEIKYU(IDX1)     TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-GSEIKYU(IDX1)
              MOVE  SUM-RYOSYU(IDX1)     TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-GRYOSYU(IDX1)
      *
             END-PERFORM
      *
      *    月計
      *    保険点数、件数
              MOVE    SUM-HKNTEN(32)     TO  WRK-Z9-9
              MOVE    WRK-Z9-9           TO  M700-GHKNTEN(32)
              IF WRK-NYUGAIKBN =   "2"
                 MOVE    SUM-SHKNKEN(32) TO  WRK-Z9-5
                 MOVE    WRK-Z9-5        TO  M700-SHKNKEN(32)
                 MOVE    SUM-OHKNKEN(32) TO  WRK-Z9-5
                 MOVE    WRK-Z9-5        TO  M700-OHKNKEN(32)
              END-IF
              MOVE    SUM-RHKNKEN(32)    TO  WRK-Z9-5
              MOVE    WRK-Z9-5           TO  M700-RHKNKEN(32)
      *
      *    診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
                      UNTIL   IDX   >    16
                      MOVE    SUM-TEN (IDX 32)
                                         TO   WRK-Z9-9
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX 32)
              END-PERFORM
      *
      *    金額
              MOVE  SUM-HKNFTN(32)       TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-HKNFTN(32)
              MOVE  SUM-JIHIFTN(32)      TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-JIHIFTN(32)
              MOVE  SUM-SEIKYU(32)       TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-GSEIKYU(32)
              MOVE  SUM-RYOSYU(32)       TO   WRK-S9-10
              MOVE  WRK-S9-10            TO   M7001-GRYOSYU(32)
      *
           END-IF
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
      *
           IF    ( WRK-PARA-PRTSYMD  >=  CONST-H200401 )
               MOVE    "A00000M700N.red"       TO  WRK-FORMID
               MOVE    "A00000M7001N.red"      TO  WRK-FORMID1
           ELSE
               MOVE    "A00000M700O.red"       TO  WRK-FORMID
               MOVE    "A00000M7001O.red"      TO  WRK-FORMID1
           END-IF
      *
           MOVE    WRK-FORMID          TO  SPRT-PRTID
           MOVE    "月計表（診療費請求明細）"
                                       TO  SPRT-TITLE
           MOVE    M700                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               MOVE    WRK-FORMID1         TO  SPRT-PRTID
               MOVE    "月計表（診療費請求明細）"
                                           TO  SPRT-TITLE
               MOVE    M7001               TO  SPRT-PRTDATA
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
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    ZERO        TO  FLG-TOTAL
      *    CSVヘッダ編集処理
           PERFORM  800-CSV-HEAD-SEC
      *
      *    CSV編集
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-KA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票<入外区分別全科合計>印刷処理
      *****************************************************************
      *
       330-TERM-GK-PRINT-SEC         SECTION.
      *
           INITIALIZE                      M700
           INITIALIZE                      M7001
      *
           MOVE    WRK-SYSYMDWH        TO  M700-SYSYMD
           MOVE    WRK-PARA-PRTYMWH    TO  M700-PRTYM
           MOVE    WRK-SYSYMDWH        TO  M7001-SYSYMD
           MOVE    WRK-PARA-PRTYMWH    TO  M7001-PRTYM
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"         TO  M700-NYUGAIKBN
                   MOVE "入院"         TO  M7001-NYUGAIKBN
                   MOVE  "請求件数"    TO  M700-KENSU2
              WHEN "2"
                   MOVE "外来"         TO  M700-NYUGAIKBN
                   MOVE "外来"         TO  M7001-NYUGAIKBN
                   MOVE  "その他件数"  TO  M700-KENSU3
                   MOVE  "再診件数"    TO  M700-KENSU2
                   MOVE  "初診件数"    TO  M700-KENSU1
                   MOVE  "受診者数"    TO  M700-TJYUSHIN
           END-EVALUATE
      *
      *    診療科名称
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                    IF  WRK-PRNSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                        MOVE  "全科合計"    TO  M700-SRYKA
                        MOVE  "全科合計"    TO  M7001-SRYKA
                    ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                       INITIALIZE                    SYS-1005-REC
                       MOVE  "1005"              TO  SYS-1005-KANRICD
                       MOVE  WRK-SRYKA           TO  SYS-1005-KBNCD
                       MOVE  SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
                       MOVE  WRK-PARA-PRTLYMD    TO  WRK-SYMD
                       MOVE  WRK-PARA-PRTSYMD    TO  SYS-1005-EDYUKYMD
                       MOVE  WRK-PARA-PRTLYMD    TO  SYS-1005-STYUKYMD
                       MOVE  SYS-1005-REC        TO  MCPDATA-REC
                       PERFORM 820-SYSKANRI-READ-SEC
                       IF    FLG-SYSKANRI2        =   ZERO
                        MOVE  MCPDATA-REC        TO  SYS-1005-REC
                        MOVE  SYS-1005-SRYKANAME TO  M700-SRYKA
                        MOVE  SYS-1005-SRYKANAME TO  M7001-SRYKA
                       ELSE
                        MOVE  SPACE              TO  M700-SRYKA
                        MOVE  SPACE              TO  M7001-SRYKA
                       END-IF
                    END-IF
               WHEN "2"
                    IF  WRK-PRGSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                        MOVE  "全科合計"         TO  M700-SRYKA
                        MOVE  "全科合計"         TO  M7001-SRYKA
                    ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                       INITIALIZE                    SYS-1005-REC
                       MOVE  "1005"              TO  SYS-1005-KANRICD
                       MOVE  WRK-SRYKA           TO  SYS-1005-KBNCD
                       MOVE  SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
                       MOVE  WRK-PARA-PRTLYMD    TO  WRK-SYMD
                       MOVE  WRK-PARA-PRTSYMD    TO  SYS-1005-EDYUKYMD
                       MOVE  WRK-PARA-PRTLYMD    TO  SYS-1005-STYUKYMD
                       MOVE  SYS-1005-REC        TO  MCPDATA-REC
                       PERFORM 820-SYSKANRI-READ-SEC
                       IF    FLG-SYSKANRI2        =   ZERO
                        MOVE  MCPDATA-REC        TO  SYS-1005-REC
                        MOVE  SYS-1005-SRYKANAME TO  M700-SRYKA
                        MOVE  SYS-1005-SRYKANAME TO  M7001-SRYKA
                       ELSE
                        MOVE  SPACE              TO  M700-SRYKA
                        MOVE  SPACE              TO  M7001-SRYKA
                       END-IF
                    END-IF
           END-EVALUATE
      *
           IF  FLG-GET = ZERO
      *    対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M700-JOBMSG
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M7001-JOBMSG
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
           ELSE
      *
      *    対象データが１件以上存在した場合
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
      *
      *    月末最終日の設定
             MOVE WRK-PARA-PRTLYMD(7:2)      TO  IDX7G
             PERFORM VARYING   IDX1    FROM  1   BY  1
                     UNTIL     IDX1    >     IDX7
      *
      *    日別
      *    保険点数、件数
              MOVE    SUM-HKNTEN-GK(IDX1)    TO  WRK-Z9-9
              MOVE    WRK-Z9-9               TO  M700-GHKNTEN(IDX1)
              IF WRK-NYUGAIKBN =   "2"
                MOVE    SUM-SHKNKEN-GK(IDX1) TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-SHKNKEN(IDX1)
                MOVE    SUM-OHKNKEN-GK(IDX1) TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-OHKNKEN(IDX1)
              END-IF
              MOVE    SUM-RHKNKEN-GK(IDX1)   TO  WRK-Z9-5
              MOVE    WRK-Z9-5               TO  M700-RHKNKEN(IDX1)
      *
      *    診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
                      UNTIL   IDX   >    16
                      MOVE    SUM-TEN-GK(IDX IDX1)
                                         TO   WRK-Z9-9
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX IDX1)
              END-PERFORM
      *
      *    金額
              MOVE  SUM-HKNFTN-GK(IDX1)   TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-HKNFTN(IDX1)
              MOVE  SUM-JIHIFTN-GK(IDX1)  TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-JIHIFTN(IDX1)
              MOVE  SUM-SEIKYU-GK(IDX1)   TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-GSEIKYU(IDX1)
              MOVE  SUM-RYOSYU-GK(IDX1)   TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-GRYOSYU(IDX1)
      *
      *    受診者数
              IF WRK-NYUGAIKBN =   "2"
                   MOVE  SUM-JYUSHINKEN-GK(IDX1)
                         TO   WRK-Z9-5
                   MOVE  WRK-Z9-5
                         TO   M700-JYUSHINCNT(IDX1)
              END-IF
             END-PERFORM
      *
      *    月計
      *    保険点数、件数
              MOVE    SUM-HKNTEN-GK(32)      TO  WRK-Z9-9
              MOVE    WRK-Z9-9               TO  M700-GHKNTEN(32)
              IF WRK-NYUGAIKBN =   "2"
                MOVE    SUM-SHKNKEN-GK(32)   TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-SHKNKEN(32)
                MOVE    SUM-OHKNKEN-GK(32)   TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-OHKNKEN(32)
              END-IF
              MOVE    SUM-RHKNKEN-GK(32)     TO  WRK-Z9-5
              MOVE    WRK-Z9-5               TO  M700-RHKNKEN(32)
      *
      *    診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
                      UNTIL   IDX   >    16
                      MOVE    SUM-TEN-GK(IDX 32)
                                         TO   WRK-Z9-9
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX 32)
              END-PERFORM
      *
      *    金額
              MOVE  SUM-HKNFTN-GK(32)     TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-HKNFTN(32)
              MOVE  SUM-JIHIFTN-GK(32)    TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-JIHIFTN(32)
              MOVE  SUM-SEIKYU-GK(32)     TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-GSEIKYU(32)
              MOVE  SUM-RYOSYU-GK(32)     TO   WRK-S9-10
              MOVE  WRK-S9-10             TO   M7001-GRYOSYU(32)
      *
      *    受診者数
              IF WRK-NYUGAIKBN =   "2"
                   MOVE  SUM-JYUSHINKEN-GK(32)
                         TO   WRK-Z9-5
                   MOVE  WRK-Z9-5
                         TO   M700-JYUSHINCNT(32)
              END-IF
           END-IF
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
      *
           IF    ( WRK-PARA-PRTSYMD  >=  CONST-H200401 )
               MOVE    "A00000M700N.red"       TO  WRK-FORMID
               MOVE    "A00000M7001N.red"      TO  WRK-FORMID1
           ELSE
               MOVE    "A00000M700O.red"       TO  WRK-FORMID
               MOVE    "A00000M7001O.red"      TO  WRK-FORMID1
           END-IF
      *
           MOVE    WRK-FORMID          TO  SPRT-PRTID
           MOVE    "月計表（診療費請求明細）"
                                       TO  SPRT-TITLE
           MOVE    M700                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               MOVE    WRK-FORMID1         TO  SPRT-PRTID
               MOVE    "月計表（診療費請求明細）"
                                           TO  SPRT-TITLE
               MOVE    M7001               TO  SPRT-PRTDATA
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
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    1        TO  FLG-TOTAL
      *    CSVヘッダ編集処理
           PERFORM  800-CSV-HEAD-SEC
      *
      *    CSV編集
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-TERM-GK-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                TO  IDXP
      *    対象年月
           INITIALIZE                WRK-ITEM
           STRING    "対象年月："    DELIMITED  BY  SIZE
                     M700-PRTYM      DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分セット
           INITIALIZE                WRK-ITEM
           STRING    "入外区分："    DELIMITED  BY  SIZE
                     M700-NYUGAIKBN  DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科セット
           INITIALIZE                WRK-ITEM
           STRING    "診療科："      DELIMITED  BY  SIZE
                     M700-SRYKA      DELIMITED  BY  SIZE
                                     INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付
           MOVE    M700-SYSYMD       TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    名称セット
      *    受診者数
           MOVE    M700-TJYUSHIN     TO  CSV-HEAD-KENSU1
      *    初診件数
           MOVE    M700-KENSU1       TO  CSV-HEAD-KENSU2
      *    再診件数
           MOVE    M700-KENSU2       TO  CSV-HEAD-KENSU3
      *    その他件数
           MOVE    M700-KENSU3       TO  CSV-HEAD-KENSU4
      *
           MOVE    "日"              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    各項目名称セット
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             IF  ( CSV-HEAD-ITEM(IDX4)  NOT =  SPACE )
               MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
               IF   IDX4  >=  CSV-HEAD-MAX
                 PERFORM  810-CSVDATA-EDIT-SEC
               ELSE
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
             END-IF
           END-PERFORM
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  32
      *    日
             IF  IDX4  =  32
               MOVE    "計"            TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             ELSE
               MOVE    IDX4            TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
             IF  WRK-NYUGAIKBN  =  1
      *    再診件数
               MOVE    M700-RHKNKEN(IDX4)     TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             ELSE
      *    受診者数
               IF   FLG-TOTAL  =  1
                 MOVE    M700-JYUSHINCNT(IDX4)  TO  WRK-ITEM
                 PERFORM  800-CSVDATA-EDIT-SEC
               END-IF
      *
      *    初診件数
               MOVE    M700-SHKNKEN(IDX4)     TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    再診件数
               MOVE    M700-RHKNKEN(IDX4)     TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    その他件数
               MOVE    M700-OHKNKEN(IDX4)     TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
      *    保険点数
             MOVE    M700-GHKNTEN(IDX4)       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療内容明細
             PERFORM  VARYING  IDX5  FROM  1  BY  1
                      UNTIL  IDX5  >  16
               MOVE    M700-TEN(IDX5 IDX4)  TO  WRK-ITEM
               PERFORM  800-CSVDATA-EDIT-SEC
             END-PERFORM
      *
      *    保険負担額
             MOVE    M7001-HKNFTN(IDX4)       TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    自費負担額
             MOVE    M7001-JIHIFTN(IDX4)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    窓口請求金額
             MOVE    M7001-GSEIKYU(IDX4)      TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    窓口領収金額
             MOVE    M7001-GRYOSYU(IDX4)      TO  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    ＣＳＶ出力処理
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-CR-SEC
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
           PERFORM  VARYING  IDX6  FROM  1  BY  1
                    UNTIL  IDX6  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX6:)  =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX6:1)  NOT =  SPACE
                 MOVE    IDX6      TO  IDX-ST
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
           IF  WRK-PARA-PRTYM  >=  "200804"
             INITIALIZE                     ORCSTOUKEICSVAREA
             MOVE    "INS"                  TO  STOUKEICSV-MODE
             MOVE    LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
             MOVE    LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
             MOVE    LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
             MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
             MOVE    LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
             MOVE    LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
             MOVE    ZERO                   TO  STOUKEICSV-PTID
             MOVE    "/var/tmp/"            TO  STOUKEICSV-TO-FOLDER
      *
             STRING  SPA-HOSPNUM            DELIMITED  BY  SIZE
                     "gekkei_shinryouhi_"   DELIMITED  BY  SIZE
                     WRK-PARA-PRTYM         DELIMITED  BY  SIZE
                     ".csv"                 DELIMITED  BY  SIZE
                                            INTO  STOUKEICSV-TO-DATA
             END-STRING
             MOVE    "2"                    TO  STOUKEICSV-CODE-TYPE
             MOVE    WRK-CSVDATA            TO  STOUKEICSV-CSVDATA
             MOVE    "月計表（診療費請求明細"  TO  STOUKEICSV-TITLE
      *
             CALL    "ORCSTOUKEICSV"     USING
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
             INITIALIZE                  WRK-CSVDATA
           END-IF
      *
           .
      *
       230-TOUKEICSV-EXT.
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
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
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"          TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SMEI-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SMEI-NYUHEN-YMD
           MOVE    WRK-PARA-PRTLYMD    TO  SMEI-UPNYUHEN-YMD
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *
           MOVE   "tbl_syumei"                TO  MCP-TABLE
           MOVE   "st_key3"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
                PERFORM 910-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE   "tbl_syumei"                TO  MCP-TABLE
           MOVE   "st_key3"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SYUMEI
               MOVE    MCPDATA-REC   TO  SYUMEI-REC
           ELSE
               MOVE    1             TO  FLG-SYUMEI
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
           MOVE   "tbl_syumei"                TO  MCP-TABLE
           MOVE   "st_key3"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    受診履歴SELECT
      ******************************************************************
       900-JYURRK-SELECT-SEC                 SECTION.

           INITIALIZE                         JYURRK-REC
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)    TO   JYURRK-HOSPNUM
           MOVE    WRK-SMEI-PTID(IDX-S)       TO   JYURRK-PTID
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)  TO   JYURRK-NYUGAIKBN
           MOVE    WRK-SMEI-DENPNUM(IDX-S)    TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"             TO   MCP-FUNC
           MOVE    "tbl_jyurrk"           TO   MCP-TABLE
           MOVE    "key6 "                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                    TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    受診履歴FETCH
      ******************************************************************
       900-JYURRK-FETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key6"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC       TO   JYURRK-REC
             MOVE    ZERO              TO   FLG-JYURRK
           ELSE
             MOVE    1                 TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      ******************************************************************
      *    受診履歴CLOSE
      ******************************************************************
       900-JYURRK-CLOSE-SEC                 SECTION.
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
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
           MOVE    SMEI-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID        TO  SYU-PTID
           MOVE    SMEI-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE   "tbl_syunou"            TO  MCP-TABLE
               MOVE   "key"                   TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO                    TO  FLG-SYUNOU
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)
                         TO  SYU-HOSPNUM
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)
                         TO  SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)
                         TO  SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)
                         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE   "tbl_syunou"            TO  MCP-TABLE
               MOVE   "key"                   TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO                    TO  FLG-SYUNOU
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SYU-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    SYU-SYUHKNNUM       TO  HKN-HKNNUM
           MOVE    WRK-PARA-PRTLYMD    TO  HKN-TEKSTYMD
           MOVE    ALL "9"             TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE   "tbl_hknnum"             TO  MCP-TABLE
           MOVE   "key5"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE   "tbl_hknnum"             TO  MCP-TABLE
               MOVE   "key5"                   TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"         USING
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
           MOVE   "tbl_hknnum"             TO  MCP-TABLE
           MOVE   "key5"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"              USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE   "tbl_syskanri"              TO  MCP-TABLE
               MOVE   "key10"                     TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
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
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE   "tbl_syskanri"              TO  MCP-TABLE
               MOVE   "key10"                     TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
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
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC                   =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_syskanri"              TO  MCP-TABLE
               MOVE    "key10"                     TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    ZERO         TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1            TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1                TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"              TO  MCP-TABLE
           MOVE    "key10"                     TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       820-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"           TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
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
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"           TO  MCP-TABLE
               MOVE    "key"                    TO  MCP-PATHNAME
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
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災マスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"           TO  MCP-TABLE
               MOVE    "key"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_ptrsiinf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
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
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"           TO  MCP-TABLE
               MOVE    "key"                    TO  MCP-PATHNAME
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
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
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
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_sryacct"            TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
               MOVE    "tbl_sryacct"            TO  MCP-TABLE
               MOVE    "key3"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SRYACCT-REC
                   MOVE    ZERO                    TO  FLG-SRYACCT
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
           MOVE    "tbl_sryacct"            TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
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
           MOVE    SRYACT-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"        TO   MCP-FUNC
           MOVE    "tbl_sryact"      TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO            TO   FLG-SRYACT
           ELSE
             MOVE    1               TO   FLG-SRYACT
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
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "DBFETCH"          TO   MCP-FUNC
           MOVE    "tbl_sryact"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO             TO   FLG-SRYACT
             MOVE    MCPDATA-REC      TO   SRYACT-REC
           ELSE
             MOVE    1                TO   FLG-SRYACT
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
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_sryact"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
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
           MOVE    "DBCOMMIT"           TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
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
