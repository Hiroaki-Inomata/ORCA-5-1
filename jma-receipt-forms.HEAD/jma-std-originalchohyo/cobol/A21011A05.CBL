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
       PROGRAM-ID.             A21011A05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保留・再請求・月遅れリスト
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/10/03    @@@@-有村     新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02                 11/01/13  公費請求テーブルを参照
      *                                     月途中での本家区分変更へ対応
      *  01.00.03                 11/01/27  公費請求テーブルの参照方法を変更
      *  01.00.04    @@@@-門間    11/10/18  月途中での負担者番号変更へ対応
      *  01.00.05    @@@@-門間    11/12/07  公費単独で公費が２つ以上存在する場合、
      *                                     日数が集計されるように変更
      *  01.00.06    @@@@-門間    12/03/21  「レセプト請求しない」公費を持っている保留データにおいて、
      *                                     「保険者番号、本家区分、主保険、給付割合」が印字されない。
      *  01.00.07    @@@@-門間    12/05/25  公費単独の「回数、金額、標準負担額」が印字されない。
      *                                     CSVファイル作成機能追加
      *  01.00.08    @@@@-門間    12/09/26  公費併用の場合、一部負担金が印字されない。
      *  01.00.09    @@@@-門間    14/04/14  請求管理クローズ処理が実行されるよう変更
      *                                     公費請求読み込み時、項目を追加
      *                           14/04/18  公費単独複数登録時の不具合修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    16/03/11  一時ファイルに特記事項を追加（07老併、08老健）
      *  02.00.03    @@@@-門間    16/05/16  一時ファイルに適用開始年月日を追加
      *  02.00.04    @@@@-門間    17/08/03  旧ＣＳＶ出力機能を削除
      *  02.00.05    @@@@-門間    18/07/19  公費請求情報参照時の処理条件等を変更
      *  03.00.01    @@@@-門間    20/03/03  処理時間の改善
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    レセプト集計表一時ファイル
           SELECT  A05TMP-FILE   ASSIGN    A05TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  A05TMP-KEY
                                   FILE    STATUS  IS  STS-A05TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    レセプト集計表一時ファイル
       FD  A05TMP-FILE.
       01  A05TMP-REC.
           COPY    "A21011A05TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //A05TP//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A21011A05V02.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-A05TMP          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-TYPE            PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-NYUINRRK        PIC 9(01).
           03  FLG-KOHI            PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-RECE10          PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1               PIC 9(04).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(04).
           03  IDX5               PIC 9(04).
           03  IDX6               PIC 9(04).
           03  IDX-RECEMAX        PIC 9(03).
           03  IDX                PIC 9(04).
           03  IDY                PIC 9(04).
           03  IDZ                PIC 9(04).
           03  IDM                PIC 9(04).
           03  IDS                PIC 9(02).
           03  IDS2               PIC 9(02).
           03  IDS3               PIC 9(02).
           03  IDS4               PIC 9(02).
      *
           03  IDX8              PIC 9(04).
           03  IDX10             PIC 9(04).
           03  IDX-ST            PIC 9(03).
           03  IDX-ED            PIC 9(03).
           03  IDXP              PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *    出力区分
           03  WRK-PARA-SYUTURYOKU   PIC X(01).
      *    対象診療年月
           03  WRK-PARA-SKYYM        PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY         PIC 9(04).
               05  WRK-SRYMM         PIC 9(02).
               05  WRK-SRYDD         PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SRYYM             PIC X(22).
           03  WRK-SEIYMD            PIC X(22).
           03  WRK-RECEERR           PIC X(200).
           03  WRK-KBN-OLD           PIC X(20).
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-SYUNISSU-TBL.
               05  WRK-SYUNISSU-LINE  OCCURS 31.
                   07  WRK-SYUNISSU   PIC 9(01).
           03  WRK-SYUYMD            PIC 9(08).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z4                PIC ZZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11               PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13               PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14               PIC ZZ,ZZZ,ZZZ,ZZZ.
           03  WRK-ZZ                PIC ZZZZZZZZZZZZ.
           03  WRK-S9                PIC -----------9.
           03  WRK-SZ                PIC -----------Z.
      *    集計用エリア
       01  SUM-AREA.
           03  SUM-NISSU                PIC  9(04).
           03  SUM-TENSU                PIC  9(07).
           03  SUM-YKZFTN               PIC  9(07).
           03  SUM-ITBFTN               PIC  9(07).
           03  SUM-SNISSU               PIC  9(04).
           03  SUM-SKINGK               PIC  9(07).
           03  SUM-SFTNGK               PIC  9(07).
      *
           03  SUM-SYUNISSU         PIC 9(03).
      *
           03  WRK-SRYKANAME         PIC X(10).
           03  WRK-REC-LENGTH        PIC 9(04).
           03  IDW                   PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA          PIC X(100).
               05  WRK-NAGASA        PIC 9(04).
           03  WRK-SABNAME           PIC X(16).
           03  WRK-STUKBN            PIC X(01).
           03  WRK-STARTKBN          PIC X(01).
      *    キー用エリア
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-SYUTURYOKU     PIC  X(01).
           03  KEY-OLD.
               05  KEY-O-SYUTURYOKU     PIC  X(01).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ＣＳＶファイル出力用
       01  A21011A05CSV.
           03  FILLER       PIC X(11)   VALUE
                                  "/home/orca/".
           03  CSV-DENPPRTNAME      PIC X(09)   VALUE   "A21011A05".
           03  FILLER               PIC X(01)   VALUE   "-".
           03  CSV-PRTKBN           PIC X(01).
           03  FILLER               PIC X(01)   VALUE   "-".
           03  CSV-DENPPRTYMDHM     PIC 9(12).
           03  FILLER               PIC X(04)   VALUE   ".csv".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-PTNUM      PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-PTNAME     PIC X(50) VALUE "患者氏名".
               07  CSV-HEAD-HKNJANUM   PIC X(50) VALUE "保険者番号".
               07  CSV-HEAD-NYUGAIKBN  PIC X(50) VALUE "入外".
               07  CSV-HEAD-HONKZKKBN  PIC X(50) VALUE "本家".
               07  CSV-HEAD-SYUHKN     PIC X(50) VALUE "主保険".
               07  CSV-HEAD-ZOKUGARA   PIC X(50) VALUE "老人保険".
               07  CSV-HEAD-KYURATE    PIC X(50) VALUE "給付割合".
               07  CSV-HEAD-KOH1       PIC X(50) VALUE "公費１".
               07  CSV-HEAD-KOH2       PIC X(50) VALUE "公費２".
               07  CSV-HEAD-NISSU      PIC X(50) VALUE "日数".
               07  CSV-HEAD-TENSU      PIC X(50) VALUE "点数".
               07  CSV-HEAD-YKZITBFTN  PIC X(50) VALUE "薬剤一部負担金".
               07  CSV-HEAD-ITBFTN     PIC X(50) VALUE "一部負担金".
               07  CSV-HEAD-KAISU      PIC X(50) VALUE "回数".
               07  CSV-HEAD-KINGAKU    PIC X(50) VALUE "金額".
               07  CSV-HEAD-FTNMONEY   PIC X(50) VALUE "標準負担額".
               07  CSV-HEAD-SRYYM      PIC X(50) VALUE "診療年月".
      *
             05  CSV-HEAD-R            REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM       PIC X(50) OCCURS 18.
             05  CSV-HEAD-MAX          PIC 9(05) VALUE  18.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    ORCAレセプト印刷順情報
           COPY    "CPSK2003.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    収納情報
       01  NYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
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
           IF  FLG-ERR  =  1
             PERFORM  210-ERR-SEC
           ELSE
             PERFORM  200-MAIN-SEC
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
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           ACCEPT    SYS-YMD       FROM    DATE
           ACCEPT    SYS-TIME      FROM    TIME
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
                                               WRK-PARA-SYUTURYOKU
                                               WRK-PARA-SKYYM
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
           MOVE    "A21011A05"      TO  JOB-PGID
           MOVE    "保留・再請求・月遅れリスト"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "A2A05TP"                TO  A05TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  A05TP-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  A05TP-HOSPNUM
      *
           MOVE    A05TP-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  A05TP-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *
           MOVE    WRK-PARA-SKYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    WRK-PARA-SKYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
                                               SPA-AREA
           IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMD
           END-IF
      *
      *    医療機関ＩＤ編集
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE    "*"            TO      SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1001-STYUKYMD(1:6)
                                           SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF     FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO    SYS-1001-REC
                  IF      SYS-1001-HOSPNAME  =  LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-HOSPNAME
                  END-IF
                  IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-KAISETUNAME
                  END-IF
           ELSE
                  MOVE    "医療機関情報が取得できませんでした"
                                          TO    WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO    FLG-END
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE     SPACE         TO        SYS-1031-REC
           INITIALIZE                       SYS-1031-REC
           MOVE     "1031"        TO        SYS-1031-KANRICD
           MOVE     "*"           TO        SYS-1031-KBNCD
           MOVE     SYS-1031-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI        =   ZERO
                    MOVE    MCPDATA-REC TO  SYS-1031-REC
           ELSE
                    INITIALIZE              SYS-1031-REC
           END-IF
      *
      *    システム日付セット
           MOVE  LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SEIYMD
      *
      *    エラーチェック
           IF    WRK-PARA-SYUTURYOKU  NOT  =  "1" AND "2" AND "3"
                                          AND "9"
             MOVE    1     TO    FLG-ERR
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE   A05
           OPEN      OUTPUT A05TMP-FILE
           CLOSE            A05TMP-FILE
           OPEN   I-O       A05TMP-FILE
      *
           PERFORM  900-SEIKYU-SELECT-SEC
           PERFORM  UNTIL  FLG-END  =  1
      *
      *      初期化
             INITIALIZE  A05TMP-REC
      *
      *      一時ファイル編集処理
             IF  FLG-KOHI  =  ZERO
               PERFORM  2001-HENSYU-SEC
             ELSE
               PERFORM  2002-HENSYU-SEC
             END-IF
      *
             WRITE  A05TMP-REC
      *
             PERFORM  900-SEIKYU-READ-SEC
      *
           END-PERFORM
           PERFORM  900-SEIKYU-CLOSE-SEC
      *
           CLOSE            A05TMP-FILE
      *    印刷処理
           IF  CNT-RECE10   >       ZERO
               PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       210-ERR-SEC                 SECTION.
      *
           INITIALIZE                        A05
           ADD  1  TO  CNT-PAGE
      *
      *    ページ数
           MOVE    CNT-PAGE    TO    WRK-PAGE
           MOVE    WRK-PAGE    TO    A05-PAGE
      *
      *    請求年月
           STRING  WRK-SRYYM         DELIMITED  BY SIZE
                   "分"              DELIMITED  BY SIZE
             INTO                    A05-SKYYM
           END-STRING
      *
      *    作成年月日
           MOVE  WRK-SEIYMD    TO    A05-SYSYMD
      *
           IF    FLG-ERR    =    1
             STRING  "出力区分エラー:"     DELIMITED  BY  SIZE
                     WRK-PARA-SYUTURYOKU   DELIMITED  BY  SIZE
                     INTO                  A05-JOBMSG
             END-STRING
           ELSE
             STRING "対象データがありません。出力区分:"
                                           DELIMITED BY   SIZE
                    WRK-PARA-SYUTURYOKU    DELIMITED BY   SIZE
                    INTO                   A05-JOBMSG
             END-STRING
           END-IF
      *
           PERFORM   330-PRINT-OUT-SEC
      *
           .
      *
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル編集処理
      *****************************************************************
       2001-HENSYU-SEC                SECTION.
      *
      *    出力区分設定
           MOVE    FLG-TYPE            TO    A05TMP-SYUTURYOKU
      *
      *    患者番号設定
           MOVE    RECE10-PTNUM        TO    A05TMP-PTNUM
      *
      *    患者氏名設定
           MOVE    PTINF-NAME          TO    A05TMP-PTNAME
      *
      *    入外区分設定
           MOVE    RECE10-NYUGAIKBN    TO    A05TMP-NYUGAIKBN
      *
      *    給付割合設定
           MOVE    RECE10-KYURATE      TO    A05TMP-KYURATE
      *
      *    診療年月設定
           MOVE    RECE10-SRYYM        TO    A05TMP-SRYYM
      *
      *    保険者番号設定
           MOVE    RECE10-HKNJANUM     TO    A05TMP-HKNJANUM
      *
      *    本人・家族区分設定
           MOVE    RECE10-HONKZKKBN    TO    A05TMP-HONKZKKBN
      *
      *    主保険番号設定
           MOVE    RECE10-HKNNUM       TO    A05TMP-HKNNUM
      *
      *    老人保健番号設定
           MOVE    RECE10-RJNHKNNUM    TO    A05TMP-RJNHKNNUM
      *
      *    公費番号設定
           MOVE    RECE10-KOHNUM(1)     TO    A05TMP-KOHNUM1
           MOVE    RECE10-KOHNUM(2)     TO    A05TMP-KOHNUM2
      *
      *    負担者番号設定
           PERFORM  VARYING  IDS  FROM  1  BY  1
                    UNTIL    IDS  >     2
      *
             PERFORM  910-PTKOHINF-READ-SEC
      *
             IF  IDS  =  1
               MOVE    PTKOH-FTNJANUM    TO    A05TMP-KOHFTNNUM1
             ELSE
               MOVE    PTKOH-FTNJANUM    TO    A05TMP-KOHFTNNUM2
             END-IF
           END-PERFORM
      *
      *    適用開始年月日
           MOVE    RECE10-TEKSTYMD     TO    A05TMP-TEKSTYMD
      *
      *    特記事項（07老併、08老健）
           PERFORM  VARYING  IDS  FROM  1  BY  1
                    UNTIL    IDS  >     5
             IF  RECE10-TOKKI(IDS)  =  "07"  OR  "08"
               MOVE    RECE10-TOKKI(IDS)   TO  A05TMP-TOKKI
             END-IF
           END-PERFORM
      *
           INITIALIZE  IDS
                       SUM-SYUNISSU
                       WRK-SYUNISSU-TBL
      *
           IF  ( RECE10-KOHNUM(1)  NOT =  SPACE )
               AND  ( RECE10-HKNNUM  =  SPACE )
      *      他の公費を併用しているか判定
             IF  ( RECE10-KOHNUM(2)  NOT =  SPACE )
               OR ( RECE10-KOHNUM(3) NOT =  SPACE )
               OR ( RECE10-KOHNUM(4) NOT =  SPACE )
      *
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >  4
                 IF  RECE10-KOHNUM(IDX4)  NOT = SPACE
      *
      *            点数設定
                   ADD     RECE10-TOTALTEN(IDX4 + 1)
                                                 TO  A05TMP-TENSU
      *
      *            食事回数
                   ADD     RECE10-SHOKUJINISSU(IDX4 + 1)
                                                 TO  A05TMP-SNISSU
      *
      *            食事療養費
                   ADD     RECE10-SHOKUJIRYOYOHI(IDX4 + 1)
                                                 TO  A05TMP-SKINGK
      *
      *            標準負担額
                   ADD     RECE10-SHOKUJIFTN(IDX4 + 1)
                                                 TO  A05TMP-SFTNGK
      *
                 END-IF
               END-PERFORM
      *
               PERFORM  VARYING  IDS  FROM  1  BY  1
                        UNTIL  IDS  >  31
                 MOVE  RECE10-SRYYM  TO  WRK-SYUYMD(1:6)
                 MOVE  IDS  TO  WRK-SYUYMD(7:2)
      *
                 INITIALIZE               SYUNOU-REC
                 MOVE  RECE10-HOSPNUM     TO   SYU-HOSPNUM
                 MOVE  RECE10-NYUGAIKBN   TO   SYU-NYUGAIKBN
                 MOVE  RECE10-PTID        TO   SYU-PTID
      *
                 PERFORM  920-SYUNOU-SET-SEC
                 PERFORM  925-SYUNOU-READ-SEC
                 IF  FLG-SYUNOU  =  ZERO
                   IF  SYU-NYUGAIKBN = 2
                     MOVE  1  TO  WRK-SYUNISSU(IDS)
                   END-IF
                 END-IF
                 PERFORM  940-SYUNOU-CLOSE-SEC
               END-PERFORM
               IF  RECE10-NYUGAIKBN = 2
                 PERFORM  VARYING  IDS  FROM  1  BY  1
                          UNTIL  IDS > 31
                   IF  WRK-SYUNISSU(IDS)  =  1
                     ADD    1        TO  SUM-SYUNISSU
                   END-IF
                 END-PERFORM
                 MOVE  SUM-SYUNISSU  TO  A05TMP-NISSU
               ELSE
      *
      *          対象年月最終日取得
                 INITIALIZE   WRK-SYSYMD
                 MOVE    RECE10-SRYYM       TO     WRK-SYSYMD(1:6)
                 PERFORM  31012-LASTDAY-GET-SEC
      *
                 INITIALIZE                     NYUINRRK-REC
                 MOVE   RECE10-HOSPNUM     TO   PTNYUINRRK-HOSPNUM
                 MOVE   RECE10-PTID        TO   PTNYUINRRK-PTID
                 MOVE   RECE10-SRYYM
                                    TO   PTNYUINRRK-TENNYUYMD(1:6)
                 MOVE   WRK-SYSYMD(7:2)
                                    TO   PTNYUINRRK-TENNYUYMD(7:2)
                 MOVE   RECE10-SRYYM
                                    TO   PTNYUINRRK-TENSTUYMD(1:6)
                 MOVE   "01"        TO   PTNYUINRRK-TENSTUYMD(7:2)
                 PERFORM  900-NYUINRRK-SET-SEC
                 PERFORM  900-NYUINRRK-READ-SEC
                 PERFORM  UNTIL  FLG-NYUINRRK  =  1
      *            開始日設定
                   IF  RECE10-SRYYM  >  PTNYUINRRK-TENNYUYMD(1:6)
                     MOVE     1                       TO   IDS2
                   ELSE
                     MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO   IDS2
                   END-IF
      *            終了日設定
                   IF  PTNYUINRRK-TENSTUYMD(1:6)  >  RECE10-SRYYM
                     MOVE  WRK-SYSYMD(7:2)            TO   IDS3
                   ELSE
                     MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO   IDS3
                   END-IF
                   PERFORM  VARYING IDS4  FROM IDS2  BY 1
                            UNTIL  IDS4  >  IDS3
                     MOVE   1     TO    WRK-SYUNISSU(IDS4)
                   END-PERFORM
                   PERFORM  900-NYUINRRK-READ-SEC
                 END-PERFORM
                 PERFORM  900-NYUINRRK-CLOSE-SEC
      *
                 PERFORM  VARYING  IDS  FROM  1  BY  1
                          UNTIL  IDS > 31
                   IF  WRK-SYUNISSU(IDS)  =  1
                     ADD    1        TO  SUM-SYUNISSU
                   END-IF
                 END-PERFORM
                 MOVE  SUM-SYUNISSU  TO  A05TMP-NISSU
      *
               END-IF
      *
             ELSE
      *        日数設定
               MOVE    RECE10-JNISSU(2)     TO  A05TMP-NISSU
      *
      *        点数設定
               MOVE    RECE10-TOTALTEN(2)   TO  A05TMP-TENSU
      *
      *        食事回数
               MOVE    RECE10-SHOKUJINISSU(2)
                                            TO  A05TMP-SNISSU
      *
      *        食事療養費
               MOVE    RECE10-SHOKUJIRYOYOHI(2)
                                            TO  A05TMP-SKINGK
      *
      *        標準負担額
               MOVE    RECE10-SHOKUJIFTN(2)
                                            TO  A05TMP-SFTNGK
             END-IF
      *
           ELSE
      *      日数設定
             MOVE    RECE10-JNISSU(1)     TO  A05TMP-NISSU
      *
      *      点数設定
             MOVE    RECE10-TOTALTEN(1)   TO  A05TMP-TENSU
      *
      *      食事回数
             MOVE    RECE10-SHOKUJINISSU(1)
                                          TO  A05TMP-SNISSU
      *
      *      食事療養費
             MOVE    RECE10-SHOKUJIRYOYOHI(1)
                                          TO  A05TMP-SKINGK
      *
      *      標準負担額
             MOVE    RECE10-SHOKUJIFTN(1)
                                          TO  A05TMP-SFTNGK
      *
           END-IF
      *
      *    一部負担金
           IF    ( RECE10-KOHNUM(1) =  SPACE )
             AND ( RECE10-KOHNUM(2) =  SPACE )
             AND ( RECE10-KOHNUM(3) =  SPACE )
             AND ( RECE10-KOHNUM(4) =  SPACE )
             MOVE    RECE10-FTNMONEY(1)   TO    A05TMP-ITBFTN
           ELSE
             IF  RECE10-KOHNUM(2) =  SPACE
               MOVE    RECE10-FTNMONEY(2)   TO    A05TMP-ITBFTN
             ELSE
               IF  RECE10-KOHNUM(3) =  SPACE
                 MOVE    RECE10-FTNMONEY(3)   TO    A05TMP-ITBFTN
               ELSE
                 IF  RECE10-KOHNUM(4) =  SPACE
                   MOVE    RECE10-FTNMONEY(4)   TO    A05TMP-ITBFTN
                 ELSE
                   MOVE    RECE10-FTNMONEY(5)   TO    A05TMP-ITBFTN
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       2001-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル編集処理２
      *****************************************************************
       2002-HENSYU-SEC                SECTION.
      *
      *    出力区分設定
           MOVE    FLG-TYPE            TO    A05TMP-SYUTURYOKU
      *
      *    患者番号設定
           MOVE    RECE02-PTNUM        TO    A05TMP-PTNUM
      *
      *    患者氏名設定
           MOVE    PTINF-NAME          TO    A05TMP-PTNAME
      *
      *    入外区分設定
           MOVE    RECE02-NYUGAIKBN    TO    A05TMP-NYUGAIKBN
      *
      *    給付割合設定
           MOVE    ZERO                TO    A05TMP-KYURATE
      *
      *    診療年月設定
           MOVE    RECE02-SRYYM        TO    A05TMP-SRYYM
      *
      *    保険者番号設定
           MOVE    "00000000"          TO    A05TMP-HKNJANUM
      *
      *    本人・家族区分設定
           MOVE    SPACE               TO    A05TMP-HONKZKKBN
      *
      *    主保険番号設定
           MOVE    SPACE               TO    A05TMP-HKNNUM
      *
      *    老人保健番号設定
           MOVE    RECE02-RJNHKNNUM    TO    A05TMP-RJNHKNNUM
      *
      *    公費番号設定
           MOVE    RECE02-KOHNUM(1)    TO    A05TMP-KOHNUM1
           MOVE    RECE02-KOHNUM(2)    TO    A05TMP-KOHNUM2
      *
      *    適用開始年月日
           MOVE    RECE02-TEKSTYMD     TO    A05TMP-TEKSTYMD
      *
      *    特記事項（07老併、08老健）
           PERFORM  VARYING  IDS  FROM  1  BY  1
                    UNTIL    IDS  >     5
             IF  RECE10-TOKKI(IDS)  =  "07"  OR  "08"
               MOVE    RECE10-TOKKI(IDS)   TO  A05TMP-TOKKI
             END-IF
           END-PERFORM
      *
           INITIALIZE  IDS
                       SUM-SYUNISSU
                       WRK-SYUNISSU-TBL
      *
           IF  ( RECE02-KOHNUM(1)  NOT =  SPACE )
               AND  ( RECE02-HKNNUM  =  SPACE )
      *      他の公費を併用しているか判定
             IF  ( RECE02-KOHNUM(2)  NOT =  SPACE )
               OR ( RECE02-KOHNUM(3) NOT =  SPACE )
               OR ( RECE02-KOHNUM(4) NOT =  SPACE )
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >  4
                 IF  RECE02-KOHNUM(IDX4)  NOT = SPACE
      *
      *            点数設定
                   ADD     RECE02-TOTALTEN(IDX4 + 1)  TO  A05TMP-TENSU
      *
      *            食事回数
                   ADD     RECE02-SHOKUJINISSU(IDX4 + 1)
                                                      TO  A05TMP-SNISSU
      *
      *            食事療養費
                   ADD     RECE02-SHOKUJIRYOYOHI(IDX4 + 1)
                                                      TO  A05TMP-SKINGK
      *
      *            標準負担額
                   ADD     RECE02-SHOKUJIFTN(IDX4 + 1)
                                                      TO  A05TMP-SFTNGK
      *
                 END-IF
               END-PERFORM
      *
               PERFORM  VARYING  IDS  FROM  1  BY  1
                        UNTIL  IDS  >  31
                 MOVE  RECE02-SRYYM  TO  WRK-SYUYMD(1:6)
                 MOVE  IDS  TO  WRK-SYUYMD(7:2)
      *
                 INITIALIZE                SYUNOU-REC
                 MOVE   RECE02-HOSPNUM     TO   SYU-HOSPNUM
                 MOVE   RECE02-NYUGAIKBN   TO   SYU-NYUGAIKBN
                 MOVE   RECE02-PTID        TO   SYU-PTID
      *
                 PERFORM  920-SYUNOU-SET-SEC
                 PERFORM  925-SYUNOU-READ-SEC
                 IF  FLG-SYUNOU  =  ZERO
                   IF  SYU-NYUGAIKBN = 2
                     MOVE  1  TO  WRK-SYUNISSU(IDS)
                   END-IF
                 END-IF
                 PERFORM  940-SYUNOU-CLOSE-SEC
               END-PERFORM
               IF  RECE02-NYUGAIKBN = 2
                 PERFORM  VARYING  IDS  FROM  1  BY  1
                          UNTIL  IDS > 31
                   IF  WRK-SYUNISSU(IDS)  =  1
                     ADD    1        TO  SUM-SYUNISSU
                   END-IF
                 END-PERFORM
                 MOVE  SUM-SYUNISSU  TO  A05TMP-NISSU
               ELSE
      *
      *    対象年月最終日取得
                 INITIALIZE   WRK-SYSYMD
                 MOVE    RECE02-SRYYM       TO     WRK-SYSYMD(1:6)
                 PERFORM  31012-LASTDAY-GET-SEC
      *
                 INITIALIZE                    NYUINRRK-REC
                 MOVE   RECE02-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
                 MOVE   RECE02-PTID        TO  PTNYUINRRK-PTID
                 MOVE   RECE02-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
                 MOVE   WRK-SYSYMD(7:2)    TO  PTNYUINRRK-TENNYUYMD(7:2)
                 MOVE   RECE02-SRYYM       TO  PTNYUINRRK-TENSTUYMD(1:6)
                 MOVE   "01"               TO  PTNYUINRRK-TENSTUYMD(7:2)
      *
                 PERFORM  900-NYUINRRK-SET-SEC
                 PERFORM  900-NYUINRRK-READ-SEC
                 PERFORM  UNTIL  FLG-NYUINRRK  =  1
      *           開始日設定
                   IF  RECE02-SRYYM  >  PTNYUINRRK-TENNYUYMD(1:6)
                     MOVE     1                       TO   IDS2
                   ELSE
                     MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO   IDS2
                   END-IF
      *           終了日設定
                   IF  PTNYUINRRK-TENSTUYMD(1:6)  >  RECE02-SRYYM
                     MOVE  WRK-SYSYMD(7:2)            TO   IDS3
                   ELSE
                     MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO   IDS3
                   END-IF
                   PERFORM  VARYING IDS4  FROM IDS2  BY 1
                            UNTIL  IDS4  >  IDS3
                     MOVE   1     TO    WRK-SYUNISSU(IDS4)
                   END-PERFORM
                   PERFORM  900-NYUINRRK-READ-SEC
                 END-PERFORM
                 PERFORM  900-NYUINRRK-CLOSE-SEC
      *
                 PERFORM  VARYING  IDS  FROM  1  BY  1
                          UNTIL  IDS > 31
                   IF  WRK-SYUNISSU(IDS)  =  1
                     ADD    1        TO  SUM-SYUNISSU
                   END-IF
                 END-PERFORM
                 MOVE  SUM-SYUNISSU  TO  A05TMP-NISSU
      *
               END-IF
             ELSE
      *        日数設定
               MOVE    RECE02-JNISSU(2)          TO  A05TMP-NISSU
      *
      *        点数設定
               MOVE    RECE02-TOTALTEN(2)        TO  A05TMP-TENSU
      *
      *        食事回数
               MOVE    RECE02-SHOKUJINISSU(2)    TO  A05TMP-SNISSU
      *
      *        食事療養費
               MOVE    RECE02-SHOKUJIRYOYOHI(2)  TO  A05TMP-SKINGK
      *
      *        標準負担額
               MOVE    RECE02-SHOKUJIFTN(2)      TO  A05TMP-SFTNGK
      *
             END-IF
           ELSE
      *      日数設定
             MOVE    RECE02-JNISSU(1)          TO  A05TMP-NISSU
      *
      *      点数設定
             MOVE    RECE02-TOTALTEN(1)        TO  A05TMP-TENSU
      *
      *      食事回数
             MOVE    RECE02-SHOKUJINISSU(1)    TO  A05TMP-SNISSU
      *
      *      食事療養費
             MOVE    RECE02-SHOKUJIRYOYOHI(1)  TO  A05TMP-SKINGK
      *
      *      標準負担額
             MOVE    RECE02-SHOKUJIFTN(1)      TO  A05TMP-SFTNGK
      *
           END-IF
      *
      *    一部負担金
           IF    ( RECE02-KOHNUM(1) =  SPACE )
             AND ( RECE02-KOHNUM(2) =  SPACE )
             AND ( RECE02-KOHNUM(3) =  SPACE )
             AND ( RECE02-KOHNUM(4) =  SPACE )
             MOVE    RECE02-FTNMONEY(1)   TO    A05TMP-ITBFTN
           ELSE
             IF  RECE02-KOHNUM(2) =  SPACE
               MOVE    RECE02-FTNMONEY(2)   TO    A05TMP-ITBFTN
             ELSE
               IF  RECE02-KOHNUM(3) =  SPACE
                 MOVE    RECE02-FTNMONEY(3)   TO    A05TMP-ITBFTN
               ELSE
                 IF  RECE02-KOHNUM(4) =  SPACE
                   MOVE    RECE02-FTNMONEY(4)   TO    A05TMP-ITBFTN
                 ELSE
                   MOVE    RECE02-FTNMONEY(5)   TO    A05TMP-ITBFTN
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       2002-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN       INPUT       A05TMP-FILE
           INITIALIZE             A05TMP-REC
           MOVE       ZERO        TO        FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
           MOVE  KEY-NEW  TO  KEY-OLD
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票編集<合計>
               IF    KEY-NEW   NOT  =  KEY-OLD OR
                     FLG-PRINT      =  1
                 PERFORM  3201-GOKEI-SEC
               END-IF
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE              A05TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                        A05
           ADD  1  TO  CNT-PAGE
      *
      *    ページ数
           MOVE    CNT-PAGE    TO    WRK-PAGE
           MOVE    WRK-PAGE    TO    A05-PAGE
      *
      *    請求年月
           STRING  WRK-SRYYM         DELIMITED  BY SIZE
                   "分"              DELIMITED  BY SIZE
             INTO                    A05-SKYYM
           END-STRING
      *
      *    作成年月日
           MOVE  WRK-SEIYMD    TO    A05-SYSYMD
      *
      *    出力区分
           EVALUATE  A05TMP-SYUTURYOKU
           WHEN "1"
             MOVE    "保留"          TO    A05-KBN
           WHEN "2"
             MOVE    "月遅れ"        TO    A05-KBN
           WHEN "3"
             MOVE    "再請求"        TO    A05-KBN
           END-EVALUATE
      *
      *    CSV ヘッダー処理
           MOVE    1                   TO  IDXP
           PERFORM  800-CSV-HEAD-SEC
      *
           .
      *
       310-HEAD-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    帳票編集<明細行>処理
      ******************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
           MOVE    ZERO    TO    FLG-PRINT
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     20
                    OR       FLG-PRINT  =  1
                    OR       KEY-NEW    NOT   =   KEY-OLD
      *
      *    患者番号
             IF  A05TMP-PTNUM(11:)  =  SPACE
               MOVE    A05TMP-PTNUM     TO   A05-PTNUM(IDX1)
             ELSE
               MOVE    A05TMP-PTNUM     TO   A05-PTNUM2(IDX1)
             END-IF
      *
      *    患者氏名
             MOVE    A05TMP-PTNAME      TO      A05-NAME(IDX1)
      *
      *    保険者番号
             MOVE    A05TMP-HKNJANUM    TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO      A05-HKNJANUM(IDX1)
      *
      *    入外区分
             EVALUATE  A05TMP-NYUGAIKBN
             WHEN "1"
               MOVE    "入院"           TO      A05-NYUGAI(IDX1)
             WHEN "2"
               MOVE    "外来"           TO      A05-NYUGAI(IDX1)
             END-EVALUATE
      *
      *    給付割合
             MOVE      A05TMP-KYURATE   TO      WRK-Z2
             MOVE      WRK-Z2           TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             STRING    WRK-OUT-INPUT           DELIMITED BY SPACE
                       "割"             DELIMITED BY SIZE
                       INTO                     A05-KYURATE(IDX1)
             END-STRING
      *    本人・家族
             EVALUATE  A05TMP-HONKZKKBN
             WHEN "1"
               MOVE    "本"             TO      A05-HONKZKKBN(IDX1)
             WHEN "2"
               MOVE    "家"             TO      A05-HONKZKKBN(IDX1)
             WHEN OTHER
               MOVE    "　"             TO      A05-HONKZKKBN(IDX1)
             END-EVALUATE
      *
      *    保険番号・老人保険番号
             MOVE    A05TMP-HKNNUM      TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-HKNNUM(IDX1)
             MOVE    A05TMP-RJNHKNNUM   TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-RJNHKNNUM(IDX1)
      *
      *    公費
             MOVE    A05TMP-KOHNUM1     TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT     TO      A05-KOHNUM1(IDX1)
             MOVE    A05TMP-KOHNUM2     TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT     TO      A05-KOHNUM2(IDX1)
      *
      *    日数
             MOVE    A05TMP-NISSU       TO      WRK-Z4
             MOVE    WRK-Z4             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-NISSU(IDX1)
      *
      *    点数
             MOVE    A05TMP-TENSU       TO      WRK-Z9
             MOVE    WRK-Z9             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-TENSU(IDX1)
      *
      *    薬剤一部負担金
             MOVE    A05TMP-YKZFTN      TO      WRK-Z7
             MOVE    WRK-Z7             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-YKZFTN(IDX1)
      *
      *    一部負担金
             MOVE    A05TMP-ITBFTN      TO      WRK-Z7
             MOVE    WRK-Z7             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-ITBFTN(IDX1)
      *
      *    食事回数
             MOVE    A05TMP-SNISSU      TO      WRK-Z4
             MOVE    WRK-Z4             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-SNISSU(IDX1)
      *
      *    食事療養費
             MOVE    A05TMP-SKINGK      TO      WRK-Z9
             MOVE    WRK-Z9             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-SKINGK(IDX1)
      *
      *    標準負担額
             MOVE    A05TMP-SFTNGK      TO      WRK-Z7
             MOVE    WRK-Z7             TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      A05-SFTNGK(IDX1)
      *
      *    診療年月
             MOVE    A05TMP-SRYYM       TO      WRK-SYMD(1:6)
             MOVE    "01"               TO      WRK-SYMD(7:2)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMDG(1:16)     TO      A05-SRYYM(IDX1)
      *
             PERFORM  400-CSV-DATA-SEC
      *
      *    小計加算
             ADD     A05TMP-NISSU       TO      SUM-NISSU
             ADD     A05TMP-TENSU       TO      SUM-TENSU
             ADD     A05TMP-YKZFTN      TO      SUM-YKZFTN
             ADD     A05TMP-ITBFTN      TO      SUM-ITBFTN
             ADD     A05TMP-SNISSU      TO      SUM-SNISSU
             ADD     A05TMP-SKINGK      TO      SUM-SKINGK
             ADD     A05TMP-SFTNGK      TO      SUM-SFTNGK
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計編集
      *****************************************************************
       3201-GOKEI-SEC                SECTION.
      *
           IF    IDX1    >    20
             MOVE     A05-KBN       TO    WRK-KBN-OLD
             PERFORM  330-PRINT-OUT-SEC
             PERFORM  310-HEAD-HEN-SEC
             MOVE    SPACE          TO    A05-KBN
             MOVE    WRK-KBN-OLD    TO    A05-KBN
             MOVE    1    TO    IDX1
           END-IF
      *
           MOVE    "合    計"     TO    A05-NAME(IDX1)
      *
      *    日数
           MOVE    SUM-NISSU          TO      WRK-Z4
           MOVE    WRK-Z4             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-NISSU(IDX1)
      *
      *    点数
           MOVE    SUM-TENSU          TO      WRK-Z9
           MOVE    WRK-Z9             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-TENSU(IDX1)
      *
      *    薬剤一部負担金
           MOVE    SUM-YKZFTN         TO      WRK-Z7
           MOVE    WRK-Z7             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-YKZFTN(IDX1)
      *
      *    一部負担金
           MOVE    SUM-ITBFTN         TO      WRK-Z7
           MOVE    WRK-Z7             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-ITBFTN(IDX1)
      *
      *    食事回数
           MOVE    SUM-SNISSU         TO      WRK-Z4
           MOVE    WRK-Z4             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-SNISSU(IDX1)
      *
      *    食事療養費
           MOVE    SUM-SKINGK         TO      WRK-Z9
           MOVE    WRK-Z9             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-SKINGK(IDX1)
      *
      *    標準負担額
           MOVE    SUM-SFTNGK         TO      WRK-Z7
           MOVE    WRK-Z7             TO      WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT      TO      A05-SFTNGK(IDX1)
      *
           PERFORM  410-CSV-DATA-SEC
      *
           INITIALIZE    SUM-AREA
      *
           .
       3201-GOKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理2
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
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
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "A21011A05V02.red"   TO  SPRT-PRTID
           MOVE    "保留・再請求・月遅れリスト"  TO  SPRT-TITLE
           MOVE    A05                 TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           END-IF
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
      *    対象年月
           INITIALIZE  WRK-ITEM
           STRING  "対象年月："              DELIMITED  BY  SIZE
                   A05-SKYYM                 DELIMITED  BY  SPACE
                                             INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    出力区分
           INITIALIZE  WRK-ITEM
           STRING  "出力区分："              DELIMITED  BY  SIZE
                   A05-KBN                   DELIMITED  BY  SPACE
                                             INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    作成年月日
           INITIALIZE  WRK-ITEM
           STRING  A05-SYSYMD                DELIMITED  BY  SPACE
                   "  発行"                  DELIMITED  BY  SIZE
                                             INTO  WRK-ITEM
           END-STRING
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称
           PERFORM  VARYING  IDX8  FROM  1  BY  1
                    UNTIL    IDX8  >  CSV-HEAD-MAX
      *
             INITIALIZE  WRK-ITEM
             MOVE    CSV-HEAD-ITEM(IDX8)     TO  WRK-ITEM
      *
             IF  IDX8  =  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
           END-PERFORM
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV明細編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    患者番号
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-PTNUM              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者氏名
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-PTNAME             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者番号
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-HKNJANUM           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分
           INITIALIZE  WRK-ITEM
           EVALUATE  A05TMP-NYUGAIKBN
           WHEN "1"
             MOVE    "入院"                    TO  WRK-ITEM
           WHEN "2"
             MOVE    "外来"                    TO  WRK-ITEM
           END-EVALUATE
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    本人・家族
           INITIALIZE  WRK-ITEM
           EVALUATE  A05TMP-HONKZKKBN
           WHEN "1"
             MOVE    "本"                      TO  WRK-ITEM
           WHEN "2"
             MOVE    "家"                      TO  WRK-ITEM
           WHEN OTHER
             MOVE    SPACE                     TO  WRK-ITEM
           END-EVALUATE
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険番号
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-HKNNUM             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    老人保険番号
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-RJNHKNNUM          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    給付割合
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-KYURATE            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    公費１
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-KOHNUM1            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    公費２
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-KOHNUM2            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    日数
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-NISSU              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    点数
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-TENSU              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    薬剤一部負担金
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-YKZFTN             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    一部負担金
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-ITBFTN             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事回数
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-SNISSU             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事療養費
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-SKINGK             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    標準負担額
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-SFTNGK             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療年月
           INITIALIZE  WRK-ITEM
           MOVE    A05TMP-SRYYM              TO  WRK-SYMD(1:6)
           MOVE    "01"                      TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)         TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
      *
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV明細編集処理
      *****************************************************************
       410-CSV-DATA-SEC            SECTION.
      *
      *    患者番号
           INITIALIZE  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者氏名
           INITIALIZE  WRK-ITEM
           MOVE    "合計"                    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者番号
           INITIALIZE  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    給付割合
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    本人・家族
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険番号
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    老人保険番号
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    公費１
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    公費２
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    日数
           INITIALIZE  WRK-ITEM
           MOVE    SUM-NISSU                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    点数
           INITIALIZE  WRK-ITEM
           MOVE    SUM-TENSU                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    薬剤一部負担金
           INITIALIZE  WRK-ITEM
           MOVE    SUM-YKZFTN                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    一部負担金
           INITIALIZE  WRK-ITEM
           MOVE    SUM-ITBFTN                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事回数
           INITIALIZE  WRK-ITEM
           MOVE    SUM-SNISSU                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事療養費
           INITIALIZE  WRK-ITEM
           MOVE    SUM-SKINGK                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    標準負担額
           INITIALIZE  WRK-ITEM
           MOVE    SUM-SFTNGK                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療年月
           INITIALIZE  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           IF  FLG-PRINT  NOT =  1
             INITIALIZE  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
             PERFORM  230-TOUKEICSV-SEC
             PERFORM  800-CSV-HEAD-SEC
           END-IF
      *
           .
      *
       410-CSV-DATA-EXT.
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
       801-CSVDATA-EDIT-EXT.
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
           PERFORM  VARYING  IDX10  FROM  1  BY  1
                    UNTIL  IDX10  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX10:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX10:1)  NOT =  SPACE
                 MOVE    IDX10      TO  IDX-ST
                 MOVE    1          TO  IDX-ED
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
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID      TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM        TO  STOUKEICSV-SRYYM
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"               TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM               DELIMITED  BY  SIZE
                   "horyu_saiseikyu_tukiokure_"
                                             DELIMITED  BY  SIZE
                   WRK-PARA-SKYYM            DELIMITED  BY  SPACE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "保留・再請求・月遅れリスト"
                                             TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"           USING
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
           INITIALIZE                        WRK-CSVDATA
           MOVE    1                         TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  A05TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT  AT  END
                   MOVE  A05TMP-SYUTURYOKU    TO  KEY-N-SYUTURYOKU
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    請求管理読込
      ******************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                   RECE10-REC
           MOVE   SPA-HOSPNUM           TO  RECE10-HOSPNUM
           MOVE   WRK-PARA-SKYYM        TO  RECE10-SKYYM
           MOVE   RECE10-REC            TO  MCPDATA-REC
           MOVE   "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF     MCP-RC        =       ZERO
                  MOVE    ZERO                TO    FLG-END
                  PERFORM  900-SEIKYU-READ-SEC
           ELSE
                  MOVE    1    TO      FLG-END
           END-IF
           .
      *
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           INITIALIZE  FLG-KOHI
                       FLG-END2
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF       MCP-RC  =     ZERO
                    INITIALIZE                    RECE10-REC
                    MOVE    MCPDATA-REC   TO      RECE10-REC
      *
      *    テスト患者は対象外
                    PERFORM  900-PTINF-READ-SEC
                    IF      FLG-PTINF           =   ZERO
                      AND     PTINF-TSTPTNUMKBN   =   "1"
                        GO    TO    900-SEIKYU-READ-SEC
                    END-IF
      *
                    PERFORM  9001-CHK-SEC
      *
      *             印字対象でなければ、公費請求情報を参照
                    IF  FLG-TYPE  =  ZERO
                      PERFORM  900-KOHSKY-READ-SEC
                      PERFORM  9002-CHK-SEC
                      MOVE    1             TO  FLG-KOHI
                    END-IF
      *
                    IF  FLG-TYPE NOT  =  ZERO
                      ADD    1    TO    CNT-RECE10
                    ELSE
                      GO    TO    900-SEIKYU-READ-SEC
                    END-IF
           ELSE
                    MOVE    1             TO      FLG-END
           END-IF
           .
      *
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求情報クローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費請求書マスタ読込
      ******************************************************************
       900-KOHSKY-READ-SEC            SECTION.
      *
           INITIALIZE                    RECE02-REC
           MOVE    RECE10-HOSPNUM        TO  RECE02-HOSPNUM
           MOVE    RECE10-SRYYM          TO  RECE02-SRYYM
           MOVE    RECE10-NYUGAIKBN      TO  RECE02-NYUGAIKBN
           MOVE    RECE10-PTID           TO  RECE02-PTID
           MOVE    RECE10-RECEKA         TO  RECE02-RECEKA
           MOVE    RECE10-TEISYUTUSAKI   TO  RECE02-TEISYUTUSAKI
           MOVE    RECE10-RECESYUBETU    TO  RECE02-RECESYUBETU
           MOVE    RECE10-HKNJANUM       TO  RECE02-HKNJANUM-KEY
           MOVE    RECE10-HOJOKBN-KEY    TO  RECE02-HOJOKBN-KEY
           MOVE    RECE10-TEKSTYMD       TO  RECE02-TEKSTYMD
           MOVE    RECE10-KOHID-KEY      TO  RECE02-KOHID-KEY
           MOVE    RECE10-RECEIPT-KBN    TO  RECE02-RECEIPT-KBN
           MOVE    RECE02-REC            TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"             TO  MCP-FUNC
             MOVE    "tbl_kohsky"          TO  MCP-TABLE
             MOVE    "key"                 TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC           TO  RECE02-REC
               MOVE    ZERO                  TO  FLG-END2
             ELSE
               INITIALIZE                        RECE02-REC
                MOVE     1                   TO  FLG-END2
             END-IF
           ELSE
             MOVE    1                     TO  FLG-END2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理対象判定処理(出力区分)
      *****************************************************************
       9001-CHK-SEC                 SECTION.
      *
      *    保留判定
           MOVE  ZERO            TO      FLG-TYPE
           IF    WRK-PARA-SYUTURYOKU  =  "1" OR "9"
             IF    RECE10-SKYKBN      =  "0"
               MOVE    1         TO      FLG-TYPE
             END-IF
           END-IF
      *
      *    月遅れ判定
           IF    FLG-TYPE               =  ZERO
             IF    WRK-PARA-SYUTURYOKU  =  "2" OR "9"
               IF    RECE10-SKYYM       =    WRK-PARA-SKYYM   AND
                     RECE10-SKYKBN      =    "1"              AND
                     RECE10-SRYYM  NOT  =    "000000"         AND
                     RECE10-HENREIYM    =    "000000"
                     MOVE     2         TO   FLG-TYPE
               END-IF
             END-IF
           END-IF
      *
      *    再請求判定
           IF    FLG-TYPE               =  ZERO
             IF    WRK-PARA-SYUTURYOKU  =  "3" OR "9"
               IF    RECE10-SKYYM       =    WRK-PARA-SKYYM   AND
                     RECE10-SKYKBN      =    "1"              AND
                     RECE10-SRYYM  NOT  =    "000000"         AND
                     RECE10-HENREIYM NOT   =    "000000"
                     MOVE     3         TO   FLG-TYPE
               END-IF
             END-IF
           END-IF
      *
          .
       9001-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求管理対象判定処理(出力区分)
      *****************************************************************
       9002-CHK-SEC                 SECTION.
      *
      *    保留判定
           MOVE  ZERO            TO      FLG-TYPE
           IF    WRK-PARA-SYUTURYOKU       =    "1" OR "9"
             IF    RECE02-SKYKBN           =    "0"
               MOVE    1                   TO   FLG-TYPE
             END-IF
           END-IF
      *
      *    月遅れ判定
           IF    FLG-TYPE                  =    ZERO
             IF    WRK-PARA-SYUTURYOKU     =    "2" OR "9"
               IF    RECE02-SKYYM          =    WRK-PARA-SKYYM   AND
                     RECE02-SKYKBN         =    "1"              AND
                     RECE02-SRYYM  NOT     =    "000000"         AND
                     RECE02-HENREIYM       =    "000000"
                     MOVE     2            TO   FLG-TYPE
               END-IF
             END-IF
           END-IF
      *
      *    再請求判定
           IF    FLG-TYPE                  =    ZERO
             IF    WRK-PARA-SYUTURYOKU     =    "3" OR "9"
               IF    RECE02-SKYYM          =    WRK-PARA-SKYYM   AND
                     RECE02-SKYKBN         =    "1"              AND
                     RECE02-SRYYM     NOT  =    "000000"         AND
                     RECE02-HENREIYM  NOT  =    "000000"
                     MOVE     3            TO   FLG-TYPE
               END-IF
             END-IF
           END-IF
      *
          .
       9002-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納情報セット
      *****************************************************************
       910-PTKOHINF-READ-SEC  SECTION.
      *
           INITIALIZE                   PTKOHINF-REC
           MOVE    RECE10-HOSPNUM       TO   PTKOH-HOSPNUM
           MOVE    RECE10-PTID          TO   PTKOH-PTID
           MOVE    RECE10-KOHID(IDS)    TO   PTKOH-KOHID
           MOVE    PTKOHINF-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_ptkohinf"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"            TO   MCP-FUNC
             MOVE    "tbl_ptkohinf"       TO   MCP-TABLE
             MOVE    "key"                TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC          TO   PTKOHINF-REC
               MOVE    ZERO                 TO   FLG-PTKOHINF
             ELSE
               INITIALIZE                   PTKOHINF-REC
               MOVE    1                    TO   FLG-PTKOHINF
             END-IF
           ELSE
             INITIALIZE                   PTKOHINF-REC
             MOVE    1                    TO   FLG-PTKOHINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_ptkohinf"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           MOVE   WRK-SYUYMD         TO   SYU-SRYYMD
           MOVE   SYUNOU-REC         TO   MCPDATA-REC
           MOVE   "DBSELECT"         TO   MCP-FUNC
           MOVE   "tbl_syunou"       TO   MCP-TABLE
           MOVE   "key14"            TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"        USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE   ZERO            TO   FLG-SYUNOU
           ELSE
             MOVE    1              TO   FLG-SYUNOU
           END-IF
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
           MOVE    "tbl_syunou"     TO   MCP-TABLE
           MOVE    "key14"          TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE   MCPDATA-REC     TO   SYUNOU-REC
             MOVE   ZERO            TO   FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE    1              TO   FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    収納情報クローズ
      *****************************************************************
       940-SYUNOU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key14"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       940-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       900-NYUINRRK-SET-SEC  SECTION.
      *
           MOVE   NYUINRRK-REC       TO   MCPDATA-REC
           MOVE   "DBSELECT"         TO   MCP-FUNC
           MOVE   "tbl_ptnyuinrrk"   TO   MCP-TABLE
           MOVE   "key19"            TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"        USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE   ZERO            TO   FLG-NYUINRRK
           ELSE
             MOVE    1              TO   FLG-NYUINRRK
           END-IF
           .
       900-NYUINRRK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-NYUINRRK-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO   MCP-TABLE
           MOVE    "key19"          TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE   MCPDATA-REC     TO  NYUINRRK-REC
             MOVE   ZERO            TO   FLG-NYUINRRK
           ELSE
             INITIALIZE  NYUINRRK-REC
             MOVE    1              TO   FLG-NYUINRRK
           END-IF
      *
           .
       900-NYUINRRK-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    収納情報クローズ
      *****************************************************************
       900-NYUINRRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"         TO   MCP-TABLE
           MOVE    "key19"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYUINRRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY7-AREA
           MOVE    "71"                   TO   LNK-DAY7-IRAI
           MOVE    WRK-SYSYMD            TO   LNK-DAY7-YM
           CALL    "ORCSDAY"              USING
                                          STS-AREA-DAY
                                          LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI        TO   WRK-SYSYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(レセプト印刷順情報取得)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    LNK-PRTKANRI-SKYYMD  TO  ORC-DBYMD
           MOVE    SYS-2003-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
          IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(レセプト印刷順情報取得)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-2003-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理(レセプト印刷順情報取得)
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
           .
       31012-SEIWA-HEN-EXT.
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
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
      *
      *        ジョブ終了処理
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
           CLOSE   RECEERR-FILE
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
           .
       900-DBCLOSE-EXT.
           EXIT.
