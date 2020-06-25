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
       PROGRAM-ID.     A00000KOHS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 償還払公費患者一覧表
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/04/07    有村        新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/02/09  集計処理を修正
      *  01.00.03    @@@@-門間    11/02/21  収納情報取得条件を修正
      *  01.00.04    @@@@-門間    11/08/04  実日数の値が正しく集計されるように修正
      *                                     本人負担額に端数が表示されないように修正
      *                                     同患者にて公費1と公費2を使用した場合、
      *                                     印字されるように修正
      *  01.00.05    @@@@-門間    11/08/22  実日数(外来)は公費請求テーブルを参照するように変更
      *  01.00.06    @@@@-門間    11/10/18  同保険、同公費でレセプトが複数存在している場合、
      *                                     正しく集計されるように修正
      *  01.00.07    @@@@-門間    11/12/16  公費の件数を集計するように修正
      *                                     第2公費が印字対象でない場合、印字対象である
      *                                     第3公費が集計されない不具合を修正
      *  01.00.08    @@@@-門間    12/03/21  返戻患者の公費適用期間が「請求年月」以前に終了している場合、
      *                                     公費番号、負担者番号、受給者番号が印字されない。
      *  01.00.09    @@@@-門間    12/05/25  CSVファイル作成機能追加
      *  01.00.10    @@@@-門間    12/07/12  複数科保険(保険組合せ)でまとめ入力を行った時の不具合を修正
      *  01.00.11    @@@@-門間    12/08/10  保険者番号が不明な場合、患者名が正しく印字されない不具合を修正
      *  01.00.12    @@@@-門間    13/01/23  テーブルのprimary key変更対応
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    17/08/03  旧ＣＳＶ出力機能を削除
      *  02.00.03    @@@@-門間    18/01/30  保険番号情報取得時の不具合修正
      *  02.00.04    @@@@-門間    19/08/30  月途中で負担者及び受給者番号が変わった場合、
      *                                     分かれて印字されるように修正
      *  02.00.05    @@@@-萩野    20/03/23  実日数の集計処理を修正
      *****************************************************************
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE    ASSIGN    MF100T
                                 ORGANIZATION    IS  INDEXED
                                 ACCESS  MODE    IS  DYNAMIC
                                 RECORD  KEY     IS  MF100-KEY
                                 FILE    STATUS  IS  STS-MF100.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE  ASSIGN  RECEERR
                                 FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "A00000KOHSTMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    レセプト明細ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //MF100T//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    帳票コピー句
           COPY    "A00000KOHSV02.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100            PIC X(02).
           03  STS-RECE02           PIC X(02).
           03  STS-RECEERR          PIC X(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY           PIC 9(02).
               05  SYS-MM           PIC 9(02).
               05  SYS-DD           PIC 9(02).
           03  SYS-TIME             PIC 9(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END              PIC 9(01).
           03  FLG-PRINT            PIC 9(01).
           03  FLG-SYSKANRI         PIC 9(01).
           03  FLG-SYUNOU           PIC 9(01).
           03  FLG-HKNNUM           PIC 9(01).
           03  FLG-PTINF            PIC 9(01).
           03  FLG-PTKOHINF         PIC 9(01).
           03  FLG-PTHKNINF         PIC 9(01).
           03  FLG-RECE10           PIC 9(01).
           03  FLG-OK               PIC 9(01).
           03  FLG-KBN              PIC 9(01).
           03  FLG-KOHCHK           PIC 9(01).
           03  FLG-KOHCHK2          PIC 9(01).
           03  FLG-HKNCOMBI         PIC 9(01).
           03  FLG-INVALID          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                 PIC 9(04).
           03  IDX2                 PIC 9(04).
           03  IDX3                 PIC 9(04).
           03  IDX4                 PIC 9(04).
           03  IDX5                 PIC 9(04).
           03  IDX                  PIC 9(04).
           03  IDX6                 PIC 9(04).
           03  IDY                  PIC 9(04).
           03  IDZ                  PIC 9(04).
           03  IDX-PG               PIC 9(02).
           03  IDX7                 PIC 9(04).
           03  IDX8                 PIC 9(02).
           03  IDX9                 PIC 9(04).
      *
           03  IDX10             PIC 9(04).
           03  IDX11             PIC 9(05).
           03  IDX12             PIC 9(02).
           03  IDX-ST            PIC 9(03).
           03  IDX-ED            PIC 9(03).
           03  IDXP              PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID       PIC 9(07).
           03  WRK-PARA-SHELLID     PIC X(08).
           03  WRK-PARA-HOSPNUM     PIC 9(02).
           03  WRK-PARA-PAGE        PIC 9(10).
           03  WRK-PARA-SRYYM       PIC X(06).
           03  WRK-PARA-PRTSYMD     PIC X(08).
           03  WRK-PARA-PRTLYMD     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR   PIC X(200).
           03  WRK-SYMD.
               05  WRK-SYY          PIC 9(04).
               05  WRK-SMM          PIC 9(02).
               05  WRK-SDD          PIC 9(02).
           03  WRK-SRYYMWH          PIC X(16).
           03  WRK-HENYMDG          PIC X(22).
           03  WRK-HENYMDG1         PIC X(09).
           03  WRK-KOHNUM           PIC X(03).
           03  WRK-TEKSTYMD         PIC X(08).
           03  WRK-TEKEDYMD         PIC X(08).
           03  WRK-TAHO             PIC X(20).
      *
           03  WRK-CITYNAME1        PIC X(60).
           03  WRK-CITYNAME2        PIC X(80).
           03  WRK-MOJI1            PIC 9(02).
      *
           03  WRK-MAE-INPUT        PIC X(200).
           03  WRK-OUT-INPUT        PIC X(200).
      *
           03  FILE-NAME            PIC X(512).
           03  WRK-PATH             PIC X(64).
      *
           03  WRK-Z2               PIC ZZ.
           03  WRK-Z3               PIC ZZZ.
           03  WRK-Z9               PIC Z,ZZZ,ZZZ.
           03  WRK-SRYYMD           PIC 9(08).
           03  FLG-KOHSKY           PIC 9(01).
           03  WRK-SRYKANAME        PIC X(10).
           03  WRK-REC-LENGTH       PIC 9(04).
           03  IDW                  PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA         PIC X(100).
               05  WRK-NAGASA       PIC 9(04).
           03  WRK-SKYMONEY         PIC 9(09).
           03  WRK-NISSU1           PIC 9(02).
           03  WRK-NISSU2           PIC 9(02).
      *
           03  WRK-JIHI1            PIC S9(09).
           03  WRK-JIHI2            PIC S9(09).
      *
           03  WRK-KENSU            PIC 9(03).
      *
       01  PGOPTION-AREA.
           03  CITY-CITYNAME        PIC X(40).
           03  KOHS-KAIPAGE         PIC 9(01).
           03  KOHI-DATA-TBL        OCCURS 40.
               05  KOHI-KOHNUM      PIC X(03).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE10           PIC 9(06).
           03  CNT-MF100            PIC 9(06).
           03  CNT-PAGE             PIC 9(06).
      *    改ページKEY領域
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-KOHNUM     PIC X(03).
           03  KEY-OLD.
               05  KEY-O-KOHNUM     PIC X(03).
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-KOHI          PIC X(50) VALUE "公費名".
               07  CSV-HEAD-PTID          PIC X(50) VALUE "患者ＩＤ".
               07  CSV-HEAD-KANANAME      PIC X(50) VALUE "カナ氏名".
               07  CSV-HEAD-PTNAME        PIC X(50) VALUE "患者氏名".
               07  CSV-HEAD-FTNJANUM      PIC X(50) VALUE "負担者番号".
               07  CSV-HEAD-JKYSNUM       PIC X(50) VALUE "受給者番号".
               07  CSV-HEAD-SRYYM         PIC X(50) VALUE "診療年月".
               07  CSV-HEAD-SEX           PIC X(50) VALUE "性別".
               07  CSV-HEAD-NYUGAIKBN     PIC X(50) VALUE "入外区分".
               07  CSV-HEAD-HKNJANUM      PIC X(50) VALUE "保険者番号".
               07  CSV-HEAD-KIGO          PIC X(50) VALUE "記号".
               07  CSV-HEAD-BANGO         PIC X(50) VALUE "番号".
               07  CSV-HEAD-NISSU         PIC X(50) VALUE "実日数".
               07  CSV-HEAD-TENSU         PIC X(50) VALUE "点数".
               07  CSV-HEAD-FTNMONEY      PIC X(50) VALUE "本人負担額".
               07  CSV-HEAD-KAISU         PIC X(50) VALUE "回数".
               07  CSV-HEAD-SKJ-SKYMONEY  PIC X(50) VALUE "請求額".
               07  CSV-HEAD-SKJ-FTNMONEY  PIC X(50) VALUE "標準負担額".
               07  CSV-HEAD-SRYKANAME     PIC X(50) VALUE "診療科名".
               07  CSV-HEAD-KYURATE       PIC X(50) VALUE "給付割合".
               07  CSV-HEAD-TAHOKOHI      PIC X(50) VALUE "他法公費".
               07  CSV-HEAD-GOKEI         PIC X(50) VALUE "合計件数".
      *
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 22.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE  22.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY  "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY  "CPSK1005.INC".
      *
      *    請求管理
           COPY  "CPRCF010.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY  "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY  "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY  "CPPTHKNINF.INC".
           COPY  "CPKOHSKY.INC"
                 REPLACING  //KOHSKY//
                 BY         //RECE02//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ"
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
       LINKAGE  SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *
      ****************************************************************
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
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  KEY-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
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
      *
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SRYYM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
           PERFORM  110-INI-FILE-SEC
      *
      *    ステップ開始処理
           MOVE    "STS"              TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
           MOVE    "A00000KOHS"       TO  JOB-PGID
           MOVE    "償還払公費一覧表"      TO   JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "MF100TP"                TO  MF100T-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  MF100T-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  MF100T-HOSPNUM
      *
           MOVE    MF100T-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  MF100T-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    請求年月の和暦を取得
           MOVE    WRK-PARA-SRYYM     TO   WRK-SYMD (1:6)
           MOVE    "01"               TO   WRK-SYMD (7:2)
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)  TO   WRK-SRYYMWH
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM     TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD           TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日
           MOVE    WRK-PARA-SRYYM     TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"               TO   WRK-PARA-PRTSYMD(7:2)
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE    "1001"             TO   SYS-1001-KANRICD
           MOVE    "*"                TO   SYS-1001-KBNCD
           MOVE    WRK-PARA-PRTSYMD   TO   SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO   SYS-1001-STYUKYMD
           MOVE    SPA-HOSPNUM        TO   SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC       TO   MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC        TO   SYS-1001-REC
           ELSE
             MOVE  1  TO  FLG-END
           END-IF
      *
           OPEN  OUTPUT MF100-FILE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    INIファイル読み込み処理
      *****************************************************************
       110-INI-FILE-SEC                SECTION.
      *
      *    プログラムオプション取込み
           INITIALIZE                        ORCSPRGOPTIONAREA
                                             PGOPTION-AREA
           MOVE    "A00000KOHS"      TO      ORCSPRGOPTION-PRGID
      *    自治体名
           MOVE    "JICHI"           TO      ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"   USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA  TO  CITY-CITYNAME
           END-IF
      *
      *    改ページ
           MOVE    "PAGE"            TO      ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"   USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA  TO  KOHS-KAIPAGE
           END-IF
      *
      *    償還払公費
           MOVE    "KOH"             TO      ORCSPRGOPTION-IN-DATA
           PERFORM  VARYING  IDX-PG  FROM  1  BY  1
                    UNTIL    IDX-PG  >    40
             MOVE   IDX-PG   TO      ORCSPRGOPTION-IN-DATA(4:2)
             CALL   "ORCSPRGOPTION"  USING   SPA-AREA
                                                 ORCSPRGOPTIONAREA
             IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
               MOVE  ORCSPRGOPTION-OT-PARA  TO  KOHI-KOHNUM(IDX-PG)
             ELSE
               MOVE  41                     TO  IDX-PG
             END-IF
           END-PERFORM
      *
           .
       110-INI-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
      *    データ抽出処理
           PERFORM  2001-SYUKEI-SEC
      *
      *    データ印刷処理
           IF    CNT-MF100    >    ZERO
             PERFORM  2003-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           PERFORM  900-KOHSKY-SELECT-SEC
           PERFORM  UNTIL  FLG-KOHSKY  =  1
      *
             PERFORM  20021-KOHCHK-SELECT-SEC
             PERFORM  900-KOHSKY-FETCH-SEC
      *
           END-PERFORM
      *
           PERFORM  900-KOHSKY-CLOSE-SEC
      *
           CLOSE   MF100-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ編集処理
      *****************************************************************
       2002-HENSYU-SEC  SECTION.
      *
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           INITIALIZE  MF100-REC
           MOVE    RECE10-HOSPNUM        TO   MF100-HOSPNUM
           MOVE    PTKOH-KOHNUM          TO   MF100-KOHNUM-KEY
           MOVE    RECE10-PTNUM          TO   MF100-PTNUM
           MOVE    RECE10-SRYYM          TO   MF100-SRYYM
           MOVE    RECE10-NYUGAIKBN      TO   MF100-NYUGAIKBN
           MOVE    RECE10-RECEKA         TO   MF100-SRYKA
           MOVE    RECE10-TEISYUTUSAKI   TO   MF100-TEISYUTUSAKI
           MOVE    RECE10-RECESYUBETU    TO   MF100-RECESYUBETU
           MOVE    RECE10-HKNJANUM       TO   MF100-HKNJANUM
           MOVE    RECE10-TEKSTYMD       TO   MF100-TEKSTYMD
      *
           MOVE    PTKOH-FTNJANUM        TO   MF100-FTNJANUM
           MOVE    PTKOH-JKYSNUM         TO   MF100-JKYSNUM
      *
      *    一時ファイル読込
           PERFORM  700-TMP-READ-SEC
      *
      *    データ設定
           MOVE    RECE10-PTID           TO   MF100-PTID
           MOVE    PTINF-NAME            TO   MF100-NAME
           MOVE    PTINF-KANANAME        TO   MF100-FURIGANA
           MOVE    PTINF-SEX             TO   MF100-SEX
           MOVE    RECE10-KIGO           TO   MF100-KIGO
           MOVE    RECE10-NUM            TO   MF100-NUM
           MOVE    RECE10-KYURATE        TO   MF100-KYURATE
           MOVE    RECE10-RJNHKNNUM      TO   MF100-RJNHKNNUM
      *
           MOVE    PTKOH-KOHNUM          TO   MF100-KOHNUM
      *
      *    公費名取得
           MOVE    MF100-KOHNUM              TO   WRK-KOHNUM
           MOVE    PTKOH-TEKSTYMD            TO   WRK-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD            TO   WRK-TEKEDYMD
           PERFORM  950-HKNNUM-SET-SEC
           MOVE    HKN-TANSEIDONAME          TO   MF100-KOHNAME
      *
      *    他法公費情報設定
           MOVE    1                         TO   IDX1
           IF   RECE10-RJNHKNNUM  NOT =  SPACE
             MOVE   "27"                     TO   MF100-TAHO(IDX1)
             ADD    1                        TO   IDX1
           END-IF
      *
           IF   RECE10-ETC-KOHNUM(1)   =   "972"
               OR  RECE10-ETC-KOHNUM(2)   =   "972"
               OR  RECE10-ETC-KOHNUM(3)   =   "972"
               OR  RECE10-ETC-KOHNUM(4)   =   "972"
             MOVE  "長"                      TO   MF100-TAHO(IDX1)
             ADD   1                         TO   IDX1
           END-IF
      *
           PERFORM  VARYING  IDX2   FROM    1   BY  1
                    UNTIL    IDX2   >   4
                    OR   IDX1   >   4
                    OR   RECE10-KOHNUM(IDX2)  =  SPACE
             IF     RECE10-KOHNUM(IDX2)(1:1)  =  "0"
               MOVE     RECE10-KOHNUM(IDX2)  TO   WRK-KOHNUM
               MOVE     RECE10-SRYYM         TO   WRK-TEKSTYMD(1:6)
                                                  WRK-TEKEDYMD(1:6)
               MOVE     "01"                 TO   WRK-TEKSTYMD(7:2)
               MOVE     "31"                 TO   WRK-TEKEDYMD(7:2)
               PERFORM  950-HKNNUM-SET-SEC
               MOVE     HKN-HBTNUM           TO   MF100-TAHO(IDX1)
               ADD      1                    TO   IDX1
             END-IF
           END-PERFORM
      *
      *    公費請求テーブルより、実日数(外来)を集計
           IF  RECE02-NYUGAIKBN  =  "2"
      *      実日数(外来)
             MOVE    RECE02-JNISSU(IDX3 + 1)   TO   MF100-JNISSU
           END-IF
      *
      *    収納テーブルより、実日数、点数、本人負担額、回数(食事療養費)、
      *    請求額(食事療養費)、負担額(食事療養費)を集計
           PERFORM  910-SYUNOU-SELECT-SEC
           PERFORM  UNTIL  FLG-SYUNOU  =  1
             IF        SYU-DENPJTIKBN       NOT =  "3"
               AND     RECE02-HKNCOMBI(IDX8)        =  SYU-HKNCOMBINUM
               AND  (( RECE02-KOHNUMHC(IDX8 IDX7)   =  SYU-KOH1HKNNUM
               AND     SYU-KOH1HKNNUM       NOT =  SPACE )
               OR    ( RECE02-KOHNUMHC(IDX8 IDX7)   =  SYU-KOH2HKNNUM
               AND     SYU-KOH2HKNNUM       NOT =  SPACE )
               OR    ( RECE02-KOHNUMHC(IDX8 IDX7)   =  SYU-KOH3HKNNUM
               AND     SYU-KOH3HKNNUM       NOT =  SPACE )
               OR    ( RECE02-KOHNUMHC(IDX8 IDX7)   =  SYU-KOH4HKNNUM
               AND     SYU-KOH4HKNNUM       NOT =  SPACE ))
               AND     RECE02-SRYYM             =  SYU-SRYYMD(1:6)
      *
               IF  SYU-NYUGAIKBN  =  "1"
      *
      *          実日数(入院)
                 MOVE  SYU-SKYSTYMD(7:2)    TO  WRK-NISSU1
                 MOVE  SYU-SKYEDYMD(7:2)    TO  WRK-NISSU2
                 COMPUTE  WRK-NISSU1  =  (WRK-NISSU2  -  WRK-NISSU1)
                                      +  1
                 ADD    WRK-NISSU1          TO  MF100-JNISSU
      *          点数(入院)
                 ADD    SYU-TOTAL-HKNTEN    TO  MF100-TOTALTEN
      *
      *          回数(食事療養費)、請求額(食事療養費)、負担額(食事療養費)
                 IF   SYU-KOH1HKNNUM  =  MF100-KOHNUM
                   ADD   SYU-KOH1SKJ-NISSU    TO  MF100-SKJKAISU
                   ADD   SYU-KOH1SKJ-RYOYOHI  TO  MF100-SKJSKY
                   ADD   SYU-KOH1SKJ-FTN      TO  MF100-SKJFTN
                 ELSE
                   IF  SYU-KOH2HKNNUM  =  MF100-KOHNUM
                     ADD   SYU-KOH2SKJ-NISSU    TO  MF100-SKJKAISU
                     ADD   SYU-KOH2SKJ-RYOYOHI  TO  MF100-SKJSKY
                     ADD   SYU-KOH2SKJ-FTN      TO  MF100-SKJFTN
                   ELSE
                     IF  SYU-KOH3HKNNUM  =  MF100-KOHNUM
                       ADD   SYU-KOH3SKJ-NISSU    TO  MF100-SKJKAISU
                       ADD   SYU-KOH3SKJ-RYOYOHI  TO  MF100-SKJSKY
                       ADD   SYU-KOH3SKJ-FTN      TO  MF100-SKJFTN
                     ELSE
                       IF  SYU-KOH4HKNNUM  =  MF100-KOHNUM
                         ADD   SYU-KOH4SKJ-NISSU    TO  MF100-SKJKAISU
                         ADD   SYU-KOH4SKJ-RYOYOHI  TO  MF100-SKJSKY
                         ADD   SYU-KOH4SKJ-FTN      TO  MF100-SKJFTN
                       ELSE
                         ADD   SYU-SYUSKJ-NISSU     TO  MF100-SKJKAISU
                         ADD   SYU-SYUSKJ-RYOYOHI   TO  MF100-SKJSKY
                         ADD   SYU-SYUSKJ-FTN       TO  MF100-SKJFTN
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *
               ELSE
      *
      *          点数(外来)
                 ADD    SYU-TOTAL-HKNTEN         TO   MF100-TOTALTEN
      *
               END-IF
      *
      *        本人負担額、自費端数処理
               INITIALIZE  WRK-JIHI1
                           WRK-JIHI2
      *
               EVALUATE  SYS-1001-DISCOUNT-HASU-KBN2J
               WHEN  1
                 COMPUTE  WRK-JIHI1 ROUNDED = SYU-JIHI-TOTAL / 10
                 COMPUTE  WRK-JIHI1 = WRK-JIHI1 * 10
                 COMPUTE  WRK-JIHI2 ROUNDED = SYU-JIHI-TOTAL-TAX / 10
                 COMPUTE  WRK-JIHI2 = WRK-JIHI2 * 10
               WHEN  2
                 COMPUTE  WRK-JIHI1 = SYU-JIHI-TOTAL / 10
                 COMPUTE  WRK-JIHI1 = WRK-JIHI1 * 10
                 COMPUTE  WRK-JIHI2 = SYU-JIHI-TOTAL-TAX / 10
                 COMPUTE  WRK-JIHI2 = WRK-JIHI2 * 10
               WHEN  3
                 MOVE     SYU-JIHI-TOTAL      TO  WRK-JIHI1
                 MOVE     SYU-JIHI-TOTAL-TAX  TO  WRK-JIHI2
                 IF   SYU-JIHI-TOTAL(7:1)      NOT =  0
                   COMPUTE  WRK-JIHI1 = SYU-JIHI-TOTAL / 10
                   COMPUTE  WRK-JIHI1 = WRK-JIHI1 * 10 + 10
                 END-IF
                 IF   SYU-JIHI-TOTAL-TAX(7:1)  NOT =  0
                   COMPUTE  WRK-JIHI2 = SYU-JIHI-TOTAL-TAX  /  10
                   COMPUTE  WRK-JIHI2 = WRK-JIHI2 * 10 + 10
                 END-IF
               WHEN  4
                 MOVE     SYU-JIHI-TOTAL      TO  WRK-JIHI1
                 MOVE     SYU-JIHI-TOTAL-TAX  TO  WRK-JIHI2
               WHEN  0
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN2
                 WHEN  1
                   COMPUTE  WRK-JIHI1 ROUNDED = SYU-JIHI-TOTAL / 10
                   COMPUTE  WRK-JIHI1 = WRK-JIHI1 * 10
                   COMPUTE  WRK-JIHI2 ROUNDED = SYU-JIHI-TOTAL-TAX / 10
                   COMPUTE  WRK-JIHI2 = WRK-JIHI2 * 10
                 WHEN  2
                   COMPUTE  WRK-JIHI1 = SYU-JIHI-TOTAL / 10
                   COMPUTE  WRK-JIHI1 = WRK-JIHI1 * 10
                   COMPUTE  WRK-JIHI2 = SYU-JIHI-TOTAL-TAX / 10
                   COMPUTE  WRK-JIHI2 = WRK-JIHI2 * 10
                 WHEN  3
                   MOVE     SYU-JIHI-TOTAL      TO  WRK-JIHI1
                   MOVE     SYU-JIHI-TOTAL-TAX  TO  WRK-JIHI2
                   IF   SYU-JIHI-TOTAL(7:1)      NOT =  0
                     COMPUTE  WRK-JIHI1 = SYU-JIHI-TOTAL / 10
                     COMPUTE  WRK-JIHI1 = WRK-JIHI1 * 10 + 10
                   END-IF
                   IF   SYU-JIHI-TOTAL-TAX(7:1)  NOT =  0
                     COMPUTE  WRK-JIHI2 = SYU-JIHI-TOTAL-TAX / 10
                     COMPUTE  WRK-JIHI2 = WRK-JIHI2 * 10 + 10
                   END-IF
                 WHEN  4
                   MOVE     SYU-JIHI-TOTAL      TO  WRK-JIHI1
                   MOVE     SYU-JIHI-TOTAL-TAX  TO  WRK-JIHI2
                 END-EVALUATE
               END-EVALUATE
      *
               COMPUTE  WRK-SKYMONEY      =    SYU-SKYMONEY
                                          -    WRK-JIHI1
                                          -    WRK-JIHI2
                                          -    SYU-TOTAL-TGMONEY
                                          -    SYU-TOTAL-TGMONEY-TAX
               ADD      WRK-SKYMONEY      TO   MF100-FTNMONEY
      *
             END-IF
             PERFORM  910-SYUNOU-FETCH-SEC
           END-PERFORM
      *
           PERFORM  910-SYUNOU-CLOSE-SEC
      *
           IF  FLG-INVALID  =  1
             WRITE    MF100-REC
             ADD     1        TO   CNT-MF100
           ELSE
             REWRITE  MF100-REC
           END-IF
      *
           .
       2002-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費チェック処理
      *****************************************************************
       20021-KOHCHK-SELECT-SEC             SECTION.
      *
      *    対象公費所持者判定
           INITIALIZE  MF100-REC
      *
           PERFORM  VARYING  IDX8   FROM    1   BY  1
                    UNTIL    IDX8   >   10
                    OR       RECE02-HKNCOMBI(IDX8)  =  SPACE
      *
             PERFORM  VARYING  IDX7   FROM    1   BY  1
                      UNTIL    IDX7   >   4
               IF  RECE02-KOHNUMHC(IDX8 IDX7)  NOT =  SPACE
                 IF  RECE02-KOHNUMHC(IDX8 IDX7)  =  KOHI-KOHNUM(1)
                   OR  KOHI-KOHNUM(2)    OR  KOHI-KOHNUM(3)
                   OR  KOHI-KOHNUM(4)    OR  KOHI-KOHNUM(5)
                   OR  KOHI-KOHNUM(6)    OR  KOHI-KOHNUM(7)
                   OR  KOHI-KOHNUM(8)    OR  KOHI-KOHNUM(9)
                   OR  KOHI-KOHNUM(10)   OR  KOHI-KOHNUM(11)
                   OR  KOHI-KOHNUM(12)   OR  KOHI-KOHNUM(13)
                   OR  KOHI-KOHNUM(14)   OR  KOHI-KOHNUM(15)
                   OR  KOHI-KOHNUM(16)   OR  KOHI-KOHNUM(17)
                   OR  KOHI-KOHNUM(18)   OR  KOHI-KOHNUM(19)
                   OR  KOHI-KOHNUM(20)   OR  KOHI-KOHNUM(21)
                   OR  KOHI-KOHNUM(22)   OR  KOHI-KOHNUM(23)
                   OR  KOHI-KOHNUM(24)   OR  KOHI-KOHNUM(25)
                   OR  KOHI-KOHNUM(26)   OR  KOHI-KOHNUM(27)
                   OR  KOHI-KOHNUM(28)   OR  KOHI-KOHNUM(29)
                   OR  KOHI-KOHNUM(30)   OR  KOHI-KOHNUM(31)
                   OR  KOHI-KOHNUM(32)   OR  KOHI-KOHNUM(33)
                   OR  KOHI-KOHNUM(34)   OR  KOHI-KOHNUM(35)
                   OR  KOHI-KOHNUM(36)   OR  KOHI-KOHNUM(37)
                   OR  KOHI-KOHNUM(38)   OR  KOHI-KOHNUM(39)
                   OR  KOHI-KOHNUM(40)
      *
                   INITIALIZE  FLG-KOHCHK
                   INITIALIZE  FLG-KOHCHK2
      *
      *            保険組合せマスター検索
                   INITIALIZE                     HKNCOMBI-REC
                   MOVE    RECE02-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    RECE02-PTID            TO  COMB-PTID
                   MOVE    RECE02-HKNCOMBI(IDX8)  TO  COMB-HKNCOMBINUM
                   PERFORM  910-HKNCOMBI-READ-SEC
      *
                   PERFORM  9001-CHK-SEC
      *
                   IF      FLG-KOHCHK   =  1
                      AND  FLG-KOHCHK2  =  1
                     PERFORM  2002-HENSYU-SEC
                   END-IF
      *
                 END-IF
               END-IF
             END-PERFORM
           END-PERFORM
      *
           .
       20021-KOHCHK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ印刷処理
      *****************************************************************
       2003-PRINT-SEC  SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO   FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM   900-TMP-READ-SEC
      *
           PERFORM   UNTIL  FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
             PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
             PERFORM  330-TERM-HEN-SEC
      *
           END-PERFORM
           CLOSE    MF100-FILE
      *
           .
       2003-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD   1     TO  CNT-PAGE
           INITIALIZE  KOHS-DATA
      *
      *    自治体名
           INITIALIZE                       WRK-CITYNAME1
           STRING   CITY-CITYNAME           DELIMITED BY SPACE
                   "償還払公費患者一覧表"   DELIMITED BY SIZE
                   INTO                     WRK-CITYNAME1
           END-STRING
      *
      *    文字数計算
           MOVE  ZERO                      TO    WRK-MOJI1
           PERFORM  VARYING  IDX1  FROM  1  BY  2
                    UNTIL    WRK-CITYNAME1(IDX1:2)  =  SPACE
                    OR       IDX1  >     60
             ADD    2                      TO    WRK-MOJI1
           END-PERFORM
      *    診療年月分の文字数加算
           ADD      20                     TO    WRK-MOJI1
      *    空白のバイト数計算
           COMPUTE  IDX1   = ((80 - WRK-MOJI1) / 2) + 1
           STRING   WRK-CITYNAME1           DELIMITED BY SPACE
                    "（" WRK-SRYYMWH "）"   DELIMITED BY SIZE
                    INTO                    WRK-CITYNAME2
           END-STRING
           MOVE  WRK-CITYNAME2(1:WRK-MOJI1)
                                  TO   KOHS-CITYNAME(IDX1:WRK-MOJI1)
      *
      *    CSV ヘッダー処理
           IF  CNT-PAGE  =  1
             PERFORM  800-CSV-HEAD-SEC
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE     KEY-NEW        TO    KEY-OLD
      *
           PERFORM  VARYING  IDX1  FROM  1   BY   1
                    UNTIL    IDX1  >  15
                    OR    FLG-PRINT   =   1
                    OR    KEY-NEW  NOT =  KEY-OLD
      *    公費名
             MOVE   MF100-KOHNAME          TO   KOHS-KOHNAME(IDX1)
      *
             MOVE   MF100-KOHNUM           TO   WRK-KOHNUM
             ADD    1                      TO   WRK-KENSU
      *
      *    ID(患者番号)
             MOVE   MF100-PTNUM            TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             IF    WRK-OUT-INPUT(21:)  =  SPACE
               MOVE   WRK-OUT-INPUT          TO   KOHS-PTNUM(IDX1)
             ELSE
               MOVE   WRK-OUT-INPUT          TO   KOHS-PTNUM2(IDX1)
             END-IF
      *
      *    氏名
             MOVE   MF100-NAME             TO   KOHS-NAME(IDX1)
             MOVE   MF100-FURIGANA         TO   KOHS-FURIGANA(IDX1)
      *
      *    負担者番号
             MOVE   MF100-FTNJANUM         TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE   WRK-OUT-INPUT          TO   KOHS-FTNJANUM(IDX1)
      *
      *    受給者番号
             MOVE   MF100-JKYSNUM          TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE   WRK-OUT-INPUT          TO   KOHS-JKYSNUM(IDX1)
      *
      *    診療年月(請求年月と異なる時のみ)
             IF     MF100-SRYYM  NOT =   WRK-PARA-SRYYM
               MOVE    MF100-SRYYM         TO   WRK-SYMD (1:6)
               MOVE    "01"                TO   WRK-SYMD (7:2)
               PERFORM   31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG1        TO   WRK-MAE-INPUT
               PERFORM   2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO   KOHS-SRYYM(IDX1)
             END-IF
      *
      *    性別
             EVALUATE  MF100-SEX
             WHEN  1
               MOVE    "男"                TO   KOHS-SEX(IDX1)
             WHEN  2
               MOVE    "女"                TO   KOHS-SEX(IDX1)
             END-EVALUATE
      *
      *    入外区分
             EVALUATE  MF100-NYUGAIKBN
             WHEN  "1"
               MOVE    "入"                TO   KOHS-NYUGAIKBN(IDX1)
             WHEN  "2"
               MOVE    "外"                TO   KOHS-NYUGAIKBN(IDX1)
             END-EVALUATE
      *
      *    保険者番号
             MOVE    MF100-HKNJANUM        TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-HKNJANUM(IDX1)
      *
      *    記号
             MOVE      MF100-KIGO          TO   KOHS-KIGO(IDX1)
      *
      *    番号
             MOVE      MF100-NUM           TO   KOHS-NUM(IDX1)
      *
      *    実日数
             MOVE    MF100-JNISSU          TO   WRK-Z2
             MOVE    WRK-Z2                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-NISSU(IDX1)
      *
      *    点数
             MOVE    MF100-TOTALTEN        TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-TENSU(IDX1)
      *
      *    本人負担額
             MOVE    MF100-FTNMONEY        TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-ITBFTN(IDX1)
      *
      *    食事療養費-日数
             MOVE    MF100-SKJKAISU        TO   WRK-Z2
             MOVE    WRK-Z2                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-SKJKAISU(IDX1)
      *    食事療養費-請求額
             MOVE    MF100-SKJSKY          TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-SKJSKY(IDX1)
      *    食事療養費-標準負担額
             MOVE    MF100-SKJFTN          TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-SKJFTN(IDX1)
      *
      *    診療科名
             IF   MF100-SRYKA   NOT =   "00"
               INITIALIZE                  SYS-1005-REC
               MOVE    SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
               MOVE    "1005"              TO   SYS-1005-KANRICD
               MOVE    MF100-SRYKA         TO   SYS-1005-KBNCD
               MOVE  WRK-PARA-PRTSYMD      TO   SYS-1005-EDYUKYMD
               MOVE  WRK-PARA-PRTLYMD      TO   SYS-1005-STYUKYMD
               MOVE    SYS-1005-REC        TO   MCPDATA-REC
               PERFORM 800-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI  =  ZERO
                 MOVE  MCPDATA-REC         TO   SYS-1005-REC
                 MOVE  SYS-1005-SRYKANAME  TO   KOHS-SRYKANAME(IDX1)
               END-IF
             END-IF
      *
      *    給付割合
             MOVE     MF100-KYURATE        TO   WRK-Z2
             MOVE     WRK-Z2               TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT        TO   KOHS-KYURATE(IDX1)
      *
      *    他法公費
             INITIALIZE                         WRK-TAHO
             MOVE     MF100-TAHO(1)        TO   WRK-TAHO(1:2)
             PERFORM  VARYING    IDX2  FROM  2  BY  1
                      UNTIL   IDX2   >   4
                      OR    MF100-TAHO(IDX2)   =   SPACE
               STRING  WRK-TAHO            DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       MF100-TAHO(IDX2)    DELIMITED  BY  SIZE
                       INTO                WRK-TAHO
               END-STRING
             END-PERFORM
             MOVE     WRK-TAHO             TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT        TO   KOHS-TAHO(IDX1)
      *
             PERFORM  400-CSV-DATA-SEC
      *
             PERFORM  900-TMP-READ-SEC
      *
      *      CSV 件数初期化
             INITIALIZE  WRK-ITEM
      *
      *      改ページフラグが０の時は公費番号で改ページしない
             IF  KOHS-KAIPAGE              =   ZERO
               MOVE     KEY-NEW            TO    KEY-OLD
      *
      *        公費件数の印字（行）
               IF    WRK-KOHNUM  NOT =  MF100-KOHNUM
                 OR  FLG-PRINT   =  1
                 MOVE    "件数合計＝"      TO   KOHS-KOHKENSU1(IDX1)
                 MOVE    WRK-KENSU         TO   WRK-Z3
                 MOVE    WRK-Z3            TO   KOHS-KENSU1(IDX1)
      *          CSV 件数格納
                 MOVE    WRK-KENSU         TO   WRK-ITEM
                 INITIALIZE  WRK-KENSU
               END-IF
             ELSE
      *        公費件数の印字（改頁）
               IF    WRK-KOHNUM  NOT =  MF100-KOHNUM
                 OR  FLG-PRINT   =  1
                 MOVE    "（件数合計＝"    TO   KOHS-KOHKENSU2-1
                 MOVE    "）"              TO   KOHS-KOHKENSU2-2
                 MOVE    WRK-KENSU         TO   WRK-Z3
                 MOVE    WRK-Z3            TO   KOHS-KENSU2
      *          CSV 件数格納
                 MOVE    WRK-KENSU         TO   WRK-ITEM
                 INITIALIZE  WRK-KENSU
               END-IF
             END-IF
      *
             PERFORM  810-CSVDATA-EDIT-SEC
             PERFORM  230-TOUKEICSV-SEC
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *
           MOVE   "A00000KOHSV02.red"         TO    SPRT-PRTID
           MOVE    "償還払公費患者一覧表"  TO    SPRT-TITLE
           MOVE    KOHS-DATA               TO    SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO    SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO    SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO    SPRT-PRTNM
      *
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN   =   ZERO
             CONTINUE
           END-IF
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                   TO  IDXP
      *
      *    自治体名
           INITIALIZE  WRK-ITEM
           STRING  "自治体名："          DELIMITED  BY  SIZE
                   CITY-CITYNAME         DELIMITED  BY  SPACE
                                         INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    対象年月
           INITIALIZE  WRK-ITEM
           STRING  "対象年月："          DELIMITED  BY  SIZE
                   WRK-SRYYMWH           DELIMITED  BY  SPACE
                                         INTO  WRK-ITEM
           END-STRING
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称セット
           PERFORM  VARYING  IDX11  FROM  1  BY  1
                    UNTIL    IDX11  >  CSV-HEAD-MAX
      *
             INITIALIZE  WRK-ITEM
             MOVE    CSV-HEAD-ITEM(IDX11)     TO  WRK-ITEM
      *
             IF  IDX11  =  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
      *
           END-PERFORM
      *
           PERFORM  230-TOUKEICSV-SEC
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
      *    公費名
           INITIALIZE  WRK-ITEM
           MOVE    MF100-KOHNAME          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    ID(患者番号)
           INITIALIZE  WRK-ITEM
           MOVE    MF100-PTNUM            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    カナ氏名
           INITIALIZE  WRK-ITEM
           MOVE    MF100-FURIGANA         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    氏名
           INITIALIZE  WRK-ITEM
           MOVE    MF100-NAME             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    負担者番号
           INITIALIZE  WRK-ITEM
           MOVE    MF100-FTNJANUM         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    受給者番号
           INITIALIZE  WRK-ITEM
           MOVE    MF100-JKYSNUM          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療年月(請求年月と異なる時のみ)
           INITIALIZE  WRK-ITEM
           IF  MF100-SRYYM  NOT =  WRK-PARA-SRYYM
             MOVE    MF100-SRYYM            TO  WRK-SYMD (1:6)
             MOVE    "01"                   TO  WRK-SYMD (7:2)
             PERFORM   31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMDG1           TO  WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT(1:12)    TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    性別
           INITIALIZE  WRK-ITEM
           EVALUATE  MF100-SEX
           WHEN  1
             MOVE    "男"                   TO  WRK-ITEM
           WHEN  2
             MOVE    "女"                   TO  WRK-ITEM
           END-EVALUATE
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分
           INITIALIZE  WRK-ITEM
           EVALUATE  MF100-NYUGAIKBN
           WHEN  "1"
             MOVE    "入"                   TO  WRK-ITEM
           WHEN  "2"
             MOVE    "外"                   TO  WRK-ITEM
           END-EVALUATE
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者番号
           INITIALIZE  WRK-ITEM
           MOVE    MF100-HKNJANUM         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    記号
           INITIALIZE  WRK-ITEM
           MOVE    MF100-KIGO             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    番号
           INITIALIZE  WRK-ITEM
           MOVE    MF100-NUM              TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    実日数
           INITIALIZE  WRK-ITEM
           MOVE    MF100-JNISSU           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    点数
           INITIALIZE  WRK-ITEM
           MOVE    MF100-TOTALTEN         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    本人負担額
           INITIALIZE  WRK-ITEM
           MOVE    MF100-FTNMONEY         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事療養費-日数
           INITIALIZE  WRK-ITEM
           MOVE    MF100-SKJKAISU         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事療養費-請求額
           INITIALIZE  WRK-ITEM
           MOVE    MF100-SKJSKY           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事療養費-標準負担額
           INITIALIZE  WRK-ITEM
           MOVE    MF100-SKJFTN           TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科名
           INITIALIZE  WRK-ITEM
           IF   MF100-SRYKA   NOT =   "00"
             MOVE    SYS-1005-SRYKANAME     TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    給付割合
           INITIALIZE  WRK-ITEM
           MOVE    MF100-KYURATE          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    他法公費
           INITIALIZE  WRK-ITEM
           PERFORM  VARYING  IDX12  FROM  1  BY  1
                    UNTIL    IDX12  >  22
                    OR       WRK-TAHO(IDX12:1)  =  SPACE
             IF  WRK-TAHO(IDX12:1)  =  ","
               MOVE    "/"   TO  WRK-TAHO(IDX12:1)
             END-IF
           END-PERFORM
      *
           MOVE    WRK-TAHO               TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
           .
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
                   "kouhiichiran_"           DELIMITED  BY  SIZE
                   WRK-PARA-SRYYM            DELIMITED  BY  SPACE
                   ".csv"                    DELIMITED  BY  SIZE
                                             INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA               TO  STOUKEICSV-CSVDATA
           MOVE    "償還払公費患者一覧表"    TO  STOUKEICSV-TITLE
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
           READ  MF100-FILE   NEXT
             AT  END
               MOVE    1                   TO   FLG-PRINT
             NOT  AT  END
               MOVE    MF100-KOHNUM-KEY    TO   KEY-N-KOHNUM
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF   STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN   OUTPUT  RECEERR-FILE
      *
             MOVE   WRK-RECEERR     TO   RECEERR-REC
             WRITE  RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE               STS-AREA-DAY
           INITIALIZE               LNK-DAY7-AREA
           MOVE    "71"             TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD         TO   LNK-DAY7-YM
           CALL    "ORCSDAY"        USING
                                    STS-AREA-DAY
                                    LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI     TO   WRK-SYMD
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
           MOVE    "2"                 TO   KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO   KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF   KANACHK-RC   =   ZERO
             MOVE   KANACHK-OUT-INPUT  TO   WRK-OUT-INPUT
           ELSE
               MOVE   WRK-MAE-INPUT    TO   WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理セレクト
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY          TO   RECE10-KEY
           MOVE    RECE10-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                 TO   FLG-END
           END-IF
           PERFORM  900-SEIKYU-CLOSE-SEC
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-END
             MOVE    MCPDATA-REC       TO   RECE10-REC
             ADD    1                  TO   CNT-RECE10
           ELSE
             MOVE    1                 TO   FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費請求マスタ読込
      ******************************************************************
       900-KOHSKY-SELECT-SEC            SECTION.
      *
           INITIALIZE                   RECE02-REC
           MOVE    SPA-HOSPNUM          TO   RECE02-HOSPNUM
           MOVE    WRK-PARA-SRYYM       TO   RECE02-SKYYM
           MOVE    RECE02-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_kohsky"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM  900-KOHSKY-FETCH-SEC
           END-IF
           .
       900-KOHSKY-SELECT-EXT.
           EXIT.
      ******************************************************************
      *    公費請求マスタ
      ******************************************************************
       900-KOHSKY-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
           MOVE    "tbl_kohsky"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC        TO   RECE02-REC
      *
      *        テスト患者のときは読み飛ばす
             MOVE    SPACE             TO   PTINF-REC
             INITIALIZE                     PTINF-REC
             MOVE    RECE02-HOSPNUM    TO   PTINF-HOSPNUM
             MOVE    RECE02-PTID       TO   PTINF-PTID
             MOVE    PTINF-REC         TO   MCPDATA-REC
             PERFORM 800-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND   PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-KOHSKY-FETCH-EXT
             END-IF
      *
           ELSE
             MOVE    1                  TO   FLG-KOHSKY
             INITIALIZE                 RECE02-REC
           END-IF
           .
       900-KOHSKY-FETCH-EXT.
           EXIT.
      ******************************************************************
      *    公費請求マスタ
      ******************************************************************
       900-KOHSKY-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_kohsky"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    対象公費所持者判定処理
      *****************************************************************
       9001-CHK-SEC                    SECTION.
      *
           INITIALIZE  PTKOHINF-REC
      *
           MOVE    COMB-HOSPNUM         TO   PTKOH-HOSPNUM
           MOVE    COMB-PTID            TO   PTKOH-PTID
           MOVE    COMB-KOHID(IDX7)     TO   PTKOH-KOHID
           MOVE    PTKOHINF-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_ptkohinf"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
      *
             IF   ( PTKOH-TEKSTYMD(1:6)     <=  RECE02-SRYYM
               AND  PTKOH-TEKEDYMD(1:6)     >=  RECE02-SRYYM )
               AND  PTKOH-SAKJOKBN       NOT =  "1"
               MOVE    1     TO   FLG-KOHCHK
             END-IF
      *
             PERFORM  VARYING  IDX9  FROM  1 BY  1
                      UNTIL    IDX9  >  4
                      OR       FLG-KOHCHK2  =  1
      *
               IF     RECE02-KOHNUMHC(IDX8 IDX7)  =  RECE02-KOHNUM(IDX9)
                 AND  ( PTKOH-FTNJANUM  =  RECE02-KOHFTNJANUM(IDX9)
                 AND    PTKOH-JKYSNUM   =  RECE02-KOHJKYSNUM(IDX9)  )
      *
                 MOVE    IDX9  TO  IDX3
                 MOVE    1     TO  FLG-KOHCHK2
      *
               END-IF
      *
             END-PERFORM
      *
           ELSE
             DISPLAY  "PTKOHINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"          TO   MCP-FUNC
           MOVE    "tbl_ptkohinf"           TO   MCP-TABLE
           MOVE    "key"                    TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       9001-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"      TO   MCP-FUNC
           CALL    "ORCDBMAIN"    USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE  MCPDATA-REC    TO   PTKOHINF-REC
             MOVE  ZERO           TO   FLG-PTKOHINF
           ELSE
             INITIALIZE   PTKOHINF-REC
             MOVE    1            TO   FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_ptinf"        TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-PTINF
               MOVE    MCPDATA-REC      TO   PTINF-REC
             ELSE
               MOVE    1                TO   FLG-PTINF
               INITIALIZE               PTINF-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-PTINF
             INITIALIZE                 PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      **    収納情報検索
      ******************************************************************
       910-SYUNOU-SELECT-SEC             SECTION.
      *
           INITIALIZE                    SYUNOU-REC
      *
           MOVE    RECE10-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    RECE10-PTID       TO  SYU-PTID
           MOVE    RECE10-SRYYM      TO  SYU-SKYSTYMD(1:6)
           MOVE    "01"              TO  SYU-SKYSTYMD(7:2)
           MOVE    "99999999"        TO  SYU-SKYEDYMD
      *
           MOVE    SYUNOU-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"        TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key5"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 910-SYUNOU-FETCH-SEC
           ELSE
               MOVE    1             TO  FLG-SYUNOU
           END-IF
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-SYUNOU-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key5"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SYUNOU
               MOVE    MCPDATA-REC   TO  SYUNOU-REC
           ELSE
               MOVE    1             TO  FLG-SYUNOU
           END-IF
           .
       910-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-SYUNOU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key5"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       910-SYUNOU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  SYUNOU-REC
           MOVE  RECE10-HOSPNUM       TO   SYU-HOSPNUM
           MOVE  RECE10-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE  RECE10-PTID          TO   SYU-PTID
           MOVE  SYUNOU-REC           TO   MCPDATA-REC
           MOVE  "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  925-SYUNOU-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  RECE10-SRYYM  =  SYU-SRYYMD(1:6)
                 IF  SYU-DENPJTIKBN  NOT =  3
                   COMPUTE  WRK-SKYMONEY  =  SYU-SKYMONEY
                                          -  SYU-JIHI-TOTAL
                                          -  SYU-JIHI-TOTAL-TAX
                   ADD   WRK-SKYMONEY     TO   MF100-FTNMONEY
                 END-IF
               END-IF
      *
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF    MCP-RC  =  ZERO
             MOVE    MCPDATA-REC      TO   SYUNOU-REC
             MOVE    ZERO             TO   FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           PERFORM  955-HKNNUM-SELECT-SEC
           PERFORM  955-HKNNUM-FETCH-SEC  UNTIL  FLG-HKNNUM = 1
           PERFORM  955-HKNNUM-CLOSE-SEC
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       955-HKNNUM-SELECT-SEC  SECTION.
      *
           INITIALIZE                     FLG-HKNNUM
           INITIALIZE                     HKNNUM-REC
           MOVE    SPA-HOSPNUM        TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM         TO  HKN-HKNNUM
           MOVE    WRK-TEKSTYMD       TO  HKN-TEKSTYMD
           MOVE    WRK-TEKEDYMD       TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key5"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  955-HKNNUM-FETCH-SEC
           ELSE
             MOVE      1                TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       955-HKNNUM-FETCH-SEC  SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key5"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  HKNNUM-REC
             MOVE    ZERO               TO  FLG-HKNNUM
           ELSE
             MOVE    1                  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       955-HKNNUM-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key5"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       955-HKNNUM-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
             ELSE
               MOVE    1               TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(NEXT READ)
      *****************************************************************
       700-TMP-READ-SEC              SECTION.
      *
           INITIALIZE                FLG-INVALID
           READ    MF100-FILE
               INVALID    KEY
                 MOVE    1           TO  FLG-INVALID
               NOT  INVALID  KEY
                 MOVE    ZERO        TO  FLG-INVALID
           END-READ
           .
       700-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC  SECTION.
      *
           MOVE    CNT-PAGE          TO   WRK-PARA-PAGE
      *
      *    ステップ終了処理
           MOVE    "STE"             TO   SJOBKANRI-MODE
           INITIALIZE                     JOBKANRI-REC
           MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE     TO   JOB-UPDCNT
           MOVE    WRK-PARA-JOBID    TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID  TO   JOB-SHELLID
           CALL    "ORCSJOB"         USING
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
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE    1                TO   FLG-SYSKANRI
             END-IF
           ELSE
               MOVE    1                TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE     "DBOPEN"       TO   MCP-FUNC.
           MOVE    LOW-VALUE       TO   MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
                                   .
      *
           MOVE    "DBSTART"       TO   MCP-FUNC.
           MOVE    LOW-VALUE       TO   MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
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
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    "DBCOMMIT"         TO   MCP-FUNC.
           MOVE    LOW-VALUE          TO   MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
                                      .
      *
           MOVE    "DBDISCONNECT"     TO   MCP-FUNC.
           MOVE    LOW-VALUE          TO   MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
                                      .
           .
       900-DBCLOSE-EXT.
           EXIT.
