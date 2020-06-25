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
       PROGRAM-ID.     SEIKYU2705.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 大阪・一部負担金相当額等一部助成費請求書
      *                      (188)
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/05/09    山口          ORCAのバージョンアップに伴う(2.5.0)再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    山口    05/06/28    全国公費を併用した場合でも受給者
      *                                  負担金額が１円単位ででるよう修正
      *  01.00.02    中川    06/08/23    堺市政令指定都市化対応
      *  01.00.03    内田    07/01/12    診療時の保険番号マスタを参照する
      *                                  一部負担金、受給者負担金額欄は
      *                                  ０なら０を印字
      *  01.00.04    内田    07/03/09    診療時の保険組み合わせを取得する
      *  01.00.05    内田    07/09/17    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  長期上位（974）は長期と同様の
      *                                  処理をする
      *                                  市町村名「豊中市」の印字
      *                                   (負担者番号から保険者を特定できない)
      *  01.00.06    内田    08/02/08    返戻分は印字対象外とする
      *  01.00.07    門間    08/11/20    SPA-AREA初期化
      *  01.00.08    門間    18/12/03    修正履歴コメント削除
      *                                  SITEKBN変更（動作に変更なし）
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
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
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI2705.INC".
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
           COPY  "SC2705.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-SYSKANRI3  PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-SEIKYU     PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
           03  FLG-HKNNUM     PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-HKNCOMBI2  PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-PTHKNINF   PIC 9(01).
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
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z29               PIC Z9.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC ZZZZZ.
           03  WRK-Z59               PIC ZZZZ9.
           03  WRK-Z6                PIC ZZZZZZ.
           03  WRK-Z69               PIC ZZZZZ9.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z99               PIC Z,ZZZ,ZZ9.
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
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-RJNRITU	     PIC 9(03).
           03  WRK-RJNRITUGAKU       PIC 9(05).
           03  WRK-KOHFTN	     PIC 9(08).
           03  WRK-KOBETUCITYNUM     PIC X(03).
           03  WRK-SYUKEI-TBL.
               05  WRK-SEIKYUGK-G    PIC 9(08)V9(01).
               05  WRK-SEIKYUGK-G2   PIC 9(08).
               05  WRK-SEIKYUGK      PIC 9(08)V9(01).
               05  WRK-FTNMONEY-G    PIC 9(08)V9(01).
               05  WRK-FTNMONEY-G2   PIC 9(08).
               05  WRK-FTNMONEY      PIC 9(08)V9(01).
           03  WRK-TOTALTEN          PIC 9(08).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
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
      *    集計エリア
       01  SUM-AREA.
           03  SUM-SYOKEI-AREA.
               05  SUM-ITBFTN        PIC 9(07).
               05  SUM-JKYFTN        PIC 9(07).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *    キー用エリア
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-FTNJANUM   PIC 9(03).
           03  KEY-OLD.
               05  KEY-O-FTNJANUM   PIC 9(03).
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
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険情番号報
       01  HKNNUM-REC.
           COPY  "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           INITIALIZE  SUM-AREA
           INITIALIZE  SPA-AREA
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
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU2705"    TO  JOB-PGID
           MOVE    "一部負担金相当額等一部助成費請求書"
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
           MOVE  "MF100ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)  TO  MF100PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM           TO  MF100PARA-HOSPNUM
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1001-STYUKYMD(1:6)
                                   SYS-1001-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU2705 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1002-STYUKYMD(1:6)
                                   SYS-1002-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU2705 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
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
           MOVE    ZERO                    TO  IDXX
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
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
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
      *
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
      *   月途中の主保険切替え対応
      *   負担者番号、受給者番号をKEYにセットして一時ファイルを読み込む
           IF  RECE02-RJNHKNNUM  =  SPACE
             INITIALIZE                 WRK-KOHNUM
               PERFORM  910-HKNCOMBI-SELECT-SEC
               PERFORM  930-PTKOHINF-SET-SEC
           ELSE
             MOVE  RECE02-RJNJKYSNUM         TO  MF100-KOHJKYSNUM
                                                 MF100-JKYSNUM
             MOVE  RECE02-RJNFTNJANUM        TO  MF100-KOHFTNJANUM
             MOVE  RECE02-RJNFTNJANUM(5:3)   TO  MF100-FTNJANUM
             MOVE  RECE02-RJNFTNJANUM        TO  MF100-FTNJANUM-KEY
           END-IF
      *
      *    個別発行用市町村番号設定
           MOVE   MF100-FTNJANUM      TO  WRK-KOBETUCITYNUM
      *
      *    大阪市は１つにまとめる
           IF    MF100-FTNJANUM(1:1) = "4"
             MOVE  "400"              TO    MF100-FTNJANUM
           END-IF
      *
           IF    MF100-FTNJANUM(1:2) = "50"  OR
                 MF100-FTNJANUM(1:3) = "002"
                 MOVE  "500"      TO    MF100-FTNJANUM
           END-IF
      *
      *    豊中市は４１老人負担者番号から市町村を特定できないので代入
           IF   RECE02-RJNHKNNUM  =  SPACE
           AND  MF100-FTNJANUM    =  "045"
               MOVE  "004"        TO  MF100-FTNJANUM
           END-IF
      *
           PERFORM  800-SKTTMP-READ-SEC
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
      *
           INITIALIZE                 WRK-TOTALTEN
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
             IF  RECE02-KOHNUM (IDX) = "188"
                MOVE  RECE02-KOHID      (IDX)  TO  MF100-KOHID
                MOVE  RECE02-KOHNUM     (IDX)  TO  MF100-KOHNUM
                MOVE  RECE02-KOHHBTNUM  (IDX)  TO  MF100-KOHHBTNUM
               COMPUTE IDY     =         IDX   +   1
               ADD   RECE02-JNISSU   (IDY)     TO  MF100-JNISSU
               ADD   RECE02-TOTALTEN (IDY)     TO  MF100-TOTALTEN
      *
      *       一部負担金の計算をするための点数を退避させる
               MOVE  RECE02-TOTALTEN (IDY)     TO  WRK-TOTALTEN
             END-IF
           END-PERFORM
      *
      *    ４１の一部負担金計算処理
           IF  RECE02-RJNHKNNUM  =  SPACE
             PERFORM  2001-ITBFTN-SEC
           ELSE
             MOVE     ZERO                     TO  FLG-TYOKI
             INITIALIZE                            WRK-SYUKEI-TBL
             MOVE     MF100-KOHNUM             TO  WRK-KOHNUM
             MOVE     MF100-KOHID              TO  WRK-KOHID
             IF      RECE02-NYUGAIKBN    =   "1"
                 MOVE    "key33"        TO  WRK-PATH
             ELSE
                 MOVE    "key25"        TO  WRK-PATH
             END-IF
             PERFORM  920-SYUNOU-SET-SEC
      *
             COMPUTE  WRK-SEIKYUGK-G2 ROUNDED  =  WRK-SEIKYUGK-G
      *
      *    長期
             IF    FLG-TYOKI    =    1  AND
                   WRK-SEIKYUGK-G2    >  10000
                 MOVE    10000          TO    WRK-SEIKYUGK-G2
             END-IF
      *    長期上位
             IF    FLG-TYOKI    =    2  AND
                   WRK-SEIKYUGK-G2    >  20000
                 MOVE    20000          TO    WRK-SEIKYUGK-G2
             END-IF
      *    主保険に限度額が有る場合、限度額を越えているか判定
             IF    RECE02-FTNMONEY(1)         >  ZERO    AND
                   RECE02-FTNMONEY(1)         <  WRK-SEIKYUGK-G2
               MOVE     RECE02-FTNMONEY(1)   TO  WRK-SEIKYUGK-G2
             END-IF
      *
             ADD   WRK-SEIKYUGK-G2           TO  MF100-RJNFTN
      *
           END-IF
      *
      *    受給者負担金額
           COMPUTE  WRK-FTNMONEY-G2 ROUNDED = WRK-FTNMONEY-G
           MOVE     WRK-FTNMONEY-G2    TO    MF100-FTNMONEY
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           DISPLAY "患者番号-="  MF100-PTNUM(1:10)
                   "市町村番号=" MF100-FTNJANUM
                   "負担者番号=" MF100-KOHFTNJANUM(1:10)
                   "受給者番号=" MF100-KOHJKYSNUM(1:10)
                   "点数="       MF100-TOTALTEN
                   "一部負担金=" MF100-RJNFTN
                   "患者負担金=" MF100-FTNMONEY
      *
           IF       FLG-ERR                    =   ZERO
      *     一時ファイルにレコードが無ければWRITE、あればREWRITEする
             IF  FLG-MF100  =  1
                 WRITE    MF100-REC
                 ADD      1                 TO    CNT-MF100
             ELSE
                 REWRITE  MF100-REC
             END-IF
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                     "一部負担金="          MF100-FTNMONEY
                     "点数="                MF100-TOTALTEN
                     "実日数="              MF100-JNISSU
           END-IF
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ４１の一部負担金計算処理
      *****************************************************************
       2001-ITBFTN-SEC   SECTION.
      *
           DISPLAY "４１老人一部負担金対象者=" RECE02-PTNUM
                   "点数=" MF100-TOTALTEN
                   "入外区分=" RECE02-NYUGAIKBN
      *
           DISPLAY "910-END 公費番号="  WRK-KOHNUM
                   "フラグ=" FLG-HKNCOMBI
      *
           IF    WRK-KOHNUM   =   SPACE
             DISPLAY "WRK-KOHNUM スペース" WRK-KOHNUM
             COMPUTE  WRK-KOHFTN  =  WRK-TOTALTEN *
                                     (RECE02-KYURATE  /  10)
           ELSE
             PERFORM  920-HKNNUM-SET-SEC
      *
      *      低所得者上限編集
             EVALUATE RECE02-SHOKUJIFTNKBN
                 WHEN    "1"
                 WHEN    "2"
                     MOVE    24600   TO  WRK-RJNRITUGAKU
                 WHEN    "3"
                     MOVE    15000   TO  WRK-RJNRITUGAKU
             END-EVALUATE
      *
             COMPUTE  WRK-KOHFTN  =  WRK-TOTALTEN *
                                     (WRK-RJNRITU / 10)
             IF    RECE02-NYUGAIKBN  =  "1"  AND
                   WRK-KOHFTN    >    WRK-RJNRITUGAKU
      *
               DISPLAY "上限を越えた為編集 上限=" WRK-RJNRITUGAKU
      *
               MOVE    WRK-RJNRITUGAKU    TO    WRK-KOHFTN
             END-IF
           END-IF
      *
           DISPLAY "４１老人一部負担金編集結果=" WRK-KOHFTN
      *
           ADD  WRK-KOHFTN         TO   MF100-RJNFTN
      *
           MOVE  MF100-KOHNUM       TO   WRK-KOHNUM
           MOVE  MF100-KOHID        TO   WRK-KOHID
           INITIALIZE                    WRK-SYUKEI-TBL
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"        TO  WRK-PATH
           ELSE
               MOVE    "key25"        TO  WRK-PATH
           END-IF
           PERFORM 920-SYUNOU-SET-SEC
      *
           .
       2001-ITBFTN-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "27"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)  OR
                         ( WRK-HKNJANUM(1:3)  =  "274"   AND
                           TBL-PARA-HKNJANUM(IDXX)(1:5)  =  "27400" )
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
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
             PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理 (医療機関控用)
             MOVE  "医療機関控"  TO  SC05-TEISYUTU1
             MOVE  "○"          TO  SC05-TEISYUTU2
             PERFORM  330-PRINT-OUT-SEC
      *
      *    帳票印刷処理（提出用）
             MOVE  "提出用"      TO  SC05-TEISYUTU1
             MOVE  "  "          TO  SC05-TEISYUTU2
             ADD    1            TO  CNT-PAGE
             PERFORM  330-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
      *
           ADD     1    TO    CNT-PAGE
      *
           INITIALIZE                             SC05
      *
      *    対象年月
           MOVE    WRK-SRYYMWH               TO   SC05-PRTYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH              TO   SC05-SEIYMD
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCDN         TO   SC05-HOSPCDN
      *    医療機関住所
           MOVE    WRK-HOSPADRS              TO   SC05-ADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME              TO   SC05-HOSPNAME
      *    開設者名
           MOVE     SYS-1001-KAISETUNAME     TO   SC05-KAISETU
      *    電話番号
           MOVE     WRK-HOSPTEL              TO   SC05-TEL
      *
           MOVE     "27"                     TO   WRK-HKNJANUM(1:2)
           MOVE     MF100-FTNJANUM           TO   WRK-HKNJANUM(3:3)
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE     HKNJA-HKNJANAME          TO   SC05-TOWNNAME
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
      *
           MOVE     KEY-NEW      TO  KEY-OLD
           INITIALIZE                SUM-SYOKEI-AREA
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >    20
                    OR       KEY-NEW    NOT   =    KEY-OLD
                    OR       FLG-PRINT        =     1
      *
      *    負担者番号
             INITIALIZE                      WRK-MAE-INPUT
             MOVE    MF100-KOHFTNJANUM   TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT(1:4)  TO  SC05-FTNJANUM1(IDX1)
             MOVE    WRK-OUT-INPUT(9:8)  TO  SC05-FTNJANUM2(IDX1)
      *
      *    受給者番号
             INITIALIZE                      WRK-MAE-INPUT
             MOVE    MF100-JKYSNUM       TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT(1:14) TO  SC05-JKYSNUM(IDX1)
      *
      *    氏名
             MOVE    MF100-NAME          TO  SC05-NAME(IDX1)
      *
      *    診療年月
             IF  MF100-SRYYM  NOT  =  LNK-PRTKANRI-SRYYM
               MOVE    MF100-SRYYM         TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1(2:2) TO SC05-SRYYY(IDX1)
               MOVE    LNK-DAY2-EDTYMD1(5:2) TO SC05-SRYMM(IDX1)
             END-IF
      *
      *    入外区分
             IF  MF100-NYUGAIKBN  =  "1"
               MOVE  "◯"                 TO  SC05-NYUGAI(IDX1 1)
             ELSE
               MOVE  "◯"                 TO  SC05-NYUGAI(IDX1 2)
             END-IF
      *
      *    日数
             MOVE  MF100-JNISSU          TO  WRK-Z2
             INITIALIZE                      WRK-MAE-INPUT
             MOVE    WRK-Z2              TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT       TO  SC05-NISSU(IDX1)
      *
      *    一部負担金
             MOVE  MF100-RJNFTN          TO  WRK-Z69
             INITIALIZE                      WRK-MAE-INPUT
             MOVE    WRK-Z69             TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT       TO  SC05-ITBFTN(IDX1)
      *
      *    受給者負担金額
             MOVE  MF100-FTNMONEY        TO  WRK-Z59
             INITIALIZE                      WRK-MAE-INPUT
             MOVE    WRK-Z59             TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT       TO  SC05-JKYFTN(IDX1)
      *
      *    備考
             IF  MF100-HEIKOHNUM    NOT   =  SPACE
               MOVE  "◯"                TO  SC05-BIKO2(IDX1)
               MOVE  MF100-HEIKOHNUM(2:2) TO SC05-BIKO1(IDX1)
             END-IF
      *
      *    小計加算
             ADD     MF100-RJNFTN        TO  SUM-ITBFTN
             ADD     MF100-FTNMONEY      TO  SUM-JKYFTN
      *
      *    一時ファイル読込
             PERFORM   900-TMP-READ-SEC
           END-PERFORM
      *
      *    小計編集
           MOVE  SUM-ITBFTN            TO  WRK-Z6
           INITIALIZE                      WRK-MAE-INPUT
           MOVE    WRK-Z6              TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  SC05-GITBFTN
      *
           MOVE  SUM-JKYFTN            TO  WRK-Z5
           INITIALIZE                      WRK-MAE-INPUT
           MOVE    WRK-Z5              TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  SC05-GJKYFTN
      *
           COMPUTE  WRK-SEIKYUGK-G2     =  SUM-ITBFTN - SUM-JKYFTN
           MOVE     WRK-SEIKYUGK-G2    TO  WRK-Z99
           MOVE     WRK-Z99            TO  SC05-GKOHFTN
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
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
           MOVE   "SC2705.red"         TO  SPRT-PRTID
           MOVE    "一部負担金相当額等一部助成費請求書"
                                    TO  SPRT-TITLE
           MOVE    SC05             TO  SPRT-PRTDATA
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
       330-PRINT-OUT-EXT.
           EXIT.
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
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2705 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2705 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU2705 END ***"
      *
           IF  CNT-MF100  =  0
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
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT  AT  END
                   MOVE  MF100-FTNJANUM  TO  KEY-N-FTNJANUM
           END-READ
           .
       900-TMP-READ-EXT.
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF  RECE02-KOHNUM(1) = "188"
                OR RECE02-KOHNUM(2) = "188"
                OR RECE02-KOHNUM(3) = "188"
                OR RECE02-KOHNUM(4) = "188"
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
      *        返戻分は印字対象外
                   IF  RECE02-HENREIYM  NOT =  ZERO
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
                   DISPLAY "患者番号=" RECE02-PTNUM
                           "負担者番号=" RECE02-KOHFTNJANUM(1)
                           "保険者番号=" RECE02-HKNJANUM
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
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC  SECTION.
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM                 TO  COMB-HOSPNUM
           MOVE    RECE02-PTID                 TO  COMB-PTID
           MOVE    RECE02-HKNID                TO  COMB-HKNID
           MOVE    HKNCOMBI-REC                TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBI
               PERFORM  910-HKNCOMBI-FETCH-SEC
      *    診療時の有効な保険組み合わせから４１老人を取得
      *    （前期高齢者は４１老人を持っていても診療時には使わない）
               PERFORM  UNTIL  FLG-HKNCOMBI  =  1
                 IF  (COMB-TEKSTYMD (1:6)  <=  RECE02-SRYYM
                 AND  COMB-TEKEDYMD (1:6)  >=  RECE02-SRYYM)
                 AND  COMB-DELKBN  NOT =  "1"
                   EVALUATE  TRUE
                   WHEN COMB-KOHHKNNUM(1)  =  "141" OR "241"
                      MOVE    1                 TO  FLG-HKNCOMBI
                      MOVE    COMB-KOHHKNNUM(1) TO  WRK-KOHNUM
                   WHEN COMB-KOHHKNNUM(2)  =  "141" OR "241"
                      MOVE    1                 TO  FLG-HKNCOMBI
                      MOVE    COMB-KOHHKNNUM(2) TO  WRK-KOHNUM
                   WHEN COMB-KOHHKNNUM(3)  =  "141" OR "241"
                      MOVE    1                 TO  FLG-HKNCOMBI
                      MOVE    COMB-KOHHKNNUM(3) TO  WRK-KOHNUM
                   WHEN COMB-KOHHKNNUM(4)  =  "141" OR "241"
                      MOVE    1                 TO  FLG-HKNCOMBI
                      MOVE    COMB-KOHHKNNUM(4) TO  WRK-KOHNUM
                   END-EVALUATE
                 END-IF
      *
                 PERFORM  910-HKNCOMBI-FETCH-SEC
               END-PERFORM
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key5"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-FETCH-SEC  SECTION.
      *
           MOVE    "DBFETCH"                   TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
                                   WRK-FTNMONEY
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"  AND
                   SYU-KOH-HKNNUM(IDX) NOT  =  "027"
               MOVE   1                TO    FLG-HEIYOU
               MOVE   SYU-KOH-COMPFTN-ENTANI(IDX)
                                       TO    WRK-SEIKYUGK
      *
      *    併用チェック
               IF    SYU-KOH-HKNNUM(IDX) =  "010" OR "011" OR "020"
                                         OR "021" OR "015" OR "051"
                 MOVE   SYU-KOH-HKNNUM(IDX) TO  MF100-HEIKOHNUM
               END-IF
      *
             END-IF
             IF    SYU-KOH-HKNNUM(IDX)   =   "972"
               MOVE    1     TO    FLG-TYOKI
             END-IF
      *      長期上位
             IF    SYU-KOH-HKNNUM(IDX)   =   "974"
               MOVE    1     TO    FLG-TYOKI
             END-IF
      *
             IF  SYU-KOH-HKNNUM(IDX)  =  "188"
                 IF  RECE02-FTNMONEY(1)  =  ZERO
                   MOVE  SYU-KOH-COMPFTN-ENTANI(IDX)    TO  WRK-FTNMONEY
                 ELSE
                   MOVE  SYU-KOH-COMPFTN(IDX)           TO  WRK-FTNMONEY
                 END-IF
             END-IF
      *
           END-PERFORM
      *
           IF    FLG-HEIYOU    =    ZERO
      *    主保険に限度がある場合は１０円単位、そうでない場合は１円単位
             IF    RECE02-FTNMONEY(1) =  ZERO
               MOVE     SYU-KOH-COMPFTN-ENTANI(1) TO  WRK-SEIKYUGK
             ELSE
               MOVE     SYU-KOH-COMPFTN(1)        TO  WRK-SEIKYUGK
             END-IF
           END-IF
      *
           ADD    WRK-FTNMONEY               TO    WRK-FTNMONEY-G
           ADD    WRK-SEIKYUGK               TO    WRK-SEIKYUGK-G
      *
           .
       800-SEIKYU-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM (1)
                                          SYU-KOH-HKNNUM (2)
                                          SYU-KOH-HKNNUM (3)
                                          SYU-KOH-HKNNUM (4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10)
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID (1)  =  WRK-KOHID   OR
                         COMB-KOHID (2)  =  WRK-KOHID   OR
                         COMB-KOHID (3)  =  WRK-KOHID   OR
                         COMB-KOHID (4)  =  WRK-KOHID
                        PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
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
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
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
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM                TO  COMB-HOSPNUM
           MOVE    SYU-PTID                   TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM            TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI2
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI2
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                  TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                 TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                 TO  MCPDATA-REC
           MOVE  "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_hknjainf"             TO  MCP-TABLE
           MOVE    "key2"                     TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE  "DBFETCH"              TO  MCP-FUNC
           MOVE  "tbl_hknjainf"         TO  MCP-TABLE
           MOVE  "key2"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
             IF  HKNJA-HKNNUM NOT = "060" AND "067"
                 GO    TO    925-HKNJAINF-READ-SEC
             END-IF
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
      *  保険情報セット
      *****************************************************************
       920-HKNNUM-SET-SEC  SECTION.
      *
             INITIALIZE                   HKNNUM-REC
             MOVE  SPA-HOSPNUM               TO  HKN-HOSPNUM
             MOVE  WRK-KOHNUM                TO  HKN-HKNNUM
             MOVE  LNK-PRTKANRI-SRYYM        TO  HKN-TEKSTYMD
             MOVE  "00"                      TO  HKN-TEKSTYMD(7:2)
             MOVE  LNK-PRTKANRI-SRYYM        TO  HKN-TEKEDYMD
             MOVE  "00"                      TO  HKN-TEKEDYMD(7:2)
             MOVE  HKNNUM-REC                TO  MCPDATA-REC
             MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_hknnum"              TO  MCP-TABLE
           MOVE    "key5"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"              TO  MCP-TABLE
               MOVE    "key5"                    TO  MCP-PATHNAME
               PERFORM  925-HKNNUM-READ-SEC
                 IF  RECE02-NYUGAIKBN = '2'
                   MOVE  HKN-HON-GAIKFTNRATE    TO  WRK-RJNRITU
                   MOVE  HKN-HON-GAIMNAIJGNGAK  TO  WRK-RJNRITUGAKU
                   DISPLAY "外来負担割合=" WRK-RJNRITU
                           "外来上限額=" WRK-RJNRITUGAKU
                 ELSE
                   MOVE  HKN-HON-NYUKFTNRATE    TO  WRK-RJNRITU
                   MOVE  HKN-HON-NYUMJGNGAK     TO  WRK-RJNRITUGAKU
                   DISPLAY "入院負担割合=" WRK-RJNRITU
                           "入院上限額=" WRK-RJNRITUGAKU
                 END-IF
      *
             ELSE
               DISPLAY  "HKNNUM SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_hknnum"              TO  MCP-TABLE
           MOVE    "key5"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
           .
       920-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報読込
      *****************************************************************
       925-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       925-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *  患者公費情報セット
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
             INITIALIZE  PTKOHINF-REC
             MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
             MOVE  RECE02-PTID        TO  PTKOH-PTID
             MOVE  WRK-KOHNUM         TO  PTKOH-KOHNUM
             MOVE  RECE02-SRYYM(1:6)  TO  PTKOH-TEKSTYMD(1:6)
             MOVE  "99"               TO  PTKOH-TEKSTYMD(7:2)
             MOVE  RECE02-SRYYM(1:6)  TO  PTKOH-TEKEDYMD(1:6)
             MOVE  "01"               TO  PTKOH-TEKEDYMD(7:2)
      *
             DISPLAY  "PTKOH-TEKEDYMD MAE = " PTKOH-TEKEDYMD
             DISPLAY  "RECE02-TEKSTYMD = " RECE02-TEKSTYMD
      *
      *  月の途中で主保険が切り替わっていて、主保険の適用開始日が
      *  月の初日より後だったら主保険の適用開始日を設定
             IF  PTKOH-TEKEDYMD  <  RECE02-TEKSTYMD
               MOVE  RECE02-TEKSTYMD    TO  PTKOH-TEKEDYMD
             END-IF
      *
      *  月の途中で主保険が切り替わっていて、主保険の適用終了日が
      *  月の末日より前だったら主保険の適用終了日を設定
             PERFORM  910-PTHKNINF-READ-SEC
      *
             DISPLAY  "PTKOH-TEKSTYMD MAE = " PTKOH-TEKSTYMD
             DISPLAY  "PTHKN-TEKEDYMD = " PTHKN-TEKEDYMD
      *
             IF  PTKOH-TEKSTYMD  >  PTHKN-TEKEDYMD
               MOVE  PTHKN-TEKEDYMD     TO  PTKOH-TEKSTYMD
             END-IF
      *
             MOVE  PTKOHINF-REC       TO  MCPDATA-REC
      *
             DISPLAY  "PTKOH-TEKSTYMD ATO = " PTKOH-TEKSTYMD
             DISPLAY  "PTKOH-TEKEDYMD ATO = " PTKOH-TEKEDYMD
      *
             MOVE  "DBSELECT"                TO  MCP-FUNC
             MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
             MOVE    "key6"                 TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"            USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
               MOVE    "key6"                 TO  MCP-PATHNAME
               PERFORM  935-PTKOHINF-READ-SEC
      *
             ELSE
               DISPLAY  "PTKOHINF SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"           TO  MCP-FUNC
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
      *    患者公費情報読込
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
             MOVE  PTKOH-FTNJANUM        TO  MF100-KOHFTNJANUM
             MOVE  PTKOH-FTNJANUM(5:3)   TO  MF100-FTNJANUM
             MOVE  PTKOH-JKYSNUM         TO  MF100-KOHJKYSNUM
                                             MF100-JKYSNUM
             MOVE  PTKOH-FTNJANUM        TO  MF100-FTNJANUM-KEY
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1  TO  FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE  SPA-HOSPNUM                     TO  PTHKN-HOSPNUM
           MOVE    RECE02-PTID                   TO  PTHKN-PTID
           MOVE    RECE02-HKNID                  TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC                  TO  MCPDATA-REC
           MOVE    "DBSELECT"                    TO  MCP-FUNC
           MOVE    "tbl_pthkninf"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_pthkninf"             TO  MCP-TABLE
               MOVE    "key"                      TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"                USING
                                                  MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_pthkninf"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
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
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
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
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
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
           CALL  "ORCDBMAIN"           USING
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
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
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
