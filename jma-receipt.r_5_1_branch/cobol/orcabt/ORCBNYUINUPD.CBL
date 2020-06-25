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
       PROGRAM-ID.             ORCBNYUINUPD.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 入院会計点数置換（令和元年１０月増税に伴う点数変更）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  19/09/17    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *
           COPY    "CPRECEERR.INC".
      *
      *    一覧
           COPY    "HCMSL55.INC".
      *
      *
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-NYUINACCTARI            PIC 9(01).
           03  FLG-NYUINACCT-MAE-CHK       PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
           03  FLG-NG                      PIC 9(01).
           03  FLG-UPD                     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXB                        PIC 9(05).
           03  TN-IDX                      PIC 9(02).
           03  IDX-DAY                     PIC 9(02).
           03  IDX-SKJ                     PIC 9(02).
           03  IDX-GAIHAKU                 PIC 9(02).
           03  IDX-HKN                     PIC 9(02).
           03  IDX-KSN                     PIC 9(02).
           03  KIKAN-IDX1                  PIC 9(04).
           03  KIKAN-IDX2                  PIC 9(04).
           03  IDX-SKJ-TAIHI               PIC 9(02).
           03  SC-IDX                      PIC 9(02).
           03  SC-IDX2                     PIC 9(02).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID              PIC 9(07).
           03  WRK-PARA-SHELLID            PIC X(08).
           03  WRK-PARA-HOSPNUM            PIC 9(02).
           03  WRK-PARA-SRYYM              PIC X(06).
           03  WRK-PARA-BTUNUM             PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
           03  WRK-ERRNO                   PIC 9(2).
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-GENSAN                  PIC 9(08)V999.
           03  WRK-TNS-YMD                 PIC X(08).
           03  WRK-NAIYO                   PIC X(100).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
      *
      *    入院剤再計算点数
           03  WRK-TOTAL-TEN               PIC 9(08)V999.
           03  WRK-TOTAL-TEN2              PIC 9(08).
           03  WRK-PRT-TEN                 PIC ZZ,ZZZ.
      *    入院会計更新キー
           03  UPD-KEY.
             05  UPD-HOSPNUM               PIC 9(02).
             05  UPD-NYUGAIKBN             PIC X(01).
             05  UPD-PTID                  PIC 9(10).
             05  UPD-SRYKA                 PIC X(02).
             05  UPD-SRYYM                 PIC X(06).
             05  UPD-SRYKBN                PIC X(02).
             05  UPD-ZAINUM                PIC 9(08).
      *
       01  LST-AREA.
           03  LST-HEAD1.
               05                      PIC X(40)   VALUE   " ".
               05                      PIC X(42)   VALUE   
                         "入院会計点数置換結果リスト（消費増税対応）".
               05                      PIC X(15)   VALUE   " ".
               05  LST-HEAD-PAGE       PIC Z,ZZ9.
               05                      PIC X(05)   VALUE   "頁".
           03  LST-HEAD.
               05                      PIC X(04)   VALUE   "番号".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(20)   VALUE   "患者番号".
               05                      PIC X(01)   VALUE   " ".
               05                      PIC X(26)   VALUE   "氏名".
               05                      PIC X(05)   VALUE   " ".
               05                      PIC X(08)   VALUE   "対象年月".
               05                      PIC X(03)   VALUE   " ".
               05                      PIC X(30)   VALUE   "処理結果".
      *
           03  LST-MEISAI-G.
               05  LST-MEISAI-OCC      OCCURS      50.
                   07  LST-NO          PIC Z(04).
                   07  LST-FIL1        PIC X(01).
                   07  LST-PTNUM       PIC X(20).
                   07  LST-FIL2        PIC X(01).
                   07  LST-NAME        PIC X(30).
                   07  LST-FIL4        PIC X(01).
                   07  LST-SRYYM       PIC X(08).
                   07  LST-FIL5        PIC X(03).
                   07  LST-NAIYO       PIC X(52).
      *
       01  CONST-AREA.
           03  CONST-LST-HEAD-LINE     PIC 9(05)   VALUE   2.
           03  CONST-LST-MEISAI-MAX    PIC 9(05)   VALUE   50.
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK2009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    入院行為照会用テーブル作成
           COPY  "CPORCSS010.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
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
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
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
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  PARA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBNYUINUPD"      TO  JOB-PGID
           MOVE    "入院会計点数置換（消費税）"  TO  JOB-SHELLMSG
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      **     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      **     MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    "201910"            TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-PREFNUM TO  SPA-PREFNUM
                   ELSE
                         MOVE  "医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         GO  TO  100-INIT-EXT
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    主科情報取得
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
               IF      SYS-2009-SYUKA-KBN  =  "1"
                   MOVE   "1"          TO  SPA-SYUKA-KBN
                   MOVE   SYS-2009-SYUKA-NYUIN
                                       TO  SPA-SYUKA-NYUIN
                   MOVE   SYS-2009-SYUKA-MODE
                                       TO  SPA-SYUKA-MODE
                   MOVE   SYS-2009-SYUKA-SELECT
                                       TO  SPA-SYUKA-SELECT
               ELSE
                   MOVE   "0"          TO  SPA-SYUKA-KBN
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票初期処理
           PERFORM 2001-HCMSL55-INIT-SEC
      *
      *    置換対象の入院会計検索
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    WRK-PARA-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    "201910"            TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key66"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key66"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM UNTIL ( FLG-NYUINACCT       NOT =   ZERO )
      *
      *            入院会計更新処理
                   MOVE       NACCT-PTID       TO  WRK-PTID
                   PERFORM    2001-NYUINACCT-UPD-SEC
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key66"             TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key66"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( CNT-LINE            >   ZERO )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票初期処理
      *****************************************************************
       2001-HCMSL55-INIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
                                           CNT-PAGE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2001-HCMSL55-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計更新処理
      *****************************************************************
       2001-NYUINACCT-UPD-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-UPD
           MOVE    ZERO                TO  WRK-TOTAL-TEN
      *
      *    入院行為テーブルの検索
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           PERFORM 910-NYUINACT-READ-SEC
      *
      *
           PERFORM    VARYING   IDX   FROM  1  BY  1
             UNTIL    (NSRY-SRYCD(IDX)   =    SPACE)   OR
                      (IDX               >    5)
               IF      NSRY-SRYCD(IDX)       NOT =  SPACE
                   MOVE    NSRY-SRYCD(IDX)   TO  WRK-SRYCD
                   PERFORM    48011-TENSU-SEL-SEC
                   IF  TNS-TENSIKIBETU     =   3
                       ADD    TNS-TEN      TO  WRK-TOTAL-TEN
                   END-IF
                   IF  TNS-TENSIKIBETU     =   6
                       COMPUTE  WRK-GENSAN =   WRK-TOTAL-TEN   *
                                               TNS-TEN  /  100
                       COMPUTE  WRK-TOTAL-TEN  =   WRK-TOTAL-TEN  -
                                                   WRK-GENSAN
                       COMPUTE  WRK-TOTAL-TEN  =   WRK-TOTAL-TEN  +
                                                   0.5
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE  WRK-TOTAL-TEN     TO     WRK-TOTAL-TEN2
           IF   (NACCT-ZAITEN      NOT =  WRK-TOTAL-TEN2)  OR
                (NACCT-SYUTEN1     NOT =  WRK-TOTAL-TEN2)
                 MOVE    1         TO  FLG-UPD
                 MOVE    NACCT-HOSPNUM       TO  UPD-HOSPNUM
                 MOVE    NACCT-NYUGAIKBN     TO  UPD-NYUGAIKBN
                 MOVE    NACCT-PTID          TO  UPD-PTID
                 MOVE    NACCT-SRYKA         TO  UPD-SRYKA
                 MOVE    NACCT-SRYYM         TO  UPD-SRYYM
                 MOVE    NACCT-SRYKBN        TO  UPD-SRYKBN
                 MOVE    NACCT-ZAINUM        TO  UPD-ZAINUM
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院会計点数更新
           IF      FLG-UPD    =    1
                DISPLAY   "-----------------------------"
                DISPLAY   "NYUINACCT-UPDATE SHORI"
                DISPLAY   "NACCT-PTID     = " NACCT-PTID
                DISPLAY   "NACCT-SRYYM    = " NACCT-SRYYM
                DISPLAY   "NACCT-ZAITEN   = " NACCT-ZAITEN
                DISPLAY   "WRK-TOTAL-TEN  = " WRK-TOTAL-TEN
                DISPLAY   "WRK-TOTAL-TEN2 = " WRK-TOTAL-TEN2
      *
                MOVE    WRK-TOTAL-TEN2    TO  NACCT-ZAITEN
                                              NACCT-SYUTEN1
                MOVE    NYUINACCT-REC     TO  MCPDATA-REC
                MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
                MOVE    "key"             TO  MCP-PATHNAME
                PERFORM 910-DBUPDATE-SEC
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-NYUINACCT-UPD-EXT.
           EXIT.
      ******************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-HCMSL55-HEN-SEC            SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
      *    患者情報検索
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    患者番号検索
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           ADD     1                   TO  CNT-JOB-UPDCNT
           MOVE    CNT-JOB-UPDCNT      TO  LST-NO    (CNT-LINE)
           MOVE    PTNUM-PTNUM         TO  LST-PTNUM (CNT-LINE)
      *
           MOVE    PTINF-NAME          TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           IF    ( FLG-ENTRYCHK        =   2 )
               MOVE    ALL "　"        TO  LST-NAME  (CNT-LINE)
               STRING  PTINF-NAME          DELIMITED   BY  SPACE
               INTO    LST-NAME  (CNT-LINE)
               END-STRING
           ELSE
               MOVE    PTINF-NAME      TO  LST-NAME  (CNT-LINE)
           END-IF
      *
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK2-SYORI
               MOVE    LST-NAME  (CNT-LINE)
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"  USING
                                       ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  LST-NAME  (CNT-LINE)
           END-IF
      *
           MOVE    NACCT-SRYYM(1:4)    TO  LST-SRYYM (CNT-LINE)(1:4)
           MOVE    "/"                 TO  LST-SRYYM (CNT-LINE)(5:1)
           MOVE    NACCT-SRYYM(5:2)    TO  LST-SRYYM (CNT-LINE)(6:2)
      *
           IF      FLG-UPD      =   ZERO
               MOVE  "入院会計の更新はありません" TO  WRK-NAIYO
           ELSE
               MOVE  "入院会計の剤点数を更新しました　更新後点数＝"
                                                  TO  WRK-NAIYO(1:44)
               MOVE  WRK-TOTAL-TEN2    TO  WRK-PRT-TEN
               MOVE  WRK-PRT-TEN       TO  WRK-NAIYO(45:6)
               MOVE  "点"              TO  WRK-NAIYO(51:2)
           END-IF
           MOVE    WRK-NAIYO           TO  LST-NAIYO (CNT-LINE)
      *
           IF    ( CNT-LINE            >=  CONST-LST-MEISAI-MAX )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2002-HCMSL55-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2002-HCMSL55-PRT-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  LST-HEAD-PAGE
           MOVE    LST-HEAD1           TO  HCMSL55-MOJI (1)
           MOVE    LST-HEAD            TO  HCMSL55-MOJI (2)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LST-MEISAI-MAX )
      *
               MOVE    LST-MEISAI-OCC (IDX0)
                                       TO  HCMSL55-MOJI
                                          (IDX0 + CONST-LST-HEAD-LINE)
      *
           END-PERFORM
      *
           PERFORM 290-PRINT-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2002-HCMSL55-PRT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       290-PRINT-SEC                   SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE   "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "入院会計点数置換（消費増税対応）"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       290-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9               TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *
      *    全角文字の場合、間に半角スペースがなかったかチェック
           IF    ( KANACHK-SYUBETU     =   2  )
            AND  ( KANACHK-RC2         =   8  )
               MOVE    9               TO  FLG-ENTRYCHK
           ELSE
               MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       48011-TENSU-SEL-SEC              SECTION.
      *
      *
           MOVE    ZERO            TO  FLG-TENSU
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  TNS-HOSPNUM
           IF      WRK-SRYCD       NOT = SPACE
               MOVE   WRK-SRYCD    TO  TNS-SRYCD
           END-IF
           MOVE    "201910"        TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
               MOVE    ZERO        TO  FLG-TENSU
           ELSE
               DISPLAY "TENSU-READ INVALID"
               DISPLAY "WRK-SRYCD = " WRK-SRYCD
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       48011-TENSU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO              TO  FLG-DBRC
           MOVE    WRK-MCP-TABLE     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME  TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC            =   ZERO )
               MOVE    WRK-MCP-TABLE     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME  TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-SELECT-SEC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-READ-SEC
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-SELECT-SEC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-PTINF-READ-SEC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *      
      *****************************************************************
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター検索
      *****************************************************************
       910-PTNUM-SELECT-SEC        SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込
      *****************************************************************
       910-PTNUM-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター検索
      *****************************************************************
       910-PTINF-SELECT-SEC        SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスター読込
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *      
