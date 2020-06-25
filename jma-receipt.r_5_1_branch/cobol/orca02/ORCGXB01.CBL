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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGXB01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ジョブ一覧
      *  コンポーネント名  : ジョブ一覧（ＸＢ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/11/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    19/02/07  API在庫管理を月次統計で表示する
      *  05.00.02    NACL-太田    19/07/16  クラウドレセ−情報削除抑止対応
      *  05.00.03    NACL-藤原    19/07/23  ＨＡＯＲＩ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ジョブ一覧共通パラメタ
           COPY    "XB01COMMON-SPA".
      *
           COPY    "XB01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
           COPY    "CMJOBNAME.INC".
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SHELLID         PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-JOBKANRI        PIC 9(01).
           03  FLG-MONBATCH        PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-JOBLIST-ROW     PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-RUNNING         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDXL                PIC 9(05).
           03  IDXP                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-XBIDMSG         PIC X(70).
           03  WRK-HMS             PIC X(06).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZ              PIC ZZZ9.
           03  WRK-JOBIDZ          PIC Z(6)9.
           03  WRK-YMD             PIC X(10).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-SHELLID         PIC X(08).
           03  WRK-DEL-SHELLID     PIC X(08).
           03  WRK-SELNUM          PIC 9(05).
           03  WRK-LIMIT-YMDHMS.
               05  WRK-LIMIT-YMD   PIC X(08).
               05  WRK-LIMIT-HMS   PIC 9(06).
           03  WRK-NOW-YMDHMS.
               05  WRK-NOW-YMD     PIC X(08).
               05  WRK-NOW-HMS     PIC X(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HENTIME-HH  PIC X(02).
               05  WRK-HENTIME-F1  PIC X(01).
               05  WRK-HENTIME-MM  PIC X(02).
               05  WRK-HENTIME-F2  PIC X(01).
               05  WRK-HENTIME-SS  PIC X(02).
      *
       01  WRK-ROW.
           05  WRK-JOBLIST-ROW     PIC S9(9)   BINARY.
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  TJOBINF-AREA.
           03  TJOBINF-MAX             PIC 9(05).
           03  TJOBINF-OCC             OCCURS  100.
               05  TJOBINF-SHELLID     PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-JOBLIST-MAX   PIC 9(03)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA". 
      *
       01  SCRAREA.
           COPY    "ORCA02SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XB01
           MOVE    SPA-WORK        TO  SPA-XB01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-XB01KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XB01        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG         =   "XBER" )
               MOVE    SPACE          TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END        =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "XB01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF    ( SPA-MOTOPG          =   "XBID" )
               CONTINUE
           ELSE
               IF    ( LINKAREA        NOT =   SPACE )
                   INITIALIZE              SPA-XB01-AREA
                                           SPA-XB01KYOUTU
                   MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
                   MOVE    SPA-MOTOPG  TO  SPA-XB01-MOTOPG
                   IF      SPA-XB01-MOTOPG     =   "XA01"  OR  "XG01"
                                               OR  "XG04"
                       MOVE    SPA-WORK        TO  SPA-XB01-LN-MOTOPG
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "XBID"
               PERFORM 330-XBID-SET-SEC
           WHEN    OTHER
      *
               PERFORM 310-SPASET-SEC
      *
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-XB01
      *
           MOVE    10              TO  SPA-NAI-DURATION
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    1               TO  FLG-JOBLIST-ROW
           PERFORM 3101-JOBLIST-EDIT-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コラムリスト編集処理
      *****************************************************************
       3101-JOBLIST-EDIT-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN-JOBLIST-AREA
                                           SPA-NAI-JOBLIST-AREA
                                           TJOBINF-AREA
      *
           MOVE    ZERO                TO  IDXL
      *
           PERFORM 900-JOBKANRI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-JOBKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-JOBLIST-MAX >=  CONST-JOBLIST-MAX )
      *
               PERFORM 31011-JOBLIST-EDIT-SEC
      *
               PERFORM 900-JOBKANRI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_jobkanri"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 900-MONBATCH-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( SPA-GMN-JOBLIST-MAX >=  CONST-JOBLIST-MAX )
      *
               IF    ( SHELLTBL-SHELLID    =   WRK-DEL-SHELLID )
                   CONTINUE
               ELSE
      *
                   MOVE    ZERO            TO  CNT-SHELLID
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   SPA-GMN-JOBLIST-MAX )
      *
                       IF    ( SHELLTBL-SHELLID
                                       =   SPA-GMN-TSHELLID (IDX0) )
                           COMPUTE CNT-SHELLID =   CNT-SHELLID +  1
                           IF  ( SPA-NAI-TMONBATCH (IDXL)
                                           =   ZERO )
                               MOVE    1   TO  SPA-NAI-TMONBATCH (IDX0)
                           END-IF
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( CNT-SHELLID     =   ZERO )
                       PERFORM 31012-JOBLIST-EDIT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-MONBATCH-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-JOBLIST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    コラムリスト編集処理
      *****************************************************************
       31011-JOBLIST-EDIT-SEC          SECTION.
      *
           COMPUTE IDXL  = IDXL    +   1
      *
           MOVE    IDXL                TO  SPA-GMN-JOBLIST-MAX
      *
           IF    ( TJOBINF-MAX         =   ZERO )
               COMPUTE TJOBINF-MAX =   TJOBINF-MAX +   1
           ELSE
               IF      IDXL        >   1
                   COMPUTE IDX0    =   IDXL    -   1
                   IF      JOB-SHELLID
                                   NOT =   SPA-GMN-TSHELLID (IDX0)
                       COMPUTE TJOBINF-MAX =   TJOBINF-MAX +   1
                   END-IF
               END-IF
           END-IF
      *
           MOVE    TJOBINF-MAX         TO  SPA-GMN-TNO (IDXL)
      *
           MOVE    IDXL                TO  SPA-NAI-TNO (IDXL)
      *
           MOVE    1                   TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   MNAME-MAX )
                     OR  ( IDXP    >   1 )
      *
               IF    ( JOB-SHELLID =  MNAME-SHELLID (IDX0))
                   STRING  MNAME-JOBNAME (IDX0)    DELIMITED BY SPACE
                   INTO    SPA-GMN-TJOBNAME (IDXL)
                   WITH    POINTER IDXP
                   END-STRING
               END-IF
      *
           END-PERFORM
           STRING  JOB-SHELLMSG        DELIMITED BY SPACE
           INTO    SPA-GMN-TJOBNAME (IDXL)
           WITH    POINTER IDXP
           END-STRING
      *
           MOVE    JOB-JOBID           TO  SPA-GMN-TJOBID (IDXL)
           MOVE    JOB-SHELLID         TO  SPA-GMN-TSHELLID (IDXL)
      *
           MOVE    JOB-STARTYMD    TO  SPA-NAI-TSTYMD (IDXL)
           IF    ( JOB-STARTYMD        NOT =   SPACE )
               MOVE    JOB-STARTYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSTYMD (IDXL)
           END-IF
      *
           IF    ( JOB-STARTTIME       NOT =   SPACE )
               MOVE    JOB-STARTTIME   TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-HENTIME     TO  SPA-GMN-TSTHMS (IDXL)
           END-IF
      *
           MOVE    JOB-ENDYMD      TO  SPA-NAI-TEDYMD (IDXL)
           IF    ( JOB-ENDYMD          NOT =   SPACE )
               MOVE    JOB-ENDYMD      TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TEDYMD (IDXL)
           END-IF
      *
           IF    ( JOB-ENDTIME         NOT =   SPACE )
               MOVE    JOB-ENDTIME     TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-HENTIME     TO  SPA-GMN-TEDHMS (IDXL)
           END-IF
      *
           IF    ( JOB-STEPSTARTTIME   NOT =   SPACE )
               MOVE    JOB-STEPSTARTTIME
                                       TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-HENTIME     TO  SPA-GMN-TSTEPSTHMS (IDXL)
           END-IF
      *
           IF    ( JOB-STEPENDTIME     NOT =   SPACE )
               MOVE    JOB-STEPENDTIME TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-HENTIME     TO  SPA-GMN-TSTEPEDHMS (IDXL)
           END-IF
      *
           MOVE    JOB-ERRCD           TO  SPA-GMN-TERRCD (IDXL)
           MOVE    JOB-YOBI            TO  SPA-NAI-TYOBI  (IDXL)
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
            AND  ( JOB-SHELLID         =   "RECEPT1" )
               MOVE    "1"             TO  SPA-NAI-TSTATUS  (IDXL)
           END-IF
      *
           .
       31011-JOBLIST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    コラムリスト編集処理（MONBATCHより）
      *****************************************************************
       31012-JOBLIST-EDIT-SEC          SECTION.
      *
           COMPUTE IDXL  = IDXL    +   1
      *
           MOVE    IDXL                TO  SPA-GMN-JOBLIST-MAX
      *
           MOVE    2                   TO  SPA-NAI-TMONBATCH (IDXL)
      *
           COMPUTE TJOBINF-MAX     =   TJOBINF-MAX +   1
      *
           MOVE    TJOBINF-MAX         TO  SPA-GMN-TNO (IDXL)
           MOVE    IDXL                TO  SPA-NAI-TNO (IDXL)
      *
           MOVE    1                   TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   MNAME-MAX )
                     OR  ( IDXP    >   1 )
      *
               IF    ( SHELLTBL-SHELLID =  MNAME-SHELLID (IDX0) )
                   STRING  MNAME-JOBNAME (IDX0)    DELIMITED BY SPACE
                   INTO    SPA-GMN-TJOBNAME (IDXL)
                   WITH    POINTER IDXP
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
           MOVE    SHELLTBL-SHELLID    TO  SPA-GMN-TSHELLID (IDXL)
      *
           IF    ( SHELLTBL-STARTTIME  NOT =   SPACE )
               MOVE    SPACE           TO  WRK-SYMD
               MOVE    SHELLTBL-STARTTIME (1:4)
                                       TO  WRK-SYMD(1:4)
               MOVE    SHELLTBL-STARTTIME (6:2)
                                       TO  WRK-SYMD(5:2)
               MOVE    SHELLTBL-STARTTIME (9:2)
                                       TO  WRK-SYMD(7:2)
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSTYMD (IDXL)
      *
               MOVE    SPACE           TO  WRK-HMS
               MOVE    SHELLTBL-STARTTIME (12:2)
                                       TO  WRK-HMS (1:2)
               MOVE    SHELLTBL-STARTTIME (15:2)
                                       TO  WRK-HMS (3:2)
               MOVE    SHELLTBL-STARTTIME (18:2)
                                       TO  WRK-HMS (5:2)
               PERFORM 800-TIME-SEC
               MOVE    WRK-HENTIME     TO  SPA-GMN-TSTHMS (IDXL)
           END-IF
      *
           .
       31012-JOBLIST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ）ＯＫ処理
      *****************************************************************
       330-XBID-SET-SEC            SECTION.
      *
           IF    ( SPA-XBID-FLG    =   "OK" )
               EVALUATE    SPA-XBIDCD
               WHEN    "3001"
                   PERFORM 4201-DELETE-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-XBID-FLG
           MOVE    SPACE           TO  SPA-XBIDCD
           .
       330-XBID-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    終了
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-DELETE-SEC
      *    状況更新
           WHEN    "CLICKED"       ALSO    "pandatimer1"
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 490-REFRESH-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
      *    行選択
           WHEN    ANY             ALSO    "JOBLIST"
               PERFORM 4100-JOBLIST-SELECT-SEC
           WHEN    OTHER
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
          IF    ( SPA-ERRCD    =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
          END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    XB01-SELNUM     TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-JOBLIST-SEL
           MOVE    SPACE           TO  SPA-NAI-SHELLID
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )
               CONTINUE
           ELSE
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL        >   SPA-GMN-JOBLIST-MAX )
                        OR   ( SPA-GMN-JOBLIST-SEL
                                           >   ZERO )
      *
                   IF    ( SPA-GMN-TNO (IDXL)
                                   =   SPA-GMN-SELNUM )
                       MOVE    SPA-GMN-SELNUM
                                   TO  SPA-GMN-JOBLIST-SEL
                       MOVE    SPA-GMN-TSHELLID (IDXL)
                                   TO  SPA-NAI-SHELLID
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SPA-GMN-JOBLIST-SEL =    ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4100-JOBLIST-SELECT-SEC             SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-JOBLIST-MAX )
               IF    ( XB01-JOBLIST-SELECT (IDXL)   =   "T" )
                   IF      SPA-GMN-TNO (IDXL)
                                           =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-TNO (IDXL)
                                               TO  WRK-SELNUM
                       MOVE    SPA-GMN-JOBLIST-MAX
                                               TO  IDXL
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  XB01-SELNUM
           END-IF
      *
      **** MOVE    SPA-GMN-SELNUM      TO  XB01-SELNUM
           PERFORM 4100-SELNUM-CHK-SEC
           MOVE    202                 TO  SPA-GMN-CUR
      *
           .
       4100-JOBLIST-SELECT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 210-BACK-SYORI-SEC
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    SPA-XB01-MOTOPG     TO  SPA-SAKIPG
           MOVE    "XB01"              TO  SPA-MOTOPG
      *
           IF      SPA-XB01-MOTOPG     =   "XA01"  OR  "XG01"
                                       OR  "XG04"
               MOVE    SPA-XB01-LN-MOTOPG  TO  SPA-WORK
           END-IF
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除処理
      *****************************************************************
       420-DELETE-SEC                SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    IF    ( SPA-ERRCD   =   SPACE )
      *        PERFORM 4201-RUNNING-JOB-CHK-SEC
      *    END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-GMN-JOBLIST-SEL  =   ZERO )
                OR   ( SPA-NAI-SHELLID =   SPACE )
                   MOVE    "1101"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-GMN-JOBLIST-SEL  >   ZERO )
                AND  ( SPA-NAI-TSTATUS (SPA-GMN-JOBLIST-SEL)
                                            =   "1" )
                   PERFORM 4201-UNDELETABLE-TIME-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               MOVE    "3001"  TO  SPA-XBIDCD
           END-IF
      *
           .
       420-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除処理
      *****************************************************************
       4201-DELETE-SEC                 SECTION.
      *
      *    IF    ( SPA-ERRCD   =   SPACE )
      *        PERFORM 4201-RUNNING-JOB-CHK-SEC
      *    END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-JOBLIST-SEL  >   ZERO )
                AND  ( SPA-NAI-TSTATUS (SPA-GMN-JOBLIST-SEL)
                                            =   "1" )
                   PERFORM 4201-UNDELETABLE-TIME-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 42011-DELETE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 42012-DELETE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    SPA-NAI-SHELLID TO  WRK-DEL-SHELLID
      *
               MOVE    1               TO  FLG-JOBLIST-ROW
               PERFORM 3101-JOBLIST-EDIT-SEC
           END-IF
      *
           .
       4201-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    情報削除処理
      *****************************************************************
       42011-DELETE-SEC                SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    SPA-NAI-SHELLID TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    NOT =   ZERO )
               MOVE    "1102"      TO  SPA-ERRCD
           END-IF
      *
           .
       42011-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    情報削除処理
      *****************************************************************
       42012-DELETE-SEC                 SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           PERFORM 900-MONBATCH-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               IF    ( SHELLTBL-SHELLID    =   SPA-NAI-SHELLID )
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    LOW-VALUE       TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
      *            IF    ( MCP-RC          NOT =   ZERO )
      *                MOVE    "1103"      TO  SPA-ERRCD
      *            END-IF
               END-IF
      *
               PERFORM 900-MONBATCH-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       42012-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    実行中ジョブチェック処理
      *****************************************************************
       4201-RUNNING-JOB-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-RUNNING
      *
           PERFORM 900-MONBATCH-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-RUNNING     NOT =   ZERO )
      *
               IF    ( SHELLTBL-SHELLID    =   SPA-NAI-SHELLID )
      *
                   MOVE    1           TO  FLG-RUNNING
      *
               END-IF
      *
               PERFORM 900-MONBATCH-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-RUNNING         =   1 )
      *
               PERFORM 900-JOBKANRI-KEY4-SEL-SEC
      *
               PERFORM UNTIL ( FLG-JOBKANRI    NOT =   ZERO )
                        OR   ( SPA-ERRCD       NOT =   SPACE )
      *
                   IF    ( JOB-SHELLID =   SPA-NAI-SHELLID )
                    AND  ( JOB-STOPFLG =   3 )
                    AND  ( JOB-ENDYMD  =   SPACE )
                       MOVE    "1104"  TO  SPA-ERRCD
                   END-IF
      *
                   PERFORM 900-JOBKANRI-KEY4-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       4201-RUNNING-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    削除抑止時間チェック処理
      *****************************************************************
       4201-UNDELETABLE-TIME-CHK-SEC   SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    SPA-NAI-SHELLID TO  JOB-SHELLID
           MOVE    SPA-GMN-TJOBID (SPA-GMN-JOBLIST-SEL)
                                   TO  JOB-JOBID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
            AND  ( JOB-ENDYMD          =   SPACE )
      *
               MOVE    JOB-STARTYMD    TO  WRK-LIMIT-YMD
               MOVE    FUNCTION NUMVAL(JOB-STARTTIME)
                                       TO  WRK-LIMIT-HMS
               COMPUTE WRK-LIMIT-HMS   =   WRK-LIMIT-HMS   +   20000
               IF    ( WRK-LIMIT-HMS  >=   240000 )
      *
                   COMPUTE WRK-LIMIT-HMS
                       =   WRK-LIMIT-HMS   -   240000
                   MOVE    JOB-STARTYMD    TO  WRK-SYMD
                   MOVE    1               TO  WRK-ZOGEN
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-SYMD        TO  WRK-LIMIT-YMD
               END-IF
               PERFORM 800-ORCSMCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  WRK-NOW-YMD
               MOVE    SMCNDATE-HMS        TO  WRK-NOW-HMS
      D        DISPLAY "WRK-NOW-YMDHMS  :" WRK-NOW-YMDHMS
      D        DISPLAY "WRK-LIMIT-YMDHMS:" WRK-LIMIT-YMDHMS
               IF    ( WRK-NOW-YMDHMS      >   WRK-LIMIT-YMDHMS )
                   CONTINUE
               ELSE
                   MOVE    "1105"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4201-UNDELETABLE-TIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    リフレッシュ処理
      *****************************************************************
       490-REFRESH-SEC                 SECTION.
      *
           MOVE    SPA-NAI-SHELLID     TO  WRK-SHELLID
           PERFORM 3101-JOBLIST-EDIT-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-JOBLIST-SEL
                                           SPA-GMN-SELNUM
           MOVE    SPACE               TO  SPA-NAI-SHELLID
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   SPA-GMN-JOBLIST-MAX )
                    OR   ( SPA-GMN-JOBLIST-SEL
                                   >   ZERO )
      *
               IF    ( WRK-SHELLID =   SPA-GMN-TSHELLID (IDXL))
                   MOVE    SPA-GMN-TNO (IDXL)
                                       TO  SPA-GMN-JOBLIST-SEL
                                           SPA-GMN-SELNUM
                   MOVE    WRK-SHELLID TO  SPA-NAI-SHELLID
               END-IF
      *
           END-PERFORM
      *
           .
       490-REFRESH-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-XBIDCD      NOT =   SPACE )
               PERFORM 520-XBIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XB01    "          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    XB01-JOBLIST-ROW    TO  WRK-JOBLIST-ROW
      *
           MOVE    SPACE               TO  XB01
           INITIALIZE                      XB01
      *
           EVALUATE    FLG-JOBLIST-ROW
           WHEN    1
               MOVE    ZERO            TO  XB01-JOBLIST-ROW
               MOVE    ZERO            TO  XB01-JOBLIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-JOBLIST-ROW TO  XB01-JOBLIST-ROW
               MOVE    ZERO            TO  XB01-JOBLIST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-JOBLIST-MAX TO  XB01-JOBLIST-COUNT
           PERFORM VARYING     IDXL    FROM    1   BY  1
                   UNTIL     ( IDXL        >   SPA-GMN-JOBLIST-MAX )
      *
               IF    ( IDXL    =  1 )
                OR   ( SPA-GMN-TNO (IDXL)
                                   NOT =   SPA-GMN-TNO (IDXL - 1 ))
                   MOVE    SPA-GMN-TNO (IDXL)
                                       TO  WRK-ZZ
                   MOVE    WRK-ZZ      TO  XB01-TNO (IDXL)
               END-IF
      *
               MOVE    SPA-GMN-TJOBNAME (IDXL)
                                       TO  XB01-TJOBNAME (IDXL)
               MOVE    SPA-GMN-TJOBID (IDXL)
                                       TO  WRK-JOBIDZ
               MOVE    WRK-JOBIDZ  TO  XB01-TJOBID (IDXL)
               MOVE    SPA-GMN-TSHELLID (IDXL)
                                       TO  XB01-TSHELLID (IDXL)
               MOVE    SPA-GMN-TSTYMD (IDXL)
                                       TO  XB01-TSTYMD (IDXL)
               MOVE    SPA-GMN-TSTHMS (IDXL)
                                       TO  XB01-TSTHMS (IDXL)
               MOVE    SPA-GMN-TEDYMD (IDXL)
                                       TO  XB01-TEDYMD (IDXL)
               MOVE    SPA-GMN-TEDHMS (IDXL)
                                       TO  XB01-TEDHMS (IDXL)
               MOVE    SPA-GMN-TSTEPSTHMS (IDXL)
                                       TO  XB01-TSTEPSTHMS (IDXL)
               MOVE    SPA-GMN-TSTEPEDHMS (IDXL)
                                       TO  XB01-TSTEPEDHMS (IDXL)
               MOVE    SPA-GMN-TERRCD (IDXL)
                                       TO  XB01-TERRCD (IDXL)
      *
               IF    ( SPA-GMN-TSHELLID (IDXL)  =   SPA-NAI-SHELLID )
                   MOVE    "T"         TO  XB01-JOBLIST-SELECT (IDXL)
               ELSE
                   MOVE    "F"         TO  XB01-JOBLIST-SELECT (IDXL)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-JOBLIST-SEL >   ZERO )
               DISPLAY "SPA-NAI-TMONBATCH = " SPA-GMN-JOBLIST-SEL ":"
                       SPA-NAI-TMONBATCH (SPA-GMN-JOBLIST-SEL)
           END-IF
      *
           IF    ( SPA-GMN-JOBLIST-SEL >   ZERO )
            AND  ( SPA-NAI-TMONBATCH (SPA-GMN-JOBLIST-SEL) NOT =  ZERO )
            AND  ( SPA-NAI-TSTATUS   (SPA-GMN-JOBLIST-SEL) NOT =  "1" )
               MOVE    "処理終了"      TO  XB01-B02-LABEL
           ELSE
               MOVE    "情報削除"      TO  XB01-B02-LABEL
           END-IF
      *
           MOVE    "black"             TO  XB01-LBLMSG01-STYLE
      *
           IF    ( SPA-GMN-JOBLIST-MAX =   ZERO )
               MOVE    "blue"          TO  XB01-LBLMSG01-STYLE
               MOVE     "指定された条件の処理が存在しません"
                                       TO  XB01-LBLMSG01
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  XB01-SELNUM
      *
           MOVE    SPA-NAI-DURATION    TO  XB01-DURATION
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE    "SELNUM"    TO  MCP-WIDGET
           WHEN    202
               MOVE    "B02"       TO  MCP-WIDGET
           END-EVALUATE
      *
          .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    202        TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "正しい選択番号を入力してください" 
                                       TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE    "選択番号を入力してください" 
                                       TO  SPA-ERRMSG
           WHEN    "1102"
           WHEN    "1103"
           WHEN    "1104"
               MOVE    "削除不可な処理実行中です。削除できません"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               STRING  "処理中です。処理を中止する場合は"
                                           DELIMITED BY SIZE
                       "処理結果画面より中止処理を行ってください"
                                           DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  XBER
           MOVE    SPA-ERRCD            TO  XBER-ERCODE
           MOVE    SPA-ERRMSG           TO  XBER-ERMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "XB01"               TO  SPA-MOTOPG
           MOVE    "XBER"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XBER"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-XBIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-XBIDMSG
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           EVALUATE    SPA-XBIDCD
           WHEN    "3001"
               IF    ( SPA-NAI-TMONBATCH (SPA-GMN-JOBLIST-SEL)
                                    NOT =   ZERO )
                  MOVE    "処理を終了します。よろしいですか"
                                       TO  WRK-XBIDMSG
               ELSE
                  MOVE    "処理を削除します。よろしいですか"
                                       TO  WRK-XBIDMSG
               END-IF
               MOVE    "B01"           TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    SPACE                TO  XBID
           MOVE    SPA-XBIDCD           TO  XBID-IDCODE
           MOVE    WRK-XBIDMSG          TO  XBID-IDMSG
      *
           MOVE    "XB01"               TO  SPA-MOTOPG
           MOVE    "XBID"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XBID"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-XBIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得
      *****************************************************************
       800-ORCSMCNDATE-SEC                SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       800-ORCSMCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       800-HIZUKE-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           IF    ( WRK-SYMD            =   ALL "9"  OR   SPACE )
               MOVE    SPACE           TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)   TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)   TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)   TO  WRK-HDD
               MOVE    WRK-HENYMD      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-SYMD        TO  SGDAY-INDAY
               CALL    "ORCSGDAY"      USING
                                               ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時間編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           MOVE    SPACE           TO  WRK-HENTIME
      *
           MOVE    WRK-HMS (1:2)   TO  WRK-HENTIME-HH
           MOVE    ":"             TO  WRK-HENTIME-F1
           MOVE    WRK-HMS (3:2)   TO  WRK-HENTIME-MM
           MOVE    ":"             TO  WRK-HENTIME-F2
           MOVE    WRK-HMS (5:2)   TO  WRK-HENTIME-SS
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理検索処理
      *****************************************************************
       900-JOBKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-JOBKANRI
      *
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-JOBKANRI-KEY4-FET-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
               INITIALIZE                  JOBKANRI-REC
           END-IF
      *
           .
       900-JOBKANRI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理FETCH処理
      *****************************************************************
       900-JOBKANRI-KEY4-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JOBKANRI
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-JOBKANRI    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
      *
                   MOVE    MCPDATA-REC     TO  JOBKANRI-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( JOB-SHELLID         =   SPA-JOB-SHELLID )
                    OR  (( SPA-JOB-SHELLID     =  "SOKATU" )
                     AND ( JOB-SHELLID         =  "COLLECT" ))
                    OR  (( SPA-JOB-SHELLID     =  "MONTHLY" )
                     AND ( JOB-SHELLID         =  "STOCK"   ))
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-JOBKANRI
                   INITIALIZE                  JOBKANRI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-JOBKANRI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    MONBATCH検索処理
      *****************************************************************
       900-MONBATCH-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
      *
           INITIALIZE                      SHELLTBL
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-MONBATCH-FET-SEC
           ELSE
               MOVE    1               TO  FLG-MONBATCH
           END-IF
      *
           .
       900-MONBATCH-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONBATCH FETCH処理
      *****************************************************************
       900-MONBATCH-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    LOW-VALUE       TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SHELLTBL
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( SHELLTBL-HOSPNUM    NOT =  SPA-HOSPNUM )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( SHELLTBL-SHELLID    =  SPA-JOB-SHELLID )
                    OR  (( SPA-JOB-SHELLID     =  "SOKATU" )
                     AND ( SHELLTBL-SHELLID    =  "COLLECT" ))
                    OR  (( SPA-JOB-SHELLID     =  "MONTHLY" )
                     AND ( SHELLTBL-SHELLID    =  "STOCK"   ))
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-MONBATCH
               END-IF
      *
           END-PERFORM
      *
           .
       900-MONBATCH-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢＳＥＬＥＣＴ処理(FETCHはしない)
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＬＯＳＥＣＵＲＳＯＲ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

