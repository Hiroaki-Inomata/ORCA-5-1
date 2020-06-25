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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI052R9V3S02.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : マスタ取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TERM                    PIC X(64).
           03  WRK-WINDOW                  PIC X(64).
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY             PIC 9(04).
               05  WRK-DATE-FL1            PIC X(01).
               05  WRK-DATE-MM             PIC 9(02).
               05  WRK-DATE-FL2            PIC X(01).
               05  WRK-DATE-DD             PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH              PIC 9(02).
               05  WRK-HMS-MM              PIC 9(02).
               05  WRK-HMS-SS              PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH             PIC 9(02).
               05  WRK-TIME-FL1            PIC X(01).
               05  WRK-TIME-MM             PIC 9(02).
               05  WRK-TIME-FL2            PIC X(01).
               05  WRK-TIME-SS             PIC 9(02).
      *
           03  WRK-RECORD-COUNT-X.
               05  WRK-RECORD-COUNT        PIC 9(10).
           03  WRK-JOB-PARAM.
               05  WRK-JOB-JOBNAME         PIC X(16).
      *
       01  CONST-AREA.
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD        PIC X(08) VALUE "052".
      *    印刷管理ＤＢのキー値
           03  CONST-TBL-KEY           PIC X(08) VALUE "MONTHLY".
           03  CONST-TBL-GROUP         PIC X(14) VALUE SPACE.
      *    ジョブ管理ＤＢのキー値
           03  CONST-SHELLID           PIC X(08) VALUE "MONTHLY".
           03  CONST-PARAM-ME          PIC X(16) VALUE "MASTER_EXPORT".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK3003.INC".
      *
      *    API XML用定義体
           COPY    "CPAPIV3REQ.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
           COPY    "CPMEV3REQ.INC".
           COPY    "CPMEV3RES.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               MEREQ-AREA
                                               MERES-AREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  MERES-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KARTE-UID-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JOBAKANRI-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カルテＵＩＤ・オルカＵＩＤチェック
      *****************************************************************
       2001-KARTE-UID-CHK-SEC              SECTION.
      *
           IF    ( MEREQ-KARTE-UID     =   SPACE )
               MOVE    "E04"           TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( MEREQ-ORCA-UID  =   SPACE )
                   MOVE    "E05"       TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-KARTE-UID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理状況チェック
      *****************************************************************
       2001-JOBAKANRI-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
      *
           MOVE    ZERO                TO  WRK-RECORD-COUNT
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3003"              TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC TO SYS-3003-REC
           END-IF
      *
           PERFORM VARYING IDX1 FROM   1   BY  1
                   UNTIL ( IDX1 >      SYS-3003-JOBID )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
      *        ジョブ管理チェック処理
               MOVE    "CH2"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    IDX1            TO  JOB-JOBID
               MOVE    CONST-SHELLID   TO  JOB-SHELLID
               PERFORM 800-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   PERFORM 20011-JOBAKANRI-CHK-SEC
               ELSE
                   MOVE    "E40"      TO  SPA-ERRCD
               END-IF
      *
           END-PERFORM
      *
           .
       2001-JOBAKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理状況チェック
      *****************************************************************
       20011-JOBAKANRI-CHK-SEC              SECTION.
      *
           MOVE    JOB-PARAM         TO  WRK-JOB-PARAM
      *
           IF    ( MEREQ-KARTE-UID     NOT =   JOB-KARTE-UID )
            OR   ( MEREQ-ORCA-UID      NOT =   JOB-ORCA-UID  )
            OR   ( WRK-JOB-JOBNAME     NOT =   CONST-PARAM-ME  )
               MOVE    "E41"         TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   =    SPACE )
               IF    ( JOB-ENDYMD    =  SPACE )
                   MOVE    "E70"     TO  SPA-ERRCD
                   MOVE    SPACE     TO  MERES-API-RESULT-MSG
                   STRING  "処理中です【"  DELIMITED  BY  SIZE
                           JOB-SHELLMSG    DELIMITED  BY  SPACE
                           "】"            DELIMITED  BY  SIZE
                   INTO  MERES-API-RESULT-MSG
                   END-STRING
               ELSE
                   IF    ( JOB-ERRCD       =   SPACE   )
                   OR    ( JOB-ERRCD       =   "9998"  )
                       EVALUATE    WRK-JOB-JOBNAME
                       WHEN    CONST-PARAM-ME
                           MOVE    JOB-UPDCNT  TO  WRK-RECORD-COUNT
                           MOVE    WRK-RECORD-COUNT-X
                                               TO  MERES-RECORD-COUNT
                       END-EVALUATE
                   ELSE
                       MOVE    "E30"           TO  SPA-ERRCD
                       MOVE    SPACE           TO  MERES-API-RESULT-MSG
                       STRING  JOB-YOBI        DELIMITED  BY  SPACE
                               "【"            DELIMITED  BY  SIZE
                               JOB-SHELLMSG    DELIMITED  BY  SPACE
                               "】"            DELIMITED  BY  SIZE
                       INTO    MERES-API-RESULT-MSG
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-JOBAKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC              SECTION.
      *
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                         SECTION.
      *
           MOVE    MEREQ-ORCA-UID      TO  MERES-ORCA-UID
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "0000"          TO  SPA-ERRCD
               MOVE    "02"            TO  MERES-RESNUM
           ELSE
               MOVE    "01"            TO  MERES-RESNUM
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
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
      *    XMLWRITE処理
      *****************************************************************
       910-XMLWRITE-SEC                 SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLWRITE-EXT.
           EXIT.
