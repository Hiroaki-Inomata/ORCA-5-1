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
       PROGRAM-ID.         ORAPI000R3V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : Push通知取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/01/25    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-MPE                 PIC 9(01).
           03  FLG-TEXT-ERR            PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-RESPONSE            PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-TEXT                PIC X(512).
           03  WRK-DATETIME.
               05  WRK-DATETIME-DATE   PIC X(10).
               05  WRK-DATETIME-FL     PIC X(01).
               05  WRK-DATETIME-TIME   PIC X(05).
      *
       01  API-AREA.
           03  API-START-TIMESTAMP.
               05  API-START-TS-DATE   PIC X(10).
               05  API-START-TS-FL     PIC X(01).
               05  API-START-TS-TIME   PIC X(08).
           03  API-END-TIMESTAMP.
               05  API-END-TS-DATE     PIC X(10).
               05  API-END-TS-FL       PIC X(01).
               05  API-END-TS-TIME     PIC X(08).
      *
           COPY    "CPPEGV2REQ.INC".
           COPY    "CPPEGV2ERR.INC".
           COPY    "CPPEGV2RES.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    文字列チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML共通定義
           COPY    "CPAPIV2REQ.INC".
           COPY    "CPAPIMSG.INC".
           COPY    "CPIMGGETV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      * MonPushEvent
        01 MPE-REC.
           COPY    "CPMPE.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*********************"
           DISPLAY   "* pusheventgetv2 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  XML-PEGREQ
           INITIALIZE                  XML-PEGERR
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "*********************"
           DISPLAY   "* pusheventgetv2 end  *"
           DISPLAY   "*********************"
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
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"           TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  SPA-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  SPA-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  SPA-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  SPA-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  SPA-ERRCD
               END-EVALUATE
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               CONTINUE
           ELSE
               MOVE    "0099"          TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-EVENT-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-USER-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-START-TIME-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-END-TIME-CHECK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 10011-KANREN-CHECK-SEC
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    イベント名チェック処理
      *****************************************************************
       10011-EVENT-CHECK-SEC           SECTION.
      *
           MOVE    PEGREQ-EVENT    TO  WRK-TEXT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-TEXT-ERR    NOT =   ZERO )
               MOVE    "0001"      TO  SPA-ERRCD
           END-IF
      *
           .
       10011-EVENT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ名チェック処理
      *****************************************************************
       10011-USER-CHECK-SEC            SECTION.
      *
           MOVE    PEGREQ-USER     TO  WRK-TEXT
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-TEXT-ERR    NOT =   ZERO )
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
       10011-USER-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    取得開始日時チェック処理
      *****************************************************************
       10011-START-TIME-CHECK-SEC      SECTION.
      *
           MOVE    "1900-01-01 00:00:00"   TO  API-START-TIMESTAMP
      *
           IF    ( PEGREQ-START-TIME (1:10)    NOT =   SPACE )
               MOVE    PEGREQ-START-TIME   TO  WRK-DATETIME
               PERFORM 100111-TIME-CHECK-SEC
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( WRK-DATETIME-DATE   NOT =   SPACE )
                       MOVE    WRK-DATETIME-DATE
                                           TO  API-START-TS-DATE
                       IF    ( WRK-DATETIME-TIME
                                               NOT =   SPACE )
                           MOVE    WRK-DATETIME-TIME
                                           TO  API-START-TS-TIME
                           MOVE    ":00"   TO  API-START-TS-TIME(6:)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       10011-START-TIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    取得終了日時チェック処理
      *****************************************************************
       10011-END-TIME-CHECK-SEC        SECTION.
      *
           MOVE    "9999-12-31 23:59:59"   TO  API-END-TIMESTAMP
      *
           IF    ( PEGREQ-END-TIME (1:10)    NOT =   SPACE )
               MOVE    PEGREQ-END-TIME     TO  WRK-DATETIME
               PERFORM 100111-TIME-CHECK-SEC
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( WRK-DATETIME-DATE   NOT =   SPACE )
                       MOVE    WRK-DATETIME-DATE
                                           TO  API-END-TS-DATE
                       IF    ( WRK-DATETIME-TIME
                                               NOT =   SPACE )
                           MOVE    WRK-DATETIME-TIME
                                           TO  API-END-TS-TIME
                           MOVE    ":59"   TO  API-END-TS-TIME(6:)
                       END-IF
                   END-IF
               ELSE
                   EVALUATE    SPA-ERRCD
                   WHEN    "0003"
                       MOVE    "0005"      TO  SPA-ERRCD
                   WHEN    "0004"
                       MOVE    "0006"      TO  SPA-ERRCD
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       10011-END-TIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    日時チェック処理
      *****************************************************************
       100111-TIME-CHECK-SEC           SECTION.
      *
           MOVE    WRK-DATETIME-DATE
                                   TO  WRK-DATE
           PERFORM 800-SYMD-SEC
           PERFORM 800-HIZUKE-SEC
           IF    ( WRK-EDTYMD1 =   SPACE )
               MOVE    "0003"  TO  SPA-ERRCD
           ELSE
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  WRK-DATETIME-DATE
      *
               IF    ( WRK-DATETIME-TIME
                                   NOT =   SPACE )
                   IF    ( WRK-DATETIME-TIME (1:2)    IS NUMERIC )
                    AND  ( WRK-DATETIME-TIME (4:2)    IS NUMERIC )
                    AND  ( WRK-DATETIME-TIME (1:2)    >=  "00" )
                    AND  ( WRK-DATETIME-TIME (1:2)    <=  "23" )
                    AND  ( WRK-DATETIME-TIME (4:2)    >=  "00" )
                    AND  ( WRK-DATETIME-TIME (4:2)    <=  "59" )
                       MOVE    ":"     TO  WRK-DATETIME-TIME(3:1)
                   ELSE
                       MOVE    "0004"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       100111-TIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       10011-KANREN-CHECK-SEC          SECTION.
      *
           IF    ( API-START-TIMESTAMP >   API-END-TIMESTAMP )
               MOVE    "0008"          TO  SPA-ERRCD
           END-IF
      *
           .
       10011-KANREN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-RESPONSE-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2001-RESPONSE-EDIT-SEC                  SECTION.
      *
           MOVE    ZERO                    TO  CNT-RESPONSE
      *
           INITIALIZE                          XML-APIMSG
           MOVE    3                       TO  APIMSG-MODE
           MOVE    "xml_pusheventgetv2res" TO  MCP-TABLE
           MOVE    LOW-VALUE               TO  MCP-PATHNAME
           PERFORM 910-MSGOPEN-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               DISPLAY "MSGOPEN failure = " MCP-RC
           END-IF
      *
           PERFORM 900-MONPUSHEVENT-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MPE     NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE                      XML-PEGRES
      *        MOVE    MPE-ID              TO  PEGRES-ID
      *        MOVE    MPE-EVENT           TO  PEGRES-EVENT
      *        MOVE    MPE-PUSHED-AT-DATE  TO  WRK-SYMD
      *        PERFORM 800-DATE-SEC
      *        MOVE    MPE-PUSHED-AT-TIME  TO  WRK-HMS
      *        PERFORM 800-TIME-SEC
      *        STRING  MPE-PUSHED-AT-DATE      DELIMITED BY SIZE
      *                " "                     DELIMITED BY SIZE
      *                MPE-PUSHED-AT-TIME(1:8) DELIMITED BY SIZE
      *        INTO    PEGRES-TIME
      *        END-STRING
      *
               MOVE    MPE-DATA            TO  PEGRES-DATA
               MOVE    XML-PEGRES          TO  XML-APIMSG
               MOVE    "xml_pusheventgetv2res"
                                           TO  MCP-TABLE
               MOVE    LOW-VALUE           TO  MCP-PATHNAME
               PERFORM 910-MSGWRITE-SEC
               IF    ( MCP-RC              =   ZERO )
                   COMPUTE  CNT-RESPONSE   =   CNT-RESPONSE + 1
               ELSE
                   MOVE    "4001"          TO  SPA-ERRCD
                   DISPLAY "MSGWRITE failure = " MCP-RC
               END-IF
      *
               PERFORM 900-MONPUSHEVENT-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "monpushevent"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                          XML-APIMSG
           MOVE    "xml_pusheventgetv2res" TO  MCP-TABLE
           MOVE    LOW-VALUE               TO  MCP-PATHNAME
           PERFORM 910-MSGUNESCAPE-SEC
           PERFORM 910-MSGCLOSE-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    APIMSG-OBJECT       TO  APILST37-BODY
               MOVE    "application/json"  TO  APILST37-CONTENT-TYPE
           ELSE
               DISPLAY "MSGCLOSE failure = " MCP-RC
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    "4002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( CNT-RESPONSE        =   ZERO )
                   MOVE    "0007"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 9901-ERR-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *
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
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
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
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
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
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
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
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TEXT-ERR
      *
           IF  ( WRK-TEXT          NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-TEXT        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-TEXT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1       TO  FLG-TEXT-ERR
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-TEXT-ERR
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC               SECTION.
      *
           MOVE        SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "イベント名に半角以外の文字が入力されています。"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "ユーザ名に半角以外の文字が入力されています。"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "期間開始日時の日付の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "期間開始日時の時刻の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "期間終了日時の日付の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "期間終了日時の時刻の入力に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "対象となるＰＵＳＨ通知が存在しませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "期間開始日時＞期間終了日時です。"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4002"
               MOVE    "更新処理に失敗しました。"
                                       TO  SPA-ERRMSG
      *共通エラー
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "グループ医療機関が不整合です。" DELIMITED BY SIZE
                      "処理を終了して下さい。"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0097"
               MOVE    "送信内容に誤りがあります。"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE    "送信内容の読込ができませんでした。"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONPUSHEVENT検索処理
      *****************************************************************
       900-MONPUSHEVENT-KEY2-SEL-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-MPE
      *
           INITIALIZE                  MPE-REC
           MOVE    PEGREQ-EVENT    TO  MPE-EVENT
           MOVE    PEGREQ-USER     TO  MPE-USER
           IF    ( API-START-TIMESTAMP NOT =   SPACE )
               STRING API-START-TIMESTAMP  DELIMITED BY SIZE
                      " JST"               DELIMITED BY SIZE
               INTO   MPE-PUSHED-FROM
               END-STRING
           END-IF
           IF    ( API-END-TIMESTAMP   NOT =   SPACE )
               STRING API-END-TIMESTAMP    DELIMITED BY SIZE
                      " JST"               DELIMITED BY SIZE
               INTO   MPE-PUSHED-TO
               END-STRING
           END-IF
           MOVE    MPE-REC         TO  MCPDATA-REC
           MOVE    "monpushevent"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  MPE-REC
           ELSE
               MOVE    1           TO  FLG-MPE
               INITIALIZE              MPE-REC
           END-IF
      *
           .
       900-MONPUSHEVENT-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONPUSHEVENT検索処理
      *****************************************************************
       900-MONPUSHEVENT-KEY2-FET-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-MPE
      *
           MOVE    "monpushevent"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  MPE-REC
           ELSE
               MOVE    1           TO  FLG-MPE
               INITIALIZE              MPE-REC
           END-IF
      *
           .
       900-MONPUSHEVENT-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST37-BODY    NOT =   LOW-VALUE )
               DISPLAY "pusheventgetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_pusheventgetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST37-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "pusheventgetv2req" TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"           TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST37-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  PEGREQ-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す（エラー時）
      *****************************************************************
       900-XML-WRITE-SEC           SECTION.
      *
           IF    ( APILST37-BODY    NOT =   LOW-VALUE )
               DISPLAY "pusheventgetv2 object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    PEGERR-AREA             TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_pusheventgetv2err" 
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    3                       TO  APIREQ-MODE
           MOVE    "error"                 TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO )
               MOVE    APIREQ-OBJECT       TO  APILST37-BODY
               MOVE    "application/xml"   TO  APILST37-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
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
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＭＳＧオープン処理
      *****************************************************************
       910-MSGOPEN-SEC                 SECTION.
      *
           MOVE    "MSGOPEN"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＭＳＧ書込処理
      *****************************************************************
       910-MSGWRITE-SEC                SECTION.
      *
           MOVE    "MSGWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGWRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＭＳＧアンエスケープ処理
      *****************************************************************
       910-MSGUNESCAPE-SEC             SECTION.
      *
           MOVE    "MSGUNESCAPE"   TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGUNESCAPE-EXT.
           EXIT.
      *****************************************************************
      *    ＭＳＧクローズ処理
      *****************************************************************
       910-MSGCLOSE-SEC                SECTION.
      *
           MOVE    "MSGCLOSE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIMSG
      *
           .
      *
       910-MSGCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  APILST37-HTTPSTATUS
           ELSE
               PERFORM 9901-ERR-RESPONSE-EDIT-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    エラー時出力処理
      *****************************************************************
       9901-ERR-RESPONSE-EDIT-SEC      SECTION.
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    SPA-ERRCD           TO  PEGERR-CODE
           MOVE    SPA-ERRMSG          TO  PEGERR-MESSAGE
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-ERR-RESPONSE-EDIT-EXT.
           EXIT.
