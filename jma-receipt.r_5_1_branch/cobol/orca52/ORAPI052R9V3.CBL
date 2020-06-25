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
       PROGRAM-ID.         ORAPI052R9V3.
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
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXE                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
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
           03  WRK-TABLE               PIC X(64).
           03  WRK-PATHNAME            PIC X(64).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-MODE-G.
               05  CONST-MODE-INFO     PIC X(02)   VALUE "00".
               05  CONST-MODE-RUN      PIC X(02)   VALUE "01".
               05  CONST-MODE-STATUS   PIC X(02)   VALUE "02".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY    "CPORCSTEMPDIR.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *    API XML用定義体
           COPY    "CPAPIV3REQ.INC".
           COPY    "CPMEV3REQ.INC".
           COPY    "CPMEV3RES.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY   "MCPAREA".
            COPY   "ORCA-SPA".
            COPY   "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
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
           DISPLAY     "*************************"
           DISPLAY     "* master_exportv3 start *"
           DISPLAY     "*************************"
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
           DISPLAY     "***********************"
           DISPLAY     "* master_exportv3 end *"
           DISPLAY     "***********************"
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
           INITIALIZE                  SPA-AREA
           INITIALIZE                  MEREQ-AREA
           INITIALIZE                  MERES-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "E99"            TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
      *
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
      *
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 900-MEREQ-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    MEREQ-REQNUM
           WHEN    "00"
               CALL    "ORAPI052R9V3S00" USING
                                         SCRAREA
                                         MEREQ-AREA
                                         MERES-AREA
                                         SPA-AREA
           WHEN    "01"
               CALL    "ORAPI052R9V3S01" USING
                                         SCRAREA
                                         MEREQ-AREA
                                         MERES-AREA
                                         SPA-AREA
           WHEN    "02"
               CALL    "ORAPI052R9V3S02" USING
                                         SCRAREA
                                         MEREQ-AREA
                                         MERES-AREA
                                         SPA-AREA
      *
           WHEN    OTHER
               MOVE    "E91"        TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           PERFORM 990-TERMINATE-SEC
      *
           .
       300-END-EXT.
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    SPA-ERRCD
           WHEN    SPACE
           WHEN    ZERO
               EVALUATE    MEREQ-REQNUM
               WHEN    "00"
                   MOVE    "正常終了"      TO  SPA-ERRMSG
               WHEN    "01"
                   MOVE    "処理実施終了"  TO  SPA-ERRMSG
               WHEN    "02"
                   MOVE    "処理確認終了"  TO  SPA-ERRMSG
               END-EVALUATE
           WHEN    "E04"
               MOVE    "電子カルテＵＩＤが未設定です。"
                                           TO  SPA-ERRMSG
           WHEN    "E05"
               MOVE    "オルカＵＩＤが未設定です。"
                                           TO  SPA-ERRMSG
           WHEN    "E11"
               MOVE    "基準日が暦日ではありません。"
                                           TO  SPA-ERRMSG
           WHEN    "E12"
               MOVE    "マスタＩＤが未設定です。"
                                           TO  SPA-ERRMSG
           WHEN    "E13"
               MOVE    "該当するマスタが存在しません。"
                                       TO  SPA-ERRMSG
           WHEN    "E20"
               MOVE    "ジョブ管理が削除できませんでした。"
                                           TO  SPA-ERRMSG
           WHEN    "E21"
               MOVE    "ジョブ管理が更新できませんでした。"
                                           TO  SPA-ERRMSG
           WHEN    "E22"
               MOVE    "システム管理が削除できませんでした。"
                                           TO  SPA-ERRMSG
           WHEN    "E23"
               MOVE    "システム管理が更新できませんでした。"
                                           TO  SPA-ERRMSG
      *
           WHEN    "E40"
               MOVE    "対象のジョブ管理がありません。"
                                           TO  SPA-ERRMSG
           WHEN    "E41"
               MOVE    "対象の処理はありません。"
                                           TO  SPA-ERRMSG
      *
           WHEN    "E88"
               MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                           TO  SPA-ERRMSG 
           WHEN    "8900"
               MOVE    "システム項目が設定できません。"
                                           TO  SPA-ERRMSG
           WHEN    "8901"
               MOVE    "職員情報が取得できません。"
                                           TO  SPA-ERRMSG
           WHEN    "8902"
               MOVE    "医療機関情報が取得できません。"
                                           TO  SPA-ERRMSG
           WHEN    "8903"
               MOVE    "システム日付が取得できません。"
                                           TO  SPA-ERRMSG
           WHEN    "8905"
               MOVE    "患者番号構成情報が取得できません。"
                                           TO  SPA-ERRMSG
           WHEN    "8915"
               STRING  "グループ医療機関が不整合です。"
                                               DELIMITED  BY  SIZE
                       "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "E91"
               MOVE    "リクエスト番号が不正です。"
                                           TO  SPA-ERRMSG
           WHEN    "E97"
               MOVE   "送信内容に誤りがあります。"
                                           TO  SPA-ERRMSG
           WHEN    "E98"
               MOVE   "送信内容の読込ができませんでした。"
                                           TO  SPA-ERRMSG
           WHEN    "E99"
               MOVE    "ユーザＩＤが未登録です。"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           IF    ( SPA-ERRCD (1:2)         =   "89" )
               MOVE    "E89"               TO  SPA-ERRCD
           END-IF
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-MEREQ-READ-SEC              SECTION.
      *
           INITIALIZE                  APIREQ-AREA
           MOVE    "xml_master_exportv3req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST11-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "master_exportv3req"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC          =   ZERO  OR  1 )
               MOVE    APIREQ-AREA TO  MEREQ-AREA
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"            TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST11-BODY
      *
           .
       900-MEREQ-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC               SECTION.
      *
           MOVE    MEREQ-REQNUM        TO  MERES-REQNUM
      *
           IF    ( MEREQ-REQNUM    NOT =   ZERO )
               MOVE    MEREQ-KARTE-UID
                                       TO  MERES-KARTE-UID
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  MERES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  MERES-INFORMATION-TIME
      *
           IF    ( SPA-ERRCD       =   "E70" OR "E30" )
               MOVE    SPA-ERRCD       TO  MERES-API-RESULT
           ELSE
               PERFORM 890-ERRCD-MSG-SEC
               IF    ( SPA-ERRCD       =   ZERO )
                   MOVE    "000"       TO  MERES-API-RESULT
               ELSE
                   MOVE    SPA-ERRCD   TO  MERES-API-RESULT
               END-IF
               MOVE    SPA-ERRMSG      TO  MERES-API-RESULT-MSG
           END-IF
      *
           MOVE    "xml_master_exportv3res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    MERES-AREA          TO  APIREQ-AREA
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "master_exportv3res"    TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  APILST11-BODY
               MOVE    "application/xml"
                                       TO  APILST11-CONTENT-TYPE
            ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XMLREAD処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLREAD-EXT.
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
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "E99" )
               MOVE    404             TO  APILST11-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           PERFORM 990-TERMINATE-SEC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    終了前実行処理
      *****************************************************************
       990-TERMINATE-SEC               SECTION.
      *
      *
           .
       990-TERMINATE-EXT.
           EXIT.
