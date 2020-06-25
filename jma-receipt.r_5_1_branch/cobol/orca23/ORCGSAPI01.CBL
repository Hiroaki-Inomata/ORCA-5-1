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
       PROGRAM-ID.         ORCGSAPI01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 収納（請求一覧）
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
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP01FILE//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
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
           03  WRK-TEMPDIR-ROOT        PIC X(512).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-MODE-G.
      *
               05  CONST-MODE-ICHIRAN      PIC X(02)   VALUE "01".
               05  CONST-MODE-KAKUNIN      PIC X(02)   VALUE "02".
      *
               05  CONST-MODE-NYUKIN       PIC X(02)   VALUE "01".
               05  CONST-MODE-RRKSYU       PIC X(02)   VALUE "02".
               05  CONST-MODE-TORIKESHI    PIC X(02)   VALUE "03".
               05  CONST-MODE-HENKIN       PIC X(02)   VALUE "04".
               05  CONST-MODE-SAIKEISAN    PIC X(02)   VALUE "05".
               05  CONST-MODE-M-SAIKEISAN  PIC X(02)   VALUE "06".
               05  CONST-MODE-M-NYUKIN     PIC X(02)   VALUE "07".
               05  CONST-MODE-SKY-TORIKESHI
                                           PIC X(02)   VALUE "08".
      *
               05  CONST-MODE-REPRINT       PIC X(02)  VALUE "09".
               05  CONST-MODE-MONTHLY-PRINT PIC X(02)  VALUE "10".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      *    API XML用定義体
           COPY    "ORCA-BLOB".
           COPY    "CPAPIV3REQ.INC".
           COPY    "CPINCOMEV3REQ.INC".
           COPY    "CPINCOMEV3REQ01.INC".
           COPY    "CPINCOMEV3RES01.INC".
           COPY    "CPINCOMEV3REQ02.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK1039.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
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
           DISPLAY   "******************"
           DISPLAY   "* incomev3 start *"
           DISPLAY   "******************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
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
           DISPLAY   "****************"
           DISPLAY   "* incomev3 end *"
           DISPLAY   "****************"
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
           INITIALIZE                  INCOMEREQ-AREA
                                       INCOMEREQ01-AREA
                                       INCOMERES01-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
                                           SPA-SRYYMD
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           MOVE    "S"                 TO  SPA-SAKIPG
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
           PERFORM 1001-SAVE-OBJECT-SEC
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    TMP01FILE           TO  ORCA-BLOB-FILE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           PERFORM 910-BLOBIMPORT-SEC
           MOVE    ORCA-BLOB-OBJECT    TO  SAPI01-BODY
      *
           PERFORM 900-INCOMEREQ-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       1001-SAVE-OBJECT-SEC                SECTION.
      *
          MOVE     SPACE               TO  TMP01FILE-AREA
          INITIALIZE                       TMP01FILE-AREA
      *
          INITIALIZE                   STEMPDIR-AREA
          CALL     "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
          MOVE     STEMPDIR-TEMPDIR    TO  MCP-TEMPDIR
      *
           MOVE    SPA-HOSPNUM         TO  TMP01FILE-HOSPNUM
           MOVE    "API02301"          TO  TMP01FILE-FILE-ID
           MOVE    1                   TO  TMP01FILE-SYOKBN1
           MOVE    SPA-TERMID          TO  TMP01FILE-TERMID
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TMP01FILE-BASENAME  TO SGETTEMP-BASENAMES(1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TMP01FILE
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    SAPI01-BODY         TO  ORCA-BLOB-OBJECT
           MOVE    TMP01FILE           TO  ORCA-BLOB-FILE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           PERFORM 910-BLOBEXPORT-SEC
      *
           .
       1001-SAVE-OBJECT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    INCOMEREQ-REQNUM
           WHEN    "01"
           WHEN    "02"
      *
               INITIALIZE                  ORCA-BLOB
               MOVE    TMP01FILE       TO  ORCA-BLOB-FILE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "blob"          TO  MCP-TABLE
               PERFORM 910-BLOBIMPORT-SEC
               MOVE    ORCA-BLOB-OBJECT
                                       TO  SAPI01-BODY
      *
               EVALUATE    INCOMEREQ-REQNUM
               WHEN    "01"
                   PERFORM 2001-REQ01-SEC
               WHEN    "02"
                   PERFORM 2001-REQ02-SEC
               END-EVALUATE
      *
           WHEN    "99"
               CALL    "ORCGSAPI01S99" USING
                                       SCRAREA
                                       INCOMEREQ-AREA
                                       SPA-AREA
      *
           WHEN    OTHER
               MOVE    "0091"          TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    参照系処理選択処理
      *****************************************************************
       2001-REQ01-SEC                  SECTION.
      *
           EVALUATE    INCOMEREQ-REQMODE
      *    請求一覧
           WHEN    CONST-MODE-ICHIRAN
      *
               PERFORM 900-INCOMEREQ01-READ-SEC
      *
               CALL    "ORCGSAPI01S01" USING
                                       SCRAREA
                                       INCOMEREQ01-AREA
                                       SPA-AREA
      *
      *    請求確認
           WHEN    CONST-MODE-KAKUNIN
               PERFORM 900-INCOMEREQ02-READ-SEC
      *
               CALL    "ORCGSAPI01S03" USING
                                       SCRAREA
                                       INCOMEREQ02-AREA
                                       SPA-AREA
           WHEN    OTHER
               MOVE    "0092"          TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-EVALUATE
      *
           .
       2001-REQ01-EXT.
           EXIT.
      *****************************************************************
      *    更新系処理選択処理
      *****************************************************************
       2001-REQ02-SEC                  SECTION.
      *
           EVALUATE    INCOMEREQ-REQMODE
           WHEN    CONST-MODE-NYUKIN
           WHEN    CONST-MODE-RRKSYU
           WHEN    CONST-MODE-TORIKESHI
           WHEN    CONST-MODE-HENKIN
           WHEN    CONST-MODE-SAIKEISAN
           WHEN    CONST-MODE-M-SAIKEISAN
           WHEN    CONST-MODE-M-NYUKIN
           WHEN    CONST-MODE-SKY-TORIKESHI
      *
               PERFORM 900-INCOMEREQ01-READ-SEC
      *
               CALL    "ORCGSAPI01S02" USING
                                       SCRAREA
                                       INCOMEREQ01-AREA
                                       SPA-AREA
      *
           WHEN    CONST-MODE-REPRINT
           WHEN    CONST-MODE-MONTHLY-PRINT
      *
               PERFORM 900-INCOMEREQ01-READ-SEC
      *
               CALL    "ORCGSAPI01S04" USING
                                       SCRAREA
                                       INCOMEREQ01-AREA
                                       SPA-AREA
           WHEN    OTHER
               MOVE    "0092"          TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-EVALUATE
      *
           .
       2001-REQ02-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
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
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API"           TO  SPA-MOTOPG
           CALL    "ORCGSERRMSG"   USING
                                   SPA-AREA
                                   ORCSLOCKAREA
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-INCOMEREQ-READ-SEC          SECTION.
      *
           INITIALIZE                  APIREQ-AREA
           MOVE    "xml_incomev3req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    SAPI01-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "incomev3req"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC          =   ZERO  OR  1 )
               MOVE    APIREQ-AREA TO  INCOMEREQ-AREA
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"       TO  SPA-ERRCD
           END-IF
      *
           .
       900-INCOMEREQ-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-INCOMEREQ01-READ-SEC          SECTION.
      *
           INITIALIZE                      APIREQ-AREA
           MOVE    "xml_incomev3req01" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    SAPI01-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "incomev3req"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC          =   ZERO  OR  1 )
               MOVE    APIREQ-AREA TO  INCOMEREQ01-AREA
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"       TO  SPA-ERRCD
           END-IF
      *
           .
       900-INCOMEREQ01-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-INCOMEREQ02-READ-SEC          SECTION.
      *
           INITIALIZE                  APIREQ-AREA
           MOVE    "xml_incomev3req02" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    SAPI01-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "incomev3req"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC          =   ZERO  OR  1 )
               MOVE    APIREQ-AREA TO  INCOMEREQ02-AREA
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"           TO  SPA-ERRCD
           END-IF
      *
           .
       900-INCOMEREQ02-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC               SECTION.
      *
           MOVE    INCOMEREQ-REQNUM    TO  INCOMERES01-REQNUM
           MOVE    INCOMEREQ-REQMODE   TO  INCOMERES01-REQMODE
           MOVE    INCOMEREQ-KARTE-UID TO  INCOMERES01-KARTE-UID
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  INCOMERES01-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  INCOMERES01-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO            TO  INCOMERES01-API-RESULT
           ELSE
               MOVE    "E"             TO  INCOMERES01-API-RESULT
               MOVE    SPA-ERRCD       TO  INCOMERES01-API-RESULT(2:)
           END-IF
           MOVE    SPA-ERRMSG          TO  INCOMERES01-API-RESULT-MSG
      *
           MOVE    "xml_incomev3res01" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    INCOMERES01-AREA    TO  APIREQ-AREA
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "incomev3res"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  SAPI01-BODY
               MOVE    "application/xml"
                                       TO  SAPI01-CONTENT-TYPE
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
           MOVE    "XMLWRITE"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-BLOB
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
      *****************************************************************
      *    BLOBEXPORT処理
      *****************************************************************
       910-BLOBEXPORT-SEC                SECTION.
      *
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-BLOB
      *
           .
       910-BLOBEXPORT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  SAPI01-HTTPSTATUS
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
           CALL "CBL_DELETE_FILE"  USING TMP01FILE
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       990-TERMINATE-EXT.
           EXIT.
