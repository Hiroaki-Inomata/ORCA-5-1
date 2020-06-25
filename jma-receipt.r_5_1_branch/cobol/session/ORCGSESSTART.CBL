      *****************************************************************
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
      *****************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGSESSTART.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セションスタート処理
      *  コンポーネント名  : ログイン認証
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/03/29    NACL−伊藤　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
      *
       DATA                DIVISION.
      *
       WORKING-STORAGE         SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PASSWD          PIC 9(01).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPORCSAUTH.INC".
      *    パスワード
           COPY    "CPPASSWD.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
       LINKAGE                 SECTION.
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "SESSION.INC".
      *****************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *****************************************************************
       000-MAIN                    SECTION.
      *
           EVALUATE    HTTP-METHOD
               WHEN    'GET'
                   PERFORM 010-GET
               WHEN    ANY
                   PERFORM 020-ERROR
           END-EVALUATE
           .
       000-MAIN-EXT.
           EXIT    PROGRAM.
      *****************************************************************
       010-GET                     SECTION.
      *
           MOVE    LOW-VALUE       TO  SESSION-BODY.
      *
           PERFORM 900-PASSWD-READ-SEC
      *
           IF      FLG-PASSWD      =   ZERO
               INITIALIZE                  ORCSAUTHAREA
               MOVE    "auth"          TO  AUTH-COMMAND
               MOVE    1               TO  AUTH-ID
               MOVE    PASSWORD        TO  AUTH-PASSWORD1
               MOVE    PASSWD-PASSWORD TO  AUTH-PASSWORD2
               CALL    "orcsauth"          USING
                                           ORCSAUTHAREA
               IF      AUTH-RC         =   ZERO
                   MOVE    200             TO  HTTP-STATUS
               ELSE
                   MOVE    403             TO  HTTP-STATUS
               END-IF
           ELSE
               MOVE    403             TO  HTTP-STATUS
           END-IF
           .
       010-GET-EXT.
           EXIT.
      *****************************************************************
       020-ERROR                   SECTION.
      *
           MOVE    400             TO  HTTP-STATUS
           . 
       020-ERROR-EXT.
           EXIT.
      *****************************************************************
      *    パスワード検索処理
      *****************************************************************
       900-PASSWD-READ-SEC         SECTION.
      *
           INITIALIZE                  SPA-AREA
           MOVE    01              TO  SPA-HOSPNUM
           MOVE    ZERO            TO  FLG-PASSWD
      *
           INITIALIZE                  PASSWD-REC
           MOVE    USER            TO  PASSWD-USERID
           MOVE    PASSWD-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_passwd"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PASSWD-REC
                   MOVE    ZERO            TO  FLG-PASSWD
               ELSE
                   MOVE    1               TO  FLG-PASSWD
                   INITIALIZE                  PASSWD-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PASSWD
               INITIALIZE                  PASSWD-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_passwd"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PASSWD-READ-EXT.
           EXIT.
      *
