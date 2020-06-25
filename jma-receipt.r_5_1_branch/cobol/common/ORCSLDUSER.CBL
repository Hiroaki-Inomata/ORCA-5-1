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
       PROGRAM-ID.             ORCSLDUSER.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : ユーザー一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/08/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.00    NACL−森脇    13/04/08  システムユーザー構造変更
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
      *
       01  IDX-AREA.
           03  IDXT                PIC 9(05).
           03  IDXU                PIC 9(05).
      *
       01  SGYOUMU-CODE            PIC 9(03).
      *
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システムユーザ
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
           COPY    "ORCA-SYSTEM".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSLDUSER.INC".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   SLDUSER-AREA
                                   SPA-AREA.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
      *
           PERFORM 200-MAIN-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    SLDUSER-KBN
      *        一括情報取得
               WHEN    1
                   PERFORM 2001-LISTENTRY-SEC
      *        個別情報取得
               WHEN    2
                   PERFORM 2001-GETDATA-SEC
      *        一括メッセージ表示
               WHEN    3
               WHEN    5
               WHEN    7
                   PERFORM 2001-SETMESSAGEALL-SEC
      *        個別メッセージ表示
               WHEN    4
               WHEN    6
               WHEN    8
                   PERFORM 2001-SETMESSAGE-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    個別メッセージ処理
      *****************************************************************
       2001-SETMESSAGE-SEC         SECTION.
      *
           IF      SLDUSER-ID      =   SPACE
               GO  TO  2001-SETMESSAGE-EXT
           END-IF
      *
           IF      SLDUSER-MESSAGE =   SPACE
               GO  TO  2001-SETMESSAGE-EXT
           END-IF
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    SLDUSER-ID      TO  ORCA-SYSTEM-ID
      *
           EVALUATE    SLDUSER-KBN
      *        個別ポップアップ
               WHEN    4
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-POPUP
      *        個別ダイアログ
               WHEN    6
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-DIALOG
      *        個別アボート
               WHEN    8
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-ABORT
           END-EVALUATE
      *
           MOVE    "SETMESSAGE"    TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
      *
           .
       2001-SETMESSAGE-EXT.
           EXIT.
      *****************************************************************
      *    一斉メッセージ処理
      *****************************************************************
       2001-SETMESSAGEALL-SEC      SECTION.
      *
           IF      SLDUSER-MESSAGE =   SPACE
               GO  TO  2001-SETMESSAGEALL-EXT
           END-IF
      *
           INITIALIZE                  ORCA-SYSTEM
      *
           EVALUATE    SLDUSER-KBN
      *        一斉ポップアップ
               WHEN    3
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-POPUP
      *        一斉ダイアログ
               WHEN    5
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-DIALOG
      *        一斉アボート
               WHEN    7
                   MOVE    SLDUSER-MESSAGE TO  ORCA-SYSTEM-ABORT
           END-EVALUATE
      *
           MOVE    "SETMESSAGEALL" TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
      *
           .
       2001-SETMESSAGEALL-EXT.
           EXIT.
      *****************************************************************
      *    個別情報取得処理
      *****************************************************************
       2001-GETDATA-SEC            SECTION.
      *
           IF      SLDUSER-ID      =   SPACE
               GO  TO  2001-GETDATA-EXT
           END-IF
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    SLDUSER-ID      TO  ORCA-SYSTEM-ID
           MOVE    "GETDATA"       TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
           IF      MCP-RC          =   ZERO
               MOVE    1               TO  IDXT
               PERFORM 20011-GETVALUE-SEC
           END-IF
      *
           .
       2001-GETDATA-EXT.
           EXIT.
      *****************************************************************
      *    端末一覧取得処理
      *****************************************************************
       2001-LISTENTRY-SEC              SECTION.
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    "SELECTALL"     TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       ORCA-SYSTEM
           PERFORM VARYING IDXT    FROM    1   BY  1
                   UNTIL ( MCP-RC  NOT =   ZERO )
               MOVE    "FETCH"         TO  MCP-FUNC
               MOVE    "system"        TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           ORCA-SYSTEM
               IF      MCP-RC      =   ZERO
                   PERFORM 20011-GETVALUE-SEC
               END-IF
           END-PERFORM
      *
           MOVE    ZERO            TO  SLDUSER-OTHERS
           PERFORM VARYING IDXU    FROM    1   BY  1
                   UNTIL ( IDXU    >   SLDUSER-MAX )
      *
               IF    ( SLDUSER-HOSPNUM (IDXU)      =   SPA-HOSPNUM )
                AND  ( SLDUSER-LD      (IDXU)      =   SLDUSER-MY-LD )
                AND  ( SLDUSER-TERMID  (IDXU)  NOT =   MCP-TERM )
      *
                   COMPUTE SLDUSER-OTHERS  =  SLDUSER-OTHERS + 1
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2001-LISTENTRY-EXT.
           EXIT.
      *****************************************************************
      *    端末情報取得処理
      *****************************************************************
       20011-GETVALUE-SEC          SECTION.
      *
           MOVE    ORCA-SYSTEM-ID  TO  SLDUSER-TERMID(IDXT)
           MOVE    ORCA-SYSTEM-IN-PROCESS
                                   TO  SLDUSER-IN-PROCESS(IDXT)
           MOVE    ORCA-SYSTEM-WINDOW
                                   TO  SLDUSER-WINDOW(IDXT)
           MOVE    ORCA-SYSTEM-USER
                                   TO  SLDUSER-USER(IDXT)
           MOVE    ORCA-SYSTEM-EVENT
                                   TO  SLDUSER-EVENT(IDXT)
           MOVE    ORCA-SYSTEM-ACCESS-TIME
                                   TO  SLDUSER-ACCESS-TIME(IDXT)
           MOVE    ORCA-SYSTEM-PROCESS-TIME
                                   TO  SLDUSER-PROCESS-TIME(IDXT)
           MOVE    ORCA-SYSTEM-TOTAL-PROCESS-TIME
                                   TO  SLDUSER-TOTAL-PROC(IDXT)
           MOVE    ORCA-SYSTEM-AGENT
                                   TO  SLDUSER-AGENT(IDXT)
           MOVE    ORCA-SYSTEM-HOST
                                   TO  SLDUSER-HOST(IDXT)
           MOVE    ORCA-SYSTEM-WIDGET
                                   TO  SLDUSER-WIDGET(IDXT)
           MOVE    ORCA-SYSTEM-CREATE-TIME
                                   TO  SLDUSER-CREATE-TIME(IDXT)
           PERFORM 900-SYSUSER-KEY-SEL-SEC
           MOVE    SYSUSER-HOSPNUM TO  SLDUSER-HOSPNUM(IDXT)
      *
           MOVE    SPA-AREA        TO  WSPA-AREA
           PERFORM 800-GYOUMUGET-SEC
           MOVE    WSPA-AREA       TO  SPA-AREA
           MOVE    SGYOUMU-CODE    TO  SLDUSER-LD(IDXT)
      *
           MOVE    IDXT            TO  SLDUSER-MAX
      *
           .
       20011-GETVALUE-EXT.
           EXIT.
      *****************************************************************
      *    業務取得処理
      *****************************************************************
       800-GYOUMUGET-SEC           SECTION.
      *
           MOVE    ZERO            TO  SGYOUMU-CODE
      *
           MOVE    SLDUSER-WINDOW(IDXT)
                                   TO  SPA-SAKIPG
      *
           EVALUATE    TRUE
           WHEN  ( SPA-SAKIPG(1:1)     =   "X" )
               IF    (  SPA-SAKIPG(2:1)     IS  NUMERIC )
                  OR (  SPA-SAKIPG(1:4)     =   "XERR" )
                  OR (  SPA-SAKIPG(1:3)     =   "XID" )
                   CONTINUE
               ELSE
                   MOVE    "?"         TO  SPA-SAKIPG(1:1)
               END-IF
           WHEN  ( SPA-SAKIPG               =   "P97" )
                   MOVE    "?"         TO  SPA-SAKIPG(1:1)
           END-EVALUATE
      *
           CALL    "ORCSGYOUMUGET"     USING
                                       SPA-AREA
                                       SGYOUMU-CODE
      *
           .
       800-GYOUMUGET-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー環境検索処理
      *****************************************************************
       900-SYSUSER-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSUSER
      *
           INITIALIZE                  SYSUSER-REC
           MOVE   ORCA-SYSTEM-USER TO  SYSUSER-USERID
           MOVE    SYSUSER-REC     TO  MCPDATA-REC
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
      *
           MOVE    "tbl_sysuser"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSUSER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
