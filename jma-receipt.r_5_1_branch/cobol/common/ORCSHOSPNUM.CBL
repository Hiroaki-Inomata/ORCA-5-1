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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSHOSPNUM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : マスタメニュー
      *  管理者            : 
      *  07/01/29    NACL−伊藤　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  15/04/28  パスワードチェック追加
      *  04.08.00    NACL-太田    15/11/17  パスワードチェック廃止
      *  04.08.00    NACL-多々納  15/11/20  APIのSYSUSER処理追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSBASE         PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
           03  FLG-PASSWD          PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-USER.
               05  WRK-USERHOSPNUM PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    パスワード
           COPY    "CPPASSWD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
           COPY    MCPAREA.
           COPY    COMMON-SPA.
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                     FLG-AREA
      *
      *    パスワード登録チェック（API対応）
      *    IF      SPA-MOTOPG          =   "API"
      *        PERFORM 100-PASSWD-CHECK-SEC
      *    END-IF
      *
      *    医療機関識別番号設定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-HOSPNUM-SET-SEC
           END-IF
      *
      *    システム基本チェック処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-SYSBASE-CHECK-SEC
               IF      FLG-OK              =   ZERO
                   MOVE    ZERO                TO  SPA-HOSPNUM
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    医療機関識別番号設定処理
      *****************************************************************
       100-HOSPNUM-SET-SEC       SECTION.
      *
           MOVE    ZERO                TO  SPA-HOSPNUM
      *
           MOVE    "1"                 TO  SPA-NOCHK-HOSPNUM
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    SPA-OPID            TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSUSER         =   ZERO
      *        医療機関識別番号
               MOVE    SYSUSER-HOSPNUM     TO  SPA-HOSPNUM
      *        スーパーバイザー
               MOVE    SYSUSER-SUPERVISOR  TO  SPA-SUPERVISOR
      *ginbee ログインチェック
               IF     (MCP-MIDDLEWARE-NAME =   "ginbee")
                 AND  (SPA-HOSPNUM     NOT =   1)
                   MOVE    "1023"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "1002"              TO  SPA-ERRCD
      *
      *ginbee API 対応
               IF     (MCP-MIDDLEWARE-NAME =   "ginbee" )
                 AND  (SPA-MOTOPG          =   "API"    )
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    01                  TO  SPA-HOSPNUM
                   MOVE    SPACE               TO  SPA-SUPERVISOR
               END-IF
      *
           END-IF
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
      *
           .
       100-HOSPNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    システム基本チェック処理
      *****************************************************************
       100-SYSBASE-CHECK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-SYSBASE
           INITIALIZE                      SYSBASE-REC
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           PERFORM UNTIL   FLG-SYSBASE =   1
               IF  SPA-HOSPNUM         =   SYSBASE-HOSPNUM
                   MOVE    1           TO  FLG-OK
                   MOVE    1           TO  FLG-SYSBASE
               END-IF
               IF  FLG-SYSBASE         =   ZERO
                   MOVE    "tbl_sysbase"   TO  MCP-TABLE 
                   MOVE    "all"           TO  MCP-PATHNAME 
                   PERFORM 900-SYSBASE-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           .
       100-SYSBASE-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    パスワードチェック処理
      *****************************************************************
       100-PASSWD-CHECK-SEC       SECTION.
      *
           MOVE    "1"                 TO  SPA-NOCHK-HOSPNUM
      *
           INITIALIZE                      PASSWD-REC
           MOVE    SPA-OPID            TO  PASSWD-USERID
           MOVE    PASSWD-REC          TO  MCPDATA-REC
           MOVE    "tbl_passwd"        TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PASSWD-REC
                   MOVE    ZERO            TO  FLG-PASSWD
               ELSE
                   MOVE    1               TO  FLG-PASSWD
               END-IF
           ELSE
               INITIALIZE                  PASSWD-REC
               MOVE    1               TO  FLG-PASSWD
           END-IF
           MOVE    "tbl_passwd"        TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
      *    パスワードなし
           IF      FLG-PASSWD      NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
           .
       100-PASSWD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSBASE-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           .
       900-SYSBASE-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルFETCH処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
