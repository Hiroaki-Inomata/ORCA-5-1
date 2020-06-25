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
       PROGRAM-ID.                 ORCBSQL1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタ更新処理
      *  コンポーネント名  : ＳＱＬ更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11.04.13    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：ＳＱＬ更新データ
           SELECT  IN-FILE         ASSIGN
                                   WRK-PARA-SQLFILE
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力：ＳＱＬ更新データ
       FD  IN-FILE.
       01  IN-REC.
           03  IN-SQL                  PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-IN                  PIC 9(01).
           03  FLG-SQL                 PIC 9(01).
           03  FLG-SQL-END             PIC 9(01).
           03  FLG-BREAK               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-SQL-LINE            PIC 9(06).
      *
       01  IDX-AREA.
           03  I                       PIC 9(04).
      *
       01  SQL-AREA.
           03  W-SQLSTR                PIC X(20000).
           03  W-MOVP                  PIC 9(06).
      *
       01  CONST-AREA.
           03  CONST-SQL-MAXLEN        PIC 9(06)  VALUE  20000.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-SQLFILE            PIC X(256).
           03  WRK-IN                  PIC X(01).
           03  WRK-RC                  PIC S9(9)   BINARY.
           03  WRK-IN-LEN              PIC 9(05).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      *****************************************************************
       PROCEDURE                       DIVISION
                                       USING   COMMAND-PARAM.
      *
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL ( FLG-END     =   1 )
                    OR   ( FLG-IN      =   1 )
      *
           PERFORM 300-END-SEC
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-AREA
           MOVE    ZERO                TO  CNT-AREA
           INITIALIZE                      WRK-AREA
                                           SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-SQLFILE
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "DBOPEN ERR"
           END-IF
      *
      *    入力　初期処理
           OPEN    INPUT               IN-FILE
           IF  STS-IN              NOT =   ZERO
               MOVE    1               TO  FLG-IN
               DISPLAY "*(ORCBSQL1)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    ＳＱＬ更新データ読込み処理
           IF      FLG-IN              =   ZERO
               PERFORM 900-SQL-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    W-SQLSTR            TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "exec_sql"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"        USING  MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  FLG-END
               DISPLAY "*(ORCBSQL1)* SQL ERR /RC[ " MCP-RC "] "
                       " LINE[ " CNT-SQL-LINE " ]"
               MOVE    MCP-RC          TO  WRK-RC
           ELSE
               PERFORM 900-SQL-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           CLOSE                       IN-FILE
      *
           DISPLAY "*(ORCBSQL1)* INPUT  /I   CNT[" CNT-IN  "]"
      **** ACCEPT  WRK-IN
      *
           MOVE    WRK-RC              TO  RETURN-CODE
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     ＳＱＬ文マスタ更新データ読込み処理
      *****************************************************************
       900-SQL-SEC                     SECTION.
      *
           INITIALIZE                      SQL-AREA
           MOVE    1                   TO  W-MOVP
           MOVE    ZERO                TO  FLG-SQL
           MOVE    ZERO                TO  FLG-SQL-END
           MOVE    ZERO                TO  CNT-SQL-LINE
      *
           PERFORM 900-IN-READ-SEC
           PERFORM UNTIL  (FLG-END = 1)
                      OR  (FLG-IN  = 1)
                      OR  (FLG-SQL = 1)
               IF      IN-SQL(1:2)         =   "--"
                   CONTINUE
               ELSE
                   IF      CNT-SQL-LINE        =   ZERO
                       MOVE    CNT-IN              TO  CNT-SQL-LINE
                   END-IF
                   IF      IN-SQL(WRK-IN-LEN:1)    =   ";"
                       MOVE    1                   TO  FLG-SQL-END
                   END-IF
                   MOVE    IN-SQL              TO  W-SQLSTR(W-MOVP:)
                   COMPUTE W-MOVP              =   W-MOVP
                                               +   WRK-IN-LEN
                   IF     (IN-SQL(WRK-IN-LEN:1)    NOT =   ",")  AND
                          (FLG-SQL-END             =   ZERO)
                       MOVE    X"0a"               TO  W-SQLSTR(W-MOVP:)
                       COMPUTE W-MOVP              =   W-MOVP  +  1
                   END-IF
      *
                   IF      W-MOVP              >=  CONST-SQL-MAXLEN
                       MOVE    1                   TO  FLG-END
                       DISPLAY "*(ORCBSQL1)* SQL ERR LENGTH OVER"
                       " LINE[ " CNT-SQL-LINE " ]"
                   END-IF
               END-IF
      *
               IF      FLG-END             =   ZERO
                   IF      FLG-SQL-END         =   ZERO
                       PERFORM 900-IN-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SQL
                   END-IF
               END-IF
           END-PERFORM
           .
       900-SQL-EXT.
           EXIT.
      *
      *****************************************************************
      *     マスター更新データ読込み処理
      *****************************************************************
       900-IN-READ-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-LEN
           READ    IN-FILE
               AT  END
                   MOVE    1                   TO  FLG-IN
               NOT AT  END
                   MOVE    ZERO                TO  FLG-IN
                   ADD     1                   TO  CNT-IN
                   MOVE    FUNCTION    BYTE-LENGTH (
                           FUNCTION    TRIM ( IN-SQL ))
                                               TO  WRK-IN-LEN
           END-READ
           .
       900-IN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
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
       900-DBDISCONNECT-SEC            SECTION.
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
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
