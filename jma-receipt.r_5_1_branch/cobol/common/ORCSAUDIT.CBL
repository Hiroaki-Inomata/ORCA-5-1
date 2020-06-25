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
       PROGRAM-ID.             ORCSAUDIT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 監視ログ
      *  コンポーネント名  : 監視ログ出力
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/02/27    NACL-竹田     新規作成
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
       
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD          PIC X(08).
           03  WRK-MCP-RC          PIC ---9.
           03  WRK-STR             PIC X(255).
           03  WRK-LOGMSG          PIC X(255).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-FUNC            PIC X(64).
           03  WRK-TABLE           PIC X(64).
           03  WRK-PTID            PIC 9(10).
      *
       01  ORCXKANACONVAREA.
      *   IN/OUTの長さをMCPDARA-RECに合せるため独自に引数領域を編集
      *   出力バイト数
           03 KANACONV-LEN         PIC S9(9)   BINARY.
      *   返り値( 0: 正常 1: 範囲外文字 2: 外字、変換不能 )
           03 KANACONV-RETURN      PIC S9(9) BINARY.
      *   変換文字列数(バイト)
           03 KANACONV-MAXLEN      PIC S9(9)   BINARY.
      *   変換フラグ (0: 変換しない,     1: 変換する
      *               2: 1+カタカナ変換, 3: 1+ひらがな変換)
      *   ※ 半角カナおよび外字は全てのフラグで変換される
      *      か捨てられる。
      *     (外字は■)
           03 KANACONV-CONV-FLAG   PIC S9(9)   BINARY.
      *   出力文字種指定
      *       0: 半角カナ、外字のみ変換
      *      14: 全角   2: ひらがな  4: カタカナ
      *      112: ASCII 16: 数字     32: アルファベット
      *      15: 全角+スペース改行   113: ASCII+スペース改行 
      *   出力文字種は OR をとることで複数指定できる
      *   例: (6: ひらがな、カタカナのみ)
      *       (48: 英数字のみ)
           03 KANACONV-CHAR-TYPE   PIC S9(9)   BINARY.
      *   文字領域の長さ(INDATA, OUTDATAの長さと同じであること)
           03 KANACONV-STLEN       PIC S9(9) BINARY.
      *   入力文字列
           03 KANACONV-INDATA      PIC X(50000).
      *   出力文字列
           03 KANACONV-OUTDATA     PIC X(50000).
      *
       01  CONST-AREA.
           03 CONST-MAX-LEN        PIC 9(08)   VALUE   50000.
      *
           COPY    "MCPDATA.INC"   REPLACING   //MCPDATA-//
                                   BY          //WKMCPDATA-//.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    処理識別区分
       01  SHIKIBETU-KBN           PIC X(01).
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCSLOCK.INC".
      *
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
       PROCEDURE               DIVISION    USING
                               SHIKIBETU-KBN
                               MCPAREA
                               MCPDATA-REC
                               SPA-AREA
                               ORCSLOCKAREA
                               JOBKANRI-REC
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
           MOVE    MCP-FUNC            TO  WRK-FUNC
           MOVE    1                   TO  MCP-LOGFLAG
           MOVE   "AUDITLOG"           TO  MCP-FUNC
      *
           PERFORM 100-INIT-SEC
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           MOVE    WRK-FUNC            TO  MCP-FUNC
           MOVE    ZERO                TO  MCP-LOGFLAG
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SHIKIBETU-KBN       =   "1"
      *        監視ログ編集処理 (ロックテーブル)
               PERFORM 1001-LOCKLOG-SEC
           ELSE
      *        監視ログ編集処理 (ジョブ管理テーブル)
               PERFORM 1001-JOBLOG-SEC
           END-IF
      *
           MOVE    WRK-LOGMSG          TO  WKMCPDATA-REC
      *    エラーメッセージ編集処理
           PERFORM 800-KANACONV-SEC
           IF      KANACONV-RETURN     =   ZERO 
               MOVE    WRK-LOGMSG          TO  MCP-LOGCOMMENT
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    監視ログ編集処理 (ロックテーブル)
      *****************************************************************
       1001-LOCKLOG-SEC            SECTION.
      *
           IF      SLOCK-MODE          =   1
               MOVE   "Lock_start"         TO  WRK-STR
               MOVE SLOCK-PTID             TO  WRK-PTID
           ELSE
               MOVE   "Lock_release"       TO  WRK-STR
               MOVE   SPA-LAST-PTID        TO  WRK-PTID
           END-IF
      *
           STRING WRK-STR              DELIMITED   BY  SPACE
                  " HOSPNUM["          DELIMITED   BY  SIZE
                  SPA-HOSPNUM          DELIMITED   BY  SIZE
                  "] PTID["            DELIMITED   BY  SIZE
                  WRK-PTID             DELIMITED   BY  SIZE
                  "] SRYYMD["          DELIMITED   BY  SIZE
                  SPA-SRYYMD           DELIMITED   BY  SIZE
                  "]"                  DELIMITED   BY  SIZE
                                       INTO    WRK-LOGMSG
           END-STRING
           .
      *
       1001-LOCKLOG-EXT.
           EXIT.
      *
      *****************************************************************
      *    監視ログ編集処理 (ジョブ管理テーブル)
      *****************************************************************
       1001-JOBLOG-SEC            SECTION.
      *
           MOVE   "Batch_start"        TO  WRK-STR
      *
           STRING WRK-STR              DELIMITED   BY  SPACE
                  " HOSPNUM["          DELIMITED   BY  SIZE
                  SPA-HOSPNUM          DELIMITED   BY  SIZE
                  "] SHELLID["         DELIMITED   BY  SIZE
                  JOB-SHELLID          DELIMITED   BY  SIZE
                  "] SRYYM["           DELIMITED   BY  SIZE
                  JOB-SRYYM            DELIMITED   BY  SIZE
                  "] NYUGAI["          DELIMITED   BY  SIZE
                  JOB-NYUGAIKBN        DELIMITED   BY  SIZE
                  "]"                  DELIMITED   BY  SIZE
                                       INTO    WRK-LOGMSG
           END-STRING
           .
      *
       1001-JOBLOG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-KANACONV-SEC             SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    CONST-MAX-LEN   TO  KANACONV-LEN
                                       KANACONV-STLEN
           MOVE    0               TO  KANACONV-CONV-FLAG
           MOVE    113             TO  KANACONV-CHAR-TYPE
           MOVE    WKMCPDATA-REC   TO  KANACONV-INDATA
           CALL    "kanaconv"      USING   ORCXKANACONVAREA
      *
           .
       800-KANACONV-EXT.
           EXIT.
      *
