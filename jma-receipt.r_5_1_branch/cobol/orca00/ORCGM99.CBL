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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGM99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者       記述
      *  08/10/07    NACL−森脇   新規作成
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
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "M00COMMON-SPA".
      *
           COPY    "M01SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
           03  WRK-PRT-PAGE        PIC 9(10).
      *     
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値（データ収集）  
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                  PIC X(08)    VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M01
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    処理状態
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-MOTOPG          =   "M98"
               MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           ELSE
               MOVE    "M00"               TO  SPA-SAKIPG
           END-IF
           MOVE    "M99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC             SECTION.
      *
           INITIALIZE                  M99
           MOVE    ZERO            TO  M99-COUNT
      *
           MOVE    5               TO  M99-DURATION
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-JOB-JOBID-COLLECT
      *        ジョブ管理チェック処理
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    IDX             TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
               IF      SJOBKANRI-RETURN    =   ZERO
                   PERFORM 4901-KAKUNIN-HYOUJI-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "B01"           TO  MCP-WIDGET
           IF    ( M99-MSG             =   SPACE )
               IF    ( M99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                           TO  M99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                           TO  M99-MSG
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "M99    "       TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       4901-KAKUNIN-HYOUJI-SEC     SECTION.
      *
           MOVE    JOB-JOBID           TO  M99-RENNUM   (IDX)
           MOVE    JOB-SHELLMSG        TO  M99-SHORINM  (IDX)
           MOVE    JOB-STARTTIME(1:2)  TO  WRK-HH
           MOVE    JOB-STARTTIME(3:2)  TO  WRK-MM
           MOVE    JOB-STARTTIME(5:2)  TO  WRK-SS
           MOVE    WRK-HENTIME         TO  M99-STARTTIME(IDX)
           MOVE    JOB-ENDTIME(1:2)    TO  WRK-HH
           MOVE    JOB-ENDTIME(3:2)    TO  WRK-MM
           MOVE    JOB-ENDTIME(5:2)    TO  WRK-SS
           MOVE    WRK-HENTIME         TO  M99-ENDTIME  (IDX)
           MOVE    JOB-UPDCNT          TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  M99-PAGE     (IDX)
           MOVE    JOB-YOBI            TO  M99-ERRMSG   (IDX)
           IF      JOB-ENDYMD          =   SPACE
               MOVE    "処理中です"    TO  M99-MSG
           ELSE
              IF      JOB-ERRCD        =   SPACE
                  CONTINUE
              ELSE
                  MOVE    SPACE        TO  M99-MSG
                  PERFORM 4901-JOB-ERRSET-SEC
                  IF      M99-MSG          =   SPACE
                       STRING  JOB-YOBI        DELIMITED BY SPACE
                               "【"            DELIMITED BY SIZE
                               JOB-SHELLMSG    DELIMITED BY SPACE
                               "】"            DELIMITED BY SIZE
                                               INTO  M99-MSG
                       END-STRING
                   END-IF
              END-IF
           END-IF
           MOVE    IDX                     TO  M99-COUNT
      *
           .
       4901-KAKUNIN-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       4901-JOB-ERRSET-SEC         SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】"
                                               TO  M99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】"
                                               TO  M99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】"
                                               TO  M99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】"
                                               TO  M99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】"
                                               TO  M99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】"
                                               TO  M99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】"
                                               TO  M99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCDBMAIN"      USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
