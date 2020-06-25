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
       PROGRAM-ID.                 ORCGC99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者       記述
      *  06/08/10    NACL−森脇   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-SYMD-TAIHI      PIC X(08).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-PAGE            PIC ZZ,ZZZ,ZZ9.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-SS          PIC X(02).
      *
       01  CONST-AREA.
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
           03  CONST-JOB-SHELLID   PIC X(08)   VALUE   "ORHCML09".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
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
           COPY    "ORCA22SCRAREA.INC".
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
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           PERFORM 490-KAKUNIN-SEC
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "C99"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
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
           MOVE    SPA-MOTOPG      TO  SPA-SAKIPG
           MOVE    "C99"           TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
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
           MOVE    SPACE           TO  C99
           INITIALIZE                  C99
           MOVE    ZERO            TO  C99-COUNT
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "該当する処理はありません"
                                       TO  C99-MSG
               MOVE    "B01"           TO  MCP-WIDGET
               PERFORM 4901-PUT-CURRENT-WINDOW-SEC
               GO  TO  490-KAKUNIN-EXT
           END-IF
      *
           MOVE    JOB-JOBID       TO  C99-RENNUM   (1)
           MOVE    JOB-SHELLMSG    TO  C99-SHORINM  (1)
      *
           MOVE    JOB-STARTYMD    TO  WRK-SYMD
           PERFORM 700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  C99-STARTDATE (1)
      *
           IF    ( JOB-STARTTIME   =   SPACE )
               MOVE    SPACE       TO  C99-STARTTIME(1)
           ELSE
               MOVE    JOB-STARTTIME(1:2)
                                   TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                   TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                   TO  WRK-SS
               MOVE    WRK-HENTIME TO  C99-STARTTIME(1)
           END-IF
      *
           MOVE    JOB-ENDYMD      TO  WRK-SYMD
           PERFORM 700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  C99-ENDDATE (1)
      *
           IF    ( JOB-ENDTIME =   SPACE )
               MOVE    SPACE       TO  C99-ENDTIME  (1)
           ELSE
               MOVE    JOB-ENDTIME(1:2)
                                   TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                   TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                   TO  WRK-SS
               MOVE    WRK-HENTIME TO  C99-ENDTIME  (1)
           END-IF
           MOVE    JOB-UPDCNT      TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  C99-PAGE     (1)
           MOVE    JOB-YOBI        TO  C99-ERRMSG   (1)
      *
           IF    ( JOB-ENDYMD      =   SPACE )
               MOVE    "処理中です"
                                   TO  C99-MSG
           ELSE
               IF    ( JOB-ERRCD   =   SPACE )
                   MOVE    "処理は正常に終了しました"
                                               TO  C99-MSG
               ELSE
                   STRING  JOB-YOBI    DELIMITED  BY  SPACE
                           "【"        DELIMITED  BY  SIZE
                          JOB-SHELLMSG DELIMITED  BY  SPACE
                          "】"         DELIMITED  BY  SIZE
                   INTO  C99-MSG
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    1                TO  C99-COUNT
      *
           MOVE    "B11"            TO  MCP-WIDGET
      *
           PERFORM 4901-PUT-CURRENT-WINDOW-SEC
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次画面再表示処理
      *****************************************************************
       4901-PUT-CURRENT-WINDOW-SEC SECTION.
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "C99    "       TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
      *
       4901-PUT-CURRENT-WINDOW-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC           SECTION.
      *
           IF    ( WRK-SYMD    NOT =   SPACE)
            AND  ( WRK-SYMD (7:2)  =   SPACE)
               MOVE    "20"            TO  WRK-SYMD-TAIHI(1:2)
               MOVE    WRK-SYMD (1:6)  TO  WRK-SYMD-TAIHI(3:6)
           ELSE
               MOVE    WRK-SYMD        TO  WRK-SYMD-TAIHI
           END-IF
      *
           MOVE    WRK-SYMD-TAIHI      TO  WRK-SYMD
      *
           IF    ( WRK-SYMD        =   ALL "9"  OR   SPACE )
               MOVE    SPACE           TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  WRK-HENYMDG
           END-IF.
      *
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
