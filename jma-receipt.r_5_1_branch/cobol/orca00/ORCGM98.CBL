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
       PROGRAM-ID.                 ORCGM98.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データ収集
      *  コンポーネント名  : 閲覧画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/11/22    NACL−竹田　  新規作成
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
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      * データ領域
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  SPA-XA01.
           03  SPA-M98-AREA.
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TITLE1              PIC X(08).
                   07  SPA-GMN-TITLE2              PIC X(10).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-OCC             OCCURS  100.
                           11  SPA-GMN-TNO         PIC 9(03).
                           11  SPA-GMN-TLOGYMD     PIC X(09).
                           11  SPA-GMN-TLOGTIME    PIC X(06).
                           11  SPA-GMN-TPROGNAME   PIC X(20).
                           11  SPA-GMN-TKBNID      PIC X(01).
                           11  SPA-GMN-TOBJECTYYM  PIC X(06).
                           11  SPA-GMN-TRESULT     PIC X(01).
                           11  SPA-GMN-TERRCD      PIC X(04).
                           11  SPA-GMN-TMSG        PIC X(200).
      *                 
               05  SPA-NAI-AREA.
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-OCC             OCCURS  100.
                           11  SPA-NAI-TLOGYMD    PIC X(09).
                           11  SPA-NAI-TLOGTIME   PIC X(06).
                           11  SPA-NAI-TPROGNAME  PIC X(20).
      *
      *                     
               05  SPA-WRK-TBL-KEY                 PIC X(08).            
      *   
           COPY    "CMJOBNAME.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-JOBLOG                          PIC 9(01).
           03  FLG-END                             PIC 9(01).
           03  FLG-LIST-ROW                        PIC 9(01).
           03  FLG-USER                            PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IXY                 PIC 9(04).
           03  IXZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-M98MSG          PIC X(200).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.   
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNUM          PIC 9(03). 
      *
           03  WRK-ZZ              PIC ZZ9.
      *  
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE-G.
               05  WRK-EDPAGE      PIC 9(05).
           03  WRK-STR-G.
               05  WRK-STR         PIC X(01)   OCCURS  30.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-ROW.
           05  WRK-LIST-ROW        PIC S9(9)   BINARY.
      *                             
           COPY    "CPSHELLTBL.INC".
      *     
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "001".
           03  WRK-CONS-3003-KBNCD-LIKE        PIC X(08)
                                               VALUE   "001%".
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "JOBLOG".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.     
      *    ジョブ管理ＤＢのキー値（データ収集）  
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                  PIC X(08)    VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *    ジョブ管理ＤＢのキー値（感染症）  
           03  WRK-CONS-JOB-SHELLID-INFECTION
                                  PIC X(08)    VALUE   "INFECTIO".
           03  WRK-CONS-JOB-JOBID-INFECTION
                                   PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK3003.INC".
      *
           COPY    "CPSK1010.INC".
      *
       01  JOBLOG-REC.
           COPY    "CPJOBLOG.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
      *****MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "M98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-MOTOPG
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "MID1"
                   PERFORM 330-MID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "MDAS"
                   PERFORM 330-MDAS-SET-SEC
      *             GO      TO      300-SCREEN-EXT
               WHEN    "MUID"
                   IF      SPA-MID1-FLG     =   "OK"
                       PERFORM 490-SETTEI-SYORI-SEC
                   END-IF
      *             GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   DISPLAY "run"
      *            PERFORM 3001-MID1-SYORI-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  JOBLOG-REC
           MOVE    SPA-HOSPNUM         TO  JOBLOG-HOSPNUM
           MOVE    JOBLOG-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_joblog"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC             =   ZERO
               PERFORM 900-JOBLOG-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-JOBLOG
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-JOBLOG    =   1
                               OR      IDX             =   99
      *    とりあえず100から99に変更した
      *    正しい対応はSPA-GMN-MAXの桁数を3桁以上にしなければならない
      *    他の影響が分からないので改正対応後に見直し
      *                         OR      IDX             =   100
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-GMN-TNO   (IDX)
               MOVE    JOBLOG-LOGYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-NAI-TLOGYMD   (IDX)
               MOVE    JOBLOG-LOGTIME  TO
                                           SPA-NAI-TLOGTIME  (IDX)
               MOVE    JOBLOG-PROGNAME  TO
                                           SPA-GMN-TPROGNAME (IDX)
               MOVE    JOBLOG-KBNID     TO
                                           SPA-GMN-TKBNID    (IDX)
               MOVE    JOBLOG-OBJECTYYM TO
                                           SPA-GMN-TOBJECTYYM(IDX)
               MOVE    JOBLOG-RESULT    TO
                                           SPA-GMN-TRESULT   (IDX)
               MOVE    JOBLOG-ERRCD     TO
                                           SPA-GMN-TERRCD    (IDX)
               MOVE    JOBLOG-MSG       TO
                                           SPA-GMN-TMSG      (IDX)
      *
               PERFORM 900-JOBLOG-FET-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_joblog"        TO  MCP-TABLE
           MOVE    "key3"
                                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *                                 
           MOVE    IDX                 TO  SPA-GMN-MAX
      *                                 
           MOVE    1                   TO  FLG-LIST-ROW
      *                                 
           MOVE    1                   TO  SPA-GMN-CUR                                                 
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-MID1-SET-SEC             SECTION.
      *
           IF      SPA-MID1-FLG     =   "OK"
               EVALUATE    SPA-MIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1002"
                   WHEN    "1003"
                   WHEN    "1001"
      *************WHEN    "1004"
                       PERFORM 450-PRINT-SEC
               END-EVALUATE        
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       330-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    参加同意確認画面（ＭＤＡＳ）ＯＫ処理
      *****************************************************************
       330-MDAS-SET-SEC             SECTION.
      *
           IF      SPA-MID1-FLG     =   "OK"
               PERFORM 4901-MUID-SYORI-SEC
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       330-MDAS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     終了
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *     登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GID-SEC
      *     プレビュー
      *        WHEN    "CLICKED"       ALSO    "B11"
      *            PERFORM 450-PRINT-GID-SEC
      *     処理確認
      *        WHEN    "CLICKED"       ALSO    "B13"
      ****         PERFORM 490-KAKUNIN-SEC
      *     データ収集処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                  PERFORM 490-COLLECT-KAKUNIN-SEC
      *     設定
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-SETTEI-SYORI-SEC
      *     感染症画面
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 480-MINF-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *          
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           .
       203-SELECT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
      *    MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    WRK-CONS-3003-KBNCD-LIKE
                                       TO  SYS-3003-KBNCD
      *    MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"      TO  SPA-MIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "M00"              TO  SPA-SAKIPG
           MOVE    "M98"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M98"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
      ****     MOVE    WRK-CONS-JOB-SHELLID
           MOVE    WRK-CONS-JOB-SHELLID-INFECTION
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-PRINT-GID-SEC                SECTION.
      *
      *    ジョブ管理チェック        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
      *    MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    WRK-CONS-3003-KBNCD-LIKE
                                       TO  SYS-3003-KBNCD
      *    MOVE    SPA-SYSYMD          TO  ORC-DBYMD 
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE 
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE                 
               GO  TO  450-PRINT-GID-EXT               
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE                 
               GO  TO  450-PRINT-GID-EXT               
           END-IF
      *     
      *
           IF      SPA-ERRCD       NOT =   SPACE                 
               GO  TO  450-PRINT-GID-EXT               
           END-IF
      *         
           .
       450-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       450-PRINT-SEC                   SECTION.
      * 
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
      *    MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    WRK-CONS-3003-KBNCD-LIKE
                                       TO  SYS-3003-KBNCD
      *    MOVE    SPA-SYSYMD          TO  ORC-DBYMD 
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
grpsys*        MOVE SPA-HOSPNUM TO -HOSPNUM
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"
                                           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE                                 
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  450-PRINT-EXT
               END-IF
           END-IF                 
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  450-PRINT-EXT
           END-IF             
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
      *
      *    印刷処理
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "das-upload.sh"     TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG2
      *     
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "monthly"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                    SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID 
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
      *    MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    WRK-CONS-3003-KBNCD-LIKE
                                       TO  SYS-3003-KBNCD
      *    MOVE    SPA-SYSYMD          TO  ORC-DBYMD 
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC 
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  MERR-ERRMSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE      
                                                   INTO  MERR-ERRMSG
                               END-STRING
                           END-IF                          
                       END-IF    
                   END-IF
               END-PERFORM
           END-IF
           MOVE    "B01"                TO  MCP-WIDGET
           IF      MERR-ERRMSG          =   SPACE
                   MOVE    "処理は正常に終了しました"
                                                TO  MERR-ERRMSG
                   IF      SPA-MID1-FLG    =   "PV"
                       MOVE    "B12"            TO  MCP-WIDGET
                   ELSE
                       MOVE    "B01"            TO  MCP-WIDGET
                   END-IF
           END-IF                                 
      *
           MOVE    "M98"                 TO  SPA-MOTOPG
           MOVE    "MERR"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "MERR"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ収集処理確認処理
      *****************************************************************
       490-COLLECT-KAKUNIN-SEC     SECTION.
      * 
           INITIALIZE                  M99
           MOVE    ZERO            TO  M99-COUNT
      *
           MOVE    5               TO  M99-DURATION
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  JOB-JOBID
                                       IDX
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
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
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"        TO  M99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       STRING  JOB-YOBI        DELIMITED  BY  SPACE
                               "【"            DELIMITED  BY  SIZE
                               JOB-SHELLMSG    DELIMITED  BY  SPACE
                               "】"            DELIMITED  BY  SIZE
                                         INTO  M99-MSG
                       END-STRING
                   END-IF
                   MOVE    IDX                 TO  M99-COUNT
               END-IF
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( M99-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( M99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                               TO  M99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                               TO  M99-MSG
               END-IF
           END-IF
      *
           MOVE    "M98"                 TO  SPA-MOTOPG
           MOVE    "M99"                 TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "M99"                 TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       490-COLLECT-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    設定処理
      *****************************************************************
       490-SETTEI-SYORI-SEC        SECTION.
      *
           MOVE    SPA-SYS-1010-REC    TO  SYS-1010-REC
           IF      SYS-1010-KANRIAUTHFLG   NOT =   "T"
               GO  TO  490-SETTEI-SYORI-EXT
           END-IF
      *
      *
           MOVE    "M98"               TO  SPA-MOTOPG
           MOVE    "MDAS"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MDAS"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-SETTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＭＵＩＤ画面編集処理
      *****************************************************************
       4901-MUID-SYORI-SEC          SECTION.
      *
           INITIALIZE                      SPA-MUID-AREA
           MOVE    SPACE                TO  SPA-MIDCD
           MOVE    SPACE                TO  MUID
      *
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-M01             TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGMUID"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M01
      *
           MOVE    "KANANAME"           TO  MCP-WIDGET
           MOVE    "M98 "               TO  SPA-MOTOPG
           MOVE    "MUID"               TO  SPA-SAKIPG
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MUID"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       4901-MUID-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症表示画面へ
      *****************************************************************
       480-MINF-SEC                SECTION.
      *
              MOVE    "B12"        TO  MCP-WIDGET
      *
              MOVE    "M98"        TO  SPA-MOTOPG
              MOVE    "MINF"       TO  SPA-SAKIPG
      *
              MOVE    "CHANGE"     TO  MCP-PUTTYPE
              MOVE    "MINF"       TO  MCP-WINDOW
      *
              PERFORM 900-PUT-WINDOW
      *
              MOVE    1            TO  FLG-END
      *
           .
       480-MINF-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      **** MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "M98     "          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    M98-ROW            TO  WRK-LIST-ROW
      *
           INITIALIZE  M98 
      *
           EVALUATE    FLG-LIST-ROW
           WHEN    1
               MOVE    ZERO            TO  M98-ROW
           WHEN    OTHER
               MOVE    WRK-LIST-ROW    TO  M98-ROW
           END-EVALUATE
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-TNO       (IDX) TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  M98-VALUE0    (IDX)
               MOVE    SPA-NAI-TLOGYMD(IDX)    TO  M98-VALUE1    (IDX)
               MOVE    SPA-NAI-TLOGTIME  (IDX) TO  M98-VALUE2    (IDX)
               MOVE    SPA-GMN-TPROGNAME(IDX)
                                               TO
                                               M98-VALUE3 (IDX)
               MOVE    SPA-GMN-TKBNID    (IDX) TO  M98-VALUE4    (IDX)
               MOVE    SPA-GMN-TOBJECTYYM (IDX)
	                                       TO  M98-VALUE5 (IDX)
               MOVE    SPA-GMN-TRESULT (IDX)   TO  M98-VALUE6 (IDX)
               MOVE    SPA-GMN-TERRCD  (IDX)   TO  M98-VALUE7 (IDX)
               MOVE    SPA-GMN-TMSG    (IDX)   TO  M98-VALUE8 (IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  M98-COUNT
      *
      *    設定
           MOVE    SPA-SYS-1010-REC    TO  SYS-1010-REC
           IF      SYS-1010-KANRIAUTHFLG   =   "T"
               MOVE    "設定"              TO  M98-B09-LABEL
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"       TO  MCP-WIDGET
               WHEN    3
                   MOVE    "RENNUM"       TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SHORI_RENNUM" TO  MCP-WIDGET
               WHEN    5
                   MOVE    "STPAGE"       TO  MCP-WIDGET
               WHEN    6
                   MOVE    "EDPAGE"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"          TO  MCP-WIDGET
           END-EVALUATE
          .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の選択番号はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "頁の入力が違います" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "未入力の項目があります" 
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "頁の入力が違います" 
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "該当の帳票はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "該当の帳票はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "帳票管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "帳票ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  MERR 
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M98 "                TO  SPA-MOTOPG
           MOVE    "MERR"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "MERR"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-M98MSG
      *
           EVALUATE    SPA-MIDCD
               WHEN    "1002"
                   MOVE    "該当のグループの削除処理を行います"
                                       TO  WRK-M98MSG
               WHEN    "1003"
                   MOVE
                   "処理中の可能性がありますが削除処理を行いますか"
                                       TO  WRK-M98MSG
               WHEN    "1005"
                   MOVE    "プレビュー処理を行います"
                                       TO  WRK-M98MSG
               WHEN    "6666"
                   MOVE    "印刷処理中です"
                                            TO  WRK-M98MSG
               WHEN    "8888"
                   MOVE    "処理が正常終了しました"
                                            TO  WRK-M98MSG
           END-EVALUATE
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *     データ収集DB読み込み
      *****************************************************************
       900-JOBLOG-FET-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_joblog"        TO  MCP-TABLE
           MOVE    "key3"
                                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JOBLOG-REC
               MOVE    ZERO                TO  FLG-JOBLOG
           ELSE
               MOVE    1                   TO  FLG-JOBLOG
           END-IF
           .
       900-JOBLOG-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"
                                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"
                                           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"
                                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "PUTWINDOW"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
