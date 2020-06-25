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
       PROGRAM-ID.                 ORCGL99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.07.02  タイマーイベント追加
      *  03.05.00    NACL-太田    07.05.28  グループ診療対応
      *  03.05.01    NACL-藤原    07/09/19  クライアント保存対応
      *  04.06.00    NACL-竹田    10/11/25  クライアント保存拡張対応
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
           COPY    "L01COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "L01SCR-SPA".
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
           03  WRK-KENSU-Z         PIC ZZZZZZ9.
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
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "051".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "DAILY".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "DAILY".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *    ジョブ管理ＤＢのキー値（統計処理用）  
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "DAILY1".
           03  WRK-CONS-JOB-JOBID1 PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK3003.INC".
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           COPY    "ORCA51SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-L01FREE
           MOVE    SPA-WORK        TO  SPA-L01KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-L01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-L01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    "B01"               TO  MCP-WIDGET
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "L99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
      *    プレビュー画面へ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4501-NEXT-GMN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    クライアント保存用作業ファイル削除
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-SFILESV-MAX
               INITIALIZE              ORCSFDELAREA
               STRING  SPA-SFILESV-FOR-FOLDER (IDX)
                                           DELIMITED  BY  SPACE
                       SPA-SFILESV-FOR-DATA   (IDX) 
                                           DELIMITED  BY  SPACE
                                           INTO  IF-FILENAME
               END-STRING
               CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-L99-FLG
      *
           MOVE    SPA-L99-MOTOPG      TO  SPA-SAKIPG
           MOVE    "L99"               TO  SPA-MOTOPG
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
      *    更新処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
           MOVE    "OK"                TO  SPA-L99-FLG
      *     
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "L99"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *     
           INITIALIZE                      L99
           MOVE    ZERO                TO  L99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  L99-B12-STATE
      *
           MOVE    5                   TO  L99-DURATION
      *
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           IF      SPA-MOTOPG          =   "L01"        
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                               SYS-3003-EDYUKYMD
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-3003-REC 
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SYS-3003-JOBID
      *                ジョブ管理チェック処理
                       MOVE    "CHK"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                       MOVE    IDX             TO  JOB-JOBID
                       MOVE    WRK-CONS-JOB-SHELLID
                                               TO  JOB-SHELLID
                       CALL    "ORCSJOB"       USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
                       IF      SJOBKANRI-RETURN    =   ZERO
                           PERFORM 4901-KAKUNIN-HYOUJI-SEC
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *       ジョブ管理チェック処理
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID1
                                       TO  JOB-SHELLID
               CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
               MOVE    1               TO  IDX 
               IF      SJOBKANRI-RETURN    =   ZERO
                   PERFORM 4901-KAKUNIN-HYOUJI-SEC
               END-IF
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( L99-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( L99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  L99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  L99-MSG
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF    ( SPA-LID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  L99-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                       MOVE    WIDGET-NORMAL    TO  L99-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
               END-IF
           END-IF
           IF    ( SPA-LID1-FLG    =   "SV" )
               IF      JOB-ENDYMD  NOT =   SPACE
                   IF      JOB-ERRCD       =   "9999"
                       CONTINUE
                   ELSE
                       IF      SPA-L99-FILESV-CHK-FLG  =   ZERO
                           PERFORM 4902-FILE-INFO-CHK-SEC
                       ELSE
      *                    ファイルエントリ処理(クライアント保存)
                           MOVE    1               TO  SPA-SFILESV-IDX 
                           PERFORM 3001-FILEENTRY-SEC
                           IF      FLG-END         =   1
                               GO  TO  490-KAKUNIN-EXT
                           END-IF
                       END-IF
                   END-IF
               END-IF    
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "L99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       4901-KAKUNIN-HYOUJI-SEC            SECTION.
      *     
           MOVE    JOB-JOBID       TO  L99-RENNUM   (IDX)
           MOVE    JOB-SHELLMSG    TO  L99-SHORINM  (IDX)
           MOVE    JOB-STARTTIME(1:2)
                                   TO  WRK-HH
           MOVE    JOB-STARTTIME(3:2)
                                   TO  WRK-MM
           MOVE    JOB-STARTTIME(5:2)
                                   TO  WRK-SS
           MOVE    WRK-HENTIME     TO  L99-STARTTIME(IDX)
           MOVE    JOB-ENDTIME(1:2)
                                   TO  WRK-HH
           MOVE    JOB-ENDTIME(3:2)
                                   TO  WRK-MM
           MOVE    JOB-ENDTIME(5:2)
                                   TO  WRK-SS
           MOVE    WRK-HENTIME     TO  L99-ENDTIME  (IDX)
           MOVE    JOB-UPDCNT      TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  L99-PAGE     (IDX)
           MOVE    JOB-YOBI        TO  L99-ERRMSG   (IDX)
           IF      JOB-ENDYMD      =   SPACE
               MOVE    "処理中です"    TO  L99-MSG
           ELSE
              IF      JOB-ERRCD       =   SPACE
                  CONTINUE
              ELSE
                  MOVE    SPACE       TO  L99-MSG
                  PERFORM 4901-JOB-ERRSET-SEC
                  IF      L99-MSG     =   SPACE
                       STRING  JOB-YOBI        DELIMITED BY SPACE
                               "【"            DELIMITED BY SIZE
                               JOB-SHELLMSG    DELIMITED BY SPACE
                               "】"            DELIMITED BY SIZE
                                               INTO  L99-MSG
                       END-STRING   
                   END-IF
              END-IF                          
           END-IF    
           MOVE    IDX                     TO  L99-COUNT    
           IF    ( JOB-JOBID   =   SPA-L01-JOB-JOBID )
                   MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
           END-IF
           .
       4901-KAKUNIN-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント保存情報チェック処理
      *****************************************************************
       4902-FILE-INFO-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "R"             TO  SFILESV-MODE
           MOVE    WRK-CONS-JOB-SHELLID1
                                   TO  SFILESV-TBL-KEY
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
           IF      SFILESV-RETURN  =   ZERO
               INITIALIZE               SPA-SFILESV-AREA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       100
                       OR      SFILESV-SHELLID (IDX)
                                   =       SPACE
                   MOVE    SFILESV-TITLE        (IDX)
                                       TO  SPA-SFILESV-TITLE     (IDX)
                   MOVE    SFILESV-CNT-ALL      (IDX)
                                       TO  SPA-SFILESV-CNT-ALL   (IDX)
                   MOVE    SFILESV-FOR-FOLDER   (IDX)
                                       TO  SPA-SFILESV-FOR-FOLDER(IDX)
                   MOVE    SFILESV-FOR-DATA     (IDX)
                                       TO  SPA-SFILESV-FOR-DATA  (IDX)
                   MOVE    SFILESV-TO-FOLDER    (IDX)
                                       TO  SPA-SFILESV-TO-FOLDER (IDX)
                   MOVE    SFILESV-TO-DATA      (IDX)
                                       TO  SPA-SFILESV-TO-DATA   (IDX)
                   MOVE    SFILESV-CODE-TYPE    (IDX)
                                       TO  SPA-SFILESV-CODE-TYPE (IDX)
                   MOVE    SFILESV-DATA-TYPE    (IDX)
                                       TO  SPA-SFILESV-DATA-TYPE (IDX)
                   ADD     1           TO  SPA-SFILESV-MAX
               END-PERFORM
      *
               MOVE    1                   TO  SPA-L99-FILESV-CHK-FLG
      *
               MOVE    1                   TO  L99-DURATION
           END-IF
      *
           .
       4902-FILE-INFO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエントリ処理(クライアント保存)
      *****************************************************************
       3001-FILEENTRY-SEC          SECTION.
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           STRING  SPA-SFILESV-FOR-FOLDER (SPA-SFILESV-IDX)
                                       DELIMITED  BY  SPACE
                   SPA-SFILESV-FOR-DATA   (SPA-SFILESV-IDX) 
                                       DELIMITED  BY  SPACE
                                       INTO  ORCA-BLOB-FILE
           END-STRING
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *
           MOVE    ORCA-BLOB-OBJECT    TO  L99-OBJECTDATA
           MOVE    SPA-SFILESV-TO-DATA   (SPA-SFILESV-IDX)
                                       TO  L99-FILENAME
           MOVE    ZERO                TO  L99-DURATION
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       3001-FILEENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       4901-JOB-ERRSET-SEC             SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】" 
                                               TO  L99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】" 
                                               TO  L99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】" 
                                               TO  L99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】" 
                                               TO  L99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】" 
                                               TO  L99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】" 
                                               TO  L99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】" 
                                               TO  L99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
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
           MOVE    "key10"             TO  MCP-PATHNAME
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
