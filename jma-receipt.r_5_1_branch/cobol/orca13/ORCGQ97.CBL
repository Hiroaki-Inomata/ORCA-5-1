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
       PROGRAM-ID.                 ORCGQ97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 処理結果
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/06/26    MCC−太田　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07.05.23  グループ診療対応
      *  04.01.01    NACL-藤原    07/10/01  クライアント保存対応
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
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //TMP01FILE//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "Q01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-LST             PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-SYMD-TAIHI      PIC X(08).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-PAGE            PIC ZZ,ZZZ,ZZ9.
           03  WRK-JOBID           PIC 9(07).
           03  WRK-MSG             PIC X(100).
           03  WRK-RUNJOB          PIC 9(05).
           03  WRK-KENSU-Z         PIC ZZZZZZ9.
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
           03  CONST-JOBID2        PIC 9(07)   VALUE   2.
           03  CONST-JOB-SHELLID   PIC X(08)   VALUE   "ORCBQ01".
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
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
           COPY    "MONBLOB".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
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
           COPY    "ORCA13SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-Q01FREE
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
           MOVE    SPA-Q01FREE     TO  SPA-FREE
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q97"               TO  MCP-WINDOW
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
           MOVE    SPA-Q97-MOTOPG      TO  SPA-SAKIPG
           MOVE    "Q97"               TO  SPA-MOTOPG
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
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                   Q97
           MOVE    ZERO             TO  IDX-LST
           MOVE    ZERO             TO  WRK-RUNJOB
           MOVE    SPACE            TO  WRK-MSG
      *
           MOVE    5                TO  Q97-DURATION
      *
           MOVE    "B11"            TO  MCP-WIDGET
      *
           MOVE    CONST-JOBID      TO  WRK-JOBID
           PERFORM 4901-KAKUNIN-SEC
      *
           MOVE    CONST-JOBID2     TO  WRK-JOBID
           PERFORM 4901-KAKUNIN-SEC
      *
           MOVE    IDX-LST          TO  Q97-COUNT
      *
           EVALUATE    TRUE
           WHEN  ( IDX-LST          =   ZERO )
               MOVE    "該当する処理はありません"
                                    TO  Q97-MSG
           WHEN  ( WRK-MSG      NOT =   SPACE )
               MOVE    WRK-MSG      TO  Q97-MSG
           WHEN  ( WRK-RUNJOB   NOT =   ZERO )
               MOVE    "処理中です"
                                   TO  Q97-MSG
           WHEN  OTHER
               MOVE    "処理は正常に終了しました"
                                   TO  Q97-MSG
           END-EVALUATE
      *
           IF    ( SPA-QID1-FLG    =   "SV" )
               IF    ( IDX-LST          =   ZERO )
               OR    ( WRK-MSG      NOT =   SPACE )
               OR    ( WRK-RUNJOB   NOT =   ZERO )
                   CONTINUE
               ELSE
                   IF      SPA-Q97-FILESV-CHK-FLG  =   ZERO
                       PERFORM 4902-FILE-INFO-CHK-SEC
                   ELSE
      *                ファイルエントリ処理(クライアント保存)
                       MOVE    1               TO  SPA-SFILESV-IDX 
                       PERFORM 3001-FILEENTRY-SEC
                       IF      FLG-END         =   1
                           GO  TO  490-KAKUNIN-EXT
                       END-IF
                   END-IF
               END-IF    
           END-IF
      *
           PERFORM 4901-PUT-CURRENT-WINDOW-SEC
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       4901-KAKUNIN-SEC                SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-JOBID       TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC 
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
      *
               COMPUTE IDX-LST     =   IDX-LST +   1
      *
               MOVE    JOB-JOBID       TO  Q97-RENNUM   (IDX-LST)
               MOVE    JOB-SHELLMSG    TO  Q97-SHORINM  (IDX-LST)
      *
               MOVE    JOB-STARTYMD    TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  Q97-STARTDATE (IDX-LST)
      *
               IF    ( JOB-STARTTIME   =   SPACE )
                   MOVE    SPACE   TO  Q97-STARTTIME(IDX-LST)
               ELSE
                   MOVE    JOB-STARTTIME(1:2)
                                   TO  WRK-HH
                   MOVE    JOB-STARTTIME(3:2)
                                   TO  WRK-MM
                   MOVE    JOB-STARTTIME(5:2)
                                   TO  WRK-SS
                   MOVE    WRK-HENTIME TO  Q97-STARTTIME(IDX-LST)
               END-IF
      *
               MOVE    JOB-ENDYMD      TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  Q97-ENDDATE (IDX-LST)
      *
               IF    ( JOB-ENDTIME =   SPACE )
                   MOVE    SPACE       TO  Q97-ENDTIME  (IDX-LST)
               ELSE
                   MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
                   MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
                   MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
                   MOVE    WRK-HENTIME TO  Q97-ENDTIME  (IDX-LST)
               END-IF
               MOVE    JOB-UPDCNT      TO  WRK-PAGE
               MOVE    WRK-PAGE        TO  Q97-PAGE     (IDX-LST)
               MOVE    JOB-YOBI        TO  Q97-ERRMSG   (IDX-LST)
      *
               IF    ( JOB-ENDYMD      =   SPACE )
                   COMPUTE WRK-RUNJOB  =   WRK-RUNJOB  +   1
               ELSE
                   IF    ( JOB-ERRCD   =   SPACE )
                       CONTINUE
                   ELSE
                       IF    ( WRK-MSG =    SPACE )
                           STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                   "【"        DELIMITED  BY  SIZE
                                  JOB-SHELLMSG DELIMITED  BY  SPACE
                                  "】"         DELIMITED  BY  SIZE
                           INTO  WRK-MSG
                           END-STRING
                       END-IF
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       4901-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    次画面再表示処理
      *****************************************************************
       4901-PUT-CURRENT-WINDOW-SEC     SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "Q97    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
      *
       4901-PUT-CURRENT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント保存情報チェック処理
      *****************************************************************
       4902-FILE-INFO-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "R"             TO  SFILESV-MODE
           MOVE    CONST-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    SPA-Q97-SELHMS  TO  SFILESV-SRYYM(1)
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
               MOVE    1                   TO  SPA-Q97-FILESV-CHK-FLG
      *
               MOVE    1                   TO  Q97-DURATION
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
           MOVE     SPACE              TO  TMP01FILE-AREA
           INITIALIZE                      TMP01FILE-AREA
      *
           MOVE    SPA-HOSPNUM         TO  TMP01FILE-HOSPNUM
           MOVE    "OR013097"          TO  TMP01FILE-FILE-ID
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
           INITIALIZE                      MONBLOB
           MOVE    SPA-SFILESV-FOR-DATA   (SPA-SFILESV-IDX) 
                                       TO  MONBLOB-ID
           MOVE    TMP01FILE           TO  MONBLOB-FILENAME
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "monblob"           TO  MCP-TABLE
           PERFORM 910-MONBLOBEXPORT-SEC
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    TMP01FILE           TO  ORCA-BLOB-FILE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           PERFORM 910-BLOBIMPORT-SEC
      *
           MOVE    ORCA-BLOB-OBJECT    TO  Q97-OBJECTDATA
           MOVE    SPA-SFILESV-TO-DATA   (SPA-SFILESV-IDX)
                                       TO  Q97-FILENAME
           MOVE    ZERO                TO  Q97-DURATION
      *
           CALL "CBL_DELETE_FILE"  USING TMP01FILE
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       3001-FILEENTRY-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  WRK-HENYMDG
           END-IF.
      *
       700-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
      *****************************************************************
      *    MONBLOBEXPORT処理
      *****************************************************************
       910-MONBLOBEXPORT-SEC           SECTION.
      *
           MOVE    "MONBLOBEXPORT" TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MONBLOB
      *
           .
       910-MONBLOBEXPORT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
