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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBPREV1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : プレビュー処理
      *  コンポーネント名  : プレビュー管理ＤＢの更新
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/16    NACL-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/11/29  引数にエラーファイル名を追加
      *  01.00.02    NACL-藤原    04/12/01  印刷中止処理の追加
      *
      *  03.00.01    NACL-藤原    06/05/29  tbl_prevkanri のプリンタ名を
      *                                     tbl_prtdata より設定
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-竹田    07/05/09  オンライン再印刷対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    印刷用データ
       01  PRT-REC.
           03  PRT-PRTID           PIC X(30).
           03  PRT-PRTNM           PIC X(20).
           03  PRT-OFFSET-X        PIC X(05).
           03  PRT-OFFSET-Y        PIC X(05).
           03  PRT-SITEKBN         PIC X(01).
      *  01.01.02  ADD START 
           03  PRT-PRTKBN          PIC X(01).
           03  PRT-PSFILE-AREA.
             05  PRT-PSFILE-FOLDER     PIC X(165).
             05  PRT-PSFILE-TBL-KEY    PIC X(08).
             05  PRT-PSFILE-RENNUM     PIC X(04).
             05  PRT-PSFILE-GROUP      PIC X(14).
             05  PRT-PSFILE-SRENNUM    PIC X(04).
             05  PRT-PSFILE-PAGE       PIC X(05).
             05  PRT-PSFILE-UUID       PIC X(36).
      *  01.01.02  ADD END
           03  PRT-PRTDATA         PIC X(20000).
      *
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-PRTDATA         PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
           03  FLG-PREVIEW         PIC 9(01).
           03  FLG-IN              PIC 9(01).
      *
      *    添字領域
       01  SOEJI-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(05).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(10).
           03  CNT-KETA            PIC 9(05).
           03  WRK-MOJISU          PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HC01PARA-CNT    PIC  9(03).
      *
           03  WRK-RECEERR         PIC X(200). 
      *
           03  WRK-PATH            PIC X(64). 
      *
           03  WRK-FILENAME        PIC X(200). 
           03  WRK-FILENAME1       PIC X(200). 
           03  SPA-NAI-ENV         PIC X(200). 
      *
      *    一時ディレクトリ名
           03  WRK-SGETTEMP-TEMPDIR
                                   PIC X(1024).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-STPAGE     PIC 9(05).
           03  WRK-PARA-EDPAGE     PIC 9(05).
           03  WRK-PARA-PRTFLG     PIC X(01).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-HOSPNUM    PIC X(02).
      *
       01  WRK-TBL-GROUP.
           03  WK-TBL-G           PIC  X(14) OCCURS 5.
      *    印刷用コンスタント領域 
           COPY    "CPPRTCONST.INC".          
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理ＤＢ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷ＤＢ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *    プレビューＤＢ
           COPY    "CPPRVKANRI.INC".          
           COPY    "CPPREVIEW.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME     
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-STPAGE
                                               WRK-PARA-EDPAGE
                                               WRK-PARA-PRTFLG     
                                               WRK-PARA-SYOKBN
                                               RECEERR 
                                               WRK-PARA-HOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    SGETTEMP-TEMPDIR    TO  WRK-SGETTEMP-TEMPDIR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBPREV1"         TO  JOB-PGID
           MOVE    "プレビュー設定処理"
                                       TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-END         =   ZERO
      *        印刷管理ＤＢ読み込み処理
               PERFORM 900-PRTKANRI-SELECT-SEC
      *
      *        プレビュー用前処理
               MOVE    ZERO            TO  WRK-MOJISU
               IF      WRK-PARA-PRTFLG     =   "2"  OR "3"
                 IF    LNK-PRTKANRI-TBL-KEY    NOT =   SPACE
                   PERFORM VARYING     IDX     FROM    8   BY  -1
                           UNTIL       IDX     <       1
                       IF      LNK-PRTKANRI-TBL-KEY(IDX:1)
                                               NOT =   SPACE
                           MOVE    IDX     TO  WRK-MOJISU
                           MOVE    1       TO  IDX
                       END-IF
                   END-PERFORM
      *            PERFORM 900-PRVKANRI-DELETE-SEC
                 END-IF
               END-IF
      *
      *        プレビューDB履歴更新処理
               PERFORM 120-PRVKANRI-RRKUPD-SEC
           END-IF
      * For Debug st
      *    MOVE ZERO TO      FLG-END
      * For Debug ed
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューDB履歴更新処理
      *****************************************************************
      *
       120-PRVKANRI-RRKUPD-SEC                   SECTION.
      *    TBLキーセット
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
      *    MOVE    PRTKANRI-RENNUM     TO  PRVKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRVKANRI-TBL-GROUP
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  PRVKANRI-RECE-FLG
           MOVE    WRK-PARA-SYOKBN     TO  PRVKANRI-SHORI-KBN
           MOVE    WRK-PARA-HOSPNUM    TO  PRVKANRI-HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PREVIEW
                   MOVE    MCPDATA-REC     TO  PRVKANRI-REC
                   MOVE    PRVKANRI-T-G    TO  WRK-TBL-GROUP
               ELSE  
                   MOVE    1               TO  FLG-PREVIEW
                   MOVE    SPACE           TO  WRK-TBL-GROUP
               END-IF 
           ELSE 
               MOVE    1                   TO  FLG-PREVIEW
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       120-PRVKANRI-RRKUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
           IF      FLG-END         =   1
               GO  TO  200-MAIN-EXT
           END-IF  
      *
      *    印刷用ファイル作成  
           MOVE    "HC99"          TO  HC01PARA-FORM-ID
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  HC01PARA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    1               TO  HC01PARA-CNT
      *+   OPEN    OUTPUT              PRT-FILE
      *        最新以外の帳票管理ＤＢを削除
               MOVE    SPACE           TO  PRVKANRI-REC
               INITIALIZE                  PRVKANRI-REC
               MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
               MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  PRVKANRI-RECE-FLG
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRVKANRI-TBL-GROUP
               MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  PRVKANRI-TBL-UUID
               MOVE    WRK-PARA-SYOKBN TO  PRVKANRI-SHORI-KBN
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRVKANRI-HOSPNUM
               MOVE    PRVKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prvkanri"  TO  MCP-TABLE
               MOVE    "del12"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
           PERFORM                     UNTIL   FLG-PRTKANRI =   1
                                       OR      FLG-END      =   1   
      *        印刷ＤＢ編集処理
               PERFORM 900-PRTDATA-SELECT-SEC
               PERFORM                     UNTIL   FLG-PRTDATA  =   1
                                           OR      FLG-END      =   1
                   PERFORM 2001-PRTDATA-PRINT-SEC
                   PERFORM 900-PRTDATA-FET-SEC
               END-PERFORM    
      *
               MOVE    "tbl_prtdata"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               PERFORM 2003-PRTKANRI-UPD-SEC
               PERFORM 900-PRTKANRI-FET-SEC
           END-PERFORM                                
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END     
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRTDATA-PRINT-SEC          SECTION.
      *
           MOVE    SPACE               TO  PRT-REC 
           MOVE    PRTDATA-PRTID       TO  PRT-PRTID
           IF      PRTDATA-PRTNM       =   SPACE
               MOVE    PRTKANRI-PRTNM      TO  PRT-PRTNM
           ELSE
               MOVE    PRTDATA-PRTNM       TO  PRT-PRTNM
           END-IF
           EVALUATE    PRTDATA-SITEKBN
               WHEN    "1"
               WHEN    "2"               
                   MOVE    PRTDATA-SITEKBN     TO  PRT-SITEKBN
               WHEN    OTHER
                   SET     PRTCONST-IDX        TO  1
                   SEARCH  PRTCONST-TX VARYING PRTCONST-IDX
                       AT  END
                           MOVE    "2"         TO  PRT-SITEKBN
                       WHEN    PRTCONST-PRTID (PRTCONST-IDX)
                                               =   PRTDATA-PRTID
                           MOVE    "1"         TO  PRT-SITEKBN
                   END-SEARCH
           END-EVALUATE                                                                
      *  01.01.02  SATRT
           MOVE WRK-PARA-PRTFLG                TO  PRT-PRTKBN
           MOVE    ZERO                        TO  CNT-KETA
      *****MOVE    "/var/lib/jma-receipt/orca_ps/"        
           MOVE    WRK-SGETTEMP-TEMPDIR        TO  PRT-PSFILE-FOLDER
           MOVE    LNK-PRTKANRI-TBL-KEY        TO  PRT-PSFILE-TBL-KEY
           MOVE    PRTDATA-RENNUM              TO  PRT-PSFILE-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO  PRT-PSFILE-GROUP 
           MOVE    PRTDATA-SHORI-RENNUM        TO  PRT-PSFILE-SRENNUM
           MOVE    PRTDATA-PAGE                TO  PRT-PSFILE-PAGE  
      *  01.01.02  END
           MOVE    PRTDATA-PRTDATA             TO  PRT-PRTDATA
      *    WRITE   PRT-REC
      *
      *  01.01.02  SATRT
           IF   WRK-PARA-PRTFLG        =   "2"  OR  "3"  OR  "4"
                PERFORM 2004-PRVKANRI-INS-SEC
           END-IF
      *  01.01.02  END
      *
           ADD     1                   TO  CNT-OUT
                                           CNT-IN-CANCEL     
      *
           PERFORM 2002-PRTDATA-UPD-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       2001-PRTDATA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷フラグセット処理
      *****************************************************************
       2002-PRTDATA-UPD-SEC          SECTION.
      *
      *    印刷フラグセット
           MOVE    "1"                 TO  PRTDATA-PRTFLG
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢの更新でエラーになりました"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF    
           .
       2001-PRTDATA-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷フラグセット処理
      *****************************************************************
       2003-PRTKANRI-UPD-SEC          SECTION.
      *
           MOVE    "1"                 TO  PRTKANRI-PRTFLG
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷管理ＤＢの更新でエラーになりました"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF    
           .
       2003-PRTKANRI-UPD-EXT.
           EXIT.
      *
      *  01.01.02  SATRT
      *****************************************************************
      *    プレビューＤＢ削除処理
      *****************************************************************
       900-PRVKANRI-DELETE-SEC        SECTION.
      *
      *    TBLキーセット
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
           MOVE    WRK-PARA-HOSPNUM    TO  PRVKANRI-HOSPNUM
      *
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "プレビューＤＢの削除でエラーになりました"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF    
           .
       900-PRVKANRI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューＤＢ追加処理
      *****************************************************************
       2004-PRVKANRI-INS-SEC          SECTION.
      *
      *    TBLキーセット
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
           MOVE    PRTKANRI-RENNUM     TO  PRVKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRVKANRI-TBL-GROUP
           MOVE    PRTDATA-SHORI-RENNUM
                                       TO  PRVKANRI-SHORI-RENNUM
           MOVE    PRT-PSFILE-AREA     TO  PRVKANRI-FILENAME
      *    STRING  PRT-PSFILE-FOLDER   DELIMITED  BY  SPACE
      *            PRT-PSFILE-TBL-KEY  DELIMITED  BY  SPACE
      *            PRT-PSFILE-RENNUM   DELIMITED  BY  SPACE
      *            PRT-PSFILE-GROUP    DELIMITED  BY  SPACE
      *            PRT-PSFILE-SRENNUM  DELIMITED  BY  SPACE
      *            PRT-PSFILE-PAGE     DELIMITED  BY  SPACE
      *                                INTO   PRVKANRI-FILENAME
      *    END-STRING
           MOVE    WRK-PARA-SHELLID    TO  PRVKANRI-SHELLID
           MOVE    PRTDATA-PAGE        TO  PRVKANRI-PAGE
           MOVE    PRTDATA-PRTID       TO  PRVKANRI-PRTID
           MOVE    PRTKANRI-TITLE      TO  PRVKANRI-TITLE
           MOVE    PRTDATA-PTID        TO  PRVKANRI-PTID
           MOVE    WRK-PARA-PRTFLG     TO  PRVKANRI-SHORI-MOD-FLG
      *    MOVE    PRTKANRI-SRYYM      TO  PRVKANRI-SRYYM
           MOVE    PRTDATA-SRYYM       TO  PRVKANRI-SRYYM
           MOVE    PRTKANRI-SKYYMD     TO  PRVKANRI-SKYYMD
           MOVE    PRTDATA-PRTNM       TO  PRVKANRI-PRTNM
           MOVE    PRTKANRI-TBL-GROUP  TO  
                                       PRVKANRI-TBL-GROUP-RRK(1)
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  PRVKANRI-RECE-FLG
           MOVE    WRK-PARA-SYOKBN     TO  PRVKANRI-SHORI-KBN
           MOVE    WK-TBL-G(1)         TO  
                                       PRVKANRI-TBL-GROUP-RRK(2)
           MOVE    WK-TBL-G(2)         TO  
                                       PRVKANRI-TBL-GROUP-RRK(3)
           MOVE    WK-TBL-G(3)         TO  
                                       PRVKANRI-TBL-GROUP-RRK(4)
           MOVE    WK-TBL-G(4)         TO  
                                       PRVKANRI-TBL-GROUP-RRK(5)
           MOVE    PRTKANRI-OPID       TO  PRVKANRI-OPID
           MOVE    SYS-TIME            TO  PRVKANRI-CREYMD
           MOVE    SPACE               TO  PRVKANRI-UPYMD
           MOVE    SPACE               TO  PRVKANRI-UPHMS
           MOVE    WRK-PARA-HOSPNUM    TO  PRVKANRI-HOSPNUM
      *
           PERFORM 900-PRVKANRI-INV-SEC
           IF      FLG-PREVIEW         =   ZERO  
               GO  TO                  2004-PRVKANRI-INS-EXT
           END-IF
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "プレビューＤＢの追加でエラーになりました"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF    
           .
       2004-PRVKANRI-INS-EXT.
           EXIT.
      *
      *  01.01.02  END
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
                                           FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO )
           AND   ( JOB-STOPFLG     >   ZERO )
               MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
               OPEN    INPUT   RECEERR-FILE
               IF      STS-RECEERR         =   ZERO
                   CLOSE   RECEERR-FILE
               ELSE
                   OPEN    OUTPUT              RECEERR-FILE
      *    
                   MOVE    WRK-RECEERR         TO  RECEERR-REC
                   WRITE   RECEERR-REC
                   CLOSE   RECEERR-FILE
      *         
                   IF      JOB-STOPFLG     =   1
      *                ジョブ管理終了処理
                       MOVE    "CA2"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    古い履歴の削除     
           PERFORM 900-PRV-RRKDEL-SEC
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      **** MOVE    CNT-OUT         TO  JOB-UPDCNT                        
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBPREV1 IN  "  CNT-IN
           DISPLAY "*** ORCBPREV1 OUT "  CNT-OUT
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   プレビュー管理DBの履歴削除 
      *****************************************************************
       900-PRV-RRKDEL-SEC              SECTION.
      *
      *        最新以外の帳票管理ＤＢを削除 LNK-PRTKANRI-TBL-KEY
           IF      LNK-PRTKANRI-TBL-KEY     =   "RECEPTX"
           OR      LNK-PRTKANRI-TBL-KEY     =   "ONLINE"
                   GO    TO            900-PRV-RRKDEL-EXT
           END-IF
           MOVE    SPACE               TO  PRVKANRI-REC
           INITIALIZE                  PRVKANRI-REC
           MOVE    LNK-PRTKANRI-TBL-KEY     TO  PRVKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP   TO  PRVKANRI-TBL-GROUP
           MOVE    LNK-PRTKANRI-TBL-UUID    TO  PRVKANRI-TBL-UUID
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"  TO  MCP-TABLE
           MOVE    "del12"         TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-PRV-RRKDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRTKANRI-SELECT-SEC          SECTION.
      *
           INITIALIZE                      PRTKANRI-REC
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRTKANRI-TBL-GROUP
           MOVE    WRK-PARA-HOSPNUM    TO  PRTKANRI-HOSPNUM
      *
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
                                               FLG-END
           END-IF
           .
       900-PRTKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-INV-SEC            SECTION.
      *
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PREVIEW
               ELSE  
                   MOVE    1                   TO  FLG-PREVIEW
               END-IF 
           ELSE 
               MOVE    1                   TO  FLG-PREVIEW
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PRVKANRI-INV-EXT.
           EXIT.
      *      
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRTKANRI-FET-SEC            SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
               IF      LNK-PRTKANRI-RENNUM  =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-RENNUM   NOT = 
                                           PRTKANRI-RENNUM
                       GO  TO  900-PRTKANRI-FET-SEC
                   END-IF
               END-IF                                             
               IF      LNK-PRTKANRI-SHORI-RENNUM   =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-SHORI-RENNUM   NOT = 
                                           PRTKANRI-SHORI-RENNUM
                       GO  TO  900-PRTKANRI-FET-SEC
                   END-IF
               END-IF                                             
               MOVE    ZERO                TO  FLG-PRTKANRI
                                               FLG-END
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
                                               FLG-END 
           END-IF
           .
       900-PRTKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTDATA-SELECT-SEC          SECTION.
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
           MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
           MOVE    WRK-PARA-STPAGE     TO  PRTDATAXX-STPAGE
           MOVE    WRK-PARA-EDPAGE     TO  PRTDATAXX-EDPAGE
           MOVE    WRK-PARA-HOSPNUM    TO  PRTDATA-HOSPNUM
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTDATA-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTDATA
           END-IF
           .
       900-PRTDATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTDATA-FET-SEC            SECTION.
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTDATA-REC
               ADD     1                   TO  CNT-IN
               MOVE    ZERO                TO  FLG-PRTDATA
           ELSE  
               MOVE    1                   TO  FLG-PRTDATA
           END-IF 
           .
       900-PRTDATA-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
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
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
