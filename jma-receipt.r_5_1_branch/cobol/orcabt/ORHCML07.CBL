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
       PROGRAM-ID.             ORHCML07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ
      *  コンポーネント名  : 入力コード一覧表・特定器材（ＨＣＭＬ０７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/08    MCC-森脇      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/04/24  印刷管理・印刷ＤＢの追加
      *  01.00.02    MCC-藤原     02/05/10  頁数のジョブ管理ＤＢへの更新
      *
      *  01.01.01    NACL-藤原    02/10/10  サイト区分追加
      *  01.01.02    NACL-森脇    03/03/10  TERMIDの領域指定なし
      *  02.00.01    NACL-森脇    03/12/25  その他材料追加
      *  02.00.02    NACL-森脇    06/07/27  フラグ追加、MONFUNC対応
      *  03.05.01    NACL-森脇    07/05/29  グループ診療対応
      *  04.01.01    NACL-森脇    08/01/17  器材点数追加
      *  04.03.01    NACL-森脇    08/06/13  ＣＳＶファイル出力対応
      *  04.08.01    NACL-森脇    14/07/31  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TENSUERR-FILE   ASSIGN  TENSUERR
                                   FILE    STATUS  IS  STS-TENSUERR.
      *    ＣＳＶファイル
           SELECT  TENSUCSV-FILE   ASSIGN  TENSUCSV
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TENSUCSV.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  TENSUERR-FILE.
       01  TENSUERR-REC             PIC X(200). 
      *    ＣＳＶファイル
       FD  TENSUCSV-FILE.
       01  TENSUCSV-REC.
           03  CSV-HOSPNUM         PIC 9(2).
           03  CSV-FILLER1         PIC X(1).
           03  CSV-SRYCD           PIC X(20).
           03  CSV-FILLER2         PIC X(1).
           03  CSV-NAME            PIC X(60).
           03  CSV-FILLER3         PIC X(1).
           03  CSV-TEN             PIC 9(09).99.
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *    COPY    "ORCA-DBPATH".
      *----(04.08.01)--ADD-START---
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TENSUERR//.
      *----(04.08.01)--ADD-END-----
      *     
           COPY    "HCML06.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TENSUERR        PIC X(02).
           03  STS-TENSUCSV        PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-ICD             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-KENSU           PIC 9(07).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           COPY    "CPHCMLSHELL.INC"
                                   REPLACING  //SHELL-//
                                   BY         //WRK-PARA-//.
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-CSVKBN     PIC X(1).
           03  WRK-PARA-FILE       PIC X(1).
           03  WRK-PARA-CSVDIR     PIC X(100).
           03  WRK-PARA-CSVFILE    PIC X(100).
      *----(04.08.01)--UPD-START---
      *     03  TENSUERR            PIC X(100).
           03  TENSUCSV            PIC X(1024).
      *----(04.08.01)--UPD-END-----
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
           03  WRK-TENSUERR          PIC X(200).        
      *
           03  WRK-LNKYMD            PIC X(10).
           03  WRK-MSTYMDH           PIC X(10).
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-TEN               PIC ZZZ,ZZZ,ZZ9.99.    
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関名      
           COPY  "CPSK1001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *  
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    入力コード
       01  ICD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(1000).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END         =   1
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  SUM-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  TENSUERR
           INITIALIZE                  TENSUCSV
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
                                               WRK-PARA-AREA
                                               WRK-PARA-HOSPNUM
                                               TENSUERR
                                               WRK-PARA-CSVKBN
                                               WRK-PARA-FILE
                                               WRK-PARA-CSVDIR
                                               WRK-PARA-CSVFILE
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           MOVE    "ORHCML07"      TO  JOB-PGID
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "入力コード一覧表・特定器材" 
                                   TO  JOB-SHELLMSG
               WHEN    2
                   MOVE    "入力コード一覧表・その他材料" 
                                   TO  JOB-SHELLMSG
               WHEN    3
                   MOVE    "入力コード一覧表・器材商品"
                                   TO  JOB-SHELLMSG
           END-EVALUATE
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *                             
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           MOVE    WRK-LNKYMD       TO  WRK-MSTYMDH
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TENSUERR-BASENAME   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  TENSUERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.              
      *      
      *    医療機関名称編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-MSTYMD     TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE    
               INITIALIZE                  SYS-1001-REC
               MOVE    "医療機関名称が取得できませんでした。"
                                           TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
           .
      *
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      WRK-PARA-CSVKBN =   "1"
      *        ＣＳＶ編集処理
               PERFORM 300-CSV-SEC 
           ELSE
      *        帳票編集処理
               PERFORM 300-PRINT-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END 
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       300-CSV-SEC                 SECTION.
      *
      *----(04.08.01)--UPD-START---
      *     STRING  "/tmp/"             DELIMITED SIZE
           STRING  SGETTEMP-TEMPDIR    DELIMITED SPACE
                   WRK-PARA-CSVFILE    DELIMITED SPACE
                   ".wk"               DELIMITED SIZE
                               INTO    TENSUCSV
           END-STRING
      *----(04.08.01)--UPD-END-----
      *
      *    ＣＳＶファイルを開く
           OPEN    OUTPUT              TENSUCSV-FILE
      *
      *    帳票印刷開始処理
           PERFORM 900-DBSELECT-SEC
      *    帳票編集<明細>処理
           PERFORM 330-CSV-HEN-SEC
      *    帳票印刷終了処理
           PERFORM 900-DBCLOSE-SEC
      *
      *    ＣＳＶファイルを閉じる
           CLOSE                       TENSUCSV-FILE
      *
      *    クライアント保存の場合
           IF      WRK-PARA-FILE       =   "1"
               PERFORM 380-CSV-OUT-SEC
           END-IF
      *
           .
       300-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       300-PRINT-SEC                SECTION.
      *
      *    帳票印刷開始処理
           PERFORM 900-DBSELECT-SEC
           PERFORM UNTIL   FLG-TENSU   =   1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC 
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
                       UNTIL   FLG-TENSU     =   1
                       OR      CNT-LINE      =   35
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *    帳票印刷終了処理
           PERFORM 900-DBCLOSE-SEC
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
                                           SUM-KENSU
      *
           INITIALIZE                      HCML06
           MOVE    SPACE               TO  HCML06
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HCML06-PAGE
      *
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "【特定器材】"      TO  HCML06-DAI
               WHEN    2
                   MOVE    "【その他材料】"    TO  HCML06-DAI
               WHEN    3
                   MOVE    "【器材商品】"      TO  HCML06-DAI
           END-EVALUATE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           PERFORM 900-ICDSELECT-SEC 
           PERFORM UNTIL   FLG-ICD   =   1
               IF      CNT-LINE    =   35  
      *            帳票印刷処理
                   PERFORM 390-PRINT-OUT-SEC
      *            帳票編集<見出し>処理
                   PERFORM 310-HEAD-HEN-SEC
               END-IF
               ADD     1                 TO  CNT-LINE
               MOVE    TNS-TEN           TO  WRK-TEN
               MOVE    WRK-TEN           TO  HCML06-TEN(CNT-LINE)
               MOVE    ICD-INPUTCD       TO  HCML06-SRYCD(CNT-LINE)
               MOVE    TNS-NAME          TO  HCML06-NAME(CNT-LINE)
               PERFORM 900-ICDFETCH-SEC
           END-PERFORM 
           PERFORM 900-ICDCLOSE-SEC 
      *
           PERFORM 900-DBFETCH-SEC 
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       330-CSV-HEN-SEC      SECTION.
      *
           PERFORM UNTIL   FLG-TENSU   =   1
               PERFORM 900-ICDSELECT-SEC 
               PERFORM UNTIL   FLG-ICD     =   1
                   ADD     1               TO  SUM-KENSU
                   INITIALIZE                  TENSUCSV-REC
                   MOVE    TNS-HOSPNUM     TO  CSV-HOSPNUM
                   MOVE    TNS-TEN         TO  CSV-TEN
                   MOVE    ICD-INPUTCD     TO  CSV-SRYCD
                   MOVE    TNS-NAME        TO  CSV-NAME
                   MOVE    ","             TO  CSV-FILLER1
                                               CSV-FILLER2
                                               CSV-FILLER3
                   WRITE   TENSUCSV-REC
                   PERFORM 900-ICDFETCH-SEC
               END-PERFORM 
               PERFORM 900-ICDCLOSE-SEC 
               PERFORM 900-DBFETCH-SEC 
           END-PERFORM    
      *
           .
       330-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       380-CSV-OUT-SEC                SECTION.
      *
           INITIALIZE                      ORCSFILESVAREA
           MOVE    "I"                 TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID    TO  SFILESV-TBL-KEY
           MOVE    "tensu.sh"          TO  SFILESV-SHELLID(1)
           MOVE    1                   TO  SFILESV-SHORI-RENNUM(1)
           MOVE    LNK-PRTKANRI-RENNUM TO  SFILESV-RENNUM(1)
      *----(04.08.01)--UPD-START---
      *     MOVE    "/tmp/"             TO  SFILESV-FOR-FOLDER(1)
           MOVE    SGETTEMP-TEMPDIR    TO  SFILESV-FOR-FOLDER(1)
           STRING  WRK-PARA-CSVFILE        DELIMITED  BY  SPACE
                   ".wk"                   DELIMITED  BY  SIZE
                                   INTO    SFILESV-FOR-DATA (1)
           END-STRING
      *----(04.08.01)--UPD-END-----
           MOVE    WRK-PARA-CSVDIR     TO  SFILESV-TO-FOLDER(1)
           MOVE    WRK-PARA-CSVFILE    TO  SFILESV-TO-DATA(1)
           MOVE    SUM-KENSU           TO  SFILESV-CNT-ALL(1)
           MOVE    JOB-SHELLMSG        TO  SFILESV-TITLE(1)
           CALL    "ORCSFILESV"            USING
                                           ORCSFILESVAREA
                                           SPA-AREA
      *
           .
       380-CSV-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           IF      CNT-LINE             =   ZERO
               GO  TO  390-PRINT-OUT-EXT
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCML06.red"        TO  SPRT-PRTID
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "入力コード一覧表・特定器材"
                                       TO  SPRT-TITLE
               WHEN    2
                   MOVE    "入力コード一覧表・その他材料"
                                       TO  SPRT-TITLE
               WHEN    3
                   MOVE    "入力コード一覧表・器材商品"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    HCML06              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-TENSUERR
               PERFORM 500-ERR-HENSYU-SEC                           
           END-IF                                                              
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TENSUERR-FILE
      *     
           IF      STS-TENSUERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              TENSUERR-FILE
      *
               MOVE    WRK-TENSUERR         TO  TENSUERR-REC
               WRITE   TENSUERR-REC
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM
                                       TO  JOB-HOSPNUM
               MOVE    WRK-TENSUERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           CLOSE   TENSUERR-FILE
           MOVE    1               TO  FLG-END                             
      * 
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORHCML07 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SUM-KENSU       TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *                              
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-LNKYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-LNKYMD
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　検索 
      *****************************************************************
       900-ICDSELECT-SEC           SECTION.
      *  
           INITIALIZE                  ICD-REC
           MOVE    TNS-SRYCD       TO  ICD-SRYCD
           MOVE    WRK-PARA-HOSPNUM    TO  ICD-HOSPNUM
           MOVE    ICD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
              PERFORM 900-ICDFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-ICD
           END-IF
           .
       900-ICDSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードを検索処理
      *****************************************************************
       900-ICDFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ICD-REC
               MOVE    ZERO            TO  FLG-ICD
               IF      WRK-PARA-CYUKBN     =   3
                   AND ICD-DSPSEQ          >   1
                   MOVE    1           TO  FLG-ICD
               END-IF
           ELSE
               MOVE    1               TO  FLG-ICD
           END-IF  
           .
       900-ICDFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　クローズ処理
      *****************************************************************
       900-ICDCLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ICDCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ　検索 
      *****************************************************************
       900-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           EVALUATE    WRK-PARA-KBN
               WHEN    1
                   MOVE    "700000000"         TO  TNS-SRYCD
                   MOVE    "799999999"         TO  TNSUP-SRYCD 
               WHEN    2
                   MOVE    "059000001"         TO  TNS-SRYCD
                   MOVE    "059999999"         TO  TNSUP-SRYCD 
               WHEN    3
                   MOVE    "058000001"         TO  TNS-SRYCD
                   MOVE    "058999999"         TO  TNSUP-SRYCD 
           END-EVALUATE
      *
           MOVE    WRK-PARA-MSTYMD     TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
            EVALUATE    WRK-PARA-SYUKBN        
                WHEN    1
                    MOVE    "key12s1"   TO  WRK-PATH 
                WHEN    2
                    MOVE    "key12m1"   TO  WRK-PATH 
                WHEN    3
                    MOVE    "key12k1"   TO  WRK-PATH
                WHEN    4
                    MOVE    "key12i1"   TO  WRK-PATH
            END-EVALUATE         
      *
           MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF        
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタを検索処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF  
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
grpsys                                         SPA-AREA
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
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
