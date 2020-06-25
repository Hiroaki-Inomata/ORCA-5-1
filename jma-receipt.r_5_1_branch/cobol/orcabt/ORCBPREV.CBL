      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * COpyright(C) 2002 JMA (Japan Medical Association)
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
       PROGRAM-ID.             ORCBPREV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : プレビュー用ＤＢの更新
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/05    NACL-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.01    NACL-竹田    07/05/09  オンライン再印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  PRT-FILE-ID
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTID           PIC X(30).
           03  PRT-PRTNM           PIC X(20).
           03  PRT-OFFSET-X        PIC X(05).
           03  PRT-OFFSET-Y        PIC X(05).
           03  PRT-SITEKBN         PIC X(01).
           03  PRT-PRTKBN          PIC X(01).
           03  PRT-PSFILE-AREA.
             05  PRT-OUT             PIC X(165).
             05  PRT-TBL-KEY         PIC X(08).
             05  PRT-RENNUM          PIC 9(04).
             05  PRT-TBL-GROUP       PIC X(14).
             05  PRT-SHORI-RENNUM    PIC 9(04).
             05  PRT-PAGE            PIC 9(05).
             05  PRT-TBL-UUID        PIC X(36).
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
       COPY    "CPASGNPARA.INC"    REPLACING //ASGNPARA//
                                   BY        //PRT-FILE-ID//.
      *
       01  PS-PRTDATA              PIC X(100000).
       01  PS-FILE-ID              PIC X(200).
       01  PS-FILE-DATA            PIC X(200).
      *
      *    添字領域
       01  SOEJI-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *    スパ領域
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
           03  STS-PS              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN              PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-PSFILE          PIC 9(01).
           03  FLG-PRTDATA         PIC 9(01).
           03  FLG-PREVIEW         PIC 9(01).
           03  FLG-PRVKANRI        PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
           03  FLG-MATCH           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(04).
           03  CNT-OUT             PIC 9(04).
           03  CNT-OUT-COMMIT      PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *  01.01.02  ADD START
           03  WRK-FILENAME        PIC X(200).
           03  WRK-FILENAME1       PIC X(200).
           03  SPA-NAI-ENV         PIC X(200).
           03  WRK-PRT-TBL-KEY     PIC X(08).
           03  WRK-PRT-TBL-GROUP   PIC X(14).
           03  WRK-PRT-TBL-UUID    PIC X(36).
      *  01.01.02  ADD END
      *
         01  WRK-PARA.
           03  PARA-FILENM         PIC X(200).
           03  WRK-PARA-HOSPNUM    PIC X(02).
      *
      *    一時ディレクトリ名
           03  WRK-SGETTEMP-TEMPDIR
                                   PIC X(1024).
      *
       01  WRK-TBL-GROUP.
           03  WK-TBL-G           PIC  X(14) OCCURS 5.
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    印刷ＤＢ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    印刷管理ＤＢ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    プレビュー印刷ＤＢ
           COPY    "CPPREVIEW.INC".          
           COPY    "CPPRVKANRI.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ファイル取得関数
       01 ORCFILEGET-AREA.
           03  ORCF-FILE-MODE   PIC X(1).
           03  ORCF-FILE-NAME   PIC X(200).
           03  ORCF-DATA        PIC X(100000).
           03  ORCF-RET         PIC X(2).
       01  ST                PIC 9(2).
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER          PIC X(300).
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
      *    初期処理
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
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    PARA-FILENM
                                               WRK-PARA-HOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    指定ファイル読み込み処理
           INITIALIZE                  SGETTEMP-AREA
           MOVE    PARA-FILENM         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  PRT-FILE-ID
      *
           MOVE    SGETTEMP-TEMPDIR    TO  WRK-SGETTEMP-TEMPDIR
      *
           OPEN    INPUT               PRT-FILE
           IF      STS-PRT             NOT =   ZERO
               DISPLAY "PRT-FILE OPEN ERR=" STS-PRT 
               MOVE    1               TO  FLG-END 
               GO  TO  100-INIT-EXT
           END-IF
      *
           PERFORM 900-PRTFILE-READ-SEC
      *   レセ以外はここで中間ファイルを削除する
           MOVE    PRT-TBL-KEY         TO  WRK-PRT-TBL-KEY
           IF      PRT-TBL-KEY     NOT =   "RECEPT2"
                   PERFORM 900-PRV-RRKSET-SEC
           END-IF
      *
           IF  PRT-PRTKBN              =   "4" OR "5" OR "6"
               DISPLAY "For PREVIW SKIP" 
               MOVE    1               TO  FLG-END 
               GO  TO  100-INIT-EXT
           END-IF
      *
           IF  PRT-TBL-KEY             =   "RECEPT2"
           AND PRT-PRTKBN              =   "2"
               DISPLAY "For RECEIPT SKIP " PRT-PRTKBN 
               MOVE    1               TO  FLG-END 
               GO  TO  100-INIT-EXT
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ＰＳデータ読み込み
           MOVE    SPACE           TO  PS-FILE-ID
      * 
           STRING  PRT-OUT         DELIMITED  BY  SPACE
                   SPA-HOSPNUM     DELIMITED  BY  SPACE
                   PRT-TBL-KEY     DELIMITED  BY  SPACE
                   PRT-RENNUM      DELIMITED  BY  SPACE
                   PRT-TBL-GROUP   DELIMITED  BY  SPACE
                   PRT-SHORI-RENNUM
                                   DELIMITED  BY  SPACE
                   PRT-PAGE        DELIMITED  BY  SPACE
                   PRT-TBL-UUID    DELIMITED  BY  SPACE
                   ".pdf"          DELIMITED  BY  SIZE
                                   INTO   PS-FILE-ID
           END-STRING
      * 
           IF      PS-FILE-ID      =   SPACE
               DISPLAY "PS-FILE FILENM SPACE" 
               MOVE    1               TO  FLG-END 
               GO  TO  200-MAIN-EXT
           END-IF
      *          
           INITIALIZE                  PRTDATA-REC
           MOVE    PRT-RENNUM      TO  PRTDATA-RENNUM
           MOVE    PRT-SHORI-RENNUM
                                   TO  PRTDATA-SHORI-RENNUM
           MOVE    PRT-TBL-GROUP   TO  PRTDATA-TBL-GROUP
           MOVE    PRT-TBL-UUID    TO  PRTDATA-TBL-UUID
           MOVE    PRT-TBL-KEY     TO  PRTDATA-TBL-KEY
           MOVE    PRT-PAGE        TO  PRTDATA-PAGE
grpsys     MOVE    SPA-HOSPNUM     TO  PRTDATA-HOSPNUM
           PERFORM 900-PRTDATA-INV-SEC
           IF      FLG-PRTDATA     =   1     
               DISPLAY "PRTDATA NOT DATA"
               MOVE    1               TO  FLG-END 
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *+++++++++++++++++++++++++++++++++++++++++++++++++
           MOVE    SPACE               TO  PRVKANRI-REC
           INITIALIZE                      PRVKANRI-REC
           MOVE    PRTDATA-TBL-KEY     TO  PRVKANRI-TBL-KEY
                                           PRTKANRI-TBL-KEY 
           MOVE    PRTDATA-RENNUM      TO  PRVKANRI-RENNUM
                                           PRTKANRI-RENNUM
           MOVE    PRTDATA-TBL-GROUP   TO  PRVKANRI-TBL-GROUP
                                           PRTKANRI-TBL-GROUP
           MOVE    PRTDATA-TBL-UUID    TO  PRVKANRI-TBL-UUID
                                           PRTKANRI-TBL-UUID
           MOVE    PRTDATA-SHORI-RENNUM
                                       TO  PRVKANRI-SHORI-RENNUM
                                           PRTKANRI-SHORI-RENNUM
           MOVE    PRTDATA-SHELLID     TO  PRVKANRI-SHELLID
           MOVE    PRTDATA-PRTID       TO  PRVKANRI-PRTID
           MOVE    PRTDATA-SRYYM       TO  PRVKANRI-SRYYM
           MOVE    PRTDATA-SKYYMD      TO  PRVKANRI-SKYYMD
           MOVE    PRT-PSFILE-AREA     TO  PRVKANRI-FILENAME
           MOVE    PRTKANRI-TITLE      TO  PRVKANRI-TITLE
           MOVE    PRTDATA-PTID        TO  PRVKANRI-PTID
           MOVE    PRTDATA-PAGE        TO  PRVKANRI-PAGE
           MOVE    PRTDATA-PRTNM       TO  PRVKANRI-PRTNM
grpsys     MOVE    PRTDATA-HOSPNUM     TO  PRVKANRI-HOSPNUM
                                           PRTKANRI-HOSPNUM
      *
           PERFORM 900-PRVKANRI-INV-SEC
           IF      FLG-PRVKANRI        =   ZERO  
               CONTINUE                   
           ELSE
               PERFORM 900-PRTKANRI-INV-SEC
               IF    FLG-PRTKANRI        =   ZERO  
                     CONTINUE                   
               END-IF 
             MOVE    SPACE               TO  PRVKANRI-REC
             INITIALIZE                      PRVKANRI-REC
             MOVE    PRTDATA-TBL-KEY     TO  PRVKANRI-TBL-KEY
             MOVE    PRTDATA-RENNUM      TO  PRVKANRI-RENNUM
             MOVE    PRTDATA-TBL-GROUP   TO  PRVKANRI-TBL-GROUP
             MOVE    PRTDATA-TBL-UUID    TO  PRVKANRI-TBL-UUID
             MOVE    PRTDATA-SHORI-RENNUM
                                         TO  PRVKANRI-SHORI-RENNUM
             MOVE    PRTDATA-SHELLID     TO  PRVKANRI-SHELLID
             MOVE    PRTDATA-PRTID       TO  PRVKANRI-PRTID
             MOVE    PRTDATA-SRYYM       TO  PRVKANRI-SRYYM
             MOVE    PRTDATA-SKYYMD      TO  PRVKANRI-SKYYMD
             MOVE    PRTDATA-PTID        TO  PRVKANRI-PTID
             MOVE    PRTDATA-PAGE        TO  PRVKANRI-PAGE
             MOVE    PRT-PSFILE-AREA     TO  PRVKANRI-FILENAME
             MOVE    PRTDATA-PRTNM       TO  PRVKANRI-PRTNM
             MOVE    PRTKANRI-TITLE      TO  PRVKANRI-TITLE
      * 
             MOVE    2                   TO  IDZ
      * 
             MOVE    ZERO                TO  FLG-MATCH
             PERFORM VARYING   IDY     FROM    1   BY  1
                       UNTIL   IDY     >       5
                   IF    PRTDATA-TBL-GROUP 
                                         =
                                         PRVKANRI-TBL-GROUP-RRK(IDY)
                     MOVE    1           TO  FLG-MATCH
                     MOVE    5           TO  IDY
                   END-IF
             END-PERFORM
      * 
             IF      FLG-MATCH           =   ZERO
               MOVE    PRTDATA-TBL-GROUP   TO  
                                         PRVKANRI-TBL-GROUP-RRK(1)
               PERFORM VARYING   IDY     FROM    1   BY  1
                       UNTIL       IDY     >        4
                   MOVE  WK-TBL-G(IDY)   TO
                                         PRVKANRI-TBL-GROUP-RRK(IDZ)
                   COMPUTE     IDZ       =  IDZ  +  1
               END-PERFORM
             END-IF
      *
grpsys       MOVE    PRTDATA-HOSPNUM     TO  PRVKANRI-HOSPNUM
             MOVE    PRVKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_prvkanri"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys       PERFORM 900-ORCDBMAIN-SEC
           END-IF
      *
           MOVE    SPACE               TO  PREVIEW-REC
           INITIALIZE                      PREVIEW-REC
           MOVE    PRTDATA-TBL-KEY     TO  PREVIEW-TBL-KEY
           MOVE    PRTDATA-RENNUM      TO  PREVIEW-RENNUM
           MOVE    PRTDATA-TBL-GROUP   TO  PREVIEW-TBL-GROUP
           MOVE    PRTDATA-TBL-UUID    TO  PREVIEW-TBL-UUID
           MOVE    PRTDATA-SHORI-RENNUM
                                       TO  PREVIEW-SHORI-RENNUM
           MOVE    PRTDATA-SHELLID     TO  PREVIEW-SHELLID
           MOVE    PRTDATA-PRTID       TO  PREVIEW-PRTID
           MOVE    PRTDATA-SRYYM       TO  PREVIEW-SRYYM
           MOVE    PRTDATA-SKYYMD      TO  PREVIEW-SKYYMD
           MOVE    PRTDATA-PTID        TO  PREVIEW-PTID
           MOVE    PRTDATA-PAGE        TO  PREVIEW-PAGE
           MOVE    PRTDATA-PRTNM       TO  PREVIEW-PRTNM
           MOVE    PS-PRTDATA          TO  PREVIEW-PRTDATA
grpsys     MOVE    PRTDATA-HOSPNUM     TO  PREVIEW-HOSPNUM
      *
           PERFORM 900-PREVDATA-INV-SEC
           IF      FLG-PREVIEW         =   ZERO  
               CONTINUE                   
           ELSE
      *        IF      PRTDATA-TBL-KEY NOT =  "ONLINE"
      *          PERFORM 900-PSFILE-READ-SEC
      *          IF      ORCF-RET        NOT =   ZERO
      *            DISPLAY "PS-FILE OPEN ERR=" STS-PS 
      * Why is this line ?
      *            MOVE    1               TO  FLG-END 
      *            GO  TO  200-MAIN-EXT
      *          END-IF
      *        END-IF
      *        IF      FLG-PSFILE      =   1     
      *            DISPLAY "PS-FILE NOT DATA"
      *            MOVE    1               TO  FLG-END 
      *            GO  TO  200-MAIN-EXT
      *        END-IF
      *
      *
               MOVE    SPACE               TO  PREVIEW-REC
               INITIALIZE                      PREVIEW-REC
               MOVE    PRTDATA-TBL-KEY     TO  PREVIEW-TBL-KEY
               MOVE    PRTDATA-RENNUM      TO  PREVIEW-RENNUM
               MOVE    PRTDATA-TBL-GROUP   TO  PREVIEW-TBL-GROUP
               MOVE    PRTDATA-TBL-UUID    TO  PREVIEW-TBL-UUID
               MOVE    PRTDATA-SHORI-RENNUM
                                           TO  PREVIEW-SHORI-RENNUM
               MOVE    PRTDATA-SHELLID     TO  PREVIEW-SHELLID
               MOVE    PRTDATA-PRTID       TO  PREVIEW-PRTID
               MOVE    PRTDATA-SRYYM       TO  PREVIEW-SRYYM
               MOVE    PRTDATA-SKYYMD      TO  PREVIEW-SKYYMD
               MOVE    PRTDATA-PTID        TO  PREVIEW-PTID
               MOVE    PRTDATA-PAGE        TO  PREVIEW-PAGE
               MOVE    PRTDATA-PRTNM       TO  PREVIEW-PRTNM
               MOVE    PS-PRTDATA          TO  PREVIEW-PRTDATA
grpsys         MOVE    PRTDATA-HOSPNUM     TO  PREVIEW-HOSPNUM
               MOVE    PREVIEW-REC         TO  MCPDATA-REC
      *     ラージオブジェクト対応までコメント★
      *        MOVE    PATH-TBL-PREVIEW-KEY
      *                                    TO  MCP-PATH
      *        MOVE    "DBINSERT"          TO  MCP-FUNC
      *        CALL    "MCPSUB"            USING
      *                                    MCPAREA
      *                                    MCPDATA-REC
      * end ラージオブジェクト対応までコメント★
      *
               ADD     1                   TO  CNT-OUT
                                               CNT-OUT-COMMIT
      *
               MOVE    ZERO                TO  IF-RESULT
               MOVE    PS-FILE-ID          TO  IF-FILENAME
      *     ラージオブジェクト対応までコメント★
      * ★      CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *
               IF      CNT-OUT-COMMIT      =   10
                   MOVE    ZERO                TO  CNT-OUT-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
           END-IF
      *
      *
           PERFORM 900-PRTFILE-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "tbl-key = "     WRK-PRT-TBL-KEY
           IF      WRK-PRT-TBL-KEY     NOT =   "RECEPT2"
                   PERFORM 900-PRV-RRKDEL-SEC
           END-IF
      *
           CLOSE   PRT-FILE
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBPREV IN  "  CNT-IN
           DISPLAY "*** ORCBPREV OUT "  CNT-OUT
           DISPLAY "*** ORCBPREV END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *   プレビュー管理DBの履歴削除
      *****************************************************************
       900-PRV-RRKDEL-SEC              SECTION.
      *
      *        最新以外の帳票管理ＤＢを削除
           IF      WRK-PRT-TBL-KEY     =   "RECEPTX"
           OR      WRK-PRT-TBL-KEY     =   "ONLINE"
                   GO    TO            900-PRV-RRKDEL-EXT
           END-IF
           MOVE    SPACE               TO  PRVKANRI-REC
           INITIALIZE                  PRVKANRI-REC
           MOVE    WRK-PRT-TBL-KEY     TO  PRVKANRI-TBL-KEY
           MOVE    WRK-PRT-TBL-GROUP   TO  PRVKANRI-TBL-GROUP
           MOVE    WRK-PRT-TBL-UUID    TO  PRVKANRI-TBL-UUID
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
      *    印刷データ読込
      *****************************************************************
       900-PRTFILE-READ-SEC             SECTION.
      *
           READ    PRT-FILE
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   MOVE    ZERO        TO  FLG-END
                   ADD     1           TO  CNT-IN
           END-READ
           .
       900-PRTFILE-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   プレビュー管理DBの履歴取得 
      *****************************************************************
       900-PRV-RRKSET-SEC              SECTION.
      *
           MOVE    PRT-TBL-GROUP       TO  PRVKANRI-TBL-GROUP
                                           WRK-PRT-TBL-GROUP
           MOVE    PRT-TBL-UUID        TO  PRVKANRI-TBL-UUID
                                           WRK-PRT-TBL-UUID
           MOVE    PRT-TBL-KEY         TO  PRVKANRI-TBL-KEY
                                           WRK-PRT-TBL-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
      *    TBLキーセット
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               MOVE    SPACE           TO  WRK-TBL-GROUP
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PRVKANRI
                   MOVE    MCPDATA-REC     TO  PRVKANRI-REC
                   MOVE    PRVKANRI-T-G    TO  WRK-TBL-GROUP
               ELSE
                   MOVE    1               TO  FLG-PRVKANRI
                   MOVE    SPACE           TO  WRK-TBL-GROUP
                   INITIALIZE              WRK-TBL-GROUP
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PRV-RRKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷データ読込
      *****************************************************************
       900-PSFILE-READ-SEC             SECTION.
      *
      *    READ    PS-FILE
      *        AT  END
      *            MOVE    1           TO  FLG-PSFILE
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PSFILE
      *    END-READ
           MOVE    SPACE               TO  ORCF-DATA
           MOVE    PS-FILE-ID          TO  ORCF-FILE-NAME
           MOVE    "r"                 TO  ORCF-FILE-MODE
           MOVE    SPACE               TO  PS-FILE-DATA  
           CALL    "orcfileio"     USING ST  ORCFILEGET-AREA
           IF      ORCF-RET        NOT =   ZERO
                   DISPLAY "FGET FILE OPEN ERROR (" ORCF-RET  ")"
                           "[" PS-FILE-ID "]"
           ELSE 
      *            DISPLAY "FGET FILE OPEN OK " ORCF-DATA
             MOVE  ORCF-DATA           TO  PS-FILE-DATA
             MOVE  ORCF-DATA           TO  PS-PRTDATA              
           END-IF
           .
       900-PSFILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTDATA-INV-SEC            SECTION.
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prtdata"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRTDATA-REC
                   MOVE    ZERO                TO  FLG-PRTDATA
               ELSE  
                   MOVE    1                   TO  FLG-PRTDATA
                   INITIALIZE                      PRTDATA-REC
               END-IF 
           ELSE 
               MOVE    1                   TO  FLG-PRTDATA
               INITIALIZE                      PRTDATA-REC
           END-IF
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PRTDATA-INV-EXT.
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
                   MOVE    ZERO                TO  FLG-PRVKANRI
               ELSE  
                   MOVE    1                   TO  FLG-PRVKANRI
               END-IF 
           ELSE 
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PREVDATA-INV-EXT.
           EXIT.
      *      
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTKANRI-INV-SEC            SECTION.
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prtkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PRTKANRI
                   MOVE    MCPDATA-REC         TO  PRTKANRI-REC
               ELSE  
                   MOVE    1                   TO  FLG-PRTKANRI
               END-IF 
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PRTKANRI-INV-EXT.
           EXIT.
      *      
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PREVDATA-INV-SEC            SECTION.
      *
           MOVE    PREVIEW-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *     ラージオブジェクト対応までコメント★
      *    MOVE    PATH-TBL-PREVIEW-KEY
      *                                TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC
           MOVE    1                   TO  MCP-RC
      * end ラージオブジェクト対応までコメント★
      *
           IF      MCP-RC             =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *        MOVE    PATH-TBL-PREVIEW-KEY
      *                                    TO  MCP-PATH
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PREVIEW
               ELSE  
                   MOVE    1                   TO  FLG-PREVIEW
               END-IF 
           ELSE 
               MOVE    1                   TO  FLG-PREVIEW
           END-IF
      *
      *    ラージオブジェクト対応までコメント★
      *    MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *    MOVE    PATH-TBL-PREVIEW-KEY
      *                                TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC
      * end ラージオブジェクト対応までコメント★
           .
       900-PREVDATA-INV-EXT.
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
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
