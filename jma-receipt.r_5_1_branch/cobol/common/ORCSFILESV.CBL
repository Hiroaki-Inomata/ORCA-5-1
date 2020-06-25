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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSFILESV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : クライアント保存
      *  コンポーネント名  : クライアント保存ＤＢ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/08/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-FILE-INFO           PIC 9(01). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-TBL-MAX        PIC 9(03)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    クライアント保存ＤＢ
       01  FILE-INFO-REC.
           COPY    "CPFILE-INFO.INC".          
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *     
      ******************************************************************
      *
       LINKAGE                 SECTION.
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "COMMON-SPA".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                               USING   ORCSFILESVAREA
                                       SPA-AREA
               .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           EVALUATE    SFILESV-MODE
               WHEN    "I"
                   PERFORM 110-INS-SEC
               WHEN    "D"
                   PERFORM 120-DEL-SEC
               WHEN    "R"
                   PERFORM 130-READ-SEC
               WHEN    "U"
                   PERFORM 140-UPDATE-SEC
               WHEN    OTHER
                   MOVE    99          TO  SFILESV-RETURN  
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       110-INS-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-TBL-MAX
                   OR      SFILESV-SHELLID (IDX)
                                       =   SPACE 
               MOVE    SPACE               TO  FILE-INFO-REC
               INITIALIZE                      FILE-INFO-REC
               MOVE    SPA-HOSPNUM         TO  FILE-INFO-HOSPNUM
               MOVE    SFILESV-TBL-KEY     TO  FILE-INFO-TBL-KEY
               MOVE    SFILESV-SHELLID      (IDX)
                                           TO  FILE-INFO-SHELLID
               MOVE    SFILESV-SHORI-RENNUM (IDX)
                                           TO  FILE-INFO-SHORI-RENNUM
               MOVE    SFILESV-RENNUM       (IDX)
                                           TO  FILE-INFO-RENNUM
               MOVE    SFILESV-SRYYM (IDX) TO  FILE-INFO-SRYYM
               MOVE    SFILESV-SKYYMD(IDX) TO  FILE-INFO-SKYYMD
               MOVE    SFILESV-TITLE (IDX) TO  FILE-INFO-TITLE
               MOVE    SFILESV-TEISYUTUSAKI (IDX)
                                           TO  FILE-INFO-TEISYUTUSAKI
               MOVE    SFILESV-NYUGAIKBN    (IDX)
                                           TO  FILE-INFO-NYUGAIKBN
               MOVE    SFILESV-CNT-ALL      (IDX)
                                           TO  FILE-INFO-CNT-ALL
               MOVE    SFILESV-FOR-FOLDER   (IDX)
                                           TO  FILE-INFO-FOR-FOLDER
               MOVE    SFILESV-FOR-DATA     (IDX)
                                           TO  FILE-INFO-FOR-DATA
               MOVE    SFILESV-TO-FOLDER    (IDX)
                                           TO  FILE-INFO-TO-FOLDER
               MOVE    SFILESV-TO-DATA      (IDX)
                                           TO  FILE-INFO-TO-DATA
               MOVE    SFILESV-CODE-TYPE    (IDX)
                                           TO  FILE-INFO-CODE-TYPE
               MOVE    SFILESV-DATA-TYPE    (IDX)
                                           TO  FILE-INFO-DATA-TYPE
               MOVE    SFILESV-ERR-CODE     (IDX)
                                           TO  FILE-INFO-ERR-CODE
               MOVE    SFILESV-ERR-MSG      (IDX)
                                           TO  FILE-INFO-ERR-MSG
               MOVE    SFILESV-SYOYMD       (IDX)
                                           TO  FILE-INFO-SYOYMD
               MOVE    SMCNDATE-YMD        TO  FILE-INFO-CREYMD
               MOVE    SMCNDATE-HMS        TO  FILE-INFO-CREHMS
      *
               MOVE    FILE-INFO-REC       TO  MCPDATA-REC
               MOVE    "tbl_file_info"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
      *                                      
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SFILESV-RETURN
               ELSE
                   DISPLAY "FILE-INFO INSERT ERR KEY = ["
                                  MCP-RC  " " FILE-INFO-KEY  "]"   
                   MOVE    MCP-RC            TO  SFILESV-RETURN
                   MOVE    WRK-CONS-TBL-MAX  TO  IDX
               END-IF
           END-PERFORM    
      *
           .
       110-INS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理
      *****************************************************************
       120-DEL-SEC                 SECTION.
      *
           MOVE    SPACE               TO  FILE-INFO-REC
           INITIALIZE                      FILE-INFO-REC
           MOVE    SPA-HOSPNUM         TO  FILE-INFO-HOSPNUM
           MOVE    SFILESV-TBL-KEY     TO  FILE-INFO-TBL-KEY
           MOVE    SFILESV-SHELLID (1) TO  FILE-INFO-SHELLID
           MOVE    SFILESV-SRYYM   (1) TO  FILE-INFO-SRYYM
      *
           MOVE    FILE-INFO-REC       TO  MCPDATA-REC
           MOVE    "tbl_file_info"     TO  MCP-TABLE
           IF    ( SFILESV-SHELLID (1) =   SPACE )
           AND   ( SFILESV-SRYYM   (1) =   SPACE )
               MOVE    "del1"              TO  MCP-PATHNAME
           ELSE
               IF      SFILESV-SHELLID (1) NOT =   SPACE
                   MOVE    "del3"              TO  MCP-PATHNAME
               ELSE
                   MOVE    "del2"              TO  MCP-PATHNAME
               END-IF
           END-IF
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SFILESV-RETURN
           ELSE
               DISPLAY "FILE-INFO DELETE ERR KEY = [" MCP-RC
                                          " " FILE-INFO-TBL-KEY  "]"   
               MOVE  MCP-RC            TO  SFILESV-RETURN
           END-IF
      *
           .
       120-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    読み込み処理
      *****************************************************************
       130-READ-SEC                 SECTION.
      *
           MOVE    SPACE               TO  FILE-INFO-REC
           INITIALIZE                      FILE-INFO-REC
           MOVE    SPA-HOSPNUM         TO  FILE-INFO-HOSPNUM
           MOVE    SFILESV-TBL-KEY     TO  FILE-INFO-TBL-KEY
           MOVE    SFILESV-SRYYM   (1) TO  FILE-INFO-SRYYM
           MOVE    SFILESV-SHELLID (1) TO  FILE-INFO-SHELLID 
      *
           MOVE    FILE-INFO-REC       TO  MCPDATA-REC
           IF      SFILESV-SRYYM (1)   =   SPACE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           ELSE
               IF      SFILESV-SHELLID (1) =   SPACE
                   MOVE    "key3"              TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE    "key4"              TO  WRK-MCP-PATHNAME
               END-IF
           END-IF
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "tbl_file_info"     TO  MCP-TABLE
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_info"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-FILE-INFO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILE-INFO
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *   
           PERFORM UNTIL   FLG-FILE-INFO    =   1
                   OR      IDX             >=   WRK-CONS-TBL-MAX
               ADD     1                   TO  IDX
               MOVE    FILE-INFO-SHELLID   TO  SFILESV-SHELLID     (IDX)
               MOVE    FILE-INFO-SHORI-RENNUM
                                           TO  SFILESV-SHORI-RENNUM(IDX)
               MOVE    FILE-INFO-RENNUM    TO  SFILESV-RENNUM      (IDX)
               MOVE    FILE-INFO-SRYYM     TO  SFILESV-SRYYM       (IDX)
               MOVE    FILE-INFO-SKYYMD    TO  SFILESV-SKYYMD      (IDX)
               MOVE    FILE-INFO-TITLE     TO  SFILESV-TITLE       (IDX)
               MOVE    FILE-INFO-TEISYUTUSAKI
                                           TO  SFILESV-TEISYUTUSAKI(IDX)
               MOVE    FILE-INFO-NYUGAIKBN TO  SFILESV-NYUGAIKBN   (IDX)
               MOVE    FILE-INFO-CNT-ALL   TO  SFILESV-CNT-ALL     (IDX)
               MOVE    FILE-INFO-FOR-FOLDER
                                           TO  SFILESV-FOR-FOLDER  (IDX)
               MOVE    FILE-INFO-FOR-DATA  TO  SFILESV-FOR-DATA    (IDX)
               MOVE    FILE-INFO-TO-FOLDER TO  SFILESV-TO-FOLDER   (IDX)
               MOVE    FILE-INFO-TO-DATA   TO  SFILESV-TO-DATA     (IDX)
               MOVE    FILE-INFO-CODE-TYPE TO  SFILESV-CODE-TYPE   (IDX)
               MOVE    FILE-INFO-DATA-TYPE TO  SFILESV-DATA-TYPE   (IDX)
               MOVE    FILE-INFO-ERR-CODE  TO  SFILESV-ERR-CODE    (IDX)
               MOVE    FILE-INFO-ERR-MSG   TO  SFILESV-ERR-MSG     (IDX)
               MOVE    FILE-INFO-SYOYMD    TO  SFILESV-SYOYMD      (IDX)
               MOVE    FILE-INFO-CREYMD    TO  SFILESV-CREYMD      (IDX)
               MOVE    FILE-INFO-CREHMS    TO  SFILESV-CREHMS      (IDX)
      *         
               MOVE    "tbl_file_info"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-FILE-INFO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_file_info"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    1                   TO  SFILESV-RETURN
           END-IF
      *
           .
       130-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理（処理年月日）
      *****************************************************************
       140-UPDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  FILE-INFO-REC
           INITIALIZE                      FILE-INFO-REC
           MOVE    SPA-HOSPNUM         TO  FILE-INFO-HOSPNUM
           MOVE    SFILESV-TBL-KEY     TO  FILE-INFO-TBL-KEY
           MOVE    SFILESV-SRYYM   (1) TO  FILE-INFO-SRYYM
           MOVE    SFILESV-SHELLID (1) TO  FILE-INFO-SHELLID
           MOVE    SFILESV-SHORI-RENNUM (1)
                                       TO  FILE-INFO-SHORI-RENNUM
           MOVE    SFILESV-RENNUM  (1) TO  FILE-INFO-RENNUM
      *
           MOVE    FILE-INFO-REC       TO  MCPDATA-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "tbl_file_info"     TO  MCP-TABLE
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_info"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-FILE-INFO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILE-INFO
           END-IF
      *
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "tbl_file_info"     TO  MCP-TABLE
           PERFORM 900-CLOSE-SEC
      *
      *    処理年月日の更新
           IF      FLG-FILE-INFO       =   ZERO
               MOVE    SMCNDATE-YMD        TO  FILE-INFO-SYOYMD
      *
               MOVE    FILE-INFO-REC       TO  MCPDATA-REC
               MOVE    "tbl_file_info"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
      *                                      
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SFILESV-RETURN
               ELSE
                   DISPLAY "FILE-INFO UPDATE ERR KEY = ["
                                  MCP-RC  " " FILE-INFO-KEY  "]"   
                   MOVE    MCP-RC            TO  SFILESV-RETURN
               END-IF
           ELSE
               DISPLAY "FILE-INFO UPDATE ERR KEY = ["
                                  MCP-RC  " " FILE-INFO-KEY  "]"   
               MOVE    MCP-RC            TO  SFILESV-RETURN
           END-IF    
      *
           .
       140-UPDATE-EXT.
           EXIT.
      * 
      *
      *****************************************************************
      *    マスタ読込
      *****************************************************************
       900-FILE-INFO-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  FILE-INFO-REC
               MOVE    ZERO                TO  FLG-FILE-INFO
           ELSE
               MOVE    1                   TO  FLG-FILE-INFO
           END-IF
      *
           .
       900-FILE-INFO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
