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
       PROGRAM-ID.             ORCSFILEMISSV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療情報システム
      *  コンポーネント名  : ファイル情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/02/17    NACL−伊藤     新規作成
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
           03  FLG-FILE-MIS            PIC 9(01). 
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
       01  FILE-MIS-REC.
           COPY    "CPFILE-MIS.INC".          
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
           COPY    "CPORCSFILEMISSV.INC".
      *
           COPY    "COMMON-SPA".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                               USING   ORCSFILEMISSVAREA
                                       SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           EVALUATE    FLMISSV-MODE
               WHEN    "I"
                   PERFORM 110-INS-SEC
               WHEN    "D"
                   PERFORM 120-DEL-SEC
               WHEN    "R"
                   PERFORM 130-READ-SEC
               WHEN    "U"
                   PERFORM 140-UPDATE-SEC
               WHEN    OTHER
                   MOVE    99          TO  FLMISSV-RETURN  
           END-EVALUATE
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
                   OR      FLMISSV-SHELLID (IDX)
                                       =   SPACE 
               INITIALIZE                      FILE-MIS-REC
               MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
               MOVE    FLMISSV-TBL-KEY     TO  FILE-MIS-TBL-KEY
               MOVE    FLMISSV-RENNUM(IDX) TO  FILE-MIS-RENNUM
               MOVE    FLMISSV-TBL-GROUP(IDX)
                                           TO  FILE-MIS-TBL-GROUP
               MOVE    FLMISSV-SHORI-RENNUM(IDX)
                                           TO  FILE-MIS-SHORI-RENNUM
               MOVE    FLMISSV-SHELLID(IDX)
                                           TO  FILE-MIS-SHELLID
               MOVE    FLMISSV-SRYYM(IDX)  TO  FILE-MIS-SRYYM
               MOVE    FLMISSV-SRYYMD(IDX) TO  FILE-MIS-SRYYMD
               MOVE    FLMISSV-CNT(IDX)    TO  FILE-MIS-CNT
               MOVE    FLMISSV-NYUGAIKBN(IDX)
                                           TO  FILE-MIS-NYUGAIKBN
               MOVE    FLMISSV-TO-FOLDER(IDX)
                                           TO  FILE-MIS-TO-FOLDER
               MOVE    FLMISSV-TO-DATA(IDX)
                                           TO  FILE-MIS-TO-DATA
               MOVE    FLMISSV-CODE-TYPE(IDX)
                                           TO  FILE-MIS-CODE-TYPE
               MOVE    FLMISSV-PUTFLG(IDX) TO  FILE-MIS-PUTFLG
               MOVE    FLMISSV-TITLE(IDX)  TO  FILE-MIS-TITLE
               MOVE    FLMISSV-TBL-UUID(IDX)
                                           TO  FILE-MIS-TBL-UUID
               MOVE    SMCNDATE-YMD        TO  FILE-MIS-CREYMD
               MOVE    SMCNDATE-YMD        TO  FILE-MIS-UPYMD
               MOVE    SMCNDATE-HMS        TO  FILE-MIS-UPHMS
      *
               MOVE    FILE-MIS-REC        TO  MCPDATA-REC
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
      *                                      
               IF      MCP-RC     =   ZERO
                   MOVE    MCP-RC              TO  FLMISSV-RETURN
               ELSE
                   DISPLAY "FILE-MIS INSERT ERR KEY = ["
                                  MCP-RC  " " FILE-MIS-KEY "]"   
                   MOVE    MCP-RC            TO  FLMISSV-RETURN
                   MOVE    WRK-CONS-TBL-MAX  TO  IDX
               END-IF
           END-PERFORM    
           .
       110-INS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理（未使用のため未検証）
      *****************************************************************
       120-DEL-SEC                 SECTION.
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    FLMISSV-TBL-KEY     TO  FILE-MIS-TBL-KEY
           MOVE    FLMISSV-RENNUM(1)   TO  FILE-MIS-RENNUM
           MOVE    FLMISSV-TBL-GROUP(1)
                                       TO  FILE-MIS-TBL-GROUP
           MOVE    FLMISSV-SHORI-RENNUM(1)
                                       TO  FILE-MIS-SHORI-RENNUM
      *
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "del_monblob"       TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *                                      
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC              =   ZERO
               MOVE    MCP-RC              TO  FLMISSV-RETURN
           ELSE
               DISPLAY "FILE-MIS DELETE ERR KEY = [" MCP-RC
                                       " " FILE-MIS-TBL-KEY  "]"   
               MOVE    MCP-RC              TO  FLMISSV-RETURN
           END-IF
           .
       120-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    読み込み処理（未使用のため未検証）
      *****************************************************************
       130-READ-SEC                SECTION.
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    FLMISSV-TBL-KEY     TO  FILE-MIS-TBL-KEY
           MOVE    FLMISSV-RENNUM(1)   TO  FILE-MIS-RENNUM
           MOVE    FLMISSV-TBL-GROUP(1)
                                       TO  FILE-MIS-TBL-GROUP
           MOVE    FLMISSV-SHORI-RENNUM(1)
                                       TO  FILE-MIS-SHORI-RENNUM
      *
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILE-MIS
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *   
           PERFORM UNTIL   FLG-FILE-MIS    =   1
                   OR      IDX             >=  WRK-CONS-TBL-MAX
               ADD     1                   TO  IDX
               MOVE    FILE-MIS-RENNUM     TO  FLMISSV-RENNUM      (IDX)
               MOVE    FILE-MIS-TBL-GROUP  TO  FLMISSV-TBL-GROUP   (IDX)
               MOVE    FILE-MIS-SHORI-RENNUM
                                           TO  FLMISSV-SHORI-RENNUM(IDX)
               MOVE    FILE-MIS-SHELLID    TO  FLMISSV-SHELLID     (IDX)
               MOVE    FILE-MIS-SRYYM      TO  FLMISSV-SRYYM       (IDX)
               MOVE    FILE-MIS-SRYYMD     TO  FLMISSV-SRYYMD      (IDX)
               MOVE    FILE-MIS-CNT        TO  FLMISSV-CNT         (IDX)
               MOVE    FILE-MIS-NYUGAIKBN  TO  FLMISSV-NYUGAIKBN   (IDX)
               MOVE    FILE-MIS-TO-FOLDER  TO  FLMISSV-TO-FOLDER   (IDX)
               MOVE    FILE-MIS-TO-DATA    TO  FLMISSV-TO-DATA     (IDX)
               MOVE    FILE-MIS-CODE-TYPE  TO  FLMISSV-CODE-TYPE   (IDX)
               MOVE    FILE-MIS-PUTFLG     TO  FLMISSV-PUTFLG      (IDX)
               MOVE    FILE-MIS-TITLE      TO  FLMISSV-TITLE       (IDX)
               MOVE    FILE-MIS-TBL-UUID   TO  FLMISSV-TBL-UUID    (IDX)
               MOVE    FILE-MIS-CREYMD     TO  FLMISSV-CREYMD      (IDX)
               MOVE    FILE-MIS-UPYMD      TO  FLMISSV-UPYMD       (IDX)
               MOVE    FILE-MIS-UPHMS      TO  FLMISSV-UPHMS       (IDX)
      *         
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    1                   TO  FLMISSV-RETURN
           END-IF
           .
       130-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理（更新年月日）（未使用のため未検証）
      *****************************************************************
       140-UPDATE-SEC              SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           INITIALIZE                      FILE-MIS-REC
           MOVE    SPA-HOSPNUM         TO  FILE-MIS-HOSPNUM
           MOVE    FLMISSV-TBL-KEY     TO  FILE-MIS-TBL-KEY
           MOVE    FLMISSV-RENNUM(1)   TO  FILE-MIS-RENNUM
           MOVE    FLMISSV-TBL-GROUP(1)
                                       TO  FILE-MIS-TBL-GROUP
           MOVE    FLMISSV-SHORI-RENNUM(1)
                                       TO  FILE-MIS-SHORI-RENNUM
      *
           MOVE    FILE-MIS-REC        TO  MCPDATA-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-FILE-MIS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-FILE-MIS
           END-IF
      *
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "tbl_file_mis"      TO  MCP-TABLE
           PERFORM 900-CLOSE-SEC
      *
      *    処理年月日の更新
           IF      FLG-FILE-MIS        =   ZERO
               MOVE    SMCNDATE-YMD        TO  FILE-MIS-UPYMD
               MOVE    SMCNDATE-HMS        TO  FILE-MIS-UPHMS
      *
               MOVE    FILE-MIS-REC        TO  MCPDATA-REC
               MOVE    "tbl_file_mis"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
      *                                      
               IF      MCP-RC     =   ZERO
                   MOVE    MCP-RC              TO  FLMISSV-RETURN
               ELSE
                   DISPLAY "FILE-MIS UPDATE ERR KEY = ["
                                  MCP-RC  " " FILE-MIS-KEY  "]"   
                   MOVE    MCP-RC              TO  FLMISSV-RETURN
               END-IF
           ELSE
               DISPLAY "FILE-MIS UPDATE ERR KEY = ["
                                  MCP-RC  " " FILE-MIS-KEY  "]"   
               MOVE    MCP-RC              TO  FLMISSV-RETURN
           END-IF    
           .
       140-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ読込
      *****************************************************************
       900-FILE-MIS-READ-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  FILE-MIS-REC
               MOVE    ZERO                TO  FLG-FILE-MIS
           ELSE
               INITIALIZE                      FILE-MIS-REC
               MOVE    1                   TO  FLG-FILE-MIS
           END-IF
           .
       900-FILE-MIS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
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
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
