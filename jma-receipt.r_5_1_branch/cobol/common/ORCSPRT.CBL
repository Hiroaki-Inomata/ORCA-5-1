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
       PROGRAM-ID.             ORCSPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 帳票管理
      *  コンポーネント名  : 帳票印刷管理・印刷ＤＢ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/16    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/10/10  サイト区分追加
      *  01.00.02    NACL-藤原    03/03/03  印刷済フラグ追加
      *  01.01.02    NACL-藤原    03/03/04  印刷ＤＢへのプリンタ名追加による
      *                                     修正
      *  01.03.01    NACL-藤原    03/07/07  プレビュー機能追加
      *                                     ★にて追加
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.07.01    NACL-藤原    13/09/20  第三者行為対応
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
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PRTKANRI            PIC 9(01).
           03  FLG-PRTDATA             PIC 9(01). 
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    印刷管理ＤＢ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷ＤＢ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    一時ディレクトリ取得サブ
           COPY    "CPORCSGETTEMP.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *     
      *****************************************************************
      *
       LINKAGE                 SECTION.
      *   印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE           DIVISION
               USING
               ORCSPRTAREA
               SPA-AREA
               .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    SPA-MWの値を取得
           INITIALIZE                  SGETTEMP-AREA
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE     SGETTEMP-MW        TO  SPA-MW
      *
           IF      SPRT-GYOUMU-CD  NOT NUMERIC
                   MOVE  99            TO  SPRT-GYOUMU-CD
           END-IF
      *    処理モードのチェック
           EVALUATE    SPRT-MODE
               WHEN    "INS"
                       PERFORM 110-INS-SEC
               WHEN    "DEL"
                       PERFORM 140-DEL-SEC
               WHEN    OTHER
                       MOVE  10            TO  SPRT-RETURN  
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    印刷ＤＢ追加処理
      *****************************************************************
       110-INS-SEC                 SECTION.
      *
           PERFORM 200-PRTKANRI-HENSYU-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 210-PRTDATA-HENSYU-SEC
           END-IF         
      *
           IF      MCP-RC          =   ZERO
               CONTINUE
           ELSE
               MOVE  MCP-RC            TO  SPRT-RETURN
           END-IF
      *
           .
       110-INS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理
      *****************************************************************
       140-DEL-SEC                 SECTION.
      *
      *
           .
       140-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    印刷管理ＤＢ更新処理
      *****************************************************************
       200-PRTKANRI-HENSYU-SEC         SECTION.
      *
      *
           MOVE    SPACE               TO  PRTKANRI-REC
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPRT-TBL-KEY        TO  PRTKANRI-TBL-KEY
           MOVE    SPRT-RENNUM         TO  PRTKANRI-RENNUM
           MOVE    SPRT-TBL-GROUP      TO  PRTKANRI-TBL-GROUP
           MOVE    SPRT-TBL-UUID       TO  PRTKANRI-TBL-UUID
           MOVE    SPRT-GYOUMU-CD      TO  PRTKANRI-GYOUMU-CD
           MOVE    SPRT-SHORI-RENNUM   TO  PRTKANRI-SHORI-RENNUM
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-PRTKANRI-INV-SEC
           IF      FLG-PRTKANRI        =  ZERO
               ADD     1                   TO  PRTKANRI-PAGE
           ELSE
               MOVE    SPACE               TO  PRTKANRI-REC
               INITIALIZE                      PRTKANRI-REC
               MOVE    SPRT-TBL-KEY        TO  PRTKANRI-TBL-KEY
               MOVE    SPRT-RENNUM         TO  PRTKANRI-RENNUM
               MOVE    SPRT-TBL-GROUP      TO  PRTKANRI-TBL-GROUP
               MOVE    SPRT-TBL-UUID       TO  PRTKANRI-TBL-UUID
               MOVE    SPRT-GYOUMU-CD      TO  PRTKANRI-GYOUMU-CD
               MOVE    SPRT-SHELLID        TO  PRTKANRI-SHELLID
               MOVE    SPRT-SHORI-RENNUM   TO  PRTKANRI-SHORI-RENNUM
grpsys         MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
               MOVE    SPRT-SKYYMD         TO  PRTKANRI-SKYYMD
      * ★ プレビュー対応
      *        MOVE    SPRT-SRYYM          TO  PRTKANRI-SRYYM
      *        MOVE    SPRT-PRIORITY       TO  PRTKANRI-PRIORITY
               EVALUATE    SPRT-PRIORITY
                   WHEN    11
                   WHEN    21
                   WHEN    31
                   WHEN    41
                   WHEN    51
                   WHEN    61
                   WHEN    71
                   WHEN    81
                   WHEN    91
                   WHEN    101
                           COMPUTE PRTKANRI-PRIORITY  =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    SPRT-SRYYM         TO  PRTKANRI-SRYYM
                   WHEN    12
                   WHEN    22
                   WHEN    32
                   WHEN    42
                   WHEN    52
                   WHEN    62
                   WHEN    72
                   WHEN    82
                   WHEN    92
                   WHEN    102
                           COMPUTE PRTKANRI-PRIORITY  =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    ZERO               TO  PRTKANRI-SRYYM
                   WHEN    OTHER
                           MOVE    SPRT-SRYYM         TO  PRTKANRI-SRYYM
                           MOVE    SPRT-PRIORITY      TO  
                                                      PRTKANRI-PRIORITY
               END-EVALUATE
      * ★ end
               MOVE    1                   TO  PRTKANRI-PAGE
               IF      SPRT-PRTFLG         =   "1" OR "D"
                   MOVE    SPRT-PRTFLG         TO  PRTKANRI-PRTFLG
               ELSE
                   MOVE    SPACE               TO  PRTKANRI-PRTFLG
               END-IF
               MOVE    SPRT-TITLE          TO  PRTKANRI-TITLE
               MOVE    SPRT-TERMID         TO  PRTKANRI-TERMID
               MOVE    SPRT-OPID           TO  PRTKANRI-OPID
               MOVE    SPRT-PRTNM          TO  PRTKANRI-PRTNM
               MOVE    SPRT-API-ID         TO  PRTKANRI-API-ID
           END-IF
      *
           MOVE    SPRT-TBL-GROUP(1:8) TO  PRTKANRI-CREYMD
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-PRTKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SPRT-RETURN
               ELSE
                   DISPLAY "PRTKANRI UPDATE ERR KEY = ["
                                      MCP-RC  " " PRTKANRI-KEY  "]"   
                   MOVE  MCP-RC            TO  SPRT-RETURN
               END-IF
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SPRT-RETURN
               ELSE
                   DISPLAY "PRTKANRI INSERT ERR KEY = ["
                                      MCP-RC  " " PRTKANRI-KEY  "]"   
                   MOVE  MCP-RC            TO  SPRT-RETURN
               END-IF
           END-IF    
      *
      *
           .
       200-PRTKANRI-HENSYU-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    印刷ＤＢ更新処理
      *****************************************************************
       210-PRTDATA-HENSYU-SEC          SECTION.
      * 
           MOVE    SPACE               TO  PRTDATA-REC
           INITIALIZE                      PRTDATA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    SPRT-TBL-KEY        TO  PRTDATA-TBL-KEY
           MOVE    SPRT-RENNUM         TO  PRTDATA-RENNUM
           MOVE    SPRT-TBL-GROUP      TO  PRTDATA-TBL-GROUP
           MOVE    SPRT-TBL-UUID       TO  PRTDATA-TBL-UUID
           MOVE    SPRT-GYOUMU-CD      TO  PRTDATA-GYOUMU-CD
           MOVE    SPRT-SHELLID        TO  PRTDATA-SHELLID
           MOVE    SPRT-PRTID          TO  PRTDATA-PRTID
           MOVE    SPRT-SHORI-RENNUM   TO  PRTDATA-SHORI-RENNUM
           MOVE    SPRT-SKYYMD         TO  PRTDATA-SKYYMD
           MOVE    SPRT-PTID           TO  PRTDATA-PTID 
      * ★ プレビュー対応
      *    MOVE    SPRT-SRYYM          TO  PRTDATA-SRYYM
      *    MOVE    SPRT-PRIORITY       TO  PRTDATA-PRIORITY
               EVALUATE    SPRT-PRIORITY
                   WHEN    11
                   WHEN    21
                   WHEN    31
                   WHEN    41
                   WHEN    51
                   WHEN    61
                   WHEN    71
                   WHEN    81
                   WHEN    91
                   WHEN    101
                           COMPUTE PRTDATA-PRIORITY   =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    SPRT-SRYYM         TO  PRTDATA-SRYYM
                   WHEN    12
                   WHEN    22
                   WHEN    32
                   WHEN    42
                   WHEN    52
                   WHEN    62
                   WHEN    72
                   WHEN    82
                   WHEN    92
                   WHEN    102
                           COMPUTE PRTDATA-PRIORITY   =   SPRT-PRIORITY 
                                                      /   10
                           MOVE    SPRT-SRYYM         TO  PRTDATA-SRYYM
                   WHEN    OTHER
                           MOVE    SPRT-SRYYM         TO  PRTDATA-SRYYM
                           MOVE    SPRT-PRIORITY      TO  
                                                      PRTDATA-PRIORITY
               END-EVALUATE
      * ★ end
           MOVE    PRTKANRI-PAGE       TO  PRTDATA-PAGE
           IF      SPRT-PRTFLG         =   "1" OR "D"
               MOVE    SPRT-PRTFLG         TO  PRTDATA-PRTFLG
           ELSE
               MOVE    SPACE               TO  PRTDATA-PRTFLG
           END-IF
           IF      SPRT-SITEKBN        =   SPACE
               MOVE    "0"                 TO  PRTDATA-SITEKBN
           ELSE    
               MOVE    SPRT-SITEKBN        TO  PRTDATA-SITEKBN
           END-IF    
           MOVE    SPRT-PRTNM          TO  PRTDATA-PRTNM
           MOVE    FUNCTION LOWER-CASE (SPRT-DATA-TYPE)
                                       TO  PRTDATA-DATA-TYPE
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
            AND  ( SPRT-SITEKBN        =   "2" )
               MOVE    "pdf"           TO  PRTDATA-DATA-TYPE
           END-IF
      *
           MOVE    SPRT-API-ID         TO  PRTDATA-API-ID
      *
           IF    ( PRTDATA-DATA-TYPE   =   "pdf" )
               CALL    "ORCSPDF"       USING
                                       ORCSPRTAREA
                                       SPA-AREA
               MOVE    SPRT-PDF        TO  PRTDATA-PRTDATA
           ELSE
               MOVE    SPRT-PRTDATA    TO  PRTDATA-PRTDATA
           END-IF
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *                                      
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SPRT-RETURN
           ELSE
               DISPLAY "PRTDATA INSERT ERR KEY = ["
                                  MCP-RC  " " PRTDATA-KEY  "]"   
               MOVE  MCP-RC            TO  SPRT-RETURN
           END-IF
      *
           .
       210-PRTDATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷管理マスタ読込
      *****************************************************************
       910-PRTKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prtkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRTKANRI-REC
                   MOVE    ZERO                TO  FLG-PRTKANRI
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
      *
           .
       910-PRTKANRI-INV-EXT.
           EXIT.
           .
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
