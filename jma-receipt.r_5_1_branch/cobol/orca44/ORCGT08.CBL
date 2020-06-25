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
       PROGRAM-ID.             ORCGT08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : オンライン請求−レセ電個別作成履歴
      *  管理者            : 
      *  08/09/29     NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    13/10/07  労災レセ電対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "T06SCR-SPA".
      *
           COPY    "T01COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ONRECE-K        PIC 9(01).
           03  FLG-ONRECE-B        PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).  
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
      * 
           03  WRK-T08-PARALST-ROW
                                   PIC S9(09). 
           03  WRK-SELNUM          PIC S9(05).
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZ9            PIC ZZZ9.
      *
       01  WRK-HEN-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG.
               05  WRK-HENYM       PIC X(06).
               05  WRK-HENYM-FIL   PIC X(03).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-CONST-AREA.
      *    最大行
           03  WRK-CONS-KANRILST-MAX
                                   PIC 9(04)   VALUE   100.
           03  WRK-CONS-RECELST-MAX
                                   PIC 9(04)   VALUE   1000.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  ONRECE-K-REC.
           COPY    "CPONRECE-KANRI.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA44SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
      *                                 
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T06
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
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
           END-EVALUATE.
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    初期画面セット
           PERFORM 300-SCREEN-SEC
           IF      FLG-END             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "T08    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-T08-AREA
      *
           INITIALIZE                  T08
           MOVE    SPA-GMN-T06-SRYYM-H
                                   TO  T08-SRYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE    "社保"          TO  T08-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
                   MOVE    "労災"          TO  T08-TEISYUTUSAKI
               ELSE
                   MOVE    "国保"          TO  T08-TEISYUTUSAKI
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  IDX
           INITIALIZE                  ONRECE-K-REC
           MOVE    SPA-HOSPNUM     TO  ONRECE-K-HOSPNUM
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-K-SRYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                   =   "T"
               MOVE    "1"             TO  ONRECE-K-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
                   MOVE    "3"             TO  ONRECE-K-TEISYUTUSAKI
               ELSE
                   MOVE    "2"             TO  ONRECE-K-TEISYUTUSAKI
               END-IF
           END-IF
      *
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-KANRI-SELECT-SEC
      *
           PERFORM         UNTIL   FLG-ONRECE-K    =   1
               ADD     1               TO  IDX
               MOVE    IDX             TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  T08-KANRI-TRENNUM (IDX)
               MOVE    ONRECE-K-SKYYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T08-KANRI-TSKYYMD (IDX)
               MOVE    ONRECE-K-SEND-TOTAL-CNT
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  T08-KANRI-TKENSU  (IDX)
               MOVE    ONRECE-K-CREYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T08-KANRI-TCREYMD (IDX)
      *
               MOVE    ONRECE-K-RENNUM TO  SPA-NAI-T08-TRENNUM (IDX)
      * 
               MOVE    IDX             TO  T08-KANRI-LIST-COUNT
      * 
               MOVE    "tbl_onrece_kanri"
                                       TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-KANRI-FETCH-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onrece_kanri"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      * 
           MOVE    IDX             TO  SPA-GMN-T08-MAX
      *
           MOVE    1               TO  SPA-NAI-T08-SELNUM
           PERFORM 3101-RECE-LIST-HENSYU-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成一覧明細編集処理
      *****************************************************************
       3101-RECE-LIST-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO            TO  IDX
      *
           INITIALIZE                  T08-SCROLLEDWINDOW1
      *
           INITIALIZE                  ONRECE-B-REC
           MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                      =   "T"
               MOVE    "1"                 TO  ONRECE-B-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                          =   "T"
                   MOVE    "3"                 TO  ONRECE-B-TEISYUTUSAKI
               ELSE
                   MOVE    "2"                 TO  ONRECE-B-TEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T08-TRENNUM (SPA-NAI-T08-SELNUM)
                                   TO  ONRECE-B-RENNUM
      *
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
           PERFORM         UNTIL   FLG-ONRECE-B    =   1
               ADD     1               TO  IDX
               MOVE    IDX             TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  T08-RECE-TRENNUM      (IDX)
               EVALUATE    ONRECE-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  T08-RECE-TNYUGAIKBN (IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  T08-RECE-TNYUGAIKBN (IDX)
               END-EVALUATE
               MOVE    ONRECE-B-PTNUM  TO  T08-RECE-TPTNUM       (IDX)
               MOVE    ONRECE-B-NAME   TO  T08-RECE-TNAME        (IDX)
      *
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    ONRECE-B-PTID   TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T08-RECE-TBIRTHDAY    (IDX)
               MOVE    ONRECE-B-SRYYM-B
                                       TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  T08-RECE-TSRYYM       (IDX)
      *
               EVALUATE    ONRECE-B-TEISYUTUSAKI
                   WHEN    "3"
                       EVALUATE    ONRECE-B-RECESYUBETU
                           WHEN    "7101"
                           WHEN    "7091"
                               MOVE    "傷病・業務"        TO
                                           T08-RECE-TRECESYUBETU (IDX)
                           WHEN    "7102"
                           WHEN    "7092"
                               MOVE    "傷病・通勤"        TO
                                           T08-RECE-TRECESYUBETU (IDX)
                           WHEN    "7071"
                           WHEN    "7081"
                               MOVE    "短期・業務"        TO
                                           T08-RECE-TRECESYUBETU (IDX)
                           WHEN    "7072"
                           WHEN    "7082"
                               MOVE    "短期・通勤"        TO
                                           T08-RECE-TRECESYUBETU (IDX)
                           WHEN    OTHER
                               MOVE    ONRECE-B-RECESYUBETU    TO
                                           T08-RECE-TRECESYUBETU (IDX)
                       END-EVALUATE
                   WHEN    OTHER
                       MOVE    ONRECE-B-RECESYUBETU
                                       TO  T08-RECE-TRECESYUBETU (IDX)
               END-EVALUATE
      *    
               IF     ONRECE-B-HKNJANUM
                                       NOT =   "00000000"
                   MOVE    ONRECE-B-HKNJANUM
                                       TO  T08-RECE-THKNJANUM    (IDX)
               END-IF
               STRING  ONRECE-B-KIGO   DELIMITED   BY  SPACE
                       "　"            DELIMITED   BY  SIZE
                       ONRECE-B-NUM    DELIMITED   BY  SPACE
                                       INTO    T08-RECE-TKIGO    (IDX)
               END-STRING
      *
               EVALUATE    ONRECE-B-HKNNUM
                   WHEN    "002"
                   WHEN    "031"  THRU  "034"
                       EVALUATE    ONRECE-B-HOJOKBN
                           WHEN    "1"
                           WHEN    "A"
                           WHEN    "D"
                           WHEN    "G"
                               MOVE    "職"   TO  T08-RECE-THOJOKBN(IDX)
                           WHEN    "2"
                           WHEN    "B"
                           WHEN    "E"
                           WHEN    "H"
                               MOVE    "下"   TO  T08-RECE-THOJOKBN(IDX)
                           WHEN    "3"
                           WHEN    "C"
                           WHEN    "F"
                           WHEN    "I"
                               MOVE    "通"   TO  T08-RECE-THOJOKBN(IDX)
                       END-EVALUATE
               END-EVALUATE
      * 
               MOVE    IDX             TO  T08-RECE-LIST-COUNT
      * 
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T08-MAX
               IF      IDX             =   SPA-NAI-T08-SELNUM
                   MOVE    "T"         TO  T08-KANRI-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  T08-KANRI-LIST-SELECT (IDX)
               END-IF
           END-PERFORM 
      *
           IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
               MOVE    "年金証書番号等"    TO  T08-RECE-LIST-HYODAI1
           ELSE
               MOVE    "記号・番号"        TO  T08-RECE-LIST-HYODAI1
           END-IF
           .
       3101-RECE-LIST-HENSYU-EXT.
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
      *    前回処理分表示
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-BACK-HENSYU-SEC
      *    次回処理分表示
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 450-NEXT-HENSYU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
      *
           MOVE    "T06"               TO  SPA-SAKIPG
           MOVE    "T08"               TO  SPA-MOTOPG
      *
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
      *    前回処理分表示処理
      *****************************************************************
       440-BACK-HENSYU-SEC             SECTION.
      *
           IF      SPA-NAI-T08-SELNUM  =  SPA-GMN-T08-MAX
               GO  TO  440-BACK-HENSYU-EXT
           END-IF
      *
           COMPUTE SPA-NAI-T08-SELNUM  =   SPA-NAI-T08-SELNUM  +   1
           PERFORM 3101-RECE-LIST-HENSYU-SEC
      *
          .
       440-BACK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    次回処理分表示処理
      *****************************************************************
       450-NEXT-HENSYU-SEC             SECTION.
      *
           IF      SPA-NAI-T08-SELNUM  =  1
               GO  TO  450-NEXT-HENSYU-EXT
           END-IF
      *
           COMPUTE SPA-NAI-T08-SELNUM  =   SPA-NAI-T08-SELNUM  -   1
           PERFORM 3101-RECE-LIST-HENSYU-SEC
      *
          .
       450-NEXT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO   WRK-HENYMDG
           ELSE
               IF      WRK-SYMD            =   ALL "9"
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
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               END-IF
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルＳＥＬＥＣＴ処理
      *****************************************************************
       900-ONRECE-KANRI-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-KANRI-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-ONRECE-K
               INITIALIZE                      ONRECE-K-REC
           END-IF
           .
       900-ONRECE-KANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルFETCH処理
      *****************************************************************
       900-ONRECE-KANRI-FETCH-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-ONRECE-K
               MOVE   MCPDATA-REC          TO  ONRECE-K-REC
           ELSE
               INITIALIZE                      ONRECE-K-REC
               MOVE   1                    TO  FLG-ONRECE-K
           END-IF
           .
       900-ONRECE-KANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルSELECT処理
      *****************************************************************
       900-ONRECE-BODY-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-BODY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルFETCH処理
      *****************************************************************
       900-ONRECE-BODY-FETCH-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  ONRECE-B-REC
               MOVE  ZERO                  TO  FLG-ONRECE-B
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-BODY-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
                INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
