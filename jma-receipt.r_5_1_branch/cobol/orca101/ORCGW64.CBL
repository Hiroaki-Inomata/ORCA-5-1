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
       PROGRAM-ID.                 ORCGW64.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 公費番号まとめ情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/10/24    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    09/07/21  表示名称の追加
      *
      *  04.08.01    NACL-藤原    16/09/13  公費番号の追加
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
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W64.
           03  SPA-W64-AREA.
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-OCC         OCCURS  100.
                           11  SPA-GMN-TRENNUM     PIC 9(03).
                           11  SPA-GMN-TMEISYO     PIC X(44).
                           11  SPA-GMN-TSTYUKYMD   PIC X(09).
                           11  SPA-GMN-TEDYUKYMD   PIC X(09).  
                           11  SPA-GMN-TKOHNUM     PIC X(03)
                                                       OCCURS  20.
      *
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SELNUM          PIC 9(03).
                       09  SPA-GMN-MEISYO          PIC X(44).
                       09  SPA-GMN-MEISYO2         PIC X(44).
                       09  SPA-GMN-STYUKYMD        PIC X(10).
                       09  SPA-GMN-EDYUKYMD        PIC X(10).
                       09  SPA-GMN-KOH-KOUMOKU. 
                           11  SPA-GMN-KOH-OCC     OCCURS  20.
                               13  SPA-GMN-KOHNUM  PIC X(03).
                               13  SPA-GMN-SEIDONAME
                                                   PIC X(50).
      *  
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-KOUMOKU.
                       09  SPA-NAI-KBNCD           PIC X(03).
                       09  SPA-NAI-STYUKYMD        PIC 9(08).
                       09  SPA-NAI-EDYUKYMD        PIC 9(08).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBL-OCC         OCCURS  100.
                           11  SPA-NAI-TSTYUKYMD   PIC 9(08).
                           11  SPA-NAI-TEDYUKYMD   PIC 9(08).  
                           11  SPA-NAI-TKBNCD      PIC X(03).
      *
              05  SPA-NAI-SELNUM                   PIC 9(03).
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-HKNNUM                          PIC 9(01).  
      *
           03  FLG-ROW                             PIC 9(01).
           03  FLG-CHK                             PIC 9(01).
           03  FLG-TOUROKU                         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                 PIC 9(05).
           03  IDY                                 PIC 9(05).
           03  IDZ                                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-SELNUM                          PIC 9(03). 
      *
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
      *
           03  WRK-W64-ROW                         PIC S9(09).
      *
           03  WRK-2007-KBNCD.
               05  WRK-2007-KBNCD-9                PIC 9(03).
      *
           03  WRK-SELNUM-X.
               05  WRK-SELNUM-Z                    PIC ZZ9.
      *  
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-GMN-CUR                         PIC 9(03).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  WRK-CONST-AREA.
           03  WRK-KOHNUM-MAX          PIC 9(02)   VALUE   20.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    公費番号まとめ情報
           COPY  "CPSK2007.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W64
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
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W64         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W64    "           TO  MCP-WINDOW
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
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 320-WID1-SET-SEC
                   GO  TO      300-SCREEN-EXT
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
           INITIALIZE                  SPA-W64
      *
      *    公費番号まとめ情報
           PERFORM 3101-SPA-HENSYU-SEC
      *
           MOVE    1               TO  FLG-ROW
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト特記事項編集情報処理
      *****************************************************************
       3101-SPA-HENSYU-SEC         SECTION.
      *
           INITIALIZE              SPA-GMN-TBL
                                   SPA-NAI-TBL
                                   SPA-GMN-MAX
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    "2007"          TO  SYS-KANRICD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key5"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX            >=  100
               MOVE    SYSKANRI-REC        TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  WRK-SELNUM-Z
               MOVE    WRK-SELNUM-X        TO  SPA-GMN-TRENNUM   (IDX)
               MOVE    SYS-2007-KBNCD      TO  SPA-NAI-TKBNCD    (IDX)
               MOVE    SYS-2007-STYUKYMD   TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    SYS-2007-STYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    SYS-2007-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    SYS-2007-EDYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD (IDX)
      *
               MOVE    SYS-2007-MEISYO (1) TO  SPA-GMN-TMEISYO   (IDX)
               MOVE    ZERO                TO  IDZ 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      SYS-2007-KOHNUM (IDY)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM (IDY)
                                           TO SPA-GMN-TKOHNUM (IDX IDZ)
               END-PERFORM
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      SYS-2007-KOHNUM1(IDY)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM1(IDY)
                                           TO SPA-GMN-TKOHNUM (IDX IDZ)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key5"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-MAX
      *
          MOVE    1                TO  SPA-GMN-CUR
 
           .
       3101-SPA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       320-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "2002"
                       PERFORM 421-TOUROKU-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
      *
           .
       320-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM  210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-SAKUJO-GID-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 420-TOUROKU-GID-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W64"               TO  SPA-MOTOPG
           MOVE    "W01"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 400-SELNUM-CHK-SEC
      *    一覧
           WHEN    ANY             ALSO    "KOHNUMLST"
                   PERFORM 400-KOHNUMLST-CHK-SEC
      *
           WHEN    OTHER
                   PERFORM 410-ENTRY-SYORI-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       410-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       410-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W64-MEISYO          TO  SPA-GMN-MEISYO
           MOVE    W64-MEISYO2         TO  SPA-GMN-MEISYO2
           MOVE    W64-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
           MOVE    W64-STYUKYMD        TO  SPA-GMN-STYUKYMD
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-KOHNUM-MAX
               MOVE    W64-KOHNUM (IDX)    TO  SPA-GMN-KOHNUM (IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    有効開始年月日
           PERFORM 41021-STYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効終了年月日
           PERFORM 41021-EDYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    名称
           PERFORM 41021-MEISYO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    名称２
           PERFORM 41021-MEISYO2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    公費番号
           PERFORM 41021-KOHNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       400-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *     
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-NAI-SELNUM
      *
           MOVE    W64-SELNUM      TO  SPA-GMN-SELNUM
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    2                   TO  SPA-GMN-CUR 
               GO  TO  400-SELNUM-CHK-EXT 
           ELSE
               IF    ( SPA-GMN-SELNUM      >=  1           )
               AND   ( SPA-GMN-SELNUM      <=  SPA-GMN-MAX )
                   CONTINUE 
               ELSE
                   MOVE    "0001"                  TO  SPA-ERRCD
                   GO  TO  400-SELNUM-CHK-EXT 
               END-IF
           END-IF   
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-SELNUM
      *
           PERFORM 4001-SELNUM-HENSYU-SEC               
           .
       400-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4001-SELNUM-HENSYU-SEC          SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2007"              TO  SYS-KANRICD
           MOVE    SPA-NAI-TKBNCD    (SPA-NAI-SELNUM)
                                       TO  SYS-KBNCD
           MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                       TO  SYS-STYUKYMD
           MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                       TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF     FLG-SYSKANRI         =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
               MOVE    SYS-2007-KBNCD      TO  SPA-NAI-KBNCD
      *
               MOVE    SYS-2007-STYUKYMD   TO  SPA-NAI-STYUKYMD
               MOVE    SYS-2007-STYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
               MOVE    SYS-2007-EDYUKYMD   TO  SPA-NAI-EDYUKYMD
               MOVE    SYS-2007-EDYUKYMD   TO  WRK-YMD  
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
               MOVE    SYS-2007-MEISYO (1) TO  SPA-GMN-MEISYO
               MOVE    SYS-2007-MEISYO (2) TO  SPA-GMN-MEISYO2
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-KOHNUM-MAX
                       OR      SPA-GMN-TKOHNUM (SPA-NAI-SELNUM IDX)
                                   =   SPACE
                   MOVE    SPA-GMN-TKOHNUM (SPA-NAI-SELNUM IDX)
                                               TO  SPA-GMN-KOHNUM (IDX)
                   MOVE    SPACE               TO  HKNNUM-REC
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE    SPA-GMN-KOHNUM (IDX)
                                               TO  HKN-HKNNUM
                   MOVE    "00"                TO  HKN-PAYKBN
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-READ-SEC
                   MOVE    HKN-TANSEIDONAME    TO SPA-GMN-SEIDONAME
                                                                  (IDX)
               END-PERFORM
      *
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0101"              TO  SPA-ERRCD
           END-IF    
           .
       4001-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       400-KOHNUMLST-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               IF      W64-SELECT (IDX)    =   "T"
                   IF      IDX                 =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX         TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                 SPA-GMN-KOUMOKU
                                      SPA-NAI-KOUMOKU
      *
           MOVE    SPA-GMN-TRENNUM (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-SELNUM
      *                                 
           PERFORM 4001-SELNUM-HENSYU-SEC               
           .
       400-KOHNUMLST-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    有効開始年月日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC          SECTION.
      *     
           MOVE    ZERO                TO  SPA-NAI-STYUKYMD 
      *
           IF      SPA-GMN-STYUKYMD    =   SPACE
               IF      WRK-MCP-WIDGET      =   "STYUKYMD"
                   MOVE    "00000000"          TO  SPA-GMN-STYUKYMD
               END-IF
               GO  TO  41021-STYUKYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
               IF      SPA-NAI-STYUKYMD
                                       NOT =   ZERO
                   IF      SPA-NAI-STYUKYMD (7:2)
                                               =   "01"
                       CONTINUE
                   ELSE
                       MOVE    "0106"              TO  SPA-ERRCD
                   END-IF
               END-IF  
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了年月日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-EDYUKYMD    =   SPACE
               IF      WRK-MCP-WIDGET      =   "EDYUKYMD"
                   MOVE    "99999999"          TO  SPA-GMN-EDYUKYMD
                   MOVE    99999999            TO  SPA-NAI-EDYUKYMD
               ELSE
                   MOVE    ZERO                TO  SPA-NAI-EDYUKYMD
                   GO  TO  41021-EDYUKYMD-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      WRK-HENYMDG         =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
      *        末日チェック
               IF      SPA-NAI-EDYUKYMD
                                       NOT =   99999999 
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY7-AREA
                   MOVE    "71"                TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-EDYUKYMD (1:6)
                                               TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                   IF      LNK-DAY7-KOYOMI NOT =   SPA-NAI-EDYUKYMD
                       MOVE    "0107"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    開始日との関連チェック   
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-STYUKYMD    NOT =   SPACE )  AND
                     ( SPA-GMN-EDYUKYMD    NOT =   SPACE )
                   IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                       MOVE    "0102"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称入力処理
      *****************************************************************
       41021-MEISYO-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-MEISYO      =   SPACE
      ****     IF      WRK-MCP-WIDGET      =   "MEISYO"
      ****         MOVE    "0007"              TO  SPA-ERRCD
      ****     END-IF
               CONTINUE
           ELSE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-MEISYO      TO  KANACHK-MAE-INPUT
               MOVE    44                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-MEISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称２入力処理
      *****************************************************************
       41021-MEISYO2-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-MEISYO2      =   SPACE
      ****     IF      WRK-MCP-WIDGET      =   "MEISYO2"
      ****         MOVE    "0007"              TO  SPA-ERRCD
      ****     END-IF
               CONTINUE
           ELSE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-MEISYO2     TO  KANACHK-MAE-INPUT
               MOVE    44                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO2
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-MEISYO2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       41021-KOHNUM-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-KOHNUM-MAX
                   OR      SPA-ERRCD   NOT =   SPACE
               MOVE    SPACE           TO  SPA-GMN-SEIDONAME (IDX)
      *
               IF      SPA-GMN-KOHNUM (IDX)
                                           =   SPACE
                   CONTINUE 
               ELSE 
                   MOVE    SPACE           TO  HKNNUM-REC
                   INITIALIZE                  HKNNUM-REC
                   MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
                   MOVE    SPA-GMN-KOHNUM (IDX)
                                           TO  HKN-HKNNUM
                   MOVE    "00"            TO  HKN-PAYKBN
                   MOVE    HKNNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-READ-SEC
      *
                   IF      FLG-HKNNUM      =   ZERO
                       MOVE    HKN-TANSEIDONAME     TO
                                           SPA-GMN-SEIDONAME (IDX)
                       IF      SPA-GMN-KOHNUM (IDX)(1:1)
                                               =   "0" OR  "9"
                           MOVE    "0005"          TO  SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE    "0004"          TO  SPA-ERRCD
                   END-IF
               END-IF    
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   IF      IDX             <   11
                       COMPUTE SPA-GMN-CUR     =   4   +   IDX
                   ELSE
                       COMPUTE SPA-GMN-CUR     =   5   +   IDX
                   END-IF
               END-IF
           END-PERFORM    
      *
           .
       41021-KOHNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *   
      *    編集項目が全て未入力
      *****IF    ( SPA-GMN-MEISYO      =   SPACE )
      ****     MOVE    "0007"              TO  SPA-ERRCD
      ****     MOVE    4                   TO  SPA-GMN-CUR
      **** ELSE
               MOVE    "0103"              TO  SPA-ERRCD
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-KOHNUM-MAX
                   IF    ( SPA-GMN-KOHNUM (IDX)    NOT =   SPACE )
                       MOVE    SPACE               TO  SPA-ERRCD
                       MOVE    WRK-KOHNUM-MAX      TO  IDX
                   END-IF
               END-PERFORM
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
      **** END-IF
      *
      *    有効期間の開始日と終了日が同時に変更されていないこと
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-NAI-SELNUM      >   ZERO  )
               IF    ( SPA-NAI-STYUKYMD
                                       NOT =   SPA-NAI-TSTYUKYMD
                                                      (SPA-NAI-SELNUM) )
               AND
                     ( SPA-NAI-EDYUKYMD
                                       NOT =   SPA-NAI-TEDYUKYMD
                                                      (SPA-NAI-SELNUM) )
                   MOVE    "0108"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF      
      *
      *    同一公費番号が重複してないか
           IF      SPA-ERRCD           =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-KOHNUM-MAX
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF      SPA-GMN-KOHNUM (IDX)
                                           NOT =   SPACE
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >    WRK-KOHNUM-MAX
                               OR      SPA-ERRCD   NOT =   SPACE
                           IF    ( SPA-GMN-KOHNUM (IDX)
                                                   NOT =   SPACE )
                           AND   ( IDX             NOT =   IDY   )
                               IF      SPA-GMN-KOHNUM (IDX)
                                               =   SPA-GMN-KOHNUM (IDY)
                                   MOVE    "0109"      TO  SPA-ERRCD
                                   IF      IDX         <   11
                                       COMPUTE SPA-GMN-CUR =   4   +
                                                               IDX
                                   ELSE
                                       COMPUTE SPA-GMN-CUR =   5   +
                                                               IDX
                                   END-IF
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    同一公費番号の条件で期間重複があるか
           IF      SPA-ERRCD           =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF      SPA-NAI-SELNUM  NOT =   IDX 
                       IF    ( SPA-NAI-EDYUKYMD
                                       <   SPA-NAI-TSTYUKYMD (IDX) )
                       OR    ( SPA-NAI-STYUKYMD
                                       >   SPA-NAI-TEDYUKYMD (IDX) )
                           CONTINUE
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       WRK-KOHNUM-MAX
                                   OR      SPA-ERRCD   NOT =   SPACE
                               IF      SPA-GMN-KOHNUM (IDY)
                                               NOT =   SPACE
                                   PERFORM VARYING IDZ FROM    1   BY  1
                                           UNTIL   IDZ
                                                   >   WRK-KOHNUM-MAX
                                           OR      SPA-GMN-TKOHNUM
                                                               (IDX IDZ)
                                                       =   SPACE
                                           OR      SPA-ERRCD
                                                   NOT =   SPACE
                                       IF      SPA-GMN-KOHNUM (IDY)
                                           =   SPA-GMN-TKOHNUM (IDX IDZ)
                                           MOVE    "0105"  TO  SPA-ERRCD
                                       END-IF
                                  END-PERFORM
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       IF      IDX             <   11
                           COMPUTE SPA-GMN-CUR =   4   +  IDX
                       ELSE
                           COMPUTE SPA-GMN-CUR =   5   +  IDX
                       END-IF
                   END-IF
               END-PERFORM 
           END-IF      
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-NAI-SELNUM
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       420-CLEAR-EXT.
           EXIT.
      *
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-GID-SEC              SECTION.
      *
      *    対象データ未選択時チェック
           IF      SPA-NAI-SELNUM      =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
      *    有効期間変時チェック
           IF      SPA-ERRCD           =   SPACE 
               IF    ( SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                       NOT =   SPA-NAI-STYUKYMD )
               OR    ( SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                       NOT =   SPA-NAI-EDYUKYMD )      
                   MOVE    "0104"              TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE 
               MOVE    "2001"              TO  SPA-WIDCD
           END-IF
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
      *    削除処理
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2007"              TO  SYS-KANRICD
           MOVE    SPA-NAI-KBNCD       TO  SYS-KBNCD
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 420-CLEAR-SEC
      *
      *        一覧再編集処理
               PERFORM 3101-SPA-HENSYU-SEC
               IF      W64-ROW             =   ZERO
                   MOVE    SPA-GMN-MAX         TO  W64-ROW
               END-IF
               MOVE    ZERO                TO  FLG-ROW
      *
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       421-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前処理
      *****************************************************************
       420-TOUROKU-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2002"          TO  SPA-WIDCD
           END-IF
      *
           .
       420-TOUROKU-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       421-TOUROKU-SEC                 SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-NAI-SELNUM       >   ZERO
               MOVE    SPACE               TO  SYS-2007-REC
               INITIALIZE                      SYS-2007-REC
               MOVE    "2007"              TO  SYS-2007-KANRICD
               MOVE    SPA-NAI-KBNCD       TO  SYS-2007-KBNCD
               MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                           TO  SYS-2007-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                           TO  SYS-2007-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-2007-HOSPNUM
               MOVE    SYS-2007-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF     FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2007-REC
               ELSE
                   MOVE    "0101"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-2007-REC
               INITIALIZE                      SYS-2007-REC
               MOVE    "2007"              TO  SYS-2007-KANRICD
               MOVE    SPA-HOSPNUM         TO  SYS-2007-HOSPNUM
               MOVE    SYS-2007-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2007-REC
                   MOVE    SYS-2007-KBNCD(1:3) TO  WRK-2007-KBNCD
               ELSE
                   MOVE    ZERO                TO  WRK-2007-KBNCD
               END-IF
               ADD     1                   TO  WRK-2007-KBNCD-9
           END-IF
      *
      *   登録処理
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-SELNUM       =   ZERO
                   MOVE    SPACE               TO  SYS-2007-REC
                   INITIALIZE                      SYS-2007-REC
                   MOVE    "2007"              TO  SYS-2007-KANRICD
                   MOVE    WRK-2007-KBNCD      TO  SYS-2007-KBNCD
                   MOVE    SPA-NAI-STYUKYMD    TO  SYS-2007-STYUKYMD
                   IF      SPA-NAI-EDYUKYMD    =   ZERO
                       MOVE    99999999            TO  SPA-NAI-EDYUKYMD
                   END-IF 
                   MOVE    SPA-NAI-EDYUKYMD    TO  SYS-2007-EDYUKYMD
               ELSE
                   MOVE    SPA-NAI-STYUKYMD    TO  SYS-2007-STYUKYMD
                   MOVE    SPA-NAI-EDYUKYMD    TO  SYS-2007-EDYUKYMD
               END-IF 
      *
               INITIALIZE                      SYS-2007-KOH-AREA
                                               SYS-2007-KOH1-AREA
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1  BY  1
                       UNTIL   IDX >       WRK-KOHNUM-MAX
                   IF      SPA-GMN-KOHNUM (IDX)    =   SPACE
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDY
                       IF      IDY             <   11
                           MOVE    SPA-GMN-KOHNUM (IDX)
                                               TO  SYS-2007-KOHNUM (IDY)
                       ELSE
                           COMPUTE IDZ =   IDY -   10
                           MOVE    SPA-GMN-KOHNUM (IDX)
                                               TO  SYS-2007-KOHNUM1(IDZ)
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    SPA-GMN-MEISYO      TO  SYS-2007-MEISYO (1)
               MOVE    SPA-GMN-MEISYO2     TO  SYS-2007-MEISYO (2)
      *
               MOVE    SYS-2007-REC        TO  SYSKANRI-REC
               IF      SPA-NAI-SELNUM      =   ZERO
                   MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
               ELSE
                   MOVE    "2007"              TO  SYSUP-KANRICD
                   MOVE    SPA-NAI-KBNCD       TO  SYSUP-KBNCD
                   MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                               TO  SYSUP-STYUKYMD
                   MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                               TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
               END-IF 
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   IF      SPA-NAI-SELNUM        =   ZERO
                       MOVE    "1001"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "1002"              TO  SPA-ERRCD
                   END-IF
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        画面クリア処理
               PERFORM 420-CLEAR-SEC
      *
      *        一覧再編集処理
               PERFORM 3101-SPA-HENSYU-SEC
               IF      W64-ROW             =   ZERO
                   MOVE    SPA-GMN-MAX         TO  W64-ROW
               END-IF
               MOVE    ZERO                TO  FLG-ROW
      *
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       421-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W64    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-W64-ROW
           ELSE
               MOVE    W64-ROW             TO  WRK-W64-ROW
           END-IF
      *
           INITIALIZE                      W64
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-MAX
               MOVE    SPA-GMN-TRENNUM   (IDX) TO  W64-TRENNUM    (IDX)
               MOVE    SPA-GMN-TMEISYO   (IDX) TO  W64-TMEISYO    (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX) TO  W64-TSTYUKYMD  (IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX) TO  W64-TEDYUKYMD  (IDX)
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       WRK-KOHNUM-MAX 
                   MOVE    SPA-GMN-TKOHNUM (IDX IDY)
                                               TO  W64-TKOHNUM (IDX IDY)
               END-PERFORM
      *         
               IF      IDX                 =   SPA-NAI-SELNUM
                   MOVE    "T"                 TO  W64-SELECT (IDX)
               ELSE
                   MOVE    "F"                 TO  W64-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  W64-COUNT
           MOVE    ZERO                TO  W64-ROW
           IF      WRK-W64-ROW         >   ZERO
               MOVE    WRK-W64-ROW         TO  W64-ROW
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  W64-SELNUM
           MOVE    SPA-GMN-STYUKYMD    TO  W64-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W64-EDYUKYMD
           MOVE    SPA-GMN-MEISYO      TO  W64-MEISYO
           MOVE    SPA-GMN-MEISYO2     TO  W64-MEISYO2
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       WRK-KOHNUM-MAX 
               MOVE    SPA-GMN-KOHNUM   (IDY)  TO  W64-KOHNUM    (IDY)
               MOVE    SPA-GMN-SEIDONAME(IDY)  TO  W64-SEIDONAME (IDY)
           END-PERFORM
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
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    2
                   MOVE    "STYUKYMD"      TO  MCP-WIDGET
               WHEN    3
                   MOVE    "EDYUKYMD"      TO  MCP-WIDGET
               WHEN    4
                   MOVE    "MEISYO"        TO  MCP-WIDGET
               WHEN    5
                   MOVE    "KOHNUM1"       TO  MCP-WIDGET
               WHEN    6
                   MOVE    "KOHNUM2"       TO  MCP-WIDGET
               WHEN    7
                   MOVE    "KOHNUM3"       TO  MCP-WIDGET
               WHEN    8
                   MOVE    "KOHNUM4"       TO  MCP-WIDGET
               WHEN    9
                   MOVE    "KOHNUM5"       TO  MCP-WIDGET
               WHEN    10
                   MOVE    "KOHNUM6"       TO  MCP-WIDGET
               WHEN    11
                   MOVE    "KOHNUM7"       TO  MCP-WIDGET
               WHEN    12
                   MOVE    "KOHNUM8"       TO  MCP-WIDGET
               WHEN    13
                   MOVE    "KOHNUM9"       TO  MCP-WIDGET
               WHEN    14
                   MOVE    "KOHNUM10"      TO  MCP-WIDGET
               WHEN    15
                   MOVE    "MEISYO2"       TO  MCP-WIDGET
               WHEN    16
                   MOVE    "KOHNUM11"      TO  MCP-WIDGET
               WHEN    17
                   MOVE    "KOHNUM12"      TO  MCP-WIDGET
               WHEN    18
                   MOVE    "KOHNUM13"      TO  MCP-WIDGET
               WHEN    19
                   MOVE    "KOHNUM14"      TO  MCP-WIDGET
               WHEN    20
                   MOVE    "KOHNUM15"      TO  MCP-WIDGET
               WHEN    21
                   MOVE    "KOHNUM16"      TO  MCP-WIDGET
               WHEN    22
                   MOVE    "KOHNUM17"      TO  MCP-WIDGET
               WHEN    23
                   MOVE    "KOHNUM18"      TO  MCP-WIDGET
               WHEN    24
                   MOVE    "KOHNUM19"      TO  MCP-WIDGET
               WHEN    25
                   MOVE    "KOHNUM20"      TO  MCP-WIDGET
      *
               WHEN    999
                   MOVE    "B12"           TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "MEISYO"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "MEISYO2"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "KOHNUM1"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "KOHNUM2"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "KOHNUM3"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "KOHNUM4"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "KOHNUM5"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "KOHNUM6"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "KOHNUM7"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "KOHNUM8"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "KOHNUM9"
                   MOVE    14          TO  SPA-GMN-CUR
               WHEN    "KOHNUM10"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "KOHNUM11"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "KOHNUM12"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "KOHNUM13"
                   MOVE    19          TO  SPA-GMN-CUR
               WHEN    "KOHNUM14"
                   MOVE    20          TO  SPA-GMN-CUR
               WHEN    "KOHNUM15"
                   MOVE    21          TO  SPA-GMN-CUR
               WHEN    "KOHNUM16"
                   MOVE    22          TO  SPA-GMN-CUR
               WHEN    "KOHNUM17"
                   MOVE    23          TO  SPA-GMN-CUR
               WHEN    "KOHNUM18"
                   MOVE    24          TO  SPA-GMN-CUR
               WHEN    "KOHNUM19"
                   MOVE    25          TO  SPA-GMN-CUR
               WHEN    "KOHNUM20"
                   MOVE    999         TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効期間開始日入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "有効期間終了日入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "公費番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "地方公費のみ入力できます"
                                           TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "名称が入力してありません"
                                           TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "名称は全角で入力して下さい"
                                           TO  SPA-ERRMSG
      *
               WHEN    "0101"
                   MOVE    "選択されたデータの読み込みができません"
                                           TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE
                       "有効期間開始日＞有効期間終了日となっています"
                                           TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "対象の公費番号が未入力です"
                                           TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "有効期間の変更が行われています"
                                           TO  SPA-ERRMSG
               WHEN    "0105"
                   MOVE
                  "公費番号が同じで有効期間の重複するデータが存在します"
                                           TO  SPA-ERRMSG
               WHEN    "0106"
                   MOVE    "有効期間開始日は１日で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0107"
                   MOVE    "有効期間終了日は月末日で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0108"
                   MOVE
                       "有効期間の開始日と終了日は同時に変更できません"
                                           TO  SPA-ERRMSG
               WHEN    "0109"
                   MOVE
                  "公費番号が重複して入力されています"
                                           TO  SPA-ERRMSG
      *
               WHEN    "1001"
                   MOVE    "追加できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "更新できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "削除できませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W64"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
      *
           EVALUATE    SPA-WIDCD
               WHEN    "2001"
                   MOVE
                       "選択されたデータを削除します。よろしいですか"
                                                TO  WID1-ID1MSG
               WHEN    "2002"
                   MOVE    "データを登録します。よろしいですか"
                                                TO  WID1-ID1MSG
           END-EVALUATE
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W64"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
           MOVE    ZERO            TO  WRK-SYMD
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-YMD             TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC              =   ZERO )
                   MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY          TO  WRK-SYMD
               END-IF
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC 
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           PERFORM     UNTIL (     FLG-HKNNUM    =  1                  )
                        OR   (    (HKN-TEKSTYMD  <= SPA-NAI-EDYUKYMD )
                              AND (HKN-TEKEDYMD  >= SPA-NAI-STYUKYMD ) )
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DB-CLOSE-SEC
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-N-SEC       SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNNUM
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-DB-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DB-CLOSE-EXT.
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
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

