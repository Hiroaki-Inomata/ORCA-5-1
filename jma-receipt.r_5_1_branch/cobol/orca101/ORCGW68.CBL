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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW68.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 保険者付加情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/01    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
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
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
      *
       01  SPA-W68.
           03  SPA-GMN-CUR                         PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-KJNYMD                  PIC X(10).
               05  SPA-GMN-INPUT-AREA.
                 06  SPA-GMN-SELNUM1               PIC X(50).
                 06  SPA-GMN-HKNJANUM              PIC X(08).
                 06  SPA-GMN-HKNJANAME             PIC X(100).
                 06  SPA-GMN-YUKOSTYMD             PIC X(10).
                 06  SPA-GMN-YUKOEDYMD             PIC X(10).
                 06  SPA-GMN-SELNUM2               PIC S9(2).
                 06  SPA-GMN-HKNJAFUKA-AREA.
                   07  SPA-GMN-HBTNUM10KBN-G.
                       09  SPA-GMN-HBTNUM10KBN             PIC X(01).
                       09  SPA-GMN-HBTNUM10KBNF            PIC X(01).
                       09  SPA-GMN-HBTNUM10KBNMEI          PIC X(78).
                   07  SPA-GMN-HBTNUM10TAIKBN-G.
                       09  SPA-GMN-HBTNUM10TAIKBN          PIC X(01).
                       09  SPA-GMN-HBTNUM10TAIKBNF         PIC X(01).
                       09  SPA-GMN-HBTNUM10TAIKBNMEI       PIC X(38).
                   07  SPA-GMN-HBTNUM11KBN-G.
                       09  SPA-GMN-HBTNUM11KBN             PIC X(01).
                       09  SPA-GMN-HBTNUM11KBNF            PIC X(01).
                       09  SPA-GMN-HBTNUM11KBNMEI          PIC X(78).
                   07  SPA-GMN-HBTNUM11TAIKBN-G.
                       09  SPA-GMN-HBTNUM11TAIKBN          PIC X(01).
                       09  SPA-GMN-HBTNUM11TAIKBNF         PIC X(01).
                       09  SPA-GMN-HBTNUM11TAIKBNMEI       PIC X(38).
                   07  SPA-GMN-HBTNUM20KBN-G.
                       09  SPA-GMN-HBTNUM20KBN             PIC X(01).
                       09  SPA-GMN-HBTNUM20KBNF            PIC X(01).
                       09  SPA-GMN-HBTNUM20KBNMEI          PIC X(78).
                   07  SPA-GMN-HBTNUM20TAIKBN-G.
                       09  SPA-GMN-HBTNUM20TAIKBN          PIC X(01).
                       09  SPA-GMN-HBTNUM20TAIKBNF         PIC X(01).
                       09  SPA-GMN-HBTNUM20TAIKBNMEI       PIC X(38).
                   07  SPA-GMN-HBTNUM21KBN-G.
                       09  SPA-GMN-HBTNUM21KBN             PIC X(01).
                       09  SPA-GMN-HBTNUM21KBNF            PIC X(01).
                       09  SPA-GMN-HBTNUM21KBNMEI          PIC X(78).
                   07  SPA-GMN-HBTNUM21TAIKBN-G.
                       09  SPA-GMN-HBTNUM21TAIKBN          PIC X(01).
                       09  SPA-GMN-HBTNUM21TAIKBNF         PIC X(01).
                       09  SPA-GMN-HBTNUM21TAIKBNMEI       PIC X(38).
               05  SPA-GMN-CMB-AREA.
                   07  SPA-GMN-HBTNUMKBNLST-G.
                       09  SPA-GMN-HBTNUMKBN-MAX           PIC 9(03).
                       09  SPA-GMN-HBTNUMKBNLST            OCCURS  10.
                           11  SPA-GMN-HBTNUMKBNL          PIC X(01).
                           11  SPA-GMN-HBTNUMKBNFL         PIC X(01).
                           11  SPA-GMN-HBTNUMKBNMEIL       PIC X(78).
                   07  SPA-GMN-HBTNUMTAIKBNLST-G.
                       09  SPA-GMN-HBTNUMTAIKBN-MAX        PIC 9(03).
                       09  SPA-GMN-HBTNUMTAIKBNLST         OCCURS  10.
                           11  SPA-GMN-HBTNUMTAIKBNL       PIC X(01).
                           11  SPA-GMN-HBTNUMTAIKBNFL      PIC X(01).
                           11  SPA-GMN-HBTNUMTAIKBNMEIL    PIC X(38).
               05  SPA-GMN-HKNJALST-AREA.
                   07  SPA-GMN-HKNJALST-MAX                PIC 9(5).
                   07  SPA-GMN-HKNJALST-SEL                PIC 9(5).
                   07  SPA-GMN-HKNJALST                    OCCURS  200.
                       09  SPA-GMN-THKNJANUM               PIC X(8).
                       09  SPA-GMN-THKNJANAME              PIC X(100).
                       09  SPA-GMN-TYUKOSTYMD              PIC X(10).
                       09  SPA-GMN-TYUKOEDYMD              PIC X(10).
               05  SPA-GMN-RIREKILST-AREA.
                   07  SPA-GMN-RIREKILST-MAX               PIC 9(5).
                   07  SPA-GMN-RIREKILST-SEL               PIC 9(5).
                   07  SPA-GMN-RIREKILST                   OCCURS   20.
                       09  SPA-GMN-TRYUKOSTYMD             PIC X(10).
                       09  SPA-GMN-TRYUKOEDYMD             PIC X(10).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-KJNYMD                      PIC X(08).
                   07  SPA-NAI-INPUT-AREA.
                       09  SPA-NAI-INPUT-MODE              PIC 9(1).
                       09  SPA-NAI-SELNUM1-FLG             PIC 9(1)
                                                           OCCURS  200.
                       09  SPA-NAI-HKNJANUM                PIC X(08).
                       09  SPA-NAI-HKNJANUM-ORG            PIC X(08).
                       09  SPA-NAI-YUKOSTYMD               PIC X(08).
                       09  SPA-NAI-YUKOSTYMD-ORG           PIC X(08).
                       09  SPA-NAI-YUKOEDYMD               PIC X(08).
                       09  SPA-NAI-YUKOEDYMD-ORG           PIC X(08).
                       09  SPA-NAI-HKNJAFUKA-AREA.
                           11  SPA-NAI-HKNJAFUKA-DUMMY     PIC X(01).
               05  SPA-NAI-HKNJALST-AREA.
                   07  SPA-NAI-HKNJALST                    OCCURS   200.
                       09  SPA-NAI-TYUKOSTYMD              PIC X(08).
                       09  SPA-NAI-TYUKOEDYMD              PIC X(08).
               05  SPA-NAI-RIREKILST-AREA.
                   07  SPA-NAI-RIREKILST                   OCCURS   20.
                       09  SPA-NAI-TRYUKOSTYMD             PIC X(08).
                       09  SPA-NAI-TRYUKOEDYMD             PIC X(08).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                                     PIC 9(01).
           03  FLG-SYSKANRI                                PIC 9(01).
           03  FLG-HKNJAINF                                PIC 9(01).
           03  FLG-ENTRYCHK                                PIC 9(01).
           03  FLG-HIZUKECHK                               PIC 9(01).
           03  FLG-UPD-KBN                                 PIC 9(01).
           03  FLG-KIKAN-DABURI                            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                        PIC 9(05).
           03  IDX1                                        PIC 9(05).
           03  IDX-MAX                                     PIC 9(05).
           03  IDX-CMB                                     PIC 9(05).
           03  IDX-MUL                                     PIC 9(05).
           03  IDX-LST                                     PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SEL                                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET                              PIC X(64). 
           03  WRK-WIDMSG                                  PIC X(70). 
           03  WRK-MAX                                     PIC 9(05).
           03  WRK-YMD                                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                                 PIC 9(04).
               05  WRK-SMM                                 PIC 9(02).
               05  WRK-SDD                                 PIC 9(02).
           03  WRK-HENYMDG                                 PIC X(09).
           03  WRK-YUKOSTYMD                               PIC X(08).
           03  WRK-YUKOEDYMD                               PIC X(08).
           03  WRK-KANACHK-INPUT                           PIC X(400).
           03  WRK-ZZ9                                     PIC ZZ9.
           03  WRK-ZZZ9                                    PIC ZZZ9.
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                                 PIC ZZZ.
           03  WRK-STR-TMP                                 PIC X(100).
           03  WRK-CMB-CD                                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                             PIC X(200).
               05  WRK-CMB-ITM2                            PIC X(200).
           03  WRK-ERRSTYMD                                PIC X(09).
           03  WRK-ERREDYMD                                PIC X(09).
           03  WRK-SELNUM-G.
               05  WRK-SELNUM-MUL                          PIC X(50).
               05  WRK-INPUT-MODE                          PIC 9(01).
               05  WRK-SELNUM-X                            PIC X(50).
               05  WRK-SELNUM                              PIC 9(03).
               05  WRK-BASESELNUM                          PIC 9(03).
               05  WRK-SELNUM-FLG                          PIC 9(01)
                                                           OCCURS  200.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM                             PIC X(10)
                                                           OCCURS 20.
               05  WRK-MUL-STNUM-X                         PIC X(10).
               05  WRK-MUL-EDNUM-X                         PIC X(10).
               05  WRK-MUL-STNUM                           PIC 9(05).
               05  WRK-MUL-EDNUM                           PIC 9(05).
           03  WRK-PAGENO                                  PIC 9(03).
           03  WRK-NAI-YUKOSTYMD                           PIC X(08).
           03  WRK-NAI-YUKOEDYMD                           PIC X(08).
      *
       01  SEL-AREA.
           05  SEL-HKNJANUM                                PIC X(08).
           05  SEL-KJNYMD                                  PIC X(08).
           05  SEL-STYMD                                   PIC X(08).
           05  SEL-EDYMD                                   PIC X(08).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-HKNJALST-MAX         PIC 9(03) VALUE 200.
           03  CONST-RIREKILST-MAX        PIC 9(03) VALUE 20.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    保険者付加情報
           COPY  "CPSK2012.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SEL-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W68
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W68         TO  SPA-FREE
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
           IF    ( SPA-MOTOPG      =   "WERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
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
           MOVE    "W68"               TO  MCP-WINDOW.
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
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
               IF    ( SPA-W01-KBNCD   NOT =   SPACE )
                   PERFORM 312-SPASET-SEC
               END-IF
           END-EVALUATE
      *
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
           INITIALIZE              SPA-W68
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-KJNYMD
      *
           PERFORM 311-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-RIREKILST-AREA
                                   SPA-NAI-RIREKILST-AREA
      *
           PERFORM 311-COMBO-EDIT-SEC
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 3110-HKNJALST-EDIT-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       311-COMBO-EDIT-SEC         SECTION.
      *
      *    法定外現物給付コンボ
           PERFORM 3110-HBTNUMKBNLST-SEC
      *
      *    法定外現物給付対象区分コンボ
           PERFORM 3110-HBTNUMTAIKBNLST-SEC
      *
           .
       311-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    法定外現物給付コンボ編集処理
      *****************************************************************
       3110-HBTNUMKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-HBTNUMKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-HBTNUMKBNL    (1)
           MOVE    "給付なし"      TO  SPA-GMN-HBTNUMKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-HBTNUMKBNL    (2)
           MOVE    "給付あり"      TO  SPA-GMN-HBTNUMKBNMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-HBTNUMKBN-MAX
      *
           .
       3110-HBTNUMKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    法定外現物給付対象区分コンボ編集処理
      *****************************************************************
       3110-HBTNUMTAIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-HBTNUMTAIKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-HBTNUMTAIKBNL    (1)
           MOVE    "該当被保険者全員" 
                                   TO  SPA-GMN-HBTNUMTAIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-HBTNUMTAIKBN-MAX
      *
           .
       3110-HBTNUMTAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    保険者情報リスト編集処理
      *****************************************************************
       3110-HKNJALST-EDIT-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNJALST-AREA
                                       SPA-NAI-HKNJALST-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2012"          TO  SYS-KANRICD
           MOVE    SPA-NAI-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2012-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-HKNJALST-MAX  > CONST-HKNJALST-MAX )
      *
               COMPUTE SPA-GMN-HKNJALST-MAX
                   =   SPA-GMN-HKNJALST-MAX  +   1
      *
               MOVE    SPA-GMN-HKNJALST-MAX
                                       TO  IDX-MAX
               MOVE    SYS-2012-KBNCD  TO  SPA-GMN-THKNJANUM(IDX-MAX)
               MOVE    SYS-2012-KBNCD  TO  SEL-HKNJANUM
               PERFORM 900-HKNJAINF-KEY-SEL-SEC
               MOVE    HKNJA-HKNJANAME
                                       TO  SPA-GMN-THKNJANAME(IDX-MAX)
               MOVE    SYS-2012-STYUKYMD
                                       TO  SPA-NAI-TYUKOSTYMD(IDX-MAX)
               MOVE    SYS-2012-STYUKYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TYUKOSTYMD(IDX-MAX)
      *
               MOVE    SYS-2012-EDYUKYMD
                                       TO  SPA-NAI-TYUKOEDYMD(IDX-MAX)
               MOVE    SYS-2012-EDYUKYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TYUKOEDYMD(IDX-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-2012-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3110-HKNJALST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       312-SPASET-SEC              SECTION.
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-HKNJALST-MAX )
                    OR   ( IDX-LST >   ZERO )
      *
               IF    ( SPA-GMN-THKNJANUM (IDX0)
                                   =   SPA-W01-KBNCD )
                   MOVE    IDX0    TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-LST >   ZERO )
               MOVE    IDX-LST     TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9    TO  W68-SELNUM1
               PERFORM 4100-SELNUM1-CHK-SEC
               MOVE    101     TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-W01-KBNCD
                                   TO  SEL-HKNJANUM
               PERFORM 900-HKNJAINF-KEY-SEL-SEC
               IF    ( FLG-HKNJAINF    =   ZERO  )
                   MOVE    SPA-W01-KBNCD
                                   TO  SPA-GMN-HKNJANUM
                   PERFORM 4102-KIHON-CHK-SEC
                   MOVE    101     TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
           .
       312-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "3001"
                       PERFORM 42021-HENKOU-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    6           TO      SPA-GMN-CUR
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    基準日
           WHEN    ANY             ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *    選択番号１
           WHEN    "ACTIVATE"      ALSO    "SELNUM1"
               PERFORM 4100-SELNUM1-CHK-SEC
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM2"
               PERFORM 4100-SELNUM2-CHK-SEC
      *    保険者リスト
           WHEN    ANY             ALSO    "HKNJALST"
               PERFORM 4100-HKNJALST-CHK-SEC
      *    履歴リスト
           WHEN    ANY             ALSO    "RIREKILST"
               PERFORM 4100-RIREKILST-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W68-HKNJANUM        TO  SPA-GMN-HKNJANUM
           MOVE    W68-HBTNUM10KBN     TO  SPA-GMN-HBTNUM10KBN
           MOVE    W68-HBTNUM10TAIKBN  TO  SPA-GMN-HBTNUM10TAIKBN
           MOVE    W68-HBTNUM11KBN     TO  SPA-GMN-HBTNUM11KBN
           MOVE    W68-HBTNUM11TAIKBN  TO  SPA-GMN-HBTNUM11TAIKBN
           MOVE    W68-HBTNUM20KBN     TO  SPA-GMN-HBTNUM20KBN
           MOVE    W68-HBTNUM20TAIKBN  TO  SPA-GMN-HBTNUM20TAIKBN
           MOVE    W68-HBTNUM21KBN     TO  SPA-GMN-HBTNUM21KBN
           MOVE    W68-HBTNUM21TAIKBN  TO  SPA-GMN-HBTNUM21TAIKBN
           MOVE    W68-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    W68-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM1-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HKNJANUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM10KBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM10TAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM11KBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM11TAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM20KBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM20TAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM21KBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-HBTNUM21TAIKBN-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W68"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 311-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
      *    既存の履歴データをチェックし、期間の重複したデータが
      *    存在しないこと
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    1               TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4201-SYS2012-INSERT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
               IF    ( SPA-GMN-HKNJALST-MAX  >=  CONST-HKNJALST-MAX )
                   MOVE    "1001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
      *    選択番号が入力されていること
           IF    ( SPA-GMN-SELNUM1 =   SPACE )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   PERFORM 4202-HENKOU-SEC
               ELSE
                   PERFORM 4201-HENKOU-SEC
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                        PERFORM 311-SPASET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(通常の変更処理)
      *****************************************************************
       4201-HENKOU-SEC                 SECTION.
      *
      *    保険者番号が変更されていないこと
           IF    ( SPA-GMN-HKNJANUM    =   SPA-NAI-HKNJANUM-ORG )
               CONTINUE
           ELSE
               MOVE    "1106"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間の開始日と終了日が同時に変更されていないこと
               IF    ( SPA-NAI-YUKOSTYMD   NOT = SPA-NAI-YUKOSTYMD-ORG )
                AND  ( SPA-NAI-YUKOEDYMD   NOT = SPA-NAI-YUKOEDYMD-ORG )
                   MOVE    "1107"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    2                   TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4201-SYS2012-UPDATE-SEC
           END-IF
      *
           .
       4201-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(一括変更)
      *****************************************************************
       4202-HENKOU-SEC                 SECTION.
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       4202-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(一括変更)
      *****************************************************************
       42021-HENKOU-SEC                SECTION.
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-HKNJALST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF  ( SPA-NAI-SELNUM1-FLG (IDX-LST) =   1 )
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "2012"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-THKNJANUM (IDX-LST)
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-TYUKOSTYMD (IDX-LST)
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYSKANRI-REC    TO  SYS-2012-REC
      *
      *                保険者付加情報編集処理
                       PERFORM 4201-HKNJAINF-EDIT-SEC
      *
                       MOVE    SYS-2012-REC    TO  SYSKANRI-REC
                       PERFORM 800-MCNDATE-SEC
                       MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF   (  MCP-RC          NOT =   ZERO )
                           MOVE    "4005"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
      *
           .
       42021-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    保険者付加基本情報編集処理
      *****************************************************************
       4201-SYS2012-EDIT-SEC            SECTION.
      *
           MOVE    SPACE                  TO  SYS-2012-REC
           INITIALIZE                         SYS-2012-REC
           MOVE    SPA-HOSPNUM            TO  SYS-2012-HOSPNUM
           MOVE    "2012"                 TO  SYS-2012-KANRICD
           MOVE    SPA-GMN-HKNJANUM         TO  SYS-2012-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD      TO  SYS-2012-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD      TO  SYS-2012-EDYUKYMD
      *
           PERFORM 4201-HKNJAINF-EDIT-SEC
      *
           .
       4201-SYS2012-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険者付加情報編集処理
      *****************************************************************
       4201-HKNJAINF-EDIT-SEC            SECTION.
      *
           MOVE    SPA-GMN-HBTNUM10KBN     TO SYS-2012-HBTNUM10KBN
           MOVE    SPA-GMN-HBTNUM10TAIKBN  TO SYS-2012-HBTNUM10-TAIKBN
           MOVE    SPA-GMN-HBTNUM11KBN     TO SYS-2012-HBTNUM11KBN
           MOVE    SPA-GMN-HBTNUM11TAIKBN  TO SYS-2012-HBTNUM11-TAIKBN
           MOVE    SPA-GMN-HBTNUM20KBN     TO SYS-2012-HBTNUM20KBN
           MOVE    SPA-GMN-HBTNUM20TAIKBN  TO SYS-2012-HBTNUM20-TAIKBN
           MOVE    SPA-GMN-HBTNUM21KBN     TO SYS-2012-HBTNUM21KBN
           MOVE    SPA-GMN-HBTNUM21TAIKBN  TO SYS-2012-HBTNUM21-TAIKBN
      *
           .
       4201-HKNJAINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
           PERFORM 4101-SELNUM1-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM1 =   SPACE )
                   MOVE    "1105"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2001"      TO  SPA-WIDCD
           END-IF
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
           PERFORM  4201-SYS2012-DELETE-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 311-SPASET-SEC
           END-IF
      *
           .
       421-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-KIKAN-DABURI
           MOVE    SPACE           TO  WRK-ERRSTYMD
                                       WRK-ERREDYMD
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2012"          TO  SYS-KANRICD
           MOVE    SPA-GMN-HKNJANUM  TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2012-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( FLG-KIKAN-DABURI    =   1 )
      *
      *        有効期間が重複するデータがないかチェック
      *        （変更処理の場合、元データは対象外とする）
               IF    ( FLG-UPD-KBN           =  2 )
                AND  ( SPA-NAI-HKNJANUM-ORG    =   SYS-2012-KBNCD  )
                AND  ( SPA-NAI-YUKOSTYMD-ORG =   SYS-2012-STYUKYMD )
                AND  ( SPA-NAI-YUKOEDYMD-ORG =   SYS-2012-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-YUKOEDYMD <   SYS-2012-STYUKYMD )
                     OR  ( SPA-NAI-YUKOSTYMD >   SYS-2012-EDYUKYMD )
                       CONTINUE
                   ELSE
                       MOVE    SYS-2012-STYUKYMD TO    WRK-YMD
                       PERFORM 800-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG       TO    WRK-ERRSTYMD
                       MOVE    SYS-2012-EDYUKYMD TO    WRK-YMD
                       PERFORM 800-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG       TO    WRK-ERREDYMD
                       MOVE    1             TO  FLG-KIKAN-DABURI
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-2012-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者付加情報更新処理
      *****************************************************************
       4201-SYS2012-UPDATE-SEC         SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2012"          TO  SYS-KANRICD
           MOVE    SPA-NAI-HKNJANUM-ORG
                                   TO  SYS-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD-ORG
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI     =   ZERO )
      *
               PERFORM 4201-SYS2012-EDIT-SEC
      *
               MOVE    SYS-2012-REC        TO  SYSKANRI-REC
               MOVE    SYS-2012-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-2012-KANRICD    TO  SYSUP-KANRICD
               MOVE    SPA-NAI-HKNJANUM-ORG  TO  SYSUP-KBNCD
               MOVE    SPA-NAI-YUKOSTYMD-ORG
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-NAI-YUKOEDYMD-ORG
                                           TO  SYSUP-EDYUKYMD
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
      *
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC          NOT =   ZERO )
                   MOVE    "4001"      TO  SPA-ERRCD
               END-IF
      *
           ELSE
                   MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-SYS2012-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    保険者付加情報追加処理
      *****************************************************************
       4201-SYS2012-INSERT-SEC         SECTION.
      *
      *    保険者付加情報編集処理
           PERFORM 4201-SYS2012-EDIT-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-2012-CREYMD
                                       SYS-2012-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-2012-UPHMS
      *
           MOVE    SYS-2012-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "4003"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-SYS2012-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    保険者付加情報削除処理
      *****************************************************************
       4201-SYS2012-DELETE-SEC         SECTION.
      *
           INITIALIZE                      SYS-2012-REC
           MOVE    SPA-HOSPNUM         TO  SYS-2012-HOSPNUM
           MOVE    "2012"              TO  SYS-2012-KANRICD
           MOVE    SPA-NAI-HKNJANUM-ORG  TO  SYS-2012-KBNCD
           MOVE    SPA-NAI-YUKOSTYMD-ORG
                                       TO  SYS-2012-STYUKYMD
           MOVE    SPA-NAI-YUKOEDYMD-ORG
                                       TO  SYS-2012-EDYUKYMD
           MOVE    SYS-2012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "4004"      TO  SPA-ERRCD
           END-IF
      *
           .
       4201-SYS2012-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    基準日
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-CUR
      *
           MOVE    W68-KJNYMD  TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF  ( FLG-HIZUKECHK     =   ZERO )
      *
               INITIALIZE              SPA-W68
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
           ELSE
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       4100-SELNUM1-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           INITIALIZE                  WRK-SELNUM-G
           MOVE    ZERO            TO  SPA-NAI-INPUT-MODE
      *
           MOVE    W68-SELNUM1     TO  SPA-GMN-SELNUM1
      *
           IF    ( SPA-GMN-SELNUM1 =  SPACE )
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4201-RIREKI-AREA-CLEAR-SEC
               MOVE    ZERO            TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SELNUM1     TO  WRK-KANACHK-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-SELNUM1
      *
               PERFORM 4101-SELNUM1-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 41001-SELNUM1-CHK-SEC
               ELSE
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
                   PERFORM 4201-RIREKI-AREA-CLEAR-SEC
                   MOVE    W68-SELNUM1     TO  SPA-GMN-SELNUM1
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       41001-SELNUM1-CHK-SEC           SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           IF    ( WRK-INPUT-MODE      =   2 )
               MOVE    W68-SELNUM1     TO  SPA-GMN-SELNUM1
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-HKNJALST-MAX )
                   MOVE    WRK-SELNUM-FLG (IDX0)
                                       TO  SPA-NAI-SELNUM1-FLG (IDX0)
               END-PERFORM
           END-IF
      *
           MOVE    WRK-BASESELNUM      TO  SPA-GMN-HKNJALST-SEL
      *
           MOVE    WRK-INPUT-MODE      TO  SPA-NAI-INPUT-MODE
           MOVE    SPA-GMN-THKNJANUM (WRK-BASESELNUM)
                                       TO  SEL-HKNJANUM
           MOVE    SPA-NAI-KJNYMD      TO  SEL-KJNYMD
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
       41001-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       4101-SELNUM1-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNUM1     =  SPACE )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELNUM1 (1:1)    =   "." )
                   MOVE    SPA-GMN-SELNUM1 (2:)
                                           TO  WRK-SELNUM-MUL
               ELSE
                   MOVE    SPA-GMN-SELNUM1 TO  WRK-SELNUM-MUL
               END-IF
      *
               PERFORM 41011-MULTI-SEL-CHK-SEC
           END-IF
      *
           .
      *
       4101-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数行選択チェック処理
      *****************************************************************
       41011-MULTI-SEL-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
           MOVE    ZERO            TO  WRK-INPUT-MODE
                                       WRK-BASESELNUM
      *
           UNSTRING    WRK-SELNUM-MUL  DELIMITED   BY  ","
                                                   OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
                       WRK-MUL-NUM (6)
                       WRK-MUL-NUM (7)
                       WRK-MUL-NUM (8)
                       WRK-MUL-NUM (9)
                       WRK-MUL-NUM (10)
                       WRK-MUL-NUM (11)
                       WRK-MUL-NUM (12)
                       WRK-MUL-NUM (13)
                       WRK-MUL-NUM (14)
                       WRK-MUL-NUM (15)
                       WRK-MUL-NUM (16)
                       WRK-MUL-NUM (17)
                       WRK-MUL-NUM (18)
                       WRK-MUL-NUM (19)
                       WRK-MUL-NUM (20)
           ON  OVERFLOW
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0015"      TO  SPA-ERRCD  
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-UNSTRING
      *
           MOVE    ZERO            TO  CNT-SEL
                                       WRK-SELNUM
           PERFORM VARYING IDX-MUL    FROM    1   BY  1
                   UNTIL ( IDX-MUL    >       20 )
      *
               IF    ( WRK-MUL-NUM (IDX-MUL)  =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   UNSTRING    WRK-MUL-NUM (IDX-MUL) DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       MOVE    1           TO  SPA-GMN-CUR
                       MOVE    "0001"      TO  SPA-ERRCD
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    WRK-MUL-EDNUM-X
                                           TO  WRK-MUL-STNUM-X
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-STNUM-X     TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-EDNUM
      *
      *
                   PERFORM VARYING IDX1    FROM    WRK-MUL-STNUM   BY  1
                           UNTIL ( IDX1    >       WRK-MUL-EDNUM )
                       IF    ( IDX1        >=  1 )
                        AND  ( IDX1        <=  SPA-GMN-HKNJALST-MAX )
                           COMPUTE CNT-SEL  =   CNT-SEL  +   1
                           IF    ( CNT-SEL  =   1 )
                               MOVE    IDX1    TO  WRK-BASESELNUM
                           END-IF
                           MOVE    1       TO  WRK-SELNUM-FLG (IDX1)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-SEL
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-HKNJALST-MAX )
      *
               IF    ( WRK-SELNUM-FLG (IDX1)   =   1 )
                   COMPUTE CNT-SEL =   CNT-SEL +   1
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SEL         >   1 )
               MOVE    2           TO  WRK-INPUT-MODE
           ELSE
               MOVE    1           TO  WRK-INPUT-MODE
           END-IF
      *
           .
       41011-MULTI-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号範囲チェック処理
      *****************************************************************
       4101-SELNUM-LIMIT-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-SELNUM-X    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
               MOVE    SNUM-OUTNUM TO  WRK-SELNUM
               IF    ( WRK-SELNUM  >=  1 )
                AND  ( WRK-SELNUM  <=  SPA-GMN-HKNJALST-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-SELNUM-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
      *    選択番号１
           IF    ( SPA-NAI-INPUT-MODE      =   1 )
               MOVE    SPA-GMN-HKNJALST-SEL  TO  WRK-ZZZ
               IF    ( WRK-ZZZ-G (1:1)     =   SPACE )
                   UNSTRING    WRK-ZZZ-G   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR-TMP
                               SPA-GMN-SELNUM1
                   END-UNSTRING
               ELSE
                   MOVE    WRK-ZZZ-G   TO  SPA-GMN-SELNUM1
               END-IF
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2012"          TO  SYS-KANRICD
           MOVE    SEL-HKNJANUM      TO  SYS-KBNCD
           MOVE    SEL-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-2012-REC
      *
           MOVE    SYS-2012-STYUKYMD
                                   TO  SPA-NAI-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD-ORG
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOSTYMD
      *
           MOVE    SYS-2012-EDYUKYMD
                                   TO  SPA-NAI-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD-ORG
                                       WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-YUKOEDYMD
      *
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               MOVE    SYS-2012-KBNCD  TO  SPA-GMN-HKNJANUM
                                           SPA-NAI-HKNJANUM
                                           SPA-NAI-HKNJANUM-ORG
                                           SEL-HKNJANUM
               PERFORM 900-HKNJAINF-KEY-SEL-SEC
               MOVE    HKNJA-HKNJANAME TO  SPA-GMN-HKNJANAME
           ELSE
               MOVE    SPACE           TO  SPA-GMN-HKNJANUM
                                           SPA-NAI-HKNJANUM
                                           SPA-NAI-HKNJANUM-ORG
           END-IF
      *
           MOVE    SYS-2012-HBTNUM10KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM10KBN-G
      *
           MOVE    SYS-2012-HBTNUM10-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM10TAIKBN-G
      *
           MOVE    SYS-2012-HBTNUM11KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM11KBN-G
      *
           MOVE    SYS-2012-HBTNUM11-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM11TAIKBN-G
      *
           MOVE    SYS-2012-HBTNUM20KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM20KBN-G
      *
           MOVE    SYS-2012-HBTNUM20-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM20TAIKBN-G
      *
           MOVE    SYS-2012-HBTNUM21KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM21KBN-G
      *
           MOVE    SYS-2012-HBTNUM21-TAIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-HBTNUM21TAIKBN-G
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    履歴領域編集処理
      *****************************************************************
       4100-RIREKI-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE      SPA-GMN-RIREKILST-AREA
                           SPA-NAI-RIREKILST-AREA
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2012"              TO  SYS-KANRICD
           MOVE    SPA-GMN-HKNJANUM      TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-2012-REC
      *
           PERFORM VARYING IDX-MAX FROM    1   BY  1
               UNTIL     ( FLG-SYSKANRI    NOT =   ZERO )
                 OR      ( IDX-MAX      >  CONST-RIREKILST-MAX )
      *
               MOVE    IDX-MAX         TO  SPA-GMN-RIREKILST-MAX
      *
      *        有効開始日
               MOVE    SYS-2012-STYUKYMD
                                       TO  SPA-NAI-TRYUKOSTYMD (IDX-MAX)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TRYUKOSTYMD (IDX-MAX)
      *
      *        有効終了日
               MOVE    SYS-2012-EDYUKYMD
                                       TO  SPA-NAI-TRYUKOEDYMD (IDX-MAX)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TRYUKOEDYMD (IDX-MAX)
      *
               IF    ( SPA-NAI-YUKOSTYMD-ORG
                                   =   SPA-NAI-TRYUKOSTYMD (IDX-MAX))
                AND  ( SPA-NAI-YUKOEDYMD-ORG
                                   =   SPA-NAI-TRYUKOEDYMD (IDX-MAX))
                   MOVE    IDX-MAX    TO  SPA-GMN-SELNUM2
                   MOVE    IDX-MAX    TO  SPA-GMN-RIREKILST-SEL
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
               MOVE    SYSKANRI-REC   TO  SYS-2012-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
      *
       4100-RIREKI-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号
      *****************************************************************
       4101-HKNJANUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-HKNJANUM =   SPACE   )
               MOVE    SPACE               TO  SPA-GMN-HKNJANAME
           ELSE
               MOVE    SPA-GMN-HKNJANUM    TO  SEL-HKNJANUM
               PERFORM 900-HKNJAINF-KEY-SEL-SEC
               IF    ( FLG-HKNJAINF    NOT =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
                   MOVE    "0006"      TO  SPA-ERRCD
               ELSE
                   MOVE    HKNJA-HKNJANAME TO  SPA-GMN-HKNJANAME
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-HKNJANUM      NOT =   SPA-NAI-HKNJANUM )
      *
               IF    ( SPA-ERRCD           =   SPACE )
      *
                   PERFORM 4201-YUKOKIKAN-RESET-SEC
      *
                   INITIALIZE              SPA-GMN-HKNJAFUKA-AREA
                                           SPA-NAI-HKNJAFUKA-AREA
                   PERFORM 42011-COMBO-SET-DEFAULT-SEC
      *
               END-IF
      *
               MOVE    SPA-GMN-HKNJANUM  TO  SPA-NAI-HKNJANUM
      *
           END-IF
      *
           .
      *
       4101-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    感３７の２（法別１０）チェック処理
      *****************************************************************
       4101-HBTNUM10KBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM10KBN     TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM10KBN-G
      *
           IF    ( SPA-GMN-HBTNUM10KBN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-HBTNUM10TAIKBN
           END-IF
      *
           .
      *
       4101-HBTNUM10KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    感３７の２（法別１０）対象区分チェック処理
      *****************************************************************
       4101-HBTNUM10TAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM10TAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM10TAIKBN-G
      *
           .
      *
       4101-HBTNUM10TAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    結核入院（法別１１）チェック処理
      *****************************************************************
       4101-HBTNUM11KBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM11KBN     TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM11KBN-G
      *
           IF    ( SPA-GMN-HBTNUM11KBN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-HBTNUM11TAIKBN
           END-IF
      *
           .
      *
       4101-HBTNUM11KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    結核入院（法別１１）対象区分チェック処理
      *****************************************************************
       4101-HBTNUM11TAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM11TAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM11TAIKBN-G
      *
           .
      *
       4101-HBTNUM11TAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神入院（法別２０）チェック処理
      *****************************************************************
       4101-HBTNUM20KBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM20KBN     TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM20KBN-G
      *
           IF    ( SPA-GMN-HBTNUM20KBN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-HBTNUM20TAIKBN
           END-IF
      *
           .
      *
       4101-HBTNUM20KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神入院（法別２０）対象区分チェック処理
      *****************************************************************
       4101-HBTNUM20TAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM20TAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM20TAIKBN-G
      *
           .
      *
       4101-HBTNUM20TAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神通院（法別２１）チェック処理
      *****************************************************************
       4101-HBTNUM21KBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM21KBN     TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM21KBN-G
      *
           IF    ( SPA-GMN-HBTNUM21KBN     =   "0" )
               MOVE    "0"                 TO  SPA-GMN-HBTNUM21TAIKBN
           END-IF
      *
           .
      *
       4101-HBTNUM21KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神通院（法別２１）対象区分チェック処理
      *****************************************************************
       4101-HBTNUM21TAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HBTNUM21TAIKBN  TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-HBTNUM21TAIKBN-G
      *
           .
      *
       4101-HBTNUM21TAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日
      *****************************************************************
       4101-YUKOSTYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-NAI-YUKOSTYMD   TO  WRK-YUKOSTYMD
           MOVE    SPACE               TO  SPA-NAI-YUKOSTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-YUKOSTYMD   =   SPACE OR "00000000" )
               MOVE    "00000000"  TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
           ELSE
               MOVE    SPA-GMN-YUKOSTYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( FLG-HIZUKECHK     =   ZERO )
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOSTYMD
               ELSE
                   MOVE    WRK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOSTYMD
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-YUKOSTYMD (7:2) =   "01" )
                       CONTINUE
                   ELSE
                       MOVE    "0016"  TO  SPA-ERRCD
                       MOVE    4       TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
           END-IF
      *
           .
      *
       4101-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日
      *****************************************************************
       4101-YUKOEDYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-YUKOEDYMD
           MOVE    SPACE               TO  SPA-NAI-YUKOEDYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-YUKOEDYMD   =   SPACE OR "99999999" )
               MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                           SPA-NAI-YUKOEDYMD
           ELSE
               MOVE    SPA-GMN-YUKOEDYMD
                                       TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( FLG-HIZUKECHK     =   ZERO )
                   MOVE    WRK-HENYMDG TO  SPA-GMN-YUKOEDYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOEDYMD
               ELSE
                   MOVE    WRK-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-YUKOEDYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-YUKOEDYMD
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY7-AREA
                   MOVE    "71"        TO  LNK-DAY7-IRAI
                   MOVE    SPA-NAI-YUKOEDYMD (1:6)
                                       TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"       USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
                   IF    ( LNK-DAY7-KOYOMI =   SPA-NAI-YUKOEDYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0017"  TO  SPA-ERRCD
                       MOVE    5       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4101-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       4100-SELNUM2-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    W68-SELNUM2     TO  SPA-GMN-SELNUM2
      *
           IF  (   SPA-GMN-SELNUM2 =   ZERO )
               MOVE    2           TO  SPA-GMN-CUR
           ELSE
               IF    ( SPA-GMN-RIREKILST-MAX
                                      >=   SPA-GMN-SELNUM2 )
                   PERFORM 41001-SELNUM2-CHK-SEC
               ELSE
                   MOVE    3       TO  SPA-GMN-CUR
                   MOVE    "1008"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       41001-SELNUM2-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SELNUM2 TO  SPA-GMN-RIREKILST-SEL
      *
           MOVE    SPA-NAI-HKNJANUM-ORG
                                   TO  SEL-HKNJANUM
           MOVE    SPA-NAI-TRYUKOSTYMD (SPA-GMN-SELNUM2)
                                   TO  SEL-KJNYMD
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    SPA-GMN-RIREKILST-SEL
                                   TO  SPA-GMN-SELNUM2
      *
           MOVE    1               TO  SPA-NAI-INPUT-MODE
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       41001-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者リスト
      *****************************************************************
       4100-HKNJALST-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-HKNJALST-SEL
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-HKNJALST-MAX )
                    OR   ( SPA-GMN-HKNJALST-SEL  NOT =   ZERO )
      *
               IF    ( W68-HKNJALST-SELECT (IDX-LST) =   "T" )
                   MOVE    IDX-LST     TO  SPA-GMN-HKNJALST-SEL
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-HKNJALST-SEL  TO  WRK-ZZZ
           MOVE    WRK-ZZZ-G           TO  W68-SELNUM1
           PERFORM 4100-SELNUM1-CHK-SEC
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       4100-HKNJALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴リスト
      *****************************************************************
       4100-RIREKILST-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-RIREKILST-SEL
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST    >   20 )
                    OR   ( SPA-GMN-RIREKILST-SEL   NOT =   ZERO )
      *
               IF    ( W68-RIREKILST-SELECT (IDX-LST)  =  "T" )
                   MOVE    IDX-LST TO  SPA-GMN-RIREKILST-SEL
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-RIREKILST-SEL
                                       TO  W68-SELNUM2
           PERFORM 4100-SELNUM2-CHK-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       4100-RIREKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-INPUT-AREA
                       SPA-NAI-INPUT-AREA
      *
           PERFORM 4201-YUKOKIKAN-RESET-SEC
      *
           PERFORM 42011-COMBO-SET-DEFAULT-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    有効期間初期化処理
      *****************************************************************
       4201-YUKOKIKAN-RESET-SEC        SECTION.
      *
      *    有効期間の開始日と終了日を設定
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD
                                       SPA-NAI-YUKOSTYMD
      *
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD
                                       SPA-NAI-YUKOEDYMD
      *
           .
      *
       4201-YUKOKIKAN-RESET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期値設定処理
      *****************************************************************
       42011-COMBO-SET-DEFAULT-SEC     SECTION.
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-HBTNUM10KBN-G
                                       SPA-GMN-HBTNUM11KBN-G
                                       SPA-GMN-HBTNUM20KBN-G
                                       SPA-GMN-HBTNUM21KBN-G
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-HBTNUMTAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-HBTNUM10TAIKBN-G
                                       SPA-GMN-HBTNUM11TAIKBN-G
                                       SPA-GMN-HBTNUM20TAIKBN-G
                                       SPA-GMN-HBTNUM21TAIKBN-G
      *
           .
       42011-COMBO-SET-DEFAULT-ETXT.
           EXIT.
      *****************************************************************
      *   履歴情報領域初期化処理
      *****************************************************************
       4201-RIREKI-AREA-CLEAR-SEC      SECTION.
      *
           INITIALIZE  SPA-GMN-RIREKILST-AREA
                       SPA-NAI-RIREKILST-AREA
      *
           MOVE    ZERO            TO  SPA-GMN-HKNJALST-SEL
      *
           .
      *
       4201-RIREKI-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-HKNJANUM          =   SPACE )
               MOVE    "1101"          TO  SPA-ERRCD
               MOVE   2                TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOSTYMD   =   SPACE )
                   MOVE    "1102"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   =   SPACE )
                   MOVE    "1103"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-YUKOEDYMD   <   SPA-NAI-YUKOSTYMD )
                   MOVE    "1004"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
              END-IF
          END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W68    "           TO  MCP-WINDOW.
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
           MOVE    SPACE           TO  W68
           INITIALIZE                  W68
      *
      *    基準日
           MOVE    SPA-GMN-KJNYMD      TO  W68-KJNYMD
      *
      *    選択番号１
           MOVE    SPA-GMN-SELNUM1     TO  W68-SELNUM1
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
              CONTINUE
           ELSE
              PERFORM 5001-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-HBTNUM10KBN-G       TO  W68-HBTNUM10KBN
           MOVE    SPA-GMN-HBTNUM10TAIKBN-G    TO  W68-HBTNUM10TAIKBN
           MOVE    SPA-GMN-HBTNUM11KBN-G       TO  W68-HBTNUM11KBN
           MOVE    SPA-GMN-HBTNUM11TAIKBN-G    TO  W68-HBTNUM11TAIKBN
           MOVE    SPA-GMN-HBTNUM20KBN-G       TO  W68-HBTNUM20KBN
           MOVE    SPA-GMN-HBTNUM20TAIKBN-G    TO  W68-HBTNUM20TAIKBN
           MOVE    SPA-GMN-HBTNUM21KBN-G       TO  W68-HBTNUM21KBN
           MOVE    SPA-GMN-HBTNUM21TAIKBN-G    TO  W68-HBTNUM21TAIKBN
      *
           MOVE    SPA-GMN-HBTNUMKBN-MAX       TO  W68-HBTNUM10KBN-COUNT
                                                   W68-HBTNUM11KBN-COUNT
                                                   W68-HBTNUM20KBN-COUNT
                                                   W68-HBTNUM21KBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HBTNUMKBN-MAX )
                   MOVE    SPA-GMN-HBTNUMKBNLST (IDX-CMB)
                                   TO  W68-HBTNUM10KBN-ITEM (IDX-CMB)
                                       W68-HBTNUM11KBN-ITEM (IDX-CMB)
                                       W68-HBTNUM20KBN-ITEM (IDX-CMB)
                                       W68-HBTNUM21KBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-HBTNUMKBN-MAX       TO  W68-HBTNUM10KBN-COUNT
                                                   W68-HBTNUM11KBN-COUNT
                                                   W68-HBTNUM20KBN-COUNT
                                                   W68-HBTNUM21KBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HBTNUMKBN-MAX )
                   MOVE    SPA-GMN-HBTNUMKBNLST (IDX-CMB)
                                   TO  W68-HBTNUM10TAIKBN-ITEM (IDX-CMB)
                                       W68-HBTNUM11TAIKBN-ITEM (IDX-CMB)
                                       W68-HBTNUM20TAIKBN-ITEM (IDX-CMB)
                                       W68-HBTNUM21TAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    保険者リスト
           MOVE    SPA-GMN-HKNJALST-MAX  TO  W68-HKNJALST-COUNT
           PERFORM VARYING IDX-LST    FROM    1   BY  1
               UNTIL     ( IDX-LST    >   SPA-GMN-HKNJALST-MAX )
      *
               MOVE    IDX-LST         TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W68-TNO   (IDX-LST)
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   IF    ( SPA-NAI-SELNUM1-FLG (IDX-LST)
                                       =   1 )
                       MOVE    "*"     TO  W68-TNO   (IDX-LST)(1:1)
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN-THKNJANUM  (IDX-LST)
                                       TO  W68-THKNJANUM (IDX-LST)
               MOVE    SPA-GMN-THKNJANAME (IDX-LST)
                                       TO  W68-THKNJANAME(IDX-LST)
               MOVE    SPA-GMN-TYUKOSTYMD(IDX-LST)
                                       TO  W68-TYUKSTYMD(IDX-LST)
               MOVE    SPA-GMN-TYUKOEDYMD(IDX-LST)
                                       TO  W68-TYUKEDYMD(IDX-LST)
      *
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-HKNJALST-SEL  =   IDX-LST )
                       MOVE    "T"     TO  W68-HKNJALST-SELECT (IDX-LST)
                   ELSE
                       MOVE    "F"     TO  W68-HKNJALST-SELECT (IDX-LST)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    履歴リスト
           MOVE    SPA-GMN-RIREKILST-MAX
                                       TO  W68-RIREKILST-COUNT
      *
           PERFORM VARYING IDX-LST    FROM    1   BY  1
               UNTIL     ( IDX-LST    >   SPA-GMN-RIREKILST-MAX )
      *
               MOVE    IDX-LST         TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  W68-TRNO (IDX-LST)
      *
               STRING  SPA-GMN-TRYUKOSTYMD (IDX-LST)
                       " 〜 "
                       SPA-GMN-TRYUKOEDYMD (IDX-LST)
                       DELIMITED   BY  SIZE
               INTO    W68-TRKIKAN (IDX-LST)
               END-STRING
      *
               IF    ( IDX-LST    =    SPA-GMN-RIREKILST-SEL )
                   MOVE    "T"         TO  W68-RIREKILST-SELECT(IDX-LST)
               ELSE
                   MOVE    "F"         TO  W68-RIREKILST-SELECT(IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "black"                 TO  W68-LBLMODE-STYLE
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               MOVE    "blue"              TO  W68-LBLMODE-STYLE
               MOVE    "<一括変更＞"       TO  W68-LBLMODE
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       5001-GMNHEN-SEC                 SECTION.
      *
           MOVE    SPA-GMN-HKNJANUM    TO  W68-HKNJANUM
           MOVE    SPA-GMN-HKNJANAME   TO  W68-HKNJANAME
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  W68-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD   TO  W68-YUKOEDYMD
           MOVE    SPA-GMN-SELNUM2     TO  W68-SELNUM2
      *
           .
       5001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  W68-B06-STATE
                                       W68-B07-STATE
                                       W68-B08-STATE
                                       W68-HKNJANUM-STATE
                                       W68-HKNJANAME-STATE
                                       W68-YUKOSTYMD-STATE
                                       W68-YUKOEDYMD-STATE
                                       W68-SELNUM2-STATE
                                       W68-HBTNUM10TAIKBN-STATE
                                       W68-HBTNUM10TAIKBN-ITEM-STATE
                                       W68-HBTNUM11TAIKBN-STATE
                                       W68-HBTNUM11TAIKBN-ITEM-STATE
                                       W68-HBTNUM20TAIKBN-STATE
                                       W68-HBTNUM20TAIKBN-ITEM-STATE
                                       W68-HBTNUM21TAIKBN-STATE
                                       W68-HBTNUM21TAIKBN-ITEM-STATE
      *
           IF    ( SPA-GMN-HBTNUM10KBN     =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W68-HBTNUM10TAIKBN-STATE
                                       W68-HBTNUM10TAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-HBTNUM11KBN     =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W68-HBTNUM11TAIKBN-STATE
                                       W68-HBTNUM11TAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-HBTNUM20KBN     =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W68-HBTNUM20TAIKBN-STATE
                                       W68-HBTNUM20TAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-HBTNUM21KBN     =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W68-HBTNUM21TAIKBN-STATE
                                       W68-HBTNUM21TAIKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-HKNJALST-MAX  >=  CONST-HKNJALST-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  W68-B06-STATE
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE   =  2 )
               MOVE    WIDGET-INSENSITIVE
                                        TO W68-B06-STATE
                                           W68-B08-STATE
                                           W68-HKNJANUM-STATE
                                           W68-HKNJANAME-STATE
                                           W68-YUKOSTYMD-STATE
                                           W68-YUKOEDYMD-STATE
                                           W68-SELNUM2-STATE
           END-IF
      *
           IF    ( SPA-GMN-HKNJALST-SEL   >  0 )
            AND  ( SPA-GMN-HKNJALST-SEL   <= SPA-GMN-HKNJALST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                        TO W68-HKNJANUM-STATE
                                           W68-HKNJANAME-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           WHEN    002
               MOVE    "HKNJANUM"          TO  MCP-WIDGET
           WHEN    101
               MOVE    "HBTNUM10KBN"       TO  MCP-WIDGET
           WHEN    102
               MOVE    "HBTNUM10TAIKBN"    TO  MCP-WIDGET
           WHEN    103
               MOVE    "HBTNUM11KBN"       TO  MCP-WIDGET
           WHEN    104
               MOVE    "HBTNUM11TAIKBN"    TO  MCP-WIDGET
           WHEN    105
               MOVE    "HBTNUM20KBN"       TO  MCP-WIDGET
           WHEN    106
               MOVE    "HBTNUM20TAIKBN"    TO  MCP-WIDGET
           WHEN    107
               MOVE    "HBTNUM21KBN"       TO  MCP-WIDGET
           WHEN    108
               MOVE    "HBTNUM21TAIKBN"    TO  MCP-WIDGET
           WHEN    003
               MOVE    "SELNUM2"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    005
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    006
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    007
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    008
               MOVE    "B07"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM1"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "HKNJANUM"
               MOVE    101                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM10KBN"
               MOVE    102                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM10TAIKBN"
               MOVE    103                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM11KBN"
               MOVE    104                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM11TAIKBN"
               MOVE    105                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM20KBN"
               MOVE    106                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM20TAIKBN"
               MOVE    107                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM21KBN"
               MOVE    108                 TO  SPA-GMN-CUR
           WHEN    "HBTNUM21TAIKBN"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "SELNUM2"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "YUKOSTYMD"
               MOVE    005                 TO  SPA-GMN-CUR
           WHEN    "YUKOEDYMD"
               MOVE    007                 TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    001                 TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-CUR         =   7 )
               IF    ( W68-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   1 )
               IF    ( SPA-GMN-HKNJALST-MAX  =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   2 )
               IF    ( W68-HKNJANUM-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    101                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   102 )
               IF    ( W68-HBTNUM10TAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    103                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   104 )
               IF    ( W68-HBTNUM11TAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    105                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   106 )
               IF    ( W68-HBTNUM20TAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    107                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   108 )
               IF    ( W68-HBTNUM21TAIKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-INPUT-MODE  =   2 )
               EVALUATE    SPA-GMN-CUR
               WHEN    2
                   COMPUTE SPA-GMN-CUR =   101
               WHEN    4
               WHEN    5
                   COMPUTE SPA-GMN-CUR =   8
               END-EVALUATE
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "保険者番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "有効期間開始日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "有効期間終了日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "有効期間に該当保険者情報のマスタがありません"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "選択された番号が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "有効期間開始日は１日で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "有効期間終了日は月末日で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "範囲内の選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間開始日＞有効期間終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE     "保険者番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE     "有効期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE     "有効期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1104"
               STRING  "有効期間の重複するデータが存在します（"
                                           DELIMITED   BY  SIZE
                       WRK-ERRSTYMD        DELIMITED   BY  SIZE
                       " 〜 "              DELIMITED   BY  SIZE
                       WRK-ERREDYMD        DELIMITED   BY  SIZE
                       "）"                DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1001"
               MOVE    "追加可能最大件数に達しました"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               MOVE    "保険者を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1106"
               MOVE    "保険者番号の変更はできません"
                                       TO  SPA-ERRMSG
           WHEN    "1107"
               MOVE    "有効期間の開始日と終了日は同時に変更できません"
                                       TO  SPA-ERRMSG
           WHEN    "4000"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W68"               TO  SPA-MOTOPG
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
           EVALUATE    SPA-WIDCD
           WHEN    "2001"
               MOVE   SPACE             TO  WRK-WIDMSG
               STRING "選択中の保険者付加情報を削除します。"
                                        DELIMITED  BY  SIZE
                      "よろしいですか"
                                        DELIMITED  BY  SIZE
               INTO   WRK-WIDMSG
               END-STRING
           WHEN    "3001"
               MOVE   "選択中の保険者付加情報を変更します"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W68"                TO  SPA-MOTOPG
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
      *    法廷外現物給付コンボボックスチェック処理
      *****************************************************************
       4200-HBTNUMKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HBTNUMKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HBTNUMKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-HBTNUMKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-HBTNUMKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HBTNUMKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    法廷外現物給付対象区分コンボボックスチェック処理
      *****************************************************************
       4200-HBTNUMTAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HBTNUMTAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HBTNUMTAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-HBTNUMTAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-HBTNUMTAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HBTNUMTAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-HIZUKECHK
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKECHK
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
      *    保険番号検索
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    SEL-HKNJANUM        TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-HKNJAINF
           ELSE
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
