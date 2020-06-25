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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGX99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 病名一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/04    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    チェックマスタ情報用画面スパ
           COPY    "X01SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-BYOMEI                  PIC 9(01).
           03  FLG-USERBYOMEI              PIC 9(01).
           03  FLG-PTBYOMEI                PIC 9(01).
           03  FLG-SRCBYOLST-ROW           PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-LOOP-END                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRCBYOLST               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
           03  IDX-LST                     PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM             PIC X(200).
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-SKIP-MAX                PIC 9(05).
           03  WRK-MAE-INPUT               PIC X(100).
           03  WRK-OUT-INPUT               PIC X(100).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-KEISANYMD               PIC X(08).
           03  WRK-SEL-BYOMEI-G.
               05  WRK-SEL-BUNRUICD        PIC X(03).
               05  WRK-SEL-SRYKA           PIC X(02).
               05  WRK-SEL-BYOMEI          PIC X(100).
               05  WRK-SEL-STBYOMEICD      PIC X(07).
               05  WRK-SEL-EDBYOMEICD      PIC X(07).
           03  WRK-NO                      PIC 9(05).
           03  WRK-TABLE                   PIC X(64).
           03  WRK-PATHNAME                PIC X(64).
      *
       01  WRK-ROW-AREA.
           03  WRK-SRCBYOLST-ROW           PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
           03  CONST-SRCBYOLST-MAX         PIC 9(05)   VALUE   100.
           03  CONST-SRTKBN-MAX            PIC 9(05)   VALUE   50.
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名マスター
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSBYOMEI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
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
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01FREE     TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "XERR" )
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
           MOVE    "X99"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-X99-AREA
           MOVE    SPA-X99-BYOMEI      TO  SPA-GMN-X99-SEL-BYOMEI
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           IF    ( SPA-X01-INDEX       =   ZERO )
      *
               MOVE    SPA-GMN-X99-SRCBYOKBNLST (1)
                                       TO  SPA-GMN-X99-SRCBYOKBN-G
               PERFORM 3101-SRTKBNLST-PTBYO-SET-SEC
               MOVE    SPA-GMN-X99-SRTKBNLST (1)
                                       TO  SPA-GMN-X99-SRTKBN-G
               PERFORM 4101-SRCBYOLST-SEC
               IF    ( SPA-GMN-X99-SRCBYOLST-MAX   =   ZERO )
                   MOVE    SPA-GMN-X99-SRCBYOKBNLST (3)
                                       TO  SPA-GMN-X99-SRCBYOKBN-G
                   PERFORM 3101-SRTKBNLST-UBYO-SET-SEC
                   MOVE    SPA-GMN-X99-SRTKBNLST (1)
                                       TO  SPA-GMN-X99-SRTKBN-G
                   PERFORM 4101-SRCBYOLST-SEC
               END-IF
      *
               IF    ( SPA-GMN-X99-SRCBYOLST-MAX   =   ZERO )
                   MOVE    SPA-GMN-X99-SRCBYOKBNLST (2)
                                       TO  SPA-GMN-X99-SRCBYOKBN-G
                   PERFORM 3101-SRTKBNLST-BYO-SET-SEC
                   MOVE    SPA-GMN-X99-SRTKBNLST (1)
                                       TO  SPA-GMN-X99-SRTKBN-G
                   PERFORM 4101-SRCBYOLST-SEC
               END-IF
           ELSE
               MOVE    SPA-GMN-X99-SRCBYOKBNLST (3)
                                       TO  SPA-GMN-X99-SRCBYOKBN-G
               PERFORM 3101-SRTKBNLST-UBYO-SET-SEC
               MOVE    SPA-GMN-X99-SRTKBNLST (1)
                                       TO  SPA-GMN-X99-SRTKBN-G
               PERFORM 4101-SRCBYOLST-SEC
           END-IF
      *
           IF    ( SPA-GMN-X99-SRCBYOLST-MAX   =   ZERO )
               MOVE    1               TO  SPA-GMN-X99-CUR
           ELSE
               MOVE    2               TO  SPA-GMN-X99-CUR
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    病名ソースコンボ設定処理
           PERFORM 3101-SRCBYOKBNLST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（患者病名）
      *****************************************************************
       3101-SRTKBNLST-PTBYO-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-X99-SRTKBNLST-G
      *
           MOVE    1               TO  SPA-GMN-X99-SRTKBN-MAX
      *
           MOVE    "00"            TO  SPA-GMN-X99-SRTKBNL    (1)
           MOVE    "全科"          TO  SPA-GMN-X99-SRTKBNMEIL (1)
      *
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC    TO  SYSKANRI-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SPA-GMN-X99-SRTKBN-MAX
                                              >=   CONST-SRTKBN-MAX )
      *
               COMPUTE SPA-GMN-X99-SRTKBN-MAX
                   =   SPA-GMN-X99-SRTKBN-MAX  +   1
      *
               MOVE    SYS-1005-KBNCD
                           TO  SPA-GMN-X99-SRTKBNL
                                               (SPA-GMN-X99-SRTKBN-MAX)
      *
               MOVE    SYS-1005-SRYKANAME1
                           TO  SPA-GMN-X99-SRTKBNMEIL
                                               (SPA-GMN-X99-SRTKBN-MAX)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC TO  SYS-1005-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SRTKBNLST-PTBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（病名）
      *****************************************************************
       3101-SRTKBNLST-BYO-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-X99-SRTKBNLST-G
      *
           MOVE    3               TO  SPA-GMN-X99-SRTKBN-MAX
      *
           MOVE    "00"            TO  SPA-GMN-X99-SRTKBNL    (1)
           MOVE    "すべて"        TO  SPA-GMN-X99-SRTKBNMEIL (1)
      *
           MOVE    "01"            TO  SPA-GMN-X99-SRTKBNL    (2)
           MOVE    "傷病名"        TO  SPA-GMN-X99-SRTKBNMEIL (2)
      *
           MOVE    "02"            TO  SPA-GMN-X99-SRTKBNL    (3)
           MOVE    "修飾語"        TO  SPA-GMN-X99-SRTKBNMEIL (3)
      *
           . 
       3101-SRTKBNLST-BYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分設定処理（自院病名）
      *****************************************************************
       3101-SRTKBNLST-UBYO-SET-SEC     SECTION.
      *
           INITIALIZE                  SPA-GMN-X99-SRTKBNLST-G
      *
           MOVE    1               TO  SPA-GMN-X99-SRTKBN-MAX
      *
           MOVE    "000"           TO  SPA-GMN-X99-SRTKBNL    (1)
           MOVE    "すべて"        TO  SPA-GMN-X99-SRTKBNMEIL (1)
      *
           PERFORM 900-USERBYOMEI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( SPA-GMN-X99-SRTKBN-MAX
                                              >=   CONST-SRTKBN-MAX )
      *
               COMPUTE SPA-GMN-X99-SRTKBN-MAX
                   =   SPA-GMN-X99-SRTKBN-MAX  +   1
      *
               MOVE    USBYO-BYOMEIINPUTCD
                           TO  SPA-GMN-X99-SRTKBNL
                                               (SPA-GMN-X99-SRTKBN-MAX)
      *
               MOVE    USBYO-BYOMEI
                           TO  SPA-GMN-X99-SRTKBNMEIL
                                               (SPA-GMN-X99-SRTKBN-MAX)
      *
               PERFORM 900-USERBYOMEI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           . 
       3101-SRTKBNLST-UBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースコンボ設定処理
      *****************************************************************
       3101-SRCBYOKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-X99-SRCBYOKBNLST-G
      *
           MOVE    3               TO  SPA-GMN-X99-SRCBYOKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-X99-SRCBYOKBNL    (1)
           MOVE    "患者病名"      TO  SPA-GMN-X99-SRCBYOKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-X99-SRCBYOKBNL    (2)
           MOVE    "病名"
                                   TO  SPA-GMN-X99-SRCBYOKBNMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-X99-SRCBYOKBNL    (3)
           MOVE    "自院病名"
                                   TO  SPA-GMN-X99-SRCBYOKBNMEIL (3)
      *
           . 
       3101-SRCBYOKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名検索キー編集
      *****************************************************************
       3101-SRTKBNLST-SET-SEC          SECTION.
      *
           IF    ( SPA-X01-INDEX   =   ZERO )
               PERFORM 31011-SRTKBNLST-SET-SEC
           ELSE
               PERFORM 31012-SRTKBNLST-SET-SEC
           END-IF
      *
           .
       3101-SRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名検索キー編集
      *****************************************************************
       31011-SRTKBNLST-SET-SEC         SECTION.
      *
           INITIALIZE              WRK-SEL-BYOMEI-G
           MOVE    SPA-GMN-X99-SEL-BYOMEI
                               TO  WRK-SEL-BYOMEI
      *
           INSPECT WRK-SEL-BYOMEI
                   REPLACING   ALL "＊"    BY  "%%"
      *
           EVALUATE    SPA-GMN-X99-SRCBYOLST-PTN
           WHEN    "1"
               IF    ( SPA-GMN-X99-SRCBYOLST-SRT   =   "00" )
                   MOVE    "%"         TO  WRK-SEL-SRYKA
               ELSE
                   MOVE    SPA-GMN-X99-SRCBYOLST-SRT
                                       TO  WRK-SEL-SRYKA
               END-IF
           WHEN    "2"
               EVALUATE    SPA-GMN-X99-SRCBYOLST-SRT
               WHEN    "01"
                   MOVE    "0000000"   TO  WRK-SEL-STBYOMEICD
                   MOVE    "9999999"   TO  WRK-SEL-EDBYOMEICD
               WHEN    "02"
                   MOVE    "ZZZ0000"   TO  WRK-SEL-STBYOMEICD
                   MOVE    "ZZZ9999"   TO  WRK-SEL-EDBYOMEICD
               WHEN    OTHER
                   MOVE    "0000000"   TO  WRK-SEL-STBYOMEICD
                   MOVE    "ZZZ9999"   TO  WRK-SEL-EDBYOMEICD
               END-EVALUATE
           WHEN   "3"
               IF    ( SPA-GMN-X99-SRCBYOLST-SRT   =   "000" )
                   MOVE    "%"         TO  WRK-SEL-BUNRUICD
               ELSE
                   MOVE    SPA-GMN-X99-SRCBYOLST-SRT
                                       TO  WRK-SEL-BUNRUICD
               END-IF
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-X99-SRCBYOLST-PTN
           WHEN    "1"
               PERFORM 41011-SRCBYOLST-PTBYO-SEC
           WHEN    "2"
               PERFORM 41012-SRCBYOLST-BYO-SEC
           WHEN    "3"
               PERFORM 41013-SRCBYOLST-UBYO-SEC
           END-EVALUATE
      *
           .
       31011-SRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名検索キー編集
      *****************************************************************
       31012-SRTKBNLST-SET-SEC         SECTION.
      *
           INITIALIZE              WRK-SEL-BYOMEI-G
           MOVE    SPA-GMN-X99-SEL-BYOMEI
                               TO  WRK-SEL-BYOMEI
      *
           PERFORM 41013-SRCBYOLST-UBYO-SEC
      *
           .
       31012-SRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *    次頁
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 4101-NEXT-PAGE-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B07S"
               PERFORM 4101-PREV-PAGE-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-X99-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN        ANY         ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN        ANY         ALSO    "SEL_BYOMEI"
               PERFORM 4100-SEL-BYOMEI-CHK-SEC
           WHEN        ANY         ALSO    "SRCBYOLST"
               PERFORM 4101-SRCBYOLST-SEL-SEC
           WHEN    OTHER
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
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
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE   X99-SRCBYOKBN    TO  SPA-GMN-X99-SRCBYOKBN-G
           MOVE   X99-SRTKBN       TO  SPA-GMN-X99-SRTKBN-G
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
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( SPA-X01-INDEX   =   ZERO )
               PERFORM 4101-SEL-BYOMEI-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
           PERFORM 4101-SRCBYOKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( WRK-MCP-WIDGET  =   "SRTKBN" )
            OR   ( WRK-MCP-WIDGET  =   "SRCBYOKBN" )
      *
               PERFORM 4101-SRCBYOLST-SEC
      *
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-X99-CUR
           MOVE    ZERO            TO  SPA-NAI-X99-SELNUM
      *
           MOVE    X99-SELNUM      TO  SPA-GMN-X99-SELNUM
      *
           IF    ( SPA-GMN-X99-SELNUM  =   SPACE )
               MOVE    1           TO  SPA-GMN-X99-CUR
           ELSE
               PERFORM 4101-SELNUM-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-EDIT-BYOMEIINF-SEC
                   PERFORM 210-BACK
               END-IF
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       4100-SEL-BYOMEI-CHK-SEC         SECTION.
      *
           MOVE    X99-SEL-BYOMEI      TO  SPA-GMN-X99-SEL-BYOMEI
      *
           PERFORM 4101-SEL-BYOMEI-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRCBYOLST-SEC
           END-IF
      *
           .
       4100-SEL-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト選択処理
      *****************************************************************
       4101-SRCBYOLST-SEL-SEC          SECTION.
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-X99-SRCBYOLST-MAX )
                    OR   ( FLG-END NOT =   ZERO )
      *
               MOVE    X99-SRCBYOLST-SELECT (IDX-LST)
                               TO  SPA-GMN-X99-SRCBYOLST-SELECT(IDX-LST)
      *
               IF    ( SPA-GMN-X99-SRCBYOLST-SELECT(IDX-LST)
                                   =   "T" )
                   MOVE    IDX-LST TO  WRK-ZZ9
                   MOVE    WRK-ZZ9  TO X99-SELNUM
                   PERFORM 4100-SELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       4101-SRCBYOLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-X99-SELNUM  NOT =   SPACE )
               INITIALIZE                  ORCSNUMAREA
               MOVE    SPA-GMN-X99-SELNUM  TO  SNUM-INX
               CALL    "ORCSNUM"       USING   ORCSNUMAREA
               IF    ( SNUM-RC         =   ZERO )
                AND  ( SNUM-MINKBN     =   SPACE )
                AND  ( SNUM-SYOKBN     =   SPACE ) 
                   IF    ( SNUM-OUTNUM  >=  1 )
                    AND  ( SNUM-OUTNUM  <=  SPA-GMN-X99-SRCBYOLST-MAX )
                       CONTINUE
                   ELSE
                       MOVE    "0001"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-X99-CUR
                   END-IF
               ELSE
                       MOVE    "0001"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-X99-CUR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    SNUM-OUTNUM TO  SPA-NAI-X99-SELNUM
                   MOVE    SNUM-OUTEDT TO  SPA-GMN-X99-SELNUM
               END-IF
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索病名チェック処理
      *****************************************************************
       4101-SEL-BYOMEI-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-X99-SEL-BYOMEI  TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-X99-SEL-BYOMEI
           IF    ( FLG-ERR         NOT =   ZERO )
               MOVE    "0004"          TO SPA-ERRCD
               MOVE    1               TO  SPA-GMN-X99-CUR
               GO  TO  4101-SEL-BYOMEI-CHK-EXT
           END-IF
      *
           .
       4101-SEL-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソート区分チェック処理
      *****************************************************************
       4101-SRTKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-X99-SRTKBN      TO  WRK-CMB-CD
           PERFORM 4200-SRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-X99-SRTKBNLST (1)
                                       TO  SPA-GMN-X99-SRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-X99-SRTKBN-G
           END-IF
      *
           .
       4101-SRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソース区分チェック処理
      *****************************************************************
       4101-SRCBYOKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-X99-SRCBYOKBN   =   SPACE )
               GO  TO  4101-SRCBYOKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-X99-SRCBYOKBN   TO  WRK-CMB-CD
           PERFORM 4200-SRCBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-X99-CUR
               GO  TO  4101-SRCBYOKBN-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-X99-SRCBYOKBN-G
      *
           IF    ( WRK-MCP-WIDGET  =   "SRCBYOKBN" )
      *
      *
               EVALUATE    SPA-GMN-X99-SRCBYOKBN
               WHEN    "1"
                   PERFORM 3101-SRTKBNLST-PTBYO-SET-SEC
               WHEN    "2"
                   PERFORM 3101-SRTKBNLST-BYO-SET-SEC
               WHEN    "3"
                   PERFORM 3101-SRTKBNLST-UBYO-SET-SEC
               END-EVALUATE
      *
               MOVE    SPA-GMN-X99-SRTKBNLST (1)
                                   TO  SPA-GMN-X99-SRTKBN-G
      *
           END-IF
      *
           .
       4101-SRCBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名情報編集処理
      *****************************************************************
       4101-EDIT-BYOMEIINF-SEC         SECTION.
      *
           INITIALIZE                  SPA-X99-AREA
      *
           MOVE    SPA-NAI-X99-SELNUM  TO  IDX1
      *
           MOVE    SPA-GMN-X99-TSRCBYOMEI (IDX1)
                               TO  SPA-X99-BYOMEI
      *
           EVALUATE    SPA-GMN-X99-SRCBYOLST-PTN
           WHEN    "2"
           WHEN    "3"
               PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >       CONST-MKCD-MAX )
      *
                   MOVE    SPA-NAI-X99-TSRCBYOCD (IDX1 IDX0)
                                   TO  SPA-X99-BYOCD (IDX0)
               END-PERFORM
      *
               IF    ( SPA-GMN-X99-SRCBYOLST-PTN   =   "2" )
                   INITIALIZE              ORCSBYOMEIAREA
                   MOVE    "2"     TO      LNK-BYO-SYORI
                   MOVE    SPA-NAI-X99-TSRCBYOCD (IDX1 1)
                                   TO  LNK-BYO-TBYOMEICD (1)
      *
                   CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                                   SPA-AREA
                   MOVE    LNK-BYO-BYOMEIHENFLG
                                   TO  SPA-X99-BYOSTATUS
               ELSE
                   MOVE    SPA-NAI-X99-TSRCBYOSTATUS (IDX1)
                                   TO  SPA-X99-BYOSTATUS
               END-IF
      *
           WHEN    OTHER
      *
      *        病名からコード検索処理
               INITIALIZE                      ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    SPA-X99-BYOMEI      TO  LNK-BYO-BYOMEI
      *
               CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                               SPA-AREA
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >       LNK-BYO-BYOMEI-MAX )
      *
                       MOVE    LNK-BYO-TBYOMEICD (IDX0)
                                   TO  SPA-X99-BYOCD (IDX0)
      *
               END-PERFORM
      *
               MOVE    LNK-BYO-BYOMEIHENFLG
                                   TO  SPA-X99-BYOSTATUS
           END-EVALUATE
      *
           .
       4101-EDIT-BYOMEIINF-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（患者病名より）
      *****************************************************************
       41011-SRCBYOLST-PTBYO-SEC       SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-X99-SRCBYOLST-AREA
                                   SPA-NAI-X99-SRCBYOLST-AREA
      *
           PERFORM 900-PTBYOMEI-KEY26-SEL-SEC
      *
           PERFORM 41011-PTBYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    PTBYO-BYOMEI    TO  SPA-GMN-X99-TSRCBYOMEI (IDX1)
               MOVE    SPACE           TO  SPA-GMN-X99-TSRCCD     (IDX1)
      *
               PERFORM 900-PTBYOMEI-KEY26-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDX1            TO  SPA-GMN-X99-SRCBYOLST-MAX
      *
           IF    ( FLG-PTBYOMEI    =   ZERO )
               MOVE    1           TO  SPA-GMN-X99-SRCBYOLST-NEXT
           END-IF
      *
           .
       41011-SRCBYOLST-PTBYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41011-PTBYO-SKIP-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-X99-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST      >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-PTBYOMEI-KEY26-FET-SEC
      *
           END-PERFORM
      *
           .
       41011-PTBYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（病名より）
      *****************************************************************
       41012-SRCBYOLST-BYO-SEC         SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-X99-SRCBYOLST-AREA
                                   SPA-NAI-X99-SRCBYOLST-AREA
      *
           PERFORM 900-BYOMEI-KEY7-SEL-SEC
      *
           PERFORM 41012-BYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-BYOMEI  NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    BYO-BYOMEI      TO  SPA-GMN-X99-TSRCBYOMEI(IDX1)
               MOVE    BYO-BYOMEICD    TO  SPA-GMN-X99-TSRCCD     (IDX1)
                                           SPA-NAI-X99-TSRCBYOCD(IDX1 1)
      *
               PERFORM 900-BYOMEI-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDX1            TO  SPA-GMN-X99-SRCBYOLST-MAX
      *
           IF    ( FLG-BYOMEI      =   ZERO )
               MOVE    1           TO  SPA-GMN-X99-SRCBYOLST-NEXT
           END-IF
      *
           .
       41012-SRCBYOLST-BYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41012-BYO-SKIP-SEC    SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-X99-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-BYOMEI      NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST      >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-BYOMEI-KEY7-FET-SEC
      *
           END-PERFORM
      *
           .
       41012-BYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリスト編集処理（自院病名より）
      *****************************************************************
       41013-SRCBYOLST-UBYO-SEC        SECTION.
      *
           MOVE    1               TO  FLG-SRCBYOLST-ROW
      *
           INITIALIZE              SPA-GMN-X99-SRCBYOLST-AREA
                                   SPA-NAI-X99-SRCBYOLST-AREA
      *
           IF    ( SPA-X01-INDEX   =   ZERO )
      *
               INITIALIZE              USERBYOMEI-REC
               MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
      *
               MOVE    WRK-SEL-BUNRUICD
                                       TO  USBYO-BUNRUICD
      *
               STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                       "%"             DELIMITED   BY  SIZE
               INTO    USBYO-BYOMEI
               END-STRING
               MOVE    "tbl_userbyomei"    TO  WRK-TABLE
               MOVE    "key10"             TO  WRK-PATHNAME
           ELSE
      *
               INITIALIZE              USERBYOMEI-REC
               MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
      *
               STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                       "%"             DELIMITED   BY  SIZE
               INTO    USBYO-BYOMEIINPUTCD
               END-STRING
               MOVE    "tbl_userbyomei"    TO  WRK-TABLE
               MOVE    "key11"             TO  WRK-PATHNAME
           END-IF
      *
           PERFORM 900-USERBYOMEI-SEL-SEC
      *
           PERFORM 41013-UBYO-SKIP-SEC
      *
           MOVE    ZERO            TO  IDX1
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( IDX1    >=  CONST-SRCBYOLST-MAX )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    USBYO-BYOMEI    TO  SPA-GMN-X99-TSRCBYOMEI (IDX1)
               MOVE    USBYO-BYOMEIINPUTCD
                                       TO  SPA-GMN-X99-TSRCCD     (IDX1)
               MOVE    USBYO-BYOMEIHENFLG
                                       TO  SPA-NAI-X99-TSRCBYOSTATUS
                                                                  (IDX1)
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-MKCD-MAX )
                        OR   ( USBYO-BYOMEICD (IDX0)   =   "9999999" )
                        OR   ( USBYO-BYOMEICD (IDX0)   =   SPACE )
      *
                   MOVE    USBYO-BYOMEICD (IDX0)
                                   TO  SPA-NAI-X99-TSRCBYOCD(IDX1 IDX0)
      *
               END-PERFORM
      *
               PERFORM 900-USERBYOMEI-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    IDX1            TO  SPA-GMN-X99-SRCBYOLST-MAX
      *
           IF    ( FLG-USERBYOMEI  =   ZERO )
               MOVE    1           TO  SPA-GMN-X99-SRCBYOLST-NEXT
           END-IF
      *
           .
       41013-SRCBYOLST-UBYO-EXT.
           EXIT.
      *****************************************************************
      *    病名ソースリストスキップ編集処理
      *****************************************************************
       41013-UBYO-SKIP-SEC  SECTION.
      *
           MOVE    ZERO            TO  CNT-SRCBYOLST
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-GMN-X99-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           PERFORM UNTIL ( FLG-USERBYOMEI  NOT =   ZERO )
                    OR   ( CNT-SRCBYOLST      >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-SRCBYOLST   =   CNT-SRCBYOLST   +   1
      *
               PERFORM 900-USERBYOMEI-FET-SEC
      *
           END-PERFORM
      *
           .
       41013-UBYO-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    ソース病名リスト編集処理
      *****************************************************************
       4101-SRCBYOLST-SEC              SECTION.
      *
           MOVE    1           TO  SPA-GMN-X99-SRCBYOLST-PAGE
      *
           MOVE    SPA-GMN-X99-SRCBYOKBN
                               TO  SPA-GMN-X99-SRCBYOLST-PTN
           MOVE    SPA-GMN-X99-SRTKBN
                               TO  SPA-GMN-X99-SRCBYOLST-SRT
      *
           PERFORM 3101-SRTKBNLST-SET-SEC
      *
           .
       4101-SRCBYOLST-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       4101-PREV-PAGE-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4101-PREV-PAGE-EXT
           END-IF
           IF    ( SPA-GMN-X99-SRCBYOLST-PAGE  >   1 )
               COMPUTE SPA-GMN-X99-SRCBYOLST-PAGE
                   =   SPA-GMN-X99-SRCBYOLST-PAGE  -   1
      *
               PERFORM 3101-SRTKBNLST-SET-SEC
      *
           END-IF
      *
           .
       4101-PREV-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    後頁処理
      *****************************************************************
       4101-NEXT-PAGE-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4101-NEXT-PAGE-EXT
           END-IF
      *
           IF    ( SPA-GMN-X99-SRCBYOLST-NEXT  =   1 )
               COMPUTE SPA-GMN-X99-SRCBYOLST-PAGE
                   =   SPA-GMN-X99-SRCBYOLST-PAGE  +   1
      *
               PERFORM 3101-SRTKBNLST-SET-SEC
      *
           END-IF
      *
           .
       4101-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    ソート区分コンボチェック処理
      *****************************************************************
       4200-SRTKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-X99-SRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-X99-SRCBYOLST-PTN   =   "3" )
      *
                   IF    ( SPA-GMN-X99-SRTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                       MOVE    SPA-GMN-X99-SRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   END-IF
               ELSE
                   IF    ( SPA-GMN-X99-SRTKBNL (IDX-CMB)(1:2)
                                   =   WRK-CMB-CD (1:2) )
      *
                       MOVE    SPA-GMN-X99-SRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名ソース区分コンボチェック処理
      *****************************************************************
       4200-SRCBYOKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-X99-SRCBYOKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT
                                       =   SPACE )
      *
               IF    ( SPA-GMN-X99-SRCBYOKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-X99-SRCBYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRCBYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-X99-MOTOPG      TO  SPA-SAKIPG
           MOVE    "X99"               TO  SPA-MOTOPG
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
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
           PERFORM 4100-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
           IF    ( SPA-GMN-X99-SELNUM  =   SPACE )
      *
               MOVE    "1001"          TO  SPA-ERRCD
      *
               IF    ( SPA-GMN-X99-SRCBYOLST-MAX   =   ZERO )
                   MOVE    1          TO   SPA-GMN-X99-CUR
               ELSE
                   MOVE    2          TO   SPA-GMN-X99-CUR
               END-IF
           END-IF
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X99    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE   X99-SRCBYOLST-ROW        TO  WRK-SRCBYOLST-ROW
      *
      *
           INITIALIZE                          X99
      *
           IF    ( FLG-SRCBYOLST-ROW   =   1 )
               MOVE    ZERO                TO  X99-SRCBYOLST-ROW
               MOVE    ZERO                TO  X99-SRCBYOLST-ROWATTR
           ELSE
               MOVE    WRK-SRCBYOLST-ROW   TO  X99-SRCBYOLST-ROW
               MOVE    ZERO                TO  X99-SRCBYOLST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-X99-SELNUM          TO  X99-SELNUM
           MOVE    SPA-GMN-X99-SEL-BYOMEI      TO  X99-SEL-BYOMEI
           MOVE    SPA-GMN-X99-SRTKBN-G        TO  X99-SRTKBN
           MOVE    SPA-GMN-X99-SRCBYOKBN-G     TO  X99-SRCBYOKBN
      *
           MOVE    SPA-GMN-X99-SRTKBN-MAX  TO  X99-SRTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-X99-SRTKBN-MAX )
      *
               MOVE    SPA-GMN-X99-SRTKBNLST (IDX0)
                                       TO  X99-SRTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-X99-SRCBYOKBN-MAX
                                       TO  X99-SRCBYOKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-X99-SRCBYOKBN-MAX )
      *
               MOVE    SPA-GMN-X99-SRCBYOKBNLST (IDX0)
                                       TO  X99-SRCBYOKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           EVALUATE    SPA-GMN-X99-SRCBYOLST-PTN
           WHEN    "2"
               MOVE    "コード"        TO  X99-LBLSRCBYOCD
           WHEN    "3"
               MOVE    "入力コード"    TO  X99-LBLSRCBYOCD
           WHEN    OTHER
               MOVE    SPACE           TO  X99-LBLSRCBYOCD
           END-EVALUATE
      *
           COMPUTE WRK-NO
               = ( SPA-GMN-X99-SRCBYOLST-PAGE  -   1 )
               *   CONST-SRCBYOLST-MAX
      *
           MOVE    SPA-GMN-X99-SRCBYOLST-MAX   TO  X99-SRCBYOLST-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-X99-SRCBYOLST-MAX )
      *
               COMPUTE WRK-NO  =   WRK-NO  +   1
      *
               MOVE    WRK-NO          TO  WRK-ZZ9
      *
               MOVE    WRK-ZZ9         TO  X99-TSRCNO (IDX0)
               MOVE    SPA-GMN-X99-TSRCBYOMEI (IDX0)
                                       TO  X99-TSRCBYOMEI(IDX0)
      *
               MOVE    SPA-GMN-X99-TSRCCD (IDX0)
                                       TO  X99-TSRCCD (IDX0)
      *
               MOVE    SPA-GMN-X99-SRCBYOLST-SELECT (IDX0)
                                   TO  X99-SRCBYOLST-SELECT (IDX0)
      *
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL       TO  X99-B07-STATE
                                           X99-B07S-STATE
                                           X99-SEL-BYOMEI-STATE
                                           X99-SRCBYOKBN-STATE
                                           X99-SRCBYOKBN-ITEM-STATE
                                           X99-SRTKBN-STATE
                                           X99-SRTKBN-ITEM-STATE
      *
           IF    ( SPA-GMN-X99-SRCBYOLST-PAGE  <=  1 )
               MOVE    WIDGET-INSENSITIVE  TO  X99-B07-STATE
           END-IF
      *
           IF    ( SPA-GMN-X99-SRCBYOLST-NEXT   =  ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X99-B07S-STATE
           END-IF
      *
           IF    ( SPA-X01-INDEX   NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  X99-SEL-BYOMEI-STATE
                                           X99-SRCBYOKBN-STATE
                                           X99-SRCBYOKBN-ITEM-STATE
                                           X99-SRTKBN-STATE
                                           X99-SRTKBN-ITEM-STATE
           END-IF
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
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-X99-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           IF    ( SPA-GMN-X99-CUR     =   1 )
               IF    ( SPA-GMN-X99-SRCBYOLST-MAX   =   ZERO )
                   MOVE    201         TO  SPA-GMN-X99-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-X99-CUR
           WHEN    001
               MOVE    "SEL_BYOMEI"    TO  MCP-WIDGET
           WHEN    002
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    003
               MOVE    "SRCBYOKBN"     TO  MCP-WIDGET
           WHEN    004
               MOVE    "SRTKBN"        TO  MCP-WIDGET
           WHEN    201
               MOVE    "B01"           TO  MCP-WIDGET
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
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SEL_BYOMEI"
               MOVE    2               TO  SPA-GMN-X99-CUR
           WHEN    "SELNUM"
               MOVE    2               TO  SPA-GMN-X99-CUR
           WHEN    "SRCBYOKBN"
               MOVE    4               TO  SPA-GMN-X99-CUR
           WHEN    "SRTKBN"
               MOVE    2               TO  SPA-GMN-X99-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
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
           WHEN    "0003"
               MOVE    "検索対象マスタ入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "検索病名入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "病名が選択されていません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X99"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
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
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者病名検索処理(KEY26)
      *****************************************************************
       900-PTBYOMEI-KEY26-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-PTBYOMEI
      *
           INITIALIZE              PTBYOMEI-REC
      *
           MOVE    SPA-SYSYMD      TO  WRK-YMD
           MOVE    "2"             TO  WRK-ZOGENPTN
           MOVE    -3              TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-KEISANYMD   TO  PTBYO-SRYYMD
      *
           MOVE    SPA-SYSYMD      TO  PTBYO-UPYMD
      *
           MOVE    WRK-SEL-SRYKA   TO  PTBYO-SRYKA
      *
           STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    PTBYO-BYOMEI
           END-STRING
      *
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key26"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           .
      *
       900-PTBYOMEI-KEY26-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名FETCH処理(KEY26)
      *****************************************************************
       900-PTBYOMEI-KEY26-FET-SEC      SECTION.
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key26"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           .
      *
       900-PTBYOMEI-KEY26-FET-EXT.
           EXIT.
      *****************************************************************
      *    病名検索処理(KEY7)
      *****************************************************************
       900-BYOMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BYOMEI
      *
           INITIALIZE              BYOMEI-REC
      *
           STRING  WRK-SEL-BYOMEI  DELIMITED   BY  SPACE
                   "%"             DELIMITED   BY  SIZE
           INTO    BYO-BYOMEI
           END-STRING
      *
           MOVE    WRK-SEL-STBYOMEICD  TO  BYO-BYOMEICD
           MOVE    WRK-SEL-EDBYOMEICD  TO  BYO-IKOSAKICD
      *
           MOVE    BYO-BYOMEI      TO  BYO-BYOMEIKANA
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_byomei"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               PERFORM 900-BYOMEI-KEY7-FET-SEC
           END-IF
      *
           .
      *
       900-BYOMEI-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名FETCH処理(KEY7)
      *****************************************************************
       900-BYOMEI-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-BYOMEI      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "tbl_byomei"    TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC    =     ZERO )
                   MOVE    MCPDATA-REC TO  BYOMEI-REC
      *
                   IF    ( BYO-BYOMEICD    NOT =   "0000999" )
                       MOVE    1   TO  FLG-LOOP-END
                   END-IF
      *
               ELSE
                   INITIALIZE              BYOMEI-REC
                   MOVE    1           TO  FLG-BYOMEI
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-BYOMEI-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY4)
      *****************************************************************
       900-USERBYOMEI-KEY4-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-USERBYOMEI
      *
           INITIALIZE              USERBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
      *
           MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名FETCH処理(KEY4)
      *****************************************************************
       900-USERBYOMEI-KEY4-FET-SEC    SECTION.
      *
           MOVE    "tbl_userbyomei"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY10)
      *****************************************************************
       900-USERBYOMEI-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-USERBYOMEI
      *
           MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名FETCH処理(KEY10)
      *****************************************************************
       900-USERBYOMEI-FET-SEC    SECTION.
      *
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  USERBYOMEI-REC
           ELSE
               INITIALIZE              USERBYOMEI-REC
               MOVE    1           TO  FLG-USERBYOMEI
           END-IF
      *
           .
      *
       900-USERBYOMEI-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
