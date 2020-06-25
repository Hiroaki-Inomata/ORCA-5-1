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
       PROGRAM-ID.                               ORCGW67.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 出産費用内訳情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/12/04    NACL−太田    新規作成
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
       01  SPA-W67.
           03  SPA-GMN-CUR                         PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-LBLNETCJIHI             PIC X(20)
                                                   OCCURS  10.
               05  SPA-GMN-LBLGETCJIHI             PIC X(20)
                                                   OCCURS  10.
               05  SPA-GMN-JIHILINK-AREA.
                   07  SPA-GMN-MEISAIKBN-G         OCCURS  16.
                       09  SPA-GMN-MEISAIKBN       PIC X(02).
                       09  SPA-GMN-MEISAIKBNF      PIC X(01).
                       09  SPA-GMN-MEISAIKBNMEI    PIC X(20).
                   07  SPA-GMN-NETCJIHIKBN-G       OCCURS  10.
                       09  SPA-GMN-NETCJIHIKBN     PIC X(02).
                       09  SPA-GMN-NETCJIHIKBNF    PIC X(01).
                       09  SPA-GMN-NETCJIHIKBNMEI  PIC X(20).
                   07  SPA-GMN-GETCJIHIKBN-G       OCCURS  10.
                       09  SPA-GMN-GETCJIHIKBN     PIC X(02).
                       09  SPA-GMN-GETCJIHIKBNF    PIC X(01).
                       09  SPA-GMN-GETCJIHIKBNMEI  PIC X(20).
           03  SPA-GMN-COMBO-AREA.
               05  SPA-GMN-MEISAIKBNLST-G.
                   07  SPA-GMN-MEISAIKBN-MAX       PIC 9(03).
                   07  SPA-GMN-MEISAIKBNLST        OCCURS  20.
                       09  SPA-GMN-MEISAIKBNL      PIC X(02).
                       09  SPA-GMN-MEISAIKBNFL     PIC X(01).
                       09  SPA-GMN-MEISAIKBNMEIL   PIC X(20).
               05  SPA-GMN-NETCJIHIKBNLST-G.
                   07  SPA-GMN-NETCJIHIKBN-MAX      PIC 9(03).
                   07  SPA-GMN-NETCJIHIKBNLST       OCCURS  20.
                       09  SPA-GMN-NETCJIHIKBNL     PIC X(02).
                       09  SPA-GMN-NETCJIHIKBNFL    PIC X(01).
                       09  SPA-GMN-NETCJIHIKBNMEIL  PIC X(20).
               05  SPA-GMN-GETCJIHIKBNLST-G.
                   07  SPA-GMN-GETCJIHIKBN-MAX      PIC 9(03).
                   07  SPA-GMN-GETCJIHIKBNLST       OCCURS  20.
                       09  SPA-GMN-GETCJIHIKBNL     PIC X(02).
                       09  SPA-GMN-GETCJIHIKBNFL    PIC X(01).
                       09  SPA-GMN-GETCJIHIKBNMEIL  PIC X(20).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-WIDMSG                          PIC X(200).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-ETCJIHICD                       PIC 9(02).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
           03  WRK-ERR-CUR                         PIC 9(03).
      *
       01  CONST-AREA.
           03  CONST-MEISAI-MAX        PIC 9(03)   VALUE 16.
           03  CONST-NETCJIHI-MAX      PIC 9(03)   VALUE 10.
           03  CONST-GETCJIHI-MAX      PIC 9(03)   VALUE 10.
           03  CONST-LBLMEISAI-VAL.
               05  CONST-LBLMEISAI01   PIC X(20)   VALUE "初・再診料".
               05  CONST-LBLMEISAI02   PIC X(20)   VALUE "医学管理等".
               05  CONST-LBLMEISAI03   PIC X(20)   VALUE "在宅医療".
               05  CONST-LBLMEISAI04   PIC X(20)   VALUE "投　　薬".
               05  CONST-LBLMEISAI05   PIC X(20)   VALUE "注　　射".
               05  CONST-LBLMEISAI06   PIC X(20)   VALUE "処　　置".
               05  CONST-LBLMEISAI07   PIC X(20)   VALUE "手　　術".
               05  CONST-LBLMEISAI08   PIC X(20)   VALUE "麻　　酔".
               05  CONST-LBLMEISAI09   PIC X(20)   VALUE "検　　査".
               05  CONST-LBLMEISAI10   PIC X(20)   VALUE "画像診断".
               05  CONST-LBLMEISAI11   PIC X(20)   VALUE "リハビリ".
               05  CONST-LBLMEISAI12   PIC X(20)   VALUE "精神科専門".
               05  CONST-LBLMEISAI13   PIC X(20)   VALUE "放射線治療".
               05  CONST-LBLMEISAI14   PIC X(20)   VALUE "病理診断".
               05  CONST-LBLMEISAI15   PIC X(20)   VALUE "入院料等".
               05  CONST-LBLMEISAI16   PIC X(20)   VALUE "療養担当手当".
           03  CONST-LBLMEISAI-R REDEFINES CONST-LBLMEISAI-VAL.
               05  CONST-LBLMEISAI     PIC X(20)   OCCURS 16.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    自費名称
           COPY  "CPSK1013.INC".
      *
      *    出産費用内訳情報
           COPY  "CPSK1046.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
           MOVE    SPA-FREE        TO  SPA-W67
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W67         TO  SPA-FREE
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
           MOVE    "W67    "           TO  MCP-WINDOW.
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
       300-SCREEN-SEC                  SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               MOVE    SPACE           TO  SPA-WID1-FLG
               MOVE    SPACE           TO  SPA-WIDCD
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
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W67
      *
      *    画面初期化処理
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 3100-COMBO-EDIT-SEC
      *
      *    出産費用内訳情報
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1046"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "1046"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
               PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           END-IF
           MOVE    SYSKANRI-REC        TO  SYS-1046-REC
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MEISAI-MAX )
               IF    ( SYS-1046-MEISAIKBN (IDX1)
                                   =   SPACE )
                   MOVE    SPA-GMN-MEISAIKBNLST (9)
                                       TO  SPA-GMN-MEISAIKBN-G (IDX1)
               ELSE
                   MOVE    SYS-1046-MEISAIKBN (IDX1)
                                       TO  WRK-CMB-CD
                   PERFORM 4200-MEISAIKBNLST-CHK-SEC
                   IF    ( WRK-CMB-ITM     =   SPACE )
                       MOVE    WRK-CMB-CD  TO  SPA-GMN-MEISAIKBN-G(IDX1)
                   ELSE
                       MOVE    WRK-CMB-ITM
                                           TO  SPA-GMN-MEISAIKBN-G(IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
      *
               IF    ( SPA-GMN-LBLNETCJIHI (IDX1)   =   SPACE )
                   IF    ( SYS-1046-ETCJIHIKBN (IDX1)
                                   =   SPACE OR "09" )
                       CONTINUE
                   ELSE
                       MOVE    SYS-1046-ETCJIHIKBN (IDX1)
                                       TO  SPA-GMN-NETCJIHIKBN-G (IDX1)
                   END-IF
               ELSE
                   IF    ( SYS-1046-ETCJIHIKBN (IDX1)
                                   =   SPACE )
                       MOVE    SPA-GMN-NETCJIHIKBNLST (9)
                                       TO  SPA-GMN-NETCJIHIKBN-G (IDX1)
                   ELSE
                       MOVE    SYS-1046-ETCJIHIKBN (IDX1)
                                       TO  WRK-CMB-CD
                       PERFORM 4200-NETCJIHIKBNLST-CHK-SEC
                       IF    ( WRK-CMB-ITM     =   SPACE )
                           MOVE    WRK-CMB-CD  TO  SPA-GMN-NETCJIHIKBN-G
                                                                (IDX1)
                       ELSE
                           MOVE    WRK-CMB-ITM TO  SPA-GMN-NETCJIHIKBN-G
                                                                (IDX1)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
      *
               IF    ( SPA-GMN-LBLGETCJIHI (IDX1)   =   SPACE )
                   IF    ( SYS-1046-GETCJIHIKBN (IDX1)
                                   =   SPACE OR "09" )
                       CONTINUE
                   ELSE
                       MOVE    SYS-1046-GETCJIHIKBN (IDX1)
                                       TO  SPA-GMN-GETCJIHIKBN-G (IDX1)
                   END-IF
               ELSE
                   IF    ( SYS-1046-GETCJIHIKBN (IDX1)
                                   =   SPACE )
                       MOVE    SPA-GMN-GETCJIHIKBNLST (9)
                                       TO  SPA-GMN-GETCJIHIKBN-G (IDX1)
                   ELSE
                       MOVE    SYS-1046-GETCJIHIKBN (IDX1)
                                       TO  WRK-CMB-CD
                       PERFORM 4200-GETCJIHIKBNLST-CHK-SEC
                       IF    ( WRK-CMB-ITM     =   SPACE )
                           MOVE    WRK-CMB-CD  TO  SPA-GMN-GETCJIHIKBN-G
                                                                (IDX1)
                       ELSE
                           MOVE    WRK-CMB-ITM TO  SPA-GMN-GETCJIHIKBN-G
                                                                (IDX1)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       3100-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-MEISAIKBNLST-SEC
      *
           PERFORM 3101-NETCJIHIKBNLST-SEC
      *
           PERFORM 3101-GETCJIHIKBNLST-SEC
      *
           .
       3100-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    明細区分コンボ編集処理
      *****************************************************************
       3101-MEISAIKBNLST-SEC             SECTION.
      *
           INITIALIZE              SPA-GMN-MEISAIKBNLST-G
      *
           MOVE    "01"        TO  SPA-GMN-MEISAIKBNL   (1)
           MOVE    "入院料"    TO  SPA-GMN-MEISAIKBNMEIL(1)
      *
           MOVE    "02"        TO  SPA-GMN-MEISAIKBNL   (2)
           MOVE    "室料差額"  TO  SPA-GMN-MEISAIKBNMEIL(2)
      *
           MOVE    "03"        TO  SPA-GMN-MEISAIKBNL   (3)
           MOVE    "分娩介助料"
                               TO SPA-GMN-MEISAIKBNMEIL(3)
      *
           MOVE    "04"        TO  SPA-GMN-MEISAIKBNL   (4)
           MOVE    "分娩料"    TO  SPA-GMN-MEISAIKBNMEIL(4)
      *
           MOVE    "05"        TO  SPA-GMN-MEISAIKBNL   (5)
           MOVE    "新生児管理保育料"
                               TO  SPA-GMN-MEISAIKBNMEIL(5)
      *
           MOVE    "06"        TO  SPA-GMN-MEISAIKBNL   (6)
           MOVE    "検査・薬剤料"
                               TO  SPA-GMN-MEISAIKBNMEIL(6)
      *
           MOVE    "07"        TO  SPA-GMN-MEISAIKBNL   (7)
           MOVE    "処置・手当料"
                               TO  SPA-GMN-MEISAIKBNMEIL(7)
      *
           MOVE    "08"        TO  SPA-GMN-MEISAIKBNL   (8)
           MOVE    "産科医療補償制度"
                               TO  SPA-GMN-MEISAIKBNMEIL(8)
      *
           MOVE    "09"        TO  SPA-GMN-MEISAIKBNL   (9)
           MOVE    "その他"    TO  SPA-GMN-MEISAIKBNMEIL(9)
      *
           MOVE    9           TO  SPA-GMN-MEISAIKBN-MAX
      *
           .
       3100-MEISAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    その他自費区分（入院）コンボ編集処理
      *****************************************************************
       3101-NETCJIHIKBNLST-SEC             SECTION.
      *
           INITIALIZE              SPA-GMN-NETCJIHIKBNLST-G
      *
           MOVE    "01"        TO  SPA-GMN-NETCJIHIKBNL   (1)
           MOVE    "入院料"    TO  SPA-GMN-NETCJIHIKBNMEIL(1)
      *
           MOVE    "02"        TO  SPA-GMN-NETCJIHIKBNL   (2)
           MOVE    "室料差額"  TO  SPA-GMN-NETCJIHIKBNMEIL(2)
      *
           MOVE    "03"        TO  SPA-GMN-NETCJIHIKBNL   (3)
           MOVE    "分娩介助料"
                               TO SPA-GMN-NETCJIHIKBNMEIL(3)
      *
           MOVE    "04"        TO  SPA-GMN-NETCJIHIKBNL   (4)
           MOVE    "分娩料"    TO  SPA-GMN-NETCJIHIKBNMEIL(4)
      *
           MOVE    "05"        TO  SPA-GMN-NETCJIHIKBNL   (5)
           MOVE    "新生児管理保育料"
                               TO  SPA-GMN-NETCJIHIKBNMEIL(5)
      *
           MOVE    "06"        TO  SPA-GMN-NETCJIHIKBNL   (6)
           MOVE    "検査・薬剤料"
                               TO  SPA-GMN-NETCJIHIKBNMEIL(6)
      *
           MOVE    "07"        TO  SPA-GMN-NETCJIHIKBNL   (7)
           MOVE    "処置・手当料"
                               TO  SPA-GMN-NETCJIHIKBNMEIL(7)
      *
           MOVE    "08"        TO  SPA-GMN-NETCJIHIKBNL   (8)
           MOVE    "産科医療補償制度"
                               TO  SPA-GMN-NETCJIHIKBNMEIL(8)
      *
           MOVE    "09"        TO  SPA-GMN-NETCJIHIKBNL   (9)
           MOVE    "その他"    TO  SPA-GMN-NETCJIHIKBNMEIL(9)
      *
           MOVE    9           TO  SPA-GMN-NETCJIHIKBN-MAX
      *
           .
       3100-NETCJIHIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    その他自費区分（入院）コンボ編集処理
      *****************************************************************
       3101-GETCJIHIKBNLST-SEC             SECTION.
      *
           INITIALIZE              SPA-GMN-GETCJIHIKBNLST-G
      *
           MOVE    "01"        TO  SPA-GMN-GETCJIHIKBNL   (1)
           MOVE    "入院料"    TO  SPA-GMN-GETCJIHIKBNMEIL(1)
      *
           MOVE    "02"        TO  SPA-GMN-GETCJIHIKBNL   (2)
           MOVE    "室料差額"  TO  SPA-GMN-GETCJIHIKBNMEIL(2)
      *
           MOVE    "03"        TO  SPA-GMN-GETCJIHIKBNL   (3)
           MOVE    "分娩介助料"
                               TO SPA-GMN-GETCJIHIKBNMEIL(3)
      *
           MOVE    "04"        TO  SPA-GMN-GETCJIHIKBNL   (4)
           MOVE    "分娩料"    TO  SPA-GMN-GETCJIHIKBNMEIL(4)
      *
           MOVE    "05"        TO  SPA-GMN-GETCJIHIKBNL   (5)
           MOVE    "新生児管理保育料"
                               TO  SPA-GMN-GETCJIHIKBNMEIL(5)
      *
           MOVE    "06"        TO  SPA-GMN-GETCJIHIKBNL   (6)
           MOVE    "検査・薬剤料"
                               TO  SPA-GMN-GETCJIHIKBNMEIL(6)
      *
           MOVE    "07"        TO  SPA-GMN-GETCJIHIKBNL   (7)
           MOVE    "処置・手当料"
                               TO  SPA-GMN-GETCJIHIKBNMEIL(7)
      *
           MOVE    "08"        TO  SPA-GMN-GETCJIHIKBNL   (8)
           MOVE    "産科医療補償制度"
                               TO  SPA-GMN-GETCJIHIKBNMEIL(8)
      *
           MOVE    "09"        TO  SPA-GMN-GETCJIHIKBNL   (9)
           MOVE    "その他"    TO  SPA-GMN-GETCJIHIKBNMEIL(9)
      *
           MOVE    9           TO  SPA-GMN-GETCJIHIKBN-MAX
      *
           .
       3100-GETCJIHIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 4801-TOUROKU-SEC
               WHEN "3002"
                   PERFORM 4301-DEL-SEC
               END-EVALUATE
           END-IF
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 430-DEL-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 480-TOUROKU-SEC
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
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MEISAI-MAX )
               MOVE    W67-MEISAIKBN (IDX1)
                                       TO  SPA-GMN-MEISAIKBN-G  (IDX1)
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
               MOVE    W67-NETCJIHIKBN (IDX1)
                                       TO  SPA-GMN-NETCJIHIKBN-G  (IDX1)
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
               MOVE    W67-GETCJIHIKBN (IDX1)
                                       TO  SPA-GMN-GETCJIHIKBN-G  (IDX1)
           END-PERFORM
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MEISAI-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 4101-MEISAIKBN-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 4101-NETCJIHIKBN-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 4101-GETCJIHIKBN-CHK-SEC
           END-PERFORM
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細区分チェック処理
      *****************************************************************
       4101-MEISAIKBN-CHK-SEC          SECTION.
      *
           COMPUTE WRK-ERR-CUR =   IDX1
      *
           IF    ( SPA-GMN-MEISAIKBN (IDX1)    =   SPACE )
               MOVE    SPA-GMN-MEISAIKBNLST (9)
                                       TO  SPA-GMN-MEISAIKBN-G (IDX1)
           END-IF
      *
           MOVE    SPA-GMN-MEISAIKBN (IDX1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-MEISAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-MEISAIKBN-G (IDX1)
           END-IF
      *
           .
       4101-MEISAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細区分コンボボックスチェック処理
      *****************************************************************
       4200-MEISAIKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MEISAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-MEISAIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-MEISAIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-MEISAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他自費区分（入院）チェック処理
      *****************************************************************
       4101-NETCJIHIKBN-CHK-SEC        SECTION.
      *
           COMPUTE WRK-ERR-CUR =   IDX1    +    20
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-LBLNETCJIHI (IDX1)           =   SPACE )
               IF    ( SPA-GMN-NETCJIHIKBN (IDX1)   NOT =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
               END-IF
           WHEN  ( SPA-GMN-NETCJIHIKBN (IDX1)    =   SPACE )
               MOVE    SPA-GMN-NETCJIHIKBNLST (9)
                                       TO  SPA-GMN-NETCJIHIKBN-G (IDX1)
           WHEN    OTHER
      *
               MOVE    SPA-GMN-NETCJIHIKBN (IDX1)
                                       TO  WRK-CMB-CD
               PERFORM 4200-NETCJIHIKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-NETCJIHIKBN-G (IDX1)
               END-IF
           END-EVALUATE
      *
           .
       4101-NETCJIHIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他自費区分（外来）チェック処理
      *****************************************************************
       4101-GETCJIHIKBN-CHK-SEC        SECTION.
      *
           COMPUTE WRK-ERR-CUR =   IDX1    +    30
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-LBLGETCJIHI (IDX1)           =   SPACE )
               IF    ( SPA-GMN-GETCJIHIKBN (IDX1)   NOT =   SPACE )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
               END-IF
           WHEN  ( SPA-GMN-GETCJIHIKBN (IDX1)    =   SPACE )
               MOVE    SPA-GMN-GETCJIHIKBNLST (9)
                                       TO  SPA-GMN-GETCJIHIKBN-G (IDX1)
           WHEN    OTHER
      *
               MOVE    SPA-GMN-GETCJIHIKBN (IDX1)
                                       TO  WRK-CMB-CD
               PERFORM 4200-GETCJIHIKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-GETCJIHIKBN-G (IDX1)
               END-IF
           END-EVALUATE
      *
           .
       4101-GETCJIHIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他自費区分（入院）コンボボックスチェック処理
      *****************************************************************
       4200-NETCJIHIKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NETCJIHIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NETCJIHIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NETCJIHIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NETCJIHIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他自費区分（外来）コンボボックスチェック処理
      *****************************************************************
       4200-GETCJIHIKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-GETCJIHIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-GETCJIHIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-GETCJIHIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-GETCJIHIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W67"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "W01"               TO  MCP-WINDOW
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
           INITIALIZE  SPA-GMN-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1013"          TO  SYS-KANRICD
           MOVE    "%"             TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD TO SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1013-REC
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               EVALUATE    SYS-1013-KBNCD
               WHEN    "10"
                   MOVE    SYS-1013-JIHINAME
                                   TO  SPA-GMN-LBLNETCJIHI (10)
               WHEN    "11"        THRU    "19"
                   MOVE    SYS-1013-KBNCD
                                   TO  WRK-ETCJIHICD
                   MOVE    SYS-1013-JIHINAME
                                   TO  SPA-GMN-LBLNETCJIHI
                                         (WRK-ETCJIHICD - 10)
               WHEN    "20"
                   MOVE    SYS-1013-JIHINAME
                                   TO  SPA-GMN-LBLGETCJIHI (10)
               WHEN    "21"        THRU    "29"
                   MOVE    SYS-1013-KBNCD
                                   TO  WRK-ETCJIHICD
                   MOVE    SYS-1013-JIHINAME
                                   TO  SPA-GMN-LBLGETCJIHI
                                         (WRK-ETCJIHICD - 20)
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1013-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MEISAI-MAX )
      *
               MOVE    SPA-GMN-MEISAIKBNLST (9)
                                   TO  SPA-GMN-MEISAIKBN-G  (IDX1)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
      *
               IF    ( SPA-GMN-LBLNETCJIHI (IDX1)   NOT =   SPACE )
                   MOVE    SPA-GMN-NETCJIHIKBNLST (9)
                                   TO  SPA-GMN-NETCJIHIKBN-G  (IDX1)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
      *
               IF    ( SPA-GMN-LBLGETCJIHI (IDX1)   NOT =   SPACE )
                   MOVE    SPA-GMN-GETCJIHIKBNLST (9)
                                   TO  SPA-GMN-GETCJIHIKBN-G  (IDX1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DEL-SEC                 SECTION.
      *
           MOVE    "3002"              TO  SPA-WIDCD
      *
           .
       430-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-DEL-SEC                SECTION.
      *
           INITIALIZE                      SYS-1046-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1046-HOSPNUM
           MOVE    "1046"              TO  SYS-1046-KANRICD
           MOVE    "*"                 TO  SYS-1046-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-1046-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SYS-1046-EDYUKYMD
           MOVE    SYS-1046-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "2003"          TO  SPA-ERRCD
               GO  TO  4301-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           MOVE    SPACE           TO  SYS-1046-REC
           INITIALIZE                  SYS-1046-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1046-HOSPNUM
           MOVE    "1046"              TO  SYS-1046-KANRICD
           MOVE    "*"                 TO  SYS-1046-KBNCD
           IF    ( SPA-W01-SELNUM      >   ZERO )
               MOVE    SPA-W01-SEL-STYUKYMD TO SYS-1046-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD TO SYS-1046-EDYUKYMD
           ELSE
               MOVE    SPA-W01-STYUKYMD     TO SYS-1046-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD     TO SYS-1046-EDYUKYMD
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MEISAI-MAX )
      *
               MOVE    SPA-GMN-MEISAIKBN (IDX1)
                                       TO  SYS-1046-MEISAIKBN (IDX1)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
      *
               IF    ( SPA-GMN-LBLNETCJIHI (IDX1)   =   SPACE )
                   MOVE    "09"        TO  SYS-1046-ETCJIHIKBN (IDX1)
               ELSE
                   MOVE    SPA-GMN-NETCJIHIKBN (IDX1)
                                       TO  SYS-1046-ETCJIHIKBN (IDX1)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
      *
               IF    ( SPA-GMN-LBLGETCJIHI (IDX1)   =   SPACE )
                   MOVE    "09"        TO  SYS-1046-GETCJIHIKBN (IDX1)
               ELSE
                   MOVE    SPA-GMN-GETCJIHIKBN (IDX1)
                                       TO  SYS-1046-GETCJIHIKBN (IDX1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SYS-1046-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1046-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-1046-EDYUKYMD
               MOVE    SYS-1046-REC        TO  SYSKANRI-REC
               MOVE    SYS-1046-CREYMD     TO  SYS-CREYMD
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYS-1046-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-1046-KANRICD    TO  SYSUP-KANRICD
               MOVE    SYS-1046-KBNCD      TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                           TO  SYSUP-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "2001"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SYS-1046-REC        TO  SYSKANRI-REC
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "2002"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      **   IF    ( SPA-GMN-KGIIRYOCD   =   SPACE )
      **       MOVE    "1001"      TO  SPA-ERRCD
      **       MOVE    1           TO  SPA-GMN-CUR
      **   END-IF
      *
      **   IF    ( SPA-ERRCD           =   SPACE )
      **       IF    ( SPA-GMN-MEISAIKBN-G =   SPACE )
      **           MOVE    "1002"  TO  SPA-ERRCD
      **           MOVE    2       TO  SPA-GMN-CUR
      **       END-IF
      **   END-IF
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W67    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W67
           INITIALIZE                      W67
      *
           MOVE    SPA-W01-STYUKYMDH   TO  W67-YUKOSTYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  W67-YUKOEDYMD
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-MEISAI-MAX )
      *
               MOVE    CONST-LBLMEISAI (IDX1)
                                       TO  W67-LBLMEISAI (IDX1)
               MOVE    SPA-GMN-MEISAIKBN-G (IDX1)
                                       TO  W67-MEISAIKBN (IDX1)
      *
               MOVE    SPA-GMN-MEISAIKBN-MAX
                                       TO  W67-MEISAIKBN-COUNT (IDX1)
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-MEISAIKBN-MAX )
                       MOVE    SPA-GMN-MEISAIKBNLST (IDX-CMB)
                                       TO  W67-MEISAIKBN-ITEM
                                                       (IDX1 IDX-CMB)
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
      *
               MOVE    SPA-GMN-NETCJIHIKBN-G (IDX1)
                                       TO  W67-NETCJIHIKBN (IDX1)
      *
               MOVE    SPA-GMN-LBLNETCJIHI (IDX1)
                                       TO  W67-LBLNETCJIHI (IDX1)
      *
               IF    ( SPA-GMN-LBLNETCJIHI (IDX1)   NOT =   SPACE )
                   MOVE    SPA-GMN-NETCJIHIKBN-MAX
                                       TO  W67-NETCJIHIKBN-COUNT (IDX1)
                   PERFORM VARYING IDX-CMB FROM    1   BY  1
                           UNTIL ( IDX-CMB >   SPA-GMN-NETCJIHIKBN-MAX )
                           MOVE    SPA-GMN-NETCJIHIKBNLST (IDX-CMB)
                                       TO  W67-NETCJIHIKBN-ITEM
                                                       (IDX1 IDX-CMB)
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
      *
               MOVE    SPA-GMN-GETCJIHIKBN-G (IDX1)
                                       TO  W67-GETCJIHIKBN (IDX1)
      *
               MOVE    SPA-GMN-LBLGETCJIHI (IDX1)
                                       TO  W67-LBLGETCJIHI (IDX1)
      *
               IF    ( SPA-GMN-LBLGETCJIHI (IDX1)   NOT =   SPACE )
                   MOVE    SPA-GMN-GETCJIHIKBN-MAX
                                       TO  W67-GETCJIHIKBN-COUNT (IDX1)
                   PERFORM VARYING IDX-CMB FROM    1   BY  1
                           UNTIL ( IDX-CMB >   SPA-GMN-GETCJIHIKBN-MAX )
                           MOVE    SPA-GMN-GETCJIHIKBNLST (IDX-CMB)
                                       TO  W67-GETCJIHIKBN-ITEM
                                                       (IDX1 IDX-CMB)
                   END-PERFORM
               END-IF
           END-PERFORM
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
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO  W67-B03-STATE
      *
           IF    ( SPA-W01-SELNUM      >   ZERO )
               MOVE    WIDGET-NORMAL   TO  W67-B03-STATE
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
               MOVE    "MEISAIKBN01"       TO  MCP-WIDGET
           WHEN    002
               MOVE    "MEISAIKBN02"       TO  MCP-WIDGET
           WHEN    003
               MOVE    "MEISAIKBN03"       TO  MCP-WIDGET
           WHEN    004
               MOVE    "MEISAIKBN04"       TO  MCP-WIDGET
           WHEN    005
               MOVE    "MEISAIKBN05"       TO  MCP-WIDGET
           WHEN    006
               MOVE    "MEISAIKBN06"       TO  MCP-WIDGET
           WHEN    007
               MOVE    "MEISAIKBN07"       TO  MCP-WIDGET
           WHEN    008
               MOVE    "MEISAIKBN08"       TO  MCP-WIDGET
           WHEN    009
               MOVE    "MEISAIKBN09"       TO  MCP-WIDGET
           WHEN    010
               MOVE    "MEISAIKBN10"       TO  MCP-WIDGET
           WHEN    011
               MOVE    "MEISAIKBN11"       TO  MCP-WIDGET
           WHEN    012
               MOVE    "MEISAIKBN12"       TO  MCP-WIDGET
           WHEN    013
               MOVE    "MEISAIKBN13"       TO  MCP-WIDGET
           WHEN    014
               MOVE    "MEISAIKBN14"       TO  MCP-WIDGET
           WHEN    015
               MOVE    "MEISAIKBN15"       TO  MCP-WIDGET
           WHEN    016
               MOVE    "MEISAIKBN16"       TO  MCP-WIDGET
           WHEN    021
               MOVE    "NETCJIHIKBN01"      TO  MCP-WIDGET
           WHEN    022
               MOVE    "NETCJIHIKBN02"      TO  MCP-WIDGET
           WHEN    023
               MOVE    "NETCJIHIKBN03"      TO  MCP-WIDGET
           WHEN    024
               MOVE    "NETCJIHIKBN04"      TO  MCP-WIDGET
           WHEN    025
               MOVE    "NETCJIHIKBN05"      TO  MCP-WIDGET
           WHEN    026
               MOVE    "NETCJIHIKBN06"      TO  MCP-WIDGET
           WHEN    027
               MOVE    "NETCJIHIKBN07"      TO  MCP-WIDGET
           WHEN    028
               MOVE    "NETCJIHIKBN08"      TO  MCP-WIDGET
           WHEN    029
               MOVE    "NETCJIHIKBN09"      TO  MCP-WIDGET
           WHEN    030
               MOVE    "NETCJIHIKBN10"      TO  MCP-WIDGET
           WHEN    031
               MOVE    "GETCJIHIKBN01"      TO  MCP-WIDGET
           WHEN    032
               MOVE    "GETCJIHIKBN02"      TO  MCP-WIDGET
           WHEN    033
               MOVE    "GETCJIHIKBN03"      TO  MCP-WIDGET
           WHEN    034
               MOVE    "GETCJIHIKBN04"      TO  MCP-WIDGET
           WHEN    035
               MOVE    "GETCJIHIKBN05"      TO  MCP-WIDGET
           WHEN    036
               MOVE    "GETCJIHIKBN06"      TO  MCP-WIDGET
           WHEN    037
               MOVE    "GETCJIHIKBN07"      TO  MCP-WIDGET
           WHEN    038
               MOVE    "GETCJIHIKBN08"      TO  MCP-WIDGET
           WHEN    039
               MOVE    "GETCJIHIKBN09"      TO  MCP-WIDGET
           WHEN    040
               MOVE    "GETCJIHIKBN10"      TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"               TO  MCP-WIDGET
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
           WHEN    "MEISAIKBN01"
               MOVE    002                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN02"
               MOVE    003                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN03"
               MOVE    004                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN04"
               MOVE    005                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN05"
               MOVE    006                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN06"
               MOVE    007                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN07"
               MOVE    008                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN08"
               MOVE    009                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN09"
               MOVE    010                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN10"
               MOVE    011                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN11"
               MOVE    012                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN12"
               MOVE    013                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN13"
               MOVE    014                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN14"
               MOVE    015                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN15"
               MOVE    016                 TO  SPA-GMN-CUR
           WHEN    "MEISAIKBN16"
               MOVE    021                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN01"
               MOVE    022                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN02"
               MOVE    023                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN03"
               MOVE    024                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN04"
               MOVE    025                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN05"
               MOVE    026                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN06"
               MOVE    027                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN07"
               MOVE    028                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN08"
               MOVE    029                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN09"
               MOVE    030                 TO  SPA-GMN-CUR
           WHEN    "NETCJIHIKBN10"
               MOVE    031                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN01"
               MOVE    032                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN02"
               MOVE    033                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN03"
               MOVE    034                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN04"
               MOVE    035                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN05"
               MOVE    036                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN06"
               MOVE    037                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN07"
               MOVE    038                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN08"
               MOVE    039                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN09"
               MOVE    040                 TO  SPA-GMN-CUR
           WHEN    "GETCJIHIKBN10"
               MOVE    212                 TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-NETCJIHI-MAX )
               IF    ( SPA-GMN-CUR =   IDX1 +  20 )
                   IF    ( SPA-GMN-LBLNETCJIHI (IDX1)   =   SPACE )
                    AND  ( SPA-GMN-NETCJIHIKBN (IDX1)   =   SPACE )
                       IF    ( IDX1    =   CONST-NETCJIHI-MAX )
                           MOVE    31  TO  SPA-GMN-CUR
                       ELSE
                           COMPUTE SPA-GMN-CUR
                               =   SPA-GMN-CUR +   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-GETCJIHI-MAX )
               IF    ( SPA-GMN-CUR =   IDX1 +  30 )
                   IF    ( SPA-GMN-LBLGETCJIHI (IDX1)   =   SPACE )
                    AND  ( SPA-GMN-GETCJIHIKBN (IDX1)   =   SPACE )
                       IF    ( IDX1    =   CONST-GETCJIHI-MAX )
                           MOVE    31  TO  SPA-GMN-CUR
                       ELSE
                           COMPUTE SPA-GMN-CUR
                               =   SPA-GMN-CUR +   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
      *
       5012-SKIP-CUR-EXT.
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
               MOVE    "保険適用外設定入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "その他自費（入院）設定入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "その他自費（外来）設定入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "2001"
           WHEN    "2002"
           WHEN    "2003"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W67"               TO  SPA-MOTOPG
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
           WHEN    "3001"
               MOVE    "登録します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "3002"
               MOVE    "削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W67"                TO  SPA-MOTOPG
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
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
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
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

