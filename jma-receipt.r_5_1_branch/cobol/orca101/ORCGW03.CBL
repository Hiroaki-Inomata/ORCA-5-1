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
       PROGRAM-ID.                               ORCGW03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 施設基準情報（Ｗ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正   
      *  02.00.01    NACL-多々納  02/11/12  一括入力 
      *  02.00.02    NACL-多々納  03/10/02  新規登録・削除追加
      *  02.00.03    NACL-多々納  06/04/24  MONFUNC 対応
      *  03.04.00    NACL-多々納  06/10/26  診療コード対応
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  04.07.00    NACL-多々納  12/03/06  平成２４年４月改正対応
      *  04.07.00    NACL-多々納  14/03/06  平成２６年４月改正対応
      *  04.08.00    NACL-多々納  16/01/XX  表示内容切替え対応
      *  04.08.00    NACL-多々納  16/03/18  平成２８年４月改正対応
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
      *    画面用ＳＰＡ
       01  SPA-W03.
      *    検索画面共通
           03  SPA-W25-KYOUTU          PIC X(30000).
      *
           03  SPA-W03-AREA.
      *
               05  SPA-W03-GMN-AREA.
                   07  SPA-GMN-PAGE        PIC 9(04).
                   07  SPA-GMN-LINE        PIC 9(02).
                   07  SPA-GMN-MAX         PIC 9(04).
                   07  SPA-GMN-PAGE-MAX    PIC 9(04).
                   07  SPA-GMN-CUR         PIC 9(02).
                   07  SPA-GMN-NEXT        PIC 9(01).
      *
                   07  SPA-GMN-AREA.
                       09  SPA-GMN-STYUKYMD    PIC X(09).
                       09  SPA-GMN-EDYUKYMD    PIC X(09).
                       09  SPA-GMN-TBL.
                           11  SPA-GMN-TBLO            OCCURS  25.
                               13  SPA-GMN-TCD         PIC X(04).
                               13  SPA-GMN-TNAME       PIC X(100).
                               13  SPA-GMN-TSSTKIJUN   PIC 9(01).
                               13  SPA-GMN-TKEIFLG     PIC 9(01).
      *                        期間外
                               13  SPA-GMN-TMEIFLG     PIC 9(01).
                   07  SPA-GMN-SELCD                   PIC X(04).
      *
                   07  SPA-NAI-AREA.
                       09  SPA-NAI-TBL.
                           11  SPA-NAI-TBLO            OCCURS  8500.
                               13  SPA-NAI-TSSTKIJUN   PIC 9(01).
                               13  SPA-NAI-TKEIFLG     PIC 9(01).
      *
                               13  SPA-MAE-TSSTKIJUN   PIC 9(01).
      *H28.1           施設基準名称有
                       09  SPA-NAI-TBL2.
                           11  SPA-NAI-TBL2            OCCURS  8500.
                               13  SPA-NAI-MEIFLG      PIC 9(01).
      *
                       09  SPA-SYORIFLG                PIC 9(01).
                       09  SPA-NAI-SELCD               PIC 9(04).
      *
               05  SPA-GMN-INPUT-AREA.
                   07  SPA-GMN-SRYCD                   PIC X(09).
                   07  SPA-GMN-MEISYO                  PIC X(100).
                   07  SPA-GMN-YUKOKIKAN.
                       09  SPA-GMN-YUKOSTYMD       PIC X(09).
                       09  SPA-GMN-YUKOK1          PIC X(02).
                       09  SPA-GMN-YUKOEDYMD       PIC X(09).
                   07  SPA-NAI-YUKOKIKAN-G.
                       09  SPA-NAI-YUKOSTYMD       PIC X(08).
                       09  SPA-NAI-YUKOEDYMD       PIC X(08).
                   07  SPA-GMN-SSTKIJUNCD-G.
                       09  SPA-GMN-SSTKIJUNCD          PIC 9(04)
                                                       OCCURS  10.
                   07  SPA-KIJUN-MAX                   PIC 9(04).
                   07  SPA-GMN-KIJUNTEIGENCD           PIC 9(04).
      *
                   07  SPA-NAI-KIJUNFLG                PIC 9(04).
      *
      *H28.1
               05  SPA-HYOJI-FLG           PIC 9(01).
      *
      *    検索画面用ＳＰＡ
           COPY    "W25SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
           03  FLG-SELCD                          PIC 9(01).
           03  FLG-TOUROKU                        PIC 9(01).
           03  FLG-TENSU                          PIC 9(01).
           03  FLG-INPUTCD                        PIC 9(01).
      *
           03  FLG-KANJI                         PIC 9(01).
      *H28.1
           03  FLG-HENEND                        PIC 9(01).
           03  FLG-HYOJIOK                       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
           03  IDZ1                              PIC 9(04).
           03  IDZ2                              PIC 9(04).
      *
           03  IDX-STR                           PIC 9(04).
           03  IDX2                              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-KBNCD           PIC 9(02).
           03  WRK-KBNCD-R         REDEFINES   WRK-KBNCD.
               05  WRK-KBNCD-RR    PIC 9(01).
               05  WRK-KBNCD-1     PIC 9(01).
           03  WRK-MAX-X.
               05  WRK-MAX         PIC 9(04). 
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(08).
               05  WRK-WIDGET2     PIC 9(02).
               05  WRK-WIDGET3     PIC X(03). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-EDYUKYMD        PIC X(08).
           03  WRK-YUKOSTYMD       PIC X(08).
      *
           03  WRK-TCD             PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
      *
           03  WRK-INPUTCD         PIC X(20).
           03  WRK-CD-MCNT         PIC 9(04).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面最大
       01  CONS-GYO-MAX            PIC 9(04)   VALUE    25.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    施設基準名
           COPY  "CPSK0031.INC".
      *    施設基準コードフラグ情報
           COPY  "CPSK1006.INC".
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
       PROCEDURE                                 DIVISION    USING
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W03
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W03             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK 
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "W03    "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    "W25"
                   PERFORM 340-W25-SYORI-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
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
           MOVE    SPACE               TO  SPA-W03
           INITIALIZE                  SPA-W03-AREA
           INITIALIZE                  SPA-W03-GMN-AREA
           INITIALIZE                  SPA-GMN-INPUT-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      * 
      *    施設基準情報
           MOVE    SPACE               TO  SYS-0031-REC 
           INITIALIZE                      SYS-0031-REC
           MOVE    "0031"              TO  SYS-0031-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-0031-HOSPNUM
           MOVE    SYS-0031-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  WRK-MAX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-0031-REC
      *        施設基準名称あり
               MOVE    SYS-0031-KBNCD(1:4) TO  IDY
               MOVE    1                   TO  SPA-NAI-MEIFLG(IDY)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    施設基準情報
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *    対象のシステム管理がない場合、新規とする
           IF      FLG-SYSKANRI        =   1
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               PERFORM 3102-SINKI-CHK-SEC
           ELSE
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    ZERO                TO  SPA-SYORIFLG
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
      *H2.4
               EVALUATE    SYS-1006-KBNCD
                   WHEN    "1"
                       MOVE    0               TO  IDX
                   WHEN    "2"
                       MOVE    500             TO  IDX
                   WHEN    "7"
                       MOVE    3000            TO  IDX
                   WHEN    "8"
                       MOVE    3500            TO  IDX
                   WHEN    "17"
                       MOVE    8000            TO  IDX
               END-EVALUATE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       500
                   ADD     1               TO  IDX
                   MOVE    SYS-1006-SSTKIJUN  (IDY)
                                           TO  SPA-NAI-TSSTKIJUN (IDX)
      *H28.1
                   MOVE    SYS-1006-SSTKIJUN  (IDY)
                                           TO  SPA-MAE-TSSTKIJUN (IDX)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    Ｈ１８年４月以降は、１００とする
           IF      SPA-W01-EDYUKYMD    >=  "20060401"
               IF      IDX                 <   1000
                   MOVE    1000            TO  IDX
               END-IF
           END-IF
      *H24.4  3500 とする
           IF      SPA-W01-EDYUKYMD    >=  "20120401"
               IF      IDX                 <   3500
                   MOVE    3500            TO  IDX
               END-IF
           END-IF
      *H26.4  8500 とする
           IF      SPA-W01-EDYUKYMD    >=  "20140401"
               IF      IDX                 <   8500
                   MOVE    8500            TO  IDX
               END-IF
           END-IF
      *
           END-IF
      *
           MOVE    IDX                 TO  SPA-GMN-MAX
      *
           COMPUTE SPA-GMN-PAGE-MAX    =   (SPA-GMN-MAX - 1)     /
                                           CONS-GYO-MAX  + 1
           MOVE    1                   TO  SPA-GMN-PAGE
      *
      *H28.1
           MOVE    1                   TO  SPA-HYOJI-FLG
      *
           PERFORM 3101-SSTKIJUN-HENSYU-SEC  
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準新規編集処理
      *****************************************************************
       3102-SINKI-CHK-SEC            SECTION.
      *
      *    施設基準情報
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-EDYUKYMD
           PERFORM     UNTIL   FLG-SYSKANRI   =   1
      *D               OR      IDX            >=  1000
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SPA-W01-STYUKYMD    >   SYS-1006-EDYUKYMD
                   IF      WRK-EDYUKYMD        <   SYS-1006-EDYUKYMD
      *                別の日があった場合、クリア
                       MOVE    ZERO                TO  IDX
                       INITIALIZE                  SPA-NAI-TBL
                       MOVE    SYS-1006-EDYUKYMD   TO  WRK-EDYUKYMD
                   END-IF
      *            前の日付あり（新規）
                   MOVE    1               TO  SPA-SYORIFLG
      *H2.4
                   EVALUATE    SYS-1006-KBNCD
                   WHEN    "1"
                       MOVE    0               TO  IDX
                   WHEN    "2"
                       MOVE    500             TO  IDX
                   WHEN    "7"
                       MOVE    3000            TO  IDX
                   WHEN    "8"
                       MOVE    3500            TO  IDX
                   WHEN    "17"
                       MOVE    8000            TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >   500
                       ADD     1               TO  IDX
                       MOVE    SYS-1006-SSTKIJUN  (IDY)
                                               TO  SPA-NAI-TSSTKIJUN
                                                              (IDX)
      *H28.1
                       MOVE    SYS-1006-SSTKIJUN  (IDY)
                                               TO  SPA-MAE-TSSTKIJUN
                                                              (IDX)
                   END-PERFORM
                   MOVE    SYS-1006-EDYUKYMD   TO  WRK-EDYUKYMD
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key17"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *H24.4
           IF      SPA-W01-EDYUKYMD    >=  "20120401"
               IF      IDX                 <   3500
                   MOVE    3500            TO  IDX
               END-IF
           END-IF
      *
      *H26.4
           IF      SPA-W01-EDYUKYMD    >=  "20140401"
               IF      IDX                 <   8500
                   MOVE    8500            TO  IDX
               END-IF
           END-IF
      *
      *    施設基準があるかの判定
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   IDX
               IF      SPA-NAI-TSSTKIJUN (IDY)     =   1
                   IF      SPA-NAI-MEIFLG (IDY)    =   ZERO
      *                施設基準なし
                       MOVE    ZERO            TO  SPA-NAI-TSSTKIJUN
                                                             (IDY)
                       MOVE    ZERO            TO  SPA-MAE-TSSTKIJUN
                                                             (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
      *        MOVE    SPACE               TO  SYS-0031-REC
      *        INITIALIZE                      SYS-0031-REC
      *        MOVE    "0031"              TO  SYS-0031-KANRICD
      *        MOVE    IDY                 TO  SYS-0031-KBNCD (1:4)
      *        MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
      *        MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
      *        MOVE    SPA-HOSPNUM         TO  SYS-0031-HOSPNUM
      *        MOVE    SYS-0031-REC        TO  MCPDATA-REC
      *        PERFORM 900-SYSKANRI-INV-SEC
      *        IF      FLG-SYSKANRI        =   ZERO
      *            CONTINUE
      *        ELSE
      *            MOVE    ZERO                TO  SPA-NAI-TSSTKIJUN
      *                                                         (IDY)
      *            MOVE    ZERO                TO  SPA-MAE-TSSTKIJUN
      *                                                         (IDY)
      *        END-IF
      *H26.4
      *        1000 の次は、3000
      *        IF      IDY                 =   1000
      *            MOVE    3000            TO  IDY
      *        END-IF
      *        3500 の次は、8001
      *        IF      IDY                 =   3500
      *            MOVE    8000            TO  IDY
      *        END-IF
      **** END-PERFORM
      *    対象がない時、１レコード分とする
           IF      IDX                 =   ZERO
               MOVE    1               TO  SPA-SYORIFLG
      **       MOVE    3500            TO  IDX
               MOVE    8500            TO  IDX
           END-IF
      *
           .
       3102-SINKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準編集処理
      *****************************************************************
       3101-SSTKIJUN-HENSYU-SEC            SECTION.
      *
           EVALUATE    SPA-HYOJI-FLG
               WHEN    0
      *            全体
                   PERFORM 3101-SSTKIJUN-HEN01-SEC
               WHEN    1
      *            施設基準（名称有）
                   PERFORM 3102-SSTKIJUN-HEN02-SEC
               WHEN    2
      *            設定分のみ
                   PERFORM 3103-SSTKIJUN-HEN03-SEC
           END-EVALUATE
           .
       3101-SSTKIJUN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準編集処理（全体）
      *****************************************************************
       3101-SSTKIJUN-HEN01-SEC            SECTION.
      *
      *
           INITIALIZE                      SPA-GMN-TBL
      *
      **** COMPUTE IDX =   (SPA-GMN-PAGE    -   1)   *   25
           COMPUTE IDX =   (SPA-GMN-PAGE    -   1)   *   CONS-GYO-MAX
      * 
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       CONS-GYO-MAX
               ADD     1                           TO  IDX
               IF      IDX     >   SPA-GMN-MAX
                   MOVE    CONS-GYO-MAX            TO  IDY
               ELSE
                   MOVE    IDX             TO  SPA-GMN-TCD       (IDY)
                   MOVE    SPA-NAI-TSSTKIJUN   (IDX)
                                           TO  SPA-GMN-TSSTKIJUN (IDY)
                   MOVE    SPA-NAI-TKEIFLG     (IDX)
                                           TO  SPA-GMN-TKEIFLG   (IDY)
                   MOVE    SPACE           TO  SYS-0031-REC
                   INITIALIZE                  SYS-0031-REC
                   MOVE    "0031"          TO  SYS-0031-KANRICD
                   MOVE    SPA-GMN-TCD (IDY)
                                           TO  SYS-0031-KBNCD (1:4)
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-0031-HOSPNUM
                   MOVE    SYS-0031-REC    TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-0031-REC
                       MOVE    SYS-0031-SSTKIJUN
                                               TO  SPA-GMN-TNAME(IDY)
      *H28.1
                   ELSE
                       IF     SPA-NAI-TSSTKIJUN (IDX)  =   1
      *                    履歴から編集
                           PERFORM 310211-SYS0031-HEN-SEC
                           MOVE    1           TO  SPA-GMN-TMEIFLG(IDY)
                       END-IF
                   END-IF
               END-IF 
           END-PERFORM      
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       3101-SSTKIJUN-HEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準編集処理（名称有）
      *****************************************************************
       3102-SSTKIJUN-HEN02-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-TBL
      *
           COMPUTE IDX-STR         =   (SPA-GMN-PAGE    -   1)
                                   *   CONS-GYO-MAX
                                   +   1
      *
           MOVE    ZERO                TO  FLG-HENEND
           MOVE    ZERO                TO  FLG-HYOJIOK
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >      SPA-GMN-MAX )
                       OR     (FLG-HENEND     =   1   )
      *        名称あり
               IF     (SPA-NAI-MEIFLG (IDX)   =   1 )
                  OR  (SPA-MAE-TSSTKIJUN(IDX) =   1 )
                   ADD     1                  TO  IDX2
                   IF      IDX2               <   IDX-STR
                       CONTINUE
                   ELSE
      *                テーブル編集
                       PERFORM 31021-SSTKIJUN-TBLHEN-SEC
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-HENEND          =   1
               MOVE    1                   TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-NEXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       3102-SSTKIJUN-HEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準編集処理
      *****************************************************************
       31021-SSTKIJUN-TBLHEN-SEC            SECTION.
      *
           ADD     1                       TO  IDY
           IF      IDY         >   CONS-GYO-MAX
               MOVE    CONS-GYO-MAX        TO  IDY
               MOVE    1                   TO  FLG-HENEND
           ELSE
               MOVE    IDX                 TO  SPA-GMN-TCD       (IDY)
               MOVE    SPA-NAI-TSSTKIJUN   (IDX)
                                           TO  SPA-GMN-TSSTKIJUN (IDY)
               MOVE    SPA-NAI-TKEIFLG     (IDX)
                                           TO  SPA-GMN-TKEIFLG   (IDY)
               MOVE    SPACE               TO  SYS-0031-REC
               INITIALIZE                      SYS-0031-REC
               MOVE    "0031"              TO  SYS-0031-KANRICD
               MOVE    SPA-GMN-TCD (IDY)
                                           TO  SYS-0031-KBNCD (1:4)
               MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-0031-HOSPNUM
               MOVE    SYS-0031-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-0031-REC
                   MOVE    SYS-0031-SSTKIJUN
                                               TO  SPA-GMN-TNAME(IDY)
               ELSE
      *            履歴から編集
                   PERFORM 310211-SYS0031-HEN-SEC
                   MOVE    1               TO  SPA-GMN-TMEIFLG   (IDY)
               END-IF
      *
               IF     (WRK-TCD     NOT =   ZERO  )
                   IF     (SPA-GMN-TCD (IDY)   =   WRK-TCD )
                       MOVE    1               TO  FLG-HYOJIOK
                   END-IF
                   IF     (SPA-GMN-TCD (IDY)   >   WRK-TCD )
                       MOVE    1               TO  FLG-HYOJIOK
                   END-IF
               END-IF
           END-IF
           .
       31021-SSTKIJUN-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準名称編集
      *****************************************************************
       310211-SYS0031-HEN-SEC            SECTION.
      *
      *    施設基準情報
           MOVE    SPACE               TO  SYS-0031-REC 
           INITIALIZE                      SYS-0031-REC
           MOVE    "0031"              TO  SYS-0031-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-0031-HOSPNUM
           MOVE    SPA-GMN-TCD (IDY)   TO  SYS-0031-KBNCD (1:4)
           MOVE    SYS-0031-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-0031-REC
               IF      SYS-0031-EDYUKYMD   <   SPA-W01-STYUKYMD
                   MOVE    SYS-0031-EDYUKYMD     TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   STRING  "[外]"                DELIMITED  BY  SIZE
                           SYS-0031-SSTKIJUN     DELIMITED  BY  SPACE
                           "　【終了日："
                           WRK-HENYMDG
                           "】"                  DELIMITED  BY  SIZE
                                       INTO  SPA-GMN-TNAME(IDY)
                   END-STRING
               ELSE
               IF      SYS-0031-STYUKYMD   >   SPA-W01-EDYUKYMD
                   MOVE    SYS-0031-STYUKYMD     TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   STRING  "[外] "               DELIMITED  BY  SIZE
                           SYS-0031-SSTKIJUN     DELIMITED  BY  SPACE
                           "　【開始日："
                           WRK-HENYMDG
                           "】"                  DELIMITED  BY  SIZE
                                       INTO  SPA-GMN-TNAME(IDY)
                   END-STRING
               END-IF
               END-IF
           END-IF
           .
       310211-SYS0031-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準編集処理（設定有）
      *****************************************************************
       3103-SSTKIJUN-HEN03-SEC            SECTION.
      *
      *
           INITIALIZE                      SPA-GMN-TBL
      *
           COMPUTE IDX-STR         =   (SPA-GMN-PAGE    -   1)
                                   *   CONS-GYO-MAX
                                   +   1
      *
           MOVE    ZERO                TO  FLG-HENEND
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >      SPA-GMN-MAX )
                       OR     (FLG-HENEND     =   1   )
      *        設定あり
               IF     (SPA-MAE-TSSTKIJUN(IDX) =   1 )
                  OR  (SPA-NAI-TSSTKIJUN(IDX) =   1 )
                   ADD     1                  TO  IDX2
                   IF      IDX2               <   IDX-STR
                       CONTINUE
                   ELSE
      *                テーブル編集
                       PERFORM 31021-SSTKIJUN-TBLHEN-SEC
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-HENEND          =   1
               MOVE    1                   TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-NEXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       3103-SSTKIJUN-HEN03-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
      *        登録
               WHEN    "1001"
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
      *        削除
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面より編集処理
      *****************************************************************
       340-W25-SYORI-SEC                      SECTION.
      *
           IF      SPA-W25-SRYCD   NOT =   SPACE
               MOVE    SPA-W25-SRYCD       TO  SPA-GMN-SRYCD
               PERFORM 4203-SYRCD-SYORI-SEC
           ELSE
               INITIALIZE                   SPA-GMN-INPUT-AREA
               MOVE    50                  TO  SPA-GMN-CUR
           END-IF
           .
       340-W25-SYORI-EXT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *    前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  410-KAKUTEI-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        カーソル移動
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM  409-CURSET-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
      *        ←
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-IDOU-01-SEC
      *        →
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM  408-IDOU-02-SEC
      *H28.1   表示切替え
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM  410-HYOUJI-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYCD"
      *            診療コード入力
                   PERFORM 420-SYRCD-CHK-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       420-SYRCD-CHK-SEC       SECTION.
      *
           MOVE    W03-SRYCD           TO  SPA-GMN-SRYCD
      *
           IF      SPA-GMN-SRYCD(1:1)  =   "/"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 4202-W25-SYORI-SEC
           ELSE
               IF      SPA-GMN-SRYCD   NOT =   SPACE
                   PERFORM 4201-INPUTCD-CHK-SEC
      *            半角で９文字の時、点数マスタチェックへ
                   IF     (WRK-CD-MCNT         =   9  )  AND
                          (FLG-KANJI           =   ZERO )  AND
                          (SPA-GMN-SRYCD   IS  NUMERIC  )
                       PERFORM 4203-SYRCD-SYORI-SEC
                   ELSE
                       MOVE    SPA-GMN-SRYCD       TO  WRK-INPUTCD
                       PERFORM 4202-W25-SYORI-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       420-SYRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4201-INPUTCD-CHK-SEC       SECTION.
      *
      *    MOVE    ZERO               TO  FLG-KENSAKU
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYCD       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-SRYCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0101"              TO  SPA-ERRCD
               MOVE    50                  TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   MOVE    SPACE           TO  SPA-GMN-SRYCD(9:)
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *
           .
       4201-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード編集処理
      *****************************************************************
       4203-SYRCD-SYORI-SEC       SECTION.
      *
      *    点数マスタ読込み
           MOVE    SPA-W01-EDYUKYMD    TO  WRK-YUKOSTYMD
           MOVE    SPA-GMN-SRYCD       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
      *        開始日での検索
               MOVE    SPA-W01-STYUKYMD    TO  WRK-YUKOSTYMD
               MOVE    SPA-GMN-SRYCD       TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
           END-IF
           IF      FLG-TENSU           =   1
               MOVE    "0102"              TO  SPA-ERRCD
               MOVE    50                  TO  SPA-GMN-CUR
           ELSE
               IF      TNS-SRYCD(1:1)      =   "1"
                   PERFORM 42031-TENSU-HEN-SEC
               ELSE
                   MOVE    "0104"              TO  SPA-ERRCD
                   MOVE    50                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4203-SYRCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       42031-TENSU-HEN-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-INPUT-AREA
           MOVE    TNS-SRYCD           TO  SPA-GMN-SRYCD
           MOVE    TNS-NAME            TO  SPA-GMN-MEISYO
           MOVE    TNS-YUKOSTYMD       TO  SPA-NAI-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  SPA-NAI-YUKOEDYMD
      *
           MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-YUKOEDYMD
           MOVE    "〜"                TO  SPA-GMN-YUKOK1
      *
           MOVE    ZERO                TO  SPA-KIJUN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )
               IF      TNS-SSTKIJUNCD (IDX)    NOT =   ZERO
                   ADD    1                    TO  SPA-KIJUN-MAX
               END-IF
               MOVE    TNS-SSTKIJUNCD (IDX) 
                                               TO  SPA-GMN-SSTKIJUNCD
                                                      (IDX)
           END-PERFORM
      *    基準不適合逓減対象施設基準
           MOVE    TNS-KIJUNTEIGENCD    TO  SPA-GMN-KIJUNTEIGENCD
      *
           IF     (SPA-GMN-KIJUNTEIGENCD   =   ZERO )  AND
                  (SPA-KIJUN-MAX           =   ZERO )
               MOVE    "0103"              TO  SPA-ERRCD
               MOVE    50                  TO  SPA-GMN-CUR
           ELSE
               MOVE    10                  TO  SPA-KIJUN-MAX
               MOVE    SPACE               TO  SPA-GMN-SELCD
               IF      SPA-GMN-SSTKIJUNCD(1)   NOT =   ZERO
                   MOVE    SPA-GMN-SSTKIJUNCD(1)   TO  SPA-GMN-SELCD
                   MOVE    1                   TO  SPA-NAI-KIJUNFLG
               ELSE
                   MOVE    SPA-GMN-KIJUNTEIGENCD   TO  SPA-GMN-SELCD
                   MOVE    11                  TO  SPA-NAI-KIJUNFLG
               END-IF
               MOVE    SPA-GMN-SELCD       TO  W03-SELCD
               PERFORM 410-INPUT-CHK-SEC
      *        カーソル移動
               PERFORM 4103-SELCD-CUR-SEC
           END-IF
           .
       42031-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   ZERO
               IF      (SPA-GMN-SELCD  NOT =   SPACE )  AND
                       (FLG-SELCD          =   ZERO  )
      *            コード表示へ
                   PERFORM 4103-SELCD-HENSYU-SEC
               END-IF
               IF      (SPA-GMN-SELCD  NOT =   SPACE )  AND
                       (SPA-ERRCD          =   SPACE  ) AND
                       (WRK-MCP-WIDGET     =   "SELCD")
      *            カーソル移動
                   PERFORM 4103-SELCD-CUR-SEC
               END-IF
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W03-SELCD           TO  SPA-GMN-SELCD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   CONS-GYO-MAX
               MOVE    W03-SSTKIJUN (IDX)  TO  SPA-GMN-TSSTKIJUN (IDX)
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
           PERFORM  4101-SELCD-CHK-SEC
      *
           PERFORM  4101-SSTKIJUN-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W03"               TO  SPA-MOTOPG
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
      *    選択コードチェック処理
      *****************************************************************
       4101-SELCD-CHK-SEC                         SECTION.
      *
           IF      SPA-GMN-SELCD       =   SPACE
               MOVE    ZERO                TO  SPA-NAI-SELCD
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-SELCD       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   4     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               ELSE
                   IF    ( SNUM-OUTEDT     =   ZERO )
                       MOVE    SPACE           TO  SPA-GMN-SELCD
                       MOVE    ZERO            TO  SPA-NAI-SELCD
                   ELSE
                       MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-SELCD
                       MOVE    SNUM-OUTEDT(12:4)   TO  SPA-NAI-SELCD
                   END-IF
               END-IF
      *H24.4
      *        1001から3000 はエラーとする
               IF     (SPA-NAI-SELCD       >=  1001  AND
                                           <=  3000   )
      *        3501から8000 はエラーとする
      *          OR   (SPA-NAI-SELCD       >=  3501  AND
      *                                    <=  8000   )
      *        3601から8000 はエラーとする
      **         OR   (SPA-NAI-SELCD       >=  3601  AND
      *        4001から8000 はエラーとする
                 OR   (SPA-NAI-SELCD       >=  4001  AND
                                           <=  8000   )
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-SELCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　施設基準チェック
      *****************************************************************
       4101-SSTKIJUN-CHK-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-SELCD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   CONS-GYO-MAX  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
      *
      *H28.1
      *        COMPUTE IDZ             =   IDX
      *                                +   (SPA-GMN-PAGE - 1 ) * 25
               IF      SPA-GMN-TCD (IDX)       =   ZERO
                                               OR  SPACE
                   MOVE    ZERO                TO  IDZ
               ELSE
                   MOVE    SPA-GMN-TCD (IDX)   TO  IDZ
               END-IF
      *!!!!
      *        施設基準名があること
               IF   (( SPA-GMN-TNAME (IDX)     =   SPACE )
               AND   ( SPA-GMN-TSSTKIJUN (IDX) =   1     ))
      *           今回選択あり
               OR   (( SPA-NAI-MEIFLG (IDZ)    =   ZERO  )  AND
                     ( SPA-GMN-TSSTKIJUN (IDX) =   1     )  AND
                     ( SPA-MAE-TSSTKIJUN (IDZ) =   ZERO  ))
      *
                   IF      SPA-GMN-TKEIFLG (IDX)   =   ZERO
                       MOVE    1                  TO  SPA-GMN-TKEIFLG
                                                            (IDX)
                       MOVE    "K002"             TO  SPA-ERRCD
                       MOVE    IDX                TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
               IF      SPA-GMN-TCD (IDX)   =   SPA-GMN-SELCD
                   MOVE    1               TO  FLG-SELCD
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
      *H28.1
               AND    (IDZ                 >   ZERO  )
                   EVALUATE    SPA-GMN-TSSTKIJUN (IDX)
                       WHEN    ZERO
                       WHEN    1
                           MOVE  SPA-GMN-TSSTKIJUN (IDX)
                                         TO  SPA-NAI-TSSTKIJUN (IDZ)
                           MOVE  SPA-GMN-TKEIFLG   (IDX)
                                         TO  SPA-NAI-TKEIFLG  (IDZ)
                       WHEN    OTHER
                           MOVE    "0002"             TO  SPA-ERRCD
                           MOVE    IDX                TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       4101-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コード表示へ処理
      *****************************************************************
       4103-SELCD-HENSYU-SEC             SECTION.
      *
      *H28.1
      *    コード判定
           IF      SPA-HYOJI-FLG       =   1
                                       OR  2
               PERFORM 41031-SELCE-HENSYU-02-SEC
           ELSE
      *
           MOVE    SPA-GMN-SELCD       TO  WRK-TCD
           COMPUTE WRK-PAGE            =  ((WRK-TCD - 1  )
                                       /   CONS-GYO-MAX )
                                       +   1
           IF      WRK-PAGE            >   SPA-GMN-PAGE-MAX
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    90                  TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-PAGE            TO  SPA-GMN-PAGE
               PERFORM 3101-SSTKIJUN-HENSYU-SEC
           END-IF
      *
           END-IF
           .
       4103-SELCD-HENSYU-EXT.
           EXIT.
      *H28.1
      *****************************************************************
      *    コード表示へ処理
      *****************************************************************
       41031-SELCE-HENSYU-02-SEC             SECTION.
      *
           MOVE    SPA-GMN-SELCD       TO  WRK-TCD
           IF      SPA-HYOJI-FLG       =   1
               IF      (SPA-MAE-TSSTKIJUN (WRK-TCD)  =   1 )
                    OR (SPA-NAI-MEIFLG (WRK-TCD)     =   1 )
                   CONTINUE
               ELSE
      *            対象なし
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               IF      (SPA-MAE-TSSTKIJUN (WRK-TCD)  =   1 )
                   OR  (SPA-NAI-TSSTKIJUN (WRK-TCD)  =   1 )
                   CONTINUE
               ELSE
      *            対象なし
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  FLG-HYOJIOK
               PERFORM VARYING    SPA-GMN-PAGE FROM   1  BY  1
                       UNTIL      FLG-HYOJIOK  =   1
                   PERFORM 3101-SSTKIJUN-HENSYU-SEC
                   IF      SPA-GMN-NEXT        =   ZERO
                       MOVE    1               TO  FLG-HYOJIOK
                   END-IF
               END-PERFORM
           END-IF
           .
       41031-SELCE-HENSYU-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル移動処理
      *****************************************************************
       4103-SELCD-CUR-SEC             SECTION.
      *    カーソル
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   CONS-GYO-MAX  )
               IF      SPA-GMN-TCD (IDX)   =   SPA-GMN-SELCD
                   MOVE    IDX             TO  SPA-GMN-CUR
                   MOVE    CONS-GYO-MAX    TO  IDX
               END-IF
           END-PERFORM
           MOVE    SPACE           TO  SPA-GMN-SELCD
      *
           .
       4103-SELCD-CUR-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    "0006"             TO  SPA-ERRCD
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
      *    H28.1
           IF      SPA-HYOJI-FLG       =   1
                                       OR  2
               PERFORM 3101-SSTKIJUN-HENSYU-SEC
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
      *H24.4
      *    1001から3000 までは画面表示なしとする
           IF      SPA-GMN-PAGE   =   120
               MOVE    40         TO  SPA-GMN-PAGE
           END-IF
      *H26.4
      *    3501から8000 までは画面表示なしとする
      *    IF      SPA-GMN-PAGE   =   320
      *        MOVE    140        TO  SPA-GMN-PAGE
      *    END-IF
      *H28.4
      *    3601から8000 までは画面表示なしとする
      *    IF      SPA-GMN-PAGE   =   320
      *        MOVE    144        TO  SPA-GMN-PAGE
      *    END-IF
      *H30.4
      *    4001から8000 までは画面表示なしとする
           IF      SPA-GMN-PAGE   =   320
               MOVE    160        TO  SPA-GMN-PAGE
           END-IF
      *
           PERFORM 3101-SSTKIJUN-HENSYU-SEC  
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  440-ATOGMN-EXT
           END-IF
      *    H28.1
           IF      SPA-HYOJI-FLG       =   1
                                       OR  2
               IF      SPA-GMN-NEXT        =   1
                   COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1
                   PERFORM 3101-SSTKIJUN-HENSYU-SEC
               ELSE
                   MOVE    "0007"             TO  SPA-ERRCD
               END-IF
               GO  TO  440-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   SPA-GMN-PAGE-MAX
               MOVE    "0007"             TO  SPA-ERRCD
               GO  TO  440-ATOGMN-EXT
           END-IF
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1       
      *H24.4
      *    1001から3000 までは画面表示なしとする
           IF      SPA-GMN-PAGE   =   41
               MOVE    121        TO  SPA-GMN-PAGE
           END-IF
      *H26.4
      *    3501から8000 までは画面表示なしとする
      *    IF      SPA-GMN-PAGE   =   141
      *        MOVE    321        TO  SPA-GMN-PAGE
      *    END-IF
      *H28.4
      *    3601から8000 までは画面表示なしとする
      *    IF      SPA-GMN-PAGE   =   145
      *        MOVE    321        TO  SPA-GMN-PAGE
      *    END-IF
      *H30.4
      *    4001から8000 までは画面表示なしとする
           IF      SPA-GMN-PAGE   =   161
               MOVE    321        TO  SPA-GMN-PAGE
           END-IF
      *
           PERFORM 3101-SSTKIJUN-HENSYU-SEC  
           .
       440-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    表示切替え処理
      *****************************************************************
       410-HYOUJI-SEC             SECTION.
      *
           EVALUATE    SPA-HYOJI-FLG
               WHEN    0
                   MOVE    1               TO  SPA-HYOJI-FLG
               WHEN    1
                   MOVE    2               TO  SPA-HYOJI-FLG
               WHEN    2
                   MOVE    0               TO  SPA-HYOJI-FLG
           END-EVALUATE
      *
           INITIALIZE                      SPA-GMN-SELCD
           INITIALIZE                      SPA-NAI-SELCD 
           INITIALIZE                      SPA-GMN-INPUT-AREA
      *
           MOVE    1                       TO  SPA-GMN-PAGE
           PERFORM 3101-SSTKIJUN-HENSYU-SEC
      *
           .
       410-HYOUJI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
           MOVE    SPACE               TO  SPA-GMN-SELCD
           MOVE    50                  TO  SPA-GMN-CUR
           .
       402-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    ←処理
      *****************************************************************
       405-IDOU-01-SEC             SECTION.
      *
      *    施設基準なし
           IF     (SPA-GMN-SSTKIJUNCD-G    =   ZERO )
             AND  (SPA-GMN-KIJUNTEIGENCD   =   ZERO )
               GO      TO      405-IDOU-01-EXT
           END-IF
      *
           IF      SPA-NAI-KIJUNFLG    =   1
               MOVE    11                  TO  IDX-STR
           ELSE
               COMPUTE IDX-STR             =   SPA-NAI-KIJUNFLG
                                           -   1
           END-IF
           MOVE    SPACE               TO  SPA-GMN-SELCD
           PERFORM VARYING     IDX     FROM    IDX-STR BY  -1
                   UNTIL       SPA-GMN-SELCD   NOT =   SPACE
      *
               IF      IDX             =   11
                   IF      SPA-GMN-KIJUNTEIGENCD   NOT =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KIJUNFLG
                       MOVE    SPA-GMN-KIJUNTEIGENCD
                                               TO  SPA-GMN-SELCD
                   END-IF
               ELSE
                   IF      SPA-GMN-SSTKIJUNCD(IDX) NOT =   ZERO
                       MOVE    SPA-GMN-SSTKIJUNCD(IDX)
                                               TO  SPA-GMN-SELCD
                       MOVE    IDX             TO  SPA-NAI-KIJUNFLG
                   ELSE
                       IF      IDX             =   1
                           MOVE    12          TO  IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    IF      SPA-NAI-KIJUNFLG    =   11
      *        MOVE    SPA-KIJUN-MAX       TO  SPA-NAI-KIJUNFLG
      *    ELSE
      *        IF      SPA-NAI-KIJUNFLG    =   1
      *            MOVE    11                   TO  SPA-NAI-KIJUNFLG
      *        ELSE
      *            COMPUTE SPA-NAI-KIJUNFLG     =   SPA-NAI-KIJUNFLG
      *                                         -   1
      *            IF      SPA-NAI-KIJUNFLG     =   ZERO
      *                MOVE    11                   TO  SPA-NAI-KIJUNFLG
      *            END-IF
      *        END-IF
      *    END-IF
      *
      *    IF      SPA-NAI-KIJUNFLG    =   11
      *        IF      SPA-GMN-KIJUNTEIGENCD   =   ZERO
      *            MOVE    SPA-KIJUN-MAX       TO  SPA-NAI-KIJUNFLG
      *        END-IF
      *    END-IF
      *    MOVE    SPACE               TO  SPA-GMN-SELCD
      *    IF      SPA-NAI-KIJUNFLG    =   11
      *        MOVE    SPA-GMN-KIJUNTEIGENCD   TO  SPA-GMN-SELCD
      *    ELSE
      *        MOVE    SPA-NAI-KIJUNFLG        TO  IDX
      *        IF      IDX                     =   ZERO
      *            CONTINUE
      *        ELSE
      *            IF      SPA-GMN-SSTKIJUNCD(IDX) NOT =   ZERO
      *                MOVE    SPA-GMN-SSTKIJUNCD(IDX) TO  SPA-GMN-SELCD
      *            END-IF
      *        END-IF
      ***  END-IF
      *
           IF      SPA-GMN-SELCD   NOT =   SPACE
               MOVE    SPA-GMN-SELCD       TO  W03-SELCD
               PERFORM 410-INPUT-CHK-SEC
      *        カーソル移動
               PERFORM 4103-SELCD-CUR-SEC
           END-IF
           .
       405-IDOU-01-EXT.
           EXIT.
      *****************************************************************
      *    →処理
      *****************************************************************
       408-IDOU-02-SEC             SECTION.
      *
      *    施設基準なし
           IF     (SPA-GMN-SSTKIJUNCD-G    =   ZERO )
             AND  (SPA-GMN-KIJUNTEIGENCD   =   ZERO )
               GO      TO      408-IDOU-02-EXT
           END-IF
      *
      *
           IF      SPA-NAI-KIJUNFLG    =   11
               MOVE    1                   TO  IDX-STR
           ELSE
               COMPUTE IDX-STR             =   SPA-NAI-KIJUNFLG
                                           +   1
           END-IF
           MOVE    SPACE               TO  SPA-GMN-SELCD
           PERFORM VARYING     IDX     FROM    IDX-STR BY  1
                   UNTIL       SPA-GMN-SELCD   NOT =   SPACE
      *
               IF      IDX             =   11
                   IF      SPA-GMN-KIJUNTEIGENCD   NOT =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KIJUNFLG
                       MOVE    SPA-GMN-KIJUNTEIGENCD
                                               TO  SPA-GMN-SELCD
                   ELSE
                       MOVE    0               TO  IDX
                   END-IF
               ELSE
                   IF      SPA-GMN-SSTKIJUNCD(IDX) NOT =   ZERO
                       MOVE    SPA-GMN-SSTKIJUNCD(IDX)
                                               TO  SPA-GMN-SELCD
                       MOVE    IDX             TO  SPA-NAI-KIJUNFLG
                   END-IF
               END-IF
           END-PERFORM
      *
      *
      *****ADD     1                   TO  SPA-NAI-KIJUNFLG
      *    IF      SPA-NAI-KIJUNFLG    >   SPA-KIJUN-MAX
      *        IF      SPA-NAI-KIJUNFLG    <=  10
      *            MOVE    11                  TO  SPA-NAI-KIJUNFLG
      *            IF      SPA-GMN-KIJUNTEIGENCD   =   ZERO
      *                MOVE    1                   TO  SPA-NAI-KIJUNFLG
      *            END-IF
      *        ELSE
      *            MOVE    1                   TO  SPA-NAI-KIJUNFLG
      *        END-IF
      *    END-IF
      *    MOVE    SPACE               TO  SPA-GMN-SELCD
      *    IF      SPA-NAI-KIJUNFLG    =   11
      *        MOVE    SPA-GMN-KIJUNTEIGENCD   TO  SPA-GMN-SELCD
      *    ELSE
      *        MOVE    SPA-NAI-KIJUNFLG        TO  IDX
      *        IF      IDX                     >   ZERO
      *            IF      SPA-GMN-SSTKIJUNCD(IDX) NOT =   ZERO
      *                MOVE    SPA-GMN-SSTKIJUNCD(IDX) TO  SPA-GMN-SELCD
      *            END-IF
      *        END-IF
      *****END-IF
      *
           IF      SPA-GMN-SELCD   NOT =   SPACE
               MOVE    SPA-GMN-SELCD       TO  W03-SELCD
               PERFORM 410-INPUT-CHK-SEC
      *        カーソル移動
               PERFORM 4103-SELCD-CUR-SEC
           END-IF
           .
       408-IDOU-02-EXT.
           EXIT.
      *****************************************************************
      *    診療コード検索処理
      *****************************************************************
       4202-W25-SYORI-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-W25-AREA
           INITIALIZE                  SPA-W25-GMN-AREA
           MOVE    "2"                 TO  SPA-W25-TYPE
      *
           IF      WRK-INPUTCD         NOT =   SPACE
      *        診療行為で検索（オリジナルのＺ９８は内服）
               MOVE    "8"                 TO  SPA-W25-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-W25-INPUTCD
               MOVE    "1"                 TO  SPA-W25-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-W25-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-W25-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-W25-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-W25-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-W25-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-W25-SET-SEC
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-W25-GMN-MAX     =   ZERO )  AND
                      (SPA-W25-CDSYUBETU   =   "J"  )
                   INITIALIZE                  SPA-W25GMN-AREA
                   MOVE    "8"                 TO  SPA-W25-GMN-SYORI
      *            画面編集
                   PERFORM 41011-W25-SET-SEC
               END-IF
           ELSE
               MOVE    "8"                 TO  SPA-W25-GMN-SYORI
      *        画面編集
               PERFORM 41011-W25-SET-SEC
           END-IF
      *
           MOVE    "W03"               TO  SPA-MOTOPG
           MOVE    "W25"               TO  SPA-SAKIPG
           MOVE    "W03"               TO  SPA-W25-MOTOPG
      *
           IF      SPA-W25-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "W25    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4202-W25-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-W25-SET-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-W25-GMN-AREA    TO  SPA-W25-KYOUTU
           MOVE    SPA-W03             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW25"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE            TO  SPA-W03
           MOVE    SPA-W25-KYOUTU      TO  SPA-W25-GMN-AREA
      *
           .
       41011-W25-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
           MOVE    1                TO  FLG-TOUROKU
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               IF      SPA-SYORIFLG     =   ZERO
                   MOVE    "1001"           TO  SPA-WIDCD
               ELSE
                   MOVE    "1003"           TO  SPA-WIDCD
               END-IF
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      * 
      *    施設基準情報削除
           IF      SPA-SYORIFLG        =   ZERO
      *        更新の時のみ
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                      SYS-1006-REC
               MOVE    "1006"              TO  SYS-1006-KANRICD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
               MOVE    SYS-1006-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "del11"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
      *    新規作成
           MOVE    ZERO                TO  IDX
                                           WRK-KBNCD
           PERFORM             UNTIL   IDX            >=  SPA-GMN-MAX
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                      SYS-1006-REC
               MOVE    "1006"                  TO  SYS-1006-KANRICD
               MOVE    SPA-W01-STYUKYMD        TO  SYS-1006-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD        TO  SYS-1006-EDYUKYMD
               ADD     1                       TO  WRK-KBNCD
      *H24.4
      *        1001 →　3001　とする
               IF     (WRK-KBNCD               =   3  )
                   MOVE    7                   TO  WRK-KBNCD
                   MOVE    3000                TO  IDX
               END-IF
      *H26.4
      *        3501 →　8001　とする
      *        IF     (WRK-KBNCD               =   8  )
      *            MOVE    17                  TO  WRK-KBNCD
      *            MOVE    8000                TO  IDX
      *        END-IF
      *H26.4
      *        3601 →　8001　とする
               IF     (WRK-KBNCD               =   9  )
                   MOVE    17                  TO  WRK-KBNCD
                   MOVE    8000                TO  IDX
               END-IF
               IF      WRK-KBNCD               <   10
      *            １桁をキーとする
                   MOVE    WRK-KBNCD-1             TO  SYS-1006-KBNCD
               ELSE
                   MOVE    WRK-KBNCD               TO  SYS-1006-KBNCD
               END-IF
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       500
                   ADD     1               TO  IDX
                   MOVE    SPA-NAI-TSSTKIJUN (IDX)
                                           TO  SYS-1006-SSTKIJUN  (IDY)
               END-PERFORM
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
               MOVE    SYS-1006-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W03 SYSKANRI INS ERR:" MCP-RC
                            ",KEY:" SYS-1006-KEY
                   MOVE    "1003"                  TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           END-PERFORM          
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4901-KANRENCHK-EXT
           END-IF
      *
      *****MOVE    SPACE                TO  SPA-ERRCD
      *
      *    PERFORM VARYING IDX FROM    1   BY  1
      *            UNTIL   IDX >       25
      *        EVALUATE    SPA-GMN-TSSTKIJUN (IDX)
      *            WHEN    ZERO
      *            WHEN    1  
      *                IF      SPA-GMN-TNAME     (IDX) =   SPACE
      *                AND     SPA-GMN-TSSTKIJUN (IDX) =   "1"
      *                   MOVE    "0002"             TO  SPA-ERRCD
      *                   GO  TO  4901-KANRENCHK-EXT
      *                END-IF
      *            WHEN    OTHER
      *                MOVE    "0002"             TO  SPA-ERRCD
      *                GO  TO  4901-KANRENCHK-EXT
      *        END-EVALUATE
      *****END-PERFORM      
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル移動処理
      *****************************************************************
       409-CURSET-SEC                               SECTION.
      *
           MOVE    90                  TO  SPA-GMN-CUR
           .
       409-CURSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1006-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                            BY  "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD 
                   END-IF
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "12"                TO  LNK-DAY1-IRAI
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
     *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W03    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W03
      *
           INITIALIZE                      W03
      *    COMPUTE IDY     =   SPA-GMN-PAGE    *   25
      *    COMPUTE IDZ     =   IDY             -   24
      *
           MOVE    ZERO                 TO  IDX
      **** PERFORM VARYING     IDZ1     FROM    IDZ   BY  1
      ***          UNTIL       IDZ1     >       IDY
           PERFORM VARYING     IDZ1     FROM    1     BY  1
                   UNTIL       IDZ1     >       CONS-GYO-MAX
               ADD     1                        TO  IDX
               MOVE    SPA-GMN-TCD        (IDX) TO  W03-CD       (IDX)
               MOVE    SPA-GMN-TSSTKIJUN  (IDX) TO  W03-SSTKIJUN (IDX)
               MOVE    SPA-GMN-TNAME      (IDX) TO  W03-DSPNAME  (IDX)
      *
               IF      SPA-GMN-TSSTKIJUN  (IDX) =   "1"
                   MOVE    "blue"               TO  W03-DSPNAME-STYLE
                                                                 (IDX)
               ELSE
                   MOVE    "black"              TO  W03-DSPNAME-STYLE
                                                                 (IDX)
               END-IF
      *H28.1
      *        施設基準名なし
               IF     (SPA-GMN-TSSTKIJUN  (IDX) =   "1" )
                  AND (SPA-GMN-TNAME (IDX)  NOT =   SPACE )
                  AND (SPA-GMN-TMEIFLG (IDX)    =   1   )
                   MOVE    "red"                TO  W03-DSPNAME-STYLE
                                                                 (IDX)
               END-IF
      *
               MOVE    "black"             TO  W03-CD-STYLE (IDX)
               MOVE    SPA-GMN-TCD (IDX)   TO  WRK-TCD
               PERFORM VARYING      IDZ2   FROM    1   BY  1
                       UNTIL        IDZ2   >   SPA-KIJUN-MAX
                   IF      SPA-GMN-SSTKIJUNCD (IDZ2)
                                               =   WRK-TCD
                       MOVE    "red"       TO  W03-CD-STYLE (IDX)
                       MOVE    SPA-KIJUN-MAX   TO  IDZ2
                   END-IF
               END-PERFORM
               IF     (SPA-GMN-KIJUNTEIGENCD   NOT =   ZERO )  AND
                      (SPA-GMN-KIJUNTEIGENCD   =   WRK-TCD  )
                   MOVE    "red"               TO  W03-CD-STYLE (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-STYUKYMD     TO  W03-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD     TO  W03-EDYUKYMD
      *
           MOVE    SPA-GMN-SELCD        TO  W03-SELCD
      *
           MOVE    SPA-GMN-SRYCD        TO  W03-SRYCD
           MOVE    SPA-GMN-MEISYO       TO  W03-MEISYO
           MOVE    SPA-GMN-YUKOKIKAN    TO  W03-YUKOKIKAN
           MOVE    SPA-GMN-KIJUNTEIGENCD    TO  W03-TEIGENCD
           PERFORM VARYING     IDX      FROM    1     BY  1
                   UNTIL       IDX      >       10
               MOVE    SPA-GMN-SSTKIJUNCD (IDX) TO  W03-KIJUNCD (IDX)
           END-PERFORM
      *H28.1
      *    表示モード
           EVALUATE   SPA-HYOJI-FLG
               WHEN   0
                   MOVE    "≪全コード≫"    TO  W03-MIDASI-DATA
                   MOVE    "有効分"          TO  W03-B10-LABEL
               WHEN   1
                   MOVE    "≪有効分≫"      TO  W03-MIDASI-DATA
                   MOVE    "設定分"          TO  W03-B10-LABEL
               WHEN   2
                   MOVE    "≪設定分≫"      TO  W03-MIDASI-DATA
                   MOVE    "全コード"        TO  W03-B10-LABEL
           END-EVALUATE
      *
      *    業務権限非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *    開始日がゼロの場合、削除できないこととする
           IF      SPA-W01-STYUKYMD    =   SPACE  OR  ZERO
               MOVE    WIDGET-INSENSITIVE  TO  W03-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W03-B03-STATE
           END-IF
           MOVE    WIDGET-NORMAL       TO  W03-B10-STATE
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET
               IF      WRK-WIDGET2(1:2)    NOT NUMERIC
                   MOVE    ZERO            TO  WRK-WIDGET2
               END-IF
               MOVE    WRK-WIDGET2         TO  SPA-GMN-CUR
               ADD     1                   TO  SPA-GMN-CUR
               IF      SPA-GMN-CUR         =   26
                   MOVE    90              TO  SPA-GMN-CUR
               END-IF
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "SELCD"
                       MOVE    50              TO  SPA-GMN-CUR
                   WHEN    "SRYCD"
                       MOVE    1               TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN   1
                    MOVE  "SSTKIJUN01"    TO  MCP-WIDGET
                WHEN   2
                    MOVE  "SSTKIJUN02"    TO  MCP-WIDGET
                WHEN   3
                    MOVE  "SSTKIJUN03"    TO  MCP-WIDGET
                WHEN   4
                    MOVE  "SSTKIJUN04"    TO  MCP-WIDGET
                WHEN   5
                    MOVE  "SSTKIJUN05"    TO  MCP-WIDGET
                WHEN   6
                    MOVE  "SSTKIJUN06"    TO  MCP-WIDGET
                WHEN   7
                    MOVE  "SSTKIJUN07"    TO  MCP-WIDGET
                WHEN   8
                    MOVE  "SSTKIJUN08"    TO  MCP-WIDGET
                WHEN   9
                    MOVE  "SSTKIJUN09"    TO  MCP-WIDGET
                WHEN   10
                    MOVE  "SSTKIJUN10"    TO  MCP-WIDGET
                WHEN   11
                    MOVE  "SSTKIJUN11"    TO  MCP-WIDGET
                WHEN   12
                    MOVE  "SSTKIJUN12"    TO  MCP-WIDGET
                WHEN   13
                    MOVE  "SSTKIJUN13"    TO  MCP-WIDGET
                WHEN   14
                    MOVE  "SSTKIJUN14"    TO  MCP-WIDGET
                WHEN   15
                    MOVE  "SSTKIJUN15"    TO  MCP-WIDGET
                WHEN   16
                    MOVE  "SSTKIJUN16"    TO  MCP-WIDGET
                WHEN   17
                    MOVE  "SSTKIJUN17"    TO  MCP-WIDGET
                WHEN   18
                    MOVE  "SSTKIJUN18"    TO  MCP-WIDGET
                WHEN   19
                    MOVE  "SSTKIJUN19"    TO  MCP-WIDGET
                WHEN   20
                    MOVE  "SSTKIJUN20"    TO  MCP-WIDGET
                WHEN   21
                    MOVE  "SSTKIJUN21"    TO  MCP-WIDGET
                WHEN   22
                    MOVE  "SSTKIJUN22"    TO  MCP-WIDGET
                WHEN   23
                    MOVE  "SSTKIJUN23"    TO  MCP-WIDGET
                WHEN   24
                    MOVE  "SSTKIJUN24"    TO  MCP-WIDGET
                WHEN   25
                    MOVE  "SSTKIJUN25"    TO  MCP-WIDGET
                 WHEN   50
                    MOVE  "SRYCD"         TO  MCP-WIDGET
                 WHEN   90
                    MOVE  "SELCD"         TO  MCP-WIDGET
                 WHEN   99
                    MOVE  "B12"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
                   MOVE    "選択コード入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "入力エラー"               TO  SPA-ERRMSG
               WHEN    "K002"
                   MOVE    "警告！！施設基準がありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "適用コードに入力がありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "選択コードはありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "診療コード入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "診療コードは点数マスタにありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "施設基準コードがありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "診療コードではありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W03"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします"
                                               TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "新規の登録処理をします"
                                               TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします。よろしいですか？"
                                               TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           EVALUATE    SPA-WIDCD
               WHEN    "1002"
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W03"                TO  SPA-MOTOPG
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
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

