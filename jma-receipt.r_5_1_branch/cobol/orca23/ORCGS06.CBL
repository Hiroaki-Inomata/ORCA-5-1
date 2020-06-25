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
       PROGRAM-ID.                 ORCGS06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納登録
      *  コンポーネント名  : 一括入金画面　（Ｓ０６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/09/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-太田    07/05/20  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    収納共通パラメタ
           COPY    "S01COMMON-SPA".
      *
           COPY    ENUM-VALUE.
      *
      *    スパ領域
       01  SPA-S06.
      *
           03  SPA-S06-AREA.
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-HEAD.
                       09  SPA-GMN-PTID            PIC 9(10).
                       09  SPA-GMN-PTNUM           PIC X(20).
                       09  SPA-GMN-NAME            PIC X(100).
                       09  SPA-GMN-NYUGAIKBN-G.
                           11  SPA-GMN-NYUGAIKBN   PIC X(01).
                           11  SPA-GMN-NYUGAIKBNFL PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEI
                                                   PIC X(08).
                       09  SPA-GMN-LBLMSG          PIC X(50).
                   07  SPA-GMN-INPUT.
                       09  SPA-GMN-SELNUM          PIC 9(03).
                       09  SPA-GMN-RD-NYUKIN-IKT   PIC X(01).
                       09  SPA-GMN-RD-NYUKIN-KBT   PIC X(01).
                       09  SPA-GMN-NYKNKNRKBN-G.
                           11  SPA-GMN-NYKNKNRKBN  PIC X(01).
                           11  SPA-GMN-NYKNKNRKBNFL PIC X(01).
                           11  SPA-GMN-NYKNKNRKBNMEI
                                                   PIC X(40).
                       09  SPA-GMN-IKTSRYYMKBN-G.
                           11  SPA-GMN-IKTSRYYMKBN PIC X(01).
                           11  SPA-GMN-IKTSRYYMKBNFL
                                                   PIC X(01).
                           11  SPA-GMN-IKTSRYYMKBNMEI
                                                   PIC X(30).
                       09  SPA-GMN-IKTSRYYM        PIC X(10).
                       09  SPA-GMN-NYUKINYMD       PIC X(10).
                       09  SPA-GMN-NYKNHOHO-G.
                           11  SPA-GMN-NYKNHOHO    PIC X(02).
                           11  SPA-GMN-NYKNHOHOFL  PIC X(01).
                           11  SPA-GMN-NYKNHOHOMEI
                                                   PIC X(30).
                       09  SPA-GMN-SKYPRTKBN-G.
                           11  SPA-GMN-SKYPRTKBN   PIC X(01).
                           11  SPA-GMN-SKYPRTKBNFL PIC X(01).
                           11  SPA-GMN-SKYPRTKBNMEI
                                                   PIC X(30).
                       09  SPA-GMN-SKYSUMKBN-G.
                           11  SPA-GMN-SKYSUMKBN   PIC X(01).
                           11  SPA-GMN-SKYSUMKBNFL PIC X(01).
                           11  SPA-GMN-SKYSUMKBNMEI
                                                   PIC X(40).
                       09  SPA-GMN-IKT-NYUKIN-VAL.
                           11  SPA-GMN-NYU-NYUKIN      PIC S9(09).
                           11  SPA-GMN-NYU-NYUKIN-MAX  PIC S9(09).
                           11  SPA-GMN-GAI-NYUKIN      PIC S9(09).
                           11  SPA-GMN-GAI-NYUKIN-MAX  PIC S9(09).
                       09  SPA-GMN-IKT-NYUKIN-R REDEFINES
                                                SPA-GMN-IKT-NYUKIN-VAL.
                           11  SPA-GMN-IKT-NYUKIN-OCC  OCCURS  2.
                               13  SPA-GMN-IKT-NYUKIN  PIC S9(09).
                               13  SPA-GMN-IKT-NYUKIN-MAX
                                                       PIC S9(09).
                       09  SPA-GMN-IKT-HENKIN-VAL.
                           11  SPA-GMN-NYU-HENKIN      PIC S9(09).
                           11  SPA-GMN-NYU-HENKIN-MAX  PIC S9(09).
                           11  SPA-GMN-GAI-HENKIN      PIC S9(09).
                           11  SPA-GMN-GAI-HENKIN-MAX  PIC S9(09).
                       09  SPA-GMN-IKT-HENKIN-R REDEFINES
                                                SPA-GMN-IKT-HENKIN-VAL.
                           11  SPA-GMN-IKT-HENKIN-OCC  OCCURS  2.
                               13  SPA-GMN-IKT-HENKIN  PIC S9(09).
                               13  SPA-GMN-IKT-HENKIN-MAX
                                                       PIC S9(09).
                       09  SPA-GMN-KBT-HENKIN-VAL.
                           11  SPA-GMN-NYUKIN      PIC S9(09).
                           11  SPA-GMN-NYUKIN-MAX  PIC S9(09).
                   07  SPA-GMN-MISYU.
                       09  SPA-GMN-GAI-MISYU       PIC S9(08).
                       09  SPA-GMN-NYU-MISYU       PIC S9(08).
                   07  SPA-GMN-NYUGAIKBNLST-G.
                       09  SPA-GMN-NYUGAIKBN-MAX   PIC 9(03).
                       09  SPA-GMN-NYUGAIKBNLST    OCCURS  3.
                           11  SPA-GMN-NYUGAIKBNL  PIC X(01).
                           11  SPA-GMN-NYUGAIKBNFLL PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEIL
                                                   PIC X(08).
                   07  SPA-GMN-NYKNKNRKBNLST-G.
                       09  SPA-GMN-NYKNKNRKBN-MAX  PIC 9(03).
                       09  SPA-GMN-NYKNKNRKBNLST   OCCURS  5.
                           11  SPA-GMN-NYKNKNRKBNL PIC X(01).
                           11  SPA-GMN-NYKNKNRKBNFLL
                                                   PIC X(01).
                           11  SPA-GMN-NYKNKNRKBNMEIL
                                                   PIC X(40).
                   07  SPA-GMN-IKTSRYYMKBNLST-G.
                       09  SPA-GMN-IKTSRYYMKBN-MAX PIC 9(03).
                       09  SPA-GMN-IKTSRYYMKBNLST  OCCURS  5.
                           11  SPA-GMN-IKTSRYYMKBNL PIC X(01).
                           11  SPA-GMN-IKTSRYYMKBNFLL
                                                   PIC X(01).
                           11  SPA-GMN-IKTSRYYMKBNMEIL
                                                   PIC X(30).
                   07  SPA-GMN-NYKNHOHOLST-G.
                       09  SPA-GMN-NYKNHOHO-MAX    PIC 9(03).
                       09  SPA-GMN-NYKNHOHOLST     OCCURS 50.
                           11  SPA-GMN-NYKNHOHOL   PIC X(02).
                           11  SPA-GMN-NYKNHOHOFLL PIC X(01).
                           11  SPA-GMN-NYKNHOHOMEIL
                                                   PIC X(30).
                   07  SPA-GMN-SKYPRTKBNLST-G.
                       09  SPA-GMN-SKYPRTKBN-MAX   PIC 9(03).
                       09  SPA-GMN-SKYPRTKBNLST    OCCURS  5.
                           11  SPA-GMN-SKYPRTKBNL  PIC X(01).
                           11  SPA-GMN-SKYPRTKBNFLL PIC X(01).
                           11  SPA-GMN-SKYPRTKBNMEIL
                                                   PIC X(30).
                   07  SPA-GMN-SKYSUMKBNLST-G.
                       09  SPA-GMN-SKYSUMKBN-MAX   PIC 9(03).
                       09  SPA-GMN-SKYSUMKBNLST    OCCURS  5.
                           11  SPA-GMN-SKYSUMKBNL  PIC X(01).
                           11  SPA-GMN-SKYSUMKBNFLL PIC X(01).
                           11  SPA-GMN-SKYSUMKBNMEIL
                                                   PIC X(40).
                   07  SPA-GMN-SYULIST.
                       09  SPA-GMN-SYULIST-MAX     PIC 9(05).
                       09  SPA-GMN-SYULIST-OCC     OCCURS   401.
                           11  SPA-GMN-TDENPNUM    PIC 9(07).
                           11  SPA-GMN-TNYUGAIKBN  PIC X(04).
                           11  SPA-GMN-TSRYKA      PIC X(06).
                           11  SPA-GMN-THKNCOMBI   PIC X(50).
                           11  SPA-GMN-TFTNRATE    PIC X(08).
                           11  SPA-GMN-TSRYYMD     PIC X(15).
                           11  SPA-GMN-TSKYEDYMD   PIC X(06).
                           11  SPA-GMN-THAKYMD     PIC X(09).
                           11  SPA-GMN-TSEIKYU     PIC S9(10).
                           11  SPA-GMN-TMISYU      PIC S9(10).
                           11  SPA-GMN-TNYUKIN     PIC S9(10).
      *
      *                コラムリストに表示させる収納の連番情報
                       09  SPA-GMN-SYULIST-OUT-G.
                           11  SPA-GMN-SYULIST-OUT-MAX
                                                   PIC 9(05).
                           11  SPA-GMN-SYULIST-OUT-OCC OCCURS  400.
                               13  SPA-GMN-SYULIST-OUT-IDX
                                                   PIC 9(03).
      *
                       09  SPA-GMN-SYULIST-SEL     PIC 9(05).
      *
                   07  SPA-NAI-SYULIST.
                       09  SPA-NAI-SYULIST-OCC     OCCURS  401.
                           11  SPA-NAI-TSRYKA      PIC  X(02).
                           11  SPA-NAI-THKNCOMBI   PIC  X(04).
                           11  SPA-NAI-TSRYYMD     PIC  X(08).
                           11  SPA-NAI-THAKYMD     PIC  X(08).
                           11  SPA-NAI-TNYUGAIKBN  PIC  X(01).
                           11  SPA-NAI-TCREATEKBN  PIC  X(01).
                           11  SPA-NAI-TDENPJTIKBN PIC  X(01).
                           11  SPA-NAI-THKNNUM     PIC  X(03).
                           11  SPA-NAI-TRSI-HKNKBN PIC  X(01).
                           11  SPA-NAI-TSKYMONEY-TAX-SAI
                                                   PIC  S9(10).
                           11  SPA-NAI-TNYUKIN-TOTAL
                                                   PIC  S9(10).
                           11  SPA-NAI-TSRT-KEY.
                               13  SPA-NAI-TSRT-DENPPRTYMD
                                                   PIC  X(08).
                               13  SPA-NAI-TSRT-NYUGAIKBN
                                                   PIC  X(01).
                               13  SPA-NAI-TSRT-DENPNUM
                                                   PIC  9(07).
                           11  SPA-NAI-TSRT2-KEY.
                               13  SPA-NAI-TSRT2-SRYYMD
                                                   PIC  X(08).
                               13  SPA-NAI-TSRT2-NYUGAIKBN
                                                   PIC  X(01).
                               13  SPA-NAI-TSRT2-DENPNUM
                                                   PIC  9(07).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-HEAD.
                       09  SPA-NAI-MISYUFLG        PIC  9(01).
                   07  SPA-NAI-INPUT.
                       09  SPA-NAI-IKTSRYYM        PIC  X(08).
                       09  SPA-NAI-NYUKINYMD       PIC  X(08).
                       09  SPA-NAI-SKYSUMKBN-BF    PIC  X(01).
                       09  SPA-NAI-NYKNKNRKBN-BF   PIC  X(01).
                       09  SPA-NAI-KBT-NYUKIN-VAL.
                           11  SPA-NAI-HENKINFLG   PIC  9(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-HKNCOMBI                        PIC 9(01).
           03  FLG-PTRSIINF                        PIC 9(01).
           03  FLG-SYULIST-HEN                     PIC 9(01).
           03  FLG-LOOP-END                        PIC 9(01).
           03  FLG-GMN-INIT                        PIC 9(01).
           03  FLG-GAI-SKYPRT                      PIC 9(01).
           03  FLG-NYU-SKYPRT                      PIC 9(01).
           03  FLG-SORT                            PIC 9(01).
           03  FLG-TARGET                          PIC 9(01).
           03  FLG-NYUHENKIN                       PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDX-OUT                             PIC 9(05).
           03  IDX-LST                             PIC 9(05).
           03  IDX-SRT                             PIC 9(05).
           03  IDX-TMP                             PIC 9(05).
           03  IDX-KBT                             PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-KINGAKU                 PIC ---,---,---.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-NYUGAIKBN               PIC X(01).
           03  WRK-DENPNUM                 PIC 9(07).
           03  WRK-SIDMSG                  PIC X(70).
           03  WRK-STIDX                   PIC 9(05).
      *
      *    保険組合せ名称
           03  WRK-KUMINO                  PIC X(04).
           03  WRK-HKNKUMI                 PIC X(40).
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-RSI-HKNKBN              PIC X(01).
           03  WRK-HKNID                   PIC 9(10).
           03  WRK-HKNKBN-MEI              PIC X(02).
           03  WRK-ZZZ                     PIC ZZZ.
           03  WRK-NYUKIN                  PIC 9(10).
           03  WRK-NYU-SEIKYU-PG           PIC X(20).
           03  WRK-GAI-SEIKYU-PG           PIC X(20).
           03  WRK-DENPITEM-G.
               05  WRK-NYU-NYUKIN          PIC S9(10).
               05  WRK-NYU-HENKIN          PIC S9(10).
               05  WRK-GAI-NYUKIN          PIC S9(10).
               05  WRK-GAI-HENKIN          PIC S9(10).
               05  WRK-GAI-DENPNUM         PIC 9(07).
               05  WRK-NYU-DENPNUM         PIC 9(07).
               05  WRK-GAI-SRYYMD          PIC X(08).
               05  WRK-NYU-SRYYMD          PIC X(08).
               05  WRK-GAI-KONSEIKYU       PIC S9(10).
               05  WRK-NYU-KONSEIKYU       PIC S9(10).
               05  WRK-GAI-MISYU-P         PIC S9(10).
               05  WRK-GAI-MISYU-M         PIC S9(10).
           03  WRK-FILEMEI  .
               05  WRK-FILEMEI-YMD         PIC X(08).
               05  WRK-FILEMEI-HMS         PIC X(06).
               05  WRK-FILEMEI-CD          PIC X(06).
           03  WRK-SYU-DENPNUM             PIC 9(07).
           03  WRK-NUM                     PIC S9(10).
           03  WRK-NUMZL                   PIC ZZ,ZZZ,ZZ9.
           03  WRK-NUMZS                   PIC ZZZ,ZZ9.
           03  WRK-STR                     PIC X(500).
           03  WRK-STR-Z   REDEFINES   WRK-STR.
               05  WRK-STR-ZX99            PIC Z(9)9.
               05  WRK-STR-FL              PIC X(490).
           03  WRK-STR-TMP                 PIC X(500).
           03  WRK-LBLKINGAKU              PIC S9(9).
           03  WRK-LBLKINGAKUX             PIC X(20).
           03  WRK-MISYU                   PIC S9(9).
      *
       01  WRK-ROW-AREA.
           03  WRK-SYULIST-ROW             PIC S9(9)   BINARY.
      *
       01  WK-GMN-RITU.
           03  WK-GMN-FTNRITU              PIC ZZZ.
           03  WK-GMN-PA                   PIC X(01).
           03  WK-GMN-FL                   PIC X(01).
      *
       01  S1005-SRYKALST-G.
           03  S1005-SRYKALST-MAX          PIC 9(05).
           03  S1005-SRYKALST-OCC          OCCURS 50.
               05  S1005-SRYKAL            PIC X(02).
               05  S1005-SRYKAL-STYUKYMD   PIC X(08).
               05  S1005-SRYKAL-EDYUKYMD   PIC X(08).
               05  S1005-SRYKAMEIL         PIC X(12).
      *
       01  KEY-AREA.
           03  KEY-GAI-MISYU           OCCURS 2.
               05  KEY-SRYYMD          PIC X(08).
               05  KEY-DENPNUM         PIC 9(07).
           03  KEY-GAI-SKYEDYMD        PIC X(08).
      *
       01  TSRT-AREA.
           03  TSRT-MAX                    PIC 9(05).
           03  TSRT-OCC                    OCCURS 401.
               05  TSRT-KEY.
                   07  TSRT-SRYYMD         PIC X(08).
                   07  TSRT-SRYKA          PIC X(02).
                   07  TSRT-HKNCOMBI       PIC X(04).
                   07  TSRT-DENPNUM        PIC 9(07).
               05  TSRT-NYUKIN             PIC S9(10).
      *
       01  CONST-AREA.
           03  CONST-MODE-NORMON           PIC 9(01) VALUE 0.
           03  CONST-SYULIST-MAX           PIC 9(05)   VALUE 400.
           03  CONST-SYULIST-OUT-MAX       PIC 9(05)   VALUE 400.
      *
      *    スパ領域 ワーク
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WRK-SPA-//.
      *
      *************************************************************
      *    ファイルレイアウト
      *************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1032.INC".
           COPY    "CPSK1035.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK1041.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    パラメタテーブル	
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
      *
      *************************************************************
      *    サブプロ用　領域
      *************************************************************
      *
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
      *
      *    入院請求書印刷パラメタ
           COPY    "CPORCHCN03.INC".
      *
      *    まとめ入金編集サブ
           COPY    "CPORCSNYUKINS.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険組み合せ取得
           COPY    "CPORCSHKNSET.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角・半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    未収金取得サブ
           COPY    "CPORCSMISYU.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
           COPY    "CPORCHC03S02.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *************************************************************
      *    連絡　領域
      *************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *************************************************************
      *    主　　処理
      *************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE        TO  SPA-S06
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *    INITIALIZE                  CNT-AREA
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
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *************************************************************
      *    初期　処理
      *************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "SERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
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
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "S06"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *
      *************************************************************
      *    初期画面処理
      *************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "SID1"
               PERFORM 330-SID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM                 310-SPA-SET-SEC
      *
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＳＩＤ１）ＯＫ処理
      *****************************************************************
       330-SID1-SET-SEC            SECTION.
      *
           IF      SPA-SID1-FLG    =   "OK"
               IF      SPA-SIDCD            =   "2001"
                   PERFORM 4801-TOUROKU-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID1-SET-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           INITIALIZE                  SPA-S06.
      *
           MOVE    SPA-S01-PTID        TO  SPA-GMN-PTID
           MOVE    SPA-S01-PTNUM       TO  SPA-GMN-PTNUM
           MOVE    SPA-S01-NAME        TO  SPA-GMN-NAME
      *
      *    収納機能情報取得
           PERFORM 3101-SYS-1039-GET-SEC
      *
      *    入外区分コンボ編集
           PERFORM 3101-NYUGAIKBNLST-HEN-SEC
      *
      *    入金管理区分コンボ編集
           PERFORM 3101-NYKNKNRKBNLST-HEN-SEC
      *
      *    診療年月指定コンボ編集
           PERFORM 3101-IKTSRYYMKBNLST-HEN-SEC
      *
      *    入金方法コンボ編集
           PERFORM 3101-NYKNHOHOLST-HEN-SEC
      *
      *    請求書発行区分コンボ編集
           PERFORM 3101-SKYPRTKBNLST-HEN-SEC
      *
      *    請求書集計区分コンボ編集
           PERFORM 3101-SKYSUMKBNLST-HEN-SEC
      *
      *    入外区分コンボ初期値
           MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
      *
      *    入金管理区分コンボ初期値
           MOVE    SYS-1039-NYKNKNRKBN TO  WRK-CMB-CD
           PERFORM 4200-NYKNKNRKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-NYKNKNRKBNLST (1)
                                       TO  SPA-GMN-NYKNKNRKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYKNKNRKBN-G
           END-IF
           MOVE    SPA-GMN-NYKNKNRKBN  TO  SPA-NAI-NYKNKNRKBN-BF
      *
      *    診療年月指定コンボ初期値
           MOVE    SYS-1039-IKTSRYYMKBN    TO  WRK-CMB-CD
           PERFORM 4200-IKTSRYYMKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-IKTSRYYMKBNLST (1)
                                       TO  SPA-GMN-IKTSRYYMKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-IKTSRYYMKBN-G
           END-IF
      *
           IF    ( SPA-S01-MODE            =   CONST-MODE-NORMON )
            AND  ( SPA-S01-SRYYM (1:6) NOT =   SPACE )
            AND  ( SPA-S01-SRYYM (7: )     =   SPACE )
               MOVE    SPA-S01-SRYYM       TO  SPA-NAI-IKTSRYYM
               MOVE    SPA-S01-SRYYMW      TO  SPA-GMN-IKTSRYYM
           ELSE
               MOVE    SPA-SYSYMD   (1:6)  TO  SPA-NAI-IKTSRYYM
               MOVE    SPA-SYSYMDWH (1:6)  TO  SPA-GMN-IKTSRYYM
           END-IF
      *
      *    入金方法コンボ初期値
           IF    ( SPA-GMN-NYKNHOHO-MAX    >   ZERO )
               MOVE    SPA-S01-NYUKIN-HOHO TO  WRK-CMB-CD
               PERFORM 4200-NYKNHOHOLST-CHK-SEC
               IF    ( WRK-CMB-ITM =   SPACE )
                   MOVE    SPA-GMN-NYKNHOHOLST (1)
                                       TO  SPA-GMN-NYKNHOHO-G
               ELSE
                   MOVE    WRK-CMB-ITM
                                       TO  SPA-GMN-NYKNHOHO-G
               END-IF
           END-IF
      *
      *    請求書兼領収書発行区分コンボ初期値
           MOVE    SYS-1039-S06SKYPRTGKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-SKYPRTKBNLST (1)
                                       TO  SPA-GMN-SKYPRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKYPRTKBN-G
           END-IF
      *
      *    請求書兼領収書集計区分コンボ初期値
           MOVE    SYS-1039-S06SKYSUMKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKYSUMKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-SKYSUMKBNLST (1)
                                           TO  SPA-GMN-SKYSUMKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKYSUMKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SKYSUMKBN   TO  SPA-NAI-SKYSUMKBN-BF
      *
           MOVE    "T"                 TO  SPA-GMN-RD-NYUKIN-IKT
           MOVE    "F"                 TO  SPA-GMN-RD-NYUKIN-KBT
      *
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-NYUKINYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-NYUKINYMD
      *
      *    初期表示処理
           PERFORM 310-SYOKI-HYOJI-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *************************************************************
      *    初期表示処理
      *************************************************************
       310-SYOKI-HYOJI-SEC             SECTION.
      *
      *    検索結果リスト編集処理
           PERFORM 3101-SYULIST-HEN-SEC
      *
      *    収納情報リスト画面表示編集処理
           PERFORM 3101-SYULIST-OUT-HEN-SEC
      *
      *    未収額合計編集処理
           PERFORM 3101-MISYU-HEN-SEC
      *
      *    初期表示時カーソル位置編集処理
           PERFORM 3101-SYOKI-CUR-HEN-SEC
      *
           IF    ( SPA-GMN-SYULIST-MAX =   ZERO )
               MOVE    1               TO  SPA-NAI-MISYUFLG
               MOVE    "未収金（過入金）はありません"
                                       TO  SPA-GMN-LBLMSG
           END-IF
      *
           .
       310-SYOKI-HYOJI-EXT.
           EXIT.
      *************************************************************
      *    収納機能情報取得処理
      *************************************************************
       3101-SYS-1039-GET-SEC           SECTION.
      *
           INITIALIZE  SYS-1039-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1039-HOSPNUM
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1039-REC
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1039-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       3101-SYS-1039-GET-EXT.
           EXIT.
      *************************************************************
      *    検索結果リスト編集処理（初期表示時）
      *************************************************************
       3101-SYULIST-HEN-SEC            SECTION.
      *
           INITIALIZE              SPA-GMN-SYULIST
                                   SPA-NAI-SYULIST
      *
      *    診療科ワーク編集処理
           PERFORM 3101-SRYKALST-HEN-SEC
      *
      *    保険組み合わせ編集処理
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
           MOVE    ZERO                    TO  FLG-SYUNOU
           INITIALIZE                          SYUNOU-REC
      *
           MOVE    SPA-HOSPNUM              TO  SYU-HOSPNUM
           MOVE    SPA-S01-PTID            TO  SYU-PTID
           MOVE    SYUNOU-REC              TO  MCPDATA-REC
           MOVE    "tbl_syunou"            TO  MCP-TABLE
           MOVE    "key24"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    1                   TO  FLG-SYUNOU
           ELSE
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( SPA-GMN-SYULIST-MAX
                                           >=  CONST-SYULIST-MAX )
      *
               PERFORM 31011-SYULIST-HEN-SEC
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    1               TO  FLG-SYUNOU
               ELSE
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYULIST-HEN-EXT.
           EXIT.
      *************************************************************
      *    診療科ワーク編集処理
      *************************************************************
       3101-SRYKALST-HEN-SEC           SECTION.
      *
           INITIALIZE  S1005-SRYKALST-G
      *
           INITIALIZE  SYS-1005-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( S1005-SRYKALST-MAX   >=   50   )
      *
               COMPUTE S1005-SRYKALST-MAX
                   =   S1005-SRYKALST-MAX    +   1
      *
               MOVE    S1005-SRYKALST-MAX  TO  IDXA
      *
               MOVE    SYS-1005-KBNCD      TO  S1005-SRYKAL  (IDXA)
               MOVE    SYS-1005-STYUKYMD   TO  S1005-SRYKAL-STYUKYMD
                                                             (IDXA)
               MOVE    SYS-1005-EDYUKYMD   TO  S1005-SRYKAL-EDYUKYMD
                                                             (IDXA)
               MOVE    SYS-1005-SRYKANAME1 TO  S1005-SRYKAMEIL
                                                             (IDXA)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1               TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC     TO  SYS-1005-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SRYKALST-HEN-EXT.
           EXIT.
      *************************************************************
      *    保険組み合わせ編集処理
      *************************************************************
       3101-HKNCOMBI-HEN-SEC           SECTION.
      *
           INITIALIZE                  ORCSHKNSETAREA
                                       WRK-SPA-AREA
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           MOVE    SPA-S01-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-S01-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           .
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *************************************************************
      *    検索結果リスト編集処理
      *************************************************************
       31011-SYULIST-HEN-SEC           SECTION.
      *
      *    収納マスタ内容編集
           COMPUTE SPA-GMN-SYULIST-MAX
               =   SPA-GMN-SYULIST-MAX    +   1
      *
           MOVE    SPA-GMN-SYULIST-MAX TO  IDX-LST
      *
           MOVE    SYU-DENPNUM         TO  SPA-GMN-TDENPNUM (IDX-LST)
      *
      *    入外区分
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN  (IDX-LST)
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"            TO  SPA-GMN-TNYUGAIKBN  (IDX-LST)
           WHEN    "2"
               MOVE    "外"            TO  SPA-GMN-TNYUGAIKBN  (IDX-LST)
           END-EVALUATE
      *
      *    作成区分
           MOVE    SYU-CREATEKBN       TO  SPA-NAI-TCREATEKBN  (IDX-LST)
      *
      *    状態区分
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-TDENPJTIKBN (IDX-LST)
      *
      *    診療科名
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   S1005-SRYKALST-MAX )
      *
               IF    ( SYU-SRYKA       =   S1005-SRYKAL (IDX1))
                AND  ( SYU-SRYYMD     >=   S1005-SRYKAL-STYUKYMD (IDX1))
                AND  ( SYU-SRYYMD     <=   S1005-SRYKAL-EDYUKYMD (IDX1))
                   MOVE    S1005-SRYKAMEIL (IDX1)
                                       TO  SPA-GMN-TSRYKA (IDX-LST)
                   MOVE    S1005-SRYKALST-MAX
                                       TO  IDX1
               END-IF
      *
           END-PERFORM
           MOVE    SYU-SRYKA           TO  SPA-NAI-TSRYKA (IDX-LST)
      *
      *    診療日
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    SYU-SKYSTYMD    TO  SPA-NAI-TSRYYMD   (IDX-LST)
               MOVE    SYU-SKYSTYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD   (IDX-LST)
               MOVE    SYU-SKYEDYMD    TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    "-"             TO  SPA-GMN-TSRYYMD (IDX-LST)
                                                           (10:1)
               MOVE    WRK-HENYMDG (5:5)
                                       TO  SPA-GMN-TSRYYMD (IDX-LST)
                                                           (11:5)
           WHEN    "2" 
               MOVE    SYU-SRYYMD      TO  SPA-NAI-TSRYYMD (IDX-LST)
               MOVE    SYU-SRYYMD      TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD (IDX-LST)
           END-EVALUATE
      *
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  SPA-NAI-THAKYMD (IDX-LST)
           MOVE    SYU-DENPPRTYMD      TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-THAKYMD (IDX-LST)
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SPA-GMN-TSEIKYU (IDX-LST)
      *    未収金額
           COMPUTE SPA-GMN-TMISYU(IDX-LST) =   SYU-SKYMONEY
                                           -   SYU-NYUKIN-TOTAL
      *    入金額
           MOVE    ZERO                TO  SPA-GMN-TNYUKIN (IDX-LST)
      *
      *    保険組合せより（保険と負担）
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KUMINO
           PERFORM 31011-HKNCOMBIMEI-HEN-SEC
           MOVE    WRK-HKNKUMI         TO  SPA-GMN-THKNCOMBI (IDX-LST)
           MOVE    WRK-HKNNUM          TO  SPA-NAI-THKNNUM   (IDX-LST)
           MOVE    WRK-RSI-HKNKBN      TO  SPA-NAI-TRSI-HKNKBN(IDX-LST)
           MOVE    SYU-HKNCOMBINUM     TO  SPA-NAI-THKNCOMBI(IDX-LST)
      *
      *    患者負担率
           IF    ( SYU-PTFTNRATE       =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-TFTNRATE (IDX-LST)
           ELSE
               MOVE    SPACE           TO  WK-GMN-RITU
               MOVE    SYU-PTFTNRATE   TO  WK-GMN-FTNRITU
               MOVE    "%"             TO  WK-GMN-PA
               MOVE    WK-GMN-RITU     TO  SPA-GMN-TFTNRATE (IDX-LST)
           END-IF
      *
      *    請求額−消費税（再掲）
           MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  SPA-NAI-TSKYMONEY-TAX-SAI
                                                            (IDX-LST)
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-NAI-TNYUKIN-TOTAL
                                                            (IDX-LST)
      *
           MOVE    SYU-DENPPRTYMD      TO  SPA-NAI-TSRT-DENPPRTYMD
                                                            (IDX-LST)
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TSRT-NYUGAIKBN
                                                            (IDX-LST)
           MOVE    SYU-DENPNUM         TO  SPA-NAI-TSRT-DENPNUM
                                                            (IDX-LST)
      *
           MOVE    SYU-SRYYMD          TO  SPA-NAI-TSRT2-SRYYMD
                                                            (IDX-LST)
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TSRT2-NYUGAIKBN
                                                            (IDX-LST)
           MOVE    SYU-DENPNUM         TO  SPA-NAI-TSRT2-DENPNUM
                                                            (IDX-LST)
      *
           .
       31011-SYULIST-HEN-EXT.
           EXIT.
      *************************************************************
      *    保険組み合わせ名称編集処理
      *************************************************************
       31011-HKNCOMBIMEI-HEN-SEC       SECTION.
      *
      *    対象の保険組み合わせが存在すればその名称を、
      *    なければ保険組み合わせ番号を表示する
      *
           MOVE    SPACE           TO  WRK-HKNKUMI
                                       WRK-HKNNUM
                                       WRK-RSI-HKNKBN
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB   >    100 )
      *
               IF    ( WRK-KUMINO  =   ORCSHKN-GMN-HKNCOMBINUM (IDXB))
                   MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDXB)
                                       TO  WRK-HKNKUMI
                   MOVE    ORCSHKN-NAI-HKNNUM  (IDXB)
                                       TO  WRK-HKNNUM
                   MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDXB)
                                       TO  WRK-RSI-HKNKBN
                   MOVE    100         TO  IDXB
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-HKNKUMI     =   SPACE )
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
               IF    ( FLG-HKNCOMBI    =   ZERO )
                   PERFORM 310111-HKNCOMBIMEI-HEN-SEC
               ELSE
                   MOVE    WRK-KUMINO  TO  WRK-HKNKUMI
               END-IF
           END-IF
      *
           .
       31011-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      **************************************************************
      *    保険組合せ名称編集処理
      **************************************************************
       310111-HKNCOMBIMEI-HEN-SEC      SECTION.
      *
           MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
      *
           EVALUATE    TRUE
           WHEN    ( COMB-HKNNUM =   "971" OR "973" )
               MOVE    COMB-HKNID  TO  WRK-HKNID
               PERFORM 900-PTRSIINF-KEY-SEL-SEC
               IF    ( FLG-PTRSIINF    =   ZERO )
                   MOVE    PTRSI-HKNKBN    TO  WRK-RSI-HKNKBN
      *
                   MOVE    SPACE   TO  WRK-HKNKBN-MEI
                   EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"    TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"    TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"    TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"    TO  WRK-HKNKBN-MEI
                   END-EVALUATE
                   IF    ( PTRSI-COMMENT   =   SPACE )
                       MOVE    SPACE   TO  WRK-HKNKUMI
                       STRING  COMB-SYU-TANSEIDONAME
                                               DELIMITED  BY  SPACE
                               " "             DELIMITED  BY  SIZE
                               WRK-HKNKBN-MEI  DELIMITED  BY  SPACE
                       INTO    WRK-HKNKUMI
                       END-STRING
                   ELSE
                       MOVE    SPACE   TO  WRK-HKNKUMI
                       STRING  COMB-SYU-TANSEIDONAME
                                               DELIMITED  BY  SPACE
                               " "             DELIMITED  BY  SIZE
                               WRK-HKNKBN-MEI
                                               DELIMITED  BY  SPACE
                               "（"            DELIMITED  BY  SIZE
                               PTRSI-COMMENT(1:24)
                                               DELIMITED  BY  SPACE
                               "）"            DELIMITED  BY  SIZE
                       INTO    WRK-HKNKUMI
                       END-STRING
                   END-IF
               END-IF
           WHEN    ( COMB-SYU-TANSEIDONAME   =   SPACE )
               MOVE    SPACE               TO  WRK-HKNKUMI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
               INTO    WRK-HKNKUMI
               END-STRING
           WHEN    OTHER
               MOVE    SPACE   TO  WRK-HKNKUMI
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
               INTO    WRK-HKNKUMI
               END-STRING
           END-EVALUATE
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HKNKUMI         TO  KANACHK-MAE-INPUT
           MOVE    50                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNKUMI
      *
           .
       310111-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *************************************************************
      *    入金上限額編集処理
      *************************************************************
       3101-NYUKIN-MAX-HEN-SEC     SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-NYU-NYUKIN
                                       SPA-GMN-GAI-NYUKIN
                                       SPA-GMN-NYU-NYUKIN-MAX
                                       SPA-GMN-GAI-NYUKIN-MAX
                                       SPA-GMN-NYUKIN
                                       SPA-GMN-NYUKIN-MAX
                                       SPA-NAI-HENKINFLG
                                       SPA-GMN-NYU-HENKIN
                                       SPA-GMN-GAI-HENKIN
                                       SPA-GMN-NYU-HENKIN-MAX
                                       SPA-GMN-GAI-HENKIN-MAX
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
               MOVE    ZERO        TO  FLG-NYUHENKIN
               PERFORM 31011-NYUKIN-MAX-IKT-HEN-SEC
           ELSE
               PERFORM 31011-SYULIST-OUT-IDX-CHK-SEC
               IF    ( IDX-OUT     NOT =   ZERO )
                   MOVE    IDX-OUT     TO  IDX-KBT
                   PERFORM 31011-NYUKIN-MAX-KBT-HEN-SEC
               END-IF
           END-IF
      *
           .
       3101-NYUKIN-MAX-HEN-EXT.
           EXIT.
      *************************************************************
      *    入金上限額（一括）編集処理
      *************************************************************
       31011-NYUKIN-MAX-IKT-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX-LST    FROM     1   BY  1
                   UNTIL ( IDX-LST >  SPA-GMN-SYULIST-MAX )
      *
               MOVE    1           TO FLG-TARGET
      *
               IF    ( SPA-GMN-IKTSRYYMKBN     =   "2" )
                   IF    ( SPA-NAI-IKTSRYYM    =   SPA-NAI-TSRYYMD
                                                      (IDX-LST)(1:6))
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-TARGET
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET      =   1 )
                   IF    ( FLG-NYUHENKIN        =   ZERO )
                       IF    ( SPA-GMN-TMISYU (IDX-LST)    >   ZERO )
                           IF    ( SPA-NAI-TNYUGAIKBN (IDX-LST)
                                                   =   "1" )
                               COMPUTE SPA-GMN-NYU-NYUKIN-MAX
                                   =   SPA-GMN-NYU-NYUKIN-MAX
                                   +   SPA-GMN-TMISYU  (IDX-LST)
                           ELSE
                               COMPUTE SPA-GMN-GAI-NYUKIN-MAX
                                   =   SPA-GMN-GAI-NYUKIN-MAX
                                   +   SPA-GMN-TMISYU  (IDX-LST)
                           END-IF
                       ELSE
                           IF    ( SPA-NAI-TNYUGAIKBN (IDX-LST)
                                                   =   "1" )
                               COMPUTE SPA-GMN-NYU-HENKIN-MAX
                                   =   SPA-GMN-NYU-HENKIN-MAX
                                   + ( SPA-GMN-TMISYU  (IDX-LST) * -1 )
                           ELSE
                               COMPUTE SPA-GMN-GAI-HENKIN-MAX
                                   =   SPA-GMN-GAI-HENKIN-MAX
                                   + ( SPA-GMN-TMISYU  (IDX-LST) * -1 )
                           END-IF
                       END-IF
                   ELSE
                       COMPUTE SPA-GMN-TNYUKIN (IDX-LST)
                           =   SPA-GMN-TNYUKIN (IDX-LST)
                           +   SPA-GMN-TMISYU (IDX-LST)
                       MOVE    ZERO        TO  SPA-GMN-TMISYU (IDX-LST)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       31011-NYUKIN-MAX-IKT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入金上限額（個別）編集処理
      *****************************************************************
       31011-NYUKIN-MAX-KBT-HEN-SEC SECTION.
      *
           COMPUTE SPA-GMN-NYUKIN-MAX
               =   SPA-GMN-TMISYU (IDX-KBT)
               +   SPA-GMN-TNYUKIN (IDX-KBT)
      *
           IF    ( SPA-GMN-TNYUKIN (IDX-KBT)   =   ZERO )
               MOVE    SPA-GMN-NYUKIN-MAX
                                       TO  SPA-GMN-NYUKIN
           ELSE
               MOVE    SPA-GMN-TNYUKIN (IDX-KBT)
                                       TO  SPA-GMN-NYUKIN
           END-IF
      *
           IF    ( SPA-GMN-TMISYU (IDX-KBT)    <   ZERO )
            OR   ( SPA-GMN-TNYUKIN(IDX-KBT)    <   ZERO )
               MOVE    1               TO  SPA-NAI-HENKINFLG
               COMPUTE SPA-GMN-NYUKIN-MAX  =   SPA-GMN-NYUKIN-MAX * -1
               COMPUTE SPA-GMN-NYUKIN      =   SPA-GMN-NYUKIN * -1
           END-IF
      *
           .
       31011-NYUKIN-MAX-KBT-HEN-EXT.
           EXIT.
      *************************************************************
      *    入外区分コンボ編集処理
      *************************************************************
       3101-NYUGAIKBNLST-HEN-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUGAIKBNLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-NYUGAIKBNL       (1)
           MOVE    "全部"              TO  SPA-GMN-NYUGAIKBNMEIL    (1)
           MOVE    "1"                 TO  SPA-GMN-NYUGAIKBNL       (2)
           MOVE    "入院"              TO  SPA-GMN-NYUGAIKBNMEIL    (2)
           MOVE    "2"                 TO  SPA-GMN-NYUGAIKBNL       (3)
           MOVE    "外来"              TO  SPA-GMN-NYUGAIKBNMEIL    (3)
      *
           MOVE    3                   TO  SPA-GMN-NYUGAIKBN-MAX
      *
           .
       3101-NYUGAIKBNLST-HEN-EXT.
           EXIT.
      *************************************************************
      *    入金管理区分コンボ編集処理
      *************************************************************
       3101-NYKNKNRKBNLST-HEN-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-NYKNKNRKBNLST-G
      *
           MOVE    "1"                 TO  SPA-GMN-NYKNKNRKBNL (1)
           MOVE    "伝票の古い未収順に入金"
                                       TO  SPA-GMN-NYKNKNRKBNMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-NYKNKNRKBNL (2)
           MOVE    "伝票の新しい未収順に入金"
                                       TO  SPA-GMN-NYKNKNRKBNMEIL (2)
           MOVE    "3"                 TO  SPA-GMN-NYKNKNRKBNL (3)
           MOVE    "診療日の古い未収順に入金"
                                       TO  SPA-GMN-NYKNKNRKBNMEIL (3)
           MOVE    "4"                 TO  SPA-GMN-NYKNKNRKBNL (4)
           MOVE    "診療日の新しい未収順に入金"
                                       TO  SPA-GMN-NYKNKNRKBNMEIL (4)
      *
           MOVE    4                   TO  SPA-GMN-NYKNKNRKBN-MAX
      *
           .
       3101-NYKNKNRKBNLST-HEN-EXT.
           EXIT.
      *************************************************************
      *    診療年月指定コンボ編集処理
      *************************************************************
       3101-IKTSRYYMKBNLST-HEN-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-IKTSRYYMKBNLST-G
      *
           MOVE    "1"                 TO  SPA-GMN-IKTSRYYMKBNL (1)
           MOVE    "診療年月を指定しない"
                                       TO  SPA-GMN-IKTSRYYMKBNMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-IKTSRYYMKBNL (2)
           MOVE    "診療年月を指定する"
                                       TO  SPA-GMN-IKTSRYYMKBNMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-IKTSRYYMKBN-MAX
      *
           .
       3101-IKTSRYYMKBNLST-HEN-EXT.
           EXIT.
      *************************************************************
      *    入金方法コンボ編集処理
      *************************************************************
       3101-NYKNHOHOLST-HEN-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-NYKNHOHOLST-G
      *
      *    入金方法
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1041-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1041-STYUKYMD
                                           SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1041-REC
           ELSE
               INITIALIZE                  SYS-1041-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX4
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX4           >=   50 )
      *
               COMPUTE IDX4    =   IDX4    +   1
      *
               MOVE    SYS-1041-KBNCD  TO  SPA-GMN-NYKNHOHOL    (IDX4)
               MOVE    SYS-1041-NYKN-NAME
                                       TO  SPA-GMN-NYKNHOHOMEIL (IDX4)
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC        NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-1041-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX4                TO  SPA-GMN-NYKNHOHO-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-NYKNHOHOLST-HEN-EXT.
           EXIT.
      *************************************************************
      *    請求書発行区分コンボ編集処理
      *************************************************************
       3101-SKYPRTKBNLST-HEN-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SKYPRTKBNLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-SKYPRTKBNL (1)
           MOVE    "発行しない"        TO  SPA-GMN-SKYPRTKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SKYPRTKBNL (2)
           MOVE    "発行する"          TO SPA-GMN-SKYPRTKBNMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-SKYPRTKBN-MAX
      *
           .
       3101-SKYPRTKBNLST-HEN-EXT.
           EXIT.
      *************************************************************
      *    請求書集計区分コンボ編集処理
      *************************************************************
       3101-SKYSUMKBNLST-HEN-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SKYSUMKBNLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-SKYSUMKBNL (1)
           MOVE    "個別に発行"        TO  SPA-GMN-SKYSUMKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SKYSUMKBNL (2)
           MOVE    "月毎に診療科・保険組合せ別に発行"
                                       TO SPA-GMN-SKYSUMKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-SKYSUMKBNL (3)
           MOVE    "月毎に保険組合せ別に発行"
                                       TO SPA-GMN-SKYSUMKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-SKYSUMKBNL (4)
           MOVE    "月毎に診療科別に発行"
                                       TO SPA-GMN-SKYSUMKBNMEIL (4)
           MOVE    "4"                 TO  SPA-GMN-SKYSUMKBNL (5)
           MOVE    "月毎にまとめて発行"
                                       TO SPA-GMN-SKYSUMKBNMEIL (5)
      *
           MOVE    5                   TO  SPA-GMN-SKYSUMKBN-MAX
      *
           .
       3101-SKYSUMKBNLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納情報リスト画面表示編集処理
      *****************************************************************
       3101-SYULIST-OUT-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  WRK-NYUGAIKBN
                                       WRK-DENPNUM
      *
           MOVE    SPA-GMN-NYU-NYUKIN  TO  WRK-NYU-NYUKIN
           MOVE    SPA-GMN-NYU-HENKIN  TO  WRK-NYU-HENKIN
           MOVE    SPA-GMN-GAI-NYUKIN  TO  WRK-GAI-NYUKIN
           MOVE    SPA-GMN-GAI-HENKIN  TO  WRK-GAI-HENKIN
           MOVE    SPA-GMN-NYUKIN      TO  WRK-NYUKIN
      *
           MOVE    ZERO                TO  SPA-GMN-NYU-NYUKIN
           MOVE    ZERO                TO  SPA-GMN-NYU-HENKIN
           MOVE    ZERO                TO  SPA-GMN-GAI-NYUKIN
           MOVE    ZERO                TO  SPA-GMN-GAI-HENKIN
           MOVE    ZERO                TO  SPA-GMN-NYUKIN
      *
           PERFORM 31011-SYULIST-OUT-IDX-CHK-SEC
           IF    ( IDX-OUT         NOT =   ZERO )
               MOVE    SPA-NAI-TNYUGAIKBN (IDX-OUT)
                                       TO  WRK-NYUGAIKBN
               MOVE    SPA-GMN-TDENPNUM (IDX-OUT)
                                       TO  WRK-DENPNUM
           END-IF
      *
           INITIALIZE                  SPA-GMN-SELNUM
                                       SPA-GMN-SYULIST-SEL
      *
           COMPUTE IDX-TMP =   SPA-GMN-SYULIST-MAX     +   1
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-SYULIST-MAX - 1)
               COMPUTE WRK-STIDX   =   IDX-LST +   1
               PERFORM VARYING IDX-SRT FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-SRT >   SPA-GMN-SYULIST-MAX )
      *
                   MOVE    ZERO        TO  FLG-SORT
      *
                   IF    ( SPA-GMN-NYKNKNRKBN  =   "3" OR "4" )
                       IF    ( SPA-NAI-TSRT2-KEY (IDX-LST)
                                       >   SPA-NAI-TSRT2-KEY (IDX-SRT))
                           MOVE    1   TO  FLG-SORT
                       END-IF
                   ELSE
                       IF    ( SPA-NAI-TSRT-KEY (IDX-LST)
                                       >   SPA-NAI-TSRT-KEY (IDX-SRT))
                           MOVE    1   TO  FLG-SORT
                       END-IF
                   END-IF
      *
                   IF    ( FLG-SORT    =   1 )
                       MOVE    SPA-GMN-SYULIST-OCC (IDX-LST)
                                       TO  SPA-GMN-SYULIST-OCC (IDX-TMP)
                       MOVE    SPA-GMN-SYULIST-OCC (IDX-SRT)
                                       TO  SPA-GMN-SYULIST-OCC (IDX-LST)
                       MOVE    SPA-GMN-SYULIST-OCC (IDX-TMP)
                                       TO  SPA-GMN-SYULIST-OCC (IDX-SRT)
                       MOVE    SPA-NAI-SYULIST-OCC (IDX-LST)
                                       TO  SPA-NAI-SYULIST-OCC (IDX-TMP)
                       MOVE    SPA-NAI-SYULIST-OCC (IDX-SRT)
                                       TO  SPA-NAI-SYULIST-OCC (IDX-LST)
                       MOVE    SPA-NAI-SYULIST-OCC (IDX-TMP)
                                       TO  SPA-NAI-SYULIST-OCC (IDX-SRT)
                   END-IF
               END-PERFORM
           END-PERFORM
           INITIALIZE  SPA-GMN-SYULIST-OCC (IDX-TMP)
                       SPA-NAI-SYULIST-OCC (IDX-TMP)
      *
           INITIALIZE                  SPA-GMN-SYULIST-OUT-G
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-SYULIST-MAX )
      *
               MOVE    1               TO  FLG-SYULIST-HEN
      *
               IF    ( SPA-GMN-NYUGAIKBN   NOT =   "0" )
                   IF    ( SPA-GMN-NYUGAIKBN
                                   NOT =   SPA-NAI-TNYUGAIKBN (IDX-LST))
                       MOVE    ZERO    TO  FLG-SYULIST-HEN
                   END-IF
               END-IF
      *
               IF    ( FLG-SYULIST-HEN =    1 )
                   IF    ( SPA-GMN-SYULIST-OUT-MAX
                                          < CONST-SYULIST-OUT-MAX )
                       COMPUTE SPA-GMN-SYULIST-OUT-MAX
                           =   SPA-GMN-SYULIST-OUT-MAX +   1
                       MOVE    IDX-LST TO  SPA-GMN-SYULIST-OUT-IDX
                                             (SPA-GMN-SYULIST-OUT-MAX)
      *
                       IF    ( SPA-NAI-TNYUGAIKBN (IDX-LST)
                                       =   WRK-NYUGAIKBN )
                        AND  ( SPA-GMN-TDENPNUM (IDX-LST)
                                       =   WRK-DENPNUM )
                           MOVE    SPA-GMN-SYULIST-OUT-MAX
                                       TO  SPA-GMN-SELNUM
                                           SPA-GMN-SYULIST-SEL
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
               MOVE    WRK-NYU-NYUKIN  TO  SPA-GMN-NYU-NYUKIN
               MOVE    WRK-NYU-HENKIN  TO  SPA-GMN-NYU-HENKIN
               MOVE    WRK-GAI-NYUKIN  TO  SPA-GMN-GAI-NYUKIN
               MOVE    WRK-GAI-HENKIN  TO  SPA-GMN-GAI-HENKIN
           ELSE
               IF    ( SPA-GMN-SYULIST-SEL >   ZERO )
                   MOVE    WRK-NYUKIN  TO  SPA-GMN-NYUKIN
               END-IF
           END-IF
      *
           .
       3101-SYULIST-OUT-HEN-EXT.
           EXIT.
      
      *****************************************************************
      *    未収額合計編集処理
      *****************************************************************
       3101-MISYU-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-NYU-MISYU
                                           SPA-GMN-GAI-MISYU
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   CONST-SYULIST-MAX )
      *
               IF    ( SPA-NAI-TNYUGAIKBN (IDX-LST)    =   "1" )
                   COMPUTE SPA-GMN-NYU-MISYU
                       =   SPA-GMN-NYU-MISYU
                       +   SPA-GMN-TMISYU (IDX-LST)
               ELSE
                   COMPUTE SPA-GMN-GAI-MISYU
                       =   SPA-GMN-GAI-MISYU
                       +   SPA-GMN-TMISYU (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           .
       3101-MISYU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納情報リストインデックスチェック処理
      *****************************************************************
       31011-SYULIST-OUT-IDX-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  IDX-OUT
      *
           IF    ( SPA-GMN-SYULIST-SEL >=  1 )
            AND  ( SPA-GMN-SYULIST-SEL <=  CONST-SYULIST-OUT-MAX )
                MOVE   SPA-GMN-SYULIST-OUT-IDX (SPA-GMN-SYULIST-SEL)
                                       TO  IDX-OUT
           END-IF
      *
           .
       31011-SYULIST-OUT-IDX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初期表示時カーソル位置編集処理
      *****************************************************************
       3101-SYOKI-CUR-HEN-SEC          SECTION.
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               IF    ( SPA-GMN-SELNUM  >   ZERO )
                   MOVE    6           TO  SPA-GMN-CUR
               ELSE
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       3101-SYOKI-CUR-HEN-EXT.
           EXIT.
      *************************************************************
      *    画面遷移処理
      *************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO      WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR
      *
      *    入金日
           WHEN    "CLICKED"       ALSO    "B04"
               MOVE    07          TO      SPA-GMN-CUR
      *
      *    一括／個別
           WHEN    "CLICKED"       ALSO    "B08"
               IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
                   MOVE    "F"     TO      S06-RD-NYUKIN-IKT
                   MOVE    "T"     TO      S06-RD-NYUKIN-KBT
               ELSE
                   MOVE    "T"     TO      S06-RD-NYUKIN-IKT
                   MOVE    "F"     TO      S06-RD-NYUKIN-KBT
               END-IF
      *
               PERFORM 4101-RD-NYUKIN-CHK-SEC
      *
      *    一括入返金
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 4101-ALL-NYUHENKIN-SEC
      *
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
      *
      *    一括入金
      *    個別入金
           WHEN    "CLICKED"       ALSO    "RD_NYUKIN_IKT"
           WHEN    "CLICKED"       ALSO    "RD_NYUKIN_KBT"
               PERFORM 4101-RD-NYUKIN-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *************************************************************
      *    会話　処理
      *************************************************************
       200-ENTRY                       SECTION.
      *
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    入外区分
           WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 4100-NYUGAIKBN-CHK-SEC
      *
      *    診療年月指定
           WHEN    "ACTIVATE"      ALSO    "IKTSRYYMKBN"
                   PERFORM 4100-IKTSRYYMKBN-CHK-SEC
      *
      *    一括入金診療年月
           WHEN    "ACTIVATE"      ALSO    "IKTSRYYM"
                   PERFORM 4100-IKTSRYYM-CHK-SEC
      *
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4100-SELNUM-CHK-SEC
      *
      *    収納情報リスト
           WHEN    ANY             ALSO    "SYULIST"
                   PERFORM 4100-SYULIST-CHK-SEC
           WHEN    OTHER
      *
               MOVE    ZERO            TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
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
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    入金管理区分
           MOVE    S06-NYKNKNRKBN      TO  SPA-GMN-NYKNKNRKBN-G
      *    入金日
           MOVE    S06-NYUKINYMD       TO  SPA-GMN-NYUKINYMD
      *    入金方法
           MOVE    S06-NYKNHOHO        TO  SPA-GMN-NYKNHOHO-G
      *    請求書発行区分
           MOVE    S06-SKYPRTKBN       TO  SPA-GMN-SKYPRTKBN-G
      *    請求書集計区分
           MOVE    S06-SKYSUMKBN       TO  SPA-GMN-SKYSUMKBN-G
      *    外来入金額
           MOVE    S06-GAI-NYUKIN      TO  SPA-GMN-GAI-NYUKIN
      *    外来返金額
           MOVE    S06-GAI-HENKIN      TO  SPA-GMN-GAI-HENKIN
      *    入院入金額
           MOVE    S06-NYU-NYUKIN      TO  SPA-GMN-NYU-NYUKIN
      *    入院返金額
           MOVE    S06-NYU-HENKIN      TO  SPA-GMN-NYU-HENKIN
      *    個別入金額
           MOVE    S06-NYUKIN          TO  SPA-GMN-NYUKIN
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    入外区分チェック
           PERFORM 41001-NYUGAIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
      *
      *        入金管理区分チェック
               PERFORM 4101-NYKNKNRKBN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        診療年月指定区分チェック
               PERFORM 41001-IKTSRYYMKBN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        一括入金診療年月チェック
               PERFORM 41001-IKTSRYYM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        外来入金額チェック
               PERFORM 4101-GAI-NYUKIN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        外来返金額チェック
               PERFORM 4101-GAI-HENKIN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        入院入金額チェック
               PERFORM 4101-NYU-NYUKIN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        入院返金額チェック
               PERFORM 4101-NYU-HENKIN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
      *
      *        選択番号チェック
               PERFORM 4101-SELNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        入金額チェック
               PERFORM 4101-NYUKIN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    入金日チェック処理
           PERFORM 4101-NYUKINYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入金方法チェック
           PERFORM 4101-NYKNHOHO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    請求書兼領収書発行区分チェック
           PERFORM 4101-SKYPRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    請求書兼領収書集計区分チェック
           PERFORM 4101-SKYSUMKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報リストチェック
      *****************************************************************
       4100-SYULIST-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
                                           SPA-GMN-SYULIST-SEL
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-SYULIST-OUT-MAX )
      *
               IF    ( S06-SYULIST-SELECT (IDX-LST) =   "T" )
                   MOVE    IDX-LST     TO  SPA-GMN-SYULIST-SEL
                                           SPA-GMN-SELNUM
      *
                   MOVE    IDX-LST     TO  IDX-KBT
      *
                   MOVE    SPA-GMN-SYULIST-OUT-MAX
                                       TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-SYULIST-SEL  >  ZERO )
      *
               MOVE    "F"   TO  SPA-GMN-RD-NYUKIN-IKT
               MOVE    "T"   TO  SPA-GMN-RD-NYUKIN-KBT
      *
               PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
               PERFORM 3101-SYOKI-CUR-HEN-SEC
           END-IF
      *
           .
      *
       4100-SYULIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4100-NYUGAIKBN-CHK-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    S06-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
      *
           PERFORM 41001-NYUGAIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-NYUGAIKBN-CHK-EXT
           END-IF
      *
           PERFORM 3101-SYULIST-OUT-HEN-SEC
      *
           PERFORM 3101-SYOKI-CUR-HEN-SEC
      *
           .
      *
       4100-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月指定区分チェック
      *****************************************************************
       4100-IKTSRYYMKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    S06-IKTSRYYMKBN     TO  SPA-GMN-IKTSRYYMKBN-G
      *
           PERFORM 41001-IKTSRYYMKBN-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 3101-NYUKIN-MAX-HEN-SEC
           END-IF
      *
           .
      *
       4100-IKTSRYYMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月指定区分チェック処理
      *****************************************************************
       41001-IKTSRYYMKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-IKTSRYYMKBN TO  WRK-CMB-CD
           PERFORM 4200-IKTSRYYMKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    SPA-GMN-IKTSRYYMKBNLST (1)
                                       TO  SPA-GMN-IKTSRYYMKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-IKTSRYYMKBN-G
           END-IF
      *
           .
       41001-IKTSRYYMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金診療年月チェック
      *****************************************************************
       4100-IKTSRYYM-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    S06-IKTSRYYM        TO  SPA-GMN-IKTSRYYM
           PERFORM 41001-IKTSRYYM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 3101-NYUKIN-MAX-HEN-SEC
           ELSE
               MOVE   SPA-NAI-IKTSRYYM TO  WRK-YMD
               PERFORM 800-SEIWA-YM-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-IKTSRYYM
           END-IF
      *
           .
      *
       4100-IKTSRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金診療年月チェック
      *****************************************************************
       41001-IKTSRYYM-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-IKTSRYYM    TO  WRK-YMD
           PERFORM 800-SEIWA-YM-HEN-SEC
           IF   ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE   "0016"           TO  SPA-ERRCD
               MOVE    12              TO  SPA-GMN-CUR
           ELSE
               MOVE   WRK-HENYMDG      TO  SPA-GMN-IKTSRYYM
               MOVE   WRK-SYMD         TO  SPA-NAI-IKTSRYYM
           END-IF
      *
           .
      *
       41001-IKTSRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-SYULIST-SEL
                                           SPA-GMN-NYUKIN
                                           SPA-GMN-NYUKIN-MAX
      *
           MOVE    S06-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
               MOVE    ZERO            TO  SPA-GMN-SELNUM
           END-IF
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           PERFORM 41001-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-GMN-SYULIST-SEL
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX-KBT
      *
           PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       41001-NYUGAIKBN-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  41001-NYUGAIKBN-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUGAIKBN-G
           END-IF
      *
           .
       41001-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNUM      >=  ZERO )
            AND  ( SPA-GMN-SELNUM      <=  SPA-GMN-SYULIST-OUT-MAX )
               CONTINUE
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  41001-SELNUM-CHK-EXT
           END-IF
      *
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括／個別入金ラジオボタンチェック処理
      *****************************************************************
       4101-RD-NYUKIN-CHK-SEC          SECTION.
      *
           MOVE    S06-RD-NYUKIN-IKT   TO  SPA-GMN-RD-NYUKIN-IKT
           MOVE    S06-RD-NYUKIN-KBT   TO  SPA-GMN-RD-NYUKIN-KBT
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
               MOVE    ZERO            TO  SPA-GMN-SELNUM
                                           SPA-GMN-SYULIST-SEL
           END-IF
      *
           PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
           PERFORM 3101-SYOKI-CUR-HEN-SEC
      *
           .
       4101-RD-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入返金処理
      *****************************************************************
       4101-ALL-NYUHENKIN-SEC          SECTION.
      *
           MOVE    1               TO  FLG-NYUHENKIN
           PERFORM 31011-NYUKIN-MAX-IKT-HEN-SEC
      *
           PERFORM 3101-MISYU-HEN-SEC
      *
           PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
           .
       4101-ALL-NYUHENKIN-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )
               MOVE    "0013"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           PERFORM 41001-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金管理区分チェック処理
      *****************************************************************
       4101-NYKNKNRKBN-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-NYKNKNRKBN  TO  WRK-CMB-CD
           PERFORM 4200-NYKNKNRKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4101-NYKNKNRKBN-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYKNKNRKBN-G
           END-IF
      *
           IF   (( SPA-GMN-NYKNKNRKBN          =   "1" OR "2" )
             AND ( SPA-NAI-NYKNKNRKBN-BF       =   "1" OR "2" ))
            OR  (( SPA-GMN-NYKNKNRKBN          =   "3" OR "4" )
             AND ( SPA-NAI-NYKNKNRKBN-BF       =   "3" OR "4" ))
               CONTINUE
           ELSE
      *
               PERFORM 3101-SYULIST-OUT-HEN-SEC
               PERFORM 3101-SYOKI-CUR-HEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-NYKNKNRKBN  TO  SPA-NAI-NYKNKNRKBN-BF
      *
           .
       4101-NYKNKNRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来入金額チェック処理
      *****************************************************************
       4101-GAI-NYUKIN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-GAI-NYUKIN  <   ZERO )
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  4101-GAI-NYUKIN-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-GAI-NYUKIN  >   SPA-GMN-GAI-NYUKIN-MAX )
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  4101-GAI-NYUKIN-CHK-EXT
           END-IF
      *
           IF    ( WRK-MCP-WIDGET      =   "GAI_NYUKIN" )
               PERFORM 41011-IKT-KARINYUKIN-SEC
           END-IF
      *
           .
       4101-GAI-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来返金額チェック処理
      *****************************************************************
       4101-GAI-HENKIN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-GAI-HENKIN  <   ZERO )
               MOVE    "0017"          TO  SPA-ERRCD
               MOVE    13              TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-GAI-HENKIN  >   SPA-GMN-GAI-HENKIN-MAX )
                   MOVE    "0018"      TO  SPA-ERRCD
                   MOVE    13          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "GAI_HENKIN" )
                   PERFORM 41011-IKT-KARINYUKIN-SEC
               END-IF
           END-IF
      *
           .
       4101-GAI-HENKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院入金額チェック処理
      *****************************************************************
       4101-NYU-NYUKIN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-NYU-NYUKIN  <   ZERO )
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
               GO  TO  4101-NYU-NYUKIN-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-NYU-NYUKIN  >   SPA-GMN-NYU-NYUKIN-MAX )
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
               GO  TO  4101-NYU-NYUKIN-CHK-EXT
           END-IF
      *
           IF    ( WRK-MCP-WIDGET      =   "NYU_NYUKIN" )
               PERFORM 41011-IKT-KARINYUKIN-SEC
           END-IF
      *
           .
       4101-NYU-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来返金額チェック処理
      *****************************************************************
       4101-NYU-HENKIN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-NYU-HENKIN  <   ZERO )
               MOVE    "0019"          TO  SPA-ERRCD
               MOVE    14              TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYU-HENKIN  >   SPA-GMN-NYU-HENKIN-MAX )
                   MOVE    "0020"      TO  SPA-ERRCD
                   MOVE    14          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "NYU_HENKIN" )
                   PERFORM 41011-IKT-KARINYUKIN-SEC
               END-IF
           END-IF
      *
           .
       4101-NYU-HENKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理
      *****************************************************************
       4101-NYUKIN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-NYUKIN  <   ZERO )
               IF    ( SPA-NAI-HENKINFLG   =   ZERO )
                   MOVE    "0009"  TO  SPA-ERRCD
               ELSE
                   MOVE    "0021"  TO  SPA-ERRCD
               END-IF
               MOVE    6           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-NYUKIN  >   SPA-GMN-NYUKIN-MAX )
                   IF    ( SPA-NAI-HENKINFLG   =   ZERO )
                       MOVE    "0010"  TO  SPA-ERRCD
                   ELSE
                       MOVE    "0022"  TO  SPA-ERRCD
                   END-IF
                   MOVE    6       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-MCP-WIDGET      =   "NYUKIN" )
                   PERFORM 41011-KBT-KARINYUKIN-SEC
               END-IF
           END-IF
      *
           .
       4101-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括仮入金処理
      *****************************************************************
       41011-IKT-KARINYUKIN-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           EVALUATE    SPA-GMN-NYKNKNRKBN
           WHEN    "1"
           WHEN    "3"
               PERFORM VARYING IDX-LST
                       FROM    1   BY  1
                       UNTIL ( IDX-LST >   CONST-SYULIST-MAX )
                        OR   ( FLG-LOOP-END    =   1 )
      *
                   PERFORM 41011-IKT-KARINYUKIN-HEN-SEC
      *
               END-PERFORM
           WHEN    "2"
           WHEN    "4"
               PERFORM VARYING IDX-LST
                       FROM    CONST-SYULIST-MAX   BY  -1
                       UNTIL ( IDX-LST <   1 )
                        OR   ( FLG-LOOP-END    =   1 )
      *
                   PERFORM 41011-IKT-KARINYUKIN-HEN-SEC
      *
               END-PERFORM
           END-EVALUATE
      *
      *    未収額合計編集処理
           PERFORM 3101-MISYU-HEN-SEC
      *
      *    入金上限額編集処理
           PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
           .
       41011-IKT-KARINYUKIN-EXT.
           EXIT.
      *****************************************************************
      *    一括仮入金編集処理
      *****************************************************************
       41011-IKT-KARINYUKIN-HEN-SEC    SECTION.
      *
           IF    ( SPA-NAI-TNYUGAIKBN (IDX-LST)
                                           =   "1" )
               MOVE    1           TO  IDX3
           ELSE
               MOVE    2           TO  IDX3
           END-IF
      *
           MOVE    1               TO  FLG-TARGET
      *
           IF    ( SPA-GMN-TMISYU (IDX-LST) NOT =   ZERO )
               CONTINUE
           ELSE
                   MOVE    ZERO    TO  FLG-TARGET
           END-IF
      *
           IF    ( SPA-GMN-IKTSRYYMKBN     =   "2" )
               IF    ( SPA-NAI-IKTSRYYM    =   SPA-NAI-TSRYYMD
                                                  (IDX-LST)(1:6))
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
           END-IF
      *
           IF    ( FLG-TARGET      =   1 )
      *
               IF    ( SPA-GMN-TMISYU (IDX-LST)    >=  ZERO )
                   IF    ( SPA-GMN-TMISYU (IDX-LST)
                                       >   SPA-GMN-IKT-NYUKIN (IDX3) )
                       COMPUTE SPA-GMN-TNYUKIN (IDX-LST)
                           =   SPA-GMN-TNYUKIN (IDX-LST)
                           +   SPA-GMN-IKT-NYUKIN (IDX3)
                       COMPUTE SPA-GMN-TMISYU (IDX-LST)
                           =   SPA-GMN-TMISYU (IDX-LST)
                           -   SPA-GMN-IKT-NYUKIN (IDX3)
                       MOVE    ZERO    TO  SPA-GMN-IKT-NYUKIN (IDX3)
                   ELSE
                       COMPUTE SPA-GMN-TNYUKIN (IDX-LST)
                           =   SPA-GMN-TNYUKIN (IDX-LST)
                           +   SPA-GMN-TMISYU (IDX-LST)
                       COMPUTE SPA-GMN-IKT-NYUKIN (IDX3)
                           =   SPA-GMN-IKT-NYUKIN (IDX3)
                           -   SPA-GMN-TMISYU (IDX-LST)
                       MOVE    ZERO    TO  SPA-GMN-TMISYU (IDX-LST)
                   END-IF
               ELSE
                   COMPUTE WRK-MISYU
                       =   SPA-GMN-TMISYU (IDX-LST)
                       +   SPA-GMN-IKT-HENKIN (IDX3)
                   IF    ( WRK-MISYU       >=  ZERO )
                       COMPUTE SPA-GMN-TNYUKIN (IDX-LST)
                           =   SPA-GMN-TNYUKIN (IDX-LST)
                           +   SPA-GMN-TMISYU (IDX-LST)
                       COMPUTE SPA-GMN-TMISYU (IDX-LST)
                           =   0
                       COMPUTE SPA-GMN-IKT-HENKIN (IDX3)
                           =   WRK-MISYU
                   ELSE
                       COMPUTE SPA-GMN-TNYUKIN (IDX-LST)
                           =   SPA-GMN-TNYUKIN (IDX-LST)
                           -   SPA-GMN-IKT-HENKIN (IDX3)
                       COMPUTE SPA-GMN-TMISYU (IDX-LST)
                           =   SPA-GMN-TMISYU (IDX-LST)
                           +   SPA-GMN-IKT-HENKIN (IDX3)
                       COMPUTE SPA-GMN-IKT-HENKIN (IDX3)
                           =   0
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-NYU-NYUKIN  <=  ZERO )
                AND  ( SPA-GMN-NYU-HENKIN  <=  ZERO )
                AND  ( SPA-GMN-GAI-NYUKIN  <=  ZERO )
                AND  ( SPA-GMN-GAI-HENKIN  <=  ZERO )
                   MOVE    1       TO  FLG-LOOP-END
               END-IF
           END-IF
      *
           .
       41011-IKT-KARINYUKIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    個別仮入金処理
      *****************************************************************
       41011-KBT-KARINYUKIN-SEC        SECTION.
      *
           PERFORM 31011-SYULIST-OUT-IDX-CHK-SEC
           IF    ( IDX-OUT         NOT =   ZERO )
               IF    ( SPA-NAI-HENKINFLG   =   ZERO )
                   COMPUTE SPA-GMN-TMISYU (IDX-OUT)
                       =   SPA-GMN-TMISYU (IDX-OUT)
                       +   SPA-GMN-TNYUKIN (IDX-OUT)
                       -   SPA-GMN-NYUKIN
                   MOVE    SPA-GMN-NYUKIN  TO  SPA-GMN-TNYUKIN (IDX-OUT)
               ELSE
                   COMPUTE SPA-GMN-TMISYU (IDX-OUT)
                       =   SPA-GMN-TMISYU (IDX-OUT)
                       +   SPA-GMN-TNYUKIN (IDX-OUT)
                       +   SPA-GMN-NYUKIN
                   COMPUTE SPA-GMN-TNYUKIN (IDX-OUT)
                       =   SPA-GMN-NYUKIN  *  -1
               END-IF
           END-IF
      *
      *    未収額合計編集処理
           PERFORM 3101-MISYU-HEN-SEC
      *
      *    入金上限額編集処理
           PERFORM 3101-NYUKIN-MAX-HEN-SEC
      *
           .
       41011-KBT-KARINYUKIN-EXT.
           EXIT.
      *****************************************************************
      *    入金日チェック処理
      *****************************************************************
       4101-NYUKINYMD-CHK-SEC          SECTION.
      *
            MOVE   SPACE               TO  SPA-NAI-NYUKINYMD
      *
            IF   ( SPA-GMN-NYUKINYMD   =   SPACE )
             AND ( WRK-MCP-WIDGET      =   "NYUKINYMD" )
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-NYUKINYMD
               MOVE    SPA-SYSYMD      TO  SPA-NAI-NYUKINYMD
               GO  TO  4101-NYUKINYMD-CHK-EXT
            END-IF
      *
            MOVE   SPA-GMN-NYUKINYMD   TO  WRK-YMD
            PERFORM    800-SEIWA-HEN-SEC
            IF   ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE    7               TO  SPA-GMN-CUR
               MOVE    "0012"          TO  SPA-ERRCD
               GO  TO  4101-NYUKINYMD-CHK-EXT
            END-IF
      *
            MOVE   WRK-HENYMDG         TO  SPA-GMN-NYUKINYMD
            MOVE   WRK-SYMD            TO  SPA-NAI-NYUKINYMD
      *
           .
       4101-NYUKINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金方法チェック
      *****************************************************************
       4101-NYKNHOHO-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-NYKNHOHO    TO  WRK-CMB-CD
           PERFORM 4200-NYKNHOHOLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-CUR
               GO  TO  4101-NYKNHOHO-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYKNHOHO-G
           END-IF
      *
           .
       4101-NYKNHOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行区分チェック処理
      *****************************************************************
       4101-SKYPRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SKYPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-SKYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
               GO  TO  4101-SKYPRTKBN-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKYPRTKBN-G
           END-IF
      *
           .
       4101-SKYPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書集計区分チェック処理
      *****************************************************************
       4101-SKYSUMKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SKYSUMKBN   TO  WRK-CMB-CD
           PERFORM 4200-SKYSUMKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "0015"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
               GO  TO  4101-SKYSUMKBN-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKYSUMKBN-G
           END-IF
      *
           IF    ( SPA-GMN-SKYSUMKBN   NOT =   SPA-NAI-SKYSUMKBN-BF )
               MOVE    "1"             TO  WRK-CMB-CD
               PERFORM 4200-SKYPRTKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SKYPRTKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SKYSUMKBN   TO  SPA-NAI-SKYSUMKBN-BF
      *
           .
       4101-SKYSUMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-NYUGAIKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-NYUGAIKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-NYUGAIKBN-MAX )
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金管理区分コンボチェック処理
      *****************************************************************
       4200-NYKNKNRKBNLST-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-NYKNKNRKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-NYKNKNRKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-NYKNKNRKBN-MAX )
               MOVE    SPA-GMN-NYKNKNRKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYKNKNRKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月指定コンボチェック処理
      *****************************************************************
       4200-IKTSRYYMKBNLST-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-IKTSRYYMKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-IKTSRYYMKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-IKTSRYYMKBN-MAX )
               MOVE    SPA-GMN-IKTSRYYMKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-IKTSRYYMKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金方法コンボチェック処理
      *****************************************************************
       4200-NYKNHOHOLST-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-NYKNHOHO-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-NYKNHOHOL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-NYKNHOHO-MAX )
               MOVE    SPA-GMN-NYKNHOHOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYKNHOHOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書区分コンボチェック処理
      *****************************************************************
       4200-SKYPRTKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-SKYPRTKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-SKYPRTKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-SKYPRTKBN-MAX )
               MOVE    SPA-GMN-SKYPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKYPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書集計区分コンボチェック処理
      *****************************************************************
       4200-SKYSUMKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-SKYSUMKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-SKYSUMKBNL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-SKYSUMKBN-MAX )
               MOVE    SPA-GMN-SKYSUMKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKYSUMKBNLST-CHK-EXT.
           EXIT.
      *************************************************************
      *    戻る　処理
      *************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "S02"               TO  SPA-SAKIPG
           MOVE    "S06"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
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
      *************************************************************
      *    クリア　処理
      *************************************************************
       220-CLEAR                       SECTION.
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4801-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "2001"              TO  SPA-SIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック
      *****************************************************************
       4801-KANREN-CHK-SEC             SECTION.
      *
      *    入金日チェック
           PERFORM 4101-NYUKINYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入金方法チェック
           PERFORM 4101-NYKNHOHO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    請求書兼領収書発行区分チェック
           PERFORM 4101-SKYPRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    請求書兼領収書集計区分チェック
           PERFORM 4101-SKYSUMKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
           .
       4801-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-GAI-SKYPRT
                                           FLG-NYU-SKYPRT
           INITIALIZE                      WRK-DENPITEM-G
                                           KEY-AREA
      *
      *    収納機能情報取得
           PERFORM 3101-SYS-1039-GET-SEC
      *
      *    まとめ入金サブ呼出処理
           PERFORM 4801-ORCSNYUKINS-CALL-SEC
      *
      *    請求書出力処理
           PERFORM 4801-SEIKYUSYO-OUT-SEC
      *
           MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
      *
           PERFORM 310-SYOKI-HYOJI-SEC
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *************************************************************
      *    まとめ入金サブ呼出処理
      *************************************************************
       4801-ORCSNYUKINS-CALL-SEC   SECTION.
      *
           INITIALIZE                      ORCSNYUKINAREAS
           MOVE    SPA-HOSPNUM         TO  LNK-SCNYUKINS-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  LNK-SCNYUKINS-PTID
           MOVE    SPA-NAI-NYUKINYMD   TO  LNK-SCNYUKINS-NYUKIN-YMD
           MOVE    SPA-GMN-NYKNHOHO    TO  LNK-SCNYUKINS-NYUKIN-HOHO
      *
           MOVE    HIGH-VALUE          TO  KEY-GAI-MISYU (1)
           MOVE    LOW-VALUE           TO  KEY-GAI-SKYEDYMD
           MOVE    ZERO                TO  LNK-SCNYUKINS-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SYULIST-MAX )
      *
               IF    ( SPA-GMN-TNYUKIN (IDX0)  NOT =   ZERO )
                   COMPUTE LNK-SCNYUKINS-MAX
                       =   LNK-SCNYUKINS-MAX   +   1
      *
      *            入外区分
                   MOVE    SPA-NAI-TNYUGAIKBN (IDX0)
                                       TO  LNK-SCNYUKINS-NYUGAIKBN
                                                 (LNK-SCNYUKINS-MAX)
      *            伝票番号
                   MOVE    SPA-GMN-TDENPNUM (IDX0)
                                       TO  LNK-SCNYUKINS-DENPNUM
                                                 (LNK-SCNYUKINS-MAX)
      *            入金額
                   MOVE    SPA-GMN-TNYUKIN  (IDX0)
                                       TO  LNK-SCNYUKINS-NYUKIN-MONEY
                                                 (LNK-SCNYUKINS-MAX)
      *
                   IF    ( SPA-NAI-TNYUGAIKBN (IDX0)   =   "1" )
                       MOVE    1       TO  FLG-NYU-SKYPRT
                       COMPUTE WRK-NYU-NYUKIN
                           =   WRK-NYU-NYUKIN
                           +   SPA-GMN-TNYUKIN  (IDX0)
                       COMPUTE WRK-NYU-KONSEIKYU
                           =   WRK-NYU-KONSEIKYU
                           +   SPA-GMN-TMISYU   (IDX0)
                       IF    ( SPA-GMN-TDENPNUM  (IDX0)
                                       >   WRK-NYU-DENPNUM )
                           MOVE    SPA-GMN-TDENPNUM  (IDX0)
                                       TO  WRK-NYU-DENPNUM
                           MOVE    SPA-NAI-TSRYYMD   (IDX0)
                                       TO  WRK-NYU-SRYYMD
                       END-IF
                   ELSE
                       MOVE    1       TO  FLG-GAI-SKYPRT
                       COMPUTE WRK-GAI-NYUKIN
                           =   WRK-GAI-NYUKIN
                           +   SPA-GMN-TNYUKIN  (IDX0)
                       COMPUTE WRK-GAI-KONSEIKYU
                           =   WRK-GAI-KONSEIKYU
                           +   SPA-GMN-TMISYU   (IDX0)
                       IF    ( SPA-GMN-TDENPNUM  (IDX0)
                                       >   WRK-GAI-DENPNUM )
                           MOVE    SPA-GMN-TDENPNUM  (IDX0)
                                       TO  WRK-GAI-DENPNUM
                           MOVE    SPA-NAI-TSRYYMD   (IDX0)
                                       TO  WRK-GAI-SRYYMD
                       END-IF
      *
                       MOVE    SPA-GMN-TSRYYMD  (IDX0)
                                       TO  KEY-SRYYMD (2)
                       MOVE    SPA-GMN-TDENPNUM  (IDX0)
                                       TO  KEY-DENPNUM (2)
      *
                       IF    ( KEY-GAI-MISYU (1)
                                       >   KEY-GAI-MISYU (2))
                           MOVE    KEY-GAI-MISYU (2)
                                       TO  KEY-GAI-MISYU (1)
                       END-IF
                       IF    ( KEY-GAI-SKYEDYMD
                                       <   SPA-NAI-TSRYYMD  (IDX0))
                           MOVE    SPA-NAI-TSRYYMD  (IDX0)
                                       TO  KEY-GAI-SKYEDYMD
                       END-IF
      *
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           CALL    "ORCSNYUKINS"   USING   ORCSNYUKINAREAS
                                           SPA-AREA
           IF    ( LNK-SCNYUKINS-RC    NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
               GO  TO  4801-ORCSNYUKINS-CALL-EXT
           END-IF
      *
           .
       4801-ORCSNYUKINS-CALL-EXT.
           EXIT.
      *************************************************************
      *    請求書出力処理
      *************************************************************
       4801-SEIKYUSYO-OUT-SEC      SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF   (( FLG-GAI-SKYPRT      =   1 )
              AND( SPA-GMN-SKYPRTKBN   =   "1" ))
            OR  (( FLG-NYU-SKYPRT      =   1 )
              AND( SPA-GMN-SKYPRTKBN   =   "1" ))
      *
               IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                OR   ( SPA-PRTCONF         =   "1" )
                   PERFORM 800-CLIENT-PRT-SEC
               END-IF
      *
           END-IF
      *
           IF    ( FLG-GAI-SKYPRT      =   1 )
            AND  ( SPA-GMN-SKYPRTKBN   =   "1" )
               PERFORM 4801-GAI-SKYPRT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( FLG-NYU-SKYPRT      =   1 )
                AND  ( SPA-GMN-SKYPRTKBN   =   "1" )
                   PERFORM 4801-NYU-SKYPRT-SEC
               END-IF
           END-IF
      *
           PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           .
       4801-SEIKYUSYO-OUT-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理 （入院）
      *****************************************************************
       4801-NYU-PRINT-PG-SEC           SECTION.
       
      *    請求書
           MOVE    SPACE               TO  WRK-NYU-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-NYU-SEIKYU-PG
           ELSE
              MOVE    "ORCHCN03"       TO  WRK-NYU-SEIKYU-PG
           END-IF
      *
           .
      *
       4801-NYU-PRINT-PG-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       4801-PRINT-PG-SEC               SECTION.
      *
      *    請求書
           MOVE    SPACE               TO  WRK-GAI-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1032-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-GAI-SEIKYU-PG
           ELSE
              MOVE    "ORCHC03"        TO  WRK-GAI-SEIKYU-PG
           END-IF
      *
           .
       4801-PRINT-PG-EXT.
           EXIT.
      *************************************************************
      *    請求書出力処理
      *************************************************************
       4801-GAI-SKYPRT-SEC             SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD        TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS        TO  WRK-FILEMEI-HMS
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-S01-KANANAME    TO  WRK-SPA-KANANAME
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
      *
      *    帳票プログラム取得（外来）
           PERFORM 4801-PRINT-PG-SEC
      *
           MOVE    KEY-DENPNUM(1)      TO  WRK-SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           MOVE    ZERO                TO  SYU-GRP-DENPNUM
           MOVE    KEY-GAI-SKYEDYMD    TO  SYU-SKYEDYMD
           INITIALIZE                  SMISYU-AREA
           CALL    "ORCSMISYU"         USING
                                       SYUNOU-REC
                                       SMISYU-AREA
                                       WRK-SPA-AREA
           MOVE    SMISYU-GAIRAI-P     TO  WRK-GAI-MISYU-P
           MOVE    SMISYU-GAIRAI-M     TO  WRK-GAI-MISYU-M
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
                                       TSRT-AREA
      *
      *
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
           MOVE    SMISYU-NYUIN        TO  SYUPARA01-NYU-MISYU
           MOVE    SMISYU-GAIRAI       TO  SYUPARA01-GAI-MISYU
           IF    ( SYS-1039-MISYUKIKANKBN  =   "1" )
               CONTINUE
           ELSE
               COMPUTE SYUPARA01-GAI-MISYU
                   =   SYUPARA01-GAI-MISYU   -   WRK-GAI-KONSEIKYU
               COMPUTE SYUPARA01-NYU-MISYU
                   =   SYUPARA01-NYU-MISYU   -   WRK-NYU-KONSEIKYU
               IF    ( WRK-GAI-KONSEIKYU     <   WRK-GAI-NYUKIN )
                   COMPUTE SYUPARA01-GAI-MISYU-M
                       =   SYUPARA01-GAI-MISYU-M
                       - ( WRK-GAI-KONSEIKYU -   WRK-GAI-NYUKIN )
               ELSE
                   COMPUTE SYUPARA01-GAI-MISYU-P
                       =   SYUPARA01-GAI-MISYU-P
                       - ( WRK-GAI-KONSEIKYU -   WRK-GAI-NYUKIN )
               END-IF
           END-IF
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
           MOVE    WRK-NYU-NYUKIN      TO  SYUPARA01-NYU-NYUKIN
           MOVE    WRK-GAI-NYUKIN      TO  SYUPARA01-GAI-NYUKIN
           MOVE    SPA-GMN-SKYSUMKBN   TO  SYUPARA01-SKYSUMFLG
           MOVE    3                   TO  SYUPARA01-HAKKOFLG
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
      D    DISPLAY "WRK-GAI-NYUKIN       [" WRK-GAI-NYUKIN "]"
      D    DISPLAY "WRK-GAI-KONSEIKYU    [" WRK-GAI-KONSEIKYU "]"
      D    DISPLAY "SYUPARA01-NYU-MISYU  [" SYUPARA01-NYU-MISYU "]"
      D    DISPLAY "SYUPARA01-GAI-MISYU  [" SYUPARA01-GAI-MISYU "]"
      D    DISPLAY "SYUPARA01-GAI-MISYU-P[" SYUPARA01-GAI-MISYU-P "]"
      D    DISPLAY "SYUPARA01-GAI-MISYU-M[" SYUPARA01-GAI-MISYU-M "]"
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "001%"          TO  WRK-FILEMEI-CD
               PERFORM 4801-DELETE-PARA-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  PARA-REC
               MOVE    SPA-HOSPNUM     TO  PARA-HOSPNUM
               MOVE    "S02"           TO  PARA-GYOUMUID
               MOVE    SPA-TERMID      TO  PARA-TERMID
               MOVE    "00101"         TO  WRK-FILEMEI-CD
               MOVE    WRK-FILEMEI     TO  PARA-FILEMEI
               MOVE    1               TO  PARA-EDANUM
               MOVE    SYUPARA01-AREA  TO  PARA-DATA-REC
               MOVE    PARA-REC        TO  MCPDATA-REC
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4003"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0            >   SPA-GMN-SYULIST-MAX )
               IF    ( SPA-NAI-TNYUGAIKBN (IDX0)   =   "2"  )
                AND  ( SPA-GMN-TNYUKIN (IDX0)  NOT =   ZERO )
                   COMPUTE TSRT-MAX    =   TSRT-MAX    +   1
                   MOVE    SPA-NAI-TSRYYMD (IDX0)
                                       TO  TSRT-SRYYMD(TSRT-MAX)
                   MOVE    SPA-NAI-TSRYKA (IDX0)
                                       TO  TSRT-SRYKA(TSRT-MAX)
                   MOVE    SPA-NAI-THKNCOMBI (IDX0)
                                       TO  TSRT-HKNCOMBI(TSRT-MAX)
                   MOVE    SPA-GMN-TDENPNUM (IDX0)
                                       TO  TSRT-DENPNUM(TSRT-MAX)
                   MOVE    SPA-GMN-TNYUKIN (IDX0)
                                       TO  TSRT-NYUKIN (TSRT-MAX)
               END-IF
           END-PERFORM
      *
           COMPUTE IDX-TMP =   SPA-GMN-SYULIST-MAX     +   1
           PERFORM VARYING IDXA FROM    1   BY  1
                   UNTIL ( IDXA     >   TSRT-MAX - 1)
               COMPUTE WRK-STIDX   =   IDXA +   1
               PERFORM VARYING IDX-SRT FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-SRT >   TSRT-MAX )
      *
                   IF    ( TSRT-KEY (IDXA)
                                       >   TSRT-KEY (IDX-SRT))
                       MOVE    TSRT-OCC (IDXA)
                                       TO  TSRT-OCC (IDX-TMP)
                       MOVE    TSRT-OCC (IDX-SRT)
                                       TO  TSRT-OCC (IDXA)
                       MOVE    TSRT-OCC (IDX-TMP)
                                       TO  TSRT-OCC (IDX-SRT)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0            >   TSRT-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE PARA-EDANUM =   PARA-EDANUM +   1
      *
                   INITIALIZE              SYUPARA02-AREA
                   MOVE    TSRT-DENPNUM (IDX0)
                                       TO  SYUPARA02-DENPNUM
                   MOVE    TSRT-NYUKIN (IDX0)
                                       TO  SYUPARA02-NYUKIN
                   MOVE    SYUPARA02-AREA
                                       TO  PARA-DATA-REC
                   MOVE    PARA-REC    TO  MCPDATA-REC
                   MOVE    "tbl_para"      TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4004"      TO  SPA-ERRCD
                   END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =       SPACE )
               INITIALIZE                      ORCHC03S02AREA
               MOVE    SPA-HOSPNUM     TO      ORCHC03S02-HOSPNUM
               MOVE    "S02"           TO      ORCHC03S02-GYOUMUID
               MOVE    SPA-TERMID      TO      ORCHC03S02-TERMID
               MOVE    "00101"         TO      WRK-FILEMEI-CD
               MOVE    WRK-FILEMEI     TO      ORCHC03S02-FILEMEI
               CALL    "ORCHC03S02"    USING   WRK-SPA-AREA
                                               ORCHC03S02AREA
               IF    ( ORCHC03S02-RC   NOT =   ZERO )
                   MOVE    "4005"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =       SPACE )
      *
               MOVE    WRK-GAI-SRYYMD      TO  WRK-SPA-SRYYMD
               INITIALIZE                      ORCHC03AREA
               MOVE    WRK-GAI-DENPNUM     TO  ORCHC03-DENPNUM
               MOVE    3                   TO  ORCHC03-HAKKOFLG
               MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
               MOVE    1                   TO  ORCHC03-PARAFLG
      *
               MOVE    "S02"               TO  ORCHC03-PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  ORCHC03-PARA-TERMID
               MOVE    "00102"             TO  WRK-FILEMEI-CD
               MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
               MOVE    4                   TO  ORCHC03-MOTOID
               MOVE    "01"                TO  WRK-SPA-PRT-FLG
               CALL    WRK-GAI-SEIKYU-PG       USING
                                               WRK-SPA-AREA
                                               ORCHC03AREA
               MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "001%"          TO  WRK-FILEMEI-CD
               PERFORM 4801-DELETE-PARA-SEC
           END-IF
      *
           .
       4801-GAI-SKYPRT-EXT.
           EXIT.
      *************************************************************
      *    請求書出力処理
      *************************************************************
       4801-NYU-SKYPRT-SEC             SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-S01-KANANAME    TO  WRK-SPA-KANANAME
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
      *
           MOVE    "1"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    WRK-NYU-SRYYMD      TO  WRK-SPA-SRYYMD
      *
      *    帳票プログラム取得（入院）
           PERFORM 4801-NYU-PRINT-PG-SEC
      *
           INITIALIZE                      ORCHCN03AREA
           MOVE    WRK-SPA-HOSPNUM      TO  ORCHCN03-HEAD-HOSPNUM
           MOVE    WRK-SPA-STAFFCD     TO  ORCHCN03-STAFFCD
           MOVE    WRK-SPA-SYSYMD      TO  ORCHCN03-SYSYMD
           MOVE    WRK-SPA-TERMID      TO  ORCHCN03-TERMID
           MOVE    4                   TO  ORCHCN03-MOTOID
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SYULIST-MAX )
      *
               IF    ( SPA-NAI-TNYUGAIKBN (IDX0)   =   "1" )
                 AND ( SPA-GMN-TNYUKIN  (IDX0) NOT =   ZERO )
      *
                   COMPUTE ORCHCN03-CNT    =   ORCHCN03-CNT    +   1
      *
                   MOVE    SPA-GMN-TDENPNUM (IDX0)
                                       TO  WRK-SYU-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
      *            労災判定
                   IF    ( SPA-NAI-THKNNUM (IDX0)
                                           =   "971" OR "973" )
                       MOVE    1           TO  WRK-SPA-HKNKBN
                   ELSE
                       MOVE    ZERO        TO  WRK-SPA-HKNKBN
                   END-IF
      *
                   MOVE   SPA-NAI-TRSI-HKNKBN (IDX0)
                                           TO  WRK-SPA-RSI-HKNKBN
      *
      *
                   MOVE    WRK-SPA-HOSPNUM  TO  ORCHCN03-HOSPNUM
                                                         (ORCHCN03-CNT)
                   MOVE    WRK-SPA-PTID    TO  ORCHCN03-PTID
                                                         (ORCHCN03-CNT)
                   MOVE    WRK-SPA-PTNUM   TO  ORCHCN03-PTNUM
                                                         (ORCHCN03-CNT)
                   MOVE    "1"             TO  ORCHCN03-NYUGAIKBN
                                                         (ORCHCN03-CNT)
      *
                   MOVE    SYU-SKYSTYMD    TO  ORCHCN03-KJNYMD
                                                         (ORCHCN03-CNT)
                   MOVE    SYU-DENPNUM     TO  ORCHCN03-DENPNUM
                                                         (ORCHCN03-CNT)
                   IF    ( SPA-NAI-TNYUKIN-TOTAL (IDX0) 
                                           =   ZERO )
                       MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  ORCHCN03-SEIKYU-TAX-SAI
                                                         (ORCHCN03-CNT)
                   END-IF
                   COMPUTE ORCHCN03-SEIKYU (ORCHCN03-CNT)
                       =   SYU-SKYMONEY
                       -   SPA-NAI-TNYUKIN-TOTAL (IDX0)
                   MOVE    SPA-GMN-TNYUKIN (IDX0)
                                           TO  ORCHCN03-NYUKIN
                                                         (ORCHCN03-CNT)
                   MOVE    4               TO  ORCHCN03-HAKKOFLG
                                                         (ORCHCN03-CNT)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "02"                    TO  WRK-SPA-PRT-FLG
           CALL    WRK-NYU-SEIKYU-PG   USING   ORCHCN03AREA
                                               WRK-SPA-AREA
           MOVE    SPACE                   TO  WRK-SPA-PRT-FLG
      *
           .
       4801-NYU-SKYPRT-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       4801-DELETE-PARA-SEC            SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-DELETE-PARA-EXT.
           EXIT.
      *************************************************************
      *    自画面編集処理
      *************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF.
      *
           IF      SPA-SIDCD       NOT =   SPACE
               PERFORM 520-SIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU.
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "S06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *************************************************************
      *    画面編集処理
      *************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    S06-SYULIST-ROW     TO  WRK-SYULIST-ROW
      *
           MOVE    SPACE               TO  S06
           INITIALIZE                      S06
      *
           IF    ( FLG-GMN-INIT        =   1 )
               MOVE    ZERO            TO  S06-SYULIST-ROW
               MOVE    ZERO            TO  S06-SYULIST-ROWATTR
           ELSE
               MOVE    WRK-SYULIST-ROW TO  S06-SYULIST-ROW
               MOVE    ZERO            TO  S06-SYULIST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  S06-PTNUM
           MOVE    SPA-GMN-NAME        TO  S06-NAME
      *
           MOVE    SPA-GMN-NYUGAIKBN-G TO  S06-NYUGAIKBN
           MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  S06-NYUGAIKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUGAIKBN-MAX )
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX1)
                                       TO  S06-NYUGAIKBN-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYU-MISYU   TO  S06-NYU-MISYU
           MOVE    SPA-GMN-GAI-MISYU   TO  S06-GAI-MISYU
      *
           MOVE    SPA-GMN-SELNUM      TO  S06-SELNUM
      *
           MOVE    SPA-GMN-RD-NYUKIN-KBT
                                       TO  S06-RD-NYUKIN-KBT
           MOVE    SPA-GMN-RD-NYUKIN-IKT
                                       TO  S06-RD-NYUKIN-IKT
      *
           MOVE    SPA-GMN-NYKNKNRKBN-G
                                       TO  S06-NYKNKNRKBN
           MOVE    SPA-GMN-NYKNKNRKBN-MAX
                                       TO  S06-NYKNKNRKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYKNKNRKBN-MAX )
               MOVE    SPA-GMN-NYKNKNRKBNLST (IDX1)
                                       TO  S06-NYKNKNRKBN-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-IKTSRYYMKBN-G
                                       TO  S06-IKTSRYYMKBN
           MOVE    SPA-GMN-IKTSRYYMKBN-MAX
                                       TO  S06-IKTSRYYMKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-IKTSRYYMKBN-MAX )
               MOVE    SPA-GMN-IKTSRYYMKBNLST (IDX1)
                                       TO  S06-IKTSRYYMKBN-ITEM (IDX1)
           END-PERFORM
      *
           IF    ( SPA-GMN-IKTSRYYMKBN =   "2" )
               MOVE    SPA-GMN-IKTSRYYM    TO  S06-IKTSRYYM
           END-IF
      *
           MOVE    SPA-GMN-NYUKINYMD   TO  S06-NYUKINYMD
      *
           MOVE    SPA-GMN-NYKNHOHO-G
                                       TO  S06-NYKNHOHO
           MOVE    SPA-GMN-NYKNHOHO-MAX
                                       TO  S06-NYKNHOHO-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYKNHOHO-MAX )
               MOVE    SPA-GMN-NYKNHOHOLST (IDX1)
                                       TO  S06-NYKNHOHO-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYPRTKBN-G
                                       TO  S06-SKYPRTKBN
           MOVE    SPA-GMN-SKYPRTKBN-MAX
                                       TO  S06-SKYPRTKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SKYPRTKBN-MAX )
               MOVE    SPA-GMN-SKYPRTKBNLST (IDX1)
                                       TO  S06-SKYPRTKBN-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYSUMKBN-G
                                       TO  S06-SKYSUMKBN
           MOVE    SPA-GMN-SKYSUMKBN-MAX
                                       TO  S06-SKYSUMKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SKYSUMKBN-MAX )
               MOVE    SPA-GMN-SKYSUMKBNLST (IDX1)
                                       TO  S06-SKYSUMKBN-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-GAI-NYUKIN  TO  S06-GAI-NYUKIN
           MOVE    SPA-GMN-GAI-HENKIN  TO  S06-GAI-HENKIN
           MOVE    SPA-GMN-NYU-NYUKIN  TO  S06-NYU-NYUKIN
           MOVE    SPA-GMN-NYU-HENKIN  TO  S06-NYU-HENKIN
           MOVE    SPA-GMN-NYUKIN      TO  S06-NYUKIN
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
      *
               MOVE    "上限"          TO  S06-GAI-LNYUKIN-MAX
      *
               MOVE    SPA-GMN-GAI-NYUKIN-MAX
                                       TO  WRK-LBLKINGAKU
               PERFORM 800-LBLKINGAKU-SEC
               MOVE    WRK-LBLKINGAKUX
                                       TO  S06-GAI-NYUKIN-MAX
      *
               MOVE    "上限"          TO  S06-GAI-LHENKIN-MAX
               MOVE    SPA-GMN-GAI-HENKIN-MAX
                                       TO  WRK-LBLKINGAKU
               PERFORM 800-LBLKINGAKU-SEC
               MOVE    WRK-LBLKINGAKUX
                                       TO  S06-GAI-HENKIN-MAX
               MOVE    "上限"          TO  S06-NYU-LNYUKIN-MAX
               MOVE    SPA-GMN-NYU-NYUKIN-MAX
                                       TO  WRK-LBLKINGAKU
               PERFORM 800-LBLKINGAKU-SEC
               MOVE    WRK-LBLKINGAKUX
                                       TO  S06-NYU-NYUKIN-MAX
               MOVE    "上限"          TO  S06-NYU-LHENKIN-MAX
               MOVE    SPA-GMN-NYU-HENKIN-MAX
                                       TO  WRK-LBLKINGAKU
               PERFORM 800-LBLKINGAKU-SEC
               MOVE    WRK-LBLKINGAKUX
                                       TO  S06-NYU-HENKIN-MAX
           END-IF
      *
      *
           IF    ( SPA-NAI-HENKINFLG   =   ZERO )
               MOVE    "入金額"        TO  S06-LBLNYUKIN
           ELSE
               MOVE    "返金額"        TO  S06-LBLNYUKIN
           END-IF
      *
      *
           IF    ( SPA-GMN-RD-NYUKIN-KBT   =   "T" )
               MOVE    "上限"          TO  S06-LNYUKIN-MAX
               MOVE    SPA-GMN-NYUKIN-MAX
                                       TO  WRK-LBLKINGAKU
               PERFORM 800-LBLKINGAKU-SEC
               MOVE    WRK-LBLKINGAKUX TO  S06-NYUKIN-MAX
           END-IF
      *
           MOVE   SPA-GMN-SYULIST-OUT-MAX
                                       TO  S06-SYULIST-COUNT
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-SYULIST-OUT-MAX )
      *
               MOVE    SPA-GMN-SYULIST-OUT-IDX (IDX2)
                                       TO  IDX1
      *
               MOVE    IDX2                    TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  S06-TNUM       (IDX2)
               IF    ( SPA-NAI-TNYUGAIKBN(IDX1)        =   "1" )
                   IF    ( SPA-NAI-TCREATEKBN(IDX1)    =   "1" )
                       STRING  SPA-GMN-TDENPNUM(IDX1)  DELIMITED BY SIZE
                               " 退"                   DELIMITED BY SIZE
                       INTO    S06-TDENPNUM   (IDX2) 
                       END-STRING
                   ELSE
                       STRING  SPA-GMN-TDENPNUM(IDX1)  DELIMITED BY SIZE
                               " 定"                   DELIMITED BY SIZE
                       INTO    S06-TDENPNUM   (IDX2) 
                       END-STRING
                   END-IF
               ELSE
                   MOVE    SPA-GMN-TDENPNUM(IDX1)
                                               TO  S06-TDENPNUM   (IDX2)
               END-IF
               MOVE    SPA-GMN-TNYUGAIKBN(IDX1)
                                               TO  S06-TNYUGAI    (IDX2)
               MOVE    SPA-GMN-TSRYKA(IDX1)    TO  S06-TSRYKA     (IDX2)
               MOVE    SPA-GMN-THKNCOMBI(IDX1) TO  S06-THKN       (IDX2)
               MOVE    SPA-GMN-TFTNRATE(IDX1)  TO  S06-TFTNRATE   (IDX2)
               MOVE    SPA-GMN-THAKYMD (IDX1)  TO  S06-TSKYSTYMD  (IDX2)
               MOVE    SPA-GMN-TSRYYMD (IDX1)  TO  S06-TSRYYMD    (IDX2)
               MOVE    SPA-GMN-TSEIKYU(IDX1)   TO  WRK-KINGAKU
               MOVE    WRK-KINGAKU             TO  S06-TSKYMONEY  (IDX2)
               MOVE    SPA-GMN-TMISYU (IDX1)   TO  WRK-KINGAKU
               MOVE    WRK-KINGAKU             TO  S06-TMISYUMONEY(IDX2)
               MOVE    SPA-GMN-TNYUKIN (IDX1)  TO  WRK-KINGAKU
               MOVE    WRK-KINGAKU             TO  S06-TNYKNMONEY(IDX2)
      *
               MOVE    1                       TO  IDX0
      *
               IF    ( SPA-GMN-SYULIST-SEL     =   IDX2 )
                   MOVE    "T"                 TO  S06-SYULIST-SELECT
                                                                  (IDX2)
               ELSE
                   MOVE    "F"                 TO  S06-SYULIST-SELECT
                                                                  (IDX2)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-MISYUFLG        =   1 )
               MOVE    "blue"              TO  S06-LBLMSG-STYLE
               MOVE    SPA-GMN-LBLMSG      TO  S06-LBLMSG
           END-IF
      *
           PERFORM 5001-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       5001-GMNHEN-STATE-EDIT-SEC      SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  S06-SELNUM-STATE
                                           S06-NYKNKNRKBN-STATE
                                           S06-NYKNKNRKBN-ITEM-STATE
                                           S06-NYU-NYUKIN-STATE
                                           S06-GAI-NYUKIN-STATE
                                           S06-NYU-HENKIN-STATE
                                           S06-GAI-HENKIN-STATE
                                           S06-NYUKIN-STATE
                                           S06-SKYSUMKBN-STATE
                                           S06-SKYSUMKBN-ITEM-STATE
                                           S06-IKTSRYYMKBN-STATE
                                           S06-IKTSRYYMKBN-ITEM-STATE
      *
           IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S06-SELNUM-STATE
                                           S06-NYUKIN-STATE
               IF    ( SPA-GMN-IKTSRYYMKBN =   "1" )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  S06-IKTSRYYM-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  S06-NYKNKNRKBN-STATE
                                           S06-NYKNKNRKBN-ITEM-STATE
                                           S06-NYU-NYUKIN-STATE
                                           S06-GAI-NYUKIN-STATE
                                           S06-NYU-HENKIN-STATE
                                           S06-GAI-HENKIN-STATE
                                           S06-IKTSRYYMKBN-STATE
                                           S06-IKTSRYYMKBN-ITEM-STATE
                                           S06-IKTSRYYM-STATE
           END-IF
      *
           .
       5001-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *************************************************************
      *    エラーメッセージ表示理
      *************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                   TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入外区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "選択番号入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "入金管理区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "外来入金額にマイナスは設定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "外来入金額が上限を上回っています"
                                           TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "入院入金額にマイナスは設定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "入院入金額が上限を上回っています"
                                           TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "入金額にマイナスは設定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "入金額が上限を上回っています"
                                           TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "請求書兼領収書発行区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "入金日入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "選択番号を入力してください"
                                           TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "入金方法入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "外来分発行方法入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "診療年月指定エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "外来返金額にマイナスは設定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "外来返金額が上限を上回っています"
                                           TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "入院返金額にマイナスは設定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "入院返金額が上限を上回っています"
                                           TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "返金額にマイナスは設定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "返金額が上限を上回っています"
                                           TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                   TO  SERR
           MOVE    SPA-ERRCD               TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG              TO  SERR-ERRMSG
           MOVE    "B01"                   TO  MCP-WIDGET
      *
           MOVE    "S06"                   TO  SPA-MOTOPG
           MOVE    "SERR"                  TO  SPA-SAKIPG
      *
           MOVE    "NEW"                   TO  MCP-PUTTYPE
           MOVE    "SERR"                  TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                       TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SIDMSG
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2001"
               MOVE    "入金処理を行います"
                                        TO  WRK-SIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  SID1
           INITIALIZE                       SID1
           MOVE    SPA-SIDCD            TO  SID1-ID1CODE
           MOVE    WRK-SIDMSG           TO  SID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S06"                TO  SPA-MOTOPG
           MOVE    "SID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SIDSET-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-GMN-CUR         =   ZERO )
               PERFORM 50011-GMN-CUR-SET-SEC
           END-IF
      *
           PERFORM 50012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE  "NYUGAIKBN"       TO  MCP-WIDGET
           WHEN    02
               MOVE  "SELNUM"          TO  MCP-WIDGET
           WHEN    03
               MOVE  "NYKNKNRKBN"      TO  MCP-WIDGET
           WHEN    04
               MOVE  "GAI_NYUKIN"      TO  MCP-WIDGET
           WHEN    05
               MOVE  "NYU_NYUKIN"      TO  MCP-WIDGET
           WHEN    06
               MOVE  "NYUKIN"          TO  MCP-WIDGET
           WHEN    07
               MOVE  "NYUKINYMD"       TO  MCP-WIDGET
           WHEN    08
               MOVE  "SKYPRTKBN"       TO  MCP-WIDGET
           WHEN    09
               MOVE  "NYKNHOHO"        TO  MCP-WIDGET
           WHEN    10
               MOVE  "SKYSUMKBN"       TO  MCP-WIDGET
           WHEN    11
               MOVE  "IKTSRYYMKBN"     TO  MCP-WIDGET
           WHEN    12
               MOVE  "IKTSRYYM"        TO  MCP-WIDGET
           WHEN    13
               MOVE  "GAI_HENKIN"      TO  MCP-WIDGET
           WHEN    14
               MOVE  "NYU_HENKIN"      TO  MCP-WIDGET
           WHEN    212
               MOVE  "B12"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力カーソル位置編集処理
      *****************************************************************
       50011-GMN-CUR-SET-SEC      SECTION.
      *
      *    次のカーソル位置を決定
           EVALUATE    WRK-MCP-WIDGET
      *    入外区分　　　　→　外来入金額（または入金額）
           WHEN    "NYUGAIKBN"
               IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
                   MOVE    04          TO  SPA-GMN-CUR
               ELSE
                   MOVE    06          TO  SPA-GMN-CUR
               END-IF
      *    選択番号　　　　→　入金額
           WHEN    "SELNUM"
               MOVE    06              TO  SPA-GMN-CUR
      *    入金管理区分　　→　診療年月指定区分
           WHEN    "NYKNKNRKBN"
               MOVE    11              TO  SPA-GMN-CUR
      *    診療年月指定区分→　一括入金診療年月
           WHEN    "IKTSRYYMKBN"
               MOVE    12              TO  SPA-GMN-CUR
      *    一括入金診療年月　　→　外来入金額
           WHEN    "IKTSRYYM"
               IF    ( SPA-GMN-GAI-NYUKIN-MAX   =  ZERO )
                AND  ( SPA-GMN-GAI-NYUKIN       =  ZERO )
                AND  ( SPA-GMN-GAI-HENKIN-MAX   =  ZERO )
                AND  ( SPA-GMN-GAI-HENKIN       =  ZERO )
                AND  ( SPA-GMN-NYU-NYUKIN-MAX   =  ZERO )
                AND  ( SPA-GMN-NYU-NYUKIN       =  ZERO )
                AND  ( SPA-GMN-NYU-HENKIN-MAX   =  ZERO )
                AND  ( SPA-GMN-NYU-HENKIN       =  ZERO )
                   MOVE    12          TO  SPA-GMN-CUR
               ELSE
                   MOVE    04          TO  SPA-GMN-CUR
               END-IF
      *    外来入金額　　　→　外来返金額
           WHEN    "GAI_NYUKIN"
               MOVE    13              TO  SPA-GMN-CUR
      *    外来返金額　　　→　入院入金額
           WHEN    "GAI_HENKIN"
               MOVE    05              TO  SPA-GMN-CUR
      *    入院入金額　　　→　入院返金額
           WHEN    "NYU_NYUKIN"
               MOVE    14              TO  SPA-GMN-CUR
      *    入院返金額　　　→　外来入金額
           WHEN    "NYU_HENKIN"
               MOVE    04              TO  SPA-GMN-CUR
      *    入金額　　　　　→　選択番号
           WHEN    "NYUKIN"
               MOVE    02              TO  SPA-GMN-CUR
      *    入金日　　→入金方法
           WHEN    "NYUKINYMD"
               MOVE    09              TO  SPA-GMN-CUR
      *    入金方法　→請求書兼領収書
           WHEN    "NYKNHOHO"
               MOVE    08              TO  SPA-GMN-CUR
      *    請求兼領収書　　→外来分発行方法
           WHEN    "SKYPRTKBN"
               MOVE    10              TO  SPA-GMN-CUR
      *    外来分発行方法　　→外来入金額
           WHEN    "SKYSUMKBN"
               MOVE    04              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-GMN-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       50012-CUR-SKIP-SEC              SECTION.
      *
           IF    ( SPA-GMN-CUR         =   02 )
               IF    ( S06-SELNUM-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    04              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   06 )
               IF    ( S06-NYUKIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    04              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   03 )
               IF    ( S06-NYKNKNRKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   11 )
               IF    ( S06-IKTSRYYMKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   12 )
               IF    ( S06-IKTSRYYM-STATE
                                       =   WIDGET-INSENSITIVE )
                   IF    ( SPA-GMN-RD-NYUKIN-IKT   =   "T" )
                       MOVE    04      TO  SPA-GMN-CUR
                   ELSE
                       MOVE    06      TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   04 )
               IF    ( S06-GAI-NYUKIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    06              TO  SPA-GMN-CUR
               ELSE
                   IF    ( SPA-GMN-GAI-NYUKIN-MAX   =  ZERO )
                    AND  ( SPA-GMN-GAI-NYUKIN       =  ZERO )
                       MOVE    13          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   04 )
               IF    ( S06-GAI-HENKIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    06              TO  SPA-GMN-CUR
               ELSE
                   IF    ( SPA-GMN-GAI-HENKIN-MAX   =  ZERO )
                    AND  ( SPA-GMN-GAI-HENKIN       =  ZERO )
                       MOVE    05          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   05 )
               IF    ( S06-NYU-NYUKIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    06              TO  SPA-GMN-CUR
               ELSE
                   IF    ( SPA-GMN-NYU-NYUKIN-MAX   =  ZERO )
                    AND  ( SPA-GMN-NYU-NYUKIN       =  ZERO )
                       MOVE    14          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   14 )
               IF    ( S06-NYU-HENKIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    06              TO  SPA-GMN-CUR
               ELSE
                   IF    ( SPA-GMN-NYU-HENKIN-MAX   =  ZERO )
                    AND  ( SPA-GMN-NYU-HENKIN       =  ZERO )
                       MOVE    04          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   10 )
               IF    ( S06-SKYSUMKBN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    212         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       50012-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       800-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                          ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           .
       800-CLIENT-PRT-EXT.
           EXIT.
      *****************************************************************
      *    クライアントダミー印刷　処理
      *****************************************************************
       800-CLIENT-DUMMY-PRT-SEC        SECTION.
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           .
       800-CLIENT-DUMMY-PRT-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-YM-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "2"                 TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-YM-HEN-EXT.
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
      *    文字列左詰処理
      *****************************************************************
       800-LTRIM-SEC               SECTION.
      *
           IF    ( WRK-STR         =   SPACE )
               CONTINUE
           ELSE
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >       500 )
      *
                   IF    ( WRK-STR (IDXL:1)    NOT =   SPACE )
                       MOVE    WRK-STR (IDXL:)
                                           TO  WRK-STR-TMP
                       MOVE    WRK-STR-TMP TO  WRK-STR
                       MOVE    500         TO  IDXL
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       800-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    金額編集処理
      *****************************************************************
       800-LBLKINGAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-LBLKINGAKUX
      *
           IF    ( WRK-LBLKINGAKU      <   1000000 )
               MOVE    WRK-LBLKINGAKU      TO  WRK-NUMZS
               STRING  WRK-NUMZS           DELIMITED   BY  SIZE
                       "円"                DELIMITED   BY  SIZE
               INTO    WRK-LBLKINGAKUX
               END-STRING
           ELSE
               MOVE    WRK-LBLKINGAKU      TO  WRK-NUMZL
               STRING  WRK-NUMZL           DELIMITED   BY  SIZE
                       "円"                DELIMITED   BY  SIZE
               INTO    WRK-LBLKINGAKUX
               END-STRING
           END-IF
      *
           .
       800-LBLKINGAKU-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    WRK-SPA-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    WRK-SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    WRK-SPA-PTID        TO  SYU-PTID
           MOVE    WRK-SYU-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    WRK-KUMINO          TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    WRK-HKNID           TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *************************************************************
      *    ＰＵＴ　処理
      *************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
