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
       PROGRAM-ID.                 ORCGS03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納登録
      *  コンポーネント名  : 請求確認　（Ｓ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    NACL-太田    02/12/12  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−小豆沢  01/04/10  無処理時のＰＦ１２キー無効対処
      *  01.01.02    MCC-藤原     01/06/27  金額項目の変更
      *  01.01.03    MCC-藤原     01/07/12  自費名称編集追加
      *  01.01.04    MCC-藤原     01/07/16  日付画面用チェックサブ追加
      *  01.01.05    MCC−小豆沢  01/09/25  コラムリストの選択追加
      *  01.01.06    MCC-藤原     01/11/07  S01の削除
      *  01.01.07    MCC-藤原     01/11/15  選択番号がないときのエラー
      *                                     の追加
      *  01.01.08    MCC-小豆沢   02/02/20  収納明細の表示に診療訂正の
      *                                     表示を追加(伝票状態区分＝８)
      *  01.01.09    MCC-藤原     02/02/27  未収理由の削除、自費消費税
      *                                     なしの追加
      *  01.01.10    NACL-藤原    02/11/05  入外区分追加
      *  01.01.11    NACL-太田    02/12/12  前回未収額表示の不具合を修正
      *  01.01.12    NACL-太田    02/12/13  請求金額を超えて入金できる
      *                                     不具合を修正
      *  01.01.13    NACL-太田    02.12.16  コラムリストの金額欄を
      *                                     カンマ編集するよう修正
      *  01.01.14    NACL-森脇    03.01.06  S04の追加
      *  01.01.15    NACL-太田    03.10.27  入金上限額のチェックを修正
      *  01.01.16    NACL-太田    03.10.29  履歴修正処理修正
      *  01.01.17    NACL-太田    03.11.12  更新日付編集対応
      *  01.01.18    NACL-森脇    03.12.11  処理区分表示修正
      *  01.01.19    NACL-太田    04.02.09  コラムリスト編集修正
      *  01.01.20    NACL-太田    04.07.12  コラムリストに入金方法を追加
      *  01.01.21    NACL-小豆沢  05.07.15  自費１０項目の対応
      *  03.05.00    NACL-太田    07/05/20  グループ診療対応
      *  04.07.00    NACL-太田    11/12/08  S03Nを吸収
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
      *    収納用共通パラメタ
           COPY    "S01COMMON-SPA".
      *
           COPY    ENUM-VALUE.
      *
      *    スパ領域
       01  SPA-S03.
      *
           03  SPA-S03-AREA.
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-MAX                     PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-HEAD-AREA.
                       09  SPA-GMN-PTNUM           PIC X(20).
                       09  SPA-GMN-KANANAME        PIC X(100).
                       09  SPA-GMN-SEX             PIC X(02).
                       09  SPA-GMN-KUMI            PIC X(50).
                       09  SPA-GMN-RITU            PIC X(10).
                       09  SPA-GMN-SRYYMD          PIC X(09).
                       09  SPA-GMN-NAME            PIC X(100).
                       09  SPA-GMN-BIRTHDAY        PIC X(09).
                       09  SPA-GMN-SRYKA           PIC X(16).
                       09  SPA-GMN-NYUGAIKBN       PIC X(06).
                       09  SPA-GMN-JOUTAI          PIC X(10).
                       09  SPA-GMN-HAKYMD          PIC X(09).
                       09  SPA-GMN-DENPNO          PIC X(10).
                       09  SPA-GMN-SYORIKBN-G.
                           11  SPA-GMN-SYORIKBN    PIC X(01).
                           11  SPA-GMN-SYORIKBNFL  PIC X(01).
                           11  SPA-GMN-SYORIKBNMEI PIC X(10).
      *
                   07  SPA-GMN-KINGAKU-AREA.
                       09  SPA-GMN-HKNRYO          PIC 9(07)
                                                   OCCURS  17.
                       09  SPA-GMN-TGMONEY         PIC 9(07)
                                                   OCCURS  17.
                       09  SPA-GMN-HKNTEKI         PIC 9(10).
                       09  SPA-GMN-SKJRYOYO        PIC 9(07).
                       09  SPA-GMN-LIFERYOYO       PIC 9(07).
                       09  SPA-GMN-SKJFTN          PIC 9(07).
                       09  SPA-GMN-LIFEFTN         PIC 9(07).
                       09  SPA-GMN-ROUFTN          PIC 9(07).
                       09  SPA-GMN-KOHFTN          PIC 9(07).
                       09  SPA-GMN-JIHI-G          OCCURS  2.
                           11  SPA-GMN-JIHI        PIC 9(07)
                                                   OCCURS 10.
                           11  SPA-GMN-JIHIKEI     PIC 9(07).
                       09  SPA-GMN-JIHIZEI         PIC 9(07).
                       09  SPA-GMN-HKNGAI          PIC 9(07).
                       09  SPA-GMN-ICHIFTN         PIC 9(07).
                       09  SPA-GMN-YAKFTN          PIC 9(07).
                       09  SPA-GMN-CHOSEI1         PIC S9(07).
                       09  SPA-GMN-CHOSEI2         PIC S9(07).
                       09  SPA-GMN-RMSAGAKU        PIC 9(07).
                       09  SPA-GMN-SKYMONEY        PIC 9(07).
                       09  SPA-GMN-ZENMISYU        PIC 9(07).
                       09  SPA-GMN-ZENMISYU-GAI    PIC 9(07).
                       09  SPA-GMN-ZENMISYU-NYU    PIC 9(07).
                       09  SPA-GMN-SEIGOK          PIC S9(07).
                       09  SPA-GMN-MISYU-ZAN       PIC S9(07).
                       09  SPA-GMN-NYUKIN-TTL      PIC 9(08).
                   07  SPA-GMN-INPUT-AREA.
                       09  SPA-GMN-CHOSEI1IN       PIC S9(10).
                       09  SPA-GMN-CHOSEI1INX      PIC X(10).
                       09  SPA-GMN-CHOSEI2IN       PIC S9(10).
                       09  SPA-GMN-CHOSEI2INX      PIC X(10).
                       09  SPA-GMN-SKYMONEYIN      PIC S9(10).
                       09  SPA-GMN-NYUKIN          PIC S9(10).
                       09  SPA-GMN-NYUKINX         PIC X(10).
                       09  SPA-GMN-NYUKINYMD       PIC X(10).
                       09  SPA-GMN-NYKN-HOHO-G.
                           11  SPA-GMN-NYKN-HOHO       PIC X(02).
                           11  SPA-GMN-NYKN-HOHOFL     PIC X(01).
                           11  SPA-GMN-NYKN-HOHOMEI    PIC X(30).
                   07  SPA-GMN-SYORIKBNLST-G.
                       09  SPA-GMN-SYORIKBN-MAX        PIC 9(05).
                       09  SPA-GMN-SYORIKBNLST         OCCURS  5.
                           11  SPA-GMN-SYORIKBNL       PIC X(01).
                           11  SPA-GMN-SYORIKBNFL      PIC X(01).
                           11  SPA-GMN-SYORIKBNMEIL    PIC X(10).
                   07  SPA-GMN-NYKN-HOHOLST-G.
                       09  SPA-GMN-NYKN-HOHO-MAX       PIC 9(05).
                       09  SPA-GMN-NYKN-HOHOLST        OCCURS  50.
                           11  SPA-GMN-NYKN-HOHOL      PIC X(02).
                           11  SPA-GMN-NYKN-HOHOFL     PIC X(01).
                           11  SPA-GMN-NYKN-HOHOMEIL   PIC X(30).
      *
                   07  SPA-GMN-SELNUM              PIC 9(02).
                   07  SPA-GMN-SKYPRT-G.
                       09  SPA-GMN-SKYPRT          PIC X(01).
                       09  SPA-GMN-SKYPRTFL        PIC X(01).
                       09  SPA-GMN-SKYPRTMEI       PIC X(22).
                   07  SPA-GMN-SKYPRTLST-G.
                       09  SPA-GMN-SKYPRT-MAX      PIC 9(05).
                       09  SPA-GMN-SKYPRTLST       OCCURS  5.
                           11  SPA-GMN-SKYPRTL     PIC X(01).
                           11  SPA-GMN-SKYPRTFLL   PIC X(01).
                           11  SPA-GMN-SKYPRTMEIL  PIC X(22).
                   07  SPA-GMN-SRYPRT-G.
                       09  SPA-GMN-SRYPRT          PIC X(01).
                       09  SPA-GMN-SRYPRTFL        PIC X(01).
                       09  SPA-GMN-SRYPRTMEI       PIC X(22).
                   07  SPA-GMN-SRYPRTLST-G.
                       09  SPA-GMN-SRYPRT-MAX      PIC 9(05).
                       09  SPA-GMN-SRYPRTLST       OCCURS  5.
                           11  SPA-GMN-SRYPRTL     PIC X(01).
                           11  SPA-GMN-SRYPRTFLL   PIC X(01).
                           11  SPA-GMN-SRYPRTMEIL  PIC X(22).
                   07  SPA-GMN-JIHIMSG             PIC X(8)
                                                   OCCURS  10.
                   07  SPA-GMN-RSISHOSHIN          PIC 9(10).
                   07  SPA-GMN-RSISAISHIN          PIC 9(10).
                   07  SPA-GMN-RSISDO              PIC 9(10).
                   07  SPA-GMN-RSIETC              PIC 9(10).
      *
               05  SPA-GMN-SMEITBL-AREA.
                   07  SPA-GMN-SMEILIST-MAX        PIC 9(02).
                   07  SPA-GMN-SMEILIST-SEL        PIC 9(02).
                   07  SPA-GMN-SMEITBL-OCC         OCCURS  99.
                       09  SPA-GMN-TDENPEDANUM     PIC 9(02).
                       09  SPA-GMN-TSKYMONEY       PIC S9(10).
                       09  SPA-GMN-TNYUKIN         PIC S9(10).
                       09  SPA-GMN-TSYORIYMD       PIC X(09).
                       09  SPA-GMN-TJOUTAI         PIC X(10).
                       09  SPA-GMN-TNYKN-HOHO      PIC X(10).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-HEAD-AREA.
                       09  SPA-NAI-MEIDSPCTRL      PIC X(01).
                       09  SPA-NAI-MEIDSPKBN       PIC X(01).
                       09  SPA-NAI-HEAD-DENPJTIKBN PIC X(01).
                       09  SPA-NAI-SRYKA           PIC X(02).
                       09  SPA-NAI-DISCOUNT-KBN        PIC  X(02).
                       09  SPA-NAI-DISCOUNT-BODY       PIC  X(160).
                       09  SPA-NAI-DISCOUNT-RATEKBN    PIC  X(02).
                       09  SPA-NAI-DISCOUNT-TEIRITU    PIC  X(01).
                       09  SPA-NAI-DISCOUNT-RATE       PIC  9(07).
                       09  SPA-NAI-DISCOUNT-MONEY      PIC  9(07).
                       09  SPA-NAI-GRP-DENPNUM         PIC  9(07).
                   07  SPA-NAI-INPUT-AREA.
                       09  SPA-NAI-CHOSEI1IN-MOTO  PIC S9(10).
                       09  SPA-NAI-CHOSEI2IN-MOTO  PIC S9(10).
                       09  SPA-NAI-SKYMONEY-MOTO   PIC S9(10).
                       09  SPA-NAI-SKYMONEY        PIC S9(10).
                       09  SPA-NAI-NYUKINYMD       PIC X(08).
                       09  SPA-NAI-NYUKIN-MOTO     PIC S9(10).
                       09  SPA-NAI-DENPEDANUM      PIC X(02).
                       09  SPA-NAI-SYURRKNUM       PIC 9(09).
                       09  SPA-NAI-SYUEDANUM       PIC 9(09).
                       09  SPA-NAI-SYURRKUPDKBN    PIC X(1).
                       09  SPA-NAI-DENPJTIKBN      PIC X(01).
                       09  SPA-NAI-MINUS           PIC X(01).
                       09  SPA-NAI-PROC-FLG        PIC 9(01).
                   07  SPA-NAI-NYKN-HOHOLST-G.
                       09  SPA-NAI-NYKN-HOHOLST    OCCURS  50.
                           11  SPA-NAI-NYKN-HOHOMEIL
                                                   PIC X(10).
      *
               05  SPA-NAI-SMEITBL-AREA.
                   07  SPA-NAI-SMEITBL-OCC         OCCURS  99.
                       09  SPA-NAI-TNYKN-HOHO      PIC X(02).
                       09  SPA-NAI-TJOUTAIKBN      PIC X(1).
                       09  SPA-NAI-TMEIJOUTAIKBN   PIC X(1).
                       09  SPA-NAI-TSYORIYMD       PIC X(08).
                       09  SPA-NAI-TMINUS          PIC X(1).
                       09  SPA-NAI-TSYURRKNUM      PIC 9(9).
                       09  SPA-NAI-TSYUEDANUM      PIC 9(9).
                       09  SPA-NAI-TSYURRKUPDKBN   PIC X(1).
                       09  SPA-NAI-TCHOSEI1        PIC S9(10).
                       09  SPA-NAI-TCHOSEI2        PIC S9(10).
                       09  SPA-NAI-TPROC-FLG       PIC 9(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
           03  FLG-SYUMEI                          PIC 9(01).
           03  FLG-SYORI                           PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-SYURRK                          PIC 9(01).
      *
           03  FLG-SMEILIST1-ROW                   PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
           03  FLG-TORIKESHI                       PIC 9(01).
           03  FLG-MEIVISIBLE                      PIC 9(01).
           03  FLG-ORCSNUMERR                      PIC 9(01).
           03  FLG-SYUTTL                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX-SKY                             PIC 9(05).
           03  IDX-SRY                             PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-LST                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD PIC X(08).
               05  WRK-FILEMEI-HMS PIC X(06).
               05  WRK-FILEMEI-CD  PIC X(06).
           03  WRK-SKYMONEY                        PIC S9(10).
           03  WRK-NYUKIN                          PIC S9(10).
           03  WRK-KAKO-NYUKIN                     PIC S9(10).
           03  WRK-MISYU-ZAN                       PIC S9(10).
           03  WRK-MOTOPG                          PIC X(08).
           03  WRK-SIDMSG                          PIC X(70).
           03  WRK-SYMD                            PIC X(10).
           03  WRK-HENYMDG                         PIC X(10).
           03  WRK-Z9                              PIC --,---,---.
           03  WRK-Z99                             PIC --,---,--9.
           03  WRK-ZZ9                             PIC ZZ9.
           03  WRK-EDABAN                          PIC ZZ.
           03  WRK-MISYU                           PIC 9(08).
           03  WRK-NYUGAIKBN                       PIC X(01).
           03  WRK-GMN-RITU.
               05  WRK-GMN-FL                      PIC X(02).
               05  WRK-GMN-FTNRITU                 PIC ZZZ.
               05  WRK-GMN-PA                      PIC X(01).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
           03  WRK-JIHI-IDX-X.
               05  WRK-JIHI-IDX                    PIC 9(01).
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-DENPEDANUM                      PIC 9(02).
           03  WRK-PRINT-DENPEDANUM                PIC 9(02).
           03  WRK-SKYPRT-SKYMONEY                 PIC S9(10).
           03  WRK-SKYPRT-NYUKIN                   PIC S9(10).
           03  WRK-SKYPRT-TAX-SAI                  PIC S9(10).
           03  WRK-SEIKYU-PG                       PIC X(20).
           03  WRK-MEISAI-PG                       PIC X(20).
           03  WRK-SKYPRTKBN                       PIC X(01).
           03  WRK-SRYPRTKBN                       PIC X(01).
           03  WRK-MEIDSPKBN                       PIC X(01).
           03  WRK-OLD-NYUHEN-YMD                  PIC X(08).
           03  WRK-KINGAKUX                        PIC X(10).
           03  WRK-KINGAKU9                        PIC S9(10).
           03  WRK-KINGAKUZ                        PIC ----------.
           03  WRK-SKYPRT                          PIC X(01).
           03  WRK-SRYPRT                          PIC X(01).
           03  WRK-GMN-CUR                         PIC 9(3).
      *
       01  WRK-ROW.
           03  WRK-SMEILIST1-ROW                   PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-KINGAKU-MAX       PIC 9(10)   VALUE 9999999.
           03  CONST-SMEILIST-MAX      PIC 9(03)   VALUE   99.
           03  CONST-JIHI-MAX          PIC 9(03)   VALUE  10.
           03  CONST-PROC-NO-G.
               05  CONST-PROC-NYUKIN   PIC X(02)   VALUE   "05".
               05  CONST-PROC-RRKSYU   PIC X(02)   VALUE   "08".
               05  CONST-PROC-TORIKESHI
                                       PIC X(02)   VALUE   "10".
               05  CONST-PROC-SYOKAI   PIC X(02)   VALUE   "09".
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
           COPY    "CPSK1001.INC".
           COPY    "CPSK1013.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK1041.INC".
           COPY    "CPSK5000.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納履歴
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    収納合計マスタ
       01  SYUTOTAL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *************************************************************
      *    サブプロ用　領域
      *************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    収納履歴編集サブ
           COPY    "CPORCSS002.INC".
      *
       01  LNK-SYUNOU-REC.
           COPY "CPSYUNOU.INC" REPLACING //SYU-// BY //LNK-SYU-//.
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    請求書・明細書編集サブ
           COPY    "CPSSUB03.INC".
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
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE        TO  SPA-S03
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
      *
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *
      *        画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *
      *    入力
            WHEN   OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S03        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *************************************************************
      *    初期　処理
      *************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "SERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "S03"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *
      *
      *************************************************************
      *    初期画面処理
      *************************************************************
       300-SCREEN-SEC                  SECTION.
      *
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG 
      *
      *        元の画面へ戻す
               IF    ( WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "E01" )
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-S03
                   MOVE    SPA-WORK        TO  SPA-S01KYOUTU
               END-IF
      *
      *        予約とエラー表示の時、そのまま画面表示へ
               IF    ( WRK-MOTOPG(1:3)     =   "Y01" )
                   GO  TO  300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF    ( WRK-MOTOPG(1:3)     =   "U01" )
                   GO  TO  300-SCREEN-EXT
               ELSE
                   MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
               END-IF
           END-IF
      *
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "SID1"
               PERFORM 330-SID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPA-SET-SEC
               MOVE    1           TO  FLG-SMEILIST1-ROW
           END-EVALUATE
      *
           .

       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＳＩＤ１）ＯＫ処理
      *****************************************************************
       330-SID1-SET-SEC            SECTION.
      *
           IF    ( SPA-SID1-FLG    =   "OK" )
               EVALUATE    SPA-SIDCD
               WHEN    "2001"
                   PERFORM 4101-TOROKU-SEC
               END-EVALUATE
      *
           END-IF
      *
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID1-SET-EXT.
           EXIT.
      *
      *************************************************************
      *    画面編集処理
      *************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           INITIALIZE                  SPA-S03
           PERFORM 3101-SPA-SET-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       3101-SPA-SET-SEC                 SECTION.
      *
      *    収納項目設定処理
           PERFORM 3101-SYUNOU-SET-SEC
      *
      *    コンボ編集処理
           PERFORM 3101-COMBO-SET-SEC
      *
      *    基本項目設定処理
           PERFORM 3101-KHN-SET-SEC
      *
      *    収納明細項目設定処理
           PERFORM 3101-SYUMEI-SET-SEC
      *
      *    入金欄
           EVALUATE    SPA-S01-SYORINO
           WHEN    CONST-PROC-NYUKIN
           WHEN    CONST-PROC-RRKSYU
           WHEN    CONST-PROC-TORIKESHI
      *
               MOVE    SPA-SYSYMD      TO  SPA-NAI-NYUKINYMD
               MOVE    SPA-SYSYMD      TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUKINYMD
      *
               IF    ( SPA-GMN-NYKN-HOHO-MAX   >   ZERO )
                   MOVE    SPA-S01-NYUKIN-HOHO TO  WRK-CMB-CD
                   PERFORM 4200-NYKN-HOHOLST-CHK-SEC
                   IF    ( WRK-CMB-ITM =   SPACE )
                       MOVE    SPA-GMN-NYKN-HOHOLST (1)
                                       TO  SPA-GMN-NYKN-HOHO-G
                   ELSE
                       MOVE    WRK-CMB-ITM
                                       TO  SPA-GMN-NYKN-HOHO-G
                   END-IF
               END-IF
      *
           END-EVALUATE
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
      *    請求書・明細書発行コンボ初期値
           PERFORM 3101-PRINTLST-SYOKI-SEC
      *
           EVALUATE   SPA-S01-SYORINO
           WHEN   CONST-PROC-NYUKIN
           WHEN   CONST-PROC-TORIKESHI
               MOVE    2               TO  SPA-GMN-CUR
           WHEN   OTHER
               MOVE    1               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       3101-SPA-SET-EXT.
           EXIT.
      *
      *************************************************************
      *    コンボ項目設定処理
      *************************************************************
       3101-COMBO-SET-SEC              SECTION.
      *
           PERFORM 3101-SYORIKBNLST-SET-SEC
      *
           PERFORM 3101-NYKN-HOHOLST-SET-SEC
      *
           PERFORM 3101-SKYPRTLST-SET-SEC
      *
           PERFORM 3101-SRYPRTLST-SET-SEC
      *
           .
       3101-COMBO-SET-EXT.
           EXIT.
      *************************************************************
      *    処理区分コンボ設定処理
      *************************************************************
       3101-SYORIKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-SYORIKBNLST-G
      *
           IF    ( SPA-NAI-HEAD-DENPJTIKBN =   "3" )
      *
               MOVE    "2"                 TO  SPA-GMN-SYORIKBNL (1)
               MOVE    "履歴修正"          TO  SPA-GMN-SYORIKBNMEIL (1)
      *
               MOVE    "3"                 TO  SPA-GMN-SYORIKBNL (2)
               MOVE    "入金取消"          TO  SPA-GMN-SYORIKBNMEIL (2)
      *
               MOVE    "4"                 TO  SPA-GMN-SYORIKBNL (3)
               MOVE    "照会"              TO  SPA-GMN-SYORIKBNMEIL (3)
      *
               MOVE    3                   TO  SPA-GMN-SYORIKBN-MAX
           ELSE
      *
               MOVE    "1"                 TO  SPA-GMN-SYORIKBNL (1)
               MOVE    "入金"              TO  SPA-GMN-SYORIKBNMEIL (1)
      *
               MOVE    "2"                 TO  SPA-GMN-SYORIKBNL (2)
               MOVE    "履歴修正"          TO  SPA-GMN-SYORIKBNMEIL (2)
      *
               MOVE    "3"                 TO  SPA-GMN-SYORIKBNL (3)
               MOVE    "入金取消"          TO  SPA-GMN-SYORIKBNMEIL (3)
      *
               MOVE    "4"                 TO  SPA-GMN-SYORIKBNL (4)
               MOVE    "照会"              TO  SPA-GMN-SYORIKBNMEIL (4)
      *
               MOVE    4                   TO  SPA-GMN-SYORIKBN-MAX
           END-IF
      *
           .
       3101-SYORIKBNLST-SET-EXT.
           EXIT.
      *************************************************************
      *    入金方法コンボ設定処理
      *************************************************************
       3101-NYKN-HOHOLST-SET-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-NYKN-HOHOLST-G
                                           SPA-NAI-NYKN-HOHOLST-G
      *
      *    入金方法
           MOVE   ZERO                 TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1041-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-S01-SRYYMD      TO  SYS-1041-STYUKYMD
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
           MOVE    ZERO                TO  IDX3
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               COMPUTE IDX3    =   IDX3    +   1
      *
               MOVE    SYS-1041-KBNCD  TO  SPA-GMN-NYKN-HOHOL    (IDX3)
               MOVE    SYS-1041-NYKN-NAME
                                       TO  SPA-GMN-NYKN-HOHOMEIL (IDX3)
               MOVE    SYS-1041-NYKN-TANNAME
                                       TO  SPA-NAI-NYKN-HOHOMEIL (IDX3)
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-FETCH-SEC
               MOVE    MCPDATA-REC     TO  SYS-1041-REC
      *
           END-PERFORM
      *
           MOVE    IDX3                TO  SPA-GMN-NYKN-HOHO-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           .
       3101-NYKN-HOHOLST-SET-EXT.
           EXIT.
      *************************************************************
      *    請求書発行コンボ設定処理
      *************************************************************
       3101-SKYPRTLST-SET-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-SKYPRTLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-SKYPRTL (1)
           MOVE    "発行しない"        TO  SPA-GMN-SKYPRTMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-SKYPRTL (2)
           MOVE    "発行する"          TO  SPA-GMN-SKYPRTMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-SKYPRT-MAX
      *
           .
       3101-SKYPRTLST-SET-EXT.
           EXIT.
      *************************************************************
      *    明細書発行コンボ設定処理
      *************************************************************
       3101-SRYPRTLST-SET-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-SRYPRTLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-SRYPRTL (1)
           MOVE    "発行しない"        TO  SPA-GMN-SRYPRTMEIL (1)
      *
           MOVE    "1"                 TO  SPA-GMN-SRYPRTL (2)
           MOVE    "発行する"          TO  SPA-GMN-SRYPRTMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-SRYPRT-MAX
      *
           .
       3101-SRYPRTLST-SET-EXT.
           EXIT.
      *************************************************************
      *    請求書・明細書発行コンボ初期値設定処理
      *************************************************************
       3101-PRINTLST-SYOKI-SEC         SECTION.
      *
           INITIALIZE                  SSUB03-AREA
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           MOVE    SPA-GMN-SKYPRT      TO  SSUB03-SKYPRT
           MOVE    SPA-GMN-SRYPRT      TO  SSUB03-SRYPRT
           MOVE    1                   TO  SSUB03-REQNO
           CALL    "ORCGSSUB03"        USING
                                       WRK-SPA-AREA
                                       SPA-S01KYOUTU
                                       SSUB03-AREA
      *
           MOVE    SSUB03-SKYPRT       TO  SPA-GMN-SKYPRT
           MOVE    SSUB03-SRYPRT       TO  SPA-GMN-SRYPRT
      *
           PERFORM 4101-SKYPRT-CHK-SEC
           PERFORM 4101-SRYPRT-CHK-SEC
      *
           .
       3101-PRINTLST-SYOKI-EXT.
           EXIT.
      *************************************************************
      *    基本項目設定処理
      *************************************************************
       3101-KHN-SET-SEC                SECTION.
      *
           MOVE    SPA-S01-PTNUM       TO  SPA-GMN-PTNUM
           MOVE    SPA-S01-KANANAME    TO  SPA-GMN-KANANAME
           MOVE    SPA-S01-NAME        TO  SPA-GMN-NAME
           MOVE    SPA-S01-SEX         TO  SPA-GMN-SEX
           MOVE    SPA-S01-BIRTHDAYW   TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-S01-HKNCOMBIMEI TO  SPA-GMN-KUMI
           MOVE    SPA-S01-SRYKA       TO  SPA-NAI-SRYKA
           MOVE    SPA-S01-SRYKAMEI    TO  SPA-GMN-SRYKA
      *
           EVALUATE    SPA-S01-NYUGAIKBN
           WHEN    "1"
               MOVE    "入院"          TO  SPA-GMN-NYUGAIKBN
           WHEN    "2"
               MOVE    "外来"          TO  SPA-GMN-NYUGAIKBN
           END-EVALUATE
      *
      *    処理区分
           EVALUATE    SPA-S01-SYORINO
           WHEN    CONST-PROC-NYUKIN
               MOVE    "1"             TO  WRK-CMB-CD
           WHEN    CONST-PROC-RRKSYU
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN    CONST-PROC-TORIKESHI
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN    CONST-PROC-SYOKAI
               MOVE    "4"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-SYORIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SYORIKBN-G
      *
           .
       3101-KHN-SET-EXT.
           EXIT.
      *************************************************************
      *    収納項目設定処理
      *************************************************************
       3101-SYUNOU-SET-SEC             SECTION.
      *
      *    伝票番号
           MOVE    SPA-S01-DENPNUM     TO  SPA-GMN-DENPNO
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU    NOT   =   ZERO )
               GO  TO  3101-SYUNOU-SET-EXT
           END-IF
      *
           IF    ( SYU-HKNTEKKBN       =   "1" )
               MOVE    SYU-KOH-FTN     TO  SYU-HKNTEKMONEY
               MOVE    ZERO            TO  SYU-KOH-FTN
           END-IF
      *
      *    診療年月
           MOVE    SYU-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-SRYYMD
      *
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-HAKYMD
      *
      *    患者負担率
           IF    ( SYU-PTFTNRATE       =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-RITU
           ELSE
               MOVE    SPACE           TO  WRK-GMN-RITU
               MOVE    SYU-PTFTNRATE   TO  WRK-GMN-FTNRITU
               MOVE    "%"             TO  WRK-GMN-PA
               MOVE    WRK-GMN-RITU    TO  SPA-GMN-RITU
           END-IF.
      *
      *    伝票状態区分
           EVALUATE   SYU-DENPJTIKBN
           WHEN   SPACE
               MOVE  "請求額なし"      TO  SPA-GMN-JOUTAI
           WHEN   "1"
               MOVE  "入金済"          TO  SPA-GMN-JOUTAI
           WHEN   "2"
               MOVE  "未入金"          TO  SPA-GMN-JOUTAI
           WHEN   "3"
               MOVE  "請求取消し"      TO  SPA-GMN-JOUTAI
           WHEN   "7"
               MOVE  "継続"            TO  SPA-GMN-JOUTAI
           END-EVALUATE
      *
      *    保険料
           PERFORM VARYING     IDX0    FROM    1   BY  1
                   UNTIL     ( IDX0    >   16 )
               MOVE    SYU-HKNTEN (IDX0)
                                       TO  SPA-GMN-HKNRYO (IDX0)
      *
           END-PERFORM
      *
           MOVE    SYU-TOTAL-HKNTEN    TO SPA-GMN-HKNRYO (17)
      *
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-HEAD-DENPJTIKBN
      *
      *    保険適用外
           PERFORM VARYING     IDX0     FROM    1   BY  1
                   UNTIL     ( IDX0     >   16 )
      *
               MOVE    ZERO            TO   SPA-GMN-TGMONEY (IDX0)
               COMPUTE SPA-GMN-TGMONEY (IDX0)
                     = SYU-TGMONEY (IDX0)  +   SYU-TGMONEY-TAX (IDX0)
           END-PERFORM
      *
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY     TO  SPA-GMN-HKNTEKI
      *
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  SPA-GMN-ROUFTN
      *
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  SPA-GMN-KOHFTN
      *
      *    その他自費
           PERFORM VARYING IDX0 FROM    1   BY  1
                   UNTIL ( IDX0 >   10)
      *
               MOVE    SYU-JIHI-TAXNASI (IDX0)
                                       TO  SPA-GMN-JIHI (1 IDX0)
               MOVE    SYU-JIHI-TAXARI  (IDX0)
                                       TO  SPA-GMN-JIHI (2 IDX0)
      *
           END-PERFORM
      *
           MOVE    SYU-JIHI-TOTAL      TO  SPA-GMN-JIHIKEI(1)
      *
           MOVE    SYU-JIHI-TOTAL-TAX  TO  SPA-GMN-JIHIKEI(2)
      *
           MOVE    SYU-JIHI-TAX        TO  SPA-GMN-JIHIZEI
      *
      *    保険適用外
           MOVE    ZERO                TO  SPA-GMN-HKNGAI.
           COMPUTE  SPA-GMN-HKNGAI
                 =  SYU-TOTAL-TGMONEY  +   SYU-TOTAL-TGMONEY-TAX
      *
      *    労災保険適用分
           MOVE    SYU-RSISHOSHIN-MONEY
                                       TO SPA-GMN-RSISHOSHIN
           MOVE    SYU-RSISAISHIN-MONEY
                                       TO SPA-GMN-RSISAISHIN
           MOVE    SYU-RSISDO-MONEY    TO SPA-GMN-RSISDO
           MOVE    SYU-RSIETC-MONEY    TO SPA-GMN-RSIETC
      *
      *    食事療養費
           COMPUTE SPA-GMN-SKJRYOYO
               =   SYU-RYOYOHI-SKJ
               +   SYU-RYOYOHI-SKJ-JIHI
      *
      *    生活療養費
           COMPUTE SPA-GMN-LIFERYOYO
               =   SYU-RYOYOHI-LIFE
               +   SYU-RYOYOHI-LIFE-JIHI
      *
      *    食事負担額
           COMPUTE SPA-GMN-SKJFTN
               =   SYU-SKYMONEY-SKJ-KEI
               +   SYU-SKYMONEY-SKJ-JIHI-KEI
      *
      *    生活負担額
           COMPUTE SPA-GMN-LIFEFTN
               =   SYU-SKYMONEY-LIFE-KEI
               +   SYU-SKYMONEY-LIFE-JIHI-KEI
      *
      *    薬剤一部負担  
           MOVE    SYU-YKZ-FTN         TO  SPA-GMN-YAKFTN.
      *
      *    一部負担金
           COMPUTE SPA-GMN-ICHIFTN     =   SPA-GMN-ROUFTN
                                       +   SPA-GMN-KOHFTN
                                       +   SPA-GMN-YAKFTN
                                       +   SPA-GMN-SKJFTN
                                       +   SPA-GMN-LIFEFTN
      *    室料差額
           MOVE    SYU-RMSAGAKU        TO  SPA-GMN-RMSAGAKU
      *
      *    調整金１
           MOVE    SYU-CHOSEI1         TO  SPA-GMN-CHOSEI1
      *
      *    調整金２
           MOVE    SYU-CHOSEI2         TO  SPA-GMN-CHOSEI2
      *
      *    入金額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-NYUKIN-TTL
      *
      *    未収残
           COMPUTE SPA-GMN-MISYU-ZAN
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
      *    減免情報
           MOVE    SYU-DISCOUNT-KBN    TO  SPA-NAI-DISCOUNT-KBN
           MOVE    SYU-DISCOUNT-BODY   TO  SPA-NAI-DISCOUNT-BODY
           MOVE    SYU-DISCOUNT-RATEKBN TO SPA-NAI-DISCOUNT-RATEKBN
           MOVE    SYU-DISCOUNT-TEIRITU TO SPA-NAI-DISCOUNT-TEIRITU
           MOVE    SYU-DISCOUNT-RATE   TO  SPA-NAI-DISCOUNT-RATE
           MOVE    SYU-DISCOUNT-MONEY  TO  SPA-NAI-DISCOUNT-MONEY
      *
      *    グループ伝票番号
           MOVE    SYU-GRP-DENPNUM     TO  SPA-NAI-GRP-DENPNUM
      *
      *    合計請求額計算処理
           PERFORM 3102-SEIKYU-KEI-SEC
      *
      *    前回未収
           PERFORM 3101-ZENMISYU-SEC
      *
      *    合計未収額
           IF    ( SPA-GMN-MISYU-ZAN   >   ZERO )
               COMPUTE SPA-GMN-SEIGOK      =   SPA-GMN-MISYU-ZAN
                                           +   SPA-GMN-ZENMISYU-GAI
                                           +   SPA-GMN-ZENMISYU-NYU
           ELSE
               COMPUTE SPA-GMN-SEIGOK      =   SPA-GMN-ZENMISYU-GAI
                                           +   SPA-GMN-ZENMISYU-NYU
           END-IF.
      *
           .
       3101-SYUNOU-SET-EXT.
           EXIT.
      *************************************************************
      *    収納明細項目設定処理
      *************************************************************
       3101-SYUMEI-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-SMEITBL-AREA
                                       SPA-NAI-SMEITBL-AREA
      *
           MOVE    ZERO                TO  SPA-GMN-SMEILIST-MAX
      *
      *    収納明細マスターを検索し内容をスパ領域にセット
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SMEI-PTID
           MOVE    SPA-S01-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE     ZERO               TO  FLG-TORIKESHI
           MOVE     ZERO               TO  IDX1
           PERFORM UNTIL ( FLG-SYUMEI NOT  =   ZERO )
                    OR   ( IDX1           >=   CONST-SMEILIST-MAX )
      *
             MOVE    1                   TO  FLG-MEIVISIBLE
      *
             IF    ( SMEI-JOUTAIKBN      =   "2" OR "4" OR "6" )
              AND  ( SMEI-NYUHEN-MONEY   =   ZERO )
                 MOVE      "1"           TO  SPA-NAI-MEIDSPCTRL
                 IF  ( SPA-NAI-MEIDSPKBN =   "1" )
                     MOVE    ZERO        TO  FLG-MEIVISIBLE
                 END-IF
             END-IF
      *
             IF    ( FLG-MEIVISIBLE      =   1 )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
      *
               MOVE    SMEI-DENPEDANUM     TO  SPA-GMN-TDENPEDANUM(IDX1)
               MOVE    SMEI-SYURRKNUM      TO  SPA-NAI-TSYURRKNUM(IDX1)
               MOVE    SMEI-SYUEDANUM      TO  SPA-NAI-TSYUEDANUM(IDX1)
               MOVE    SMEI-SYURRKUPDKBN TO  SPA-NAI-TSYURRKUPDKBN(IDX1)
               MOVE    SMEI-SKYMONEY       TO  SPA-GMN-TSKYMONEY (IDX1)
               MOVE    SMEI-NYUHEN-MONEY   TO  SPA-GMN-TNYUKIN (IDX1)
               MOVE    SMEI-JOUTAIKBN      TO  SPA-NAI-TJOUTAIKBN (IDX1)
               MOVE    SMEI-MEISAIJOUTAIKBN TO SPA-NAI-TMEIJOUTAIKBN
                                                                  (IDX1)
      *
               IF    ( SMEI-JOUTAIKBN      =   "4" OR  "6" )
                OR  (( SMEI-JOUTAIKBN      =   "8" )
                 AND ( SMEI-NYUHEN-MONEY   <   ZERO ))
                   MOVE    "1"             TO  SPA-NAI-TMINUS (IDX1)
               END-IF
      *
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    SPACE
                   MOVE   "請求額なし"
                                       TO  SPA-GMN-TJOUTAI (IDX1)
               WHEN    "1"
                   IF   ( SMEI-SKYMONEY    >   SMEI-NYUHEN-MONEY )
                       MOVE   "請求・未収"
                                       TO  SPA-GMN-TJOUTAI (IDX1)
                   ELSE
                       MOVE   "請求・入金"
                                       TO  SPA-GMN-TJOUTAI (IDX1)
                   END-IF
      *
               WHEN    "2"
                   MOVE    "入金"      TO  SPA-GMN-TJOUTAI (IDX1)
      *
               WHEN    "3"
                   MOVE    "再計算"    TO  SPA-GMN-TJOUTAI (IDX1)
      *
               WHEN    "4"
                   MOVE    "返金"      TO  SPA-GMN-TJOUTAI (IDX1)
      *
               WHEN    "5"
                   MOVE    "請求取消"  TO  SPA-GMN-TJOUTAI (IDX1)
                   MOVE    1           TO  FLG-TORIKESHI
      *
               WHEN    "6"
                   MOVE    "入金取消"  TO  SPA-GMN-TJOUTAI (IDX1)
      *
               WHEN    "7"
                   MOVE    "継続"      TO  SPA-GMN-TJOUTAI (IDX1)
      *
               WHEN    "8"
                   MOVE    "診療訂正"  TO  SPA-GMN-TJOUTAI (IDX1)
               WHEN    "9"
                   MOVE    "請求"      TO  SPA-GMN-TJOUTAI (IDX1)
               END-EVALUATE
      *
               MOVE    SMEI-NYUHEN-YMD TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSYORIYMD (IDX1)
      *
               MOVE    SMEI-NYUHEN-YMD TO  SPA-NAI-TSYORIYMD (IDX1)
      *
               MOVE    SMEI-NYUKIN-HOHO
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYKN-HOHOLST-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO SPA-GMN-TNYKN-HOHO (IDX1)
      *
               MOVE    SMEI-NYUKIN-HOHO
                                       TO SPA-NAI-TNYKN-HOHO (IDX1)
               EVALUATE    SPA-NAI-TJOUTAIKBN (IDX1)
               WHEN    " "
               WHEN    "1"
               WHEN    "9"
                   MOVE    1           TO  SPA-NAI-TPROC-FLG (IDX1)
               WHEN    "3"
               WHEN    "7"
               WHEN    "8"
               WHEN    "5"
                   MOVE    2           TO  SPA-NAI-TPROC-FLG (IDX1)
               WHEN    "2"
                   MOVE    3           TO  SPA-NAI-TPROC-FLG (IDX1)
               WHEN    "4"
               WHEN    "6"
                   MOVE    4           TO  SPA-NAI-TPROC-FLG (IDX1)
               END-EVALUATE
      *
               IF    ( SMEI-SYURRKNUM  NOT =   ZERO )
                   PERFORM 900-SYURRK-KEY-SEL-SEC
                   IF    ( FLG-SYURRK      =   ZERO )
                       MOVE    SYURRK-CHOSEI1
                                       TO  SPA-NAI-TCHOSEI1(IDX1)
                       MOVE    SYURRK-CHOSEI2
                                       TO  SPA-NAI-TCHOSEI2(IDX1)
                   ELSE
                       MOVE    ZERO    TO  SPA-NAI-TSYURRKNUM(IDX1)
                                           SPA-NAI-TSYUEDANUM(IDX1)
                   END-IF
               END-IF
      *
             END-IF
      *
               MOVE    "tbl_syumei"    TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUMEI-REC
               ELSE
                   MOVE    1           TO  FLG-SYUMEI
                   INITIALIZE              SYUMEI-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX1                TO  SPA-GMN-SMEILIST-MAX
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYUMEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC            SECTION.
      *
      *    医療機関情報−自費名称
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-S01-SRYYMD      TO  SYS-1013-STYUKYMD
                                           SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1013-REC
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               IF    ( SPA-S01-NYUGAIKBN   =   SYS-1013-KBNCD(1:1) )
                   MOVE   SYS-1013-KBNCD(2:1)
                                           TO  WRK-JIHI-IDX-X
                   IF    ( WRK-JIHI-IDX-X >=   "1" )   AND
                         ( WRK-JIHI-IDX-X <=   "9" )
                       MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-JIHIMSG
                                                    (WRK-JIHI-IDX)
                   ELSE
                       IF    SYS-1013-KBNCD(2:1)   =   "0"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-JIHIMSG (10)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-FETCH-SEC
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *************************************************************
      *    前回未収額算出処理
      *************************************************************
       3101-ZENMISYU-SEC               SECTION.
      *
           MOVE    "1"             TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SEC
           MOVE    WRK-MISYU       TO  SPA-GMN-ZENMISYU-NYU
      *
           MOVE    "2"             TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SEC
           MOVE    WRK-MISYU       TO  SPA-GMN-ZENMISYU-GAI
      *
           COMPUTE SPA-GMN-ZENMISYU
               =   SPA-GMN-ZENMISYU-NYU
               +   SPA-GMN-ZENMISYU-GAI
      *
           .
       3101-ZENMISYU-EXT.
           EXIT.
      *
      *************************************************************
      *    未収額算出処理
      *************************************************************
       3101-MISYU-SEC                  SECTION.
      *
           MOVE   ZERO                 TO  WRK-MISYU
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  =  1 )
      *
               MOVE    ZERO            TO  FLG-SYORI
      *
               IF     (SPA-S01-DENPPRTYMD  <  SYU-DENPPRTYMD )
                OR   ((SPA-S01-NYUGAIKBN   =  SYU-NYUGAIKBN )
                  AND (SPA-S01-DENPNUM     <= SYU-DENPNUM   ))
                       MOVE    1     TO      FLG-SYORI
               END-IF
      *
               IF    ( FLG-SYORI           =   ZERO )
                   IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                       COMPUTE WRK-MISYU
                           =   WRK-MISYU
                           + ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYUNOU-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-MISYU-EXT.
           EXIT.
      *************************************************************
      *    合計請求額計算処理
      *************************************************************
       3102-SEIKYU-KEI-SEC             SECTION.
      *
      *    今回請求額
           MOVE   SYU-SKYMONEY         TO  SPA-GMN-SKYMONEY 
      *
           .
       3102-SEIKYU-KEI-EXT.
           EXIT.
      *************************************************************
      *    戻る　処理
      *************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "S02"               TO  SPA-SAKIPG
           MOVE    "S03"              TO  SPA-MOTOPG
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
      *
      *************************************************************
      *    自画面編集処理
      *************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-SIDCD       NOT =   SPACE )
                   PERFORM 520-SIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "S03    "          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    S03-CLIST1-ROW     TO  WRK-SMEILIST1-ROW
      *
           MOVE    SPACE               TO  S03
           INITIALIZE                      S03
      *
           EVALUATE SPA-S01-SYORINO
      *
           WHEN    CONST-PROC-NYUKIN
               MOVE    "履歴修正"      TO  S03-B03-LABEL
      *
           WHEN    CONST-PROC-RRKSYU
               MOVE    "照会"          TO  S03-B03-LABEL
      *
           WHEN    CONST-PROC-SYOKAI
               MOVE    "入金"          TO  S03-B03-LABEL
      *
           WHEN    OTHER
               MOVE    "照会"          TO  S03-B03-LABEL
           END-EVALUATE
      *
           MOVE    SPA-GMN-PTNUM       TO  S03-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  S03-KANANAME
           MOVE    SPA-GMN-NAME        TO  S03-NAME
           MOVE    SPA-GMN-SEX         TO  S03-SEX
           MOVE    SPA-GMN-KUMI        TO  S03-HKNCOMBI
           MOVE    SPA-GMN-RITU        TO  S03-RATE
           MOVE    SPA-GMN-SRYYMD      TO  S03-SRYYMD
           MOVE    SPA-GMN-BIRTHDAY    TO  S03-BIRTHDAY
           MOVE    SPA-GMN-SRYKA       TO  S03-SRYKA
           MOVE    SPA-GMN-NYUGAIKBN   TO  S03-NYUGAIKBN
      *
           MOVE    SPA-GMN-SYORIKBN-G  TO  S03-NAIYO
           MOVE    SPA-GMN-SYORIKBN-MAX
                                       TO  S03-NAIYO-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SYORIKBN-MAX )
               MOVE    SPA-GMN-SYORIKBNLST (IDX0)
                                       TO  S03-NAIYOLST(IDX0)
           END-PERFORM
      *
           MOVE    SPA-GMN-JOUTAI      TO  S03-DENPJTI
           MOVE    SPA-GMN-HAKYMD      TO  S03-HAKYMD
           MOVE    SPA-S01-DENPNUM     TO  S03-DENPNUM (3:)
      *
           MOVE    SPA-GMN-HKNRYO (01) TO  S03-SSUHKNTEN
           MOVE    SPA-GMN-HKNRYO (02) TO  S03-SDOHKNTEN
           MOVE    SPA-GMN-HKNRYO (03) TO  S03-ZTKHKNTEN
           MOVE    SPA-GMN-HKNRYO (04) TO  S03-TYKHKNTEN
           MOVE    SPA-GMN-HKNRYO (05) TO  S03-CSYHKNTEN
           MOVE    SPA-GMN-HKNRYO (06) TO  S03-SYCHKNTEN
           MOVE    SPA-GMN-HKNRYO (07) TO  S03-SJTHKNTEN
           MOVE    SPA-GMN-HKNRYO (08) TO  S03-MSIHKNTEN
           MOVE    SPA-GMN-HKNRYO (09) TO  S03-KNSHKNTEN
           MOVE    SPA-GMN-HKNRYO (10) TO  S03-GZUHKNTEN
           MOVE    SPA-GMN-HKNRYO (11) TO  S03-ETCHKNTEN
           MOVE    SPA-GMN-HKNRYO (12) TO  S03-SSNHKNTEN
           MOVE    SPA-GMN-HKNRYO (13) TO  S03-HOUHKNTEN
           MOVE    SPA-GMN-HKNRYO (14) TO  S03-BYRHKNTEN
           MOVE    SPA-GMN-HKNRYO (15) TO  S03-NYUHKNTEN
           MOVE    SPA-GMN-HKNRYO (16) TO  S03-RYOHKNTEN
           MOVE    SPA-GMN-HKNRYO (17) TO  S03-TOTALHKNTEN
           MOVE    SPA-GMN-HKNTEKI     TO  S03-HKNTEKMONEY
           MOVE    SPA-GMN-JIHI (1 1)  TO  S03-JIHI1
           MOVE    SPA-GMN-JIHI (1 2)  TO  S03-JIHI2
           MOVE    SPA-GMN-JIHI (1 3)  TO  S03-JIHI3
           MOVE    SPA-GMN-JIHI (1 4)  TO  S03-JIHI4
           MOVE    SPA-GMN-JIHI (1 5)  TO  S03-JIHI5
           MOVE    SPA-GMN-JIHI (1 6)  TO  S03-JIHI6
           MOVE    SPA-GMN-JIHI (1 7)  TO  S03-JIHI7
           MOVE    SPA-GMN-JIHI (1 8)  TO  S03-JIHI8
           MOVE    SPA-GMN-JIHI (1 9)  TO  S03-JIHI9
           MOVE    SPA-GMN-JIHI (1 10) TO  S03-JIHI10
           MOVE    SPA-GMN-JIHIKEI(1)  TO  S03-JIHITOTAL
           MOVE    SPA-GMN-JIHI (2 1)  TO  S03-JIHI1TAX
           MOVE    SPA-GMN-JIHI (2 2)  TO  S03-JIHI2TAX
           MOVE    SPA-GMN-JIHI (2 3)  TO  S03-JIHI3TAX
           MOVE    SPA-GMN-JIHI (2 4)  TO  S03-JIHI4TAX
           MOVE    SPA-GMN-JIHI (2 5)  TO  S03-JIHI5TAX
           MOVE    SPA-GMN-JIHI (2 6)  TO  S03-JIHI6TAX
           MOVE    SPA-GMN-JIHI (2 7)  TO  S03-JIHI7TAX
           MOVE    SPA-GMN-JIHI (2 8)  TO  S03-JIHI8TAX
           MOVE    SPA-GMN-JIHI (2 9)  TO  S03-JIHI9TAX
           MOVE    SPA-GMN-JIHI (2 10) TO  S03-JIHI10TAX
           MOVE    SPA-GMN-JIHIKEI(2)  TO  S03-JIHITAXTOTAL
           MOVE    SPA-GMN-JIHIZEI     TO  S03-JIHITAX
           MOVE    SPA-GMN-TGMONEY (01) TO  S03-SSUTGMONEY
           MOVE    SPA-GMN-TGMONEY (02) TO  S03-SDOTGMONEY
           MOVE    SPA-GMN-TGMONEY (03) TO  S03-ZTKTGMONEY
           MOVE    SPA-GMN-TGMONEY (04) TO  S03-TYKTGMONEY
           MOVE    SPA-GMN-TGMONEY (05) TO  S03-CSYTGMONEY
           MOVE    SPA-GMN-TGMONEY (06) TO  S03-SYCTGMONEY
           MOVE    SPA-GMN-TGMONEY (07) TO  S03-SJTGMONEY
           MOVE    SPA-GMN-TGMONEY (08) TO  S03-MSITGMONEY
           MOVE    SPA-GMN-TGMONEY (09) TO  S03-KNSTGMONEY
           MOVE    SPA-GMN-TGMONEY (10) TO  S03-GZUTGMONEY
           MOVE    SPA-GMN-TGMONEY (11) TO  S03-ETCTGMONEY
           MOVE    SPA-GMN-TGMONEY (12) TO  S03-SSNTGMONEY
           MOVE    SPA-GMN-TGMONEY (13) TO  S03-HOUTGMONEY
           MOVE    SPA-GMN-TGMONEY (14) TO  S03-BYRTGMONEY
           MOVE    SPA-GMN-TGMONEY (15) TO  S03-NYUTGMONEY
           MOVE    SPA-GMN-TGMONEY (16) TO  S03-RYOTGMONEY
           MOVE    SPA-GMN-HKNGAI      TO  S03-HKNTEKGAI
      *
           MOVE    "F"                 TO  S03-FR2-VISIBLE
                                           S03-FR3-VISIBLE
      *
           IF    ( SPA-S01-NYUGAIKBN   =   "1" )
      *
               MOVE    "T"             TO  S03-FR2-VISIBLE
               MOVE    SPA-GMN-SKJRYOYO    TO  S03-S03N-SKJRYOYO
               MOVE    SPA-GMN-LIFERYOYO   TO  S03-S03N-LIFERYOYO
      *        MOVE    SPA-GMN-YAKFTN      TO  S03-S03N-YKZFTN 
               MOVE    SPA-GMN-SKJFTN      TO  S03-S03N-SKJFTN
               MOVE    SPA-GMN-LIFEFTN     TO  S03-S03N-LIFEFTN
               MOVE    SPA-GMN-ROUFTN      TO  S03-S03N-RJNFTN
               MOVE    SPA-GMN-KOHFTN      TO  S03-S03N-KOHFTN
               MOVE    SPA-GMN-ICHIFTN     TO  S03-S03N-FTNTOTAL
               MOVE    SPA-GMN-RSISHOSHIN  TO  S03-S03N-RSISHOSHIN
               MOVE    SPA-GMN-RSISAISHIN  TO  S03-S03N-RSISAISHIN
               MOVE    SPA-GMN-RSISDO      TO  S03-S03N-RSISDO
               MOVE    SPA-GMN-RSIETC      TO  S03-S03N-RSIETC
               MOVE    SPA-GMN-RMSAGAKU    TO  S03-S03N-RMSAGAKU
               MOVE    SPA-GMN-CHOSEI1     TO  S03-S03N-CHOSEI1
               MOVE    SPA-GMN-CHOSEI2     TO  S03-S03N-CHOSEI2
               MOVE    SPA-GMN-SKYMONEY    TO  S03-S03N-SKYMONEY
               MOVE    SPA-GMN-NYUKIN-TTL  TO  S03-S03N-NYUKINGK1
               MOVE    SPA-GMN-MISYU-ZAN   TO  S03-S03N-MISYUZAN
               MOVE    SPA-GMN-ZENMISYU-GAI
                                           TO  S03-S03N-ZENMISYU-GAI
               MOVE    SPA-GMN-ZENMISYU-NYU
                                           TO  S03-S03N-ZENMISYU-NYU
               MOVE    SPA-GMN-SEIGOK      TO  S03-S03N-GOKMISYU
           ELSE
               MOVE    "T"             TO  S03-FR3-VISIBLE
               MOVE    SPA-GMN-YAKFTN      TO  S03-S03G-YKZFTN 
               MOVE    SPA-GMN-ROUFTN      TO  S03-S03G-RJNFTN
               MOVE    SPA-GMN-KOHFTN      TO  S03-S03G-KOHFTN
               MOVE    SPA-GMN-ICHIFTN     TO  S03-S03G-FTNTOTAL
               MOVE    SPA-GMN-RSISHOSHIN  TO  S03-S03G-RSISHOSHIN
               MOVE    SPA-GMN-RSISAISHIN  TO  S03-S03G-RSISAISHIN
               MOVE    SPA-GMN-RSISDO      TO  S03-S03G-RSISDO
               MOVE    SPA-GMN-RSIETC      TO  S03-S03G-RSIETC
               MOVE    SPA-GMN-CHOSEI1     TO  S03-S03G-CHOSEI1
               MOVE    SPA-GMN-CHOSEI2     TO  S03-S03G-CHOSEI2
               MOVE    SPA-GMN-SKYMONEY    TO  S03-S03G-SKYMONEY
               MOVE    SPA-GMN-NYUKIN-TTL  TO  S03-S03G-NYUKINGK1
               MOVE    SPA-GMN-MISYU-ZAN   TO  S03-S03G-MISYUZAN
               MOVE    SPA-GMN-ZENMISYU    TO  S03-S03G-ZENMISYU
               MOVE    SPA-GMN-SEIGOK      TO  S03-S03G-GOKMISYU
           END-IF
      *
           MOVE    SPA-GMN-CHOSEI1INX  TO  S03-CHOSEI1IN
           MOVE    SPA-GMN-CHOSEI2INX  TO  S03-CHOSEI2IN
           MOVE    SPA-GMN-SKYMONEYIN  TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  S03-SKYMONEYIN
      *
      *
      *    入金欄
           EVALUATE    SPA-S01-SYORINO
           WHEN    CONST-PROC-NYUKIN
           WHEN    CONST-PROC-TORIKESHI
           WHEN    CONST-PROC-RRKSYU
               MOVE    SPA-GMN-NYUKINX     TO  S03-NYUKINGK2
               MOVE    SPA-GMN-NYUKINYMD   TO  S03-NYUKINYMD
      *
               IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
                AND  ( SPA-NAI-MINUS       =   "1" )
                AND  ( SPA-NAI-DENPJTIKBN  NOT =   "6" )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-NYKN-HOHO-G TO  S03-NYUKINHOUHOU
                   MOVE    SPA-GMN-NYKN-HOHO-MAX
                                           TO  S03-NYUKINHOUHOU-COUNT
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   SPA-GMN-NYKN-HOHO-MAX )
                       MOVE    SPA-GMN-NYKN-HOHOLST (IDX0)
                                       TO  S03-NYUKINHOUHOULST(IDX0)
                   END-PERFORM
               END-IF
      *
               MOVE    SPA-GMN-SKYPRT-G    TO  S03-SKYPRT
               MOVE    SPA-GMN-SKYPRT-MAX  TO  S03-SKYPRT-COUNT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-SKYPRT-MAX )
                   MOVE    SPA-GMN-SKYPRTLST (IDX0)
                                           TO  S03-SKYPRTLST(IDX0)
               END-PERFORM
      *
               MOVE    SPA-GMN-SRYPRT-G    TO  S03-SRYPRT
               MOVE    SPA-GMN-SRYPRT-MAX  TO  S03-SRYPRT-COUNT
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-SRYPRT-MAX )
                   MOVE    SPA-GMN-SRYPRTLST (IDX0)
                                           TO  S03-SRYPRTLST(IDX0)
               END-PERFORM
      *
           END-EVALUATE
      *
           MOVE    SPA-GMN-SELNUM          TO  S03-RIREKIIN
      *
           MOVE    SPA-GMN-SMEILIST-MAX    TO  S03-CLIST1-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SMEILIST-MAX )
      *
               MOVE    IDX0                TO  WRK-EDABAN
               MOVE    WRK-EDABAN          TO  S03-TRENNUM (IDX0)
               MOVE    SPA-GMN-TSKYMONEY    (IDX0)
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  S03-TSKYMONY(IDX0)
               MOVE    SPA-GMN-TNYUKIN  (IDX0)
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  S03-TNYUKINMONEY(IDX0)
               MOVE    SPA-GMN-TSYORIYMD (IDX0)
                                           TO  S03-TSYORIYMD   (IDX0)
               IF    ( SPA-NAI-TMEIJOUTAIKBN  (IDX0)   =   "1" )
                   STRING  SPA-GMN-TJOUTAI  (IDX0)
                                           DELIMITED   BY  SPACE
                           "*"             DELIMITED   BY  SIZE
                   INTO    S03-TJTI        (IDX0)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-TJOUTAI  (IDX0)
                                           TO  S03-TJTI        (IDX0)
               END-IF
               MOVE    SPA-GMN-TNYKN-HOHO(IDX0)
                                           TO  S03-TNYKN-NAME  (IDX0)
               IF    ( SPA-GMN-SMEILIST-SEL    =   IDX0 )
                   MOVE    "T"             TO  S03-SELECT (IDX0)
               ELSE
                   MOVE    "F"             TO  S03-SELECT (IDX0)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-MEIDSPCTRL      =   1 )
                   MOVE    "表示切替"      TO  S03-B05-LABEL
           END-IF
      *
      *    自費名称
           PERFORM     VARYING IDX0    FROM    1   BY  1      
                       UNTIL ( IDX0    >       CONST-JIHI-MAX )
               MOVE    SPA-GMN-JIHIMSG(IDX0)
                                       TO  S03-JIHIMSG (IDX0)
           END-PERFORM
      *
      *
           MOVE    "black"             TO  S03-SKYMONEYIN-STYLE
           MOVE    WIDGET-NORMAL       TO  S03-CHOSEI1IN-STATE
                                           S03-CHOSEI2IN-STATE
                                           S03-SKYMONEYIN-STATE
                                           S03-NYUKINGK2-STATE
                                           S03-NYUKINYMD-STATE
                                           S03-NYUKINHOUHOU-STATE
                                           S03-NYUKINHOUHOULST-STATE
                                           S03-SKYPRT-STATE
                                           S03-SKYPRTLST-STATE
                                           S03-SRYPRT-STATE
                                           S03-SRYPRTLST-STATE
                                           S03-B12-STATE
                                           S03-B12S-STATE
                                           S03-NAIYO-STATE
                                           S03-NAIYOLST-STATE
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
      *
               MOVE    "blue"          TO  S03-SKYMONEYIN-STYLE
      *
               IF    ( SPA-NAI-MINUS           =   "1" )
                AND  ( SPA-NAI-DENPJTIKBN  NOT =   "6" )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  S03-NYUKINHOUHOU-STATE
                                           S03-NYUKINHOUHOULST-STATE
               END-IF
      *
               IF    ( SPA-NAI-PROC-FLG    =   1  OR 2 )
                   IF    ( SPA-NAI-SYURRKNUM   =   ZERO )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  S03-CHOSEI1IN-STATE
                                           S03-CHOSEI2IN-STATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
            AND  ( SPA-NAI-PROC-FLG    =   3  OR 4)
               MOVE    WIDGET-INSENSITIVE
                                       TO  S03-CHOSEI1IN-STATE
                                           S03-CHOSEI2IN-STATE
                                           S03-SKYMONEYIN-STATE
           END-IF
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-SYOKAI )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S03-CHOSEI1IN-STATE
                                           S03-CHOSEI2IN-STATE
                                           S03-SKYMONEYIN-STATE
                                           S03-NYUKINGK2-STATE
                                           S03-NYUKINYMD-STATE
                                           S03-NYUKINHOUHOU-STATE
                                           S03-NYUKINHOUHOULST-STATE
                                           S03-SKYPRT-STATE
                                           S03-SKYPRTLST-STATE
                                           S03-SRYPRT-STATE
                                           S03-SRYPRTLST-STATE
                                           S03-B12-STATE
           END-IF
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-NYUKIN
                                       OR  CONST-PROC-TORIKESHI )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S03-CHOSEI1IN-STATE
                                           S03-CHOSEI2IN-STATE
                                           S03-SKYMONEYIN-STATE
           END-IF
      *
      *
           IF    ( SPA-NAI-HEAD-DENPJTIKBN =   "3" )
               MOVE    WIDGET-INSENSITIVE 
                                       TO  S03-CHOSEI1IN-STATE
                                           S03-CHOSEI2IN-STATE
                                           S03-SKYMONEYIN-STATE
      *
               MOVE    SPACE               TO  S03-B03-LABEL
      *
               EVALUATE SPA-S01-SYORINO
               WHEN    CONST-PROC-RRKSYU
                   MOVE    "入金取消"      TO  S03-B03-LABEL
      *
               WHEN    CONST-PROC-TORIKESHI
                   MOVE    "照会"          TO  S03-B03-LABEL
      *
               WHEN    CONST-PROC-SYOKAI
                   MOVE    "履歴修正"      TO  S03-B03-LABEL
      *
               WHEN    OTHER
                   MOVE    "照会"          TO  S03-B03-LABEL
               END-EVALUATE
           ELSE
               EVALUATE SPA-S01-SYORINO
               WHEN    CONST-PROC-NYUKIN
                   MOVE    "履歴修正"      TO  S03-B03-LABEL
      *
               WHEN    CONST-PROC-RRKSYU
                   MOVE    "入金取消"      TO  S03-B03-LABEL
      *
               WHEN    CONST-PROC-TORIKESHI
                   MOVE    "照会"          TO  S03-B03-LABEL
      *
               WHEN    CONST-PROC-SYOKAI
                   MOVE    "入金"          TO  S03-B03-LABEL
      *
               WHEN    OTHER
                   MOVE    "照会"          TO  S03-B03-LABEL
               END-EVALUATE
           END-IF
      *
           MOVE    "black"         TO  S03-LBLGENMEN-STYLE
           IF    ( SPA-NAI-DISCOUNT-KBN  NOT =   SPACE )
               MOVE    "blue"  TO  S03-LBLGENMEN-STYLE
               EVALUATE    SPA-NAI-DISCOUNT-TEIRITU
               WHEN    "1"
                   MOVE    SPA-NAI-DISCOUNT-RATE
                                   TO  WRK-ZZ9
                   STRING  "減免有（"  DELIMITED   BY  SIZE
                           WRK-ZZ9     DELIMITED   BY  SIZE
                           "％）"      DELIMITED   BY  SIZE
                   INTO    S03-LBLGENMEN
                   END-STRING
               WHEN    "2"
                   MOVE    SPA-NAI-DISCOUNT-RATE
                                   TO  WRK-Z99
                   STRING  "減免有（"  DELIMITED   BY  SIZE
                           WRK-Z99     DELIMITED   BY  SIZE
                           "円）"      DELIMITED   BY  SIZE
                   INTO    S03-LBLGENMEN
                   END-STRING
               END-EVALUATE
           END-IF
      *
           EVALUATE    FLG-SMEILIST1-ROW
           WHEN    1
               MOVE    ZERO               TO  S03-CLIST1-ROW
               MOVE    ZERO               TO  S03-CLIST1-ROWATTR
           WHEN    2
               MOVE    S03-CLIST1-COUNT   TO  S03-CLIST1-ROW
               MOVE    1                  TO  S03-CLIST1-ROWATTR
           WHEN    OTHER
               MOVE    WRK-SMEILIST1-ROW  TO  S03-CLIST1-ROW
               MOVE    ZERO               TO  S03-CLIST1-ROWATTR
           END-EVALUATE
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *************************************************************
      *    画面カーソルセット処理
      *************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 50012-CUR-SKIP-SEC
      *
           EVALUATE   SPA-GMN-CUR
           WHEN   01
               MOVE  "RIREKIIN"        TO  MCP-WIDGET
           WHEN   02
               MOVE  "NYUKINGK2"       TO  MCP-WIDGET
           WHEN   03
               MOVE  "NYUKINYMD"       TO  MCP-WIDGET
           WHEN   04
               MOVE  "NYUKINHOUHOU"    TO  MCP-WIDGET
           WHEN   05
               MOVE  "SKYPRT"          TO  MCP-WIDGET
           WHEN   07
               MOVE  "SRYPRT"          TO  MCP-WIDGET
           WHEN   06
               MOVE  "NAIYO"           TO  MCP-WIDGET
           WHEN   08
               MOVE  "CHOSEI1IN"       TO  MCP-WIDGET
           WHEN   09
               MOVE  "CHOSEI2IN"       TO  MCP-WIDGET
           WHEN   112
               MOVE  "B12"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHOSEI1IN"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "CHOSEI2IN"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "RIREKIIN"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "NYUKINGK2"
               MOVE    04              TO  SPA-GMN-CUR
           WHEN    "NYUKINHOUHOU"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "NYUKINYMD"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "SKYPRT"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "SRYPRT"
               MOVE    112             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       50012-CUR-SKIP-SEC              SECTION.
      *
           IF    ( SPA-GMN-CUR             =   8 )
               IF   ( S03-CHOSEI1IN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    9           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   9 )
               IF   ( S03-CHOSEI2IN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *
           IF    ( SPA-GMN-CUR             =   6 )
               IF   ( S03-NAIYO-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   2 )
               IF   ( S03-NYUKINGK2-STATE =   WIDGET-INSENSITIVE )
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   4 )
               IF   ( S03-NYUKINHOUHOU-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   3 )
               IF   ( S03-NYUKINYMD-STATE =   WIDGET-INSENSITIVE )
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   5 )
               IF   ( S03-SKYPRT-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    7           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   7 )
               IF   ( S03-SRYPRT-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    112         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR             =   112 )
               IF   ( S03-B12-STATE       =   WIDGET-INSENSITIVE )
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       50012-CUR-SKIP-EXT.
           EXIT.
      *************************************************************
      *    画面遷移処理
      *************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"           ALSO    "B01"
               PERFORM 210-BACK
      *    処理区分
           WHEN    "CLICKED"           ALSO    "B03"
               PERFORM 430-SYORIKBN-SEC
      *    明細表示区分
           WHEN    "CLICKED"           ALSO    "B05"
               PERFORM 450-MEIDSPKBN-SEC
      *    予約登録
           WHEN    "CLICKED"           ALSO    "B10"
               PERFORM 480-YOYAKU-SEC
      *    受付一覧
           WHEN    "CLICKED"           ALSO    "B11"
               PERFORM 490-UKEICHI-SEC
      *    印刷
           WHEN    "CLICKED"           ALSO    "B11S"
               MOVE    "0"             TO  WRK-SKYPRT
               MOVE    "1"             TO  WRK-SRYPRT
               PERFORM 410-PRINT-SEC
      *    印刷
           WHEN    "CLICKED"           ALSO    "B12S"
               MOVE    "1"             TO  WRK-SKYPRT
               MOVE    "0"             TO  WRK-SRYPRT
               PERFORM 410-PRINT-SEC
      *    登録
           WHEN    "CLICKED"           ALSO    "B12"
               PERFORM 410-TOROKU-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *************************************************************
      *    会話　処理
      *************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
           WHEN    ANY             ALSO    "clist1"
               PERFORM 4100-SMEILIST-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "NAIYO"
               PERFORM 4100-SYORIKBN-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "RIREKIIN"
               PERFORM 4100-SELNUM-CHK-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
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
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    S03-CHOSEI1IN      TO  SPA-GMN-CHOSEI1INX
           MOVE    S03-CHOSEI2IN      TO  SPA-GMN-CHOSEI2INX
           MOVE    S03-NYUKINGK2      TO  SPA-GMN-NYUKINX
           MOVE    S03-NYUKINYMD      TO  SPA-GMN-NYUKINYMD
           MOVE    S03-NYUKINHOUHOU   TO  SPA-GMN-NYKN-HOHO-G
           MOVE    S03-SKYPRT         TO  SPA-GMN-SKYPRT-G
           MOVE    S03-SRYPRT         TO  SPA-GMN-SRYPRT-G
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
           EVALUATE    SPA-S01-SYORINO
      *
           WHEN    CONST-PROC-NYUKIN
               PERFORM 4102-NYUKIN-KIHON-CHK-SEC
      *
           WHEN    CONST-PROC-RRKSYU
               PERFORM 4102-RRKSYU-KIHON-CHK-SEC
      *
           WHEN    CONST-PROC-TORIKESHI
               PERFORM 4102-TORIKESHI-KIHON-CHK-SEC
      *
           WHEN    CONST-PROC-SYOKAI
               PERFORM 4102-SYOKAI-KIHON-CHK-SEC
           END-EVALUATE
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金処理時基本チェック処理
      *****************************************************************
       4102-NYUKIN-KIHON-CHK-SEC       SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-NYUKIN-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴修正処理時基本チェック処理
      *****************************************************************
       4102-RRKSYU-KIHON-CHK-SEC       SECTION.
      *
           IF    ( WRK-MCP-WIDGET      =   "SKYPRT" OR "SRYPRT" )
               CONTINUE
           ELSE
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI1IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI2IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-MINUS  =   "1" )
                   PERFORM 4102-NYUKIN-CHK-SEC
               ELSE
                   PERFORM 4101-NYUKIN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-RRKSYU-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金取消時基本チェック処理
      *****************************************************************
       4102-TORIKESHI-KIHON-CHK-SEC    SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4102-NYUKIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-TORIKESHI-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    照会処理時基本チェック処理
      *****************************************************************
       4102-SYOKAI-KIHON-CHK-SEC       SECTION.
      *
      *
           .
       4102-SYOKAI-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納明細リストチェック
      *****************************************************************
       4100-SMEILIST-CHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SMEILIST-SEL
                                       SPA-GMN-SELNUM
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
               INITIALIZE              SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-SMEILIST-MAX )
      *
               IF    ( S03-SELECT (IDX2)   =   "T" )
                   MOVE    IDX2    TO  SPA-GMN-SMEILIST-SEL
                                       SPA-GMN-SELNUM
                   IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
                       PERFORM 41001-INPUT-AREA-HEN-SEC
                   END-IF
      *
                   MOVE    2       TO  SPA-GMN-CUR
      *
                   MOVE    SPA-GMN-SMEILIST-MAX
                                   TO  IDX2
               END-IF
      *
           END-PERFORM
      *
           .
       4100-SMEILIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4100-SYORIKBN-CHK-SEC           SECTION.
      *
           MOVE    S03-NAIYO      TO  SPA-GMN-SYORIKBN-G
      *
           EVALUATE SPA-GMN-SYORIKBN
           WHEN    "1"
               MOVE    CONST-PROC-NYUKIN   TO  SPA-S01-SYORINO
           WHEN    "2"
               MOVE    CONST-PROC-RRKSYU   TO  SPA-S01-SYORINO
           WHEN    "3"
               MOVE    CONST-PROC-TORIKESHI TO SPA-S01-SYORINO
           WHEN    OTHER
               MOVE    CONST-PROC-SYOKAI   TO  SPA-S01-SYORINO
           END-EVALUATE
      *
           PERFORM 4200-SPA-SET-SEC
      *
           MOVE    1               TO  FLG-SMEILIST1-ROW
      *
           .
       4100-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-SMEILIST-SEL
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
               INITIALIZE              SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
           END-IF
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUKINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-NYUKINYMD
      *
           MOVE    S03-RIREKIIN   TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM  NOT =   ZERO )
               PERFORM 41001-SELNUM-CHK-SEC
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-S01-SYORINO     =   CONST-PROC-RRKSYU )
                       PERFORM 41001-INPUT-AREA-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNUM  >=  1 )
            AND  ( SPA-GMN-SELNUM  <=  SPA-GMN-SMEILIST-MAX )
               MOVE   SPA-GMN-SELNUM
                                   TO  SPA-GMN-SMEILIST-SEL
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0005"      TO  SPA-ERRCD
           END-IF
      *
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41001-INPUT-AREA-HEN-SEC        SECTION.
      *
           MOVE    SPA-GMN-TSKYMONEY (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-SKYMONEYIN
                                           SPA-NAI-SKYMONEY-MOTO
      *
           MOVE    SPA-NAI-TCHOSEI1 (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-CHOSEI1IN
                                           SPA-NAI-CHOSEI1IN-MOTO
      *
           MOVE    SPA-GMN-CHOSEI1IN   TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-CHOSEI1INX
      *
           MOVE    SPA-NAI-TCHOSEI2 (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-CHOSEI2IN
                                           SPA-NAI-CHOSEI2IN-MOTO
      *
           MOVE    SPA-GMN-CHOSEI2IN   TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-CHOSEI2INX
      *
           COMPUTE SPA-NAI-SKYMONEY
               =   SPA-GMN-SKYMONEYIN
               -   SPA-NAI-CHOSEI1IN-MOTO
               -   SPA-NAI-CHOSEI2IN-MOTO
      *
      *
           MOVE    SPA-GMN-TNYUKIN   (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-NYUKIN
                                           SPA-NAI-NYUKIN-MOTO
           MOVE    SPA-GMN-NYUKIN      TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-NYUKINX
           MOVE    SPA-GMN-TSYORIYMD (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-NYUKINYMD
      *
           MOVE    SPA-NAI-TNYKN-HOHO(SPA-GMN-SELNUM)
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYKN-HOHOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYKN-HOHO-G
      *
           MOVE    SPA-NAI-TSYORIYMD (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-NYUKINYMD
           MOVE    SPA-GMN-TDENPEDANUM(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-DENPEDANUM
           MOVE    SPA-NAI-TSYURRKNUM (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-SYURRKNUM
           MOVE    SPA-NAI-TSYUEDANUM (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-SYUEDANUM
           MOVE    SPA-NAI-TSYURRKUPDKBN (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-SYURRKUPDKBN
           MOVE    SPA-NAI-TJOUTAIKBN(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-DENPJTIKBN
           MOVE    SPA-NAI-TMINUS    (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-MINUS
           MOVE    SPA-NAI-TPROC-FLG  (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-PROC-FLG
      *
           .
       41001-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 41011-SELNUM-CHK-SEC
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE              SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
      *
               MOVE    SPA-SYSYMD  TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-NYUKINYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-NYUKINYMD
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       41011-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SMEILIST-SEL
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0006"      TO  SPA-ERRCD
           ELSE
               PERFORM 41001-SELNUM-CHK-SEC
           END-IF
      *
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額欄妥当性チェック処理
      *****************************************************************
       4101-KINGAKU-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-CHOSEI1IN
               MOVE    SPA-GMN-CHOSEI1INX
                                       TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    8           TO  SPA-GMN-CUR
                   MOVE    "0017"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-CHOSEI1IN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-CHOSEI1INX
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-CHOSEI2IN
               MOVE    SPA-GMN-CHOSEI2INX
                                       TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    9           TO  SPA-GMN-CUR
                   MOVE    "0018"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-CHOSEI2IN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-CHOSEI2INX
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
               MOVE    SPA-GMN-NYUKINX TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    2           TO  SPA-GMN-CUR
                   MOVE    "0019"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-NYUKIN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-NYUKINX
               END-IF
           END-IF
      *
           .
       4101-KINGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金１チェック処理
      *****************************************************************
       4101-CHOSEI1IN-CHK-SEC           SECTION.
      *
      *
           .
       4101-CHOSEI1IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金２チェック処理
      *****************************************************************
       4101-CHOSEI2IN-CHK-SEC           SECTION.
      *
           COMPUTE SPA-GMN-SKYMONEYIN
               =   SPA-NAI-SKYMONEY
               +   SPA-GMN-CHOSEI1IN
               +   SPA-GMN-CHOSEI2IN
      *
           MOVE    ZERO                TO  WRK-GMN-CUR
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-CHOSEI1IN   NOT =   ZERO )
               MOVE    8               TO  WRK-GMN-CUR
           WHEN  ( SPA-GMN-CHOSEI2IN   NOT =   ZERO )
               MOVE    9               TO  WRK-GMN-CUR
           END-EVALUATE
      *
           IF    ( SPA-GMN-SKYMONEYIN  >   CONST-KINGAKU-MAX )
                   MOVE    "0020"      TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD        =  SPACE )
               MOVE    ZERO            TO  WRK-SKYMONEY
               IF    ( SPA-S01-SYORINO =   CONST-PROC-RRKSYU )
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-SKYMONEY
                       -   SPA-NAI-SKYMONEY-MOTO
                       +   SPA-GMN-SKYMONEYIN
               ELSE
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-SKYMONEY
                       +   SPA-GMN-SKYMONEYIN
               END-IF
      *
               IF    ( WRK-SKYMONEY  > CONST-KINGAKU-MAX )
                       MOVE    "0021"      TO  SPA-ERRCD
                       MOVE    WRK-GMN-CUR TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-CHOSEI2IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理
      *****************************************************************
       4101-NYUKIN-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-NYUKIN
      *
           EVALUATE    SPA-S01-SYORINO
           WHEN    CONST-PROC-NYUKIN
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-NYUKIN-TTL
                   +   SPA-GMN-NYUKIN
           WHEN    CONST-PROC-RRKSYU
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-NYUKIN-TTL
                   -   SPA-NAI-NYUKIN-MOTO
                   +   SPA-GMN-NYUKIN
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-NYUKIN  <  ZERO )
                   MOVE    2       TO  SPA-GMN-CUR
                   MOVE    "0014"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  <  ZERO )
                   MOVE    2       TO  SPA-GMN-CUR
                   MOVE    "0008"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  >  CONST-KINGAKU-MAX )
                   MOVE    2       TO  SPA-GMN-CUR
                   MOVE    "0022"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "NYUKINGK2" )
                   IF    ( SPA-S01-SYORINO     =   CONST-PROC-NYUKIN )
                    AND  ( SPA-GMN-NYUKIN      =   ZERO )
                    AND  ( SPA-GMN-MISYU-ZAN   >   ZERO )
                       MOVE    SPA-GMN-MISYU-ZAN
                                   TO  SPA-GMN-NYUKIN
                       MOVE    SPA-GMN-NYUKIN  TO  WRK-KINGAKUZ
                       MOVE    WRK-KINGAKUZ    TO  SPA-GMN-NYUKINX
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理（入金取消、返金の履歴修正）
      *****************************************************************
       4102-NYUKIN-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-NYUKIN
      *
           EVALUATE    SPA-S01-SYORINO
           WHEN    CONST-PROC-TORIKESHI
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-NYUKIN-TTL
                   +   SPA-GMN-NYUKIN
           WHEN    CONST-PROC-RRKSYU
      *
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-NYUKIN-TTL
                   -   SPA-NAI-NYUKIN-MOTO
                   +   SPA-GMN-NYUKIN
           END-EVALUATE
      *
           IF    ( SPA-GMN-NYUKIN  >   ZERO )
               MOVE    2           TO  SPA-GMN-CUR
               MOVE    "0013"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  <  ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
                   MOVE    "0008"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  >   CONST-KINGAKU-MAX )
                   MOVE    10      TO  SPA-GMN-CUR
                   MOVE    "0022"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-S01-SYORINO     =   CONST-PROC-TORIKESHI )
               MOVE    ZERO            TO  WRK-NYUKIN
      *
               PERFORM VARYING IDX-LST FROM    1   BY  1
                       UNTIL ( IDX-LST >   SPA-GMN-SMEILIST-MAX )
      *
                   IF ( SPA-NAI-NYUKINYMD
                                       =   SPA-NAI-TSYORIYMD (IDX-LST))
                       COMPUTE WRK-NYUKIN
                           =   WRK-NYUKIN
                           +   SPA-GMN-TNYUKIN (IDX-LST)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "NYUKINGK2" )
                   IF    ( SPA-S01-SYORINO
                                       =   CONST-PROC-TORIKESHI )
                    AND  ( SPA-GMN-NYUKIN      =   ZERO )
                       IF   ( WRK-NYUKIN          >   ZERO )
                           COMPUTE SPA-GMN-NYUKIN
                               =   WRK-NYUKIN      *   -1
      *
                           MOVE    SPA-GMN-NYUKIN
                                           TO  WRK-KINGAKUZ
                           MOVE    WRK-KINGAKUZ
                                           TO  SPA-GMN-NYUKINX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4102-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金日チェック処理
      *****************************************************************
       4101-NYUKINYMD-CHK-SEC          SECTION.
      *
           MOVE    SPACE                   TO  SPA-NAI-NYUKINYMD
      *
           IF    ( SPA-GMN-NYUKINYMD   =   SPACE )
               MOVE    SPA-SYSYMD          TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUKINYMD
               MOVE    WRK-SYMD            TO  SPA-NAI-NYUKINYMD
           ELSE
               MOVE    SPA-GMN-NYUKINYMD   TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR      =   ZERO )
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-NYUKINYMD
                   MOVE    WRK-SYMD        TO  SPA-NAI-NYUKINYMD
               ELSE
                   MOVE    3               TO  SPA-GMN-CUR
                   MOVE    "0010"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101-NYUKINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金方法チェック処理
      *****************************************************************
       4101-NYKN-HOHO-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-NYKN-HOHO-G     =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-NYKN-HOHO
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYKN-HOHOLST-CHK-SEC
               IF    (  WRK-CMB-ITM    =   SPACE )
                   MOVE    4           TO  SPA-GMN-CUR
                   MOVE    "0011"      TO  SPA-ERRCD
               ELSE
                   MOVE WRK-CMB-ITM    TO  SPA-GMN-NYKN-HOHO-G
               END-IF
           END-IF
      *
           .
       4101-NYKN-HOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行コンボチェック処理
      *****************************************************************
       4101-SKYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-SKY
      *
           IF    ( SPA-GMN-SKYPRT-G    =   SPACE )
               MOVE    SPA-GMN-SKYPRTLST (IDX-SKY)
                                       TO  SPA-GMN-SKYPRT-G
           ELSE
               MOVE    SPA-GMN-SKYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SKYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SKYPRTLST (IDX-SKY)
                                       TO  SPA-GMN-SKYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-SKYPRT-G
               END-IF
           END-IF
      *
           .
       4101-SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行コンボチェック処理
      *****************************************************************
       4101-SRYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-SRY
      *
           IF    ( SPA-GMN-SRYPRT-G    =   SPACE )
               MOVE    SPA-GMN-SRYPRTLST (IDX-SRY)
                                       TO  SPA-GMN-SRYPRT-G
           ELSE
               MOVE    SPA-GMN-SRYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SRYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SRYPRTLST (IDX-SRY)
                                       TO  SPA-GMN-SRYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYPRT-G
               END-IF
           END-IF
      *
           .
       4101-SRYPRT-CHK-EXT.
           EXIT.
      *************************************************************
      *    印刷処理
      *************************************************************
       410-PRINT-SEC                   SECTION.
      *
           MOVE    ZERO            TO  WRK-PRINT-DENPEDANUM
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-GMN-SELNUM  >=  1 )
            AND  ( SPA-GMN-SELNUM  <=  SPA-GMN-SMEILIST-MAX )
                   MOVE    SPA-GMN-TDENPEDANUM(SPA-GMN-SELNUM)
                                   TO  WRK-PRINT-DENPEDANUM
           ELSE
               IF    ( WRK-SKYPRT  =   "1" )
                   IF    ( SPA-GMN-SELNUM  =   ZERO )
                       MOVE    "0006"  TO  SPA-ERRCD
                       MOVE    1       TO  SPA-GMN-CUR
                   ELSE
                       MOVE    "0005"  TO  SPA-ERRCD
                       MOVE    1       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PRINT-SEC
           END-IF
      *
           .
       410-PRINT-EXT.
           EXIT.
      *************************************************************
      *    印刷処理
      *************************************************************
       4101-PRINT-SEC                  SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( WRK-SKYPRT      =   "1" )
             OR  ( WRK-SRYPRT      =   "1" )
               IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                OR   ( SPA-PRTCONF         =   "1" )
                   PERFORM 800-CLIENT-PRT-SEC
               END-IF
           END-IF
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                      SSUB03-AREA
           MOVE    2                   TO  SSUB03-REQNO
           MOVE    WRK-PRINT-DENPEDANUM TO SSUB03-DENPEDANUM
           MOVE    WRK-SKYPRT          TO  SSUB03-SKYPRT
           MOVE    WRK-SRYPRT          TO  SSUB03-SRYPRT
           CALL    "ORCGSSUB03"        USING
                                       WRK-SPA-AREA
                                       SPA-S01KYOUTU
                                       SSUB03-AREA
      *
           PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
           .
       4101-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行コンボチェック処理
      *****************************************************************
       4111-SRYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-SRY
      *
           IF    ( SPA-GMN-SRYPRT-G    =   SPACE )
               MOVE    SPA-GMN-SRYPRTLST (IDX-SRY)
                                       TO  SPA-GMN-SRYPRT-G
           ELSE
               MOVE    SPA-GMN-SRYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SRYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SRYPRTLST (IDX-SRY)
                                       TO  SPA-GMN-SRYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYPRT-G
               END-IF
           END-IF
      *
           .
       4111-SRYPRT-CHK-EXT.
           EXIT.
      *************************************************************
      *    登録処理
      *************************************************************
       410-TOROKU-SEC                  SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4901-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  WRK-NYUKIN
                                       WRK-SKYMONEY
               IF    ( SPA-S01-SYORINO
                                   =   CONST-PROC-RRKSYU )
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-SKYMONEY
                       -   SPA-NAI-SKYMONEY-MOTO
                       +   SPA-GMN-SKYMONEYIN
      *
                   COMPUTE WRK-NYUKIN
                       =   SPA-GMN-NYUKIN-TTL
                       -   SPA-NAI-NYUKIN-MOTO
                       +   SPA-GMN-NYUKIN
               ELSE
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-SKYMONEY
                       +   SPA-GMN-SKYMONEYIN
      *
                   COMPUTE WRK-NYUKIN
                       =   SPA-GMN-NYUKIN-TTL
                       +   SPA-GMN-NYUKIN
               END-IF
      *
               IF    ( SPA-NAI-PROC-FLG    =   4 )
                       PERFORM 4101-TOROKU-SEC
               ELSE
                   IF    ( WRK-SKYMONEY    <   WRK-NYUKIN )
                       MOVE    "2001"  TO  SPA-SIDCD
                   ELSE
                       PERFORM 4101-TOROKU-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       410-TOROKU-EXT.
           EXIT.
      *
      *************************************************************
      *    登録処理
      *************************************************************
       4101-TOROKU-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    SPA-S01-SYORINO
               WHEN    CONST-PROC-NYUKIN
                   PERFORM   4902-NYUKIN-KOUSIN-SEC
               WHEN    CONST-PROC-TORIKESHI
                   PERFORM   4902-TORIKESHI-KOUSIN-SEC
               WHEN    CONST-PROC-RRKSYU
                   PERFORM   4902-NYUKIN-RRKSYU-SEC
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               MOVE    SMEI-DENPEDANUM TO  WRK-PRINT-DENPEDANUM
               MOVE    SPA-GMN-SKYPRT  TO  WRK-SKYPRT
               MOVE    SPA-GMN-SRYPRT  TO  WRK-SRYPRT
      *
               PERFORM 4101-PRINT-SEC
      *
               INITIALIZE                  SPA-GMN-SKYPRT-G
                                           SPA-GMN-SRYPRT-G
               PERFORM 4200-SPA-SET-SEC
      *
               EVALUATE    SPA-S01-SYORINO
               WHEN    CONST-PROC-NYUKIN
               WHEN    CONST-PROC-TORIKESHI
                   MOVE    2       TO  FLG-SMEILIST1-ROW
                   MOVE    2       TO  SPA-GMN-CUR
               WHEN    CONST-PROC-RRKSYU
                   MOVE    1       TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           .
       4101-TOROKU-EXT.
           EXIT.
      *
      *************************************************************
      *    関連チェック
      *************************************************************
       4901-KANRENCHK-SEC              SECTION.
      *
           EVALUATE    SPA-S01-SYORINO
           WHEN    CONST-PROC-NYUKIN
               PERFORM 4901-NYUKIN-CHK-SEC
           WHEN    CONST-PROC-TORIKESHI
               PERFORM 4901-TORIKESHI-CHK-SEC
           WHEN    CONST-PROC-RRKSYU
               PERFORM 4901-RRKSYU-CHK-SEC
           END-EVALUATE
      *
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *************************************************************
      *    入金取消時関連チェック処理
      *************************************************************
       4901-TORIKESHI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-NYUKIN      =   ZERO )
               MOVE    2               TO  SPA-GMN-CUR
               MOVE    "0007"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    3           TO  SPA-GMN-CUR
                   MOVE    "0012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4901-TORIKESHI-CHK-EXT.
           EXIT.
      *************************************************************
      *    入金処理時関連チェック処理
      *************************************************************
       4901-NYUKIN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-NYUKIN      =   ZERO )
               MOVE    2               TO  SPA-GMN-CUR
               MOVE    "0007"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    3           TO  SPA-GMN-CUR
                   MOVE    "0012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYKN-HOHO-G =   SPACE )
                   MOVE    4           TO  SPA-GMN-CUR
                   MOVE    "0004"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4901-NYUKIN-CHK-EXT.
           EXIT.
      *************************************************************
      *    履歴修正処理時関連チェック処理
      *************************************************************
       4901-RRKSYU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    "0006"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-PROC-FLG    =   1  OR  2 )
      *
                   COMPUTE WRK-SKYMONEY
                      =   SPA-GMN-SKYMONEY
                      -   SPA-NAI-SKYMONEY-MOTO
                      +   SPA-GMN-SKYMONEYIN
      *
                   IF    ( WRK-SKYMONEY    <   ZERO )
                       MOVE    "0016"      TO  SPA-ERRCD
                       IF    ( SPA-GMN-CHOSEI2IN   NOT =   ZERO )
                           MOVE    9       TO  SPA-GMN-CUR
                       ELSE
                           MOVE    8       TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    3           TO  SPA-GMN-CUR
                   MOVE    "0012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-NYUKIN  >   ZERO )
                AND  ( SPA-GMN-NYKN-HOHO-G =   SPACE )
                       MOVE    4           TO  SPA-GMN-CUR
                       MOVE    "0004"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4901-RRKSYU-CHK-EXT.
           EXIT.
      *************************************************************
      *    入金編集処理
      *************************************************************
       4902-NYUKIN-KOUSIN-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-INS-SEC
           END-IF
      *
           .
       4902-NYUKIN-KOUSIN-EXT.
           EXIT.
      *************************************************************
      *    履歴修正編集処理
      *************************************************************
       4902-NYUKIN-RRKSYU-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF  ( SPA-S01-NYUGAIKBN   =   "2" )
                AND( SPA-NAI-PROC-FLG    =   1  OR  2 )
                   PERFORM 49021-SYUTTL-UPD-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-UPD-SEC
           END-IF
      *
           .
       4902-NYUKIN-RRKSYU-EXT.
           EXIT.
      *************************************************************
      *    入金取消編集処理
      *************************************************************
       4902-TORIKESHI-KOUSIN-SEC       SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-INS-SEC
           END-IF
      *
           .
       4902-TORIKESHI-KOUSIN-EXT.
           EXIT.
      *************************************************************
      *    収納マスタ更新処理
      *************************************************************
       49021-SYUNOU-UPD-SEC            SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU      NOT =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               EVALUATE    SPA-S01-SYORINO
               WHEN    CONST-PROC-NYUKIN
               WHEN    CONST-PROC-TORIKESHI
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       +   SPA-GMN-NYUKIN
               WHEN    CONST-PROC-RRKSYU
      *
                   IF    ( SPA-NAI-PROC-FLG    =   1  OR  2 )
      *
                       COMPUTE SYU-SKYMONEY
                           =   SYU-SKYMONEY
                           -   SPA-NAI-SKYMONEY-MOTO
                           +   SPA-GMN-SKYMONEYIN
      *
                       COMPUTE SYU-CHOSEI1
                           =   SYU-CHOSEI1
                           -   SPA-NAI-CHOSEI1IN-MOTO
                           +   SPA-GMN-CHOSEI1IN
      *
                       COMPUTE SYU-CHOSEI2
                           =   SYU-CHOSEI2
                           -   SPA-NAI-CHOSEI2IN-MOTO
                           +   SPA-GMN-CHOSEI2IN
      *
                       COMPUTE SYU-CHOSEI
                           =   SYU-CHOSEI1
                           +   SYU-CHOSEI2
                   END-IF
      *
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       -   SPA-NAI-NYUKIN-MOTO
                       +   SPA-GMN-NYUKIN
               END-EVALUATE
      *
               COMPUTE SYU-NYUKIN-KAISU    =   SYU-NYUKIN-KAISU    +   1
      *
               IF    ( SPA-NAI-HEAD-DENPJTIKBN =   "3" )
                   CONTINUE
               ELSE
                   IF    ( SYU-SKYMONEY    =   ZERO )
                       MOVE    SPACE       TO  SYU-DENPJTIKBN
                   ELSE
                       IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                           MOVE    "2"     TO  SYU-DENPJTIKBN
                       ELSE
                           MOVE    "1"     TO  SYU-DENPJTIKBN
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    SPA-OPID            TO  SYU-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               INITIALIZE                  SS002-AREA
               MOVE    "2"                 TO  SS002-UPDKBN
               MOVE    SPA-NAI-SYURRKNUM   TO  SS002-IN-SYURRKNUM
               MOVE    SPA-NAI-SYUEDANUM   TO  SS002-IN-SYUEDANUM
               MOVE    SPA-NAI-SYURRKUPDKBN TO SS002-IN-SYURRKUPDKBN
               CALL    "ORCSS002"  USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
               IF    ( SS002-RC    NOT =   ZERO )
                   GO  TO  49021-SYUNOU-UPD-EXT
               END-IF
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    "1002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-SYUNOU-UPD-EXT.
           EXIT.
      *************************************************************
      *    収納合計マスタ更新処理
      *************************************************************
       49021-SYUTTL-UPD-SEC            SECTION.
      *
           PERFORM 900-SYUTOTAL-KEY-SEL-SEC
           IF    ( FLG-SYUTTL      =   ZERO )
      *
               COMPUTE SYUTTL-SKYMONEY
                   =   SYUTTL-SKYMONEY
                   -   SPA-NAI-SKYMONEY-MOTO
                   +   SPA-GMN-SKYMONEYIN
      *
               COMPUTE SYUTTL-CHOSEI1
                   =   SYUTTL-CHOSEI1
                   -   SPA-NAI-CHOSEI1IN-MOTO
                   +   SPA-GMN-CHOSEI1IN
      *
               COMPUTE SYUTTL-CHOSEI2
                   =   SYUTTL-CHOSEI2
                   -   SPA-NAI-CHOSEI2IN-MOTO
                   +   SPA-GMN-CHOSEI2IN
      *
               COMPUTE SYUTTL-CHOSEI
                   =   SYUTTL-CHOSEI1
                   +   SYUTTL-CHOSEI2
      *
               MOVE    SPA-OPID            TO  SYUTTL-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
      *
               MOVE    SYUTOTAL-REC        TO  MCPDATA-REC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    "1007"          TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
       49021-SYUTTL-UPD-EXT.
           EXIT.
      *************************************************************
      *    収納明細作成処理
      *************************************************************
       49021-SYUMEI-INS-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SMEI-PTID
           MOVE    SPA-S01-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      NOT =   ZERO )
            OR   ( SMEI-DENPEDANUM    >=   CONST-SMEILIST-MAX )
               MOVE    "1003"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               COMPUTE WRK-DENPEDANUM  =   SMEI-DENPEDANUM +   1
      *
               INITIALIZE                  SYUMEI-REC
      *
               MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
               MOVE    SPA-S01-NYUGAIKBN   TO  SMEI-NYUGAIKBN
               MOVE    SPA-S01-PTID        TO  SMEI-PTID
      *
      *        伝票番号
               MOVE    SPA-S01-DENPNUM     TO  SMEI-DENPNUM
      *
      *        伝票番号枝番
               MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *
      *        入金連番
               MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *
      *        診療科
               MOVE    SPA-NAI-SRYKA       TO  SMEI-SRYKA
      *
      *        伝票状態区分
               MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *
      *        請求発行日（発行日の西暦）
               MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *
      *        請求再発行日
               MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *
      *        領収書発行日
               MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *
      *        領収書再発行日
               MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *
      *        請求金額
               MOVE    ZERO                TO  SMEI-SKYMONEY
      *
      *        入返金区分
               IF    ( SPA-S01-SYORINO     =   CONST-PROC-NYUKIN )
                   MOVE    "1"             TO  SMEI-NYUHEN-KBN
               ELSE
                   MOVE    "2"             TO  SMEI-NYUHEN-KBN
               END-IF
      *
      *        請求金額
               IF    ( SPA-NAI-PROC-FLG    = 1 OR 2 )
                   MOVE    SPA-GMN-SKYMONEYIN
                                           TO  SMEI-SKYMONEY
               ELSE
                   MOVE    ZERO            TO  SMEI-SKYMONEY
               END-IF
      *
      *        入返金額
               MOVE    SPA-GMN-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *
      *        入返金日
               IF    ( SPA-NAI-NYUKINYMD   =   SPACE )
                   MOVE    SPA-SYSYMD      TO  SMEI-NYUHEN-YMD
               ELSE
                   MOVE    SPA-NAI-NYUKINYMD
                                           TO  SMEI-NYUHEN-YMD
               END-IF
      *
      *        入金方法
               IF    ( SPA-S01-SYORINO     =   CONST-PROC-NYUKIN )
                   MOVE    SPA-GMN-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
               ELSE
                   IF    ( SPA-GMN-NYKN-HOHO   =   SPACE )
                       MOVE    "00"        TO  SMEI-NYUKIN-HOHO
                   ELSE
                       MOVE    SPA-GMN-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
                   END-IF
               END-IF
      *
      *        状態区分
               EVALUATE    SPA-S01-SYORINO
               WHEN    CONST-PROC-NYUKIN
                   IF    ( SPA-GMN-NYUKIN  >=  ZERO )
                       MOVE    "2"         TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "4"         TO  SMEI-JOUTAIKBN
                   END-IF
               WHEN    CONST-PROC-TORIKESHI
                       MOVE    "6"         TO  SMEI-JOUTAIKBN
               WHEN    OTHER
                   MOVE    "8"             TO  SMEI-JOUTAIKBN
               END-EVALUATE
      *
               MOVE    SPA-OPID            TO  SMEI-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                               SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                               SMEI-UPHMS
      *
               MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
               MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
               MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
               PERFORM 800-DAILYKEY-SEC
               MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "1004"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-SYUMEI-INS-EXT.
           EXIT.
      *************************************************************
      *    収納明細更新処理
      *************************************************************
       49021-SYUMEI-UPD-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SMEI-PTID
           MOVE    SPA-S01-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SPA-NAI-DENPEDANUM  TO  SMEI-DENPEDANUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =    ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      NOT =   ZERO )
               MOVE    "1005"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
               MOVE    SMEI-NYUHEN-YMD     TO  WRK-OLD-NYUHEN-YMD
      *
      *        請求金額
               IF    ( SPA-NAI-PROC-FLG    = 1 OR 2 )
                   MOVE    SPA-GMN-SKYMONEYIN
                                           TO  SMEI-SKYMONEY
               ELSE
                   MOVE    ZERO            TO  SMEI-SKYMONEY
               END-IF
      *
      *        入返金額
               MOVE    SPA-GMN-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *
      *        入返金日
               IF    ( SPA-NAI-NYUKINYMD   =   ZERO )
                   MOVE     SPA-SYSYMD     TO  SMEI-NYUHEN-YMD
               ELSE
                   MOVE     SPA-NAI-NYUKINYMD
                                           TO  SMEI-NYUHEN-YMD
               END-IF
      *
      *        入金方法
               IF    ( SPA-GMN-NYUKIN      >   ZERO )
                OR   ( SMEI-DENPEDANUM     =   1 )
                   MOVE  SPA-GMN-NYKN-HOHO TO  SMEI-NYUKIN-HOHO
               ELSE
                   IF    ( SMEI-JOUTAIKBN  =   "6" )
                    AND  ( SPA-GMN-NYUKIN  <   ZERO )
                       IF    ( SPA-GMN-NYKN-HOHO   =   SPACE )
                           MOVE    "00"    TO  SMEI-NYUKIN-HOHO
                       ELSE
                           MOVE    SPA-GMN-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
                       END-IF
                   ELSE
                           MOVE    "00"    TO  SMEI-NYUKIN-HOHO
                   END-IF
               END-IF
      *
      *        伝票状態区分
               EVALUATE    SPA-NAI-PROC-FLG
               WHEN    1
                   IF    ( SPA-GMN-SKYMONEYIN   <=   ZERO )
                       MOVE    SPACE   TO  SMEI-JOUTAIKBN
                   ELSE
                       IF    ( SPA-S01-NYUGAIKBN
                                       =   "1" )
                           MOVE    "9" TO  SMEI-JOUTAIKBN
                       ELSE
                           MOVE    "1" TO  SMEI-JOUTAIKBN
                       END-IF
                   END-IF
               WHEN    2
               WHEN    4
                   CONTINUE
               WHEN    OTHER
                   IF    ( SPA-GMN-NYUKIN     >=   ZERO )
                       MOVE    "2"     TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "4"     TO  SMEI-JOUTAIKBN
                   END-IF
               END-EVALUATE
      *
               MOVE    SPA-OPID            TO  SMEI-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
               IF    ( SMEI-NYUHEN-YMD     NOT =   WRK-OLD-NYUHEN-YMD )
                   MOVE    SMEI-UPYMD          TO  SDAILYKEY-CREYMD
                   MOVE    SMEI-UPHMS          TO  SDAILYKEY-CREHMS
                   MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
                   PERFORM 800-DAILYKEY-SEC
                   MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
               END-IF
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-SYUMEI-UPD-EXT.
           EXIT.
      **************************************************************
      *    処理区分変更処理
      **************************************************************
       430-SYORIKBN-SEC                SECTION.
      *
           IF    ( SPA-NAI-HEAD-DENPJTIKBN =   "3" )
      *
               EVALUATE SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-RRKSYU
               MOVE    CONST-PROC-TORIKESHI TO SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-TORIKESHI 
               MOVE    CONST-PROC-SYOKAI   TO  SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-SYOKAI
               MOVE    CONST-PROC-RRKSYU   TO  SPA-S01-SYORINO
      *
               WHEN    OTHER
               MOVE    CONST-PROC-SYOKAI   TO  SPA-S01-SYORINO
               END-EVALUATE
           ELSE
      *
               EVALUATE SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-NYUKIN
               MOVE    CONST-PROC-RRKSYU   TO  SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-RRKSYU
               MOVE    CONST-PROC-TORIKESHI TO SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-TORIKESHI 
               MOVE    CONST-PROC-SYOKAI   TO  SPA-S01-SYORINO
      *
               WHEN    CONST-PROC-SYOKAI
               MOVE    CONST-PROC-NYUKIN   TO  SPA-S01-SYORINO
      *
               WHEN    OTHER
               MOVE    CONST-PROC-SYOKAI   TO  SPA-S01-SYORINO
               END-EVALUATE
           END-IF
      *
           PERFORM 4200-SPA-SET-SEC
      *
           MOVE    1               TO  FLG-SMEILIST1-ROW
      *
           .
       430-SYORIKBN-EXT.
           EXIT.
      **************************************************************
      *    明細表示区分変更処理
      **************************************************************
       450-MEIDSPKBN-SEC                SECTION.
      *
           IF    ( SPA-NAI-MEIDSPCTRL  =   "1" )
             IF    ( SPA-NAI-MEIDSPKBN   =   " " )
               MOVE    "1"         TO  SPA-NAI-MEIDSPKBN
             ELSE
               MOVE    " "         TO  SPA-NAI-MEIDSPKBN
             END-IF
      *
             PERFORM 4200-SPA-SET-SEC
             MOVE    1               TO  FLG-SMEILIST1-ROW
           END-IF
      *
           .
       450-MEIDSPKBN-EXT.
           EXIT.
      **************************************************************
      *    予約登録へ
      **************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S03            TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "S03"              TO  SPA-MOTOPG
           MOVE    "S03"              TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "Y01"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      **************************************************************
      *    受付一覧へ
      **************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S03            TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S03"              TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "U01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    処理区分コンボチェック処理
      *****************************************************************
       4200-SYORIKBNLST-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB         >   SPA-GMN-SYORIKBN-MAX )
                OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
                IF   ( WRK-CMB-CD  =   SPA-GMN-SYORIKBNL (IDX-CMB) )
                    MOVE    SPA-GMN-SYORIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                END-IF
      *
           END-PERFORM
      *
           .
       4200-SYORIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金コンボチェック処理
      *****************************************************************
       4200-NYKN-HOHOLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-NYKN-HOHO-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-NYKN-HOHOL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-NYKN-HOHO-MAX )
               MOVE    SPA-GMN-NYKN-HOHOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               MOVE    SPA-NAI-NYKN-HOHOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM2
           END-IF
      *
           .
       4200-NYKN-HOHOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分コンボチェック処理
      *****************************************************************
       4200-SKYPRTLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-SKYPRT-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-SKYPRTL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-SKYPRT-MAX )
               MOVE    SPA-GMN-SKYPRTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKYPRTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行区分コンボチェック処理
      *****************************************************************
       4200-SRYPRTLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-SRYPRT-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-SRYPRTL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-SRYPRT-MAX )
               MOVE    SPA-GMN-SRYPRTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SRYPRTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡ編集処理
      *****************************************************************
       4200-SPA-SET-SEC                SECTION.
      *
           MOVE    SPA-NAI-MEIDSPKBN   TO  WRK-MEIDSPKBN
           MOVE    SPA-GMN-SKYPRT      TO  WRK-SKYPRTKBN
           MOVE    SPA-GMN-SRYPRT      TO  WRK-SRYPRTKBN
      *
           INITIALIZE                      SPA-S03
      *
           MOVE    WRK-MEIDSPKBN       TO  SPA-NAI-MEIDSPKBN
      *
           PERFORM 3101-SPA-SET-SEC
           MOVE    WRK-SKYPRTKBN       TO  SPA-GMN-SKYPRT
           PERFORM 4101-SKYPRT-CHK-SEC
           MOVE    WRK-SRYPRTKBN       TO  SPA-GMN-SRYPRT
           PERFORM 4101-SRYPRT-CHK-SEC
      *
           .
       4200-SPA-SET-EXT.
           EXIT.
      *************************************************************
      *    エラーメッセージ表示理
      *************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0003"
               MOVE    "入金合計額が請求額を超えています"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "入金方法を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "明細を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "入金額を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "入金合計額が０円を下回ります"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "入金データを選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "入金日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "入金方法入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "入金日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "マイナスの金額を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "入金額が０円を下回ります"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "請求金額が０円を下回ります"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "調整金１入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "調整金２入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "入金額入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "今回請求金額が上限を超えます"
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "請求金額が上限を超えます"
                                       TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "入金額が上限を超えます"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
           WHEN    "1002"
               MOVE    "収納の更新処理でエラーになりました"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
           WHEN    "1004"
           WHEN    "1005"
           WHEN    "1006"
               MOVE    "収納明細の更新処理でエラーになりました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SERR
           MOVE    SPA-ERRCD           TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  SERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "S03"              TO  SPA-MOTOPG
           MOVE    "SERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "SERR"              TO  MCP-WINDOW
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
       520-SIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SIDMSG
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2001"
               MOVE    "過入金状態になりますがよろしいですか"
                                        TO  WRK-SIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  SID1
           INITIALIZE                       SID1
           MOVE    SPA-SIDCD            TO  SID1-ID1CODE
           MOVE    WRK-SIDMSG           TO  SID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S03"               TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
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
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
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
      *    金額数値変換処理
      *****************************************************************
       800-KINGAKU-HEN-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-ORCSNUMERR
      *
           INITIALIZE                  WRK-KINGAKU9
                                       WRK-KINGAKUZ
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-KINGAKUX    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-SEISUU         >   7     )
            OR   ( SNUM-SYOKBN         =   "1"   )
               MOVE    1               TO  FLG-ORCSNUMERR
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKU9
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKUZ
           END-IF
      *
           .
       800-KINGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    SPA-S01-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( FLG-PTINF       =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-FETCH-SEC          SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    ZERO        TO  FLG-SYSKANRI
           ELSE
               INITIALIZE              MCPDATA-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ読込
      *****************************************************************
       900-SYUNOU-FETCH-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              MCPDATA-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *************************************************************
      *    収納マスター検索処理
      *************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           MOVE    SPA-S01-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納合計マスター検索処理
      *************************************************************
       900-SYUTOTAL-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUTTL
           INITIALIZE                       SYUTOTAL-REC
           MOVE    SPA-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SYUTTL-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYUTTL-PTID
           MOVE    SPA-NAI-GRP-DENPNUM TO  SYUTTL-DENPNUM
           MOVE    SYUTOTAL-REC        TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUTOTAL-REC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE                  SYUTOTAL-REC
           END-IF
      *
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUTOTAL-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SMEI-SYUEDANUM      TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           .
       900-SYURRK-KEY3-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴FETCH処理
      *************************************************************
       900-SYURRK-KEY3-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           .
       900-SYURRK-KEY3-FET-EXT.
           EXIT.
      *************************************************************
      *    トランザクションキャンセル処理
      *************************************************************
       910-DBABORT-SEC               SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "abort"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
      *
           .
       910-DBABORT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ追加処理
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
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
