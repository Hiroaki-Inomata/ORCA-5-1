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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGW23.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 定期請求情報（Ｗ２３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.08.01    ひさなが 　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02/11/04  暫定的に定期請求を月末のみ
      *                                     選択可とする
      *  01.00.02    NACL-太田    03/02/12  期別の指定を可能にする
      *                           03/02/25  再請求区分を削除
      *  01.00.03    NACL-太田    03/10/14  入金区分を追加
      *  01.00.04    NACL-太田    03/11/18  更新日付編集対応
      *  01.00.05    NACL-太田    04/04/12  再処理の取り扱いを追加
      *  02.07.00    NACL-太田    06/01/16  削除区分追加
      *  03.05.00    NACL-太田    07.05.18  グループ診療対応
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
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
       01  SPA-W23.
           03  SPA-W23-AREA.
               05  SPA-GMN-PAGE               PIC 9(02).
               05  SPA-GMN-MAX                PIC 9(03).
               05  SPA-GMN-CUR                PIC 9(03).
               05  SPA-GMN-KBN                PIC X(02).
               05  SPA-GMN-RUN                PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-YUKOSTYMD      PIC X(9).
                   07  SPA-GMN-YUKOEDYMD      PIC X(9).
                   07  SPA-GMN-SEIKYU-VAL.
                       09  SPA-GMN-SEIKYU01   PIC X(03).
                       09  SPA-GMN-SEIKYU02   PIC X(03).
                       09  SPA-GMN-SEIKYU03   PIC X(03).
                   07  SPA-GMN-SEIKYU-R    REDEFINES
                       SPA-GMN-SEIKYU-VAL.
                       09  SPA-GMN-SEIKYU-OCC OCCURS  3.
                           11  SPA-GMN-SEIKYU PIC   X(03).
                   07  SPA-GMN-NYUKINKBN-G.
                       09  SPA-GMN-NYUKINKBN  PIC   X(01).
                       09  SPA-GMN-NYUKINKBNFL PIC   X(01).
                       09  SPA-GMN-NYUKINKBNMEI
                                              PIC   X(58).
                   07  SPA-GMN-NYUKINPRTKBN-G.
                       09  SPA-GMN-NYUKINPRTKBN
                                              PIC   X(01).
                       09  SPA-GMN-NYUKINPRTKBNFL
                                              PIC   X(01).
                       09  SPA-GMN-NYUKINPRTKBNMEI
                                              PIC   X(28).
                   07  SPA-GMN-SAISYORIKBN-G.
                       09  SPA-GMN-SAISYORIKBN  PIC   X(01).
                       09  SPA-GMN-SAISYORIKBNFL
                                              PIC   X(01).
                       09  SPA-GMN-SAISYORIKBNMEI
                                              PIC   X(58).
                   07  SPA-GMN-CHGWINDOWKBN-G.
                       09  SPA-GMN-CHGWINDOWKBN     PIC   X(01).
                       09  SPA-GMN-CHGWINDOWKBNFL   PIC   X(01).
                       09  SPA-GMN-CHGWINDOWKBNMEI
                                              PIC   X(18).
                   07  SPA-GMN-DELKBN-G.
                       09  SPA-GMN-DELKBN     PIC   X(01).
                       09  SPA-GMN-DELKBNFL   PIC   X(01).
                       09  SPA-GMN-DELKBNMEI
                                              PIC   X(18).
                   07  SPA-GMN-SKYSUMKBN-G.
                       09  SPA-GMN-SKYSUMKBN  PIC   X(01).
                       09  SPA-GMN-SKYSUMKBNFL PIC   X(01).
                       09  SPA-GMN-SKYSUMKBNMEI
                                              PIC   X(28).
                   07  SPA-GMN-SKYPRTKBN-G.
                       09  SPA-GMN-SKYPRTKBN       PIC   X(01).
                       09  SPA-GMN-SKYPRTKBNFL     PIC   X(01).
                       09  SPA-GMN-SKYPRTKBNMEI    PIC   X(38).
                   07  SPA-GMN-SRYPRTKBN-G.
                       09  SPA-GMN-SRYPRTKBN       PIC   X(01).
                       09  SPA-GMN-SRYPRTKBNFL     PIC   X(01).
                       09  SPA-GMN-SRYPRTKBNMEI    PIC   X(38).
                   07  SPA-GMN-PTINFREFKBN-G.
                       09  SPA-GMN-PTINFREFKBN       PIC   X(01).
                       09  SPA-GMN-PTINFREFKBNFL     PIC   X(01).
                       09  SPA-GMN-PTINFREFKBNMEI    PIC   X(28).
                   07  SPA-GMN-PRTJYUN-G.
                       09  SPA-GMN-PRTJYUN         PIC   X(02).
                       09  SPA-GMN-PRTJYUNFL       PIC   X(01).
                       09  SPA-GMN-PRTJYUNMEI      PIC   X(22).
                   07  SPA-GMN-TAIINGRPKBN-G.
                       09  SPA-GMN-TAIINGRPKBN     PIC   X(01).
                       09  SPA-GMN-TAIINGRPKBNFL   PIC   X(01).
                       09  SPA-GMN-TAIINGRPKBNMEI  PIC   X(28).
                   07  SPA-GMN-NYUKINKBNLST-G.
                       09  SPA-GMN-NYUKINKBN-MAX   PIC   9(03).
                       09  SPA-GMN-NYUKINKBNLST    OCCURS  2.
                           11  SPA-GMN-NYUKINKBNL  PIC   X(01).
                           11  SPA-GMN-NYUKINKBNFLL PIC   X(01).
                           11  SPA-GMN-NYUKINKBNMEIL
                                                   PIC   X(58).
                   07  SPA-GMN-NYUKINPRTKBNLST-G.
                       09  SPA-GMN-NYUKINPRTKBN-MAX    PIC   9(03).
                       09  SPA-GMN-NYUKINPRTKBNLST     OCCURS  2.
                           11  SPA-GMN-NYUKINPRTKBNL   PIC   X(01).
                           11  SPA-GMN-NYUKINPRTKBNFLL PIC   X(01).
                           11  SPA-GMN-NYUKINPRTKBNMEIL
                                                       PIC   X(28).
                   07  SPA-GMN-SAISYORIKBNLST-G.
                       09  SPA-GMN-SAISYORIKBN-MAX   PIC   9(03).
                       09  SPA-GMN-SAISYORIKBNLST    OCCURS  2.
                           11  SPA-GMN-SAISYORIKBNL  PIC   X(01).
                           11  SPA-GMN-SAISYORIKBNFLL PIC   X(01).
                           11  SPA-GMN-SAISYORIKBNMEIL
                                                     PIC   X(58).
                   07  SPA-GMN-CHGWINDOWKBNLST-G.
                       09  SPA-GMN-CHGWINDOWKBN-MAX   PIC   9(03).
                       09  SPA-GMN-CHGWINDOWKBNLST    OCCURS  2.
                           11  SPA-GMN-CHGWINDOWKBNL  PIC   X(01).
                           11  SPA-GMN-CHGWINDOWKBNFLL PIC   X(01).
                           11  SPA-GMN-CHGWINDOWKBNMEIL
                                                     PIC   X(18).
                   07  SPA-GMN-DELKBNLST-G.
                       09  SPA-GMN-DELKBN-MAX   PIC   9(03).
                       09  SPA-GMN-DELKBNLST    OCCURS  2.
                           11  SPA-GMN-DELKBNL  PIC   X(01).
                           11  SPA-GMN-DELKBNFLL PIC   X(01).
                           11  SPA-GMN-DELKBNMEIL
                                                     PIC   X(18).
                   07  SPA-GMN-SKYSUMKBNLST-G.
                       09  SPA-GMN-SKYSUMKBN-MAX       PIC   9(03).
                       09  SPA-GMN-SKYSUMKBNLST        OCCURS  5.
                           11  SPA-GMN-SKYSUMKBNL      PIC   X(01).
                           11  SPA-GMN-SKYSUMKBNFLL    PIC   X(01).
                           11  SPA-GMN-SKYSUMKBNMEIL   PIC   X(28).
                   07  SPA-GMN-SKYPRTKBNLST-G.
                       09  SPA-GMN-SKYPRTKBN-MAX       PIC   9(03).
                       09  SPA-GMN-SKYPRTKBNLST        OCCURS  5.
                           11  SPA-GMN-SKYPRTKBNL      PIC   X(01).
                           11  SPA-GMN-SKYPRTKBNFLL    PIC   X(01).
                           11  SPA-GMN-SKYPRTKBNMEIL   PIC   X(38).
                   07  SPA-GMN-SRYPRTKBNLST-G.
                       09  SPA-GMN-SRYPRTKBN-MAX       PIC   9(03).
                       09  SPA-GMN-SRYPRTKBNLST        OCCURS  5.
                           11  SPA-GMN-SRYPRTKBNL      PIC   X(01).
                           11  SPA-GMN-SRYPRTKBNFLL    PIC   X(01).
                           11  SPA-GMN-SRYPRTKBNMEIL   PIC   X(38).
                   07  SPA-GMN-PTINFREFKBNLST-G.
                       09  SPA-GMN-PTINFREFKBN-MAX       PIC   9(03).
                       09  SPA-GMN-PTINFREFKBNLST        OCCURS  5.
                           11  SPA-GMN-PTINFREFKBNL      PIC   X(01).
                           11  SPA-GMN-PTINFREFKBNFLL    PIC   X(01).
                           11  SPA-GMN-PTINFREFKBNMEIL   PIC   X(28).
                   07  SPA-GMN-PRTJYUNLST-G.
                       09  SPA-GMN-PRTJYUN-MAX         PIC   9(03).
                       09  SPA-GMN-PRTJYUNLST          OCCURS  10.
                           11  SPA-GMN-PRTJYUNL        PIC   X(02).
                           11  SPA-GMN-PRTJYUNFLL      PIC   X(01).
                           11  SPA-GMN-PRTJYUNMEIL     PIC   X(22).
                   07  SPA-GMN-TAIINGRPKBNLST-G.
                       09  SPA-GMN-TAIINGRPKBN-MAX     PIC   9(03).
                       09  SPA-GMN-TAIINGRPKBNLST      OCCURS  5.
                           11  SPA-GMN-TAIINGRPKBNL    PIC   X(01).
                           11  SPA-GMN-TAIINGRPKBNFLL  PIC   X(01).
                           11  SPA-GMN-TAIINGRPKBNMEIL PIC   X(28).
                   07  SPA-GMN-REPOS-NAME              PIC   X(128).
                   07  SPA-GMN-USER-PG-G.
                       09  SPA-GMN-SKYPRT-NAME         PIC   X(20).
                       09  SPA-GMN-SRYPRT-NAME         PIC   X(20).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-CUSTOM-VISIBLE          PIC X(01).
                   07  SPA-NAI-CUSTOM-CAUTION          PIC X(01).
                   07  SPA-NAI-CUSTOM-ENABLED          PIC   X(01).
                   07  SPA-NAI-SEIKYU-VAL.
                       09  SPA-NAI-SEIKYU01   PIC 9(02).
                       09  SPA-NAI-SEIKYU02   PIC 9(02).
                       09  SPA-NAI-SEIKYU03   PIC 9(02).
                   07  SPA-NAI-SEIKYU-R    REDEFINES
                       SPA-NAI-SEIKYU-VAL.
                       09  SPA-NAI-SEIKYU-OCC OCCURS  3.
                           11  SPA-NAI-SEIKYU PIC   9(02).
      *
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-DAY             PIC 9(02).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-HANKAKU         PIC X(500).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDXA                PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERROR           PIC 9(01).
      *
      *****************************************************************
      *    ファイル連絡　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    定期請求管理マスタ
           COPY    "CPSK5010.INC".
           COPY    "CPSK3103.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU.
           MOVE    SPA-FREE        TO  SPA-W23.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK.
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-W23         TO  SPA-FREE.
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA.
      *    エラー画面より
           IF  SPA-MOTOPG   =   "WERR" 
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF.
      *
           MOVE    SPACE                TO  LINKAREA
      * 
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W23"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF  LINKAREA   NOT =   SPACE
               MOVE    LNK-KYOUTU              TO  SPA-KYOUTU
           END-IF.
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               IF      SPA-WIDCD            =   "1001"
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W23
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               INITIALIZE                          SBC-AREA
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
               IF    ( SBC-BILLINGUSER =   "1" )
                   MOVE    "1"         TO  SPA-NAI-CUSTOM-ENABLED
                   MOVE    "1"         TO  SPA-NAI-CUSTOM-VISIBLE
               END-IF
           END-IF
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-YUKOSTYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-YUKOEDYMD
      *
           MOVE    2                   TO  SPA-GMN-NYUKINKBN-MAX
           MOVE    "1"                 TO  SPA-GMN-NYUKINKBNL(1)
           MOVE    "未入金として処理する"
                                       TO  SPA-GMN-NYUKINKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-NYUKINKBNL(2)
           MOVE    "入金済として処理する"
                                       TO  SPA-GMN-NYUKINKBNMEIL(2)
           MOVE    SPA-GMN-NYUKINKBNLST(1)
                                       TO  SPA-GMN-NYUKINKBN-G
      *
           MOVE    2                   TO  SPA-GMN-NYUKINPRTKBN-MAX
           MOVE    "1"                 TO  SPA-GMN-NYUKINPRTKBNL(1)
           MOVE    "入金額を印字しない"
                                       TO  SPA-GMN-NYUKINPRTKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-NYUKINPRTKBNL(2)
           MOVE    "入金額を印字する"
                                       TO  SPA-GMN-NYUKINPRTKBNMEIL(2)
           MOVE    SPA-GMN-NYUKINPRTKBNLST(1)
                                       TO  SPA-GMN-NYUKINPRTKBN-G
      *
           MOVE    2                   TO  SPA-GMN-SAISYORIKBN-MAX
           MOVE    "1"                 TO  SPA-GMN-SAISYORIKBNL(1)
           MOVE    "請求取消後、再作成"
                                       TO  SPA-GMN-SAISYORIKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-SAISYORIKBNL(2)
           MOVE    "元の請求を訂正更新"
                                       TO  SPA-GMN-SAISYORIKBNMEIL(2)
           MOVE    SPA-GMN-SAISYORIKBNLST(1)
                                       TO  SPA-GMN-SAISYORIKBN-G
      *
           MOVE    2                   TO  SPA-GMN-CHGWINDOWKBN-MAX
           MOVE    "0"                 TO  SPA-GMN-CHGWINDOWKBNL(1)
           MOVE    "自動遷移しない"    TO  SPA-GMN-CHGWINDOWKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-CHGWINDOWKBNL(2)
           MOVE    "自動遷移する"      TO  SPA-GMN-CHGWINDOWKBNMEIL(2)
           MOVE    SPA-GMN-CHGWINDOWKBNLST(1)
                                       TO  SPA-GMN-CHGWINDOWKBN-G
      *
           MOVE    2                   TO  SPA-GMN-DELKBN-MAX
           MOVE    "1"                 TO  SPA-GMN-DELKBNL(1)
           MOVE    "使用しない"        TO  SPA-GMN-DELKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-DELKBNL(2)
           MOVE    "使用する"          TO  SPA-GMN-DELKBNMEIL(2)
           MOVE    SPA-GMN-DELKBNLST(1)
                                       TO  SPA-GMN-DELKBN-G
      *
           MOVE    2                   TO  SPA-GMN-SKYSUMKBN-MAX
           MOVE    "0"                 TO  SPA-GMN-SKYSUMKBNL(1)
           MOVE    "個別に発行する"        TO SPA-GMN-SKYSUMKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-SKYSUMKBNL(2)
           MOVE    "期別にまとめて発行する"
                                       TO SPA-GMN-SKYSUMKBNMEIL(2)
           MOVE    SPA-GMN-SKYSUMKBNLST(1)
                                       TO  SPA-GMN-SKYSUMKBN-G
      *
      *
           MOVE    4                 TO  SPA-GMN-SKYPRTKBN-MAX
           MOVE    "0"               TO  SPA-GMN-SKYPRTKBNL   (1)
           MOVE    "発行しない"
                                     TO  SPA-GMN-SKYPRTKBNMEIL(1)
           MOVE    "1"               TO  SPA-GMN-SKYPRTKBNL   (2)
           MOVE    "発行する"        TO  SPA-GMN-SKYPRTKBNMEIL(2)
      *
           MOVE    "2"               TO  SPA-GMN-SKYPRTKBNL   (3)
           MOVE    "発行する（請求あり）"
                                     TO  SPA-GMN-SKYPRTKBNMEIL(3)
      *
           MOVE    "A"               TO  SPA-GMN-SKYPRTKBNL   (4)
           MOVE    "医療機関情報−入院基本の設定に従う"
                                     TO  SPA-GMN-SKYPRTKBNMEIL(4)
           MOVE    SPA-GMN-SKYPRTKBNLST(4)
                                       TO  SPA-GMN-SKYPRTKBN-G
      *
           MOVE    4                 TO  SPA-GMN-SRYPRTKBN-MAX
           MOVE    "0"               TO  SPA-GMN-SRYPRTKBNL   (1)
           MOVE    "発行しない"
                                     TO  SPA-GMN-SRYPRTKBNMEIL(1)
           MOVE    "1"               TO  SPA-GMN-SRYPRTKBNL   (2)
           MOVE    "発行する"        TO  SPA-GMN-SRYPRTKBNMEIL(2)
      *
           MOVE    "2"               TO  SPA-GMN-SRYPRTKBNL   (3)
           MOVE    "発行する（請求あり）"
                                     TO  SPA-GMN-SRYPRTKBNMEIL(3)
      *
           MOVE    "A"               TO  SPA-GMN-SRYPRTKBNL   (4)
           MOVE    "医療機関情報−入院基本の設定に従う"
                                     TO  SPA-GMN-SRYPRTKBNMEIL(4)
           MOVE    SPA-GMN-SRYPRTKBNLST(4)
                                       TO  SPA-GMN-SRYPRTKBN-G
      *
           MOVE    2                 TO  SPA-GMN-PTINFREFKBN-MAX
           MOVE    "0"               TO  SPA-GMN-PTINFREFKBNL   (1)
           MOVE    "参照しない"
                                     TO  SPA-GMN-PTINFREFKBNMEIL(1)
           MOVE    "1"               TO  SPA-GMN-PTINFREFKBNL   (2)
           MOVE    "参照する"        TO  SPA-GMN-PTINFREFKBNMEIL(2)
           MOVE    SPA-GMN-PTINFREFKBNLST(2)
                                     TO  SPA-GMN-PTINFREFKBN-G
      *
           MOVE    6                     TO  SPA-GMN-PRTJYUN-MAX
           MOVE    "01"                  TO  SPA-GMN-PRTJYUNL    (1)
           MOVE    "病棟番号・カナ氏名順"
                                         TO  SPA-GMN-PRTJYUNMEIL (1)
           MOVE    "02"                  TO  SPA-GMN-PRTJYUNL    (2)
           MOVE    "病棟番号・患者番号順"
                                         TO  SPA-GMN-PRTJYUNMEIL (2)
           MOVE    "03"                  TO  SPA-GMN-PRTJYUNL    (3)
           MOVE    "カナ氏名順"          TO  SPA-GMN-PRTJYUNMEIL (3)
           MOVE    "04"                  TO  SPA-GMN-PRTJYUNL    (4)
           MOVE    "患者番号順"          TO  SPA-GMN-PRTJYUNMEIL (4)
           MOVE    "05"                  TO  SPA-GMN-PRTJYUNL    (5)
           MOVE    "病棟番号・病室番号順"
                                         TO  SPA-GMN-PRTJYUNMEIL (5)
           MOVE    "06"                  TO  SPA-GMN-PRTJYUNL    (6)
           MOVE    "病室番号順"          TO  SPA-GMN-PRTJYUNMEIL (6)
           MOVE    SPA-GMN-PRTJYUNLST(01)
                                         TO  SPA-GMN-PRTJYUN-G
      *
           MOVE    1                     TO  SPA-GMN-TAIINGRPKBN-MAX
           MOVE    "1"                   TO  SPA-GMN-TAIINGRPKBNL (1)
           MOVE    "退院患者は別に集計する"
                                         TO  SPA-GMN-TAIINGRPKBNMEIL(1)
      *
           INITIALIZE                      SYS-5010-REC.
           MOVE    "5010"              TO  SYS-5010-KANRICD.
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5010-STYUKYMD
                                           SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC.
           IF    ( FLG-SYSKANRI   =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5010-REC
           ELSE
               INITIALIZE                  SYS-5010-REC
           END-IF
      *
           INITIALIZE                      SYS-3103-REC.
           MOVE    "3103"              TO  SYS-3103-KANRICD.
           MOVE    "5010001"           TO  SYS-3103-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3103-STYUKYMD
                                           SYS-3103-EDYUKYMD
           MOVE    SYS-3103-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC.
           IF    ( FLG-SYSKANRI   =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3103-REC
           ELSE
               INITIALIZE                  SYS-3103-REC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               MOVE    SYS-5010-SEIKYU-DAY (IDX1)
                                       TO  SPA-NAI-SEIKYU (IDX1)
                                           SPA-GMN-SEIKYU (IDX1)
               IF    ( SYS-5010-SEIKYU-DAY (IDX1)
                                   =   ZERO )
                   MOVE    SPACE       TO  SPA-GMN-SEIKYU (IDX1)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUKINKBN-MAX )
      *
               IF    ( SYS-5010-NYUKINKBN
                                   =   SPA-GMN-NYUKINKBNL (IDX1))
                   MOVE    SPA-GMN-NYUKINKBNLST (IDX1)
                                       TO  SPA-GMN-NYUKINKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUKINPRTKBN-MAX )
      *
               IF    ( SYS-5010-NYUKINPRTKBN
                                   =   SPA-GMN-NYUKINPRTKBNL (IDX1))
                   MOVE    SPA-GMN-NYUKINPRTKBNLST (IDX1)
                                       TO  SPA-GMN-NYUKINPRTKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SAISYORIKBN-MAX )
      *
               IF    ( SYS-5010-SAISYORIKBN
                                   =   SPA-GMN-SAISYORIKBNL (IDX1))
                   MOVE    SPA-GMN-SAISYORIKBNLST (IDX1)
                                       TO  SPA-GMN-SAISYORIKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-CHGWINDOWKBN-MAX )
      *
               IF    ( SYS-5010-CHGWINDOWKBN
                                   =   SPA-GMN-CHGWINDOWKBNL (IDX1))
                   MOVE    SPA-GMN-CHGWINDOWKBNLST (IDX1)
                                       TO  SPA-GMN-CHGWINDOWKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-DELKBN-MAX )
      *
               IF    ( SYS-5010-DELKBN
                                   =   SPA-GMN-DELKBNL (IDX1))
                   MOVE    SPA-GMN-DELKBNLST (IDX1)
                                       TO  SPA-GMN-DELKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SKYSUMKBN-MAX )
      *
               IF    ( SYS-5010-SKYSUMKBN
                                   =   SPA-GMN-SKYSUMKBNL (IDX1))
                   MOVE    SPA-GMN-SKYSUMKBNLST (IDX1)
                                       TO  SPA-GMN-SKYSUMKBN-G
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-NYUKINKBN   =   "1" )
               MOVE    SPACE           TO  SPA-GMN-NYUKINPRTKBN-G
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SKYPRTKBN-MAX )
      *
               IF    ( SYS-5010-SKYPRTKBN
                                   =   SPA-GMN-SKYPRTKBNL (IDX1))
                   MOVE    SPA-GMN-SKYPRTKBNLST (IDX1)
                                       TO  SPA-GMN-SKYPRTKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SRYPRTKBN-MAX )
      *
               IF    ( SYS-5010-SRYPRTKBN
                                   =   SPA-GMN-SRYPRTKBNL (IDX1))
                   MOVE    SPA-GMN-SRYPRTKBNLST (IDX1)
                                       TO  SPA-GMN-SRYPRTKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-PTINFREFKBN-MAX )
      *
               IF    ( SYS-5010-PTINFREFKBN
                                   =   SPA-GMN-PTINFREFKBNL (IDX1))
                   MOVE    SPA-GMN-PTINFREFKBNLST (IDX1)
                                       TO  SPA-GMN-PTINFREFKBN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-PRTJYUN-MAX )
      *
               IF    ( SYS-5010-PRTJYUN
                                   =   SPA-GMN-PRTJYUNL (IDX1))
                   MOVE    SPA-GMN-PRTJYUNLST (IDX1)
                                       TO  SPA-GMN-PRTJYUN-G
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-TAIINGRPKBN-MAX )
      *
               IF    ( SYS-5010-TAIINGRPKBN
                                   =   SPA-GMN-TAIINGRPKBNL (IDX1))
                   MOVE    SPA-GMN-TAIINGRPKBNLST (IDX1)
                                       TO  SPA-GMN-TAIINGRPKBN-G
               END-IF
      *
           END-PERFORM
      *
            MOVE    SYS-3103-REPOS-NAME
                                       TO  SPA-GMN-REPOS-NAME
            MOVE    SYS-3103-PROGRAM-NAME (1)
                                       TO  SPA-GMN-SKYPRT-NAME
            MOVE    SYS-3103-PROGRAM-NAME (2)
                                       TO  SPA-GMN-SRYPRT-NAME
      *
           IF    ( SPA-MW          =   SPA-GINBEE )
               IF    ( SYS-3103-REPOS-NAME
                                   NOT =   SPACE )
                OR   ( SYS-3103-PROGRAM-NAME-G
                                   NOT =   SPACE )
                   MOVE    "1" TO  SPA-NAI-CUSTOM-VISIBLE
                   IF    ( SPA-NAI-CUSTOM-ENABLED
                                       =   SPACE )
                       MOVE  "1"  TO  SPA-NAI-CUSTOM-CAUTION
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 530-RIDSET2-SEC
               WHEN    OTHER
                   MOVE    1                   TO  SPA-GMN-CUR
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO                  410-INPUT-CHK-EXT
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
           MOVE    W23-SEIKYU01        TO  SPA-GMN-SEIKYU01
           MOVE    W23-SEIKYU02        TO  SPA-GMN-SEIKYU02
           MOVE    W23-SEIKYU03        TO  SPA-GMN-SEIKYU03
           MOVE    W23-NYUKINKBN       TO  SPA-GMN-NYUKINKBN-G
           MOVE    W23-NYUKINPRTKBN    TO  SPA-GMN-NYUKINPRTKBN-G
           MOVE    W23-SAISYORIKBN     TO  SPA-GMN-SAISYORIKBN-G
           MOVE    W23-CHGWINDOWKBN    TO  SPA-GMN-CHGWINDOWKBN-G
           MOVE    W23-DELKBN          TO  SPA-GMN-DELKBN-G
           MOVE    W23-SKYSUMKBN       TO  SPA-GMN-SKYSUMKBN-G
           MOVE    W23-SKYPRTKBN       TO  SPA-GMN-SKYPRTKBN-G
           MOVE    W23-SRYPRTKBN       TO  SPA-GMN-SRYPRTKBN-G
           MOVE    W23-PTINFREFKBN     TO  SPA-GMN-PTINFREFKBN-G
           MOVE    W23-PRTJYUN         TO  SPA-GMN-PRTJYUN-G
           MOVE    W23-TAIINGRPKBN     TO  SPA-GMN-TAIINGRPKBN-G
           MOVE    W23-REPOS-NAME      TO  SPA-GMN-REPOS-NAME
           MOVE    W23-SKYPRT-NAME     TO  SPA-GMN-SKYPRT-NAME
           MOVE    W23-SRYPRT-NAME     TO  SPA-GMN-SRYPRT-NAME
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
           IF    ( SPA-ERRCD       =   SPACE )
      *        請求日確認処理（１）
               PERFORM 4100-SKYYMD-CHK1-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        請求日確認処理（２）
               PERFORM 4100-SKYYMD-CHK2-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        請求日確認処理（３）
               PERFORM 4100-SKYYMD-CHK3-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        入金区分チェック
               PERFORM 4100-NYUKINKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        入金額印字区分
               PERFORM 4100-NYUKINPRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        再処理区分チェック
               PERFORM 4100-SAISYORIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        自動遷移区分チェック
               PERFORM 4100-CHGWINDOWKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        削除区分チェック
               PERFORM 4100-DELKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        請求書発行区分チェック
               PERFORM 4100-SKYSUMKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        請求書印刷区分チェック
               PERFORM 4100-SKYPRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        明細書印刷区分チェック
               PERFORM 4100-SRYPRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        患者情報個別参照区分チェック
               PERFORM 4100-PTINFREFKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        出力順区分チェック
               PERFORM 4100-PRTJYUN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        退院患者集計区分チェック
               PERFORM 4100-TAIINGRPKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-MW          =   SPA-GINBEE )
            AND  ( SPA-NAI-CUSTOM-VISIBLE
                                   =   "1" )
               IF    ( SPA-ERRCD   =   SPACE )
      *            リポジトリ名チェック
                   PERFORM 4100-REPOS-NAME-CHK-SEC
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
      *            請求書兼領収書プログラム名チェック
                   PERFORM 4100-SKYPRT-NAME-CHK-SEC
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
      *            診療費明細書プログラム名チェック
                   PERFORM 4100-SRYPRT-NAME-CHK-SEC
               END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求日 確認処理（１）
      *****************************************************************
       4100-SKYYMD-CHK1-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-SEIKYU01
      *
           IF    ( SPA-GMN-SEIKYU01        =   SPACE )
               GO  TO  4100-SKYYMD-CHK1-EXT
           END-IF
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    SPA-GMN-SEIKYU01    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF  ( SNUM-RC       NOT =   ZERO  )
            OR ( SNUM-MINKBN   NOT =   SPACE )
            OR ( SNUM-SYOKBN   NOT =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO TO   4100-SKYYMD-CHK1-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-DAY
               IF    (( WRK-DAY        >=  01 )
                AND   ( WRK-DAY        <=  27 ))
                OR    ( WRK-DAY         =  99 )
                   CONTINUE
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
                   GO TO   4100-SKYYMD-CHK1-EXT
               END-IF
           END-IF
      *
           MOVE    WRK-DAY             TO  SPA-NAI-SEIKYU01
                                           SPA-GMN-SEIKYU01
           .
      *
       4100-SKYYMD-CHK1-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求日 確認処理（２）
      *****************************************************************
       4100-SKYYMD-CHK2-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-SEIKYU02
      *
           IF    ( SPA-GMN-SEIKYU02        =   SPACE )
               GO  TO  4100-SKYYMD-CHK2-EXT
           END-IF
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    SPA-GMN-SEIKYU02    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF  ( SNUM-RC       NOT =   ZERO  )
            OR ( SNUM-MINKBN   NOT =   SPACE )
            OR ( SNUM-SYOKBN   NOT =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO TO   4100-SKYYMD-CHK2-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-DAY
               IF    (( WRK-DAY        >=  01 )
                AND   ( WRK-DAY        <=  27 ))
                OR    ( WRK-DAY         =  99 )
                   CONTINUE
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
                   GO TO   4100-SKYYMD-CHK2-EXT
               END-IF
           END-IF
      *
           MOVE    WRK-DAY             TO  SPA-NAI-SEIKYU02
                                           SPA-GMN-SEIKYU02
           .
      *
       4100-SKYYMD-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求日 確認処理（３）
      *****************************************************************
       4100-SKYYMD-CHK3-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-SEIKYU03
      *
           IF    ( SPA-GMN-SEIKYU03        =   SPACE )
               GO  TO  4100-SKYYMD-CHK3-EXT
           END-IF
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    SPA-GMN-SEIKYU03    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF  ( SNUM-RC       NOT =   ZERO  )
            OR ( SNUM-MINKBN   NOT =   SPACE )
            OR ( SNUM-SYOKBN   NOT =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO TO   4100-SKYYMD-CHK3-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-DAY
               IF    (( WRK-DAY        >=  01 )
                AND   ( WRK-DAY        <=  27 ))
                OR    ( WRK-DAY         =  99 )
                   CONTINUE
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-CUR
                   GO TO   4100-SKYYMD-CHK3-EXT
               END-IF
           END-IF
      *
           MOVE    WRK-DAY             TO  SPA-NAI-SEIKYU03
                                           SPA-GMN-SEIKYU03
           .
      *
       4100-SKYYMD-CHK3-EXT.
           EXIT.
      *****************************************************************
      *    入金区分チェック処理
      *****************************************************************
       4100-NYUKINKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-NYUKINKBN-G =   SPACE )
               MOVE    SPA-GMN-NYUKINKBNLST(1)
                                       TO  SPA-GMN-NYUKINKBN-G
               GO  TO  4100-NYUKINKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-NYUKINKBN-MAX )
                     OR  ( SPA-GMN-NYUKINKBN
                                   =   SPA-GMN-NYUKINKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-NYUKINKBN-MAX )
               MOVE    SPA-GMN-NYUKINKBNLST(1)
                                       TO  SPA-GMN-NYUKINKBN-G
           ELSE
               MOVE    SPA-GMN-NYUKINKBNLST(IDXA)
                                       TO  SPA-GMN-NYUKINKBN-G
           END-IF
      *
           .
      *
       4100-NYUKINKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額印字区分チェック処理
      *****************************************************************
       4100-NYUKINPRTKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-NYUKINKBN      =   "1" )
               MOVE    SPACE           TO  SPA-GMN-NYUKINPRTKBN-G
               GO  TO  4100-NYUKINPRTKBN-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-NYUKINPRTKBN-G =   SPACE )
               MOVE    SPA-GMN-NYUKINPRTKBNLST(1)
                                       TO  SPA-GMN-NYUKINPRTKBN-G
               GO  TO  4100-NYUKINPRTKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-NYUKINPRTKBN-MAX )
                     OR  ( SPA-GMN-NYUKINPRTKBN
                                   =   SPA-GMN-NYUKINPRTKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-NYUKINPRTKBN-MAX )
               MOVE    SPA-GMN-NYUKINPRTKBNLST(1)
                                       TO  SPA-GMN-NYUKINPRTKBN-G
           ELSE
               MOVE    SPA-GMN-NYUKINPRTKBNLST(IDXA)
                                       TO  SPA-GMN-NYUKINPRTKBN-G
           END-IF
      *
           .
      *
       4100-NYUKINPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再処理区分チェック処理
      *****************************************************************
       4100-SAISYORIKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-SAISYORIKBN-G =   SPACE )
               MOVE    SPA-GMN-SAISYORIKBNLST(1)
                                       TO  SPA-GMN-SAISYORIKBN-G
               GO  TO  4100-SAISYORIKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-SAISYORIKBN-MAX )
                     OR  ( SPA-GMN-SAISYORIKBN
                                   =   SPA-GMN-SAISYORIKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-SAISYORIKBN-MAX )
               MOVE    SPA-GMN-SAISYORIKBNLST(1)
                                       TO  SPA-GMN-SAISYORIKBN-G
           ELSE
               MOVE    SPA-GMN-SAISYORIKBNLST(IDXA)
                                       TO  SPA-GMN-SAISYORIKBN-G
           END-IF
      *
           .
      *
       4100-SAISYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自動遷移区分チェック処理
      *****************************************************************
       4100-CHGWINDOWKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-CHGWINDOWKBN-G =   SPACE )
               MOVE    SPA-GMN-CHGWINDOWKBNLST(1)
                                       TO  SPA-GMN-CHGWINDOWKBN-G
               GO  TO  4100-CHGWINDOWKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-CHGWINDOWKBN-MAX )
                     OR  ( SPA-GMN-CHGWINDOWKBN
                                   =   SPA-GMN-CHGWINDOWKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-CHGWINDOWKBN-MAX )
               MOVE    SPA-GMN-CHGWINDOWKBNLST(1)
                                       TO  SPA-GMN-CHGWINDOWKBN-G
           ELSE
               MOVE    SPA-GMN-CHGWINDOWKBNLST(IDXA)
                                       TO  SPA-GMN-CHGWINDOWKBN-G
           END-IF
      *
           .
      *
       4100-CHGWINDOWKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    削除区分チェック処理
      *****************************************************************
       4100-DELKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-DELKBN-G =   SPACE )
               MOVE    SPA-GMN-DELKBNLST(1)
                                       TO  SPA-GMN-DELKBN-G
               GO  TO  4100-DELKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-DELKBN-MAX )
                     OR  ( SPA-GMN-DELKBN
                                   =   SPA-GMN-DELKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-DELKBN-MAX )
               MOVE    SPA-GMN-DELKBNLST(1)
                                       TO  SPA-GMN-DELKBN-G
           ELSE
               MOVE    SPA-GMN-DELKBNLST(IDXA)
                                       TO  SPA-GMN-DELKBN-G
           END-IF
      *
           .
      *
       4100-DELKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書発行区分チェック処理
      *****************************************************************
       4100-SKYSUMKBN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-SKYSUMKBN-G =   SPACE )
               MOVE    SPA-GMN-SKYSUMKBNLST(1)
                                       TO  SPA-GMN-SKYSUMKBN-G
               GO  TO  4100-SKYSUMKBN-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-SKYSUMKBN-MAX )
                     OR  ( SPA-GMN-SKYSUMKBN
                                   =   SPA-GMN-SKYSUMKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-SKYSUMKBN-MAX )
               MOVE    SPA-GMN-SKYSUMKBNLST(1)
                                       TO  SPA-GMN-SKYSUMKBN-G
           ELSE
               MOVE    SPA-GMN-SKYSUMKBNLST(IDXA)
                                       TO  SPA-GMN-SKYSUMKBN-G
           END-IF
      *
           .
      *
       4100-SKYSUMKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書印刷区分チェック処理
      *****************************************************************
       4100-SKYPRTKBN-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-SKYPRTKBN-MAX )
                     OR  ( SPA-GMN-SKYPRTKBN
                                   =   SPA-GMN-SKYPRTKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-SKYPRTKBN-MAX )
               MOVE    SPA-GMN-SKYPRTKBNLST(4)
                                       TO  SPA-GMN-SKYPRTKBN-G
           ELSE
               MOVE    SPA-GMN-SKYPRTKBNLST(IDXA)
                                       TO  SPA-GMN-SKYPRTKBN-G
           END-IF
      *
           .
      *
       4100-SKYPRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細書印刷区分チェック処理
      *****************************************************************
       4100-SRYPRTKBN-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-SRYPRTKBN-MAX )
                     OR  ( SPA-GMN-SRYPRTKBN
                                   =   SPA-GMN-SRYPRTKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-SRYPRTKBN-MAX )
               MOVE    SPA-GMN-SRYPRTKBNLST(4)
                                       TO  SPA-GMN-SRYPRTKBN-G
           ELSE
               MOVE    SPA-GMN-SRYPRTKBNLST(IDXA)
                                       TO  SPA-GMN-SRYPRTKBN-G
           END-IF
      *
           .
      *
       4100-SRYPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報個別参照区分チェック処理
      *****************************************************************
       4100-PTINFREFKBN-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-PTINFREFKBN-MAX )
                     OR  ( SPA-GMN-PTINFREFKBN
                                   =   SPA-GMN-PTINFREFKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-PTINFREFKBN-MAX )
               MOVE    SPA-GMN-PTINFREFKBNLST(2)
                                       TO  SPA-GMN-PTINFREFKBN-G
           ELSE
               MOVE    SPA-GMN-PTINFREFKBNLST(IDXA)
                                       TO  SPA-GMN-PTINFREFKBN-G
           END-IF
      *
           .
      *
       4100-PTINFREFKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷順チェック処理
      *****************************************************************
       4100-PRTJYUN-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-PRTJYUN-MAX )
                     OR  ( SPA-GMN-PRTJYUN
                                   =   SPA-GMN-PRTJYUNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-PRTJYUN-MAX )
               MOVE    SPA-GMN-PRTJYUNLST(1)
                                       TO  SPA-GMN-PRTJYUN-G
           ELSE
               MOVE    SPA-GMN-PRTJYUNLST(IDXA)
                                       TO  SPA-GMN-PRTJYUN-G
           END-IF
      *
           .
      *
       4100-PRTJYUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院患者集計区分チェック処理
      *****************************************************************
       4100-TAIINGRPKBN-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-TAIINGRPKBN-MAX )
                     OR  ( SPA-GMN-TAIINGRPKBN
                                   =   SPA-GMN-TAIINGRPKBNL (IDXA))
               CONTINUE
           END-PERFORM
      *
           IF    ( IDXA    >   SPA-GMN-TAIINGRPKBN-MAX )
               MOVE    SPACE           TO  SPA-GMN-TAIINGRPKBN-G
           ELSE
               MOVE    SPA-GMN-TAIINGRPKBNLST(IDXA)
                                       TO  SPA-GMN-TAIINGRPKBN-G
           END-IF
      *
           .
      *
       4100-TAIINGRPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リポジトリ名チェック処理
      *****************************************************************
       4100-REPOS-NAME-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-REPOS-NAME  TO  WRK-HANKAKU
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-HANKAKU         TO  SPA-GMN-REPOS-NAME
           IF    ( FLG-ERROR       NOT =   ZERO )
               MOVE    "0105"          TO  SPA-ERRCD
               MOVE    15              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-REPOS-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書プログラム名チェック
      *****************************************************************
       4100-SKYPRT-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SKYPRT-NAME TO  WRK-HANKAKU
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-HANKAKU         TO  SPA-GMN-SKYPRT-NAME
           IF    ( FLG-ERROR       NOT =   ZERO )
               MOVE    "0106"          TO  SPA-ERRCD
               MOVE    16              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-SKYPRT-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療身明細書プログラム名チェック
      *****************************************************************
       4100-SRYPRT-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SRYPRT-NAME TO  WRK-HANKAKU
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-HANKAKU         TO  SPA-GMN-SRYPRT-NAME
           IF    ( FLG-ERROR       NOT =   ZERO )
               MOVE    "0107"          TO  SPA-ERRCD
               MOVE    17              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-SRYPRT-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
           MOVE    "W23"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           IF    ( SPA-ERRCD     =   SPACE )
               PERFORM 4901-KOUSHIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD     =   SPACE )
               PERFORM 4902-KOUSHIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD     =   SPACE )
               MOVE    "JOIN"              TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
      *
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4901-KOUSHIN-SEC           SECTION.
      *
           MOVE    SPACE                   TO  SYS-5010-REC
           INITIALIZE                          SYS-5010-REC
      *
           INITIALIZE                      SYS-5010-REC.
           MOVE    "5010"              TO  SYS-5010-KANRICD.
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5010-STYUKYMD
                                           SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF  FLG-SYSKANRI   =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-5010-REC
           ELSE
               MOVE    SPACE                   TO  SYS-5010-REC
               INITIALIZE                      SYS-5010-REC
           END-IF
      *
           MOVE    SPA-HOSPNUM             TO  SYS-5010-HOSPNUM
           MOVE   "5010"                   TO  SYS-5010-KANRICD
           MOVE    "*"                     TO  SYS-5010-KBNCD
           MOVE    SPA-W01-STYUKYMD        TO  SYS-5010-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-5010-EDYUKYMD
           MOVE    SPA-NAI-SEIKYU01        TO  SYS-5010-SEIKYU-DAY1
           MOVE    SPA-NAI-SEIKYU02        TO  SYS-5010-SEIKYU-DAY2
           MOVE    SPA-NAI-SEIKYU03        TO  SYS-5010-SEIKYU-DAY3
           MOVE    SPA-GMN-NYUKINKBN       TO  SYS-5010-NYUKINKBN
           MOVE    SPA-GMN-NYUKINPRTKBN    TO  SYS-5010-NYUKINPRTKBN
           MOVE    SPA-GMN-SAISYORIKBN     TO  SYS-5010-SAISYORIKBN
           MOVE    SPA-GMN-CHGWINDOWKBN    TO  SYS-5010-CHGWINDOWKBN
           MOVE    SPA-GMN-DELKBN          TO  SYS-5010-DELKBN
           MOVE    SPA-GMN-SKYSUMKBN       TO  SYS-5010-SKYSUMKBN
           MOVE    SPA-GMN-SKYPRTKBN       TO  SYS-5010-SKYPRTKBN
           MOVE    SPA-GMN-SRYPRTKBN       TO  SYS-5010-SRYPRTKBN
           MOVE    SPA-GMN-PTINFREFKBN     TO  SYS-5010-PTINFREFKBN
           MOVE    SPA-GMN-PRTJYUN         TO  SYS-5010-PRTJYUN
           MOVE    SPA-GMN-TAIINGRPKBN     TO  SYS-5010-TAIINGRPKBN
           MOVE    SPA-TERMID              TO  SYS-5010-TERMID
           MOVE    SPA-OPID                TO  SYS-5010-OPID
           MOVE    SYS-5010-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
      *
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4902-KOUSHIN-SEC           SECTION.
      *
           MOVE    SPACE                   TO  SYS-3103-REC
           INITIALIZE                          SYS-3103-REC
      *
           INITIALIZE                      SYS-3103-REC.
           MOVE    "3103"              TO  SYS-3103-KANRICD.
           MOVE    "5010001"           TO  SYS-3103-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3103-STYUKYMD
                                           SYS-3103-EDYUKYMD
           MOVE    SYS-3103-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF  FLG-SYSKANRI   =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-3103-REC
           ELSE
               MOVE    SPACE                   TO  SYS-3103-REC
               INITIALIZE                      SYS-3103-REC
           END-IF
      *
           MOVE    SPA-HOSPNUM             TO  SYS-3103-HOSPNUM
           MOVE   "3103"                   TO  SYS-3103-KANRICD
           MOVE    "5010001"               TO  SYS-3103-KBNCD
           MOVE    "00000000"              TO  SYS-3103-STYUKYMD
           MOVE    "99999999"              TO  SYS-3103-EDYUKYMD
           MOVE    SPA-GMN-REPOS-NAME      TO  SYS-3103-REPOS-NAME
           MOVE    SPA-GMN-SKYPRT-NAME     TO  SYS-3103-PROGRAM-NAME (1)
           MOVE    SPA-GMN-SRYPRT-NAME     TO  SYS-3103-PROGRAM-NAME (2)
           MOVE    SPA-TERMID              TO  SYS-3103-TERMID
           MOVE    SPA-OPID                TO  SYS-3103-OPID
           MOVE    SYS-3103-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
      *
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "2002"              TO  SPA-ERRCD
           END-IF
      *
           .
       4902-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
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
           MOVE    SPACE           TO  LINKAREA.
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "W23"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE                   TO  W23.
      *
           MOVE    SPA-GMN-YUKOSTYMD       TO  W23-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD       TO  W23-YUKOEDYMD
      *
           MOVE    SPA-GMN-SEIKYU01        TO  W23-SEIKYU01
           MOVE    SPA-GMN-SEIKYU02        TO  W23-SEIKYU02
           MOVE    SPA-GMN-SEIKYU03        TO  W23-SEIKYU03
           MOVE    SPA-GMN-NYUKINKBN-G     TO  W23-NYUKINKBN
           MOVE    SPA-GMN-NYUKINKBN-MAX   TO  W23-NYUKINKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUKINKBN-MAX )
               MOVE    SPA-GMN-NYUKINKBNLST(IDX1)
                                           TO  W23-NYUKINKBNLST(IDX1)
           END-PERFORM
           MOVE    SPA-GMN-NYUKINPRTKBN-G     TO  W23-NYUKINPRTKBN
           MOVE    SPA-GMN-NYUKINPRTKBN-MAX   TO  W23-NYUKINPRTKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUKINPRTKBN-MAX )
               MOVE    SPA-GMN-NYUKINPRTKBNLST(IDX1)
                                           TO  W23-NYUKINPRTKBNLST(IDX1)
           END-PERFORM
           MOVE    SPA-GMN-SAISYORIKBN-G     TO  W23-SAISYORIKBN
           MOVE    SPA-GMN-SAISYORIKBN-MAX   TO  W23-SAISYORIKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SAISYORIKBN-MAX )
               MOVE    SPA-GMN-SAISYORIKBNLST(IDX1)
                                           TO  W23-SAISYORIKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-CHGWINDOWKBN-G  TO  W23-CHGWINDOWKBN
           MOVE    SPA-GMN-CHGWINDOWKBN-MAX TO W23-CHGWINDOWKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-CHGWINDOWKBN-MAX )
               MOVE    SPA-GMN-CHGWINDOWKBNLST(IDX1)
                                           TO  W23-CHGWINDOWKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-DELKBN-G        TO  W23-DELKBN
           MOVE    SPA-GMN-DELKBN-MAX      TO  W23-DELKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-DELKBN-MAX )
               MOVE    SPA-GMN-DELKBNLST(IDX1)
                                           TO  W23-DELKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYSUMKBN-G     TO  W23-SKYSUMKBN
           MOVE    SPA-GMN-SKYSUMKBN-MAX   TO  W23-SKYSUMKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SKYSUMKBN-MAX )
               MOVE    SPA-GMN-SKYSUMKBNLST(IDX1)
                                           TO  W23-SKYSUMKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYPRTKBN-G     TO  W23-SKYPRTKBN
           MOVE    SPA-GMN-SKYPRTKBN-MAX   TO  W23-SKYPRTKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SKYPRTKBN-MAX )
               MOVE    SPA-GMN-SKYPRTKBNLST(IDX1)
                                           TO  W23-SKYPRTKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYPRTKBN-G     TO  W23-SRYPRTKBN
           MOVE    SPA-GMN-SRYPRTKBN-MAX   TO  W23-SRYPRTKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SRYPRTKBN-MAX )
               MOVE    SPA-GMN-SRYPRTKBNLST(IDX1)
                                           TO  W23-SRYPRTKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-PTINFREFKBN-G     TO  W23-PTINFREFKBN
           MOVE    SPA-GMN-PTINFREFKBN-MAX   TO  W23-PTINFREFKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-PTINFREFKBN-MAX )
               MOVE    SPA-GMN-PTINFREFKBNLST(IDX1)
                                           TO  W23-PTINFREFKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTJYUN-G     TO  W23-PRTJYUN
           MOVE    SPA-GMN-PRTJYUN-MAX   TO  W23-PRTJYUN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-PRTJYUN-MAX )
               MOVE    SPA-GMN-PRTJYUNLST(IDX1)
                                           TO  W23-PRTJYUNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-TAIINGRPKBN-G
                                       TO  W23-TAIINGRPKBN
           MOVE    SPA-GMN-TAIINGRPKBN-MAX
                                       TO  W23-TAIINGRPKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-TAIINGRPKBN-MAX )
               MOVE    SPA-GMN-TAIINGRPKBNLST(IDX1)
                                       TO  W23-TAIINGRPKBNLST(IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-REPOS-NAME      TO  W23-REPOS-NAME
           MOVE    SPA-GMN-SKYPRT-NAME     TO  W23-SKYPRT-NAME
           MOVE    SPA-GMN-SRYPRT-NAME     TO  W23-SRYPRT-NAME
      *
           MOVE    "F"                     TO  W23-FR-REPO-VISIBLE
           MOVE    "red"                   TO  W23-LBLMSG01-STYLE
           IF    ( SPA-MW                  =   SPA-GINBEE )
               IF    ( SPA-NAI-CUSTOM-VISIBLE  =   "1" )
                   MOVE    "T"             TO  W23-FR-REPO-VISIBLE
               END-IF
      *
               IF    ( SPA-NAI-CUSTOM-CAUTION  =   "1" )
                 MOVE  "ユーザ設定を利用するためには、課金が必要です"
                                           TO  W23-LBLMSG01
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-NYUKINKBN       =   "1" )
               MOVE    WIDGET-INSENSITIVE  TO  W23-NYUKINPRTKBN-STATE
                                               W23-NYUKINPRTKBNLST-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W23-NYUKINPRTKBN-STATE
                                               W23-NYUKINPRTKBNLST-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
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
               MOVE    "入力エラー"                    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "有効な日付（１〜２７，９９）を入力してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "請求日を設定してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "請求日は１期より順に設定してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "日付の設定に誤りがあります"
                                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "月末日（９９）を設定してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "リポジトリ名は半角で入力してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0106"
             MOVE "請求書兼領収書のプログラム名は半角で入力してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0107"
             MOVE "診療費明細書のプログラム名は半角で入力してください"
                                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "リポジトリ名が入力されています。"
                                           DELIMITED   BY  SIZE
                       "一つ以上のユーザプログラム名を入力してください"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0109"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "ユーザプログラム名が入力されています。"
                                           DELIMITED   BY  SIZE
                       "リポジトリ名を入力してください"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "2001"
           WHEN    "2002"
               MOVE    "更新処理に失敗しました"        TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W23"                TO  SPA-MOTOPG
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
               MOVE    "定期請求情報を更新します"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W23"                TO  SPA-MOTOPG
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
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
           PERFORM 50012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
                   MOVE    "SEIKYU01"      TO  MCP-WIDGET
           WHEN    02
                   MOVE    "SEIKYU02"      TO  MCP-WIDGET
           WHEN    03
                   MOVE    "SEIKYU03"      TO  MCP-WIDGET
           WHEN    04
                   MOVE    "NYUKINKBN"     TO  MCP-WIDGET
           WHEN    05
                   MOVE    "SAISYORIKBN"   TO  MCP-WIDGET
           WHEN    06
                   MOVE    "NYUKINPRTKBN"  TO  MCP-WIDGET
           WHEN    07
                   MOVE    "DELKBN"        TO  MCP-WIDGET
           WHEN    08
                   MOVE    "SKYSUMKBN"     TO  MCP-WIDGET
           WHEN    09
                   MOVE    "CHGWINDOWKBN"  TO  MCP-WIDGET
           WHEN    10
                   MOVE    "SKYPRTKBN"     TO  MCP-WIDGET
           WHEN    11
                   MOVE    "SRYPRTKBN"     TO  MCP-WIDGET
           WHEN    12
                   MOVE    "PTINFREFKBN"   TO  MCP-WIDGET
           WHEN    13
                   MOVE    "PRTJYUN"       TO  MCP-WIDGET
           WHEN    14
                   MOVE    "TAIINGRPKBN"   TO  MCP-WIDGET
           WHEN    15
                   MOVE    "REPOS_NAME"    TO  MCP-WIDGET
           WHEN    16
                   MOVE    "SKYPRT_NAME"   TO  MCP-WIDGET
           WHEN    17
                   MOVE    "SRYPRT_NAME"   TO  MCP-WIDGET
           WHEN    101
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SEIKYU01"
                   MOVE    2           TO  SPA-GMN-CUR
           WHEN    "SEIKYU02"
                   MOVE    3           TO  SPA-GMN-CUR
           WHEN    "SEIKYU03"
                   MOVE    4           TO  SPA-GMN-CUR
           WHEN    "NYUKINKBN"
                   MOVE    6           TO  SPA-GMN-CUR
           WHEN    "NYUKINPRTKBN"
                   MOVE    5           TO  SPA-GMN-CUR
           WHEN    "SAISYORIKBN"
                   MOVE    9           TO  SPA-GMN-CUR
           WHEN    "CHGWINDOWKBN"
                   MOVE    7           TO  SPA-GMN-CUR
           WHEN    "DELKBN"
                   MOVE    8           TO  SPA-GMN-CUR
           WHEN    "SKYSUMKBN"
                   MOVE    10          TO  SPA-GMN-CUR
           WHEN    "SKYPRTKBN"
                   MOVE    11          TO  SPA-GMN-CUR
           WHEN    "SRYPRTKBN"
                   MOVE    12          TO  SPA-GMN-CUR
           WHEN    "PTINFREFKBN"
                   MOVE    13          TO  SPA-GMN-CUR
           WHEN    "PRTJYUN"
                   MOVE    14          TO  SPA-GMN-CUR
           WHEN    "TAIINGRPKBN"
                   MOVE    15          TO  SPA-GMN-CUR
           WHEN    "REPOS_NAME"
                   MOVE    16          TO  SPA-GMN-CUR
           WHEN    "SKYPRT_NAME"
                   MOVE    17          TO  SPA-GMN-CUR
           WHEN    "SRYPRT_NAME"
                   MOVE    101         TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       50012-CUR-SKIP-SEC              SECTION.
      *
           IF    ( SPA-GMN-CUR         =   6 )
               IF   ( W23-NYUKINPRTKBN-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR         =   15 )
               IF    ( SPA-MW          =   SPA-GINBEE )
                AND  ( SPA-NAI-CUSTOM-VISIBLE
                                       =   "1" )
                   CONTINUE
               ELSE
                   MOVE    101         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       50012-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       530-RIDSET2-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 410-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    (  SPA-NAI-SEIKYU (1) =   ZERO)
                AND  (  SPA-NAI-SEIKYU (2) =   ZERO)
                AND  (  SPA-NAI-SEIKYU (3) =   ZERO)
                   MOVE    "0101"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM VARYING IDX1        FROM    1   BY  1
                       UNTIL ( IDX1        >   2 )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE IDX2    =   IDX1    +   1
      *
                   IF    ( SPA-NAI-SEIKYU (IDX1)       =   ZERO )
                    AND  ( SPA-NAI-SEIKYU (IDX2)   NOT =   ZERO )
                       MOVE    "0102"      TO  SPA-ERRCD
                       MOVE    IDX1        TO  SPA-GMN-CUR
                   END-IF
      *
                   IF    ( SPA-ERRCD   =   SPACE )
                       IF    ( SPA-NAI-SEIKYU (IDX1)   NOT =   ZERO )
                        AND  ( SPA-NAI-SEIKYU (IDX2)   NOT =   ZERO )
                           IF    ( SPA-NAI-SEIKYU (IDX1)
                                       <   SPA-NAI-SEIKYU (IDX2) )
                               CONTINUE
                           ELSE
                               MOVE    "0103"  TO  SPA-ERRCD
                               MOVE    IDX1    TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               MOVE    ZERO            TO   FLG-CHK
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >    3 )
                   IF    (  SPA-NAI-SEIKYU (IDX1) =   "99" )
                       MOVE    1       TO  FLG-CHK
                       MOVE    3       TO  IDX1
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-CHK         =   ZERO )
                   MOVE    "0104"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-MW      =   SPA-GINBEE )
                AND  ( SPA-NAI-CUSTOM-VISIBLE
                                   =   "1"        )
      *
                   IF    ( SPA-GMN-REPOS-NAME  NOT =   SPACE )
                    AND  ( SPA-GMN-USER-PG-G       =   SPACE )
                       MOVE    "0108"  TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-GMN-REPOS-NAME      =   SPACE )
                    AND  ( SPA-GMN-USER-PG-G   NOT =   SPACE )
                       MOVE    "0109"  TO  SPA-ERRCD
                   END-IF
      *
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "1001"      TO  SPA-WIDCD
           END-IF
           .
      *
       530-RIDSET2-EXT.
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
      *    半角チェック処理
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ERROR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-HANKAKU         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   2     )
               MOVE    1               TO  FLG-ERROR
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-HANKAKU
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC.
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF  MCP-RC   =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING      MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF  MCP-RC   =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC.
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *
