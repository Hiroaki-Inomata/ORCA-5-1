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
       PROGRAM-ID.                 ORCSCHKTCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 包括診療行為チェック処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/02/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.04.00    NACL-多々納  08/12/XX  入院料包括対応
      *  04.05.00    NACL-多々納  09/09/XX  外来まとめ対応
      *                                     慢性維持透析医学管理料等対応
      *  04.05.00    NACL-多々納  10/02/XX  アフターケア対象外対応
      *  04.06.00    NACL-多々納  10/10/XX  包括算定（剤）対応
      *  04.06.00    NACL-多々納  11/01/XX  電子点数表マスタ対応
      *  04.06.00    NACL-多々納  12/03/28  Ｈ２４年４月改正対応
      *  04.07.00    NACL-多々納  12/08/XX  サンプル薬（サ）対応
      *  04.07.00    NACL-多々納  14/03/XX  Ｈ２６年４月改正対応
      *                                     短期滞在手術対応
      *  04.08.00    NACL-多々納  15/09/XX  短期滞在手術３　外来対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *                                     対象テーブル２００へ
      *  04.08.00    NACL-多々納  17/01/17  翌月入院料チェック対応
      *  04.08.00    NACL-多々納  17/04/XX  短術３検体検査管理加算対応
      *  04.08.00    NACL-多々納  17/09/XX  当月受診ありチェック対応
      *  05.00.00    NACL-多々納  18/10/XX  短術３画像診断等チェック対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HKTSANTEI           PIC 9(01).
           03  FLG-HKTSRYCD            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK2                PIC 9(01).
           03  FLG-CHK3                PIC 9(01).
           03  FLG-CHK4                PIC 9(01).
           03  FLG-HOUKATU             PIC 9(01).
           03  FLG-SONOTA              PIC 9(01).
           03  FLG-ZAIARI              PIC 9(01).
           03  FLG-SANTEIOK            PIC 9(01).
           03  FLG-INPUTOK             PIC 9(01).
           03  FLG-DEKIDAKA            PIC 9(01).
      *
           03  FLG-HKTINP              PIC 9(01).
      *ver.4.7
           03  FLG-INPUTOK2            PIC 9(01).
           03  FLG-INPUTOK6            PIC 9(01).
           03  FLG-INPUTOK7            PIC 9(01).
      *H29.4
           03  FLG-TANKI3HKT-OK        PIC 9(01).
           03  FLG-TANKI3HKT-NO        PIC 9(01).
      *H29.9
           03  FLG-TEISEI-ARI          PIC 9(01).
           03  FLG-JYURRK-CHK          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX-N                   PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-K                   PIC 9(04).
           03  IDX-Z                   PIC 9(04).
           03  IDX-Z2                  PIC 9(04).
           03  IDX-Z3                  PIC 9(04).
      *
           03  IDX-L1                  PIC 9(04).
           03  IDX-L2                  PIC 9(04).
           03  IDX-ERR                 PIC 9(04).
      *
           03  IDD                     PIC 9(02).
           03  IDD2                    PIC 9(02).
           03  IDX-D                   PIC 9(02).
           03  IDX-SN                  PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TEI-CNT             PIC 9(03).
           03  WRK-DAY                 PIC S9(03).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
           03  WRK-ERRDAY              PIC 9(02).
      *
           03  WRK-K-SRYCD             PIC X(09).
           03  WRK-NT-SRYCD            PIC X(09).
      *
           03  WRK-IDX-N               PIC 9(04).
      *H27.9
           03  WRK-TANKI3-IDX          PIC 9(04).
      *
      *    包括する診療区分
           03  WRK-HKT-SRYKBN-G.
               05  WRK-HKTKBN20        PIC 9(01).
               05  WRK-HKTKBN30        PIC 9(01).
               05  WRK-HKTKBN40        PIC 9(01).
               05  WRK-HKTKBN50        PIC 9(01).
               05  WRK-HKTKBN60        PIC 9(01).
               05  WRK-HKTKBN70        PIC 9(01).
               05  WRK-HKTKBN80        PIC 9(01).
               05  WRK-HKTKBN81        PIC 9(01).
               05  WRK-HKTKBN82        PIC 9(01).
               05  WRK-HKTKBN83        PIC 9(01).
               05  WRK-HKTKBN64        PIC 9(01).
               05  WRK-HKTKBN13        PIC 9(01).
               05  WRK-HKTKBN14        PIC 9(01).
               05  WRK-HKTKBN10        PIC 9(01).
      *    包括
           03  WRK-NYUHKT-AREA.
               05  WRK-NYUHKT-OCCURS   OCCURS   31.
                   07  WRK-NYUHKTCD        PIC X(03).
                   07  WRK-N-SRYCD         PIC X(09).
      *
      *    自動発生のチェック区分
           03  WRK-ORCSC-KASAN.
               05  WRK-ORCSC-KASAN-TBL  OCCURS  10.
                   07  WRK-ORCSC-KASN-TBL2     OCCURS   30.
      *                包括剤
                       09  WRK-ORCSC-HKTKBN        PIC 9(01).
      *H29.1
      *    翌月入院料
           03  WRK-N-SRYYM.
               05  WRK-N-SRYYY         PIC 9(04).
               05  WRK-N-SRYMM         PIC 9(02).
           03  WRK-TANKI-DAY           PIC 9(02).
           03  WRK-LST-DD              PIC 9(02).
      *
      *H30.10
           03  WRK-CHK-SRYKBN          PIC X(02).
      *
      *    チェック用
       01  WRK-TBL-AREA.
           03  WRK-KANRICD             PIC  X(03).
           03  WRK-SANTEI-HOU          PIC  X(01).
           03  WRK-SANTEI-KBN          PIC  X(01).
           03  WRK-NYUGAIKBN           PIC  X(01).
           03  WRK-HKNCOMBIKBN         PIC  X(01).
           03  WRK-SRYKBN-G.
               07  WRK-SRYKBN-KBN      PIC X(01)
                                           OCCURS   50.
      *
       01  TBL-AREA.
           03  TBL-DATA-G              OCCURS   100.
               05  TBL-KANRICD             PIC  X(03).
               05  TBL-SANTEI-HOU          PIC  X(01).
               05  TBL-SANTEI-KBN          PIC  X(01).
               05  TBL-NYUGAIKBN           PIC  X(01).
               05  TBL-HKNCOMBIKBN         PIC  X(01).
               05  TBL-SRYKBN-G.
                   07  TBL-SRYKBN-KBN      PIC X(01)
                                           OCCURS   50.
               05  TBL-SRYCD-G.
                   07  TBL-SRYCD           PIC X(09)
                                           OCCURS   200.
           03  TBL-MAX                 PIC 9(04).
      *入院料
       01  TBL-NYUAREA.
           03  TBL-NYUDATA-G              OCCURS   100.
               05  TBL-N-KANRICD             PIC  X(03).
               05  TBL-N-SANTEI-HOU          PIC  X(01).
               05  TBL-N-SANTEI-KBN          PIC  X(01).
               05  TBL-N-NYUGAIKBN           PIC  X(01).
               05  TBL-N-HKNCOMBIKBN         PIC  X(01).
               05  TBL-N-SRYKBN-G.
                   07  TBL-N-SRYKBN-KBN      PIC X(01)
                                             OCCURS   50.
               05  TBL-N-SRYCD-G.
                   07  TBL-N-SRYCD           PIC X(09)
                                             OCCURS   200.
           03  TBL-N-MAX                 PIC 9(04).
      *
       01  TBL-HKTSANTEI-AREA.
           03  TBL-HKTSANTEI-DATA-G        OCCURS   600.
               05  TBL-HKTSRYCD            PIC X(09).
               05  TBL-SANTEIKBN           PIC X(01).
               05  TBL-KENTAIKBN           PIC X(01).
      *
      *    出来高算定日
       01  TBL-DEKIDAKA-AREA.
           03  TBL-DEKIDAKA-G              OCCURS   31.
               05  TBL-DEKIDAKA            PIC 9(01).
      *
      *    保存ＳＰＡ領域ワーク
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *H28.9
      *持参薬予約コード
       01  CONS-AREA.
           03  CONS-JSNYCD1        PIC X(09)    VALUE   "099209811".
           03  CONS-JSNYCD2        PIC X(09)    VALUE   "099209812".
           03  CONS-JSNYCD3        PIC X(09)    VALUE   "099209821".
           03  CONS-JSNYCD4        PIC X(09)    VALUE   "099209822".
      *
           03  CONS-JSNYCD5        PIC X(09)    VALUE   "099209711".
           03  CONS-JSNYCD6        PIC X(09)    VALUE   "099209712".
           03  CONS-JSNYCD7        PIC X(09)    VALUE   "099209721".
           03  CONS-JSNYCD8        PIC X(09)    VALUE   "099209722".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1014.INC".
      *    包括チェックマスタ
       01  HKTSANTEI-REC.
           COPY    "CPHKTSANTEI.INC".
      *    包括診療コードマスタ
       01  HKTSRYCD-REC.
           COPY    "CPHKTSRYCD.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    電子点数表マスタチェックパラメタ
           COPY    "CPORCSCETNSCHK.INC".
      *
      *    短期滞在手術３チェックパラメタ
           COPY    "CPORCSTANKI3HKT.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    画面スパ領域
      *    COPY    "K02SCR-SPA".
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為算定用共通パラメタ(自動発生分チェック）
           COPY    "CPORCSC00.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCHKTCHKAREA
           SPA-AREA
     ****  SPA-K02
           SPA-SCR-REC
           ORCSC00AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           IF      ORCSCHKTCHK-PG      =   "K02"
                                       OR  "K02N"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
           END-IF
           INITIALIZE                  ORCSCHKTCHK-OUTAREA
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           EVALUATE    ORCSCHKTCHK-SYORI
               WHEN    "1"
      *        全体チェック
      *            包括対象有無
                   PERFORM 100-INIT-SYORI-SEC
      *            チェック処理
                   PERFORM 200-MAIN-SYORI-SEC
               WHEN    "2"
      *        自動発生分
      *            包括対象有無
                   PERFORM 100-INIT-SYORI-SEC
      *            包括チェック
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    "3"
      *        エラー削除
                   PERFORM 400-ERR-DELETE-SEC
               WHEN    "5"
      *        入院料算定チェック
      *            包括対象有無
                   PERFORM 100-INIT-SYORI-SEC
      *            チェック処理
                   PERFORM 500-NYUIN-CHKSYORI-SEC
               WHEN    "6"
      *        入院料算定期間・包括チェック
      *            包括対象有無
                   PERFORM 100-INIT-SYORI-SEC
      *            入院料チェック処理
                   IF      ORCSCHKTCHK-NYUHKTCHK   =   1
                       PERFORM 600-NYUINDAYCHK-SEC
                   ELSE
      *                包括区分クリア
                       PERFORM 602-HOUKATU-INIT-SEC
                   END-IF
                   IF      SPA-HKTSANTEI-FLG   =   1
      *                包括チェック判定
                       PERFORM 2001-TAISYOU-CHK-SEC
                   END-IF
      *            包括算定（剤）チェック
                   PERFORM 200213-HKTINPUT-CHK-SEC
      *            剤の編集
                   PERFORM 2002-ZAIHENSYU-SEC
           END-EVALUATE
           IF      ORCSCHKTCHK-PG      =   "K02"
                                       OR  "K02N"
                                       OR  "KT01"
               MOVE    SPA-K01KYOUTU        TO  SPA-WORK
           END-IF
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC            SECTION.
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                  SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    WRK-SRYYMD          TO  SYS-1014-STYUKYMD
           MOVE    WRK-SRYYMD          TO  SYS-1014-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
      *
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
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-N
           INITIALIZE                  TBL-AREA
           INITIALIZE                  TBL-NYUAREA
           PERFORM UNTIL  (FLG-SYSKANRI    =   1  )
                       OR (IDX             >=  100 )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               IF      SYS-1014-SANTEI-HOU     =   "1"
                                               OR  "2"
      *            包括算定方法あり
                   PERFORM 1002-KBNCD-TBLHEN1-SEC
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コード編集処理
      *****************************************************************
       1002-KBNCD-TBLHEN1-SEC            SECTION.
      *
           IF      SYS-1014-KBNCD (1:3)    >=  "500"
      *        入院料
               ADD     1                   TO  IDX-N
               MOVE    SYS-1014-KBNCD (1:3)
                                           TO  TBL-N-KANRICD (IDX-N)
      *        包括算定方法
               MOVE    SYS-1014-SANTEI-HOU
                                           TO  TBL-N-SANTEI-HOU (IDX-N)
               MOVE    SYS-1014-SANTEI-KBN
                                           TO  TBL-N-SANTEI-KBN (IDX-N)
               MOVE    SYS-1014-NYUGAIKBN
                                           TO  TBL-N-NYUGAIKBN (IDX-N)
               MOVE    SYS-1014-HKNCOMBI-KBN
                                           TO  TBL-N-HKNCOMBIKBN (IDX-N)
               MOVE    SYS-1014-SRYKBN-G
                                           TO  TBL-N-SRYKBN-G (IDX-N)
               PERFORM 1001-HKTSRYCD-HEN-SEC
               MOVE    IDX-N               TO  TBL-N-MAX
      *H27.9
      *        短期滞在手術基本料３　あり
               IF      SYS-1014-KBNCD (1:3)    =   "526"
                   MOVE    IDX-N               TO  WRK-TANKI3-IDX
               END-IF
           ELSE
      *        診療コード
               ADD     1                   TO  IDX
               MOVE    SYS-1014-KBNCD (1:3)
                                           TO  TBL-KANRICD (IDX)
      *        包括算定方法
               MOVE    SYS-1014-SANTEI-HOU
                                           TO  TBL-SANTEI-HOU (IDX)
               MOVE    SYS-1014-SANTEI-KBN
                                           TO  TBL-SANTEI-KBN (IDX)
               MOVE    SYS-1014-NYUGAIKBN
                                           TO  TBL-NYUGAIKBN (IDX)
               MOVE    SYS-1014-HKNCOMBI-KBN
                                           TO  TBL-HKNCOMBIKBN (IDX)
               MOVE    SYS-1014-SRYKBN-G
                                           TO  TBL-SRYKBN-G (IDX)
               PERFORM 1001-HKTSRYCD-HEN-SEC
               MOVE    IDX                 TO  TBL-MAX
           END-IF
           .
       1002-KBNCD-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コード編集処理
      *****************************************************************
       1001-HKTSRYCD-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                  HKTSRYCD-REC
           MOVE    SYS-1014-KBNCD (1:3)    TO  HKTSRYCD-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSRYCD-YUKOEDYMD
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL  (FLG-HKTSRYCD    =   1  )
                      OR  (IDY             >=  200 )
               IF      SYS-1014-KBNCD (1:3)    >=  "500"
                   ADD     1                   TO  IDY
                   MOVE    HKTSRYCD-SRYCD      TO  TBL-N-SRYCD
                                                           (IDX-N IDY)
               ELSE
                   ADD     1                   TO  IDY
                   MOVE    HKTSRYCD-SRYCD      TO  TBL-SRYCD(IDX IDY)
      *
               END-IF
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-HKTSRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SYORI-SEC            SECTION.
      *
      *    包括区分クリア
           MOVE    ZERO                TO  FLG-DEKIDAKA
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ZAI (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ERR (IDX-Z)
      *        出来高（会計）があれば、すべて出来高
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999905"
                   MOVE    1                   TO  FLG-DEKIDAKA
               END-IF
      *        包括算定（剤）判定
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999908"
                   MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
               END-IF
      *H28.9
      *        持参薬予約コードは包括剤とする
               IF      SPA-COM-INPUTCD (IDX-Z) =   CONS-JSNYCD1
                                               OR  CONS-JSNYCD2
                                               OR  CONS-JSNYCD3
                                               OR  CONS-JSNYCD4
                                               OR  CONS-JSNYCD5
                                               OR  CONS-JSNYCD6
                                               OR  CONS-JSNYCD7
                                               OR  CONS-JSNYCD8
                   MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
               END-IF
           END-PERFORM
      *
           IF      FLG-DEKIDAKA        =   ZERO
      *    包括チェック判定
               PERFORM 2001-TAISYOU-CHK-SEC
           END-IF
      *    包括算定（剤）チェック
           PERFORM 200213-HKTINPUT-CHK-SEC
      *    剤の編集
           PERFORM 2002-ZAIHENSYU-SEC
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括剤編集処理
      *****************************************************************
       2002-ZAIHENSYU-SEC            SECTION.
      *
      *    包括コードがある剤はすべて包括とする
           MOVE    SPA-SCR-REC         TO  HZN-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  IDX-S
           MOVE    ZERO                TO  IDX-ERR
           MOVE    ZERO                TO  FLG-HOUKATU
           MOVE    ZERO                TO  FLG-SONOTA
           MOVE    ZERO                TO  ORCSCHKTCHK-HKTERR
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                             ((HZN-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (HZN-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-COM-HOUKATU-ERR (IDX-Z) =   1  )
                   MOVE    1                   TO  ORCSCHKTCHK-HKTERR
               END-IF
               IF     (SPA-COM-HOUKATU-FLG (IDX-Z) =   1  )
      *            コメント以外
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1)  =   "8" )
                       OR (SPA-COM-INPUTCD (IDX-Z)(1:3)  =   "008")
                       OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)  =   "."  )
      *ver.4.7         （サ）は包括対象とする
                       IF     SPA-COM-INPUTCD(IDX-Z) =   "820000046"
                           MOVE    1                   TO  FLG-HOUKATU
                       END-IF
      *????
                       CONTINUE
                   ELSE
      *H26.3
      *            入院算定日指定
                       IF     (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "*"  )
                           CONTINUE
                       ELSE
                           MOVE    1                   TO  FLG-HOUKATU
                       END-IF
                   END-IF
               END-IF
               IF     (SPA-COM-HOUKATU-FLG (IDX-Z) =   ZERO  )
                   IF    ((SPA-COM-INPUTCD (IDX-Z)(1:1)    =   "1" )
                      AND (SPA-COM-DATAKBN (IDX-Z)         =   "1" ))
                   OR    ( SPA-COM-INPUTCD (IDX-Z)(1:1)    =   "6"
                                                           OR  "7" )
                   OR    ( SPA-COM-HOUKATU-FLG2 (IDX-Z)    =   1   )
                       MOVE    1                   TO  FLG-SONOTA
                   END-IF
               END-IF
               IF     (SPA-COM-ZAIEND (IDX-Z)  =   "1"   )
                   PERFORM VARYING     IDX     FROM    IDX-S  BY  1
                           UNTIL      (IDX         >   IDX-Z  )
                       MOVE    FLG-HOUKATU     TO  SPA-COM-HOUKATU-ZAI
                                                              (IDX)
                       IF      FLG-HOUKATU     =   ZERO
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-FLG
                                                              (IDX)
                       END-IF
      *                剤内に包括外がある場合、警告
                       IF     (FLG-HOUKATU     =   1   )  AND
                              (FLG-SONOTA      =   1   )  AND
                              (SPA-COM-HOUKATU-FLG (IDX) =   ZERO)
                           PERFORM 2002-HOUKATU-GAI-CHK-SEC
                       ELSE
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX) 
                       END-IF
                   END-PERFORM
                   MOVE    IDX                 TO  IDX-S
                   MOVE    ZERO                TO  FLG-HOUKATU
                   MOVE    ZERO                TO  FLG-SONOTA
               END-IF
           END-PERFORM
      *
           IF      IDX-ERR         NOT =   ZERO
               IF      SPA-HKTKEI-FLG      =   ZERO
                   MOVE    "K202"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-HKTKEI-FLG
                   MOVE    "1"             TO  SPA-COM-CUR(IDX-ERR)
               END-IF
           END-IF
      *
           .
       2002-ZAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括外チェック処理
      *****************************************************************
       2002-HOUKATU-GAI-CHK-SEC            SECTION.
      *
           EVALUATE    WRK-KANRICD
           WHEN    "001"
      *        外来診療料は薬剤・器材は算定可
               IF    (SPA-COM-INPUTCD (IDX)(1:1)   =   "6"
                                                   OR  "7" )
                   MOVE    1           TO  SPA-COM-HOUKATU-FLG2 (IDX)
               END-IF
           WHEN    "201"
      *        慢性維持透析患者料は薬剤・器材は算定可
               IF    (SPA-COM-INPUTCD (IDX)(1:1)   =   "6"
                                                   OR  "7" )
                   MOVE    1           TO  SPA-COM-HOUKATU-FLG2 (IDX)
               END-IF
           WHEN    "208"
           WHEN    "525"
      *        短期滞在手術１、２のフィルムは算定可
               IF    (SPA-COM-SRYKBN  (IDX)        =   "70")  AND
                     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7" )  AND
                     (SPA-COM-DATAKBN (IDX)        =   "3" )
                   MOVE    1           TO  SPA-COM-HOUKATU-FLG2 (IDX)
               END-IF
           END-EVALUATE
      *
           IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" ) AND
                  (SPA-COM-DATAKBN (IDX)       =   "1" )
               IF     (SPA-COM-SRYKBN (IDX)    =   "60"  ) AND
                      (SPA-COM-KENTAICOMMENT(IDX)  =   1 )
      *            検体検査コメントは対象外
                   CONTINUE
               ELSE
                   MOVE    1               TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX)
               END-IF
           END-IF
      *    慢性維持透析患者管理料の画像診断
           IF     (WRK-KANRICD             =   "201"  )
           AND    (SPA-COM-SRYKBN (IDX)    =   "70" )
           AND    (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )
      *        診断料前の撮影料は包括へ
               IF     (SPA-COM-DATAKBN (IDX)       =   "1" )
               AND    (SPA-COM-TNSSRYSYUKBN(IDX)   =   "721" )
                   MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX)
                   MOVE    1               TO  SPA-COM-HOUKATU-FLG
                                                                (IDX)
               END-IF
      *        自動発生以外の加算は算定可
               IF     (SPA-COM-DATAKBN (IDX)   =   "2"   )
                 AND  (SPA-COM-AUTOKBN (IDX)   =   SPACE )
                   MOVE    1               TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX)
               END-IF
           END-IF
      *
           IF      SPA-COM-HOUKATU-FLG2 (IDX)  =   1
               IF      IDX-ERR             =   ZERO
                   MOVE    IDX                 TO  IDX-ERR
               END-IF
           END-IF
           .
       2002-HOUKATU-GAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括判定・チェック処理
      *****************************************************************
       2001-TAISYOU-CHK-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   TBL-MAX )
               MOVE    ZERO            TO  FLG-CHK
      *!!!!
      *        入外区分判定（入院料以外の入外区分別は外来のみチェック）
      *               入院算定不可のコードのみが対象の為。
      *               両方での算定可があれば対応が必要
               IF     (TBL-NYUGAIKBN (IDX)     =   "1" )
                 AND  (SPA-NYUGAIKBN           =   1   )
                   MOVE    2               TO  FLG-CHK
               END-IF
      *!!!!
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                       UNTIL      (IDX-Z       >   SPA-GMN-MAX )
                               OR (FLG-CHK NOT =   ZERO )
                   IF     (SPA-COM-HKNCOMBI (IDX-Z)        =   "9999")
                      OR  (SPA-COM-SRYKBN (IDX-Z)(1:1)     =   "9"   )
      *               治験対象外
                      OR  (SPA-COM-CHIKENKBN(IDX-Z)        =   "1"   )
      *               薬評対象外
                      OR  (SPA-COM-YAKUHYO  (IDX-Z)        =   "1"   )
                      OR  (SPA-COM-GENTEN   (IDX-Z)        =   "1"   )
      *               アフターケア
                      OR  (SPA-RSI-HKNKBN                  =   "3" )
                       CONTINUE
                    ELSE
      *                手技
                       IF     (SPA-COM-INPUTCD(IDX-Z)(1:1) =   "1" )
                           MOVE    SPA-COM-INPUTCD(IDX-Z)  TO  WRK-SRYCD
                           PERFORM 2001-HKTSRYCD-CHK-SEC
                           IF      FLG-CHK2            =   1
                               MOVE    1               TO  FLG-CHK
      *H29.9
      *                        月毎の包括チェック
                               IF      TBL-SANTEI-KBN(IDX) =   "3"
                                   PERFORM 2002-SANTEI03-CHEK-SEC
                               END-IF
      *
                           END-IF
                       ELSE
      *                外来診療料チェックは再診区分があれば対象
                           IF     (TBL-KANRICD (IDX)       =   "001")
                              AND (SPA-COM-SRYKBN (IDX-Z)  =   "12" )
      *                       一般病床数
                              AND  (SPA-BEDSUIPN        >=  200 )
                              AND  (SPA-HOSPSBT         =   1   )
                               MOVE    1               TO  FLG-CHK
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF     (FLG-CHK                 =   ZERO )
                  AND (TBL-SANTEI-KBN(IDX) NOT =   "1" )
      *            算定履歴による包括診療チェック
                   PERFORM 2003-HKTSRYCD-RRK-SEC
               END-IF
               IF      FLG-CHK             =   1
                   MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
                   MOVE    TBL-KANRICD (IDX)       TO  WRK-KANRICD
                   MOVE    TBL-SANTEI-HOU (IDX)    TO  WRK-SANTEI-HOU
                   MOVE    TBL-SANTEI-KBN (IDX)    TO  WRK-SANTEI-KBN
                   MOVE    TBL-NYUGAIKBN (IDX)     TO  WRK-NYUGAIKBN
                   MOVE    TBL-HKNCOMBIKBN (IDX)   TO  WRK-HKNCOMBIKBN
                   MOVE    TBL-SRYKBN-G    (IDX)   TO  WRK-SRYKBN-G
      *            入力項目の包括チェク
                   PERFORM 2002-HKTSANTEI-CHK-SEC
               END-IF
           END-PERFORM
      *
      *!!!!!!!!!
      *H27.9
      *    短期滞在手術基本料３の外来チェック
           IF     (WRK-TANKI3-IDX  NOT =   ZERO )
      *H30.10
      *****   AND (SPA-NYUGAIKBN       =   2    )
               MOVE    1                   TO  ORCSCHKTCHK-HKTFLG
               PERFORM 20013-TANKI3HKT-SEC
           END-IF
           .
       2001-TAISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括コードチェック処理
      *****************************************************************
       2001-HKTSRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   200   )  OR
                              (TBL-SRYCD (IDX IDY) =   SPACE)
               IF      WRK-SRYCD           =   TBL-SRYCD (IDX IDY)
                   MOVE    TBL-SRYCD (IDX IDY) TO  WRK-K-SRYCD
                   MOVE    1                   TO  FLG-CHK2
                   MOVE    200                 TO  IDY
               END-IF
           END-PERFORM
      *
           .
       2001-HKTSRYCD-CHK-EXT.
           EXIT.
      *
      *H29.9
      *****************************************************************
      *    月毎の包括チェックチェック処理
      *****************************************************************
       2002-SANTEI03-CHEK-SEC            SECTION.
      *
           IF     (ORCSCHKTCHK-PG      =   "K02" )
               IF      SPA-SYORIFLG       =   0
      *            新規
                   PERFORM 20021-SANTEI03-CHEK02-SEC
               ELSE
      *            訂正
                   PERFORM 20022-SANTEI03-CHEK03-SEC
               END-IF
           END-IF
           .
       2002-SANTEI03-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    月毎の包括チェックチェック処理
      *****************************************************************
       20021-SANTEI03-CHEK02-SEC            SECTION.
      *
      *    当月の受診履歴登録判定
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *    １日から月末まで
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    WRK-SRYYMD          TO  JYURRK-UPSRYYMD
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-JYURRK-CHK
           IF      SPA-SYORIFLG        =   1
      *        訂正
               PERFORM UNTIL      (FLG-JYURRK      =   1 )
                            OR    (FLG-JYURRK-CHK  =   1 )
                   IF     (SPA-DENPNUM         =   JYURRK-DENPNUM)
                      OR  (SPA-DENPNUM         =   JYURRK-GRP-DENPNUM)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-JYURRK-CHK
                   END-IF
                   MOVE    "key33"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               IF      FLG-JYURRK          =   ZERO
                   MOVE    1               TO  FLG-JYURRK-CHK
               END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK-CHK      =   1
               IF      SPA-COM-KZMFLG9  (IDX-Z)  =   SPACE
                   MOVE   "K300"           TO  SPA-ERRCD
                   MOVE   "1"              TO  SPA-COM-CUR
                                                      (IDX-Z)
                   MOVE   "1"              TO  SPA-COM-KZMFLG9
                                                      (IDX-Z)
               END-IF
           END-IF
      *
           .
       20021-SANTEI03-CHEK02-EXT.
           EXIT.
      *****************************************************************
      *    訂正時、包括診療チェック処理
      *****************************************************************
       20022-SANTEI03-CHEK03-SEC            SECTION.
      *
           MOVE    ZERO                    TO  FLG-TEISEI-ARI
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   (IDZ        >   SPA-MAX-LINE )  OR
                           (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                        OR (FLG-TEISEI-ARI =   1     )
               IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                   MOVE    1               TO  FLG-TEISEI-ARI
               END-IF
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   200   )  OR
                                  (TBL-SRYCD (IDX IDY) =   SPACE)
                                OR (FLG-TEISEI-ARI =   1     )
                   IF       LNK-ORCSC-TEI-SRYCD(IDZ)
                                               =   TBL-SRYCD (IDX IDY)
                       MOVE    1               TO  FLG-TEISEI-ARI
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    前回と同じ包括はチェックなし
           IF      FLG-TEISEI-ARI      =   ZERO
               PERFORM 20021-SANTEI03-CHEK02-SEC
           END-IF
      *
           .
       20022-SANTEI03-CHEK03-EXT.
           EXIT.
      *H29.9
      *
      *
      *****************************************************************
      *    算定履歴による包括診療チェック処理
      *****************************************************************
       2003-HKTSRYCD-RRK-SEC            SECTION.
      *
      *    外来まとめはパラメタで判定
           IF      ORCSCHKTCHK-KT01FLG =   "1"
               PERFORM 20030-KT01-HKTSRYCD-CHK-SEC
               GO      TO      2003-HKTSRYCD-RRK-EXT
           END-IF
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYM           TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL      (FLG-SANTEI      =   1  )  OR
                          (FLG-CHK         =   1  )
               MOVE    SANTEI-SRYCD            TO  WRK-SRYCD
               PERFORM 2001-HKTSRYCD-CHK-SEC
               IF     (SANTEI-MSANTEICNT   >   ZERO )
                 AND  (FLG-CHK2            =   1    )
      *            保険別の時、保険組合せ判定
                   IF      (TBL-HKNCOMBIKBN(IDX)   =   "1" )
                       AND (SANTEI-HKNCOMBINUM NOT =   SPA-HKNCOMBINUM)
                       CONTINUE
                   ELSE
                       PERFORM 20031-SANTEI-CHK-SEC
                   END-IF
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2003-HKTSRYCD-RRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来まとめの算定履歴による包括診療チェック処理
      *****************************************************************
       20030-KT01-HKTSRYCD-CHK-SEC            SECTION.
      *
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )
                        OR    (ORCSCHKTCHK-KT01-SRYCD (IDX-SN)
                                           =   SPACE)
                        OR    (FLG-CHK         =   1  )
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
               PERFORM VARYING     IDD         FROM    1   BY  1
                       UNTIL      (IDD         >   31  )
                   MOVE    ORCSCHKTCHK-KT01-DAY (IDX-SN IDD)
                                               TO  SANTEI-DAY (IDD)
                   ADD     SANTEI-DAY (IDD)    TO  SANTEI-MSANTEICNT
                   IF      SANTEI-DAY (IDD)    >   ZERO
                       IF      SANTEI-MSANTEID     =   SPACE
                           MOVE    IDD            TO  SANTEI-MSANTEID
                       END-IF
                   END-IF
               END-PERFORM
              MOVE    ORCSCHKTCHK-KT01-SRYCD  (IDX-SN)
                                               TO  WRK-SRYCD
               PERFORM 2001-HKTSRYCD-CHK-SEC
               IF     (SANTEI-MSANTEICNT   >   ZERO )
                 AND  (FLG-CHK2            =   1    )
      *            保険別の時、保険組合せ判定
      *            IF      (TBL-HKNCOMBIKBN(IDX)   =   "1" )
      *                AND (SANTEI-HKNCOMBINUM NOT =   SPA-HKNCOMBINUM)
      *                CONTINUE
      *            ELSE
                       PERFORM 20031-SANTEI-CHK-SEC
      *            END-IF
               END-IF
           END-PERFORM
           .
       20030-KT01-HKTSRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴の算定チェック処理
      *****************************************************************
       20031-SANTEI-CHK-SEC            SECTION.
      *
      *    訂正の時、当日算定有無の判定
           MOVE    ZERO                TO  WRK-TEI-CNT
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   (IDZ        >   SPA-MAX-LINE )  OR
                               (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-SRYDD           TO  IDD2
           EVALUATE    TBL-SANTEI-KBN(IDX)
               WHEN    "2"
      *            算定日
                   IF     (SANTEI-DAY (IDD2)   =   ZERO )  OR
                          (SANTEI-DAY (IDD2)   =   WRK-TEI-CNT)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-CHK
      *H27.8
      *            小児科外診療料算定日の労災・自賠責は対象外
                       IF     (TBL-KANRICD (IDX)   =   "202" )
                          AND (SPA-SYONIKA-ARI     =   ZERO  )
                          AND (SPA-HKNKBN          =   1     )
                           MOVE    ZERO            TO  FLG-CHK
                       END-IF
      *H30.8
      *            小児科かかりつけ診療料算定日の労災・自賠責は対象外
                       IF     (TBL-KANRICD (IDX)   =   "209" )
                          AND (SPA-SYONIKKR-ARI    =   ZERO  )
                          AND (SPA-HKNKBN          =   1     )
                           MOVE    ZERO            TO  FLG-CHK
                       END-IF
                   END-IF
               WHEN    "3"
      *            算定月
                   COMPUTE WRK-DAY             =   SANTEI-MSANTEICNT
                                               -   WRK-TEI-CNT
                   IF      WRK-DAY             >   ZERO
                       MOVE    1               TO  FLG-CHK
                   END-IF
               WHEN    "4"
      *            算定日以降
                   PERFORM VARYING     IDD     FROM    1   BY  1
                           UNTIL       IDD     >   IDD2
                       IF     (SANTEI-DAY (IDD)    >   ZERO )
                           IF      (IDD            =   IDD2 )  AND
                                   (SANTEI-DAY (IDD)    =   WRK-TEI-CNT)
                               CONTINUE
                           ELSE
                               MOVE    1               TO  FLG-CHK
                           END-IF
                           MOVE    IDD2                TO  IDD
                       END-IF
                   END-PERFORM
           END-EVALUATE
           .
       20031-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の包括チェク処理
      *****************************************************************
       2002-HKTSANTEI-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  HKTSANTEI-REC
           INITIALIZE                  HKTSANTEI-REC
           MOVE    WRK-KANRICD         TO  HKTSANTEI-KANRICD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  HKTSANTEI-YUKOEDYMD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    HKTSANTEI-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
           INITIALIZE                  TBL-HKTSANTEI-AREA
           MOVE    ZERO                TO  IDX-H
      *    電子点数表対象分から対象外
           IF      WRK-SRYYMD          >=  "20100401"
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           PERFORM UNTIL  (FLG-HKTSANTEI   =   1  )
                      OR  (IDX-H           >=  600 )
               ADD     1                   TO  IDX-H
               MOVE    HKTSANTEI-SRYCD     TO  TBL-HKTSRYCD (IDX-H)
               MOVE    HKTSANTEI-SANTEIKBN TO  TBL-SANTEIKBN (IDX-H)
               MOVE    HKTSANTEI-KENTAIKBN TO  TBL-KENTAIKBN (IDX-H)
               MOVE    "tbl_hktsantei"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKTSANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsantei"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  WRK-HKT-SRYKBN-G
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   13  )
               IF      WRK-SRYKBN-KBN (IDX-K)  =   "1"
                   EVALUATE    IDX-K
                       WHEN    1
                           MOVE    1           TO  WRK-HKTKBN20
                       WHEN    2
                           MOVE    1           TO  WRK-HKTKBN30
                       WHEN    3
                           MOVE    1           TO  WRK-HKTKBN40
                       WHEN    4
                           MOVE    1           TO  WRK-HKTKBN50
                       WHEN    5
                           MOVE    1           TO  WRK-HKTKBN60
                       WHEN    6
                           MOVE    1           TO  WRK-HKTKBN70
                       WHEN    7
                           MOVE    1           TO  WRK-HKTKBN80
                       WHEN    8
                           MOVE    1           TO  WRK-HKTKBN81
                       WHEN    9
                           MOVE    1           TO  WRK-HKTKBN82
                       WHEN    10
                           MOVE    1           TO  WRK-HKTKBN64
                       WHEN    11
                           MOVE    1           TO  WRK-HKTKBN13
                       WHEN    12
                           MOVE    1           TO  WRK-HKTKBN14
                       WHEN    13
                           MOVE    1           TO  WRK-HKTKBN10
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF      ORCSCHKTCHK-SYORI   =   "1"
                                       OR  "6"
      *        入力項目
               PERFORM 20021-INPUT-CHK-SEC
           ELSE
      *        自動発生
               PERFORM 30021-AUTO-CHK-SEC
           END-IF
           .
       2002-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目包括診療コードチェック処理
      *****************************************************************
       20021-INPUT-CHK-SEC            SECTION.
      *
      *    診療区分による包括
           MOVE    ZERO                TO  FLG-SANTEIOK
           MOVE    ZERO                TO  FLG-INPUTOK
           MOVE    ZERO                TO  FLG-INPUTOK2
           MOVE    ZERO                TO  FLG-INPUTOK6
           MOVE    ZERO                TO  FLG-INPUTOK7
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-HOUKATU-FLG(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-ERR(IDX-Z)  =   1     )
                   OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "#"
                                                   OR  "." 
                                                   OR  "$"   )
      *            治験対象外
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            薬評対象外
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            アフターケア
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1) =   "1"
                                                        OR  "6"
                                                        OR  "7" )
                       MOVE    1                   TO  FLG-INPUTOK
      *H26.11
      *                電子点数表からのチェックは、薬剤・器材のみ
                       IF     (TBL-HKTSRYCD (1)        =   SPACE )
                           IF      SPA-COM-INPUTCD (IDX-Z)(1:1) =   "6"
                                                                OR  "7"
                               MOVE    1               TO  FLG-INPUTOK6
                           END-IF
                       END-IF
                   END-IF
      *H24.8       （サ）、ユーザ器材
                   IF     (SPA-COM-INPUTCD (IDX-Z)     =   "820000046")
      ***************     (SPA-COM-INPUTCD (IDX-Z)(1:3)    = "059"  )
                       MOVE    1                   TO  FLG-INPUTOK
                       MOVE    1                   TO  FLG-INPUTOK2
                   END-IF
      *H30.1       ユーザ器材
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:3)    = "059"  )
                       MOVE    1                   TO  FLG-INPUTOK
                       MOVE    1                   TO  FLG-INPUTOK7
                   END-IF
      *H24.8
      *            剤点数がある剤のみ
                   IF     (SPA-COM-ZAIEND (IDX-Z)  =   "1"  )
      *************  AND  (SPA-COM-TENSU  (IDX-Z)  >   ZERO )
                     AND  (FLG-SANTEIOK            =   ZERO )
                     AND  (FLG-INPUTOK             =   1    )
      *H26.11
      *                電子点数表からのチェックは、薬剤・器材のみ
                       IF     (TBL-HKTSRYCD (1)        =   SPACE )
                         AND  (FLG-INPUTOK6            =   ZERO  )
                         AND  (FLG-INPUTOK7            =   ZERO  )
                           CONTINUE
                       ELSE
                           PERFORM 20022-SRYKBN-CHECK-SEC
                       END-IF
                   END-IF
               END-IF
      *        出来高算定判定
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-SANTEIOK 
                   MOVE    ZERO                TO  FLG-INPUTOK
                   MOVE    ZERO                TO  FLG-INPUTOK2
      *H30.1
                   MOVE    ZERO                TO  FLG-INPUTOK6
                   MOVE    ZERO                TO  FLG-INPUTOK7
               END-IF
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999903"
                   MOVE    1                   TO  FLG-SANTEIOK
               END-IF
           END-PERFORM
      *
      *    包括診療コードチェック
           MOVE    ZERO                TO  FLG-SANTEIOK
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-HOUKATU-FLG(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-ERR(IDX-Z)  =   1     )
                   OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "#"
                                                   OR  "$"   )
      *            治験対象外
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            薬評対象外
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            アフターケア
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1) =   "1" )
                   AND    (FLG-SANTEIOK        =   ZERO )
      *
                       MOVE    SPA-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       IF     (TBL-HKTSRYCD (1)        =   SPACE )
      *                    電子点数表からのチェック
                           PERFORM 20028-ETENSU-CHECK-SEC
                       ELSE
      *                    包括チェックマスタからのチェック
                           PERFORM 20021-SRYCD-CHECK-SEC
                       END-IF
                   END-IF
               END-IF
      *        出来高算定判定
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-SANTEIOK
               END-IF
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999903"
                   MOVE    1                   TO  FLG-SANTEIOK
               END-IF
           END-PERFORM
      *
      *    包括診療コード編集
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HOUKATU-KFLG (IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KFLG2(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KERR (IDX-Z)  =   1     )
                   MOVE    SPA-COM-HOUKATU-KFLG(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KFLG2(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG2 (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KERR(IDX-Z)
                                       TO  SPA-COM-HOUKATU-ERR (IDX-Z)
               END-IF
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG2
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KERR
                                                               (IDX-Z)
           END-PERFORM
           .
       20021-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括算定（剤）入力チェック処理
      *****************************************************************
       200213-HKTINPUT-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-HKTINP
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-SRYKBN   (IDX-Z)    =   "95"
                                                   OR  "96"
                                                   OR  "98"
                                                   OR  "99"  )
                   CONTINUE
               ELSE
      *            包括算定（剤）
                   IF     (SPA-COM-INPUTCD (IDX-Z) =   "099999908")
      *H28.9
      *        持参薬予約コードは包括剤とする
                    OR    (SPA-COM-INPUTCD (IDX-Z) =   CONS-JSNYCD1
                                                   OR  CONS-JSNYCD2
                                                   OR  CONS-JSNYCD3
                                                   OR  CONS-JSNYCD4
                                                   OR  CONS-JSNYCD5
                                                   OR  CONS-JSNYCD6
                                                   OR  CONS-JSNYCD7
                                                   OR  CONS-JSNYCD8)
      *
                       MOVE    1                   TO  FLG-HKTINP
      *                前行も包括とする
                       IF      SPA-GMN-INPUTCD (IDX-Z - 1) =   "."
                           MOVE    1           TO  SPA-COM-HOUKATU-FLG
                                                          (IDX-Z - 1)
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-FLG2
                                                          (IDX-Z - 1)
                           MOVE    ZERO        TO  SPA-COM-HOUKATU-KERR
                                                          (IDX-Z - 1)
                       END-IF
                   END-IF
                   IF      FLG-HKTINP          =   1
                       MOVE    1               TO  SPA-COM-HOUKATU-FLG
                                                                (IDX-Z)
                       MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2
                                                                (IDX-Z)
                       MOVE    ZERO            TO  SPA-COM-HOUKATU-KERR
                                                                (IDX-Z)
                   END-IF
               END-IF
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-HKTINP
               END-IF
           END-PERFORM
           .
       200213-HKTINPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コードチェック処理
      *****************************************************************
       20021-SRYCD-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
           PERFORM VARYING     IDX-H    FROM    1   BY  1
                   UNTIL      (IDX-H    >  600   )  OR
                              (TBL-HKTSRYCD(IDX-H) =   SPACE)
               IF      WRK-SRYCD           =   TBL-HKTSRYCD (IDX-H)
                   MOVE    1               TO  FLG-CHK2
                   IF      TBL-SANTEIKBN (IDX-H)   =   "1"
                       MOVE    2               TO  FLG-CHK2
                   END-IF
                   IF      TBL-KENTAIKBN (IDX-H)    =   "1"
                       MOVE    1               TO  FLG-CHK3
                   END-IF
                   MOVE    600             TO  IDX-H
               END-IF
           END-PERFORM
      *
      *    外来診療料で上の行が検体コード尿の場合のみ対象
           IF      FLG-CHK3            =   1
               IF    (WRK-KANRICD          =   "001")  AND
                     (FLG-CHK2             =   1    )
                   IF     (IDX-Z               >   1  )  AND
                          (SPA-COM-INPUTCD (IDX-Z - 1 )
                                               =   "160138910" )
                       MOVE    1               TO  FLG-CHK2
                   ELSE
                       MOVE    ZERO            TO  FLG-CHK2
                   END-IF
               ELSE
                   MOVE    ZERO                TO  FLG-CHK3
               END-IF
           END-IF
      *
      *    包括コード
           IF      FLG-CHK2            =   1
               IF      WRK-SANTEI-HOU      =   "1"
      *            会計データ作成
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               ELSE
      *            エラー
                   MOVE    1           TO  SPA-COM-HOUKATU-KERR(IDX-Z)
               END-IF
           END-IF
      *
           IF      FLG-CHK2            =   2
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KERR(IDX-Z)
      *        加算の場合
               IF     (SPA-COM-DATAKBN (IDX-Z)     =   "2" )
                  AND (SPA-COM-AUTOKBN (IDX-Z) NOT =   "2" )
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG2(IDX-Z)
               END-IF
           END-IF
      *
           .
       20021-SRYCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分による包括処理
      *****************************************************************
       20022-SRYKBN-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK3
      *    点数ゼロは包括外
      *    IF      SPA-COM-TENSU  (IDX-Z)    =   ZERO
      *        CONTINUE
      *    ELSE
      *
           EVALUATE    SPA-COM-SRYKBN (IDX-Z)(1:1)
      *        投薬
               WHEN    "2"
      *            院外
                   IF     (SPA-COM-SRYSYUKBN(IDX-Z)  =   "212"
                                                     OR  "213"
                                                     OR  "222"
                                                     OR  "223"
                                                     OR  "232"
                                                     OR  "233")
                     OR  ((SPA-COM-TENSU  (IDX-Z)    =   ZERO )  AND
                          (FLG-INPUTOK2              =   ZERO))
      *H28.4           院内は点数ゼロも包括対象
                       IF    (SPA-COM-SRYSYUKBN(IDX-Z)   =   "231"
                                                         OR  "211"
                                                         OR  "291"
                                                         OR  "221"
                                                         OR  "297"
                                                         OR  "294")
                           IF      WRK-HKTKBN20    =   1
                               MOVE    1           TO  FLG-CHK3
                           END-IF
                       END-IF
                   ELSE
                       IF      WRK-HKTKBN20    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "3"
                   IF      WRK-HKTKBN30    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "4"
                   IF      WRK-HKTKBN40    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "5"
                   IF      WRK-HKTKBN50    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "6"
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "60"
                       IF      WRK-HKTKBN60    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "64"
                       IF      WRK-HKTKBN64    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "7"
                   IF      WRK-HKTKBN70    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "8"
      *            処方せん料（投薬）
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "820"
                       IF      WRK-HKTKBN20    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            リハビリ（その他）
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "800"
                       IF      WRK-HKTKBN80    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            精神科専門
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "830"
                       IF      WRK-HKTKBN81    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            放射線治療
                   IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "840"
                       IF      WRK-HKTKBN82    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "1"
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "13"
      *            指導料
                       IF      WRK-HKTKBN13    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "14"
      *            在宅料
                       IF      SPA-COM-SRYSYUKBN (IDX-Z)   =   "147"
                                                           OR  "148"
                           CONTINUE
                       ELSE
                           IF      WRK-HKTKBN14    =   1
                               MOVE    1           TO  FLG-CHK3
                           END-IF
                       END-IF
                   END-IF
      *            初診・再診・入院
                   IF      SPA-COM-SRYKBN (IDX-Z)      =   "11"
                                                       OR  "12"
                                                       OR  "90"
                                                       OR  "92"
                       IF      WRK-HKTKBN10    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
           END-EVALUATE
      *
      ***  END-IF
      *
      *    包括コード
           IF      FLG-CHK3            =   1
      *        剤をすべて
               PERFORM VARYING     IDX-Z2  FROM    IDX-Z  BY  -1
                       UNTIL      (IDX-Z2  <   1   )
                   IF     (IDX-Z           NOT =   IDX-Z2  )  AND
                          (SPA-COM-ZAIEND (IDX-Z2) =   "1" )
                       MOVE    1                   TO  IDX-Z2
                   ELSE
                       IF      WRK-SANTEI-HOU      =   "1"
      *                会計データ作成
                           MOVE    1           TO  SPA-COM-HOUKATU-KFLG
                                                           (IDX-Z2)
                       ELSE
      *                エラー
                          MOVE    1           TO  SPA-COM-HOUKATU-KERR
                                                            (IDX-Z2)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       20022-SRYKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    電子点数表　包括チェック処理
      *****************************************************************
       20028-ETENSU-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
      *
      *    診療区分で包括の時のみ
           INITIALIZE                  ORCSCETNSCHKAREA
           MOVE    "1"                 TO  ORCSCETNSCHK-SYORI
           MOVE    WRK-SRYYMD          TO  ORCSCETNSCHK-SRYYMD
           MOVE    WRK-K-SRYCD         TO  ORCSCETNSCHK-SRYCD1
           MOVE    WRK-SRYCD           TO  ORCSCETNSCHK-SRYCD2
           CALL    "ORCSCETNSCHK"      USING
                                       ORCSCETNSCHKAREA
                                       SPA-AREA
           IF      ORCSCETNSCHK-CHKKBN =   ZERO
      *        包括対象外
               MOVE    2                   TO  FLG-CHK2
           END-IF
           IF      ORCSCETNSCHK-CHKKBN =   1
      *        包括あり
               MOVE    1                   TO  FLG-CHK2
           END-IF
           IF      ORCSCETNSCHK-KNT-SRYCD-G    NOT =   SPACE
      *         検体コードあり
               MOVE    1                   TO  FLG-CHK3
           END-IF
      *
      *    上の行が検体コードの場合のみ包括対象
           IF      FLG-CHK3            =   1
               IF     (IDX-Z               >   1  )
                 AND  (SPA-COM-KENTAICOMMENT (IDX-Z - 1)   =   1 )
      *            検体コード判定
                   PERFORM 200281-ETENSU-SMP-CHK-SEC
               ELSE
                   MOVE    2               TO  FLG-CHK2
               END-IF
           END-IF
      *    包括コード
           IF      FLG-CHK2            =   1
               IF      WRK-SANTEI-HOU      =   "1"
      *            会計データ作成
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               ELSE
      *            エラー
                   MOVE    1           TO  SPA-COM-HOUKATU-KERR(IDX-Z)
               END-IF
           END-IF
      *
      *    包括解除
           IF      FLG-CHK2            =   2
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KERR(IDX-Z)
      *        加算の場合
               IF     (SPA-COM-DATAKBN (IDX-Z)     =   "2" )
                  AND (SPA-COM-AUTOKBN (IDX-Z) NOT =   "2" )
                   MOVE    1           TO  SPA-COM-HOUKATU-KFLG2(IDX-Z)
               END-IF
           END-IF
      *
           .
       20028-ETENSU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検体コード判定処理
      *****************************************************************
       200281-ETENSU-SMP-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK4
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   10    )
                          OR  (ORCSCETNSCHK-KNT-SRYCD (IDX-K)
                                           =   SPACE  )
                          OR  (FLG-CHK4    =   1      )
               IF      SPA-COM-INPUTCD (IDX-Z - 1 )
                                           =   ORCSCETNSCHK-KNT-SRYCD
                                                              (IDX-K)
                   MOVE    1               TO  FLG-CHK4
               END-IF
           END-PERFORM
           IF      FLG-CHK4            =   1
               MOVE    1                   TO  FLG-CHK2
           ELSE
               MOVE    2                   TO  FLG-CHK2
           END-IF
           .
       200281-ETENSU-SMP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    短期滞在手術基本料３（外来）処理
      *****************************************************************
       20013-TANKI3HKT-SEC         SECTION.
      *
      *    包括処理
           MOVE    TBL-N-SANTEI-HOU (WRK-TANKI3-IDX)
                                       TO  WRK-SANTEI-HOU
           MOVE    TBL-N-SANTEI-KBN (WRK-TANKI3-IDX)
                                       TO  WRK-SANTEI-KBN
      *
           IF      ORCSCHKTCHK-SYORI   =   "1"
                                       OR  "6"
      *        入力項目
               PERFORM 200131-TANKI3INPUT-CHK-SEC
           ELSE
      *        自動発生
               PERFORM 200132-TANKI3AUTO-CHK-SEC
           END-IF
           .
       20013-TANKI3HKT-EXT.
           EXIT.
      *
      *****************************************************************
      *    短期滞在手術基本料３（外来）入力項目処理
      *****************************************************************
       200131-TANKI3INPUT-CHK-SEC         SECTION.
      *
      *    包括診療コードチェック
           MOVE    ZERO                TO  FLG-SANTEIOK
           MOVE    ZERO                TO  FLG-TANKI3HKT-NO
           MOVE    ZERO                TO  FLG-TANKI3HKT-OK
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-HOUKATU-FLG(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-ERR(IDX-Z)  =   1     )
                   OR (SPA-GMN-INPUTCD (IDX-Z)(1:1)
                                                   =   "#"
                                                   OR  "$"   )
      *            治験対象外
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            薬評対象外
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            アフターケア
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
      *            検査判断料のみのチェックとする
                   IF     (SPA-COM-INPUTCD (IDX-Z)(1:1) =   "1" )
                   AND    (FLG-SANTEIOK        =   ZERO )
                   AND    (SPA-COM-SRYKBN (IDX-Z)  =   "60"
      *H30.10            画像診断追加
                                                   OR  "70"     )
                   AND    (SPA-COM-DATAKBN(IDX-Z)  =   "1"      )
      *H30.10
                       MOVE    SPA-COM-SRYKBN (IDX-Z)
                                                   TO  WRK-CHK-SRYKBN
                       MOVE    SPA-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
      *                チェック
                       PERFORM 200131-TANKI3HKT-CHEK-SEC
                       IF      LNK-TANKI3HKT-KBN   =   "1"
                           IF      WRK-SANTEI-HOU      =   "1"
      *                        会計データ作成
                               MOVE    1       TO  SPA-COM-HOUKATU-KFLG
                                                               (IDX-Z)
                           ELSE
      *                        エラー
                               MOVE    1       TO  SPA-COM-HOUKATU-KERR
                                                               (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *        出来高算定判定
               IF      SPA-COM-ZAIEND (IDX-Z)  =   "1"
                   MOVE    ZERO                TO  FLG-SANTEIOK
               END-IF
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999903"
                   MOVE    1                   TO  FLG-SANTEIOK
               END-IF
           END-PERFORM
      *
      *    包括診療コード編集
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF     (SPA-COM-HOUKATU-KFLG (IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KFLG2(IDX-Z)  =   1     )
                   OR (SPA-COM-HOUKATU-KERR (IDX-Z)  =   1     )
                   MOVE    SPA-COM-HOUKATU-KFLG(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KFLG2(IDX-Z)
                                       TO  SPA-COM-HOUKATU-FLG2 (IDX-Z)
                   MOVE    SPA-COM-HOUKATU-KERR(IDX-Z)
                                       TO  SPA-COM-HOUKATU-ERR (IDX-Z)
               END-IF
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KFLG2
                                                               (IDX-Z)
               MOVE    ZERO                TO  SPA-COM-HOUKATU-KERR
                                                               (IDX-Z)
           END-PERFORM
           .
       200131-TANKI3INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    短期滞在手術基本料３処理
      *****************************************************************
       200131-TANKI3HKT-CHEK-SEC         SECTION.
      *
      *H30.1O
      *    H30.4から他入院料とコンピュータ断層診断等対象
           IF     (WRK-SRYYMD          <   "20180401")
             AND ((WRK-CHK-SRYKBN      =   "70" )
               OR (SPA-NYUGAIKBN       =   "1"  ))
               GO      TO      200131-TANKI3HKT-CHEK-EXT
           END-IF
      *
           INITIALIZE                  ORCSTANKI3HKT-AREA
      *    医療機関番号
           MOVE    SPA-HOSPNUM         TO  LNK-TANKI3HKT-HOSPNUM
      *    患者ＩＤ
           MOVE    SPA-PTID            TO  LNK-TANKI3HKT-PTID
      *    算定年月日
           MOVE    WRK-SRYYMD          TO  LNK-TANKI3HKT-SANTEIYMD
      *    診療行為コード
           MOVE    WRK-SRYCD           TO  LNK-TANKI3HKT-SRYCD
      *H29.4
           IF     (FLG-TANKI3HKT-OK    =   1  )
              AND (FLG-TANKI3HKT-NO    =   ZERO )
               MOVE    "1"                 TO  LNK-TANKI3HKT-HKTKBN
           END-IF
      *
      *
           CALL    "ORCSTANKI3HKT"     USING
                                       ORCSTANKI3HKT-AREA
                                       SPA-AREA
      *
           IF      LNK-TANKI3HKT-KBN2  =   "1"
               MOVE    1                   TO  FLG-TANKI3HKT-NO
           END-IF
           IF      LNK-TANKI3HKT-KBN   =   "1"
               MOVE    1                   TO  FLG-TANKI3HKT-OK
           END-IF
           .
       200131-TANKI3HKT-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    短期滞在手術基本料３（外来）自動発生処理
      *****************************************************************
       200132-TANKI3AUTO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TANKI3HKT-NO
           MOVE    ZERO                TO  FLG-TANKI3HKT-OK
           INITIALIZE                  WRK-ORCSC-KASAN
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                           NOT = SPACE     )
      *                治験対象外
                   AND    (LNK-ORCSC-CHIKENKBN (IDX-L1 IDX-L2)
                                               =   SPACE   )
      *            アフターケア
                   AND    (SPA-RSI-HKNKBN      NOT =   "3" )
                   AND    (LNK-ORCSC-HKTKBN  (IDX-L1 IDX-L2)
                                                   =   ZERO)
      *                診療コード包括
                       MOVE    LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                                   TO  WRK-SRYCD
      *                手技料のみチェック
                       IF     (WRK-SRYCD (1:1)     =   "1" )
                         AND  (LNK-ORCSC-SRYKBN (IDX-L1 IDX-L2)
                                                   =   "60"
      *H30.10            画像診断追加
                                                   OR  "70")
      *
      *H30.10
                           MOVE    LNK-ORCSC-SRYKBN (IDX-L1 IDX-L2)
                                                   TO  WRK-CHK-SRYKBN
      *                    チェック
                           PERFORM 200131-TANKI3HKT-CHEK-SEC
                           IF      LNK-TANKI3HKT-KBN   =   "1"
                               IF      WRK-SANTEI-HOU      =   "1"
      *                         会計データ作成
                                   MOVE    1       TO  WRK-ORCSC-HKTKBN
                                                      (IDX-L1 IDX-L2)
                               ELSE
      *                        エラー（削除）
                                   MOVE    SPACE   TO  LNK-ORCSC-SRYCD
                                                      (IDX-L1 IDX-L2)
                               END-IF
      *
                          END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    包括診療コード編集
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD  (IDX-L1 IDX-L2)
                                               NOT =   SPACE )  AND
                          (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                                   =   1     )
                       MOVE    WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           TO  LNK-ORCSC-HKTKBN
                                                       (IDX-L1 IDX-L2)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       200132-TANKI3AUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動発生分のみ処理
      *****************************************************************
       300-AUTO-SYORI-SEC         SECTION.
      *
      *    出来高判定
           MOVE    ZERO                TO  FLG-DEKIDAKA
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
      *        出来高（会計）があれば、すべて出来高
               IF      SPA-COM-INPUTCD (IDX-Z) =   "099999905"
                   MOVE    1                   TO  FLG-DEKIDAKA
               END-IF
           END-PERFORM
      *
           IF      FLG-DEKIDAKA        =   ZERO
      *    包括チェック判定
           IF      SPA-HKTSANTEI-FLG   =   1
               PERFORM 2001-TAISYOU-CHK-SEC
           END-IF
      *    入院料包括チェック判定
           IF      ORCSCHKTCHK-NYUHKTCHK   =   1
               PERFORM 600-NYUHKT-SYORI-SEC
           END-IF
      *
           END-IF
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動発生チェック処理
      *****************************************************************
       30021-AUTO-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-ORCSC-KASAN
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                           NOT = SPACE     )
      *                治験対象外
                   AND    (LNK-ORCSC-CHIKENKBN (IDX-L1 IDX-L2)
                                               =   SPACE   )
      *            アフターケア
                   AND    (SPA-RSI-HKNKBN      NOT =   "3" )
      *                診療区分による包括
                       PERFORM 30022-SRYKBN-CHECK-SEC
      *                診療コード包括
                       MOVE    LNK-ORCSC-SRYCD (IDX-L1 IDX-L2)
                                                   TO  WRK-SRYCD
      *                手技料のみチェック
                       IF      WRK-SRYCD (1:1)     =   "1"
      *
                       IF      TBL-HKTSRYCD (1)    =   SPACE
      *                    電子点数表からのチェック
                           PERFORM 30022-ETENSU-CHECK-SEC
                       ELSE
      *                    包括チェックマスタからのチェック
                           PERFORM 30021-SRYCD-CHECK-SEC
                       END-IF
      *
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    包括診療コード編集
           PERFORM VARYING     IDX-L1  FROM    1   BY  1
                   UNTIL      (IDX-L1      >   10  )
               PERFORM VARYING     IDX-L2      FROM    1   BY  1
                       UNTIL      (IDX-L2      >   30      )
                   IF     (LNK-ORCSC-SRYCD  (IDX-L1 IDX-L2)
                                               NOT =   SPACE )  AND
                          (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                                   =   1     )
                       MOVE    WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           TO  LNK-ORCSC-HKTKBN
                                                       (IDX-L1 IDX-L2)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       30021-AUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分による包括処理
      *****************************************************************
       30022-SRYKBN-CHECK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK3
           EVALUATE    LNK-ORCSC-SRYKBN (IDX-L1 IDX-L2)(1:1)
      *        投薬（外来はチェックなし）
               WHEN    "2"
                   IF      WRK-HKTKBN20    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "3"
                   IF      WRK-HKTKBN30    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "4"
                   IF      WRK-HKTKBN40    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "5"
                   IF      WRK-HKTKBN50    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "6"
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "60"
                       IF      WRK-HKTKBN60    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "64"
                       IF      WRK-HKTKBN64    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "7"
                   IF      WRK-HKTKBN70    =   1
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               WHEN    "8"
      *            処方せん料（投薬）
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "820"
                       IF      WRK-HKTKBN20    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "800"
                       IF      WRK-HKTKBN80    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "830"
                       IF      WRK-HKTKBN81    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYSYUKBN(IDX-L1 IDX-L2)
                                                   =   "840"
                       IF      WRK-HKTKBN82    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
               WHEN    "1"
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "13"
      *            指導料
                       IF      WRK-HKTKBN13    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "14"
      *            在宅料
                       IF      WRK-HKTKBN14    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
      *            初診・再診・入院
                   IF      LNK-ORCSC-SRYKBN(IDX-L1 IDX-L2) =   "11"
                                                       OR  "12"
                                                       OR  "90"
                                                       OR  "92"
                       IF      WRK-HKTKBN10    =   1
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    外来診療料は自動発生した判断料を算定
           IF    (WRK-KANRICD          =   "001")
               MOVE    ZERO                TO  FLG-CHK3
           END-IF
      *
           IF      FLG-CHK3            =   1
               IF      WRK-SANTEI-HOU      =   "1"
      *                会計データ作成
                   MOVE    1           TO  WRK-ORCSC-HKTKBN
                                                  (IDX-L1 IDX-L2)
               ELSE
      *                エラー（削除）
                   MOVE    2           TO  WRK-ORCSC-HKTKBN
                                                  (IDX-L1 IDX-L2)
               END-IF
           END-IF
      *
           .
       30022-SRYKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コードによる包括処理
      *****************************************************************
       30021-SRYCD-CHECK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
      *
           PERFORM VARYING     IDX-H    FROM    1   BY  1
                   UNTIL      (IDX-H    >  600   )  OR
                              (TBL-HKTSRYCD(IDX-H) =   SPACE)
               IF      WRK-SRYCD           =   TBL-HKTSRYCD (IDX-H)
                   MOVE    1               TO  FLG-CHK2
                   IF      TBL-SANTEIKBN (IDX-H)   =   "1"
                       MOVE    2               TO  FLG-CHK2
                   END-IF
                   IF      TBL-KENTAIKBN (IDX-H)    =   "1"
                       MOVE    1               TO  FLG-CHK3
                   END-IF
                   MOVE    600             TO  IDX-H
               END-IF
           END-PERFORM
      *
      *    外来診療料で上の行が検体コード尿の場合のみ対象
           IF      FLG-CHK3            =   1
               IF    (WRK-KANRICD          =   "001")  AND
                     (FLG-CHK2             =   1    )
                   IF     (IDX-Z               >   1  )  AND
                          (SPA-COM-INPUTCD (IDX-Z - 1 )
                                               =   "160138910" )
                       MOVE    1               TO  FLG-CHK2
                   ELSE
                       MOVE    ZERO            TO  FLG-CHK2
                   END-IF
               ELSE
                   MOVE    ZERO                TO  FLG-CHK3
               END-IF
           END-IF
      *
      *    包括コード
           IF      FLG-CHK2            =   2
      *        包括対象外
               MOVE    ZERO            TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
           ELSE
               IF     (FLG-CHK2            =   1  )
                   IF      WRK-SANTEI-HOU      =   "1"
      *                会計データ作成
                       MOVE    1           TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
                   ELSE
      *                エラー（削除）
                       MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
                   END-IF
               END-IF
      *        診療区分のエラーの削除
               IF     (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           =   2  )
      *            エラー（削除）
                   MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
               END-IF
           END-IF
           .
       30021-SRYCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    電子点数表　包括チェック処理
      *****************************************************************
       30022-ETENSU-CHECK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK2
           MOVE    ZERO                TO  FLG-CHK3
      *
      *    診療区分で包括の時のみ
           INITIALIZE                  ORCSCETNSCHKAREA
           MOVE    "1"                 TO  ORCSCETNSCHK-SYORI
           MOVE    WRK-SRYYMD          TO  ORCSCETNSCHK-SRYYMD
           MOVE    WRK-K-SRYCD         TO  ORCSCETNSCHK-SRYCD1
           MOVE    WRK-SRYCD           TO  ORCSCETNSCHK-SRYCD2
           CALL    "ORCSCETNSCHK"      USING
                                       ORCSCETNSCHKAREA
                                       SPA-AREA
           IF      ORCSCETNSCHK-CHKKBN =   ZERO
               MOVE    2                   TO  FLG-CHK2
           END-IF
           IF      ORCSCETNSCHK-CHKKBN =   1
               MOVE    1                   TO  FLG-CHK2
           END-IF
      *
      *    包括コード
           IF      FLG-CHK2            =   2
      *        包括対象外
               MOVE    ZERO            TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
           ELSE
               IF     (FLG-CHK2            =   1  )
                   IF      WRK-SANTEI-HOU      =   "1"
      *                会計データ作成
                       MOVE    1           TO  WRK-ORCSC-HKTKBN 
                                                  (IDX-L1 IDX-L2)
                   ELSE
      *                エラー（削除）
                       MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
                   END-IF
               END-IF
      *        診療区分のエラーの削除
               IF     (WRK-ORCSC-HKTKBN (IDX-L1 IDX-L2)
                                           =   2  )
      *            エラー（削除）
                   MOVE    SPACE       TO  LNK-ORCSC-SRYCD
                                                  (IDX-L1 IDX-L2)
               END-IF
           END-IF
      *
           .
       30022-ETENSU-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    エラー削除処理
      *****************************************************************
       400-ERR-DELETE-SEC         SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-COM-HOUKATU-ERR (IDX-Z) =   1  )
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD (IDX-Z)
               END-IF
           END-PERFORM
      *    診療種別だけの剤は削除
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
               IF     (SPA-GMN-INPUTCD (IDX-Z) (1:1)   =   "." )
                   COMPUTE IDX-Z3              =   IDX-Z   +  1
                   MOVE    ZERO                TO  FLG-ZAIARI
                   PERFORM VARYING     IDX-Z2  FROM    IDX-Z3  BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE)
                       IF      (SPA-GMN-INPUTCD (IDX-Z2)(1:1)
                                                   NOT =   "." ) AND
                               (SPA-GMN-INPUTCD (IDX-Z2) NOT =   SPACE)
                           MOVE    1               TO  FLG-ZAIARI
                       END-IF
                       IF      (SPA-COM-ZAIEND (IDX-Z2)    =   "1"  )
                           MOVE    SPA-MAX-LINE        TO  IDX-Z2
                       END-IF
                   END-PERFORM
                   IF      FLG-ZAIARI          =   ZERO
                       MOVE    SPACE       TO  SPA-GMN-INPUTCD (IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
           .
       400-ERR-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料算定判定処理
      *****************************************************************
       500-NYUIN-CHKSYORI-SEC         SECTION.
      *
      *    入院料未算定日取得
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-NYUINDAY-PTID
           MOVE    WRK-SRYYM           TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *    入院料算定なし
           IF      FLG-NYUINACCT       =   1
               MOVE    1                   TO  ORCSCHKTCHK-NYUERR
           ELSE
               MOVE    ZERO                TO  ORCSCHKTCHK-NYUERR
           END-IF
      *
           PERFORM UNTIL      (FLG-NYUINACCT     =   1)
      *
               PERFORM 5001-NYUINACT-SYORI-SEC
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療日時点の入院料と同じ日のみ対象とする
           MOVE    WRK-SRYDD           TO  IDX-D
           MOVE    SPACE               TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPACE               TO  ORCSCHKTCHK-NYUSRYCD
           IF      WRK-NYUHKTCD (IDX-D)    NOT =   "XXX"
               MOVE    WRK-NYUHKTCD (IDX-D)    TO  ORCSCHKTCHK-NYUHKTCD
               MOVE    WRK-N-SRYCD  (IDX-D)    TO  ORCSCHKTCHK-NYUSRYCD
           END-IF
      *    包括
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      WRK-NYUHKTCD (IDX)  =   WRK-NYUHKTCD (IDX-D)
                   MOVE    1               TO  ORCSCHKTCHK-NYUHKTFLG
                                                              (IDX)
               ELSE
                   MOVE    ZERO            TO  ORCSCHKTCHK-NYUHKTFLG
                                                              (IDX)
               END-IF
               IF      WRK-NYUHKTCD (IDX)  =   SPACE
                                           OR  "XXX"
                   CONTINUE
               ELSE
      *            月内に包括入院料がある時
                   MOVE    1               TO  ORCSCHKTCHK-NYUHKTCHK
               END-IF
           END-PERFORM
      *
      *    出来高算定日検索
           PERFORM 5009-SANTEI-CHK-SEC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      TBL-DEKIDAKA (IDX)  =   1
      *            出来高算定コード算定
                   MOVE    1               TO  ORCSCHKTCHK-DEKIDAFLG
                                                              (IDX)
               END-IF
           END-PERFORM
      *!!!
      *H29.1
      *    入院調剤料の為、翌月の入院料判定
           MOVE    ZERO                TO  ORCSCHKTCHK-NYUERR-2
           IF     (ORCSCHKTCHK-NYUERR      =   ZERO)
              AND (ORCSCHKTCHK-NYUHKTCHK   =   1  )
              PERFORM 5009-NEXTYMD-NYUINCHEK-SEC
           END-IF
           .
       500-NYUIN-CHKSYORI-EXT.
           EXIT.
      *
      *H29.1
      *****************************************************************
      *    翌月の入院料の判定処理
      *****************************************************************
       5009-NEXTYMD-NYUINCHEK-SEC         SECTION.
      *
      *    月末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SRYYM           TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-LST-DD
           ELSE
               MOVE    31                  TO  WRK-LST-DD
           END-IF
           IF     (WRK-TANKI-DAY   NOT =   ZERO )
             AND  (WRK-TANKI-DAY       =   WRK-LST-DD )
               GO    TO   5009-NEXTYMD-NYUINCHEK-EXT
           END-IF
      *
           MOVE    WRK-SRYYM           TO  WRK-N-SRYYM
           IF      WRK-N-SRYMM         =   12
               MOVE    1               TO  WRK-N-SRYMM
               ADD     1               TO  WRK-N-SRYYY
           ELSE
               ADD     1               TO  WRK-N-SRYMM
           END-IF
      *
      *    入院料未算定日取得
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-NYUINDAY-PTID
           MOVE    WRK-N-SRYYM         TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-N-SRYYM         TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *    入院料算定なし
           IF      FLG-NYUINACCT       =   1
               MOVE    1                   TO  ORCSCHKTCHK-NYUERR-2
           ELSE
               MOVE    ZERO                TO  ORCSCHKTCHK-NYUERR-2
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       5009-NEXTYMD-NYUINCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括入院料の判定処理
      *****************************************************************
       5001-NYUINACT-SYORI-SEC         SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-IDX-N
           PERFORM UNTIL     ( FLG-NYUINACT    =   1 )
               PERFORM VARYING     IDZ     FROM    1  BY  1
                       UNTIL      (IDZ         >   5  )  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE)
                   PERFORM 5001-SRYCD-CHK-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
      *        短期滞在手術基本料判定
               IF    ((WRK-IDX-N               >   ZERO  ) AND
                      (TBL-N-KANRICD (WRK-IDX-N)   =   "526"
                                                   OR  "525"))
                 AND ( LNK-NYUINDAY-TANKI2-KBN (IDX-D)
                                           NOT =   SPACE     )
                   MOVE    1               TO  NACCT-DAY (IDX-D)
      *
      *H29.1
                   MOVE    IDX-D           TO  WRK-TANKI-DAY
               END-IF
      *
               IF    ( NACCT-DAY (IDX-D)   NOT =   ZERO )
                   IF      WRK-IDX-N       >   ZERO
                       MOVE    TBL-N-KANRICD (WRK-IDX-N)
                                           TO  WRK-NYUHKTCD (IDX-D)
                       MOVE    WRK-NT-SRYCD
                                           TO  WRK-N-SRYCD (IDX-D)
                   ELSE
                       MOVE    "XXX"       TO  WRK-NYUHKTCD (IDX-D)
                   END-IF
               END-IF
           END-PERFORM
           .
       5001-NYUINACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括入院料の判定処理
      *****************************************************************
       5001-SRYCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-NT-SRYCD
           PERFORM VARYING     IDX-N   FROM    1  BY  1
                   UNTIL      (IDX-N       >   TBL-N-MAX )
                           OR (FLG-CHK     =   1         )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   200  )  OR
                                  (TBL-N-SRYCD (IDX-N IDY) =   SPACE)
                   IF      NSRY-SRYCD(IDZ) =   TBL-N-SRYCD (IDX-N IDY)
                       MOVE    TBL-N-SRYCD (IDX-N IDY)
                                                   TO  WRK-NT-SRYCD
                       MOVE    1                   TO  FLG-CHK
                       MOVE    IDX-N               TO  WRK-IDX-N
                       MOVE    200                 TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       5001-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出来高算定日検索チェック処理
      *****************************************************************
       5009-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-DEKIDAKA-AREA
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYM           TO  SANTEI-SRYYM
           MOVE    "099999904"         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    訂正の時、当日算定有無の判定
           MOVE    SANTEI-SRYCD        TO  WRK-SRYCD
           MOVE    WRK-SRYDD           TO  IDX-D
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   (IDZ        >   SPA-MAX-LINE )  OR
                               (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       MOVE    ZERO            TO  SANTEI-DAY (IDX-D)
                   END-IF
               END-PERFORM
           END-IF
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               MOVE    SANTEI-DAY (IDD)    TO  TBL-DEKIDAKA (IDD)
           END-PERFORM
      *
           .
       5009-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院期間チェック処理
      *****************************************************************
       600-NYUINDAYCHK-SEC         SECTION.
      *
      *    出来高算定日コード判定
           INITIALIZE                  TBL-DEKIDAKA-AREA
           PERFORM VARYING IDX-N       FROM    1   BY  1
                   UNTIL  (IDX-N       >   31  )
               MOVE    ORCSCHKTCHK-DEKIDAFLG (IDX-N)
                                           TO  TBL-DEKIDAKA  (IDX-N)
           END-PERFORM
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
               IF      SPA-COM-INPUTCD  (IDX-Z)    =   "099999904"
                   PERFORM VARYING IDX-N       FROM    1   BY  1
                           UNTIL  (IDX-N       >   31  )
                       IF      SPA-COM-NYUINDAY(IDX-Z IDX-N)
                                                   >   ZERO
                           MOVE    1           TO  TBL-DEKIDAKA  (IDX-N)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           MOVE    WRK-SRYDD           TO  IDX-D
           IF      ORCSCHKTCHK-NYUHKTCD    NOT =   SPACE
      *        診療日が入院料包括で出来高算定日の時、出来高とする
               IF     (TBL-DEKIDAKA  (IDX-D)   =   1  )
                   MOVE    SPACE               TO  ORCSCHKTCHK-NYUHKTCD
                   MOVE    SPACE               TO  ORCSCHKTCHK-NYUSRYCD
                   PERFORM VARYING IDX-N       FROM    1   BY  1
                           UNTIL  (IDX-N       >   31  )
                       IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)   =  1
                           IF     (TBL-DEKIDAKA  (IDX-N)   =   ZERO )
                               MOVE    ZERO    TO  ORCSCHKTCHK-NYUHKTFLG
                                                            (IDX-N)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *        包括算定日から出来高算定日へ
               PERFORM VARYING IDX-N       FROM    1   BY  1
                       UNTIL  (IDX-N       >   31  )
                   IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)   =  ZERO
                       IF     (TBL-DEKIDAKA  (IDX-N)   =   1  )
                           MOVE    1       TO  ORCSCHKTCHK-NYUHKTFLG
                                                          (IDX-N)
                       END-IF
                    END-IF
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                TO  WRK-ERRDAY
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  )
                          OR  (SPA-ERRCD   NOT =   SPACE    )
               IF     (SPA-COM-HKNCOMBI (IDX-Z)    =   "9999")
                   OR (SPA-COM-SRYKBN   (IDX-Z)    =   "95"
                                                   OR  "96"
                                                   OR  "99"
                                                   OR  "98"  )
      *            治験対象外
                   OR (SPA-COM-CHIKENKBN(IDX-Z)    =   "1"   )
      *            薬評対象外
                   OR  (SPA-COM-YAKUHYO (IDX-Z)    =   "1"   )
                   OR  (SPA-COM-GENTEN  (IDX-Z)    =   "1"   )
      *            アフターケア
                   OR  (SPA-RSI-HKNKBN             =   "3" )
                   CONTINUE
               ELSE
                   IF      SPA-GMN-INPUTCD (IDX-Z)(1:1)  =   "*"
                       PERFORM VARYING IDX-N       FROM    1   BY  1
                               UNTIL  (IDX-N       >   31  )
                                   OR (SPA-ERRCD   NOT =   SPACE )
                          IF      SPA-COM-NYUINDAY(IDX-Z IDX-N)
                                                   >   ZERO
                               IF      ORCSCHKTCHK-NYUHKTFLG (IDX-N)
                                                       =   ZERO
                                   MOVE    IDX-N       TO  WRK-ERRDAY
                                   MOVE    "0218"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       END-PERFORM
                       IF      SPA-ERRCD       NOT =   SPACE
                           MOVE    "1"         TO  SPA-COM-CUR (IDX-Z)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  AND
                  (WRK-ERRDAY          >   ZERO  )
               MOVE    SPACE               TO  SPA-ERRMSG2
               STRING  WRK-ERRDAY
                       "日"                DELIMITED  BY  SIZE
                       INTO                SPA-ERRMSG2
               END-STRING
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        包括区分クリア
               PERFORM 602-HOUKATU-INIT-SEC
               IF      ORCSCHKTCHK-NYUHKTCD    NOT =   SPACE
      *            入院料包括チェック
                   PERFORM 600-NYUHKT-SYORI-SEC
               END-IF
           END-IF
           .
       600-NYUINDAYCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括区分クリア処理
      *****************************************************************
       602-HOUKATU-INIT-SEC         SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-GMN-MAX  ) 
               MOVE    ZERO            TO  SPA-COM-HOUKATU-KFLG(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-FLG2(IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ZAI (IDX-Z)
               MOVE    ZERO            TO  SPA-COM-HOUKATU-ERR (IDX-Z)
           END-PERFORM
           .
       602-HOUKATU-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料包括チェック処理
      *****************************************************************
       600-NYUHKT-SYORI-SEC         SECTION.
      *
           MOVE    1                   TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   TBL-N-MAX )
              IF       TBL-N-KANRICD (IDY) =   ORCSCHKTCHK-NYUHKTCD
                   MOVE    IDY                 TO  IDX
                   MOVE    TBL-N-MAX           TO  IDY
               END-IF
           END-PERFORM
           MOVE    1                       TO  ORCSCHKTCHK-HKTFLG
           MOVE    TBL-N-KANRICD (IDX)     TO  WRK-KANRICD
           MOVE    TBL-N-SANTEI-HOU (IDX)  TO  WRK-SANTEI-HOU
           MOVE    TBL-N-SANTEI-KBN (IDX)  TO  WRK-SANTEI-KBN
           MOVE    TBL-N-NYUGAIKBN (IDX)   TO  WRK-NYUGAIKBN
           MOVE    TBL-N-HKNCOMBIKBN (IDX) TO  WRK-HKNCOMBIKBN
           MOVE    TBL-N-SRYKBN-G    (IDX) TO  WRK-SRYKBN-G
      *    包括対象入院料コード
           MOVE    ORCSCHKTCHK-NYUSRYCD    TO  WRK-K-SRYCD
      *    入力項目の包括チェク
           PERFORM 2002-HKTSANTEI-CHK-SEC
      *    剤の編集
           PERFORM 2002-ZAIHENSYU-SEC
           .
       600-NYUHKT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療コード読み込み
      *****************************************************************
       910-HKTSRYCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
               MOVE    ZERO            TO  FLG-HKTSRYCD
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       910-HKTSRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    包括診療コード読み込み
      *****************************************************************
       910-HKTSANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKTSANTEI-REC
               MOVE    ZERO            TO  FLG-HKTSANTEI
           ELSE
               INITIALIZE                  HKTSANTEI-REC
               MOVE    1               TO  FLG-HKTSANTEI
           END-IF
      *
           .
       910-HKTSANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院会計マスター次読込
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       930-NYUINACT-RAED-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       930-NYUINACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
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
