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
       PROGRAM-ID.                 ORCSC80200204.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（その他）
      *                      Ｈ１４．４改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/16  労災用追加
      * 01.00.02     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.03     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.04     NACL-多々納  03/03/19  老人の労災加算の修正
      * 01.00.05     NACL-多々納  03/09/22  家族通院精神療法算定追加
      * 01.00.06     NACL-多々納  03/10/10  リハビリチェック変更
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
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
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-TONPUK          PIC 9(01).
           03  FLG-GAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-ZAI             PIC 9(01).
           03  FLG-NAIYAK2         PIC 9(01).
           03  FLG-TONPUK2         PIC 9(01).
           03  FLG-GAIYAK2         PIC 9(01).
      *
           03  FLG-YAKJYO          PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    施設基準不適合
           03  FLG-KIJUNN          PIC 9(01).
      *    早期リハビリあり
      *****03  FLG-SOUKI           PIC 9(01).
           03  FLG-SOUKI-ALL       PIC 9(01).
           03  FLG-SOUKI-END       PIC 9(01).
           03  FLG-SOUKI-ARI       PIC 9(01).
           03  FLG-SOUKI-ARI1      PIC 9(01).
           03  FLG-SOUKI-ARI2      PIC 9(01).
           03  FLG-SANTEI-ARI      PIC 9(01).
      *    手技料計算フラグ
           03  FLG-SYUGI           PIC 9(01).
      *    労災用
           03  FLG-RIHA            PIC 9(01).
      *    労災加算あり
           03  FLG-ROU-ARI         PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
      *    リハビリあり
           03  FLG-RIGAKU          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDY2                PIC 9(02).
      *
           03  IDD                 PIC 9(02).
      *
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  HZN-IDX             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDX-R1              PIC 9(04).
      *
           03  IDX-RS              PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-Z2              PIC 9(04).
           03  IDX-RSI             PIC 9(04).
      *
           03  IDX-HZN             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
      *    減額診療コード（労災）
           03  WRK-GEN-SRYCD       PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-DAYCNT          PIC 9(03).
           03  WRK-TUKI-CNT        PIC 9(03).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC S9(08).
           03  WRK-TENSUKEI-G      PIC S9(08).
           03  WRK-TENSUKEI-K      PIC S9(08).
           03  WRK-TENSUKEI-RSI    PIC S9(08).
           03  WRK-TENGAKEI-G      PIC S9(10).
           03  WRK-TENGAKEI-K      PIC S9(10).
      *
           03  WRK-GOK-TENSUKEI        PIC S9(10).
           03  WRK-GOK-TENGAKEI        PIC S9(10).
      *
           03  WRK-IDX             PIC 9(04).
      *
           03  WRK-RIGAKU-KBN      PIC X(01).
           03  WRK-RIGAKU-RYO      PIC X(01).
           03  WRK-RIGAK-CNTALL    PIC 9(03).
      *
      *
      *    理学最大単位
           03  WRK-RIGAK-MAX       PIC 9(03).
      *
           03   WRK-SYOYMD.
                05   WRK-SYO-YY    PIC 9(04).
                05   WRK-SYO-MM    PIC 9(02).
                05   WRK-SYO-DD    PIC 9(02).
      *    診療日
           03   WRK-SRYYMD         PIC X(08).
      *
      *    自賠責金額用
           03  WRK-KINGAK          PIC 9(08).
           03  FLG-JIHI            PIC 9(01).
      *
           03  WRK-NYU-KAI         PIC 9(03).
      *
      *    リハビリ回数集計
        01  WRK-RIGAKU-NCNT-AREA.
            03  WRK-RIGAKU-INT              PIC X(01).
            03  WRK-ORCSC-RIGAKU-TBL.
      *        １:理学、２：作業、３：言語
               05  WRK-DCNT-TBL        OCCURS  3.
      *        集団
                   07  WRK-DCNT-SN-G.
                       09  WRK-DCNT-SN     PIC 9(03)   OCCURS  31.
      *            当日
                   07  WRK-TCNT-S          PIC 9(03).
      *            月計
                   07  WRK-TCNT-SN         PIC 9(03).
      *        個別
                   07  WRK-DCNT-KN-G.
                       09  WRK-DCNT-KN     PIC 9(03)   OCCURS  31.
                   07  WRK-TCNT-K          PIC 9(03).
                   07  WRK-TCNT-KN         PIC 9(03).
      *        早期リハ等加算（逓減対象外）
           03  WRK-GAICNT-G.
                   07  WRK-GCNT            PIC 9(01)   OCCURS  31.
      *    早期リハ等加算（今回入力分）
           03  WRK-ORCSC-GAI-FLG-G.
               05  WRK-ORCSC-GAI-FLG           PIC 9(01)   OCCURS  31.
      *****03  WRK-RIGAKU-INT              PIC X(01).
      *
      *    保存用
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *    リハビリの早期リハ加算テーブル
           COPY   "CMRIHAKSNTBL.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ（施設基準）
           COPY    "CPSK1006.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
      *    COPY    "CPTENSU.INC".
      *
      *    算定履歴
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
      *
      *    チェックマスタ−
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    リハビリ当月算定カウントパラメタ
           COPY    "CPORCSC80CNT.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           MOVE    LNK-RIGAKU-NCNT-AREA    TO  WRK-RIGAKU-NCNT-AREA
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           END-IF
      *    初回に当月分編集
           IF      LNK-ORCSC-PG        =   "K02"
               IF      WRK-RIGAKU-INT      =   SPACE
                   INITIALIZE                  WRK-RIGAKU-NCNT-AREA
      *            リハビリの回数編集
                   INITIALIZE                  ORCSC80CNTAREA
                   MOVE    LNK-ORCSC-SYORIFLG  TO  ORCSC80CNT-SYORIFLG
                   CALL    "ORCSC80CNT"        USING
                                               SPA-AREA
                                               ORCSC00AREA
                                               ORCSC80CNTAREA
                   MOVE    ORCSC80CNT-RIGAKU-AREA
                                           TO  WRK-ORCSC-RIGAKU-TBL
                   MOVE    "1"             TO  WRK-RIGAKU-INT
               END-IF
           ELSE
               INITIALIZE              WRK-RIGAKU-NCNT-AREA
           END-IF
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
           MOVE    WRK-RIGAKU-NCNT-AREA    TO  LNK-RIGAKU-NCNT-AREA
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )  OR
                  (LNK-ORCSC-KIDCD NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
      *    剤内に早期リハビリ存在チェック
      **** PERFORM 10010-SOUKI-CHK-SEC
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *        労災加算振替え
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "099509901"
                   MOVE    "1"                 TO  WRK-MOJI
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            乳幼児の自動加算を追加する
                   PERFORM 10011-NENREI-SET-SEC
      *            施設基準不適合チェック
                   IF      WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                               NOT =   ZERO
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *            労災の時、労災加算の算定
                   IF      SPA-HKNKBN          =   1
                       PERFORM 100113-RSIHKN-SYORI-SEC
                   ELSE
                       IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
      *                    理学療法の逓減チェック
                           MOVE    SPACE               TO  WRK-GEN-SRYCD
                           IF      LNK-ORCSC-PG        =   "K02"
                               PERFORM 100112-RIGAKU-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
      *
      *            精神通院の逓減チェック
                   IF      LNK-ORCSC-PG        =   "K02"
                       PERFORM 100113-SEISIN-CHK-SEC
                   END-IF
      *
      *            加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *        器材
               WHEN    "7"
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                フィルムの時エラー
                       MOVE    "0047"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適合チェック加算処理
      *****************************************************************
       100111-KIJUNN-CHK-SEC           SECTION.
      *
      *    手技の時、適合チェック
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2  OR  3
               PERFORM 1001111-SSTKIJUN-CHK-SEC
      *        施設基準適応
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-KIJUNN
                   GO      TO      100111-KIJUNN-CHK-EXT
               END-IF
      *        施設基準不適応
               IF     (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2 )  OR
                     ((WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   3   ) AND
                      (LNK-ORCSC-NENREI-YY             =   ZERO))
                   MOVE    2                   TO  FLG-KIJUNN
      *            施設基準不適合（放射線）（他の加算はないこととする）
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    "180025270"         TO  WRK-SRYCD
                   IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                       CONTINUE
                   ELSE
                       MOVE    IDX-Z           TO  IDX-S
                       MOVE    ZERO            TO  FLG-ARI
                       PERFORM 1002-KASAN-SET-SEC
                  END-IF
              END-IF
           END-IF
      *
      *    加算の時、対象の手技が有ること
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   1
               IF      FLG-KIJUNN          =   ZERO
                   MOVE    "0073"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100111-KIJUNN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適応チェック処理
      *****************************************************************
       1001111-SSTKIJUN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "1"                 TO  SYS-1006-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    基準不適合逓減対象施設基準
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
           IF      SYS-1006-SSTKIJUN (IDY) =   1
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-ERR
           END-IF
      *
           .
       1001111-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    理学療法の逓減チェック処理
      *****************************************************************
       100112-RIGAKU-CHK-SEC            SECTION.
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      100112-RIGAKU-CHK-EXT
           END-IF
      *
      *
           SET     TBL-SAN             TO  1
           SEARCH      TBL-SANTEI-OCC  VARYING   TBL-SAN
                   AT  END
                       MOVE    ZERO            TO  FLG-RIGAKU
                       MOVE    SPACE           TO  WRK-RIGAKU-KBN
                       MOVE    SPACE           TO  WRK-RIGAKU-RYO
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-SANTEI-SRYCD
                                                           (TBL-SAN)
                       MOVE    TBL-RIGAKU-KBN   (TBL-SAN)
                                               TO  WRK-RIGAKU-KBN
                       MOVE    TBL-RIGAKU-RYO   (TBL-SAN)
                                               TO  WRK-RIGAKU-RYO
                       MOVE    1               TO  FLG-RIGAKU
           END-SEARCH
           IF      FLG-RIGAKU          =   ZERO
               GO      TO      100112-RIGAKU-CHK-EXT
           END-IF
      *
           IF     (SPA-NYUGAIKBN       =   "2" ) OR
                  (LNK-ORCSC-SYORIFLG  =   1   )
      *        外来か訂正の時の日上限と月上限のチェックと自動逓減
               PERFORM 2002-GAIRAI-ALL-CHK-SEC
           ELSE
      *        入院の日上限と月上限のチェックと自動逓減
               PERFORM 2001-NYUIN-ALL-CHK-SEC
           END-IF
      *
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *
           .
       100112-RIGAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     精神通院の逓減チェック処理
      *****************************************************************
       100113-SEISIN-CHK-SEC            SECTION.
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      100113-SEISIN-CHK-EXT
           END-IF
      *
      *
           IF      WRK-COM-INPUTCD (IDX-Z) =   "180012210"  OR
                                               "180012310"  OR
                                               "180020410"  OR
                                               "180020570"
      *             家族
                                           OR  "180007250"
                                           OR  "180007350"
               CONTINUE
           ELSE
               GO     TO      100113-SEISIN-CHK-EXT
           END-IF
      *
           IF      LNK-ORCSC-SYORIFLG      =   1
      *        訂正時点の初診算定日
               MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
           ELSE
               MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
           END-IF
      *    初診算定日から６月以内
           IF     (WRK-SYOYMD          =   SPACE  )  OR
                  (LNK-ORCSC-SYOSIN2   =   1      )
               MOVE    SPA-SRYYMD          TO  WRK-SYOYMD
           END-IF
      *    初診料算定日＋６月後
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
      *****MOVE    LNK-ORCSC-SYOYMD    TO  LNK-DAY6-KIJUN
           MOVE    WRK-SYOYMD          TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    6                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-SYOYMD
      *
      *
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
               WHEN    "180012210"
               WHEN    "180012310"
               WHEN    "180020410"
      *             家族
               WHEN    "180007250"
               WHEN    "180007350"
                   IF      SPA-SRYYMD          <=  WRK-SYOYMD
      *            逓減あり
                       MOVE    IDX-Z               TO  IDX-S
                       MOVE    "180020570"         TO  WRK-SRYCD
                       IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                           CONTINUE
                       ELSE
                           MOVE    IDX-Z           TO  IDX-S
                           MOVE    ZERO            TO  FLG-ARI
                           PERFORM 1002-KASAN-SET-SEC
                       END-IF
                   END-IF
               WHEN    "180020570"
      *            逓減チェック
                   IF      WRK-COM-INPUTCD (IDX-Z - 1)
                                               =   "180012210"  OR
                                                   "180012310"
                                               OR  "180020410"
      *             家族
                                               OR  "180007250"
                                               OR  "180007350"
                       IF      SPA-SRYYMD      >   WRK-SYOYMD
                           MOVE    "0082"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR
                                                           (IDX-Z)
                       END-IF
                   ELSE
                       MOVE    "0083"      TO  WRK-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       100113-SEISIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災加算の算定処理
      *****************************************************************
       100113-RSIHKN-SYORI-SEC        SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
      *
      *    理学療法の逓減は、傷病発生３月以降
           IF     (WRK-COM-DATAKBN (IDX-Z) =   "1"  )
      *        減額診療コード（労災）
               MOVE    "101800030"         TO  WRK-GEN-SRYCD
      *        理学療法の逓減チェック
               IF      LNK-ORCSC-PG        =   "K02"
                   PERFORM 100112-RIGAKU-CHK-SEC
                   MOVE    HZN-IDX             TO  IDX-Z
               END-IF
           END-IF
      *
      *    早期リハビリ加算は対象外
           MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
           PERFORM 20011-SOUKI-CHK-SEC
           IF      FLG-SOUKI           =   1
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    労災の時、加算なし
           IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101"
      *        四肢加算チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "101800010" 
                   MOVE    2               TO  WRK-COM-SANFLG (IDX-Z)
               END-IF
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
           MOVE    SPACE                TO  WRK-SRYCD
           MOVE    ZERO                 TO  FLG-ROUSAI
      *    労災用　区分番号
           IF      WRK-COM-CDKBN-KBN (IDX-Z)  =   "H"
               EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX-Z)
                   WHEN    "001"
                   WHEN    "002"
                   WHEN    "003"
                   WHEN    "004"
                   WHEN    "005"
                       EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
                           WHEN    2
      *                        １．５倍
                               MOVE    "101800010"      TO  WRK-SRYCD
                       END-EVALUATE
                       MOVE    1                    TO  FLG-ROUSAI
               END-EVALUATE
           END-IF
      *    老人のコードでリハビリは対象とする
           IF      FLG-ROUSAI          =   ZERO
               MOVE    WRK-COM-INPUTCD (IDX-Z)     TO  WRK-SRYCD2
               PERFORM 10011312-RIGAKU-SERCH-SEC
               IF      FLG-RIGAKU          =   1
                   EVALUATE    SPA-RSI-SISIKBN
                       WHEN    1
                       WHEN    2
      *                    １．５倍とする
                           MOVE    "101800010"     TO  WRK-SRYCD
                   END-EVALUATE
                   MOVE    1                    TO  FLG-ROUSAI
               END-IF
           END-IF
      *
      *    既に他の労災加算が入力されている時は自動算定しない
           MOVE    ZERO                TO  FLG-ROU-ARI
           COMPUTE IDX-Z2              =   IDX-Z     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ROU-ARI             =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   "101800010"  OR
                                                   "099509901"
                   IF      WRK-COM-AUTOKBN (IDX1)  =   SPACE
                        MOVE    1                  TO  FLG-ROU-ARI
                   END-IF
               END-IF
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                      (WRK-COM-DATAKBN (IDX1)      =  "1" )
                  MOVE    SPA-MAX-LINE             TO  IDX1
               END-IF
           END-PERFORM
           IF      FLG-ROU-ARI         =   1
               IF      FLG-ROUSAI          =   ZERO
                   MOVE    "8004"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX1 - 1)
               END-IF
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
           IF      WRK-SRYCD           =   SPACE
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    加算料チェック処理（次ぎの手技料の前に）
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           COMPUTE IDX-Z2              =   IDX-Z     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1            TO  IDX-T
                   MOVE    1               TO  FLG-ARI
               ELSE
                   IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                          (WRK-COM-DATAKBN (IDX1)      =  "2" )
                      MOVE    IDX1             TO  IDX-T
                   END-IF
                   IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                          (WRK-COM-DATAKBN (IDX1)      =  "1" )
                      MOVE    SPA-MAX-LINE             TO  IDX1
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               IF      IDX-T           =   ZERO
                   MOVE    IDX-Z           TO  IDX-S
               ELSE
                   MOVE    IDX-T           TO  IDX-S
               END-IF
           END-IF
      *
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    2               TO  WRK-COM-SANFLG (IDX-S)
           END-IF
           .
       100113-RSIHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    理学療法テーブル検索処理
      *****************************************************************
       10011312-RIGAKU-SERCH-SEC            SECTION.
      *
      *
           SET     TBL-SAN             TO  1
           SEARCH      TBL-SANTEI-OCC  VARYING   TBL-SAN
                   AT  END
                       MOVE    SPACE           TO  WRK-RIGAKU-RYO
                       MOVE    SPACE           TO  WRK-RIGAKU-KBN
                       MOVE    ZERO            TO  FLG-RIGAKU
                   WHEN    WRK-SRYCD2          =   TBL-SANTEI-SRYCD
                                                           (TBL-SAN)
                       MOVE    TBL-RIGAKU-KBN   (TBL-SAN)
                                               TO  WRK-RIGAKU-KBN
                       MOVE    TBL-RIGAKU-RYO   (TBL-SAN)
                                               TO  WRK-RIGAKU-RYO
                       MOVE    1               TO  FLG-RIGAKU
           END-SEARCH
      *
           .
       10011312-RIGAKU-SERCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       10011-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        加算料チェック処理
               PERFORM 100111-KASAN-CHK-SEC
      *        加算料追加処理
               MOVE    IDX-Z               TO  IDX-S
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10011-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       100111-KASAN-CHK-SEC        SECTION.
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       100111-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料追加処理
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    自動追加分（入力時に追加）
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           ADD     1                   TO  WRK-GMN-MAX
      *
           MOVE    1                   TO  FLG-ADD
           ADD     1                   TO  IDX-Z
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
      *                自賠責
                       PERFORM 10021-JIHI-KEI-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    薬剤点数計算
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-MAX             TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    割合加算
      *****PERFORM 1006-KASAN-SET-SEC
      *    手技料の計算
           IF      IDX-SYU             >   ZERO
               MOVE    2               TO  FLG-SYUGI
               COMPUTE IDX-Z           =   IDX-MAX    +  1
               PERFORM 100212-TENSUKEI-SEC
           END-IF
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    器材点数
           MOVE    LNK-SC90-KIZTEN     TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *    点数計　計算
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *    セット用合計点数
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責計算処理
      *****************************************************************
       10021-JIHI-KEI-SEC                  SECTION.
      *
           IF      WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591"  OR
                                                   "09592"  OR
                                                   "09593"  OR
                                                   "09594"
               CONTINUE
           ELSE
               GO      TO      10021-JIHI-KEI-EXT
           END-IF
      *
      *    自賠責明細料・診断料
           COMPUTE WRK-COM-KISOTEN(IDX-Z)  =   WRK-COM-TEN (IDX-Z)
                                           /   10
      *
           ADD     WRK-COM-KISOTEN (IDX-Z) TO  LNK-SC90-TENGAKEI
      *    回数
           MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU (IDX-Z)
      *
           MOVE    SPACE                   TO  WRK-COM-KEIFLG (IDX-Z)
      *
           .
       10021-JIHI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               IF      IDX-SYU             >   ZERO
      *            手技料の計算
                   MOVE    1               TO  FLG-SYUGI
                   PERFORM 100212-TENSUKEI-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *****手技料判定
      *    IF      (WRK-MOJI                   =   "1"  )  AND
      *            (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
      *        MOVE    IDX-Z               TO  IDX-SYU
      *****END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100212-TENSUKEI-SEC             SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
      *    手技料の合計を編集する
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN2 (IDX-SYU)
      *
      *    労災の外来管理加算読み替え処理
           IF      SPA-HKNKBN          =   1
      *!       回数を掛けた点数で判定する
      *!         （２単位以上の時、読み替えはないはず）
      *!       COMPUTE WRK-SYOKEI      =   WRK-COM-SYUTEN2 (IDX-SYU)
      *!                               *   LNK-ORCSC-KAISU
               MOVE    WRK-COM-SYUTEN2 (IDX-SYU)
                                       TO  WRK-SYOKEI
               IF     (WRK-COM-RSIGIRI (IDX-SYU)
                                           =   1  )  AND
                      (WRK-SYOKEI          <   LNK-ORCSC-GAIRAI-TEN)
                   MOVE     LNK-ORCSC-GAIRAI-TEN
                                           TO  LNK-SC90-TENSUKEI
               END-IF
           END-IF
      *
      *
           ADD     LNK-SC90-TENSUKEI   TO  WRK-GOK-TENSUKEI
           ADD     LNK-SC90-TENGAKEI   TO  WRK-GOK-TENGAKEI
           MOVE    ZERO                TO  LNK-SC90-TENSUKEI
           MOVE    ZERO                TO  LNK-SC90-TENGAKEI
      *
      *    最後の手技料時に、以下の計算を行う
           IF      FLG-SYUGI           =   2
               MOVE    WRK-GOK-TENSUKEI    TO  LNK-SC90-TENSUKEI
               MOVE    WRK-GOK-TENGAKEI    TO  LNK-SC90-TENGAKEI
           END-IF
      *
           .
       100212-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *        労災を対象外とする
      **     IF      WRK-COM-INPUTCD(IDX)(1:3)       =   "101"
      *        CONTINUE
      **     ELSE
      *        手技料の点数を所定点数とする
      *        ％加算
             IF       (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101") AND
                      (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *        ％減算（労災減算も計算する）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
      *******END-IF
           END-PERFORM
      *
      *    労災の時、労災加算
           IF      SPA-HKNKBN          =   1
               PERFORM 10061-RSIKASAN-SET-SEC
           END-IF
      *
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災割合加算計算処理
      *****************************************************************
       10061-RSIKASAN-SET-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-RSI
           MOVE    ZERO                TO  IDX-RSI
      *    労災加算対象点数集計
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *
      *        手技料の点数を所定点数とする
      *        ％加算
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   2   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =  (WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               -   WRK-TENSUKEI-G )
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-RSI
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-RSI
      *
           .
       10061-RSIKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の日上限と月上限のチェックと自動逓減処理
      *****************************************************************
       2002-GAIRAI-ALL-CHK-SEC                SECTION.
      *
      *    厚生労働大臣が定める患者のチェック
           PERFORM 200221-SOUKIKSN-CHK-SEC
      *
           MOVE    SPA-SRYYMD(7:2)    TO  IDD
           MOVE    SPA-SRYYMD         TO  WRK-SRYYMD
      *
           MOVE    ZERO               TO  WRK-TUKI-CNT
           MOVE    ZERO               TO  WRK-DAYCNT
      *
      *    当日上限チェック
           MOVE    WRK-COM-KAISU(IDX-Z)   TO  WRK-KAISU
           PERFORM 20021-DAYMAX-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      2002-GAIRAI-ALL-CHK-EXT
           END-IF
      *
      *    労災の時、理学療法の逓減は傷病発生３月以降とする
           IF     (SPA-HKNKBN          =   1        )  AND
                  (SPA-SRYYMD          <   SPA-RSI-SHOBYOYMD3)
      *        今回分集計
               PERFORM 20023-KAISU-HEN-SEC
               GO      TO      2002-GAIRAI-ALL-CHK-EXT
           END-IF
      *
      *    月上限判定
           EVALUATE    WRK-RIGAKU-RYO
      *        集団
               WHEN    "S"
                   MOVE    ZERO                TO  WRK-TUKI-CNT
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   IDD
                       COMPUTE WRK-TUKI-CNT    =   WRK-TUKI-CNT
                                               +   WRK-DCNT-SN
                                                          (IDX-R1 IDY)
                   END-PERFORM
      *            １月８単位まで
                   IF     (WRK-TUKI-CNT    +   WRK-KAISU)
                                           >   8
                       MOVE    "0084"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       STRING  "既に"          DELIMITED  BY  SIZE
                                WRK-TUKI-CNT
                                               DELIMITED  BY  SIZE
                                "単位算定済です。"
                                               DELIMITED  BY  SIZE
                                           INTO    LNK-ORCSC-ERRMSG
                       END-STRING
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      2002-GAIRAI-ALL-CHK-EXT
                   END-IF
      *        個別
               WHEN    "K"
      *            早期リハ加算等がある場合
                   IF      FLG-SOUKI-ARI       =   1
      *                今回分集計
                       PERFORM 20023-KAISU-HEN-SEC
                       GO      TO      2002-GAIRAI-ALL-CHK-EXT
                   END-IF
      *            早期リハ加算分を引く
      *            当月の算定回数
                   MOVE    ZERO                TO  WRK-TUKI-CNT
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   IDD
                       IF     (WRK-GCNT (IDY)      =   ZERO    )  AND
                              (WRK-ORCSC-GAI-FLG (IDY) =   ZERO)
                           COMPUTE WRK-TUKI-CNT    =   WRK-TUKI-CNT
                                                   +   WRK-DCNT-KN
                                                          (IDX-R1 IDY)
                       END-IF
                   END-PERFORM
      *            １月１１単位から逓減
                   IF      WRK-TUKI-CNT        >=  10
      *                逓減あり
                       PERFORM 20024-RIGAKU-TEIGEN-SEC
                   ELSE
                       IF    ((WRK-TUKI-CNT    +   WRK-KAISU)
                                               >   10  )  AND
                             (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                             (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                               (IDX-Z)
                           MOVE    "0081"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                           STRING  "既に"          DELIMITED  BY  SIZE
                                   WRK-TUKI-CNT
                                                   DELIMITED  BY  SIZE
                                   "単位算定済です。"
                                                   DELIMITED  BY  SIZE
                                           INTO    LNK-ORCSC-ERRMSG
                           END-STRING
                           MOVE    "1"             TO  WRK-COM-CUR
                                                           (IDX-Z)
                           GO      TO      2002-GAIRAI-ALL-CHK-EXT
                       ELSE
      *                    逓減以外で逓減コードがある時
                           PERFORM 20025-TEIGEN-CHK-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *    今回分集計
           PERFORM 20023-KAISU-HEN-SEC
      *
           .
       2002-GAIRAI-ALL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    当日上限チェック処理
      *****************************************************************
       20021-DAYMAX-CHK-SEC         SECTION.
      *
      *    当日上限チェック
           EVALUATE    WRK-RIGAKU-KBN
      *        理学
               WHEN    "R"
                   MOVE    1                   TO  IDX-R1
      *        作業
               WHEN    "S"
                   MOVE    2                   TO  IDX-R1
      *        言語
               WHEN    "G"
                   MOVE    3                   TO  IDX-R1
      *        エラ−
               WHEN    OTHER
                   MOVE    ZERO                TO  IDX-R1
                   GO      TO      20021-DAYMAX-CHK-EXT
           END-EVALUATE
           EVALUATE    WRK-RIGAKU-RYO
      *        個別
               WHEN    "K"
      *            １日３単位まで
                   IF     (WRK-DCNT-KN (IDX-R1 IDD)    +   WRK-KAISU)
                                           >   3
                       MOVE    "0092"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      20021-DAYMAX-CHK-EXT
                   END-IF
      *            同一日集団と混在しないこと
                   IF      WRK-DCNT-SN (IDX-R1 IDD)    >   ZERO
                       MOVE    "0093"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      20021-DAYMAX-CHK-EXT
                   END-IF
      *        集団
               WHEN    "S"
      *            １日２単位まで
                   IF     (WRK-DCNT-SN (IDX-R1 IDD)    +   WRK-KAISU)
                                           >   2
                       MOVE    "0092"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      20021-DAYMAX-CHK-EXT
                   END-IF
      *            同一日個別と混在しないこと
                   IF      WRK-DCNT-KN (IDX-R1 IDD)    >   ZERO
                       MOVE    "0093"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      20021-DAYMAX-CHK-EXT
                   END-IF
           END-EVALUATE
      *    １日合計単位チェック
           COMPUTE WRK-RIGAK-CNTALL    =   WRK-DCNT-KN (1 IDD)
                                       +   WRK-DCNT-KN (2 IDD)
                                       +   WRK-DCNT-KN (3 IDD)
                                       +   WRK-DCNT-SN (1 IDD)
                                       +   WRK-DCNT-SN (2 IDD)
                                       +   WRK-DCNT-SN (3 IDD)
                                       +   WRK-KAISU
      *
      *    当日に早期リハ加算等がある時、早期リハありとする
           IF     (FLG-SOUKI-ARI       =   1    )
               OR (WRK-GCNT (IDD)      >   ZERO )
               OR (WRK-ORCSC-GAI-FLG(IDD)  >   ZERO )
      *        早期リハビリ加算あり１日最大６単位とする
               MOVE    6               TO  WRK-RIGAK-MAX
           ELSE
      *       １日最大４単位とする
               MOVE    4               TO  WRK-RIGAK-MAX
           END-IF
           IF      WRK-RIGAK-CNTALL    >   WRK-RIGAK-MAX
      *        ４単位以上の時は警告とする
               IF     (WRK-RIGAK-MAX       =   4  )  AND
                      (WRK-RIGAK-CNTALL    <=  6  )
                  IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE) AND
                         (LNK-ORCSC-KEI-IDX        =   ZERO )
                       MOVE    "K092"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       MOVE    "1"             TO  WRK-COM-KZMFLG(IDX-Z)
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                   END-IF
               ELSE
                   MOVE    "0092"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
               GO      TO      20021-DAYMAX-CHK-EXT
           END-IF
      *
           .
       20021-DAYMAX-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回分集計処理
      *****************************************************************
       20023-KAISU-HEN-SEC         SECTION.
      *
           EVALUATE    WRK-RIGAKU-RYO
      *        個別
               WHEN    "K"
                   ADD     WRK-KAISU       TO  WRK-DCNT-KN (IDX-R1 IDD)
                   ADD     WRK-KAISU       TO  WRK-TCNT-K (IDX-R1)
      *
      *        集団
               WHEN    "S"
                   ADD     WRK-KAISU       TO  WRK-DCNT-SN (IDX-R1 IDD)
                   ADD     WRK-KAISU       TO  WRK-TCNT-S (IDX-R1)
           END-EVALUATE
      *    早期リハビリ他
           IF      FLG-SOUKI-ARI       =   1
               ADD     1                   TO  WRK-ORCSC-GAI-FLG (IDD)
           END-IF
      *
           .
       20023-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    早期リハビリ、外来移行加算の算定チェック処理
      *****************************************************************
       200221-SOUKIKSN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SOUKI-ARI
      *
           PERFORM VARYING     IDX-R   FROM    1   BY  1
                   UNTIL      (IDX-R       >   SPA-MAX-LINE )   OR
                              (FLG-SOUKI-ARI   =   1   )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE   AND
                               WRK-COM-INPUTCD (IDX-Z) =   SPACE )
               IF      (WRK-COM-INPUTCD (IDX-R)(1:1)    =   "1" )  AND
                       (WRK-COM-DATAKBN (IDX-R)         =   "2" )
      *            厚生労働大臣が定める患者のチェック
                   MOVE    WRK-COM-INPUTCD (IDX-R) TO  WRK-SRYCD
                   PERFORM 20011-SOUKI-CHK-SEC
                   IF      FLG-SOUKI           =   1
                       MOVE    1               TO  FLG-SOUKI-ARI
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200221-SOUKIKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別療法逓減処理（共通）
      *****************************************************************
       20024-RIGAKU-TEIGEN-SEC            SECTION.
      *
      *    理学療法の逓減は、傷病発生３月以降
           IF      SPA-HKNKBN          =   1
      *        減額診療コード（労災）
               MOVE    "101800030"         TO  WRK-SRYCD
           ELSE
               IF      LNK-ORCSC-ROUJIN    =   ZERO
                   MOVE    "180025370"         TO  WRK-SRYCD
               ELSE
                   MOVE    "180709070"         TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    逓減ＯＫの処理
           IF     (WRK-COM-KZMFLG2 (IDX-Z) =   SPACE )  AND
                  (WRK-COM-INPUTCD (IDX-Z + 1) NOT =   WRK-SRYCD)
               IF      LNK-ORCSC-KEI-IDX   =   ZERO
                   MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"             TO  WRK-COM-KZMFLG2 (IDX-Z)
               END-IF
               MOVE    "0180"          TO  LNK-ORCSC-KIDCD
               GO      TO      20024-RIGAKU-TEIGEN-EXT
           END-IF
      *
      *    リハビリ逓減確認
           IF      LNK-ORCSC-FLG-RIHTEI    =   ZERO
               MOVE    "1"             TO  WRK-COM-KZMFLG (IDX-Z)
               GO      TO      20024-RIGAKU-TEIGEN-EXT
           END-IF
      *
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               CONTINUE
           ELSE
               MOVE    IDX-Z           TO  IDX-S
               MOVE    ZERO            TO  FLG-ARI
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD         =   1
      *            自動追加にしない
                   MOVE    SPACE           TO  WRK-COM-AUTOKBN (IDX-S)
               END-IF
           END-IF
      *
           .
       20024-RIGAKU-TEIGEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減以外で逓減コードの判定処理
      *****************************************************************
       20025-TEIGEN-CHK-SEC         SECTION.
      *
           IF      WRK-COM-INPUTCD (IDX-Z + 1)
                                       =   "101800030"  OR
                                           "180025370"  OR
                                           "180709070"
               IF     (WRK-COM-KZMFLG (IDX-Z + 1)  =   SPACE )  AND
                      (LNK-ORCSC-KEI-IDX           =   ZERO  )
                   MOVE    "1"         TO  WRK-COM-KZMFLG (IDX-Z + 1)
                   MOVE    "K082"      TO  SPA-ERRCD
                   MOVE    "1"         TO  WRK-COM-CUR (IDX-Z + 1)
                   COMPUTE LNK-ORCSC-KEI-IDX   =   IDX-Z   +   1
               END-IF
           END-IF
      *
           .
       20025-TEIGEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院全体チェック処理
      *****************************************************************
       2001-NYUIN-ALL-CHK-SEC         SECTION.
      *
      *    厚生労働大臣が定める患者のチェック
           PERFORM 200221-SOUKIKSN-CHK-SEC
           MOVE    FLG-SOUKI-ARI       TO  FLG-SOUKI-ARI1
           MOVE    ZERO                TO  WRK-NYU-KAI
      *
      *    入力日分のチェック
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                   ADD     1                   TO  WRK-NYU-KAI
                   MOVE    IDX2                TO  IDD
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   MOVE    IDD                 TO  WRK-SRYYMD (7:2)
      *
      *            当日上限チェック
                   MOVE    WRK-COM-KAISU(IDX-Z)   TO  WRK-KAISU
      *            当日に早期リハ加算がある時、ありとする
                   IF     (WRK-GCNT (IDD)          >   ZERO) OR
                          (WRK-ORCSC-GAI-FLG (IDD) >   ZERO )
                       MOVE    1                   TO  FLG-SOUKI-ARI2
                   ELSE
                       MOVE    ZERO                TO  FLG-SOUKI-ARI2
                   END-IF
                   IF      (FLG-SOUKI-ARI1         =   1  )  OR
                           (FLG-SOUKI-ARI2         =   1  )
                       MOVE    1                   TO  FLG-SOUKI-ARI
                   ELSE
                       MOVE    ZERO                TO  FLG-SOUKI-ARI
                   END-IF
      *
                   PERFORM 20021-DAYMAX-CHK-SEC
      *
                   IF      SPA-ERRCD           =   SPACE
      *                月上限チェック（入院）
                       PERFORM 20012-NTUKIMAX-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-NYUIN-ALL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院月上限チェック処理
      *****************************************************************
       20012-NTUKIMAX-CHK-SEC         SECTION.
      *
      *    労災の時、理学療法の逓減は傷病発生３月以降とする
           IF     (SPA-HKNKBN          =   1        )  AND
                  (WRK-SRYYMD          <   SPA-RSI-SHOBYOYMD3)
      *        今回分集計
               PERFORM 20023-KAISU-HEN-SEC
               GO      TO      20012-NTUKIMAX-CHK-EXT
           END-IF
      *
      *    月上限判定
           EVALUATE    WRK-RIGAKU-RYO
      *        集団
               WHEN    "S"
                   MOVE    ZERO                TO  WRK-TUKI-CNT
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   IDD
                       COMPUTE WRK-TUKI-CNT    =   WRK-TUKI-CNT
                                               +   WRK-DCNT-SN
                                                          (IDX-R1 IDY)
                   END-PERFORM
      *            １月８単位まで
                   IF     (WRK-TUKI-CNT    +   WRK-KAISU )
                                               >   8
                       MOVE    "0084"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       STRING  "既に"          DELIMITED  BY  SIZE
                               IDD             DELIMITED  BY  SIZE
                               "日までに"
                                               DELIMITED  BY  SIZE
                                WRK-TUKI-CNT
                                               DELIMITED  BY  SIZE
                                "単位算定済です。"
                                               DELIMITED  BY  SIZE
                                           INTO    LNK-ORCSC-ERRMSG
                       END-STRING
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      20012-NTUKIMAX-CHK-EXT
                   END-IF
      *        個別
               WHEN    "K"
      *            早期リハ加算等がある場合
                   IF      FLG-SOUKI-ARI       =   1
      *                今回分集計
                       PERFORM 20023-KAISU-HEN-SEC
                       GO      TO      20012-NTUKIMAX-CHK-EXT
                   END-IF
      *            早期リハ加算分を引く
      *            当月の算定回数
                   MOVE    ZERO                TO  WRK-TUKI-CNT
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   IDD
                       IF     (WRK-GCNT (IDY)      =   ZERO) AND
                              (WRK-ORCSC-GAI-FLG (IDY)
                                                   =   ZERO)
                           COMPUTE WRK-TUKI-CNT    =   WRK-TUKI-CNT
                                                   +   WRK-DCNT-KN
                                                          (IDX-R1 IDY)
                       END-IF
                   END-PERFORM
      *            １月１１単位から逓減
                   IF      WRK-TUKI-CNT        >=  10
      *                逓減あり
                       MOVE    IDX-Z               TO  IDX-HZN
                       PERFORM 20024-RIGAKU-TEIGEN-SEC
                       IF      FLG-ADD             =   1
                           MOVE    IDX-HZN             TO  IDX-Z
                       END-IF
                   ELSE
                       IF    ((WRK-TUKI-CNT    +   WRK-KAISU)
                                               >=  10  )  AND
                             (WRK-COM-KZMFLG (IDX-Z)   =   SPACE) AND
                             (LNK-ORCSC-KEI-IDX        =   ZERO )
      *                    月上限単位警告編集処理
                           PERFORM 200121-NYUTUKI-HEN-SEC
                           IF      LNK-ORCSC-KEI-IDX   NOT =   ZERO
                               GO      TO      20012-NTUKIMAX-CHK-EXT
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *    今回分集計
           PERFORM 20023-KAISU-HEN-SEC
      *
           .
       20012-NTUKIMAX-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    月上限単位警告編集処理
      *****************************************************************
       200121-NYUTUKI-HEN-SEC      SECTION.
      *
      *    診療日が１つで今回の回数で１０単位になる時、警告しない
           IF   ((WRK-TUKI-CNT     +   WRK-KAISU)
                                   =  10          ) AND
                 (WRK-COM-NYU-KAI (IDX-Z)  =   WRK-NYU-KAI)
               GO      TO      200121-NYUTUKI-HEN-EXT
           END-IF
      *
           MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
           MOVE    "1"                 TO  WRK-COM-KZMFLG (IDX-Z)
           MOVE    "K081"              TO  SPA-ERRCD
           MOVE    SPACE               TO  LNK-ORCSC-ERRMSG
           STRING  "既に"              DELIMITED  BY  SIZE
                   IDD                 DELIMITED  BY  SIZE
                   "日までに"          DELIMITED  BY  SIZE
                   WRK-TUKI-CNT        DELIMITED  BY  SIZE
                   "単位算定済です。"  DELIMITED  BY  SIZE
                                       INTO    LNK-ORCSC-ERRMSG
           END-STRING
           MOVE    "1"                 TO  WRK-COM-CUR  (IDX-Z)
           .
       200121-NYUTUKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    早期リハビリ加算処理
      *****************************************************************
       20011-SOUKI-CHK-SEC         SECTION.
      *
      *    早期リハビリ加算
           SET     TBL-RIDX            TO  1
           SEARCH      TBL-RIHABIRI-OCC    VARYING   TBL-RIDX
                   AT  END
                       MOVE    ZERO            TO  FLG-SOUKI
                   WHEN    WRK-SRYCD           =   TBL-RIHA-SRYCD
                                                           (TBL-RIDX)
                       MOVE    1               TO  FLG-SOUKI
           END-SEARCH
      *
           .
       20011-SOUKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
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
       990-DBCLOSE-SEC           SECTION.
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
      *
