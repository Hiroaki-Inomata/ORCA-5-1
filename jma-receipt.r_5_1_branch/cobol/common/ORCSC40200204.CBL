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
       PROGRAM-ID.                 ORCSC40200204.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（処置）
      *                      Ｈ１４．４改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/16  労災用追加
      * 01.00.02     MCC-多々納   02/04/30  消炎鎮痛処置チェック変更
      * 01.00.03     MCC-多々納   02/06/12  時間外特例追加
      * 01.00.04     MCC-多々納   02/07/09  労災加算の入力を可能とする
      * 01.00.05     NACL-多々納  02/08/20  労災チェック追加
      * 01.00.06     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.07     NACL-多々納  03/05/21  注射液残量廃棄追加
      * 01.00.08     NACL-多々納  05/12/14  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
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
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    施設基準不適合
           03  FLG-KIJUNN          PIC 9(01).
      *    労災加算あり
           03  FLG-ROU-ARI         PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-ZS              PIC 9(04).
           03  IDX-ZS2             PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SANSO           PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-TBL             PIC 9(04).
      *
           03  IDX-Z2              PIC 9(04).
           03  HZN-IDX             PIC 9(04).
      *
           03  IDX-HAI             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-SRYCD           PIC X(09). 
           03  WRK-TIMEFLG         PIC 9(01). 
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
           03  WRK-TENSUKEI-K      PIC 9(08).
      *
           03  WRK-SANSO           PIC 9(08).
           03  WRK-SANSOKIN        PIC 9(08)V99.
           03  WRK-SANKEI          PIC 9(08).
           03  WRK-SANTEN          PIC 9(08).
      *
           03  WRK-SANSOKIN-SUM    PIC 9(08)V99.
      *
      *    手技料合計
           03  WRK-GOK-TENSUKEI    PIC 9(09).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9999.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ERRCD           PIC X(04).
           03  WRK-ERRCD2          PIC X(04).
           03  WRK-IDX-ERR         PIC 9(04).
      *    消炎鎮痛処理
           03  WRK-SYOCHI-KBN      PIC X(02).
      *
      *    消炎鎮痛処理
           03  WRK-DAY-CNT         PIC 9(04).
           03  WRK-TEI-CNT         PIC 9(04).
      *    消炎鎮痛処理（１月）
           03  WRK-DAY-CNT-TBL.
               05  WRK-DAY         PIC 9(03)
                                   OCCURS   31.
      *
      *
           03  WRK-INPUTCD         PIC X(22).
      *
      *    保存用
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *    消炎鎮痛処置テーブル
       01  TBL-SYOCHI-AREA.
           03  FILLER              PIC X(12)   VALUE   "01 140029610".
           03  FILLER              PIC X(12)   VALUE   "02 140040310".
           03  FILLER              PIC X(12)   VALUE   "03 140002210".
           03  FILLER              PIC X(12)   VALUE   "03 140002310".
       01  TBL-SYOCHI-AREA-R       REDEFINES   TBL-SYOCHI-AREA.
           03  TBL-SYOCHI-OCC      OCCURS   4  INDEXED  TBL-IDX.
               05  TBL-KBN         PIC X(02).
               05  TBL-FILLER      PIC X(01).
               05  TBL-SRYCD       PIC X(09).
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
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
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
                  (SPA-ERRCD       NOT =   SPACE )
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
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-SANSO
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) OR
                              (WRK-ERRCD       NOT =   SPACE)
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
      *    消炎鎮痛処置のエラーセット
           IF      WRK-ERRCD2      NOT =   SPACE
               IF      WRK-IDX-ERR     >   ZERO
                  MOVE    "1"              TO  WRK-COM-CUR (WRK-IDX-ERR)
                  MOVE    WRK-ERRCD2       TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    酸素の補正率を自動発生する
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               PERFORM 10011-SANSO-ADD-SEC
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-ZS              >   ZERO )
      *        乳幼児、時間外の自動加算を追加する
               PERFORM 1002-JIDO-KASAN-SEC
           END-IF
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
      *            手技料 加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *            手技料
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       IF      IDX-ZS          >   ZERO
      *                    乳幼児、時間外の自動加算を追加する
                           PERFORM 1002-JIDO-KASAN-SEC
                       END-IF
                       MOVE    IDX-Z           TO  IDX-ZS
                       MOVE    IDX-Z           TO  IDX-ZS2
                   END-IF
      *            施設基準不適合チェック
                   IF      WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                               NOT =   ZERO
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *            消炎鎮痛処置の逓減チェック
                   PERFORM 100112-SYOEN-CHK-SEC
      *            労災の時、労災加算の算定
                   IF      SPA-HKNKBN          =   1
                       PERFORM 100113-RSIHKN-SYORI-SEC
                   END-IF
      *        薬剤
               WHEN    "6"
      *            麻毒区分
                   IF      WRK-COM-MADOKUKBN (IDX-Z)   =   1
                       MOVE   1                TO  FLG-MADOKU
                   END-IF
      *            生物学的製剤区分
                   IF      WRK-COM-SEIBUTUKBN(IDX-Z)   =   1
                       MOVE   1                TO  FLG-SEIBETU
                   END-IF
      *        器材
               WHEN    "7"
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                フィルムの時エラー
                       MOVE    "0047"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   END-IF
      *
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
      *****************IF      WRK-COM-INPUTCD (IDX-Z) =   "770000000"
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z       TO  IDX-SANSO
      *!                   PERFORM 10011-SANSO-ADD-SEC
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  WRK-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                              (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
      *
      *
           SET     TBL-SANSO           TO  1
           SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    WRK-SRYCD       =   TBL-SANSO-SRYCD
                                                           (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
           END-SEARCH 
      *
           .
       100115-SANSO-CHK-EXT.
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
      *
      *        施設基準不適応
               IF     (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2 )  OR
                     ((WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   3   ) AND
                      (LNK-ORCSC-NENREI-YY             =   ZERO))
                   MOVE    2                   TO  FLG-KIJUNN
      *            エタナールの局所注入は対象外
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "140032510"
                       CONTINUE
                   ELSE
      *                施設基準不適合（処置）
                       MOVE    IDX-Z               TO  IDX-S
                       MOVE    "140047770"         TO  WRK-SRYCD
                       IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-ARI
                           PERFORM 1002-KASAN-SET-SEC
                           MOVE    IDX-S               TO  IDX-ZS2
                      END-IF
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
      *    消炎鎮痛処置の逓減チェック加算処理
      *****************************************************************
       100112-SYOEN-CHK-SEC           SECTION.
      *
      *    包括まとめ入力は以下対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      100112-SYOEN-CHK-EXT
           END-IF
      *
      *
           SET     TBL-IDX              TO  1
           SEARCH  TBL-SYOCHI-OCC       VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SYOCHI-KBN
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KBN  (TBL-IDX)
                                               TO  WRK-SYOCHI-KBN
           END-SEARCH
      *    消炎鎮痛処置なし
           IF      WRK-SYOCHI-KBN      =   SPACE
               GO      TO      100112-SYOEN-CHK-EXT
           END-IF
      *
      *    １日に１回数のみチェック（Ｋ０２のみのチェック）
           IF      (LNK-ORCSC-PG       =   "K02" )
      *        労災の時、チェックをしない
               AND (SPA-HKNKBN         =   ZERO  )
               MOVE    ZERO            TO  WRK-DAY-CNT
               MOVE    SPACE           TO  WRK-ERRCD2
               PERFORM VARYING     IDX-TBL     FROM    1   BY  1
                       UNTIL      (IDX-TBL     >   4   )   OR
                                  (WRK-ERRCD2  NOT =   SPACE)
                   SET     TBL-IDX             TO  IDX-TBL
                   MOVE    TBL-SRYCD (TBL-IDX) TO  WRK-SRYCD
                   PERFORM 3001-SANTEI-CHK-SEC
                   ADD     LNK-SC91-DAYCNT     TO  WRK-DAY-CNT
                   IF      WRK-DAY-CNT         >   1
      ***********  IF    ((WRK-SRYCD           =   WRK-COM-INPUTCD
      *                                                    (IDX-Z)) AND
      *                   (LNK-SC91-DAYCNT     >   1              ))  OR
      *                  ((WRK-SRYCD       NOT =   WRK-COM-INPUTCD
      *                                                    (IDX-Z)) AND
      ********************(LNK-SC91-DAYCNT     >   ZERO          ))
                       MOVE    "1022"          TO  WRK-ERRCD2
                       MOVE    IDX-Z           TO  WRK-IDX-ERR
      *****************MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-PERFORM
               IF      WRK-ERRCD2      NOT =   SPACE
                   GO      TO      100112-SYOEN-CHK-EXT
               END-IF
           END-IF
      *
      *    労災でマッサージ等、器具等の時、日に３部位とする
           IF     (SPA-HKNKBN          =   1   )  AND
                  (WRK-SYOCHI-KBN      =   "01" OR  "02")
               PERFORM 1001121-RSI-SYOEN-CHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      100112-SYOEN-CHK-EXT
               END-IF
           END-IF
      *
      *    労災の時、同一部位毎に逓減するので、対象外とする
           IF      SPA-HKNKBN          =   1
               GO      TO      100112-SYOEN-CHK-EXT
           END-IF
      *
      *    当月に５回以上の時、逓減（マッサージ等は対象外）
           IF     (WRK-SYOCHI-KBN      =   "01")
               GO      TO      100112-SYOEN-CHK-EXT
           END-IF
           IF     (SPA-NYUGAIKBN       =   "2"  )  AND
                  (LNK-ORCSC-SYOEN-CNT <   4   )
               GO      TO      100112-SYOEN-CHK-EXT
           END-IF
           IF     (SPA-NYUGAIKBN       =   "1" )  AND
                  (WRK-COM-NYU-KAI (IDX-Z)     >   ZERO )
               IF     (WRK-COM-NYU-KAI (IDX-Z)  +  LNK-ORCSC-SYOEN-CNT)
                                           <=  4
                   COMPUTE LNK-ORCSC-SYOEN-CNT  =  LNK-ORCSC-SYOEN-CNT
                                                +  WRK-COM-NYU-KAI
                                                            (IDX-Z)
                   GO      TO      100112-SYOEN-CHK-EXT
               END-IF
      *        今回入力時に逓減となる時
               IF     (WRK-COM-NYU-KAI (IDX-Z) >   1  )  AND
                      (LNK-ORCSC-SYOEN-CNT     <   4  )
                   COMPUTE LNK-ORCSC-SYOEN-CNT =   LNK-ORCSC-SYOEN-CNT
                                               +   WRK-COM-NYU-KAI
                                                                (IDX-Z)
                   MOVE    "140047670"         TO  WRK-SRYCD
                   IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                       CONTINUE
                   ELSE
                       MOVE    "9014"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z + 1)
                       GO      TO      100112-SYOEN-CHK-EXT
                   END-IF
               END-IF
               COMPUTE LNK-ORCSC-SYOEN-CNT =   LNK-ORCSC-SYOEN-CNT
                                           +   WRK-COM-NYU-KAI
                                                             (IDX-Z)
           END-IF
           IF     (SPA-NYUGAIKBN       =   "1" )  AND
                  (WRK-COM-NYU-KAI (IDX-Z) =   ZERO )  AND
                  (LNK-ORCSC-SYOEN-CNT <   4   )
               GO      TO      100112-SYOEN-CHK-EXT
           END-IF
      *
      *    消炎鎮痛逓減
           MOVE    IDX-Z               TO  IDX-S
           MOVE    "140047670"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-ARI
               PERFORM 1002-KASAN-SET-SEC
               MOVE    IDX-S               TO  IDX-ZS2
           END-IF
      *
           .
       100112-SYOEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災でマッサージ等チェック処理
      *****************************************************************
       1001121-RSI-SYOEN-CHK-SEC       SECTION.
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    ZERO                TO  WRK-DAY-CNT
      *
           INITIALIZE                  WRK-DAY-CNT-TBL
      *
      *    マッサージ等を当月のすべての保険を検索する
           MOVE    WRK-COM-INPUTCD (IDX-Z)
                                       TO  WRK-SRYCD
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *    算定履歴区分を１とする
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM
               UNTIL       FLG-SANTEI      =   1
               COMPUTE WRK-DAY-CNT         =   WRK-DAY-CNT
                                           +   SANTEI-DAY (IDX-Y2)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   31
                   COMPUTE WRK-DAY (IDX2)  =   WRK-DAY (IDX2)
                                           +   SANTEI-DAY (IDX2)
               END-PERFORM
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    訂正の時、訂正分はカウントしない
           MOVE    ZERO                TO  WRK-TEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (LNK-ORCSC-TEI-SRYCD(IDX)
                                           =   SPACE )
               IF      WRK-SRYCD           =   LNK-ORCSC-TEI-SRYCD(IDX)
                   ADD     LNK-ORCSC-TEI-KAISU(IDX)
                                           TO  WRK-TEI-CNT
               END-IF
           END-PERFORM
           IF      WRK-TEI-CNT         <=  WRK-DAY-CNT
               COMPUTE WRK-DAY-CNT     =   WRK-DAY-CNT
                                       -   WRK-TEI-CNT
           END-IF
      *
           IF      WRK-DAY-CNT         >=  3
               MOVE    "0076"              TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               GO      TO      1001121-RSI-SYOEN-CHK-EXT
           END-IF
      *    入院で入院日の指定がある時
           IF     (SPA-NYUGAIKBN       =   "2" )  OR
                  (LNK-ORCSC-SYORIFLG  =   "1" )  OR
                  (WRK-COM-NYU-KAI (IDX-Z) =   ZERO )
               GO      TO      1001121-RSI-SYOEN-CHK-EXT
           END-IF
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (WRK-DAY (IDX2)  +   WRK-COM-NYUINDAY(IDX-Z IDX2))
                                           >=  3
                   MOVE    "0076"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z + 1)
               END-IF
           END-PERFORM
      *
           .
       1001121-RSI-SYOEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素補正率自動加算処理
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
           MOVE    "770020070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-MAX-LINE  )  OR
                              (WRK-COM-INPUTCD(IDX1)   =   SPACE AND
                               WRK-GMN-INPUTCD(IDX1)   =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1                TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               MOVE    IDX-SANSO           TO  IDX-S
           END-IF
      *    酸素補正率自動加算
           PERFORM 1002-KASAN-SET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災加算の算定処理
      *****************************************************************
       100113-RSIHKN-SYORI-SEC        SECTION.
      *
      *    初診時ブラッシング料は初診算定時のみの算定とする
           IF      WRK-COM-INPUTCD(IDX-ZS) =   "101400030"
               IF      LNK-ORCSC-SYOSIN2       =   1
                   CONTINUE
               ELSE
                   MOVE    "8014"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               END-IF
      *        アフターケアの時算定不可
               IF      SPA-RSI-HKNKBN          =   "3"
                   MOVE    "8018"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
      *    労災の時、加算なし
           IF      WRK-COM-INPUTCD(IDX-ZS)(1:3)    =   "101"
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
           MOVE    SPACE                TO  WRK-SRYCD
           MOVE    ZERO                 TO  FLG-ROUSAI
      *    労災用　区分番号
           IF      WRK-COM-CDKBN-KBN (IDX-ZS)   =   "J"
               EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX-ZS)
      *            創傷処置
                   WHEN    "000"
      *            術後創傷処置
                   WHEN    "001"
      *            ドレーン法
                   WHEN    "002"
      *            皮膚科軟膏
                   WHEN    "053"
      *            間接
                   WHEN    "116"
                       EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
      *                        １．５倍
                               MOVE    "101400020"      TO  WRK-SRYCD
                           WHEN    2
      *                        ２倍
                               MOVE    "101400010"      TO  WRK-SRYCD
                       END-EVALUATE
                       MOVE    1                    TO  FLG-ROUSAI
      *
      *            皮膚科軟膏処置
                   WHEN    "054"
      *            鋼線等による直接牽引
                   WHEN    "117"
                       EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
      *                        １．５倍
                               MOVE    "101400020"      TO  WRK-SRYCD
                           WHEN    2
      *                        皮膚科光線治療・ 鋼線等による直接牽引は
      *                        手の場合も、１．５倍する
                               IF      WRK-COM-INPUTCD(IDX-ZS)
                                                        =   "140028810"
                                                        OR  "140029010"
                                                        OR  "140029210"
      *                            １．５倍
                                   MOVE    "101400020"  TO  WRK-SRYCD
                               END-IF
                       END-EVALUATE
                       MOVE    2                    TO  FLG-ROUSAI
      *            消炎鎮痛処置
                   WHEN    "119"
                       EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
      *                        １．５倍
                                MOVE    "101400020"      TO  WRK-SRYCD
                           WHEN    2
      *                        消炎鎮痛の時、手の場合も、１．５倍する
                               IF      WRK-COM-INPUTCD(IDX-ZS)
                                                        =   "140029610"
                                                        OR  "140040310"
      *                            １．５倍
                                   MOVE    "101400020"  TO  WRK-SRYCD
                               END-IF
                       END-EVALUATE
                       MOVE    2                    TO  FLG-ROUSAI
      *                湿布処置の時、部位による
                       IF      WRK-COM-INPUTCD(IDX-ZS) =   "140002210"
                                                       OR  "140002310"
                           EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
      *                        １．５倍
                               MOVE    "101400020"      TO  WRK-SRYCD
                           WHEN    2
      *                        ２倍
                               MOVE    "101400010"      TO  WRK-SRYCD
                           END-EVALUATE
                           MOVE    1                    TO  FLG-ROUSAI
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    既に他の労災加算が入力されている時は自動算定しない
           MOVE    ZERO                TO  FLG-ROU-ARI
           COMPUTE IDX-Z2              =   IDX-ZS      +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ROU-ARI             =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   "101400020"  OR
                                                   "101400010"  OR
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
               EVALUATE    FLG-ROUSAI
      *            四肢加算なし
                   WHEN    ZERO
                       MOVE    "8004"      TO  WRK-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX1 - 1)
      *            １．５倍のみ
                   WHEN    2
                       IF       WRK-COM-INPUTCD (IDX1 - 1)
                                                =   "101400020"
                                                OR  "099509901"
                           CONTINUE
                       ELSE
                           MOVE    "8005"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX1 - 1)
                       END-IF
               END-EVALUATE
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
           IF      WRK-SRYCD           =   SPACE
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    加算料チェック処理（次ぎの手技料の前に）
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           COMPUTE IDX-Z2              =   IDX-ZS    +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
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
                      MOVE    SPA-MAX-LINE            TO  IDX1
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               IF      IDX-T           =   ZERO
                   MOVE    IDX-ZS          TO  IDX-S
               ELSE
                   MOVE    IDX-T           TO  IDX-S
               END-IF
           END-IF
      *
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           .
       100113-RSIHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算追加処理
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
           MOVE    IDX-ZS2             TO  IDX-S
      *
      *    外来で診察料の無い場合の時間加算
           IF      (SPA-NYUGAIKBN          =   "2"  )  AND
                   (LNK-ORCSC-PG           =   "K02")  AND
                   (LNK-ORCSC-SYORIFLG     =   ZERO )  AND
                   (SPA-SRYYMD             =   SPA-SYSYMD) AND
                   (SPA-HKNCOMBINUM    NOT =   "9999") AND
                   (LNK-ORCSC-SINSATU-ARI  =   ZERO )  AND
                   (SPA-GAI-JIKANKBN   NOT =   ZERO )  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS)
                                       NOT =   ZERO )
               PERFORM 10019-JIKANKASAN-CHK-SEC
           END-IF
      *    時間加算
           IF      (LNK-ORCSC-TIMEFLG          NOT =   ZERO   OR
                    WRK-COM-TIMEFLG (IDX-ZS)   NOT =   ZERO )  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS) NOT =   ZERO )
      *    外来の時のみ
               AND (SPA-NYUGAIKBN      =   "2"  )
               PERFORM 10012-JIKANKASAN-SET-SEC
           END-IF
      *    入院の時、時間加算区分のみ
           IF      (SPA-NYUGAIKBN      =   "1"  )   AND
                   (WRK-COM-TIMEFLG (IDX-ZS)   NOT =   ZERO )  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS) NOT =   ZERO )
               PERFORM 10012-JIKANKASAN-SET-SEC
           END-IF
      *
      *    年齢加算処理
           PERFORM 10013-NENREI-SET-SEC
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来で診察料の無い場合の時間加算処理
      *****************************************************************
       10019-JIKANKASAN-CHK-SEC        SECTION.
      *
      *    時間外の有無
           IF     (WRK-GMN-INPUTCD (IDX-ZS)(1:1)       NUMERIC  ) AND
                  (WRK-GMN-INPUTCD (IDX-ZS)(2:1)       =   SPACE) AND
                  (WRK-GMN-INPUTCD (IDX-ZS)(3:)    NOT =   SPACE)
               CONTINUE
           ELSE
      *        時間外設定
               MOVE    WRK-GMN-INPUTCD (IDX-ZS)    TO  WRK-INPUTCD
               MOVE    SPACE                       TO  WRK-GMN-INPUTCD
                                                         (IDX-ZS)
               MOVE    SPA-GAI-JIKANKBN            TO  WRK-GMN-INPUTCD
                                                         (IDX-ZS)(1:1)
               MOVE    WRK-INPUTCD                 TO  WRK-GMN-INPUTCD
                                                         (IDX-ZS)(3:)
               MOVE    SPA-GAI-JIKANKBN            TO  WRK-COM-TIMEFLG
                                                         (IDX-ZS)(1:1)
           END-IF
      *
           .
       10019-JIKANKASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    時間加算加算料追加処理
      *****************************************************************
       10012-JIKANKASAN-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-TIMEFLG
           IF      WRK-TIMEFLG         =   ZERO
               MOVE    WRK-COM-TIMEFLG(IDX-ZS) TO  WRK-TIMEFLG
           END-IF
      *    入院の時
           IF      SPA-NYUGAIKBN       =   "1"
      *        時間外区分の無い場合はクリアする
               IF     (WRK-GMN-INPUTCD(IDX-ZS)(1:1)    NUMERIC) AND
                      (WRK-GMN-INPUTCD(IDX-ZS)(2:1)    =   SPACE )  AND
                      (WRK-GMN-INPUTCD(IDX-ZS)(3:) NOT =   SPACE )
                   MOVE    WRK-COM-TIMEFLG(IDX-ZS) TO  WRK-TIMEFLG
               ELSE
                   IF      LNK-ORCSC-TIMEFLG       =   ZERO
                       MOVE    ZERO                TO  WRK-TIMEFLG
                       MOVE    ZERO                TO  WRK-COM-TIMEFLG
                                                                (IDX-ZS)
                   END-IF
               END-IF
           END-IF
      *
      *    時間加算
           EVALUATE    WRK-TIMEFLG
      *      /* 時間外 */
               WHEN    1
                   MOVE    "140000190"     TO  WRK-SRYCD
      *     /* 休日 */
               WHEN    2
                   MOVE    "140000290"     TO  WRK-SRYCD
      *        /* 深夜 */
               WHEN    3
                   MOVE    "140000390"     TO  WRK-SRYCD
      *        /* 時間外特例 */
               WHEN    4
                   MOVE    "140000490"     TO  WRK-SRYCD
           END-EVALUATE
      *
           IF      WRK-SRYCD            =    SPACE
               GO      TO      10012-JIKANKASAN-SET-EXT
           END-IF
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    MOVE    IDX1           TO  IDX-T
               END-IF
           END-PERFORM
           IF      IDX-T               =   ZERO
               CONTINUE
           ELSE
               MOVE    IDX-T               TO  IDX-S
           END-IF
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
           END-IF
      *
           .
       10012-JIKANKASAN-SET-EXT.
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
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
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
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
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
      *    年齢加算追加処理（共通）
      *****************************************************************
       10013-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-ZS IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-ZS IDX2)  TO  WRK-SRYCD
      *        加算料チェック処理
               PERFORM 100111-KASAN-CHK-SEC
      *        加算料追加処理
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD            =   1
                   MOVE    1                   TO  WRK-COM-SANFLG
                                                           (IDX-S)
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10013-NENREI-SET-EXT.
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
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
           END-IF
      *    時間加算かの判定
           IF      (WRK-COM-TIMEFLG (IDX-Z)   =   ZERO  ) AND
                   (WRK-COM-AUTOKBN (IDX-Z)   =   SPACE ) AND
                   (WRK-COM-TIMEKSNKBN(IDX-Z) NOT =   ZERO)
               MOVE    1                TO  WRK-COM-SANFLG (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-SANSO
           MOVE    ZERO                TO  WRK-SANSO
           MOVE    ZERO                TO  WRK-SANKEI
           MOVE    ZERO                TO  WRK-SANTEN
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-SYUTEN1 (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-SYUTEN2 (IDX-Z)
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
      *    材料再計算編集
           PERFORM 1007-KIZAI-TEN-SEC
      *    割合加算
      **** PERFORM 1006-KASAN-SET-SEC
      *    手技料の計算
           IF      IDX-ZS              >   ZERO
               COMPUTE IDX-Z           =   IDX-MAX    +  1
               PERFORM 100212-TENSUKEI-SEC
            END-IF
      **** COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
      ****                             +   LNK-SC90-TENGAKEI
           COMPUTE WRK-GOK-TENSUKEI    =   WRK-GOK-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
           MOVE    WRK-GOK-TENSUKEI    TO  LNK-SC90-TENSUKEI
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
      *
      *    セット用合計点数
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
           .
       1002-TENSU-KEI-EXT.
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
               IF      IDX-ZS              >   ZERO
      *            手技料の計算
                   PERFORM 100212-TENSUKEI-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-ZS
           END-IF
      *
           MOVE    ZERO                TO  LNK-SC90-HAIKI
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *    各チェック処理
      *DD  EVALUATE    WRK-MOJI
      *        薬剤
      *        WHEN    "6"
      *            廃棄が有った時、破棄コメントを追加する
      *            IF      LNK-SC90-HAIKI          =   1
      *                MOVE    IDX-Z               TO  IDX-HAI
      *                PERFORM 10011-HAIKI-ADD-SEC
      *            END-IF
      *DD  END-EVALUATE
      *
      *
      *    酸素計算
           IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
               PERFORM 100211-SANSO-KEI-SEC
           END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    破棄コメント自動発生成処理
      *****************************************************************
       10011-HAIKI-ADD-SEC           SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-HAI + 1) =   WRK-SRYCD
               GO      TO      10011-HAIKI-ADD-EXT
           END-IF
      *
           MOVE    IDX-HAI             TO  IDX-S
           MOVE    ZERO                TO  FLG-ARI
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    IDX-HAI             TO  IDX-Z
           END-IF
      *
           .
       10011-HAIKI-ADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    酸素計算処理
      *****************************************************************
       100211-SANSO-KEI-SEC            SECTION.
      *
      *    酸素判定
           MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
           PERFORM 100115-SANSO-CHK-SEC
           IF      FLG-SANSO-OK        =   1
      *        酸素金額計算
               COMPUTE WRK-SANSOKIN    =   WRK-COM-KISOTEN (IDX-Z)
                                       *   WRK-COM-SURYO (IDX-Z)
               ADD     WRK-SANSOKIN    TO  WRK-SANSOKIN-SUM
           END-IF
      *
           .
       100211-SANSO-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100212-TENSUKEI-SEC             SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
      *
      *    手技料の合計を編集する
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN2 (IDX-ZS)
      *    労災の外来管理加算読み替え処理
           IF      SPA-HKNKBN          =   1
               IF     (WRK-COM-RSIGIRI (IDX-ZS)
                                           =   1  )  AND
                      (WRK-COM-SYUTEN2 (IDX-ZS)
                                           <   LNK-ORCSC-GAIRAI-TEN)
                   MOVE     LNK-ORCSC-GAIRAI-TEN
                                           TO  LNK-SC90-TENSUKEI
               END-IF
           END-IF
      *
           ADD     LNK-SC90-TENSUKEI   TO  WRK-GOK-TENSUKEI
           MOVE    ZERO                TO  LNK-SC90-TENSUKEI
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
      *    自動加算以外の加算（時間外、年齢以外）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
      *    手技料の点数を所定点数とする
           PERFORM VARYING     IDX     FROM    IDX-ZS  BY  1
                   UNTIL      (IDX         >=  HZN-IDX     )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        ％加算（労災以外）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                               (IDX-ZS)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *        ％減算
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                               (IDX-ZS)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
      *    自動加算の加算（時間外、年齢）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    IDX-ZS  BY  1
                   UNTIL       IDX         >=  HZN-IDX
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100.00
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
           IF      SPA-HKNKBN          =   ZERO
               GO      TO      1006-KASAN-SET-EXT
           END-IF
      *    労災の加算
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    IDX-ZS  BY  1
                   UNTIL       IDX         >=  HZN-IDX
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100.00
                                               +   0.9
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    材料再計算編集処理
      *****************************************************************
       1007-KIZAI-TEN-SEC                SECTION.
      *
      *    酸素の点数数計算
           IF      WRK-SANSOKIN-SUM    NOT =   ZERO
               PERFORM 10071-SANSO-KEISAN-SEC
           END-IF
      *
           .
       1007-KIZAI-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     酸素の点数数計算処理
      *****************************************************************
       10071-SANSO-KEISAN-SEC                SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            酸素補正率自動加算
                   WHEN    "770020070"
      *                酸素金額＋（酸素金額×補正率／１００）　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN-SUM
                                               *   WRK-COM-KISOTEN (IDX)
                                               /   100
                       COMPUTE WRK-SANSOKIN    =   WRK-SANSOKIN-SUM
                                               +   WRK-SYOKEIS
               END-EVALUATE
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            高気圧酸素計算
                   WHEN    "770030070"
      *                酸素金額＋（酸素金額×補正率／１００）
      *                                           ＋気圧数　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                               *   WRK-COM-SURYO (IDX)
                       MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
               END-EVALUATE
           END-PERFORM
      *    酸素の点数数計算
      *    金額計　四捨五入
           COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
                                       +   0.5
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *    酸素の点数を器材点数に加算
           ADD     WRK-SANTEN          TO  LNK-SC90-KIZTEN
      *
           .
       10071-SANSO-KEISAN-EXT.
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
       300-AUTO-SYORI-SEC                SECTION.
     *
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       3001-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                      ORCSC91AREA
           MOVE    "3"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"       USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-ERRCD      =   SPACE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           ELSE
      *        追加なし
               MOVE    1               TO  FLG-ERR
           END-IF
      *
           .
       3001-SANTEI-CHK-EXT.
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
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
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
