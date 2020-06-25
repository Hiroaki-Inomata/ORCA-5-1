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
       PROGRAM-ID.                 ORCSC20200504.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（薬剤）
      *                      レセ電対応（Ｈ１７年？月から）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/09    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    多々納       01/04/18  麻毒の判定、薬剤数カウント
      *                                     の修正等
      *  01.00.02    多々納       01/05/21  診療種別の判定の追加
      *  02.00.00    多々納       02/03/20  改正対応
      *  02.00.01    多々納       02/03/29  臨時投薬１４日以上の組み込み
      *  02.00.02    多々納       02/04/19  生活習慣病指導算定時の追加
      *  02.00.03    NACL-多々納  02/09/04  臨時投薬の１４日以内を修正
      *  02.00.04    NACL-多々納  02/12/03  他科受診メッセージ削除
      *  02.00.05    NACL-多々納  03/07/10  入院調剤料算定なし追加
      *  02.00.06    NACL-多々納  04/03/10  改正対応
      *  02.00.07    NACL-多々納  04/06/XX  複数科・保険対応
      *  02.00.08    NACL-多々納  05/12/14  .213 自動変換対応
      *  02.00.09    NACL-多々納  05/12/14  MONFUNC 対応 他
      *  02.00.10    NACL-多々納  05/12/20  処方せん料と調基の訂正判定
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
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-SRYACCT-ARI     PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-TONPUK          PIC 9(01).
           03  FLG-GAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-MADOKU-ARI      PIC 9(01).
           03  FLG-ZAI             PIC 9(01).
           03  FLG-NAIYAK2         PIC 9(01).
           03  FLG-TONPUK2         PIC 9(01).
           03  FLG-GAIYAK2         PIC 9(01).
      *
      **** 03  FLG-YAKJYO         PIC 9(01).
           03  FLG-ADD            PIC 9(01).
      *
           03  FLG-TOKUSYO1         PIC 9(01).
           03  FLG-TOKUSYO2         PIC 9(01).
           03  FLG-SYOHORYO         PIC 9(01).
           03  FLG-SYOHOSENN        PIC 9(01).
           03  FLG-SYOHO-CHK        PIC 9(01).
           03  FLG-SYOHO-CHK2       PIC 9(01).
      *
           03  FLG-KOUHATU          PIC 9(01).
      *    内服
           03  FLG-NAIFUKU          PIC 9(01).
           03  FLG-OK3              PIC 9(01).
      *    診察料
           03  FLG-SINSATU          PIC 9(01).
           03  FLG-JYURRK-END       PIC 9(01).
      *    長期あり
           03  FLG-CHOUKI           PIC 9(01).
      *
           03  FLG-CHOKIKA1         PIC 9(01).
           03  FLG-CHOKIKA2         PIC 9(01).
      *
           03  FLG-CHOKIKSN         PIC 9(01).
      *    薬剤判定
           03  FLG-YAKUZAI-ARI         PIC 9(01).
           03  FLG-ZAIEND              PIC 9(01).
      *
           03  FLG-ALL                  PIC 9(01).
           03  FLG-YAKUJYO              PIC 9(01).
           03  FLG-CHOKI                PIC 9(01).
      *
           03  FLG-SYOHOU-ERR           PIC 9(01).
           03  FLG-TOKUTEI              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
           03  IDX-T1              PIC 9(04).
           03  IDX-T2              PIC 9(04).
      *
           03  IDN1                PIC 9(02).
      *
           03  IDX-SYOHOU          PIC 9(04).
           03  IDX-KA              PIC 9(04).
           03  IDX-KA2             PIC 9(04).
           03  IDX-CHOKI           PIC 9(04).
           03  IDT                 PIC 9(04).
      *
           03  IDX-GEN             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-ERRCD3          PIC X(04). 
           03  WRK-ERRCD6          PIC X(04). 
      *
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-TENSUKEI        PIC 9(08)V9(04).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-TENSU-KEISAN.
               05  WRK-TENSU-GAI       PIC 9(08)V99.
               05  WRK-TENSU-KEI       PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
           03  WRK-TENSUKEI-GEN    PIC 9(08).
      *
           03  WRK-TEI-CNT         PIC 9(04).
      *
           03  WRK-IDX             PIC 9(04).
      *
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-KAISU-H         PIC X(04).
      *
           03  WRK-ZAIKEI-AREA.
               05  WRK-KONGO           PIC 9(05).
               05  WRK-SONOTA          PIC 9(05).
               05  WRK-ZAICNT          PIC 9(05).
               05  WRK-KEIKEI          PIC 9(08).
               05  WRK-ZAICNT2         PIC 9(05).
               05  WRK-KEIKEI2         PIC 9(08).
               05  WRK-SANTEN          PIC 9(08).
               05  WRK-SANTEN-9        PIC 9(08)V99.
               05  WRK-TENKEI          PIC 9(08).
               05  WRK-ZAITEN          PIC 9(08).
               05  WRK-ZAITEN-G        PIC 9(08)V9(8).
      *
               05  WRK-CHK-SRYSYUKBN   PIC X(03).
      *    保険毎の逓減計算
           03  WRK-HKNKEI-G.
               05  WRK-CNT-HKN         PIC 9(03).
               05  WRK-SANTEN-G        PIC 9(08).
               05  WRK-SANTEN-GEN      PIC 9(08).
               05  WRK-SANTENSU        PIC 9(08).
               05  WRK-SANTEN-KEI      PIC 9(08).
               05  WRK-HKNKEI-TBL      OCCURS   5.
                   07  WRK-HKNKEI-COMBI    PIC X(04).
                   07  WRK-HKNKEI-KEIKEI   PIC 9(08).
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *    入院の回数
           03  WRK-NISUU-AREA.
               05  WRK-NISUU-TBL           OCCURS   31.
                   07  WRK-DAY             PIC 9(02).
                   07  WRK-SEL             PIC X(01).
           03  IDN2                    PIC 9(04).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
           03  FLG-DAY                 PIC 9(01).
           03  WRK-H-DAY               PIC 9(02).
           03  WRK-H-SEL               PIC X(01).
      *    末日
           03  WRK-MATU                PIC 9(02).
      *
           03  WRK-ORC-DBPATH          PIC X(63).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
      *
           03  WRK-HEN-MEI             PIC X(20).
           03  WRK-HENSYU-MEISYO       PIC X(60).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *    薬剤毎の保険
           03  WRK-NAI-HKNCOMBI        PIC X(04).
           03  WRK-GAI-HKNCOMBI        PIC X(04).
           03  WRK-MADO1-HKNCOMBI      PIC X(04).
           03  WRK-MADO2-HKNCOMBI      PIC X(04).
           03  WRK-CHOU-HKNCOMBI       PIC X(04).
           03  WRK-SYO-HKNCOMBI        PIC X(04).
      *
      *    継続の時、自動発生のチェック
       01  WRK-CONTCHK-TBL.
           03  WRK-CONTCHK-OCC         OCCURS  30.
               05  WRK-TBL-SRYCD       PIC X(09).
           03  WRK-TBL-HKNCOMBI        PIC X(04).
           03  WRK-IDX-Y               PIC 9(04).
           03  IDX-TBL                 PIC 9(04).
           03  FLG-CNTCHK              PIC 9(01).
      *
      *    入院日
       01  TBL-NYUINDAY-AREA.
           03  TBL-NYUINDAY-OCC        OCCURS   31.
               05  TBL-NYUINDAY        PIC 9(01).
      *
      *    診療科・保険
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS   10.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-HKNCOMBI        PIC X(04).
      *
      *    保存用
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
      *
      *    チェックマスタ−
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90TEN.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
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
           COPY    "CPK02SPASCR.INC"   REPLACING
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
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
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
                   MOVE    "1099"          TO  SPA-ERRCD
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
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )   OR
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
      *            粉砕の時、９とする
                   IF      WRK-COM-INPUTCD(IDX-Z)      =   "099209901"
                       MOVE    "9"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *        診療種別の変更
               IF      WRK-SRYSYUKBN   NOT =   SPACE
                   MOVE    WRK-SRYSYUKBN   TO  WRK-COM-SRYSYUKBN (IDX-Z)
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
                   WHEN    "."
                       PERFORM 10010-SRYKBN-CHK-SEC
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
      *    投薬の処方のみチェック処理
      *****************************************************************
       10010-SRYKBN-CHK-SEC             SECTION.
      *
           IF      LNK-ORCSC-PG        =   "K05"
      *        セット登録のチェックなし
               CONTINUE
           ELSE
      *
           MOVE    WRK-COM-SRYSYUKBN (IDX-Z)
                                       TO  WRK-SRYSYUKBN
           EVALUATE    WRK-GMN-INPUTCD(IDX-Z)(2:3)
               WHEN    "213"
               WHEN    "223"
               WHEN    "233"
      *            投薬の処方のみは、投薬が算定できない時のみ
                   IF      (LNK-ORCSC-ROUMANSEI-ARI    =   ZERO) AND
                           (LNK-ORCSC-ROUMANSEI-ARI2   =   ZERO) AND
                           (LNK-ORCSC-NETAKIRI-ARI     =   ZERO) AND
                           (LNK-ORCSC-NETAKIRI-ARI2    =   ZERO) AND
                           (LNK-ORCSC-SYONIKA-ARI      =   ZERO)  AND
                           (LNK-ORCSC-SEIKATU-ARI2     =   ZERO) AND
                           (LNK-ORCSC-SEIKATU-ARI      =   ZERO) AND
                           (SPA-ZAIMATU-ARI2           =   ZERO) AND
                           (SPA-ZAIMATU-ARI            =   ZERO)
      *************    MOVE    "0074"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR
      *************                                          (IDX-Z)
                       MOVE    "0"             TO  WRK-SRYSYUKBN(3:1)
                       MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                       PERFORM 100101-SRYSYUMEI-HEN-SEC
                   END-IF
               WHEN    "212"
               WHEN    "222"
               WHEN    "232"
      *            院外処方は外来のみ
                   IF      SPA-NYUGAIKBN   =   "1"
                       MOVE    "9028"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
               WHEN    "214"
               WHEN    "224"
               WHEN    "234"
      *            入院調剤料なしは入院のみ
                   IF      SPA-NYUGAIKBN   =   "2"
                       MOVE    "9024"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           END-IF
           .
       10010-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別名編集処理
      *****************************************************************
       100101-SRYSYUMEI-HEN-SEC             SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-GMN-MEISYO (IDX-Z)
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                       TO  WRK-GMN-MEISYO (IDX-Z)
           END-SEARCH 
      *
           .
       100101-SRYSYUMEI-HEN-EXT.
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
      *            粉砕のチェック
               WHEN    "9"
                   IF      WRK-COM-INPUTCD(IDX-Z + 1)  NOT =   SPACE
                       IF     (WRK-COM-INPUTCD(IDX-Z + 1)(1:1)
                                                       =   "6"  )  AND
                              (WRK-COM-ZAIGATAKBN(IDX-Z + 1)
                                                       =   0    )
                           CONTINUE
                       ELSE
                           MOVE    "0063"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z + 1)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
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
                   UNTIL      (IDX1        >   SPA-MAX-LINE    )   OR
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
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
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
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
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
                   WHEN    "*"
                       CONTINUE
                   WHEN    "S"
      *                編集チェックへ
                       PERFORM 10023-SETHEN-SEC
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    割合加算
           PERFORM 1006-KASAN-SET-SEC
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    計
      *    外用薬時、回数は１とする
           IF      WRK-COM-SRYKBN (IDX-MAX)    =   "23"
               MOVE   1                    TO  LNK-ORCSC-KAISU
               MOVE    WRK-COM-TENSU  (IDX-MAX)
                                               TO  WRK-COM-KEI (IDX-MAX)
           ELSE
               COMPUTE WRK-COM-KEI (IDX-MAX)
                                           =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
           END-IF
      *
      *    院外の時、点数をゼロとする
           MOVE    WRK-COM-SRYSYUKBN(IDX-MAX)  TO  WRK-CHK-SRYSYUKBN
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      WRK-CHK-SRYSYUKBN       =   SPACE
               EVALUATE    WRK-COM-SRYKBN (IDX-MAX)
                   WHEN    "21"
                       MOVE    "210"           TO  WRK-CHK-SRYSYUKBN
                   WHEN    "22"
                       MOVE    "220"           TO  WRK-CHK-SRYSYUKBN
                   WHEN    "23"
                       MOVE    "230"           TO  WRK-CHK-SRYSYUKBN
               END-EVALUATE
           END-IF
           IF     (WRK-CHK-SRYSYUKBN           =   "212"  OR
                                                   "222"  OR
                                                   "232"  OR
                                                   "292" )  OR
                 ((LNK-ORCSC-INGAIKBN          =   "1"   ) AND
                  (WRK-CHK-SRYSYUKBN           =   "210"  OR
                                                   "220"  OR
                                                   "230"  OR
                                                   "290" ))  OR
                 ( WRK-CHK-SRYSYUKBN           =   "213"  OR
                                                   "223"  OR
                                                   "233" ) 
               MOVE    ZERO            TO  WRK-COM-KEI    (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-TENSU  (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-YKZTEN (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-YKZTEN-M(IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KIZAITEN(IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KIZTEN-M(IDX-MAX)
           END-IF
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
      *    外用薬時、回数は１回とする（数量＊回数を数量とする）
           IF     (WRK-COM-SRYKBN (IDX-Z)  =   "23" ) AND
                 ((WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"
                                               OR  "7" )  OR
                  (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059" ))
               COMPUTE WRK-COM-SURYO (IDX-Z)   =   WRK-COM-SURYO2(IDX-Z)
                                               *   LNK-ORCSC-KAISU
               MOVE    1                   TO  WRK-KAISU
           ELSE
               MOVE    LNK-ORCSC-KAISU     TO  WRK-KAISU
           END-IF
      *
      *    手技点数計算
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   PERFORM 100211-SYUGITEN-KEI-SEC
           END-EVALUATE
      *    回数
           MOVE    WRK-KAISU           TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技点数計算処理
      *****************************************************************
       100211-SYUGITEN-KEI-SEC              SECTION.
      *
           IF      WRK-COM-TENSIKIBETU (IDX-Z)     =   5
                                                   OR  6
               CONTINUE
           ELSE
      *        通則加算対象外区分判定
               IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "1"
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-GAI
               ELSE
                   ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-TENSU-KEI
               END-IF
           END-IF
           .
       100211-SYUGITEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-GEN
      *    １剤１手技とする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE      )   OR
                             ((WRK-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX)   =   SPACE ))
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
      *
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-GEN
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   WRK-TENSU-KEI
                                       +   WRK-TENSU-GAI
                                       +   WRK-TENSUKEI-G
                                       -   WRK-TENSUKEI-GEN
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束セット回数処理
      *****************************************************************
       10023-SETHEN-SEC                SECTION.
      *
      *    外用薬時、回数は１回とする（数量＊回数を数量とする）
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "23"
               COMPUTE WRK-COM-SURYO (IDX-Z)   =   WRK-COM-SURYO2(IDX-Z)
                                               *   LNK-ORCSC-KAISU
           END-IF
      *
           .
       10023-SETHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *    包括まとめ入力は以下対象外
           IF     (SPA-HKNCOMBINUM     =   "9999" )  OR
                  (LNK-ORCSC-PG    NOT =   "K02"  )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  IDX-CHOKI
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                                 ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                                  (WRK-COM-INPUTCD(IDX) =   SPACE))
      *            調基の判定
                   IF     (WRK-COM-SRYKBN (IDX)    =   "27"  )  AND
                          (WRK-COM-INPUTCD(IDX)    =   "120001810" OR
                                                       "120001710")
                       MOVE    IDX                 TO  IDX-CHOKI
                   END-IF
               END-PERFORM
      *        調基が算定時の処方せん料とのチェック
               IF      IDX-CHOKI           >   ZERO
      *            処方せん料の算定チェック
                   PERFORM 30024-SYOHOUSEN-CHK-SEC
                   IF      FLG-SYOHOU-ERR  NOT =   ZERO
                       MOVE    "1039"      TO  SPA-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-CHOKI)
                   END-IF
               END-IF
           END-IF
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
      *    包括まとめ入力は以下対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *        他保険で算定済み
               INITIALIZE                  SPA-SYOHOU-ZUMI
           END-IF
      *
           INITIALIZE                      WRK-CONTCHK-TBL
           MOVE    ZERO                TO  WRK-KAISU
      *
      *    継続の時、算定分の編集
      *!!  IF     (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
      *        入院の時
      *!!      OR (SPA-NYUGAIKBN        =   "1"  )
               PERFORM 30010-JYURRK-SYORI-SEC
      *!!  END-IF
      *
      *    科毎
           INITIALIZE                  TBL-SRYKA-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
               IF     (WRK-COM-SRYKBN(IDX)(1:1)    =   "2" )  OR
                      (WRK-COM-SRYSYUKBN(IDX)      =   "820")
                   IF      WRK-COM-SRYKA (IDX)     =   SPACE
                       MOVE    SPA-SRYKA      TO  WRK-COM-SRYKA(IDX)
                   END-IF
                   IF      WRK-COM-HKNCOMBI (IDX)  =   SPACE
                       MOVE    SPA-HKNCOMBINUM
                                              TO  WRK-COM-HKNCOMBI(IDX)
                   END-IF
                   PERFORM VARYING     IDX-KA  FROM    1   BY  1
                           UNTIL       IDX-KA  >   10
                       IF      TBL-SRYKA (IDX-KA)  =   SPACE
                           MOVE    WRK-COM-SRYKA(IDX)  TO  TBL-SRYKA
                                                              (IDX-KA)
                           MOVE    WRK-COM-HKNCOMBI(IDX)
                                                       TO  TBL-HKNCOMBI
                                                              (IDX-KA)
                       END-IF
                       IF      TBL-SRYKA (IDX-KA)  =   WRK-COM-SRYKA
                                                               (IDX)
                           MOVE    10              TO  IDX-KA
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDXS
           MOVE    SPACE               TO  LNK-ORCSC-KIDCD
      *
           PERFORM VARYING     IDX-KA  FROM    1   BY  1
                   UNTIL      (IDX-KA      >   10   )  OR
                              (TBL-SRYKA (IDX-KA)  =   SPACE)  OR
                              (LNK-ORCSC-KIDCD NOT =   SPACE)
                          OR  (SPA-ERRCD       NOT =   SPACE)
               MOVE    TBL-SRYKA (IDX-KA)      TO  WRK-SRYKA
               MOVE    TBL-HKNCOMBI(IDX-KA)    TO  WRK-HKNCOMBI
      *
               MOVE    ZERO            TO  FLG-TOKUTEI
               PERFORM VARYING     IDX-KA2 FROM    1   BY  1
                       UNTIL      (IDX-KA2     >   10   )  OR
                                  (SPA-TOKU-SRYKA (IDX-KA2)
                                               =   SPACE   )
                   IF      SPA-TOKU-SRYKA (IDX-KA2)    =   WRK-SRYKA
                       IF      SPA-TOKU-FLG(IDX-KA2)   =   05
                           MOVE    1           TO  FLG-TOKUTEI
                       END-IF
                       MOVE    10              TO  IDX-KA2
                   END-IF
               END-PERFORM
               IF      (SPA-TOKUTEIFLG     NOT =   ZERO )  OR
                       (SPA-CHOKITOFLG     NOT =   ZERO )
                   IF      (SPA-TOKUTEI-SRYKA  NOT =   SPACE  )  AND
                           (SPA-TOKUTEI-SRYKA  NOT =   WRK-SRYKA)
                       MOVE    ZERO        TO  FLG-TOKUTEI
                   END-IF
               END-IF
               PERFORM 3000-JIDOU-SYORI-SEC
           END-PERFORM
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定発生処理
      *****************************************************************
       3000-JIDOU-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-NAIYAK
           MOVE    ZERO                TO  FLG-TONPUK
           MOVE    ZERO                TO  FLG-GAIYAK
           MOVE    ZERO                TO  FLG-NAIYAK2
           MOVE    ZERO                TO  FLG-TONPUK2
           MOVE    ZERO                TO  FLG-GAIYAK2
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  IDX-SYOHOU
           MOVE    ZERO                TO  FLG-MADOKU-ARI
           MOVE    ZERO                TO  FLG-SINSATU
           MOVE    ZERO                TO  FLG-CHOUKI 
      *
           MOVE    ZERO                TO  FLG-TOKUSYO1
           MOVE    ZERO                TO  FLG-TOKUSYO2
           MOVE    ZERO                TO  FLG-CHOKIKA1
           MOVE    ZERO                TO  FLG-CHOKIKA2
      *
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  IDX-CHOKI
      *
           MOVE    SPACE               TO  WRK-NAI-HKNCOMBI
           MOVE    SPACE               TO  WRK-GAI-HKNCOMBI
           MOVE    SPACE               TO  WRK-MADO1-HKNCOMBI
           MOVE    SPACE               TO  WRK-MADO2-HKNCOMBI
           MOVE    SPACE               TO  WRK-CHOU-HKNCOMBI
           MOVE    SPACE               TO  WRK-SYO-HKNCOMBI
      *
           INITIALIZE                  WRK-ZAIKEI-AREA
           INITIALIZE                  TBL-NYUINDAY-AREA
           MOVE    SPACE               TO  WRK-HENSYU-MEISYO
      *
           INITIALIZE                  WRK-HKNKEI-G
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
           IF      WRK-COM-SRYKA (IDX)     =   WRK-SRYKA
               MOVE    WRK-COM-SRYSYUKBN (IDX) TO  WRK-CHK-SRYSYUKBN
      *----(01.00.01) LINE ADD START ----------------------------------
               IF      WRK-CHK-SRYSYUKBN       =   SPACE
                   EVALUATE    WRK-COM-SRYKBN (IDX)
                       WHEN    "21"
                           MOVE    "210"       TO  WRK-CHK-SRYSYUKBN
                       WHEN    "22"
                           MOVE    "220"       TO  WRK-CHK-SRYSYUKBN
                       WHEN    "23"
                           MOVE    "230"       TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               END-IF
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *        院外の時
               IF      LNK-ORCSC-INGAIKBN     =   "1"
                   EVALUATE    WRK-CHK-SRYSYUKBN
                       WHEN    "210"
                           MOVE    "212"          TO  WRK-CHK-SRYSYUKBN
                       WHEN    "290" 
                           MOVE    "292"          TO  WRK-CHK-SRYSYUKBN
                       WHEN    "220" 
                           MOVE    "222"          TO  WRK-CHK-SRYSYUKBN
                       WHEN    "230" 
                           MOVE    "232"          TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               ELSE
                   EVALUATE    WRK-CHK-SRYSYUKBN
                       WHEN    "210"
                           MOVE    "211"          TO  WRK-CHK-SRYSYUKBN
                       WHEN    "290" 
                           MOVE    "291"          TO  WRK-CHK-SRYSYUKBN
                       WHEN    "220" 
                           MOVE    "221"          TO  WRK-CHK-SRYSYUKBN
                       WHEN    "230" 
                           MOVE    "231"          TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               END-IF
      *        投薬の時、薬剤コードがあること
               IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "29" )  AND
                       (FLG-ZAIEND             =   ZERO )
                   PERFORM 30001-TOUYAKU-CHK-SEC
                   MOVE    1                   TO  FLG-ZAIEND
               END-IF
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "211"
                   WHEN    "291"
                       MOVE     1              TO  FLG-NAIYAK
                       IF      WRK-NAI-HKNCOMBI    =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-NAI-HKNCOMBI
                       END-IF
                       IF      WRK-CNT-HKN         =   ZERO
                           MOVE    1               TO  WRK-CNT-HKN
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-HKNKEI-COMBI
                                                      (WRK-CNT-HKN)
                       END-IF
                   WHEN    "212"
                   WHEN    "292"
                       MOVE     1              TO  FLG-NAIYAK2
                   WHEN    "221"
                       MOVE     1              TO  FLG-TONPUK
                       IF      WRK-NAI-HKNCOMBI    =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-NAI-HKNCOMBI
                       END-IF
                   WHEN    "222"
                       MOVE     1              TO  FLG-TONPUK2
                   WHEN    "231"
                       MOVE     1              TO  FLG-GAIYAK
                       IF      WRK-GAI-HKNCOMBI    =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-GAI-HKNCOMBI
                       END-IF
                   WHEN    "232"
                       MOVE     1              TO  FLG-GAIYAK2
               END-EVALUATE
      *
      *     処方のみの時以下、算定なし
               IF     (WRK-CHK-SRYSYUKBN(1:1)  NOT =   "2"  ) OR
                      (WRK-CHK-SRYSYUKBN       =   "213" OR
                                                   "223" OR
                                                   "233"  )
                   CONTINUE
               ELSE
      *            処方内容チェク
                   PERFORM 3000-SYOHOU-CHK-SEC
               END-IF
      *
      *        処方せん料算定チェック
               EVALUATE    WRK-COM-INPUTCD(IDX)
      *            後発医薬品を含む
                   WHEN    "120002910"
      *            後発医薬品を含まない
                   WHEN    "120003010"
      *            ７種類以上 後発医薬品を含む
                   WHEN    "120002710"
      *            ７種類以上 後発医薬品を含まない
                   WHEN    "120002810"
                       IF      IDX-SYOHOU      NOT =   ZERO
                           MOVE    "1021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR (IDX)
                       ELSE
                           MOVE    IDX             TO  IDX-SYOHOU
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-SYO-HKNCOMBI
                       END-IF
               END-EVALUATE
      *
      *        特定疾患処方箋
               IF      WRK-COM-INPUTCD (IDX)   =   "120002570"
                   MOVE    1                   TO  FLG-TOKUSYO1
                   MOVE    IDX                 TO  IDX-T1
               END-IF
      *        特定疾患処方料
               IF      WRK-COM-INPUTCD (IDX)   =   "120002270"
                   MOVE    1                   TO  FLG-TOKUSYO2
                   MOVE    IDX                 TO  IDX-T2
               END-IF
      *
      *        長期投与加算（処方）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003170"
                   MOVE    1                   TO  FLG-CHOKIKA2
                   MOVE    IDX                 TO  IDX-T2
               END-IF
      *        長期投与加算（処方せん）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003270"
                   MOVE    1                   TO  FLG-CHOKIKA1
                   MOVE    IDX                 TO  IDX-T1
               END-IF
      *
      *        投薬で入院回数の時、回数集計
               IF     (SPA-NYUGAIKBN               =   "1" )  AND
                      (WRK-COM-SRYKBN (IDX)(1:1)   =   "2" )  AND
                      ((WRK-GMN-INPUTCD(IDX)(1:1)  =   "*" )  OR
                       (WRK-COM-KAIFLG (IDX)       =   "1" )  OR
                       (WRK-COM-ZAIEND (IDX)       =   "1" ))
      *         薬剤のない剤は対象外
                AND   (WRK-CHK-SRYSYUKBN       NOT =   "990" )
                   PERFORM 3009-NYUINKAISU-SEC
               END-IF
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO                TO  FLG-MADOKU-ARI
                   MOVE    ZERO                TO  FLG-ZAIEND
               END-IF
      *
      *        他保険にて算定の判定
               IF     (WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                   "12"  )
                   MOVE    1                   TO  FLG-SINSATU
               END-IF
      *        調基の判定
               IF     (WRK-COM-INPUTCD(IDX)    =   "120001810" OR
                                                   "120001710"  )
                   MOVE    IDX                 TO  IDX-CHOKI
               END-IF
      *
            END-IF
           END-PERFORM
      *
      *    入力分のチェック
           PERFORM 3003-CHEK-SYORI-SEC
           IF     (LNK-ORCSC-KIDCD     NOT =   SPACE )  OR
                  (SPA-ERRCD           NOT =   SPACE )
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           ELSE
      *        院外分
               PERFORM 3001-INGAI-SYORI-SEC
      *        院内分
               IF      SPA-NYUGAIKBN       =   "1"
      *            入院
                   PERFORM 30021-NYUIN-INNAI-SYORI-SEC
               ELSE
      *            外来
                   PERFORM 3002-INNAI-SYORI-SEC
               END-IF
           END-IF
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    WRK-HENSYU-MEISYO   TO  SPA-SYOHO-MEI
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           END-IF
      *
           .
       3000-JIDOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力内容のチェック処理
      *****************************************************************
       3003-CHEK-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHOSENN
      *    処方箋算定
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )) OR
                  (IDX-SYOHOU      NOT =   ZERO )
               MOVE    1               TO  FLG-SYOHOSENN
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYOHORYO
      *    処方箋算定
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))   OR
                  (IDX-SYOHOU      NOT =   ZERO )
               MOVE    ZERO                TO  FLG-SYOHORYO
           ELSE
               IF     (FLG-NAIYAK          =   1    )  OR
                      (FLG-TONPUK          =   1    )  OR
                      (FLG-GAIYAK          =   1    )
                   MOVE    1               TO  FLG-SYOHORYO
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算処方箋料がある時、処方箋料があること
           IF      FLG-TOKUSYO1        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "1014"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T1)
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算処方料がある時の、処方料があること
           IF      FLG-TOKUSYO2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "1015"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T2)
               END-IF
           END-IF
      *
      *    長期投薬処方箋料がある時、処方箋料があること
           IF      FLG-CHOKIKA1        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "1038"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T1)
               ELSE
      *            長期投薬があること
                  IF      FLG-CHOUKI          =   ZERO
                      IF     (WRK-COM-KZMFLG (IDX-T1)  =   SPACE) AND
                             (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    "K021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T1)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T1)
                           MOVE    IDX-T1          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算処方料がある時の、処方料があること
           IF      FLG-CHOKIKA2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "1037"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T2)
               ELSE
      *            長期投薬があること
                   IF      FLG-CHOUKI          =   ZERO
                       IF    (WRK-COM-KZMFLG (IDX-T2)  =   SPACE) AND
                             (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    "K021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T2)
                           MOVE    IDX-T2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    調基が算定時の処方せん料とのチェック
           IF      IDX-CHOKI           >   ZERO
      *        処方せん料の算定チェック
               PERFORM 30024-SYOHOUSEN-CHK-SEC
               IF      FLG-SYOHOU-ERR  NOT =   ZERO
                   MOVE    "1039"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-CHOKI)
               END-IF
           END-IF
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    WRK-HENSYU-MEISYO   TO  SPA-SYOHO-MEI
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           END-IF
      *
           .
       3003-CHEK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   処方内容チェク処理
      *****************************************************************
       30001-TOUYAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUZAI-ARI
      *
           PERFORM VARYING     IDX3    FROM    IDX   BY  1
                   UNTIL      (IDX3    >   SPA-MAX-LINE   )  OR
                              (WRK-GMN-INPUTCD(IDX3) =   SPACE)  OR
                              (FLG-YAKUZAI-ARI       =   1    )
               IF      WRK-COM-INPUTCD(IDX3)(1:1)  =   "6"
                   MOVE    1              TO  FLG-YAKUZAI-ARI
               ELSE
                   IF      WRK-COM-ZAIEND (IDX3) =   "1"
                       MOVE    IDX3           TO  IDX
                       MOVE    SPA-MAX-LINE   TO  IDX3
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-YAKUZAI-ARI      =   ZERO
               MOVE    "990"            TO  WRK-CHK-SRYSYUKBN
           END-IF
      *
           .
       30001-TOUYAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   処方内容チェク処理
      *****************************************************************
       3000-SYOHOU-CHK-SEC              SECTION.
      *
      *    複数保険判定（内服のみ）
           IF      WRK-CHK-SRYSYUKBN   =   "211"
                                       OR  "291"
               IF      WRK-HKNKEI-COMBI (WRK-CNT-HKN)
                                       =   WRK-COM-HKNCOMBI (IDX)
                   CONTINUE
               ELSE
                   ADD     1               TO  WRK-CNT-HKN
                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                           TO  WRK-HKNKEI-COMBI
                                                      (WRK-CNT-HKN)
               END-IF
           END-IF
      *
           EVALUATE    WRK-CHK-SRYSYUKBN
               WHEN    "211"
               WHEN    "221"
               WHEN    "231"
               WHEN    "291"
      *            麻毒区分判定
                   IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
      *----(01.00.01) LINE UPD START ----------------------------------
                      IF      WRK-COM-MADOKUKBN (IDX) NOT =   ZERO
      *----(01.00.01) LINE UPD END   ----------------------------------
                           MOVE    1           TO  FLG-MADOKU
                           MOVE    1           TO  FLG-MADOKU-ARI
                           IF      WRK-CHK-SRYSYUKBN   =   "231"
                               IF      WRK-MADO2-HKNCOMBI  =   SPACE
                                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-MADO2-HKNCOMBI
                               END-IF
                           ELSE
                               IF      WRK-MADO1-HKNCOMBI  =   SPACE
                                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-MADO1-HKNCOMBI
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    内服薬チェック
      *      臨時で１４日以内の時は対象外
           IF      (WRK-CHK-SRYSYUKBN (1:2)   =   "21" ) OR
                  ((WRK-CHK-SRYSYUKBN (1:2)   =   "29" ) AND
                   (WRK-COM-KAISU (IDX)       >   14   ))
               MOVE    1                   TO  FLG-NAIFUKU
           ELSE
               MOVE    ZERO                TO  FLG-NAIFUKU
           END-IF
           IF      (FLG-NAIFUKU            =   1  )   AND
                   (WRK-COM-INPUTCD(IDX)(1:1)  =   "6"   )
      *        剤数加算
               IF      WRK-COM-ZAIGATAKBN (IDX)    =   1  OR
                                                       2  OR
                                                       3
                   MOVE    1               TO  WRK-KONGO
               ELSE
      *            粉砕コードがある時、混合とする
                   IF      (IDX            >   1   )  AND
                           (WRK-COM-INPUTCD (IDX - 1)
                                           =   "099209901" )
                       MOVE    1               TO  WRK-KONGO
                   ELSE
                       ADD     1               TO  WRK-SONOTA
                   END-IF
               END-IF
      *        点数
               COMPUTE WRK-SYOKEIS2        =   WRK-COM-KISOTEN(IDX)
                                           *   WRK-COM-SURYO  (IDX)
               ADD     WRK-SYOKEIS2        TO  WRK-ZAITEN-G
      *
           END-IF
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      (FLG-NAIFUKU            =   1  )  AND
                   (WRK-COM-ZAIEND (IDX)   =   "1")
               MOVE    WRK-ZAITEN-G        TO  WRK-ZAITEN
               IF      WRK-ZAITEN-G        <=  205
                   IF      WRK-ZAITEN-G        =   ZERO
                       CONTINUE
                   ELSE
      *
                       IF      WRK-CHK-SRYSYUKBN   =   "211"
                                                   OR  "291"
                           ADD     1               TO  WRK-ZAICNT
                       ELSE
                           ADD     1               TO  WRK-ZAICNT2
                       END-IF
      *
                   END-IF
               ELSE
                   IF      WRK-CHK-SRYSYUKBN   =   "211"
                                               OR  "291"
                       COMPUTE WRK-ZAICNT      =   WRK-ZAICNT
                                               +   WRK-KONGO
                                               +   WRK-SONOTA
                   ELSE
                       COMPUTE WRK-ZAICNT2     =   WRK-ZAICNT2
                                               +   WRK-KONGO
                                               +   WRK-SONOTA
                   END-IF 
               END-IF
      *        点数加算
               IF      WRK-CHK-SRYSYUKBN   =   "211"
                                           OR  "291"
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-HKNKEI-KEIKEI
                                                       (WRK-CNT-HKN)
               ELSE
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI2
               END-IF
      *
               MOVE    ZERO            TO  WRK-KONGO
               MOVE    ZERO            TO  WRK-SONOTA
               MOVE    ZERO            TO  WRK-ZAITEN
               MOVE    ZERO            TO  WRK-ZAITEN-G
           END-IF
      *
      *    後発医薬品チェック
           IF     (WRK-COM-INPUTCD(IDX)(1:1) =   "6"  )  AND
                  (WRK-CHK-SRYSYUKBN   =   "212"  OR
                                           "222"  OR
                                           "232"  OR
                                           "292"     ) AND
                  (WRK-COM-KOUHATUKBN (IDX)
                                        =   1     )
               MOVE    1                   TO  FLG-KOUHATU
           END-IF
      *
      *    長期投薬品チェック
           IF      (WRK-COM-INPUTCD(IDX)(1:1) =   "6"  )  AND
                   (WRK-CHK-SRYSYUKBN (1:1)   =   "2"  )  AND
                   (WRK-COM-KAISU (IDX)       >=  28   )
               MOVE    1                   TO  FLG-CHOUKI
               IF      WRK-CHOU-HKNCOMBI   =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-CHOU-HKNCOMBI
               END-IF
           END-IF
      *    外用の場合
           IF      (WRK-COM-INPUTCD(IDX)(1:1) =   "6"  )  AND
                   (WRK-CHK-SRYSYUKBN (1:2)   =   "23" )  AND
                   (WRK-COM-KAISU2(IDX)       >=  28   )
               MOVE    1                   TO  FLG-CHOUKI
               IF      WRK-CHOU-HKNCOMBI   =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-CHOU-HKNCOMBI
               END-IF
           END-IF
      *
           .
       3000-SYOHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外　算定処理
      *****************************************************************
       3009-NYUINKAISU-SEC              SECTION.
      *
      *    調剤料算定なしの時、対象外
           IF      WRK-COM-SRYSYUKBN(IDX) =   "214"
                                          OR  "224"
                                          OR  "234"
               GO      TO      3009-NYUINKAISU-EXT
           END-IF
      *
           MOVE    1                   TO  WRK-KAISU
      *    内服は日数分作成する
           IF      WRK-COM-SRYKBN (IDX)    =   "21"
               IF      WRK-COM-KAISU (IDX)     >   WRK-KAISU
                   MOVE    WRK-COM-KAISU (IDX) TO  WRK-KAISU
               END-IF
           END-IF
      *
      *    入院回数以外
           IF       WRK-GMN-INPUTCD(IDX)(1:1)  NOT =   "*"
               MOVE    SPA-SRYYMD (7:2)    TO  IDN1
               MOVE    1                   TO  WRK-COM-NYUINDAY
                                                      (IDX IDN1)
      ******** IF      FLG-MADOKU-ARI      =   1
      *            MOVE    2               TO  TBL-NYUINDAY (IDN1)
      *        ELSE
      *            IF      TBL-NYUINDAY (IDN1) =   ZERO
      *                MOVE    1           TO  TBL-NYUINDAY (IDN1)
      *            END-IF
      *        END-IF
      *******  GO      TO      3009-NYUINKAISU-EXT
           END-IF
      *
      *    投薬で入院回数の時、回数集計
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF      WRK-COM-NYUINDAY (IDX IDN1) >   ZERO
                   MOVE    IDN1                TO  IDN4
                   PERFORM VARYING     IDN3    FROM    1   BY  1
                           UNTIL      (IDN3    >   WRK-KAISU )  OR
                                      (IDN4    >   31        )
                       IF      FLG-MADOKU-ARI      =   1
                           MOVE    2               TO  TBL-NYUINDAY
                                                              (IDN4)
                       ELSE
                           IF      TBL-NYUINDAY (IDN4) =   ZERO
                               MOVE    1           TO  TBL-NYUINDAY
                                                             (IDN4)
                           END-IF
                       END-IF
                       ADD     1                   TO  IDN4
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       3009-NYUINKAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外　算定処理
      *****************************************************************
       3001-INGAI-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHOSENN
           MOVE    ZERO                TO  FLG-SYOHO-CHK
           MOVE    ZERO                TO  FLG-SYOHO-CHK2
      *
      *    長期投薬加算算定時、保険の変更
           IF      SPA-CHOKITOFLG          =   2
               IF     (WRK-CHOU-HKNCOMBI   NOT =   WRK-HKNCOMBI) AND
                      (WRK-CHOU-HKNCOMBI   NOT =   SPACE       )
                   MOVE    WRK-CHOU-HKNCOMBI   TO  WRK-HKNCOMBI
               END-IF
           END-IF
           IF      WRK-SYO-HKNCOMBI    NOT =   SPACE
               MOVE    WRK-SYO-HKNCOMBI    TO  WRK-HKNCOMBI
           END-IF
      *
      *    処方箋を交付する診察料を算定している場合、算定なし
           IF      (LNK-ORCSC-ROUMANSEI-ARI    =   1  )  OR
                   (LNK-ORCSC-ROUMANSEI-ARI2   >   ZERO) OR
                   (LNK-ORCSC-NETAKIRI-ARI     =   1  )  OR
                   (LNK-ORCSC-NETAKIRI-ARI2    >   ZERO) OR
                   (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO) OR
                   (LNK-ORCSC-SEIKATU-ARI2     >   ZERO) OR
                   (LNK-ORCSC-SEIKATU-ARI      >   ZERO)
      *             在宅末期医療総合診察料あり
               OR  (SPA-ZAIMATU-ARI            >   ZERO )
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-SYOHO-CHK
           END-IF
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))   AND
                  (IDX-SYOHOU          =   ZERO )
               MOVE    1                   TO  FLG-SYOHO-CHK2
           END-IF
      *    処方せん料算定あり
           IF     (FLG-SYOHO-CHK       =   1   )  AND
                  (FLG-SYOHO-CHK2      =   1   )
               IF     (LNK-ORCSC-KIDCD     =   SPACE )  AND
                      (SPA-CHOZAI-CHK      =   ZERO  )
      *            調基算定チェック
                   PERFORM 30019-CHOUKI-CHK-SEC
               END-IF
               IF     (SPA-NAIFUKU-CHK     =   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )
      *            ７種類以上の場合、確認
                   IF      WRK-ZAICNT2         >=  7
      *            内服の薬剤逓減有りチェック
                       MOVE    1               TO  SPA-NAIFUKU-CHK
                       MOVE    "2002"          TO  LNK-ORCSC-KIDCD
                   END-IF
               END-IF
               IF     (LNK-ORCSC-KIDCD     =   SPACE )  AND
                      (SPA-ERRCD           =   SPACE )
      *            処方せん料編集
                   PERFORM 30011-SYOHOUSEN-SYORI-SEC
               ELSE
                   GO      TO      3001-INGAI-SYORI-EXT
               END-IF
           ELSE
      *        処方箋料を算定しない場合の処方箋料の有無
               IF     (FLG-SYOHO-CHK       =   1   )  AND
                      (IDX-SYOHOU      NOT =   ZERO)
                   MOVE    1               TO  FLG-SYOHOSENN
               END-IF
           END-IF
      *
      *    主科の時のみ行う
      *****IF     (IDX-KA          NOT =   1  )  OR
      *TEST
      *    新規の時のみ行う
      *!   IF     (LNK-ORCSC-SYORIFLG  =   1  )  OR
      *    処方せん料算定時の時のみ行う
           IF     (FLG-SYOHO-CHK       =   ZERO) OR
                  (FLG-SYOHOSENN       =   ZERO)
               GO      TO      3001-INGAI-SYORI-EXT
           END-IF
      *TEST
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEITOKU-FLG  =   "0")
               CONTINUE
           ELSE
      *        特定疾患処方管理加算の処理
               PERFORM 3003-INGAI-TOKUTEI-SEC
           END-IF
      *
           .
       3001-INGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   特定疾患処方管理加算の処理
      *****************************************************************
       3003-INGAI-TOKUTEI-SEC            SECTION.
      *
      *    特定疾患処方管理加算のチェック
           IF     (FLG-SYOHOSENN       =   1  )  AND
                  (FLG-TOKUTEI         =   1  )
      *     長期投薬加算、特定疾患処方管理加算の入力が無い時
            AND   (FLG-TOKUSYO1            =   ZERO)
            AND   (FLG-CHOKIKA1            =   ZERO)
               IF      (SPA-TOKUTEIFLG         =   ZERO )  OR
                       (SPA-CHOKITOFLG         =   ZERO )
      *            算定内容チェック
                   PERFORM 30011-INGAITOKUTEIKSN-CHK-SEC
      *            確認メッセージ表示か、加算なしの時
                   IF     (LNK-ORCSC-KIDCD NOT =   SPACE )  OR
                         ((SPA-TOKUTEIFLG          =  9  ) AND
                          (SPA-CHOKITOFLG          =  9  ))
                       GO      TO      3003-INGAI-TOKUTEI-EXT
                   END-IF
               END-IF
      *        長期投薬加算発生
               IF      SPA-CHOKITOFLG          =   2
      *            長期投薬加算
                   MOVE    "120003270"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
      *                自動発生分としない
      *T               MOVE    "1"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO1
                   GO      TO      3003-INGAI-TOKUTEI-EXT
               END-IF
      *        特定疾患処方管理
               IF      SPA-TOKUTEIFLG          =   2
                   MOVE    "120002570"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
      *                自動発生分としない
      *T               MOVE    "1"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO1
               END-IF
      *
           END-IF
      *
           .
       3003-INGAI-TOKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん料編集処理
      *****************************************************************
       30011-SYOHOUSEN-SYORI-SEC            SECTION.
      *
      *    ７種類以上確認後、しない場合は回数を変更する
           IF      SPA-NAIFUKU-CHK     =   1
               IF      SPA-NAIFUKU-OK      =   ZERO
                   MOVE    6               TO  WRK-ZAICNT2
               END-IF
           END-IF
      *
      *    処方箋料
           IF      FLG-KOUHATU         =   ZERO
      *        後発医薬品を含まない
               IF      WRK-ZAICNT2         >=  7
                   MOVE    "120002810"         TO  WRK-SRYCD
                   MOVE    "120003010"         TO  WRK-SRYCD2
               ELSE
                   MOVE    "120003010"         TO  WRK-SRYCD
                   MOVE    "120002810"         TO  WRK-SRYCD2
               END-IF
           ELSE
      *        後発医薬品を含む
               IF      WRK-ZAICNT2         >=  7
                   MOVE    "120002710"         TO  WRK-SRYCD
                   MOVE    "120002910"         TO  WRK-SRYCD2
               ELSE
                   MOVE    "120002910"         TO  WRK-SRYCD
                   MOVE    "120002710"         TO  WRK-SRYCD2
               END-IF
           END-IF
      *
           PERFORM 3002-KASAN-SYORI-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
               MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               MOVE    1               TO  FLG-SYOHOSENN
           END-IF
      *
           .
       30011-SYOHOUSEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    調基算定チェック処理
      *****************************************************************
       30019-CHOUKI-CHK-SEC            SECTION.
      *
           MOVE    "120001810"         TO  WRK-SRYCD
           PERFORM 3001-SANTEI-CHK-SEC
           IF      FLG-ERR             =   1
               IF      SPA-CHOZAI-CHK      =   ZERO
                   MOVE    1               TO  SPA-CHOZAI-CHK
                   MOVE    "K024"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "120001710"         TO  WRK-SRYCD
               PERFORM 3001-SANTEI-CHK-SEC
               IF      FLG-ERR             =   1
                   IF      SPA-CHOZAI-CHK      =   ZERO
                       MOVE    1               TO  SPA-CHOZAI-CHK
                       MOVE    "K024"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           MOVE    1                   TO  SPA-CHOZAI-CHK
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           .
       30019-CHOUKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外特定疾患加算チェック処理
      *****************************************************************
       30011-INGAITOKUTEIKSN-CHK-SEC            SECTION.
      *
           IF     (SPA-CHOKITOFLG          =   2  )  OR
                  (SPA-TOKUTEIFLG          =   2  )
               GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
           END-IF
      *
           IF     (FLG-CHOUKI          =   1   )  AND
                  (SPA-CHOKITOFLG      =   ZERO)
      *        長期投薬加算チェック
               MOVE    "120003270"         TO  WRK-SRYCD
               MOVE    1                   TO  FLG-CHOKIKSN
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
      *        算定可能かのチェック
               PERFORM 3001-KYOUTU-CHEK-SEC
      *        算定回数エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD3      NOT =   SPACE)
      *            算定済み
                   MOVE    9               TO  SPA-TOKUTEIFLG
                   MOVE    9               TO  SPA-CHOKITOFLG
                   GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
               END-IF
      *        併用算定エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD6      NOT =   SPACE)
                   IF      SPA-CHOKITOFLG-CHK  =   ZERO
                       MOVE    "K019"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-CHOKITOFLG-CHK
                   END-IF
                   MOVE    9               TO  SPA-TOKUTEIFLG
                   MOVE    9               TO  SPA-CHOKITOFLG
                   GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
               END-IF
      *        以外のエラー
               IF      FLG-ERR             =   1
                   MOVE    9               TO  SPA-CHOKITOFLG
               ELSE
      *            算定可能
                   MOVE    "0130"          TO  LNK-ORCSC-KIDCD
                   GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-TOKUTEIFLG      =   ZERO
      *        特定疾患処方管理
               MOVE    "120002570"         TO  WRK-SRYCD
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
      *        算定可能かのチェック
               PERFORM 3001-KYOUTU-CHEK-SEC
      *        算定エラー
               IF      FLG-ERR             =   1
                   MOVE    9               TO  SPA-TOKUTEIFLG
               ELSE
      *            算定可能
                   MOVE    "0113"          TO  LNK-ORCSC-KIDCD
               END-IF
           END-IF
      *
           .
       30011-INGAITOKUTEIKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内 算定処理
      *****************************************************************
       3002-INNAI-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHORYO
      *
           IF      WRK-ZAICNT          >=  7
      *        内服の薬剤逓減有りチェック
               IF      SPA-NAIFUKU-CHK     =   ZERO
                   MOVE    1               TO  SPA-NAIFUKU-CHK
                   MOVE    "2001"          TO  LNK-ORCSC-KIDCD
                   GO      TO      3002-INNAI-SYORI-EXT
               ELSE
                  IF      SPA-NAIFUKU-OK       =   1
      *            薬剤料逓減あり
                   COMPUTE WRK-SANTEN-9    =   WRK-KEIKEI
                                           *   90
                                           /   100.00
                                           +   0.5
                       MOVE    WRK-SANTEN-9    TO  WRK-SANTEN
                       PERFORM 3001-GENHEN-SEC
                   ELSE
      *            逓減なし
                       MOVE    6           TO  WRK-ZAICNT
                   END-IF
               END-IF
           END-IF
      *
      *    麻毒の算定されている保険で作成する
           IF      WRK-MADO1-HKNCOMBI  NOT =   SPACE
      *        内服
               MOVE    WRK-MADO1-HKNCOMBI  TO  WRK-NAI-HKNCOMBI
               MOVE    WRK-MADO1-HKNCOMBI  TO  WRK-HKNCOMBI
           ELSE
      *        外用
               IF      WRK-MADO2-HKNCOMBI  NOT =   SPACE
                   MOVE    WRK-MADO2-HKNCOMBI  TO  WRK-GAI-HKNCOMBI
                   MOVE    WRK-MADO2-HKNCOMBI  TO  WRK-HKNCOMBI
               END-IF
           END-IF
      *    長期投薬加算算定時、保険の変更
           IF      SPA-CHOKITOFLG          =   2
               IF     (WRK-CHOU-HKNCOMBI   NOT =   WRK-HKNCOMBI) AND
                      (WRK-CHOU-HKNCOMBI   NOT =   SPACE       )
                   IF     (WRK-MADO1-HKNCOMBI      =   SPACE )  AND
                          (WRK-MADO2-HKNCOMBI      =   SPACE )
                       MOVE    WRK-CHOU-HKNCOMBI   TO  WRK-HKNCOMBI
                   END-IF
               END-IF
           END-IF
      *
      **** 薬剤料逓減
      *    IF      WRK-ZAICNT          >=  7
      *        MOVE    "630010002"         TO  WRK-SRYCD
      *        MOVE    SPACE               TO  WRK-SRYCD2
      *        PERFORM 3002-KASAN-SYORI-SEC
      *        IF      FLG-ERR             =   ZERO
      *            MOVE    "210"           TO  LNK-ORCSC-SRYSYUKBN
      *                                                  (02 IDXS)
      *            MOVE    "21"            TO  LNK-ORCSC-SRYKBN
      *                                                  (02 IDXS)
      *            MOVE    WRK-NAI-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
      *                                                  (02 IDXS)
      *        END-IF
      *****END-IF
      *
      *    院外処方が有れば自動加算をしない
           IF     (FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )
               GO      TO      3002-INNAI-SYORI-EXT
           END-IF
      *
      *    外来
      *    内服調剤料加算
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )
               MOVE    "120000710"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "241"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-NAI-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
               END-IF
           END-IF
      *    外用調剤料加算
           IF      FLG-GAIYAK          =   1
               MOVE    "120001010"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "242"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "23"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-GAI-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
               END-IF
           END-IF
      *    処方料
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               IF      WRK-ZAICNT          >=  7
                   MOVE    "120002610"         TO  WRK-SRYCD
                   MOVE    "120001210"         TO  WRK-SRYCD2
               ELSE
                   MOVE    "120001210"         TO  WRK-SRYCD
                   MOVE    "120002610"         TO  WRK-SRYCD2
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    1               TO  FLG-SYOHORYO
               END-IF
           END-IF
      *    麻毒加算料
           IF      FLG-MADOKU          =   1
      *        麻毒調剤料
               MOVE    "120000110"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "260"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "26"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
      *        麻毒処方料
               MOVE    "120001310"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "260"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "26"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *
      *TEST
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEI-FLG      =   "0")
               CONTINUE
           ELSE
      *        薬剤情報提供料、調基
               PERFORM 30021-INNAI-AUTO1-SEC
           END-IF
      *
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEITOKU-FLG  =   "0")
               CONTINUE
           ELSE
      *        特定疾患処方管理加算
               PERFORM 30021-INNAI-TOKUTEI-SEC
           END-IF
           .
       3002-INNAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報提供料、調基算定処理
      *****************************************************************
       30021-INNAI-AUTO1-SEC     SECTION.
      *
      *    薬剤情報提供料
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               IF     (FLG-YAKUJYO         =   ZERO )
                   PERFORM 30022-INNAI-YAKUJYOUCHK-SEC
               END-IF
           END-IF
      *
      *    調剤技術基本料（入院外）
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
      *----(01.00.02) LINE ADD START ----------------------------------
      *           調基が１であること
             IF     (LNK-ORCSC-CHOKI-FLG   =   1    )
               AND  (FLG-CHOKI             =   ZERO )
      *----(01.00.02) LINE ADD END   ----------------------------------
               MOVE    "120001810"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               MOVE    1                   TO  FLG-ALL
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
      *            処方せん料の算定チェック
                   PERFORM 30024-SYOHOUSEN-CHK-SEC
                   IF      FLG-SYOHOU-ERR      =   ZERO
                       MOVE    "270"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "27"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                       MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                       MOVE    1               TO  FLG-CHOKI
                   ELSE
                       INITIALIZE             LNK-ORCSC-KASN-TBL2
                                                         (02 IDXS)
                       COMPUTE IDXS            =   IDXS   -   1
                   END-IF
               END-IF
             END-IF
           END-IF
           .
       30021-INNAI-AUTO1-EXT.
           EXIT.
      *****************************************************************
      *    特定疾患処方管理加算算定処理
      *****************************************************************
       30021-INNAI-TOKUTEI-SEC     SECTION.
      *
      *    特定疾患処方管理加算のチェック
           IF     (FLG-SYOHORYO            =   1  )  AND
                  (FLG-TOKUTEI         =   1  )
      *     長期投薬加算、特定疾患処方管理加算の入力が無い時
            AND   (FLG-TOKUSYO2            =   ZERO)
            AND   (FLG-CHOKIKA2            =   ZERO)
               IF      (SPA-TOKUTEIFLG         =   ZERO )  OR
                       (SPA-CHOKITOFLG         =   ZERO )
      *            算定内容チェック
                   PERFORM 30021-INNAITOKUTEIKSN-CHK-SEC
      *            確認メッセージ表示か、加算なしの時
                   IF     (LNK-ORCSC-KIDCD NOT =   SPACE )  OR
                         ((SPA-TOKUTEIFLG          =  9  ) AND
                          (SPA-CHOKITOFLG          =  9  ))
                       GO      TO      30021-INNAI-TOKUTEI-EXT
                   END-IF
               END-IF
      *        長期投薬加算発生
               IF      SPA-CHOKITOFLG          =   2
      *            長期投薬加算
                   MOVE    "120003170"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                       MOVE    WRK-CHOU-HKNCOMBI
                                               TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
      *                自動発生分としない
      *T               MOVE    "1"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO2
                   GO      TO      30021-INNAI-TOKUTEI-EXT
               END-IF
      *        特定疾患処方管理
               IF      SPA-TOKUTEIFLG          =   2
                   MOVE    "120002270"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
      *                自動発生分としない
      *T               MOVE    "1"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
      *TEST
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO2
               END-IF
      *
           END-IF
           .
       30021-INNAI-TOKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん料の算定チェック処理
      *****************************************************************
       30024-SYOHOUSEN-CHK-SEC     SECTION.
      *
           IF      SPA-SYOHOU-CHK      =   1
               MOVE    1                   TO  FLG-SYOHOU-ERR
           ELSE
               MOVE    ZERO                TO  FLG-SYOHOU-ERR
           END-IF
           MOVE    "120003010"         TO  WRK-SRYCD
           PERFORM 300241-SRYACT-CHK-SEC
      *
           IF      FLG-SYOHOU-ERR      =   ZERO
               MOVE    "120002810"         TO  WRK-SRYCD
               PERFORM 300241-SRYACT-CHK-SEC
           END-IF
      *
           IF      FLG-SYOHOU-ERR      =   ZERO
               MOVE    "120002710"         TO  WRK-SRYCD
               PERFORM 300241-SRYACT-CHK-SEC
           END-IF
      *
           IF      FLG-SYOHOU-ERR      =   ZERO
               MOVE    "120002910"         TO  WRK-SRYCD
               PERFORM 300241-SRYACT-CHK-SEC
           END-IF
      *
           IF      FLG-SYOHOU-ERR  NOT =   ZERO
               IF      SPA-SYOHOU-CHK      =   ZERO
                   MOVE    1               TO  SPA-SYOHOU-CHK
      *??          MOVE    "K023"          TO  SPA-ERRCD
               END-IF
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           END-IF
           .
       30024-SYOHOUSEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内薬剤情報提供料処理
      *****************************************************************
       300241-SRYACT-CHK-SEC     SECTION.
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDN1
      *
           MOVE    ZERO                TO  WRK-TEI-CNT
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   SPA-MAX-LINE  )  OR
                              (LNK-ORCSC-TEI-SRYCD(IDT)
                                           =   SPACE )
               IF      WRK-SRYCD       =   LNK-ORCSC-TEI-SRYCD(IDT)
                   ADD     LNK-ORCSC-TEI-KAISU(IDT)
                                           TO  WRK-TEI-CNT
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    "2"                 TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(1)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(2)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(3)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(4)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT   =   1 )  OR
                             (FLG-SYOHOU-ERR   =   1   )
      *        診療会計マスタ読み込み
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 940-SRYACCT-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                      SRYACCT-REC
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               PERFORM VARYING     IDX2    FROM    1   BY  1
      *!!!!!!!!!!!     UNTIL      (IDX2        >   IDN1  )  OR
      *            当月内で算定
                       UNTIL      (IDX2        >   31    )  OR
                                  (FLG-SYOHOU-ERR  =   1 )
      *            当日以前に算定
                   IF      IDN1            =   IDX2
                       IF      WRK-TEI-CNT     <=   ACCT-DAY (1 IDX2)
                           COMPUTE ACCT-DAY (1 IDX2)
                                                   =   ACCT-DAY (1 IDX2)
                                                   -   WRK-TEI-CNT
                       ELSE
                           MOVE    ZERO            TO  ACCT-DAY (1 IDX2)
                       END-IF
                   END-IF
                   IF      ACCT-DAY (1 IDX2)   >   ZERO
                       MOVE    1               TO  FLG-SYOHOU-ERR
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300241-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内薬剤情報提供料処理
      *****************************************************************
       30022-INNAI-YAKUJYOUCHK-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    老人区分判定
           IF      LNK-ORCSC-ROUJIN    =   0
               IF      LNK-ORCSC-YAKJYO-1      =    1
                                               OR   2
                    MOVE    "120002370"         TO  WRK-SRYCD
                    MOVE    SPACE               TO  WRK-SRYCD2
               END-IF
           ELSE
      *******  老人の時、手帳記載あり、なしの両方の算定チェック
      *        MOVE    "113701310"         TO  WRK-SRYCD
      *        PERFORM 3003-YAKJYO-CHK-SEC
      *        IF      FLG-YAKJYO          =   ZERO
      *            MOVE    "113701410"         TO  WRK-SRYCD
      *            PERFORM 3003-YAKJYO-CHK-SEC
      ******** END-IF
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               IF      FLG-YAKUJYO         =   ZERO
                   IF      LNK-ORCSC-YAKJYO-2  =    1
                                               OR   2
                       MOVE    "113701310"         TO  WRK-SRYCD
                       MOVE    "113701410"         TO  WRK-SRYCD2
                   END-IF
                   IF      LNK-ORCSC-YAKJYO-3  =   1
                                               OR   2
                       MOVE    "113701410"         TO  WRK-SRYCD
                       MOVE    "113701310"         TO  WRK-SRYCD2
                   END-IF
               END-IF
           END-IF
           IF      WRK-SRYCD           =   SPACE
               GO      TO      30022-INNAI-YAKUJYOUCHK-EXT
           END-IF
           MOVE    1                   TO  FLG-ALL
           PERFORM 3003-YAKJYO-CHK-SEC
           MOVE    ZERO                TO  FLG-ALL
      *****PERFORM 3002-KASAN-SYORI-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    "130"           TO  LNK-ORCSC-SRYSYUKBN(02 IDXS)
               MOVE    "13"            TO  LNK-ORCSC-SRYKBN   (02 IDXS)
               MOVE    1               TO  FLG-YAKUJYO
      *        自動発生分としない
               MOVE    "6"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
           END-IF
           .
       30022-INNAI-YAKUJYOUCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内特定疾患加算チェック処理
      *****************************************************************
       30021-INNAITOKUTEIKSN-CHK-SEC            SECTION.
      *
           IF     (SPA-CHOKITOFLG          =   2   )  OR
                  (SPA-TOKUTEIFLG          =   2  )
               GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
           END-IF
      *
           IF     (FLG-CHOUKI          =   1   )  AND
                  (SPA-CHOKITOFLG      =   ZERO)
      *        長期投薬加算チェック
               MOVE    "120003170"         TO  WRK-SRYCD
      *        算定可能かのチェック
               MOVE     1                  TO  FLG-CHOKIKSN
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
               PERFORM 3001-KYOUTU-CHEK-SEC
      *        算定回数エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD3      NOT =   SPACE)
      *            算定済み
                   MOVE    9               TO  SPA-TOKUTEIFLG
                   MOVE    9               TO  SPA-CHOKITOFLG
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
      *        併用算定エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD6      NOT =   SPACE)
                   IF      SPA-CHOKITOFLG-CHK  =   ZERO
                       MOVE    "K019"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-CHOKITOFLG-CHK
                   END-IF
                   MOVE    9               TO  SPA-TOKUTEIFLG
                   MOVE    9               TO  SPA-CHOKITOFLG
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
      *        以外のエラー
               IF      FLG-ERR             =   1
                   MOVE    9               TO  SPA-CHOKITOFLG
               ELSE
      *            算定可能
                   MOVE    "0130"          TO  LNK-ORCSC-KIDCD
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-TOKUTEIFLG      =   ZERO
      *        特定疾患処方管理
               MOVE    "120002270"         TO  WRK-SRYCD
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
      *        算定可能かのチェック
               PERFORM 3001-KYOUTU-CHEK-SEC
      *        算定エラー
               IF      FLG-ERR             =   1
                   MOVE    9               TO  SPA-TOKUTEIFLG
               ELSE
      *            算定可能
                   MOVE    "0113"          TO  LNK-ORCSC-KIDCD
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
           END-IF
      *
           .
       30021-INNAITOKUTEIKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤料逓減自動算定処理
      *****************************************************************
       3001-GENHEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE)  AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
                       OR    ( SPA-ERRCD       NOT  =   SPACE )
      *
             IF      WRK-COM-SRYKA(IDX)    =   WRK-SRYKA
      *
               MOVE    WRK-COM-SRYSYUKBN  (IDX)  TO  WRK-CHK-SRYSYUKBN
      *----(01.00.01) LINE ADD START ----------------------------------
               IF      WRK-CHK-SRYSYUKBN       =   SPACE
                   EVALUATE    WRK-COM-SRYKBN (IDX)
                       WHEN    "21"
                           MOVE    "210"       TO  WRK-CHK-SRYSYUKBN
                       WHEN    "22"
                           MOVE    "220"       TO  WRK-CHK-SRYSYUKBN
                       WHEN    "23"
                           MOVE    "230"       TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               END-IF
      *        内服薬チェック
      *          臨時で１４日以内の時は対象外
               IF     ( WRK-CHK-SRYSYUKBN      =   "210"
                                               OR  "211" )  OR
                      ((WRK-CHK-SRYSYUKBN      =   "290"
                                               OR  "291" )  AND
      **************** (WRK-COM-KAISU (IDX)    >=  14 ))
                       (WRK-COM-KAISU (IDX)    >   14 ))
                   MOVE    1                   TO  FLG-NAIFUKU
               ELSE
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
      ******** IF     (WRK-COM-SRYKBN   (IDX)  =   "21"  )  AND
      *               (WRK-CHK-SRYSYUKBN       =   "210" OR
      *********                                    "211" )  AND
               IF     (FLG-NAIFUKU             =   1     )  AND
                      (FLG-ZAI                 =   ZERO  )
                   IF      WRK-GMN-INPUTCD(IDX)(1:1)   =   "."
      *                約束セットの時、約束セットの後に追加
                       IF      WRK-GMN-INPUTCD(IDX + 1)(1:1)   =   "S"
                           COMPUTE IDX-Z           =   IDX     +   2
                       ELSE
                           COMPUTE IDX-Z           =   IDX     +   1
                       END-IF
                   ELSE
                       MOVE    IDX             TO  IDX-Z
                   END-IF
                   MOVE    "820000047"     TO  WRK-SRYCD
                   PERFORM 30011-GENTUIKA-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  WRK-COM-CUR (IDX)
                   END-IF
                   MOVE    1               TO  FLG-ZAI
               END-IF
             END-IF
      *
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO            TO  FLG-ZAI
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
               ADD     1               TO  WRK-GMN-MAX
           END-PERFORM
      *
      *    薬剤料逓減
           MOVE    ZERO                TO  WRK-SANTEN-G
           COMPUTE WRK-SANTEN-KEI      =   WRK-KEIKEI
                                       -   WRK-SANTEN
      *
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKN
               IF      IDX-GEN         =   WRK-CNT-HKN
                   COMPUTE WRK-SANTENSU    =   WRK-SANTEN-KEI
                                           -   WRK-SANTEN-G
               ELSE
                   COMPUTE WRK-SANTEN-9    =   WRK-HKNKEI-KEIKEI
                                                       (IDX-GEN)
                                           *   90
                                           /   100.00
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-HKNKEI-KEIKEI
                                                       (IDX-GEN)
                                           -   WRK-SANTEN-GEN
                   ADD     WRK-SANTENSU    TO  WRK-SANTEN-G
               END-IF
      *
               MOVE    "630010002"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "210"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
               END-IF
           END-PERFORM
      *
      *
           .
       3001-GENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤料逓減追加処理
      *****************************************************************
       30011-GENTUIKA-SEC         SECTION.
      *
           MOVE    IDX-Z               TO  IDX-S
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           IF      WRK-COM-INPUTCD (IDX-Z) =   WRK-SRYCD
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "1"                 TO  LNK-SC92-KBN
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
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
               IF      SPA-ERRCD           =   "0020"  OR  "0007"
                   MOVE    "7007"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               GO      TO      30011-GENTUIKA-EXT
           END-IF
      *
           IF     (IDX-S               =   SPA-MAX-LINE )  OR
                  (WRK-COM-SRYKBN (IDX-S + 1)(1:1) NOT =   "2")
               MOVE    "21"                TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    "21"                TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    "210"               TO  WRK-COM-SRYSYUKBN(IDX-S)
               MOVE    WRK-SRYKA           TO  WRK-COM-SRYKA    (IDX-S)
               MOVE    WRK-HKNCOMBI        TO  WRK-COM-HKNCOMBI (IDX-S)
           ELSE
               MOVE    WRK-COM-SRYKBN (IDX-S + 1)
                                           TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-COM-SRYSYUKBN (IDX-S + 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-S)
               MOVE    WRK-COM-SRYKA     (IDX-S + 1)
                                           TO  WRK-COM-SRYKA    (IDX-S)
               MOVE    WRK-COM-HKNCOMBI  (IDX-S + 1)
                                           TO  WRK-COM-HKNCOMBI (IDX-S)
           END-IF
      *
      *    自動追加分（入力時に追加）
           MOVE    "3"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           .
       30011-GENTUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の自動算定処理
      *****************************************************************
       30021-NYUIN-INNAI-SYORI-SEC         SECTION.
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU
      *
      *    調剤料（入院）加算
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               MOVE    "120001110"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 300211-NYUKASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    ZERO            TO  FLG-DAY
                   PERFORM 300212-NISUU-SET-SEC
                   MOVE    "240"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "24"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *    麻毒加算料
           IF      FLG-MADOKU          =   1
      *        麻毒調剤料
               MOVE    "120000410"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 300211-NYUKASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-DAY
                   PERFORM 300212-NISUU-SET-SEC
                   MOVE    "260"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "26"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEI-FLG      =   "0")
               GO      TO      30021-NYUIN-INNAI-SYORI-EXT
           END-IF
      *
      *    調剤技術基本料（入院）
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
      *        調基が１であること
               IF      LNK-ORCSC-CHOKI-FLG   =   1
      **********   IF      SPA-HOSPSBT         =   1
      *                病院
                       MOVE    "120001710"         TO  WRK-SRYCD
      ************ ELSE
      *                診療所
      *                MOVE    "120001810"         TO  WRK-SRYCD
      *************END-IF
                   MOVE    SPACE               TO  WRK-SRYCD2
                   MOVE    1                   TO  FLG-ALL
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       PERFORM 30024-SYOHOUSEN-CHK-SEC
                       IF      FLG-SYOHOU-ERR      =   ZERO
                           MOVE    3               TO  FLG-DAY
                           PERFORM 300212-NISUU-SET-SEC
                           MOVE    "270"       TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                           MOVE    "27"        TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                           MOVE    "4"         TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                       ELSE
                           INITIALIZE             LNK-ORCSC-KASN-TBL2
                                                         (02 IDXS)
                           COMPUTE IDXS            =   IDXS   -   1
                       END-IF
                   ELSE
      *                病院で外来で算定済みかの判断
                       IF     (WRK-SRYCD           =   "120001710") AND
                              (SPA-CHOKICHK-FLG    =   "1"     )   AND
                              (SPA-CHOKI-ARI       =   SPACE   )
                       AND    (SPA-CHOZAI-CHK      =   ZERO    )
                           PERFORM 300211-CHOZAI-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       30021-NYUIN-INNAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の調剤技術基本料算定チェック処理
      *****************************************************************
       300211-CHOZAI-CHK-SEC           SECTION.
      *
           IF      SPA-CHOKI-ARI       =   SPACE
      *        入院の調剤料のチェック
               MOVE    "120001110"         TO  WRK-SRYCD
               PERFORM 3002110-SRYACT-CHOZAI-SEC
               IF      FLG-SRYACCT-ARI     =   1
                   MOVE    "1"             TO  SPA-CHOKI-ARI
               END-IF
           END-IF
      *
           IF      SPA-CHOKI-ARI       =   SPACE
               MOVE    "120001810"         TO  WRK-SRYCD
               PERFORM 3001-SANTEI-CHK-SEC
               IF      FLG-ERR             =   1
                   IF      SPA-CHOZAI-CHK      =   ZERO
                       MOVE    1               TO  SPA-CHOZAI-CHK
                       MOVE    "K017"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           .
       300211-CHOZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院調剤料算定判定処理
      *****************************************************************
       3002110-SRYACT-CHOZAI-SEC     SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "24"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
               IF     (ACCT-HKNCOMBI     =   "9999" ) OR
                      (ACCT-ZAIKAISU     =   ZERO   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-SRYACCT-ARI
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3002110-SRYACT-CHOZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の日数自動算定処理
      *****************************************************************
       300212-NISUU-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDN1
           MOVE    ZERO                TO  IDN2
      *
           INITIALIZE                      WRK-NISUU-AREA
           MOVE    ZERO                TO  WRK-H-DAY
           MOVE    SPACE               TO  WRK-H-SEL
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   32
               IF     (IDN1                =   32  )  OR
                      (IDN1                >   WRK-MATU)  OR
                      (TBL-NYUINDAY(IDN1)  =   ZERO)
                   IF      WRK-H-DAY       NOT =   ZERO
                       IF      WRK-H-SEL   NOT =   SPACE
                           ADD     1               TO  IDN2
                           MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                           MOVE    WRK-H-SEL       TO  WRK-SEL(IDN2 - 1)
                       END-IF
                       MOVE    ZERO            TO  WRK-H-DAY
                       MOVE    SPACE           TO  WRK-H-SEL
                   END-IF
               ELSE
                   IF    ((FLG-DAY                 =   ZERO
                                                   OR  3   )  AND
                          (TBL-NYUINDAY (IDN1) NOT =   ZERO ))  OR
                         ((FLG-DAY                 =   1    )  AND
                          (TBL-NYUINDAY (IDN1)     =   2    ))
                       MOVE    1               TO  FLG-OK3
                   ELSE
                       MOVE    ZERO            TO  FLG-OK3
                   END-IF
                 IF      FLG-OK3             =   1
                   IF      WRK-H-DAY           =   ZERO
                       MOVE    IDN1            TO  WRK-H-DAY
                       ADD     1               TO  IDN2
                       MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
      *                調基は最初の日とする
                       IF      FLG-DAY         =   3
                           MOVE    32              TO  IDN1
                       END-IF
                   ELSE
                       IF      WRK-H-DAY       =  (IDN1  -  1 )
                           MOVE    "-"             TO  WRK-H-SEL
                       END-IF
                       MOVE    IDN1            TO  WRK-H-DAY
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF     (WRK-SEL (IDN1)      =   SPACE )  AND
                      (IDN1                <   31    )
                   IF      WRK-DAY (IDN1 + 1 )     NOT =   ZERO
                       MOVE    ","          TO  WRK-SEL (IDN1)
                   END-IF
               ELSE
                   IF     (IDN1                    =   31    )  OR
                          (WRK-DAY (IDN1 + 1 )     =   ZERO  )
                       MOVE    SPACE        TO  WRK-SEL (IDN1)
                   END-IF
               END-IF
           END-PERFORM
           INITIALIZE                      LNK-ORCSC-INPUTCD-G (02 IDXS)
      *
      *    日数なしか調基は最初の日とする
           MOVE    1                   TO  WRK-KAISU
           IF    ( WRK-KAISU           =   1  OR  ZERO ) OR
                 ( FLG-DAY             =   3    )
               MOVE    SPACE           TO  WRK-KAISU-H
               MOVE    ZERO            TO  IDN4
           ELSE
               MOVE    WRK-KAISU       TO  WRK-KAISU-Z
               MOVE    SPACE           TO  WRK-KAISU-H
               MOVE    ZERO            TO  IDN4
               PERFORM VARYING     IDN1    FROM    1   BY  1
                       UNTIL       IDN1    >   4
                   IF     (WRK-KAISU-X (IDN1:1)    NOT =   SPACE) OR
                          (IDN4                    >   ZERO  )
                       ADD     1               TO  IDN4
                       MOVE    WRK-KAISU-X (IDN1:1)    TO  WRK-KAISU-H
                                                             (IDN4:1)
                   END-IF
               END-PERFORM
           END-IF
           MOVE    1                   TO  IDN2
           MOVE    1                   TO  IDN3
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )  OR
                              (IDN2            >=  5  )  OR
                              (WRK-DAY (IDN1)  =   ZERO )
               IF      IDN3                >   19
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
               IF      LNK-ORCSC-INPUTCD(02 IDXS IDN2)   =   SPACE
                   STRING  "*"             DELIMITED  BY  SIZE
                           WRK-KAISU-H     DELIMITED  BY  SPACE
                           "/"             DELIMITED  BY  SIZE
                           INTO            LNK-ORCSC-INPUTCD
                                                   (02 IDXS IDN2)
                   END-STRING
                   ADD     2               TO  IDN3
                   ADD     IDN4            TO  IDN3
               END-IF
               IF      WRK-SEL (IDN1)      =   SPACE
                   MOVE    ","             TO  WRK-SEL (IDN1)
               END-IF
               MOVE    WRK-NISUU-TBL(IDN1) TO  LNK-ORCSC-INPUTCD
                                                  (02 IDXS IDN2)(IDN3:3)
               ADD     3                   TO  IDN3
           END-PERFORM
      * 
           .
       300212-NISUU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定処理(入院）
      *****************************************************************
       300211-NYUKASAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   1
               GO      TO      300211-NYUKASAN-SYORI-EXT
           END-IF
      *
           PERFORM 3001-KYOUTU-CHEK-SEC
           IF      FLG-ERR             =   1
               GO      TO      300211-NYUKASAN-SYORI-EXT
           END-IF
      *
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      300211-NYUKASAN-SYORI-EXT
           END-IF
      *
      *        算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR             =   ZERO
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (02 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
      *
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
           END-IF
      *
           .
       300211-NYUKASAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定処理
      *****************************************************************
       3002-KASAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
            IF     (WRK-COM-SRYKA (IDX)    =   WRK-SRYKA)  OR
                   (FLG-ALL                =   1        )  OR
                   (SPA-NYUGAIKBN          =   "1"      )
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                     ((WRK-SRYCD2          NOT =   SPACE )  AND
                      (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2))
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *        処方箋料のチェック
               EVALUATE    WRK-SRYCD
                   WHEN    "120002710"
                   WHEN    "120002910"
                   WHEN    "120003010"
                   WHEN    "120002810"
                       EVALUATE    WRK-COM-INPUTCD(IDX)
                           WHEN    "120002710"
                           WHEN    "120002910"
                           WHEN    "120003010"
                           WHEN    "120002810"
                               MOVE    1               TO  FLG-ERR
                       END-EVALUATE
               END-EVALUATE
             END-IF
           END-PERFORM
           MOVE    ZERO                TO  FLG-ALL
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
      *    継続分にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30      )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-TBL-SRYCD(IDX)   =   SPACE)
               IF     (WRK-TBL-SRYCD(IDX)      =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-TBL-SRYCD(IDX)      =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *        処方箋料のチェック
               EVALUATE    WRK-SRYCD
                   WHEN    "120002710"
                   WHEN    "120002910"
                   WHEN    "120003010"
                   WHEN    "120002810"
                       EVALUATE    WRK-TBL-SRYCD (IDX)
                           WHEN    "120002710"
                           WHEN    "120002910"
                           WHEN    "120003010"
                           WHEN    "120002810"
                               MOVE    1               TO  FLG-ERR
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM
           IF      FLG-ERR             =   1
      *        継続、入院の場合算定しない
               IF     (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
      *            入院の時
                   OR (SPA-NYUGAIKBN        =   "1"  )
                   GO      TO      3002-KASAN-SYORI-EXT
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
                   IF      FLG-SINSATU         =   ZERO
                       MOVE    1               TO  SPA-SYOHO-KBN
                       MOVE    WRK-TBL-HKNCOMBI
                                               TO  SPA-SYOHO-HKN
                       PERFORM 30021-DOUJI-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 3001-KYOUTU-CHEK-SEC
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
      *    算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR             =   ZERO
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (02 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
               IF      WRK-SRYCD       =   "630010002"
      *********    COMPUTE LNK-ORCSC-TENSU (02 IDXS)
      *                                    =   WRK-KEIKEI
      *********                            -   WRK-SANTEN
                   MOVE    WRK-SANTENSU    TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
               END-IF
               MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                         (02 IDXS)
               MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
           END-IF
      *
           .
       3002-KASAN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    共通チェック処理
      *****************************************************************
       3001-KYOUTU-CHEK-SEC         SECTION.
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
      *
           COMPUTE LNK-SC92-GMN-IDX    =   WRK-GMN-MAX
                                       +   1
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE 
               MOVE    SPA-MAX-LINE        TO  LNK-SC92-GMN-IDX
           END-IF
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       HOZ-SCR-REC
                                       SPA-AREA
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        追加なし
               MOVE    1               TO  FLG-ERR
           ELSE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           END-IF
      *    長期の時、併用・回数エラーは別へ
           IF      FLG-CHOKIKSN        =   1
               MOVE    LNK-SC92-ERRCD3     TO  WRK-ERRCD3
               MOVE    LNK-SC92-ERRCD6     TO  WRK-ERRCD6
               MOVE    SPACE               TO  LNK-SC92-ERRCD3
               MOVE    SPACE               TO  LNK-SC92-ERRCD6
               IF     (SPA-ERRCD           NOT =   SPACE )  OR
                      (LNK-SC92-ERRAREA    NOT =   SPACE )
      *            追加なし
                   MOVE    1               TO  FLG-ERR
               ELSE
                   MOVE    ZERO            TO  FLG-ERR
               END-IF
           END-IF
           MOVE    ZERO                TO  FLG-CHOKIKSN
      *
           .
       3001-KYOUTU-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤料の他保険の算定済み編集処理
      *****************************************************************
       30021-DOUJI-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-HEN-MEI
           EVALUATE    WRK-SRYCD
               WHEN    "120000710"
                   MOVE    "内服調剤料"        TO  WRK-HEN-MEI
               WHEN    "120001010"
                   MOVE    "外用調剤料"        TO  WRK-HEN-MEI
               WHEN    "120001210"
               WHEN    "120002610"
                   MOVE    "処方料"            TO  WRK-HEN-MEI
               WHEN    "120002710"
               WHEN    "120002910"
               WHEN    "120003010"
               WHEN    "120002810"
                   MOVE    "処方せん料"         TO  WRK-HEN-MEI
           END-EVALUATE
           IF      WRK-HEN-MEI         =   SPACE
               GO      TO      30021-DOUJI-HEN-EXT
           END-IF
      *
           IF      WRK-HENSYU-MEISYO   =   SPACE
               MOVE    WRK-HEN-MEI         TO  WRK-HENSYU-MEISYO
           ELSE
               STRING  WRK-HENSYU-MEISYO   DELIMITED  BY  SPACE
                       "、"                DELIMITED  BY  SIZE
                       WRK-HEN-MEI         DELIMITED  BY  SPACE
                                           INTO      WRK-HENSYU-MEISYO
               END-STRING
           END-IF
      *
           .
       30021-DOUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定処理
      *****************************************************************
       3002-KASAN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *        処方箋料のチェック
               EVALUATE    WRK-SRYCD
                   WHEN    "120002710"
                   WHEN    "120002910"
                   WHEN    "120003010"
                   WHEN    "120002810"
                       EVALUATE    WRK-COM-INPUTCD (IDX)
                           WHEN    "120002710"
                           WHEN    "120002910"
                           WHEN    "120003010"
                           WHEN    "120002810"
                               MOVE    1               TO  FLG-ERR
                      END-EVALUATE
               END-EVALUATE
           END-PERFORM
      *
           .
       3002-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定チェック（薬剤情報提供料）処理
      *****************************************************************
       3003-YAKJYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-YAKUJYO
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                     ((WRK-SRYCD2          NOT =   SPACE  )  AND
                       (WRK-COM-INPUTCD(IDX)   =   WRK-SRYCD2))
                   MOVE    1                   TO  FLG-YAKUJYO
     ************* MOVE    WRK-COM-MAE-AUTOKBN(IDX)
     ************                          TO   WRK-COM-AUTOKBN(IDX)
               END-IF
           END-PERFORM
           IF      FLG-YAKUJYO         =   1
               MOVE    1                   TO  FLG-ERR
               GO      TO      3003-YAKJYO-CHK-EXT
           END-IF
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
     **********MOVE    1                   TO  FLG-YAKJYO
               GO      TO      3003-YAKJYO-CHK-EXT
           END-IF
      *
      *        算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           MOVE    ZERO            TO  FLG-ERR
           IF     (LNK-SC91-ERRCD      =   SPACE )  AND
                  (LNK-SC91-ERRCD2     =   SPACE )
               MOVE    ZERO            TO  FLG-ERR
           ELSE
               IF     (LNK-ORCSC-ROUJIN    =   0    )  AND
                      (LNK-ORCSC-YAKJYO-1  =   1    )
                   MOVE    1                   TO  FLG-ERR
               END-IF
               IF     (LNK-ORCSC-ROUJIN    =   1    )  AND
                     ((LNK-ORCSC-YAKJYO-2  =   1    )  OR
                      (LNK-ORCSC-YAKJYO-3  =   1    ))
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
           IF      FLG-ERR             =   ZERO
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (02 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
               MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                         (02 IDXS)
               MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
           END-IF
      *
           .
       3003-YAKJYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       3002-NENREI-KASAN-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (TNS-AGEKSNSRYCD(IDX2)   =   SPACE  OR
                                                           ZERO )
               MOVE    TNS-AGEKSNSRYCD(IDX2)   TO  WRK-SRYCD
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
      *            年齢チェック
                   PERFORM 30021-NENREI-CHK-SEC
                   IF      FLG-CHK              =   ZERO
      *
      *                加算料計算
                       MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD-KS
                                                       (02 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU-KS
                                                       (02 IDXS)
                       MOVE    4               TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3002-NENREI-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       30021-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-KGNAGE          TO  WRK-KGNAGE(2:2)
           MOVE    TNS-JGNAGE          TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-KGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY   =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM   =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD   <   28   )
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY   >=  WRK-KGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-JGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD    <   28   )
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY   <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *
           .
       30021-NENREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       3001-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSC91AREA
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA
                                       TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
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
      *    継続の時、算定分の編集処理
      *****************************************************************
       30010-JYURRK-SYORI-SEC         SECTION.
      *
      *    継続分の時、算定
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    1                   TO  JYURRK-RENNUM
               MOVE    "key5"              TO  WRK-ORC-DBPATH
           ELSE
      *        継続の時、算定分の編集
               IF     (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
                   MOVE    LNK-ORCSC-DOUJI-RENNUM  TO  JYURRK-RENNUM
                   MOVE    "key5"           TO  WRK-ORC-DBPATH
               ELSE
                   MOVE    "key4"           TO  WRK-ORC-DBPATH
               END-IF
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL    FLG-JYURRK         =   1
               MOVE     ZERO            TO  FLG-JYURRK-END
               IF      (LNK-ORCSC-SYORIFLG     =   1  )   AND
                       (LNK-ORCSC-DENPNUM      =   JYURRK-DENPNUM)
      *            訂正の時対象外
                   MOVE    1               TO  FLG-JYURRK-END
               ELSE
      *            入院、継続の時、算定分の編集
                   IF      (SPA-NYUGAIKBN        =   "1"    )  OR
                           (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
                       CONTINUE
                   ELSE
                       IF      JYURRK-HKNCOMBINUM  =   SPA-HKNCOMBINUM
                           MOVE    1               TO  FLG-JYURRK-END
                       END-IF
                   END-IF
               END-IF
      *        包括まとめは対象外
               IF      JYURRK-HKNCOMBINUM      =   "9999"
                   MOVE    1                   TO  FLG-JYURRK-END
               END-IF
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX                 >   15  )  OR
                                  (FLG-JYURRK-END      =   1    ) OR
                                  (JYURRK-ZAINUM(IDX)  =   ZERO )
      *            診療会計マスタ読み込み
                   MOVE    SPACE               TO  SRYACCT-REC
                   INITIALIZE                      SRYACCT-REC
                   MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
                   MOVE    SPA-PTID            TO  ACCT-PTID
                   MOVE    SPA-SRYKA           TO  ACCT-SRYKA
                   MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
                   MOVE    JYURRK-ZAINUM(IDX)  TO  ACCT-ZAINUM
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 940-SRYACCT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
                   PERFORM UNTIL    FLG-SRYACCT    =   1
      *
                      COMPUTE IDX-Y3              =   JYURRK-RENNUM
                                                  +   1
      *ver.4.8
      ****            IF      IDX-Y3              >   4
      *                    MOVE    4               TO  IDX-Y3
      *****           END-IF
                      MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                      IF      (ACCT-DAY (IDX-Y3 IDX-Y2)
                                                   >   ZERO  ) AND
                              (ACCT-SRYKBN(1:1)    =   "2"   OR
                               ACCT-SRYKBN         =   "80"  )
      *                    診療行為マスター編集
                           PERFORM 300011-YAKFTN-SRYACT-SEC
                       END-IF
      *
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 940-SRYACCT-READ-SEC
                   END-PERFORM
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30010-JYURRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       300011-YAKFTN-SRYACT-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
      *
      *            投薬の自動発生チェック用
                   IF     ( SRY-SRYCD (IDX2)(1:1)  =   "1"   )  AND
                          ( SRY-SRYKBN(1:1)        =   "2"   OR
                            SRY-SRYSYUKBN          =   "820" )
                       ADD     1                   TO  IDX-TBL
                       EVALUATE     SRY-SRYCD (IDX2)
      *                処方箋料
      *                    WHEN    "180016410"
      *                    WHEN    "180011110"
      *                        MOVE    "180011110"     TO  WRK-SRYCD
      *                 処方料
                           WHEN    "120002610"
                           WHEN    "120001210"
                               MOVE    "120001210"     TO  WRK-SRYCD
                           WHEN     OTHER
                               MOVE      SRY-SRYCD (IDX2)
                                                   TO  WRK-SRYCD
                       END-EVALUATE
                       MOVE    WRK-SRYCD           TO  WRK-TBL-SRYCD
                                                        (IDX-TBL)
                       MOVE    JYURRK-HKNCOMBINUM  TO  WRK-TBL-HKNCOMBI
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300011-YAKFTN-SRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
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
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
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
