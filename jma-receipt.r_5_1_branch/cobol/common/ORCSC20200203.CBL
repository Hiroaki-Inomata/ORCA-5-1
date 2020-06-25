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
       PROGRAM-ID.                 ORCSC20200203.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（薬剤）
      *                      Ｈ１４．４改正まで
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    多々納       01/04/18  麻毒の判定、薬剤数カウント
      *                                     の修正等
      *  01.00.02    多々納       01/05/21  診療種別の判定の追加
      *  02.00.06    NACL-多々納  05/12/14  MONFUNC 対応 他
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
           03  FLG-YAKJYO         PIC 9(01).
           03  FLG-ADD            PIC 9(01).
      *
           03  FLG-TOKUSYO1         PIC 9(01).
           03  FLG-TOKUSYO2         PIC 9(01).
           03  FLG-SYOHORYO         PIC 9(01).
           03  FLG-SYOHOSENN        PIC 9(01).
      *    内服
           03  FLG-NAIFUKU          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
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
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V9(8).
           03  WRK-GAIKEI          PIC 9(08)V9(8).
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-IDX             PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
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
      *
               05  WRK-CHK-SRYSYUKBN   PIC X(03).
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *
      *    継続の時、自動発生のチェック
       01  WRK-CONTCHK-TBL.
           03  WRK-CONTCHK-OCC         OCCURS  30.
               05  WRK-TBL-SRYCD       PIC X(09).
           03  WRK-IDX-Y               PIC 9(04).
           03  IDX-TBL                 PIC 9(04).
           03  FLG-CNTCHK              PIC 9(01).
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
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
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
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
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
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "S"
                       CONTINUE
      *
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
           EVALUATE    WRK-GMN-INPUTCD(IDX-Z)(2:3)
               WHEN    "213"
               WHEN    "223"
               WHEN    "233"
      *            投薬の処方のみは、投薬が算定できない時のみ
                   IF      (LNK-ORCSC-ROUMANSEI-ARI    =   ZERO) AND
                           (LNK-ORCSC-ROUMANSEI-ARI2   =   ZERO) AND
                           (LNK-ORCSC-NETAKIRI-ARI     =   ZERO) AND
                           (LNK-ORCSC-NETAKIRI-ARI2    =   ZERO) AND
                           (LNK-ORCSC-SYONIKA-ARI      =   ZERO)
                       MOVE    "0074"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
           END-EVALUATE
           .
       10010-SRYKBN-CHK-EXT.
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
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
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
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    器材点数
           MOVE    ZERO                TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *
      *    点数計　計算
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
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
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "23"
      *********IF      WRK-COM-SURYO2(IDX-Z)   =   ZERO
      *            MOVE    WRK-COM-SURYO (IDX-Z)   TO
      *                                        WRK-COM-SURYO2(IDX-Z)
      *        END-IF
      *        IF      WRK-COM-KAISU2(IDX-Z)   =   ZERO 
      *            MOVE    LNK-ORCSC-KAISU     TO
      *                                        WRK-COM-KAISU2(IDX-Z)
      *********END-IF
               COMPUTE WRK-COM-SURYO (IDX-Z)   =   WRK-COM-SURYO2(IDX-Z)
                                               *   LNK-ORCSC-KAISU
               MOVE    1                   TO  LNK-SC90-KAISU
           ELSE
               MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           END-IF
      *
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
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
           INITIALIZE                      WRK-CONTCHK-TBL
      *
      *    継続の時、算定分の編集
           IF      LNK-ORCSC-CONTKBN    =   "1"  OR  "2"
               PERFORM 30010-JYURRK-SYORI-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-NAIYAK
           MOVE    ZERO                TO  FLG-TONPUK
           MOVE    ZERO                TO  FLG-GAIYAK
           MOVE    ZERO                TO  FLG-NAIYAK2
           MOVE    ZERO                TO  FLG-TONPUK2
           MOVE    ZERO                TO  FLG-GAIYAK2
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-TOKUSYO1
           MOVE    ZERO                TO  FLG-TOKUSYO2
      *
           INITIALIZE                  WRK-ZAIKEI-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)
      *
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
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
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
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "211"
                   WHEN    "291"
                       MOVE     1              TO  FLG-NAIYAK
                   WHEN    "212"
                   WHEN    "292"
                       MOVE     1              TO  FLG-NAIYAK2
                   WHEN    "221"
                       MOVE     1              TO  FLG-TONPUK
                   WHEN    "222"
                       MOVE     1              TO  FLG-TONPUK2
                   WHEN    "231"
                       MOVE     1              TO  FLG-GAIYAK
                   WHEN    "232"
                       MOVE     1              TO  FLG-GAIYAK2
               END-EVALUATE
      *
      *     処方のみの時以下、算定なし
               IF      WRK-CHK-SRYSYUKBN       =   "213" OR
                                                   "223" OR
                                                   "233"
                   CONTINUE
               ELSE
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "211"
                   WHEN    "221"
                   WHEN    "231"
                   WHEN    "291"
      *                麻毒区分判定
                       IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
      *----(01.00.01) LINE UPD START ----------------------------------
      *********************IF      WRK-COM-MADOKUKBN (IDX)     =   1
                           IF      WRK-COM-MADOKUKBN (IDX) NOT =   ZERO
      *----(01.00.01) LINE UPD END   ----------------------------------
                               MOVE   1                TO  FLG-MADOKU
                           END-IF
                       END-IF
               END-EVALUATE
      *
      *        内服薬チェック
      *          臨時で１４日以内の時は対象外
               IF      (WRK-CHK-SRYSYUKBN (1:2)   =   "21" ) OR
                       (WRK-CHK-SRYSYUKBN (1:2)   =   "29" AND
                        WRK-COM-KAISU (IDX)       >=  14    )
                   MOVE    1                   TO  FLG-NAIFUKU
               ELSE
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
               IF      (FLG-NAIFUKU            =   1  )   AND
                       (WRK-COM-INPUTCD(IDX)(1:1) =   "6"   )
      *            剤数加算
                   IF      WRK-COM-ZAIGATAKBN (IDX)    =   1  OR
                                                           2  OR
                                                           3
                       MOVE    1               TO  WRK-KONGO
                   ELSE
      *****************ADD     1               TO  WRK-SONOTA
      *                粉砕コードがある時、混合とする
                       IF      (IDX            >   1   )  AND
                               (WRK-COM-INPUTCD (IDX - 1)
                                               =   "099209901" )
                           MOVE    1               TO  WRK-KONGO
                       ELSE
                           ADD     1               TO  WRK-SONOTA
                       END-IF
                   END-IF
      *            点数
                   COMPUTE WRK-SYOKEIS2        =   WRK-COM-KISOTEN(IDX)
                                               *   WRK-COM-SURYO  (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-ZAITEN
      *
      *----(01.00.01) LINE ADD START ----------------------------------
               END-IF
               IF      (FLG-NAIFUKU            =   1  )  AND
                       (WRK-COM-ZAIEND (IDX)   =   "1")
                   IF      WRK-ZAITEN          <   205
                       IF      WRK-CHK-SRYSYUKBN   =   "211"
                                                   OR  "291"
                           ADD     1               TO  WRK-ZAICNT
                       ELSE
                           ADD     1               TO  WRK-ZAICNT2
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
      *            点数加算
                   IF      WRK-CHK-SRYSYUKBN   =   "211"
                                               OR  "291"
                       ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI
                   ELSE
                       ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI2
                   END-IF
      *
                   MOVE    ZERO            TO  WRK-KONGO
                   MOVE    ZERO            TO  WRK-SONOTA
                   MOVE    ZERO            TO  WRK-ZAITEN
               END-IF
      *
               END-IF
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
           END-PERFORM
      *
           MOVE    ZERO                TO  IDXS
      *    院外分
           PERFORM 3001-INGAI-SYORI-SEC
      *    院内分
           PERFORM 3002-INNAI-SYORI-SEC
      *
      *     特定疾患処方管理加算処方箋料がある時、処方箋料があること
           IF      FLG-TOKUSYO1        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "1014"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T1)
               END-IF
           ELSE
      *        特定疾患処方管理加算が算定できる時、確認メッセージ
               IF     (LNK-ORCSC-TOKUTEI       =   1   )  AND
                      (LNK-ORCSC-TOKUTEIFLG    =   ZERO)
                   IF      FLG-SYOHOSENN       =   1
                       MOVE    "0113"          TO  LNK-ORCSC-KIDCD
                   END-IF
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算処方料がある時の、処方料があること
           IF      FLG-TOKUSYO2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "1015"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T2)
               END-IF
           ELSE
      *        特定疾患処方管理加算が算定できる時、確認メッセージ
               IF     (LNK-ORCSC-TOKUTEI       =   1   )  AND
                      (LNK-ORCSC-TOKUTEIFLG    =   ZERO)
                   IF      FLG-SYOHORYO        =   1
                       MOVE    "0113"              TO  LNK-ORCSC-KIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外　算定処理
      *****************************************************************
       3001-INGAI-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHOSENN
      *
      *    処方箋を交付する診察料を算定している場合、算定なし
           IF      (LNK-ORCSC-ROUMANSEI-ARI    =   1  )  OR
                   (LNK-ORCSC-ROUMANSEI-ARI2   >   ZERO) OR
                   (LNK-ORCSC-NETAKIRI-ARI     =   1  )  OR
                   (LNK-ORCSC-NETAKIRI-ARI2    >   ZERO) OR
                   (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO)
               GO      TO      3001-INGAI-SYORI-EXT
           END-IF
      *
      *    処方箋料
           IF     (FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )
               IF      WRK-ZAICNT2         >=  7
                   MOVE    "180016410"         TO  WRK-SRYCD
                   MOVE    "180001110"         TO  WRK-SRYCD2
               ELSE
                   MOVE    "180001110"         TO  WRK-SRYCD
                   MOVE    "180016410"         TO  WRK-SRYCD2
               END-IF
               PERFORM 3002-KASAN-SYORI-SEC
               MOVE    1               TO  FLG-SYOHOSENN
               IF      FLG-ERR             =   ZERO
                   MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                     (02 IDXS)
      *            他科受診の時、メッセージを追加
                   IF      LNK-ORCSC-FUKU-SRYKA      =   1
                       MOVE    "099999901"     TO  LNK-ORCSC-SRYCD-KS
                                                       (02 IDXS)
                       MOVE    ZERO            TO  LNK-ORCSC-TENSU-KS
                                                       (02 IDXS)
                   END-IF
               END-IF
           ELSE
      *        処方箋料を算定しない場合の処方箋料の有無
               MOVE    "180001110"         TO  WRK-SRYCD
               MOVE    "180016410"         TO  WRK-SRYCD2
               PERFORM 3002-KASAN-CHK-SEC
               IF      FLG-ERR             =   1
                   MOVE    1               TO  FLG-SYOHOSENN
               END-IF
           END-IF
      *    他科受診の時、メッセージを追加
           IF     (FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )
               IF      LNK-ORCSC-FUKU-SRYKA      =   1
                   MOVE    "099999901"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "990"           TO  LNK-ORCSC-SRYSYUKBN
                                                             (02 IDXS)
                       MOVE    "99"            TO  LNK-ORCSC-SRYKBN
                                                            (02 IDXS)
                   END-IF
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算のチェック
           IF     (FLG-SYOHOSENN       =   1  )  AND
                  (LNK-ORCSC-TOKUTEI   =   1)
               IF      LNK-ORCSC-TOKUTEIFLG     =   1
                   GO      TO      3001-INGAI-SYORI-EXT
               END-IF
               MOVE    "120002570"         TO  WRK-SRYCD
               PERFORM 3003-YAKJYO-CHK-SEC
      *        入力あり
               IF      FLG-YAKJYO          =   1
                   MOVE    1                   TO  FLG-TOKUSYO1
               ELSE
                   IF      LNK-ORCSC-TOKUTEIFLG     =   2
                       MOVE    "120002570"         TO  WRK-SRYCD
                       MOVE    SPACE               TO  WRK-SRYCD2
                       PERFORM 3002-KASAN-SYORI-SEC
                       IF      FLG-ERR             =   ZERO
                           MOVE    "820"       TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                           MOVE    "80"        TO  LNK-ORCSC-SRYKBN
                                                     (02 IDXS)
                       END-IF
                       MOVE    1                   TO  FLG-TOKUSYO1
                   END-IF
               END-IF
           END-IF
           .
       3001-INGAI-SYORI-EXT.
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
      *        薬剤料逓減あり
               COMPUTE WRK-SANTEN-9    =   WRK-KEIKEI
                                       *   90
                                       /   100
                                       +   0.5
               MOVE    WRK-SANTEN-9    TO  WRK-SANTEN
               PERFORM 3001-GENHEN-SEC
           END-IF
      *
      *    薬剤料逓減
           IF      WRK-ZAICNT          >=  7
               MOVE    "630010002"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "210"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *
      *    院外処方が有れば自動加算をしない
           IF     (FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )
               GO      TO      3002-INNAI-SYORI-EXT
           END-IF
      *
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
      *    調剤技術基本料（入院外）
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
      *----(01.00.02) LINE ADD START ----------------------------------
      *           調基が１であること
             IF      LNK-ORCSC-CHOKI-FLG   =   1
      *----(01.00.02) LINE ADD END   ----------------------------------
               MOVE    "120001810"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "270"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "27"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
             END-IF
           END-IF
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    薬剤情報提供料
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               MOVE    SPACE               TO  WRK-SRYCD
      *        老人区分判定
               IF      LNK-ORCSC-ROUJIN    =   0
                   IF      LNK-ORCSC-YAKJYO-1      =    1
                       MOVE    "120002370"         TO  WRK-SRYCD
                       MOVE    SPACE               TO  WRK-SRYCD2
                       PERFORM 3002-KASAN-SYORI-SEC
                       IF      FLG-ERR             =   ZERO
                           MOVE    "130"       TO  LNK-ORCSC-SRYSYUKBN
                                                             (02 IDXS)
                           MOVE    "13"        TO  LNK-ORCSC-SRYKBN
                                                             (02 IDXS)
                       END-IF
                   END-IF
               ELSE
      *            老人の時、手帳記載あり、なしの両方の算定チェック
                   MOVE    "113701310"         TO  WRK-SRYCD
                   PERFORM 3003-YAKJYO-CHK-SEC
                   IF      FLG-YAKJYO          =   ZERO
                       MOVE    "113701410"         TO  WRK-SRYCD
                       PERFORM 3003-YAKJYO-CHK-SEC
                   END-IF
                   IF      FLG-YAKJYO          =   ZERO
                       MOVE    SPACE               TO  WRK-SRYCD
                       IF      LNK-ORCSC-YAKJYO-2      =    1
                           MOVE    "113701310"         TO  WRK-SRYCD
                       ELSE
                           IF      LNK-ORCSC-YAKJYO-3  =    1
                               MOVE    "113701410"     TO  WRK-SRYCD
                           END-IF
                       END-IF
                       IF      WRK-SRYCD       NOT =   SPACE
                           MOVE    SPACE               TO  WRK-SRYCD2
                           PERFORM 3002-KASAN-SYORI-SEC
                           IF      FLG-ERR             =   ZERO
                               MOVE    "130"     TO
                                                 LNK-ORCSC-SRYSYUKBN
                                                              (02 IDXS)
                               MOVE    "13"      TO  LNK-ORCSC-SRYKBN
                                                              (02 IDXS)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    他科受診の時、メッセージを追加
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               IF      LNK-ORCSC-FUKU-SRYKA      =   1
                   MOVE    "099999901"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "990"           TO  LNK-ORCSC-SRYSYUKBN
                                                             (02 IDXS)
                       MOVE    "99"            TO  LNK-ORCSC-SRYKBN
                                                            (02 IDXS)
                   END-IF
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算のチェック
           IF     (FLG-SYOHORYO            =   1  )  AND
                  (LNK-ORCSC-TOKUTEI       =   1  )
               IF      LNK-ORCSC-TOKUTEIFLG    =   1
                   GO      TO      3002-INNAI-SYORI-EXT
               END-IF
               MOVE    "120002270"         TO  WRK-SRYCD
               PERFORM 3003-YAKJYO-CHK-SEC
      *        入力あり
               IF      FLG-YAKJYO          =   1
                   MOVE    1                   TO  FLG-TOKUSYO2
               ELSE
                   IF      LNK-ORCSC-TOKUTEIFLG     =   2
                       MOVE    "120002270"         TO  WRK-SRYCD
                       MOVE    SPACE               TO  WRK-SRYCD2
                       PERFORM 3002-KASAN-SYORI-SEC
                       IF      FLG-ERR             =   ZERO
                           MOVE    "250"       TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                           MOVE    "25"        TO  LNK-ORCSC-SRYKBN
                                                     (02 IDXS)
                       END-IF
                       MOVE    1                   TO  FLG-TOKUSYO2
                   END-IF
               END-IF
           END-IF
      *
           .
       3002-INNAI-SYORI-EXT.
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
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)
      *
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
                       (WRK-COM-KAISU (IDX)    >=  14    ))
                   MOVE    1                   TO  FLG-NAIFUKU
               ELSE
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
      ******** IF     (WRK-COM-SRYKBN   (IDX)  =   "21"  )  AND
      *               (WRK-CHK-SRYSYUKBN       =   "210" OR
      **********                                   "211" )  AND
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
                   MOVE    1               TO  FLG-ZAI
               END-IF
      *
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO            TO  FLG-ZAI
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)
               ADD     1               TO  WRK-GMN-MAX
           END-PERFORM
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
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              WRK-GMN-TBLREC (IDX-S)
               INITIALIZE              WRK-COM-TBLREC (IDX-S)
               COMPUTE IDX-S           =   IDX-S
                                       -   1
               GO      TO      30011-GENTUIKA-EXT
           END-IF
      *
           IF      IDX-S               =   1
               MOVE    "21"                TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    "21"                TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    "210"               TO  WRK-COM-SRYSYUKBN(IDX-S)
           ELSE
               MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-S)
           END-IF
      *
      *    自動追加分（入力時に追加）
           MOVE    "3"                 TO  WRK-COM-AUTOKBN (IDX-S)
           .
       30011-GENTUIKA-EXT.
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
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-PERFORM
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
           END-PERFORM
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      3002-KASAN-SYORI-EXT
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
               IF      WRK-SRYCD       =   "630010002"
                   COMPUTE LNK-ORCSC-TENSU (02 IDXS)
                                           =   WRK-KEIKEI
                                           -   WRK-SANTEN
               END-IF
      *
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
           END-IF
      *
           .
       3002-KASAN-SYORI-EXT.
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
           END-PERFORM
      *
           .
       3002-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定チェック（薬剤情報提供料　老人）処理
      *****************************************************************
       3003-YAKJYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-YAKJYO
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF      WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD
                   MOVE    1                   TO  FLG-YAKJYO
               END-IF
           END-PERFORM
           IF      FLG-YAKJYO          =   1
               GO      TO      3003-YAKJYO-CHK-EXT
           END-IF
      *
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    1                   TO  FLG-YAKJYO
               GO      TO      3003-YAKJYO-CHK-EXT
           END-IF
      *
      *        算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR         NOT =   ZERO
               MOVE    1                   TO  FLG-YAKJYO
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
               IF      LNK-ORCSC-NENREI-YY   >=  WRK-KGNAGE
                   CONTINUE
               ELSE
                   MOVE    1                 TO  FLG-CHK
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
                          (LNK-ORCSC-NENREI-DD    <=  28   )
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
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
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
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    LNK-ORCSC-DOUJI-RENNUM  TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL    FLG-JYURRK         =   1
               IF      (LNK-ORCSC-SYORIFLG     =   1  )   AND
                       (LNK-ORCSC-DENPNUM      =   JYURRK-DENPNUM)
                   CONTINUE
               ELSE
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX                 >   15  )  OR
                                  (JYURRK-ZAINUM(IDX)  =   SPACE)
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
                       PERFORM 940-SINACCT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
                   PERFORM UNTIL    FLG-SRYACCT    =   1
      *
                      COMPUTE IDX-Y3              =   JYURRK-RENNUM
                                                  +   1
      *ver.4.8
      *               IF      IDX-Y3              >   4
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
                       PERFORM 940-SINACCT-READ-SEC
                   END-PERFORM
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-PERFORM
      *
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30010-JYURRK-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       300011-YAKFTN-SRYACT-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
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
                           WHEN    "180016410"
                           WHEN    "180011110"
                               MOVE    "180011110"     TO  WRK-SRYCD
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
       940-SINACCT-READ-SEC         SECTION.
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
       940-SINACCT-READ-EXT.
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
      *
