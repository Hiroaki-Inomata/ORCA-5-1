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
       PROGRAM-ID.                 ORCSC54200203.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（麻酔）
      *                      Ｈ１４．４改正まで
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-MIJYU           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-ZS              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SANSO           PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-MAX             PIC 9(04).
      *
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
      *
           03  WRK-SANSO           PIC 9(08).
           03  WRK-SANSOKIN        PIC 9(08)V99.
           03  WRK-SANKEI          PIC 9(08).
           03  WRK-SANTEN          PIC 9(08).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9999.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ERRCD           PIC X(04).
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
      *    COPY    "CPTENSU.INC".
      *
      *    算定履歴
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
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
      **** COPY    "CPORCMCP.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
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
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
           IF      IDX-ZS              >   ZERO
      *        乳幼児、時間外の自動加算を追加する
               MOVE    SPA-ERRCD          TO  WRK-ERRCD
               MOVE    SPACE              TO  SPA-ERRCD
               PERFORM 1002-JIDO-KASAN-SEC
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
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
      *            未熟児加算チェック
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "150286690"
                       PERFORM 10013-MIJYUKUJI-SEC
                   END-IF
      *            手技料
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       MOVE    IDX-Z           TO  IDX-ZS
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
                       MOVE    "0047"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   END-IF
      *
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "770000000"
                           MOVE    IDX-Z       TO  IDX-SANSO
                           PERFORM 10011-SANSO-ADD-SEC
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  SPA-ERRCD
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
      *    酸素補正率自動加算処理
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
      *    酸素補正率自動加算
           MOVE    IDX-Z               TO  IDX-S
           MOVE    "770020070"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               MOVE    1               TO  FLG-ARI
               MOVE    IDX-S           TO  IDX-T
               ADD     1               TO  IDX-T
           ELSE
               MOVE    ZERO            TO  FLG-ARI
           END-IF
           PERFORM 1002-KASAN-SET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    未熟児加算チェック処理
      *****************************************************************
       10013-MIJYUKUJI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    年齢判定（９０日以内）
           IF     (LNK-ORCSC-NENREI-YY =   ZERO )  AND
                  (LNK-ORCSC-NENREI-MM <=  3    )
      *
      *        年齢期間算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    SPA-BIRTHDAY        TO  LNK-DAY5-START
               MOVE    SPA-SRYYMD          TO  LNK-DAY5-END
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     <   90
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0041"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               MOVE    ZERO            TO  WRK-COM-SANFLG(IDX-Z)
           ELSE
               MOVE    1               TO  WRK-COM-SANFLG(IDX-Z)
               MOVE    1               TO  FLG-MIJYU
           END-IF
      *
           .
       10013-MIJYUKUJI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自動加算追加処理
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
           MOVE    IDX-ZS              TO  IDX-S
      *
      *    乳幼児加算
           IF     (LNK-ORCSC-NENREI-YY <   6  )  AND
                  (FLG-MIJYU           =   ZERO)
               PERFORM 10011-YOUJIKASAN-SET-SEC
           ELSE
               PERFORM 10013-YOUJIKASAN-CHK-SEC
           END-IF
      *
      *    時間加算
           IF      (LNK-ORCSC-TIMEFLG          NOT =   ZERO   OR
                    WRK-COM-TIMEFLG (IDX-ZS)   NOT =   ZERO )  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS) NOT =   ZERO )
               PERFORM 10012-JIKANKASAN-SET-SEC
           END-IF
      *
      *    年齢加算処理
      *    PERFORM 10013-NENREI-SET-SEC
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *****************************************************************
      *    乳幼児加算料追加処理
      *****************************************************************
       10011-YOUJIKASAN-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    新生児加算
           MOVE    "150231590"         TO  WRK-SRYCD
      *    加算料追加処理
           PERFORM 100111-KASAN-CHK-SEC
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
      *    乳幼児の加算
           MOVE    "150231690"         TO  WRK-SRYCD
      *    加算料追加処理
           PERFORM 100111-KASAN-CHK-SEC
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
      *    幼児加算
           MOVE    "150265390"         TO  WRK-SRYCD
      *    加算料追加処理
           PERFORM 100111-KASAN-CHK-SEC
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
           .
       10011-YOUJIKASAN-SET-EXT.
           EXIT.
      *****************************************************************
      *    乳幼児加算料チェック処理
      *****************************************************************
       10013-YOUJIKASAN-CHK-SEC        SECTION.
      *
      *    未熟児加算入力時、乳幼児加算がないこと
           IF      FLG-MIJYU           =   1
      *
      *        新生児加算
               MOVE    "150231590"         TO  WRK-SRYCD
               PERFORM 100111-KASAN-CHK-SEC
               IF      FLG-ARI             =   1
                   MOVE    "0040"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-T)
                   GO      TO      10013-YOUJIKASAN-CHK-EXT
               END-IF
      *
      *        乳幼児の加算
               MOVE    "150231690"         TO  WRK-SRYCD
               PERFORM 100111-KASAN-CHK-SEC
               IF      FLG-ARI             =   1
                   MOVE    "0040"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-T)
                   GO      TO      10013-YOUJIKASAN-CHK-EXT
               END-IF
           END-IF
      *
           .
       10013-YOUJIKASAN-CHK-EXT.
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
      *    時間加算加算料追加処理
      *****************************************************************
       10012-JIKANKASAN-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *
           MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-TIMEFLG
           IF      WRK-TIMEFLG         =   ZERO
               MOVE    WRK-COM-TIMEFLG(IDX-ZS)     TO  WRK-TIMEFLG
           END-IF
      *
      *    時間加算
           EVALUATE    WRK-TIMEFLG
      *      /* 時間外 */
               WHEN    1
                   MOVE    "150231790"     TO  WRK-SRYCD
      *     /* 休日 */
               WHEN    2
                   MOVE    "150231890"     TO  WRK-SRYCD
      *        /* 深夜 */
               WHEN    3
                   MOVE    "150231990"     TO  WRK-SRYCD
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
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
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
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10013-NENREI-SET-EXT.
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
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
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
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
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
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
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
      *    材料再計算編集
           PERFORM 1007-KIZAI-TEN-SEC
      *    割合加算
           PERFORM 1006-KASAN-SET-SEC
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
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
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               MOVE    IDX-Z               TO  IDX-ZS
           END-IF
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
      *    酸素計算処理
      *****************************************************************
       100211-SANSO-KEI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SANSO
           EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *        酸素金額
               WHEN    "770000000"
                   COMPUTE WRK-SANSOKIN    =   WRK-COM-KISOTEN (IDX-Z)
                                           *   WRK-COM-SURYO (IDX-Z)
      *        酸素補正率自動加算
               WHEN    "770020070"
      *            酸素金額＋（酸素金額×補正率／１００）　四捨五入
                   COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                           *   WRK-COM-KISOTEN (IDX-Z)
                                           /   100
                   COMPUTE WRK-SANSOKIN        =   WRK-SANSOKIN
                                               +   WRK-SYOKEIS
      *
      *        高気圧酸素計算
               WHEN    "770030070"
      *            酸素金額＋（酸素金額×補正率／１００）
      *                                       ＋気圧数　四捨五入
                   COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                           *   WRK-COM-SURYO (IDX-Z)
                   MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
           END-EVALUATE
      *
           .
       100211-SANSO-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
           MOVE    ZERO                TO  WRK-TENSUKEI-G
      *
      *    自動加算以外の加算（時間外、年齢以外）
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        手技料の点数を所定点数とする
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                               (IDX-ZS)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
      *    自動加算の加算（時間外、年齢）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
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
           IF      WRK-SANSOKIN    NOT =   ZERO
      *        金額計　四捨五入
               COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
                                           +   0.5
      *        金額計÷１０　四捨五入
               COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                           /   10
                                           +   0.5
               MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *        酸素の点数を器材点数に加算
               ADD     WRK-SANTEN          TO  LNK-SC90-KIZTEN
      *
           END-IF
           .
       1007-KIZAI-TEN-EXT.
           EXIT.
      *
      *
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
      *    INITIALIZE                      ORCSC91AREA
      *    MOVE    "2"                 TO  LNK-SC91-KBN
      *    MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
      *    MOVE    WRK-COM-INPUTCD(IDX)
      *                                TO  LNK-SC91-SRYCD
      *    CALL    "ORCSC91"       USING
      *                                MCPAREA
      *                                ORCSC91AREA
      *                                WRK-SCR-REC
      *                                SPA-AREA
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
      *
