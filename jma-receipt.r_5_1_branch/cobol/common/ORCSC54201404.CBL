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
       PROGRAM-ID.                 ORCSC54201404.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（麻酔）
      *                      Ｈ２６．４改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/06/12  時間外特例追加
      * 01.00.02     NACL-多々納  02/12/09  労災加算追加
      * 01.00.03     NACL-多々納  03/02/12  年齢加算・時間外加算計算修正
      * 01.00.04     NACL-多々納  03/07/30  通則加算区分追加
      * 01.00.05     NACL-多々納  06/01/13  神経ブロック重複チェック追加
      * 04.02.00     NACL-多々納  08/03/26  平成２０年４月対応
      * 04.05.00     NACL-多々納  10/03/XX  平成２２年４月対応
      * 04.06.00     NACL-多々納  10/07/01  残量廃棄自動算定対応
      * 04.07.00     NACL-多々納  14/03/XX  平成２６年４月対応
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
           03  FLG-SYUGI           PIC 9(01).
           03  FLG-CHK             PIC 9(01).
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
           03  HZN-IDX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-NEN             PIC 9(04).
           03  IDX-TUS             PIC 9(04).
      *
           03  IDD                 PIC 9(02).
      *
           03  IDX-MSYU            PIC 9(04).
           03  IDX-M               PIC 9(04).
           03  IDX-SURYO           PIC 9(04).
      *H26.4
           03  IDX-SYU2            PIC 9(04).
           03  IDXM                PIC 9(04).
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
           03  WRK-TENSUKEI-K      PIC 9(08).
      *    通則対象外
           03  WRK-TENGAKEI-G          PIC S9(10).
           03  WRK-TENGAKEI-K          PIC S9(10).
      *    手技料合計
           03  WRK-GOK-TENSUKEI        PIC S9(10).
           03  WRK-GOK-TENGAKEI        PIC S9(10).
      *    点数計
           03  WRK-SUM-TENSUKEI        PIC 9(08)V99.
      *    点数計（通則加算対象外計）
           03  WRK-SUM-TENGAKEI        PIC 9(08)V99.
      *
           03  WRK-SYOKEIS         PIC 9(08)V9999.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *   きざみ値行為再計算用
           03  WRK-SATU-TEN        PIC 9(08)V9(03).
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-K         PIC S9(05)V9(05).
           03  WRK-SURYO-H         PIC S9(05)V9(05).
           03  WRK-KZMTEN          PIC 9(08)V9(03).
           03  WRK-KZMJGN          PIC 9(08)V9(03).
           03  WRK-KZMKGN          PIC 9(08)V9(03).
      *
           03  WRK-ERRCD           PIC X(04).
      *
           03  WRK-INPUTCD         PIC X(22).
      *
           03  WRK-SURYO-Z         PIC 9(07)V9(05).
           03  WRK-SURYO-ZR         REDEFINES   WRK-SURYO-Z.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *
       01  WRK-AREA2.
      *    閉鎖循環式麻酔の数量
           03  WRK-SURYO-G         PIC 9(05)V9(05).
           03  WRK-MASUI-CNT       PIC 9(03).
           03  WRK-MASUI           PIC 9(03).
      *
      *    閉鎖循環式麻酔の数量チェック
       01  TBL-MASUICHK-AREA.
           03  TBL-MASUICHK-OCC            OCCURS   400.
               05  TBL-SURYO-Z         PIC S9(05)V9(05).
               05  TBL-CHK             PIC X(01).
      *
      *    神経ブロック・トリガーポイント注射判定
       01  TBL-CHK-AREA.
           03  TBL-CHK-OCC             OCCURS   31.
               05  TBL-CHK-L100        PIC 9(01).
               05  TBL-CHK-L102        PIC 9(01).
               05  TBL-CHK-L104        PIC 9(01).
      *
      *
      *    保存用
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
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
           COPY    "CPORCSC90TEN2.INC".
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
                   IF      SPA-HKNCOMBINUM     =   "9999"
                       CONTINUE
                   ELSE
                       PERFORM 200-ALL-SYORI-SEC
                   END-IF
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
               CONTINUE
           ELSE
      *
      *        点数計算
               PERFORM 1002-TENSU-KEI-SEC
           END-IF
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
           INITIALIZE                  WRK-AREA2
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                                                       OR  "*"
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
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    酸素の補正率を自動発生する
           IF     (SPA-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               MOVE    SPA-ERRCD          TO  WRK-ERRCD
               MOVE    SPACE              TO  SPA-ERRCD
               PERFORM 10011-SANSO-ADD-SEC
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
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
      *                閉鎖循環式全身麻酔判定
                       PERFORM 10014-MASUI-CHK-SEC
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
      *            残量廃棄
                   PERFORM 1006-HAIKI-SYORI-SEC
      *        器材
               WHEN    "7"
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z       TO  IDX-SANSO
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
      *    閉鎖循環式全身麻酔判定処理
      *****************************************************************
       10014-MASUI-CHK-SEC           SECTION.
      *
           IF      (WRK-COM-CDKBN-KBN       (IDX-Z) =   "L"  )
              AND  (WRK-COM-CDKBN-KBNNUM    (IDX-Z) =   "008")
              AND  (WRK-COM-CDKBN-KBNNUM-EDA(IDX-Z) =   "00" )
      *        閉鎖循環式全身麻酔の集計
               ADD     1                       TO  WRK-MASUI-CNT
               IF     (IDX-ZS          NOT =   ZERO )
                  AND (WRK-MASUI-CNT       =   1    )
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                      AND (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K216"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-Z)
                       MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           ELSE
               IF      WRK-MASUI-CNT       >  ZERO
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                      AND (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K216"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                       MOVE    "1"                 TO  WRK-COM-CUR
                                                              (IDX-Z)
                       MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
           .
       10014-MASUI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
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
      *****************************************************************
      *    残量廃棄コメントの有無
      *****************************************************************
       1006-HAIKI-SYORI-SEC           SECTION.
      *
      *    アンプルの時、小数以下判定
           IF     (WRK-COM-TANICD (IDX-Z)  =   "014" OR "022" OR "046" )
           AND    (LNK-ORCSC-HAIKIKBN-FLG  =   "2"            )
      *
               MOVE    WRK-COM-SURYO(IDX-Z)    TO  WRK-SURYO-Z
               IF     (WRK-SURYO-S2                >   ZERO    )  AND
                      (WRK-COM-AUTOFLG2 (IDX-Z)    =   SPACE   )
                   PERFORM 10011-HAIKI-ADD-SEC
               END-IF
           END-IF
           .
       1006-HAIKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    破棄コメント自動発生成処理
      *****************************************************************
       10011-HAIKI-ADD-SEC             SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               CONTINUE
           ELSE
               MOVE    IDX-Z               TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               PERFORM 1002-KASAN-SET-SEC
               MOVE    IDX-S               TO  IDX-Z
           END-IF
           .
       10011-HAIKI-ADD-EXT.
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
      *        通則年齢区分がある場合のみ
               IF     (IDX-ZS          >   ZERO )  AND
                      (WRK-COM-TSUSOKUAGE (IDX-ZS) =   "1" )
                   CONTINUE
               ELSE
                   MOVE    "0041"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   MOVE    ZERO            TO  WRK-COM-SANFLG(IDX-Z)
               END-IF
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
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-NEN
           MOVE    ZERO                TO  IDX-TUS
           MOVE    ZERO                TO  FLG-ADD
      *
           PERFORM VARYING     IDX1    FROM    1    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE  AND
                               WRK-COM-INPUTCD (IDX1)  =   SPACE )
      *        主たる手技を判定対象とする
               IF     (WRK-COM-DATAKBN (IDX1)  =   "1"   )   AND
                      (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )
                   IF      IDX-SYU         =   ZERO
                       MOVE    IDX1            TO  IDX-SYU
                  END-IF
               END-IF
      *        最後の通則加算対象位置
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )  AND
                      (WRK-COM-TUSOKUGAIKBN(IDX1)  NOT =   "1" )
                   MOVE    IDX1            TO  IDX-NEN
               END-IF
           END-PERFORM
      *
           MOVE    IDX-SYU             TO  IDX-ZS
      *
      **** MOVE    IDX-ZS              TO  IDX-S
      *    年齢加算処理
           PERFORM 10013-NENREI-SET-SEC
      *
      *    乳幼児加算
           IF      FLG-ADD             =   ZERO
      *        年齢加算コードなし
               IF     (LNK-ORCSC-NENREI-YY <   6  )  AND
                      (FLG-MIJYU           =   ZERO)
                   PERFORM 10011-YOUJIKASAN-SET-SEC
               ELSE
                   PERFORM 10013-YOUJIKASAN-CHK-SEC
               END-IF
      *
           END-IF
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
               IF      (SPA-GAI-JIKANKBN       =   8 )
                   CONTINUE
               ELSE
                   PERFORM 10019-JIKANKASAN-CHK-SEC
               END-IF
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
      *    通則年齢区分がある場合のみ
           IF      WRK-COM-TSUSOKUAGE (IDX-ZS) =   "1"
               CONTINUE
           ELSE
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    新生児加算
           MOVE    "150231590"         TO  WRK-SRYCD
      *    加算料追加処理
           PERFORM 100111-KASAN-CHK-SEC
           IF      FLG-ARI             =   ZERO
      *        最後の通則年齢加算位置
               MOVE    IDX-NEN             TO  IDX-S
           END-IF
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
           IF      FLG-ARI             =   ZERO
      *        最後の通則年齢加算位置
               MOVE    IDX-NEN             TO  IDX-S
           END-IF
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
      *    入院の時、手技料の時間外フラグを優先する
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (WRK-COM-TIMEFLG(IDX-ZS) NOT =   ZERO )
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
                   MOVE    "150231790"     TO  WRK-SRYCD
      *     /* 休日 */
               WHEN    2
                   MOVE    "150231890"     TO  WRK-SRYCD
      *        /* 深夜 */
               WHEN    3
                   MOVE    "150231990"     TO  WRK-SRYCD
      *        /* 深夜 */
               WHEN    4
                   MOVE    "150232090"     TO  WRK-SRYCD
           END-EVALUATE
      *
           IF      WRK-SRYCD            =    SPACE
               GO      TO      10012-JIKANKASAN-SET-EXT
           END-IF
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  IDX-TUS
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE    )   OR
      *****************       (FLG-ARI                 =   1    )  OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE  AND
                               WRK-COM-INPUTCD (IDX1)  =   SPACE )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    MOVE    IDX1           TO  IDX-T
               END-IF
      *        最後の通加算対象位置
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )  AND
                      (WRK-COM-TUSOKUGAIKBN (IDX1) =   "0" )
                   MOVE    IDX1            TO  IDX-TUS
               END-IF
           END-PERFORM
           IF      IDX-T               =   ZERO
               IF      IDX-TUS             =   ZERO
                   MOVE    IDX-ZS              TO  IDX-S
               ELSE
                   MOVE    IDX-TUS             TO  IDX-S
               END-IF
           ELSE
               MOVE    IDX-T               TO  IDX-S
           END-IF
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
           END-IF
      *
      *    入院の時、手技料に時間外フラグを設定する
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (WRK-COM-TIMEFLG(IDX-ZS) =   ZERO )
               MOVE    WRK-TIMEFLG         TO  WRK-COM-TIMEFLG(IDX-ZS)
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
               IF      FLG-ARI             =   ZERO
      *            最後の通則年齢加算位置
                   MOVE    IDX-NEN             TO  IDX-S
               END-IF
      *        加算料追加処理
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    2               TO  WRK-COM-SANFLG(IDX-S)
                   MOVE    4               TO  IDX2
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
                   MOVE    IDX1            TO  IDX-SYU2
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
           END-IF
      *
      *    副鼻腔手術用内視鏡加算
           IF     (SPA-ERRCD           =   SPACE )
              AND (IDX-SYU2        NOT =   ZERO  )
              AND (WRK-COM-YOBI1 (IDX-Z)(4:1)
                                   NOT =   ZERO  )
      *        手技料の加算と不一致はエラー
               IF      WRK-COM-YOBI1(IDX-SYU2)(4:1)
                                   NOT =   WRK-COM-YOBI1 (IDX-Z)(4:1)
                   MOVE    "0541"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               END-IF
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
      *    閉鎖循環式全身麻酔が複数
           IF      WRK-MASUI-CNT       >   1
               PERFORM 10022-HEIMA-SYORI-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-SANSO
           MOVE    ZERO                TO  IDX-MSYU
           MOVE    ZERO                TO  WRK-MASUI
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *        手技毎
               INITIALIZE                  WRK-COM-PLUS-AREA (IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                                                       OR  "*"
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
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    手技料の計算
           IF      IDX-ZS              >   ZERO
               COMPUTE IDX-Z           =   IDX-MAX    +  1
               MOVE    2               TO  FLG-SYUGI
               PERFORM 100212-TENSUKEI-SEC
           END-IF
      *    合計点数計算・編集（１剤毎の合計点数のみとする）
           COMPUTE LNK-SC90-TENSU      =   WRK-GOK-TENSUKEI
                                       +   WRK-GOK-TENGAKEI
      *
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
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
                   MOVE    1               TO  FLG-SYUGI
                   PERFORM 100212-TENSUKEI-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-ZS
           END-IF
      *
      *    手技料集計
           IF       WRK-MOJI                   =   "1"
               PERFORM 100212-SYUGI-KEI-SEC
           END-IF
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料計算処理
      *****************************************************************
       100212-SYUGI-KEI-SEC            SECTION.
      *    点数
      *        ％加算・減算は集計しない
           IF      WRK-COM-TENSIKIBETU (IDX-Z)   =   5
                                                 OR  6
               GO      TO      100212-SYUGI-KEI-EXT
           END-IF
      *!!
      *    閉鎖循環式全身麻酔の点数再計算
           IF     (WRK-COM-DATAKBN         (IDX-Z) =   "1"  )
             AND  (WRK-COM-CDKBN-KBN       (IDX-Z) =   "L"  )
             AND  (WRK-COM-CDKBN-KBNNUM    (IDX-Z) =   "008")
             AND  (WRK-COM-CDKBN-KBNNUM-EDA(IDX-Z) =   "00" )
               IF      WRK-MASUI-CNT       >   1
                   PERFORM 100214-HEISA-TENKEI-SEC
               ELSE
      *            再計算
                   MOVE    WRK-COM-SURYO (IDX-Z)
                                               TO  WRK-COM-SURYO2(IDX-Z)
                   PERFORM 1002141-HEIMA-KEISAN01-SEC
                   MOVE    ZERO                TO  WRK-COM-SURYO2(IDX-Z)
               END-IF
           END-IF
      *!!
      *    通則加算対象外区分判定
           IF      WRK-COM-TUSOKUGAIKBN (IDX-Z)    =   "1"
               ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
           ELSE
               ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-SUM-TENSUKEI
           END-IF
      *
           .
       100212-SYUGI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    閉鎖循環式全身麻酔複数処理
      *****************************************************************
       10022-HEIMA-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-MASUI
           MOVE    ZERO                TO  IDX-MSYU
           MOVE    ZERO                TO  WRK-SURYO-G
           MOVE    ZERO                TO  WRK-SURYO-H
           INITIALIZE                  TBL-MASUICHK-AREA
      *    端数以外の数量
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        閉鎖循環式全身麻酔の数量集計
               IF     (WRK-COM-DATAKBN         (IDX-Z) =   "1"  )
                 AND  (WRK-COM-CDKBN-KBN       (IDX-Z) =   "L"  )
                 AND  (WRK-COM-CDKBN-KBNNUM    (IDX-Z) =   "008")
                 AND  (WRK-COM-CDKBN-KBNNUM-EDA(IDX-Z) =   "00" )
      *            外用の数量使用してきざみ値計算を行う
                   MOVE    ZERO            TO  WRK-COM-SURYO2 (IDX-Z)
                   IF      IDX-MSYU            =   ZERO
                       MOVE    IDX-Z               TO  IDX-MSYU
      *                最初の麻酔は下限値まで
                       MOVE    WRK-COM-KZMKGN (IDX-Z)
                                               TO  WRK-COM-SURYO2(IDX-Z)
                       COMPUTE WRK-SURYO-H     =   WRK-COM-SURYO (IDX-Z)
                                               -   WRK-COM-KZMKGN(IDX-Z)
                       MOVE    WRK-SURYO-H     TO  TBL-SURYO-Z (IDX-Z)
                       IF      WRK-SURYO-H     <   ZERO
                           MOVE    "1"             TO  TBL-CHK (IDX-Z)
                       ELSE
                           MOVE    WRK-SURYO-H     TO  WRK-SURYO-K
                           PERFORM
                               UNTIL   WRK-SURYO-K <  WRK-COM-KZM(IDX-Z)
                               ADD     WRK-COM-KZM(IDX-Z)
                                                   TO  WRK-COM-SURYO2
                                                             (IDX-Z)
                               COMPUTE WRK-SURYO-K =   WRK-SURYO-K
                                                   -   WRK-COM-KZM
                                                              (IDX-Z)
                           END-PERFORM
                           MOVE    WRK-SURYO-K     TO  WRK-SURYO-H
                           MOVE    WRK-SURYO-H     TO  TBL-SURYO-Z
                                                          (IDX-Z)
                       END-IF
                   ELSE
      *                きざみ値毎に数量
                       MOVE    WRK-COM-SURYO (IDX-Z)
                                               TO  WRK-SURYO-K
                       PERFORM
                               UNTIL   WRK-SURYO-K     <=  ZERO
                           IF      WRK-SURYO-H     <   ZERO
                               COMPUTE WRK-SURYO-K     =   WRK-SURYO-K
                                                       +   WRK-SURYO-H
                               MOVE    WRK-SURYO-K     TO  TBL-SURYO-Z
                                                                (IDX-Z)
                               MOVE    ZERO            TO  WRK-SURYO-H
                           END-IF
                           IF      WRK-SURYO-K    <   WRK-COM-KZM(IDX-Z)
                               MOVE    WRK-SURYO-K     TO  TBL-SURYO-Z
                                                                (IDX-Z)
                               ADD     WRK-SURYO-K     TO  WRK-SURYO-H
                               MOVE    ZERO            TO  WRK-SURYO-K
                           ELSE
                               ADD     WRK-COM-KZM(IDX-Z)
                                                   TO  WRK-COM-SURYO2
                                                             (IDX-Z)
                               COMPUTE WRK-SURYO-K =   WRK-SURYO-K
                                                   -   WRK-COM-KZM
                                                              (IDX-Z)
                               MOVE    WRK-SURYO-K     TO  TBL-SURYO-Z
                                                                (IDX-Z)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
      *
      *    端数を数量へ加算
           PERFORM 
                   UNTIL      (WRK-SURYO-H     <=  ZERO  )
               MOVE    ZERO                TO  WRK-SURYO
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                       UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                                  (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *            閉鎖循環式全身麻酔の数量集計
                   IF     (WRK-COM-DATAKBN         (IDX-Z) =   "1"  )
                     AND  (WRK-COM-CDKBN-KBN       (IDX-Z) =   "L"  )
                     AND  (WRK-COM-CDKBN-KBNNUM    (IDX-Z) =   "008")
                     AND  (WRK-COM-CDKBN-KBNNUM-EDA(IDX-Z) =   "00" )
                     AND  (TBL-CHK (IDX-Z)       =   SPACE  )
                       IF      TBL-SURYO-Z   (IDX-Z)   >   WRK-SURYO
                           MOVE    TBL-SURYO-Z (IDX-Z)     TO  WRK-SURYO
                           MOVE    IDX-Z                   TO  IDX-SURYO
                       END-IF
                   END-IF
               END-PERFORM
               IF      IDX-SURYO       >   ZERO
                   MOVE    IDX-SURYO       TO  IDX-Z
                   IF      WRK-COM-KZM(IDX-Z)  <=  WRK-SURYO-H
                       ADD     WRK-COM-KZM(IDX-Z)  TO  WRK-COM-SURYO2
                                                             (IDX-Z)
                   ELSE
                       ADD     WRK-SURYO-H         TO  WRK-COM-SURYO2
                                                             (IDX-Z)
                   END-IF
                   COMPUTE WRK-SURYO-H     =   WRK-SURYO-H
                                           -   WRK-COM-KZM(IDX-Z)
                   MOVE    "1"             TO  TBL-CHK (IDX-Z)
               END-IF
           END-PERFORM
           .
       10022-HEIMA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100214-HEISA-TENKEI-SEC             SECTION.
      *
           ADD     1                   TO  WRK-MASUI
           IF      WRK-MASUI           =   1
               MOVE    IDX-Z               TO  IDX-MSYU
           END-IF
      *
           IF      WRK-MASUI           =   1
      *        再計算
               PERFORM 1002141-HEIMA-KEISAN01-SEC
           ELSE
      *        注加算の点数計算
               PERFORM 1002142-HEIMA-KEISAN02-SEC
           END-IF
      *    数量クリア
           MOVE    ZERO                TO  WRK-COM-SURYO2(IDX-Z)
           .
       100214-HEISA-TENKEI-EXT.
           EXIT.
      *****************************************************************
      *    手技料の再計算処理
      *****************************************************************
       1002141-HEIMA-KEISAN01-SEC             SECTION.
      *
      *    きざみ値計算
           MOVE    WRK-COM-SURYO2 (IDX-Z)  TO  WRK-SURYO
           MOVE    WRK-COM-TEN (IDX-Z)     TO  WRK-KEI-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-SURYO       <=  (WRK-COM-KZMKGN(IDX-Z)
                                        -   WRK-COM-KZM   (IDX-Z) )
                   IF      WRK-COM-KZMERR (IDX-Z)  =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (WRK-COM-KZMKGN (IDX-Z)    <   WRK-SURYO )   AND
                        (WRK-COM-KZMJGN (IDX-Z)    >=  WRK-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                           -   WRK-COM-KZMKGN (IDX-Z))
                                           /   WRK-COM-KZM (IDX-Z)
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   WRK-COM-TEN (IDX-Z)
                                           + ( WRK-KEI-TEN2
                                           *   WRK-COM-KZMTEN (IDX-Z))
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    WRK-COM-KZMJGN (IDX-Z)  <   WRK-SURYO
                   IF      WRK-COM-KZMERR (IDX-Z)  =   0  OR  2
      *            警告エラーとする
                       MOVE    "K030"          TO  SPA-ERRCD
      *            点数計算式１
                       COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                               -   WRK-COM-KZMKGN
                                                               (IDX-Z))
                                               /   WRK-COM-KZM (IDX-Z)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-COM-TEN (IDX-Z)
                                               + ( WRK-KEI-TEN2
                                               *   WRK-COM-KZMTEN
                                                               (IDX-Z))
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (WRK-COM-KZMJGN(IDX-Z)
                                               -   WRK-COM-KZMKGN
                                                                (IDX-Z))
                                               /   WRK-COM-KZM  (IDX-Z)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-COM-TEN (IDX-Z)
                                               + ( WRK-KEI-TEN2
                                               *   WRK-COM-KZMTEN
                                                               (IDX-Z))
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-KEI-TEN2        >   ZERO
                   MOVE    WRK-KEI-TEN2    TO  WRK-COM-KISOTEN  (IDX-Z)
               ELSE
                   MOVE    ZERO            TO  WRK-COM-KISOTEN  (IDX-Z)
               END-IF
           END-IF
           .
       1002141-HEIMA-KEISAN01-EXT.
           EXIT.
      *****************************************************************
      *    注加算の再計算処理
      *****************************************************************
       1002142-HEIMA-KEISAN02-SEC             SECTION.
      *
           MOVE    WRK-COM-SURYO2 (IDX-Z)  TO  WRK-SURYO
           MOVE    ZERO                TO  WRK-KEI-TEN1 
           MOVE    ZERO                TO  WRK-KEI-TEN2 
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦きざみ値
      ******   WHEN   (WRK-SURYO       <   WRK-COM-KZM   (IDX-Z)
               WHEN   (WRK-SURYO       =   ZERO          )
                   CONTINUE
      *
      *            データ≦上限値
               WHEN     (WRK-COM-KZMJGN (IDX-Z)    >=  WRK-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =   WRK-SURYO
                                           /   WRK-COM-KZM (IDX-Z)
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   WRK-KEI-TEN2
                                           *   WRK-COM-KZMTEN (IDX-Z)
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    WRK-COM-KZMJGN (IDX-Z)      <   WRK-SURYO
                   IF      WRK-COM-KZMERR (IDX-Z)  =   0  OR  2
      *            点数計算式１
                       COMPUTE WRK-KEI-TEN1    =   WRK-SURYO
                                               /   WRK-COM-KZM (IDX-Z)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-KEI-TEN2
                                               *   WRK-COM-KZMTEN
                                                               (IDX-Z)
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (WRK-COM-KZMJGN
                                                             (IDX-Z)
                                               -   WRK-COM-KZMKGN
                                                             (IDX-Z))
                                               /   WRK-COM-KZM  (IDX-Z)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-KEI-TEN2
                                               *   WRK-COM-KZMTEN
                                                               (IDX-Z)
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-KEI-TEN2        >   ZERO
                   MOVE    WRK-KEI-TEN2    TO  WRK-COM-KISOTEN  (IDX-Z)
               ELSE
                   MOVE    ZERO            TO  WRK-COM-KISOTEN  (IDX-Z)
               END-IF
           END-IF
           .
       100214-HEISA-TENKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100212-TENSUKEI-SEC             SECTION.
      *
      *    集計済点数編集
      *    MOVE    WRK-SUM-TENGAKEI    TO  LNK-SC90-TENGAKEI
      *    MOVE    WRK-SUM-TENSUKEI    TO  LNK-SC90-TENSUKEI
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
      *
      *    手技料の合計を編集する
           MOVE    WRK-SUM-TENSUKEI    TO  WRK-COM-SYUTEN2 (IDX-ZS)
      *    労災の外来管理加算読み替え処理
           IF      SPA-HKNKBN          =   1
             AND  (SPA-RSI-HKNKBN  NOT =   "5" )
               IF     (WRK-COM-RSIGIRI (IDX-ZS)
                                           =   1  )  AND
                      (WRK-COM-SYUTEN2 (IDX-ZS)
                                           <   LNK-ORCSC-GAIRAI-TEN)
                   MOVE     LNK-ORCSC-GAIRAI-TEN
                                           TO  WRK-SUM-TENSUKEI
               END-IF
           END-IF
      *
           ADD     WRK-SUM-TENSUKEI    TO  WRK-GOK-TENSUKEI
           ADD     WRK-SUM-TENGAKEI    TO  WRK-GOK-TENGAKEI
      *
           MOVE    ZERO                TO  WRK-SUM-TENSUKEI
           MOVE    ZERO                TO  WRK-SUM-TENGAKEI
      *
      *    最後の手技料時に、以下の計算を行う
           IF      FLG-SYUGI           =   2
               PERFORM 1007-KASAN-SET-ALL-SEC
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
      *    自動加算以外の加算（時間外、年齢以外）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
      *    手技料の点数を所定点数とする
           PERFORM VARYING     IDX     FROM    IDX-ZS  BY  1
                   UNTIL      (IDX         >=  HZN-IDX     )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        時間外加算のものは、自動加算とする
               IF     (WRK-COM-DATAKBN    (IDX)        =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX)    NOT =   ZERO )  AND
                      (WRK-COM-TENSIKIBETU(IDX)        =   5    )
                   MOVE    1               TO  WRK-COM-SANFLG (IDX)
               END-IF
      *
      *        ％加算
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
      *
      *            通則加算対象外区分判定
                   IF      WRK-COM-TUSOKUGAIKBN (IDX-ZS)  =   "1"
                       ADD     WRK-TENSUKEI-9  TO  WRK-TENGAKEI-K
                   ELSE
                       ADD     WRK-TENSUKEI-9  TO  WRK-TENSUKEI-K
                   END-IF
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
      *
      *            通則加算対象外区分判定
                   IF      WRK-COM-TUSOKUGAIKBN (IDX-ZS)  =   "1"
                       ADD     WRK-TENSUKEI-9  TO  WRK-TENGAKEI-G
                   ELSE
                       ADD     WRK-TENSUKEI-9  TO  WRK-TENSUKEI-G
                   END-IF
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-SUM-TENSUKEI    =   WRK-SUM-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
           COMPUTE WRK-SUM-TENGAKEI    =   WRK-SUM-TENGAKEI
                                       +   WRK-TENGAKEI-K
                                       -   WRK-TENGAKEI-G
      *
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理（全体）
      *****************************************************************
       1007-KASAN-SET-ALL-SEC              SECTION.
      *
      *    幼児加算の計算
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *        ％加算（幼児加算）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   2   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GOK-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-GOK-TENSUKEI    =   WRK-GOK-TENSUKEI
                                       +   WRK-TENSUKEI-K
      *
      *    自動加算の加算（時間外、年齢（幼児加算以外））
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *        ％加算（時間外・乳幼児加算）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GOK-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-GOK-TENSUKEI    =   WRK-GOK-TENSUKEI
                                       +   WRK-TENSUKEI-K
      *
           .
       1007-KASAN-SET-ALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
           INITIALIZE                  TBL-CHK-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)  OR
                              (SPA-ERRCD        NOT =   SPACE)
      *        神経ブロック判定
               IF     (WRK-COM-SRYKBN  (IDX)   =   "54" ) AND
                      (WRK-COM-DATAKBN  (IDX)  =   "1"  ) AND
                      (WRK-COM-CDKBN-KBN (IDX) =   "L"  )  AND
                      (WRK-COM-CDKBN-KBNNUM (IDX)  =   "100"
                                                   OR  "101"
                                                   OR  "102"
                                                   OR  "104" )
                   PERFORM 2001-JYUFUKU-CHK-SEC
               END-IF
      *        長時間麻酔管理加算チェック
               IF     (WRK-COM-SRYKBN  (IDX)       =   "54" ) AND
                      (WRK-COM-INPUTCD (IDX)(1:1)  =  "1"  ) AND
                      (WRK-COM-DATAKBN (IDX)       =   "2"  ) AND
                      (WRK-COM-YOBI1   (IDX)(4:1)
                                               NOT =   ZERO  )
      *            長時間麻酔管理加算あり
                   PERFORM 2002-MASUIKANRI-CHK-SEC
               END-IF
           END-PERFORM
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    重複判定処理
      *****************************************************************
       2001-JYUFUKU-CHK-SEC                SECTION.
      *
           IF      SPA-NYUGAIKBN       =   "2"
               MOVE    SPA-SRYYMD(7:2)     TO  IDD
               PERFORM 20011-BUROKU-CHK-SEC
           ELSE
      *        入力日分のチェック
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   31  )   OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      WRK-COM-NYUINDAY (IDX IDX2)   >   ZERO
                       MOVE    IDX2                TO  IDD
                       PERFORM 20011-BUROKU-CHK-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                           STRING  IDD             DELIMITED  BY  SIZE
                                   "日に同時算定となります。"
                                                   DELIMITED  BY  SIZE
                                           INTO    LNK-ORCSC-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       2001-JYUFUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    神経ブロック・トリガーポイント注射重複判定処理
      *****************************************************************
       20011-BUROKU-CHK-SEC                SECTION.
      *
           EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX)
               WHEN    "100"
               WHEN    "101"
                   MOVE    1                   TO  TBL-CHK-L100(IDD)
                   IF     (TBL-CHK-L102 (IDD)  =   1  )  OR
                          (TBL-CHK-L104 (IDD)  =   1  )
                       IF     (WRK-COM-KZMFLG (IDX)    =   SPACE)  AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    IDX             TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                               (IDX)
                           MOVE    "K540"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               WHEN    "102"
                   MOVE    1                   TO  TBL-CHK-L102(IDD)
                   IF     (TBL-CHK-L100 (IDD)  =   1  )  OR
                          (TBL-CHK-L104 (IDD)  =   1  )
                       IF     (WRK-COM-KZMFLG (IDX)    =   SPACE)  AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    IDX             TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                               (IDX)
                           MOVE    "K540"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               WHEN    "104"
                   MOVE    1                   TO  TBL-CHK-L104(IDD)
                   IF     (TBL-CHK-L100 (IDD)  =   1  )  OR
                          (TBL-CHK-L102 (IDD)  =   1  )
                       IF     (WRK-COM-KZMFLG (IDX)    =   SPACE)  AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    IDX             TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                               (IDX)
                           MOVE    "K540"          TO  SPA-ERRCD
                       END-IF
                   END-IF
           END-EVALUATE
           .
       20011-BUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長時間麻酔管理加算チェック処理
      *****************************************************************
       2002-MASUIKANRI-CHK-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDXM    FROM    1   BY  1
                   UNTIL      (IDXM    >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDXM) =   SPACE)  OR
                              (SPA-ERRCD        NOT =   SPACE )  OR
                              (FLG-CHK          NOT =   ZERO  )
               IF     (WRK-COM-HKNCOMBI(IDXM)  NOT =   "9999")
                 AND  (WRK-COM-SRYKBN  (IDXM)      =   "50"  )
                 AND  (WRK-COM-INPUTCD (IDXM)(1:1) =   "1"   ) 
                 AND  (WRK-COM-DATAKBN (IDXM)      =   "1"   )
                 AND  (WRK-COM-YOBI1   (IDXM)(4:1) =   "2"   )
      *            手術に長時間麻酔管理加算＝２があること
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               IF     (WRK-COM-KZMFLG (IDX)    =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    IDX             TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"             TO  WRK-COM-KZMFLG (IDX)
                   MOVE    "K541"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2002-MASUIKANRI-CHK-EXT.
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
      *****************************************************************
      *
