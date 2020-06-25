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
       PROGRAM-ID.                 ORCSC90TEN2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 剤ごと点数計算 (ORCSC90TEN )
      *                      Ｈ２２年４月改正から（SRYYMD 追加）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/03/03    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.03.00    NACL-多々納  07/03/06  酸素点数計算修正
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  10/01/XX  薬評対応
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      *  04.07.00    NACL-多々納  12/03/XX  平成２４年４月改正対応
      *  04.07.00    NACL-多々納  12/08/28  薬剤料減点対応
      *  04.08.00    NACL-多々納  18/06/XX  桁数オーバー対応
      *  05.00.00    NACL-多々納  19/03/XX  点数など桁数オーバー対応
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
           03  FLG-SP              PIC 9(01).
           03  FLG-GENTEN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-END             PIC 9(04).
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
           03  IDX-K               PIC 9(04).
      *
           03  IDX7                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SYOKEIS         PIC 9(10)V9(8).
           03  WRK-SYOKEIS2        PIC 9(10)V9(8).
           03  WRK-SYOKEIS3        PIC 9(10)V9(8).
           03  WRK-SYOKEI          PIC 9(10).
           03  WRK-TENSUKEIS       PIC 9(10)V9(6).
           03  WRK-TENSUKEI-9      PIC 9(10).
      *    薬剤
           03  WRK-ZAIKEI          PIC 9(10)V9(8).
      *    器材
           03  WRK-KIZKEI          PIC 9(10)V9(5).
           03  WRK-KIZKEI-3        PIC 9(10)V9(5).
      *    コバルト
           03  WRK-KIZKEI-4        PIC 9(10)V9(5).
      *    フィルム
           03  WRK-FIRKEI          PIC 9(10)V9(5).
           03  WRK-NYU-FIRKEI      PIC 9(10)V9(5).
           03  WRK-NYU-FIRTEN      PIC 9(10)V9(5).
           03  WRK-FIRTEN-G        PIC 9(10).
           03  WRK-FIRTEN          PIC 9(10).
      *    窒素
           03  WRK-CHISOKEI        PIC 9(10)V9(5).
           03  WRK-CHITEN          PIC 9(10).
      *    酸素
           03  WRK-SANSOKIN        PIC 9(10)V9(5).
           03  WRK-SANSOKIN-SUM    PIC 9(10)V9(5).
           03  WRK-SANSOGOK        PIC 9(10).
           03  WRK-SANTEN          PIC 9(10).
      *
      *    薬剤点数
           03  WRK-SC90-YKZTEN         PIC 9(10).
      *    器材点数
           03  WRK-SC90-KIZTEN         PIC 9(10).
      *    フィルム点数
           03  WRK-SC90-FIRTEN         PIC 9(10).
      *
      *    注射液計算用
           03  WRK-SURYO-CHU9      PIC 9(05).
           03  WRK-SURYO-CHU       PIC 9(05)V9(05).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-MAISU           PIC 9(05).
      *ver.4.7
           03  WRK-GENTEN          PIC 9(08).
      *H31.3
           03  WRK-GOK-TENSU       PIC 9(14).
           03  WRK-GOK-KIZAI       PIC 9(14).
      *
      *診療行為処理最大行
       01  MAX-LINE                PIC 9(03)   VALUE   400.
      *
      *    スパ領域
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC90TEN2.INC".
      *
      *    スパ領域
       01  LNK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //LNK-//.
      *
       PROCEDURE                   DIVISION    USING
           ORCSC90AREA
           LNK-SCR-REC
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
           IF      LNK-SC90-IDX-STR    =   ZERO
               MOVE    1                   TO  IDX-STR
           ELSE
               MOVE    LNK-SC90-IDX-STR    TO  IDX-STR
           END-IF
           IF      LNK-SC90-IDX-END    =   ZERO
               MOVE    MAX-LINE            TO  IDX-END
           ELSE
               MOVE    LNK-SC90-IDX-END    TO  IDX-END
           END-IF
      *
      *    手技毎の集計
           PERFORM 100-MEISAI-SET-SEC
      *
           EVALUATE    LNK-SC90-KBN
      *    剤合計
               WHEN    "2"
      *            酸素・窒素集計
                   PERFORM 200-TENSUKEI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       100-MEISAI-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *    手技料以外の点数集計
           PERFORM VARYING     IDX     FROM    IDX-STR  BY  1
                   UNTIL      (IDX     >   IDX-END     )  OR
                             ((LNK-COM-INPUTCD(IDX)    =   SPACE ) AND
                              (LNK-GMN-INPUTCD(IDX)    =   SPACE ))
               IF      LNK-COM-INPUTCD(IDX) (1:3)  =   "059"
                   MOVE    "7"                 TO  WRK-MOJI
               ELSE
                   MOVE    LNK-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
               END-IF
      *
               MOVE    IDX                 TO  IDX-MAX
      *        薬評は対象外
               IF      LNK-COM-YAKUHYO (IDX)   =   "1"
                   CONTINUE
               ELSE
      *
      *****    MOVE    IDX                 TO  IDX-MAX
               EVALUATE    WRK-MOJI
      *        薬剤
               WHEN    "6"
                   EVALUATE    LNK-COM-YKZKBN(IDX)
                       WHEN    "4"
      *                    注射薬
      *H27.9               外用薬の残量廃棄対応
                       WHEN    "6"
                           PERFORM 1002-CHUUSYA-SET-SEC
      *ver.4.7
                       WHEN    "3"
      *                    その他
                           PERFORM 10021-SONOTA-SET-SEC
      *
                       WHEN    OTHER
      *                    内服薬
                           PERFORM 1001-YAKUZAI-SET-SEC
                   END-EVALUATE
      *        器材
               WHEN    "7"
                   IF      LNK-COM-TENSIKIBETU (IDX)   =   1
                                                       OR  2
                       IF      LNK-COM-DATAKBN (IDX)   =   "3"
      *                    フィルムの編集
                           PERFORM 1003-FIRUMU-SET-SEC
                       ELSE
      *                    フィルム以外の編集
                           PERFORM 1004-KIZAI-SET-SEC
                       END-IF
                   END-IF
               END-EVALUATE
      *
               END-IF
           END-PERFORM
      *
      *    薬剤点数計算（剤内でまとめて計算）
           PERFORM 1005-KEIZAITENU-SET-SEC
      *    器材点数計算処理
           PERFORM 1006-KIZTEN-KEI-SEC
      *    フィルム点数計算処理
           PERFORM 1007-FIRUMU-KEI-SEC
      *    手技の行
           IF      LNK-SC90-IDX        =   ZERO
               MOVE    IDX-MAX             TO  IDX-SYU
           ELSE
               MOVE    LNK-SC90-IDX        TO  IDX-SYU
      *        手技毎
               MOVE    1                   TO  LNK-COM-SYUFLG(IDX-SYU)
           END-IF
      *ver.4.7
      *    薬剤料減点
           IF      WRK-GENTEN          >   ZERO
               MOVE    WRK-GENTEN          TO  WRK-SC90-YKZTEN
           END-IF
      *
      *H31.2
      *    合計が９桁オーバーはエラー
           IF    ((WRK-SC90-YKZTEN *   LNK-SC90-YKZTEN)
                                       >   999999999   )
             OR  ((WRK-SC90-KIZTEN *   LNK-SC90-KIZTEN )
                                       >   999999999   )
             OR  ((WRK-SC90-FIRTEN  *   LNK-SC90-FIRTEN )
                                       >   999999999   )
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
      *
      *    手技点数
           MOVE    LNK-SC90-TENSU      TO  LNK-COM-SYUTEN-M (IDX-SYU)
           ADD     LNK-SC90-TENSU      TO  LNK-SC90-TENSUKEI
      *    薬剤
           MOVE    WRK-SC90-YKZTEN     TO  LNK-COM-YKZTEN-M(IDX-SYU)
           ADD     WRK-SC90-YKZTEN     TO  LNK-SC90-YKZTEN
      *    器材
           MOVE    WRK-SC90-KIZTEN     TO  LNK-COM-KIZTEN-M (IDX-SYU)
           ADD     WRK-SC90-KIZTEN     TO  LNK-SC90-KIZTEN
      *    フィルム点数
           MOVE    WRK-SC90-FIRTEN     TO  LNK-COM-FIRTEN-M (IDX-SYU)
           ADD     WRK-SC90-FIRTEN     TO  LNK-SC90-FIRTEN
           .
       100-MEISAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    内服・頓服編集処理
      *****************************************************************
       1001-YAKUZAI-SET-SEC           SECTION.
      *
      *    点数
           COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)  )
           ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
      *
      *H31.2
      *    金額が１０桁オーバーはエラー
           IF     (LNK-COM-KISOTEN (IDX)   *   LNK-COM-SURYO(IDX))
                                           >   9999999999
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
           .
       1001-YAKUZAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    注射薬編集処理
      *****************************************************************
       1002-CHUUSYA-SET-SEC           SECTION.
      *
      *    残量廃棄判定
      *!   IF     (LNK-COM-TANICD (IDX)    =   "014"
      *                                    OR  "022"
      *!                                   OR  "046"  )  AND
      *    残量廃棄のあるすべての注射薬剤を対象とする
           IF     (IDX                     <   MAX-LINE    )  AND
                  (LNK-COM-INPUTCD(IDX + 1)  =   "099309901")
               MOVE    LNK-COM-SURYO(IDX)    TO  WRK-SURYO
               IF      WRK-SURYO-S2          >   ZERO
                   COMPUTE WRK-SURYO-CHU9      =   LNK-COM-SURYO (IDX)
                                               +   0.99999
                   MOVE    WRK-SURYO-CHU9          TO  WRK-SURYO-CHU
               ELSE
                   MOVE    LNK-COM-SURYO (IDX)     TO  WRK-SURYO-CHU
               END-IF
      *H31.2
      *        金額が１０桁オーバーはエラー
               IF     (LNK-COM-KISOTEN (IDX)   *   WRK-SURYO-CHU)
                                           >   9999999999
                   MOVE    "0249"          TO  LNK-SC90-ERRCD
               END-IF
      *        点数
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   WRK-SURYO-CHU
           ELSE
      *H28.4
      *        湿布薬の枚数変更
               IF     (LNK-COM-SIPPUKBN (IDX)  =   "1" )
                  AND (LNK-COM-SIPGEN (IDX)    =   "1"
                                               OR  "2" )
                   MOVE    LNK-COM-SIP-SURYO (IDX) TO  WRK-SURYO
               ELSE
                   MOVE    LNK-COM-SURYO  (IDX)    TO  WRK-SURYO
               END-IF
      *ない
               MOVE    LNK-COM-SURYO  (IDX)    TO  WRK-SURYO
      *
      *H31.2
      *        金額が１０桁オーバーはエラー
               IF     (LNK-COM-KISOTEN (IDX)   *   WRK-SURYO)
                                           >   9999999999
                   MOVE    "0249"          TO  LNK-SC90-ERRCD
               END-IF
      *
      *        点数
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN(IDX)
                                           *   WRK-SURYO
           END-IF
           ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
           .
       1002-CHUUSYA-SET-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    その他（減点）編集処理
      *****************************************************************
       10021-SONOTA-SET-SEC           SECTION.
      *
           IF     (LNK-COM-TENSIKIBETU (IDX) =   7 )
             AND  (LNK-COM-TEN (IDX)     =   ZERO  )
      *!!!!!
      *      薬剤料減点、治験包括分のみ
             AND  (LNK-COM-INPUTCD (IDX)   =   "630010001")
      *!!!!!
               MOVE    LNK-COM-SURYO(IDX)  TO  WRK-GENTEN
               MOVE    LNK-COM-SURYO(IDX)  TO  LNK-COM-KISOTEN (IDX)
               MOVE    1                   TO  LNK-COM-PLUSKBN(IDX)
           END-IF
           .
       10021-SONOTA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材（フィルム）画像診断以外編集処理
      *****************************************************************
       1003-FIRUMU-SET-SEC           SECTION.
      *
      *H31.2
      *    金額が１０桁オーバーはエラー
           IF     (LNK-COM-KISOTEN (IDX)   *   LNK-COM-SURYO(IDX))
                                           >   9999999999
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
      *
      *    点数
      *    (購入価格＊数量)
           COMPUTE WRK-SYOKEIS         =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           MOVE    WRK-SYOKEIS         TO  WRK-SYOKEI
      *
      *    画像診断
           IF      LNK-COM-SRYKBN (IDX)    =   "70"
      *        特定器材年齢加算区分判定
               IF      LNK-COM-TOKUKIZAIAGEKSNKBN (IDX)
                                           =   1
      *            乳幼児加算分
                   PERFORM 10031-FIRUMU-KASAN-SEC
      ************ ADD     WRK-SYOKEI          TO  WRK-NYU-FIRKEI
               ELSE
      *            フィルム点数加算
                   ADD     WRK-SYOKEI          TO  WRK-FIRKEI
               END-IF
           ELSE
      *        上限点数判定（画像診断以外）
               IF      LNK-COM-JGNTEN (IDX) =   ZERO
      *            フィルム点数加算
                   ADD     WRK-SYOKEI          TO  WRK-FIRKEI
               ELSE
                   COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                               /   10
                                               +   0.5
                   MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
                   IF      WRK-FIRTEN          >   LNK-COM-JGNTEN (IDX)
                       MOVE    LNK-COM-JGNTEN(IDX) TO  WRK-FIRTEN
      *                器材点数に上限点数を集計
                       ADD     WRK-FIRTEN          TO  WRK-FIRTEN-G
                   ELSE
                       ADD     WRK-SYOKEI          TO  WRK-FIRKEI
                   END-IF
               END-IF
           END-IF
           .
       1003-FIRUMU-SET-EXT.
           EXIT.
      *****************************************************************
      *    器材編集処理
      *****************************************************************
       1004-KIZAI-SET-SEC            SECTION.
      *
      *H31.2
      *    金額が１０桁オーバーはエラー
           IF     (LNK-COM-KISOTEN (IDX)   *   LNK-COM-SURYO(IDX))
                                           >   9999999999
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
      *
           EVALUATE    LNK-COM-TOKUKIZAISBT1 (IDX)
               WHEN    ZERO
      *            通常
      *            (購入価格＊数量)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI
               WHEN    2
                   IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            窒素
      *                (購入価格＊数量)　四捨五入
                       COMPUTE WRK-SYOKEIS2    =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                       MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                       ADD     WRK-SYOKEIS         TO  WRK-CHISOKEI
                   END-IF
      *            酸素
                   IF      LNK-COM-SANSOKBN (IDX)  =   1
      *                (購入価格＊数量)
                       COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX)
                       ADD     WRK-SYOKEIS2        TO  WRK-SANSOKIN-SUM
                   END-IF
               WHEN    3
      *            高線量率イリジウム
      *            (購入価格＊数量)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI-3
               WHEN    4
      *            コバルト
      *            (購入価格＊数量)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI-4
           END-EVALUATE
      *
           .
       1004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤点数計算処理
      *****************************************************************
       1005-KEIZAITENU-SET-SEC           SECTION.
      *
           MOVE    WRK-ZAIKEI          TO  WRK-SYOKEIS2
      *
      * =============================================================
      *---// １日分の点数　15点以下 ⇒ １点 （薬剤以外の時、ゼロ）  //
      *-- // １日分の点数　15点超過 ⇒ （１日分の点数−15）/ 10 
      *==============================================================
      *
           IF      WRK-SYOKEIS2        <=  15
               IF     (LNK-COM-SRYKBN (01)     =   "21"  OR  "22"  OR
                                                   "23"  OR
                                                   "31"  OR  "32"  OR
                                                   "33"   )  AND
                      (WRK-SYOKEIS2        >   ZERO       )
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
               COMPUTE WRK-SYOKEIS3        =  ((WRK-SYOKEIS2   -  15)
                                           /    10 )
                                           +   1
           END-IF
      *
           COMPUTE WRK-SC90-YKZTEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       1005-KEIZAITENU-EXT.
           EXIT.
      *****************************************************************
      *    器材点数計算処理（フィルム、酸素、窒素以外）
      *****************************************************************
       1006-KIZTEN-KEI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SC90-KIZTEN
      *    その他器材
           IF      WRK-KIZKEI          >   ZERO
      *    金額計÷１０　四捨五入
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI
                                           /   10  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *    高線量率イリジウム
           IF      WRK-KIZKEI-3        >   ZERO
      *H22.4 改正
      *        金額計÷５０　四捨五入　（H22.4 から）
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI-3
                                           /   50  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *H24.4 改正
      *    コバルト
           IF      WRK-KIZKEI-4        >   ZERO
      *        金額計÷1000　四捨五入
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI-4
                                           /   1000 )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *
           .
       1006-KIZTEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像診断乳幼児加算フィルム点数計算処理
      *****************************************************************
       10031-FIRUMU-KASAN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEIS
           COMPUTE IDX7                =   IDX   +   1
           IF      LNK-COM-INPUTCD(IDX7)
                                       =   "799990070"
      *        直下に乳幼児加算がある時
               COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                           *   LNK-COM-KISOTEN (IDX7)
                                           /   100
               COMPUTE WRK-NYU-FIRTEN      =   WRK-SYOKEI
                                           +   WRK-TENSUKEIS
           ELSE
               MOVE    WRK-SYOKEI          TO  WRK-NYU-FIRTEN
           END-IF
      *
           ADD     WRK-NYU-FIRTEN          TO  WRK-NYU-FIRKEI
      *
           .
       10031-FIRUMU-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    フィルム点数計算処理
      *****************************************************************
       1007-FIRUMU-KEI-SEC           SECTION.
      *
           IF      WRK-NYU-FIRKEI      >   ZERO
               COMPUTE WRK-FIRKEI          =   WRK-FIRKEI
                                           +   WRK-NYU-FIRKEI
           END-IF
      *
           IF      WRK-FIRKEI        >   ZERO
      *        金額計÷１０　四捨五入
               COMPUTE WRK-SYOKEIS2        =  (WRK-FIRKEI
                                           /   10  )
                                           +   0.5
               MOVE    WRK-SYOKEIS2        TO  WRK-SC90-FIRTEN
           ELSE
      *        上限点数分
               MOVE    WRK-FIRTEN-G        TO  WRK-SC90-FIRTEN
           END-IF
      *
           .
       1007-FIRUMU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       200-TENSUKEI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-CHISOKEI
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  FLG-GENTEN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   MAX-LINE  )  OR
                             ((LNK-COM-INPUTCD(IDX)    =   SPACE ) AND
                              (LNK-GMN-INPUTCD(IDX)    =   SPACE ))
               ADD     1                   TO  IDX-MAX
               IF     (LNK-COM-INPUTCD (IDX)(1:1)  =   "7"  ) AND
                      (LNK-COM-TENSIKIBETU (IDX)   =   1
                                                   OR  2    )
                   IF      LNK-COM-TOKUKIZAISBT1(IDX)   =  2
      *                酸素・窒素の集計
                       PERFORM 2004-KIZAI-SET-SEC
                   END-IF
               END-IF
      *        減点
               IF     (LNK-COM-PLUSKBN (IDX)   =   1   )
                   MOVE    1               TO  FLG-GENTEN
               END-IF
               IF      FLG-GENTEN          =   1
                   MOVE    1               TO  LNK-COM-PLUSKBN (IDX)
               END-IF
           END-PERFORM
      *    窒素点数計算処理
           IF      WRK-CHISOKEI        =   ZERO
               MOVE      ZERO              TO  LNK-SC90-CHITEN
           ELSE
               PERFORM 2007-CHITEN-KEI-SEC
           END-IF
      *    酸素点数計算処理
           IF      WRK-SANSOKIN-SUM    =   ZERO
               MOVE      ZERO              TO  LNK-SC90-SANTEN
           ELSE
               PERFORM 2008-SANSO-KEI-SEC
           END-IF
      *
           MOVE    LNK-SC90-CHITEN     TO  LNK-COM-CHISOTEN(IDX-MAX)
           MOVE    LNK-SC90-SANTEN     TO  LNK-COM-SANSOTEN(IDX-MAX)
      *
      *    合計点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  LNK-COM-SYUTEN1 (IDX-MAX)
      *    薬剤
           MOVE    LNK-SC90-YKZTEN     TO  LNK-COM-YKZTEN  (IDX-MAX)
      *    器材
      *H31.3
      *    COMPUTE LNK-COM-KIZAITEN(IDX-MAX)
      *                                =   LNK-SC90-KIZTEN
      *                                +   LNK-SC90-SANTEN
      **                               +   LNK-SC90-CHITEN
           COMPUTE WRK-GOK-KIZAI
                                       =   LNK-SC90-KIZTEN
                                       +   LNK-SC90-SANTEN
                                       +   LNK-SC90-CHITEN
           MOVE    WRK-GOK-KIZAI       TO  LNK-COM-KIZAITEN(IDX-MAX)
      *    フィルム
           IF      LNK-COM-SRYKBN (01)    =   "70"
      *        フィルム料を手技料２へ
               MOVE    LNK-SC90-FIRTEN     TO  LNK-COM-SYUTEN2(IDX-MAX)
      *        フィルム料を手技料へ加算
               COMPUTE LNK-COM-SYUTEN1 (IDX-MAX)
                                           =   LNK-COM-SYUTEN1 (IDX-MAX)
                                           +   LNK-SC90-FIRTEN
           ELSE
               ADD     LNK-SC90-FIRTEN     TO  LNK-COM-KIZAITEN(IDX-MAX)
           END-IF
      *
      *    点数計　計算
      *    COMPUTE LNK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
      *                                    +   LNK-SC90-YKZTEN
      *                                    +   LNK-SC90-KIZTEN
      *                                    +   LNK-SC90-FIRTEN
      *                                    +   LNK-SC90-SANTEN
      *                                    +   LNK-SC90-CHITEN
      *H31.3
      *    点数計　計算
           COMPUTE WRK-GOK-TENSU           =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-YKZTEN
                                           +   LNK-SC90-KIZTEN
                                           +   LNK-SC90-FIRTEN
                                           +   LNK-SC90-SANTEN
                                           +   LNK-SC90-CHITEN
      *    ８桁まで編集する
           MOVE    WRK-GOK-TENSU           TO  LNK-COM-TENSU (IDX-MAX)
      *    ７桁まで
           IF      WRK-GOK-TENSU       >   9999999
               MOVE    "0249"              TO  LNK-SC90-ERRCD
           END-IF
      *
      *    減点分
           IF      FLG-GENTEN          =   1
               MOVE    1               TO  LNK-COM-PLUSKBN (IDX-MAX)
           END-IF
      *
           .
       200-TENSUKEI-EXT.
           EXIT.
      *****************************************************************
      *    器材編集処理
      *****************************************************************
       2004-KIZAI-SET-SEC            SECTION.
      *
           EVALUATE    LNK-COM-TOKUKIZAISBT1 (IDX)
               WHEN    2
                   IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            窒素
      *                (購入価格＊数量)　四捨五入
                       COMPUTE WRK-SYOKEIS2    =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                       MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                       ADD     WRK-SYOKEIS         TO  WRK-CHISOKEI
                   END-IF
      *            酸素
                   IF      LNK-COM-SANSOKBN (IDX)  =   1
      *                (購入価格＊数量)
                       COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX)
                       ADD     WRK-SYOKEIS2        TO  WRK-SANSOKIN-SUM
                   END-IF
           END-EVALUATE
      *
           .
       2004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材点数計算処理（窒素）
      *****************************************************************
       2007-CHITEN-KEI-SEC             SECTION.
      *
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =  (WRK-CHISOKEI
                                       /   10  )
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-CHITEN
      *
           .
       2007-CHITEN-KEI-SEC-EXT.
           EXIT.
      *****************************************************************
      *     酸素の点数数計算処理
      *****************************************************************
       2008-SANSO-KEI-SEC                SECTION.
      *
           MOVE      ZERO              TO  LNK-SC90-SANTEN
      *
      *    酸素毎の補正率加算とする
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
           MOVE    ZERO                TO  IDX-H
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   IDX-MAX )
               EVALUATE    LNK-COM-INPUTCD (IDX)
      *            酸素補正率自動加算
                   WHEN    "770020070"
                       MOVE    IDX             TO  IDX-H
      *            高気圧酸素計算
                   WHEN    "770030070"
                       MOVE    IDX             TO  IDX-K
               END-EVALUATE
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-SYOKEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-MAX )
               MOVE    ZERO                TO  WRK-SANSOKIN
               MOVE    ZERO                TO  WRK-SANSOGOK
      *        酸素
               IF     (LNK-COM-INPUTCD (IDX)(1:1)  =   "7"  ) AND
                      (LNK-COM-TENSIKIBETU (IDX)   =   1
                                                   OR  2    ) AND
                      (LNK-COM-TOKUKIZAISBT1(IDX)  =   2    ) AND
                      (LNK-COM-SANSOKBN (IDX)      =   1    )
      *            (購入価格＊数量)
                   COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                           *   LNK-COM-SURYO  (IDX)
      *            酸素金額＋（酸素金額×補正率／１００）　四捨五入
                   COMPUTE WRK-SYOKEIS3    =   WRK-SYOKEIS2
                                           *   LNK-COM-KISOTEN (IDX-H)
                                           /   100
                   COMPUTE WRK-SANSOKIN    =   WRK-SYOKEIS2
                                           +   WRK-SYOKEIS3
      *            高気圧酸素計算
                   IF      IDX-K           >   ZERO
      *                酸素金額＋（酸素金額×補正率／１００）
      *                                           ×気圧数　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                               *   LNK-COM-SURYO (IDX-K)
                       MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
                   END-IF
      *            金額　四捨五入
                   COMPUTE WRK-SANSOGOK        =   WRK-SANSOKIN
                                               +   0.5
                   COMPUTE WRK-SYOKEI      =   WRK-SYOKEI
                                           +   WRK-SANSOGOK
               END-IF
           END-PERFORM
      *
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   IDX-MAX
      *        EVALUATE    LNK-COM-INPUTCD (IDX)
      *            高気圧酸素計算
      *            WHEN    "770030070"
      *                酸素金額＋（酸素金額×補正率／１００）
      *                                           ×気圧数　四捨五入
      *                COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
      *                                        *   LNK-COM-SURYO (IDX)
      *                MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
      *        END-EVALUATE
      *    END-PERFORM
      *    酸素の点数数計算
      *    金額計　四捨五入
      *    COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
      *                                +   0.5
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *    酸素の点数を器材点数に加算
           MOVE    WRK-SANTEN          TO  LNK-SC90-SANTEN
      *
           .
       2008-SANSO-KEI-EXT.
           EXIT.
      *
      *
