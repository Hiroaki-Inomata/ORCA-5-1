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
       PROGRAM-ID.                 ORCSC90.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 剤ごとの算定サブより、編集・計算処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.05.25  投薬点数計算の中間桁数の変更
      *  01.00.02    NACL-多々納  04.11.18  フィルム料計算変更
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS-Z    PIC ZZZZ9.99.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-KEICOM-X.
               05  WRK-KEICOM-Z    PIC ZZZZZZZ9.
           03  WRK-KEICOMS-X.
               05  WRK-KEICOMS-Z   PIC ZZZZ9V99.
           03  WRK-GOKEI-X.
               05  WRK-GOKEI-Z     PIC ZZZZZZZZ.
           03  WRK-GMN-INPUTCD     PIC X(22).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEIS3        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
           03  WRK-TENSUKEIS       PIC 9(08)V9(6).
           03  WRK-FIRTEN          PIC 9(08).
      *
      *    注射液計算用
           03  WRK-SURYO-CHU9      PIC 9(05).
           03  WRK-SURYO-CHU       PIC 9(05)V9(05).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-MAISU           PIC 9(05).
      *
      *診療行為処理最大行
       01  MAX-LINE                PIC 9(03)   VALUE   400.
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC90.INC".
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
           IF      LNK-SC90-IDX        <   1  OR  >  MAX-LINE
               GO      TO      000-PROC-EXT
           END-IF
      *
           MOVE    LNK-SC90-IDX        TO  IDX
      *
           EVALUATE    LNK-SC90-KBN
      *        明細編集
               WHEN    "1"
                   PERFORM 100-MEISAI-SET-SEC
      *        点数計算
               WHEN    "2"
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
           IF      LNK-COM-INPUTCD(IDX) (1:3)  =   "059"
               MOVE    "7"                 TO  WRK-MOJI
           ELSE
               MOVE    LNK-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
           END-IF
      *
      **** EVALUATE    LNK-COM-INPUTCD(1:1)
           EVALUATE    WRK-MOJI
      *        手技料
               WHEN    "1"
                   PERFORM 1001-SYUGI-SET-SEC
      *        薬剤
               WHEN    "6"
                   EVALUATE    LNK-COM-YKZKBN(IDX)
                       WHEN    "6"
      *                    外用薬
                           PERFORM 1003-GAIYAKU-SET-SEC
                       WHEN    "4"
      *                    注射薬
                           PERFORM 1006-CHUUSYA-SET-SEC
                       WHEN    OTHER
      *                    内服薬
                           PERFORM 1002-YAKUZAI-SET-SEC
                   END-EVALUATE
      *        器材
               WHEN    "7"
                   IF      LNK-COM-DATAKBN (IDX)    =   "3"
      *                フィルムの編集
                       PERFORM 1005-FIRUMU-SET-SEC
                   ELSE
      *                フィルム以外の編集
                       PERFORM 1004-KIZAI-SET-SEC
                   END-IF 
           END-EVALUATE
           .
       100-MEISAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料編集処理
      *****************************************************************
       1001-SYUGI-SET-SEC           SECTION.
      *
      *****数量
      *    IF      LNK-COM-SURYO       >   1
      *        MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *        PERFORM 10011-SURYO-HEN-SEC
      *        MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    ELSE
      *        MOVE    SPACE               TO  LNK-GMN-SURYO
      *****END-IF
      *    点数
      *        ％加算・減算は集計しない
           IF      LNK-COM-TENSIKIBETU (IDX)   =   5
                                               OR  6
               CONTINUE
           ELSE
      *        通則加算対象外区分判定
               IF      LNK-COM-TUSOKUGAIKBN (IDX)  =   "1"
                   ADD     LNK-COM-KISOTEN (IDX)  TO  LNK-SC90-TENGAKEI
               ELSE
                   ADD     LNK-COM-KISOTEN(IDX)   TO  LNK-SC90-TENSUKEI
               END-IF
           END-IF
      *    回数
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU  (IDX)
           .
       1001-SYUGI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服・頓服編集処理
      *****************************************************************
       1002-YAKUZAI-SET-SEC           SECTION.
      *
      *    数量
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    点数
           COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)  )
           ADD     WRK-SYOKEIS2        TO  LNK-SC90-ZAIKEI
      *    回数
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
           .
       1002-YAKUZAI-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射薬編集処理
      *****************************************************************
       1006-CHUUSYA-SET-SEC           SECTION.
      *
      *    数量
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *
           MOVE    ZERO                TO  LNK-SC90-HAIKI
      *
      *    注射で、アンプルの時、単位以下計算
      *    IF    ((LNK-COM-SRYKBN (IDX)    =   "31" OR "32" OR
      *                                                "33"
      *D          処置追加
      *D                                   OR  "40" 
      *                                                   ) AND
      *          ((LNK-COM-TANICD (IDX)    =   "014" OR "022" OR "046")
      *        OR  ((IDX                     <   MAX-LINE  )  AND
      *             (LNK-COM-INPUTCD(IDX + 1)  =   "099309901"))) )
      *        処置の時、次が残量廃棄
      *       OR  ((LNK-COM-SRYKBN (IDX)    =   "40" )  AND
      *            (IDX                     <   MAX-LINE  )  AND
      ************ (LNK-COM-INPUTCD(IDX + 1)  =   "099309901"))
      *    IF     (LNK-COM-TANICD (IDX)    =   "014"
      *                                    OR  "022"
      ***                                  OR  "046"  )  AND
      *    残量廃棄のあるすべての注射薬を対象とする
           IF     (IDX                     <   MAX-LINE )  AND
                  (LNK-COM-INPUTCD(IDX + 1)  =   "099309901")
               IF      LNK-COM-SURYO(IDX)(6:5) >   ZERO
                   COMPUTE WRK-SURYO-CHU9      =   LNK-COM-SURYO (IDX)
                                               +   0.99999
                   MOVE    WRK-SURYO-CHU9      TO  WRK-SURYO-CHU
               ELSE
                   MOVE    LNK-COM-SURYO (IDX)     TO  WRK-SURYO-CHU
               END-IF
      *        点数
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   WRK-SURYO-CHU
           ELSE
      *        点数
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN(IDX)
                                           *   LNK-COM-SURYO  (IDX)
           END-IF
      *
           ADD     WRK-SYOKEIS2        TO  LNK-SC90-ZAIKEI
      *    回数
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
           .
       1006-CHUUSYA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外用編集処理
      *****************************************************************
       1003-GAIYAKU-SET-SEC            SECTION.
      *
      *    回数は１回とする（数量＊回数を数量とする）(ORCSC20で判定）
      *    COMPUTE LNK-COM-SURYO (IDX) =   LNK-COM-SURYO (IDX)
      *                                *   LNK-SC90-KAISU 
      *    数量
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    点数
           COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           ADD     WRK-SYOKEIS2        TO  LNK-SC90-GAIKEI
      *    回数
      *****MOVE    1                   TO  LNK-COM-KAISU  (IDX)
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU  (IDX)
           .
       1003-GAIYAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    器材編集処理
      *****************************************************************
       1004-KIZAI-SET-SEC            SECTION.
      *
      *    数量
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    点数
      *        特定器材種別１判定
           IF      LNK-COM-TENSIKIBETU (IDX)   =   5
                                               OR  6
      *
      *        回数
               MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
      *
               GO     TO  1004-KIZAI-SET-EXT
           END-IF
      *
           IF      LNK-COM-TOKUKIZAISBT1 (IDX) =   ZERO
      *        (購入価格＊数量)
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   LNK-COM-SURYO   (IDX)
      *
               ADD     WRK-SYOKEIS2        TO  LNK-SC90-KIZKEI
           ELSE
      *        窒素
               IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            (購入価格＊数量)　四捨五入
                   COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                   MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                   ADD     WRK-SYOKEIS         TO  LNK-SC90-CHIKEI
               END-IF
           END-IF
      *
      *    回数
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
      *
           .
       1004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材（フィルム）編集処理
      *****************************************************************
       1005-FIRUMU-SET-SEC           SECTION.
      *
      *    数量
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    枚数加算
      *****ADD     LNK-COM-SURYO       TO  LNK-SC90-MAISU
      *
      *    分画数
           IF      LNK-COM-BUNGSU (IDX)    =   ZERO
               MOVE    1                   TO  LNK-COM-BUNGSU (IDX)
           END-IF
           ADD     LNK-COM-BUNGSU (IDX)    TO  LNK-SC90-BUNGSU
      *---(01.00.03) ---  
      *    枚数加算
           COMPUTE WRK-MAISU           =   LNK-COM-SURYO  (IDX)
                                       *   LNK-COM-BUNGSU (IDX)
           ADD     WRK-MAISU           TO  LNK-SC90-MAISU
      *---(01.00.03)
      *    点数
      *    (購入価格＊数量)
           COMPUTE WRK-SYOKEIS         =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           MOVE    WRK-SYOKEIS         TO  WRK-SYOKEI
      *
      *    画像診断以外の計算
           IF      LNK-COM-SRYKBN (IDX)    =   "70"
      *        フィルム点数加算
               MOVE    WRK-SYOKEI          TO  LNK-SC90-FIRTEN
           ELSE
               PERFORM 10051-FIRUMU-ETC-SEC
           END-IF
      *
      *    回数
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
      *
           .
       1005-FIRUMU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像診断以外の計算処理
      *****************************************************************
       10051-FIRUMU-ETC-SEC           SECTION.
      *    上限点数判定
           IF      LNK-COM-JGNTEN (IDX) =   ZERO
      *        フィルム点数加算
               ADD     WRK-SYOKEI          TO  LNK-SC90-FIRTEN
           ELSE
               COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                           /   10
                                           +   0.5
               MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
               IF      WRK-FIRTEN          >   LNK-COM-JGNTEN (IDX)
                   MOVE    LNK-COM-JGNTEN(IDX) TO  WRK-FIRTEN
      *            器材点数に上限点数を集計
                   ADD     WRK-FIRTEN          TO  LNK-SC90-KIZTEN
               ELSE
                   ADD     WRK-SYOKEI          TO  LNK-SC90-FIRTEN
               END-IF
           END-IF
      *
           .
       10051-FIRUMU-ETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    数計算処理
      *****************************************************************
       200-TENSUKEI-SEC           SECTION.
      *    薬剤点数計算処理
      *    IF      LNK-SC90-ZAIKEI     =   ZERO
      *        MOVE      ZERO              TO  LNK-SC90-ZAITEN
      *    ELSE
      *        PERFORM 2001-ZAITENU-SET-SEC
      *    END-IF
      *
      *    外用薬点数計算処理
      *    IF      LNK-SC90-GAIKEI     =   ZERO
      *        MOVE      ZERO              TO  LNK-SC90-GAITEN
      *    ELSE
      *        PERFORM 2002-GAITENU-SET-SEC
      *    END-IF
      *
      *    薬剤点数計算（剤内でまとめて計算）
           PERFORM 2001-KEIZAITENU-SET-SEC
      *
      *    器材点数計算処理
      *    IF      LNK-SC90-KIZKEI     =   ZERO
      *        MOVE      ZERO              TO  LNK-SC90-KIZTEN
      *    ELSE
               PERFORM 2003-KIZTEN-SET-SEC
      *****END-IF
      *
      *    器材（窒素）点数計算処理
           IF      LNK-SC90-CHIKEI     =   ZERO
               MOVE      ZERO              TO  LNK-SC90-CHITEN
           ELSE
               PERFORM 2004-CHITEN-SET-SEC
           END-IF
      *
           COMPUTE LNK-SC90-KIZTEN     =   LNK-SC90-KIZTEN
                                       +   LNK-SC90-CHITEN
           .
       200-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤点数計算処理
      *****************************************************************
       2001-KEIZAITENU-SET-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SYOKEIS2
           COMPUTE WRK-SYOKEIS2        =   LNK-SC90-ZAIKEI
                                       +   LNK-SC90-GAIKEI
      *
      * =============================================================
      *---// １日分の点数　15点以下 ⇒ １点 （薬剤以外の時、ゼロ）  //
      *-- // １日分の点数　15点超過 ⇒ （１日分の点数−15）/ 10 
      *==============================================================
      *
           IF      WRK-SYOKEIS2        <=  15
               IF     (LNK-COM-SRYKBN (IDX)    =   "21"  OR  "22"  OR
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
           COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       2001-KEIZAITENU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    薬剤点数計算処理
      *****************************************************************
       2001-ZAITENU-SET-SEC           SECTION.
      *
      *****MOVE    ZERO                TO  WRK-SYOKEIS2
           MOVE    ZERO                TO  WRK-SYOKEIS3
      *
      * =============================================================
      *---// １日分の点数　15点以下 ⇒ １点 （薬剤以外の時、ゼロ）  //
      *-- // １日分の点数　15点超過 ⇒ （１日分の点数−15）/ 10 
      *==============================================================
      *
           IF      LNK-SC90-ZAIKEI         <=  15
               IF    ( LNK-COM-SRYKBN (IDX)    =   "21"  OR  "22"  OR
                                                   "23"  OR
                                                   "31"  OR  "32"  OR
                                                   "33"    )  AND
                      ( LNK-SC90-ZAIKEI        >    ZERO   )
      *
      ***********  MOVE    1               TO  WRK-SYOKEIS2
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
      ***********  MOVE    ZERO            TO  WRK-SYOKEIS2
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
      ******** COMPUTE WRK-SYOKEIS2    =  (LNK-SC90-ZAIKEI   -  15)
               COMPUTE WRK-SYOKEIS3    =  ((LNK-SC90-ZAIKEI  -  15)
                                       / 10 )
                                       +   1
           END-IF
      *
      *****COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS2   + 0.99
           COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       2001-ZAITENU-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤点数計算処理（外用薬）
      *****************************************************************
       2002-GAITENU-SET-SEC           SECTION.
      *
      * =============================================================
      *---// １日分の点数　15点以下 ⇒ １点（薬剤以外の時ゼロ）     //
      *-- // １日分の点数　15点超過 ⇒ （１日分の点数−15）/ 10 + 1 //
      *=============================================================
           IF      LNK-SC90-GAIKEI         <=  15
               IF    ( LNK-COM-SRYKBN (IDX)    =   "21"  OR
                                               "22"  OR
                                               "23"  OR
                                               "31"  OR
                                               "32"  OR
                                               "33"       )  AND
                      (LNK-SC90-GAIKEI     >   ZERO       )
      *************MOVE    1               TO  WRK-SYOKEIS2
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
      ************ MOVE    ZERO            TO  WRK-SYOKEIS2
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
      *********COMPUTE WRK-SYOKEIS2    =  (LNK-SC90-GAIKEI    -  15)
               COMPUTE WRK-SYOKEIS3    =  ((LNK-SC90-GAIKEI   -  15)
                                                   / 10)
                                                   +   1
           END-IF
      *
      *****COMPUTE LNK-SC90-GAITEN     =   WRK-SYOKEIS2   + 0.99
           COMPUTE LNK-SC90-GAITEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       2002-GAITENU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材点数計算処理（フィルム、酸素、窒素以外）
      *****************************************************************
       2003-KIZTEN-SET-SEC             SECTION.
      *
      *?   画像診断以外のフィルム計算
      *    IF      LNK-COM-SRYKBN (01) =   "70"
      *        MOVE    LNK-SC90-KIZKEI     TO  WRK-SYOKEIS3
      *    ELSE
      *        COMPUTE WRK-SYOKEIS3    =   LNK-SC90-KIZKEI
      *                                +   LNK-SC90-FIRTEN
      *?   END-IF
      *    画像診断以外のフィルム計算
           IF      LNK-COM-SRYKBN (01) NOT =   "70"
               MOVE    LNK-SC90-FIRTEN     TO  WRK-SYOKEIS3
               IF      WRK-SYOKEIS3        >   ZERO
      *        金額計÷１０　四捨五入
                   COMPUTE WRK-SYOKEIS2        =  (WRK-SYOKEIS3
                                               /   10  )
                                               +   0.5
                   ADD     WRK-SYOKEIS2        TO  LNK-SC90-KIZTEN
               END-IF
           END-IF
      *    器材
           MOVE    LNK-SC90-KIZKEI     TO  WRK-SYOKEIS3
           IF      WRK-SYOKEIS3        >   ZERO
      *    金額計÷１０　四捨五入
               COMPUTE WRK-SYOKEIS2        =  (WRK-SYOKEIS3
                                           /   10  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  LNK-SC90-KIZTEN
           END-IF
      *
           .
       2003-KIZTEN-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    器材点数計算処理（窒素）
      *****************************************************************
       2004-CHITEN-SET-SEC             SECTION.
      *
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =  (LNK-SC90-CHIKEI
                                       /   10  )
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-CHITEN
      *
           .
       2004-CHITEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
      *10011-SURYO-HEN-SEC                  SECTION.
      *
      *    MOVE    SPACE               TO  WRK-SURYO-X
      *    MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
      *    MOVE    ZERO                TO  FLG-SP
      *    PERFORM VARYING     IDM     FROM    3  BY  -1
      *            UNTIL       IDM     <   1
      *        IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
      *            MOVE    1               TO  FLG-SP
      *        END-IF
      *        IF      FLG-SP          =   1
      *            MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
      *        END-IF
      *    END-PERFORM
      *
      *    IF      WRK-SURYO-Z3        NOT =   SPACE
      *        MOVE    "."                 TO  WRK-SURYO-Z2
      *            MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
      *        END-IF
      *    END-IF
      *    .
      *10011-SURYO-HEN-EXT.
      *    EXIT.
      *
