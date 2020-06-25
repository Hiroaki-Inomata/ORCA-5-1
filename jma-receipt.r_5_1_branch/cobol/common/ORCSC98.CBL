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
       PROGRAM-ID.                 ORCSC98.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 点数計算後共通チェック処理
      *                      （投薬量等のチェック）
      *                      （Ｋ０２、Ｊ０４のみ共通使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/12/12    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.01.00    NACL-多々納  07/12/XX  患者禁忌薬剤対応
      *  04.08.00    NACL-多々納  16/11/XX  投薬投与量チェック区分対応
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
           03  FLG-SP                  PIC 9(01).
           03  FLG-TOUYO-CHK           PIC 9(01).
           03  FLG-TOUYO-CHK1          PIC 9(01).
           03  FLG-TOUYO-CHK2          PIC 9(01).
           03  FLG-NENREI-CD           PIC 9(01).
           03  FLG-KINKI-ARI           PIC 9(01).
      *
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-YAKU-TBL            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDM2                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-SET             PIC 9(04).
           03  IDX-N2              PIC 9(02).
      *
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDY-TBL             PIC 9(04).
           03  IDX-C               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NEN                 PIC 9(03).
           03  WRK-TOURYO              PIC 9(05)V9(05).
      *
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
           03  WRK-SURYO-H         PIC X(11).
      *    年令チェック
           03  WRK-NENCD               PIC X(02).
           03  WRK-NENREI.
               05  WRK-NEN-YY          PIC 9(03).
               05  WRK-NEN-MM          PIC 9(02).
               05  WRK-NEN-DD          PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-ERRMSG              PIC X(100).
      *    月総量・上限回数チェック
           03  WRK-SRYCD               PIC X(09).
           03  WRK-KAISU               PIC S9(03).
           03  WRK-TUKIKAISU           PIC S9(03).
           03  WRK-TUKISURYO           PIC S9(05)V9(05).
           03  WRK-KAISU-H             PIC ZZ9.
           03  WRK-KAISU-H2            PIC ZZ9.
           03  WRK-SOURYO-H1           PIC X(11).
           03  WRK-SOURYO-H2           PIC X(11).
      *
      *    年令単位テーブル
           COPY    "CMNENREICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    投与量等チェックパラメタ
           COPY    "CPORCSC98.INC".
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSC98AREA
           SPA-SCR-REC
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
      *
           EVALUATE    LNK-SC98-KBN
               WHEN    SPACE
                   PERFORM 100-MAIN-SYORI-SEC
               WHEN    "1"
                   PERFORM 200-TUKI-SYORI-SEC
           END-EVALUATE
      *
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       100-MAIN-SYORI-SEC            SECTION.
      *
           MOVE    SPACE               TO  LNK-SC98-ERRMSG
           MOVE    ZERO                TO  IDX-SET
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
                           OR (SPA-ERRCD   NOT =   SPACE   )
      *        セットの行位置
               IF      SPA-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    IDX-Z               TO  IDX-SET
               END-IF
      *
      *        患者禁忌薬剤チェック
               IF     (SPA-COM-INPUTCD (IDX-Z) (1:1)
                                                   =   "6" )
                   PERFORM 300-PTKINKI-CHK-SEC
               END-IF
      *
      *        在宅・投薬・注射のみ
               IF    ((SPA-COM-SRYKBN (01)     =   "14"
                                               OR  "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "31"
                                               OR  "32"
                                               OR  "33" )
      *H28.11    すべての診療区分
                 OR   (LNK-SC98-TOUYOCHK-FLG   =   "1" ))
               AND   ( SPA-ERRCD               =   SPACE )
      *            点数附加情報のチェック
                   PERFORM 200-TENSUPLUS-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       100-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数附加情報のチェック処理
      *****************************************************************
       200-TENSUPLUS-CHK-SEC            SECTION.
      *
      *    長期投薬期間
           IF     LNK-SC98-PG              =   "J04"
               CONTINUE
           ELSE
               IF    (SPA-COM-INPUTCD(IDX-Z)(1:1)  =   "6" ) AND
                     (SPA-COM-CHOKINISSU (IDX-Z)   >   ZERO) AND
                     (SPA-COM-CHOKINISSU (IDX-Z)   <   SPA-COM-KAISU
                                                          (IDX-Z))
      *H28.11      投薬のみ
                   IF    ( SPA-COM-SRYKBN (01) =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "31"
                                               OR  "32"
                                               OR  "33" )
                       PERFORM 10012-CHOKI-ERR-SEC
                   END-IF
               END-IF
           END-IF
      *    投与量
           IF    (SPA-COM-INPUTCD(IDX-Z)(1:1)  =   "6" ) AND
                 (SPA-ERRCD                    =   SPACE)
               IF     (SPA-COM-SURFLG (IDX-Z)  =   "1" )  OR
                      (SPA-COM-SURCHK (IDX-Z)  NOT =   SPACE)
                   PERFORM 1001-TOUYO-CHK-SEC
                   MOVE    "1"             TO  SPA-COM-SURCHK
                                                              (IDX-Z)
               ELSE
                   MOVE    "2"             TO  SPA-COM-SURCHK
                                                              (IDX-Z)
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  LNK-SC98-ERRMSG
           END-IF
           .
       200-TENSUPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期日数エラー処理
      *****************************************************************
       10012-CHOKI-ERR-SEC            SECTION.
      *
           IF     (SPA-COM-CHOKIERR (IDX-Z)    =   9 )
               IF      SPA-COM-SET (IDX-Z) =   "1"
                   IF      SPA-COM-KZMFLG4 (IDX-SET) =   SPACE
                       MOVE    "K200"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-KZMFLG4
                                                            (IDX-SET)
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-SET)
                   END-IF
               ELSE
                   IF      SPA-COM-KZMFLG4 (IDX-Z) =   SPACE
                       MOVE    "K200"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-KZMFLG4
                                                            (IDX-Z)
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           ELSE
               MOVE    "0200"              TO  SPA-ERRCD
               IF      SPA-COM-SET (IDX-Z) =   "1"
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-SET)
               ELSE
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
           MOVE    SPA-COM-CHOKINISSU (IDX-Z)
                                       TO  WRK-SURYO
           PERFORM 10021-SURYO-HEN-SEC
      *
           MOVE    SPACE               TO  LNK-SC98-ERRMSG
           STRING  "長期日数の上限は " DELIMITED  BY  SIZE
                   WRK-SURYO-H         DELIMITED  BY  SPACE
                   "日です。"          DELIMITED  BY  SIZE
                                       INTO    LNK-SC98-ERRMSG
           END-STRING
           .
       10012-CHOKI-ERR-EXT.
      *
      *****************************************************************
      *    投与量チェック処理
      *****************************************************************
       1001-TOUYO-CHK-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  FLG-TOUYO-CHK
               IF     (SPA-COM-TOUYORYO1 (IDX-Z IDX) =   ZERO ) AND
                      (SPA-COM-TOUYORYO2 (IDX-Z IDX) =   ZERO )
      *            チェックなし
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
      *                下限・上限設定なし
                       WHEN    (SPA-COM-TOUYOKGNAGE (IDX-Z IDX)
                                                   =   ZERO )  AND
                               (SPA-COM-TOUYOJGNAGE (IDX-Z IDX)
                                                   =   ZERO )
                           MOVE    1               TO  FLG-TOUYO-CHK
      *                下限のみ設定
                       WHEN    (SPA-COM-TOUYOKGNAGE (IDX-Z IDX)
                                               NOT =   ZERO )  AND
                               (SPA-COM-TOUYOJGNAGE (IDX-Z IDX)
                                                   =   ZERO )
                           PERFORM 10011-KGNAGE-CHK-SEC
                           IF      FLG-TOUYO-CHK1      =   1
                               MOVE    1               TO  FLG-TOUYO-CHK
                           END-IF
      *                上限のみ設定
                       WHEN    (SPA-COM-TOUYOKGNAGE (IDX-Z IDX)
                                                   =   ZERO )  AND
                               (SPA-COM-TOUYOJGNAGE (IDX-Z IDX)
                                               NOT =   ZERO )
                           PERFORM 10012-JGNAGE-CHK-SEC
                           IF      FLG-TOUYO-CHK2      =   1
                               MOVE    1               TO  FLG-TOUYO-CHK
                           END-IF
      *                共に設定あり
                       WHEN    (SPA-COM-TOUYOKGNAGE (IDX-Z IDX)
                                               NOT =   ZERO )  AND
                               (SPA-COM-TOUYOJGNAGE (IDX-Z IDX)
                                               NOT =   ZERO )
                           PERFORM 10011-KGNAGE-CHK-SEC
                           PERFORM 10012-JGNAGE-CHK-SEC
                           IF     (FLG-TOUYO-CHK1      =   1 ) AND
                                  (FLG-TOUYO-CHK2      =   1 )
                               MOVE    1               TO  FLG-TOUYO-CHK
                           END-IF
                   END-EVALUATE
               END-IF
      *
               IF      FLG-TOUYO-CHK       =   1
      *             投与量チェック
                   IF      SPA-COM-SRYKBN (IDX-Z)  =   "22"
      *                頓服判定
                       IF      (SPA-COM-TOUYORYO2 (IDX-Z IDX)  =   ZERO)
                           MOVE    SPA-COM-TOUYORYO1 (IDX-Z IDX)
                                                   TO  WRK-TOURYO
                       ELSE
                           MOVE    SPA-COM-TOUYORYO2 (IDX-Z IDX)
                                                   TO  WRK-TOURYO
                       END-IF
                       IF      (WRK-TOURYO         >   ZERO )  AND
                              ((SPA-COM-SURYO (IDX-Z)  *
                                SPA-COM-KAISU (IDX-Z)  )
                                                   >   WRK-TOURYO)
                           PERFORM 1002-TOUYO-ERR-SEC
                       END-IF
                   ELSE
                       IF      (SPA-COM-TOUYORYO1 (IDX-Z IDX)  >   ZERO)
                          AND  (SPA-COM-TOUYORYO1 (IDX-Z IDX)
                                                   <   SPA-COM-SURYO
                                                             (IDX-Z))
                           MOVE    SPA-COM-TOUYORYO1 (IDX-Z IDX)
                                                   TO  WRK-TOURYO
                           PERFORM 1002-TOUYO-ERR-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       1001-TOUYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    下限年齢チェック処理
      *****************************************************************
       10011-KGNAGE-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUYO-CHK1
           MOVE    SPA-COM-TOUYOKGNAGE (IDX-Z IDX)
                                       TO  WRK-NENCD
           PERFORM 10013-NENREICD-CHK-SEC
           IF      FLG-NENREI-CD       =   1
               CONTINUE
           ELSE
               MOVE    SPA-COM-TOUYOKGNAGE (IDX-Z IDX)
                                           TO  WRK-NEN-YY(2:2)
           END-IF
           IF      LNK-SC98-NENREI     <   WRK-NENREI
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-TOUYO-CHK1
           END-IF
           .
       10011-KGNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    上限年齢チェック処理
      *****************************************************************
       10012-JGNAGE-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUYO-CHK2
           MOVE    SPA-COM-TOUYOJGNAGE (IDX-Z IDX)
                                       TO  WRK-NENCD
           PERFORM 10013-NENREICD-CHK-SEC
           IF      FLG-NENREI-CD       =   1
               CONTINUE
           ELSE
               MOVE    SPA-COM-TOUYOJGNAGE (IDX-Z IDX)
                                           TO  WRK-NEN-YY(2:2)
           END-IF
           IF      LNK-SC98-NENREI     <   WRK-NENREI
               MOVE    1               TO  FLG-TOUYO-CHK2
           END-IF
           .
       10012-JGNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年齢コードチェック処理
      *****************************************************************
       10013-NENREICD-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-NENREI-CD
           MOVE    ZERO                TO  WRK-NENREI
      *
           SET     NENREI-IDX          TO  1
           SEARCH      NENREI-TBL-OCC VARYING   NENREI-IDX
               AT  END
                   MOVE    ZERO            TO  FLG-NENREI-CD
               WHEN    NENREI-NENCD(NENREI-IDX) 
                                           =   WRK-NENCD
                   MOVE    1               TO  FLG-NENREI-CD
                   MOVE    NENREI-DD (NENREI-IDX) 
                                           TO  WRK-NEN-DD
                   MOVE    NENREI-MM (NENREI-IDX) 
                                           TO  WRK-NEN-MM
           END-SEARCH
           .
       10013-NENREICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    投与量チェック処理
      *****************************************************************
       1002-TOUYO-ERR-SEC            SECTION.
      *
           IF      SPA-COM-TOUYOERR(IDX-Z) =   9
               IF      SPA-COM-SET (IDX-Z) =   "1"
                   IF      SPA-COM-KZMFLG5 (IDX-SET) =   SPACE
                       MOVE    "1"         TO  SPA-COM-KZMFLG5(IDX-SET)
                       MOVE    "1"         TO  SPA-COM-CUR (IDX-SET)
                       MOVE    "K201"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      SPA-COM-KZMFLG5 (IDX-Z) =   SPACE
                       MOVE    "1"         TO  SPA-COM-KZMFLG5 (IDX-Z)
                       MOVE    "1"         TO  SPA-COM-CUR (IDX-Z)
                       MOVE    "K201"      TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    "0201"              TO  SPA-ERRCD
               IF      SPA-COM-SET (IDX-Z) =   "1"
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-SET)
               ELSE
                   MOVE    "1"             TO  SPA-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
           MOVE    WRK-TOURYO          TO  WRK-SURYO
           PERFORM 10021-SURYO-HEN-SEC
      *
           MOVE    SPACE               TO  LNK-SC98-ERRMSG
           IF      SPA-COM-SRYKBN (IDX-Z)  =   "22"
               STRING  "頓服の投与量の上限数量は "
                                               DELIMITED  BY  SIZE
                       WRK-SURYO-H             DELIMITED  BY  SPACE
                       " です。"               DELIMITED  BY  SIZE
                                           INTO    LNK-SC98-ERRMSG
               END-STRING
           ELSE
               STRING  "投与量の上限数量は "   DELIMITED  BY  SIZE
                       WRK-SURYO-H             DELIMITED  BY  SPACE
                       " です。"               DELIMITED  BY  SIZE
                                           INTO    LNK-SC98-ERRMSG
               END-STRING
           END-IF
           .
       1002-TOUYO-ERR-EXT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       10021-SURYO-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               IF      WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
           MOVE    ZERO                TO  IDM2
           MOVE    SPACE               TO  WRK-SURYO-H
           PERFORM VARYING     IDM     FROM    1  BY  1
                   UNTIL       IDM     >   11
              IF       WRK-SURYO-X (IDM:1) NOT =   SPACE
                   MOVE    WRK-SURYO-X(IDM:)   TO  WRK-SURYO-H(1:)
                   MOVE    11                  TO  IDM
              END-IF
           END-PERFORM
           .
       2001-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤チェック処理
      *****************************************************************
       300-PTKINKI-CHK-SEC              SECTION.
      *
           MOVE    SPA-COM-PTKINKI-ARI (IDX-Z) TO  FLG-KINKI-ARI
      *
           IF    ((SPA-NYUGAIKBN       =   "1"   )  OR
                  (LNK-SC98-PG         =   "KT01" ))
            AND   (SPA-COM-KINKSTYMD (IDX-Z)   NOT =   SPACE )
               PERFORM 3001-KINKSTYMD-CHK-SEC
               MOVE    FLG-KINKI-ARI       TO  SPA-COM-PTKINKI-ARI
                                                        (IDX-Z)
           END-IF
      *
           IF     FLG-KINKI-ARI        =   1
               IF      SPA-COM-SET (IDX-Z)     =   "1"
                   IF      SPA-COM-KINKIFLG (IDX-SET)  =   SPACE
                       MOVE    "K210"      TO  SPA-ERRCD
                       IF      SPA-COM-PTKINKICD (IDX-Z) =   1
                           MOVE    "K213"          TO  SPA-ERRCD
                       END-IF
                       PERFORM 3001-KINKIERR-HEN-SEC
                       MOVE    "1"         TO  SPA-COM-KINKIFLG
                                                            (IDX-SET)
                       MOVE    "1"         TO  SPA-COM-CUR  (IDX-SET)
                       MOVE    1           TO  SPA-COM-PTKINKI-ARI
                                                            (IDX-SET)
                   END-IF
               ELSE
                   IF      SPA-COM-KINKIFLG (IDX-Z)  =   SPACE
                       MOVE    "K211"          TO  SPA-ERRCD
                       IF      SPA-COM-PTKINKICD (IDX-Z) =   1
                           MOVE    "K212"          TO  SPA-ERRCD
                       END-IF
                       PERFORM 3001-KINKIERR-HEN-SEC
                       MOVE    "1"             TO  SPA-COM-KINKIFLG
                                                               (IDX-Z)
                       MOVE    "1"             TO  SPA-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
           .
       300-PTKINKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤チェック処理
      *****************************************************************
       3001-KINKSTYMD-CHK-SEC              SECTION.
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           MOVE    ZERO                TO  FLG-KINKI-ARI
      *
           PERFORM VARYING IDX-N2      FROM    1   BY  1
                   UNTIL  (IDX-N2      >   31  )
               IF      SPA-COM-NYUINDAY(IDX-Z IDX-N2)  >   ZERO
                   MOVE    IDX-N2              TO  WRK-SRYDD
                   IF      SPA-COM-KINKSTYMD (IDX-Z)
                                               <=  WRK-SRYYMD
                       MOVE    1                   TO  FLG-KINKI-ARI
                       MOVE    31                  TO  IDX-N2
                   END-IF
               END-IF
           END-PERFORM
           .
       3001-KINKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤エラーメッセージ編集
      *****************************************************************
       3001-KINKIERR-HEN-SEC              SECTION.
      *
           IF      SPA-COM-TOUYOYMD (IDX-Z)    NOT =   SPACE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    SPA-COM-TOUYOYMD (IDX-Z)
                                           TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "投与開始日："         DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3       DELIMITED  BY  SPACE
                       "　"                   DELIMITED  BY  SIZE
                                           INTO    LNK-SC98-ERRMSG
               END-STRING
           END-IF
           IF      SPA-COM-KINKSTYMD(IDX-Z)    NOT =   SPACE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    SPA-COM-KINKSTYMD (IDX-Z)
                                           TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-SC98-ERRMSG     TO  WRK-ERRMSG
               MOVE    SPACE               TO  LNK-SC98-ERRMSG
      *
               STRING  WRK-ERRMSG             DELIMITED  BY  SPACE
                       "禁忌開始日："         DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3       DELIMITED  BY  SPACE
                                           INTO    LNK-SC98-ERRMSG
               END-STRING
           END-IF
           .
       3001-KINKIERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月上限回数・月総投与量チェック処理
      *****************************************************************
       200-TUKI-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LNK-SC98-ERRMSG
           MOVE    ZERO                TO  IDX-SET
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                             ((SPA-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Z) =   SPACE ))
                           OR (SPA-ERRCD   NOT =   SPACE   )
      *        セットの行位置
               IF      SPA-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    IDX-Z               TO  IDX-SET
               END-IF
      *        月投与量・回数チェック
               IF     (SPA-ERRCD               =   SPACE )
               AND    (SPA-COM-INPUTCD (IDX-Z) (1:1)
                                               =   "6" )
               AND    (SPA-COM-YAKUHYO (IDX-Z) =   SPACE )
                   IF     (SPA-COM-TUKIJGNKAISU (IDX-Z)
                                                   >   ZERO )  OR
                          (SPA-COM-TUKIJGNTOUYORYO (IDX-Z)
                                                   >   ZERO )
                       PERFORM 400-TUJIJGEN-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       200-TUKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    月上限回数・月総投与量チェック処理
      *****************************************************************
       400-TUJIJGEN-CHK-SEC             SECTION.
      *
           MOVE    SPA-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
      *    月合計テーブル検索
           PERFORM 4001-YAKU-TBL-CHK-SEC
      *    今回入力分集計
           PERFORM 4003-TUJI-SUM-SEC
      *    セットの時、セット行にカーソル移動
           IF      SPA-COM-SET (IDX-Z) =   "1"
               MOVE    IDX-SET             TO  IDX-C
           ELSE
               MOVE    IDX-Z               TO  IDX-C
           END-IF
      *    月総投与量チェック
           IF     (SPA-ERRCD                     =   SPACE)  AND
                  (SPA-COM-TUKIJGNTOUYORYO (IDX-Z)  >   ZERO )  AND
                  (SPA-COM-TUKIJGNTOUYORYO (IDX-Z)  <   WRK-TUKISURYO)
               IF      (SPA-COM-TUKIJGNFLG (IDX-C)  =   SPACE )
                   PERFORM 4003-TUKISYORO-ERR-SEC
               END-IF
           END-IF
      *    月上限回数チェック
           IF     (SPA-ERRCD                     =   SPACE)
           AND    (SPA-COM-TUKIJGNKAISU (IDX-Z)  >   ZERO )
           AND    (SPA-COM-TUKIJGNKAISU (IDX-Z)  <   WRK-TUKIKAISU )
      *        回数(投薬・注射のみ）
           AND    (SPA-COM-SRYKBN      (IDX-Z) =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "31"
                                               OR  "32"
                                               OR  "33"          )
               IF     (SPA-COM-TUKIJGNFLG1(IDX-C)  =   SPACE )
                   PERFORM 4003-TUKIKAISU-ERR-SEC
               END-IF
           END-IF
           .
       400-TUJIJGEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月総投与量エラー編集処理
      *****************************************************************
       4003-TUKISYORO-ERR-SEC             SECTION.
      *
           MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG(IDX-C)
           MOVE    "1"                 TO  SPA-COM-CUR      (IDX-C)
           MOVE    "K228"              TO  SPA-ERRCD
      *
           MOVE    SPA-COM-TUKIJGNTOUYORYO (IDX-Z)
                                       TO  WRK-SURYO
           PERFORM 10021-SURYO-HEN-SEC
           MOVE    WRK-SURYO-H         TO  WRK-SOURYO-H1
           MOVE    WRK-TUKISURYO       TO  WRK-SURYO
           PERFORM 10021-SURYO-HEN-SEC
           MOVE    WRK-SURYO-H         TO  WRK-SOURYO-H2
           MOVE    SPACE               TO  LNK-SC98-ERRMSG
           STRING  "月総投与量："          DELIMITED  BY  SIZE
                   WRK-SOURYO-H1           DELIMITED  BY  SPACE
                   "　累計総投与量："      DELIMITED  BY  SIZE
                   WRK-SOURYO-H2           DELIMITED  BY  SPACE
                                       INTO    LNK-SC98-ERRMSG
           END-STRING
           .
       4003-TUKISYORO-ERR-EXT.
           EXIT.
      *****************************************************************
      *    月上限回数エラー編集処理
      *****************************************************************
       4003-TUKIKAISU-ERR-SEC             SECTION.
      *
           MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG1 (IDX-C)
           MOVE    "1"                 TO  SPA-COM-CUR         (IDX-C)
           MOVE    "K227"              TO  SPA-ERRCD
           MOVE    SPA-COM-TUKIJGNKAISU (IDX-Z)
                                       TO  WRK-KAISU-H
           MOVE    WRK-TUKIKAISU       TO  WRK-KAISU-H2
           MOVE    SPACE               TO  LNK-SC98-ERRMSG
           STRING  "月上限回数："          DELIMITED  BY  SIZE
                   WRK-KAISU-H             DELIMITED  BY  SIZE
                   "　累計回数："          DELIMITED  BY  SIZE
                   WRK-KAISU-H2            DELIMITED  BY  SIZE
                                       INTO    LNK-SC98-ERRMSG
           END-STRING
           .
       4003-TUKIKAISU-ERR-EXT.
           EXIT.
      *****************************************************************
      *    月上限回数・月総投与量テーブルチェック処理
      *****************************************************************
       4001-YAKU-TBL-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKU-TBL
           MOVE    ZERO                TO  IDY-TBL
           MOVE    ZERO                TO  WRK-TUKIKAISU
           MOVE    ZERO                TO  WRK-TUKISURYO
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   100 )
                         OR   (IDY-TBL     >   ZERO )
               IF     (LNK-SC98-YAKUZAI-SRYCD (IDY)
                                           =   WRK-SRYCD  )
               OR     (LNK-SC98-YAKUZAI-SRYCD (IDY)
                                           =   SPACE     )
                   MOVE    IDY             TO  IDY-TBL
               END-IF
           END-PERFORM
           IF      IDY-TBL             >   ZERO
               IF      LNK-SC98-YAKUZAI-CHKFLG (IDY-TBL)   =   ZERO
      *            診療行為テーブルからの集計
                   PERFORM 4002-SRYACT-HEN-SEC
                   MOVE    1               TO  LNK-SC98-YAKUZAI-CHKFLG
                                                             (IDY-TBL)
               END-IF
               COMPUTE WRK-TUKISURYO       =   LNK-SC98-YAKUZAI-SURYO
                                                             (IDY-TBL)
                                           +   LNK-SC98-YAKUTEI-SURYO
                                                              (IDY-TBL)
               COMPUTE WRK-TUKIKAISU       =   LNK-SC98-YAKUZAI-KAISU
                                                               (IDY-TBL)
                                           +   LNK-SC98-YAKUTEI-KAISU
                                                               (IDY-TBL)
           END-IF
           .
       4001-YAKU-TBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回入力分集計処理
      *****************************************************************
       4003-TUJI-SUM-SEC             SECTION.
      *
           PERFORM VARYING     IDX-Y   FROM    1   BY  1
                   UNTIL      (IDX-Y       >   SPA-MAX-LINE     )   OR
                              (IDX-Y       >   IDX-Z            )   OR
                             ((SPA-GMN-INPUTCD (IDX-Y) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-Y) =   SPACE ))
                           OR (SPA-ERRCD   NOT =   SPACE   )
               IF      (SPA-COM-YAKUHYO (IDX-Y)    =   SPACE  )
               AND     (SPA-COM-INPUTCD (IDX-Y)    =   WRK-SRYCD  )
                   IF     (SPA-NYUGAIKBN       =   "1" )
                   OR     (LNK-SC98-PG         =   "KT01" )
                        COMPUTE WRK-KAISU      =   SPA-COM-NYU-KAI
                                                               (IDX-Y)
                                               *   SPA-COM-KAISU(IDX-Y)
                   ELSE
                       MOVE    SPA-COM-KAISU (IDX-Y)   TO  WRK-KAISU
                   END-IF
      *            総数量
                   COMPUTE WRK-TUKISURYO   =   WRK-TUKISURYO
                                           +  (SPA-COM-SURYO (IDX-Y)
                                           *   WRK-KAISU        )
      *            回数(投薬・注射のみ）
                   IF      SPA-COM-SRYKBN  (IDX-Y) =   "21"
                                                   OR  "22"
                                                   OR  "23"
                                                   OR  "31"
                                                   OR  "32"
                                                   OR  "33"
                       COMPUTE WRK-TUKIKAISU   =   WRK-TUKIKAISU
                                               +   WRK-KAISU
                   END-IF
               END-IF
           END-PERFORM
           .
       4003-TUJI-SUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    月上限回数・月総投与量データ集計処理
      *****************************************************************
       4002-SRYACT-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  WRK-TUKIKAISU
           MOVE    ZERO                TO  WRK-TUKISURYO
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
               IF      SRY-RENNUM          =   1
      *            診療会計検索
                   PERFORM 40021-SRYACCT-SEL-SEC
                   IF      ACCT-ZAIKBN         =   1
                       MOVE    ZERO                TO  ACCT-ZAIKAISU
                   END-IF
      *            投薬・注射のみ回数集計
                   IF      SRY-SRYKBN          =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "31"
                                               OR  "32"
                                               OR  "33"
                       ADD     ACCT-ZAIKAISU       TO  WRK-TUKIKAISU
                   END-IF
               END-IF
               PERFORM VARYING     IDY2    FROM    1   BY  1
                       UNTIL      (IDY2        >   5   )   OR
                                  (SRY-SRYCD (IDY2)    =   SPACE )
                               OR (ACCT-ZAIKAISU   =   ZERO  )
                   IF      SRY-SRYCD (IDY2)    =   WRK-SRYCD
                       COMPUTE WRK-TUKISURYO   =   WRK-TUKISURYO
                                               +  (SRY-SRYSURYO (IDY2)
                                               *   ACCT-ZAIKAISU  )
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-SRYCD       TO  LNK-SC98-YAKUZAI-SRYCD
                                                             (IDY-TBL)
           MOVE    WRK-TUKISURYO   TO  LNK-SC98-YAKUZAI-SURYO
                                                              (IDY-TBL)
           MOVE    WRK-TUKIKAISU   TO  LNK-SC98-YAKUZAI-KAISU
                                                              (IDY-TBL)
           .
       4002-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ検索処理 
      *****************************************************************
       40021-SRYACCT-SEL-SEC           SECTION.
      *
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
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
                   MOVE    ZERO                TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40021-SRYACCT-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
