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
       PROGRAM-ID.                 ORCSC93.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為の剤分割・編集・計算処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/06/14    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/08/23  注射に外用薬を入力可能とする
      * 01.00.02     NACL-多々納  02/09/12  自賠責の追加（09591〜09592)
      * 01.00.03     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.04     NACL-多々納  02/10/18  入院追加
      * 01.00.05     NACL-多々納  02/11/11  ＤＵＭＭＹ診察料追加
      * 01.00.06     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.07     NACL-多々納  02/11/22  画像診断の点滴追加（731)
      * 01.00.08     NACL-多々納  02/12/17  在宅指導料の追加（143)
      * 01.00.09     NACL-多々納  02/12/20  自費修正時の修正
      * 01.00.10     NACL-多々納  03/01/30  入院時検査まとめをやめる
      * 01.00.11     NACL-多々納  03/02/12  自費の剤分離の修正
      * 01.00.12     NACL-多々納  03/02/21  入院料の加算のみ修正
      * 01.00.13     NACL-多々納  03/05/09  剤明細を３０までとする
      * 01.00.14     NACL-多々納  03/09/12  検査包括なし追加（610）
      * 01.00.15     NACL-多々納  03/10/28  検査包括修正
      * 01.00.16     NACL-多々納  04/01/23  セット内の検査包括対象外
      * 01.00.17     NACL-多々納  04/03/17  Ｈ１６．４改正対応
      * 01.00.18     NACL-多々納  04/04/27  ユーザーコード追加
      * 01.00.19     NACL-多々納  04/07/26  複数科・保険追加
      *                                     処置加算料追加(403)
      * 01.00.20     NACL-多々納  05/01/12  初診時ブラッシング料対応
      * 01.00.20     NACL-多々納  05/01/13  歯科薬剤の外用薬対応
      * 01.00.21     NACL-多々納  05/06/03  検体検査コメント対応
      * 01.00.22     NACL-多々納  05/08/12  包括検査 ０９，１０の追加
      * 01.00.23     NACL-多々納  06/01/23  用量割合コード追加
      * 01.00.24     NACL-多々納  06/03/XX  Ｈ１８．４改正対応
      * 03.04.00     NACL-多々納  06/12/19  外用薬の内服算定追加
      * 04.01.00     NACL-多々納  07/10/XX  悪性腫瘍検査自動設定対応
      * 04.03.00     NACL-多々納  08/07/01  検査加算種別対応他
      * 04.04.00     NACL-多々納  08/12/21  出来高算定コード対応
      * 04.04.00     NACL-多々納  09/01/09  包括検査コメント編集追加
      * 04.04.00     NACL-多々納  09/02/12  外来まとめ対応
      * 04.05.00     NACL-多々納  10/01/XX  治験対応
      * 04.06.00     NACL-多々納  10/12/01  自賠責金額入力対応
      * 04.07.00     NACL-多々納  12/08/28  薬剤料減点対応
      * 04.07.00     NACL-多々納  13/07/XX  第三者行為対応
      * 04.07.00     NACL-多々納  14/06/XX  外用薬（臨時投薬）対応
      * 04.07.00     NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      * 04.07.00     NACL-多々納  15/03/XX  Ｈ２７年４月改正対応他
      * 04.08.00     NACL-多々納  16/03/XX  Ｈ２８年４月改正対応他
      * 04.08.00     NACL-多々納  16/08/XX  持参薬予約コード対応
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０２年４月改正対応他
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
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-BUIKBN          PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *    剤チェック対象外
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-JIHI            PIC 9(01).
      *    手技料フラグ
           03  FLG-SYUGI           PIC 9(01).
      *H28.4
           03  FLG-KSNARI          PIC 9(01).
      *    １剤フラグ
           03  FLG-ZAI             PIC 9(01).
      *    発生日フラグ
           03  FLG-HASSYO          PIC 9(01).
      *    処方せん料フラグ
           03  FLG-SYOHO           PIC 9(01).
      *
           03  FLG-ZEN             PIC 9(01).
           03  FLG-HOUKATU         PIC 9(01).
      *@@
      *    悪性腫瘍指導料あり
           03  FLG-AKUSEI-ARI      PIC 9(01).
      *    コメント以外あり
           03  FLG-CONTCHK         PIC 9(01).
      *    薬評・器評
           03  FLG-YAKUHYO         PIC 9(01).
      *    減点分
           03  FLG-GENTEN          PIC 9(01).
      *H28.9 持参薬予約コード
           03  FLG-JISANYAKU       PIC 9(01).
      *
       01  FLG-AREA2.
           03  FLG-KENSA-CHK       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-SET             PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-2               PIC 9(04).
           03  IDX-HZN             PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  ID2                 PIC 9(02).
      *
           03  IDX-S               PIC 9(04).
           03  IDX-4               PIC 9(04).
           03  IDX-5               PIC 9(04).
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYKBN-1        PIC X(02).
      *
           03  WRK-ZAINUM          PIC 9(04).
           03  WRK-H-SRYSYUKBN     PIC X(03).
           03  WRK-H-SRYKBN        PIC X(02).
      *    検査回数
           03  WRK-KENSA-CNT       PIC 9(04).
           03  WRK-KENSA-MAX       PIC 9(04).
           03  WRK-CHK-MAX         PIC 9(04).
           03  IDX-KEN             PIC 9(04).
           03  IDX-KEN2            PIC 9(04).
           03  FLG-KENSA           PIC 9(01).
           03  FLG-KENSA-ARI       PIC 9(01).
           03  FLG-KEN-ERR         PIC 9(01).
           03  TBL-HOUKNARI-TBL.
               05  TBL-HOUKNARI    PIC 9(01)   OCCURS 20.
           03  FLG-KENSA-OK        PIC 9(01).
      *
           03  WRK-HOUKNSKBN       PIC X(02).
           03  WRK-HOU-IDX         PIC 9(04).
      *
           03  WRK-INPUTCD         PIC X(22).
      *
           03  WRK-ZAI-CNT         PIC 9(04).
      *    小児科外来診察料加算判定
           03  WRK-SYONIKA-KBN         PIC X(01).
      *
           03  WRK-ERRCD               PIC X(04).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DRCD                PIC X(04).
           03  WRK-CHIKENKBN           PIC X(01).
      *    入院回数
           03  WRK-HZN-INPUTCD-G.
               05  WRK-HZN-INPUTCD     PIC X(22)  OCCURS   10.
           03  IDX-TBL2                PIC 9(03).
      *
      *    小児科外来診察料テーブル
           COPY   "CMSYONIKATBL.INC".
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *H24.5
       01  CONS-AREA.
      *    時間外対応加算２（地域医療貢献加算２）
           03  TBL-CHISRYCD2       PIC X(09)   VALUE   "112015670".
      *    時間外対応加算１
           03  TBL-CHISRYCD1       PIC X(09)   VALUE   "112016070".
      *    時間外対応加算３
           03  TBL-CHISRYCD3       PIC X(09)   VALUE   "112016170".
      *
      *H28.9
      *持参薬予約コード
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-SCR-REC
           SPA-AREA
           ORCSC93AREA
           MSYU-DATA-REC
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
           EVALUATE    LNK-SC93-DELSYORIFLG
               WHEN    1
      *            同一検査削除
                   PERFORM 300-KENSA-DELETE-SEC
           END-EVALUATE
      *    クリア処理
           PERFORM 100-INTI-SYORI-SEC
      *    剤分離処理
           PERFORM 100-ZAI-SET-SEC
      *
           .
       000-PROC-EXT.
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    クリア離処理
      *****************************************************************
       100-INTI-SYORI-SEC              SECTION.
      *
      *    診療行為、計計算
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX)
      *        計算チェック済フラグ
               MOVE    ZERO            TO  SPA-COM-SANFLG (IDX)
      *        カーソルのクリア
               MOVE    SPACE           TO  SPA-COM-CUR    (IDX)
      *
               IF      SPA-COM-SRYKBN (IDX)    =   "23"
                   PERFORM 1003-SAIHENSYU-SEC
               END-IF
      *        診療区分、種別のクリア
               IF      SPA-COM-INPUTCD(IDX)(1:1)   NOT =   "1" 
                   MOVE    SPACE           TO  SPA-COM-SRYKBN    (IDX)
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN (IDX)
               ELSE
                   IF    ( SPA-COM-SRYSYUKBN (IDX)
                                     NOT =   SPA-COM-TNSSRYSYUKBN(IDX))
                     AND ( SPA-COM-SRYSYUKBN (IDX)
                                     NOT =   "700" )
                       MOVE    SPACE       TO  SPA-COM-SRYKBN(IDX)
                       MOVE    SPACE       TO  SPA-COM-SRYSYUKBN (IDX)
                   END-IF
               END-IF
      *        検体検査コメントの判定
               IF      SPA-COM-KENTAICOMMENT (IDX) =   1
                   MOVE    ZERO            TO  SPA-COM-HOUKNSKBN(IDX)
               END-IF
      *        同一検査判定コード
               MOVE    SPACE               TO  SPA-COM-KENGRP (IDX)
      *        薬評クリア
               MOVE    SPACE               TO  SPA-COM-YAKUHYO (IDX)
               MOVE    SPACE               TO  SPA-COM-GENTEN  (IDX)
               MOVE    SPACE               TO  SPA-COM-JISANYAKU(IDX)
      *
               IF      (SPA-GMN-INPUTCD(IDX) =   SPACE )   AND
                       (SPA-COM-INPUTCD(IDX) =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
           .
       100-INTI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤分離処理
      *****************************************************************
       100-ZAI-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-KENSA-ARI
      *H27.10
           MOVE    ZERO                TO  FLG-KENSA-CHK
      *    剤分離
           PERFORM 1002-ZAIBUNNRI-SEC
           IF     (SPA-ERRCD           =   SPACE ) 
             AND  (LNK-SC93-DELSYORIFLG    =   ZERO)
             AND  (LNK-SC93-ERRCD          =   ZERO)
      *    検査の包括があった時、まとめる
      *    入院で診療行為入力はしない
               IF    ((SPA-NYUGAIKBN       =   "1" )  AND
                      (LNK-SC93-PG         =   "K02N") )
               OR     (LNK-SC93-PG         =   "KT01")
                  IF      FLG-KENSA-ARI       =   1
                       IF      LNK-SC93-KENSAARIFLG    =   1
                           PERFORM 200-KENSA-SORT-SEC
                       END-IF
      *H27.10
                       MOVE    1               TO  FLG-KENSA-CHK
      *                剤分離
                       PERFORM 1002-ZAIBUNNRI-SEC
                   END-IF
               ELSE
                   IF      FLG-KENSA-ARI       =   1
                       PERFORM 200-KENSA-SORT-SEC
      *H27.10
                       MOVE    1               TO  FLG-KENSA-CHK
      *                剤分離
                       PERFORM 1002-ZAIBUNNRI-SEC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  WRK-ZAI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-MAX-LINE )  OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE ))
      *
               MOVE    SPACE               TO  SPA-GMN-SRYKBN (IDX)
      *        剤毎の同一項目再編集
               IF     (IDX                         =   1   )  OR
                      (SPA-GMN-INPUTCD(IDX)(1:1)   =   "." )  OR
                      (SPA-COM-ZAIEND (IDX - 1)    =   "1" )
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  SPA-GMN-SRYKBN
                                                                (IDX)
                   MOVE    SPA-COM-SRYSYUKBN (IDX) TO  WRK-H-SRYSYUKBN
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-H-SRYKBN
                   ADD     1                       TO  WRK-ZAINUM
                   MOVE    ZERO                    TO  WRK-ZAI-CNT
                   MOVE    ZERO                    TO  FLG-CONTCHK
               END-IF
      *        剤回数チェック
               EVALUATE    LNK-SC93-PG
                   WHEN    "J04"
                   WHEN    "K05"
                       PERFORM 1002-ZAIKAI-CHK-SEC
               END-EVALUATE
               MOVE    WRK-H-SRYSYUKBN     TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    WRK-H-SRYKBN        TO  SPA-COM-SRYKBN (IDX)
               MOVE    WRK-ZAINUM          TO  SPA-COM-ZAINUM (IDX)
      *        診療種別は剤明細としない
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                                                   OR  "*"
                                                   OR  "#"
                                                   OR  "$"
                   CONTINUE
               ELSE
                   ADD     1                   TO  WRK-ZAI-CNT
      *            コメント以外
                   IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"
                                                       OR  "6"
                                                       OR  "7" )  OR
                          (SPA-COM-INPUTCD(IDX)(1:3)   =   "059")
                       MOVE    1                   TO  FLG-CONTCHK
                   END-IF
               END-IF
               IF      WRK-ZAI-CNT         >   50
                   MOVE    "7006"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR(IDX)
               END-IF
      *!!
      *        継続コメントのチェック
               IF      SPA-COM-INPUTCONT (IDX) =   "1"
                   IF      FLG-CONTCHK         =   ZERO
                       MOVE    "0208"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR(IDX)
                   END-IF
      *            入院料・食事加算はエラー
                   IF     (SPA-ERRCD           =   SPACE )
                   AND    (WRK-H-SRYKBN        =   "90"
                                               OR  "92"
                                               OR  "97"  )
                       MOVE    "0209"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR(IDX)
                   END-IF
               END-IF
      *        内服１種類入力のチェック
               IF      SPA-COM-INPUTNAIF (IDX) =   "1"
      *            内服以外はエラー
                   IF     (SPA-ERRCD           =   SPACE )
                   AND    (WRK-H-SRYKBN    NOT =   "21"  )
                       MOVE    "0203"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR(IDX)
                   END-IF
               END-IF
      *!!
      *!!!  H24.4 削除
      *    医療観察法専用のコード判定
      *        IF      SPA-ERRCD           =   SPACE
      *            IF     (SPA-COM-INPUTCD (IDX)(1:3)  =   "188"  )
      *            AND    (SPA-COM-KZMFLG  (IDX)   =   SPACE      )
      *                MOVE    "K269"          TO  SPA-ERRCD
      *                MOVE    "1"             TO  SPA-COM-KZMFLG(IDX)
      *                MOVE    "1"             TO  SPA-COM-CUR(IDX)
      *            END-IF
      *        END-IF
      *!!!
           END-PERFORM
      *    剤内をすべて同じ区分
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  IDX-S
           MOVE    ZERO                TO  FLG-HOUKATU
           MOVE    ZERO                TO  FLG-YAKUHYO
           MOVE    ZERO                TO  FLG-GENTEN
      *H28.9 持参薬予約コード
           MOVE    ZERO                TO  FLG-JISANYAKU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE ))
               IF     (SPA-COM-HOUKATU-FLG (IDX)   =   1  )
                   MOVE    1                   TO  FLG-HOUKATU
               END-IF
               IF     (SPA-COM-GENTEN (IDX)    =   "1"  )
                   MOVE    1                   TO  FLG-GENTEN
               END-IF
               IF     (SPA-COM-YAKUHYO(IDX)    =   "1"  )
                   MOVE    1                   TO  FLG-YAKUHYO
               END-IF
               IF     (SPA-COM-JISANYAKU(IDX)   =   "1"  )
                   MOVE    1                   TO  FLG-JISANYAKU
               END-IF
               IF     (SPA-COM-JISANYAKU(IDX)   =   "2"  )
                   MOVE    2                   TO  FLG-JISANYAKU
               END-IF
               IF     (SPA-COM-ZAIEND (IDX)    =   "1"   )
                   PERFORM VARYING     IDX-Z   FROM    IDX-S  BY  1
                           UNTIL      (IDX-Z       >   IDX    )
                       MOVE    FLG-HOUKATU     TO  SPA-COM-HOUKATU-ZAI
                                                              (IDX-Z)
                       IF      FLG-GENTEN      =   1
                           MOVE    "1"         TO  SPA-COM-GENTEN (IDX)
                       ELSE
                           MOVE    SPACE       TO  SPA-COM-GENTEN (IDX)
                       END-IF
                       IF      FLG-YAKUHYO     =   1
                           MOVE    "1"         TO  SPA-COM-YAKUHYO (IDX)
                       ELSE
                           MOVE    SPACE       TO  SPA-COM-YAKUHYO (IDX)
                       END-IF
      *H28.9 持参薬予約コード
                       MOVE    SPACE           TO  SPA-COM-JISANYAKU
                                                               (IDX-Z)
                       IF      FLG-JISANYAKU   =   1
                           MOVE    "1"         TO  SPA-COM-JISANYAKU
                                                              (IDX-Z)
      *                    包括扱い
                           MOVE    1           TO  SPA-COM-HOUKATU-ZAI
                                                              (IDX-Z)
                       END-IF
                       IF      FLG-JISANYAKU   =   2
                           MOVE    "2"         TO  SPA-COM-JISANYAKU
                                                              (IDX-Z)
      *                    包括扱い
                           MOVE    1           TO  SPA-COM-HOUKATU-ZAI
                                                              (IDX-Z)
                       END-IF
                   END-PERFORM
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    ZERO                TO  FLG-HOUKATU
                   MOVE    ZERO                TO  FLG-YAKUHYO
                   MOVE    ZERO                TO  FLG-GENTEN
                   MOVE    ZERO                TO  FLG-JISANYAKU
               END-IF
           END-PERFORM
           .
      *
       100-ZAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    剤分離処理
      *****************************************************************
       1002-ZAIBUNNRI-SEC              SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    クリア
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-SYUGI
      *H28.4
           MOVE    ZERO                TO  FLG-KSNARI
      *!
           MOVE    SPACE               TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-CHIKENKBN
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    SPACE               TO  WRK-DRCD
      *    外来
           IF     (SPA-NYUGAIKBN       =   "2"  )  AND
                  (LNK-SC93-PG         =   "K02")
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
               MOVE    SPA-CHIKENKBN       TO  WRK-CHIKENKBN
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               MOVE    SPA-DRCD(2:4)       TO  WRK-DRCD
           END-IF
      *    入院
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (LNK-SC93-PG         =   "K02N")
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               MOVE    SPA-CHIKENKBN       TO  WRK-CHIKENKBN
           END-IF
      *    外来まとめ
           IF     (LNK-SC93-PG         =   "KT01")
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               MOVE    SPA-DRCD(2:4)       TO  WRK-DRCD
               MOVE    SPA-CHIKENKBN       TO  WRK-CHIKENKBN
           END-IF
      *!
      *    診療種別区分編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   SPA-MAX-LINE)   OR
                              (SPA-ERRCD        NOT =   SPACE )   OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE ))
      *        入院回数のチェック
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
      *            入院以外エラー
                   IF      LNK-SC93-PG         =   "K02N"
                                               OR  "KT01"
                       IF     (IDX             =   1  )  OR
                              (SPA-GMN-INPUTCD(IDX - 1)  =   ".")
                           MOVE    "9005"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                   ELSE
                       MOVE    "9004"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR(IDX)
                   END-IF
               END-IF
      *        保険・診療科入力
               IF     (SPA-NYUGAIKBN       =   "2"  )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)   =   "#"
                                                   OR  "$")
                   IF      LNK-SC93-PG         =   "K02"
      *!               １行目はエラー
                       IF      IDX             =   1
                           MOVE    "1056"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                   ELSE
                       MOVE    "1055"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR(IDX)
                   END-IF
      *H28.1
                   IF      SPA-ERRCD       NOT =   SPACE
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    SPA-ERRCD       TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-IF
               IF      SPA-ERRCD           =   SPACE
      *            剤分離
                   PERFORM 5101-SYRKBN-SET-SEC
      *!!!!!!!!!!!!!
                   IF      SPA-ERRCD       NOT =   SPACE
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    SPA-ERRCD       TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-IF
      *
      *        各処理のチェック
               IF      SPA-ERRCD           =   SPACE
                   EVALUATE    LNK-SC93-PG
                       WHEN    "K05"
                           PERFORM 1001-K05-SET-CHK-SEC
                       WHEN    "J04"
                           IF     (IDX             >   1 )  AND
                                  (SPA-COM-SRYKBN (IDX - 1)
                                           NOT =   SPA-COM-SRYKBN(IDX))
                               MOVE    "1020"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                 (IDX)
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
           END-IF
           .
      *
       1002-ZAIBUNNRI-EXT.
           EXIT.
      *****************************************************************
      *    セット入力時、チェック処理
      *****************************************************************
       1001-K05-SET-CHK-SEC              SECTION.
      *
      *    セットコードが約束の時、セットは入力不可とする
           IF      LNK-SC93-SETCODE(1:1)   =   "S"
               IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" OR "P" ) AND
                      (SPA-GMN-INPUTCD(IDX)(2:5)   NUMERIC        )
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "5009"              TO  SPA-ERRCD
                   END-IF
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
      *        診療種別は入力しない
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "5011"              TO  SPA-ERRCD
                   END-IF
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
           END-IF
      *
           .
       1001-K05-SET-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数チェック処理
      *****************************************************************
       1002-ZAIKAI-CHK-SEC              SECTION.
      *
           EVALUATE    LNK-SC93-PG
               WHEN    "J04"
      *            剤修正の時、１剤のみとする
                   IF      WRK-ZAINUM          >   1
                       MOVE    "1023"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               WHEN    "K05"
      *            約束セットの時、剤は１剤のみとする
                   IF     (WRK-ZAINUM              >   1  )  AND
                          (LNK-SC93-SETCODE(1:1)   =   "S")
                       MOVE    "5010"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  SPA-COM-CUR(IDX)
                   END-IF
           END-EVALUATE
           .
       1002-ZAIKAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分編集決定処理
      *****************************************************************
       5101-SYRKBN-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAI
      *
           MOVE    WRK-HKNCOMBI        TO  SPA-COM-HKNCOMBI (IDX)
           MOVE    WRK-SRYKA           TO  SPA-COM-SRYKA    (IDX)
           MOVE    WRK-DRCD            TO  SPA-COM-DRCD     (IDX)
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "#"
               CONTINUE
           ELSE
               MOVE    WRK-CHIKENKBN       TO  SPA-COM-CHIKENKBN(IDX)
           END-IF
      *
           EVALUATE    SPA-GMN-INPUTCD(IDX)(1:1)
               WHEN    "#"
      *            保険入力時、剤の終了とする
                   MOVE    SPA-GMN-INPUTCD(IDX)(2:4)   TO  WRK-HKNCOMBI
                   MOVE    SPA-COM-CHIKENKBN(IDX)  TO  WRK-CHIKENKBN
                   MOVE    "1"                 TO  SPA-COM-ZAIEND (IDX)
                   IF      IDX                 >   1
                       MOVE    "1"                 TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                   END-IF
                   MOVE    ZERO                TO  FLG-JIHI
                   MOVE    ZERO                TO  FLG-BUIKBN
                   MOVE    ZERO                TO  FLG-SYUGI
      *H28.4
                   MOVE    ZERO            TO  FLG-KSNARI
                   MOVE    ZERO                TO  FLG-HASSYO
                   MOVE    ZERO                TO  FLG-AKUSEI-ARI
                   MOVE    ZERO                TO  FLG-YAKUHYO
                   MOVE    ZERO                TO  FLG-GENTEN
                   MOVE    ZERO                TO  WRK-KENSA-CNT
                   MOVE    SPACE               TO  WRK-SRYKBN
                   MOVE    SPACE               TO  WRK-SRYSYUKBN
                   MOVE    SPACE               TO  WRK-HOUKNSKBN
                   MOVE    ZERO                TO  WRK-HOU-IDX
                   MOVE    WRK-HKNCOMBI        TO  SPA-COM-HKNCOMBI
                                                              (IDX)
                   MOVE    WRK-CHIKENKBN       TO  SPA-COM-CHIKENKBN
                                                              (IDX)
      *        診療科、剤の終了とする
               WHEN    "$"
                   MOVE    SPA-GMN-INPUTCD(IDX)(2:2)   TO  WRK-SRYKA
                   MOVE    SPA-GMN-INPUTCD(IDX)(5:4)   TO  WRK-DRCD
                   MOVE    "1"                 TO  SPA-COM-ZAIEND (IDX)
                   IF      IDX                 >   1
                       MOVE    "1"                 TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                   END-IF
                   MOVE    ZERO                TO  FLG-JIHI
                   MOVE    ZERO                TO  FLG-BUIKBN
                   MOVE    ZERO                TO  FLG-SYUGI
      *H28.4
                   MOVE    ZERO            TO  FLG-KSNARI
                   MOVE    ZERO                TO  FLG-HASSYO
                   MOVE    ZERO                TO  FLG-AKUSEI-ARI
                   MOVE    ZERO                TO  FLG-YAKUHYO
                   MOVE    ZERO                TO  FLG-GENTEN
                   MOVE    ZERO                TO  WRK-KENSA-CNT
                   MOVE    SPACE               TO  WRK-SRYKBN
                   MOVE    SPACE               TO  WRK-SRYSYUKBN
                   MOVE    SPACE               TO  WRK-HOUKNSKBN
                   MOVE    ZERO                TO  WRK-HOU-IDX
                   MOVE    WRK-SRYKA           TO  SPA-COM-SRYKA(IDX)
                   MOVE    WRK-DRCD            TO  SPA-COM-DRCD (IDX)
      *!
      *        セット入力時、前の診療種別とする
               WHEN    "S"
                   IF     (IDX                 =   1   )   OR
                          (SPA-GMN-INPUTCD(IDX - 1)(1:1)   NOT =   ".")
                       MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                       MOVE    "0048"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SPA-COM-SRYSYUKBN (IDX - 1)
                                           TO  SPA-COM-SRYSYUKBN (IDX)
                       MOVE    SPA-COM-SRYKBN (IDX - 1)
                                           TO  SPA-COM-SRYKBN (IDX)
                       MOVE    SPACE       TO  SPA-COM-ZAIEND (IDX)
                       MOVE    IDX                 TO  IDX-SET
                       MOVE    ZERO                TO  FLG-SYUGI
      *H28.4
                       MOVE    ZERO            TO  FLG-KSNARI
                   END-IF
      *
               WHEN    "."
      *        診療種別入力時、剤の終了とする
                   MOVE    SPA-GMN-INPUTCD(IDX)(2:3)   TO  WRK-SRYSYUKBN
      *            診療種別区分名称、診療行為区分セット
                   PERFORM 510-SRYKBN-MEI-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
                   MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO (IDX)
                   MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN (IDX)
      *
                   MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                   MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (IDX)
                   IF      IDX                 >   1
                       MOVE    "1"                 TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                   END-IF
                   MOVE    SPACE               TO  SPA-COM-ZAIEND (IDX)
      *            自費の時、回数入力まで自費とする
                   IF      WRK-SRYSYUKBN       =   "950"  OR  "960"
      *H25.10
                                                          OR  "961"
                       MOVE    1                   TO  FLG-JIHI
                   ELSE
                       MOVE    ZERO                TO  FLG-JIHI
                   END-IF
                   MOVE    ZERO                TO  FLG-BUIKBN
                   MOVE    ZERO                TO  FLG-SYUGI
      *H28.4
                   MOVE    ZERO            TO  FLG-KSNARI
                   MOVE    ZERO                TO  FLG-HASSYO
                   MOVE    ZERO                TO  FLG-AKUSEI-ARI
                   MOVE    ZERO                TO  FLG-YAKUHYO
                   MOVE    ZERO                TO  FLG-GENTEN
                   MOVE    ZERO                TO  WRK-KENSA-CNT
                   MOVE    SPACE               TO  WRK-HOUKNSKBN
                   MOVE    ZERO                TO  WRK-HOU-IDX
      *!           処方コメントは入院のみ
      *            IF      WRK-SRYSYUKBN       =   "980"
      *                IF      SPA-NYUGAIKBN       =   "1"
      *                    MOVE    "9028"          TO  SPA-ERRCD
      *                    MOVE    "1"             TO  SPA-COM-CUR(IDX)
      *                END-IF
      *!           END-IF
      *!           退院時院外処方コメントは入院のみ
                   IF      WRK-SRYSYUKBN       =   "982"
                       IF      SPA-NYUGAIKBN       =   "2"
                           MOVE    "9024"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                   END-IF
      *ver.4.5.0
      *            処置読み替え対象外はＨ２０年４月から
                   IF      WRK-SRYSYUKBN       =   "409"
                       IF      SPA-SRYYMD          <   "20080401"
                           MOVE    "0501"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                       IF     (SPA-HKNKBN          =   1   )
                         AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                         AND  (SPA-NYUGAIKBN       =   "2" )
                           CONTINUE
                       ELSE
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "0502"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                 (IDX)
                           END-IF
                       END-IF
                   END-IF
      *ver.4.6.0
      *            自賠責金額入力はＨ２２年４月から
                   IF      WRK-SRYSYUKBN       =   "809"
                       IF      SPA-SRYYMD          <   "20100401"
                           MOVE    "0503"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                       IF     (SPA-HKNKBN          =   1   )
      ************       AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                           CONTINUE
                       ELSE
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "0502"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                 (IDX)
                           END-IF
                       END-IF
                   END-IF
      *ver.4.7.0
      *            臨時投薬（外用）はH２６年４月から
                   IF      WRK-SRYSYUKBN       =   "293"
                                               OR  "294"
                                               OR  "295"
                       IF      SPA-SRYYMD          <   "20140401"
                           MOVE    "0504"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                   END-IF
      *            臨時投薬（頓服）はH２６年１０月から
                   IF      WRK-SRYSYUKBN       =   "296"
                                               OR  "297"
                                               OR  "298"
                       IF      SPA-SRYYMD          <   "20141001"
                           MOVE    "0505"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                   END-IF
      *H28.9
      *            初診・再診加算のみはＨ２８年４月から
                   IF      WRK-SRYSYUKBN       =   "114"
                                               OR  "124"
                                               OR  "133"
                       IF      SPA-SRYYMD          <   "20160401"
                           MOVE    "0506"          TO  SPA-ERRCD
                           MOVE    "1"             TO  SPA-COM-CUR(IDX)
                       END-IF
                   END-IF
               WHEN    OTHER
      *        その他
                   IF      FLG-JIHI            =   1
      *                自費の時、回数入力まで自費とする
                       PERFORM 5101-JIHI-HEN-SEC
                   ELSE
                       PERFORM 51011-SRYKBN-HEN-SEC
                   END-IF
           END-EVALUATE
           .
       5101-SYRKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費処理
      *****************************************************************
       5101-JIHI-HEN-SEC           SECTION.
      *
           MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
           MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (IDX)
      *
      *    きざみ値以外は点数再編集（労災円変換）
           IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1"  )   AND
                  (SPA-COM-KZMCOMPSIKIBETU (IDX)   =   1 )
               CONTINUE
           ELSE
               MOVE    SPA-COM-TEN (IDX)   TO  SPA-COM-KISOTEN  (IDX)
           END-IF
           IF      SPA-COM-KAIFLG(IDX)     =   "1"
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
               MOVE    SPACE           TO  WRK-SRYSYUKBN
               MOVE    ZERO            TO  FLG-JIHI
           END-IF
      *    金額入力の自費に付いては、１剤１コードとする
           IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "095" OR
                                                   "096" )  AND
                  (SPA-COM-KISOTEN(IDX)        =   ZERO  )
               MOVE    "1"             TO  SPA-COM-ZAIEND(IDX)
               MOVE    SPACE           TO  WRK-SRYSYUKBN
               MOVE    ZERO            TO  FLG-JIHI
           END-IF
           MOVE    ZERO                TO  FLG-SYUGI
      *H28.4
           MOVE    ZERO                TO  FLG-KSNARI
           MOVE    ZERO                TO  FLG-AKUSEI-ARI
      *
           .
       5101-JIHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区判定処理
      *****************************************************************
       51011-SRYKBN-HEN-SEC           SECTION.
      *
      *    診療区を判定する
           MOVE    SPA-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   MOVE    SPA-COM-INPUTCD(IDX)(2:2)
                                           TO  WRK-SRYKBN
      *            労災コード
                   IF      SPA-COM-RSIKBN (IDX)    =   1
                       MOVE    SPA-COM-INPUTCD(IDX)(4:2)
                                               TO  WRK-SRYKBN
                   END-IF
      *            画像診断の点滴チェック
                   IF     (IDX                 >   1   )  AND
                          (SPA-COM-SRYSYUKBN (IDX - 1) =   "731") AND
                          (SPA-COM-ZAIEND    (IDX - 1) =   SPACE)
                       PERFORM 51013-731-CHKSYORI-SEC
                   ELSE
      *            点数マスタの診療種別より診療区を判定する
                       IF      SPA-COM-TNSSRYSYUKBN(IDX) NOT =   SPACE
                           PERFORM 51011-SRYSYUKBN-KBN-SEC
                       END-IF
                   END-IF
      *            加算行為については、前の剤とする（労災以外）
                   IF     (SPA-COM-DATAKBN (IDX)   =   "2"   )  AND
                          (SPA-COM-RSIKBN  (IDX)   NOT =   1 )
                       PERFORM 51012-KASAN-CHK-SEC
                   ELSE
                       MOVE    WRK-SRYKBN      TO  SPA-COM-SRYKBN(IDX)
                   END-IF
      *---(01.00.0X)--
      *            再診時の手技料で剤終了とする
      *            IF     (IDX                     >   1    )  AND
      *                   (SPA-COM-SRYKBN (IDX)    =   "12" )  AND
      *                   (SPA-COM-DATAKBN (IDX)   =   "1"  )
      *                IF      SPA-GMN-INPUTCD (IDX - 1)(1:1)  =   "."
      *                    CONTINUE
      *                ELSE
      *                    MOVE    "1"             TO  SPA-COM-ZAIEND
      *                                                      (IDX - 1)
      *                END-IF
      *            END-IF
      *---(01.00.0X)--
      *            薬評・器評はエラー
                   IF      FLG-YAKUHYO         =   1
                       MOVE    ZERO            TO  FLG-YAKUHYO
                       MOVE    "1"             TO  SPA-COM-ZAIEND
                                                           (IDX - 1)
                   END-IF
      *
               WHEN    "0"
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN(IDX)
      *            ユーザー登録分
                   PERFORM 5101-SRYKBN-0-HEN-SEC
      *
               WHEN    "8"
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN(IDX)
      *            コメントの時、前の剤とする(前行に回数がない時）
                   IF     (IDX                 =   1   )  OR
                          (SPA-COM-KAIFLG (IDX - 1 )   =   "1" ) OR
                          (SPA-GMN-INPUTCD(IDX - 1 )(1:1)  =  "#"
                                                           OR "$")
      *H26.10
                       OR (SPA-COM-INPUTCD(IDX - 1)
                                               =   "099140012" )
      *                薬剤料逓減の時、内服薬剤とする
                       IF      SPA-COM-INPUTCD (IDX)   =   "820000047"
                           MOVE    "21"            TO  SPA-COM-SRYKBN
                                                                (IDX)
                       ELSE
                           MOVE    "99"            TO  SPA-COM-SRYKBN
                                                                (IDX)
                           MOVE    "990"           TO  SPA-COM-SRYSYUKBN
                                                                (IDX)
                       END-IF
                   ELSE
      *                薬剤料逓減の時、内服薬剤とする
                       IF      (SPA-COM-INPUTCD(IDX)  =   "820000047")
      *???               AND   (SPA-COM-AUTOKBN(IDX)  =   "3"        )
                         AND   (SPA-COM-AUTOKBN(IDX)  =   SPACE      )
                           MOVE    "21"            TO  SPA-COM-SRYKBN
                                                                (IDX)
                       ELSE
                           MOVE    SPA-COM-SRYKBN (IDX - 1)
                                                   TO  SPA-COM-SRYKBN
                                                                (IDX)
                           MOVE    SPA-COM-ZAIEND (IDX - 1)
                                                   TO  SPA-COM-ZAIEND 
                                                                (IDX)
                           MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                              (IDX - 1)
      *                    診療種別入力
      *                    IF      WRK-SRYSYUKBN   NOT =   SPACE
      *                        MOVE    WRK-SRYSYUKBN
      *                                            TO  SPA-COM-SRYSYUKBN
      *                                                         (IDX)
      *                        MOVE    SPACE       TO  SPA-COM-ZAIEND
      *                                                         (IDX)
      *                    END-IF
                       END-IF
                   END-IF
      *            時間入力の時、前のコードをチェックする
                   IF      (SPA-COM-INPUTCD (IDX)  =   "840000071"  OR
                                                       "840000044"  )
                       PERFORM 510112-JIKAN-INP-SEC
                   END-IF
      *            薬評・器評
                   IF      FLG-YAKUHYO     =   1
                       MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX)
                   END-IF
      *            薬評・器評
                   IF      (SPA-COM-INPUTCD (IDX)  =   "820000094"  OR
                                                       "820000095"
      *H26.12          （加評）
                                                   OR  "820000171"
      *H28.4           （体評）
                                                   OR  "820000226"  )
      *                診療種別の直下
                       IF     (IDX             >   1     )  AND
                              (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                =   "."  )
                           MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX)
                           MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX - 1)
                           MOVE    1               TO  FLG-YAKUHYO
                       ELSE
                           MOVE    "0221"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            減点
                   IF      SPA-ERRCD           =   SPACE
                       IF      FLG-GENTEN          =   1
                           MOVE    "1"             TO  SPA-COM-GENTEN
                                                               (IDX)
                       END-IF
                   END-IF
               WHEN    "6"
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN(IDX)
      *
      *H24.4改正
      *点数を戻す
                   MOVE    SPA-COM-TEN (IDX)   TO  SPA-COM-KISOTEN(IDX)
      *
      *            薬剤の時、内服・外用を決定する
                   EVALUATE    SPA-COM-YKZKBN(IDX)
                       WHEN    "6"
                           MOVE    "23"            TO  WRK-SRYKBN
                       WHEN    "4"
                           MOVE    "30"            TO  WRK-SRYKBN
      *                    歯科薬剤
                       WHEN    "8"
                       WHEN    "9"
                           MOVE    "23"            TO  WRK-SRYKBN
      *ver.4.7
                       WHEN    "3"
      *                    その他
      *                    薬剤料減点は注射とする
                           IF      SPA-COM-INPUTCD (IDX) =   "630010001"
                               MOVE    "30"            TO  WRK-SRYKBN
                           ELSE
                               MOVE    "21"            TO  WRK-SRYKBN
                           END-IF
      *
                       WHEN    OTHER
                           MOVE    "21"            TO  WRK-SRYKBN
                   END-EVALUATE
      *            外用薬の種別が入力されたら、すべて外用とする
                   IF      WRK-SRYSYUKBN       =   "230" OR
                                                   "231" OR
                                                   "232"
                                               OR  "233"
                                               OR  "234"
      *H26.4 から　臨時投薬追加
                                               OR  "293"
                                               OR  "294"
                                               OR  "295"
                       MOVE    "23"            TO  WRK-SRYKBN
                   END-IF
      *!
      *            外用薬で内服算定の判定
                   IF     (WRK-SRYKBN          =   "23"  )  AND
                          (SPA-COM-NAIFUKUKBN (IDX)
                                               =   1     )  AND
                          (WRK-SRYSYUKBN       =   "210" OR
                                                   "211" OR
                                                   "212"
                                               OR  "213"
                                               OR  "214"
                                               OR  "290"
                                               OR  "291"
                                               OR  "292"  )
                       MOVE    "21"            TO  WRK-SRYKBN
                   END-IF
      *!
      *            注射薬で内服算定の判定
                   IF     (WRK-SRYKBN          =   "30"  )  AND
                          (SPA-COM-NAIFUKUKBN (IDX)
                                               =   1     )  AND
                          (WRK-SRYSYUKBN       =   "210" OR
                                                   "211" OR
                                                   "212"
                                               OR  "213"
                                               OR  "214"
                                               OR  "290"
                                               OR  "291"
                                               OR  "292"  )
                       MOVE    "21"            TO  WRK-SRYKBN
                   END-IF
      *            前が処方せん料は薬剤は入力しない
                   IF     (IDX                     >   1     ) AND
                          (SPA-COM-SRYKBN (IDX - 1) =   "80" ) AND
                          (SPA-COM-SRYSYUKBN (IDX - 1)
                                                   =   "820" )
                       MOVE    1               TO  FLG-SYOHO
                   ELSE
                       MOVE    ZERO            TO  FLG-SYOHO
                   END-IF
      *            内服の時、前の行為が薬剤に付属できるか判定
                   IF     (IDX                     >   1     ) AND
                          (SPA-COM-KAIFLG(IDX - 1) =   SPACE ) AND
                          (WRK-SRYKBN              =   "21"  )
                     AND  (FLG-HASSYO              =   ZERO  )
                     AND  (FLG-SYOHO               =   ZERO  )
                     AND  (SPA-COM-SRYSYUKBN(IDX - 1)
                                               NOT =   "809" )
                       EVALUATE    SPA-COM-SRYKBN(IDX - 1)
                           WHEN    "21"
                           WHEN    "22"
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                    TO  WRK-SRYKBN
                               MOVE    SPACE        TO  SPA-COM-ZAIEND
                                                              (IDX - 1)
                           WHEN    "23"
                               CONTINUE
      ****指導料削除       WHEN    "13"
                           WHEN    "14"
                           WHEN    "40"
                           WHEN    "50"
                           WHEN    "54"
                           WHEN    "60"
                           WHEN    "64"
                           WHEN    "70"
                           WHEN    "80"
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                   TO  WRK-SRYKBN
                               MOVE    SPACE       TO  SPA-COM-ZAIEND
                                                              (IDX - 1)
                       END-EVALUATE
                   END-IF
      *            外用薬の時、処置・検査・手術に付属する
                   IF     (IDX                     >   1     ) AND
                          (SPA-COM-KAIFLG(IDX - 1) =   SPACE ) AND
                          (WRK-SRYKBN              =   "23"  )
                     AND  (FLG-HASSYO              =   ZERO  )
                     AND  (FLG-SYOHO               =   ZERO  )
                     AND  (SPA-COM-SRYSYUKBN(IDX - 1)
                                               NOT =   "809" )
                       EVALUATE    SPA-COM-SRYKBN(IDX - 1)
      ****指導料削除       WHEN    "13"
                           WHEN    "14"
      *                    注射を可能とする
                           WHEN    "31"
                           WHEN    "32"
                           WHEN    "33"
      *
                           WHEN    "40"
                           WHEN    "50"
                           WHEN    "54"
                           WHEN    "60"
                           WHEN    "64"
                           WHEN    "70"
                           WHEN    "80"
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                   TO  WRK-SRYKBN
                               MOVE    SPACE       TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                       END-EVALUATE
                   END-IF
      *            注射液　注射行為中であること
                   IF      WRK-SRYKBN          =   "30"
                       IF     (IDX                     >   1   )  AND
                              (SPA-COM-KAIFLG(IDX - 1) =   SPACE )
                         AND  (FLG-HASSYO              =   ZERO  )
                         AND  (FLG-SYOHO               =   ZERO  )
                         AND  (SPA-COM-SRYSYUKBN(IDX - 1)
                                                   NOT =   "809" )
                           EVALUATE    SPA-COM-SRYKBN(IDX - 1)
      ****指導料削除           WHEN    "13"
                               WHEN    "14"
                               WHEN    "31"
                               WHEN    "32"
                               WHEN    "33"
                               WHEN    "40"
                               WHEN    "50"
                               WHEN    "54"
                               WHEN    "60"
                               WHEN    "64"
                               WHEN    "70"
                               WHEN    "80"
                                   MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                   TO WRK-SRYKBN
                                   MOVE    SPACE   TO  SPA-COM-ZAIEND
                                                            (IDX - 1)
                               WHEN    OTHER
                                   MOVE    "0009"  TO  SPA-ERRCD
                           END-EVALUATE
                       ELSE
                           MOVE    "0009"          TO  SPA-ERRCD
                       END-IF
                   END-IF
                   MOVE    WRK-SRYKBN      TO  SPA-COM-SRYKBN(IDX)
      *            薬評・器評
                   IF      FLG-YAKUHYO     =   1
                       MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX)
                   END-IF
      *            薬評・器評
                   IF      (SPA-COM-INPUTCD (IDX)  =   "630010011"
                                                   OR  "630010012"
                                                   OR  "630010013"  )
      *                診療種別の直下
                       IF     (IDX             >   1     )  AND
                              (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                =   "."  )
                           MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX)
                           MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX - 1)
                           MOVE    1               TO  FLG-YAKUHYO
                       ELSE
                           MOVE    "0221"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *ver.4.7
      *            減点入力
                   IF     (SPA-COM-TENSIKIBETU (IDX)   =   7  )
      *                診療種別の直下
                       IF     (IDX             >   1     )  AND
                              (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                =   "."  )
                           MOVE    "1"             TO  SPA-COM-GENTEN
                                                              (IDX)
                           MOVE    "1"             TO  SPA-COM-GENTEN
                                                              (IDX - 1)
                           MOVE    1               TO  FLG-GENTEN
                       ELSE
                           MOVE    "0240"          TO  SPA-ERRCD
                       END-IF
      *                薬剤料減点のチェック（その他注射・入院）
                       IF     (SPA-COM-INPUTCD (IDX) =   "630010001" )
                           IF     (SPA-NYUGAIKBN         =   "2"   )
                             AND  (SPA-ERRCD             =   SPACE )
                               MOVE    "0242"          TO  SPA-ERRCD
                           END-IF
                           IF     (WRK-SRYSYUKBN     NOT =   "340" )
                             AND  (SPA-ERRCD             =   SPACE )
                               MOVE    "0241"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                            (IDX - 1)
                           END-IF
                       END-IF
                   END-IF
      *!!!!
               WHEN    "7"
      *            前が処方せん料は薬剤は入力しない
                   IF     (IDX                     >   1     ) AND
                          (SPA-COM-SRYKBN (IDX - 1) =   "80" ) AND
                          (SPA-COM-SRYSYUKBN (IDX - 1)
                                                   =   "820" )
                       MOVE    1               TO  FLG-SYOHO
                   ELSE
                       MOVE    ZERO            TO  FLG-SYOHO
                   END-IF
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN(IDX)
      *            器材（前の剤に付属する）
                   IF     (IDX             >   1   )  AND
                          (SPA-COM-KAIFLG(IDX - 1) =   SPACE ) AND
                          (SPA-COM-SRYKBN(IDX - 1) 
                                           =   "31"  OR  "32"  OR
                                               "33"  OR  "40"  OR
                                               "50"  OR  "54"  OR
                                               "60"  OR  "70"  OR
                                               "80"  OR  "14"
                                           OR  "64"
                                           OR  "21" OR  "22" OR
                                               "23"          )
                     AND  (FLG-HASSYO              =   ZERO  )
                     AND  (FLG-SYOHO               =   ZERO  )
                       MOVE    SPA-COM-SRYKBN(IDX - 1)
                                               TO  SPA-COM-SRYKBN(IDX)
                       MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
      *                薬評・器評
                       IF      FLG-YAKUHYO     =   1
                           MOVE    "1"             TO  SPA-COM-YAKUHYO
                                                              (IDX)
                       END-IF
                   ELSE
                       MOVE    "0047"          TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
      *    診区　変更時
               IF     (IDX                 >   1         )  AND
                      (SPA-COM-SRYKBN (IDX - 1)
                                       NOT =  SPA-COM-SRYKBN(IDX))
      *        約束セットの時、回数入力まで同じであること
                   IF      SPA-COM-SET (IDX)   =   "1"
                       MOVE    "0049"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR(IDX-SET)
                   END-IF
               END-IF
           END-IF
      *    剤終了判定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 510112-ZAIEND-HEN-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  SPA-COM-CUR  (IDX)
           END-IF
           .
       51011-SRYKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤終了判定処理
      *****************************************************************
       510112-ZAIEND-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    最終行・回数・計　入力時
           IF     (SPA-COM-KAIFLG (IDX)     =   "1"        )  OR
                  (SPA-COM-KEIFLG (IDX)     =   "1" OR "2" )
               MOVE    1               TO  FLG-SYORI
           END-IF
      *    診察の時、手技料で剤分離とする
           IF     (SPA-COM-SRYKBN (IDX)(1:1)    =   "1" )  AND
                  (SPA-COM-DATAKBN (IDX)   =   "1"  )
               IF      FLG-SYUGI           =   1
                  OR   FLG-KSNARI          =   1
                   MOVE    2               TO  FLG-SYORI
               END-IF
               MOVE    1               TO  FLG-SYUGI
           END-IF
           IF     (SPA-COM-SRYKBN (IDX)(1:1)    =   "1" )  AND
                  (SPA-COM-DATAKBN (IDX)   =   "2"  )
               MOVE    1                TO  FLG-KSNARI
           END-IF
      *
      *H25.7
      *    労災コードのその他８０は剤分離
           IF     (SPA-SRYYMD          >=  "20130701" )
               IF     (SPA-COM-SRYKBN (IDX)    =   "80" )
                AND   (SPA-COM-INPUTCD(IDX)(1:3)   =   "101")
                AND   (SPA-COM-DATAKBN (IDX)   =   "1"  )
                   IF      FLG-SYUGI           =   1
                       MOVE    2               TO  FLG-SYORI
                   END-IF
                   MOVE    1               TO  FLG-SYUGI
               END-IF
           END-IF
      *!!
      *    処方せん料は手技料で剤分離とする
           IF     (SPA-COM-SRYKBN (IDX)    =   "80" )  AND
                  (SPA-COM-SRYSYUKBN(IDX)  =   "820")  AND
                  (SPA-COM-DATAKBN (IDX)   =   "1"  )
               IF      FLG-SYUGI           =   1
                   MOVE    2               TO  FLG-SYORI
               END-IF
               MOVE    1               TO  FLG-SYUGI
           END-IF
      *    処方料は手技料で剤分離とする
           IF     (SPA-COM-SRYKBN (IDX)(1:1)   =   "2" )  AND
                  (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                  (SPA-COM-DATAKBN(IDX)        =   "1"  )
               IF      FLG-SYUGI           =   1
                   MOVE    2               TO  FLG-SYORI
               END-IF
               MOVE    1               TO  FLG-SYUGI
           END-IF
      *!!
      *    診区　変更時
           IF     (IDX                 >   1         )  AND
                  (SPA-COM-SRYKBN (IDX - 1)
                                   NOT =  SPA-COM-SRYKBN(IDX))
               MOVE    2               TO  FLG-SYORI
           END-IF
      *
      *    約束セット終了時、終了とする
           IF      IDX                 <   SPA-MAX-LINE
               IF     (SPA-COM-SET (IDX)        =   "1" )  AND
                      (SPA-COM-SET (IDX + 1)    =   SPACE)
                   MOVE    1               TO  FLG-SYORI
               END-IF
           END-IF
      *    診療種別 　変更時
           IF     (IDX                 >   1         )  AND
                  (SPA-COM-SRYSYUKBN (IDX - 1)
                                   NOT =   SPACE     )  AND
                  (SPA-COM-SRYSYUKBN (IDX)
                                   NOT =   SPACE     )  AND
                  (SPA-COM-SRYSYUKBN (IDX - 1)
                                   NOT =   SPA-COM-SRYSYUKBN(IDX))
               MOVE    2               TO  FLG-SYORI
           END-IF
      *---(02.00.05)  LINE ADD  START  --------------------------------
      *    検査の時、包括対象検査により、剤を決定する
           IF     (SPA-COM-SRYKBN (IDX)    =   "60"
                                           OR  "64"  )  AND
                  (SPA-GMN-INPUTCD(IDX)(1:1)   NOT =   "." )  AND
                  (SPA-COM-DATAKBN(IDX)    =   "1"  )
              AND (SPA-COM-SET (IDX)   NOT =   "1"  )
               PERFORM 5101121-HKTKENSA-HEN-SEC
           END-IF
      *
      *    前行が急性発生の時終了とする
           IF     (SPA-COM-INPUTCD(IDX)    =   "099800101"
                                           OR  "099800102"
      *        在総診算定終了
                                           OR  "099140001"
                                           OR  "099140004" )
      *H26.10  在宅訪問診療他
            OR    (SPA-COM-INPUTCD(IDX)    =   "099140011"
                                           OR  "099140012" )
               MOVE    1               TO  FLG-HASSYO
           ELSE
               IF      FLG-HASSYO          =   1
                   IF     (SPA-COM-SRYKBN(IDX)
                                       =   SPA-COM-SRYKBN(IDX - 1)) AND
                         ((SPA-COM-INPUTCD(IDX)(1:1)
                                               =   "8"  )  OR
                          (SPA-COM-INPUTCD(IDX)(1:3)
                                               =   "008"  ))
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  FLG-HASSYO
                       MOVE    2               TO  FLG-SYORI
                   END-IF
               END-IF
           END-IF
      *    減点分はコメント以外は終了
           IF      FLG-GENTEN          =   1
               IF      (SPA-COM-INPUTCD (IDX)(1:1) =   "8" )
                   OR  (SPA-COM-INPUTCD (IDX)(1:3) =   "008")
                   OR  (SPA-COM-TENSIKIBETU(IDX)   =   7    )
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-GENTEN
                   MOVE    2                   TO  FLG-SYORI
               END-IF
           END-IF
      *
      *    会計照時、剤変更時、診療区分変更エラー
           IF      LNK-SC93-PG         =   "J04"
               IF      FLG-SYORI           =   2
      *            検体検査コメントの判定
                   IF    ((SPA-COM-KENTAICOMMENT (IDX) =   1) AND
                          (IDX                 <   SPA-GMN-MAX))
                      OR ( IDX                 =   1          )
                       CONTINUE
                   ELSE
                       IF     (SPA-COM-SRYKBN (IDX - 1)
                                           =   SPA-COM-SRYKBN(IDX))
                         AND  (SPA-COM-SRYKBN (IDX)    =   "60"   )
                           MOVE    "1034"          TO  SPA-ERRCD
                       ELSE
                           MOVE    "1020"          TO  SPA-ERRCD
                       END-IF
                       MOVE    "1"             TO  SPA-COM-CUR(IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    １剤１明細のコードの時は、剤終了とする
           IF      FLG-ZAI             =   1
               IF     (IDX             >   1   )  AND
                     ((SPA-GMN-INPUTCD(IDX - 1)(1:1)   NOT =   ".") OR
                      (SPA-COM-SRYKBN (IDX - 1)
                                       NOT =   SPA-COM-SRYKBN(IDX)))
                   MOVE    2               TO  FLG-SYORI
               ELSE
                   MOVE    1               TO  FLG-SYORI
               END-IF
           END-IF
      *    １剤１明細のコード（自賠責その他）の時は、剤終了とする
           IF      FLG-ZAI             =   2
               IF     (IDX             >   1   )  AND
                     ((SPA-COM-SRYSYUKBN (IDX - 1)
                                       NOT =   SPA-COM-SRYSYUKBN(IDX))
                 OR   (SPA-GMN-INPUTCD   (IDX - 1)(1:1)
                                       NOT =   "."               ))
                   MOVE    2               TO  FLG-SYORI
               ELSE
                   MOVE    1               TO  FLG-SYORI
               END-IF
           END-IF
      *
           IF      FLG-SYORI           =   ZERO
      *        剤継続
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX)
               IF     (IDX             >   1  )  AND
                      (SPA-COM-ZAIEND(IDX - 1)   =   SPACE )
                   MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                           TO  SPA-COM-SRYSYUKBN (IDX)
               END-IF
           ELSE
      *        剤終了
               IF    ((IDX                 =   1    )  AND
                      (FLG-SYORI       NOT =   2    )   )  OR
                      (SPA-COM-KAIFLG(IDX) =   "1"      )
                   MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
                   MOVE    SPACE           TO  WRK-SRYSYUKBN
                   MOVE    ZERO            TO  IDX-SET
                   MOVE    ZERO            TO  FLG-SYUGI
      *H28.4
                   MOVE    ZERO            TO  FLG-KSNARI
                   MOVE    ZERO            TO  FLG-AKUSEI-ARI
               END-IF
               IF      FLG-SYORI           =   2
                   IF      IDX             >   1
                       MOVE    "1"         TO  SPA-COM-ZAIEND (IDX - 1)
                   END-IF
                   MOVE    SPACE           TO  WRK-SRYSYUKBN
                   MOVE    ZERO            TO  IDX-SET
               ELSE
                   MOVE    ZERO            TO  FLG-SYUGI
      *H28.4
                   MOVE    ZERO            TO  FLG-KSNARI
               END-IF
           END-IF
      *
      *    前行に回数入力時、前回を終了とする
           IF     (IDX                 >   1         )  AND
                  (SPA-COM-KAIFLG (IDX - 1)    =   "1" )  AND
                  (SPA-GMN-INPUTCD(IDX - 1)(1:1)   NOT =  ".")
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX - 1)
      *H28.8   削除
      *        IF     (SPA-COM-SRYKBN(IDX - 1) NOT =   "13" )
      *        AND    (SPA-COM-SRYKBN(IDX)     =   "13"     )
      *            CONTINUE
      *        ELSE
      *            MOVE    ZERO            TO  FLG-AKUSEI-ARI
      ****     END-IF
           END-IF
      *
      *    入院回数入力時、終了とする
           IF     (IDX                 >   1         )  AND
                  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "*" )
               MOVE    SPACE           TO  SPA-COM-ZAIEND (IDX - 1)
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
               MOVE    "1"             TO  SPA-COM-KAIFLG (IDX)
               MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-SRYKBN   (IDX - 1)
                                       TO  SPA-COM-SRYKBN (IDX)
           END-IF
      *
      *    最終行を剤終了とする
           IF      IDX                 =   SPA-GMN-MAX
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
           END-IF
      *
      *    部位フラグクリア
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
               MOVE    ZERO                TO  FLG-BUIKBN
               MOVE    ZERO                TO  WRK-KENSA-CNT
               MOVE    ZERO                TO  FLG-AKUSEI-ARI
               MOVE    SPACE               TO  WRK-HOUKNSKBN
               MOVE    ZERO                TO  WRK-HOU-IDX
               MOVE    ZERO                TO  FLG-YAKUHYO
               MOVE    ZERO                TO  FLG-GENTEN
           END-IF
           .
       510112-ZAIEND-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括検査の剤終了判定処理
      *****************************************************************
       5101121-HKTKENSA-HEN-SEC           SECTION.
      *
      *    前行位置
           IF      IDX                 =   1
               MOVE    1               TO  IDX-1
           ELSE
               COMPUTE IDX-1           =   IDX    -   1
           END-IF
      *    包括対象外は対象としない
           IF      (SPA-COM-SRYSYUKBN(IDX)  =   "610" )
               OR  (SPA-COM-KENTAICOMMENT (IDX) =   1    )
               OR  (SPA-COM-INPUTCD (IDX)(1:1)  NOT =   "1"  )
                CONTINUE
           ELSE
      *         包括対象検査件数チェック
                PERFORM 51013-KENSA-ZAI-CHK-SEC
           END-IF
      *
           IF      (SPA-GMN-INPUTCD(IDX-1)(1:1)  =   "." )  OR
                   (SPA-COM-SRYKBN (IDX-1)   NOT =  SPA-COM-SRYKBN
                                                             (IDX))
      *        包括出来高
             OR    (SPA-COM-INPUTCD (IDX-1)      =   "099999903" )
      *        包括算定（剤）
             OR    (SPA-COM-INPUTCD (IDX-1)      =   "099999908" )
      *        コメント
             OR    (SPA-COM-INPUTCD (IDX)(1:1)   =   "8"    )
             OR    (SPA-COM-INPUTCD (IDX)(1:3)   =   "008"  )
               CONTINUE
           ELSE
      *        検体検査コメントの判定
               IF     (SPA-COM-KENTAICOMMENT (IDX-1)   =   1 )
      *            検査等実施判断グループ判定
                   IF      SPA-COM-KNSJISGRPKBN (IDX)  >=  01
                                                   AND <=  07
                       MOVE    SPA-COM-HOUKNSKBN(IDX)
                                                   TO  SPA-COM-HOUKNSKBN
                                                               (IDX-1)
      *                診療種別
                       IF    (SPA-COM-SRYKBN (IDX-1 - 1)
                                                   =  SPA-COM-SRYKBN
                                                             (IDX-1))
                       AND   (SPA-COM-HOUKNSKBN(IDX-1)
                                                   =  SPA-COM-HOUKNSKBN
                                                          (IDX-1 - 1))
                       AND   (SPA-COM-HOUKNSKBN(IDX-1)
                                               NOT =   ZERO        )
                           MOVE    SPA-COM-SRYSYUKBN (IDX-1 - 1)
                                                   TO  SPA-COM-SRYSYUKBN
                                                             (IDX-1)
                           MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                             (IDX-1 - 1)
                           MOVE    SPA-COM-SRYSYUKBN (IDX-1 - 1)
                                                   TO  WRK-SRYSYUKBN
                       END-IF
      *                同一検査判定コード
                       MOVE    SPA-COM-INPUTCD(IDX-1)
                                                   TO  SPA-COM-KENGRP
                                                               (IDX)
                       MOVE    SPACE               TO  SPA-COM-ZAIEND
                                                         (IDX-1)
                       IF      FLG-SYORI           =   2
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                   END-IF
               END-IF
      *        包括対象検査なし
               IF      SPA-COM-HOUKNSKBN(IDX)   =   ZERO
                   MOVE    2                    TO  FLG-SYORI
      *            検体検査コメントの判定
                   IF     (SPA-COM-KENTAICOMMENT(IDX-1) =   1)
      *            検査等実施判断グループ判定
                      AND (SPA-COM-KNSJISGRPKBN (IDX)  >=  01
                                                   AND <=  07 )
                       MOVE    1               TO  FLG-SYORI
                   END-IF
      *            前が包括出来高コードは前行で終了
                   IF      (SPA-COM-INPUTCD (IDX-1)
                                                   =   "099999903"
      *            包括算定（剤）
                                                   OR  "099999908" )
                       MOVE    1                   TO  FLG-SYORI
                   END-IF
      *H27.10
                   IF      FLG-SYORI               =   2
                       MOVE    SPACE               TO  WRK-HOUKNSKBN
                       MOVE    ZERO                TO  WRK-KENSA-CNT
                   END-IF
               ELSE
      *            包括対象検査変更時
                   IF     (SPA-COM-HOUKNSKBN(IDX)
                                               =  SPA-COM-HOUKNSKBN
                                                             (IDX-1)
                                               OR  WRK-HOUKNSKBN  )
      *            包括出来高
                      OR  (SPA-COM-INPUTCD (IDX-1)
                                               =   "099999903"
      *            包括算定（剤）
                                               OR  "099999908" )
                       CONTINUE
                   ELSE
                       MOVE    2                    TO  FLG-SYORI
                   END-IF
      *
      *            前が検体検査コメント
                   IF     (SPA-COM-KENTAICOMMENT (IDX-1) =   1)
                     AND  (IDX-1             >   1   )
                       IF     (SPA-COM-HOUKNSKBN(IDX)
                                               =   WRK-HOUKNSKBN )
                           IF      WRK-HOU-IDX     =   ZERO
                               COMPUTE WRK-HOU-IDX     =   IDX-1  -  1
                           END-IF
                           MOVE    ZERO      TO  FLG-SYORI
                           MOVE    SPA-COM-SRYSYUKBN (WRK-HOU-IDX)
                                                 TO  SPA-COM-SRYSYUKBN
                                                             (IDX)
                           MOVE    SPA-COM-SRYSYUKBN (WRK-HOU-IDX)
                                                 TO  SPA-COM-SRYSYUKBN
                                                             (IDX-1)
                           MOVE    SPACE         TO  SPA-COM-ZAIEND
                                                             (IDX-1)
                           MOVE    SPA-COM-SRYSYUKBN (WRK-HOU-IDX)
                                                 TO  WRK-SRYSYUKBN
                       END-IF
      *! !!!                                     =   SPA-COM-HOUKNSKBN
      *                                                    (IDX-1 - 1)
      *                    MOVE    ZERO      TO  FLG-SYORI
      *                    MOVE    SPA-COM-SRYSYUKBN (IDX-1 - 1)
      *                                          TO  SPA-COM-SRYSYUKBN
      *                                                      (IDX)
      *                    MOVE    SPA-COM-SRYSYUKBN (IDX-1 - 1)
      *                                          TO  SPA-COM-SRYSYUKBN
      *                                                      (IDX-1)
      *                    MOVE    SPACE         TO  SPA-COM-ZAIEND
      *                                                      (IDX-1)
      *                    MOVE    SPA-COM-SRYSYUKBN (IDX-1 - 1)
      *                                          TO  WRK-SRYSYUKBN
      *!!!!!!!!1       END-IF
                   END-IF
               END-IF
      *
      *        包括対象外の剤終了時、診療種別を検査にする
               IF    (SPA-COM-SRYSYUKBN(IDX)  =   "610" ) AND
                     (FLG-SYORI               =   2     )
                   MOVE    SPA-COM-TNSSRYSYUKBN (IDX)
                                            TO  SPA-COM-SRYSYUKBN (IDX)
               END-IF
           END-IF
      *
      *    検査等実施判断グループ判定
           IF     (SPA-COM-HOUKNSKBN(IDX)  NOT =   ZERO  )
              AND (SPA-COM-INPUTCD  (IDX)(1:1) =   "1"   )
              AND (SPA-COM-KNSJISGRPKBN (IDX)  >=  01
                                           AND <=  07     )
      *
               MOVE    SPA-COM-HOUKNSKBN(IDX)  TO  WRK-HOUKNSKBN
               MOVE    IDX                     TO  WRK-HOU-IDX
           END-IF
           .
       5101121-HKTKENSA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー登録コード診療区判定処理
      *****************************************************************
       5101-SRYKBN-0-HEN-SEC           SECTION.
      *
           EVALUATE    SPA-COM-INPUTCD (IDX)(1:3)
      *        薬剤　服用コメント(前が薬剤の時、同じ剤にする）
               WHEN    "001"
                   MOVE    ZERO                TO  FLG-ZEN
                   IF     (IDX                 >   1   )  AND
                          (SPA-COM-SRYKBN(IDX - 1) =  "21" OR
                                                      "22" OR
                                                      "23")  AND
                          (SPA-COM-ZAIEND(IDX - 1) =   SPACE )
                       MOVE    1               TO  FLG-ZEN
                   END-IF
                   IF     (IDX                 >   1   )  AND
                         ((SPA-COM-SRYKBN(IDX - 1) =  "14")  AND
                          (SPA-COM-INPUTCD(IDX - 1)(1:1)
                                               NOT =   "1")) AND
                          (SPA-COM-ZAIEND(IDX - 1) =   SPACE )
                       MOVE    1               TO  FLG-ZEN
                   END-IF
                   IF     (IDX                 >   1      ) AND
                          (SPA-COM-SRYKBN(IDX - 1) =  "98") AND
                          (SPA-COM-ZAIEND(IDX - 1) =   SPACE)
                       MOVE    1               TO  FLG-ZEN
                   END-IF
                   IF      FLG-ZEN             =   1
                       MOVE    SPA-COM-SRYKBN (IDX - 1)
                                               TO  SPA-COM-SRYKBN(IDX)
                       MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                            (IDX - 1)
                   ELSE
                       MOVE    "99"            TO  SPA-COM-SRYKBN(IDX)
      *!               MOVE    "990"           TO  SPA-COM-SRYSYUKBN
      *!                                                         (IDX)
                   END-IF
      *        画像診断撮影部位(前が画像診断であること）
               WHEN    "002"
                   IF     (IDX                 >   1    )  AND
                          (SPA-COM-SRYKBN(IDX - 1) =   "70"  )
                       MOVE    SPA-COM-SRYSYUKBN (IDX - 1)
                                               TO  SPA-COM-SRYSYUKBN
                                                                 (IDX)
                       MOVE    SPA-COM-SRYKBN (IDX - 1)
                                               TO  SPA-COM-SRYKBN (IDX)
                       MOVE    SPA-COM-ZAIEND (IDX - 1)
                                               TO  SPA-COM-ZAIEND
                                                                 (IDX)
                       MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                              (IDX - 1)
                   ELSE
                       MOVE    "70"            TO  SPA-COM-SRYKBN (IDX)
                       MOVE    SPACE           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                   END-IF
      *            剤終了
                   IF      FLG-BUIKBN      =   1
                        MOVE    "1"            TO  SPA-COM-ZAIEND
                                                              (IDX - 1)
                   END-IF
                   MOVE    1                 TO  FLG-BUIKBN
      *        保険外（自費）（1行１剤とする）
               WHEN    "095"
                   MOVE    "95"            TO  SPA-COM-SRYKBN (IDX)
                   MOVE    "950"           TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
                   MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
                   MOVE    1               TO  FLG-ZAI
      *            自賠責の時、８０とする
                   IF     (SPA-RSI-HKNKBN            =  "4" )  AND
                          (SPA-COM-INPUTCD(IDX)(1:5) =  "09591"
                                                    OR  "09592"
                                                    OR  "09593")
                       MOVE    "80"        TO  SPA-COM-SRYKBN (IDX)
                       MOVE    "800"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
      *                点数マスタ金額＝０
                       IF     (SPA-COM-TEN (IDX)    =   ZERO )
                       AND    (SPA-SRYYMD           >=  "20100401")
                           MOVE    "809"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
                       END-IF
                       MOVE    "1"         TO  SPA-COM-ZAIEND (IDX)
                       MOVE    2           TO  FLG-ZAI
                   END-IF
      *            労災の時、８０とする
                   IF     (SPA-RSI-HKNKBN            =  "1"
                                                     OR "2"
                                                     OR "3"
      *                   公務災害
                                                     OR "5") AND
                          (SPA-COM-INPUTCD(IDX)(1:5) =  "09593")
                       MOVE    "80"        TO  SPA-COM-SRYKBN  (IDX)
                       MOVE    "800"       TO  SPA-COM-SRYSYUKBN(IDX)
      *                点数マスタ金額＝０
                       IF     (SPA-COM-TEN (IDX)    =   ZERO )
                       AND    (SPA-SRYYMD           >=  "20100401")
                           MOVE    "809"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
                       END-IF
                       MOVE    "1"         TO  SPA-COM-ZAIEND  (IDX)
                       MOVE    2           TO  FLG-ZAI
                   END-IF
      *            労災・自賠責で入院の時、８０とする
                   IF     (SPA-HKNKBN                =  1  ) AND
                          (SPA-NYUGAIKBN             =  "1") AND
                          (SPA-COM-INPUTCD(IDX)(1:5) =  "09594")
                       MOVE    "80"        TO  SPA-COM-SRYKBN (IDX)
                       MOVE    "800"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
      *                点数マスタ金額＝０
                       IF     (SPA-COM-TEN (IDX)    =   ZERO )
                       AND    (SPA-SRYYMD           >=  "20100401")
                           MOVE    "809"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
                       END-IF
                       MOVE    "1"         TO  SPA-COM-ZAIEND  (IDX)
                       MOVE    2           TO  FLG-ZAI
                   END-IF
      *H25.7
      *            第三者行為の時　８０とする
                   IF     (SPA-RSI-HKNKBN            =  "6" )
                    AND  ((SPA-COM-INPUTCD(IDX)(1:5) =  "09591"
                                                    OR  "09592"
                                                    OR  "09593")  OR
                         ((SPA-NYUGAIKBN             =  "1"  ) AND
                          (SPA-COM-INPUTCD(IDX)(1:5) =  "09594")))
                       MOVE    "80"        TO  SPA-COM-SRYKBN (IDX)
                       MOVE    "800"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
      *                点数マスタ金額＝０
                       IF     (SPA-COM-TEN (IDX)    =   ZERO )
                       AND    (SPA-SRYYMD           >=  "20100401")
                           MOVE    "809"       TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
                       END-IF
                       MOVE    "1"         TO  SPA-COM-ZAIEND (IDX)
                       MOVE    2           TO  FLG-ZAI
                   END-IF
      *
      *        保険外（自費）（1行１剤とする）
               WHEN    "096"
                   MOVE    "96"            TO  SPA-COM-SRYKBN (IDX)
                   MOVE    "960"           TO  SPA-COM-SRYSYUKBN (IDX)
                   MOVE    "1"             TO  SPA-COM-ZAIEND  (IDX)
                   MOVE    1               TO  FLG-ZAI
      *        特定器材（前の剤に付属する）
               WHEN    "058"
               WHEN    "059"
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN (IDX)
      *            器材（前の剤に付属する）
                   IF     (IDX             >   1   )  AND
                          (SPA-COM-KAIFLG(IDX - 1) =   SPACE )
                     AND  (SPA-COM-SRYKBN(IDX - 1) 
                                            =   "31" OR  "32" OR
                                                "33" OR  "40" OR
                                                "50" OR  "54" OR
                                                "60" OR  "70" OR
                                                "80" OR  "14" OR
                                                "21" OR  "22" OR
                                                "23" OR  "13"
                                            OR  "64"           )
                       MOVE    SPA-COM-SRYKBN(IDX - 1)
                                               TO  SPA-COM-SRYKBN(IDX)
                       MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                   ELSE
                       MOVE    "0047"          TO  SPA-ERRCD
                   END-IF
      *        コメント（前の剤に付属する）
               WHEN    "008"
                   MOVE    SPACE           TO  SPA-COM-SRYSYUKBN(IDX)
      *            前行に回数がない時）
                   IF     (SPA-COM-INPUTCD (IDX)(1:4)
                                           NOT =   "0082"
                                               AND "0083"
                                               AND "0084"
                                               AND "0086"
                                               AND "0085" )  OR
                         ((IDX                 =   1   )  OR
                          (SPA-COM-KAIFLG (IDX - 1 )   =   "1")
                       OR (SPA-GMN-INPUTCD(IDX - 1 )(1:1)
                                                       =   "#"
                                                       OR  "$" )
      *H26.10
                       OR (SPA-COM-INPUTCD(IDX - 1)
                                               =   "099140012" ))
                       MOVE    "99"            TO  SPA-COM-SRYKBN(IDX)
                       MOVE    "990"           TO  SPA-COM-SRYSYUKBN
                                                                 (IDX)
                   ELSE
                       MOVE    SPA-COM-SRYKBN (IDX - 1)
                                               TO  SPA-COM-SRYKBN(IDX)
                       MOVE    SPA-COM-ZAIEND (IDX - 1)
                                               TO  SPA-COM-ZAIEND(IDX)
                       MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                              (IDX - 1)
                   END-IF
      *        フリーの時、点数マスタより、決定
               WHEN    OTHER
                   EVALUATE    SPA-COM-INPUTCD (IDX)
      *            残量廃棄の時、注射であること
                       WHEN    "099309901"
                           PERFORM 51019-ZANHAIKI-CHK-SEC
      *            粉砕の時、内服とする
                       WHEN    "099209901"
                           MOVE    "21"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
      *                    点滴手技なし
                       WHEN    "099303301"
                           MOVE    "33"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
      *                労災・自賠責の加算置換え
                       WHEN    "099509901"
                           IF     (IDX             >   1   )  AND
                                  (SPA-COM-SRYKBN(IDX - 1)
                                                   =   "40" OR
                                                       "50" OR
                                                       "80"  )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                   TO  SPA-COM-SRYKBN
                                                                  (IDX)
                               MOVE    SPACE       TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                           ELSE
                               MOVE    "8020"      TO  SPA-ERRCD
                           END-IF
      *                ＤＵＭＭＹ診察料
                       WHEN    "099110001"
                           MOVE    "11"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "110"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    診察料の時剤を分ける
                           IF     (IDX             >   1  )  AND
                                  (SPA-COM-SRYKBN(IDX - 1)
                                                   =   "11" )  AND
                                  (SPA-COM-DATAKBN(IDX - 1)
                                                   =   "1"  )
                               MOVE    "1"         TO  SPA-COM-ZAIEND
                                                               (IDX - 1)
                           END-IF
                       WHEN    "099120001"
                           MOVE    "12"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "120"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    診察料の時剤を分ける
                           IF     (IDX             >   1  )  AND
                                  (SPA-COM-SRYKBN(IDX - 1)
                                                   =   "12" )  AND
                                  (SPA-COM-DATAKBN(IDX - 1)
                                                   =   "1"   )
                               MOVE    "1"         TO  SPA-COM-ZAIEND
                                                             (IDX - 1)
                           END-IF
      *                Ｈ１６．４から急性発生日
                       WHEN    "099800101"
                       WHEN    "099800102"
                           MOVE    "80"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "810"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *                在総診終了
                       WHEN    "099140001"
      *                在宅患者共同開始日
                       WHEN    "099140004"
                           MOVE    "14"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "140"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *H26.10
      *                訪問診療に掛かる記録書
                       WHEN    "099140011"
                           MOVE    "99"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
      *                    1行前が診療種別以外
                           IF     (IDX             >   1  )  AND
                                  (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                                   =   ".990"
                                                   OR  ".991" )
                               CONTINUE
                           ELSE
                               MOVE    "991"       TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                           END-IF
      *                診療人数合計
                       WHEN    "099140012"
                           MOVE    "99"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
      *                    1行前が診療種別以外
                           IF     (IDX             >   1  )  AND
                                  (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                                   =   ".990"
                                                   OR  ".991" )
                               CONTINUE
                           ELSE
                               MOVE    "991"       TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                           END-IF
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *TEST
      *                用量割合
                       WHEN    "099200011"
                       WHEN    "099200012"
                       WHEN    "099200013"
                       WHEN    "099200014"
                       WHEN    "099200015"
      *H27.4           ３０日投薬可能
                       WHEN    "099200101"
                           IF     (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23" )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "2062"        TO  SPA-ERRCD
                           END-IF
      *H30.4
      *               分割調剤
                       WHEN    "099208101"
                           IF     (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23" )
                              AND (SPA-COM-ZAIEND (IDX - 1)
                                                            =  SPACE )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "0281"        TO  SPA-ERRCD
                           END-IF
      *
      *                後発医薬品への変更可（処方せん毎）
                       WHEN    "099209902"
      *                後発医薬品への変更不可
                       WHEN    "099209904"
      *H28.4           湿布薬枚数制限解除
                       WHEN    "099200201"
      *                保険医療機関へ疑義照会した上で調剤
                       WHEN    "099209921"
      *                保険医療機関へ情報提供
                       WHEN    "099209922"
      *H28.11          残薬指示プログラムオプション解除
                       WHEN    "099209923"
                           MOVE    "98"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "980"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    入院時は982 へ
                           IF      SPA-NYUGAIKBN   =   "1"
                               IF      SPA-COM-SRYSYUKBN(IDX - 1)
                                                   NOT =   "980"
                                   MOVE    "982"   TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                               END-IF
                           END-IF
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *                後発医薬品への変更不可（薬剤毎）
                       WHEN    "099209903"
                           IF     (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23"
                                                           OR  "14" )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "2063"        TO  SPA-ERRCD
                           END-IF
      *                含量規格変更不可（薬剤毎）
                       WHEN    "099209905"
      *                剤型変更不可（薬剤毎）
                       WHEN    "099209906"
                           IF     (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23"
                                                           OR  "14" )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "0268"        TO  SPA-ERRCD
                           END-IF
      *H24.4 改正
      *                銘柄名記載
                       WHEN    "099209907"
      *                一般名記載
                       WHEN    "099209908"
                           IF     (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23"
                                                           OR  "14" )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "2071"        TO  SPA-ERRCD
                           END-IF
      *H28.9 持参薬予約コード
                       WHEN    CONS-JSNYCD1
                       WHEN    CONS-JSNYCD2
                       WHEN    CONS-JSNYCD3
                       WHEN    CONS-JSNYCD4
      *                    投薬の１件目とする
                           IF     (IDX             >   ZERO )
                             AND  (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                   =   "."  )
                             AND  (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23"
                                                           OR  "14" )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                               MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                                   TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                        持参薬区分
                               MOVE    "1"         TO  SPA-COM-JISANYAKU
                                                              (IDX)
                               MOVE    "1"         TO  SPA-COM-JISANYAKU
                                                              (IDX - 1)
                           ELSE
                               MOVE    "0227"        TO  SPA-ERRCD
                           END-IF
      *                    包括保険はエラー
                           IF      SPA-COM-HKNCOMBI (IDX)  =   "9999"
                               MOVE    "0228"        TO  SPA-ERRCD
                           END-IF
      *                数量不明
                       WHEN    CONS-JSNYCD5
                       WHEN    CONS-JSNYCD6
                       WHEN    CONS-JSNYCD7
                       WHEN    CONS-JSNYCD8
      *                    投薬の１件目とする
                           IF     (IDX             >   ZERO )
                             AND  (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                   =   "."  )
                             AND  (SPA-COM-SRYKBN(IDX - 1) =   "21"
                                                           OR  "22"
                                                           OR  "23"
                                                           OR  "14" )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                               MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                                   TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                        持参薬区分
                               MOVE    "2"         TO  SPA-COM-JISANYAKU
                                                              (IDX)
                               MOVE    "2"         TO  SPA-COM-JISANYAKU
                                                              (IDX - 1)
                           ELSE
                               MOVE    "0227"        TO  SPA-ERRCD
                           END-IF
      *                    包括保険はエラー
                           IF      SPA-COM-HKNCOMBI (IDX)  =   "9999"
                               MOVE    "0228"        TO  SPA-ERRCD
                           END-IF
      *H27.4
      *                後発医薬品への変更可（処方せん毎）
                       WHEN    "099209910"
      *                後発医薬品への変更不可（処方せん毎）
                       WHEN    "099209911"
                           MOVE    "98"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "980"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    入院時は982 へ
                           IF      SPA-NYUGAIKBN   =   "1"
                               MOVE    "982"       TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                           END-IF
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
      *                    コメントも不可
                           MOVE   "1"              TO  SPA-COM-KAIFLG
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *
      *                包括出来高算定（剤）
                       WHEN    "099999903"
                           IF     (IDX             >   ZERO )  AND
                                  (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                   =   "."  )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                               MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                                   TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "0219"          TO  SPA-ERRCD
                           END-IF
      *                包括算定（剤）
                       WHEN    "099999908"
                           IF     (IDX             >   ZERO )  AND
                                  (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                                   =   "."  )
                               MOVE    SPA-COM-SRYKBN(IDX - 1)
                                                     TO  SPA-COM-SRYKBN
                                                                  (IDX)
                               MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                                   TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                           ELSE
                               MOVE    "0229"          TO  SPA-ERRCD
                           END-IF
      *                包括出来高算定日
                       WHEN    "099999904"
                           MOVE    "99"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "990"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *H26.4
      *                磁気による刺激法開始日
                       WHEN    "099400001"
      *H30.4
      *                高気圧酸素治療開始日
                       WHEN    "099400002"
      *R02.4
      *                心不全に対する遠赤外線温熱療法開始日
                       WHEN    "099400003"
                           MOVE    "40"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "400"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *ver.4.7         精神科ケア開始
                       WHEN    "099830101"
      *H26.4           通院・在宅精神療法開始
                       WHEN    "099830102"
                           MOVE    "80"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "830"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
      *H28.4           依存症集団療法開始
                       WHEN    "099830103"
      *H30.4           精神科ＳＣ疾患別等専門プログラム加算開始日
                       WHEN    "099830104"
      *R02.4
      *                依存症集団療法開始日（ギャンブル依存症）
                       WHEN    "099830105"
      *                経頭蓋磁気刺激療法開始日
                       WHEN    "099830106"
      *                経頭蓋磁気刺激療法終了日
                       WHEN    "099830107"
                           MOVE    "80"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "830"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
                       WHEN    OTHER
      *                Ｈ１８．４からリハビリ発生日
                       IF       SPA-COM-INPUTCD (IDX)(1:6) =   "099800"
                           MOVE    "80"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
                           MOVE    "810"           TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
      *                    １剤１明細とする
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                                  (IDX)
                           MOVE    1               TO  FLG-ZAI
                       ELSE
      *                    コメントとする
                           MOVE    "99"            TO  SPA-COM-SRYKBN
                                                                  (IDX)
      *!                   MOVE    "990"           TO  SPA-COM-SRYSYUKBN
      *!                                                           (IDX)
                       END-IF
                   END-EVALUATE
           END-EVALUATE
      *
      *    薬評・器評
           IF     (FLG-YAKUHYO         =   1  )
             AND  (SPA-ERRCD           =   SPACE )
               EVALUATE    SPA-COM-INPUTCD (IDX)(1:3)
                   WHEN    "008"
                   WHEN    "059"
                       MOVE    "1"             TO  SPA-COM-YAKUHYO (IDX)
                   WHEN    OTHER
                       MOVE    ZERO            TO  FLG-YAKUHYO
                       IF      SPA-COM-SRYKBN (IDX)
                                               =   SPA-COM-SRYKBN
                                                           (IDX - 1)
                           MOVE    "0225"          TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF
      *    減点
           IF     (FLG-GENTEN          =   1     )
             AND  (SPA-ERRCD           =   SPACE )
               IF      SPA-COM-INPUTCD (IDX)(1:3)  =   "008"
                   MOVE    "1"             TO  SPA-COM-GENTEN (IDX)
               ELSE
                   MOVE    ZERO            TO  FLG-GENTEN
                   IF      SPA-COM-SRYSYUKBN (IDX)
                                           =   SPA-COM-SRYSYUKBN
                                                           (IDX - 1)
                       MOVE    "0226"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       5101-SRYKBN-0-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄チェック処理
      *****************************************************************
       51019-ZANHAIKI-CHK-SEC           SECTION.
      *
           IF      LNK-SC93-HAIKIKBN-FLG   =   "2"
      *        注射・処置・検査・手術・麻酔・画像診断・リハビリ等
               IF      SPA-COM-SRYKBN(IDX - 1)(1:1)    =   "3"
                                                       OR  "4"
                                                       OR  "5"
                                                       OR  "6"
                                                       OR  "7"
                                                       OR  "8"
                   MOVE    SPA-COM-SRYKBN(IDX - 1)
                                               TO  SPA-COM-SRYKBN (IDX)
               ELSE
                   MOVE    "0061"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        注射・処置のみ
               IF      SPA-COM-SRYKBN(IDX - 1) =   "31"
                                               OR  "32"
                                               OR  "33"
      *             処置追加
                                               OR  "40"
                   MOVE    SPA-COM-SRYKBN(IDX - 1)
                                               TO  SPA-COM-SRYKBN (IDX)
               ELSE
                   MOVE    "0061"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       51019-ZANHAIKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタの診療種別より診療区判定処理
      *****************************************************************
       51011-SRYSYUKBN-KBN-SEC           SECTION.
      *
      *ver.4.5.0
      *    治験減点分
           IF     (SPA-COM-TENSIKIBETU (IDX)   =   7  )
              IF      (IDX                 >   1      )  AND
                      (SPA-GMN-INPUTCD (IDX - 1)(1:1)
                                           =   "."    )  AND
                      (WRK-SRYSYUKBN       =   "120"
                                           OR  "130"
                                           OR  "140"
                                           OR  "900"
                                           OR  "920"  )
                   MOVE    1                   TO  FLG-GENTEN
                   MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
                   MOVE    SPA-COM-SRYKBN (IDX - 1)    TO  WRK-SRYKBN
                   MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN
                                                                  (IDX)
                   MOVE    "1"                 TO  SPA-COM-GENTEN
                                                                  (IDX)
                   MOVE    "1"                 TO  SPA-COM-GENTEN
                                                              (IDX - 1)
               ELSE
                   MOVE    "0222"              TO  SPA-ERRCD
               END-IF
               GO      TO      51011-SRYSYUKBN-KBN-EXT
           END-IF
      *
      *    在宅指導料の時、在宅指導料とする
           IF     (WRK-SRYSYUKBN           =   "143" )  AND
                  (SPA-COM-TNSSRYKBN (IDX) =   "14"  )
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-TNSSRYKBN (IDX) TO  WRK-SRYKBN
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN   (IDX)
               GO      TO      51011-SRYSYUKBN-KBN-EXT
           END-IF
      *
      *    検査包括なしの時、診療種別を編集する
           IF     (WRK-SRYSYUKBN           =   "610" )  AND
                  (SPA-COM-TNSSRYKBN (IDX) =   "60"  )
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-TNSSRYKBN (IDX) TO  WRK-SRYKBN
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN   (IDX)
               GO      TO      51011-SRYSYUKBN-KBN-EXT
           END-IF
      *
      *ver.4.5.0
      *    労災読み替え対象外処置
           IF     (WRK-SRYSYUKBN           =   "409" )  AND
                  (SPA-COM-TNSSRYKBN (IDX) =   "40"  )
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-TNSSRYKBN (IDX) TO  WRK-SRYKBN
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN   (IDX)
               GO      TO      51011-SRYSYUKBN-KBN-EXT
           END-IF
      *
      *    皮下筋肉注射の振替えなしの時、そのまま
           IF     (IDX                     >   1     )  AND
                  (SPA-GMN-INPUTCD(IDX - 1)(1:4)   =   ".312" ) AND
                  (SPA-COM-TNSSRYKBN (IDX) =   "31"  )
               MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-TNSSRYKBN (IDX) TO  WRK-SRYKBN
               MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN   (IDX)
               GO      TO      51011-SRYSYUKBN-KBN-EXT
           END-IF
           IF      SPA-COM-SRYSYUKBN (IDX)  =   "312"
               IF     (IDX                  =   1   )  OR
                      (SPA-COM-SRYSYUKBN (IDX - 1)  NOT =   "312")
                   MOVE    SPA-COM-TNSSRYSYUKBN (IDX)
                                            TO  SPA-COM-SRYSYUKBN (IDX)
               END-IF
           END-IF
      *
           IF      SPA-COM-TNSSRYKBN (IDX) NOT =   ZERO
               MOVE    SPA-COM-TNSSRYKBN (IDX) TO  WRK-SRYKBN
           END-IF
      *    前回自費の時、元へ
           IF      SPA-COM-SRYSYUKBN (IDX) =   "960"  OR
                                               "950"  OR
                                               SPACE
      *H26.10
                                           OR  "961"
      *
               MOVE    SPA-COM-TNSSRYSYUKBN(IDX)
                                           TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPACE               TO  SPA-COM-KEIFLG    (IDX)
      *        画像診断の時、すべて"700"とする
               EVALUATE    SPA-COM-SRYSYUKBN (IDX)(1:1)
                   WHEN    "7"
                       MOVE    "700"       TO  SPA-COM-SRYSYUKBN(IDX)
               END-EVALUATE
      *!
      *         労災コードの円変換
               IF     (SPA-HKNKBN          =   1  )
                 AND  (SPA-COM-INPUTCD (IDX)(1:3)  =   "101")
                 AND  (SPA-COM-TNSSRYKBN(IDX)  NOT =   "97" )
                   IF     (SPA-COM-TENSIKIBETU(IDX)    =   "1"  )
      *                金額を点数へ置換える
                       COMPUTE SPA-COM-KISOTEN(IDX)    =   SPA-COM-TEN
                                                             (IDX)
                                                       /   10
                   END-IF
               END-IF
      *!
           END-IF
      *
      *    初診時ブラッシング料判定（Ｈ２５年７月からは手術あり）
           IF     (SPA-COM-INPUTCD(IDX)    =   "101400030" )
             AND  (SPA-SRYYMD              <   "20130701"  )
               IF     (IDX                 >   1  )  AND
                     ((SPA-GMN-INPUTCD(IDX - 1)(1:1) =   ".") OR
                      (SPA-COM-ZAIEND(IDX -  1)   =  SPACE  ))
                   IF      SPA-COM-SRYKBN(IDX - 1)   =   "40"
                                                     OR  "50"
                       MOVE    SPA-COM-SRYKBN(IDX - 1)   TO  WRK-SRYKBN
                       MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                               TO  WRK-SRYSYUKBN
                       MOVE    WRK-SRYSYUKBN   TO  SPA-COM-SRYSYUKBN
                                                               (IDX)
                       MOVE    WRK-SRYKBN      TO  SPA-COM-SRYKBN(IDX)
                   END-IF
               END-IF
           END-IF
      *!@
      *悪性腫瘍治療管理料チェック（Ｈ１８．４から）
           IF      SPA-SRYYMD              >=  "20060401"
               IF      SPA-COM-INPUTCD (IDX)   =   "113001210"  OR
                                                   "113001310"  OR
                                                   "113002110"
                   MOVE    1                   TO  FLG-AKUSEI-ARI
               END-IF
      *        悪性腫瘍と同一剤内の腫瘍検査はコメント扱い
               IF     (FLG-AKUSEI-ARI          =   1   )  AND
                      (SPA-COM-SRYKBN(IDX - 1) =   "13"    )  AND
                      (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )  AND
                      (SPA-COM-CDKBN-KBN (IDX)     =   "D"  )  AND
                      (SPA-COM-CDKBN-KBNNUM(IDX)   =   "009"  )
                   MOVE    SPACE               TO  SPA-COM-DATAKBN (IDX)
                   MOVE    SPA-COM-SRYKBN(IDX - 1) 
                                               TO  WRK-SRYKBN
                   MOVE    SPA-COM-SRYSYUKBN(IDX - 1)
                                               TO  WRK-SRYSYUKBN
                   MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (IDX)
                   MOVE    WRK-SRYSYUKBN       TO  SPA-COM-SRYSYUKBN
                                                                  (IDX)
               END-IF
           END-IF
      *!@
           .
       51011-SRYSYUKBN-KBN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画像診断の点滴チェックク処理
      *****************************************************************
       51013-731-CHKSYORI-SEC           SECTION.
      *
      *    画像診断の点滴チェック
           MOVE    SPA-COM-SRYKBN    (IDX - 1)
                                       TO  SPA-COM-SRYKBN    (IDX)
           MOVE    SPA-COM-SRYSYUKBN (IDX - 1)
                                       TO  SPA-COM-SRYSYUKBN (IDX)
           MOVE    SPA-COM-SRYKBN    (IDX)
                                       TO  WRK-SRYKBN
      *
           .
       51013-731-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間入力の時、前のコードチェック処理
      *****************************************************************
       510112-JIKAN-INP-SEC           SECTION.
      *
           EVALUATE    SPA-COM-INPUTCD (IDX)
      *        画像診断（時間外緊急院内画像診断加算）
               WHEN    "840000071"
                   IF      (IDX            =   1     )  OR 
                           (SPA-COM-INPUTCD (IDX - 1)
                                       NOT =   "170016010" )
                       MOVE    "0025"              TO  SPA-ERRCD
                   END-IF
      *        検査    （時間外緊急院内画像診断加算）
               WHEN    "840000044"
                   IF      (IDX            =   1     )  OR 
                           (SPA-COM-INPUTCD (IDX - 1)
                                       NOT =   "160000210" )
                       MOVE    "0025"              TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
           .
       510112-JIKAN-INP-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括対象検査変更処理
      *****************************************************************
       51013-KENSA-ZAI-CHK-SEC           SECTION.
      *
           EVALUATE    SPA-COM-HOUKNSKBN(IDX)
               WHEN    01
      *            血液化学検査の包括
                   MOVE    5             TO  WRK-KENSA-MAX
               WHEN    02
      *            内分泌学検査の包括
                   MOVE    3             TO  WRK-KENSA-MAX
               WHEN    03
      *            肝炎ウイルス関連の包括
                   MOVE    3             TO  WRK-KENSA-MAX
               WHEN    04
      *            腫瘍マーカーの包括
                   MOVE    2             TO  WRK-KENSA-MAX
               WHEN    05
                   MOVE    2             TO  WRK-KENSA-MAX
      *            ４以外の腫瘍マーカーの包括
               WHEN    06
      *            出血・凝固検査の包括
                   MOVE    3             TO  WRK-KENSA-MAX
               WHEN    07
      *            自己抗体検査の包括
                   MOVE    2             TO  WRK-KENSA-MAX
               WHEN    08
      *            内部泌負荷試験の包括（保留）
      ****         MOVE    1             TO  WRK-KENSA-MAX
      *H29.12      まとめ廃止
                   MOVE    ZERO          TO  WRK-KENSA-MAX
      *平成１５年９月
               WHEN    09
      *            ウイルス抗体価
                   MOVE    8             TO  WRK-KENSA-MAX
               WHEN    10
      *            グロブリンクラス別ウイルス抗体価
                   MOVE    2             TO  WRK-KENSA-MAX
      *平成１８年４月
               WHEN    11
      *            特異的ＩＧＥ
                   MOVE    2             TO  WRK-KENSA-MAX
      *平成３０年４月
               WHEN    12
      *            悪性腫瘍遺伝子検査
                   MOVE    2             TO  WRK-KENSA-MAX
      *R02.4
               WHEN    13
      *            悪性腫瘍遺伝子検査（容易なもの）
                   MOVE    2             TO  WRK-KENSA-MAX
               WHEN    14
      *            悪性腫瘍遺伝子検査（複雑なもの）
                   MOVE    2             TO  WRK-KENSA-MAX
               WHEN    OTHER
                   MOVE    ZERO          TO  WRK-KENSA-MAX
           END-EVALUATE
      *
           MOVE    ZERO                TO  WRK-KENSA-CNT
           PERFORM VARYING     IDX-KEN     FROM    1   BY  1
                   UNTIL       IDX-KEN     >   SPA-MAX-LINE
               IF      (SPA-COM-SRYKBN (IDX-KEN)   =   "60"
                                                   OR  "64"  ) AND
                       (SPA-COM-SRYSYUKBN (IDX-KEN)
                                               NOT =   "610")  AND
                       (SPA-COM-INPUTCD (IDX-KEN)(1:1)
                                                   =   "1"  )  AND
                       (SPA-COM-DATAKBN (IDX-KEN)  =   "1"  )
                   AND (SPA-COM-SET (IDX-KEN)  NOT =   "1"  )
                   AND (SPA-COM-KENTAICOMMENT (IDX-KEN)
                                                   =   0     )
                   MOVE    ZERO                TO  FLG-KEN-ERR
                   IF      (SPA-COM-KAIFLG (IDX-KEN)   =   "1" )  AND
                           (SPA-COM-KAISU  (IDX-KEN)   >   1   )
      *                回数が１以上で、１剤の時、包括対象外
                       IF     ((IDX-KEN                >   1   )  AND
                               (SPA-GMN-INPUTCD (IDX-KEN - 1)(1:1)
                                                       =   "." )) OR
                              ( WRK-KENSA-CNT          =   ZERO )
                           MOVE    1                   TO  FLG-KEN-ERR
                       END-IF
                   END-IF
                   IF     (FLG-KEN-ERR         =   ZERO )  AND
                          (SPA-COM-HOUKNSKBN(IDX)
                                               =   SPA-COM-HOUKNSKBN
                                                        (IDX-KEN) )
                       ADD     1                 TO  WRK-KENSA-CNT
                   END-IF
      *H27.10
      *            包括区分変更で終了
                   IF      FLG-KENSA-CHK       =   1
      *
                       IF     (WRK-KENSA-CNT       >   ZERO )  AND
                             ((SPA-COM-HOUKNSKBN(IDX)
                                           NOT =   SPA-COM-HOUKNSKBN
                                                        (IDX-KEN) )
                        OR    (SPA-COM-KAIFLG (IDX-KEN)   =   "1" ))
                           IF      IDX             >   IDX-KEN
                               MOVE    ZERO        TO  WRK-KENSA-CNT
                           ELSE
                               MOVE    SPA-MAX-LINE    TO  IDX-KEN
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *H29.6
      *        診療種別入力で終了
               IF     (SPA-GMN-INPUTCD (IDX-KEN)(1:1)  =   "."
                                                       OR  "*" )
                 AND  (SPA-NYUGAIKBN        =   1     )
                 AND  (FLG-KENSA-CHK        =   1    )
                    IF      IDX             >   IDX-KEN
                        MOVE    ZERO        TO  WRK-KENSA-CNT
                    ELSE
                        MOVE    SPA-MAX-LINE    TO  IDX-KEN
                    END-IF
               END-IF
      *
           END-PERFORM
           IF      WRK-KENSA-MAX       =   ZERO
               CONTINUE
           ELSE
               IF      WRK-KENSA-MAX       <=  WRK-KENSA-CNT
                   MOVE    1                   TO  FLG-KENSA-ARI
      *            包括対象
                   MOVE    SPA-COM-HOUKNSKBN(IDX)  TO ID2
                   IF      TBL-HOUKNARI (ID2)  =   ZERO
                       MOVE    1               TO  TBL-HOUKNARI (ID2)
                   END-IF
               END-IF
      *        入院の日付指定がある場合、対象外
      ******** IF     (SPA-NYUGAIKBN       =   "1" )  AND
               IF     (IDX                 <   SPA-MAX-LINE )  AND
                      (SPA-GMN-INPUTCD (IDX + 1)(1:1) =   "*" )
                 AND  (ID2                 >   ZERO  )
                   MOVE    9               TO  TBL-HOUKNARI (ID2)
      *            警告
                   MOVE    1               TO  LNK-SC93-ERRCD
               END-IF
           END-IF
      *
           IF    ((WRK-KENSA-MAX       >   1             )  AND
                  (WRK-KENSA-MAX       <=  WRK-KENSA-CNT )) OR
                  (SPA-GMN-INPUTCD(IDX-1)(1:1)   =   "." ) 
              OR  (SPA-COM-INPUTCD(IDX-1)   =   "099999903" )
              OR  (SPA-COM-INPUTCD(IDX-1)   =   "099999908" )
      *       会計照会で最終行の判定
              OR  (IDX                  =   1     ) 
              OR  ((LNK-SC93-PG         =   "J04" ) AND
                   (IDX             NOT =   SPA-GMN-MAX ))
               CONTINUE
           ELSE
               MOVE    2               TO  FLG-SYORI
           END-IF
      *
           .
       51013-KENSA-ZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算データチェック処理
      *****************************************************************
       51012-KASAN-CHK-SEC           SECTION.
      *
      *    加算データの時、前に手技料があること
      *        但し、画像診断の時、撮影部位コードがあること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    IDX   BY  -1
                   UNTIL      (IDX1        =   ZERO )  OR
                              (FLG-ARI     =   1    )
               IF      SPA-COM-INPUTCD(IDX)(1:3)   =   "170"
                   IF     (SPA-COM-INPUTCD(IDX1)(1:3)  =   "002" )  OR
                          (SPA-COM-DATAKBN(IDX1)   NOT =   "2"   )
                       MOVE    1               TO  FLG-ARI
                   END-IF
               ELSE
                   IF     (SPA-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                          (SPA-COM-DATAKBN(IDX1)   NOT =   "2" )
                       MOVE    1               TO  FLG-ARI
                   END-IF
               END-IF
               IF      FLG-ARI             =   ZERO
                   IF     (IDX1                =   1   )  OR
                          (SPA-COM-ZAIEND (IDX1 - 1)   =   "1" )
                       MOVE    1                   TO  IDX1
                   END-IF
               END-IF
           END-PERFORM
      *
      *    前の診療区分が、薬剤以外であること
           IF     (IDX                 >   1  )  AND
                  (SPA-COM-SRYKBN (IDX - 1)
                                       =   "21"  OR  "22"  OR  "23" )
               MOVE    ZERO                TO  FLG-ARI
           END-IF
      *
      *    小児科外来診察料の時間外のみはＯＫとする
           IF      FLG-ARI             =   ZERO
               IF      LNK-SC93-SYONIKA-ARI2   =   1
                   PERFORM 10052-SYONIKASRYCD-CHK-SEC
      *            再診の時間外の時、ＯＫとする
                   IF      WRK-SYONIKA-KBN     =   "4"
                       IF     (IDX             >   1 )  AND
                              (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                               =   ".130")
                           MOVE    1           TO  FLG-ARI
                       END-IF
                   END-IF
               END-IF
      *H26.4
      *        地域包括診療料時間外
               IF     (IDX                 >   1 )  AND
                      (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                           =   ".130")
                    IF      SPA-COM-TIMEKSNKBN (IDX)   NOT =   ZERO
                        MOVE    1           TO  FLG-ARI
                    END-IF
               END-IF
      *
      *        在宅指導料の時、点数加算はＯＫ
      ******** IF      LNK-SC93-NETAKIRI-ARI   =   1
                   IF     (WRK-SRYSYUKBN           =   "143" )  AND
                          (SPA-COM-SRYKBN (IDX)    =   "14"  )
                       IF      SPA-COM-TENSIKIBETU (IDX)   =   "1"  OR
                                                               "3"
                           MOVE    1               TO  FLG-ARI
                       END-IF
                   END-IF
      *********END-IF
      *        入院点滴で診療種別が入力されていること
               IF      SPA-NYUGAIKBN           =   "1"
                   IF      (SPA-COM-TNSSRYSYUKBN (IDX) =   "300" OR
                                                           "330" OR
                                                           "340" )  AND
                           (IDX                    >   1 )  AND
                           (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                                   =   ".330" 
                                                   OR  ".332"
                                                   OR  ".340" )
      *                年齢加算以外
                       IF      SPA-COM-INPUTCD(IDX)    =   "130009470"
                           MOVE    ZERO                TO  FLG-ARI
                       ELSE
                           MOVE    1                   TO  FLG-ARI
                       END-IF
                   END-IF
      *H28.5
               ELSE
      *            外来は、332,340のみ
                   IF      (SPA-COM-TNSSRYSYUKBN (IDX) =   "300" OR
                                                           "330" OR
                                                           "340" )  AND
                           (IDX                    >   1 )  AND
                           (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                                   =   ".332"
                                                   OR  ".340" )
      *                通則加算のみ
                       IF     (SPA-COM-CDKBN-KBN (IDX)     =   "G"  )
                         AND  (SPA-COM-CDKBN-KBNNUM(IDX)   =   "000")
                           MOVE    1                   TO  FLG-ARI
                       END-IF
                   END-IF
               END-IF
      *
      *        入院種別が入力されていること
               IF      SPA-NYUGAIKBN           =   "1"
                   IF      (SPA-COM-TNSSRYSYUKBN (IDX) =   "900")  AND
                           (IDX                    >   1 )  AND
                           (SPA-GMN-INPUTCD(IDX - 1)(1:4)
                                                   =   ".900"
                                                   OR  ".920" )
                       MOVE    1                   TO  FLG-ARI
                   END-IF
               END-IF
      *
      *        画像診断の加算のみ
               IF     (WRK-SRYSYUKBN           =   "704" )  AND
                      (SPA-COM-SRYKBN (IDX)    =   "70"  )
                   MOVE    1               TO  FLG-ARI
               END-IF
      *
      *        処置加算のみ
               IF     (WRK-SRYSYUKBN           =   "403" )  AND
                      (SPA-COM-TNSSRYKBN (IDX) =   "40"  )
                   MOVE    1               TO  FLG-ARI
               END-IF
      *!
      *        検査加算のみ
               IF     (WRK-SRYSYUKBN           =   "603" )  AND
                      (SPA-COM-TNSSRYKBN (IDX) =   "60"  )
                   MOVE    1               TO  FLG-ARI
               END-IF
      *        病理診断加算のみ
               IF     (WRK-SRYSYUKBN           =   "643" )  AND
                      (SPA-COM-TNSSRYKBN (IDX) =   "64"  )
                   MOVE    1               TO  FLG-ARI
               END-IF
      *
      *H28.1
      *        初診・再診加算のみ
               IF    ((WRK-SRYSYUKBN           =   "114" )  AND
                      (SPA-COM-TNSSRYKBN (IDX) =   "11"  ))
               OR    ((WRK-SRYSYUKBN           =   "124" )  AND
                      (SPA-COM-TNSSRYKBN (IDX) =   "12"  ))
               OR    ((WRK-SRYSYUKBN           =   "133" )  AND
                      (SPA-COM-TNSSRYKBN (IDX) =   "13"  ))
                   MOVE    1               TO  FLG-ARI
               END-IF
           END-IF
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(IDX)
           ELSE
               MOVE    SPA-COM-SRYKBN (IDX - 1)
                                           TO  SPA-COM-SRYKBN (IDX)
               MOVE    SPA-COM-SRYSYUKBN (IDX - 1)
                                           TO  SPA-COM-SRYSYUKBN (IDX)
               MOVE    SPA-COM-ZAIEND (IDX - 1)
                                           TO  SPA-COM-ZAIEND (IDX)
               MOVE    SPACE               TO  SPA-COM-ZAIEND (IDX - 1)
           END-IF
      *
           .
       51012-KASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    小児科外来診察料科チェックク処理
      *****************************************************************
       10052-SYONIKASRYCD-CHK-SEC                SECTION.
      *
      *    診察料検索
           SET     TBL-SYO             TO  1
           SEARCH  TBL-SYONIKA-OCC     VARYING   TBL-SYO
               AT  END
                   MOVE    SPACE           TO  WRK-SYONIKA-KBN
                WHEN   SPA-COM-INPUTCD (IDX)
                                           =   TBL-SYONIKA-SRYCD
                                                      (TBL-SYO)
                   MOVE    TBL-SYONIKA-KBN (TBL-SYO)
                                           TO  WRK-SYONIKA-KBN
           END-SEARCH
           .
       10052-SYONIKASRYCD-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査ソート編集処理
      *****************************************************************
       200-KENSA-SORT-SEC              SECTION.
      *
      *    画面ＳＰＡ領域ワーク
           INITIALIZE                  WRK-GMN-REC
           INITIALIZE                  HZN-GMN-REC
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                  SPA-GMN-REC
           MOVE    ZERO                TO  IDX-TBL
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (WRK-COM-INPUTCD(IDX)    =   SPACE ) AND
                      (WRK-GMN-INPUTCD(IDX)    =   SPACE )
                   CONTINUE
               ELSE
                   IF     (WRK-COM-SRYKBN (IDX)    =   "60"
                                                   OR  "64" ) AND
                          (WRK-COM-HOUKNSKBN(IDX)  NOT =  ZERO)
                   AND    (WRK-COM-SRYSYUKBN(IDX)  NOT =  "610")
                   AND    (WRK-COM-SET      (IDX)  NOT =   "1"  )
      *            入院の回数行
                   AND    (WRK-GMN-INPUTCD(IDX)(1:1)   NOT =   "*")
                       IF     (IDX                 >   1          )  AND
                              (WRK-GMN-INPUTCD (IDX - 1)(1:4)
                                                       =   ".600" OR
                               WRK-COM-ZAIEND (IDX - 1)
                                                       =   "1"   )  AND
                              (WRK-COM-KAISU   (IDX)   >  1 ) AND
                              (WRK-COM-KAIFLG  (IDX)   =  "1")
                           ADD     1               TO  IDX-TBL
                           MOVE    WRK-GMN-TBL (IDX)
                                               TO  SPA-GMN-TBL(IDX-TBL)
                           MOVE    SPACE       TO  WRK-GMN-TBL (IDX)
                       ELSE
                           PERFORM 2001-KENSA-SET-SEC
                       END-IF
                   ELSE
                       ADD     1               TO  IDX-TBL
                       MOVE    WRK-GMN-TBL (IDX)
                                               TO  SPA-GMN-TBL(IDX-TBL)
                       MOVE    SPACE           TO  WRK-GMN-TBL (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-MAX     NOT =   IDX-TBL
               MOVE    IDX-TBL         TO  SPA-GMN-MAX
           END-IF
      *
           .
       200-KENSA-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査ソート編集処理
      *****************************************************************
       2001-KENSA-SET-SEC              SECTION.
      *
           INITIALIZE                  HZN-SCR-REC
           MOVE    IDX                 TO  IDX-HZN
           MOVE    WRK-COM-HOUKNSKBN(IDX)
                                       TO  WRK-HOUKNSKBN
           MOVE    WRK-COM-HKNCOMBI (IDX)
                                       TO  WRK-HKNCOMBI
           MOVE    WRK-COM-SRYKA (IDX)
                                       TO  WRK-SRYKA
      *    包括対象検査数の判定
           MOVE    WRK-HOUKNSKBN       TO  ID2
           IF      TBL-HOUKNARI (ID2)  =   ZERO
                                       OR  9
               ADD     1               TO  IDX-TBL
               MOVE    WRK-GMN-TBL (IDX)   TO  SPA-GMN-TBL(IDX-TBL)
               MOVE    SPACE               TO  WRK-GMN-TBL (IDX)
           ELSE
               PERFORM 20011-KENSA-SYORI-SEC
           END-IF
           .
       2001-KENSA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括検査ソート処理
      *****************************************************************
       20011-KENSA-SYORI-SEC              SECTION.
      *
           PERFORM VARYING     IDX-2   FROM    IDX   BY  1
                   UNTIL       IDX-2   >   SPA-MAX-LINE
               MOVE    ZERO            TO  FLG-KENSA-OK
               IF     (WRK-COM-INPUTCD(IDX-2)    =   SPACE  )  AND
                      (WRK-GMN-INPUTCD(IDX-2)    =   SPACE  )
                   MOVE    1                TO  FLG-KENSA-OK
               END-IF
               IF      (IDX-2               >   1          )  AND
                       (WRK-COM-SRYKBN (IDX-2)    =   "60" )  AND
                       (WRK-GMN-INPUTCD (IDX-2 - 1)(1:4)
                                                   =   ".600" OR
                        WRK-COM-ZAIEND  (IDX-2 - 1) =  "1"   )  AND
                       (WRK-COM-KAISU   (IDX-2)    >  1 ) AND
                       (WRK-COM-KAIFLG  (IDX-2)    =  "1")
                   MOVE    1                TO  FLG-KENSA-OK
               END-IF
               IF      (WRK-COM-SRYSYUKBN (IDX-2) =   "610" )
                  OR   (WRK-COM-SET      (IDX-2)  =   "1"  )
                   MOVE    1                TO  FLG-KENSA-OK
               END-IF
               IF      FLG-KENSA-OK         =   ZERO
                   IF     (WRK-COM-SRYKBN (IDX-2)    =   "60" )  AND
                          (WRK-COM-HOUKNSKBN(IDX-2)  =   WRK-HOUKNSKBN)
                     AND  (WRK-COM-HKNCOMBI (IDX-2)  =   WRK-HKNCOMBI )
                     AND  (WRK-COM-SRYKA    (IDX-2)  =   WRK-SRYKA    )
                       CONTINUE
                   ELSE
                       MOVE    1                TO  FLG-KENSA-OK
                   END-IF
               END-IF
               IF      FLG-KENSA-OK         =   ZERO
      *            前の行が診療種別で削除できるかどうかの判定
                   IF     ((IDX-2              >   1  )  AND
                           (WRK-GMN-INPUTCD(IDX-2 - 1)(1:4)
                                               =   ".600")) AND
                           (IDX-2              >   IDX   )
                       MOVE    SPACE           TO  WRK-GMN-TBL
                                                       (IDX-2 - 1)
                   END-IF
      *
                   ADD     1               TO  IDX-TBL
                   MOVE    WRK-GMN-TBL (IDX-2)
                                               TO  SPA-GMN-TBL(IDX-TBL)
                   IF     (SPA-COM-KAISU  (IDX-TBL)  >  1 )
                     AND  (SPA-COM-KAIFLG (IDX-TBL)  =  "1")
                       CONTINUE
                   ELSE
                       IF      SPA-COM-KAIFLG (IDX-TBL)  =   "1"
                           MOVE    SPACE           TO  WRK-INPUTCD
                           STRING  SPA-GMN-INPUTCD(IDX-TBL)
                                                   DELIMITED  BY  "*"
                                                   INTO    WRK-INPUTCD
                           END-STRING
                           MOVE    WRK-INPUTCD TO  SPA-GMN-INPUTCD
                                                            (IDX-TBL)
                       END-IF
                       MOVE    SPACE           TO  SPA-COM-KAIFLG
                                                              (IDX-TBL)
                       MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                              (IDX-TBL)
                   END-IF
                   MOVE    SPACE           TO  WRK-GMN-TBL (IDX-2)
      *            次の行のコメントを一緒に移動する
                   COMPUTE IDX-5               =   IDX-2  +  1
                   PERFORM VARYING     IDX-4   FROM    IDX-5   BY  1
                           UNTIL       IDX-4   >   SPA-MAX-LINE
                       IF    ((WRK-COM-INPUTCD (IDX-4) (1:1)
                                                   =   "8"    )  OR
                              (WRK-COM-INPUTCD (IDX-4) (1:3)
                                                   =   "008"  ))
                       AND    (WRK-COM-KAISU  (IDX-4)  <= 1 )
                           ADD     1               TO  IDX-TBL
                           MOVE    WRK-GMN-TBL (IDX-4)
                                                   TO  SPA-GMN-TBL
                                                             (IDX-TBL)
                           MOVE    SPACE           TO  WRK-GMN-TBL
                                                             (IDX-4)
                           MOVE    IDX-4           TO  IDX-2
      *                    回数削除
                           IF      SPA-COM-KAIFLG (IDX-TBL)  =   "1"
                               MOVE    SPACE           TO  WRK-INPUTCD
                               STRING  SPA-GMN-INPUTCD(IDX-TBL)
                                                   DELIMITED  BY  "*"
                                                   INTO    WRK-INPUTCD
                               END-STRING
                               MOVE    WRK-INPUTCD TO  SPA-GMN-INPUTCD
                                                            (IDX-TBL)
                           END-IF
                           MOVE    SPACE           TO  SPA-COM-KAIFLG
                                                              (IDX-TBL)
                           MOVE    SPACE           TO  SPA-COM-ZAIEND
                                                              (IDX-TBL)
                       ELSE
                           MOVE    SPA-MAX-LINE    TO  IDX-4
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       20011-KENSA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分変更時編集処理
      *****************************************************************
       1003-SAIHENSYU-SEC              SECTION.
      *
           MOVE    SPA-COM-SURYO2 (IDX) TO  SPA-COM-SURYO (IDX)
           MOVE    SPA-COM-KAISU2 (IDX) TO  SPA-COM-KAISU (IDX)
      *
           .
       1003-SAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一検査削除処理
      *****************************************************************
       300-KENSA-DELETE-SEC                  SECTION.
      *
           INITIALIZE                  WRK-SCR-REC
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *
      *    同一検査を削除
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
                             ((SPA-GMN-INPUTCD (IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD (IDX) =   SPACE ))
               IF     (SPA-COM-SRYKBN (IDX)  =   "60"  )  AND
                      (SPA-COM-INPUTCD(IDX)(1:1)  =   "1" )  AND
                      (SPA-COM-DATAKBN(IDX)  =   "1"   )
      *        逓減以外
               AND    (SPA-COM-TEIGENKBN (IDX)   NOT =   1    )
      *        包括対象以外
               AND    (SPA-COM-SRYSYUKBN (IDX)   NOT =   "610")
      *        包括分
               AND    (SPA-COM-HOUKNSKBN(IDX)    NOT =   ZERO )
      *        検体検査コメント以外
               AND    (SPA-COM-KENTAICOMMENT (IDX)  =   ZERO )
                   PERFORM VARYING     IDX-2  FROM    IDX     BY  1
                           UNTIL       IDX-2      >   SPA-MAX-LINE
                       IF     (IDX-2       NOT =   IDX )  AND
                              (SPA-COM-SRYSYUKBN (IDX-2)
                                           NOT =   "610"  )  AND
                              (SPA-COM-INPUTCD(IDX)
                                               =   SPA-COM-INPUTCD
                                                        (IDX-2))
      *                 検体検査コメントの判定
                         AND  (WRK-COM-KENGRP(IDX)
                                             =   WRK-COM-KENGRP(IDX-2))
                           MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                        (IDX-2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM 
           INITIALIZE                  WRK-SCR-REC
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  IDX-TBL
      *    同一検査を削除
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE     )
               IF      WRK-GMN-INPUTCD (IDX)   =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-TBL
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-GMN-TBL (IDX)   TO  SPA-GMN-TBL (IDX-TBL)
               END-IF
           END-PERFORM
           .
       300-KENSA-DELETE-EXT.
           EXIT.
