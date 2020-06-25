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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCKT100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 外来まとめ入力
      *  コンポーネント名  : 画面一括処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/07/30  リハビリ発症日編集対応
      *  04.06.00    NACL-多々納  10/07/01  残量廃棄自動算定対応
      *  04.07.00    NACL-多々納  15/03/XX  Ｈ２７年４月改正追加
      *  04.08.00    NACL-多々納  16/05/XX  一般名表示対応
      *  04.08.00    NACL-多々納  16/11/XX  投薬投与量チェック区分対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
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
           03  FLG-OK                  PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ZAIEND              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-DELFLG              PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-TUIKA               PIC 9(01).
           03  FLG-SONOTA              PIC 9(01).
           03  FLG-NAIFUTAN            PIC 9(01).
      *    剤チェック対象外
           03  FLG-TAISYO              PIC 9(01).
      *    回数入力なし
           03  FLG-KAISU               PIC 9(01).
      *    包括診療用
           03  FLG-HKTCHK              PIC 9(01).
           03  FLG-HOUKATU-ZAI         PIC 9(01).
           03  FLG-AUTOERR             PIC 9(01).
      *
           03  FLG-IKKATU              PIC 9(01).
      *
           03  FLG-SANTEI-KBN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-Z                   PIC 9(04).
           03  IDX-KAI                 PIC 9(04).
           03  IDX-KAI2                PIC 9(04).
           03  IDK1                    PIC 9(04).
           03  IDK2                    PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDN1                    PIC 9(04).
      *
           03  IDX-HKN                 PIC 9(04).
           03  IDX-SN                  PIC 9(04).
           03  IDX-SN2                 PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDD                     PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *    入院日
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SRYDD               PIC 9(02).
      *   入院回数・日数
           03  WRK-NYUIN-AREA.
               05  WRK-NYU-KAI        PIC 9(03).
               05  WRK-NYUINDAY       PIC 9(01)    OCCURS  31.
           03  WRK-NYUIN-CHK-AREA.
               05  WRK-NYUIN-KAI       PIC 9(03).
           03  IDX-N2                  PIC 9(02).
           03  WRK-NYU-KAISU           PIC 9(03).
           03  WRK-KAISU               PIC 9(04).
           03  WRK-KAISU-GOK           PIC 9(04).
           03  WRK-GOKEI-9             PIC 9(10).
           03  WRK-GOKEI-9S            PIC S9(10).
      *    消炎鎮痛回数
           03  WRK-SYOEN-CNT           PIC 9(03).
      *    診療選択より
           03  WRK-DRCD                PIC X(05).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-ZAINUM              PIC 9(08).
      *    急性発生日
           03  WRK-KYUSEI-YMD          PIC X(08).
           03  WRK-KYUSEI-END          PIC X(08).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
      *
           03  WRK-KEI-ERRCD           PIC X(04).
           03  WRK-KEI-ERRMSG2         PIC X(80).
           03  WRK-ERR-ERRCD           PIC X(04).
           03  WRK-ERR-ERRMSG2         PIC X(80).
           03  WRK-KIDCD               PIC X(04).
      *    時間外フラグ
           03  WRK-TIMEFLG             PIC X(01).
           03  WRK-KEI-IDX             PIC 9(04).
           03  WRK-MAX                 PIC 9(04).
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(04).
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
      *    入力時表示用
           03  WRK-HEN-INPUTCD         PIC X(54).
      *    診察料有無
           03  WRK-SINSATU-ARI         PIC 9(01).
      *    同日初診フラグ
           03  WRK-DOUSIN              PIC 9(01).
      *    在宅療養指導料算定
           03  WRK-ZAITAKU-CNT         PIC 9(04).
      *    算定回数
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SANTEICNT           PIC 9(04).
      *
           03  WRK-KAIFLG2             PIC X(01).
      *@
      *    今回リハビリ発生日
           03  WRK-KYUSEI-INP-G.
               05  WRK-KYUSEI-IN-G         OCCURS   7.
                   07  WRK-KYUSEI-ST       PIC X(01).
                   07  WRK-KYUSEI-ED       PIC X(01).
               05  WRK-TEIGEN-IN           PIC X(01).
      *!!!!!!!!!!!!!!!!!
      *H27.1
      *    一般・銘柄名記載区分（処方毎）
           03  WRK-INGIKSN-KBN-G.
               05  WRK-INGIKSN-KBN-TBL      OCCURS   20.
                   07  WRK-INGKSN-SRYKA        PIC X(02).
                   07  WRK-INGKSN-HKNCOMBI     PIC X(04).
                   07  WRK-INGKSN-KBN          PIC X(01).
           03  IDX-M                      PIC 9(04).
      *!!!!!!!!!!!!!!!!!
      *
      *    自動加算テーブル
       01  WRK-KASAN-AREA.
           03  WRK-KASAN-TBL                       OCCURS   60.
               05  WRK-KSN-HKNCOMBI        PIC X(04).
               05  WRK-KSN-SRYKA           PIC X(02).
               05  WRK-KSN-SRYSYUKBN       PIC X(03).
               05  WRK-KSN-SRYKBN          PIC X(02).
               05  WRK-KSN-SRYCD           PIC X(09).
               05  WRK-KSN-ATAI-G.
                   07  WRK-KSN-ATAI        PIC X(08)   OCCURS  5.
               05  WRK-KSN-TENSU           PIC 9(08).
               05  WRK-KSN-SURYO           PIC 9(05)V9(05).
               05  WRK-KSN-ZAINUM          PIC 9(08).
               05  WRK-KSN-ZAIEND          PIC X(01).
               05  WRK-KSN-INPUTCD         PIC X(54).
               05  WRK-KSN-AUTOKBN         PIC X(01).
               05  WRK-KSN-HKTKBN          PIC 9(01).
           03  WRK-KSN-IDX             PIC 9(04).
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *    保存ＳＰＡ領域ワーク
       01  MAE-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //MAE-//.
      *
       01 WRK-HOZ-IDX                  PIC 9(04).
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"      REPLACING
      *                              //SPA-// BY //WKSPA-//.
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *H28.4
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    点数マスタ−（パラメタ用）
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    投与量等チェックパラメタ
           COPY    "CPORCSC98.INC".
      *
      *    診察料自動発生パラメタ（外来まとめ）
           COPY    "CPORCSCKT10S.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCKTWKSRY.INC".
      *
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *    全角変換
           COPY    "CPORCSKANACHK.INC".
      *
      *H28.3
      *    院外処方名編集パラメタ
           COPY    "CPORCSCGENNAME.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSC100.INC".
      *
      *    画面ＳＰＡ領域
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "KT01SCR-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSC100AREA
           SPA-AREA
           SPA-KT01
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           EVALUATE    ORCSC100-KBN
               WHEN    "1"
               WHEN    "3"
                   PERFORM 100-GMN-SYORI-SEC
               WHEN    "2"
                   PERFORM 300-TOUROKU-SYORI-SEC
      *H28.4   中途終了
               WHEN    "4"
                   PERFORM 100-GMN-SYORI-SEC
                   IF     (SPA-ERRCD       =   SPACE )  OR
                          (SPA-KIDCD       =   SPACE )
                       PERFORM 200-TOUYAKUCHK-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           .
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       100-GMN-SYORI-SEC               SECTION.
      *
      *    空白行をなくす
           PERFORM 420-SYOKYO-SEC
      *
      *    診療行為画面テーブル再編集
           PERFORM 5101-TBLSAIHEN-SEC
      *    悪性腫瘍検査一覧へ
           IF      SPA-AKUSEI-FLG      =   1
               GO    TO  100-GMN-SYORI-EXT
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               OR (ORCSC100-INIT       =   1      )
               GO      TO      100-GMN-SYORI-EXT
           END-IF
      *
      *    登録前の全体のチェックを行う
           INITIALIZE                  ORCSC00AREA
           MOVE    "2"                 TO  LNK-ORCSC-KBN
      *H28.4
      *    登録時
           IF      ORCSC100-TOROKU     =   "1"
               MOVE    "1"                 TO  LNK-ORCSC-KBN2
           END-IF
           PERFORM 520-ALL-CHEK-SEC
      *    更新前に確認画面を表示する
           IF      ORCSC100-TOROKU     =   SPACE
               IF      SPA-KIDCD           =   "0102"
                   MOVE    SPACE           TO  SPA-KIDCD
               END-IF
           END-IF
      *
      *    診療行為の追加、修正時、再編集へ
           IF      FLG-TUIKA           =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
      *        診療行為画面テーブル再編集
               PERFORM 5101-TBLSAIHEN-SEC
           END-IF
      *
           .
       100-GMN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為画面テーブル再編集処理
      *****************************************************************
       5101-TBLSAIHEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-SYOSIN
      *
      *    特定の診療行為の有無
           IF      SPA-HKNCOMBINUM NOT =   "9999"
               PERFORM 6100-TOKUTEICHK-SEC
           END-IF
      *
      *    剤分離処理
           INITIALIZE                  ORCSC93AREA
           IF      ORCSC100-KBN        =   "3"
               MOVE    "K02"               TO  LNK-SC93-PG
           ELSE
               MOVE    "KT01"              TO  LNK-SC93-PG
           END-IF
           IF      SPA-NYUGAIKBN       =   "2"
      *        小児科外来診療科算定有り
               MOVE    SPA-SYONIKA-ARI2    TO  LNK-SC93-SYONIKA-ARI2
      *        寝たきり在宅科算定有り
               IF      (SPA-NETAKIRI-ARI   NOT =   ZERO )  OR
                       (SPA-NETAKIRI-ARI2  NOT =   ZERO )
                   MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
               END-IF
           END-IF
      *    同一検査削除
           IF      ORCSC100-KENSADEL   =   1
               MOVE    1               TO  LNK-SC93-DELSYORIFLG
           END-IF
      *    残量廃棄
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-SC93-HAIKIKBN-FLG
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF     (SPA-ERRCD       NOT =   SPACE )
              AND (ORCSC100-INIT       =   ZERO )
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *
      *    特定の診療行為の有無
           IF      SPA-HKNCOMBINUM NOT =   "9999"
               IF     (ORCSC100-KBN        =   "3" )
                OR    (FLG-IKKATU          =   ZERO )
                   PERFORM 6200-TOKUTEIHANTEI-SEC
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE)
              AND (ORCSC100-INIT       =   ZERO )
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *
      *    剤ごとに回数を入れ直す（リハビリ等同時回数チェックのため）
           MOVE    ZERO                TO  FLG-SONOTA
           MOVE    ZERO                TO  FLG-NAIFUTAN
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                      WRK-NYUIN-AREA
           MOVE    SPACE               TO  WRK-KYUSEI-YMD
           MOVE    SPACE               TO  WRK-KYUSEI-END
           MOVE    SPACE               TO  WRK-KYUSEI-INP-G
      *    同日初診フラグ
           MOVE    ZERO                TO  WRK-DOUSIN
           MOVE    ZERO                TO  SPA-HKTSANTEI-08
           PERFORM VARYING     IDX     FROM    SPA-MAX-LINE BY  -1
                   UNTIL       IDX     <   1
               IF     (SPA-GMN-INPUTCD (IDX)   =   SPACE )  AND
                      (SPA-COM-INPUTCD (IDX)   =   SPACE )
                   CONTINUE
               ELSE
      *            回数再編集
                   PERFORM 51011-KAISU-SET1-SEC
      *            その他の算定有無
                   IF      SPA-COM-SRYKBN (IDX)    =   "80"
                       MOVE    1               TO  FLG-SONOTA
      *                各急性発生日判定
                       IF      SPA-COM-INPUTCD(IDX)(1:4)   =   "0998"
                           PERFORM 51019-KYUSEI-SEC
                       END-IF
                   END-IF
      *            内分泌負担試験有無
                   IF     (SPA-COM-SRYKBN (IDX)    =   "60"  )  AND
                          (SPA-COM-DATAKBN(IDX)    =   "1"   )  AND
                          (SPA-COM-HOUKNSKBN(IDX)  =   08    )
                       MOVE    1               TO  FLG-NAIFUTAN
                   END-IF
      *            同日初診有無（通院精神療法の為）
                   IF     (SPA-COM-SRYKBN (IDX)    =   "11"  )  AND
                          (SPA-COM-HKNCOMBI(IDX)   NOT =   "9999") AND
                          (SPA-COM-INPUTCD(IDX)    =   "111011810"
      *H25.4                        同日初診（紹介なし）
                                                   OR  "111012610"
                                                   OR  "101110040")
                       MOVE    1               TO  WRK-DOUSIN
                   END-IF
      *            包括算定（剤）入力判定
                   IF     (SPA-COM-HKNCOMBI(IDX)   NOT =   "9999") AND
                          (SPA-COM-INPUTCD(IDX)    =   "099999908" )
                       IF     (SPA-COM-SRYKBN (IDX)    =   "95"
                                                       OR  "96"
                                                       OR  "98"
                                                       OR  "99"    )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  SPA-HKTSANTEI-08
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    算定回数・併用算定チェック
           IF      ORCSC100-KBN        =   "3"
            OR    (FLG-IKKATU          =   ZERO )
               PERFORM 51017-SANTEI-CHEK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)
                  AND (ORCSC100-INIT       =   ZERO )
                   GO      TO      5101-TBLSAIHEN-EXT
               END-IF
           END-IF
      *
      *H27.1
      *    後発品変更可・不可処方毎処理
           IF     (ORCSC100-KBN        =   "3"  )
            OR    (FLG-IKKATU          =   ZERO )
               PERFORM 51019-INGIKSN-HEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE)
                   GO      TO      5101-TBLSAIHEN-EXT
               END-IF
           END-IF
      *
      *
      *!!
      *    内分泌負担試験編集
           IF      FLG-NAIFUTAN        =   1
               INITIALIZE                  ORCSSANFUKAAREA
               MOVE    2                   TO  LNK-SANFUKA-SYORI
               MOVE    "KT1"               TO  LNK-SANFUKA-PG
               MOVE    SPA-SRYYMD          TO  LNK-SANFUKA-SRYYMD
      *        訂正時の診療コード
               MOVE    SPA-NAI-FUKATEN-MAX TO  LNK-SANFUKA-TEN-MAX
               MOVE    SPA-NAI-FUKATEN-ALL TO  LNK-SANFUKA-MSANTESU
               MOVE    SPA-NAI-FUKATEN-DAY TO  LNK-SANFUKA-MSANTEID
               MOVE    SPA-NAI-FUKATEN-DEL TO  LNK-SANFUKA-MSANTESU-DEL
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
      *        上限点数
               MOVE    LNK-SANFUKA-TEN-MAX     TO  SPA-NAI-FUKATEN-MAX
      *        当月分合計
               MOVE    LNK-SANFUKA-MSANTESU    TO  SPA-NAI-FUKATEN-ALL
      *        当月最終算定日
               MOVE    LNK-SANFUKA-MSANTEID    TO  SPA-NAI-FUKATEN-DAY
      *        訂正分合計
               MOVE    LNK-SANFUKA-MSANTESU-DEL
                                               TO  SPA-NAI-FUKATEN-DEL
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (LNK-SANFUKA-ERRMSG  NOT =   SPACE)
                   MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG2
                   MOVE    "1061"              TO  SPA-ERRCD
               END-IF
               IF     (SPA-ERRCD(1:1)      =   "K"   )  AND
                      (LNK-SANFUKA-ERRMSG  NOT =   SPACE)
                   MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG2
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE)
              AND (ORCSC100-INIT       =   ZERO )
               GO      TO      5101-TBLSAIHEN-EXT
           END-IF
      *!!
      *    剤ごとの処理
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HZN-SCR-AREA
           INITIALIZE                  WRK-HOZ-IDX
           MOVE    ZERO                TO  WRK-TIMEFLG
           MOVE    SPACE               TO  WRK-KEI-ERRCD
           MOVE    SPACE               TO  WRK-ERR-ERRCD
           MOVE    ZERO                TO  WRK-KEI-IDX
           MOVE    ZERO                TO  WRK-GOKEI-9
           MOVE    ZERO                TO  WRK-GOKEI-9S
           MOVE    SPACE               TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-SRYKA
      *    特定薬剤治療管理加算の初回算定日クリア
           MOVE    SPACE               TO  SPA-TOKTEI-YMD
      *    他保険受診ありクリア
           MOVE    ZERO                TO  SPA-TAHOKEN-FLG
           MOVE    ZERO                TO  WRK-KAISU-GOK
      *    消炎鎮痛処置当月内算定
           MOVE    SPA-SYOEN-CNT       TO  WRK-SYOEN-CNT
           MOVE    SPACE               TO  WRK-KIDCD
      *    剤別初回編集
           INITIALIZE                  ORCSC00AREA
      *    リハビリ初回
           MOVE    SPACE               TO  SPA-RIGAKU-INIT
      *    リハビリ算定回数
           MOVE    WRK-KYUSEI-INP-G    TO  SPA-RIGAKU-REC (1:15)
      *
           MOVE    SPACE               TO  WRK-KAIFLG2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE      )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        回数の入力があるかのチェック
               PERFORM 51011-KAISU-CHK-SEC
      *        剤ごとの算定処理で変更される加算データは対象としない
      *          但し、コメントの自動追加分は、そのまま
      *              （注射の廃棄コメントははずす）
               MOVE    ZERO                TO  FLG-TAISYO
               IF      SPA-COM-AUTOKBN (IDX)    =   "2"  OR  "3"
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)
                                               NOT =   "8"   )   OR
                          ((SPA-COM-SRYKBN  (IDX)  =   "31" OR
                                                       "32" OR
                                                       "33")   AND
                           (SPA-COM-INPUTCD (IDX)  =   "099309901" ))
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            薬剤の自動追加コメントは対象外
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)   =  "8" ) AND
                          ( SPA-COM-SRYKBN  (IDX)    =   "21"
                                                     OR  "22"
                                                     OR  "33"  )
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *H26.10      （減）（精減）の自動コメントは対象外
                   IF     ( SPA-COM-INPUTCD (IDX)  =   "820000047"
                                                   OR  "820000166"
                                                   OR   CONS-SPCM-SRYCD)
                     AND  ( SPA-COM-AUTOKBN (IDX)  =   "3"        )
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *
      *            自動追加コメントで入力値ありの時対象外
                   IF      (SPA-COM-INPUTCD (IDX)(1:2)   =  "84")  OR
                           (SPA-COM-INPUTCD (IDX)(1:4)   =  "0084")
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
      *            自動追加で画像診断の時対象外
                   IF      (SPA-COM-SRYKBN(IDX) NOT =  "95" AND
                                                       "96" ) AND
                           (SPA-COM-INPUTCD (IDX)   =  "170012070"
                                                   OR  "170020470")
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
               END-IF
      *
               IF      FLG-TAISYO              =   ZERO
                   ADD     1               TO  IDX-Z
                   MOVE    SPA-COM-TBLREC (IDX)
                                           TO  WRK-COM-TBLREC(IDX-Z)
                   MOVE    SPA-GMN-TBLREC (IDX)
                                           TO  WRK-GMN-TBLREC(IDX-Z)
      *            自費以外の計をクリアする
                   IF      WRK-COM-SRYKBN (IDX-Z)   =   "95"  OR "96"
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  WRK-COM-KEI(IDX-Z)
                       MOVE    ZERO            TO  WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                    END-IF
               END-IF
      *        回数編集
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
                   IF     (SPA-COM-KAIFLG (IDX)    =   "1" )
                      AND (WRK-KAISU               =   ZERO)
                        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                 外用薬の時、入力回数を
                       IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
                              (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                              (SPA-COM-KAISU (IDX) =   1    )
                           MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                       END-IF
                   END-IF
               END-IF
      *        入院の回数合計
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
                   ADD     SPA-COM-NYU-KAI (IDX)
                                               TO  WRK-KAISU-GOK
               ELSE
                   ADD     WRK-KAISU           TO  WRK-KAISU-GOK
               END-IF
      *        外来の保険・診療科設定
               IF      SPA-NYUGAIKBN       =   "2"
                   IF      WRK-HKNCOMBI        =   SPACE
                       MOVE    SPA-COM-HKNCOMBI(IDX)
                                               TO  WRK-HKNCOMBI
                   END-IF
                   IF      WRK-SRYKA           =   SPACE
                       MOVE    SPA-COM-SRYKA   (IDX)
                                               TO  WRK-SRYKA
                   END-IF
               END-IF
      *
               IF     (IDX                 =   SPA-GMN-MAX   )  OR
                      (SPA-COM-ZAIEND (IDX)
                                           =   "1"           )
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   MOVE    SPACE               TO  WRK-SRYKA
               END-IF
           END-PERFORM
      *
      *検査一覧表示へ
           IF      WRK-ERR-ERRCD       =   "AK98"
               MOVE    SPACE               TO  WRK-ERR-ERRCD
               MOVE    1                   TO  SPA-AKUSEI-FLG
           ELSE
               MOVE    ZERO                TO  SPA-AKUSEI-FLG
           END-IF
      *
      *
      *    エラーの再セットを行う
           IF      WRK-ERR-ERRCD   NOT =   SPACE
               MOVE    WRK-ERR-ERRCD       TO  SPA-ERRCD
               MOVE    WRK-ERR-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *    警告メッセージの時、ＳＰＡの再セットを行う
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (WRK-KEI-ERRCD   NOT =   SPACE)
               MOVE    WRK-KEI-ERRCD       TO  SPA-ERRCD
               MOVE    WRK-KEI-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-KIDCD           TO  SPA-KIDCD
           END-IF
      *@
      *    投薬附加情報チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (WRK-ERR-ERRCD       =   SPACE )  AND
                  (WRK-KEI-ERRCD       =   SPACE )  AND
                  (WRK-KIDCD           =   SPACE )
               PERFORM 51022-TENSUPLUS2-CHK02-SEC
           END-IF
      *@
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (WRK-HZN-GMN-INPUTCD (IDX)   =   SPACE)  AND
                      (WRK-HZN-COM-INPUTCD (IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
                   MOVE    WRK-HZN-GMN-TBLREC (IDX)
                                               TO  SPA-GMN-TBLREC
                                                         (SPA-GMN-MAX)
                   MOVE    WRK-HZN-COM-TBLREC (IDX)
                                               TO  SPA-COM-TBLREC
                                                         (SPA-GMN-MAX)
      *@
      *            検査一覧表示へ
                   IF      SPA-AKUSEI-FLG      =   1
                       IF    SPA-COM-CUR (SPA-GMN-MAX)  =  "A"
                           MOVE    SPA-GMN-MAX     TO  SPA-AKUSEI-IDX
                           MOVE    SPACE           TO  SPA-COM-CUR
                                                       (SPA-GMN-MAX)
                       END-IF
                    END-IF
               END-IF
           END-PERFORM
      *
           COMPUTE SPA-GMN-GOKEI       =   WRK-GOKEI-9
                                       -   WRK-GOKEI-9S
      *
           .
       5101-TBLSAIHEN-EXT.
           EXIT.
      *****************************************************************
      *    回数再編集処理
      *****************************************************************
       51011-KAISU-SET1-SEC             SECTION.
      *
           IF     (WRK-KAISU               =   ZERO   )  OR
                  (SPA-COM-KAIFLG (IDX)    =   "1"    )  OR
                  (SPA-COM-ZAIEND (IDX)    =   "1"    )  OR
                  (IDX                     =   SPA-MAX-LINE)
      *        ’＊’の数だけ更新する
               IF      (SPA-GMN-INPUTCD(IDX)(1:1)   =   "*" )
                  AND  (IDX                <   SPA-MAX-LINE)
                  AND  (SPA-GMN-INPUTCD(IDX + 1)(1:1)
                                           =   "*"    )
                   CONTINUE
               ELSE
      *            回数変更あり
                   IF      SPA-COM-KAIFLG2(IDX)    =   "1"
                       MOVE    "1"                 TO  WRK-KAIFLG2
                   ELSE
                       MOVE    SPACE               TO  WRK-KAIFLG2
                   END-IF
                   MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
                   INITIALIZE                      WRK-NYUIN-AREA
      *            外用薬の時、入力回数を
                   IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
                          (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                          (SPA-COM-KAISU (IDX) =   1    )
                       MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                   END-IF
                   IF      WRK-KAISU      =   ZERO
                       MOVE    1              TO  WRK-KAISU
                   END-IF
               END-IF
           END-IF
      *    回数・日数
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
      *        回数変更あり
               IF      SPA-COM-KAIFLG2(IDX)    =   "1"
                   MOVE    "1"                 TO  WRK-KAIFLG2
               ELSE
                   MOVE    SPACE               TO  WRK-KAIFLG2
               END-IF
               MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
               PERFORM VARYING     IDX-N2  FROM    1   BY  1
                       UNTIL       IDX-N2  >   31
                   IF      SPA-COM-NYUINDAY  (IDX IDX-N2)
                                           NOT =   ZERO
                       MOVE    SPA-COM-NYUINDAY  (IDX IDX-N2)
                                               TO  WRK-NYUINDAY
                                                           (IDX-N2)
                   END-IF
               END-PERFORM
               ADD     SPA-COM-NYU-KAI (IDX)   TO  WRK-NYU-KAI
           END-IF
           IF     (WRK-NYU-KAI         =   ZERO )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-N2
               MOVE    1                   TO  WRK-NYUINDAY
                                                           (IDX-N2)
               MOVE    1                   TO  WRK-NYU-KAI
           END-IF
      *    回数入力分以外
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   NOT =   "*"
      *        算定日が違った時、警告クリア
               IF     (WRK-NYUIN-AREA  NOT =   SPA-COM-NYUIN-AREA(IDX))
      ******      OR  (WRK-KAISU       NOT =   SPA-COM-KAISU     (IDX)))
      *            訂正時展開以外
                AND    (ORCSC100-INIT       =   ZERO   )
                   MOVE    SPACE       TO  SPA-COM-KZMFLG(IDX)
                   MOVE    SPACE       TO  SPA-COM-HCHKFLG(IDX)
                   IF      WRK-NYU-KAI NOT =   SPA-COM-NYU-KAI (IDX)
                       MOVE    SPACE       TO  SPA-COM-TUKIJGNFLG (IDX)
                       MOVE    SPACE       TO  SPA-COM-TUKIJGNFLG1(IDX)
                   END-IF
               END-IF
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (IDX)
               MOVE    WRK-NYUIN-AREA  TO  SPA-COM-NYUIN-AREA (IDX)
           END-IF
      *    投薬月上限回数用
           MOVE    SPACE               TO  SPA-COM-KAIFLG2 (IDX)
           IF      WRK-KAIFLG2         =   "1"
               MOVE    SPACE               TO  SPA-COM-TUKIJGNFLG (IDX)
               MOVE    SPACE               TO  SPA-COM-TUKIJGNFLG1(IDX)
           END-IF
           .
       51011-KAISU-SET1-EXT.
           EXIT.
      *****************************************************************
      *    回数再編集処理（外来）
      *****************************************************************
      *51012-KAISU-SET2-SEC             SECTION.
      *
      *    IF     (WRK-KAISU               =   ZERO   )  OR
      *           (SPA-COM-ZAIEND (IDX)    =   "1"    )
      *        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *        外用薬の時、入力回数を
      *        IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
      *               (SPA-COM-KAISU2(IDX) >   ZERO ) AND
      *               (SPA-COM-KAISU (IDX) =   1    )
      *            MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
      *        END-IF
      *        IF      WRK-KAISU          =   ZERO
      *            MOVE    1              TO  WRK-KAISU
      *        END-IF
      *    END-IF
      *    MOVE    WRK-KAISU           TO  SPA-COM-KAISU (IDX)
      *    .
      *51012-KAISU-SET2-EXT.
      *    EXIT.
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51011-KAISU-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "*"
               GO      TO      51011-KAISU-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-KAISU
           PERFORM VARYING     IDX-KAI FROM    1   BY  1
                   UNTIL       IDX-KAI     >   54
               IF      FLG-OK              =   1
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1) NUMERIC
                       MOVE    1               TO  FLG-KAISU
                   END-IF
               ELSE
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1)   =   "*"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *    ＊ 以降に入力のない時、＊を削除
           IF      FLG-KAISU           =   ZERO
               INSPECT SPA-GMN-INPUTCD(IDX)
                       REPLACING   FIRST "*"  BY  " "
           END-IF
      *    訂正の時、剤の最後が回数入力となっている
           IF      SPA-COM-KAIFLG (IDX)    =   "1"
               MOVE    1               TO  FLG-KAISU
           END-IF
      *
           .
       51011-KAISU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定回数・併用算定チェック処理
      *****************************************************************
       51017-SANTEI-CHEK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-KAISU-GOK
           MOVE    ZERO                TO  WRK-KAISU
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE    )   OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE ))
                          OR ( SPA-ERRCD           NOT =   SPACE )
      *        IF      (SPA-COM-INPUTCD (IDX)(1:1) =   "1" )
      *          AND   (SPA-COM-HKNCOMBI(IDX)  NOT =   "9999")
      *          AND   (SPA-COM-HCHKFLG (IDX)      =   SPACE )
               IF      (SPA-COM-HKNCOMBI(IDX)  NOT =   "9999")
                 AND   (SPA-COM-HCHKFLG (IDX)      =   SPACE )
      *            回数チェック
                   IF      (SPA-COM-SANTEIRRKKBN(IDX)
                                               NOT =   ZERO )
                   AND    ((SPA-COM-INPUTCD (IDX)(1:1) =   "1" )  OR
                           (SPA-COM-INPUTCD (IDX)(1:3) =   "099"))
                       PERFORM 510171-KAISU-CHK-SEC
                   END-IF
      ***          併用算定チェック
      *            IF     (SPA-ERRCD              =   SPACE )
      *                PERFORM 510172-HEISANTEI-CHK-SEC
      ***          END-IF
      *            併用算定チェック
                   IF     (SPA-RSI-HKNKBN          =   "3"   )
                   OR     (SPA-COM-CHIKENKBN(IDX)  =   "1"   )
                   OR     (SPA-ERRCD           NOT =   SPACE )
      *            治験・アフターケアはチェックなし
                       CONTINUE
                   ELSE
      *!!
                   IF     (SPA-COM-INPUTCD (IDX)(1:1) =   "1" )
                   OR     (SPA-COM-INPUTCD (IDX)(1:1) =   "7" )
                   OR     (SPA-COM-INPUTCD (IDX)(1:1) =   "8" )
                   OR     (SPA-COM-INPUTCD (IDX)(1:3) =   "008" )
      *                診療コード、器材、コメントのみ
      *
                       PERFORM 510172-HEISANTEI-CHK-SEC
                   END-IF
      *!!!
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "1"             TO  SPA-COM-HCHKFLG(IDX)
                   ELSE
                       MOVE    SPACE           TO  SPA-COM-HCHKFLG(IDX)
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       51017-SANTEI-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定チェック処理
      *****************************************************************
       510172-HEISANTEI-CHK-SEC             SECTION.
      *
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
      **   IF      SPA-COM-NYU-KAI (IDX)  >   ZERO
      *        最初の日でチェックを行う
      *        PERFORM VARYING IDX-N2      FROM    1   BY  1
      *                UNTIL  (IDX-N2      >   31  )   OR
      *                       (SPA-ERRCD   NOT =   SPACE )
      *            IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
      *                MOVE    IDX-N2              TO  SPA-SRYYMD(7:2)
      *                MOVE    31                  TO  IDX-N2
      *            END-IF
      *        END-PERFORM
      **** END-IF
      *
           IF     ((SPA-COM-HCHKFLG    (IDX)   =   SPACE )
               OR  (SPA-COM-DAYCHK-FLG (IDX)   NOT =   "2"  ))
      *       併用算定チェック
      *             点数マスタ編集・基本チェック
               INITIALIZE                  ORCSC92AREA
               MOVE    "4"                 TO  LNK-SC92-KBN
               MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
               MOVE    "KT01"              TO  LNK-SC92-PG
      *        警告チェックあり
               MOVE    "1"                 TO  LNK-SC92-KEIFLG
               MOVE    IDX                 TO  LNK-SC92-GMN-IDX
               MOVE    SPA-COM-INPUTCD (IDX)
                                           TO  LNK-SC92-SRYCD
               MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
               MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
               MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
               MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
               MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
               PERFORM 5101721-KAISU-HEN-SEC
               CALL    "ORCSC92"           USING
                                           MCPAREA
                                           ORCSC92AREA
                                           SPA-SCR-REC
                                           SPA-AREA
               IF       LNK-SC92-ERRCD(6)  NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(6)   TO  SPA-ERRCD
                   MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
               END-IF
               MOVE    SPACE       TO  SPA-COM-HCHKFLG (IDX)
           END-IF
      *
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
           .
       510172-HEISANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数編集処理
      *****************************************************************
       5101721-KAISU-HEN-SEC             SECTION.
      *
           INITIALIZE                  LNK-SC92-KT01-AREA
           MOVE    "1"                 TO  LNK-SC92-KT01FLG
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
           MOVE    ZERO                TO  IDX-SN2
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )   OR
                              (SPA-SAN-SRYCD  (IDX-SN) =   SPACE)
               IF     (SPA-SAN-DAY-G (IDX-SN)  =   ZERO )
               AND    (SPA-SANE-DAY-G (IDX-SN) =   ZERO )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-SN2
                   MOVE    SPA-SAN-SRYCD (IDX-SN)
                                           TO  LNK-SC92-KT01-SRYCD
                                                              (IDX-SN2)
                   PERFORM VARYING     IDX-N2  FROM    1   BY  1
                           UNTIL       IDX-N2  >   31
                       IF      IDX-N2          <   IDD
                           ADD     SPA-SAN-DAY (IDX-SN IDX-N2)
                                               TO  LNK-SC92-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                       END-IF
                       ADD     SPA-SANE-DAY (IDX-SN IDX-N2)
                                               TO  LNK-SC92-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       5101721-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数のチェック処理
      *****************************************************************
       510171-KAISU-CHK-SEC             SECTION.
      *
      *    画像診断のＣＴ．ＭＲＩは２回目のコードが違うのでチェックなし
           IF     (SPA-COM-TNSSRYSYUKBN (IDX)  =   "723"
                                               OR  "724"  )  AND
                  (SPA-COM-HOUKATUTEIGENKBN(IDX)
                                               =   "201"
                                               OR  "202"
                                               OR  "203"  )  AND
                   (SPA-COM-KAISU (IDX)        =   1      )  AND
                   (SPA-COM-NYU-KAI (IDX)      <=  1      )
               CONTINUE
           ELSE
      *
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
           IF      SPA-COM-NYU-KAI (IDX)  >   ZERO
      *        日指定の回数
               MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
               MOVE    WRK-KAISU           TO  WRK-KAISU-GOK
      *        日の指定がある時、指定分
               PERFORM VARYING IDX-N2      FROM    1   BY  1
                       UNTIL  (IDX-N2      >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                       MOVE    IDX-N2              TO  SPA-SRYYMD(7:2)
      *                算定回数チェック
                       PERFORM 510172-SANTEIKAI-CHK-SEC
                   END-IF
               END-PERFORM
           ELSE
      *        診療日のみ
               MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
               MOVE    WRK-KAISU           TO  WRK-KAISU-GOK
      *        算定回数チェック
               PERFORM 510172-SANTEIKAI-CHK-SEC
           END-IF
      *
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
      *
           END-IF
           .
       510171-KAISU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴の算定回数チェック処理
      *****************************************************************
       510172-SANTEIKAI-CHK-SEC        SECTION.
      *
      *    算定履歴チェック処理
           INITIALIZE                  ORCSC91AREA
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-SYORIFLG        TO  LNK-SC91-SYORIFLG
           MOVE    SPA-COM-INPUTCD (IDX)
                                       TO  LNK-SC91-SRYCD
           MOVE    "3"                 TO  LNK-SC91-KBN
           MOVE    IDX                 TO  LNK-SC91-GMN-IDX
           MOVE    WRK-KAISU-GOK       TO  LNK-SC91-KAISU
           MOVE    SPA-COM-SURYO (IDX)
                                       TO  LNK-SC91-SURYO
      *H24.8
      *    数量999以上は999で
           IF      SPA-COM-SURYO (IDX)   >  999
               MOVE    999                 TO  LNK-SC91-SURYO
           END-IF
      *H24.8
           PERFORM 5101721-KAISU-HEN-SEC
           MOVE    LNK-SC92-KT01-AREA  TO  LNK-SC91-KT01-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
           IF      LNK-SC91-DAYMAXCNT  >   ZERO
      *        日上限は回数
               MOVE    WRK-KAISU           TO  WRK-KAISU-GOK
           ELSE
      *        日上限以外は回数を集計
               ADD     WRK-KAISU           TO  WRK-KAISU-GOK
           END-IF
      *
      *    警告判定
           IF      LNK-SC91-ERRCD(1:1)     =   "K"
               IF      SPA-COM-KZMFLG (IDX)    =   SPACE
                   MOVE    "1"             TO  SPA-COM-KZMFLG (IDX)
                   MOVE    "2"             TO  SPA-COM-CUR  (IDX)
               ELSE
                   MOVE    SPACE           TO  LNK-SC91-ERRCD
                   MOVE    SPACE           TO  LNK-SC91-ERRMSG
                   MOVE    SPACE           TO  SPA-COM-CUR  (IDX)
               END-IF
           END-IF
           MOVE    LNK-SC91-ERRCD      TO  SPA-ERRCD
           IF      SPA-COM-NYU-KAI (IDX)  >   ZERO
               IF     (LNK-SC91-ERRCD  NOT =   SPACE )  AND
                      (LNK-SC91-ERRMSG     =   SPACE )
                   MOVE    IDX-N2          TO  LNK-SC91-ERRMSG(1:2)
                   MOVE    "日算定済み"    TO  LNK-SC91-ERRMSG(3:)
               END-IF
           END-IF
           MOVE    LNK-SC91-ERRMSG     TO  SPA-ERRMSG2
           .
       510172-SANTEIKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性発生設定処理
      *****************************************************************
       51019-KYUSEI-SEC             SECTION.
      *
      *    急性発生日判定
           IF      SPA-COM-INPUTCD(IDX)    =   "099800101"
               MOVE    SPA-SRYYMD          TO  WRK-KYUSEI-YMD
               PERFORM VARYING IDX-N2      FROM    1   BY  1
                       UNTIL  (IDX-N2      >   31  )   OR
                             (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                       MOVE    IDX-N2          TO  WRK-KYUSEI-YMD(7:2)
                   END-IF
               END-PERFORM
           END-IF
      *    急性発生終了日判定
           IF      SPA-COM-INPUTCD(IDX)    =   "099800102"
               MOVE    SPA-SRYYMD          TO  WRK-KYUSEI-END
               PERFORM VARYING IDX-N2      FROM    1   BY  1
                       UNTIL  (IDX-N2      >   31  )   OR
                             (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                       MOVE    IDX-N2          TO  WRK-KYUSEI-END(7:2)
                   END-IF
               END-PERFORM
           END-IF
           EVALUATE    SPA-COM-INPUTCD(IDX)
      *       疾患別リハビリ発生日
               WHEN    "099800111"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (1)
               WHEN    "099800121"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (2)
               WHEN    "099800131"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (3)
               WHEN    "099800141"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (4)
               WHEN    "099800151"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (5)
               WHEN    "099800161"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (6)
               WHEN    "099800171"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (7)
               WHEN    "099800401"
                   MOVE    "1"             TO  WRK-TEIGEN-IN
           END-EVALUATE
      *
           .
       51019-KYUSEI-EXT.
           EXIT.
      *****************************************************************
      *    算定回数編集処理
      *****************************************************************
       51021-ORCSCKAISU-HEN-SEC             SECTION.
      *
           INITIALIZE                  LNK-ORCSC-KT01-AREA
           MOVE    "1"                 TO  LNK-ORCSC-KT01FLG
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
           MOVE    ZERO                TO  IDX-SN2
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100     )   OR
                              (SPA-SAN-SRYCD  (IDX-SN) =   SPACE)
               IF     (SPA-SAN-DAY-G (IDX-SN)  =   ZERO )
               AND    (SPA-SANE-DAY-G (IDX-SN) =   ZERO )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-SN2
                   MOVE    SPA-SAN-SRYCD (IDX-SN)
                                           TO  LNK-ORCSC-KT01-SRYCD
                                                              (IDX-SN2)
                   PERFORM VARYING     IDX-N2  FROM    1   BY  1
                           UNTIL       IDX-N2  >   31
                       IF      IDX-N2          <   IDD
                           ADD     SPA-SAN-DAY (IDX-SN IDX-N2)
                                               TO  LNK-ORCSC-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                       END-IF
                       ADD     SPA-SANE-DAY (IDX-SN IDX-N2)
                                               TO  LNK-ORCSC-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       51021-ORCSCKAISU-HEN-EXT.
           EXIT.
      *!!!
      *H27.1
      *****************************************************************
      *    後発品変更可・不可処方毎の処理
      *****************************************************************
       51019-INGIKSN-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE    )   OR
                             ((SPA-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX)   =   SPACE ))
                          OR ( SPA-ERRCD           NOT =   SPACE )
               IF      SPA-COM-INPUTCD (IDX)   =   "099209910"
                                               OR  "099209911"
                   PERFORM 510191-INGIKSN-HENSYORI-SEC
               END-IF
           END-PERFORM
           .
       51019-INGIKSN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般・銘柄処方毎処理
      *****************************************************************
       510191-INGIKSN-HENSYORI-SEC     SECTION.
      *
           PERFORM VARYING IDX-M       FROM    1   BY  1
                   UNTIL        (IDX-M         >   20    )
                           OR   (SPA-ERRCD NOT =   SPACE )
               IF      (WRK-INGKSN-SRYKA (IDX-M)   =   SPACE)
                   MOVE    SPA-COM-SRYKA(IDX)
                                          TO  WRK-INGKSN-SRYKA   (IDX-M)
                   MOVE    SPA-COM-HKNCOMBI(IDX)
                                          TO  WRK-INGKSN-HKNCOMBI(IDX-M)
               END-IF
               IF     (SPA-COM-SRYKA(IDX)      =   WRK-INGKSN-SRYKA
                                                               (IDX-M))
                 AND  (SPA-COM-HKNCOMBI(IDX)   =   WRK-INGKSN-HKNCOMBI
                                                               (IDX-M))
                   IF      WRK-INGKSN-KBN(IDX-M)   =   SPACE
                       IF      SPA-COM-INPUTCD (IDX)   =   "099209910"
                           MOVE    "1"             TO  WRK-INGKSN-KBN
                                                               (IDX-M)
                       ELSE
                           MOVE    "2"             TO  WRK-INGKSN-KBN
                                                               (IDX-M)
                       END-IF
                   ELSE
                       MOVE    "2072"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                   END-IF
                   MOVE    20                  TO  IDX-M
               END-IF
           END-PERFORM
           .
       510191-INGIKSN-HENSYORI-EXT.
           EXIT.
      *!!!
      *
      *H28.4
      *****************************************************************
      *    投薬の名称再編集処理
      *****************************************************************
       51023-MEIHENKAN-SEC          SECTION.
      *
           IF     (WRK-COM-INGAITEN (WRK-MAX)  NOT =   ZERO )
              OR  (WRK-COM-SRYSYUKBN (01)  =   "148"
                                           OR  "149"
                                           OR  "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "298"
                                           OR  "295" )
      *        院外投薬
               PERFORM 510231-IPN-HEN-SEC
           ELSE
      *        院内
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                       UNTIL       IDX-Z       >   WRK-MAX
      *
                   IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"  )
                       MOVE    WRK-COM-MEI-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                       MOVE    SPACE       TO  WRK-COM-MEI-KBN(IDX-Z)
                   END-IF
               END-PERFORM
           END-IF
           .
       51023-MEIHENKAN-EXT.
           EXIT.
      *****************************************************************
      *    投薬の名称再編集処理
      *****************************************************************
       510231-IPN-HEN-SEC          SECTION.
      *
           INITIALIZE                  ORCSCGENNAMEAREA
           MOVE    SPACE               TO  ORCSCGENN-SYORIKBN
           MOVE    "1"                 TO  ORCSCGENN-SYORIKBN2
           MOVE    SPA-SRYYMD          TO  ORCSCGENN-SRYYMD
      *    後発医薬品への変更可署名(SYS-1030-KOUHATUKA)
           MOVE    SPA-KOUHATUKA       TO  ORCSCGENN-1030-KOUHATUKA
      *    後発品変更不可（処方毎）有
           IF      SPA-GENECHG-KBN     =   1
               MOVE    "1"             TO  ORCSCGENN-KOUHATU-IN
           END-IF
      *H29.3  後発品変更可（処方毎）有
           IF      SPA-GENECHG-KBN     =   2
               MOVE    "2"             TO  ORCSCGENN-KOUHATU-IN
           END-IF
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z   >   WRK-MAX      )
               MOVE    WRK-GMN-INPUTCD(IDX-Z)
                                       TO  ORCSCGENN-INPUTCD(IDX-Z)
               MOVE    WRK-COM-INPUTCD(IDX-Z)
                                       TO  ORCSCGENN-SRYCD (IDX-Z)
               MOVE    WRK-COM-KOUHATUKBN (IDX-Z)
                                       TO  ORCSCGENN-TNS-KOUHATUKBN
                                                            (IDX-Z) 
               MOVE    WRK-COM-TNSP-KISAIKBN (IDX-Z)
                                       TO  ORCSCGENN-IPN-KISAIKBN
                                                            (IDX-Z)
               MOVE    WRK-COM-YAKKAKJNCD (IDX-Z)
                                       TO  ORCSGETGE-YAKKAKJNCD
                                                            (IDX-Z)
               MOVE    WRK-COM-IPN-NAME(IDX-Z)
                                       TO  ORCSCGENN-HEN-NAME
                                                            (IDX-Z)
      *
               MOVE    IDX-Z           TO  ORCSCGENN-TBLMAX
           END-PERFORM
      *
           CALL    "ORCSCGENNAME"      USING    ORCSCGENNAMEAREA
                                                SPA-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z   >   WRK-MAX      )
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"  )
                   IF     (ORCSCGENN-NAME-KBN (IDX-Z)  =   "1"
                                                       OR  "3"  )
                      AND (ORCSCGENN-HEN-NAME(IDX-Z) NOT =   SPACE)
      *                一般名
                       MOVE    ORCSCGENN-HEN-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                       MOVE    "1"         TO  WRK-COM-MEI-KBN(IDX-Z)
                   ELSE
      *                銘柄名
                       MOVE    WRK-COM-MEI-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                       MOVE    SPACE       TO  WRK-COM-MEI-KBN(IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       510231-IPN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬の銘柄記載編集処理
      *****************************************************************
       51024-MEIHENKAN02-SEC          SECTION.
      *
      *    院内
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL       IDX-Z       >   WRK-MAX
      *
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"  )
                   MOVE    WRK-COM-MEI-NAME(IDX-Z)
                                           TO  WRK-GMN-MEISYO(IDX-Z)
                   MOVE    SPACE           TO  WRK-COM-MEI-KBN(IDX-Z)
               END-IF
           END-PERFORM
           .
       51024-MEIHENKAN02-EXT.
           EXIT.
      *TEST
      *
      *****************************************************************
      *    剤ごとの処理
      *****************************************************************
       5102-ZAIBETU-SEC             SECTION.
      *
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
           INITIALIZE                  LNK-ORCSZAI-AREA
           MOVE    "1"                 TO  LNK-ORCSC-KBN
           PERFORM 51021-ORCSCKAISU-HEN-SEC
           IF      ORCSC100-KBN        =   "3"
               MOVE    "K02"               TO  LNK-ORCSC-PG
           ELSE
               MOVE    "KT01"              TO  LNK-ORCSC-PG
           END-IF
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    SPA-SYOYMD          TO  LNK-ORCSC-SYOYMD 
           MOVE    SPA-SYOYMD-OLD      TO  LNK-ORCSC-SYOYMD-OLD
           MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
           MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-INGAIKBN        TO  LNK-ORCSC-INGAIKBN
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-NAI-TIMEFLG     TO  LNK-ORCSC-TIMEFLG
           ELSE
               MOVE    WRK-TIMEFLG         TO  LNK-ORCSC-TIMEFLG
           END-IF
           MOVE    SPA-NAI-GAIFLG      TO  LNK-ORCSC-GAIFLG
           MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
           MOVE    SPA-NAIACCT2-AREA   TO  LNK-NAIACCT2-AREA
           MOVE    ORCSC100-TOROKU     TO  LNK-ORCSC-TOROKU
      *    最終受診日（履歴）
           MOVE    SPA-RRK-LSTYMD      TO  LNK-ORCSC-RRK-LSTYMD
      *    処理フラグ
           MOVE    SPA-SYORIFLG        TO  LNK-ORCSC-SYORIFLG
      *    初診フラグ
           MOVE    SPA-SYOSIN          TO  LNK-ORCSC-SYOSIN2
      *    初診フラグ（初診で、履歴の無い時、’１’）
           IF      SPA-NYUGAIKBN       =   "2"
               IF     ((SPA-SYOSIN         =   1   )  AND
                       (SPA-GMN-MAX1       =   ZERO)) OR
                       (SPA-SYOYMD         =   SPACE)
                   MOVE    1                   TO  LNK-ORCSC-SYOSIN
               ELSE
                   MOVE    ZERO                TO  LNK-ORCSC-SYOSIN
               END-IF
           ELSE
               MOVE    ZERO                TO  LNK-ORCSC-SYOSIN
           END-IF
      *    今回初診料入力
           IF      SPA-NAI-SYOSIN  NOT =   SPACE
               MOVE    SPA-NAI-SYOSIN      TO  LNK-ORCSC-SYOYMD
           END-IF
      *    同日初診フラグ
           MOVE    WRK-DOUSIN          TO  LNK-ORCSC-DOUSIN
      *
           MOVE    SPA-CHOKI-FLG       TO  LNK-ORCSC-CHOKI-FLG
           MOVE    SPA-YAKJYO-1        TO  LNK-ORCSC-YAKJYO-1
           MOVE    SPA-YAKJYO-2        TO  LNK-ORCSC-YAKJYO-2
           MOVE    SPA-YAKJYO-3        TO  LNK-ORCSC-YAKJYO-3
           MOVE    SPA-TOKUTEI-FLG     TO  LNK-ORCSC-TOKUTEI-FLG
      *    外来診療料算定有無
           MOVE    SPA-GAIRAISIN       TO  LNK-ORCSC-GAIRAISIN
      *
           MOVE    SPA-ROUMANSEI-ARI   TO  LNK-ORCSC-ROUMANSEI-ARI
           MOVE    SPA-ROUMANSEI-ARI2  TO  LNK-ORCSC-ROUMANSEI-ARI2
           MOVE    SPA-NETAKIRI-ARI    TO  LNK-ORCSC-NETAKIRI-ARI
           MOVE    SPA-NETAKIRI-ARI2   TO  LNK-ORCSC-NETAKIRI-ARI2
           MOVE    SPA-NETAKIRI-ARI3   TO  LNK-ORCSC-NETAKIRI-ARI3
           MOVE    SPA-SYONIKA-ARI     TO  LNK-ORCSC-SYONIKA-ARI
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-ORCSC-SYONIKA-ARI2
      *    生活習慣病指導管理料
           MOVE    SPA-SEIKATU-ARI     TO  LNK-ORCSC-SEIKATU-ARI
           MOVE    SPA-SEIKATU-ARI2    TO  LNK-ORCSC-SEIKATU-ARI2
      *    複数診療科あり
           MOVE    SPA-FUKU-SRYKA      TO  LNK-ORCSC-FUKU-SRYKA
      *    訂正前診療科
           MOVE    SPA-OLD-SRYKA       TO  LNK-ORCSC-OLD-SRYKA
           MOVE    SPA-OLD-HKNCOMBI    TO  LNK-ORCSC-OLD-HKNCOMBI
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *
      *    電話再診の有無
           MOVE    SPA-TELSAI-ARI      TO  LNK-ORCSC-TELSAI-ARI
      *
           MOVE    SPA-HOUMON-ARI      TO  LNK-ORCSC-HOUMON-ARI
      *    特定疾患あり
           MOVE    SPA-TOKUTEI         TO  LNK-ORCSC-TOKUTEI
           MOVE    SPA-TOKUTEIFLG      TO  LNK-ORCSC-TOKUTEIFLG
      *    診察回数
           MOVE    SPA-SINSATU-CNT     TO  LNK-ORCSC-SINSATU-CNT
      *   １５歳未満再診科算定有り
           MOVE    SPA-15MIMAN-ARI     TO  LNK-ORCSC-15MIMAN-ARI
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEI-KNJ     TO  LNK-ORCSC-TOKUTEI-KNJ
           MOVE    SPA-TOKUTEI-KNJ2    TO  LNK-ORCSC-TOKUTEI-KNJ2
      *    人工腎臓患者
           MOVE    SPA-TOKUTEI-JNZ     TO  LNK-ORCSC-TOKUTEI-JNZ
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEIG-KNJ    TO  LNK-ORCSC-TOKUTEIG-KNJ
           MOVE    SPA-TOKUTEIG-KNJ2   TO  LNK-ORCSC-TOKUTEIG-KNJ2
      *    消炎鎮痛処置当月内算定（Ｈ２０．３まで）
           IF      SPA-NYUGAIKBN       =   "1"
      *        入院は今回分の集計
               MOVE    WRK-SYOEN-CNT       TO  LNK-ORCSC-SYOEN-CNT
           ELSE
      *        外来は今回分の集計なし
               MOVE    SPA-SYOEN-CNT       TO  LNK-ORCSC-SYOEN-CNT
           END-IF
      *    急性発生１８０日以内
           MOVE    SPA-KYUSEI-90       TO  LNK-ORCSC-KYUSEI-90
           MOVE    SPA-KYUSEI-180      TO  LNK-ORCSC-KYUSEI-180
           MOVE    SPA-KYUSEI-FLG      TO  LNK-ORCSC-KYUSEI-FLG
      *    急性発生日
           MOVE    WRK-KYUSEI-YMD      TO  LNK-ORCSC-KYUSEI-YMD
           MOVE    WRK-KYUSEI-END      TO  LNK-ORCSC-KYUSEI-END
      *    当月外来管理加算の取れない患者
           MOVE    SPA-GAIRAI-NASI     TO  LNK-ORCSC-GAIRAI-NASI
      *    再診チェック済
           MOVE    SPA-SAISIN-CHK      TO  LNK-ORCSC-SAISIN-CHK
      *    労災用 外来管理点数
           MOVE    SPA-GAIRAI-TEN      TO  LNK-ORCSC-GAIRAI-TEN
      *
      *    継続領域
           MOVE    SPA-CONTKBN         TO  LNK-ORCSC-CONTKBN
           MOVE    SPA-DENPNUM         TO  LNK-ORCSC-DENPNUM
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-RENNUM          TO  LNK-ORCSC-DOUJI-RENNUM
           ELSE
               MOVE    SPA-DOUJI-RENNUM    TO  LNK-ORCSC-DOUJI-RENNUM
           END-IF
      *
      *    診察料有無
           MOVE    WRK-SINSATU-ARI     TO  LNK-ORCSC-SINSATU-ARI
      *    外来管理加算削除のチェック
           MOVE    SPA-GAIKANRIKBN-CHK TO  LNK-ORCSC-GAIKANRIKBN-CHK
      *    入院期間（最終退院日）
           MOVE    SPA-K02N-NYUINYMD   TO  LNK-ORCSC-NYUINYMD
           MOVE    SPA-K02N-TAIINYMD   TO  LNK-ORCSC-TAIINYMD
      *    リハビリ逓減確認
           MOVE    ORCSC100-RIHTEI     TO  LNK-ORCSC-FLG-RIHTEI
      *    リハビリ算定回数
           MOVE    SPA-RIGAKU-NCNT-AREA    TO  LNK-RIGAKU-NCNT-AREA
      *    検査包括
           IF      ORCSC100-KBN        =   "3"
               MOVE    1                   TO  LNK-ORCSC-KENSAFLG
           END-IF
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
           MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-ORCSC-HAIKIKBN-FLG
      *
           MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
           IF      ORCSC100-KBN        =   "1"
      *    最後の日を診療日とする
               PERFORM VARYING IDX-N2      FROM    1   BY  1
                       UNTIL   IDX-N2      >   31
                   IF      SPA-COM-NYUINDAY(IDX IDX-N2)    >   ZERO
                       MOVE    IDX-N2              TO  SPA-SRYYMD(7:2)
                   END-IF
               END-PERFORM
           END-IF
      *
      *H27.1
      *    後発変更不可処方毎編集
           MOVE    ZERO                TO  SPA-GENECHG-KBN
           PERFORM VARYING IDX-M       FROM    1   BY  1
                   UNTIL         ( IDX-M   >   20  )
                            OR   ( WRK-INGKSN-SRYKA (IDX-M)
                                               =   SPACE )
               IF     (SPA-COM-SRYKA(IDX)      =   WRK-INGKSN-SRYKA
                                                               (IDX-M))
                 AND  (SPA-COM-HKNCOMBI(IDX)   =   WRK-INGKSN-HKNCOMBI
                                                               (IDX-M))
                   MOVE    WRK-INGKSN-KBN(IDX-M)   TO  SPA-GENECHG-KBN
                   MOVE    20                 TO  IDX-M
               END-IF
           END-PERFORM
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *
           PERFORM 51021-ZAIBETU-CHK-SEC
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
           .
       5102-ZAIBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤別のチェック処理
      *****************************************************************
       51021-ZAIBETU-CHK-SEC          SECTION.
      *
           EVALUATE    WRK-COM-SRYKBN (01)
      *            診療
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
                   CALL    "ORCSC10"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (01)
      *            初診フラグ
                   MOVE    LNK-ORCSC-SYOSIN2   TO  SPA-SYOSIN
                   IF      LNK-ORCSC-SYOSIN2   =   1
                       MOVE    SPA-SRYYMD          TO  SPA-NAI-SYOSIN
                   END-IF
      *
      *            同日初診フラグ
                   MOVE    LNK-ORCSC-DOUSIN    TO  WRK-DOUSIN
      *            診察の時間加算の時、全体へ
                   MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-TIMEFLG
      *            外来診療料算定有無
                   MOVE    LNK-ORCSC-GAIRAISIN TO  SPA-GAIRAISIN
      *
      *            小児科外来診療科の時、年齢エラーはそのまま
                   IF      SPA-SYONIKA-ARI3    NOT =   ZERO
                       IF      SPA-ERRCD           =   "0032"
                           MOVE    SPACE               TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            特定薬剤治療管理加算の初回算定日
                   MOVE    LNK-ORCSC-TOKTEI-YMD
                                           TO  SPA-TOKTEI-YMD
      *            厚生省が定める患者
                   MOVE    LNK-ORCSC-TOKUTEI-KNJ
                                           TO  SPA-TOKUTEI-KNJ
      *            人工腎臓患者
                   MOVE    LNK-ORCSC-TOKUTEI-JNZ
                                           TO  SPA-TOKUTEI-JNZ
      *            厚生省が定める患者
                   MOVE    LNK-ORCSC-TOKUTEIG-KNJ
                                           TO  SPA-TOKUTEIG-KNJ
      *            労災用 外来管理点数
                   MOVE    LNK-ORCSC-GAIRAI-TEN
                                           TO  SPA-GAIRAI-TEN
      *            診察料有無
                   MOVE    LNK-ORCSC-SINSATU-ARI
                                           TO  WRK-SINSATU-ARI
      *
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
               WHEN    "25"
      *
               WHEN    "24"
               WHEN    "26"
               WHEN    "27"
      *            処方せんコメント
               WHEN    "98"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
      *            注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   CALL    "ORCSC30"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (03)
      *            処置
               WHEN    "40"
                   CALL    "ORCSC40"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (04)
      *            消炎鎮痛処置当月内算定
                   MOVE    LNK-ORCSC-SYOEN-CNT
                                           TO  WRK-SYOEN-CNT
      *            手術
               WHEN    "50"
                   CALL    "ORCSC50"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (05)
      *            麻酔
               WHEN    "54"
                   CALL    "ORCSC54"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (06)
      *            検査
               WHEN    "60"
               WHEN    "64"
                   CALL    "ORCSC60"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (07)
      *            画像診断
               WHEN    "70"
                   CALL    "ORCSC70"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (08)
      *            その他
               WHEN    "80"
      *          処方せん料は投薬とする
                 IF      WRK-COM-SRYSYUKBN (01)  =   "820"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
                 ELSE
                   CALL    "ORCSC80"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *            リハビリ確認
                   MOVE    LNK-RIGAKU-NCNT-AREA
                                           TO  SPA-RIGAKU-NCNT-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (09)
                 END-IF
      *            コメント等
               WHEN    "93"
               WHEN    "99"
                   PERFORM 510211-SRYKBN-90-SEC
      *            自費
               WHEN    "95"
               WHEN    "96"
                   CALL    "ORCSCJIH"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *            入院（食事）
               WHEN    "90"
               WHEN    "92"
               WHEN    "97"
                   IF      SPA-NYUGAIKBN   =   "2"
                       MOVE    "9200"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (01)
                   ELSE
                       CALL    "ORCSCN97"      USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
                       ADD     1               TO  SPA-SRYKBN-CNT (10)
                   END-IF
           END-EVALUATE
      *@
      *    投薬附加情報チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (WRK-ERR-ERRCD       =   SPACE )  AND
                  (WRK-KEI-ERRCD       =   SPACE )  AND
                  (WRK-KIDCD           =   SPACE )
               PERFORM 51022-TENSUPLUS2-CHK-SEC
           END-IF
      *@
      *
           MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
           MOVE    LNK-ORCSC-ERRMSG    TO  SPA-ERRMSG2
      *
      *    エラーコードを保存する
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD(1:1)      =   "K"
      *            既にエラーがある時、フラグをクリアする
                   IF     (WRK-ERR-ERRCD   NOT =   SPACE )  OR
                          (WRK-KEI-ERRCD   NOT =   SPACE )  OR
                          (WRK-KIDCD       NOT =   SPACE )
                       MOVE    SPACE           TO  WRK-COM-KZMFLG
                                                    (LNK-ORCSC-KEI-IDX)
                   ELSE
                       MOVE    SPA-ERRCD       TO  WRK-KEI-ERRCD
                       MOVE    SPA-ERRMSG2     TO  WRK-KEI-ERRMSG2
                   END-IF
               ELSE
                   IF      WRK-ERR-ERRCD       =   SPACE
                       MOVE    SPA-ERRCD       TO  WRK-ERR-ERRCD
                       MOVE    SPA-ERRMSG2     TO  WRK-ERR-ERRMSG2
                   END-IF
               END-IF
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
      *    既にエラーがある時、フラグをクリアする
           IF    ( SPA-KIDCD       NOT =   SPACE )  AND
                 ((WRK-ERR-ERRCD   NOT =   SPACE )  OR
                  (WRK-KEI-ERRCD   NOT =   SPACE )  OR
                  (WRK-KIDCD       NOT =   SPACE ))
               MOVE    SPACE           TO  WRK-COM-KZMFLG
                                                    (LNK-ORCSC-KEI-IDX)
           END-IF
           IF     (SPA-KIDCD       NOT =   SPACE )  AND
                  (WRK-KIDCD           =   SPACE )
               MOVE    SPA-KIDCD           TO  WRK-KIDCD
           END-IF
      *
           MOVE    ZERO                TO  WRK-MAX
      *
           MOVE    ZERO                TO  FLG-HOUKATU-ZAI
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX-Z) =   SPACE ))
      *
      *        診区
               IF      IDX-Z               =   1
                   MOVE    WRK-COM-SRYKBN (IDX-Z)
                                           TO  WRK-GMN-SRYKBN(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-GMN-SRYKBN(IDX-Z)
                   MOVE    WRK-COM-SRYSYUKBN (01)
                                           TO  WRK-COM-SRYSYUKBN(IDX-Z)
               END-IF
      *        名称（最初の行に’＊’）
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "8" )  OR
                      (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "008")
                 OR   (WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "#"   )
                 OR   (WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "$"   )
                   CONTINUE
               ELSE
                   IF      IDX-Z               =   1
                       MOVE    "* "            TO  WRK-GMN-MEIH (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-GMN-MEIH (IDX-Z)
                   END-IF
               END-IF
      *
      *        最終行を剤終了とする
               IF      ( IDX-Z             =   SPA-MAX-LINE )  OR
                       ((WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE ) AND
                        (WRK-GMN-INPUTCD(IDX-Z + 1) =   SPACE ))
                   MOVE    "1"             TO  WRK-COM-ZAIEND(IDX-Z)
                   MOVE    IDX-Z           TO  WRK-MAX
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND(IDX-Z)
               END-IF
      *        回数
               IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
               END-IF
      *
      *        外用の回数
               IF      WRK-COM-SRYKBN(IDX-Z)   =   "23"
                   MOVE    WRK-KAISU           TO  WRK-COM-KAISU2(IDX-Z)
               END-IF
      *        最終行に回数入力（＊）が有った時、最終行を回数入力
               IF      FLG-KAISU           NOT =   ZERO
                   IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
                       IF      WRK-COM-KAIFLG (IDX-Z)  =   "1"
                           MOVE    SPACE           TO  WRK-COM-KAIFLG
                                                               (IDX-Z)
                           PERFORM 51021-KAISU-SAIHEN-SEC
                       END-IF
                   ELSE
      *                最終行
                       IF      WRK-COM-KAIFLG (IDX-Z)  =   SPACE
                           PERFORM 51021-KAISU-SAIHEN-SEC
                           MOVE    "1"             TO  WRK-COM-KAIFLG
                                                               (IDX-Z)
                       END-IF
                   END-IF
               END-IF
      *        自費以外の点数を集計
               IF     (WRK-COM-ZAIEND (IDX-Z)      =   "1"   )  AND
                      (WRK-COM-SRYKBN (IDX-Z)  NOT =   "95"  AND "96")
               AND    (WRK-COM-HKNCOMBI (IDX-Z)    NOT =   "9999" )
               AND    (WRK-COM-HOUKATU-ZAI (IDX-Z) =   ZERO  )
                   IF      WRK-COM-PLUSKBN (IDX-Z)     =   1
                       ADD     WRK-COM-KEI    (IDX-Z)  TO  WRK-GOKEI-9S
                   ELSE
                       ADD     WRK-COM-KEI    (IDX-Z)  TO  WRK-GOKEI-9
                   END-IF
               END-IF
           END-PERFORM
      *
      *H28.5
      *    院外投薬名称変換
           IF     (WRK-COM-SRYKBN (01) =   "21"
                                       OR  "22"
                                       OR  "23"
                                       OR  "14" )
             AND  (SPA-SRYYMD          >=  "20160401")
               IF      (SPA-GENERICFLG      =   "1"   )
      *            一般名記載
                   PERFORM 51023-MEIHENKAN-SEC
               ELSE
      *            銘柄記載
                   PERFORM 51024-MEIHENKAN02-SEC
               END-IF
           END-IF
      *!!!!
      *
      *    点数再セット
      *    診察日を戻す
           MOVE    WRK-SPA-SRYYMD      TO  SPA-SRYYMD
           MOVE    ZERO                TO  FLG-HOUKATU-ZAI
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL       IDX-Z       >   WRK-MAX
      *
               IF     (WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "*"  )  OR
                      (WRK-COM-KAIFLG (IDX-Z)      =   "1"  )
                   IF      IDX-Z           NOT =   WRK-MAX
      *            各点数編集
                       MOVE    WRK-COM-TENSU (WRK-MAX)
                                           TO  WRK-COM-TENSU (IDX-Z)
                       MOVE    WRK-COM-SYUTEN1 (WRK-MAX)
                                           TO  WRK-COM-SYUTEN1 (IDX-Z)
                       MOVE    WRK-COM-SYUTEN2 (WRK-MAX)
                                           TO  WRK-COM-SYUTEN2 (IDX-Z)
                       MOVE    WRK-COM-YKZTEN (WRK-MAX)
                                           TO  WRK-COM-YKZTEN (IDX-Z)
                       MOVE    WRK-COM-KIZAITEN (WRK-MAX)
                                           TO  WRK-COM-KIZAITEN(IDX-Z)
                   END-IF
      *            計
      *            自費の時,計
                   IF      WRK-COM-SRYSYUKBN(IDX-Z)(1:2)
                                               =   "95" OR "96"
                       CONTINUE
                   ELSE
                       COMPUTE WRK-COM-KEI (IDX-Z)
                                               =   WRK-COM-TENSU(IDX-Z)
                                               *   WRK-COM-KAISU(IDX-Z)
                       MOVE    WRK-COM-KEI (IDX-Z)
                                               TO  WRK-GMN-KEI  (IDX-Z)
                   END-IF
               ELSE
      *            回数
                   IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                       MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU
                                                                (IDX-Z)
                   END-IF
               END-IF
      *
      *        保険組合せ
               IF      WRK-HKNCOMBI        =   SPACE
                   MOVE    SPA-HKNCOMBINUM     TO  WRK-COM-HKNCOMBI
                                                               (IDX-Z)
               ELSE
                   MOVE    WRK-HKNCOMBI        TO  WRK-COM-HKNCOMBI
                                                               (IDX-Z)
               END-IF
               IF      WRK-SRYKA           =   SPACE
                   MOVE    SPA-SRYKA       TO  WRK-COM-SRYKA  (IDX-Z)
               ELSE
                   MOVE    WRK-SRYKA        TO  WRK-COM-SRYKA (IDX-Z)
               END-IF
      *
      *        約束セット行の編集
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    LNK-ORCSC-ZAITEN        TO  WRK-COM-TENSU
                                                                (IDX-Z)
                   MOVE    LNK-ORCSC-ZAIKEI        TO  WRK-COM-KEI
                                                               (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU
                                                               (IDX-Z)
               END-IF
      *        包括診療コードの加算も包括分とする
               IF     (WRK-COM-HOUKATU-ZAI (IDX-Z) =   1   )
      *TEST
      *        包括剤
               OR     (WRK-COM-INPUTCD(IDX-Z)  =   "099999904")
      *TEST
                   MOVE    1                   TO  FLG-HOUKATU-ZAI
               ELSE
                   IF      FLG-HOUKATU-ZAI     =   1
                       MOVE    1               TO  WRK-COM-HOUKATU-ZAI
                                                           (IDX-Z)
                   END-IF
               END-IF
      *        画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "2"                 TO  LNK-SC94-KBN
               MOVE    "K02"               TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               MOVE    SPA-SYORIFLG        TO  LNK-SC94-SYORIFLG
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               IF      WRK-HOZ-IDX         >   SPA-MAX-LINE
                   IF      SPA-SYORIFLG        =   1
                       MOVE    "9100"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "0007"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HZN-COM-TBLREC
                                                       (WRK-HOZ-IDX)
                   MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HZN-GMN-TBLREC
                                                       (WRK-HOZ-IDX)
               END-IF
      *
           END-PERFORM
           .
       51021-ZAIBETU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数の表示変更処理
      *****************************************************************
       51021-KAISU-SAIHEN-SEC          SECTION.
      *
           IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
      *        *　以降を削除
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX-KAI FROM    1   BY  1
                       UNTIL      (IDX-KAI     >   54  )   OR
                                  (FLG-OK      =   1   )
                   IF      WRK-GMN-INPUTCD(IDX-Z)(IDX-KAI:1)   =   "*"
                       MOVE    IDX-KAI         TO  IDX-KAI2
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK          =   1
                   MOVE    SPACE           TO  WRK-GMN-INPUTCD(IDX-Z)
                                                   (IDX-KAI2:)
               END-IF
           ELSE
      *
      *        * を追加
               MOVE    WRK-COM-KAISU(IDX-Z)    TO  WRK-KAISU-Z
               MOVE    ZERO                TO  IDK2
               MOVE    SPACE               TO  WRK-KAISU-H 
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   4
                   IF      WRK-KAISU-X(IDK1:1)  NOT =   SPACE
                       ADD     1           TO  IDK2
                       MOVE    WRK-KAISU-X(IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
                   END-IF
               END-PERFORM
      *
               MOVE    WRK-GMN-INPUTCD (IDX-Z) TO  WRK-HEN-INPUTCD
               MOVE    SPACE                   TO  WRK-GMN-INPUTCD
                                                            (IDX-Z)
      *
               STRING  WRK-HEN-INPUTCD         DELIMITED   BY  SPACE
                       "*"                     DELIMITED   BY  SIZE
                       WRK-KAISU-H             DELIMITED   BY  SPACE
                       INTO                    WRK-GMN-INPUTCD(IDX-Z)
               END-STRING
      *
           END-IF
           .
       51021-KAISU-SAIHEN-EXT.
           EXIT.
      *****************************************************************
      *    長期投薬期間、投与量チェック処理
      *****************************************************************
       51022-TENSUPLUS2-CHK-SEC                SECTION.
      *
           INITIALIZE                  ORCSC98AREA
      **   MOVE    "K02"               TO  LNK-SC98-PG
           MOVE    "KT01"              TO  LNK-SC98-PG
           MOVE    SPA-NENREI-YY       TO  LNK-SC98-NENREI-YY
           MOVE    SPA-NENREI-MM       TO  LNK-SC98-NENREI-MM
           MOVE    SPA-NENREI-DD       TO  LNK-SC98-NENREI-DD
      *H28.11
      *    薬剤投与量チェック区分
           MOVE    SPA-TOUYOCHK-FLG    TO  LNK-SC98-TOUYOCHK-FLG
           CALL    "ORCSC98"           USING
                                       SPA-AREA
                                       ORCSC98AREA
                                       WRK-SCR-REC
           IF      LNK-SC98-ERRMSG     NOT =   SPACE
               MOVE    LNK-SC98-ERRMSG     TO  LNK-ORCSC-ERRMSG
           END-IF
           .
       51022-TENSUPLUS2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月上限投薬期間、投与量チェック処理
      *****************************************************************
       51022-TENSUPLUS2-CHK02-SEC                SECTION.
      *
           INITIALIZE                  ORCSC98AREA
      **   MOVE    "K02"               TO  LNK-SC98-PG
           MOVE    "KT01"              TO  LNK-SC98-PG
           MOVE    "1"                 TO  LNK-SC98-KBN
           MOVE    SPA-NENREI-YY       TO  LNK-SC98-NENREI-YY
           MOVE    SPA-NENREI-MM       TO  LNK-SC98-NENREI-MM
           MOVE    SPA-NENREI-DD       TO  LNK-SC98-NENREI-DD
           MOVE    SPA-YAKUZAI-AREA    TO  LNK-SC98-YAKUZAI-AREA
      *H28.11
      *    薬剤投与量チェック区分
           MOVE    SPA-TOUYOCHK-FLG    TO  LNK-SC98-TOUYOCHK-FLG
           CALL    "ORCSC98"           USING
                                       SPA-AREA
                                       ORCSC98AREA
                                       WRK-HZN-SCR-AREA
           MOVE    LNK-SC98-YAKUZAI-AREA   TO  SPA-YAKUZAI-AREA
           IF      LNK-SC98-ERRMSG     NOT =   SPACE
               MOVE    LNK-SC98-ERRMSG     TO  SPA-ERRMSG2
           END-IF
           .
       51022-TENSUPLUS2-CHK02-EXT.
           EXIT.
      *****************************************************************
      *     コメント等　編集処理
      *****************************************************************
       510211-SRYKBN-90-SEC            SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF      WRK-COM-KISOTEN (IDX-Z) =   ZERO
                   MOVE    ZERO            TO  WRK-COM-KEI (IDX-Z)
                   MOVE    1               TO  WRK-COM-KAISU(IDX-Z)
                   MOVE    1               TO  WRK-COM-SURYO(IDX-Z)
                   MOVE    ZERO            TO  WRK-COM-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-SURYO(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-KAISU(IDX-Z)
                   MOVE    ZERO            TO  WRK-GMN-KEI (IDX-Z)
               END-IF
      *        他保険受診済みが存在する
               EVALUATE    SPA-COM-INPUTCD (IDX-Z)
                   WHEN    "099999902"
                       MOVE    1                   TO  SPA-TAHOKEN-FLG
               END-EVALUATE
           END-PERFORM
      *
           .
       510211-SRYKBN-90-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  FLG-DELFLG
           MOVE    ZERO                TO  FLG-SYORI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   IF      WRK-COM-TIMEFLG (IDX)   NOT =   ZERO
                       MOVE    1               TO  FLG-DELFLG
                       IF      WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                           "12"
                            MOVE    2               TO  FLG-DELFLG
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    NOT =   "1" )  AND
                          (WRK-COM-DATAKBN(IDX)        =   "1"  OR
                           WRK-COM-INPUTCD(IDX)        =   SPACE)
                       MOVE    1               TO  FLG-ZAIEND
                   END-IF
      *            器材商品名
                   IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "058")
                       MOVE    2               TO  FLG-ZAIEND
                   END-IF
      *            前も空白の時対象行のみ
                   IF     (WRK-COM-INPUTCD(IDX)        =   SPACE )  AND
                          (WRK-MAE-INPUTCD(IDX)        =   SPACE )
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *            前が器材商品名
                   IF     (WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE )
                      AND (WRK-COM-INPUTCD(IDX - 1)(1:3)
                                                       =   "058")
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX - 1)
                   END-IF
      *
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                     <   SPA-MAX-LINE) AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               ELSE
                   IF      FLG-ZAIEND          =   1
                       IF      WRK-COM-AUTOKBN(IDX)    NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                       END-IF
                   END-IF
      *            前が器材商品名
                   IF      FLG-ZAIEND          =   2
                       IF      WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                       END-IF
                   END-IF
      *            診療の時間加算削除の時、自動追加した時間加算を削除
                   IF      FLG-DELFLG          =   2
                       IF     (WRK-COM-AUTOKBN(IDX)    NOT =   SPACE)
                         AND  (WRK-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                (IDX)
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                 <   SPA-MAX-LINE )  AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-DELFLG      NOT =   ZERO
               MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           END-IF
      *
           MOVE    ZERO                TO  FLG-IKKATU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
      *            登録前チェックの時、診療種別のみの剤は削除する
                   IF      ORCSC100-TOROKU     =   "1"
                       IF      WRK-GMN-INPUTCD (IDX)(1:1)  =  "."
                           IF      (IDX            =   SPA-MAX-LINE ) OR
                                   (WRK-GMN-INPUTCD(IDX + 1)(1:1)
                                                   =   "."     )  OR
                                   (WRK-COM-INPUTCD(IDX + 1)
                                                   =   SPACE  AND
                                    WRK-GMN-INPUTCD(IDX + 1)
                                                   =   SPACE   )
      ***                      OR  (WRK-GMN-SRYKBN (IDX + 1)
      ***                                          NOT =   SPACE )
                               MOVE    1               TO  FLG-ERR
                           ELSE
      *H24.11
                               IF    (WRK-GMN-SRYKBN (IDX + 1)
                                                   NOT =   SPACE )
      *                            診療種別の行削除チェック
                                   PERFORM 4201-ZAIDEL-CHK-SEC
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      FLG-ERR             =   ZERO
      *
                       ADD     1               TO  SPA-GMN-MAX
                       MOVE    WRK-COM-TBLREC (IDX) 
                                               TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                       MOVE    WRK-GMN-TBLREC (IDX) 
                                               TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
      *
                       IF      SPA-GMN-INPUTCD(IDX)(1:1)
                                               =   "*"
                           MOVE    1                   TO  FLG-IKKATU
                       END-IF
      *
                       IF      SPA-GMN-MAX     NOT =   IDX
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別の行削除チェック処理
      *****************************************************************
       4201-ZAIDEL-CHK-SEC                 SECTION.
      *
           MOVE    1               TO  FLG-ERR
      *
           IF     (WRK-COM-INPUTCD(IDX + 1)(1:1)   =   "6"   )
           AND    (SPA-COM-KAIFLG (SPA-GMN-MAX)
                                               NOT =   "1"   )
               IF     (SPA-COM-SRYKBN(SPA-GMN-MAX)
                                               =   "11"
                                               OR  "12"
                                               OR  "13" )
               OR     (SPA-COM-SRYSYUKBN(SPA-GMN-MAX)
                                               =   "600" )
                   CONTINUE
               ELSE
                   MOVE    "1"                 TO  SPA-COM-KAIFLG
                                                    (SPA-GMN-MAX)
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "1"                 TO  LNK-SC94-KBN
                   MOVE    "K02"               TO  LNK-SC94-PG
                   MOVE    SPA-GMN-MAX         TO  LNK-SC94-IDX
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               SPA-SCR-REC
                                               SPA-AREA
               END-IF
           END-IF
      *
           IF     (WRK-COM-INPUTCD(IDX + 1)(1:1)   =   "6"   )
           AND    (SPA-COM-SRYSYUKBN(SPA-GMN-MAX)  =   "600" )
           AND    (SPA-COM-KAISU(SPA-GMN-MAX)  <=   1     )
      *        検査の時、回数が無効となる為、診療種別を変更する
               MOVE    ZERO                TO  FLG-ERR
               MOVE    WRK-COM-SRYKBN(IDX + 1)
                                           TO  WRK-GMN-INPUTCD
                                                   (IDX)(2:2)
               MOVE    "0"                 TO  WRK-GMN-INPUTCD
                                                   (IDX)(4:1)
           END-IF
      *
           .
       4201-ZAIDEL-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    各診療行為チェックへ
      *****************************************************************
       520-ALL-CHEK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-TUIKA
      *    変更の判定用に保存
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *
           PERFORM 51021-ORCSCKAISU-HEN-SEC
           IF      ORCSC100-KBN        =   "3"
               MOVE    "K02"               TO  LNK-ORCSC-PG
           ELSE
               MOVE    "KT01"              TO  LNK-ORCSC-PG
               IF      LNK-ORCSC-KBN       =   "3"
                   MOVE    "K02"               TO  LNK-ORCSC-PG
               END-IF
           END-IF
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    SPA-NAIACCT-AREA    TO  LNK-NAIACCT-AREA
           MOVE    SPA-NAIACCT2-AREA   TO  LNK-NAIACCT2-AREA
           MOVE    SPA-SYOYMD          TO  LNK-ORCSC-SYOYMD 
           MOVE    SPA-SYOYMD-OLD      TO  LNK-ORCSC-SYOYMD-OLD
           MOVE    SPA-LSTYMD          TO  LNK-ORCSC-LSTYMD 
           MOVE    SPA-LSTSRYKA        TO  LNK-ORCSC-LSTSRYKA 
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-ORCSC-LSTSRYKAMEI 
           MOVE    SPA-INGAIKBN        TO  LNK-ORCSC-INGAIKBN
           MOVE    ORCSC100-TOROKU     TO  LNK-ORCSC-TOROKU
      *    最終受診日（履歴）
           MOVE    SPA-RRK-LSTYMD      TO  LNK-ORCSC-RRK-LSTYMD
      *    処理フラグ
           MOVE    SPA-SYORIFLG        TO  LNK-ORCSC-SYORIFLG
      *    初診フラグ
           MOVE    SPA-SYOSIN          TO  LNK-ORCSC-SYOSIN2
      *    初診フラグ（初診で、履歴の無い時、’１’）
           IF      SPA-NYUGAIKBN       =   "2"
               IF     ((SPA-SYOSIN         =   1   )  AND
                       (SPA-GMN-MAX1       =   ZERO)) OR
                       (SPA-SYOYMD         =   SPACE)
                   MOVE    1                   TO  LNK-ORCSC-SYOSIN
               ELSE
                   MOVE    ZERO                TO  LNK-ORCSC-SYOSIN
               END-IF
           ELSE
               MOVE    ZERO                TO  LNK-ORCSC-SYOSIN
           END-IF
      *    今回初診料入力
           IF      SPA-NAI-SYOSIN  NOT =   SPACE
               MOVE    SPA-NAI-SYOSIN      TO  LNK-ORCSC-SYOYMD
           END-IF
      *
           MOVE    SPA-CHOKI-FLG       TO  LNK-ORCSC-CHOKI-FLG
           MOVE    SPA-YAKJYO-1        TO  LNK-ORCSC-YAKJYO-1
           MOVE    SPA-YAKJYO-2        TO  LNK-ORCSC-YAKJYO-2
           MOVE    SPA-YAKJYO-3        TO  LNK-ORCSC-YAKJYO-3
           MOVE    SPA-TOKUTEI-FLG     TO  LNK-ORCSC-TOKUTEI-FLG
      *    外来診療料算定有無
           MOVE    SPA-GAIRAISIN       TO  LNK-ORCSC-GAIRAISIN
      *
           MOVE    SPA-ROUMANSEI-ARI   TO  LNK-ORCSC-ROUMANSEI-ARI
           MOVE    SPA-ROUMANSEI-ARI2  TO  LNK-ORCSC-ROUMANSEI-ARI2
           MOVE    SPA-NETAKIRI-ARI    TO  LNK-ORCSC-NETAKIRI-ARI
           MOVE    SPA-NETAKIRI-ARI2   TO  LNK-ORCSC-NETAKIRI-ARI2
           MOVE    SPA-NETAKIRI-ARI3   TO  LNK-ORCSC-NETAKIRI-ARI3
           MOVE    SPA-SYONIKA-ARI     TO  LNK-ORCSC-SYONIKA-ARI
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-ORCSC-SYONIKA-ARI2
      *    生活習慣病指導管理料
           MOVE    SPA-SEIKATU-ARI     TO  LNK-ORCSC-SEIKATU-ARI
           MOVE    SPA-SEIKATU-ARI2    TO  LNK-ORCSC-SEIKATU-ARI2
           MOVE    SPA-NAI-GAIFLG      TO  LNK-ORCSC-GAIFLG
      *    複数診療科あり
           MOVE    SPA-FUKU-SRYKA      TO  LNK-ORCSC-FUKU-SRYKA
      *    訂正前診療科
           MOVE    SPA-OLD-SRYKA       TO  LNK-ORCSC-OLD-SRYKA
           MOVE    SPA-OLD-HKNCOMBI    TO  LNK-ORCSC-OLD-HKNCOMBI
      *    電話再診の有無
           MOVE    SPA-TELSAI-ARI      TO  LNK-ORCSC-TELSAI-ARI
      *    特定疾患あり
           MOVE    SPA-TOKUTEI         TO  LNK-ORCSC-TOKUTEI
           MOVE    SPA-TOKUTEIFLG      TO  LNK-ORCSC-TOKUTEIFLG
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    時間外加算特例乳幼児
           MOVE    SPA-JIKANTOKU-NYU   TO  LNK-ORCSC-JIKANTOKU-NYU
      *    診察回数
           MOVE    SPA-SINSATU-CNT     TO  LNK-ORCSC-SINSATU-CNT
      *   １５歳未満再診科算定有り
           MOVE    SPA-15MIMAN-ARI     TO  LNK-ORCSC-15MIMAN-ARI
      *    特定薬剤治療管理加算の初回算定日
           MOVE    SPA-TOKTEI-YMD      TO LNK-ORCSC-TOKTEI-YMD
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEI-KNJ     TO LNK-ORCSC-TOKUTEI-KNJ
           MOVE    SPA-TOKUTEI-KNJ2    TO LNK-ORCSC-TOKUTEI-KNJ2
      *    人工腎臓患者
           MOVE    SPA-TOKUTEI-JNZ     TO LNK-ORCSC-TOKUTEI-JNZ
      *    厚生省が定める患者
           MOVE    SPA-TOKUTEIG-KNJ    TO LNK-ORCSC-TOKUTEIG-KNJ
           MOVE    SPA-TOKUTEIG-KNJ2   TO LNK-ORCSC-TOKUTEIG-KNJ2
      *    再診チェック済
           MOVE    SPA-SAISIN-CHK      TO  LNK-ORCSC-SAISIN-CHK
      *    消炎鎮痛処置当月内算定
           MOVE    SPA-SYOEN-CNT       TO  LNK-ORCSC-SYOEN-CNT
      *    急性発生１８０日以内
           MOVE    SPA-KYUSEI-90       TO  LNK-ORCSC-KYUSEI-90
           MOVE    SPA-KYUSEI-180      TO  LNK-ORCSC-KYUSEI-180
           MOVE    SPA-KYUSEI-FLG      TO  LNK-ORCSC-KYUSEI-FLG
      *    急性発生日
           MOVE    WRK-KYUSEI-YMD      TO  LNK-ORCSC-KYUSEI-YMD
           MOVE    WRK-KYUSEI-END      TO  LNK-ORCSC-KYUSEI-END
      *    当月外来管理加算の取れない患者
           MOVE    SPA-GAIRAI-NASI     TO  LNK-ORCSC-GAIRAI-NASI
      *    労災用 外来管理点数
           MOVE    SPA-GAIRAI-TEN      TO  LNK-ORCSC-GAIRAI-TEN
      *
      *    継続領域
           MOVE    SPA-CONTKBN         TO  LNK-ORCSC-CONTKBN
           MOVE    SPA-DENPNUM         TO  LNK-ORCSC-DENPNUM
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-RENNUM          TO  LNK-ORCSC-DOUJI-RENNUM
           ELSE
               MOVE    SPA-DOUJI-RENNUM    TO  LNK-ORCSC-DOUJI-RENNUM
           END-IF
      *
      **** MOVE    SPA-K02-TEISEI-AREA TO  LNK-ORCSC-TEISEI-AREA
      *    外来管理加算削除のチェック
           MOVE    SPA-GAIKANRIKBN-CHK TO  LNK-ORCSC-GAIKANRIKBN-CHK
      *
      *    入院期間
           MOVE    SPA-K02N-NYUINYMD   TO  LNK-ORCSC-NYUINYMD
           MOVE    SPA-K02N-TAIINYMD   TO  LNK-ORCSC-TAIINYMD
      *    リハビリ逓減確認
           MOVE    ORCSC100-RIHTEI     TO  LNK-ORCSC-FLG-RIHTEI
      *    リハビリ算定回数
           MOVE    SPA-RIGAKU-NCNT-AREA    TO  LNK-RIGAKU-NCNT-AREA
      *    検査包括
           IF      ORCSC100-KBN        =   "3"
               MOVE    1                   TO  LNK-ORCSC-KENSAFLG
           END-IF
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
           MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-ORCSC-HAIKIKBN-FLG
      *
      *    小児科特例時間外チェック
           MOVE    ZERO                TO  LNK-ORCSC-JIKANERR-FLG
      **** MOVE    SPACE               TO  SPA-GMN-JIKANMSG
      *
      *            診療
           IF      SPA-SRYKBN-CNT (01) >   ZERO
               CALL    "ORCSC10"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *        小児科外来診療科の時、年齢エラーはそのまま
               IF      SPA-SYONIKA-ARI3    NOT =   ZERO
                   IF      SPA-ERRCD           =   "0032"
                       MOVE    SPACE               TO  SPA-ERRCD
                   END-IF
               END-IF
      *        厚生省が定める患者
               MOVE    LNK-ORCSC-TOKUTEI-KNJ   TO  SPA-TOKUTEI-KNJ
      *        人工腎臓患者
               MOVE    LNK-ORCSC-TOKUTEI-JNZ   TO  SPA-TOKUTEI-JNZ
      *        厚生省が定める患者
               MOVE    LNK-ORCSC-TOKUTEIG-KNJ  TO  SPA-TOKUTEIG-KNJ
      *        再診チェック済
               MOVE    LNK-ORCSC-SAISIN-CHK    TO  SPA-SAISIN-CHK
               MOVE    LNK-ORCSC-CHK-IDX       TO  SPA-CHK-IDX
      *        労災用 外来管理点数
               MOVE    LNK-ORCSC-GAIRAI-TEN    TO  SPA-GAIRAI-TEN
      *
      *        外来管理加算削除のチェック
               IF      ORCSC100-KBN        =   "3"
      **           IF      SPA-GAIKANRIKBN-CHK     =   "1"  OR  "2"
      *                IF     (LNK-ORCSC-KIDCD     =   "0102" )  AND
      *                       (LNK-ORCSC-KBN       =   "2"    )  AND
      *                       (ORCSC100-TOROKU     =   SPACE  )
      *                    MOVE    SPACE           TO  LNK-ORCSC-KIDCD
      *                END-IF
      ****         ELSE
                       IF      LNK-ORCSC-KIDCD     =   "0102"
                           IF     (SPA-GAIKANRIKBN-CHK  =   "4" ) AND
                                  (ORCSC100-TOROKU      =   SPACE  )
                             MOVE    SPACE           TO  LNK-ORCSC-KIDCD
                           ELSE
                             MOVE    SPACE           TO  SPA-COM-INPUTCD
                                                    (LNK-ORCSC-CHK-IDX)
                             MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                    (LNK-ORCSC-CHK-IDX)
                             MOVE    SPACE           TO  LNK-ORCSC-KIDCD
                             MOVE    1               TO  FLG-TUIKA
                           END-IF
                       END-IF
                   END-IF
      ******   END-IF
      *
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *
      *    投薬（更新前チェックはすべて行う）
           IF     (SPA-SRYKBN-CNT (02) >   ZERO )
               OR (LNK-ORCSC-KBN       =   "3"  )
               IF      LNK-ORCSC-KBN       =   "3"
                   INITIALIZE                  SPA-SYOHOU-ZUMI
                   MOVE    SPA-K01KYOUTU   TO  SPA-WORK
               END-IF
               CALL    "ORCSC20"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               IF      LNK-ORCSC-KBN       =   "3"
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
                   PERFORM 5201-SYOHOU-HKN-SEC
               END-IF
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *H28.4
      *    中途終了時は、投薬のみチェック
           IF     (LNK-ORCSC-KBN2      =   "1" )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      ****
      *            注射
           IF     (SPA-SRYKBN-CNT (03) >   ZERO )
      *            入院で手術の時も点滴チェックを行う
             OR  ((SPA-SRYKBN-CNT (05) >   ZERO  )  AND
                  (SPA-NYUGAIKBN       =   "1"     ))
      *            入院で画像診断の時も点滴チェックを行う
             OR  ((SPA-SRYKBN-CNT (08) >   ZERO  )  AND
                  (SPA-NYUGAIKBN       =   "1"     ))
               CALL    "ORCSC30"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            処置
           IF      SPA-SRYKBN-CNT (04) >   ZERO
               CALL    "ORCSC40"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            手術
           IF      SPA-SRYKBN-CNT (05) >   ZERO
               CALL    "ORCSC50"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            麻酔
           IF      SPA-SRYKBN-CNT (06) >   ZERO
               CALL    "ORCSC54"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
               MOVE    LNK-ORCSC-ERRMSG    TO  SPA-ERRMSG2
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            検査
           IF      SPA-SRYKBN-CNT (07) >   ZERO
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               CALL    "ORCSC60"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    LNK-NAIACCT-AREA    TO  SPA-NAIACCT-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
               MOVE    LNK-ORCSC-ERRMSG    TO  SPA-ERRMSG2
      *
               IF      LNK-ORCSC-KIDCD     NOT =   SPACE
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            画像診断
           IF      SPA-SRYKBN-CNT (08) >   ZERO
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               CALL    "ORCSC70"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            その他
           IF      SPA-SRYKBN-CNT (09) >   ZERO
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               CALL    "ORCSC80"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
               MOVE    LNK-ORCSC-ERRMSG    TO  SPA-ERRMSG2
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *            入院（食事）
           IF      SPA-SRYKBN-CNT (10) >   ZERO
               CALL    "ORCSCN97"      USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
               MOVE    LNK-ORCSC-CHK-IDX   TO  SPA-CHK-IDX
               MOVE    LNK-ORCSC-KIDCD     TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO    520-ALL-CHEK-EXT
           END-IF
      *
      *    変更の判定用に保存
           IF     (FLG-TUIKA           =   ZERO   )  AND
                  (SPA-SCR-REC     NOT =   WRK-SCR-REC )
               PERFORM 5209-HENKOUCHK-SEC
               MOVE    1               TO  FLG-TUIKA
           END-IF
      *    労災の時、読み替えあり
           IF      LNK-ORCSC-RSIOK     =   "1"
               MOVE    1               TO  FLG-TUIKA
           END-IF
      *
           .
       520-ALL-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    変更有無判定処理
      *****************************************************************
       5209-HENKOUCHK-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )
                           OR (FLG-TUIKA   =   1   )
               IF     (SPA-GMN-INPUTCD(IDX)    =   WRK-GMN-INPUTCD(IDX))
                 AND  (SPA-COM-INPUTCD(IDX)    =   WRK-COM-INPUTCD(IDX))
                 AND  (SPA-COM-SRYKBN (IDX)    =   WRK-COM-SRYKBN (IDX))
                 AND  (SPA-COM-SRYSYUKBN(IDX)  =   WRK-COM-SRYSYUKBN
                                                                  (IDX))
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-TUIKA
               END-IF
           END-PERFORM
           .
       5209-HENKOUCHK-EXT.
           EXIT.
      *****************************************************************
      *    他保険で算定済の保険編集処理
      *****************************************************************
       5201-SYOHOU-HKN-SEC              SECTION.
      *
           IF      SPA-SYOHO-HKN       =   SPACE
               MOVE    SPACE               TO  SPA-SYOHO-HKNMEI
           ELSE
               MOVE    SPACE               TO  SPA-SYOHO-HKNMEI
               PERFORM VARYING     IDX-HKN FROM    1   BY  1
                       UNTIL       IDX-HKN >   15
                   IF      SPA-SYOHO-HKN   =   SPA-GMN-THKNCOMBINUM
                                                              (IDX-HKN)
                       MOVE    SPA-GMN-THKNCOMBIMEI (IDX-HKN)
                                           TO  SPA-SYOHO-HKNMEI
                   END-IF
               END-PERFORM
           END-IF
      *
           IF       SPA-ERRCD      NOT =   SPACE
               MOVE    SPA-SCR-REC         TO  MAE-SCR-REC
               INITIALIZE                  SPA-SCR-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                                 ((MAE-GMN-INPUTCD (IDX) =   SPACE )
                              AND (MAE-COM-INPUTCD (IDX) =   SPACE ))
      *            （減）削除
                   IF     (MAE-COM-SRYKBN (IDX)    =   "21" )  AND
                          (MAE-COM-AUTOKBN(IDX)    =   "3" )
                       CONTINUE
                   ELSE
                       ADD     1                   TO  SPA-GMN-MAX
                       MOVE    MAE-GMN-TBL (IDX)   TO  SPA-GMN-TBL
                                                          (SPA-GMN-MAX)
                       IF     (SPA-COM-KZMFLG (IDX)     =   "1"  ) AND
                              (LNK-ORCSC-KEI-IDX        =   IDX  )
                           MOVE    SPA-GMN-MAX     TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       5201-SYOHOU-HKN-EXT.
           EXIT.
      *
      *H28.4
      *****************************************************************
      *    中途終了時、投薬チェック処理
      *****************************************************************
       200-TOUYAKUCHK-SEC          SECTION.
      *
      *    登録前の全体のチェックを行う
           INITIALIZE                  ORCSC00AREA
           MOVE    "2"                 TO  LNK-ORCSC-KBN
           MOVE    "1"                 TO  LNK-ORCSC-KBN2
           PERFORM 520-ALL-CHEK-SEC
           .
       200-TOUYAKUCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       300-TOUROKU-SYORI-SEC          SECTION.
      *
      *    確定前、自動算定追加
           INITIALIZE                  ORCSC00AREA
           MOVE    "3"                 TO  LNK-ORCSC-KBN
           PERFORM 520-ALL-CHEK-SEC
      *
      *H26.10
      *    （減）の自動発生行は削除
           IF     (SPA-ERRCD       NOT =   SPACE )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE      )   OR
                                  (SPA-GMN-INPUTCD (IDX)   =   SPACE )
                   IF     ( SPA-COM-INPUTCD (IDX)  =   "820000047"
                                                   OR  "820000166"
                                                   OR  CONS-SPCM-SRYCD)
                     AND  ( SPA-COM-AUTOKBN (IDX)  =   "3"        )
                        MOVE    SPACE       TO  SPA-GMN-INPUTCD(IDX)
                        MOVE    SPACE       TO  SPA-COM-INPUTCD(IDX)
                   END-IF
               END-PERFORM
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO  TO  300-TOUROKU-SYORI-EXT
           END-IF
      *
      *    自動作成分包括診療チェック初期設定
           IF     (SPA-HKTSANTEI-CHK   =   1      )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999" )
            AND   (SPA-CHIKENKBN       =   SPACE  )
               MOVE    SPA-K01KYOUTU       TO   SPA-WORK
               INITIALIZE                  ORCSCHKTCHKAREA
               MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
               MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
               MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
               MOVE    "2"                 TO  ORCSCHKTCHK-SYORI
               MOVE    "KT01"              TO  ORCSCHKTCHK-PG
      *        算定履歴からの包括を行なう
               MOVE    LNK-ORCSC-KT01-AREA TO  ORCSCHKTCHK-KT01-AREA
               CALL    "ORCSCHKTCHK"       USING
                                           ORCSCHKTCHKAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           ORCSC00AREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           END-IF
      *
      *    自動作成の診療行為作成処理
           INITIALIZE                  WRK-KASAN-AREA
           MOVE    ZERO                TO  WRK-KSN-IDX
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  IDX-S
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10   )  OR
                              (IDX-S   >=  60   )
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   30   )  OR
                                  (IDX-S   >=  60   )
                   IF      LNK-ORCSC-SRYCD (IDX IDY)
                                           NOT = SPACE
      *保留**********  PERFORM 3001-AUTOZAI-SYORI-SEC
                       PERFORM 3001-AUTOZAI-HEN-SEC
      *                算定履歴テーブル編集
                       IF      LNK-ORCSC-SANTEIRRKKBN(IDX IDY)
                                                   =   ZERO
                                                   OR  SPACE
                           CONTINUE
                       ELSE
                           PERFORM 3004-SANTEI-SET-SEC
                       END-IF
                   END-IF
      *H27.4
      *            複数加算対応（入院はないとする）
                   IF      LNK-ORCSC-SRYSYUKBN (IDX IDY) =  "NET"
                       PERFORM 3002-AUTOKSN-HEN-SEC
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           MOVE    IDX-S               TO  WRK-KSN-IDX
           INITIALIZE                  ORCSCKTWKSRYAREA
      *    自動発生分の編集
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   WRK-KSN-IDX
               MOVE    SPA-HKNCOMBINUM     TO  ORCSCWKSRY-KSN-HKNCOMBI
                                                                (IDX)
               MOVE    SPA-SRYKA           TO  ORCSCWKSRY-KSN-SRYKA
                                                                (IDX)
               MOVE    WRK-KSN-HKNCOMBI (IDX)
                                           TO  ORCSCWKSRY-KSN-HKNCOMBI
                                                                (IDX)
               MOVE    WRK-KSN-SRYKA (IDX) TO  ORCSCWKSRY-KSN-SRYKA
                                                                (IDX)
               MOVE    WRK-KSN-SRYSYUKBN (IDX)
                                           TO  ORCSCWKSRY-KSN-SRYSYUKBN
                                                                (IDX)
               MOVE    WRK-KSN-SRYSYUKBN (IDX)
                                           TO  ORCSCWKSRY-KSN-SRYSYUKBN
                                                                (IDX)
               MOVE    WRK-KSN-SRYKBN(IDX) TO  ORCSCWKSRY-KSN-SRYKBN
                                                                (IDX)
               MOVE    WRK-KSN-SRYCD(IDX)  TO  ORCSCWKSRY-KSN-SRYCD
                                                                (IDX)
               MOVE    WRK-KSN-INPUTCD(IDX)
                                           TO  ORCSCWKSRY-KSN-INPUTCD
                                                                (IDX)
               MOVE    WRK-KSN-ATAI-G (IDX)
                                           TO  ORCSCWKSRY-KSN-ATAI-G
                                                                (IDX)
               MOVE    WRK-KSN-TENSU (IDX) TO  ORCSCWKSRY-KSN-TENSU
                                                                (IDX)
               MOVE    WRK-KSN-SURYO (IDX) TO  ORCSCWKSRY-KSN-SURYO
                                                                (IDX)
               MOVE    WRK-KSN-ZAINUM(IDX) TO  ORCSCWKSRY-KSN-ZAINUM
                                                                (IDX)
               MOVE    WRK-KSN-ZAIEND(IDX) TO  ORCSCWKSRY-KSN-ZAIEND
                                                                (IDX)
               MOVE    WRK-KSN-INPUTCD(IDX) TO  ORCSCWKSRY-KSN-INPUTCD
                                                                (IDX)
               MOVE    WRK-KSN-AUTOKBN(IDX) TO  ORCSCWKSRY-KSN-AUTOKBN
                                                                (IDX)
               MOVE    WRK-KSN-HKTKBN (IDX) TO  ORCSCWKSRY-KSN-HKTKBN
                                                                (IDX)
           END-PERFORM
           MOVE    WRK-KSN-IDX         TO  ORCSCWKSRY-KSN-IDX
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    "3"                 TO  ORCSCWKSRY-KBN
           MOVE    "KT01"              TO  ORCSCWKSRY-PG
           MOVE    SPA-SRYYMD(7:2)     TO  ORCSCWKSRY-DAY
           CALL    "ORCSCKTWKSRY"      USING
                                       ORCSCKTWKSRYAREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           .
       300-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    自動作成の診療行為編集処理
      *****************************************************************
       3001-AUTOZAI-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-AUTOERR
      *    外来で後期高齢者診療料算定中の判定
           IF     (SPA-NYUGAIKBN       =   "2"  )  AND
                 ((SPA-KOURESRY-ARI    NOT =   ZERO ) OR
                 ( SPA-KOURESRY-ARI2   NOT =   ZERO ))
               PERFORM 30011-AUTO-KOURESRY-SEC
           END-IF
           IF      FLG-AUTOERR         =   ZERO
               PERFORM 3001-AUTOZAI-HEN-SEC
           END-IF
           .
       3001-AUTOZAI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者診療料算定中の判定処理
      *****************************************************************
       30011-AUTO-KOURESRY-SEC                SECTION.
      *
           EVALUATE    LNK-ORCSC-SRYKBN (IDX IDY)
      *        画像診断・検査・処置
               WHEN    "40"
               WHEN    "60"
               WHEN    "70"
      *            手技・加算で５５０点以下はエラー (包括剤以外）
                   IF    ( LNK-ORCSC-TENSU (IDX IDY)   <   550   )
                   AND   ( LNK-ORCSC-HKTKBN (IDX IDY)  =   ZERO  )
                       MOVE    1               TO  FLG-AUTOERR
                   END-IF
           END-EVALUATE
           .
       30011-AUTO-KOURESRY-EXT.
           EXIT.
      *****************************************************************
      *    自動作成の診療行為編集処理
      *****************************************************************
       3001-AUTOZAI-HEN-SEC                SECTION.
      *
           ADD     1                   TO  IDX-S
           ADD     1                   TO  WRK-ZAINUM
           MOVE    WRK-ZAINUM          TO  WRK-KSN-ZAINUM  (IDX-S)
           IF      LNK-ORCSC-HKNCOMBI  (IDX IDY)   =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  LNK-ORCSC-HKNCOMBI
                                                          (IDX IDY)
           END-IF
           IF      LNK-ORCSC-SRYKA  (IDX IDY)  =   SPACE
               MOVE    SPA-SRYKA           TO  LNK-ORCSC-SRYKA
                                                          (IDX IDY)
           END-IF
           MOVE    LNK-ORCSC-HKNCOMBI  (IDX IDY)
                                       TO  WRK-KSN-HKNCOMBI (IDX-S)
           MOVE    LNK-ORCSC-SRYKA     (IDX IDY)
                                       TO  WRK-KSN-SRYKA    (IDX-S)
           MOVE    LNK-ORCSC-SRYSYUKBN (IDX IDY)
                                       TO  WRK-KSN-SRYSYUKBN (IDX-S)
           MOVE    LNK-ORCSC-SRYKBN    (IDX IDY)
                                       TO  WRK-KSN-SRYKBN   (IDX-S)
           MOVE    LNK-ORCSC-SRYCD (IDX IDY)
                                       TO  WRK-KSN-SRYCD    (IDX-S)
           MOVE    LNK-ORCSC-TENSU (IDX IDY)
                                       TO  WRK-KSN-TENSU  (IDX-S)
           MOVE    LNK-ORCSC-SURYO (IDX IDY)
                                       TO  WRK-KSN-SURYO  (IDX-S)
           IF      LNK-ORCSC-AUTOKBN(IDX IDY)  =   SPACE
               MOVE    "3"                 TO  WRK-KSN-AUTOKBN  (IDX-S)
           ELSE
               MOVE    LNK-ORCSC-AUTOKBN(IDX IDY)
                                           TO  WRK-KSN-AUTOKBN  (IDX-S)
           END-IF
      *    包括剤
           MOVE    LNK-ORCSC-HKTKBN (IDX IDY)
                                           TO  WRK-KSN-HKTKBN  (IDX-S)
      *
           IF      LNK-ORCSC-SRYCD-KS(IDX IDY)   =   SPACE
               MOVE    "1"                 TO  WRK-KSN-ZAIEND (IDX-S)
           ELSE
      *        加算
               ADD     1                   TO  IDX-S
               IF      IDX-S               <=  60
                   MOVE    WRK-ZAINUM          TO  WRK-KSN-ZAINUM
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-SRYCD-KS(IDX IDY)
                                               TO  WRK-KSN-SRYCD
                                                              (IDX-S)
      *            コメントの入力値（算定日）
                   MOVE    LNK-ORCSC-ATAI-G  (IDX IDY)
                                           TO  WRK-KSN-ATAI-G
                                                               (IDX-S)
      *
                   MOVE    LNK-ORCSC-HKNCOMBI  (IDX IDY)
                                           TO  WRK-KSN-HKNCOMBI
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-SRYKA  (IDX IDY)
                                           TO  WRK-KSN-SRYKA (IDX-S)
                   MOVE    LNK-ORCSC-SRYSYUKBN (IDX IDY)
                                           TO  WRK-KSN-SRYSYUKBN
                                                             (IDX-S)
                   MOVE    LNK-ORCSC-SRYKBN    (IDX IDY)
                                           TO  WRK-KSN-SRYKBN (IDX-S)
                   MOVE    LNK-ORCSC-TENSU-KS (IDX IDY)
                                           TO  WRK-KSN-TENSU  (IDX-S)
                   MOVE    "1"             TO  WRK-KSN-ZAIEND (IDX-S)
                   IF      LNK-ORCSC-AUTOKBN(IDX IDY)  =   SPACE
                       MOVE    "3"         TO  WRK-KSN-AUTOKBN (IDX-S)
                   ELSE
                       MOVE    LNK-ORCSC-AUTOKBN(IDX IDY)
                                           TO  WRK-KSN-AUTOKBN (IDX-S)
                   END-IF
      *            包括剤
                   MOVE    LNK-ORCSC-HKTKBN (IDX IDY)
                                           TO  WRK-KSN-HKTKBN  (IDX-S)
               END-IF
           END-IF
      *    回数（入院）
           PERFORM VARYING IDN1        FROM    1   BY  1
                   UNTIL   (IDN1        >   5  )    OR
                           (LNK-ORCSC-INPUTCD (IDX IDY IDN1)
                                        =   SPACE)
               MOVE    SPACE               TO  WRK-KSN-ZAIEND (IDX-S)
               ADD     1                   TO  IDX-S
               IF      IDX-S               <=  60
                   MOVE    WRK-ZAINUM      TO  WRK-KSN-ZAINUM
                                                              (IDX-S)
                   MOVE    "1"             TO  WRK-KSN-ZAIEND (IDX-S)
                   MOVE    LNK-ORCSC-SRYSYUKBN (IDX IDY)
                                           TO  WRK-KSN-SRYSYUKBN
                                                              (IDX-S)
                   MOVE    LNK-ORCSC-SRYKBN    (IDX IDY)
                                           TO  WRK-KSN-SRYKBN (IDX-S)
                   MOVE    LNK-ORCSC-INPUTCD (IDX IDY IDN1)
                                           TO  WRK-KSN-INPUTCD (IDX-S)
               END-IF
           END-PERFORM
           .
       3001-AUTOZAI-HEN-EXT.
           EXIT.
      *H27.4
      *****************************************************************
      *    複数加算対応処理
      *****************************************************************
       3002-AUTOKSN-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-KSN-ZAIEND (IDX-S)
      *    追加の加算
           ADD     1                   TO  IDX-S
           IF      IDX-S               <=  60
               MOVE    WRK-ZAINUM          TO  WRK-KSN-ZAINUM
                                                              (IDX-S)
               MOVE    LNK-ORCSC-SRYCD-KS(IDX IDY)
                                               TO  WRK-KSN-SRYCD
                                                              (IDX-S)
      *        コメントの入力値（算定日）
               MOVE    LNK-ORCSC-ATAI-G  (IDX IDY)
                                           TO  WRK-KSN-ATAI-G (IDX-S)
      *        前行を複写
               MOVE    WRK-KSN-HKNCOMBI  (IDX-S - 1)
                                           TO  WRK-KSN-HKNCOMBI
                                                              (IDX-S)
      *        MOVE    WRK-KSN-CHIKENKBN (IDX-S - 1)
      *                                    TO  WRK-KSN-CHIKENKBN
      *                                                       (IDX-S)
               MOVE    WRK-KSN-SRYKA     (IDX-S - 1)
                                           TO  WRK-KSN-SRYKA (IDX-S)
               MOVE    WRK-KSN-SRYSYUKBN (IDX-S - 1)
                                           TO  WRK-KSN-SRYSYUKBN
                                                             (IDX-S)
               MOVE    WRK-KSN-SRYKBN    (IDX-S - 1)
                                           TO  WRK-KSN-SRYKBN (IDX-S)
               MOVE    WRK-KSN-AUTOKBN  (IDX-S - 1)
                                           TO  WRK-KSN-AUTOKBN(IDX-S)
               MOVE    WRK-KSN-HKTKBN  (IDX-S - 1)
                                           TO  WRK-KSN-HKTKBN (IDX-S)
      *
               MOVE    LNK-ORCSC-TENSU-KS (IDX IDY)
                                           TO  WRK-KSN-TENSU  (IDX-S)
               MOVE    "1"                 TO  WRK-KSN-ZAIEND (IDX-S)
           END-IF
           .
       3002-AUTOKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数処理
      *****************************************************************
       3004-SANTEI-SET-SEC             SECTION.
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-N2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   100  )
               IF      SPA-SAN-SRYCD (IDX2)    =   SPACE
                   MOVE    LNK-ORCSC-SRYCD (IDX IDY)
                                               TO  SPA-SAN-SRYCD (IDX2)
                   MOVE    IDX2                TO  SPA-SANTEI-MAX
               END-IF
               IF      LNK-ORCSC-SRYCD (IDX IDY)
                                               =   SPA-SAN-SRYCD (IDX2)
      *            回数を設定する
                   ADD     1                   TO   SPA-SAN-DAY
                                                           (IDX2 IDX-N2)
                   MOVE    100             TO  IDX2
               END-IF
          END-PERFORM
           .
       3004-SANTEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    特定の診療行為の有無処理
      *****************************************************************
       6100-TOKUTEICHK-SEC                SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSCKT10SAREA
           MOVE    "6"                 TO  ORCSCKT10S-SYORI
           CALL    "ORCSCKT10S"        USING
                                       ORCSCKT10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-KT01
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       6100-TOKUTEICHK-EXT.
           EXIT.
      *****************************************************************
      *    特定の診療行為の有無処理
      *****************************************************************
       6200-TOKUTEIHANTEI-SEC                SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSCKT10SAREA
           MOVE    "7"                 TO  ORCSCKT10S-SYORI
           CALL    "ORCSCKT10S"        USING
                                       ORCSCKT10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-KT01
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       6200-TOKUTEIHANTEI-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       9101-TENSU-READ2-SEC              SECTION.
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
       9101-TENSU-READ2-EXT.
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
