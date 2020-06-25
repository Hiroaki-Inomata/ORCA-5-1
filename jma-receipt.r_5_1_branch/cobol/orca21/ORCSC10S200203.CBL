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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC10S200203.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為診察料自動発生編集処理
      *                      （Ｋ０２のみ共通使用）
      *                      Ｈ１４．４改正まで
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  05/11/18  MONFUNC 対応 他
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
           03  FLG-JYURRK          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-SYOSIN          PIC 9(01).
      *
           03  FLG-ROUMANSEI       PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
           03  FLG-SYONIKA-ARI     PIC 9(01).
      *    寝たきり老人算定
           03  FLG-TOUG-SANTEI         PIC 9(03).
           03  FLG-JYURR-ARI           PIC 9(01).
      *
      *    特定疾患自動算定チェック用
           03  FLG-KYUJITU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-IDX-IN              PIC 9(04).
      *    算定回数
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    年齢判定用
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
           03  WRK-NYUYOJI         PIC 9(04).
           03  WRK-DAYCNT          PIC 9(04).
      *
           03  WRK-KEIYMD          PIC X(08).
      *
      *    算定年月
           03  WRK-SANTEI-YM           PIC X(06).
           03  WRK-SANTEI-ARI          PIC X(06).
           03  WRK-SANTEI-NASI         PIC X(06).
      *
      *    老人慢性疾病診察料回数
           03  WRK-ROUMANSEI           PIC 9(04).
           03  WRK-ROUMANSEI2          PIC 9(04).
           03  WRK-ROUMANSEI-CNT       PIC 9(04).
           03  WRK-GAIRAI-IDX          PIC 9(04).
      *
      *    寝たきり回数
           03  WRK-NETAKIRI-CNT    PIC 9(04).
      *
      *    在宅自己注射
           03  WRK-ZAITAKU             PIC 9(01).
           03  WRK-ZAITAKU-IDX         PIC 9(04).
      *
      *    パス名
           03  WRK-PATH                PIC X(64).
      *
      *    来院回数
           03  WRK-SINSATU-CNT         PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
      *    COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *    COPY    "CPORCSPTID.INC".
      *
      *    負担金額計算用パラメタ
      *    COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
      *    COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      *    COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
      *    COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
      *    COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
      *    COPY    "CPORCSC96.INC".
      *
      *    エラーメッセージ編集パラメタ
      *    COPY    "CPORCSC97.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSC10S.INC".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSC10SAREA
           SPA-AREA
           SPA-SCR-REC
           SPA-K02
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
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           EVALUATE    LNK-SC10S-SYORI
      *        初期自動発生編集
               WHEN    "1"
                   PERFORM 100-SANTEI-CHK-SEC
                   PERFORM 200-SYOSIN-SET-SEC
      *        同時再診算定（自動）
               WHEN    "2"
                   PERFORM 300-DOUSAISIN-SET-SEC
      *        算定回数チェック
               WHEN    "3"
                   PERFORM 100-SANTEI-CHK-SEC
      *        院内院外変更
               WHEN    "4"
                   PERFORM 400-INGAI-HEN-SEC
      *        同時再診算定（入力分）
               WHEN    "5"
                   PERFORM 500-INDOUSAISIN-SET-SEC
      *        剤分離前の特定算定回数チェック
               WHEN    "6"
                   PERFORM 600-SANTEI-CNT-SEC
      *        剤分離後の特定算定チェック
               WHEN    "7"
                   PERFORM 700-SANTEI-CHK-SEC
           END-EVALUATE
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    IDX2                TO  LNK-SC10S-IDX2
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    各診察算定回数処理
      *****************************************************************
       100-SANTEI-CHK-SEC                  SECTION.
      *
      *    訂正の時、初診料を算定中かの判定
           EVALUATE    LNK-SC10S-SYORI
               WHEN    "3"
                   PERFORM VARYING     IDX     FROM    1   BY  1
                        UNTIL      (IDX     >   SPA-MAX-LINE    )  OR
                                  (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                                   SPA-GMN-INPUTCD(IDX) =   SPACE )
                       EVALUATE    SPA-COM-INPUTCD (IDX)
                           WHEN    "111700110"
                           WHEN    "111700210"
                           WHEN    "111000110"
                           WHEN    "111003610"
                           WHEN    "113003510"
                           WHEN    "113003710"
                               MOVE    1              TO  SPA-SYOSIN
                       END-EVALUATE
                   END-PERFORM
           END-EVALUATE
      *
      *
      *    受診履歴より診察回数を求める
           PERFORM 33042-SINSATU-KAISU-SEC
      *
      *    小児科外来診療科
           IF     (SPA-SYONIKA         =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               PERFORM 1001-SYONIKA-CHK-SEC
           END-IF
      *
      *    寝たきり老人在宅総合診療科算定処理
           IF     (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               PERFORM 1002-NETAKIRI-CHK-SEC
           END-IF
      *
      *    老人慢性疾患外来総合診療科算定
           IF     (SPA-ROUMANSEI       =   1   )  AND
                  (SPA-SYOSIN      NOT =   1   )  AND
                  (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               PERFORM 1003-ROUMANSEI-CHK-SEC
           END-IF
      *
           .
       100-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科回数処理
      *****************************************************************
       1001-SYONIKA-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-SYONIKA-ARI
      *
      *    ３歳になる年月日を計算
           MOVE    SPA-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE WRK-BIRTH-YY        =   WRK-BIRTH-YY    +  3
      *    3歳未満に適用
           IF      WRK-BIRTHDAY(1:6)   <   SPA-SRYYMD (1:6)
               GO      TO      1001-SYONIKA-CHK-EXT
           END-IF
      *
      *    当月算定チェック
           MOVE    ZERO                TO  WRK-NYUYOJI
           MOVE    ZERO                TO  WRK-DAYCNT
      *    院外処方   初診
           MOVE    "113003510"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
      *    院外処方   再診
           MOVE    "113003610"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
      *    院内処方   初診
           MOVE    "113003710"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
      *    院内処方   再診
           MOVE    "113003810"         TO  WRK-SRYCD
           PERFORM 33031-JYURRK-CHK-SEC
           ADD     SANTEI-MSANTEICNT   TO  WRK-NYUYOJI
      *
      *    ３歳の当月時、当月算定がなければ対象外
           IF     (SPA-NENREI-YY   =   3               )  AND
                  (WRK-BIRTHDAY(1:6)   =   SPA-SRYYMD (1:6))
               IF      WRK-NYUYOJI         =   ZERO
                   GO      TO      1001-SYONIKA-CHK-EXT
               END-IF
           END-IF
      *
      *    当日算定があればフラグを立てる
           IF      WRK-DAYCNT          >   ZERO
               MOVE    1                   TO  SPA-SYONIKA-ARI2
           ELSE
               MOVE    ZERO                TO  SPA-SYONIKA-ARI2
           END-IF
      *    小児科対象
           MOVE    1                   TO  FLG-SYONIKA-ARI
      *
           .
       1001-SYONIKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    寝たきり老人在宅総合診療科自動算定処理
      *****************************************************************
       1002-NETAKIRI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUG-SANTEI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI2
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
      *    院外処方あり
           MOVE    "114701210"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >=  1
               MOVE    1                   TO  SPA-NETAKIRI-ARI2
           ELSE
               MOVE    WRK-SANTEI-YM       TO  WRK-SANTEI-ARI
      *        院外処方なし
               MOVE    "114701710"         TO  WRK-SRYCD
               PERFORM 33051-JYURRK-MAECHK-SEC
      *        当月算定あり
               IF      FLG-TOUG-SANTEI     >=  1
                   MOVE    2                   TO  SPA-NETAKIRI-ARI2
               END-IF
               MOVE    WRK-SANTEI-YM       TO  WRK-SANTEI-NASI
           END-IF
      *
      *    当月算定回数
           MOVE    WRK-SANTEI-CNT      TO  WRK-NETAKIRI-CNT
      *
           .
       1002-NETAKIRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    老人慢性疾患外来総合診療科算定回数処理
      *****************************************************************
       1003-ROUMANSEI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ROUMANSEI
           MOVE    ZERO                TO  SPA-ROUMANSEI-ARI
           MOVE    ZERO                TO  SPA-ROUMANSEI-ARI2
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           MOVE    ZERO                TO  WRK-ROUMANSEI-CNT
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    老人慢性疾病外来診療科の算定チェック
           MOVE    ZERO                TO  WRK-ROUMANSEI
           MOVE    ZERO                TO  WRK-ROUMANSEI2
      *
      *    院外処方
           MOVE    "113701010"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-ROUMANSEI
           END-IF
      *    院内処方１回目
           MOVE    "113701110"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-ROUMANSEI
               ADD     1                   TO  WRK-ROUMANSEI2
           END-IF
      *    院内処方２回目
           MOVE    "113701510"         TO  WRK-SRYCD
           PERFORM 33051-JYURRK-MAECHK-SEC
      *    当月算定あり
           IF      FLG-TOUG-SANTEI     >   ZERO
               ADD     FLG-TOUG-SANTEI     TO  WRK-ROUMANSEI
           END-IF
      *
      *    算定回数
           MOVE    WRK-SANTEI-CNT      TO  WRK-ROUMANSEI-CNT
      *
      *    当月に算定時、フラグをセットする
           IF      WRK-ROUMANSEI       >   ZERO
               MOVE    1                   TO  SPA-ROUMANSEI-ARI
               IF      WRK-ROUMANSEI2      >   ZERO
                   MOVE    2                   TO  SPA-ROUMANSEI-ARI
               END-IF
           END-IF
      *
      *    算定回数セット
           MOVE    WRK-ROUMANSEI       TO  SPA-ROUMANSEI-ARI2
           .
       1003-ROUMANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       200-SYOSIN-SET-SEC              SECTION.
      *
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    小児科外来診療科
           IF     (SPA-SYONIKA         =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )  AND
                  (FLG-SYONIKA-ARI     =   1   )
               PERFORM 3303-SYONIKA-SET-SEC
               IF      FLG-SYONIKA         =   1
                   GO      TO      200-SYOSIN-SET-EXT
               END-IF
           END-IF
      *
      *    初診料算定
           IF      SPA-SYOSIN          =   1
               EVALUATE    SPA-NAI-ROUJIN  ALSO  SPA-HOSPSBT
      *                老人　病院
                   WHEN    1        ALSO         1
                       MOVE    "111700110"         TO  WRK-SRYCD
      *                老人、診療所
                   WHEN    1        ALSO         2
                       MOVE    "111700210"         TO  WRK-SRYCD
      *                老人外、病院  
                   WHEN    ZERO     ALSO         1
                       MOVE    "111000110"         TO  WRK-SRYCD
      *                老人外、診療所
                   WHEN    ZERO     ALSO         2
                       MOVE    "111003610"         TO  WRK-SRYCD
               END-EVALUATE
      *
           ELSE
      *        同日再診料算定
               IF      SPA-LSTYMD          =   SPA-SRYYMD  OR
                       SPA-RRK-LSTYMD      =   SPA-SRYYMD
                   PERFORM 3301-DOUSAISIN-SEC
               ELSE
      *        再診料算定
                   EVALUATE    SPA-NAI-ROUJIN   ALSO   SPA-HOSPSBT
      *                老人　病院
                       WHEN    1       ALSO          1
                           MOVE    "112700210"         TO  WRK-SRYCD
      *                老人、診療所
                       WHEN    1       ALSO          2
                           MOVE    "112700310"         TO  WRK-SRYCD
      *                老人外、病院  
                       WHEN    ZERO    ALSO          1
                           MOVE    "112000110"         TO  WRK-SRYCD
      *                老人外、診療所
                       WHEN    ZERO    ALSO          2
                           MOVE    "112000210"         TO  WRK-SRYCD
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    初診料自動追加処理
           PERFORM 3301-SYOSIN-SYORI-SEC
           IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    "0109"              TO  SPA-KIDCD
           END-IF
      *
      *    指導料の自動発生
           PERFORM 33012-JIDOU-SYORI-SEC
           .
       200-SYOSIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       3301-SYOSIN-SYORI-SEC                  SECTION.
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    以降の自動算定なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
           ADD     1                   TO  IDX2
           IF      WRK-SRYCD(1:2)      =   "83"
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    以降、再診時のみ
           IF      SPA-SYOSIN          =   1
               GO      TO      3301-SYOSIN-SYORI-EXT
           END-IF
      *
      *    再診料算定（幼児加算）
           MOVE    "112001070"         TO  WRK-SRYCD
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
               MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
           END-IF
      *
      *    外来管理加算コード追加
           EVALUATE    SPA-NAI-ROUJIN   ALSO   SPA-HOSPSBT
      *            老人　病院
               WHEN    1       ALSO          1
                   MOVE    "112701410"         TO  WRK-SRYCD
      *            老人、診療所
               WHEN    1       ALSO          2
                   MOVE    "112701510"         TO  WRK-SRYCD
      *            老人外
               WHEN    ZERO    ALSO          ANY
                   MOVE    "112005610"         TO  WRK-SRYCD
           END-EVALUATE
      *
           PERFORM 3301-SYOSIN-HEN-SEC
           MOVE    ZERO                TO  WRK-GAIRAI-IDX
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
      *********MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
               MOVE    SPA-GMN-IDX     TO  WRK-GAIRAI-IDX
           END-IF
      *
      *    継続管理加算追加
      *    　初診料算定月の判定
           IF      SPA-SYOYMD(1:6)     =   SPA-SRYYMD(1:6)
               IF      SPA-SYOYMD      <=  SPA-SRYYMD
                   CONTINUE
               ELSE
      *            継続管理加算追加処理
                   PERFORM 3302-KEIZOKU-HEN-SEC
               END-IF
           ELSE
      *        継続管理加算追加処理
               PERFORM 3302-KEIZOKU-HEN-SEC
           END-IF
      *
      *    寝たきり老人在宅総合診療科算定処理
           IF     (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    ZERO            TO  WRK-IDX-IN
               PERFORM 3305-NETAKIRI-SET-SEC
               IF     (SPA-NETAKIRI-ARI    NOT =   ZERO )  OR
                      (SPA-NETAKIRI-ARI2   NOT =   ZERO )
      *        寝たきり老人が当月算定済みの時、老人慢性疾患を算定しない
                   GO      TO      3301-SYOSIN-SYORI-EXT
               END-IF
           END-IF
      *
      *    老人慢性疾患外来総合診療科算定
           IF     (SPA-ROUMANSEI       =   1   )  AND
                  (SPA-SYOSIN      NOT =   1   )  AND
                  (SPA-NAI-ROUJIN      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    ZERO                TO  WRK-IDX-IN
               PERFORM 3304-ROUMANSEI-SET-SEC
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *    老人慢性疾患外来総合診療科が当月算定時で３回目の再診料算算定
      *           以外の時、外来管理加算は削除
           IF     (SPA-ROUMANSEI-ARI   =   ZERO )  AND
                  (SPA-ROUMANSEI-ARI2  NOT =   ZERO )  AND
                  (SPA-SINSATU-CNT     >=  3    )  AND
                  (WRK-GAIRAI-IDX      >   ZERO )
               MOVE    1                   TO  SPA-NAI-GAIFLG
               INITIALIZE              SPA-GMN-TBLREC (WRK-GAIRAI-IDX)
               INITIALIZE              SPA-COM-TBLREC (WRK-GAIRAI-IDX)
           END-IF
      *
           .
       3301-SYOSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料自動発生処理
      *****************************************************************
       3301-DOUSAISIN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *
           IF     (SPA-TAKA-SRYKA-R    =   SPACE    )
      *        同日再診料
               EVALUATE    SPA-NAI-ROUJIN  ALSO  SPA-HOSPSBT
      *            老人　病院
                   WHEN    1        ALSO         1
                       MOVE    "112700550"         TO  WRK-SRYCD
      *            老人、診療所
                   WHEN    1        ALSO         2
                       MOVE    "112700850"         TO  WRK-SRYCD
      *            老人外、病院  
                   WHEN    ZERO     ALSO         1
                       MOVE    "112003250"         TO  WRK-SRYCD
      *            老人外、診療所
                   WHEN    ZERO     ALSO         2
                       MOVE    "112003350"         TO  WRK-SRYCD
               END-EVALUATE
           ELSE
               IF      SPA-SYOYMD          =   SPA-SRYYMD
                   MOVE    "830000020"         TO  WRK-SRYCD
               ELSE
                   MOVE    "830000021"         TO  WRK-SRYCD
               END-IF
      *
               MOVE    "0109"              TO  SPA-KIDCD
           END-IF
      *
           .
       3301-DOUSAISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動算定処理
      *****************************************************************
       3301-SYOSIN-HEN-SEC                SECTION.
      *
           IF     (WRK-SRYCD(1:2)      =   "83"  )   AND
                  (SPA-GMN-IDX         =   ZERO  )
               ADD     1                   TO  IDX2
               ADD     1                   TO  SPA-GMN-IDX
      *        診療区分編集
               IF      WRK-SRYCD           =   "830000020"
                   MOVE    ".110"          TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   MOVE    "110"           TO  SPA-COM-SRYSYUKBN
                                                         (SPA-GMN-IDX)
                   MOVE    "11"            TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    ".120"          TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   MOVE    "120"           TO  SPA-COM-SRYSYUKBN
                                                         (SPA-GMN-IDX)
                   MOVE    "12"            TO  SPA-COM-SRYKBN
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           ADD     1                   TO  SPA-GMN-IDX
           IF      WRK-SRYCD(1:2)      =   "83"
               MOVE    SPA-TAKA-SRYKA(1)   TO  SPA-COM-ATAI1
                                                         (SPA-GMN-IDX)
               MOVE    1                   TO  SPA-FUKU-SRYKA
           END-IF
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                       -   1
               GO      TO      3301-SYOSIN-HEN-EXT
           END-IF
      *
           IF      WRK-SRYCD(1:2)      =   "83"
      *        再編集処理
               MOVE    SPA-GMN-IDX         TO  IDX
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ADD
           .
       3301-SYOSIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K02"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    継続管理加算追加処理
      *****************************************************************
       3302-KEIZOKU-HEN-SEC                SECTION.
      *
           IF      SPA-NAI-ROUJIN      =   ZERO
      *        一般
               MOVE    "112007170"         TO  WRK-SRYCD
           ELSE
      *        老人
               MOVE    "112702270"         TO  WRK-SRYCD
           END-IF
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-HEN-SEC
      *    自動算定あり
           IF      FLG-ADD             =   1
               ADD     1               TO  IDX2
      *        自動追加分
      *********MOVE    "1"             TO  SPA-COM-AUTOKBN(SPA-GMN-IDX)
           END-IF
      *
           .
       3302-KEIZOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科自動発生処理
      *****************************************************************
       3303-SYONIKA-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYONIKA
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *     院外処方
           IF      SPA-INGAIKBN        =   "1"
               IF      SPA-SYOSIN          =   1
      *            初診
                   MOVE    "113003510"         TO  WRK-SRYCD
               ELSE
      *            再診
                   MOVE    "113003610"         TO  WRK-SRYCD
               END-IF
           ELSE
      *        院内処方
               IF      SPA-SYOSIN          =   1
      *            初診
                   MOVE    "113003710"         TO  WRK-SRYCD
               ELSE
      *            再診
                   MOVE    "113003810"         TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    小児科外来診療科算定
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      WRK-IDX-IN          =   ZERO
               ADD     1                   TO  IDX2
               ADD     1                   TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IDX-IN          TO  IDX2
               MOVE    WRK-IDX-IN          TO  SPA-GMN-IDX
           END-IF
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           IF      WRK-IDX-IN          =   ZERO
      *        初期算定
               MOVE    "1"                 TO  LNK-SC92-KBN
               MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           ELSE
      *        院外院内変更
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    WRK-IDX-IN          TO  LNK-SC92-GMN-IDX
               INITIALIZE                      SPA-COM-TBLREC
                                                        (WRK-IDX-IN)
               MOVE    WRK-SRYCD           TO  SPA-COM-INPUTCD
                                                        (WRK-IDX-IN)
           END-IF
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    年齢エラーの時、エラーをはずす（当月はＯＫ）
           IF      LNK-SC92-ERRCD5 NOT =   SPACE
               MOVE    SPACE               TO  LNK-SC92-ERRCD5
           END-IF
      *
      *    同日再診時、併用算定エラー
           IF      LNK-SC92-ERRCD3 NOT =   SPACE
               MOVE    SPACE               TO  SPA-ERRCD
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    ZERO                TO  SPA-GMN-IDX
               MOVE    ZERO                TO  IDX2
               MOVE    1                   TO  FLG-SYONIKA
               MOVE    2                   TO  SPA-SYONIKA-ARI
               MOVE    1                   TO  SPA-SYONIKA-ARI2
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-IDX
               MOVE    ZERO            TO  IDX2
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    算定があり、既に当日他の診察料を算定している時、エラー
           IF     (SPA-SYOSIN          =   ZERO  )  AND
                  (SPA-LSTYMD          =   SPA-SRYYMD)
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    1               TO  FLG-SYONIKA
               MOVE    ZERO            TO  SPA-GMN-IDX
               MOVE    ZERO            TO  IDX2
               MOVE    "K005"          TO  SPA-ERRCD
               GO      TO      3303-SYONIKA-SET-EXT
           END-IF
      *
      *    入力項目編集処理
      *    INITIALIZE                  WRK-MOJI-AREA
      *    PERFORM 800-KOMOKU-HENSYU-SEC
      *
      *    再編集処理
           MOVE    SPA-GMN-IDX         TO  IDX
           PERFORM 3101-INPUTCD-HEN-SEC
      *
           MOVE    1                   TO  FLG-SYONIKA
           MOVE    1                   TO  SPA-SYONIKA-ARI
           .
       3303-SYONIKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       33031-JYURRK-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURR-ARI
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      33031-JYURRK-CHK-EXT
           END-IF
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   1
               MOVE    SPACE               TO  SANTEI-REC
               INITIALIZE              SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
      *    初回算定日＜診療日であること
           IF     (SANTEI-MSANTEID     >   ZERO           ) AND
                  (SANTEI-MSANTEID     <   SPA-SRYYMD(7:2))
               MOVE    1                   TO  FLG-JYURR-ARI
           END-IF
      *    上限１日
           IF     (TNS-JGNCNT1D        >   ZERO  )  AND
                  (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    SANTEI-DAY (IDX-Y2) TO  WRK-DAYCNT
           END-IF
      *
           .
       33031-JYURRK-CHK-EXT.
           EXIT.
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *****************************************************************
      *    老人慢性疾患外来総合診療科自動発生処理
      *****************************************************************
       3304-ROUMANSEI-SET-SEC          SECTION.
      *
      *    過去に算定が無い時算定なし
           IF     (WRK-ROUMANSEI-CNT   =   ZERO )  AND
                  (WRK-IDX-IN          =   ZERO )
               GO      TO      3304-ROUMANSEI-SET-EXT
           END-IF
      *    ２回以上は算定なし
           IF     (SPA-ROUMANSEI-ARI2  >=  2    )
            AND   (WRK-IDX-IN          =   ZERO )
      *********OR (SPA-ROUMANSEI-ARI       =   ZERO)
               MOVE    ZERO            TO  SPA-ROUMANSEI-ARI
               GO      TO      3304-ROUMANSEI-SET-EXT
           END-IF
      *
      *     院外処方
           IF      SPA-INGAIKBN        =   "1"
               MOVE    "113701010"         TO  WRK-SRYCD
           ELSE
      *        院内処方
               IF      WRK-ROUMANSEI       =   ZERO
      *            １回目
                   MOVE    "113701110"         TO  WRK-SRYCD
               ELSE
      *            ２回目
                   MOVE    "113701510"         TO  WRK-SRYCD
               END-IF
           END-IF
      *
      *    追加共通処理
           PERFORM 33041-SRYCD-ADD-SEC
      *    追加なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3304-ROUMANSEI-SET-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROUMANSEI
      *
           IF      WRK-SRYCD           =   "113701110"  OR
                                           "113701510"
               MOVE    2                   TO  SPA-ROUMANSEI-ARI
           ELSE
               IF      SPA-ROUMANSEI-ARI   =   ZERO
                   MOVE    1                   TO  SPA-ROUMANSEI-ARI
               END-IF
           END-IF
      *
      *    外来管理加算のチェックを行う
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      TNS-GAIKANRIKBN     =   ZERO
               MOVE    ZERO                TO  WRK-GAIRAI-IDX
           END-IF
      *
      *    外来加算料を削除する
           IF      WRK-GAIRAI-IDX      >   ZERO
               INITIALIZE              SPA-GMN-TBLREC (WRK-GAIRAI-IDX)
               INITIALIZE              SPA-COM-TBLREC (WRK-GAIRAI-IDX)
           END-IF
           .
       3304-ROUMANSEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴より診察回数を求める処理
      *****************************************************************
       33042-SINSATU-KAISU-SEC          SECTION.
      *
      *    受診履歴より診察回数を求める
      *    受診歴を検索する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *
           MOVE    "JYURRK-KEY8"       TO  WRK-PATH
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  WRK-SINSATU-CNT
           PERFORM
                   UNTIL   (FLG-JYURRK     =   1  )  OR
                           (WRK-SINSATU-CNT    >   3  )
               IF      JYURRK-SRYYMD(1:6)  <   SPA-SRYYMD(1:6)
                   MOVE    1               TO  FLG-JYURRK
               ELSE
                   IF       JYURRK-SRYYMD(1:6) =   SPA-SRYYMD(1:6)
                       IF     (JYURRK-EDANUM   =   1   )  AND
                              (JYURRK-DOUJI-RENNUM
                                              =   1  OR ZERO )
                           ADD     1               TO  WRK-SINSATU-CNT
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-JYURRK          =   ZERO
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM 970-JYURRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-SINSATU-CNT     TO  SPA-SINSATU-CNT
      *
           .
       33042-ROUMASEI-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    追加共通処理処理
      *****************************************************************
       33041-SRYCD-ADD-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      WRK-IDX-IN          =   ZERO
               ADD     1                   TO  IDX2
               ADD     1                   TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IDX-IN          TO  IDX2
               MOVE    WRK-IDX-IN          TO  SPA-GMN-IDX
           END-IF
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           IF      WRK-IDX-IN          =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
               MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           ELSE
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    WRK-IDX-IN          TO  LNK-SC92-GMN-IDX
               INITIALIZE                      SPA-COM-TBLREC
                                                        (WRK-IDX-IN)
               MOVE    WRK-SRYCD           TO  SPA-COM-INPUTCD
                                                        (WRK-IDX-IN)
           END-IF
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX     -   1
               COMPUTE IDX2            =   IDX2            -   1
               GO      TO      33041-SRYCD-ADD-EXT
           END-IF
      *
           MOVE    1                  TO  FLG-ADD
      *
           .
       33041-SRYCD-ADD-EXT.
           EXIT.
      *****************************************************************
      *    寝たきり老人在宅総合診療科自動算定処理
      *****************************************************************
       3305-NETAKIRI-SET-SEC          SECTION.
      *
      *    当月算定済み 自動作成なし
           IF    ((SPA-NETAKIRI-ARI2   NOT =   ZERO )  OR
                  (WRK-NETAKIRI-CNT        =   ZERO ))  AND
                  (WRK-IDX-IN              =   ZERO )
               GO      TO      3305-NETAKIRI-SET-EXT
           END-IF
      *
      *    自動算定
           IF    ( WRK-SANTEI-ARI      >   WRK-SANTEI-NASI )  OR
                 ( SPA-INGAIKBN        =   "1"             )
      *        院外処方あり
               MOVE    "114701210"         TO  WRK-SRYCD
               MOVE    1                   TO  SPA-NETAKIRI-ARI
           ELSE
      *        院外処方なし
               MOVE    "114701710"         TO  WRK-SRYCD
               MOVE    2                   TO  SPA-NETAKIRI-ARI
           END-IF
      *
      *    追加共通処理
           PERFORM 33041-SRYCD-ADD-SEC
      *    追加なし
           IF      FLG-ADD             =   ZERO
               GO      TO      3305-NETAKIRI-SET-EXT
           END-IF
      *
           .
       3305-NETAKIRI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理（前履歴）
      *****************************************************************
       33051-JYURRK-MAECHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUG-SANTEI
           MOVE    SPACE               TO  WRK-SANTEI-YM
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      33051-JYURRK-MAECHK-EXT
           END-IF
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
                   ADD     1                   TO  WRK-SANTEI-CNT
      *            当月算定済み
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SANTEI-SRYYM        TO  WRK-SANTEI-YM
                       MOVE    SANTEI-MSANTEICNT   TO  FLG-TOUG-SANTEI
                   END-IF
                   IF      SANTEI-SRYYM        <   SPA-SRYYMD(1:6)
                       MOVE    SANTEI-SRYYM        TO  WRK-SANTEI-YM
                       MOVE    1                   TO  FLG-SANTEI
                   END-IF
               END-IF
               IF     FLG-SANTEI           =   ZERO
                  MOVE    "key3"              TO  MCP-PATHNAME
                  PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       33051-JYURRK-MAECHK-EXT.
           EXIT.
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *
      *****************************************************************
      *    指導料の自動発生処理
      *****************************************************************
       33012-JIDOU-SYORI-SEC      SECTION.
      *
           IF      SPA-TOKUTEI         =   ZERO
               GO      TO      33012-JIDOU-SYORI-EXT
           END-IF
      *
      *    指導料の自動発生
           EVALUATE    SPA-NAI-ROUJIN  ALSO  SPA-HOSPSBT
      *        老人　病院
               WHEN    1        ALSO         1
                   IF      SPA-BEDSU           <   100
                       MOVE    "113700710"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "113700810"         TO  WRK-SRYCD
                   END-IF
      *        老人、診療所
               WHEN    1        ALSO         2
                   MOVE    "113700610"         TO  WRK-SRYCD
      *        一般　病院
               WHEN    ZERO     ALSO         1
                   IF      SPA-BEDSU           <   100
                       MOVE    "113001910"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "113002010"         TO  WRK-SRYCD
                   END-IF
      *        一般、診療所
               WHEN    ZERO     ALSO         2
                   MOVE    "113001810"         TO  WRK-SRYCD
           END-EVALUATE
      *
      *    点数マスタ編集・基本チェック
           COMPUTE SPA-GMN-IDX         =   SPA-GMN-IDX   +   1
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (LNK-SC92-ERRAREA    =   SPACE )
      *        指導料初診日チェック（ORCSC10 と同様のチェック）
               PERFORM 330121-JIDOU-CHK-SEC
           END-IF
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                       -   1
           END-IF
      *
           .
       33012-JIDOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料初診日チェック処理（ORCSC10 と同様のチェック）
      *****************************************************************
       330121-JIDOU-CHK-SEC               SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      SPA-SYOYMD          =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
           ELSE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-SYOYMD          TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KEIYMD
               IF      LNK-DAY6-KOYOMI NOT =   LNK-DAY6-KOYOMIZEN
                   MOVE    1                   TO  FLG-KYUJITU
               END-IF
           END-IF
      *
           EVALUATE    TRUE
               WHEN    WRK-KEIYMD          <   SPA-SRYYMD
                   CONTINUE
               WHEN    WRK-KEIYMD          >   SPA-SRYYMD
                   MOVE    "0034"          TO  SPA-ERRCD
               WHEN    WRK-KEIYMD          =   SPA-SRYYMD
                   IF      FLG-KYUJITU     =   ZERO
                       MOVE    "0034"          TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           .
       330121-JIDOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同時再診算定処理（確認後）
      *****************************************************************
       300-DOUSAISIN-SET-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
      *
      *    同日再診料へ置換え
           EVALUATE    SPA-NAI-ROUJIN  ALSO  SPA-HOSPSBT
      *        老人　病院
               WHEN    1        ALSO         1
                   MOVE    "112700550"         TO  WRK-SRYCD
      *        老人、診療所
               WHEN    1        ALSO         2
                   MOVE    "112700850"         TO  WRK-SRYCD
      *        老人外、病院  
               WHEN    ZERO     ALSO         1
                   MOVE    "112003250"         TO  WRK-SRYCD
      *        老人外、診療所
               WHEN    ZERO     ALSO         2
                   MOVE    "112003350"         TO  WRK-SRYCD
           END-EVALUATE
      *
      *    初診料自動追加
           PERFORM 3301-SYOSIN-SYORI-SEC
           .
       300-DOUSAISIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内院外変更処理
      *****************************************************************
       400-INGAI-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX-IN
           MOVE    ZERO                TO  WRK-ZAITAKU
           MOVE    ZERO                TO  WRK-ZAITAKU-IDX
           MOVE    ZERO                TO  WRK-GAIRAI-IDX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )  OR
                              (SPA-COM-INPUTCD (IDX)  =   SPACE AND
                               SPA-GMN-INPUTCD (IDX)  =   SPACE )
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *            小児科外来診察料
                   WHEN    "113003510" 
                   WHEN    "113003610" 
                   WHEN    "113003710" 
                   WHEN    "113003810" 
                       MOVE    IDX                 TO  WRK-IDX-IN
      *            寝たきり老人在宅総合診療科
                   WHEN    "114701210"
                   WHEN    "114701710"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *            老人慢性疾病外来総合診療科
                   WHEN    "113701010"
                   WHEN    "113701110"
                   WHEN    "113701510"
                       MOVE    IDX                 TO  WRK-IDX-IN
      *            在宅自己注射（院外）
                   WHEN    "114009210"
                       MOVE    IDX                 TO  WRK-ZAITAKU-IDX
                       MOVE    1                   TO  WRK-ZAITAKU
      *            在宅自己注射（院ない）
                   WHEN    "114003210"
                       MOVE    IDX                 TO  WRK-ZAITAKU-IDX
                       MOVE    0                   TO  WRK-ZAITAKU
               END-EVALUATE
           END-PERFORM
      *
           IF     (WRK-IDX-IN          =   ZERO ) AND
                  (WRK-ZAITAKU-IDX     =   ZERO )
               GO     TO      400-INGAI-HEN-EXT
           END-IF
      *
           IF      SPA-SYONIKA-ARI     NOT =   ZERO
      *        小児科外来診療科自動発生処理
               PERFORM 3303-SYONIKA-SET-SEC
           END-IF
      *    老人慢性疾患外来総合診療科自動発生処理
           IF      SPA-ROUMANSEI-ARI   NOT =   ZERO
               MOVE    1                   TO  WRK-ROUMANSEI-CNT
               PERFORM 3304-ROUMANSEI-SET-SEC
           END-IF
      *    寝たきり老人在宅総合診療科自動算定処理
           IF      SPA-NETAKIRI-ARI    NOT =   ZERO
      *        当月算定済み 自動作成なし
               MOVE    1                   TO  WRK-NETAKIRI-CNT
               MOVE    SPACE               TO  WRK-SANTEI-ARI
               MOVE    SPACE               TO  WRK-SANTEI-NASI
               PERFORM 3305-NETAKIRI-SET-SEC
           END-IF
      *
      *    在宅自己注射
           IF      WRK-ZAITAKU-IDX   NOT =   ZERO
               PERFORM 4001-ZAITAKU-CHU-SEC
           END-IF
           .
       400-INGAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    在宅自己注射処理
      *****************************************************************
       4001-ZAITAKU-CHU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF      SPA-INGAIKBN        =   "1"
      *        院外 
               IF      WRK-ZAITAKU           =   0
      *            在宅自己注射（院外）へ
                   MOVE    "114009210"         TO  WRK-SRYCD
               END-IF
           ELSE
               IF      WRK-ZAITAKU           =   1
      *            在宅自己注射（院内）へ
                   MOVE    "114003210"         TO  WRK-SRYCD
               END-IF
           END-IF
           IF      WRK-SRYCD           =   SPACE
               GO      TO      4001-ZAITAKU-CHU-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-ADD
      *
      *    追加共通処理
           MOVE    WRK-ZAITAKU-IDX     TO  WRK-IDX-IN
           PERFORM 33041-SRYCD-ADD-SEC
      *
           .
       4001-ZAITAKU-CHU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同時再診算定処理（入力分）
      *****************************************************************
       500-INDOUSAISIN-SET-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           EVALUATE    SPA-COM-INPUTCD (SPA-GMN-IDX)
      *        電話再診（病院）一般
               WHEN    "112002850"
                   MOVE    "112003050"         TO  WRK-SRYCD
      *        電話再診（診療所）一般
               WHEN    "112002950"
                   MOVE    "112003150"         TO  WRK-SRYCD
      *        電話再診（病院）老人
               WHEN    "112700450"
                   MOVE    "112700650"         TO  WRK-SRYCD
      *        電話再診（診療所）老人
               WHEN    "112700750"
                   MOVE    "112700950"         TO  WRK-SRYCD
           END-EVALUATE
      *    電話再診の時、同日電話再診へ振り替え
           IF      WRK-SRYCD           NOT =   SPACE
               MOVE    SPA-GMN-IDX         TO  WRK-IDX-IN
               PERFORM 33041-SRYCD-ADD-SEC
               GO      TO      500-TELDOUSAISIN-SET-EXT
           END-IF
      *
      *    電話再診以外の時、再度診察料算定へ
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
           PERFORM 100-SANTEI-CHK-SEC
           PERFORM 200-SYOSIN-SET-SEC
      *
           .
       500-TELDOUSAISIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤分離前の特定算定回数チェック処理
      *****************************************************************
       600-SANTEI-CNT-SEC              SECTION.
      *
      *    特定の診療行為の有無
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-ROUMANSEI-ARI
           MOVE    ZERO                TO  SPA-HOUMON-ARI
           MOVE    ZERO                TO  SPA-SYONIKA-ARI
           MOVE    ZERO                TO  SPA-TELSAI-ARI
           MOVE    ZERO                TO  SPA-FUKU-SRYKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE     )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
      *
      *        特定の診療行為判定
               EVALUATE    SPA-COM-INPUTCD (IDX)
      *            寝たきり老人在宅総合診療科（院外処方箋あり）
                   WHEN    "114701210"
                       MOVE    1               TO  SPA-NETAKIRI-ARI
      *            寝たきり老人在宅総合診療科（院外処方箋なし）
                   WHEN    "114701710"
                       MOVE    2               TO  SPA-NETAKIRI-ARI
      *
      *            老人慢性疾病外来総合診療科（院外処方あり）
                   WHEN    "113701010"
                       MOVE    1               TO  SPA-ROUMANSEI-ARI
      *            老人慢性疾病外来総合診療科（院外処方なし）
                   WHEN    "113701110"
                   WHEN    "113701510"
                       MOVE    2               TO  SPA-ROUMANSEI-ARI
      *
      *            寝たきり老人訪問診察料
                   WHEN    "114700110"
                   WHEN    "114701110"
                       MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            在宅患者訪問診察料
                   WHEN    "114001110"
                       MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            在宅末期医療総合診察料
                   WHEN    "114007610"
                   WHEN    "114007710"
                       MOVE    1               TO  SPA-HOUMON-ARI
      *
      *            小児科外来診療科算定
                   WHEN    "113003510"
                   WHEN    "113003610"
                   WHEN    "113003710"
                   WHEN    "113003810"
                       MOVE    1               TO  SPA-SYONIKA-ARI
      *
      *            電話再診算定
                   WHEN    "112002850"
                   WHEN    "112002950"
                   WHEN    "112700450"
                   WHEN    "112700750"
      *                同日電話再診
                   WHEN    "112003050"
                   WHEN    "112003150"
                   WHEN    "112700650"
                   WHEN    "112700950"
                       MOVE    1               TO  SPA-TELSAI-ARI
      *                同日他科受診
                   WHEN    "830000020"
                   WHEN    "830000021"
                       MOVE    1               TO  SPA-FUKU-SRYKA
               END-EVALUATE
           END-PERFORM
           IF      SPA-SYONIKA-ARI     =   ZERO
               MOVE    SPA-SYONIKA-ARI2    TO  SPA-SYONIKA-ARI
           END-IF
           .
       600-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *     剤分離後の特定算定チェック処理
      *****************************************************************
       700-SANTEI-CHK-SEC              SECTION.
      *
      *    小児科外来診療チェック
           PERFORM 5019-SYONIKA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      700-SANTEI-CHK-EXT
           END-IF
      *
      *    老人慢性疾病外来総合診療科チェック
           IF     (SPA-ROUMANSEI-ARI   NOT =   ZERO )  OR
                  (SPA-ROUMANSEI-ARI2  NOT =   ZERO )
               PERFORM 5020-ROUMANSEI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      700-SANTEI-CHK-EXT
               END-IF
           END-IF
      *
      *    寝たきり老人在宅総合診療科チェック
           IF     (SPA-NETAKIRI-ARI    NOT =   ZERO )  OR
                  (SPA-NETAKIRI-ARI2   NOT =   ZERO )
               PERFORM 5021-NETAKIRI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      700-SANTEI-CHK-EXT
               END-IF
           END-IF
           .
       700-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診療科チェック処理
      *****************************************************************
       5019-SYONIKA-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        自費は対象外
             IF      SPA-COM-SRYKBN(IDX)     =   "95" OR
                                                 "96"
                 CONTINUE
             ELSE
      *        小児科外来診療科の時、小児科外来診療科のみであること
               IF      SPA-SYONIKA-ARI     NOT =   ZERO
                   EVALUATE    SPA-COM-INPUTCD (IDX)(1:1)
                       WHEN    "1"
      *                    特定の診療行為であること
                           EVALUATE    SPA-COM-INPUTCD (IDX)
                               WHEN    "113003610"
                               WHEN    "113003510"
                               WHEN    "113003810"
                               WHEN    "113003710"
      *                            同日再診の時、診察料は算定できない
                                   IF      SPA-SYONIKA-ARI     =   2
                                       MOVE    "0046"  TO  SPA-ERRCD
                                       MOVE    "1"     TO  SPA-COM-CUR
                                                                 (IDX)
                                   END-IF
                               WHEN    "111011170"
                               WHEN    "111010370"
                               WHEN    "112005770"
                               WHEN    "113005270"
                               WHEN    "111011070"
                               WHEN    "111010470"
                               WHEN    "111010970"
                               WHEN    "112006070"
                               WHEN    "113005570"
                               WHEN    "113005370"
                               WHEN    "112005870"
                               WHEN    "111010770"
                               WHEN    "111010870"
                               WHEN    "111010570"
                               WHEN    "113005170"
                               WHEN    "113005070"
                               WHEN    "113005470"
                               WHEN    "112005970"
                               WHEN    "111010670"
                                   CONTINUE
                               WHEN    OTHER
      *                            往診料であること
                                   IF      SPA-COM-SRYKBN (IDX)
                                                           =  "14"
                                       CONTINUE
                                   ELSE
                                       MOVE    "0044"  TO  SPA-ERRCD
                                       MOVE    "1"     TO  SPA-COM-CUR
                                                                (IDX)
                                   END-IF
                           END-EVALUATE
                       WHEN    "6"
      *                    薬剤の時、院外処方であること
                           IF    ((SPA-INGAIKBN        =   "1"  ) AND
                                  (SPA-COM-SRYSYUKBN(IDX)
                                                       =   "210" OR
                                                           "212" OR
                                                           "220" OR
                                                           "222" OR
                                                           "230" OR
                                                           "232" OR
                                                           "290" OR
                                                           "292" OR
                                                           SPACE )) OR
                                 ( SPA-COM-SRYSYUKBN(IDX)
                                                       =   "212" OR
                                                           "222" OR
                                                           "232" )
                               CONTINUE
                           ELSE
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0044"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                             END-IF
                           END-IF
                       WHEN    OTHER
      *                    院外薬剤と往診料であること
                           IF    ((SPA-INGAIKBN          =   "1" ) AND
                                  (SPA-COM-SRYSYUKBN(IDX)
                                                         =   "210" OR
                                                             "212" OR
                                                             "220" OR
                                                             "222" OR
                                                             "230" OR
                                                             "232" OR
                                                             "290" OR
                                                             "292" ))
                              OR  ( SPA-COM-SRYSYUKBN(IDX)
                                                         =   "212" OR
                                                             "222" OR
                                                             "232" OR
                                                             "130" OR
                                                             "140" )
                               CONTINUE
                           ELSE
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0044"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                             END-IF
                           END-IF
                   END-EVALUATE
               ELSE
      *        小児科外来診療科以外の時、小児科外来診療科でないこと
                   EVALUATE    SPA-COM-INPUTCD (IDX)
                               WHEN    "113003610"
                               WHEN    "113003510"
                               WHEN    "113003810"
                               WHEN    "113003710"
                               WHEN    "111011170"
                               WHEN    "111010370"
                               WHEN    "112005770"
                               WHEN    "113005270"
                               WHEN    "111011070"
                               WHEN    "111010470"
                               WHEN    "111010970"
                               WHEN    "112006070"
                               WHEN    "113005570"
                               WHEN    "113005370"
                               WHEN    "112005870"
                               WHEN    "111010770"
                               WHEN    "111010870"
                               WHEN    "111010570"
                               WHEN    "113005170"
                               WHEN    "113005070"
                               WHEN    "113005470"
                               WHEN    "112005970"
                               WHEN    "111010670"
                                   MOVE    "0045"      TO  SPA-ERRCD
                                   MOVE    "1"         TO  SPA-COM-CUR
                                                                (IDX)
                   END-EVALUATE 
               END-IF
             END-IF
           END-PERFORM
           .
       5019-SYONIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人慢性疾病外来総合診療科チェックク処理
      *****************************************************************
       5020-ROUMANSEI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE    )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        生活指導、検査、注射、在宅料以外であること
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                   WHEN    "60"
                       MOVE    "0052"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232"  )
      *                    院外処方箋を交付しない場合の時、エラー
                           IF      SPA-ROUMANSEI-ARI   =   2
      *                      処方のみはＯＫ
                             IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0052"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                               (IDX)
                             END-IF
                           END-IF
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0052"         TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                               (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM
           .
       5020-ROUMANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    寝たきり老人在宅総合診療科チェック処理
      *****************************************************************
       5021-NETAKIRI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE      )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE )
      *        検査、投薬は算定できない
               EVALUATE    SPA-COM-SRYKBN(IDX)
                   WHEN    "60"
                       MOVE    "0054"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
      *                薬剤の時、院外処方であること
                       IF    ((SPA-INGAIKBN        =   "1"  ) AND
                              (SPA-COM-SRYSYUKBN(IDX)
                                                   =   "210" OR
                                                       "212" OR
                                                       "220" OR
                                                       "222" OR
                                                       "230" OR
                                                       "232" OR
                                                       "290" OR
                                                       "292" OR
                                                       SPACE  ))  OR
                             ( SPA-COM-SRYSYUKBN(IDX)
                                                   =   "212" OR
                                                       "222" OR
                                                       "232"  )
      *                    院外処方箋を交付しない場合の時、エラー
                           IF     (SPA-NETAKIRI-ARI   =   2 )  OR
                                  (SPA-NETAKIRI-ARI2  =   2 )
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0054"          TO  SPA-ERRCD
                               MOVE    "1"             TO  SPA-COM-CUR
                                                                (IDX)
                             END-IF
                           END-IF
                       ELSE
      *                    処方のみはＯＫ
                           IF      SPA-COM-SRYSYUKBN(IDX)(3:1)
                                                   NOT =   "3" 
                               MOVE    "0054"      TO  SPA-ERRCD
                               MOVE    "1"         TO  SPA-COM-CUR
                                                             (IDX)
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM
           .
       5021-NETAKIRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC              SECTION.
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
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
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
      ******************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
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
