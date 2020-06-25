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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCSYUNOUG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : ワーク診療行為の外来収納編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/04/10    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  18/01/17  第三者行為分計算優先対応
      *  05.00.00    NACL-多々納  18/11/20  初診料算定フラグ２対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-SYOSIN-OK       PIC 9(01).
           03  FLG-SYU40           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-Y                   PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDH                     PIC 9(04).
           03  IDH1                    PIC 9(04).
           03  IDH2                    PIC 9(04).
           03  IDX-KOUI                PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDX-TS                  PIC 9(04).
           03  IDX-S1                  PIC 9(04).
           03  IDS1                    PIC 9(04).
           03  IDS2                    PIC 9(04).
      *    福岡県対応用
           03  IDX-SYU                 PIC 9(04).
           03  IDX-SYU2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD               PIC X(09).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
      *    点数
           03  WRK-TENSU               PIC 9(08).
           03  WRK-HKNTEN              PIC 9(08).
      *
      *     一部負担金計算用
           03  WRK-SYUTEN1             PIC 9(08).
           03  WRK-TENSU-SYU           PIC 9(08).
           03  WRK-TEN                 PIC 9(08).
      *     一部負担金計算用
           03  WRK-SYUTEN1-KEI         PIC S9(08).
           03  WRK-LNK-S01-SYUTEN      PIC S9(08).
           03  WRK-SINDAN              PIC S9(07).
           03  WRK-SEIKYU              PIC S9(07).
           03  WRK-CHK-SRYSYUKBN       PIC X(03).
           03  WRK-S01-TENSU           PIC 9(08).
      *    粉砕判定用
           03  WRK-FUNSA-AREA.
               05  FLG-FUNSAI          PIC 9(01).
               05  WRK-FUNSAI-CNT      PIC 9(04).
      *
           03  WRK-ZAINUM              PIC 9(08).
      *
      *    小児科外来診察料加算判定
      *    03  WRK-SYONIKA-KBN         PIC X(01).
      *    自費用
      *
      *    集計収納請求内容
       01  WRK-SUM-AREA.
           03  SUM-SYU-SKY-NAIYOU.
               05  SUM-SYU-SKY-TBL             OCCURS   17.
      *    保険点数
                   07  SUM-SYU-HKNTEN          PIC  S9(07).
      *
      *    収納マスタワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //WRK-SYU-//.
      *    収納マスタワーク２
       01  LNK-SYUNOU2-AREA.
           02  LNK-SYUNOU2-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU2-//.
       01  LNK-SYUNOU2-MAX              PIC 9(04).
      *
      *
      *    ワーク診療行為マスタ−（パラメタ）
       01  LNK2-WKSRYACT-AREA.
           02  LNK2-WKSRYACT-REC         OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK2-WKSRY-//.
      *    附加情報を追加
           03  LNK2-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK2-WKSRYP-//.
      *
      *
       01  LNK2-WKSRYACT-MAX            PIC 9(04).
      *
       01  LNK-WKSRYACT-MAX            PIC 9(04).
       01  LNK-SYUNOU-MAX              PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *    附加情報
       01  WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
      * 01 TENSUPLUS-REC.
      *    COPY    "CPTENSUPLUS.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *     パラメタテーブル
      *01  PARA-REC.
      *    COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
      *01  SPATMP-REC.
      *    COPY    "CPSPA-TMP.INC".
      *
      *    COPY    "MCPDATA2.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *福岡県対応用
      *    給付対象外点数編集サブパラメタ
           COPY    "CPORCSSYU40.INC".
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定用共通パラメタ
      *    COPY    "CPORCSC00.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
      **** COPY    "ORCA-DBPATH".
      *
      *    COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    外来収納作成パラメタ領域
           COPY    "CPORCSCSYUNOUG.INC".
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    収納データ
       01  LNK-SYUNOU-AREA.
           02  LNK-SYUNOU-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU-//.
      *    診療行為データ
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSCSYUNOUGAREA
           LNK-ORCS01-REC
           SPA-AREA
           LNK-SYUNOU-AREA
           LNK-WKSRYACT-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *****INITIALIZE                  STS-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    初期処理
           PERFORM 100-INIT-SYORI-SEC
      *    主処理
           IF      ORCSCSYU-ERRCD      =   ZERO
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
      *??  MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SYORI-SEC          SECTION.
      *
           IF      ORCSCSYU-WKSRYACT-MAX   =   ZERO
               MOVE    ZERO                TO  LNK-WKSRYACT-MAX
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   600 )
                              OR  (LNK-WKSRY-SRYYMD (IDX)  =   SPACE )
                   MOVE    IDX                 TO  LNK-WKSRYACT-MAX
               END-PERFORM
           ELSE
               MOVE    ORCSCSYU-WKSRYACT-MAX   TO  LNK-WKSRYACT-MAX
           END-IF
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )
                          OR  (LNK-SYU-SRYYMD (IDX)  =   SPACE )
               MOVE    IDX                 TO  LNK-SYUNOU-MAX
           END-PERFORM
           IF     (LNK-SYUNOU-MAX          =   ZERO )
               MOVE    01                  TO  ORCSCSYU-ERRCD
               GO      TO      100-INIT-SYORI-EXT
           END-IF
      *
      *    保険組合せ未設定
           IF      ORCSCSYU-HKNCOMBI(1)    =   SPACE
               IF      LNK-SYUNOU-MAX      =   1
      *            収納の１件目
                   MOVE    LNK-SYU-HKNCOMBINUM(1)
                                           TO  ORCSCSYU-HKNCOMBI(1)
                   MOVE    LNK-SYU-SRYKA  (1)
                                           TO  ORCSCSYU-SRYKA (1 1)
               ELSE
                   MOVE    01                  TO  ORCSCSYU-ERRCD
                   GO      TO      100-INIT-SYORI-EXT
               END-IF
           END-IF
      *
           IF      ORCSCSYU-SYORIKBN   =   "1"
      *        負担金初期編集
               PERFORM 1001-ORCS01-INIT-SEC
           END-IF
      *
      *    自賠責の時、システム管理検索
           IF      (SPA-HKNKBN         =   1   )  AND
                   (SPA-RSI-HKNKBN     =   "4"
                                       OR  "5"
      *             第三者行為追加
                                       OR  "6"  ) 
               PERFORM 4500-RSI-SYSKANRI-SEC
           END-IF
      *
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    負担金初期編集処理
      *****************************************************************
       1001-ORCS01-INIT-SEC             SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  IDX-Y
           MOVE    ZERO                TO  IDY
           ADD     1                   TO  IDX-Y
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           MOVE    ZERO                TO  WRK-FUNSAI-CNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *        一部負担額計算用クリア（現在未使用）
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  FLG-FUNSAI
                   MOVE    ZERO            TO  WRK-FUNSAI-CNT
               END-IF
      *
               IF     (LNK-WKSRY-HKNCOMBI (IDX)    =   "9999")
                   OR (LNK-WKSRY-CONTKBN  (IDX)    =   "H"   )
      *           薬評以外
                   OR (LNK-WKSRY-ZAIKBN   (IDX)    =   1    )
                   CONTINUE
               ELSE
      *            診療内容編集
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )
      *                一部負担額計算用（現在未使用）
                       PERFORM 1002-ORCS01-SET-SEC
      *                特定公費（診察料未算定扱い）判定
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409905"
                           MOVE    "1"         TO  LNK-S01-SINSATUFLG2
                       END-IF
      *                特定公費（時間外受診）判定
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409907"
                           MOVE    "1"         TO  LNK-S01-JIKANGAI
                       END-IF
      *H30.1
      *                第三者行為分計算優先フラグ
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099999931"
                           MOVE    "1"     TO  LNK-S01-DAISANYUSENFLG
                       END-IF
      *H30.11
      *                初診料算定フラグ２（地方公費用）
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409906"
                           MOVE    "1"     TO  LNK-S01-SYOSINFLG2
                       END-IF
      *
      *                初診料判定
                       PERFORM 1001-SYOSIN-CHK-SEC
                   END-PERFORM
               END-IF
      *        診察料算定フラグ（地方公費用）
               IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "12"
                                               OR  "13"
                                               OR  "14"
                   MOVE    "1"                 TO  LNK-S01-SINSATUFLG
               END-IF
           END-PERFORM
      *
      *一部負担額計算用（現在未使用）
      *    剤ごとに点数を入れ直す
           PERFORM VARYING     IDY     FROM    40 BY  -1
                   UNTIL       IDY     <   1
               IF      LNK-S01-SRYKBN (IDX-Y IDY)  NOT =   SPACE
                   IF     (IDY                     =   40    )  OR
                          (LNK-S01-ZAIBAN (IDX-Y IDY)  NOT =
                                       LNK-S01-ZAIBAN (IDX-Y IDY + 1))
                       MOVE    LNK-S01-TENSUU (IDX-Y IDY)
                                               TO  WRK-S01-TENSU
                   END-IF
                   MOVE    WRK-S01-TENSU       TO  LNK-S01-TENSUU
                                                       (IDX-Y IDY)
               END-IF
           END-PERFORM
      *
      *    訂正処理
           IF      SPA-SYORIFLG        =   1
               MOVE    "1"             TO  LNK-S01-SAIKEISAN-KBN
           END-IF
      *
      *H27.4　以降から使用する
      *    在宅総合・在宅末期　算定患者区分
           IF     ( SPA-SRYYMD         <   "20021001")  OR
                  ((SPA-NETAKIRI-ARI   =   ZERO )  AND
                   (SPA-NETAKIRI-ARI2  =   ZERO )  AND
                   (SPA-ZAIMATU-ARI    =   ZERO )  AND
                   (SPA-ZAIMATU-ARI2   =   ZERO )  AND
      *            Ｈ１８．４追加
                   (SPA-ZAIKANRI-ARI   =   ZERO )  AND
                   (SPA-ZAIKANRI-ARI2  =   ZERO )
      *            Ｈ２０．４追加（特定施設入居時医学総合管理料）
              AND  (SPA-IGAKUKAN-ARI   =   ZERO )
              AND  (SPA-IGAKUKAN-ARI2  =   ZERO )  )
               CONTINUE
           ELSE
               MOVE    "1"             TO  LNK-S01-KOUREI-ZAITAKU-KBN
           END-IF
      *
      *    初診料算定フラグ（地方公費用）
           IF      FLG-SYOSIN-OK       =   1
               MOVE    "1"             TO  LNK-S01-SYOSINFLG
           END-IF
      *
           .
       1001-ORCS01-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額計算用セット処理　（未使用項目）
      *****************************************************************
       1002-ORCS01-SET-SEC            SECTION.
      *
      *    粉砕の時最初の１剤のみとする
           IF      LNK-WKSRY-SRYCD(IDX IDS)    =   "099209901"
               ADD     1                   TO  WRK-FUNSAI-CNT
               MOVE    1                   TO  FLG-FUNSAI
               GO      TO      1002-ORCS01-SET-EXT
           END-IF
           IF     (FLG-FUNSAI          =   1   )  AND
                  (WRK-FUNSAI-CNT      >   1   )
               MOVE    ZERO                TO  FLG-FUNSAI
               GO      TO      1002-ORCS01-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "6"  ) OR
                  (LNK-WKSRY-SRYCD(IDX IDS)(1:3) =   "001")
      *        院内薬剤のみとする
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX) TO
                                                   WRK-CHK-SRYSYUKBN
               IF      WRK-CHK-SRYSYUKBN       =   SPACE
                   EVALUATE    LNK-WKSRY-SRYKBN (IDX)
                       WHEN    "21"
                           MOVE    "210"     TO  WRK-CHK-SRYSYUKBN
                       WHEN    "22"
                           MOVE    "220"     TO  WRK-CHK-SRYSYUKBN
                       WHEN    "23"
                           MOVE    "230"     TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               END-IF
               IF     (WRK-CHK-SRYSYUKBN           =   "211"  OR
                                                       "221"  OR
                                                       "231"  OR
                                                       "291"  OR
                                                       "294" )  OR
                     ((SPA-INGAIKBN            NOT =   "1"   ) AND
                      (WRK-CHK-SRYSYUKBN           =   "210"  OR
                                                       "220"  OR
                                                       "230"  OR
                                                       "290"  OR
                                                       "293" ))
                   ADD     1                   TO  IDY
                   IF      IDY                 >   40
                       GO      TO      1002-ORCS01-SET-EXT
                   END-IF
                   MOVE    LNK-WKSRY-SRYKBN(IDX)
                                                   TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAINUM (IDX)
                                                   TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)
                                                   TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDS)
                                                   TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                                   TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                                   TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
               END-IF
           END-IF
      *
           .
       1002-ORCS01-SET-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定判定処理
      *****************************************************************
       1001-SYOSIN-CHK-SEC             SECTION.
      *
           IF      LNK-WKSRY-SRYKBN(IDX)   =   "11"  OR  "13"
               EVALUATE    LNK-WKSRY-SRYCD  (IDX IDS)
      *            初診料
                   WHEN    "111000110"
                   WHEN    "111003610"
                   WHEN    "111700110"
                   WHEN    "111700210"
                   WHEN    "113003510"
                   WHEN    "113003710"
                   WHEN    "101110010"
      *            ダミー追加
                   WHEN    "099110001"
      *            紹介状なし追加
                   WHEN    "111012510"
      *H27.1       妥結率５割以下
                   WHEN    "111012710"
      *H28.4
                   WHEN    "113019710"
                   WHEN    "113019910"
      *H29.2       同一日２科目
                   WHEN    "111011810"
                   WHEN    "111012610"
                   WHEN    "111012810"
      *R02.4
      *           初診料（新型コロナウイルス感染症・臨時）
                  WHEN    "111013850"
                       MOVE    1             TO  FLG-SYOSIN-OK
               END-EVALUATE
           END-IF
           .
       1001-SYOSIN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SYORI-SEC          SECTION.
      *
      *
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-MAX
      *    福岡公費用
           MOVE    ZERO                TO  FLG-SYU40
      *
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (ORCSCSYU-HKNCOMBI(IDH1)   =   SPACE)
               MOVE    ORCSCSYU-HKNCOMBI(IDH1)    TO  WRK-HKNCOMBI
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (ORCSCSYU-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    ORCSCSYU-SRYKA (IDH1 IDH2)  TO  WRK-SRYKA
                   PERFORM VARYING     IDH     FROM    1   BY  1
                           UNTIL      (IDH     >   15  )  OR
                                      (IDH     >   LNK-SYUNOU-MAX)
                       IF      (LNK-SYU-HKNCOMBINUM (IDH)
                                               =   WRK-HKNCOMBI ) AND
                               (LNK-SYU-SRYKA (IDH)
                                               =   WRK-SRYKA    )
      *                    保険組合せ毎に収納編集
                           PERFORM 2001-SYUNOU-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
               IF     (ORCSCSYU-SRYKA (IDH1 2) NOT =   SPACE)
      *            複数科合計追加（K08)
                   MOVE    ZERO            TO  WRK-SRYKA
                   PERFORM 2002-SYUNOU-ADD-SEC
               END-IF
           END-PERFORM
      *
           IF      ORCSCSYU-ERRCD  NOT =   ZERO
               MOVE    LNK-SYUNOU2-AREA    TO  LNK-SYUNOU-AREA
               MOVE    LNK-SYUNOU2-MAX     TO  ORCSCSYU-SYUNOU-MAX
               GO      TO      200-MAIN-SYORI-EXT
           END-IF
      *
      *    複数科まとめ集計フラグ
           MOVE    SPA-KAGRPFLG        TO  LNK-S01-KAGRPFLG
      *
      *    負担金計算処理
           CALL    "ORCSGAIFTN"        USING
                                       MCPAREA
                                       LNK-ORCS01-REC
                                       LNK-SYUNOU2-AREA
                                       SPA-AREA
      *    計算金額桁数チェックフラグ
           IF      LNK-S01-SKYCHKFLG   =   "1"
               MOVE    11              TO  ORCSCSYU-ERRCD
           END-IF
           IF      LNK-S01-SKYCHKFLG   =   "2"
               MOVE    12              TO  ORCSCSYU-ERRCD
           END-IF
      *
           MOVE    LNK-SYUNOU2-AREA    TO  LNK-SYUNOU-AREA
           MOVE    LNK-SYUNOU2-MAX     TO  ORCSCSYU-SYUNOU-MAX
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       2001-SYUNOU-HENSYU-SEC             SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDH)    TO  SYUNOU-REC
           INITIALIZE                  WRK-SUM-AREA
           IF      LNK-SYUNOU2-MAX     >=  15
               MOVE    9                   TO  ORCSCSYU-ERRCD
               GO      TO      2001-SYUNOU-HENSYU-EXT
           END-IF
           ADD     1                   TO  LNK-SYUNOU2-MAX
      *
      *    各点数
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    SPACE               TO  LNK2-WKSRYACT-AREA
           INITIALIZE                      LNK2-WKSRYACT-AREA
      *
           MOVE    ZERO                TO  WRK-LNK-S01-SYUTEN
           MOVE    ZERO                TO  IDX-S1
           MOVE    ZERO                TO  SYU-SHOHOU-SAI
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >  LNK-WKSRYACT-MAX
               IF     (LNK-WKSRY-HKNCOMBI(IDY) =   WRK-HKNCOMBI)
                  AND (LNK-WKSRY-SRYKA   (IDY) =   WRK-SRYKA   )
      *            包括・薬評以外
                   IF     (LNK-WKSRY-HKNCOMBI (IDY)    =   "9999")
                       OR (LNK-WKSRY-CONTKBN  (IDY)    =   "H"   )
                       OR (LNK-WKSRY-ZAIKBN   (IDY)    =   1     )
                       CONTINUE
                   ELSE
                       IF     (IDX-KOUI        >   ZERO  )  AND
                              (LNK-WKSRY-ZAINUM (IDY) NOT =
                                           LNK2-WKSRY-ZAINUM (IDX-KOUI))
      *                    剤毎に処理を行う
                           PERFORM 20011-ZAINUM-SYORI-SEC
                           MOVE    ZERO            TO  IDX-KOUI
                           MOVE    SPACE       TO  LNK2-WKSRYACT-AREA
                           INITIALIZE              LNK2-WKSRYACT-AREA
                       END-IF
                       ADD     1               TO  IDX-KOUI
                       MOVE    LNK-WKSRYACT-REC (IDY)
                                               TO  LNK2-WKSRYACT-REC
                                                            (IDX-KOUI)
                   END-IF
               END-IF
           END-PERFORM
           IF      IDX-KOUI            >   ZERO
               PERFORM 20011-ZAINUM-SYORI-SEC
           END-IF
      *    収納保険点数編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   17
               MOVE    ZERO                TO  SYU-HKNTEN(IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   16
      *        保険点数
               IF      SUM-SYU-HKNTEN(IDX)     <   ZERO
                   MOVE    ZERO                TO  SUM-SYU-HKNTEN (IDX)
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
               MOVE    SUM-SYU-HKNTEN(IDX)     TO  SYU-HKNTEN(IDX)
      *        保険合計点数
               ADD     SYU-HKNTEN(IDX)         TO  SYU-HKNTEN (17)
           END-PERFORM
      *
      *    自賠責の手技点数
           MOVE    WRK-LNK-S01-SYUTEN  TO  LNK-S01-SYUTEN
                                                   (LNK-SYUNOU2-MAX)
      *
      *    薬剤一部負担金計算処理
           PERFORM 20012-YAKFTN-KEI-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
      *
           .
       2001-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険点数再集計処理
      *****************************************************************
       20011-ZAINUM-SYORI-SEC             SECTION.
      *
      *    保険点数再集計処理
           PERFORM 20011-HKNTEN-KEI-SEC
      *
           MOVE    ZERO                TO  WRK-SYUTEN1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *        自賠責の時、技術点を集計する
               IF      (SPA-HKNKBN         =   1   )  AND
                       (SPA-RSI-HKNKBN     =   "4"
                                           OR  "5"
      *                第三者行為追加
                                           OR  "6"  ) 
                   PERFORM 200112-JIBAI-SYORI-SEC
               END-IF
      *        公害保険の時、技術点を集計する
               IF      (SPA-HKNKBN         =   2   )  AND
                       (LNK2-WKSRY-SYUTEN1 (IDX)    NOT =  ZERO )
                   PERFORM 200113-KOGAI-SYORI-SEC
               END-IF
           END-PERFORM
           .
       20011-ZAINUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険点数再集計処理
      *****************************************************************
       20011-HKNTEN-KEI-SEC             SECTION.
      *
           MOVE    LNK2-WKSRY-SRYKBN   (1) TO  WRK-SRYKBN
           MOVE    LNK2-WKSRY-SRYSYUKBN(1) TO  WRK-SRYSYUKBN
      *    集計位置決定
           PERFORM 200111-SYUNOU-TENSU-SEC
      *
           IF      IDX-TS                  =   ZERO
               GO      TO      20011-HKNTEN-KEI-EXT
           END-IF
           IF     (LNK2-WKSRYP-RSIKINGAKU(1)  >    ZERO )
      *        労災金額ありは、労災点数を集計（１件目）
               MOVE    LNK2-WKSRYP-RSI-TENSU (1)   TO  WRK-TENSU
           ELSE
      *        剤点数は最後の行
               MOVE    LNK2-WKSRY-ZAITENKEI(IDX-KOUI)  TO  WRK-TENSU
           END-IF
      *
           COMPUTE WRK-HKNTEN          =   WRK-TENSU
                                       *   LNK2-WKSRY-ZAIKAIKEI
                                                          (IDX-KOUI)
      *
           IF      LNK2-WKSRYP-PLUSKBN (IDX-KOUI)   =   ZERO
      *        集計
               ADD     WRK-HKNTEN          TO  SUM-SYU-HKNTEN (IDX-TS)
           ELSE
      *        減点
               COMPUTE SUM-SYU-HKNTEN (IDX-TS)
                                           =   SUM-SYU-HKNTEN (IDX-TS)
                                           -   WRK-HKNTEN
           END-IF
      *    処方せん料
           IF      WRK-SRYSYUKBN       =   "820"
      *        処方せん料再掲
               ADD     WRK-HKNTEN          TO  SYU-SHOHOU-SAI
           END-IF
      *
           .
       20011-HKNTEN-KEI-EXT.
           EXIT.
      *****************************************************************
      *    自賠責の時、技術点を集計処理
      *****************************************************************
       200112-JIBAI-SYORI-SEC          SECTION.
      *
      *    診察料と診断料等の時、診療コードを判定する
           IF      LNK2-WKSRY-SRYKBN (IDX) =   "80"
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK2-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
      *
                   MOVE    LNK2-WKSRY-SRYCD (IDX IDS)  TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            自賠責金額入力
                   IF      LNK2-WKSRY-SRYSYUKBN(IDX)    =   "809"
                       MOVE    LNK2-WKSRY-JIHIMONEY(IDX IDS)
                                                   TO  TNS-TEN
                   END-IF
      *            自賠責金額集計
      *            自賠責診断書料集計
                   IF      LNK2-WKSRY-SRYCD (IDX IDS)(1:5)
                                                   =   "09591"
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK2-WKSRY-ZAIKAIKEI
                                                            (IDX-KOUI)
                       ADD     WRK-SINDAN      TO  LNK-S01-SINDAN
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *            自賠責明細書料集計
                   IF      LNK2-WKSRY-SRYCD (IDX IDS)(1:5)
                                                   =   "09592"
                       COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK2-WKSRY-ZAIKAIKEI
                                                            (IDX-KOUI)
                       ADD     WRK-SINDAN      TO  LNK-S01-MEISAI
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *            自賠責その他のコード編集
                   IF      LNK2-WKSRY-SRYCD (IDX IDS)(1:5)
                                                   =   "09591"
                                                   OR  "09592"
                                                   OR  "09593"
                       ADD     1               TO  IDX-S1
                       IF      IDX-S1          <=  20
                           MOVE    LNK2-WKSRY-SRYCD (IDX IDS)
                                           TO   LNK-S01-SONOTA-SRYCD
                                              (LNK-SYUNOU2-MAX IDX-S1)
                           MOVE    LNK2-WKSRY-ZAIKAIKEI (IDX)
                                           TO   LNK-S01-SONOTA-ZAIKAISU
                                                (LNK-SYUNOU2-MAX IDX-S1)
                           MOVE    TNS-TEN
                                           TO   LNK-S01-SONOTA-TANKA
                                                (LNK-SYUNOU2-MAX IDX-S1)
                       END-IF
                   END-IF
      *            療養担当手当の点数をゼロとする
                   IF     (LNK2-WKSRY-SRYKBN(IDX)      =   "80")  AND
                          (LNK2-WKSRY-SRYCD (IDX IDS)  =   "199000610")
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK2-WKSRY-ZAIKAIKEI
                                                            (IDX-KOUI)
                       ADD     WRK-SINDAN      TO  LNK-S01-RYOYO
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *H25.7
      *            救急医療管理加算点数
                   IF     (SPA-SRYYMD                  >=  "20130701")
                     AND  (LNK2-WKSRY-SRYCD (IDX IDS)  =   "101800890")
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK2-WKSRY-ZAIKAIKEI
                                                            (IDX-KOUI)
                       MOVE    WRK-SINDAN      TO  LNK-S01-KYUKYU
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *
               END-PERFORM
           END-IF
      *    腰部固定帯加算を器材点数として対象外とする
      *    自賠責固定帯加算等取扱を判定する
           IF     (SYS-4001-KOTEIKBN    =   "2"  OR   SPACE )  AND
                  (LNK2-WKSRY-SRYKBN(IDX)(1:1)   NOT =   "9"  )
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK2-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK2-WKSRY-SRYCD(IDX IDS)    =   "140037490"
                                                       OR  "140040110"
                                                       OR  "150266970"
      *                    頸部固定帯加算
                                                       OR  "140052610"
                       MOVE    LNK2-WKSRY-SRYCD (IDX IDS)
                                               TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
               END-PERFORM
           END-IF
      *
      *    剤の最終行
           IF      IDX             NOT =   IDX-KOUI
               GO      TO      200112-JIBAI-SYORI-EXT
           END-IF
      *H26.10
      *    薬剤料逓減は対象外
           IF     (LNK2-WKSRY-SRYCD (IDX 01)(1:1)  =   "6" )
             AND  (LNK2-WKSRYP-PLUSKBN (01)     =   1   )
               GO      TO      200112-JIBAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK2-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK2-WKSRY-SYUTEN2 (IDX)    TO  WRK-SYUTEN1
           END-IF
      *
           IF     (LNK2-WKSRYP-RSIKINGAKU(1)  >    ZERO )
      *        労災金額ありは、労災点数（１件目）を手技点数
               MOVE    LNK2-WKSRYP-RSI-TENSU (1)   TO  WRK-TENSU-SYU
           ELSE
      *        剤の手技点数は最後の行
               MOVE    LNK2-WKSRY-SYUTEN1(IDX)     TO  WRK-TENSU-SYU
           END-IF
      *
      *    自賠責手技点合計
           IF      WRK-TENSU-SYU           <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE WRK-SYUTEN1-KEI     =  (WRK-TENSU-SYU
                                           -   WRK-SYUTEN1  )
                                           *   LNK2-WKSRY-ZAIKAIKEI
                                                             (IDX)
               IF      LNK2-WKSRY-ZAIKBN (IDX) =   2
      *            減点分
                   COMPUTE WRK-SYUTEN1-KEI     =   WRK-SYUTEN1-KEI
                                               *   -1
               END-IF
               COMPUTE WRK-LNK-S01-SYUTEN  =   WRK-LNK-S01-SYUTEN
                                           +   WRK-SYUTEN1-KEI
           END-IF
      *
           .
       200112-JIBAI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    公害保険の時、技術点を集計処理
      *****************************************************************
       200113-KOGAI-SYORI-SEC          SECTION.
      *
      *    公害固有のコードを対象外とする
           IF     (LNK2-WKSRY-SRYKBN(IDX)(1:1)  NOT =   "9"  )
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK2-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK2-WKSRY-SRYCD(IDX IDS)(1:3)
                                                   =   "102"
                       MOVE    LNK2-WKSRY-SRYCD (IDX IDS)  TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
      *H28.3
      *            療養担当手当の点数をゼロとする
                   IF     (LNK2-WKSRY-SRYKBN(IDX)      =   "80")  AND
                          (LNK2-WKSRY-SRYCD (IDX IDS)  =   "199000610")
                       MOVE    LNK2-WKSRY-SRYCD (IDX IDS)  TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK2-WKSRY-ZAIKAIKEI
                                                            (IDX-KOUI)
                       ADD     WRK-SINDAN      TO  LNK-S01-RYOYO
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    剤の最終行
           IF      IDX             NOT =   IDX-KOUI
               GO      TO      200113-KOGAI-SYORI-EXT
           END-IF
      *
      *    薬剤料逓減は対象外
           IF     (LNK2-WKSRY-SRYCD (IDX 01)(1:1)   =   "6" )
             AND  (LNK2-WKSRYP-PLUSKBN (01)     =   1  )
               GO      TO      200113-KOGAI-SYORI-EXT
           END-IF
      *
           IF      LNK2-WKSRY-SYUTEN1 (IDX)    =   ZERO
               GO      TO      200113-KOGAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK2-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK2-WKSRY-SYUTEN2 (IDX)     TO  WRK-SYUTEN1
           END-IF
      *
      *    手技点合計
           IF      LNK2-WKSRY-SYUTEN1 (IDX) <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE WRK-SYUTEN1-KEI     =  (LNK2-WKSRY-SYUTEN1 (IDX)
                                           -   WRK-SYUTEN1  )
                                           *   LNK2-WKSRY-ZAIKAIKEI
                                                             (IDX)
               IF      LNK2-WKSRY-ZAIKBN (IDX) =   2
      *            減点分
                   COMPUTE WRK-SYUTEN1-KEI     =   WRK-SYUTEN1-KEI
                                               *   -1
               END-IF
               COMPUTE WRK-LNK-S01-SYUTEN  =   WRK-LNK-S01-SYUTEN
                                           +   WRK-SYUTEN1-KEI
           END-IF
      *
           .
       200113-KOGAI-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       20012-YAKFTN-KEI-SEC             SECTION.
      *
      *    船員保険で初診料算定時
           IF     (SYU-SYUHKNNUM       =   "002" )  AND
                  (SYU-HKNTEN (01)     >   ZERO  )  AND
                  (LNK-S01-SYOSINFLG   =   "1"   )
               PERFORM 200121-SENINFLG-SET-SEC
           END-IF
      *
      *福岡県公費対応
      *    福岡県で、一般保険算定時
           IF     (SPA-PREFNUM         =   40   )  AND
                  (SPA-HKNKBN          =   ZERO )
               PERFORM 200122-PREFNUM-40-SEC
           END-IF
      *
           .
       20012-YAKFTN-KEI-EXT.
           EXIT.
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       2002-SYUNOU-ADD-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-HKNCOMBINUM(IDS1)
                                           =   WRK-HKNCOMBI
                   IF      SYU-PTID        =   ZERO
                       MOVE    LNK-SYUNOU2-REC (IDS1)
                                           TO  SYUNOU-REC
                   ELSE
                       MOVE    LNK-SYUNOU2-REC (IDS1)
                                               TO  WRK-SYUNOU-REC
                       PERFORM 450141-SYUNOU-ADD-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SYU-SRYKA
           IF      LNK-SYUNOU2-MAX     >=  15
               MOVE    9                   TO  ORCSCSYU-ERRCD
               GO      TO      2002-SYUNOU-ADD-EXT
           END-IF
           ADD     1                   TO  LNK-SYUNOU2-MAX
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
      *
           .
       2002-SYUNOU-ADD-EXT.
           EXIT.
      *****************************************************************
      *    収納点数集計位置処理
      *****************************************************************
       200111-SYUNOU-TENSU-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-TS
           EVALUATE    WRK-SRYKBN
      *        診察
               WHEN    "11"
               WHEN    "12"
                   MOVE    1                TO  IDX-TS
      *        指導
               WHEN    "13"
                   MOVE    2                TO  IDX-TS
      *        在宅
               WHEN    "14"
                   MOVE    3                TO  IDX-TS
      *        投薬
               WHEN    "21"   THRU  "27"
                   MOVE    4                TO  IDX-TS
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    5                TO  IDX-TS
      *        処置
               WHEN    "40"
                   MOVE    6                TO  IDX-TS
      *        手術
               WHEN    "50"
                   MOVE    7                TO  IDX-TS
      *        麻酔
               WHEN    "54"
                   MOVE    8                TO  IDX-TS
      *        検査
               WHEN    "60"
                   MOVE    9                TO  IDX-TS
      *        病理診断
               WHEN    "64"
                   MOVE    14               TO  IDX-TS
      *        画像診断
               WHEN    "70"
                   MOVE    10               TO  IDX-TS
               WHEN    "80"
                   EVALUATE    WRK-SRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       MOVE    11               TO  IDX-TS
      *            自賠責金額
                   WHEN    "809"
                       MOVE    11               TO  IDX-TS
      *            処方せん料
                   WHEN    "820"
                       MOVE    4                TO  IDX-TS
      *            精神科
                   WHEN    "830"
                       MOVE    12               TO  IDX-TS
      *            放射線治療
                   WHEN    "840"
                       MOVE    13               TO  IDX-TS
      *            療養担当手当て
                   WHEN    "850"
                       MOVE    16               TO  IDX-TS
      *            入院料（外来）
                   WHEN    "890"
                       MOVE    15               TO  IDX-TS
      *            その他
                   WHEN    OTHER
                       MOVE    11               TO  IDX-TS
                   END-EVALUATE
      *            食事
               WHEN    "90"
               WHEN    "92"
                   MOVE    15               TO  IDX-TS
           END-EVALUATE
           .
       200111-SYUNOU-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    船員保険で初診料算定時セット処理
      *****************************************************************
       200121-SENINFLG-SET-SEC            SECTION.
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    SPA-PTID           TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM    TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *        本人で通勤災害
               IF     (COMB-HONKZKKBN      =   "1"  )  AND
                      (COMB-HOJOKBN        =   "3"  OR
                                               "C"  OR
                                               "F"  OR
                                               "I"   )
                   MOVE    "1"             TO  LNK-S01-SENINFLG
                                                      (LNK-SYUNOU2-MAX)
               END-IF
           END-IF
           .
       200121-SENINFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    福岡県公費対応処理
      *****************************************************************
       200122-PREFNUM-40-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           INITIALIZE                  ORCSSYU40AREA
           MOVE    "1"                 TO  ORCS40-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-WKSRYACT-MAX )  OR
                              (IDX-SYU >=  5                )
      *
           IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
                  (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
              OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
                  (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
               CONTINUE
           ELSE
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  IDX-SYU2
               END-IF
      *
      *        初診・往診、小児科外来診察料の初診時
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "14" )  OR
                     ((SPA-SYONIKA-ARI         =   1    )  AND
                      (SPA-SYOSIN              =   1    )  AND
                      (LNK-WKSRY-SRYKBN (IDX)  =   "13"
                                               OR  "99"  ))
      *            連番が１の時、テーブル編集
                   IF      LNK-WKSRY-RENNUM (IDX)  =   1
                       ADD     1                   TO  IDX-SYU
                       MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  ORCS40-SRYKBN
                                                           (IDX-SYU)
                   END-IF
                   MOVE    LNK-WKSRY-ZAITENKEI (IDX)   TO  ORCS40-ZAITEN
                                                           (IDX-SYU)
      *            診療内容編集
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )  OR
                                      (IDX-SYU2    >=  5     )
                       IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "1") OR
                              (LNK-WKSRY-SRYCD(IDX IDS) =   "099114001")
                            ADD     1              TO  IDX-SYU2
                            MOVE    LNK-WKSRY-SRYCD(IDX IDS)
                                                   TO  ORCS40-SRYCD
                                                     (IDX-SYU IDX-SYU2)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
               END-IF
           END-PERFORM
      *
      *    対象があった場合
           IF      IDX-SYU             >   ZERO
      *        給付対象外点数編集
               CALL    "ORCSSYU40"     USING
                                       SPA-AREA
                                       ORCSSYU40AREA
                                       SYUNOU-REC
      *        初診料と同日初診がある時、初診料のみ
               IF      FLG-SYU40           =   1
                   COMPUTE SYU-KYUFUGAI-GOKEI-TEN
                                           =   SYU-KYUFUGAI-GOKEI-TEN
                                           -   SYU-KYUFUGAI-SHOSHIN-TEN
                   MOVE    ZERO            TO  SYU-KYUFUGAI-SHOSHIN-TEN
               END-IF
               IF      SYU-KYUFUGAI-SHOSHIN-TEN    >   ZERO
                   MOVE    1               TO  FLG-SYU40
               END-IF
           ELSE
      *        診療区分別給付外点数クリア
               INITIALIZE              SYU-KYUFUGAI
           END-IF
      *
           .
       200122-PREFNUM-40-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       450141-SYUNOU-ADD-HEN-SEC             SECTION.
      *
           ADD     WRK-SYU-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           ADD     WRK-SYU-TAX-MONEY   TO  SYU-TAX-MONEY
           ADD     WRK-SYU-SKYGK       TO  SYU-SKYGK
           ADD     WRK-SYU-HKNTEKMONEY TO  SYU-HKNTEKMONEY
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   17
               ADD    WRK-SYU-HKNTEN (IDS2)    TO  SYU-HKNTEN(IDS2)
               ADD    WRK-SYU-MONEY (IDS2)     TO  SYU-MONEY (IDS2)
               ADD    WRK-SYU-TGMONEY (IDS2)   TO  SYU-TGMONEY(IDS2)
               ADD    WRK-SYU-TGMONEY-TAX (IDS2)
                                               TO  SYU-TGMONEY-TAX(IDS2)
           END-PERFORM
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   10
               ADD    WRK-SYU-JIHI-TAXNASI (IDS2)
                                           TO  SYU-JIHI-TAXNASI(IDS2)
               ADD    WRK-SYU-JIHI-TAXARI (IDS2)
                                           TO  SYU-JIHI-TAXARI(IDS2)
           END-PERFORM
      *    処方せん料再掲
           ADD     WRK-SYU-SHOHOU-SAI      TO  SYU-SHOHOU-SAI
           ADD     WRK-SYU-JIHI-TOTAL      TO  SYU-JIHI-TOTAL
           ADD     WRK-SYU-JIHI-TOTAL-TAX  TO  SYU-JIHI-TOTAL-TAX
           ADD     WRK-SYU-JIHI-TAX        TO  SYU-JIHI-TAX
           ADD     WRK-SYU-RJN-FTN         TO  SYU-RJN-FTN
           ADD     WRK-SYU-KOH-FTN         TO  SYU-KOH-FTN
           ADD     WRK-SYU-KOH-FTN-ENTANI  TO  SYU-KOH-FTN-ENTANI
           ADD     WRK-SYU-YKZ-FTN         TO  SYU-YKZ-FTN
      *調整金
           ADD     WRK-SYU-CHOSEI          TO  SYU-CHOSEI
           ADD     WRK-SYU-CHOSEI1         TO  SYU-CHOSEI1
           ADD     WRK-SYU-CHOSEI2         TO  SYU-CHOSEI2
      *労災保険
           ADD     WRK-SYU-RSISHOSHIN-MONEY
                                           TO  SYU-RSISHOSHIN-MONEY
           ADD     WRK-SYU-RSISAISHIN-MONEY
                                           TO  SYU-RSISAISHIN-MONEY
           ADD     WRK-SYU-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           ADD     WRK-SYU-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           ADD     WRK-SYU-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           ADD     WRK-SYU-RSI-TOTAL       TO  SYU-RSI-TOTAL
           ADD     WRK-SYU-RSIJIBAI-SKYMONEY
                                           TO  SYU-RSIJIBAI-SKYMONEY
      *請求金額−消費税（再掲）
           ADD     WRK-SYU-SKYMONEY-TAX-SAI TO  SYU-SKYMONEY-TAX-SAI
      *請求金額
           ADD     WRK-SYU-SKYMONEY        TO  SYU-SKYMONEY
      *入金合計額
           ADD     WRK-SYU-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL 
      *診療区分別給付外点数
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   6
               ADD    WRK-SYU-KYUFUGAI-TEN (IDS2)
                                       TO  SYU-KYUFUGAI-TEN(IDS2)
           END-PERFORM
      *
           .
       450141-SYUNOU-ADD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理（労災情報）検索処理
      *****************************************************************
       4500-RSI-SYSKANRI-SEC         SECTION.
      *
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-4001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-4001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4500-RSI-SYSKANRI-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
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
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO            TO  MCP-RC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
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
