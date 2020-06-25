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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI021SUB1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 中途終了データへの編集サブ（一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/05/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  15/11/XX  セット登録対応他
      *  04.08.00    NACL-多々納  17/05/XX  包括検査数、単位コード対応
      *                                     フィルム分画数等対応
      *  05.01.00    NACL-多々納  19/09/XX  改行コードエラー
      *  05.00.00    NACL-多々納  17/11/XX  入院対応追加
      *  05.00.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  05.00.00    NACL-多々納  19/01/XX  複数保険・科対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-INPUTSET            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SURYO               PIC 9(01).
           03  FLG-JIDO-ARI            PIC 9(01).
           03  FLG-SURYO-ZER           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-W               PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDX-I               PIC 9(04).
           03  IDX-I1              PIC 9(04).
           03  IDX-I2              PIC 9(04).
           03  IDX-IMAX            PIC 9(04).
           03  IDX-E               PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-EDABAN              PIC 9(03).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-RENNUM              PIC 9(01).
      *
      *    診療内容
           03  WRK-KAISU               PIC 9(05).
           03  WRK-KAISU-23            PIC 9(05).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-ZAITENKEI           PIC 9(08).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SURYO               PIC 9(05)V9(5).
           03  WRK-MONEY               PIC 9(07).
      *    コメント入力値編集
           03  WRK-INPUTCHI-G.
               05  WRK-INPUTCHI        PIC X(40)   OCCURS  5.
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-TERMID              PIC X(64).
      *
           03  WRK-GYOUMUID            PIC X(04).
      *
           03  WRK-JIDO-KBN            PIC X(01).
      *
      *H29.12
      *    入院日指定
      *    03  TBL-NYUINDAY-AREA.
      *        05  TBL-NYUINDAY-OCC        OCCURS   31.
      *            07  TBL-NYUINDAY        PIC 9(01).
           03  WRK-NISUU-AREA.
               05  WRK-NISUU-TBL           OCCURS   31.
                   07  WRK-DAY             PIC 9(02).
                   07  WRK-SEL             PIC X(01).
           03  IDN1                    PIC 9(04).
           03  IDN2                    PIC 9(04).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
           03  FLG-DAY                 PIC 9(01).
           03  WRK-H-DAY               PIC 9(02).
           03  WRK-H-SEL               PIC X(01).
           03  WRK-KAISU-H             PIC X(04).
      *    末日
           03  WRK-MATU                PIC 9(02).
      *    診療日付
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-Z95                 PIC ZZZZ9.
           03  WRK-INPUTCD-G.
               05  WRK-INPUTCD         PIC X(54)
                                           OCCURS   5.
      *
      *!!!!
      *    複数科・保険組合せ対応
           03  WRK-FUK-HKNCOMBI        PIC 9(04).
           03  WRK-FUK-DRCD-G.
               05  WRK-FUK-DRCD-1      PIC X(01).
               05  WRK-FUK-DRCD        PIC X(04).
           03  FLG-CHK-OK              PIC 9(01).
           03  IDH                     PIC 9(04).
      *
      *    入力コード項目桁数
       01  CONS-INPUT              PIC 9(02)   VALUE   54.
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-ATO-INPUT           PIC X(200).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    ワーク診療行為マスタ−テーブル
       01  TBL-WKSRYACT-AREA.
           02  TBL-WKSRYACT-REC      OCCURS   40.
           COPY    "CPWKSRYACT.INC"  REPLACING  //WKSRY//
                                     BY         //TBL-WKSRY//.
      *
      *対象外コード
       01  CONS-JIDOTBL-AREA.
           03  CONS-JIDOTBL-TBL.
      *調剤料（内服薬・浸煎薬・屯服薬）
               05  FILLER          PIC  X(11)  VALUE   "120000710".
      *調剤料（外用薬）
               05  FILLER          PIC  X(11)  VALUE   "120001010".
      *調剤料（麻・向・覚・毒）（入院外）
               05  FILLER          PIC  X(11)  VALUE   "120000110".
      *!
      *処方料（７種類以上）
               05  FILLER          PIC  X(11)  VALUE   "120002610 1".
      *処方料（その他）
               05  FILLER          PIC  X(11)  VALUE   "120001210 1".
      *処方料（麻・向・覚・毒）加算
               05  FILLER          PIC  X(11)  VALUE   "120001310 1".
      *処方料（乳幼児）加算
               05  FILLER          PIC  X(11)  VALUE   "120002170 1".
      *処方料（向精神薬多剤投与）
               05  FILLER          PIC  X(11)  VALUE   "120003610 1".
      *処方料（紹介率が低い大病院３０日以上投薬減算）
               05  FILLER          PIC  X(11)  VALUE   "120003870 1".
      *（減）
               05  FILLER          PIC  X(11)  VALUE   "820000047" .
      *（精減）
               05  FILLER          PIC  X(11)  VALUE   "820000166" .
      *薬剤料逓減（９０／１００）（内服薬）
               05  FILLER          PIC  X(11)  VALUE   "630010002".
      *薬剤料逓減（８０／１００）（向精神薬多剤投与）
               05  FILLER          PIC  X(11)  VALUE   "630010005".
      *薬剤料逓減（６０／１００）（紹介率が低い大病院３０日以上投薬）
               05  FILLER          PIC  X(11)  VALUE   "630010006".
      *
           03  CONS-JIDOTBL-TBLR   REDEFINES   CONS-JIDOTBL-TBL.
               05  CONS-JIDOTBL-OCC        OCCURS  14
                                           INDEXED    IDX-JIDO.
                   07  CONS-JIDO-CODE          PIC  X(9).
                   07  CONS-JIDO-YOBI          PIC  X(1).
                   07  CONS-JIDO-KBN           PIC  X(1).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *    COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "MCPAREA".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB1V3.INC".
      *
      *2019.1
      *    複数保険対応（画面スパ領域）
           COPY    "K02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORAPI021SUB1V3AREA
      *2019.1
           SPA-K02
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  ORAPISUB01-ERRCD
           INITIALIZE                  ORAPISUB01-INP-ERR-G
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      ORAPISUB01-ERRCD    =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *H27.11
      *    セット登録
           IF      ORAPISUB01-SYORIKBN =   "SET"
               IF      SPA-SRYYMD      =   SPACE
                   MOVE    SPA-SYSYMD      TO  SPA-SRYYMD
               END-IF
           ELSE
      *
      *
           IF     (SPA-PTID            =   ZERO )
              OR  (SPA-SRYKA           =   SPACE)
              OR  (SPA-SRYYMD          =   SPACE)
              OR  (ORAPISUB01-HKNCOMBI =   SPACE)
      *        未設定項目
               MOVE    "01"                TO  ORAPISUB01-ERRCD
               MOVE    "項目未設定"        TO  ORAPISUB01-ERRMSG
               GO      TO      100-INIT-EXT
           END-IF
      *
           END-IF
      *
           MOVE    ORAPISUB01-HKNCOMBI     TO  WRK-HKNCOMBI
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療行為処理
           PERFORM 2002-WKSRYACT-SYORI-SEC
      *
           IF     (CNT-SRYCNT          =   ZERO )
             AND  (ORAPISUB01-ERRCD    =   SPACE)
      *        対象なし
               MOVE    "20"                TO  ORAPISUB01-ERRCD
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       2002-WKSRYACT-SYORI-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  IDX-W
           MOVE    ZERO                TO  IDX-E
      *    診療情報設定
           PERFORM VARYING   IDY   FROM    1   BY  1
                   UNTIL    (IDY           >   110  )
      ***              OR   (ORAPISUB01-MDC-CLASS (IDY) =   SPACE )
                       OR  ((ORAPISUB01-MDC-CLASS (IDY) =   SPACE )
                        AND (ORAPISUB01-MDC-DATA-KBN (IDY)
                                                        =   SPACE ))
                       OR   (ORAPISUB01-ERRCD  NOT =   SPACE )
      *
      *2019.1
               MOVE    SPACE               TO  WRK-ERRCD
               EVALUATE    ORAPISUB01-MDC-DATA-KBN (IDY)
               WHEN    "Com"
      *            複数　保険組合せ
                   PERFORM 200213-FUKU-HKNCOMBI-HEN-SEC
               WHEN    "Dep"
      *            複数　診療科
                   PERFORM 200214-FUKU-SRYKA-HEN-SEC
      *
               WHEN    SPACE
      *            剤毎の編集
                   PERFORM 200211-ZAI-HENSYU-SEC
                   IF      TBL-MAX             >   ZERO
      *                中間データ出力
                       PERFORM 200212-WKSRYACT-OUT-SEC
                   END-IF
               WHEN    OTHER
                   MOVE    "021"               TO  WRK-ERRCD
                   MOVE    ZERO                TO  IDZ
                   MOVE    SPACE               TO  WRK-SRYCD
                   PERFORM 200210-ERRCD-HENSYU-SEC
      *
               END-EVALUATE
      *!!!!
           END-PERFORM
      *
           .
       2002-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *!!!!
      *2019.1
      *****************************************************************
      *    複数科・保険組合せ編集処理
      *****************************************************************
       200213-FUKU-HKNCOMBI-HEN-SEC     SECTION.
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    SPACE               TO  TBL-WKSRYACT-AREA
           INITIALIZE                      TBL-WKSRYACT-AREA
      *
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    1                   TO  WRK-ZAITENKEI
      *
      *    複数　保険組合せ
           INITIALIZE                      ORCSNUMAREA
           MOVE    ORAPISUB01-MDC-COMBINATION (IDY)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        整数４桁
               MOVE    "015"               TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-FUK-HKNCOMBI
      *
               MOVE    ZERO                TO  FLG-CHK-OK
               PERFORM VARYING    IDH      FROM    1   BY  1
                       UNTIL     (IDH      >   SPA-HKN-MAX )
                             OR  (FLG-CHK-OK   =   1   )
                   IF      WRK-FUK-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                               (IDH)
      *                労災・自賠責はエラー
                       IF      ((SPA-NAI-RSI-HKNKBN (IDH)
                                                     NOT =  SPACE )
                            OR  (SPA-NAI-HKNNUM (IDH)    =   "975"))
                           MOVE    "016"           TO  WRK-ERRCD
                       END-IF
                       MOVE    1               TO  FLG-CHK-OK
                   END-IF
               END-PERFORM
               IF      FLG-CHK-OK          =   ZERO
                   MOVE    "015"               TO  WRK-ERRCD
               END-IF
           END-IF
      *    労災・自賠責・公害はエラー
           IF     (SPA-HKNCOMBINUM     =   "9999")
             OR   (SPA-HKNKBN          =   1     )
             OR   (SPA-HKNKBN          =   2     )
               MOVE    "018"               TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    1                   TO  TBL-MAX
               MOVE    SPACE               TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)
               MOVE    "#"                 TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)(1:1)
               MOVE    WRK-FUK-HKNCOMBI    TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)(2:4)
      *        中間データ出力
               PERFORM 200212-WKSRYACT-OUT-SEC
           ELSE
               MOVE    ZERO                TO  IDZ
               MOVE    SPACE               TO  WRK-SRYCD
               PERFORM 200210-ERRCD-HENSYU-SEC
           END-IF
      *
           .
       200213-FUKU-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    複数科・保険組合せ編集処理
      *****************************************************************
       200214-FUKU-SRYKA-HEN-SEC     SECTION.
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    SPACE               TO  TBL-WKSRYACT-AREA
           INITIALIZE                      TBL-WKSRYACT-AREA
      *
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    1                   TO  WRK-ZAITENKEI
      *
      *    診療科チェック
           MOVE    ZERO                TO  FLG-CHK-OK
           PERFORM VARYING    IDH      FROM    1   BY  1
                   UNTIL     (IDH      >   SPA-SRYKA-MAX )
                         OR  (FLG-CHK-OK   =   1   )
               IF      ORAPISUB01-MDC-DEP-CODE (IDY)
                                       =   SPA-GMN-SRYKAL(IDH)
                   MOVE    1               TO  FLG-CHK-OK
               END-IF
           END-PERFORM
           IF      FLG-CHK-OK          =   ZERO
               MOVE    "019"               TO  WRK-ERRCD
               MOVE    ZERO                TO  IDZ
               MOVE    SPACE               TO  WRK-SRYCD
               PERFORM 200210-ERRCD-HENSYU-SEC
           END-IF
      *
      *    ドクターＣＤ
           MOVE    SPACE               TO  WRK-FUK-DRCD-G
           IF      ORAPISUB01-MDC-DR-CODE (IDY)    NOT =   SPACE
      *        システム管理チェック
               MOVE    ORAPISUB01-MDC-DR-CODE (IDY)
                                           TO  WRK-FUK-DRCD-G
               PERFORM 2002141-DRCD-SYSKANRI-SEC
      *
               IF      FLG-SYSKANRI    NOT =   ZERO
                   MOVE    "020"               TO  WRK-ERRCD
                   MOVE    ZERO                TO  IDZ
                   MOVE    SPACE               TO  WRK-SRYCD
                   PERFORM 200210-ERRCD-HENSYU-SEC
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    1                   TO  TBL-MAX
               MOVE    SPACE               TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)
               MOVE    "$"                 TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)(1:1)
               MOVE    ORAPISUB01-MDC-DEP-CODE (IDY)
                                           TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)(2:2)
      *        ドクターコード　４桁
               MOVE    WRK-FUK-DRCD        TO  TBL-WKSRY-INPUTCD
                                                      (TBL-MAX 1)(5:4)
      *        中間データ出力
               PERFORM 200212-WKSRYACT-OUT-SEC
           END-IF
           .
       200214-FUKU-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ドクターＣＤチェック処理
      *****************************************************************
       2002141-DRCD-SYSKANRI-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-FUK-DRCD-G      TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
                INITIALIZE                  SYS-1010-REC
           END-IF
           .
       2002141-DRCD-SYSKANRI-EXT.
           EXIT.
      *!!!!
      *
      *****************************************************************
      *    剤毎の編集処理
      *****************************************************************
       200211-ZAI-HENSYU-SEC     SECTION.
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    SPACE               TO  TBL-WKSRYACT-AREA
           INITIALIZE                      TBL-WKSRYACT-AREA
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
      *    診療種別
      *    診療種別検索
           IF      ORAPISUB01-MDC-CLASS (IDY)  NOT =  SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBN
                   WHEN    ORAPISUB01-MDC-CLASS (IDY)
                                               =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                                TO  WRK-SRYSYUKBN
               END-SEARCH
      *
      *        入力できない診療種別はエラー
               EVALUATE    WRK-SRYSYUKBN
      *            ギブス
                   WHEN    "520"
      *            Ｘ線
                   WHEN    "710"
                   WHEN    "711"
                   WHEN    "712"
                   WHEN    "713"
                   WHEN    "720"
                   WHEN    "721"
                   WHEN    "723"
                   WHEN    "724"
      *                診療種別エラー
                       MOVE    "002"           TO  WRK-ERRCD
                   WHEN    SPACE
      *                診療種別なし
                       MOVE    "002"           TO  WRK-ERRCD
               END-EVALUATE
           ELSE
      *        診療区分から
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBN
                   WHEN    ORAPISUB01-MDC-CLASS-KBN (IDY)
                                               =   MSYU-SRYKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYSYUKBN (MSYU-IDX)
                                                TO  WRK-SRYSYUKBN
               END-SEARCH
      *
               IF      WRK-SRYSYUKBN       =   SPACE
      *            診療種別なし
                   MOVE    "003"           TO  WRK-ERRCD
               END-IF
           END-IF
      *    剤回数
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  WRK-KAISU-23
      *    未設定は１
           IF     (ORAPISUB01-MDC-CLASS-NUMBER (IDY)   =   SPACE )
               MOVE    "1"             TO  ORAPISUB01-MDC-CLASS-NUMBER
                                                              (IDY)
           END-IF
           IF     (ORAPISUB01-MDC-CLASS-NUMBER (IDY)(1:1)  =   "*")
      *        日付指定はなし（入院なし）
               MOVE    "001"               TO  WRK-ERRCD
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    ORAPISUB01-MDC-CLASS-NUMBER (IDY)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   3     )   OR
                      (SNUM-SYOSUU         >   0     )
      *            整数３桁
                   MOVE    "001"               TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-KAISU
               END-IF
           END-IF
           IF      WRK-KAISU           =   ZERO
               MOVE    "001"               TO  WRK-ERRCD
           END-IF
           MOVE    ZERO                TO  IDZ
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    SPACE               TO  WRK-SRYCD
               PERFORM 200210-ERRCD-HENSYU-SEC
           END-IF
      *
      *    外用薬で薬剤の時、回数＝１　編集
           IF      WRK-SRYKBN          =   "23"
               MOVE    WRK-KAISU           TO  WRK-KAISU-23
               MOVE    1                   TO  WRK-KAISU
           END-IF
      *
           MOVE    ZERO                TO  IDX-S
           MOVE    1                   TO  TBL-MAX
      *    剤内容
           PERFORM VARYING   IDZ   FROM    1   BY  1
                   UNTIL    (IDZ           >   50)
               IF     (ORAPISUB01-PRSCRPT-CODE (IDY IDZ)
                                           NOT =   SPACE  )
               OR     (ORAPISUB01-ERRCD    NOT =   SPACE  )
      *        対象外コード判定（処方料・調剤料等は対象外）
                   MOVE    SPACE               TO  WRK-ERRCD
                   MOVE    ORAPISUB01-PRSCRPT-CODE (IDY IDZ)
                                               TO  WRK-SRYCD
                   PERFORM 2002123-JIDO-CHK-SEC
      *
                   IF      WRK-ERRCD           =   SPACE
                       ADD     1               TO  IDX-S
                       IF      IDX-S           >   5
                           ADD     1               TO  TBL-MAX
                           MOVE    1               TO  IDX-S
                       END-IF
                       PERFORM 200212-SRYCD-HENSYU-SEC
                   ELSE
                       PERFORM 200210-ERRCD-HENSYU-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *H29.12
      *    入院の算定日
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 2900-NYUIN-DAYHEN-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    ZERO                TO  IDZ
                   MOVE    SPACE               TO  WRK-SRYCD
                   PERFORM 200210-ERRCD-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200211-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード明細編集処理
      *****************************************************************
       200212-SRYCD-HENSYU-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-SRYCD
      *    診療コード
           INITIALIZE                      ORCSNUMAREA
           MOVE    ORAPISUB01-PRSCRPT-CODE (IDY IDZ)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC             =   ZERO  )   AND
                  (SNUM-MINKBN    NOT  =   "1"   )   AND
                  (SNUM-SYOKBN    NOT  =   "1"   )
      *        整数９桁
               MOVE    SNUM-OUTEDT(7:9)    TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
           ELSE
               MOVE    ORAPISUB01-PRSCRPT-CODE (IDY IDZ)
                                           TO  TBL-WKSRY-SRYCD
                                                     (TBL-MAX IDX-S)
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                           =   "P"   )
                 AND  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(7:)
                                           =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    "004"           TO  WRK-ERRCD
                   PERFORM 200210-ERRCD-HENSYU-SEC
                   GO      TO      200212-SRYCD-HENSYU-EXT
               END-IF
           END-IF
      *
           IF      TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                       =   "P"
      *        セットコード検索
               PERFORM 2002121-INPUTSET-CHK-SEC
           ELSE
               MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                           TO  WRK-SRYCD
      *        点数マスタ検索
               MOVE    SPACE               TO  TNS-TENSU-REC
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *        入力コード
               IF      FLG-TENSU           =   ZERO
                   MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                           TO  TBL-WKSRY-INPUTCD
                                                    (TBL-MAX IDX-S)
      *            時間外区分設定
                   IF     (ORAPISUB01-TIMEFLG  NOT =   SPACE )
                      AND (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                               =   "1"   )
                      AND (TNS-DATAKBN         =   "1"   )
                      AND (TNS-SRYKBN          =   "11"
                                               OR  "12"
                                               OR  "13"    )
                       PERFORM 2002121-TIMEKBN-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 200210-ERRCD-HENSYU-SEC
               GO      TO      200212-SRYCD-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                 TO  FLG-SURYO
      *
      *H27.11
      *    数量ゼロ、有
           MOVE    ZERO                TO  FLG-SURYO-ZER
           IF     (SPA-SURYOZERO-FLG   =   1   )
             AND  (ORAPISUB01-SYORIKBN =   "SET")
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                           =   "7"
                                           OR  "6"   )
                 OR   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                           =   "059" )
                 OR  ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                           =   "1"   ) AND
                      (TNS-KZMCOMPSIKIBETU NOT =   0 ))
                   MOVE    1                   TO  FLG-SURYO-ZER
               END-IF
           END-IF
      *
      *    画像診断のフィルム
           IF    ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                       =   "7"   )  OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                       =   "059"  ))  AND
                  (TNS-DATAKBN         =   "3"    )   AND
                  (WRK-SRYKBN          =   "70"   )   AND
                  (ORAPISUB01-PRSCRPT-NUMBER(IDY IDZ)  NOT =   SPACE )
               PERFORM 200322-FIRUMU-HEN70-SEC
               MOVE    1               TO  FLG-SURYO
           END-IF
      *
           IF      FLG-SURYO           =   ZERO
               MOVE    ZERO                TO  WRK-SURYO
      *        数量
               IF      ORAPISUB01-PRSCRPT-NUMBER(IDY IDZ)  =   SPACE
      *            未設定は「１」
                   MOVE    1               TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
               ELSE
      *            数値変換
                   MOVE    ZERO                TO  WRK-SURYO
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    ORAPISUB01-PRSCRPT-NUMBER(IDY IDZ)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
                          (SNUM-SEISUU         >   5     )   OR
                          (SNUM-SYOSUU         >   5     )
                       MOVE    "007"               TO  WRK-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-SURYO
                       IF      WRK-SURYO           =   ZERO
                           IF      FLG-SURYO-ZER       =   ZERO
                               MOVE    "007"           TO  WRK-ERRCD
                           END-IF
                       END-IF
                   END-IF
                   MOVE    WRK-SURYO           TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
                   IF      WRK-ERRCD           =   SPACE
      *                数量チェック
                       PERFORM 2002122-SURYO-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *    自費金額
           IF     (ORAPISUB01-PRSCRPT-MONEY (IDY IDZ)
                                   NOT =   SPACE  )
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                           =   "095"
                                           OR  "096"  )
                 AND  (TNS-TEN             =   ZERO   )
                   MOVE    ZERO                TO  WRK-MONEY
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    ORAPISUB01-PRSCRPT-MONEY(IDY IDZ)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
                          (SNUM-SEISUU         >   7     )   OR
                          (SNUM-SYOSUU         >   ZERO  )
                       MOVE    "008"               TO  WRK-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-MONEY
                   END-IF
                   MOVE    WRK-MONEY           TO  TBL-WKSRY-JIHIMONEY
                                                     (TBL-MAX IDX-S)
      *            数量と金額の両方はエラー
                   IF     (TBL-WKSRY-SRYSURYO(TBL-MAX IDX-S)
                                           NOT =   1     )
                      AND (WRK-MONEY           >   ZERO  )
                      AND (WRK-ERRCD           =   SPACE )
                       MOVE    "012"               TO  WRK-ERRCD
                   END-IF
               ELSE
                   MOVE    "009"               TO  WRK-ERRCD
               END-IF
           END-IF
      *    コメント値
           IF    ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "84"   )  OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0084"  ) OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                       =   "001"   ) OR
      *R02.4
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "85"    ) OR
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                       =   "831"   ) OR
      *
                  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:7)
                                       =   "0992081"))  AND
                  (ORAPISUB01-ATAI-G  (IDY IDZ)
                                   NOT =   SPACE )
               PERFORM 200321-INPUTCHI-HEN-SEC
           END-IF
      *    フリーコメントの内容
           IF     ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       =   "810000001")  OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:2)
                                       =   "83"      )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0083"    )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0085"    )   OR
                   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                       =   "0086"    ))  AND
                   (ORAPISUB01-PRSCRPT-NAME (IDY IDZ)
                                   NOT =   SPACE )
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                           =   "831"  )
                   CONTINUE
               ELSE
      *
      *        コメント
               MOVE    ORAPISUB01-PRSCRPT-NAME (IDY IDZ)
                                           TO  WRK-MAE-INPUT
      *R02.4   MOVE    80                  TO  WRK-MAX
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT       TO  TBL-WKSRY-INPUTCOMENT
                                                       (TBL-MAX IDX-S)
               ELSE
                   MOVE    "011"               TO  WRK-ERRCD
               END-IF
      *
               END-IF
           END-IF
      *
      *    外用薬で薬剤の時、回数を編集
           IF       WRK-SRYKBN         =   "23"
      *        総量へ変更
               IF      TBL-WKSRY-SRYSURYO (TBL-MAX IDX-S)  >   ZERO
                   COMPUTE WRK-SURYO       =   TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
                                           *   WRK-KAISU-23
                   MOVE    WRK-SURYO       TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
               END-IF
               MOVE    WRK-KAISU-23    TO  TBL-WKSRY-SRYKAISU
                                                       (TBL-MAX IDX-S)
           END-IF
      *    継続指示区分
           IF      ORAPISUB01-CONTKBN (IDY IDZ)  =   "1"
      *        継続コメント
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                               =   "8" )
                 OR   (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                               =   "008" )
                   MOVE    "1"             TO  TBL-WKSRY-INPUTKBN
                                                       (TBL-MAX IDX-S)
               END-IF
           END-IF
           IF      ORAPISUB01-CONTKBN (IDY IDZ)  =   "2"
      *        商品名の器材コード
               IF     (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                               =   "7" )
                 AND  (TBL-WKSRY-SRYCD (TBL-MAX IDX-S - 1)(1:3)
                                               =   "058" )
                   MOVE    "T"             TO  TBL-WKSRY-INPUTKBN
                                                       (TBL-MAX IDX-S)
               END-IF
           END-IF
      *
      *    内服種類数指示区分
           IF      ORAPISUB01-NAIFKBN (IDY IDZ)  =   "1"
      *        内服１種類区分
               MOVE    "2"             TO  TBL-WKSRY-INPUTKBN
                                                       (TBL-MAX IDX-S)
           END-IF
      *    自動区分
           IF      ORAPISUB01-AUTOKBN (IDY IDZ)  NOT =   SPACE
               MOVE    ORAPISUB01-AUTOKBN  (IDY IDZ)
                                       TO  TBL-WKSRY-AUTOKBN
                                                       (TBL-MAX IDX-S)
           END-IF
      *H29.5
      *    自動算定なし区分（在宅療養実績加算など）
      *    (更新時間を流用）
           IF      TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:1)
                                       =   "1"
      *
               EVALUATE    ORAPISUB01-NOAUTO (IDY IDZ)
               WHEN    "YES"
               WHEN    "Yes"
                   MOVE    "Yes"           TO  TBL-WKSRY-INPUTCOMENT
                                                       (TBL-MAX IDX-S)
               END-EVALUATE
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 200210-ERRCD-HENSYU-SEC
           END-IF
      *
           .
       200212-SRYCD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    時間外区分編集処理
      *****************************************************************
       2002121-TIMEKBN-HEN-SEC         SECTION.
      *
      *    時間外加算算定可
           IF     (TNS-TIMEKSNKBN      =   "1"  )
      *       （労災再診料に加算区分の設定がないので判定）
              OR  (TNS-SRYCD           =   "101120010")
               MOVE    SPACE               TO  TBL-WKSRY-INPUTCD
                                                   (TBL-MAX IDX-S)
               STRING  ORAPISUB01-TIMEFLG
                       " "
                       TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                           DELIMITED  BY  SIZE
                                        INTO   TBL-WKSRY-INPUTCD
                                                   (TBL-MAX IDX-S)
               END-STRING
           END-IF
           .
       2002121-TIMEKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量チェック処理
      *****************************************************************
       2002122-SURYO-CHK-SEC         SECTION.
      *
      *    数量入力あり
           IF     (WRK-SRYCD(1:1)      =   "6"
                                       OR  "7"  )
             OR   (WRK-SRYCD(1:3)      =   "059")
             OR   (WRK-SRYCD(1:3)      =   "058")
               MOVE    "S"             TO  TBL-WKSRY-AUTOKBN
                                                     (TBL-MAX IDX-S)
           ELSE
               IF      WRK-SURYO       NOT =   1
                   IF    ((WRK-SRYCD(1:1)      =   "1"  )  AND
                          (TNS-KZMCOMPSIKIBETU NOT =   ZERO ))
                      OR  (WRK-SRYCD(1:3)      =   "095"
                                               OR  "096"  )
      *                数量入力あり
                       MOVE    "S"             TO  TBL-WKSRY-AUTOKBN
                                                     (TBL-MAX IDX-S)
                   ELSE
                       MOVE    "012"               TO  WRK-ERRCD
                   END-IF
      *H29.7   数量空白は数量=1で返却するので、フィルム枚数が反映できない為　廃止。
      ****     ELSE
      *            Ｘ線撮影は１でも数量入力有
      *            IF     (WRK-SRYCD(1:1)      =   "1"  )  AND
      *                   (TNS-KZMCOMPSIKIBETU NOT =   ZERO ) AND
      *                   (TNS-SRYSYUKBN       =   "721"
      *                                        OR  "720"    )
      *                数量入力あり
      *                MOVE    "S"             TO  TBL-WKSRY-AUTOKBN
      *                                              (TBL-MAX IDX-S)
      ****         END-IF
               END-IF
           END-IF
           .
       2002122-SURYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象外コード判定処理
      *****************************************************************
       2002123-JIDO-CHK-SEC         SECTION.
      *
      *
           SET     IDX-JIDO            TO  1
           SEARCH      CONS-JIDOTBL-OCC    VARYING   IDX-JIDO
               AT   END
                   MOVE    ZERO            TO  FLG-JIDO-ARI
                   MOVE    SPACE           TO  WRK-JIDO-KBN
               WHEN    WRK-SRYCD           =   CONS-JIDO-CODE
                                                           (IDX-JIDO)
                   MOVE    1               TO  FLG-JIDO-ARI
                   MOVE    CONS-JIDO-KBN (IDX-JIDO)
                                           TO  WRK-JIDO-KBN
           END-SEARCH 
      *
      *    処方料はシステム管理により対象とする
           IF     (WRK-JIDO-KBN        =   "1" )
              AND (SPA-TEISHOHOU-FLG   =   "1" )
               MOVE    ZERO                TO  FLG-JIDO-ARI
           END-IF
      *
           IF     FLG-JIDO-ARI         =   1
               MOVE    "W13"               TO  WRK-ERRCD
           END-IF
           .
       2002123-JIDO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメント値編集処理
      *****************************************************************
       200321-INPUTCHI-HEN-SEC         SECTION.
      *
      *    用法コメントは５まで
           IF      TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)    =   "001"
               MOVE    5                   TO  IDX-IMAX
           ELSE
               MOVE    4                   TO  IDX-IMAX
           END-IF
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL       IDX-I   >   IDX-IMAX
               IF      ORAPISUB01-ATAI(IDY IDZ IDX-I) NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    ORAPISUB01-ATAI(IDY IDZ IDX-I)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
      *            整数のみ対象
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN         =   "1"   )   OR
      *R02.4              (SNUM-SEISUU         >   8     )   OR
                          (SNUM-SEISUU         >   10    )   OR
                          (SNUM-SYOSUU         >   5     )
                       MOVE    "010"               TO  WRK-ERRCD
                   ELSE
      *                整数がゼロの時、ゼロ
                       IF      SNUM-SEISUU     =   ZERO
                           MOVE    "0"         TO  TBL-WKSRY-INPUTCHI
                                                   (TBL-MAX IDX-S IDX-I)
                       ELSE
                           COMPUTE IDX-I2      =   15  -   SNUM-SEISUU
                                               +   1
                           MOVE    SNUM-OUTEDT(IDX-I2:SNUM-SEISUU)
                                               TO  TBL-WKSRY-INPUTCHI
                                                  (TBL-MAX IDX-S IDX-I)
                       END-IF
      *                ユーザコメントで小数部がある時、そのまま
                       IF   ((TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:4)
                                               =   "0084"   )  OR
                             (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                               =   "001"    )
                          OR (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:7)
                                               =   "0992081"))
                        AND  (SNUM-SYOSUU      >   0        )
                           MOVE    ORAPISUB01-ATAI(IDY IDZ IDX-I)
                                               TO  TBL-WKSRY-INPUTCHI
                                                   (TBL-MAX IDX-S IDX-I)
                       END-IF
                   END-IF
      *R02.4
      *            コメントパターン　８４２
                   IF    (TBL-WKSRY-SRYCD (TBL-MAX IDX-S)(1:3)
                                            =   "842"   )
                       MOVE    SPACE               TO  WRK-ERRCD
                       MOVE    ORAPISUB01-ATAI(IDY IDZ IDX-I)
                                               TO  TBL-WKSRY-INPUTCHI
                                                   (TBL-MAX IDX-S IDX-I)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200321-INPUTCHI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画像診断のフィルム数量編集処理
      *****************************************************************
       200322-FIRUMU-HEN70-SEC         SECTION.
      *
      *    MOVE    SPACE               TO  WRK-INPUTCHI-G
      *    MOVE    1                   TO  IDX-I1
      *    MOVE    ZERO                TO  IDX-I2
      *    PERFORM VARYING     IDX-I   FROM    1   BY  1
      *            UNTIL      (IDX-I   >   12  )   OR
      *                       (IDX-I1  >   4   )
      *        IF      ORAPISUB01-PRSCRPT-NUMBER(IDY IDZ)(IDX-I:1)
      *                                    =   "-"
      *            ADD     1                   TO  IDX-I1
      *            MOVE    ZERO                TO  IDX-I2
      *        ELSE
      *            ADD     1                   TO  IDX-I2
      *            MOVE    ORAPISUB01-PRSCRPT-NUMBER(IDY IDZ)(IDX-I:1)
      *                                        TO  WRK-INPUTCHI
      *                                              (IDX-I1)(IDX-I2:1)
      *        END-IF
      **** END-PERFORM
      *    数量
           MOVE    ZERO                TO  WRK-SURYO
           INITIALIZE                      ORCSNUMAREA
      **** MOVE    WRK-INPUTCHI(1)     TO  SNUM-INX
           MOVE    ORAPISUB01-PRSCRPT-NUMBER(IDY IDZ)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   5     )
               MOVE    "007"               TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-SURYO
               IF      WRK-SURYO           =   ZERO
                   IF      FLG-SURYO-ZER       =   ZERO
                       MOVE    "007"           TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
           MOVE    WRK-SURYO           TO  TBL-WKSRY-SRYSURYO
                                                     (TBL-MAX IDX-S)
      *    分画数
      **** IF      WRK-INPUTCHI(2) NOT =   SPACE
           IF      ORAPISUB01-PRSCRPT-FILMNUM(IDY IDZ) NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-INPUTCHI(2)     TO  SNUM-INX
               MOVE    ORAPISUB01-PRSCRPT-FILMNUM(IDY IDZ)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   7     )   OR
                      (SNUM-SYOSUU         >   5     )
                   MOVE    "017"               TO  WRK-ERRCD
               ELSE
                   IF      SNUM-OUTNUM     NOT =   ZERO
                       MOVE    SNUM-OUTNUM     TO  TBL-WKSRY-SRYKAISU
                                                     (TBL-MAX IDX-S)
                   END-IF
               END-IF
           END-IF
      *
           .
       200322-FIRUMU-HEN70-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコードチェック処理
      *****************************************************************
       2002121-INPUTSET-CHK-SEC     SECTION.
      *
      *    セットコード検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    TBL-WKSRY-SRYCD (TBL-MAX IDX-S)
                                       TO  ISET-SETCD
           MOVE    SPA-SRYYMD          TO  ISET-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *    セットなし
           IF      FLG-INPUTSET        =   1
               MOVE    "006"               TO  WRK-ERRCD
           END-IF
           PERFORM     UNTIL      (FLG-INPUTSET    =   1 )
                               OR (WRK-ERRCD   NOT =   SPACE )
      *        約束セットはエラー
               IF      ISET-INPUTCD (1:1)  =   "S"
                   MOVE    "005"               TO  WRK-ERRCD
               END-IF
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2002121-INPUTSET-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー内容編集処理
      *****************************************************************
       200210-ERRCD-HENSYU-SEC     SECTION.
      *
           ADD     1                   TO  IDX-E
           IF      IDX-E               >   200
               GO      TO      200210-ERRCD-HENSYU-EXT
           END-IF
      *H27.11
           MOVE    WRK-SRYCD           TO  ORAPISUB01-IN-SRYCD (IDX-E)
      *
           MOVE    WRK-ERRCD           TO  ORAPISUB01-IN-ERR (IDX-E)
           MOVE    IDY                 TO  ORAPISUB01-IN-POS (IDX-E)
           MOVE    IDZ                 TO  ORAPISUB01-IN-LINE (IDX-E)
           EVALUATE    WRK-ERRCD
               WHEN    "001"
                   MOVE    "回数指定エラー"    TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "002"
                   MOVE    "診療種別区分がありません"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "003"
                   MOVE    "診療区分がありません"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "004"
      ********     MOVE    "診療コードがありません"
                   MOVE    "診療コードの構成が違います"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "005"
                   STRING  "セット内容に約束セットが存在します。"
                           "使用できません。"
                                           DELIMITED  BY  SIZE
                                           INTO    ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
                   END-STRING
               WHEN    "006"
                   MOVE    "セットコードがありません"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "007"
                   MOVE    "数量の数値エラー"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "017"
                   MOVE    "分画数の数値エラー"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "008"
                   MOVE    "自費金額の数値エラー"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "009"
                   MOVE    "自費金額は設定できません"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "010"
                   MOVE    "埋め込み数値エラー"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "011"
                   MOVE    "名称全角エラー"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "012"
                   MOVE    "数量は設定できません"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "W13"
                   MOVE    "自動算定対象です。削除しました。"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "014"
                   MOVE    "入院算定日区分は０、１で設定して下さい。"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
      *
      *2019.1
               WHEN    "015"
                   MOVE    "保険組合せ番号がありません。"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
               WHEN    "016"
                   STRING  "労災・自賠責分・公害の複数保険設定は"
                           "できません。"
                                           DELIMITED  BY  SIZE
                                           INTO    ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
                   END-STRING
               WHEN    "018"
                   STRING  "労災・自賠責分・公害、包括分入力で"
                           "複数保険設定はできません。"
                                           DELIMITED  BY  SIZE
                                           INTO    ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
                   END-STRING
               WHEN    "019"
                   STRING  "診療科が存在しません"
                                           DELIMITED  BY  SIZE
                                           INTO    ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
                   END-STRING
               WHEN    "020"
                   STRING  "ドクターが存在しません。"
                                           DELIMITED  BY  SIZE
                                           INTO    ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
                   END-STRING
               WHEN    "021"
                   STRING  "複数科・複数保険指示区分がエラーです。"
                                           DELIMITED  BY  SIZE
                                           INTO    ORAPISUB01-IN-ERRMSG
                                                               (IDX-E)
                   END-STRING
      *!!!
           END-EVALUATE
      *
           .
       200210-ERRCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間データ出力処理
      *****************************************************************
       200212-WKSRYACT-OUT-SEC     SECTION.
      *
      *    院内分は剤点数を設定する
           MOVE    ZERO                TO  WRK-ZAITENKEI
           IF     (SPA-SYOHOKBN        =   "0"       )   AND
                  (WRK-SRYKBN          =   "21"  OR  "22"  OR  "23")
               IF      WRK-SRYSYUKBN(3:1)  =   "2"
                   CONTINUE
               ELSE
                   MOVE    1000                TO  WRK-ZAITENKEI
               END-IF
           END-IF
      *
           ADD     1                   TO  WRK-ZAINUM
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL      (IDX-W   >   TBL-MAX)
                           OR (ORAPISUB01-ERRCD  NOT =   SPACE )
               MOVE    TBL-WKSRYACT-REC (IDX-W)    TO  WKSRYACT-REC
               PERFORM 2002121-WKSRYACT-HEN-SEC
      *        ワーク診療行為マスタからの作成判定とする
               MOVE    "WKSRYACT"          TO  WKSRY-TERMID
               MOVE    "APIV3"             TO  WKSRY-OPID
      *        一時データ出力
               PERFORM 2002129-PARA-DBINSERT-SEC
      *
               ADD     1                   TO  CNT-SRYCNT
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  TBL-MAX
           MOVE    SPACE               TO  TBL-WKSRYACT-AREA
           INITIALIZE                      TBL-WKSRYACT-AREA
           .
       200212-WKSRYACT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ出力処理
      *****************************************************************
       2002129-PARA-DBINSERT-SEC          SECTION.
      *
      *R01.11
           IF      WRK-EDABAN          =   999
               MOVE    "42"                TO  ORAPISUB01-ERRCD
               GO  TO  2002129-PARA-DBINSERT-EXT
           END-IF
      *
           ADD     1                   TO  WRK-EDABAN
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    WRK-EDABAN          TO  PARA-EDANUM
           MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  PARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  PARA-UPHMS
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
               MOVE    "80"                TO  ORAPISUB01-ERRCD
               MOVE    "一時データ出力エラー"
                                           TO  ORAPISUB01-ERRMSG
           END-IF
           .
       2002129-PARA-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為編集処理
      *****************************************************************
       2002121-WKSRYACT-HEN-SEC     SECTION.
      *
           ADD     1                   TO  WRK-RENNUM
      *
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    WRK-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    WRK-ZAINUM          TO  WKSRY-ZAINUM
           MOVE    WRK-RENNUM          TO  WKSRY-RENNUM
           MOVE    WRK-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    WRK-KAISU           TO  WKSRY-ZAIKAIKEI
           MOVE    WRK-ZAITENKEI       TO  WKSRY-ZAITENKEI
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
           .
       2002121-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *H29.12
      *****************************************************************
      *    入院の算定日変換処理
      *****************************************************************
       2900-NYUIN-DAYHEN-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *    算定区分チェック
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               EVALUATE    ORAPISUB01-MDC-DAY (IDY IDN1)
               WHEN    SPACE
               WHEN    ZERO
                   MOVE    "0"             TO  ORAPISUB01-MDC-DAY
                                                           (IDY IDN1)
               WHEN    "1"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "014"           TO  WRK-ERRCD
               END-EVALUATE
           END-PERFORM
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2900-NYUIN-DAYHEN-EXT
           END-IF
      *
      *    指定なしは診療日の算定とする
           IF      ORAPISUB01-MDC-DAY-G (IDY)  =   ZERO
               MOVE    SPA-SRYYMD      TO  WRK-SRYYMD
               MOVE    ZERO            TO  ORAPISUB01-MDC-DAY-G
                                                         (IDY)
               MOVE    "1"             TO  ORAPISUB01-MDC-DAY
                                                   (IDY WRK-SRYDD)
           END-IF
      *
           INITIALIZE                      WRK-NISUU-AREA
           MOVE    ZERO                TO  WRK-H-DAY
           MOVE    SPACE               TO  WRK-H-SEL
           MOVE    ZERO                TO  IDN1
           MOVE    ZERO                TO  IDN2
           MOVE    ZERO                TO  IDN3
           MOVE    ZERO                TO  IDN4
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   32
               IF     (IDN1                =   32  )  OR
      *日は別で       (IDN1                >   WRK-MATU)  OR
                      (ORAPISUB01-MDC-DAY(IDY IDN1)
                                           =   "0"    )
                   IF      WRK-H-DAY       NOT =   ZERO
                       IF      WRK-H-SEL   NOT =   SPACE
                           ADD     1               TO  IDN2
                           MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                           MOVE    WRK-H-SEL       TO  WRK-SEL(IDN2 - 1)
                       END-IF
                       MOVE    ZERO            TO  WRK-H-DAY
                       MOVE    SPACE           TO  WRK-H-SEL
                   END-IF
               ELSE
                   IF      WRK-H-DAY           =   ZERO
                       MOVE    IDN1            TO  WRK-H-DAY
                       ADD     1               TO  IDN2
                       MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                   ELSE
                       IF      WRK-H-DAY       =  (IDN1  -  1 )
                           MOVE    "-"             TO  WRK-H-SEL
                       END-IF
                       MOVE    IDN1            TO  WRK-H-DAY
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF     (WRK-SEL (IDN1)      =   SPACE )  AND
                      (IDN1                <   31    )
                   IF      WRK-DAY (IDN1 + 1 )     NOT =   ZERO
                       MOVE    ","          TO  WRK-SEL (IDN1)
                   END-IF
               ELSE
                   IF     (IDN1                    =   31    )  OR
                          (WRK-DAY (IDN1 + 1 )     =   ZERO  )
                       MOVE    SPACE        TO  WRK-SEL (IDN1)
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                      WRK-INPUTCD-G
      *    回数変換
           MOVE    SPACE               TO  WRK-KAISU-H
           MOVE    WRK-KAISU           TO  WRK-Z95
           MOVE    ZERO                TO  IDN4
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   5    )
               IF      WRK-Z95(IDN1:1) NOT =   SPACE
                   ADD     1               TO  IDN4
                   MOVE    WRK-Z95(IDN1:1) TO  WRK-KAISU-H
                                                     (IDN4:1)
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  IDN2
           MOVE    1                   TO  IDN3
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )  OR
                              (IDN2            >=  5  )  OR
                              (WRK-DAY (IDN1)  =   ZERO )
               IF      IDN3                >   CONS-INPUT
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
      *        '-' の次判定
               IF     (WRK-SEL (IDN1)      =   "-" )
                  AND ((IDN3   +   3 )     >=  CONS-INPUT  )
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
               IF      WRK-INPUTCD (IDN2)  =   SPACE
                   STRING  "*"             DELIMITED  BY  SIZE
                           WRK-KAISU-H     DELIMITED  BY  SPACE
                           "/"             DELIMITED  BY  SIZE
                           INTO            WRK-INPUTCD (IDN2)
                   END-STRING
                   ADD     2               TO  IDN3
                   ADD     IDN4            TO  IDN3
               END-IF
               IF      WRK-SEL (IDN1)      =   SPACE
                   MOVE    ","             TO  WRK-SEL (IDN1)
               END-IF
               MOVE    WRK-NISUU-TBL(IDN1) TO  WRK-INPUTCD(IDN2)
                                                       (IDN3:3)
               ADD     3                   TO  IDN3
           END-PERFORM
      *
           PERFORM VARYING    IDN2     FROM    1   BY  1
                   UNTIL     (IDN2     >   5  )
                         OR  (WRK-INPUTCD(IDN2)    =   SPACE)
               ADD     1               TO  IDX-S
               IF      IDX-S           >   5
                   ADD     1               TO  TBL-MAX
                   MOVE    1               TO  IDX-S
               END-IF
               MOVE    WRK-INPUTCD (IDN2)  TO  TBL-WKSRY-INPUTCD
                                                   (TBL-MAX IDX-S)
           END-PERFORM
           .
       2900-NYUIN-DAYHEN-EXT.
           EXIT.
      *H29.12
      *
      *****************************************************************
      *    半角を全角へ変換処理
      *****************************************************************
       880-HALF-ZENCNG-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *    拡張漢字有無
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    WRK-MAE-INPUT       TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )
      *H29.9   改行エラー
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       880-HALF-ZEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
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
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       910-INPUTSET-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       910-INPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
