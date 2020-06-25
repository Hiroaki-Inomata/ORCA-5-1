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
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU4503.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 宮崎・複写式レセプト
      *                      (181,281,381,481,581,681,188,190,195)
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/07/06    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    有村    05/09/09    対象公費番号追加(381)
      *  01.00.02    内田    07/01/19    対象公費番号追加(195)
      *  01.00.03    内田    07/05/08    老人負担者番号等を印字
      *                                  プレビュー時の患者番号を印字
      *                                  レセプト種別の修正（高７）
      *  01.00.04    内田    07/07/11    印字対象番号の追加(190)
      *                                  療養の給付、食事・生活療養、
      *                                  低所得の場合の１，２の記載追加
      *  01.00.05    内田    07/09/17    食事療養費欄等０の場合０を印字
      *  01.00.06    内田    07/10/06    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.07    内田    07/10/23    保険番号190は一部負担金を印字しない
      *  01.00.08    吉川    08/10/27    対象公費番号追加(188,481,581)
      *                                  SPA-AREA初期化
      *  01.00.09    内田    08/11/18    レセプト種別修正（３歳→６歳等）
      *  01.00.10    吉川    09/02/16    公費の助成がない場合は印字しない
      *  01.00.11    吉川    09/06/09    190は助成の有無に関わらず記載
      *  01.00.12    吉川    09/10/28    対象公費番号追加(681)
      *  01.00.13    吉川    10/02/23    特記事項を印字する
      *  01.00.14    吉川    10/06/15    対象公費番号をINIファイルから
      *                                  取得するようにする
      *  01.00.15    吉川    11/11/21    保険＋全国公費＋地方公費の場合
      *                                  保険と公費２の点数は同じにする
      *  01.00.16    吉川    12/02/10    月途中に公費を変更した場合
      *                                  変更前に診療を登録した公費が
      *                                  公費１に印字されるように修正
      *  01.00.17    吉川    12/12/20    医療機関情報編集対応
      *  01.00.18    吉川    13/02/08    プラグイン対応
      *  01.00.19    吉川    13/06/18    プログラムオプション対応
      *  01.00.20    吉川    13/08/09    併用した全国公費の負担者番号と
      *                                  受給者番号が印字されないのを修正
      *  01.00.21    吉川    14/10/28    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.22    萩野    15/06/09    特記事項欄の記載修正
      *  01.00.23    門間    17/06/06    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.24    門間    17/11/01    SITEKBN変更（動作に変更なし）
      *  01.00.25    門間    18/05/02    特記事項に印字内容追加
      *                                  公費欄−一部負担金金額置換対応
      *                      18/05/10    保険欄−一部負担金記載置換対応
      *  01.00.26    門間    19/02/01    修正履歴コメント削除
      *  01.00.27    井上    19/06/05    改元対応
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI4503.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    対象公費番号データ
           COPY  "TAISYO4501.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY  "SC4503.INC"  REPLACING  //HCM09V02//
                               BY         //SC03//.
           COPY  "SC4504.INC"  REPLACING  //HCN06V03//
                               BY         //SC04//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
           03  STS-SYUTURYOKU PIC X(02).
           03  STS-TAISYOKOHI PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-SYSKANRI3  PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-TAISYOKOHI PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-SRYACT     PIC 9(01).
           03  FLG-PTCOM      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDX4           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
           03  IDW1           PIC 9(04).
           03  IDW2           PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
           03  WRK-PARA-STSRYYM      PIC 9(06).
           03  WRK-PARA-EDSRYYM      PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z29               PIC Z9.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC ZZZZZ.
           03  WRK-Z6                PIC ZZZZZZ.
           03  WRK-Z69               PIC ZZZZZ9.
           03  WRK-Z7                PIC ZZZZZZZ.
           03  WRK-Z79               PIC ZZZZZZ9.
           03  WRK-Z8                PIC ZZZZZZZZ.
           03  WRK-Z89               PIC ZZZZZZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZ.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY           PIC 9(04).
               05  WRK-NMM           PIC 9(02).
               05  WRK-NDD           PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY           PIC 9(04).
               05  WRK-TMM           PIC 9(02).
               05  WRK-TDD           PIC 9(02).
           03  WRK-HKNJANUM          PIC X(08).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-PTID              PIC 9(10).
           03  WRK-PTKOHNUM          PIC X(03).
           03  WRK-SRYYM             PIC X(06).
           03  WRK-FTNJANUM          PIC X(20).
           03  WRK-JKYSNUM           PIC X(20).
           03  WRK-TEKSTYMD          PIC X(08).
           03  WRK-SRYKA-TBL  OCCURS 50.
               05  WRK-SRYKACD       PIC X(02).
               05  WRK-RECEKA        PIC X(02).
               05  WRK-SRYKANAME     PIC X(50).
           03  WRK-CHK-SRYKA         PIC X(02).
           03  WRK-CHK-SKJ           PIC 9(09).
           03  WRK-SYUKEI-TBL.
               05  WRK-SEIKYUGK-G    PIC 9(08)V9(01).
               05  WRK-SEIKYUGK-G2   PIC 9(08).
               05  WRK-SEIKYUGK      PIC 9(08)V9(01).
               05  WRK-KOHTENSU      PIC 9(07).
               05  WRK-KOHTENSU-G    PIC 9(07).
           03  WRK-PATH              PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
           03  WRK-STUNUM-T.
               05  FILLER            PIC X(21).
               05  WRK-STUNUM        PIC X(02).
      *
      *    医療機関情報
           03  WRK-HOSPNAME          PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1     PIC X(50).
               05  WRK-HOSPNAME2     PIC X(50).
           03  WRK-HOSPADRS          PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1     PIC X(50).
               05  WRK-HOSPADRS2     PIC X(50).
           03  WRK-HOSPTEL           PIC X(15).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-INPUTCOMENT         PIC X(80).
           03  WRK-INPUTCOMENT2        PIC X(14).
           03  WRK-INPUTCOMENT-KIN-T.
               05  WRK-INPUTCOMENT-KIN     PIC 9(07).
           03  WRK-INPUTCOMENT-KIN2-T.
               05  WRK-INPUTCOMENT-KIN2    PIC X(07).
      *
      *    レセプト作成指示(印刷順)情報退避エリア
           03  WRK-2003-TBL OCCURS 100.
               05  WRK-2003-TEISYUTUSAKI       PIC X(01).
               05  WRK-2003-NYUGAIKBN          PIC X(01).
               05  WRK-2003-DSPSEQ             PIC 9(03).
               05  WRK-2003-SYUBETU-G.
                   07  WRK-2003-SYUBETU        PIC X(04) OCCURS 10.
      *
      *   特記事項
       01  TBL-TOKKI-AREA.
           03  FILLER              PIC X(02)   VALUE   "01".
           03  FILLER              PIC X(10)   VALUE   "０１公".
           03  FILLER              PIC X(02)   VALUE   "02".
           03  FILLER              PIC X(10)   VALUE   "０２長".
           03  FILLER              PIC X(02)   VALUE   "03".
           03  FILLER              PIC X(10)   VALUE   "０３長処".
           03  FILLER              PIC X(02)   VALUE   "04".
           03  FILLER              PIC X(10)   VALUE   "０４後保".
           03  FILLER              PIC X(02)   VALUE   "07".
           03  FILLER              PIC X(10)   VALUE   "０７老併".
           03  FILLER              PIC X(02)   VALUE   "08".
           03  FILLER              PIC X(10)   VALUE   "０８老健".
           03  FILLER              PIC X(02)   VALUE   "09".
           03  FILLER              PIC X(10)   VALUE   "０９施".
           03  FILLER              PIC X(02)   VALUE   "10".
           03  FILLER              PIC X(10)   VALUE   "１０第三".
           03  FILLER              PIC X(02)   VALUE   "11".
           03  FILLER              PIC X(10)   VALUE   "１１薬治".
           03  FILLER              PIC X(02)   VALUE   "12".
           03  FILLER              PIC X(10)   VALUE   "１２器治".
           03  FILLER              PIC X(02)   VALUE   "13".
           03  FILLER              PIC X(10)   VALUE   "１３先進".
           03  FILLER              PIC X(02)   VALUE   "14".
           03  FILLER              PIC X(10)   VALUE   "１４制超".
           03  FILLER              PIC X(02)   VALUE   "15".
           03  FILLER              PIC X(10)   VALUE   "１５経過".
           03  FILLER              PIC X(02)   VALUE   "16".
           03  FILLER              PIC X(10)   VALUE   "１６長２".
           03  FILLER              PIC X(02)   VALUE   "17".
           03  FILLER              PIC X(10)   VALUE   "１７上位".
           03  FILLER              PIC X(02)   VALUE   "18".
           03  FILLER              PIC X(10)   VALUE   "１８一般".
           03  FILLER              PIC X(02)   VALUE   "19".
           03  FILLER              PIC X(10)   VALUE   "１９低所".
           03  FILLER              PIC X(02)   VALUE   "20".
           03  FILLER              PIC X(10)   VALUE   "２０二割".
           03  FILLER              PIC X(02)   VALUE   "21".
           03  FILLER              PIC X(10)   VALUE   "２１高半".
           03  FILLER              PIC X(02)   VALUE   "22".
           03  FILLER              PIC X(10)   VALUE   "２２多上".
           03  FILLER              PIC X(02)   VALUE   "23".
           03  FILLER              PIC X(10)   VALUE   "２３多一".
           03  FILLER              PIC X(02)   VALUE   "24".
           03  FILLER              PIC X(10)   VALUE   "２４多低".
           03  FILLER              PIC X(02)   VALUE   "26".
           03  FILLER              PIC X(10)   VALUE   "２６区ア".
           03  FILLER              PIC X(02)   VALUE   "27".
           03  FILLER              PIC X(10)   VALUE   "２７区イ".
           03  FILLER              PIC X(02)   VALUE   "28".
           03  FILLER              PIC X(10)   VALUE   "２８区ウ".
           03  FILLER              PIC X(02)   VALUE   "29".
           03  FILLER              PIC X(10)   VALUE   "２９区エ".
           03  FILLER              PIC X(02)   VALUE   "30".
           03  FILLER              PIC X(10)   VALUE   "３０区オ".
           03  FILLER              PIC X(02)   VALUE   "31".
           03  FILLER              PIC X(10)   VALUE   "３１多ア".
           03  FILLER              PIC X(02)   VALUE   "32".
           03  FILLER              PIC X(10)   VALUE   "３２多イ".
           03  FILLER              PIC X(02)   VALUE   "33".
           03  FILLER              PIC X(10)   VALUE   "３３多ウ".
           03  FILLER              PIC X(02)   VALUE   "34".
           03  FILLER              PIC X(10)   VALUE   "３４多エ".
           03  FILLER              PIC X(02)   VALUE   "35".
           03  FILLER              PIC X(10)   VALUE   "３５多オ".
           03  FILLER              PIC X(02)   VALUE   "36".
           03  FILLER              PIC X(10)   VALUE   "３６加治".
           03  FILLER              PIC X(02)   VALUE   "37".
           03  FILLER              PIC X(10)   VALUE   "３７申出".
           03  FILLER              PIC X(02)   VALUE   "38".
           03  FILLER              PIC X(10)   VALUE   "３８医併".
           03  FILLER              PIC X(02)   VALUE   "39".
           03  FILLER              PIC X(10)   VALUE   "３９医療".
       01  TBL-TOKKI-R             REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   37.
               05  TBL-TOKKICD     PIC X(02).
               05  TBL-TOKKINAME   PIC X(10).
       01  TBL-TOKKI-MAX           PIC 9(02)   VALUE   37.
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
      *    保険欄一部負担金置換コード
           03  WRK-CONS-KIN-OKIKAE-HKN  PIC X(09)  VALUE   "099999910".
      *    公費一部負担金置換コード
           03  WRK-CONS-KIN-OKIKAE     PIC X(09)   VALUE   "099999921".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    レセプト作成指示(印刷順)情報
           COPY    "CPSK2003.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
               USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  TAISYO-DAT
           INITIALIZE  RECEERR
      *
           DISPLAY "START"
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU4503"    TO  JOB-PGID
           MOVE    "複写式レセプト"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1001-STYUKYMD(7:2)
                                       SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1001-REC
           ELSE
               DISPLAY "*** SEIKYU4503 SYS 1001 ERR ***"
               MOVE    1           TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"            TO  SYS-1002-STYUKYMD(7:2)
                                       SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1002-REC
           ELSE
               DISPLAY "*** SEIKYU4503 SYS 1002 ERR ***"
               MOVE    1           TO  FLG-END
           END-IF
      *
      *    システム管理マスタよりレセプト出力順を取得
           MOVE    "2003"          TO  SYS-2003-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                   TO  SYS-2003-STYUKYMD(1:6)
                                       SYS-2003-EDYUKYMD(1:6)
           MOVE    "01"            TO  SYS-2003-STYUKYMD(7:2)
                                       SYS-2003-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-2003-HOSPNUM
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE    1       TO  IDX
           PERFORM UNTIL   FLG-SYSKANRI3   =   1
                   OR      IDX     >   100
               IF      FLG-SYSKANRI3   =   ZERO
                   MOVE    SYS-2003-TEISYUTUSAKI
                                   TO  WRK-2003-TEISYUTUSAKI(IDX)
                   MOVE    SYS-2003-SYUBETU-G
                                   TO  WRK-2003-SYUBETU-G(IDX)
                   MOVE    SYS-2003-NYUGAIKBN
                                   TO  WRK-2003-NYUGAIKBN(IDX)
                   MOVE    SYS-2003-DSPSEQ
                                   TO  WRK-2003-DSPSEQ(IDX)
                   ADD     1       TO  IDX
               END-IF
               PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU4503"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    レセプト出力順
           MOVE    "RECESYUTURYOKU"          TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:23)       TO  WRK-STUNUM-T
           DISPLAY "レセプト出力順="  WRK-STUNUM
      *
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  TAISYO-KOHNUM(IDX)
           END-PERFORM
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1  >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1"
               INITIALIZE                      BTPARA-REC
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
      *
           IF       CNT-RECE02   >       0
                    PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY2
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  RECE02-RECESYUBETU  TO  MF100-RECESYUBETU
           MOVE  RECE02-TEISYUTUSAKI TO  MF100-TEISYUTUSAKI
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-TOTALTEN(1)  TO  MF100-TOTALTEN-G
           MOVE  RECE02-JNISSU(1)    TO  MF100-JNISSU-G
           MOVE  RECE02-FTNMONEY(1)  TO  MF100-FTNMONEY-G
           MOVE  ZERO                TO  FLG-ERR
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
      *
             INITIALIZE                            TBL-HIKAKU-KOHI
             MOVE    RECE02-KOHNUM(IDX)        TO  TBL-HKOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  200112-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
               MOVE    1                       TO  FLG-ERR
      *        公費の助成があるかどうか
               MOVE    RECE02-KOHNUM (IDX)     TO  WRK-KOHNUM
               MOVE    RECE02-KOHID (IDX)      TO  WRK-KOHID
               IF      RECE02-NYUGAIKBN    =   "1"
                   MOVE    "key33"             TO  WRK-PATH
               ELSE
                   MOVE    "key25"             TO  WRK-PATH
               END-IF
               PERFORM  920-SYUNOU-SET-SEC
      *
               IF  MF100-KOHNUM      = SPACE
                 MOVE  RECE02-KOHINF      (IDX)  TO  MF100-KOHINF
                 MOVE  MF100-KOHFTNJANUM         TO  MF100-FTNJANUM
                 MOVE  MF100-KOHJKYSNUM          TO  MF100-JKYSNUM
                 COMPUTE IDY     =         IDX   +   1
                 MOVE  RECE02-JNISSU   (IDY)     TO  MF100-JNISSU
                 MOVE  RECE02-TOTALTEN (IDY)     TO  MF100-TOTALTEN
      *          保険番号１９０は一部負担金を印字しない
                 IF  RECE02-KOHNUM (IDX)  NOT = "190"
                     MOVE  RECE02-FTNMONEY(IDY)  TO  MF100-FTNMONEY
                 END-IF
                 MOVE  RECE02-SHOKUJIINF (IDY)   TO  MF100-SHOKUJIINF
                 MOVE  RECE02-PTID               TO  WRK-PTID
                 MOVE  RECE02-KOHNUM(IDX)        TO  WRK-PTKOHNUM
                 MOVE  RECE02-SRYYM(1:6)         TO  WRK-SRYYM
                 PERFORM  930-PTKOHINF-SET-SEC
                 MOVE  WRK-TEKSTYMD              TO  MF100-KOHTEKSTYMD
               ELSE
      *    月の途中で市町村が切り替わった場合の対応
                 MOVE  RECE02-KOHNUM(IDX)        TO  MF100-T-KOHNUM
                 MOVE  RECE02-KOHFTNJANUM(IDX)   TO  MF100-T-FTNJANUM
                 MOVE  RECE02-KOHJKYSNUM(IDX)    TO  MF100-T-JKYSNUM
                 COMPUTE IDY     =         IDX   +   1
                 MOVE  RECE02-JNISSU   (IDY)     TO  MF100-T-JNISSU
                 MOVE  RECE02-TOTALTEN (IDY)     TO  MF100-T-TOTALTEN
      *          保険番号１９０は一部負担金を印字しない
                 IF  RECE02-KOHNUM (IDX)  NOT = "190"
                     MOVE  RECE02-FTNMONEY(IDY)  TO  MF100-T-FTNMONEY
                 END-IF
                 MOVE  RECE02-SHOKUJIINF(IDY)  TO  MF100-T-SHOKUJIINF
                 MOVE  RECE02-PTID               TO  WRK-PTID
                 MOVE  RECE02-KOHNUM(IDX)        TO  WRK-PTKOHNUM
                 MOVE  RECE02-SRYYM(1:6)         TO  WRK-SRYYM
                 PERFORM  930-PTKOHINF-SET-SEC
                 MOVE  WRK-TEKSTYMD              TO  MF100-T-KOHTEKSTYMD
               END-IF
               IF  RECE02-KOHNUM (IDX)  = "190"
                 MOVE  ZERO                    TO  FLG-ERR
               END-IF
             END-IF
           END-PERFORM
      *    出力順設定
      *    保険者番号順
           IF    WRK-STUNUM  =  "01" OR "02"
             EVALUATE  MF100-HKNNUM
      *      ４桁
             WHEN "001"
               MOVE  MF100-HKNJANUM(1:4)  TO  MF100-HKNJANUM-KEY(5:4)
      *      ８桁
             WHEN OTHER
               MOVE  MF100-HKNJANUM       TO  MF100-HKNJANUM-KEY
             END-EVALUATE
             DISPLAY "保険者番号前＝" MF100-HKNJANUM
             DISPLAY "保険者番号後＝" MF100-HKNJANUM-KEY
      *      システム管理マスタの出力順を設定
             PERFORM  VARYING  IDX   FROM  1  BY  1
                      UNTIL    IDX   >     100
               IF    (WRK-2003-TEISYUTUSAKI(IDX)
                                              = MF100-TEISYUTUSAKI)
                 AND (WRK-2003-NYUGAIKBN(IDX) = MF100-NYUGAIKBN)
                 IF  MF100-RECESYUBETU = WRK-2003-SYUBETU(IDX 1)
                   OR                    WRK-2003-SYUBETU(IDX 2)
                   OR                    WRK-2003-SYUBETU(IDX 3)
                   OR                    WRK-2003-SYUBETU(IDX 4)
                   OR                    WRK-2003-SYUBETU(IDX 5)
                   OR                    WRK-2003-SYUBETU(IDX 6)
                   OR                    WRK-2003-SYUBETU(IDX 7)
                   OR                    WRK-2003-SYUBETU(IDX 8)
                   OR                    WRK-2003-SYUBETU(IDX 9)
                   OR                    WRK-2003-SYUBETU(IDX 10)
                   MOVE  WRK-2003-DSPSEQ(IDX)
                                          TO  MF100-DSPSEQ
                   DISPLAY "出力番号=" MF100-DSPSEQ
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
      *    カナ氏名順・患者番号順
           IF  WRK-STUNUM  =  "01" OR "03"
             MOVE  MF100-KANANAME  TO  MF100-KANANAME-KEY
           ELSE
             MOVE  MF100-PTNUM     TO  MF100-PTNUM-KEY
           END-IF
      *
      *
      *    総実日数と同じ場合は記載しない
           IF  MF100-JNISSU    =  MF100-JNISSU-G
             MOVE    ZERO                  TO  MF100-JNISSU
           END-IF
      *
      *    請求管理情報より、全国公費情報取得
           PERFORM  900-SEIKYU-READ-SEC
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     4
             IF  RECE10-TOKKI(IDX2)  NOT =  SPACE
               MOVE  RECE10-TOKKI(IDX2)     TO  MF100-TOKKI(IDX2)
             END-IF
           END-PERFORM
      *
      *    食事取得
           MOVE  RECE10-SHOKUJIINF(1)     TO  MF100-SHOKUJIINF-G
           IF  RECE10-KOHNUM(1) NOT = SPACE
             MOVE    RECE10-KOHNUM(1)     TO  MF100-T-KOHNUM
             MOVE    RECE10-TOTALTEN(2)   TO  MF100-T-TOTALTEN
             MOVE    RECE10-FTNMONEY(2)   TO  MF100-T-FTNMONEY
      *    食事取得
             MOVE    RECE10-SHOKUJIINF(2)     TO  MF100-T-SHOKUJIINF
             MOVE  RECE10-PTID            TO  WRK-PTID
             MOVE  RECE10-KOHNUM(1)       TO  WRK-PTKOHNUM
             MOVE  RECE10-SRYYM(1:6)      TO  WRK-SRYYM
             PERFORM  930-PTKOHINF-SET-SEC
             MOVE  WRK-FTNJANUM           TO  MF100-T-FTNJANUM
             MOVE  WRK-JKYSNUM            TO  MF100-T-JKYSNUM
      *    公費点数=保険点数(地方公費点数)の場合は記載しない
             IF  MF100-TOTALTEN  =  MF100-T-TOTALTEN
               MOVE     ZERO              TO  MF100-T-TOTALTEN
             ELSE
               CONTINUE
             END-IF
           END-IF
      *
      *    総点数と公費２が同じでも、全国公費との分点がある場合は記載
           IF  (RECE10-KOHNUM(1)  NOT =  SPACE)
           AND (MF100-T-TOTALTEN  NOT =  ZERO)
             CONTINUE
           ELSE
      *    総点数と同じ場合は記載しない
             IF  MF100-TOTALTEN  =  MF100-TOTALTEN-G
               MOVE    ZERO               TO  MF100-TOTALTEN
             END-IF
           END-IF
      *
      *    診療行為情報取得
           MOVE    WRK-CONS-KIN-OKIKAE-HKN   TO  WRK-SRYCD
           PERFORM 940-SRYACT-SET-SEC
           IF  FLG-SRYACT  =  ZERO
      *      保険欄−一部負担金置換編集
             PERFORM  200112-HKN-ICHIBUFTN-HENSYU-SEC
           END-IF
      *    診療行為情報取得
           MOVE    WRK-CONS-KIN-OKIKAE     TO  WRK-SRYCD
           PERFORM 940-SRYACT-SET-SEC
           IF  FLG-SRYACT  =  ZERO
      *      公費−一部負担金置換編集
             PERFORM  200112-ICHIBUFTN-HENSYU-SEC
           END-IF
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF      FLG-ERR     =   ZERO
               WRITE   MF100-REC
               ADD     1       TO  CNT-MF100
           ELSE
               DISPLAY "対象外患者番号=" RECE02-PTNUM
                       "診療年月="       RECE02-SRYYM
                       "負担者番号="     MF100-FTNJANUM
                       "点数="           MF100-TOTALTEN
           END-IF
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "45"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-FTNJANUM(5:3)      TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄−一部負担金金額編集処理
      *****************************************************************
       200112-HKN-ICHIBUFTN-HENSYU-SEC             SECTION.
      *
           INITIALIZE                             WRK-INPUTCOMENT
                                                  WRK-INPUTCOMENT2
                                                  WRK-INPUTCOMENT-KIN
                                                  WRK-INPUTCOMENT-KIN2
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  5
             IF  ((SRY-SRYCD(IDX4)(1:1)  =  "8")  OR
                  (SRY-SRYCD(IDX4)(1:3)  =  "008"))
             AND ( SRY-INPUTNUM(IDX4)  NOT =  ZERO )
      *
      *        患者コメント情報読込
               PERFORM  950-PTCOM-READ-SEC
               MOVE  PTCOM-INPUTCOMENT           TO  WRK-INPUTCOMENT
      *
               IF  WRK-INPUTCOMENT(1:12)  =  "（再掲上）："
               OR  WRK-INPUTCOMENT(1:12)  =  "（再掲下）："
               OR  WRK-INPUTCOMENT(1:12)  =  "（再掲３）："
               OR  WRK-INPUTCOMENT(1:12)  =  "（再掲４）："
               OR  WRK-INPUTCOMENT(1:12)  =  "保険負担金："
                 MOVE  WRK-INPUTCOMENT(13:14)    TO  WRK-INPUTCOMENT2
      *
      *          置換金額編集
                 IF  WRK-INPUTCOMENT2  =  "　　　　　　　"  OR  SPACE
                   CONTINUE
                 ELSE
      *            全角数字を半角に置き換える
                   PERFORM  VARYING  IDW1  FROM  1  BY  2
                            UNTIL    IDW1  >  14
                     COMPUTE  IDW2  =  (IDW1  +  1)  /  2
                     EVALUATE  WRK-INPUTCOMENT2(IDW1:2)
                       WHEN  "０"
                         MOVE  "0"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "１"
                         MOVE  "1"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "２"
                         MOVE  "2"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "３"
                         MOVE  "3"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "４"
                         MOVE  "4"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "５"
                         MOVE  "5"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "６"
                         MOVE  "6"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "７"
                         MOVE  "7"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "８"
                         MOVE  "8"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "９"
                         MOVE  "9"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  OTHER
                         MOVE  14   TO  IDW1
                     END-EVALUATE
                   END-PERFORM
      *
                   IF  WRK-INPUTCOMENT-KIN2-T  NOT =  SPACE
      *              右詰めの数字項目に変換する
                     MOVE    8      TO  IDW2
                     PERFORM  VARYING  IDW1  FROM  7  BY  -1
                              UNTIL    IDW1  <  1
                       IF  WRK-INPUTCOMENT-KIN2-T(IDW1:1)  =  SPACE
                         CONTINUE
                       ELSE
                         COMPUTE  IDW2  =  IDW2  -  1
                         MOVE  WRK-INPUTCOMENT-KIN2-T(IDW1:1)
                                    TO  WRK-INPUTCOMENT-KIN-T(IDW2:1)
                       END-IF
                     END-PERFORM
      *
      *              集計用ファイルに格納
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲上）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI1
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI1
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲下）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI2
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI2
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲３）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI3
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI3
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "（再掲４）："
                       MOVE  ZERO        TO  MF101-OKIKAE-SAIKEI4
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-SAIKEI4
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "保険負担金："
                       MOVE  ZERO        TO  MF101-OKIKAE-HKN
                       MOVE  WRK-INPUTCOMENT-KIN
                                         TO  MF101-OKIKAE-HKN
                     END-IF


                   END-IF
      *
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       200112-HKN-ICHIBUFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費欄−一部負担金金額編集処理
      *****************************************************************
       200112-ICHIBUFTN-HENSYU-SEC                SECTION.
      *
           INITIALIZE                             WRK-INPUTCOMENT
                                                  WRK-INPUTCOMENT2
                                                  WRK-INPUTCOMENT-KIN
                                                  WRK-INPUTCOMENT-KIN2
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  5
             IF  ((SRY-SRYCD(IDX4)(1:1)  =  "8")  OR
                  (SRY-SRYCD(IDX4)(1:3)  =  "008"))
             AND ( SRY-INPUTNUM(IDX4)  NOT =  ZERO )
      *
      *        患者コメント情報読込
               PERFORM  950-PTCOM-READ-SEC
               MOVE  PTCOM-INPUTCOMENT           TO  WRK-INPUTCOMENT
      *
               IF  WRK-INPUTCOMENT(1:12)  =  "公１負担金："
               OR  WRK-INPUTCOMENT(1:12)  =  "公２負担金："
               OR  WRK-INPUTCOMENT(1:12)  =  "公３負担金："
               OR  WRK-INPUTCOMENT(1:12)  =  "公４負担金："
                 MOVE  WRK-INPUTCOMENT(13:14)    TO  WRK-INPUTCOMENT2
      *
      *          置換金額編集
                 IF  WRK-INPUTCOMENT2  =  "　　　　　　　"  OR  SPACE
                   CONTINUE
                 ELSE
      *            全角数字を半角に置き換える
                   PERFORM  VARYING  IDW1  FROM  1  BY  2
                            UNTIL    IDW1  >  14
                     COMPUTE  IDW2  =  (IDW1  +  1)  /  2
                     EVALUATE  WRK-INPUTCOMENT2(IDW1:2)
                       WHEN  "０"
                         MOVE  "0"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "１"
                         MOVE  "1"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "２"
                         MOVE  "2"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "３"
                         MOVE  "3"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "４"
                         MOVE  "4"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "５"
                         MOVE  "5"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "６"
                         MOVE  "6"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "７"
                         MOVE  "7"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "８"
                         MOVE  "8"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  "９"
                         MOVE  "9"  TO  WRK-INPUTCOMENT-KIN2-T(IDW2:1)
                       WHEN  OTHER
                         MOVE  14   TO  IDW1
                     END-EVALUATE
                   END-PERFORM
      *
                   IF  WRK-INPUTCOMENT-KIN2-T  NOT =  SPACE
      *              右詰めの数字項目に変換する
                     MOVE    8      TO  IDW2
                     PERFORM  VARYING  IDW1  FROM  7  BY  -1
                              UNTIL    IDW1  <  1
                       IF  WRK-INPUTCOMENT-KIN2-T(IDW1:1)  =  SPACE
                         CONTINUE
                       ELSE
                         COMPUTE  IDW2  =  IDW2  -  1
                         MOVE  WRK-INPUTCOMENT-KIN2-T(IDW1:1)
                                    TO  WRK-INPUTCOMENT-KIN-T(IDW2:1)
                       END-IF
                     END-PERFORM
      *
      *              集計用ファイルに格納
                     IF  WRK-INPUTCOMENT(1:12)  =  "公１負担金："
                       MOVE  WRK-INPUTCOMENT-KIN
                                    TO  MF100-OKIKAE-FTN(1)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公２負担金："
                       MOVE  WRK-INPUTCOMENT-KIN
                                    TO  MF100-OKIKAE-FTN(2)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公３負担金："
                       MOVE  WRK-INPUTCOMENT-KIN
                                    TO  MF100-OKIKAE-FTN(3)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公４負担金："
                       MOVE  WRK-INPUTCOMENT-KIN
                                    TO  MF100-OKIKAE-FTN(4)
                     END-IF
                   END-IF
      *
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       200112-ICHIBUFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       200112-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       TAISYO-KOHNUM(IDXX)  =  SPACE
             IF  TBL-HKOHNUM(1)  =  TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(2)  =  TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(3)  =  TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(4)  =  TAISYO-KOHNUM(IDXX)
               MOVE     1                   TO  FLG-TAISYO
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       200112-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *
           OPEN    INPUT   MF100-FILE
      *
           MOVE    ZERO    TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
      *
           PERFORM UNTIL   FLG-PRINT   =   1
      *
               IF  MF100-NYUGAIKBN  =  "2"
      *
      *            帳票編集<見出し>処理
                   PERFORM  310-HEAD-HEN-SEC
      *
      *            帳票編集<明細>処理
                   PERFORM  320-BODY-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  330-PRINT-OUT-SEC
               ELSE
      *
      *            帳票編集<見出し>処理
                   PERFORM  311-HEAD-HEN-SEC
      *
      *            帳票編集<明細>処理
                   PERFORM  321-BODY-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  331-PRINT-OUT-SEC
               END-IF
      *
      *        一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE   MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
      *
           ADD     1   TO  CNT-PAGE
      *
           INITIALIZE  SC03
      *
      *    患者番号
           MOVE    MF100-PTNUM     TO  SC03-PTNUM
      *
      *    提出先
           IF      MF100-RECESYUBETU  =    ZERO    OR  8888
               MOVE    SPACE           TO  SC03-TEISYUTUSAKI
           ELSE
               EVALUATE    MF100-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "１社"          TO  SC03-TEISYUTUSAKI
                   WHEN    2
                       MOVE    "２国"          TO  SC03-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  SC03-TEISYUTUSAKI
               END-EVALUATE
           END-IF
      *
      *    診療年月（診療年月日として退避）
           MOVE    MF100-SRYYM        TO  WRK-SYMD
           MOVE    1                   TO  WRK-SDD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3-G  TO  SC03-SRYG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  SC03-SRYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  SC03-SRYM
      *
      *    県番
           MOVE    SYS-1001-PREFNUM    TO  SC03-PREFNUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  SC03-HOSPCDN
      *
      *    <保険種別１>
           MOVE    "１社"              TO  SC03-HKNSBT1
      *
      *    <保険種別２>
      *    不明分・自費は編集しない
           IF      MF100-RECESYUBETU  =    ZERO    OR  8888
               CONTINUE
           ELSE
               EVALUATE    MF100-RECESYUBETU (3:1)
                   WHEN    "1"
                       MOVE    "１単独"        TO  SC03-HKNSBT2
                   WHEN    "2"
                       MOVE    "２２併"        TO  SC03-HKNSBT2
                   WHEN    "3"
                       MOVE    "３３併"        TO  SC03-HKNSBT2
               END-EVALUATE
           END-IF
      *
      *    <本人・家族>
      *    不明分・自費は編集しない
           EVALUATE    MF100-RECESYUBETU (4:1)
               WHEN    "0"
                   MOVE    "０高外７"      TO  SC03-HONKZK
               WHEN    "2"
                   MOVE    "２本外"        TO  SC03-HONKZK
               WHEN    "4"
                   MOVE    "４六外"        TO  SC03-HONKZK
               WHEN    "6"
                   MOVE    "６家外"        TO  SC03-HONKZK
               WHEN    "8"
                   MOVE    "８高外一"      TO  SC03-HONKZK
           END-EVALUATE
      *
      *    老人市町村番号
           MOVE    MF100-RJNFTNJANUM    TO  SC03-RJNFTNJANUM
      *    老人受給者番号
           MOVE    MF100-RJNJKYSNUM     TO  SC03-RJNJKYSNUM
      *
      *    保険者番号
           IF      MF100-HKNNUM    =   "001"
      *        政府管掌の場合（４桁右詰に編集）
               MOVE    SPACE           TO  WRK-HKNJANUM(1:4)
               MOVE    MF100-HKNJANUM  TO  WRK-HKNJANUM(5:)
           ELSE
      *        他の保険の場合（８桁）
               MOVE    MF100-HKNJANUM  TO  WRK-HKNJANUM
           END-IF
           MOVE    WRK-HKNJANUM        TO  SC03-HKNJANUM
      *
      *    記号・番号
           MOVE    MF100-KIGO          TO  SC03-KIGO
           MOVE    MF100-NUM           TO  SC03-NUM
      *
      *    カナ氏名
           IF     MF100-KANANAME (41:)    NOT =   SPACE
                MOVE    MF100-KANANAME  TO  SC03-KANANAME2
           ELSE
                MOVE    MF100-KANANAME  TO  SC03-KANANAME
           END-IF
      *
      *    氏名
           MOVE    MF100-NAME          TO  SC03-NAME
      *
      *    性別
           EVALUATE    MF100-SEX
           WHEN    "1"
               MOVE    "１"            TO  SC03-SEXNUM
               MOVE    "男"            TO  SC03-SEXN
           WHEN    "2"
               MOVE    "２"            TO  SC03-SEXNUM
               MOVE    "女"            TO  SC03-SEXN
           END-EVALUATE
      *
      *    生年月日
           MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           MOVE    LNK-DAY1-WYMD       TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:2)
                                       TO  SC03-BIRTHDAY(1:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2 (1:2)
                                       TO  SC03-BIRTHDAY(3:2)
           MOVE    LNK-DAY2-EDTYMD2 (5:)
                                       TO  SC03-BIRTHDAY(5:)
      *
      *    医療機関所在地
           MOVE    WRK-HOSPADRS        TO  SC03-HOSPADRS
           INSPECT SC03-HOSPADRS       REPLACING ALL "  " BY "　"
      *
      *    医療機関名称
           MOVE    WRK-HOSPNAME        TO  SC03-HOSPNAME
           INSPECT SC03-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  SC03-HOSPTEL
      *
      *    特記事項
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     4
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL    IDY  >   TBL-TOKKI-MAX
               IF  MF100-TOKKI(IDX2)  =  TBL-TOKKICD(IDY)
                 MOVE  TBL-TOKKINAME(IDY)   TO  SC03-TOKKI(IDX2)
                 MOVE  TBL-TOKKI-MAX        TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    点数１
           MOVE    MF100-TOTALTEN-G        TO  WRK-Z7
           MOVE    WRK-Z7                  TO  SC03-TOTALTEN(1)
      *    実日数１
           MOVE    MF100-JNISSU-G          TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-JINISSU
      *    負担金額
           IF  MF101-OKIKAE-HKN  NOT =  ZERO
               MOVE    MF101-OKIKAE-HKN      TO  WRK-Z8
               MOVE    WRK-Z8                TO  SC03-FTNMONEY(1)
           ELSE
               MOVE    MF100-FTNMONEY-G      TO  WRK-Z8
               MOVE    WRK-Z8                TO  SC03-FTNMONEY(1)
           END-IF
      *
      *    全国公費併用の場合
           IF      MF100-T-KOHNUM(1:1)     =   "0"
               MOVE    1       TO  IDX1
               MOVE    2       TO  IDX2
      *        負担者番号１(全国公費)
               MOVE    MF100-T-FTNJANUM    TO  SC03-KOHFTNJANUM1
      *
      *        受給者番号１(全国公費)
               MOVE    MF100-T-JKYSNUM     TO  SC03-KOHJKYSNUM1
      *
      *        点数２(全国公費)
               MOVE    MF100-T-TOTALTEN    TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC03-TOTALTEN(2)
      *
      *        一部負担金２(全国公費)
               IF  MF100-OKIKAE-FTN(1)  NOT =  ZERO
                 MOVE    MF100-OKIKAE-FTN(1) TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC03-FTNMONEY(2)
               ELSE
                 MOVE    MF100-T-FTNMONEY    TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC03-FTNMONEY(2)
               END-IF
      *
      *        実日数２(全国公費)
               MOVE    MF100-T-JNISSU      TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC03-JINISSU1
      *
      *        負担者番号２(地方公費)
               MOVE    MF100-FTNJANUM      TO  SC03-KOHFTNJANUM2
      *
      *        受給者番号２(地方公費)
               MOVE    MF100-JKYSNUM       TO  SC03-KOHJKYSNUM2
      *
      *        点数３(地方公費)
               MOVE    MF100-TOTALTEN      TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC03-TOTALTEN(3)
      *
      *        一部負担金３(地方公費)
               IF  MF100-OKIKAE-FTN(2)  NOT =  ZERO
                 MOVE    MF100-OKIKAE-FTN(2) TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC03-FTNMONEY(3)
               ELSE
                 MOVE    MF100-FTNMONEY      TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC03-FTNMONEY(3)
               END-IF
      *
      *        実日数３(地方公費)
               MOVE    MF100-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC03-JINISSU2
      *
           ELSE
      *
      *        公費適用期間の若い順に記載
               IF  (MF100-T-KOHTEKSTYMD  =  SPACE)
               OR  (MF100-KOHTEKSTYMD  <=  MF100-T-KOHTEKSTYMD)
      *          負担者番号１(地方公費１つ目)
                 MOVE    MF100-FTNJANUM      TO  SC03-KOHFTNJANUM1
      *
      *          受給者番号１(地方公費１つ目)
                 MOVE    MF100-JKYSNUM       TO  SC03-KOHJKYSNUM1
      *
      *          点数２(地方公費１つ目)
                 MOVE    MF100-TOTALTEN      TO  WRK-Z7
                 MOVE    WRK-Z7              TO  SC03-TOTALTEN(2)
      *
      *          一部負担金２(地方公費１つ目)
                 IF  MF100-OKIKAE-FTN(1)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(1) TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(2)
                 ELSE
                   MOVE    MF100-FTNMONEY      TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(2)
                 END-IF
      *
      *          実日数２(地方公費１つ目)
                 MOVE    MF100-JNISSU        TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC03-JINISSU1
      *
      *          負担者番号２(地方公費２つ目)
                 MOVE    MF100-T-FTNJANUM    TO  SC03-KOHFTNJANUM2
      *
      *          受給者番号２(地方公費２つ目)
                 MOVE    MF100-T-JKYSNUM     TO  SC03-KOHJKYSNUM2
      *
      *          点数３(地方公費２つ目)
                 MOVE    MF100-T-TOTALTEN    TO  WRK-Z7
                 MOVE    WRK-Z7              TO  SC03-TOTALTEN(3)
      *
      *          一部負担金３(地方公費２つ目)
                 IF  MF100-OKIKAE-FTN(2)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(2) TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(3)
                 ELSE
                   MOVE    MF100-T-FTNMONEY    TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(3)
                 END-IF
      *
      *          実日数３(地方公費２つ目)
                 MOVE    MF100-T-JNISSU      TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC03-JINISSU2
      *
               ELSE
      *
      *          負担者番号１(地方公費２つ目)
                 MOVE    MF100-T-FTNJANUM    TO  SC03-KOHFTNJANUM1
      *
      *          受給者番号１(地方公費２つ目)
                 MOVE    MF100-T-JKYSNUM     TO  SC03-KOHJKYSNUM1
      *
      *          点数２(地方公費２つ目)
                 MOVE    MF100-T-TOTALTEN    TO  WRK-Z7
                 MOVE    WRK-Z7              TO  SC03-TOTALTEN(2)
      *
      *          一部負担金２(地方公費２つ目)
                 IF  MF100-OKIKAE-FTN(1)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(1) TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(2)
                 ELSE
                   MOVE    MF100-T-FTNMONEY    TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(2)
                 END-IF
      *
      *          実日数２(地方公費２つ目)
                 MOVE    MF100-T-JNISSU      TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC03-JINISSU1
      *
      *          負担者番号２(地方公費１つ目)
                 MOVE    MF100-FTNJANUM      TO  SC03-KOHFTNJANUM2
      *
      *          受給者番号２(地方公費１つ目)
                 MOVE    MF100-JKYSNUM       TO  SC03-KOHJKYSNUM2
      *
      *          点数３(地方公費１つ目)
                 MOVE    MF100-TOTALTEN      TO  WRK-Z7
                 MOVE    WRK-Z7              TO  SC03-TOTALTEN(3)
      *
      *          一部負担金３(地方公費１つ目)
                 IF  MF100-OKIKAE-FTN(2)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(2) TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(3)
                 ELSE
                   MOVE    MF100-FTNMONEY      TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC03-FTNMONEY(3)
                 END-IF
      *
      *          実日数３(地方公費１つ目)
                 MOVE    MF100-JNISSU        TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC03-JINISSU2
      *
               END-IF
           END-IF
      *
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "SC4503.red"         TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                    TO  SPRT-TITLE
           MOVE    SC03             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           MOVE    MF100-PTID          TO  SPRT-PTID
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
      *
           ADD     1   TO  CNT-PAGE
      *
           INITIALIZE  SC04
      *
      *    患者番号
           MOVE    MF100-PTNUM        TO  SC04-PTNUM
      *
      *    提出先
           IF      MF100-RECESYUBETU  =    ZERO    OR  8888
               MOVE    SPACE           TO  SC04-TEISYUTUSAKI
           ELSE
               EVALUATE    MF100-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "１社"          TO  SC04-TEISYUTUSAKI
                   WHEN    2
                       MOVE    "２国"          TO  SC04-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  SC04-TEISYUTUSAKI
               END-EVALUATE
           END-IF
      *
      *    診療年月（診療年月日として退避）
           MOVE    MF100-SRYYM         TO  WRK-SYMD
           MOVE    1                   TO  WRK-SDD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3-G  TO  SC04-SRYG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  SC04-SRYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  SC04-SRYM
      *
      *    県番
           MOVE    SYS-1001-PREFNUM    TO  SC04-PREFNUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  SC04-HOSPCDN
      *
      *    <保険種別１>
           MOVE    "１社"              TO  SC04-HKNSBT1
      *
      *    <保険種別２>
      *    不明分・自費は編集しない
           IF      MF100-RECESYUBETU   =   ZERO    OR  8888
               CONTINUE
           ELSE
               EVALUATE    MF100-RECESYUBETU (3:1)
                   WHEN    "1"
                       MOVE    "１単独"        TO  SC04-HKNSBT2
                   WHEN    "2"
                       MOVE    "２２併"        TO  SC04-HKNSBT2
                   WHEN    "3"
                       MOVE    "３３併"        TO  SC04-HKNSBT2
               END-EVALUATE
           END-IF
      *
      *    <本人・家族>
      *    不明分・自費は編集しない
           EVALUATE    MF100-RECESYUBETU (4:1)
               WHEN    "1"
                   MOVE    "１本入"     TO  SC04-HONKZK
               WHEN    "3"
                   MOVE    "３六入"     TO  SC04-HONKZK
               WHEN    "5"
                   MOVE    "５家入"     TO  SC04-HONKZK
               WHEN    "7"
                   MOVE    "７高入一"   TO  SC04-HONKZK
               WHEN    "9"
                   MOVE    "９高入７"   TO  SC04-HONKZK
           END-EVALUATE
      *
      *    老人市町村番号
           MOVE    MF100-RJNFTNJANUM    TO  SC04-RJNFTNJANUM
      *    老人受給者番号
           MOVE    MF100-RJNJKYSNUM     TO  SC04-RJNJKYSNUM
      *
      *    保険者番号
           IF  MF100-HKNNUM      =   "001"
      *        政府管掌の場合（４桁右詰に編集）
               MOVE    SPACE           TO  WRK-HKNJANUM(1:4)
               MOVE    MF100-HKNJANUM  TO  WRK-HKNJANUM(5:)
           ELSE
      *        他の保険の場合（８桁）
               MOVE    MF100-HKNJANUM  TO  WRK-HKNJANUM
           END-IF
           MOVE    WRK-HKNJANUM        TO  SC04-HKNJANUM
      *
      *    記号・番号
           MOVE    MF100-KIGO          TO  SC04-KIGO
           MOVE    MF100-NUM           TO  SC04-NUM
      *
      *    カナ氏名
           IF     MF100-KANANAME (41:)    NOT =   SPACE
                MOVE    MF100-KANANAME    TO  SC04-KANANAME2
           ELSE
                MOVE    MF100-KANANAME    TO  SC04-KANANAME
           END-IF
      *
      *    氏名
           MOVE    MF100-NAME          TO  SC04-NAME
      *
      *    性別
           EVALUATE    MF100-SEX
               WHEN    "1"
                   MOVE    "１"        TO  SC04-SEXNUM
                   MOVE    "男"        TO  SC04-SEXN
               WHEN    "2"
                   MOVE    "２"        TO  SC04-SEXNUM
                   MOVE    "女"        TO  SC04-SEXN
           END-EVALUATE
      *
      *    生年月日
           MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           MOVE    LNK-DAY1-WYMD       TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:2)
                                       TO  SC04-BIRTHDAY(1:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2 (1:2)
                                       TO  SC04-BIRTHDAY(3:2)
           MOVE    LNK-DAY2-EDTYMD2 (5:)
                                       TO  SC04-BIRTHDAY(5:)
      *
      *    医療機関所在地
           MOVE    WRK-HOSPADRS        TO  SC04-HOSPADRS
           INSPECT SC04-HOSPADRS       REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SC04-HOSPNAME
           INSPECT SC04-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    電話番号
           MOVE    WRK-HOSPTEL         TO  SC04-HOSPTEL
      *
      *    特記事項
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     4
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL    IDY  >   TBL-TOKKI-MAX
               IF  MF100-TOKKI(IDX2)  =  TBL-TOKKICD(IDY)
                 MOVE  TBL-TOKKINAME(IDY)   TO  SC04-TOKKI(IDX2)
                 MOVE  TBL-TOKKI-MAX        TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    点数１
           MOVE    MF100-TOTALTEN-G        TO  WRK-Z7
           MOVE    WRK-Z7                  TO  SC04-TOTALTEN(1)
      *    実日数１
           MOVE    MF100-JNISSU-G          TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC04-JINISSU
           IF  MF101-OKIKAE-HKN  NOT =  ZERO
              MOVE    MF101-OKIKAE-HKN     TO  WRK-Z8
              MOVE    WRK-Z8               TO  SC04-FTNMONEY(1)
           ELSE
              MOVE    MF100-FTNMONEY-G     TO  WRK-Z8
              MOVE    WRK-Z8               TO  SC04-FTNMONEY(1)
           END-IF
      *    食事日数
           MOVE    MF100-SHOKUJINISSU-G    TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC04-SYONISSU(1)
      *    食事療養費
           MOVE    MF100-SHOKUJIRYOYOHI-G  TO  WRK-Z7
           MOVE    WRK-Z7                  TO  SC04-SYOTOTALTEN(1)
      *    標準負担額
           MOVE    MF100-SHOKUJIFTN-G      TO  WRK-Z8
           MOVE    WRK-Z8                  TO  SC04-SYOFTNMONEY(1)
      *
      *    全国公費併用の場合
           IF      MF100-T-KOHNUM(1:1)      =   "0"
               MOVE    1       TO  IDX1
               MOVE    2       TO  IDX2
      *
      *        負担者番号１(全国公費)
               MOVE    MF100-T-FTNJANUM    TO  SC04-KOHFTNJANUM1
      *
      *        受給者番号１(全国公費)
               MOVE    MF100-T-JKYSNUM     TO  SC04-KOHJKYSNUM1
      *
      *        点数２(全国公費)
               MOVE    MF100-T-TOTALTEN    TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC04-TOTALTEN(2)
      *
      *        一部負担金２(全国公費)
               IF  MF100-OKIKAE-FTN(1)  NOT =  ZERO
                 MOVE    MF100-OKIKAE-FTN(1) TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC04-FTNMONEY(2)
               ELSE
                 MOVE    MF100-T-FTNMONEY    TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC04-FTNMONEY(2)
               END-IF
      *
      *        実日数２(全国公費)
               MOVE    MF100-T-JNISSU      TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-JINISSU1
      *
      *        食事日数（全国公費）
               IF  MF100-SHOKUJINISSU-G  NOT =  MF100-T-SHOKUJINISSU
                 MOVE    MF100-T-SHOKUJINISSU    TO  WRK-Z29
                 MOVE    WRK-Z29             TO  SC04-SYONISSU(2)
               END-IF
      *        食事療養費（全国公費）
               IF  MF100-SHOKUJIRYOYOHI-G NOT = MF100-T-SHOKUJIRYOYOHI
                 MOVE    MF100-T-SHOKUJIRYOYOHI  TO  WRK-Z79
                 MOVE    WRK-Z79             TO  SC04-SYOTOTALTEN(2)
               END-IF
      *        標準負担額（全国公費）
               IF  MF100-SHOKUJIFTN-G  NOT =  MF100-T-SHOKUJIFTN
                 MOVE    MF100-T-SHOKUJIFTN      TO  WRK-Z89
                 MOVE    WRK-Z89             TO  SC04-SYOFTNMONEY(2)
               END-IF
      *
      *        負担者番号２(地方公費)
               MOVE    MF100-FTNJANUM      TO  SC04-KOHFTNJANUM2
      *
      *        受給者番号２(地方公費)
               MOVE    MF100-JKYSNUM       TO  SC04-KOHJKYSNUM2
      *
      *        点数３(地方公費)
               MOVE    MF100-TOTALTEN      TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC04-TOTALTEN(3)
      *
      *        一部負担金３(地方公費)
               IF  MF100-OKIKAE-FTN(2)  NOT =  ZERO
                 MOVE    MF100-OKIKAE-FTN(2) TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC04-FTNMONEY(3)
               ELSE
                 MOVE    MF100-FTNMONEY      TO  WRK-Z8
                 MOVE    WRK-Z8              TO  SC04-FTNMONEY(3)
               END-IF
      *
      *        実日数３(地方公費)
               MOVE    MF100-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC04-JINISSU2
      *
      *        食事日数（地方公費）
               IF  MF100-T-SHOKUJINISSU  NOT =  MF100-SHOKUJINISSU
                 MOVE    MF100-SHOKUJINISSU    TO  WRK-Z2
                 MOVE    WRK-Z2                TO  SC04-SYONISSU(3)
               END-IF
      *        食事療養費（地方公費）
               IF  MF100-T-SHOKUJIRYOYOHI  NOT =  MF100-SHOKUJIRYOYOHI
                 MOVE    MF100-SHOKUJIRYOYOHI  TO  WRK-Z7
                 MOVE    WRK-Z7                TO  SC04-SYOTOTALTEN(3)
               END-IF
      *        標準負担額（地方公費）
               IF  MF100-T-SHOKUJIFTN  NOT =  MF100-SHOKUJIFTN
                 MOVE    MF100-SHOKUJIFTN      TO  WRK-Z8
                 MOVE    WRK-Z8                TO  SC04-SYOFTNMONEY(3)
               END-IF
      *
           ELSE
      *
      *        公費適用期間の若い順に記載
               IF  (MF100-T-KOHTEKSTYMD  =  SPACE)
               OR  (MF100-KOHTEKSTYMD  <=  MF100-T-KOHTEKSTYMD)
      *          負担者番号１(地方公費１つ目)
                 MOVE    MF100-FTNJANUM      TO  SC04-KOHFTNJANUM1
      *
      *          受給者番号１(地方公費１つ目)
                 MOVE    MF100-JKYSNUM       TO  SC04-KOHJKYSNUM1
      *
      *          点数２(地方公費１つ目)
                 MOVE    MF100-TOTALTEN      TO  WRK-Z7
                 MOVE    WRK-Z7              TO  SC04-TOTALTEN(2)
      *
      *          一部負担金２(地方公費１つ目)
                 IF  MF100-OKIKAE-FTN(1)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(1) TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC04-FTNMONEY(2)
                 ELSE
                   MOVE    MF100-FTNMONEY      TO  WRK-Z8
                   MOVE    WRK-Z8              TO  SC04-FTNMONEY(2)
                 END-IF
      *
      *          実日数２(地方公費１つ目)
                 MOVE    MF100-JNISSU        TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC04-JINISSU1
      *
      *          食事日数（地方公費１つ目）
                 IF  MF100-SHOKUJINISSU-G  NOT =  MF100-SHOKUJINISSU
                   MOVE  MF100-SHOKUJINISSU  TO  WRK-Z29
                   MOVE  WRK-Z29             TO  SC04-SYONISSU (2)
                 END-IF
      *          食事療養費（地方公費１つ目）
                 IF  MF100-SHOKUJIRYOYOHI-G  NOT =  MF100-SHOKUJIRYOYOHI
                   MOVE  MF100-SHOKUJIRYOYOHI    TO  WRK-Z79
                   MOVE  WRK-Z79             TO  SC04-SYOTOTALTEN (2)
                 END-IF
      *          標準負担額（地方公費１つ目）
                 IF  MF100-SHOKUJIFTN-G  NOT =  MF100-SHOKUJIFTN
                   MOVE  MF100-SHOKUJIFTN    TO  WRK-Z89
                   MOVE  WRK-Z89             TO  SC04-SYOFTNMONEY (2)
                 END-IF
      *
      *          負担者番号２(地方公費２つ目)
                 MOVE    MF100-T-FTNJANUM     TO  SC04-KOHFTNJANUM2
      *
      *          受給者番号２(地方公費２つ目)
                 MOVE    MF100-T-JKYSNUM         TO  SC04-KOHJKYSNUM2
      *
      *          点数３(地方公費２つ目)
                 MOVE    MF100-T-TOTALTEN        TO  WRK-Z7
                 MOVE    WRK-Z7                  TO  SC04-TOTALTEN(3)
      *
      *          一部負担金３(地方公費２つ目)
                 IF  MF100-OKIKAE-FTN(2)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(2)     TO  WRK-Z8
                   MOVE    WRK-Z8                  TO  SC04-FTNMONEY(3)
                 ELSE
                   MOVE    MF100-T-FTNMONEY        TO  WRK-Z8
                   MOVE    WRK-Z8                  TO  SC04-FTNMONEY(3)
                 END-IF
      *
      *          実日数３(地方公費２つ目)
                 MOVE    MF100-T-JNISSU          TO  WRK-Z2
                 MOVE    WRK-Z2                  TO  SC04-JINISSU2
      *
      *          食事日数（地方公費２つ目）
                 IF  MF100-T-SHOKUJINISSU  NOT =  MF100-SHOKUJINISSU
                   MOVE  MF100-T-SHOKUJINISSU    TO  WRK-Z29
                   MOVE  WRK-Z29             TO  SC04-SYONISSU (3)
                 END-IF
      *          食事療養費（地方公費２つ目）
                 IF  MF100-T-SHOKUJIRYOYOHI  NOT =  MF100-SHOKUJIRYOYOHI
                   MOVE  MF100-T-SHOKUJIRYOYOHI  TO  WRK-Z79
                   MOVE  WRK-Z79             TO  SC04-SYOTOTALTEN (3)
                 END-IF
      *          標準負担額（地方公費２つ目）
                 IF  MF100-T-SHOKUJIFTN  NOT =  MF100-SHOKUJIFTN
                   MOVE  MF100-T-SHOKUJIFTN      TO  WRK-Z89
                   MOVE  WRK-Z89             TO  SC04-SYOFTNMONEY (3)
                 END-IF
      *
               ELSE
      *
      *          負担者番号１(地方公費２つ目)
                 MOVE    MF100-T-FTNJANUM     TO  SC04-KOHFTNJANUM1
      *
      *          受給者番号１(地方公費２つ目)
                 MOVE    MF100-T-JKYSNUM         TO  SC04-KOHJKYSNUM1
      *
      *          点数２(地方公費２つ目)
                 MOVE    MF100-T-TOTALTEN        TO  WRK-Z7
                 MOVE    WRK-Z7                  TO  SC04-TOTALTEN(2)
      *
      *          一部負担金２(地方公費２つ目)
                 IF  MF100-OKIKAE-FTN(1)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(1)     TO  WRK-Z8
                   MOVE    WRK-Z8                  TO  SC04-FTNMONEY(2)
                 ELSE
                   MOVE    MF100-T-FTNMONEY        TO  WRK-Z8
                   MOVE    WRK-Z8                  TO  SC04-FTNMONEY(2)
                 END-IF
      *
      *          実日数２(地方公費２つ目)
                 MOVE    MF100-T-JNISSU          TO  WRK-Z2
                 MOVE    WRK-Z2                  TO  SC04-JINISSU1
      *
      *          食事日数（地方公費２つ目）
                 IF  MF100-SHOKUJINISSU-G  NOT =  MF100-T-SHOKUJINISSU
                   MOVE  MF100-T-SHOKUJINISSU    TO  WRK-Z29
                   MOVE  WRK-Z29             TO  SC04-SYONISSU (2)
                 END-IF
      *          食事療養費（地方公費２つ目）
                 IF  MF100-SHOKUJIRYOYOHI-G  NOT =
                                                  MF100-T-SHOKUJIRYOYOHI
                   MOVE  MF100-T-SHOKUJIRYOYOHI  TO  WRK-Z79
                   MOVE  WRK-Z79             TO  SC04-SYOTOTALTEN (2)
                 END-IF
      *          標準負担額（地方公費２つ目）
                 IF  MF100-SHOKUJIFTN-G  NOT =  MF100-T-SHOKUJIFTN
                   MOVE  MF100-T-SHOKUJIFTN      TO  WRK-Z89
                   MOVE  WRK-Z89             TO  SC04-SYOFTNMONEY (2)
                 END-IF
      *
      *          負担者番号２(地方公費１つ目)
                 MOVE    MF100-FTNJANUM      TO  SC04-KOHFTNJANUM2
      *
      *          受給者番号２(地方公費１つ目)
                 MOVE    MF100-JKYSNUM       TO  SC04-KOHJKYSNUM2
      *
      *          点数３(地方公費１つ目)
                 MOVE    MF100-TOTALTEN      TO  WRK-Z7
                 MOVE    WRK-Z7              TO  SC04-TOTALTEN(3)
      *
      *          一部負担金３(地方公費１つ目)
                 IF  MF100-OKIKAE-FTN(2)  NOT =  ZERO
                   MOVE    MF100-OKIKAE-FTN(2)   TO  WRK-Z8
                   MOVE    WRK-Z8                TO  SC04-FTNMONEY(3)
                 ELSE
                   MOVE    MF100-FTNMONEY        TO  WRK-Z8
                   MOVE    WRK-Z8                TO  SC04-FTNMONEY(3)
                 END-IF
      *
      *          実日数３(地方公費１つ目)
                 MOVE    MF100-JNISSU        TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC04-JINISSU2
      *
      *          食事日数（地方公費１つ目）
                 IF  MF100-T-SHOKUJINISSU  NOT =  MF100-SHOKUJINISSU
                   MOVE  MF100-SHOKUJINISSU  TO  WRK-Z29
                   MOVE  WRK-Z29             TO  SC04-SYONISSU (3)
                 END-IF
      *          食事療養費（地方公費１つ目）
                 IF  MF100-T-SHOKUJIRYOYOHI  NOT =  MF100-SHOKUJIRYOYOHI
                   MOVE  MF100-SHOKUJIRYOYOHI    TO  WRK-Z79
                   MOVE  WRK-Z79             TO  SC04-SYOTOTALTEN (3)
                 END-IF
      *          標準負担額（地方公費１つ目）
                 IF  MF100-T-SHOKUJIFTN  NOT =  MF100-SHOKUJIFTN
                   MOVE  MF100-SHOKUJIFTN    TO  WRK-Z89
                   MOVE  WRK-Z89             TO  SC04-SYOFTNMONEY (3)
                 END-IF
               END-IF
           END-IF
      *
      *    低所得
           COMPUTE  WRK-CHK-SKJ  =  MF100-SHOKUJIFTN-G
                                 /  MF100-SHOKUJINISSU-G
           EVALUATE  TRUE
             WHEN  WRK-CHK-SKJ  <=  100
               MOVE  "低所得１"       TO  SC04-GENGAKUMEI
             WHEN  WRK-CHK-SKJ  <=  210
               MOVE  "低所得２"       TO  SC04-GENGAKUMEI
           END-EVALUATE
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "SC4504.red"         TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                    TO  SPRT-TITLE
           MOVE    SC04             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    MF100-PTID          TO  SPRT-PTID
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       331-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR  TO  RECEERR-REC
             WRITE RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦・和暦変換処理
      *****************************************************************
       31011-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           .
       31011-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU4503 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU4503 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU4503 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >     4  OR
                    SYU-KOH-HKNNUM(IDX4)  =  SPACE
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
             IF   SYU-KOH-HKNNUM(IDX4)     =  WRK-KOHNUM
               IF     IDX4                 =  1
                 IF  SYU-SYUCOMPFTN-ENTANI
                                       > SYU-KOH-COMPFTN-ENTANI(IDX4)
                   MOVE  ZERO          TO  FLG-ERR
                 END-IF
               ELSE
                 IF  SYU-KOH-COMPFTN-ENTANI(IDX4 - 1)
                                      >  SYU-KOH-COMPFTN-ENTANI(IDX4)
                   MOVE  ZERO          TO  FLG-ERR
                 END-IF
               END-IF
             END-IF
           END-PERFORM
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10)
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "tbl_syunou"   TO  MCP-TABLE
           MOVE  WRK-PATH       TO  MCP-PATHNAME
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM            TO  COMB-HOSPNUM
           MOVE    SYU-PTID               TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM        TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_hkncombi"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                 HKNCOMBI-REC
               MOVE    1              TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"              TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)          TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)          TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)          TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)          TO  TBL-HKOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  200112-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
      *            個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO                TO  FLG-ERR
                           MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                         END-IF
                     END-PERFORM
                   END-IF
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタクローズ
      *****************************************************************
       900-KOHSKY-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY             TO  RECE10-KEY
           MOVE    RECE10-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_seikyu"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE    "tbl_seikyu"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE10
                   INITIALIZE                  RECE10-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
               INITIALIZE                  RECE10-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
                       WRK-FTNJANUM
                       WRK-JKYSNUM
                       WRK-TEKSTYMD
      *
           MOVE    SPA-HOSPNUM             TO  PTKOH-HOSPNUM
           MOVE    WRK-PTID                TO  PTKOH-PTID
           MOVE    WRK-PTKOHNUM            TO  PTKOH-KOHNUM
           MOVE    WRK-SRYYM               TO  PTKOH-TEKSTYMD(1:6)
                                               PTKOH-TEKEDYMD(1:6)
           MOVE    "01"                    TO  PTKOH-TEKSTYMD(7:2)
           MOVE    "31"                    TO  PTKOH-TEKSTYMD(7:2)
      *
           MOVE    PTKOHINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
           MOVE    "key6"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
               MOVE    "key6"                TO  MCP-PATHNAME
               PERFORM 935-PTKOHINF-READ-SEC
               MOVE    PTKOH-FTNJANUM  TO  WRK-FTNJANUM
               MOVE    PTKOH-JKYSNUM   TO  WRK-JKYSNUM
               MOVE    PTKOH-TEKSTYMD  TO  WRK-TEKSTYMD
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
           MOVE    "key6"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       930-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"   TO  MCP-FUNC
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC  =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE  PTKOHINF-REC
               MOVE  1  TO  FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報セット
      *****************************************************************
       940-SRYACT-SET-SEC  SECTION.
      *
           INITIALIZE                      SRYACT-REC
      *
           MOVE  SPA-HOSPNUM           TO  SRY-HOSPNUM
           MOVE  RECE02-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE  RECE02-PTID           TO  SRY-PTID
           MOVE  RECE02-SRYYM          TO  SRY-SRYYM
           MOVE  WRK-SRYCD             TO  SRY-SRYCD(1)
                                           SRY-SRYCD(2)
                                           SRY-SRYCD(3)
                                           SRY-SRYCD(4)
                                           SRY-SRYCD(5)
           MOVE  SRYACT-REC            TO  MCPDATA-REC
           MOVE  "DBSELECT"            TO  MCP-FUNC
           MOVE  "tbl_sryact"          TO  MCP-TABLE
           MOVE  "key7"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  "tbl_sryact"        TO  MCP-TABLE
             MOVE  "key7"              TO  MCP-PATHNAME
             PERFORM  945-SRYACT-READ-SEC
           ELSE
             DISPLAY  "SRYACT SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE  "tbl_sryact"          TO  MCP-TABLE
           MOVE  "key7"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       940-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報読込
      *****************************************************************
       945-SRYACT-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SRYACT-REC
             MOVE  ZERO         TO  FLG-SRYACT
           ELSE
             INITIALIZE   SRYACT-REC
             MOVE  1  TO  FLG-SRYACT
           END-IF
      *
           .
       945-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント情報セット
      *****************************************************************
       950-PTCOM-READ-SEC  SECTION.
      *
             INITIALIZE                      PTCOM-REC
      *
             MOVE  SPA-HOSPNUM           TO  PTCOM-HOSPNUM
             MOVE  SRY-PTID              TO  PTCOM-PTID
             MOVE  SRY-ZAINUM            TO  PTCOM-ZAINUM
             MOVE  SRY-SRYCD(IDX4)       TO  PTCOM-SRYCD
             MOVE  SRY-INPUTNUM(IDX4)    TO  PTCOM-RENNUM
             MOVE  PTCOM-REC             TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_ptcom"           TO  MCP-TABLE
             MOVE  "key"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_ptcom"         TO  MCP-TABLE
               MOVE  "key"               TO  MCP-PATHNAME
               MOVE  "DBFETCH"           TO  MCP-FUNC
               CALL  "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF  MCP-RC  =  ZERO
                 MOVE  MCPDATA-REC       TO  PTCOM-REC
                 MOVE  ZERO              TO  FLG-PTCOM
               ELSE
                 INITIALIZE                  PTCOM-REC
                 MOVE    1               TO  FLG-PTCOM
               END-IF
             ELSE
               DISPLAY  "PTCOM SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_ptcom"           TO  MCP-TABLE
             MOVE  "key"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC  =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE  "tbl_syskanri"        TO  MCP-TABLE
               MOVE  "key10"               TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC  =   ZERO
                   MOVE    ZERO    TO  FLG-SYSKANRI
               ELSE
                   MOVE    1       TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1       TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key10"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(レセプト出力順情報取得)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-2003-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key2"                   TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
          IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(レセプト出力順情報取得)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key2"                   TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-2003-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key2"                   TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE                   TO  SYS-1900-REC
           INITIALIZE                          SYS-1900-REC
           MOVE    "1900"                  TO  SYS-1900-KANRICD
           MOVE    "*"                     TO  SYS-1900-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-1900-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1901-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1902-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
