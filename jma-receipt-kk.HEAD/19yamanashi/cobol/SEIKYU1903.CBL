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
       PROGRAM-ID.     SEIKYU1903.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 山梨・複写式レセプト
      *                      (143)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/06/04    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    14/08/13    摘要欄へのコメントの記載は
      *                                  自動で行うように修正
      *  01.00.02    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.03    吉川    15/02/13    平成２７年１月改正対応
      *  01.00.04    門間    16/05/12    医療機関編集情報への開設者名追加対応
      *                                  特記事項欄に「37申出」を追加
      *  01.00.05    門間    17/07/11    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.06    吉川    17/10/16    SITEKBN変更
      *  01.00.07    萩野    18/05/08    特記事項に印字内容追加
      *  01.00.08    門間    18/06/01    一部負担金金額置換対応
      *  01.00.09    井上    18/11/12    修正履歴コメント削除
      *  01.00.10    井上    19/04/09    元号対応
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF101-FILE  ASSIGN    MF101PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF101-KEY
                               FILE    STATUS  IS  STS-MF101.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF101-FILE.
       01  MF101-REC.
           COPY  "SEI1903.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF101//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
           COPY  "SC1903.INC".
      *
      *    対象公費領域
       01  TAISYO-DAT.
           03  TAISYO-TBL  OCCURS  100.
               05  TAISYO-KOHNUM     PIC X(03).
               05  TAISYO-KAIGYO     PIC X(01).
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF101             PIC X(02).
           03  STS-RECE02            PIC X(02).
           03  STS-RECEERR           PIC X(02).
           03  STS-RECE              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF             PIC 9(01).
           03  FLG-END               PIC 9(01).
           03  FLG-MF101             PIC 9(01).
           03  FLG-PRINT             PIC 9(01).
           03  FLG-SYSKANRI          PIC 9(01).
           03  FLG-SYSKANRI3         PIC 9(01).
           03  FLG-ERR               PIC 9(01).
           03  FLG-RECE10            PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-PTKOHINF          PIC 9(01).
           03  FLG-SRYACT            PIC 9(01).
           03  FLG-PTCOM             PIC 9(01).
           03  FLG-PRGOPTION         PIC 9(01).
           03  FLG-OK                PIC 9(01).
           03  FLG-TAISYOKOHI        PIC 9(01).
           03  FLG-TAISYO            PIC 9(01).
           03  FLG-BTPARA            PIC 9(01).
           03  FLG-SYORI             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(05).
           03  IDXX                  PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDX4                  PIC 9(04).
           03  IDY                   PIC 9(04).
           03  IDZ                   PIC 9(04).
           03  IDW1                  PIC 9(04).
           03  IDW2                  PIC 9(04).
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
           03  WRK-Z1                PIC Z.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z79               PIC ZZZ,ZZ9.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
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
           03  WRK-HKNJANUM          PIC X(06).
           03  WRK-SRYCD             PIC X(09).
           03  WRK-GENMEN            PIC X(10).
           03  WRK-TEISHO            PIC X(20).
           03  WRK-TEKIYO            PIC X(52).
           03  WRK-TEKIYOCD          PIC X(09).
           03  WRK-INPUTCOMENT       PIC X(80).
           03  WRK-INPUTCOMENT2      PIC X(14).
           03  WRK-PRTID             PIC X(20).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOBETUCITYNUM     PIC X(03).
           03  WRK-FTNMONEY-G        PIC 9(08).
           03  WRK-FTNJANUM          PIC X(20).
           03  WRK-JKYSNUM           PIC X(20).
           03  WRK-ETC-FTNJANUM      PIC X(20).
           03  WRK-ETC-JKYSNUM       PIC X(20).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-INPUTCOMENT-KIN-T.
               05  WRK-INPUTCOMENT-KIN     PIC 9(07).
           03  WRK-INPUTCOMENT-KIN2-T.
               05  WRK-INPUTCOMENT-KIN2    PIC X(07).
      *
           03  WRK-PATH              PIC  X(50).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *    プログラムオプション用
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
      *    その他公費情報領域
       01  WRK-ETC-TBL.
           03  WRK-ETC-KOHINFO OCCURS 3.
               05  WRK-ETC-KOHNUM    PIC X(03).
               05  WRK-ETC-KOHID     PIC 9(10).
               05  WRK-ETC-TENSU     PIC 9(07).
               05  WRK-ETC-ITBFTN    PIC 9(07).
               05  WRK-ETC-S-NISSU   PIC 9(02).
               05  WRK-ETC-S-KINGK   PIC 9(08).
               05  WRK-ETC-S-FTNGK   PIC 9(08).
      *
      *    その他公費情報一時保存領域
           03  WRK-HOZON-KOHINFO.
               05  WRK-HOZON-KOHNUM  PIC X(03).
               05  WRK-HOZON-KOHID   PIC 9(10).
               05  WRK-HOZON-TENSU   PIC 9(07).
               05  WRK-HOZON-ITBFTN  PIC 9(07).
               05  WRK-HOZON-S-NISSU PIC 9(02).
               05  WRK-HOZON-S-KINGK PIC 9(08).
               05  WRK-HOZON-S-FTNGK PIC 9(08).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF101   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM           PIC X(03)  OCCURS  4.
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    特記事項
       01  TBL-TOKKI-AREA.
           03  FILLER          PIC X(02)   VALUE   "01".
           03  FILLER          PIC X(10)   VALUE   "０１公".
           03  FILLER          PIC X(02)   VALUE   "02".
           03  FILLER          PIC X(10)   VALUE   "０２長".
           03  FILLER          PIC X(02)   VALUE   "03".
           03  FILLER          PIC X(10)   VALUE   "０３長処".
           03  FILLER          PIC X(02)   VALUE   "04".
           03  FILLER          PIC X(10)   VALUE   "０４後保".
           03  FILLER          PIC X(02)   VALUE   "07".
           03  FILLER          PIC X(10)   VALUE   "０７老併".
           03  FILLER          PIC X(02)   VALUE   "08".
           03  FILLER          PIC X(10)   VALUE   "０８老健".
           03  FILLER          PIC X(02)   VALUE   "09".
           03  FILLER          PIC X(10)   VALUE   "０９施".
           03  FILLER          PIC X(02)   VALUE   "10".
           03  FILLER          PIC X(10)   VALUE   "１０第三".
           03  FILLER          PIC X(02)   VALUE   "11".
           03  FILLER          PIC X(10)   VALUE   "１１薬治".
           03  FILLER          PIC X(02)   VALUE   "12".
           03  FILLER          PIC X(10)   VALUE   "１２器治".
           03  FILLER          PIC X(02)   VALUE   "13".
           03  FILLER          PIC X(10)   VALUE   "１３先進".
           03  FILLER          PIC X(02)   VALUE   "14".
           03  FILLER          PIC X(10)   VALUE   "１４制超".
           03  FILLER          PIC X(02)   VALUE   "15".
           03  FILLER          PIC X(10)   VALUE   "１５経過".
           03  FILLER          PIC X(02)   VALUE   "16".
           03  FILLER          PIC X(10)   VALUE   "１６長２".
           03  FILLER          PIC X(02)   VALUE   "17".
           03  FILLER          PIC X(10)   VALUE   "１７上位".
           03  FILLER          PIC X(02)   VALUE   "18".
           03  FILLER          PIC X(10)   VALUE   "１８一般".
           03  FILLER          PIC X(02)   VALUE   "19".
           03  FILLER          PIC X(10)   VALUE   "１９低所".
           03  FILLER          PIC X(02)   VALUE   "20".
           03  FILLER          PIC X(10)   VALUE   "２０二割".
           03  FILLER          PIC X(02)   VALUE   "21".
           03  FILLER          PIC X(10)   VALUE   "２１高半".
           03  FILLER          PIC X(02)   VALUE   "22".
           03  FILLER          PIC X(10)   VALUE   "２２多上".
           03  FILLER          PIC X(02)   VALUE   "23".
           03  FILLER          PIC X(10)   VALUE   "２３多一".
           03  FILLER          PIC X(02)   VALUE   "24".
           03  FILLER          PIC X(10)   VALUE   "２４多低".
           03  FILLER          PIC X(02)   VALUE   "25".
           03  FILLER          PIC X(10)   VALUE   "２５出産".
           03  FILLER          PIC X(02)   VALUE   "26".
           03  FILLER          PIC X(10)   VALUE   "２６区ア".
           03  FILLER          PIC X(02)   VALUE   "27".
           03  FILLER          PIC X(10)   VALUE   "２７区イ".
           03  FILLER          PIC X(02)   VALUE   "28".
           03  FILLER          PIC X(10)   VALUE   "２８区ウ".
           03  FILLER          PIC X(02)   VALUE   "29".
           03  FILLER          PIC X(10)   VALUE   "２９区エ".
           03  FILLER          PIC X(02)   VALUE   "30".
           03  FILLER          PIC X(10)   VALUE   "３０区オ".
           03  FILLER          PIC X(02)   VALUE   "31".
           03  FILLER          PIC X(10)   VALUE   "３１多ア".
           03  FILLER          PIC X(02)   VALUE   "32".
           03  FILLER          PIC X(10)   VALUE   "３２多イ".
           03  FILLER          PIC X(02)   VALUE   "33".
           03  FILLER          PIC X(10)   VALUE   "３３多ウ".
           03  FILLER          PIC X(02)   VALUE   "34".
           03  FILLER          PIC X(10)   VALUE   "３４多エ".
           03  FILLER          PIC X(02)   VALUE   "35".
           03  FILLER          PIC X(10)   VALUE   "３５多オ".
           03  FILLER          PIC X(02)   VALUE   "36".
           03  FILLER          PIC X(10)   VALUE   "３６加治".
           03  FILLER          PIC X(02)   VALUE   "37".
           03  FILLER          PIC X(10)   VALUE   "３７申出".
           03  FILLER          PIC X(02)   VALUE   "38".
           03  FILLER          PIC X(10)   VALUE   "３８医併".
           03  FILLER          PIC X(02)   VALUE   "39".
           03  FILLER          PIC X(10)   VALUE   "３９医療".
      *
       01  TBL-TOKKI-R         REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   37.
               05  TBL-TOKKICD             PIC X(02).
               05  TBL-TOKKINAME           PIC X(10).
       01  TBL-TOKKI-MAX                   PIC 9(02)   VALUE   37.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *    山梨コメント
           03  WRK-CONS-COMMENT PIC X(16) VALUE  "●山梨県自動還付".
      *    保険欄一部負担金置換コード
           03  WRK-CONS-KIN-OKIKAE-HKN  PIC X(09)  VALUE   "099999910".
      *    公費一部負担金置換コード
           03  WRK-CONS-KIN-OKIKAE      PIC X(09)  VALUE   "099999921".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
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
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
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
           MOVE    "SEIKYU1903"    TO  JOB-PGID
           MOVE    "複写式レセプト" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO  WRK-LASTYMD
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
           MOVE  WRK-PARA-HOSPNUM     TO  MF101PARA-HOSPNUM
           MOVE  "MF101ZZ"            TO  MF101PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF101PARA-TERMID
      *
           MOVE  MF101PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF101PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"           TO  SYS-1001-KANRICD
           MOVE  "*"              TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"             TO  SYS-1001-STYUKYMD(7:2)
                                      SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM      TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC     TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI        =  ZERO
             MOVE  MCPDATA-REC    TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1903 SYS 1001 ERR ***"
             MOVE  1              TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1903 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
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
           MOVE    "SEIKYU1903"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
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
                         OR   (IDX1    >  100)
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
           OPEN     OUTPUT  MF101-FILE
           CLOSE            MF101-FILE
           OPEN     I-O     MF101-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF101-FILE
      *
           IF  CNT-RECE02   >       0
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
      *
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
               INITIALIZE                          TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(IDZ)      TO  TBL-HKOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                   PERFORM 20011-SYUKEI-SEC
               END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF101-REC
      *
           MOVE    SPA-HOSPNUM                 TO  MF101-HOSPNUM
           MOVE    RECE02-PTNUM                TO  MF101-PTNUM
           MOVE    RECE02-RECEKA               TO  MF101-SRYKA
           MOVE    RECE02-HKNJANUM-KEY         TO  MF101-HKNJANUM-KEY
           MOVE    RECE02-TEKSTYMD             TO  MF101-TEKSTYMD
           MOVE    RECE02-NYUGAIKBN            TO  MF101-NYUGAIKBN
           MOVE    RECE02-SRYYM                TO  MF101-SRYYM
           MOVE    RECE02-TEISYUTUSAKI         TO  MF101-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU          TO  MF101-RECESYUBETU
           MOVE    RECE02-HOJOKBN-KEY          TO  MF101-HOJOKBN-KEY
      *
           MOVE    RECE02-PTID                 TO  MF101-PTID
           MOVE    RECE02-NAME                 TO  MF101-NAME
           MOVE    RECE02-KANANAME             TO  MF101-KANANAME
           MOVE    RECE02-BIRTHDAY             TO  MF101-BIRTHDAY
           MOVE    RECE02-SEX                  TO  MF101-SEX
           MOVE    RECE02-AGE                  TO  MF101-AGE
           MOVE    RECE02-HKN                  TO  MF101-HKN
           MOVE    RECE02-RJNHKN               TO  MF101-RJNHKN
           MOVE    RECE02-ZAITAKU              TO  MF101-ZAI
      *
           MOVE    RECE02-JNISSU(1)            TO  MF101-JNISSU
           MOVE    RECE02-TOTALTEN(1)          TO  MF101-TOTALTEN
           MOVE    RECE02-SHOKUJIINF(1)        TO  MF101-SHOKUJIINF
      *
           MOVE    RECE02-KOHFTNJANUM (IDZ)    TO  MF101-FTNJANUM
           MOVE    RECE02-KOHJKYSNUM  (IDZ)    TO  MF101-JKYSNUM
           MOVE    MF101-FTNJANUM(3:6)         TO  MF101-CITYNUM
           MOVE    RECE02-KOHINF      (IDZ)    TO  MF101-KOHINF
      *
      *    個別発行用市町村番号
           MOVE    MF101-KOHFTNJANUM(5:3)      TO  WRK-KOBETUCITYNUM
      *
      *    請求管理情報より、他公費情報取得
           IF       IDZ           =     1
           AND      RECE02-KOHNUM(IDZ + 1)  =  SPACE
             PERFORM  900-SEIKYU-READ-SEC
      *      レセプトのデータを記録
             MOVE  RECE10-JNISSU(1)            TO  MF101-JNISSU
             MOVE  RECE10-TOTALTEN(1)          TO  MF101-TOTALTEN
             MOVE  RECE10-SHOKUJIINF(1)        TO  MF101-SHOKUJIINF
      *
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     3  OR
                      RECE10-KOHNUM(IDX)  =  SPACE
               IF  RECE10-KOHNUM(IDX)  NOT =  "012"
      *          公費番号
                 MOVE    RECE10-KOHNUM(IDX)  TO  MF101-E-KOHNUM(IDX)
                                                 WRK-KOHNUM
      *          実日数
                 MOVE    RECE10-JNISSU(IDX + 1) 
                                             TO  MF101-E-JNISSU(IDX)
      *          点数
                 MOVE    RECE10-TOTALTEN(IDX + 1)
                                             TO  MF101-E-TOTALTEN(IDX)
      *          一部負担金
                 MOVE    RECE10-FTNMONEY(IDX + 1)
                                             TO  MF101-E-FTNMONEY(IDX)
      *          食事日数
                 MOVE    RECE10-SHOKUJINISSU(IDX + 1)
                                     TO  MF101-E-SHOKUJINISSU(IDX)
      *          食事療養日
                 MOVE    RECE10-SHOKUJIRYOYOHI(IDX + 1)
                                     TO  MF101-E-SHOKUJIRYOYOHI(IDX)
      *          食事標準負担額
                 MOVE    RECE10-SHOKUJIFTN(IDX + 1)
                                     TO  MF101-E-SHOKUJIFTN(IDX)
      *          点数と日数は保険と同じ値の時は記載を省略する
                 IF  MF101-E-JNISSU(IDX)  =  MF101-JNISSU
                     MOVE    ZERO            TO  MF101-E-JNISSU(IDX)
                 END-IF
                 IF  MF101-E-TOTALTEN(IDX)  =  MF101-TOTALTEN
                     MOVE    ZERO            TO  MF101-E-TOTALTEN(IDX)
                 END-IF
      *
      *          負担者番号、受給者番号取得
                 MOVE    RECE10-KOHID(IDX)          TO  WRK-KOHID
                 PERFORM 930-PTKOHINF-SET-SEC
                 MOVE    WRK-ETC-FTNJANUM   TO  MF101-E-KOHFTNJANUM(IDX)
                 MOVE    WRK-ETC-JKYSNUM    TO  MF101-E-KOHJKYSNUM(IDX)
               END-IF
             END-PERFORM
      *
      *      特記事項
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     3
                 IF   RECE10-TOKKI(IDX)  NOT =  SPACE
                   MOVE  RECE10-TOKKI(IDX)  TO  MF101-TOKKI(IDX)
                 END-IF
             END-PERFORM
           END-IF
      *
      *    長期併用判定と月途中で公費を切替えた時の全国公費分のデータ集計
           MOVE     MF101-KOHNUM           TO  WRK-KOHNUM
           MOVE     MF101-KOHID            TO  WRK-KOHID
           MOVE     ZERO                   TO  WRK-FTNMONEY-G
           INITIALIZE                          WRK-ETC-TBL
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
      *
      *    月途中で公費を切替えていない場合請求管理情報より他公費情報取得
           IF  IDZ           =     1   AND
               RECE02-KOHNUM(IDZ + 1)  =  SPACE
               DISPLAY "請求管理から公費情報取得済み" RECE02-PTNUM
           ELSE
      *        月途中で公費を切替えている場合は収納テーブルから
      *        他公費情報取得(この場合日数は表示しない)
               DISPLAY "収納情報から公費情報取得 " RECE02-PTNUM
               DISPLAY  "患者番号=" RECE02-PTNUM
                        "その他公費=" WRK-ETC-KOHNUM(1)
               IF  WRK-ETC-KOHNUM(1)     NOT  =  SPACE
                 DISPLAY "その他公費番号=" WRK-ETC-KOHNUM(1)
                         ","               WRK-ETC-KOHNUM(2)
                         ","               WRK-ETC-KOHNUM(3)
      *          公費番号順にソート
                 PERFORM  VARYING  IDX  FROM  1  BY  1
                          UNTIL    IDX  >  2
                          OR       WRK-ETC-KOHNUM(IDX)  =  SPACE
                   COMPUTE  IDX1  =  IDX  +  1
                   PERFORM  VARYING  IDY  FROM  IDX1  BY  1
                            UNTIL    IDY  >  3
                            OR       WRK-ETC-KOHNUM(IDY)  =  SPACE
                     IF  WRK-ETC-KOHNUM(IDX)  >  WRK-ETC-KOHNUM(IDY)
                        MOVE    WRK-ETC-KOHINFO(IDX) 
                                   TO  WRK-HOZON-KOHINFO
                        MOVE    WRK-ETC-KOHINFO(IDY) 
                                   TO  WRK-ETC-KOHINFO(IDX)
                        MOVE    WRK-HOZON-KOHINFO   
                                   TO  WRK-ETC-KOHINFO(IDY)
                     END-IF
                   END-PERFORM
                 END-PERFORM
      *
                 PERFORM  VARYING  IDX  FROM  1  BY  1
                          UNTIL    IDX  >  3
                          OR       WRK-ETC-KOHNUM(IDX)  =  SPACE
                   MOVE    WRK-ETC-KOHNUM(IDX)    TO  WRK-KOHNUM
                   PERFORM 930-PTKOHINF-SET-SEC
                   MOVE    WRK-ETC-TENSU(IDX)   
                                   TO  MF101-E-TOTALTEN(IDX)
                   MOVE    WRK-ETC-ITBFTN(IDX)   
                                   TO  MF101-E-FTNMONEY(IDX)
                   MOVE    WRK-ETC-FTNJANUM   
                                   TO  MF101-E-KOHFTNJANUM(IDX)
                   MOVE    WRK-ETC-JKYSNUM    
                                   TO  MF101-E-KOHJKYSNUM(IDX)
                   MOVE    WRK-ETC-S-NISSU(IDX)
                                   TO  MF101-E-SHOKUJINISSU(IDX)
                   MOVE    WRK-ETC-S-KINGK(IDX)
                                   TO  MF101-E-SHOKUJIRYOYOHI(IDX)
                   MOVE    WRK-ETC-S-FTNGK(IDX)
                                   TO  MF101-E-SHOKUJIFTN(IDX)
                 END-PERFORM
               END-IF
           END-IF
      *
           COMPUTE IDY     =   IDZ +   1
      *    一部負担金
           IF  RECE02-FTNMONEY(1)          >   ZERO  AND
               RECE02-FTNMONEY(1)          <   WRK-FTNMONEY-G
             MOVE  RECE02-FTNMONEY(1)      TO  WRK-FTNMONEY-G
           END-IF
           MOVE  WRK-FTNMONEY-G            TO  MF101-FTNMONEY
      *
      *    フラグ（未就学児、特例措置）
           MOVE    RECE02-FLG (1)          TO  MF101-FLG (1)
           MOVE    RECE02-FLG (2)          TO  MF101-FLG (2)
      *
      *    山梨コメント作成
           INITIALIZE                          WRK-MAE-INPUT
                                               WRK-OUT-INPUT
           MOVE    MF101-FTNJANUM          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-FTNJANUM
      *
           MOVE    MF101-JKYSNUM           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-JKYSNUM
      *
           STRING  WRK-CONS-COMMENT        DELIMITED BY SIZE
                   WRK-FTNJANUM            DELIMITED BY SPACE
                   WRK-JKYSNUM             DELIMITED BY SPACE
                   INTO                    MF101-TEKIYO
           END-STRING
      *
      *    減免区分チェック
           PERFORM 200113-GENMEN-CHK-SEC
      *
      *    低所得・低年金チェック（１：低所、２：低所３月超、３：低年金）
           IF  RECE02-NYUGAIKBN  =  "1"
             MOVE    RECE02-SHOKUJIFTNKBN      TO  MF101-TEISHO
           ELSE
             IF  RECE02-FTNMONEY(1)  >  ZERO
               MOVE    RECE02-SHOKUJIFTNKBN    TO  MF101-TEISHO
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
      *
      *    診療行為情報取得
           MOVE    WRK-CONS-KIN-OKIKAE       TO  WRK-SRYCD
           PERFORM 940-SRYACT-SET-SEC
           IF  FLG-SRYACT  =  ZERO
      *      公費−一部負担金置換編集
             PERFORM  200112-ICHIBUFTN-HENSYU-SEC
           END-IF
      *
      *    保険者番号チェック処理
           MOVE    ZERO                    TO  FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
      *    第２公費以降が第１公費と同じ場合、集計対象外
           IF  IDZ  <  4
               IF  MF101-KOHNUM  =  RECE02-KOHNUM(IDZ + 1)
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
      *
      *    患者負担がない場合は印字対象外
           IF  (FLG-ERR    =    ZERO)
           AND (RECE02-FTNMONEY(IDY)  NOT =  ZERO)
               WRITE  MF101-REC
               ADD    1                    TO  CNT-MF101
           ELSE
               DISPLAY "助成対象外患者番号="  RECE02-PTNUM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "19"                     TO  WRK-HKNJANUM(1:2)
           MOVE    WRK-KOBETUCITYNUM(1:3)   TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF101-PTNUM
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
                                    TO  MF101-OKIKAE-FTN(1)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公２負担金："
                       MOVE  WRK-INPUTCOMENT-KIN
                                    TO  MF101-OKIKAE-FTN(2)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公３負担金："
                       MOVE  WRK-INPUTCOMENT-KIN 
                                    TO  MF101-OKIKAE-FTN(3)
                     END-IF
                     IF  WRK-INPUTCOMENT(1:12)  =  "公４負担金："
                       MOVE  WRK-INPUTCOMENT-KIN 
                                    TO  MF101-OKIKAE-FTN(4)
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
      *    摘要欄編集処理
      *****************************************************************
       200112-TEKIYO-HENSYU-SEC                SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >  5
             IF  (SRY-SRYCD(IDX4)  =  WRK-TEKIYOCD)
             AND (SRY-INPUTNUM(IDX4)  NOT =  ZERO )
      *        患者コメント情報読込
               PERFORM 950-PTCOM-READ-SEC
               MOVE    PTCOM-INPUTCOMENT           TO  MF101-TEKIYO
             END-IF
           END-PERFORM
      *
           .
       200112-TEKIYO-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    減免区分チェック処理
      *****************************************************************
       200113-GENMEN-CHK-SEC                SECTION.
      *
           INITIALIZE                      PTKOHINF-REC
                                           FLG-PTKOHINF
           MOVE    SPA-HOSPNUM             TO  PTKOH-HOSPNUM
           MOVE    RECE02-PTID             TO  PTKOH-PTID
           MOVE    PTKOHINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      FLG-PTKOHINF        =   ZERO
               PERFORM  800-PTKOHINF-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-PTKOHINF  =  1
                 IF  RECE02-SRYYM  >=  PTKOH-TEKSTYMD(1:6)  AND
                     RECE02-SRYYM  <=  PTKOH-TEKEDYMD(1:6)
                   IF  PTKOH-KOHNUM  =  "960" OR "961" OR "962" OR "963"
                     MOVE    1             TO  FLG-PTKOHINF
                     EVALUATE  PTKOH-KOHNUM
                       WHEN  "960"
                       WHEN  "961"
      *                  減免
                         MOVE    1         TO  MF101-GENMEN
                       WHEN  "962"
      *                  免除
                         MOVE    2         TO  MF101-GENMEN
                       WHEN  "963"
      *                  猶予
                         MOVE    3         TO  MF101-GENMEN
                     END-EVALUATE
                   END-IF
                 END-IF
                 PERFORM  800-PTKOHINF-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       200113-GENMEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF101-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *       外来
               IF  MF101-NYUGAIKBN  =  "2"
      *        帳票編集<見出し>処理
                   PERFORM  310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                   PERFORM  320-BODY-HEN-SEC
      *        帳票印刷処理
                   PERFORM  330-PRINT-OUT-SEC
      *       入院
               ELSE
      *        帳票編集<見出し>処理
                   PERFORM  311-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                   PERFORM  321-BODY-HEN-SEC
      *        帳票印刷処理
                   PERFORM  331-PRINT-OUT-SEC
               END-IF
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    MF101-FILE
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
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC03
      *
      *    印刷日付
      *    診療年月（診療年月日として退避）
           MOVE    MF101-SRYYM         TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)     TO  SC03-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN      TO  SC03-HOSPCDN
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SC03-ADRS
           INSPECT SC03-ADRS           REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SC03-HOSPNAME
           INSPECT SC03-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE  WRK-KAISETUNAME       TO  SC03-KAISETUNAME
      *
      *    保険種別区分
           EVALUATE    MF101-RECESYUBETU(2:1)
               WHEN    "1"
                   MOVE   "◯"         TO  SC03-HKNKBN(1)
               WHEN    "2"
                   MOVE   "◯"         TO  SC03-HKNKBN(2)
               WHEN    "3"
                   MOVE   "◯"         TO  SC03-HKNKBN(3)
               WHEN    "4"
                   MOVE   "◯"         TO  SC03-HKNKBN(4)
           END-EVALUATE
      *
      *    併用区分
           IF  MF101-E-KOHFTNJANUM (2)  NOT =  SPACE
               MOVE  "○"              TO  SC03-HEIKBN(3)
           ELSE
             IF  MF101-E-KOHFTNJANUM (1)  NOT =  SPACE
                 MOVE  "○"            TO  SC03-HEIKBN(2)
             ELSE 
                 MOVE  "○"            TO  SC03-HEIKBN(1)
             END-IF
           END-IF
      *
      *    本人家族区分
           EVALUATE    MF101-RECESYUBETU(4:1)
               WHEN    "1"
               WHEN    "2"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (1)
               WHEN    "3"
               WHEN    "4"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (2)
               WHEN    "5"
               WHEN    "6"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (3)
               WHEN    "7"
               WHEN    "8"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (4)
               WHEN    "9"
               WHEN    "0"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (5)
           END-EVALUATE
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
      *    負担者番号
           IF  MF101-E-KOHFTNJANUM (1)  NOT =  SPACE
             MOVE     MF101-E-KOHFTNJANUM(1)   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-FTNJANUM(1)
           END-IF
           IF  MF101-E-KOHFTNJANUM (2)  NOT =  SPACE
             MOVE     MF101-E-KOHFTNJANUM(2)   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-FTNJANUM(2)
           END-IF
      *
      *    受給者番号
           IF  MF101-E-KOHJKYSNUM (1)  NOT =  SPACE
             MOVE     MF101-E-KOHJKYSNUM(1)    TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-JKYSNUM(1)
           END-IF
           IF  MF101-E-KOHJKYSNUM (2)  NOT =  SPACE
             MOVE     MF101-E-KOHJKYSNUM(2)    TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-JKYSNUM(2)
           END-IF
      *
      *    保険者番号
           MOVE  MF101-HKNJANUM            TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           EVALUATE  MF101-HKNNUM
               WHEN "001"
                   MOVE  WRK-OUT-INPUT(1:8)    TO  SC03-HKNJANUM(9:8)
               WHEN "060"
                   MOVE  WRK-OUT-INPUT(1:12)   TO  SC03-HKNJANUM(5:12)
               WHEN OTHER
                   MOVE  WRK-OUT-INPUT         TO  SC03-HKNJANUM
           END-EVALUATE
      *
      *    給付割合
           IF  MF101-FLG (2)  =  1
               MOVE    "○"                TO  SC03-KYURATE(3)
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN  10
                   MOVE    "○"            TO  SC03-KYURATE(2)
               WHEN  20
                   MOVE    "○"            TO  SC03-KYURATE(3)
               WHEN  30
                   MOVE    "○"            TO  SC03-KYURATE(4)
               WHEN  OTHER
                   MOVE    "○"            TO  SC03-KYURATE(1)
             END-EVALUATE
           END-IF
      *
      *    被保険者証記号・番号
           MOVE  MF101-KIGO                TO  SC03-KIGO
           MOVE  MF101-NUM                 TO  SC03-BANGO
      *
      *    患者番号
           MOVE  MF101-PTNUM               TO  SC03-PTNUM
      *
      *    氏名
           MOVE  MF101-NAME                TO  SC03-NAME
      *
      *    カナ氏名
           MOVE  MF101-KANANAME            TO  SC03-KANANAME
      *
      *    性別
           IF    MF101-SEX                 =   "1"
             MOVE  "○"                    TO  SC03-SEX(1)
           ELSE
             MOVE  "○"                    TO  SC03-SEX(2)
           END-IF
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY            TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  LNK-DAY2-EDTYMD3          TO  SC03-BIRTHDAY2
           
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC03-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
      *    診療実日数(保険)
           MOVE    MF101-JNISSU            TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-NISSU(1)
      *
      *    点数(保険)
           MOVE    MF101-TOTALTEN          TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-TENSU(1)
      *
           IF  MF101-OKIKAE-HKN  NOT =  ZERO
               MOVE    MF101-OKIKAE-HKN      TO  WRK-Z9
               MOVE    WRK-Z9                TO  SC03-KOHFTN(1)
           ELSE
               MOVE    MF101-FTNMONEY        TO  WRK-Z9
               MOVE    WRK-Z9                TO  SC03-KOHFTN(1)
           END-IF
      *
      *    診療実日数(公費１)
           MOVE    MF101-E-JNISSU(1)       TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-NISSU(2)
      *
      *    点数(公費１)
           MOVE    MF101-E-TOTALTEN(1)     TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-TENSU(2)
      *
      *    一部負担金(公費１)
           IF  MF101-OKIKAE-FTN(1)  NOT =  ZERO
             MOVE    MF101-OKIKAE-FTN(1)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(2)
           ELSE
             MOVE    MF101-E-FTNMONEY(1)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(2)
           END-IF
      *
      *    診療実日数(公費２)
           MOVE    MF101-E-JNISSU(2)       TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-NISSU(3)
      *
      *    点数(公費２)
           MOVE    MF101-E-TOTALTEN(2)     TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-TENSU(3)
      *
      *    一部負担金(公費２)
           IF  MF101-OKIKAE-FTN(2)  NOT =  ZERO
             MOVE    MF101-OKIKAE-FTN(2)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(3)
           ELSE
             MOVE    MF101-E-FTNMONEY(2)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(3)
           END-IF
      *
      *    摘要欄
           INITIALIZE                          WRK-TEKIYO
      *    減免区分
           EVALUATE  MF101-GENMEN
               WHEN  1
      *            減免
                   MOVE    "減免"          TO  WRK-GENMEN
               WHEN  2
      *            免除
                   MOVE    "免除"          TO  WRK-GENMEN
               WHEN  3
      *            猶予
                   MOVE    "猶予"          TO  WRK-GENMEN
               WHEN  OTHER
                   MOVE    SPACE           TO  WRK-GENMEN
           END-EVALUATE
      *
      *    低所得・低年金
           EVALUATE  MF101-TEISHO
               WHEN  1
      *            低所得
                   MOVE    "低所得２"      TO  WRK-TEISHO
               WHEN  2
      *            低所得３月超
                   MOVE    "低所得２（３月超）"
                                           TO  WRK-TEISHO
               WHEN  3
      *            低年金
                   MOVE    "低所得１"      TO  WRK-TEISHO
               WHEN  OTHER
                   MOVE    SPACE           TO  WRK-TEISHO
           END-EVALUATE
      *
           IF  WRK-GENMEN  NOT =  SPACE
               IF  WRK-TEISHO  NOT =  SPACE
                   STRING  WRK-GENMEN      DELIMITED BY SPACE
                           "、"            DELIMITED BY SIZE
                           WRK-TEISHO      DELIMITED BY SPACE
                           INTO            WRK-TEKIYO
                   END-STRING
               ELSE
                   MOVE    WRK-GENMEN      TO  WRK-TEKIYO
               END-IF
           ELSE
               MOVE    WRK-TEISHO          TO  WRK-TEKIYO
           END-IF
      *
      *    摘要へ印字
           IF  WRK-TEKIYO  NOT =  SPACE
               MOVE    WRK-TEKIYO          TO  SC03-TEKIYO(1)
               MOVE    ALL "―"            TO  SC03-TEKIYO(2)
           END-IF
      *
      *    第３公費があれば摘要欄へ
           IF  MF101-E-KOHNUM(3)  NOT =  SPACE
               IF  SC03-TEKIYO(1)  =  SPACE
                   MOVE    1               TO  IDX1
               ELSE
                   MOVE    3               TO  IDX1
               END-IF
               MOVE    "第３公費"          TO  SC03-TEKIYO(IDX1)
      *    負担者番号(公費３)
               STRING  "公３（"            DELIMITED BY SIZE
                       MF101-E-KOHFTNJANUM(3)
                                           DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
                       INTO                SC03-TEKIYO(IDX1 + 1)
               END-STRING
      *    受給者番号(公費３)
               STRING  "受　（"            DELIMITED BY SIZE
                       MF101-E-KOHJKYSNUM(3)
                                           DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
                       INTO                SC03-TEKIYO(IDX1 + 2)
               END-STRING
      *    診療実日数(公費３)
               MOVE    MF101-E-JNISSU(3)   TO  WRK-Z2
               STRING  "実　（"            DELIMITED BY SIZE
                       WRK-Z2              DELIMITED BY SIZE
                       "）"                DELIMITED BY SIZE
                       INTO                SC03-TEKIYO(IDX1 + 3)
               END-STRING
               MOVE    ALL "―"            TO  SC03-TEKIYO(IDX1 + 4)
           END-IF
      *
      *    山梨県自動還付コメント
           IF  SC03-TEKIYO(1)  =  SPACE
               MOVE    MF101-TEKIYO        TO  SC03-TEKIYO(1)
           ELSE
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >  35
                 IF  SC03-TEKIYO(IDX2)  =  SPACE
                     MOVE    MF101-TEKIYO  TO  SC03-TEKIYO(IDX2)
                     MOVE    35            TO  IDX2
                 END-IF
               END-PERFORM
           END-IF
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
           MOVE    "SC1903.red"        TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                       TO  SPRT-TITLE
           MOVE    SC03                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    MF101-PTID          TO  SPRT-PTID
           MOVE    "2"                 TO  SPRT-SITEKBN
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
      *    帳票編集<ヘッダー部>処理 −入院
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC03
      *
      *    印刷日付
      *    診療年月（診療年月日として退避）
           MOVE    MF101-SRYYM         TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)     TO  SC03-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN      TO  SC03-HOSPCDN
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SC03-ADRS
           INSPECT SC03-ADRS           REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SC03-HOSPNAME
           INSPECT SC03-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE  WRK-KAISETUNAME       TO  SC03-KAISETUNAME
      *
      *    保険種別区分
           EVALUATE    MF101-RECESYUBETU(2:1)
               WHEN    "1"
                   MOVE   "◯"         TO  SC03-HKNKBN(1)
               WHEN    "2"
                   MOVE   "◯"         TO  SC03-HKNKBN(2)
               WHEN    "3"
                   MOVE   "◯"         TO  SC03-HKNKBN(3)
               WHEN    "4"
                   MOVE   "◯"         TO  SC03-HKNKBN(4)
           END-EVALUATE
      *
      *    併用区分
           IF  MF101-E-KOHFTNJANUM (2)  NOT =  SPACE
               MOVE  "○"              TO  SC03-HEIKBN(3)
           ELSE
             IF  MF101-E-KOHFTNJANUM (1)  NOT =  SPACE
                 MOVE  "○"            TO  SC03-HEIKBN(2)
             ELSE 
                 MOVE  "○"            TO  SC03-HEIKBN(1)
             END-IF
           END-IF
      *
      *    本人家族区分
           EVALUATE    MF101-RECESYUBETU(4:1)
               WHEN    "1"
               WHEN    "2"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (1)
               WHEN    "3"
               WHEN    "4"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (2)
               WHEN    "5"
               WHEN    "6"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (3)
               WHEN    "7"
               WHEN    "8"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (4)
               WHEN    "9"
               WHEN    "0"
                   MOVE    "◯"        TO  SC03-HONKZKKBN (5)
           END-EVALUATE

      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理−入院
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    負担者番号
           IF  MF101-E-KOHFTNJANUM (1)  NOT =  SPACE
             MOVE     MF101-E-KOHFTNJANUM(1)   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-FTNJANUM(1)
           END-IF
           IF  MF101-E-KOHFTNJANUM (2)  NOT =  SPACE
             MOVE     MF101-E-KOHFTNJANUM(2)   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-FTNJANUM(2)
           END-IF
      *
      *    受給者番号
           IF  MF101-E-KOHJKYSNUM (1)  NOT =  SPACE
             MOVE     MF101-E-KOHJKYSNUM(1)    TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-JKYSNUM(1)
           END-IF
           IF  MF101-E-KOHJKYSNUM (2)  NOT =  SPACE
             MOVE     MF101-E-KOHJKYSNUM(2)    TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT            TO  SC03-JKYSNUM(2)
           END-IF
      *
      *    保険者番号
           MOVE  MF101-HKNJANUM            TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           EVALUATE  MF101-HKNNUM
               WHEN "001"
                   MOVE  WRK-OUT-INPUT(1:8)    TO  SC03-HKNJANUM(9:8)
               WHEN "060"
                   MOVE  WRK-OUT-INPUT(1:12)   TO  SC03-HKNJANUM(5:12)
               WHEN OTHER
                   MOVE  WRK-OUT-INPUT         TO  SC03-HKNJANUM
           END-EVALUATE
      *
      *    給付割合
           IF   MF101-FLG (2)  =  1
               MOVE    "○"                TO  SC03-KYURATE(3)
           ELSE
             EVALUATE  MF101-KYURATE
               WHEN  10
                   MOVE    "○"            TO  SC03-KYURATE(2)
               WHEN  20
                   MOVE    "○"            TO  SC03-KYURATE(3)
               WHEN  30
                   MOVE    "○"            TO  SC03-KYURATE(4)
               WHEN  OTHER
                   MOVE    "○"            TO  SC03-KYURATE(1)
             END-EVALUATE
           END-IF
      *
      *    被保険者証記号・番号
           MOVE  MF101-KIGO                TO  SC03-KIGO
           MOVE  MF101-NUM                 TO  SC03-BANGO
      *
      *    患者番号
           MOVE  MF101-PTNUM               TO  SC03-PTNUM
      *
      *    氏名
           MOVE  MF101-NAME                TO  SC03-NAME
      *
      *    カナ氏名
           MOVE  MF101-KANANAME            TO  SC03-KANANAME
      *    性別
           IF    MF101-SEX                 =   "1"
             MOVE  "○"                    TO  SC03-SEX(1)
           ELSE
             MOVE  "○"                    TO  SC03-SEX(2)
           END-IF
      *
      *    生年月日
           MOVE  MF101-BIRTHDAY            TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  LNK-DAY2-EDTYMD3          TO  SC03-BIRTHDAY2
           
      *
      *    特記事項
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL  IDY  >  TBL-TOKKI-MAX
               IF  MF101-TOKKI(IDX)  =  TBL-TOKKICD(IDY)
                   MOVE  TBL-TOKKINAME(IDY)   TO  SC03-TOKKI(IDX)
                   MOVE  TBL-TOKKI-MAX  TO  IDY
               END-IF
             END-PERFORM
           END-PERFORM
      *
      *    診療実日数(保険)
           MOVE    MF101-JNISSU            TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-NISSU(1)
      *
      *    点数(保険)
           MOVE    MF101-TOTALTEN          TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-TENSU(1)
      *
      *    一部負担金額(保険)
           IF  MF101-OKIKAE-HKN  NOT =  ZERO
              MOVE    MF101-OKIKAE-HKN     TO  WRK-Z9
              MOVE    WRK-Z9               TO  SC03-KOHFTN(1)
           ELSE
              MOVE    MF101-FTNMONEY       TO  WRK-Z9
              MOVE    WRK-Z9               TO  SC03-KOHFTN(1)
           END-IF
      *
      *    食事日数(保険)
           MOVE    MF101-SHOKUJINISSU      TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-SNISSU(1)
      *
      *    食事療養費(保険)
           MOVE    MF101-SHOKUJIRYOYOHI    TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-SKINGK(1)
      *
      *    標準負担額(保険)
           MOVE    MF101-SHOKUJIFTN        TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-SFTNGK(1)
      *
      *    診療実日数(公費１)
           MOVE    MF101-E-JNISSU(1)       TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-NISSU(2)
      *
      *    点数(公費１)
           MOVE    MF101-E-TOTALTEN(1)     TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-TENSU(2)
      *
      *    一部負担金(公費１)
           IF  MF101-OKIKAE-FTN(1)  NOT =  ZERO
             MOVE    MF101-OKIKAE-FTN(1)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(2)
           ELSE
             MOVE    MF101-E-FTNMONEY(1)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(2)
           END-IF
      *
      *    食事日数(公費１)
           MOVE    MF101-E-SHOKUJINISSU(1)
                                           TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-SNISSU(2)
      *
      *    食事療養費(公費１)
           MOVE    MF101-E-SHOKUJIRYOYOHI(1)
                                           TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-SKINGK(2)
      *
      *    標準負担額(公費１)
           MOVE    MF101-E-SHOKUJIFTN(1)   TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-SFTNGK(2)
      *
      *    診療実日数(公費２)
           MOVE    MF101-E-JNISSU(2)       TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-NISSU(3)
      *
      *    点数(公費２)
           MOVE    MF101-E-TOTALTEN(2)     TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-TENSU(3)
      *
      *    一部負担金(公費２)
           IF  MF101-OKIKAE-FTN(2)  NOT =  ZERO
             MOVE    MF101-OKIKAE-FTN(2)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(3)
           ELSE
             MOVE    MF101-E-FTNMONEY(2)     TO  WRK-Z9
             MOVE    WRK-Z9                  TO  SC03-KOHFTN(3)
           END-IF
      *
      *    食事日数(公費２)
           MOVE    MF101-E-SHOKUJINISSU(2)  
                                           TO  WRK-Z2
           MOVE    WRK-Z2                  TO  SC03-SNISSU(3)
      *
      *    食事療養費(公費２)
           MOVE    MF101-E-SHOKUJIRYOYOHI(2)
                                           TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-SKINGK(3)
      *
      *    標準負担額(公費２)
           MOVE    MF101-E-SHOKUJIFTN(2)   TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC03-SFTNGK(3)
      *
      *    減免区分
           EVALUATE  MF101-GENMEN
               WHEN  1
      *            減免
                   MOVE    "○"            TO  SC03-GENMEN(1)
               WHEN  2
      *            免除
                   MOVE    "○"            TO  SC03-GENMEN(2)
               WHEN  3
      *            猶予
                   MOVE    "○"            TO  SC03-GENMEN(3)
           END-EVALUATE
      *
      *    低所得・低年金
           EVALUATE  MF101-TEISHO
               WHEN  1
      *            低所得
                   MOVE    "○"            TO  SC03-TEISHO(2)
               WHEN  2
      *            低所得３月超
                   MOVE    "○"            TO  SC03-TEISHO(3)
               WHEN  3
      *            低年金
                   MOVE    "○"            TO  SC03-TEISHO(1)
           END-EVALUATE
      *
      *    山梨県自動還付コメント
           MOVE    MF101-TEKIYO            TO  SC03-TEKIYO(1)
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理−入院
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
           MOVE    "SC1904.red"        TO  SPRT-PRTID
           MOVE    "複写式レセプト"
                                       TO  SPRT-TITLE
           MOVE    SC03                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    MF101-PTID          TO  SPRT-PTID
           MOVE    "2"                 TO  SPRT-SITEKBN
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
      *
           CLOSE  MF101-FILE
           MOVE   CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1903 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1903 OUT "  CNT-MF101
           DISPLAY "*** SEIKYU1903 END ***"
      *
           IF  CNT-MF101  =  0
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
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
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
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF101-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
               PERFORM  800-TAISYOKOHI-CHK-SEC
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
      *    テスト患者のときは読み飛ばす
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
      *        個別発行で患者番号指定の場合、対象判定を行う
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
               MOVE     1               TO  FLG-END
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
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
      *    患者公費情報読み込み
      *****************************************************************
       800-PTKOHINF-FETCH-SEC  SECTION.
      *
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC   =   ZERO
               MOVE    ZERO                TO  FLG-PTKOHINF
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       800-PTKOHINF-FETCH-EXT.
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
           MOVE  "tbl_seikyu"                 TO  MCP-TABLE
           MOVE  "key"                        TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"                  USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE  "tbl_seikyu"                 TO  MCP-TABLE
               MOVE  "key"                        TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"                  USING
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
           MOVE  "tbl_seikyu"                 TO  MCP-TABLE
           MOVE  "key"                        TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"                  USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
           MOVE    SYU-PTID                TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =  SPACE
      *
               IF  SYU-KOH-HKNNUM(IDX)     NOT =   "027"   AND
                   SYU-KOH-HKNNUM(IDX)(1:1)    =   "0"
                   IF  WRK-ETC-KOHNUM(1)       =  SPACE
                   OR                             SYU-KOH-HKNNUM(IDX)
                     MOVE  1                    TO  IDY
                   ELSE
                     IF  WRK-ETC-KOHNUM(2)     =  SPACE
                     OR                           SYU-KOH-HKNNUM(IDX)
                       MOVE  2                  TO  IDY
                     ELSE
                       MOVE  3                  TO  IDY
                     END-IF
                   END-IF
                   MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-ETC-KOHNUM(IDY)
                   MOVE  COMB-KOHID    (IDX)   TO  WRK-ETC-KOHID(IDY)
                   ADD   SYU-TOTAL-HKNTEN      TO  WRK-ETC-TENSU(IDY)
      *            限度額が無い場合は一部負担金は表示しない
                   IF  (RECE02-FTNMONEY(1)     >   ZERO)   OR
                       (SYU-KOH-HKNNUM(IDX)    NOT =   "010" AND "011"
                                                   AND "021" AND "020")
                       ADD     SYU-KOH-COMPFTN(IDX)
                                               TO  WRK-ETC-ITBFTN(IDY)
                   END-IF
      *           主保険の食事療養費情報から食事療養費、標準負担額を取得
                   ADD  SYU-SYUSKJ-NISSU
                                           TO  WRK-ETC-S-NISSU(IDY)
                   ADD  SYU-SYUSKJ-RYOYOHI
                                           TO  WRK-ETC-S-KINGK(IDY)
                   ADD  SYU-SYUSKJ-FTN
                                           TO  WRK-ETC-S-FTNGK(IDY)
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      MF101-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      MF101-TYOKI
               END-IF
      *
           END-PERFORM
      *
           IF  RECE02-FTNMONEY(1)          >   ZERO
               IF  RECE02-RJNHKNNUM    NOT =   SPACE
                   ADD  SYU-KOH-COMPFTN(1) TO WRK-FTNMONEY-G
               ELSE
                   ADD  SYU-SYUCOMPFTN     TO WRK-FTNMONEY-G
               END-IF
           END-IF
      *
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
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    WRK-PATH       TO  MCP-PATHNAME
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
                 MOVE    "tbl_syunou"   TO  MCP-TABLE
                 MOVE    WRK-PATH       TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"   TO  MCP-FUNC
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
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
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE  RECE02-PTID        TO  PTKOH-PTID
           MOVE  WRK-KOHID          TO  PTKOH-KOHID
      *
           MOVE  PTKOHINF-REC            TO  MCPDATA-REC
           MOVE  "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
                 IF  PTKOH-SAKJOKBN  NOT = "1"
                     IF  PTKOH-TEKSTYMD(1:6) <=  RECE02-SRYYM(1:6)  AND 
                         PTKOH-TEKEDYMD(1:6) >=  RECE02-SRYYM(1:6)
                         MOVE  PTKOH-FTNJANUM    TO  WRK-ETC-FTNJANUM
                         MOVE  PTKOH-JKYSNUM     TO  WRK-ETC-JKYSNUM
                     END-IF
                 END-IF
                 MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                 MOVE    "key"               TO  MCP-PATHNAME
                 PERFORM  935-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
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
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
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
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
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
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
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
