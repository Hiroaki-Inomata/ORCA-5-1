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
       PROGRAM-ID.     SEIKYU2505.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 滋賀福祉ＣＳＶファイル作成（ＦＤ請求）
      *                      (142,242,140,141,241,341,143,243,343)
      *                      (144,244,344,145,245,345,146,246,170)
      *                      (441,541,340)
      *                      (対象公費はTAISYOKOHI.INI参照)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/02/22    内田          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    08/05/21    請求年月の印字修正
      *  01.00.02    吉川    08/10/20    結核・精神を併用していても
      *                                  公費分患者負担額を計算する
      *  01.00.03    吉川    09/11/10    特記事項０３を持っている場合は
      *                                  印字対象外とする
      *  01.00.04    吉川    10/04/16    印字対象番号の追加（340）
      *  01.00.05    吉川    10/11/24    open-cobol1.0対応
      *  01.00.06    吉川    11/12/09    長期は上限額を超えた時に記載する
      *  01.00.07    吉川    12/03/05    実日数が２日以上の場合、
      *                                  長期が記載されなくなる不具合を修正
      *  01.00.08    吉川    12/08/17    対象公費番号をINIファイルから
      *                                  取得するようにする
      *                                  地方公費を２つ所持している場合
      *                                  第二公費欄に印字する
      *  01.00.09    吉川    13/01/10    プラグイン対応
      *  01.00.10    吉川    13/10/04    プログラムオプション対応
      *  01.00.11    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.12    吉川    16/03/11    長期上位は「長」欄に16を記載する
      *  01.00.13    萩野    16/10/05    前期高齢者の軽減特例措置対象者の場合は
      *                                  給付割合に８割と印字するよう修正
      *  01.00.14    門間    17/12/04    全国公費＋長期併用時の不具合対応
      *                                  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.15    吉川    18/02/14    「長」欄の印字不具合対応
      *  01.00.16    井上    18/12/12    修正履歴コメント削除
      *  01.00.17    門間    20/03/11    岐阜県の記載仕様に対応
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
      *    レセプト０２データ（外来）
           SELECT  RECE02X-FILE     ASSIGN  RECE02XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE02X-KEY
                                   FILE    STATUS  IS  STS-RECE02X.
      *
      *    レセプト０２データ（入院）
           SELECT  RECE62X-FILE    ASSIGN  RECE62XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE62X-KEY
                                   FILE    STATUS  IS  STS-RECE62X.
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
           COPY  "SEI2503.INC".
      *
      *    レセプト０２データ（外来）
       FD  RECE02X-FILE.
       01  RECE02X-REC.
           COPY    "CPRCF002.INC"  REPLACING  //RECE02//
                                   BY         //RECE02X//.
      *
      *    レセプト０２データ（入院）
       FD  RECE62X-FILE.
       01  RECE62X-REC. 
           COPY    "CPRCF062.INC"  REPLACING  //RECE62//
                                   BY         //RECE62X//.
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    対象公費番号データ
           COPY  "TAISYO2501.INC".
      *
      *    シェル用領域
           COPY  "CPSHELLTBL.INC".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE02X//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE62X//.
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
           COPY  "SC2505.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100       PIC X(02).
           03  STS-RECEERR     PIC X(02).
           03  STS-RECE02X     PIC X(02).
           03  STS-RECE62X     PIC X(02).
           03  STS-TAISYOKOHI  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF       PIC 9(01).
           03  FLG-END         PIC 9(01).
           03  FLG-MF100       PIC 9(01).
           03  FLG-PRINT       PIC 9(01).
           03  FLG-SYSKANRI    PIC 9(01).
           03  FLG-ERR         PIC 9(01).
           03  FLG-SYUNOU      PIC 9(01).
           03  FLG-RECE10      PIC 9(01).
           03  FLG-PTKOHINF    PIC 9(01).
           03  FLG-HKNCOMBI    PIC 9(01).
           03  FLG-BTPARA      PIC 9(01).
           03  FLG-SYORI       PIC 9(01).
           03  FLG-MOJI        PIC 9(01).
           03  FLG-TYOKI       PIC 9(01).
           03  FLG-SEIKYU      PIC 9(01).
           03  FLG-TSYRRK      PIC 9(01).
           03  FLG-TNKRRK      PIC 9(01).
           03  FLG-RECE02X     PIC 9(01).
           03  FLG-RECE62X     PIC 9(01).
           03  FLG-TAISYOKOHI  PIC 9(01).
           03  FLG-TAISYO      PIC 9(01).
           03  FLG-PRGOPTION   PIC 9(01).
           03  FLG-OK          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
           03  IDW            PIC 9(04).
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
           03  WRK-Z4                PIC ZZZ9.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z8                PIC ZZZZZZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z11               PIC ZZZ,ZZZ,ZZZ.
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
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-TMP-NUM           PIC 9(09).
      *
           03  WRK-SEIKYU-COMP     PIC S9(09).
           03  WRK-SEIKYU          PIC 9(07).
           03  WRK-JKYSNUM         PIC X(08).
           03  WRK-TANJOYMD        PIC X(07).
           03  WRK-KASAN-TBL.
               05  WRK-KOHNUM      PIC X(03).
               05  WRK-KOHID       PIC 9(10).
               05  WRK-KOHTENSU    PIC 9(08).
               05  WRK-KOHFTNGK-G  PIC 9(08).
               05  WRK-KOHFTNGK    PIC 9(08).
               05  WRK-SYUCOMPFTN  PIC 9(08).
               05  WRK-TYOKIFTNGK  PIC 9(08).
               05  WRK-TYOKIFTNGK-G PIC 9(08).
           03  WRK-RECEKA           PIC X(02).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
           03  WRK-HENKAN              PIC X(20).
      *
           03  WRK-PATH                   PIC  X(50).
      *
      *    データ編集
           03  WRK-HENSHU.
               05  WRK-GOKEI-TBL           OCCURS  10.
                   07  WRK-GOKEI-YM        PIC 9(06).
                   07  WRK-GOKEI-DATA      OCCURS  150.
                       09  WRK-GOKEIKENSU  PIC  9(03).
               05  WRK-SOUGOKEIKENSU       PIC 9(07).
      *
               05  WRK-RECENO            PIC 9(05).
               05  WRK-RECEHKNSHU        PIC 9(01).
               05  WRK-TOKKI             PIC X(02).
               05  WRK-TOWN              PIC 9(03).
               05  WRK-CHK-SRYYM         PIC 9(06).
               05  WRK-HKNJACVS          PIC X(08).
               05  WRK-JKYSNUMCVS        PIC 9(07).
               05  WRK-MOJIST            PIC 9(03).
               05  WRK-MOJIED            PIC 9(03).
               05  WRK-RATE              PIC 9(02).
               05  WRK-MOJIHEN           PIC 9(10).
      *
               05  WRK-JNISSU            PIC X(03).
               05  WRK-EJNISSU1          PIC X(03).
               05  WRK-EJNISSU2          PIC X(03).
               05  WRK-TOTALTEN          PIC X(08).
               05  WRK-ETOTALTEN1        PIC X(08).
               05  WRK-ETOTALTEN2        PIC X(08).
               05  WRK-FTNMONEY          PIC X(08).
               05  WRK-EITBFTN1          PIC X(08).
               05  WRK-EITBFTN2          PIC X(08).
               05  WRK-SHOKUJINISSU      PIC X(03).
               05  WRK-SHOKUJIRYOYOHI    PIC X(08).
               05  WRK-SHOKUJIFTN        PIC X(08).
      *
           03  WRK-REC             PIC X(500).
           03  WRK-REC-LENGTH      PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(100).
               05  WRK-NUM         PIC ZZZZZZZZZZ.999.
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-ST          PIC 9(02).
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-END         PIC 9(01).
      *
           03  WRK-SCSVINFO-SEQ-NO       PIC 9(06).
           03  WRK-SCSVINFO-PTID         PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN    PIC X(01).
      *
           03  WRK-TMP-TBL  OCCURS  2.
               05  WRK-TMP-FTNJANUM      PIC X(08).
               05  WRK-TMP-JKYSNUM       PIC X(07).
               05  WRK-TMP-JNISSU        PIC 9(02).
               05  WRK-TMP-TENSU         PIC 9(07).
               05  WRK-TMP-YKZFTN        PIC 9(06).
               05  WRK-TMP-ITBFTN        PIC 9(06).
      *
           03  WRK-SUM-TBL.
               05  WRK-GKENSU            PIC 9(04).
               05  WRK-GTENSU            PIC 9(09).
               05  WRK-KOH-GTENSU        PIC 9(09).
               05  WRK-GITBFTN           PIC 9(07).
               05  WRK-KOH-GITBFTN       PIC 9(09).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-OUT     PIC 9(06).
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
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *    レセプト印刷情報
           COPY    "CPRCF008.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    低所得情報
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    低年金情報
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
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
           MOVE    "SEIKYU2505"    TO  JOB-PGID
           MOVE    "福祉ＣＳＶ・請求書"
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
           MOVE  WRK-PARA-HOSPNUM     TO  RECE02XPARA-HOSPNUM
           MOVE  "RECE02X"            TO  RECE02XPARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  RECE02XPARA-TERMID
      *
           MOVE  WRK-PARA-HOSPNUM     TO  RECE62XPARA-HOSPNUM
           MOVE  "RECE62X"            TO  RECE62XPARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  RECE62XPARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECE02XPARA-BASENAME TO  SGETTEMP-BASENAMES (2)
           MOVE  RECE62XPARA-BASENAME TO  SGETTEMP-BASENAMES (3)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (4)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECE02XPARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECE62XPARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (4)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"         TO  SYS-1001-KANRICD
           MOVE  "*"            TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"           TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC   TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU2505 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"         TO  SYS-1002-KANRICD
           MOVE  "*"            TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"           TO  SYS-1002-STYUKYMD(7:2)
                                    SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC   TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU2505 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
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
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU2505"              TO  WRK-PRGID
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
      *       ＣＳＶファイル出力
              PERFORM   2002-PRINT-SEC
      *       帳票印刷
              PERFORM   2003-PRINT-SEC
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
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
             INITIALIZE                            TBL-HIKAKU-KOHI
             MOVE    RECE02-KOHNUM(IDZ)        TO  TBL-HKOHNUM(1)
      *      対象公費番号チェック処理
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
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  RECE02-TEISYUTUSAKI TO  MF100-TEISYUTUSAKI
           MOVE  RECE02-RECESYUBETU  TO  MF100-RECESYUBETU
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
      *
           MOVE  RECE02-NAME               TO  MF100-SORT
      *
      *    一時ファイル読み込み
           PERFORM  900-SKTTMP-READ-SEC
      *
      *    前期９割（軽減特例措置）は８割
           IF   (RECE02-FLG(2)  =  1)
             MOVE  20              TO  MF100-KYURATE
           END-IF
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF100-FTNJANUM
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF100-JKYSNUM
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSU
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-K-SHOKUJIINF
      *
      *    収納情報より全国公費分集計、主保険負担金集計及び助成判定を行う
           MOVE  1                         TO  FLG-ERR
           INITIALIZE       WRK-KASAN-TBL
           MOVE    MF100-KOHNUM    TO  WRK-KOHNUM
           MOVE    MF100-KOHID     TO  WRK-KOHID
           MOVE    ZERO            TO  FLG-TYOKI
      *
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
           PERFORM 920-SYUNOU-SET-SEC
      *
      *    主保険に限度額が有る場合は集計した金額を編集する
           IF     RECE02-FTNMONEY(1)  >   ZERO     AND
                  RECE02-FTNMONEY(1)  <   WRK-SYUCOMPFTN
             MOVE    RECE02-FTNMONEY(1)   TO  WRK-SYUCOMPFTN
           END-IF
      *
      *    長期
           IF      FLG-TYOKI       =   1
           AND     WRK-TYOKIFTNGK-G  >=  10000
               MOVE    1           TO  MF100-TYOKI
               IF  WRK-KOHFTNGK-G  >=  10000
                   MOVE    10000    TO  WRK-KOHFTNGK-G
               END-IF
           END-IF
      *    長期上位
           IF      FLG-TYOKI       =   2
           AND     WRK-TYOKIFTNGK-G  >=  20000
               MOVE    2           TO  MF100-TYOKI
               IF  WRK-KOHFTNGK-G  >=  20000
                   MOVE    20000    TO  WRK-KOHFTNGK-G
               END-IF
           END-IF
      *
           MOVE    WRK-SYUCOMPFTN  TO  MF100-SYUCOMPFTN
           MOVE    WRK-KOHTENSU    TO  MF100-KOHTENSU
           MOVE    WRK-KOHFTNGK-G  TO  MF100-KOHFTNGK
      *
      *    保険使用時情報・県番号取得（公費請求テーブルにない）
           PERFORM  900-SEIKYU-SET-SEC
           MOVE  RECE10-PREFNUM            TO  MF100-PREFNUM
           MOVE  RECE10-JNISSU      (1)    TO  MF100-H-JNISSU
           MOVE  RECE10-FTNMONEY    (1)    TO  MF100-H-FTNMONEY
           MOVE  RECE10-TOTALTEN    (1)    TO  MF100-H-TOTALTEN
           MOVE  RECE10-YKZFTN      (1)    TO  MF100-H-YKZFTN
           MOVE  RECE10-SHOKUJIINF  (1)    TO  MF100-H-SHOKUJIINF
      *
      *    全国公費を併用していた場合、負担者番号、受給者番号取得
           IF      MF100-TAHONUM   NOT =   SPACE
               PERFORM 930-PTKOHINF-SET-SEC
               MOVE    PTKOH-FTNJANUM      TO  MF100-TAHOFTNJANUM
               MOVE    PTKOH-JKYSNUM       TO  MF100-TAHOJKYSNUM
               DISPLAY  "併用公費番号="    MF100-TAHONUM
                        "公費ID="          MF100-TAHOKOHID
                        "負担者番号="      MF100-TAHOFTNJANUM
                        "受給者番号="      MF100-TAHOJKYSNUM
      *
      *        全国公費併用時の実日数等取得
               MOVE  RECE10-JNISSU (2)     TO  MF100-TAHOJNISSU
               MOVE  RECE10-YKZFTN (2)     TO  MF100-TAHOYKZFTN
               MOVE  RECE10-SHOKUJIINF(2)  TO  MF100-K-SHOKUJIINF
           ELSE
      *      地方公費二つの場合でも他法公費へ格納
             IF  IDZ  >  1
               MOVE    RECE02-KOHID(IDZ - 1)      TO  MF100-TAHOKOHID
               MOVE    RECE02-KOHNUM(IDZ - 1)     TO  MF100-TAHONUM
               MOVE    RECE02-KOHFTNJANUM(IDZ - 1)
                                                  TO  MF100-TAHOFTNJANUM
               MOVE    RECE02-KOHJKYSNUM(IDZ - 1) TO  MF100-TAHOJKYSNUM
               MOVE    RECE02-JNISSU(IDY - 1)     TO  MF100-TAHOJNISSU
               MOVE    RECE02-TOTALTEN(IDY - 1)   TO  MF100-KOHTENSU
               MOVE    RECE02-FTNMONEY(IDY - 1)   TO  MF100-KOHFTNGK
             END-IF
           END-IF
      *
      *    低所得チェック
           PERFORM  900-TSYRRK-SET-SEC
           PERFORM  900-TNKRRK-SET-SEC
      *
      *    在宅再掲チェック
           IF  RECE02-NYUGAIKBN  =  1
               PERFORM  900-ZAITAKU-N-SAICHK-SEC
           ELSE
               PERFORM  900-ZAITAKU-G-SAICHK-SEC
           END-IF
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
               DISPLAY "FLG-MF100=" FLG-MF100
               DISPLAY "MF100-KEY=" MF100-KEY
           IF      FLG-ERR             =   ZERO
             IF      FLG-MF100  =  1
               WRITE   MF100-REC
               ADD     1               TO  CNT-MF100
             ELSE
                 REWRITE  MF100-REC
             END-IF
           ELSE
               DISPLAY "助成対象外=" MF100-PTNUM
                       "入外区分="   MF100-NYUGAIKBN
                       "公費番号="   MF100-KOHNUM
                       "点数="       MF100-TOTALTEN
                       "負担金="     MF100-FTNMONEY
                       "負担者番号=" MF100-FTNJANUM
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
           MOVE    MF100-FTNJANUM(3:5)   TO  WRK-HKNJANUM(1:5)
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
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
      *    ＣＳＶヘッダー処理
           PERFORM  330-CSVH-HEN-SEC
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    FLG-PRINT   =  1
      *       ＣＳＶ<レセ>処理
              PERFORM  332-CSVRECE-HEN-SEC
      *
           END-PERFORM
      *    ＣＳＶ＜合計＞処理
           PERFORM  333-CSVGOKEI-HEN-SEC
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<ヘッダー>処理
      *****************************************************************
       330-CSVH-HEN-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *    ヘッダー
      *
      *    レコード識別番号
           MOVE    "1"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    事業区分
           MOVE    9                       TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    医療機関コード（先頭スペース詰め）
           MOVE    SPACE                   TO  WRK-DATA(1:3)
      *    医療機関コード（滋賀県以外）
           IF  SYS-1001-PREFNUM  NOT =  25
             MOVE    SYS-1001-PREFNUM        TO  WRK-DATA(1:2)
             MOVE    "1"                     TO  WRK-DATA(3:1)
           END-IF
           MOVE    SYS-1001-HOSPCD         TO  WRK-DATA(4:)
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    医療機関名称
           MOVE    SYS-1001-HOSPNAME       TO  WRK-DATA
           MOVE    40                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    点数表区分
           MOVE    SYS-1001-TENHYOKBN      TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    請求年月
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                    TO  LNK-DAY1-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
           MOVE    LNK-DAY1-WYMD(1:5)      TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    マルチボリューム識別番号
           MOVE    "00"                    TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       330-CSVH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<内容>処理
      *****************************************************************
       332-CSVRECE-HEN-SEC      SECTION.
      *
           INITIALIZE  WRK-REC
                       WRK-REC-LENGTH
           INITIALIZE  FLG-MOJI
      *
      *    レセプト
      *
      *    １．レコード識別番号
           MOVE   "2"                      TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．旧総合病院診療科
           IF  SYS-1001-HOSPSBT1  =  1
               MOVE    RECE02-RECEKA       TO  WRK-DATA
           ELSE
               MOVE    SPACE               TO  WRK-DATA
           END-IF
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．保険者番号（先頭スペース詰め）
           INITIALIZE  WRK-MOJIST
           INITIALIZE  WRK-MOJIED
           PERFORM   VARYING  IDX2    FROM    1   BY  1
                UNTIL     FLG-MOJI   =   1
                   OR     WRK-MOJIED =   8
              IF  MF100-HKNJANUM(IDX2:1)    =    SPACE
                 MOVE   1             TO     FLG-MOJI
              ELSE
                 ADD    1             TO     WRK-MOJIED
              END-IF
           END-PERFORM
           COMPUTE  WRK-MOJIST  =  9  -  WRK-MOJIED
           MOVE  SPACE                TO     WRK-HKNJACVS
           MOVE  MF100-HKNJANUM       TO
                                  WRK-HKNJACVS(WRK-MOJIST:WRK-MOJIED)
      *
           MOVE    WRK-HKNJACVS            TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    保険証番号チェック
           MOVE     SPACE    TO    WRK-HENKAN
           MOVE     ZERO     TO    IDX2
           PERFORM  VARYING  IDX1  FROM  1   BY  2
                    UNTIL    IDX1  >     20
                    OR       MF100-NUM(IDX1:2) = SPACE
             EVALUATE  MF100-NUM(IDX1:2)
             WHEN "１"
               ADD      1    TO    IDX2
               MOVE    "1"   TO    WRK-HENKAN(IDX2:1)
             WHEN "２"
               ADD      1    TO    IDX2
               MOVE    "2"   TO    WRK-HENKAN(IDX2:1)
             WHEN "３"
               ADD      1    TO    IDX2
               MOVE    "3"   TO    WRK-HENKAN(IDX2:1)
             WHEN "４"
               ADD      1    TO    IDX2
               MOVE    "4"   TO    WRK-HENKAN(IDX2:1)
             WHEN "５"
               ADD      1    TO    IDX2
               MOVE    "5"   TO    WRK-HENKAN(IDX2:1)
             WHEN "６"
               ADD      1    TO    IDX2
               MOVE    "6"   TO    WRK-HENKAN(IDX2:1)
             WHEN "７"
               ADD      1    TO    IDX2
               MOVE    "7"   TO    WRK-HENKAN(IDX2:1)
             WHEN "８"
               ADD      1    TO    IDX2
               MOVE    "8"   TO    WRK-HENKAN(IDX2:1)
             WHEN "９"
               ADD      1    TO    IDX2
               MOVE    "9"   TO    WRK-HENKAN(IDX2:1)
             WHEN "０"
               ADD      1    TO    IDX2
               MOVE    "0"   TO    WRK-HENKAN(IDX2:1)
             WHEN  OTHER
               MOVE    20    TO    IDX2
               INITIALIZE          WRK-HENKAN
             END-EVALUATE
           END-PERFORM
      *    保険証番号が数字ではないなら記号欄へ追加
           IF  WRK-HENKAN   NOT =  SPACE
             MOVE  WRK-HENKAN  TO  MF100-NUM
           ELSE
             STRING  MF100-KIGO    DELIMITED  BY  SPACE
                     MF100-NUM     DELIMITED  BY  SPACE
                     INTO          MF100-KIGO
             END-STRING
             INITIALIZE            MF100-NUM
           END-IF
      *
      *    ４．被保険者証記号
           MOVE    MF100-KIGO              TO  WRK-DATA
           MOVE    20                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．被保険者証番号
           MOVE    MF100-NUM               TO  WRK-DATA
           MOVE    20                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ６．生年月日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    MF100-BIRTHDAY      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
      *
           MOVE    LNK-DAY1-WYMD           TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ７．性別
           MOVE    MF100-SEX               TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ８．市町村番号
           MOVE    MF100-RJNFTNJANUM       TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ９．受給者番号
           IF  MF100-RJNJKYSNUM  NOT =  SPACE
               MOVE    MF100-RJNJKYSNUM    TO  WRK-JKYSNUMCVS
               MOVE    WRK-JKYSNUMCVS      TO  WRK-DATA
           ELSE
               MOVE    MF100-RJNJKYSNUM    TO  WRK-DATA
           END-IF
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １０．診療年月
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                    TO  LNK-DAY1-IRAI
           MOVE    MF100-SRYYM             TO  LNK-DAY1-YMD(1:6)
           MOVE    "01"                    TO  LNK-DAY1-YMD(7:)
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
      *
           MOVE    LNK-DAY1-WYMD(1:5)      TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １１．保険制度（岐阜は別処理あり）
           MOVE    1                       TO  WRK-NUM
           IF  MF100-RJNHKNNUM  NOT =  SPACE
               MOVE    3                   TO  WRK-NUM
           END-IF
           IF   MF100-PREFNUM  =  21
           AND  MF100-HKNNUM  =  "067"
               MOVE    4                   TO  WRK-NUM
           END-IF
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １２．保険種別
           MOVE    2                   TO  WRK-NUM
           IF  MF100-TAHONUM  NOT =  SPACE
               MOVE    3               TO  WRK-NUM
           END-IF
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １３．本人家族入外
           MOVE    MF100-RECESYUBETU(4:1)  TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １４．給付割合
           EVALUATE  MF100-KYURATE
               WHEN  "010"
                  MOVE  90                 TO  WRK-NUM
               WHEN  "020"
                  MOVE  80                 TO  WRK-NUM
               WHEN  "030"
                  MOVE  70                 TO  WRK-NUM
               WHEN  OTHER
                  MOVE  00                 TO  WRK-NUM
           END-EVALUATE
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １５．特記事項１
           IF  MF100-TYOKI  =  1
               MOVE    "02"                TO  WRK-DATA
           END-IF
           IF  MF100-TYOKI  =  2
               MOVE    "16"                TO  WRK-DATA
           END-IF
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １６．特記事項２
           MOVE    SPACE                   TO  WRK-DATA
           MOVE    0                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １７．保険＿診療実日数
           MOVE    MF100-H-JNISSU          TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １８．保険＿請求点数
           MOVE    MF100-H-TOTALTEN        TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １９．保険＿薬剤一部負担金
           MOVE    MF100-H-YKZFTN          TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２０．請求公費負担金額
           IF  MF100-NYUGAIKBN  =  1
               MOVE  MF100-ZAITAKU-SAIKEI  TO  WRK-NUM
           ELSE
               MOVE  ZERO                  TO  WRK-NUM
           END-IF
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２１．保険＿請求一部負担金
           MOVE    MF100-H-FTNMONEY        TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２２．公費負担額（外来分）
           IF  MF100-NYUGAIKBN  =  2
               MOVE  MF100-ZAITAKU-SAIKEI  TO  WRK-NUM
           ELSE
               MOVE  ZERO                  TO  WRK-NUM
           END-IF
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    全国公費を併用しているか
      *    （SEIKYU2503修正時についでに直しておきたい）
           IF  MF100-TAHONUM       NOT =   SPACE
               MOVE  MF100-TAHOFTNJANUM    TO  WRK-TMP-FTNJANUM(1)
               MOVE  MF100-TAHOJKYSNUM     TO  WRK-TMP-JKYSNUM(1)
               MOVE  MF100-TAHOJNISSU      TO  WRK-TMP-JNISSU(1)
               MOVE  MF100-KOHTENSU        TO  WRK-TMP-TENSU(1)
               MOVE  MF100-TAHOYKZFTN      TO  WRK-TMP-YKZFTN(1)
               MOVE  MF100-KOHFTNGK        TO  WRK-TMP-ITBFTN(1)
      *
               MOVE  MF100-FTNJANUM        TO  WRK-TMP-FTNJANUM(2)
               MOVE  MF100-JKYSNUM         TO  WRK-TMP-JKYSNUM(2)
               MOVE  MF100-JNISSU          TO  WRK-TMP-JNISSU(2)
               MOVE  MF100-TOTALTEN        TO  WRK-TMP-TENSU(2)
               MOVE  MF100-YKZFTN          TO  WRK-TMP-YKZFTN(2)
               MOVE  MF100-FTNMONEY        TO  WRK-TMP-ITBFTN(2)
           ELSE
               MOVE  MF100-FTNJANUM        TO  WRK-TMP-FTNJANUM(1)
               MOVE  MF100-JKYSNUM         TO  WRK-TMP-JKYSNUM(1)
               MOVE  MF100-JNISSU          TO  WRK-TMP-JNISSU(1)
               MOVE  MF100-TOTALTEN        TO  WRK-TMP-TENSU(1)
               MOVE  MF100-YKZFTN          TO  WRK-TMP-YKZFTN(1)
               MOVE  MF100-FTNMONEY        TO  WRK-TMP-ITBFTN(1)
      *
               INITIALIZE                  WRK-TMP-TBL(2)
           END-IF
      *    ２３．公１＿公費負担者番号
           MOVE    WRK-TMP-FTNJANUM(1)     TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２４．公１＿受給者番号
           IF  WRK-TMP-JKYSNUM(1)  NOT =  SPACE
               MOVE    WRK-TMP-JKYSNUM(1)  TO  WRK-JKYSNUMCVS
               MOVE    WRK-JKYSNUMCVS      TO  WRK-DATA
           ELSE
               MOVE    WRK-TMP-JKYSNUM(1)  TO  WRK-DATA
           END-IF
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２５．公１＿診療実日数
           MOVE    WRK-TMP-JNISSU(1)       TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２６．公１＿請求点数
           MOVE    WRK-TMP-TENSU(1)        TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２７．公１＿薬剤一部負担金
           MOVE    WRK-TMP-YKZFTN(1)       TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２８．公１＿一部負担金
           MOVE    WRK-TMP-ITBFTN(1)       TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ２９．公２＿公費負担者番号
           MOVE    WRK-TMP-FTNJANUM(2)     TO  WRK-DATA
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３０．公２＿受給者番号
           IF  WRK-TMP-JKYSNUM(2)  NOT =  SPACE
               MOVE    WRK-TMP-JKYSNUM(2)  TO  WRK-JKYSNUMCVS
               MOVE    WRK-JKYSNUMCVS      TO  WRK-DATA
           ELSE
               MOVE    WRK-TMP-JKYSNUM(2)  TO  WRK-DATA
           END-IF
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３１．公２＿診療実日数
           MOVE    WRK-TMP-JNISSU(2)       TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３２．公２＿請求点数
           MOVE    WRK-TMP-TENSU(2)        TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３３．公２＿薬剤一部負担金
           MOVE    WRK-TMP-YKZFTN(2)       TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３４．公２＿一部負担金
           MOVE    WRK-TMP-ITBFTN(2)       TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３５．保険＿食事日数
           MOVE    MF100-H-SHOKUJINISSU    TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３６．保険＿食事請求基準額
           MOVE    MF100-H-SHOKUJIRYOYOHI  TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３７．保険＿食事標準負担額
           MOVE    MF100-H-SHOKUJIFTN      TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３８．第１公費＿食事日数
           MOVE    MF100-K-SHOKUJINISSU    TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３９．第１公費＿食事請求基準額
           MOVE    MF100-K-SHOKUJIRYOYOHI  TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ４０．第一公費＿食事標準負担額
           MOVE    MF100-K-SHOKUJIFTN      TO  WRK-NUM
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ４１．氏名
           MOVE    MF100-NAME              TO  WRK-DATA
           MOVE    40                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４２．カルテ番号 OR 患者ＩＤ
           MOVE    MF100-PTID              TO  WRK-DATA
           MOVE    20                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４３．低所得１
           MOVE    MF100-TEISHO1           TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４４．低所得２
           MOVE    MF100-TEISHO2           TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
      *    合計加算
           ADD  1  TO  WRK-GKENSU
           COMPUTE  WRK-GTENSU  =  WRK-GTENSU  +  MF100-H-TOTALTEN
           COMPUTE  WRK-KOH-GTENSU  =  WRK-KOH-GTENSU
                                    +  MF100-TOTALTEN
           COMPUTE  WRK-GITBFTN  =  WRK-GITBFTN  +  MF100-H-FTNMONEY
           COMPUTE  WRK-KOH-GITBFTN  =  WRK-KOH-GITBFTN
                                     +  MF100-FTNMONEY
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
           .
       332-CSVRECE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<合計>処理
      *****************************************************************
       333-CSVGOKEI-HEN-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *    合計
      *
      *    レコード識別情報
           MOVE    "3"                     TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    明細データ：レコード件数
           MOVE    WRK-GKENSU              TO  WRK-NUM
           MOVE    4                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    明細データ：請求点数合計
           MOVE    WRK-GTENSU              TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    明細データ：福祉医療費分点数合計
           MOVE    WRK-KOH-GTENSU          TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    明細データ：保険＿請求一部負担金合計
           MOVE    WRK-GITBFTN             TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    明細データ：福祉医療費に係る一部負担金
           MOVE    WRK-KOH-GITBFTN         TO  WRK-NUM
           MOVE    9                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       333-CSVGOKEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯ出力処理
      *****************************************************************
       500-CSVINFO-SEC                SECTION.
      *
           ADD     1                   TO  WRK-SCSVINFO-SEQ-NO
      *
           INITIALIZE                  ORCSCSVINFOAREA
           MOVE    "INS"               TO  SCSVINFO-MODE
           MOVE    WRK-PARA-SHELLID    TO  SCSVINFO-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SCSVINFO-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SCSVINFO-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-RENNUM TO  SCSVINFO-RENNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SCSVINFO-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SCSVINFO-SKYYMD
           MOVE    WRK-SCSVINFO-SEQ-NO TO  SCSVINFO-SEQ-NO
           MOVE    WRK-SCSVINFO-NYUGAIKBN
                                       TO  SCSVINFO-NYUGAIKBN
           MOVE    WRK-SCSVINFO-PTID   TO  SCSVINFO-PTID
           MOVE    WRK-REC             TO  SCSVINFO-CSVDATA
           CALL    "ORCSCSVINFO"       USING
                                       ORCSCSVINFOAREA
                                       SPA-AREA
           IF      SCSVINFO-RETURN     =   ZERO
               ADD     1                   TO  CNT-OUT
           ELSE
               MOVE    "ＣＳＶ用ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           .
       500-CSVINFO-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2003-PRINT-SEC                SECTION.
      *
      *    帳票編集処理
             PERFORM 300-BODY-HEN-SEC
      *
      *    帳票印刷処理
             PERFORM 310-PRINT-OUT-SEC
      *
           .
       2003-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-BODY-HEN-SEC          SECTION.
      *
           ADD     1                       TO  CNT-PAGE
           INITIALIZE                          SC05
      *
      *    医療機関番号
           MOVE    SYS-1001-HOSPCD         TO  SC05-HOSPCD-TBL(4:)
      *    医療機関番号（滋賀県以外）
           IF  SYS-1001-PREFNUM  NOT =  25
             MOVE    SYS-1001-PREFNUM        TO  SC05-HOSPCD-TBL(1:2)
             MOVE    "1"                     TO  SC05-HOSPCD-TBL(3:1)
           END-IF
      *    医療機関名称
           MOVE    SYS-1001-HOSPNAME       TO  SC05-HOSPNAME
      *    送付年月日
           MOVE    WRK-SEIYMDWH (1:8)      TO  SC05-SOUYY
           MOVE    WRK-SEIYMDWH (11:4)     TO  SC05-SOUMM
           MOVE    WRK-SEIYMDWH (17:4)     TO  SC05-SOUDD
      *    請求年月
           MOVE    WRK-SRYYMWH (1:16)      TO  SC05-SRYYM
      *    件数合計
           MOVE    WRK-GKENSU              TO  WRK-Z7
           MOVE    WRK-Z7                  TO  SC05-GKENSU
      *    請求点数合計
           MOVE    WRK-GTENSU              TO  WRK-Z11
           MOVE    WRK-Z11                 TO  SC05-GTENSU
      *    福祉医療費分点数合計
           MOVE    WRK-KOH-GTENSU          TO  WRK-Z11
           MOVE    WRK-Z11                 TO  SC05-GFUKUSHITEN
      *    一部負担金合計
           MOVE    WRK-GITBFTN             TO  WRK-Z11
           MOVE    WRK-Z11                 TO  SC05-GITBFTN
      *    福祉医療費に係る一部負担金合計
           MOVE    WRK-KOH-GITBFTN         TO  WRK-Z11
           MOVE    WRK-Z11                 TO  SC05-GFITBFTN
      *
           .
       300-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       310-PRINT-OUT-SEC                SECTION.
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
           MOVE   "SC2505.red"         TO  SPRT-PRTID
           MOVE   "福祉医療費請求書(磁気媒体専用)"
                                       TO  SPRT-TITLE
           MOVE    SC05                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
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
       310-PRINT-OUT-EXT.
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
      *      ジョブ終了処理
             MOVE    "JBE"           TO  SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
             MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID
                                     TO  JOB-SHELLID
             MOVE    WRK-RECEERR     TO  JOB-YOBI
             MOVE    "9999"          TO  JOB-ERRCD
             CALL    "ORCSJOB"       USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
           END-IF
      *
           MOVE   1                  TO  FLG-END
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
           CLOSE  MF100-FILE
           MOVE   CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2505 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2505 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU2505 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ファイル保存情報更新
           PERFORM 3001-FILE-INFO-INSERT-SEC
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
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
           STRING  "25"                DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN  DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED  BY  SIZE
                   "fuku"              DELIMITED  BY  SIZE
                   "00"                DELIMITED  BY  SIZE
                   ".CSV"              DELIMITED  BY  SIZE
                   INTO                SFILESV-TO-DATA  (1)
           END-STRING
           MOVE    WRK-GKENSU      TO  SFILESV-CNT-ALL  (1)
           MOVE    "福祉ＣＳＶ"    TO  SFILESV-TITLE    (1)
           IF      WRK-GKENSU      >   ZERO
               MOVE    "1"         TO  SFILESV-DATA-TYPE(1)
           END-IF 
           MOVE    WRK-RECEERR     TO  SFILESV-ERR-MSG  (1)
           IF      WRK-RECEERR NOT =   SPACE
               MOVE    "1"         TO  SFILESV-ERR-CODE (1)
           END-IF
           MOVE    "1"             TO  SFILESV-CODE-TYPE (1)
      *
           CALL    "ORCSFILESV"        USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       900-SKTTMP-READ-EXT.
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
               PERFORM  900-KOHSKY-CLOSE-SEC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *            特記事項０３を持っている場合は読み飛ばす
                   PERFORM  900-SEIKYU-SET-SEC
      *
                   IF  RECE10-TOKKI(1)  =  "03"
                   OR  RECE10-TOKKI(2)  =  "03"
                   OR  RECE10-TOKKI(3)  =  "03"
                   OR  RECE10-TOKKI(4)  =  "03"
                   OR  RECE10-TOKKI(5)  =  "03"
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
      *    公費請求マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
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
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
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
           MOVE    ZERO    TO      WRK-KOHFTNGK
                                   WRK-TYOKIFTNGK
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
      *
      *            公費番号設定
                   MOVE    SYU-KOH-HKNNUM(IDX) TO  MF100-TAHONUM
                   MOVE    COMB-KOHID(IDX)     TO  MF100-TAHOKOHID
      *            公費分点数集計
                   ADD     SYU-TOTAL-HKNTEN    TO  WRK-KOHTENSU
      *
      *                結核・精神は記載を省略
                   IF      RECE02-FTNMONEY(1)  >   ZERO
                       MOVE    SYU-KOH-COMPFTN(IDX)
                                               TO  WRK-KOHFTNGK
                   ELSE
                           MOVE    SYU-KOH-COMPFTN(IDX)
                                               TO  WRK-KOHFTNGK
                   END-IF
               END-IF
      *
      *        計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF      FLG-ERR                 =   1
               AND     SYU-KOH-HKNNUM(IDX)     =   WRK-KOHNUM
                   IF      IDX                 =   1
                       IF      SYU-SYUCOMPFTN-ENTANI
                                       >   SYU-KOH-COMPFTN-ENTANI(IDX)
                           MOVE    ZERO        TO  FLG-ERR
                       END-IF
                   ELSE
                       IF      SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                       >   SYU-KOH-COMPFTN-ENTANI(IDX)
                           MOVE    ZERO        TO  FLG-ERR
                       END-IF
                   END-IF
               END-IF
      *
      *
               IF      SYU-KOH-HKNNUM(IDX)     =   "972"
                   MOVE    1       TO  FLG-TYOKI
                   MOVE    SYU-KOH-COMPFTN(IDX)  TO  WRK-TYOKIFTNGK
               END-IF
      *
      *        長期上位
               IF      SYU-KOH-HKNNUM(IDX)     =   "974"
                   MOVE    2       TO  FLG-TYOKI
                   MOVE    SYU-KOH-COMPFTN(IDX)  TO  WRK-TYOKIFTNGK
               END-IF
           END-PERFORM
      *
      *    主保険に限度額が有る場合のみ主保険計算負担金を集計
           IF      RECE02-FTNMONEY(1)          >   ZERO
               IF      RECE02-RJNHKNNUM    NOT =   SPACE
                   ADD     SYU-KOH-COMPFTN(1)  TO  WRK-SYUCOMPFTN
               ELSE
                   ADD     SYU-SYUCOMPFTN      TO  WRK-SYUCOMPFTN
               END-IF
           END-IF   
      *
           ADD     WRK-KOHFTNGK        TO  WRK-KOHFTNGK-G
           ADD     WRK-TYOKIFTNGK      TO  WRK-TYOKIFTNGK-G
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
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
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
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
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
           CALL  "ORCDBMAIN"        USING
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
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-SET-SEC         SECTION.
      *
           INITIALIZE                          RECE10-REC
           MOVE    RECE02-KEY              TO  RECE10-KEY
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-RECE10
                   MOVE    MCPDATA-REC     TO  RECE10-REC
               ELSE
                   MOVE    1               TO  FLG-RECE10
                   INITIALIZE              RECE10-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
               INITIALIZE                  RECE10-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SEIKYU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得情報読み込み
      *****************************************************************
       900-TSYRRK-SET-SEC         SECTION.
      *
           INITIALIZE                          TSYRRK-REC
                                               FLG-TSYRRK
           MOVE    RECE02-HOSPNUM          TO  TSYRRK-HOSPNUM
           MOVE    RECE02-PTID             TO  TSYRRK-PTID
           MOVE    TSYRRK-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM  UNTIL  FLG-TSYRRK  =  1
                   PERFORM  900-TSYRRK-READ-SEC
      *        低所得が該当診療月で有効か
                   IF   TSYRRK-NINSTYMD(1:6)  <  RECE02-SRYYM
                   AND  TSYRRK-NINEDYMD(1:6)  >  RECE02-SRYYM
                       MOVE  "2"           TO  MF100-TEISHO2
                   END-IF
               END-PERFORM
           ELSE
               MOVE    1                   TO  FLG-TSYRRK
               INITIALIZE                  TSYRRK-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-TSYRRK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得情報フェッチ
      *****************************************************************
       900-TSYRRK-READ-SEC  SECTION.
      *
           MOVE    "tbl_tsyrrk"   TO  MCP-TABLE
           MOVE    "key2"         TO  MCP-PATHNAME
           MOVE    "DBFETCH"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  TSYRRK-REC
           ELSE
               INITIALIZE         TSYRRK-REC
               MOVE  1            TO  FLG-TSYRRK
           END-IF
      *
           .
       900-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低年金情報読み込み
      *****************************************************************
       900-TNKRRK-SET-SEC         SECTION.
      *
           INITIALIZE                          TNKRRK-REC
                                               FLG-TNKRRK
           MOVE    RECE02-HOSPNUM          TO  TNKRRK-HOSPNUM
           MOVE    RECE02-PTID             TO  TNKRRK-PTID
           MOVE    TNKRRK-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM  UNTIL  FLG-TNKRRK  =  1
                   PERFORM  900-TNKRRK-READ-SEC
      *        低所得が該当診療月で有効か
                   IF   TNKRRK-STYMD(1:6)  <  RECE02-SRYYM
                   AND  TNKRRK-EDYMD(1:6)  >  RECE02-SRYYM
                       MOVE  "1"           TO  MF100-TEISHO1
                   END-IF
               END-PERFORM
           ELSE
               MOVE    1                   TO  FLG-TNKRRK
               INITIALIZE                  TSYRRK-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-TNKRRK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    低年金情報フェッチ
      *****************************************************************
       900-TNKRRK-READ-SEC  SECTION.
      *
           MOVE    "tbl_tnkrrk"   TO  MCP-TABLE
           MOVE    "key2"         TO  MCP-PATHNAME
           MOVE    "DBFETCH"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  TNKRRK-REC
           ELSE
               INITIALIZE         TNKRRK-REC
               MOVE  1            TO  FLG-TNKRRK
           END-IF
      *
           .
       900-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE    SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    RECE02-PTID        TO  PTKOH-PTID
           MOVE    MF100-TAHOKOHID    TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  935-PTKOHINF-READ-SEC
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
           MOVE  "DBFETCH"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW    <       1
                   IF      WRK-DATA (IDW:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW)           DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW    FROM    WRK-ST    BY  1
                       UNTIL   IDW    >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                      IDZ
                        MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
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
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"               TO  MCP-FUNC
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
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
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
      *****************************************************************
      *    在宅再掲チェック（外来）
      *****************************************************************
       900-ZAITAKU-G-SAICHK-SEC          SECTION.
      *
           MOVE  RECE02-RECEKA   TO  WRK-RECEKA
      *    ファイル取得の為少しキー変更
           IF  RECE02-RECEKA  =  ZERO
               MOVE  SPACE       TO  RECE02-RECEKA
           END-IF
           IF  RECE02-HOJOKBN-KEY  =  ZERO
               MOVE  SPACE       TO  RECE02-HOJOKBN-KEY
           END-IF
      *
           OPEN    INPUT               RECE02X-FILE
           IF  STS-RECE02X       NOT =   ZERO
               DISPLAY "*** RECE02X OPEN ERR  ***"
           END-IF
      *
           PERFORM  990-RECE02X-READ-SEC
      *
      *    在宅再掲データを取得
           IF  FLG-RECE02X  =  ZERO
               IF  RECE02X-ZAITAKU-SAIKEI  >  ZERO
                   MOVE  RECE02X-ZAITAKU-SAIKEI
                                          TO  MF100-ZAITAKU-SAIKEI
               END-IF
           END-IF
           CLOSE        RECE02X-FILE
      *
           MOVE  WRK-RECEKA     TO  RECE02-RECEKA
      *
           .
       900-ZAITAKU-G-SAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理（外来）
      *****************************************************************
       990-RECE02X-READ-SEC           SECTION.
      *
           READ    RECE02X-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE02X
               NOT AT  END
                   MOVE    ZERO                TO  FLG-RECE02X
                   IF  RECE02X-KEY  NOT =  RECE02-KEY
                       GO  TO  990-RECE02X-READ-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE02X-READ-EXT.
           EXIT.
      *****************************************************************
      *    在宅再掲チェック（入院）
      *****************************************************************
       900-ZAITAKU-N-SAICHK-SEC          SECTION.
      *
      *    ファイル取得の為少しキー変更
           MOVE  RECE02-RECEKA   TO  WRK-RECEKA
           IF  RECE02-RECEKA  =  ZERO
               MOVE  SPACE       TO  RECE02-RECEKA
           END-IF
           IF  RECE02-HOJOKBN-KEY  =  ZERO
               MOVE  SPACE       TO  RECE02-HOJOKBN-KEY
           END-IF
      *
           OPEN    INPUT               RECE62X-FILE
           IF  STS-RECE62X          NOT =   ZERO
               DISPLAY "*** RECE62X OPEN ERR  ***"
           END-IF
      *
           PERFORM  990-RECE62X-READ-SEC
      *
      *    在宅再掲データを取得
           IF  FLG-RECE62X  =  ZERO
               IF  RECE62X-KOUREI-SAIKEI  >  ZERO
                   MOVE  RECE62X-KOUREI-SAIKEI
                                          TO  MF100-ZAITAKU-SAIKEI
               END-IF
           END-IF
           CLOSE        RECE62X-FILE
      *
           MOVE  WRK-RECEKA     TO  RECE02-RECEKA
      *
           .
       900-ZAITAKU-N-SAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理（入院）
      *****************************************************************
       990-RECE62X-READ-SEC           SECTION.
      *
           READ    RECE62X-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE62X
               NOT AT  END
                   MOVE    ZERO                TO  FLG-RECE62X
                   IF  RECE62X-KEY  NOT =  RECE02-KEY
                       GO  TO  990-RECE62X-READ-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE62X-READ-EXT.
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
