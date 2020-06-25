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
       PROGRAM-ID.             SOKATU1100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・退職者医療・老人保健及び公費総括表
      *                      (無床診療所用)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/07     有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者   日付      内容
      *  01.00.01    有村    05/08/17   「当月＋月遅れ」と「返戻分」は
      *                                 別紙で出力するよう修正
      *  01.00.02    劉洋    06/02/11   川越市の老人福祉医療を併用している
      *                                 患者が集計される
      *  01.00.03    劉亭    06/04/12   公費計の誤集計、印字のバグ修正
      *  01.00.04    内田    07/04/21   印字対象番号の追加（181等）
      *  01.00.05    内田    07/09/21   日レセver4.0対応
      *                                 システム管理マスタ「1901」の
      *                                 医療機関情報を参照
      *  01.00.06    内田    08/03/12   特別療養費対応
      *  01.00.07    長屋    08/04/24   帳票様式に伴う修正
      *  01.00.08    内田    08/05/08   請求額払い金額欄修正
      *                                 （前期高齢９割の特例措置対応）
      *  01.00.09    内田    08/06/25   後期高齢のみの場合でも印字するよう修正
      *                                 印字対象番号の追加（481等）
      *  01.00.10    内田    08/09/09   個別指定の保険者を後期高齢の抽出に
      *                                 適用していたのを修正
      *  01.00.11    吉川    08/10/08   印字対象番号の追加（581）
      *                                 シス管「2007」の公費まとめ対応
      *                                 SPA-AREA初期化
      *  01.00.12    内田    09/06/17   印字対象番号の追加（381）
      *  01.00.13    吉川    09/08/21   印字対象番号の追加（352,353,354）
      *  01.00.14    吉川    09/10/16   印字対象番号の追加（382）
      *  01.00.15    吉川    09/11/18   シス管「2007」のまとめ公費で
      *                                 6つ目以降がまとまらない不具合を修正
      *  01.00.16    吉川    10/02/04   シス管「2010」の公費再掲欄への
      *                                 地方公費記載設定対応
      *  01.00.17    吉川    10/02/18   保険番号782は請求額を記載しない
      *  01.00.18    吉川    10/08/03   オンライン返戻対応
      *  01.00.19    吉川    10/10/27   保険番号543は請求額を記載しない
      *  01.00.20    吉川    10/11/25   open-cobol1.0対応
      *  01.00.21    吉川    11/05/24   東北地方太平洋沖地震対応
      *  01.00.22    吉川    11/07/07   保険番号383は請求額を記載しない
      *  01.00.23    吉川    11/08/11   保険番号372,373は請求額を記載しない
      *  01.00.24    吉川    11/10/11   請求額を記載しない公費番号を
      *                                 INIファイルから取得するように修正
      *  01.00.25    吉川    12/04/17   「全国土木」「全国建設」は
      *                                 東京都に含めて集計する
      *                                 県外分は都道府県ごとに集計する
      *  01.00.26    吉川    12/12/21   プラグイン対応
      *                                 医療機関情報編集対応
      *  01.00.27    吉川    14/05/16   プログラムオプション対応
      *  01.00.28    吉川    14/07/02   再印刷のタイトル修正
      *  01.00.29    吉川    14/10/08    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.30    吉川    15/05/12   請求払い金額欄は主保険上限額を
      *                                 考慮するように修正
      *  01.00.31    吉川    15/11/16   月途中で保険を変更した場合の
      *                                 請求払い金額欄の修正
      *                                 医療機関編集情報への開設者名追加対応
      *  01.00.32    門間    17/09/04   医療機関コード変更時、
      *                                 医療機関コード別の作成処理対応
      *                                 公費番号まとめ情報の公費番号の追加
      *                                 特別療養費の返戻分が記載されるように修正
      *  01.00.33    門間    18/07/04   SITEKBN変更（動作に変更なし）
      *                                 記述漏れ対応
      *  01.00.34    井上    20/01/10   修正履歴コメント削除
      *  01.00.35    井上    20/03/12   公費番号まとめ情報があるとき、「請求額払の金額」欄が
      *                                 合算されるように修正。
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    総括表一時ファイル  
           SELECT  SKT1100TMP-FILE ASSIGN  SKT1100TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT1100TMP-KEY
                                   FILE    STATUS  IS  STS-SKT1100.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    総括表一時ファイル  
       FD  SKT1100TMP-FILE.
           COPY    "CPSKT1100TMP.INC".
      *
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求額欄に印字しない公費番号データ
           COPY    "KOHI1100.INC".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    総括表一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT1100TP//.
      *
      *    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *     
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    印刷用領域
           COPY    "SKT1100.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SKT1100         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-KOHI            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT1100TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-NG              PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-RECE02          PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-TOKUBETU        PIC 9(01).
           03  FLG-2010            PIC 9(01).
           03  FLG-KOHI            PIC 9(01).
           03  FLG-TAISYOGAI       PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX-O               PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
       01  GOUKEI-AREA.
           03  KOKUHO-TAL.
               05  WRK-KKHKENSU       PIC 9(05).
               05  WRK-KKHNISSU       PIC 9(05).
               05  WRK-KKHTENSU       PIC 9(09).
               05  WRK-KKHSEIKYUGK    PIC 9(09).
           03  KOUHI-TAL.
               05  WRK-KOHKENSU         PIC 9(05).
               05  WRK-KOHNISSU         PIC 9(05).
               05  WRK-KOHTENSU         PIC 9(09).
               05  WRK-KOHSEIKYUGK      PIC 9(09).
      *
           03  WRK-KOHNUM               PIC X(03).
           03  WRK-SEIKYUGK-G           PIC 9(07).
           03  WRK-HEIKIN               PIC 9(07)V9(1).
           03  WRK-HEIKIN-Z             PIC ZZZ,ZZZ.Z.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD        PIC X(08).
           03  WRK-SEIYMDWH      PIC X(20).
           03  WRK-SRYYMWH       PIC X(16).
           03  WRK-SRYYMD        PIC X(08).
      *
           03   WRK-KEISAN1      PIC S9(08)V9(01).
           03   WRK-KEISAN1-G    PIC S9(08).
           03   WRK-KEISAN2      PIC S9(08)V9(01).
           03   WRK-KEISAN2-G    PIC S9(08).
      *  
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z92               PIC ZZZZZZZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR           PIC X(200). 
      *
           03  WRK-HC01PARA-CNT      PIC  9(06).
      *
           03  WRK-KBN               PIC  9(02).
           03  WRK-KOHNAME           PIC  X(10).
           03  WRK-KENNAME           PIC  X(08).
      *
      *    請求管理情報参照用
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
           03  WRK-PATH3               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-KOUKI-5             PIC 9(10).
           03  WRK-KOUKI-6             PIC 9(10).
           03  WRK-KOUKI-9             PIC 9(10).
           03  WRK-KOHID             PIC 9(10).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
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
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-RJ              PIC 9(08).
      *
       01  HEN-AREA.
           03  HEN-KYURATE-AREA.
               05  FILLER            PIC  X(01) VALUE "(".
               05  HEN-KYURATE       PIC  X(02).
               05  FILLER            PIC  X(02) VALUE "割".
               05  FILLER            PIC  X(01) VALUE ")".
      *
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL          OCCURS  100.
               05  WRK-2007-MEISYO   PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX          PIC 9(03).
      *
       01  WRK-2010-AREA.
           03  WRK-2010-TBL          OCCURS  100.
               05  WRK-2010-KOHNUM   PIC X(03).
           03  WRK-2010-MAX          PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE              PIC 9(03).
           03  CNT-RECE10            PIC 9(06).
           03  CNT-UPDFILE           PIC 9(06).
           03  CNT-LINE              PIC 9(02).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-GOKEI-TBL.
               05  SUM-KENSU         PIC 9(05).
               05  SUM-NISSU         PIC 9(05).
               05  SUM-TENSU         PIC 9(09).
               05  SUM-SEIKYUGK      PIC 9(09).
      *
           COPY    "COMMON-SPA".
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ＪＯＢ管理ＤＢ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
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
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
      *
      *    地方公費保険番号付加情報
           COPY    "CPSK2010.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      ******************************************************************
      *    サブプロ用  領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".          
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主    処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
      *
           PERFORM  300-END-SEC
      *
           STOP     RUN
      *
           .
      *
      *****************************************************************
      *    初期  処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  SUM-AREA
           INITIALIZE  GOUKEI-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-PARA
           INITIALIZE  SPA-AREA
           INITIALIZE  KOHI-DAT
           INITIALIZE  RECEERR
      *
           PERFORM    100-DBOPEN-SEC
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
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
           MOVE    "SKT1100"       TO  JOB-PGID
           MOVE    "埼玉国保総括表(無床診療所用)"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT1100"           TO  SKT1100TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT1100TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT1100TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT1100TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT1100TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
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
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    公費番号まとめ情報
           INITIALIZE                      WRK-2007-AREA
      *
           INITIALIZE                      SYS-2007-REC
           MOVE    "2007"              TO  SYS-2007-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2007-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2007-STYUKYMD (7:2)
           MOVE    SYS-2007-STYUKYMD   TO  SYS-2007-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2007-HOSPNUM
           MOVE    SYS-2007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX1
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX1           >=  100
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX1
               MOVE    SYS-2007-MEISYO(1) TO  WRK-2007-MEISYO(IDX1)
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       10
                       OR      SYS-2007-KOHNUM (IDX2)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM (IDX2)
                                           TO  WRK-2007-KOHNUM(IDX1 IDZ)
               END-PERFORM
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       20
                       OR      SYS-2007-KOHNUM1(IDX2)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM1(IDX2)
                                           TO  WRK-2007-KOHNUM(IDX1 IDZ)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    IDX1                    TO  WRK-2007-MAX
      *
      *    請求書公費再掲欄記載情報
           INITIALIZE                      WRK-2010-AREA
      *
           INITIALIZE                      SYS-2010-REC
           MOVE    "2010"              TO  SYS-2010-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2010-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2010-STYUKYMD (7:2)
           MOVE    SYS-2010-STYUKYMD   TO  SYS-2010-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2010-HOSPNUM
           MOVE    SYS-2010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX            >=  100
               MOVE    MCPDATA-REC         TO  SYS-2010-REC
               IF    ( SYS-2010-SAIKEIKBN  =   "1"         )
               AND   ( SYS-2010-SAIKEI-TAIKBN
                                           =   "0" OR  "1" )
                   ADD     1                   TO  IDX
                   MOVE    SYS-2010-KBNCD(1:3) TO WRK-2010-KOHNUM (IDX)
               END-IF
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    IDX                 TO  WRK-2010-MAX
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           OPEN     OUTPUT  UPD-FILE
           EVALUATE    WRK-PARA-SYOKBN 
               WHEN    "2"
                   MOVE    "key37"        TO  WRK-PATH2
                   MOVE    "key65"                  TO  WRK-PATH3
               WHEN    "1"
                   MOVE    "key38"        TO  WRK-PATH2
                   MOVE    "key66"                  TO  WRK-PATH3
           END-EVALUATE
      *
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU1100"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    請求額印字対象外公費番号
           MOVE    "KOHI"                    TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  KOHI-KOHNUM(IDX)
             DISPLAY "公費番号="  KOHI-KOHNUM(IDX)
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
      *    主    処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           OPEN     OUTPUT  SKT1100TMP-FILE
           CLOSE            SKT1100TMP-FILE
           OPEN     I-O     SKT1100TMP-FILE
      *
           INITIALIZE             SUM-AREA
      *
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
      *    後期高齢
           MOVE     ZERO    TO        FLG-END
           MOVE     1       TO        FLG-KOUKI
           MOVE    "039"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
      *
      *    国保合計・公費合計書き出し
           PERFORM 2203-SYORI-SEC
      *
      *    特別療養費集計
           PERFORM  2004-TOKUBETU-SEC
      *
      *    特別医療費（後期分）集計
           PERFORM  2004-TOKUBETU-KOU-SEC
      *
           CLOSE   SKT1100TMP-FILE
      *
           IF       FLG-UPD   =         1
                    MOVE      ZERO      TO        WRK-KBN
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           CLOSE   UPD-FILE
           CLOSE   SKT1100TMP-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了    処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      CNT-RECE10          =   ZERO 
                   CONTINUE
           ELSE
                   IF      FLG-UPD     =   1
                           PERFORM     3001-SEIKYU-UPDATE-SEC
                   END-IF
           END-IF    
      *
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU1100 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU1100 END ***"
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

           PERFORM 900-DBDISCONNECT-SEC
      *
           .
      *
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号別集計処理                        
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC               SECTION.
      *
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT1100TMP-REC
                            WRK-KENNAME
      *
      *    県内は保険番号ごと、県外は都道府県ごとに集計
           IF  RECE10-PREFKBN  =  "1"
               MOVE    RECE10-HKNJANUM-S   TO  SKT1100TMP-HKNJANUM  
      *
      *        区の場合代表番号セット
               IF      SKT1100TMP-HKNJANUM(1:3)    =   "114"
                   MOVE    "114009"    TO  SKT1100TMP-HKNJANUM
               END-IF
           ELSE
      *        保険者番号から県番号を取得
               IF  RECE10-HKNJANUM(7:2)  =  SPACE
                   MOVE  RECE10-HKNJANUM(1:2)  TO  SKT1100TMP-HKNJANUM
               ELSE
                   MOVE  RECE10-HKNJANUM(3:2)  TO  SKT1100TMP-HKNJANUM
               END-IF
      *        県名セット
               PERFORM  32061-KENNAME-SET-SEC
               MOVE    WRK-KENNAME             TO  SKT1100TMP-HKNJANAME
           END-IF
      *
           IF  RECE10-HKNJANUM  =  "133033" OR "133298"
               MOVE    "13"                    TO  SKT1100TMP-HKNJANUM
               MOVE    "東京都"                TO  SKT1100TMP-HKNJANAME
           END-IF
      *
      *    老人保険・退職者医療の場合
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               INITIALIZE          SKT1100TMP-KEY
               MOVE    1       TO  SKT1100TMP-KBN1
           ELSE
               IF      RECE10-HKNNUM   =   "067"
                   INITIALIZE          SKT1100TMP-KEY
                   MOVE    2       TO  SKT1100TMP-KBN1
               ELSE
      *
      *            前期高齢特例措置か
                   IF   RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
                   AND  RECE10-FLG (2)  =  1
                       MOVE  8                 TO  RECE10-KYURATE
                   END-IF
      *
      *            組合のみ給付割合別
                   IF      SKT1100TMP-HKNJANUM(3:1)    =   "3"
                       MOVE    RECE10-KYURATE  TO  SKT1100TMP-KYURATE
                   ELSE
                       MOVE    ZERO            TO  SKT1100TMP-KYURATE
                   END-IF
                   MOVE    6       TO  SKT1100TMP-KBN1
               END-IF
           END-IF
      *
      *    災害該当分
           IF  RECE10-ETC-YKZFTN(1)  =  1 OR 2 OR 4 OR 5 OR 7 OR 8
             INITIALIZE          SKT1100TMP-KEY
             MOVE    7       TO  SKT1100TMP-KBN1
      *
      *      県ごとに集計
             IF  RECE10-HKNJANUM(7:2)  =  SPACE
               MOVE  RECE10-HKNJANUM(1:2)  TO  SKT1100TMP-HKNJANUM
             ELSE
               MOVE  RECE10-HKNJANUM(3:2)  TO  SKT1100TMP-HKNJANUM
             END-IF
           END-IF
      *
           IF  FLG-KOUKI  =  ZERO
               PERFORM 800-SKTTMP-READ-SEC
               PERFORM  220-HKN-SYUKEI-SEC
           ELSE
               IF  SKT1100TMP-KBN1  NOT =  7
                 MOVE    5    TO  SKT1100TMP-KBN1
               END-IF
               PERFORM  800-SKTTMP-READ-SEC
               PERFORM  220-KOUKI-SYUKEI-SEC
           END-IF
      *
      *    公費集計処理
           IF      RECE10-KOHNUM(1) NOT =  SPACE
               PERFORM 2201-KOH-SYUKEI-SEC
           END-IF
      *
      *    集計の対象にならなかったとき
           IF     FLG-UPD        =       1
                  PERFORM   240-UPD-FILE-HENSYU-SEC
      *
               IF  FLG-KOUKI  =  ZERO
                  PERFORM   900-SEIKYU-READ-SEC
               ELSE
                  PERFORM   950-SEIKYU-READ-SEC
               END-IF
           ELSE
                  MOVE      SPACE TO      WRK-RECEERR
                  STRING    "集計対象外  患者番号＝"
                                               DELIMITED  BY  SIZE
                            RECE10-PTNUM       DELIMITED  BY  SPACE
                            " 診療年月＝"      DELIMITED  BY  SIZE
                            RECE10-SRYYM       DELIMITED  BY  SIZE
                            " レセ種別＝"      DELIMITED  BY  SIZE
                            RECE10-RECESYUBETU DELIMITED  BY  SIZE
                         INTO                  WRK-RECEERR
                  END-STRING                                
                  PERFORM   500-ERR-HENSYU-SEC
                  MOVE      1         TO        FLG-END
           END-IF
      *
           .
      *
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       220-HKN-SYUKEI-SEC               SECTION.
      *
      *    件数
           ADD     1                   TO  SKT1100TMP-KENSU
      *
      *    日数
           ADD     RECE10-JNISSU(1)    TO  SKT1100TMP-NISSU
      *
      *    点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT1100TMP-TENSU
      *
           IF      SKT1100TMP-KBN1     =   6
      *
      *        請求額払の金額
               MOVE    ZERO            TO  WRK-SEIKYUGK-G
               COMPUTE WRK-SEIKYUGK-G  =
                                       RECE10-TOTALTEN(1)
                                   *   RECE10-KYURATE
               ADD     WRK-SEIKYUGK-G  TO  SKT1100TMP-SEIKYUGK
      *
      *       国保合計加算(老人：退職の場合は加算しない)
               ADD     RECE10-JNISSU(1)    TO  WRK-KKHNISSU
               ADD     RECE10-TOTALTEN(1)  TO  WRK-KKHTENSU
               ADD     1                   TO  WRK-KKHKENSU
               ADD     WRK-SEIKYUGK-G      TO  WRK-KKHSEIKYUGK
           END-IF
      *           
           IF      FLG-SKT1100TMP  =   1
               WRITE   SKT1100TMP-REC
           ELSE
               REWRITE SKT1100TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
       220-HKN-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       2201-KOH-SYUKEI-SEC               SECTION.
      *
      *    対象は全国共通公費と公費番号１４１のみ
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4   OR
                   RECE10-KOHNUM(IDX3)     =   SPACE
               MOVE    ZERO                  TO  FLG-2010
      *
      *        請求書公費再掲欄記載情報があるとき
               IF  WRK-2010-MAX  >  ZERO
                 PERFORM VARYING IDZ1    FROM    1   BY  1
                         UNTIL   IDZ1    >       WRK-2010-MAX
                   IF  RECE10-KOHNUM(IDX3)  =  WRK-2010-KOHNUM(IDZ1)
                     MOVE    1               TO  FLG-2010
                     MOVE    WRK-2010-MAX    TO  IDZ1
                   END-IF
                 END-PERFORM
               END-IF
      *
               IF  FLG-2010  =  ZERO
      *    公費番号まとめ情報があるとき
                 MOVE  SPACE     TO  WRK-KOHNAME
                 MOVE  RECE10-KOHNUM(IDX3)     TO  WRK-KOHNUM
                 IF  WRK-2007-MAX  >  ZERO
                   PERFORM VARYING IDZ1    FROM    1   BY  1
                           UNTIL   IDZ1    >       WRK-2007-MAX
                     PERFORM VARYING IDZ2  FROM    1   BY  1
                             UNTIL   IDZ2  >       20
                             OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                       IF  RECE10-KOHNUM(IDX3)  
                                     =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                         MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                     TO  RECE10-KOHNUM (IDX3)
                         MOVE  WRK-2007-MEISYO (IDZ1)
                                     TO  WRK-KOHNAME
                         MOVE  20            TO  IDZ2
                         MOVE  WRK-2007-MAX  TO  IDZ1
                       END-IF
                     END-PERFORM
                   END-PERFORM
                 END-IF
      *
                   PERFORM 2202-SYORI-SEC
               ELSE
                   MOVE    1                   TO  FLG-UPD
               END-IF
           END-PERFORM
           .
      *
       2201-KOH-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    公費集計処理
      *****************************************************************
       2202-SYORI-SEC               SECTION.
      *
      *    TMPキーセット
           INITIALIZE          SKT1100TMP-REC
           MOVE    8       TO  SKT1100TMP-KBN1
      *
           IF      RECE10-KOHNUM(IDX3) =   "091"
               MOVE    "051"               TO  SKT1100TMP-HKNJANUM
           ELSE
               MOVE    RECE10-KOHNUM(IDX3) TO  SKT1100TMP-HKNJANUM
           END-IF
      *
      *    一時ファイル読み込み
           PERFORM 800-SKTTMP-READ-SEC
      *
      *    公費名称
           MOVE    WRK-KOHNAME             TO  SKT1100TMP-KOHNAME
      *
      *    件数
           ADD     1                           TO   SKT1100TMP-KENSU
      *
      *    日数
           ADD     RECE10-JNISSU(IDX3 + 1)     TO  SKT1100TMP-NISSU
      *
      *    点数
           ADD     RECE10-TOTALTEN(IDX3 + 1)   TO SKT1100TMP-TENSU
      *
           MOVE    RECE10-KOHID(IDX3)      TO  WRK-KOHID
           MOVE    ZERO                    TO  WRK-SEIKYUGK-G
           MOVE    ZERO                    TO  WRK-KEISAN1
                                               WRK-KEISAN2
           PERFORM 950-HKNNUM-SET-SEC
      *
           IF      RECE10-NYUGAIKBN    =   "1"
               MOVE    "key33"         TO  WRK-PATH
           ELSE
               MOVE    "key25"         TO  WRK-PATH
           END-IF
           PERFORM 920-SYUNOU-SET-SEC
      *
      *    保険限度額
           IF   RECE10-FTNMONEY(1)  >  ZERO
           AND  RECE10-FTNMONEY(1)  <  WRK-KEISAN1
               MOVE    RECE10-FTNMONEY(1)  TO  WRK-KEISAN1
           END-IF
      *
           COMPUTE WRK-KEISAN1-G   ROUNDED =   WRK-KEISAN1
           COMPUTE WRK-KEISAN2-G   ROUNDED =   WRK-KEISAN2
           IF      WRK-KEISAN1-G   <=  WRK-KEISAN2-G
               MOVE    ZERO        TO  WRK-SEIKYUGK-G
           ELSE
               COMPUTE WRK-SEIKYUGK-G  =   WRK-KEISAN1-G
                                       -   WRK-KEISAN2-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-TAISYOGAI
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       KOHI-KOHNUM(IDXX)  =  SPACE
             IF  RECE10-KOHNUM(IDX3)  =  KOHI-KOHNUM(IDXX)
               MOVE    1               TO  FLG-TAISYOGAI
               MOVE    100             TO  IDXX
             END-IF
           END-PERFORM
           IF  FLG-TAISYOGAI  =  ZERO
             ADD     WRK-SEIKYUGK-G    TO  SKT1100TMP-SEIKYUGK
           END-IF
      *
      *    公費合計加算
           ADD     RECE10-JNISSU(IDX3 + 1)     TO  WRK-KOHNISSU
           ADD     RECE10-TOTALTEN(IDX3 + 1)   TO  WRK-KOHTENSU
           ADD     1                           TO  WRK-KOHKENSU
           IF  FLG-TAISYOGAI  =  ZERO
             ADD     WRK-SEIKYUGK-G            TO  WRK-KOHSEIKYUGK
           END-IF
      *           
           IF      FLG-SKT1100TMP     =          1
               WRITE   SKT1100TMP-REC
           ELSE
               REWRITE SKT1100TMP-REC
           END-IF
      *
           .
       2202-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    国保合計・公費合計書き出し
      *****************************************************************
       2203-SYORI-SEC               SECTION.
      *
      *    国保合計書き出し
      *    KEY設定
           INITIALIZE                  SKT1100TMP-REC
           MOVE    6               TO  SKT1100TMP-KBN1
           MOVE    1               TO  SKT1100TMP-KBN2
      *
           MOVE    KOKUHO-TAL      TO  SKT1100TMP-DATA
      *
           WRITE SKT1100TMP-REC
      *
      *    公費合計書き出し
      *    KEY設定
           INITIALIZE                  SKT1100TMP-REC
           MOVE    8               TO  SKT1100TMP-KBN1
           MOVE    1               TO  SKT1100TMP-KBN2
      *
           MOVE    KOUHI-TAL       TO  SKT1100TMP-DATA
      *
           WRITE SKT1100TMP-REC
           .
      *
       2203-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    特別療養費処理
      *****************************************************************
       2004-TOKUBETU-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH2            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           INITIALIZE    FLG-END
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "2"
                   MOVE    "key39"        TO  WRK-PATH2
               WHEN    "1"
                   MOVE    "key40"        TO  WRK-PATH2
           END-EVALUATE
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END = 1
      *
      *TMP-FILE キーセット
               INITIALIZE                  SKT1100TMP-REC
               MOVE    3               TO  SKT1100TMP-KBN1
      *
               PERFORM  800-SKTTMP-READ-SEC
               PERFORM  20041-TOKU-SYUKEI-SEC
      *
               PERFORM  900-SEIKYU-READ-SEC
           END-PERFORM
      *
           .
      *
       2004-TOKUBETU-EXT.
           EXIT.
      *****************************************************************
      *    特別療養費(後期分)処理
      *****************************************************************
       2004-TOKUBETU-KOU-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH2            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           INITIALIZE    FLG-END
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "2"
                   MOVE    "key65"        TO  WRK-PATH3
               WHEN    "1"
                   MOVE    "key66"        TO  WRK-PATH3
           END-EVALUATE
           MOVE    "040"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END = 1
      *
      *TMP-FILE キーセット
               INITIALIZE                  SKT1100TMP-REC
               MOVE    4               TO  SKT1100TMP-KBN1
      *
               PERFORM  800-SKTTMP-READ-SEC
               PERFORM  20041-TOKU-SYUKEI-SEC
      *
               PERFORM  950-SEIKYU-READ-SEC
           END-PERFORM
      *
           PERFORM  950-SEIKYU-CLOSE-SEC
      *
           .
      *
       2004-TOKUBETU-KOU-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者分集計処理
      *****************************************************************
       220-KOUKI-SYUKEI-SEC               SECTION.
      *
      *    件数
           ADD     1                   TO  SKT1100TMP-KENSU
      *    日数
           ADD     RECE10-JNISSU(1)    TO  SKT1100TMP-NISSU
      *    点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT1100TMP-TENSU
      *
           IF      FLG-SKT1100TMP  =   1
               WRITE   SKT1100TMP-REC
           ELSE
               REWRITE SKT1100TMP-REC
           END-IF
      ****
           MOVE    1               TO  FLG-UPD 
      ****
           .
       220-KOUKI-SYUKEI-EXT.
           EXIT.  
      *****************************************************************
      *    特別療養費処理
      *****************************************************************
       20041-TOKU-SYUKEI-SEC         SECTION.
      *
      *    件数
           ADD     1                   TO  SKT1100TMP-KENSU
      *    日数
           ADD     RECE10-JNISSU(1)    TO  SKT1100TMP-NISSU
      *    点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT1100TMP-TENSU
      *
           IF      FLG-SKT1100TMP  =   1
               WRITE   SKT1100TMP-REC
           ELSE
               REWRITE SKT1100TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
       20041-TOKU-SYUKEI-EXT.
           EXIT. 
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理             
      *****************************************************************
       240-UPD-FILE-HENSYU-SEC         SECTION.
      *
      *    集計の対象にならなかったとき                
           IF     FLG-UPD   NOT   =       1
                  MOVE      SPACE TO      WRK-RECEERR
                  STRING    "集計対象外  患者番号＝"
                                               DELIMITED  BY  SIZE
                            RECE10-PTNUM       DELIMITED  BY  SPACE
                            " 診療年月＝"      DELIMITED  BY  SIZE
                            RECE10-SRYYM       DELIMITED  BY  SIZE
                            " レセ種別＝"      DELIMITED  BY  SIZE
                            RECE10-RECESYUBETU DELIMITED  BY  SIZE
                         INTO                  WRK-RECEERR
                  END-STRING                                
                  PERFORM   500-ERR-HENSYU-SEC
                  MOVE      1         TO        FLG-END
                  GO        TO        240-UPD-FILE-HENSYU-EXT 
           END-IF
      *
           MOVE   RECE10-REC          TO        RECE10X-REC
           WRITE  RECE10X-REC
      *
           ADD    1                   TO        CNT-UPDFILE
      *
           .
      *
       240-UPD-FILE-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票ファイル出力処理                    
      *****************************************************************
       2002-PRINT-SEC                   SECTION.
      *
           OPEN    INPUT       SKT1100TMP-FILE
      *
           MOVE    ZERO    TO  FLG-PRINT
           INITIALIZE          SKT1100TMP-KEY
      *
      *    一時ファイル読み込み
           PERFORM 900-TMP-READ-SEC
      *
           PERFORM UNTIL   FLG-PRINT   =   1
      *
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *
      *        合計印字
               IF      FLG-PRINT   =   1
                   PERFORM 2002-LAST-SEC
               END-IF
      *
               PERFORM 330-PRINT-OUT-SEC
           END-PERFORM
      *
           .
      *
       2002-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    合計編集                    
      *****************************************************************
       2002-LAST-SEC                   SECTION.
      *
      *    療養の給付：件数
           MOVE    SUM-KENSU           TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-GKENSU
      *
      *    療養の給付：日数
           MOVE    SUM-NISSU           TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-GNISSU
      *
      *    療養の給付：点数
           MOVE    SUM-TENSU           TO  WRK-Z9
           MOVE    WRK-Z9              TO  SKT00-GTENSU
      *
      *    請求額払
           MOVE    SUM-SEIKYUGK        TO  WRK-Z10
           MOVE    WRK-Z10             TO  SKT00-GSEIKYUGK
      *
      *    平均点数追加
           COMPUTE WRK-HEIKIN  ROUNDED =   SUM-TENSU   /   SUM-KENSU 
           MOVE    WRK-HEIKIN          TO  WRK-HEIKIN-Z
           MOVE    WRK-HEIKIN-Z        TO  SKT00-HEIKIN
      *
      *    診療年月
           MOVE    LNK-PRTKANRI-SRYYM(5:2) TO  SKT00-LSRYYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  SKT00-LHOSPCD
      *
      *    請求額
           MOVE    SUM-SEIKYUGK            TO  WRK-Z92
           MOVE    WRK-Z92                 TO  SKT00-LSEIKYUGK
      * 
      *    
           .
      *
       2002-LAST-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC             SECTION.
      *
      *
           INITIALIZE                             SKT1100
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT00-SRYYM
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT00-HOSPADRS1
           INSPECT SKT00-HOSPADRS1         REPLACING ALL "  " BY "　"
      *
      *    電話番号
           MOVE    WRK-HOSPTEL             TO  SKT00-TEL1
      *
      *    ＦＡＸ
           MOVE    SYS-1002-FAX            TO  SKT00-FAX1
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT00-HOSPNAME1
           INSPECT SKT00-HOSPNAME1         REPLACING ALL "  " BY "　"
      *
      *    開設者氏名
           MOVE    WRK-KAISETUNAME         TO  SKT00-KAISETUNAME1
      *
      *    医療機関CD
           MOVE    SYS-1001-HOSPCDN        TO  SKT00-HOSPCDN
      *
           MOVE    SKT00-HOSP1             TO  SKT00-HOSP2
      *
           .
      *
       310-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<保険明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
      *
           MOVE    ZERO    TO  CNT-LINE
      *
           PERFORM UNTIL   FLG-PRINT   =   1
                   OR      CNT-LINE    =   17
      *
               EVALUATE    SKT1100TMP-KBN1
      *
      *            老人保健
                   WHEN    1
                       ADD     1       TO  CNT-LINE
                       PERFORM 3205-BODY-HEN-SEC
      *
      *            退職者医療
                   WHEN    2
                       PERFORM 3202-BODY-HEN-SEC
      *
      *            特別療養費
                   WHEN    3
                       ADD     1       TO  CNT-LINE
                       PERFORM 3205-BODY-HEN-SEC
      *
      *            特別療養費(後期分)
                   WHEN    4
                       ADD     1       TO  CNT-LINE
                       PERFORM 3205-BODY-HEN-SEC
                   WHEN    5
                       PERFORM 3201-BODY-HEN-SEC
      *
      *            一般国保
                   WHEN    6
                       ADD     1       TO  CNT-LINE
                       PERFORM 3203-BODY-HEN-SEC
      *
      *            災害該当分
                   WHEN    7
                       ADD     1       TO  CNT-LINE
                       PERFORM 3206-BODY-HEN-SEC
      *
      *            公費
                   WHEN    8
                       ADD     1       TO  CNT-LINE
                       PERFORM 3204-BODY-HEN-SEC
               END-EVALUATE
      *
      *        一時ファイル読み込み
               PERFORM 900-TMP-READ-SEC
      *
           END-PERFORM
      *
      *
           .
      *
       320-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<老人保健>処理
      *****************************************************************
       3201-BODY-HEN-SEC              SECTION.
      *
      *    療養の給付：件数
           ADD     SKT1100TMP-KENSU     TO  WRK-KOUKI-5
           MOVE    WRK-KOUKI-5      TO  WRK-Z5
           MOVE    WRK-Z5           TO  SKT00-RKENSU
      *
      *    療養の給付：日数
           ADD     SKT1100TMP-NISSU    TO  WRK-KOUKI-6
           MOVE    WRK-KOUKI-6     TO  WRK-Z5
           MOVE    WRK-Z5          TO  SKT00-RNISSU
      *
      *    療養の給付：点数
           ADD    SKT1100TMP-TENSU     TO  WRK-KOUKI-9
           MOVE   WRK-KOUKI-9          TO  WRK-Z9
           MOVE   WRK-Z9          TO  SKT00-RTENSU
      *
      *    合計加算
           ADD     SKT1100TMP-KENSU    TO  SUM-KENSU
      *
           ADD     SKT1100TMP-NISSU    TO  SUM-NISSU
      *
           ADD     SKT1100TMP-TENSU    TO  SUM-TENSU
      *
           .
      *
       3201-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<退職者医療>処理
      *****************************************************************
       3202-BODY-HEN-SEC              SECTION.
      *
      *    療養の給付：件数
           MOVE    SKT1100TMP-KENSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-TKENSU
      *
      *    療養の給付：日数
           MOVE    SKT1100TMP-NISSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-TNISSU
      *
      *    療養の給付：点数
           MOVE    SKT1100TMP-TENSU    TO  WRK-Z9
           MOVE    WRK-Z9              TO  SKT00-TTENSU
      *
      *    合計加算
           ADD     SKT1100TMP-KENSU    TO    SUM-KENSU
      *
           ADD     SKT1100TMP-NISSU    TO    SUM-NISSU
      *
           ADD     SKT1100TMP-TENSU    TO    SUM-TENSU
      *
           .
      *
       3202-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<特別療養費>処理
      *****************************************************************
       3205-BODY-HEN-SEC              SECTION.
      *
      *    療養の給付：件数
           MOVE    SKT1100TMP-KENSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-KENSU(CNT-LINE)
      *
      *    療養の給付：日数
           MOVE    SKT1100TMP-NISSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-NISSU(CNT-LINE)
      *
      *    療養の給付：点数
           MOVE    SKT1100TMP-TENSU    TO  WRK-Z9
           MOVE    WRK-Z9              TO  SKT00-TENSU(CNT-LINE)
      *
           EVALUATE SKT1100TMP-KBN1
             WHEN  1
              MOVE  "老人保健（月遅れ分）" TO  SKT00-NAME(CNT-LINE)
             WHEN  3
              MOVE  "特別療養費"           TO  SKT00-NAME(CNT-LINE)
             WHEN  4
              MOVE  "特別療養費(後期分)"   TO  SKT00-NAME(CNT-LINE)
           END-EVALUATE
      *
      *    合計加算
           ADD     SKT1100TMP-KENSU    TO  SUM-KENSU
      *
           ADD     SKT1100TMP-NISSU    TO  SUM-NISSU
      *
           ADD     SKT1100TMP-TENSU    TO  SUM-TENSU
      *
           .
      *
       3205-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<一般国保>処理
      *****************************************************************
       3203-BODY-HEN-SEC              SECTION.
      *
      *    療養の給付：件数
           MOVE    SKT1100TMP-KENSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-KENSU(CNT-LINE)
      *
      *    療養の給付：日数
           MOVE    SKT1100TMP-NISSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-NISSU(CNT-LINE)
      *
      *    療養の給付：点数
           MOVE    SKT1100TMP-TENSU    TO  WRK-Z9
           MOVE    WRK-Z9              TO  SKT00-TENSU(CNT-LINE)
      *
      *    請求額払の金額
           MOVE    SKT1100TMP-SEIKYUGK TO  WRK-Z10
           MOVE    WRK-Z10             TO  SKT00-SEIKYUGK(CNT-LINE)
      *
      *    国保計の時だけ加算
           IF      SKT1100TMP-KBN2     =   1
      *
               MOVE    "国保計"        TO  SKT00-NAME(CNT-LINE)
      *
      *        合計加算
               ADD     SKT1100TMP-KENSU    TO  SUM-KENSU
      *
               ADD     SKT1100TMP-NISSU    TO  SUM-NISSU
      *
               ADD     SKT1100TMP-TENSU    TO  SUM-TENSU
      *
               ADD     SKT1100TMP-SEIKYUGK TO  SUM-SEIKYUGK
           ELSE
      *
      *        保険者名
             IF  SKT1100TMP-HKNJANAME  NOT =  SPACE
               MOVE    SKT1100TMP-HKNJANAME
                                           TO  SKT00-NAME(CNT-LINE)
             ELSE
               INITIALIZE  HKNJAINF-REC
               MOVE    SKT1100TMP-HKNJANUM TO  HKNJA-HKNJANUM
               PERFORM 900-HKNJAINF-INV-SEC
               IF      HKNJA-HKNJANAME =   SPACE
                   MOVE    HKNJA-HKNJANUM  TO  WRK-MAE-INPUT
                   PERFORM 2009-HENKAN-SEC
                   MOVE    WRK-OUT-INPUT   TO  SKT00-NAME(CNT-LINE)
               ELSE
                   MOVE    HKNJA-HKNJANAME TO  SKT00-NAME(CNT-LINE)
               END-IF
             END-IF
      *
      *        給付割合
               IF      SKT1100TMP-KYURATE  NOT =   ZERO
                   MOVE    SKT1100TMP-KYURATE  TO  WRK-Z2
                   MOVE    WRK-Z2              TO  HEN-KYURATE
                   MOVE    HEN-KYURATE-AREA    TO  SKT00-WARI(CNT-LINE)
               END-IF
           END-IF
      *
           .
      *
       3203-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<公費>処理
      *****************************************************************
       3204-BODY-HEN-SEC              SECTION.
      *
      *    公費計の時だけ加算
           IF      SKT1100TMP-KBN2     =   1
      *
               MOVE    "公費計"        TO  SKT00-NAME(CNT-LINE)
      *
      *        合計加算
               COMPUTE  SKT1100TMP-SEIKYUGK = 
                        SKT1100TMP-SEIKYUGK - WRK-RJ
               ADD     SKT1100TMP-SEIKYUGK
                                       TO  SUM-SEIKYUGK
           ELSE
      *
      *        公費名
               INITIALIZE  HKNNUM-REC
               MOVE    SKT1100TMP-HKNJANUM  TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               IF  SKT1100TMP-HKNJANUM  =  "180"
                 MOVE    "老人医療"         TO  SKT00-NAME(CNT-LINE)(5:)
               ELSE
                 IF  SKT1100TMP-KOHNAME  NOT =  SPACE
                   MOVE  SKT1100TMP-KOHNAME TO  SKT00-NAME(CNT-LINE)(5:)
                 ELSE
                   MOVE  HKN-TANSEIDONAME   TO  SKT00-NAME(CNT-LINE)(5:)
                 END-IF
               END-IF
      *
      *        法別番号
               MOVE    HKN-HBTNUM          TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:4)  TO  SKT00-NAME(CNT-LINE)(1:4)
           END-IF
      *
      *    療養の給付：件数
           MOVE    SKT1100TMP-KENSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-KENSU(CNT-LINE)
      *
      *    療養の給付：日数
           MOVE    SKT1100TMP-NISSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-NISSU(CNT-LINE)
      *
      *    療養の給付：点数
           MOVE    SKT1100TMP-TENSU    TO  WRK-Z9
           MOVE    WRK-Z9              TO  SKT00-TENSU(CNT-LINE)
      *
      *    請求額払の金額
      *  川越老人の場合、「請求額払の金額」欄には何も印字しない
      *  (公費計の「請求額払の金額」欄にも川越老人の分は含めない)
           IF  HKN-HBTNUM  = "80"
             MOVE  SKT1100TMP-SEIKYUGK TO  WRK-RJ
             MOVE  ZERO                TO  SKT1100TMP-SEIKYUGK
           END-IF
           IF SKT1100TMP-KENSU = 0
             INITIALIZE SKT00-SEIKYUGK(CNT-LINE)
           ELSE
             MOVE    SKT1100TMP-SEIKYUGK TO  WRK-Z10
             MOVE    WRK-Z10             TO  SKT00-SEIKYUGK(CNT-LINE)
           END-IF
      *
           MOVE  ZERO                      TO  HKN-HBTNUM
      *
           .
      *
       3204-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<災害該当分>処理
      *****************************************************************
       3206-BODY-HEN-SEC              SECTION.
      *
      *    療養の給付：件数
           MOVE    SKT1100TMP-KENSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-KENSU(CNT-LINE)
      *
      *    療養の給付：日数
           MOVE    SKT1100TMP-NISSU    TO  WRK-Z5
           MOVE    WRK-Z5              TO  SKT00-NISSU(CNT-LINE)
      *
      *    療養の給付：点数
           MOVE    SKT1100TMP-TENSU    TO  WRK-Z9
           MOVE    WRK-Z9              TO  SKT00-TENSU(CNT-LINE)
      *
      *    県名セット
           PERFORM  32061-KENNAME-SET-SEC
           STRING  WRK-KENNAME         DELIMITED BY SPACE
                   "（被災分）"        DELIMITED BY SIZE
           INTO    SKT00-NAME(CNT-LINE)
           END-STRING
      *
      *    合計加算
           ADD     SKT1100TMP-KENSU    TO  SUM-KENSU
      *
           ADD     SKT1100TMP-NISSU    TO  SUM-NISSU
      *
           ADD     SKT1100TMP-TENSU    TO  SUM-TENSU
      *
           .
      *
       3206-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    県名セット
      *****************************************************************
       32061-KENNAME-SET-SEC                SECTION.
      *
           EVALUATE  SKT1100TMP-HKNJANUM(1:2)
             WHEN  "01"  MOVE  "北海道"    TO  WRK-KENNAME
             WHEN  "02"  MOVE  "青森県"    TO  WRK-KENNAME
             WHEN  "03"  MOVE  "岩手県"    TO  WRK-KENNAME
             WHEN  "04"  MOVE  "宮城県"    TO  WRK-KENNAME
             WHEN  "05"  MOVE  "秋田県"    TO  WRK-KENNAME
             WHEN  "06"  MOVE  "山形県"    TO  WRK-KENNAME
             WHEN  "07"  MOVE  "福島県"    TO  WRK-KENNAME
             WHEN  "08"  MOVE  "茨城県"    TO  WRK-KENNAME
             WHEN  "09"  MOVE  "栃木県"    TO  WRK-KENNAME
             WHEN  "10"  MOVE  "群馬県"    TO  WRK-KENNAME
             WHEN  "11"  MOVE  "埼玉県"    TO  WRK-KENNAME
             WHEN  "12"  MOVE  "千葉県"    TO  WRK-KENNAME
             WHEN  "13"  MOVE  "東京都"    TO  WRK-KENNAME
             WHEN  "14"  MOVE  "神奈川県"  TO  WRK-KENNAME
             WHEN  "15"  MOVE  "新潟県"    TO  WRK-KENNAME
             WHEN  "16"  MOVE  "富山県"    TO  WRK-KENNAME
             WHEN  "17"  MOVE  "石川県"    TO  WRK-KENNAME
             WHEN  "18"  MOVE  "福井県"    TO  WRK-KENNAME
             WHEN  "19"  MOVE  "山梨県"    TO  WRK-KENNAME
             WHEN  "20"  MOVE  "長野県"    TO  WRK-KENNAME
             WHEN  "21"  MOVE  "岐阜県"    TO  WRK-KENNAME
             WHEN  "22"  MOVE  "静岡県"    TO  WRK-KENNAME
             WHEN  "23"  MOVE  "愛知県"    TO  WRK-KENNAME
             WHEN  "24"  MOVE  "三重県"    TO  WRK-KENNAME
             WHEN  "25"  MOVE  "滋賀県"    TO  WRK-KENNAME
             WHEN  "26"  MOVE  "京都府"    TO  WRK-KENNAME
             WHEN  "27"  MOVE  "大阪府"    TO  WRK-KENNAME
             WHEN  "28"  MOVE  "兵庫県"    TO  WRK-KENNAME
             WHEN  "29"  MOVE  "奈良県"    TO  WRK-KENNAME
             WHEN  "30"  MOVE  "和歌山県"  TO  WRK-KENNAME
             WHEN  "31"  MOVE  "鳥取県"    TO  WRK-KENNAME
             WHEN  "32"  MOVE  "島根県"    TO  WRK-KENNAME
             WHEN  "33"  MOVE  "岡山県"    TO  WRK-KENNAME
             WHEN  "34"  MOVE  "広島県"    TO  WRK-KENNAME
             WHEN  "35"  MOVE  "山口県"    TO  WRK-KENNAME
             WHEN  "36"  MOVE  "徳島県"    TO  WRK-KENNAME
             WHEN  "37"  MOVE  "香川県"    TO  WRK-KENNAME
             WHEN  "38"  MOVE  "愛媛県"    TO  WRK-KENNAME
             WHEN  "39"  MOVE  "高知県"    TO  WRK-KENNAME
             WHEN  "40"  MOVE  "福岡県"    TO  WRK-KENNAME
             WHEN  "41"  MOVE  "佐賀県"    TO  WRK-KENNAME
             WHEN  "42"  MOVE  "長崎県"    TO  WRK-KENNAME
             WHEN  "43"  MOVE  "熊本県"    TO  WRK-KENNAME
             WHEN  "44"  MOVE  "大分県"    TO  WRK-KENNAME
             WHEN  "45"  MOVE  "宮崎県"    TO  WRK-KENNAME
             WHEN  "46"  MOVE  "鹿児島県"  TO  WRK-KENNAME
             WHEN  "47"  MOVE  "沖縄県"    TO  WRK-KENNAME
           END-EVALUATE
      *
           .
       32061-KENNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                   SECTION.
      * 
           ADD  1  TO  CNT-PAGE
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
           MOVE   "SKT1100.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE    "国保総括表"    TO  SPRT-TITLE
             WHEN  "2"
               MOVE    "国保総括表［返戻分］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT1100             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           DISPLAY  "ORCSPRT call"
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
      *
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
      *
      *        ジョブ終了処理
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
           CLOSE   RECEERR-FILE
      *
           .
      *
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *   請求管理ＤＢ更新処理                       
      *****************************************************************
       3001-SEIKYU-UPDATE-SEC              SECTION.
      *
           OPEN     INPUT           UPD-FILE
      *
           PERFORM  900-UPD-FILE-READ-SEC
      *
           PERFORM  UNTIL   FLG-UPDFILE     =     1
              MOVE  RECE10X-KEY     TO    RECE10-KEY
              MOVE  RECE10-REC      TO    MCPDATA-REC
      *   
              PERFORM   900-SEIKYU-INV-SEC
              IF        FLG-SEIKYU  =     ZERO
                        MOVE     "2"    TO     RECE10-SKYKBN
                        MOVE     LNK-PRTKANRI-SRYYM
                                        TO     RECE10-SKYYM
                        MOVE     LNK-PRTKANRI-SKYYMD
                                        TO     RECE10-UPDYMD
                        MOVE     RECE10-REC
                                        TO     MCPDATA-REC
                        MOVE     "DBUPDATE"
                                        TO     MCP-FUNC
                        MOVE    "tbl_seikyu"    TO  MCP-TABLE
                        MOVE    "key"           TO  MCP-PATHNAME
                        CALL    "ORCDBMAIN"     USING
                                                MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
                        IF       MCP-RC =      ZERO
                                 PERFORM       900-UPD-FILE-READ-SEC
                        ELSE
                                 DISPLAY "UPD ERR KEY="  RECE10-KEY 
                                 MOVE    
                                "*** SOKATU1100 SEIKYU UPDATE ERROR ***"
                                           TO  WRK-RECEERR
                                 MOVE    RECE10-KEY     TO
                                         WRK-RECEERR(33:)
                                 PERFORM 500-ERR-HENSYU-SEC
                                 MOVE    1      TO      FLG-UPDFILE
                        END-IF
      *
              ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                       MOVE    "*** SOKATU1100 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY     TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1      TO      FLG-UPDFILE
              END-IF
           END-PERFORM
      *
           CLOSE   UPD-FILE
      *
           .
      *
       3001-SEIKYU-UPDATE-EXT.
           EXIT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
      *
           .
      *
       31012-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
           MOVE    SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM      TO  RECE10-SKYYM.
      *
           MOVE    SPACE                   TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"              TO  RECE10-XXEDHKNJANUM
      *
           MOVE    000000                  TO  RECE10-SRYYM
           MOVE    999999                  TO  RECE10-XXSRYYM
           MOVE    RECE10-REC              TO  MCPDATA-REC.
           MOVE    "DBSELECT"              TO  MCP-FUNC.
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH2           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF     MCP-RC        =       ZERO
                  PERFORM  900-SEIKYU-READ-SEC
           ELSE
                  MOVE    1    TO      FLG-END
           END-IF
      *
           .
      *
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF       MCP-RC  =     ZERO
               MOVE    MCPDATA-REC   TO      RECE10-REC
      *    入院データは対象外
               IF  RECE10-NYUGAIKBN  NOT = "2"
                   GO    TO    900-SEIKYU-READ-SEC
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *
      *    １４１を使用している患者は対象外にする
      *    (老人医療費請求書で保険分も含めて請求する為)
               PERFORM  900-KOHSKY-READ-SEC
               IF  RECE02-KOHNUM(1)  =  "141"
                OR RECE02-KOHNUM(2)  =  "141"
                OR RECE02-KOHNUM(3)  =  "141"
                OR RECE02-KOHNUM(4)  =  "141"
                OR RECE02-KOHNUM(1)  =  "180"
                OR RECE02-KOHNUM(2)  =  "180"
                OR RECE02-KOHNUM(3)  =  "180"
                OR RECE02-KOHNUM(4)  =  "180"
                   DISPLAY "１４１use=" RECE02-PTNUM
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
               DISPLAY "ok"
      *
               ADD     1             TO      CNT-RECE10   
           ELSE
               DISPLAY "NG"
               MOVE    1             TO      FLG-END
           END-IF
      *
           .
      *
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       900-SEIKYU-INV-SEC             SECTION.
      *
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       900-SEIKYU-INV-EXT.
           EXIT.
      *****************************************************************
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
      *
           .
      *
       900-UPD-FILE-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスター読込
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE  SPA-HOSPNUM               TO  HKNJA-HOSPNUM
           MOVE    HKNJAINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_hknjainf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       900-HKNJAINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    請求管理読込（後期高齢）
      *****************************************************************
       950-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
           MOVE   SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM      TO  RECE10-SKYYM
           MOVE   WRK-CONS-SRYYM-200804   TO  RECE10-SRYYM
           MOVE   999999                  TO  RECE10-XXSRYYM
           MOVE   WRK-HKNNUM                   TO  RECE10-HKNNUM-S
                                              RECE10-HKNNUM
      *
               MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH3               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 950-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               DISPLAY "SELECT ERR"
           END-IF
           .
      *
       950-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（後期高齢）
      *****************************************************************
       950-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH3           TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
               IF  RECE10-NYUGAIKBN  NOT = "2"
                   GO    TO    950-SEIKYU-READ-SEC
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  950-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF   
      *         
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
      *
       950-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH2            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       950-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH3            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    請求額払集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX)  =  WRK-KOHNUM
      *
      *    保険番号マスタのレセプト１円単位表示区分が１の場合は１０円単位
      *    但し、マスタの設定が１０円単位でも全額負担の場合は１円単位
      *    (主保険に限度額が有る場合は無条件で１０円単位)
      *
               IF     IDX  =    1
                 IF  RECE10-FTNMONEY(1) > ZERO
                   ADD  SYU-SYUCOMPFTN            TO  WRK-KEISAN1
                   ADD  SYU-KOH-COMPFTN(IDX)      TO  WRK-KEISAN2
                 ELSE
                   IF  (HKN-RECEFTNDSPKBN     =  "1"   AND
                        SYU-KOH-COMPFTN(IDX)  >  ZERO)
                     ADD  SYU-KOH-COMPFTN(IDX)    TO  WRK-KEISAN2
                   ELSE
                     ADD  SYU-KOH-COMPFTN-ENTANI(IDX)
                                                  TO  WRK-KEISAN2
                   END-IF
                   ADD    SYU-SYUCOMPFTN-ENTANI   TO  WRK-KEISAN1
                 END-IF
               ELSE
                 IF  RECE10-FTNMONEY(1) > ZERO
                   ADD  SYU-KOH-COMPFTN(IDX - 1)  TO  WRK-KEISAN1
                   ADD  SYU-KOH-COMPFTN(IDX)      TO  WRK-KEISAN2
                 ELSE
                   IF  (HKN-RECEFTNDSPKBN     =  "1"   AND
                        SYU-KOH-COMPFTN(IDX)  >  ZERO)
                     ADD  SYU-KOH-COMPFTN(IDX)    TO  WRK-KEISAN2
                   ELSE
                     ADD  SYU-KOH-COMPFTN-ENTANI(IDX)
                                                  TO  WRK-KEISAN2
                   END-IF
                   ADD    SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                                  TO  WRK-KEISAN1
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
             MOVE  RECE10-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE10-PTID        TO  SYU-PTID
             MOVE  RECE10-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)

             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    WRK-PATH           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_syunou"       TO  MCP-TABLE
               MOVE    WRK-PATH           TO  MCP-PATHNAME
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
      *
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE10-HKNCOMBI(1) OR
                                             RECE10-HKNCOMBI(2) OR
                                             RECE10-HKNCOMBI(3) OR
                                             RECE10-HKNCOMBI(4) OR
                                             RECE10-HKNCOMBI(5) OR
                                             RECE10-HKNCOMBI(6) OR
                                             RECE10-HKNCOMBI(7) OR
                                             RECE10-HKNCOMBI(8) OR
                                             RECE10-HKNCOMBI(9) OR
                                             RECE10-HKNCOMBI(10) 
                     PERFORM  930-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID (1)  =  WRK-KOHID   OR
                         COMB-KOHID (2)  =  WRK-KOHID   OR
                         COMB-KOHID (3)  =  WRK-KOHID   OR
                         COMB-KOHID (4)  =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_syunou"       TO  MCP-TABLE
                 MOVE    WRK-PATH           TO  MCP-PATHNAME
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
           MOVE  "DBFETCH"         TO  MCP-FUNC
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
      *    保険組合せマスター読み込み
      *****************************************************************
       930-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                         HKNCOMBI-REC
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
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC    TO  HKNCOMBI-REC
                   MOVE    ZERO           TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                 HKNCOMBI-REC
                   MOVE    1              TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                     HKNCOMBI-REC
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
            MOVE    "tbl_hkncombi"        TO  MCP-TABLE
            MOVE    "key"                 TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"           USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       930-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE    SPA-HOSPNUM              TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM               TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)  TO  HKN-TEKSTYMD(1:6)
                                                HKN-TEKEDYMD(1:6)
           MOVE    "01"                     TO  HKN-TEKSTYMD(7:2)
                                                HKN-TEKEDYMD(7:2)
           MOVE    HKNNUM-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"         TO  MCP-TABLE
               MOVE    "key5"               TO  MCP-PATHNAME
               PERFORM  945-HKNNUM-READ-SEC
      *
           ELSE
               DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       945-HKNNUM-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
      *
             IF    HKN-HKNNUM  =  "051"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "特定疾患"   TO   HKN-TANSEIDONAME
             END-IF
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1            TO  FLG-HKNNUM
           END-IF
      *
           .
       945-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT1100TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT1100TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT1100TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT1100TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
                                FLG-END
                   DISPLAY "END"
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求情報読み込み
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    RECE10-KEY              TO  RECE02-KEY
           MOVE    RECE02-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_kohsky"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE02
                   MOVE    MCPDATA-REC         TO  RECE02-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE02
                   INITIALIZE                  RECE02-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE02
               INITIALIZE                  RECE02-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-KOHSKY-READ-EXT.
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
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
      *    ＤＢ  オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ  クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
