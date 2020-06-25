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
       PROGRAM-ID.             SOKATU2800.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国民健康保険診療報酬総括票
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/16    山口     ORCAのバージョンアップ(2.5.0)による再作成
      *                      (dia→Monpe、Open cobolのバージョンアップ対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    山口    05/06/30    高齢重度障害者の件数を表示
      *                                  福祉件数の対象公費を追加
      *  01.00.02    山口    05/08/01    福祉件数の対象公費に(227)を追加
      *  01.00.03    山口    05/10/04    請求書枚数の集計方法修正
      *  01.00.04    山口    05/12/01    返戻対応
      *  01.00.05    内田    07/09/02    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.06    内田    07/12/05    レセ電時、レセ電国保請求書の枚数を集計
      *  01.00.07    内田    07/12/19    レセ電国保請求書は２枚１組で１枚とする
      *                                  レセ電国保請求書は県毎で帳票を分けて
      *                                  カウントする
      *  01.00.08    内田    08/02/23    同一保険者で複数診療月がある場合の
      *                                  請求書枚数の修正
      *  01.00.09    内田    08/04/22    帳票様式の変更に伴う修正
      *                                  SPA-AREA初期化
      *  01.00.10    内田    08/04/30    レセ電時は請求書枚数は印字しない
      *  01.00.11    内田    08/05/20    福祉件数の対象公費に"380"を追加
      *  01.00.12    内田    08/06/20    後期高齢のみの場合も印字する
      *  01.00.13    内田    08/07/11    県外保険者欄の後期分が国保分と
      *                                  同一の数しか印字されないのを修正
      *  01.00.14    吉川    08/11/04    後期高齢の請求書枚数のカウント修正
      *  01.00.15    吉川    09/07/09    福祉件数欄への対象公費番号の追加
      *                                 （480,580,680,780,382,482,385,
      *                                   485,585,343,443）
      *  01.00.16    吉川    10/04/06    オンライン返戻対応
      *  01.00.17    吉川    10/11/02    特別療養費国保対応
      *  01.00.18    吉川    10/11/15    複数診療月の返戻がある場合の
      *                                  請求書枚数のカウント修正
      *  01.00.19    吉川    10/11/24    open-cobol1.0対応
      *  01.00.20    吉川    11/10/27    対象公費番号をINIファイルから
      *                                  取得するように修正
      *  01.00.21    吉川    13/01/11    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.22    吉川    13/06/17    プログラムオプション対応
      *  01.00.23    吉川    13/09/03    医療機関区分の設定不具合修正
      *  01.00.24    吉川    14/10/24    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.25    吉川    14/11/12    県外保険者名は１２文字まで印字
      *                                  社保福祉医療費請求件数欄は
      *                                  ０件の場合はゼロを印字する
      *                                  返戻時、再印刷画面のタイトル修正
      *  01.00.26    門間    17/04/13    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.27    萩野    18/03/01    医療機関編集情報への開設者名追加対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.28    門間    18/07/06    後期高齢の返戻が複数診療月ある場合の
      *                                  請求書枚数のカウント修正
      *                                  記述漏れ対応
      *  01.00.29    萩野    19/03/08    修正履歴コメント削除
      *                                  システム管理マスタ「2005」を設定していない場合、
      *                                  処理がエラーとなるのを修正。
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
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
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
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
           COPY    "SKT2800.INC".
      *
      *    対象公費番号データ
           COPY    "TAISYO2805.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-SRYKA           PIC X(02).
           03  STS-HOSPKBN         PIC X(02).
           03  STS-TAISYOKOHI      PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-TAISYOKOHI      PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(02).
           03  IDXX                PIC 9(04).
           03  IDY                 PIC 9(04).
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
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD        PIC X(08).
           03  WRK-SEIYMDWH      PIC X(20).
           03  WRK-SRYYMWH       PIC X(16).
           03  WRK-SRYYMD        PIC X(08).
      *
      *    ファイル名称用エリア
           03  WRK-SRYKANAME    PIC X(34) OCCURS 3.
      *
           03  WRK-Z3            PIC ZZZ.
           03  WRK-Z3-9          PIC ZZ9.
           03  WRK-Z6            PIC ZZZZZZ.
           03  WRK-Z9            PIC ZZZZZZZZZ.
           03  WRK-HKNJANUM      PIC X(06).
           03  WRK-CNT           PIC 9(03).
           03  WRK-KENNAME       PIC X(10).
           03  WRK-HKNJANAME     PIC X(24).
           03  WRK-MAISU         PIC 9(03).
           03  WRK-AMARI         PIC 9(02).
           03  WRK-MAISUKEI      PIC 9(03).
           03  WRK-GMAISU        PIC 9(03).
      *
      *    請求書枚数カウント用エリア
           03  WRK-SEIKYUSYO-CNT-TBL OCCURS 200.
               05  WRK-SEIKYUSYO-KEY.
                   07  WRK-KEY-PREFKBN   PIC X(01).
                   07  WRK-KEY-PREFNUM   PIC X(02).
                   07  WRK-KEY-HKNJANUM  PIC X(06).
                   07  WRK-KEY-SRYYM     PIC X(06).
                   07  WRK-KEY-ZAITAKU   PIC X(01).
           03  WRK-CHK-KEY.
               05  WRK-CHK-PREFKBN       PIC X(01).
               05  WRK-CHK-PREFNUM       PIC X(02).
               05  WRK-CHK-HKNJANUM      PIC X(06).
               05  WRK-CHK-SRYYM         PIC X(06).
               05  WRK-CHK-ZAITAKU       PIC X(01).
      *
           03  WRK-PREFKENSU-TBL  OCCURS  47.
               05  WRK-PREFNUM           PIC X(02).
               05  WRK-PREFKENSU         PIC 9(03).
      *
      *    県外保険者数カウント用エリア
           03  WRK-HKN-TBL OCCURS 2.
               05  WRK-CNT-TBL OCCURS 200.
                   07  WRK-CNT-HKNJANUM  PIC X(06).
      *
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *    請求書枚数カウント（後期高齢）
       01  SCNT-AREA.
           03  SCNT-HKN-TBL  OCCURS  2.
               05  SCNT-DATA-TBL  OCCURS  50.
                   07  SCNT-SRYYM        PIC X(06).
                   07  SCNT-HKNJANUM     PIC X(06).
                   07  SCNT-ZAITAKU      PIC X(01).
                   07  SCNT-KOHSU        PIC 9(02).
                   07  SCNT-KOH-TBL  OCCURS  20.
                       09  SCNT-KOHNUM   PIC X(03).
      *
       01  WRK-POSTG.
           03  WRK-POSTMAE       PIC X(06).
           03  FILLER            PIC X(02) VALUE "−".
           03  WRK-POSTATO       PIC X(08).
      *  
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-KBN               PIC  9(02).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOHTENSU          PIC 9(08).
      *
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-KENNUM            PIC 9(02).
           03  WRK-KOHKEI            PIC 9(03).
           03  WRK-KOHKEI2           PIC 9(03).
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
           03  WRK-HOSPKBN.
               05  FILLER            PIC X(13).
               05  WRK-HOSPKBN-CD    PIC X(01).
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE              PIC 9(03).
           03  CNT-RECE10            PIC 9(06).
           03  CNT-UPDFILE           PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-SEIKYU-DATA        OCCURS  10.
               05  SUM-TENSU         PIC 9(09).
               05  SUM-KENSU         PIC 9(05).
           03  SUM-SEIKYUSYO-G    OCCURS  6.
               05  SUM-SEIKYUSYO     PIC 9(03).
           03  SUM-SYAHOSEIKYU       PIC 9(03).
           03  SUM-SYAHOSEIKYU2      PIC 9(03).
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "COMMON-SPA".
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM             PIC X(03)  OCCURS  4.
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ＪＯＢ管理ＤＢ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＪＯＢ管理ＤＢ（制御用）
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *    医療機関情報−レセプト・総括印刷情報
           COPY    "CPSK2005.INC".
      *    医療機関情報−レセプト・総括印刷情報（後期高齢）
           COPY    "CPSK200501.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE                   FLG-AREA
           INITIALIZE                   STS-AREA
           INITIALIZE                   WRK-AREA
           INITIALIZE                   CNT-AREA
           INITIALIZE                   SUM-AREA
           INITIALIZE                   IDX-AREA
           INITIALIZE                   SPA-AREA
           INITIALIZE                   SCNT-AREA
           INITIALIZE                   TAISYO-DAT
           INITIALIZE                   RECEERR
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
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-TOKUBETU
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU2800"    TO  JOB-PGID
           MOVE    "兵庫国保診療報酬総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID     TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
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
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE    "*"            TO      SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"           TO      SYS-1001-STYUKYMD(7:2)
                                          SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM    TO      SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF     FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO    SYS-1001-REC
                  IF      SYS-1001-HOSPNAME  =  LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-HOSPNAME
                  END-IF
                  IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-KAISETUNAME
                  END-IF    
           ELSE
                  MOVE    "医療機関情報が取得できませんでした"
                                          TO    WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO    FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                     SYS-1002-REC
           MOVE     "1002"        TO      SYS-1002-KANRICD
           MOVE     "*"           TO      SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE     "01"          TO      SYS-1002-STYUKYMD(7:2)
                                          SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM    TO      SYS-1002-HOSPNUM
           MOVE     SYS-1002-REC  TO      MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
                    MOVE    MCPDATA-REC   TO    SYS-1002-REC
                    IF      SYS-1002-ADRS =     LOW-VALUE
                            MOVE    SPACE TO    SYS-1002-ADRS
                    END-IF
           ELSE
                    MOVE "医療機関情報（所在地）が取得できませんでした"
                                          TO    WRK-RECEERR
                    PERFORM   500-ERR-HENSYU-SEC
                    MOVE      1           TO    FLG-END
           END-IF 
      *
      *    医療機関情報−レセプト・総括印刷情報
           INITIALIZE                     SYS-2005-REC
           MOVE     "2005"        TO      SYS-2005-KANRICD
           MOVE     "*"           TO      SYS-2005-KBNCD
           MOVE     LNK-PRTKANRI-SRYYM(1:6)
                                  TO      SYS-2005-STYUKYMD(1:6)
                                          SYS-2005-EDYUKYMD(1:6)
           MOVE     "01"          TO      SYS-2005-STYUKYMD(7:2)
                                          SYS-2005-EDYUKYMD(7:2)
           MOVE     SPA-HOSPNUM   TO      SYS-2005-HOSPNUM
           MOVE     SYS-2005-REC  TO      MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
               MOVE    MCPDATA-REC   TO    SYS-2005-REC
           ELSE
               INITIALIZE                     SYS-2005-REC
           END-IF 
      *
      *    医療機関情報−レセプト・総括印刷情報（後期高齢）
           INITIALIZE                     SYS-200501-REC
           MOVE     "2005"        TO      SYS-200501-KANRICD
           MOVE     "01"          TO      SYS-200501-KBNCD
           MOVE     LNK-PRTKANRI-SRYYM(1:6)
                                  TO      SYS-200501-STYUKYMD(1:6)
                                          SYS-200501-EDYUKYMD(1:6)
           MOVE     "01"          TO      SYS-200501-STYUKYMD(7:2)
                                          SYS-200501-EDYUKYMD(7:2)
           MOVE     SPA-HOSPNUM   TO      SYS-200501-HOSPNUM
           MOVE     SYS-200501-REC  TO      MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
               MOVE    MCPDATA-REC   TO    SYS-200501-REC
           ELSE
               INITIALIZE                     SYS-200501-REC
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
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           OPEN     OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
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
           MOVE    "SOKATU2800"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    医療機関区分
           MOVE    "HOSPKBN"                 TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:14)       TO  WRK-HOSPKBN
           DISPLAY "医療機関区分="  WRK-HOSPKBN-CD
      *
      *    診療科
           MOVE    "SRYKA"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             IF  WRK-OPTION(IDX)(1:2)  =  "※"
               MOVE    SPACE                 TO  WRK-SRYKANAME(IDX)
             ELSE
               MOVE    WRK-OPTION(IDX)(1:34) TO  WRK-SRYKANAME(IDX)
             END-IF
           END-PERFORM
           DISPLAY "診療科１="  WRK-SRYKANAME(1)
           DISPLAY "診療科２="  WRK-SRYKANAME(2)
           DISPLAY "診療科３="  WRK-SRYKANAME(3)
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
      *    主    処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
      *    データ集計処理
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
           IF  WRK-PARA-TOKUBETU  =  "1"
             MOVE     ZERO    TO        FLG-END
             MOVE     1       TO        FLG-KOUKI
             MOVE    "039"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           ELSE
             MOVE     1       TO        FLG-KOUKI
             MOVE     ZERO    TO        FLG-END
             MOVE    "040"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           END-IF
      *
      *    社保福祉件数集計処理
           PERFORM  2001-KOHI-SYUKEI-SEC
      *
      *    プリント処理
           IF  FLG-UPD   =       1
               PERFORM   2001-PRINT-SEC
           END-IF
           MOVE      1      TO     FLG-END
      *
           CLOSE   UPD-FILE
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
           DISPLAY "*** SOKATU2800 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU2800 END ***"
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
      *
           .
      *
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号別集計処理                        
      *****************************************************************
       2001-SYUKEI-SEC               SECTION.
      *
      *    データ加算処理
           IF  FLG-KOUKI  =  ZERO
               PERFORM  2002-SYORI-SEC
           ELSE
               PERFORM  2002-KOUKI-SYORI-SEC
           END-IF
      *
           MOVE  1              TO   FLG-UPD
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
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ加算処理
      *****************************************************************
       2002-SYORI-SEC               SECTION.
      *
      *    集計場所設定
           IF     RECE10-PREFKBN      =  "1"
             IF     RECE10-NYUGAIKBN  =  "1"
               MOVE     1     TO     IDX1
             ELSE
               MOVE     2     TO     IDX1
             END-IF
             MOVE       1     TO     IDX2
           ELSE
             IF     RECE10-NYUGAIKBN  =  "1"
               MOVE     3     TO     IDX1
             ELSE
               MOVE     4     TO     IDX1
             END-IF
             MOVE       2     TO     IDX2
           END-IF
      *
      *    件数加算
           ADD          1                 TO     SUM-KENSU(IDX1)
                                                 SUM-KENSU(5)
      *
      *    点数加算
           ADD     RECE10-TOTALTEN(1)     TO     SUM-TENSU(IDX1)
                                                 SUM-TENSU(5)
      *
      *    請求書件数チェック処理
           PERFORM  20021-SEIKYUSYO-CHK-SEC
      *
           .
      *
       2002-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ加算処理（後期高齢）
      *****************************************************************
       2002-KOUKI-SYORI-SEC               SECTION.
      *
      *    集計場所設定
           IF     RECE10-PREFKBN      =  "1"
             IF     RECE10-NYUGAIKBN  =  "1"
               MOVE     6     TO     IDX1
             ELSE
               MOVE     7     TO     IDX1
             END-IF
             MOVE       6     TO     IDX2
           ELSE
             IF     RECE10-NYUGAIKBN  =  "1"
               MOVE     8     TO     IDX1
             ELSE
               MOVE     9     TO     IDX1
             END-IF
             MOVE       8     TO     IDX2
           END-IF
      *
      *    件数加算
           ADD          1                 TO     SUM-KENSU(IDX1)
                                                 SUM-KENSU(10)
      *
      *    点数加算
           ADD     RECE10-TOTALTEN(1)     TO     SUM-TENSU(IDX1)
                                                 SUM-TENSU(10)
      *
      *    請求書件数チェック処理
           PERFORM  20021-KOU-SEIKYUSYO-CHK-SEC
      *    県外保険者種類取得(保険者番号のみで判定)
           IF     RECE10-PREFKBN    =    "2"
             MOVE    ZERO      TO    FLG-OK
             PERFORM  VARYING  IDX3 FROM  1  BY  1
                      UNTIL    IDX3 >     200
                      OR       FLG-OK     =   1
               IF     WRK-CNT-HKNJANUM(2 IDX3)(1:4)
                      =   RECE10-HKNJANUM(1:4)
                 MOVE          1              TO  FLG-OK
               ELSE
                 IF   WRK-CNT-HKNJANUM(2 IDX3)  =   SPACE
                   MOVE        1              TO  FLG-OK
                   MOVE   RECE10-HKNJANUM (1:4)
                          TO  WRK-CNT-HKNJANUM(2 IDX3)(1:4)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
      *
       2002-KOUKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書件数チェック処理
      *****************************************************************
       20021-SEIKYUSYO-CHK-SEC        SECTION.
      *
      *    KEY設定(保険者番号・診療年月・在宅フラグで判定)
           INITIALIZE          WRK-CHK-KEY
           MOVE     RECE10-HKNJANUM-S      TO    WRK-CHK-HKNJANUM
           MOVE     RECE10-ZAITAKU         TO    WRK-CHK-ZAITAKU
      *    レセ電と紙レセで条件が異なる
           IF   SYS-2005-KKHRECEKBN  NOT =  ZERO
           AND  WRK-PARA-SYOKBN      NOT =  2
               IF  RECE10-PREFKBN  =  1
                   MOVE  RECE10-PREFKBN    TO    WRK-CHK-PREFKBN
               ELSE
                   MOVE  RECE10-PREFNUM    TO    WRK-CHK-PREFNUM
               END-IF
               MOVE  RECE10-HKNJANUM-S     TO    WRK-CHK-HKNJANUM
           ELSE
               MOVE  RECE10-HKNJANUM-S     TO    WRK-CHK-HKNJANUM
               MOVE  RECE10-ZAITAKU        TO    WRK-CHK-ZAITAKU
           END-IF
      *
      *    神戸市は代表番号で括る
           IF       WRK-CHK-HKNJANUM(1:3)  =     "284"
             MOVE   "284000"               TO    WRK-CHK-HKNJANUM
           END-IF
      *
           MOVE     ZERO     TO     FLG-OK
           PERFORM  VARYING  IDX3 FROM  1  BY  1
                   UNTIL    IDX3 >     200
                    OR       FLG-OK    =   1
            IF     WRK-SEIKYUSYO-KEY(IDX3)  =  WRK-CHK-KEY
               MOVE     1     TO     FLG-OK
             ELSE
               IF   WRK-SEIKYUSYO-KEY(IDX3)  =  SPACE
                MOVE  WRK-CHK-KEY       TO WRK-SEIKYUSYO-KEY(IDX3)
                ADD   1                     TO SUM-SEIKYUSYO(IDX2)
                                                SUM-SEIKYUSYO(3)
                 MOVE  1                     TO FLG-OK
               END-IF
             END-IF
           END-PERFORM
      *
      *    県外保険者種類取得(保険者番号のみで判定)
           IF     RECE10-PREFKBN    =    "2"
             MOVE    ZERO      TO    FLG-OK
             PERFORM  VARYING  IDX3 FROM  1  BY  1
                      UNTIL    IDX3 >     200
                      OR       FLG-OK     =   1
               IF     WRK-CNT-HKNJANUM(1 IDX3)  =   WRK-CHK-HKNJANUM
                 MOVE          1              TO  FLG-OK
               ELSE
                 IF   WRK-CNT-HKNJANUM(1 IDX3)  =   SPACE
                   MOVE        1              TO  FLG-OK
                   MOVE   WRK-CHK-HKNJANUM    
                                      TO  WRK-CNT-HKNJANUM(1 IDX3)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
      *
       20021-SEIKYUSYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書件数チェック処理
      *****************************************************************
       20021-KOU-SEIKYUSYO-CHK-SEC      SECTION.
      *
           IF  RECE10-PREFKBN  =  1
               MOVE   1  TO  IDX1
           ELSE
               MOVE   2  TO  IDX1
           END-IF
           MOVE  RECE10-HKNJANUM-S (1:4)  TO  WRK-HKNJANUM
      *
      *    重複データか
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                     UNTIL    IDX2  >     50
                     OR       SCNT-HKNJANUM (IDX1 IDX2)  =  SPACE
               IF   (SCNT-HKNJANUM (IDX1 IDX2)  =  WRK-HKNJANUM)
               AND  (SCNT-ZAITAKU  (IDX1 IDX2)  =  RECE10-ZAITAKU)
               AND  (SCNT-SRYYM    (IDX1 IDX2)  =  RECE10-SKYYM)
                   MOVE  IDX2  TO  WRK-CNT
                   MOVE  50  TO  IDX2
               END-IF
           END-PERFORM
      *
           IF  IDX2  <  50
               MOVE  IDX2             TO  WRK-CNT
               MOVE  WRK-HKNJANUM     TO  SCNT-HKNJANUM (IDX1 WRK-CNT)
               MOVE  RECE10-ZAITAKU   TO  SCNT-ZAITAKU (IDX1 WRK-CNT)
               MOVE  RECE10-SKYYM     TO  SCNT-SRYYM (IDX1 WRK-CNT)
           END-IF
      *    重複公費か
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     4
                    OR       RECE10-KOHNUM (IDX3) =   SPACE
      *
      *       ０５１と０９１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX3) =  "091"
                   MOVE    "051"       TO  RECE10-KOHNUM(IDX3)
               END-IF
      *        公費判別
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL   IDX4    >       20
                       OR      SCNT-KOHNUM(IDX1 WRK-CNT IDX4) = SPACE
                   IF  RECE10-KOHNUM (IDX3)
                       =   SCNT-KOHNUM (IDX1 WRK-CNT IDX4)
                       MOVE    20      TO  IDX4
                   END-IF
               END-PERFORM
      *        公費重複でない場合は格納
               IF  IDX4  <  20
                   MOVE  RECE10-KOHNUM (IDX3)
                         TO  SCNT-KOHNUM (IDX1 WRK-CNT IDX4)
                   ADD   1     TO  SCNT-KOHSU (IDX1 WRK-CNT)
               END-IF
           END-PERFORM
      *
           .
      *
       20021-KOU-SEIKYUSYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費医療点数集計処理
      *****************************************************************
       2001-KOHI-SYUKEI-SEC                SECTION.
      *
           MOVE    ZERO        TO    FLG-END
      *
           PERFORM 900-KOHSKY-START-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
      *  兵庫県では全公と地公は併用させないという事なので、地方公費
      *  を持っていれば無条件でカウントする
             IF   WRK-PARA-SYOKBN   =  1
               PERFORM VARYING IDX3 FROM    1   BY  1
                       UNTIL   IDX3 >       4
                 INITIALIZE                            TBL-HIKAKU-KOHI
                 MOVE    RECE02-KOHNUM(IDX3)       TO  TBL-HKOHNUM(1)
      *          対象公費番号チェック処理
                 PERFORM  20011-TAISYOKOHI-CHK-SEC
                 IF  FLG-TAISYO  =  1
                   ADD     1         TO      SUM-SYAHOSEIKYU
                 ELSE
                   IF  RECE02-KOHNUM (IDX3) = "127" OR "227"
                     ADD     1         TO      SUM-SYAHOSEIKYU2
                   END-IF
                 END-IF
               END-PERFORM
             ELSE
               DISPLAY  "返戻 患者番号 = "  RECE02-PTNUM
               DISPLAY  "WRK-PARA-SYOKBN = " WRK-PARA-SYOKBN
             END-IF
      *
             PERFORM  900-KOHSKY-READ-SEC
           END-PERFORM
      *
           .
       2001-KOHI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       20011-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       TAISYO-KOHNUM(IDXX)  =  SPACE
             IF  TAISYO-KOHNUM(IDXX)  =  TBL-HKOHNUM(1)
                                     OR  TBL-HKOHNUM(2)
                                     OR  TBL-HKOHNUM(3)
                                     OR  TBL-HKOHNUM(4)
               MOVE     1                   TO  FLG-TAISYO
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       20011-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
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
       2001-PRINT-SEC                   SECTION.
      *
      *集計データ移行
           MOVE     1      TO        WRK-CNT
           MOVE     ZERO   TO        FLG-OK
           PERFORM  UNTIL     FLG-OK  =   1
                    AND       WRK-CNT-HKNJANUM(1 WRK-CNT) = SPACE
                    AND       WRK-CNT-HKNJANUM(2 WRK-CNT) = SPACE
      *
      *    帳票編集<見出し>処理
             PERFORM  310-HEAD-HEN-SEC
      *
      *    帳票編集(明細)処理
             PERFORM  320-BODY-HEN-SEC
      *
      *    帳票編集(印刷)処理
             PERFORM  330-PRINT-OUT-SEC
             MOVE     1        TO     FLG-OK
             DISPLAY  "県外保険者２="   WRK-CNT-HKNJANUM(1 WRK-CNT)
             DISPLAY "カウント２="  WRK-CNT
           END-PERFORM
      *
           .
      *
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC             SECTION.
      *
           INITIALIZE                             SKT2800
                                                  WRK-MAISUKEI
      *
           MOVE     WRK-SRYYMWH              TO   SKT2800-SRYYM
      *
      *    医療機関住所
           MOVE     WRK-HOSPADRS             TO   SKT2800-ADRS
           INSPECT SKT2800-ADRS              REPLACING ALL "  " BY "　"
      *    
      *    郵便番号
           MOVE     SYS-1002-POST(1:3)       TO   WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT            TO   WRK-POSTMAE
      *
           MOVE     SYS-1002-POST(4:4)       TO   WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT            TO   WRK-POSTATO
      *
           MOVE     WRK-POSTG                TO   SKT2800-POST
      *
      *    電話番号
           MOVE     WRK-HOSPTEL              TO   SKT2800-TEL
      *
      *    医療機関名
           MOVE     WRK-HOSPNAME             TO   SKT2800-HOSPNAME
           INSPECT SKT2800-HOSPNAME          REPLACING ALL "  " BY "　"
      *
      *    開設者氏名
           MOVE     WRK-KAISETUNAME          TO   SKT2800-KAISETUNAME
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD          TO   WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT            TO   SKT2800-HOSPCD
      *
      *    点数票区分
           EVALUATE    SYS-1001-TENHYOKBN
           WHEN   1
               MOVE    "◯"           TO     SKT2800-MARU1
           WHEN   3
               MOVE    "◯"           TO     SKT2800-MARU2
           END-EVALUATE
      *
      *    診療科名
           MOVE    WRK-SRYKANAME(1)   TO     SKT2800-SRYKAMEI(1)
           MOVE    WRK-SRYKANAME(2)   TO     SKT2800-SRYKAMEI(2)
           MOVE    WRK-SRYKANAME(3)   TO     SKT2800-SRYKAMEI(3)
      *
      *    医療機関区分
           EVALUATE  WRK-HOSPKBN-CD
           WHEN "1"
             MOVE  "○"               TO     SKT2800-HOSPKBN(1)
           WHEN "2"
             MOVE  "○"               TO     SKT2800-HOSPKBN(2)
           WHEN "3"
             MOVE  "○"               TO     SKT2800-HOSPKBN(3)
           WHEN "4"
             MOVE  "○"               TO     SKT2800-HOSPKBN(4)
           END-EVALUATE
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"       TO  SKT2800-TOKUBETU
           END-IF
      *
           .
      *
       310-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    集計データ移行                    
      *****************************************************************
       320-BODY-HEN-SEC                   SECTION.
      *
      *    県外保険者による２枚目か？
           IF     WRK-CNT      =     1
      *点数・件数データ移行
             PERFORM   VARYING   IDX2  FROM  1  BY  1
                       UNTIL     IDX2  >     10
               MOVE     SUM-TENSU(IDX2)    TO    WRK-Z9
               MOVE     WRK-Z9             TO    WRK-MAE-INPUT
               PERFORM  2009-HENKAN-SEC
               MOVE     WRK-OUT-INPUT      TO    SKT2800-TENSU(IDX2)
      *
               MOVE     SUM-KENSU(IDX2)    TO    WRK-Z6
               MOVE     WRK-Z6             TO    WRK-MAE-INPUT
               PERFORM  2009-HENKAN-SEC
               MOVE     WRK-OUT-INPUT          TO    SKT2800-KENSU(IDX2)
             END-PERFORM
      *
      *請求書枚数データ移行
             PERFORM   VARYING   IDX3  FROM  1  BY  1
                       UNTIL     IDX3  >     3
      *        レセ電はレセ電国保請求書の行数（２２）で割る
               IF   SYS-2005-KKHRECEKBN  NOT =  ZERO
               AND  WRK-PARA-SYOKBN      NOT =  2
                   IF  IDX3  =  3
                       MOVE  WRK-MAISUKEI     TO  WRK-Z3
                   ELSE
                     IF  IDX3  =  1
                       DIVIDE  SUM-SEIKYUSYO(IDX3)  BY  22
                       GIVING  WRK-GMAISU  REMAINDER  WRK-AMARI
                       IF  WRK-AMARI  >  ZERO
                           ADD   1            TO  WRK-GMAISU
                       END-IF
                     ELSE
      *        県外データ
                       INITIALIZE                    WRK-GMAISU
                       PERFORM  VARYING  IDX4  FROM  1  BY  1
                                UNTIL    IDX4  >     47
                                OR   WRK-PREFKENSU(IDX4)  =  ZERO
                         DIVIDE  WRK-PREFKENSU(IDX4)  BY  22
                         GIVING  WRK-MAISU  REMAINDER  WRK-AMARI
                         IF  WRK-AMARI  >  ZERO
                             ADD   1          TO  WRK-MAISU
                         END-IF
                         ADD   WRK-MAISU      TO  WRK-GMAISU
                       END-PERFORM
                     END-IF
                   END-IF
               ELSE
                   MOVE  SUM-SEIKYUSYO(IDX3)  TO  WRK-Z3
               END-IF
               MOVE     WRK-Z3               TO  WRK-MAE-INPUT
               PERFORM  2009-HENKAN-SEC
               MOVE     WRK-OUT-INPUT        TO  SKT2800-SEIKYUSYO(IDX3)
             END-PERFORM
      *
      *      レセ電時は印字しない
             IF   SYS-200501-KOUIKIRECEKBN  NOT =  ZERO
             AND  WRK-PARA-SYOKBN      NOT =  2
               DISPLAY  "---RECEDEN---"
             ELSE
      *      請求書の枚数
               INITIALIZE  WRK-GMAISU
               PERFORM  VARYING  IDX1  FROM  1  BY  1
                        UNTIL    IDX1  >     2
      *
                 INITIALIZE  WRK-MAISU
                 PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL    SCNT-HKNJANUM(IDX1 IDX2)  =  SPACE
                     ADD  1  TO  WRK-MAISU
                 END-PERFORM
      *
                 ADD  WRK-MAISU       TO  WRK-GMAISU
                 MOVE  WRK-MAISU      TO  WRK-Z3
                 MOVE  WRK-Z3         TO  SKT2800-SEIKYUSYO (IDX1 + 3)
               END-PERFORM
             END-IF
             MOVE  WRK-GMAISU       TO  WRK-Z3
             MOVE  WRK-Z3           TO  SKT2800-SEIKYUSYO (6)
      *
      *    社保福祉医療費請求件数データ移行
             MOVE      SUM-SYAHOSEIKYU    TO  WRK-Z3-9
             MOVE      WRK-Z3-9           TO  WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE      WRK-OUT-INPUT      TO  SKT2800-SYAHOSEIKYU
           END-IF
      *
      *    県外保険者名
           PERFORM  VARYING  IDX3   FROM  1  BY  1
                    UNTIL    IDX3   >     12
             PERFORM  VARYING  IDX4   FROM  1  BY  1
                      UNTIL    IDX4   >     2
               DISPLAY "県外保険者="  WRK-CNT-HKNJANUM(IDX4 WRK-CNT)
               DISPLAY "カウント="    WRK-CNT
               IF  WRK-CNT-HKNJANUM(IDX4 WRK-CNT) NOT = SPACE
                 MOVE  WRK-CNT-HKNJANUM(IDX4 WRK-CNT)  TO  WRK-HKNJANUM
                 PERFORM  920-HKNJAINF-SET-SEC
                 PERFORM  400-KENNAME-SEC
                 IF  IDX4  =  1
                   MOVE  WRK-HKNJANAME  TO  SKT2800-KGIHKNJANAME(IDX3)
                 ELSE
                   MOVE  WRK-HKNJANAME  TO  SKT2800-KGIKOUKINAME(IDX3)
                 END-IF
               END-IF
             END-PERFORM
             ADD   1                   TO  WRK-CNT
           END-PERFORM
           .
      *
       320-BODY-HEN-EXT.
           EXIT.
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
           MOVE   "SKT2800.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保総括表"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保総括表［返戻分］"
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保総括表［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保総括表［返戻分］［特別療養費］" 
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT2800             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               DISPLAY  "ORCSPRT call ok"
           ELSE
               DISPLAY  "ORCSPRT call ng"
               MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
      *
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    県名設定処理
      *****************************************************************
       400-KENNAME-SEC                SECTION.
      *
           MOVE     SPACE         TO    WRK-HKNJANAME
                                        WRK-KENNAME
           IF  IDX4  =  1
             MOVE  WRK-CNT-HKNJANUM(IDX4 WRK-CNT)  TO  WRK-HKNJANUM
           ELSE
             MOVE  WRK-CNT-HKNJANUM(IDX4 WRK-CNT)(3:) TO WRK-HKNJANUM
           END-IF
      *
           EVALUATE  WRK-HKNJANUM (1:2)
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
      *    国保の場合、国保組合は県名を印字しない
           IF     WRK-CNT-HKNJANUM(IDX4 WRK-CNT)(3:1)  =  "3"
           AND    IDX4  =  1
             MOVE  SPACE                 TO  WRK-KENNAME
           END-IF
           IF  IDX4  =  1
               STRING  WRK-KENNAME       DELIMITED BY SPACE
                       HKNJA-HKNJANAME   DELIMITED BY SPACE
                       INTO              WRK-HKNJANAME
               END-STRING
           ELSE
               STRING  WRK-KENNAME       DELIMITED BY SPACE
                       "広域連合"        DELIMITED BY SPACE
                       INTO              WRK-HKNJANAME
               END-STRING
           END-IF
           DISPLAY "保険者名=" WRK-HKNJANAME
      *
           .
       400-KENNAME-EXT.
           EXIT.
      *
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
                                "*** SOKATU2800 SEIKYU UPDATE ERROR ***"
                                           TO  WRK-RECEERR
                                 MOVE    RECE10-KEY     TO
                                         WRK-RECEERR(33:)
                                 PERFORM 500-ERR-HENSYU-SEC
                                 MOVE    1      TO      FLG-UPDFILE
                        END-IF
      *
              ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                       MOVE    "*** SOKATU2800 SEIKYU INVALID ***"
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
      *
           MOVE   SPA-HOSPNUM           TO    RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           MOVE   SPACE                 TO    RECE10-XXSTHKNJANUM
           MOVE   "99999999"            TO    RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000               TO  RECE10-SRYYM
             MOVE  999999               TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE   RECE10-REC            TO    MCPDATA-REC
           MOVE   "DBSELECT"            TO    MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
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
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF       MCP-RC  =     ZERO
                    MOVE    MCPDATA-REC   TO      RECE10-REC
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
      *             返戻分でオンライン請求は読み飛ばす
                    IF      (WRK-PARA-SYOKBN         =  "2")
                    AND     (RECE10-HENREI-SIJI-FLG  =  1)
                        GO  TO  900-SEIKYU-READ-SEC
                    END-IF
      *
      *        テスト患者のときは読み飛ばす
                    MOVE    SPACE               TO  PTINF-REC
                    INITIALIZE                      PTINF-REC
                    MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                    MOVE    RECE10-PTID         TO  PTINF-PTID
                    MOVE    PTINF-REC           TO  MCPDATA-REC
                    PERFORM 800-PTINF-READ-SEC
                    IF      FLG-PTINF            =   ZERO
                      AND     PTINF-TSTPTNUMKBN  =   "1"
                         GO  TO  900-SEIKYU-READ-SEC
                    END-IF   
                    ADD     1             TO      CNT-RECE10   
           ELSE
                    MOVE    1             TO      FLG-END
           END-IF
           .
      *
       900-SEIKYU-READ-EXT.
           EXIT.
      *
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
           MOVE   WRK-HKNNUM              TO  RECE10-HKNNUM-S
                                              RECE10-HKNNUM
      *
           MOVE    SPACE                  TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"             TO  RECE10-XXEDHKNJANUM
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
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
           MOVE    WRK-PATH2           TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
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
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  950-SEIKYU-READ-SEC
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
           MOVE    WRK-PATH             TO  MCP-PATHNAME
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
           MOVE    WRK-PATH2            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                 TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               INITIALIZE                          TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)        TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  TBL-HKOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  20011-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
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
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
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
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     4  OR
                    SYU-KOH-HKNNUM(IDX) =  SPACE
      *
             IF    SYU-KOH-HKNNUM(IDX)  NOT  =  "027"  AND
                   SYU-KOH-HKNNUM(IDX)(1:1)  =  "0"
               MOVE  SYU-TOTAL-HKNTEN      TO    WRK-KOHTENSU
             END-IF
           END-PERFORM
      *
           .
       800-SEIKYU-SEC-EXT.
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
      *
           .
      *
       900-DBDISCONNECT-EXT.
           EXIT.
