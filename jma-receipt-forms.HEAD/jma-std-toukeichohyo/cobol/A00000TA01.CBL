      ******************************************************************
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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000TA01.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 総括表患者別
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付         内容
      *   01.00.00    吉川    2011/10/27   新規作成
      *   01.00.01    吉川    2013/09/02   患者数が100件以上の場合、
      *                                    処理が終了しない不具合を修正
      *                                    集計範囲期間の計算修正
      *   01.00.02    門間    2013/10/03   CSVファイルが20kBを超える場合、
      *                                    正しく出力されない不具合を修正
      *   01.00.03    吉川    2014/04/01   CSVの小計行のズレ修正
      *   01.00.04    吉川    2014/04/16   当月と月遅れに診療がある場合
      *                                    月遅れ分が印字されないのを修正
      *   01.00.05    吉川    2014/05/22   保険が月に変更になった場合
      *                                    それぞれの一部負担金を印字する
      *                                    実日数を3桁まで印字する
      *                                    実人数、延べ人数を9999まで
      *                                    カウントできるように修正
      *                                    まとめ公費を使用した場合
      *                                    負担者番号と受給者番号が
      *                                    正しく印字されないのを修正
      *   02.00.01    門間    2014/10/31   ver4.8.0対応（一時ディレクトリ変更）
      *   02.00.02    吉川    2016/11/14   分点時、保険単独分の負担金を
      *                                    一部負担欄に加算するように修正
      *   02.00.03    門間    2019/03/14   食事金額を振込金額に含めるように修正
      *                                    実人数及び延べ人数のオーバーフロー対応
      *******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    一時ファイル
           SELECT  TA01-FILE       ASSIGN          TA01P
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TA01TMP-KEY
                                   FILE    STATUS  IS  STS-TA01.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    一時ファイル
       FD  TA01-FILE.
           COPY    "A00000TA01TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01PARA//
                                       BY         //TA01P//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    帳票コピー句
           COPY    "A00000TA01.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TA01                    PIC X(02).
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-UPD                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-RECE10                  PIC 9(01).
           03  FLG-RECE02                  PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TA01TMP                 PIC 9(01).
           03  FLG-HENREI                  PIC 9(01).
           03  FLG-PRINT                   PIC 9(01).
           03  FLG-KENSU                   PIC 9(01).
           03  FLG-BUNTEN                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE10                  PIC 9(06).
           03  CNT-PAGE                    PIC 9(02).
           03  CNT-GPAGE                   PIC 9(02).
           03  CNT-PRTYM                   PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  IDX3                        PIC 9(04).
           03  IDX4                        PIC 9(04).
           03  IDY                         PIC 9(04).
           03  IDY1                        PIC 9(05).
           03  IDY2                        PIC 9(05).
           03  IDZ                         PIC 9(04).
           03  IDZ1                        PIC 9(04).
           03  IDZ2                        PIC 9(04).
           03  IDW                         PIC 9(04).
           03  IDXP                        PIC 9(05).
           03  IDXS                        PIC 9(05).
           03  IDX-ST                      PIC 9(03).
           03  IDX-ED                      PIC 9(03).
           03  IDX-POINT                   PIC 9(05).
           
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID              PIC 9(07).
           03  WRK-PARA-SHELLID            PIC X(08).
           03  WRK-PARA-HOSPNUM            PIC 9(02).
           03  WRK-PARA-PRTYMST            PIC X(06).
           03  WRK-PARA-PRTYMED            PIC X(06).
           03  WRK-PARA-NYUGAIKBN          PIC X(01).
           03  WRK-PARA-HKNSBT             PIC X(01).
           03  WRK-PARA-HKNJANUM           PIC X(08).
           03  WRK-PARA-KOHNUM             PIC X(03).
           03  WRK-PARA-PREFKBN            PIC X(01).
           03  WRK-PARA-HENREI             PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD                  PIC X(08).
           03  WRK-SEIYMDWH                PIC X(22).
           03  WRK-SRYYMWH                 PIC X(16).
           03  WRK-SRYYMD                  PIC X(08).
      *    ファイル名称用エリア
           03  FILE-NAME                   PIC X(512).
      *  
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(22).
      *
           03  WRK-PRTYMST.
               05  WRK-PRTYYST             PIC 9(04).
               05  WRK-PRTMMST             PIC 9(02).
           03  WRK-PRTYMED.
               05  WRK-PRTYYED             PIC 9(04).
               05  WRK-PRTMMED             PIC 9(02).
           03  WRK-YEAR                    PIC 9(02).
           03  WRK-MONTH                   PIC 9(02).
      *
           03  WRK-Z2                      PIC Z9.
           03  WRK-Z3                      PIC ZZZ.
           03  WRK-Z3-9                    PIC ZZ9.
           03  WRK-Z4-9                    PIC ZZZ9.
           03  WRK-Z5                      PIC Z,ZZZ.
           03  WRK-Z5-9                    PIC ZZZZ9.
           03  WRK-Z6                      PIC ZZ,ZZZ.
           03  WRK-Z6-9                    PIC ZZ,ZZ9.
           03  WRK-Z9                      PIC Z,ZZZ,ZZZ.
           03  WRK-Z9-9                    PIC Z,ZZZ,ZZ9.
           03  WRK-Z10                     PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z10-9                   PIC ZZ,ZZZ,ZZ9.
           03  WRK-S                       PIC 9(05).
           03  WRK-TAIHI                   PIC X(10).
           03  WRK-KOHNUM                  PIC X(03).
           03  WRK-KOHID                   PIC 9(10).
           03  WRK-KOHNAME                 PIC X(10).
           03  WRK-HKNCOMBI                PIC 9(04) OCCURS 10.
           03  WRK-FTNJANUM                PIC X(08).
           03  WRK-JKYSNUM                 PIC X(08).
           03  WRK-SRYKA                   PIC X(02).
           03  WRK-TEISYUTUSAKI            PIC 9(01).
           03  WRK-AMARI                   PIC 9(02).
           03  WRK-FURIKOMI                PIC 9(09).
           03  WRK-SEIKYUGK                PIC 9(09).
           03  WRK-RMSAGAKU                PIC 9(09).
           03  WRK-JNINZU                  PIC 9(05).
           03  WRK-NNINZU                  PIC 9(05).
           03  WRK-CNTLINE                 PIC 9(05).
           03  WRK-CNTPAGE                 PIC 9(02).
           03  WRK-GPAGE                   PIC 9(02).
           03  WRK-HKNSBT.
               05  WRK-HKNSBT1             PIC X(01).
               05  WRK-HKNSBT2             PIC X(01).
               05  WRK-HONKZK              PIC X(01).
           03  WRK-TANSEIKYUGK             PIC 9(09).
           03  WRK-TANDOKUTEN              PIC 9(09).
      *
           03  WRK-RECEERR                 PIC X(200).
           03  WRK-PATH                    PIC X(50).
           03  WRK-PATH2                   PIC X(50).
      *
      *    集計領域
       01  SUM-AREA.
      *    小計
           03  SUM-SYOKEI.
               05  SUM-SNISSU              PIC 9(05).
               05  SUM-STENSU              PIC 9(09).
               05  SUM-SITBFTN             PIC 9(09).
               05  SUM-SSKJ-NISSU          PIC 9(05).
               05  SUM-SSKJ-RYOYOHI        PIC 9(09).
               05  SUM-SSKJ-FTN            PIC 9(09).
               05  SUM-SRMSAGAKU           PIC 9(09).
               05  SUM-SFURIKOMI           PIC 9(09).
               05  SUM-SK-NISSU            PIC 9(05).
               05  SUM-SK-TENSU            PIC 9(09).
               05  SUM-SK-ITBFTN           PIC 9(09).
               05  SUM-SK-SKJ-RYOYOHI      PIC 9(09).
               05  SUM-SK-SKJ-FTN          PIC 9(09).
      *    総合計
           03  SUM-GOKEI.
               05  SUM-GNISSU              PIC 9(06).
               05  SUM-GTENSU              PIC 9(10).
               05  SUM-GITBFTN             PIC 9(10).
               05  SUM-GSKJ-NISSU          PIC 9(06).
               05  SUM-GSKJ-RYOYOHI        PIC 9(10).
               05  SUM-GSKJ-FTN            PIC 9(10).
               05  SUM-GRMSAGAKU           PIC 9(10).
               05  SUM-GFURIKOMI           PIC 9(10).
               05  SUM-GK-NISSU            PIC 9(06).
               05  SUM-GK-TENSU            PIC 9(10).
               05  SUM-GK-ITBFTN           PIC 9(10).
               05  SUM-GK-SKJ-RYOYOHI      PIC 9(10).
               05  SUM-GK-SKJ-FTN          PIC 9(10).
      *
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL  OCCURS  100.
               05  WRK-2007-MEISYO         PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                           PIC X(03)   OCCURS  10.
           03  WRK-2007-MAX                PIC 9(03).
      *
      *    KEYエリア
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-NEW-SRYYM         PIC X(06).
           03  KEY-OLD.
               05  KEY-OLD-SRYYM         PIC X(06).
      *
      *    ＣＳＶエリア
       01  CSV-DATA-AREA.
           03  CSV-DATA-TBL.
             05  CSV-DATA                  PIC X(100).
           03  WRK-OUTPUT-DATA             PIC X(20000).
           03  CSV-DATA-SIZE               PIC 9(03)  VALUE 100.
           03  CSV-DIR                     PIC X(200).
           03  CSV-NAME                    PIC X(200).
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-HKNSBT1        PIC X(50) VALUE "保険".
               07  CSV-HEAD-HKNSBT2        PIC X(50) VALUE "単併".
               07  CSV-HEAD-HONKZK         PIC X(50) VALUE "本家".
               07  CSV-HEAD-PTNUM          PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-NAME           PIC X(50) VALUE "氏名".
               07  CSV-HEAD-HKNJANUM       PIC X(50) VALUE "保険者番号".
               07  CSV-HEAD-NISSU          PIC X(50) VALUE "実日数".
               07  CSV-HEAD-TENSU          PIC X(50) VALUE "点数".
               07  CSV-HEAD-ITBFTN         PIC X(50) VALUE "一部負担".
               07  CSV-HEAD-SNISSU         PIC X(50) VALUE "食事回数".
               07  CSV-HEAD-SKINGK         PIC X(50) VALUE "食事金額".
               07  CSV-HEAD-SFTN           PIC X(50) VALUE "食事負担".
               07  CSV-HEAD-RMSAGAKU       PIC X(50) VALUE "室料差額".
               07  CSV-HEAD-FURIKOMI       PIC X(50) VALUE "振込金額".
               07  CSV-HEAD-KOH1KOHNUM     PIC X(50) 
                                           VALUE "公費１法別".
               07  CSV-HEAD-KOH1FTNJANUM   PIC X(50) 
                                           VALUE "公費１負担者番号".
               07  CSV-HEAD-KOH1JKYSNUM    PIC X(50) 
                                           VALUE "公費１受給者番号".
               07  CSV-HEAD-KOH1NISSU      PIC X(50) 
                                           VALUE "公費１日数".
               07  CSV-HEAD-KOH1TENSU      PIC X(50) 
                                           VALUE "公費１点数".
               07  CSV-HEAD-KOH1ITBFTN     PIC X(50) 
                                           VALUE "公費１一部負担".
               07  CSV-HEAD-KOH1SKINGK     PIC X(50) 
                                           VALUE "公費１食事金額".
               07  CSV-HEAD-KOH1SFTN       PIC X(50) 
                                           VALUE "公費１食事負担".
               07  CSV-HEAD-KOH2KOHNUM     PIC X(50) 
                                           VALUE "公費２法別".
               07  CSV-HEAD-KOH2FTNJANUM   PIC X(50) 
                                           VALUE "公費２負担者番号".
               07  CSV-HEAD-KOH2JKYSNUM    PIC X(50) 
                                           VALUE "公費２受給者番号".
               07  CSV-HEAD-KOH2NISSU      PIC X(50) 
                                           VALUE "公費２日数".
               07  CSV-HEAD-KOH2TENSU      PIC X(50) 
                                           VALUE "公費２点数".
               07  CSV-HEAD-KOH2ITBFTN     PIC X(50) 
                                           VALUE "公費２一部負担".
               07  CSV-HEAD-KOH2SKINGK     PIC X(50) 
                                           VALUE "公費２食事金額".
               07  CSV-HEAD-KOH2SFTN       PIC X(50) 
                                           VALUE "公費２食事負担".
               07  CSV-HEAD-KOH3KOHNUM     PIC X(50) 
                                           VALUE "公費３法別".
               07  CSV-HEAD-KOH3FTNJANUM   PIC X(50) 
                                           VALUE "公費３負担者番号".
               07  CSV-HEAD-KOH3JKYSNUM    PIC X(50) 
                                           VALUE "公費３受給者番号".
               07  CSV-HEAD-KOH3NISSU      PIC X(50) 
                                           VALUE "公費３日数".
               07  CSV-HEAD-KOH3TENSU      PIC X(50) 
                                           VALUE "公費３点数".
               07  CSV-HEAD-KOH3ITBFTN     PIC X(50) 
                                           VALUE "公費３一部負担".
               07  CSV-HEAD-KOH3SKINGK     PIC X(50) 
                                           VALUE "公費３食事金額".
               07  CSV-HEAD-KOH3SFTN       PIC X(50) 
                                           VALUE "公費３食事負担".
               07  CSV-HEAD-KOH4KOHNUM     PIC X(50) 
                                           VALUE "公費４法別".
               07  CSV-HEAD-KOH4FTNJANUM   PIC X(50) 
                                           VALUE "公費４負担者番号".
               07  CSV-HEAD-KOH4JKYSNUM    PIC X(50) 
                                           VALUE "公費４受給者番号".
               07  CSV-HEAD-KOH4NISSU      PIC X(50) 
                                           VALUE "公費４日数".
               07  CSV-HEAD-KOH4TENSU      PIC X(50) 
                                           VALUE "公費４点数".
               07  CSV-HEAD-KOH4ITBFTN     PIC X(50) 
                                           VALUE "公費４一部負担".
               07  CSV-HEAD-KOH4SKINGK     PIC X(50) 
                                           VALUE "公費４食事金額".
               07  CSV-HEAD-KOH4SFTN       PIC X(50) 
                                           VALUE "公費４食事負担".
               07  CSV-HEAD-HENREIYM       PIC X(50) VALUE "診療月".
             05  CSV-HEAD-R                REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM           PIC X(50) OCCURS 47.
             05  CSV-HEAD-MAX              PIC 9(05) VALUE 47.
      *    
      *    ソート用エリア
       01  SRT-AREA.
           03  SORT-PTNUM                  PIC X(10)   OCCURS  20000.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    収納
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
      *
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER            PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "*** START ***"
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC  
      *
           PERFORM  400-END-SEC
           DISPLAY "*** END ***"
      *
           STOP  RUN
           .
      
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                    FLG-AREA
           INITIALIZE                    STS-AREA
           INITIALIZE                    WRK-AREA
           INITIALIZE                    CNT-AREA
           INITIALIZE                    SUM-AREA
           INITIALIZE                    SPA-AREA
           INITIALIZE                    KEY-AREA
           INITIALIZE                    IDX-AREA
           INITIALIZE                    SRT-AREA
           INITIALIZE                    RECEERR
      *
           PERFORM  100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO   LNK-PRTKANRI-RENNUM
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
                                               RECEERR
                                               WRK-PARA-PRTYMST
                                               WRK-PARA-PRTYMED
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HKNSBT
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-KOHNUM
                                               WRK-PARA-PREFKBN
                                               WRK-PARA-HENREI
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM      TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"                 TO  SJOBKANRI-MODE
           INITIALIZE                    JOBKANRI-REC
           MOVE    SPA-HOSPNUM           TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID        TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID      TO  JOB-SHELLID
           MOVE    "A00000TA01"             TO  JOB-PGID
           MOVE    "総括表 患者別"       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"             USING
                                         ORCSJOBKANRIAREA
                                         JOBKANRI-REC
                                         SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           MOVE    "ATA01TP"                TO  TA01P-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  TA01P-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  TA01P-HOSPNUM
      *
           MOVE    TA01P-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  TA01P-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
           MOVE    WRK-PARA-PRTYMST      TO  WRK-SRYYMD (1:6)
           MOVE    "01"                  TO  WRK-SRYYMD (7:2)
           MOVE    WRK-PARA-PRTYMST      TO  WRK-SYMD (1:6)
           MOVE    "01"                  TO  WRK-SYMD (7:2)
           PERFORM 1001-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)     TO  WRK-SRYYMWH 
      *
           INITIALIZE                        STS-AREA-DAY
           INITIALIZE                        LNK-DAY2-AREA
           MOVE    "21"                  TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD   TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF  STS-DAY-RC1  =  ZERO 
             MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTYMST      TO  SYS-1001-EDYUKYMD(1:6)
                                             SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                  TO  SYS-1001-STYUKYMD(7:2)
                                             SYS-1001-EDYUKYMD(7:2)
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM   900-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    公費番号まとめ情報
           INITIALIZE                        WRK-2007-AREA
           INITIALIZE                        SYS-2007-REC
           MOVE    "2007"                TO  SYS-2007-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM    TO  SYS-2007-STYUKYMD (1:6)
           MOVE    "01"                  TO  SYS-2007-STYUKYMD (7:2)
           MOVE    SYS-2007-STYUKYMD     TO  SYS-2007-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM      TO  SYS-2007-HOSPNUM
           MOVE    SYS-2007-REC          TO  MCPDATA-REC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           MOVE    "DBSELECT"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
             MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                  TO  IDX1
           PERFORM  UNTIL  FLG-SYSKANRI  =  1
                    OR     IDX1          >=  100
             MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
             ADD     1                   TO  IDX1
             MOVE    SYS-2007-MEISYO(1)  TO  WRK-2007-MEISYO(IDX1)
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >  10
                      OR       SYS-2007-KOHNUM(IDX2)  =  SPACE
                 MOVE    SYS-2007-KOHNUM(IDX2)
                                         TO WRK-2007-KOHNUM(IDX1 IDX2)
             END-PERFORM
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           MOVE    IDX1                  TO  WRK-2007-MAX
      *
      *    パラメタ編集処理
           PERFORM  110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       1001-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT  WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
      *
           .
       1001-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                  TO  FLG-END
      *
      *    入外区分チェック
           IF  WRK-PARA-NYUGAIKBN  NOT =  "1" AND "2"
             MOVE    "入外区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    保険種別チェック
           IF  WRK-PARA-HKNSBT  NOT =  "1" AND "2" AND "3" AND "4"
                                     AND  "5" AND SPACE
             MOVE    "保険種別が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    県内県外区分チェック
           IF  WRK-PARA-PREFKBN  NOT = "1" AND "2" AND SPACE
             MOVE    "県内県外区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
      *    返戻区分チェック
           IF  WRK-PARA-HENREI  NOT = "1" AND SPACE
             MOVE    "返戻区分が間違っています"    TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    一時ファイルの初期化、オープン
           OPEN    OUTPUT   TA01-FILE
           CLOSE   TA01-FILE
           OPEN    I-O      TA01-FILE
      *
      *    複数診療月がある場合
           MOVE  WRK-PARA-PRTYMST             TO  WRK-PRTYMST
           MOVE  WRK-PARA-PRTYMED             TO  WRK-PRTYMED
      *
           IF  (WRK-PARA-PRTYMED  NOT =  SPACE)
           AND (WRK-PARA-PRTYMST  NOT =  WRK-PARA-PRTYMED)
      *      開始月＞終了月か
             IF  WRK-PRTMMST  >  WRK-PRTMMED
      *        年
               COMPUTE  WRK-YEAR  =  WRK-PRTYYED - WRK-PRTYYST - 1
      *        月（年またぎ考慮）
               COMPUTE  WRK-MONTH = (WRK-PRTMMED + 12 - WRK-PRTMMST) + 1
             ELSE
      *        年
               COMPUTE  WRK-YEAR  =  WRK-PRTYYED - WRK-PRTYYST
      *        月
               COMPUTE  WRK-MONTH =  WRK-PRTMMED - WRK-PRTMMST + 1
             END-IF
      *      期間月数
             COMPUTE  CNT-PRTYM = (WRK-YEAR * 12)  +  WRK-MONTH
           ELSE
             MOVE    1                        TO  CNT-PRTYM
           END-IF
           DISPLAY "期間月数 =" CNT-PRTYM
      *
      *    集計
           IF  WRK-PARA-HKNSBT  NOT =  SPACE
             EVALUATE  WRK-PARA-HKNSBT
      *      社保、公費単独
             WHEN  "1"
             WHEN  "5"
               MOVE    1           TO  WRK-TEISYUTUSAKI
      *      国保、退職
             WHEN  "2"
             WHEN  "3"
               MOVE    2           TO  WRK-TEISYUTUSAKI
      *      後期高齢
             WHEN  "4"
               MOVE    6           TO  WRK-TEISYUTUSAKI
             END-EVALUATE
      *
             PERFORM  VARYING  IDW  FROM  1  BY  1
                      UNTIL    IDW  >  CNT-PRTYM
               MOVE    ZERO          TO  FLG-END
               PERFORM  900-SEIKYU-SELECT-SEC
      *        集計処理
               PERFORM  210-SYUKEI-MAIN-SEC
                        UNTIL  FLG-END  =  1
               PERFORM  900-SEIKYU-CLOSE-SEC
      *
               ADD     1                   TO  WRK-PRTMMST
               IF  WRK-PRTMMST  >  12
                 ADD     1                 TO  WRK-PRTYYST
                 MOVE    1                 TO  WRK-PRTMMST
               END-IF
      *
            END-PERFORM
      *
           ELSE
      *      全指定
             PERFORM  VARYING  IDW  FROM  1  BY  1
                      UNTIL    IDW  >  CNT-PRTYM
               PERFORM  VARYING  IDZ  FROM  1  BY  1
                        UNTIL    IDZ  >  3
                 MOVE    ZERO        TO  FLG-END
                 EVALUATE  IDZ
                   WHEN  1
                     MOVE    1       TO  WRK-TEISYUTUSAKI
                   WHEN  2
                     MOVE    2       TO  WRK-TEISYUTUSAKI
                   WHEN  3
                     MOVE    6       TO  WRK-TEISYUTUSAKI
                 END-EVALUATE
                 PERFORM  900-SEIKYU-SELECT-SEC
      *          集計処理
                 PERFORM  210-SYUKEI-MAIN-SEC
                          UNTIL  FLG-END  =  1
                 PERFORM  900-SEIKYU-CLOSE-SEC
               END-PERFORM
      *
               ADD     1                   TO  WRK-PRTMMST
               IF  WRK-PRTMMST  >  12
                 ADD     1                 TO  WRK-PRTYYST
                 MOVE    1                 TO  WRK-PRTMMST
               END-IF
      *
             END-PERFORM
           END-IF
      *
      *    一時ファイルクローズ
           CLOSE    TA01-FILE
      *
      *    印刷処理
           IF  FLG-UPD  =  1
             PERFORM  300-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計主処理
      *****************************************************************
       210-SYUKEI-MAIN-SEC        SECTION.
      *
           INITIALIZE  TA01TMP-REC
      *
      *    TMPファイルキーセット
      *
      *    入外区分(一応)
           MOVE    RECE10-NYUGAIKBN      TO  TA01TMP-NYUGAIKBN
      *
      *    県区分（一応）
           IF  WRK-PARA-PREFKBN  NOT =  SPACE
             MOVE  RECE10-PREFKBN        TO  TA01TMP-PREFKBN
           END-IF
      *
      *    診療年月
           IF  RECE10-SKYYM  NOT =  ZERO
             MOVE    RECE10-SKYYM        TO  TA01TMP-SRYYM
           ELSE
             MOVE    RECE10-SRYYM        TO  TA01TMP-SRYYM
           END-IF
      *
      *    公費単独かどうか
           IF  RECE10-HKNNUM  =  SPACE
             MOVE    "2"                 TO  TA01TMP-HKNSBT1
             MOVE    "1"                 TO  TA01TMP-HKNSBT2
             MOVE    "1"                 TO  TA01TMP-HONKZK
           ELSE
      *      保険種別１（社保→公費→国保→退職→後期 の順）
             EVALUATE  RECE10-HKNNUM
      *        国保
               WHEN  "060"
               WHEN  "068"
                 MOVE    "3"             TO  TA01TMP-HKNSBT1
      *        退職
               WHEN  "067"
               WHEN  "069"
                 MOVE    "4"             TO  TA01TMP-HKNSBT1
      *        後期
               WHEN  "039"
               WHEN  "040"
                 MOVE    "5"             TO  TA01TMP-HKNSBT1
      *        社保
               WHEN OTHER
                 MOVE    "1"             TO  TA01TMP-HKNSBT1
             END-EVALUATE
      *
      *      保険種別２
             MOVE    RECE10-RECESYUBETU(3:1)
                                         TO  TA01TMP-HKNSBT2
      *
      *      本人家族区分
             MOVE    RECE10-RECESYUBETU(4:1)
                                         TO  TA01TMP-HONKZK
           END-IF
      *
      *    患者番号
           MOVE    RECE10-PTNUM          TO  TA01TMP-PTNUM
      *
      *    保険者番号
           MOVE    RECE10-HKNJANUM       TO  TA01TMP-HKNJANUM
      *
      *    診療月(月遅れ、返戻)
           IF  RECE10-SRYYM  NOT =  WRK-PRTYMST
             MOVE    RECE10-SRYYM        TO  TA01TMP-HENREIYM
           END-IF
      *
      *    キーを元にTMPファイル読み込み
           PERFORM  800-STXTMP-READ-SEC
      *
      *    集計処理
           PERFORM  220-SYUKEI-SEC
      *
      *    公費集計処理
           IF  RECE10-KOHNUM(1)  NOT =  SPACE
      *      請求管理テーブルから探索
             PERFORM  230-KOH-SYUKEI-SEC
           ELSE
      *      公費請求テーブルから探索
             PERFORM  900-KOHSKY-READ-SEC
             PERFORM  240-KOH-SYUKEI-SEC
           END-IF
      *
      *    TMPファイル書き込み(1=新規データ 0=既存データ)
           IF  FLG-TA01TMP  =  1
             WRITE   TA01TMP-REC
           ELSE
             REWRITE TA01TMP-REC
           END-IF
      *
      *    ソートテーブルに格納
           ADD     1                     TO  IDXS
           MOVE    RECE10-PTNUM          TO  SORT-PTNUM(IDXS)
      *
      *    次データ読み込み
           PERFORM  910-SEIKYU-READ-SEC
      *
           .
       210-SYUKEI-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       220-SYUKEI-SEC       SECTION.
      *
      *    氏名（患者情報から取得）
           MOVE    PTINF-NAME            TO  TA01TMP-NAME
      *
      *    実日数
           ADD  RECE10-JNISSU(1)         TO  TA01TMP-NISSU
      *
      *    点数
           ADD  RECE10-TOTALTEN(1)       TO  TA01TMP-TENSU
      *
      *    一部負担
      *     ADD  RECE10-FTNMONEY(1)       TO  TA01TMP-ITBFTN
      *
           IF  RECE10-NYUGAIKBN  =  "1"
      *    食事日数
             ADD  RECE10-SHOKUJINISSU(1) TO  TA01TMP-SKJ-NISSU
      *
      *    食事金額
             ADD  RECE10-SHOKUJIRYOYOHI(1)
                                         TO  TA01TMP-SKJ-RYOYOHI
      *
      *    食事負担
             ADD  RECE10-SHOKUJIFTN(1)   TO  TA01TMP-SKJ-FTN
           END-IF
      *
      *    室料差額（収納から取得）
           PERFORM  900-SYUNOU-SET-SEC
           ADD     WRK-RMSAGAKU          TO  TA01TMP-RMSAGAKU
      *
      *    一部負担
      *     ADD     WRK-SEIKYUGK          TO  TA01TMP-ITBFTN
      *
      *    振込金額
           IF  TA01TMP-HKNSBT1  =  "2"
             COMPUTE  WRK-FURIKOMI  =  RECE10-TOTALTEN(2)  *  10
                                    +  RECE10-SHOKUJIRYOYOHI(2)
           ELSE
             COMPUTE  WRK-FURIKOMI  =  RECE10-TOTALTEN(1)  *  10
                                    +  RECE10-SHOKUJIRYOYOHI(1)
                                    -  WRK-SEIKYUGK
           END-IF
      *
      *    診療科（収納から取得）
      *     PERFORM  900-SYUNOU-SET-SEC
      *     MOVE    WRK-SRYKA             TO  TA01TMP-SRYKA
      *
           MOVE    1                     TO  FLG-UPD
      *
           .
       220-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理（請求管理）
      *****************************************************************
       230-KOH-SYUKEI-SEC       SECTION.
      *
           INITIALIZE                          FLG-BUNTEN
      *
           PERFORM  VARYING  IDY  FROM  1  BY  1
                    UNTIL    IDY  >     4
                    OR  RECE10-KOHNUM(IDY)  =  SPACE
      *
      *      負担者番号、受給者番号を取得
             MOVE    RECE10-KOHNUM(IDY)    TO  WRK-KOHNUM
             PERFORM 900-PTKOHINF-SET-SEC
      *
      *      特定疾患は０５１でまとめる
             IF  RECE10-KOHNUM(IDY)  =  "091"
               MOVE    "051"               TO  RECE10-KOHNUM(IDY)
             END-IF
      *
      *      公費番号まとめ情報があるとき
             IF  WRK-2007-MAX  >  ZERO
               MOVE  SPACE                 TO  WRK-KOHNAME
               PERFORM  VARYING  IDZ1  FROM  1  BY  1
                        UNTIL    IDZ1  >  WRK-2007-MAX
                 PERFORM  VARYING  IDZ2  FROM  1  BY  1
                          UNTIL    IDZ2  >  10
                          OR  WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                   IF  RECE10-KOHNUM(IDY)  =  WRK-2007-KOHNUM(IDZ1 IDZ2)
                     MOVE    WRK-2007-KOHNUM(IDZ1 1)
                                           TO  RECE10-KOHNUM(IDY)
                     MOVE    WRK-2007-MEISYO (IDZ1) 
                                           TO  WRK-KOHNAME
                     MOVE    10            TO  IDZ2
                     MOVE    WRK-2007-MAX  TO  IDZ1
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
      *
      *      公費番号
             MOVE    RECE10-KOHNUM(IDY)    TO  TA01TMP-KOHNUM(IDY)
      *
      *      公費負担者番号
             MOVE    WRK-FTNJANUM          TO  TA01TMP-FTNJANUM(IDY)
      *      
      *      公費受給者番号
             MOVE    WRK-JKYSNUM           TO  TA01TMP-JKYSNUM(IDY)
      *      
      *      公費−日数
             ADD  RECE10-JNISSU(IDY + 1)   TO  TA01TMP-K-NISSU(IDY)
      *
      *      公費−点数
             ADD  RECE10-TOTALTEN(IDY + 1) TO  TA01TMP-K-TENSU(IDY)
      *
      *      公費−一部負担
             ADD  RECE10-FTNMONEY(IDY + 1) TO  TA01TMP-K-ITBFTN(IDY)
      *
             IF  RECE10-NYUGAIKBN  =  "1"
      *      食事−療養費
               ADD  RECE10-SHOKUJIRYOYOHI(IDY + 1)
                                       TO  TA01TMP-K-SKJ-RYOYOHI(IDY)
      *
      *      食事−負担金
               ADD  RECE10-SHOKUJIFTN(IDY + 1)
                                       TO  TA01TMP-K-SKJ-FTN(IDY)
             END-IF
      *
      *      一部負担金保存
             MOVE    RECE10-FTNMONEY(IDY + 1) 
                                           TO  WRK-SEIKYUGK
      *
      *      分点判定
             IF  RECE10-TOTALTEN(1)  NOT =  RECE10-TOTALTEN(IDY + 1)
                 MOVE    1                 TO  FLG-BUNTEN
             END-IF
      *
           END-PERFORM
      *
      *    一部負担金再計算
           IF  (FLG-BUNTEN  =  1)
           AND (TA01TMP-HKNSBT1  NOT =  "2")
      *        保険単独分
               COMPUTE  WRK-TANDOKUTEN   =  RECE10-TOTALTEN(1)
                                         -  RECE10-TOTALTEN(2)
                                         -  RECE10-TOTALTEN(3)
                                         -  RECE10-TOTALTEN(4)
               COMPUTE  WRK-TANSEIKYUGK  =  WRK-TANDOKUTEN  
                                         * (10 - RECE10-KYURATE)
      *        保険単独分を加算
               COMPUTE  WRK-SEIKYUGK     =  WRK-SEIKYUGK
                                         +  WRK-TANSEIKYUGK
           END-IF
      *    
      *    振込金額
           IF  TA01TMP-HKNSBT1  =  "2"
             COMPUTE  WRK-FURIKOMI  =  RECE10-TOTALTEN(2)  *  10
                                    +  RECE10-SHOKUJIRYOYOHI(2)
           ELSE
             COMPUTE  WRK-FURIKOMI  =  RECE10-TOTALTEN(1)  *  10
                                    +  RECE10-SHOKUJIRYOYOHI(1)
                                    -  WRK-SEIKYUGK
           END-IF
           ADD  WRK-SEIKYUGK             TO  TA01TMP-ITBFTN
           ADD  WRK-FURIKOMI             TO  TA01TMP-FURIKOMI
      *
           .
       230-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理（公費請求）
      *****************************************************************
       240-KOH-SYUKEI-SEC       SECTION.
      *
           INITIALIZE                          FLG-BUNTEN
      *
           PERFORM  VARYING  IDY  FROM  1  BY  1
                    UNTIL    IDY  >     4
                    OR  RECE02-KOHNUM(IDY)  =  SPACE
      *
      *      特定疾患は０５１でまとめる
             IF  RECE02-KOHNUM(IDY)  =  "091"
               MOVE    "051"               TO  RECE02-KOHNUM(IDY)
             END-IF
      *
      *      公費番号まとめ情報があるとき
             IF  WRK-2007-MAX  >  ZERO
               MOVE  SPACE                 TO  WRK-KOHNAME
               PERFORM  VARYING  IDZ1  FROM  1  BY  1
                        UNTIL    IDZ1  >  WRK-2007-MAX
                 PERFORM  VARYING  IDZ2  FROM  1  BY  1
                          UNTIL    IDZ2  >  10
                          OR  WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                   IF  RECE02-KOHNUM(IDY)  =  WRK-2007-KOHNUM(IDZ1 IDZ2)
                     MOVE    WRK-2007-KOHNUM(IDZ1 1)
                                           TO  RECE02-KOHNUM(IDY)
                     MOVE    WRK-2007-MEISYO (IDZ1) 
                                           TO  WRK-KOHNAME
                     MOVE    10            TO  IDZ2
                     MOVE    WRK-2007-MAX  TO  IDZ1
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
      *
      *      公費番号
             MOVE    RECE02-KOHNUM(IDY)    TO  TA01TMP-KOHNUM(IDY)
                                               WRK-KOHNUM
      *
      *      公費負担者番号
             MOVE    RECE02-KOHFTNJANUM(IDY) 
                                           TO  TA01TMP-FTNJANUM(IDY)
      *      
      *      公費受給者番号
             MOVE    RECE02-KOHJKYSNUM(IDY) 
                                           TO  TA01TMP-JKYSNUM(IDY)
      *      
      *      公費−日数
             ADD  RECE02-JNISSU(IDY + 1)   TO  TA01TMP-K-NISSU(IDY)
      *
      *      公費−点数
             ADD  RECE02-TOTALTEN(IDY + 1) TO  TA01TMP-K-TENSU(IDY)
      *
      *      公費−一部負担
             ADD  RECE02-FTNMONEY(IDY + 1) TO  TA01TMP-K-ITBFTN(IDY)
      *
             IF  RECE02-NYUGAIKBN  =  "1"
      *      食事−療養費
               ADD  RECE02-SHOKUJIRYOYOHI(IDY + 1)
                                       TO  TA01TMP-K-SKJ-RYOYOHI(IDY)
      *
      *      食事−負担金
               ADD  RECE02-SHOKUJIFTN(IDY + 1)
                                       TO  TA01TMP-K-SKJ-FTN(IDY)
             END-IF
      *
      *      一部負担金保存
             MOVE     RECE02-FTNMONEY(IDY + 1) 
                                           TO  WRK-SEIKYUGK
      *
      *      分点判定
             IF  RECE10-TOTALTEN(1)  NOT =  RECE02-TOTALTEN(IDY + 1)
                 MOVE    1                 TO  FLG-BUNTEN
             END-IF
      *
           END-PERFORM
      *
      *    一部負担金再計算
           IF  (FLG-BUNTEN  =  1)
           AND (TA01TMP-HKNSBT1  NOT =  "2")
      *        保険単独分
               COMPUTE  WRK-TANDOKUTEN   =  RECE10-TOTALTEN(1)
                                         -  RECE02-TOTALTEN(2)
                                         -  RECE02-TOTALTEN(3)
                                         -  RECE02-TOTALTEN(4)
               COMPUTE  WRK-TANSEIKYUGK  =  WRK-TANDOKUTEN  
                                         * (10 - RECE10-KYURATE)
      *        保険単独分を加算
               COMPUTE  WRK-SEIKYUGK     =  WRK-SEIKYUGK
                                         +  WRK-TANSEIKYUGK
           END-IF
      *
      *    振込金額再計算
           IF  TA01TMP-HKNSBT1  =  "2"
             CONTINUE
           ELSE
             COMPUTE  WRK-FURIKOMI  =  RECE10-TOTALTEN(1)  *  10
                                    +  RECE10-SHOKUJIRYOYOHI(1)
                                    -  WRK-SEIKYUGK
           END-IF
           ADD  WRK-SEIKYUGK             TO  TA01TMP-ITBFTN
           ADD  WRK-FURIKOMI             TO  TA01TMP-FURIKOMI
      *    
           .
       240-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       300-PRINT-SEC                SECTION.
      *
      *    人数カウント
           PERFORM  3001-NINZU-CNT-SEC
      *
      *    総ページ数チェック
           PERFORM  305-PAGECHK-SEC
      *
           OPEN    INPUT  TA01-FILE
      *
           MOVE    ZERO        TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  390-TMP-READ-SEC
      *
           INITIALIZE              SUM-SYOKEI
                                   SUM-GOKEI
                                   WRK-HKNSBT
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *      KEY格納
             MOVE  KEY-NEW   TO  KEY-OLD
      *
      *      見出し印字
             PERFORM  310-HEAD-PRINT-SEC
      *      明細印字
             PERFORM  320-BODY-PRINT-SEC
      *
      *      合計印字
             IF  FLG-PRINT  =  1
             OR  KEY-NEW  NOT =  KEY-OLD
               PERFORM  330-GOKEI-PRINT-SEC
             END-IF
      *
      *      印刷処理
             PERFORM  340-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE  TA01-FILE
      *
      *    CSV処理
           OPEN    INPUT  TA01-FILE
      *
           MOVE    ZERO        TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  390-TMP-READ-SEC
      *
           INITIALIZE              SUM-SYOKEI
                                   SUM-GOKEI
                                   WRK-HKNSBT
      *
      *    CSVファイル編集
           PERFORM  300-CSV-HEN-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *      KEY格納
             MOVE  KEY-NEW   TO  KEY-OLD
      *
      *      見出し印字<CSV>
             PERFORM  310-CSV-HEAD-SEC
      *      明細印字<CSV>
             PERFORM  320-CSV-BODY-SEC
      *
      *      合計印字
             IF  FLG-PRINT  =  1
             OR  KEY-NEW  NOT =  KEY-OLD
               PERFORM  330-CSV-GOUKEI-SEC
             END-IF
           END-PERFORM
      *
           CLOSE  TA01-FILE
      *
      *
           .
       300-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    人数カウント処理
      *****************************************************************
       3001-NINZU-CNT-SEC                SECTION.
      *
      *    内部ソート
           PERFORM  VARYING  IDY1  FROM  1  BY  1
           UNTIL    IDY1  >  20000
           OR       SORT-PTNUM(IDY1)  =  SPACE
      *
             COMPUTE  WRK-S  =  IDY1  +  1
      *
             PERFORM  VARYING  IDY2  FROM  WRK-S  BY  1
             UNTIL    IDY2  >  20000
             OR       SORT-PTNUM(IDY2)  =  SPACE
      *
               IF  SORT-PTNUM(IDY1)  >  SORT-PTNUM(IDY2)
                 MOVE    SORT-PTNUM(IDY1)    TO  WRK-TAIHI
                 MOVE    SORT-PTNUM(IDY2)    TO  SORT-PTNUM(IDY1)
                 MOVE    WRK-TAIHI           TO  SORT-PTNUM(IDY2)
               END-IF
      *
             END-PERFORM
           END-PERFORM
      *
      *    実人数、延べ人数カウント
           INITIALIZE                            WRK-TAIHI
           PERFORM  VARYING  IDY1  FROM  1  BY  1
           UNTIL    IDY1  >  20000
           OR       SORT-PTNUM(IDY1)  =  SPACE
      *      延べ人数
             ADD    1                        TO  WRK-NNINZU
      *      実人数
             IF  SORT-PTNUM(IDY1)  NOT =  WRK-TAIHI
               ADD    1                      TO  WRK-JNINZU
               MOVE   SORT-PTNUM(IDY1)       TO  WRK-TAIHI
             END-IF
      *
           END-PERFORM
      *
           .
       3001-NINZU-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    総ページ数チェック処理
      *****************************************************************
       305-PAGECHK-SEC          SECTION.
      *
           INITIALIZE                        WRK-CNTLINE
      *                                       WRK-CNTPAGE
           MOVE    ZERO                     TO  WRK-CNTPAGE
      *
           OPEN  INPUT  TA01-FILE
           PERFORM  390-TMP-READ-SEC
      *
           MOVE    TA01TMP-HKNSBT-KEY    TO  WRK-HKNSBT
      *
      *    データ件数を数える
           PERFORM  UNTIL  FLG-PRINT  =  1
      *      KEY格納
             MOVE  KEY-NEW   TO  KEY-OLD
      *
      *      ページカウントアップ
             ADD    1                      TO  WRK-CNTPAGE
      *
      *      34行を超える及びキー区分が変わったらページカウント
             PERFORM  VARYING  WRK-CNTLINE  FROM  1  BY  1
                      UNTIL    WRK-CNTLINE  >  34
                      OR       KEY-NEW  NOT =  KEY-OLD
                      OR       FLG-PRINT  =  1
      *
      *        区分が変わったときのみ、保険種別を退避
               IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
                 MOVE  TA01TMP-HKNSBT-KEY  TO  WRK-HKNSBT
               END-IF
      *
      *        公費分考慮
               PERFORM  VARYING  IDY  FROM  1  BY  1
                        UNTIL    IDY  >  4
                        OR  TA01TMP-KOHNUM(IDY)  =  SPACE
      *          第二公費以降も行数カウント
                 IF  IDY  >  1
                   ADD  1                  TO  WRK-CNTLINE
                 END-IF
      *          第二公費以降が次ページにまたがる場合、ページカウント
                 IF  WRK-CNTLINE  >  34
                   ADD     1               TO  WRK-CNTPAGE
                   MOVE    1               TO  WRK-CNTLINE
                 END-IF
               END-PERFORM
      *
      *        次データ読み込み
               PERFORM  390-TMP-READ-SEC
      *
      *        区分が変わったら小計行カウント
               IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
               OR  FLG-PRINT  =  1
                 ADD    1                  TO  WRK-CNTLINE
      *          小計が次ページにまたがる場合、ページカウント
                 IF  WRK-CNTLINE  >  34
                   ADD     1               TO  WRK-CNTPAGE
                   MOVE    1               TO  WRK-CNTLINE
                 END-IF
               END-IF
      *
             END-PERFORM
      *
           END-PERFORM
      *
      *
      *
      **      1件カウント
      *       ADD  1                      TO  WRK-CNTLINE
      **      公費の行数分も考慮する
      *       PERFORM  VARYING  IDY  FROM  1  BY  1
      *                UNTIL    IDY  >  4
      *                OR  TA01TMP-KOHNUM(IDY)  =  SPACE
      **        第二公費以上を持っている場合、1件プラス
      *         IF  IDY  >  1
      *           ADD  1                  TO  WRK-CNTLINE
      *         END-IF
      *       END-PERFORM
      **
      **      次データ読み込み
      *       PERFORM  390-TMP-READ-SEC
      *
      *
      *      小計は一件プラス
      *       IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
      *       OR  FLG-PRINT  =  1
      *         ADD  1                    TO  WRK-CNTLINE
      *         MOVE    TA01TMP-HKNSBT-KEY  TO  WRK-HKNSBT
      *       END-IF
      *     END-PERFORM
      **
      **    総ページ数を計算
      *     IF  WRK-CNTLINE  NOT =  ZERO
      *       DIVIDE  WRK-CNTLINE  BY  34
      *               GIVING     WRK-CNTPAGE
      *               REMAINDER  WRK-AMARI
      *       IF  WRK-AMARI  NOT =  ZERO
      *         ADD  1           TO  WRK-CNTPAGE
      *       END-IF
      *     END-IF
      *
           CLOSE  TA01-FILE
      *
           .
       305-PAGECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し印字処理
      *****************************************************************
       310-HEAD-PRINT-SEC          SECTION.
      *
           INITIALIZE  TA01
      *
           ADD     1                     TO  CNT-PAGE
      *
      *    診療年月
           MOVE   TA01TMP-SRYYM          TO  WRK-SYMD(1:6)
           MOVE   "01"                   TO  WRK-SYMD(7:2)
           PERFORM 1001-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)     TO  WRK-SRYYMWH 
           MOVE    WRK-SRYYMWH           TO  TA01-SRYYM
      *
      *    作成年月日
           MOVE    WRK-SEIYMDWH          TO  TA01-CREATEYMD
      *
      *    サブタイトル
           IF  WRK-PARA-PREFKBN  NOT =  SPACE
             EVALUATE  WRK-PARA-PREFKBN
               WHEN  "1"
                 MOVE    "（県内）"      TO  TA01-SUBTITLE
               WHEN  "2"
                 MOVE    "（県外）"      TO  TA01-SUBTITLE
             END-EVALUATE
           END-IF
      *
      *    サブタイトル２
           IF  WRK-PARA-HKNSBT  NOT =  SPACE
             EVALUATE  WRK-PARA-HKNSBT
               WHEN  "1"
                 MOVE    "（社保）"      TO  TA01-SUBTITLE2
               WHEN  "2"
                 MOVE    "（国保）"      TO  TA01-SUBTITLE2
               WHEN  "3"
                 MOVE    "（退職国保）"  TO  TA01-SUBTITLE2
               WHEN  "4"
                 MOVE    "（後期高齢）"  TO  TA01-SUBTITLE2
               WHEN  "5"
                 MOVE    "（公費単独）"  TO  TA01-SUBTITLE2
             END-EVALUATE
           END-IF
      *
      *    入外区分
           EVALUATE  WRK-PARA-NYUGAIKBN
             WHEN  "1"
               MOVE    "入院"            TO  TA01-NYUGAI
             WHEN  "2"
               MOVE    "外来"            TO  TA01-NYUGAI
           END-EVALUATE
      *
      *    ページ数
           MOVE    CNT-PAGE              TO  WRK-Z2
           MOVE    WRK-Z2                TO  TA01-PAGE
      *
      *    合計ページ数
           MOVE    WRK-CNTPAGE           TO  WRK-Z2
           MOVE    WRK-Z2                TO  TA01-GPAGE
      *
           .
       310-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字処理
      *****************************************************************
       320-BODY-PRINT-SEC          SECTION.
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  34
                    OR       KEY-NEW  NOT =  KEY-OLD
                    OR       FLG-PRINT  =  1
      *
      *      区分が変わったときのみ、保険種別を印字
             IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
      *        保険種別１
               EVALUATE  TA01TMP-HKNSBT1
                 WHEN  "1"
                   MOVE    "社保"        TO  TA01-HKNSBT1(IDX1)
                 WHEN  "2"
                   MOVE    "公費"        TO  TA01-HKNSBT1(IDX1)
                 WHEN  "3"
                   MOVE    "国保"        TO  TA01-HKNSBT1(IDX1)
                 WHEN  "4"
                   MOVE    "退職"        TO  TA01-HKNSBT1(IDX1)
                 WHEN  "5"
                   MOVE    "後期"        TO  TA01-HKNSBT1(IDX1)
               END-EVALUATE
      *
      *        保険種別２
               EVALUATE  TA01TMP-HKNSBT2
                 WHEN  "1"
                   MOVE    "単独"      TO  TA01-HKNSBT2(IDX1)
                 WHEN  "2"
                   MOVE    "２併"      TO  TA01-HKNSBT2(IDX1)
                 WHEN  "3"
                   MOVE    "３併"      TO  TA01-HKNSBT2(IDX1)
               END-EVALUATE
      *
      *        本人家族区分
               EVALUATE  TA01TMP-HONKZK
                 WHEN  "1"
                 WHEN  "2"
                   MOVE    "本人"      TO  TA01-HONKZK(IDX1)
                 WHEN  "3"
                 WHEN  "4"
                   MOVE    "未就"      TO  TA01-HONKZK(IDX1)
                 WHEN  "5"
                 WHEN  "6"
                   MOVE    "家族"      TO  TA01-HONKZK(IDX1)
                 WHEN  "7"
                 WHEN  "8"
                   MOVE    "高一"      TO  TA01-HONKZK(IDX1)
                 WHEN  "9"
                 WHEN  "0"
                   MOVE    "高７"      TO  TA01-HONKZK(IDX1)
               END-EVALUATE
      *
      *        保険種別退避
               MOVE  TA01TMP-HKNSBT-KEY  TO  WRK-HKNSBT
             END-IF
      *
      *    ====================
      *           一般分
      *    ====================
      *      患者番号
             IF  TA01TMP-PTNUM(11:1)  NOT =  SPACE
               MOVE    TA01TMP-PTNUM     TO  TA01-PTNUM2(IDX1)
             ELSE
               MOVE    TA01TMP-PTNUM     TO  TA01-PTNUM(IDX1)
             END-IF
      *
      *      氏名
             MOVE    TA01TMP-NAME        TO  TA01-NAME(IDX1)
      *
      *      実日数
             MOVE    TA01TMP-NISSU       TO  WRK-Z3-9
             MOVE    WRK-Z3-9            TO  TA01-NISSU(IDX1)
      *
      *      点数
             MOVE    TA01TMP-TENSU       TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-TENSU(IDX1)
      *
      *      一部負担
             MOVE    TA01TMP-ITBFTN      TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-ITBFTN(IDX1)
      *
             IF  WRK-PARA-NYUGAIKBN  =  "1"
      *        食事−日数
               MOVE    TA01TMP-SKJ-NISSU TO  WRK-Z3-9
               MOVE    WRK-Z3-9          TO  TA01-SKJ-NISSU(IDX1)
      *
      *        食事−療養費
               MOVE    TA01TMP-SKJ-RYOYOHI  
                                         TO  WRK-Z9-9
               MOVE    WRK-Z9-9          TO  TA01-SKJ-RYOYOHI(IDX1)
      *
      *        食事−負担金
               MOVE    TA01TMP-SKJ-FTN   TO  WRK-Z9-9
               MOVE    WRK-Z9-9          TO  TA01-SKJ-FTN(IDX1)
             END-IF
      *
      *      室料差額
             MOVE    TA01TMP-RMSAGAKU    TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-RMSAGAKU(IDX1)
      *
      *      振込金額
             MOVE    TA01TMP-FURIKOMI    TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-FURIKOMI(IDX1)
      *
      *      保険者番号
             MOVE    TA01TMP-HKNJANUM    TO  TA01-HKNJANUM(IDX1)
      *
      *      診療科
      *       IF  (TA01TMP-SRYKA  NOT =  ZERO)
      *       AND (TA01TMP-SRYKA  NOT =  "99")
      **      診療科目情報取得
      *         INITIALIZE                    SYS-1005-REC
      *         MOVE    "1005"            TO  SYS-1005-KANRICD
      *         MOVE    TA01TMP-SRYKA     TO  SYS-1005-KBNCD
      *         MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                                   TO  SYS-1005-STYUKYMD(1:6)
      *                                       SYS-1005-EDYUKYMD(1:6)
      *         MOVE    "01"              TO  SYS-1005-STYUKYMD(7:2)
      *                                       SYS-1005-EDYUKYMD(7:2)
      *         MOVE    SPA-HOSPNUM       TO  SYS-1005-HOSPNUM
      *         MOVE    SYS-1005-REC      TO  MCPDATA-REC
      *         PERFORM 900-SYSKANRI-READ-SEC
      *         IF  FLG-SYSKANRI  =  ZERO
      *           MOVE    MCPDATA-REC     TO  SYS-1005-REC
      *           MOVE    SYS-1005-SRYKANAME3 
      *                                   TO  TA01-SRYKA(IDX1)
      *         END-IF
      *       END-IF
      *
      *      診療月
             IF  TA01TMP-HENREIYM  NOT =  SPACE
               STRING  TA01TMP-HENREIYM(1:4) DELIMITED  BY  SIZE
                       "."                   DELIMITED  BY  SIZE
                       TA01TMP-HENREIYM(5:2) DELIMITED  BY  SIZE
                       INTO                  TA01-HENREIYM(IDX1)
               END-STRING
             END-IF
      *
      *      合計集計
             ADD  TA01TMP-NISSU          TO  SUM-SNISSU
                                             SUM-GNISSU
             ADD  TA01TMP-TENSU          TO  SUM-STENSU
                                             SUM-GTENSU
             ADD  TA01TMP-ITBFTN         TO  SUM-SITBFTN
                                             SUM-GITBFTN
             ADD  TA01TMP-SKJ-NISSU      TO  SUM-SSKJ-NISSU
                                             SUM-GSKJ-NISSU
             ADD  TA01TMP-SKJ-RYOYOHI    TO  SUM-SSKJ-RYOYOHI
                                             SUM-GSKJ-RYOYOHI
             ADD  TA01TMP-SKJ-FTN        TO  SUM-SSKJ-FTN
                                             SUM-GSKJ-FTN
             ADD  TA01TMP-RMSAGAKU       TO  SUM-SRMSAGAKU
                                             SUM-GRMSAGAKU
             ADD  TA01TMP-FURIKOMI       TO  SUM-SFURIKOMI
                                             SUM-GFURIKOMI
      *
      *    ====================
      *           公費分
      *    ====================
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL    IDY  >  4
                      OR  TA01TMP-KOHNUM(IDY)  =  SPACE
      *
      *        第二公費以降は次行に印字
               IF  IDY  >  1
                 ADD  1                  TO  IDX1
               END-IF
      *
      *        第二公費以降が次ページにまたがる場合、改ページする
               IF  IDX1  >  34
      *          印刷処理
                 PERFORM  340-PRINT-OUT-SEC
      *          見出し印字
                 PERFORM  310-HEAD-PRINT-SEC
                 MOVE    1               TO  IDX1
               END-IF
      *
      *        公費−法別
               MOVE    TA01TMP-KOHNUM(IDY)
                                         TO  WRK-Z3
               MOVE    WRK-Z3            TO  TA01-KOHNUM(IDX1)
      *
      *        公費−負担者番号
               MOVE    TA01TMP-FTNJANUM(IDY)
                                         TO  TA01-FTNJANUM(IDX1)
      *        公費−受給者番号
               MOVE    TA01TMP-JKYSNUM(IDY)
                                         TO  TA01-JKSNUM(IDX1)
      *
      *        公費−実日数
               MOVE    TA01TMP-K-NISSU(IDY)
                                         TO  WRK-Z3-9
               MOVE    WRK-Z3-9          TO  TA01-K-NISSU(IDX1)
      *
      *        公費−点数
               MOVE    TA01TMP-K-TENSU(IDY) 
                                         TO  WRK-Z9-9
               MOVE    WRK-Z9-9          TO  TA01-K-TENSU(IDX1)
      *
      *        公費−一部負担
               MOVE    TA01TMP-K-ITBFTN(IDY)
                                         TO WRK-Z9-9
               MOVE    WRK-Z9-9          TO  TA01-K-ITBFTN(IDX1)
      *
               IF  WRK-PARA-NYUGAIKBN  =  "1"
      *          公費−食事金額
                 MOVE    TA01TMP-K-SKJ-RYOYOHI(IDY)
                                         TO  WRK-Z9-9
                 MOVE    WRK-Z9-9        TO  TA01-K-SKJ-RYOYOHI(IDX1)
      *
      *          公費−食事負担
                 MOVE   TA01TMP-K-SKJ-FTN(IDY) 
                                         TO  WRK-Z9-9
                 MOVE   WRK-Z9-9         TO  TA01-K-SKJ-FTN(IDX1)
               END-IF
      *
      *        合計集計
               ADD  TA01TMP-K-NISSU(IDY) TO  SUM-SK-NISSU
                                             SUM-GK-NISSU
               ADD  TA01TMP-K-TENSU(IDY) TO  SUM-SK-TENSU
                                             SUM-GK-TENSU
               ADD  TA01TMP-K-ITBFTN(IDY)
                                         TO  SUM-SK-ITBFTN
                                             SUM-GK-ITBFTN
               ADD  TA01TMP-K-SKJ-RYOYOHI(IDY)
                                         TO  SUM-SK-SKJ-RYOYOHI
                                             SUM-GK-SKJ-RYOYOHI
               ADD  TA01TMP-K-SKJ-FTN(IDY)
                                         TO  SUM-SK-SKJ-FTN
                                             SUM-GK-SKJ-FTN
             END-PERFORM
      *
      *      次データ読み込み
             PERFORM  390-TMP-READ-SEC
      *
      *      区分が変わったら1行送って小計印字
             IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
             OR  FLG-PRINT  =  1
               PERFORM  325-SYOKEI-PRINT-SEC
             END-IF
      *
           END-PERFORM
      *
           .
       320-BODY-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    小計印字処理
      *****************************************************************
       325-SYOKEI-PRINT-SEC          SECTION.
      *
           ADD  1                        TO  IDX1
      *
      *    小計が次ページにまたがる場合、改ページする
           IF  IDX1  >  34
      *      印刷処理
             PERFORM  340-PRINT-OUT-SEC
      *      見出し印字
             PERFORM  310-HEAD-PRINT-SEC
             MOVE    1               TO  IDX1
           END-IF
      *
      *    「小計」印字
           MOVE    "　　　　　　　　小計"  TO  TA01-NAME(IDX1)
      *
      *    実日数
           MOVE    SUM-SNISSU            TO  WRK-Z3-9
           MOVE    WRK-Z3-9              TO  TA01-NISSU(IDX1)
      *
      *    点数
           MOVE    SUM-STENSU            TO  WRK-Z9-9
           MOVE    WRK-Z9-9              TO  TA01-TENSU(IDX1)
      *
      *    一部負担
           MOVE    SUM-SITBFTN           TO  WRK-Z9-9
           MOVE    WRK-Z9-9              TO  TA01-ITBFTN(IDX1)
      *
           IF  WRK-PARA-NYUGAIKBN  =  "1"
      *      食事−日数
             MOVE    SUM-SSKJ-NISSU      TO  WRK-Z3-9
             MOVE    WRK-Z3-9            TO  TA01-SKJ-NISSU(IDX1)
      *
      *      食事−療養費
             MOVE    SUM-SSKJ-RYOYOHI    TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-SKJ-RYOYOHI(IDX1)
      *
      *      食事−負担金
             MOVE    SUM-SSKJ-FTN        TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-SKJ-FTN(IDX1)
           END-IF
      *
      *    室料差額
           MOVE    SUM-SRMSAGAKU         TO  WRK-Z9-9
           MOVE    WRK-Z9-9              TO  TA01-RMSAGAKU(IDX1)
      *
      *    振込金額
           MOVE    SUM-SFURIKOMI         TO  WRK-Z9-9
           MOVE    WRK-Z9-9              TO  TA01-FURIKOMI(IDX1)
      *
      *    公費−実日数
           MOVE    SUM-SK-NISSU          TO  WRK-Z3-9
           MOVE    WRK-Z3-9              TO  TA01-K-NISSU(IDX1)
      *
      *    公費−点数
           MOVE    SUM-SK-TENSU          TO  WRK-Z9-9
           MOVE    WRK-Z9-9              TO  TA01-K-TENSU(IDX1)
      *
      *    公費−一部負担
           MOVE    SUM-SK-ITBFTN         TO  WRK-Z9-9
           MOVE    WRK-Z9-9              TO  TA01-K-ITBFTN(IDX1)
      *
           IF  WRK-PARA-NYUGAIKBN  =  "1"
      *      公費−食事金額
             MOVE    SUM-SK-SKJ-RYOYOHI  TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-K-SKJ-RYOYOHI(IDX1)
      *
      *      公費−食事負担
             MOVE    SUM-SK-SKJ-FTN      TO  WRK-Z9-9
             MOVE    WRK-Z9-9            TO  TA01-K-SKJ-FTN(IDX1)
           END-IF
      *
      *    小計初期化
           INITIALIZE                        SUM-SYOKEI
      *
           .
       325-SYOKEI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    総合計印字処理
      *****************************************************************
       330-GOKEI-PRINT-SEC          SECTION.
      *
      *    実日数
           MOVE    SUM-GNISSU            TO  WRK-Z4-9
           MOVE    WRK-Z4-9              TO  TA01-GNISSU
      *
      *    点数
           MOVE    SUM-GTENSU            TO  WRK-Z10-9
           MOVE    WRK-Z10-9             TO  TA01-GTENSU
      *
      *    一部負担
           MOVE    SUM-GITBFTN           TO  WRK-Z10-9
           MOVE    WRK-Z10-9             TO  TA01-GITBFTN
      *
           IF  WRK-PARA-NYUGAIKBN  =  "1"
      *      食事−日数
             MOVE    SUM-GSKJ-NISSU      TO  WRK-Z4-9
             MOVE    WRK-Z4-9            TO  TA01-GSKJ-NISSU
      *
      *      食事−療養費
             MOVE    SUM-GSKJ-RYOYOHI    TO  WRK-Z10-9
             MOVE    WRK-Z10-9           TO  TA01-GSKJ-RYOYOHI
      *
      *      食事−負担金
             MOVE    SUM-GSKJ-FTN        TO  WRK-Z10-9
             MOVE    WRK-Z10-9           TO  TA01-GSKJ-FTN
           END-IF
      *
      *    室料差額
           MOVE    SUM-GRMSAGAKU         TO  WRK-Z10-9
           MOVE    WRK-Z10-9             TO  TA01-GRMSAGAKU
      *
      *    振込金額
           MOVE    SUM-GFURIKOMI         TO  WRK-Z10-9
           MOVE    WRK-Z10-9             TO  TA01-GFURIKOMI
      *
      *    公費−実日数
           MOVE    SUM-GK-NISSU          TO  WRK-Z4-9
           MOVE    WRK-Z4-9              TO  TA01-GK-NISSU
      *
      *    公費−点数
           MOVE    SUM-GK-TENSU          TO  WRK-Z10-9
           MOVE    WRK-Z10-9             TO  TA01-GK-TENSU
      *
      *    公費−一部負担
           MOVE    SUM-GK-ITBFTN         TO  WRK-Z10-9
           MOVE    WRK-Z10-9             TO  TA01-GK-ITBFTN
      *
           IF  WRK-PARA-NYUGAIKBN  =  "1"
      *      公費−食事金額
             MOVE    SUM-GK-SKJ-RYOYOHI  TO  WRK-Z10-9
             MOVE    WRK-Z10-9           TO  TA01-GK-SKJ-RYOYOHI
      *
      *      公費−食事負担
             MOVE    SUM-GK-SKJ-FTN      TO  WRK-Z10-9
             MOVE    WRK-Z10-9           TO  TA01-GK-SKJ-FTN
           END-IF
      *
      *    実人数
           MOVE    WRK-JNINZU            TO  WRK-Z5-9
           MOVE    WRK-Z5-9              TO  TA01-JNINZU2
      *     
      *    延べ人数
           MOVE    WRK-NNINZU            TO  WRK-Z5-9
           MOVE    WRK-Z5-9              TO  TA01-NNINZU2
      *
           INITIALIZE                        SUM-GOKEI
      *
           .
       330-GOKEI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO  SPRT-PRIORITY
           MOVE    "A00000TA01.red"           TO  SPRT-PRTID
           MOVE    "総括表 患者別"         TO  SPRT-TITLE
           MOVE    TA01                 TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO  SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVファイル編集
      *****************************************************************
       300-CSV-HEN-SEC               SECTION.
      *
           INITIALIZE                     CSV-DIR
                                          CSV-NAME
           MOVE    "/var/tmp/"            TO  CSV-DIR
      *
           MOVE    SPACE                  TO  CSV-NAME
           IF  WRK-PARA-PRTYMED  NOT =  SPACE
             STRING  SPA-HOSPNUM          DELIMITED  BY  SIZE
                     "sokatuptlist"       DELIMITED  BY  SIZE
                     WRK-PARA-PRTYMST     DELIMITED  BY  SIZE
                     "-"                  DELIMITED  BY  SIZE
                     WRK-PARA-PRTYMED     DELIMITED  BY  SIZE
                     ".csv"               DELIMITED  BY  SIZE
                                          INTO  CSV-NAME
             END-STRING
           ELSE
             STRING  SPA-HOSPNUM          DELIMITED  BY  SIZE
                     "sokatuptlist"       DELIMITED  BY  SIZE
                     WRK-PARA-PRTYMST     DELIMITED  BY  SIZE
                     ".csv"               DELIMITED  BY  SIZE
                                          INTO  CSV-NAME
             END-STRING
           END-IF
      *
           .
       300-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダーファイル編集
      *****************************************************************
       310-CSV-HEAD-SEC              SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *
      *    集計対象年月日
      *    開始月日
           MOVE    WRK-PARA-PRTYMST      TO  WRK-SYMD(1:6)
           MOVE    "01"                  TO  WRK-SYMD(7:2)
           PERFORM  1001-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  CSV-DATA(1:22)
           MOVE    "　〜　"              TO  CSV-DATA(23:6)
      *    終了月日
           IF  WRK-PARA-PRTYMED  NOT =  SPACE
             MOVE    WRK-PARA-PRTYMED    TO  WRK-SYMD(1:6)
           ELSE
             MOVE    WRK-PARA-PRTYMST    TO  WRK-SYMD(1:6)
           END-IF
           PERFORM 31012-LASTDAY-GET-SEC
           PERFORM  1001-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  CSV-DATA(29:22)
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    印刷種別・印字区分
           EVALUATE  WRK-PARA-NYUGAIKBN
             WHEN  "1"
               MOVE    "入院"            TO  CSV-DATA
               PERFORM  510-CSV-DATAHEN-SEC
             WHEN  "2"
               MOVE    "外来"            TO  CSV-DATA
               PERFORM  510-CSV-DATAHEN-SEC
           END-EVALUATE
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL  IDX3  >  CSV-HEAD-MAX
      *
             MOVE    CSV-HEAD-ITEM(IDX3)    TO  CSV-DATA
      *
             IF  IDX3  >=  CSV-HEAD-MAX
               PERFORM  510-CSV-DATAHEN-SEC
             ELSE
               PERFORM  500-CSV-DATAHEN-SEC
             END-IF
      *
           END-PERFORM
      *
           PERFORM  500-CSV-OUTPUT-SEC
      *
           .
       500-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV明細編集
      *****************************************************************
       320-CSV-BODY-SEC               SECTION.
      *
           PERFORM  UNTIL    KEY-NEW  NOT =  KEY-OLD
                    OR       FLG-PRINT  =  1
      *
             MOVE    1      TO  IDX-POINT
      *
             IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
      *        保険種別退避
               MOVE  TA01TMP-HKNSBT-KEY  TO  WRK-HKNSBT
             END-IF
      *
      *      ====================
      *             一般分
      *      ====================
      *      保険種別１
             EVALUATE  TA01TMP-HKNSBT1
               WHEN  "1"
                 MOVE    "社保"            TO  CSV-DATA
               WHEN  "2"
                 MOVE    "公費"            TO  CSV-DATA
               WHEN  "3"
                 MOVE    "国保"            TO  CSV-DATA
               WHEN  "4"
                 MOVE    "退職"            TO  CSV-DATA
               WHEN  "5"
                 MOVE    "後期"            TO  CSV-DATA
             END-EVALUATE
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      保険種別２
             EVALUATE  TA01TMP-HKNSBT2
               WHEN  "1"
                 MOVE    "単独"            TO  CSV-DATA
               WHEN  "2"
                 MOVE    "２併"            TO  CSV-DATA
               WHEN  "3"
                 MOVE    "３併"            TO  CSV-DATA
             END-EVALUATE
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      本人家族区分
             EVALUATE  TA01TMP-HONKZK
               WHEN  "1"
               WHEN  "2"
                 MOVE    "本人"            TO  CSV-DATA
               WHEN  "3"
               WHEN  "4"
                 MOVE    "未就"            TO  CSV-DATA
               WHEN  "5"
               WHEN  "6"
                 MOVE    "家族"            TO  CSV-DATA
               WHEN  "7"
               WHEN  "8"
                 MOVE    "高一"            TO  CSV-DATA
               WHEN  "9"
               WHEN  "0"
                 MOVE    "高７"            TO  CSV-DATA
             END-EVALUATE
             PERFORM  500-CSV-DATAHEN-SEC
      *      
      *      患者番号
             MOVE    TA01TMP-PTNUM         TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      氏名
             MOVE    TA01TMP-NAME          TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      保険者番号
             MOVE    TA01TMP-HKNJANUM      TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      実日数
             MOVE    TA01TMP-NISSU         TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      点数
             MOVE    TA01TMP-TENSU         TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      一部負担
             MOVE    TA01TMP-ITBFTN        TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      食事−日数
             MOVE    TA01TMP-SKJ-NISSU     TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      食事−療養費
             MOVE    TA01TMP-SKJ-RYOYOHI   TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      食事−負担金
             MOVE    TA01TMP-SKJ-FTN       TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      室料差額
             MOVE    TA01TMP-RMSAGAKU      TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *      振込金額
             MOVE    TA01TMP-FURIKOMI      TO  CSV-DATA
             PERFORM  500-CSV-DATAHEN-SEC
      *
      *        合計集計
             ADD  TA01TMP-NISSU            TO  SUM-SNISSU
                                               SUM-GNISSU
             ADD  TA01TMP-TENSU            TO  SUM-STENSU
                                               SUM-GTENSU
             ADD  TA01TMP-ITBFTN           TO  SUM-SITBFTN
                                               SUM-GITBFTN
             ADD  TA01TMP-SKJ-NISSU        TO  SUM-SSKJ-NISSU
                                               SUM-GSKJ-NISSU
             ADD  TA01TMP-SKJ-RYOYOHI      TO  SUM-SSKJ-RYOYOHI
                                               SUM-GSKJ-RYOYOHI
             ADD  TA01TMP-SKJ-FTN          TO  SUM-SSKJ-FTN
                                               SUM-GSKJ-FTN
             ADD  TA01TMP-RMSAGAKU         TO  SUM-SRMSAGAKU
                                               SUM-GRMSAGAKU
             ADD  TA01TMP-FURIKOMI         TO  SUM-SFURIKOMI
                                               SUM-GFURIKOMI
      *
      *      ====================
      *             公費分
      *      ====================
             PERFORM  VARYING  IDY  FROM  1  BY  1
                      UNTIL    IDY  >  4
      *                OR  TA01TMP-KOHNUM(IDY)  =  SPACE
      *
      *        公費−法別
               MOVE    TA01TMP-KOHNUM(IDY) 
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−負担者番号
               MOVE    TA01TMP-FTNJANUM(IDY)
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−受給者番号
               MOVE    TA01TMP-JKYSNUM(IDY)
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−実日数
               MOVE    TA01TMP-K-NISSU(IDY)
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−点数
               MOVE    TA01TMP-K-TENSU(IDY) 
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−一部負担
               MOVE    TA01TMP-K-ITBFTN(IDY)
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−食事金額
               MOVE    TA01TMP-K-SKJ-RYOYOHI(IDY)
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        公費−食事負担
               MOVE   TA01TMP-K-SKJ-FTN(IDY) 
                                           TO  CSV-DATA
               PERFORM  500-CSV-DATAHEN-SEC
      *
      *        合計集計
               ADD  TA01TMP-K-NISSU(IDY)   TO  SUM-SK-NISSU
                                               SUM-GK-NISSU
               ADD  TA01TMP-K-TENSU(IDY)   TO  SUM-SK-TENSU
                                               SUM-GK-TENSU
               ADD  TA01TMP-K-ITBFTN(IDY)  TO  SUM-SK-ITBFTN
                                               SUM-GK-ITBFTN
               ADD  TA01TMP-K-SKJ-RYOYOHI(IDY)
                                           TO  SUM-SK-SKJ-RYOYOHI
                                               SUM-GK-SKJ-RYOYOHI
               ADD  TA01TMP-K-SKJ-FTN(IDY) TO  SUM-SK-SKJ-FTN
                                               SUM-GK-SKJ-FTN
             END-PERFORM
      *
      *      診療月
             IF  TA01TMP-HENREIYM  NOT =  SPACE
               STRING  TA01TMP-HENREIYM(1:4) DELIMITED  BY  SIZE
                       "."                   DELIMITED  BY  SIZE
                       TA01TMP-HENREIYM(5:2) DELIMITED  BY  SIZE
                       INTO                  CSV-DATA
               END-STRING
             ELSE
               MOVE    SPACE               TO  CSV-DATA
             END-IF
             PERFORM  510-CSV-DATAHEN-SEC
      *
             PERFORM  500-CSV-OUTPUT-SEC
      *
      *      次データ読み込み
             PERFORM  390-TMP-READ-SEC
      *
      *      区分が変わったら1行送って小計印字
             IF  TA01TMP-HKNSBT-KEY  NOT =  WRK-HKNSBT
             OR  FLG-PRINT  =  1
               PERFORM  325-CSV-SYOKEI-SEC
             END-IF
      *
           END-PERFORM
      *
           .
       320-CSV-BODY--EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV小計集計
      *****************************************************************
       325-CSV-SYOKEI-SEC           SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *
      *    空白４つ
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    名称
           MOVE    "小計"                TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    保険者番号（空白）
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *    
      *    実日数
           MOVE    SUM-SNISSU            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    点数
           MOVE    SUM-STENSU            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    一部負担
           MOVE    SUM-SITBFTN           TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    食事−日数
           MOVE    SUM-SSKJ-NISSU        TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    食事−療養費
           MOVE    SUM-SSKJ-RYOYOHI      TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    食事−負担金
           MOVE    SUM-SSKJ-FTN          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    室料差額
           MOVE    SUM-SRMSAGAKU         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    振込金額
           MOVE    SUM-SFURIKOMI         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    空白3つ
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *    
      *    公費−実日数
           MOVE    SUM-SK-NISSU          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    公費−点数
           MOVE    SUM-SK-TENSU          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    公費−一部負担
           MOVE    SUM-SK-ITBFTN         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *      公費−食事金額
           MOVE    SUM-SK-SKJ-RYOYOHI    TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *      公費−食事負担
           MOVE    SUM-SK-SKJ-FTN        TO  CSV-DATA
           PERFORM  510-CSV-DATAHEN-SEC
      *
      *    小計初期化
           INITIALIZE                        SUM-SYOKEI
      *
           PERFORM  500-CSV-OUTPUT-SEC
      *
           .
       325-CSV-SYOKEI-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計集計
      *****************************************************************
       330-CSV-GOUKEI-SEC           SECTION.
      *
           MOVE    1      TO  IDX-POINT
      *
      *    実人数
           MOVE    "実人数"              TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    WRK-JNINZU            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *     
      *    延べ人数
           MOVE    "延べ人数"            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    WRK-NNINZU            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    総合計
           MOVE    "総合計"              TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    保険者番号（空白）
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *    
      *    実日数
           MOVE    SUM-GNISSU            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    点数
           MOVE    SUM-GTENSU            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    一部負担
           MOVE    SUM-GITBFTN           TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    食事−日数
           MOVE    SUM-GSKJ-NISSU        TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    食事−療養費
           MOVE    SUM-GSKJ-RYOYOHI      TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    食事−負担金
           MOVE    SUM-GSKJ-FTN          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    室料差額
           MOVE    SUM-GRMSAGAKU         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    振込金額
           MOVE    SUM-GFURIKOMI         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    空白2つと"公費合計"
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    SPACE                 TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
           MOVE    "公費合計"            TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *    
      *    公費−実日数
           MOVE    SUM-GK-NISSU          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    公費−点数
           MOVE    SUM-GK-TENSU          TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    公費−一部負担
           MOVE    SUM-GK-ITBFTN         TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    公費−食事金額
           MOVE    SUM-GK-SKJ-RYOYOHI    TO  CSV-DATA
           PERFORM  500-CSV-DATAHEN-SEC
      *
      *    公費−食事負担
           MOVE    SUM-GK-SKJ-FTN        TO  CSV-DATA
           PERFORM  510-CSV-DATAHEN-SEC
      *
           INITIALIZE                        SUM-GOKEI
      *
           PERFORM  500-CSV-OUTPUT-SEC
      *
           .
       330-CSV-GOUKEI-SEC-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       500-CSV-DATAHEN-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       500-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       510-CSV-DATAHEN-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
           .
      *
       510-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF  CSV-DATA  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  CSV-DATA(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-OUTPUT-DATA
                                              WITH  POINTER  IDX-POINT
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-OUTPUT-DATA
                               WITH  POINTER  IDX-POINT
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-OUTPUT-DATA
                               WITH  POINTER  IDX-POINT
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-DATA-SIZE
                    OR  CSV-DATA(IDX4:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  CSV-DATA(IDX4:1)  NOT =  SPACE
                 MOVE    IDX4      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
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
      *    ＣＳＶ出力処理
      *****************************************************************
       500-CSV-OUTPUT-SEC              SECTION.
      *
           INITIALIZE                        ORCSTOUKEICSVAREA
           MOVE    "INS"                     TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY      TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP    TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID      TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM        TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN        TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                      TO  STOUKEICSV-PTID
           MOVE    CSV-DIR                   TO  STOUKEICSV-TO-FOLDER
           MOVE    CSV-NAME                  TO  STOUKEICSV-TO-DATA
           MOVE    "2"                       TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-OUTPUT-DATA           TO  STOUKEICSV-CSVDATA
           MOVE    "総括表 患者別ＣＳＶ作成"  TO  STOUKEICSV-TITLE
           CALL    "ORCSTOUKEICSV"           USING
                                             ORCSTOUKEICSVAREA
                                             SPA-AREA
      *
           IF   STOUKEICSV-RETURN  =  ZERO
             CONTINUE
           ELSE
             MOVE    "ＣＳＶＤＢに更新できませんでした"  TO  WRK-RECEERR
             PERFORM  800-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  WRK-OUTPUT-DATA
      *
           .
       500-CSV-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       390-TMP-READ-SEC  SECTION.
      *
           READ  TA01-FILE   NEXT
             AT  END
               MOVE    1                    TO  FLG-PRINT
             NOT  AT  END
               MOVE  TA01TMP-SRYYM          TO  KEY-NEW-SRYYM
           END-READ
      *
           .
       390-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       400-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"                TO  SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE             TO  JOB-UPDCNT
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       400-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       800-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT             RECEERR-FILE
      *
             MOVE    WRK-RECEERR        TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                   JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
             MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           MOVE    1           TO  FLG-END
      *
           .
       800-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計用TMPファイル読込
      *****************************************************************
       800-STXTMP-READ-SEC             SECTION.
      *
           READ  TA01-FILE
             INVALID    KEY
               MOVE    1           TO  FLG-TA01TMP
             NOT INVALID  KEY
               MOVE    ZERO        TO  FLG-TA01TMP
           END-READ
      *
           .
       800-STXTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    "DBFETCH"        TO  MCP-FUNC
             MOVE    "tbl_syskanri"   TO  MCP-TABLE
             MOVE    "key10"          TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE    ZERO           TO  FLG-SYSKANRI
             ELSE
               MOVE    1              TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key10"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-SELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"       TO  MCP-FUNC
           MOVE    "tbl_syskanri"   TO  MCP-TABLE
           MOVE    "key9"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM  910-SYSKANRI-FETCH-SEC 
           ELSE
             MOVE    1              TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE    "tbl_syskanri"    TO  MCP-TABLE
           MOVE    "key9"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
             MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       910-SYSKANRI-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    WRK-PRTYYST         TO  RECE10-SKYYM(1:4)
           MOVE    WRK-PRTMMST         TO  RECE10-SKYYM(5:2)
           MOVE    WRK-TEISYUTUSAKI    TO  RECE10-TEISYUTUSAKI
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM 910-SEIKYU-READ-SEC
           ELSE
             MOVE    1                 TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       910-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO              TO  FLG-END
             MOVE    MCPDATA-REC       TO  RECE10-REC
      *
      *    ====================
      *       データチェック   
      *    ====================
      *      テスト患者のときは読み飛ばす
             MOVE    SPACE             TO  PTINF-REC
             INITIALIZE                    PTINF-REC
             MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
             MOVE    RECE10-PTID       TO  PTINF-PTID
             MOVE    PTINF-REC         TO  MCPDATA-REC
             PERFORM 900-PTINF-READ-SEC
             IF  FLG-PTINF  =  ZERO
             AND PTINF-TSTPTNUMKBN  =  "1"
                 GO  TO  910-SEIKYU-READ-SEC
             END-IF
      *
      *      該当入外区分でなければ処理対象外
             EVALUATE  WRK-PARA-NYUGAIKBN
               WHEN  "1"
                 IF  RECE10-NYUGAIKBN  NOT =  "1"
                   GO  TO  910-SEIKYU-READ-SEC
                 END-IF
               WHEN  "2"
                 IF  RECE10-NYUGAIKBN  NOT =  "2"
                   GO  TO  910-SEIKYU-READ-SEC
                 END-IF
             END-EVALUATE
      *
      *      該当保険種別でなければ処理対象外
             IF  WRK-PARA-HKNSBT  NOT =  SPACE
               EVALUATE  WRK-PARA-HKNSBT
                 WHEN  "2"
                   IF  RECE10-HKNNUM  NOT =  "060" AND "068"
                     GO  TO  910-SEIKYU-READ-SEC
                   END-IF
                 WHEN  "3"
                   IF  RECE10-HKNNUM  NOT =  "067" AND "069"
                     GO  TO  910-SEIKYU-READ-SEC
                   END-IF
                 WHEN  "1"
                   IF  RECE10-HKNJANUM  =  ZERO
                     GO  TO  910-SEIKYU-READ-SEC
                   END-IF
                 WHEN  "5"
                   IF  RECE10-HKNJANUM  NOT =  ZERO
                     GO  TO  910-SEIKYU-READ-SEC
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      該当保険者番号でなければ処理対象外
             IF  WRK-PARA-HKNJANUM  NOT =  SPACE
               IF  WRK-PARA-HKNJANUM  NOT =  RECE10-HKNJANUM
                 GO  TO  910-SEIKYU-READ-SEC
               END-IF
             END-IF
      *
      *      該当公費番号でなければ処理対象外
             IF  WRK-PARA-KOHNUM  NOT =  SPACE
               IF  WRK-PARA-KOHNUM  NOT =  RECE10-KOHNUM(1)
                                      AND  RECE10-KOHNUM(2)
                                      AND  RECE10-KOHNUM(3)
                                      AND  RECE10-KOHNUM(4)
                 GO  TO  910-SEIKYU-READ-SEC
               END-IF
             END-IF
      *
      *      該当県内県外区分でなければ処理対象外
             IF  WRK-PARA-PREFKBN  NOT =  SPACE
               IF  WRK-PARA-PREFKBN  NOT =  RECE10-PREFKBN
                 GO  TO  910-SEIKYU-READ-SEC
               END-IF
             END-IF
      *      
      *      返戻区分がチェックされている場合、返戻でなければ処理対象外
             IF  WRK-PARA-HENREI  =  "1"
               IF  RECE10-HENREIYM  =  ZERO
                 GO  TO  910-SEIKYU-READ-SEC
               END-IF
             END-IF
      *      
      *      公害保険は読み飛ばす
             IF  RECE10-HKNNUM  =  "975"
               GO  TO  910-SEIKYU-READ-SEC
             END-IF
      *
             ADD     1                 TO  CNT-RECE10
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       910-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM               TO  PTKOH-HOSPNUM
           MOVE  RECE10-PTID               TO  PTKOH-PTID
           MOVE  WRK-KOHNUM                TO  PTKOH-KOHNUM
      *
           MOVE  PTKOHINF-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
             MOVE    "key4"                TO  MCP-PATHNAME
             PERFORM  910-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
               IF  PTKOH-SAKJOKBN  NOT = "1"
                 IF  PTKOH-TEKSTYMD(1:6) <= RECE10-SRYYM(1:6)  AND
                     PTKOH-TEKEDYMD(1:6) >= RECE10-SRYYM(1:6)
      *              負担者番号と受給者番号を取得
                     MOVE  PTKOH-FTNJANUM  TO  WRK-FTNJANUM
                     MOVE  PTKOH-JKYSNUM   TO  WRK-JKYSNUM
                 END-IF
               END-IF
                 MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                 MOVE    "key4"            TO  MCP-PATHNAME
                 PERFORM  910-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報ＦＥＴＣＨ
      *****************************************************************
       910-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"                 TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC           TO  PTKOHINF-REC
             MOVE    ZERO                  TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE    1                     TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報取得
      *****************************************************************
       900-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  SYUNOU-REC
                       WRK-RMSAGAKU
                       WRK-SEIKYUGK
      *
      *    収納情報SELECT
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    RECE10-PTID         TO  SYU-PTID
           MOVE    RECE10-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
           MOVE    "__"                TO  SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
      *    収納情報FETCH
             PERFORM  910-SYUNOU-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT =  "3"
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
      *            室料差額集計
                   ADD    SYU-RMSAGAKU     TO  WRK-RMSAGAKU
      *
      *            主保険負担金集計
                   ADD    SYU-SYUCOMPFTN-ENTANI   TO  WRK-SEIKYUGK
                 END-IF
               END-IF
      *   
               PERFORM  910-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
      *    収納情報CLOSE
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       910-SYUNOU-READ-SEC  SECTION.
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  SYUNOU-REC
             MOVE    ZERO              TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE    1                 TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報取得（請求金額計算用）
      *****************************************************************
       900-SYUNOU2-SET-SEC  SECTION.
      *
           INITIALIZE                      SYUNOU-REC
      *
      *    収納情報SELECT
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    RECE10-PTID         TO  SYU-PTID
           MOVE    RECE10-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
           MOVE    "__"                TO  SYU-SRYYMD(7:2)
           MOVE    WRK-KOHNUM          TO  SYU-KOH-HKNNUM(1)
                                           SYU-KOH-HKNNUM(2)
                                           SYU-KOH-HKNNUM(3)
                                           SYU-KOH-HKNNUM(4)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
      *    収納情報FETCH
             PERFORM  910-SYUNOU2-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN   NOT = "3"
                 IF    SYU-HKNCOMBINUM  =  WRK-HKNCOMBI(1) OR
                                           WRK-HKNCOMBI(2) OR
                                           WRK-HKNCOMBI(3) OR
                                           WRK-HKNCOMBI(4) OR
                                           WRK-HKNCOMBI(5) OR
                                           WRK-HKNCOMBI(6) OR
                                           WRK-HKNCOMBI(7) OR
                                           WRK-HKNCOMBI(8) OR
                                           WRK-HKNCOMBI(9) OR
                                           WRK-HKNCOMBI(10) 
                   PERFORM  920-HKNCOMBI-READ-SEC
                   IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                       COMB-KOHID(2)    =  WRK-KOHID   OR
                       COMB-KOHID(3)    =  WRK-KOHID   OR
                       COMB-KOHID(4)    =  WRK-KOHID
      *
      *              収納から負担金計算が必要になった場合に取っておく
                     CONTINUE
      *
                   END-IF
                 END-IF
               END-IF
      *
               PERFORM  910-SYUNOU2-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
      *    収納情報CLOSE
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYUNOU2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込（請求金額計算用）
      *****************************************************************
       910-SYUNOU2-READ-SEC  SECTION.
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  SYUNOU-REC
             MOVE    ZERO              TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE    1                 TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU2-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       920-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SYU-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                             SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  HKNCOMBI-REC
                   MOVE    ZERO        TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE              HKNCOMBI-REC
                   MOVE    1           TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費請求情報読み込み
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           INITIALIZE                      RECE02-REC
           MOVE    RECE10-KEY          TO  RECE02-KEY
           MOVE    RECE02-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_kohsky"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO        TO  FLG-RECE02
                   MOVE    MCPDATA-REC TO  RECE02-REC
               ELSE
                   MOVE    1           TO  FLG-RECE02
                   INITIALIZE              RECE02-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-RECE02
               INITIALIZE                  RECE02-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
      *    患者情報SELECT
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
      *    患者情報FETCH
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTINF-REC
             ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE  PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
             INITIALIZE    PTINF-REC
           END-IF
      *
      *    患者情報CLOSE
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
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
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                    SECTION.
      *
           MOVE    "DBOPEN"              TO  MCP-FUNC.
           MOVE    LOW-VALUE             TO  MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
                                         .
      *
           MOVE    "DBSTART"             TO  MCP-FUNC.
           MOVE    LOW-VALUE             TO  MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
                                         .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC               SECTION.
      *
           MOVE    "DBCOMMIT"             TO  MCP-FUNC.
           MOVE    LOW-VALUE              TO  MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                                          .
      *
           MOVE    "DBDISCONNECT"         TO  MCP-FUNC.
           MOVE    LOW-VALUE              TO  MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                                          .
           .
       900-DBCLOSE-EXT.
           EXIT.
