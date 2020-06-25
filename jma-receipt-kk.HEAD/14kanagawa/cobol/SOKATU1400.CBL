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
       PROGRAM-ID.             SOKATU1400.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・総括表（神奈川）用集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/25    山口         ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者     日付	内容
      *  01.00.01    山口    05/11/22    他府県の政令指定都市は市でまとめず
      *                                  に区毎で保険者数を数えるよう修正
      *  01.00.02    山口    05/11/30    返戻対応
      *  01.00.03    内田    07/10/08    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.04    内田    08/04/29    ４月改正に伴う修正
      *                                  SPA-AREA初期化
      *  01.00.05    安藤    08/05/02    新レイアウト対応
      *  01.00.06    内田    08/05/31    帳票追加
      *  01.00.07    内田    08/06/10    国保がない場合でも印刷
      *  01.00.08    吉川    10/08/04    オンライン返戻対応
      *                                  特別療養費国保対応
      *                                  相模原市の代表番号追加
      *  01.00.09    吉川    11/01/25    open-cobol1.0対応
      *  01.00.10    吉川    11/06/23    東北地方太平洋沖地震対応
      *  01.00.11    吉川    12/12/25    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.12    吉川    13/08/06    総括表をもう1枚印刷する
      *                                  プログラムオプション対応
      *  01.00.13    吉川    13/10/10    2枚目の帳票様式変更に伴う修正
      *  01.00.14    吉川    14/10/09    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.15    吉川    14/12/04    返戻時、再印刷画面のタイトル修正
      *  01.00.16    門間    17/10/17    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.17    萩野    18/07/09    記述漏れ修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    総括表一時ファイル  
           SELECT  SKT1400TMP-FILE   ASSIGN    SKT1400TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT1400TMP-KEY
                                   FILE    STATUS  IS  STS-SKT1400.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **=== 災害対応 by Yoshikawa(2011/06/23) start
      **    オンライン請求設定ファイル
      *     SELECT  ONLINE-FILE   ASSIGN FILE-NAME
      *                           ORGANIZATION   IS  LINE SEQUENTIAL
      *                           ACCESS MODE    IS  SEQUENTIAL
      *                           FILE   STATUS  IS  STS-ONLINE.
      **=== 災害対応 by Yoshikawa(2011/06/23) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    総括表一時ファイル  
       FD  SKT1400TMP-FILE.
           COPY    "CPSKT1400TMP.INC".
      *
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(3000).
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **=== 災害対応 by Yoshikawa(2011/06/23) start
      * FD  ONLINE-FILE.
      * 01  ONLINE-REC.
      *     03  ONLINE-ONLINEKBN    PIC X(01).
      *     03  FILLER              PIC X(01).
      **=== 災害対応 by Yoshikawa(2011/06/23) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    総括表一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT1400TP//.
      *
      *    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
      *    エラーファイル 名称領域 
      *---(01.00.03) LINE ADD START ------------------------------------
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *     COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
      *                              BY         //RECEERR//.
           COPY    "CPRECEERR.INC".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *---(01.00.03) LINE ADD END   ------------------------------------
      *     
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    印刷用領域
           COPY    "SKT1400.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SKT1400         PIC X(02).
           03  STS-RECE10          PIC X(02). 
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *=== 災害対応 by Yoshikawa(2011/06/23) start
           03  STS-ONLINE          PIC X(02).
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT1400TMP        PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
           03  FLG-PRINT           PIC 9(01).
      ***** 05/11/30 yamaguchi INS
           03  FLG-PTINF           PIC 9(01).
      *****
      *---(01.00.04) LINE ADD START ------------------------------------
           03  FLG-KOUKI           PIC 9(01).
      *---(01.00.04) LINE ADD END   ------------------------------------
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *---(01.00.03) LINE ADD END   ------------------------------------
           03  WRK-PARA-PAGE       PIC 9(10).
      ***** 05/11/30 yamaguchi INS
           03  WRK-PARA-SYOKBN     PIC X(01).
      *****
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           03  WRK-PARA-TOKUBETU   PIC X(01).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **=== プラグイン対応 by Yoshikawa(2012/12/25) start
      **    ファイル名称用エリア
      * 01  FILE-NAME.
      *     03  FILLER              PIC X(05)  VALUE "/tmp/".
      *     03  FILE-HOSPNUM        PIC 9(02).
      *     03  FILLER              PIC X(10)  VALUE "SOKATU1400".
      *     03  FILLER              PIC X(10)  VALUE "ONLINE.INI".
      **=== プラグイン対応 by Yoshikawa(2012/12/25) end
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD        PIC X(08).
           03  WRK-SEIYMDWH      PIC X(20).
           03  WRK-SRYYMWH       PIC X(16).
           03  WRK-SRYYMD        PIC X(08).
           03  WRK-HKNJANUM.
               05  WRK-HKNJANUM1 PIC 9(02).
               05  WRK-HKNJANUM2 PIC 9(03).
               05  WRK-HKNJANUM3 PIC 9(01).
               05  WRK-HKNJANUM4 PIC 9(02).
      *  
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-Z9-1              PIC Z,ZZ9.
           03  WRK-Z9-2              PIC ZZZ,ZZ9.
           03  WRK-Z9-3              PIC ZZ,ZZZ,ZZZ,ZZ9.
      *
           03  WRK-RECEERR           PIC X(200). 
      *     
           03  WRK-HC01PARA-CNT      PIC  9(06).
           03  SV-HKNJANUM           PIC  X(06).
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      ***** 05/11/30 yamaguchi INS
      *     03  WRK-PATH.
      *         05  FILLER          PIC S9(09)  BINARY.
      *         05  FILLER          PIC S9(09)  BINARY.
      *         05  FILLER          PIC S9(09)  BINARY.
      *****
           03  WRK-PATH            PIC X(50).
      *---(01.00.04) LINE ADD START ------------------------------------
           03  WRK-PATH2               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
      *---(01.00.04) LINE ADD END   ------------------------------------
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
      *    医療機関情報
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *=== プラグイン対応 by Yoshikawa(2012/12/25) start
      *     03  FILE-NAME           PIC X(512).
      *=== プラグイン対応 by Yoshikawa(2012/12/25) end
           03  WRK-ONLINEKBN       PIC X(01).
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE              PIC 9(03).
           03  CNT-RECE10            PIC 9(06).
           03  CNT-UPDFILE           PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03     SUM-NYUIN-KENSU          PIC  9(06).
           03     SUM-NYUIN-TENSU          PIC  9(10).
           03     SUM-GAIRAI-KENSU         PIC  9(06).
           03     SUM-GAIRAI-TENSU         PIC  9(10).
      *---(01.00.05) LINE ADD START ------------------------------------
           03     SUM-KNYUIN-KENSU         PIC  9(06).
           03     SUM-KNYUIN-TENSU         PIC  9(10).
           03     SUM-KGAIRAI-KENSU        PIC  9(06).
           03     SUM-KGAIRAI-TENSU        PIC  9(10).
           03     SUM-GONYUIN-KENSU        PIC  9(06).
           03     SUM-GONYUIN-TENSU        PIC  9(10).
           03     SUM-GOGAIRAI-KENSU       PIC  9(06).
           03     SUM-GOGAIRAI-TENSU       PIC  9(10).
      *     03     SUM-SOU-KENSU            PIC  9(06).
      *     03     SUM-SOU-TENSU            PIC  9(10).
      *---(01.00.05) LINE ADD END   ------------------------------------
           03     SUM-UKETUKE-SU           PIC  9(06).
           03     SUM-HKNJACNT             PIC  9(06).
           03     SUM-SYOKUJI-KENSU        PIC  9(06).
           03     SUM-SYOKUJI-MONEY        PIC  9(10).
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *---(01.00.03) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.04) LINE ADD START ------------------------------------
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *---(01.00.04) LINE ADD END   ------------------------------------
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
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      ***** 05/11/30 yamaguchi INS
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *****
      * 
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
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
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
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
           DISPLAY "*** SOKATU1400aaa START ***"
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
      *---(01.00.07) LINE ADD START ------------------------------------
      *              UNTIL   FLG-END   =   1
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
           PERFORM  300-END-SEC
      *
           STOP     RUN
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
      *---(01.00.04) LINE ADD START ------------------------------------
           INITIALIZE                   SPA-AREA
      *---(01.00.04) LINE ADD END   ------------------------------------
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
           INITIALIZE                   RECEERR
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
      *
           ACCEPT     SYS-TIME          FROM    TIME     
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
      ***** 05/11/30 yamaguchi DEL
      *                                         WRK-PARA-JOBID
      *                                         WRK-PARA-SHELLID
      *                                         WRK-PARA-HOSPID
      ***** 05/11/30 yamaguchi INS
      *---(01.00.03) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-SYOKBN
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
                                               WRK-PARA-TOKUBETU
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *---(01.00.03) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.03) LINE ADD END   ------------------------------------
           END-UNSTRING
      *****
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU1400"     TO  JOB-PGID
           MOVE    "総括表" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
           MOVE    "SKT1400"             TO  SKT1400TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID     TO  SKT1400TP-TERMID
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SKT1400TP-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID     TO  RECEUPD-TERMID
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID     TO  RECEERR-TERMID
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *=== ver4.8対応 by Yoshikawa(2014/10/09) start
           MOVE  SKT1400TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT1400TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *=== ver4.8対応 by Yoshikawa(2014/10/09) end
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                                 TO  SYS-1001-STYUKYMD(1:6)
      *                                     SYS-1001-EDYUKYMD(1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
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
           INITIALIZE                  SYS-1002-REC
           MOVE     "1002"             TO  SYS-1002-KANRICD
           MOVE     "*"                TO  SYS-1002-KBNCD
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM(1:6)
      *                                 TO  SYS-1002-STYUKYMD(1:6)
      *                                     SYS-1002-EDYUKYMD(1:6)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **=== 災害対応 by Yoshikawa(2011/06/23) start
      *     PERFORM  110-INI-FILE-SEC
      **=== 災害対応 by Yoshikawa(2011/06/23) end
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *
           OPEN     OUTPUT  UPD-FILE
      *
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
      ****** 05/11/30 yamaguchi INS
      *     EVALUATE    WRK-PARA-SYOKBN 
      *         WHEN    "2"
      **---(01.00.03) LINE ADD START ------------------------------------
      **             MOVE    PATH-TBL-SEIKYU-KEY37    TO  WRK-PATH
      *             MOVE    "key37"                  TO  WRK-PATH
      **---(01.00.03) LINE ADD END   ------------------------------------
      **---(01.00.04) LINE ADD START ------------------------------------
      *             MOVE    "key65"                  TO  WRK-PATH2
      **---(01.00.04) LINE ADD END   ------------------------------------
      *         WHEN    "1"
      **---(01.00.03) LINE ADD START ------------------------------------
      **             MOVE    PATH-TBL-SEIKYU-KEY38    TO  WRK-PATH
      *             MOVE    "key38"                  TO  WRK-PATH
      **---(01.00.03) LINE ADD END   ------------------------------------
      **---(01.00.04) LINE ADD START ------------------------------------
      *             MOVE    "key66"                  TO  WRK-PATH2
      **---(01.00.04) LINE ADD END   ------------------------------------
      *     END-EVALUATE
      ******
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
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *  
           PERFORM  900-SEIKYU-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
      **=== 災害対応 by Yoshikawa(2011/06/23) start
      ******************************************************************
      **    オンライン請求設定INIファイル読み込み処理  
      ******************************************************************
      * 110-INI-FILE-SEC                SECTION.
      **
      **    オンライン請求設定取得
      **    ファイルディレクトリ位置指定
      **=== プラグイン対応 by Yoshikawa(2012/12/25) start
      **     INITIALIZE           ORCSMKPASSAREA
      **     MOVE  "data/ONLINE.INI"    TO  MKPASS-IN-01
      **     CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
      **     MOVE  MKPASS-OT-01  TO  FILE-NAME
      *     MOVE  SPA-HOSPNUM   TO  FILE-HOSPNUM
      **=== プラグイン対応 by Yoshikawa(2012/12/25) end
      **
      *     DISPLAY "ファイル名="  FILE-NAME
      **
      *     INITIALIZE   ONLINE-REC
      **
      *     OPEN  INPUT  ONLINE-FILE
      **
      *     READ ONLINE-FILE
      *       AT END
      *         INITIALIZE        ONLINE-REC
      *     END-READ
      **
      *     CLOSE ONLINE-FILE
      **
      *     DISPLAY "オンライン請求設定取得=" ONLINE-ONLINEKBN
      *     IF    ONLINE-ONLINEKBN  =  "0" OR "1"
      *       MOVE   ONLINE-ONLINEKBN  TO  WRK-ONLINEKBN
      *     ELSE
      *       MOVE   "0"               TO  WRK-ONLINEKBN
      *     END-IF
      **
      *     DISPLAY "オンライン請求設定 =" WRK-ONLINEKBN
      **
      *     .
      * 110-INI-FILE-EXT.
      *     EXIT.
      **
      **=== 災害対応 by Yoshikawa(2011/06/23) end
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU1400"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    オンライン請求設定
           MOVE    "ONLINE"                  TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           IF  WRK-OPTION(1)(1:1)  =  "0" OR "1"
             MOVE   WRK-OPTION(1)(1:1)       TO  WRK-ONLINEKBN
           ELSE
             MOVE   "0"                      TO  WRK-ONLINEKBN
           END-IF
           DISPLAY "オンライン請求設定="  WRK-ONLINEKBN
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
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *****************************************************************
      *    主    処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           OPEN     OUTPUT  SKT1400TMP-FILE
           CLOSE            SKT1400TMP-FILE
           OPEN     I-O     SKT1400TMP-FILE
      *
           INITIALIZE             SUM-AREA
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *     MOVE     ZERO    TO        FLG-END
      *---(01.00.07) LINE ADD END   ------------------------------------
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
      *---(01.00.04) LINE ADD START ------------------------------------
           PERFORM  900-SEIKYU-CLOSE-SEC
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           IF  WRK-PARA-TOKUBETU  =  "1"
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
             MOVE     ZERO    TO        FLG-END
             MOVE     1       TO        FLG-KOUKI
             MOVE    "039"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           ELSE
             MOVE     1       TO        FLG-KOUKI
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
             MOVE     ZERO    TO        FLG-END
             MOVE    "040"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
      *=== 記述漏れ修正 by hagino(2018/07/09) start
             PERFORM  950-SEIKYU-CLOSE-SEC
      *=== 記述漏れ修正 by hagino(2018/07/09) end
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           END-IF
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *---(01.00.04) LINE ADD END   ------------------------------------
      *
           IF       FLG-UPD   =       1
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           CLOSE   UPD-FILE
           CLOSE   SKT1400TMP-FILE
           .
      *
       200-MAIN-EXT.
           EXIT.
      *
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
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU1400 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU1400 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------

           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理                        
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC               SECTION.
      *
           MOVE     ZERO    TO      FLG-UPD
      *
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT1400TMP-REC
           INITIALIZE       SKT1400TMP-HKNJANUM
           INITIALIZE       WRK-HKNJANUM
      *
      *---(01.00.05) LINE ADD START ------------------------------------
           IF  FLG-KOUKI   NOT  =  ZERO
              MOVE  RECE10-HKNJANUM(3:6)   TO  WRK-HKNJANUM(1:6)
           ELSE
              IF  RECE10-HKNJANUM(7:2)  =  SPACE
                 MOVE  RECE10-HKNJANUM(1:6)  TO  WRK-HKNJANUM(1:6)
              ELSE
                 MOVE  RECE10-HKNJANUM(3:6)  TO  WRK-HKNJANUM(1:6)
              END-IF
           END-IF
      *---(01.00.05) LINE ADD END   ------------------------------------
      *
      *区の場合代表番号セット
      *---(01.00.05) LINE ADD START ------------------------------------
           IF  FLG-KOUKI   NOT  =  ZERO
              PERFORM  200101-HKNJANUM-SET-SEC
           ELSE
              PERFORM  2002-HKNJANUM-SET-SEC
           END-IF
      *---(01.00.05) LINE ADD END   ------------------------------------
           MOVE  WRK-HKNJANUM    TO  SKT1400TMP-HKNJANUM
      *
           PERFORM  900-TMP-READ-SEC
           PERFORM  220-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき                
           IF     FLG-UPD        =       1
                  PERFORM   240-UPD-FILE-HENSYU-SEC
      *
      ***** 05/11/30 yamaguchi DEL
      *            MOVE      PATH-TBL-SEIKYU-KEY4     TO    MCP-PATH
      *****
      *---(01.00.04) LINE ADD START ------------------------------------
      *            PERFORM   900-SEIKYU-READ-SEC
               IF  FLG-KOUKI  =  ZERO
                  PERFORM   900-SEIKYU-READ-SEC
               ELSE
                  PERFORM   950-SEIKYU-READ-SEC
               END-IF
      *---(01.00.04) LINE ADD END   ------------------------------------
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
      *****************************************************************
      *    代表番号セット
      *****************************************************************
       2002-HKNJANUM-SET-SEC                SECTION.
      *
      ***** 05/11/22 yamaguchi DEL
      **    札幌市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "014"
      *       MOVE  "014001"  TO  WRK-HKNJANUM
      *     END-IF
      **    仙台市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "044"
      *       MOVE  "044008"  TO  WRK-HKNJANUM
      *     END-IF
      **    千葉市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "124"
      *       MOVE  "124008"  TO  WRK-HKNJANUM
      *     END-IF
      *    横浜市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "144"
             MOVE  "144006"  TO  WRK-HKNJANUM
           END-IF
      *    川崎市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "145"
             MOVE  "145003"  TO  WRK-HKNJANUM
           END-IF
      *=== 相模原市の代表番号追加 by Yoshikawa(2010/08/04) start
      *    相模原市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "146"
             MOVE  "146001"  TO  WRK-HKNJANUM
           END-IF
      *=== 相模原市の代表番号追加 by Yoshikawa(2010/08/04) end
      **    名古屋市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "234"
      *       MOVE  "234005"  TO  WRK-HKNJANUM
      *     END-IF
      **    京都市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "264"
      *       MOVE  "264002"  TO  WRK-HKNJANUM
      *     END-IF
      **    神戸市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "284"
      *       MOVE  "284000"  TO  WRK-HKNJANUM
      *     END-IF
      **    広島市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "344"
      *       MOVE  "344002"  TO  WRK-HKNJANUM
      *     END-IF
      **    北九州市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "404"
      *       MOVE  "404004"  TO  WRK-HKNJANUM
      *     END-IF
      **    福岡市の場合、代表番号セット
      *     IF  WRK-HKNJANUM (1:3) = "405"
      *       MOVE  "405001"  TO  WRK-HKNJANUM
      *     END-IF
      *****
           .
       2002-HKNJANUM-SET-EXT.
           EXIT.
      *
      *---(01.00.05) LINE ADD START ------------------------------------
      *****************************************************************
      *    広域連合・代表番号セット
      *****************************************************************
       200101-HKNJANUM-SET-SEC                SECTION.
      *
           EVALUATE  WRK-HKNJANUM(1:2)
      *      北海道
             WHEN  "01"
                 MOVE  "010004"  TO  WRK-HKNJANUM
      *      青森県
             WHEN  "02"
                 MOVE  "020003"  TO  WRK-HKNJANUM
      *      岩手県
             WHEN  "03"
                 MOVE  "030002"  TO  WRK-HKNJANUM
      *      宮城県
             WHEN  "04"
                 MOVE  "040001"  TO  WRK-HKNJANUM
      *      秋田県
             WHEN  "05"
                 MOVE  "050000"  TO  WRK-HKNJANUM
      *      山形県
             WHEN  "06"
                 MOVE  "060009"  TO  WRK-HKNJANUM
      *      福島県
             WHEN  "07"
                 MOVE  "070008"  TO  WRK-HKNJANUM
      *      茨城県
             WHEN  "08"
                 MOVE  "080007"  TO  WRK-HKNJANUM
      *      栃木県
             WHEN  "09"
                 MOVE  "090006"  TO  WRK-HKNJANUM
      *      群馬県
             WHEN  "10"
                 MOVE  "100003"  TO  WRK-HKNJANUM
      *      埼玉県
             WHEN  "11"
                 MOVE  "110002"  TO  WRK-HKNJANUM
      *      千葉県
             WHEN  "12"
                 MOVE  "120001"  TO  WRK-HKNJANUM
      *      東京都
             WHEN  "13"
                 MOVE  "130000"  TO  WRK-HKNJANUM
      *      神奈川県
             WHEN  "14"
                 MOVE  "140009"  TO  WRK-HKNJANUM
      *      新潟県
             WHEN  "15"
                 MOVE  "150008"  TO  WRK-HKNJANUM
      *      富山県
             WHEN  "16"
                 MOVE  "160007"  TO  WRK-HKNJANUM
      *      石川県
             WHEN  "17"
                 MOVE  "170006"  TO  WRK-HKNJANUM
      *      福井県
             WHEN  "18"
                 MOVE  "180005"  TO  WRK-HKNJANUM
      *      山梨県
             WHEN  "19"
                 MOVE  "190004"  TO  WRK-HKNJANUM
      *      長野県
             WHEN  "20"
                 MOVE  "200001"  TO  WRK-HKNJANUM
      *      岐阜県
             WHEN  "21"
                 MOVE  "210000"  TO  WRK-HKNJANUM
      *      静岡県
             WHEN  "22"
                 MOVE  "220009"  TO  WRK-HKNJANUM
      *      愛知県
             WHEN  "23"
                 MOVE  "230008"  TO  WRK-HKNJANUM
      *      三重県
             WHEN  "24"
                 MOVE  "240007"  TO  WRK-HKNJANUM
      *      滋賀県
             WHEN  "25"
                 MOVE  "250006"  TO  WRK-HKNJANUM
      *      京都府
             WHEN  "26"
                 MOVE  "260005"  TO  WRK-HKNJANUM
      *      大阪府
             WHEN  "27"
                 MOVE  "270004"  TO  WRK-HKNJANUM
      *      兵庫県
             WHEN  "28"
                 MOVE  "280003"  TO  WRK-HKNJANUM
      *      奈良県
             WHEN  "29"
                 MOVE  "290002"  TO  WRK-HKNJANUM
      *      和歌山県
             WHEN  "30"
                 MOVE  "300009"  TO  WRK-HKNJANUM
      *      鳥取県
             WHEN  "31"
                 MOVE  "310008"  TO  WRK-HKNJANUM
      *      島根県
             WHEN  "32"
                 MOVE  "320007"  TO  WRK-HKNJANUM
      *      岡山県
             WHEN  "33"
                 MOVE  "330006"  TO  WRK-HKNJANUM
      *      広島県
             WHEN  "34"
                 MOVE  "340005"  TO  WRK-HKNJANUM
      *      山口県
             WHEN  "35"
                 MOVE  "350004"  TO  WRK-HKNJANUM
      *      徳島県
             WHEN  "36"
                 MOVE  "360003"  TO  WRK-HKNJANUM
      *      香川県
             WHEN  "37"
                 MOVE  "370002"  TO  WRK-HKNJANUM
      *      愛媛県
             WHEN  "38"
                 MOVE  "380001"  TO  WRK-HKNJANUM
      *      高知県
             WHEN  "39"
                 MOVE  "390000"  TO  WRK-HKNJANUM
      *      福岡県
             WHEN  "40"
                 MOVE  "400007"  TO  WRK-HKNJANUM
      *      佐賀県
             WHEN  "41"
                 MOVE  "410006"  TO  WRK-HKNJANUM
      *      長崎県
             WHEN  "42"
                 MOVE  "420005"  TO  WRK-HKNJANUM
      *      熊本県
             WHEN  "43"
                 MOVE  "430004"  TO  WRK-HKNJANUM
      *      大分県
             WHEN  "44"
                 MOVE  "440003"  TO  WRK-HKNJANUM
      *      宮崎県
             WHEN  "45"
                 MOVE  "450002"  TO  WRK-HKNJANUM
      *      鹿児島県
             WHEN  "46"
                 MOVE  "460001"  TO  WRK-HKNJANUM
      *      沖縄県
             WHEN  "47"
                 MOVE  "470000"  TO  WRK-HKNJANUM
           END-EVALUATE
      *
           .
       200101-HKNJANUM-SET-EXT.
           EXIT.
      *
      *---(01.00.05) LINE ADD END   ------------------------------------
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       220-HKN-SYUKEI-SEC               SECTION.
      *
           IF   RECE10-NYUGAIKBN   =   "1"
      *---(01.00.05) LINE ADD START ------------------------------------
              IF  FLG-KOUKI   NOT  =  ZERO
                 ADD      1           TO    SKT1400TMP-KN-KENSU
                 COMPUTE  SKT1400TMP-KN-TENSU       =
                          SKT1400TMP-KN-TENSU       +
                          RECE10-TOTALTEN(1)
                IF    RECE10-SHOKUJINISSU(1)     >    0
                   ADD      1            TO   SKT1400TMP-S-KENSU
                END-IF
                ADD    RECE10-SHOKUJIRYOYOHI(1) TO
                                              SKT1400TMP-S-MONEY
              ELSE
      *---(01.00.05) LINE ADD END   ------------------------------------
                 ADD      1           TO    SKT1400TMP-N-KENSU
                 COMPUTE  SKT1400TMP-N-TENSU       =
                          SKT1400TMP-N-TENSU       +
                          RECE10-TOTALTEN(1)
      ***** 05/11/30 yamaguchi DEL
      *         IF    RECE10-SHOKUJINISSU(1)     >    1
      *             ADD      1            TO   SKT1400TMP-S-KENSU
      *          END-IF
      ***** 05/11/30 yamaguchi INS
                IF    RECE10-SHOKUJINISSU(1)     >    0
                   ADD      1            TO   SKT1400TMP-S-KENSU
                END-IF
      *****
                ADD    RECE10-SHOKUJIRYOYOHI(1) TO
                                              SKT1400TMP-S-MONEY
              END-IF
      *---(01.00.05) LINE ADD START ------------------------------------
           ELSE
              IF  FLG-KOUKI   NOT  =  ZERO
                 COMPUTE  SKT1400TMP-KG-TENSU       =
                          SKT1400TMP-KG-TENSU       +
                          RECE10-TOTALTEN(1)     
                 ADD      1           TO    SKT1400TMP-KG-KENSU
              ELSE
      *---(01.00.05) LINE ADD END   ------------------------------------
                 COMPUTE  SKT1400TMP-G-TENSU       =
                          SKT1400TMP-G-TENSU       +
                          RECE10-TOTALTEN(1)     
                 ADD      1           TO    SKT1400TMP-G-KENSU
              END-IF
      *---(01.00.05) LINE ADD START ------------------------------------
           END-IF
      *---(01.00.05) LINE ADD END   ------------------------------------
      *
           IF      FLG-SKT1400TMP     =          1
                WRITE    SKT1400TMP-REC
           ELSE
                REWRITE  SKT1400TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD 
           .
      *
       220-HKN-SYUKEI-EXT.
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
           .
      *
       240-UPD-FILE-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票ファイル出力処理                    
      *****************************************************************
       2002-PRINT-SEC                          SECTION.
      *
      *    一時ファイルＳＴＡＲＴ処理
           PERFORM    310-TMP-START-SEC
           IF     FLG-PRINT  =    1
                  GO     TO   2002-PRINT-EXT
           END-IF
      *
      *    一時ファイル累計処理
           PERFORM    320-TMP-RUIKEI-SEC
                      UNTIL    FLG-PRINT   =    1
      *
      *    帳票編集処理
           PERFORM    330-BODY-HEN-SEC
      *
      *=== 2枚目の帳票様式変更 by Yoshikawa(2013/10/10) start
      **---(01.00.06) LINE ADD START ------------------------------------
      **    追加帳票（受領書）印刷処理
      *     PERFORM    341-PRINT-OUT-SEC
      **---(01.00.06) LINE ADD END   ------------------------------------
      **    帳票印刷処理
      **=== 総括表をもう1枚印刷する by Yoshikawa(2013/08/06) start
      **     PERFORM    340-PRINT-OUT-SEC
      *     PERFORM  2  TIMES
      *         PERFORM    340-PRINT-OUT-SEC
      *     END-PERFORM
      **=== 総括表をもう1枚印刷する by Yoshikawa(2013/08/06) end
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  3
      *      帳票印刷処理
             PERFORM 340-PRINT-OUT-SEC
           END-PERFORM
      *=== 2枚目の帳票様式変更 by Yoshikawa(2013/10/10) end
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイルＳＴＡＲＴ処理
      *****************************************************************
       310-TMP-START-SEC             SECTION.
      *
           INITIALIZE   SKT1400TMP-KEY
           START    SKT1400TMP-FILE
                    KEY     IS    >     SKT1400TMP-KEY
                    INVALID       KEY
                            MOVE  1     TO    FLG-PRINT
           END-START
      *
           MOVE   SPACE     TO     SV-HKNJANUM
           INITIALIZE              SUM-AREA
           .
       310-TMP-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    累計処理
      *****************************************************************
       320-TMP-RUIKEI-SEC               SECTION.
      *
           READ     SKT1400TMP-FILE   NEXT
                    AT      END         
                            MOVE      1     TO      FLG-PRINT
                            GO        TO    320-TMP-RUIKEI-EXT
           END-READ
      *
           IF       SV-HKNJANUM    =    SPACE
                    MOVE  SKT1400TMP-HKNJANUM    TO  SV-HKNJANUM
                    MOVE  1     TO   SUM-HKNJACNT
           END-IF
      *
           DISPLAY "****"
           DISPLAY "SV-HKNJANUM****"SV-HKNJANUM
           DISPLAY "SK-HKNJANUM****"SKT1400TMP-HKNJANUM
           DISPLAY " "
           IF       SV-HKNJANUM    NOT   =   SKT1400TMP-HKNJANUM
                    MOVE  SKT1400TMP-HKNJANUM    TO  SV-HKNJANUM
                    ADD   1     TO   SUM-HKNJACNT
           END-IF
      *
           ADD   SKT1400TMP-N-KENSU     TO  SUM-NYUIN-KENSU
           ADD   SKT1400TMP-G-KENSU     TO  SUM-GAIRAI-KENSU
           ADD   SKT1400TMP-N-TENSU     TO  SUM-NYUIN-TENSU
           ADD   SKT1400TMP-G-TENSU     TO  SUM-GAIRAI-TENSU
      *---(01.00.05) LINE ADD START ------------------------------------
           ADD   SKT1400TMP-KN-KENSU    TO  SUM-KNYUIN-KENSU
           ADD   SKT1400TMP-KG-KENSU    TO  SUM-KGAIRAI-KENSU
           ADD   SKT1400TMP-KN-TENSU    TO  SUM-KNYUIN-TENSU
           ADD   SKT1400TMP-KG-TENSU    TO  SUM-KGAIRAI-TENSU
      *---(01.00.05) LINE ADD END   ------------------------------------
           ADD   SKT1400TMP-S-KENSU     TO  SUM-SYOKUJI-KENSU
           ADD   SKT1400TMP-S-MONEY     TO  SUM-SYOKUJI-MONEY

      *
           .
       320-TMP-RUIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       330-BODY-HEN-SEC                 SECTION.
      *
           INITIALIZE                   SKT1400
      *---(01.00.06) LINE ADD START ------------------------------------
      *     ADD  1                       TO     CNT-PAGE
      *=== 総括表をもう1枚印刷する by Yoshikawa(2013/08/06) start
      *     ADD  2                       TO     CNT-PAGE
           ADD  3                       TO     CNT-PAGE
      *=== 総括表をもう1枚印刷する by Yoshikawa(2013/08/06) end
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *   総括表発行日
           MOVE     WRK-SRYYMWH         TO     SKT1400-SEIYM
      *
      *   医療機関名,コード
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE     SYS-1001-HOSPNAME   TO     SKT1400-HOSPNAME
           MOVE     WRK-HOSPNAME        TO     SKT1400-HOSPNAME
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) start
           INSPECT SKT1400-HOSPNAME     REPLACING ALL "  " BY "　"
      *=== 医療機関情報編集対応 by Yoshikawa(2012/12/25) end
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE     SYS-1001-HOSPCDN    TO     SKT1400-HOSPCD
      *   保険者数
           MOVE     SUM-HKNJACNT        TO     WRK-Z9-1
           MOVE     WRK-Z9-1            TO     SKT1400-HKNJASU
      *---(01.00.05) LINE ADD START ------------------------------------
      *   入院件数
           MOVE     SUM-NYUIN-KENSU     TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(1)
      *   入院点数 
           MOVE     SUM-NYUIN-TENSU     TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(1)
      *   外来件数
           MOVE     SUM-GAIRAI-KENSU    TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(2)
      *   外来点数
           MOVE     SUM-GAIRAI-TENSU    TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(2)
      *   後期高齢入院件数
           MOVE     SUM-KNYUIN-KENSU    TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(3)
      *   後期高齢入院点数 
           MOVE     SUM-KNYUIN-TENSU    TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(3)
      *   後期高齢外来件数
           MOVE     SUM-KGAIRAI-KENSU   TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(4)
      *   後期高齢外来点数
           MOVE     SUM-KGAIRAI-TENSU   TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(4)
      *   入院件数合計
           COMPUTE SUM-GONYUIN-KENSU    =      SUM-NYUIN-KENSU
                                        +      SUM-KNYUIN-KENSU
           MOVE     SUM-GONYUIN-KENSU   TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(5)
      *   入院点数合計
           COMPUTE SUM-GONYUIN-TENSU    =      SUM-NYUIN-TENSU
                                        +      SUM-KNYUIN-TENSU
           MOVE     SUM-GONYUIN-TENSU   TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(5)
      *   外来件数合計
           COMPUTE SUM-GOGAIRAI-KENSU   =      SUM-GAIRAI-KENSU
                                        +      SUM-KGAIRAI-KENSU
           MOVE     SUM-GOGAIRAI-KENSU  TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(6)
      *   外来点数合計
           COMPUTE SUM-GOGAIRAI-TENSU   =      SUM-GAIRAI-TENSU
                                        +      SUM-KGAIRAI-TENSU
           MOVE     SUM-GOGAIRAI-TENSU  TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(6)
      *   食事件数
           MOVE     SUM-SYOKUJI-KENSU   TO     WRK-Z9-2
           MOVE     WRK-Z9-2            TO     SKT1400-KENSU(7)
      *   食事療養費
           MOVE     SUM-SYOKUJI-MONEY   TO     WRK-Z9-3
           MOVE     WRK-Z9-3            TO     SKT1400-TENSU(7)
      *
      *     COMPUTE SUM-SOU-KENSU        =      SUM-NYUIN-KENSU
      *                                  +      SUM-GAIRAI-KENSU
      *     COMPUTE SUM-SOU-TENSU        =      SUM-NYUIN-TENSU
      *                                  +      SUM-GAIRAI-TENSU
      **   総件数・総点数
      *     MOVE     SUM-SOU-KENSU       TO     WRK-Z9-2
      *     MOVE     WRK-Z9-2            TO     SKT1400-KENSU-KEI
      *     MOVE     SUM-SOU-TENSU       TO     WRK-Z9-3
      *     MOVE     WRK-Z9-3            TO     SKT1400-TENSU-KEI
      **   入院件数
      *     MOVE     SUM-KNYUIN-KENSU     TO     WRK-Z9-2
      *     MOVE     WRK-Z9-2            TO     SKT1400-N-KENSU
      **   入院点数 
      *     MOVE     SUM-KNYUIN-TENSU     TO     WRK-Z9-3
      *     MOVE     WRK-Z9-3            TO     SKT1400-N-TENSU
      **   外来件数
      *     MOVE     SUM-GAIRAI-KENSU    TO     WRK-Z9-2
      *     MOVE     WRK-Z9-2            TO     SKT1400-G-KENSU
      **   外来点数
      *     MOVE     SUM-GAIRAI-TENSU    TO     WRK-Z9-3
      *     MOVE     WRK-Z9-3            TO     SKT1400-G-TENSU
      **   食事件数
      *     MOVE     SUM-SYOKUJI-KENSU   TO     WRK-Z9-2
      *     MOVE     WRK-Z9-2            TO     SKT1400-S-KENSU
      **   食事療養費
      *     MOVE     SUM-SYOKUJI-MONEY   TO     WRK-Z9-3
      *     MOVE     WRK-Z9-3            TO     SKT1400-S-MONEY
      **
      *     COMPUTE SUM-SOU-KENSU        =      SUM-NYUIN-KENSU
      *                                  +      SUM-GAIRAI-KENSU
      *     COMPUTE SUM-SOU-TENSU        =      SUM-NYUIN-TENSU
      *                                  +      SUM-GAIRAI-TENSU
      **   総件数・総点数
      *     MOVE     SUM-SOU-KENSU       TO     WRK-Z9-2
      *     MOVE     WRK-Z9-2            TO     SKT1400-KENSU-KEI
      *     MOVE     SUM-SOU-TENSU       TO     WRK-Z9-3
      *     MOVE     WRK-Z9-3            TO     SKT1400-TENSU-KEI
      *---(01.00.05) LINE ADD END   ------------------------------------
      *   処方箋受付回数
           MOVE     SPACE               TO     SKT1400-UKETUKE-SU
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"         TO     SKT1400-TOKUBETU
           END-IF
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *
           .
       330-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC                   SECTION.
      * 
      *      GO  TO  340-PRINT-OUT-EXT

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
      *=== 2枚目の帳票様式変更 by Yoshikawa(2013/10/10) start
      *     MOVE   "SKT1400.red"        TO  SPRT-PRTID
           EVALUATE  IDX1
             WHEN  1
      *        受領書
               MOVE    "SKT1401.red"   TO  SPRT-PRTID
             WHEN  2
      *        総括表2枚目
               MOVE    "SKT1402.red"   TO  SPRT-PRTID
             WHEN  3
      *        総括表3枚目
               MOVE    "SKT1400.red"   TO  SPRT-PRTID
           END-EVALUATE
      *=== 2枚目の帳票様式変更 by Yoshikawa(2013/10/10) end
      *=== 返戻時、再印刷画面のタイトル修正 by Yoshikawa(2014/12/04) start
      *     MOVE    "国保総括表"        TO  SPRT-TITLE
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
      *=== 返戻時、再印刷画面のタイトル修正 by Yoshikawa(2014/12/04) end
           MOVE    SKT1400             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *=== SITEKBN変更 by Monma(2017/10/17) start
           MOVE    "2"                 TO  SPRT-SITEKBN
      *=== SITEKBN変更 by Monma(2017/10/17) end
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.03) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      SPRT-RETURN         =   ZERO
                   CONTINUE
           ELSE
            DISPLAY "error"


                   MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *---(01.00.06) LINE ADD START ------------------------------------
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       341-PRINT-OUT-SEC                   SECTION.
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
           MOVE   "SKT1401.red"        TO  SPRT-PRTID
      *=== 返戻時、再印刷画面のタイトル修正 by Yoshikawa(2014/12/04) start
      *     MOVE    "国保総括表"        TO  SPRT-TITLE
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
      *=== 返戻時、再印刷画面のタイトル修正 by Yoshikawa(2014/12/04) end
           MOVE    SKT1400             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *=== SITEKBN変更 by Monma(2017/10/17) start
           MOVE    "2"                 TO  SPRT-SITEKBN
      *=== SITEKBN変更 by Monma(2017/10/17) end
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
                   CONTINUE
           ELSE
            DISPLAY "error"


                   MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       341-PRINT-OUT-EXT.
           EXIT.
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------

           END-IF
      *
           CLOSE   RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *                  MOVE     PATH-TBL-SEIKYU-KEY
      *                                  TO     MCP-PATH
      *                  CALL     "MCPSUB"      USING
      *                                         MCPAREA
      *                                        MCPDATA-REC
                       MOVE    "tbl_seikyu"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"     USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
                        IF       MCP-RC =      ZERO
                                 PERFORM       900-UPD-FILE-READ-SEC
                        ELSE
                                 DISPLAY "UPD ERR KEY="  RECE10-KEY 
                                 MOVE    
                                "*** SOKATU1400 SEIKYU UPDATE ERROR ***"
                                           TO  WRK-RECEERR
                                 MOVE    RECE10-KEY     TO
                                         WRK-RECEERR(33:)
                                 PERFORM 500-ERR-HENSYU-SEC
                                 MOVE    1      TO      FLG-UPDFILE
                        END-IF
      *
              ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                       MOVE    "*** SOKATU1400 SEIKYU INVALID ***"
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
       3001-SEIKYU-UPDATE-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE   SYS-1001-HOSPID       TO    RECE10-HOSPID.
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
           IF  WRK-PARA-TOKUBETU  =  "2"
      *=== open-cobol1.0対応 by Yoshikawa(2011/01/25) start
      *       MOVE  "000000"            TO  RECE10-SRYYM
      *       MOVE  "999999"            TO  RECE10-XXSRYYM
             MOVE  000000              TO  RECE10-SRYYM
             MOVE  999999              TO  RECE10-XXSRYYM
      *=== open-cobol1.0対応 by Yoshikawa(2011/01/25) end
           END-IF
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *
           MOVE   RECE10-REC                   TO    MCPDATA-REC.
           MOVE   "DBSELECT"                   TO    MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      ***** 05/11/30 yamaguchi DEL
      **     MOVE   PATH-TBL-SEIKYU-KEY4         TO    MCP-PATH.
      ***** 05/11/30 yamaguchi INS
      *     MOVE   WRK-PATH              TO    MCP-PATH.
      *****
      *     CALL   "MCPSUB"                    USING
      *                                        MCPAREA
      *                                        MCPDATA-REC.
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           IF     MCP-RC        =       ZERO
      ***** 05/11/30 yamaguchi DEL
      *            MOVE     PATH-TBL-SEIKYU-KEY4     TO    MCP-PATH
      *****
                  PERFORM  900-SEIKYU-READ-SEC
           ELSE
                  MOVE    1    TO      FLG-END
           END-IF
           .
      *
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    総括表一時ファイルＲＥＡＤ処理
      *****************************************************************
       900-TMP-READ-SEC                 SECTION.
      *
           MOVE   ZERO        TO        FLG-SKT1400TMP
           READ   SKT1400TMP-FILE
                  INVALID     KEY     
                              MOVE    1     TO      FLG-SKT1400TMP
           END-READ
           .
      *
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      ***** 05/11/29 yamaguchi INS
      *     MOVE   WRK-PATH      TO      MCP-PATH
      *****
      *     MOVE   "DBFETCH"     TO      MCP-FUNC
      *     CALL   "MCPSUB"              USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      ***** 05/11/30 yamaguchi DEL
      *     IF       MCP-RC  =     ZERO
      *              MOVE    MCPDATA-REC   TO      RECE10-REC
      *      DISPLAY "hknjanum" RECE10-PTNUM
      *              ADD     1             TO      CNT-RECE10   
      *     ELSE
      *              MOVE    1             TO      FLG-END
      *     END-IF
      ***** 05/11/30 yamaguchi INS
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *=== オンライン返戻対応 by Yoshikawa(2010/08/04) start
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *=== オンライン返戻対応 by Yoshikawa(2010/08/04) end
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *        オンライン請求の場合、保険者未特定以外は読み飛ばす
               IF  WRK-ONLINEKBN  =  "1"
                 IF   RECE10-HKNJANUM  =  ZERO
                   CONTINUE
                 ELSE
                   GO  TO  900-SEIKYU-READ-SEC
                 END-IF
               END-IF
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *         
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *****
      *
           .
      *
       900-SEIKYU-READ-EXT.
           EXIT.
      *---(01.00.04) LINE ADD START ------------------------------------
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
      *=== オンライン返戻対応 by Yoshikawa(2010/08/04) start
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF
      *=== オンライン返戻対応 by Yoshikawa(2010/08/04) end
      *
      *=== 災害対応 by Yoshikawa(2011/06/23) start
      *        オンライン請求の場合、保険者未特定以外は読み飛ばす
               IF  WRK-ONLINEKBN  =  "1"
                 IF   RECE10-HKNJANUM  =  ZERO
                   CONTINUE
                 ELSE
                   GO  TO  950-SEIKYU-READ-SEC
                 END-IF
               END-IF
      *=== 災害対応 by Yoshikawa(2011/06/23) end
      *
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  950-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
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
      *---(01.00.04) LINE ADD END   ------------------------------------
      *****************************************************************
      *    患者マスタ読込  05/11/30 yamaguchi INS
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       900-SEIKYU-INV-SEC             SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SEIKYU-KEY        TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SEIKYU-KEY        TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SEIKYU-KEY        TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
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
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
      * 900-HKNJAINF-INV-SEC         SECTION.
      *
      *     MOVE    "DBSELECT"                 TO  MCP-FUNC
      *     MOVE    PATH-TBL-HKNJAINF-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
      *     IF      MCP-RC              =   ZERO
      *         MOVE    "DBFETCH"                  TO  MCP-FUNC
      *         MOVE    PATH-TBL-HKNJAINF-KEY      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
      *         IF      MCP-RC              =   ZERO
      *             MOVE    ZERO                TO  FLG-HKNJAINF
      *             MOVE    MCPDATA-REC         TO  HKNJAINF-REC
      *         ELSE
      *             MOVE    1                   TO  FLG-HKNJAINF
      *         END-IF
      *     ELSE
      *        MOVE    1                   TO  FLG-HKNJAINF
      *     END-IF
      *
      *     MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
      *     MOVE    PATH-TBL-HKNJAINF-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
      *
      *     .
      * 900-HKNJAINF-INV-EXT.
      *     EXIT.
      *---(01.00.03) LINE ADD END   ------------------------------------
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-STYUKYMD(1:6)
      *     MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
      *     MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-EDYUKYMD(1:6)
      *     MOVE    "01"                    TO  SYS-1900-EDYUKYMD(7:2)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-EDYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1901-EDYUKYMD(7:2)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
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
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) start
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
      *     MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
      *=== 医療機関コード変更時作成対応 by Monma(2017/10/17) end
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
      *---(01.00.03) LINE ADD END   ------------------------------------
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) start
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
      *=== プログラムオプション対応 by Yoshikawa(2013/08/06) end
      *****************************************************************
      *    ＤＢ  オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ  クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           .
       900-DBDISCONNECT-EXT.
           EXIT.
