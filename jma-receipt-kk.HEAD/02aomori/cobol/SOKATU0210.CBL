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
       PROGRAM-ID.             SOKATU0210.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/29    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    山口    2005/10/18   公費再掲食事療養費欄の集計間違い
      *                                   を修正
      *  01.00.02    山口    2005/12/01   "180"と"280"は分けずに１項目と
      *                                   なるよう修正
      *  01.00.03    内田    2006/10/09   帳票変更による修正（公費欄の増加）
      *  01.00.04    内田    2006/11/02   前期高齢者の集計の修正
      *  01.00.05    内田    2006/11/17   食事がない場合でも生活療養費を印字
      *  01.00.06    内田    2007/06/17   ０歳１０割患者を１０割欄へ印字
      *  01.00.07    内田    2007/09/11   老人の一部負担金欄を印字
      *                                   公費再掲欄にて法別番号を印字
      *                                   日レセver4.0対応
      *                                   システム管理マスタ「1901」の
      *                                   医療機関情報を参照
      *  01.00.08    内田    2008/04/27   帳票様式の変更に伴う修正
      *  01.00.09    内田    2008/09/04   シス管「2007」の公費まとめ対応
      *                                   SPA-AREA初期化
      *  01.00.10    吉川    2009/11/17   シス管「2007」のまとめ公費で
      *                                   6つ目以降がまとまらない不具合を修正
      *  01.00.11    吉川    2010/06/17   オンライン返戻対応
      *  01.00.12    吉川    2010/09/14   返戻の場合旧帳票が印字されるのを修正
      *  01.00.13    吉川    2011/03/24   東北地方太平洋沖地震対応（対象者別集計）
      *  01.00.14    吉川    2011/06/15   特別療養費国保対応
      *  01.00.15    吉川    2012/06/12   妊産婦(144)を使用している場合は
      *                                   １０割欄に印字するように修正
      *  01.00.16    吉川    2012/12/17   プラグイン対応
      *                                   医療機関情報編集修正
      *  01.00.17    吉川    2014/05/16   前期高齢者２割対応
      *                                   プログラムオプション対応
      *  01.00.18    吉川    2014/09/04   返戻時、再印刷画面のタイトル修正
      *  01.00.19    吉川    2014/10/03   ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.20    門間    2015/11/12   医療機関編集情報への開設者名追加対応
      *  01.00.21    萩野    2017/10/11   公費番号まとめ情報の公費番号の追加
      *                                   医療機関コード変更時、
      *                                   医療機関コード別の作成処理対応
      *                                   法別番号の記載修正
      *                                   SITEKBN変更（動作に変更なし）
      *  01.00.22    萩野    2018/06/08   医療機関情報の取得エラーを修正
      *  01.00.23    井上    2019/12/09   修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT0210TMP-FILE ASSIGN    SKT0210TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT0210TMP-KEY
                                   FILE    STATUS  IS  STS-SKT0210.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル  
       FD  SKT0210TMP-FILE.
           COPY    "CPSKT0210TMP.INC".
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
      *    請求書一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT0210TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
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
           COPY    "SKT0220.INC".
           COPY    "SKT0230.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT0210         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT0210TMP      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-RECE02          PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-CNT             PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
      *
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
           03  WRK-KOHMAX          PIC 9(02).
           03  WRK-KOHNAME         PIC X(16).
      *
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-YUSEN           PIC X(06).
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
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX        PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KOH             PIC 9(02).
      *
           COPY    "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
           COPY    "CPRCF010.INC".
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
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
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
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    半角チェックサブ
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-TOKUBETU
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU0210"    TO  JOB-PGID
           MOVE    "国保請求書"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT0210"         TO  SKT0210TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT0210TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT0210TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT0210TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT0210TP-FULLNAME
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
           PERFORM 910-SYSKANRI-INV-SEC
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
           PERFORM 910-SYSKANRI-INV-SEC
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
                                           TO WRK-2007-KOHNUM(IDX1 IDZ)
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
           OPEN    OUTPUT  UPD-FILE
      *  
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU0210"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    優先保険者番号
           MOVE    "YUSEN"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:6)        TO  WRK-YUSEN
           DISPLAY "優先保険者番号="  WRK-YUSEN
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
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT0210TMP-FILE
           CLOSE            SKT0210TMP-FILE
           OPEN     I-O     SKT0210TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT0210TMP-FILE
      *
           IF       FLG-UPD   =       1
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
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
                           PERFORM     310-SEIKYU-UPD-SEC
                   END-IF
           END-IF
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU0210 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU0210 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA

           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC              SECTION.
      * 
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT0210TMP-REC
      *
           MOVE  RECE10-SRYYM       TO  SKT0210TMP-SRYYM
      *
           IF       RECE10-HKNJANUM(7:2)  =     SPACE
                    MOVE    RECE10-HKNJANUM(1:6)    TO
                            SKT0210TMP-HKNJANUM(1:6)
           ELSE
                    MOVE    RECE10-HKNJANUM(3:6)    TO
                            SKT0210TMP-HKNJANUM(1:6)
           END-IF
      *
      *   在総診の場合請求書を別にする
           MOVE     RECE10-ZAITAKU    TO    SKT0210TMP-ZAITAKU
      *
      *    出力順設定
           IF     WRK-YUSEN    =    SKT0210TMP-HKNJANUM
             MOVE        1          TO   SKT0210TMP-SYUTURYOKU
           ELSE
             IF     RECE10-PREFKBN  =    "1"
               MOVE      2          TO   SKT0210TMP-SYUTURYOKU
             ELSE
               MOVE      3          TO   SKT0210TMP-SYUTURYOKU
             END-IF
           END-IF
      *
      *    災害対象者は別帳票
           IF  RECE10-ETC-YKZFTN (1)  NOT =  ZERO
             MOVE  RECE10-ETC-YKZFTN (1)  TO  SKT0210TMP-GENMEN
           END-IF
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
           PERFORM  210-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " 診療年月＝"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " レセ種別＝"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    入外区分
           IF  RECE10-NYUGAIKBN  =  1
             MOVE  1  TO  IDX2
           ELSE
             MOVE  2  TO  IDX2
           END-IF
      *
           PERFORM  900-KOHSKY-READ-SEC
      *    老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
             IF  RECE10-KYURATE  =  9
               MOVE  13      TO  IDX1
             ELSE
               MOVE  14      TO  IDX1
             END-IF
           ELSE
      *
      *    一般／退職者
             IF  RECE10-HKNNUM  =  "060" OR "068"
      *    年齢チェック
               IF  RECE10-AGE  >=  70
                 EVALUATE  RECE10-KYURATE
                   WHEN   9    MOVE  1  TO  IDX1
                   WHEN  8
                     IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                         MOVE    1    TO    IDX1
                     ELSE
                         MOVE    2    TO    IDX1
                     END-IF
                   WHEN   7    MOVE  2  TO  IDX1
                 END-EVALUATE
               ELSE
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                     IF  RECE10-AGE  =  ZERO  AND  RECE10-KYURATE = 10
                       MOVE  5  TO  IDX1
                     ELSE
                       MOVE  6  TO  IDX1
                     END-IF
                 ELSE
                   IF  RECE10-AGE  >  2
                     EVALUATE  RECE10-KYURATE
                       WHEN   7    MOVE  3  TO  IDX1
                       WHEN  10    MOVE  5  TO  IDX1
                       WHEN  OTHER
                         MOVE  4  TO  IDX1
                         MOVE  RECE10-KYURATE  
                                       TO  SKT0210TMP-KYURATE(IDX2)
                     END-EVALUATE
                   ELSE
      *    １歳未満＋１０割患者は１０割に分類
                     IF  RECE10-AGE  =  ZERO  AND  RECE10-KYURATE = 10
                       MOVE  5  TO  IDX1
                     ELSE
                       MOVE  6  TO  IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *    乳幼児＋ゼロ歳の時のみ１０割に分類
               IF    RECE10-AGE       =  ZERO  AND
                    (RECE10-KOHNUM(1) = "141" OR
                     RECE10-KOHNUM(2) = "141" OR
                     RECE10-KOHNUM(2) = "141" OR
                     RECE10-KOHNUM(2) = "141")
                 MOVE    5    TO    IDX1
               END-IF
      *    妊産婦は無条件で１０割に分類
               IF    RECE02-KOHNUM(1)  =  "144" OR
                     RECE02-KOHNUM(2)  =  "144" OR
                     RECE02-KOHNUM(3)  =  "144" OR
                     RECE02-KOHNUM(4)  =  "144"
                 MOVE    5    TO    IDX1
               END-IF
             ELSE
      *
               IF  RECE10-HONKZKKBN  =  "1"
      *    年齢チェック
                 IF  RECE10-AGE  >=  70
                   IF  RECE10-KYURATE  =  9
                     MOVE  8   TO  IDX1
                   ELSE
                     MOVE  9   TO  IDX1
                   END-IF
                 ELSE
                   MOVE   7    TO  IDX1
                 END-IF
               ELSE
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                     IF  RECE10-AGE  =  ZERO  AND  RECE10-KYURATE = 10
                       MOVE  11  TO  IDX1
                     ELSE
                       MOVE  12  TO  IDX1
                     END-IF
                 ELSE
                   IF  RECE10-AGE  >  2
                     IF  RECE10-AGE  >=  70
                       IF  RECE10-KYURATE  =  9
                         MOVE  8   TO  IDX1
                       ELSE
                         MOVE  9   TO  IDX1
                       END-IF
                     ELSE
                       MOVE   10    TO  IDX1
                     END-IF
                   ELSE
      *    １歳未満＋１０割患者は１０割に分類
                     IF  RECE10-AGE  =  ZERO  AND  RECE10-KYURATE = 10
                       MOVE  11  TO  IDX1
                     ELSE
                       MOVE  12  TO  IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *    乳幼児＋ゼロ歳の時のみ１０割に分類
               IF    RECE10-AGE       =  ZERO  AND
                    (RECE10-KOHNUM(1) = "141" OR
                     RECE10-KOHNUM(2) = "141" OR
                     RECE10-KOHNUM(3) = "141" OR
                     RECE10-KOHNUM(4) = "141")
                 MOVE    11    TO    IDX1
               END-IF
      *    妊産婦は無条件で１０割に分類
               IF    RECE02-KOHNUM(1)  =  "144" OR
                     RECE02-KOHNUM(2)  =  "144" OR
                     RECE02-KOHNUM(3)  =  "144" OR
                     RECE02-KOHNUM(4)  =  "144"
                 MOVE    11    TO    IDX1
               END-IF
             END-IF
           END-IF
      *
           PERFORM  2102-SYUKEI-SEC
      *
      *    公費併用
           IF  RECE10-KOHNUM(1)  NOT =  SPACE
             PERFORM  220-KOH-SYUKEI-SEC
           END-IF
      *
           IF  FLG-SKT0210TMP  =  1
               WRITE    SKT0210TMP-REC
           ELSE
               REWRITE  SKT0210TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・集計処理
      *****************************************************************
       2102-SYUKEI-SEC         SECTION.
      *
      *    総件数
           IF  IDX1  =  13  OR  14
             ADD   1                 TO  SKT0210TMP-GRKENSU
           ELSE
             ADD   1                 TO  SKT0210TMP-GKENSU
           END-IF
      *    療養の給付：件数
           ADD   1                   TO  SKT0210TMP-KENSU (IDX1 IDX2)
      *    療養の給付：診療実日数
           ADD   RECE10-JNISSU (1)   TO  SKT0210TMP-NISSU (IDX1 IDX2)
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT0210TMP-TENSU (IDX1 IDX2)
      *    療養の給付：薬剤一部負担金
           ADD   RECE10-YKZFTN (1)   TO  SKT0210TMP-YKZFTN(IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT0210TMP-ITBFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU (1)  >  0
      *    食事療養：件数
             ADD   1                 TO  SKT0210TMP-S-KENSU (IDX1)
           END-IF
      *    食事療養：日数
           ADD   RECE10-SHOKUJINISSU (1)   
                                     TO  SKT0210TMP-S-NISSU (IDX1)
      *    食事療養：金額
           ADD   RECE10-SHOKUJIRYOYOHI (1)
                                     TO  SKT0210TMP-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
           ADD   RECE10-SHOKUJIFTN (1)
                                     TO  SKT0210TMP-S-FTNGAKU(IDX1)
      *
           MOVE    1               TO  FLG-UPD
           .
       2102-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *    ０５１と０９１は同じ場所に集計する
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"    TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
      *    乳児と妊産婦は公費再掲から除く
             IF    RECE10-KOHNUM(IDX3)  =  "144"  OR
                  (RECE10-KOHNUM(IDX3)  =  "141"  AND
                   RECE10-AGE           =  ZERO)
                DISPLAY "県単公費の為、再掲対象外 " RECE10-PTNUM
                DISPLAY "年齢="  RECE10-AGE
             ELSE
      *
               IF  RECE10-KOHNUM(IDX3)  =  "280"
                 MOVE  "180"               TO  RECE10-KOHNUM(IDX3)
               END-IF
      *        公費番号まとめ情報があるとき
               IF  WRK-2007-MAX  >  ZERO
                 MOVE  SPACE     TO  WRK-KOHNAME
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
      *    公費判別
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >     20
                        OR       SKT0210TMP-KOHNUM (IDX4) = SPACE
                 IF  RECE10-KOHNUM (IDX3)  =  SKT0210TMP-KOHNUM (IDX4)
                     MOVE  IDX4  TO  CNT-KOH
                     MOVE  20    TO  IDX4
                 END-IF
               END-PERFORM
      *
      *    該当公費がなかった場合
               IF  IDX4  < 20
                 MOVE  IDX4  TO  CNT-KOH
               END-IF
      *    公費名
               MOVE  RECE10-KOHNUM(IDX3)
                                     TO  SKT0210TMP-KOHNUM(CNT-KOH)
      *    公費名称
               MOVE  WRK-KOHNAME     TO  SKT0210TMP-KOHNAME(CNT-KOH)
      *
      *    公費が負担する分だけセットする
      *    療養の給付：件数
               ADD  1                TO SKT0210TMP-KKENSU(CNT-KOH IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU(IDX3 + 1) 
                                     TO SKT0210TMP-KNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1) 
                                     TO SKT0210TMP-KTENSU(CNT-KOH IDX2)
      *    療養の給付：薬剤一部負担金
               ADD  RECE10-YKZFTN(IDX3 + 1) 
                                     TO SKT0210TMP-KYKZFTN(CNT-KOH IDX2)
      *    療養の給付：一部負担金
               ADD  RECE10-FTNMONEY (IDX3 + 1)
                                     TO SKT0210TMP-KITBFTN(CNT-KOH IDX2)
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養：件数
                 ADD   1             TO  SKT0210TMP-S-KKENSU (CNT-KOH)
               END-IF
      *    食事療養：日数
               ADD   RECE10-SHOKUJINISSU (IDX3 + 1)    
                                     TO  SKT0210TMP-S-KNISSU (CNT-KOH)
      *    食事療養：金額
               ADD   RECE10-SHOKUJIRYOYOHI (IDX3 + 1) 
                                     TO  SKT0210TMP-S-KKINGAKU(CNT-KOH)
      *    食事療養：標準負担額
               ADD   RECE10-SHOKUJIFTN (IDX3 + 1) 
                                     TO  SKT0210TMP-S-KFTNGAKU(CNT-KOH)
      *****
             END-IF
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
006900*    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       SKT0210TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     ZERO        TO       FLG-PASS
           MOVE     1           TO       WRK-CNT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    帳票編集<見出し>処理
             PERFORM 311-HEAD-HEN-SEC
      *    帳票編集<明細>処理
             PERFORM 321-BODY-HEN-SEC
      *    帳票印刷処理
             PERFORM 331-PRINT-OUT-SEC
      *
      *    公費が残っているか
             PERFORM  UNTIL  SKT0210TMP-KOHNUM (WRK-CNT) = SPACE
                      AND    FLG-PASS  =  1
      *    帳票編集<見出し>処理
               PERFORM 313-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 323-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 333-PRINT-OUT-SEC
               MOVE  1  TO  FLG-PASS
             END-PERFORM
             MOVE  ZERO  TO  FLG-PASS
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
             MOVE  1  TO  WRK-CNT
           END-PERFORM
      *
           CLOSE    SKT0210TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（一般）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT0220
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT0220-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT0220-SEIYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT0220-ADRS
           INSPECT SKT0220-ADRS          REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT0220-HOSPNAME
           INSPECT SKT0220-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT0220-KAISETUNAME
      *
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT0220-TEL
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCDN     TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT        TO  SKT0220-HOSPCDN
      *
      *    給付割合
           IF  SKT0210TMP-KYURATE(1) NOT = ZERO
             MOVE  SKT0210TMP-KYURATE(1)   TO  SKT0220-KYURATE(1)
           END-IF
           IF  SKT0210TMP-KYURATE(2) NOT = ZERO
             MOVE  SKT0210TMP-KYURATE(2)   TO  SKT0220-KYURATE(2)
           END-IF
      *
      *    保険者名
           PERFORM  920-HKNJAINF-SET-SEC
           IF     HKNJA-HKNJANAME        =   SPACE
             MOVE     "殿"               TO  SKT0220-HKNJANAME(25:2)
           ELSE
             STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                     "  殿"              DELIMITED BY SIZE
                     INTO                SKT0220-HKNJANAME
             END-STRING
           END-IF
      *
      *     ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT0220-NO
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT0220-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（一般）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     12
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT0210TMP-KENSU (IDX1 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT0220-KENSU(IDX1 IDX2)
      *
      *    療養の給付：日数
               MOVE  SKT0210TMP-NISSU (IDX1 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT0220-NISSU(IDX1 IDX2)
      *
      *    療養の給付：点数
               MOVE  SKT0210TMP-TENSU (IDX1 IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT0220-TENSU(IDX1 IDX2)
      *
      *    療養の給付：一部負担金
               IF  IDX1  =  1 OR 2 OR 8 OR 9
                 MOVE  SKT0210TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT0220-ITBFTN(IDX1 IDX2)
               END-IF
             END-PERFORM
      *
      *    食事療養：件数
             MOVE  SKT0210TMP-S-KENSU (IDX1)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT0220-S-KENSU(IDX1)
      *
      *    食事療養：日数
             MOVE  SKT0210TMP-S-NISSU (IDX1)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT0220-S-NISSU(IDX1)
      *
      *    食事療養：金額
             MOVE  SKT0210TMP-S-KINGAKU (IDX1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT0220-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
             MOVE  SKT0210TMP-S-FTNGAKU (IDX1)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT0220-S-FTNGAKU(IDX1)
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（一般）
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           IF  SKT0210TMP-SRYYM  >=  WRK-CONS-SRYYM-200804
               MOVE    "SKT0220.red"   TO  SPRT-PRTID
           ELSE
               MOVE    "SKT0220o.red"  TO  SPRT-PRTID
           END-IF
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT0220             TO  SPRT-PRTDATA
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
       331-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（老人）
      *****************************************************************
       313-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT0230
           INITIALIZE                    IDX4
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCDN     TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT        TO  SKT0230-HOSPCDN
      *
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT0230-NO
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT0230-TOKUBETU
           END-IF
      *
           .
       313-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（老人）
      *****************************************************************
       323-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT  =  1
      *    編集処理
             PERFORM  VARYING  IDX1  FROM  13  BY  1
                      UNTIL    IDX1  >     14
               ADD  1  TO  IDX4
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT0210TMP-KENSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT0230-KENSU(IDX4 IDX2)
      *
      *    療養の給付：日数
                 MOVE  SKT0210TMP-NISSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT0230-NISSU(IDX4 IDX2)
      *
      *    療養の給付：点数
                 MOVE  SKT0210TMP-TENSU (IDX1 IDX2)   TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT0230-TENSU(IDX4 IDX2)
      *
      *    療養の給付：一部負担金
      *          老人も印字する
                 MOVE  SKT0210TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z11
                 MOVE  WRK-Z11  TO  SKT0230-ITBFTN(IDX4 IDX2)
               END-PERFORM
      *
      *    食事療養：件数
               MOVE  SKT0210TMP-S-KENSU (IDX1)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT0230-S-KENSU(IDX4)
      *
      *    食事療養：日数
               MOVE  SKT0210TMP-S-NISSU (IDX1)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT0230-S-NISSU(IDX4)
      *
      *    食事療養：金額
               MOVE  SKT0210TMP-S-KINGAKU (IDX1)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT0230-S-KINGAKU(IDX4)
      *
      *    食事療養：標準負担額
               MOVE  SKT0210TMP-S-FTNGAKU (IDX1)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT0230-S-FTNGAKU(IDX4)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           IF  WRK-PARA-SYOKBN  =  "1"
               MOVE  8  TO  WRK-KOHMAX
           ELSE
               MOVE  5  TO  WRK-KOHMAX
           END-IF
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     WRK-KOHMAX
                    OR  WRK-CNT    >     20
                    OR  SKT0210TMP-KOHNUM(WRK-CNT) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT0210TMP-KKENSU (WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT0230-KKENSU(IDX3 IDX2)
      *
      *    療養の給付：日数
               MOVE  SKT0210TMP-KNISSU (WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT0230-KNISSU(IDX3 IDX2)
      *
      *    療養の給付：点数
               MOVE  SKT0210TMP-KTENSU (WRK-CNT IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT0230-KTENSU(IDX3 IDX2)
      *
      *    療養の給付：一部負担金
               MOVE  SKT0210TMP-KITBFTN (WRK-CNT IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT0230-KITBFTN(IDX3 IDX2)
             END-PERFORM
      *
      *    食事療養：件数
             MOVE  SKT0210TMP-S-KKENSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT0230-S-KKENSU(IDX3)
      *
      *    食事療養：日数
             MOVE  SKT0210TMP-S-KNISSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT0230-S-KNISSU(IDX3)
      *
      *    食事療養：金額
             MOVE  SKT0210TMP-S-KKINGAKU (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT0230-S-KKINGAKU(IDX3)
      *
      *    食事療養：標準負担額
             MOVE  SKT0210TMP-S-KFTNGAKU (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT0230-S-KFTNGAKU(IDX3)
      *
      *    公費名
             IF  SKT0210TMP-KOHNUM(WRK-CNT)  =  "180"
               MOVE  "マル障"           TO  SKT0230-KOHNAME (IDX3)
               MOVE  "80"               TO  SKT0230-HBTNUM (IDX3)
             ELSE
               MOVE  SKT0210TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
               PERFORM  950-HKNNUM-SET-SEC
               IF  SKT0210TMP-KOHNAME (WRK-CNT)  NOT =  SPACE
                 MOVE  SKT0210TMP-KOHNAME (WRK-CNT)
                                          TO  SKT0230-KOHNAME (IDX3)
               ELSE
                 MOVE  HKN-TANSEIDONAME   TO  SKT0230-KOHNAME (IDX3)
               END-IF
      *      法別番号
               MOVE  HKN-HBTNUM           TO  SKT0230-HBTNUM (IDX3)
             END-IF
      *****
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       323-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（老人）
      *****************************************************************
       333-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
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
           IF  SKT0210TMP-SRYYM  >=  WRK-CONS-SRYYM-200804
               MOVE   "SKT0230.red"    TO  SPRT-PRTID
           ELSE
               MOVE   "SKT0230o.red"   TO  SPRT-PRTID
           END-IF
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT0230             TO  SPRT-PRTDATA
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
       333-PRINT-OUT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
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
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求管理ＤＢ更新処理
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
      *
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC           TO  MCPDATA-REC
                   MOVE    "DBUPDATE"           TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU0210 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU0210 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                   MOVE    RECE10-KEY      TO  WRK-RECEERR(33:)
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM              TO  HKNJA-HOSPNUM
           MOVE  SKT0210TMP-HKNJANUM      TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC             TO  MCPDATA-REC
           MOVE  "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"        TO  MCP-TABLE
             MOVE    "key"                 TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"               USING
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
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE  SPA-HOSPNUM             TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM              TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                         TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                  TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD          TO  HKN-TEKEDYMD
           MOVE  HKNNUM-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key5"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknnum"          TO  MCP-TABLE
             MOVE    "key5"                TO  MCP-PATHNAME
             PERFORM  955-HKNNUM-READ-SEC
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key5"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"               USING
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
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
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
           MOVE   SPA-HOSPNUM           TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE  WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                            RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE            TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"       TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000               TO  RECE10-SRYYM
             MOVE  999999               TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
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
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
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
      *        返戻分でオンライン請求は読み飛ばす
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
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *         
               ADD     1               TO  CNT-RECE10   
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
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
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
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
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT0210TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT0210TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT0210TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求情報読み込み
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           INITIALIZE                       RECE02-REC
           MOVE    RECE10-KEY           TO  RECE02-KEY
           MOVE    RECE02-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_kohsky"     TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO         TO  FLG-RECE02
                   MOVE    MCPDATA-REC  TO  RECE02-REC
               ELSE
                   MOVE    1            TO  FLG-RECE02
                   INITIALIZE               RECE02-REC
               END-IF
           ELSE
               MOVE    1                TO  FLG-RECE02
               INITIALIZE                   RECE02-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_kohsky"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       900-KOHSKY-READ-EXT.
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
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT0210TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
           PERFORM 910-SYSKANRI-INV-SEC
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
           PERFORM 910-SYSKANRI-INV-SEC
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
           PERFORM 910-SYSKANRI-INV-SEC
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
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
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
      *****************************************************************
      *    ＤＢ　オープン処理
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
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
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
       900-DBCLOSE-EXT.
           EXIT.
