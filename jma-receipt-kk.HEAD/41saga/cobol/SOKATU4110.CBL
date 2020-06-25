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
       PROGRAM-ID.             SOKATU4110.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 佐賀・診療報酬請求書(国保請求書)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/25     山口         ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    有村       05/05/06    医療機関情報取得条件修正
      *  01.00.02    内田       07/08/03    老人＋原爆を４３被爆へ集計
      *  01.00.03    内田       07/09/30    日レセver4.0対応
      *                                     システム管理マスタ「1901」の
      *                                     医療機関情報を参照
      *                                     長期上位（974）は長期と同様の
      *                                     処理をする
      *  01.00.04    内田       07/12/05    政令指定都市は市でまとめる
      *  01.00.05    内田       08/01/29    保険者名の印字修正
      *  01.00.06    内田       08/05/11    ４月改正対応
      *  01.00.08    門間       08/11/20    シス管「2007」公費まとめ対応
      *                                     SPA-AREA初期化
      *  01.00.09    吉川       09/03/19    札幌市と仙台市の
      *                                     代表保険者番号変更
      *  01.00.10    吉川       09/04/03    精神通院の患者負担金は
      *                                     患者負担額欄に印字
      *  01.00.11    吉川       10/01/07    シス管「2007」のまとめ公費で
      *                                     6つ目以降がまとまらない不具合を修正
      *  01.00.12    吉川       10/05/17    オンライン返戻対応
      *  01.00.13    吉川       11/01/12    特別療養費国保対応
      *  01.00.14    吉川       13/02/07    プラグイン対応
      *                                     医療機関情報編集対応
      *  01.00.15    吉川       13/06/17    プログラムオプション対応
      *  01.00.16    吉川       14/05/15    前期高齢者２割対応
      *  01.00.17    吉川       14/10/27    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.18    門間       16/02/15    医療機関編集情報への開設者名追加対応
      *  01.00.19    門間       16/10/04    帳票様式の変更
      *                                     公費番号まとめ情報の公費番号の追加
      *                                     返戻時、再印刷画面のタイトル修正
      *  01.00.20    萩野       17/04/14    点数欄の印字桁数増加
      *                                     医療機関コード変更時、
      *                                     医療機関コード別の作成処理対応
      *  01.00.21    萩野       17/10/11    公費再掲欄に印字されない
      *                                     公費がある不具合を修正
      *                                     SITEKBN変更（動作に変更なし）
      *  01.00.22    萩野       19/02/07    修正履歴コメント削除
      *                                     保険番号情報の取得処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求書一時ファイル  
           SELECT  SKT4110TMP-FILE ASSIGN    SKT4110TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT4110TMP-KEY
                                   FILE    STATUS  IS  STS-SKT4110.
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル  
       FD  SKT4110TMP-FILE.
           COPY    "CPSKT4110TMP.INC".
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求書一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT4110TP//.
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
           COPY    "SKT4110.INC".
           COPY    "SKT4120.INC".
           COPY    "SKT4140.INC".
           COPY    "SKT4150.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT4110         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT4110TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-PASS2           PIC 9(01).
           03  FLG-PASS3           PIC 9(01).
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
           03  IDX6                PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *##  パラメタ領域（地方公費仕様）
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
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
           03  WRK-CNT2            PIC 9(02).
           03  WRK-CNT3            PIC 9(02).
           03  WRK-CNT4            PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-ITBFTN-G        PIC 9(09).
           03  WRK-ITBFTN          PIC 9(08).
           03  WRK-KOHNAME         PIC X(10).
      *
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13             PIC Z,ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2           PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-YUSEN           PIC X(06).
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
           03  WRK-KAISETUNAME     PIC X(80).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
           03  CNT-KOH             PIC 9(02).
           03  CNT-PRINTKOH        PIC 9(02).
           03  CNT-GENBAKU         PIC 9(01).
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
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "COMMON-SPA".
      *
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
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
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
      *
           COPY    "CPRCF010.INC".
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
           INITIALIZE                  WRK-PARA-HKNJANUM
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *###（地方公費仕様）
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
           MOVE    "SOKATU4110"    TO  JOB-PGID
           MOVE    "国保請求書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
      *##（地方公費仕様：LNK-PRTKANRI-TERMID)
           MOVE    "SKT4110"         TO  SKT4110TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT4110TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT4110TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT4110TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT4110TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
      *###（地方公費仕様)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *##
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
           OPEN    OUTPUT  UPD-FILE
      *
           DISPLAY     "WRK-PARA-SYOKBN=" WRK-PARA-SYOKBN
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
           MOVE    "SOKATU4110"              TO  WRK-PRGID
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
           OPEN     OUTPUT  SKT4110TMP-FILE
           CLOSE            SKT4110TMP-FILE
           OPEN     I-O     SKT4110TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT4110TMP-FILE
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
           DISPLAY "*** SOKATU4110 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU4110 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
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
           INITIALIZE       SKT4110TMP-REC
           INITIALIZE       SKT4110TMP-HKNJANUM
      *
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  SKT4110TMP-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  SKT4110TMP-HKNJANUM(1:6)
           END-IF
      *    政令指定都市は代表番号でまとめる
           PERFORM  20011-SEIREI-SEC
      *
      *    在宅・在医総
           MOVE    RECE10-ZAITAKU        TO  SKT4110TMP-ZAITAKU
      *
      *    法定外給付チェック
           IF  SKT4110TMP-HKNJANUM(3:1) = "3"
             IF  RECE10-RJNHKNNUM  =  SPACE
               IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
                 IF  (RECE10-FLG(1) NOT = 1) AND (RECE10-AGE < 70)
                   IF  RECE10-KYURATE  NOT =  7
                       MOVE  RECE10-KYURATE TO  SKT4110TMP-KYURATE
                   END-IF
                 END-IF
               ELSE
                 IF  (RECE10-AGE  >  2)  AND  (RECE10-AGE  <  70)
                   IF  RECE10-KYURATE  NOT =  7
                       MOVE  RECE10-KYURATE TO  SKT4110TMP-KYURATE
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
      *    印字順設定
           IF    WRK-YUSEN        =  SKT4110TMP-HKNJANUM
             MOVE          1   TO    SKT4110TMP-SYUTURYOKU
           ELSE
      *   県外→県内市町村→県内扱い国保組合の順で印刷する
             IF     RECE10-PREFKBN            =  "1"
               IF   SKT4110TMP-HKNJANUM(3:1)  =  "3"
                 MOVE  4     TO     SKT4110TMP-SYUTURYOKU
               ELSE
                 MOVE  3     TO     SKT4110TMP-SYUTURYOKU
               END-IF
             ELSE
                 MOVE  2     TO     SKT4110TMP-SYUTURYOKU
             END-IF
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
      *    政令指定都市の代表番号集計
      *****************************************************************
       20011-SEIREI-SEC              SECTION.
      *
      *    札幌市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "014"
             MOVE  "010017"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    仙台市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "044"
             MOVE  "040014"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    さいたま市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "114"
             MOVE  "114009"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    千葉市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "124"
             MOVE  "124008"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    横浜市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "144"
             MOVE  "144006"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    川崎市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "145"
             MOVE  "145003"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    名古屋市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "234"
             MOVE  "234005"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    京都市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "264"
             MOVE  "264002"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    大阪市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "274"
             MOVE  "274001"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    神戸市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "284"
             MOVE  "284000"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    広島市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "344"
             MOVE  "344002"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    北九州市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "404"
             MOVE  "404004"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    福岡市の場合、代表番号セット
           IF  SKT4110TMP-HKNJANUM (1:3) = "405"
             MOVE  "405001"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
           .
       20011-SEIREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    入外区分
           IF  RECE10-NYUGAIKBN  =  "1"
             MOVE  1      TO   IDX2
           ELSE
             MOVE  2      TO   IDX2
           END-IF
      *
      *    老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
             IF  RECE10-KYURATE  =  9
               MOVE  10   TO   IDX1
             ELSE
               MOVE  11   TO   IDX1
             END-IF
             MOVE    1    TO   SKT4110TMP-PAGE-FLG2
           ELSE
             MOVE    1    TO   SKT4110TMP-PAGE-FLG1
      *    一般国保か
             IF  RECE10-HKNNUM  =  "060" OR "068"
               IF  RECE10-AGE  >= 70
                 EVALUATE  RECE10-KYURATE
                   WHEN  9
                     MOVE    1     TO    IDX1
                   WHEN  8
                     IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                         MOVE    1    TO    IDX1
                     ELSE
                         MOVE    2    TO    IDX1
                     END-IF
                   WHEN  7
                     MOVE    2     TO    IDX1
                 END-EVALUATE
               ELSE
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                    MOVE  4      TO  IDX1
                 ELSE
                   IF  RECE10-AGE  >= 3
                     MOVE    3     TO    IDX1
                   ELSE
                     MOVE    4     TO    IDX1
                   END-IF
                 END-IF
               END-IF
      *
             ELSE
               IF  RECE10-AGE  >=  70
                 EVALUATE  RECE10-KYURATE
                   WHEN  9
                     MOVE    6     TO    IDX1
                   WHEN  8
                     IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                         MOVE    6    TO    IDX1
                     ELSE
                         MOVE    7    TO    IDX1
                     END-IF
                   WHEN  7
                     MOVE    7     TO    IDX1
                 END-EVALUATE
               ELSE
                 IF  RECE10-HONKZKKBN  =  1
                   MOVE  5    TO   IDX1
                 ELSE
                   IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                   AND  (RECE10-FLG (1)   =  1)
                      MOVE   9  TO  IDX1
                   ELSE
                     IF  RECE10-AGE  <  3
                       MOVE  9  TO  IDX1
                     ELSE
                       MOVE  8  TO  IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
      *     在宅の患者であったらIDXをふり直す
           IF  RECE10-ZAITAKU  =  1
             EVALUATE  IDX1
             WHEN  6
               MOVE   3           TO  IDX1
             WHEN  7
               MOVE   4           TO  IDX1
             WHEN  10
               MOVE   5           TO  IDX1
             WHEN  11
               MOVE   6           TO  IDX1
             END-EVALUATE
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
      **    公費負担分
           IF  RECE10-KOHNUM (1)   NOT =   SPACE  OR
               RECE10-KOHNUM (2)   NOT =   SPACE  OR
               RECE10-KOHNUM (3)   NOT =   SPACE  OR
               RECE10-KOHNUM (4)   NOT =   SPACE
             IF  RECE10-RJNHKNNUM  =  SPACE
                 PERFORM 220-KOH-SYUKEI-SEC
             ELSE
                 PERFORM 2201-ROJINKOH-SYUKEI-SEC
             END-IF
           END-IF
      *
      *   老人・原爆集計
           IF  RECE10-ETC-KOHNUM(1)    =  "019"   OR
               RECE10-ETC-KOHNUM(2)    =  "019"   OR
               RECE10-ETC-KOHNUM(3)    =  "019"   OR
               RECE10-ETC-KOHNUM(4)    =  "019"
             PERFORM 2201-ROJINGENBK-SYUKEI-SEC
           END-IF
      *
      **   長期療養件数集計
           IF  RECE10-ETC-KOHNUM(1)    =  "972"
           OR  RECE10-ETC-KOHNUM(2)    =  "972"
           OR  RECE10-ETC-KOHNUM(3)    =  "972"
           OR  RECE10-ETC-KOHNUM(4)    =  "972"
           OR  RECE10-ETC-KOHNUM(1)    =  "974"
           OR  RECE10-ETC-KOHNUM(2)    =  "974"
           OR  RECE10-ETC-KOHNUM(3)    =  "974"
           OR  RECE10-ETC-KOHNUM(4)    =  "974"
             ADD    1       TO    SKT4110TMP-TYOUKI
             MOVE    1    TO   SKT4110TMP-PAGE-FLG1
           END-IF   
      *
           IF  FLG-SKT4110TMP  =  1
               WRITE    SKT4110TMP-REC
           ELSE
               REWRITE  SKT4110TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・一般国保分集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                  TO  SKT4110TMP-KENSU (IDX1 IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(1)    TO  SKT4110TMP-NISSU(IDX1 IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO  SKT4110TMP-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD  RECE10-FTNMONEY(1)  TO  SKT4110TMP-ITBFTN(IDX1 IDX2)
      *    療養の給付：薬剤一部負担金
           ADD  RECE10-YKZFTN(1)    TO  SKT4110TMP-YKZFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養:件数
             ADD   1  TO  SKT4110TMP-S-KENSU(IDX1)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(1)  TO  SKT4110TMP-S-NISSU(IDX1)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(1) TO SKT4110TMP-S-KINGK(IDX1)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(1)    TO  SKT4110TMP-S-FTNGK(IDX1)
      *
           MOVE    1               TO  FLG-UPD 
           .
       2101-SYUKEI-EXT.
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
      *    ５１、９１は１つにまとめる
            IF    RECE10-KOHNUM(IDX3)  =  "091"
              MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
            END-IF
      *
      *      公費番号まとめ情報があるとき
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
             MOVE    1    TO   SKT4110TMP-PAGE-FLG2
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     20
                      OR       SKT4110TMP-KOHNUM (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX3)  =  SKT4110TMP-KOHNUM (IDX4)
                   MOVE  IDX4  TO  CNT-KOH
                   MOVE  20    TO  IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF  IDX4  < 20
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
             MOVE  RECE10-KOHNUM(IDX3)  TO  SKT4110TMP-KOHNUM(CNT-KOH)
      *    公費名称
             MOVE  WRK-KOHNAME          TO  SKT4110TMP-KOHNAME(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  SKT4110TMP-KKENSU (CNT-KOH IDX2)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX3 + 1)  TO
                               SKT4110TMP-KNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                               SKT4110TMP-KTENSU(CNT-KOH IDX2)
      *
      *      公費181の場合は患者負担額へ集計
             IF  RECE10-KOHNUM(IDX3)  =  "181"
                 OR  "021"
      *        療養の給付：患者負担額
               ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                 SKT4110TMP-KKANFTN(CNT-KOH IDX2)
             ELSE
      *        療養の給付：一部負担金
               ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                               SKT4110TMP-KITBFTN(CNT-KOH IDX2)
             END-IF
      *
             IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
               ADD   1  TO  SKT4110TMP-S-KKENSU(CNT-KOH)
             END-IF
      *    食事療養:日数
             ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT4110TMP-S-KNISSU(CNT-KOH)
      *    食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT4110TMP-S-KKINGK(CNT-KOH)
      *    食事療養：標準負担額
             ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                               SKT4110TMP-S-KFTNGK(CNT-KOH)
           END-PERFORM
      *
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    老人・原爆分集計処理 
      *****************************************************************
       2201-ROJINGENBK-SYUKEI-SEC        SECTION.
      *
      * 老人原爆集計
      *
           IF  RECE10-ZAITAKU     NOT =   1
             MOVE    1    TO   SKT4110TMP-PAGE-FLG3
           END-IF
      *
      *    療養の給付：件数
           ADD   1  TO  SKT4110TMP-R1-KKENSU (1 IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(1)  TO
                          SKT4110TMP-R1-KNISSU(1 IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO
                          SKT4110TMP-R1-KTENSU(1 IDX2)
      *    食事療養:件数
           IF  RECE10-SHOKUJINISSU(1)  >  0
             ADD   1  TO  SKT4110TMP-SR1-KKENSU(1)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(1)  TO
                          SKT4110TMP-SR1-KNISSU(1)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(1)  TO
                          SKT4110TMP-SR1-KKINGK(1)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(1)  TO
                             SKT4110TMP-SR1-KFTNGK(1)
      *
           .
       2201-ROJINGENBK-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費負担分集計処理１
      *****************************************************************
       2201-ROJINKOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *     ５１、９１は１つにまとめる
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
      *      公費番号まとめ情報があるとき
               IF  WRK-2007-MAX  >  ZERO
                 MOVE  SPACE     TO  WRK-KOHNAME
                 PERFORM VARYING IDZ1    FROM    1   BY  1
                         UNTIL   IDZ1    >       WRK-2007-MAX
                   PERFORM VARYING IDZ2  FROM    1   BY  1
                           UNTIL   IDZ2  >       10
                           OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                     IF  RECE10-KOHNUM(IDX3)  
                                   =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                       MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                   TO  RECE10-KOHNUM (IDX3)
                       MOVE  WRK-2007-MEISYO (IDZ1)
                                   TO  WRK-KOHNAME
                       MOVE  10            TO  IDZ2
                       MOVE  WRK-2007-MAX  TO  IDZ1
                     END-IF
                   END-PERFORM
                 END-PERFORM
               END-IF
      *
      *    公費判別
               IF  RECE10-ZAITAKU     NOT =   1
                 MOVE    1    TO   SKT4110TMP-PAGE-FLG3
               END-IF
      *
               IF  (( RECE10-KOHNUM (IDX3)  = "019" OR
                   "015" OR "051" OR "010" OR"011" OR "021" OR "020" )
                      AND ( RECE10-ZAITAKU NOT = 1 ))  OR
                   (( RECE10-KOHNUM (IDX3)  = "015" OR "051" OR "019" )  
                      AND  (RECE10-ZAITAKU = 1 ))
                   PERFORM  2202-ROJINKOH-SYUKEI2-SEC
               ELSE
                   PERFORM  2202-ROJINKOH-SYUKEI3-SEC
               END-IF
           END-PERFORM
      *
           .
       2201-ROJINKOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費負担分集計処理２(固定分) 
      *****************************************************************
       2202-ROJINKOH-SYUKEI2-SEC        SECTION.
      *
           EVALUATE  RECE10-KOHNUM (IDX3)
           WHEN  "019"
               MOVE    1           TO  IDX6
           WHEN  "015"
               MOVE    2           TO  IDX6
           WHEN  "051"
               MOVE    3           TO  IDX6
           WHEN  "010"
               MOVE    4           TO  IDX6
           WHEN  "011"
               MOVE    5           TO  IDX6
           WHEN  "021"
               MOVE    6           TO  IDX6
           WHEN  "020"
               MOVE    7           TO  IDX6
           END-EVALUATE
      *
      *    療養の給付：件数
           ADD   1  TO  SKT4110TMP-R1-KKENSU (IDX6 IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                     SKT4110TMP-R1-KNISSU(IDX6 IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                     SKT4110TMP-R1-KTENSU(IDX6 IDX2)
      *    療養の給付：一部負担金
           ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                     SKT4110TMP-R1-KITBFTN(IDX6 IDX2)
      *    療養の給付：控除額
           ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                     SKT4110TMP-R1-KKOUJO(IDX6 IDX2)
      *    食事療養:件数
           IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
             ADD   1  TO  SKT4110TMP-SR1-KKENSU(IDX6)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                                     SKT4110TMP-SR1-KNISSU(IDX6)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                                     SKT4110TMP-SR1-KKINGK(IDX6)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                                     SKT4110TMP-SR1-KFTNGK(IDX6)
           .
       2202-ROJINKOH-SYUKEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費負担分集計処理３ 
      *****************************************************************
       2202-ROJINKOH-SYUKEI3-SEC        SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >     13
                    OR       SKT4110TMP-R2-KOHNUM (IDX4) = SPACE
             IF  RECE10-KOHNUM (IDX3)  =  SKT4110TMP-R2-KOHNUM (IDX4)
                 MOVE  IDX4  TO  CNT-KOH
                 MOVE  13    TO  IDX4
             END-IF
           END-PERFORM
      *
      *    該当公費がなかった場合
           IF  IDX4  < 13
             MOVE  IDX4  TO  CNT-KOH
           END-IF
      *
      *    公費名
           MOVE  RECE10-KOHNUM(IDX3)  TO  SKT4110TMP-R2-KOHNUM(CNT-KOH)
      *    公費名称
           MOVE  WRK-KOHNAME          TO  SKT4110TMP-R2-KOHNAME(CNT-KOH)
      *    療養の給付：件数
           ADD   1  TO  SKT4110TMP-R2-KKENSU (CNT-KOH IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                     SKT4110TMP-R2-KNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                     SKT4110TMP-R2-KTENSU(CNT-KOH IDX2)
      *    療養の給付：一部負担金
           ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                     SKT4110TMP-R2-KITBFTN(CNT-KOH IDX2)
      *    療養の給付：控除額
           ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                     SKT4110TMP-R2-KKOUJO(CNT-KOH IDX2)
      *
           IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
             ADD   1  TO  SKT4110TMP-SR2-KKENSU(CNT-KOH)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                                        SKT4110TMP-SR2-KNISSU(CNT-KOH)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                                        SKT4110TMP-SR2-KKINGK(CNT-KOH)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                                        SKT4110TMP-SR2-KFTNGK(CNT-KOH)
           .
       2202-ROJINKOH-SYUKEI3-EXT.
           EXIT.
      *
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
           OPEN     INPUT       SKT4110TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     1           TO       WRK-CNT
                                         WRK-CNT2
                                         WRK-CNT3
                                         WRK-CNT4
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
             IF  SKT4110TMP-ZAITAKU  NOT =  1
      *        １枚目
      *        帳票編集<見出し>処理
               IF  SKT4110TMP-PAGE-FLG1     =   1
                 PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                 PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
                 PERFORM 330-PRINT-OUT-SEC
               END-IF
      *
      *        ２枚目
      *        公費が残っているか
               IF  SKT4110TMP-PAGE-FLG2     =   1
                 PERFORM  UNTIL  SKT4110TMP-KOHNUM (WRK-CNT) = SPACE
                          AND    FLG-PASS  =  1
      *        帳票編集<見出し>処理
                   PERFORM 311-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                   PERFORM 321-BODY-HEN-SEC
      *        帳票印刷処理
                   PERFORM 331-PRINT-OUT-SEC
                   MOVE  1  TO  FLG-PASS
                 END-PERFORM
               END-IF
      *
      *        ３枚目
               IF  SKT4110TMP-PAGE-FLG3  =  1
                 PERFORM  UNTIL SKT4110TMP-R2-KOHNUM(WRK-CNT2) = SPACE
                          AND   FLG-PASS2  =  1
      *        帳票編集<見出し>処理
                   PERFORM 312-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                   PERFORM 322-BODY-HEN-SEC
      *        帳票印刷処理
                   PERFORM 332-PRINT-OUT-SEC
                   MOVE  1  TO  FLG-PASS2
                 END-PERFORM
               END-IF
      *
      *      ４枚目(在宅用)
             ELSE
               PERFORM  UNTIL  SKT4110TMP-KOHNUM (WRK-CNT3) = SPACE
                        AND    SKT4110TMP-R2-KOHNUM (WRK-CNT4) = SPACE
                        AND    FLG-PASS3  =  1
      *        帳票編集<見出し>処理
                 PERFORM 313-HEAD-HEN-SEC
      *        帳票編集<明細>処理
                 PERFORM 323-BODY-HEN-SEC
      *        帳票印刷処理
                 PERFORM 333-PRINT-OUT-SEC
                 MOVE  1  TO  FLG-PASS3
               END-PERFORM
             END-IF
      *
             MOVE  ZERO  TO  FLG-PASS
                             FLG-PASS2
                             FLG-PASS3
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
             MOVE  1  TO  WRK-CNT
                          WRK-CNT2
                          WRK-CNT3
                          WRK-CNT4
           END-PERFORM
      *
           CLOSE    SKT4110TMP-FILE
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
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT4110
      *
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT4110-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT4110-SRYYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT4110-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT4110-ADRS
           INSPECT SKT4110-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT4110-HOSPNAME
           INSPECT SKT4110-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT4110-KAISETUNAME
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT4110-TEL
      *    保険者番号
           MOVE    SKT4110TMP-HKNJANUM   TO  SKT4110-HKNJANUM
      *    保険者名
           MOVE    SKT4110TMP-HKNJANUM  TO  WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE    WRK-HKNJANAME        TO  SKT4110-HOKENJANAME
      *    保険者番号の印字を変える
      *    （指定されている番号が保険者マスタと異なる）
           PERFORM  3101-HKNJANUM-CHANG-SEC
      *    保険者番号
           MOVE    SKT4110TMP-HKNJANUM   TO  SKT4110-HKNJANUM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT4110-HOSPCD
      *    在総診・在医総
           IF    SKT4110TMP-ZAITAKU  =  "1"
             MOVE    "在"                TO  SKT4110-ZAI1
             MOVE    "◯"                TO  SKT4110-ZAI2
           END-IF
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT4110-TOKUBETU
           END-IF
      *
      *    給付割合
           EVALUATE    SKT4110TMP-KYURATE
           WHEN 10
             MOVE    "◯"                TO  SKT4110-KYURATE(1)
             MOVE    "×"                TO  SKT4110-KYURATE(4)
           WHEN 9
             MOVE    "◯"                TO  SKT4110-KYURATE(2)
             MOVE    "×"                TO  SKT4110-KYURATE(4)
           WHEN 8
             MOVE    "◯"                TO  SKT4110-KYURATE(3)
             MOVE    "×"                TO  SKT4110-KYURATE(4)
           END-EVALUATE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *---(01.00.04) LINE ADD START ------------------------------------
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       3101-HKNJANUM-CHANG-SEC          SECTION.
      *
      *    札幌市
           IF  SKT4110TMP-HKNJANUM  = "010017"
             MOVE  "014019"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    仙台市
           IF  SKT4110TMP-HKNJANUM  = "040014"
             MOVE  "044016"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    千葉市
           IF  SKT4110TMP-HKNJANUM  = "124008"
             MOVE  "124016"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    横浜市
           IF  SKT4110TMP-HKNJANUM  = "144006"
             MOVE  "144014"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    川崎市
           IF  SKT4110TMP-HKNJANUM  = "145003"
             MOVE  "145011"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    名古屋市
           IF  SKT4110TMP-HKNJANUM  = "234005"
             MOVE  "234013"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    京都市
           IF  SKT4110TMP-HKNJANUM  = "264002"
             MOVE  "264010"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    大阪市
           IF  SKT4110TMP-HKNJANUM  = "274001"
             MOVE  "274019"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    神戸市
           IF  SKT4110TMP-HKNJANUM  = "284000"
             MOVE  "284018"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    広島市
           IF  SKT4110TMP-HKNJANUM  = "344002"
             MOVE  "344010"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    北九州市
           IF  SKT4110TMP-HKNJANUM  = "404004"
             MOVE  "404012"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
      *    福岡市
           IF  SKT4110TMP-HKNJANUM  = "405001"
             MOVE  "405019"  TO  SKT4110TMP-HKNJANUM
           END-IF
      *
           .
       3101-HKNJANUM-CHANG-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM    1   BY  1
                    UNTIL    IDX1  >  9
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT4110TMP-KENSU(IDX1 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4110-KENSU(IDX1 IDX2)
      *    療養の給付：日数
               MOVE  SKT4110TMP-NISSU(IDX1 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4110-NISSU(IDX1 IDX2)
      *    療養の給付：点数
               MOVE  SKT4110TMP-TENSU(IDX1 IDX2)  TO  WRK-Z10
               MOVE  WRK-Z10  TO  SKT4110-TENSU(IDX1 IDX2)
      *    療養の給付：薬剤一部負担金
               MOVE  SKT4110TMP-YKZFTN (IDX1 IDX2)  TO  WRK-Z7
               MOVE  WRK-Z7   TO  SKT4110-ITBFTN(IDX1 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT4110TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT4110-ITBFTN(IDX1 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT4110TMP-S-KENSU(IDX1)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4110-S-KENSU(IDX1)
      *    食事療養：日数
             MOVE  SKT4110TMP-S-NISSU(IDX1)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4110-S-NISSU(IDX1)
      *    食事療養：金額
             MOVE  SKT4110TMP-S-KINGK(IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4110-S-KINGK(IDX1)
      *    食事療養：標準負担額
             MOVE  SKT4110TMP-S-FTNGK(IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4110-S-FTNGK(IDX1)
           END-PERFORM
      *
      *    長期療養件数
             MOVE    SKT4110TMP-TYOUKI       TO  WRK-Z5
             MOVE    WRK-Z5   TO  SKT4110-TYOKENSU
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
           MOVE   "SKT4110.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT4110             TO  SPRT-PRTDATA
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
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（老人・公費等）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT4120
      *
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT4120-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT4120-SRYYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT4120-SEIYMD
      *    保険者番号
           MOVE    SKT4110TMP-HKNJANUM   TO  SKT4120-HKNJANUM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT4120-HOSPCD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT4120-ADRS
           INSPECT SKT4120-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT4120-HOSPNAME
           INSPECT SKT4120-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME  TO  SKT4120-KAISETUNAME
      *    電話番号
           MOVE    WRK-HOSPTEL          TO  SKT4120-TEL
      *    保険者名
           MOVE    SKT4110TMP-HKNJANUM  TO  WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE    WRK-HKNJANAME        TO  SKT4120-HOKENJANAME
      *
      *    保険者番号の印字を変える
      *    （指定されている番号が保険者マスタと異なる）
           PERFORM  3101-HKNJANUM-CHANG-SEC
      *    保険者番号
           MOVE    SKT4110TMP-HKNJANUM   TO  SKT4120-HKNJANUM
      *    在総診・在医総
           IF    SKT4110TMP-ZAITAKU  =  "1"
             MOVE    "在"                TO  SKT4120-ZAI1
             MOVE    "◯"                TO  SKT4120-ZAI2
           END-IF
      *    給付割合
           EVALUATE    SKT4110TMP-KYURATE
           WHEN 10
             MOVE    "◯"                TO  SKT4120-KYURATE(1)
             MOVE    "×"                TO  SKT4120-KYURATE(4)
           WHEN 9
             MOVE    "◯"                TO  SKT4120-KYURATE(2)
             MOVE    "×"                TO  SKT4120-KYURATE(4)
           WHEN 8
             MOVE    "◯"                TO  SKT4120-KYURATE(3)
             MOVE    "×"                TO  SKT4120-KYURATE(4)
           END-EVALUATE
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT4120-TOKUBETU
           END-IF
      *
           INITIALIZE                    IDX4
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（公費等）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT  =  1
      *    老人編集処理
             PERFORM  VARYING  IDX1  FROM   10   BY  1
                      UNTIL    IDX1  >  11
               ADD  1  TO  IDX4
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT4110TMP-KENSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4120-KENSU(IDX4 IDX2)
      *    療養の給付：日数
                 MOVE  SKT4110TMP-NISSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4120-NISSU(IDX4 IDX2)
      *    療養の給付：点数
                 MOVE  SKT4110TMP-TENSU(IDX1 IDX2)  TO  WRK-Z10
                 MOVE  WRK-Z10  TO  SKT4120-TENSU(IDX4 IDX2)
      *    療養の給付：一部負担金
                   MOVE  SKT4110TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
                   MOVE  WRK-Z9  TO  SKT4120-ITBFTN(IDX4 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT4110TMP-S-KENSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4120-S-KENSU(IDX4)
      *    食事療養：日数
               MOVE  SKT4110TMP-S-NISSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4120-S-NISSU(IDX4)
      *    食事療養：金額
               MOVE  SKT4110TMP-S-KINGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4120-S-KINGK(IDX4)
      *    食事療養：標準負担額
               MOVE  SKT4110TMP-S-FTNGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4120-S-FTNGK(IDX4)
             END-PERFORM
      *
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     6
                    OR  WRK-CNT    >     20
                    OR  SKT4110TMP-KOHNUM(WRK-CNT) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT4110TMP-KKENSU(WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4120-KKENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT4110TMP-KNISSU(WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4120-KNISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT4110TMP-KTENSU(WRK-CNT IDX2)   TO  WRK-Z10
               MOVE  WRK-Z10  TO  SKT4120-KTENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT4110TMP-KITBFTN(WRK-CNT IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4120-KITBFTN(IDX3 IDX2)
      *    療養の給付：患者負担金
               MOVE  SKT4110TMP-KKANFTN(WRK-CNT IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4120-KKANFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT4110TMP-S-KKENSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4120-S-KKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT4110TMP-S-KNISSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4120-S-KNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT4110TMP-S-KKINGK (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4120-S-KKINGK(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT4110TMP-S-KFTNGK (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4120-S-KFTNGK(IDX3)
      *    公費名
             MOVE  SKT4110TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM         TO  SKT4120-HBTNUM(IDX3)
             MOVE  HKN-TANSEIDONAME   TO  SKT4120-KOHNAME(IDX3)
      *
             IF  SKT4110TMP-KOHNAME (WRK-CNT)  NOT =  SPACE
               MOVE  SKT4110TMP-KOHNAME (WRK-CNT)
                                        TO  SKT4120-KOHNAME(IDX3)
             ELSE
               MOVE  HKN-TANSEIDONAME   TO  SKT4120-KOHNAME(IDX3)
             END-IF
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（老人・公費等）
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
           MOVE   "SKT4120.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT4120             TO  SPRT-PRTDATA
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
057400*
      *****************************************************************
      *  帳票編集<ヘッダー部>処理（老人保険＋公費）
      *****************************************************************
       312-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT4140
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT4140-NO
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT4140-HOSPCD
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT4140-TOKUBETU
           END-IF
      *
           .
       312-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *  帳票編集<明細行>処理（老人保健＋公費）
      *****************************************************************
       322-BODY-HEN-SEC      SECTION.
      *
      *    ２枚目以上か
           IF  WRK-CNT2  =  1
      *    固定の公費分(上記７つ分)
             PERFORM  VARYING  IDX1  FROM   1   BY  1
                      UNTIL    IDX1  >  7
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT4110TMP-R1-KKENSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4140-KKENSU(IDX1 IDX2)
      *    療養の給付：日数
                 MOVE  SKT4110TMP-R1-KNISSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4140-KNISSU(IDX1 IDX2)
      *    療養の給付：点数
                 MOVE  SKT4110TMP-R1-KTENSU(IDX1 IDX2)  TO  WRK-Z10
                 MOVE  WRK-Z10  TO  SKT4140-KTENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT4110TMP-R1-KITBFTN (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9  TO  SKT4140-KITBFTN(IDX1 IDX2)
      *    療養の給付：控除額
                 MOVE  SKT4110TMP-R1-KKOUJO (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9  TO  SKT4140-KKOUJO(IDX1 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT4110TMP-SR1-KKENSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4140-S-KKENSU(IDX1)
      *    食事療養：日数
               MOVE  SKT4110TMP-SR1-KNISSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4140-S-KNISSU(IDX1)
      *    食事療養：金額
               MOVE  SKT4110TMP-SR1-KKINGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4140-S-KKINGK(IDX1)
      *    食事療養：標準負担額
               MOVE  SKT4110TMP-SR1-KFTNGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4140-S-KFTNGK(IDX1)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     1
                    OR  WRK-CNT2   >     13
                    OR  SKT4110TMP-R2-KOHNUM(WRK-CNT2) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT4110TMP-R2-KKENSU(WRK-CNT2 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4140-KKENSU(8 IDX2)
      *    療養の給付：日数
               MOVE  SKT4110TMP-R2-KNISSU(WRK-CNT2 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4140-KNISSU(8 IDX2)
      *    療養の給付：点数
               MOVE  SKT4110TMP-R2-KTENSU(WRK-CNT2 IDX2)  TO  WRK-Z10
               MOVE  WRK-Z10  TO  SKT4140-KTENSU(8 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT4110TMP-R2-KITBFTN (WRK-CNT2 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9  TO  SKT4140-KITBFTN(8 IDX2)
      *    療養の給付：控除額
               MOVE  SKT4110TMP-R2-KKOUJO (WRK-CNT2 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9  TO  SKT4140-KKOUJO(8 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT4110TMP-SR2-KKENSU(WRK-CNT2)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4140-S-KKENSU(8)
      *    食事療養：日数
             MOVE  SKT4110TMP-SR2-KNISSU(WRK-CNT2)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4140-S-KNISSU(8)
      *    食事療養：金額
             MOVE  SKT4110TMP-SR2-KKINGK(WRK-CNT2)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4140-S-KKINGK(8)
      *    食事療養：標準負担額
             MOVE  SKT4110TMP-SR2-KFTNGK(WRK-CNT2)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4140-S-KFTNGK(8)
      *    公費名
             MOVE  SKT4110TMP-R2-KOHNUM(WRK-CNT2)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM         TO  SKT4140-HBTNUM
             MOVE  HKN-TANSEIDONAME   TO  SKT4140-KOHNAME
      *
             IF  SKT4110TMP-R2-KOHNAME(WRK-CNT2)  NOT =  SPACE
               MOVE  SKT4110TMP-R2-KOHNAME(WRK-CNT2)
                                        TO  SKT4140-KOHNAME
             ELSE
               MOVE  HKN-TANSEIDONAME   TO  SKT4140-KOHNAME
             END-IF
      *
             ADD  1  TO  WRK-CNT2
           END-PERFORM
      *
           .
       322-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *  帳票印刷処理（老人保険＋公費）
      *****************************************************************
       332-PRINT-OUT-SEC                SECTION.
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
           MOVE   "SKT4140.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT4140             TO  SPRT-PRTDATA
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
       332-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *  帳票編集<ヘッダー部>処理（在宅分）
      *****************************************************************
       313-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT4150
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT4150-NO
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT4150-SRYYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT4150-SEIYMD
      *    保険者番号
           MOVE    SKT4110TMP-HKNJANUM   TO  SKT4150-HKNJANUM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT4150-HOSPCD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT4150-ADRS
           INSPECT SKT4150-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT4150-HOSPNAME
           INSPECT SKT4150-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME  TO  SKT4150-KAISETUNAME
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT4150-TEL
      *    保険者名
           MOVE    SKT4110TMP-HKNJANUM   TO  WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE    WRK-HKNJANAME         TO  SKT4150-HOKENJANAME
      *
      *    保険者番号の印字を変える
      *    （指定されている番号が保険者マスタと異なる）
           PERFORM  3101-HKNJANUM-CHANG-SEC
      *    保険者番号
           MOVE    SKT4110TMP-HKNJANUM   TO  SKT4150-HKNJANUM
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT4150-TOKUBETU
           END-IF
      *
           .
       313-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *  帳票編集<明細行>処理（在宅分）
      *****************************************************************
       323-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT3  =  1
      *    在宅編集処理
             PERFORM  VARYING  IDX1  FROM   1   BY  1
                      UNTIL    IDX1  >  6
      *    療養の給付：件数
               MOVE  SKT4110TMP-KENSU(IDX1 2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4150-KENSU(IDX1)
      *    療養の給付：日数
               MOVE  SKT4110TMP-NISSU(IDX1 2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4150-NISSU(IDX1)
      *    療養の給付：点数
               MOVE  SKT4110TMP-TENSU(IDX1 2)  TO  WRK-Z10
               MOVE  WRK-Z10  TO  SKT4150-TENSU(IDX1)
      *    療養の給付：一部負担金
               MOVE  SKT4110TMP-ITBFTN(IDX1 2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT4150-ITBFTN(IDX1)
             END-PERFORM
      *
      *    老人保健 ＋ 公費(固定の上記３つ分)の印字
             PERFORM  VARYING  IDX1  FROM   1   BY  1
                      UNTIL    IDX1  >  3
      *    療養の給付：件数
               MOVE  SKT4110TMP-R1-KKENSU(IDX1 2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4150-R-KKENSU(IDX1)
      *    療養の給付：日数
               MOVE  SKT4110TMP-R1-KNISSU(IDX1 2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT4150-R-KNISSU(IDX1)
      *    療養の給付：点数
               MOVE  SKT4110TMP-R1-KTENSU(IDX1 2)  TO  WRK-Z10
               MOVE  WRK-Z10  TO  SKT4150-R-KTENSU(IDX1)
      *    療養の給付：一部負担金
               MOVE  SKT4110TMP-R1-KITBFTN (IDX1 2)  TO  WRK-Z9
               MOVE  WRK-Z9  TO  SKT4150-R-KITBFTN(IDX1)
      *    療養の給付：控除額
               MOVE  SKT4110TMP-R1-KKOUJO (IDX1 2)  TO  WRK-Z9
               MOVE  WRK-Z9  TO  SKT4150-R-KKOUJO(IDX1)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     4
                    OR  WRK-CNT3   >     20
                    OR  SKT4110TMP-KOHNUM(WRK-CNT3) = SPACE
      *    療養の給付：件数
             MOVE  SKT4110TMP-KKENSU(WRK-CNT3 2)   TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4150-KKENSU(IDX3)
      *    療養の給付：日数
             MOVE  SKT4110TMP-KNISSU(WRK-CNT3 2)   TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4150-KNISSU(IDX3)
      *    療養の給付：点数
             MOVE  SKT4110TMP-KTENSU(WRK-CNT3 2)   TO  WRK-Z10
             MOVE  WRK-Z10  TO  SKT4150-KTENSU(IDX3)
      *    療養の給付：一部負担金
             MOVE  SKT4110TMP-KITBFTN(WRK-CNT3 2)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4150-KITBFTN(IDX3)
      *    療養の給付：患者負担額
             MOVE  SKT4110TMP-KKANFTN(WRK-CNT3 2)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT4150-KKANFTN(IDX3)
      *    公費名
             MOVE  SKT4110TMP-KOHNUM (WRK-CNT3)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM         TO  SKT4150-HBTNUM(IDX3)
             MOVE  HKN-TANSEIDONAME   TO  SKT4150-KOHNAME(IDX3)
      *
             IF  SKT4110TMP-KOHNAME (WRK-CNT3)  NOT =  SPACE
               MOVE  SKT4110TMP-KOHNAME (WRK-CNT3)
                                        TO  SKT4150-KOHNAME(IDX3)
             ELSE
               MOVE  HKN-TANSEIDONAME   TO  SKT4150-KOHNAME(IDX3)
             END-IF
      *
             ADD  1  TO  WRK-CNT3
           END-PERFORM
      *
      *    老人保健 ＋ 公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     1
                    OR  WRK-CNT4   >     13
                    OR  SKT4110TMP-R2-KOHNUM(WRK-CNT4) = SPACE
      *    療養の給付：件数
             MOVE  SKT4110TMP-R2-KKENSU(WRK-CNT4 2)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4150-R-KKENSU(4)
      *    療養の給付：日数
             MOVE  SKT4110TMP-R2-KNISSU(WRK-CNT4 2)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT4150-R-KNISSU(4)
      *    療養の給付：点数
             MOVE  SKT4110TMP-R2-KTENSU(WRK-CNT4 2)  TO  WRK-Z10
             MOVE  WRK-Z10  TO  SKT4150-R-KTENSU(4)
      *    療養の給付：一部負担金
             MOVE  SKT4110TMP-R2-KITBFTN (WRK-CNT4 2)  TO  WRK-Z9
             MOVE  WRK-Z9  TO  SKT4150-R-KITBFTN(4)
      *    療養の給付：控除額
             MOVE  SKT4110TMP-R2-KKOUJO (WRK-CNT4 2)  TO  WRK-Z9
             MOVE  WRK-Z9  TO  SKT4150-R-KKOUJO(4)
      *    公費名
             MOVE  SKT4110TMP-R2-KOHNUM(WRK-CNT4)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM         TO  SKT4150-R-HBTNUM
             MOVE  HKN-TANSEIDONAME   TO  SKT4150-R-KOHNAME
      *
             IF  SKT4110TMP-R2-KOHNAME (WRK-CNT4)  NOT =  SPACE
               MOVE  SKT4110TMP-R2-KOHNAME (WRK-CNT4)
                                        TO  SKT4150-R-KOHNAME
             ELSE
               MOVE  HKN-TANSEIDONAME   TO  SKT4150-R-KOHNAME
             END-IF
      *
             ADD  1  TO  WRK-CNT4
           END-PERFORM
      *
           .
       323-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *  帳票印刷処理（在宅分）
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
           MOVE   "SKT4150.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT4150             TO  SPRT-PRTDATA
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
      *
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
      *###（地方公費仕様）
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"             TO  MCP-FUNC
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
                       MOVE  "*** SOKATU4110 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU4110 SEIKYU INVALID ***"
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
             MOVE    "tbl_hknjainf"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
             MOVE  HKNJA-HKNJANAME   TO  WRK-HKNJANAME
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
             IF    HKN-HKNNUM  =  "051"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "特定疾患"   TO   HKN-TANSEIDONAME
             END-IF
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1  TO  FLG-HKNJAINF
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
           MOVE  SPA-HOSPNUM                 TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM                  TO  HKN-HKNNUM
           MOVE  LNK-PRTKANRI-SRYYM(1:6)     TO  HKN-TEKSTYMD(1:6)
           MOVE  "01"                        TO  HKN-TEKSTYMD(7:2)
           MOVE  HKN-TEKSTYMD                TO  HKN-TEKEDYMD
           MOVE  HKNNUM-REC                  TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key5"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
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
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key5"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
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
           MOVE  "DBFETCH"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC     TO  HKNNUM-REC
             MOVE  ZERO            TO  FLG-HKNNUM
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1               TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      *###（地方公費仕様）
           MOVE    SPA-HOSPNUM          TO  RECE10-HOSPNUM
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
      *
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
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT4110TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT4110TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT4110TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT4110TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
      *###（地方公費仕様）
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
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
       910-SEIKYU-INV-EXT.
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
