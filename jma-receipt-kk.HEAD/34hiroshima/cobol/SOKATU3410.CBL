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
       PROGRAM-ID.             SOKATU3410.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療報酬請求書
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/15    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    有村    05/05/06    医療機関情報取得条件修正
      *  01.00.02    中川    06/08/16    一部負担金非表示対応
      *  01.00.03    内田    06/09/29    市町村公費保険番号も印字対象とする
      *  01.00.04    内田    06/11/22    帳票様式の変更に伴う修正
      *  01.00.05    内田    07/07/10    特別療養費国保対応
      *                                  老人原爆のカウント修正
      *                                  （老人原爆は併用として扱う）
      *  01.00.06    内田    07/09/07    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.07    長屋    08/04/24    帳票様式に伴う修正
      *  01.00.08    内田    08/06/18    印字保険番号追加（193）
      *  01.00.09    内田    08/09/17    公費再掲欄のソート修正
      *                                  SPA-AREA初期化
      *                                  公費再掲欄の食事件数判定修正
      *                                  シス管「2007」の公費まとめ対応
      *  01.00.10    吉川    09/12/25    シス管「2007」のまとめ公費で
      *                                  6つ目以降がまとまらない不具合を修正
      *  01.00.11    吉川    10/04/07    オンライン返戻対応
      *  01.00.12    吉川    10/08/24    中国地方豪雨対応（対象者別集計）
      *  01.00.13    吉川    11/03/10    open-cobol1.0対応
      *  01.00.14    吉川    13/02/06    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.15    吉川    14/01/10    プログラムオプション対応
      *  01.00.16    吉川    14/05/14    前期高齢者２割対応
      *  01.00.17    吉川    14/07/04    再印刷時のタイトル修正
      *  01.00.18    吉川    14/10/27    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.19    門間    16/02/04    医療機関編集情報への開設者名追加対応
      *  01.00.20    萩野    17/11/08    公費番号まとめ情報の公費番号の追加
      *                                  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.21    井上    19/01/09    修正履歴コメント削除
      *                                  保険番号情報の取得処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT3410TMP-FILE ASSIGN    SKT3410TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT3410TMP-KEY
                                   FILE    STATUS  IS  STS-SKT3410.
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
       FD  SKT3410TMP-FILE.
           COPY    "CPSKT3410TMP.INC".
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
                                   BY         //SKT3410TP//.
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
           COPY    "SKT3410.INC".
           COPY    "SKT3420.INC".
           COPY    "SKT3430.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT3410         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT3410TMP      PIC 9(01).
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
           03  IDX-MAX             PIC 9(02).
           03  IDX-MIN             PIC 9(02).
           03  IDX-MAX-RJNKOH      PIC 9(02).
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
           03  WRK-HKNJANUM     PIC X(06).
           03  WRK-HKNJANAME    PIC X(40).
      *
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-CNT             PIC 9(02).
           03  WRK-CNT2            PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-KOHNAME         PIC X(16).
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
           03  WRK-PATH              PIC X(50).
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
      *    公費ソート用領域
      *    一般・退職用
       01  SORT-AREA.
           03  SORT-KOH-TBL OCCURS 20.
               05  SORT-KOHNUM                PIC  X(03).
               05  SORT-KNYUGAI-TBL  OCCURS  2.
                   07  SORT-KKENSU             PIC  9(05).
                   07  SORT-KNISSU             PIC  9(05).
                   07  SORT-KTENSU             PIC  9(11).
                   07  SORT-KITBFTN            PIC  9(07).
               05  SORT-S-KKENSU               PIC  9(05).
               05  SORT-S-KNISSU               PIC  9(05).
               05  SORT-S-KKINGK               PIC  9(11).
               05  SORT-S-KFTNGK               PIC  9(09).
      *    老人用
       01  SORT-AREA2.
           03  SORT-KOH-TBL2   OCCURS  20.
               05  SORT-KOHNUM2                PIC  X(03).
               05  SORT-KNYUGAI-TBL2  OCCURS  2.
                   07  SORT-KKENSU2             PIC  9(05).
                   07  SORT-KNISSU2             PIC  9(05).
                   07  SORT-KTENSU2             PIC  9(11).
                   07  SORT-KITBFTN2            PIC  9(07).
               05  SORT-S-KKENSU2               PIC  9(05).
               05  SORT-S-KNISSU2               PIC  9(05).
               05  SORT-S-KKINGK2               PIC  9(11).
               05  SORT-S-KFTNGK2               PIC  9(09).
      *    一時退避用
       01  WRK-TMP-AREA.
           03  WRK-TMP-TBL.
               05  WRK-TMP-KOHNUM                PIC  X(03).
      *
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX        PIC 9(03).
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
           COPY    "CPSHELLTBL.INC".
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
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
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
           INITIALIZE                  SORT-AREA
           INITIALIZE                  SORT-AREA2
           INITIALIZE                  IDX-AREA
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
           MOVE    "SOKATU3410"    TO  JOB-PGID
           MOVE    "国保診療報酬請求書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT3410"           TO  SKT3410TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT3410TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT3410TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT3410TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT3410TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
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
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
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
           MOVE    "SOKATU3410"              TO  WRK-PRGID
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
           OPEN     OUTPUT  SKT3410TMP-FILE
           CLOSE            SKT3410TMP-FILE
           OPEN     I-O     SKT3410TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT3410TMP-FILE
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
           DISPLAY "*** SOKATU3410 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU3410 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           INITIALIZE       SKT3410TMP-REC
           INITIALIZE       SKT3410TMP-HKNJANUM
      *
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  SKT3410TMP-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  SKT3410TMP-HKNJANUM(1:6)
           END-IF
      *
           MOVE    RECE10-PREFKBN    TO    SKT3410TMP-KENKBN
      * 
      *    指定した保険者→広島市内保険者→県内市町村→県内国保組合→
      *    →県内扱い国保組合→県外保険者の順に印刷
           IF    WRK-YUSEN  =  SKT3410TMP-HKNJANUM
             MOVE          1   TO    SKT3410TMP-SYUTURYOKU
           ELSE
             IF  SKT3410TMP-HKNJANUM (1:3) = "344"
               MOVE          2   TO    SKT3410TMP-SYUTURYOKU
             ELSE
               IF  RECE10-PREFKBN          = "1"
                 IF  SKT3410TMP-HKNJANUM(3:1)  =  "3"
                   IF  RECE10-PREFNUM      = "34"
                     MOVE    4   TO    SKT3410TMP-SYUTURYOKU
                   ELSE
                     MOVE    5   TO    SKT3410TMP-SYUTURYOKU
                   END-IF
                 ELSE
                   MOVE      3   TO    SKT3410TMP-SYUTURYOKU
                 END-IF
               ELSE
                 MOVE        6   TO    SKT3410TMP-SYUTURYOKU
               END-IF
             END-IF
           END-IF
      *
      *    中国地方豪雨の対象者は別帳票
           IF  RECE10-ETC-YKZFTN (1)  NOT =  ZERO
             MOVE  RECE10-ETC-YKZFTN (1)    TO  SKT3410TMP-GENMEN
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
           ELSE
      *    退職者かどうか
             IF  RECE10-HKNNUM  =  "060"  OR  "068"
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
                       MOVE  4     TO  IDX1
                   ELSE
                     IF  RECE10-AGE  >= 3
                        MOVE    3     TO    IDX1
                     ELSE
                        MOVE    4     TO    IDX1
                     END-IF
                   END-IF
               END-IF
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
                       MOVE  9     TO  IDX1
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
           PERFORM  2101-SYUKEI-SEC
      *
      *    原爆公費（老人以外）
           IF  RECE10-RJNHKNNUM  =  SPACE
               PERFORM  2102-GENBAKU-SEC
           END-IF
      *
      *    公費番号まとめ情報があるとき
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM(IDX3)  =  SPACE
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
           END-PERFORM
      *
      *    福祉医療
           IF  RECE10-KOHNUM (1) = "141" OR "241"
                                OR "190" OR "290" OR "390"
                                OR "191" OR "291" OR "391"
                                OR "192" OR "292" OR "392"
                                OR "193" OR
               RECE10-KOHNUM (2) = "141" OR "241"
                                OR "190" OR "290" OR "390"
                                OR "191" OR "291" OR "391"
                                OR "192" OR "292" OR "392"
                                OR "193" OR
               RECE10-KOHNUM (3) = "141" OR "241" 
                                OR "190" OR "290" OR "390"
                                OR "191" OR "291" OR "391"
                                OR "192" OR "292" OR "392"
                                OR "193" OR
               RECE10-KOHNUM (4) = "141" OR "241"
                                OR "190" OR "290" OR "390"
                                OR "191" OR "291" OR "391"
                                OR "192" OR "292" OR "392"
                                OR "193"
               PERFORM 220-FKOH-SYUKEI-SEC
           END-IF
      *
      *    公費医療
           IF  RECE10-KOHNUM (1)   NOT =   SPACE  OR
               RECE10-KOHNUM (2)   NOT =   SPACE  OR
               RECE10-KOHNUM (3)   NOT =   SPACE  OR
               RECE10-KOHNUM (4)   NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
           IF  FLG-SKT3410TMP  =  1
               WRITE    SKT3410TMP-REC
           ELSE
               REWRITE  SKT3410TMP-REC
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
           ADD   1                  TO  SKT3410TMP-KENSU (IDX1 IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(1)    TO  SKT3410TMP-NISSU(IDX1 IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO  SKT3410TMP-TENSU(IDX1 IDX2)
      *
      *    療養の給付：一部負担金
      *    全国、地方問わず公費の一部負担金を再掲する
           ADD  RECE10-FTNMONEY(1)  TO  SKT3410TMP-ITBFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養:件数
             ADD   1  TO  SKT3410TMP-S-KENSU(IDX1)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(1)  TO  SKT3410TMP-S-NISSU(IDX1)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(1) TO SKT3410TMP-S-KINGK(IDX1)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(1)    TO  SKT3410TMP-S-FTNGK(IDX1)
      *
      *    老人原爆件数
           IF  IDX1  =  10  OR  11
             IF  RECE10-ETC-KOHNUM (1)  =  "019"
             OR  RECE10-ETC-KOHNUM (2)  =  "019"
             OR  RECE10-ETC-KOHNUM (3)  =  "019"
             OR  RECE10-ETC-KOHNUM (4)  =  "019"
               ADD  1           TO  SKT3410TMP-GENBAKU(IDX1 IDX2)
             END-IF
             IF  RECE10-KOHNUM (1)  =  "019"
             OR  RECE10-KOHNUM (2)  =  "019"
             OR  RECE10-KOHNUM (3)  =  "019"
             OR  RECE10-KOHNUM (4)  =  "019"
               ADD  1           TO  SKT3410TMP-GENBAKU(IDX1 IDX2)
             END-IF
           END-IF
           MOVE    1               TO  FLG-UPD 
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    原爆集計処理
      *****************************************************************
       2102-GENBAKU-SEC         SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
             IF  RECE10-KOHNUM (IDX3)  =  "019"
      *    療養の給付：件数
               ADD   1                  TO  SKT3410TMP-GKENSU(IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU(IDX3 + 1)
                                        TO  SKT3410TMP-GNISSU(IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)
                                        TO  SKT3410TMP-GTENSU(IDX2)
      *
               ADD  RECE10-FTNMONEY(IDX3 + 1)
                                        TO  SKT3410TMP-GITBFTN(IDX2)
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO  SKT3410TMP-S-GKENSU
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(IDX3 + 1)
                                        TO  SKT3410TMP-S-GNISSU
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)
                                        TO SKT3410TMP-S-GKINGK
      *    食事療養：標準負担額
               ADD  RECE10-SHOKUJIFTN(IDX3 + 1)
                                        TO  SKT3410TMP-S-GFTNGK
             END-IF
           END-PERFORM
      *
           .
       2102-GENBAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般・退職 ＋ 公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      * ５１、９１は１つにまとめる
            IF    RECE10-KOHNUM(IDX3)  =  "091"
              MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
            END-IF
      *    全国共通公費のみ対象
             IF  RECE10-KOHNUM(IDX3)(1:1) = "0"
      *    公費判別
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >     20
                        OR       SKT3410TMP-KOHNUM (IDX4) = SPACE
                 IF  RECE10-KOHNUM (IDX3)  =  SKT3410TMP-KOHNUM (IDX4)
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
      *    原爆は別集計
               IF  RECE10-KOHNUM(IDX3)  =  "019"
                 CONTINUE
               ELSE
      *    公費名
                 MOVE  RECE10-KOHNUM(IDX3)
                                   TO  SKT3410TMP-KOHNUM(CNT-KOH)
      *    療養の給付：件数
                 ADD   1  TO  SKT3410TMP-KKENSU (CNT-KOH IDX2)
      *    療養の給付:診療実日数
                 ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                   SKT3410TMP-KNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
                 ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                   SKT3410TMP-KTENSU(CNT-KOH IDX2)
      *    療養の給付：一部負担金
                 ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                   SKT3410TMP-KITBFTN(CNT-KOH IDX2)
      *
                 IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                   ADD   1  TO  SKT3410TMP-S-KKENSU(CNT-KOH)
                 END-IF
      *    食事療養:日数
                 ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                                 SKT3410TMP-S-KNISSU(CNT-KOH)
      *    食事療養：金額
                 ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                                 SKT3410TMP-S-KKINGK(CNT-KOH)
      *    食事療養：標準負担額
                 ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                                 SKT3410TMP-S-KFTNGK(CNT-KOH)
               END-IF
             END-IF
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    福祉集計処理
      *****************************************************************
       220-FKOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
             EVALUATE  RECE10-KOHNUM (IDX3)
             WHEN "141"
             WHEN "241"
               ADD    1    TO    SKT3410TMP-FKENSU (1 IDX2)
               ADD    RECE10-FTNMONEY(IDX3 + 1)
                           TO    SKT3410TMP-FITBFTN(1 IDX2)
             WHEN "190"
             WHEN "290"
             WHEN "390"
               ADD    1    TO    SKT3410TMP-FKENSU(2 IDX2)
               ADD    RECE10-FTNMONEY(IDX3 + 1)
                           TO    SKT3410TMP-FITBFTN(2 IDX2)
             WHEN "191"
             WHEN "291"
             WHEN "391"
               ADD    1    TO    SKT3410TMP-FKENSU(3 IDX2)
               ADD    RECE10-FTNMONEY(IDX3 + 1)
                           TO    SKT3410TMP-FITBFTN(3 IDX2)
             WHEN "192"
             WHEN "292"
             WHEN "392"
               ADD    1    TO    SKT3410TMP-FKENSU(4 IDX2)
               ADD    RECE10-FTNMONEY(IDX3 + 1)
                           TO    SKT3410TMP-FITBFTN(4 IDX2)
             WHEN "193"
               ADD    1    TO    SKT3410TMP-FKENSU(5 IDX2)
               ADD    RECE10-FTNMONEY(IDX3 + 1)
                           TO    SKT3410TMP-FITBFTN(5 IDX2)
             END-EVALUATE
           END-PERFORM
      *
      *
           .
       220-FKOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    一般・退職 ＋ 原爆・結核公費
      *****************************************************************
       2201-GEN-KESE-SEC        SECTION.
      *
           EVALUATE  RECE10-KOHNUM(IDX3)
      *    原爆
           WHEN "019"
      *    療養の給付：件数
               ADD   1  TO  SKT3410TMP-GKKENSU(IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                 SKT3410TMP-GKNISSU(IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                 SKT3410TMP-GKTENSU(IDX2)
      *    療養の給付：一部負担金
               ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                 SKT3410TMP-GKITBFTN(IDX2)
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO  SKT3410TMP-S-GKKENSU
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT3410TMP-S-GKNISSU
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT3410TMP-S-GKKINGK
      *    食事療養：標準負担額
               ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                               SKT3410TMP-S-GKFTNGK
      *    結核３４条
           WHEN "010"
      *    療養の給付：件数
               ADD   1  TO  SKT3410TMP-34KENSU(IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                 SKT3410TMP-34NISSU(IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                 SKT3410TMP-34TENSU(IDX2)
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO  SKT3410TMP-S-34KENSU
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT3410TMP-S-34NISSU
      *    結核３５条
           WHEN "011"
      *
      *    療養の給付：件数
               ADD   1  TO  SKT3410TMP-35KENSU
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                 SKT3410TMP-35NISSU
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                 SKT3410TMP-35TENSU
      *    療養の給付：一部負担金
               ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                 SKT3410TMP-35ITBFTN
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO  SKT3410TMP-S-35KENSU
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT3410TMP-S-35NISSU
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT3410TMP-S-35KINGK
           END-EVALUATE
      *
           .
       2201-GEN-KESE-EXT.
           EXIT.
057400*
      *****************************************************************
      *    老人 ＋ 公費負担分集計処理
      *****************************************************************
       220-RKOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *    ５１、９１は１つにまとめる
            IF    RECE10-KOHNUM(IDX3)  =  "091"
              MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
            END-IF
      *
      *    全国共通公費のみ対象
             IF  RECE10-KOHNUM(IDX3)(1:1) = "0"
      *    公費判別
               DISPLAY "老人公費番号=" RECE10-KOHNUM (IDX3)
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >     20
                        OR       SKT3410TMP-RKOHNUM (IDX4) = SPACE
                 IF  RECE10-KOHNUM (IDX3)  =  SKT3410TMP-RKOHNUM (IDX4)
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
                 MOVE  RECE10-KOHNUM(IDX3)  
                                  TO  SKT3410TMP-RKOHNUM(CNT-KOH)
      *    療養の給付：件数
                 ADD   1  TO  SKT3410TMP-RKKENSU (CNT-KOH IDX2)
      *    療養の給付:診療実日数
                 ADD  RECE10-JNISSU(IDX3 + 1)  TO
                                 SKT3410TMP-RKNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
                 ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                                 SKT3410TMP-RKTENSU(CNT-KOH IDX2)
      *    療養の給付：一部負担金
                 ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                                 SKT3410TMP-RKITBFTN(CNT-KOH IDX2)
      *
                 IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                   ADD   1  TO  SKT3410TMP-S-RKKENSU(CNT-KOH)
                 END-IF
      *    食事療養:日数
                 ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT3410TMP-S-RKNISSU(CNT-KOH)
      *    食事療養：金額
                 ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT3410TMP-S-RKKINGK(CNT-KOH)
      *    食事療養：標準負担額
                 ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                               SKT3410TMP-S-RKFTNGK(CNT-KOH)
             END-IF
           END-PERFORM
      *
      *
           .
       220-RKOH-SYUKEI-EXT.
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
           OPEN     INPUT       SKT3410TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     1           TO       WRK-CNT
                                         WRK-CNT2
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    １枚目帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *    １枚目帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *    １枚目帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *    公費ソート処理
               PERFORM KOH-SORT-SEC
      *    公費が残っているか
               PERFORM  UNTIL  SKT3410TMP-KOHNUM (WRK-CNT)  =  SPACE
                        AND    FLG-PASS  =  1
      *    ２枚目帳票編集<見出し>処理
                 PERFORM 311-HEAD-HEN-SEC
      *    ２枚目帳票編集<明細>処理
                 PERFORM 321-BODY-HEN-SEC
      *    ２枚目帳票印刷処理
                 PERFORM 331-PRINT-OUT-SEC
                 MOVE  1  TO  FLG-PASS
               END-PERFORM
               MOVE  ZERO  TO  FLG-PASS
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
               MOVE  1  TO  WRK-CNT
                            WRK-CNT2
           END-PERFORM
      *
           CLOSE    SKT3410TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    １枚目帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT3410
      *
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3410-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT3410-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT3410-SEIYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3410-ADRS
           INSPECT SKT3410-ADRS          REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3410-HOSPNAME
           INSPECT SKT3410-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT3410-KAISETUNAME
      *
      *    保険者名
           PERFORM 920-HKNJAINF-SET-SEC
           MOVE    HKNJA-HKNJANAME       TO  SKT3410-HKNJANAME
           MOVE    SKT3410TMP-HKNJANUM   TO  SKT3410-HKNJANUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT3410-HOSPCD
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT3410-TOKUBETU
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １枚目帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM    1   BY  1
                    UNTIL    IDX1  >  9
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT3410TMP-KENSU(IDX1 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3410-KENSU(IDX1 IDX2)
      *    療養の給付：日数
               MOVE  SKT3410TMP-NISSU(IDX1 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3410-NISSU(IDX1 IDX2)
      *    療養の給付：点数
               MOVE  SKT3410TMP-TENSU(IDX1 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3410-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3410TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3410-ITBFTN(IDX1 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3410TMP-S-KENSU(IDX1)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3410-S-KENSU(IDX1)
      *    食事療養：日数
             MOVE  SKT3410TMP-S-NISSU(IDX1)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3410-S-NISSU(IDX1)
      *    食事療養：金額
             MOVE  SKT3410TMP-S-KINGK(IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3410-S-KINGK(IDX1)
      *    食事療養：標準負担額
             MOVE  SKT3410TMP-S-FTNGK(IDX1)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3410-S-FTNGK(IDX1)
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １枚目帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "１枚目"
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
           MOVE   "SKT3410.red"        TO  SPRT-PRTID
      *
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE    "国保請求書"    TO  SPRT-TITLE
             WHEN  "2"
               MOVE    "国保請求書［返戻分］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3410             TO  SPRT-PRTDATA
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
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
      *
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
                                 IDX2 
           MOVE    SPACE   TO    WRK-KOHNUM
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT3410TMP-KOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM               =   SPACE   OR
                     SKT3410TMP-KOHNUM(IDX1)  <   WRK-KOHNUM
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT3410TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT3410TMP-KOH-AREA(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "一般ソート前１" SKT3410TMP-KOHNUM(1)
           DISPLAY "一般ソート前２" SKT3410TMP-KOHNUM(2)
           DISPLAY "一般ソート前３" SKT3410TMP-KOHNUM(3)
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   "999"   TO    WRK-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT3410TMP-KOHNUM(IDX2)  >   SORT-KOHNUM(IDX1 - 1)
                   IF  SKT3410TMP-KOHNUM(IDX2)  <  WRK-KOHNUM
                     MOVE  SKT3410TMP-KOHNUM(IDX2)  TO WRK-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
               END-PERFORM
               MOVE  SKT3410TMP-KOH-AREA(IDX3)  TO  SORT-KOH-TBL(IDX1)
               MOVE   "999"   TO    WRK-KOHNUM
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT3410TMP-KOH-TBL
           MOVE  SORT-AREA  TO    SKT3410TMP-KOH-TBL
      *
           DISPLAY "一般ソート後１" SKT3410TMP-KOHNUM(1)
           DISPLAY "一般ソート後２" SKT3410TMP-KOHNUM(2)
           DISPLAY "一般ソート後３" SKT3410TMP-KOHNUM(3)
      *
      *
           .
       KOH-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人併用公費ソート処理
      *****************************************************************
       RKOH-SORT-SEC          SECTION.
      *
      *
           INITIALIZE  SORT-AREA
           INITIALIZE  SORT-AREA2
           MOVE    ZERO    TO  WRK-TMP-KOHNUM
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT3410TMP-RKOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               ADD   1     TO    IDX-MAX
               IF    WRK-TMP-KOHNUM  <  SKT3410TMP-RKOHNUM(IDX1)
                 MOVE  SKT3410TMP-RKOHNUM(IDX1)  TO  WRK-TMP-KOHNUM
                 MOVE  IDX1    TO    IDX5
               END-IF
             END-IF
           END-PERFORM
      *
           DISPLAY "老人ソート前１" SKT3410TMP-RKOHNUM(1)
           DISPLAY "老人ソート前２" SKT3410TMP-RKOHNUM(2)
           DISPLAY "老人ソート前３" SKT3410TMP-RKOHNUM(3)
      *
           MOVE  IDX-MAX     TO     IDX-MAX-RJNKOH
      *
           IF      IDX-MAX    >     ZERO
             MOVE    SKT3410TMP-RKOH-AREA(IDX5)  TO
                                   SORT-KOH-TBL2(1)
             MOVE    2    TO    IDX1
             DISPLAY "OK1"
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   WRK-TMP-KOHNUM  TO  SORT-KOHNUM2(1)
             MOVE   ZERO    TO    WRK-TMP-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  UNTIL    IDX2  >    IDX-MAX
                 IF  SKT3410TMP-RKOHNUM(IDX2) < SORT-KOHNUM2(IDX1 - 1)
                   IF  SKT3410TMP-RKOHNUM(IDX2)  >  WRK-TMP-KOHNUM
                     MOVE  SKT3410TMP-RKOHNUM(IDX2)  TO WRK-TMP-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
                 ADD    1    TO    IDX2
               END-PERFORM
               MOVE  SKT3410TMP-RKOH-AREA(IDX3) TO SORT-KOH-TBL2(IDX1)
               MOVE  ZERO   TO    WRK-TMP-KOHNUM
               ADD   1    TO    IDX1
               MOVE  1    TO    IDX2
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT3410TMP-RKOH-TBL
           MOVE  SORT-AREA2  TO    SKT3410TMP-RKOH-TBL
      *
           DISPLAY "老人ソート後１" SKT3410TMP-RKOHNUM(1)
           DISPLAY "老人ソート後２" SKT3410TMP-RKOHNUM(2)
           DISPLAY "老人ソート後３" SKT3410TMP-RKOHNUM(3)
      *
           .
       RKOH-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２枚目帳票編集<ヘッダー部>処理
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT3420
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3420-NO
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT3420-HOSPCD
      *
      *    保険者番号
           MOVE    SKT3410TMP-HKNJANUM   TO  SKT3420-HKNJANUM
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT3420-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２枚目帳票編集<明細行>処理
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    公費のための２枚目以上か
           IF  WRK-CNT  =  1
      *      老人保健分
      *      編集処理
             PERFORM  VARYING  IDX1  FROM    10   BY  1
                      UNTIL    IDX1  >  11
               COMPUTE  IDX3 = IDX1 - 9
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT3410TMP-KENSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3420-KENSU(IDX3 IDX2)
                 DISPLAY "老人件数=" SKT3420-KENSU(IDX3 IDX2)
      *    療養の給付：日数
                 MOVE  SKT3410TMP-NISSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3420-NISSU(IDX3 IDX2)
      *    療養の給付：点数
                 MOVE  SKT3410TMP-TENSU(IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3420-TENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT3410TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3420-ITBFTN(IDX3 IDX2)
      *    療養の給付：原爆件数
                 MOVE  SKT3410TMP-GENBAKU (IDX1 IDX2)  TO  WRK-Z3
                 MOVE  WRK-Z3   TO  SKT3420-GENBAKU(IDX3 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT3410TMP-S-KENSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3420-S-KENSU(IDX3)
      *    食事療養：日数
               MOVE  SKT3410TMP-S-NISSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3420-S-NISSU(IDX3)
      *    食事療養：金額
               MOVE  SKT3410TMP-S-KINGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3420-S-KINGK(IDX3)
      *    食事療養：標準負担額
               MOVE  SKT3410TMP-S-FTNGK(IDX1)  TO  WRK-Z7
               MOVE  WRK-Z7   TO  SKT3420-S-FTNGK(IDX3)
               MOVE  SKT3410TMP-S-FTNGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3420-S-FTNGK(IDX3)
             END-PERFORM
      *    原爆分
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT3410TMP-GKENSU(IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3420-GKENSU(IDX2)
      *    療養の給付：日数
               MOVE  SKT3410TMP-GNISSU(IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3420-GNISSU(IDX2)
      *    療養の給付：点数
               MOVE  SKT3410TMP-GTENSU(IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3420-GTENSU(IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3410TMP-GITBFTN(IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3420-GITBFTN(IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3410TMP-S-GKENSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3420-S-GKENSU
      *    食事療養：日数
             MOVE  SKT3410TMP-S-GNISSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3420-S-GNISSU
      *    食事療養：金額
             MOVE  SKT3410TMP-S-GKINGK  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3420-S-GKINGK
      *    食事療養：標準負担額
             MOVE  SKT3410TMP-S-GFTNGK  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3420-S-GFTNGK
      *
      *    地方公費件数再掲
             PERFORM  VARYING  IDX1  FROM    1   BY  1
                      UNTIL    IDX1  >  5
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT3410TMP-FKENSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3420-FKENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT3410TMP-FITBFTN(IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3420-FITBFTN(IDX1 IDX2)
               END-PERFORM
             END-PERFORM
           END-IF
      *
      *    一般・退職公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     3
                    OR  WRK-CNT    >     20
                    OR  SKT3410TMP-KOHNUM(WRK-CNT) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >  2
      *
      *    療養の給付：件数
               MOVE  SKT3410TMP-KKENSU (WRK-CNT IDX2) TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3420-KKENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT3410TMP-KNISSU (WRK-CNT IDX2) TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3420-KNISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT3410TMP-KTENSU (WRK-CNT IDX2) TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3420-KTENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3410TMP-KITBFTN(WRK-CNT IDX2) TO WRK-Z9
               MOVE  WRK-Z9   TO  SKT3420-KITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3410TMP-S-KKENSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3420-S-KKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT3410TMP-S-KNISSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3420-S-KNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT3410TMP-S-KKINGK(WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3420-S-KKINGK(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT3410TMP-S-KFTNGK (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3420-S-KFTNGK(IDX3)
      *    公費名
             MOVE  SKT3410TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-TANSEIDONAME   TO  SKT3420-KNAME(IDX3)
             MOVE  HKN-HBTNUM         TO  SKT3420-HBTNUM(IDX3)
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２枚目帳票印刷処理
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           MOVE   "SKT3420.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE    "国保請求書"    TO  SPRT-TITLE
             WHEN  "2"
               MOVE    "国保請求書［返戻分］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3420             TO  SPRT-PRTDATA
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
      *    ３枚目帳票編集<ヘッダー部>処理（老人・公費等）
      *****************************************************************
       312-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT3430
           INITIALIZE                    IDX4
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3430-NO
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT3430-HOSPCD
      *
      *    保険者番号
           MOVE    SKT3410TMP-HKNJANUM   TO  SKT3430-HKNJANUM
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT3430-TOKUBETU
           END-IF
      *
           .
       312-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（公費等）
      *****************************************************************
       322-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  FLG-PASS  =  ZERO
      *
      *
      *    結核３４条分
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT3410TMP-34KENSU(IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3430-34KENSU(IDX2)
      *    療養の給付：日数
               MOVE  SKT3410TMP-34NISSU(IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3430-34NISSU(IDX2)
      *    療養の給付：点数
               MOVE  SKT3410TMP-34TENSU(IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3430-34TENSU(IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3410TMP-S-34KENSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-S-34KENSU
      *    食事療養：日数
             MOVE  SKT3410TMP-S-34NISSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-S-34NISSU
      *
      *
      *    結核３５条分
      *    療養の給付：件数
             MOVE  SKT3410TMP-35KENSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-35KENSU
      *    療養の給付：日数
             MOVE  SKT3410TMP-35NISSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-35NISSU
      *    療養の給付：点数
             MOVE  SKT3410TMP-35TENSU  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3430-35TENSU
      *    療養の給付：一部負担金
             MOVE  SKT3410TMP-35ITBFTN  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3430-35ITBFTN
      *    食事療養：件数
             MOVE  SKT3410TMP-S-35KENSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-S-35KENSU
      *    食事療養：日数
             MOVE  SKT3410TMP-S-35NISSU  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-S-35NISSU
      *    食事療養：金額
             MOVE  SKT3410TMP-S-35KINGK  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3430-S-35KINGK
           END-IF
      *
      *    一般・退職公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     6
                    OR  WRK-CNT    >     20
                    OR  SKT3410TMP-KOHNUM(WRK-CNT) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >  2
      *
      *    療養の給付：件数
               MOVE  SKT3410TMP-KKENSU (WRK-CNT IDX2) TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3430-KKENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT3410TMP-KNISSU (WRK-CNT IDX2) TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3430-KNISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT3410TMP-KTENSU (WRK-CNT IDX2) TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3430-KTENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3410TMP-KITBFTN(WRK-CNT IDX2) TO WRK-Z9
               MOVE  WRK-Z9   TO  SKT3430-KITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3410TMP-S-KKENSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-S-KKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT3410TMP-S-KNISSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3430-S-KNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT3410TMP-S-KKINGK(WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3430-S-KKINGK(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT3410TMP-S-KFTNGK (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3430-S-KFTNGK(IDX3)
      *    公費名
             MOVE  SKT3410TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-TANSEIDONAME   TO  SKT3430-KNAME(IDX3)
             MOVE  HKN-HBTNUM         TO  SKT3430-HBTNUM(IDX3)
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           MOVE    IDX3    TO    IDX4
      *
      *    一般・退職公費を全部印字してから老人公費分を印字する
           IF  SKT3410TMP-KOHNUM(WRK-CNT) = SPACE
             PERFORM  UNTIL    IDX4  >     6
                      OR  WRK-CNT2    >     20
                      OR  SKT3410TMP-RKOHNUM(WRK-CNT2) = SPACE
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >  2
      *    療養の給付：件数
                 MOVE  SKT3410TMP-RKKENSU(WRK-CNT2 IDX2) TO WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3430-KKENSU(IDX4 IDX2)
      *    療養の給付：日数
                 MOVE  SKT3410TMP-RKNISSU(WRK-CNT2 IDX2) TO WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3430-KNISSU(IDX4 IDX2)
      *    療養の給付：点数
                 MOVE  SKT3410TMP-RKTENSU(WRK-CNT2 IDX2) TO WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3430-KTENSU(IDX4 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT3410TMP-RKITBFTN(WRK-CNT2 IDX2) TO WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3430-KITBFTN(IDX4 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT3410TMP-S-RKKENSU (WRK-CNT2)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3430-S-KKENSU(IDX4)
      *    食事療養：日数
               MOVE  SKT3410TMP-S-RKNISSU (WRK-CNT2)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3430-S-KNISSU(IDX4)
      *    食事療養：金額
               MOVE  SKT3410TMP-S-RKKINGK (WRK-CNT2)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3430-S-KKINGK(IDX4)
      *    食事療養：標準負担額
               MOVE  SKT3410TMP-S-RKFTNGK (WRK-CNT2)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3430-S-KFTNGK(IDX4)
      *    公費名
               MOVE  SKT3410TMP-RKOHNUM (WRK-CNT2)  TO  WRK-KOHNUM
               PERFORM  950-HKNNUM-SET-SEC
               MOVE  HKN-TANSEIDONAME   TO  SKT3430-KNAME(IDX4)
               MOVE  HKN-HBTNUM          TO  SKT3430-RHBTNUM1(IDX4)
               MOVE  "27"                TO  SKT3430-RHBTNUM2(IDX4)
               MOVE  "−"                TO  SKT3430-RSEN(IDX4)
               ADD  1  TO  WRK-CNT2
               ADD  1  TO  IDX4
             END-PERFORM
           END-IF
      *
           .
       322-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ３枚目帳票印刷処理（公費）
      *****************************************************************
       332-PRINT-OUT-SEC                SECTION.
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
           MOVE   "SKT3430.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE    "国保請求書"    TO  SPRT-TITLE
             WHEN  "2"
               MOVE    "国保請求書［返戻分］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3430             TO  SPRT-PRTDATA
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
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
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
                       MOVE  "*** SOKATU3410 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU3410 SEIKYU INVALID ***"
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
           MOVE    SPA-HOSPNUM             TO  HKNJA-HOSPNUM
           MOVE    SKT3410TMP-HKNJANUM     TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknjainf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 925-HKNJAINF-READ-SEC
           ELSE
               DISPLAY "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC  =   ZERO
               MOVE    MCPDATA-REC  TO  HKNJAINF-REC
               MOVE    ZERO         TO  FLG-HKNJAINF
           ELSE
               INITIALIZE  HKNJAINF-REC
               MOVE    1            TO  FLG-HKNJAINF
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
           MOVE    SPA-HOSPNUM                 TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM                  TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)   TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                      TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD              TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC                  TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC  =   ZERO
               MOVE    "tbl_hknnum"         TO  MCP-TABLE
               MOVE    "key5"               TO  MCP-PATHNAME
               PERFORM 955-HKNNUM-READ-SEC
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
      *    保険者情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
               IF      HKN-HKNNUM      =   "051"
                   INITIALIZE              HKN-TANSEIDONAME
                   MOVE    "特定疾患"  TO  HKN-TANSEIDONAME
               END-IF
           ELSE
               INITIALIZE  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
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
           MOVE   SPA-HOSPNUM              TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM       TO  RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM     NOT =   ZERO
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
                                               RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  2
             MOVE    000000            TO  RECE10-SRYYM
             MOVE    999999            TO  RECE10-XXSRYYM
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
               MOVE    ZERO            TO  FLG-END
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
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
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
           READ    SKT3410TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT3410TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT3410TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
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
      *
           MOVE    "DBSELECT"                     TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                      TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"                TO  MCP-FUNC
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
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC                  =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT3410TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
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
       900-DBDISCONNECT-EXT.
           EXIT.
