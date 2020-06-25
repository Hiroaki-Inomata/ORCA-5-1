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
       PROGRAM-ID.             SOKATU3310.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 岡山・診療報酬請求書(国保請求書)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/08/12    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev　　修正者	　日付		内容
      *  01.00.01    内田　　06/09/17  備考欄の修正
      *  01.00.02    内田    06/11/21  帳票様式の変更に伴う修正
      *                                食事がない場合でも生活療養費を印字
      *  01.00.03    内田    06/12/12  公費再掲欄で地方公費は法別番号毎にする
      *  01.00.04    内田    07/04/09  返戻分はその時の診療年月を印字
      *  01.00.05    内田    07/04/21  公費再掲欄の印字対象番号追加（386等）
      *  01.00.06    内田    07/09/11  老人−在宅の点数負担金欄のバイト数増加
      *                                日レセver4.0対応
      *                                システム管理マスタ「1901」の
      *                                医療機関情報を参照
      *  01.00.07    内田    08/02/23  返戻時の診療年月の修正
      *  01.00.08    内田    08/05/01  帳票様式の変更に伴う修正
      *                                SPA-AREA初期化
      *  01.00.09    門間    08/11/20  シス管「2007」公費まとめ対応
      *  01.00.10    内田    09/09/29  平成21年台風第9号対応（対象者別集計）
      *  01.00.11    吉川    09/12/25  シス管「2007」のまとめ公費で
      *                                6つ目以降がまとまらない不具合を修正
      *  01.00.12    吉川    10/04/07  オンライン返戻対応
      *  01.00.13    吉川    10/12/09  特別療養費国保対応
      *                                open-cobol1.0対応
      *  01.00.14    吉川    12/06/13  帳票様式の変更に伴う修正
      *  01.00.15    吉川    13/02/06  プラグイン対応
      *                                医療機関情報編集対応
      *  01.00.16    吉川    13/09/19  プログラムオプション対応
      *  01.00.17    吉川    14/05/13  前期高齢者２割対応
      *  01.00.18    吉川    14/10/24  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.19    門間    16/02/04  医療機関編集情報への開設者名追加対応
      *  01.00.20    門間    16/08/08  返戻分は直近の診療年月でまとめる
      *  01.00.21    萩野    17/11/08  公費番号まとめ情報の公費番号の追加
      *                                医療機関コード変更時、
      *                                医療機関コード別の作成処理対応
      *                                SITEKBN変更（動作に変更なし）
      *  01.00.22    井上    19/01/09  修正履歴コメント削除
      *                                保険番号情報の取得処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT3310TMP-FILE ASSIGN    SKT3310TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT3310TMP-KEY
                                   FILE    STATUS  IS  STS-SKT3310.
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
       FD  SKT3310TMP-FILE.
           COPY    "CPSKT3310TMP.INC".
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
                                   BY         //SKT3310TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
           COPY    "SKT3310.INC".
           COPY    "SKT3320.INC".
           COPY    "SKT3330.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT3310         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT3310TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-BIKO            PIC 9(01).
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
           03  IDX-MAX             PIC 9(04).
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
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-KOHNUM-R        PIC X(04).
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
           03  WRK-Z79             PIC ZZZ,ZZ9.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-MAE-INPUT                  PIC X(200).
           03  WRK-OUT-INPUT                  PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
           03  WRK-PATH3             PIC X(50).
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
           03  WRK-BIKO            PIC 9(09).
           03  WRK-BIKO2           PIC 9(09).
      *
           03  WRK-HEN-SRYYM         PIC  9(06).
      *
       01  SORT-AREA.
           03  SORT-KOH-TBL OCCURS 20.
               05  SORT-KOHNUM                 PIC  X(03).
               05  SORT-KOHNAME                PIC  X(10).
               05  SORT-KNYUGAI-TBL  OCCURS  2.
                   07  SORT-KKENSU             PIC  9(05).
                   07  SORT-KNISSU             PIC  9(05).
                   07  SORT-KTENSU             PIC  9(09).
                   07  SORT-KYKZFTN            PIC  9(07).
                   07  SORT-KITBFTN            PIC  9(07).
               05  SORT-K-SKENSU               PIC  9(05).
               05  SORT-K-SNISSU               PIC  9(05).
               05  SORT-K-SKINGK               PIC  9(09).
               05  SORT-K-SFTNGK               PIC  9(09).
       01  SORT-AREA2.
           03  SORT-BKOH-TBL OCCURS 20.
               05  SORT-BKOHNUM                PIC  X(03).
               05  SORT-BKOHNAME               PIC  X(10).
               05  SORT-BNYUGAIKBN             PIC  X(01).
               05  SORT-BKENSU                 PIC  9(05).
               05  SORT-BITBFTN                PIC  9(09).
               05  SORT-BFTNGK                 PIC  9(09).
           03  SORT-BKOH-TBL-R.
               05  SORT-BKOH-TBL-RR  OCCURS  20.
                   07  SORT-BKOHNUM-R          PIC  X(04).
                   07  SORT-BKOHNAME-R         PIC  X(10).
                   07  SORT-BKENSU-R           PIC  9(05).
                   07  SORT-BITBFTN-R          PIC  9(09).
                   07  SORT-BFTNGK-R           PIC  9(09).
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
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
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
      *    請求管理情報
           COPY    "CPRCF010.INC".
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
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    パラメタ設定
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
           MOVE    "SOKATU3310"    TO  JOB-PGID
           MOVE    "国保請求書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT3310"           TO  SKT3310TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT3310TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT3310TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT3310TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT3310TP-FULLNAME
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
           OPEN    OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH3
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH3
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH3
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH3
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
           MOVE    "SOKATU3310"              TO  WRK-PRGID
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
           OPEN    OUTPUT  SKT3310TMP-FILE
           CLOSE           SKT3310TMP-FILE
           OPEN    I-O     SKT3310TMP-FILE
      *
           MOVE    ZERO    TO  FLG-END
           MOVE    ZERO    TO  FLG-UPD
      *
           PERFORM 2001-KOKUHO-SYUKEI-SEC
                   UNTIL   FLG-END     =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT3310TMP-FILE
      *
           IF      FLG-UPD     =   1
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
           DISPLAY "*** SOKATU3310 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU3310 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
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
      *    TMP-FILE キーセット
           INITIALIZE       SKT3310TMP-REC
      *
      *    保険者番号
           MOVE    RECE10-HKNJANUM-S   TO  SKT3310TMP-HKNJANUM
      *
      *    出力順設定
           IF      WRK-YUSEN       =   SKT3310TMP-HKNJANUM
               MOVE    1       TO  SKT3310TMP-SYUTURYOKU
           ELSE
               MOVE    2       TO  SKT3310TMP-SYUTURYOKU
           END-IF
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *    返戻分は実際の診療年月を設定
      *=== 返戻分は直近の診療年月でまとめる by Monma(2016/08/08) start
      *      IF  WRK-PARA-SYOKBN  =  "2"
      *        MOVE    RECE10-SRYYM          TO   SKT3310TMP-SRYYM
      *    当月分・月遅れ分は画面入力された診療年月を設定
      *      ELSE
      *---(01.00.08) LINE ADD START ------------------------------------
      *        MOVE    LNK-PRTKANRI-SRYYM    TO   SKT3310TMP-SRYYM
      *    平成２０年３月以前は旧様式
                MOVE  LNK-PRTKANRI-SRYYM    TO  SKT3310TMP-SRYYM
      *
           IF  (      WRK-PARA-SYOKBN  =  "2"
                 AND  RECE10-SRYYM     >  WRK-HEN-SRYYM  )
               MOVE    RECE10-SRYYM  TO  WRK-HEN-SRYYM
           END-IF
      *
      *    平成21年台風第9号の対象者は別帳票
           IF  RECE10-ETC-YKZFTN (1)  NOT =  ZERO
             MOVE  RECE10-ETC-YKZFTN (1)    TO  SKT3310TMP-GENMEN
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
           IF      RECE10-NYUGAIKBN    =   "1"
               MOVE    1       TO  IDX2
           ELSE
               MOVE    2       TO  IDX2
           END-IF
      *
           IF      SKT3310TMP-KOKUKUMI-KBN     =   "1"
               PERFORM 2101-KUMIAI-SET-SEC
           ELSE
               PERFORM 2101-IPPAN-SET-SEC
           END-IF
      *
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
           IF      FLG-SKT3310TMP  =   1
               WRITE   SKT3310TMP-REC
           ELSE
               REWRITE SKT3310TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保組合集計場所設定処理
      *****************************************************************
       2101-KUMIAI-SET-SEC         SECTION.
      *
      *    老人保健か
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               IF      RECE10-KYURATE  =   9
                   MOVE    8       TO   IDX1
               ELSE
                   MOVE    9       TO   IDX1
               END-IF
           ELSE
      *        組合員か、その他か
               IF      RECE10-HONKZKKBN    =   "1"
                   IF      RECE10-AGE      >=  70
      
                       IF      RECE10-KYURATE  =   9
                           MOVE    1       TO  IDX1
                       ELSE
                           MOVE    2       TO  IDX1
                       END-IF
                   ELSE
                       MOVE    3       TO  IDX1
                   END-IF
               ELSE
                   IF      RECE10-AGE  >=  70
                       IF      RECE10-KYURATE  =   9
                           MOVE    4       TO  IDX1
                       ELSE
                           MOVE    5       TO  IDX1
                       END-IF
                   ELSE
                     IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                     AND  (RECE10-FLG (1)   =  1)
                         MOVE  7       TO  IDX1
                     ELSE
                       IF  RECE10-AGE  >=  3
                           MOVE    6   TO  IDX1
                       ELSE
                           MOVE    7   TO  IDX1
                       END-IF
                     END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    在宅判定
           IF      RECE10-ZAITAKU  =   "1"
               EVALUATE    IDX1
                   WHEN    1
                   WHEN    2
                       DISPLAY "在宅・集計場所そのまま"
                   WHEN    4
                       MOVE    3     TO   IDX1
                   WHEN    5
                       MOVE    4     TO   IDX1
                   WHEN    8
                       MOVE    5     TO   IDX1
                   WHEN    OTHER
                       MOVE    6     TO   IDX1
               END-EVALUATE
               PERFORM 2101-ZAI-SYUKEI-SEC
           ELSE
               PERFORM 2101-SYUKEI-SEC
           END-IF
      *
           .
       2101-KUMIAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般保険者集計場所設定処理
      *****************************************************************
       2101-IPPAN-SET-SEC         SECTION.
      *
      *    老人保健か
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               IF      RECE10-KYURATE  =  9
                   MOVE    10      TO  IDX1
               ELSE
                   MOVE    11      TO  IDX1
               END-IF
           ELSE
      *        一般国保
               IF      RECE10-HKNNUM   =   "060" OR "068"
                   IF      RECE10-AGE  >=  70
                     EVALUATE  RECE10-KYURATE
                       WHEN  9
                         MOVE    1         TO  IDX1
                       WHEN  8
                         IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                             MOVE    1         TO  IDX1
                         ELSE
                             MOVE    2         TO  IDX1
                         END-IF
                       WHEN  7
                         MOVE    2         TO  IDX1
                     END-EVALUATE
                   ELSE
                     IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                     AND  (RECE10-FLG (1)   =  1)
                         MOVE      4       TO  IDX1
                     ELSE
                       IF      RECE10-AGE  >=  3
                           MOVE    3       TO  IDX1
                       ELSE
                           MOVE    4       TO  IDX1
                       END-IF
                     END-IF
                   END-IF
      *        退職国保
               ELSE
                   IF      RECE10-AGE  >=  70
                       IF      RECE10-KYURATE  =   9
                           MOVE    6       TO  IDX1
                       ELSE
                           MOVE    7       TO  IDX1
                       END-IF
                   ELSE
                       IF      RECE10-HONKZKKBN    =   "1"
                           MOVE    5       TO  IDX1
                       ELSE
                         IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                         AND  (RECE10-FLG (1)   =  1)
                             MOVE      9       TO  IDX1
                         ELSE
                           IF      RECE10-AGE  <   3
                               MOVE    9       TO  IDX1
                           ELSE
                               MOVE    8       TO  IDX1
                           END-IF
                         END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
               PERFORM 2101-SYUKEI-SEC
      *
           .
       2101-IPPAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・一般国保分集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD     1                   TO  SKT3310TMP-KENSU (IDX1 IDX2)
      *
      *    療養の給付:診療実日数
           ADD     RECE10-JNISSU(1)    TO  SKT3310TMP-NISSU(IDX1 IDX2)
      *
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT3310TMP-TENSU(IDX1 IDX2)
      *
      *    療養の給付：一部負担金
           ADD     RECE10-FTNMONEY(1)  TO  SKT3310TMP-ITBFTN(IDX1 IDX2)
      *
           IF      RECE10-SHOKUJINISSU(1)  >  0
      *        食事療養:件数
               ADD     1               TO  SKT3310TMP-SKENSU(IDX1)
           END-IF
      *    食事療養:日数
           ADD     RECE10-SHOKUJINISSU(1)
                                       TO  SKT3310TMP-SNISSU(IDX1)
      *    食事療養：金額
           ADD     RECE10-SHOKUJIRYOYOHI(1)
                                       TO SKT3310TMP-SKINGK(IDX1)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(1)
                                       TO  SKT3310TMP-SFTNGK(IDX1)
      *
           MOVE    1       TO  FLG-UPD 
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅分集計処理
      *****************************************************************
       2101-ZAI-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD     1                   TO  SKT3310TMP-ZKENSU (IDX1)
      *
      *    療養の給付:診療実日数
           ADD     RECE10-JNISSU(1)    TO  SKT3310TMP-ZNISSU(IDX1)
      *
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT3310TMP-ZTENSU(IDX1)
      *
      *    療養の給付：一部負担金
           ADD     RECE10-FTNMONEY(1)  TO  SKT3310TMP-ZITBFTN(IDX1)
      *
           MOVE    1       TO  FLG-UPD 
           .
       2101-ZAI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人併用公費負担分集計処理
      *****************************************************************
       220-RKOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      RECE10-KOHNUM(IDX3) =   SPACE
      *
      *
      *        ０５１と０９１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX3) =   "091"
                   MOVE    "051"       TO  RECE10-KOHNUM(IDX3)
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
      *        １４１と２４１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX3) =   "241"
                   MOVE    "141"       TO  RECE10-KOHNUM(IDX3)
               END-IF
      *
      *        公費判別
               IF      RECE10-KOHNUM(IDX3)     =   "141"
                   PERFORM 220-41KOH-SYUKEI-SEC
               ELSE
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       20
                           OR      SKT3310TMP-BKOHNUM(IDX4) = SPACE
                       IF      RECE10-KOHNUM (IDX3)    =
                                           SKT3310TMP-BKOHNUM(IDX4)
                           AND  RECE10-NYUGAIKBN  =
                                           SKT3310TMP-BNYUGAIKBN(IDX4)
                           MOVE    IDX4    TO  CNT-KOH
                           MOVE    20      TO  IDX4
                       END-IF
                   END-PERFORM
      *
      *            該当公費がなかった場合
                   IF  IDX4  < 20
                       MOVE  IDX4  TO  CNT-KOH
                   END-IF
      *
                   MOVE  1  TO  FLG-BIKO
                   PERFORM  2201-BIKO-SYUKEI-SEC
      *
      *            入院データの場合は公費番号毎に食事療養費等を集計する
      **             IF      RECE10-SHOKUJINISSU(IDX3 + 1)   >   ZERO
                   IF  RECE10-NYUGAIKBN  =  1
                       PERFORM VARYING IDX4    FROM    1   BY  1
                               UNTIL   IDX4    >       20
                               OR      SKT3310TMP-BSKOHNUM(IDX4)
                                                           = SPACE
                           IF      RECE10-KOHNUM(IDX3) =  
                                           SKT3310TMP-BSKOHNUM(IDX4)
                               MOVE    IDX4    TO  CNT-KOH
                               MOVE    20      TO  IDX4
                           END-IF
                       END-PERFORM
      *
      *                該当公費がなかった場合
                       IF  IDX4    <   20
                           MOVE    IDX4    TO  CNT-KOH
                       END-IF
      *
                       MOVE  2  TO  FLG-BIKO
                       PERFORM  2201-BIKO-SYUKEI-SEC
      *
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           .
       220-RKOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    備考集計処理（公費負担分は収納から）
      *****************************************************************
       2201-BIKO-SYUKEI-SEC        SECTION.
      *
           IF  RECE10-NYUGAIKBN  =  "1"
               MOVE  "key33"                TO  WRK-PATH2
           ELSE
               MOVE  "key25"                TO  WRK-PATH2
           END-IF
      *
           INITIALIZE  WRK-BIKO
           PERFORM  900-SYUNOU-READ-SEC
      *
      *    療養の給付／食事
           IF  FLG-BIKO  =  1
      *      公費名
             MOVE  RECE10-KOHNUM(IDX3)  TO  SKT3310TMP-BKOHNUM(CNT-KOH)
      *
      *    公費名称
             MOVE  WRK-KOHNAME    TO  SKT3310TMP-BKOHNAME(CNT-KOH)
      *
      *      入外区分
             MOVE  RECE10-NYUGAIKBN  TO  SKT3310TMP-BNYUGAIKBN(CNT-KOH)
      *
      *      療養の給付：件数
             ADD   1                    TO  SKT3310TMP-BKENSU(CNT-KOH)
      *
      *      療養の給付：一部負担金
             ADD  WRK-BIKO              TO  SKT3310TMP-BITBFTN(CNT-KOH)
           ELSE
      *      公費名
             MOVE  RECE10-KOHNUM(IDX3)  TO
                                        SKT3310TMP-BSKOHNUM(CNT-KOH)
      *
      *      食事件数
             ADD   1                    TO  SKT3310TMP-BSKENSU(CNT-KOH)
      *
      *      食事療養：金額
             ADD   WRK-BIKO             TO
                                        SKT3310TMP-BSKINGK(CNT-KOH)
           END-IF
      *
           .
       2201-BIKO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    備考欄へのデータ格納
      *****************************************************************
       2202-BIKO-SET-SEC        SECTION.
      *
           INITIALIZE  WRK-BIKO2
           PERFORM  VARYING  IDX5  FROM  1  BY  1
                    UNTIL    IDX5  >     4
                    OR       SYU-KOH-HKNNUM (IDX5)  =  SPACE
             IF  FLG-BIKO  =  1
               IF  SYU-KOH-HKNNUM (IDX5)  =  RECE10-KOHNUM (IDX3)
                 IF  IDX5  =  1
                   COMPUTE  WRK-BIKO2  =  SYU-SYUCOMPFTN-ENTANI
                                       -  SYU-KOH-COMPFTN-ENTANI (1)
                 ELSE
                   COMPUTE  WRK-BIKO2  =
                            SYU-KOH-COMPFTN-ENTANI (IDX5 - 1)
                         -  SYU-KOH-COMPFTN-ENTANI (IDX5)
                 END-IF
               END-IF
             ELSE
               IF  SYU-KOH-HKNNUM (IDX5)  =  RECE10-KOHNUM (IDX3)
                 IF  IDX5  =  1
                   COMPUTE  WRK-BIKO2  =  SYU-SYUSKJ-FTN
                                       -  SYU-SKJ-FTN (1)
                 ELSE
                   COMPUTE  WRK-BIKO2  =  SYU-SKJ-FTN (IDX5 - 1)
                                       -  SYU-SKJ-FTN (IDX5)
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           ADD  WRK-BIKO2           TO  WRK-BIKO
      *
           .
       2202-BIKO-SET-EXT.
           EXIT.
057400*
      *****************************************************************
      *    一般公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      RECE10-KOHNUM(IDX3) =   SPACE
      *
      *        ０５１と０９１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX3) =   "091"
                   MOVE    "051"       TO  RECE10-KOHNUM(IDX3)
               END-IF
      *
      *      公費番号まとめ情報があるとき
               MOVE  SPACE     TO  WRK-KOHNAME
               IF  WRK-2007-MAX  >  ZERO
                   MOVE  SPACE     TO  WRK-KOHNAME
                   PERFORM VARYING IDZ1    FROM    1   BY  1
                           UNTIL   IDZ1    >       WRK-2007-MAX
                       PERFORM VARYING IDZ2  FROM    1   BY  1
                               UNTIL   IDZ2  >       20
                               OR  WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                           IF  RECE10-KOHNUM(IDX3)  
                                        = WRK-2007-KOHNUM(IDZ1 IDZ2)
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
      *        １４１と２４１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX3)  =  "241"  OR  "341"
                                            OR "441" OR "541"
                   MOVE    "141"       TO  RECE10-KOHNUM(IDX3)
               END-IF
      *        乳幼児・障害・母子は同じ場所に集計する
               IF  RECE10-KOHNUM(IDX3)  =  "285" OR "385"
                                        OR "485" OR "585"
                                        OR "685" OR "785"
                   MOVE    "185"       TO  RECE10-KOHNUM(IDX3)
               END-IF
               IF  RECE10-KOHNUM(IDX3)  =  "280" OR "380" OR "480"
                   MOVE    "180"       TO  RECE10-KOHNUM(IDX3)
               END-IF
               IF  RECE10-KOHNUM(IDX3)  =  "286" OR "386" OR "486"
                   MOVE    "186"       TO  RECE10-KOHNUM(IDX3)
               END-IF
      *        公費判別
               IF      RECE10-KOHNUM(IDX3) =   "141"
                   PERFORM 220-41KOH-SYUKEI-SEC
               ELSE
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       20
                           OR      SKT3310TMP-KOHNUM(IDX4) = SPACE
                       IF      RECE10-KOHNUM (IDX3)    =  
                                           SKT3310TMP-KOHNUM (IDX4)
                           MOVE    IDX4    TO  CNT-KOH
                           MOVE    20      TO  IDX4
                       END-IF
                   END-PERFORM
      *
      *            該当公費がなかった場合
                   IF      IDX4    <   20
                       MOVE    IDX4    TO  CNT-KOH
                   END-IF
      *
      *            公費名
                   MOVE    RECE10-KOHNUM(IDX3)
                                   TO  SKT3310TMP-KOHNUM(CNT-KOH)
      *
      *            公費名称
                   MOVE    WRK-KOHNAME
                                   TO  SKT3310TMP-KOHNAME(CNT-KOH)
      *
      *            療養の給付：件数
                   ADD     1       TO  SKT3310TMP-K-KENSU(CNT-KOH IDX2)
      *
      *            療養の給付:診療実日数
                   ADD     RECE10-JNISSU(IDX3 + 1)
                                   TO  SKT3310TMP-K-NISSU(CNT-KOH IDX2)
      *
      *            療養の給付：点数
                   ADD     RECE10-TOTALTEN(IDX3 + 1)
                                   TO  SKT3310TMP-K-TENSU(CNT-KOH IDX2)
      *
      *            療養の給付：一部負担金
                   ADD     RECE10-FTNMONEY(IDX3 + 1)
                                   TO  SKT3310TMP-K-ITBFTN(CNT-KOH IDX2)
      *
                   IF      RECE10-SHOKUJINISSU(IDX3 + 1)   >   0
      *                食事療養:件数
                       ADD     1   TO  SKT3310TMP-K-SKENSU(CNT-KOH)
                   END-IF
      *            食事療養:日数
                   ADD     RECE10-SHOKUJINISSU(IDX3 + 1)
                                   TO  SKT3310TMP-K-SNISSU(CNT-KOH)
      *            食事療養：金額
                   ADD     RECE10-SHOKUJIRYOYOHI(IDX3 + 1)
                                   TO  SKT3310TMP-K-SKINGK(CNT-KOH)
      *            食事療養：標準負担額
                   ADD     RECE10-SHOKUJIFTN(IDX3 + 1)
                                   TO  SKT3310TMP-K-SFTNGK(CNT-KOH)
               END-IF
           END-PERFORM
      *
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    ４１老人公費負担分集計処理
      *****************************************************************
       220-41KOH-SYUKEI-SEC        SECTION.
      *
      *
      *    公費名
           MOVE    RECE10-KOHNUM(IDX3) TO  SKT3310TMP-41-KOHNUM
      *    公費名称
           MOVE    WRK-KOHNAME         TO  SKT3310TMP-41-KOHNAME
      *    療養の給付：件数
           ADD     1       TO  SKT3310TMP-K41-KENSU (IDX2)
      *    療養の給付:診療実日数
           ADD     RECE10-JNISSU(IDX3 + 1)
                           TO  SKT3310TMP-K41-NISSU(IDX2)
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(IDX3 + 1)
                           TO  SKT3310TMP-K41-TENSU(IDX2)
      *
      *    療養の給付：一部負担金
           ADD     RECE10-FTNMONEY(IDX3 + 1)
                           TO  SKT3310TMP-K41-ITBFTN(IDX2)
      *    療養の給付：薬剤一部負担金
           IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
               ADD   1  TO  SKT3310TMP-K41-SKENSU
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT3310TMP-K41-SNISSU
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT3310TMP-K41-SKINGK
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                               SKT3310TMP-K41-SFTNGK
      *
           .
       220-41KOH-SYUKEI-EXT.
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
           OPEN    INPUT   SKT3310TMP-FILE
           MOVE    ZERO    TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
      *
           PERFORM UNTIL   FLG-PRINT   =   1
               MOVE    1       TO  WRK-CNT
                                   WRK-CNT2
                                   WRK-CNT3
               MOVE    ZERO    TO  FLG-PASS
             
      *        公費ソート処理
               PERFORM KOH-SORT-SEC
      *
      *        一般保険者・国保組合判定
               IF      SKT3310TMP-KOKUKUMI-KBN     =   ZERO
      *
      *            帳票編集<見出し>処理
                   PERFORM 310-HEAD-HEN-SEC
      *
      *            帳票編集<明細>処理
                   PERFORM 320-BODY-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 330-PRINT-OUT-SEC
      *
      *            公費が残っているか
                   PERFORM UNTIL   SKT3310TMP-KOHNUM (WRK-CNT) =   SPACE
                           AND     SKT3310TMP-BKOHNUM(WRK-CNT2)
                                                               =   SPACE
                           AND     SKT3310TMP-BSKOHNUM(WRK-CNT3)
                                                               =   SPACE
                           AND     FLG-PASS    =   1
      *
      *                帳票編集<見出し>処理
                       PERFORM 311-HEAD-HEN-SEC
      *
                       IF      FLG-PASS    =   ZERO
      *                    帳票編集<明細>処理
                           PERFORM 321-BODY-HEN-SEC
                       END-IF
      *
      *                帳票編集<公費>処理
                       PERFORM  3211-BODY-HEN-SEC
      *
      *                帳票印刷処理
                       PERFORM 331-PRINT-OUT-SEC
                       MOVE    1   TO  FLG-PASS
                   END-PERFORM
               ELSE
      *
      *            公費が残っているか
                   PERFORM UNTIL   SKT3310TMP-KOHNUM (WRK-CNT) =   SPACE
                            AND    SKT3310TMP-BKOHNUM(WRK-CNT2)
                                                               =   SPACE
                            AND    SKT3310TMP-BSKOHNUM(WRK-CNT3)
                                                               =   SPACE
                            AND    FLG-PASS    =   1
      *
      *                帳票編集<見出し>処理
                       PERFORM 312-HEAD-HEN-SEC
      *
                       IF      FLG-PASS    =   ZERO
      *                    帳票編集<明細>処理
                           PERFORM 322-BODY-HEN-SEC
                       END-IF
      *
      *                帳票編集<公費>処理
                       PERFORM 3221-BODY-HEN-SEC
      *
      *                帳票印刷処理
                       PERFORM 332-PRINT-OUT-SEC
                       MOVE    1       TO  FLG-PASS
                   END-PERFORM
               END-IF
      *
      *        一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE   SKT3310TMP-FILE
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
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT3310
      *
      *    診療年月
           IF  WRK-PARA-SYOKBN  =  "2"
               MOVE    WRK-HEN-SRYYM       TO  WRK-SYMD (1:6)
           ELSE
               MOVE    SKT3310TMP-SRYYM    TO  WRK-SYMD (1:6)
           END-IF
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
           MOVE    WRK-SRYYMWH             TO  SKT3310-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT3310-SEIYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT3310-ADRS
           INSPECT SKT3310-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT3310-HOSPNAME
           INSPECT SKT3310-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SKT3310-KAISETUNAME
      *
      *    電話番号
           MOVE    WRK-HOSPTEL             TO  SKT3310-TEL
      *
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD        TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT          TO  SKT3310-HOSPCD
      *
      *    保険者番号
           MOVE     SKT3310TMP-HKNJANUM    TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE     WRK-OUT-INPUT          TO  SKT3310-HKNJANUM
      *
      *    保険者名
           MOVE    SKT3310TMP-HKNJANUM     TO  WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME     NOT =   SPACE
               STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                       "  殿"              DELIMITED BY SIZE
                       INTO                SKT3310-HKNJANAME
               END-STRING
           ELSE
               MOVE    "殿"        TO  SKT3310-HKNJANAME(23:2)
           END-IF
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"            TO  SKT3310-TOKUBETU
           END-IF
      *
      *    ページNO
           MOVE    CNT-PAGE        TO  WRK-Z4
           MOVE    WRK-Z4          TO  SKT3310-NO
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
      *    編集処理
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       9
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-KENSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3310-KENSU(IDX1 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-NISSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3310-NISSU(IDX1 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-TENSU(IDX1 IDX2) TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3310-TENSU(IDX1 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-ITBFTN(IDX1 IDX2)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3310-ITBFTN(IDX1 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-SKENSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3310-SKENSU(IDX1)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-SNISSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3310-SNISSU(IDX1)
      *
      *        食事療養：金額
               MOVE    SKT3310TMP-SKINGK(IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3310-SKINGK(IDX1)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-SFTNGK(IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3310-SFTNGK(IDX1)
           END-PERFORM
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
           IF  WRK-SYMD (1:6)  >=  WRK-CONS-SRYYM-200804
               MOVE   "SKT3310.red"    TO  SPRT-PRTID
           ELSE
               MOVE   "SKT3310o.red"   TO  SPRT-PRTID
           END-IF
           EVALUATE  WRK-PARA-TOKUBETU
             WHEN  "1"
               MOVE  "国保請求書"               TO  SPRT-TITLE
             WHEN  "2"
               MOVE  "国保請求書［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3310             TO  SPRT-PRTDATA
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
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT3320
      *    保険者番号
           MOVE    SKT3310TMP-HKNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3320-HKNJANUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3320-HOSPCD
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"            TO  SKT3320-TOKUBETU
           END-IF
      *
      *    ページ番号
           MOVE    CNT-PAGE                TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SKT3320-NO
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
      *
      *    老人編集処理
           PERFORM VARYING IDX1    FROM    10  BY  1
                   UNTIL   IDX1    >       11
               COMPUTE IDX3    =   IDX1    -   9
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-KENSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3320-KENSU(IDX3 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-NISSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3320-NISSU(IDX3 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-TENSU(IDX1 IDX2) TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3320-TENSU(IDX3 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-ITBFTN (IDX1 IDX2)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3320-ITBFTN(IDX3 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-SKENSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3320-SKENSU(IDX3)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-SNISSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3320-SNISSU(IDX3)
      *
      *       食事療養：金額
               MOVE    SKT3310TMP-SKINGK(IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3320-SKINGK(IDX3)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-SFTNGK(IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3320-SFTNGK(IDX3)
           END-PERFORM
      *
      *    ４１老人
           IF      SKT3310TMP-41-KOHNUM    NOT =   SPACE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-K41-KENSU(IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3320-K-KENSU(1 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-K41-NISSU(IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3320-K-NISSU(1 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-K41-TENSU(IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3320-K-TENSU(1 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-K41-ITBFTN(IDX2) TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3320-K-ITBFTN(1 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-K41-SKENSU   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3320-K-SKENSU(1)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-K41-SNISSU   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3320-K-SNISSU(1)
      *
      *        食事療養：金額
               MOVE    SKT3310TMP-K41-SKINGK   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3320-K-SKINGK(1)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-K41-SFTNGK   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3320-K-SFTNGK(1)
      *
      *        公費名
               MOVE    SKT3310TMP-41-KOHNUM    TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM          TO  SKT3320-HBTNUM(1)
               MOVE    HKN-TANSEIDONAME    TO  SKT3320-TANSEIDONAME(1)
      *
               IF  SKT3310TMP-41-KOHNAME       NOT =  SPACE
                   MOVE  SKT3310TMP-41-KOHNAME
                                            TO  SKT3320-TANSEIDONAME(1)
               ELSE
                   MOVE  HKN-TANSEIDONAME   TO  SKT3320-TANSEIDONAME(1)
               END-IF
           END-IF
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<公費行>処理（公費等）
      *****************************************************************
       3211-BODY-HEN-SEC      SECTION.
      *
      *    ４１老人のデータがあり、１枚目の場合は、２番目から印字する
           IF      FLG-PASS    =   ZERO
           AND     SKT3310TMP-41-KOHNUM    NOT =   SPACE
               MOVE    2       TO  IDX4
           ELSE
               MOVE    1       TO  IDX4
           END-IF
      *
      *    一般公費の印字
           PERFORM VARYING IDX3    FROM    IDX4    BY  1
                   UNTIL   IDX3    >       8
                   OR      WRK-CNT >       20
                   OR      SKT3310TMP-KOHNUM(WRK-CNT)  =   SPACE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-K-KENSU(WRK-CNT IDX2)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3320-K-KENSU(IDX3 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-K-NISSU(WRK-CNT IDX2)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3320-K-NISSU(IDX3 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-K-TENSU(WRK-CNT IDX2)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3320-K-TENSU(IDX3 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-K-ITBFTN(WRK-CNT IDX2)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3320-K-ITBFTN(IDX3 IDX2)
               END-PERFORM 
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-K-SKENSU (WRK-CNT)   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3320-K-SKENSU(IDX3)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-K-SNISSU (WRK-CNT)   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3320-K-SNISSU(IDX3)
      *
      *        食事療養：金額
               MOVE    SKT3310TMP-K-SKINGK (WRK-CNT)   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3320-K-SKINGK(IDX3)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-K-SFTNGK (WRK-CNT)   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3320-K-SFTNGK(IDX3)
      *
      *        公費名
               MOVE    SKT3310TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM          TO  SKT3320-HBTNUM(IDX3)
               MOVE    HKN-TANSEIDONAME    
                                       TO  SKT3320-TANSEIDONAME(IDX3)
      *
               IF  SKT3310TMP-KOHNAME (WRK-CNT)  NOT =  SPACE
                   MOVE  SKT3310TMP-KOHNAME (WRK-CNT)
                                        TO  SKT3320-TANSEIDONAME(IDX3)
               ELSE
                   MOVE  HKN-TANSEIDONAME
                                        TO  SKT3320-TANSEIDONAME(IDX3)
               END-IF
               ADD     1   TO  WRK-CNT
           END-PERFORM
      *
           .
       3211-BODY-HEN-EXT.
           EXIT.
057400*
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
           IF  WRK-SYMD (1:6)  >=  WRK-CONS-SRYYM-200804
               MOVE   "SKT3320.red"    TO  SPRT-PRTID
           ELSE
               MOVE   "SKT3320o.red"   TO  SPRT-PRTID
           END-IF
           EVALUATE  WRK-PARA-TOKUBETU
             WHEN  "1"
               MOVE  "国保請求書"               TO  SPRT-TITLE
             WHEN  "2"
               MOVE  "国保請求書［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3320             TO  SPRT-PRTDATA
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
      *    国保組合用帳票編集<ヘッダー部>処理
      *****************************************************************
       312-HEAD-HEN-SEC          SECTION.
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT3330
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT3330-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT3330-SEIYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT3330-ADRS
           INSPECT SKT3330-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT3330-HOSPNAME
           INSPECT SKT3330-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME    TO  SKT3330-KAISETUNAME
      *
      *    電話番号
           MOVE    SYS-1002-TEL            TO  SKT3330-TEL
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3330-HOSPCD
      *
      *    保険者番号
           MOVE    SKT3310TMP-HKNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT3330-HKNJANUM
      *
      *    保険者名
           MOVE    SKT3310TMP-HKNJANUM     TO  WRK-HKNJANUM
           PERFORM 920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME     NOT =   SPACE
               STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                       "  殿"              DELIMITED BY SIZE
                       INTO                SKT3330-HKNJANAME
               END-STRING
           ELSE
               MOVE    "殿"                TO  SKT3330-HKNJANAME(23:2)
           END-IF
      *    
      *     ページ番号
           MOVE    CNT-PAGE    TO  WRK-Z4
           MOVE    WRK-Z4      TO  SKT3330-NO
      *
           .
       312-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保組合用帳票編集<明細行>処理（公費等）
      *****************************************************************
       322-BODY-HEN-SEC      SECTION.
      *
      *    老人編集処理
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       9
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-KENSU(IDX1 IDX2)     TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3330-KENSU(IDX1 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-NISSU(IDX1 IDX2)     TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3330-NISSU(IDX1 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-TENSU(IDX1 IDX2)     TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3330-TENSU(IDX1 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-ITBFTN (IDX1 IDX2)   TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3330-ITBFTN(IDX1 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-SKENSU(IDX1) TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3330-SKENSU(IDX1)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-SNISSU(IDX1) TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3330-SNISSU(IDX1)
      *
      *        食事療養：金額
               MOVE    SKT3310TMP-SKINGK(IDX1) TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3330-SKINGK(IDX1)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-SFTNGK(IDX1) TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3330-SFTNGK(IDX1)
           END-PERFORM
      *
      *    在宅分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       6
      *
      *        件数
               MOVE    SKT3310TMP-ZKENSU(IDX1) TO  WRK-Z3
               MOVE    WRK-Z3      TO  SKT3330-ZKENSU(IDX1)
      *
      *        日数
               MOVE    SKT3310TMP-ZNISSU(IDX1) TO  WRK-Z3
               MOVE    WRK-Z3      TO  SKT3330-ZNISSU(IDX1)
      *
      *        点数
               MOVE    SKT3310TMP-ZTENSU(IDX1) TO  WRK-Z7
               MOVE    WRK-Z7      TO  SKT3330-ZTENSU(IDX1)
      *
      *        一部負担金
               MOVE    SKT3310TMP-ZITBFTN(IDX1)
                                               TO  WRK-Z7
               MOVE    WRK-Z7      TO  SKT3330-ZITBFTN(IDX1)
           END-PERFORM
      *
      *    ４１老人
           IF      SKT3310TMP-41-KOHNUM    NOT =   SPACE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-K41-KENSU(IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3330-K-KENSU(1 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-K41-NISSU(IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3330-K-NISSU(1 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-K41-TENSU(IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3330-K-TENSU(1 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-K41-ITBFTN(IDX2) TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3330-K-ITBFTN(1 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-K41-SKENSU   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3330-K-SKENSU(1)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-K41-SNISSU   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3330-K-SNISSU(1)
      *
      *        食事療養：金額
               MOVE    SKT3310TMP-K41-SKINGK   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3330-K-SKINGK(1)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-K41-SFTNGK   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3330-K-SFTNGK(1)
      *
      *        公費名
               MOVE    SKT3310TMP-41-KOHNUM    TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM          TO  SKT3330-HBTNUM(1)
               MOVE    HKN-TANSEIDONAME    TO  SKT3330-TANSEIDONAME(1)
      *
               IF  SKT3310TMP-41-KOHNAME    NOT =  SPACE
                   MOVE  SKT3310TMP-41-KOHNAME
                                            TO  SKT3330-TANSEIDONAME(1)
               ELSE
                   MOVE  HKN-TANSEIDONAME   TO  SKT3330-TANSEIDONAME(1)
               END-IF
           END-IF
           .
       322-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保組合用帳票編集<公費行>処理
      *****************************************************************
       3221-BODY-HEN-SEC                SECTION.
      *
      *    ４１老人のデータがあり、１枚目の場合は、２番目から印字する
           IF      FLG-PASS    =   ZERO
           AND     SKT3310TMP-41-KOHNUM    NOT =   SPACE
               MOVE    2       TO  IDX4
           ELSE
               MOVE    1       TO  IDX4
           END-IF
      *
      *    一般公費の印字
           PERFORM VARYING IDX3    FROM    IDX4    BY  1
                   UNTIL   IDX3    >       4
                   OR      WRK-CNT >       20
                   OR      SKT3310TMP-KOHNUM(WRK-CNT)  =   SPACE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT3310TMP-K-KENSU(WRK-CNT IDX2)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3330-K-KENSU(IDX3 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT3310TMP-K-NISSU(WRK-CNT IDX2)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT3330-K-NISSU(IDX3 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT3310TMP-K-TENSU(WRK-CNT IDX2)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3330-K-TENSU(IDX3 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT3310TMP-K-ITBFTN(WRK-CNT IDX2)
                                                       TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT3330-K-ITBFTN(IDX3 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT3310TMP-K-SKENSU (WRK-CNT)   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3330-K-SKENSU(IDX3)
      *
      *        食事療養：日数
               MOVE    SKT3310TMP-K-SNISSU (WRK-CNT)   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT3330-K-SNISSU(IDX3)
      *
      *        食事療養：金額
               MOVE    SKT3310TMP-K-SKINGK (WRK-CNT)   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3330-K-SKINGK(IDX3)
      *
      *        食事療養：標準負担額
               MOVE    SKT3310TMP-K-SFTNGK (WRK-CNT)   TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT3330-K-SFTNGK(IDX3)
      *
      *        公費名
               MOVE    SKT3310TMP-KOHNUM (WRK-CNT)     TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM      TO  SKT3330-HBTNUM(IDX3)
               MOVE    HKN-TANSEIDONAME   
                                       TO  SKT3330-TANSEIDONAME(IDX3)
      *
               IF  SKT3310TMP-KOHNAME (WRK-CNT)  NOT =  SPACE
                   MOVE  SKT3310TMP-KOHNAME (WRK-CNT)
                                         TO  SKT3330-TANSEIDONAME(IDX3)
               ELSE
                   MOVE  HKN-TANSEIDONAME
                                         TO  SKT3330-TANSEIDONAME(IDX3)
               END-IF
               ADD     1   TO  WRK-CNT
           END-PERFORM
      *
      *    老人併用公費療養の給付の印字(備考欄)
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      WRK-CNT2   >     20
                   OR      SKT3310TMP-BKOHNUM(WRK-CNT2)    =   SPACE
      *
      *        療養の給付：入外
               IF  SKT3310TMP-BNYUGAIKBN(WRK-CNT2)  =  "1"
                 MOVE    "入"      TO  SKT3330-BNYUGAI (IDX3)
               ELSE
                 MOVE    "外"      TO  SKT3330-BNYUGAI (IDX3)
               END-IF
      *        療養の給付：件数
               MOVE    SKT3310TMP-BKENSU(WRK-CNT2)     TO  WRK-Z3
               MOVE    WRK-Z3   TO  SKT3330-BKENSU(IDX3)
               MOVE    "件"     TO  SKT3330-BKEN(IDX3)
      *
      *        療養の給付：一部負担金
               MOVE    SKT3310TMP-BITBFTN(WRK-CNT2)    TO  WRK-Z79
               MOVE    WRK-Z79  TO  SKT3330-BITBFTN(IDX3)
               MOVE    "円"     TO  SKT3330-BFILLER2(IDX3)
      *
      *        公費名
               MOVE    SKT3310TMP-BKOHNUM (WRK-CNT2)   TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM      TO  SKT3330-BKOHNUM(IDX3)
      *
               ADD     1       TO  WRK-CNT2
           END-PERFORM
      *
      *    老人併用公費食事療養費の印字(備考欄)
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       2
                   OR      WRK-CNT3    >   20
                   OR      SKT3310TMP-BSKOHNUM(WRK-CNT3)   =   SPACE
      *
      *        食事療養費：件数
               MOVE    SKT3310TMP-BKENSU(WRK-CNT3)     TO  WRK-Z3
               MOVE    WRK-Z3      TO  SKT3330-BSKENSU(IDX3)
      *
      *        食事療養費：金額
               MOVE    SKT3310TMP-BSKINGK(WRK-CNT3)    TO  WRK-Z79
               MOVE    WRK-Z79     TO  SKT3330-BSKINGK(IDX3)
               MOVE    "円"        TO  SKT3330-BSFILLER1(IDX3)
      *
      *        公費名
               MOVE    SKT3310TMP-BSKOHNUM(WRK-CNT3)   TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM      TO  SKT3330-BSKOHNUM(IDX3)
      *
               ADD     1       TO  WRK-CNT3
           END-PERFORM
      *
           .
       3221-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（国保組合用）
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
           MOVE   "SKT3330.red"        TO  SPRT-PRTID
           MOVE    "国保請求書"        TO  SPRT-TITLE
           MOVE    SKT3330             TO  SPRT-PRTDATA
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
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
      *
           INITIALIZE  SORT-AREA
           INITIALIZE  SORT-AREA2
      *    一般公費数と最小公費を取得
           MOVE    ZERO    TO  IDX-MAX
                               IDX2
           MOVE    SPACE   TO  WRK-KOHNUM
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       20
               IF  SKT3310TMP-KOHNUM(IDX1) =   SPACE
                   MOVE    20      TO  IDX1
               ELSE
                   IF      WRK-KOHNUM              =   SPACE
                   OR      SKT3310TMP-KOHNUM(IDX1) <   WRK-KOHNUM
                       MOVE    IDX1        TO  IDX2
                       MOVE    SKT3310TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
                   END-IF
                   ADD     1       TO  IDX-MAX
               END-IF
           END-PERFORM
      *
           IF      IDX2    >   ZERO
               MOVE    SKT3310TMP-KOH-TBL(IDX2)    TO  SORT-KOH-TBL(1)
           END-IF
      *
           DISPLAY "一般ソート前１" SKT3310TMP-KOHNUM(1)
           DISPLAY "一般ソート前２" SKT3310TMP-KOHNUM(2)
           DISPLAY "一般ソート前３" SKT3310TMP-KOHNUM(3)
           DISPLAY "一般ソート前４" SKT3310TMP-KOHNUM(4)
           DISPLAY "一般ソート前５" SKT3310TMP-KOHNUM(5)
      *
      *    公費が無い場合はソートしない
           IF      IDX-MAX     >   ZERO
               MOVE    2       TO  IDX1
               MOVE    1       TO  IDX2
                                   IDX3
               MOVE    "999"   TO  WRK-KOHNUM
               PERFORM UNTIL   IDX1    >   IDX-MAX
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       IDX-MAX
                       IF     (SKT3310TMP-KOHNUM(IDX2)
                                           >   SORT-KOHNUM(IDX1 - 1))
                       AND    (SKT3310TMP-KOHNUM(IDX2) <   WRK-KOHNUM)
                           MOVE    SKT3310TMP-KOHNUM(IDX2)
                                                       TO WRK-KOHNUM
                           MOVE    IDX2    TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    SKT3310TMP-KOH-TBL(IDX3)    
                                           TO  SORT-KOH-TBL(IDX1)
                   MOVE    "999"   TO  WRK-KOHNUM
                   ADD     1       TO  IDX1
               END-PERFORM
      *
               INITIALIZE  SKT3310TMP-KOH-AREA
               MOVE    SORT-AREA   TO  SKT3310TMP-KOH-AREA
      * 
           END-IF
           DISPLAY "一般ソート後１" SKT3310TMP-KOHNUM(1)
           DISPLAY "一般ソート後２" SKT3310TMP-KOHNUM(2)
           DISPLAY "一般ソート後３" SKT3310TMP-KOHNUM(3)
           DISPLAY "一般ソート後４" SKT3310TMP-KOHNUM(4)
           DISPLAY "一般ソート後５" SKT3310TMP-KOHNUM(5)
      *
      *
      *    老人併用公費数と最小公費を取得
           MOVE    ZERO    TO  IDX-MAX
                               IDX2
           MOVE    SPACE   TO  WRK-KOHNUM-R
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       20
               IF  SKT3310TMP-BKOHNUM(IDX1)    =   SPACE
                   MOVE    20      TO  IDX1
               ELSE
                   IF      WRK-KOHNUM-R                =  SPACE
                   OR      SKT3310TMP-BKOHNUM-R(IDX1)  <  WRK-KOHNUM-R
                       MOVE    IDX1        TO  IDX2
                       MOVE    SKT3310TMP-BKOHNUM-R(IDX1)
                                                       TO  WRK-KOHNUM-R
                   END-IF
                   ADD     1       TO  IDX-MAX
               END-IF
           END-PERFORM
           IF      IDX2    >   ZERO
               MOVE    SKT3310TMP-BIKO-TBL(IDX2)   
                                               TO  SORT-BKOH-TBL(1)
           END-IF
      *
           DISPLAY "老人併用ソート前１" SKT3310TMP-BKOHNUM(1)
           DISPLAY "老人併用ソート前２" SKT3310TMP-BKOHNUM(2)
           DISPLAY "老人併用ソート前３" SKT3310TMP-BKOHNUM(3)
           DISPLAY "老人併用ソート前４" SKT3310TMP-BKOHNUM(4)
           DISPLAY "老人併用ソート前５" SKT3310TMP-BKOHNUM(5)
      *
      *    公費が無い場合はソートしない
           IF      IDX-MAX     >   ZERO
               MOVE    2       TO  IDX1
               MOVE    1       TO  IDX2
                                   IDX3
               MOVE    "9999"  TO  WRK-KOHNUM-R
               PERFORM UNTIL   IDX1    >   IDX-MAX
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       IDX-MAX
                       IF     (SKT3310TMP-BKOHNUM-R(IDX2)
                                           >   SORT-BKOHNUM-R(IDX1 - 1))
                       AND    (SKT3310TMP-BKOHNUM-R(IDX2)
                                                   <   WRK-KOHNUM-R)
                           MOVE    SKT3310TMP-BKOHNUM-R(IDX2)
                                                   TO  WRK-KOHNUM-R
                           MOVE    IDX2    TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    SKT3310TMP-BIKO-TBL(IDX3) 
                                           TO  SORT-BKOH-TBL(IDX1)
                   MOVE    "9999"  TO  WRK-KOHNUM-R
                   ADD     1       TO  IDX1
               END-PERFORM
      *
               INITIALIZE  SKT3310TMP-BIKO-AREA
               MOVE    SORT-AREA2  TO  SKT3310TMP-BIKO-AREA
      * 
           END-IF
           DISPLAY "老人併用ソート後１" SKT3310TMP-BKOHNUM(1)
           DISPLAY "老人併用ソート後２" SKT3310TMP-BKOHNUM(2)
           DISPLAY "老人併用ソート後３" SKT3310TMP-BKOHNUM(3)
           DISPLAY "老人併用ソート後４" SKT3310TMP-BKOHNUM(4)
           DISPLAY "老人併用ソート後５" SKT3310TMP-BKOHNUM(5)
      *
      *
      *    老人併用公費数(食事療養費分)と最小公費を取得
           MOVE    ZERO    TO  IDX-MAX
                               IDX2
           MOVE    SPACE   TO  WRK-KOHNUM
           INITIALIZE  SORT-AREA2
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       20
               IF  SKT3310TMP-BSKOHNUM(IDX1)   =   SPACE
                   MOVE    20      TO  IDX1
               ELSE
                   IF      WRK-KOHNUM                  =   SPACE   
                   OR      SKT3310TMP-BSKOHNUM(IDX1)   <   WRK-KOHNUM
                       MOVE    IDX1        TO  IDX2
                       MOVE    SKT3310TMP-BSKOHNUM(IDX1)
                                                       TO  WRK-KOHNUM
                   END-IF
                   ADD     1       TO  IDX-MAX
               END-IF
           END-PERFORM
           IF      IDX2    >   ZERO
               MOVE    SKT3310TMP-BIKO-TBL2(IDX2)  TO  SORT-BKOH-TBL(1)
           END-IF
      *
           DISPLAY "老人併用食事ソート前１" SKT3310TMP-BSKOHNUM(1)
           DISPLAY "老人併用食事ソート前２" SKT3310TMP-BSKOHNUM(2)
           DISPLAY "老人併用食事ソート前３" SKT3310TMP-BSKOHNUM(3)
           DISPLAY "老人併用食事ソート前４" SKT3310TMP-BSKOHNUM(4)
           DISPLAY "老人併用食事ソート前５" SKT3310TMP-BSKOHNUM(5)
      *
      *    公費が無い場合はソートしない
           IF      IDX-MAX     >   ZERO
               MOVE    2       TO  IDX1
               MOVE    1       TO  IDX2
                                   IDX3
               MOVE    "999"   TO  WRK-KOHNUM
               PERFORM UNTIL   IDX1    >   IDX-MAX
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       IDX-MAX
                       IF     (SKT3310TMP-BSKOHNUM(IDX2)
                                           >   SORT-BKOHNUM(IDX1 - 1))
                       AND    (SKT3310TMP-BSKOHNUM(IDX2)   
                                           <   WRK-KOHNUM)
                           MOVE    SKT3310TMP-BSKOHNUM(IDX2)
                                           TO  WRK-KOHNUM
                           MOVE    IDX2    TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    SKT3310TMP-BIKO-TBL2(IDX3)
                                       TO  SORT-BKOH-TBL(IDX1)
                   MOVE    "999"       TO  WRK-KOHNUM
                   ADD     1       TO  IDX1
               END-PERFORM
      *
               INITIALIZE             SKT3310TMP-BIKO-AREA2
               MOVE  SORT-AREA2  TO   SKT3310TMP-BIKO-AREA2
      * 
           END-IF
           DISPLAY "老人併用食事ソート後１" SKT3310TMP-BSKOHNUM(1)
           DISPLAY "老人併用食事ソート後２" SKT3310TMP-BSKOHNUM(2)
           DISPLAY "老人併用食事ソート後３" SKT3310TMP-BSKOHNUM(3)
           DISPLAY "老人併用食事ソート後４" SKT3310TMP-BSKOHNUM(4)
           DISPLAY "老人併用食事ソート後５" SKT3310TMP-BSKOHNUM(5)
      *
           .
       KOH-SORT-EXT.
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
                       MOVE  "*** SOKATU3310 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU3310 SEIKYU INVALID ***"
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
           MOVE    WRK-HKNJANUM            TO  HKNJA-HKNJANUM
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
               PERFORM  925-HKNJAINF-READ-SEC
           ELSE
               DISPLAY  "HKNJAINF SELECT ERR"
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
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
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
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)   TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                      TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD              TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"     TO  MCP-TABLE
               MOVE    "key5"           TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
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
           CALL    "ORCDBMAIN"         USING
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
      *
             IF    HKN-HKNNUM  =  "141"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "４１老人"   TO   HKN-TANSEIDONAME
             END-IF
      *
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
      *    収納マスタ読込
      *****************************************************************
       900-SYUNOU-READ-SEC         SECTION.
      *
           INITIALIZE  SYUNOU-REC
           MOVE  RECE10-HOSPNUM     TO  SYU-HOSPNUM
           MOVE  RECE10-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE  RECE10-PTID        TO  SYU-PTID
           MOVE  RECE10-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
           MOVE  "__"               TO  SYU-SRYYMD(7:2)
           MOVE  RECE10-KOHNUM(IDX3)    TO  SYU-KOH-HKNNUM(1)
                                            SYU-KOH-HKNNUM(2)
                                            SYU-KOH-HKNNUM(3)
                                            SYU-KOH-HKNNUM(4)
           MOVE  SYUNOU-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    WRK-PATH2        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           IF    MCP-RC              =   ZERO
               PERFORM  900-SYUNOU-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
      *          備考欄へのデータ集計
                 PERFORM  2202-BIKO-SET-SEC
              DISPLAY  "BIKO GET"  SYU-NYUGAIKBN
      *
                 PERFORM  900-SYUNOU-FETCH-SEC
               END-PERFORM
      *
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    WRK-PATH2        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタフェッチ
      *****************************************************************
       900-SYUNOU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    WRK-PATH2        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT3310TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT3310TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT3310TMP
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
           READ  SKT3310TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
      *
           MOVE    "DBCLOSECURSOR"                    TO  MCP-FUNC
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
