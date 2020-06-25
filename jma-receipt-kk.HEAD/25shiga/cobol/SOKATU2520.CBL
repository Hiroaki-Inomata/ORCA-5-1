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
       PROGRAM-ID.             SOKATU2520.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/07/01    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    山口    05/09/14    対象公費の追加
      *  01.00.02    内田    06/11/26    長期上位の追加
      *  01.00.03    内田    07/02/06    帳票様式の変更に伴う修正
      *                                  堺市は市でまとめる
      *  01.00.04    内田    07/09/08    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.05    内田    08/01/07    印字対象番号の追加（441,541）
      *  01.00.06    内田    08/03/18    政令指定都市の代表番号は
      *                                  国保総括表にある代表番号に変更
      *  01.00.07    伊藤    08/04/29    帳票様式に伴う修正
      *  01.00.08    内田    08/05/04    帳票様式に伴う修正
      *  01.00.09    吉川    08/10/20    シス管「2007」の公費まとめ対応
      *                                  SPA-AREA初期化
      *  01.00.10    吉川    09/12/02    シス管「2007」のまとめ公費で
      *                                  6つ目以降がまとまらない不具合を修正
      *  01.00.11    吉川    10/04/05    オンライン返戻対応
      *  01.00.12    吉川    10/04/16    印字対象番号の追加（340）
      *                                  政令指定都市対応（相模原市）
      *  01.00.13    吉川    10/11/01    特別療養費国保対応
      *  01.00.14    吉川    10/11/24    open-cobol1.0対応
      *  01.00.15    吉川    12/05/25    在宅はまとめて集計するように修正
      *  01.00.16    吉川    13/01/10    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.17    吉川    13/06/17    対象公費番号をINIファイルから
      *                                  取得するようにする
      *  01.00.18    吉川    13/10/04    プログラムオプション対応
      *  01.00.19    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.20    門間    16/01/06    医療機関編集情報への開設者名追加対応
      *  01.00.21    吉川    16/03/11    返戻時、再印刷画面のタイトル修正
      *  01.00.22    萩野    16/10/05    公費番号まとめ情報の公費番号の追加
      *  01.00.23    門間    17/12/04    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.24    井上    18/12/12    修正履歴コメント削除
      *                                  保険番号情報の取得処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT2520TMP-FILE ASSIGN    SKT2520TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT2520TMP-KEY
                                   FILE    STATUS  IS  STS-SKT2520.
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
       FD  SKT2520TMP-FILE.
           COPY    "CPSKT2520TMP.INC".
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
                                   BY         //SKT2520TP//.
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
           COPY    "SKT2520.INC".
      *
      *    対象公費番号データ
           COPY  "TAISYO2501.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT2520         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
           03  STS-TAISYOKOHI      PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT2520TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KOH             PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-TAISYOKOHI      PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
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
           03  WRK-ITBFTN          PIC 9(10).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-KOHNAME         PIC X(12).
      *
           03  WRK-TENHYO          PIC 99.
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
           03  WRK-YUSEN             PIC X(06).
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
           03  WRK-KAISETUNAME     PIC X(80).
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
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
           03  CNT-KOH             PIC 9(02).
      *
       01  SORT-AREA.
           03  SORT-TBL.
               05  SORT-KOH-TBL      OCCURS  20.
                   07  SORT-KOHNUM       PIC  X(03).
                   07  SORT-KOHNAME      PIC  X(12).
                   07  SORT-KKENSU       PIC  9(06).
                   07  SORT-KNISSU       PIC  9(06).
                   07  SORT-KTENSU       PIC  9(10).
                   07  SORT-KYKZFTN      PIC  9(10).
                   07  SORT-KITBFTN      PIC  9(10).
      *
           COPY    "COMMON-SPA".
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
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
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TAISYO-DAT
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
           MOVE    "SOKATU2520"    TO  JOB-PGID
           MOVE    "滋賀国保請求書"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT2520"           TO  SKT2520TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT2520TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT2520TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT2520TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT2520TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
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
           MOVE    "SOKATU2520"              TO  WRK-PRGID
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
      *
           OPEN    OUTPUT  UPD-FILE
      *
           OPEN    OUTPUT  SKT2520TMP-FILE
           CLOSE           SKT2520TMP-FILE
           OPEN    I-O     SKT2520TMP-FILE
      *
           MOVE    ZERO    TO  FLG-END
           MOVE    ZERO    TO  FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT2520TMP-FILE
      *
           DISPLAY "FLG-UPD=" FLG-UPD
           IF      FLG-UPD =   1
               PERFORM 2002-PRINT-SEC
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
           DISPLAY "*** SOKATU2520 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU2520 END ***"
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
           INITIALIZE       SKT2520TMP-REC
      *
           MOVE    RECE10-HKNJANUM-S   TO  SKT2520TMP-HKNJANUM
      *
           EVALUATE    SKT2520TMP-HKNJANUM(1:3)
      *        札幌市の場合、代表番号セット
               WHEN    "014"
                   MOVE    "014001"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "札幌市"    TO  SKT2520TMP-HKNJANAME
      *        仙台市の場合、代表番号セット
               WHEN    "044"
                   MOVE    "044008"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "仙台市"    TO  SKT2520TMP-HKNJANAME
      *        さいたま市の場合、代表番号セット
               WHEN    "114"
                   MOVE    "114009"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "さいたま市"    TO  SKT2520TMP-HKNJANAME
      *        千葉市の場合、代表番号セット
               WHEN    "124"
                   MOVE    "124008"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "千葉市"    TO  SKT2520TMP-HKNJANAME
      *        横浜市の場合、代表番号セット
               WHEN    "144"
                   MOVE    "144006"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "横浜市"    TO  SKT2520TMP-HKNJANAME
      *        川崎市の場合、代表番号セット
               WHEN    "145"
                   MOVE    "145003"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "川崎市"    TO  SKT2520TMP-HKNJANAME
      *        相模原市の場合、代表番号セット
               WHEN    "146"
                   MOVE    "146001"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "相模原市"  TO  SKT2520TMP-HKNJANAME
      *        新潟市の場合、代表番号セット
               WHEN    "154"
                   MOVE    "154005"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "新潟市"    TO  SKT2520TMP-HKNJANAME
      *        名古屋市の場合、代表番号セット
               WHEN    "234"
                   MOVE    "234005"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "名古屋市"  TO  SKT2520TMP-HKNJANAME
                   IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                   AND  (RECE10-FLG (1)   =  1)
                       MOVE    7   TO  SKT2520TMP-KYURATE
                   ELSE
                       IF  (RECE10-AGE > 2) AND 
                           (RECE10-AGE < 70)
                           MOVE    RECE10-KYURATE
                                       TO  SKT2520TMP-KYURATE
                       ELSE
                           MOVE    7   TO  SKT2520TMP-KYURATE
                       END-IF
                   END-IF
      *        京都市の場合、代表番号セット
               WHEN    "264"
                   MOVE    "264002"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "京都市"    TO  SKT2520TMP-HKNJANAME
      *        大阪市の場合、代表番号セット
               WHEN    "274"
                   MOVE    "274001"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "大阪市"    TO  SKT2520TMP-HKNJANAME
      *        堺市の場合、代表番号セット
               WHEN    "275"
                   MOVE    "275008"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "堺市"      TO  SKT2520TMP-HKNJANAME
      *        神戸市の場合、代表番号セット
               WHEN    "284"
                   MOVE    "284000"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "神戸市"    TO  SKT2520TMP-HKNJANAME
      *        広島市の場合、代表番号セット
               WHEN    "344"
                   MOVE    "344002"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "広島市"    TO  SKT2520TMP-HKNJANAME
      *        北九州市の場合、代表番号セット
               WHEN    "404"
                   MOVE    "404004"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "北九州市"  TO  SKT2520TMP-HKNJANAME
      *        福岡市の場合、代表番号セット
               WHEN    "405"
                   MOVE    "405001"    TO  SKT2520TMP-HKNJANUM
                   MOVE    "福岡市"    TO  SKT2520TMP-HKNJANAME
           END-EVALUATE
      *
      *        法定外給付チェック(滋賀県医師国保は除く)
               EVALUATE    TRUE
                   WHEN   (SKT2520TMP-HKNJANUM (3:1) = "3") OR
                          (SKT2520TMP-HKNJANUM = "230011")
                       IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                       AND  (RECE10-FLG (1)   =  1)
                           MOVE    7   TO  SKT2520TMP-KYURATE
                       ELSE
                           IF  (RECE10-AGE > 2) AND 
                               (RECE10-AGE < 70)
                               MOVE    RECE10-KYURATE
                                       TO  SKT2520TMP-KYURATE
                           ELSE
                               MOVE    7   TO  SKT2520TMP-KYURATE
                           END-IF
                       END-IF
               END-EVALUATE
      *
      *    指定された保険者は、最後に印刷する
           IF      WRK-YUSEN       =   SKT2520TMP-HKNJANUM
               MOVE    99          TO  SKT2520TMP-SYUTURYOKU
           ELSE
               IF      RECE10-PREFKBN  =   "1"
                   PERFORM 20011-KENNAI-SEC
               ELSE
                   PERFORM 20012-KENGAI-SEC
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
      *    県内出力順設定処理
      *****************************************************************
       20011-KENNAI-SEC          SECTION.
      *
           EVALUATE    SKT2520TMP-HKNJANUM
               WHEN    "253013"
                  MOVE    1       TO  SKT2520TMP-SYUTURYOKU
               WHEN    "093013"
                  MOVE    2       TO  SKT2520TMP-SYUTURYOKU
               WHEN    "133033"
                  MOVE    3       TO  SKT2520TMP-SYUTURYOKU
               WHEN    "133264"
                  MOVE    4       TO  SKT2520TMP-SYUTURYOKU
               WHEN    "133298"
                  MOVE    5       TO  SKT2520TMP-SYUTURYOKU
               WHEN    OTHER
                  MOVE    6       TO  SKT2520TMP-SYUTURYOKU
           END-EVALUATE
      *
           .
       20011-KENNAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    県外出力順設定処理
      *****************************************************************
       20012-KENGAI-SEC          SECTION.
      *
           EVALUATE    SKT2520TMP-HKNJANUM
               WHEN "010017"
                   MOVE    7       TO  SKT2520TMP-SYUTURYOKU
               WHEN "040014"
                   MOVE    8       TO  SKT2520TMP-SYUTURYOKU
               WHEN "110940"
                   MOVE    9       TO  SKT2520TMP-SYUTURYOKU
               WHEN "120014"
                   MOVE    10      TO  SKT2520TMP-SYUTURYOKU
               WHEN "140012"
                   MOVE    11      TO  SKT2520TMP-SYUTURYOKU
               WHEN "140020"
                   MOVE    12      TO  SKT2520TMP-SYUTURYOKU
               WHEN "230011"
                   MOVE    13      TO  SKT2520TMP-SYUTURYOKU
               WHEN "260018"
                   MOVE    14      TO  SKT2520TMP-SYUTURYOKU
               WHEN "270017"
                   MOVE    15      TO  SKT2520TMP-SYUTURYOKU
               WHEN "275008"
                   MOVE    16      TO  SKT2520TMP-SYUTURYOKU
               WHEN "280016"
                   MOVE    17      TO  SKT2520TMP-SYUTURYOKU
               WHEN "340018"
                   MOVE    18      TO  SKT2520TMP-SYUTURYOKU
               WHEN "400010"
                   MOVE    19      TO  SKT2520TMP-SYUTURYOKU
               WHEN "400028"
                   MOVE    20      TO  SKT2520TMP-SYUTURYOKU
               WHEN "133231"
                   MOVE    21      TO  SKT2520TMP-SYUTURYOKU
               WHEN "133280"
                   MOVE    22      TO  SKT2520TMP-SYUTURYOKU
               WHEN "233064"
                   MOVE    23      TO  SKT2520TMP-SYUTURYOKU
               WHEN "263129"
                   MOVE    24      TO  SKT2520TMP-SYUTURYOKU
               WHEN "273102"
                   MOVE    25      TO  SKT2520TMP-SYUTURYOKU
               WHEN OTHER
                   MOVE    26      TO  SKT2520TMP-SYUTURYOKU
           END-EVALUATE
      *
           .
       20012-KENGAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    老人保健か
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               IF  RECE10-KYURATE  =  7
                   MOVE  11   TO  IDX1
               ELSE
                   MOVE  10   TO  IDX1
               END-IF
           ELSE
      *        一般／退職者
               IF      RECE10-HKNNUM   =   "060" OR "068"
                   IF      RECE10-AGE  >=  70
                       IF  RECE10-KYURATE  =  7
                           MOVE  2   TO  IDX1
                       ELSE
                           MOVE  1   TO  IDX1
                       END-IF
                   ELSE
                       IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                       AND  (RECE10-FLG (1)   =  1)
                           MOVE    4      TO  IDX1
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
                       IF  RECE10-KYURATE  =  7
                           MOVE  7   TO  IDX1
                       ELSE
                           MOVE  6   TO  IDX1
                       END-IF
                   ELSE
                       IF      RECE10-HONKZKKBN    =   "1"
                           MOVE    5       TO  IDX1
                       ELSE
                           IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                           AND  (RECE10-FLG (1)   =  1)
                               MOVE    9      TO  IDX1
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
           PERFORM 2102-SYUKEI-SEC
      *
      *    公費併用
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
      *    長期・原爆チェック
           IF      RECE10-ETC-KOHNUM(1)    NOT =   SPACE
               PERFORM 220-ETC-KOH-SEC
           END-IF
      *
           IF      FLG-SKT2520TMP  =   1
               WRITE   SKT2520TMP-REC
           ELSE
               REWRITE SKT2520TMP-REC
           END-IF
      *
           MOVE    1   TO  FLG-UPD
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
      *    療養の給付：件数
           ADD   1                   TO  SKT2520TMP-KENSU (IDX1)
      *
      *    療養の給付：診療実日数
           ADD   RECE10-JNISSU (1)   TO  SKT2520TMP-NISSU (IDX1)
      *
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT2520TMP-TENSU (IDX1)
      *
      *    療養の給付：一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT2520TMP-ITBFTN(IDX1)
      *
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
      *        ５１、９１は１つにまとめる
               IF    RECE10-KOHNUM(IDX3)  =  "091"
                   MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
               END-IF
      *
      *        全国共通公費
               IF      RECE10-KOHNUM(IDX3)(1:1)    =   "0"
      *        公費番号まとめ情報があるとき
                   MOVE  SPACE     TO  WRK-KOHNAME
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
      *            公費判別
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       20
                           OR      SKT2520TMP-KOHNUM (IDX4) = SPACE
                       IF      RECE10-KOHNUM(IDX3) =
                                           SKT2520TMP-KOHNUM(IDX4)
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
                                 TO  SKT2520TMP-KOHNUM(CNT-KOH)
      *
      *            公費名称
                   MOVE    WRK-KOHNAME
                                 TO  SKT2520TMP-KOHNAME(CNT-KOH)
      *
      *            療養の給付：件数
                   ADD     1       TO SKT2520TMP-KKENSU(CNT-KOH)
      *
      *            療養の給付:診療実日数
                   ADD     RECE10-JNISSU(IDX3 + 1) 
                                 TO SKT2520TMP-KNISSU(CNT-KOH)
      *
      *            療養の給付：点数
                   ADD     RECE10-TOTALTEN(IDX3 + 1)
                                 TO SKT2520TMP-KTENSU(CNT-KOH)
      *
      *            療養の給付：一部負担金 
                   ADD     RECE10-FTNMONEY (IDX3 + 1)
                                 TO SKT2520TMP-KITBFTN(CNT-KOH)
      *
               END-IF
      *
      *        対象地方公費(福祉)
               INITIALIZE                        TBL-HIKAKU-KOHI
               MOVE    RECE10-KOHNUM(IDX3)   TO  TBL-HKOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
                   IF      RECE10-FTNMONEY(IDX3 + 1)   >   ZERO
                       MOVE    RECE10-KOHNUM (IDX3)    TO  WRK-KOHNUM
                       PERFORM 950-SYUNOU-SET-SEC
                   ELSE
                      MOVE 1 TO FLG-KOH
                   END-IF
      *
                   IF FLG-KOH = 1
      *
      *                療養の給付：件数
                       ADD     1           TO  SKT2520TMP-FKENSU
      *
      *                療養の給付:診療実日数
                       ADD     RECE10-JNISSU(IDX3 + 1)
                                           TO  SKT2520TMP-FNISSU
      *
      *                療養の給付：点数
                       ADD     RECE10-TOTALTEN(IDX3 + 1)
                                           TO  SKT2520TMP-FTENSU
      *
      *                療養の給付：一部負担金
                       ADD     RECE10-FTNMONEY (IDX3 + 1)
                                           TO SKT2520TMP-FITBFTN
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期・原爆件数集計処理
      *****************************************************************
       220-ETC-KOH-SEC        SECTION.
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      RECE10-ETC-KOHNUM(IDX3) =   SPACE
               EVALUATE    RECE10-ETC-KOHNUM(IDX3)
                   WHEN    "972"
                   WHEN    "974"
                       ADD     1       TO   SKT2520TMP-TYOKI-KENSU
                   WHEN    "019"
                       ADD     1       TO   SKT2520TMP-GENBK-KENSU
               END-EVALUATE
           END-PERFORM
      *
           .
       220-ETC-KOH-EXT.
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
           OPEN     INPUT       SKT2520TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *        公費が残っているか
               MOVE    1           TO  WRK-CNT
               MOVE    ZERO        TO  FLG-PASS
      *
      *        公費印字順編集処理
               PERFORM  KOH-SORT-SEC
      *
               PERFORM UNTIL   SKT2520TMP-KOHNUM (WRK-CNT) = SPACE
                       AND     FLG-PASS  =  1
      *
      *            帳票編集<見出し>処理
                   PERFORM 311-HEAD-HEN-SEC
      *
                   IF      WRK-CNT     =   1
      *
      *                帳票編集<明細>処理
                       PERFORM 321-BODY-HEN-SEC
                   END-IF
      *
      *            帳票編集<明細>処理
                   PERFORM 323-BODY-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 331-PRINT-OUT-SEC
                   MOVE    1       TO  FLG-PASS
               END-PERFORM
      *
      *        一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    SKT2520TMP-FILE
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
           INITIALIZE                    SKT2520
      *    NO
           MOVE    CNT-PAGE            TO  WRK-Z4
           MOVE    WRK-Z4              TO  SKT2520-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT2520-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT2520-SEIYMD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT2520-ADRS
           INSPECT SKT2520-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT2520-HOSPNAME
           INSPECT SKT2520-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    電話番号
           MOVE    WRK-HOSPTEL             TO  SKT2520-TEL
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SKT2520-KAISETUNAME
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  SKT2520-HOSPCD
      *
      *    保険者番号
           MOVE    SKT2520TMP-HKNJANUM     TO  SKT2520-HKNJANUM
      *
      *    保険者名
           MOVE    SKT2520TMP-HKNJANUM     TO  WRK-HKNJANUM
           IF      SKT2520TMP-HKNJANAME    =   SPACE
              PERFORM 920-HKNJAINF-SET-SEC
           ELSE
              MOVE    SKT2520TMP-HKNJANAME TO  HKNJA-HKNJANAME
           END-IF
      *
           IF      HKNJA-HKNJANAME         =   SPACE
               MOVE    "殿"                TO  SKT2520-HKNJANAME(23:2)
           ELSE
               STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                       "  殿"              DELIMITED BY SIZE
                       INTO                SKT2520-HKNJANAME
               END-STRING
           END-IF
      *
           EVALUATE    SKT2520TMP-KYURATE
               WHEN 7
                   MOVE    "◯"    TO  SKT2520-KYURATE(4)
               WHEN 8
                   MOVE    "◯"    TO  SKT2520-KYURATE(3)
               WHEN 9
                   MOVE    "◯"    TO  SKT2520-KYURATE(2)
               WHEN 10
                   MOVE    "◯"    TO  SKT2520-KYURATE(1)
           END-EVALUATE
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"    TO  SKT2520-TOKUBETU
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       11
      *
      *        療養の給付：件数
               MOVE    SKT2520TMP-KENSU (IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT2520-KENSU(IDX1)
      *
      *        療養の給付：日数
               MOVE    SKT2520TMP-NISSU (IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT2520-NISSU(IDX1)
      *
      *        療養の給付：点数
               MOVE    SKT2520TMP-TENSU (IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT2520-TENSU(IDX1)
      *
      *        療養の給付：一部負担金
               MOVE    SKT2520TMP-ITBFTN (IDX1)    TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT2520-ITBFTN(IDX1)
           END-PERFORM
      *
      *    福祉公費の印字
      *    療養の給付：件数
           MOVE    SKT2520TMP-FKENSU   TO  WRK-Z5
           MOVE    WRK-Z5      TO  SKT2520-FKENSU
      *
      *    療養の給付：日数
           MOVE    SKT2520TMP-FNISSU   TO  WRK-Z5
           MOVE    WRK-Z5      TO  SKT2520-FNISSU
      *
      *    療養の給付：点数
           MOVE    SKT2520TMP-FTENSU   TO  WRK-Z9
           MOVE    WRK-Z9      TO  SKT2520-FTENSU
      *
      *    療養の給付：一部負担金
           MOVE    SKT2520TMP-FITBFTN  TO  WRK-Z9
           MOVE    WRK-Z9      TO  SKT2520-FITBFTN
      *
      *    原爆・長期件数印字
           MOVE    SKT2520TMP-TYOKI-KENSU      TO  WRK-Z5
           MOVE    WRK-Z5      TO  SKT2520-TYOKI-KENSU
           MOVE    SKT2520TMP-GENBK-KENSU      TO  WRK-Z5
           MOVE    WRK-Z5      TO  SKT2520-GENBK-KENSU
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
           IF  WRK-SYMD (1:6)  >=  WRK-CONS-SRYYM-200804
               MOVE   "SKT2520.red"    TO  SPRT-PRTID
           ELSE
               MOVE   "SKT2520o.red"   TO  SPRT-PRTID
           END-IF
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書(入院外用)"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書(入院外用)［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書(入院外用)［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書(入院外用)［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT2520             TO  SPRT-PRTDATA
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
      *    帳票編集<明細行>処理（公費）
      *****************************************************************
       323-BODY-HEN-SEC      SECTION.
      *
      *    公費の印字
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       2
                   OR      WRK-CNT >       20
                   OR      SKT2520TMP-KOHNUM(WRK-CNT) = SPACE
      *
      *        療養の給付：件数
               MOVE    SKT2520TMP-KKENSU(WRK-CNT)  TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT2520-KKENSU(IDX3)
      *
      *        療養の給付：日数
               MOVE    SKT2520TMP-KNISSU (WRK-CNT) TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT2520-KNISSU(IDX3)
      *
      *        療養の給付：点数
               MOVE    SKT2520TMP-KTENSU (WRK-CNT) TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT2520-KTENSU(IDX3)
      *
      *        療養の給付：一部負担金
               MOVE    SKT2520TMP-KITBFTN (WRK-CNT)  TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT2520-KITBFTN(IDX3)
      *
      *        公費名
               MOVE    SKT2520TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
               PERFORM 950-HKNNUM-SET-SEC
               MOVE    HKN-HBTNUM       TO  SKT2520-KNAME(IDX3)(1:2)
               IF  SKT2520TMP-KOHNAME (WRK-CNT)  NOT =  SPACE
                 MOVE  SKT2520TMP-KOHNAME (WRK-CNT)
                                        TO  SKT2520-KNAME(IDX3)(3:)
               ELSE
                 MOVE  HKN-TANSEIDONAME TO  SKT2520-KNAME(IDX3)(3:)
               END-IF
      *
               ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       323-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO  IDX-MAX
                               IDX2 
           MOVE    SPACE   TO  WRK-KOHNUM
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       20
               IF      SKT2520TMP-KOHNUM(IDX1) =   SPACE
                   MOVE    20      TO  IDX1
               ELSE
                   IF      WRK-KOHNUM          =   SPACE
                   OR      SKT2520TMP-KOHNUM(IDX1) <   WRK-KOHNUM
                       MOVE    IDX1            TO  IDX2
                       MOVE    SKT2520TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
                   END-IF
                   ADD     1       TO  IDX-MAX
               END-IF
           END-PERFORM
           IF      IDX2    >   ZERO
               MOVE    SKT2520TMP-KOH-TBL(IDX2)    TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "一般ソート前１" SKT2520TMP-KOHNUM(1)
           DISPLAY "一般ソート前２" SKT2520TMP-KOHNUM(2)
           DISPLAY "一般ソート前３" SKT2520TMP-KOHNUM(3)
      *
           IF      IDX-MAX     >   ZERO
               MOVE    2       TO  IDX1
               MOVE    1       TO  IDX2
               MOVE    1       TO  IDX3
               MOVE    "999"   TO  WRK-KOHNUM
               PERFORM UNTIL   IDX1    >   IDX-MAX
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       IDX-MAX
                       IF      (SKT2520TMP-KOHNUM(IDX2)
                                           >   SORT-KOHNUM(IDX1 - 1))
                       AND     (SKT2520TMP-KOHNUM(IDX2)
                                           <   WRK-KOHNUM)
                           MOVE    SKT2520TMP-KOHNUM(IDX2)
                                           TO  WRK-KOHNUM
                           MOVE    IDX2    TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    SKT2520TMP-KOH-TBL(IDX3)
                                       TO  SORT-KOH-TBL(IDX1)
                   MOVE    "999"       TO  WRK-KOHNUM
                   ADD     1           TO  IDX1
               END-PERFORM
           END-IF
      *
           INITIALIZE  SKT2520TMP-KOH-AREA
           MOVE    SORT-AREA   TO  SKT2520TMP-KOH-AREA
      *
           DISPLAY "一般ソート後１" SKT2520TMP-KOHNUM(1)
           DISPLAY "一般ソート後２" SKT2520TMP-KOHNUM(2)
           DISPLAY "一般ソート後３" SKT2520TMP-KOHNUM(3)
      *
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
                       MOVE  "*** SOKATU2520 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU2520 SEIKYU INVALID ***"
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
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
               MOVE    MCPDATA-REC  TO  HKNJAINF-REC
               MOVE    ZERO         TO  FLG-HKNJAINF
           ELSE
               INITIALIZE  HKNJAINF-REC
               MOVE    1   TO  FLG-HKNJAINF
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求額集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
      *
      *        計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF      SYU-KOH-HKNNUM(IDX)     =   WRK-KOHNUM
                   IF      IDX                 =   1
                       IF      SYU-SYUCOMPFTN-ENTANI
                                       >   SYU-KOH-COMPFTN-ENTANI(IDX)
                           MOVE    1   TO  FLG-KOH
                       END-IF
                   ELSE
                       IF      SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                       >   SYU-KOH-COMPFTN-ENTANI(IDX)
                           MOVE    1   TO  FLG-KOH
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *   収納情報セット
      *****************************************************************
       950-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  FLG-KOH
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
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key25"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_syunou"         TO  MCP-TABLE
             MOVE    "key25"              TO  MCP-PATHNAME
             PERFORM  925-SYUNOU-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN   NOT = "3"
                   PERFORM  800-SEIKYU-SEC
               END-IF
      *
               MOVE    "tbl_syunou"         TO  MCP-TABLE
               MOVE    "key25"              TO  MCP-PATHNAME
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key25"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       950-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
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
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE  LNK-PRTKANRI-SRYYM(1:6)   TO  HKN-TEKSTYMD(1:6)
           MOVE  "01"                      TO  HKN-TEKSTYMD(7:2)
           MOVE  HKN-TEKSTYMD              TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"         TO  MCP-TABLE
               MOVE    "key5"               TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
           ELSE
               DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
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
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
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
           MOVE    SPA-HOSPNUM          TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               EVALUATE    WRK-PARA-HKNJANUM(1:6)
                   WHEN    "010017"
                       MOVE    "014000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "014999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "040014"
                       MOVE    "044000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "044999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "120014"
                       MOVE    "124000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "124999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "140012"
                       MOVE    "144000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "144999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "140020"
                       MOVE    "145000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "145999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "230011"
                       MOVE    "234000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "234999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "260018"
                       MOVE    "264000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "264999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "270017"
                       MOVE    "274000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "274999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "275008"
                       MOVE    "275000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "275999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "280016"
                       MOVE    "284000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "284999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "340018"
                       MOVE    "344000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "344999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "400010"
                       MOVE    "404000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "404999"        TO  RECE10-XXEDHKNJANUM
                   WHEN    "400028"
                       MOVE    "405000"        TO  RECE10-XXSTHKNJANUM
                       MOVE    "405999"        TO  RECE10-XXEDHKNJANUM
                   WHEN OTHER
                       MOVE    WRK-PARA-HKNJANUM
                                               TO  RECE10-XXSTHKNJANUM
                                                   RECE10-XXEDHKNJANUM
               END-EVALUATE
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
      *        外来データのみ対象
               IF  RECE10-NYUGAIKBN NOT = "2"
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
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT2520TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT2520TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT2520TMP
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT2520TMP-FILE   NEXT
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
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD       TO  SYS-1900-EDYUKYMD
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
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD   TO  SYS-1901-EDYUKYMD
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
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD   TO  SYS-1902-EDYUKYMD
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
       900-DBDISCONNECT-EXT.
           EXIT.
