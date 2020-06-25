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
       PROGRAM-ID.             SOKATU4300.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/12/13    山口          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    07/09/09    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.02    内田    07/11/16    計欄は最終頁に総合計を印字し、
      *                                  それ以外は印字しない
      *  01.00.03    内田k   08/04/29    帳票様式の変更に伴う修正
      *                                  SPA-AREA初期化
      *  01.00.04    吉川    08/05/29    国組3〜6歳を分けないように修正
      *  01.00.05    吉川    08/06/17    月遅れ及び返戻処理の修正
      *  01.00.06    吉川    08/07/04    H20年4月改正前は旧様式で印字する
      *  01.00.07    門間    09/01/19    シス管「2007」公費まとめ対応
      *  01.00.08    吉川    10/01/07    シス管「2007」のまとめ公費で
      *                                  6つ目以降がまとまらない不具合を修正
      *  01.00.09    吉川    10/01/14    国保・退職者の総件数を印字する
      *  01.00.10    吉川    10/06/01    オンライン返戻対応
      *  01.00.11    吉川    10/12/16    open-cobol1.0対応
      *  01.00.12    吉川    11/02/07    ３併の場合、公費の件数が正しく
      *                                  カウントされない不具合を修正
      *  01.00.13    吉川    11/02/10    特別療養費国保対応
      *  01.00.14    吉川    11/09/05    SRYKA.INIがある他県のプログラムを
      *                                  インストールすると標榜科欄の
      *                                  表示がおかしくなるのを修正
      *  01.00.15    吉川    13/02/08    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.16    吉川    13/12/11    プログラムオプション対応
      *  01.00.17    吉川    14/05/26    前期高齢者２割対応
      *  01.00.18    吉川    14/10/28    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.19    吉川    15/03/10    返戻時、再印刷画面のタイトル修正
      *  01.00.20    萩野    16/03/11    医療機関編集情報への開設者名追加対応
      *  01.00.21    吉川    16/12/07    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  公費番号まとめ情報の公費番号の追加
      *  01.00.22    門間    18/03/01    SITEKBN変更（動作に変更なし）
      *  01.00.23    門間    18/09/05    不要コメントを削除
      *                                  医療機関情報の取得の修正
      *  01.00.24    井上    19/02/05    保険番号情報の取得処理を修正
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル
           SELECT  SKT4300TMP-FILE ASSIGN    SKT4300TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT4300TMP-KEY
                                   FILE    STATUS  IS  STS-SKT4300.
      *    公費一時ファイル
           SELECT  KOH4300TMP-FILE   ASSIGN    KOH4300TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  KOH4300TMP-KEY
                                   FILE    STATUS  IS  STS-KOH4300.
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
       FD  SKT4300TMP-FILE.
           COPY    "CPSKT4300TMP.INC".
      *
      *    請求書一時ファイル
       FD  KOH4300TMP-FILE.
           COPY    "CPKOH4300TMP.INC".
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
                                   BY         //SKT4300TP//.
      *
      *    公費一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //KOH4300TP//.
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
           COPY    "SKT4300.INC".
           COPY    "SKT4310.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT4300         PIC X(02).
           03  STS-KOH4300         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-SRYKA           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT4300TMP      PIC 9(01).
           03  FLG-KOH4300TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PRINT2          PIC 9(01).
           03  FLG-PRINT3          PIC 9(01).
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
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-KYURATE         PIC X(06).
           03  WRK-KOHNUM          PIC X(03).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z7              PIC ZZZZZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
      *    ファイル名称用エリア
           03  WRK-SRYKANAME    PIC X(16) OCCURS 2.
      *
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
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
      *    合計エリア
       01  SUM-AREA.
           03  SUM-HKN-TBL  OCCURS 11.
               05  SUM-NYUGAIKBN  OCCURS 2.
                   07  SUM-KENSU   PIC 9(04).
                   07  SUM-TENSU   PIC 9(07).
               05  SUM-SKINGK      PIC 9(07).
               05  SUM-SFTNGK      PIC 9(07).
           03  SUM-SSKENSU    PIC 9(04).
           03  SUM-SSKINGK    PIC 9(07).
           03  SUM-SSFTNGK    PIC 9(07).
           03  SUM-SOUKENSU-TBL  OCCURS 3.
               05  SUM-SOUKENSU    PIC 9(06).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
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
       01  KEY-AREA.
           03  KEY-NEW             PIC 9(06).
           03  KEY-OLD             PIC 9(06).
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
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
           MOVE    "SOKATU4300"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT4300"           TO  SKT4300TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT4300TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT4300TP-HOSPNUM
      *
           MOVE    "KOH4300"           TO  KOH4300TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  KOH4300TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  KOH4300TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT4300TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  KOH4300TP-BASENAME      TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (3)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (4)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT4300TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  KOH4300TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (4)  TO  RECEERR
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
           PERFORM 900-SEIKYU-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU4300"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    診療科
           MOVE    "SRYKA"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  2
             IF  WRK-OPTION(IDX)(1:2)  =  "※"
               MOVE    SPACE                 TO  WRK-SRYKANAME(IDX)
             ELSE
               MOVE    WRK-OPTION(IDX)(1:16) TO  WRK-SRYKANAME(IDX)
             END-IF
           END-PERFORM
           DISPLAY "診療科１="  WRK-SRYKANAME(1)
           DISPLAY "診療科２="  WRK-SRYKANAME(2)
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
           MOVE    ZERO                      TO  IDY
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
           OPEN     OUTPUT  SKT4300TMP-FILE
           CLOSE            SKT4300TMP-FILE
           OPEN     I-O     SKT4300TMP-FILE
      *
           OPEN     OUTPUT  KOH4300TMP-FILE
           CLOSE            KOH4300TMP-FILE
           OPEN     I-O     KOH4300TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT4300TMP-FILE
           CLOSE   KOH4300TMP-FILE
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
           DISPLAY "*** SOKATU4300 IN  "  CNT-RECE10
           DISPLAY "*** SOKATU4300 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
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
           INITIALIZE       SKT4300TMP-REC
           INITIALIZE       SKT4300TMP-HKNJANUM
      *
           IF  RECE10-HKNJANUM(7:2)  =   SPACE
               MOVE  RECE10-HKNJANUM (1:6)   TO
                     WRK-HKNJANUM(1:6)
           ELSE
               MOVE  RECE10-HKNJANUM (3:6)   TO
                     WRK-HKNJANUM(1:6)
           END-IF
           MOVE  WRK-HKNJANUM         TO    SKT4300TMP-HKNJANUM
      *
      *    国保組合法定外給付判定
           IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
               IF   (WRK-HKNJANUM(3:1)    =   "3" )  AND
                    (RECE10-RJNHKNNUM     = SPACE )  AND
                    (RECE10-FLG (1)   NOT =    1  )  AND
                    (RECE10-AGE           <    70 )  AND
                    (RECE10-KYURATE   NOT =    7  )
                 MOVE  RECE10-KYURATE     TO    SKT4300TMP-KYURATE
               END-IF
           ELSE
               IF   (WRK-HKNJANUM(3:1)    =   "3" )  AND
                    (RECE10-RJNHKNNUM     = SPACE )  AND
                    (RECE10-AGE           >    2     AND
                                          <    70 )  AND
                    (RECE10-KYURATE     NOT =  7  )
                 MOVE  RECE10-KYURATE     TO    SKT4300TMP-KYURATE
               END-IF
           END-IF
           IF  RECE10-SRYYM  <  WRK-CONS-SRYYM-200804
               MOVE  200803      TO  SKT4300TMP-SRYYM
               MOVE  1           TO  SKT4300TMP-KBN
           ELSE
               MOVE   ZERO       TO  SKT4300TMP-SRYYM
               MOVE   2          TO  SKT4300TMP-KBN
           END-IF
           MOVE  RECE10-SKYYM        TO  SKT4300TMP-SKYYM
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
           IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                PERFORM  220-HKN-SYUKEI-SEC
           ELSE
                PERFORM  210-HKN-SYUKEI-SEC
           END-IF
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
                 IF  RECE10-AGE  >= 3
                   MOVE    3     TO    IDX1
                 ELSE
                   MOVE    4     TO    IDX1
                 END-IF
               END-IF
      *
             ELSE
               IF  RECE10-AGE  >=  70
                 IF  RECE10-KYURATE  =  9
                   MOVE  6    TO  IDX1
                 ELSE
                   MOVE  7    TO  IDX1
                 END-IF
               ELSE
                 IF  RECE10-HONKZKKBN  =  1
                   MOVE  5    TO   IDX1
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
      *
           PERFORM  2101-SYUKEI-SEC
      *
      *    公費負担分
           IF  RECE10-KOHNUM (1)   NOT =   SPACE  OR
               RECE10-KOHNUM (2)   NOT =   SPACE  OR
               RECE10-KOHNUM (3)   NOT =   SPACE  OR
               RECE10-KOHNUM (4)   NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
           IF  FLG-SKT4300TMP  =  1
               WRITE    SKT4300TMP-REC
           ELSE
               REWRITE  SKT4300TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険分集計処理
      *****************************************************************
       220-HKN-SYUKEI-SEC          SECTION.
      *
      *    入外区分
           IF  RECE10-NYUGAIKBN  =  "1"
             MOVE  1      TO   IDX2
           ELSE
             MOVE  2      TO   IDX2
           END-IF
      *
      *    退職者かどうか
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
                   MOVE  4     TO  IDX1
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
               IF  RECE10-HONKZKKBN  =  1
                 MOVE  5    TO   IDX1
               ELSE
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                     MOVE  7     TO  IDX1
                 ELSE
                   IF  RECE10-AGE  <  3
                     MOVE  7  TO  IDX1
                   ELSE
                     MOVE  6  TO  IDX1
                   END-IF
                 END-IF
               END-IF
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
      *    公費負担分
           IF  RECE10-KOHNUM (1)   NOT =   SPACE  OR
               RECE10-KOHNUM (2)   NOT =   SPACE  OR
               RECE10-KOHNUM (3)   NOT =   SPACE  OR
               RECE10-KOHNUM (4)   NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
           IF  FLG-SKT4300TMP  =  1
               WRITE    SKT4300TMP-REC
           ELSE
               REWRITE  SKT4300TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       220-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・一般国保分集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                  TO  SKT4300TMP-KENSU(IDX1 IDX2)
      *
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO  SKT4300TMP-TENSU(IDX1 IDX2)
      *
      *    食事療養：件数
           IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
             IF      RECE10-SHOKUJINISSU(1)  NOT =   ZERO
                 ADD     1                 TO  SKT4300TMP-SSKENSU
             END-IF
      *      食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(1) TO  SKT4300TMP-SSKINGK
      *
      *      食事療養：標準負担額
             ADD  RECE10-SHOKUJIFTN(1)     TO  SKT4300TMP-SSFTNGK
           ELSE
      *      食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(1) TO  SKT4300TMP-SKINGK(IDX1)
      *
      *      食事療養：標準負担額
             ADD  RECE10-SHOKUJIFTN(1)     TO  SKT4300TMP-SFTNGK(IDX1)
           END-IF
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
             INITIALIZE       KOH4300TMP-REC
      *
             IF  RECE10-SRYYM  <  WRK-CONS-SRYYM-200804
                 MOVE  200803      TO  KOH4300TMP-SRYYM
                 MOVE  1           TO  KOH4300TMP-KBN
             ELSE
               MOVE   ZERO         TO  KOH4300TMP-SRYYM
               MOVE    2           TO  KOH4300TMP-KBN
             END-IF
      *
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
      *      公費番号まとめ情報があるとき
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
                     MOVE  20            TO  IDZ2
                     MOVE  WRK-2007-MAX  TO  IDZ1
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
      *
      *    キーセット
             MOVE  RECE10-KOHNUM (IDX3)  TO  KOH4300TMP-KOHNUM
      *
             PERFORM  900-KOHTMP-READ-SEC
      *
      *    療養の給付：件数
               ADD   1  TO  KOH4300TMP-KENSU(IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)        TO
                                 KOH4300TMP-TENSU(IDX2)
               IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
      *    食事療養：件数
                 IF   RECE10-SHOKUJINISSU(IDX3 + 1)  NOT =   ZERO
                     ADD     1           TO  KOH4300TMP-SKENSU
                 END-IF
               END-IF
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                                 KOH4300TMP-SKINGK
      *    食事療養：標準負担額
               ADD  RECE10-SHOKUJIFTN(IDX3 + 1)      TO
                                 KOH4300TMP-SFTNGK
      *
             IF  FLG-KOH4300TMP  =  1
               WRITE    KOH4300TMP-REC
             ELSE
               REWRITE  KOH4300TMP-REC
             END-IF
           END-PERFORM
      *
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
           OPEN     INPUT       SKT4300TMP-FILE
           OPEN     INPUT       KOH4300TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     ZERO        TO       FLG-PRINT2
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
           PERFORM  950-KOHTMPFILE-READ-SEC
      *
           IF  SKT4300TMP-SRYYM  NOT  =  SPACE
               MOVE  SKT4300TMP-SRYYM    TO  WRK-SYMD (1:6)
           ELSE
               MOVE  LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           END-IF
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
                    AND       FLG-PRINT2  =  1
      *
      *      新様式か
             IF  SKT4300TMP-KBN  NOT =  1
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 330-BODY-HEN-SEC
      *        帳票編集<公費>
               PERFORM 331-KOH-HEN-SEC
               IF   FLG-PRINT   =  1  AND  FLG-PRINT2  =  1
      *        帳票編集<小計>処理
                 PERFORM 3213-SHOKEI-HEN-SEC
               END-IF
      *        帳票印刷処理
               PERFORM 331-PRINT-OUT-SEC
      *
      *      旧様式
             ELSE
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
               IF   FLG-PRINT   =  1  AND  FLG-PRINT2  =  1
      *        帳票編集<小計>処理
                 PERFORM 321-BODY-HEN-SEC
               END-IF
      *        帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
             END-IF
      *
           END-PERFORM
           CLOSE    SKT4300TMP-FILE
           CLOSE    KOH4300TMP-FILE
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
           INITIALIZE                        SKT4300
           INITIALIZE                        SKT4310
      *    診療年月

           IF  SKT4300TMP-SRYYM  NOT =  ZERO
               MOVE  SKT4300TMP-SRYYM    TO  WRK-SYMD (1:6)
           ELSE
               MOVE  LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           END-IF
           MOVE    WRK-SRYYMWH           TO  SKT4300-SRYYM
                                             SKT4310-SRYYM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT4300-HOSPCDN
                                             SKT4310-HOSPCDN
      *    郵便番号
           MOVE    SYS-1002-POST         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT4300-POST
                                             SKT4310-POST
      *
      *    標榜科名
           MOVE    WRK-SRYKANAME(1)      TO  SKT4300-SRYKANAME(1)
                                             SKT4310-SRYKANAME(1)
           MOVE    WRK-SRYKANAME(2)      TO  SKT4300-SRYKANAME(2)
                                             SKT4310-SRYKANAME(2)
      *    No
           ADD     1                     TO  CNT-PAGE
           MOVE    CNT-PAGE              TO  WRK-Z3
           MOVE    WRK-Z3                TO  SKT4300-NO
           IF  SKT4300TMP-KBN  NOT =  1
             CONTINUE
           ELSE
             ADD     1                     TO  CNT-PAGE
             MOVE    CNT-PAGE              TO  WRK-Z3
             MOVE    WRK-Z3                TO  SKT4310-NO
           END-IF
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT4300-ADRS
                                             SKT4310-ADRS
           INSPECT SKT4300-ADRS          REPLACING ALL "  " BY "　"
           INSPECT SKT4310-ADRS          REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT4300-HOSPNAME
                                             SKT4310-HOSPNAME
           INSPECT SKT4300-HOSPNAME      REPLACING ALL "  " BY "　"
           INSPECT SKT4310-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT4300-KAISETU
                                             SKT4310-KAISETU
      *    電話番号
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT4300-TEL
                                             SKT4310-TEL
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT4300-TOKUBETU
           END-IF
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
           INITIALIZE                       SUM-AREA
      *
           MOVE     SKT4300TMP-SRYYM   TO   KEY-NEW
      *    月遅れ・返戻分
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     8
                    OR       SKT4300TMP-KBN  NOT =  1
                    OR       FLG-PRINT   =      1
      *
      *        保険者名
               MOVE     SKT4300TMP-HKNJANUM     TO  WRK-HKNJANUM
               PERFORM  920-HKNJAINF-SET-SEC
               MOVE     WRK-HKNJANAME     TO  SKT4300-HKNJANAME(IDX1)
                                            SKT4310-HKNJANAME(IDX1)
      *      給付割合(法定外分のみ)
               IF       SKT4300TMP-KYURATE  NOT  =  ZERO
                 MOVE   SKT4300TMP-KYURATE      TO  WRK-Z2
                 INITIALIZE                   WRK-KYURATE
                 STRING "(" WRK-Z2 "割"  ")"  DELIMITED BY SIZE
                        INTO                  WRK-KYURATE
                 END-STRING
                 MOVE  WRK-KYURATE      TO    SKT4300-KYURATE(IDX1)
                                              SKT4310-KYURATE(IDX1)
               END-IF
      *
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     11
                 IF     IDX2     <     7
                   PERFORM  3201-BODY-HEN-SEC
                 ELSE
                   PERFORM  3202-BODY-HEN-SEC
                 END-IF
               END-PERFORM
      *        一時ファイル読み込み
               MOVE     KEY-NEW  TO  KEY-OLD
               PERFORM  900-TMP-READ-SEC
               MOVE     SKT4300TMP-SRYYM  TO  KEY-NEW
      *
           END-PERFORM
      *    公費分編集(１枚目)
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     6
                    OR       KOH4300TMP-KBN  NOT = 1
                    OR       FLG-PRINT2  =      1
      *
      *    法別番号
             MOVE   KOH4300TMP-KOHNUM     TO     WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE   HKN-HBTNUM            TO     SKT4300-HBTNUM(IDX1)
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    公費：件数
               MOVE  KOH4300TMP-KENSU(IDX2)  TO  WRK-Z4
               MOVE  WRK-Z4         TO  SKT4300-K-KENSU(IDX1 IDX2)
      *    公費：点数
               MOVE  KOH4300TMP-TENSU(IDX2)  TO  WRK-Z7
               MOVE  WRK-Z7         TO  SKT4300-K-TENSU(IDX1 IDX2)
             END-PERFORM
      *    公費：食事療養費
             MOVE  KOH4300TMP-SKINGK         TO  WRK-Z7
             MOVE  WRK-Z7           TO  SKT4300-K-SKINGK(IDX1)
      *    公費：標準負担額
             MOVE  KOH4300TMP-SFTNGK         TO  WRK-Z7
             MOVE  WRK-Z7           TO  SKT4300-K-SFTNGK(IDX1)
      *
      *    公費一時ファイル読み込み
             PERFORM  950-KOHTMPFILE-READ-SEC
           END-PERFORM
      *
      *    公費分編集(２枚目)
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     6
                    OR       KOH4300TMP-KBN  NOT =  1
                    OR       FLG-PRINT2  =      1
      *
      *    法別番号
             MOVE   KOH4300TMP-KOHNUM     TO     WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE   HKN-HBTNUM            TO     SKT4310-HBTNUM(IDX1)
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    公費：件数
               MOVE  KOH4300TMP-KENSU(IDX2)  TO  WRK-Z4
               MOVE  WRK-Z4         TO  SKT4310-K-KENSU(IDX1 IDX2)
      *    公費：点数
               MOVE  KOH4300TMP-TENSU(IDX2)  TO  WRK-Z7
               MOVE  WRK-Z7         TO  SKT4310-K-TENSU(IDX1 IDX2)
             END-PERFORM
      *    公費：食事療養費
             MOVE  KOH4300TMP-SKINGK         TO  WRK-Z7
             MOVE  WRK-Z7           TO  SKT4310-K-SKINGK(IDX1)
      *    公費：標準負担額
             MOVE  KOH4300TMP-SFTNGK         TO  WRK-Z7
             MOVE  WRK-Z7           TO  SKT4310-K-SFTNGK(IDX1)
      *
      *    公費一時ファイル読み込み
             PERFORM  950-KOHTMPFILE-READ-SEC
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １枚目編集処理
      *****************************************************************
       3201-BODY-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     2
      *    件数
             MOVE    SKT4300TMP-KENSU(IDX2 IDX3)  TO  WRK-Z4
             MOVE    WRK-Z4     TO    SKT4300-KENSU(IDX1 IDX2 IDX3)
      *    点数
             MOVE    SKT4300TMP-TENSU(IDX2 IDX3)  TO  WRK-Z7
             MOVE    WRK-Z7     TO    SKT4300-TENSU(IDX1 IDX2 IDX3)
           END-PERFORM
      *
      *    食事療養費
           MOVE      SKT4300TMP-SKINGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4300-SKINGK(IDX1 IDX2)
      *    標準負担額
           MOVE      SKT4300TMP-SFTNGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4300-SFTNGK(IDX1 IDX2)
      *
      *    小計加算(ページ計)
           ADD       SKT4300TMP-KENSU(IDX2 1)   TO  SUM-KENSU(IDX2 1)
           ADD       SKT4300TMP-KENSU(IDX2 2)   TO  SUM-KENSU(IDX2 2)
           ADD       SKT4300TMP-TENSU(IDX2 1)   TO  SUM-TENSU(IDX2 1)
           ADD       SKT4300TMP-TENSU(IDX2 2)   TO  SUM-TENSU(IDX2 2)
           ADD       SKT4300TMP-SKINGK(IDX2)    TO  SUM-SKINGK(IDX2)
           ADD       SKT4300TMP-SFTNGK(IDX2)    TO  SUM-SFTNGK(IDX2)
      *
           .
       3201-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２枚目編集処理
      *****************************************************************
       3202-BODY-HEN-SEC      SECTION.
      *
           COMPUTE  IDX4     =     IDX2  -      6
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     2
      *    件数
             MOVE    SKT4300TMP-KENSU(IDX2 IDX3)  TO  WRK-Z4
             MOVE    WRK-Z4     TO    SKT4310-KENSU(IDX1 IDX4 IDX3)
      *    点数
             MOVE    SKT4300TMP-TENSU(IDX2 IDX3)  TO  WRK-Z7
             MOVE    WRK-Z7     TO    SKT4310-TENSU(IDX1 IDX4 IDX3)
           END-PERFORM
      *
      *    食事療養費
           MOVE      SKT4300TMP-SKINGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4310-SKINGK(IDX1 IDX4)
      *    標準負担額
           MOVE      SKT4300TMP-SFTNGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4310-SFTNGK(IDX1 IDX4)
      *
      *    小計加算(ページ計)
           ADD       SKT4300TMP-KENSU(IDX2 1)   TO  SUM-KENSU(IDX2 1)
           ADD       SKT4300TMP-KENSU(IDX2 2)   TO  SUM-KENSU(IDX2 2)
           ADD       SKT4300TMP-TENSU(IDX2 1)   TO  SUM-TENSU(IDX2 1)
           ADD       SKT4300TMP-TENSU(IDX2 2)   TO  SUM-TENSU(IDX2 2)
           ADD       SKT4300TMP-SKINGK(IDX2)    TO  SUM-SKINGK(IDX2)
           ADD       SKT4300TMP-SFTNGK(IDX2)    TO  SUM-SFTNGK(IDX2)
      *
           .
       3202-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       330-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     8
                    OR       FLG-PRINT   =      1
      *
      *    保険者名
             MOVE     SKT4300TMP-HKNJANUM     TO  WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME     TO  SKT4300-HKNJANAME(IDX1)
                                            SKT4310-HKNJANAME(IDX1)
      *    給付割合(法定外分のみ)
             IF       SKT4300TMP-KYURATE  NOT  =  ZERO
               MOVE   SKT4300TMP-KYURATE      TO  WRK-Z2
               INITIALIZE                   WRK-KYURATE
               STRING "(" WRK-Z2 "割"  ")"  DELIMITED BY SIZE
                      INTO                  WRK-KYURATE
               END-STRING
               MOVE  WRK-KYURATE      TO    SKT4300-KYURATE(IDX1)
                                            SKT4310-KYURATE(IDX1)
             END-IF
      *
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     7
                 PERFORM  3203-BODY-HEN-SEC
             END-PERFORM
      *    一時ファイル読み込み
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           .
       330-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    新１枚目編集処理
      *****************************************************************
       3203-BODY-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     2
      *    件数
             MOVE    SKT4300TMP-KENSU(IDX2 IDX3)  TO  WRK-Z4
             MOVE    WRK-Z4     TO    SKT4300-KENSU(IDX1 IDX2 IDX3)
      *    点数
             MOVE    SKT4300TMP-TENSU(IDX2 IDX3)  TO  WRK-Z7
             MOVE    WRK-Z7     TO    SKT4300-TENSU(IDX1 IDX2 IDX3)
           END-PERFORM
      *
           IF  IDX2 =  7
      *      食事件数
             MOVE      SKT4300TMP-SSKENSU      TO  WRK-Z4
             MOVE      WRK-Z4     TO    SKT4300-SSKENSU(IDX1)
      *      食事療養費
             MOVE      SKT4300TMP-SSKINGK      TO  WRK-Z7
             MOVE      WRK-Z7     TO    SKT4300-SSKINGK(IDX1)
      *      標準負担額
             MOVE      SKT4300TMP-SSFTNGK      TO  WRK-Z7
             MOVE      WRK-Z7     TO    SKT4300-SSFTNGK(IDX1)
      *    小計加算(ページ計)
             ADD       SKT4300TMP-SSKENSU   TO  SUM-SSKENSU
             ADD       SKT4300TMP-SSKINGK   TO  SUM-SSKINGK
             ADD       SKT4300TMP-SSFTNGK   TO  SUM-SSFTNGK
           END-IF
      *
      *    小計加算(ページ計)
           ADD       SKT4300TMP-KENSU(IDX2 1)   TO  SUM-KENSU(IDX2 1)
           ADD       SKT4300TMP-KENSU(IDX2 2)   TO  SUM-KENSU(IDX2 2)
           ADD       SKT4300TMP-TENSU(IDX2 1)   TO  SUM-TENSU(IDX2 1)
           ADD       SKT4300TMP-TENSU(IDX2 2)   TO  SUM-TENSU(IDX2 2)
      *
      *    総件数
           IF  IDX2  <  5
      *      国保
             ADD     SKT4300TMP-KENSU(IDX2 1)   TO  SUM-SOUKENSU(1)
             ADD     SKT4300TMP-KENSU(IDX2 2)   TO  SUM-SOUKENSU(1)
           ELSE
      *      退職
             ADD     SKT4300TMP-KENSU(IDX2 1)   TO  SUM-SOUKENSU(2)
             ADD     SKT4300TMP-KENSU(IDX2 2)   TO  SUM-SOUKENSU(2)
           END-IF
      *    国保＋退職
           ADD       SKT4300TMP-KENSU(IDX2 1)   TO  SUM-SOUKENSU(3)
           ADD       SKT4300TMP-KENSU(IDX2 2)   TO  SUM-SOUKENSU(3)
      *
           .
       3203-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理
      *****************************************************************
       331-KOH-HEN-SEC      SECTION.
      *
      *    公費分編集
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     6
                    OR       FLG-PRINT2  =      1
      *
      *    法別番号
             MOVE   KOH4300TMP-KOHNUM     TO     WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE   HKN-HBTNUM          TO     SKT4300-HBTNUM(IDX1)
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    公費：件数
               MOVE  KOH4300TMP-KENSU(IDX2)  TO  WRK-Z4
               MOVE  WRK-Z4         TO  SKT4300-K-KENSU(IDX1 IDX2)
      *    公費：点数
               MOVE  KOH4300TMP-TENSU(IDX2)  TO  WRK-Z7
               MOVE  WRK-Z7         TO  SKT4300-K-TENSU(IDX1 IDX2)
             END-PERFORM
      *    公費：食事療養費
             MOVE  KOH4300TMP-SKINGK         TO  WRK-Z7
             MOVE  WRK-Z7           TO  SKT4300-K-SKINGK(IDX1)
      *    公費：標準負担額
             MOVE  KOH4300TMP-SFTNGK         TO  WRK-Z7
             MOVE  WRK-Z7           TO  SKT4300-K-SFTNGK(IDX1)
      *
      *    公費一時ファイル読み込み
             PERFORM  950-KOHTMPFILE-READ-SEC
           END-PERFORM
      *
           .
       331-KOH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理
      *****************************************************************
       332-KOH-HEN-SEC      SECTION.
      *
      *    公費分編集
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     12
                    OR       FLG-PRINT2  =      1
      *
             IF  IDX1 <  6
      *        法別番号
               MOVE   KOH4300TMP-KOHNUM     TO     WRK-KOHNUM
               PERFORM  950-HKNNUM-SET-SEC
               MOVE   HKN-HBTNUM          TO     SKT4300-HBTNUM(IDX1)
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *      公費：件数
                 MOVE  KOH4300TMP-KENSU(IDX2)  TO  WRK-Z4
                 MOVE  WRK-Z4         TO  SKT4300-K-KENSU(IDX1 IDX2)
      *      公費：点数
                 MOVE  KOH4300TMP-TENSU(IDX2)  TO  WRK-Z7
                 MOVE  WRK-Z7         TO  SKT4300-K-TENSU(IDX1 IDX2)
               END-PERFORM
      *      公費：食事療養費
               MOVE  KOH4300TMP-SKINGK         TO  WRK-Z7
               MOVE  WRK-Z7           TO  SKT4300-K-SKINGK(IDX1)
      *      公費：標準負担額
               MOVE  KOH4300TMP-SFTNGK         TO  WRK-Z7
               MOVE  WRK-Z7           TO  SKT4300-K-SFTNGK(IDX1)
      *
             ELSE
      *        法別番号
               MOVE   KOH4300TMP-KOHNUM     TO     WRK-KOHNUM
               PERFORM  950-HKNNUM-SET-SEC
               MOVE   HKN-HBTNUM            TO     SKT4310-HBTNUM(IDX1)
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *      公費：件数
                 MOVE  KOH4300TMP-KENSU(IDX2)  TO  WRK-Z4
                 MOVE  WRK-Z4         TO  SKT4310-K-KENSU(IDX1 IDX2)
      *      公費：点数
                 MOVE  KOH4300TMP-TENSU(IDX2)  TO  WRK-Z7
                 MOVE  WRK-Z7         TO  SKT4310-K-TENSU(IDX1 IDX2)
               END-PERFORM
      *      公費：食事療養費
               MOVE  KOH4300TMP-SKINGK         TO  WRK-Z7
               MOVE  WRK-Z7           TO  SKT4310-K-SKINGK(IDX1)
      *      公費：標準負担額
               MOVE  KOH4300TMP-SFTNGK         TO  WRK-Z7
               MOVE  WRK-Z7           TO  SKT4310-K-SFTNGK(IDX1)
      *
             END-IF
      *
      *      公費一時ファイル読み込み
               PERFORM  950-KOHTMPFILE-READ-SEC
               IF  WRK-SYMD  NOT  =  KOH4300TMP-SRYYM
                   MOVE  1  TO FLG-PRINT2
               END-IF
      *
           END-PERFORM
      *
           .
       332-KOH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<小計>処理
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     11
             IF     IDX2     <     7
               PERFORM  3211-BODY-HEN-SEC
             ELSE
               PERFORM  3212-BODY-HEN-SEC
             END-IF
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １枚目小計編集処理
      *****************************************************************
       3211-BODY-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     2
      *    件数
             MOVE    SUM-KENSU(IDX2 IDX3)  TO  WRK-Z4
             MOVE    WRK-Z4     TO    SKT4300-G-KENSU(IDX2 IDX3)
      *    点数
             MOVE    SUM-TENSU(IDX2 IDX3)  TO  WRK-Z7
             MOVE    WRK-Z7     TO    SKT4300-G-TENSU(IDX2 IDX3)
           END-PERFORM
      *
      *    食事療養費
           MOVE      SUM-SKINGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4300-G-SKINGK(IDX2)
      *    標準負担額
           MOVE      SUM-SFTNGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4300-G-SFTNGK(IDX2)
      *
      *
           .
       3211-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２枚目小計編集処理
      *****************************************************************
       3212-BODY-HEN-SEC      SECTION.
      *
           COMPUTE  IDX4     =     IDX2  -      6
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     2
      *    件数
             MOVE    SUM-KENSU(IDX2 IDX3)  TO  WRK-Z4
             MOVE    WRK-Z4     TO    SKT4310-G-KENSU(IDX4 IDX3)
      *    点数
             MOVE    SUM-TENSU(IDX2 IDX3)  TO  WRK-Z7
             MOVE    WRK-Z7     TO    SKT4310-G-TENSU(IDX4 IDX3)
           END-PERFORM
      *
      *    食事療養費
           MOVE      SUM-SKINGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4310-G-SKINGK(IDX4)
      *    標準負担額
           MOVE      SUM-SFTNGK(IDX2)      TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4310-G-SFTNGK(IDX4)
      *
      *
           .
       3212-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    小計編集処理
      *****************************************************************
       3213-SHOKEI-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     7
             PERFORM  VARYING  IDX3  FROM  1  BY  1
                      UNTIL    IDX3  >     2
      *      件数
               MOVE    SUM-KENSU(IDX2 IDX3)  TO  WRK-Z4
               MOVE    WRK-Z4     TO    SKT4300-G-KENSU(IDX2 IDX3)
      *      点数
               MOVE    SUM-TENSU(IDX2 IDX3)  TO  WRK-Z7
               MOVE    WRK-Z7     TO    SKT4300-G-TENSU(IDX2 IDX3)
             END-PERFORM
      *
           END-PERFORM
      *
      *    食事件数
           MOVE      SUM-SSKENSU     TO  WRK-Z4
           MOVE      WRK-Z4     TO    SKT4300-G-SSKENSU
      *    食事療養費
           MOVE      SUM-SSKINGK     TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4300-G-SSKINGK
      *    標準負担額
           MOVE      SUM-SSFTNGK     TO  WRK-Z7
           MOVE      WRK-Z7     TO    SKT4300-G-SSFTNGK
      *
      *    総件数
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >     3
              MOVE   SUM-SOUKENSU(IDX4)  TO  WRK-Z7
              MOVE   WRK-Z7              TO  SKT4300-SOUKENSU(IDX4)
           END-PERFORM
      *
           .
       3213-SHOKEI-HEN-EXT.
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
           MOVE   "SKT4300o.red"            TO  SPRT-PRTID
           MOVE    "国保総括表"        TO  SPRT-TITLE
           MOVE    SKT4300             TO  SPRT-PRTDATA
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
           MOVE   "SKT4310o.red"            TO  SPRT-PRTID
           MOVE    "国保総括表"        TO  SPRT-TITLE
           MOVE    SKT4310             TO  SPRT-PRTDATA
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
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
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
           MOVE   "SKT4300.red"            TO  SPRT-PRTID
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
           MOVE    SKT4300             TO  SPRT-PRTDATA
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
      *
           .
       331-PRINT-OUT-EXT.
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
                       MOVE  "*** SOKATU4300 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY
                   MOVE  "*** SOKATU4300 SEIKYU INVALID ***"
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
      *  保険者情報　取得
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM            TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM           TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"       TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
             MOVE  HKNJA-HKNJANAME   TO  WRK-HKNJANAME
      *
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
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
           CALL    "ORCDBMAIN"              USING
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
           MOVE  SPA-HOSPNUM           TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM            TO  HKN-HKNNUM
           MOVE  LNK-PRTKANRI-SRYYM(1:6)   TO  HKN-TEKSTYMD(1:6)
           MOVE  "01"                      TO  HKN-TEKSTYMD(7:2)
           MOVE  HKN-TEKSTYMD              TO  HKN-TEKEDYMD
           MOVE  HKNNUM-REC            TO  MCPDATA-REC
           MOVE  "DBSELECT"            TO  MCP-FUNC
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
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
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
           MOVE    SPACE                 TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"            TO  RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000                TO  RECE10-SRYYM
             MOVE  999999                TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
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
           MOVE    WRK-PATH              TO  MCP-PATH
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"          TO  MCP-FUNC
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
           READ    SKT4300TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT4300TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT4300TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計ファイル読込
      *****************************************************************
       900-KOHTMP-READ-SEC             SECTION.
      *
           READ    KOH4300TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-KOH4300TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-KOH4300TMP
           END-READ
           .
       900-KOHTMP-READ-EXT.
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
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
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
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
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
           READ  SKT4300TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
                   MOVE  1  TO  FLG-PRINT3
           END-READ
           DISPLAY "FLG-PRINT"FLG-PRINT
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費一時ファイル読込
      *****************************************************************
       950-KOHTMPFILE-READ-SEC                SECTION.
      *
           READ  KOH4300TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT2
           END-READ
      *
           .
       950-KOHTMPFILE-READ-EXT.
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
