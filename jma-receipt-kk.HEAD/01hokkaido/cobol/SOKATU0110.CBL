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
       PROGRAM-ID.             SOKATU0110.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/18    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            返戻対応
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev   修正者	日付		内容
      *  01.00.01    有村   05/08/04    記載するデータが無い場合、
      *                                 ２枚目は印刷しないよう修正
      *  01.00.02    内田   06/11/15    食事がない場合でも生活療養費を印字
      *  01.00.03    内田   07/01/10    個別指定時、札幌市代表番号でも印字する
      *                                 在総診を別集計とする
      *  01.00.04    内田   07/05/23    長期上位対応
      *                                 医療機関情報取得条件修正
      *  01.00.05    内田   07/08/26    公費再掲欄にて保険番号190等を
      *                                 法別番号でまとめる
      *  01.00.06    内田   07/09/26    日レセver4.0対応
      *                                 システム管理マスタ「1901」の
      *                                 医療機関情報を参照
      *  01.00.07    内田   08/03/18    長期欄の割合を印字する
      *                                 長期欄の一部負担金は印字しない
      *  01.00.08    内田k  08/04/25    帳票様式の変更に伴う修正
      *                                 SPA-AREA初期化
      *  01.00.09    内田   08/05/25    長期欄の給付割合印字修正
      *  01.00.10    内田   08/06/25    公費再掲欄の給付割合で、前期高齢９割は
      *                                 ８割として扱う
      *  01.00.11    内田   08/07/08    札幌市代表保険者番号変更
      *                                 定数超過または標欠の場合、
      *                                 備考欄に「超過」「標欠」印字
      *  01.00.12    内田   08/08/01    入院基本情報のエラーメッセージ削除
      *  01.00.13    吉川   08/09/11    シス管「2007」の公費まとめ対応
      *  01.00.14    内田   09/05/12    後志広域連合対応
      *  01.00.15    吉川   09/11/17    シス管「2007」のまとめ公費で
      *                                 6つ目以降がまとまらない不具合を修正
      *  01.00.16    吉川   10/06/17    オンライン返戻対応
      *  01.00.17    吉川   11/02/09    特別療養費国保対応
      *  01.00.18    吉川   11/04/13    札幌市は区毎に集計するように修正
      *  01.00.19    吉川   12/12/13    医療機関情報編集修正
      *  01.00.20    吉川   13/03/27    プラグイン対応
      *  01.00.21    吉川   13/06/14    プログラムオプション対応
      *  01.00.22    萩野   13/12/05    在宅はまとめて集計するように修正
      *  01.00.23    吉川   14/05/20    前期高齢者２割対応
      *  01.00.24    萩野   14/08/07    返戻時、再印刷画面のタイトル修正
      *  01.00.25    吉川   14/10/02    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.26    門間   16/05/10    医療機関編集情報への開設者名追加対応
      *  01.00.27    吉川   16/11/16    公費番号まとめ情報の公費番号の追加
      *  01.00.28    吉川   17/02/10    医療機関コード変更時、
      *                                 医療機関コード別の作成処理対応
      *  01.00.29    吉川   18/06/11    SITEKBN変更（動作に変更なし）
      *  01.00.30    萩野   18/09/11    修正履歴コメント削除
      *                                 保険番号マスタの取得を「key2」から
      *                                 「key5」に修正
      *                                 法別番号でまとめる処理に障害、
      *                                 ひとり親を追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求書一時ファイル
           SELECT  SKT0110TMP-FILE ASSIGN    SKT0110TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT0110TMP-KEY
                                   FILE    STATUS  IS  STS-SKT0110.
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
       FD  SKT0110TMP-FILE.
           COPY    "CPSKT0110TMP.INC".
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
                                   BY         //SKT0110TP//.
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
           COPY    "SKT0110.INC".
           COPY    "SKT0120.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT0110         PIC X(02).
           03  STS-SKT0111         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT0110TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
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
           03  IDX-MAX             PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *##  パラメタ領域
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
           03  WRK-KOH-KEY.
               05  WRK-KOHNUM      PIC  X(03).
               05  WRK-RJN-19-KBN  PIC  X(03).
               05  WRK-RENNUM      PIC  9(02).
      *
           03  WRK-CNT               PIC  9(02).
           03  WRK-RATE              PIC  9(03).
           03  WRK-HKNJANUM          PIC  X(06).
           03  WRK-KOHNAME           PIC X(12).
      *
           03  WRK-Z1              PIC Z.
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *公費ソート用エリア
       01  SORT-AREA.
           03  SORT-KOH-TBL     OCCURS  40.
               05  SORT-KOH-KEY.
                   07  SORT-KOHNUM            PIC  X(03).
                   07  SORT-RJN-19-KBN        PIC  X(03).
                   07  SORT-RENNUM            PIC  9(02).
               05  SORT-K-RATE-TBL            OCCURS  2.
                   07  SORT-K-NYUGAI-TBL      OCCURS  2.
                       09  SORT-K-RATE        PIC  9(03).
                       09  SORT-K-KENSU       PIC  9(05).
                       09  SORT-K-NISSU       PIC  9(05).
                       09  SORT-K-TENSU       PIC  9(09).
                       09  SORT-K-ITBFTN      PIC  9(09).
                   07  SORT-K-SKENSU          PIC  9(05).
                   07  SORT-K-SNISSU          PIC  9(05).
                   07  SORT-K-SKINGK          PIC  9(09).
                   07  SORT-K-SFTNGK          PIC  9(09).
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
           03  CNT-KOH2            PIC 9(02).
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
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *　　保険組合せ情報
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    医療機関情報−入院基本
           COPY    "CPSK5000.INC".
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
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
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
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
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
           MOVE    "SOKATU0110"    TO  JOB-PGID
           MOVE    "国保請求書"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT0110"         TO  SKT0110TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT0110TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT0110TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT0110TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT0110TP-FULLNAME
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
      *    医療機関情報−入院基本
           INITIALIZE                  SYS-5000-REC
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-5000-STYUKYMD(1:6)
                                           SYS-5000-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5000-STYUKYMD(7:2)
                                           SYS-5000-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-5000-REC
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
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU0110"              TO  WRK-PRGID
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           DISPLAY  "200-MAIN-SEC START"
      *
           OPEN     OUTPUT  SKT0110TMP-FILE
           CLOSE            SKT0110TMP-FILE
           OPEN     I-O     SKT0110TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT0110TMP-FILE
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
           DISPLAY "*** SOKATU0110 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU0110 END ***"
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
           INITIALIZE       SKT0110TMP-REC
      *
           MOVE  RECE10-HKNJANUM-S       TO  WRK-HKNJANUM
      *
      *区の場合代表番号セット
      *    札幌市の場合、代表番号セット
           MOVE  WRK-HKNJANUM  TO  SKT0110TMP-HKNJANUM
      *
      *    出力順設定
           IF     WRK-YUSEN         =    SKT0110TMP-HKNJANUM
             MOVE        1          TO   SKT0110TMP-SYUTURYOKU
           ELSE
             IF     RECE10-PREFKBN  =    "1"
               MOVE      2          TO   SKT0110TMP-SYUTURYOKU
             ELSE
               MOVE      3          TO   SKT0110TMP-SYUTURYOKU
             END-IF
           END-IF
      *
           PERFORM  800-SKTTMP-READ-SEC
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
      *    2008年3月以前の保険でIDX1が1000番台は公費欄へ記載する
      *
      *    入院／外来
           IF  RECE10-NYUGAIKBN  =  "1"
             MOVE  1  TO  IDX2
           ELSE
             MOVE  2  TO  IDX2
           END-IF
      *
      *    老人かどうか
           IF  RECE10-RJNHKNNUM  NOT =  SPACE
             IF  RECE10-KYURATE = 9
               MOVE  1279     TO  IDX1
                      DISPLAY "月遅れ"IDX1
             ELSE
               MOVE  1277     TO  IDX1
                      DISPLAY "月遅れ"IDX1
             END-IF
             MOVE    1       TO  SKT0110TMP-PAGE-FLG
           ELSE
      *    一般か退職者か
             IF  RECE10-HKNNUM  =  "060" OR "068"
      *    年齢チェック
               IF  RECE10-AGE  >=  70
                 EVALUATE  RECE10-KYURATE
                   WHEN  9
                     IF  (RECE10-SRYYM  <  WRK-CONS-SRYYM-200804)
                        MOVE  1099    TO    IDX1
                        DISPLAY "月遅れ"IDX1
                     ELSE
                        MOVE   1      TO    IDX1
                     END-IF
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
                     MOVE  5     TO  IDX1
                 ELSE
                   IF  RECE10-AGE  <  3
                     MOVE    5    TO    IDX1
                   ELSE
                     IF  RECE10-KYURATE  =  7
                       MOVE  4    TO    IDX1
                     ELSE
                       MOVE  3    TO    IDX1
                       MOVE  RECE10-KYURATE  
                             TO  SKT0110TMP-KYURATE(IDX2)
                     END-IF
                   END-IF
                 END-IF
               END-IF
             ELSE
               IF  RECE10-AGE  >=  70
                   AND  RECE10-SRYYM  <  WRK-CONS-SRYYM-200804  
                 IF  RECE10-KYURATE  =  9
                   MOVE   1699     TO  IDX1
                      DISPLAY "月遅れ"IDX1
                 ELSE
                   MOVE   1677     TO  IDX1
                      DISPLAY "月遅れ"IDX1
                 END-IF
               ELSE
                 IF  RECE10-HONKZKKBN  =  "1"
                   MOVE   6    TO  IDX1
                 ELSE
                   IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                   AND  (RECE10-FLG (1)   =  1)
                       MOVE  8     TO  IDX1
                   ELSE
                     IF  RECE10-AGE    <    3
                         MOVE   8    TO    IDX1
                     ELSE
                         MOVE   7     TO    IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *    集計
           IF  IDX1  <  10
             PERFORM   2101-SYUKEI-SEC
           END-IF
      *
      *    公費併用
           IF  RECE10-KOHNUM (1) NOT = SPACE
               PERFORM  2202-KOH-SEC
             MOVE    1       TO  SKT0110TMP-PAGE-FLG
           END-IF
      *    月遅れ公費欄計上分
           IF   IDX1  >  10
                PERFORM  2203-KOH-SEC
                MOVE    1       TO  SKT0110TMP-PAGE-FLG
           END-IF
      *    長期
           IF  RECE10-ETC-KOHNUM(1)  =  "972"
            OR RECE10-ETC-KOHNUM(2)  =  "972"
            OR RECE10-ETC-KOHNUM(3)  =  "972"
            OR RECE10-ETC-KOHNUM(4)  =  "972"
            OR RECE10-ETC-KOHNUM(1)  =  "974"
            OR RECE10-ETC-KOHNUM(2)  =  "974"
            OR RECE10-ETC-KOHNUM(3)  =  "974"
            OR RECE10-ETC-KOHNUM(4)  =  "974"
             MOVE    1       TO  SKT0110TMP-PAGE-FLG
             PERFORM  22022-TYOKI-SEC
           END-IF
      *
      *    定数超過、標欠チェック
           IF  SYS-5000-TEISUKBN  =  "1"
           OR  (SYS-5000-HYOUKETUKBN  NOT =  SPACE  AND  "0")
             MOVE    1       TO  SKT0110TMP-PAGE-FLG
           END-IF
           IF      FLG-SKT0110TMP  =  1
                   WRITE    SKT0110TMP-REC
           ELSE
                   REWRITE  SKT0110TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *療養の給付
      *    件数
           ADD   1                   TO  SKT0110TMP-KENSU(IDX1 IDX2)
      *    日数
           ADD   RECE10-JNISSU(1)    TO  SKT0110TMP-NISSU(IDX1 IDX2)
      *    点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT0110TMP-TENSU(IDX1 IDX2)
      *    一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT0110TMP-ITBFTN(IDX1 IDX2)
      *食事療養
      *    件数
           IF  RECE10-SHOKUJINISSU(1)        >  ZERO
             ADD  1                         TO  SKT0110TMP-SKENSU(IDX1)
           END-IF
      *    日数
           ADD  RECE10-SHOKUJINISSU(1)      TO  SKT0110TMP-SNISSU(IDX1)
      *    金額
           ADD  RECE10-SHOKUJIRYOYOHI(1)    TO
                                    SKT0110TMP-SKINGK(IDX1)
      *    標準負担額
           ADD  RECE10-SHOKUJIFTN(1)        TO  SKT0110TMP-SFTNGK(IDX1)
      *
           MOVE    1               TO  FLG-UPD 
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2202-KOH-SEC              SECTION.
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
      *    法別番号でまとめる
      *    乳幼児
            EVALUATE  RECE10-KOHNUM(IDX3)
              WHEN  "290"
                MOVE  "190"       TO  RECE10-KOHNUM(IDX3)
              WHEN  "291"
                MOVE  "191"       TO  RECE10-KOHNUM(IDX3)
              WHEN  "292"
              WHEN  "392"
                MOVE  "192"       TO  RECE10-KOHNUM(IDX3)
            END-EVALUATE
      *
      *    障害
            EVALUATE  RECE10-KOHNUM(IDX3)
              WHEN  "245"
                MOVE  "145"       TO  RECE10-KOHNUM(IDX3)
              WHEN  "246"
                MOVE  "146"       TO  RECE10-KOHNUM(IDX3)
              WHEN  "247"
              WHEN  "347"
                MOVE  "147"       TO  RECE10-KOHNUM(IDX3)
            END-EVALUATE
      *
      *    ひとり親
            EVALUATE  RECE10-KOHNUM(IDX3)
              WHEN  "293"
                MOVE  "193"       TO  RECE10-KOHNUM(IDX3)
              WHEN  "294"
                MOVE  "194"       TO  RECE10-KOHNUM(IDX3)
              WHEN  "295"
              WHEN  "395"
                MOVE  "195"       TO  RECE10-KOHNUM(IDX3)
            END-EVALUATE
      *
      *
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
      *    負担割合
               IF  RECE10-KOHNUM(IDX3)  =  "019"  AND
                   RECE10-RJNHKNNUM NOT =  SPACE
                 MOVE     ZERO         TO  WRK-RATE
               ELSE
                 IF   (RECE10-SRYYM   >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (2)  =  1)
                   MOVE     2          TO  WRK-RATE
                 ELSE
                   COMPUTE  WRK-RATE      =  10  -  RECE10-KYURATE
                 END-IF
               END-IF
      *
               DISPLAY  "患者番号=" RECE10-PTNUM
               PERFORM  VARYING  IDX4  FROM  1   BY  1
                        UNTIL    IDX4  >     40
                   DISPLAY "IDX4=" IDX4
                 IF  SKT0110TMP-KOHNUM(IDX4)  =  SPACE
                   DISPLAY "TMPの公費番号スペース=" IDX4
                   MOVE  RECE10-RJNHKNNUM
                                         TO  SKT0110TMP-RJN-19-KBN(IDX4)
      *
                   MOVE  IDX4                TO  SKT0110TMP-RENNUM(IDX4)
                                                 CNT-KOH
                   MOVE  RECE10-KOHNUM(IDX3) TO  SKT0110TMP-KOHNUM(IDX4)
                   MOVE  1                   TO  CNT-KOH2
                   MOVE  WRK-RATE            TO
                                SKT0110TMP-K-RATE(CNT-KOH CNT-KOH2 IDX2)
                   MOVE  40                  TO  IDX4
                 ELSE
                   IF  RECE10-KOHNUM(IDX3)    =  SKT0110TMP-KOHNUM(IDX4)
                     DISPLAY "公費番号が同じ=" RECE10-KOHNUM(IDX3)
                             "患者側負担割合=" WRK-RATE
                             "データ側負担割合1=" 
                                     SKT0110TMP-K-RATE(CNT-KOH 1 IDX2) 
                             "データ側負担割合2=" 
                                     SKT0110TMP-K-RATE(CNT-KOH 2 IDX2)
                             "入外区分=" IDX2
                     IF  (RECE10-KOHNUM(IDX3) NOT =  "019")
                      OR (RECE10-RJNHKNNUM    =  
                                         SKT0110TMP-RJN-19-KBN(IDX4))
                       DISPLAY "019以外"
                               "WRK-RATE=" WRK-RATE
                               "SKT0110TMP-K-RATE1=" 
                                      SKT0110TMP-K-RATE(CNT-KOH  1 IDX2)
                               "SKT0110TMP-K-RATE2=" 
                                      SKT0110TMP-K-RATE(CNT-KOH  2 IDX2)
                       MOVE  IDX4            TO  CNT-KOH
                       IF  SKT0110TMP-K-RATE(CNT-KOH  1 IDX2) =
                                                    WRK-RATE OR ZERO
                         MOVE    1         TO  CNT-KOH2
                         MOVE    WRK-RATE  TO  
                                 SKT0110TMP-K-RATE(CNT-KOH 1 IDX2)
                         MOVE      40        TO  IDX4
                         DISPLAY "１つ目の給付割合と同じ"
                       ELSE
                         IF  SKT0110TMP-K-RATE(CNT-KOH  2 IDX2) =
                                                      WRK-RATE OR  ZERO
                           MOVE     2        TO  CNT-KOH2
                           MOVE   WRK-RATE  TO  
                                  SKT0110TMP-K-RATE(CNT-KOH 2 IDX2)
                           MOVE    40        TO  IDX4
                           DISPLAY "２つ目の給付割合と同じ"
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-PERFORM
      *
               DISPLAY "公費番号"  RECE10-KOHNUM(IDX3)
                       "連番号="   SKT0110TMP-RENNUM(CNT-KOH)
                       "入外区分=" RECE10-NYUGAIKBN
                       "負担割合=" WRK-RATE
      *    療養の給付：件数
               ADD   1  TO  SKT0110TMP-K-KENSU(CNT-KOH CNT-KOH2 IDX2)
      *    療養の給付:診療実日数
      *    老人＋１９＋外来の場合は実日数を記載しない
               IF  RECE10-KOHNUM(IDX3)   =  "019"   AND
                   RECE10-RJNHKNNUM  NOT =  SPACE   AND
                   RECE10-NYUGAIKBN      =  "2"
                 DISPLAY "老人＋１９＋外来の為、実日数を記載しない"
                         RECE10-PTNUM
               ELSE
                 ADD  RECE10-JNISSU(IDX3 + 1)    TO
                          SKT0110TMP-K-NISSU(CNT-KOH CNT-KOH2 IDX2)
               END-IF  
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                          SKT0110TMP-K-TENSU(CNT-KOH CNT-KOH2 IDX2)
      *
      *    療養の給付：一部負担金 
      *    原爆の場合は、主保険欄の一部負担金を記載する
               IF  RECE10-KOHNUM(IDX3)  =  "019"
                 ADD  RECE10-FTNMONEY(1)  TO
                          SKT0110TMP-K-ITBFTN(CNT-KOH CNT-KOH2 IDX2)
               ELSE
                 ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                          SKT0110TMP-K-ITBFTN(CNT-KOH CNT-KOH2 IDX2)
               END-IF
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO  SKT0110TMP-K-SKENSU(CNT-KOH CNT-KOH2)
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(IDX3 + 1)    TO
                               SKT0110TMP-K-SNISSU(CNT-KOH CNT-KOH2)
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT0110TMP-K-SKINGK(CNT-KOH CNT-KOH2)
      *    食事療養：標準負担額
               ADD  RECE10-SHOKUJIFTN(IDX3 + 1)      TO
                               SKT0110TMP-K-SFTNGK(CNT-KOH CNT-KOH2)
           END-PERFORM
      *
           .
       2202-KOH-EXT.
           EXIT.
      *
      *****************************************************************
      *    月遅れ編集処理
      *****************************************************************
       2203-KOH-SEC              SECTION.
      *
           DISPLAY "月遅れ2203-KOH-SEC"
      *    負担割合
               MOVE  IDX1(4:1)   TO  WRK-RATE(3:1)
               DISPLAY  "WRK-RATE"WRK-RATE
               DISPLAY  "患者番号=" RECE10-PTNUM
               PERFORM  VARYING  IDX4  FROM  1   BY  1
                        UNTIL    IDX4  >     40
                   DISPLAY "IDX4=" IDX4
                 IF  SKT0110TMP-KOHNUM(IDX4)  =  SPACE
                   DISPLAY "TMPの主保険番号スペース=" IDX4
                   MOVE  IDX4                TO  SKT0110TMP-RENNUM(IDX4)
                                                 CNT-KOH
                   MOVE  IDX1(2:3)           TO  SKT0110TMP-KOHNUM(IDX4)
                   DISPLAY "IDX1"IDX1
                   MOVE  1                   TO  CNT-KOH2
                   MOVE  WRK-RATE            TO
                                SKT0110TMP-K-RATE(CNT-KOH CNT-KOH2 IDX2)
                   MOVE  40                  TO  IDX4
                 ELSE
                   IF  IDX1(2:3)  =  SKT0110TMP-KOHNUM(IDX4)
                     DISPLAY "番号が同じ=" IDX1(2:2)
                             "患者側負担割合=" WRK-RATE
                             "データ側負担割合1=" 
                                     SKT0110TMP-K-RATE(CNT-KOH 1 IDX2) 
                             "データ側負担割合2=" 
                                     SKT0110TMP-K-RATE(CNT-KOH 2 IDX2)
                             "入外区分=" IDX2
                       MOVE  IDX4            TO  CNT-KOH
                       IF  SKT0110TMP-K-RATE(CNT-KOH  1 IDX2) =
                                                    WRK-RATE OR ZERO
                         MOVE    1         TO  CNT-KOH2
                         MOVE    WRK-RATE  TO  
                                 SKT0110TMP-K-RATE(CNT-KOH 1 IDX2)
                         MOVE      40        TO  IDX4
                         DISPLAY "１つ目の給付割合と同じ"
                       ELSE
                         IF  SKT0110TMP-K-RATE(CNT-KOH  2 IDX2) =
                                                      WRK-RATE OR  ZERO
                           MOVE     2        TO  CNT-KOH2
                           MOVE   WRK-RATE  TO  
                                  SKT0110TMP-K-RATE(CNT-KOH 2 IDX2)
                           MOVE    40        TO  IDX4
                           DISPLAY "２つ目の給付割合と同じ"
                         END-IF
                       END-IF
                   END-IF
                 END-IF
               END-PERFORM
      *
               DISPLAY "主保険番号"  IDX1(2:2)
                       "連番号="   SKT0110TMP-RENNUM(CNT-KOH)
                       "入外区分=" RECE10-NYUGAIKBN
                       "負担割合=" WRK-RATE
      *    療養の給付：件数
               ADD   1  TO  SKT0110TMP-K-KENSU(CNT-KOH CNT-KOH2 IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU(1)   TO
                        SKT0110TMP-K-NISSU(CNT-KOH CNT-KOH2 IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(1)  TO
                          SKT0110TMP-K-TENSU(CNT-KOH CNT-KOH2 IDX2)
      *
      *    療養の給付：一部負担金 
               ADD  RECE10-FTNMONEY(1)  TO
                          SKT0110TMP-K-ITBFTN(CNT-KOH CNT-KOH2 IDX2)
      *
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO  SKT0110TMP-K-SKENSU(CNT-KOH CNT-KOH2)
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(1)    TO
                               SKT0110TMP-K-SNISSU(CNT-KOH CNT-KOH2)
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(1)  TO
                               SKT0110TMP-K-SKINGK(CNT-KOH CNT-KOH2)
      *    食事療養：標準負担額
               ADD  RECE10-SHOKUJIFTN(1)      TO
                               SKT0110TMP-K-SFTNGK(CNT-KOH CNT-KOH2)
      *
           .
       2203-KOH-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期療養処理
      *****************************************************************
       22022-TYOKI-SEC         SECTION.
      *
      *    給付割合
           IF  RECE10-KYURATE  =  7
             MOVE  2  TO  IDX4
           ELSE
             MOVE  1  TO  IDX4
             EVALUATE  RECE10-KYURATE
               WHEN  9
                 IF   (RECE10-SRYYM   >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (2)  =  1)
                   MOVE  2              TO  SKT0110TMP-T-RATE (IDX4)
                 ELSE
                   MOVE  1              TO  SKT0110TMP-T-RATE (IDX4)
                 END-IF
               WHEN  8
                 MOVE  2                TO  SKT0110TMP-T-RATE (IDX4)
               WHEN  OTHER
                 MOVE  0                TO  SKT0110TMP-T-RATE (IDX4)
             END-EVALUATE
           END-IF
      *療養の給付
      *    件数
           ADD   1                   TO  SKT0110TMP-T-KENSU(IDX4)
      *    日数
           ADD   RECE10-JNISSU(1)    TO  SKT0110TMP-T-NISSU(IDX4)
      *    点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT0110TMP-T-TENSU(IDX4)
      *    一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT0110TMP-T-ITBFTN(IDX4)
      *
           .
       22022-TYOKI-EXT.
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
           OPEN     INPUT       SKT0110TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     1           TO       WRK-CNT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
             PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
             PERFORM  330-PRINT-OUT-SEC
      *    公費は残っているか
             MOVE     ZERO   TO   FLG-PASS
             MOVE     1      TO   WRK-CNT
      *
      *    公費ソート処理
             PERFORM  KOH-SORT-SEC
      *
             IF      SKT0110TMP-PAGE-FLG =   1
               PERFORM  UNTIL  SKT0110TMP-KOHNUM(WRK-CNT) = SPACE
                        AND    FLG-PASS  =  1
      *
      *            帳票編集<見出し>処理
                   PERFORM  311-HEAD-HEN-SEC
      *
                   IF  WRK-CNT  =  1
      *              帳票編集<明細>処理
                     PERFORM  321-BODY-HEN-SEC
                   END-IF
      *
      *            帳票編集<公費>処理
                   PERFORM  321-KOH-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  331-PRINT-OUT-SEC
                   MOVE  1  TO  FLG-PASS
               END-PERFORM
             END-IF 
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    SKT0110TMP-FILE
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
           ADD      1         TO        CNT-PAGE
      *
           INITIALIZE                             SKT0110
      *
      *    ページ番号
           MOVE    CNT-PAGE                  TO  WRK-Z4
           MOVE    WRK-Z4                    TO  SKT0110-NO
      *    診療年月
           MOVE     WRK-SRYYMWH              TO   SKT0110-SRYYM
      *    請求年月日
           MOVE     WRK-SEIYMDWH             TO   SKT0110-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS              TO  SKT0110-ADRS
           INSPECT SKT0110-ADRS              REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME              TO  SKT0110-HOSPNAME
           INSPECT SKT0110-HOSPNAME          REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME           TO   SKT0110-KAISETUNAME
      *    電話番号
           MOVE     WRK-HOSPTEL              TO   SKT0110-TEL
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD          TO   SKT0110-HOSPCD
      *    保険者番号
           INITIALIZE                       WRK-MAE-INPUT
           IF  SKT0110TMP-HKNJANUM  =  "014001"
             MOVE     "010017"               TO   WRK-MAE-INPUT
           ELSE
             MOVE     SKT0110TMP-HKNJANUM    TO   WRK-MAE-INPUT
           END-IF
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO   SKT0110-HKNJANUM
      *
      *    保険者名
           MOVE  SKT0110TMP-HKNJANUM  TO  WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
      *    後志広域連合かどうか
           IF  SKT0110TMP-HKNJANUM  =  "010835"
           OR  SKT0110TMP-HKNJANUM  =  "010850"
           OR  SKT0110TMP-HKNJANUM  =  "010868"
           OR  SKT0110TMP-HKNJANUM  =  "010876"
           OR  SKT0110TMP-HKNJANUM  =  "010884"
           OR  SKT0110TMP-HKNJANUM  =  "010892"
           OR  SKT0110TMP-HKNJANUM  =  "010900"
           OR  SKT0110TMP-HKNJANUM  =  "010918"
           OR  SKT0110TMP-HKNJANUM  =  "010926"
           OR  SKT0110TMP-HKNJANUM  =  "010934"
           OR  SKT0110TMP-HKNJANUM  =  "010959"
           OR  SKT0110TMP-HKNJANUM  =  "010967"
           OR  SKT0110TMP-HKNJANUM  =  "010975"
           OR  SKT0110TMP-HKNJANUM  =  "010983"
           OR  SKT0110TMP-HKNJANUM  =  "010999"
           OR  SKT0110TMP-HKNJANUM  =  "011015"
             STRING  "（"             DELIMITED  BY  SIZE
                     HKNJA-HKNJANAME  DELIMITED  BY  SPACE
                     "）"             DELIMITED  BY  SIZE
                     INTO            SKT0110-HKNJANAME
             END-STRING
             MOVE  "後志広域連合"    TO  SKT0110-HKNJANAME2
           ELSE
             MOVE  HKNJA-HKNJANAME   TO  SKT0110-HKNJANAME
           END-IF
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"              TO   SKT0110-TOKUBETU
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
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >    10
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL   IDX2  >     2
      *    件数
               MOVE     SKT0110TMP-KENSU (IDX1 IDX2)  TO   WRK-Z5
               MOVE     WRK-Z5       TO   SKT0110-KENSU (IDX1 IDX2)
      *    日数
               MOVE     SKT0110TMP-NISSU (IDX1 IDX2)  TO   WRK-Z5
               MOVE     WRK-Z5       TO   SKT0110-NISSU (IDX1 IDX2)
      *    点数
               MOVE     SKT0110TMP-TENSU (IDX1 IDX2)  TO   WRK-Z9
               MOVE     WRK-Z9       TO   SKT0110-TENSU (IDX1 IDX2)
      *    一部負担金
               MOVE     SKT0110TMP-ITBFTN (IDX1 IDX2) TO   WRK-Z9
               MOVE     WRK-Z9       TO   SKT0110-ITBFTN (IDX1 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE     SKT0110TMP-SKENSU (IDX1)     TO   WRK-Z5
             MOVE     WRK-Z5       TO   SKT0110-SKENSU (IDX1)
      *    食事療養：日数
             MOVE     SKT0110TMP-SNISSU (IDX1)     TO   WRK-Z5
             MOVE     WRK-Z5       TO   SKT0110-SNISSU (IDX1)
      *    食事療養：金額
             MOVE     SKT0110TMP-SKINGK (IDX1)     TO   WRK-Z9
             MOVE     WRK-Z9       TO   SKT0110-SKINGK (IDX1)
      *    食事療養：標準負担額
             MOVE     SKT0110TMP-SFTNGK (IDX1)     TO   WRK-Z9
             MOVE     WRK-Z9       TO   SKT0110-SFTNGK (IDX1)
           END-PERFORM
      *
      *    給付割合
           MOVE  SKT0110TMP-KYURATE (1) TO  WRK-Z2
           MOVE  WRK-Z2  TO  SKT0110-KYURATE (1)
           MOVE  SKT0110TMP-KYURATE (2) TO  WRK-Z2
           MOVE  WRK-Z2  TO  SKT0110-KYURATE (2)
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
           MOVE   "SKT0110.red"        TO  SPRT-PRTID
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
           MOVE    SKT0110             TO  SPRT-PRTDATA
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
      *    帳票編集<ヘッダー部>処理（老人）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD      1         TO        CNT-PAGE
      *
           INITIALIZE                             SKT0120
      *
      *    ページ番号
           MOVE    CNT-PAGE                  TO  WRK-Z4
           MOVE    WRK-Z4                    TO  SKT0120-NO
      *    診療年月
           MOVE     WRK-SRYYMWH              TO   SKT0120-SRYYM
      *    医療機関コード
           MOVE     SYS-1001-HOSPCD          TO   SKT0120-HOSPCD
      *    保険者番号
           INITIALIZE                       WRK-MAE-INPUT
           IF  SKT0110TMP-HKNJANUM  =  "014001"
             MOVE     "010017"               TO   WRK-MAE-INPUT
           ELSE
             MOVE     SKT0110TMP-HKNJANUM    TO   WRK-MAE-INPUT
           END-IF
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO   SKT0120-HKNJANUM
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"              TO   SKT0120-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（老人）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
           PERFORM  VARYING  IDX1  FROM 11  BY  1
                    UNTIL    IDX1  >    12
             COMPUTE         IDX3  =    IDX1  -  10
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                       UNTIL   IDX2  >     2
      *    件数
               MOVE     SKT0110TMP-KENSU (IDX1 IDX2)  TO   WRK-Z5
               MOVE     WRK-Z5       TO   SKT0120-KENSU (IDX3 IDX2)
      *    日数
               MOVE     SKT0110TMP-NISSU (IDX1 IDX2)  TO   WRK-Z5
               MOVE     WRK-Z5       TO   SKT0120-NISSU (IDX3 IDX2)
      *    点数
               MOVE     SKT0110TMP-TENSU (IDX1 IDX2)  TO   WRK-Z9
               MOVE     WRK-Z9       TO   SKT0120-TENSU (IDX3 IDX2)
      *    一部負担金
               MOVE     SKT0110TMP-ITBFTN (IDX1 IDX2) TO   WRK-Z9
               MOVE     WRK-Z9       TO   SKT0120-ITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE     SKT0110TMP-SKENSU (IDX1)     TO   WRK-Z5
             MOVE     WRK-Z5       TO   SKT0120-SKENSU (IDX3)
      *    食事療養：日数
             MOVE     SKT0110TMP-SNISSU (IDX1)     TO   WRK-Z5
             MOVE     WRK-Z5       TO   SKT0120-SNISSU (IDX3)
      *    食事療養：金額
             MOVE     SKT0110TMP-SKINGK (IDX1)     TO   WRK-Z9
             MOVE     WRK-Z9       TO   SKT0120-SKINGK (IDX3)
      *    食事療養：標準負担額
             MOVE     SKT0110TMP-SFTNGK (IDX1)     TO   WRK-Z9
             MOVE     WRK-Z9       TO   SKT0120-SFTNGK (IDX3)
           END-PERFORM
      *
      *    指定公費（道老）
      *    件数
           MOVE  SKT0110TMP-D-KENSU  TO  WRK-Z5
           MOVE  WRK-Z5  TO  SKT0120-D-KENSU
      *    日数
           MOVE  SKT0110TMP-D-NISSU  TO  WRK-Z5
           MOVE  WRK-Z5  TO  SKT0120-D-NISSU
      *    点数
           MOVE  SKT0110TMP-D-TENSU  TO  WRK-Z9
           MOVE  WRK-Z9  TO  SKT0120-D-TENSU
      *    一部負担金
           MOVE  SKT0110TMP-D-ITBFTN  TO  WRK-Z9
           MOVE  WRK-Z9  TO  SKT0120-D-ITBFTN
      *    食事療養：件数
           MOVE  SKT0110TMP-D-SKENSU  TO  WRK-Z5
           MOVE  WRK-Z5  TO  SKT0120-D-SKENSU
      *    食事療養：日数
           MOVE  SKT0110TMP-D-SNISSU  TO  WRK-Z5
           MOVE  WRK-Z5  TO  SKT0120-D-SNISSU
      *    食事療養：金額
           MOVE  SKT0110TMP-D-SKINGK  TO  WRK-Z9
           MOVE  WRK-Z9  TO  SKT0120-D-SKINGK
      *    食事療養：標準負担額
           MOVE  SKT0110TMP-D-SFTNGK  TO  WRK-Z9
           MOVE  WRK-Z9  TO  SKT0120-D-SFTNGK
      *
      *    指定公費（長期療養）
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     2
      *    割合
             IF  IDX1  =  1
               MOVE  SKT0110TMP-T-RATE (IDX1)   TO  WRK-Z2
               MOVE  WRK-Z2  TO  SKT0120-T-RATE (IDX1)
             END-IF
      *    件数
             MOVE  SKT0110TMP-T-KENSU (IDX1)   TO  WRK-Z5
             MOVE  WRK-Z5  TO  SKT0120-T-KENSU (IDX1)
      *    日数
             MOVE  SKT0110TMP-T-NISSU (IDX1)   TO  WRK-Z5
             MOVE  WRK-Z5  TO  SKT0120-T-NISSU (IDX1)
      *    点数
             MOVE  SKT0110TMP-T-TENSU (IDX1)   TO  WRK-Z9
             MOVE  WRK-Z9  TO  SKT0120-T-TENSU (IDX1)
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<公費>処理
      *****************************************************************
       321-KOH-HEN-SEC               SECTION.
      *
           DISPLAY  "321-KOH-HEN-SEC"
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     5
                    OR    WRK-CNT  >     40
                    OR    SKT0110TMP-KOHNUM (WRK-CNT) = SPACE
      *    公費番号
             IF  SKT0110TMP-KOHNUM(WRK-CNT)(1:2)  =  "09" OR "27" 
                                                  OR "67" OR "69"
               MOVE  SKT0110TMP-KOHNUM(WRK-CNT)
                                       TO  SKT0120-KOHNUM(IDX1)
             ELSE
               MOVE  SKT0110TMP-KOHNUM(WRK-CNT)    TO  WRK-KOHNUM
               PERFORM  950-HKNNUM-SET-SEC
               MOVE  HKN-HBTNUM        TO  SKT0120-KOHNUM(IDX1)
             END-IF
             MOVE  "◯"              TO  SKT0120-KOHMARU(IDX1)
      *    ◯原
             IF  SKT0110TMP-KOHNUM(WRK-CNT)     =  "019"  AND
                 SKT0110TMP-RJN-19-KBN(WRK-CNT) =  "027"
               MOVE  "原"            TO  SKT0120-GENBAKU1(IDX1)
               MOVE  "◯"            TO  SKT0120-GENBAKU2(IDX1)
             END-IF
      *
             IF  SKT0110TMP-KOHNUM(WRK-CNT)(1:2)  =  "27"
                 IF  SKT0110TMP-KOHNUM(WRK-CNT)(3:1)  =  9
                     MOVE  "九割"  TO  SKT0120-GENBAKU1(IDX1)
                 ELSE
                     MOVE  "七割"  TO  SKT0120-GENBAKU1(IDX1)
                 END-IF
             END-IF
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     2
      *    割合
                 DISPLAY "割合"SKT0110TMP-K-RATE(WRK-CNT IDX2 IDX3)
                 MOVE  SKT0110TMP-K-RATE(WRK-CNT IDX2 IDX3) TO WRK-Z1
                 MOVE  WRK-Z1  TO  SKT0120-K-RATE (IDX1 IDX2 IDX3)
      *    件数
                 MOVE  SKT0110TMP-K-KENSU(WRK-CNT IDX2 IDX3) TO WRK-Z5
                 MOVE  WRK-Z5  TO  SKT0120-K-KENSU (IDX1 IDX2 IDX3)
      *    日数
                 MOVE  SKT0110TMP-K-NISSU(WRK-CNT IDX2 IDX3) TO WRK-Z5
                 MOVE  WRK-Z5  TO  SKT0120-K-NISSU (IDX1 IDX2 IDX3)
      *    点数
                 MOVE  SKT0110TMP-K-TENSU (WRK-CNT IDX2 IDX3) TO WRK-Z9
                 MOVE  WRK-Z9  TO  SKT0120-K-TENSU (IDX1 IDX2 IDX3)
      *    一部負担金
                 MOVE  SKT0110TMP-K-ITBFTN(WRK-CNT IDX2 IDX3) TO WRK-Z9
                 MOVE  WRK-Z9  TO  SKT0120-K-ITBFTN (IDX1 IDX2 IDX3)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT0110TMP-K-SKENSU(WRK-CNT IDX2) TO WRK-Z5
               MOVE  WRK-Z5  TO  SKT0120-K-SKENSU (IDX1 IDX2)
      *    食事療養：日数
               MOVE  SKT0110TMP-K-SNISSU(WRK-CNT IDX2) TO WRK-Z5
               MOVE  WRK-Z5  TO  SKT0120-K-SNISSU (IDX1 IDX2)
      *    食事療養：金額
               MOVE  SKT0110TMP-K-SKINGK(WRK-CNT IDX2) TO WRK-Z9
               MOVE  WRK-Z9  TO  SKT0120-K-SKINGK (IDX1 IDX2)
      *    食事療養：標準負担額
               MOVE  SKT0110TMP-K-SFTNGK(WRK-CNT IDX2)  TO WRK-Z9
               MOVE  WRK-Z9  TO  SKT0120-K-SFTNGK (IDX1 IDX2)
             END-PERFORM
      *
             ADD   1  TO  WRK-CNT
           END-PERFORM
      *
      *    備考欄（定数超過、標欠）
           IF  SYS-5000-TEISUKBN  =  "1"
             MOVE  "超過"  TO  SKT0120-TYOKA
             MOVE  "○"    TO  SKT0120-TYOKA-MARU
           END-IF
           IF  SYS-5000-HYOUKETUKBN  NOT =  SPACE  AND  "0"
             MOVE  "標欠"  TO  SKT0120-HYOKETU
             MOVE  "○"    TO  SKT0120-HYOKETU-MARU
           END-IF
      *
           .
       321-KOH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（老人）
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
           MOVE   "SKT0120.red"        TO  SPRT-PRTID
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
           MOVE    SKT0120             TO  SPRT-PRTDATA
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
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
                                 IDX2 
           INITIALIZE  WRK-KOH-KEY
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  40
             IF  SKT0110TMP-KOHNUM(IDX1)  =  SPACE
               MOVE  40    TO    IDX1
             ELSE
               IF    WRK-KOHNUM                =   SPACE   OR
                     SKT0110TMP-KOH-KEY(IDX1)  <   WRK-KOH-KEY
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT0110TMP-KOH-KEY(IDX1) TO  WRK-KOH-KEY
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT0110TMP-KOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "一般ソート前１" SKT0110TMP-KOH-KEY(1)
           DISPLAY "一般ソート前２" SKT0110TMP-KOH-KEY(2)
           DISPLAY "一般ソート前３" SKT0110TMP-KOH-KEY(3)
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2           TO    IDX1
             MOVE    1           TO    IDX2
             MOVE    1           TO    IDX3
             MOVE    ALL   "9"     TO    WRK-KOH-KEY
      *
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT0110TMP-KOH-KEY(IDX2)
                                       > SORT-KOH-KEY(IDX1 - 1)
                 AND SKT0110TMP-KOH-KEY(IDX2)
                                       < WRK-KOH-KEY
                     MOVE  SKT0110TMP-KOH-KEY(IDX2)  TO WRK-KOH-KEY
                     MOVE  IDX2          TO    IDX3
                 END-IF
               END-PERFORM
               MOVE  SKT0110TMP-KOH-TBL(IDX3)  TO  SORT-KOH-TBL(IDX1)
      *
               MOVE  "9999" TO    WRK-KOH-KEY
      *
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT0110TMP-KOH-AREA
           MOVE  SORT-AREA  TO    SKT0110TMP-KOH-AREA
      *
           DISPLAY "一般ソート後１" SKT0110TMP-KOH-KEY(1)
           DISPLAY "一般ソート後２" SKT0110TMP-KOH-KEY(2)
           DISPLAY "一般ソート後３" SKT0110TMP-KOH-KEY(3)
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
                       MOVE  "*** SOKATU0110 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU0110 SEIKYU INVALID ***"
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
             MOVE    "tbl_hknjainf"       TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
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
      *    保険番号情報読込
      *****************************************************************
       945-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
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
      *
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
      *   保険情報セット(公費名取得)
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE  SPA-HOSPNUM                 TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM                  TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)   TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                      TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD              TO  HKN-TEKEDYMD
           MOVE  HKNNUM-REC                  TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknnum"        TO  MCP-TABLE
             MOVE    "key5"              TO  MCP-PATHNAME
             PERFORM  945-HKNNUM-READ-SEC
      *
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
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
             EVALUATE  WRK-PARA-HKNJANUM(1:3)
             WHEN "014"
               MOVE  "014000"           TO  RECE10-XXSTHKNJANUM
               MOVE  "014999"           TO  RECE10-XXEDHKNJANUM
             WHEN OTHER
               MOVE  WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                            RECE10-XXEDHKNJANUM
             END-EVALUATE
      *      札幌市代表番号でも印字
             IF  WRK-PARA-HKNJANUM  =  "010017"
               MOVE  "014000"           TO  RECE10-XXSTHKNJANUM
               MOVE  "014999"           TO  RECE10-XXEDHKNJANUM
             END-IF
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
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
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
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
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT0110TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT0110TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT0110TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT0110TMP-FILE   NEXT
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
