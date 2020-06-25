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
       PROGRAM-ID.             SOKATU1900.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療報酬請求総括表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/07/09    有村          仕様変更による新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01       有村     04/10/12  最終ページには総合計を印字するよう修正
      *  01.00.02       有村     05/02/22  diaからmonpeに変更
      *                                    OpenCOBOLバージョン変更対応
      *  01.00.03       山口     05/04/18  ORCAのバージョンアップ(2.5.0)対応
      *  01.00.04       有村     05/05/06  医療機関情報取得条件修正
      *  01.00.05       内田     07/03/27  返戻対応
      *  01.00.06       内田     07/08/05  日レセver4.0対応
      *                                    システム管理マスタ「1901」の
      *                                    医療機関情報を参照
      *  01.00.07       内田     08/05/03  ４月改正に伴う修正
      *  01.00.08       吉川     08/05/27  帳票様式の変更に伴う修正
      *  01.00.09       内田     08/06/05  国保がない場合でも印刷
      *                 吉川     08/06/06  後期高齢は保険者番号を印字しない
      *                                    改正前の月遅れ分は旧様式に印字
      *                                    ページ番号を印字
      *  01.00.10       門間     08/12/11  SPA-AREA初期化
      *  01.00.15       吉川     09/06/30  改正後の返戻分が旧様式で
      *                                    印字される不具合を修正
      *  01.00.16       吉川     10/10/01  オンライン返戻対応
      *                                    特別療養費国保対応
      *  01.00.17       吉川     11/03/10  open-cobol1.0対応
      *  01.00.18       吉川     13/01/09  医療機関情報編集対応
      *  01.00.19       吉川     14/05/19  前期高齢者２割対応
      *  01.00.20       吉川     14/08/06  返戻時、再印刷画面のタイトル修正
      *  01.00.21       吉川     14/10/23  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.22       門間     16/05/12  医療機関編集情報への開設者名追加対応
      *  01.00.23       門間     17/07/11  医療機関コード変更時、
      *                                    医療機関コード別の作成処理対応
      *  01.00.24       吉川     17/10/16  SITEKBN変更
      *  01.00.25       萩野     18/05/08  記述漏れ修正
      *  01.00.26       井上     18/11/12  修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT1900TMP-FILE ASSIGN    SKT1900TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT1900TMP-KEY
                                   FILE    STATUS  IS  STS-SKT1900.
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
       FD  SKT1900TMP-FILE.
           COPY    "CPSKT1900TMP.INC".
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
                                   BY         //SKT1900TP//.
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
           COPY    "SKT1900.INC".
           COPY    "SKT1910.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT1900         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT1900TMP      PIC 9(01).
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
           03  FLG-KOUKI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
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
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-MAX1            PIC 9(03).
           03  WRK-MAX2            PIC 9(03).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13             PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-HKNJANAME       PIC X(20).
      *
           03  WRK-RECEERR         PIC X(200).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2             PIC X(50).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-KENNUM            PIC 9(02).
           03  WRK-KENNAME           PIC X(10).
           03  WRK-KOHKEI            PIC 9(03).
           03  WRK-KOHKEI2           PIC 9(03).
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
      *    合計エリア
       01  SUM-AREA.
           03  SUM-SGOKEI-AREA.
               05  SUM-SGKANSYO-TBL OCCURS 9.
                   07  SUM-NYUGAI-SKJ-TBL OCCURS 3.
                       09  SUM-GKENSU         PIC 9(06).
                       09  SUM-GNISSU         PIC 9(06).
                       09  SUM-GTENSU         PIC 9(10).
      *    一般・老人
           03  SUM-SGOKEI-AREA2.
               05  SUM-SGKANSYO-TBL2 OCCURS 9.
                   07  SUM-NYUGAI-SKJ-TBL2 OCCURS 3.
                       09  SUM-GKENSU2         PIC 9(06).
                       09  SUM-GNISSU2         PIC 9(06).
                       09  SUM-GTENSU2         PIC 9(10).
      *    退職
           03  SUM-SGOKEI-AREA3.
               05  SUM-SGKANSYO-TBL3 OCCURS 5.
                   07  SUM-NYUGAI-SKJ-TBL3 OCCURS 3.
                       09  SUM-GKENSU3         PIC 9(06).
                       09  SUM-GNISSU3         PIC 9(06).
                       09  SUM-GTENSU3         PIC 9(10).
      *    旧様式
           03  SUM-SGOKEI-AREA4.
               05  SUM-SGKANSYO-TBL4 OCCURS 6.
                   07  SUM-NYUGAI-SKJ-TBL4 OCCURS 3.
                       09  SUM-GKENSU4         PIC 9(06).
                       09  SUM-GNISSU4         PIC 9(06).
                       09  SUM-GTENSU4         PIC 9(10).
      *
           COPY    "CPSHELLTBL.INC".
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
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *    請求管理情報
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
           MOVE    "SOKATU1900"    TO  JOB-PGID
           MOVE    "国保総括表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
      *##（地方公費仕様：LNK-PRTKANRI-TERMID)
           MOVE    "SKT1900"           TO  SKT1900TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT1900TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT1900TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
           MOVE  SKT1900TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT1900TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *###（地方公費仕様)
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
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
           OPEN    OUTPUT  UPD-FILE
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT1900TMP-FILE
           CLOSE            SKT1900TMP-FILE
           OPEN     I-O     SKT1900TMP-FILE
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
           IF  WRK-PARA-TOKUBETU  =  "1"
             MOVE     ZERO    TO        FLG-END
             MOVE     1       TO        FLG-KOUKI
             MOVE    "039"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           ELSE
             MOVE     1       TO        FLG-KOUKI
             MOVE     ZERO    TO        FLG-END
             MOVE    "040"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           END-IF
      *
           CLOSE   UPD-FILE
           CLOSE   SKT1900TMP-FILE
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
           DISPLAY "*** SOKATU1900 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU1900 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
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
           INITIALIZE       SKT1900TMP-REC
           INITIALIZE       SKT1900TMP-HKNJANUM
      *
           IF       FLG-KOUKI   =   1
             MOVE  RECE10-HKNJANUM(1:4)    TO  SKT1900TMP-HKNJANUM(1:4)
           ELSE
             IF  RECE10-HKNJANUM(7:2)  =  SPACE
               MOVE  RECE10-HKNJANUM(1:6)  TO  SKT1900TMP-HKNJANUM(1:6)
             ELSE
               MOVE  RECE10-HKNJANUM(3:6)  TO  SKT1900TMP-HKNJANUM(1:6)
             END-IF
           END-IF
      *    県外保険者→県内市町村→県内国保組合→県内扱い国保組合
             IF     RECE10-PREFKBN   =    "2"
               MOVE      1          TO   SKT1900TMP-SYUTURYOKU
             ELSE
               IF   SKT1900TMP-HKNJANUM(3:1) NOT  =    "3"
                 MOVE    2          TO   SKT1900TMP-SYUTURYOKU
               ELSE
                 IF  RECE10-PREFNUM               =    "19"
                   MOVE    3          TO   SKT1900TMP-SYUTURYOKU
                 ELSE
                   MOVE    4          TO   SKT1900TMP-SYUTURYOKU
                 END-IF
               END-IF
             END-IF
           IF  FLG-KOUKI   =   1
             MOVE    9                TO   SKT1900TMP-SYUTURYOKU
           END-IF
           IF    RECE10-SRYYM          >=  WRK-CONS-SRYYM-200804
      *    新様式
             MOVE          1          TO  SKT1900TMP-TYOHYOKBN
           ELSE
             IF  RECE10-RJNHKNNUM  NOT  =  SPACE   OR
                 RECE10-HKNNUM          =  "060" OR "068"
      *    旧様式
               MOVE        2          TO  SKT1900TMP-TYOHYOKBN
             ELSE
      *    旧様式(退職)
               MOVE        3          TO  SKT1900TMP-TYOHYOKBN
             END-IF
           END-IF
      *    国保組合法定外給付判定
           IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
             IF   SKT1900TMP-HKNJANUM(3:1)  =  "3"    AND
                  RECE10-RJNHKNNUM          =  SPACE  AND
                  RECE10-FLG(1)             =  ZERO   AND
                  RECE10-AGE                <  70     AND
                  RECE10-KYURATE        NOT =  7
               MOVE    RECE10-KYURATE      TO  SKT1900TMP-KYURATE
             END-IF
           ELSE
             IF   SKT1900TMP-HKNJANUM(3:1)  =  "3"    AND
                  RECE10-RJNHKNNUM          =  SPACE  AND
                  RECE10-AGE                >  2      AND
                  RECE10-AGE                <  70     AND
                  RECE10-KYURATE        NOT =  7
               MOVE    RECE10-KYURATE      TO  SKT1900TMP-KYURATE
             END-IF
           END-IF
      *    平成２０年３月以前は旧様式
           IF  RECE10-SRYYM  <  WRK-CONS-SRYYM-200804
               MOVE  RECE10-SRYYM   TO  WRK-SRYYMD(1:6)
               MOVE  "01"           TO  WRK-SRYYMD(6:2)
           ELSE
               MOVE  SPACE          TO  WRK-SRYYMD
           END-IF
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
           PERFORM  210-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
               IF  FLG-KOUKI  =  ZERO
                  PERFORM   900-SEIKYU-READ-SEC
               ELSE
                  PERFORM   950-SEIKYU-READ-SEC
               END-IF
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
      *
      *    老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
             IF  RECE10-KYURATE  =  9
               MOVE  5   TO   IDX1
             ELSE
               MOVE  6   TO   IDX1
             END-IF
           ELSE
      *    後期高齢化どうか
             IF  FLG-KOUKI  NOT  =  ZERO
               IF  RECE10-KYURATE  =  9
                 MOVE  8   TO   IDX1
               ELSE
                 MOVE  9   TO   IDX1
               END-IF
             ELSE
      *    退職者かどうか
               IF  RECE10-HKNNUM  =  "060" OR "068"
                 IF  RECE10-AGE  >=  70
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
                       MOVE  3    TO  IDX1
                     ELSE
                       MOVE  4    TO  IDX1
                     END-IF
                   END-IF
                 END-IF
               ELSE
                 IF  RECE10-AGE  >=  70
                   IF  RECE10-KYURATE  =  9
                     MOVE  2    TO  IDX1
                   ELSE
                     MOVE  3    TO  IDX1
                   END-IF
                 ELSE
                   IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                   AND  (RECE10-FLG (1)   =  1)
                     MOVE  7     TO  IDX1
                   ELSE
                    IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
                      IF  RECE10-HONKZKKBN = "1"
                         MOVE  5  TO  IDX1
                      ELSE
                         MOVE  6  TO  IDX1
                      END-IF
                    ELSE  
                     IF  RECE10-AGE >= 3
                       IF  RECE10-HONKZKKBN = "1"
                         MOVE  1  TO  IDX1
                       ELSE
                         MOVE  4  TO  IDX1
                       END-IF
                     ELSE
                       MOVE    5  TO  IDX1
                     END-IF
                    END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
           IF  FLG-SKT1900TMP  =  1
               WRITE    SKT1900TMP-REC
           ELSE
               REWRITE  SKT1900TMP-REC
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
      *    入外区分
           IF  RECE10-NYUGAIKBN  =  "1"
             MOVE  1      TO   IDX2
           ELSE
             MOVE  3      TO   IDX2
           END-IF
      *
      *    療養の給付：件数
           ADD   1                  TO  SKT1900TMP-KENSU (IDX1 IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(1)    TO  SKT1900TMP-NISSU(IDX1 IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO  SKT1900TMP-TENSU(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養:件数
             ADD   1                TO  SKT1900TMP-KENSU(IDX1 2)
           END-IF
      *    食事療養:日数
             ADD  RECE10-SHOKUJINISSU(1)
                                    TO  SKT1900TMP-NISSU(IDX1 2)
      *    食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(1)
                                    TO SKT1900TMP-TENSU(IDX1 2)
      *
           MOVE    1               TO  FLG-UPD 
           .
       2101-SYUKEI-EXT.
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
           INITIALIZE           SKT1900TMP-REC
           OPEN     INPUT       SKT1900TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           INITIALIZE    SUM-SGOKEI-AREA2
           INITIALIZE    SUM-SGOKEI-AREA3
           INITIALIZE    SUM-SGOKEI-AREA4
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    帳票判定
             IF    SKT1900TMP-TYOHYOKBN   =   1
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
             ELSE
              IF    SKT1900TMP-TYOHYOKBN   =   2
      *    帳票編集<見出し>処理
               PERFORM  312-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  322-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  332-PRINT-OUT-SEC
              ELSE
      *    帳票編集<見出し>処理
               PERFORM  311-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  321-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  331-PRINT-OUT-SEC
              END-IF
             END-IF
           END-PERFORM
      *
           CLOSE    SKT1900TMP-FILE
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
           INITIALIZE                    SKT1900
           MOVE  LNK-PRTKANRI-SRYYM      TO  WRK-SYMD (1:6)
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT1900-SRYYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT1900-ADRS
           INSPECT SKT1900-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT1900-HOSPNAME
           INSPECT SKT1900-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT1900-KAISETUNAME
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT1900-HOSPCDN
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT1900-NO
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT1900-TOKUBETU
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
      *    編集処理
      *
           INITIALIZE    SUM-SGOKEI-AREA
           MOVE  10  TO  WRK-MAX1
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     WRK-MAX1 OR
                    FLG-PRINT      =     1  OR
                    SKT1900TMP-TYOHYOKBN    NOT  =  1
      *    保険者名
             IF  SKT1900TMP-SYUTURYOKU  =  9
               PERFORM  400-KENNAME-SEC
               STRING WRK-KENNAME "広域連合"   DELIMITED BY SPACE
                      INTO                     SKT1900-HKNJANAME(IDX1)
               END-STRING
               MOVE   SPACE                TO  WRK-HKNJANUM
             ELSE
               MOVE   SKT1900TMP-HKNJANUM  TO  WRK-HKNJANUM
               PERFORM  920-HKNJAINF-SET-SEC
               MOVE   WRK-HKNJANAME        TO  SKT1900-HKNJANAME(IDX1)
             END-IF
      *    保険者番号
           IF  WRK-SYMD (1:6)   >=  WRK-CONS-SRYYM-200804
               MOVE  WRK-HKNJANUM          TO  SKT1900-HKNJANUM(IDX1)
           END-IF
      *    給付割合
             IF       SKT1900TMP-KYURATE    NOT  =  ZERO
               MOVE  SKT1900TMP-KYURATE     TO  WRK-Z2
               STRING    "("  WRK-Z2 "割"  ")"  DELIMITED BY SIZE
                         INTO                   SKT1900-KYURATE(IDX1) 
               END-STRING
             END-IF
      *
             MOVE  9  TO  WRK-MAX2
      *
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >    WRK-MAX2
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SKT1900TMP-KENSU(IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5     TO  SKT1900-KENSU(IDX1 IDX2 IDX3)
      *    日数
                 MOVE  SKT1900TMP-NISSU(IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5     TO  SKT1900-NISSU(IDX1 IDX2 IDX3)
      *    点数・金額
                 MOVE  SKT1900TMP-TENSU(IDX2 IDX3)  TO  WRK-Z9
                 MOVE  WRK-Z9     TO  SKT1900-TENSU(IDX1 IDX2 IDX3)
      *    ページ計に加算
                 ADD   SKT1900TMP-KENSU(IDX2 IDX3)
                                  TO  SUM-GKENSU(IDX2 IDX3)
                 ADD   SKT1900TMP-NISSU(IDX2 IDX3)
                                  TO  SUM-GNISSU(IDX2 IDX3)
                 ADD   SKT1900TMP-TENSU(IDX2 IDX3)
                                  TO  SUM-GTENSU(IDX2 IDX3)
      *    総合計に加算
      *    ページ計に加算
                 ADD   SKT1900TMP-KENSU(IDX2 IDX3)
                                  TO  SUM-GKENSU2(IDX2 IDX3)
                 ADD   SKT1900TMP-NISSU(IDX2 IDX3)
                                  TO  SUM-GNISSU2(IDX2 IDX3)
                 ADD   SKT1900TMP-TENSU(IDX2 IDX3)
                                  TO  SUM-GTENSU2(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           IF  FLG-PRINT                 =  1  OR  
               SKT1900TMP-TYOHYOKBN NOT  =  1
      *    総合計編集
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     WRK-MAX2
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SUM-GKENSU2(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SUM-GNISSU2(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GNISSU(IDX2 IDX3)
      *    点数・金額
                 MOVE  SUM-GTENSU2(IDX2 IDX3)   TO   WRK-Z10
                 MOVE  WRK-Z10  TO  SKT1900-GTENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
           ELSE
      *    ページ計編集
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     WRK-MAX2
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SUM-GKENSU(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SUM-GNISSU(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GNISSU(IDX2 IDX3)
      *    点数・金額
                 MOVE  SUM-GTENSU(IDX2 IDX3)   TO   WRK-Z10
                 MOVE  WRK-Z10  TO  SKT1900-GTENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
           END-IF
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理(国保・老人)
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
           MOVE    "SKT1900.red"       TO  SPRT-PRTID
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
           MOVE    SKT1900             TO  SPRT-PRTDATA
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
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT1910
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT1910-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
      *
           INITIALIZE    SUM-SGOKEI-AREA
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     7  OR
                    FLG-PRINT      =     1  
      *    保険者名
             IF  SKT1900TMP-SYUTURYOKU  =  9
               PERFORM  400-KENNAME-SEC
               MOVE  WRK-KENNAME           TO  SKT1910-HKNJANAME(IDX1)
             ELSE
               MOVE  SKT1900TMP-HKNJANUM   TO  WRK-HKNJANUM
               PERFORM  920-HKNJAINF-SET-SEC
               MOVE  WRK-HKNJANAME         TO  SKT1910-HKNJANAME(IDX1)
             END-IF
      *
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SKT1900TMP-KENSU(IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5     TO  SKT1910-KENSU(IDX1 IDX2 IDX3)
      *    日数
                 MOVE  SKT1900TMP-NISSU(IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5     TO  SKT1910-NISSU(IDX1 IDX2 IDX3)
      *    点数・金額
                 MOVE  SKT1900TMP-TENSU(IDX2 IDX3)  TO  WRK-Z9
                 MOVE  WRK-Z9     TO  SKT1910-TENSU(IDX1 IDX2 IDX3)
      *    ページ計に加算
                 ADD   SKT1900TMP-KENSU(IDX2 IDX3)
                                  TO  SUM-GKENSU(IDX2 IDX3)
                 ADD   SKT1900TMP-NISSU(IDX2 IDX3)
                                  TO  SUM-GNISSU(IDX2 IDX3)
                 ADD   SKT1900TMP-TENSU(IDX2 IDX3)
                                  TO  SUM-GTENSU(IDX2 IDX3)
      *    総合計に加算
                 ADD   SKT1900TMP-KENSU(IDX2 IDX3)
                                  TO  SUM-GKENSU3(IDX2 IDX3)
                 ADD   SKT1900TMP-NISSU(IDX2 IDX3)
                                  TO  SUM-GNISSU3(IDX2 IDX3)
                 ADD   SKT1900TMP-TENSU(IDX2 IDX3)
                                  TO  SUM-GTENSU3(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *    総合計編集
           IF  FLG-PRINT                 =  1
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SUM-GKENSU3(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1910-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SUM-GNISSU3(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1910-GNISSU(IDX2 IDX3)
      *    点数・金額
                 MOVE  SUM-GTENSU3(IDX2 IDX3)   TO   WRK-Z10
                 MOVE  WRK-Z10  TO  SKT1910-GTENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
           ELSE
      *    ページ計編集
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SUM-GKENSU(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1910-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SUM-GNISSU(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1910-GNISSU(IDX2 IDX3)
      *    点数・金額
                 MOVE  SUM-GTENSU(IDX2 IDX3)   TO   WRK-Z10
                 MOVE  WRK-Z10  TO  SKT1910-GTENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
      *    総合計編集
           END-IF
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（退職者）
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
           MOVE   "SKT1910.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-TOKUBETU
             WHEN  "1"
               MOVE  "国保総括表"               TO  SPRT-TITLE
             WHEN  "2"
               MOVE  "国保総括表 [特別療養費]"  TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT1910             TO  SPRT-PRTDATA
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
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       312-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT1900
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT1900-SRYYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT1900-ADRS
           INSPECT SKT1900-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT1900-HOSPNAME
           INSPECT SKT1900-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT1900-KAISETUNAME
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT1900-HOSPCDN
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT1900-TOKUBETU
           END-IF
      *
           .
       312-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       322-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
      *
           INITIALIZE    SUM-SGOKEI-AREA
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     7  OR
                    FLG-PRINT      =     1  OR
                    SKT1900TMP-TYOHYOKBN    NOT  =  2
      *    保険者名
             MOVE   SKT1900TMP-HKNJANUM  TO  WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE   WRK-HKNJANAME        TO  SKT1900-HKNJANAME(IDX1)
      *    給付割合
             IF       SKT1900TMP-KYURATE    NOT  =  ZERO
               MOVE  SKT1900TMP-KYURATE     TO  WRK-Z2
               STRING    "("  WRK-Z2 "割"  ")"  DELIMITED BY SIZE
                         INTO                   SKT1900-KYURATE(IDX1) 
               END-STRING
             END-IF
      *
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     6
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SKT1900TMP-KENSU(IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5     TO  SKT1900-KENSU(IDX1 IDX2 IDX3)
      *    日数
                 MOVE  SKT1900TMP-NISSU(IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5     TO  SKT1900-NISSU(IDX1 IDX2 IDX3)
      *    点数・金額
                 MOVE  SKT1900TMP-TENSU(IDX2 IDX3)  TO  WRK-Z9
                 MOVE  WRK-Z9     TO  SKT1900-TENSU(IDX1 IDX2 IDX3)
      *    ページ計に加算
                 ADD   SKT1900TMP-KENSU(IDX2 IDX3)
                                  TO  SUM-GKENSU(IDX2 IDX3)
                 ADD   SKT1900TMP-NISSU(IDX2 IDX3)
                                  TO  SUM-GNISSU(IDX2 IDX3)
                 ADD   SKT1900TMP-TENSU(IDX2 IDX3)
                                  TO  SUM-GTENSU(IDX2 IDX3)
      *    総合計に加算
      *    ページ計に加算
                 ADD   SKT1900TMP-KENSU(IDX2 IDX3)
                                  TO  SUM-GKENSU4(IDX2 IDX3)
                 ADD   SKT1900TMP-NISSU(IDX2 IDX3)
                                  TO  SUM-GNISSU4(IDX2 IDX3)
                 ADD   SKT1900TMP-TENSU(IDX2 IDX3)
                                  TO  SUM-GTENSU4(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           IF  FLG-PRINT                 =  1  OR  
               SKT1900TMP-TYOHYOKBN NOT  =  2
      *    総合計編集
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     6
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SUM-GKENSU4(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SUM-GNISSU4(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GNISSU(IDX2 IDX3)
      *    点数・金額
                 MOVE  SUM-GTENSU4(IDX2 IDX3)   TO   WRK-Z10
                 MOVE  WRK-Z10  TO  SKT1900-GTENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
           ELSE
      *    ページ計編集
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     6
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     3
      *    件数
                 MOVE  SUM-GKENSU(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SUM-GNISSU(IDX2 IDX3)   TO   WRK-Z6
                 MOVE  WRK-Z6  TO  SKT1900-GNISSU(IDX2 IDX3)
      *    点数・金額
                 MOVE  SUM-GTENSU(IDX2 IDX3)   TO   WRK-Z10
                 MOVE  WRK-Z10  TO  SKT1900-GTENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
           END-IF
      *
           .
       322-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理(国保・老人)
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
           MOVE   "SKT1900o.red"   TO  SPRT-PRTID
           EVALUATE  WRK-PARA-TOKUBETU
             WHEN  "1"
               MOVE  "国保総括表"               TO  SPRT-TITLE
             WHEN  "2"
               MOVE  "国保総括表 [特別療養費]"  TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT1900            TO  SPRT-PRTDATA
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
      *    県名設定処理
      *****************************************************************
       400-KENNAME-SEC                SECTION.
      *
           INITIALIZE                 WRK-KENNAME
           EVALUATE  SKT1900TMP-HKNJANUM(3:2)
           WHEN  "01"  MOVE  "北海道"    TO  WRK-KENNAME
           WHEN  "02"  MOVE  "青森県"    TO  WRK-KENNAME
           WHEN  "03"  MOVE  "岩手県"    TO  WRK-KENNAME
           WHEN  "04"  MOVE  "宮城県"    TO  WRK-KENNAME
           WHEN  "05"  MOVE  "秋田県"    TO  WRK-KENNAME
           WHEN  "06"  MOVE  "山形県"    TO  WRK-KENNAME
           WHEN  "07"  MOVE  "福島県"    TO  WRK-KENNAME
           WHEN  "08"  MOVE  "茨城県"    TO  WRK-KENNAME
           WHEN  "09"  MOVE  "栃木県"    TO  WRK-KENNAME
           WHEN  "11"  MOVE  "埼玉県"    TO  WRK-KENNAME
           WHEN  "10"  MOVE  "群馬県"    TO  WRK-KENNAME
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
           WHEN  "28"  MOVE  "兵庫県"    TO  WRK-KENNAME
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
           .
       400-KENNAME-EXT.
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
      *###（地方公費仕様）
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
                   MOVE    RECE10-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"             TO  MCP-FUNC
                   MOVE    "tbl_seikyu"     TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"      USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU1910 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU1910 SEIKYU INVALID ***"
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
             MOVE  HKNJA-HKNJANAME          TO  WRK-HKNJANAME
           ELSE 
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
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
            CALL  "ORCDBMAIN"          USING
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
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE    WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                              RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000                 TO  RECE10-SRYYM
             MOVE  999999                 TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_seikyu"           TO  MCP-TABLE
           MOVE    WRK-PATH               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *****
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
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  950-SEIKYU-READ-SEC
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
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
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT1900TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT1900TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT1900TMP
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
           READ  SKT1900TMP-FILE   NEXT
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
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"                    TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
