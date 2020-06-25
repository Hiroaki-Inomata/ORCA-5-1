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
       PROGRAM-ID.             SOKATU3200.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保総括表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/12/27    山口       再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * ORCA:ver    修正者     日付         内容
      * 2.5.0.ver02  山口      06/01/17    福祉件数の集計方法修正
      * 2.6.0.ver01  李路芳    06/03/21    市町村を合併後のものに修正
      *  01.00.03    内田      07/08/14    日レセver4.0対応
      *                                    システム管理マスタ「1901」の
      *                                    医療機関情報を参照
      *  01.00.04    内田      08/05/02    帳票様式の変更に伴う修正
      *  01.00.05    門間      08/12/16    SPA-AREA初期化
      *  01.00.06    吉川      10/04/07    オンライン返戻対応
      *  01.00.07    吉川      10/10/06    後期高齢のみの場合でも印字するよう修正
      *  01.00.08    吉川      10/11/09    電子請求の場合、県単独医療分は記載不要
      *  01.00.09    吉川      10/11/25    open-cobol1.0対応
      *  01.00.10    吉川      10/12/09    特別療養費候補対応
      *  01.00.11    吉川      12/06/06    帳票様式の変更に伴う修正
      *  01.00.12    吉川      13/01/11    プラグイン対応
      *                                    医療機関情報編集対応
      *  01.00.13    吉川      14/05/14    プログラムオプション対応
      *  01.00.14    萩野      14/08/08    返戻時、再印刷画面のタイトル修正
      *  01.00.15    吉川      14/10/24    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.16    吉川      15/04/06    乳児欄、福祉欄の対象公費は
      *                                    プログラムオプションを参照する
      *  01.00.17    門間      17/11/01    医療機関コード変更時、
      *                                    医療機関コード別の作成処理対応
      *                                    SITEKBN変更（動作に変更なし）
      *  01.00.18    萩野      18/07/11    クローズ漏れ対応
      *  01.00.19    井上      19/01/09    修正履歴コメント削除
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

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
           COPY    "SKT3200.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT2500         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-SEIKYU          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT2500TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KOHI            PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
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
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-SEIKYUKBN       PIC X(01).
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-KOHNUM       PIC X(03).
           03  WRK-KOHID        PIC 9(10).
      *
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-GKENSU          PIC 9(10).
      *
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *    件数退避エリア
           03  WRK-SUM-TBL OCCURS 60.
               05  WRK-SUM-KENSU   PIC 9(06).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2           PIC X(50).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-KENNAME         PIC X(10).
      *
      *    対象乳児公費番号
           03  WRK-NYJ-TBL.
               05  WRK-NYJ         PIC X(03)   OCCURS 10.
      *    対象福祉公費番号
           03  WRK-FKS-TBL.
               05  WRK-FKS         PIC X(03)   OCCURS 10.
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
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
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
      *
      *    カウント領域
       01  SUM-AREA.
           03  SUM-KN-IKENSU       PIC S9(04)   OCCURS 26.
           03  SUM-KN-TKENSU       PIC S9(04)   OCCURS 26.
           03  SUM-KN-RKENSU       PIC S9(04)   OCCURS 26.
           03  SUM-KN-KENSU        PIC S9(06).
      *
           03  SUM-KG-HKNJANUM     PIC  X(08)   OCCURS 500.
           03  SUM-KG-IKENSU       PIC S9(04)   OCCURS 500.
           03  SUM-KG-TKENSU       PIC S9(04)   OCCURS 500.
           03  SUM-KG-RKENSU       PIC S9(04)   OCCURS 500.
           03  SUM-KG-KENSU        PIC S9(06).
      *
           03  SUM-FKKENSU         PIC S9(06).
           03  SUM-NYKENSU         PIC S9(06).
           03  SUM-FRKENSU         PIC S9(06).
      *
           03  SUM-KOUKI-GKENSU    PIC  9(07).
           03  SUM-KOUKI-TBL  OCCURS  50.
               05  SUM-KOUKI-PREFNUM   PIC  X(02).
               05  SUM-KOUKI-KENSU     PIC  9(05).
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
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
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
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
           MOVE    "SOKATU3200"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
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
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
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
      *    県外共通保険者の設定
      *    左官タイル
           MOVE   "133231" TO  SUM-KG-HKNJANUM(1)
      *    全国板金
           MOVE   "133280" TO  SUM-KG-HKNJANUM(2)
      *    全国歯科
           MOVE   "093013" TO  SUM-KG-HKNJANUM(3)
      *    全国土木
           MOVE   "133033" TO  SUM-KG-HKNJANUM(4)
      *    中央建設
           MOVE   "133264" TO  SUM-KG-HKNJANUM(5)
      *    全国工事
           MOVE   "133298" TO  SUM-KG-HKNJANUM(6)
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
           MOVE    "SOKATU3200"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    請求設定
           MOVE    "SEIKYU"                  TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           IF    WRK-OPTION(1)(1:1)  =  "1" OR "2"
             MOVE    WRK-OPTION(1)(1:1)      TO  WRK-SEIKYUKBN
           ELSE
             MOVE    "1"                     TO  WRK-SEIKYUKBN
           END-IF
           DISPLAY "請求設定="  WRK-SEIKYUKBN
      *
      *    対象乳児番号
           MOVE    "SEIKYU3203"              TO  WRK-PRGID
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  10
             IF  WRK-OPTION(IDX)  NOT =  SPACE
               MOVE    WRK-OPTION(IDX)(1:3)  TO  WRK-NYJ(IDX)
             ELSE
               MOVE    "999"                 TO  WRK-NYJ(IDX)
             END-IF
             DISPLAY "対象乳児公費番号=" WRK-NYJ(IDX)
           END-PERFORM
      *
      *    対象福祉番号
           MOVE    "SEIKYU3205"              TO  WRK-PRGID
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  10
             IF  WRK-OPTION(IDX)  NOT =  SPACE
               MOVE    WRK-OPTION(IDX)(1:3)  TO  WRK-FKS(IDX)
             ELSE
               MOVE    "999"                 TO  WRK-FKS(IDX)
             END-IF
             DISPLAY "対象福祉公費番号=" WRK-FKS(IDX)
           END-PERFORM
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
           MOVE     ZERO    TO      FLG-UPD
      *
      *    国保分件数
           PERFORM 900-SEIKYU-SELECT-SEC
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
      *    後期高齢件数
           PERFORM  900-SEIKYU-CLOSE-SEC
           IF  WRK-PARA-TOKUBETU  =  "1"
             MOVE     ZERO    TO        FLG-END
             MOVE     1       TO        FLG-KOUKI
             MOVE    "039"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2002-KOUKI-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           ELSE
             MOVE     1       TO        FLG-KOUKI
             MOVE     ZERO    TO        FLG-END
             MOVE    "040"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2002-KOUKI-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           END-IF
      *    福祉件数集計
           PERFORM  210-KOH2-SYUKEI-SEC
      *
           CLOSE   UPD-FILE
      *
           IF  FLG-UPD  =   1 
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
           DISPLAY "*** SOKATU3200 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU3200 END ***"
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
           PERFORM  210-HKN-SYUKEI-SEC
      *
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
      *    集計の対象にならなかったとき
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
      *    医療保険集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    集計処理
               INITIALIZE      IDX2
               INITIALIZE      IDX4
      *
      * 保険者番号
           IF       RECE10-HKNJANUM(7:2)  =     SPACE
                    MOVE    RECE10-HKNJANUM(1:6)    TO
                            WRK-HKNJANUM
           ELSE
                    MOVE    RECE10-HKNJANUM(3:6)    TO
                            WRK-HKNJANUM
           END-IF
      *県内市長村別
           EVALUATE  WRK-HKNJANUM(1:5)
                WHEN  "32001"   MOVE  1   TO  IDX2
                WHEN  "32002"   MOVE  2   TO  IDX2
                WHEN  "32003"   MOVE  3   TO  IDX2
                WHEN  "32004"   MOVE  4   TO  IDX2
                WHEN  "32005"   MOVE  5   TO  IDX2
                WHEN  "32006"   MOVE  6   TO  IDX2
                WHEN  "32007"   MOVE  7   TO  IDX2
                WHEN  "32009"   MOVE  8   TO  IDX2
                WHEN  "32078"   MOVE  9   TO  IDX2
                WHEN  "32091"   MOVE  10  TO  IDX2
                WHEN  "32099"   MOVE  11  TO  IDX2
                WHEN  "32100"   MOVE  12  TO  IDX2
                WHEN  "32101"   MOVE  13  TO  IDX2
                WHEN  "32102"   MOVE  14  TO  IDX2
                WHEN  "32103"   MOVE  15  TO  IDX2
                WHEN  "32104"   MOVE  16  TO  IDX2
                WHEN  "32105"   MOVE  17  TO  IDX2
                WHEN  "32106"   MOVE  18  TO  IDX2
                WHEN  "32107"   MOVE  19  TO  IDX2
                WHEN  "32301"   MOVE  20  TO  IDX2
           END-EVALUATE
      *
           IF  IDX2  >  ZERO
      *
      *    県内集計処理
      *    県内計件数
             ADD      1        TO    SUM-KN-KENSU
             DISPLAY "国内計件数"    SUM-KN-KENSU 
      *
             IF  RECE10-RJNHKNNUM  = "027"
      *    県内老健件数
                 ADD    1      TO     SUM-KN-RKENSU(IDX2)
             ELSE
               IF  RECE10-HKNNUM  = "067" OR "069"
      *    県内退職件数
                   ADD    1      TO     SUM-KN-TKENSU(IDX2)
               ELSE
                   IF  RECE10-HKNNUM  = "060" OR "068"
      *    県内一般件数
                       ADD    1      TO     SUM-KN-IKENSU(IDX2)
                   END-IF
               END-IF
             END-IF
           ELSE
      *
      *    県外集計処理
      *    県外計件数
             ADD      1        TO    SUM-KG-KENSU
             DISPLAY "国外計件数"    SUM-KG-KENSU 
             MOVE     500      TO    IDX1
             PERFORM  VARYING  IDX2  FROM    1   BY  1
                      UNTIL    IDX2  >       IDX1
                IF  WRK-HKNJANUM  =  SUM-KG-HKNJANUM(IDX2)
                  IF  RECE10-RJNHKNNUM  = "027"
      *    県外老健件数
                      ADD    1      TO     SUM-KG-RKENSU(IDX2)
                  ELSE
                    IF  RECE10-HKNNUM  = "067" OR "069"
      *    県外退職件数
                      ADD    1      TO     SUM-KG-TKENSU(IDX2)
                    ELSE
                      IF RECE10-HKNNUM  = "060" OR "068"
      *    県外一般件数
                         ADD    1      TO     SUM-KG-IKENSU(IDX2)
                      END-IF
                    END-IF
                  END-IF
                  MOVE     ZERO        TO  IDX1
                ELSE
                 IF  SUM-KG-HKNJANUM(IDX2) =   SPACE
                  MOVE  WRK-HKNJANUM    TO  SUM-KG-HKNJANUM(IDX2)
                  IF    RECE10-RJNHKNNUM   =   "027"
      *    県外老健件数
                      ADD    1      TO     SUM-KG-RKENSU(IDX2)
                  ELSE
                    IF  RECE10-HKNNUM  = "067" OR "069"
      *    県外退職件数
                      ADD    1      TO     SUM-KG-TKENSU(IDX2)
                    ELSE
                      IF RECE10-HKNNUM  = "060" OR "068"
      *    県外一般件数
                         ADD    1      TO  SUM-KG-IKENSU(IDX2)
                      END-IF
                    END-IF
                  END-IF
                  MOVE     ZERO        TO  IDX1
                 END-IF
                END-IF
             END-PERFORM
           END-IF
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    福祉公費集計処理
      *****************************************************************
       210-KOH2-SYUKEI-SEC          SECTION.
      *
           PERFORM 900-KOHSKY-START-SEC
      *
      *    公費集計処理
           PERFORM  UNTIL  FLG-KOHI    =    1
      *
             PERFORM VARYING IDZ FROM    1   BY  1
                     UNTIL   IDZ >       4
                     OR   RECE02-KOHNUM(IDZ)  =  SPACE
      *
               IF        RECE02-KOHNUM(IDZ)    NOT  =  SPACE
                 MOVE     1                         TO  FLG-ERR
                 MOVE     RECE02-KOHNUM(IDZ)        TO  WRK-KOHNUM
                 MOVE     RECE02-KOHID (IDZ)        TO  WRK-KOHID
                 IF      RECE02-NYUGAIKBN    =   "1"
                     MOVE    "key33"                TO  WRK-PATH2
                 ELSE
                     MOVE    "key25"                TO  WRK-PATH2
                 END-IF
                 PERFORM  920-SYUNOU-SET-SEC
               END-IF
      *
               IF  (RECE02-KOHNUM(IDZ) =  WRK-FKS(1)
                                       OR WRK-FKS(2)
                                       OR WRK-FKS(3)
                                       OR WRK-FKS(4)
                                       OR WRK-FKS(5)
                                       OR WRK-FKS(6)
                                       OR WRK-FKS(7)
                                       OR WRK-FKS(8)
                                       OR WRK-FKS(9)
                                       OR WRK-FKS(10))
               AND  FLG-ERR           =  ZERO
      *    福祉医療件数
                   IF   WRK-PARA-SYOKBN      =  "2"
                   AND  RECE02-HENREIYM  NOT =  "000000"
                     ADD    1     TO     SUM-FKKENSU
                   ELSE
                     IF   WRK-PARA-SYOKBN     =  "1"
                     AND  RECE02-HENREIYM     =  "000000"
                       ADD    1     TO     SUM-FKKENSU
                     END-IF
                   END-IF
               END-IF
      *
               IF  (RECE02-KOHNUM(IDZ) =  WRK-NYJ(1)
                                       OR WRK-NYJ(2)
                                       OR WRK-NYJ(3)
                                       OR WRK-NYJ(4)
                                       OR WRK-NYJ(5)
                                       OR WRK-NYJ(6)
                                       OR WRK-NYJ(7)
                                       OR WRK-NYJ(8)
                                       OR WRK-NYJ(9)
                                       OR WRK-NYJ(10))
               AND  FLG-ERR           =  ZERO
      *    乳児医療件数
                 IF   WRK-PARA-SYOKBN      =  "2"
                 AND  RECE02-HENREIYM  NOT =  "000000"
                    ADD    1      TO   SUM-NYKENSU
                 ELSE
                    IF   WRK-PARA-SYOKBN     =  "1"
                    AND  RECE02-HENREIYM     =  "000000"
                      ADD    1      TO   SUM-NYKENSU
                    END-IF
                 END-IF
               END-IF
      *
             END-PERFORM
      *
             PERFORM 900-KOHSKY-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_kohsky"              TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
           .
       210-KOH2-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢者集計処理
      *****************************************************************
       2002-KOUKI-SYUKEI-SEC              SECTION.
      * 
      *    後期件数
           MOVE  "32"  TO  SUM-KOUKI-PREFNUM (1)
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     50
                    OR   SUM-KOUKI-PREFNUM (IDX1)  =  SPACE
             IF  RECE10-HKNJANUM (3:2)  =  SUM-KOUKI-PREFNUM (IDX1)
                 ADD   1   TO  SUM-KOUKI-KENSU (IDX1)
                               SUM-KOUKI-GKENSU
                 MOVE  50  TO  IDX1
             END-IF
           END-PERFORM
      *
           IF  IDX1  <  50
               MOVE  RECE10-HKNJANUM(3:2)  TO  SUM-KOUKI-PREFNUM(IDX1)
               ADD   1                     TO  SUM-KOUKI-KENSU(IDX1)
                                               SUM-KOUKI-GKENSU
           END-IF
      *
           MOVE    1    TO  FLG-UPD 
      *
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 950-SEIKYU-READ-SEC
           ELSE
      *    集計の対象にならなかったとき
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
       2002-KOUKI-SYUKEI-EXT.
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
           MOVE     ZERO        TO       FLG-PRINT
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
           END-PERFORM
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
           INITIALIZE                    SKT3200
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT3200-PRTYM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT3200-HOSPCD
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3200-HOSPNAME
           INSPECT SKT3200-HOSPNAME      REPLACING ALL "  " BY "　"
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT3200-TOKUBETU
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
      *県内
            PERFORM  VARYING    IDX2   FROM    1   BY  1
                     UNTIL      IDX2   >       20     
      *    県内一般件数
                   MOVE   SUM-KN-IKENSU(IDX2)  TO  WRK-Z5
                   MOVE   WRK-Z5 TO  SKT3200-IKENSU(IDX2) 
      *    県内退職件数
                   MOVE   SUM-KN-TKENSU(IDX2)  TO  WRK-Z5
                   MOVE   WRK-Z5 TO  SKT3200-TKENSU(IDX2) 
      *    県内老健件数
                   MOVE   SUM-KN-RKENSU(IDX2)  TO  WRK-Z5
                   MOVE   WRK-Z5 TO  SKT3200-RKENSU(IDX2) 
             END-PERFORM
      *
      *    県内計件数
             MOVE   SUM-KN-KENSU        TO  WRK-Z7
             MOVE   WRK-Z7              TO  SKT3200-KNKENSU 
      *県外
            PERFORM  VARYING IDX2  FROM  1   BY  1
                     UNTIL   IDX2  >     14
                     OR  SUM-KG-HKNJANUM(IDX2)  =  SPACE
                     COMPUTE IDX3  =     IDX2    +     22
      *    保険者名称
             DISPLAY "県外保険者"  SUM-KG-HKNJANUM(IDX2)
               IF  IDX2      >     6
                   INITIALIZE      HKNJAINF-REC
                   MOVE   SPA-HOSPNUM          TO  HKNJA-HOSPNUM
                   MOVE   SUM-KG-HKNJANUM(IDX2)
                          TO       HKNJA-HKNJANUM
                   PERFORM    900-HKNJAINF-INV-SEC
                   MOVE   HKNJA-HKNJANAME
                          TO  SKT3200-HKNJANAME(IDX2)
               END-IF
      *    県外一般件数
                   MOVE   SUM-KG-IKENSU(IDX2)  TO  WRK-Z5
                   MOVE   WRK-Z5 TO  SKT3200-IKENSU(IDX3) 
      *    県外退職件数
                   MOVE   SUM-KG-TKENSU(IDX2)  TO  WRK-Z5
                   MOVE   WRK-Z5 TO  SKT3200-TKENSU(IDX3) 
      *    県外老健件数
                   MOVE   SUM-KG-RKENSU(IDX2)  TO  WRK-Z5
                   MOVE   WRK-Z5 TO  SKT3200-RKENSU(IDX3) 
             END-PERFORM
      *
      *    県外計件数
             MOVE   SUM-KG-KENSU        TO  WRK-Z7
             MOVE   WRK-Z7              TO  SKT3200-KGKENSU 
      *
      *      県単独医療は紙請求時のみ記載する
             IF  WRK-SEIKYUKBN  =  "1"
      *
      *    福祉医療件数
               MOVE   SUM-FKKENSU         TO  WRK-Z7
               MOVE   WRK-Z7              TO  SKT3200-FKKENSU 
      *
      *    福祉老人件数
               MOVE   SUM-FRKENSU         TO  WRK-Z7
               MOVE   WRK-Z7              TO  SKT3200-FRKENSU 
      *
      *    乳児医療件数
               MOVE   SUM-NYKENSU         TO  WRK-Z7
               MOVE   WRK-Z7              TO  SKT3200-NYKENSU 
             END-IF
      *
             DISPLAY "福祉医療件数"  SUM-FKKENSU 
             DISPLAY "福祉老人件数"  SUM-FRKENSU 
             DISPLAY "乳児医療件数"  SUM-NYKENSU 
             DISPLAY "社保件数" IDX6 
      *
      *    後期高齢者分
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
                      OR       SUM-KOUKI-PREFNUM (IDX2)  =  SPACE
      *    県名
                 PERFORM  400-KENNAME-SEC
      *    件数
                 MOVE  SUM-KOUKI-KENSU (IDX2)    TO  WRK-Z5
                 MOVE  WRK-Z5        TO  SKT3200-KOU-KENSU (IDX2)
             END-PERFORM
             MOVE  SUM-KOUKI-GKENSU  TO  WRK-Z7
             MOVE  WRK-Z7            TO  SKT3200-KOUKENSU
      *
      *    総括票の印字は１頁のみ
             MOVE   1     TO   FLG-PRINT
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
           MOVE   "SKT3200.red"            TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保総括表"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保総括表［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保総括表［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保総括表［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3200             TO  SPRT-PRTDATA
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
      *    県名設定処理
      *****************************************************************
       400-KENNAME-SEC                SECTION.
      *
           INITIALIZE                 WRK-KENNAME
           EVALUATE  SUM-KOUKI-PREFNUM (IDX2)
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
           MOVE  WRK-KENNAME  TO  SKT3200-KOU-KEN (IDX2)
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
                       MOVE  "*** SOKATU2500 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU2500 SEIKYU INVALID ***"
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
           MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000              TO  RECE10-SRYYM
             MOVE  999999              TO  RECE10-XXSRYYM
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
      *
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
           MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
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
      ******************************************** *********************
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
      *
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
       950-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込 
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
               MOVE    ZERO                TO  FLG-KOHI
           ELSE
               MOVE    1                   TO  FLG-KOHI
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ 
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"             TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-KOHI
               MOVE    MCPDATA-REC         TO  RECE02-REC
               IF  RECE02-KOHNUM(1)  =  WRK-NYJ(1) OR WRK-NYJ(2)
                                    OR  WRK-NYJ(3) OR WRK-NYJ(4)
                                    OR  WRK-NYJ(5) OR WRK-NYJ(6)
                                    OR  WRK-NYJ(7) OR WRK-NYJ(8)
                                    OR  WRK-NYJ(9) OR WRK-NYJ(10)
                                    OR  WRK-FKS(1) OR WRK-FKS(2)
                                    OR  WRK-FKS(3) OR WRK-FKS(4)
                                    OR  WRK-FKS(5) OR WRK-FKS(6)
                                    OR  WRK-FKS(7) OR WRK-FKS(8)
                                    OR  WRK-FKS(9) OR WRK-FKS(10)
               OR  RECE02-KOHNUM(2)  =  WRK-NYJ(1) OR WRK-NYJ(2)
                                    OR  WRK-NYJ(3) OR WRK-NYJ(4)
                                    OR  WRK-NYJ(5) OR WRK-NYJ(6)
                                    OR  WRK-NYJ(7) OR WRK-NYJ(8)
                                    OR  WRK-NYJ(9) OR WRK-NYJ(10)
                                    OR  WRK-FKS(1) OR WRK-FKS(2)
                                    OR  WRK-FKS(3) OR WRK-FKS(4)
                                    OR  WRK-FKS(5) OR WRK-FKS(6)
                                    OR  WRK-FKS(7) OR WRK-FKS(8)
                                    OR  WRK-FKS(9) OR WRK-FKS(10)
               OR  RECE02-KOHNUM(3)  =  WRK-NYJ(1) OR WRK-NYJ(2)
                                    OR  WRK-NYJ(3) OR WRK-NYJ(4)
                                    OR  WRK-NYJ(5) OR WRK-NYJ(6)
                                    OR  WRK-NYJ(7) OR WRK-NYJ(8)
                                    OR  WRK-NYJ(9) OR WRK-NYJ(10)
                                    OR  WRK-FKS(1) OR WRK-FKS(2)
                                    OR  WRK-FKS(3) OR WRK-FKS(4)
                                    OR  WRK-FKS(5) OR WRK-FKS(6)
                                    OR  WRK-FKS(7) OR WRK-FKS(8)
                                    OR  WRK-FKS(9) OR WRK-FKS(10)
               OR  RECE02-KOHNUM(4)  =  WRK-NYJ(1) OR WRK-NYJ(2)
                                    OR  WRK-NYJ(3) OR WRK-NYJ(4)
                                    OR  WRK-NYJ(5) OR WRK-NYJ(6)
                                    OR  WRK-NYJ(7) OR WRK-NYJ(8)
                                    OR  WRK-NYJ(9) OR WRK-NYJ(10)
                                    OR  WRK-FKS(1) OR WRK-FKS(2)
                                    OR  WRK-FKS(3) OR WRK-FKS(4)
                                    OR  WRK-FKS(5) OR WRK-FKS(6)
                                    OR  WRK-FKS(7) OR WRK-FKS(8)
                                    OR  WRK-FKS(9) OR WRK-FKS(10)
      *        テスト患者のときは読み飛ばす
                 INITIALIZE                      PTINF-REC
                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                 MOVE    RECE02-PTID         TO  PTINF-PTID
                 MOVE    PTINF-REC           TO  MCPDATA-REC
                 PERFORM 800-PTINF-READ-SEC
                 IF      FLG-PTINF           =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
                     DISPLAY "テスト患者" PTINF-PTID
                     GO  TO  900-KOHSKY-READ-SEC
                 END-IF
               ELSE
                     GO  TO  900-KOHSKY-READ-SEC
               END-IF  
           ELSE
               MOVE    1               TO  FLG-KOHI
           END-IF
           .
       900-KOHSKY-READ-EXT.
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
      *    請求点数集計処理   06/01/19
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
      *
      *    計算負担金に差がある場合、地方公費の助成があったと判断する
               IF  FLG-ERR                  =  1    AND
                   SYU-KOH-HKNNUM(IDX)      =  WRK-KOHNUM
                 IF     IDX                 =  1
                   IF  SYU-SYUCOMPFTN-ENTANI
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR
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
      *  収納情報セット   06/01/19
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    WRK-PATH2          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    WRK-PATH2      TO  MCP-PATHNAME
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10) 
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_syunou"       TO  MCP-TABLE
                 MOVE    WRK-PATH2          TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH2          TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込   06/01/19
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
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
      *    保険組合せマスター読み込み  06/01/19
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM            TO  COMB-HOSPNUM
           MOVE    SYU-PTID               TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM        TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_hkncombi"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
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
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    HKNJAINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_hknjainf"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
                   DISPLAY "保険者マスタ取得 正常"   
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               DISPLAY "保険者マスタ取得 エラー"  HKNJA-HKNJANUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-HKNJAINF-INV-EXT.
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
