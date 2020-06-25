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
       PROGRAM-ID.             SOKATU2500.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/30    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    山口    2006/02/16    返戻対応
      *  01.00.02    内田    2007/05/09    帳票様式の変更に伴う修正
      *  01.00.03    内田    2007/09/08    日レセver4.0対応
      *                                    システム管理マスタ「1901」の
      *                                    医療機関情報を参照
      *  01.00.04    伊藤    2008/04/29    帳票様式に伴う修正
      *  01.00.05    内田    2008/07/15    後期高齢のみの場合でも印刷する
      *  01.00.06    吉川    2008/10/20    SPA-AREA初期化
      *  01.00.07    吉川    2009/06/01    レセ電時は件数を印字しない
      *                      2009/06/10    「愛荘町」の保険者番号修正
      *  01.00.08    吉川    2010/04/05    オンライン返戻対応
      *  01.00.09    吉川    2010/04/14    県内・県外判定の変更
      *                                    帳票様式の変更に伴う修正
      *  01.00.10    吉川    2010/05/11    滋賀県医師国保（253013）が
      *                                    印字されない不具合を修正
      *  01.00.11    吉川    2012/04/20    帳票様式の変更に伴う修正
      *                                    （県内・県外を同一様式へ）
      *  01.00.12    吉川    2013/01/10    医療機関情報編集対応
      *  01.00.13    吉川    2013/06/17    対象公費番号をINIファイルから
      *                                    取得するようにする
      *                                    地方公費を２つ以上所持しても
      *                                    福祉件数欄は1件でカウントする
      *  01.00.14    吉川    2013/10/04    プログラムオプション対応
      *  01.00.15    吉川    2014/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.16    門間    2016/01/06    医療機関編集情報への開設者名追加対応
      *  01.00.17    吉川    2016/03/11    返戻時、再印刷画面のタイトル修正
      *  01.00.18    門間    2017/12/04    医療機関コード変更時、
      *                                    医療機関コード別の作成処理対応
      *                                    SITEKBN変更（動作に変更なし）
      *  01.00.19    井上    2018/12/12    修正履歴コメント削除
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
           COPY    "SKT2500.INC".
      *
      *    対象公費番号データ
           COPY  "TAISYO2501.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-TAISYOKOHI      PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-RECEDEN         PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-KOURECEDEN      PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-TAISYOKOHI      PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
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
           03  IDX                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDY                 PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
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
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-PATH            PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
      **    医療機関情報
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
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
           03  WRK-RECEERR         PIC X(200).
      *
      *    他保険者テーブル
           03  WRK-EHKNJA-TBL  OCCURS 7.
               05  WRK-EHKNJANUM   PIC 9(06).
               05  WRK-EKENSU      PIC 9(05).
           03  WRK-PATH2               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-TODOUHUKEN          PIC X(100).
           03  WRK-RETSU               PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-KOHID           PIC 9(10).
           03  WRK-FKENSU1         PIC 9(04).
           03  WRK-FKENSU2         PIC 9(04).
           03  WRK-FMAISU1         PIC 9(04).
           03  WRK-FMAISU2         PIC 9(04).
           03  WRK-FMAISUG         PIC 9(04).
           03  WRK-AMARI           PIC 9(01).
           03  WRK-KENNAME         PIC X(10).
           03  WRK-KOUKI-TBL OCCURS 5.
               05  WRK-PREFNUM     PIC X(02).
               05  WRK-KOUKENSU    PIC 9(05).
      *
      *    集計エリア
       01  SUM-AREA.
           03  SUM-HKNJA-TBL OCCURS 61.
               05  SUM-KENSU       PIC 9(05).
           03  SUM-GKENSU-TBL OCCURS 2.
               05  SUM-GKENSU      PIC 9(05).
               05  SUM-GKENSUK     PIC 9(05).
           03  SUM-TOKGKENSU       PIC 9(05).
           03  SUM-TOKGKENSUK      PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
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
      *    レセプト・総括印刷情報
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報−レセプト・総括印刷情報（後期高齢）
           COPY    "CPSK200501.INC".
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  TAISYO-DAT
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
                                               WRK-PARA-SYOKBN
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
           MOVE    "SOKATU2500"    TO  JOB-PGID
           MOVE    "国保総括票"    TO  JOB-SHELLMSG
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
      *    レセプト・総括印刷情報　レセ電か
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-2005-STYUKYMD(1:6)
                                           SYS-2005-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD(7:2)
                                           SYS-2005-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
               MOVE    SYS-2005-KKHRECEKBN TO  FLG-RECEDEN
           ELSE
               MOVE    0                   TO  FLG-RECEDEN
           END-IF 
      *
      *    医療機関情報−レセプト・総括印刷情報（後期高齢）
           INITIALIZE                     SYS-200501-REC
           MOVE     "2005"        TO      SYS-200501-KANRICD
           MOVE     "01"          TO      SYS-200501-KBNCD
           MOVE     LNK-PRTKANRI-SRYYM(1:6)
                                  TO      SYS-200501-STYUKYMD(1:6)
                                          SYS-200501-EDYUKYMD(1:6)
           MOVE     "01"          TO      SYS-200501-STYUKYMD(7:2)
                                          SYS-200501-EDYUKYMD(7:2)
           MOVE     SPA-HOSPNUM   TO      SYS-200501-HOSPNUM
           MOVE     SYS-200501-REC  TO      MCPDATA-REC
           PERFORM  910-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
               MOVE    MCPDATA-REC   TO    SYS-200501-REC
               MOVE    SYS-200501-KOUIKIRECEKBN TO  FLG-KOURECEDEN
           ELSE
               MOVE    ZERO                     TO  FLG-KOURECEDEN
           END-IF
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
           EVALUATE    WRK-PARA-SYOKBN
	       WHEN    "2"
                   MOVE    "key37"                  TO  WRK-PATH
                   MOVE    "key65"     TO  WRK-PATH2
	       WHEN    "1"
                   MOVE    "key38"                  TO  WRK-PATH
                   MOVE    "key66"     TO  WRK-PATH2
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
           MOVE    "SOKATU2500"              TO  WRK-PRGID
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
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
           PERFORM  900-SEIKYU-CLOSE-SEC
           MOVE     ZERO    TO        FLG-END
           MOVE     1       TO        FLG-KOUKI
           MOVE    "039"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
           MOVE     ZERO    TO        FLG-END
           MOVE    "040"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
      *
           MOVE     ZERO    TO        FLG-END
           PERFORM  2001-KOKUHO-TOKU-SYUKEI-SEC
      *    福祉医療費請求書件数集計
           PERFORM  2001-FUKUSI-SYUKEI-SEC
      *
           CLOSE   UPD-FILE
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
           DISPLAY "*** SOKATU2500 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU2500 END ***"
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
      *TMP-FILE キーセット
      *
           MOVE    RECE10-HKNJANUM-S(1:6) TO    WRK-HKNJANUM(1:6)
      *
      *    札幌市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "014"
               MOVE    "010017"           TO  WRK-HKNJANUM
           END-IF
      *
      *    仙台市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "044"
               MOVE    "044008"           TO  WRK-HKNJANUM
           END-IF
      *
      *    さいたま市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "114"
               MOVE    "114009"           TO  WRK-HKNJANUM
           END-IF
      *
      *    千葉市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "124"
               MOVE    "124008"           TO  WRK-HKNJANUM
           END-IF 
      *
      *    横浜市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "144"
               MOVE    "144006"           TO  WRK-HKNJANUM
           END-IF
      *
      *    川崎市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "145"
               MOVE    "145003"           TO  WRK-HKNJANUM
           END-IF
      *
      *    相模原市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "146"
               MOVE    "146001"           TO  WRK-HKNJANUM
           END-IF
      *
      *    新潟市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "154"
               MOVE    "154005"           TO  WRK-HKNJANUM
           END-IF
      *
      *    静岡市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "224"
               MOVE    "224006"           TO  WRK-HKNJANUM
           END-IF
      *
      *    浜松市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "225"
               MOVE    "225003"           TO  WRK-HKNJANUM
           END-IF
      *
      *    名古屋市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "234"
               MOVE    "234005"           TO  WRK-HKNJANUM
           END-IF
      *
      *    京都市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "264"
               MOVE    "264002"           TO  WRK-HKNJANUM
           END-IF
      *
      *    大阪市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "274"
               MOVE    "274001"           TO  WRK-HKNJANUM
           END-IF
      *
      *    堺市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "275"
               MOVE    "275008"           TO  WRK-HKNJANUM
           END-IF
      *
      *    神戸市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "284"
               MOVE    "284000"           TO  WRK-HKNJANUM
           END-IF
      *
      *    広島市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "344"
               MOVE    "344002"           TO  WRK-HKNJANUM
           END-IF
      *
      *    北九州市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "404"
               MOVE    "404004"           TO  WRK-HKNJANUM
           END-IF
      *
      *    福岡市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  =  "405"
               MOVE    "405001"           TO  WRK-HKNJANUM
           END-IF
      *
      *    県内／県外
           IF  RECE10-PREFKBN  =  "1"
               MOVE    1                  TO  IDX2
           ELSE
               MOVE    2                  TO  IDX2
           END-IF
      *
           IF  FLG-KOUKI  =  ZERO
               PERFORM  210-HKN-SYUKEI-SEC
           ELSE
               PERFORM  220-KOUKI-SYUKEI-SEC
           END-IF
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
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
      *    医療保険集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    集計場所設定
           MOVE    ZERO      TO       IDX1
           EVALUATE    WRK-HKNJANUM
               WHEN    "253013"    MOVE    1   TO  IDX1
               WHEN    "250019"    MOVE    2   TO  IDX1
               WHEN    "250027"    MOVE    3   TO  IDX1
               WHEN    "250035"    MOVE    4   TO  IDX1
               WHEN    "250043"    MOVE    5   TO  IDX1
               WHEN    "250050"    MOVE    6   TO  IDX1
               WHEN    "250068"    MOVE    7   TO  IDX1
               WHEN    "250076"    MOVE    8   TO  IDX1
               WHEN    "250092"    MOVE    9   TO  IDX1
               WHEN    "250100"    MOVE    10  TO  IDX1
               WHEN    "250118"    MOVE    11  TO  IDX1
               WHEN    "250126"    MOVE    12  TO  IDX1
               WHEN    "250134"    MOVE    13  TO  IDX1
               WHEN    "250522"    MOVE    14  TO  IDX1
               WHEN    "250647"    MOVE    15  TO  IDX1
               WHEN    "250654"    MOVE    16  TO  IDX1
               WHEN    "250712"    MOVE    17  TO  IDX1
               WHEN    "250738"    MOVE    18  TO  IDX1
               WHEN    "250746"    MOVE    19  TO  IDX1
               WHEN    "250753"    MOVE    20  TO  IDX1
               WHEN    "133033"    MOVE    21  TO  IDX1
               WHEN    "133264"    MOVE    22  TO  IDX1
               WHEN    "133298"    MOVE    23  TO  IDX1
               WHEN    "233064"    MOVE    24  TO  IDX1
               WHEN    "261329"    MOVE    25  TO  IDX1
               WHEN    "273102"    MOVE    26  TO  IDX1
               WHEN    OTHER
      *    他市町村名の場合
                   PERFORM  VARYING  IDX  FROM  1  BY  1
                            UNTIL    IDX  >     7
                     IF  WRK-EHKNJANUM (IDX)  =  WRK-HKNJANUM
                     OR  WRK-EHKNJANUM (IDX)  =  ZERO
                       MOVE  WRK-HKNJANUM  TO  WRK-EHKNJANUM(IDX)
                       IF  FLG-RECEDEN      =  ZERO
                       OR  WRK-PARA-SYOKBN  =  "2"
                         ADD   1           TO  WRK-EKENSU (IDX)
                       END-IF
                       MOVE  7             TO  IDX
                     END-IF
                   END-PERFORM
           END-EVALUATE
      *
           IF  FLG-RECEDEN      =  ZERO
           OR  WRK-PARA-SYOKBN  =  "2"
             IF      IDX1    >   ZERO
                 ADD     1   TO  SUM-KENSU(IDX1)
             END-IF
             ADD     1       TO  SUM-GKENSU(IDX2)
           END-IF
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢者分集計処理
      *****************************************************************
       220-KOUKI-SYUKEI-SEC               SECTION.
      *
      *    県内時集計
           IF  FLG-KOURECEDEN      =  ZERO
           OR  WRK-PARA-SYOKBN  =  "2"
                 IF  RECE10-HKNNUM  =  "039"
                     ADD  1  TO  SUM-GKENSUK(IDX2)
                 END-IF
                 IF  RECE10-HKNNUM  =  "040"
                     ADD  1  TO  SUM-TOKGKENSUK
                 END-IF
           END-IF
      *
      *    県外後期高齢の集計
           IF  IDX2  =  2  AND  RECE10-HKNNUM  =  "039"
      *        県番号セット
               IF  RECE10-HKNJANUM(7:2)  =  SPACE
                 MOVE  RECE10-HKNJANUM(1:2)  TO  WRK-HKNJANUM
               ELSE
                 MOVE  RECE10-HKNJANUM(3:2)  TO  WRK-HKNJANUM
               END-IF
      *
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     5
                 IF  WRK-PREFNUM(IDX3)  =  WRK-HKNJANUM
                 OR  WRK-PREFNUM(IDX3)  =  SPACE
                   MOVE  WRK-HKNJANUM  TO  WRK-PREFNUM(IDX3)
                   IF  FLG-KOURECEDEN      =  ZERO
                   OR  WRK-PARA-SYOKBN  =  "2"
                     ADD   1           TO  WRK-KOUKENSU(IDX3)
                   END-IF
                   MOVE  5             TO  IDX3
                 END-IF
               END-PERFORM
           END-IF
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       220-KOUKI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別療養費国保集計処理
      *****************************************************************
       2001-KOKUHO-TOKU-SYUKEI-SEC              SECTION.
      * 
           MOVE    "key41"                  TO  WRK-PATH
      *
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM 
                   UNTIL       FLG-END     =   1
               EVALUATE    WRK-PARA-SYOKBN
                   WHEN    "2"
                       IF    RECE10-HENREIYM  NOT =  "000000"
                           ADD  1  TO  SUM-TOKGKENSU
                       END-IF
                   WHEN    "1"
                       IF    RECE10-HENREIYM    =  "000000"
                           ADD  1  TO  SUM-TOKGKENSU
                       END-IF
               END-EVALUATE
               PERFORM   900-SEIKYU-READ-SEC
      *
           END-PERFORM
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
           .
       2001-KOKUHO-TOKU-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    連名簿枚数合計取得処理
      *****************************************************************
       2001-FUKUSI-SYUKEI-SEC                SECTION.
      *
           MOVE    ZERO    TO  FLG-END
           PERFORM 900-KOHSKY-START-SEC
      *
           PERFORM UNTIL   FLG-END =   1
      *
               MOVE    1                         TO  IDZ
                   INITIALIZE                        TBL-HIKAKU-KOHI
                   MOVE    RECE02-KOHNUM(IDZ)    TO  TBL-HKOHNUM(1)
      *            対象公費番号チェック処理
                   PERFORM  800-TAISYOKOHI-CHK-SEC
                   IF  FLG-TAISYO  =  1
                       PERFORM 20011-KOH-CHK-SEC
      *
                   END-IF
      *
               PERFORM 900-KOHSKY-READ-SEC
           END-PERFORM
      *
      *    連名簿枚数取得
      *    入院枚数計算
           DIVIDE  WRK-FKENSU1    BY  6
                   GIVING      WRK-FMAISU1
                   REMAINDER   WRK-AMARI
           IF      WRK-AMARI   NOT =   0
               ADD     1   TO  WRK-FMAISU1
           END-IF
      *    外来枚数計算
           DIVIDE  WRK-FKENSU2    BY  6
                   GIVING      WRK-FMAISU2
                   REMAINDER   WRK-AMARI
           IF      WRK-AMARI   NOT =   0
               ADD     1   TO  WRK-FMAISU2
           END-IF
      *
           DISPLAY "入院福祉件数=" WRK-FKENSU1
                   "外来福祉件数=" WRK-FKENSU2
      *
           COMPUTE  WRK-FMAISUG  =  WRK-FMAISU1  +  WRK-FMAISU2
      *
           .
       2001-FUKUSI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    福祉医療費請求書枚数計算
      *****************************************************************
       20011-KOH-CHK-SEC            SECTION.
      *
           MOVE    1       TO  FLG-ERR
           MOVE    RECE02-KOHNUM(IDZ)  TO  WRK-KOHNUM
           MOVE    RECE02-KOHID(IDZ)   TO  WRK-KOHID
      *
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"         TO  WRK-PATH
           ELSE
               MOVE    "key25"         TO  WRK-PATH
           END-IF
      *
      *    助成判定
           PERFORM 920-SYUNOU-SET-SEC
      *
      *  当月・月遅れ分の処理の時だけ福祉医療費請求書枚数を記載する
           IF      FLG-ERR     =   ZERO
             IF   WRK-PARA-SYOKBN      =    "1"
               IF   RECE02-NYUGAIKBN     =     "1"
                 ADD     1       TO  WRK-FKENSU1
               ELSE
                 ADD     1       TO  WRK-FKENSU2
               END-IF
               MOVE    1       TO  FLG-UPD
             END-IF
           ELSE
               DISPLAY "助成対象外=" RECE02-PTNUM
                       "入外区分="   RECE02-NYUGAIKBN
                       "公費番号="   RECE02-KOHNUM(IDZ)
                       "点数="       RECE02-TOTALTEN(IDZ + 1)
                       "負担金="     RECE02-FTNMONEY(IDZ + 1)
                       "負担者番号=" RECE02-KOHFTNJANUM(IDZ)
           END-IF
      *
      *
           .
       20011-KOH-CHK-EXT.
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
      *
      *    帳票編集<見出し>処理
           PERFORM 310-HEAD-HEN-SEC
      *
      *    帳票編集<明細>処理
           PERFORM 320-BODY-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 330-PRINT-OUT-SEC
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
           INITIALIZE                    SKT2500
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT2500-SRYYM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  SKT2500-HOSPCD
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT2500-ADRS
           INSPECT SKT2500-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT2500-HOSPNAME
           INSPECT SKT2500-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SKT2500-KAISETU
      *
      *    電話番号
           MOVE    WRK-HOSPTEL             TO  SKT2500-TEL
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT2500-SEIYMD
      *
      *    レセ電か
           IF  FLG-RECEDEN  =  "0"
             MOVE    "○"                  TO  SKT2500-RECE (1)
           ELSE
             MOVE    "○"                  TO  SKT2500-RECE (2)
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
           PERFORM VARYING IDX1    FROM    1   BY  1
                              UNTIL     IDX1    >     26
               MOVE    SUM-KENSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5              TO  SKT2500-KENSU(IDX1)
           END-PERFORM
      *
      *    他保険者
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     7
                    OR       WRK-EHKNJANUM (IDX1)  =  ZERO
               MOVE  WRK-EHKNJANUM (IDX1)  TO  WRK-HKNJANUM
               PERFORM  900-HKNJAINF-SET-SEC
               IF  HKNJA-HKNJANAME(15:1)  =  SPACE
                 MOVE  HKNJA-HKNJANAME     TO  SKT2500-EHKNJANAME1(IDX1)
               ELSE
                 MOVE  HKNJA-HKNJANAME     TO  SKT2500-EHKNJANAME2(IDX1)
               END-IF
               MOVE  WRK-EHKNJANUM (IDX1)  TO  SKT2500-EHKNJANUM(IDX1)
               MOVE  WRK-EKENSU (IDX1)     TO  WRK-Z5
               MOVE  WRK-Z5                TO  SKT2500-EKENSU(IDX1)
           END-PERFORM
      *
      *    県外保険者
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     5
                    OR       WRK-PREFNUM (IDX1)  =  SPACE
               MOVE  WRK-PREFNUM (IDX1)  TO  WRK-HKNJANUM
               PERFORM  800-KENNAME-SET-SEC
               MOVE  WRK-KENNAME           TO  SKT2500-KENNAME(IDX1)
               MOVE  WRK-KOUKENSU (IDX1)   TO  WRK-Z5
               MOVE  WRK-Z5                TO  SKT2500-KOUKENSU(IDX1)
           END-PERFORM
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL    IDX2  >     2
      *        国保分
               MOVE    SUM-GKENSU(IDX2)    TO  WRK-Z6
               MOVE    WRK-Z6              TO  SKT2500-GKENSU(IDX2)
      *        後期高齢者分
               MOVE    SUM-GKENSUK(IDX2)   TO  WRK-Z6
               MOVE    WRK-Z6              TO  SKT2500-GKENSUK(IDX2)
           END-PERFORM
      *
      *    合計処理（特別療養費国保）
           MOVE    SUM-TOKGKENSU  TO  WRK-Z3
           MOVE    WRK-Z3      TO  SKT2500-TOKURYO
      *    合計処理（特別療養費後期高齢者）
           MOVE    SUM-TOKGKENSUK  TO  WRK-Z3
           MOVE    WRK-Z3      TO  SKT2500-TOKURYOK
      *
      *    福祉医療費請求書紙合計
           MOVE    WRK-FMAISUG             TO  WRK-Z3
           MOVE    WRK-Z3                  TO  SKT2500-FKENSUK
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
           IF  WRK-SYMD (1:6)  >=  WRK-CONS-SRYYM-200804
               MOVE   "SKT2500.red"    TO  SPRT-PRTID
           ELSE
               MOVE   "SKT2500o.red"   TO  SPRT-PRTID
           END-IF
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE  "国保総括票"      TO  SPRT-TITLE
             WHEN  "2"
               MOVE  "国保総括票［返戻分］" 
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT2500             TO  SPRT-PRTDATA
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
      *    都道府県名セット
      *****************************************************************
        800-KENNAME-SET-SEC                SECTION.
      *
           EVALUATE  WRK-HKNJANUM(1:2)
             WHEN  "01"  MOVE  "北海道"    TO  WRK-KENNAME
             WHEN  "02"  MOVE  "青森県"    TO  WRK-KENNAME
             WHEN  "03"  MOVE  "岩手県"    TO  WRK-KENNAME
             WHEN  "04"  MOVE  "宮城県"    TO  WRK-KENNAME
             WHEN  "05"  MOVE  "秋田県"    TO  WRK-KENNAME
             WHEN  "06"  MOVE  "山形県"    TO  WRK-KENNAME
             WHEN  "07"  MOVE  "福島県"    TO  WRK-KENNAME
             WHEN  "08"  MOVE  "茨城県"    TO  WRK-KENNAME
             WHEN  "09"  MOVE  "栃木県"    TO  WRK-KENNAME
             WHEN  "10"  MOVE  "群馬県"    TO  WRK-KENNAME
             WHEN  "11"  MOVE  "埼玉県"    TO  WRK-KENNAME
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
       800-KENNAME-SET-EXT.
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM   TO  RECE10-SKYYM.
      *
           MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
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
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM                TO  COMB-HOSPNUM
           MOVE    SYU-PTID                   TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM            TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
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
               IF      FLG-ERR                 =   1
               AND     SYU-KOH-HKNNUM(IDX)     =   WRK-KOHNUM
                   IF      IDX                 =   1
                       IF      SYU-SYUCOMPFTN-ENTANI
                                       >   SYU-KOH-COMPFTN-ENTANI(IDX)
                           MOVE    ZERO        TO  FLG-ERR
                       END-IF
                   ELSE
                       IF      SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                       >   SYU-KOH-COMPFTN-ENTANI(IDX)
                           MOVE    ZERO        TO  FLG-ERR
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
      *  収納情報セット
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
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    WRK-PATH         TO  MCP-PATHNAME
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
                 MOVE    "tbl_syunou"     TO  MCP-TABLE
                 MOVE    WRK-PATH         TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING
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
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1               TO  FLG-END
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
           MOVE    "DBFETCH"               TO  MCP-FUNC
           MOVE    "tbl_kohsky"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               INITIALIZE                          TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)        TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  TBL-HKOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
      *            テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
               ELSE
                   GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタクローズ
      *****************************************************************
       900-KOHSKY-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
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
      *   保険者情報セット
      *****************************************************************
       900-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM              TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM             TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC             TO  MCPDATA-REC
           MOVE  "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"       TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             PERFORM  900-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       900-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       900-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
           ELSE
             INITIALIZE             HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
           END-IF
      *
           .
       900-HKNJAINF-READ-EXT.
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
       900-SEIKYU-CLOSE-EXT.
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
       900-DBDISCONNECT-EXT.
           EXIT.
