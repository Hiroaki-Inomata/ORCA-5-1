      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      *******************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU1803.
      *******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 福井・医療費助成事業総括表
      *                      (191,193,194,196,198)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/03/13    中川
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者  日付         内容
      *  01.00.01       吉川    2007/05/17   帳票様式変更に伴う修正と
      *                                      仕様変更に伴う修正
      *  01.00.02       吉川    2007/10/05   日レセver4.0対応
      *                                      システム管理マスタ「1901」の
      *                                      医療機関情報を参照
      *  01.00.03       吉川    2007/10/25   特定負無(公費番号=091)の患者を
      *                                      帳票に表示させないように修正
      *  01.00.04       吉川    2008/02/26   負担金が発生しない場合は
      *                                      印字対象外とする
      *  01.00.05       吉川    2009/03/18   退院していない入院患者が
      *                                      取得されない不具合を修正
      *  01.00.06       吉川    2011/02/17   open-cobol1.0対応
      *  01.00.07       吉川    2011/12/16   同月内で受給者番号が変わっても
      *                                      帳票に反映されないのを修正
      *  01.00.08       吉川    2013/01/09   医療機関情報編集対応
      *  01.00.09       吉川    2014/10/09   ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.10       吉川    2015/11/26   患者負担が食事のみのデータは
      *                                      印字対象とするように修正
      *  01.00.11       門間    2016/11/15   対象公費はPGオプションから取得する
      *  01.00.12       門間    2016/12/05   帳票が集計されない不具合を修正
      *  01.00.13       門間    2017/03/08   医療機関コード変更時、
      *                                      医療機関コード別の作成処理対応
      *  01.00.14       萩野    2018/04/09   SITEKBN変更（動作に変更なし）
      *  01.00.15       萩野    2018/11/07   修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI1803.INC".
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    印刷用データ 名称領域
           COPY  "CPCOMMONPRT.INC".
      *    シェル用領域
           COPY  "CPCOMMONSHELL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    印刷用データ
           COPY  "SC1803.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100   PIC X(02).
           03  STS-RECE02  PIC X(02).
           03  STS-RECEERR  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-END2       PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-COMP.
               05  WRK-COMP-SRYY   PIC  9(4).
               05  WRK-COMP-BIRY   PIC  9(4).
               05  WRK-COMP-SRYM   PIC  9(2).
               05  WRK-COMP-BIR    PIC  9(2).
           03  WRK-KOHFTN          PIC 9(07).
           03  WRK-YKZFTN          PIC 9(07).
      *
           03  WRK-RECEERR         PIC  X(200).
           03  WRK-DENPPRTYMWH     PIC  X(16).
           03  WRK-SYSYMDWH        PIC  X(22).
           03  WRK-SRYYM           PIC  X(09).
      *
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-PAGE            PIC 9(02).
           03  WRK-AMARI           PIC 9(02).
           03  WRK-GKENSU          PIC 9(02).
           03  WRK-GKINGAKU        PIC 9(07).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-LASTYMD         PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY         PIC 9(04).
               05  WRK-NMM         PIC 9(02).
               05  WRK-NDD         PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY         PIC 9(04).
               05  WRK-TMM         PIC 9(02).
               05  WRK-TDD         PIC 9(02).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-SEIKYU-COMP     PIC S9(09).
           03  WRK-SEIKYU          PIC 9(07).
           03  WRK-KASAN-TBL.
               05  WRK-KOHNUM      PIC X(03).
               05  WRK-KOHID       PIC 9(10).
               05  WRK-SEIKYUGK-G  PIC 9(08)V9(01).
               05  WRK-SEIKYUGK-G2 PIC 9(08).
               05  WRK-SEIKYUGK    PIC 9(08)V9(01).
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *    プログラムオプション
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
      *
           03  WRK-PATH              PIC X(50).
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
      *
      *    編集用エリア
           03  WRK-ITBFTNX     	   PIC ZZZ,ZZZ.
           03  WRK-KENSUX          PIC Z,ZZZ.
           03  WRK-Z1              PIC Z.
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
      *
       01  WRK-DATA.
           03  WRK-KOH-TBL OCCURS  6.
               05  WRK-DATA-TBL OCCURS  18.
                   07  WRK-KENSU   PIC 9(05).
                   07  WRK-ITBFTN  PIC 9(07).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02          PIC 9(06).
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-PRINT           PIC 9(06).
           03  CNT-MF100           PIC 9(05).
      *
       01  SUM-AREA  VALUE   ZERO.
           03  SUM-SYOKEI.
               05  SUM-SKENSU      PIC 9(10).
               05  SUM-SSEIKYU	   PIC 9(10).
      *    キー用エリア
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYYM     PIC X(06).
           03  KEY-OLD.
               05  KEY-O-SRYYM     PIC X(06).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME            PIC 9(08).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *
           COPY  "CPSHELLTBL.INC".
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
                  USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  SUM-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
      *
           DISPLAY "START"
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU1803"    TO  JOB-PGID
           MOVE    "医療費助成事業総括表" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
      *
           PERFORM 31012-SEIWA-HEN-SEC
      *
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
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
      *
           MOVE  WRK-HENYMDG(1:16)          TO  WRK-DENPPRTYMWH
           MOVE  WRK-HENYMDG1               TO  WRK-SRYYM
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1803 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"        TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1803 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
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
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU1803"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             MOVE    WRK-OPTION(IDX)(1:3)    TO  WRK-TAISYO-KOHNUM(IDX)
             DISPLAY "対象公費番号="  WRK-TAISYO-KOHNUM(IDX)
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
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"       TO  MCP-TABLE
                   MOVE    "key5"             TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *
                   MOVE    "tbl_btpara"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF 
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN    OUTPUT  MF100-FILE
           CLOSE           MF100-FILE
           OPEN    I-O     MF100-FILE
      *
           INITIALIZE  WRK-DATA
           MOVE    ZERO    TO      FLG-END
           MOVE    ZERO    TO      FLG-END2
      *
           PERFORM 2001-SYUKEI-SEC
                   UNTIL   FLG-END     =   1
      *
           CLOSE   MF100-FILE
      *
           MOVE    ZERO    TO      FLG-END
           MOVE    ZERO    TO      FLG-END2
      *
           OPEN    INPUT     MF100-FILE
      *
      *    レセプト明細読込
           PERFORM 900-MF100-READ-SEC
      *
      *    データ集計処理
           PERFORM 20012-DATA-SHUKEI-SEC
                   UNTIL   FLG-END     =  1
      *
           IF  CNT-RECE02  >  0  AND  FLG-END2  =  0
               PERFORM   2002-PRINT-SEC
           END-IF
      *
           MOVE    1   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               INITIALIZE                            WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(IDX)        TO  WRK-CHK-KOHNUM(1)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
                   PERFORM 20011-SYUKEI-SEC
               END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
           INITIALIZE      MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-SKYYM        TO  MF100-SKYYM
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-KOHFTNJANUM (IDX)  TO  MF100-FTNJANUM
           MOVE  RECE02-KOHJKYSNUM  (IDX)  TO  MF100-KOHJKYSNUM
           MOVE  RECE02-KOHJKYSNUM  (IDX)  TO  MF100-JKYSNUM
           MOVE  RECE02-KOHID       (IDX)  TO  MF100-KOHID
           MOVE  RECE02-KOHINF      (IDX)  TO  MF100-KOHINF
           COMPUTE IDY     =       IDX     +   1
           MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSUINF
           MOVE  RECE02-TOTALTENINF (IDY)  TO  MF100-TOTALTENINF
           MOVE  RECE02-FTNMONEYINF (IDY)  TO  MF100-FTNMONEYINF
           MOVE  RECE02-YKZFTNINF   (IDY)  TO  MF100-YKZFTNINF
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
      *
           IF      RECE02-NYUGAIKBN   =    "1"
             MOVE  "key33"            TO   WRK-PATH
           ELSE
             MOVE  "key25"            TO   WRK-PATH
           END-IF
           PERFORM  800-SYUNOU-SET-SEC
           MOVE  WRK-KOHFTN  TO  MF100-ITBFTN
           MOVE  WRK-YKZFTN  TO  MF100-YKZFTN
      *
      *    請求管理テーブル読み込み
           PERFORM 900-SEIKYU-START-SEC
      *
           IF   MF100-ITBFTN  NOT =  ZERO
           OR   RECE10-SKYSKJ-ICHIBUFTN  NOT =  ZERO
              WRITE  MF100-REC
              ADD    1       TO  CNT-MF100
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ集計処理
      *****************************************************************
       20012-DATA-SHUKEI-SEC  SECTION.
      *
           INITIALIZE IDX1
      *
           EVALUATE TRUE
             WHEN  MF100-KOHJKYSNUM(1:2) = "01"
                   MOVE   1  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "02"
                   MOVE   2  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "04"
                   MOVE   3  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "05"
                   MOVE   4  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "06"
                   MOVE   5  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "07"
                   MOVE   6  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "08" 
                   MOVE   7  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "09" 
                   MOVE   8  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "10" 
                   MOVE   9  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "63"
                   MOVE   10  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "74"
                   MOVE   11  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "77"
                   MOVE   12  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "79"
                   MOVE   13  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "80"
                   MOVE   14  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "81"
                   MOVE   15  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "82"
                   MOVE   16  TO  IDX1
             WHEN  MF100-KOHJKYSNUM(1:2) = "83"
                   MOVE   17  TO  IDX1
             WHEN  OTHER
               MOVE    "判別不明な受給者番号です。番号："
                                          TO  WRK-RECEERR
               MOVE  MF100-KOHJKYSNUM     TO  WRK-RECEERR(33:)
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-END
               MOVE    1                  TO  FLG-END2
      *
           END-EVALUATE
      *
           EVALUATE TRUE
      *
             WHEN  MF100-KOHNUM = "191"
                   MOVE   1  TO  IDX2
             WHEN  MF100-KOHNUM = "193"
                   MOVE   2  TO  IDX2
             WHEN  MF100-KOHNUM = "194"
                   MOVE   3  TO  IDX2
             WHEN  MF100-KOHNUM = "196"
                   MOVE   4  TO  IDX2
             WHEN  MF100-KOHNUM = "198"
                   MOVE   5  TO  IDX2
      *
           END-EVALUATE
      *
      *    一部負担金
           ADD  MF100-ITBFTN  TO  WRK-ITBFTN (IDX2 IDX1) 
           ADD  MF100-ITBFTN  TO  WRK-ITBFTN (6 IDX1)
           ADD  MF100-ITBFTN  TO  WRK-ITBFTN (IDX2 18)
      *
      *    件数
           ADD  1             TO  WRK-KENSU (IDX2 IDX1)
           ADD  1             TO  WRK-KENSU (6 IDX1)
           ADD  1             TO  WRK-KENSU (IDX2 18)
      *
      *    レセプト明細読込
           PERFORM 900-MF100-READ-SEC
      *
      *    負担金合計
           COMPUTE  WRK-ITBFTN(6 18) = WRK-ITBFTN(1 18) +
                                       WRK-ITBFTN(2 18) +
                                       WRK-ITBFTN(3 18) +
                                       WRK-ITBFTN(4 18) +
                                       WRK-ITBFTN(5 18)
      *    件数合計
           COMPUTE  WRK-KENSU(6 18)  = WRK-KENSU(1 18) +
                                       WRK-KENSU(2 18) +
                                       WRK-KENSU(3 18) +
                                       WRK-KENSU(4 18) +
                                       WRK-KENSU(5 18)
            .
       20012-DATA-SHUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
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
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC  SECTION.
      *
           MOVE  ZERO  TO  CNT-LINE
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC1803
      *
      *    請求日付
           MOVE  WRK-DENPPRTYMWH    TO  SC1803-SRYYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN   TO  SC1803-HOSPCD
      *
      *    住所
           MOVE  WRK-HOSPADRS       TO  SC1803-HOSPADRS
           INSPECT SC1803-HOSPADRS  REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME       TO  SC1803-HOSPNAME
           INSPECT SC1803-HOSPNAME  REPLACING ALL "  " BY "　"
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC  SECTION.
      *
           PERFORM   VARYING       IDX1    FROM    1   BY  1
                        UNTIL   IDX1         >  18
               PERFORM   VARYING       IDX2    FROM    1   BY  1
                        UNTIL   IDX2         >  6
      *
                 MOVE  WRK-ITBFTN (IDX2 IDX1)  TO WRK-ITBFTNX
                 MOVE  WRK-ITBFTNX  TO  SC1803-ITBFTN (IDX2 IDX1)
                 MOVE  WRK-KENSU (IDX2 IDX1)  TO WRK-KENSUX
                 MOVE  WRK-KENSUX  TO  SC1803-KENSU (IDX2 IDX1)
      *
               END-PERFORM
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC  SECTION.
      *
           INITIALIZE                     ORCSPRTAREA
           MOVE    "INS"                  TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM    TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                          TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                          TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM     TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD    TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                          TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                          TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                          TO  SPRT-PRIORITY
           MOVE   "SC1803.red"            TO  SPRT-PRTID
           MOVE    "医療費助成事業総括表"
                                          TO  SPRT-TITLE
           MOVE    SC1803                 TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID      TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM     TO  SPRT-PRTNM
           MOVE    "2"                    TO  SPRT-SITEKBN
           CALL    "ORCSPRT"              USING
                                          ORCSPRTAREA
                                          SPA-AREA
           IF      SPRT-RETURN            =   ZERO
               CONTINUE
           ELSE
               MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR  TO  RECEERR-REC
             WRITE RECEERR-REC
      *
      *        ジョブ終了処理
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
      *
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1803 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1803 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU1803 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
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
                    OR       WRK-TAISYO-KOHNUM(IDXX)  =  SPACE
               IF  WRK-CHK-KOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
               OR  WRK-CHK-KOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
                   MOVE     1                 TO  FLG-TAISYO
                   MOVE     100               TO  IDXX
               END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       800-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  WRK-KOHNUM
             INITIALIZE  WRK-KOHFTN
             INITIALIZE  WRK-YKZFTN
             INITIALIZE  SYUNOU-REC
      *
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  RECE02-KOHNUM(IDX) TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"     TO  MCP-TABLE
               MOVE  WRK-PATH         TO  MCP-PATHNAME
               PERFORM  805-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  MF100-SRYYM  =  SYU-SRYYMD(1:6)
                   PERFORM  810-SEIKYU-SEC
                 END-IF
      *
                 MOVE  "tbl_syunou"   TO  MCP-TABLE
                  MOVE  WRK-PATH      TO  MCP-PATHNAME
                 PERFORM   805-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR" UPON CONSOLE
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       800-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       805-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1            TO  FLG-SYUNOU
           END-IF
      *
           .
       805-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療情報　　処理
      *****************************************************************
       810-SEIKYU-SEC  SECTION.
      *
      *    負担者番号取得
           IF  SYU-KOH1HKNNUM = "191" OR
               SYU-KOH2HKNNUM = "191" OR
               SYU-KOH3HKNNUM = "191" OR
               SYU-KOH4HKNNUM = "191"
               MOVE  "191"  TO  WRK-KOHNUM
               PERFORM  820-PTKOHINF-SET-SEC
           END-IF
      *
           IF  SYU-KOH1HKNNUM = "193" OR
               SYU-KOH2HKNNUM = "193" OR
               SYU-KOH3HKNNUM = "193" OR
               SYU-KOH4HKNNUM = "193"
               MOVE  "193"  TO  WRK-KOHNUM
               PERFORM  820-PTKOHINF-SET-SEC
           END-IF
      *
           IF  SYU-KOH1HKNNUM = "194" OR
               SYU-KOH2HKNNUM = "194" OR
               SYU-KOH3HKNNUM = "194" OR
               SYU-KOH4HKNNUM = "194"
               MOVE  "194"  TO  WRK-KOHNUM
               PERFORM  820-PTKOHINF-SET-SEC
           END-IF
      *
           IF  SYU-KOH1HKNNUM = "196" OR
               SYU-KOH2HKNNUM = "196" OR
               SYU-KOH3HKNNUM = "196" OR
               SYU-KOH4HKNNUM = "196"
               MOVE  "196"  TO  WRK-KOHNUM
               PERFORM  820-PTKOHINF-SET-SEC
           END-IF
      *
           IF  SYU-KOH1HKNNUM = "198" OR
               SYU-KOH2HKNNUM = "198" OR
               SYU-KOH3HKNNUM = "198" OR
               SYU-KOH4HKNNUM = "198"
               MOVE  "198"  TO  WRK-KOHNUM
               PERFORM  820-PTKOHINF-SET-SEC
           END-IF
      *
           EVALUATE TRUE
             WHEN  SYU-KOH1HKNNUM = "191" OR "193" OR "194" OR "196"
                                    OR "198"
                 COMPUTE  WRK-KOHFTN = WRK-KOHFTN + SYU-KOH1COMPTOTAL
      *
             WHEN  SYU-KOH2HKNNUM = "191" OR "193" OR "194" OR "196"
                                    OR "198"
                 COMPUTE  WRK-KOHFTN = WRK-KOHFTN + SYU-KOH2COMPTOTAL
      *
             WHEN  SYU-KOH3HKNNUM = "191" OR "193" OR "194" OR "196"
                                    OR "198"
                 COMPUTE  WRK-KOHFTN = WRK-KOHFTN + SYU-KOH3COMPTOTAL
      *
             WHEN  SYU-KOH4HKNNUM = "191" OR "193" OR "194" OR "196"
                                    OR "198"
                 COMPUTE  WRK-KOHFTN = WRK-KOHFTN + SYU-KOH4COMPTOTAL
           END-EVALUATE
      *
           .
       810-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  患者公費情報セット
      *****************************************************************
       820-PTKOHINF-SET-SEC  SECTION.
      *
             INITIALIZE  PTKOHINF-REC
      *
             MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
             MOVE  RECE02-PTID        TO  PTKOH-PTID
             MOVE  MF100-KOHID        TO  PTKOH-KOHID
             MOVE  PTKOHINF-REC       TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
             MOVE  "key"              TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_ptkohinf"   TO  MCP-TABLE
               MOVE  "key"            TO  MCP-PATHNAME
               PERFORM  825-PTKOHINF-READ-SEC
      *
             ELSE
               DISPLAY  "PTKOHINF SELECT ERR" UPON CONSOLE
             END-IF
      *
             INITIALIZE  MCPAREA
      *
             MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
             MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
             MOVE  "key"              TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       820-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       825-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1            TO  FLG-PTKOHINF
           END-IF
      *
           .
       825-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-MF100-READ-SEC  SECTION.
      *
           READ  MF100-FILE  NEXT
             AT  END
               MOVE  1           TO  FLG-END
             NOT AT  END
               ADD   1           TO  CNT-MF100
               MOVE    MF100-SRYYM     TO  KEY-N-SRYYM
           END-READ
           .
       900-MF100-READ-EXT.
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
               PERFORM 900-KOHSKY-CLOSE-SEC
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
      *
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE02-KOHNUM(1)        TO  WRK-CHK-KOHNUM(1)
               MOVE    RECE02-KOHNUM(2)        TO  WRK-CHK-KOHNUM(2)
               MOVE    RECE02-KOHNUM(3)        TO  WRK-CHK-KOHNUM(3)
               MOVE    RECE02-KOHNUM(4)        TO  WRK-CHK-KOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
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
                       PERFORM  900-KOHSKY-READ-SEC
                   END-IF
      *
      *           個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO        TO  FLG-ERR
                         END-IF
                     END-PERFORM 
                   END-IF
                   DISPLAY "患者番号=" RECE02-PTNUM
                           "負担者番号=" RECE02-KOHFTNJANUM(1)
                           "保険者番号=" RECE02-HKNJANUM
      *
                   IF      FLG-ERR           =   1
                     PERFORM  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                   PERFORM  900-KOHSKY-READ-SEC
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理　読み込み 
      *****************************************************************
       900-SEIKYU-START-SEC            SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY          TO  RECE10-KEY
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-RECE10
                   MOVE    MCPDATA-REC TO  RECE10-REC
               ELSE
                   MOVE    1           TO  FLG-RECE10
               END-IF  
           ELSE
               MOVE   1                TO  FLG-RECE10
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SEIKYU-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"                TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE    "tbl_ptinf"            TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
               IF      MCP-RC              =  ZERO
                   MOVE    ZERO               TO  FLG-PTINF
                   MOVE    MCPDATA-REC        TO  PTINF-REC
               ELSE
                   MOVE    1                  TO  FLG-PTINF
                   INITIALIZE                 PTINF-REC
               END-IF
           ELSE
               MOVE    1                      TO  FLG-PTINF
               INITIALIZE                     PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"                TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  LNK-PRTKANRI-SKYYMD  TO  ORC-DBYMD
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE  "tbl_syskanri"       TO  MCP-TABLE
           MOVE  "key10"              TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"          TO  MCP-FUNC
             MOVE  "tbl_syskanri"     TO  MCP-TABLE
             MOVE  "key10"            TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO             TO  FLG-SYSKANRI
             ELSE
               MOVE  1                TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1                  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE  "tbl_syskanri"       TO  MCP-TABLE
           MOVE  "key10"              TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
       .
       800-SYSKANRI-READ-EXT.
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           PERFORM 800-SYSKANRI-READ-SEC
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"              TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"             TO  MCP-FUNC.
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
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBCOMMIT"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
