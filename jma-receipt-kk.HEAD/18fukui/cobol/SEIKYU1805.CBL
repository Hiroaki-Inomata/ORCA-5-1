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
      *****************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU1805.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 福井・医療費助成事業対象者一覧表
      *                      (191,193,194,196,198)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/04/05     中川
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者  日付         内容
      *  01.00.01       吉川    2007/05/18   帳票様式変更に伴う修正と
      *                                      仕様変更に伴う修正
      *  01.00.02       長屋    2007/08/01   食事医療費負担額に関する修正
      *                                      本人家族区分の印字値の振り分け修正
      *  01.00.03       吉川    2007/08/09   食事療養負担合計の計算修正
      *                                      特定負無(公費番号=091)の患者を
      *                                      帳票に表示させないように修正
      *  01.00.04       吉川    2007/10/05   日レセver4.0対応
      *                                      システム管理マスタ「1901」の
      *                                      医療機関情報を参照
      *  01.00.05       吉川    2007/10/25   保険番号でソートする
      *  01.00.06       吉川    2008/02/26   負担金が発生しない場合は
      *                                      印字対象外とする
      *                                      受給者番号でソートする
      *  01.00.07       吉川    2008/05/16   H20年4月改正対応
      *  01.00.08       吉川    2008/06/06   請求を取り消した情報は
      *                                      取得しないように修正
      *  01.00.09       吉川    2008/07/14   本人家族区分は
      *                                      レセプト種別区分から取得する
      *  01.00.10       吉川    2008/10/17   点数欄は保険の総点数を印字する
      *  01.00.11       吉川    2008/11/17   収納テーブルの入院データが
      *                                      正しく取得されないのを修正
      *                                      同月内に保険を切替えた場合の
      *                                      点数と負担金の不具合を修正
      *  01.00.12       吉川    2009/04/02   特定負無の法別と点数を記載する
      *  01.00.13       吉川    2011/02/17   open-cobol1.0対応
      *                                      公費請求テーブルclose処理位置変更
      *  01.00.14       吉川    2011/06/20   実日数欄は総日数を印字する
      *  01.00.15       吉川    2011/12/16   同月内で受給者番号が変わっても
      *                                      帳票に反映されないのを修正
      *  01.00.16       吉川    2012/04/13   同月内に公費を変更した場合
      *                                      請求点数と実日数はそれぞれの
      *                                      公費で診療した値を印字する
      *  01.00.17       吉川    2013/01/09   医療機関情報編集対応
      *  01.00.18       吉川    2014/10/09   ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.19       吉川    2015/11/11   患者負担が食事のみのデータは
      *                                      印字対象とするように修正
      *                                      難病の経過措置の場合、食事負担は
      *                                      １／２とするように修正
      *  01.00.20       門間    2016/11/15   対象公費はPGオプションから取得する
      *  01.00.21       門間    2017/03/08   公費にかかる分の点数日数を印字するように修正
      *                                      医療機関コード変更時、
      *                                      医療機関コード別の作成処理対応
      *  01.00.22       萩野    2018/04/09   SITEKBN変更（動作に変更なし）
      *  01.00.23       萩野    2018/11/07   修正履歴コメント削除
      *  01.00.24       萩野    2019/06/12   結核を併用している場合は
      *                                      公費対象患者負担額に印字されるように修正
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF110-FILE   ASSIGN    MF110PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF110-KEY
                               FILE    STATUS  IS  STS-MF110.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF110-FILE.
       01  MF110-REC.
           COPY  "SEI1805.INC".
      *
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
                   BY         //MF110//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
      *    印刷用データ
           COPY  "SC1805.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF110   PIC X(02).
           03  STS-RECE01  PIC X(02).
           03  STS-PARA     PIC X(02).
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
           03  FLG-MF110      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-PRGOPTION  PIC 9(01).
           03  FLG-OK         PIC 9(01).
           03  FLG-TAISYO     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(05).
           03  IDXX           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
      *
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
               05  WRK-COMP-SRYY      PIC  9(4).
               05  WRK-COMP-BIRY      PIC  9(4).
               05  WRK-COMP-SRYM      PIC  9(2).
               05  WRK-COMP-BIR       PIC  9(2).
           03  WRK-KOHFTN             PIC 9(07).
           03  WRK-YKZFTN             PIC 9(07).
      *
           03  WRK-RECEERR            PIC X(200).
           03  WRK-DENPPRTYMWH        PIC X(16).
           03  WRK-SYSYMDWH           PIC X(22).
           03  WRK-SRYYM              PIC X(09).
           03  WRK-POST               PIC X(09).
      *
           03  WRK-Z1                 PIC Z.
           03  WRK-Z2                 PIC ZZ.
           03  WRK-Z6                 PIC ZZ,ZZZ.
           03  WRK-Z7                 PIC ZZZ,ZZZ.
           03  WRK-Z10                PIC ZZ,ZZZ,ZZZ.
      *
           03  WRK-SRYYMWH            PIC X(16).
           03  WRK-SEIYMDWH           PIC X(22).
           03  WRK-PAGE               PIC 9(02).
           03  WRK-AMARI              PIC 9(02).
           03  WRK-GKINGAKU           PIC 9(07).
           03  WRK-GOKEI.
               05  WRK-GKENSU         PIC 9(02).
               05  WRK-GITBFTN        PIC 9(07).
               05  WRK-GKOHFTN        PIC 9(07).
               05  WRK-GSHOKUFTN      PIC 9(07).
               05  WRK-GKOHSHOKUFTN   PIC 9(07).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMDG1        PIC X(09).
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
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
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
      *    年齢
           03  WRK-CALC-YMD1.
              05  WRK-CALC-YY1     PIC 9(04).
              05  WRK-CALC-MM1     PIC 9(02).
              05  WRK-CALC-DD1     PIC 9(02).
      *
           03  WRK-CALC-YMD2.
              05  WRK-CALC-YY2     PIC 9(04).
              05  WRK-CALC-MM2     PIC 9(02).
              05  WRK-CALC-DD2     PIC 9(02).
           03  WRK-AGE-YY             PIC S9(04).
           03  WRK-AGE-MM             PIC S9(02).
           03  WRK-AGE-G.
              05  WRK-AGE-Z           PIC ZZ9.
              05  WRK-AGE-MEI         PIC X(02).
              05                      PIC X(01).
           03  WRK-AGE-M-R     REDEFINES   WRK-AGE-G.
              05  WRK-AGE-M-Z         PIC Z9.
              05  WRK-AGE-M-MEI       PIC X(04).
      *
      *市町村コード確認用
       01  WRK-DATA.
           03  WRK-CITYCD      PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE01  PIC 9(06).
           03  CNT-RECE10  PIC 9(06).
           03  CNT-MF110   PIC 9(06).
           03  CNT-LINE  PIC 9(02).
           03  CNT-PAGE  PIC 9(03).
           03  CNT-PRINT PIC 9(06).
      *
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
               05  SYS-YY  PIC 9(02).
               05  SYS-MM  PIC 9(02).
               05  SYS-DD  PIC 9(02).
           03  SYS-TIME    PIC 9(08).
      *
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
           COPY  "CPSHELLTBL.INC".
      *
      *    DB参照用
           COPY  "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY  "CPSK1005.INC".
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
           COPY  "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    公費請求書
           COPY  "CPKOHSKY.INC"
                 REPLACING  //KOHSKY//
                 BY         //RECE01//.
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
           COPY  "CPPTKOHINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
           COPY    "CPRCF010.INC".
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
           COPY  "CPORCSJOBKANRI.INC".
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *    日付変換サブ
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
           COPY  "MCPDATA.INC".
           COPY  "CPORCMCP.INC".
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
           MOVE    "SEIKYU1805"    TO  JOB-PGID
           MOVE    "医療費助成事業対象者一覧表"  
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
           MOVE  WRK-PARA-HOSPNUM     TO  MF110PARA-HOSPNUM
           MOVE  "MF110ZZ"            TO  MF110PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF110PARA-TERMID
      *
           MOVE  MF110PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF110PARA-FULLNAME
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
             DISPLAY "*** SEIKYU1805 SYS 1001 ERR ***"
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
             DISPLAY "*** SEIKYU1805 SYS 1002 ERR ***"
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
           MOVE    "SEIKYU1805"              TO  WRK-PRGID
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
                   MOVE    "tbl_btpara"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
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
           OPEN    OUTPUT  MF110-FILE
           CLOSE           MF110-FILE
           OPEN    I-O     MF110-FILE
      *
           MOVE    ZERO    TO      FLG-END
      *
           PERFORM 2001-SYUKEI-SEC
                   UNTIL   FLG-END     =   1
      *
           CLOSE           MF110-FILE
      *
           IF  CNT-RECE01  >       0
               PERFORM   2002-PRINT-SEC
           END-IF
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
           PERFORM  VARYING  IDZ  FROM    1   BY    1
                    UNTIL    IDZ  >       4
               INITIALIZE                            WRK-CHK-KOHNUM-TBL
               MOVE    RECE01-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
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
           INITIALIZE  MF110-REC
      *
           MOVE   SPA-HOSPNUM         TO  MF110-HOSPNUM
           MOVE   RECE01-PTNUM        TO  MF110-PTNUM
           MOVE   RECE01-RECEKA       TO  MF110-SRYKA
           MOVE   RECE01-HKNJANUM-KEY TO  MF110-HKNJANUM-KEY
           MOVE   RECE01-TEKSTYMD     TO  MF110-TEKSTYMD
           MOVE   RECE01-NYUGAIKBN    TO  MF110-NYUGAIKBN
      *
           MOVE   RECE01-PTID         TO  MF110-PTID
           MOVE   RECE01-NAME         TO  MF110-NAME
           MOVE   RECE01-BIRTHDAY     TO  MF110-BIRTHDAY
           MOVE   RECE01-HKN          TO  MF110-HKN
           MOVE   RECE01-RJNHKN       TO  MF110-RJNHKN
           MOVE   RECE01-SKYYM        TO  MF110-SKYYM
           MOVE   RECE01-SRYYM        TO  MF110-SRYYM
      *
           MOVE   RECE01-KOHNUM      (IDZ)  TO  MF110-KOHHKNNUM
           MOVE   RECE01-KOHFTNJANUM (IDZ)  TO  MF110-FTNJANUM
           MOVE   RECE01-KOHJKYSNUM  (IDZ)  TO  MF110-KOHJKYSNUM
           MOVE   RECE01-KOHJKYSNUM(IDZ)(3:) TO MF110-JKYSNUM
           MOVE   RECE01-KOHINF      (IDZ)  TO  MF110-KOHINF
           COMPUTE  IDY    =    IDZ    +   1
           MOVE   RECE01-JNISSUINF   (IDY)  TO  MF110-JNISSUINF
           MOVE   RECE01-TOTALTENINF (IDY)  TO  MF110-TOTALTENINF
           MOVE   RECE01-FTNMONEYINF (IDY)  TO  MF110-FTNMONEYINF
           MOVE   RECE01-SHOKUJINISSU(1)    TO  MF110-SHOKUJIKAISU
           MOVE   RECE01-SHOKUJIRYOYOHI(1)  TO  MF110-SHOKUJIRYOYOHI
           MOVE   RECE01-SHOKUJIFTN(1)      TO  MF110-SHOKUJIFTN
           PERFORM  VARYING  IDX  FROM  1  BY  1  UNTIL  IDX  >  5
             MOVE   RECE01-FLG(IDX)         TO  MF110-FLG(IDX)
           END-PERFORM
           PERFORM  830-CITY-SET-SEC
      *
      *    本人家族区分
      *    年齢を計算
           PERFORM  920-AGE-CALC-SEC
      *
      *    レセプト種別区分から本人家族区分を取得
           MOVE  RECE01-RECESYUBETU(4:1)         TO  MF110-RECEHONKZKKBN
      
      *    国の国保を検索
           PERFORM   910-SEIKYU-START-SEC
      *
           IF   MF110-RJNHBTNUM   =   "027"
              PERFORM  VARYING  IDX  FROM    1    BY    1
                        UNTIL   IDX >       4
                IF       RECE10-KOHNUM (IDX)  =  "091"
                  MOVE   "51"  TO            MF110-RECEKOHHBTNUM(2)
                ELSE
                  MOVE   RECE10-KOHNUM (IDX)(2:2)    TO
                                             MF110-RECEKOHHBTNUM(2)
                END-IF
      *
                  MOVE   RECE10-JNISSU (IDX + 1)         TO
                                             MF110-RECEJNISSU(2)
      *
                  MOVE   RECE10-TOTALTEN (IDX + 1)       TO
                                             MF110-RECETOTALTEN(2)
      *
                  MOVE   RECE10-FTNMONEY (IDX + 1)       TO
                                             MF110-RECEFTNMONEY(2)
      *
                  MOVE   RECE10-SHOKUJINISSU (IDX + 1)   TO
                                             MF110-RECESHOKUJIKAISU(2)
      *
                  MOVE   RECE10-SHOKUJIRYOYOHI (IDX + 1) TO
                                             MF110-RECESHOKUJIRYOYOHI(2)
      *
                  MOVE   RECE10-SHOKUJIFTN (IDX + 1)     TO
                                             MF110-RECESHOKUJIFTN(2)
      *
                  IF   MF110-RECEKOHHBTNUM (2)  NOT  =  SPACE
                    MOVE   4                   TO    IDX
                  END-IF
              END-PERFORM
           ELSE
      *       長期区分
              IF   RECE10-ETC-KOHNUM (1)  =  "972"
              OR   RECE10-ETC-KOHNUM (2)  =  "972"
              OR   RECE10-ETC-KOHNUM (3)  =  "972"
              OR   RECE10-ETC-KOHNUM (4)  =  "972"
              OR   RECE10-ETC-KOHNUM (1)  =  "974"
              OR   RECE10-ETC-KOHNUM (2)  =  "974"
              OR   RECE10-ETC-KOHNUM (3)  =  "974"
              OR   RECE10-ETC-KOHNUM (4)  =  "974"
                MOVE   1                   TO    MF110-CHOKIKBN
              END-IF
      *
              MOVE    2                   TO    IDY
              PERFORM  VARYING  IDX  FROM    1    BY    1
                        UNTIL   IDX >       4
                IF   RECE10-KOHNUM (IDX)  =  "091"
                  MOVE   "51"  TO        MF110-RECEKOHHBTNUM(IDY - 1)
                ELSE
                  MOVE   RECE10-KOHNUM (IDX)(2:2)   TO
                                         MF110-RECEKOHHBTNUM(IDY - 1)
                END-IF
      *
                  MOVE   RECE10-JNISSU (IDY)         TO
                                         MF110-RECEJNISSU(IDY - 1)
      *
                  MOVE   RECE10-TOTALTEN (IDY)       TO
                                         MF110-RECETOTALTEN(IDY - 1)
      *
                 IF   RECE10-KOHNUM (IDX)  =  "010"
                    MOVE   RECE10-FTNMONEY-KOJ (IDY)   TO
                                         MF110-RECEFTNMONEY(IDY - 1)
                 ELSE
                    MOVE   RECE10-FTNMONEY (IDY)       TO
                                         MF110-RECEFTNMONEY(IDY - 1)
                 END-IF
      *
                  MOVE   RECE10-SHOKUJINISSU (IDY)   TO
                                         MF110-RECESHOKUJIKAISU(IDY - 1)
      *
                  MOVE   RECE10-SHOKUJIRYOYOHI (IDY) TO
                                       MF110-RECESHOKUJIRYOYOHI(IDY - 1)
      *
                  MOVE    RECE10-KOHNUM (IDX)    TO  WRK-KOHNUM
                  MOVE    RECE10-KOHID  (IDX)    TO  WRK-KOHID
                  PERFORM 820-PTKOHINF-SET-SEC
      *           難病の経過措置か
                  IF  RECE10-KOHNUM (IDX)  =  "054"
                  AND PTKOH-FTNJANUM(5:3)  =  "501"
                      COMPUTE MF110-RECESHOKUJIFTN(IDY - 1) = 
                              RECE10-SHOKUJIFTN(IDY)  /  2
                  ELSE
                      MOVE   RECE10-SHOKUJIFTN (IDY) TO
                                         MF110-RECESHOKUJIFTN(IDY - 1)
                  END-IF
      *
                  IF   RECE10-KOHNUM (IDX + 1)   =   SPACE
                    MOVE   4                   TO    IDX
                  END-IF
                  ADD   1                   TO    IDY
              END-PERFORM
           END-IF
      *
           IF      RECE01-NYUGAIKBN   =    "1"
               MOVE    "key33"        TO   WRK-PATH
           ELSE
               MOVE    "key25"        TO   WRK-PATH
           END-IF
           MOVE   MF110-KOHID         TO   WRK-KOHID
      *
      *    主保険の点数を取得
      *    公費２以降に値がある、及びIDZが２以降の場合、
      *    月途中に公費の変更があったとみなす
           MOVE   RECE01-TOTALTENINF(IDZ + 1)
                                      TO  MF110-TOTALTENINF
           MOVE   RECE01-JNISSUINF(IDZ + 1)
                                      TO  MF110-JNISSUINF
           PERFORM   800-SYUNOU-SET-SEC
           MOVE   WRK-KOHFTN          TO   MF110-ITBFTN
      *
      *    請求管理の「請求額情報（食事）−患者負担」を使用する
           IF  MF110-NYUGAIKBN  =  "1"
               MOVE    RECE10-SKYSKJ-ICHIBUFTN
                                      TO  MF110-SKY-SHOKUJIFTN
           END-IF
      *
           IF  MF110-ITBFTN  NOT =  ZERO
           OR  MF110-SKY-SHOKUJIFTN  NOT =  ZERO
             WRITE  MF110-REC
             ADD    1                 TO   CNT-MF110
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ集計処理
      *****************************************************************
       830-CITY-SET-SEC    SECTION.
      *
           INITIALIZE  FLG-END2
      *
           EVALUATE TRUE
             WHEN  MF110-KOHJKYSNUM(1:2) = "01"
                   MOVE   "福井市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "02"
                   MOVE   "敦賀市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "04"
                   MOVE   "小浜市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "05"
                   MOVE   "大野市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "06"
                   MOVE   "勝山市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "07"
                   MOVE   "鯖江市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "08"
                   MOVE   "あわら市"     TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "09"
                   MOVE   "越前市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "10"
                   MOVE   "坂井市"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "51"
                   MOVE   "美山町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "52"
                   MOVE   "松岡町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "53"
                   MOVE   "永平寺町"     TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "54"
                   MOVE   "上志比村"     TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "55"
                   MOVE   "和泉村"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "56"
                   MOVE   "三国町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "59"
                   MOVE   "丸岡町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "60"
                   MOVE   "春江町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "61"
                   MOVE   "坂井町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "63"
                   MOVE   "池田町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "70"
                   MOVE   "越廼村"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "72"
                   MOVE   "清水町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "74"
                   MOVE   "美浜町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "76"
                   MOVE   "名田庄村"     TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "77"
                   MOVE   "高浜町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "78"
                   MOVE   "大飯町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "79"
                   MOVE   "南越前町"     TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "80"
                   MOVE   "越前町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "81"
                   MOVE   "若狭町"       TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "82"
                   MOVE   "永平寺町"     TO    MF110-CITYNAME
             WHEN  MF110-KOHJKYSNUM(1:2) = "83"
                   MOVE   "おおい町"     TO    MF110-CITYNAME
      *
             WHEN  OTHER
               MOVE    "判別不明な受給者番号です。番号："
                                         TO  WRK-RECEERR
               MOVE  MF110-KOHJKYSNUM    TO  WRK-RECEERR(33:)
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                 TO  FLG-END
               MOVE    1                 TO  FLG-END2
           END-EVALUATE
           MOVE   MF110-KOHJKYSNUM(1:2)  TO    MF110-CITYCD
      *
            .
       830-CITY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       MF110-FILE
      *
           MOVE     ZERO        TO       FLG-END
      *
      *    レセプト明細読込
           PERFORM 900-MF110-READ-SEC
      *
           PERFORM  UNTIL       FLG-END  =  1
      *
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *
           END-PERFORM
      *
           CLOSE    MF110-FILE
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
           MOVE  ZERO                    TO  CNT-LINE
           ADD   1                       TO  CNT-PAGE
      *
           INITIALIZE  SC1805
           INITIALIZE  WRK-GOKEI
      *
      *    請求日付
           MOVE  WRK-DENPPRTYMWH         TO  SC1805-SKYYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN        TO  SC1805-HOSPCD
      *
      *    郵便番号
           MOVE  SYS-1002-POST (1:3)     TO  WRK-POST (1:3)
           MOVE  "-"                     TO  WRK-POST (4:1)
           MOVE  SYS-1002-POST (4:)      TO  WRK-POST (5:)
           MOVE  WRK-POST                TO  SC1805-POST
      *
      *    住所
           MOVE  WRK-HOSPADRS            TO  SC1805-HOSPADRS
           INSPECT SC1805-HOSPADRS       REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME            TO  SC1805-HOSPNAME
           INSPECT SC1805-HOSPNAME       REPLACING ALL "  " BY "　"
      *
      *    電話番号
           MOVE  "電話番号"              TO  SC1805-HOSPTEL(1:8)
           MOVE  WRK-HOSPTEL             TO  SC1805-HOSPTEL(9:)
      *
      *    FAX
           MOVE  "ＦＡＸ"                TO  SC1805-HOSPFAX(1:6)
           MOVE  SYS-1002-FAX            TO  SC1805-HOSPFAX(7:)
      *
      *    市町村コード
           MOVE  MF110-CITYCD            TO  WRK-CITYCD
           MOVE  MF110-CITYCD            TO  SC1805-CITYCD
      *
      *    市町村名称
           MOVE  MF110-CITYNAME          TO  SC1805-CITYNAME
      *
      *    ページ数
           MOVE  CNT-PAGE                TO  SC1805-PAGE
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC  SECTION.
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                     UNTIL   ( IDX1  > 5 )
                     OR      ( WRK-CITYCD  NOT  =  MF110-CITYCD )
                     OR      ( FLG-END          =  1 )
      *　診療月
             IF MF110-SRYYM(5:1) = "0"
                 MOVE MF110-SRYYM(6:1)  TO    SC1805-SRYM (IDX1)
             ELSE
                 MOVE MF110-SRYYM(5:2)  TO    SC1805-SRYM (IDX1)
             END-IF
      *　受給資格者番号
             MOVE  MF110-HKNJANUM       TO    SC1805-HKNJANUM (IDX1)
      *　患者氏名
             MOVE  MF110-NAME           TO    SC1805-PTNAME (IDX1)
      *　生年月日
      *
             MOVE  MF110-BIRTHDAY       TO    WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             EVALUATE  WRK-HENYMDG1(1:1)
                 WHEN    "M"
                     MOVE    "○"            TO  SC1805-GENGO (IDX1 1)
                 WHEN    "T"
                     MOVE    "○"            TO  SC1805-GENGO (IDX1 2)
                 WHEN    "S"
                     MOVE    "○"            TO  SC1805-GENGO (IDX1 3)
                 WHEN    "H"
                     MOVE    "○"            TO  SC1805-GENGO (IDX1 4)
             END-EVALUATE
             MOVE    WRK-HENYMDG(5:4)        TO  SC1805-TANJOYY (IDX1)
             MOVE    WRK-HENYMDG(11:4)       TO  SC1805-TANJOMM (IDX1)
             MOVE    WRK-HENYMDG(17:4)       TO  SC1805-TANJODD (IDX1)
      *       MOVE  WRK-HENYMDG          TO    SC1805-BIRTHDAY (IDX1)
      *　保険者番号
             MOVE  MF110-KOHJKYSNUM(3:17)      
                                        TO    SC1805-JKYSNUM (IDX1)
      *　記号
             MOVE  MF110-KIGO           TO    SC1805-HKNKIGO (IDX1)
      *　番号
             MOVE  MF110-NUM            TO    SC1805-HKNBANGO (IDX1)
      *　本人家族区分
             MOVE  MF110-RECEHONKZKKBN  TO    SC1805-HONKZKKBN (IDX1)
      *　診療科（レセプト）
             MOVE  MF110-SRYKA          TO    WRK-Z2
             MOVE  WRK-Z2               TO    SC1805-SRYKA (IDX1)
      *　長期区分
             MOVE  MF110-CHOKIKBN       TO    WRK-Z1
             MOVE  WRK-Z1               TO    SC1805-CHOKI (IDX1)
      *　給付割合
      *      前期高齢特例措置か
             IF   MF110-SRYYM  >=  WRK-CONS-SRYYM-200804
             AND  MF110-FLG (2)  =  1
                  MOVE  8               TO    WRK-Z2
                  MOVE  WRK-Z2          TO    SC1805-KYUFURATE (IDX1)
             ELSE
               COMPUTE  WRK-Z2  =  ( 100   -   MF110-KYURATE )  /  10
               MOVE  WRK-Z2             TO    SC1805-KYUFURATE (IDX1)
             END-IF
      *　日数
             MOVE  MF110-JNISSU         TO    WRK-Z2
             MOVE  WRK-Z2               TO    SC1805-SRYONISSU (IDX1)
      *　食事回数
             MOVE  MF110-SHOKUJIKAISU   TO    WRK-Z2
             MOVE  WRK-Z2               TO    SC1805-SHOKUKAISU (IDX1)
      *　点数
             MOVE  MF110-TOTALTEN       TO    WRK-Z7
             MOVE  WRK-Z7               TO    SC1805-TENSU (IDX1)
      *　【ページ合計計算】
             ADD   MF110-SHOKUJIFTN     TO    WRK-GSHOKUFTN
      *　食事療養費金額
             MOVE  MF110-SHOKUJIRYOYOHI TO    WRK-Z7
             MOVE  WRK-Z7               TO    SC1805-SHOKUMONEY (IDX1)
      *　【ページ合計計算】
             ADD   MF110-SKY-SHOKUJIFTN TO    MF110-ITBFTN
             ADD   MF110-ITBFTN         TO    WRK-GITBFTN
      *　一部負担金
             MOVE  MF110-ITBFTN         TO    WRK-Z7
             MOVE  WRK-Z7               TO    SC1805-ITBFTNMONEY (IDX1)
      *　食事療養費標準負担額
             MOVE  MF110-SHOKUJIFTN     TO    WRK-Z7
             MOVE  WRK-Z7               TO   SC1805-SHOKFTNMONEY (IDX1)
      *
      *　公費負担医療等　集計
      *
             PERFORM VARYING IDX2 FROM    1   BY  1
                     UNTIL   IDX2 >       2
                IF    MF110-RJNHBTNUM     =    "27"
      *　法別番号（老人）
                  MOVE  MF110-RJNHBTNUM     TO
                                              SC1805-KOHHOBETU (IDX1 1)
      *　法別番号
                  MOVE  MF110-RECEKOHHBTNUM (2)       TO
                                           SC1805-KOHHOBETU (IDX1 2)
      *　日数
                  MOVE  MF110-RECEJNISSU (2)          TO      WRK-Z2
                  MOVE  WRK-Z2   TO         SC1805-KOHNISSU (IDX1 2)
      *　公費対象点数
                  MOVE  MF110-RECETOTALTEN (2)        TO      WRK-Z7
                  MOVE  WRK-Z7   TO           SC1805-KOHTEN (IDX1 2)
      *　【ページ合計計算】
                  ADD   MF110-RECEFTNMONEY (2)        TO  WRK-GKOHFTN
      *　公費対象者負担額
                  MOVE  MF110-RECEFTNMONEY (2)        TO      WRK-Z7
                  MOVE  WRK-Z7   TO      SC1805-KOHFTNMONEY (IDX1 2)
      *　食事回数
                  MOVE  MF110-RECESHOKUJIKAISU (2)    TO      WRK-Z2
                  MOVE  WRK-Z2   TO     SC1805-KOHSHOKKAISU (IDX1 2)
      *　食事療養費金額
                  MOVE  MF110-RECESHOKUJIRYOYOHI (2)  TO      WRK-Z7
                  MOVE  WRK-Z7   TO    SC1805-KOHSHOKUMONEY (IDX1 2)
      *　【ページ合計計算】
                  ADD   MF110-RECESHOKUJIFTN (2)      TO
                                                     WRK-GKOHSHOKUFTN
      *食事療養費標準負担額
                  MOVE  MF110-RECESHOKUJIFTN (2)      TO      WRK-Z7
                  MOVE  WRK-Z7  TO  SC1805-KOHSHOKUFTNMONEY (IDX1 2)
      *
                ELSE
                  MOVE  MF110-RECEKOHHBTNUM (IDX2)       TO
                                            SC1805-KOHHOBETU (IDX1 IDX2)
      *
                  MOVE  MF110-RECEJNISSU (IDX2)          TO      WRK-Z2
                  MOVE  WRK-Z2   TO         SC1805-KOHNISSU (IDX1 IDX2)
      *
                  MOVE  MF110-RECETOTALTEN (IDX2)        TO      WRK-Z7
                  MOVE  WRK-Z7   TO           SC1805-KOHTEN (IDX1 IDX2)
      * ページ合計：公費対象患者負担額
                  ADD   MF110-RECEFTNMONEY (IDX2)        TO WRK-GKOHFTN
      *
                  MOVE  MF110-RECEFTNMONEY (IDX2)        TO      WRK-Z7
                  MOVE  WRK-Z7   TO      SC1805-KOHFTNMONEY (IDX1 IDX2)
      *
                  MOVE  MF110-RECESHOKUJIKAISU (IDX2)    TO      WRK-Z2
                  MOVE  WRK-Z2   TO     SC1805-KOHSHOKKAISU (IDX1 IDX2)
      *
                  MOVE  MF110-RECESHOKUJIRYOYOHI (IDX2)  TO      WRK-Z7
                  MOVE  WRK-Z7   TO    SC1805-KOHSHOKUMONEY (IDX1 IDX2)
      * ページ合計：公費負担分食事療養費標準負担額
                  ADD   MF110-RECESHOKUJIFTN (IDX2)      TO
                                                        WRK-GKOHSHOKUFTN
      *
                  MOVE  MF110-RECESHOKUJIFTN (IDX2)      TO      WRK-Z7
                  MOVE  WRK-Z7  TO  SC1805-KOHSHOKUFTNMONEY (IDX1 IDX2)
                END-IF
             END-PERFORM
             ADD   1                       TO    WRK-GKENSU
      *    レセプト明細読込
             PERFORM 900-MF110-READ-SEC
      *
           END-PERFORM
      *
      * ページ合計
           MOVE  WRK-GKENSU              TO    WRK-Z2
           MOVE  WRK-Z2                  TO    SC1805-PKENSU
      *
           MOVE  WRK-GITBFTN             TO    WRK-Z7
           MOVE  WRK-Z7                  TO    SC1805-PITFTN
      *
           MOVE  WRK-GKOHFTN             TO    WRK-Z7
           MOVE  WRK-Z7                  TO    SC1805-PKOHFTN
      *
           MOVE  WRK-GSHOKUFTN           TO    WRK-Z7
           MOVE  WRK-Z7                  TO    SC1805-PSHOKFTN
      *
           MOVE  WRK-GKOHSHOKUFTN        TO    WRK-Z7
           MOVE  WRK-Z7                  TO    SC1805-PKOHSHOKFTN
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
           INITIALIZE  ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO  SPRT-PRIORITY
           MOVE   "SC1805.red"             TO  SPRT-PRTID
           MOVE    "医療費助成事業対象者一覧表"  TO  SPRT-TITLE
           MOVE    SC1805                  TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO  SPRT-PRTNM
           MOVE    "2"                     TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                          SPA-AREA
           IF      SPRT-RETURN   =         ZERO
              CONTINUE
           ELSE
              MOVE   "帳票ＤＢに更新できませんでした"
                                             TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
           END-IF
                 .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC  SECTION.
      *
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE  "21"                TO  LNK-DAY2-IRAI
           MOVE  WRK-SYMD            TO  LNK-DAY2-YMD
           CALL  "ORCSDAY"           USING  STS-AREA-DAY
                                            LNK-DAY2-AREA
           MOVE  LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE  LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR   =   ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR             TO  RECEERR-REC
             WRITE  RECEERR-REC
             CLOSE  RECEERR-FILE
      *         
      *        ジョブ終了処理
               MOVE   "JBE"                   TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    SPA-HOSPNUM            TO  JOB-HOSPNUM
               MOVE   WRK-PARA-JOBID          TO  JOB-JOBID
               MOVE   WRK-PARA-SHELLID        TO  JOB-SHELLID
               MOVE   WRK-RECEERR             TO  JOB-YOBI
               MOVE   "9999"                  TO  JOB-ERRCD
               CALL   "ORCSJOB"               USING
                                              ORCSJOBKANRIAREA
                                              JOBKANRI-REC
                                              SPA-AREA
           END-IF
      *
           MOVE   1                       TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE  MF110-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1805 IN  "  CNT-RECE01
           DISPLAY "*** SEIKYU1805 OUT "  CNT-MF110
           DISPLAY "*** SEIKYU1805 END ***"
      *
           IF  CNT-MF110  =  0
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
      *    レセプト明細書読込
      *****************************************************************
       900-MF110-READ-SEC  SECTION.
      *
           READ  MF110-FILE  NEXT
              AT  END
                 MOVE   1                        TO  FLG-END
              NOT AT  END
                 ADD    1                        TO  CNT-MF110
           END-READ
           .
       900-MF110-READ-EXT.
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
           IF      MCP-RC              =      ZERO
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
      *  収納情報セット
      *****************************************************************
       800-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  WRK-KOHNUM
             INITIALIZE  WRK-KOHFTN
             INITIALIZE  WRK-YKZFTN
             INITIALIZE  SYUNOU-REC
      *
             MOVE  SPA-HOSPNUM          TO  SYU-HOSPNUM
             MOVE   RECE01-NYUGAIKBN    TO  SYU-NYUGAIKBN
             MOVE   RECE01-PTID         TO  SYU-PTID
             MOVE   RECE01-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
             MOVE   "__"                TO  SYU-SRYYMD(7:2)
             MOVE   MF110-KOHHKNNUM     TO  SYU-KOH-HKNNUM(1)
                                            SYU-KOH-HKNNUM(2)
                                            SYU-KOH-HKNNUM(3)
                                            SYU-KOH-HKNNUM(4)
             MOVE   SYUNOU-REC          TO  MCPDATA-REC
             MOVE   "DBSELECT"          TO  MCP-FUNC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  WRK-PATH             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC              =   ZERO
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
               PERFORM  805-SYUNOU-READ-SEC
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE01-HKNCOMBI(1) OR
                                             RECE01-HKNCOMBI(2) OR
                                             RECE01-HKNCOMBI(3) OR
                                             RECE01-HKNCOMBI(4) OR
                                             RECE01-HKNCOMBI(5) OR
                                             RECE01-HKNCOMBI(6) OR
                                             RECE01-HKNCOMBI(7) OR
                                             RECE01-HKNCOMBI(8) OR
                                             RECE01-HKNCOMBI(9) OR
                                             RECE01-HKNCOMBI(10)
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF     COMB-KOHID(1)    =  WRK-KOHID   OR
                            COMB-KOHID(2)    =  WRK-KOHID   OR
                            COMB-KOHID(3)    =  WRK-KOHID   OR
                            COMB-KOHID(4)    =  WRK-KOHID
                        PERFORM  810-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
                 PERFORM  805-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR" UPON CONSOLE
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE   "DBCLOSECURSOR"           TO   MCP-FUNC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  WRK-PATH             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
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
           MOVE  "DBFETCH"             TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE  MCPDATA-REC         TO  SYUNOU-REC
             MOVE  ZERO                TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1                   TO  FLG-SYUNOU
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
           IF  SYU-KOH1HKNNUM   =   "191"  OR
               SYU-KOH2HKNNUM   =   "191"  OR
               SYU-KOH3HKNNUM   =   "191"  OR
               SYU-KOH4HKNNUM   =   "191"
              MOVE  "191"               TO  WRK-KOHNUM
           END-IF
      *
           IF  SYU-KOH1HKNNUM   =   "193"  OR
               SYU-KOH2HKNNUM   =   "193"  OR
               SYU-KOH3HKNNUM   =   "193"  OR
               SYU-KOH4HKNNUM   =   "193"
               MOVE  "193"              TO  WRK-KOHNUM
           END-IF
      *
           IF  SYU-KOH1HKNNUM   =  "194"  OR
               SYU-KOH2HKNNUM   =  "194"  OR
               SYU-KOH3HKNNUM   =  "194"  OR
               SYU-KOH4HKNNUM   =  "194"
               MOVE  "194"             TO  WRK-KOHNUM
           END-IF
      *
           IF  SYU-KOH1HKNNUM   =  "196"  OR
               SYU-KOH2HKNNUM   =  "196"  OR
               SYU-KOH3HKNNUM   =  "196"  OR
               SYU-KOH4HKNNUM   =  "196"
               MOVE  "196"             TO  WRK-KOHNUM
           END-IF
      *
           IF  SYU-KOH1HKNNUM   =  "198"  OR
               SYU-KOH2HKNNUM   =  "198"  OR
               SYU-KOH3HKNNUM   =  "198"  OR
               SYU-KOH4HKNNUM   =  "198"
               MOVE  "198"             TO  WRK-KOHNUM
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
             MOVE  SPA-HOSPNUM         TO  PTKOH-HOSPNUM
             MOVE  RECE01-PTID         TO  PTKOH-PTID
             MOVE  WRK-KOHID           TO  PTKOH-KOHID
             MOVE  WRK-KOHNUM          TO  PTKOH-KOHNUM
             MOVE  PTKOHINF-REC        TO  MCPDATA-REC
             MOVE  "DBSELECT"          TO  MCP-FUNC
             MOVE  "tbl_ptkohinf"      TO  MCP-TABLE
             MOVE  "key"               TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC   =   ZERO
               MOVE  "tbl_ptkohinf"     TO  MCP-TABLE
               MOVE  "key"              TO  MCP-PATHNAME
               PERFORM  825-PTKOHINF-READ-SEC
      *
             ELSE
               DISPLAY  "PTKOHINF SELECT ERR" UPON CONSOLE
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE  "tbl_ptkohinf"       TO  MCP-TABLE
             MOVE  "key"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
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
           MOVE  "DBFETCH"           TO  MCP-FUNC
           CALL  "ORCDBMAIN"         USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE  MCPDATA-REC       TO  PTKOHINF-REC
             MOVE  ZERO              TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1                 TO  FLG-PTKOHINF
           END-IF
      *
           .
       825-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込 
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE01-REC
           MOVE    SPA-HOSPNUM         TO  RECE01-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE01-SKYYM
           MOVE    RECE01-REC          TO  MCPDATA-REC
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
              MOVE    1                TO  FLG-END
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
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF    MCP-RC              =  ZERO
              MOVE    ZERO                TO  FLG-END
              MOVE    MCPDATA-REC         TO  RECE01-REC
      *
               INITIALIZE                          WRK-CHK-KOHNUM-TBL
               MOVE    RECE01-KOHNUM(1)        TO  WRK-CHK-KOHNUM(1)
               MOVE    RECE01-KOHNUM(2)        TO  WRK-CHK-KOHNUM(2)
               MOVE    RECE01-KOHNUM(3)        TO  WRK-CHK-KOHNUM(3)
               MOVE    RECE01-KOHNUM(4)        TO  WRK-CHK-KOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE01-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE01-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
      *            テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    RECE01-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
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
                         IF      RECE01-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO        TO  FLG-ERR
                         END-IF
                     END-PERFORM 
                   END-IF
                   DISPLAY "患者番号=" RECE01-PTNUM
                           "負担者番号=" RECE01-KOHFTNJANUM(1)
                           "保険者番号=" RECE01-HKNJANUM
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                 ADD    1                   TO  CNT-RECE01
              ELSE
                 GO  TO  900-KOHSKY-READ-SEC
              END-IF
           ELSE
              MOVE    1                   TO  FLG-END
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
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理　読み込み 
      *****************************************************************
       910-SEIKYU-START-SEC            SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE01-KEY          TO  RECE10-KEY
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE   "SEIKYU-KEY2"       TO  ORC-DBPATH
               MOVE   "tbl_seikyu"        TO  MCP-TABLE
               MOVE   "key"               TO  MCP-PATHNAME
               PERFORM 910-SEIKYU-READ-SEC
           ELSE
               MOVE   1                   TO  FLG-END
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-SEIKYU-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *     請求管理  検索
      *****************************************************************
       910-SEIKYU-READ-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE10-REC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF  
      *
           .
       910-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                        HKNCOMBI-REC
           MOVE    SPA-HOSPNUM           TO  COMB-HOSPNUM
           MOVE    SYU-PTID              TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC            =   ZERO
               MOVE    "DBFETCH"         TO  MCP-FUNC
               MOVE    "tbl_hkncombi"    TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF      MCP-RC        =   ZERO
                   MOVE    MCPDATA-REC   TO  HKNCOMBI-REC
                   MOVE    ZERO          TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                HKNCOMBI-REC
                   MOVE    1             TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                    HKNCOMBI-REC
               MOVE    1                 TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       920-AGE-CALC-SEC               SECTION.
      *
           MOVE   MF110-BIRTHDAY           TO  WRK-CALC-YMD1
           MOVE   LNK-PRTKANRI-SKYYMD      TO  WRK-CALC-YMD2
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
      *
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
      *
           .
      *
       920-AGE-CALC-EXT.
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
