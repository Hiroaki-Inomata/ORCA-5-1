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
       PROGRAM-ID.     SEIKYU1807.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 福井・医療費助成事業対象者明細書ファイル
      *                      (191,193,194,196,198)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/10/25     萩野         新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者  日付         内容
      *  01.00.01       萩野    2013/11/12   保険者証番号を半角に変換する
      *  01.00.02       萩野    2013/12/06   点数欄は小数点以下１桁までを記録し、
      *                                      小数点そのものは削除する
      *  01.00.03       萩野    2014/02/12   保険医療機関コード欄を[18＋点数表＋医療機関番号]
      *                                      と記録するように修正
      *                                      患者カナ氏名欄は半角で記録するように修正
      *  01.00.04       萩野    2014/04/09   数字項目について、桁数に達しない場合は
      *                                      先頭に０を付加するよう修正
      *  01.00.05       萩野    2014/07/09   受給資格者番号の先頭の値が
      *                                      「０」になる不具合を修正
      *  01.00.06       吉川    2014/10/09   ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.07       吉川    2015/11/26   患者負担が食事のみのデータは
      *                                      印字対象とするように修正
      *                                      難病の経過措置の場合、食事負担は
      *                                      １／２とするように修正
      *  01.00.08       門間    2016/11/15   対象公費はPGオプションから取得する
      *  01.00.09       門間    2017/03/08   公費にかかる分の点数日数を印字するように修正
      *                                      医療機関コード変更時、
      *                                      医療機関コード別の作成処理対応
      *  01.00.10       萩野    2018/11/07   修正履歴コメント削除
      *  01.00.11       門間    2019/05/09   改元対応
      *  01.00.12       萩野    2019/06/12   結核を併用している場合は
      *                                      公費対象患者負担額に印字されるように修正
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF110-FILE  ASSIGN    MF110PARA
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
      *    スパ領域
       01  STS-AREA.
           03  STS-MF110   PIC X(02).
           03  STS-RECE02  PIC X(02).
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
           03  IDX3           PIC 9(04).
           03  IDX4           PIC 9(04).
           03  IDW            PIC 9(04).
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
           03  WRK-PATH            PIC X(50).
           03  WRK-FILENAME        PIC X(40).
           03  WRK-SOUGOKEIKENSU   PIC 9(07).
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
           03  WRK-REC                 PIC X(500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-MOJIHEN             PIC 9(10).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(100).
               05  WRK-NUM             PIC ZZZZZZZZZZZ.
               05  WRK-NUM1            PIC ZZZZZZZZ.Z.
               05  WRK-NUM9            PIC 99999999999.
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-NAGASA-SPACE    PIC 9(04).
               05  WRK-ST              PIC 9(02).
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *
           03  WRK-GYYMM               PIC X(07).
           03  WRK-SCSVINFO-SEQ-NO     PIC 9(06).
           03  WRK-SCSVINFO-PTID       PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN  PIC X(01).
      *
           03  WRK-KANA-G              PIC X(128).
      *
      *市町村コード確認用
       01  WRK-CITY-DATA.
           03  WRK-CITYCD      PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-RECE10  PIC 9(06).
           03  CNT-MF110   PIC 9(06).
           03  CNT-LINE    PIC 9(02).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-PRINT   PIC 9(06).
           03  CNT-OUT     PIC 9(06).
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
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
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
                 BY         //RECE02//.
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
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
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
           MOVE    "SEIKYU1807"    TO  JOB-PGID
           MOVE    "医療費助成事業対象者明細書ファイル"
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
             DISPLAY "*** SEIKYU1807 SYS 1001 ERR ***"
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
             DISPLAY "*** SEIKYU1807 SYS 1002 ERR ***"
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
           MOVE    "SEIKYU1807"              TO  WRK-PRGID
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
           IF  CNT-RECE02  >       0
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
               MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
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
           MOVE   RECE02-PTNUM        TO  MF110-PTNUM
           MOVE   RECE02-RECEKA       TO  MF110-SRYKA
           MOVE   RECE02-HKNJANUM-KEY TO  MF110-HKNJANUM-KEY
           MOVE   RECE02-TEKSTYMD     TO  MF110-TEKSTYMD
           MOVE   RECE02-NYUGAIKBN    TO  MF110-NYUGAIKBN
      *
           MOVE   RECE02-PTID         TO  MF110-PTID
           MOVE   RECE02-KANANAME     TO  MF110-KANANAME
           MOVE   RECE02-NAME         TO  MF110-NAME
           MOVE   RECE02-BIRTHDAY     TO  MF110-BIRTHDAY
           MOVE   RECE02-HKN          TO  MF110-HKN
           MOVE   RECE02-SEX          TO  MF110-SEX
           MOVE   RECE02-RJNHKN       TO  MF110-RJNHKN
           MOVE   RECE02-SKYYM        TO  MF110-SKYYM
           MOVE   RECE02-SRYYM        TO  MF110-SRYYM
      *
           MOVE   RECE02-KOHNUM      (IDZ)   TO  MF110-KOHHKNNUM
           MOVE   RECE02-KOHFTNJANUM (IDZ)   TO  MF110-FTNJANUM
           MOVE   RECE02-KOHJKYSNUM  (IDZ)   TO  MF110-KOHJKYSNUM
           MOVE   RECE02-KOHJKYSNUM(IDZ)(3:) TO  MF110-JKYSNUM
           MOVE   RECE02-KOHINF      (IDZ)   TO  MF110-KOHINF
           COMPUTE  IDY    =    IDZ    +   1
           MOVE   RECE02-JNISSUINF   (IDY)   TO  MF110-JNISSUINF
           MOVE   RECE02-TOTALTENINF (IDY)   TO  MF110-TOTALTENINF
           MOVE   RECE02-FTNMONEYINF (IDY)   TO  MF110-FTNMONEYINF
           MOVE   RECE02-SHOKUJINISSU(1)     TO  MF110-SHOKUJIKAISU
           MOVE   RECE02-SHOKUJIRYOYOHI(1)   TO  MF110-SHOKUJIRYOYOHI
           MOVE   RECE02-SHOKUJIFTN(1)       TO  MF110-SHOKUJIFTN
           PERFORM  VARYING  IDX  FROM  1  BY  1  UNTIL  IDX  >  5
             MOVE   RECE02-FLG(IDX)          TO  MF110-FLG(IDX)
           END-PERFORM
           PERFORM  830-CITY-SET-SEC
      *
      *    被保区分
           MOVE  RECE02-HONKZKKBN            TO  MF110-HONHUYOKBN
      
      *    本人家族区分
           IF   RECE02-HKNNUM     =   "067"
      *      退国保
             MOVE  RECE02-HONKZKKBN          TO  MF110-RECEHONKZKKBN
           ELSE
             MOVE  SPACE                     TO  MF110-RECEHONKZKKBN
           END-IF
      *    
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
              ELSE
                MOVE   ZERO                TO    MF110-CHOKIKBN
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
                     MOVE   RECE10-FTNMONEY-KOJ (IDY)  TO
                                         MF110-RECEFTNMONEY(IDY - 1)
                 ELSE
                     MOVE   RECE10-FTNMONEY (IDY)      TO
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
           IF      RECE02-NYUGAIKBN   =    "1"
               MOVE    "key33"        TO   WRK-PATH
           ELSE
               MOVE    "key25"        TO   WRK-PATH
           END-IF
           MOVE   MF110-KOHID         TO   WRK-KOHID
      *
      *    主保険の点数を取得
      *    公費２以降に値がある、及びIDZが２以降の場合、
      *    月途中に公費の変更があったとみなす
           MOVE   RECE02-TOTALTENINF(IDZ + 1)
                                      TO  MF110-TOTALTENINF
           MOVE   RECE02-JNISSUINF(IDZ + 1)
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
           ADD   1   TO   WRK-SOUGOKEIKENSU
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
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    レセプト明細読込
           PERFORM 900-MF110-READ-SEC
      *
           PERFORM  UNTIL       FLG-END  =  1
      *
      *        ＣＳＶ<データレコード>処理
               PERFORM 310-CSV-HEN-SEC
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
      *    ＣＳＶ編集<内容>処理
      *****************************************************************
       310-CSV-HEN-SEC      SECTION.
      *
           INITIALIZE                     WRK-REC
                                          WRK-REC-LENGTH
                                          WRK-MOJIHEN
      *
      *    請求年月(和暦)
           MOVE    LNK-PRTKANRI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"                    TO  WRK-SYMD(7:2)
           PERFORM   31011-SEIWA-HEN-SEC
           MOVE    SPACE                   TO  WRK-GYYMM
           MOVE    LNK-DAY1-WYMD(1:5)      TO  WRK-GYYMM
           MOVE    WRK-GYYMM               TO  WRK-NUM
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    診療年月(和暦)
           MOVE    MF110-SRYYM             TO  WRK-SYMD(1:6)
           MOVE    "01"                    TO  WRK-SYMD(7:2)
           PERFORM   31011-SEIWA-HEN-SEC
           MOVE    SPACE                   TO  WRK-GYYMM
           MOVE    LNK-DAY1-WYMD(1:5)      TO  WRK-GYYMM
           MOVE    WRK-GYYMM               TO  WRK-NUM
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    保険医療機関コード
      *    保険医療機関コード（１８＋点数表＋医療機関番号）
           STRING  "18"                    DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN      DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   INTO                    WRK-DATA
           END-STRING
           MOVE    10                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    受給資格者番号
           MOVE    MF110-KOHJKYSNUM        TO  WRK-NUM
           MOVE    11                      TO  WRK-NAGASA
           
           DISPLAY "***  KOHJKYSNUM 1="MF110-KOHJKYSNUM
           DISPLAY "***  KOHJKYSNUM 2="WRK-NUM
           
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    患者カナ氏名
           INITIALIZE                          WRK-MAE-INPUT
                                               WRK-OUT-INPUT
           MOVE    MF110-KANANAME(1:40)    TO  WRK-MAE-INPUT
           PERFORM 31013-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-DATA
           COMPUTE WRK-NAGASA-SPACE = ( ( IDX3 - IDX4 ) - 1 ) / 2 
           IF  WRK-NAGASA-SPACE  >   20
               MOVE    20                      TO  WRK-NAGASA-SPACE
           END-IF
           COMPUTE WRK-NAGASA = 20 + WRK-NAGASA-SPACE
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    患者氏名
           MOVE    MF110-NAME              TO  WRK-DATA
           MOVE    24                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療科コード
           MOVE    MF110-SRYKA             TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    保険者番号
           MOVE    MF110-HKNJANUM          TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    記号
           MOVE    MF110-KIGO              TO  WRK-DATA
           MOVE    20                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    番号
           MOVE    MF110-NUM               TO  WRK-DATA
           MOVE    WRK-DATA                TO  WRK-MAE-INPUT
           MOVE    SPACE                   TO  WRK-OUT-INPUT
           PERFORM 31014-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  WRK-DATA
           MOVE    15                      TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    性別
           MOVE    MF110-SEX               TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    生年月日(和暦)
           MOVE    MF110-BIRTHDAY          TO  WRK-SYMD
           PERFORM   31011-SEIWA-HEN-SEC
           MOVE    SPACE                   TO  WRK-GYYMM
           MOVE    LNK-DAY1-WYMD           TO  WRK-GYYMM
           MOVE    WRK-GYYMM               TO  WRK-NUM
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    入外区分
           MOVE    MF110-NYUGAIKBN         TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    被保険区分
           MOVE    MF110-HONHUYOKBN        TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    本人・家族区分(退職医療該当者)
           MOVE    MF110-RECEHONKZKKBN     TO  WRK-NUM
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    長期区分
           MOVE    MF110-CHOKIKBN          TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    給付割合
      *      前期高齢特例措置
           IF   MF110-SRYYM  >=  WRK-CONS-SRYYM-200804
           AND  MF110-FLG (2)  =  1
               MOVE    8                    TO  WRK-Z2
               MOVE    WRK-Z2               TO  WRK-NUM
               MOVE    2                    TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
           ELSE
               COMPUTE WRK-Z2  =  ( 100   -   MF110-KYURATE )  /  10
               MOVE    WRK-Z2               TO  WRK-NUM
               MOVE    2                    TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *
      *    日数
           MOVE    MF110-JNISSU            TO  WRK-NUM
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    点数
           COMPUTE WRK-NUM  =  ( MF110-TOTALTEN  *  10 )
           MOVE    9                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      * 
      *    一部負担金 
      *    食事医療費負担額を加算
           ADD     MF110-SKY-SHOKUJIFTN    TO  MF110-ITBFTN
           MOVE    MF110-ITBFTN            TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    余白
           MOVE    "000000"                TO  WRK-DATA
           MOVE    6                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *
      *    公費負担医療
           IF    MF110-RJNHBTNUM     =    "27"
      *        負担医療　第１公費
      *        法別番号（老人）
               MOVE    MF110-RJNHBTNUM        TO  WRK-NUM
               MOVE    2                      TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        日数
               MOVE  ZERO                     TO  WRK-NUM
               MOVE    2                      TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        公費対象点数
               MOVE  ZERO                     TO  WRK-NUM
               MOVE    9                      TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        余白
               MOVE    "000000"                  TO  WRK-DATA
               MOVE    6                         TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *
      *        公費対象患者負担額
               MOVE  ZERO                        TO  WRK-NUM
               MOVE    6                         TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        負担医療　第２公費
      *        法別番号
               MOVE    MF110-RECEKOHHBTNUM (2)   TO  WRK-NUM
               MOVE    2                         TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        日数
               MOVE    MF110-RECEJNISSU (2)      TO  WRK-Z2
               MOVE    WRK-Z2                    TO  WRK-NUM
               MOVE    2                         TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        公費対象点数
               COMPUTE WRK-NUM  =  ( MF110-RECETOTALTEN (2)  *  10 )
               MOVE    9                         TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
      *        余白
               MOVE    "000000"                  TO  WRK-DATA
               MOVE    6                         TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *
      *        公費対象患者負担額
               MOVE    MF110-RECEFTNMONEY (2)    TO  WRK-NUM
               MOVE    6                         TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *      
           ELSE
      *      
      *      公費負担医療　第１、２公費
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       2
      *          法別番号
                 MOVE   MF110-RECEKOHHBTNUM (IDX2) TO  WRK-NUM
                 MOVE    2                         TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
      *      
      *          日数
                 MOVE  MF110-RECEJNISSU (IDX2)     TO  WRK-Z2
                 MOVE  WRK-Z2                      TO  WRK-NUM
                 MOVE    2                         TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
      *      
      *          公費対象点数
                 COMPUTE WRK-NUM  =  ( MF110-RECETOTALTEN (IDX2) * 10 )
                 MOVE    9                         TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
      *      
      *          余白
                 MOVE    "000000"                  TO  WRK-DATA
                 MOVE    6                         TO  WRK-NAGASA
                 PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *          公費対象患者負担額
                 MOVE  MF110-RECEFTNMONEY (IDX2)   TO  WRK-Z7
                 MOVE  WRK-Z7                      TO  WRK-NUM
                 MOVE    6                         TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
               END-PERFORM
           END-IF
      *    
      *    食事療養費　医療保険
      *    食事回数
           MOVE  MF110-SHOKUJIKAISU      TO  WRK-NUM
           MOVE  2                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *      
      *    食事療養費
           MOVE  MF110-SHOKUJIRYOYOHI    TO  WRK-NUM
           MOVE  6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *      
      *    標準負担額
           MOVE  MF110-SHOKUJIFTN        TO  WRK-NUM
           MOVE  6                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    
      *    食事療養費
           IF    MF110-RJNHBTNUM     =    "27"
      *        老人
      *        第１公費分
      *        食事回数
               MOVE  ZERO                          TO  WRK-NUM
               MOVE  2                             TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *    
      *        食事療養費金額
               MOVE  ZERO                          TO  WRK-NUM
               MOVE  6                             TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *    
      *        標準負担額
               MOVE  ZERO                          TO  WRK-NUM
               MOVE  6                             TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *      
      *        第２公費分
      *        食事回数
               MOVE  MF110-RECESHOKUJIKAISU (2)    TO  WRK-NUM
               MOVE  2                             TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *    
      *        食事療養費金額
               MOVE  MF110-RECESHOKUJIRYOYOHI (2)  TO  WRK-NUM
               MOVE  6                             TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *    
      *        標準負担額
               MOVE  MF110-RECEFTNMONEY (2)        TO  WRK-NUM
               MOVE  6                             TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
           ELSE
      *        食事療養費　第１、２公費分
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       2
      *          食事回数
                 MOVE  MF110-RECESHOKUJIKAISU (IDX2)    TO  WRK-NUM
                 MOVE  2                                TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
      *          食事療養費金額
                 MOVE  MF110-RECESHOKUJIRYOYOHI (IDX2)  TO  WRK-NUM
                 MOVE  6                                TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
      *          標準負担額
                 MOVE  MF110-RECESHOKUJIFTN (IDX2)      TO  WRK-NUM
                 MOVE  6                                TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
               END-PERFORM
           END-IF
          
      *    漢字区分
           MOVE  2                         TO  WRK-NUM
           MOVE  1                         TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    
      *    患者コード
           MOVE  MF110-PTNUM               TO  WRK-NUM
           MOVE  10                        TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    
      *    余白
           INITIALIZE  WRK-DATA
           MOVE    38                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    
      *    
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
      *    ＣＳＶ＿ＩＮＦＯ出力処理
           PERFORM 500-CSVINFO-SEC
      *
      *    一時ファイル読込（レセプト明細）
           PERFORM 900-MF110-READ-SEC
      *
           .
       310-CSV-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦・和暦変換処理
      *****************************************************************
       31011-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           .
       31011-SEIWA-HEN-EXT.
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
      *    半角変換処理
      *****************************************************************
       31014-HENKAN-SEC                 SECTION.
      *
           MOVE  1  TO  IDX3
           PERFORM VARYING  IDX2  FROM  1  BY  2
                   UNTIL    IDX2  >=  81
             EVALUATE   WRK-MAE-INPUT(IDX2:2)
               WHEN  "１"
                 MOVE  "1"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "２"
                 MOVE  "2"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "３"
                 MOVE  "3"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "４"
                 MOVE  "4"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "５"
                 MOVE  "5"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "６"
                 MOVE  "6"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "７"
                 MOVE  "7"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "８"
                 MOVE  "8"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "９"
                 MOVE  "9"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "０"
                 MOVE  "0"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "―"
                 MOVE  "-"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "ー"
                 MOVE  "-"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "−"
                 MOVE  "-"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "‐"
                 MOVE  "-"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
             END-EVALUATE
           END-PERFORM
      *
           .
       31014-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角変換処理
      *****************************************************************
       31013-HENKAN-SEC                 SECTION.
      *
           MOVE  1     TO  IDX3
           MOVE  ZERO  TO  IDX4
           PERFORM VARYING  IDX2  FROM  1  BY  2
                   UNTIL    IDX2  >  40
             EVALUATE   WRK-MAE-INPUT(IDX2:2)
               WHEN  "ア"
                 MOVE  "ｱ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "イ"
                 MOVE  "ｲ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ウ"
                 MOVE  "ｳ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "エ"
                 MOVE  "ｴ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "オ"
                 MOVE  "ｵ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "カ"
                 MOVE  "ｶ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "キ"
                 MOVE  "ｷ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ク"
                 MOVE  "ｸ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ケ"
                 MOVE  "ｹ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "コ"
                 MOVE  "ｺ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "サ"
                 MOVE  "ｻ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "シ"
                 MOVE  "ｼ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ス"
                 MOVE  "ｽ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "セ"
                 MOVE  "ｾ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ソ"
                 MOVE  "ｿ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "タ"
                 MOVE  "ﾀ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "チ"
                 MOVE  "ﾁ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ツ"
                 MOVE  "ﾂ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "テ"
                 MOVE  "ﾃ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ト"
                 MOVE  "ﾄ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ナ"
                 MOVE  "ﾅ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ニ"
                 MOVE  "ﾆ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヌ"
                 MOVE  "ﾇ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ネ"
                 MOVE  "ﾈ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ノ"
                 MOVE  "ﾉ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ハ"
                 MOVE  "ﾊ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヒ"
                 MOVE  "ﾋ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "フ"
                 MOVE  "ﾌ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヘ"
                 MOVE  "ﾍ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ホ"
                 MOVE  "ﾎ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "マ"
                 MOVE  "ﾏ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ミ"
                 MOVE  "ﾐ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ム"
                 MOVE  "ﾑ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "メ"
                 MOVE  "ﾒ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "モ"
                 MOVE  "ﾓ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヤ"
                 MOVE  "ﾔ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ユ"
                 MOVE  "ﾕ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヨ"
                 MOVE  "ﾖ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ラ"
                 MOVE  "ﾗ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "リ"
                 MOVE  "ﾘ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ル"
                 MOVE  "ﾙ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "レ"
                 MOVE  "ﾚ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ロ"
                 MOVE  "ﾛ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ワ"
                 MOVE  "ﾜ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ヲ"
                 MOVE  "ｦ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ン"
                 MOVE  "ﾝ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ァ"
                 MOVE  "ｧ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ィ"
                 MOVE  "ｨ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ゥ"
                 MOVE  "ｩ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ェ"
                 MOVE  "ｪ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ォ"
                 MOVE  "ｫ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ャ"
                 MOVE  "ｬ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ュ"
                 MOVE  "ｭ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ョ"
                 MOVE  "ｮ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ッ"
                 MOVE  "ｯ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "１"
                 MOVE  "1"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "２"
                 MOVE  "2"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "３"
                 MOVE  "3"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "４"
                 MOVE  "4"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "５"
                 MOVE  "5"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "６"
                 MOVE  "6"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "７"
                 MOVE  "7"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "８"
                 MOVE  "8"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "９"
                 MOVE  "9"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "０"
                 MOVE  "0"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   1    TO  IDX3
               WHEN  "　"
                 MOVE  " "  TO  WRK-OUT-INPUT(IDX3:1)
                 ADD   1    TO  IDX3
                 IF  WRK-MAE-INPUT(IDX2 - 2:2)   NOT  =   SPACE
                   IF  WRK-MAE-INPUT(IDX2 + 2:2)   NOT  =   SPACE
                       ADD   1    TO  IDX4
                   END-IF
                 END-IF
               WHEN  "ー"
                 MOVE  "ｰ"  TO  WRK-OUT-INPUT(IDX3:2)
                 ADD   2    TO  IDX3
               WHEN  "ガ"
                 MOVE  "ｶﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ギ"
                 MOVE  "ｷﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "グ"
                 MOVE  "ｸﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゲ"
                 MOVE  "ｹﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゴ"
                 MOVE  "ｺﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ザ"
                 MOVE  "ｻﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ジ"
                 MOVE  "ｼﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ズ"
                 MOVE  "ｽﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゼ"
                 MOVE  "ｾﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ゾ"
                 MOVE  "ｿﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ダ"
                 MOVE  "ﾀﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ヂ"
                 MOVE  "ﾁﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ヅ"
                 MOVE  "ﾂﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "デ"
                 MOVE  "ﾃﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ド"
                 MOVE  "ﾄﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "バ"
                 MOVE  "ﾊﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ビ"
                 MOVE  "ﾋﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ブ"
                 MOVE  "ﾌﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ベ"
                 MOVE  "ﾍﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ボ"
                 MOVE  "ﾎﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "パ"
                 MOVE  "ﾊﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ピ"
                 MOVE  "ﾋﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "プ"
                 MOVE  "ﾌﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ペ"
                 MOVE  "ﾍﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ポ"
                 MOVE  "ﾎﾟ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
               WHEN  "ヴ"
                 MOVE  "ｳﾞ" TO  WRK-OUT-INPUT(IDX3:4)
                 ADD   4    TO  IDX3
             END-EVALUATE
           END-PERFORM
      *
           .
       31013-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字・固定長）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-DATA(1:WRK-NAGASA)    DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
           ADD     WRK-NAGASA               TO  WRK-REC-LENGTH
      *
           INITIALIZE                      WRK-HENSYU-AREA
      *
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字・固定長）
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           MOVE    WRK-NUM                 TO  WRK-NUM9
           DISPLAY "***  NUM-HENSYU 3="WRK-NUM9
      *      開始位置
             COMPUTE WRK-ST  =   12      -   WRK-NAGASA
             STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                     WRK-NUM9(WRK-ST:WRK-NAGASA) DELIMITED  BY SIZE
                     INTO        WRK-REC
             END-STRING
             ADD     WRK-NAGASA             TO  WRK-REC-LENGTH
      *       
             INITIALIZE                      WRK-HENSYU-AREA
      *
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理2（数字・固定長）
      *****************************************************************
       5006-NUM-HENSYU-SEC          SECTION.
      *
           IF  WRK-NUM  =  ZERO
             MOVE    SPACE                  TO  WRK-DATA
             PERFORM  5004-MOJI-HENSYU-SEC
           ELSE
      *      開始位置
             COMPUTE WRK-ST  =   11      -   WRK-NAGASA
             STRING  WRK-REC(1:WRK-REC-LENGTH)   DELIMITED  BY SIZE
                     WRK-NUM1(WRK-ST:WRK-NAGASA) DELIMITED  BY SIZE
                     INTO        WRK-REC
             END-STRING
             ADD     WRK-NAGASA             TO  WRK-REC-LENGTH
      *       
             INITIALIZE                      WRK-HENSYU-AREA
           END-IF
      *
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ＿ＩＮＦＯ出力処理
      *****************************************************************
       500-CSVINFO-SEC                SECTION.
      *
           ADD     1                   TO  WRK-SCSVINFO-SEQ-NO
      *
           INITIALIZE                  ORCSCSVINFOAREA
           MOVE    "INS"               TO  SCSVINFO-MODE
           MOVE    WRK-PARA-SHELLID    TO  SCSVINFO-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SCSVINFO-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SCSVINFO-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-RENNUM TO  SCSVINFO-RENNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SCSVINFO-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SCSVINFO-SKYYMD
           MOVE    WRK-SCSVINFO-SEQ-NO TO  SCSVINFO-SEQ-NO
           MOVE    WRK-SCSVINFO-NYUGAIKBN
                                       TO  SCSVINFO-NYUGAIKBN
           MOVE    WRK-SCSVINFO-PTID   TO  SCSVINFO-PTID
           MOVE    WRK-REC             TO  SCSVINFO-CSVDATA
           CALL    "ORCSCSVINFO"       USING
                                       ORCSCSVINFOAREA
                                       SPA-AREA
           IF      SCSVINFO-RETURN     =   ZERO
               ADD     1                   TO  CNT-OUT
           ELSE
               MOVE    "ＣＳＶ用ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           .
       500-CSVINFO-EXT.
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
           MOVE   WRK-SOUGOKEIKENSU  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1807 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1807 OUT "  CNT-MF110
           DISPLAY "*** SEIKYU1807 END ***"
      *
           IF  CNT-MF110  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ファイル保存情報更新
           PERFORM 3001-FILE-INFO-INSERT-SEC
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
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
      *    ファイル名
           STRING  "J"                     DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   ".TXT"                  DELIMITED  BY  SIZE
                   INTO                    WRK-FILENAME
           END-STRING
           MOVE    WRK-FILENAME        TO  SFILESV-TO-DATA  (1)
           MOVE    WRK-SOUGOKEIKENSU   TO  SFILESV-CNT-ALL  (1)
           MOVE    "医療費助成事業対象者明細書ファイル"
                                       TO  SFILESV-TITLE    (1)
           IF      WRK-SOUGOKEIKENSU   >   ZERO
               MOVE    "1"         TO  SFILESV-DATA-TYPE(1)
           END-IF 
           MOVE    WRK-RECEERR     TO  SFILESV-ERR-MSG  (1)
           IF      WRK-RECEERR NOT =   SPACE
               MOVE    "1"         TO  SFILESV-ERR-CODE (1)
           END-IF
           MOVE    "1"             TO  SFILESV-CODE-TYPE (1)
      *
           CALL    "ORCSFILESV"        USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
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
             MOVE   RECE02-NYUGAIKBN    TO  SYU-NYUGAIKBN
             MOVE   RECE02-PTID         TO  SYU-PTID
             MOVE   RECE02-SRYYM(1:6)   TO  SYU-SRYYMD(1:6)
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
             MOVE  RECE02-PTID         TO  PTKOH-PTID
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
              MOVE    MCPDATA-REC         TO  RECE02-REC
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
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                 ADD    1                   TO  CNT-RECE02
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
           MOVE    RECE02-KEY          TO  RECE10-KEY
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
