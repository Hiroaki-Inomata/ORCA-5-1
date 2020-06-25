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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU0315.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 岩手・福祉医療費ＣＳＶ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/08/03    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付       内容
      *  04.08.01    門間    17/01/11   医療機関コード変更時、
      *                                 医療機関コード別の作成処理対応
      *  04.08.02    萩野    17/03/08   医療機関情報の取得の修正
      *  04.08.03    井上    18/10/10   修正履歴コメント削除
      *  05.01.01    吉川    19/04/25   元号改正対応
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
           COPY  "SEI0313.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
           COPY   "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100      PIC X(02).
           03  STS-RECE02     PIC X(02).
           03  STS-RECEERR    PIC X(02).
           03  STS-TOWN       PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-TOWN       PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
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
           03  IDX4           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
           03  IDW            PIC 9(04).
           03  IDW1           PIC 9(04).
           03  IDW2           PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
           03  WRK-PARA-STSRYYM      PIC 9(06).
           03  WRK-PARA-EDSRYYM      PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZZ.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-HENYMDG1          PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-NYMD.
               05  WRK-NYY           PIC 9(04).
               05  WRK-NMM           PIC 9(02).
               05  WRK-NDD           PIC 9(02).
           03  WRK-TYMD.
               05  WRK-TYY           PIC 9(04).
               05  WRK-TMM           PIC 9(02).
               05  WRK-TDD           PIC 9(02).
           03  WRK-HOSPNUM           PIC X(10).
           03  WRK-GENGO             PIC X(05).
           03  WRK-BIRTHDAY          PIC X(16).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-KEISAN            PIC 9(08).
           03  WRK-AMARI             PIC 9(05).
           03  WRK-KBN               PIC X(01).
           03  WRK-CITYKENSU         PIC 9(03).
           03  WRK-CITYNUM           PIC X(02).
           03  WRK-GPAGE             PIC 9(03).
           03  WRK-GKENSU            PIC 9(05).
           03  WRK-CALC              PIC 9(05).
           03  WRK-KASAN-TBL.
               05   WRK-SEIKYUGK-G   PIC 9(08)V9(01).
               05   WRK-SEIKYUGK-G2  PIC 9(08).
               05   WRK-SEIKYUGK     PIC 9(08)V9(01).
           03  WRK-GOKEI-AREA.
               05  WRK-GKENSU        PIC 9(05).
               05  WRK-GITBFTN       PIC 9(10).
               05  WRK-GKINGAKU      PIC 9(10).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH              PIC  X(50).
           03  WRK-FILENAME          PIC  X(40).
      *
      *    データ編集
           03  WRK-HENSHU.
               05  WRK-RATE          PIC 9(02).
               05  WRK-MOJIHEN       PIC 9(10).
      *
           03  WRK-REC               PIC X(500).
           03  WRK-REC-LENGTH        PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA          PIC X(100).
               05  WRK-NUM           PIC ZZZZZZZZZ9.999.
               05  WRK-NAGASA        PIC 9(04).
               05  WRK-ST            PIC 9(02).
               05  WRK-SYOSUU        PIC 9(01).
               05  WRK-END           PIC 9(01).
      *
           03  WRK-SCSVINFO-SEQ-NO   PIC 9(06).
           03  WRK-SCSVINFO-PTID     PIC 9(10).
           03  WRK-SCSVINFO-NYUGAIKBN    PIC X(01).
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
      *    パラメータファイル名
           03  FILE-NAME             PIC X(512).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
           03  WRK-CHK-KOHNUM-TBL.
               05  WRK-CHK-KOHNUM    PIC X(03)  OCCURS 4.
      *    対象公費
           03  WRK-TAISYO-KOHNUM     PIC X(03)  OCCURS 100.
      *
      *    ファイル名
       01  DATA-FILE.
           03  FILLER      PIC X(05)  VALUE  "data/".
           03  FILLER      PIC X(12)  VALUE  "TOWN0301.TXT".
      *
      *    集計領域
       01  SUM-AREA.
      *    小計
           03  SUM-SYOKEI-AREA.
               05  SUM-SKENSU        PIC 9(10).
               05  SUM-SITBFTN       PIC 9(10).
               05  SUM-SKINGAKU      PIC 9(10).
      *    総合計
           03  SUM-GOKEI-AREA.
               05  SUM-GKENSU        PIC 9(10).
               05  SUM-GITBFTN       PIC 9(10).
               05  SUM-GKINGAKU      PIC 9(10).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02            PIC 9(06).
           03  CNT-MF100             PIC 9(06).
           03  CNT-PAGE              PIC 9(03).
           03  CNT-OUT               PIC 9(06).
      *
      *    キー領域
       01  KEY-AREA  VALUE  ZERO.
           03  KEY-NEW.
               05  KEY-N-CITYNUM     PIC X(03).
           03  KEY-OLD.
               05  KEY-O-CITYNUM     PIC X(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    固定項目
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
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
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
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
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY    "MCPDATA.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
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
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU0315"    TO  JOB-PGID
           MOVE    "福祉医療費ＣＳＶ" 
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
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU0315 SYS 1001 ERR ***"
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
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU0315 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
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
           MOVE    "SEIKYU0315"              TO  WRK-PRGID
      *
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
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
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
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
      *
           IF       CNT-RECE02   >       0
                    PERFORM   2002-PRINT-SEC
           END-IF
           MOVE   1   TO   FLG-END
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
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
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
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
           INITIALIZE                    MF100-REC
      *
           MOVE    SPA-HOSPNUM                 TO  MF100-HOSPNUM
           MOVE    RECE02-PTNUM                TO  MF100-PTNUM
           MOVE    RECE02-RECEKA               TO  MF100-SRYKA
           MOVE    RECE02-TEISYUTUSAKI         TO  MF100-TEISYUTUSAKI
           MOVE    RECE02-HKNJANUM-KEY         TO  MF100-HKNJANUM-KEY
           MOVE    RECE02-TEKSTYMD             TO  MF100-TEKSTYMD
           MOVE    RECE02-NYUGAIKBN            TO  MF100-NYUGAIKBN
      *
           MOVE    RECE02-NAME                 TO  MF100-NAME
           MOVE    RECE02-SEX                  TO  MF100-SEX
           MOVE    RECE02-BIRTHDAY             TO  MF100-BIRTHDAY
           MOVE    RECE02-HKN                  TO  MF100-HKN
           MOVE    RECE02-SKYYM                TO  MF100-SKYYM
           MOVE    RECE02-SRYYM                TO  MF100-SRYYM
      *
           MOVE    RECE02-KOHINF      (IDZ)    TO  MF100-KOHINF
           MOVE    RECE02-KOHFTNJANUM (IDZ)    TO  MF100-FTNJANUM
           MOVE    RECE02-KOHJKYSNUM  (IDZ)    TO  MF100-JKYSNUM
           MOVE    RECE02-KOHJKYSNUM(IDZ)(1:2) TO  WRK-CITYNUM
           MOVE    WRK-CITYNUM                 TO  MF100-CITYNUM
           COMPUTE IDY     =       IDZ     +   1
           MOVE    RECE02-JNISSUINF   (IDY)    TO  MF100-JNISSUINF
           MOVE    RECE02-TOTALTENINF (IDY)    TO  MF100-TOTALTENINF
           MOVE    RECE02-FTNMONEYINF (IDY)    TO  MF100-FTNMONEYINF
           MOVE    RECE02-SHOKUJIINF  (IDY)    TO  MF100-SHOKUJIINF
      *
      *    明細用
           MOVE    1                           TO  MF100-SIKIBETU
      *
           MOVE    1                           TO  FLG-ERR
           MOVE    MF100-KOHNUM                TO  WRK-KOHNUM
           MOVE    MF100-KOHID                 TO  WRK-KOHID
           MOVE    ZERO                        TO  WRK-SEIKYUGK-G
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"                 TO  WRK-PATH
           ELSE
               MOVE    "key25"                 TO  WRK-PATH
           END-IF
      *    収納情報読み込み
           PERFORM 920-SYUNOU-SET-SEC
      *
      *    保険に限度額が有る場合の対応
           IF  RECE02-FTNMONEY(1)  >   ZERO
           AND RECE02-FTNMONEY(1)  <   WRK-SEIKYUGK-G
               MOVE    RECE02-FTNMONEY(1)      TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    長期
           IF  FLG-TYOKI        =  1
           AND WRK-SEIKYUGK-G  >=  10000
               MOVE    10000                   TO  WRK-SEIKYUGK-G
           END-IF
      *    長期上位
           IF  FLG-TYOKI        =  2
           AND WRK-SEIKYUGK-G  >=  20000
               MOVE    20000                   TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    一部負担金
           MOVE     WRK-SEIKYUGK-G             TO  MF100-ITBFTN
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR    =    ZERO
               WRITE    MF100-REC
               ADD      1                      TO  CNT-MF100
           ELSE
               DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                       "診療年月="            MF100-SRYYM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  RECE02-PTNUM
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
      *
                   DISPLAY  "TBL-PARA-HKNJANUM = " 
                                               TBL-PARA-HKNJANUM(IDXX)
      *
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM 
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           DISPLAY "2002-PRINT START"
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO                     TO  FLG-PRINT
           ADD      1                        TO  CNT-PAGE
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *    キーセット
               MOVE    KEY-NEW               TO  KEY-OLD
      *    ＣＳＶ<ヘッダーレコード>処理
               PERFORM  310-CSV-HEAD-SEC
      *    ＣＳＶ<データレコード>処理
               PERFORM  320-CSV-DATA-SEC
      *    ＣＳＶ<フッターレコード>処理
               PERFORM  330-CSV-FOOT-SEC
           END-PERFORM
      *
      *    ＣＳＶ<エンドコード>処理
           PERFORM 340-CSV-END-SEC
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ<ヘッダーレコード>処理
      *****************************************************************
       310-CSV-HEAD-SEC          SECTION.
      *
           INITIALIZE                            WRK-REC
                                                 WRK-REC-LENGTH
      *
      *    １．データ種別
           MOVE    "1"                       TO  WRK-DATA
           MOVE    1                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．請求年月
           MOVE    LNK-PRTKANRI-SKYYMD       TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           MOVE    LNK-DAY1-WYMD(1:5)        TO  WRK-DATA
           MOVE    5                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．保険医療機関番号
           STRING  "031"                     DELIMITED BY SIZE
                   SYS-1001-HOSPCD           DELIMITED BY SIZE
                   INTO                      WRK-DATA
           END-STRING
           MOVE    10                        TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．市町村番号
           MOVE    SPACE                     TO  WRK-DATA
           MOVE    "0"                       TO  WRK-DATA(1:1)
           MOVE    MF100-CITYNUM             TO  WRK-DATA(2:2)
           MOVE    3                         TO  WRK-NAGASA
           MOVE    1                         TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)     DELIMITED  BY  SIZE
                   WRK-KAIGYO                    DELIMITED  BY  SIZE
                   INTO                          WRK-REC
           END-STRING
      *
           MOVE    ZERO                      TO  WRK-SCSVINFO-PTID
           MOVE    SPACE                     TO  WRK-SCSVINFO-NYUGAIKBN
           PERFORM 500-CSVINFO-SEC
      *
           .
       310-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<データレコード>処理
      *****************************************************************
       320-CSV-DATA-SEC      SECTION.
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    KEY-NEW    NOT  =   KEY-OLD
                    OR       FLG-PRINT       =   1
      *
             INITIALIZE                      WRK-REC
                                             WRK-REC-LENGTH
      *
      *    １．データ種別
             MOVE    "2"                     TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．保険医療機関番号
             STRING  "031"                   DELIMITED BY SIZE
                    SYS-1001-HOSPCD          DELIMITED BY SIZE
                                             INTO  WRK-DATA
             END-STRING
             MOVE    10                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．受給者番号
             MOVE    MF100-JKYSNUM           TO  WRK-DATA
             MOVE    11                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．保険者番号
             IF  MF100-TEISYUTUSAKI  =  2
                 IF  MF100-HKNJANUM(1:2)  =  "67"
                     MOVE    MF100-HKNJANUM  TO  WRK-DATA
                 ELSE
      *              国保は頭ゼロをつけて右詰め
                     MOVE    "00"            TO  WRK-DATA(1:2)
                     MOVE    MF100-HKNJANUM  TO  WRK-DATA(3:)
                 END-IF
             ELSE
                 MOVE    MF100-HKNJANUM      TO  WRK-DATA
             END-IF
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．記号
             MOVE    MF100-KIGO              TO  WRK-MAE-INPUT
             PERFORM 31013-ZEN2HAN-HEN-SEC
             MOVE    WRK-OUT-INPUT           TO  WRK-DATA
             MOVE    20                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ６．番号
             MOVE    MF100-NUM               TO  WRK-MAE-INPUT
             PERFORM 31013-ZEN2HAN-HEN-SEC
             MOVE    WRK-OUT-INPUT           TO  WRK-DATA
             MOVE    20                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ７．氏名
             MOVE    MF100-NAME              TO  WRK-DATA
             MOVE    40                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ８．性別
             MOVE    MF100-SEX               TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ９．生年月日
             MOVE    MF100-BIRTHDAY          TO  WRK-SYMD
             PERFORM 31011-SEIWA-HEN-SEC
             MOVE    LNK-DAY1-WYMD           TO  WRK-DATA
             MOVE    7                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １０．診療年月
             MOVE    MF100-SRYYM             TO  WRK-SYMD(1:6)
             MOVE    "01"                    TO  WRK-SYMD(7:2)
             PERFORM 31011-SEIWA-HEN-SEC
             MOVE    LNK-DAY1-WYMD(1:5)      TO  WRK-DATA
             MOVE    5                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １１．入院区分
             MOVE    MF100-NYUGAIKBN         TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    １２．処方箋発行医療機関番号
             MOVE    "0000000000"            TO  WRK-DATA
             MOVE    10                      TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *    
      *    １３．一部負担金
             MOVE    MF100-ITBFTN            TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      * 
      *    １４．窓口負担額
             MOVE    MF100-FTNMONEY          TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             MOVE    1                       TO  WRK-END
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *    改行コード編集
             STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                     WRK-KAIGYO                 DELIMITED  BY  SIZE
                     INTO                       WRK-REC
             END-STRING
      *
             PERFORM 500-CSVINFO-SEC
      *
      *      合計計算
             ADD     1                       TO  SUM-SKENSU
                                                 SUM-GKENSU
             ADD     MF100-ITBFTN            TO  SUM-SITBFTN
                                                 SUM-GITBFTN
             ADD     MF100-FTNMONEY          TO  SUM-SKINGAKU
                                                 SUM-GKINGAKU
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
      *
           END-PERFORM
      *
           .
       320-CSV-DATA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<フッターレコード>処理
      *****************************************************************
       330-CSV-FOOT-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *    １．データ種別
           MOVE    "3"                       TO  WRK-DATA
           MOVE    1                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．データ件数
           MOVE    SUM-SKENSU                TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３．一部負担金合計
           MOVE    SUM-SITBFTN               TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ４．窓口負担額合計
           MOVE    SUM-SKINGAKU              TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           MOVE    1                         TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      * 
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           INITIALIZE                         SUM-SYOKEI-AREA
      *
           .
       330-CSV-FOOT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<エンドレコード>処理
      *****************************************************************
       340-CSV-END-SEC      SECTION.
      *
           INITIALIZE                      WRK-REC
                                           WRK-REC-LENGTH
      *
      *    １．データ種別
           MOVE    "9"                       TO  WRK-DATA
           MOVE    1                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．データ件数
           MOVE    SUM-GKENSU                TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ３．一部負担金合計
           MOVE    SUM-GITBFTN               TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    ４．窓口負担額合計
           MOVE    SUM-GKINGAKU              TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           MOVE    1                         TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
      * 
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)  DELIMITED  BY  SIZE
                   WRK-KAIGYO                 DELIMITED  BY  SIZE
                   INTO                       WRK-REC
           END-STRING
      *
           PERFORM 500-CSVINFO-SEC
      *
           .
       340-CSV-END-EXT.
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
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
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
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角→全角変換処理
      *****************************************************************
       31013-ZEN2HAN-HEN-SEC        SECTION.
      *
           INITIALIZE             WRK-OUT-INPUT
      *
           PERFORM  VARYING  IDW1  FROM  1  BY  2
                    UNTIL    IDW1  >  20
               COMPUTE  IDW2  =  (IDW1  +  1)  /  2
               EVALUATE  WRK-MAE-INPUT(IDW1:2)
                 WHEN  "０"   MOVE  "0"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "１"   MOVE  "1"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "２"   MOVE  "2"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "３"   MOVE  "3"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "４"   MOVE  "4"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "５"   MOVE  "5"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "６"   MOVE  "6"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "７"   MOVE  "7"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "８"   MOVE  "8"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "９"   MOVE  "9"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "ー"   MOVE  "-"  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  "　"   MOVE  " "  TO  WRK-OUT-INPUT(IDW2:1)
                 WHEN  SPACE  MOVE  20   TO  IDW1
                 WHEN  OTHER  MOVE  " "  TO  WRK-OUT-INPUT(IDW2:1)
               END-EVALUATE
           END-PERFORM
      *
           .
       31013-ZEN2HAN-HEN-EXT.
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
           DISPLAY "*** SEIKYU0315 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU0315 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU0315 END ***"
      *
           IF  CNT-MF100  =  0
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
      *    ファイル名は「医療機関番号１０桁.CSV」
           STRING  "031"                   DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD         DELIMITED  BY  SIZE
                   ".CSV"                  DELIMITED  BY  SIZE
                   INTO                    WRK-FILENAME
           END-STRING
           MOVE    WRK-FILENAME    TO  SFILESV-TO-DATA  (1)
           MOVE    SUM-GKENSU      TO  SFILESV-CNT-ALL  (1)
           MOVE    "福祉医療費ＣＳＶ"    TO  SFILESV-TITLE    (1)
           IF      SUM-GKENSU   >   ZERO
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
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW    <       1
                   IF      WRK-DATA (IDW:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW)           DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW    FROM    WRK-ST    BY  1
                       UNTIL   IDW    >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                      IDZ
                        MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
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
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID     KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ    MF100-FILE  NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
                   MOVE  HIGH-VALUE        TO  KEY-N-CITYNUM
               NOT AT END
                   MOVE  MF100-CITYNUM     TO  KEY-N-CITYNUM
           END-READ
           .
       900-TMP-READ-EXT.
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
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
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
           MOVE    "DBFETCH"                TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
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
      *        テスト患者のときは読み飛ばす
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
      *        個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO                TO  FLG-ERR
                           MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                         END-IF
                     END-PERFORM 
                   END-IF
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *
                   ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
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
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-KEY              TO  RECE10-KEY
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE10
                   INITIALIZE                  RECE10-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
               INITIALIZE                  RECE10-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
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
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM              TO  COMB-HOSPNUM
           MOVE    SYU-PTID                 TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_hkncombi"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
      *
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF  SYU-KOH-HKNNUM(IDX)  =  WRK-KOHNUM
                 IF  IDX  =  1
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
      *    一部負担金欄は保険にかかる患者負担を集計（１０円単位）
           ADD    SYU-SYUCOMPFTN             TO  WRK-SEIKYUGK-G
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
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"     TO  MCP-TABLE
               MOVE  WRK-PATH         TO  MCP-PATHNAME
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
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"   TO  MCP-FUNC
             MOVE  "tbl_syunou"      TO  MCP-TABLE
             MOVE  WRK-PATH          TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"       USING
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"       USING
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
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           PERFORM 800-SYSKANRI-READ-SEC
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
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
       900-DBDISCONNECT-SEC  SECTION.
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
      *
