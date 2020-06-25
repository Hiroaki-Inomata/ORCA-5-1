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
       PROGRAM-ID.     SEIKYU1521.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 新潟・高額受領委任払承認兼支給申請書
      *                      (180,280,182,282,184,284,186,286,100)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/13    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    07/09/21    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.02    内田    09/01/28    SPA-AREA初期化
      *  01.00.03    吉川    10/05/18    市町村名の取得方法の変更
      *  01.00.04    吉川    10/06/08    帳票様式の変更に伴う修正
      *  01.00.05    吉川    10/06/22    印字対象番号の追加(280,282,284,286)
      *  01.00.06    吉川    12/12/25    医療機関情報編集対応
      *  01.00.07    吉川    14/10/09    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.08    門間    16/03/08    医療機関編集情報への開設者名追加対応
      *  01.00.09    萩野    16/07/14    対象公費はPGオプションから取得する
      *  01.00.10    萩野    18/02/07    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.11    萩野    18/11/07    修正履歴コメント削除
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
      *    新潟後期高齢保険番号データ
           SELECT  TOWN-FILE     ASSIGN FILE-NAME
                                 ORGANIZATION   IS  LINE SEQUENTIAL
                                 ACCESS MODE    IS  SEQUENTIAL
                                 FILE   STATUS  IS  STS-TOWN.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI1521.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
      *    市町村名用データ
       FD  TOWN-FILE.
           COPY  "TOWN1501.INC".
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY  "SC1521.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100   PIC X(02).
           03  STS-RECE02  PIC X(02).
           03  STS-RECEERR  PIC X(02).
           03  STS-TOWN    PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-TOWN       PIC 9(01).
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
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
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
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-KEISAN            PIC 9(08).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
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
      *    市町村名退避エリア
       01  TOWN-DAT.
           03  TOWN-TBL  OCCURS  100.
               05  TOWN-NUM           PIC X(06).
               05  TOWN-SPACE1        PIC X(01).
               05  TOWN-KOKINUM       PIC X(08).
               05  TOWN-SPACE2        PIC X(01).
               05  TOWN-NAME          PIC X(20).
               05  TOWN-KAIGYO        PIC X(01).
      *
      *    パラメータファイル名
           03  FILE-NAME             PIC X(512).
      *
      *    ファイル名
       01  DATA-FILE.
           03  FILLER      PIC X(05)  VALUE  "data/".
           03  FILLER      PIC X(12)  VALUE  "TOWN1501.TXT".
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報
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
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *　　保険者情報
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
           COPY  "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
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
           MOVE    "SEIKYU1521"    TO  JOB-PGID
           MOVE    "新潟・高額受領委任払承認兼支給申請書" 
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
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1521 SYS 1001 ERR ***"
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
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1521 SYS 1002 ERR ***"
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
      *    市町村情報
           PERFORM 1001-TOWN-SEC 
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
      *    市町村名用テキスト読み込み処理 
      *****************************************************************
       1001-TOWN-SEC                SECTION.
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE           ORCSMKPASSAREA
           MOVE  DATA-FILE     TO  MKPASS-IN-01
           CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
           MOVE  MKPASS-OT-01  TO  FILE-NAME
      *
           OPEN  INPUT  TOWN-FILE
      *
           MOVE  ZERO   TO  FLG-TOWN
           PERFORM VARYING IDX FROM 1 BY 1 
                   UNTIL FLG-TOWN = 1
             READ TOWN-FILE NEXT
               AT END
                 MOVE  1         TO  FLG-TOWN
               NOT AT END
                 MOVE  TOWN-REC  TO  TOWN-TBL(IDX)
             END-READ
           END-PERFORM
      *
           CLOSE TOWN-FILE
      *
           .
       1001-TOWN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU1521"              TO  WRK-PRGID
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
           MOVE    ZERO                TO  IDXX
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
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-SRYKA(1)     TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF100-ZAITAKU
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
             INITIALIZE                            WRK-CHK-KOHNUM-TBL
             MOVE    RECE02-KOHNUM(IDZ)        TO  WRK-CHK-KOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
      *
               MOVE  RECE02-KOHINF      (IDX)  TO  MF100-KOHINF
               MOVE  MF100-KOHJKYSNUM          TO  MF100-JKYSNUM
               COMPUTE IDY     =       IDX     +   1
               MOVE  RECE02-JNISSU      (IDY)  TO  MF100-JNISSU
               MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
               MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
               MOVE  RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
               MOVE  RECE02-YKZFTN      (IDY)  TO  MF100-YKZFTN
      *
               MOVE    4                       TO  IDX
             END-IF
           END-PERFORM
      *
      *    保険区分設定
           IF   RECE02-RJNHKNNUM  NOT =  SPACE
             IF     RECE02-HKNNUM     =  "060" OR "067"
               MOVE     4                      TO  MF100-HKNKBN-KEY
             ELSE
               MOVE     5                      TO  MF100-HKNKBN-KEY
             END-IF
           ELSE
             IF     RECE02-HKNNUM     =  "060" OR "067"
               IF     RECE02-AGE      <  70
                  MOVE  1                      TO  MF100-HKNKBN-KEY
               ELSE
                  MOVE  2                      TO  MF100-HKNKBN-KEY
               END-IF
             ELSE
               IF     RECE02-HKNNUM   =  "039" OR "040"
                  MOVE  3                      TO  MF100-HKNKBN-KEY
               END-IF
             END-IF
           END-IF
      *
      *    市町村番号設定(老人は負担者番号、その他は保険者番号)
           IF      RECE02-RJNHKNNUM   NOT       =    SPACE
             MOVE  RECE02-RJNFTNJANUM(3:6)      TO   MF100-CITYNUM   
           ELSE
      *      後期高齢者は外部ファイルから保険者番号を取得する
             IF    RECE02-HKNNUM                =  "039" OR "040"
               PERFORM  VARYING  IDX1  FROM  1  BY  1
                        UNTIL    IDX1    >   100
                 IF  RECE02-HKNJANUM  =  TOWN-KOKINUM(IDX1)
                     MOVE  TOWN-NUM(IDX1)       TO  MF100-CITYNUM
                     MOVE  100                  TO  IDX1
                 END-IF
               END-PERFORM
             ELSE
               IF    RECE02-HKNJANUM(7:2)       =    SPACE
                 MOVE  RECE02-HKNJANUM(1:6)     TO   MF100-CITYNUM
               ELSE
                 MOVE  RECE02-HKNJANUM(3:6)     TO   MF100-CITYNUM
               END-IF
             END-IF
           END-IF
      *
      *    患者住所・電話番号
           STRING   PTINF-HOME-ADRS   DELIMITED BY SPACE
                    PTINF-HOME-BANTI  DELIMITED BY SPACE
                    INTO              MF100-ADRS
           END-STRING
           MOVE     PTINF-HOME-TEL1             TO   MF100-TEL
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR    =    ZERO
             WRITE    MF100-REC 
             ADD      1                 TO    CNT-MF100
           ELSE
               DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                       "公費番号="            MF100-KOHNUM
                       "診療年月="            MF100-SRYYM
           END-IF
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    MF100-CITYNUM(1:5)           TO  WRK-HKNJANUM
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF100-PTNUM
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
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
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
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
      *
           END-PERFORM
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
       310-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                            SC21
      *    印刷日付
           MOVE     WRK-SEIYMDWH             TO  SC21-SEIYMD
      *
      *    診療年月
           MOVE    MF100-SRYYM               TO  WRK-SYMD (1:6)
           MOVE    "01"                      TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3(5:4)     TO  SC21-SRYYY
           MOVE    LNK-DAY2-EDTYMD3(11:4)    TO  SC21-SRYMM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT             TO  SC21-HOSPCD
      *
      *    市町村名
           MOVE    MF100-CITYNUM(1:5)        TO  WRK-HKNJANUM(1:5)
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE  HKNJA-HKNJANAME             TO  SC21-CITYNAME
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS              TO  SC21-ADRS
           INSPECT SC21-ADRS                 REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME              TO  SC21-HOSPNAME
           INSPECT SC21-HOSPNAME             REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME           TO  SC21-KAISETU
      *
      *    電話番号
           MOVE     WRK-HOSPTEL              TO  SC21-TEL
      *
      *    科別
           IF  SYS-1001-HOSPSBT   =  1
      *      診療科目情報取得
             INITIALIZE                SYS-1005-REC
             MOVE    "1005"            TO  SYS-1005-KANRICD
             MOVE    MF100-SRYKA       TO  SYS-1005-KBNCD
             MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1005-STYUKYMD(1:6)
                                           SYS-1005-EDYUKYMD(1:6)
             MOVE    "01"              TO  SYS-1005-STYUKYMD(7:2)
                                           SYS-1005-EDYUKYMD(7:2)
             MOVE    SPA-HOSPNUM       TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC      TO  MCPDATA-REC
             PERFORM 800-SYSKANRI-READ-SEC
             IF      FLG-SYSKANRI      =   ZERO
                 MOVE    MCPDATA-REC         TO  SYS-1005-REC
                 MOVE    SYS-1005-SRYKANAME  TO  SC21-SRYKANAME
             END-IF
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
      *    レセプト種別
      *    国保・社保
           IF     MF100-HKNNUM             =  "039" OR "040"
             MOVE     "○"                TO  SC21-SYUBETU(8)
           ELSE
             IF   MF100-HKNNUM             =  "060" OR "067"
               MOVE   "○"                TO  SC21-SYUBETU(1)
             ELSE
               MOVE   "○"                TO  SC21-SYUBETU(2)
             END-IF
           END-IF
      *
      *    一般・退職・老人
           IF     MF100-RJNHKNNUM          =  SPACE
             IF   MF100-HKNNUM             =  "060"
               MOVE   "○"                TO  SC21-SYUBETU(3)
             ELSE
               MOVE   "○"                TO  SC21-SYUBETU(4)
             END-IF
           ELSE
             MOVE     "○"                TO  SC21-SYUBETU(5)
           END-IF
      *
      *    高８・高９  給付割合
           EVALUATE  MF100-KYURATE
             WHEN  10
               MOVE   "○"                TO  SC21-SYUBETU(6)
                                              SC21-KYURATE(1)
             WHEN  20
               MOVE   "○"                TO  SC21-SYUBETU(6)
                                              SC21-KYURATE(2)
             WHEN  30
               MOVE   "○"                TO  SC21-SYUBETU(7)
                                              SC21-KYURATE(3)
           END-EVALUATE
      *
      *    市町村番号(老人保健負担者番号)
           MOVE    MF100-RJNFTNJANUM      TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT          TO  SC21-CITYNUM
      *
      *    老人保健受給者番号
           MOVE    MF100-RJNJKYSNUM       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT          TO  SC21-JKYSNUM
      *
      *    保険者番号
           MOVE    MF100-HKNJANUM         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           EVALUATE  MF100-HKNNUM
           WHEN "060"
             MOVE    WRK-OUT-INPUT(1:12) TO  SC21-HKNJANUM(5:12)
           WHEN "001"
             MOVE    WRK-OUT-INPUT(1:8)  TO  SC21-HKNJANUM(9:8)
           WHEN OTHER
             MOVE    WRK-OUT-INPUT       TO  SC21-HKNJANUM
           END-EVALUATE  
      *
      *    記号・番号
           MOVE      MF100-KIGO           TO  SC21-KIGO
           MOVE      MF100-NUM            TO  SC21-NUM
      *
      *    氏名・カナ氏名
           MOVE      MF100-NAME           TO  SC21-NAME
           MOVE      MF100-KANANAME       TO  SC21-KANANAME
      *
      *    性別
           IF        MF100-SEX            =   1
             MOVE    "○"                 TO  SC21-SEX(1)
           ELSE
             MOVE    "○"                 TO  SC21-SEX(2)
           END-IF
      *
      *    誕生日
           MOVE    MF100-BIRTHDAY            TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY2-EDTYMD3(1:4)
           WHEN "明治"
             MOVE    "○"                    TO  SC21-GENGO(1)
           WHEN "大正"
             MOVE    "○"                    TO  SC21-GENGO(2)
           WHEN "昭和"
             MOVE    "○"                    TO  SC21-GENGO(3)
           END-EVALUATE
           MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  SC21-TANJOYMD
      *
      *    氏名２(前期高齢者のみ)
           IF      MF100-RJNHKNNUM        =  SPACE   AND
                   MF100-AGE             >=  70
             MOVE  MF100-NAME            TO  SC21-NAME2
           END-IF
      *
      *    患者住所
           MOVE    MF100-ADRS            TO  SC21-PTADRS
      *
      *    患者電話番号
           MOVE    MF100-TEL             TO  SC21-PTTEL
      *
      *    自己負担限度額
           EVALUATE  MF100-KOHNUM
           WHEN "180"
           WHEN "280"
             MOVE    "○"                TO  SC21-KOHKBN(1)
           WHEN "182"
           WHEN "282"
             MOVE    "○"                TO  SC21-KOHKBN(2)
           WHEN "184"
           WHEN "284"
             MOVE    "○"                TO  SC21-KOHKBN(3)
           WHEN "186"
           WHEN "286"
             MOVE    "○"                TO  SC21-KOHKBN(4)
           WHEN "100"
             MOVE    "○"                TO  SC21-KOHKBN(5)
           END-EVALUATE
           IF  MF100-KOHNUM = "280" OR "282" OR "284" OR "286"
             MOVE    "○"                TO  SC21-KOHKBN(6)
           END-IF
      *
      *    実日数
           MOVE    MF100-JNISSU          TO  WRK-Z2
           MOVE    WRK-Z2                TO  SC21-NISSU
      *
      *    点数
           MOVE    MF100-TOTALTEN        TO  WRK-Z7
           MOVE    WRK-Z7                TO  SC21-TENSU
      *
      *    受領委任額
           MOVE     ZERO                 TO  WRK-KEISAN
           COMPUTE  WRK-KEISAN  =  (MF100-TOTALTEN * MF100-KYURATE / 10)
                                -   MF100-FTNMONEY
           MOVE     WRK-KEISAN           TO  WRK-Z9
           MOVE     WRK-Z9               TO  SC21-SEIKYUGK
      *     
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
           ADD      1         TO        CNT-PAGE
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
           MOVE   "SC1521.red"         TO  SPRT-PRTID
           MOVE    "高額受領委任払承認兼支給申請書"
                                    TO  SPRT-TITLE
           MOVE    SC21             TO  SPRT-PRTDATA
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
           DISPLAY "*** SEIKYU1521 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1521 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU1521 END ***"
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
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
           MOVE    "DBSELECT"                TO  MCP-FUNC
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
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                  TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                 TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                 TO  MCPDATA-REC
           MOVE  "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"        TO  MCP-TABLE
             MOVE    "key2"                TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
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
      *    保険組合せ情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    国保でない時は、読み直す
             IF    HKNJA-HKNNUM  NOT  =  "060" AND "067"
               GO               TO    925-HKNJAINF-READ-SEC
             END-IF
      *
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
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
             MOVE  "tbl_syskanri"        TO  MCP-TABLE
             MOVE  "key10"               TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
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
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key10"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
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
           CALL  "ORCDBMAIN"           USING
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
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
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
