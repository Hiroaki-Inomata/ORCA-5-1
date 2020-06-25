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
       PROGRAM-ID.     SEIKYU3207.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 島根福祉老人併用医療費請求書(191,391)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/12/27    山口       再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * ORCA:ver    修正者     日付            内容
      * 2.5.0.ver02  王奇    06/01/17    公費併用の場合は３段記載
      *                                  桁数のバイト数に変更
      *                                  控除額の計算方法を修正
      * 2.6.0.ver01  王奇    06/03/17    仕様変更
      *                                  （縦版は横版に変更）
      * 2.6.0.ver02  王奇    06/04/10    診療年月の修正
      *  01.00.04    内田    07/08/15    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  長期上位（974）は長期と同様の
      *                                  処理をする
      *  01.00.05    門間    08/12/16    SPA-AREA初期化
      *  01.00.06    井上    19/01/09    修正履歴コメント削除
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
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI3203.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *    印刷用データ 名称領域
           COPY  "CPCOMMONPRT.INC".
      *
      *    シェル用領域
           COPY  "CPCOMMONSHELL.INC".
      *
      *    印刷用データ
           COPY  "SC3207.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100	PIC X(02).
           03  STS-RECE02       PIC X(02).
           03  STS-PARA         PIC X(02).
           03  STS-RECEERR      PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IDX4       PIC 9(01).
           03  FLG-KOHNUM     PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-PTINF      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDX4           PIC 9(04).
           03  IDX5           PIC 9(04).
           03  IDX6           PIC 9(04).
           03  IDX7           PIC 9(04).
           03  IDX8           PIC 9(04).
           03  IDX9           PIC 9(04).
           03  IDX10          PIC 9(04).
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
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-GKOUJO          PIC 9(07).
           03  WRK-SUM-SATESU      PIC 9(06).
           03  WRK-SUM-FTNGK       PIC 9(06).
           03  WRK-KYUFURATE       PIC 9(01).
           03  WRK-SUM-SATESU1     PIC 9(06).
      *
           03  WRK-TAHOFTN         PIC 9(07).
           03  WRK-KEISAN          PIC 9(08).
           03  WRK-AMARI           PIC 9(02).
           03  WRK-TAHOKOHNUM      PIC X(03).
           03  WRK-ETC-FTNJANUM    PIC X(20).
      *
           03  WRK-ETC-JKYJANUM    PIC X(20).
      *
           03  WRK-DENPPRTYMWH  PIC X(16).
           03  WRK-SYSYMDWH     PIC X(22).
           03  WRK-SEIYMDWH     PIC X(22).
           03  WRK-SRYYM        PIC X(09).
           03  WRK-HOSPCD       PIC 9(01).
           03  WRK-KENSYO       PIC 9(02).
           03  WRK-GKENSYO      PIC 9(02).
      *
           03  WRK-KOHNUM       PIC X(03).
           03  WRK-KOHID        PIC 9(10).
      *
           03  WRK-KESE         PIC  9(06).
           03  WRK-GKESE        PIC  9(06).
      *
           03  WRK-SYMD.
               05  WRK-SYY   PIC 9(04).
               05  WRK-SMM   PIC 9(02).
               05  WRK-SDD   PIC 9(02).
           03  WRK-LASTYMD   PIC 9(08).
           03  WRK-HENYMDG   PIC X(22).
           03  WRK-HENYMDG1  PIC X(09).
           03  WRK-HKNJANUM  PIC X(05).
      *
      *    編集用エリア
           03  WRK-NISSUX      PIC Z9.
           03  WRK-ZERO3        PIC ZZ9.
           03  WRK-ZERO5        PIC Z,ZZ9.
           03  WRK-ZERO2        PIC Z9.
           03  WRK-ZERO4        PIC ZZZ9.
           03  WRK-ZERO6        PIC ZZ,ZZ9.
           03  WRK-ZERO7        PIC ZZZ,ZZ9.
           03  WRK-ZERO9        PIC Z,ZZZ,ZZ9.
           03  WRK-Z1           PIC Z.
           03  WRK-Z2           PIC ZZ.
           03  WRK-Z7           PIC ZZZ,ZZZ.
           03  WRK-Z5           PIC Z,ZZZ.
           03  WRK-Z6           PIC ZZ,ZZZ.
           03  WRK-Z9           PIC Z,ZZZ,ZZZ.
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH              PIC  X(50).
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
      *    集計用エリア
       01  SUM-AREA  VALUE   ZERO.
           03  SUM-SYOKEI.
               05  SUM-SKENSU      PIC 9(10).
           03  SUM-TABLE	OCCURS  4.
               05  SUM-KENSU	PIC  9(04).
               05  SUM-NISSU	PIC  9(05).
               05  SUM-TENSU	PIC  9(07).
               05  SUM-KOHFTN	PIC  9(07).
               05  SUM-KOUJO	PIC  9(09).
               05  SUM-TYOKI	PIC  9(02).
       01  SUM-AREA1            VALUE  ZERO.
           03  SUM-TABLE1       OCCURS  2.
             05  SUM-TABLE2     OCCURS  100.
               07  SUM-GKENSU   PIC  9(04).
               07  SUM-GNISSU   PIC  9(05).
               07  SUM-GTENSU   PIC  9(07).
               07  SUM-GKOHFTN  PIC  9(07).
               07  SUM-GTYOKI   PIC  9(02).
       01  SUM-AREA3            VALUE  ZERO.
           03  SUM-KOHTABLE1      OCCURS  100.
             05  SUM-KOHTABLE2    OCCURS  100.
               07  SUM-KOHNUM     PIC  X(02).
               07  SUM-KKENSU     PIC  9(04).
               07  SUM-KTENSU     PIC  9(07).
               07  SUM-KFTNKG     PIC  9(09).
      *
       01  SUM-AREA4           VALUE  ZERO.
           03  SUM-GKOHTABLE1     OCCURS  100.
             05  SUM-GKOHNUM      PIC  X(02).
             05  SUM-GKKENSU      PIC  9(04).
             05  SUM-GKTENSU      PIC  9(07).
             05  SUM-GKFTNKG      PIC  9(09).
      *
      *    キー用エリア
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-FTNJANUM   PIC X(03).
           03  KEY-OLD.
               05  KEY-O-FTNJANUM   PIC X(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-LINE    PIC 9(02).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PRINT   PIC 9(06).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
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
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    請求管理
           COPY  "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    患者公費情報マスタ
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
           COPY  "MCPDATA.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE  SUM-AREA1
           INITIALIZE  SUM-AREA3
           INITIALIZE  SUM-AREA4
           INITIALIZE  SPA-AREA
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
           MOVE    "SEIKYU3207"    TO  JOB-PGID
           MOVE    "福祉医療（老人併用）費請求書" 
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
           MOVE  LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE  "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)  TO  WRK-DENPPRTYMWH
           MOVE  WRK-HENYMDG1       TO  WRK-SRYYM
      *
      *    システム日付セット
           MOVE  LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SYSYMDWH
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
           MOVE  "MF100ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)  TO  MF100PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM           TO  MF100PARA-HOSPNUM
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1001-STYUKYMD(1:6)
                                   SYS-1001-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU3203 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1002-STYUKYMD(1:6)
                                   SYS-1002-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU3203 SYS 1002 ERR ***" 
             MOVE  1            TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
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
      *    集計処理１
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
             IF  RECE02-KOHNUM (IDZ) =   "191" OR "391"
               IF  RECE02-RJNHKNNUM = "027"
                 PERFORM  20011-SYUKEI-SEC
               END-IF
             END-IF
           END-PERFORM
      *
           PERFORM 900-KOHSKY-READ-SEC
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
           INITIALIZE  MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
           MOVE  RECE02-RECESYUBETU  TO  MF100-RECESYUBETU
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
      *
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF100-FTNJANUM
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF100-KOHJKYSNUM
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSUINF
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-FTNMONEY (IDY)     TO  WRK-GKOUJO
           MOVE  RECE02-FTNMONEY (1)       TO  MF100-RJNFTN
           MOVE  RECE02-YKZFTNINF   (IDY)  TO  MF100-YKZFTNINF
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
      *
           MOVE     1                      TO   FLG-ERR
           MOVE     MF100-KOHNUM           TO  WRK-KOHNUM
           MOVE     MF100-KOHID            TO  WRK-KOHID
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"             TO  WRK-PATH
           ELSE
               MOVE    "key25"             TO  WRK-PATH
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
      *    請求管理ＲＥＡＤ
           PERFORM  810-SEIKYU-READ-SEC
      *
           IF    MF100-TYOKI    =  1
           AND   MF100-RJNFTN   >=  10000
             MOVE  10000        TO  MF100-RJNFTN
             DISPLAY  "長期限度額 = " RECE02-PTNUM
           END-IF
      *
           IF    MF100-TYOKI    =  1
           AND   WRK-GKOUJO    >=  10000
               MOVE    10000   TO  WRK-GKOUJO
           END-IF
      *
           IF    MF100-TYOKI  =  1
           AND   MF100-RJNFTN  >=  10000
                 MOVE  10000           TO  MF100-RJNFTN
           END-IF
      *
           IF    MF100-TYOKI  =  1
           AND   MF100-TAHOFTN  >=  10000
                 MOVE  10000           TO  MF100-TAHOFTN
           END-IF
      *    長期上位
           IF    MF100-TYOKI    =  2
           AND   MF100-RJNFTN   >=  20000
             MOVE  20000        TO  MF100-RJNFTN
             DISPLAY  "長期限度額 = " RECE02-PTNUM
           END-IF
      *
           IF    MF100-TYOKI    =  2
           AND   WRK-GKOUJO    >=  20000
               MOVE    20000   TO  WRK-GKOUJO
           END-IF
      *
           IF    MF100-TYOKI  =  2
           AND   MF100-RJNFTN  >=  20000
                 MOVE  20000           TO  MF100-RJNFTN
           END-IF
      *
           IF    MF100-TYOKI  =  2
           AND   MF100-TAHOFTN  >=  20000
                 MOVE  20000           TO  MF100-TAHOFTN
           END-IF
      *
           MOVE  WRK-GKOUJO  TO  MF100-KOUJO
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR    =    ZERO
             WRITE  MF100-REC
             ADD   1  TO  CNT-MF100
           ELSE
             DISPLAY "対象外患者番号="  RECE02-PTNUM
           END-IF
      *
           INITIALIZE  WRK-GKOUJO
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
           INITIALIZE      WRK-HKNJANUM
           MOVE    "32"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-KOHFTNJANUM(5:3)   TO  WRK-HKNJANUM(3:3)
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
      *    帳票合計処理
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
             MOVE  1                    TO  IDX6
      *
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
             MOVE  KEY-NEW              TO  KEY-OLD
      *
             MOVE  0                    TO  IDX7
      *
               PERFORM   UNTIL  FLG-PRINT      =   1
                         OR     KEY-NEW    NOT =   KEY-OLD
      *    入外院集計
      *
               IF  MF100-RECESYUBETU(4:1) = "7" OR "9"
                 MOVE  1  TO  IDX
               END-IF
               IF  MF100-RECESYUBETU(4:1) = "8" OR "0"
                 MOVE  2  TO  IDX
               END-IF
      *    件数集計
               ADD  1  TO  SUM-KENSU (IDX)
      *
      *    日数集計
               ADD  MF100-JNISSU    TO  SUM-NISSU (IDX)
      *
      *    点数集計
               ADD  MF100-TOTALTEN  TO  SUM-TENSU (IDX)
      *    負担金額集計
               IF  MF100-TAHO    NOT = "1"
                 ADD  MF100-RJNFTN    TO  SUM-KOHFTN (IDX)
               ELSE
                 COMPUTE  WRK-SUM-SATESU  = MF100-TOTALTEN
                                          - MF100-TAHOTEN
                 MOVE  MF100-KYURATE (2:1) TO WRK-KYUFURATE
                 COMPUTE  WRK-SUM-FTNGK   = WRK-SUM-SATESU * 10
                                          * WRK-KYUFURATE / 10
                 ADD  WRK-SUM-FTNGK       TO  SUM-KOHFTN (IDX)
               END-IF
      *
      *    ０２長件数
               IF  MF100-TYOKI = 1 OR 2
                 ADD 1              TO  SUM-TYOKI (IDX)
               END-IF
      *
      *
      *     91集計
      *
                 MOVE  "91"         TO  SUM-GKOHNUM(IDX6)
                 ADD   1            TO  SUM-GKKENSU(IDX6)
      *
                 COMPUTE  SUM-GKFTNKG(IDX6) = SUM-GKFTNKG(IDX6)
                                            + MF100-KOUJO
                 IF MF100-TAHO   =   1
                    COMPUTE  SUM-GKTENSU(IDX6) = SUM-GKTENSU(IDX6) +
                             ( MF100-TOTALTEN - MF100-TAHOTEN )
                 ELSE
                    COMPUTE  SUM-GKTENSU(IDX6)  = SUM-GKTENSU(IDX6)
                                                + MF100-TOTALTEN
                 END-IF
      *
      *     公費再掲集計
                 IF  MF100-TAHO  =  1
      *
                   COMPUTE  IDX9 = IDX7 + 1
                   PERFORM  VARYING IDX8 FROM 1 BY 1
                            UNTIL   IDX8  >  IDX9
                     IF  MF100-TAHOFTNJANUM(1:2) = 
                                              SUM-KOHNUM(IDX6 IDX8)
                       MOVE  1                TO  FLG-KOHNUM
                       MOVE  IDX8             TO  IDX7
                       MOVE  IDX9             TO  IDX8
                     ELSE
                       MOVE  0                TO  FLG-KOHNUM
                     END-IF
      *
                   END-PERFORM
      *
                   IF FLG-KOHNUM = 1
                      MOVE  MF100-TAHOFTNJANUM(1:2)
                                              TO SUM-KOHNUM(IDX6 IDX7)
                      ADD   1                 TO SUM-KKENSU(IDX6 IDX7)
                      ADD   MF100-TAHOTEN
                                              TO SUM-KTENSU(IDX6 IDX7)
                        ADD MF100-TAHOFTN     TO SUM-KFTNKG(IDX6 IDX7)
                   ELSE
                      ADD 1                   TO IDX7
                      MOVE  MF100-TAHOFTNJANUM(1:2)
                                              TO SUM-KOHNUM(IDX6 IDX7)
                      MOVE  1                 TO SUM-KKENSU(IDX6 IDX7)
                      MOVE  MF100-TAHOTEN     TO SUM-KTENSU(IDX6 IDX7)
                        MOVE  MF100-TAHOFTN   TO SUM-KFTNKG(IDX6 IDX7)
                   END-IF
                 END-IF
      *
      *     一時ファイル読込
      *
                 PERFORM  900-TMP-READ-SEC
      *
               END-PERFORM
      *
               ADD  1                    TO  IDX6
      *
               IF    FLG-PRINT            =  1
               OR    KEY-N-FTNJANUM  NOT  =  KEY-O-FTNJANUM
                 ADD     1      TO   IDX5
                 MOVE    SUM-KENSU(1)   TO  SUM-GKENSU(1 IDX5)
                 MOVE    SUM-NISSU(1)   TO  SUM-GNISSU(1 IDX5)
                 MOVE    SUM-TENSU(1)   TO  SUM-GTENSU(1 IDX5)
                 MOVE    SUM-KOHFTN(1)  TO  SUM-GKOHFTN(1 IDX5)
                 MOVE    SUM-TYOKI(1)   TO  SUM-GTYOKI(1 IDX5)
                 MOVE    SUM-KENSU(2)   TO  SUM-GKENSU(2 IDX5)
                 MOVE    SUM-NISSU(2)   TO  SUM-GNISSU(2 IDX5)
                 MOVE    SUM-TENSU(2)   TO  SUM-GTENSU(2 IDX5)
                 MOVE    SUM-KOHFTN(2)  TO  SUM-GKOHFTN(2 IDX5)
                 MOVE    SUM-TYOKI(2)   TO  SUM-GTYOKI(2 IDX5)
      *
                 INITIALIZE  SUM-AREA
      *
               END-IF
      *
           END-PERFORM
      *
           CLOSE    MF100-FILE
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
           IF  IDX4        = 0
           OR  KEY-NEW NOT = KEY-OLD
               ADD  1      TO  IDX4
               MOVE 1      TO  FLG-IDX4
           ELSE
               MOVE 0      TO  FLG-IDX4
           END-IF
      *
      *    帳票編集<見出し>処理
             PERFORM 310-HEAD-HEN-SEC
      *
      *    帳票編集<明細>処理
             PERFORM 320-BODY-HEN-SEC
      *
      *    帳票編集<小計>処理
             IF  FLG-IDX4 = 1
               ADD  1          TO  IDX10
               PERFORM 325-SYOKEI-HEN-SEC
             END-IF
      *
      *    帳票印刷処理
             PERFORM 330-PRINT-OUT-SEC
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
       310-HEAD-HEN-SEC  SECTION.
      *
           MOVE  ZERO  TO  CNT-LINE
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC07
      *
      *    印刷日付
           MOVE  WRK-DENPPRTYMWH(5:4)   TO  SC07-PRTYY
           MOVE  WRK-DENPPRTYMWH(11:4)  TO  SC07-PRTMM 
      *
      *    福祉医療負担者番号
           MOVE  MF100-KOHFTNJANUM(5:)  TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT          TO  SC07-KOHFTNJANUM
      *
      *    市町村名・市町村番号
           DISPLAY "負担者番号=" MF100-FTNJANUM
           MOVE  MF100-KOHFTNJANUM(3:5) TO      WRK-HKNJANUM
           PERFORM  920-HKNJAINF-SET-SEC
           IF       FLG-HKNJAINF        =   ZERO
                    MOVE    HKNJA-HKNJANAME
                                        TO  SC07-TOWNNAME
                    DISPLAY "保険者名取得成功"
           ELSE
                    MOVE    "該当無し"  TO  SC07-TOWNNAME
           END-IF
      *
      *    市町村番号
           MOVE    HKNJA-HKNJANUM(3:4)     TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC07-TOWNNUM
      *
      *    請求日付
           MOVE  WRK-SYSYMDWH  TO  SC07-SEIYMD
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCD     TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT       TO  SC07-HOSPCD
      *
      *    住所
           MOVE  WRK-HOSPADRS          TO  SC07-ADRS
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME          TO  SC07-HOSPNAME
      *
      *    管理者名
           MOVE  SYS-1001-KAISETUNAME  TO  SC07-KANRINAME
      *
      *    電話番号
           MOVE  WRK-HOSPTEL           TO  WRK-MAE-INPUT
           PERFORM  2009-HENKAN-SEC
           MOVE  WRK-OUT-INPUT         TO  SC07-TEL
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
           INITIALIZE  SUM-SYOKEI
           INITIALIZE  SUM-AREA
           MOVE    KEY-NEW     TO  KEY-OLD
      *
           PERFORM VARYING IDX1 FROM    1   BY  1
                   UNTIL   IDX1 >       14
                   OR      KEY-NEW     NOT =   KEY-OLD
                   OR      FLG-PRINT       =   1
      *
             IF  MF100-TAHO    =    "1"    AND
                 IDX1      >= 19
               DISPLAY "公費併用による改ページ"
               DISPLAY  "IDX1 = "  IDX1
               DISPLAY  "MF100-PTNUM = "  MF100-PTNUM
             ELSE
      *
      *    受給資格証記号番号（受給者番号）
               MOVE  MF100-KOHJKYSNUM  TO  SC07-JKYSNUM (IDX1)
      *    氏名
               MOVE  MF100-NAME        TO  SC07-NAME (IDX1)
      *    保険者番号
               MOVE  MF100-HKNJANUM    TO  SC07-HKNJANUM (IDX1)
      *    被保険者証記号番号
               STRING  MF100-KIGO  DELIMITED  BY  SPACE
                       MF100-NUM   DELIMITED  BY  SPACE
                       INTO       SC07-KIGONUM (IDX1)
               END-STRING
      *    老人医療受給者番号
               MOVE  MF100-RJNJKYSNUM  TO  SC07-RJNNUM (IDX1)
      *    診療年月
               MOVE  WRK-SRYYM (2:2)   TO  SC07-SRYYM (IDX1)(1:2)
               MOVE  WRK-SRYYM (5:2)   TO  SC07-SRYYM (IDX1)(3:2)
               IF  WRK-SRYYM(5:1)  =  SPACE
                 MOVE  "0"  TO  SC07-SRYYM(IDX1)  (3:1)
               END-IF
      *    入外区分
               MOVE    MF100-RECESYUBETU (4:1)
                                       TO  SC07-NYUGAIKBN (IDX1)
      *    生年月日
               MOVE   MF100-BIRTHDAY  TO  WRK-SYMD
               PERFORM  31012-SEIWA-HEN-SEC
               MOVE   WRK-HENYMDG1    TO  SC07-BIRTHDAY (IDX1)
      *    給付割合
               EVALUATE  MF100-KYURATE(2:1)
                 WHEN  1
                   MOVE  "9"  TO  SC07-KYUFURATE (IDX1)
                 WHEN  2
                   MOVE  "8"  TO  SC07-KYUFURATE (IDX1)
                 WHEN  3
                   MOVE  "7"  TO  SC07-KYUFURATE (IDX1)
               END-EVALUATE
      *    長期療養
               IF  MF100-TYOKI = 1 OR 2
                 MOVE  "02"  TO  SC07-TYOKI (IDX1)
               ELSE
                 MOVE  "  "  TO  SC07-TYOKI (IDX1)
               END-IF
      *    在宅区分
               MOVE  MF100-ZAITAKU   TO  WRK-Z1
               MOVE  WRK-Z1          TO  SC07-ZAITAKU (IDX1)
      *    日数
               MOVE  MF100-JNISSU    TO  WRK-Z2
               MOVE  WRK-Z2          TO  SC07-NISSU (IDX1)
             IF  MF100-TAHO           NOT   = "1"
      *    福祉医療点数
               MOVE  MF100-TOTALTEN  TO  WRK-Z7
               MOVE  WRK-Z7          TO  SC07-TENSU  (IDX1)
      *    老人医療一部負担
               MOVE  MF100-RJNFTN    TO  WRK-Z7
               MOVE  WRK-Z7          TO  SC07-FTNMONEY (IDX1)
      *    控除額
               MOVE  MF100-KOUJO     TO  WRK-Z6
               MOVE  WRK-Z6          TO  SC07-KOUJO (IDX1)
            ELSE
      *    上段
      *    総点数(a)
      *    保険点数
               MOVE  MF100-TOTALTEN  TO  WRK-Z7
               MOVE  WRK-Z7          TO  SC07-TENSU  (IDX1)
      *    負担金(d)
      *    一部負担金額
      ****2006/03/17 @@@@-BRAND START
      *        MOVE  MF100-RJNFTN    TO  WRK-Z7
      *        MOVE  WRK-Z7          TO  SC07-RJNFTN  (IDX1)
      *    中段
      *        ADD   1               TO  IDX1
      *    負担者番号
      *    公費負担者番号１
               MOVE  MF100-TAHOFTNJANUM
                                     TO  SC07-KOH1FTNJANUM(IDX1)
      *    公費受給者番号１
               MOVE  MF100-TAHOJKYJANUM
                                     TO  SC07-KOH1JKYJANUM(IDX1)
      *    第1公費(b)
      *    公費１点数
               MOVE  MF100-TAHOTEN   TO  WRK-Z7
               MOVE  WRK-Z7          TO  SC07-KOH1TEN (IDX1)
      *    患者負担金(e) 
      *    公費１一部負担金額
                 MOVE  MF100-TAHOFTN   TO  WRK-Z7
                 MOVE  WRK-Z7          TO  SC07-KOH1FTN  (IDX1)
      *    下段
      *        ADD   1               TO  IDX1
      *    差点数(c)
      *    公費併用時福祉点数
               COMPUTE  WRK-SUM-SATESU  =  MF100-TOTALTEN  -
                                           MF100-TAHOTEN
               MOVE  WRK-SUM-SATESU  TO  WRK-Z7
               MOVE  WRK-Z7          TO  SC07-FKSTEN (IDX1)
      *    一部負担金額
               MOVE  SC07-KYUFURATE (IDX1) TO  WRK-KYUFURATE
               COMPUTE  WRK-SUM-FTNGK   =  WRK-SUM-SATESU * 10 
                                        *  (10 - WRK-KYUFURATE) /10
               MOVE  WRK-SUM-FTNGK   TO  WRK-Z7
               MOVE  WRK-Z7          TO  SC07-FTNMONEY  (IDX1)
      *    負担金(f)
               MOVE  MF100-KOUJO     TO  WRK-Z6
               MOVE  WRK-Z6          TO  SC07-KOUJO  (IDX1)
            END-IF
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
             END-IF
           END-PERFORM
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *              帳票編集<小計>処理
      *****************************************************************
       325-SYOKEI-HEN-SEC  SECTION.
      *
      *    件数・日数・点数・負担金・控除額・長期療養件数
      *    公費再掲集計
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX > 2
               MOVE  SUM-GKENSU (IDX IDX4)  TO  WRK-Z5
               MOVE  WRK-Z5           TO  SC07-KENSU (IDX)
               MOVE  SUM-GNISSU (IDX IDX4)  TO  WRK-Z5
               MOVE  WRK-Z5           TO  SC07-GNISSU (IDX)
               MOVE  SUM-GTENSU (IDX IDX4)  TO  WRK-Z9
               MOVE  WRK-Z9           TO  SC07-GTENSU (IDX)
               MOVE  SUM-GKOHFTN (IDX IDX4) TO  WRK-Z9
               MOVE  WRK-Z9           TO  SC07-GRJNFTN (IDX)
               MOVE  SUM-GTYOKI (IDX IDX4)  TO  WRK-Z2
               MOVE  WRK-Z2           TO  SC07-GTYOKI (IDX)
           END-PERFORM
      *
      *    91集計
           MOVE  SUM-GKOHNUM(IDX10)            TO  SC07-KOHBANGO
           MOVE  SUM-GKKENSU(IDX10)            TO  WRK-Z5
           MOVE  WRK-Z5                        TO  SC07-BKENSU
           MOVE  SUM-GKTENSU(IDX10)            TO  WRK-Z9
           MOVE  WRK-Z9                        TO  SC07-BTENSU
           MOVE  SUM-GKFTNKG(IDX10)            TO  WRK-Z9
           MOVE  WRK-Z9                        TO  SC07-BRJNFTN
      *
      *    公費再掲
           PERFORM VARYING IDX FROM      1  BY  1
                   UNTIL   IDX   > 3
               MOVE  SUM-KOHNUM(IDX10 IDX)  TO  WRK-Z2
               MOVE  WRK-Z2                 TO  SC07-ZAIKEI(IDX)
               MOVE  SUM-KKENSU(IDX10 IDX)  TO  WRK-Z5
               MOVE  WRK-Z5                 TO  SC07-ZKENSU(IDX)
               MOVE  SUM-KTENSU(IDX10 IDX)  TO  WRK-Z9
               MOVE  WRK-Z9                 TO  SC07-ZTENSU(IDX)
               MOVE  SUM-KFTNKG(IDX10 IDX)  TO  WRK-Z9
               MOVE  WRK-Z9                 TO  SC07-ZRJFTN(IDX)
           END-PERFORM
      *
           .
       325-SYOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC  SECTION.
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
           MOVE   "SC3207.red"             TO  SPRT-PRTID
           MOVE    "福祉（老人併用）医療費請求書"
                                       TO  SPRT-TITLE
           MOVE    SC07                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
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
           MOVE  LNK-DAY2-EDTYMD3  TO  WRK-HENYMDG
           MOVE  LNK-DAY2-EDTYMD1  TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE  ZERO            TO  WRK-TAHOFTN
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   MOVE    1       TO      MF100-TAHO
      *
                   MOVE     SYU-KOH-HKNNUM(IDX)  TO  WRK-TAHOKOHNUM
                   PERFORM  930-PTKOHINF-SET-SEC
                   MOVE     WRK-ETC-FTNJANUM     TO  MF100-TAHOFTNJANUM
                   MOVE     WRK-ETC-JKYJANUM     TO  MF100-TAHOJKYJANUM
      *
                   ADD    SYU-TOTAL-HKNTEN   TO  MF100-TAHOTEN
                   MOVE   SYU-KOH-COMPFTN(IDX)
                                           TO  WRK-TAHOFTN
      *
                   DIVIDE   WRK-TAHOFTN   BY  10
                            GIVING             WRK-KEISAN
                            REMAINDER          WRK-AMARI
                   COMPUTE  WRK-TAHOFTN    =  WRK-TAHOFTN
                                       -  WRK-AMARI
                   IF  WRK-AMARI     >=     5
                     ADD    10             TO  WRK-TAHOFTN
                   END-IF
      *
                   IF    SYU-KOH-HKNNUM(IDX)  =  "010" OR "021"
                       MOVE  1           TO  MF100-KESEFLG
                   END-IF
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      MF100-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      MF100-TYOKI
               END-IF
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
           ADD     WRK-TAHOFTN        TO  MF100-TAHOFTN
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  請求管理ＲＥＡＤ
      *****************************************************************
       810-SEIKYU-READ-SEC          SECTION.
      *
             INITIALIZE  RECE10-REC
             MOVE  SPA-HOSPNUM           TO  RECE10-HOSPNUM
             MOVE  RECE02-SRYYM(1:6)     TO  RECE10-SRYYM
             MOVE  RECE02-PTID           TO  RECE10-PTID
             MOVE  RECE02-RECEKA         TO  RECE10-RECEKA
             MOVE  RECE10-REC            TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_seikyu"          TO  MCP-TABLE
             MOVE  "key2"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "DBFETCH"               TO  MCP-FUNC
             MOVE  "tbl_seikyu"          TO  MCP-TABLE
             MOVE  "key2"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
               IF  MCP-RC  =  ZERO
                 MOVE  RECE10-ZAITAKU    TO  MF100-ZAITAKU
               ELSE
                  DISPLAY "請求管理 READ  ERRO!!!"
               END-IF
             ELSE
               DISPLAY "請求管理 READ  ERRO!!!"
	     END-IF
      *
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_seikyu"          TO  MCP-TABLE
             MOVE  "key2"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       810-SEIKYU-READ-EXT.
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
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    WRK-PATH       TO  MCP-PATHNAME
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
                 MOVE    "tbl_syunou"   TO  MCP-TABLE
                 MOVE    WRK-PATH       TO  MCP-PATHNAME
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"      USING
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU3207 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU3207 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU3207 END ***"
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC  SECTION.
      *
           READ  MF100-FILE  NEXT
             AT  END
               MOVE  HIGH-VALUE  TO  KEY-NEW
               MOVE  1  TO  FLG-PRINT
             NOT AT  END
               MOVE  MF100-FTNJANUM (5:3)  TO  KEY-N-FTNJANUM
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
           MOVE    "DBSELECT"            TO  MCP-FUNC
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
           MOVE    "DBFETCH"             TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF  RECE02-KOHNUM(1) = "191" OR "391"
                OR RECE02-KOHNUM(2) = "191" OR "391"
                OR RECE02-KOHNUM(3) = "191" OR "391"
                OR RECE02-KOHNUM(4) = "191" OR "391"
      *    テスト患者のときは読み飛ばす
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
                   IF RECE02-RJNHKNNUM NOT = "027"
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
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC        SECTION.
      *
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
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
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM              TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM             TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC             TO  MCPDATA-REC
           MOVE  "DBSELECT"               TO  MCP-FUNC
           MOVE  "tbl_hknjainf"           TO  MCP-TABLE
           MOVE  "key2"                   TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "tbl_hknjainf"         TO  MCP-TABLE
             MOVE  "key2"                 TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_hknjainf"           TO  MCP-TABLE
           MOVE  "key2"                   TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
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
           CALL  "ORCDBMAIN"              USING
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
      *    公費の負担者番号取得   2006/01/06  INS
      *****************************************************************
       930-PTKOHINF-SET-SEC  SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE  SPA-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE  RECE02-PTID        TO  PTKOH-PTID
           MOVE  WRK-TAHOKOHNUM     TO  PTKOH-KOHNUM
      *
           MOVE  PTKOHINF-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE  "tbl_ptkohinf"            TO  MCP-TABLE
           MOVE  "key4"                    TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"               USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "tbl_ptkohinf"          TO  MCP-TABLE
             MOVE  "key4"                  TO  MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
             PERFORM  UNTIL  FLG-PTKOHINF = 1
               IF  PTKOH-SAKJOKBN  NOT = "1"
                 IF  PTKOH-TEKSTYMD(1:6) <= RECE02-SRYYM(1:6)  AND 
                     PTKOH-TEKEDYMD(1:6) >= RECE02-SRYYM(1:6)
                     MOVE  PTKOH-FTNJANUM  TO  WRK-ETC-FTNJANUM
                     MOVE  PTKOH-JKYSNUM   TO  WRK-ETC-JKYJANUM
                 END-IF
               END-IF
               MOVE  "tbl_ptkohinf"        TO  MCP-TABLE
               MOVE  "key4"                TO  MCP-PATHNAME
                 PERFORM  935-PTKOHINF-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE  "key4"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       930-PTKOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の負担者番号取得   2006/01/06  INS
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  PTKOHINF-REC
             MOVE  ZERO         TO  FLG-PTKOHINF
           ELSE
             INITIALIZE  PTKOHINF-REC
             MOVE  1  TO  FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
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
           MOVE  "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-STYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-EDYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-EDYUKYMD(7:2)
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-EDYUKYMD(7:2)
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
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
