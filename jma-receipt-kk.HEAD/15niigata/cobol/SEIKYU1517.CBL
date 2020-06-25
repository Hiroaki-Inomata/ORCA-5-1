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
       PROGRAM-ID.     SEIKYU1517.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 新潟・県単医療費助成金請求内訳書
      *                      (160,260,360,460,161,261,361,461,163,263)
      *                      (165,265,190,290,164,264,364,464)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/08    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (個別発行対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田　　06/08/27    公費分点数が２倍になるのを修正
      *                                  更正の上限を考慮するよう修正
      *                                  「１５」に○をつける（複数公費の対応）
      *  01.00.02    内田　　06/11/25    帳票様式の変更に伴う修正
      *  01.00.03    内田　　07/04/22    帳票様式の変更による再作成
      *  01.00.04    内田　　07/06/15    食事標準負担額の印字箇所の判断修正
      *                                  長期上位対応
      *                                  ７０歳未満高額療養費の場合、
      *                                  自己負担適用区分に○を印字
      *  01.00.05    内田　　07/09/11    ７０歳未満でも２７老人の場合は
      *                                  給付割合「高９」等へ印字
      *  01.00.06    内田    07/09/21    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.07    内田　　08/03/22    複数保険所持時の長期欄の○印字修正
      *                                  長期所持時の患者負担額の修正
      *                                  患者負担額の四捨五入修正
      *  01.00.08    内田    08/06/24    帳票様式の変更に伴う修正
      *                                  公費の助成がない場合は印字対象外
      *                                  一部負担金欄は０の場合は０印字
      *                                  保険番号１６０等使用時、
      *                                  負担金が印字されないのを修正
      *  01.00.09    内田    09/01/28    助成の有無の判定修正
      *                                  SPA-AREA初期化
      *  01.00.10    吉川    10/02/08    複数科診療の場合、患者負担額が
      *                                  四捨五入されない不具合を修正
      *  01.00.11    吉川    10/12/06    open-cobol1.0対応
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
           COPY  "SEI1517.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
      *---(01.00.06) LINE ADD START ------------------------------------
      *     COPY  "CPCOMMONDAT2.INC"
      *             REPLACING  //RECE01PARA//
      *             BY         //RECEERR//.
      *     03      PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           COPY  "SC1517.INC".
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
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-SRYACT     PIC 9(01).
           03  FLG-HKNNUM     PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-ERR2       PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-PTKOHINF   PIC 9(01).
           03  FLG-KEIKA      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
      *---(01.00.07) LINE ADD START ------------------------------------
           03  FLG-RECE10     PIC 9(01).
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *---(01.00.06) LINE ADD END   ------------------------------------
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
      *---(01.00.08) LINE ADD START ------------------------------------
           03  WRK-Z79               PIC ZZZ,ZZ9.
      *---(01.00.08) LINE ADD END   ------------------------------------
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
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-SRYCD             PIC X(09).
           03  WRK-AMARI             PIC 9(05).
           03  WRK-KEISAN2           PIC 9(08).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-COMPFTN           PIC 9(07).
           03  WRK-TAHOTEN           PIC 9(09).
           03  WRK-KEISAN3           PIC 9(08).
           03  WRK-TYOKI             PIC 9(08).
           03  WRK-BIKO              PIC X(16).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *     03  WRK-PATH.
      *         05  FILLER            PIC S9(09)  BINARY.
      *         05  FILLER            PIC S9(09)  BINARY.
      *         05  FILLER            PIC S9(09)  BINARY.
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
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           03  KASAN-AREA.
               05  KASAN-TBL    OCCURS  4.
                   07  KASAN-COMPFTN            PIC 9(08).
                   07  KASAN-DENPNUM            PIC 9(07).
                   07  KASAN-TAHO               PIC 9(01).
                   07  KASAN-TAHOFTN            PIC 9(07).
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.08) LINE ADD START ------------------------------------
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *---(01.00.08) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *    請求管理
           COPY    "CPRCF010.INC".
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY    "MCPDATA.INC".
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
      *---(01.00.09) LINE ADD START ------------------------------------
           INITIALIZE  SPA-AREA
      *---(01.00.09) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
                                               WRK-PARA-SYORIFLG
      *---(01.00.06) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.06) LINE ADD END   ------------------------------------
           END-UNSTRING
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU1517"    TO  JOB-PGID
           MOVE    "新潟・県単医療費助成金請求内訳書" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
           MOVE  "MF100ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)  TO  MF100PARA-TERMID
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  WRK-PARA-HOSPNUM           TO  MF100PARA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  "RECEERR"        TO  RECEERR-FILE-ID
      *     MOVE  LNK-PRTKANRI-TERMID(1:16)
      *                            TO  RECEERR-TERMID
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1517 SYS 1001 ERR ***"
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
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1517 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
      *
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
      *---(01.00.06) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
      *         MOVE    "DBSELECT"          TO  MCP-FUNC
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
      *---(01.00.06) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *    月途中の公費切替え対応
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >     4
             IF  RECE02-KOHNUM (IDZ) = "160" OR "260" OR "360" OR "460"
                                    OR "161" OR "261" OR "361" OR "461"
                                    OR "164" OR "264" OR "364" OR "464"
                                    OR "163" OR "263" OR "165" OR "265"
                                    OR "190" OR "290" OR "561" OR "564"
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
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF100-REC
           INITIALIZE       WRK-TYOKI
                            FLG-TYOKI
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  RECE02-HOSPID       TO  MF100-HOSPID
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF100-ZAITAKU
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE  MF100-KOHJKYSNUM          TO  MF100-JKYSNUM
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSU      (IDY)  TO  MF100-JNISSU
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
           MOVE  RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
           MOVE  RECE02-YKZFTN      (IDY)  TO  MF100-YKZFTN
      *
      *    ソートKEY設定
           MOVE    MF100-KOHFTNJANUM(5:3)    TO   MF100-CITYNUM
           MOVE    MF100-KOHHBTNUM           TO   MF100-HBTNUM-KEY
      *
      *    収納情報より他法公費情報を取得
           MOVE     MF100-KOHNUM             TO   WRK-KOHNUM  
           MOVE     MF100-KOHID              TO   WRK-KOHID
           MOVE     ZERO                     TO   MF100-FTNMONEY 
                                                  MF100-TAHOFTN
                                                  KASAN-AREA
      *---(01.00.08) LINE ADD START ------------------------------------
      *     IF  MF100-RJNHKNNUM          NOT   =   SPACE
      *       MOVE    1                       TO   FLG-ERR2
      *     ELSE
      *       MOVE    ZERO                    TO   FLG-ERR2
      *    END-IF
           MOVE    1                         TO   FLG-ERR2
      *---(01.00.08) LINE ADD END   ------------------------------------
           IF      RECE02-NYUGAIKBN    =   "1"
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY33   TO  WRK-PATH
               MOVE    "key33"                 TO  WRK-PATH
      *---(01.00.06) LINE ADD END   ------------------------------------
           ELSE
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY25   TO  WRK-PATH
               MOVE    "key25"                 TO  WRK-PATH
      *---(01.00.06) LINE ADD END   ------------------------------------
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *    長期チェック
           PERFORM  900-SEIKYU-SELECT-SEC
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *    長期
      *---(01.00.07) LINE ADD START ------------------------------------
      *     IF   WRK-TYOKI                    >=   10000     AND
      *          FLG-TYOKI                     =   1
           IF   FLG-TYOKI                    =  1         AND
                MF100-TAHOFTN                >  10000
      *---(01.00.07) LINE ADD END   ------------------------------------
             MOVE   10000                    TO   MF100-TAHOFTN
             MOVE   1                        TO   MF100-TYOKI
           END-IF
      *
           IF   MF100-FTNMONEY               >=   10000     AND
                FLG-TYOKI                     =   1
             MOVE   10000                    TO   MF100-FTNMONEY
             MOVE   1                        TO   MF100-TYOKI
           END-IF
      *    長期上位
      *---(01.00.07) LINE ADD START ------------------------------------
      **---(01.00.04) LINE ADD START ------------------------------------
      **     IF   WRK-TYOKI                     >   20000     AND
      *     IF   WRK-TYOKI                    >=   20000     AND
      *---(01.00.04) LINE ADD END   ------------------------------------
      *          FLG-TYOKI                     =   2
           IF   FLG-TYOKI                    =  2         AND
                MF100-TAHOFTN                >  20000
      *---(01.00.07) LINE ADD END   ------------------------------------
             MOVE   20000                    TO   MF100-TAHOFTN
             MOVE   2                        TO   MF100-TYOKI
           END-IF
      *
      *---(01.00.04) LINE ADD START ------------------------------------
      *     IF   MF100-FTNMONEY                >   20000     AND
           IF   MF100-FTNMONEY               >=   20000     AND
      *---(01.00.04) LINE ADD END   ------------------------------------
                FLG-TYOKI                     =   2
             MOVE   20000                    TO   MF100-FTNMONEY
             MOVE   2                        TO   MF100-TYOKI
           END-IF
      *
      *    県老の場合診療行為情報より在宅診療があったかチェック
           IF     MF100-KOHNUM  =  "160" OR "260" OR "360" OR "460"
             PERFORM  20011-ZAITAKU-CHK-SEC
           END-IF
      *
      *    経過措置チェック
           INITIALIZE          FLG-KEIKA
           PERFORM  900-PTKOHINF-READ-SEC
           IF  FLG-KEIKA  =  1
             MOVE   1  TO  MF100-KEIKA
           END-IF
      *
      *=== open-cobol1.0対応 by Yoshikawa(2010/12/06) start
           IF  RECE02-NYUGAIKBN  =  "1"
             MOVE   1   TO   IDX2
           ELSE
             MOVE   2   TO   IDX2
           END-IF
      *=== open-cobol1.0対応 by Yoshikawa(2010/12/06) end
      *
      *    保険負担額
           IF  RECE02-NYUGAIKBN  =  1
           OR  RECE02-ZAITAKU    =  1
             IF  RECE02-FTNMONEY (1)  =  ZERO
      *=== open-cobol1.0対応 by Yoshikawa(2010/12/06) start
      *         COMPUTE  MF100-HKNGAKU(RECE02-NYUGAIKBN)  =
               COMPUTE  MF100-HKNGAKU(IDX2)  =
      *=== open-cobol1.0対応 by Yoshikawa(2010/12/06) end
                        RECE02-TOTALTEN (1)  *
                        RECE02-KYURATE  /  10
             ELSE
               MOVE  RECE02-FTNMONEY(1)  TO
      *=== open-cobol1.0対応 by Yoshikawa(2010/12/06) start
      *               MF100-HKNGAKU(RECE02-NYUGAIKBN)
                     MF100-HKNGAKU(IDX2)
      *=== open-cobol1.0対応 by Yoshikawa(2010/12/06) end
             END-IF
           END-IF
      *
      *---(01.00.08) LINE ADD START ------------------------------------
      *    フラグ（未就学児、特例措置）
           MOVE    RECE02-FLG (1)     TO  MF100-FLG (1)
           MOVE    RECE02-FLG (2)     TO  MF100-FLG (2)
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
      *    保険者番号チェック処理
           MOVE    ZERO               TO   FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR             =    ZERO
      *     食事の助成が発生する261,361,264,364を併用している
      *     入院データは印刷する
             IF  FLG-ERR2             =   ZERO
             OR  ((MF100-NYUGAIKBN =   "1" )   AND
                  (MF100-KOHNUM    =  "261" OR "361" OR "264" OR "364"))
      *---(01.00.09) LINE ADD START ------------------------------------
             OR  RECE02-KOHNUM (1)  =  "463" OR "465" OR "490"
             OR  RECE02-KOHNUM (2)  =  "463" OR "465" OR "490"
             OR  RECE02-KOHNUM (3)  =  "463" OR "465" OR "490"
             OR  RECE02-KOHNUM (4)  =  "463" OR "465" OR "490"
      *---(01.00.09) LINE ADD END   ------------------------------------
               MOVE     FLG-ERR2     TO  MF100-JOSEI
               WRITE    MF100-REC 
               ADD      1                 TO    CNT-MF100
             ELSE
               DISPLAY "助成対象外患者=" MF100-PTNUM
                       "診療年月=" MF100-SRYYM
                       "入外区分=" MF100-NYUGAIKBN
                       "点数=" MF100-TOTALTEN
                       "一部負担金=" MF100-FTNMONEY
                       "他法公費点数=" MF100-TAHOTEN
                       "他法公費負担金=" MF100-TAHOFTN
                       "公費ID=" WRK-KOHID
             END-IF
           ELSE
             DISPLAY  "個別発行対象外 =" MF100-PTNUM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅診療チェック処理
      *****************************************************************
       20011-ZAITAKU-CHK-SEC  SECTION.
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     4  OR
                    MF100-ZAITAKU =     1
             MOVE    SPACE           TO  WRK-SRYCD
             EVALUATE  IDX
             WHEN 1
               MOVE  "114701710"     TO  WRK-SRYCD
             WHEN 2
               MOVE  "114701210"     TO  WRK-SRYCD
             WHEN 3
               MOVE  "114007610"     TO  WRK-SRYCD
             WHEN 4
               MOVE  "114007710"     TO  WRK-SRYCD
             END-EVALUATE
             PERFORM  900-SRYACT-SELECT-SEC
             PERFORM  900-SRYACT-CLOSE-SEC
             IF     FLG-SRYACT       =   ZERO
               MOVE     1            TO  MF100-ZAITAKU
             END-IF
           END-PERFORM
      *
           .
       20011-ZAITAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "15"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-CITYNUM(1:3)       TO  WRK-HKNJANUM(3:3)
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
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
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
           INITIALIZE                             SC17
      *
      *    診療年月
           MOVE    MF100-SRYYM               TO  WRK-SYMD (1:6)
           MOVE    "01"                      TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(5:12)         TO  SC17-PRTYM 
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD        TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT          TO  SC17-HOSPCD
      *
      *    市町村名
           MOVE    "15"                   TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-CITYNUM          TO  WRK-HKNJANUM(3:3)
           PERFORM  920-HKNJAINF-SET-SEC
           MOVE    HKNJA-HKNJANAME        TO  SC17-CITYNAME
      *
      *---(01.00.06) LINE ADD START ------------------------------------
      *    医療機関住所
      *     MOVE     SYS-1002-ADRS            TO   SC17-ADRS
      *
      *    医療機関名
      *     MOVE     SYS-1001-HOSPNAME        TO   SC17-HOSPNAME
      *
      *    電話番号
      *     MOVE     SYS-1002-TEL             TO   SC17-TEL
      *    医療機関住所
           MOVE    WRK-HOSPADRS           TO  SC17-ADRS
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME           TO  SC17-HOSPNAME
      *
      *    電話番号
           MOVE    WRK-HOSPTEL            TO  SC17-TEL
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *    制度種別
           EVALUATE  MF100-HBTNUM-KEY
           WHEN "60"
             MOVE    "○"                  TO  SC17-KOHKBN(1)
           WHEN "61"
             MOVE    "○"                  TO  SC17-KOHKBN(2)
           WHEN "63"
             MOVE    "○"                  TO  SC17-KOHKBN(3)
           WHEN "64"
             MOVE    "○"                  TO  SC17-KOHKBN(4)
           WHEN "65"
             MOVE    "○"                  TO  SC17-KOHKBN(5)
           WHEN "90"
             MOVE    "○"                  TO  SC17-KOHKBN(6)
           END-EVALUATE
      *
      *    市町村番号
           MOVE     MF100-KOHFTNJANUM(5:4) TO  SC17-CITYNUM
      *
      *    受給者番号
           MOVE     MF100-KOHJKYSNUM       TO  SC17-JKYSNUM
      *
      *    氏名
           MOVE     MF100-NAME             TO  SC17-NAME
           MOVE     MF100-KANANAME         TO  SC17-KANANAME
      *
      *    性別
           IF       MF100-SEX              =   "1"
             MOVE   "○"                   TO  SC17-SEX(1)
           ELSE
             MOVE   "○"                   TO  SC17-SEX(2)
           END-IF
      *
      *    生年月日
           INITIALIZE                          WRK-SYMD
           MOVE    MF100-BIRTHDAY          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE  WRK-HENYMDG(1:4)
           WHEN "明治"
             MOVE   "○"                   TO  SC17-GENGO(1)
           WHEN "大正"
             MOVE   "○"                   TO  SC17-GENGO(2)
           WHEN "昭和"
             MOVE   "○"                   TO  SC17-GENGO(3)
           WHEN "平成"
             MOVE   "○"                   TO  SC17-GENGO(4)
           END-EVALUATE
           MOVE    WRK-HENYMDG(5:18)       TO  SC17-TANJOYMD
      *
      *    給付割合
      *---(01.00.08) LINE ADD START ------------------------------------
           IF   MF100-SRYYM   >=  WRK-CONS-SRYYM-200804
           AND  MF100-FLG (1)  =  1
               MOVE  "○"                      TO  SC17-KYURATE(2)
           ELSE
      *---(01.00.08) LINE ADD END   ------------------------------------
      *---(01.00.05) LINE ADD START ------------------------------------
      *     IF  MF100-AGE  <  70
             IF  MF100-AGE  <  70  AND  MF100-RJNHKNNUM  =  SPACE
      *---(01.00.05) LINE ADD END   ------------------------------------
               IF  MF100-AGE  >=  3
                 MOVE   "○"                   TO  SC17-KYURATE(1)
               ELSE
                 MOVE   "○"                   TO  SC17-KYURATE(2)
               END-IF
             ELSE
               IF  MF100-KYURATE  =  "010"
                 MOVE   "○"                   TO  SC17-KYURATE(3)
               ELSE
                 MOVE   "○"                   TO  SC17-KYURATE(4)
               END-IF
             END-IF
      *---(01.00.08) LINE ADD START ------------------------------------
           END-IF
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
      *    保険者番号
           IF    MF100-HKNNUM              =   "001"
             MOVE   MF100-HKNJANUM(1:4)    TO  SC17-HKNJANUM(5:4)
           ELSE
             MOVE   MF100-HKNJANUM         TO  SC17-HKNJANUM
           END-IF
      *
      *    老人保健の受給者番号
           MOVE     MF100-RJNJKYSNUM       TO  SC17-RJNJKYSNUM
      *
      *    入院時の自己負担限度額(県老・前期高齢者・老人のみ)
           IF       MF100-HBTNUM-KEY       =   "60"   OR
                    MF100-RJNHKNNUM   NOT  =   SPACE  OR
                    MF100-AGE             >=   70
             EVALUATE  MF100-KOHNUM
             WHEN "360"
             WHEN "361"
             WHEN "364"
             WHEN "561"
             WHEN "564"
               MOVE    "○"               TO   SC17-GENDO(1)
             WHEN "260"
             WHEN "261"
             WHEN "264"
               MOVE    "○"               TO   SC17-GENDO(2)
             END-EVALUATE
           END-IF
      *
      *---(01.00.04) LINE ADD START ------------------------------------
      *    在宅時の自己負担限度額
      *     IF    MF100-ZAITAKU             =   "1"
      *       EVALUATE  MF100-KOHNUM
      *       WHEN "460"
      *       WHEN "461"
      *       WHEN "464"
      *         MOVE    "○"               TO   SC17-GENDO(3)
      *       WHEN "160"
      *       WHEN "161"
      *       WHEN "164"
      *         MOVE    "○"               TO   SC17-GENDO(4)
      *       WHEN "260"
      *       WHEN "360"
      *       WHEN "261"
      *       WHEN "361"
      *       WHEN "264"
      *       WHEN "364"
      *       WHEN "561"
      *       WHEN "564"
      *         MOVE    "○"               TO   SC17-GENDO(5)
      *       END-EVALUATE
      *     END-IF
      *    高額療養費の場合○印字
           EVALUATE  MF100-ZAITAKU
             WHEN  2
               MOVE    "○"               TO   SC17-GENDO(3)
             WHEN  3
               MOVE    "○"               TO   SC17-GENDO(4)
             WHEN  4
               MOVE    "○"               TO   SC17-GENDO(5)
           END-EVALUATE
      *---(01.00.04) LINE ADD END   ------------------------------------
      *
      *    点数・日数・一部負担金・標準負担額
           IF     MF100-NYUGAIKBN          =   "1"
      *      入院点数
             MOVE  MF100-TOTALTEN          TO  WRK-Z7
             MOVE  WRK-Z7                  TO  SC17-TENSU(1)
      *
      *      保険負担額
             MOVE  MF100-HKNGAKU (1)       TO  WRK-Z9
             MOVE  WRK-Z9                  TO  SC17-HKNGAKU(1)
      *
      *    入院日数の記載場所は保険番号マスタの１日上限額で判定する
             MOVE    MF100-KOHNUM        TO  WRK-KOHNUM
             PERFORM 950-HKNNUM-SET-SEC
             IF      HKN-HON-NYUDJGNGAK      =   500
                 MOVE  MF100-JNISSU          TO  WRK-Z2
                 MOVE  WRK-Z2                TO  SC17-NISSU(2)
             ELSE
                 MOVE  MF100-JNISSU          TO  WRK-Z2
                 MOVE  WRK-Z2                TO  SC17-NISSU(1)
             END-IF
      *
      *      入院一部負担金
      *---(01.00.08) LINE ADD START ------------------------------------
      *       MOVE    MF100-FTNMONEY        TO  WRK-Z7
      *       MOVE    WRK-Z7                TO  SC17-ITBFTN(1)
             MOVE    MF100-FTNMONEY        TO  WRK-Z79
             MOVE    WRK-Z79               TO  SC17-ITBFTN(1)
      *---(01.00.08) LINE ADD END   ------------------------------------
      *
      *      点数に対する助成は無いが、食事の助成がある為印刷する時は
      *      一部負担金、入院日数は記載しない
             IF  MF100-JOSEI     =   1
                 MOVE    SPACE   TO  SC17-NISSU(1)
                                     SC17-NISSU(2)
                                     SC17-ITBFTN(1)
             END-IF
      *
      *      標準負担額(減額認定を受けている６１・６３・６４のみ)
             IF     MF100-KOHNUM  =  "261" OR "361" OR "263" 
                                  OR "264" OR "364" OR "561" OR "564"
               MOVE    ZERO                    TO  WRK-KEISAN
               DIVIDE  MF100-SHOKUJIFTN    BY      MF100-SHOKUJINISSU
                                       GIVING      WRK-KEISAN
               EVALUATE  TRUE
      *---(01.00.04) LINE ADD START ------------------------------------
      *         WHEN WRK-KEISAN  >=  650
      *           MOVE    1                     TO  IDX
      *         WHEN WRK-KEISAN  >=  500
      *           MOVE    2                     TO  IDX
      *         WHEN WRK-KEISAN  >=  300
      *           MOVE    3                     TO  IDX
      *         WHEN OTHER
      *           MOVE    ZERO                  TO  IDX
      *         END-EVALUATE
               WHEN WRK-KEISAN  >=  210
                 MOVE    1                     TO  IDX
               WHEN WRK-KEISAN  >=  160
                 MOVE    2                     TO  IDX
               WHEN WRK-KEISAN  >=  100
                 MOVE    3                     TO  IDX
               WHEN OTHER
                 MOVE    ZERO                  TO  IDX
               END-EVALUATE
      *---(01.00.04) LINE ADD END   ------------------------------------
               MOVE  MF100-SHOKUJINISSU        TO  WRK-Z2
               MOVE  MF100-SHOKUJIFTN          TO  WRK-Z6
               IF    IDX                        >  ZERO
                 MOVE  WRK-Z2                  TO  SC17-SNISSU(IDX)
                 MOVE  WRK-Z6                  TO  SC17-SFTNGK(IDX)
               END-IF
               MOVE    WRK-Z6                  TO  SC17-SFTNGK(5)
             END-IF
           ELSE
      *
      *      外来点数
             MOVE  MF100-TOTALTEN          TO  WRK-Z7
             MOVE  WRK-Z7                  TO  SC17-TENSU(2)
      *
      *      保険負担額
             MOVE  MF100-HKNGAKU (2)       TO  WRK-Z9
             MOVE  WRK-Z9                  TO  SC17-HKNGAKU(2)
      *
      *      外来一部負担金
      *---(01.00.08) LINE ADD START ------------------------------------
      *       MOVE    MF100-FTNMONEY        TO  WRK-Z7
      *       MOVE    WRK-Z7                TO  SC17-ITBFTN(2)
             MOVE    MF100-FTNMONEY        TO  WRK-Z79
             MOVE    WRK-Z79               TO  SC17-ITBFTN(2)
      *---(01.00.08) LINE ADD END   ------------------------------------
           END-IF
      *
      *    備考
           IF  MF100-TYOKI  =  1
             MOVE  "○"                    TO  SC17-BIKO (1)
           END-IF
           IF  MF100-TYOKI  =  2
             MOVE  "○"                    TO  SC17-BIKO (2)
           END-IF
           IF  MF100-KEIKA  =  1
             MOVE  "経"                    TO  SC17-BIKO (3)
           END-IF
      *
      *    他法法別番号
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >     9
                    OR       MF100-TAHO(IDX)  =  SPACE
             MOVE  MF100-TAHO (IDX)    TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
      *
             IF  SC17-TAHO  NOT =  SPACE
               STRING   SC17-TAHO        DELIMITED BY SPACE
                        ","              DELIMITED BY SIZE
                        INTO             SC17-TAHO
               END-STRING
             END-IF
             STRING   SC17-TAHO        DELIMITED BY SPACE
                      HKN-HBTNUM       DELIMITED BY SIZE
                      INTO             SC17-TAHO
             END-STRING
           END-PERFORM
      *
      *    公費分点数
           IF  MF100-TAHOTEN  >  MF100-TOTALTEN
             MOVE     MF100-TOTALTEN     TO  WRK-Z7
           ELSE
             MOVE     MF100-TAHOTEN      TO  WRK-Z7
           END-IF
           MOVE     WRK-Z7               TO  SC17-TAHOTEN
      *
      *    公費患者負担額
           MOVE     MF100-TAHOFTN        TO  WRK-Z7
           MOVE     WRK-Z7               TO  SC17-TAHOFTN
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
           MOVE   "SC1517.red"         TO  SPRT-PRTID
           MOVE    "県単医療費助成金請求内訳書"
                                    TO  SPRT-TITLE
           MOVE    SC17             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.06) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
           DISPLAY "*** SEIKYU1517 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1517 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU1517 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    WRK-PARA-HOSPID     TO  RECE02-HOSPID
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2       TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
           MOVE    "DBFETCH"                TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
             IF  RECE02-KOHNUM (1) = "160" OR "260" OR "360" OR "460"
                                  OR "161" OR "261" OR "361" OR "461"
                                  OR "163" OR "263" OR "165" OR "265"
                                  OR "164" OR "264" OR "364" OR "464"
                                  OR "190" OR "290" OR "564" OR "561" OR
                 RECE02-KOHNUM (2) = "160" OR "260" OR "360" OR "460"
                                  OR "161" OR "261" OR "361" OR "461"
                                  OR "163" OR "263" OR "165" OR "265"
                                  OR "164" OR "264" OR "364" OR "464"
                                  OR "190" OR "290" OR "564" OR "561" OR
                 RECE02-KOHNUM (3) = "160" OR "260" OR "360" OR "460"
                                  OR "161" OR "261" OR "361" OR "461"
                                  OR "163" OR "263" OR "165" OR "265"
                                  OR "164" OR "264" OR "364" OR "464"
                                  OR "190" OR "290" OR "564" OR "561" OR
                 RECE02-KOHNUM (4) = "160" OR "260" OR "360" OR "460"
                                  OR "161" OR "261" OR "361" OR "461"
                                  OR "163" OR "263" OR "165" OR "265"
                                  OR "164" OR "264" OR "364" OR "464"
                                  OR "190" OR "290" OR "564" OR "561"
      *
      *        テスト患者のときは読み飛ばす
                 MOVE    SPACE               TO  PTINF-REC
                 INITIALIZE                      PTINF-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *           MOVE    RECE02-HOSPID       TO  PTINF-HOSPID
                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2       TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY       TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報SELECT(抽出)
      *****************************************************************
       900-SRYACT-SELECT-SEC         SECTION.
      *
           INITIALIZE                      SRYACT-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    RECE02-HOSPID       TO  SRY-HOSPID
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    RECE02-NYUGAIKBN    TO  SRY-NYUGAIKBN
           MOVE    RECE02-PTID         TO  SRY-PTID
           MOVE    RECE02-SRYYM        TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(1)
                                           SRY-SRYCD(2)
                                           SRY-SRYCD(3)
                                           SRY-SRYCD(4)
                                           SRY-SRYCD(5)
           MOVE    SRYACT-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SRYACT-KEY7     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key7"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC               =   ZERO
                   MOVE  ZERO               TO FLG-SRYACT
                   PERFORM   900-SRYACT-FETCH-SEC
           ELSE
                   MOVE   1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報１件読み込み(抽出)
      *****************************************************************
       900-SRYACT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"                 TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SRYACT-KEY7      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key7"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
             MOVE    MCPDATA-REC     TO  SRYACT-REC
             MOVE    ZERO            TO  FLG-SRYACT
           ELSE
             MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報クローズ(抽出)
      *****************************************************************
       900-SRYACT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SRYACT-KEY7        TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key7"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報SELECT
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           INITIALIZE                             PTKOHINF-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    SYS-1001-HOSPID            TO  PTKOH-HOSPID
           MOVE    SPA-HOSPNUM                TO  PTKOH-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    RECE02-PTID                TO  PTKOH-PTID
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               PERFORM  900-PTKOHINF-FETCH-SEC
      *
               PERFORM  UNTIL  FLG-PTKOHINF  =  1
                 IF  PTKOH-KOHNUM  =  "978"  OR  "979"
                   IF   PTKOH-TEKSTYMD(1:6)  <=  RECE02-SRYYM
                   AND  PTKOH-TEKEDYMD(1:6)  >=  RECE02-SRYYM
                     MOVE  1     TO  FLG-KEIKA
                     MOVE  1     TO  FLG-PTKOHINF
                   END-IF
                 END-IF
                 PERFORM  900-PTKOHINF-FETCH-SEC
               END-PERFORM
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報FETCH
      *****************************************************************
       900-PTKOHINF-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTKOHINF-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTKOHINF
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       900-PTKOHINF-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    SYU-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE    SYU-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-HKNCOMBI-KEY     TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  SYS-1001-HOSPID             TO  HKNJA-HOSPID
           MOVE  SPA-HOSPNUM                 TO  HKNJA-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  WRK-HKNJANUM                TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNJAINF-KEY2      TO  MCP-PATH
      *     CALL  "MCPSUB"             USING
      *                                MCPAREA
      *                                MCPDATA-REC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.06) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-HKNJAINF-KEY2    TO  MCP-PATH
             MOVE  "tbl_hknjainf"     TO  MCP-TABLE
             MOVE  "key2"             TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNJAINF-KEY2   TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    国保でない時は、読み直す
             IF    HKNJA-HKNNUM  NOT  =  "060" AND "067"
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE  PATH-TBL-HKNJAINF-KEY2   TO  MCP-PATH
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           INITIALIZE  WRK-TAHOTEN
                       WRK-KEISAN2
                       WRK-KEISAN3
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
      *      長期チェック
             IF  SYU-KOH-HKNNUM(IDX)     =  "972"  OR  "974"
               IF  SYU-KOH-HKNNUM(IDX)   =  "972"
                 MOVE  1                   TO  FLG-TYOKI
               ELSE
                 MOVE  2                   TO  FLG-TYOKI
               END-IF
               ADD   SYU-KOH-COMPFTN(IDX)  TO  WRK-TYOKI
             END-IF
      *
      *      他法公費の計算負担金を四捨五入
             IF  (SYU-KOH-HKNNUM(IDX)(1:1) = "0"  AND
                  SYU-KOH-HKNNUM(IDX) NOT  = "027")
      *---(01.00.07) LINE ADD START ------------------------------------
      *         MOVE  SYU-KOH-COMPFTN(IDX)  TO  WRK-COMPFTN
               MOVE  SYU-KOH-HKNFTNMONEY(IDX)  TO  WRK-COMPFTN
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *        他法公費と点数
               PERFORM  VARYING  IDX1  FROM  1  BY  1
                        UNTIL    IDX1  >     9
                        OR       MF100-TAHO(IDX1)  =  SPACE
                 IF  MF100-TAHO(IDX1)  =  SYU-KOH-HKNNUM(IDX)
                   MOVE  9                   TO  IDX1
                 END-IF
               END-PERFORM
      *        法別番号取得
               IF  IDX1  <  9
                 MOVE  SYU-KOH-HKNNUM(IDX)   TO  MF100-TAHO(IDX1)
               END-IF
      *        公費分点数
               MOVE   SYU-TOTAL-HKNTEN       TO  WRK-TAHOTEN
      *
      *        通常の診療の場合と複数科診療で処理を分ける
               IF  SYU-GRP-DENPNUM  =  ZERO  OR
                   WRK-COMPFTN      =  530   OR
                   WRK-COMPFTN      =  0
                 MOVE  WRK-COMPFTN             TO  WRK-KEISAN2
      *---(01.00.07) LINE ADD START ------------------------------------
      *           DIVIDE   WRK-KEISAN2    BY  10
      *                    GIVING             WRK-KEISAN
      *                    REMAINDER          WRK-AMARI
      *           COMPUTE  WRK-KEISAN2     =  WRK-KEISAN2
      *                                    -  WRK-AMARI
      *           IF  WRK-AMARI     >=     5
      *             ADD    10             TO  WRK-KEISAN2
      *           END-IF
      *---(01.00.07) LINE ADD END   ------------------------------------
                 MOVE      WRK-KEISAN2       TO  WRK-KEISAN3
      *
      *        複数科診療の場合は伝票番号毎にまとめて合算する
               ELSE
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                          UNTIL    IDX1  >     4
                   IF  SYU-GRP-DENPNUM      =  KASAN-DENPNUM(IDX1)
                   OR  KASAN-DENPNUM (IDX1)  =  ZERO
                     MOVE  SYU-GRP-DENPNUM      TO  KASAN-DENPNUM(IDX1)
                     MOVE    1                  TO  KASAN-TAHO(IDX1)
                     ADD   WRK-COMPFTN          TO  KASAN-TAHOFTN(IDX1)
                     MOVE    4                  TO  IDX1
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
      *
      *     地方公費の計算負担金を四捨五入
             IF  SYU-KOH-HKNNUM(IDX)    =  MF100-KOHNUM
      *
      *      通常の診療
               IF  SYU-GRP-DENPNUM        =  ZERO  OR
                   SYU-KOH-COMPFTN(IDX)   =  530   OR
                   SYU-KOH-COMPFTN(IDX)   =  0
      *---(01.00.07) LINE ADD START ------------------------------------
      *           MOVE  SYU-KOH-COMPFTN(IDX)    TO  WRK-KEISAN2
      *           DIVIDE   WRK-KEISAN2    BY  10
      *                    GIVING             WRK-KEISAN
      *                    REMAINDER          WRK-AMARI
      *           COMPUTE  WRK-KEISAN2     =  WRK-KEISAN2
      *                                    -  WRK-AMARI
      *           IF  WRK-AMARI     >=     5
      *             ADD    10             TO  WRK-KEISAN2
      *          END-IF
      *           ADD     WRK-KEISAN2     TO  MF100-FTNMONEY
      *---(01.00.08) LINE ADD START ------------------------------------
      *           MOVE  SYU-KOH-HKNFTNMONEY(IDX)    TO  WRK-KEISAN2
                 IF  SYU-KOH-HKNFTNMONEY(IDX)  =  ZERO
                   COMPUTE  WRK-KEISAN2 =
                           (SYU-KOH-COMPFTN(IDX) + 5)  /  10
                   COMPUTE  WRK-KEISAN2  =  WRK-KEISAN2  *  10
                 ELSE
                   MOVE  SYU-KOH-HKNFTNMONEY(IDX)   TO  WRK-KEISAN2
                 END-IF
      *---(01.00.08) LINE ADD END   ------------------------------------
                 ADD     WRK-KEISAN2     TO  MF100-FTNMONEY
      *---(01.00.07) LINE ADD END   ------------------------------------
      *
      *      複数科診療
               ELSE
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                          UNTIL    IDX1  >     4
                   IF  SYU-GRP-DENPNUM      =  KASAN-DENPNUM(IDX1)
                   OR  KASAN-DENPNUM (IDX1)  =  ZERO
                     MOVE  SYU-GRP-DENPNUM    TO KASAN-DENPNUM(IDX1)
      *=== 複数科診療時の四捨五入修正 by Yoshikawa(2010/02/08) start
      *               ADD   SYU-KOH-COMPFTN(IDX)
      *                                        TO KASAN-COMPFTN(IDX1)
                     IF  SYU-KOH-HKNFTNMONEY(IDX)  =  ZERO
                       COMPUTE  WRK-KEISAN2  =
                               (SYU-KOH-COMPFTN(IDX) + 5)  /  10
                       COMPUTE  WRK-KEISAN2  =  WRK-KEISAN2  *  10
                     ELSE
                       ADD  SYU-KOH-HKNFTNMONEY(IDX)  TO  WRK-KEISAN2
                     END-IF
                     ADD  WRK-KEISAN2         TO  KASAN-COMPFTN(IDX1)
      *=== 複数科診療時の四捨五入修正 by Yoshikawa(2010/02/08) end
                     MOVE  4                     TO  IDX1
                   END-IF
                 END-PERFORM
               END-IF
      *
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
               IF  FLG-ERR2    =     1
                 IF     IDX                 =  1
                   IF  SYU-SYUCOMPFTN-ENTANI
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR2
                   END-IF
                 ELSE
                   IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE     ZERO         TO  FLG-ERR2
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           ADD  WRK-TAHOTEN          TO  MF100-TAHOTEN
           ADD  WRK-KEISAN3          TO  MF100-TAHOFTN
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理２
      *****************************************************************
       8001-SEIKYU-SEC  SECTION.
      *
      *    複数科診療の分を四捨五入して加算
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1                 >     4  
                    OR       KASAN-DENPNUM(IDX1)  =  ZERO
             MOVE  KASAN-COMPFTN(IDX1)    TO  WRK-KEISAN2
      *---(01.00.07) LINE ADD START ------------------------------------
      *       DIVIDE   WRK-KEISAN2    BY  10
      *                GIVING             WRK-KEISAN
      *                REMAINDER          WRK-AMARI
      *       COMPUTE  WRK-KEISAN2     =  WRK-KEISAN2
      *                                -  WRK-AMARI
      *       IF  WRK-AMARI     >=     5
      *         ADD    10             TO  WRK-KEISAN2
      *       END-IF
      *---(01.00.07) LINE ADD END   ------------------------------------
             ADD     WRK-KEISAN2     TO  MF100-FTNMONEY
      *
             IF  KASAN-TAHO(IDX1)  =  1
               MOVE  KASAN-TAHOFTN(IDX1)    TO  WRK-KEISAN2
      *---(01.00.07) LINE ADD START ------------------------------------
      *         DIVIDE   WRK-KEISAN2    BY  10
      *                  GIVING             WRK-KEISAN
      *                  REMAINDER          WRK-AMARI
      *         COMPUTE  WRK-KEISAN2     =  WRK-KEISAN2
      *                                  -  WRK-AMARI
      *         IF  WRK-AMARI     >=     5
      *           ADD    10             TO  WRK-KEISAN2
      *         END-IF
      *---(01.00.07) LINE ADD END   ------------------------------------
               ADD     WRK-KEISAN2     TO  MF100-TAHOFTN
             END-IF
           END-PERFORM
      *
           .
       8001-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *       MOVE  RECE02-HOSPID      TO  SYU-HOSPID
             MOVE    SPA-HOSPNUM      TO  SYU-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *       MOVE  WRK-PATH           TO  MCP-PATH
      *       CALL  "MCPSUB"           USING
      *                                MCPAREA
      *                                MCPDATA-REC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
             IF  MCP-RC  =  ZERO
      *---(01.00.06) LINE ADD START ------------------------------------
      *         MOVE  WRK-PATH         TO  MCP-PATH
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *           MOVE  WRK-PATH             TO  MCP-PATH
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
      *      複数科診療分を加算する
             PERFORM  8001-SEIKYU-SEC
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *       MOVE  WRK-PATH             TO  MCP-PATH
      *       CALL  "MCPSUB"     USING
      *                          MCPAREA
      *                          MCPDATA-REC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  WRK-PATH             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  SYS-1001-HOSPID      TO  HKN-HOSPID
           MOVE  SPA-HOSPNUM          TO  HKN-HOSPNUM
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE  MF100-SRYYM          TO  HKN-TEKSTYMD(1:6)
                                          HKN-TEKEDYMD(1:6)
           MOVE  "99"                 TO  HKN-TEKSTYMD(7:2)
           MOVE  "00"                 TO  HKN-TEKEDYMD(7:2)
           MOVE  HKNNUM-REC               TO  MCPDATA-REC
           MOVE  "DBSELECT"               TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNNUM-KEY5     TO  MCP-PATH
      *     CALL  "MCPSUB"             USING
      *                                MCPAREA
      *                                MCPDATA-REC
           MOVE  "tbl_hknnum"       TO  MCP-TABLE
           MOVE  "key5"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.06) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-HKNNUM-KEY5      TO  MCP-PATH
             MOVE  "tbl_hknnum"       TO  MCP-TABLE
             MOVE  "key5"             TO  MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
             PERFORM  955-HKNNUM-READ-SEC
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"             TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNNUM-KEY5        TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE  "tbl_hknnum"       TO  MCP-TABLE
           MOVE  "key5"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      *---(01.00.07) LINE ADD START ------------------------------------
      *****************************************************************
      *   長期チェック−請求管理情報
      *****************************************************************
       900-SEIKYU-SELECT-SEC  SECTION.
      *
           INITIALIZE  RECE10-REC
           MOVE  RECE02-HOSPNUM       TO  RECE10-HOSPNUM
           MOVE  RECE02-SRYYM         TO  RECE10-SRYYM
           MOVE  RECE02-NYUGAIKBN     TO  RECE10-NYUGAIKBN
           MOVE  RECE02-PTID          TO  RECE10-PTID
           MOVE  RECE02-RECEKA        TO  RECE10-RECEKA
           MOVE  RECE02-TEISYUTUSAKI  TO  RECE10-TEISYUTUSAKI
           MOVE  RECE02-RECESYUBETU   TO  RECE10-RECESYUBETU
           MOVE  RECE02-HKNJANUM      TO  RECE10-HKNJANUM
           MOVE  RECE02-HOJOKBN-KEY   TO  RECE10-HOJOKBN-KEY
           MOVE  RECE02-TEKSTYMD      TO  RECE10-TEKSTYMD
           MOVE  RECE10-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE  "tbl_seikyu"         TO  MCP-TABLE
           MOVE  "key"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  900-SEIKYU-READ-SEC
      *      長期チェック
             IF  RECE10-TOKKI (1)  =  "02"
             OR  RECE10-TOKKI (2)  =  "02"
             OR  RECE10-TOKKI (3)  =  "02"
             OR  RECE10-TOKKI (4)  =  "02"
             OR  RECE10-TOKKI (5)  =  "02"
                 MOVE  1  TO  FLG-TYOKI
                 MOVE  1  TO  MF100-TYOKI
             END-IF
             IF  RECE10-TOKKI (1)  =  "16"
             OR  RECE10-TOKKI (2)  =  "16"
             OR  RECE10-TOKKI (3)  =  "16"
             OR  RECE10-TOKKI (4)  =  "16"
             OR  RECE10-TOKKI (5)  =  "16"
                 MOVE  2  TO  FLG-TYOKI
                 MOVE  2  TO  MF100-TYOKI
             END-IF
           ELSE
             DISPLAY  "SEIKYU SELECT ERR"
           END-IF
      *
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE  "tbl_seikyu"       TO  MCP-TABLE
           MOVE  "key"              TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-READ-SEC  SECTION.
      *
           MOVE  "tbl_seikyu"   TO  MCP-TABLE
           MOVE  "key"          TO  MCP-PATHNAME
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  RECE10-REC
             MOVE  ZERO         TO  FLG-RECE10
           ELSE
             INITIALIZE             RECE10-REC
             MOVE  1            TO  FLG-RECE10
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *---(01.00.07) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-SYSKANRI-KEY10   TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
      *---(01.00.06) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      *       CALL  "MCPSUB"        USING
      *                             MCPAREA
      *                             MCPDATA-REC
               MOVE  "tbl_syskanri"        TO  MCP-TABLE
               MOVE  "key10"               TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      *     CALL  "MCPSUB"        USING
      *                           MCPAREA
      *                           MCPDATA-REC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key10"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                 MCPAREA
      *                                 MCPDATA-REC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
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
      *---(01.00.06) LINE ADD START ------------------------------------
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
      *---(01.00.06) LINE ADD END   ------------------------------------
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  "DBOPEN"   TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  "DBSTART"  TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
      *
      *---(01.00.06) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.06) LINE ADD END   ------------------------------------
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
      *---(01.00.06) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.06) LINE ADD END   ------------------------------------
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
