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
       PROGRAM-ID.     SEIKYU1209.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 千葉・子ども医療費請求書
      *                      (183,283,383,483,583,683)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/26    山口       ORCAのバージョンアップ(2.5.0)による再作成
      *                         (個別発行対応、月途中公費切替え対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01     内田    06/10/11    市町村名の取得の修正
      *  01.00.02     内田    07/04/24    長期上位（974）対応
      *  01.00.03     内田    07/08/29    日レセver4.0対応
      *                                   システム管理マスタ「1901」の
      *                                   医療機関情報を参照
      *  01.00.04     内田    08/06/10    ４月改正対応
      *                                   SPA-AREA初期化
      *  01.00.05     吉川    08/12/02    印字対象番号の追加(583,683)
      *  01.00.06     吉川    09/02/18    公費の助成がない場合は印字しない
      *  01.00.07     吉川    10/12/03    帳票様式の変更に伴う修正
      *  01.00.08     吉川    11/05/11    実日数が0の場合は0を印字する
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
      *    千葉保険番号データ
           SELECT  TOWN-FILE     ASSIGN FILE-NAME
                                 ORGANIZATION   IS  LINE SEQUENTIAL
                                 ACCESS MODE    IS  SEQUENTIAL
                                 FILE   STATUS  IS  STS-TOWN.
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
           COPY  "SEI1207.INC".
      *
      *    千葉保険番号データ
       FD  TOWN-FILE.
           01  TOWN-REC.
               03  TOWN-DAT-NUM      PIC 9(02).
               03  TOWN-DAT-KANMA    PIC X(01).
               03  TOWN-DAT-CITYNUM  PIC X(05).
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    千葉保険番号データ
           COPY  "TOWN1205.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
      *---(01.00.03) LINE ADD START ------------------------------------
      *     COPY  "CPCOMMONDAT2.INC"
      *             REPLACING  //RECE01PARA//
      *             BY         //RECEERR//.
      *     03      PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           COPY  "SC1209.INC".
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
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-HKNJAINF  PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-PTNYUINRRK PIC 9(01).
           03  FLG-TOWN       PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-HKNNUM     PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
      *
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *---(01.00.03) LINE ADD END   ------------------------------------
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR                PIC  X(200).
           03  WRK-Z3                     PIC ZZZ.
      *=== 実日数が0の場合は0を印字 by Yoshikawa(2011/05/11) start
           03  WRK-Z3-9                   PIC ZZ9.
      *=== 実日数が0の場合は0を印字 by Yoshikawa(2011/05/11) end
           03  WRK-Z7                     PIC ZZZ,ZZZ.
           03  WRK-Z79                    PIC ZZZ,ZZ9.
           03  WRK-Z10                    PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11                    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-SRYYMWH                PIC X(16).
           03  WRK-SEIYMDWH               PIC X(22).
           03  WRK-SYSYMDWH               PIC X(09).
           03  WRK-HENYMDG                PIC X(22).
           03  WRK-HENYMDG2               PIC X(20).
           03  WRK-SYMD.
               05  WRK-SYY                PIC 9(04).
               05  WRK-SMM                PIC 9(02).
               05  WRK-SDD                PIC 9(02).
           03  WRK-HENYMDG1.
               05  WRK-DAY2-EDTYMD1-G     PIC X(01).
               05  WRK-DAY2-EDTYMD1-Y     PIC Z9.
               05  WRK-DAY2-EDTYMD1-P1    PIC X(01).
               05  WRK-DAY2-EDTYMD1-M     PIC Z9.
               05  WRK-DAY2-EDTYMD1-P2    PIC X(01).
               05  WRK-DAY2-EDTYMD1-D     PIC Z9.
               05  WRK-SDD                PIC 9(02).
           03  WRK-HENKAN-G.
               05  WRK-HENKAN             PIC 9(02).
           03  WRK-HENKAN-G2.
               05  WRK-HENKAN1            PIC 9(02).
               05  WRK-HENKAN2            PIC 9(02).
           03  WRK-LASTYMD                PIC 9(08).
           03  WRK-KASAN-TBL.
               05   WRK-KOHNUM            PIC X(03).
               05   WRK-SEIKYUGK-G        PIC 9(08).
               05   WRK-SEIKYUGK          PIC 9(08).
           03  WRK-KOHID                  PIC 9(10).
           03  WRK-HKNJANUM               PIC X(05).
      *     文字変換
           03  WRK-MAE-INPUT              PIC X(200).
           03  WRK-OUT-INPUT              PIC X(200).
      *
      *    パラメータファイル名
           03  FILE-NAME                  PIC X(512).
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     03  WRK-PATH.
      *         05  FILLER            PIC S9(09)  BINARY.
      *         05  FILLER            PIC S9(09)  BINARY.
      *         05  FILLER            PIC S9(09)  BINARY.
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
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
       01  SUM-AREA.
           03  SUM-SEIKYU-TBL.
               05  SUM-GKENSU      PIC 9(03).
               05  SUM-GNISSU      PIC 9(10).
               05  SUM-GTENSU      PIC 9(10).
               05  SUM-GSEIKYUGAKU PIC 9(10).
               05  SUM-GFTNMONEY   PIC 9(10).
               05  SUM-GSHOKUJIFTN PIC 9(10).
               05  SUM-GSHOKUJIRYOYOHI PIC 9(10).
      *
       01  DATA-FILE.
           03  FILLER       PIC X(05)  VALUE  "data/".
           03  FILLER       PIC X(12)  VALUE  "TOWN1205.TXT".
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
      *    KEYエリア
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-NEW-SRYYM         PIC X(06).
               05  KEY-NEW-CITYNUM       PIC X(02).
               05  KEY-NEW-TOKUKYURATE   PIC 9(03).
           03  KEY-OLD.
               05  KEY-OLD-SRYYM         PIC X(06).
               05  KEY-OLD-CITYNUM       PIC X(02).
               05  KEY-OLD-TOKUKYURATE   PIC 9(03).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
      *---(01.00.03) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.04) LINE ADD START ------------------------------------
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *---(01.00.04) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    入院履歴マスタ
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
      *
      *    半角チェックサブ
           COPY  "CPORCSKANACHK.INC".
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
           INITIALIZE  SUM-AREA
           INITIALIZE  KEY-AREA
      *---(01.00.04) LINE ADD START ------------------------------------
           INITIALIZE  SPA-AREA
      *---(01.00.04) LINE ADD END   ------------------------------------
           INITIALIZE  TOWN-DAT
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
                                               WRK-PARA-SYORIFLG
      *---(01.00.03) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.03) LINE ADD END   ------------------------------------
           END-UNSTRING
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU1209"    TO  JOB-PGID
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) start
      *     MOVE    "乳幼児医療費請求書" 
      *                             TO  JOB-SHELLMSG
           MOVE    "子ども医療費請求書" 
                                   TO  JOB-SHELLMSG
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) end
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-SYSYMDWH
           END-IF
      *
      *
           MOVE  "MF101ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)  TO  MF100PARA-TERMID
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE  WRK-PARA-HOSPNUM           TO  MF100PARA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  "RECEERR"        TO  RECEERR-FILE-ID
      *     MOVE  LNK-PRTKANRI-TERMID(1:16)
      *                            TO  RECEERR-TERMID
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1209 SYS 1001 ERR ***" 
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
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1209 SYS 1002 ERR ***" 
             MOVE  1            TO  FLG-END
           END-IF
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *    市町村情報
           PERFORM 110-TOWNINF-GET-SEC
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
      *    公費保険番号データ読込
      *****************************************************************
       110-TOWNINF-GET-SEC  SECTION.
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE           ORCSMKPASSAREA
           MOVE  DATA-FILE     TO  MKPASS-IN-01
           CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
           MOVE  MKPASS-OT-01  TO  FILE-NAME
      *
           DISPLAY "FILE-NAME=" FILE-NAME
           OPEN  INPUT  TOWN-FILE
      *
           MOVE  ZERO   TO  FLG-TOWN
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL FLG-TOWN = 1
             READ TOWN-FILE NEXT
               AT END
                 MOVE  1         TO  FLG-TOWN
               NOT AT END
                 MOVE  TOWN-REC  TO  TOWN-TBL(IDX)
                 DISPLAY  "CITYNUM  "  TOWN-CITYNUM(IDX)
             END-READ
           END-PERFORM
      *
           CLOSE TOWN-FILE
           .
       110-TOWNINF-GET-EXT.
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
      *---(01.00.03) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *---(01.00.03) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
      *---(01.00.03) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
             IF  RECE02-KOHNUM (IDZ) =   "183" OR "283"OR "383"OR "483"
      *=== 印字対象番号追加 by Yoshikawa(2008/12/02) start
                                      OR "583" OR "683"
      *=== 印字対象番号追加 by Yoshikawa(2008/12/02) end
                 PERFORM  20011-SYUKEI-SEC
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
      *    集計処理
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF100-REC
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  RECE02-HOSPID       TO  MF100-HOSPID
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-ZAITAKU      TO  MF100-ZAITAKU
      *---(01.00.04) LINE ADD START ------------------------------------
           MOVE  RECE02-RECESYUBETU  TO  MF100-RECESYUBETU
      *---(01.00.04) LINE ADD END   ------------------------------------
      *
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF100-FTNJANUM
           MOVE  RECE02-KOHFTNJANUM (IDZ)(6:2)
                                           TO  MF100-CITYNUM1
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF100-JKYSNUM
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSUINF
           MOVE  RECE02-TOTALTENINF (IDY)  TO  MF100-TOTALTENINF
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
           MOVE  RECE02-YKZFTNINF   (IDY)  TO  MF100-YKZFTNINF
           ADD    RECE02-FTNMONEY   (IDY)  TO  MF100-FTNMONEY
      *
      *    請求金額集計集計
           MOVE    MF100-KOHNUM    TO    WRK-KOHNUM
           MOVE    MF100-KOHID     TO    WRK-KOHID
           MOVE    ZERO            TO    WRK-SEIKYUGK-G
                                         FLG-TYOKI
      *=== 公費の助成なしは印字対象外 by Yoshikawa(2009/02/18) start
           MOVE    1               TO    FLG-ERR
      *=== 公費の助成なしは印字対象外 by Yoshikawa(2009/02/18) end
           IF      RECE02-NYUGAIKBN    =   "1"
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY33   TO  WRK-PATH
               MOVE    "key33"             TO  WRK-PATH
      *---(01.00.03) LINE ADD END   ------------------------------------
           ELSE
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY25   TO  WRK-PATH
               MOVE    "key25"             TO  WRK-PATH
      *---(01.00.03) LINE ADD END   ------------------------------------
           END-IF
           PERFORM  920-SYUNOU-SET-SEC
      *
      *    長期
           IF    FLG-TYOKI    =    1  AND
                 WRK-SEIKYUGK-G    >  10000
               MOVE    10000          TO    WRK-SEIKYUGK-G
           END-IF
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *    長期上位
           IF    FLG-TYOKI    =    2  AND
                 WRK-SEIKYUGK-G    >  20000
               MOVE    20000          TO    WRK-SEIKYUGK-G
           END-IF
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
           COMPUTE  MF100-SEIKYUGK  = WRK-SEIKYUGK-G
                                    - MF100-FTNMONEY
      *
      *---(01.00.04) LINE ADD START ------------------------------------
      *     IF    MF100-RJNHKNNUM      = SPACE   AND
      *           MF100-HKNNUM         = "060"   AND
      *           MF100-HKNJANUM(3:1)  = "3"     AND
      *           MF100-AGE            < 70      AND
      *           MF100-AGE           >=  3      AND
      *           MF100-KYURATE    NOT =  30
      *       MOVE    MF100-KYURATE   TO  MF100-TOKUKYURATE
      *     END-IF
           IF  RECE02-SRYYM  >=  WRK-CONS-SRYYM-200804
             IF    MF100-RJNHKNNUM      = SPACE   AND
                   MF100-HKNNUM         = "060"   AND
                   MF100-HKNJANUM(3:1)  = "3"     AND
                   MF100-AGE            < 70      AND
                   RECE02-FLG (1)   NOT =  1      AND
                   MF100-KYURATE    NOT =  30
               MOVE    MF100-KYURATE   TO  MF100-TOKUKYURATE
             END-IF
           ELSE
             IF    MF100-RJNHKNNUM      = SPACE   AND
                   MF100-HKNNUM         = "060"   AND
                   MF100-HKNJANUM(3:1)  = "3"     AND
                   MF100-AGE            < 70      AND
                   MF100-AGE           >=  3      AND
                   MF100-KYURATE    NOT =  30
               MOVE    MF100-KYURATE   TO  MF100-TOKUKYURATE
             END-IF
           END-IF
      *---(01.00.04) LINE ADD END   ------------------------------------
      *
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) start
           EVALUATE  RECE02-ZAITAKU
             WHEN  "2"
               MOVE  "17"              TO  MF100-TOKKI
             WHEN  "3"
               MOVE  "18"              TO  MF100-TOKKI
             WHEN  "4"
               MOVE  "19"              TO  MF100-TOKKI
           END-EVALUATE
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) end
      *
      *    保険者番号チェック処理
      *=== 公費の助成なしは印字対象外 by Yoshikawa(2009/02/18) start
      *     MOVE    ZERO               TO   FLG-ERR
      *=== 公費の助成なしは印字対象外 by Yoshikawa(2009/02/18) end
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR    =    ZERO
               WRITE    MF100-REC 
               ADD      1              TO    CNT-MF100
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                     "公費番号="            MF100-KOHNUM
                     "診療年月="            MF100-SRYYM
           END-IF
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
      *
           PERFORM  VARYING  IDX    FROM    1    BY    1
                    UNTIL    IDX    >    100
             IF  MF100-CITYNUM1  =  TOWN-NUM(IDX)
                 MOVE    TOWN-CITYNUM(IDX)   TO  WRK-HKNJANUM
                                                 MF100-CITYNUM2
                 DISPLAY  "MF100-CITYNUM1 = "  MF100-CITYNUM1
                 DISPLAY  "TOWN-CITYNUM = " TOWN-CITYNUM(IDX)
                 MOVE    100                            TO  IDX
             END-IF
           END-PERFORM
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
               MOVE  KEY-NEW   TO  KEY-OLD
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票編集<合計>処理
               PERFORM  3201-GOKEI-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
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
      *
           ADD   1     TO  CNT-PAGE
      *
           INITIALIZE  SC09
      *
      *    印刷日付
           MOVE    MF100-SRYYM          TO  WRK-SYMD (1:6)
           MOVE    "01"                 TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  LNK-DAY2-EDTYMD1(2:2)  TO  SC09-PRTYY
           MOVE  LNK-DAY2-EDTYMD1(5:2)  TO  SC09-PRTMM
      *
      *    請求日付
           MOVE  WRK-SYSYMDWH(2:2)      TO  SC09-SEIYY
           MOVE  WRK-SYSYMDWH(5:2)      TO  SC09-SEIMM
           MOVE  WRK-SYSYMDWH(8:2)      TO  SC09-SEIDD
      *
      *    医療機関コード
            MOVE     SYS-1001-HOSPCD     TO  WRK-MAE-INPUT
            PERFORM  2009-HENKAN-SEC
            MOVE     WRK-OUT-INPUT       TO  SC09-HOSPCD
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *    住所
      *     MOVE  SYS-1002-ADRS          TO  SC09-ADRS
      *
      *    医療機関名
      *     MOVE  SYS-1001-HOSPNAME      TO  SC09-HOSPNAME
      *
      *    医療機関電話番号
      *     MOVE  SYS-1002-TEL           TO  SC09-TEL
      *    住所
           MOVE  WRK-HOSPADRS           TO  SC09-ADRS
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME           TO  SC09-HOSPNAME
      *
      *    医療機関電話番号
           MOVE  WRK-HOSPTEL            TO  SC09-TEL
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *    管理者名
           MOVE  SYS-1001-KAISETUNAME   TO  SC09-KAISETUNAME
      *    
      *    市町村名称取得
           PERFORM  900-HKNJAINF-INV-SEC
           MOVE     HKNJA-HKNJANAME     TO  SC09-CITYNAME
      *
      *    点数票区分
           EVALUATE  SYS-1001-TENHYOKBN
           WHEN  1
               MOVE  "○"               TO  SC09-TENSUKBN(1)
           WHEN  3
               MOVE  "○"               TO  SC09-TENSUKBN(2)
           WHEN  4
               MOVE  "○"               TO  SC09-TENSUKBN(3)
           WHEN  6
               MOVE  "○"               TO  SC09-TENSUKBN(4)
           END-EVALUATE
      *
      *    特給区分
           IF    MF100-TOKUKYURATE  NOT  =  ZERO
             EVALUATE  MF100-TOKUKYURATE
             WHEN 10
               MOVE    "9割"            TO  SC09-TOKKYUKBN
             WHEN 20
               MOVE    "8割"            TO  SC09-TOKKYUKBN
             END-EVALUATE
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
           INITIALIZE     SUM-AREA
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  FLG-PRINT  =  1
                    OR  IDX1        >  10
                    OR  KEY-NEW NOT =  KEY-OLD
      *    入外区分
      *---(01.00.04) LINE ADD START ------------------------------------
      *       IF  MF100-HONKZKKBN  =  "2"
      *           IF  MF100-AGE     <    3
      *               IF    MF100-NYUGAIKBN   =   "1"
      *                 MOVE  "3"        TO  SC09-NYUGAIKBN1(IDX1)
      *                 MOVE  "三入"     TO  SC09-NYUGAIKBN2(IDX1)
      *               ELSE
      *                 MOVE  "4"        TO  SC09-NYUGAIKBN1(IDX1)
      *                 MOVE  "三外"     TO  SC09-NYUGAIKBN2(IDX1)
      *               END-IF
      *           ELSE
      *               IF    MF100-NYUGAIKBN   =   "1"
      *                 MOVE  "5"        TO  SC09-NYUGAIKBN1(IDX1)
      *                 MOVE  "家入"     TO  SC09-NYUGAIKBN2(IDX1)
      *               ELSE
      *                 MOVE  "6"        TO  SC09-NYUGAIKBN1(IDX1)
      *                 MOVE  "家外"     TO  SC09-NYUGAIKBN2(IDX1)
      *               END-IF
      *           END-IF
      *       END-IF
             EVALUATE  MF100-RECESYUBETU(4:1)
                 WHEN  "3"
                     MOVE  "3"        TO  SC09-NYUGAIKBN1(IDX1)
                     MOVE  "六入"     TO  SC09-NYUGAIKBN2(IDX1)
                 WHEN  "4"
                     MOVE  "4"        TO  SC09-NYUGAIKBN1(IDX1)
                     MOVE  "六外"     TO  SC09-NYUGAIKBN2(IDX1)
                 WHEN  "5"
                     MOVE  "5"        TO  SC09-NYUGAIKBN1(IDX1)
                     MOVE  "家入"     TO  SC09-NYUGAIKBN2(IDX1)
                 WHEN  "6"
                     MOVE  "6"        TO  SC09-NYUGAIKBN1(IDX1)
                     MOVE  "家外"     TO  SC09-NYUGAIKBN2(IDX1)
             END-EVALUATE
      *---(01.00.04) LINE ADD END   ------------------------------------
      *
      *    負担者番号
             MOVE     MF100-FTNJANUM  TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT   TO  SC09-FTNJANUM (IDX1) 
      *
      *    受給資格証記号番号
             MOVE     MF100-JKYSNUM   TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT   TO  SC09-JKYSNUM (IDX1)
      *
      *    保険者番号
             MOVE  MF100-HKNJANUM     TO  SC09-HKNJANUM (IDX1)
      *
      *    生年月
             MOVE  MF100-BIRTHDAY     TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             INSPECT WRK-HENYMDG2    REPLACING  ALL "　"  BY  "０"
             MOVE    WRK-HENYMDG2(5:4)   TO  SC09-TANJOGYM(IDX1)(1:4)
             MOVE    WRK-HENYMDG2(11:4)  TO  SC09-TANJOGYM(IDX1)(5:4)
      *
      *    氏名
             MOVE  MF100-NAME         TO  SC09-NAME (IDX1)
      *
      *    性別
             MOVE    MF100-SEX        TO  SC09-SEX(IDX1)
      *
      *    診療日数
      *=== 実日数が0の場合は0を印字 by Yoshikawa(2011/05/11) start
      *       MOVE  MF100-JNISSU       TO  WRK-Z3
      *       MOVE  WRK-Z3             TO  SC09-SRYNISSU(IDX1)
             MOVE  MF100-JNISSU       TO  WRK-Z3-9
             MOVE  WRK-Z3-9           TO  SC09-SRYNISSU(IDX1)
      *=== 実日数が0の場合は0を印字 by Yoshikawa(2011/05/11) end
      *
      *    請求点数
             MOVE  MF100-TOTALTEN     TO  WRK-Z11
             MOVE  WRK-Z11            TO  SC09-SEIKYUTEN(IDX1)
      *
      *    負担金額
             MOVE    MF100-FTNMONEY    TO   WRK-Z7
             MOVE    WRK-Z7            TO   SC09-FTNMONEY(IDX1)
      *
      *    請求額
             MOVE  MF100-SEIKYUGK     TO  WRK-Z10
             MOVE  WRK-Z10            TO  SC09-SEIKYUGAKU(IDX1)
      *
      *    食事保険請求額
             MOVE  MF100-SHOKUJIRYOYOHI  TO  WRK-Z11
             MOVE  WRK-Z11       TO  SC09-SHOKUJIMONEY(IDX1)
      *
      *    食事標準負担額
             MOVE  MF100-SHOKUJIFTN   TO  WRK-Z10
             MOVE  WRK-Z10          TO  SC09-SHOKUJIFTN(IDX1)
      *
      *    入院年月日
             IF  MF100-NYUGAIKBN = "1"
                 PERFORM  910-DBSELECT-SEC
                   MOVE  PTNYUINRRK-NYUINYMD  TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE  WRK-HENYMDG1   TO  SC09-NYUINYMD(IDX1)
                 PERFORM   910-DBCLOSE-SEC
             END-IF
      *
      *    公費法別番号
            MOVE  MF100-TAHOU            TO   WRK-KOHNUM
            PERFORM  950-HKNNUM-SET-SEC
            MOVE  HKN-HBTNUM             TO   SC09-KOHNUM(IDX1)
      *
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) start
      *    特記事項
            MOVE  MF100-TOKKI            TO   SC09-TOKKI(IDX1)
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) end
      *
      *    合計小計
      *    件数
             ADD  1                      TO  SUM-GKENSU
      *
      *    保総点数
             ADD  MF100-TOTALTEN         TO  SUM-GTENSU
      *
      *    請求額
             ADD  MF100-SEIKYUGK         TO  SUM-GSEIKYUGAKU
      *
      *    患者負担額
             ADD  MF100-FTNMONEY         TO  SUM-GFTNMONEY
      *
      *    食事療養費
             ADD  MF100-SHOKUJIRYOYOHI   TO  SUM-GSHOKUJIRYOYOHI
      *    食事標準負担額
             ADD  MF100-SHOKUJIFTN       TO  SUM-GSHOKUJIFTN
      *  
      *    レセプト明細読込
             PERFORM 900-TMP-READ-SEC
           END-PERFORM
      *
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *              帳票編集<合計>処理
      *****************************************************************
       3201-GOKEI-HEN-SEC  SECTION.
      *
      *    合計件数
           MOVE  SUM-GKENSU         TO  WRK-Z3
           MOVE  WRK-Z3             TO  SC09-G-KENSU
      *
      *    保険総点数
           MOVE  SUM-GTENSU         TO  WRK-Z11
           MOVE  WRK-Z11            TO  SC09-G-SEIKYUTEN
      *
      *    患者請求額
           MOVE  SUM-GSEIKYUGAKU    TO  WRK-Z10
           MOVE  WRK-Z10            TO  SC09-G-SEIKYUGAKU
      *
      *    患者負担額
           MOVE  SUM-GFTNMONEY      TO  WRK-Z79
           MOVE  WRK-Z79            TO  SC09-G-FTNMONEY
      *    食事療養費
           MOVE  SUM-GSHOKUJIRYOYOHI    TO  WRK-Z11
           MOVE  WRK-Z11            TO  SC09-G-SHOKUJIMONEY
      *    食事標準負担額
           MOVE  SUM-GSHOKUJIFTN    TO  WRK-Z10
           MOVE  WRK-Z10            TO  SC09-G-SHOKUJIFTN
      *
           .
       3201-GOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
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
           MOVE   "SC1209.red"            TO  SPRT-PRTID
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) start
      *     MOVE    "乳幼児医療費請求書"
      *                              TO  SPRT-TITLE
           MOVE    "子ども医療費請求書"   TO  SPRT-TITLE
      *=== 帳票様式の変更に伴う修正 by Yoshikawa(2010/12/03) end
           MOVE    SC09             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.03) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
           MOVE    LNK-DAY2-EDTYMD2    TO  WRK-HENYMDG2
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
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
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"  AND
                   SYU-KOH-HKNNUM(IDX) NOT  =  "027"
               MOVE   1                TO    FLG-HEIYOU
               MOVE   SYU-KOH-COMPFTN(IDX)
                                       TO    WRK-SEIKYUGK
               MOVE  SYU-KOH-HKNNUM(IDX)  TO   MF100-TAHOU
             END-IF
             IF    SYU-KOH-HKNNUM(IDX)   =   "972"
               MOVE    1     TO    FLG-TYOKI
             END-IF
      *---(01.00.02) LINE ADD START ------------------------------------
             IF    SYU-KOH-HKNNUM(IDX)   =   "974"
               MOVE    2     TO    FLG-TYOKI
             END-IF
      *---(01.00.02) LINE ADD END   ------------------------------------
      *=== 公費の助成なしは印字対象外 by Yoshikawa(2009/02/18) start
      *    計算負担金に差が有る場合、地方公費の助成があったと判定する
             IF   SYU-KOH-HKNNUM(IDX)      =  WRK-KOHNUM
               IF     IDX                  =  1
                 IF  SYU-SYUCOMPFTN-ENTANI
                                       > SYU-KOH-COMPFTN-ENTANI(IDX)
                   MOVE  ZERO          TO  FLG-ERR
                 END-IF
               ELSE
                 IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                      >  SYU-KOH-COMPFTN-ENTANI(IDX)
                   MOVE  ZERO          TO  FLG-ERR
                 END-IF
               END-IF
             END-IF
      *=== 公費の助成なしは印字対象外 by Yoshikawa(2009/02/18) end
           END-PERFORM
      *
           IF    FLG-HEIYOU    =    ZERO
      *    総点数×給付割合を加算
               COMPUTE  WRK-SEIKYUGK  =  SYU-TOTAL-HKNTEN *
                                         RECE02-KYURATE / 10
           END-IF
      *
           ADD    WRK-SEIKYUGK               TO    WRK-SEIKYUGK-G
      *
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *       MOVE  RECE02-HOSPID      TO  SYU-HOSPID
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *       MOVE  WRK-PATH           TO  MCP-PATH
      *       CALL  "MCPSUB"           USING
      *                                MCPAREA
      *                                MCPDATA-REC
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    WRK-PATH         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
             IF  MCP-RC  =  ZERO
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE  WRK-PATH         TO  MCP-PATH
               MOVE    "tbl_syunou"     TO  MCP-TABLE
               MOVE    WRK-PATH         TO  MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF  SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *           MOVE  WRK-PATH              TO  MCP-PATH
                 MOVE    "tbl_syunou"     TO  MCP-TABLE
                 MOVE    WRK-PATH         TO  MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"     TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *       MOVE  WRK-PATH            TO  MCP-PATH
      *       CALL  "MCPSUB"     USING
      *                          MCPAREA
      *                          MCPDATA-REC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    SYU-HOSPID                 TO  COMB-HOSPID
           MOVE    SYU-HOSPNUM                TO  COMB-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    SYU-PTID                   TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM            TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-HKNCOMBI-KEY      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_hkncombi"         TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報読込
      *****************************************************************
       945-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1            TO  FLG-HKNNUM
           END-IF
      *
           .
       945-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  MF100-HOSPID                TO  HKN-HOSPID
           MOVE  SPA-HOSPNUM                 TO  SYU-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  WRK-KOHNUM                  TO  HKN-HKNNUM
           MOVE  HKNNUM-REC                  TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNNUM-KEY2        TO  MCP-PATH
      *     CALL  "MCPSUB"             USING
      *                                MCPAREA
      *                                MCPDATA-REC
           MOVE    "tbl_hknnum"              TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.03) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-HKNNUM-KEY2      TO  MCP-PATH
             MOVE    "tbl_hknnum"            TO  MCP-TABLE
             MOVE    "key2"                  TO  MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
             PERFORM  945-HKNNUM-READ-SEC
      *
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNNUM-KEY2    TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           .
       950-HKNNUM-SET-EXT.
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
           DISPLAY "*** SEIKYU1209 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1209 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU1209 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.03) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
               NOT  AT  END
                   MOVE  MF100-CITYNUM1  TO    KEY-NEW-CITYNUM
                   MOVE  MF100-SRYYM     TO    KEY-NEW-SRYYM
                   MOVE  MF100-TOKUKYURATE TO KEY-NEW-TOKUKYURATE
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    WRK-PARA-HOSPID     TO  RECE02-HOSPID
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *---(01.00.03) LINE ADD END   ------------------------------------
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
      *=== 印字対象番号追加 by Yoshikawa(2008/12/02) start
      *         IF  RECE02-KOHNUM(1) = "183" OR "283" OR "383" OR "483"
      *          OR RECE02-KOHNUM(2) = "183" OR "283" OR "383" OR "483"
      *          OR RECE02-KOHNUM(3) = "183" OR "283" OR "383" OR "483"
      *          OR RECE02-KOHNUM(4) = "183" OR "283" OR "383" OR "483"
               IF  RECE02-KOHNUM(1) = "183" OR "283" OR "383"
                                   OR "483" OR "583" OR "683"
                OR RECE02-KOHNUM(2) = "183" OR "283" OR "383"
                                   OR "483" OR "583" OR "683"
                OR RECE02-KOHNUM(3) = "183" OR "283" OR "383"
                                   OR "483" OR "583" OR "683"
                OR RECE02-KOHNUM(4) = "183" OR "283" OR "383"
                                   OR "483" OR "583" OR "683"
      *=== 印字対象番号追加 by Yoshikawa(2008/12/02) end
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
      *---(01.00.03) LINE ADD START ------------------------------------
      *             MOVE    RECE02-HOSPID       TO  PTINF-HOSPID
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *---(01.00.03) LINE ADD END   ------------------------------------
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴 SELECT
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  PTNYUINRRK-REC
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    MF100-HOSPID           TO  PTNYUINRRK-HOSPID
           MOVE    MF100-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    MF100-PTID             TO  PTNYUINRRK-PTID
           MOVE    MF100-SRYYM(1:6)       TO
                                              PTNYUINRRK-NYUINYMD(1:6)
                                              PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "99"                   TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    "01"                   TO  PTNYUINRRK-TAIINYMD(7:2)
           MOVE    PTNYUINRRK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY11  TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key11"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF   MCP-RC       =   ZERO
              PERFORM 910-DBFETCH-SEC
           ELSE
              MOVE    1     TO  FLG-PTNYUINRRK
           END-IF        
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴 FETCH
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY11  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key11"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF  MCP-RC     =   ZERO
              MOVE    ZERO              TO  FLG-PTNYUINRRK
              MOVE    MCPDATA-REC       TO  PTNYUINRRK-REC
           ELSE
              MOVE    1    TO  FLG-PTNYUINRRK
           END-IF  
      *
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴 CLOSE
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"               TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY11     TO  MCP-PATH
      *     CALL    "MCPSUB"               USING
      *                                    MCPAREA
      *                                    MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key11"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    SYS-1001-HOSPID     TO  HKNJA-HOSPID
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE    MF100-CITYNUM2      TO  HKNJA-HKNJANUM(1:5)
           DISPLAY "CITYNUM=" MF100-CITYNUM2
           MOVE    HKNJAINF-REC                TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNJAINF-KEY2      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
      *---(01.00.01) LINE ADD START ------------------------------------
               PERFORM  900-HKNJAINF-READ-SEC
      *
               PERFORM  UNTIL  FLG-HKNJAINF  =  1
      *        国保かどうか
                 IF  HKNJA-HKNNUM  =  "060"
                     MOVE  1  TO  FLG-HKNJAINF
                 ELSE
                     PERFORM  900-HKNJAINF-READ-SEC
                 END-IF
               END-PERFORM
      *---(01.00.01) LINE ADD END   ------------------------------------
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
               INITIALIZE                      HKNJAINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNJAINF-KEY2      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key2"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
           .
      *
       900-HKNJAINF-INV-EXT.
           EXIT.
      *---(01.00.01) LINE ADD START ------------------------------------
      *****************************************************************
      *    保険者情報FETCH
      *****************************************************************
       900-HKNJAINF-READ-SEC        SECTION.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE    "DBFETCH"                   TO  MCP-FUNC
      *     MOVE    PATH-TBL-HKNJAINF-KEY2      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-HKNJAINF
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
         DISPLAY  "保険者  =  "  HKNJA-HKNJANAME
         DISPLAY  "           "  HKNJA-HKNJANUM
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
               INITIALIZE                      HKNJAINF-REC
           END-IF
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *---(01.00.01) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-SYSKANRI-KEY10   TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      *       CALL  "MCPSUB"        USING
      *                             MCPAREA
      *                             MCPDATA-REC
             MOVE  "tbl_syskanri"          TO  MCP-TABLE
             MOVE  "key10"                 TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      *     CALL  "MCPSUB"        USING
      *                           MCPAREA
      *                           MCPDATA-REC
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
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
      *---(01.00.03) LINE ADD END   ------------------------------------
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                 MCPAREA
      *                                 MCPDATA-REC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
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
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  "DBOPEN"   TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  "DBSTART"  TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
      *
      *---(01.00.03) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.03) LINE ADD END   ------------------------------------
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
      *---(01.00.03) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.03) LINE ADD END   ------------------------------------
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
