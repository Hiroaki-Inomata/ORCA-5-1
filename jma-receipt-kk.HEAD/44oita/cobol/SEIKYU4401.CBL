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
       PROGRAM-ID.     SEIKYU4401.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 大分・乳幼児医療費助成金請求書
      *                      (183,283)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/30    有村          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01     内田     06/10/17    帳票様式の変更にともなう修正
      *  01.00.02     内田     07/08/14    日レセver4.0対応
      *                                    システム管理マスタ「1901」の
      *                                    医療機関情報を参照
      *                                    長期上位（974）は長期と同様の
      *                                    処理をする
      *  01.00.03     内田     08/05/21    一部負担額欄を１円単位四捨五入
      *                                    社保の保険者名が印字されなかった
      *                                    のを修正
      *  01.00.04     吉川     08/11/20    SPA-AREA初期化
      *  01.00.05     門間     08/12/02    小計・合計日数を３ｹﾀ→４ｹﾀに修正
      *  01.00.06     吉川     09/07/03    豊後大野市、由布市、国東市の
      *                                    市町村名を取得できるように修正
      *  01.00.07     吉川     10/10/12    帳票様式の変更に伴う修正
      *  01.00.08     吉川     11/02/01    高額療養費の対象で
      *                                    限度額認定証を持っていない場合
      *                                    適用区分は「Ｄ」とする
      *                                    open-cobol1.0対応
      *  01.00.09     吉川     11/03/14    佐伯市と姫島村の場合は、
      *                                    一部負担金欄は0と記載する
      *  01.00.10     吉川     11/06/21    佐伯市、負担者番号83448050で
      *                                    外来の場合のみ負担金襴は記載しない
      *  01.00.11     吉川     11/10/12    佐伯市と姫島村の場合の日数と
      *                                    一部負担金欄の記載を修正
      *  01.00.12     吉川     13/02/08    医療機関情報編集対応
      *  01.00.13     吉川     14/10/28    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.14     吉川     15/01/30    平成２７年１月改正対応
      *  01.00.15     吉川     15/10/14    自己負担額欄は１円単位四捨五入
      *                                    医療機関編集情報への開設者名
      *                                    追加対応
      *  01.00.16     井上     19/02/05    修正履歴コメント削除
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
           COPY  "SEI4401.INC".
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
           COPY    "CPRECEERR.INC".
      *
           COPY  "SC4401.INC".
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
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-RECE10     PIC 9(01).
           03  FLG-TEKIYOD    PIC 9(01).
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
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC  X(200).
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z6-9            PIC ZZ,ZZ9.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMDG1        PIC X(09).
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
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-SYUKEI-TBL.
               05  WRK-KOHNUM      PIC X(03).
               05  WRK-KOHID       PIC 9(10).
               05  WRK-SEIKYUGK-G  PIC 9(08)V9(01).
               05  WRK-SEIKYUGK-G2 PIC 9(08).
               05  WRK-SEIKYUGK    PIC 9(08)V9(01).
               05  WRK-KOHTENSU    PIC 9(08).
               05  WRK-KOHFTN      PIC 9(08).
               05  WRK-KOHNISSU    PIC 9(02).
               05  WRK-KOHSKJFTN   PIC 9(08).
               05  WRK-KOHSKJFTN-G PIC 9(08).
               05  WRK-TAHOTENSU   PIC 9(08).
               05  WRK-TAHOFTN     PIC 9(08)V9(01).
               05  WRK-TAHOFTN-G   PIC 9(08)V9(01).
               05  WRK-KEISAN      PIC 9(08)V9(01).
           03  WRK-PATH            PIC X(50).
           03  WRK-ITBFTN          PIC 9(09).
           03  WRK-NISSU           PIC 9(09).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
      *
      *    TMPファイルデータ一時退避領域
       01  MF101-REC.
           COPY  "SEI4401.INC"
                   REPLACING  //MF100//
                   BY         //MF101//.
      *
       01  SUM-AREA  VALUE   ZERO.
           03  SUM-GOKEI.
               05  SUM-GNISSU          PIC 9(04).
               05  SUM-GTENSU          PIC 9(07).
               05  SUM-GKOHFTNGK       PIC 9(07).
               05  SUM-GITBFTN         PIC 9(07).
               05  SUM-GNNISSU         PIC 9(04).
               05  SUM-GNYUFTNGK       PIC 9(07).
               05  SUM-GSEIKYUGK       PIC 9(07).
           03  SUM-SYOKEI.
               05  SUM-SNISSU          PIC 9(04).
               05  SUM-STENSU          PIC 9(07).
               05  SUM-SKOHFTNGK       PIC 9(07).
               05  SUM-SITBFTN         PIC 9(07).
               05  SUM-SNNISSU          PIC 9(04).
               05  SUM-SNYUFTNGK       PIC 9(07).
               05  SUM-SSEIKYUGK       PIC 9(07).
      *    キー用エリア
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-FTNJANUM     PIC X(08).
           03  KEY-OLD.
               05  KEY-O-FTNJANUM     PIC X(08).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    固定領域
       01  CONS-AREA.
      *    市町村番号(FTN=負担者番号、CITY=保険者番号)
           03  CONS-CITYNUM.
      *        豊後大野
               05  CONS-BUNGO-FTN      PIC X(02)  VALUE  "36".
               05  CONS-BUNGO-CITY     PIC X(03)  VALUE  "101".
      *        由布
               05  CONS-YUFU-FTN       PIC X(02)  VALUE  "25".
               05  CONS-YUFU-CITY      PIC X(03)  VALUE  "102".
      *        国東
               05  CONS-KUNISAKI-FTN   PIC X(02)  VALUE  "17".
               05  CONS-KUNISAKI-CITY  PIC X(03)  VALUE  "103".
      *        佐伯市
               05  CONS-SAIKI-CITYNUM  PIC X(02)  VALUE  "05".
      *        姫島村
               05  CONS-HIMESHIMA-CITYNUM  PIC X(02)  VALUE  "16".
      *    負担者番号
           03  CONS-FTNJANUM.
               05  CONS-SAIKI-SYOGAKU  PIC X(08)  VALUE  "83448050".
           03  WRK-CONS-SRYYM-201501   PIC X(06)  VALUE  "201501".
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
           COPY  "CPSYUNOU.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    請求管理
           COPY    "CPRCF010.INC".
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
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
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
           MOVE    "SEIKYU4401"    TO  JOB-PGID
           MOVE    "子ども医療費助成金請求書" 
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
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1001-STYUKYMD(1:6)
                                   SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"        TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU4401 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1002-STYUKYMD(1:6)
                                   SYS-1002-EDYUKYMD(1:6)
           MOVE    "01"        TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU4401 SYS 1002 ERR ***"
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
           OPEN    OUTPUT  MF100-FILE
           CLOSE           MF100-FILE
           OPEN    I-O     MF100-FILE
      *
           MOVE    ZERO    TO      FLG-END
      *
           PERFORM 2001-SYUKEI-SEC
                   UNTIL   FLG-END     =   1
      *
           CLOSE   MF100-FILE
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
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               IF  RECE02-KOHNUM (IDZ) = "183" OR "283"
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
           INITIALIZE      MF100-REC
      *
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-TEISYUTUSAKI TO  MF100-TEISYUTUSAKI
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-FLG(1)       TO  MF100-MISYUGAKU
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF100-FTNJANUM
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF100-JKYSNUM
           COMPUTE IDY     =       IDZ     +   1
           MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSU
           MOVE  RECE02-TOTALTENINF (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-FTNMONEYINF (IDY)  TO  MF100-FTNMONEY
           MOVE  RECE02-SHOKUJIINF  (IDY)  TO  MF100-SHOKUJIINF
      *
      *    乳幼児−日数
           MOVE  MF100-JNISSU              TO  MF100-NNISSU
           IF  (MF100-NYUGAIKBN  =  1)  AND  (MF100-JNISSU  >  14)
             MOVE  14  TO  MF100-NNISSU
           END-IF
           IF  (MF100-NYUGAIKBN  =  2)  AND  (MF100-JNISSU  >  4)  AND
               (MF100-KOHNUM  =  183)
             MOVE  4  TO  MF100-NNISSU
           END-IF
           IF  (MF100-NYUGAIKBN  =  2)  AND  (MF100-JNISSU  >  2)  AND
               (MF100-KOHNUM  =  283)
             MOVE  2  TO  MF100-NNISSU
           END-IF
      *
      *    保険者番号チェック処理
           MOVE  ZERO                          TO  FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF      FLG-ERR                 =   ZERO
               PERFORM 20012-HENSYU-SEC
           ELSE
               DISPLAY "助成対象外=" RECE02-PTNUM
                       "診療年月="   RECE02-SRYYM
                       "入外区分="   RECE02-NYUGAIKBN
                       "負担者番号=" MF100-FTNJANUM
           END-IF
      *
      *
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
           MOVE    "440"                 TO  WRK-HKNJANUM(1:3)
           MOVE    MF100-FTNJANUM(6:2)   TO  WRK-HKNJANUM(4:2)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
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
      *    患者ファイル編集処理
      *****************************************************************
       20012-HENSYU-SEC  SECTION.
      *
      *    収納情報より請求金額を集計
           INITIALIZE       WRK-SYUKEI-TBL
           MOVE    MF100-KOHNUM    TO  WRK-KOHNUM
           MOVE    MF100-KOHID     TO  WRK-KOHID
           MOVE    ZERO            TO  FLG-TYOKI
                                       FLG-TEKIYOD
      *
           IF      RECE02-NYUGAIKBN    =   "1"
               MOVE    "key33"     TO  WRK-PATH
           ELSE
               MOVE    "key25"     TO  WRK-PATH
           END-IF
           PERFORM 920-SYUNOU-SET-SEC
      *
      *    保険に限度額が有る場合の対応
           IF     RECE02-FTNMONEY(1)  >   ZERO     AND
                  RECE02-FTNMONEY(1)  <   WRK-SEIKYUGK-G
             MOVE    RECE02-FTNMONEY(1)   TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    長期
           IF    FLG-TYOKI    =    1   AND
                 WRK-SEIKYUGK-G    >=  10000
               MOVE    10000          TO    WRK-SEIKYUGK-G
           END-IF
      *
      *    長期上位
           IF    FLG-TYOKI    =    2   AND
                 WRK-SEIKYUGK-G    >=  20000
               MOVE    20000          TO    WRK-SEIKYUGK-G
           END-IF
      *
      *    四捨五入
           COMPUTE  WRK-SEIKYUGK-G2 ROUNDED  =  WRK-SEIKYUGK-G
           MOVE     WRK-SEIKYUGK-G2    TO    MF100-SEIKYUGK
      *
           IF      (RECE02-KOHNUM(IDZ + 1)   =   SPACE)
           AND     (IDZ        =   1)
               PERFORM  900-SEIKYU-READ-SEC
               IF      RECE10-KOHNUM(1)  NOT =   SPACE
                   MOVE    RECE10-TOTALTEN(2)    TO  WRK-KOHTENSU
                   MOVE    WRK-TAHOFTN-G         TO  WRK-KOHFTN
                   MOVE    RECE10-JNISSU(2)      TO  WRK-KOHNISSU
               END-IF
           ELSE
               MOVE    WRK-TAHOTENSU         TO  WRK-KOHTENSU
               MOVE    WRK-TAHOFTN-G         TO  WRK-KOHFTN
           END-IF
      *
      *    １円単位は四捨五入する
           COMPUTE  WRK-KEISAN  =  WRK-KOHFTN  /  10 
           COMPUTE  WRK-KOHFTN ROUNDED =  WRK-KEISAN 
           COMPUTE  WRK-KOHFTN  =  WRK-KOHFTN  *  10
      *
      *    総点数＝公費分点数の場合
           IF      MF100-TOTALTEN  =   WRK-KOHTENSU
               MOVE    WRK-KOHFTN          TO  MF100-KOHITBFTN
               MOVE    WRK-KOHSKJFTN-G     TO  MF100-SHOKUJIFTN
               COMPUTE MF100-SEIKYUGK      =   WRK-KOHFTN
                                           +   WRK-KOHSKJFTN-G
               MOVE    ZERO                TO  MF100-KYURATE
               MOVE    1                   TO  MF100-KBN
           ELSE
      *        総点数と公費分点数が異る場合
               IF      WRK-KOHTENSU    >   ZERO
                   MOVE    MF100-REC           TO  MF101-REC
                   MOVE    WRK-KOHTENSU        TO  MF100-TOTALTEN
                   MOVE    WRK-KOHNISSU        TO  MF100-JNISSU
                   MOVE    WRK-KOHFTN          TO  MF100-KOHITBFTN
                   MOVE    ZERO                TO  MF100-KYURATE
                   MOVE    2                   TO  MF100-KBN
                   
                   INITIALIZE  MF100-SHOKUJIINF
                   COMPUTE MF100-SEIKYUGK      =   WRK-KOHFTN
                   WRITE   MF100-REC
      *
                   MOVE    MF101-REC           TO  MF100-REC
      *            分点している場合、総点数から分点を差し引く
                   COMPUTE MF100-TOTALTEN      =   MF100-TOTALTEN
                                               -   WRK-KOHTENSU
                   MOVE    WRK-SEIKYUGK-G2     TO  MF100-KOHFTNGK
                   MOVE    1                   TO  MF100-KBN
                   MOVE    WRK-KOHSKJFTN-G     TO  MF100-SHOKUJIFTN
                   IF      WRK-KOHSKJFTN-G     =   ZERO
                       MOVE    ZERO            TO  MF100-SHOKUJINISSU
                   END-IF
                   COMPUTE MF100-SEIKYUGK      =   WRK-SEIKYUGK-G2
                                               +   WRK-KOHSKJFTN-G
               ELSE
      *            全国公費を併用していない場合
                   MOVE    WRK-SEIKYUGK-G2     TO  MF100-KOHFTNGK
                   COMPUTE MF100-SEIKYUGK      =   WRK-SEIKYUGK-G2
                                               +   WRK-KOHSKJFTN-G
                   MOVE    1                   TO  MF100-KBN
               END-IF
           END-IF
      *    乳幼児−日数
      *
      *    適用区分（高額療養費）
           IF  MF100-SRYYM  >=  WRK-CONS-SRYYM-201501
               EVALUATE  RECE02-ZAITAKU
                 WHEN  "2"      MOVE     5     TO  MF100-KOGAKU
                 WHEN  "3"      MOVE     6     TO  MF100-KOGAKU
                 WHEN  "4"      MOVE     7     TO  MF100-KOGAKU
                 WHEN  "5"      MOVE     8     TO  MF100-KOGAKU
                 WHEN  "6"      MOVE     9     TO  MF100-KOGAKU
               END-EVALUATE
               IF  FLG-TEKIYOD  =  1
                 MOVE      4                   TO  MF100-KOGAKU
               END-IF
           ELSE
             IF  RECE02-NYUGAIKBN  =  "1"
             
               EVALUATE  RECE02-ZAITAKU
                 WHEN  "2"
                   MOVE      1                 TO  MF100-KOGAKU
                 WHEN  "3"
                   MOVE      2                 TO  MF100-KOGAKU
                 WHEN  "4"
                   MOVE      3                 TO  MF100-KOGAKU
               END-EVALUATE
               IF  FLG-TEKIYOD  =  1
                 MOVE      4                   TO  MF100-KOGAKU
               END-IF
             END-IF
           END-IF
      *
      *    佐伯市、姫島村は自己負担はゼロ
           IF ((MF100-FTNJANUM(6:2)  =  CONS-HIMESHIMA-CITYNUM)
                OR (MF100-FTNJANUM(6:2)  =  CONS-SAIKI-CITYNUM))
           AND (MF100-NYUGAIKBN  =  "2")
           AND (MF100-MISYUGAKU NOT =  "1")
             MOVE    ZERO                      TO  MF100-FTNMONEY
           END-IF
      *
           WRITE   MF100-REC
      * 
           .
       20012-HENSYU--EXT.
           EXIT.
      **
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN    INPUT       MF100-FILE
      *
           MOVE    ZERO        TO       FLG-PRINT
           INITIALIZE  SUM-AREA
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
             PERFORM 310-HEAD-HEN-SEC
      *
      *    帳票編集<明細>処理
             PERFORM 320-BODY-HEN-SEC
      *
      *    帳票編集<合計>処理
             IF      KEY-NEW     NOT =   KEY-OLD
                 PERFORM 325-SYOKEI-HEN-SEC
                 INITIALIZE  SUM-AREA
             END-IF
      *
      *    帳票印刷処理
             PERFORM 330-PRINT-OUT-SEC
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
           ADD     1           TO      CNT-PAGE
           INITIALIZE                             SC01
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SC01-PRTYM
      *
      *    請求日付
           MOVE    WRK-SEIYMDWH            TO  SC01-SEIYMD
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC01-HOSPCD
      *
      *    住所
           MOVE  WRK-HOSPADRS              TO  SC01-ADRS
           INSPECT SC01-ADRS               REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME              TO  SC01-HOSPNAME
           INSPECT SC01-HOSPNAME           REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SC01-KAISETU
      *
      *    負担者番号
           MOVE    MF100-FTNJANUM          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SC01-FTNJANUM
      *
      *    市町村名
           MOVE    "440"                   TO  WRK-HKNJANUM(1:3)
           EVALUATE  MF100-FTNJANUM(6:2)
      *      豊後大野市
             WHEN  CONS-BUNGO-FTN
                 MOVE  CONS-BUNGO-CITY     TO  WRK-HKNJANUM(3:3)
      *      由布市
             WHEN  CONS-YUFU-FTN
                 MOVE  CONS-YUFU-CITY      TO  WRK-HKNJANUM(3:3)
      *      国東市
             WHEN  CONS-KUNISAKI-FTN
                 MOVE  CONS-KUNISAKI-CITY  TO  WRK-HKNJANUM(3:3)
      *      その他
             WHEN  OTHER
                 MOVE  MF100-FTNJANUM(6:2) TO  WRK-HKNJANUM(4:2)
           END-EVALUATE
           MOVE    "key2"                  TO  WRK-PATH
           PERFORM 920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME           =    SPACE
               MOVE    "市町村長　殿"      TO   SC01-CITYNAME(17:12)
           ELSE
               STRING  HKNJA-HKNJANAME     DELIMITED  BY SPACE
                       "市町村長　殿"      DELIMITED  BY SIZE
                                           INTO    SC01-CITYNAME
               END-STRING
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
           INITIALIZE  SUM-SYOKEI
           MOVE    KEY-NEW     TO     KEY-OLD
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       15
                   OR      KEY-NEW     NOT =   KEY-OLD
                   OR      FLG-PRINT       =   1
      *
      *        受給資格証記号番号（受給者番号）
               MOVE    MF100-KOHJKYSNUM        TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT   TO  SC01-JKYSNUM(IDX)
      *
      *        氏名
               MOVE    MF100-NAME              TO  SC01-NAME(IDX)
      *
      *        生年月日
               MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3(5:4)
                                           TO  SC01-TANJOYMD(IDX)(1:4)
               MOVE    LNK-DAY2-EDTYMD3(11:4)
                                           TO  SC01-TANJOYMD(IDX)(5:4)
               MOVE    LNK-DAY2-EDTYMD3(17:4)
                                           TO  SC01-TANJOYMD(IDX)(9:4)
      *
      *        性別
               IF      MF100-SEX   =   "1"
                   MOVE    "◯"        TO  SC01-SEX(IDX 1)
               ELSE
                   MOVE    "◯"        TO  SC01-SEX(IDX 2)
               END-IF
      *
      *        保険
               IF      MF100-TEISYUTUSAKI  =   1
                   MOVE    "◯"        TO  SC01-HKN(IDX 1)
               ELSE
                   MOVE    "◯"        TO  SC01-HKN(IDX 2)
               END-IF
      * 
      *        入外区分
               IF      MF100-NYUGAIKBN     =   "1"
                   MOVE    "◯"        TO  SC01-NYUGAIKBN(IDX 1)
               ELSE
                   MOVE    "◯"        TO  SC01-NYUGAIKBN(IDX 2)
               END-IF
      *
      *        給付割合
      *        全国公費を併用しており、一部負担金欄に記載する場合は
      *        何も記載しない
               EVALUATE    MF100-KYURATE
                   WHEN    10
                       MOVE    "1"     TO  SC01-FTNRATE(IDX)
                   WHEN    20
                       MOVE    "2"     TO  SC01-FTNRATE(IDX)
                   WHEN    30
                       MOVE    "3"     TO  SC01-FTNRATE(IDX)
               END-EVALUATE
      *
      *        日数
               MOVE    MF100-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC01-NISSU(IDX)
      *
      *        点数
               MOVE    MF100-TOTALTEN      TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC01-TENSU(IDX)
      *
      *        公費負担額
               MOVE    MF100-KOHFTNGK      TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC01-KOHFTNGK(IDX)
      *
      *        一部負担金
               MOVE    MF100-KOHITBFTN     TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC01-ITBFTN(IDX)
      *
      *        佐伯市と姫島村で小中学の外来は記載しない
               IF ((MF100-FTNJANUM(6:2)  =  CONS-HIMESHIMA-CITYNUM)
                    OR (MF100-FTNJANUM(6:2)  =  CONS-SAIKI-CITYNUM))
               AND (MF100-NYUGAIKBN  =  "2")
               AND (MF100-MISYUGAKU  NOT =  "1")
                 MOVE    ZERO                TO  MF100-FTNMONEY
               ELSE
      *          乳幼児−日数
                 MOVE    MF100-NNISSU        TO  WRK-Z2
                 MOVE    WRK-Z2              TO  SC01-NNISSU(IDX)
      *
      *          乳幼児−一部負担額
                   MOVE    MF100-FTNMONEY    TO  WRK-Z6
                   MOVE    WRK-Z6            TO  SC01-NYUFTNGK(IDX)
               END-IF
      *
      *        市町村負担額
               COMPUTE  WRK-ITBFTN = MF100-KOHFTNGK + MF100-KOHITBFTN
                                   - MF100-FTNMONEY
               MOVE    WRK-ITBFTN          TO  WRK-Z7
               MOVE    WRK-Z7              TO  SC01-SEIKYUGK(IDX)
      *
      *        診療年月
               IF      MF100-SRYYM    NOT  =   LNK-PRTKANRI-SRYYM
                   MOVE    MF100-SRYYM     TO  WRK-SYMD (1:6)
                   MOVE    "01"            TO  WRK-SYMD (7:2)
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD1(2:2)
                                           TO  SC01-SRYYM(IDX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD1(5:2)
                                           TO  SC01-SRYYM(IDX)(3:2)
               END-IF
      *
      *        保険者名
               MOVE    MF100-HKNJANUM          TO  WRK-HKNJANUM
               MOVE    "key"                   TO  WRK-PATH
               PERFORM 920-HKNJAINF-SET-SEC
               MOVE    HKNJA-HKNJANAME     TO  SC01-HKNJANAME(IDX)
      *
      *        適用区分
               IF  MF100-SRYYM  >=  WRK-CONS-SRYYM-201501
                 EVALUATE  MF100-KOGAKU
                   WHEN  5
                     MOVE    "ア"          TO  SC01-KOGAKU(IDX)
                   WHEN  6
                     MOVE    "イ"          TO  SC01-KOGAKU(IDX)
                   WHEN  7
                     MOVE    "ウ"          TO  SC01-KOGAKU(IDX)
                   WHEN  8
                     MOVE    "エ"          TO  SC01-KOGAKU(IDX)
                   WHEN  9
                     MOVE    "オ"          TO  SC01-KOGAKU(IDX)
                   WHEN  4
                     MOVE    "カ"          TO  SC01-KOGAKU(IDX)
                 END-EVALUATE
               ELSE
                 IF  MF100-NYUGAIKBN  =  "1"
                   EVALUATE  MF100-KOGAKU
                     WHEN  1
                       MOVE    "Ａ"        TO  SC01-KOGAKU(IDX)
                     WHEN  2
                       MOVE    "Ｂ"        TO  SC01-KOGAKU(IDX)
                     WHEN  3
                       MOVE    "Ｃ"        TO  SC01-KOGAKU(IDX)
                     WHEN  4
                       MOVE    "Ｄ"        TO  SC01-KOGAKU(IDX)
                   END-EVALUATE
                 END-IF
               END-IF
      *
      *        小計加算
               ADD     MF100-JNISSU            TO  SUM-SNISSU
               ADD     MF100-TOTALTEN          TO  SUM-STENSU
               ADD     MF100-KOHFTNGK          TO  SUM-SKOHFTNGK
               ADD     MF100-KOHITBFTN         TO  SUM-SITBFTN 
      *
      *        佐伯市と姫島村で小中学の外来は記載しない
               IF ((MF100-FTNJANUM(6:2)  =  CONS-HIMESHIMA-CITYNUM)
                    OR (MF100-FTNJANUM(6:2)  =  CONS-SAIKI-CITYNUM))
               AND (MF100-NYUGAIKBN  =  "2")
               AND (MF100-MISYUGAKU  NOT =  "1")
                 CONTINUE
               ELSE
                 ADD   MF100-NNISSU            TO  SUM-SNNISSU
                 ADD   MF100-FTNMONEY          TO  SUM-SNYUFTNGK
               END-IF
               ADD     MF100-SEIKYUGK          TO  SUM-SSEIKYUGK
      *
      *        合計加算
               ADD     MF100-JNISSU            TO  SUM-GNISSU
               ADD     MF100-TOTALTEN          TO  SUM-GTENSU
               ADD     MF100-KOHFTNGK          TO  SUM-GKOHFTNGK
               ADD     MF100-KOHITBFTN         TO  SUM-GITBFTN 
      *
      *        佐伯市と姫島村で小中学の外来は記載しない
               IF ((MF100-FTNJANUM(6:2)  =  CONS-HIMESHIMA-CITYNUM)
                    OR (MF100-FTNJANUM(6:2)  =  CONS-SAIKI-CITYNUM))
               AND (MF100-NYUGAIKBN  =  "2")
               AND (MF100-MISYUGAKU  NOT =  "1")
                 CONTINUE
               ELSE
                 ADD   MF100-NNISSU            TO  SUM-GNNISSU
                 ADD   MF100-FTNMONEY          TO  SUM-GNYUFTNGK
               END-IF
               ADD     MF100-SEIKYUGK          TO  SUM-GSEIKYUGK
      *
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
      *    小計編集
      *    日数
           MOVE    SUM-SNISSU              TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SC01-GNISSU(1)
      *
      *    点数
           MOVE    SUM-STENSU              TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GTENSU(1)
      *
      *    公費負担額
           MOVE    SUM-SKOHFTNGK           TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GKOHFTNGK(1)
      * 
      *    一部負担金
           MOVE    SUM-SITBFTN             TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GITBFTN(1)
      *
      *    乳幼児−日数
           MOVE    SUM-SNNISSU             TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SC01-GNNISSU(1)
      *
      *    乳幼児−一部負担額
           MOVE    SUM-SNYUFTNGK           TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GNYUFTNGK(1)
      *
      *    市町村負担額
           COMPUTE  WRK-ITBFTN =  SUM-SKOHFTNGK + SUM-SITBFTN
                               -  SUM-SNYUFTNGK
           MOVE    WRK-ITBFTN              TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GSEIKYUGK(1)
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *              帳票編集<合計>処理
      *****************************************************************
       325-SYOKEI-HEN-SEC  SECTION.
      *
      *    日数
           MOVE    SUM-GNISSU              TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SC01-GNISSU(2)
      *
      *    点数
           MOVE    SUM-GTENSU              TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GTENSU(2)
      *
      *    公費負担額
           MOVE    SUM-GKOHFTNGK           TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GKOHFTNGK(2)
      * 
      *    一部負担金
           MOVE    SUM-GITBFTN             TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GITBFTN(2)
      *
      *    乳幼児−日数
           MOVE    SUM-GNNISSU             TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SC01-GNNISSU(2)
      *
      *    乳幼児−一部負担額
           MOVE    SUM-GNYUFTNGK           TO  WRK-Z9
           MOVE    WRK-Z9                  TO  SC01-GNYUFTNGK(2)
      *
      *    市町村負担額
           COMPUTE  WRK-ITBFTN = SUM-GKOHFTNGK + SUM-GITBFTN
                               - SUM-GNYUFTNGK
           MOVE    WRK-ITBFTN              TO  WRK-Z9 
           MOVE    WRK-Z9                  TO  SC01-GSEIKYUGK(2)
      *
           .
       325-SYOKEI-HEN-EXT.
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
           MOVE   "SC4401.red"         TO  SPRT-PRTID
           MOVE   "子ども医療費助成金請求書"
                                       TO  SPRT-TITLE
           MOVE    SC01                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
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
           DISPLAY "*** SEIKYU4401 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU4401 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU4401 END ***"
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
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE    1       TO  FLG-PRINT
                   MOVE    HIGH-VALUE  TO  KEY-NEW
               NOT AT  END
                   MOVE    ZERO    TO  FLG-PRINT
                   MOVE    MF100-FTNJANUM      TO  KEY-N-FTNJANUM
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
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
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF  RECE02-KOHNUM(1) = "183" OR "283"
               OR  RECE02-KOHNUM(2) = "183" OR "283"
               OR  RECE02-KOHNUM(3) = "183" OR "283"
               OR  RECE02-KOHNUM(4) = "183" OR "283"
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
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
      *    請求管理情報読み込み
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
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
      *    請求額集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO      WRK-SEIKYUGK
                                   WRK-TAHOFTN
                                   FLG-HEIYOU
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
               IF  SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                   SYU-KOH-HKNNUM(IDX)     NOT =   "027"
                   MOVE    1       TO      FLG-HEIYOU
                   ADD     SYU-TOTAL-HKNTEN    TO  WRK-TAHOTENSU
      *
                   MOVE    SYU-KOH-COMPFTN-ENTANI(IDX)
                                               TO  WRK-TAHOFTN
                   MOVE    SYU-SKJ-FTN(IDX)    TO  WRK-KOHSKJFTN
      *
               END-IF
      *
               IF  SYU-KOH-HKNNUM(IDX)         =   "972"
                   MOVE    1       TO      FLG-TYOKI
               END-IF
      *        長期上位
               IF  SYU-KOH-HKNNUM(IDX)         =   "974"
                   MOVE    2       TO      FLG-TYOKI
               END-IF
      *
      *        限度額認定証を持っていない場合の高額療養費相当額が
      *        計算されていたら、適用区分Dのフラグを立てる
               IF  SYU-KOH-HKNNUM(IDX)  =  "183" OR "283"
                 IF  SYU-KOH-COMPFTN-ENTANI(IDX)  <  SYU-KOH-FTN-ENTANI
                   MOVE    1       TO      FLG-TEKIYOD
                 END-IF
               END-IF
      *
           END-PERFORM
      *
           IF  FLG-HEIYOU      =   ZERO
               MOVE    SYU-SYUSKJ-FTN  TO  WRK-KOHSKJFTN
                   IF  RECE02-RJNHKNNUM    NOT =   SPACE
                       MOVE    SYU-KOH-COMPFTN(1)  TO  WRK-SEIKYUGK
                   ELSE
                       MOVE    SYU-SYUCOMPFTN      TO  WRK-SEIKYUGK
                   END-IF
           END-IF
      *
           ADD     WRK-SEIKYUGK        TO  WRK-SEIKYUGK-G
           ADD     WRK-TAHOFTN         TO  WRK-TAHOFTN-G
           ADD     WRK-KOHSKJFTN       TO  WRK-KOHSKJFTN-G
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
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
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
                 MOVE  "tbl_syunou"       TO  MCP-TABLE
                 MOVE  WRK-PATH           TO  MCP-PATHNAME
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
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
             CALL  "ORCDBMAIN"        USING
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM                  TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                 TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                 TO  MCPDATA-REC
           MOVE  "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_hknjainf"             TO  MCP-TABLE
           MOVE    WRK-PATH                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    WRK-PATH               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
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
           MOVE  "DBFETCH"              TO  MCP-FUNC
           MOVE  "tbl_hknjainf"         TO  MCP-TABLE
           MOVE  WRK-PATH               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    KEY2を使用しており、国保でない時は、読み直す
             IF    (HKNJA-HKNNUM  NOT  =  "060" AND "067")
             AND   (WRK-PATH           =  "key2")
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
           MOVE  "tbl_syskanri"          TO  MCP-TABLE
           MOVE  "key10"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD (1:6)
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
