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
       PROGRAM-ID.     SEIKYU4403.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 大分・子ども医療費ＣＳＶ
      *                      (183,283)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/05/10    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev   修正者   日付        内容
      *  01.00.01     吉川     14/02/05    診療年月は月遅れに関わらず記載
      *  01.00.02     吉川     14/10/28    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.03     吉川     15/10/14    自己負担額欄は１円単位四捨五入
      *                                    医療機関編集情報への開設者名
      *                                    追加対応
      *  01.00.04     井上     19/02/05    修正履歴コメント削除
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
      *    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI4401.INC".
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
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
           03  IDX                   PIC 9(04).
           03  IDXX                  PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX2                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDX4                  PIC 9(04).
           03  IDY                   PIC 9(04).
           03  IDZ                   PIC 9(04).
           03  IDW                   PIC 9(04).
           03  IDW1                  PIC 9(04).
           03  IDW2                  PIC 9(04).
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
           03  WRK-WYMD.
               05  WRK-WGEN        PIC 9(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
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
           03  WRK-TAISYOYM        PIC X(05).
           03  WRK-ITBFTN          PIC 9(08).
           03  WRK-GITBFTN         PIC 9(09).
           03  WRK-NISSU           PIC 9(09).
           03  WRK-CITYRENNUM      PIC 9(03).
           03  WRK-RENNUM          PIC 9(03).
           03  WRK-KENSU           PIC 9(04).
           03  WRK-GKENSU          PIC 9(04).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
      *    ファイル名称用エリア
           03  FILE-NAME             PIC X(512).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-FILENAME          PIC  X(40).
      *    データ編集
           03  WRK-HENSHU.
               05  WRK-RATE          PIC 9(02).
               05  WRK-MOJIHEN       PIC 9(10).
      *
           03  WRK-REC               PIC X(500).
           03  WRK-REC-LENGTH        PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA          PIC X(200).
               05  WRK-NUM           PIC ZZZZZZZZZZ.999.
               05  WRK-NUM9          PIC ZZZZZZZZZ9.999.
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-OUT     PIC 9(06).
      *
      *    TMPファイルデータ一時退避領域
       01  MF101-REC.
           COPY  "SEI4401.INC"
                   REPLACING  //MF100//
                   BY         //MF101//.
      *
       01  SUM-AREA.
           03  SUM-GOKEI      OCCURS 50.
               05  SUM-KEY.
                   07  SUM-CITYRENNUM  PIC 9(03).
                   07  SUM-FTNJANUM    PIC X(08).
               05  SUM-GKENSU          PIC 9(03).
               05  SUM-GITBFTN         PIC 9(09).
      *
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
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)  VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)  VALUE   X"0D".
      *    EOFコード
           03  WRK-EOF                 PIC X(01)  VALUE   X"1A".
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
           COPY    "MCPAREA".
      *
      *    日付変換サブ
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
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
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
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM        TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"                   TO  SJOBKANRI-MODE
           INITIALIZE                          JOBKANRI-REC
           MOVE    SPA-HOSPNUM             TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID          TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID        TO  JOB-SHELLID
           MOVE    "SEIKYU4403"            TO  JOB-PGID
           MOVE    "子ども医療費ＣＳＶ"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM      TO  WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD                TO  WRK-LASTYMD
      *
           MOVE    LNK-PRTKANRI-SRYYM      TO  WRK-SYMD (1:6)
           MOVE    "01"                    TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1        TO  WRK-HENYMDG1
           MOVE    WRK-HENYMDG(1:16)       TO  WRK-SRYYMWH
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                    TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
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
           MOVE  "1001"                    TO  SYS-1001-KANRICD
           MOVE  "*"                       TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                           TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1001-STYUKYMD(7:2)
                                               SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM               TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC              TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC             TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU4403 SYS 1001 ERR ***"
             MOVE  1                       TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE    "1002"                  TO  SYS-1002-KANRICD
           MOVE    "*"                     TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                           TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1002-STYUKYMD(7:2)
                                               SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM             TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC            TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC           TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU4403 SYS 1002 ERR ***"
             MOVE    1                     TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME       TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME    TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS           TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL            TO  WRK-HOSPTEL
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
      *        ＣＳＶファイル出力
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
           IF  RECE02-NYUGAIKBN  =  "1"
             EVALUATE  RECE02-ZAITAKU
               WHEN  "2"
                 MOVE      1                   TO  MF100-KOGAKU
               WHEN  "3"
                 MOVE      2                   TO  MF100-KOGAKU
               WHEN  "4"
                 MOVE      3                   TO  MF100-KOGAKU
             END-EVALUATE
             IF  FLG-TEKIYOD  =  1
               MOVE      4                     TO  MF100-KOGAKU
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
           ADD    1                            TO  CNT-MF100
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
           DISPLAY "2002-PRINT START"
      *
      *    件数計算
           PERFORM 20021-KENSUCNT-SEC
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           INITIALIZE                  ORCSFILESVAREA
           MOVE    1            TO   WRK-CITYRENNUM
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
           
      *      ＣＳＶ<ヘッダーレコード>処理
             PERFORM 310-HEAD-HEN-SEC
      *
      *      ＣＳＶ<データレコード>処理
             PERFORM  330-CSV-HEN-SEC
      *
      *      負担者番号が変わった場合は、CSV名用の連番をリセット
             IF      KEY-NEW     NOT =   KEY-OLD
                 MOVE    1             TO  WRK-CITYRENNUM
             END-IF
      *
      *      ファイル保存情報更新
             PERFORM 3001-FILE-INFO-INSERT-SEC
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
      *    件数カウント処理
      *****************************************************************
       20021-KENSUCNT-SEC                SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO                     TO  FLG-PRINT
                                                 WRK-KENSU
                                                 WRK-GITBFTN
           MOVE     1                        TO  IDY
                                                 WRK-CITYRENNUM
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
             MOVE    KEY-NEW     TO     KEY-OLD
             PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       99
                   OR      KEY-NEW     NOT =   KEY-OLD
                   OR      FLG-PRINT       =   1
      *
               ADD    1                      TO  WRK-KENSU
               COMPUTE  WRK-ITBFTN = MF100-KOHFTNGK + MF100-KOHITBFTN
                                   - MF100-FTNMONEY
               ADD    WRK-ITBFTN             TO  WRK-GITBFTN
      *
      *        次データ読込
               PERFORM  900-TMP-READ-SEC
      * 
      *        負担者番号が変わった場合は、CSV名用の連番をリセット
               IF  KEY-NEW  NOT =  KEY-OLD
                   MOVE    1                 TO  WRK-CITYRENNUM
               END-IF
      *
             END-PERFORM
      *
      *      99行を超える場合のファイル名用の連番
             IF  IDX  >  99
             AND KEY-NEW  =  KEY-OLD
             AND FLG-PRINT  NOT =  1
                 ADD    1                    TO  WRK-CITYRENNUM
             END-IF
      *
             DISPLAY "KEY-OLD         : " KEY-OLD
             DISPLAY "WRK-CITYRENNUM  : " WRK-CITYRENNUM
             DISPLAY "IDY             : " IDY
      *      負担者ごとの件数と負担額合計をワークに保存
             MOVE    WRK-CITYRENNUM          TO  SUM-CITYRENNUM(IDY)
             MOVE    KEY-OLD                 TO  SUM-FTNJANUM(IDY)
             MOVE    WRK-KENSU               TO  SUM-GKENSU(IDY)
             MOVE    WRK-GITBFTN             TO  SUM-GITBFTN(IDY)
      *
             MOVE    ZERO                    TO  WRK-KENSU
                                                 WRK-GITBFTN
             ADD     1                       TO  IDY
      *
           END-PERFORM
      *
           CLOSE  MF100-FILE
      *
           .
       20021-KENSUCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ<ヘッダーレコード>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                            WRK-REC
                                                 WRK-REC-LENGTH
      *
           ADD     1                         TO  CNT-PAGE
      *
      *    １．レコード識別情報
           MOVE    "HD"                      TO  WRK-DATA
           MOVE    2                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ２．対象年月
      *    資料には「対象年月は請求年月の前の月」とあったが
      *    おそらく診療年月を指すので、パラメタの診療年月から取得する
           MOVE    LNK-PRTKANRI-SRYYM(1:6)   TO  WRK-SYMD(1:6)
           MOVE    "01"                      TO  WRK-SYMD(7:2)
           PERFORM   31011-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY1-GENKIGO
             WHEN  "M"
               MOVE    1                     TO  WRK-WGEN
             WHEN  "T"
               MOVE    2                     TO  WRK-WGEN
             WHEN  "S"
               MOVE    3                     TO  WRK-WGEN
             WHEN  "H"
               MOVE    4                     TO  WRK-WGEN
           END-EVALUATE
           MOVE    LNK-DAY1-WYMD(2:2)        TO  WRK-WYY
           MOVE    LNK-DAY1-WYMD(4:2)        TO  WRK-WMM
           MOVE    WRK-WYMD(1:5)             TO  WRK-DATA
                                                 WRK-TAISYOYM
           MOVE    5                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ３．医療機関コード
           MOVE    SYS-1001-HOSPCD           TO  WRK-DATA
           MOVE    7                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ４．医療機関名称
           MOVE    WRK-HOSPNAME              TO  WRK-DATA
           MOVE    40                        TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ５．所在地
           MOVE    WRK-HOSPADRS              TO  WRK-DATA
           MOVE    180                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ６．開設者氏名
           MOVE    WRK-KAISETUNAME           TO  WRK-DATA
           MOVE    30                        TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ７．請求日付
           MOVE    LNK-PRTKANRI-SKYYMD       TO  WRK-SYMD
           PERFORM   31011-SEIWA-HEN-SEC
           EVALUATE  LNK-DAY1-GENKIGO
             WHEN  "M"
               MOVE    1                     TO  WRK-WGEN
             WHEN  "T"
               MOVE    2                     TO  WRK-WGEN
             WHEN  "S"
               MOVE    3                     TO  WRK-WGEN
             WHEN  "H"
               MOVE    4                     TO  WRK-WGEN
           END-EVALUATE
           MOVE    LNK-DAY1-WYMD(2:2)        TO  WRK-WYY
           MOVE    LNK-DAY1-WYMD(4:2)        TO  WRK-WMM
           MOVE    LNK-DAY1-WYMD(6:2)        TO  WRK-WDD
           MOVE    WRK-WYMD(1:7)             TO  WRK-DATA
           MOVE    7                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    ８．公費負担者番号
           MOVE    MF100-FTNJANUM            TO  WRK-DATA
           MOVE    8                         TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           DISPLAY "MF100-FTNJANUM  : " MF100-FTNJANUM
           DISPLAY "WRK-CITYRENNUM  : " WRK-CITYRENNUM
      *    合計場所特定
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >   50
                    OR       SUM-FTNJANUM(IDX1)  =  SPACE
             DISPLAY "IDX1            : " IDX1
             DISPLAY "SUM-KEY         : " SUM-KEY(IDX1)
             IF  MF100-FTNJANUM  =  SUM-FTNJANUM(IDX1)
             AND WRK-CITYRENNUM  =  SUM-CITYRENNUM(IDX1)
               MOVE    SUM-GKENSU(IDX1)      TO  WRK-KENSU
               MOVE    SUM-GITBFTN(IDX1)     TO  WRK-GITBFTN
               MOVE    50                    TO  IDX1
             END-IF
           END-PERFORM
      *
      *    ９．件数
           MOVE    WRK-KENSU                 TO  WRK-NUM
           MOVE    2                         TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    １０．市町村負担額合計
           MOVE    WRK-GITBFTN               TO  WRK-NUM
           MOVE    8                         TO  WRK-NAGASA
           MOVE    1                         TO  WRK-END
           PERFORM 5005-NUM-HENSYU-SEC
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
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ編集<データレコード>処理
      *****************************************************************
       330-CSV-HEN-SEC      SECTION.
      *
           MOVE    ZERO                        TO  WRK-RENNUM
      *
           MOVE    KEY-NEW     TO     KEY-OLD
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       99
                   OR      KEY-NEW     NOT =   KEY-OLD
                   OR      FLG-PRINT       =   1
      *
             INITIALIZE                            WRK-REC
                                                   WRK-REC-LENGTH
      *
      *      １．レコード識別情報
             MOVE    "DT"                      TO  WRK-DATA
             MOVE    2                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *      ２．データ行番号
             ADD     1                         TO  WRK-RENNUM
             MOVE    WRK-RENNUM                TO  WRK-NUM
             MOVE    2                         TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *
      *      ３．受給資格者番号
             MOVE    MF100-KOHJKYSNUM          TO  WRK-DATA
             MOVE    7                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      ４．氏名
             MOVE    MF100-NAME                TO  WRK-DATA
             MOVE    40                        TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      ５・生年月日
             MOVE    MF100-BIRTHDAY            TO  WRK-SYMD
             PERFORM   31011-SEIWA-HEN-SEC
             EVALUATE  LNK-DAY1-GENKIGO
               WHEN  "M"
                 MOVE    1                     TO  WRK-WGEN
               WHEN  "T"
                 MOVE    2                     TO  WRK-WGEN
               WHEN  "S"
                 MOVE    3                     TO  WRK-WGEN
               WHEN  "H"
                 MOVE    4                     TO  WRK-WGEN
             END-EVALUATE
             MOVE    LNK-DAY1-WYMD(2:2)        TO  WRK-WYY
             MOVE    LNK-DAY1-WYMD(4:2)        TO  WRK-WMM
             MOVE    LNK-DAY1-WYMD(6:2)        TO  WRK-WDD
             MOVE    WRK-WYMD(1:7)             TO  WRK-DATA
             MOVE    7                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      ６．性別
             MOVE    MF100-SEX                 TO  WRK-DATA
             MOVE    1                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      ７．保険区分
             MOVE    MF100-TEISYUTUSAKI        TO  WRK-DATA
             MOVE    1                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      ８．診療区分
             MOVE    MF100-NYUGAIKBN           TO  WRK-DATA
             MOVE    1                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      ９．給付割合
             EVALUATE  MF100-KYURATE
               WHEN  10
                 MOVE    1                     TO  WRK-NUM9
               WHEN  20
                 MOVE    2                     TO  WRK-NUM9
               WHEN  30
                 MOVE    3                     TO  WRK-NUM9
               WHEN  OTHER
                 MOVE    ZERO                  TO  WRK-NUM9
             END-EVALUATE
             MOVE    2                         TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
      *      
      *      １０．日数
             MOVE    MF100-JNISSU              TO  WRK-NUM9
             MOVE    2                         TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
      *      
      *      １１．法定点数
             MOVE    MF100-TOTALTEN            TO  WRK-NUM9
             MOVE    7                         TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
      *      
      *      １２．一部負担額（Ａ）
             MOVE    MF100-KOHFTNGK            TO  WRK-NUM9
             MOVE    6                         TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
      *      
      *      １３．自己負担額（Ｂ）
             MOVE    MF100-KOHITBFTN           TO  WRK-NUM9
             MOVE    6                         TO  WRK-NAGASA
             PERFORM 5006-NUM-HENSYU-SEC
      *      
      *      佐伯市と姫島村で小中学の外来は記載しない
             IF  (MF100-FTNJANUM(6:2)  =  CONS-HIMESHIMA-CITYNUM
                                       OR CONS-SAIKI-CITYNUM)
             AND (MF100-NYUGAIKBN  =  "2")
             AND (MF100-MISYUGAKU  NOT =  "1")
               MOVE    ZERO                    TO  MF100-FTNMONEY
               MOVE    ZERO                    TO  MF100-NNISSU
             END-IF
      *
      *      １４．一部自己負担日数
             MOVE    MF100-NNISSU              TO  WRK-NUM
             MOVE    2                         TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *      
      *      １５．一部自己負担額（Ｃ）
             MOVE    MF100-FTNMONEY            TO  WRK-NUM
             MOVE    6                         TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *      
      *      １６．市町村負担額（Ａ＋Ｂ−Ｃ）
             COMPUTE  WRK-ITBFTN = MF100-KOHFTNGK + MF100-KOHITBFTN
                                 - MF100-FTNMONEY
             MOVE    WRK-ITBFTN                TO  WRK-NUM
             MOVE    6                         TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *      
      *      １７．診療年月
               MOVE    MF100-SRYYM             TO  WRK-SYMD (1:6)
               MOVE    "01"                    TO  WRK-SYMD (7:2)
               PERFORM   31011-SEIWA-HEN-SEC
               EVALUATE  LNK-DAY1-GENKIGO
                 WHEN  "M"
                   MOVE    1                   TO  WRK-WGEN
                 WHEN  "T"
                   MOVE    2                   TO  WRK-WGEN
                 WHEN  "S"
                   MOVE    3                   TO  WRK-WGEN
                 WHEN  "H"
                   MOVE    4                   TO  WRK-WGEN
               END-EVALUATE
               MOVE    LNK-DAY1-WYMD(2:2)      TO  WRK-WYY
               MOVE    LNK-DAY1-WYMD(4:2)      TO  WRK-WMM
               MOVE    WRK-WYMD(1:5)           TO  WRK-DATA
             MOVE    5                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      
      *      １８．保険者番号
             MOVE    MF100-HKNJANUM            TO  WRK-DATA
             MOVE    8                         TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *      １９．適用区分
             MOVE    MF100-KOGAKU              TO  WRK-NUM
             MOVE    1                         TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *      
      *      ２０．備考
             MOVE    SPACE                     TO  WRK-DATA
             MOVE    128                       TO  WRK-NAGASA
             MOVE    1                         TO  WRK-END
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *      改行コード編集
             STRING  WRK-REC(1:WRK-REC-LENGTH)     DELIMITED  BY  SIZE
                     WRK-KAIGYO                    DELIMITED  BY  SIZE
                     INTO                          WRK-REC
             END-STRING
      *
             PERFORM 500-CSVINFO-SEC
      *
             ADD    1                          TO  WRK-GKENSU
      *
      *      一時ファイル読込
             PERFORM  900-TMP-READ-SEC
      *
           END-PERFORM
      *
      *    99行を超える場合のファイル名用の連番
           IF  IDX  >  99
           AND KEY-NEW  =  KEY-OLD
           AND FLG-PRINT  NOT =  1
             ADD    1                          TO  WRK-CITYRENNUM
           END-IF
      *
           .
       330-CSV-HEN-SEC-EXT.
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
           MOVE    CNT-PAGE            TO  SCSVINFO-RENNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  SCSVINFO-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SCSVINFO-SKYYMD
           MOVE    WRK-SCSVINFO-SEQ-NO TO  SCSVINFO-SEQ-NO
           MOVE    WRK-SCSVINFO-NYUGAIKBN
                                       TO  SCSVINFO-NYUGAIKBN
           MOVE    WRK-SCSVINFO-PTID   TO  SCSVINFO-PTID
           MOVE    WRK-REC             TO  SCSVINFO-CSVDATA
           DISPLAY "WRK-REC =" WRK-REC
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
      *      ジョブ終了処理
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
           MOVE   1                  TO  FLG-END
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
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
      *    出力レコード処理（数字）ZZZZZZZZZZ.999の編集
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
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5006-NUM-HENSYU-SEC          SECTION.
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
                   IF      WRK-NUM9 (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM9(IDW:IDZ)  DELIMITED  BY  SIZE
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
           MOVE   WRK-GKENSU        TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU4403 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU4403 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU4403 END ***"
      *
      *    ファイル保存情報更新
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
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           MOVE    "I"                      TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID         TO  SFILESV-TBL-KEY
           MOVE    LNK-PRTKANRI-SHELLID     TO  SFILESV-SHELLID (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  SFILESV-SHORI-RENNUM (1)
           MOVE    CNT-PAGE                 TO  SFILESV-RENNUM (1)
           MOVE    LNK-PRTKANRI-SRYYM       TO  SFILESV-SRYYM (1)
           MOVE    LNK-PRTKANRI-SKYYMD      TO  SFILESV-SKYYMD (1)
      *    ファイル名は「対象年月_医療機関コード_公費負担者番号_
      *                  公費負担者番号毎の3桁連番.csv」
           STRING  WRK-TAISYOYM                 DELIMITED  BY  SIZE
                   "_"                          DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD              DELIMITED  BY  SIZE
                   "_"                          DELIMITED  BY  SIZE
                   KEY-OLD                      DELIMITED  BY  SPACE
                   "_"                          DELIMITED  BY  SIZE
                   WRK-CITYRENNUM               DELIMITED  BY  SIZE
                   ".csv"                       DELIMITED  BY  SIZE
                   INTO                         WRK-FILENAME
           END-STRING
           MOVE    WRK-FILENAME             TO  SFILESV-TO-DATA (1)
           MOVE    WRK-KENSU                TO  SFILESV-CNT-ALL (1)
           MOVE    "子ども医療費ＣＳＶ"
                                            TO  SFILESV-TITLE (1)
           IF      WRK-KENSU   >   ZERO
               MOVE    "1"                  TO  SFILESV-DATA-TYPE(1)
           END-IF 
           MOVE    WRK-RECEERR              TO  SFILESV-ERR-MSG (1)
           IF      WRK-RECEERR NOT =   SPACE
               MOVE    "1"                  TO  SFILESV-ERR-CODE (1)
           END-IF
           MOVE    "1"                      TO  SFILESV-CODE-TYPE (1)
           CALL    "ORCSFILESV"                 USING
                                                ORCSFILESVAREA
                                                SPA-AREA
      *
           .
       3001-FILE-INFO-INSERT-EXT.
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBFETCH"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC  TO  BTPARA-REC
               MOVE    ZERO         TO  FLG-BTPARA
           ELSE
               MOVE    1            TO  FLG-BTPARA
               INITIALIZE               BTPARA-REC
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
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
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
