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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000L200.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別総括表 
      *  管理者            : 
      *  作成日付    作業者       記述
      *  03/12/17    @@@@-有村    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/15  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    12/03/09  コメント削除、改行コード変更
      *                                     CSVファイル作成機能追加
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    門間         18/05/18  公費単独の集計を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    社保用ファイル
           SELECT  L200TMP-FILE    ASSIGN    L200TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  L200TMP-KEY
                                   FILE    STATUS  IS  STS-L200TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    社保一時ファイル
       FD  L200TMP-FILE.
           COPY    "A00000L200TMP.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    社保一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //L200TP//.
      *
           COPY    "A00000L200V03.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECE            PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-L200TMP         PIC X(02).
      ***** 04/02/04 INS
           03  STS-SETTEI          PIC X(02).
      *****
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-HKNJAINF             PIC 9(01).
           03  FLG-SYUNOU               PIC 9(01).
           03  FLG-PASS                 PIC 9(01).
           03  FLG-PRINT                PIC 9(01).
           03  FLG-ERR                  PIC 9(01).
           03  FLG-HKNNUM               PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-HEIYOU               PIC 9(01).
           03  FLG-END                  PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
           03  FLG-PTKOHINF             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-DATA            PIC 9(06).
           03  CNT-READ            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                PIC 9(02).
           03  IDX1               PIC 9(04).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(04).
           03  IDX5               PIC 9(04).
           03  IDX6               PIC 9(04).
           03  IDA-G.
               05  IDA            PIC 9(02).
           03  IDB-G.
               05  IDB            PIC 9(02).
           03  IDC-G.
               05  IDC            PIC 9(04).
           03  IDD-G.
               05  IDD            PIC 9(04).
           03  IDY                PIC 9(04).
           03  IDZ                PIC 9(04).
           03  IDM                PIC 9(04).
           03  IDX-MAX            PIC 9(04).
      *
           03  IDX-ST            PIC 9(03).
           03  IDX-ED            PIC 9(03).
           03  IDXP              PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM       PIC 9(02).
      *
      *    画面入力パラメタ
      *    対象診療年月
           03  WRK-PARA-SRYYM        PIC X(06).
      *    入外区分
           03  WRK-PARA-NYUGAIKBN    PIC X(01).
      *    保険者番号
           03  WRK-PARA-HKNJANUM     PIC X(08).
      *    負担者番号
           03  WRK-PARA-FTNJANUM     PIC X(20).
      *    本人・家族
           03  WRK-PARA-HONKZKKBN    PIC X(01).
      *    診療科
           03  WRK-PARA-SRYKA        PIC X(02).
      *    印字順序
           03  WRK-PARA-SYUTURYOKU   PIC X(01).
      *    出力区分
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD      PIC X(08).
           03  WRK-PARA-PRTSYMD      PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY         PIC 9(04).
               05  WRK-SRYMM         PIC 9(02).
               05  WRK-SRYDD         PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SRYYM             PIC X(22).
           03  WRK-SEIYMD            PIC X(22).
           03  WRK-RECEERR           PIC X(200).
           03  WRK-ERR-MSG           PIC X(60).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-PTID              PIC 9(10).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
           03  WRK-KOHI-TBL.
               05  WRK-KOH1-KOHNUM   PIC X(03).
               05  WRK-KOH1-FTNJANUM PIC X(20).
      *
               05  WRK-KOH2-KOHNUM   PIC X(03).
               05  WRK-KOH2-FTNJANUM PIC X(20).
      *
               05  WRK-KOH1-JKYSNUM  PIC X(20).
               05  WRK-KOH2-JKYSNUM  PIC X(20).
      *
           03  WRK-HKNJANUM          PIC X(08).
           03  WRK-HKNJANAME         PIC X(40).
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-PATH2             PIC X(64).
           03  WRK-CHK-HKNJANUM      PIC X(08).
           03  WRK-CHK-FTNJANUM      PIC X(20).
      *    ファイル名称用エリア
           03  FILE-NAME             PIC X(512).
           03  SETTEI-PARA           PIC 9(01).
      *
           03  WRK-GOKEI-AREA.
               05    WRK-GNISSU      PIC 9(04).
               05    WRK-GTENSU      PIC 9(09).
               05    WRK-GITBFTN     PIC 9(09).
               05    WRK-GSNISSU     PIC 9(09).
               05    WRK-GKINGK      PIC 9(09).
               05    WRK-GFTNGK      PIC 9(09).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZ9.
           03  WRK-Z4                PIC ZZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z92               PIC ZZZZZZZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11               PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13               PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14               PIC ZZ,ZZZ,ZZZ,ZZZ.
           03  WRK-Z4-2              PIC ZZZZ.
           03  WRK-Z6-2              PIC ZZZZZZ.
           03  WRK-Z7-2              PIC ZZZZZZZ.
           03  WRK-Z9-2              PIC ZZZZZZZZZ.
           03  WRK-Z10-2             PIC ZZZZZZZZZZ.
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX                         PIC 9(03).
           03  SUM-SRYDR-TBL       OCCURS 100.
               05  SUM-SRYKA                        PIC X(02).
               05  SUM-SRYKANAME                    PIC X(24).
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-NYUGAIKBN          PIC  X(01).
               05  KEY-N-SRYKA              PIC  X(02).
           03  KEY-OLD.
               05  KEY-O-NYUGAIKBN          PIC  X(01).
               05  KEY-O-SRYKA              PIC  X(02).
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE         PIC 9(03)     VALUE  100.
           03  WRK-ITEM              PIC X(100)    VALUE  SPACE.
           03  WRK-CSVDATA           PIC X(20000)  VALUE  SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-L001  PIC X(50)
                            VALUE  "連番".
               07  CSV-HEAD-L002  PIC X(50)
                            VALUE  "患者番号".
               07  CSV-HEAD-L003  PIC X(50)
                            VALUE  "患者氏名".
               07  CSV-HEAD-L004  PIC X(50)
                            VALUE  "保険者番号".
               07  CSV-HEAD-L005  PIC X(50)
                            VALUE  "保険者名称".
               07  CSV-HEAD-L006  PIC X(50)
                            VALUE  "公費（１）負担者番号".
               07  CSV-HEAD-L007  PIC X(50)
                            VALUE  "公費（２）負担者番号".
               07  CSV-HEAD-L008  PIC X(50)
                            VALUE  "生年月日".
               07  CSV-HEAD-L009  PIC X(50)
                            VALUE  "年齢".
               07  CSV-HEAD-L010  PIC X(50)
                            VALUE  "本／家".
               07  CSV-HEAD-L011  PIC X(50)
                            VALUE  "負割".
               07  CSV-HEAD-L012  PIC X(50)
                            VALUE  "診療年月".
               07  CSV-HEAD-L013  PIC X(50)
                            VALUE  "療養の給付＿日数".
               07  CSV-HEAD-L014  PIC X(50)
                            VALUE  "療養の給付＿点数".
               07  CSV-HEAD-L015  PIC X(50)
                            VALUE  "療養の給付＿負担額".
               07  CSV-HEAD-L016  PIC X(50)
                            VALUE  "食事療養＿日数".
               07  CSV-HEAD-L017  PIC X(50)
                            VALUE  "食事療養＿金額".
               07  CSV-HEAD-L018  PIC X(50)
                            VALUE  "食事療養＿標準負担額".
      *
             05  CSV-HEAD-R   REDEFINES   CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM        PIC X(50)  OCCURS  18.
      *
             05  CSV-HEAD-MAX         PIC 9(05)  VALUE  18.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *
      *    請求管理マスタ
           COPY    "CPRCF010.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    FLG-ERR    =    ZERO
             PERFORM 200-MAIN-SEC
           ELSE
             PERFORM 200-ERR-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
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
                                               RECEERR
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-FTNJANUM
                                               WRK-PARA-HONKZKKBN
                                               WRK-PARA-SRYKA
                                               WRK-PARA-SYUTURYOKU
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000L200"      TO  JOB-PGID
           MOVE    "保険別患者台帳"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *+
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AL200TP"                TO  L200TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID      TO  L200TP-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO  L200TP-HOSPNUM
      *
           MOVE    L200TP-BASENAME          TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  L200TP-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
           MOVE    WRK-PARA-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM 
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD               TO    WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM     TO    WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"               TO    WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE     "%"           TO      SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM    TO      SYS-1001-HOSPNUM
           MOVE  WRK-PARA-PRTSYMD TO      SYS-1001-STYUKYMD
           MOVE  WRK-PARA-PRTLYMD TO      SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF     FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO    SYS-1001-REC
                  IF      SYS-1001-HOSPNAME  =  LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-HOSPNAME
                  END-IF
                  IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-KAISETUNAME
                  END-IF    
           ELSE
                  MOVE    "医療機関情報が取得できませんでした"
                                          TO    WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE     SPACE         TO        SYS-1031-REC
           INITIALIZE                       SYS-1031-REC
           MOVE     SPA-HOSPNUM   TO        SYS-1031-HOSPNUM
           MOVE  WRK-PARA-PRTSYMD TO        SYS-1031-STYUKYMD
           MOVE  WRK-PARA-PRTLYMD TO        SYS-1031-EDYUKYMD
           MOVE     "1031"        TO        SYS-1031-KANRICD
           MOVE     "%"           TO        SYS-1031-KBNCD
           MOVE     SYS-1031-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI        =   ZERO
                    MOVE    MCPDATA-REC TO  SYS-1031-REC
           ELSE
                    INITIALIZE              SYS-1031-REC
           END-IF
      *
      *    診療科情報取得
             MOVE    "1005"                    TO   SYS-1005-KANRICD
             MOVE     SPA-HOSPNUM              TO   SYS-1005-HOSPNUM
             MOVE    SPA-SRYYMD             TO  SYS-1005-STYUKYMD
                                                SYS-1005-EDYUKYMD
             MOVE    WRK-PARA-PRTLYMD  TO    WRK-SYMD
             PERFORM 820-SYSKANRI-DBSELECT-SEC
             MOVE   1        TO     IDX1 
             PERFORM  UNTIL  FLG-SYSKANRI3 = 1
                  IF   FLG-SYSKANRI3 = ZERO
                        MOVE SYS-1005-KBNCD      TO SUM-SRYKA(IDX1)
                        MOVE SYS-1005-SRYKANAME1 TO SUM-SRYKANAME(IDX1)
                        ADD  1        TO     IDX1 
                  END-IF  
                  PERFORM 820-SYSKANRI-DBFETCH-SEC
             END-PERFORM
             PERFORM 820-SYSKANRI-DBCLOSE-SEC
             SUBTRACT  1        FROM IDX1  
             MOVE  IDX1         TO   SUM-SRYKAMAX
      *
           INITIALIZE                      ORCSPRGOPTIONAREA
           MOVE    "A00000L200"        TO  ORCSPRGOPTION-PRGID
           MOVE    "SETTEI"            TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE  ORCSPRGOPTION-OT-PARA(1:1)  TO  SETTEI-PARA
      *
      *    システム日付セット
           MOVE  LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SEIYMD
      *
      *    パラメタチェック処理
           PERFORM  110-PARA-CHK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタチェック処理
      *****************************************************************
       110-PARA-CHK-SEC                SECTION.
      *    
           MOVE    ZERO    TO    FLG-ERR
      *
      *    入外区分
           IF    WRK-PARA-NYUGAIKBN  NOT  =  SPACE AND "1" AND "2"
             MOVE    1     TO    FLG-ERR
           END-IF
      *
      *    本人家族区分
           IF    WRK-PARA-HONKZKKBN  NOT  =  SPACE AND "1" AND "2"
             MOVE    2     TO    FLG-ERR
           END-IF
      *
      *    診療科
           IF    SYS-1001-HOSPSBT1    =    ZERO   AND
                 WRK-PARA-SRYKA  NOT  =    SPACE
             MOVE    3     TO    FLG-ERR
           END-IF
      *
      *    印字順序
           IF    WRK-PARA-SYUTURYOKU  NOT =  SPACE AND "0"
                                      AND      "1" AND "2"
             MOVE    4     TO    FLG-ERR
           END-IF
      *
           .
       110-PARA-CHK-SECEXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データ集計
           PERFORM  2001-SYUKEI-SEC
      *
      *    印刷処理
           IF    CNT-DATA    >    ZERO
             PERFORM  2002-PRINT-SEC
           ELSE
             MOVE      5    TO    FLG-ERR
             PERFORM  200-ERR-SEC
           END-IF
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       200-ERR-SEC                SECTION.
      *
           INITIALIZE    L200
      *
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    L200-PAGE
      *
           EVALUATE  FLG-ERR
           WHEN 1
             MOVE 
              "入外区分の入力エラーです。もう一度入力してください。"
                  TO    L200-ERR-MSG
           WHEN 2
             MOVE 
           "本人家族区分の入力エラーです。もう一度入力してください。"
                  TO    L200-ERR-MSG
           WHEN 3
             MOVE 
              "診療科の入力エラーです。もう一度入力してください。"
                  TO    L200-ERR-MSG
           WHEN 4
             MOVE 
              "印字順序の入力エラーです。もう一度入力してください。"
                  TO    L200-ERR-MSG
           WHEN 5
             MOVE 
              "対象データがありません。もう一度入力してください。"
                  TO    L200-ERR-MSG
           WHEN OTHER
             MOVE  "入力エラーです。もう一度入力してください。"
                  TO   L200-ERR-MSG
           END-EVALUATE
      *
           MOVE    "診療年月"        TO    L200-NAME(1)
           MOVE    "入外区分"        TO    L200-NAME(2)
           MOVE    "保険者番号"      TO    L200-NAME(3)
           MOVE    "負担者番号"      TO    L200-NAME(4)
           MOVE    "本人家族区分"    TO    L200-NAME(5)
           MOVE    "診療科"          TO    L200-NAME(6)
           MOVE    "印字順序"        TO    L200-NAME(7)
      *
           MOVE    WRK-PARA-SRYYM         TO    L200-HKNJANUM(1)
           MOVE    WRK-PARA-NYUGAIKBN     TO    L200-HKNJANUM(2)
           MOVE    WRK-PARA-HKNJANUM      TO    L200-HKNJANUM(3)
           MOVE    WRK-PARA-FTNJANUM      TO    L200-HKNJANUM(4)
           MOVE    WRK-PARA-HONKZKKBN     TO    L200-HKNJANUM(5)
           MOVE    WRK-PARA-SRYKA         TO    L200-HKNJANUM(6)
           MOVE    WRK-PARA-SYUTURYOKU    TO    L200-HKNJANUM(7)
      *
           PERFORM  330-PRINT-OUT-SEC
      *
           .
       200-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
           OPEN     OUTPUT  L200TMP-FILE
           CLOSE            L200TMP-FILE
           OPEN     I-O     L200TMP-FILE
      *
      *    データ抽出
           PERFORM  2001-HKN-SYUKEI-SEC
      *
           CLOSE   L200TMP-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ抽出処理
      *****************************************************************
       2001-HKN-SYUKEI-SEC                SECTION.
      *
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL    IDZ  >  3
             INITIALIZE        WRK-PATH
             EVALUATE  IDZ
               WHEN  1
                 MOVE    "SEIKYU-KEY3"   TO    WRK-PATH
               WHEN  2
                 MOVE    "SEIKYU-KEY4"   TO    WRK-PATH
               WHEN  3
                 MOVE    "SEIKYU-KEY60"    TO    WRK-PATH
             END-EVALUATE
      *
             PERFORM  900-SEIKYU-SELECT-SEC
      *
             PERFORM  UNTIL  FLG-END   =   1
      *
      *    データ設定
               PERFORM 2001-DATA-SET1-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
             END-PERFORM
      *
             PERFORM  900-SEIKYU-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       2001-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ設定処理
      *****************************************************************
       2001-DATA-SET1-SEC                SECTION.
      *
           INITIALIZE      L200TMP-REC
      *    公費負担者番号設定
           PERFORM  2001-FTNJANUM-SEC
      *
      *    キー設定
           MOVE    RECE10-NYUGAIKBN        TO    L200TMP-NYUGAIKBN-KEY
           MOVE    RECE10-RECEKA           TO    L200TMP-SRYKA-KEY
      *
           EVALUATE  WRK-PARA-SYUTURYOKU
           WHEN "0"
             MOVE    PTINF-KANANAME        TO    L200TMP-SORT-KEY
           WHEN "1"
             MOVE    RECE10-HKNJANUM       TO    L200TMP-SORT-KEY
           WHEN "2"
             MOVE    L200TMP-FTNJANUM1     TO    L200TMP-SORT-KEY
           WHEN OTHER
             MOVE    RECE10-PTNUM          TO    L200TMP-SORT-KEY
           END-EVALUATE
      *
      *    請求管理情報のキーをそのまま設定
           MOVE    RECE10-KEY              TO    L200TMP-KEY2
      *
      *    患者番号
           MOVE    RECE10-PTNUM            TO    L200TMP-PTNUM
      *
      *    年齢
           MOVE    RECE10-AGE              TO    L200TMP-AGE
      *
      *    在宅区分
           MOVE    RECE10-ZAITAKU          TO    L200TMP-ZAITAKU
      *
      *    保険番号
           MOVE    RECE10-HKNNUM           TO    L200TMP-HKNNUM
      *
      *    本人家族区分
           MOVE    RECE10-HONKZKKBN        TO    L200TMP-HONKZKKBN
      *
      *    老人保険番号
           MOVE    RECE10-RJNHKNNUM        TO    L200TMP-RJNHKNNUM
      *
      *    氏名
           MOVE    PTINF-NAME              TO    L200TMP-NAME
      *
      *    誕生日
           MOVE    PTINF-BIRTHDAY          TO    L200TMP-TANJO
      *
      *
           IF  RECE10-HKNNUM  =  SPACE
      *
      *      給付割合  公費単独の時は０を設定
             MOVE    ZERO                        TO  L200TMP-KYURATE
      *
             PERFORM  VARYING  IDX  FROM  2  BY  1
                      UNTIL    IDX  >  5
      *        実日数
               ADD     RECE10-JNISSU(IDX)          TO  L200TMP-NISSU
      *
      *        総点数
               ADD     RECE10-TOTALTEN(IDX)        TO  L200TMP-TENSU
      *
      *        一部負担金
               ADD     RECE10-FTNMONEY(IDX)        TO  L200TMP-ITBFTN
      *
      *        食事日数
               ADD     RECE10-SHOKUJINISSU(IDX)    TO  L200TMP-SNISSU
      *
      *        食事療養費
               ADD     RECE10-SHOKUJIRYOYOHI(IDX)  TO  L200TMP-KINGK
      *
      *        標準負担額
               ADD     RECE10-SHOKUJIFTN(IDX)      TO  L200TMP-FTNGK
             END-PERFORM
      *
           ELSE
      *
      *      給付割合
             COMPUTE  L200TMP-KYURATE  =  10  -  RECE10-KYURATE
      *
      *      実日数
             MOVE    RECE10-JNISSU(1)            TO  L200TMP-NISSU
      *
      *      総点数
             MOVE    RECE10-TOTALTEN(1)          TO  L200TMP-TENSU
      *
      *      一部負担金
             MOVE    RECE10-FTNMONEY(1)          TO  L200TMP-ITBFTN
      *
      *      食事日数
             MOVE    RECE10-SHOKUJINISSU(1)      TO  L200TMP-SNISSU
      *
      *      食事療養費
             MOVE    RECE10-SHOKUJIRYOYOHI(1)    TO  L200TMP-KINGK
      *
      *      標準負担額
             MOVE    RECE10-SHOKUJIFTN(1)        TO  L200TMP-FTNGK
      *
           END-IF
      *
      *    書き出し
           IF    FLG-PASS   =   ZERO
             WRITE    L200TMP-REC
             ADD      1      TO      CNT-DATA
           END-IF
      *
           .
       2001-DATA-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担者番号設定
      *****************************************************************
       2001-FTNJANUM-SEC                SECTION.
      *
           INITIALIZE    WRK-KOHI-TBL
      *
      *    負担者番号取得
           MOVE    ZERO    TO    FLG-PASS
           PERFORM  2001-KOHI-HEIYOU-SEC
      *    負担者番号判定処理
           PERFORM  2001-FTNJA-CHK-SEC
      *    公費番号設定
           IF    WRK-KOH1-FTNJANUM  NOT  =  SPACE
             MOVE  WRK-KOH1-FTNJANUM    TO    L200TMP-FTNJANUM1
           END-IF
      *
           IF    WRK-KOH2-FTNJANUM  NOT  =  SPACE
             IF    L200TMP-FTNJANUM1     =  SPACE
               MOVE    WRK-KOH2-FTNJANUM    TO  L200TMP-FTNJANUM1
             ELSE
               MOVE    WRK-KOH2-FTNJANUM    TO  L200TMP-FTNJANUM2
             END-IF
           END-IF
      *
           .
       2001-FTNJANUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費併用チェック
      *****************************************************************
       2001-KOHI-HEIYOU-SEC                SECTION.
      *
           IF      RECE10-KOHNUM(1)     NOT  =  SPACE
             INITIALIZE                 PTKOHINF-REC
             MOVE    "PTKOHINF-KEY6"    TO   WRK-PATH2
             MOVE    RECE10-HOSPNUM TO   PTKOH-HOSPNUM
             MOVE    RECE10-PTID   TO   PTKOH-PTID
             MOVE    RECE10-KOHNUM(1)   TO
                                        PTKOH-KOHNUM
             MOVE    RECE10-SRYYM  TO   PTKOH-TEKSTYMD(1:6)
                                        PTKOH-TEKEDYMD(1:6)
             MOVE    "01"          TO   PTKOH-TEKEDYMD(7:2)
             MOVE    "99"          TO   PTKOH-TEKSTYMD(7:2)
             PERFORM  940-PTKOHINF-READ-SEC
             MOVE     PTKOH-FTNJANUM    TO  WRK-KOH1-FTNJANUM
             MOVE     PTKOH-JKYSNUM     TO  WRK-KOH1-JKYSNUM
           END-IF
      *
           IF      RECE10-KOHNUM(2)     NOT  =  SPACE
             INITIALIZE                 PTKOHINF-REC
             MOVE    "PTKOHINF-KEY6"    TO   WRK-PATH2
             MOVE    RECE10-HOSPNUM TO   PTKOH-HOSPNUM
             MOVE    RECE10-PTID   TO   PTKOH-PTID
             MOVE    RECE10-KOHNUM(2)   TO
                                        PTKOH-KOHNUM
             MOVE    RECE10-SRYYM  TO   PTKOH-TEKSTYMD(1:6)
                                        PTKOH-TEKEDYMD(1:6)
             MOVE    "01"          TO   PTKOH-TEKEDYMD(7:2)
             MOVE    "99"          TO   PTKOH-TEKSTYMD(7:2)
             PERFORM  940-PTKOHINF-READ-SEC
             MOVE     PTKOH-FTNJANUM    TO  WRK-KOH2-FTNJANUM
             MOVE     PTKOH-JKYSNUM     TO  WRK-KOH2-JKYSNUM
           END-IF
      *
           .
       2001-KOHI-HEIYOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号判定処理
      *****************************************************************
       2001-FTNJA-CHK-SEC                SECTION.
      *
      *    負担者番号判定
      *    "@"はデータ有り、スペースはデータ無しを対象とする
           IF    WRK-PARA-FTNJANUM  =  SPACE  OR "@"
               IF  (WRK-PARA-FTNJANUM      =  SPACE  AND
                   (WRK-KOH1-FTNJANUM NOT  =  SPACE  OR
                    WRK-KOH2-FTNJANUM NOT  =  SPACE)) OR
                   (WRK-PARA-FTNJANUM      =  "@"    AND
                    WRK-KOH1-FTNJANUM      =  SPACE  AND
                    WRK-KOH2-FTNJANUM      =  SPACE)
                 MOVE    1    TO    FLG-PASS
      *    設定ファイルが"２"の時は受給者番号もチェックする
                 IF    SETTEI-PARA         =  "2"
                   IF  (WRK-PARA-FTNJANUM     =  SPACE  AND
                       (WRK-KOH1-JKYSNUM NOT  =  SPACE  OR
                        WRK-KOH2-JKYSNUM NOT  =  SPACE)) OR
                       (WRK-PARA-FTNJANUM     =  "@"    AND
                        WRK-KOH1-JKYSNUM      =  SPACE  AND
                        WRK-KOH2-JKYSNUM      =  SPACE)
                     MOVE    1    TO    FLG-PASS
                   ELSE
                     MOVE    ZERO TO    FLG-PASS
      *    受給者番号で対象になった場合は、負担者番号に受給者番号を
      *    設定する
                     MOVE    WRK-KOH1-JKYSNUM  TO  WRK-KOH1-FTNJANUM
                     MOVE    WRK-KOH2-JKYSNUM  TO  WRK-KOH2-FTNJANUM
                   END-IF
                 END-IF
               END-IF
           ELSE
      *    半角、全角判定
             IF    WRK-PARA-FTNJANUM(1:1)  = "0" OR "1" OR "2" OR "3"
                                          OR "4" OR "5" OR "6" OR "7"
                                          OR "8" OR "9" OR "*"
      *    半角
               PERFORM  2001-FTNJA-CHK1-SEC
             ELSE
      *    全角
               PERFORM  2001-FTNJA-CHK2-SEC
             END-IF
           END-IF
      *
           .
       2001-FTNJA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角チェック
      *****************************************************************
       2001-FTNJA-CHK1-SEC                SECTION.
      *
      *    公費１負担者番号チェック
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     8  OR
                      FLG-PASS =    1
               IF  (WRK-PARA-FTNJANUM(IDX:1)     =  "*"  AND
                    WRK-KOH1-FTNJANUM(IDX:1)     =  SPACE) OR
                   (WRK-PARA-FTNJANUM(IDX:1) NOT =  "*"  AND
                    WRK-PARA-FTNJANUM(IDX:1) NOT =
                    WRK-KOH1-FTNJANUM(IDX:1))
                 MOVE    1    TO    FLG-PASS
               END-IF
               IF    WRK-KOH1-FTNJANUM(IDX:1) NOT = "0" AND "1" AND
                            "2" AND "3" AND "4" AND "5" AND "6" AND
                                    "7" AND "8" AND "9" AND " "
                 MOVE   1     TO    FLG-PASS
               END-IF
             END-PERFORM
      *    公費２負担者番号チェック
             IF    FLG-PASS     =    1
               MOVE     ZERO     TO   FLG-PASS
               PERFORM  VARYING  IDX  FROM  1  BY  1
                        UNTIL    IDX  >     8  OR
                        FLG-PASS =    1
                 IF  (WRK-PARA-FTNJANUM(IDX:1)     =  "*"  AND
                      WRK-KOH2-FTNJANUM(IDX:1)     =  SPACE) OR
                     (WRK-PARA-FTNJANUM(IDX:1) NOT =  "*"  AND
                      WRK-PARA-FTNJANUM(IDX:1) NOT =
                      WRK-KOH2-FTNJANUM(IDX:1))
                   MOVE    1    TO    FLG-PASS
                 END-IF
                 IF    WRK-KOH2-FTNJANUM(IDX:1) NOT = "0" AND 
                      "1" AND "2" AND "3" AND "4" AND "5" AND
                      "6 "AND "7" AND "8" AND "9" AND " "
                   MOVE   1     TO    FLG-PASS
                 END-IF
               END-PERFORM
             END-IF
      *
      *    設定ファイルが"２"の時は、受給者番号もチェックする
           IF    SETTEI-PARA    =    "2" AND
                 FLG-PASS       =     1
      *    公費１受給者番号チェック
               MOVE     ZERO     TO   FLG-PASS
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                        UNTIL    IDX  >     20  OR
                        FLG-PASS =    1
                 IF  (WRK-PARA-FTNJANUM(IDX:1)     =  "*"    AND
                      WRK-KOH1-JKYSNUM(IDX:1)      =  SPACE) OR
                     (WRK-PARA-FTNJANUM(IDX:1) NOT =  "*"    AND
                      WRK-PARA-FTNJANUM(IDX:1) NOT =
                      WRK-KOH1-JKYSNUM(IDX:1))
                   MOVE    1    TO    FLG-PASS
                 END-IF
                 IF    WRK-KOH1-JKYSNUM(IDX:1) NOT = "1" AND 
                              "2" AND "3" AND "4" AND "5" AND
                              "6 "AND "7" AND "8" AND "9" AND
                              "0" AND " "
                   MOVE   1     TO    FLG-PASS
                 END-IF
               END-PERFORM
      *    公費２受給者番号チェック
               IF    FLG-PASS    =    1
                 MOVE     ZERO     TO     FLG-PASS
                 PERFORM  VARYING  IDX  FROM  1  BY  1
                          UNTIL    IDX  >     20  OR
                          FLG-PASS =    1
                   IF  (WRK-PARA-FTNJANUM(IDX:1)     =  "*"    AND
                        WRK-KOH2-JKYSNUM(IDX:1)      =  SPACE) OR
                       (WRK-PARA-FTNJANUM(IDX:1) NOT =  "*"    AND
                        WRK-PARA-FTNJANUM(IDX:1) NOT =
                        WRK-KOH2-JKYSNUM(IDX:1))
                     MOVE    1    TO    FLG-PASS
                   END-IF
                   IF    WRK-KOH2-JKYSNUM(IDX:1) NOT = "1" AND 
                                "2" AND "3" AND "4" AND "5" AND
                                "6 "AND "7" AND "8" AND "9" AND
                                "0" AND " "
                     MOVE   1     TO    FLG-PASS
                   END-IF
                 END-PERFORM
               END-IF
      *    受給者番号で対象となった場合は、負担者番号に受給者番号をセットする
               IF    FLG-PASS    =    ZERO
                 MOVE    WRK-KOH1-JKYSNUM  TO  WRK-KOH1-FTNJANUM
                 MOVE    WRK-KOH2-JKYSNUM  TO  WRK-KOH2-FTNJANUM
               END-IF
           END-IF
      *
           .
       2001-FTNJA-CHK1-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック
      *****************************************************************
       2001-FTNJA-CHK2-SEC                SECTION.
      *
      *    公費１負担者番号チェック
             PERFORM  VARYING  IDX  FROM  1  BY  2
                      UNTIL    IDX  >     20  OR
                      FLG-PASS =    1
               IF  (WRK-PARA-FTNJANUM(IDX:2)     =  "＊"  AND
                    WRK-KOH1-FTNJANUM(IDX:2)     =  SPACE) OR
                   (WRK-PARA-FTNJANUM(IDX:2) NOT =  "＊"  AND
                    WRK-PARA-FTNJANUM(IDX:2) NOT =
                    WRK-KOH1-FTNJANUM(IDX:2))
                 MOVE    1    TO    FLG-PASS
               END-IF
               IF    WRK-KOH1-FTNJANUM(IDX:1) = "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6" OR
                                                "7" OR "8" OR "9" OR
                                                "0"
                 MOVE   1     TO    FLG-PASS
               END-IF
             END-PERFORM
      *    公費２負担者番号チェック
             IF    FLG-PASS     =    1
               MOVE     ZERO     TO   FLG-PASS
               PERFORM  VARYING  IDX  FROM  1  BY  2
                        UNTIL    IDX  >     20  OR
                        FLG-PASS =    1
                 IF  (WRK-PARA-FTNJANUM(IDX:2)     =  "＊"  AND
                      WRK-KOH2-FTNJANUM(IDX:2)     =  SPACE) OR
                     (WRK-PARA-FTNJANUM(IDX:2) NOT =  "＊"  AND
                      WRK-PARA-FTNJANUM(IDX:2) NOT =
                      WRK-KOH2-FTNJANUM(IDX:2))
                   MOVE    1    TO    FLG-PASS
                 END-IF
                 IF  WRK-KOH2-FTNJANUM(IDX:1) = "1" OR "2" OR "3" OR
                                               "4" OR "5" OR "6" OR
                                               "7" OR "8" OR "9" OR
                                               "0"
                   MOVE   1     TO    FLG-PASS
                 END-IF
               END-PERFORM
      *
             END-IF
      *
      *    設定ファイルが"２"の時は、受給者番号もチェックする
           IF    SETTEI-PARA    =    "2" AND
                 FLG-PASS       =     1
      *    公費１受給者番号チェック
               MOVE     ZERO     TO   FLG-PASS
      *
               PERFORM  VARYING  IDX  FROM  1  BY  2
                        UNTIL    IDX  >     20  OR
                        FLG-PASS =    1
                 IF  (WRK-PARA-FTNJANUM(IDX:2)     =  "＊"    AND
                      WRK-KOH1-JKYSNUM(IDX:2)      =  SPACE) OR
                     (WRK-PARA-FTNJANUM(IDX:2) NOT =  "＊"    AND
                      WRK-PARA-FTNJANUM(IDX:2) NOT =
                      WRK-KOH1-JKYSNUM(IDX:2))
                   MOVE    1    TO    FLG-PASS
                 END-IF
                 IF  WRK-KOH1-JKYSNUM(IDX:1) = "1" OR "2" OR "3" OR
                                               "4" OR "5" OR "6" OR
                                               "7" OR "8" OR "9" OR
                                               "0"
                   MOVE   1     TO    FLG-PASS
                 END-IF
               END-PERFORM
      *    公費２受給者番号チェック
               IF    FLG-PASS    =    1
                 MOVE     ZERO     TO     FLG-PASS
                 PERFORM  VARYING  IDX  FROM  1  BY  2
                          UNTIL    IDX  >     20  OR
                          FLG-PASS =    1
                   IF  (WRK-PARA-FTNJANUM(IDX:2)     =  "＊"    AND
                        WRK-KOH2-JKYSNUM(IDX:2)      =  SPACE) OR
                       (WRK-PARA-FTNJANUM(IDX:2) NOT =  "＊"    AND
                        WRK-PARA-FTNJANUM(IDX:2) NOT =
                        WRK-KOH2-JKYSNUM(IDX:2))
                     MOVE    1    TO    FLG-PASS
                   END-IF
                   IF  WRK-KOH2-JKYSNUM(IDX:1) = "1" OR "2" OR "3"
                                       OR "4" OR "5" OR "6" OR "7"
                                       OR "8" OR "9" OR "0"
                     MOVE   1     TO    FLG-PASS
                   END-IF
                 END-PERFORM
               END-IF
      *    受給者番号で対象となった場合は、負担者番号に受給者番号をセットする
               IF    FLG-PASS    =    ZERO
                 MOVE    WRK-KOH1-JKYSNUM  TO  WRK-KOH1-FTNJANUM
                 MOVE    WRK-KOH2-JKYSNUM  TO  WRK-KOH2-FTNJANUM
               END-IF
           END-IF
      *
           .
       2001-FTNJA-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           INITIALIZE           L200TMP-REC
           OPEN     INPUT       L200TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
                                         CNT-READ
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT  =  1
      *
               MOVE    KEY-NEW    TO    KEY-OLD
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
           END-PERFORM
           CLOSE  L200TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       310-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      L200
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    L200-PAGE
      *
      *    診療科名(旧総合病院時のみ)
           IF    SYS-1001-HOSPSBT1  =  1
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  L200TMP-SRYKA-KEY       =     SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO  L200-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
           END-IF
      *
           IF    L200TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   L200-NYUGAI
           ELSE
             MOVE     "外来"        TO   L200-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   L200-SRYYM
           MOVE    WRK-SEIYMD       TO   L200-SEIYMD
      *
           MOVE    WRK-PARA-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  L200-GHKNJANUM
           MOVE    WRK-PARA-FTNJANUM   TO  L200-GFTNJANUM
      *
           IF    CNT-READ      =  ZERO
             OR  KEY-NEW   NOT =  KEY-OLD
             PERFORM  800-CSV-HEAD-SEC
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細>処理
      *****************************************************************
       320-BODY-HEN-SEC  SECTION.
      *
      *    初期化処理
           MOVE    ZERO    TO     CNT-LINE
      *
           PERFORM  VARYING  CNT-LINE  FROM  1  BY  1
                   UNTIL CNT-LINE    >  40
                   OR  KEY-NEW  NOT =  KEY-OLD
                   OR  FLG-PRINT     =  1
      *
      *    連番
             ADD    1                TO    CNT-READ
             MOVE   CNT-READ         TO    WRK-Z4
             MOVE   WRK-Z4           TO    L200-NO(CNT-LINE)
      *
      *    患者番号
             MOVE   L200TMP-PTNUM    TO    L200-PTNUM(CNT-LINE)
      *
      *    患者氏名
             MOVE   L200TMP-NAME     TO    L200-NAME(CNT-LINE)
      *
      *    保険者番号
      *    公費単独の時は印字しない
             IF    L200TMP-HKNNUM  NOT  =  SPACE
               MOVE   L200TMP-HKNJANUM   TO    WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE   WRK-OUT-INPUT      TO    L200-HKNJANUM(CNT-LINE)
      *
      *    保険者名
      *    保険者マスタに登録されてない場合があるので番号を変換する
               EVALUATE  L200TMP-HKNNUM
               WHEN "067"
                 MOVE    L200TMP-HKNJANUM(3:)  TO  WRK-HKNJANUM
               WHEN "072"
                 MOVE    "31"     TO     WRK-HKNJANUM(1:2)
                 MOVE    L200TMP-HKNJANUM(3:)
                                  TO     WRK-HKNJANUM(3:) 
               WHEN "073"
                 MOVE    "32"     TO     WRK-HKNJANUM(1:2) 
                 MOVE    L200TMP-HKNJANUM(3:)
                                  TO     WRK-HKNJANUM(3:) 
               WHEN "074"
                 MOVE    "33"     TO     WRK-HKNJANUM(1:2) 
                 MOVE    L200TMP-HKNJANUM(3:)
                                  TO     WRK-HKNJANUM(3:) 
               WHEN "075"
                 MOVE    "34"     TO     WRK-HKNJANUM(1:2) 
                 MOVE    L200TMP-HKNJANUM(3:)
                                  TO     WRK-HKNJANUM(3:) 
               WHEN OTHER
                 MOVE    L200TMP-HKNJANUM  TO  WRK-HKNJANUM
               END-EVALUATE
               PERFORM  920-HKNJAINF-SET-SEC
               MOVE   WRK-HKNJANAME    TO    L200-HKNJANAME(CNT-LINE)
             END-IF
      *
      *    負担者番号
             MOVE   L200TMP-FTNJANUM1 TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE   WRK-OUT-INPUT     TO   L200-FTNJANUM1(CNT-LINE)
             MOVE   L200TMP-FTNJANUM2 TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE   WRK-OUT-INPUT     TO   L200-FTNJANUM2(CNT-LINE)
      *
      *    生年月日
             MOVE  L200TMP-TANJO         TO   WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  LNK-DAY2-EDTYMD1      TO   L200-TANJO(CNT-LINE)
      *
      *    年齢
             MOVE  L200TMP-AGE        TO   WRK-Z3
             MOVE  WRK-Z3             TO   L200-AGE(CNT-LINE)
      *
      *    本人・家族
             EVALUATE L200TMP-HONKZKKBN
             WHEN "1"
               MOVE    "本"           TO   L200-HONKZKKBN(CNT-LINE)
             WHEN "2"
               MOVE    "家"           TO   L200-HONKZKKBN(CNT-LINE)
             END-EVALUATE
      *
      *    負担割合
             MOVE    L200TMP-KYURATE  TO   WRK-Z2
             MOVE    WRK-Z2           TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO   L200-FUTANWARI(CNT-LINE)
      *
      *    診療年月
             IF    L200TMP-SRYYM  NOT  =   WRK-PARA-SRYYM
               MOVE    L200TMP-SRYYM  TO   WRK-SYMD(1:6)
               MOVE    "01"           TO   WRK-SYMD(7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE  LNK-DAY2-EDTYMD1(1:6) TO L200-SRYYM2(CNT-LINE)
             END-IF
      *
      *    実日数
             MOVE    L200TMP-NISSU    TO    WRK-Z2
             MOVE    WRK-Z2           TO    WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO    L200-NISSU(CNT-LINE)
      *
      *    総点数
             MOVE    L200TMP-TENSU    TO    WRK-Z7
             MOVE    WRK-Z7           TO    WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO    L200-TENSU(CNT-LINE)
      *
      *    一部負担金
             MOVE    L200TMP-ITBFTN   TO    WRK-Z7
             MOVE    WRK-Z7           TO    WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO    L200-ITBFTN(CNT-LINE)
      *
      *    食事日数
             MOVE    L200TMP-SNISSU   TO    WRK-Z2
             MOVE    WRK-Z2           TO    WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO    L200-SNISSU(CNT-LINE)
      *
      *    食事療養費
             MOVE    L200TMP-KINGK    TO    WRK-Z7
             MOVE    WRK-Z7           TO    WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO    L200-KINGK(CNT-LINE)
      *
      *    標準負担額
             MOVE    L200TMP-FTNGK    TO    WRK-Z6
             MOVE    WRK-Z6           TO    WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT    TO    L200-FTNGK(CNT-LINE)
      *
      *    合計加算
             ADD    L200TMP-NISSU     TO    WRK-GNISSU
             ADD    L200TMP-TENSU     TO    WRK-GTENSU
             ADD    L200TMP-ITBFTN    TO    WRK-GITBFTN
             ADD    L200TMP-SNISSU    TO    WRK-GSNISSU
             ADD    L200TMP-KINGK     TO    WRK-GKINGK
             ADD    L200TMP-FTNGK     TO    WRK-GFTNGK
      *
             PERFORM  400-CSV-DATA-SEC
      *
      *    レセプト明細読込
             PERFORM  900-TMP-READ-SEC
      *
           END-PERFORM
      *
      *    合計印字
           IF  FLG-PRINT     =    1   OR
               KEY-NEW   NOT = KEY-OLD
      *
             PERFORM  3201-GOKEI-SEC
      *
             PERFORM  800-CSV-HEAD-SEC
      *
           END-IF
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       3201-GOKEI-SEC         SECTION.
      *
      *    日数
           MOVE    WRK-GNISSU        TO    WRK-Z4
           MOVE    WRK-Z4            TO    L200-GNISSU
      *
      *    点数
           MOVE    WRK-GTENSU        TO    WRK-Z10
           MOVE    WRK-Z10           TO    L200-GTENSU
      *
      *    一部負担金
           MOVE    WRK-GITBFTN       TO    WRK-Z10
           MOVE    WRK-Z10           TO    L200-GITBFTN
      *
      *    食事日数
           MOVE    WRK-GSNISSU       TO    WRK-Z4
           MOVE    WRK-Z4            TO    L200-GSNISSU
      *
      *    金額
           MOVE    WRK-GKINGK        TO    WRK-Z10
           MOVE    WRK-Z10           TO    L200-GKINGK
      *
      *    標準負担額
           MOVE    WRK-GFTNGK        TO    WRK-Z9
           MOVE    WRK-Z9            TO    L200-GFTNGK
      *
           PERFORM  400-CSV-GOUKEI-SEC
      *
           INITIALIZE    WRK-GOKEI-AREA
      *
           .
       3201-GOKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY  TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY  TO  SPRT-PRIORITY
           MOVE    "A00000L200V03.red" TO  SPRT-PRTID
           MOVE    "保険別患者台帳"    TO  SPRT-TITLE
           MOVE    L200                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE                    
           END-IF
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                    TO  IDXP
      *
      *    診療科名(旧総合病院時のみ)
           INITIALIZE  WRK-ITEM
           IF    SYS-1001-HOSPSBT1  =  1
             MOVE    "診療科名："         TO  WRK-ITEM(1:10)
             MOVE    L200-SRYKANAME       TO  WRK-ITEM(11:)
             PERFORM  800-CSVDATA-EDIT-SEC
           END-IF
      *
      *    対象年月
           INITIALIZE  WRK-ITEM
           MOVE    "対象年月："         TO  WRK-ITEM(1:10)
           MOVE    L200-SRYYM           TO  WRK-ITEM(11:)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分
           INITIALIZE  WRK-ITEM
           MOVE    "入外区分："         TO  WRK-ITEM(1:10)
           IF    L200TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"              TO   WRK-ITEM(11:)
           ELSE
             MOVE     "外来"              TO   WRK-ITEM(11:)
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者番号
           INITIALIZE  WRK-ITEM
           MOVE    "保険者番号："       TO  WRK-ITEM(1:12)
           MOVE    L200-GHKNJANUM       TO  WRK-ITEM(13:)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    負担者番号
           INITIALIZE  WRK-ITEM
           MOVE    "負担者番号："       TO  WRK-ITEM(1:12)
           MOVE    L200-GFTNJANUM       TO  WRK-ITEM(13:)
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    作成年月日
           INITIALIZE  WRK-ITEM
           MOVE    L200-SEIYMD          TO  WRK-ITEM(1:22)
           MOVE    "  発行"             TO  WRK-ITEM(23:6)
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称セット
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
             IF   IDX4  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    連番
           INITIALIZE  WRK-ITEM
           MOVE    CNT-READ                     TO  WRK-Z4
           MOVE    WRK-Z4                       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-PTNUM                TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者氏名
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-NAME                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者番号
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-HKNJANUM             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険者名
           INITIALIZE  WRK-ITEM
           MOVE    L200-HKNJANAME(CNT-LINE)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    負担者番号１
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-FTNJANUM1            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    負担者番号２
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-FTNJANUM2            TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    生年月日
           INITIALIZE  WRK-ITEM
           MOVE    L200-TANJO(CNT-LINE)         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    年齢
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-AGE                  TO  WRK-Z3
           MOVE    WRK-Z3                       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    本人・家族
           INITIALIZE  WRK-ITEM
           MOVE    L200-HONKZKKBN(CNT-LINE)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    負担割合
           INITIALIZE  WRK-ITEM
           MOVE    L200-FUTANWARI(CNT-LINE)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療年月
           INITIALIZE  WRK-ITEM
           MOVE    L200-SRYYM2(CNT-LINE)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    実日数
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-NISSU                TO  WRK-Z4
           MOVE    WRK-Z4                       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    総点数
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-TENSU                TO  WRK-Z7-2
           MOVE    WRK-Z7-2                     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    一部負担金
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-ITBFTN               TO  WRK-Z6-2
           MOVE    WRK-Z6-2                     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事日数
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-SNISSU               TO  WRK-Z2
           MOVE    WRK-Z2                       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事療養費
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-KINGK                TO  WRK-Z7-2
           MOVE    WRK-Z7-2                     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    標準負担額
           INITIALIZE  WRK-ITEM
           MOVE    L200TMP-FTNGK                TO  WRK-Z6-2
           MOVE    WRK-Z6-2                     TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
      *
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計編集処理
      *****************************************************************
       400-CSV-GOUKEI-SEC            SECTION.
      *
           INITIALIZE  WRK-ITEM
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  11
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    計
           INITIALIZE  WRK-ITEM
           MOVE    "計"                      TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    日数
           INITIALIZE  WRK-ITEM
           MOVE    WRK-GNISSU                TO  WRK-Z4-2
           MOVE    WRK-Z4-2                  TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    点数
           INITIALIZE  WRK-ITEM
           MOVE    WRK-GTENSU                TO  WRK-Z10-2
           MOVE    WRK-Z10-2                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    一部負担金
           INITIALIZE  WRK-ITEM
           MOVE    WRK-GITBFTN               TO  WRK-Z9-2
           MOVE    WRK-Z9-2                  TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    食事日数
           INITIALIZE  WRK-ITEM
           MOVE    WRK-GSNISSU               TO  WRK-Z4-2
           MOVE    WRK-Z4-2                  TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    金額
           INITIALIZE  WRK-ITEM
           MOVE    WRK-GKINGK                TO  WRK-Z10-2
           MOVE    WRK-Z10-2                 TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    標準負担額
           INITIALIZE  WRK-ITEM
           MOVE    WRK-GFTNGK                TO  WRK-Z9-2
           MOVE    WRK-Z9-2                  TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           IF  FLG-PRINT  NOT =  1
             INITIALIZE  WRK-ITEM
             PERFORM  810-CSVDATA-EDIT-SEC
           END-IF
      *
           PERFORM  230-TOUKEICSV-SEC
      *
           .
       400-CSV-GOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       800-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                         ORCSTOUKEICSVAREA
           MOVE    "INS"                      TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM        TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID       TO  STOUKEICSV-SHELLID
           MOVE    LNK-PRTKANRI-SRYYM         TO  STOUKEICSV-SRYYM
           MOVE    ZERO                       TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"                TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM                DELIMITED  BY  SIZE
                   "kanjadaichou_hoken_"      DELIMITED  BY  SIZE
                   WRK-PARA-SRYYM             DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "保険別患者台帳"           TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
                                              ORCSTOUKEICSVAREA
                                              SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                                TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                         WRK-CSVDATA
           MOVE    1                          TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    LNK-PRTKANRI-SKYYMD  TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYMD              TO  ORC-DBYMD
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
           END-IF        
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
               MOVE    1                      TO  FLG-SYSKANRI3
           END-IF  
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
           .
       31012-SEIWA-HEN-EXT.
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
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
      *
      *    ジョブ終了処理
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
           CLOSE   RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ    L200TMP-FILE  NEXT
               AT END
                   MOVE  1  TO  FLG-PRINT
               NOT AT END
                   MOVE  L200TMP-NYUGAIKBN   TO  KEY-N-NYUGAIKBN
                   MOVE  L200TMP-SRYKA-KEY   TO  KEY-N-SRYKA
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE   SYS-1001-HOSPNUM       TO  RECE10-HOSPNUM
           IF WRK-PATH = "SEIKYU-KEY4"
                MOVE  "00000000"         TO  RECE10-XXSTHKNJANUM
                MOVE  "99999999"         TO  RECE10-XXEDHKNJANUM
           END-IF
           MOVE    WRK-PARA-SRYYM        TO  RECE10-SKYYM
           IF   WRK-PATH = "SEIKYU-KEY60"
                MOVE    6                TO  RECE10-TEISYUTUSAKI
           END-IF
           MOVE    RECE10-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"          TO  MCP-TABLE
      *
           EVALUATE  WRK-PATH
             WHEN   "SEIKYU-KEY3"
              MOVE    "key3"             TO  MCP-PATHNAME
             WHEN   "SEIKYU-KEY4"
              MOVE    "key4"             TO  MCP-PATHNAME
             WHEN   "SEIKYU-KEY60"
              MOVE    "key60"            TO  MCP-PATHNAME
           END-EVALUATE
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           EVALUATE  WRK-PATH
             WHEN   "SEIKYU-KEY3"
              MOVE    "key3"            TO  MCP-PATHNAME
             WHEN   "SEIKYU-KEY4"
              MOVE    "key4"            TO  MCP-PATHNAME
             WHEN   "SEIKYU-KEY60"
              MOVE    "key60"           TO  MCP-PATHNAME
           END-EVALUATE
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-PTID         TO  WRK-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *        対象判定
               PERFORM  9001-CHK-SEC
               IF    FLG-PASS    =    1
                 GO    TO    900-SEIKYU-READ-SEC
               END-IF
      *
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    対象判定
      *****************************************************************
       9001-CHK-SEC                 SECTION.
      *
           MOVE    ZERO    TO    FLG-PASS
      *    入外区分判定
           IF    WRK-PARA-NYUGAIKBN   NOT  =  SPACE  AND
                 RECE10-NYUGAIKBN     NOT  =  WRK-PARA-NYUGAIKBN
             MOVE    1    TO    FLG-PASS
           END-IF
      *
      *    SPACEは公費単独等で、保険を持たない患者が対象とする
            IF    WRK-PARA-HKNJANUM      =  SPACE OR 
                  WRK-PARA-HKNJANUM(1:1) =  "@"
              IF  (WRK-PARA-HKNJANUM   =  SPACE AND
                   RECE10-HKNJANUM NOT =  "00000000") OR
                  (WRK-PARA-HKNJANUM(1:1) = "@" AND
                   RECE10-HKNJANUM  =  "00000000")
                MOVE    1     TO     FLG-PASS
              END-IF
            ELSE 
      *    頭２桁が２７で、８桁の場合は老人負担者番号をチェックする
             IF    WRK-PARA-HKNJANUM(1:2)  =  "27"  AND
                   WRK-PARA-HKNJANUM(8:1) NOT = SPACE
               INITIALIZE                 PTKOHINF-REC
               MOVE    "PTKOHINF-KEY6"    TO   WRK-PATH2
               MOVE    RECE10-HOSPNUM     TO   PTKOH-HOSPNUM
               MOVE    RECE10-PTID        TO   PTKOH-PTID
               MOVE    RECE10-RJNHKNNUM   TO   PTKOH-KOHNUM
               MOVE    RECE10-SRYYM       TO   PTKOH-TEKSTYMD(1:6)
                                               PTKOH-TEKEDYMD(1:6)
               MOVE    "01"               TO   PTKOH-TEKEDYMD(7:2)
               MOVE    "99"               TO   PTKOH-TEKSTYMD(7:2)
               PERFORM  940-PTKOHINF-READ-SEC
               MOVE     PTKOH-FTNJANUM    TO  WRK-CHK-HKNJANUM
             ELSE
               MOVE     RECE10-HKNJANUM   TO  WRK-CHK-HKNJANUM
             END-IF
      *
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL    IDX  >     8  OR
                      FLG-PASS =    1
               EVALUATE  WRK-PARA-HKNJANUM(IDX:1)
               WHEN "*"
                 IF    WRK-CHK-HKNJANUM(IDX:1) =  SPACE
                   MOVE    1    TO  FLG-PASS
                 END-IF
               WHEN SPACE
                 IF    RECE10-HKNJANUM(IDX:1) NOT = SPACE
                   MOVE    1    TO  FLG-PASS
                 ELSE
                   MOVE    8    TO  IDX
                 END-IF
               WHEN OTHER
                 IF    WRK-PARA-HKNJANUM(IDX:1)  NOT =
                                         WRK-CHK-HKNJANUM(IDX:1)
                   MOVE    1    TO  FLG-PASS
                 END-IF
               END-EVALUATE
             END-PERFORM
      *
           END-IF
      *
      *    本人家族区分の判定
           IF    WRK-PARA-HONKZKKBN  NOT  =  SPACE  AND
                 RECE10-HONKZKKBN    NOT  =  WRK-PARA-HONKZKKBN
             MOVE    1    TO    FLG-PASS
           END-IF
      *
      *    診療科の判定
           IF    RECE10-NYUGAIKBN   =   "2"   AND
                 RECE10-RECEKA  NOT =  WRK-PARA-SRYKA  AND
                 SYS-1001-HOSPSBT1  =  1  AND
                 WRK-PARA-SRYKA NOT =  SPACE
             MOVE    1    TO    FLG-PASS
           END-IF
      *
           .
       9001-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理情報クローズ処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           EVALUATE  WRK-PATH
             WHEN   "SEIKYU-KEY3"
              MOVE    "key3"            TO  MCP-PATHNAME
             WHEN   "SEIKYU-KEY4"
              MOVE    "key4"            TO  MCP-PATHNAME
             WHEN   "SEIKYU-KEY60"
              MOVE    "key60"            TO  MCP-PATHNAME
           END-EVALUATE
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
                MOVE    "tbl_ptinf"         TO  MCP-TABLE
                MOVE    "key"               TO  MCP-PATHNAME
                CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  WRK-PARA-HOSPNUM     TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "HKNJAINF-KEY"       TO  ORC-DBPATH
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR" UPON CONSOLE
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_hknjainf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
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
           MOVE    "DBFETCH"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC          TO  HKNJAINF-REC
             MOVE  ZERO                 TO  FLG-HKNJAINF
             MOVE  HKNJA-HKNJANAME      TO  WRK-HKNJANAME
           ELSE
             INITIALIZE                 HKNJAINF-REC
             MOVE  1                    TO  FLG-HKNJAINF
             MOVE  WRK-HKNJANUM         TO  WRK-HKNJANAME
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
           MOVE  ACCT-HOSPNUM         TO  COMB-HOSPNUM
           MOVE  ACCT-PTID            TO  COMB-PTID
           MOVE  ACCT-HKNCOMBI        TO  COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      ORCMCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hkncombi"     TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             MOVE    "DBFETCH"          TO  MCP-FUNC
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  HKNCOMBI-REC
               MOVE  ZERO         TO  FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1            TO  FLG-HKNCOMBI
             END-IF
      *
           ELSE
             MOVE    1            TO  FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_hkncombi" TO  MCP-TABLE
           MOVE    "key"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"    USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
      *
           .
       930-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行会計報SELECT
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                       SRYACCT-REC
           MOVE    RECE10-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    RECE10-PTID          TO  ACCT-PTID
           MOVE    RECE10-SRYYM         TO  ACCT-SRYYM
           MOVE    RECE10-NYUGAIKBN     TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC               =   ZERO
                   MOVE  ZERO               TO FLG-SRYACCT
                   PERFORM   900-SRYACCT-FETCH-SEC
           ELSE
             MOVE   1               TO  FLG-SRYACCT
             INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報読み込み
      *****************************************************************
       900-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    MCPDATA-REC     TO  SRYACCT-REC
             MOVE    ZERO            TO  FLG-SRYACCT
      *    旧総合病院時は診療科で判定を行う
             IF     RECE10-NYUGAIKBN   =  "2"  AND
                    SYS-1001-HOSPSBT1  =   1   AND
                    RECE10-RECEKA  NOT  =  ACCT-SRYKA
               GO    TO    900-SRYACCT-FETCH-SEC
             END-IF
           ELSE
             MOVE   1                TO  FLG-SRYACCT
             INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      ***************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO  MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.

