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
       PROGRAM-ID.     SEIKYU3004.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 和歌山ひとり親家庭等医療費請求書(143)
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/05/01    笠原
      *****************************************************************
      * wakayama chihou kouhi contribution
      * Special thanks to michiyo noda,motohide takagaki,katsunori yoneda
      * for help in development
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.00       笠原    02/12/16        開設者名に変更
      *  01.00.01       楠本    03/07/02        地方公費対応
      *                                       　有田市　吉備町
      *                                         湯浅町　金屋町　岩出町
      *                                         打田町　桃山町　橋本市
      *                                         かつらぎ町　九度山町
      *                                         御坊市
      *  01.00.02       楠本    04/01/12        田辺市
      *  01.00.03       楠本    05/04/20        清水町　粉河町　那賀町
      *  01.00.04       楠本    05/05/30        串本町
      *  01.00.05       楠本    05/08/25        上富田
      *  01.00.06       楠本    05/10/07        日高川町
      *  01.00.07       楠本    05/11/30        由良町　みなべ町　美浜町
      *                                         印南町　日高町
      *  01.00.08       楠本    06/07/07        MONFUNC DBCLOSESURSOR 対応
      *  02.00.00       楠本　　07/10/11        ORCA4.0対応
      *  02.00.01       吉川    10/06/02        SPA-AREA初期化
      *                                         システム管理マスタ「1901」の
      *                                         医療機関情報を参照
      *                                         かつらぎ町は未就学で帳票を分ける
      *  02.00.02       吉川    10/06/24        和歌山市、橋本市以外の帳票廃止
      *  02.00.03       吉川    10/12/08        open-cobol1.0対応
      *  02.00.04       吉川    11/05/30        ver4.6.0での印字不具合対応
      *  02.00.05       吉川    13/01/11        医療機関情報編集対応
      *  02.00.06       吉川    14/10/24        ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.07       萩野    16/02/12        医療機関編集情報への開設者名追加対応
      *  02.00.08       萩野    17/02/14        医療機関コード変更時、
      *                                         医療機関コード別の作成処理対応
      *  02.00.09       萩野    18/04/09        SITEKBN変更（動作に変更なし）
      *  02.00.10       萩野    18/12/12        修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル  
           SELECT  MF100-FILE   ASSIGN  MF100PARA
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
      *    請求書用ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI3003.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *    シェル用領域
           COPY  "CPCOMMONSHELL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
           COPY  "SC3003.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA     PIC X(02).
           03  STS-MF100    PIC X(02).
           03  STS-RECEERR  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END         PIC 9(01).
           03  FLG-SYSKANRI    PIC 9(01).
           03  FLG-HKNJAINF    PIC 9(01). 
           03  FLG-KEY         PIC 9(01).
           03  FLG-KATSURAGI   PIC 9(01).
           03  FLG-HASIMOTO    PIC 9(01).
           03  FLG-KUDOYAMA    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE  PIC 9(02).
           03  CNT-PAGE  PIC 9(03).
           03  CNT-PRINT PIC 9(06).
           03  CNT-MF100  PIC 9(05).
           03  CNT-SUM         PIC 9(03).
      *
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY  PIC 9(02).
               05  SYS-MM  PIC 9(02).
               05  SYS-DD  PIC 9(02).
           03  SYS-TIME    PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX  PIC 9(04).
           03  IDY  PIC 9(04).
           03  IDZ  PIC 9(02).
      *
      *    改ページ判定テーブル（市町村追加時改定要）
       01  TBL-KAIP. 
           03  TBL-KAIP-TBL          OCCURS 22
                                     INDEXED  BY  IDX-MEI.
               05  TBL-KAIP-STSCD    PIC X(03). 
               05  TBL-KAIP-PNO      PIC X(03).
               05  TBL-KAIP-GYO      PIC X(02).
               05  TBL-KAIP-GOUKEI   PIC X(01).
       01  TBL-KAIP-VAL.
      *                                            有田市   
           03  TBL-KAIP-VAL1         PIC X(09) VALUE "004004101". 
      *                                            吉備町   
           03  TBL-KAIP-VAL2         PIC X(09) VALUE "025026010".
      *                                            湯浅町   
           03  TBL-KAIP-VAL3         PIC X(09) VALUE "023023010".
      *                                            金屋町   
           03  TBL-KAIP-VAL4         PIC X(09) VALUE "026026010".
      *                                            岩出町
           03  TBL-KAIP-VAL5         PIC X(09) VALUE "017017010".
      *                                            紀の川市
           03  TBL-KAIP-VAL6         PIC X(09) VALUE "012012060".
      *                                            橋本市
           03  TBL-KAIP-VAL7         PIC X(09) VALUE "003003100". 
      *                                        かつらぎ町   
           03  TBL-KAIP-VAL8         PIC X(09) VALUE "018018150".
      *                                       　 九度山町   
           03  TBL-KAIP-VAL9         PIC X(09) VALUE "020020100".
      *                                       　   御坊市   
           03  TBL-KAIP-VAL10        PIC X(09) VALUE "005005010".
      *                                       　   田辺市   
           03  TBL-KAIP-VAL11        PIC X(09) VALUE "006006180".
      *                                            清水町
           03  TBL-KAIP-VAL12        PIC X(09) VALUE "027026010".
      *                                            串本町
           03  TBL-KAIP-VAL13        PIC X(09) VALUE "045045200".
      *                                          上富田町
           03  TBL-KAIP-VAL14        PIC X(09) VALUE "042042120".
      *                                          日高川町
           03  TBL-KAIP-VAL15        PIC X(09) VALUE "031031080".
      *                                          由良町
           03  TBL-KAIP-VAL16        PIC X(09) VALUE "030030010".
      *                                          みなべ町
           03  TBL-KAIP-VAL17        PIC X(09) VALUE "035035120".
      *                                          美浜町
           03  TBL-KAIP-VAL18        PIC X(09) VALUE "028028010".
      *                                          印南町
           03  TBL-KAIP-VAL19        PIC X(09) VALUE "037037010".
      *                                          日高町
           03  TBL-KAIP-VAL20        PIC X(09) VALUE "029029010".
      *                                          広川町
           03  TBL-KAIP-VAL21        PIC X(09) VALUE "024024151".
      *                                          白浜町
           03  TBL-KAIP-VAL22        PIC X(09) VALUE "038035120".
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-RECEERR      PIC X(200).
           03  WRK-DENPPRTYMWH  PIC X(16).
           03  WRK-SYSYMDWH     PIC X(22).
           03  WRK-SRYYM        PIC X(09).
           03  WRK-STSCD        PIC X(03).
           03  WRK-NYUGAI       PIC X(01).
      *
           03  WRK-SYMD.
               05  WRK-SYY   PIC 9(04).
               05  WRK-SMM   PIC 9(02).
               05  WRK-SDD   PIC 9(02).
           03  WRK-HENYMDG   PIC X(22).
           03  WRK-HENYMDG1  PIC X(09).
           03  WRK-KEISAN1   PIC 9(09).
           03  WRK-KEISAN2   PIC 9(09).
           03  WRK-KEISAN3   PIC 9(09).
           03  WRK-KEISAN4   PIC 9(09).
           03  WRK-KEISAN5   PIC 9(09).
           03  WRK-KEISAN6   PIC 9(09).
           03  WRK-KEISAN7   PIC 9(09).
           03  WRK-KEISAN8   PIC 9(09).
      *
      *    帳票パターン判定用エリア
           03  WRK-KAIP.
               05  WRK-KAIP-STSCD      PIC X(03).
               05  WRK-KAIP-PNO        PIC X(03).
               05  WRK-KAIP-GYO        PIC X(02).
               05  WRK-KAIP-GOUKEI     PIC X(01).
      *
      *    編集用エリア
           03  WRK-FTNRATEX    PIC 9.
           03  WRK-GKENSUX     PIC ZZ9.
           03  WRK-GKOHFTNX    PIC ZZZ,ZZ9.
           03  WRK-NYUNISSUX   PIC Z9.
           03  WRK-GTENSUX     PIC ZZ,ZZ9.
           03  WRK-GTEN-WARIX  PIC ZZZ,ZZ9.
           03  WRK-YKZFTNX     PIC ZZ,ZZ9.
           03  WRK-GKINGKX     PIC ZZZ,ZZ9.
           03  WRK-KOHKENSUX   PIC ZZ9.
           03  WRK-YKZKENSUX   PIC ZZ9.
           03  WRK-JNISSUX     PIC Z9.
           03  WRK-SNISUX      PIC ZZ9.
           03  WRK-SKENSUX     PIC ZZ9.
           03  WRK-STENSUX     PIC ZZ,ZZ9.
           03  WRK-SKINX       PIC ZZ,ZZZ,ZZ9.
           03  WRK-SYKZFTNX    PIC ZZ,ZZZ,ZZ9.
           03  WRK-GYKZFTNX    PIC ZZ,ZZZ,ZZ9.
           03  WRK-HIYOUX      PIC ZZ,ZZZ,ZZ9.
           03  WRK-GTESUX      PIC ZZ,ZZZ,ZZ9.
	   03  WRK-GKIN2X      PIC ZZ,ZZZ,ZZ9.
      *
      *    合計エリア
           03  WRK-KOHFTN      PIC 9(09).
           03  WRK-YKZFTN      PIC 9(09).
           03  WRK-KOHKENSU    PIC 9(05).
           03  WRK-YKZKENSU    PIC 9(05).
           03  WRK-KOHFTN2     PIC 9(09).
           03  WRK-YKZFTN2     PIC 9(09).
           03  WRK-KOHKENSU2   PIC 9(05).
           03  WRK-YKZKENSU2   PIC 9(05).
           03  WRK-TOTALTEN2   PIC 9(05).
           03  WRK-SNISU       PIC 9(04).
           03  WRK-SNNISU      PIC 9(04).
           03  WRK-SKENSU      PIC 9(04).
           03  WRK-SKENSUK     PIC 9(04).
           03  WRK-SKENSUS     PIC 9(04).
           03  WRK-SKENSUNYU   PIC 9(04).
           03  WRK-SKENSUGAI   PIC 9(04).
           03  WRK-STENSU      PIC 9(10).
           03  WRK-STENSUN     PIC 9(10).
           03  WRK-GKENSU      PIC 9(04).
           03  WRK-SKIN        PIC 9(10).
           03  WRK-SKINN       PIC 9(10).
           03  WRK-GKIN        PIC 9(10).
	       03  WRK-GKIN2       PIC 9(10).
           03  WRK-SYKZFTN     PIC 9(10).
           03  WRK-GTESU       PIC 9(10).
      *
      *    医療機関情報
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
       01  SUM-AREA.
           03  SUM-GOKEI  OCCURS  60
                              INDEXED  BY  IDX-GOU.
               05  SUM-STSCD   PIC X(03).
               05  SUM-GKENSU  PIC 9(10).
               05  SUM-GKOHFTN PIC 9(10).
               05  SUM-TENSU   PIC 9(10).
               05  SUM-GKENSUK PIC 9(10).
               05  SUM-GKENSUS PIC 9(10).
               05  SUM-GYKZFTN PIC 9(10).
               05  SUM-GTESU   PIC 9(10).
               05  SUM-GKENSUGAI PIC 9(10).
               05  SUM-GKENSUNYU PIC 9(10).
       01  WRK-KEY-AREA.
      *    キーエリア
           03  WRK-MF100-KEY.
               05  FILLER                    PIC X(04).
               05  WRK-MF100-STSCD.
                   07  FILLER                PIC X(02).
                   07  WRK-MF100-KOHFTNJA-C  PIC X(03).
                   07  FILLER                PIC X(01).
               05  WRK-NYUGAIKBNP            PIC X(01).
               05  WRK-MISYUKBNP             PIC X(01).
               05  WRK-SRYM                  PIC X(06).
               05  FILLER                    PIC X(59).
      *
      *    ＲＥＤファイルエリア
           03  WRK-PRT-ID.
               05  WRK-ID          PIC X(06).
               05  WRK-PRT-CTV     PIC X(03).
               05  WRK-FILE        PIC X(04).
      *
      *    ＲＥＤファイルエリア
           03  WRK-MPRT-ID.
               05  WRK-MID         PIC X(06).
               05  WRK-MPRT-CTV    PIC X(03).
               05  WRK-MFILE       PIC X(05).
      *
      *    ＲＥＤファイルエリア（合計用）
           03  WRK-GPRT-ID.
               05  WRK-GID         PIC X(06).
               05  WRK-GPRT-CTV    PIC X(03).
               05  WRK-GFILE       PIC X(05).
      *
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-STSCD  PIC X(03).
           03  KEY-OLD.
               05  KEY-O-STSCD  PIC X(03).
      *
           COPY  "CPSHELLTBL.INC".
      *
      *     COPY  "ORCA-DBPATH".
           COPY  "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報‐所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *    診療科目情報情報
           COPY  "CPSK1005.INC".
      *
      *    医療機関編集情報
           COPY  "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY  "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY  "CPSK1902.INC".
      *
      *    医療機関住所編集情報（２）
           COPY  "CPSK1903.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY  "CPJOBKANRI.INC".
      *
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY  "CPPRTKANRI.INC".
      *
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY  "CPPRTDATA.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY  "CPHKNJAINF.INC".
      *
       01  PTHKNINF-REC.
           COPY  "CPPTHKNINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY  "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
           COPY  "MCPDATA.INC".
      *     COPY  "CPORCMCP.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY  "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY  "CPORCSPRT.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    口座取得サブ
       01  KOUZA-PARA.
           03  KOUZA-GINKO           PIC X(80).
           03  KOUZA-SITEN           PIC X(80).
           03  KOUZA-SYURUI          PIC X(80).
           03  KOUZA-KOUZANO         PIC X(80).
           03  KOUZA-MEIGI           PIC X(80).
           03  KOUZA-KANAMEIGI       PIC X(80).
           03  KOUZA-HENKO           PIC X(80).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE  SECTION.
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
           COPY    "CPRECEERR.INC".
      *
      ******************************************************************
      *
       PROCEDURE  DIVISION
                  USING   WRK-PARA
                          RECEERR
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
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
           INITIALIZE  WRK-KAIP
           INITIALIZE  SUM-AREA
           INITIALIZE  WRK-KEY-AREA
           INITIALIZE  SPA-AREA
      *
           ACCEPT  SYS-TIME  FROM  TIME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE  LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE  "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)   TO  WRK-DENPPRTYMWH
           MOVE  WRK-HENYMDG1        TO  WRK-SRYYM 
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAME
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAME    TO  MF100PARA-FULLNAME
      *
           OPEN  INPUT  MF100-FILE
      *
      *    システム管理マスタ（医療機関情報）
           PERFORM 120-HOSPINF-GET-SEC
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC  SECTION.
      *
      *    システム日付セット
           MOVE  LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SYSYMDWH
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
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU3004 SYS 1001 ERR ***" UPON CONSOLE
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報‐所在地、連絡先
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
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU3004 SYS 1002 ERR ***" UPON CONSOLE
             MOVE  1            TO  FLG-END
           END-IF
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
      *    合計集計
           PERFORM  210-GOKEI-SYUKEI-SEC
      *
           INITIALIZE  CNT-MF100
           INITIALIZE  FLG-END
           INITIALIZE  FLG-KEY
           INITIALIZE  KEY-AREA
           INITIALIZE  FLG-KEY
      *
           OPEN  INPUT  MF100-FILE
      *
      *    レセプト明細読込
           PERFORM 900-MF100-READ-SEC
           MOVE  MF100-KEY         TO  WRK-MF100-KEY
           MOVE  KEY-NEW           TO  KEY-OLD
           MOVE  1                 TO  CNT-SUM
      *
           PERFORM  UNTIL  FLG-END =   1
           IF  KEY-NEW         NOT =   KEY-OLD
               INITIALIZE  FLG-KEY
               MOVE  KEY-NEW       TO  KEY-OLD
           END-IF
      *
      *    帳票編集<見出し>処理
             PERFORM 310-HEAD-HEN-SEC
      *
      *    帳票編集<明細>処理
             PERFORM 320-BODY-HEN-SEC
      *　　かつらぎ町
             IF  WRK-MF100-KEY (7:3)  =  "018"
                 MOVE   "018018200"    TO  TBL-KAIP-VAL8
             END-IF
      *   橋本市 
             IF  WRK-MF100-KEY (7:3)  =  "003"
                 MOVE   "003003140"    TO  TBL-KAIP-VAL7
             END-IF
      *   九度山町 
             IF  WRK-MF100-KEY (7:3)  =  "020"
                 MOVE   "020020140"    TO  TBL-KAIP-VAL9
             END-IF
      *
      *    帳票印刷処理
      *    合計帳票印刷
             PERFORM 390-PRINT-OUT-SEC
      *
      *
           END-PERFORM
      *
           CLOSE  MF100-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計集計処理
      *****************************************************************
       210-GOKEI-SYUKEI-SEC  SECTION.
      
           INITIALIZE  SUM-AREA
           OPEN  INPUT  MF100-FILE
      
      *    レセプト明細読込
           PERFORM 900-MF100-READ-SEC
      *
           MOVE  KEY-NEW           TO  KEY-OLD
           MOVE  1                 TO  CNT-SUM
           MOVE  KEY-N-STSCD       TO  SUM-STSCD  (CNT-SUM)
      *
           PERFORM  UNTIL  FLG-END =   1
      *
             IF  KEY-NEW       NOT =   KEY-OLD
                 MOVE  KEY-NEW     TO  KEY-OLD
                 ADD   1           TO  CNT-SUM
                 MOVE  KEY-N-STSCD TO  SUM-STSCD   (CNT-SUM)
             END-IF
      *
             ADD   1               TO  SUM-GKENSU  (CNT-SUM)
             ADD  MF100-TOTALTEN   TO  SUM-TENSU   (CNT-SUM)
             EVALUATE  MF100-HKNNUM
               WHEN  "060"
                 ADD    1          TO  SUM-GKENSUK (CNT-SUM)
               WHEN  "067"
                 ADD    1          TO  SUM-GKENSUK (CNT-SUM)
               WHEN OTHER
                 ADD    1          TO  SUM-GKENSUS (CNT-SUM)
             END-EVALUATE
             IF  MF100-FTNMONEY  NOT = ZERO 
                 IF  KEY-N-STSCD     =  "017"
                     MOVE   MF100-FTNMONEY TO  WRK-KEISAN6
                 ELSE
                     COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN 
                                         *  MF100-KYURATE
                                         /  10
                 END-IF
             ELSE
                 COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN *  MF100-KYURATE
                                        /  10
             END-IF
             ADD  WRK-KEISAN6      TO  SUM-GKOHFTN (CNT-SUM)
             ADD  MF100-YKZFTN     TO  SUM-GYKZFTN (CNT-SUM)
             IF  SUM-STSCD (CNT-SUM)   =   "012" OR "035" OR "038"
                 ADD  100          TO  SUM-GTESU   (CNT-SUM)
             END-IF
      *    入院/入院外
             EVALUATE MF100-NYUGAIKBN
               WHEN "1"
                 ADD     1         TO  SUM-GKENSUNYU (CNT-SUM)
               WHEN "2"
                 ADD     1         TO  SUM-GKENSUGAI (CNT-SUM)
               END-EVALUATE
      *    レセプト明細読込
             PERFORM 900-MF100-READ-SEC
      *
           END-PERFORM
      *
           CLOSE  MF100-FILE
      *
           .
       210-GOKEI-SYUKEI-EXT.
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
           INITIALIZE  SC03
      *
      *    市町村データ取り込み
           PERFORM 31013-KAIP-SEC
      *
      *    市町村名
           MOVE     SPACE               TO  SC03-STSNAME
           EVALUATE   MF100-KEY(7:3)
            WHEN "001"
                MOVE "和歌山市"          TO  SC03-STSNAME
            WHEN "002" 
                MOVE "海南市"            TO  SC03-STSNAME
            WHEN "003"
                 MOVE "橋本市"           TO  SC03-STSNAME
            WHEN "004"
                  MOVE "有田市"          TO  SC03-STSNAME
            WHEN "005" 
                 MOVE "御坊市"           TO  SC03-STSNAME
            WHEN "006"  
                MOVE "田辺市"            TO  SC03-STSNAME
            WHEN "007"
                MOVE "新宮市"            TO  SC03-STSNAME
            WHEN "012" 
                MOVE "紀の川市"          TO  SC03-STSNAME
            WHEN "017"
                 MOVE "岩出市"           TO  SC03-STSNAME
            WHEN "010"
                MOVE "紀美野町"          TO  SC03-STSNAME
            WHEN "018" 
                MOVE "かつらぎ町"        TO  SC03-STSNAME
            WHEN "020"
                 MOVE "九度山町"         TO  SC03-STSNAME
            WHEN "021"
                  MOVE "高野町"          TO  SC03-STSNAME
            WHEN "023" 
                 MOVE "湯浅町"           TO  SC03-STSNAME
            WHEN "024"  
                MOVE "広川町"            TO  SC03-STSNAME
            WHEN "025" 
                 MOVE "有田川町"         TO  SC03-STSNAME
            WHEN "028" 
                 MOVE "美浜町"           TO  SC03-STSNAME
            WHEN "029" 
                 MOVE "日高町"           TO  SC03-STSNAME
            WHEN "030" 
                 MOVE "由良町"           TO  SC03-STSNAME
            WHEN "031"  
                MOVE "日高川町"          TO  SC03-STSNAME
            WHEN "035"  
                MOVE "みなべ町"          TO  SC03-STSNAME
            WHEN "037"
                 MOVE "印南町"           TO  SC03-STSNAME
            WHEN "038"
                 MOVE "白浜町"           TO  SC03-STSNAME
            WHEN "042" 
                MOVE "上富田町"          TO  SC03-STSNAME
            WHEN "044" 
                MOVE "すさみ町"          TO  SC03-STSNAME
            WHEN "045" 
                MOVE "串本町"            TO  SC03-STSNAME
            WHEN "046" 
                MOVE "那智勝浦町"        TO  SC03-STSNAME
            WHEN "047" 
                MOVE "太地町"            TO  SC03-STSNAME
            WHEN "049" 
                MOVE "古座川町"          TO  SC03-STSNAME
            WHEN "052" 
                MOVE "北山村"            TO  SC03-STSNAME
            WHEN "005" 
                MOVE "御坊市"            TO  SC03-STSNAME
            WHEN OTHER
                MOVE     ZERO           TO  IDZ
                MOVE     1              TO  FLG-HKNJAINF
                PERFORM UNTIL IDZ            >   9  OR
                         FLG-HKNJAINF   =   ZERO
                INITIALIZE                     HKNJAINF-REC
                MOVE     SYS-1001-HOSPNUM  TO  HKNJA-HOSPNUM
                MOVE     MF100-KEY(5:5)   TO  HKNJA-HKNJANUM (1:5)
                MOVE     IDZ(2:1)          TO  HKNJA-HKNJANUM (6:1)
                MOVE     HKNJAINF-REC      TO  MCPDATA-REC
                PERFORM  900-HKNJAINF-INV-SEC
                IF  FLG-HKNJAINF           =   ZERO
                    MOVE    HKNJA-HKNJANAME    TO  SC03-STSNAME
                END-IF
                ADD      1                 TO  IDZ
                END-PERFORM
           END-EVALUATE
      *
      *    印刷日付
           MOVE   WRK-DENPPRTYMWH(05:04)  TO  SC03-PRTYY
           MOVE   WRK-DENPPRTYMWH(11:04)  TO  SC03-PRTMM
           MOVE   WRK-DENPPRTYMWH(01:16)  TO  SC03-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN       TO  SC03-HOSPCDN
      *
      *    請求日付
           MOVE  WRK-SYSYMDWH           TO  SC03-SEIYMD
      *
      *    郵便番号
           MOVE  SYS-1002-POST(1:3)     TO  SC03-POST(1:3)
           MOVE  "-"                    TO  SC03-POST(4:1)
           MOVE  SYS-1002-POST(4:4)     TO  SC03-POST(5:4)
      *
      *    住所
           MOVE  WRK-HOSPADRS           TO  SC03-ADRS
           INSPECT SC03-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME           TO  SC03-HOSPNAME
           INSPECT SC03-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    電話番号
           MOVE  WRK-HOSPTEL            TO  SC03-TEL
      *
      *    管理者名
           MOVE  WRK-KAISETUNAME        TO  SC03-KANRINAME
      *
           IF  FLG-KEY = 0
               MOVE    KEY-N-STSCD     TO  WRK-STSCD
               PERFORM 31014-GOUKEI-GET-SEC
               MOVE  1  TO  FLG-KEY
           END-IF
      *
      *    振込み先銀行
           MOVE  KOUZA-GINKO            TO  SC03-BANKNAME
      *
      *    振込み先銀行支店
           MOVE  KOUZA-SITEN            TO  SC03-SITENNAME
      *
      *    口座種類
           EVALUATE   KOUZA-SYURUI
             WHEN "1"
               MOVE    "当座"           TO  SC03-SYUBETU
               MOVE    "○"             TO  SC03-KOUZA2
             WHEN "2"
               MOVE    "普通"           TO  SC03-SYUBETU
               MOVE    "○"             TO  SC03-KOUZA1
           END-EVALUATE
      *
      *    振込み先口座番号
           MOVE  KOUZA-KOUZANO          TO  SC03-KOZANUM
      *
      *    振込み先名義人
           MOVE  KOUZA-MEIGI            TO  SC03-MEIGI
      *
      *    振込み先名義人 カナ
           MOVE  KOUZA-KANAMEIGI        TO  SC03-KANAMEIGI
      * 
      *    口座変更
           EVALUATE   KOUZA-HENKO
             WHEN "1"
               MOVE    "○"             TO  SC03-HENKO1 
             WHEN "2"
               MOVE    "○"             TO  SC03-HENKO2
           END-EVALUATE
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC  SECTION.
      *
           MOVE  ZERO               TO  CNT-LINE
           MOVE  MF100-KEY          TO  WRK-MF100-KEY 
      *
           PERFORM  UNTIL  FLG-END  =   1
                    OR   CNT-LINE   =   WRK-KAIP-GYO
                    OR   MF100-KEY (1:18) NOT =  WRK-MF100-KEY (1:18) 
      *
             ADD  1  TO  CNT-LINE
      *
      *    入院/入院外
             EVALUATE  MF100-NYUGAIKBN
               WHEN "1"
                 MOVE  "◯"         TO  SC03-PTENSUKBN  (1)
                 MOVE  "◯"         TO  SC03-TENSUKBN   (CNT-LINE 1)
                 MOVE  "◯"         TO  SC03-NYUGAI-KBN (CNT-LINE 1)
               WHEN "2"
                 MOVE  "◯"         TO  SC03-PTENSUKBN  (2)
                 MOVE  "◯"         TO  SC03-TENSUKBN   (CNT-LINE 3)
                 MOVE  "◯"         TO  SC03-NYUGAI-KBN (CNT-LINE 2)
             END-EVALUATE
      *
      *    受給資格証記号番号
             MOVE  MF100-KOHJKYSNUM TO  SC03-JKYSNUM (CNT-LINE)
      *
      *    保険者名
           INITIALIZE                       HKNJAINF-REC
           MOVE     SYS-1001-HOSPNUM    TO  HKNJA-HOSPNUM
           MOVE     MF100-HKNJANUM-KEY  TO  HKNJA-HKNJANUM
           MOVE     HKNJAINF-REC        TO  MCPDATA-REC
           PERFORM  900-HKNJAINF-INV-SEC
           IF  FLG-HKNJAINF             =   ZERO
               MOVE    HKNJA-HKNJANAME  TO  SC03-NKJANAME (CNT-LINE)
           ELSE
               MOVE    SPACE            TO  SC03-NKJANAME (CNT-LINE)
           END-IF   
      *
      *    氏名
             MOVE  MF100-NAME             TO  SC03-NAME (CNT-LINE)
             PERFORM 900-PTHKNDB-SEC
             IF  MF100-KEY(5:5)          =   "30005" OR "30028"
                 MOVE  PTHKN-HIHKNJANAME  TO  SC03-NAME2 (1)
                                              SC03-NAME2 (2)
             ELSE
                 MOVE  PTHKN-HIHKNJANAME  TO  SC03-NAME2 (CNT-LINE)
             END-IF
      *
      *    性別 
             IF  MF100-SEX          =  "1"
                 MOVE  "◯"         TO  SC03-SEX-M (CNT-LINE)
             ELSE
                 MOVE  "◯"         TO  SC03-SEX-F (CNT-LINE)
             END-IF
      *
      *    生年月日
             MOVE  MF100-BIRTHDAY   TO  SC03-BIRTHDAY (CNT-LINE)
      *
             MOVE  MF100-BIRTHDAY   TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  LNK-DAY2-EDTYMD3 TO  SC03-BIRTHDAYK (CNT-LINE)
      *
      *    保険者番号
             MOVE  MF100-HKNJANUM   TO  SC03-HKNJANUM (CNT-LINE)
      *
      *    加入保険
             EVALUATE  MF100-HKNNUM
               WHEN  "060"
                 MOVE  "○"         TO  SC03-HKNKBN-KOK (CNT-LINE)
               WHEN  "067"
                 MOVE  "○"         TO  SC03-HKNKBN-KOK (CNT-LINE)
               WHEN OTHER
                 MOVE  "○"         TO  SC03-HKNKBN-SYHH (CNT-LINE)
             END-EVALUATE 
             IF  MF100-KEY(5:5)    =   "30024"
                 IF  MF100-HKNNUM   =   "060" OR "067"
                     CONTINUE
                 ELSE
                     IF  MF100-HONKZKKBN    =  "2"
                         MOVE  SPACE TO  SC03-HKNKBN-SYHH (CNT-LINE)
                         MOVE  "○"  TO  SC03-HKNKBN-SYHK (CNT-LINE)
                     END-IF
                 END-IF
             END-IF
      *    加入保険
             EVALUATE  MF100-HKNNUM
               WHEN  "001"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 1)
               WHEN  "006"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 2)
               WHEN  "003"
                  MOVE "○"         TO  SC03-HKNSYUBETU (CNT-LINE 3)
               WHEN  "004"
                  MOVE "○"         TO  SC03-HKNSYUBETU (CNT-LINE 3)
               WHEN  "002"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 4)
               WHEN  "031"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 5)
               WHEN  "032"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 5)
               WHEN  "033"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 5)
               WHEN  "034"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 5)
               WHEN  "060"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 6)
               WHEN  "067"
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 6)
               WHEN OTHER
                 MOVE  "○"         TO  SC03-HKNSYUBETU (CNT-LINE 7)
             END-EVALUATE 
      *
             IF  MF100-KEY(5:5)    =   "30045"
                 IF  MF100-HKNNUM   =   "060" OR "067"
                     MOVE   "国保"  TO  SC03-BIKO (CNT-LINE)
                 ELSE
                     MOVE   "社保"  TO  SC03-BIKO (CNT-LINE)
                 END-IF
             END-IF
      *
      *    記号・番号
             MOVE  MF100-KIGO(1:20) TO  SC03-KIGO (CNT-LINE)
             MOVE  MF100-NUM(1:20)  TO  SC03-NUM  (CNT-LINE)
      *
      *    本人家族
             EVALUATE  MF100-HONKZKKBN
               WHEN  "1"
                 MOVE  "○"         TO  SC03-HONKZKKBN (CNT-LINE 1)
               WHEN  "2"
                 MOVE  "○"         TO  SC03-HONKZKKBN (CNT-LINE 2)
             END-EVALUATE 
             IF  (MF100-KEY(5:5)   =   "30003" OR "30020")  AND
                 (MF100-HKNNUM      =   "060" OR "067")
                 MOVE    SPACE      TO  SC03-HONKZKKBN (CNT-LINE 1)
                                        SC03-HONKZKKBN (CNT-LINE 2)
             END-IF
      *
      *    給付割合
             COMPUTE WRK-FTNRATEX   =   MF100-KYURATE / 10
             EVALUATE  WRK-FTNRATEX
               WHEN  3
                 MOVE  "○"         TO  SC03-FTNWARIAI-3   (CNT-LINE)
                 MOVE  "○"         TO  SC03-FTNWARIAI1    (CNT-LINE 1)
               WHEN  2
                 MOVE  "○"         TO  SC03-FTNWARIAI-2   (CNT-LINE)
                 MOVE  "○"         TO  SC03-FTNWARIAI1    (CNT-LINE 2)
               WHEN  1
                 MOVE  "○"         TO  SC03-FTNWARIAI-9   (CNT-LINE)
               WHEN OTHER
                 MOVE  WRK-FTNRATEX TO  SC03-FTNWARIAI-ETC (CNT-LINE)
                 MOVE  WRK-FTNRATEX TO  SC03-FTNWARIAI2    (CNT-LINE)
             END-EVALUATE
             IF  MF100-KEY(5:5)    =   "30031" OR "30035" OR "30038"
                 MOVE  WRK-FTNRATEX TO  SC03-FTNWARIAI-ETC (CNT-LINE)
             END-IF
      *
      *    診療月
             MOVE  SPACE            TO  SC03-SRYM (CNT-LINE)
             MOVE  MF100-SRYYM(5:2) TO  SC03-SRMM (CNT-LINE)
             MOVE  MF100-SRYYM(5:2) TO  SC03-SRYM (CNT-LINE)
      *
      *    点数表
             EVALUATE  MF100-NYUGAIKBN
               WHEN "1"
                 MOVE  "○"         TO  SC03-TENSUKBN (CNT-LINE 1)
               WHEN "2"
                 MOVE  "○"         TO  SC03-TENSUKBN (CNT-LINE 3)
             END-EVALUATE
      *
      *    入院日数
             EVALUATE  MF100-NYUGAIKBN
               WHEN "1"
                 MOVE  MF100-JNISSU   TO  WRK-NYUNISSUX
                 MOVE  WRK-NYUNISSUX  TO  SC03-NYUNISSU (CNT-LINE)
               WHEN "2"
                 MOVE  SPACE          TO  SC03-NYUNISSU (CNT-LINE)
             END-EVALUATE
      *      
      *    診療実日数開始日
             MOVE  MF100-SRYYM         TO  WRK-SYMD (1:6)
             MOVE  "01"                TO  WRK-SYMD (7:2)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  WRK-HENYMDG(05:04)  TO SC03-SRNISU-FROM-YY (CNT-LINE)
             MOVE  WRK-HENYMDG(11:04)  TO SC03-SRNISU-FROM-MM (CNT-LINE)
             MOVE "０１"               TO SC03-SRNISU-FROM-DD (CNT-LINE)
      *    診療実日数終了日
             MOVE  WRK-HENYMDG(05:04)  TO SC03-SRNISU-TO-YY (CNT-LINE)
             MOVE  WRK-HENYMDG(11:04)  TO SC03-SRNISU-TO-MM (CNT-LINE)
             MOVE "　末"               TO SC03-SRNISU-TO-DD (CNT-LINE)
      *    実日数
             IF  MF100-KEY(5:5)   =   "30003" OR "30020" OR "30031" OR
                                       "30024"
                 IF  MF100-NYUGAIKBN   =   "1"
                     MOVE  SPACE        TO  SC03-SRNISU (CNT-LINE)
                 ELSE
                     MOVE  MF100-JNISSU TO  WRK-JNISSUX
                     MOVE  WRK-JNISSUX  TO  SC03-SRNISU (CNT-LINE)
                 END-IF
             ELSE
                 MOVE  MF100-JNISSU     TO  WRK-JNISSUX
                 MOVE  WRK-JNISSUX      TO  SC03-SRNISU (CNT-LINE)
             END-IF
      *
      *    点数
             MOVE  MF100-TOTALTEN      TO  WRK-GTENSUX
             IF  MF100-KEY(5:5)       =   "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     MOVE  WRK-GTENSUX TO  SC03-GTENSUN (CNT-LINE)
                   ELSE
                     MOVE  WRK-GTENSUX TO  SC03-GTENSU (CNT-LINE)
                 END-IF
             ELSE
                 MOVE  WRK-GTENSUX     TO  SC03-GTENSU (CNT-LINE)
             END-IF
      *
      *    総点数×割合
             COMPUTE  WRK-GTEN-WARIX    =  MF100-TOTALTEN *
                                           WRK-FTNRATEX
             MOVE  WRK-GTEN-WARIX   TO  SC03-GTEN-WARI (CNT-LINE)
      *
      *    薬剤一部負担金
             MOVE  MF100-YKZFTN     TO  WRK-YKZFTNX
             MOVE  WRK-YKZFTNX      TO  SC03-YKZITBFTN (CNT-LINE)
      *
      *    合計金額
             IF  MF100-FTNMONEY  NOT = ZERO 
                 IF  MF100-KEY(5:5)     =  "30017"
                     MOVE   MF100-FTNMONEY TO  WRK-KEISAN6
                 ELSE
                     COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN 
                                         *  MF100-KYURATE
                                         /  10
                 END-IF
             ELSE
                 COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN *  MF100-KYURATE
                                        /  10
             END-IF
             MOVE  WRK-KEISAN6      TO  WRK-GKINGKX
             IF  MF100-KEY(5:5)       =   "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     ADD   WRK-KEISAN6 TO  WRK-SKINN
                 ELSE
                     ADD   WRK-KEISAN6 TO  WRK-SKIN  
                 END-IF
             ELSE
                 ADD   WRK-KEISAN6     TO  WRK-SKIN
             END-IF
             ADD   WRK-KEISAN6         TO  WRK-GKIN
             IF  MF100-KEY(5:5)       =   "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     MOVE  WRK-GKINGKX TO  SC03-GKINGKN (CNT-LINE)
                 ELSE
                     MOVE  WRK-GKINGKX TO  SC03-GKINGK (CNT-LINE)
                 END-IF
             ELSE
                 MOVE  WRK-GKINGKX     TO  SC03-GKINGK (CNT-LINE)
             END-IF
      *    総費用
             COMPUTE WRK-KEISAN8    =   MF100-TOTALTEN *  10
             MOVE  WRK-KEISAN8      TO  WRK-HIYOUX
             MOVE  WRK-HIYOUX       TO  SC03-HIYOU (CNT-LINE)
      *
      *    医療費請求額
             MOVE  WRK-GKINGKX      TO  SC03-KOHIITBFTN (CNT-LINE)
      *
      *    合計エリア加算
             IF  WRK-KAIP-GOUKEI       =  "1"
                 COMPUTE  WRK-KOHFTN   =   WRK-KOHFTN   + WRK-KEISAN6
                 COMPUTE  WRK-YKZFTN   =   WRK-YKZFTN   + MF100-YKZFTN
                 COMPUTE  WRK-KOHKENSU =   WRK-KOHKENSU + 1
                 IF  MF100-YKZFTN  NOT =   ZERO
                     COMPUTE  WRK-YKZKENSU =   WRK-YKZKENSU + 1
                 END-IF
             END-IF
      *
             COMPUTE  WRK-TOTALTEN2 =   WRK-TOTALTEN2 + MF100-TOTALTEN
             COMPUTE  WRK-KOHFTN2   =   WRK-KOHFTN2   + WRK-KEISAN6
             COMPUTE  WRK-YKZFTN2   =   WRK-YKZFTN2   + MF100-YKZFTN
             COMPUTE  WRK-KOHKENSU2 =   WRK-KOHKENSU2 + 1
             IF  MF100-YKZFTN   NOT =   ZERO
                 COMPUTE  WRK-YKZKENSU2 =   WRK-YKZKENSU2 + 1
             END-IF
      *
           EVALUATE  MF100-HKNNUM
             WHEN  "060"
               ADD    1             TO  WRK-SKENSUK
             WHEN  "067"
               ADD    1             TO  WRK-SKENSUK
             WHEN OTHER
               ADD    1             TO  WRK-SKENSUS
           END-EVALUATE
      *    入院/入院外 件数
           EVALUATE MF100-NYUGAIKBN
             WHEN "1"
               ADD     1            TO  WRK-SKENSUNYU
             WHEN "2"
               ADD     1            TO  WRK-SKENSUGAI
           END-EVALUATE
           ADD    1                 TO  WRK-SKENSU
           ADD    1                 TO  WRK-GKENSU
           IF  MF100-KEY(5:5)      =   "30031"
               IF  MF100-NYUGAIKBN      =   "1"
                   ADD    MF100-TOTALTEN   TO  WRK-STENSUN
               ELSE
                   ADD    MF100-TOTALTEN   TO  WRK-STENSU
               END-IF
           ELSE
               ADD    MF100-TOTALTEN       TO  WRK-STENSU 
           END-IF
           ADD    MF100-YKZFTN      TO  WRK-SYKZFTN
           IF  MF100-NYUGAIKBN      =   1
               ADD    MF100-JNISSU  TO  WRK-SNNISU
           ELSE
               ADD    MF100-JNISSU  TO  WRK-SNISU
           END-IF
      *
      *    市町村ブレイク用キーセット 
             MOVE    MF100-KEY      TO  WRK-MF100-KEY   
      *
      *    レセプト明細読込
             PERFORM 900-MF100-READ-SEC 
           END-PERFORM
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
      *    医療費請求額
           MOVE  WRK-TOTALTEN2         TO  WRK-GTENSUX
           MOVE  WRK-GTENSUX           TO  SC03-GOKEI-GTENSU
      *
      *    医療費請求額
           MOVE  WRK-KOHFTN2           TO  WRK-GKINGKX
           MOVE  WRK-GKINGKX           TO  SC03-GOKEI-GITBFTN
      *
      *    薬剤一部負担請求額
           MOVE  WRK-YKZFTN2           TO  WRK-YKZFTNX
           MOVE  WRK-YKZFTNX           TO  SC03-GOKEI-GYKZFTN
      *
      *    医療費請求件数
           MOVE  WRK-KOHKENSU2         TO  WRK-KOHKENSUX
           MOVE  WRK-KOHKENSUX         TO  SC03-GOKEI-GKEN
      *
      *    薬剤一部負担請求件数
           MOVE  WRK-YKZKENSU2         TO  WRK-YKZKENSUX
           MOVE  WRK-YKZKENSUX         TO  SC03-GOKEI-GYKZKEN
      *
           MOVE    WRK-SKENSU          TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC03-SKEN
                                           SC03-SKEN2
           MOVE    WRK-SKENSUK         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC03-SKENK
           MOVE    WRK-SKENSUS         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC03-SKENS
           MOVE    WRK-SKENSUNYU       TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC03-SKENNYU
           MOVE    WRK-SKENSUGAI       TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC03-SKENGAI
           MOVE    WRK-STENSU          TO  WRK-STENSUX
           MOVE    WRK-STENSUX         TO  SC03-STENSU
           MOVE    WRK-STENSUN         TO  WRK-STENSUX
           MOVE    WRK-STENSUX         TO  SC03-STENSUN
           MOVE    WRK-SKIN            TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC03-SKIN
           MOVE    WRK-SKINN           TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC03-SKINN
           MOVE    WRK-SYKZFTN         TO  WRK-SYKZFTNX
           MOVE    WRK-SYKZFTNX        TO  SC03-SYKZFTN
           MOVE    WRK-SNISU           TO  WRK-SNISUX
           MOVE    WRK-SNISUX          TO  SC03-SNISU
           MOVE    WRK-SNNISU          TO  WRK-SNISUX
           MOVE    WRK-SNISUX          TO  SC03-SNNISU
      *
           MOVE    ZERO                TO  WRK-SKENSU
                                           WRK-SKENSUK
                                           WRK-SKENSUS
                                           WRK-SKENSUNYU
                                           WRK-SKENSUGAI
                                           WRK-STENSU
                                           WRK-STENSUN
                                           WRK-SKIN
                                           WRK-SKINN
                                           WRK-SYKZFTN
                                           WRK-SNISU
                                           WRK-SNNISU
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
           MOVE    SPACE               TO  SPRT-PRTID
      *
           IF  WRK-KAIP-PNO        NOT =  "003"
               MOVE   "SC3003.red"     TO  SPRT-PRTID
           ELSE
               IF  FLG-KATSURAGI       =   1
                   IF  WRK-KAIP-PNO    =  "018"
                       MOVE   "SC3003"       TO  WRK-MID 
                       MOVE    WRK-KAIP-PNO  TO  WRK-MPRT-CTV
                       MOVE   "1.red"        TO  WRK-MFILE
                       MOVE    WRK-MPRT-ID   TO  SPRT-PRTID
                   ELSE
                       MOVE   "SC3003"       TO  WRK-ID 
                       MOVE    WRK-KAIP-PNO  TO  WRK-PRT-CTV
                       MOVE   ".red"         TO  WRK-FILE
                       MOVE    WRK-PRT-ID    TO  SPRT-PRTID
                   END-IF
               ELSE
                IF  FLG-HASIMOTO        =   1
                    IF  WRK-KAIP-PNO    =  "003"
                        MOVE   "SC3003"       TO  WRK-MID 
                        MOVE    WRK-KAIP-PNO  TO  WRK-MPRT-CTV
                        MOVE   "1.red"        TO  WRK-MFILE
                        MOVE    WRK-MPRT-ID   TO  SPRT-PRTID
                    ELSE
                        MOVE   "SC3003"       TO  WRK-ID 
                        MOVE    WRK-KAIP-PNO  TO  WRK-PRT-CTV
                        MOVE   ".red"         TO  WRK-FILE
                        MOVE    WRK-PRT-ID    TO  SPRT-PRTID
                    END-IF
                ELSE
                 IF  FLG-KUDOYAMA        =   1
                     IF  WRK-KAIP-PNO    =  "020"
                         MOVE   "SC3003"       TO  WRK-MID 
                         MOVE    WRK-KAIP-PNO  TO  WRK-MPRT-CTV
                         MOVE   "1.red"        TO  WRK-MFILE
                         MOVE    WRK-MPRT-ID   TO  SPRT-PRTID
                     ELSE
                         MOVE   "SC3003"       TO  WRK-ID 
                         MOVE    WRK-KAIP-PNO  TO  WRK-PRT-CTV
                         MOVE   ".red"         TO  WRK-FILE
                         MOVE    WRK-PRT-ID    TO  SPRT-PRTID
                     END-IF
                 ELSE
                   MOVE   "SC3003"           TO  WRK-ID 
                   MOVE    WRK-KAIP-PNO      TO  WRK-PRT-CTV
                   MOVE   ".red"             TO  WRK-FILE
                   MOVE    WRK-PRT-ID        TO  SPRT-PRTID
                 END-IF
                END-IF
               END-IF
           END-IF
           IF  WRK-KAIP-PNO            =  "018"
               MOVE    1               TO  FLG-KATSURAGI
           ELSE
               MOVE    ZERO            TO  FLG-KATSURAGI
           END-IF
           IF  WRK-KAIP-PNO            =  "003"
               MOVE    1               TO  FLG-HASIMOTO
           ELSE
               MOVE    ZERO            TO  FLG-HASIMOTO
           END-IF
           IF  WRK-KAIP-PNO            =  "020"
               MOVE    1               TO  FLG-KUDOYAMA
           ELSE
               MOVE    ZERO            TO  FLG-KUDOYAMA
           END-IF
      *
           MOVE    "ひとり親"          TO  SPRT-TITLE
           MOVE    SC03                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               ADD     1               TO  CNT-PRINT
           ELSE
               MOVE    "帳票ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *    ワーク加算エリアクリア
           MOVE    ZERO            TO  WRK-KOHFTN2
           MOVE    ZERO            TO  WRK-YKZFTN2
           MOVE    ZERO            TO  WRK-KOHKENSU2
           MOVE    ZERO            TO  WRK-YKZKENSU2
           MOVE    ZERO            TO  WRK-TOTALTEN2
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC  SECTION.
      *
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE  "21"      TO  LNK-DAY2-IRAI
           MOVE  WRK-SYMD  TO  LNK-DAY2-YMD
           CALL  "ORCSDAY" USING  STS-AREA-DAY
                                  LNK-DAY2-AREA
           MOVE  LNK-DAY2-EDTYMD3  TO  WRK-HENYMDG
           MOVE  LNK-DAY2-EDTYMD1  TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    市町村データ取り込み
      *****************************************************************
       31013-KAIP-SEC       SECTION.
      *
           MOVE  MF100-KEY (7:3)      TO  WRK-KAIP-STSCD
           SET  IDX-MEI                TO  1  
           MOVE    TBL-KAIP-VAL        TO  TBL-KAIP
           SEARCH  TBL-KAIP-TBL   VARYING  IDX-MEI 
                                       AT  END
                   MOVE    "000"       TO  WRK-KAIP-PNO
                   MOVE    "05"        TO  WRK-KAIP-GYO
                   MOVE    "0"         TO  WRK-KAIP-GOUKEI
             WHEN  WRK-KAIP-STSCD      =   TBL-KAIP-STSCD (IDX-MEI)     
                   MOVE    TBL-KAIP-PNO (IDX-MEI)                     
                                       TO  WRK-KAIP-PNO 
                   MOVE    TBL-KAIP-GYO (IDX-MEI)
                                       TO  WRK-KAIP-GYO  
                   MOVE    TBL-KAIP-GOUKEI (IDX-MEI)
                                       TO  WRK-KAIP-GOUKEI            
           END-SEARCH
           .
       31013-KAIP-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計取り込み
      *****************************************************************
       31014-GOUKEI-GET-SEC       SECTION.
      *
           SET  IDX-GOU            TO  1  
           SEARCH  SUM-GOKEI       VARYING  IDX-GOU 
                                   AT  END   
               MOVE  ZERO          TO  WRK-GKENSUX
               MOVE  ZERO          TO  WRK-GKOHFTNX
             WHEN    WRK-STSCD     =   SUM-STSCD (IDX-GOU)
  　　*　  請求件数
               MOVE  SUM-GKENSU  (IDX-GOU) TO  WRK-GKENSUX
  　           MOVE  WRK-GKENSUX   TO  SC03-GKEN
                                       SC03-GKEN2
      *    請求件数（入院）
               MOVE  SUM-GKENSUNYU (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC03-GKENNYU
      *    請求件数（外来）
               MOVE  SUM-GKENSUGAI (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC03-GKENGAI
  　　*　  請求件数（国保）
               MOVE  SUM-GKENSUK (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC03-GKENK
  　　*　  請求件数（社保）
               MOVE  SUM-GKENSUS (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC03-GKENS
      *    点数
               MOVE  SUM-TENSU   (IDX-GOU) TO  WRK-GTENSUX
               MOVE  WRK-GTENSUX   TO  SC03-GGTENSU
      *    請求金額
               IF  WRK-STSCD       =   "012" OR "035" OR "038"
                   MOVE  SUM-GTESU (IDX-GOU) TO  WRK-GTESUX
                   MOVE  WRK-GTESUX    TO   SC03-GTESU
                   MOVE  SUM-GKOHFTN (IDX-GOU) TO WRK-GKIN2
                   ADD   SUM-GTESU (IDX-GOU) TO  WRK-GKIN2
                   MOVE  WRK-GKIN2     TO  WRK-GKIN2X
                   MOVE  WRK-GKIN2X    TO   SC03-GKIN2
                   MOVE  SUM-GKOHFTN (IDX-GOU)  TO  WRK-GKOHFTNX
                   MOVE  WRK-GKOHFTNX  TO  SC03-GKIN
               ELSE
                   MOVE  SUM-GKOHFTN (IDX-GOU)  TO  WRK-GKOHFTNX
                   MOVE  WRK-GKOHFTNX  TO  SC03-GKIN
               END-IF
      *    薬剤負担
               MOVE  SUM-GYKZFTN (IDX-GOU)  TO  WRK-GYKZFTNX
               MOVE  WRK-GYKZFTNX  TO  SC03-GYKZFTN
           END-SEARCH
           .
       31014-GOUKEI-GET-EXT.
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC  SECTION.
      *
           CLOSE  MF100-FILE
      *
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU3004 IN   "  CNT-MF100
           DISPLAY "*** SEIKYU3004 PAGE "  CNT-PRINT
           DISPLAY "*** SEIKYU3004 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-MF100-READ-SEC  SECTION.
      *
           READ  MF100-FILE  NEXT
             AT  END
               MOVE  1             TO  FLG-END
               MOVE  HIGH-VALUE    TO  MF100-KEY
             NOT AT  END
                 ADD   1                 TO  CNT-MF100
                 MOVE  MF100-KEY (7:3)  TO  KEY-N-STSCD
           END-READ
           .
       900-MF100-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE   "tbl_syskanri"      TO  MCP-TABLE
           MOVE   "key10"             TO  MCP-PATHNAME
            CALL  "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF  MCP-RC                  =   ZERO
               MOVE  "DBFETCH"         TO  MCP-FUNC
               MOVE  "tbl_syskanri"    TO  MCP-TABLE
               MOVE  "key10"           TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"       USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC            =  ZERO
               MOVE  ZERO              TO  FLG-SYSKANRI
             ELSE
               MOVE  1                 TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE   "tbl_syskanri"       TO  MCP-TABLE
           MOVE   "key10"              TO  MCP-PATHNAME
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    患者マスタ　検索 
      *****************************************************************
       900-PTHKNDB-SEC                    SECTION.
      *
           INITIALIZE                     PTHKNINF-REC
           MOVE    WRK-PARA-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    MF100-PTID             TO  PTHKN-PTID
           MOVE    MF100-HKNID            TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_pthkninf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF  MCP-RC  =  ZERO
                   MOVE    MCPDATA-REC    TO  PTHKNINF-REC
               END-IF  
           END-IF        
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_pthkninf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           .
       900-PTHKNDB-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC      SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hknjainf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC TO  HKNJAINF-REC
               ELSE
                   MOVE    1           TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-HKNJAINF-INV-EXT.
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
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
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
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
