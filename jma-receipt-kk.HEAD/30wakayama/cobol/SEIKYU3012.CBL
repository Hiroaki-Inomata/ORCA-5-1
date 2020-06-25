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
       PROGRAM-ID.     SEIKYU3012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 和歌山重度心身障害児(老人除く)医療費請求書
      *                      町単独分
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/01/16    楠本
      *****************************************************************
      * wakayama chihou kouhi contribution
      * Special thanks to michiyo noda,motohide takagaki,katsunori yoneda
      * for help in development
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      * 
      *  01.00.07       楠本    05/11/30        紀の川市
      *                                         印南町　日高町
      *  01.00.08       楠本    06/07/07        MONFUNC DBCLOSESURSOR 対応
      *  02.00.00       楠本　　07/10/11    ORCA4.0対応
      *  02.00.01       吉川    10/06/02        SPA-AREA初期化
      *                                         システム管理マスタ「1901」の
      *                                         医療機関情報を参照
      *  02.00.02       吉川    10/12/08        open-cobol1.0対応
      *  02.00.03       吉川    11/05/30        ver4.6.0での印字不具合対応
      *  02.00.04       吉川    13/01/11        医療機関情報編集対応
      *  02.00.05       吉川    14/10/24        ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.06       萩野    16/02/12        医療機関編集情報への開設者名追加対応
      *  02.00.07       萩野    17/02/14        医療機関コード変更時、
      *                                         医療機関コード別の作成処理対応
      *  02.00.08       萩野    18/04/09        SITEKBN変更（動作に変更なし）
      *  02.00.09       萩野    18/12/12        修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル  
           SELECT  MF100-FILE    ASSIGN  MF100PARA
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
      *    請求書用ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI3003.INC".
      *
006900*    エラーファイル
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
           COPY  "SC3011.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA     PIC X(02).
           03  STS-MF100     PIC X(02).
           03  STS-RECEERR  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END       PIC 9(01).
           03  FLG-SYSKANRI  PIC 9(01).
           03  FLG-HKNJAINF  PIC 9(01).
           03  FLG-KEY       PIC 9(01). 
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE  PIC 9(02).
           03  CNT-PAGE  PIC 9(03).
           03  CNT-PRINT PIC 9(06).
           03  CNT-MF100 PIC 9(05).
           03  CNT-SUM   PIC 9(03).
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
           03  WRK-KEISAN6   PIC 9(09).
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
      *    帳票パターン判定用エリア
           03  WRK-KAIP.
               05  WRK-KAIP-STSCD      PIC X(03).
               05  WRK-KAIP-PNO        PIC X(03).
               05  WRK-KAIP-GYO        PIC X(02).
               05  WRK-KAIP-GOUKEI     PIC X(01).
      *
      *    改ページ判定テーブル（市町村追加時改定要）
       01  TBL-KAIP. 
           03  TBL-KAIP-TBL          OCCURS 3
                                     INDEXED  BY  IDX-MEI.
               05  TBL-KAIP-STSCD    PIC X(03). 
               05  TBL-KAIP-PNO      PIC X(03).
               05  TBL-KAIP-GYO      PIC X(02).
               05  TBL-KAIP-GOUKEI   PIC X(01).
       01  TBL-KAIP-VAL.
      *                                            紀の川市 
           03  TBL-KAIP-VAL1         PIC X(09) VALUE "012012070".
      *                                          印南町
           03  TBL-KAIP-VAL2         PIC X(09) VALUE "037037080".
      *                                          日高町
           03  TBL-KAIP-VAL3         PIC X(09) VALUE "029029080".

      *
      *    編集用エリア
           03  WRK-FTNRATEX    PIC 9.
           03  WRK-GKENSUX     PIC ZZ9.
           03  WRK-GKOHFTNX    PIC ZZ,ZZ9.
           03  WRK-YKZFTNX     PIC ZZ,ZZ9.
           03  WRK-NYUNISSUX   PIC Z9.
           03  WRK-GTENSUX     PIC ZZ,ZZ9.
           03  WRK-GTEN-WARIX  PIC ZZZ,ZZ9.
           03  WRK-GKINGKX     PIC ZZZ,ZZ9.
      *
      *    合計エリア
           03  WRK-KOHFTN      PIC 9(09).
           03  WRK-YKZFTN      PIC 9(09).
           03  WRK-KOHKENSU    PIC 9(05).
           03  WRK-YKZKENSU    PIC 9(05).
           03  WRK-SKENSU      PIC 9(04).
           03  WRK-SKENSUK     PIC 9(04).
           03  WRK-SKENSUS     PIC 9(04).
           03  WRK-STENSU      PIC 9(10).
      *
       01  SUM-AREA  VALUE   ZERO.
           03  SUM-GOKEI  OCCURS  60
                              INDEXED  BY  IDX-GOU.
               05  SUM-STSCD   PIC X(03).
               05  SUM-NYUGAI  PIC X(01).
               05  SUM-GKENSU  PIC 9(10).
               05  SUM-GKOHFTN PIC 9(10).
               05  SUM-TENSU   PIC 9(10).
               05  SUM-GKENSUK PIC 9(10).
               05  SUM-GKENSUS PIC 9(10).
      *
      *    キーエリア
           03  WRK-MF100-KEY.
               05  FILLER                    PIC X(4).
               05  WRK-MF100-STSCD.
                   07  FILLER                PIC X(02).
                   07  WRK-MF100-KOHFTNJA-C  PIC X(03).
                   07  FILLER                PIC X(01).
               05  WRK-NYUGAIKBNP            PIC X(01).
               05  WRK-SRYM                  PIC X(06).
               05  FILLER                    PIC X(59).
      *
      *    ＲＥＤファイルエリア   
           03  WRK-PRT-ID.
               05  WRK-ID          PIC X(06).
               05  WRK-PRT-CTV     PIC X(03). 
               05  WRK-FILE        PIC X(04).
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
               05  KEY-N-NYUGAI PIC X(01).
           03  KEY-OLD.
               05  KEY-O-STSCD  PIC X(03).
               05  KEY-O-NYUGAI PIC X(01).
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
      *    医療機関情報−所在地、連絡先
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
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *     COPY    "CPORCMCP.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           INITIALIZE  SPA-AREA
      *
           ACCEPT  SYS-TIME  FROM  TIME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE  LNK-PRTKANRI-SRYYM    TO  WRK-SYMD (1:6)
           MOVE  "01"                  TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)     TO  WRK-DENPPRTYMWH
           MOVE  WRK-HENYMDG1          TO  WRK-SRYYM 
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
           MOVE  LNK-PRTKANRI-SKYYMD   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG           TO  WRK-SYSYMDWH
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"                TO  SYS-1001-KANRICD
           MOVE  "*"                   TO  SYS-1001-KBNCD
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
           MOVE  SYS-1001-REC          TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI            =   ZERO
             MOVE  MCPDATA-REC         TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU3012 SYS 1001 ERR ***" UPON CONSOLE
             MOVE  1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"                TO  SYS-1002-KANRICD
           MOVE  "*"                   TO  SYS-1002-KBNCD
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
           MOVE  SYS-1002-REC          TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI            =   ZERO
             MOVE  MCPDATA-REC         TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU3012 SYS 1002 ERR ***" UPON CONSOLE
             MOVE  1                   TO  FLG-END
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
           PERFORM  UNTIL  FLG-END  =  1
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
      *
      *    帳票印刷処理
             PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      ******************************************************************
      *    合計集計処理
      ******************************************************************
       210-GOKEI-SYUKEI-SEC  SECTION.
      
           INITIALIZE  SUM-AREA
           OPEN  INPUT  MF100-FILE
      
      *    レセプト明細読込
           PERFORM 900-MF100-READ-SEC
      *
           MOVE  KEY-NEW           TO  KEY-OLD
           MOVE  1                 TO  CNT-SUM
           MOVE  KEY-N-STSCD       TO  SUM-STSCD  (CNT-SUM)
           MOVE  KEY-N-NYUGAI      TO  SUM-NYUGAI (CNT-SUM)
      *
           PERFORM  UNTIL  FLG-END =   1
      *
             IF  KEY-NEW       NOT =   KEY-OLD
                 MOVE  KEY-NEW     TO  KEY-OLD
                 ADD   1           TO  CNT-SUM
                 MOVE  KEY-N-STSCD TO  SUM-STSCD   (CNT-SUM)
                 MOVE  KEY-N-NYUGAI  TO  SUM-NYUGAI (CNT-SUM)
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
                 MOVE   MF100-FTNMONEY TO  WRK-KEISAN6
             ELSE
                 COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN *  MF100-KYURATE
                                        /  10
             END-IF
             ADD  WRK-KEISAN6       TO  SUM-GKOHFTN (CNT-SUM)
      *
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
           INITIALIZE  SC11
      *
      *    市町村データ取り込み
           PERFORM 31013-KAIP-SEC
      *
      *    印刷日付
           MOVE  WRK-DENPPRTYMWH(05:04) TO  SC11-PRTYY
           MOVE  WRK-DENPPRTYMWH(11:04) TO  SC11-PRTMM
           MOVE  WRK-DENPPRTYMWH(1:16)  TO  SC11-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN       TO  SC11-HOSPCDN
      *
      *    市町村名
           MOVE     SPACE               TO  SC11-STSNAME
           MOVE     ZERO                TO  IDZ
           MOVE     1                   TO  FLG-HKNJAINF
           PERFORM UNTIL IDZ            >   9  OR
                         FLG-HKNJAINF   =   ZERO
             INITIALIZE                       HKNJAINF-REC
             MOVE     SYS-1001-HOSPNUM  TO  HKNJA-HOSPNUM
             MOVE     MF100-KEY(5:5)     TO  HKNJA-HKNJANUM (1:5)
             MOVE     IDZ(2:1)            TO  HKNJA-HKNJANUM (6:1)
             MOVE     HKNJAINF-REC      TO  MCPDATA-REC
             PERFORM  900-HKNJAINF-INV-SEC
             IF  FLG-HKNJAINF           =   ZERO
                 MOVE    HKNJA-HKNJANAME    TO  SC11-STSNAME
             END-IF
             ADD      1                 TO  IDZ
           END-PERFORM
      *
      *
      *    請求日付
           MOVE  WRK-SYSYMDWH           TO  SC11-SEIYMD
      *
           MOVE  SYS-1002-POST(1:3)     TO  SC11-POST(1:3)
           MOVE  "-"                    TO  SC11-POST(4:1)
           MOVE  SYS-1002-POST(4:4)     TO  SC11-POST(5:4)
      *
      *    住所
           MOVE  WRK-HOSPADRS           TO  SC11-ADRS
           INSPECT SC11-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE  WRK-HOSPNAME           TO  SC11-HOSPNAME
           INSPECT SC11-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    電話番号
           MOVE  WRK-HOSPTEL            TO  SC11-TEL
      *
      *    管理者名
           MOVE  WRK-KAISETUNAME        TO  SC11-KANRINAME
      *
           IF  FLG-KEY = 0
               MOVE    KEY-N-STSCD     TO  WRK-STSCD
               MOVE    KEY-N-NYUGAI    TO  WRK-NYUGAI
               PERFORM 31014-GOUKEI-GET-SEC
               MOVE  1  TO  FLG-KEY
           END-IF
      *
      *    振込み先銀行
           MOVE  KOUZA-GINKO            TO  SC11-BANKNAME
      *
      *    振込み先銀行支店
           MOVE  KOUZA-SITEN            TO  SC11-SITENNAME
      *
      *    口座種類
           EVALUATE   KOUZA-SYURUI
             WHEN "1"
               MOVE    "当座"           TO  SC11-SYUBETU
               MOVE    "○"             TO  SC11-KOUZA2
             WHEN "2"
               MOVE    "普通"           TO  SC11-SYUBETU
               MOVE    "○"             TO  SC11-KOUZA1
           END-EVALUATE
      *
      *    振込み先口座番号
           MOVE  KOUZA-KOUZANO          TO  SC11-KOZANUM
      *
      *    振込み先名義人
           MOVE  KOUZA-MEIGI            TO  SC11-MEIGI
      *
      *    振込み先名義人 カナ
           MOVE  KOUZA-KANAMEIGI        TO  SC11-KANAMEIGI
      * 
      *    口座変更
           EVALUATE   KOUZA-HENKO
             WHEN "1"
               MOVE    "○"             TO  SC11-HENKO1 
             WHEN "2"
               MOVE    "○"             TO  SC11-HENKO2
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
           MOVE  ZERO  TO  CNT-LINE
           MOVE  MF100-KEY             TO  WRK-MF100-KEY 
      *
           PERFORM  UNTIL  FLG-END  =  1
                    OR  CNT-LINE    =  WRK-KAIP-GYO
                    OR  MF100-KEY (1:17) NOT =  WRK-MF100-KEY (1:17) 
      *
             ADD  1  TO  CNT-LINE
      *    負担者番号
             MOVE  MF100-FTNJANUM      TO  SC11-FTNJANUM (CNT-LINE)
      *    受給資格証記号番号
             MOVE  MF100-KOHJKYSNUM(1:7)   TO  
                                           SC11-JKYSNUM (CNT-LINE)
      *    氏名
             MOVE  MF100-NAME          TO  SC11-NAME (CNT-LINE)
      *    性別 
             IF  MF100-SEX             =  "1"
                 MOVE  "○"            TO  SC11-SEX-M (CNT-LINE)
             ELSE
                 MOVE  "○"            TO  SC11-SEX-F (CNT-LINE)
             END-IF
      *    生年月日
             MOVE  MF100-BIRTHDAY      TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  LNK-DAY2-EDTYMD3    TO  SC11-BIRTHDAY (CNT-LINE)
      *    保険者番号
             MOVE  MF100-HKNJANUM      TO  SC11-HKNJANUM (CNT-LINE)
      *    記号・番号
             MOVE  MF100-KIGO(1:20)    TO  SC11-KIGO (CNT-LINE)
             MOVE  MF100-NUM(1:20)     TO  SC11-NUM (CNT-LINE)
      *    種別
             EVALUATE  MF100-HKNNUM
               WHEN  "001"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 1)
               WHEN  "006"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 2)
               WHEN  "003"
                  MOVE "○"            TO  SC11-HKNSYUBETU (CNT-LINE 3)
               WHEN  "004"
                  MOVE "○"            TO  SC11-HKNSYUBETU (CNT-LINE 3)
               WHEN  "002"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 4)
               WHEN  "031"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 5)
               WHEN  "032"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 5)
               WHEN  "033"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 5)
               WHEN  "034"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 5)
               WHEN  "060"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 6)
               WHEN  "067"
                 MOVE  "○"            TO  SC11-HKNSYUBETU (CNT-LINE 6)
             END-EVALUATE 
      *    診療実日数開始日
             MOVE  MF100-SRYYM         TO  WRK-SYMD (1:6)
             MOVE  "01"                TO  WRK-SYMD (7:2)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  WRK-HENYMDG(05:04) 
                            TO  SC11-SRNISU-FROM-YY (CNT-LINE)
             MOVE  WRK-HENYMDG(11:04)
                            TO  SC11-SRNISU-FROM-MM (CNT-LINE)
             MOVE "０１"                  
                            TO  SC11-SRNISU-FROM-DD (CNT-LINE)
      *    診療実日数終了日
             MOVE  WRK-HENYMDG(05:04)
                            TO  SC11-SRNISU-TO-YY  (CNT-LINE)
             MOVE  WRK-HENYMDG(11:04)
                            TO  SC11-SRNISU-TO-MM  (CNT-LINE)
             MOVE "　末"                  
                            TO  SC11-SRNISU-TO-DD  (CNT-LINE)
      *    診療実日数
             MOVE  MF100-JNISSU        TO  SC11-SRNISU  (CNT-LINE)
      *    本人家族
             EVALUATE  MF100-HONKZKKBN
               WHEN  "1"
                 MOVE  "○"            TO  SC11-HONKZKKBN (CNT-LINE 1)
               WHEN  "2"
                 MOVE  "○"            TO  SC11-HONKZKKBN (CNT-LINE 2)
             END-EVALUATE 
      *    給付割合
             COMPUTE WRK-FTNRATEX      =   MF100-KYURATE / 10
             EVALUATE  WRK-FTNRATEX
               WHEN  3
                 MOVE  "○"            TO  SC11-FTNWARIAI1 (CNT-LINE 1)
               WHEN  2
                 MOVE  "○"            TO  SC11-FTNWARIAI1 (CNT-LINE 2)
               WHEN OTHER
                 MOVE  WRK-FTNRATEX    TO  SC11-FTNWARIAI2 (CNT-LINE)
             END-EVALUATE
             EVALUATE  WRK-FTNRATEX
               WHEN  3
                 MOVE  "○"            TO  SC11-FTNWARIAI-7 (CNT-LINE)
               WHEN  2
                 MOVE  "○"            TO  SC11-FTNWARIAI-8 (CNT-LINE)
               WHEN  1
                 MOVE  "○"            TO  SC11-FTNWARIAI-9 (CNT-LINE)
               WHEN OTHER
                 MOVE  WRK-FTNRATEX    TO  SC11-FTNWARIAI-ETC (CNT-LINE)
             END-EVALUATE
      *    診療月
      *       MOVE  SPACE               TO  SC11-SRYM (CNT-LINE)
             MOVE  MF100-SRYYM(5:2)    TO  SC11-SRYM (CNT-LINE)
      *    点数表
             EVALUATE  MF100-NYUGAIKBN
               WHEN "1"
                 MOVE  "○"            TO  SC11-TENSUKBN (CNT-LINE 1)
                 MOVE  "○"            TO  SC11-PTENSUKBN(3)
               WHEN "2"
                 MOVE  "○"            TO  SC11-TENSUKBN (CNT-LINE 3)
                 MOVE  "○"            TO  SC11-PTENSUKBN(4)
             END-EVALUATE
      *    入院日数
             MOVE  ZERO                TO  WRK-NYUNISSUX
             MOVE  WRK-NYUNISSUX       TO  SC11-NYUNISSU (CNT-LINE)
      *    点数
             MOVE  MF100-TOTALTEN      TO  WRK-GTENSUX
             MOVE  WRK-GTENSUX         TO  SC11-GTENSU  (CNT-LINE)
      *    総点数×割合
             COMPUTE  WRK-GTEN-WARIX   =
                      MF100-TOTALTEN   *   WRK-FTNRATEX
             MOVE  WRK-GTEN-WARIX      TO  SC11-GTEN-WARI (CNT-LINE)
      *    薬剤一部負担金
             MOVE  MF100-YKZFTN        TO  WRK-YKZFTNX
             MOVE  WRK-YKZFTNX         TO  SC11-YKZITBFTN (CNT-LINE)
      *    合計金額
             IF  MF100-FTNMONEY  NOT = ZERO
                 MOVE   MF100-FTNMONEY TO  WRK-KEISAN6
             ELSE
                 COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN *  MF100-KYURATE
                                        /  10
             END-IF
             MOVE  WRK-KEISAN6         TO  WRK-GKINGKX
             MOVE  WRK-GKINGKX         TO  SC11-GKINGK (CNT-LINE)
             MOVE  WRK-GKINGKX         TO  SC11-KOHIITBFTN (CNT-LINE)     
      *
      *    市町村ブレイク用キーセット 
             MOVE    MF100-KEY         TO  WRK-MF100-KEY   
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
           IF  WRK-KAIP-PNO            =  "000"
               MOVE   "SC3011.red"     TO  SPRT-PRTID
           ELSE
               MOVE   "SC3011"         TO  WRK-ID 
               MOVE    WRK-KAIP-PNO    TO  WRK-PRT-CTV
               MOVE   ".red"           TO  WRK-FILE
               MOVE    WRK-PRT-ID      TO  SPRT-PRTID
           END-IF
           MOVE    "重度心身障害"      TO  SPRT-TITLE
           MOVE    SC11                TO  SPRT-PRTDATA
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
           MOVE  "21"                  TO  LNK-DAY2-IRAI
           MOVE  WRK-SYMD              TO  LNK-DAY2-YMD
           CALL  "ORCSDAY"          USING  STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE  LNK-DAY2-EDTYMD3      TO  WRK-HENYMDG
           MOVE  LNK-DAY2-EDTYMD1      TO  WRK-HENYMDG1
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
               MOVE  WRK-GKENSUX   TO  SC11-GKENSU
               MOVE  ZERO          TO  WRK-GKOHFTNX
               MOVE  WRK-GKOHFTNX  TO  SC11-GKOHFTN
             WHEN    WRK-STSCD     =   SUM-STSCD (IDX-GOU) AND
                     WRK-NYUGAI    =   SUM-NYUGAI (IDX-GOU)
      *    請求件数
               MOVE  SUM-GKENSU  (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC11-GKENSU
                                       SC11-GKEN
      *    請求金額
               MOVE  SUM-GKOHFTN (IDX-GOU)  TO  WRK-GKOHFTNX
               MOVE  WRK-GKOHFTNX  TO  SC11-GKOHFTN
      * 
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
           IF  STS-RECEERR             =  ZERO
             CONTINUE
           ELSE
             OPEN  OUTPUT  RECEERR-FILE
      *
             MOVE  WRK-RECEERR         TO  RECEERR-REC
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
           DISPLAY "*** SEIKYU3012 IN   "  CNT-MF100
           DISPLAY "*** SEIKYU3012 PAGE "  CNT-PRINT
           DISPLAY "*** SEIKYU3012 END ***"
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
               MOVE  1                 TO  FLG-END
               MOVE  HIGH-VALUE        TO  MF100-KEY
             NOT AT  END
               ADD   1                 TO  CNT-MF100
               MOVE  MF100-KEY (7:3)  TO  KEY-N-STSCD
               MOVE  MF100-KEY (11:1)  TO  KEY-N-NYUGAI
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
           MOVE  "DBSELECT"            TO  MCP-FUNC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
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
             IF  MCP-RC                =   ZERO
               MOVE  ZERO              TO  FLG-SYSKANRI
             ELSE
               MOVE  1                 TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
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
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                       SPA-AREA
           .
      *
       900-HKNJAINF-INV-EXT.
           EXIT.
      *=== シス管「1901」対応 by Yoshikawa(2010/06/02) start
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
