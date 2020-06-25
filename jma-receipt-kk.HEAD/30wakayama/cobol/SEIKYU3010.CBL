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
       PROGRAM-ID.     SEIKYU3010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 和歌山重度心身障害児(老人併用)医療費請求書(146,246)
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
      *  01.00.01       楠本    03/07/07        地方公費対応
      *                                         橋本市 御坊市
      *  01.00.02       楠本    05/01/13        田辺市
      *  01.00.03       楠本    05/04/20        粉河町　那賀町
      *  01.00.04       楠本    05/05/30        串本町
      *  01.00.05       楠本    05/08/26        上富田町
      *  01.00.06       楠本    05/10/07        日高川町
      *  01.00.07       楠本    05/11/30        由良町　美浜町
      *                                         印南町　日高町
      *  01.00.08       楠本    06/07/07        MONFUNC DBCLOSESURSOR 対応
      *  02.00.00       楠本　　07/10/11        ORCA4.0対応
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
      *    印刷用データ 名称領域
           COPY  "CPCOMMONPRT.INC".
      *    シェル用領域
           COPY  "CPCOMMONSHELL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01PARA//
                   BY         //RECEERR//.
           03      PIC X(04)   VALUE   ".dat".
      *
           COPY  "SC3009.INC".
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
           03  CNT-MF100  PIC 9(05).
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
      *
           03  WRK-SYMD.
               05  WRK-SYY   PIC 9(04).
               05  WRK-SMM   PIC 9(02).
               05  WRK-SDD   PIC 9(02).
           03  WRK-HENYMDG   PIC X(22).
           03  WRK-HENYMDG1  PIC X(09).
           03  WRK-KEISAN1   PIC 9(09).
           03  WRK-KEISAN6   PIC 9(09).
           03  WRK-KEISAN9   PIC 9(09).
      *
      *    編集用エリア
           03  WRK-FTNRATEX       PIC 9.
           03  WRK-GKENSUX        PIC ZZ9.
           03  WRK-GKOHFTNX       PIC ZZ,ZZ9.
           03  WRK-NYUNISSUX      PIC Z9.
           03  WRK-GTENSUX        PIC ZZ,ZZ9.
           03  WRK-GTEN-WARIX     PIC ZZZ,ZZ9.
           03  WRK-NYUGAIKBN      PIC 9.
           03  WRK-NISSUX         PIC Z9.
           03  WRK-TENSUX         PIC ZZ,ZZ9.
           03  WRK-FTNMONEYX      PIC ZZZ,ZZ9.
           03  WRK-YKZFTNX        PIC ZZZ,ZZ9.
           03  WRK-GGAIFTNMONEYX  PIC Z,ZZZ,ZZ9.
           03  WRK-GGAINISSUX     PIC Z9.
           03  WRK-GGAITENSUX     PIC ZZ,ZZ9.
           03  WRK-GOUKEIMONEYX   PIC Z,ZZZ,ZZ9.
           03  WRK-GINFTNMONEYX   PIC Z,ZZZ,ZZ9.
           03  WRK-GINFTNYKZX     PIC Z,ZZZ,ZZ9.
           03  WRK-GKINGKX        PIC ZZZ,ZZ9.
           03  WRK-SNISUX         PIC ZZ9.
           03  WRK-SKENSUX        PIC ZZ9.
           03  WRK-STENSUX        PIC ZZ,ZZ9.
           03  WRK-SKINX          PIC ZZ,ZZZ,ZZ9.
           03  WRK-SYKZFTNX       PIC ZZ,ZZZ,ZZ9.
      *
      *    合計エリア
           03  WRK-KOHFTN      PIC 9(09).
           03  WRK-YKZFTN      PIC 9(09).
           03  WRK-KOHKENSU    PIC 9(05).
           03  WRK-YKZKENSU    PIC 9(05).
           03  WRK-SNISU          PIC  9(04).
           03  WRK-SNNISU         PIC  9(04).
           03  WRK-SKENSU         PIC  9(04).
           03  WRK-SKENSUN        PIC  9(04).
           03  WRK-SKENSUG        PIC  9(04).
           03  WRK-SKENSUK        PIC  9(04).
           03  WRK-SKENSUS        PIC  9(04).
           03  WRK-STENSU         PIC  9(10).
           03  WRK-STENSUN        PIC  9(10).
           03  WRK-SKIN           PIC  9(10).
           03  WRK-SKINN          PIC  9(10).
           03  WRK-SKING          PIC  9(10).
           03  WRK-SYKZFTN        PIC  9(10).
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
           03  TBL-KAIP-TBL          OCCURS 10
                                     INDEXED  BY  IDX-MEI.
               05  TBL-KAIP-STSCD    PIC X(03). 
               05  TBL-KAIP-PNO      PIC X(03).
               05  TBL-KAIP-GYO      PIC X(02).
               05  TBL-KAIP-GOUKEI   PIC X(01).
       01  TBL-KAIP-VAL.
      *                                            橋本市   
           03  TBL-KAIP-VAL1         PIC X(09) VALUE "003003101". 
      *                                            御坊市   
           03  TBL-KAIP-VAL2         PIC X(09) VALUE "005005101". 
      *                                            田辺市   
           03  TBL-KAIP-VAL3         PIC X(09) VALUE "006006121". 
      *                                          紀の川市
           03  TBL-KAIP-VAL4         PIC X(09) VALUE "012012070".
      *                                            串本町
           03  TBL-KAIP-VAL5         PIC X(09) VALUE "045045080".
      *                                          上富田町
           03  TBL-KAIP-VAL6         PIC X(09) VALUE "042042080".
      *                                          日高川町
           03  TBL-KAIP-VAL7         PIC X(09) VALUE "031031080".
      *                                          由良町
           03  TBL-KAIP-VAL8         PIC X(09) VALUE "030030080".
      *                                          印南町
           03  TBL-KAIP-VAL9         PIC X(09) VALUE "037037080".
      *                                          日高町
           03  TBL-KAIP-VAL10        PIC X(09) VALUE "029029080".
      *    合計エリア
       01  SUM-AREA.
           03  SUM-GFTNMONEY-TBL OCCURS 2.
               05  SUM-GFTNMONEY  PIC  9(10).
           03  SUM-GNISU-TBL     OCCURS 2.
               05  SUM-GNISU      PIC  9(02).
           03  SUM-GTENSU-TBL    OCCURS 2.
               05  SUM-GTENSU     PIC  9(06).
           03  SUM-GFTNYJZ        PIC  9(10).
           03  SUM-GOKEI  OCCURS  50
                              INDEXED  BY  IDX-GOU.
               05  SUM-STSCD   PIC X(03).
               05  SUM-GKENSU  PIC 9(10).
               05  SUM-GKOHFTN PIC 9(10).
               05  SUM-TENSU   PIC 9(10).
               05  SUM-GKENSUK PIC 9(10).
               05  SUM-GKENSUS PIC 9(10).
      *
       01  WRK-KEY-AREA.
      *    キー用エリア
           03  WRK-MF100-KEY.
               05  FILLER          PIC X(4).
               05  WRK-MF100-STSCD.
                   07  FILLER      PIC X(02).
                   07  WRK-MF100-KOHFTNJA-C  PIC X(03).
                   07  FILLER PIC  X(01).
               05  WRK-NYUGAIKBNP  PIC X(01).
               05  WRK-SRYM        PIC X(06).
               05  FILLER          PIC X(59).   
           03  WRK-PRT-ID.
               05  WRK-ID          PIC X(06).
               05  WRK-PRT-CTV     PIC X(03). 
               05  WRK-FILE        PIC X(04).
      *
      *
       01  KEY-AREA  VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-STSCD  PIC X(03).
           03  KEY-OLD.
               05  KEY-O-STSCD  PIC X(03).
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
      *
      ******************************************************************
      *
       PROCEDURE  DIVISION
                  USING   WRK-PARA.
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
           INITIALIZE  KOUZA-PARA
           INITIALIZE  WRK-KEY-AREA
      *
           ACCEPT  SYS-TIME  FROM  TIME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE  LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE  "01"            TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG(1:16)  TO  WRK-DENPPRTYMWH
           MOVE  WRK-HENYMDG1       TO  WRK-SRYYM 
      *
           MOVE  "MF100ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)
                                  TO  MF100PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM    TO  MF100PARA-HOSPNUM
      *
           MOVE  "RECEERR"        TO  RECEERR-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)
                                  TO  RECEERR-TERMID
           MOVE  WRK-PARA-HOSPNUM    TO  RECEERR-HOSPNUM
      *
           OPEN  INPUT  MF100-FILE
      *
      *    システム管理マスタ（医療機関情報）
           PERFORM 120-HOSPINF-GET-SEC
      *
      *    口座名取得
           CALL    "KOUZASUB"          USING
                                       KOUZA-PARA
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
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1001-STYUKYMD(1:6)
                                           SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU3010 SYS 1001 ERR ***" UPON CONSOLE
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
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU3010 SYS 1002 ERR ***" UPON CONSOLE
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
      *    帳票編集<合計>処理
             PERFORM 330-GOKEI-HEN-SEC
      *
      *    帳票印刷処理
             PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
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
           MOVE  KEY-N-STSCD       TO  SUM-STSCD   (CNT-SUM)
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
      *    負担金
            IF  (MF100-FTNMONEY    NOT =   ZERO)    AND
                (KEY-N-STSCD           =   "017")
                ADD  MF100-FTNMONEY     TO  SUM-GKOHFTN (CNT-SUM)
            ELSE
              IF  (MF100-KEY (7:3)   =  "001" OR "012" OR "018") OR
                  (MF100-KEY (7:3)   =  "002" AND 
                   MF100-NYUGAIKBN = "2" ) 
                 COMPUTE WRK-KEISAN1    =   MF100-TOTALTEN * 
                                            MF100-KYURATE  /
                                            10
                 IF  MF100-NYUGAIKBN    =   "1"
                     IF  MF100-SHOKUJIFTNKBN    = "1" OR "2"
                         IF  WRK-KEISAN1        >   24600
                             MOVE        24600  TO  WRK-KEISAN1
                         END-IF
                     ELSE
                       IF  MF100-SHOKUJIFTNKBN  = "3"
                           IF  WRK-KEISAN1      >   15000
                               MOVE    15000    TO  WRK-KEISAN1
                           END-IF
                       ELSE
                          IF  MF100-KYURATE     =   30
                              IF  MF100-TOTALTEN    >   26700
                                  COMPUTE WRK-KEISAN9   = 
                                  80100 + 
                                  (((MF100-TOTALTEN *10) - 267000)
                                                    * 0.01) + 0.5
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              ELSE
                                  COMPUTE WRK-KEISAN9   =
                                  ((MF100-TOTALTEN * 10) * 0.3)
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              END-IF
                          ELSE
                              IF  WRK-KEISAN1       >   44400
                                  MOVE    44400    TO  WRK-KEISAN1
                              END-IF
                          END-IF
                       END-IF
                     END-IF
                 END-IF
                 ADD  WRK-KEISAN1       TO  SUM-GKOHFTN (CNT-SUM)
             ELSE
                 COMPUTE WRK-KEISAN1    =   MF100-TOTALTEN  * 
                                            MF100-KYURATE  /
                                            10
                 COMPUTE WRK-KEISAN1    =   WRK-KEISAN1    +  5
                 COMPUTE WRK-KEISAN1    =   WRK-KEISAN1    /  10
                 COMPUTE WRK-KEISAN1    =   WRK-KEISAN1    *  10
                 IF  MF100-NYUGAIKBN    =   "1"
                     IF  MF100-SHOKUJIFTNKBN    = "1" OR "2"
                         IF  WRK-KEISAN1        >   24600
                             MOVE        24600  TO  WRK-KEISAN1
                         END-IF
                     ELSE
                       IF  MF100-SHOKUJIFTNKBN  = "3"
                           IF  WRK-KEISAN1      >   15000
                               MOVE    15000    TO  WRK-KEISAN1
                           END-IF
                       ELSE
                          IF  MF100-KYURATE     =   20
                              IF  MF100-TOTALTEN    >   26700
                                  COMPUTE WRK-KEISAN9   = 
                                  80100 + 
                                  (((MF100-TOTALTEN * 10) - 267000)
                                                    * 0.01) + 0.5
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              ELSE
                                  COMPUTE WRK-KEISAN9   =
                                  ((MF100-TOTALTEN * 10) * 0.3)
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              END-IF
                          ELSE
                              IF  WRK-KEISAN1       >   44400
                                  MOVE    44400    TO  WRK-KEISAN1
                              END-IF
                          END-IF
                       END-IF
                     END-IF
                 END-IF
                 ADD  WRK-KEISAN1       TO  SUM-GKOHFTN (CNT-SUM)
             END-IF
            END-IF
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
           ADD   1  TO  CNT-PAGE
      *
           INITIALIZE  SC09
      *
      *    市町村データ取り込み
           PERFORM 31013-KAIP-SEC
      *
      *    老人支払区分
           IF  SYS-1001-ROUPAYKBN  =  "1"
             MOVE  "○"  TO  SC09-TTKBN (1)
           ELSE
             MOVE  "○"  TO  SC09-TTKBN (2)
           END-IF
      *
      *    点数表
           MOVE  "○"  TO  SC09-TENSUKBN (SYS-1001-TENHYOKBN)
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN       TO  SC09-HOSPCDN
      *  
      *    市町村名
           MOVE     SPACE               TO  SC09-STSNAME
      *     MOVE     ZERO                TO  IDZ
      *     MOVE     1                   TO  FLG-HKNJAINF
      *     PERFORM UNTIL IDZ            >   9  OR
      *                   FLG-HKNJAINF   =   ZERO
      *       INITIALIZE                     HKNJAINF-REC
      *       MOVE     SYS-1001-HOSPID   TO  HKNJA-HOSPID
      *       MOVE     MF100-KEY(27:5)   TO  HKNJA-HKNJANUM (1:5)
      *       MOVE     IDZ(2:1)          TO  HKNJA-HKNJANUM (6:1)
      *       MOVE     HKNJAINF-REC      TO  MCPDATA-REC
      *       PERFORM  900-HKNJAINF-INV-SEC
      *       IF  FLG-HKNJAINF           =   ZERO
      *           MOVE    HKNJA-HKNJANAME    TO  SC09-STSNAME
      *       END-IF
      *       ADD      1                 TO  IDZ
      *     END-PERFORM
           EVALUATE   MF100-KEY(7:3)
            WHEN "001"
                MOVE "和歌山市"          TO  SC09-STSNAME
            WHEN "002" 
                MOVE "海南市"            TO  SC09-STSNAME
            WHEN "003"
                 MOVE "橋本市"           TO  SC09-STSNAME
            WHEN "004"
                  MOVE "有田市"          TO  SC09-STSNAME
            WHEN "005" 
                 MOVE "御坊市"           TO  SC09-STSNAME
            WHEN "006"  
                MOVE "田辺市"            TO  SC09-STSNAME
            WHEN "007"
                MOVE "新宮市"            TO  SC09-STSNAME
            WHEN "012" 
                MOVE "紀の川市"          TO  SC09-STSNAME
            WHEN "017"
                 MOVE "岩出市"           TO  SC09-STSNAME
            WHEN "010"
                MOVE "紀美野町"          TO  SC09-STSNAME
            WHEN "018" 
                MOVE "かつらぎ町"        TO  SC09-STSNAME
            WHEN "020"
                 MOVE "九度山町"         TO  SC09-STSNAME
            WHEN "021"
                  MOVE "高野町"          TO  SC09-STSNAME
            WHEN "023" 
                 MOVE "湯浅町"           TO  SC09-STSNAME
            WHEN "024"  
                MOVE "広川町"            TO  SC09-STSNAME
            WHEN "025" 
                 MOVE "有田川町"         TO  SC09-STSNAME
            WHEN "028" 
                 MOVE "美浜町"           TO  SC09-STSNAME
            WHEN "029" 
                 MOVE "日高町"           TO  SC09-STSNAME
            WHEN "030" 
                 MOVE "由良町"           TO  SC09-STSNAME
            WHEN "031"  
                MOVE "日高川町"          TO  SC09-STSNAME
            WHEN "035"  
                MOVE "みなべ町"          TO  SC09-STSNAME
            WHEN "037"
                 MOVE "印南町"           TO  SC09-STSNAME
            WHEN "038"
                 MOVE "白浜町"           TO  SC09-STSNAME
            WHEN "042" 
                MOVE "上富田町"          TO  SC09-STSNAME
            WHEN "044" 
                MOVE "すさみ町"          TO  SC09-STSNAME
            WHEN "045" 
                MOVE "串本町"            TO  SC09-STSNAME
            WHEN "046" 
                MOVE "那智勝浦町"        TO  SC09-STSNAME
            WHEN "047" 
                MOVE "太地町"            TO  SC09-STSNAME
            WHEN "049" 
                MOVE "古座川町"          TO  SC09-STSNAME
            WHEN "052" 
                MOVE "北山村"            TO  SC09-STSNAME
            WHEN "005" 
                MOVE "御坊市"            TO  SC09-STSNAME
            WHEN OTHER
                MOVE     ZERO           TO  IDZ
                MOVE     1              TO  FLG-HKNJAINF
                PERFORM UNTIL IDZ            >   9  OR
                         FLG-HKNJAINF   =   ZERO
                INITIALIZE                     HKNJAINF-REC
      *          MOVE     SYS-1001-HOSPID   TO  HKNJA-HOSPID
                MOVE     SYS-1001-HOSPNUM  TO  HKNJA-HOSPNUM
                MOVE     MF100-KEY(5:5)   TO  HKNJA-HKNJANUM (1:5)
                MOVE     IDZ(2:1)          TO  HKNJA-HKNJANUM (6:1)
                MOVE     HKNJAINF-REC      TO  MCPDATA-REC
                PERFORM  900-HKNJAINF-INV-SEC
                IF  FLG-HKNJAINF           =   ZERO
                    MOVE    HKNJA-HKNJANAME    TO  SC09-STSNAME
                END-IF
                ADD      1                 TO  IDZ
                END-PERFORM
           END-EVALUATE
      *
      *    請求日付
           MOVE  WRK-SYSYMDWH           TO  SC09-SEIYMD
      *
      *    住所
           MOVE  SYS-1002-ADRS          TO  SC09-ADRS
      *
      *    医療機関名
           MOVE  SYS-1001-HOSPNAME      TO  SC09-HOSPNAME
      *
      *    管理者名
           MOVE  SYS-1001-KAISETUNAME   TO  SC09-KANRINAME
      *
      *    電話番号
           MOVE  SYS-1002-TEL           TO  SC09-TEL
      *
           IF  FLG-KEY = 0
               MOVE    KEY-N-STSCD     TO  WRK-STSCD
               PERFORM 31014-GOUKEI-GET-SEC
　　           MOVE  1  TO  FLG-KEY
           END-IF
      *    印刷日付
           MOVE  WRK-DENPPRTYMWH(1:16)  TO  SC09-PRTYM
      *
      *    振込み先銀行
           MOVE  KOUZA-GINKO            TO  SC09-BANKNAME
      *
      *    振込み先銀行支店
           MOVE  KOUZA-SITEN            TO  SC09-SITENNAME
      *
      *    口座種類
           EVALUATE   KOUZA-SYURUI
             WHEN "1"
               MOVE    "当座"           TO  SC09-SYUBETU
               MOVE    "○"             TO  SC09-KOUZA2
             WHEN "2"
               MOVE    "普通"           TO  SC09-SYUBETU
               MOVE    "○"             TO  SC09-KOUZA1
           END-EVALUATE
      *
      *    振込み先口座番号
           MOVE  KOUZA-KOUZANO          TO  SC09-KOZANUM
      *
      *    振込み先名義人
           MOVE  KOUZA-MEIGI            TO  SC09-MEIGI
      *
      *    振込み先名義人 カナ
           MOVE  KOUZA-KANAMEIGI        TO  SC09-KANAMEIGI
      * 
      *    口座変更
           EVALUATE   KOUZA-HENKO
             WHEN "1"
               MOVE    "○"             TO  SC09-HENKO1 
             WHEN "2"
               MOVE    "○"             TO  SC09-HENKO2
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
           MOVE    MF100-KEY      TO  WRK-MF100-KEY 
      *
           PERFORM  UNTIL  FLG-END  =  1
                    OR  CNT-LINE    =  WRK-KAIP-GYO
                    OR  MF100-KEY (1:17) NOT =  WRK-MF100-KEY (1:17) 
      *
             ADD  1  TO  CNT-LINE
      *    受給資格証記号番号
             MOVE  MF100-KOHJKYSNUM  TO  SC09-JKYSNUM (CNT-LINE)
      *    氏名
             MOVE  MF100-NAME        TO  SC09-NAME (CNT-LINE)
      *    生年月日
             MOVE  MF100-BIRTHDAY   TO  SC09-BIRTHDAY (CNT-LINE)
      *
             MOVE  MF100-BIRTHDAY   TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  LNK-DAY2-EDTYMD3 TO  SC09-BIRTHDAYK (CNT-LINE)

      *    診療月
      *       MOVE  WRK-SRYYM(5:2)   TO  SC09-SRYM (CNT-LINE)
             MOVE  MF100-SRYYM(5:2)  TO  SC09-SRYM (CNT-LINE)
      *
             EVALUATE  MF100-NYUGAIKBN
               WHEN  "1"
                 MOVE  2           TO  WRK-NYUGAIKBN
               WHEN  "2"
                 MOVE  1           TO  WRK-NYUGAIKBN
             END-EVALUATE
      *    日数
             MOVE  MF100-JNISSU    TO  WRK-NISSUX
             MOVE  WRK-NISSUX      TO  
                                SC09-NISSU (CNT-LINE WRK-NYUGAIKBN)
             ADD   MF100-JNISSU    TO  SUM-GNISU (WRK-NYUGAIKBN)
      *    点数
             MOVE  MF100-TOTALTEN  TO  WRK-TENSUX
             MOVE  WRK-TENSUX      TO  
                                SC09-TENSU (CNT-LINE WRK-NYUGAIKBN)
             ADD   MF100-TOTALTEN  TO  SUM-GTENSU (WRK-NYUGAIKBN)
      *    負担金
             IF  (MF100-FTNMONEY    NOT =   ZERO)     AND
                 (MF100-KEY (7:3)      =   "017")
                 MOVE   MF100-FTNMONEY  TO  WRK-GKINGKX
                 MOVE  WRK-GKINGKX      TO  
                             SC09-FTNMONEY (CNT-LINE WRK-NYUGAIKBN)
             ELSE
              IF  (MF100-KEY (7:3)   =  "001" OR "012" OR "018") OR
                  (MF100-KEY (7:3)   =  "002" AND 
                   MF100-NYUGAIKBN = "2" ) 
                 COMPUTE WRK-KEISAN1    =   MF100-TOTALTEN * 
                                            MF100-KYURATE  /
                                            10
                 IF  MF100-NYUGAIKBN    =   "1"
                     IF  MF100-SHOKUJIFTNKBN    = "1" OR "2"
                         IF  WRK-KEISAN1        >   24600
                             MOVE        24600  TO  WRK-KEISAN1
                         END-IF
                     ELSE
                       IF  MF100-SHOKUJIFTNKBN  = "3"
                           IF  WRK-KEISAN1      >   15000
                               MOVE    15000    TO  WRK-KEISAN1
                           END-IF
                       ELSE
                          IF  MF100-KYURATE     =   30
                              IF  MF100-TOTALTEN    >   26700
                                  COMPUTE WRK-KEISAN9   = 
                                  80100 + 
                                  (((MF100-TOTALTEN * 10) - 267000)
                                                    * 0.01) + 0.5
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              ELSE
                                  COMPUTE WRK-KEISAN9   =
                                  ((MF100-TOTALTEN * 10) * 0.3)
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              END-IF
                          ELSE
                              IF  WRK-KEISAN1       >   44400
                                  MOVE    44400    TO  WRK-KEISAN1
                              END-IF
                          END-IF
                       END-IF
                     END-IF
                 END-IF
                 MOVE  WRK-KEISAN1      TO  WRK-GKINGKX
                 MOVE  WRK-GKINGKX      TO  
                             SC09-FTNMONEY (CNT-LINE WRK-NYUGAIKBN)
             ELSE
                 COMPUTE WRK-KEISAN1    =   MF100-TOTALTEN  * 
                                            MF100-KYURATE  /
                                            10
                 COMPUTE WRK-KEISAN1    =   WRK-KEISAN1    +  5
                 COMPUTE WRK-KEISAN1    =   WRK-KEISAN1    /  10
                 COMPUTE WRK-KEISAN1    =   WRK-KEISAN1    *  10
                 IF  MF100-NYUGAIKBN    =   "1"
                     IF  MF100-SHOKUJIFTNKBN    = "1" OR "2"
                         IF  WRK-KEISAN1        >   24600
                             MOVE        24600  TO  WRK-KEISAN1
                         END-IF
                     ELSE
                       IF  MF100-SHOKUJIFTNKBN  = "3"
                           IF  WRK-KEISAN1      >   15000
                               MOVE    15000    TO  WRK-KEISAN1
                           END-IF
                       ELSE
                          IF  MF100-KYURATE     =   20
                              IF  MF100-TOTALTEN    >   26700
                                  COMPUTE WRK-KEISAN9   = 
                                  80100 + 
                                  (((MF100-TOTALTEN * 10) - 267000)
                                                    * 0.01) + 0.5
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              ELSE
                                  COMPUTE WRK-KEISAN9   =
                                  ((MF100-TOTALTEN * 10) * 0.3)
                                  MOVE    WRK-KEISAN9
                                                    TO  WRK-KEISAN1
                              END-IF
                          ELSE
                              IF  WRK-KEISAN1       >   44400
                                  MOVE    44400    TO  WRK-KEISAN1
                              END-IF
                          END-IF
                       END-IF
                     END-IF
                 END-IF
                 MOVE  WRK-KEISAN1      TO  WRK-GKINGKX       
             MOVE  WRK-GKINGKX      TO  
                             SC09-FTNMONEY (CNT-LINE WRK-NYUGAIKBN)
             END-IF
            END-IF
      *    加入保険
             EVALUATE  MF100-HKNNUM
               WHEN  "060"
                 MOVE  "○"         TO  SC09-HKNKBN-KOK (CNT-LINE)
               WHEN  "067"
                 MOVE  "○"         TO  SC09-HKNKBN-KOK (CNT-LINE)
               WHEN OTHER
                 MOVE  "○"         TO  SC09-HKNKBN-SYH (CNT-LINE)
             END-EVALUATE 
      *    給付割合
             COMPUTE WRK-FTNRATEX   =   MF100-KYURATE / 10
             MOVE    WRK-FTNRATEX   TO  SC09-FTNWARIAI2 (CNT-LINE)
      *    備考欄
             IF  MF100-YKZFTN          =   30
                 MOVE  "３割"          TO  SC09-SYOHOU (CNT-LINE)
             ELSE
                 MOVE   SPACE          TO  SC09-SYOHOU (CNT-LINE)
             END-IF
      *
      *    合計
      *    請求金額
           IF (MF100-FTNMONEY NOT =   ZERO)  AND
              (MF100-KEY (7:3)   =   "017")
              ADD   MF100-FTNMONEY  TO
                         SUM-GFTNMONEY (WRK-NYUGAIKBN)
              ADD   MF100-FTNMONEY  TO  WRK-SKIN
           ELSE
              ADD  WRK-KEISAN1   TO  SUM-GFTNMONEY (WRK-NYUGAIKBN)
              ADD  WRK-KEISAN1   TO  WRK-SKIN
           END-IF   
           IF  MF100-NYUGAIKBN   =   1
               IF (MF100-FTNMONEY    NOT = ZERO)  AND
                  (MF100-KEY (7:3)      =   "017")
                   ADD   MF100-FTNMONEY TO  WRK-SKINN
               ELSE
                   ADD  WRK-KEISAN1  TO  WRK-SKINN
               END-IF
           ELSE
                IF (MF100-FTNMONEY    NOT = ZERO)  AND
                   (MF100-KEY (7:3)      =   "017")          
                   ADD   MF100-FTNMONEY TO  WRK-SKING
                ELSE
                   ADD  WRK-KEISAN1  TO  WRK-SKING    
                END-IF   
           END-IF
      *
      *    薬剤金額
      *
           EVALUATE  MF100-HKNNUM
             WHEN  "060"
               ADD    1             TO  WRK-SKENSUK
             WHEN  "067"
               ADD    1             TO  WRK-SKENSUK
             WHEN OTHER
               ADD    1             TO  WRK-SKENSUS
           END-EVALUATE
           ADD    1                 TO  WRK-SKENSU
           IF  MF100-NYUGAIKBN      =   "1"
               ADD    MF100-TOTALTEN    TO  WRK-STENSUN
           ELSE
               ADD    MF100-TOTALTEN    TO  WRK-STENSU
           END-IF
           ADD    MF100-YKZFTN      TO  WRK-SYKZFTN
           IF  MF100-NYUGAIKBN      =   1
               ADD    MF100-JNISSU  TO  WRK-SNNISU
               ADD    1             TO  WRK-SKENSUN
           ELSE
               ADD    MF100-JNISSU  TO  WRK-SNISU
               ADD    1             TO  WRK-SKENSUG
           END-IF
      *
      *    市町村ブレイク用キーセット 
             MOVE    MF100-KEY         TO  WRK-MF100-KEY
      *   
      *    レセプト明細読込
             PERFORM 900-MF100-READ-SEC 
           END-PERFORM
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計行>処理
      *****************************************************************
       330-GOKEI-HEN-SEC  SECTION.
      *
           MOVE  SUM-GNISU (1)      TO  WRK-GGAINISSUX
           MOVE  WRK-GGAINISSUX     TO  SC09-GGAINISSU
      *
           MOVE  SUM-GNISU (2)      TO  WRK-GGAINISSUX
           MOVE  WRK-GGAINISSUX     TO  SC09-GINNISSU
      *
           MOVE  SUM-GTENSU (1)     TO  WRK-GGAITENSUX
           MOVE  WRK-GGAITENSUX     TO  SC09-GGAITENSU
      *
           MOVE  SUM-GTENSU (2)     TO  WRK-GGAITENSUX
           MOVE  WRK-GGAITENSUX     TO  SC09-GINTENSU
      *
           MOVE  SUM-GFTNMONEY (1)  TO  WRK-GGAIFTNMONEYX
           MOVE  WRK-GGAIFTNMONEYX  TO  SC09-GGAIFTNMONEY
      *
           MOVE  SUM-GFTNMONEY (2)  TO  WRK-GGAIFTNMONEYX
           MOVE  WRK-GGAIFTNMONEYX  TO  SC09-GINFTNMONEY
      *
           COMPUTE WRK-GOUKEIMONEYX =   SUM-GFTNMONEY (1) +
                                        SUM-GFTNMONEY (2)
           MOVE  WRK-GOUKEIMONEYX   TO  SC09-SOGOKEI
      *
           IF  MF100-KEY (1:9) NOT =  WRK-MF100-KEY (1:9) 
               MOVE    ZERO         TO  SUM-GFTNMONEY (1)
                                        SUM-GFTNMONEY (2)
                                        SUM-GNISU (1)
                                        SUM-GNISU (2)
                                        SUM-GTENSU (1)
                                        SUM-GTENSU (2) 
      *                                 SUM-GFTNYKZ
           END-IF
           .
       330-GOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           MOVE    WRK-SKENSU          TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC09-SKEN
                                           SC09-SKEN2
           MOVE    WRK-SKENSUN         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC09-SKENN
           MOVE    WRK-SKENSUG         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC09-SKENG
           MOVE    WRK-SKENSUK         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC09-SKENK
           MOVE    WRK-SKENSUS         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC09-SKENS
           MOVE    WRK-STENSU          TO  WRK-STENSUX
           MOVE    WRK-STENSUX         TO  SC09-STENSU 
           MOVE    WRK-STENSUN         TO  WRK-STENSUX
           MOVE    WRK-STENSUX         TO  SC09-STENSUN
           MOVE    WRK-SKIN            TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC09-SKIN
           MOVE    WRK-SKINN           TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC09-SKINN
           MOVE    WRK-SKING           TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC09-SKING
           MOVE    WRK-SYKZFTN         TO  WRK-SYKZFTNX
           MOVE    WRK-SYKZFTNX        TO  SC09-SYKZFTN
           MOVE    WRK-SNISU           TO  WRK-SNISUX
           MOVE    WRK-SNISUX          TO  SC09-SNISU
           MOVE    WRK-SNNISU          TO  WRK-SNISUX
           MOVE    WRK-SNISUX          TO  SC09-SNNISU
      *
           MOVE    ZERO                TO  WRK-SKENSU
                                           WRK-SKENSUN
                                           WRK-SKENSUG
                                           WRK-SKENSUK
                                           WRK-SKENSUS
                                           WRK-STENSU
                                           WRK-STENSUN
                                           WRK-SKIN
                                           WRK-SKINN
                                           WRK-SKING
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
           IF  WRK-KAIP-PNO            =  "000"
               MOVE   "SC3009.red"     TO  SPRT-PRTID
           ELSE
               MOVE   "SC3009"       TO  WRK-ID 
               MOVE    WRK-KAIP-PNO  TO  WRK-PRT-CTV
               MOVE   ".red"         TO  WRK-FILE
               MOVE    WRK-PRT-ID    TO  SPRT-PRTID
           END-IF
           MOVE    "重度心身障害"      TO  SPRT-TITLE
           MOVE    SC09                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
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
                   MOVE    "08"        TO  WRK-KAIP-GYO
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
               MOVE  WRK-GKENSUX   TO  SC09-GKENSU
               MOVE  ZERO          TO  WRK-GKOHFTNX
               MOVE  WRK-GKOHFTNX  TO  SC09-GKOHFTN
             WHEN    WRK-STSCD     =   SUM-STSCD (IDX-GOU)
  　　*　  請求件数
               MOVE  SUM-GKENSU  (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC09-GKENSU
  　                                   SC09-GKEN
  　　*　  請求件数（国保）
               MOVE  SUM-GKENSUK (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC09-GKENK
  　　*　  請求件数（社保）
               MOVE  SUM-GKENSUS (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC09-GKENS
      *    点数
               MOVE  SUM-TENSU   (IDX-GOU) TO  WRK-GTENSUX
               MOVE  WRK-GTENSUX   TO  SC09-GTENSU
      *    請求金額
               MOVE  SUM-GKOHFTN (IDX-GOU)  TO  WRK-GKOHFTNX
               MOVE  WRK-GKOHFTNX  TO  SC09-GKOHFTN
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
           DISPLAY "*** SEIKYU3010 IN   "  CNT-MF100
           DISPLAY "*** SEIKYU3010 PAGE "  CNT-PRINT
           DISPLAY "*** SEIKYU3010 END ***"
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
               MOVE  1           TO  FLG-END
      *         MOVE  HIGH-VALUE  TO  KEY-NEW
             NOT AT  END
      ****      田辺市読飛し
      ****       IF  MF100-FTNJANUM(1:7)  =  "8130006"
      ****         GO  TO  900-MF100-READ-SEC
      ****       ELSE
               ADD   1           TO  CNT-MF100
               MOVE  MF100-KEY (7:3)  TO  KEY-N-STSCD
      *         MOVE  MF100-FTNJANUM  TO  KEY-N-FTNJANUM
      ****       END-IF
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
      *    保険者マスター読込（キー）
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
