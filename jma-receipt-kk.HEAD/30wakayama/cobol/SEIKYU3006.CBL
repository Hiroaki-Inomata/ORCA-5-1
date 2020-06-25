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
       PROGRAM-ID.     SEIKYU3006.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 和歌山重度心身障害児(老人除く)医療費請求書(146,246)
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
      *  01.00.01       楠本    03/09/22        地方公費対応
      *                                       　有田市　吉備町
      *                                         湯浅町　金屋町　海南市
      *                                         打田町　貴志川町
      *                                         橋本市  御坊市 由良町
      *  01.00.02       楠本    05/01/13        田辺市
      *  01.00.03       楠本    05/02/21        粉河町
      *  01.00.04       楠本    05/04/20        那賀町　清水町
      *  01.00.04       楠本    05/05/30        串本町
      *  01.00.05       楠本    05/08/25        上富田町
      *  01.00.06       楠本    05/10/07        日高川町
      *  01.00.07       楠本    05/11/30        みなべ町　美浜町
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
      *    エラーファイル 名称領域
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01PARA//
                   BY         //RECEERR//.
           03      PIC X(04)   VALUE   ".dat".
      *
           COPY  "SC3005.INC".
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
           03  FLG-PTKOHINF  PIC 9(01).
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
           03  WRK-KYURATE      PIC 9(03).
      *
           03  WRK-SYMD.
               05  WRK-SYY   PIC 9(04).
               05  WRK-SMM   PIC 9(02).
               05  WRK-SDD   PIC 9(02).
           03  WRK-HENYMDG   PIC X(22).
           03  WRK-HENYMDG1  PIC X(09).
           03  WRK-KEISAN6   PIC 9(09).
      *
      *    編集用エリア
           03  WRK-FTNRATEX    PIC 9.
           03  WRK-GKENSUX     PIC ZZ9.
           03  WRK-GKOHFTNX    PIC ZZZ,ZZ9.
           03  WRK-YKZFTNX     PIC ZZ,ZZ9.
           03  WRK-NYUNISSUX   PIC Z9.
           03  WRK-GTENSUX     PIC ZZ,ZZ9.
           03  WRK-GTEN-WARIX  PIC ZZZ,ZZ9.
           03  WRK-YKZFTNX     PIC ZZ,ZZ9.
           03  WRK-GKINGKX     PIC ZZZ,ZZ9.
           03  WRK-SNISUX      PIC ZZ9.
           03  WRK-SKENSUX     PIC ZZ9.
           03  WRK-STENSUX     PIC ZZ,ZZ9.
           03  WRK-SKINX       PIC ZZ,ZZZ,ZZ9.
           03  WRK-SYKZFTNX    PIC ZZ,ZZZ,ZZ9.
           03  WRK-JNISSUX     PIC Z9.
      *
      *    合計エリア
           03  WRK-KOHFTN      PIC 9(09).
           03  WRK-YKZFTN      PIC 9(09).
           03  WRK-KOHKENSU    PIC 9(05).
           03  WRK-YKZKENSU    PIC 9(05).
           03  WRK-SNISU       PIC 9(04).
           03  WRK-SNNISU      PIC 9(04).
           03  WRK-SKENSU      PIC 9(04).
           03  WRK-SKENSUK     PIC 9(04).
           03  WRK-SKENSUS     PIC 9(04).
           03  WRK-STENSU      PIC 9(10).
           03  WRK-STENSUN     PIC 9(10).
           03  WRK-SKIN        PIC 9(10).
           03  WRK-SKINN       PIC 9(10).
           03  WRK-SYKZFTN     PIC 9(10).
      *
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
           03  TBL-KAIP-TBL          OCCURS 20
                                     INDEXED  BY  IDX-MEI.
               05  TBL-KAIP-STSCD    PIC X(03). 
               05  TBL-KAIP-PNO      PIC X(03).
               05  TBL-KAIP-GYO      PIC X(02).
               05  TBL-KAIP-GOUKEI   PIC X(01).

       01  TBL-KAIP-VAL.
      *                                            有田市   
           03  TBL-KAIP-VAL1         PIC X(09) VALUE "004026010". 
      *                                            吉備町   
           03  TBL-KAIP-VAL2         PIC X(09) VALUE "025026010".
      *                                            湯浅町   
           03  TBL-KAIP-VAL3         PIC X(09) VALUE "023026010".
      *                                            金屋町   
           03  TBL-KAIP-VAL4         PIC X(09) VALUE "026026010".
      *                                            海南市
           03  TBL-KAIP-VAL5         PIC X(09) VALUE "002002010".
      *                                            紀の川市 
           03  TBL-KAIP-VAL6         PIC X(09) VALUE "012012070".
      *                                            岩出町
           03  TBL-KAIP-VAL7         PIC X(09) VALUE "017017010".
      *                                            橋本市
           03  TBL-KAIP-VAL8         PIC X(09) VALUE "003003010".
      *                                            御坊市
           03  TBL-KAIP-VAL9         PIC X(09) VALUE "005005010".
      *                                            由良町
           03  TBL-KAIP-VAL10        PIC X(09) VALUE "030030010".
      *                                            田辺市
           03  TBL-KAIP-VAL11        PIC X(09) VALUE "006006010".
      *                                            清水町
           03  TBL-KAIP-VAL12        PIC X(09) VALUE "027026010".
      *                                            串本町
           03  TBL-KAIP-VAL13        PIC X(09) VALUE "045045010".
      *                                          上富田町
           03  TBL-KAIP-VAL14        PIC X(09) VALUE "042042010".
      *                                          日高川町
           03  TBL-KAIP-VAL15        PIC X(09) VALUE "031031080".
      *                                          みなべ町
           03  TBL-KAIP-VAL16        PIC X(09) VALUE "035035010".
      *                                          美浜町
           03  TBL-KAIP-VAL17        PIC X(09) VALUE "028028010".
      *                                          印南町
           03  TBL-KAIP-VAL18        PIC X(09) VALUE "037037010".
      *                                          日高町
           03  TBL-KAIP-VAL19        PIC X(09) VALUE "029029010".
      *                                          広川町
           03  TBL-KAIP-VAL20        PIC X(09) VALUE "024026010".
      *
       01  SUM-AREA  VALUE   ZERO.
           03  SUM-GOKEI  OCCURS  60
                              INDEXED  BY  IDX-GOU.
               05  SUM-STSCD   PIC X(03).
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
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
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
           MOVE  "MF100ZZ"             TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)
                                       TO  MF100PARA-TERMID
           MOVE  WRK-PARA-HOSPNUM    TO  MF100PARA-HOSPNUM
      *
           MOVE  "RECEERR"             TO  RECEERR-FILE-ID
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
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1001-STYUKYMD(1:6)
                                           SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC          TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI            =   ZERO
             MOVE  MCPDATA-REC         TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU3006 SYS 1001 ERR ***" UPON CONSOLE
             MOVE  1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"                TO  SYS-1002-KANRICD
           MOVE  "*"                   TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1002-STYUKYMD(1:6)
                                           SYS-1002-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC          TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI            =   ZERO
             MOVE  MCPDATA-REC         TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU3006 SYS 1002 ERR ***" UPON CONSOLE
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
      *    患者公費情報から精神２１の登録チェック
             INITIALIZE                      PTKOHINF-REC
             MOVE    MF100-HOSPNUM       TO  PTKOH-HOSPNUM
             MOVE    MF100-PTID          TO  PTKOH-PTID
             MOVE    "021"               TO  PTKOH-KOHNUM
             MOVE    MF100-SRYYM         TO  PTKOH-TEKSTYMD(1:6)
                                             PTKOH-TEKEDYMD(1:6)
             MOVE    "01"                TO  PTKOH-TEKSTYMD(7:2)
                                             PTKOH-TEKEDYMD(7:2)
             PERFORM 900-PTKOHINF-KEY6-SEL-SEC
             IF    ( FLG-PTKOHINF        =   ZERO )
                 MOVE    10              TO  WRK-KYURATE
             ELSE
                 MOVE    MF100-KYURATE   TO  WRK-KYURATE
             END-IF
             IF  MF100-FTNMONEY  NOT = ZERO 
                 IF  KEY-N-STSCD     =  "017"
                     MOVE   MF100-FTNMONEY TO  WRK-KEISAN6
                 ELSE
                     COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN 
                                         *  WRK-KYURATE
                                         /  10
                 END-IF
             ELSE
                 COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN *  WRK-KYURATE
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
           INITIALIZE  SC05
      *
      *    市町村データ取り込み
           PERFORM 31013-KAIP-SEC
      *
      *    印刷日付
           MOVE  WRK-DENPPRTYMWH(05:04) TO  SC05-PRTYY
           MOVE  WRK-DENPPRTYMWH(11:04) TO  SC05-PRTMM
           MOVE  WRK-DENPPRTYMWH(1:16)  TO  SC05-PRTYM
      *
      *    医療機関コード
           MOVE  SYS-1001-HOSPCDN       TO  SC05-HOSPCDN
      *
      *    市町村名
           MOVE     SPACE               TO  SC05-STSNAME
      *      MOVE     ZERO                TO  IDZ
      *      MOVE     1                   TO  FLG-HKNJAINF
      *      PERFORM UNTIL IDZ            >   9  OR
      *                    FLG-HKNJAINF   =   ZERO
      *        INITIALIZE                       HKNJAINF-REC
      *        MOVE     SYS-1001-HOSPID     TO  HKNJA-HOSPID
      *        MOVE     MF100-KEY(27:5)     TO  HKNJA-HKNJANUM (1:5)
      *        MOVE     IDZ(2:1)            TO  HKNJA-HKNJANUM (6:1)
      *        MOVE     HKNJAINF-REC      TO  MCPDATA-REC
      *        PERFORM  900-HKNJAINF-INV-SEC
      *        IF  FLG-HKNJAINF           =   ZERO
      *            MOVE    HKNJA-HKNJANAME    TO  SC05-STSNAME
      *        END-IF
      *        ADD      1                 TO  IDZ
      *      END-PERFORM
           EVALUATE   MF100-KEY(7:3)
            WHEN "001"
                MOVE "和歌山市"          TO  SC05-STSNAME
            WHEN "002" 
                MOVE "海南市"            TO  SC05-STSNAME
            WHEN "003"
                 MOVE "橋本市"           TO  SC05-STSNAME
            WHEN "004"
                  MOVE "有田市"          TO  SC05-STSNAME
            WHEN "005" 
                 MOVE "御坊市"           TO  SC05-STSNAME
            WHEN "006"  
                MOVE "田辺市"            TO  SC05-STSNAME
            WHEN "007"
                MOVE "新宮市"            TO  SC05-STSNAME
            WHEN "012" 
                MOVE "紀の川市"          TO  SC05-STSNAME
            WHEN "017"
                 MOVE "岩出市"           TO  SC05-STSNAME
            WHEN "010"
                MOVE "紀美野町"          TO  SC05-STSNAME
            WHEN "018" 
                MOVE "かつらぎ町"        TO  SC05-STSNAME
            WHEN "020"
                 MOVE "九度山町"         TO  SC05-STSNAME
            WHEN "021"
                  MOVE "高野町"          TO  SC05-STSNAME
            WHEN "023" 
                 MOVE "湯浅町"           TO  SC05-STSNAME
            WHEN "024"  
                MOVE "広川町"            TO  SC05-STSNAME
            WHEN "025" 
                 MOVE "有田川町"         TO  SC05-STSNAME
            WHEN "028" 
                 MOVE "美浜町"           TO  SC05-STSNAME
            WHEN "029" 
                 MOVE "日高町"           TO  SC05-STSNAME
            WHEN "030" 
                 MOVE "由良町"           TO  SC05-STSNAME
            WHEN "031"  
                MOVE "日高川町"          TO  SC05-STSNAME
            WHEN "035"  
                MOVE "みなべ町"          TO  SC05-STSNAME
            WHEN "037"
                 MOVE "印南町"           TO  SC05-STSNAME
            WHEN "038"
                 MOVE "白浜町"           TO  SC05-STSNAME
            WHEN "042" 
                MOVE "上富田町"          TO  SC05-STSNAME
            WHEN "044" 
                MOVE "すさみ町"          TO  SC05-STSNAME
            WHEN "045" 
                MOVE "串本町"            TO  SC05-STSNAME
            WHEN "046" 
                MOVE "那智勝浦町"        TO  SC05-STSNAME
            WHEN "047" 
                MOVE "太地町"            TO  SC05-STSNAME
            WHEN "049" 
                MOVE "古座川町"          TO  SC05-STSNAME
            WHEN "052" 
                MOVE "北山村"            TO  SC05-STSNAME
            WHEN "005" 
                MOVE "御坊市"            TO  SC05-STSNAME
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
                    MOVE    HKNJA-HKNJANAME    TO  SC05-STSNAME
                END-IF
                ADD      1                 TO  IDZ
                END-PERFORM
           END-EVALUATE
      *
      *    保険者名
           INITIALIZE                       HKNJAINF-REC
           MOVE     SYS-1001-HOSPNUM    TO  HKNJA-HOSPNUM
           MOVE     MF100-HKNJANUM-KEY  TO  HKNJA-HKNJANUM
           MOVE     HKNJAINF-REC        TO  MCPDATA-REC
           PERFORM  900-HKNJAINF-INV-SEC
           IF       FLG-HKNJAINF        =   ZERO
                    MOVE    HKNJA-HKNJANAME
                                        TO  SC05-NKJANAME
           ELSE
                    MOVE    SPACE       TO  SC05-NKJANAME
           END-IF   
      *
      *    請求日付
           MOVE  WRK-SYSYMDWH           TO  SC05-SEIYMD
      *
           MOVE  SYS-1002-POST(1:3)     TO  SC05-POST(1:3)
           MOVE  "-"                    TO  SC05-POST(4:1)
           MOVE  SYS-1002-POST(4:7)     TO  SC05-POST(5:8)
      *    住所
           MOVE  SYS-1002-ADRS          TO  SC05-ADRS
      *
      *    医療機関名
           MOVE  SYS-1001-HOSPNAME      TO  SC05-HOSPNAME
      *
      *    管理者名
           MOVE  SYS-1001-KAISETUNAME   TO  SC05-KANRINAME
      *
      *    電話番号
           MOVE  SYS-1002-TEL           TO  SC05-TEL
      *
           IF  FLG-KEY = 0
               MOVE    KEY-N-STSCD     TO  WRK-STSCD
               PERFORM 31014-GOUKEI-GET-SEC
　　           MOVE  1  TO  FLG-KEY
           END-IF
      *
      *    振込み先銀行
           MOVE  KOUZA-GINKO            TO  SC05-BANKNAME
      *
      *    振込み先銀行支店
           MOVE  KOUZA-SITEN            TO  SC05-SITENNAME
      *
      *    口座種類
           EVALUATE   KOUZA-SYURUI
             WHEN "1"
               MOVE    "当座"           TO  SC05-SYUBETU
               MOVE    "○"             TO  SC05-KOUZA2
             WHEN "2"
               MOVE    "普通"           TO  SC05-SYUBETU
               MOVE    "○"             TO  SC05-KOUZA1
           END-EVALUATE
      *
      *    振込み先口座番号
           MOVE  KOUZA-KOUZANO          TO  SC05-KOZANUM
      *
      *    振込み先名義人
           MOVE  KOUZA-MEIGI            TO  SC05-MEIGI
      *
      *    振込み先名義人 カナ
           MOVE  KOUZA-KANAMEIGI        TO  SC05-KANAMEIGI
      * 
      *    口座変更
           EVALUATE   KOUZA-HENKO
             WHEN "1"
               MOVE    "○"             TO  SC05-HENKO1 
             WHEN "2"
               MOVE    "○"             TO  SC05-HENKO2
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
             MOVE  MF100-FTNJANUM      TO  SC05-FTNJANUM (CNT-LINE)
      *    受給資格証記号番号
             MOVE  MF100-KOHJKYSNUM(1:7)   TO  
                                           SC05-JKYSNUM (CNT-LINE)
      *    氏名
             MOVE  MF100-NAME          TO  SC05-NAME (CNT-LINE)
      *    性別 
             IF  MF100-SEX             =  "1"
                 MOVE  "○"            TO  SC05-SEX-M (CNT-LINE)
             ELSE
                 MOVE  "○"            TO  SC05-SEX-F (CNT-LINE)
             END-IF
      *    生年月日
             MOVE  MF100-BIRTHDAY      TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  LNK-DAY2-EDTYMD3    TO  SC05-BIRTHDAY (CNT-LINE)
      *    保険者番号
             MOVE  MF100-HKNJANUM      TO  SC05-HKNJANUM (CNT-LINE)
      *    記号・番号
             MOVE  MF100-KIGO(1:20)    TO  SC05-KIGO (CNT-LINE)
             MOVE  MF100-NUM(1:20)     TO  SC05-NUM (CNT-LINE)
      *    種別
             EVALUATE  MF100-HKNNUM
               WHEN  "001"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 1)
               WHEN  "006"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 2)
               WHEN  "003"
                  MOVE "○"            TO  SC05-HKNSYUBETU (CNT-LINE 3)
               WHEN  "004"
                  MOVE "○"            TO  SC05-HKNSYUBETU (CNT-LINE 3)
               WHEN  "002"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 4)
               WHEN  "031"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 5)
               WHEN  "032"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 5)
               WHEN  "033"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 5)
               WHEN  "034"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 5)
               WHEN  "060"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 6)
               WHEN  "067"
                 MOVE  "○"            TO  SC05-HKNSYUBETU (CNT-LINE 6)
             END-EVALUATE 
      *    診療実日数開始日
             MOVE  MF100-SRYYM         TO  WRK-SYMD (1:6)
             MOVE  "01"                TO  WRK-SYMD (7:2)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  WRK-HENYMDG(05:04)  TO  SC05-SRNISU-FROM-YY
             MOVE  WRK-HENYMDG(11:04)  TO  SC05-SRNISU-FROM-MM
             MOVE "０１"               TO  SC05-SRNISU-FROM-DD
      *    診療実日数終了日
             MOVE  WRK-HENYMDG(05:04)  TO  SC05-SRNISU-TO-YY
             MOVE  WRK-HENYMDG(11:04)  TO  SC05-SRNISU-TO-MM
             MOVE "　末"               TO  SC05-SRNISU-TO-DD

      *    診療実日数
             IF  MF100-KEY(5:5)   =   "30003" OR "30020" OR "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     MOVE  SPACE        TO  SC05-SRNISU (CNT-LINE)
                 ELSE
                     MOVE  MF100-JNISSU TO  WRK-JNISSUX
                     MOVE  WRK-JNISSUX  TO  SC05-SRNISU (CNT-LINE)
                 END-IF
             ELSE
                 MOVE  MF100-JNISSU     TO  WRK-JNISSUX
                 MOVE  WRK-JNISSUX      TO  SC05-SRNISU (CNT-LINE)
             END-IF
      *    本人家族
             EVALUATE  MF100-HONKZKKBN
               WHEN  "1"
                 MOVE  "○"          TO  SC05-HONKZKKBN (CNT-LINE 1)
               WHEN  "2"
                 MOVE  "○"          TO  SC05-HONKZKKBN (CNT-LINE 2)
             END-EVALUATE 
      *    給付割合
             COMPUTE WRK-FTNRATEX    =   MF100-KYURATE / 10
             EVALUATE  WRK-FTNRATEX
               WHEN  3
                 MOVE  "○"          TO  SC05-FTNWARIAI1 (CNT-LINE 1)
               WHEN  2
                 MOVE  "○"          TO  SC05-FTNWARIAI1 (CNT-LINE 2)
               WHEN OTHER
                 MOVE  WRK-FTNRATEX  TO  SC05-FTNWARIAI2 (CNT-LINE)
             END-EVALUATE
             IF  MF100-KEY(5:5)       =   "30030"
                 EVALUATE  WRK-FTNRATEX
                   WHEN  3
                     MOVE  "○"        TO  SC05-FTNWARIAI-7 (CNT-LINE)
                   WHEN  2
                     MOVE  "○"        TO  SC05-FTNWARIAI-8 (CNT-LINE)
                   WHEN OTHER
                     MOVE  WRK-FTNRATEX 
                                     TO  SC05-FTNWARIAI-ETC (CNT-LINE)
                 END-EVALUATE
             ELSE
               IF  MF100-KEY(5:5)    =   "30031"
                 MOVE  WRK-FTNRATEX TO  SC05-FTNWARIAI-ETC (CNT-LINE)
               ELSE
                 EVALUATE  WRK-FTNRATEX
                   WHEN  3
                     MOVE  "○"      TO  SC05-FTNWARIAI-7 (CNT-LINE)
                   WHEN  2
                     MOVE  "○"      TO  SC05-FTNWARIAI-8 (CNT-LINE)
                   WHEN  1
                     MOVE  "○"      TO  SC05-FTNWARIAI-9 (CNT-LINE)
                   WHEN OTHER
                     MOVE  WRK-FTNRATEX
                                     TO  SC05-FTNWARIAI-ETC (CNT-LINE)
                 END-EVALUATE
               END-IF
             END-IF
      *    診療月
      *       MOVE  SPACE               TO  SC05-SRYM (CNT-LINE)
             MOVE  MF100-SRYYM(5:2)    TO  SC05-SRYM (CNT-LINE)
      *    点数表
             EVALUATE  MF100-NYUGAIKBN
               WHEN "1"
                 MOVE  "○"            TO  SC05-TENSUKBN(CNT-LINE 1)
                 MOVE  "○"            TO  SC05-PTENSUKBN(3)
               WHEN "2"
                 MOVE  "○"            TO  SC05-TENSUKBN(CNT-LINE 3)
                 MOVE  "○"            TO  SC05-PTENSUKBN(4)
             END-EVALUATE
      *    入院日数
             EVALUATE  MF100-NYUGAIKBN
               WHEN "1"
                 MOVE  MF100-JNISSU    TO  WRK-NYUNISSUX
                 MOVE  WRK-NYUNISSUX   TO  SC05-NYUNISSU (CNT-LINE)
               WHEN "2"
                 MOVE  SPACE           TO  SC05-NYUNISSU (CNT-LINE)
             END-EVALUATE
      *    点数
             MOVE  MF100-TOTALTEN      TO  WRK-GTENSUX
             IF  MF100-KEY(5:5)       =   "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     MOVE  WRK-GTENSUX TO  SC05-GTENSUN (CNT-LINE)
                   ELSE
                     MOVE  WRK-GTENSUX TO  SC05-GTENSU (CNT-LINE)
                 END-IF
             ELSE
                 MOVE  WRK-GTENSUX     TO  SC05-GTENSU (CNT-LINE)
             END-IF
      *    総点数×割合
             COMPUTE  WRK-GTEN-WARIX   =
                      MF100-TOTALTEN   *   WRK-FTNRATEX
             MOVE  WRK-GTEN-WARIX      TO  SC05-GTEN-WARI (CNT-LINE)
      *    薬剤一部負担金
             MOVE  MF100-YKZFTN        TO  WRK-YKZFTNX
             MOVE  WRK-YKZFTNX         TO  SC05-YKZITBFTN (CNT-LINE)
      *    患者公費情報から精神２１の登録チェック
           INITIALIZE                      PTKOHINF-REC
           MOVE    MF100-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    MF100-PTID          TO  PTKOH-PTID
           MOVE    "021"               TO  PTKOH-KOHNUM
           MOVE    MF100-SRYYM         TO  PTKOH-TEKSTYMD(1:6)
                                           PTKOH-TEKEDYMD(1:6)
           MOVE    "01"                TO  PTKOH-TEKSTYMD(7:2)
                                           PTKOH-TEKEDYMD(7:2)
           PERFORM 900-PTKOHINF-KEY6-SEL-SEC
           IF    ( FLG-PTKOHINF        =   ZERO )
               MOVE    10              TO  WRK-KYURATE
           ELSE
               MOVE    MF100-KYURATE   TO  WRK-KYURATE
           END-IF
      *    合計金額
             IF  MF100-FTNMONEY  NOT = ZERO 
                 IF  MF100-KEY(5:5)     =  "30017"
                     MOVE   MF100-FTNMONEY TO  WRK-KEISAN6
                 ELSE
                     COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN 
                                         *  WRK-KYURATE
                                         /  10
                 END-IF
             ELSE
                 COMPUTE WRK-KEISAN6 =  MF100-TOTALTEN *  WRK-KYURATE
                                        /  10
             END-IF
             MOVE  WRK-KEISAN6         TO  WRK-GKINGKX
             IF  MF100-KEY(5:5)       =   "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     ADD   WRK-KEISAN6 TO  WRK-SKINN
                 ELSE
                     ADD   WRK-KEISAN6 TO  WRK-SKIN  
                 END-IF
             ELSE
                 ADD   WRK-KEISAN6     TO  WRK-SKIN
             END-IF
             IF  MF100-KEY(5:5)       =   "30031"
                 IF  MF100-NYUGAIKBN   =   "1"
                     MOVE  WRK-GKINGKX TO  SC05-GKINGKN (CNT-LINE)
                 ELSE
                     MOVE  WRK-GKINGKX TO  SC05-GKINGK (CNT-LINE)
                 END-IF
             ELSE
                 MOVE  WRK-GKINGKX     TO  SC05-GKINGK (CNT-LINE)
             END-IF
             MOVE  WRK-GKINGKX         TO  SC05-KOHIITBFTN (CNT-LINE)
      *
      *       EVALUATE  MF100-HKNNUM
      *         WHEN  "060"
      *           ADD    1             TO  WRK-SKENSUK
      *         WHEN  "067"
      *           ADD    1             TO  WRK-SKENSUK
      *         WHEN OTHER
      *           ADD    1             TO  WRK-SKENSUS
      *       END-EVALUATE
             ADD    1                 TO  WRK-SKENSU
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
      *
           MOVE    WRK-SKENSU          TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC05-SKEN
                                           SC05-SKEN2
           MOVE    WRK-SKENSUK         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC05-SKENK
           MOVE    WRK-SKENSUS         TO  WRK-SKENSUX
           MOVE    WRK-SKENSUX         TO  SC05-SKENS
           MOVE    WRK-STENSU          TO  WRK-STENSUX
           MOVE    WRK-STENSUX         TO  SC05-STENSU 
           MOVE    WRK-STENSUN         TO  WRK-STENSUX
           MOVE    WRK-STENSUX         TO  SC05-STENSUN
           MOVE    WRK-SKIN            TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC05-SKIN
           MOVE    WRK-SKINN           TO  WRK-SKINX
           MOVE    WRK-SKINX           TO  SC05-SKINN
           MOVE    WRK-SYKZFTN         TO  WRK-SYKZFTNX
           MOVE    WRK-SYKZFTNX        TO  SC05-SYKZFTN
           MOVE    WRK-SNISU           TO  WRK-SNISUX
           MOVE    WRK-SNISUX          TO  SC05-SNISU
           MOVE    WRK-SNNISU          TO  WRK-SNISUX
           MOVE    WRK-SNISUX          TO  SC05-SNNISU
           MOVE    ZERO                TO  WRK-SKENSU
                                           WRK-SKENSUK
                                           WRK-SKENSUS
                                           WRK-STENSU
                                           WRK-SKIN
                                           WRK-STENSUN
                                           WRK-SKINN
                                           WRK-SYKZFTN
                                           WRK-SNISU
                                           WRK-SNNISU
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
               MOVE   "SC3005.red"     TO  SPRT-PRTID
           ELSE
               MOVE   "SC3005"         TO  WRK-ID 
               MOVE    WRK-KAIP-PNO    TO  WRK-PRT-CTV
               MOVE   ".red"           TO  WRK-FILE
               MOVE    WRK-PRT-ID      TO  SPRT-PRTID
           END-IF
           MOVE    "重度心身障害"      TO  SPRT-TITLE
           MOVE    SC05                TO  SPRT-PRTDATA
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
               MOVE  WRK-GKENSUX   TO  SC05-GKENSU
               MOVE  ZERO          TO  WRK-GKOHFTNX
               MOVE  WRK-GKOHFTNX  TO  SC05-GKOHFTN
             WHEN    WRK-STSCD     =   SUM-STSCD (IDX-GOU)
  　　*　  請求件数
               MOVE  SUM-GKENSU  (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC05-GKENSU
  　                                   SC05-GKEN
  　　*　  請求件数（国保）
               MOVE  SUM-GKENSUK (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC05-GKENK
  　　*　  請求件数（社保）
               MOVE  SUM-GKENSUS (IDX-GOU) TO  WRK-GKENSUX
               MOVE  WRK-GKENSUX   TO  SC05-GKENS
      *    点数
               MOVE  SUM-TENSU   (IDX-GOU) TO  WRK-GTENSUX
               MOVE  WRK-GTENSUX   TO  SC05-GTENSU
      *    請求金額
               MOVE  SUM-GKOHFTN (IDX-GOU)  TO  WRK-GKOHFTNX
               MOVE  WRK-GKOHFTNX  TO  SC05-GKOHFTN
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
           DISPLAY "*** SEIKYU3006 IN   "  CNT-MF100
           DISPLAY "*** SEIKYU3006 PAGE "  CNT-PRINT
           DISPLAY "*** SEIKYU3006 END ***"
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
           CALL  "ORCDBMAIN"            USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC                  =   ZERO
               MOVE  "DBFETCH"         TO  MCP-FUNC
               MOVE  "tbl_syskanri"    TO  MCP-TABLE
               MOVE  "key10"           TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"        USING
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
           CALL  "ORCDBMAIN"            USING
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
           CALL    "ORCDBMAIN"          USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hknjainf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING
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
           CALL    "ORCDBMAIN"          USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-HKNJAINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理(KEY6)
      *****************************************************************
       900-PTKOHINF-KEY6-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTKOHINF-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA

           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
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
           CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
