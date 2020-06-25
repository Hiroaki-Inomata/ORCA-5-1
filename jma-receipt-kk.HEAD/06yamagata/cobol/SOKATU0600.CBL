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
       PROGRAM-ID.             SOKATU0600.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・総括表（山形）用集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/01    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者　日付		内容
      *  01.00.01    山口   06/02/01   返戻対応
      *  01.00.02    内田   07/06/17   レセ電の返戻分は紙提出分と印字
      *                                 (返戻分は合算するため返戻対応を外す)
      *  01.00.03    内田   07/09/30   日レセver4.0対応
      *                                システム管理マスタ「1901」の
      *                                医療機関情報を参照
      *  01.00.04    吉川   08/09/09   SPA-AREA初期化
      *  01.00.05    吉川   08/10/14   札幌市代表保険者番号変更
      *  01.00.06    内田   09/02/25   返戻対応
      *  01.00.07    吉川   09/08/11   仙台市代表保険者番号変更
      *  01.00.08    吉川   09/11/05   特別療養費国保対応
      *  01.00.09    吉川   10/07/02   代表保険者番号追加
      *                                オンライン返戻対応
      *  01.00.10    吉川   11/03/08   open-cobol1.0対応
      *  01.00.11    吉川   12/12/21   医療機関情報編集対応
      *  01.00.12    萩野   14/09/11   返戻時、再印刷画面のタイトル修正
      *  01.00.13    吉川   14/10/03   ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.14    萩野   15/10/08   医療機関編集情報への開設者名追加対応
      *  01.00.15    吉川   17/07/14   仙台市の代表保険者番号変更
      *                                医療機関コード変更時、
      *                                医療機関コード別の作成処理対応
      *  01.00.16    萩野   18/05/08   SITEKBN変更（動作に変更なし）
      *  01.00.17    井上   18/10/11   修正履歴コメント削除
      *  01.00.18    萩野   19/03/07   システム管理マスタ「2005」を設定していない場合、
      *                                処理がエラーとなるのを修正。
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    総括表一時ファイル  
           SELECT  SKT0600TMP-FILE ASSIGN  SKT0600TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT0600TMP-KEY
                                   FILE    STATUS  IS  STS-SKT0600.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    総括表一時ファイル  
       FD  SKT0600TMP-FILE.
           COPY    "CPSKT0600TMP.INC".
      *
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(3000).
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    総括表一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT0600TP//.
      *
      *    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *     
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    印刷用領域
           COPY    "SKT0600.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SKT0600         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT0600TMP      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD        PIC X(08).
           03  WRK-SEIYMDWH      PIC X(20).
           03  WRK-SRYYMWH       PIC X(16).
           03  WRK-SRYYMD        PIC X(08).
           03  WRK-HKNJANUM.
               05  WRK-HKNJANUM1 PIC 9(02).
               05  WRK-HKNJANUM2 PIC 9(03).
               05  WRK-HKNJANUM3 PIC 9(01).
               05  WRK-HKNJANUM4 PIC 9(02).
      *  
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-Z9-7              PIC ZZZ,ZZZ.
           03  WRK-Z9-14             PIC ZZ,ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR           PIC X(200). 
      *
           03  WRK-KBN               PIC  9(02).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2           PIC X(50).
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
           03  WRK-KAISETUNAME     PIC X(80).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE              PIC 9(03).
           03  CNT-RECE10            PIC 9(06).
           03  CNT-UPDFILE           PIC 9(06).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-NYUIN.
               05  SUM-N-KENSU       PIC  9(06).
               05  SUM-N-NISSU       PIC  9(06).
               05  SUM-N-TENSU       PIC  9(10).
           03  SUM-GAIRAI.
               05  SUM-G-KENSU       PIC  9(06).
               05  SUM-G-NISSU       PIC  9(06).
               05  SUM-G-TENSU       PIC  9(10).
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ＪＯＢ管理ＤＢ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＪＯＢ管理ＤＢ（制御用）
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *    レセプト・総括印刷情報
           COPY    "CPSK2005.INC".
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
      *    請求管理情報
           COPY    "CPRCF010.INC".
      * 
      ******************************************************************
      *    サブプロ用  領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".          
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
      *****************************************************************
      *    主    処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "*** SOKATU0600 START ***"
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
                    UNTIL   FLG-END   =   1
      *
           PERFORM  300-END-SEC
      *
           STOP     RUN
      *
           .
      *
      *****************************************************************
      *    初期  処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                   FLG-AREA
           INITIALIZE                   STS-AREA
           INITIALIZE                   WRK-AREA
           INITIALIZE                   CNT-AREA
           INITIALIZE                   SUM-AREA
           INITIALIZE                   SPA-AREA
           INITIALIZE                  RECEERR
      *
           ACCEPT     SYS-TIME          FROM    TIME     
      *
           PERFORM    100-DBOPEN-SEC
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-TOKUBETU
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
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
           MOVE    "SOKATU0600"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT0600"         TO  SKT0600TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID     TO  SKT0600TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT0600TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID     TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT0600TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT0600TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
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
      *    医療機関ＩＤ編集
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE    "*"            TO      SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"           TO      SYS-1001-STYUKYMD(7:2)
                                          SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM    TO      SYS-1001-HOSPNUM
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
                  MOVE    1               TO    FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                     SYS-1002-REC
           MOVE     "1002"        TO      SYS-1002-KANRICD
           MOVE     "*"           TO      SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"           TO      SYS-1002-STYUKYMD(7:2)
                                          SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM    TO      SYS-1002-HOSPNUM
           MOVE     SYS-1002-REC  TO      MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
                    MOVE    MCPDATA-REC   TO    SYS-1002-REC
                    IF      SYS-1002-ADRS =     LOW-VALUE
                            MOVE    SPACE TO    SYS-1002-ADRS
                    END-IF
           ELSE
                    MOVE "医療機関情報（所在地）が取得できませんでした"
                                          TO    WRK-RECEERR
                    PERFORM   500-ERR-HENSYU-SEC
                    MOVE      1           TO    FLG-END
           END-IF 
      *
      *    レセ電情報取得
           INITIALIZE                     SYS-2005-REC
           MOVE     "2005"        TO      SYS-2005-KANRICD
           MOVE     "*"           TO      SYS-2005-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                  TO      SYS-2005-STYUKYMD(1:6)
                                          SYS-2005-EDYUKYMD(1:6)
           MOVE    "01"           TO      SYS-2005-STYUKYMD(7:2)
                                          SYS-2005-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM    TO      SYS-2005-HOSPNUM
           MOVE     SYS-2005-REC  TO      MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
                    MOVE    MCPDATA-REC   TO    SYS-2005-REC
           ELSE
                    INITIALIZE                  SYS-2005-REC
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
           OPEN     OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
           END-EVALUATE
      *
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主    処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           OPEN     OUTPUT  SKT0600TMP-FILE
           CLOSE            SKT0600TMP-FILE
           OPEN     I-O     SKT0600TMP-FILE
      *
           INITIALIZE             SUM-AREA
      *
           MOVE     ZERO    TO        FLG-END
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           IF       FLG-UPD   =         1
                    MOVE      ZERO      TO        WRK-KBN
                    PERFORM   2002-PRINT-SEC
                              UNTIL     WRK-KBN   =    99
           END-IF
      *
           CLOSE   UPD-FILE
           CLOSE   SKT0600TMP-FILE
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了    処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      CNT-RECE10          =   ZERO 
                   CONTINUE
           ELSE
                   IF      FLG-UPD     =   1
                           PERFORM     3001-SEIKYU-UPDATE-SEC
                   END-IF
           END-IF    
      *
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU0600 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU0600 END ***"
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

           PERFORM 900-DBDISCONNECT-SEC
      *
           .
      *
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号別集計処理                        
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC               SECTION.
      *
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT0600TMP-REC
           INITIALIZE       SKT0600TMP-HKNJANUM
           INITIALIZE       WRK-HKNJANUM
      *
           IF  RECE10-HKNJANUM(7:2)  =     SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  WRK-HKNJANUM(1:6)
             MOVE  RECE10-HKNJANUM(1:6)  TO  SKT0600TMP-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  WRK-HKNJANUM(1:6)
             MOVE  RECE10-HKNJANUM(3:6)  TO  SKT0600TMP-HKNJANUM(1:6)
           END-IF
      *
      *区の場合代表番号セット
           PERFORM  2002-HKNJANUM-SET-SEC
           MOVE  WRK-HKNJANUM  TO  SKT0600TMP-HKNJANUM
      *
      *県内／県外判定
           MOVE  RECE10-PREFKBN  TO  SKT0600TMP-KBN
      *
           PERFORM  900-TMP-READ-SEC
           PERFORM  220-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき                
           IF     FLG-UPD        =       1
                  PERFORM   240-UPD-FILE-HENSYU-SEC
      *
                  PERFORM   900-SEIKYU-READ-SEC
           ELSE
                  MOVE      SPACE TO      WRK-RECEERR
                  STRING    "集計対象外  患者番号＝"
                                               DELIMITED  BY  SIZE
                            RECE10-PTNUM       DELIMITED  BY  SPACE
                            " 診療年月＝"      DELIMITED  BY  SIZE
                            RECE10-SRYYM       DELIMITED  BY  SIZE
                            " レセ種別＝"      DELIMITED  BY  SIZE
                            RECE10-RECESYUBETU DELIMITED  BY  SIZE
                         INTO                  WRK-RECEERR
                  END-STRING                                
                  PERFORM   500-ERR-HENSYU-SEC
                  MOVE      1         TO        FLG-END
           END-IF
      *
           .
      *
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    代表番号セット
      *****************************************************************
       2002-HKNJANUM-SET-SEC                SECTION.
      *
      *    札幌市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "014"
             MOVE  "010017"  TO  WRK-HKNJANUM
           END-IF
      *    仙台市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "044"
             MOVE  "044008"  TO  WRK-HKNJANUM
           END-IF
      *    さいたま市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "114"
             MOVE  "114009"  TO  WRK-HKNJANUM
           END-IF
      *    千葉市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "124"
             MOVE  "124008"  TO  WRK-HKNJANUM
           END-IF
      *    横浜市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "144"
             MOVE  "144006"  TO  WRK-HKNJANUM
           END-IF
      *    川崎市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "145"
             MOVE  "145003"  TO  WRK-HKNJANUM
           END-IF
      *    相模原市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "146"
             MOVE  "146001"  TO  WRK-HKNJANUM
           END-IF
      *    新潟市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "154"
             MOVE  "154005"  TO  WRK-HKNJANUM
           END-IF
      *    静岡市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "224"
             MOVE  "224006"  TO  WRK-HKNJANUM
           END-IF
      *    浜松市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "225"
             MOVE  "225003"  TO  WRK-HKNJANUM
           END-IF
      *    名古屋市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "234"
             MOVE  "234005"  TO  WRK-HKNJANUM
           END-IF
      *    京都市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "264"
             MOVE  "264002"  TO  WRK-HKNJANUM
           END-IF
      *    大阪市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  = "274"
             MOVE  "274001"  TO  WRK-HKNJANUM
           END-IF
      *    堺市の場合、代表番号セット
           IF  WRK-HKNJANUM(1:3)  = "275"
             MOVE  "275008"  TO  WRK-HKNJANUM
           END-IF
      *    神戸市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "284"
             MOVE  "284000"  TO  WRK-HKNJANUM
           END-IF
      *    広島市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "344"
             MOVE  "344002"  TO  WRK-HKNJANUM
           END-IF
      *    北九州市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "404"
             MOVE  "404004"  TO  WRK-HKNJANUM
           END-IF
      *    福岡市の場合、代表番号セット
           IF  WRK-HKNJANUM (1:3) = "405"
             MOVE  "405001"  TO  WRK-HKNJANUM
           END-IF
      *
           .
       2002-HKNJANUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       220-HKN-SYUKEI-SEC               SECTION.
      *
      *    入院
           IF   RECE10-NYUGAIKBN    =     1
      *         件数
                ADD      1    TO   SKT0600TMP-N-KENSU
      *
      *         日数
                COMPUTE  SKT0600TMP-N-NISSU       =
                         SKT0600TMP-N-NISSU       +
                         RECE10-JNISSU(1)
      *
      *         点数
                COMPUTE  SKT0600TMP-N-TENSU       =
                         SKT0600TMP-N-TENSU       +
                         RECE10-TOTALTEN(1)
      *
      *    入院外
           ELSE
      *         件数
                ADD      1    TO   SKT0600TMP-G-KENSU
      *
      *         日数
                COMPUTE  SKT0600TMP-G-NISSU       =
                         SKT0600TMP-G-NISSU       +
                         RECE10-JNISSU(1)
      *
      *         点数
                COMPUTE  SKT0600TMP-G-TENSU       =
                         SKT0600TMP-G-TENSU       +
                         RECE10-TOTALTEN(1)
           END-IF
      *
           IF      FLG-SKT0600TMP     =          1
                   WRITE    SKT0600TMP-REC
           ELSE
                   REWRITE  SKT0600TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
      *
       220-HKN-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理             
      *****************************************************************
       240-UPD-FILE-HENSYU-SEC         SECTION.
      *
      *    集計の対象にならなかったとき                
           IF     FLG-UPD   NOT   =       1
                  MOVE      SPACE TO      WRK-RECEERR
                  STRING    "集計対象外  患者番号＝"
                                               DELIMITED  BY  SIZE
                            RECE10-PTNUM       DELIMITED  BY  SPACE
                            " 診療年月＝"      DELIMITED  BY  SIZE
                            RECE10-SRYYM       DELIMITED  BY  SIZE
                            " レセ種別＝"      DELIMITED  BY  SIZE
                            RECE10-RECESYUBETU DELIMITED  BY  SIZE
                         INTO                  WRK-RECEERR
                  END-STRING                                
                  PERFORM   500-ERR-HENSYU-SEC
                  MOVE      1         TO        FLG-END
                  GO        TO        240-UPD-FILE-HENSYU-EXT 
           END-IF
      *
           MOVE   RECE10-REC          TO        RECE10X-REC
           WRITE  RECE10X-REC
      *
           ADD    1                   TO        CNT-UPDFILE
      *
           .
      *
       240-UPD-FILE-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票ファイル出力処理                    
      *****************************************************************
       2002-PRINT-SEC                   SECTION.
      *
           DISPLAY  "2002-PRINT-SEC"
      *
      *    帳票編集<見出し>処理
           MOVE     ZERO      TO        FLG-PRINT
           PERFORM  310-HEAD-HEN-SEC
                              UNTIL     FLG-PRINT      =    1
      *
           IF       WRK-KBN   =         99
                    GO        TO        2002-PRINT-EXT
           END-IF
      *
      *    帳票編集<保険明細>処理
           DISPLAY  "WRK-KBN  " WRK-KBN
           MOVE     ZERO      TO        FLG-PRINT
           MOVE     ZERO      TO        IDX1
           INITIALIZE                   SUM-AREA
           PERFORM  320-BODY-HEN-SEC
                              UNTIL     FLG-PRINT      =    1
      *
      *    帳票印刷処理
           IF  IDX1      >    ZERO
               PERFORM   330-PRINT-OUT-SEC
           END-IF
      *
           .
      *
       2002-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC             SECTION.
      *
           DISPLAY  "310-HEAD-HEN-SEC"
      *
           INITIALIZE                   SKT0600TMP-KEY
           ADD      1           TO      WRK-KBN
           MOVE     WRK-KBN     TO      SKT0600TMP-KBN
           START    SKT0600TMP-FILE
                    KEY     IS    >     SKT0600TMP-KEY
                    INVALID       KEY
                    MOVE    1     TO    FLG-PRINT
           END-START
      *
           IF     FLG-PRINT   =     1
                  ADD       1       TO      WRK-KBN
                  IF        WRK-KBN >       2
                            MOVE    99      TO    WRK-KBN
                  ELSE
                            MOVE    ZERO    TO    FLG-PRINT
                  END-IF
                  GO        TO      310-HEAD-HEN-EXT
           END-IF
      *
           INITIALIZE                             SKT0600
      *
           MOVE     WRK-SRYYMWH              TO   SKT0600-SEIYM
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS              TO   SKT0600-ADRS
           INSPECT SKT0600-ADRS              REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME              TO   SKT0600-HOSPNAME
           INSPECT SKT0600-HOSPNAME          REPLACING ALL "  " BY "　"
      *
           MOVE     WRK-KAISETUNAME          TO   SKT0600-KAISETUNAME
      *
           MOVE     SYS-1001-HOSPCD          TO   SKT0600-HOSPCD
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"              TO   SKT0600-TOKUBETU
           END-IF
      *
           MOVE    1                         TO   FLG-PRINT
      *
           .
      *
       310-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<保険明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
           DISPLAY  "320-BODY-HEN-SEC"
      *
           READ     SKT0600TMP-FILE   NEXT
                    AT      END
                    MOVE      1    TO        FLG-PRINT
           END-READ
      *
           DISPLAY  "SKT0600TMP-KBN      " SKT0600TMP-KBN
           DISPLAY  "SKT0600TMP-HKNJANUM " SKT0600TMP-HKNJANUM
           DISPLAY  "FLG-PRINT           " FLG-PRINT
      *
           IF       FLG-PRINT      =         1
               OR   WRK-KBN        NOT =     SKT0600TMP-KBN
               DISPLAY  "WRK-KBN             " WRK-KBN
               DISPLAY  "SKT0600TMP-KBN      " SKT0600TMP-KBN
               DISPLAY  "FLG-PRINT           " FLG-PRINT
      *        総計入院・件数
               MOVE      SUM-N-KENSU    TO   WRK-Z9-7
               MOVE      WRK-Z9-7       TO   SKT0600-N-KENSU(27)
      *
      *        総計入院・日数
               MOVE      SUM-N-NISSU    TO   WRK-Z9-7
               MOVE      WRK-Z9-7       TO   SKT0600-N-NISSU(27)
      *
      *        総計入院・点数
               MOVE      SUM-N-TENSU    TO   WRK-Z9-14
               MOVE      WRK-Z9-14      TO   SKT0600-N-TENSU(27)
      *
      *        総計入院外・件数
               MOVE      SUM-G-KENSU    TO   WRK-Z9-7
               MOVE      WRK-Z9-7       TO   SKT0600-G-KENSU(27)
      *
      *        総計入院外・日数
               MOVE      SUM-G-NISSU    TO   WRK-Z9-7
               MOVE      WRK-Z9-7       TO   SKT0600-G-NISSU(27)
      *
      *        総計入院外・点数
               MOVE      SUM-G-TENSU    TO   WRK-Z9-14
               MOVE      WRK-Z9-14      TO   SKT0600-G-TENSU(27)
               MOVE      1    TO   FLG-PRINT
               GO        TO   320-BODY-HEN-EXT
           END-IF
      *
           ADD      1         TO        IDX1
           IF       IDX1      >         26
                    PERFORM   330-PRINT-OUT-SEC
                    INITIALIZE          SKT0600-DATA
                    MOVE      1         TO        IDX1
           END-IF
      *
           PERFORM  3201-BODY-HEN-SEC
      *
           .
      *
       320-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<保険明細行>処理
      *****************************************************************
       3201-BODY-HEN-SEC              SECTION.
      *
           DISPLAY  "3201-BODY-HEN-SEC"
      *
      *    保険者名
           MOVE     SPACE               TO  HKNJAINF-REC
           INITIALIZE                       HKNJAINF-REC
           MOVE     SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE     SKT0600TMP-HKNJANUM TO  HKNJA-HKNJANUM
           MOVE     HKNJAINF-REC        TO  MCPDATA-REC
           PERFORM  900-HKNJAINF-INV-SEC
           IF       FLG-HKNJAINF        =   ZERO
                    MOVE    HKNJA-HKNJANAME
                                        TO  SKT0600-HKNJANAME(IDX1)
           ELSE
                    MOVE    SPACE       TO  SKT0600-HKNJANAME(IDX1)
           END-IF
      *
           IF   SKT0600TMP-HENREIYM  NOT =  SPACE
           AND  SKT0600TMP-HENREIYM  NOT =  "000000"
               MOVE  "（紙提出分）"     TO  SKT0600-KAMIBUN (IDX1)
           END-IF
      *
      *    入院・件数
           MOVE     SKT0600TMP-N-KENSU       TO   WRK-Z9-7
           MOVE     WRK-Z9-7                 TO   SKT0600-N-KENSU(IDX1)
      *
      *    入院・日数
           MOVE     SKT0600TMP-N-NISSU       TO   WRK-Z9-7
           MOVE     WRK-Z9-7                 TO   SKT0600-N-NISSU(IDX1)
      *
      *    入院・点数
           MOVE     SKT0600TMP-N-TENSU       TO   WRK-Z9-14
           MOVE     WRK-Z9-14                TO   SKT0600-N-TENSU(IDX1)
      *
      *    入院外・件数
           MOVE     SKT0600TMP-G-KENSU       TO   WRK-Z9-7
           MOVE     WRK-Z9-7                 TO   SKT0600-G-KENSU(IDX1)
      *
      *    入院外・日数
           MOVE     SKT0600TMP-G-NISSU       TO   WRK-Z9-7
           MOVE     WRK-Z9-7                 TO   SKT0600-G-NISSU(IDX1)
      *
      *    入院外・点数
           MOVE     SKT0600TMP-G-TENSU       TO   WRK-Z9-14
           MOVE     WRK-Z9-14                TO   SKT0600-G-TENSU(IDX1)
      *
      *    入院／入院外の合計加算
           ADD       SKT0600TMP-N-KENSU      TO   SUM-N-KENSU
           ADD       SKT0600TMP-N-NISSU      TO   SUM-N-NISSU
           ADD       SKT0600TMP-N-TENSU      TO   SUM-N-TENSU
      *
           ADD       SKT0600TMP-G-KENSU      TO   SUM-G-KENSU
           ADD       SKT0600TMP-G-NISSU      TO   SUM-G-NISSU
           ADD       SKT0600TMP-G-TENSU      TO   SUM-G-TENSU
      *
           .
      *
       3201-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                   SECTION.
      *
           DISPLAY  "330-PRINT-OUT-SEC"
      * 
           ADD  1  TO  CNT-PAGE
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
           MOVE   "SKT0600.red"            TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "総括表"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "総括表［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "総括表［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "総括表［返戻分］［特別療養費］" TO  SPRT-TITLE
            END-EVALUATE
           MOVE    SKT0600             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           DISPLAY  "ORCSPRT call"
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               DISPLAY  "ORCSPRT call ok"
               CONTINUE
           ELSE
               DISPLAY  "ORCSPRT call ng"
               MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
      *
       330-PRINT-OUT-EXT.
           EXIT.
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
      *        ジョブ終了処理
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
      *
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *   請求管理ＤＢ更新処理                       
      *****************************************************************
       3001-SEIKYU-UPDATE-SEC              SECTION.
      *
           OPEN     INPUT           UPD-FILE
      *
           PERFORM  900-UPD-FILE-READ-SEC
      *
           PERFORM  UNTIL   FLG-UPDFILE     =     1
              MOVE  RECE10X-KEY     TO    RECE10-KEY
              MOVE  RECE10-REC      TO    MCPDATA-REC
      *   
              PERFORM   900-SEIKYU-INV-SEC
              IF        FLG-SEIKYU  =     ZERO
                        MOVE     "2"    TO     RECE10-SKYKBN
                        MOVE     LNK-PRTKANRI-SRYYM
                                        TO     RECE10-SKYYM
                        MOVE     LNK-PRTKANRI-SKYYMD
                                        TO     RECE10-UPDYMD
                        MOVE     RECE10-REC
                                        TO     MCPDATA-REC
                        MOVE     "DBUPDATE"
                                        TO     MCP-FUNC
                        MOVE    "tbl_seikyu"    TO  MCP-TABLE
                        MOVE    "key"           TO  MCP-PATHNAME
                        CALL    "ORCDBMAIN"     USING
                                                MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
                        IF       MCP-RC =      ZERO
                                 PERFORM       900-UPD-FILE-READ-SEC
                        ELSE
                                 DISPLAY "UPD ERR KEY="  RECE10-KEY 
                                 MOVE    
                                "*** SOKATU0600 SEIKYU UPDATE ERROR ***"
                                           TO  WRK-RECEERR
                                 MOVE    RECE10-KEY     TO
                                         WRK-RECEERR(33:)
                                 PERFORM 500-ERR-HENSYU-SEC
                                 MOVE    1      TO      FLG-UPDFILE
                        END-IF
      *
              ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                       MOVE    "*** SOKATU0600 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY     TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1      TO      FLG-UPDFILE
              END-IF
           END-PERFORM
      *
           CLOSE   UPD-FILE
      *
           .
      *
       3001-SEIKYU-UPDATE-EXT.
           EXIT.
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
      *
           .
      *
       31012-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
           MOVE   SPA-HOSPNUM           TO    RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
           MOVE   SPACE                 TO    RECE10-XXSTHKNJANUM
           MOVE   "99999999"            TO    RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000               TO    RECE10-SRYYM
             MOVE  999999               TO    RECE10-XXSRYYM
           END-IF
      *
           MOVE   RECE10-REC                   TO    MCPDATA-REC.
           MOVE   "DBSELECT"                   TO    MCP-FUNC.
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF     MCP-RC        =       ZERO
                  PERFORM  900-SEIKYU-READ-SEC
           ELSE
                  MOVE    1    TO      FLG-END
           END-IF
      *
           .
      *
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    総括表一時ファイルＲＥＡＤ処理
      *****************************************************************
       900-TMP-READ-SEC                 SECTION.
      *
           MOVE   ZERO                TO        FLG-SKT0600TMP
           READ   SKT0600TMP-FILE
                  INVALID     KEY     
                              MOVE    1     TO      FLG-SKT0600TMP
           END-READ
      *
           .
      *
       900-TMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE   "DBFETCH"               TO    MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF       MCP-RC  =     ZERO
                    MOVE    MCPDATA-REC   TO      RECE10-REC
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
                    ADD     1             TO      CNT-RECE10   
           ELSE
                    MOVE    1             TO      FLG-END
           END-IF
      *
           .
      *
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBCLOSECURSOR"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       900-SEIKYU-INV-SEC             SECTION.
      *
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       900-SEIKYU-INV-EXT.
           EXIT.
      *****************************************************************
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
      *
           .
      *
       900-UPD-FILE-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"               TO  MCP-FUNC
           MOVE    "tbl_hknjainf"         TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
               MOVE    "tbl_hknjainf"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
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
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
      *    ＤＢ  オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ  クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       900-DBDISCONNECT-EXT.
           EXIT.
