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
       PROGRAM-ID.     SEIKYU30031.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 和歌山公費
      *  コンポーネント名  : 和歌山一人親家庭医療費請求書(143)３歳未満
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/05/01    笠原
      *****************************************************************
      * wakayama chihou kouhi contribution
      * Special thanks to michiyo noda,motohide takagaki,katsunori yoneda
      * for help in development
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01       楠本    04/02/12         公費DB化対応 ３歳未満
      *  01.00.02       楠本    05/06/14         ＯＲＣＡ２．５対応
      *  01.00.03       楠本    06/07/07         MONFUNC DBCLOSESURSOR 対応
      *  02.00.00       楠本　　07/10/11         ORCA4.0対応
      *  02.00.01       吉川    10/06/02         SPA-AREA初期化
      *                                          かつらぎ町は未就学で帳票を分ける
      *  02.00.02       吉川    10/06/09         個別発行の処理方法変更
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI3003.INC".
      *
       WORKING-STORAGE  SECTION.
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100   PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUNOU  PIC 9(01).
           03  FLG-END  PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
           03  FLG-ERR        PIC 9(01).
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX  PIC 9(04).
           03  IDY  PIC 9(04).
           03  IDZ  PIC 9(04).
           03  IDXX           PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *      03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03 WRK-KOHNUM        PIC X(03).
           03 WRK-KOHFTN        PIC 9(10).
           03 WRK-SYUCOMPTOTAL  PIC 9(10).
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
           03 WRK-HKNJANUM      PIC X(05).
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MF100   PIC 9(06).
           03  CNT-RECE02  PIC 9(06).
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC        OCCURS  100.
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
      *         05  TBL-KBN         PIC X(01).
      *         05  TBL-PARA        PIC 9(10).
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *
       01  WRK-CONS-PARA-MAX       PIC 9(03)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
           COPY  "MCPDATA.INC".
      *     COPY  "CPORCMCP.INC".
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
      *=== SPA-AREA初期化 by Yoshikawa(2010/06/02) start
           INITIALIZE  SPA-AREA
      *=== SPA-AREA初期化 by Yoshikawa(2010/06/02) end
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
                                               WRK-PARA-SYORIFLG
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU3003"    TO  JOB-PGID
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) start
      *    MOVE    "ひとり親３歳未満" 
          MOVE    "ひとり親" 
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) end
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE  "MF100ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)  TO  MF100PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  MF100PARA-HOSPNUM
      *
           OPEN  OUTPUT MF100-FILE
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
           MOVE    ZERO                TO  IDXX
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
                                           FLG-SYORI
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE   "tbl_btpara"         TO  MCP-TABLE
               MOVE   "key5"               TO  MCP-PATHNAME
               MOVE    "DBSELECT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"          USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE   "tbl_btpara"     TO  MCP-TABLE
                   MOVE   "key5"           TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
      *             MOVE    BTPARA-INFO-KBN     TO  TBL-KBN  (IDXX)
      *             MOVE    BTPARA-INFO-PTID    TO  TBL-PARA (IDXX)
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
                           "パラメタ="     BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *
                   MOVE   "tbl_btpara"     TO  MCP-TABLE
                   MOVE   "key5"           TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
               MOVE   "tbl_btpara"         TO  MCP-TABLE
               MOVE   "key5"               TO  MCP-PATHNAME
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
           INITIALIZE  MF100-REC
      *
      *     MOVE  RECE02-HOSPID       TO  MF100-HOSPID
           MOVE  RECE02-HOSPNUM      TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
             IF  RECE02-KOHNUM (IDX) =   "143"
                 MOVE  RECE02-KOHINF      (IDX)  TO  MF100-KOHINF
                 MOVE  RECE02-KOHFTNJANUM (IDX)  TO  MF100-FTNJANUM
                 MOVE  RECE02-KOHJKYSNUM  (IDX)  TO  MF100-KOHJKYSNUM
                 MOVE  RECE02-FTNMONEY    (IDX)  TO  MF100-FTNMONEY
                 COMPUTE IDY     =       IDX     +   1
                 MOVE  RECE02-JNISSUINF   (IDY)  TO  MF100-JNISSUINF
                 MOVE  RECE02-TOTALTENINF (IDY)  TO  MF100-TOTALTENINF
                 MOVE  RECE02-YKZFTNINF   (IDY)  TO  MF100-YKZFTNINF
      *
      *
                 MOVE  "143"  TO  WRK-KOHNUM
                 PERFORM  900-SYUCOMPTOTAL-SET-SEC
                 MOVE  WRK-SYUCOMPTOTAL  TO  MF100-SYUCOMPTOTAL
      *
                 MOVE  "143"  TO  WRK-KOHNUM
                 PERFORM  900-KOHFTN-SET-SEC
                 MOVE  WRK-KOHFTN  TO  MF100-KOHFTN
                 MOVE    4                           TO  IDX
             END-IF
           END-PERFORM
      *
           IF  MF100-KEY(7:3)         = "012" OR "018"
               MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBNP
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) start
               IF    RECE02-FLG(1)    =  1
                 MOVE  "1"               TO  MF100-MISYUKBNP
               ELSE
                 MOVE  "0"               TO  MF100-MISYUKBNP
               END-IF
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) end
           ELSE
               MOVE  "0"                 TO  MF100-NYUGAIKBNP
           END-IF
      *
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
      *    保険者番号チェック処理
           MOVE    ZERO                TO  FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *
           IF  MF100-KEY(7:3)         = "018"
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
             IF  FLG-ERR              =  ZERO
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
               WRITE  MF100-REC
               ADD  1  TO  CNT-MF100
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
             END-IF
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
           END-IF
      *
           MOVE   "tbl_kohsky"         TO  MCP-TABLE
           MOVE   "key2"               TO  MCP-PATHNAME
           PERFORM 900-KOHSKY-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "30"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-FTNJANUM(5:3)      TO  WRK-HKNJANUM(3:3)
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
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           MOVE   "tbl_kohsky"         TO  MCP-TABLE
           MOVE   "key2"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           CLOSE  MF100-FILE
      *
           DISPLAY "*** SEIKYU3003 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU3003 OUT "  CNT-MF100 
           DISPLAY "*** SEIKYU3003 END ***"
      *
           IF  CNT-MF100  >  0
             CALL  "SEIKYU3004"  USING  WRK-PARA
           ELSE
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
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
      *    公費請求書マスタ読込  
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
      *
           INITIALIZE                  RECE02-REC
      *     MOVE    WRK-PARA-HOSPID     TO  RECE02-HOSPID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE   "tbl_kohsky"         TO  MCP-TABLE
           MOVE   "key2"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE   "tbl_kohsky"     TO  MCP-TABLE
               MOVE   "key2"           TO  MCP-PATHNAME
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
           CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE02-REC
               IF  RECE02-KOHNUM (1)  =  "143"  OR
                   RECE02-KOHNUM (2)  =  "143"  OR
                   RECE02-KOHNUM (3)  =  "143"  OR
                   RECE02-KOHNUM (4)  =  "143"
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) start
      *             IF  RECE02-AGE     <   3
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) end
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) start
      *              MOVE  ZERO                TO  FLG-SYORI  
      *              IF      WRK-PARA-SYORIFLG   =   "1"
      *                PERFORM VARYING IDXX    FROM    1   BY  1
      *                     UNTIL   IDXX    >   WRK-CONS-PARA-MAX
      *                     OR      TBL-PARA (IDXX)     =   ZERO
      *                 IF  TBL-KBN (IDXX)            =   "1"
      *                     IF RECE02-KOHNUM (1)      =   "143" AND 
      *                        RECE02-KOHFTNJANUM (1) (3:5)
      *                                     =   TBL-PARA(IDXX) (1:5)
      *                         MOVE    1   TO  FLG-SYORI
      *                     END-IF
      *                     IF RECE02-KOHNUM (2)      =   "143" AND 
      *                        RECE02-KOHFTNJANUM (2) (3:5)
      *                                     =   TBL-PARA(IDXX) (1:5)
      *                         MOVE    1   TO  FLG-SYORI
      *                     END-IF
      *                     IF RECE02-KOHNUM (3)      =   "143" AND 
      *                        RECE02-KOHFTNJANUM (3) (3:5)
      *                                     =   TBL-PARA(IDXX) (1:5)
      *                         MOVE    1   TO  FLG-SYORI
      *                     END-IF
      *                     IF RECE02-KOHNUM (4)      =   "143" AND 
      *                        RECE02-KOHFTNJANUM (4) (3:5)
      *                                     =   TBL-PARA(IDXX) (1:5)
      *                         MOVE    1   TO  FLG-SYORI
      *                     END-IF
      *
      *                 ELSE
      *                     IF  RECE02-PTID =   TBL-PARA (IDXX)
      *                         MOVE    1   TO  FLG-SYORI
      *                     END-IF
      *                 END-IF
      *                END-PERFORM 
      *              ELSE
      *                MOVE    1                 TO  FLG-SYORI
      *              END-IF
      *              IF      FLG-SYORI           =   ZERO
      *                GO  TO  900-KOHSKY-READ-SEC
      *              END-IF
      *    個別発行で患者番号指定の場合、対象判定を行う
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
                   DISPLAY "患者番号=" RECE02-PTNUM
                           "負担者番号=" RECE02-KOHFTNJANUM(1)
                           "保険者番号=" RECE02-HKNJANUM
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF
      *=== 個別発行の処理方法の変更 by Yoshikawa(2010/06/09) end
      *
                       ADD  1  TO  CNT-RECE02
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) start
      *             ELSE
      *                 GO  TO  900-KOHSKY-READ-SEC
      *             END-IF
      *=== 未就学で帳票を分ける by Yoshikawa(2010/06/02) end
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
       900-KOHSKY-READ-EXT.
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
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
      *****************************************************************
      *    主保険患者負担額算出
      *****************************************************************
       900-SYUCOMPTOTAL-SET-SEC  SECTION.
      *
      *
           INITIALIZE  SYUNOU-REC
      *     MOVE  RECE02-HOSPID       TO  SYU-HOSPID
           MOVE  RECE02-HOSPNUM      TO  SYU-HOSPNUM
           MOVE  RECE02-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE  RECE02-PTID         TO  SYU-PTID
           MOVE  SYUNOU-REC          TO  MCPDATA-REC
           MOVE  "tbl_syunou"        TO  MCP-TABLE
           MOVE  "key2"              TO  MCP-PATHNAME
           MOVE  "DBSELECT"          TO  MCP-FUNC
           CALL  "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"    TO  MCP-TABLE
               MOVE  "key2"          TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
      *
               INITIALIZE  WRK-SYUCOMPTOTAL
               PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  LNK-PRTKANRI-SRYYM  =  SYU-SRYYMD(1:6)
                 IF  SYU-KOH1HKNNUM = WRK-KOHNUM OR
                     SYU-KOH2HKNNUM = WRK-KOHNUM OR
                     SYU-KOH3HKNNUM = WRK-KOHNUM OR
                     SYU-KOH4HKNNUM = WRK-KOHNUM
                   ADD  SYU-SYUCOMPTOTAL  TO  WRK-SYUCOMPTOTAL
                 END-IF
               END-IF
      *
               MOVE  "tbl_syunou"    TO  MCP-TABLE
               MOVE  "key2"          TO  MCP-PATHNAME
               PERFORM   900-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR" UPON CONSOLE
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "tbl_syunou"          TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-SYUCOMPTOTAL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額
      *****************************************************************
       900-KOHFTN-SET-SEC  SECTION.
      *
      *
           INITIALIZE  SYUNOU-REC
      *     MOVE  RECE02-HOSPID       TO  SYU-HOSPID
           MOVE  RECE02-HOSPNUM      TO  SYU-HOSPNUM
           MOVE  RECE02-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE  RECE02-PTID         TO  SYU-PTID
           MOVE  SYUNOU-REC          TO  MCPDATA-REC
           MOVE  "tbl_syunou"        TO  MCP-TABLE
           MOVE  "key2"              TO  MCP-PATHNAME
           MOVE  "DBSELECT"          TO  MCP-FUNC
           CALL  "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"    TO  MCP-TABLE
               MOVE  "key2"          TO  MCP-PATHNAME
             PERFORM  900-SYUNOU-READ-SEC
      *
             INITIALIZE  WRK-KOHFTN
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  LNK-PRTKANRI-SRYYM  =  SYU-SRYYMD(1:6)
                 IF  SYU-KOH1HKNNUM  =  WRK-KOHNUM
                   COMPUTE  WRK-KOHFTN = WRK-KOHFTN +
                       SYU-SYUCOMPTOTAL - SYU-KOH1COMPFTN
                 END-IF  
                 IF  SYU-KOH2HKNNUM  =  WRK-KOHNUM
                   COMPUTE  WRK-KOHFTN = WRK-KOHFTN +
                       SYU-KOH1COMPFTN - SYU-KOH2COMPFTN
                 END-IF
                 IF  SYU-KOH3HKNNUM  =  WRK-KOHNUM
                   COMPUTE  WRK-KOHFTN = WRK-KOHFTN +
                       SYU-KOH2COMPFTN - SYU-KOH3COMPFTN
                 END-IF
                 IF  SYU-KOH4HKNNUM  =  WRK-KOHNUM
                   COMPUTE  WRK-KOHFTN = WRK-KOHFTN +
                       SYU-KOH3COMPFTN - SYU-KOH4COMPFTN
                 END-IF
               END-IF
      *
               MOVE  "tbl_syunou"    TO  MCP-TABLE
               MOVE  "key2"          TO  MCP-PATHNAME
               PERFORM   900-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR" UPON CONSOLE
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "tbl_syunou"          TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           .
       900-KOHFTN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"USING
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
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *####     05/06/14  公費個別発行機能の追加
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  BTPARA-REC
               MOVE    ZERO            TO  FLG-BTPARA
           ELSE
               MOVE    1               TO  FLG-BTPARA
               INITIALIZE                  BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
