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
       PROGRAM-ID.             ORCBG019.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : （月遅れ分）レセ電データ未作成患者一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/05/29    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/04/10  特別療養費は対象外とする
      *
      *  04.05.01    NACL-藤原    11/04/28  災害対応
      *                                     システム管理の設定に従い、災
      *                                     害該当レセプトのレセ電データ
      *                                     の作成のチェックを追加
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCPOM.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-RECEDEN         PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-OUT             PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SRYYM      PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-ERRMSG          PIC X(300).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    印刷内容
       01  HEAD-01.
           03  FILLER                  PIC X(05)   VALUE   SPACE.
           03  H01-SRYYM               PIC X(16).
           03  FILLER                  PIC X(42)   VALUE
               "分（月遅れ分）レセ電データ未作成患者一覧".
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H01-PAGE                PIC Z9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(22)   VALUE   "患者番号".
           03  FILLER                  PIC X(20)   VALUE   "氏名".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(16)   VALUE   "診療年月".
           03  FILLER                  PIC X(08)   VALUE   "提出先".
           03  FILLER                  PIC X(08)   VALUE   "入外区分".
      *
       01  BODY-01.
           03  B01-FIL1                PIC X(02).
           03  B01-PTNUM               PIC X(20).
           03  B01-NAME                PIC X(20).
           03  B01-FIL2                PIC X(04).
           03  B01-SRYYM               PIC X(18).
           03  B01-TEISYUTUSAKI        PIC X(08).
           03  B01-NYUGAIKBN           PIC X(06).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBG019"       TO  JOB-PGID
           MOVE    "レセ電データ未作成患者一覧"
                                    TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
      *
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3(1:16)
                                   TO  H01-SRYYM
      *
      *    作成日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  H01-SYSYMD
      *
      *    レセ電処理区分取得
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                  SYS-2005-REC
           END-IF
           IF      SYS-2005-SYARECEKBN =   SPACE
               MOVE    "0"                 TO  SYS-2005-SYARECEKBN
           END-IF 
           IF      SYS-2005-KKHRECEKBN =   SPACE
               MOVE    "0"                 TO  SYS-2005-KKHRECEKBN
           END-IF 
      *
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-200501-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200501-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-200501-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE   MCPDATA-REC          TO  SYS-200501-REC
           ELSE
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-KOUIKIRECEKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-200501-KOUIKIRECEKBN
           END-IF 
           IF     SYS-200501-RECEDEN-DISASTER
                                       =   SPACE
               MOVE    "1"             TO  SYS-200501-RECEDEN-DISASTER
           END-IF
      *
           DISPLAY "RECEDEN-DISASTER=" SYS-200501-RECEDEN-DISASTER
      *    
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    69                  TO  CNT-LINE
           MOVE    ZERO                TO  CNT-PAGE
      *
      *    社保
           IF      SYS-2005-SYARECEKBN =   "0"
               CONTINUE
           ELSE   
               MOVE    SPACE               TO  RECE10-REC
               INITIALIZE                      RECE10-REC
               MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SKYYM
               MOVE    1                   TO  RECE10-TEISYUTUSAKI
               EVALUATE    SYS-2005-SYARECEKBN
                   WHEN    "1"
                       MOVE    "2"         TO  RECE10-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "1"         TO  RECE10-NYUGAIKBN
                   WHEN    "3"
                       MOVE    "%"         TO  RECE10-NYUGAIKBN
               END-EVALUATE
               PERFORM 2001-RECEDEN-CHK-SEC
           END-IF    
      *
      *    国保
           IF      SYS-2005-KKHRECEKBN =   "0"
               CONTINUE
           ELSE   
               MOVE    SPACE               TO  RECE10-REC
               INITIALIZE                      RECE10-REC
               MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SKYYM
               MOVE    2                   TO  RECE10-TEISYUTUSAKI
               EVALUATE    SYS-2005-KKHRECEKBN
                   WHEN    "1"
                       MOVE    "2"         TO  RECE10-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "1"         TO  RECE10-NYUGAIKBN
                   WHEN    "3"
                       MOVE    "%"         TO  RECE10-NYUGAIKBN
               END-EVALUATE
               PERFORM 2001-RECEDEN-CHK-SEC
           END-IF    
      *
      *    広域
           IF      SYS-200501-KOUIKIRECEKBN  =   "0"
               CONTINUE
           ELSE   
               MOVE    SPACE               TO  RECE10-REC
               INITIALIZE                      RECE10-REC
               MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SKYYM
               MOVE    6                   TO  RECE10-TEISYUTUSAKI
               EVALUATE    SYS-200501-KOUIKIRECEKBN
                   WHEN    "1"
                       MOVE    "2"         TO  RECE10-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "1"         TO  RECE10-NYUGAIKBN
                   WHEN    "3"
                       MOVE    "%"         TO  RECE10-NYUGAIKBN
               END-EVALUATE
               PERFORM 2001-RECEDEN-CHK-SEC
           END-IF    
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           MOVE    1                    TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     レセ電データチェック処理
      *****************************************************************
       2001-RECEDEN-CHK-SEC            SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
      *
           PERFORM             UNTIL   FLG-RECE10      =   1
               INITIALIZE                      RECEDEN-REC
               MOVE    RECE10-KEY          TO  RECEDEN-KEY
               MOVE    "1"                 TO  RECEDEN-RECKBN
               MOVE    RECEDEN-REC         TO  MCPDATA-REC
               PERFORM 900-RECEDEN-READ-SEC
               IF      FLG-RECEDEN         =   1
      *            チェック一覧作成処理
                   PERFORM 20011-PRINT-SEC
               END-IF
      *
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-RECEDEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック一覧作成処理
      *****************************************************************
       20011-PRINT-SEC          SECTION.
      *
           IF      CNT-LINE             >=  69
               PERFORM 200111-HEAD-PRINT-SEC
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SPACE               TO  BODY-01
           MOVE    RECE10-PTNUM        TO  B01-PTNUM
           MOVE    PTINF-NAME          TO  B01-NAME
           INSPECT B01-NAME            REPLACING  ALL "  "  BY  "　"
           MOVE    RECE10-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3(1:16)
                                       TO  B01-SRYYM
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    1
                   MOVE    "社保"          TO  B01-TEISYUTUSAKI
               WHEN    2
                   MOVE    "国保"          TO  B01-TEISYUTUSAKI
               WHEN    6
                   MOVE    "広域"          TO  B01-TEISYUTUSAKI
           END-EVALUATE
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    "入院"          TO  B01-NYUGAIKBN
               WHEN    "2"
                   MOVE    "入院外"        TO  B01-NYUGAIKBN
           END-EVALUATE
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-01         TO  HCPOM-MOJI (CNT-LINE)
      *
           ADD     1               TO  CNT-OUT
           .
       20011-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       200111-HEAD-PRINT-SEC                SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 200111-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCPOM
           MOVE    HEAD-01             TO  HCPOM-MOJI (1)
           MOVE    HEAD-02             TO  HCPOM-MOJI (3)
      *
           MOVE    4                   TO  CNT-LINE
           .
       200111-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       200111-PRINT-OUT-SEC                SECTION.
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
           MOVE   "HCPOM.red"          TO  SPRT-PRTID
           MOVE    HCPOM               TO  SPRT-PRTDATA
           MOVE    "レセ電データ未作成患者一覧"
                                       TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       200111-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBG019 "  DELIMITED  BY  SIZE
                   WRK-RECEERR     DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBG019 OUT  "  CNT-OUT
           DISPLAY "*** ORCBG019 PAGE "  CNT-PAGE
           DISPLAY "*** ORCBG019 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
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
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE10
               MOVE    MCPDATA-REC         TO  RECE10-REC
               IF      RECE10-RECESYUBETU  =   "6666"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
      *        未確定分をレセ電データに記録するか
               IF    (  SYS-200501-RECEDEN-DISASTER
                                           =   "0"              )
               AND   ( RECE10-ETC-YKZFTN (1)
                                           =   6   OR  7  OR  8 )
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *???
               DISPLAY "PTNUM=" RECE10-PTNUM
                       " TEISYUTUSAKI=" RECE10-TEISYUTUSAKI 
                       " SRYYM="        RECE10-SRYYM 
                       " RECESYUBETU="  RECE10-RECESYUBETU 
                       " HENREIYM="     RECE10-HENREIYM
                       " HENREI-SIJI-FLG="  RECE10-HENREI-SIJI-FLG
                       " ETC-YKZFTN1="  RECE10-ETC-YKZFTN (1)
      *???
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC             SECTION.
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEDEN
               ELSE
                   MOVE    1                   TO  FLG-RECEDEN
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECEDEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
