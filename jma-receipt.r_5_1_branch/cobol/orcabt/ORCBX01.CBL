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
       PROGRAM-ID.             ORCBX01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ設定
      *  コンポーネント名  : 一覧表印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/02/27    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.03    NACL-太田    04/05/28  DBOPEN,DBDISCONNECT削除
      *  03.05.00    NACL-太田    07/06/01  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
           SELECT  TENSU-FILE      ASSIGN  SS004-FILENAME
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TNS-KEY
                                   FILE    STATUS  IS  STS-BX0101.
      *
006900*    エラーファイル
           SELECT  CHKMERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-CHKMERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  TENSU-FILE.
           COPY    "CPTENSU.INC".
      *
      *    エラーファイル
       FD  CHKMERR-FILE.
       01  CHKMERR-REC                             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-CHKMERR                         PIC X(02).
           03  STS-BX0101                          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-CHKSND                          PIC 9(01).
           03  FLG-BX0101                          PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-JOB-UPDCNT                      PIC 9(08).
           03  CNT-CHKSND                          PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                            PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-CHKKBN                     PIC X(01).
           03  WRK-PARA-SRTKBN                     PIC X(01).
           03  WRK-PARA-STSRYCD                    PIC X(09).
           03  WRK-PARA-EDSRYCD                    PIC X(09).
           03  WRK-PARA-KJNYMD                     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TITLE                           PIC X(100).
           03  WRK-CHKMERR                         PIC X(200).
           03  WRK-TEKBYO-CHKKBN                   PIC X(1).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
           03  WRK-TEXT                            PIC X(300).
           03  WRK-TEXT-LEN                        PIC 9(05).
           03  WRK-KENSU-Z                         PIC ZZZZZZ.
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA"        REPLACING 
                                       //SPA-//    BY //WSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角変換
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSS004.INC".
      *
      *    一時チェックマスタ編集サブ
           COPY    "CPORCSS013.INC".
      *
           COPY    "CPORCSS013.INC"    REPLACING
                                       //SS013-// BY //TSS013-//.
      *
      *    一覧印刷変換領域
           COPY    "CPORCHXLST.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
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
           INITIALIZE                  TSS013-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  WRK-PARA
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
                                       WRK-PARA-CHKKBN
                                       WRK-PARA-SRTKBN
                                       WRK-PARA-STSRYCD
                                       WRK-PARA-EDSRYCD
                                       RECEERR
                                       WRK-PARA-KJNYMD
           END-UNSTRING
      *
      *    ＳＰＡエリアセット処理
           PERFORM 110-SPA-SET-SEC
      *
           MOVE    SPACE               TO  WRK-TITLE
      *
           EVALUATE    WRK-PARA-CHKKBN
           WHEN    "1"
               MOVE    "薬剤と病名一覧表"
                                       TO  WRK-TITLE
           WHEN    "2"
               MOVE    "診療行為と病名一覧表"
                                       TO  WRK-TITLE
           WHEN    "4"
               MOVE    "薬剤と併用禁忌一覧表"
                                       TO  WRK-TITLE
           WHEN    "5"
               MOVE    "診療行為の併用算定一覧表（同月内）"
                                       TO  WRK-TITLE
           WHEN    "6"
               MOVE    "薬剤と投与禁忌病名一覧表"
                                       TO  WRK-TITLE
           WHEN    "7"
               MOVE    "診療行為どうしの算定漏れ一覧表"
                                       TO  WRK-TITLE
           WHEN    "8"
               MOVE    "病名と薬剤"
                                       TO  WRK-TITLE
           WHEN    "9"
               MOVE    "病名と診療行為"
                                       TO  WRK-TITLE
           WHEN    "A"
               MOVE    "診療行為の併用算定一覧表（同日内）"
                                       TO  WRK-TITLE
           WHEN    "B"
               MOVE    "診療行為の併用算定一覧表（同会計内）"
                                       TO  WRK-TITLE
           WHEN    "Y"
               MOVE    "適応病名マスタ一覧表（薬剤）"
                                       TO  WRK-TITLE
           WHEN    "Z"
               MOVE    "適応病名マスタ一覧表（診療行為）"
                                       TO  WRK-TITLE
           END-EVALUATE
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR TO SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      *    印刷パラメタエリアセット処理
           PERFORM 110-ORCSPRTAREA-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡエリアセット処理
      *****************************************************************
       110-SPA-SET-SEC                 SECTION.
      *
      *    システム日付セット
           INITIALIZE                      SPA-AREA
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPA-SYSYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
           EVALUATE    WRK-PARA-CHKKBN
           WHEN    "Y"
               MOVE    "1"             TO  WRK-TEKBYO-CHKKBN
           WHEN    "Z"
               MOVE    "2"             TO  WRK-TEKBYO-CHKKBN
           WHEN    OTHER
               MOVE    WRK-PARA-CHKKBN TO  WRK-TEKBYO-CHKKBN
           END-EVALUATE
      *
           .
       110-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    印刷パラメタエリアセット処理
      *****************************************************************
       110-ORCSPRTAREA-SET-SEC         SECTION.
      *
      *    印刷パラメタ編集
           INITIALIZE                      ORCSPRTAREA
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
           MOVE    WRK-TITLE           TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           .
       110-ORCSPRTAREA-SET-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE     WRK-PARA-CHKKBN
           WHEN    "1"
           WHEN    "2"
               PERFORM 2002-TEKBYO-START-SEC
           WHEN    "Y"
           WHEN    "Z"
               PERFORM 2001-TEKBYO-START-SEC
           END-EVALUATE
      *
           INITIALIZE                      ORCHXLSTAREA
           MOVE    WRK-PARA-CHKKBN     TO  HXLST-CHKKBN
           MOVE    WRK-PARA-STSRYCD    TO  HXLST-SRYCD
           MOVE    WRK-PARA-EDSRYCD    TO  HXLST-SRYCD-END
           MOVE    WRK-PARA-SRTKBN     TO  HXLST-SRTKBN
           MOVE    WRK-PARA-KJNYMD     TO  HXLST-KJNYMD
           MOVE    "2"                 TO  HXLST-PROCKBN
           MOVE    ORCSPRTAREA         TO  HXLST-PRTAREA
           MOVE    TSS013-TEMP-NAME    TO  HXLST-TEMP-NAME
           MOVE    TSS013-TEMP-KEY     TO  HXLST-TEMP-KEY
      *
           CALL    "ORCHXLST"          USING
                                       SPA-AREA
                                       ORCHXLSTAREA
      *
           MOVE    HXLST-OUT-CNT       TO  CNT-JOB-UPDCNT
      *
           MOVE    1                   TO  FLG-END
      *
           IF    ( WRK-PARA-CHKKBN     =   "1" OR "2" OR "Y" OR "Z" )
               PERFORM 2001-TEKBYO-END-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    適応病名マスタ一覧表開始処理
      *****************************************************************
       2001-TEKBYO-START-SEC           SECTION.
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "対象データを取得しています"
                                       DELIMITED BY SIZE
           INTO    JOB-YOBI
           END-STRING
      *
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 20011-TEKBYO-START-SEC
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBCREATE"         TO  SS013-FUNC
           MOVE     WRK-TEKBYO-CHKKBN  TO  SS013-CHKKBN
           MOVE     "9"                TO  SS013-CREATEKBN
           MOVE     WRK-PARA-KJNYMD    TO  SS013-KJNYMD
           CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
           MOVE     SS013-AREA         TO  TSS013-AREA
      *
           OPEN    INPUT   TENSU-FILE
      *
           PERFORM 901-TENSU-READ-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
      *
               INITIALIZE                      SS013-AREA
               MOVE     "DBINSERT"         TO  SS013-FUNC
               MOVE     WRK-TEKBYO-CHKKBN  TO  SS013-CHKKBN
               MOVE     "1"                TO  SS013-TEKBYOKBN
               MOVE     TNS-SRYCD          TO  SS013-SRYCD
               MOVE     WRK-PARA-KJNYMD    TO  SS013-KJNYMD
               MOVE     TSS013-TEMP-NAME   TO  SS013-TEMP-NAME
               MOVE     TSS013-TEMP-KEY    TO  SS013-TEMP-KEY
               CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
      *
               PERFORM 901-CHKSND-KEY23-SEL-SEC
               IF    ( FLG-CHKSND          =   ZERO )
      *
                   COMPUTE CNT-CHKSND = CNT-CHKSND + 1
      *
                   DIVIDE  50  INTO    CNT-CHKSND  GIVING  WRK-SHO
                   REMAINDER WRK-AMARI
                   END-DIVIDE
      *
                   IF    ( WRK-AMARI       =   ZERO )
                       MOVE    CNT-CHKSND  TO  WRK-KENSU-Z
                       MOVE    WRK-KENSU-Z TO  WRK-TEXT
                       PERFORM 800-ZENKAKU-SEC
      *
                       MOVE    "STM"       TO  SJOBKANRI-MODE
                       INITIALIZE              JOBKANRI-REC
                       STRING  "適応病名マスタを参照しています"
                                               DELIMITED BY SIZE
                               WRK-TEXT        DELIMITED BY SPACE
                               "件"            DELIMITED BY SIZE
                       INTO    JOB-YOBI
                       END-STRING
      *
                       PERFORM 800-ORCSJOB-SEC
      *
                   END-IF
      *
                   MOVE    "0"             TO  CHKSND-CDKBN
                   MOVE    CHKSND-REC      TO  MCPDATA-REC
      *
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "tmpins2"       TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
               END-IF
      *
               PERFORM 901-TENSU-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   TENSU-FILE
      *
           MOVE    "STM"       TO  SJOBKANRI-MODE
           INITIALIZE              JOBKANRI-REC
           STRING  "印刷データを作成しています"
                                   DELIMITED BY SIZE
           INTO    JOB-YOBI
           END-STRING
      *
           PERFORM 800-ORCSJOB-SEC
      *
           .
       2001-TEKBYO-START-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ一覧表開始処理
      *****************************************************************
       2002-TEKBYO-START-SEC           SECTION.
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBCREATE"         TO  SS013-FUNC
           MOVE     WRK-TEKBYO-CHKKBN  TO  SS013-CHKKBN
           MOVE     "2"                TO  SS013-CREATEKBN
           MOVE     WRK-PARA-KJNYMD    TO  SS013-KJNYMD
           CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
           MOVE     SS013-AREA         TO  TSS013-AREA
      *
           PERFORM 900-CHKSND-KEY23-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               INITIALIZE                      SS013-AREA
               MOVE     "DBINSERT"         TO  SS013-FUNC
               MOVE     WRK-TEKBYO-CHKKBN  TO  SS013-CHKKBN
               MOVE     CHKSND-SRYCD       TO  SS013-SRYCD
               MOVE     WRK-PARA-KJNYMD    TO  SS013-KJNYMD
               MOVE     TSS013-TEMP-NAME   TO  SS013-TEMP-NAME
               MOVE     TSS013-TEMP-KEY    TO  SS013-TEMP-KEY
               CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
      *
               PERFORM 900-CHKSND-KEY23-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key23"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002-TEKBYO-START-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ一覧表開始処理
      *****************************************************************
       20011-TEKBYO-START-SEC          SECTION.
      *
           INITIALIZE                  SS004-AREA
           MOVE    WRK-TEKBYO-CHKKBN   TO  SS004-SRYCDKBN
           MOVE    6                   TO  SS004-KIKAN
           MOVE    SPA-AREA            TO  WSPA-AREA
           MOVE    WRK-PARA-KJNYMD     TO  WSPA-SYSYMD
           CALL    "ORCSS004"          USING
                                       SS004-AREA
                                       WSPA-AREA
      *
           OPEN    I-O                 TENSU-FILE
      *
           PERFORM 900-CHKSND-KEY23-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               PERFORM 900-TENSU-START-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       =   ZERO )
                       WRITE    TNS-TENSU-REC
                   END-IF
               END-IF
      *
               PERFORM 900-CHKSND-KEY23-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key23"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   TENSU-FILE
      *
           .
       20011-TEKBYO-START-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ一覧表終了処理
      *****************************************************************
       2001-TEKBYO-END-SEC             SECTION.
      *
           MOVE    "STM"       TO  SJOBKANRI-MODE
           INITIALIZE              JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           .
       2001-TEKBYO-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT           CHKMERR-FILE
           IF        ( STS-CHKMERR =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT      CHKMERR-FILE
      *
               MOVE    WRK-CHKMERR     TO  CHKMERR-REC
               WRITE   CHKMERR-REC
               CLOSE   CHKMERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CHKMERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF    ( CNT-JOB-UPDCNT  =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-CHKMERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-JOB-UPDCNT  TO  JOB-UPDCNT
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-TEXT-LEN
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE 300                TO  KANACONV-LEN
           MOVE 1000               TO  KANACONV-STLEN
           MOVE 1                  TO  KANACONV-CONV-FLAG
           MOVE 15                 TO  KANACONV-CHAR-TYPE
           MOVE WRK-TEXT           TO  KANACONV-INDATA
           CALL "kanaconv"             USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-TEXT
               MOVE    KANACONV-MAXLEN TO  WRK-TEXT-LEN
           END-IF
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理編集処理
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBX01"           TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    CHKSND-SRYCD        TO  TNS-SRYCD
           MOVE    WRK-PARA-KJNYMD     TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY23-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-TEKBYO-CHKKBN   TO  CHKSND-CHKKBN
           MOVE    "%"                 TO  CHKSND-SRYCD
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    WRK-PARA-KJNYMD     TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY23-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       901-CHKSND-KEY23-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-TEKBYO-CHKKBN   TO  CHKSND-CHKKBN
           MOVE    TNS-SRYCD           TO  CHKSND-SRYCD
           MOVE    WRK-PARA-KJNYMD     TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    "1"                 TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-CHKSND-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数ファイルSTART処理
      *****************************************************************
       900-TENSU-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    CHKSND-SRYCD        TO  TNS-SRYCD
           MOVE    LOW-VALUE           TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
           START   TENSU-FILE
               KEY IS  >=   TNS-KEY
           INVALID
               MOVE    1               TO  FLG-TENSU
           NOT INVALID
               PERFORM 900-TENSU-READ-SEC
           END-START
           
      *
           .
       900-TENSU-START-EXT.
           EXIT.
      *****************************************************************
      *    点数ファイル読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               READ    TENSU-FILE          NEXT
               AT  END
                   MOVE    1           TO  FLG-TENSU
               NOT AT  END
      *
                   IF    ( TNS-HOSPNUM     =   CHKSND-HOSPNUM )
                    AND  ( TNS-SRYCD       =   CHKSND-SRYCD )
                       IF    ( TNS-YUKOSTYMD  <=   WRK-PARA-KJNYMD )
                         AND ( TNS-YUKOEDYMD  >=   WRK-PARA-KJNYMD )
                           MOVE    1   TO  FLG-LOOPEND
                       END-IF
                   ELSE
                           MOVE    1   TO  FLG-TENSU
                   END-IF
               END-READ
      *
           END-PERFORM
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数ファイル読込処理
      *****************************************************************
       901-TENSU-READ-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           READ    TENSU-FILE          NEXT
           AT  END
               MOVE    1               TO  FLG-TENSU
           END-READ
      *
           .
       901-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
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
       900-DBDISCONNECT-SEC            SECTION.
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
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
