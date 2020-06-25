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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0940.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      摘要欄並び替え（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  中止処理追加
      *  01.00.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.00.02    NACL-門脇    05/05/26  手術・麻酔の記載順変更対応
      *  01.00.03    NACL-門脇    05/08/31  剤番号編集修正
      *  01.00.04    NACL-門脇    05.09.06  自賠責従来様式対応
      *  01.00.05    NACL-門脇    05/11/01  （検選）等コメント対応
      *  03.01.01    NACL-門脇    06/07/14  診療区分置き換えフラグの編集
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.03.01    NACL-門脇    08/07/09  投薬の記載順変更（日付順）
      *  04.04.01    NACL-門脇    10/03/29  改正対応
      *                                     （画像診断２回目以降減算編集）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    10/01/25  治験対応
      *
      *  04.07.01    NACL-門脇    13/12/03  自賠責（労災準拠）のレセプト様式
      *                                     追加対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    16/08/23  持参薬対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE86-FILE     ASSIGN  RECE86PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE86-KEY
                                   FILE    STATUS  IS  STS-RECE86.
           SELECT  RECE87-FILE    ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE87-KEY
                                   FILE    STATUS  IS  STS-RECE87.
           SELECT  RECE87X-FILE   ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE87X-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE86-FILE.
       01  RECE86-REC. 
           COPY    "CPRCF086.INC".
      *
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF087.INC".
      *
       FD  RECE87X-FILE.
       01  RECE87X-REC. 
           COPY    "CPRCF087.INC"  REPLACING   //RECE87//
                                   BY          //RECE87X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE86//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE87//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE86                              PIC X(02).
           03  STS-RECE87                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC  X(01).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *     
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
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
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       RECEERR
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE86PARA
           INITIALIZE                      RECE86PARA
           MOVE    "RECE860"           TO  RECE86PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE86PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE86PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE87PARA
           INITIALIZE                      RECE87PARA
           MOVE    "RECE870"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE86PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE86PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE87PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0940"      TO  JOB-PGID
               MOVE    "摘要欄並び替え（労災・自賠責）"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           OPEN    OUTPUT              RECE87X-FILE
           CLOSE                       RECE87X-FILE
      *
           OPEN    INPUT               RECE86-FILE
           OPEN    I-O                 RECE87-FILE
      *
           IF      FLG-END             =   ZERO
               PERFORM    900-RECE86-READ-SEC
           END-IF
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     900-RECE86-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           RECE87-REC
           MOVE    RECE86-HOSPNUM       TO      RECE87-HOSPNUM
           MOVE    RECE86-SRYYM         TO      RECE87-SRYYM
           MOVE    RECE86-NYUGAIKBN     TO      RECE87-NYUGAIKBN
           MOVE    RECE86-PTID          TO      RECE87-PTID
           MOVE    RECE86-RECEKA        TO      RECE87-RECEKA
           MOVE    RECE86-HKNID         TO      RECE87-HKNID
           MOVE    RECE86-TEKSTYMD      TO      RECE87-TEKSTYMD
           MOVE    RECE86-ZAINUM        TO      RECE87-ZAINUM
           IF      RECE86-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE87-ZAINUM
           END-IF
           IF      RECE86-SRYKBN        =       "60" OR "64"
              IF   RECE86-KNSJISKBN     =       2
                   MOVE    5            TO      RECE87-KNSJISKBN
              ELSE
                  IF   (RECE86-CDKBN-KBNNUM >=        0)   AND
                       (RECE86-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE87-KNSJISKBN
                  END-IF
                  IF   (RECE86-CDKBN-KBNNUM >=      400)   AND
                       (RECE86-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE87-KNSJISKBN
                  END-IF
                  IF   (RECE86-CDKBN-KBNNUM >=      200)   AND
                       (RECE86-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE87-KNSJISKBN
                  END-IF
                  IF   RECE86-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE87-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE86-SAFLG   NOT =  SPACE)
              OR   (RECE86-ZAIKBN  NOT =  ZERO )
                   MOVE    9            TO      RECE87-KNSJISKBN
              END-IF
           END-IF     
      ***  MOVE    RECE86-KNSJISKBN     TO      RECE87-KNSJISKBN
           MOVE    RECE86-LINENUM       TO      RECE87-LINENUM
           MOVE    RECE86-ZAIMEISAISU   TO      RECE87-ZAIMEISAISU
           MOVE    RECE86-PTNUM         TO      RECE87-PTNUM
           MOVE    RECE86-SRYKA         TO      RECE87-SRYKA
           IF     (RECE86-SRYKBN          =  "99" )
           AND    (RECE86-SRYSYUKBN   NOT =  "991")
             IF     (WRK-PARA-RECEKBN       =  "4")
             AND    (WRK-PARA-JIBAIPRTKBN   =  "2")
               MOVE  "01"               TO      RECE87-SRYKBN
             ELSE
               MOVE  SPACE              TO      RECE87-SRYKBN
             END-IF
           ELSE
               MOVE  RECE86-SRYKBN      TO      RECE87-SRYKBN
           END-IF
           MOVE    RECE86-ZAISKBKBN     TO      RECE87-ZAISKBKBN
           MOVE    RECE86-ZAISKBBAN     TO      RECE87-ZAISKBBAN
           MOVE    RECE86-TENTEKIKBN    TO      RECE87-TENTEKIKBN
           MOVE    RECE86-TENTEKIBAN    TO      RECE87-TENTEKIBAN
           MOVE    RECE86-ASTER         TO      RECE87-ASTER 
           MOVE    RECE86-TEKIYO        TO      RECE87-TEKIYO
           IF      RECE86-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE86-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE86-ZAITEN   TO  RECE87-ZAITEN
               END-IF
               MOVE   RECE86-CDKBN-SYO     TO  RECE87-CDKBN-SYO
               MOVE   RECE86-CDKBN-BU      TO  RECE87-CDKBN-BU
               MOVE   RECE86-CDKBN-KBN     TO  RECE87-CDKBN-KBN
               MOVE   RECE86-CDKBN-KBNNUM  TO  RECE87-CDKBN-KBNNUM
               MOVE   RECE86-CDKBN-KBNNUM-EDA
                                           TO  RECE87-CDKBN-KBNNUM-EDA
               MOVE   RECE86-CDKBN-KOUBAN  TO  RECE87-CDKBN-KOUBAN
               MOVE   RECE86-KOUHYOJUNNUM  TO  RECE87-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE86-SRYSYUKBN   TO   RECE87-SRYSYUKBN
           MOVE   RECE86-DAY         TO   RECE87-DAY
      *
           MOVE   RECE86-OKIKAEFLG   TO   RECE87-OKIKAEFLG
      *
           MOVE   RECE86-ZAIKBN      TO   RECE87-ZAIKBN
      *
           MOVE   RECE86-DAY2        TO   RECE87-DAY2
      *
           MOVE   RECE86-JIHOKBN     TO   RECE87-JIHOKBN
      *
           MOVE   RECE86-TEKIYO2-INF TO   RECE87-TEKIYO2-INF
      *
           IF     RECE86-SRYKBN  NOT =  "95" AND "96"
               PERFORM     2101-RECE87-WRITE-SEC
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００６１）
      *****************************************************************
       2101-RECE87-WRITE-SEC                SECTION.
      *
           WRITE   RECE87-REC
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       2101-RECE87-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE86-FILE
                   RECE87-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE86-IN  :" CNT-IN   
           DISPLAY "RECE87-OUT :" CNT-OUT   
           DISPLAY "*** ORCR0940 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE86-READ-SEC            SECTION.
      *
           READ    RECE86-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE86  PTNUM=" RECE86-PTNUM
      *
           END-READ
           .
       900-RECE86-READ-EXT.
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
      *    ＤＢ　オープン処理
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
      *    ＤＢ　クローズ処理
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
