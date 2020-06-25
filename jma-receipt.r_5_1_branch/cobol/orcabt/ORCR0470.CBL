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
       PROGRAM-ID.             ORCR0470.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      摘要欄並び替え（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.09.20  検査のときは表用番号を編集しない
      *                                     検査のときZAITENを編集する
      *  01.00.02    NACL-門脇    02.11.26  検診コードの診療区分編集
      *  01.00.03    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.04    NACL-門脇    03.03.06  初再診のコメント編集対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    05/05/26  手術・麻酔の記載順変更対応
      *  01.01.03    NACL-門脇    05/08/31  剤番号編集修正
      *  01.01.04    NACL-門脇    05.09.06  自賠責従来様式対応
      *  01.01.05    NACL-門脇    05/11/01  （検選）等コメント対応
      *  03.01.01    NACL-門脇    06/07/14  診療区分置き換えフラグの編集
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *  04.02.02    NACL-門脇    08/04/21  ８０その他記載順対応
      *
      *  04.03.01    NACL-門脇    08/07/09  投薬の記載順変更（日付順）
      *  04.04.01    NACL-門脇    10/03/29  改正対応
      *                                     （画像診断２回目以降減算編集）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    10/01/21  治験対応
      *
      *  04.07.01    NACL-門脇    13/12/03  自賠責（労災準拠）のレセプト様式
      *                                     追加対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
           SELECT  RECE47-FILE    ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE47-KEY
                                   FILE    STATUS  IS  STS-RECE47.
           SELECT  RECE47X-FILE   ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE47X-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE46-FILE.
       01  RECE46-REC. 
           COPY    "CPRCF046.INC".
      *
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF047.INC".
      *
       FD  RECE47X-FILE.
       01  RECE47X-REC. 
           COPY    "CPRCF047.INC"  REPLACING   //RECE47//
                                   BY          //RECE47X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE46//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE47//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE46                              PIC X(02).
           03  STS-RECE47                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SAISHIN                             PIC 9(01).
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
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE46PARA
           INITIALIZE                      RECE46PARA
           MOVE    "RECE460"           TO  RECE46PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE46PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE46PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE47PARA
           INITIALIZE                      RECE47PARA
           MOVE    "RECE470"           TO  RECE47PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE47PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE47PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE46PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE47PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE46PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE47PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0470"      TO  JOB-PGID
               MOVE    "摘要欄並び替え（労災・自賠責）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE47X-FILE
           CLOSE                       RECE47X-FILE
      *
           OPEN    INPUT               RECE46-FILE
           OPEN    I-O                 RECE47-FILE
      *
           PERFORM    900-RECE46-READ-SEC
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
           PERFORM     900-RECE46-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
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
           INITIALIZE                           RECE47-REC
           MOVE    RECE46-HOSPNUM       TO      RECE47-HOSPNUM
           MOVE    RECE46-SRYYM         TO      RECE47-SRYYM
           MOVE    RECE46-NYUGAIKBN     TO      RECE47-NYUGAIKBN
           MOVE    RECE46-PTID          TO      RECE47-PTID
           MOVE    RECE46-RECEKA        TO      RECE47-RECEKA
           MOVE    RECE46-HKNID         TO      RECE47-HKNID
           MOVE    RECE46-TEKSTYMD      TO      RECE47-TEKSTYMD
           MOVE    RECE46-ZAINUM        TO      RECE47-ZAINUM
           IF      RECE46-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE47-ZAINUM
           END-IF
           IF      RECE46-SRYKBN        =       "60" OR "64"
              IF   RECE46-KNSJISKBN     =       2
                   MOVE    5            TO      RECE47-KNSJISKBN
              ELSE
                  IF   (RECE46-CDKBN-KBNNUM >=        0)   AND
                       (RECE46-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE47-KNSJISKBN
                  END-IF
                  IF   (RECE46-CDKBN-KBNNUM >=      400)   AND
                       (RECE46-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE47-KNSJISKBN
                  END-IF
                  IF   (RECE46-CDKBN-KBNNUM >=      200)   AND
                       (RECE46-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE47-KNSJISKBN
                  END-IF
                  IF   RECE46-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE47-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE46-SAFLG   NOT =   SPACE )
              OR   (RECE46-ZAIKBN      =   1 OR 2)
                   MOVE    9            TO      RECE47-KNSJISKBN
              END-IF
           END-IF     
      ***  MOVE    RECE46-KNSJISKBN     TO      RECE47-KNSJISKBN
           MOVE    RECE46-LINENUM       TO      RECE47-LINENUM
           MOVE    RECE46-ZAIMEISAISU   TO      RECE47-ZAIMEISAISU
           MOVE    RECE46-PTNUM         TO      RECE47-PTNUM
           MOVE    RECE46-SRYKA         TO      RECE47-SRYKA
           IF     (RECE46-SRYKBN          =  "99" )
           AND    (RECE46-SRYSYUKBN   NOT =  "991")
             IF     (WRK-PARA-RECEKBN       =  "4")
             AND    (WRK-PARA-JIBAIPRTKBN   =  "2")
               MOVE  "01"               TO      RECE47-SRYKBN
             ELSE
               MOVE  SPACE              TO      RECE47-SRYKBN
             END-IF
           ELSE
               MOVE  RECE46-SRYKBN      TO      RECE47-SRYKBN
           END-IF
           MOVE    RECE46-ASTER         TO      RECE47-ASTER 
           MOVE    RECE46-TEKIYO        TO      RECE47-TEKIYO
           IF      RECE46-SRYKBN        =       "12"
               IF      RECE46-ASTER         =       "＊"
                   IF      RECE46-TEKIYO(1:24)  =
                                        "再診（診療所）（１回目）"
                       MOVE   1             TO      FLG-SAISHIN
                   END-IF
               END-IF
               IF      RECE46-TEKIYO(49:2)  =   SPACE
                   CONTINUE
               ELSE     
                  IF      FLG-SAISHIN          =      1
                      MOVE  ZERO               TO   FLG-SAISHIN
                  END-IF
               END-IF
           END-IF     
           IF      RECE46-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE46-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE46-ZAITEN   TO  RECE47-ZAITEN
               END-IF
               MOVE   RECE46-CDKBN-SYO     TO  RECE47-CDKBN-SYO
               MOVE   RECE46-CDKBN-BU      TO  RECE47-CDKBN-BU
               MOVE   RECE46-CDKBN-KBN     TO  RECE47-CDKBN-KBN
               MOVE   RECE46-CDKBN-KBNNUM  TO  RECE47-CDKBN-KBNNUM
               MOVE   RECE46-CDKBN-KBNNUM-EDA
                                           TO  RECE47-CDKBN-KBNNUM-EDA
               MOVE   RECE46-CDKBN-KOUBAN  TO  RECE47-CDKBN-KOUBAN
               MOVE   RECE46-KOUHYOJUNNUM  TO  RECE47-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE46-SRYSYUKBN   TO   RECE47-SRYSYUKBN
           MOVE   RECE46-DAY         TO   RECE47-DAY
      *
           MOVE   RECE46-OKIKAEFLG   TO   RECE47-OKIKAEFLG
      *
           MOVE   RECE46-SYOHOUFLG   TO   RECE47-SYOHOUFLG
      *
           MOVE   RECE46-ZAIKBN      TO   RECE47-ZAIKBN
      *
           MOVE   RECE46-DAY2        TO   RECE47-DAY2
      *
           MOVE   RECE46-JIHOKBN     TO   RECE47-JIHOKBN
      *
           MOVE   RECE46-TEKIYO2-INF TO   RECE47-TEKIYO2-INF
      *
           IF     RECE46-SRYKBN  NOT =  "95" AND "96"
               PERFORM     2101-RECE47-WRITE-SEC
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００６１）
      *****************************************************************
       2101-RECE47-WRITE-SEC                SECTION.
      *
           WRITE   RECE47-REC
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       2101-RECE47-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE46-FILE
                   RECE47-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE46-IN   :" CNT-IN   
           DISPLAY "RECE47-OUT :" CNT-OUT   
           DISPLAY "*** ORCR0470 END ***"
           .
       300-TERM-EXT.
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
      *            ジョブ管理開始処理
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
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE46-READ-SEC            SECTION.
      *
           READ    RECE46-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE46  PTNUM=" RECE46-PTNUM
      *
           END-READ
           .
       900-RECE46-READ-EXT.
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
