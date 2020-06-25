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
       PROGRAM-ID.             ORCR0700.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : 摘要欄並び替え
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/10    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.06  初再診のコメント編集対応
      *  01.00.02    NACL-門脇    03.11.26  自費レセプトの自費分
      *                                     摘要欄記載対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.01.02    NACL-門脇    05/01/06  自費レセプトの出力条件修正
      *  01.01.03    NACL-門脇    05/02/28  （サ）記載対応
      *
      *  02.06.01    NACL-門脇    05/05/26  手術・麻酔の記載順変更対応
      *  02.06.02    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.03    NACL-門脇    05/08/31  剤番号編集修正
      *  02.06.04    NACL-門脇    05/11/01  （検選）等コメント対応
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  03.05.01    NACL-小豆沢  07/06/05  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.03.01    NACL-門脇    08/07/08  診区１４のコメントのみの剤
      *                                     編集対応
      *  04.03.02    NACL-門脇    08/07/09  投薬の記載順変更（日付順）
      *  04.03.03    NACL-門脇    08/07/14  投薬剤点０（院外等）記載対応
      *  04.04.01    NACL-門脇    10/03/29  改正対応
      *                                     （画像診断２回目以降減算編集）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    10/01/12  治験対応
      *  04.05.03    NACL-門脇    11/06/20  公費分離区分区切り編集修正
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-門脇    12/10/15  診療行為入力したコメントのみの剤の
      *                                     レセプト記載対応
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    16/08/23  持参薬対応
      *
      *  05.00.01    NACL-藤原    18/06/11  ファイルのエラー処理修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE66-FILE     ASSIGN  RECE66PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE66-KEY
                                   FILE    STATUS  IS  STS-RECE66.
           SELECT  RECE661-FILE    ASSIGN  RECE661PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE661-KEY
                                   FILE    STATUS  IS  STS-RECE661.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE66-FILE.
       01  RECE66-REC. 
           COPY    "CPRCF066.INC".
      *
       FD  RECE661-FILE.
       01  RECE661-REC. 
           COPY    "CPRCF0661.INC".
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE66PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE661PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE66                              PIC X(02).
           03  STS-RECE661                             PIC X(02).
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
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
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
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE660"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE661"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE66PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE661PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0700"      TO  JOB-PGID
               MOVE    "摘要欄編集"    TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE661-FILE
           CLOSE                       RECE661-FILE
      *
           OPEN    INPUT               RECE66-FILE
           IF      STS-RECE66          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE66PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE66          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE661-FILE
      *
           PERFORM    RECE66-READ-SEC
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
           PERFORM     RECE66-READ-SEC
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
           INITIALIZE                           RECE661-REC
           MOVE    RECE66-KEY1          TO      RECE661-KEY1
           MOVE    RECE66-KOHBUNRI      TO      RECE661-KOHBUNRI
           MOVE    RECE66-ZAINUM        TO      RECE661-ZAINUM
           IF      RECE66-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE661-ZAINUM
           END-IF
           IF      RECE66-SRYKBN        =       "60" OR "64"
              IF   RECE66-KNSJISKBN     =       2
                   MOVE    5            TO      RECE661-KNSJISKBN
              ELSE
                  IF   (RECE66-CDKBN-KBNNUM >=        0)   AND
                       (RECE66-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE661-KNSJISKBN
                  END-IF
                  IF   (RECE66-CDKBN-KBNNUM >=      400)   AND
                       (RECE66-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE661-KNSJISKBN
                  END-IF
                  IF   (RECE66-CDKBN-KBNNUM >=      200)   AND
                       (RECE66-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE661-KNSJISKBN
                  END-IF
                  IF   RECE66-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE661-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE66-SAFLG   NOT =  SPACE)
              OR   (RECE66-ZAIKBN  NOT =  ZERO )
                   MOVE    9            TO      RECE661-KNSJISKBN
              END-IF
           END-IF     
      ***     MOVE    RECE66-KNSJISKBN     TO      RECE661-KNSJISKBN
           MOVE    RECE66-LINENUM       TO      RECE661-LINENUM
           MOVE    RECE66-ZAIMEISAISU   TO      RECE661-ZAIMEISAISU
           MOVE    RECE66-PTNUM         TO      RECE661-PTNUM
           MOVE    RECE66-SRYKA         TO      RECE661-SRYKA
           IF     (RECE66-SRYKBN          =  "99" )
           AND    (RECE66-SRYSYUKBN   NOT =  "991")
               MOVE  SPACE              TO      RECE661-SRYKBN
           ELSE
               MOVE  RECE66-SRYKBN      TO      RECE661-SRYKBN
           END-IF
           MOVE  RECE66-ZAISKBKBN       TO      RECE661-ZAISKBKBN
           MOVE  RECE66-ZAISKBBAN       TO      RECE661-ZAISKBBAN
           MOVE  RECE66-TENTEKIKBN      TO      RECE661-TENTEKIKBN
           MOVE  RECE66-TENTEKIBAN      TO      RECE661-TENTEKIBAN
           MOVE    RECE66-ASTER         TO      RECE661-ASTER
           MOVE    RECE66-TEKIYO        TO      RECE661-TEKIYO
           IF      RECE66-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE66-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE66-ZAITEN   TO  RECE661-ZAITEN
               END-IF
               MOVE   RECE66-CDKBN-SYO     TO  RECE661-CDKBN-SYO
               MOVE   RECE66-CDKBN-BU      TO  RECE661-CDKBN-BU
               MOVE   RECE66-CDKBN-KBN     TO  RECE661-CDKBN-KBN
               MOVE   RECE66-CDKBN-KBNNUM  TO  RECE661-CDKBN-KBNNUM
               MOVE   RECE66-CDKBN-KBNNUM-EDA
                                           TO  RECE661-CDKBN-KBNNUM-EDA
               MOVE   RECE66-CDKBN-KOUBAN  TO  RECE661-CDKBN-KOUBAN
               MOVE   RECE66-KOUHYOJUNNUM  TO  RECE661-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE66-SRYSYUKBN   TO   RECE661-SRYSYUKBN
           MOVE   RECE66-DAY         TO   RECE661-DAY
      *
           MOVE   RECE66-ZAIKBN      TO   RECE661-ZAIKBN
      *
           MOVE   RECE66-DAY2        TO   RECE661-DAY2
      *
           MOVE   RECE66-HKNCOMBI    TO   RECE661-HKNCOMBI
      *
           MOVE   RECE66-JIHOKBN     TO   RECE661-JIHOKBN
      *
           IF     RECE66-RECESYUBETU  =  8888
              IF     RECE66-ZAITEN  NOT =  ZERO
                  PERFORM     RECE661-WRITE-SEC
              ELSE
                IF   RECE66-SRYKBN  NOT =  "95" AND "96"
                     PERFORM  RECE661-WRITE-SEC
                END-IF
              END-IF
           ELSE
              IF     RECE66-SRYKBN  NOT =  "95" AND "96"
                  PERFORM     RECE661-WRITE-SEC
              END-IF
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００６１）
      *****************************************************************
       RECE661-WRITE-SEC                SECTION.
      *
           WRITE   RECE661-REC
      *
           IF      STS-RECE661         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE661PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE661         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
           .
       RECE661-WRITE-EXT.
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
                   PERFORM   900-CALL-ORCSJOB-SEC
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
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0700 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
                       PERFORM   900-CALL-ORCSJOB-SEC
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
           CLOSE   RECE66-FILE
                   RECE661-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE66-IN   :" CNT-IN
           DISPLAY "RECE661-OUT :" CNT-OUT
           DISPLAY "*** ORCR0700 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE66-READ-SEC            SECTION.
      *
           READ    RECE66-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE66  PTNUM=" RECE66-PTNUM
      *
           END-READ
           .
       RECE66-READ-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
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
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
