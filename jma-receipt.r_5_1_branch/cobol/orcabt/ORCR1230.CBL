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
       PROGRAM-ID.             ORCR1230.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  コンポーネント名  : 公害レセプト（入院外）
      *                      同一剤のまとめ（旧総合病院以外のとき）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    16/08/23  持参薬対応
      *  04.08.04    NACL-門脇    16/11/07  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応修正
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE200-FILE    ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
           SELECT  RECE220-FILE     ASSIGN  RECE220PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE220-KEY
                                   FILE    STATUS  IS  STS-RECE220.
           SELECT  RECE230-FILE     ASSIGN  RECE230PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE230-KEY
                                   FILE    STATUS  IS  STS-RECE230.
           SELECT  RECE221-FILE    ASSIGN  RECE221PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE221-KEY
                                   FILE    STATUS  IS  STS-RECE221.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC. 
           COPY    "CPRCF200.INC".
      *
       FD  RECE220-FILE.
       01  RECE220-REC. 
           COPY    "CPRCF220.INC".
      *
       FD  RECE230-FILE.
       01  RECE230-REC. 
           COPY    "CPRCF230.INC".
      *
       FD  RECE221-FILE.
       01  RECE221-REC. 
           COPY    "CPRCF221.INC".
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    レセプト明細ファイル名称
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE220//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE230//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE221//.
      *
      *    エラーファイル名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名称領域
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE200             PIC X(02).
           03  STS-RECE220             PIC X(02).
           03  STS-RECE230             PIC X(02).
           03  STS-RECE221             PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-RECE200             PIC 9(01).
           03  FLG-RECE220             PIC 9(01).
           03  FLG-RECE230             PIC 9(01).
           03  FLG-RECE221             PIC 9(01).
           03  FLG-NASHI               PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-MATOME              PIC 9(01).
           03  FLG-NG                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-IN-CANCEL           PIC 9(06).
           03  CNT-RECE221             PIC 9(06).
           03  CNT-RECE220             PIC 9(06). 
           03  CNT-RECE220W            PIC 9(06). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
           03  IDZ                     PIC 9(03).
           03  IDZ2                    PIC 9(03).
           03  IDZ3                    PIC 9(03).
           03  IDZ4                    PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-COMRENNUM           PIC 9(05).
      *    剤番号の退避
           03  WRK-ZAINUM              PIC 9(08).
      *    剤番号の退避
           03  WRK-SRYSYUKBN           PIC X(03).
      *    コメント退避
           03  WRK-COM-AREA.
               05  WRK-COM             PIC X(300)    OCCURS  10.
      *    インデックス退避
           03  WRK-INDEX-AREA.
               05  WRK-INDEX           PIC 9(003)    OCCURS  10.
      *
           03  WRK-RECEERR             PIC X(200). 
      *
       01  WRK-RECE221-REC.
           COPY    "CPRCF221.INC"  REPLACING  //RECE221//
                                   BY         //WRK-RECE221//.
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
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE700"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE200PARA
      *
           MOVE    "RECE720"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE220PARA
      *
           MOVE    "RECE730"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE230PARA
      *
           MOVE    "RECE721"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE221PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1230"      TO  JOB-PGID
               MOVE    "同一剤のまとめ"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT               RECE221-FILE
           CLOSE                        RECE221-FILE
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
      *    同一剤のまとめ処理
           OPEN    INPUT               RECE220-FILE
                                       RECE230-FILE
                                       RECE200-FILE
           OPEN    I-O                 RECE221-FILE
           PERFORM 900-RECE200-READ-SEC
      *
           PERFORM 210-RECE221-WRITE-SEC
                                   UNTIL   FLG-RECE200  =   1
                                   OR      FLG-END      =   1
           CLOSE   RECE221-FILE
                   RECE220-FILE
                   RECE230-FILE
                   RECE200-FILE
      *
      *    元のファイルに出力処理
           IF      FLG-END             =   ZERO
               OPEN    INPUT   RECE221-FILE
                       OUTPUT  RECE220-FILE
      *
               PERFORM 900-RECE221-READ-SEC 
               PERFORM 220-RECE220-WRITE-SEC
                                       UNTIL   FLG-RECE221 =   1
                                       OR      FLG-END     =   1
               CLOSE   RECE220-FILE
                       RECE221-FILE
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    同一剤のまとめ処理
      *****************************************************************
       210-RECE221-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE220-REC
           MOVE    RECE200-KEY         TO  RECE220-KEY1
           MOVE    ZERO                TO  RECE220-ZAINUM
      *
           START   RECE220-FILE
               KEY IS    >=   RECE220-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE220
               NOT INVALID
                   PERFORM 900-RECE220-READ-SEC
           END-START
           PERFORM UNTIL  ( FLG-END        =   1            )
                   OR     ( FLG-RECE220    =   1            )
                   OR     ( RECE200-KEY
                                       NOT =   RECE220-KEY1 )
      *
      *        診療種別区分取得・コメント編集
               MOVE    RECE220-ZAINUM      TO  WRK-ZAINUM
               MOVE    SPACE               TO  WRK-SRYSYUKBN
               INITIALIZE                  WRK-COM-AREA
               IF    ( RECE220-SRYKBN      =   "60" OR "64" OR "99" )
               OR    ( RECE220-COMCDFLG    =   "1"                  )
                   PERFORM 2101-RECE230-COM-HENSYU-SEC
               END-IF
      *
               MOVE    SPACE               TO  RECE221-REC
               INITIALIZE                      RECE221-REC
               MOVE    RECE220-KEY1        TO  RECE221-KEY1
               MOVE    RECE220-SRYKBN      TO  RECE221-SRYKBN
               MOVE    WRK-SRYSYUKBN       TO  RECE221-SRYSYUKBN
               MOVE    RECE220-ZAITEN      TO  RECE221-ZAITEN
               MOVE    RECE220-SRYCDTOTAL  TO  RECE221-SRYCDTOTAL
               MOVE    RECE220-SURYOUTOTAL TO  RECE221-SURYOUTOTAL
               MOVE    RECE220-RUIKEITOTAL TO  RECE221-RUIKEITOTAL
               MOVE    RECE220-MEISAISU    TO  RECE221-MEISAISU 
               MOVE    RECE220-JIHOKBN     TO  RECE221-JIHOKBN
               MOVE    RECE220-JISANYAKKBN TO  RECE221-JISANYAKKBN
               IF      RECE220-COMCDFLG    =   "1"
                 ADD     1                 TO  WRK-COMRENNUM
                 MOVE    WRK-COMRENNUM     TO  RECE221-COMRENNUM
      *
                 MOVE    RECE221-REC       TO  WRK-RECE221-REC
                 PERFORM 900-RECE221-INV2-SEC
                 IF      FLG-MATOME    =   ZERO
                     MOVE    WRK-RECE221-REC     TO  RECE221-REC
                     MOVE    RECE220-REC         TO  RECE221-REC1
                     MOVE    WRK-COM-AREA        TO  RECE221-COM-AREA
                 END-IF
      *          剤回数・日数回数テーブルの加算
                 ADD     RECE220-ZAIKAISU  TO  RECE221-ZAIKAISU
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       4
                     PERFORM VARYING IDY FROM    1   BY  1
                             UNTIL   IDY >       31
                         ADD     RECE220-KAISU (IDX IDY)
                                           TO  RECE221-KAISU (IDX IDY)
                     END-PERFORM
                 END-PERFORM
      *
                 IF  RECE220-SYODOU-SRYKA(1)  NOT =  SPACE
                   MOVE     ZERO     TO    FLG-ARI
                   PERFORM  VARYING  IDX   FROM    1   BY  1
                             UNTIL IDX                       > 10
                              OR   RECE221-SYODOU-SRYKA(IDX) = SPACE
                              OR   FLG-ARI                   = 1
                      IF  RECE220-SYODOU-SRYKA(1)
                                 =  RECE221-SYODOU-SRYKA(IDX)
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-PERFORM
                   IF  FLG-ARI  =  ZERO
                   AND IDX     <=  10
                          MOVE   RECE220-SYODOU-SRYKA(1)
                                    TO   RECE221-SYODOU-SRYKA(IDX)
                   END-IF
                 END-IF
      *
                 IF      FLG-MATOME    =   ZERO
                     WRITE   RECE221-REC
                     ADD     1                 TO  CNT-RECE221
                 ELSE
                     REWRITE RECE221-REC
                 END-IF
               ELSE
                 MOVE    RECE221-REC       TO  WRK-RECE221-REC
                 PERFORM 900-RECE221-INV-SEC
                 IF      FLG-RECE221       =   1
                     MOVE    WRK-RECE221-REC   TO  RECE221-REC
                     MOVE    RECE220-REC       TO  RECE221-REC1
                 END-IF
      *          剤回数・日数回数テーブルの加算
                 ADD     RECE220-ZAIKAISU  TO  RECE221-ZAIKAISU
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       4
                     PERFORM VARYING IDY FROM    1   BY  1
                             UNTIL   IDY >       31
                         ADD     RECE220-KAISU (IDX IDY)
                                         TO   RECE221-KAISU (IDX IDY)
                     END-PERFORM
                 END-PERFORM
      *
                 IF  RECE220-SYODOU-SRYKA(1)  NOT =  SPACE
                   MOVE     ZERO     TO    FLG-ARI
                   PERFORM  VARYING  IDX   FROM    1   BY  1
                             UNTIL IDX                       > 10
                              OR   RECE221-SYODOU-SRYKA(IDX) = SPACE
                              OR   FLG-ARI                   = 1
                      IF  RECE220-SYODOU-SRYKA(1)
                                 =  RECE221-SYODOU-SRYKA(IDX)
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-PERFORM
                   IF  FLG-ARI  =  ZERO
                   AND IDX     <=  10
                          MOVE   RECE220-SYODOU-SRYKA(1)
                                    TO   RECE221-SYODOU-SRYKA(IDX)
                   END-IF
                 END-IF
      *
                 IF      FLG-RECE221         =   1
                     WRITE   RECE221-REC
                     ADD     1                   TO  CNT-RECE221
                 ELSE
                     REWRITE RECE221-REC
                 END-IF
               END-IF
      *
               PERFORM     900-RECE220-READ-SEC
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
           AND     FLG-RECE220         =   ZERO
           AND     RECE200-KEY     NOT =   RECE220-KEY1
               COMPUTE   CNT-IN  =  CNT-IN  -  1
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-RECE200-READ-SEC
           END-IF
           .
       210-RECE221-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集・診療種別区分取得
      *****************************************************************
       2101-RECE230-COM-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDZ2
      *
           INITIALIZE                      RECE230-REC
           MOVE    RECE200-KEY         TO  RECE230-KEY1
           MOVE    WRK-ZAINUM          TO  RECE230-ZAINUM
           START   RECE230-FILE
               KEY IS    >=   RECE230-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE230
               NOT INVALID
                   PERFORM  900-RECE230-READ-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE230     =      1          ) OR
                         ( RECE200-KEY NOT =  RECE230-KEY1   ) OR
                         ( WRK-ZAINUM  NOT =  RECE230-ZAINUM )
      *
      *        検査のとき最初の診療区分を取得
               IF    ( RECE220-SRYKBN      =   "60" OR "64" OR "99" )
               AND   ( WRK-SRYSYUKBN       =    SPACE               )
                   MOVE   RECE230-SRYSYUKBN   TO   WRK-SRYSYUKBN
               END-IF
      *        コメント取得
               IF      RECE220-COMCDFLG    =   "1"
                   PERFORM  VARYING IDZ    FROM    1   BY  1
                            UNTIL ( IDZ    >       5 )
                            OR    ( RECE230-SRYCD(IDZ)
                                           =   SPACE )
                       IF    ( RECE230-SRYCD(IDZ)(1:1)     =   "8"   )
                       OR    ( RECE230-SRYCD(IDZ)(1:3)     =   "008" )
                         IF    RECE230-RECEPTFLG(IDZ)   =  "1"
                           CONTINUE
                         ELSE
                           ADD     1               TO  IDZ2
                           IF      IDZ2            <=  10
                               MOVE   RECE230-NAME (IDZ)
                                                   TO  WRK-COM (IDZ2)
                           END-IF
                         END-IF
                       END-IF
                   END-PERFORM
                   PERFORM  900-RECE230-READ-SEC
               ELSE
      ***          IF      WRK-SRYSYUKBN   NOT =   SPACE
                       MOVE    1               TO  FLG-RECE230
      ***          END-IF
               END-IF
           END-PERFORM
      *
           .
       2101-RECE230-COM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    元のファイルに出力処理
      *****************************************************************
       220-RECE220-WRITE-SEC                SECTION.
      *
           MOVE    RECE221-REC1        TO  RECE220-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE221-ZAIKAISU    TO  RECE220-ZAIKAISU
           MOVE    RECE221-DAY-AREA    TO  RECE220-DAY-AREA
      *
           MOVE    RECE221-SYODOU-SRYKA-AREA
                                       TO  RECE220-SYODOU-SRYKA-AREA
      *
           WRITE   RECE220-REC
           ADD     1                   TO  CNT-RECE220W
      *
           PERFORM     900-RECE221-READ-SEC
      *
           .
       220-RECE220-WRITE-EXT.
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE220  CNT-IN  :" CNT-IN
           DISPLAY "RECE221  CNT-OUT :" CNT-RECE221
           DISPLAY "RECE220  CNT-OUT :" CNT-RECE220W
           DISPLAY "*** ORCR01230 END ***"
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
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報ファイル読込処理
      *****************************************************************
       900-RECE200-READ-SEC           SECTION.
      *
           READ    RECE200-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE200
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE200
      *
                   DISPLAY "RECE200  PTNUM=" RECE200-PTNUM
      *
           END-READ
           .
       900-RECE200-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報ファイル読込処理
      *****************************************************************
       900-RECE220-READ-SEC            SECTION.
      *
           READ    RECE220-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE220
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE220
                   ADD     1             TO    CNT-IN
           END-READ
           .
       900-RECE220-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄情報ファイル読込処理
      *****************************************************************
       900-RECE230-READ-SEC            SECTION.
      *
           READ    RECE230-FILE   NEXT
               AT  END
                   MOVE    1      TO    FLG-RECE230
               NOT AT  END
                   MOVE    ZERO   TO    FLG-RECE230
           END-READ
           .
       900-RECE230-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE221-READ-SEC            SECTION.
      *
           READ    RECE221-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE221
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE221
           END-READ
           .
       900-RECE221-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報まとめファイル読込処理
      *****************************************************************
       900-RECE221-INV-SEC            SECTION.
      *
           READ    RECE221-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE221
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE221
           END-READ
           .
       900-RECE221-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE221-INV2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MATOME
      *
           INITIALIZE                      RECE221-REC
           MOVE    RECE220-KEY         TO  RECE221-KEY1
           MOVE    RECE220-SRYKBN      TO  RECE221-SRYKBN
           MOVE    WRK-SRYSYUKBN       TO  RECE221-SRYSYUKBN
           MOVE    RECE220-ZAITEN      TO  RECE221-ZAITEN
           MOVE    RECE220-SRYCDTOTAL  TO  RECE221-SRYCDTOTAL
           MOVE    RECE220-SURYOUTOTAL TO  RECE221-SURYOUTOTAL
           MOVE    RECE220-RUIKEITOTAL TO  RECE221-RUIKEITOTAL
           MOVE    RECE220-MEISAISU    TO  RECE221-MEISAISU
           MOVE    RECE220-JIHOKBN     TO  RECE221-JIHOKBN
           MOVE    RECE220-JISANYAKKBN TO  RECE221-JISANYAKKBN
           START   RECE221-FILE
               KEY IS    >=   RECE221-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE221
               NOT INVALID
                   PERFORM  900-RECE221-READ-SEC
           END-START
           PERFORM   UNTIL
                 ( RECE220-KEY1        NOT =  RECE221-KEY1        ) OR
                 ( RECE220-SRYKBN      NOT =  RECE221-SRYKBN      ) OR
                 ( WRK-SRYSYUKBN       NOT =  RECE221-SRYSYUKBN   ) OR
                 ( RECE220-ZAITEN      NOT =  RECE221-ZAITEN      ) OR
                 ( RECE220-SRYCDTOTAL  NOT =  RECE221-SRYCDTOTAL  ) OR
                 ( RECE220-SURYOUTOTAL NOT =  RECE221-SURYOUTOTAL ) OR
                 ( RECE220-RUIKEITOTAL NOT =  RECE221-RUIKEITOTAL ) OR
                 ( RECE220-MEISAISU    NOT =  RECE221-MEISAISU    ) OR
                 ( RECE220-JIHOKBN     NOT =  RECE221-JIHOKBN     ) OR
                 ( RECE220-JISANYAKKBN NOT =  RECE221-JISANYAKKBN ) OR
                 ( FLG-RECE221             =  1)
      *
                  MOVE     ZERO     TO     FLG-NASHI
                  MOVE     ZERO     TO     IDZ3
                  INITIALIZE               WRK-INDEX-AREA
      *
                  PERFORM  VARYING   IDZ   FROM    1    BY   1
                            UNTIL   (IDZ                >      10)  OR
                                    (WRK-COM(IDZ)       =   SPACE)
      *
                     MOVE     ZERO     TO    FLG-ARI
                     PERFORM VARYING IDZ2  FROM    1    BY   1
                              UNTIL (IDZ2               >      10)  OR
                                    (RECE221-COM(IDZ2)  =   SPACE)
                        IF  RECE221-COM(IDZ2)  =  WRK-COM(IDZ)
                            MOVE   ZERO   TO   FLG-NG
                            PERFORM VARYING IDZ4  FROM  1  BY  1
                               UNTIL (IDZ4             >  10 ) OR
                                     (WRK-INDEX(IDZ4)  = ZERO)
                               IF  WRK-INDEX(IDZ4)  =  IDZ2
                                   MOVE   1   TO   FLG-NG
                                   MOVE   10  TO   IDZ4
                               END-IF
                            END-PERFORM
                            IF   FLG-NG  =   1
                               CONTINUE
                            ELSE
                               MOVE   1       TO   FLG-ARI
                               ADD    1       TO   IDZ3
                               MOVE   IDZ2    TO   WRK-INDEX(IDZ3)
                               MOVE   10      TO   IDZ2
                            END-IF
                        END-IF
                     END-PERFORM
                     IF   FLG-ARI  =  ZERO
                         MOVE    10    TO    IDZ
                         MOVE     1    TO    FLG-NASHI
                     END-IF
      *
                  END-PERFORM
      *
                  IF   FLG-NASHI   =   1
                     PERFORM  900-RECE221-READ-SEC
                  ELSE
                     MOVE    1    TO   FLG-RECE221
                     MOVE    1    TO   FLG-MATOME
                  END-IF
      *
           END-PERFORM
      *
           .
       900-RECE221-INV2-EXT.
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
