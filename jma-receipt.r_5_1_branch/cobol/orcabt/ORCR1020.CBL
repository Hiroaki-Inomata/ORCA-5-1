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
       PROGRAM-ID.             ORCR1020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
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
      *  04.05.02    NACL-門脇    12/04/11  平成２４年４月改正
      *
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.03    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.04    NACL-門脇    16/11/07  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応修正
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT  RECE100-FILE    ASSIGN  RECE100PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE100-KEY
                                   FILE    STATUS  IS  STS-RECE100.
           SELECT  RECE120-FILE    ASSIGN  RECE120PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE120-KEY
                                   FILE    STATUS  IS  STS-RECE120.
           SELECT  RECE130-FILE    ASSIGN  RECE130PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE130-KEY
                                   FILE    STATUS  IS  STS-RECE130.
           SELECT  RECE121-FILE    ASSIGN  RECE121PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE121-KEY
                                   FILE    STATUS  IS  STS-RECE121.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE100-FILE.
       01  RECE100-REC. 
           COPY    "CPRCF100.INC".
      *
       FD  RECE120-FILE.
       01  RECE120-REC. 
           COPY    "CPRCF120.INC".
      *
       FD  RECE130-FILE.
       01  RECE130-REC. 
           COPY    "CPRCF130.INC".
      *
       FD  RECE121-FILE.
       01  RECE121-REC. 
           COPY    "CPRCF121.INC".
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE100//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE120//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE130//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE121//.
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
           03  STS-RECE100             PIC X(02).
           03  STS-RECE120             PIC X(02).
           03  STS-RECE130             PIC X(02).
           03  STS-RECE121             PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-RECE100             PIC 9(01).
           03  FLG-RECE120             PIC 9(01).
           03  FLG-RECE130             PIC 9(01).
           03  FLG-RECE121             PIC 9(01).
           03  FLG-NASHI               PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-MATOME              PIC 9(01).
           03  FLG-NG                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-IN-CANCEL           PIC 9(06).
           03  CNT-RECE121             PIC 9(06).
           03  CNT-RECE120             PIC 9(06). 
           03  CNT-RECE120W            PIC 9(06). 
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
               05  WRK-COM             PIC X(300)  OCCURS  10.
      *    インデックス退避
           03  WRK-INDEX-AREA.
               05  WRK-INDEX           PIC 9(003)  OCCURS  10.
      *
           03  WRK-RECEERR             PIC X(200).
      *
       01  WRK-RECE121-REC.
           COPY    "CPRCF121.INC"  REPLACING  //RECE121//
                                   BY         //WRK-RECE121//.
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
           MOVE    "RECE900"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE100PARA
      *
           MOVE    "RECE920"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE120PARA
      *
           MOVE    "RECE930"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE130PARA
      *
           MOVE    "RECE921"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE121PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1020"      TO  JOB-PGID
               MOVE    "同一剤のまとめ"
                                       TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT               RECE121-FILE
           CLOSE                        RECE121-FILE
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
           OPEN    INPUT               RECE120-FILE
                                       RECE130-FILE
                                       RECE100-FILE
           OPEN    I-O                 RECE121-FILE
           PERFORM 900-RECE100-READ-SEC
      *
           PERFORM 210-RECE121-WRITE-SEC
                                   UNTIL   FLG-RECE100 =   1
                                   OR      FLG-END     =   1
           CLOSE   RECE121-FILE
                   RECE120-FILE
                   RECE130-FILE
                   RECE100-FILE
      *
      *    元のファイルに出力処理
           IF      FLG-END             =   ZERO
               OPEN    INPUT   RECE121-FILE
                       OUTPUT  RECE120-FILE
               PERFORM 900-RECE121-READ-SEC 
      *
               PERFORM 220-RECE120-WRITE-SEC
                                   UNTIL   FLG-RECE121 =   1
                                   OR      FLG-END     =   1
               CLOSE   RECE120-FILE
                       RECE121-FILE
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    同一剤のまとめ処理
      *****************************************************************
       210-RECE121-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE120-REC
           MOVE    RECE100-KEY         TO  RECE120-KEY1
           MOVE    ZERO                TO  RECE120-ZAINUM
      *
           START   RECE120-FILE
               KEY IS    >=   RECE120-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE120
               NOT INVALID
                   PERFORM 900-RECE120-READ-SEC
           END-START
      *
           PERFORM UNTIL  ( FLG-END        =   1            )
                   OR     ( FLG-RECE120    =   1            )
                   OR     ( RECE100-KEY
                                       NOT =   RECE120-KEY1 )
      *
      *        診療種別区分取得・コメント編集
               MOVE    RECE120-ZAINUM      TO  WRK-ZAINUM
               MOVE    SPACE               TO  WRK-SRYSYUKBN
               INITIALIZE                  WRK-COM-AREA
               IF    ( RECE120-SRYKBN      =   "60" OR "64" OR "99" )
               OR    ( RECE120-COMCDFLG    =   "1"                  )
                   PERFORM 2101-RECE130-COM-HENSYU-SEC
               END-IF
      *
               MOVE    SPACE               TO  RECE121-REC
               INITIALIZE                      RECE121-REC
               MOVE    RECE120-KEY1        TO  RECE121-KEY1
               MOVE    RECE120-SRYKBN      TO  RECE121-SRYKBN
               MOVE    RECE120-ZAITEN      TO  RECE121-ZAITEN
               MOVE    RECE120-SRYCDTOTAL  TO  RECE121-SRYCDTOTAL
               MOVE    RECE120-SURYOUTOTAL TO  RECE121-SURYOUTOTAL
               MOVE    RECE120-RUIKEITOTAL TO  RECE121-RUIKEITOTAL
               MOVE    RECE120-MEISAISU    TO  RECE121-MEISAISU 
               MOVE    RECE120-FLG-HOUTEN-COMMENT
                                           TO
                                           RECE121-FLG-HOUTEN-COMMENT
               MOVE    RECE120-JIHOKBN     TO  RECE121-JIHOKBN
      *
               IF      RECE120-COMCDFLG    =   "2"
                   MOVE    SPACE               TO  RECE121-SRYSYUKBN
                   ADD     1                   TO  WRK-COMRENNUM
                   MOVE    WRK-COMRENNUM       TO  RECE121-COMRENNUM
                   MOVE    RECE120-ZAIKAISU    TO  RECE121-ZAIKAISU
                   MOVE    RECE120-DAY-AREA    TO  RECE121-DAY-AREA
                   MOVE    RECE120-REC         TO  RECE121-REC1
                   WRITE   RECE121-REC
                   ADD     1                   TO  CNT-RECE121
               ELSE
                   MOVE    WRK-SRYSYUKBN       TO  RECE121-SRYSYUKBN
                   IF      RECE120-COMCDFLG    =   "1"
                       ADD     1                   TO  WRK-COMRENNUM
                       MOVE    WRK-COMRENNUM       TO  RECE121-COMRENNUM
      *
                       MOVE    RECE121-REC         TO  WRK-RECE121-REC
                       PERFORM 900-RECE121-INV2-SEC
                       IF      FLG-MATOME          =   ZERO
                           MOVE    WRK-RECE121-REC TO  RECE121-REC
                           MOVE    RECE120-REC     TO  RECE121-REC1
                           MOVE    WRK-COM-AREA    TO  RECE121-COM-AREA
                       END-IF
      *                剤回数・日数回数テーブルの加算
                       ADD     RECE120-ZAIKAISU    TO  RECE121-ZAIKAISU
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >      10
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       31
                               ADD     RECE120-KAISU (IDX IDY)
                                          TO   RECE121-KAISU (IDX IDY)
                           END-PERFORM
                       END-PERFORM
      *
                       IF      RECE120-SYODOU-SRYKA (1)
                                               NOT =   SPACE
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       10
                               IF      RECE120-SYODOU-SRYKA (1)    =
                                           RECE121-SYODOU-SRYKA (IDX)
                                   MOVE    10          TO  IDX
                               ELSE
                                   IF      RECE121-SYODOU-SRYKA(IDX)
                                                           =   SPACE
                                       MOVE   RECE120-SYODOU-SRYKA (1)
                                                           TO
                                              RECE121-SYODOU-SRYKA (IDX)
                                       MOVE    10          TO  IDX
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                       IF      RECE120-SAIDOU-SRYKA (1)
                                               NOT =   SPACE
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       10
                               IF      RECE120-SAIDOU-SRYKA (1)    =
                                           RECE121-SAIDOU-SRYKA (IDX)
                                   MOVE    10          TO  IDX
                               ELSE
                                   IF      RECE121-SAIDOU-SRYKA(IDX)
                                                           =   SPACE
                                       MOVE   RECE120-SAIDOU-SRYKA (1)
                                                           TO
                                              RECE121-SAIDOU-SRYKA (IDX)
                                       MOVE    10          TO  IDX
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
      *
                       IF      FLG-MATOME      =   ZERO
                           WRITE   RECE121-REC
                           ADD     1               TO  CNT-RECE121
                       ELSE
                           REWRITE RECE121-REC
                       END-IF
                   ELSE
                       MOVE    RECE121-REC     TO  WRK-RECE121-REC
                       PERFORM 900-RECE121-INV-SEC
                       IF      FLG-RECE121     =   1
                           MOVE    WRK-RECE121-REC     TO  RECE121-REC
                           MOVE    RECE120-REC         TO  RECE121-REC1
                       END-IF
      *                剤回数・日数回数テーブルの加算
                       ADD     RECE120-ZAIKAISU    TO  RECE121-ZAIKAISU
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >      10
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       31
                             ADD     RECE120-KAISU (IDX IDY)
                                          TO   RECE121-KAISU (IDX IDY)
                           END-PERFORM
                       END-PERFORM
      *
                       IF      RECE120-SYODOU-SRYKA (1)
                                               NOT =   SPACE
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       10
                               IF      RECE120-SYODOU-SRYKA (1)    =
                                           RECE121-SYODOU-SRYKA (IDX)
                                   MOVE    10          TO  IDX
                               ELSE
                                   IF      RECE121-SYODOU-SRYKA(IDX)
                                                           =   SPACE
                                       MOVE   RECE120-SYODOU-SRYKA (1)
                                                           TO
                                              RECE121-SYODOU-SRYKA (IDX)
                                       MOVE    10          TO  IDX
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                       IF      RECE120-SAIDOU-SRYKA (1)
                                               NOT =   SPACE
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       10
                               IF      RECE120-SAIDOU-SRYKA (1)    =
                                           RECE121-SAIDOU-SRYKA (IDX)
                                   MOVE    10          TO  IDX
                               ELSE
                                   IF      RECE121-SAIDOU-SRYKA(IDX)
                                                           =   SPACE
                                       MOVE   RECE120-SAIDOU-SRYKA (1)
                                                           TO
                                              RECE121-SAIDOU-SRYKA (IDX)
                                       MOVE    10          TO  IDX
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
      *
                       IF      FLG-RECE121         =   1
                           WRITE   RECE121-REC
                           ADD     1                   TO  CNT-RECE121
                       ELSE
                           REWRITE RECE121-REC
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM     900-RECE120-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
           AND     FLG-RECE120         =   ZERO
           AND     RECE100-KEY     NOT =   RECE120-KEY1
               COMPUTE   CNT-IN  =  CNT-IN  -  1
           END-IF
      *
           IF      FLG-END             =   ZERO
               PERFORM 900-RECE100-READ-SEC
           END-IF
           .
       210-RECE121-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分・コメント取得
      *****************************************************************
       2101-RECE130-COM-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDZ2
      *
           INITIALIZE                      RECE130-REC
           MOVE    RECE100-KEY         TO  RECE130-KEY1
           MOVE    WRK-ZAINUM          TO  RECE130-ZAINUM
           START   RECE130-FILE
               KEY IS    >=   RECE130-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE130
               NOT INVALID
                   PERFORM  900-RECE130-READ-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE130     =      1          ) OR
                         ( RECE100-KEY NOT =  RECE130-KEY1   ) OR
                         ( WRK-ZAINUM  NOT =  RECE130-ZAINUM )
      *
      *        検査のとき最初の診療区分を取得
               IF    ( RECE120-SRYKBN      =   "60" OR "64" OR "99" )
               AND   ( WRK-SRYSYUKBN       =    SPACE               )
                   MOVE   RECE130-SRYSYUKBN   TO   WRK-SRYSYUKBN
               END-IF
      *        コメント取得
               IF      RECE120-COMCDFLG    =   "1"
                   PERFORM  VARYING IDZ    FROM    1   BY  1
                            UNTIL ( IDZ    >       5 )
                            OR    ( RECE130-SRYCD(IDZ)
                                           =   SPACE )
                       IF    ( RECE130-SRYCD(IDZ)(1:1)     =   "8"   )
                       OR    ( RECE130-SRYCD(IDZ)(1:3)     =   "008" )
                         IF    RECE130-RECEPTFLG(IDZ)   =  "1"
                           CONTINUE
                         ELSE
                           ADD     1               TO  IDZ2
                           IF      IDZ2            <=  10
                               MOVE   RECE130-NAME (IDZ)
                                                   TO  WRK-COM (IDZ2)
                           END-IF
                         END-IF
                       END-IF
                   END-PERFORM
                   PERFORM  900-RECE130-READ-SEC
               ELSE
      ***          IF      WRK-SRYSYUKBN   NOT =   SPACE
                       MOVE    1               TO  FLG-RECE130
      ***          END-IF
               END-IF
           END-PERFORM
      *
           .
       2101-RECE130-COM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    元のファイルに出力処理
      *****************************************************************
       220-RECE120-WRITE-SEC           SECTION.
      *
           MOVE    RECE121-REC1        TO  RECE120-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE121-ZAIKAISU     TO  RECE120-ZAIKAISU
           MOVE    RECE121-DAY-AREA     TO  RECE120-DAY-AREA
      *
           MOVE    RECE121-SYODOU-SRYKA-AREA
                                        TO  RECE120-SYODOU-SRYKA-AREA
           MOVE    RECE121-SAIDOU-SRYKA-AREA
                                        TO  RECE120-SAIDOU-SRYKA-AREA
      *
           WRITE   RECE120-REC
           ADD     1                   TO  CNT-RECE120W
      *
           PERFORM     900-RECE121-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       220-RECE120-WRITE-EXT.
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
                       MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                       MOVE    WRK-PARA-SHELLID
                                               TO  JOB-SHELLID
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE120  CNT-IN  :" CNT-IN
           DISPLAY "RECE121  CNT-OUT :" CNT-RECE121
           DISPLAY "RECE120  CNT-OUT :" CNT-RECE120W
           DISPLAY "*** ORCR1020 END ***"
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
       900-RECE100-READ-SEC           SECTION.
      *
           READ    RECE100-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE100
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE100
      *
                   DISPLAY "RECE100  PTNUM=" RECE100-PTNUM
           END-READ
           .
       900-RECE100-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報ファイル読込処理
      *****************************************************************
       900-RECE120-READ-SEC            SECTION.
      *
           READ    RECE120-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE120
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE120
                   ADD     1             TO    CNT-IN
                   ADD     1             TO    CNT-IN-CANCEL
           END-READ
           .
       900-RECE120-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄情報ファイル読込処理
      *****************************************************************
       900-RECE130-READ-SEC            SECTION.
      *
           READ    RECE130-FILE   NEXT
               AT  END
                   MOVE    1      TO    FLG-RECE130
               NOT AT  END
                   MOVE    ZERO   TO    FLG-RECE130
           END-READ
           .
       900-RECE130-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報まとめファイル読込処理
      *****************************************************************
       900-RECE121-READ-SEC            SECTION.
      *
           READ    RECE121-FILE   NEXT
               AT  END
                   MOVE    1             TO  FLG-RECE121
               NOT AT END    
                   MOVE    ZERO          TO  FLG-RECE121
                   ADD     1             TO  CNT-IN-CANCEL
           END-READ
           .
       900-RECE121-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数情報まとめファイル読込処理
      *****************************************************************
       900-RECE121-INV-SEC            SECTION.
      *
           READ    RECE121-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE121
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE121
           END-READ
           .
       900-RECE121-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE121-INV2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MATOME
      *
           INITIALIZE                      RECE121-REC
           MOVE    RECE120-KEY1        TO  RECE121-KEY1
           MOVE    RECE120-SRYKBN      TO  RECE121-SRYKBN
           MOVE    WRK-SRYSYUKBN       TO  RECE121-SRYSYUKBN
           MOVE    RECE120-ZAITEN      TO  RECE121-ZAITEN
           MOVE    RECE120-SRYCDTOTAL  TO  RECE121-SRYCDTOTAL
           MOVE    RECE120-SURYOUTOTAL TO  RECE121-SURYOUTOTAL
           MOVE    RECE120-RUIKEITOTAL TO  RECE121-RUIKEITOTAL
           MOVE    RECE120-MEISAISU    TO  RECE121-MEISAISU
           MOVE    RECE120-FLG-HOUTEN-COMMENT
                                       TO  RECE121-FLG-HOUTEN-COMMENT
           MOVE    RECE120-JIHOKBN     TO  RECE121-JIHOKBN
           START   RECE121-FILE
               KEY IS    >=   RECE121-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE121
               NOT INVALID
                   PERFORM  900-RECE121-READ-SEC
           END-START
           PERFORM   UNTIL
                 ( RECE120-KEY1        NOT =  RECE121-KEY1        ) OR
                 ( RECE120-SRYKBN      NOT =  RECE121-SRYKBN      ) OR
                 ( WRK-SRYSYUKBN       NOT =  RECE121-SRYSYUKBN   ) OR
                 ( RECE120-ZAITEN      NOT =  RECE121-ZAITEN      ) OR
                 ( RECE120-SRYCDTOTAL  NOT =  RECE121-SRYCDTOTAL  ) OR
                 ( RECE120-SURYOUTOTAL NOT =  RECE121-SURYOUTOTAL ) OR
                 ( RECE120-RUIKEITOTAL NOT =  RECE121-RUIKEITOTAL ) OR
                 ( RECE120-MEISAISU    NOT =  RECE121-MEISAISU    ) OR
                 ( RECE120-FLG-HOUTEN-COMMENT
                                       NOT =
                                       RECE121-FLG-HOUTEN-COMMENT ) OR
                 ( RECE120-JIHOKBN     NOT =  RECE121-JIHOKBN     ) OR
                 ( FLG-RECE121             =  1                   )
      *
               MOVE     ZERO       TO  FLG-NASHI
               MOVE     ZERO       TO  IDZ3
               INITIALIZE              WRK-INDEX-AREA
      *
               PERFORM VARYING IDZ FROM    1   BY   1
                       UNTIL ( IDZ         >   10    )  OR
                             ( WRK-COM (IDZ)
                                           =   SPACE )
                   MOVE    ZERO        TO  FLG-ARI
                   PERFORM VARYING IDZ2    FROM    1   BY  1
                           UNTIL ( IDZ2                >   10    )  OR
                                 ( RECE121-COM (IDZ2)  =   SPACE )
                       IF      RECE121-COM (IDZ2)  =  WRK-COM (IDZ)
                           MOVE   ZERO             TO  FLG-NG
                           PERFORM VARYING IDZ4    FROM    1   BY  1
                               UNTIL (IDZ4                 >   10   ) OR
                                     (WRK-INDEX (IDZ4)     =   ZERO )
                               IF      WRK-INDEX (IDZ4) =  IDZ2
                                   MOVE    1               TO  FLG-NG
                                   MOVE    10              TO  IDZ4
                               END-IF
                           END-PERFORM
                           IF      FLG-NG          =   1
                               CONTINUE
                           ELSE
                               MOVE    1           TO   FLG-ARI
                               ADD     1           TO   IDZ3
                               MOVE    IDZ2        TO   WRK-INDEX (IDZ3)
                               MOVE    10          TO   IDZ2
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      FLG-ARI             =   ZERO
                       MOVE    10                  TO  IDZ
                       MOVE     1                  TO  FLG-NASHI
                   END-IF
               END-PERFORM
      *
               IF      FLG-NASHI           =   1
                   PERFORM 900-RECE121-READ-SEC
               ELSE
                   MOVE    1               TO   FLG-RECE121
                   MOVE    1               TO   FLG-MATOME
               END-IF
           END-PERFORM
      *
           .
       900-RECE121-INV2-EXT.
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
