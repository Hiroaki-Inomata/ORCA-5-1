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
       PROGRAM-ID.             ORCR0425.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 同一剤のまとめ（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/01/14    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.13  剤点数をキーに追加対応
      *  01.00.02    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.02    NACL-門脇    04.07.12  ファイルクリア追加
      *  01.01.03    NACL-門脇    04.08.17  レセプト科の編集
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    05/04/20  内服薬剤逓減編集対応
      *  01.01.03    NACL-門脇    05/07/05  検査は診療種別区分毎でまとめる
      *  01.01.04    NACL-門脇    05.09.06  自賠責従来様式対応
      *  01.01.05    NACL-門脇    05/11/11  コメントを含む場合の編集修正
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    12/04/11  平成２４年４月改正
      *
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.03    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.04    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.05    NACL-門脇    16/11/07  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応修正
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE442-FILE    ASSIGN  RECE442PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE442-KEY
                                   FILE    STATUS  IS  STS-RECE442.
           SELECT  RECE442X-FILE   ASSIGN  RECE442PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE442X-KEY
                                   FILE    STATUS  IS  STS-RECE442.
           SELECT  RECE44W-FILE    ASSIGN  RECE44WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44W-KEY
                                   FILE    STATUS  IS  STS-RECE44W.
           SELECT  RECE44WX-FILE   ASSIGN  RECE44WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44WX-KEY
                                   FILE    STATUS  IS  STS-RECE44W.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC. 
           COPY    "CPRCF044.INC".
      *
       FD  RECE45-FILE.
       01  RECE45-REC. 
           COPY    "CPRCF045.INC".
      *
       FD  RECE442-FILE.
       01  RECE442-REC. 
           COPY    "CPRCF0442.INC".
      *
       FD  RECE442X-FILE.
       01  RECE442X-REC. 
           COPY    "CPRCF0442.INC" REPLACING  //RECE442//
                                   BY         //RECE442X//.
      *
       FD  RECE44W-FILE.
       01  RECE44W-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44W//.
      *
       FD  RECE44WX-FILE.
       01  RECE44WX-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44WX//.
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE442//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44W//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41                              PIC X(02).
           03  STS-RECE44                              PIC X(02).
           03  STS-RECE45                              PIC X(02).
           03  STS-RECE442                             PIC X(02).
           03  STS-RECE44W                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE41                              PIC 9(01).
           03  FLG-RECE44                              PIC 9(01).
           03  FLG-RECE45                              PIC 9(01).
           03  FLG-RECE442                             PIC 9(01).
           03  FLG-NASHI                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-MATOME                              PIC 9(01).
           03  FLG-NG                                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-RECE442                             PIC 9(06).
           03  CNT-RECE44W                             PIC 9(06). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDZ2                                    PIC 9(03).
           03  IDZ3                                    PIC 9(03).
           03  IDZ4                                    PIC 9(03).
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
       01  WRK-RECE442-REC.
           COPY    "CPRCF0442.INC" REPLACING  //RECE442//
                                   BY         //WRK-RECE442//.
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
           MOVE    SPACE               TO  RECE41PARA
           INITIALIZE                      RECE41PARA
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE44PARA
           INITIALIZE                      RECE44PARA
           MOVE    "RECE440"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE45PARA
           INITIALIZE                      RECE45PARA
           MOVE    "RECE450"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE442PARA
           INITIALIZE                      RECE442PARA
           MOVE    "RECE442"           TO  RECE442PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE442PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE442PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE44WPARA
           INITIALIZE                      RECE44WPARA
           MOVE    "RECE44W"           TO  RECE44WPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44WPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44WPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE442PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE44WPARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE41PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE44PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE45PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE442PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE44WPARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0425"      TO  JOB-PGID
               MOVE    "同一剤のまとめ（労災・自賠責）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE442X-FILE
           CLOSE                       RECE442X-FILE
      *
           OPEN    OUTPUT              RECE44WX-FILE
           CLOSE                       RECE44WX-FILE
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
           OPEN    INPUT               RECE41-FILE
                                       RECE44-FILE
                                       RECE45-FILE
           OPEN    I-O                 RECE442-FILE
           PERFORM 900-RECE41-READ-SEC
      *
           PERFORM 210-RECE442-WRITE-SEC
                                   UNTIL   FLG-RECE41  =   1
                                   OR      FLG-END     =   1
           CLOSE   RECE442-FILE
                   RECE44-FILE
                   RECE45-FILE
                   RECE41-FILE
      *
      *    元のレイアウトに出力処理
           OPEN    INPUT   RECE442-FILE
                   OUTPUT  RECE44W-FILE
      *
           PERFORM 900-RECE442-READ-SEC 
           PERFORM 220-RECE44W-WRITE-SEC
                                   UNTIL   FLG-RECE442 =   1
                                   OR      FLG-END     =   1
           CLOSE   RECE44W-FILE
                   RECE442-FILE
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
       210-RECE442-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 900-RECE44-READ-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               MOVE    RECE44-ZAINUM     TO      WRK-ZAINUM
               MOVE    SPACE             TO      WRK-SRYSYUKBN
               IF       RECE44-SRYKBN  =  "60" OR "64" OR "99"
                   PERFORM   2101-RECE45-SRYSYUKBN-HEN-SEC
               END-IF
      *
               INITIALIZE          WRK-COM-AREA
               IF       RECE44-COMCDFLG   =   "1"
                   PERFORM   2101-RECE45-COM-HENSYU-SEC
               END-IF
      *
               IF      RECE44-COMCDFLG     =   "3"
                   MOVE    SPACE               TO  RECE442-REC
                   INITIALIZE                      RECE442-REC
                   MOVE    RECE44-HOSPNUM      TO  RECE442-HOSPNUM
                   MOVE    RECE44-SRYYM        TO  RECE442-SRYYM
                   MOVE    RECE44-NYUGAIKBN    TO  RECE442-NYUGAIKBN
                   MOVE    RECE44-PTID         TO  RECE442-PTID
                   MOVE    RECE44-RECEKA       TO  RECE442-RECEKA
                   MOVE    RECE44-HKNID        TO  RECE442-HKNID
                   MOVE    RECE44-TEKSTYMD     TO  RECE442-TEKSTYMD
                   MOVE    RECE44-SRYKBN       TO  RECE442-SRYKBN
                   MOVE    SPACE               TO  RECE442-SRYSYUKBN
                   MOVE    RECE44-ZAITEN       TO  RECE442-ZAITEN
                   MOVE    RECE44-SRYCDTOTAL   TO  RECE442-SRYCDTOTAL
                   MOVE    RECE44-SURYOUTOTAL  TO  RECE442-SURYOUTOTAL
                   MOVE    RECE44-RUIKEITOTAL  TO  RECE442-RUIKEITOTAL
                   MOVE    RECE44-MEISAISU     TO  RECE442-MEISAISU 
                   MOVE    RECE44-FLG-HOUTEN-COMMENT
                                      TO  RECE442-FLG-HOUTEN-COMMENT
                   MOVE    RECE44-JIHOKBN      TO  RECE442-JIHOKBN
                   ADD     1                   TO  WRK-COMRENNUM
                   MOVE    WRK-COMRENNUM       TO  RECE442-COMRENNUM
                   MOVE    RECE44-ZAIKAISU     TO  RECE442-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >      10
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           MOVE    RECE44-KAISU (IDX IDY)
                                           TO   RECE442-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
                   MOVE    RECE44-REC          TO  RECE442-REC1
                   WRITE   RECE442-REC
                   ADD     1                   TO  CNT-RECE442
               ELSE
                   MOVE    SPACE               TO  RECE442-REC
                   INITIALIZE                      RECE442-REC
                   MOVE    RECE44-HOSPNUM      TO  RECE442-HOSPNUM
                   MOVE    RECE44-SRYYM        TO  RECE442-SRYYM
                   MOVE    RECE44-NYUGAIKBN    TO  RECE442-NYUGAIKBN
                   MOVE    RECE44-PTID         TO  RECE442-PTID
                   MOVE    RECE44-RECEKA       TO  RECE442-RECEKA
                   MOVE    RECE44-HKNID        TO  RECE442-HKNID
                   MOVE    RECE44-TEKSTYMD     TO  RECE442-TEKSTYMD
                   MOVE    RECE44-SRYKBN       TO  RECE442-SRYKBN
                   MOVE    WRK-SRYSYUKBN       TO  RECE442-SRYSYUKBN
                   MOVE    RECE44-ZAITEN       TO  RECE442-ZAITEN
                   MOVE    RECE44-SRYCDTOTAL   TO  RECE442-SRYCDTOTAL
                   MOVE    RECE44-SURYOUTOTAL  TO  RECE442-SURYOUTOTAL
                   MOVE    RECE44-RUIKEITOTAL  TO  RECE442-RUIKEITOTAL
                   MOVE    RECE44-MEISAISU     TO  RECE442-MEISAISU 
                   MOVE    RECE44-FLG-HOUTEN-COMMENT
                                      TO  RECE442-FLG-HOUTEN-COMMENT
                   MOVE    RECE44-JIHOKBN      TO  RECE442-JIHOKBN
                   IF      RECE44-COMCDFLG   =   "1"
                     ADD     1                 TO  WRK-COMRENNUM
                     MOVE    WRK-COMRENNUM     TO  RECE442-COMRENNUM
      *
                     MOVE    RECE442-REC         TO  WRK-RECE442-REC
                     PERFORM 900-RECE442-INV2-SEC
                     IF      FLG-MATOME    =   ZERO
                         MOVE   WRK-RECE442-REC  TO  RECE442-REC
                         MOVE   RECE44-REC       TO  RECE442-REC1
                         MOVE   WRK-COM-AREA     TO  RECE442-COM-AREA
                     END-IF
      *              剤回数・日数回数テーブルの加算
                     ADD     RECE44-ZAIKAISU     TO  RECE442-ZAIKAISU
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >      10
                         PERFORM VARYING IDY FROM    1   BY  1
                                 UNTIL   IDY >       31
                             ADD     RECE44-KAISU (IDX IDY)
                                         TO   RECE442-KAISU (IDX IDY)
                         END-PERFORM
                     END-PERFORM
      *
                     IF  RECE44-SYODOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE442-SYODOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE44-SYODOU-SRYKA(1)
                                     =  RECE442-SYODOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE44-SYODOU-SRYKA(1)
                                        TO   RECE442-SYODOU-SRYKA(IDX)
                       END-IF
                     END-IF
                     IF  RECE44-SAIDOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE442-SAIDOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE44-SAIDOU-SRYKA(1)
                                     =  RECE442-SAIDOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE44-SAIDOU-SRYKA(1)
                                        TO   RECE442-SAIDOU-SRYKA(IDX)
                       END-IF
                     END-IF
      *
                     IF      FLG-MATOME    =   ZERO
                         WRITE   RECE442-REC
                         ADD     1                   TO  CNT-RECE442
                     ELSE
                         REWRITE RECE442-REC
                     END-IF
                   ELSE
                     MOVE    RECE442-REC         TO  WRK-RECE442-REC
                     PERFORM 900-RECE442-INV-SEC
                     IF      FLG-RECE442         =   1
                         MOVE    WRK-RECE442-REC     TO  RECE442-REC
                         MOVE    RECE44-REC          TO  RECE442-REC1
                     END-IF
      *              剤回数・日数回数テーブルの加算
                     ADD     RECE44-ZAIKAISU     TO  RECE442-ZAIKAISU
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >      10
                         PERFORM VARYING IDY FROM    1   BY  1
                                 UNTIL   IDY >       31
                             ADD     RECE44-KAISU (IDX IDY)
                                         TO   RECE442-KAISU (IDX IDY)
                         END-PERFORM
                     END-PERFORM
      *
                     IF  RECE44-SYODOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE442-SYODOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE44-SYODOU-SRYKA(1)
                                     =  RECE442-SYODOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE44-SYODOU-SRYKA(1)
                                        TO   RECE442-SYODOU-SRYKA(IDX)
                       END-IF
                     END-IF
                     IF  RECE44-SAIDOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE442-SAIDOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE44-SAIDOU-SRYKA(1)
                                     =  RECE442-SAIDOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE44-SAIDOU-SRYKA(1)
                                        TO   RECE442-SAIDOU-SRYKA(IDX)
                       END-IF
                     END-IF
      *
                     IF      FLG-RECE442         =   1
                         WRITE   RECE442-REC
                         ADD     1                   TO  CNT-RECE442
                     ELSE
                         REWRITE RECE442-REC
                     END-IF
                   END-IF
               END-IF
      *
               PERFORM     900-RECE44-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           PERFORM 900-RECE41-READ-SEC
           .
       210-RECE442-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分取得
      *****************************************************************
       2101-RECE45-SRYSYUKBN-HEN-SEC         SECTION.
      *
           INITIALIZE                           RECE45-REC
           MOVE    RECE41-HOSPNUM       TO      RECE45-HOSPNUM
           MOVE    RECE41-SRYYM         TO      RECE45-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE45-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE45-PTID
           MOVE    RECE41-RECEKA        TO      RECE45-RECEKA
           MOVE    RECE41-HKNID         TO      RECE45-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE45-TEKSTYMD
           MOVE    WRK-ZAINUM           TO      RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE45
               NOT INVALID
                   PERFORM  900-RECE45-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE41-HOSPNUM       NOT =  RECE45-HOSPNUM)   OR
                  (RECE41-SRYYM         NOT =  RECE45-SRYYM)     OR
                  (RECE41-NYUGAIKBN     NOT =  RECE45-NYUGAIKBN) OR
                  (RECE41-PTID          NOT =  RECE45-PTID)      OR
                  (RECE41-RECEKA        NOT =  RECE45-RECEKA)    OR
                  (RECE41-HKNID         NOT =  RECE45-HKNID)     OR
                  (RECE41-TEKSTYMD      NOT =  RECE45-TEKSTYMD)  OR
                  (WRK-ZAINUM           NOT =  RECE45-ZAINUM)    OR
                  (FLG-RECE45               =      1        )
      *
                   MOVE   RECE45-SRYSYUKBN   TO   WRK-SRYSYUKBN
      *
                   MOVE   1                  TO   FLG-RECE45
      *
           END-PERFORM
      *
           .
       2101-RECE45-SRYSYUKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集
      *****************************************************************
       2101-RECE45-COM-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                 TO      IDZ2
      *
           INITIALIZE                           RECE45-REC
           MOVE    RECE41-HOSPNUM       TO      RECE45-HOSPNUM
           MOVE    RECE41-SRYYM         TO      RECE45-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE45-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE45-PTID
           MOVE    RECE41-RECEKA        TO      RECE45-RECEKA
           MOVE    RECE41-HKNID         TO      RECE45-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE45-TEKSTYMD
           MOVE    WRK-ZAINUM           TO      RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE45
               NOT INVALID
                   PERFORM  900-RECE45-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE41-HOSPNUM       NOT =  RECE45-HOSPNUM)   OR
                  (RECE41-SRYYM         NOT =  RECE45-SRYYM)     OR
                  (RECE41-NYUGAIKBN     NOT =  RECE45-NYUGAIKBN) OR
                  (RECE41-PTID          NOT =  RECE45-PTID)      OR
                  (RECE41-RECEKA        NOT =  RECE45-RECEKA)    OR
                  (RECE41-HKNID         NOT =  RECE45-HKNID)     OR
                  (RECE41-TEKSTYMD      NOT =  RECE45-TEKSTYMD)  OR
                  (WRK-ZAINUM           NOT =  RECE45-ZAINUM)    OR
                  (FLG-RECE45               =      1        )
      *
                  PERFORM  VARYING   IDZ   FROM    1    BY   1
                            UNTIL   (IDZ                >       5)  OR
                                    (RECE45-SRYCD(IDZ)  =   SPACE)
      *
                     IF  (RECE45-SRYCD(IDZ)(1:1)  =   "8"  )  OR
                         (RECE45-SRYCD(IDZ)(1:3)  =   "008")
                       IF   (RECE45-RECEPTFLG(IDZ)   =  "1")
                       AND  (RECE45-RECEDENFLG(IDZ)  =  "1")
                         CONTINUE
                       ELSE
                         ADD       1                 TO  IDZ2
                         IF   IDZ2  <=  10
                            MOVE   RECE45-NAME(IDZ)  TO  WRK-COM(IDZ2)
                         END-IF
                       END-IF
                     END-IF
      *
                  END-PERFORM
      *
                  PERFORM  900-RECE45-READ-SEC
      *
           END-PERFORM
      *
           .
       2101-RECE45-COM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    元のレイアウトに出力処理
      *****************************************************************
       220-RECE44W-WRITE-SEC                SECTION.
      *
           MOVE    RECE442-REC1        TO  RECE44W-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE442-ZAIKAISU     TO  RECE44W-ZAIKAISU
           MOVE    RECE442-DAY-AREA     TO  RECE44W-DAY-AREA
      *
           MOVE    RECE442-SYODOU-SRYKA-AREA
                                        TO  RECE44W-SYODOU-SRYKA-AREA
           MOVE    RECE442-SAIDOU-SRYKA-AREA
                                        TO  RECE44W-SAIDOU-SRYKA-AREA
      *
           WRITE   RECE44W-REC
           ADD     1                   TO  CNT-RECE44W
      *
           PERFORM     900-RECE442-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       220-RECE44W-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
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
           DISPLAY "RECE44     :" CNT-IN   
           DISPLAY "RECE442    :" CNT-RECE442
           DISPLAY "RECE44W    :" CNT-RECE44W
           DISPLAY "*** ORCR0425 END ***"
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
      *    レセプト０２データ読込処理
      *****************************************************************
       900-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE41
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE41
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
           .
       900-RECE41-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   レセプト０４データ読込処理
      *****************************************************************
       900-RECE44-READ-SEC            SECTION.
      *
           READ    RECE44-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE44
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE44
                   ADD     1             TO    CNT-IN
                   ADD     1             TO    CNT-IN-CANCEL
           END-READ
           .
       900-RECE44-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５データ読込処理
      *****************************************************************
       900-RECE45-READ-SEC            SECTION.
      *
           READ    RECE45-FILE   NEXT
               AT  END
                   MOVE    1      TO    FLG-RECE45
               NOT AT  END
                   MOVE    ZERO   TO    FLG-RECE45
           END-READ
           .
       900-RECE45-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE442-READ-SEC            SECTION.
      *
           READ    RECE442-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE442
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE442
                   ADD     1             TO    CNT-IN-CANCEL
           END-READ
           .
       900-RECE442-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE442-INV-SEC            SECTION.
      *
           READ    RECE442-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE442
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE442
           END-READ
           .
       900-RECE442-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE442-INV2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MATOME
      *
           INITIALIZE                      RECE442-REC
           MOVE    RECE44-HOSPNUM      TO  RECE442-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE442-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE442-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE442-PTID
           MOVE    RECE44-RECEKA       TO  RECE442-RECEKA
           MOVE    RECE44-HKNID        TO  RECE442-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE442-TEKSTYMD
           MOVE    RECE44-SRYKBN       TO  RECE442-SRYKBN
           MOVE    WRK-SRYSYUKBN       TO  RECE442-SRYSYUKBN
           MOVE    RECE44-ZAITEN       TO  RECE442-ZAITEN
           MOVE    RECE44-SRYCDTOTAL   TO  RECE442-SRYCDTOTAL
           MOVE    RECE44-SURYOUTOTAL  TO  RECE442-SURYOUTOTAL
           MOVE    RECE44-RUIKEITOTAL  TO  RECE442-RUIKEITOTAL
           MOVE    RECE44-MEISAISU     TO  RECE442-MEISAISU
           MOVE    RECE44-FLG-HOUTEN-COMMENT
                                       TO  RECE442-FLG-HOUTEN-COMMENT
           MOVE    RECE44-JIHOKBN      TO  RECE442-JIHOKBN
           START   RECE442-FILE
               KEY IS    >=   RECE442-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE442
               NOT INVALID
                   PERFORM  900-RECE442-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE44-HOSPNUM      NOT =  RECE442-HOSPNUM)      OR
                  (RECE44-SRYYM        NOT =  RECE442-SRYYM)        OR
                  (RECE44-NYUGAIKBN    NOT =  RECE442-NYUGAIKBN)    OR
                  (RECE44-PTID         NOT =  RECE442-PTID)         OR
                  (RECE44-RECEKA       NOT =  RECE442-RECEKA)       OR
                  (RECE44-HKNID        NOT =  RECE442-HKNID)        OR
                  (RECE44-TEKSTYMD     NOT =  RECE442-TEKSTYMD)     OR
                  (RECE44-SRYKBN       NOT =  RECE442-SRYKBN)       OR
                  (WRK-SRYSYUKBN       NOT =  RECE442-SRYSYUKBN)    OR
                  (RECE44-ZAITEN       NOT =  RECE442-ZAITEN)       OR
                  (RECE44-SRYCDTOTAL   NOT =  RECE442-SRYCDTOTAL)   OR
                  (RECE44-SURYOUTOTAL  NOT =  RECE442-SURYOUTOTAL)  OR
                  (RECE44-RUIKEITOTAL  NOT =  RECE442-RUIKEITOTAL)  OR
                  (RECE44-MEISAISU     NOT =  RECE442-MEISAISU)     OR
                  (RECE44-FLG-HOUTEN-COMMENT
                                NOT =  RECE442-FLG-HOUTEN-COMMENT)  OR
                  (RECE44-JIHOKBN      NOT =  RECE442-JIHOKBN)      OR
                  (FLG-RECE442             =  1)
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
                                    (RECE442-COM(IDZ2)  =   SPACE)
                        IF  RECE442-COM(IDZ2)  =  WRK-COM(IDZ)
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
                     PERFORM  900-RECE442-READ-SEC
                  ELSE
                     MOVE    1    TO   FLG-RECE442
                     MOVE    1    TO   FLG-MATOME
                  END-IF
      *
           END-PERFORM
      *
           .
       900-RECE442-INV2-EXT.
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
