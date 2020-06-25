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
       PROGRAM-ID.             ORCR0655.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 科別の同一剤のまとめをする
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/01/14    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.12  剤点数をキーに追加対応
      *  01.00.02    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  01.00.03    NACL-門脇    04.07.12  ファイルクリア追加
      *  01.00.04    NACL-門脇    04.08.17  レセプト科の編集
      *
      *  02.06.01    NACL-竹田    05/05/30  realtime preview 処理追加
      *                                     中止処理の追加
      *  02.06.02    NACL-門脇    05.07.05  検査は診療種別区分毎でまとめる
      *  02.06.03    NACL-門脇    05.11.11  コメントを含む場合の編集修正
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  03.05.01    NACL-小豆沢  07/06/04  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.07.01    NACL-門脇    15/08/10  日別保険組合せ番号編集対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.03    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.04    NACL-門脇    16/08/23  持参薬対応
      *  04.08.05    NACL-門脇    16/11/07  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応修正
      *
      *  05.00.01    NACL-藤原    18/06/11  ファイルのエラー処理追加
      *  05.00.02    NACL-門脇    19/08/20  公費分離区分最小値編集修正
      *  05.00.03    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
           SELECT  RECE65-FILE     ASSIGN  RECE65PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE65-KEY
                                   FILE    STATUS  IS  STS-RECE65.
           SELECT  RECE642-FILE    ASSIGN  RECE642PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE642-KEY
                                   FILE    STATUS  IS  STS-RECE642.
           SELECT  RECE64W-FILE    ASSIGN  RECE64WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE64W-KEY
                                   FILE    STATUS  IS  STS-RECE64W.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
       FD  RECE64-FILE.
       01  RECE64-REC. 
           COPY    "CPRCF064.INC".
      *
       FD  RECE65-FILE.
       01  RECE65-REC. 
           COPY    "CPRCF065.INC".
      *
       FD  RECE642-FILE.
       01  RECE642-REC. 
           COPY    "CPRCF0642.INC".
      *
       FD  RECE64W-FILE.
       01  RECE64W-REC. 
           COPY    "CPRCF064.INC"  REPLACING  //RECE64//
                                   BY         //RECE64W//.
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE62PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE64PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE65PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE642PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE64WPARA//.
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
           03  STS-RECE62                              PIC X(02).
           03  STS-RECE64                              PIC X(02).
           03  STS-RECE65                              PIC X(02).
           03  STS-RECE642                             PIC X(02).
           03  STS-RECE64W                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE62                              PIC 9(01).
           03  FLG-RECE64                              PIC 9(01).
           03  FLG-RECE65                              PIC 9(01).
           03  FLG-RECE642                             PIC 9(01).
           03  FLG-NASHI                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-MATOME                              PIC 9(01).
           03  FLG-NG                                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-RECE642                             PIC 9(06).
           03  CNT-RECE64W                             PIC 9(06). 
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
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-COMRENNUM           PIC 9(05).
      *
           03  WRK-KOHBUNRI-MIN        PIC 9(01).
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
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *
       01  WRK-RECE642-REC.
           COPY    "CPRCF0642.INC" REPLACING  //RECE642//
                                   BY         //WRK-RECE642//.
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
      *
           MOVE    ZERO                TO  FLG-END
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
           MOVE    "RECE620"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE640"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE650"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE642"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE64W"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE62PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE64PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE65PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE642PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE64WPARA
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
               MOVE    "ORCR0655"      TO  JOB-PGID
               MOVE    "同一剤のまとめ"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
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
      *    同一剤のまとめ処理
           OPEN    INPUT               RECE64-FILE
           IF      STS-RECE64          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE64PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE64          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE65-FILE
           IF      STS-RECE65          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE65PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE65          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE62-FILE
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE642-FILE
           CLOSE                       RECE642-FILE
           OPEN    I-O                 RECE642-FILE
      *
           PERFORM 900-RECE62-READ-SEC
      *
           PERFORM 210-RECE642-WRITE-SEC
                                   UNTIL   FLG-RECE62  =   1
           CLOSE   RECE642-FILE
                   RECE64-FILE
                   RECE65-FILE
                   RECE62-FILE
      *
      *    元のレイアウトに出力処理
           OPEN    INPUT   RECE642-FILE
           IF      STS-RECE642         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE642PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE642         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
            OPEN   OUTPUT  RECE64W-FILE
      *
           PERFORM 900-RECE642-READ-SEC 
           PERFORM 220-RECE64W-WRITE-SEC
                                   UNTIL   FLG-RECE642 =   1
           CLOSE   RECE64W-FILE
                   RECE642-FILE
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
       210-RECE642-WRITE-SEC                SECTION.
      *
           MOVE       9                TO  WRK-KOHBUNRI-MIN
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE64-REC
           MOVE    RECE62-KEY          TO  RECE64-KEY1
           MOVE    ZERO                TO  RECE64-ZAINUM
      *
           START   RECE64-FILE
               KEY IS    >=   RECE64-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE64
               NOT INVALID
                   PERFORM 900-RECE64-READ-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE64           =   1                   )
               OR     ( RECE62-KEY       NOT =   RECE64-KEY1         )
      *
               MOVE    RECE64-ZAINUM     TO      WRK-ZAINUM
               MOVE    SPACE             TO      WRK-SRYSYUKBN
               IF       RECE64-SRYKBN  =  "60" OR "64" OR "99"
                   PERFORM   2101-RECE65-SRYSYUKBN-HEN-SEC
               END-IF
      *
               INITIALIZE          WRK-COM-AREA
               IF       RECE64-COMCDFLG   =   "1"
                   PERFORM   2101-RECE65-COM-HENSYU-SEC
               END-IF
      *
               MOVE    SPACE               TO  RECE642-REC
               INITIALIZE                      RECE642-REC
               MOVE    RECE64-KEY1         TO  RECE642-KEY1
               MOVE    RECE64-KOHBUNRI     TO  RECE642-KOHBUNRI
               MOVE    RECE64-SRYKBN       TO  RECE642-SRYKBN
               MOVE    WRK-SRYSYUKBN       TO  RECE642-SRYSYUKBN
               MOVE    RECE64-ZAITEN       TO  RECE642-ZAITEN
               MOVE    RECE64-SRYCDTOTAL   TO  RECE642-SRYCDTOTAL
               MOVE    RECE64-SURYOUTOTAL  TO  RECE642-SURYOUTOTAL
               MOVE    RECE64-RUIKEITOTAL  TO  RECE642-RUIKEITOTAL
               MOVE    RECE64-MEISAISU     TO  RECE642-MEISAISU 
               MOVE    RECE64-JIHOKBN      TO  RECE642-JIHOKBN
               MOVE    RECE64-JISANYAKKBN  TO  RECE642-JISANYAKKBN
               IF      RECE64-COMCDFLG   =   "1"
                 ADD     1                 TO  WRK-COMRENNUM
                 MOVE    WRK-COMRENNUM     TO  RECE642-COMRENNUM
      *
                 MOVE    RECE642-REC         TO  WRK-RECE642-REC
                 PERFORM 900-RECE642-INV2-SEC
                 IF      FLG-MATOME    =   ZERO
                     MOVE    WRK-RECE642-REC     TO  RECE642-REC
                     MOVE    RECE64-REC          TO  RECE642-REC1
                     MOVE    WRK-COM-AREA        TO  RECE642-COM-AREA
                 END-IF
      *          剤回数・日数回数テーブルの加算
                 ADD     RECE64-ZAIKAISU     TO  RECE642-ZAIKAISU
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       4
                     PERFORM VARYING IDY FROM    1   BY  1
                             UNTIL   IDY >       31
                         ADD     RECE64-KAISU (IDX IDY)
                                         TO   RECE642-KAISU (IDX IDY)
                     END-PERFORM
                 END-PERFORM
                 PERFORM VARYING IDY FROM    1   BY  1
                         UNTIL   IDY >       31
                     IF  RECE64-HKNCOMBI-DAY(IDY) NOT = ZERO
                     AND RECE642-HKNCOMBI-DAY(IDY)    = ZERO
                         MOVE    RECE64-HKNCOMBI-DAY(IDY)
                                         TO   RECE642-HKNCOMBI-DAY(IDY)
                     END-IF
                 END-PERFORM
      *
                 IF  RECE64-SYODOU-SRYKA(1)  NOT =  SPACE
                   MOVE     ZERO     TO    FLG-ARI
                   PERFORM  VARYING  IDX   FROM    1   BY  1
                             UNTIL IDX                       > 10
                              OR   RECE642-SYODOU-SRYKA(IDX) = SPACE
                              OR   FLG-ARI                   = 1
                      IF  RECE64-SYODOU-SRYKA(1)
                                 =  RECE642-SYODOU-SRYKA(IDX)
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-PERFORM
                   IF  FLG-ARI  =  ZERO
                   AND IDX     <=  10
                          MOVE   RECE64-SYODOU-SRYKA(1)
                                    TO   RECE642-SYODOU-SRYKA(IDX)
                   END-IF
                 END-IF
      *
                 IF      FLG-MATOME    =   ZERO
                     WRITE   RECE642-REC
                     ADD     1                   TO  CNT-RECE642
                 ELSE
                     REWRITE RECE642-REC
                 END-IF
               ELSE
                 MOVE    RECE642-REC         TO  WRK-RECE642-REC
                 PERFORM 900-RECE642-INV-SEC
                 IF      FLG-RECE642         =   1
                     MOVE    WRK-RECE642-REC     TO  RECE642-REC
                     MOVE    RECE64-REC          TO  RECE642-REC1
                 END-IF
      *          剤回数・日数回数テーブルの加算
                 ADD     RECE64-ZAIKAISU     TO  RECE642-ZAIKAISU
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       4
                     PERFORM VARYING IDY FROM    1   BY  1
                             UNTIL   IDY >       31
                         ADD     RECE64-KAISU (IDX IDY)
                                         TO   RECE642-KAISU (IDX IDY)
                     END-PERFORM
                 END-PERFORM
                 PERFORM VARYING IDY FROM    1   BY  1
                         UNTIL   IDY >       31
                     IF  RECE64-HKNCOMBI-DAY(IDY) NOT = ZERO
                     AND RECE642-HKNCOMBI-DAY(IDY)    = ZERO
                         MOVE    RECE64-HKNCOMBI-DAY(IDY)
                                         TO   RECE642-HKNCOMBI-DAY(IDY)
                     END-IF
                 END-PERFORM
      *
                 IF  RECE64-SYODOU-SRYKA(1)  NOT =  SPACE
                   MOVE     ZERO     TO    FLG-ARI
                   PERFORM  VARYING  IDX   FROM    1   BY  1
                             UNTIL IDX                       > 10
                              OR   RECE642-SYODOU-SRYKA(IDX) = SPACE
                              OR   FLG-ARI                   = 1
                      IF  RECE64-SYODOU-SRYKA(1)
                                 =  RECE642-SYODOU-SRYKA(IDX)
                          MOVE  1   TO   FLG-ARI
                      END-IF
                   END-PERFORM
                   IF  FLG-ARI  =  ZERO
                   AND IDX     <=  10
                          MOVE   RECE64-SYODOU-SRYKA(1)
                                    TO   RECE642-SYODOU-SRYKA(IDX)
                   END-IF
                 END-IF
      *
                 IF      FLG-RECE642         =   1
                     WRITE   RECE642-REC
                     ADD     1                   TO  CNT-RECE642
                 ELSE
                     REWRITE RECE642-REC
                 END-IF
               END-IF
      *
               IF   WRK-KOHBUNRI-MIN  >  RECE64-KOHBUNRI
                 IF  (RECE64-ZAITEN  NOT = ZERO         )
                 AND (RECE64-SRYKBN  NOT = "95" AND "96")
                   MOVE   RECE64-KOHBUNRI  TO  WRK-KOHBUNRI-MIN
                 END-IF
               END-IF
      *
               PERFORM     900-RECE64-READ-SEC
           END-PERFORM
      *
           MOVE      WRK-KOHBUNRI-MIN   TO   RECE62-KOHBUNRI-MIN
           REWRITE   RECE62-REC
      *
      *
           PERFORM 900-RECE62-READ-SEC
           .
       210-RECE642-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分取得
      *****************************************************************
       2101-RECE65-SRYSYUKBN-HEN-SEC         SECTION.
      *
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-KEY           TO      RECE65-KEY1
           MOVE    WRK-ZAINUM           TO      RECE65-ZAINUM
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE65
               NOT INVALID
                   PERFORM  900-RECE65-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE62-KEY           NOT =  RECE65-KEY1  )    OR
                  (WRK-ZAINUM           NOT =  RECE65-ZAINUM)    OR
                  (FLG-RECE65               =      1        )
      *
                   MOVE   RECE65-SRYSYUKBN   TO   WRK-SRYSYUKBN
      *
                   MOVE   1                  TO   FLG-RECE65
      *
           END-PERFORM
      *
           .
       2101-RECE65-SRYSYUKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集
      *****************************************************************
       2101-RECE65-COM-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                 TO      IDZ2
      *
           INITIALIZE                           RECE65-REC
           MOVE    RECE62-KEY           TO      RECE65-KEY1
           MOVE    WRK-ZAINUM           TO      RECE65-ZAINUM
           START   RECE65-FILE
               KEY IS    >=   RECE65-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE65
               NOT INVALID
                   PERFORM  900-RECE65-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE62-KEY           NOT =  RECE65-KEY1  )    OR
                  (WRK-ZAINUM           NOT =  RECE65-ZAINUM)    OR
                  (FLG-RECE65               =      1        )
      *
                  PERFORM  VARYING   IDZ   FROM    1    BY   1
                            UNTIL   (IDZ                >       5)  OR
                                    (RECE65-SRYCD(IDZ)  =   SPACE)
      *
                     IF  (RECE65-SRYCD(IDZ)(1:1)  =   "8"  )  OR
                         (RECE65-SRYCD(IDZ)(1:3)  =   "008")
                       IF   (RECE65-RECEPTFLG(IDZ)   =  "1")
                       AND  (RECE65-RECEDENFLG(IDZ)  =  "1")
                         CONTINUE
                       ELSE
                         ADD       1                 TO  IDZ2
                         IF   IDZ2  <=  10
                            MOVE   RECE65-NAME(IDZ)  TO  WRK-COM(IDZ2)
                         END-IF
                       END-IF
                     END-IF
      *
                  END-PERFORM
      *
                  PERFORM  900-RECE65-READ-SEC
      *
           END-PERFORM
      *
           .
       2101-RECE65-COM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       220-RECE64W-WRITE-SEC                SECTION.
      *
           MOVE    RECE642-REC1        TO  RECE64W-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE642-ZAIKAISU     TO  RECE64W-ZAIKAISU
           MOVE    RECE642-DAY-AREA     TO  RECE64W-DAY-AREA
           MOVE    RECE642-HKNCOMBI-DAY-AREA
                                        TO  RECE64W-HKNCOMBI-DAY-AREA
      *
           MOVE    RECE642-SYODOU-SRYKA-AREA
                                        TO  RECE64W-SYODOU-SRYKA-AREA
      *
           WRITE   RECE64W-REC
           ADD     1                   TO  CNT-RECE64W
      *
           PERFORM     900-RECE642-READ-SEC
           .
       220-RECE64W-WRITE-EXT.
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
           STRING  "ORCR0655 "             DELIMITED   BY  SIZE
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE64     :" CNT-IN   
           DISPLAY "RECE642    :" CNT-RECE642
           DISPLAY "RECE64W    :" CNT-RECE64W
           DISPLAY "*** ORCR0655 END ***"
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
      *    レセプト０２データ読込処理
      *****************************************************************
       900-RECE62-READ-SEC           SECTION.
      *
           READ    RECE62-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE62
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE62
      *
                   DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
           END-READ
           .
       900-RECE62-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   レセプト０４データ読込処理
      *****************************************************************
       900-RECE64-READ-SEC            SECTION.
      *
           READ    RECE64-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE64
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE64
                   ADD     1             TO    CNT-IN
           END-READ
           .
       900-RECE64-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５データ読込処理
      *****************************************************************
       900-RECE65-READ-SEC            SECTION.
      *
           READ    RECE65-FILE   NEXT
               AT  END
                   MOVE    1      TO    FLG-RECE65
               NOT AT  END
                   MOVE    ZERO   TO    FLG-RECE65
           END-READ
           .
       900-RECE65-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE642-READ-SEC            SECTION.
      *
           READ    RECE642-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE642
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE642
           END-READ
           .
       900-RECE642-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE642-INV-SEC            SECTION.
      *
           READ    RECE642-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE642
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE642
           END-READ
           .
       900-RECE642-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE642-INV2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MATOME
      *
           INITIALIZE                      RECE642-REC
           MOVE    RECE64-KEY1         TO  RECE642-KEY1
           MOVE    RECE64-KOHBUNRI     TO  RECE642-KOHBUNRI
           MOVE    RECE64-SRYKBN       TO  RECE642-SRYKBN
           MOVE    WRK-SRYSYUKBN       TO  RECE642-SRYSYUKBN
           MOVE    RECE64-ZAITEN       TO  RECE642-ZAITEN
           MOVE    RECE64-SRYCDTOTAL   TO  RECE642-SRYCDTOTAL
           MOVE    RECE64-SURYOUTOTAL  TO  RECE642-SURYOUTOTAL
           MOVE    RECE64-RUIKEITOTAL  TO  RECE642-RUIKEITOTAL
           MOVE    RECE64-MEISAISU     TO  RECE642-MEISAISU
           MOVE    RECE64-JIHOKBN      TO  RECE642-JIHOKBN
           MOVE    RECE64-JISANYAKKBN  TO  RECE642-JISANYAKKBN
           START   RECE642-FILE
               KEY IS    >=   RECE642-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE642
               NOT INVALID
                   PERFORM  900-RECE642-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE64-KEY1         NOT =  RECE642-KEY1    )     OR
                  (RECE64-KOHBUNRI     NOT =  RECE642-KOHBUNRI)     OR
                  (RECE64-SRYKBN       NOT =  RECE642-SRYKBN)       OR
                  (WRK-SRYSYUKBN       NOT =  RECE642-SRYSYUKBN)    OR
                  (RECE64-ZAITEN       NOT =  RECE642-ZAITEN)       OR
                  (RECE64-SRYCDTOTAL   NOT =  RECE642-SRYCDTOTAL)   OR
                  (RECE64-SURYOUTOTAL  NOT =  RECE642-SURYOUTOTAL)  OR
                  (RECE64-RUIKEITOTAL  NOT =  RECE642-RUIKEITOTAL)  OR
                  (RECE64-MEISAISU     NOT =  RECE642-MEISAISU)     OR
                  (RECE64-JIHOKBN      NOT =  RECE642-JIHOKBN)      OR
                  (RECE64-JISANYAKKBN  NOT =  RECE642-JISANYAKKBN)  OR
                  (FLG-RECE642             =  1)
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
                                    (RECE642-COM(IDZ2)  =   SPACE)
                        IF  RECE642-COM(IDZ2)  =  WRK-COM(IDZ)
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
                     PERFORM  900-RECE642-READ-SEC
                  ELSE
                     MOVE    1    TO   FLG-RECE642
                     MOVE    1    TO   FLG-MATOME
                  END-IF
      *
           END-PERFORM
      *
           .
       900-RECE642-INV2-EXT.
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
