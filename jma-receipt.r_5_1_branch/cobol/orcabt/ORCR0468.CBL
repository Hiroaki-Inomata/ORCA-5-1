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
       PROGRAM-ID.             ORCR0468.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      金額摘要欄編集（自賠責）（従来様式）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/07/27    NaCl-門脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-門脇    06/12/01  点数・回数編集修正
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.02.01    NACL-門脇    08/04/21  ８０その他記載順対応
      *  04.03.01    NACL-門脇    09/01/23  円摘要欄編集修正
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    09/06/18  円摘要欄コメント編集対応
      *
      *  04.07.01    NACL-門脇    13/10/02  自賠責（第三者行為）対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *
           SELECT  RECE47-FILE     ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE47-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE46-FILE.
       01  RECE46-REC.
           COPY    "CPRCF0461.INC" REPLACING  //RECE461//
                                   BY         //RECE46//.
      *
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF047.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE01//.
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
           03  STS-RECE41          PIC X(02).
           03  STS-RECE47          PIC X(02).
           03  STS-RECE46          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE46          PIC 9(01).
           03  FLG-RECE47          PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-HENSYU2         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE47          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-LINENUM         PIC 9(05).
      *    剤印字行数
           03  WRK-TBLMAX          PIC 9(03).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-KINGAKU         PIC 9(09)V9(02).
      *
      *    点数・回数編集用
           03  WRK-ZAITENX                     PIC ZZZZZZZZ9.99.
           03  WRK-ZAIKAISUX                   PIC ZZZ9.
           03  WRK-TNSKAI-AREA.
               05  WRK-TNSKAI-ST               PIC 9(03).
               05  WRK-TNSKAI-KETA             PIC 9(02).
               05  WRK-TNSKAI-LEN              PIC 9(02).
      *
               05  WRK-TNSKAI.
                   07  WRK-ZAITEN              PIC X(12).
                   07  WRK-X                   PIC X(02).
                   07  WRK-ZAIKAISU            PIC X(06).
      *
           03  WRK-ST                          PIC 9(03).
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
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    剤・診療行為テーブル
       01  TBL-ZAI-AREA.
           03  TBL-ZAI-OCC                     OCCURS 100.
               05  TBL-ZAI-NAME                PIC X(52).
      *
       01  WRK-RECE46-REC.
           COPY    "CPRCF0461.INC" REPLACING  //RECE461//
                                   BY         //WRK-RECE46//.
      *
      *    固定値
       01  CONST-AREA.
      *    適用欄最大文字数（点数欄）
           03  CONS-TEKIYOU2-LEN           PIC 9(02)   VALUE   52.
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0468"      TO  JOB-PGID
               MOVE    "金額摘要欄編集（労災・自賠責（従来様式））"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECE01PARA
           INITIALIZE                      RECE01PARA
           MOVE    "RECE410"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE46PARA
           INITIALIZE                      RECE46PARA
           MOVE    "RECE461"           TO  RECE46PARA-FILE-ID
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
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE46PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE47PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE46PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE47PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    INPUT               RECE41-FILE
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE01PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE46-FILE
           IF      STS-RECE46          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE46PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE46          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE47-FILE
           IF      STS-RECE47          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE47PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE47          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO       TO     WRK-LINENUM
           INITIALIZE                TBL-ZAI-AREA
      *
      *    外来管理加算（特例５２）の編集
           IF      RECE41-GAITOKU-PRTKBN1  =   "1"
               MOVE    1                   TO  RECE46-ZAILAST
               INITIALIZE                      TBL-ZAI-AREA
               MOVE    RECE41-GAITOKU-TEN1 TO  WRK-ZAITENX
                                               WRK-KINGAKU
               MOVE    RECE41-GAITOKU-KAISU1
                                           TO  WRK-ZAIKAISUX
               PERFORM 2001-TENSUKAI-HEN-SEC
               MOVE    "＊（特）"          TO  TBL-ZAI-NAME(1)(1:8)
               MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                           TO
                                        TBL-ZAI-NAME(1)(WRK-TNSKAI-ST:)
      *
               MOVE    1                 TO  WRK-TBLMAX
               MOVE    RECE41-KEY        TO  WRK-RECE46-KEY
      ***      MOVE    "99"              TO  WRK-RECE46-SRYKBN
               MOVE    ZERO              TO  WRK-RECE46-GAITENTTLKBN
               MOVE    SPACE             TO  WRK-RECE46-SRYCD1
               MOVE    99999986          TO  WRK-RECE46-ZAINUM
               MOVE    RECE41-PTNUM      TO  WRK-RECE46-PTNUM
               MOVE    SPACE             TO  WRK-RECE46-SRYKA
      *
               PERFORM 2002-RECE47-OUT-SEC
           END-IF
      *
      *    外来管理加算（特例２６）の編集
           IF      RECE41-GAITOKU-PRTKBN2  =   "1"
               MOVE    1                   TO  RECE46-ZAILAST
               INITIALIZE                      TBL-ZAI-AREA
               MOVE    RECE41-GAITOKU-TEN2 TO  WRK-ZAITENX
                                               WRK-KINGAKU
               MOVE    RECE41-GAITOKU-KAISU2
                                           TO  WRK-ZAIKAISUX
               PERFORM 2001-TENSUKAI-HEN-SEC
               MOVE    "＊（特）"          TO  TBL-ZAI-NAME(1)(1:8)
               MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                           TO
                                        TBL-ZAI-NAME(1)(WRK-TNSKAI-ST:)
      *
               MOVE    1                 TO  WRK-TBLMAX
               MOVE    RECE41-KEY        TO  WRK-RECE46-KEY
      ***      MOVE    "99"              TO  WRK-RECE46-SRYKBN
               MOVE    ZERO              TO  WRK-RECE46-GAITENTTLKBN
               MOVE    SPACE             TO  WRK-RECE46-SRYCD1
               MOVE    99999987          TO  WRK-RECE46-ZAINUM
               MOVE    RECE41-PTNUM      TO  WRK-RECE46-PTNUM
               MOVE    SPACE             TO  WRK-RECE46-SRYKA
      *
               PERFORM 2002-RECE47-OUT-SEC
           END-IF
      *
           MOVE        ZERO              TO  WRK-ZAINUM
           MOVE        ZERO              TO  WRK-TBLMAX
                                             WRK-KINGAKU
           INITIALIZE                        TBL-ZAI-AREA
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE46-REC
           MOVE    RECE41-HOSPNUM      TO  RECE46-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE46-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE46-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE46-PTID
           MOVE    RECE41-RECEKA       TO  RECE46-RECEKA
           MOVE    RECE41-HKNID        TO  RECE46-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE46-TEKSTYMD
      *
           START   RECE46-FILE
               KEY IS    >=   RECE46-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE46
               NOT INVALID
                   PERFORM 990-RECE46-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                )
               OR     ( FLG-RECE46           =   1                )
               OR     ( RECE41-HOSPNUM   NOT =   RECE46-HOSPNUM   )
               OR     ( RECE41-SRYYM     NOT =   RECE46-SRYYM     )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE46-NYUGAIKBN )
               OR     ( RECE41-PTID      NOT =   RECE46-PTID      )
               OR     ( RECE41-RECEKA    NOT =   RECE46-RECEKA    )
               OR     ( RECE41-HKNID     NOT =   RECE46-HKNID     )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE46-TEKSTYMD  )
      *
                   IF      RECE46-ZAINUM   NOT =   WRK-ZAINUM
                      IF      WRK-ZAINUM   NOT =   ZERO
                          PERFORM 2002-RECE47-OUT-SEC
                      END-IF
                      MOVE    RECE46-REC     TO    WRK-RECE46-REC
                      MOVE    RECE46-ZAINUM  TO    WRK-ZAINUM
                      MOVE    ZERO           TO    WRK-TBLMAX
                                                   WRK-KINGAKU
                      INITIALIZE                   TBL-ZAI-AREA
                   END-IF
      *
                   ADD     RECE46-TEN        TO    WRK-KINGAKU
                   MOVE    WRK-KINGAKU       TO    WRK-ZAITENX
                   MOVE    RECE46-ZAIKAISU   TO    WRK-ZAIKAISUX
      *
                   PERFORM 2001-TENSUKAI-HEN-SEC
      *
      *            明細編集・明細桁数計算
                   PERFORM 2003-MEISAI-KETA-HEN-SEC
      *
      *
                   PERFORM 990-RECE46-NEXT-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
           END-PERFORM
      *
           IF      FLG-END         =   ZERO
      *
               IF    WRK-ZAINUM       NOT =   ZERO
               AND   TBL-ZAI-NAME(1)  NOT =   SPACE
                     PERFORM   2002-RECE47-OUT-SEC
               END-IF
      *
               PERFORM 990-RECE41-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額・回数編集処理
      *****************************************************************
       2001-TENSUKAI-HEN-SEC    SECTION.
      *
           INITIALIZE              WRK-TNSKAI-AREA
      *
           IF   (RECE46-ZAILAST   =   ZERO)
           OR   (WRK-KINGAKU      =   ZERO)
               GO  TO  2001-TENSUKAI-HEN-EXT
           END-IF
      *
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-ZAITENX(4:6)
                                   TO  KANACHK-MAE-INPUT (1:6)
           MOVE    "X"             TO  KANACHK-MAE-INPUT (7:1)
           MOVE    WRK-ZAIKAISUX (2:3)
                                   TO  KANACHK-MAE-INPUT (8:3)
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
           AND     KANACHK-MAX     >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-TNSKAI
               MOVE    "×"            TO  WRK-X
           END-IF
      *
      *    必要有効桁数（点数ZZZZZ X 数量ZZZ の全角の桁数）
           MOVE    ZERO                TO  WRK-TNSKAI-KETA
           PERFORM VARYING   IDZ     FROM    1   BY  2
                   UNTIL     IDZ     >       20
      ***>     IF      WRK-TNSKAI (IDZ:2)      NOT =   SPACE
               IF      WRK-TNSKAI (IDZ:2)      NOT =   "　"
                   IF   IDZ = 1
                      COMPUTE WRK-TNSKAI-KETA = 20 - IDZ + 1
                      MOVE    IDZ   TO   WRK-TNSKAI-LEN
                   ELSE
                      COMPUTE WRK-TNSKAI-KETA = 20 - IDZ + 1 + 2
                      COMPUTE WRK-TNSKAI-LEN  = IDZ  -  2
                   END-IF
                   MOVE    20                  TO  IDZ
               END-IF
           END-PERFORM
      *
      *    印刷開始位置を取得
           COMPUTE   WRK-TNSKAI-ST     =   ( CONS-TEKIYOU2-LEN  -
                                             WRK-TNSKAI-KETA      )
                                       +   1
      *
           .
       2001-TENSUKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６出力処理
      *****************************************************************
       2002-RECE47-OUT-SEC    SECTION.
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL    IDX        >   WRK-TBLMAX
                        OR     IDX        >   100
                        OR     FLG-END    =     1
      *
             ADD     1                     TO  WRK-LINENUM
             MOVE    SPACE                 TO  RECE47-REC
             INITIALIZE                        RECE47-REC
      *
             MOVE    WRK-RECE46-HOSPNUM    TO  RECE47-HOSPNUM
             MOVE    WRK-RECE46-SRYYM      TO  RECE47-SRYYM
             MOVE    WRK-RECE46-NYUGAIKBN  TO  RECE47-NYUGAIKBN
             MOVE    WRK-RECE46-PTID       TO  RECE47-PTID
             MOVE    WRK-RECE46-RECEKA     TO  RECE47-RECEKA
             MOVE    WRK-RECE46-HKNID      TO  RECE47-HKNID
             MOVE    WRK-RECE46-TEKSTYMD   TO  RECE47-TEKSTYMD
             IF   RECE41-HKNKBN  =  "5"
               MOVE  "10"                  TO  RECE47-SRYKBN
             ELSE
      ***      MOVE  "99"                  TO  RECE47-SRYKBN
               MOVE  "98"                  TO  RECE47-SRYKBN
             END-IF
             MOVE    ZERO                  TO  RECE47-KNSJISKBN
             MOVE    ZERO                  TO  RECE47-SYOHOUFLG
             MOVE    SPACE                 TO  RECE47-SRYSYUKBN
             MOVE    ZERO                  TO  RECE47-DAY
             MOVE    SPACE                 TO  RECE47-OKIKAEFLG
             MOVE    9                     TO  RECE47-CDKBN-SYO
             MOVE    99                    TO  RECE47-CDKBN-BU
             MOVE    "9"                   TO  RECE47-CDKBN-KBN
             MOVE    999                   TO  RECE47-CDKBN-KBNNUM
             MOVE    99                    TO  RECE47-CDKBN-KBNNUM-EDA
             MOVE    99                    TO  RECE47-CDKBN-KOUBAN
             MOVE    999999999             TO  RECE47-KOUHYOJUNNUM
             MOVE    WRK-RECE46-GAITENTTLKBN
                                           TO  RECE47-GAITENTTLKBN
             MOVE    WRK-RECE46-SRYCD1     TO  RECE47-SRYCD1
             MOVE    ZERO                  TO  RECE47-ZAITEN
             MOVE    WRK-RECE46-ZAINUM     TO  RECE47-ZAINUM
             MOVE    WRK-LINENUM           TO  RECE47-LINENUM
      *
             MOVE    WRK-TBLMAX            TO  RECE47-ZAIMEISAISU
             MOVE    WRK-RECE46-PTNUM      TO  RECE47-PTNUM
             MOVE    WRK-RECE46-SRYKA      TO  RECE47-SRYKA
             MOVE    TBL-ZAI-NAME(IDX)     TO  RECE47-TEKIYO-INF
      *
             MOVE    SPACE                 TO  RECE47-JIHOKBN
      *
             READ    RECE47-FILE
               INVALID
                   MOVE    1               TO  FLG-RECE47
               NOT INVALID
                   MOVE    ZERO            TO  FLG-RECE47
             END-READ
             IF      FLG-RECE47            =   1
                 WRITE   RECE47-REC
                 IF      STS-RECE47          =   "00"
                     ADD     1             TO  CNT-RECE47
                 ELSE
                     MOVE    SPACE               TO  WRK-ERR-AREA
                     INITIALIZE                      WRK-ERR-AREA
      *
                     MOVE    3                   TO  WRK-ERR-FLG
                     MOVE    RECE47PARA          TO  WRK-ERR-FILE-NM
                     MOVE    STS-RECE47          TO  WRK-ERR-FILE-STS
                     PERFORM 500-FILE-ERR-ABORT-SEC
                 END-IF
             ELSE
                 DISPLAY "*** ORCR0468 RECE47 KEY CYOFUKU  ***"
             END-IF
      *
           END-PERFORM
      *
           .
       2002-RECE47-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集・明細桁数計算
      *****************************************************************
       2003-MEISAI-KETA-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  WRK-ST
      *
           MOVE    ZERO                TO  FLG-HENSYU
                                           FLG-HENSYU2
      *
           PERFORM   UNTIL   FLG-HENSYU  =   1
      *
               ADD     1               TO  WRK-TBLMAX
               IF  WRK-TBLMAX          >   100
                   DISPLAY "*** ORCR0468 INDEX[100] OVER  ***"
               END-IF
      *
               IF    WRK-TBLMAX    =    1
               AND   FLG-HENSYU2   =    ZERO
                   MOVE   "＊"  TO   TBL-ZAI-NAME(WRK-TBLMAX)(1:2)
                   MOVE    1    TO   FLG-HENSYU2
               END-IF
               MOVE  RECE46-NAME(WRK-ST:)  TO
                                     TBL-ZAI-NAME(WRK-TBLMAX)(3:)
               IF      WRK-TNSKAI      NOT =   SPACE
                   IF   TBL-ZAI-NAME(WRK-TBLMAX)(WRK-TNSKAI-ST:)
                                                          =   SPACE
                       MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                            TO  TBL-ZAI-NAME(WRK-TBLMAX)(WRK-TNSKAI-ST:)
                       MOVE    1    TO   FLG-HENSYU
                   END-IF
               END-IF
      *
               COMPUTE  WRK-ST  =  WRK-ST  +  CONS-TEKIYOU2-LEN  - 2
      *
      *        金額の編集がないときまだ印刷する文字があるか
               IF      WRK-TNSKAI          =   SPACE
                   IF      RECE46-NAME (WRK-ST:)   =   SPACE
                       MOVE    1               TO  FLG-HENSYU
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2003-MEISAI-KETA-HEN-EXT.
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
      *        ジョブ管理開始処理
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
           STRING  "ORCR0468 "             DELIMITED   BY  SIZE
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE41-FILE
           CLOSE                       RECE47-FILE
           CLOSE                       RECE46-FILE
      *
      *    ステップ管理終了処理
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41  IN      " CNT-RECE41
           DISPLAY "RECE47  ADDCNT  " CNT-RECE47
           DISPLAY "*** ORCR0468 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   IF      STS-RECE41          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE01PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE41      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1           TO  CNT-RECE41
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                   IF     (WRK-PARA-RECEKBN       =  "5")
                   OR     (RECE41-HKNKBN          =  "5")
                   OR     (RECE41-HKNKBN          =  "6")
                       CONTINUE
                   ELSE
                       GO  TO  990-RECE41-READ-SEC
                   END-IF
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE46-NEXT-SEC           SECTION.
      *
           READ    RECE46-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE46
               NOT AT  END
                   IF      STS-RECE46          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE46PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE46      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE46
                   ADD     1         TO  CNT-IN-CANCEL
           END-READ
      *
           .
       990-RECE46-NEXT-EXT.
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
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
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
