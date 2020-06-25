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
       PROGRAM-ID.             ORCR0936.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（自賠責）
      *  コンポーネント名  : 月次業務　自賠責明細書ファイル作成
      *                      金額摘要欄編集（自賠責）（従来様式）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/07/27    NaCl-門脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-門脇    06/12/01  点数・回数編集修正
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.01.01    NACL-門脇    07/11/01  公務災害対応
      *  04.03.01    NACL-門脇    09/01/23  円摘要欄編集修正
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    09/06/18  円摘要欄コメント編集対応
      *  04.05.03    NACL-門脇    13/02/07  金額項目７桁対応
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
           SELECT  RECE82-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE86-FILE     ASSIGN  RECE86PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE86-KEY
                                   FILE    STATUS  IS  STS-RECE86.
      *
           SELECT  RECE87-FILE     ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE87-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
       FD  RECE86-FILE.
       01  RECE86-REC.
           COPY    "CPRCF0861.INC" REPLACING  //RECE861//
                                   BY         //RECE86//.
      *
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF087.INC".
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
           03  STS-RECE82          PIC X(02).
           03  STS-RECE87          PIC X(02).
           03  STS-RECE86          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE86          PIC 9(01).
           03  FLG-RECE87          PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-HENSYU2         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE87          PIC 9(06).
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
           03  WRK-RECEERR         PIC X(200).
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
                   07  WRK-ZAITEN              PIC X(14).
                   07  WRK-X                   PIC X(02).
                   07  WRK-ZAIKAISU            PIC X(06).
      *
           03  WRK-ST                          PIC 9(03).
      *
      *    剤・診療行為テーブル
       01  TBL-ZAI-AREA.
           03  TBL-ZAI-OCC                     OCCURS 100.
               05  TBL-ZAI-NAME                PIC X(52).
      *
       01  WRK-RECE86-REC.
           COPY    "CPRCF0861.INC" REPLACING  //RECE861//
                                   BY         //WRK-RECE86//.
      *
      *    固定値
       01  CONST-AREA.
      *    適用欄最大文字数（点数欄）
           03  CONS-TEKIYOU2-LEN           PIC 9(02)   VALUE   52.
      *
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
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0936"          TO  JOB-PGID
               MOVE    "金額摘要欄編集（労災・自賠責（従来様式））"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           PERFORM 990-RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE01PARA
           INITIALIZE                      RECE01PARA
           MOVE    "RECE820"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE86PARA
           INITIALIZE                      RECE86PARA
           MOVE    "RECE861"           TO  RECE86PARA-FILE-ID
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
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE86PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE86PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE87PARA
      *
           OPEN    INPUT               RECE82-FILE
      *
           OPEN    INPUT               RECE86-FILE
      *
           OPEN    I-O                 RECE87-FILE
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
      *
           MOVE    ZERO       TO     WRK-ZAINUM
           MOVE    ZERO       TO     WRK-TBLMAX
                                     WRK-KINGAKU
           INITIALIZE                TBL-ZAI-AREA
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE86-REC
           MOVE    RECE82-HOSPNUM      TO  RECE86-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE86-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE86-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE86-PTID
           MOVE    RECE82-RECEKA       TO  RECE86-RECEKA
           MOVE    RECE82-HKNID        TO  RECE86-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE86-TEKSTYMD
      *
           START   RECE86-FILE
               KEY IS    >=   RECE86-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE86
               NOT INVALID
                   PERFORM 990-RECE86-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                )
               OR     ( FLG-RECE86           =   1                )
               OR     ( RECE82-HOSPNUM   NOT =   RECE86-HOSPNUM   )
               OR     ( RECE82-SRYYM     NOT =   RECE86-SRYYM     )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE86-NYUGAIKBN )
               OR     ( RECE82-PTID      NOT =   RECE86-PTID      )
               OR     ( RECE82-RECEKA    NOT =   RECE86-RECEKA    )
               OR     ( RECE82-HKNID     NOT =   RECE86-HKNID     )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE86-TEKSTYMD  )
      *
                   IF      RECE86-ZAINUM   NOT =   WRK-ZAINUM
                      IF      WRK-ZAINUM   NOT =   ZERO
                          PERFORM 2002-RECE87-OUT-SEC
                      END-IF
                      MOVE    RECE86-REC     TO    WRK-RECE86-REC
                      MOVE    RECE86-ZAINUM  TO    WRK-ZAINUM
                      MOVE    ZERO           TO    WRK-TBLMAX
                                                   WRK-KINGAKU
                      INITIALIZE                   TBL-ZAI-AREA
                   END-IF
      *
                   ADD     RECE86-TEN        TO    WRK-KINGAKU
                   MOVE    WRK-KINGAKU       TO    WRK-ZAITENX
                   MOVE    RECE86-ZAIKAISU   TO    WRK-ZAIKAISUX
      *
                   PERFORM 2001-TENSUKAI-HEN-SEC
      *
      *            明細編集・明細桁数計算
                   PERFORM 2003-MEISAI-KETA-HEN-SEC
      *
      *
                   PERFORM 990-RECE86-NEXT-SEC
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
                     PERFORM   2002-RECE87-OUT-SEC
               END-IF
      *
               PERFORM 990-RECE82-READ-SEC
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
           IF   (RECE86-ZAILAST   =   ZERO)
           OR   (WRK-KINGAKU      =   ZERO)
               GO  TO  2001-TENSUKAI-HEN-EXT
           END-IF
      *
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-ZAITENX(3:7)
                                   TO  KANACHK-MAE-INPUT (1:7)
           MOVE    "X"             TO  KANACHK-MAE-INPUT (8:1)
           MOVE    WRK-ZAIKAISUX (2:3)
                                   TO  KANACHK-MAE-INPUT (9:3)
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
                   UNTIL     IDZ     >       22
      ***>     IF      WRK-TNSKAI (IDZ:2)      NOT =   SPACE
               IF      WRK-TNSKAI (IDZ:2)      NOT =   "　"
                   IF   IDZ = 1
                      COMPUTE WRK-TNSKAI-KETA = 22 - IDZ + 1
                      MOVE    IDZ   TO   WRK-TNSKAI-LEN
                   ELSE
                      COMPUTE WRK-TNSKAI-KETA = 22 - IDZ + 1 + 2
                      COMPUTE WRK-TNSKAI-LEN  = IDZ  -  2
                   END-IF
                   MOVE    22                  TO  IDZ
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
       2002-RECE87-OUT-SEC    SECTION.
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL    IDX        >   WRK-TBLMAX
                        OR     IDX        >   100
                        OR     FLG-END    =     1
      *
             ADD     1                     TO  WRK-LINENUM
             MOVE    SPACE                 TO  RECE87-REC
             INITIALIZE                        RECE87-REC
      *
             MOVE    WRK-RECE86-HOSPNUM    TO  RECE87-HOSPNUM
             MOVE    WRK-RECE86-SRYYM      TO  RECE87-SRYYM
             MOVE    WRK-RECE86-NYUGAIKBN  TO  RECE87-NYUGAIKBN
             MOVE    WRK-RECE86-PTID       TO  RECE87-PTID
             MOVE    WRK-RECE86-RECEKA     TO  RECE87-RECEKA
             MOVE    WRK-RECE86-HKNID      TO  RECE87-HKNID
             MOVE    WRK-RECE86-TEKSTYMD   TO  RECE87-TEKSTYMD
             IF   RECE82-HKNKBN  =  "5"
               MOVE  "10"                  TO  RECE87-SRYKBN
             ELSE
      ***      MOVE  "99"                  TO  RECE87-SRYKBN
               MOVE  "98"                  TO  RECE87-SRYKBN
             END-IF
             MOVE    SPACE                 TO  RECE87-ZAISKBKBN
             MOVE    ZERO                  TO  RECE87-ZAISKBBAN
             MOVE    ZERO                  TO  RECE87-TENTEKIKBN
             MOVE    ZERO                  TO  RECE87-TENTEKIBAN
             MOVE    ZERO                  TO  RECE87-KNSJISKBN
             MOVE    SPACE                 TO  RECE87-SRYSYUKBN
             MOVE    ZERO                  TO  RECE87-DAY
             MOVE    9                     TO  RECE87-CDKBN-SYO
             MOVE    99                    TO  RECE87-CDKBN-BU
             MOVE    "9"                   TO  RECE87-CDKBN-KBN
             MOVE    999                   TO  RECE87-CDKBN-KBNNUM
             MOVE    99                    TO  RECE87-CDKBN-KBNNUM-EDA
             MOVE    99                    TO  RECE87-CDKBN-KOUBAN
             MOVE    999999999             TO  RECE87-KOUHYOJUNNUM
             MOVE    WRK-RECE86-NYUTENTTLKBN
                                           TO  RECE87-NYUTENTTLKBN
             MOVE    WRK-RECE86-SRYCD1     TO  RECE87-SRYCD1
             MOVE    ZERO                  TO  RECE87-ZAITEN
             MOVE    WRK-RECE86-ZAINUM     TO  RECE87-ZAINUM
             MOVE    WRK-LINENUM           TO  RECE87-LINENUM
      *
             MOVE    WRK-TBLMAX            TO  RECE87-ZAIMEISAISU
             MOVE    WRK-RECE86-PTNUM      TO  RECE87-PTNUM
             MOVE    WRK-RECE86-SRYKA      TO  RECE87-SRYKA
             MOVE    TBL-ZAI-NAME(IDX)     TO  RECE87-TEKIYO-INF
      *
             MOVE    SPACE                 TO  RECE87-JIHOKBN
      *
             READ    RECE87-FILE
               INVALID
                   MOVE    1               TO  FLG-RECE87
               NOT INVALID
                   MOVE    ZERO            TO  FLG-RECE87
             END-READ
             IF      FLG-RECE87            =   1
                 WRITE   RECE87-REC
                 IF      STS-RECE87          NOT =   ZERO
                     DISPLAY "*** ORCR0936 RECE87 WRITE ERR  ***"
                     MOVE    1                 TO  FLG-END
                 ELSE
                     ADD     1                 TO  CNT-RECE87
                 END-IF
             ELSE
                 DISPLAY "*** ORCR0936 RECE87 KEY CYOFUKU  ***"
             END-IF
      *
           END-PERFORM
      *
           .
       2002-RECE87-OUT-EXT.
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
                   DISPLAY "*** ORCR0936 INDEX[100] OVER  ***"
               END-IF
      *
               IF    WRK-TBLMAX    =    1
               AND   FLG-HENSYU2   =    ZERO
                   MOVE   "＊"  TO   TBL-ZAI-NAME(WRK-TBLMAX)(1:2)
                   MOVE    1    TO   FLG-HENSYU2
               END-IF
               MOVE  RECE86-NAME(WRK-ST:)  TO
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
                   IF      RECE86-NAME (WRK-ST:)   =   SPACE
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE82-FILE
           CLOSE                       RECE87-FILE
           CLOSE                       RECE86-FILE
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
           DISPLAY "RECE82  IN      " CNT-RECE82
           DISPLAY "RECE87  ADDCNT  " CNT-RECE87
           DISPLAY "*** ORCR0936 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE82
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                   IF     (WRK-PARA-RECEKBN       =  "5")
                   OR     (RECE82-HKNKBN          =  "5")
                   OR     (RECE82-HKNKBN          =  "6")
                       CONTINUE
                   ELSE
                       GO  TO  990-RECE82-READ-SEC
                   END-IF
      *
           END-READ
      *
           .
       990-RECE82-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE86-NEXT-SEC           SECTION.
      *
           READ    RECE86-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE86
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE86
                   ADD     1         TO  CNT-IN-CANCEL
           END-READ
      *
           .
       990-RECE86-NEXT-EXT.
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
