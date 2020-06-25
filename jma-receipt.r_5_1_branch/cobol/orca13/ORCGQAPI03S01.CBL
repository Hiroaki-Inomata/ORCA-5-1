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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQAPI03S01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者照会（印刷）
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-STRCHK              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
      *
      *01  CNT-AREA.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-KANACHK-INPUT       PIC X(512).
           03  WRK-INSTR               PIC X(500).
           03  WRK-LEN                 PIC S9(10).
           03  WRK-GYOUMUID            PIC X(08).
      *
       01  SPA-API-AREA.
           03  SPA-API-RESNUM          PIC X(02).
           03  SPA-API-ORCA-UID        PIC X(36).
           03  SPA-API-REQUEST-PRINT   PIC X(01).
           03  SPA-API-REQUEST-CSV     PIC X(01).
           03  SPA-API-PRINT-ID        PIC X(37).
           03  SPA-API-DATA-ID         PIC X(37).
      *
       01  CONST-AREA.
           03  CONST-SHELLNAME         PIC X(32)   VALUE   "ORCBQ01.sh".
           03  CONST-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-JOBID2            PIC 9(07)   VALUE   2.
           03  CONST-FINDID            PIC X(08)   VALUE   "api013".
           03  CONST-FOUNDID           PIC X(08)   VALUE   "api01301".
           03  CONST-UTF8              PIC X(01)   VALUE   "3".
      *
       01  RESULT-REC.
           COPY    "CPPTSRH.INC"       REPLACING  //PTSRH//
                                       BY         //RESULT//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
           COPY    "Q01SCR-SPA".
      *
      *    照会用サブ
           COPY    "CPQSUB06.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPFOUNDV3RES.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA013.INC".
      *
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    シェルテーブル
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "MONBLOB".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
           COPY    "CPFOUNDV3REQ.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               XML-FOUNDV3REQ
                                               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-API-AREA
           INITIALIZE                  RESULT-REC
      *
           MOVE    SPACE           TO  SPA-Q01FREE
           INITIALIZE                  SPA-Q01FREE
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-FOUNDV3RES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STATE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-DO-SHELL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           PERFORM 2004-STATE-UPD-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    状態確認処理
      *****************************************************************
       2001-STATE-CHK-SEC              SECTION.
      *
           IF    ( FOUNDREQ-ORCA-UID   =   SPACE )
               MOVE    "0205"              TO  SPA-ERRCD
               MOVE    "01"                TO  SPA-API-RESNUM
           END-IF
      *
           IF    ( FOUNDREQ-KARTE-UID   =  SPACE )
               MOVE    "0203"              TO  SPA-ERRCD
               MOVE    "01"                TO  SPA-API-RESNUM
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    CONST-FINDID        TO  WRK-GYOUMUID
               PERFORM 900-APIPARA-KEY5-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA013-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   EVALUATE    PARA013-STATE
                   WHEN    "01"
                       MOVE    "1040"      TO  SPA-ERRCD
                   WHEN    "02"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0206"      TO  SPA-ERRCD
                       MOVE    "01"        TO  SPA-API-RESNUM
                   END-EVALUATE
               ELSE
                   MOVE    "0206"          TO  SPA-ERRCD
                   MOVE    "01"            TO  SPA-API-RESNUM
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    CONST-FINDID        TO  WRK-GYOUMUID
               PERFORM 900-APIPARA-KEY7-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA013-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   MOVE    "1038"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-PTSRH-KEY2-SEL-SEC
               MOVE    PTSRH-REC           TO  RESULT-REC
      *
               IF    ( RESULT-DBERR    NOT =   ZERO )
                   MOVE    "1049"      TO  SPA-ERRCD
                   MOVE    "01"        TO  SPA-API-RESNUM
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( RESULT-CNT-ALL  =   ZERO )
                       MOVE    "1041"      TO  SPA-ERRCD
                       MOVE    "01"        TO  SPA-API-RESNUM
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    CONST-FOUNDID       TO  WRK-GYOUMUID
               PERFORM 900-APIPARA-KEY-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA013-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   IF    ( PARA013-STATE   =   "01" )
                       MOVE    "1052"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-STATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-REQDAT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-REQPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-API-REQUEST-PRINT   =   "1" )
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 2001-PTLIST-TITLE-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 2001-PTLIST-TARGET-CHK-SEC
               END-IF
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索結果データ作成区分リクエストチェック処理
      *****************************************************************
       2001-REQDAT-CHK-SEC             SECTION.
      *
           IF    ( FOUNDREQ-REQUEST-DATA       =   SPACE)
               MOVE    "0"             TO  SPA-API-REQUEST-CSV
           ELSE
               IF    ( FOUNDREQ-REQUEST-DATA   =   "0" OR "1" )
                   MOVE    FOUNDREQ-REQUEST-DATA
                                       TO  SPA-API-REQUEST-CSV
               ELSE
                   MOVE    "0212"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-REQDAT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索結果リスト印刷区分リクエストチェック処理
      *****************************************************************
       2001-REQPRT-CHK-SEC             SECTION.
      *
           IF    ( FOUNDREQ-REQUEST-PRINT     =   SPACE)
               MOVE    "0"             TO  SPA-API-REQUEST-PRINT
           ELSE
               IF    ( FOUNDREQ-REQUEST-PRINT =   "0" OR "1" )
                   MOVE    FOUNDREQ-REQUEST-PRINT
                                       TO  SPA-API-REQUEST-PRINT
               ELSE
                   MOVE    "0213"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-REQPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    表題チェック処理
      *****************************************************************
       2001-PTLIST-TITLE-CHK-SEC       SECTION.
      *
           MOVE    FOUNDREQ-PTLIST-TITLE
                                       TO  WRK-KANACHK-INPUT
           PERFORM 800-ZENKAKU-CHK-SEC
           IF    ( FLG-STRCHK      NOT =   ZERO )
               MOVE    "0214"          TO  SPA-ERRCD
           END-IF
      *
           .
       2001-PTLIST-TITLE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象名チェック処理
      *****************************************************************
       2001-PTLIST-TARGET-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    FOUNDREQ-PTLIST-TARGET (IDXA)
                                       TO  WRK-KANACHK-INPUT
               PERFORM 800-ZENKAKU-CHK-SEC
               IF    ( FLG-STRCHK      NOT =   ZERO )
                   MOVE    "0215"      TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           .
       2001-PTLIST-TARGET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC                 SECTION.
      *
           IF    ( FOUNDREQ-REQUEST-PRINT  =   "1" )
            OR   ( FOUNDREQ-REQUEST-DATA   =   "1" )
               CONTINUE
           ELSE
               MOVE    "1051"      TO  SPA-ERRCD
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    シェル実行処理
      *****************************************************************
       2003-DO-SHELL-SEC                   SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( FOUNDREQ-REQUEST-PRINT  =   "1" )
               PERFORM 20031-GET-MONBLOB-ID-SEC
               IF    ( MONBLOB-ID      =   SPACE   )
                   MOVE    "4011"      TO  SPA-ERRCD
               ELSE
                   MOVE    MONBLOB-ID  TO  SPA-API-PRINT-ID
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( FOUNDREQ-REQUEST-DATA   =   "1" )
               PERFORM 20031-GET-MONBLOB-ID-SEC
               IF    ( MONBLOB-ID      =   SPACE   )
                   MOVE    "4012"      TO  SPA-ERRCD
               ELSE
                   MOVE    MONBLOB-ID  TO  SPA-API-DATA-ID
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 900-PTSRH-INS2-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 20031-DO-SHELL-SEC
           END-IF
      *
           .
       2003-DO-SHELL-EXT.
           EXIT.
      *****************************************************************
      *    MONBLOBID取得処理
      *****************************************************************
       20031-GET-MONBLOB-ID-SEC        SECTION.
      *
           INITIALIZE                  MONBLOB
           MOVE    "MONBLOBNEW"    TO  MCP-FUNC
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "monblob"       TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MONBLOB
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPACE       TO  MONBLOB-ID
           END-IF
      *
           .
       20031-GET-MONBLOB-ID-EXT.
           EXIT.
      *****************************************************************
      *    シェル実行処理
      *****************************************************************
       20031-DO-SHELL-SEC               SECTION.
      *
           INITIALIZE                   STEMPDIR-AREA
           CALL     "ORCSTEMPDIR"       USING
                                        STEMPDIR-AREA
           MOVE     STEMPDIR-TEMPDIR    TO  MCP-TEMPDIR
      *
           INITIALIZE                      QSUB06-LNK
      *
      *    印刷用スクリプト処理
           MOVE    CONST-SHELLNAME     TO  QSUB06-SHELLNAME
           MOVE    ZERO                TO  QSUB06-JOBID
           MOVE    FOUNDREQ-ORCA-UID   TO  QSUB06-ORCA-UID
      *
           MOVE    SPA-API-REQUEST-CSV
                                       TO  QSUB06-REQUEST-CSV
           MOVE    SPA-API-REQUEST-PRINT
                                       TO  QSUB06-REQUEST-PRINT
           MOVE    "1"                 TO  QSUB06-FILESVKBN
           MOVE    RESULT-UPHMS        TO  QSUB06-SELHMS
           MOVE    MCP-TEMPDIR         TO  QSUB06-DIRNAME
           MOVE    CONST-UTF8          TO  QSUB06-ENCODEKBN
           MOVE    "patient_list.csv"  TO  QSUB06-FILENAME
                                           QSUB06-CSVFILENAME
           MOVE    "patient_list.pdf"  TO  QSUB06-PDFFILENAME
      *
      *    検索結果パターン
           MOVE    RESULT-OUTKBN       TO  QSUB06-OUTKBN
      *
           MOVE    FOUNDREQ-PTLIST-TITLE
                                       TO  QSUB06-HYODAI
           MOVE    FOUNDREQ-PTLIST-TARGET (1)
                                       TO  QSUB06-JYOKEN1
           MOVE    FOUNDREQ-PTLIST-TARGET (2)
                                       TO  QSUB06-JYOKEN2
           MOVE    1                   TO  QSUB06-STPAGE
           MOVE    9999999             TO  QSUB06-EDPAGE
           MOVE    1                   TO  QSUB06-STKNS
           MOVE    9999999             TO  QSUB06-EDKNS
      *
           MOVE    SPA-SYSYMD          TO  QSUB06-KJYNYMD
      *
           MOVE    RESULT-ORDER        TO  QSUB06-SQL
      *
           MOVE    "API"               TO  QSUB06-SHELLID
           MOVE    FOUNDREQ-KARTE-UID  TO  QSUB06-KARTE-UID
           MOVE    CONST-FOUNDID       TO  QSUB06-TBL-KEY
           MOVE    SPA-API-DATA-ID     TO  QSUB06-DATA-ID
           MOVE    SPA-API-PRINT-ID    TO  QSUB06-PRINT-ID
      *
           CALL    "ORCGQSUB06"    USING
                                   QSUB06-LNK
                                   SPA-AREA
      *
           .
       20031-DO-SHELL-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC              SECTION.
      *
           MOVE    SPA-API-DATA-ID     TO  FOUNDRES-DATA-ID
           MOVE    SPA-API-PRINT-ID    TO  FOUNDRES-PRINT-ID
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       2004-STATE-UPD-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE              PARA013-REC
               MOVE    "01"        TO  PARA013-STATE
               PERFORM 20041-STATE-UPD-SEC
           END-IF
      *
           .
       2004-STATE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       20041-STATE-UPD-SEC             SECTION.
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    CONST-FOUNDID   TO  APIPARA-GYOUMUID
           MOVE    FOUNDREQ-ORCA-UID
                                   TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    1               TO  APIPARA-EDANUM
           MOVE    FOUNDREQ-KARTE-UID
                                   TO  APIPARA-KARTE-UID
           MOVE    PARA013-REC     TO  APIPARA-DATA-REC
           MOVE    SMCNDATE-YMD    TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS    TO  APIPARA-UPHMS
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    "4013"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    "4014"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20041-STATE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得
      *****************************************************************
       800-LENGTH-SEC              SECTION.
      *
      *    WRK-INSTRが空白の場合、FUNCTIONで長さを求めると１が返却される。
      *    空白の場合は0を返却するように調整
           MOVE    ZERO            TO  WRK-LEN
           IF    ( WRK-INSTR       NOT =   SPACE )
               MOVE    FUNCTION BYTE-LENGTH(
                           FUNCTION TRIM(WRK-INSTR TRAILING)
                       )           TO  WRK-LEN
           END-IF
      *
           .
       800-LENGTH-EXT.
           EXIT.
      *****************************************************************
      *    全角文字チェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-STRCHK
      *
           IF  ( WRK-KANACHK-INPUT     NOT =   SPACE )
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-INPUT
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-STRCHK
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-STRCHK
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC               SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCGQERRMSG"   USING
                                   SPA-AREA
                                   SPA-Q01FREE
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC               SECTION.
      *
           MOVE    FOUNDREQ-REQNUM     TO  FOUNDRES-REQNUM
           IF    ( SPA-ERRCD       =   ZERO  OR  SPACE )
               MOVE    "02"            TO  FOUNDRES-RESNUM
           ELSE
               MOVE    "01"            TO  FOUNDRES-RESNUM
           END-IF
      *
           MOVE    FOUNDREQ-KARTE-UID  TO  FOUNDRES-KARTE-UID
           MOVE    FOUNDREQ-ORCA-UID   TO  FOUNDRES-ORCA-UID
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  FOUNDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  FOUNDRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD           =   ZERO )
               MOVE    ZERO            TO  FOUNDRES-API-RESULT
           ELSE
               MOVE    "E"             TO  FOUNDRES-API-RESULT
               MOVE    SPA-ERRCD       TO  FOUNDRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-Q01-ERRMSG      TO  FOUNDRES-API-RESULT-MSG
      *
           MOVE    WRK-TIME            TO  FOUNDRES-INFORMATION-TIME
      *
           MOVE    "xml_foundv3res"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  FOUNDRES-MODE
           MOVE    "foundv3res"        TO  FOUNDRES-RECORDNAME
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-FOUNDV3RES
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    FOUNDRES-OBJECT TO  QAPI03-BODY
               MOVE    "application/xml"
                                       TO  QAPI03-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    FOUNDREQ-ORCA-UID   TO  PTSRH-TERMID
           MOVE    "0"                 TO  PTSRH-DATAKBN
           MOVE    "%"                 TO  PTSRH-UPYMD
           MOVE    "%"                 TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    WRK-GYOUMUID        TO  APIPARA-GYOUMUID
           MOVE    FOUNDREQ-ORCA-UID   TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY5-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    WRK-GYOUMUID        TO  APIPARA-GYOUMUID
           MOVE    FOUNDREQ-ORCA-UID    TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    FOUNDREQ-KARTE-UID   TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY7-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    WRK-GYOUMUID        TO  APIPARA-GYOUMUID
           MOVE    FOUNDREQ-ORCA-UID   TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    FOUNDREQ-KARTE-UID  TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会追加処理
      *****************************************************************
       900-PTSRH-INS2-SEC              SECTION.
      *
           PERFORM 900-PTSRH-DEL1-SEC
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    FOUNDREQ-ORCA-UID
                                   TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    RESULT-UPYMD    TO  PTSRH-UPYMD
           MOVE    RESULT-UPHMS    TO  PTSRH-UPHMS
           IF    ( RESULT-OUTKBN   =   "3" )
               MOVE    "3"         TO  PTSRH-UPDATAKBN
           ELSE
               MOVE    "1"         TO  PTSRH-UPDATAKBN
           END-IF
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "ins2"          TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4015"      TO  SPA-ERRCD
           END-IF
      *
           .
       900-PTSRH-INS2-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    FOUNDREQ-ORCA-UID
                                   TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
