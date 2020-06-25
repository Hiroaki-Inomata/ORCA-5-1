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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSAPIFILE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 大容量ＡＰＩまとめ
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/09/19    NaCl−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
           SELECT  XML-FILE    ASSIGN  XMLPARA
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-XMLFILE.
      *
           SELECT  OUT-FILE   ASSIGN  OUTPARA
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-OUTFILE.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  XML-FILE.
       01  XML-REC             PIC X(10000).
      *
       FD  OUT-FILE.
       01  OUT-REC             PIC X(10000).
      *
       WORKING-STORAGE         SECTION.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //XMLPARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //OUTPARA//.
      *
       01  STS-AREA.
           03  STS-XMLFILE     PIC X(02).
           03  STS-OUTFILE     PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END         PIC 9(01).
           03  FLG-XMLFILE     PIC 9(01).
      *
       01  COUNT-AREA.
           03  CNT-IN          PIC 9(08).
           03  CNT-OUT         PIC 9(08).
      *
       01  INDEX-AREA.
           03  IDX             PIC 9(08).
      *
       01  WRK-AREA.
           03  WRK-RENNUM      PIC 9(06).
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
      *
       01  WRK-XML-REC         PIC X(10000).
       01  WRK-XML-REC1        PIC X(10000).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ファイル出力可能チェックサブ
           COPY    "CPORCSACCESS.INC".
      *
      ******************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                               USING
                               ORCSAPIFILEAREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           display "ORCSAPIFILE IN-COUNT    =" SAPIFILE-IN-COUNT
           display "ORCSAPIFILE IN-RENNUM   =" SAPIFILE-IN-RENNUM
           display "ORCSAPIFILE IN-FILE-TYPE=" SAPIFILE-IN-FILE-TYPE
      *
           PERFORM 100-INIT-SEC
      *
           IF      SAPIFILE-RC     =   0
               PERFORM 200-MAIN-SEC
           END-IF
      *
           display "ORCSAPIFILE OUT=" CNT-OUT
           display "ORCSAPIFILE RC =" SAPIFILE-RC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              COUNT-AREA
                                   FLG-AREA
                                   WRK-AREA
      *
      *    チェック
           IF    ( SAPIFILE-IN-DIRNAME
                                   =   SPACE )
           OR    ( SAPIFILE-IN-FILENAME
                                   =   SPACE )
               MOVE  10                TO  SAPIFILE-RC
               GO  TO  100-INIT-EXT
           END-IF
      *
           IF      SAPIFILE-IN-FILE-TYPE
                                   =   "xml"   OR  "json"
               CONTINUE
           ELSE
               MOVE  11                TO  SAPIFILE-RC
               GO  TO  100-INIT-EXT
           END-IF
      *
           IF      SAPIFILE-IN-COUNT
                                   =   ZERO
               MOVE  12                TO  SAPIFILE-RC
               GO  TO  100-INIT-EXT
           END-IF
      *
           IF      SAPIFILE-IN-RENNUM
                                   >   SAPIFILE-IN-COUNT
               MOVE  13                TO  SAPIFILE-RC
               GO  TO  100-INIT-EXT
           END-IF
      *
           IF    ( SAPIFILE-OT-DIRNAME
                                   =   SPACE )
           OR    ( SAPIFILE-OT-FILENAME
                                   =   SPACE )
               MOVE  14                TO  SAPIFILE-RC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    SPACE           TO  OUTPARA
           STRING   SAPIFILE-OT-DIRNAME    DELIMITED  BY  SPACE
                    "/"                    DELIMITED  BY  SIZE
                    SAPIFILE-OT-FILENAME   DELIMITED  BY  SPACE
                                           INTO    OUTPARA
           END-STRING
      *
           INITIALIZE                  SACCESS-AREA
           MOVE    "W"             TO  SACCESS-MODE
           MOVE    OUTPARA         TO  SACCESS-IN-FILENAME
           CALL    "ORCSACCESS"    USING
                                   SACCESS-AREA
           IF      SACCESS-RC      =   ZERO
               CONTINUE
           ELSE
               MOVE  20                TO  SAPIFILE-RC
               GO  TO  100-INIT-EXT
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              OUT-FILE
      *
      *    入力ファイルからの編集処理
           PERFORM VARYING IDX FROM    SAPIFILE-IN-RENNUM   BY  1
                   UNTIL   IDX >       SAPIFILE-IN-COUNT
                   OR      SAPIFILE-RC >   0
               PERFORM 2001-XMLFILE-HENSYU-SEC
           END-PERFORM
      *
           CLOSE   OUT-FILE
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入力ファイルからの編集処理
      *****************************************************************
       2001-XMLFILE-HENSYU-SEC              SECTION.
      *
          MOVE    IDX                  TO  WRK-RENNUM
      *
           MOVE    SPACE               TO  XMLPARA
           STRING   SAPIFILE-IN-DIRNAME    DELIMITED  BY  SPACE
                    "/"                    DELIMITED  BY  SIZE
                    SAPIFILE-IN-FILENAME   DELIMITED  BY  SPACE
                    WRK-RENNUM             DELIMITED  BY SIZE
                    ".xml"                 DELIMITED  BY SIZE
                                           INTO    XMLPARA
           END-STRING
      *
           OPEN    INPUT               XML-FILE
           IF      STS-XMLFILE         =   "00"
               CONTINUE
           ELSE
               MOVE  30                TO  SAPIFILE-RC
      *
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    XMLPARA             TO  WRK-ERR-FILE-NM
               MOVE    STS-XMLFILE         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
               CLOSE   XML-FILE
               GO  TO  2001-XMLFILE-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  CNT-IN
                                           FLG-XMLFILE
           IF      SAPIFILE-IN-KBN     =   "2"
               PERFORM 20011-XMLFILE-HENSYU-SEC
           ELSE
               PERFORM 20012-XMLFILE-HENSYU-SEC
           END-IF
      *
           CLOSE   XML-FILE
           .
       2001-XMLFILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＷＲＩＴＥ処理
      *****************************************************************
       20011-XMLFILE-HENSYU-SEC         SECTION.
      *
           PERFORM 900-XMLFILE-READ-SEC
           IF      FLG-XMLFILE     =   ZERO
               ADD     1               TO  CNT-IN
               MOVE    XML-REC         TO  WRK-XML-REC
      *
               PERFORM 900-XMLFILE-READ-SEC
               PERFORM             UNTIL   FLG-XMLFILE =   1
                   MOVE    WRK-XML-REC     TO  WRK-XML-REC1
      *
                   ADD     1               TO  CNT-IN
                   MOVE    XML-REC         TO  WRK-XML-REC
      *
                   PERFORM 900-XMLFILE-READ-SEC
      *
                   MOVE    SPACE               TO  OUT-REC
                   EVALUATE    SAPIFILE-IN-FILE-TYPE
                       WHEN    "xml"
                           IF      FLG-XMLFILE =   1
      *                        最後のファイルのみ最終２行を出力
                               IF      IDX         =   SAPIFILE-IN-COUNT
                                   MOVE    WRK-XML-REC1    TO  OUT-REC
                                   PERFORM 20012-WRITE-SEC
                                   IF      SAPIFILE-RC     =   0
                                       MOVE    WRK-XML-REC TO  OUT-REC
                                       PERFORM 20012-WRITE-SEC
                                   END-IF
                               END-IF
                           ELSE
      *                        最初のファイルのみ先頭３行を出力
                               IF      CNT-IN      <   5
                                   IF      IDX         =
                                                   SAPIFILE-IN-RENNUM
                                       MOVE    WRK-XML-REC1
                                                           TO  OUT-REC
                                       PERFORM 20012-WRITE-SEC
                                   END-IF
                               ELSE
                                   MOVE    WRK-XML-REC1    TO  OUT-REC
                                   PERFORM 20012-WRITE-SEC
                               END-IF
                           END-IF
                       WHEN    "json"
                       IF    ( IDX         =   SAPIFILE-IN-RENNUM )
                       AND   ( CNT-IN      =   1                  )
                           MOVE    "["         TO  OUT-REC
                           PERFORM 20012-WRITE-SEC
                       END-IF
                       MOVE    WRK-XML-REC     TO  OUT-REC
                       PERFORM 20012-WRITE-SEC
                       IF    ( IDX         =   SAPIFILE-IN-COUNT )
                       AND   ( FLG-XMLFILE =   1                 )
                          MOVE    "]"         TO  OUT-REC
                          PERFORM 20012-WRITE-SEC
                       END-IF
                   END-EVALUATE
      *
                   IF      SAPIFILE-RC >   0
                       MOVE     1          TO  FLG-XMLFILE
                   END-IF
               END-PERFORM
           END-IF 
           .
       20011-XMLFILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＷＲＩＴＥ処理（区分＝２）
      *****************************************************************
       20012-XMLFILE-HENSYU-SEC         SECTION.
      *
           PERFORM 900-XMLFILE-READ-SEC
           PERFORM             UNTIL   FLG-XMLFILE =   1
               ADD     1               TO  CNT-IN
               MOVE    XML-REC         TO  WRK-XML-REC
      *
               PERFORM 900-XMLFILE-READ-SEC
      *
               MOVE    SPACE               TO  OUT-REC
               EVALUATE    SAPIFILE-IN-FILE-TYPE
                   WHEN    "xml"
                       IF      FLG-XMLFILE =   1
      *                    最後のファイルのみ最終行を出力
                           IF      IDX         =   SAPIFILE-IN-COUNT
                               MOVE    WRK-XML-REC     TO  OUT-REC
                               PERFORM 20012-WRITE-SEC
                           END-IF
                       ELSE
      *                    最初のファイルのみ先頭２行を出力
                           IF      CNT-IN      <   3
                               IF      IDX         =
                                                   SAPIFILE-IN-RENNUM
                                   MOVE    WRK-XML-REC     TO  OUT-REC
                                   PERFORM 20012-WRITE-SEC
                               END-IF
                           ELSE
                               MOVE    WRK-XML-REC     TO  OUT-REC
                               PERFORM 20012-WRITE-SEC
                           END-IF
                       END-IF
                   WHEN    "json"
                       IF    ( IDX         =   SAPIFILE-IN-RENNUM )
                       AND   ( CNT-IN      =   1                  )
                           MOVE    "["         TO  OUT-REC
                           PERFORM 20012-WRITE-SEC
                       END-IF
                       MOVE    WRK-XML-REC     TO  OUT-REC
                       PERFORM 20012-WRITE-SEC
                       IF    ( IDX         =   SAPIFILE-IN-COUNT )
                       AND   ( FLG-XMLFILE =   1                 )
                          MOVE    "]"         TO  OUT-REC
                          PERFORM 20012-WRITE-SEC
                       END-IF
               END-EVALUATE
      *
               IF      SAPIFILE-RC >   0
                   MOVE     1          TO  FLG-XMLFILE
               END-IF
           END-PERFORM
           .
       20012-XMLFILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＷＲＩＴＥ処理
      *****************************************************************
       20012-WRITE-SEC         SECTION.
      *
           WRITE   OUT-REC
      *
           IF      STS-OUTFILE         =   "00"
               ADD     1                   TO  CNT-OUT
           ELSE
               MOVE  40                    TO  SAPIFILE-RC
      *
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    OUTPARA             TO  WRK-ERR-FILE-NM
               MOVE    STS-OUTFILE         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           .
       20012-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           MOVE    99                  TO  SAPIFILE-RC
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
                   WRK-ERR-FILE-NM         DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           DISPLAY "ORCSAPIFILE " WRK-RECEERR
      *
      *****STRING  WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
      *****        " FILE="                DELIMITED   BY  SIZE
      *****        WRK-ERR-FILE-NM         DELIMITED   BY  SPACE
      *****        "  STS="                DELIMITED   BY  SIZE
      *****        WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
      *****        LOW-VALUE               DELIMITED   BY  SIZE
      *****                                INTO    WRK-ERRMSG
      ***** END-STRING
      *
      *****PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
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
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-XMLFILE-READ-SEC            SECTION.
      *
           READ    XML-FILE    NEXT
               AT  END
                   MOVE    1               TO  FLG-XMLFILE
               NOT  AT  END
                   IF      STS-XMLFILE     =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    XMLPARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-XMLFILE     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
      *
                       MOVE    1               TO  FLG-XMLFILE
                   END-IF
           END-READ
           .
       900-XMLFILE-READ-EXT.
           EXIT.
