      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����̣��УɤޤȤ�
      *  ����ݡ��ͥ��̾  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  17/09/19    NaCl��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
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
      *    ���顼�ե�����̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-XML-REC         PIC X(10000).
       01  WRK-XML-REC1        PIC X(10000).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ե�������ϲ�ǽ�����å�����
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
      *    �����
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
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              COUNT-AREA
                                   FLG-AREA
                                   WRK-AREA
      *
      *    �����å�
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
      *    �ե�������Ͻ���
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              OUT-FILE
      *
      *    ���ϥե����뤫����Խ�����
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
      *    ���ϥե����뤫����Խ�����
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
      *    �ե�����ףңɣԣŽ���
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
      *                        �Ǹ�Υե�����Τߺǽ����Ԥ����
                               IF      IDX         =   SAPIFILE-IN-COUNT
                                   MOVE    WRK-XML-REC1    TO  OUT-REC
                                   PERFORM 20012-WRITE-SEC
                                   IF      SAPIFILE-RC     =   0
                                       MOVE    WRK-XML-REC TO  OUT-REC
                                       PERFORM 20012-WRITE-SEC
                                   END-IF
                               END-IF
                           ELSE
      *                        �ǽ�Υե�����Τ���Ƭ���Ԥ����
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
      *    �ե�����ףңɣԣŽ����ʶ�ʬ�ᣲ��
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
      *                    �Ǹ�Υե�����Τߺǽ��Ԥ����
                           IF      IDX         =   SAPIFILE-IN-COUNT
                               MOVE    WRK-XML-REC     TO  OUT-REC
                               PERFORM 20012-WRITE-SEC
                           END-IF
                       ELSE
      *                    �ǽ�Υե�����Τ���Ƭ���Ԥ����
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
      *    �ե�����ףңɣԣŽ���
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
      *    �ե����륨�顼����
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           MOVE    99                  TO  SAPIFILE-RC
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "�ե�����ϣУţΥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "�ե������ɤ߹��ߥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "�ե�����񤭹��ߥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "�ե����빹�����顼��FILE="
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
      *    ���顼����λ����
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
      *    ��֥ե�����ңţ��Ľ���
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