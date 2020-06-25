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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCOLCHK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ����Ĵ��������Ƚ��������å�����
      *  �ֵѥ��顼������  : 00:����  10:�ǡ��������ʤ�
      *                      20:�ǡ�������������  30:�ե�����¸��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07.07.31    ��ƣ          ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ����Ĵ�������ǡ���
           SELECT  COLLECT-FILE    ASSIGN  COLLECT
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-COLLECT.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ����Ĵ�������ǡ���
       FD  COLLECT-FILE.
       01  COLLECT-REC             PIC X(3000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ����Ĵ�������ǡ���
       01  COLLECT.
           03  FILLER              PIC X(08)  VALUE "/var/lib".
           03  FILLER              PIC X(12)  VALUE "/jma-receipt".
           03  FILLER              PIC X(05)  VALUE "/das/".
           03  COLLECT-HOSPNUM     PIC 9(02).
           03  FILLER              PIC X(05)  VALUE "/tmp/".
           03  FILLER              PIC X(07)  VALUE "collect".
           03  COLLECT-GN          PIC X(01).
           03  FILLER              PIC X(04)  VALUE ".csv".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-COLLECT         PIC X(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    ����ִ����ģ¤Υ����͡ʥǡ���������
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                   PIC X(08)   VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSCOLCHK.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSCOLCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  COLCHK-RC
      *
      *    ����ִ��������å��ʥǡ���������
           IF      SPA-DATACREATEFLG   NOT =   1
               MOVE    10                  TO  COLCHK-RC
           END-IF
      *
           IF      COLCHK-RC           =   ZERO
               PERFORM 100-JOBEXEC-CHK-SEC
           END-IF
      *
           IF      COLCHK-RC           =   ZERO
               PERFORM 200-FILEEXIST-CHK-SEC
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �����ǡ����������������å�����
      *****************************************************************
       100-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                       TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    20          TO  COLCHK-RC
               END-IF
           END-IF
           .
       100-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ǡ����ե�����̵ͭ�����å�����
      *****************************************************************
       200-FILEEXIST-CHK-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  COLLECT-HOSPNUM
           MOVE    "g"                 TO  COLLECT-GN
      *
           OPEN    INPUT    COLLECT-FILE
           IF      STS-COLLECT         =   "00"
                   MOVE    30          TO  COLCHK-RC
                   CLOSE   COLLECT-FILE
           END-IF
      *
           IF      COLCHK-RC           =   ZERO
               MOVE    SPA-HOSPNUM     TO  COLLECT-HOSPNUM
               MOVE    "n"             TO  COLLECT-GN
      *
               OPEN    INPUT    COLLECT-FILE
               IF      STS-COLLECT     =   "00"
                       MOVE    30      TO  COLCHK-RC
                       CLOSE   COLLECT-FILE
               END-IF
           END-IF
           .
       200-FILEEXIST-CHK-EXT.
           EXIT.
      *