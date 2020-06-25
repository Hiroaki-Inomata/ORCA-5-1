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
       PROGRAM-ID.             ORCSINFSTATE.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �����ɥ����٥����
      *                      �����������֥����å�����
      *  �ֵѥ�����        : 00:����
      *                      10:�ǡ��������ʤ�(�����ƥ����)
      *                      30:�ǡ�������������
      *                      40:�ǡ������������Ѥ�(����ִ�������ޤ�)
      *                      90:�ǡ�������������(�������������Ǥʤ�)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  11.05.19    ��ƣ          ��������
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
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YOBI            PIC 9(01).
           03  WRK-STR-YMD         PIC X(08).
           03  WRK-END-YMD         PIC X(08).
      *
       01  CONST-AREA.
      *    �����ɻ���  
           03  CONST-INFAJOBID     PIC 9(07)  VALUE    1.
           03  CONST-INFASHELLID   PIC X(08)  VALUE    "INFAGREE".
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
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
           COPY    "CPORCSINFCHK.INC".
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           SPA-AREA
           ORCSINFCHKAREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    ZERO                TO  INFCHK-RC
      *
      *    ����ִ��������å��ʴ����ɡ�
           IF     (SPA-DATAINFECTIONKBN    NOT =   1)  AND
                  (SPA-DATAINFECTIONKBN    NOT =   2)
               MOVE    10                  TO  INFCHK-RC
           END-IF
      *
      *    �ޥ������դν��η����������դ����
      **** PERFORM 100-GET-YMD-SEC
      *
      *    ����ִ������麣�������μ��Ӥ�Ĵ�٤�
      **** PERFORM 200-JOBEXEC-CHK-SEC
      *
      *    ����ִ�����̵ͭ��Ĵ�٤�
           PERFORM 210-JOBEXIST-CHK-SEC
           .
      *
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ���������н���
      *****************************************************************
       100-GET-YMD-SEC             SECTION.
      *
      *    �ޥ������ռ���
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-END-YMD
      *
      *    ��������
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-END-YMD         TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  WRK-YOBI
           ELSE
               MOVE    ZERO                TO  WRK-YOBI
           END-IF
      *
           IF      WRK-YOBI                =   1
      *        ������
               MOVE    WRK-END-YMD         TO  WRK-STR-YMD
           ELSE
      *        �ޥ������դν��η������ϡ�
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-END-YMD         TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN      =   (WRK-YOBI  *  -1)  +  1
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           END-IF
           .
       100-GET-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ӥ����å�����
      *****************************************************************
       200-JOBEXEC-CHK-SEC         SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFAJOBID     TO  JOB-JOBID
           MOVE    CONST-INFASHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    30              TO  INFCHK-RC
                   IF      JOB-STARTYMD    NOT =   WRK-END-YMD
                       MOVE    90          TO  INFCHK-RC
                   END-IF
               ELSE
                   IF     (JOB-ENDYMD      >=  WRK-STR-YMD)  AND
                          (JOB-ENDYMD      <=  WRK-END-YMD)
                       MOVE    40          TO  INFCHK-RC
                   END-IF
               END-IF
           END-IF
           .
       200-JOBEXEC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ���̵ͭ�����å�����
      *****************************************************************
       210-JOBEXIST-CHK-SEC        SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFAJOBID     TO  JOB-JOBID
           MOVE    CONST-INFASHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
                   MOVE    40          TO  INFCHK-RC
           END-IF
           .
       210-JOBEXIST-CHK-EXT.
           EXIT.
      *