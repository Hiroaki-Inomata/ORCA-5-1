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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSFILECHK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �ե�����¸�ߥ����å�
      *                    : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/08/07    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 00.00.01     NACL-����    03/05/30  �ե���������å�����
      * 04.08.00     NACL-¿��Ǽ  14/07/30  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ��ե�����
           SELECT  IN-FILE     ASSIGN  ASS-IN
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-IN.
      *
       DATA                DIVISION.
       FILE                    SECTION.
      *
      *    ���ϥե�����
       FD  IN-FILE.
       01  IN-REC          PIC X(01).
      *
       WORKING-STORAGE         SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    �ե���������ΰ�
       01  ASS-AREA.
      **** 03  ASS-IN      PIC X(256).
           03  ASS-IN      PIC X(1024).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-IN      PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-IN      PIC 9(01).
      *
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "CPORCSFILECHK.INC".
      *
      ******************************************************************
       PROCEDURE           DIVISION
                               USING
                               ORCSFILECHKAREA.
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
      *     PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           MOVE    ZERO        TO  FLG-AREA
      *
      *    ���ϥե�����
           MOVE    FILECHK-NAME    TO  ASS-IN
      *
      *    ���ϡ��������
           OPEN    INPUT           IN-FILE
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           IF          STS-IN      NOT     =   ZERO
               MOVE    2           TO  FILECHK-RC
           ELSE
      *        �쥳���ɴ����ե������ɹ���
               PERFORM 900-IN-READ-SEC
      *
               IF          FLG-IN      =   ZERO
                   MOVE    0           TO  FILECHK-RC
               ELSE
                   MOVE    1           TO  FILECHK-RC
               END-IF
      *        ��λ����
               PERFORM 300-END-SEC
           END-IF
          .
      *
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC             SECTION.
      *
      *    ���ϥե����롡��λ����
           CLOSE               IN-FILE
           .
      *
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥե������ɹ��߽���
      *****************************************************************
       900-IN-READ-SEC         SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1           TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO        TO  FLG-IN
          END-READ
          .
      *
       900-IN-READ-EXT.
          EXIT.
      *