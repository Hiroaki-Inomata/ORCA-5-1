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
       PROGRAM-ID.             ORCSMKPASS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/09/12    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
      *01  WRK-AREA.
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ǥ��쥯�ȥ���ֻ���
           COPY    "CPHOMEDIR.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPMKPASS.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSMKPASSAREA.
      *
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  IDX-AREA
      *
           INITIALIZE                  MKPASS-OT-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      MKPASS-IN (IDX)     NOT =   SPACE
                   PERFORM 100-MKPASS-SYORI-SEC
               END-IF
           END-PERFORM
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       100-MKPASS-SYORI-SEC            SECTION.
      *
           STRING  HOMEDIR-NAME        DELIMITED   BY  SPACE
                   MKPASS-IN (IDX)     DELIMITED   BY  SPACE
                                       INTO    MKPASS-OT (IDX)
           END-STRING
      *
           .
       100-MKPASS-SYORI-EXT.
           EXIT.