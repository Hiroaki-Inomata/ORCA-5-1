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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCBLMAIN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��������
      *  ����ݡ��ͥ��̾  : �����ץ������ƽ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  03/04/08    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
       WORKING-STORAGE                     SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-DUMMY                       PIC X(02).
      *
      *    �ե饰
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-OUT1                        PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                            PIC 9(05). 
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-TIME.
               05  SYS-THH                     PIC 9(02).
               05  SYS-TMM                     PIC 9(02).
               05  SYS-TSS                     PIC 9(02).
               05  SYS-TDD                     PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY                      PIC 9(04).
               05  SYS-MM                      PIC 9(02).
               05  SYS-DD                      PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PROGRAM-ID                  PIC X(50).
           03  WRK-PARAMETER                   PIC X(2000).
           03  WRK-POINT                       PIC 9(05).
      *
           COPY    "MCPAREA".
      *
      *    COPY    "COMMON-SPA".
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  COMMAND-PARAM       PIC X(400).
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       INDEX-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SYS-AREA
      *
      *
           MOVE    1               TO  WRK-POINT
           UNSTRING        COMMAND-PARAM   DELIMITED   BY  ","
           INTO            WRK-PROGRAM-ID
           WITH POINTER    WRK-POINT
           END-UNSTRING
      *
           MOVE    COMMAND-PARAM (WRK-POINT : )
                                   TO  WRK-PARAMETER
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    �����ץ������ƽ�
           CALL    WRK-PROGRAM-ID  USING   WRK-PARAMETER
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *
           .
       300-TERM-EXT.
           EXIT.
      *