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
       PROGRAM-ID.                     ORCBUPMAIN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �桼���ץ������
      *  ����ݡ��ͥ��̾  : �����ץ������ƽ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  17/10/16    NACL������    ��������
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
      *
       01  WRK-AREA.
           03  WRK-POINTER                 PIC 9(5).
           03  WRK-PROGRAM-ID              PIC X(20).
           03  WRK-PARAMETER               PIC X(400).
      *
       COPY    "CPCOMMAND-PARAM.INC"       REPLACING
                                           //COMMAND-// BY //LNK-//.
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       COPY    "CPCOMMAND-PARAM.INC".
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
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       LNK-PARAM
      *
           MOVE        1           TO  WRK-POINTER
           UNSTRING    COMMAND-PARAM   DELIMITED   BY  ","
           INTO    WRK-PROGRAM-ID
           WITH    POINTER WRK-POINTER
           END-UNSTRING
      *
           IF    ( WRK-POINTER     <=  FUNCTION LENGTH(COMMAND-PARAM))
               MOVE    COMMAND-PARAM(WRK-POINTER:)
                                   TO  LNK-PARAM
           END-IF
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
           CALL    WRK-PROGRAM-ID  USING   LNK-PARAM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *