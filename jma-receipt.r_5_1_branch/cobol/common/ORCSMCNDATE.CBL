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
       PROGRAM-ID.             ORCSMCNDATE.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ޥ������ռ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/11/11    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  00.00.01    NACL������   03/11/11  ��������
      *****************************************************************
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-YMD.
               05  WRK-YY20        PIC X(02).
               05  WRK-YYMMDD      PIC X(06).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �ޥ������ռ������֥ѥ�᥿
           COPY    "CPORCSMCNDATE.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               ORCSMCNDATEAREA.
      *
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              WRK-AREA
      *
           INITIALIZE              ORCSMCNDATEAREA
      *
           MOVE    "20"            TO      WRK-YY20
           ACCEPT  WRK-YYMMDD      FROM    DATE
           MOVE    WRK-YMD         TO      SMCNDATE-YMD
      *
           ACCEPT  SMCNDATE-TIME   FROM    TIME
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *