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
       PROGRAM-ID.             ORCSMKPRTSITE1.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ����饤��Ģɼ��������
      *                    : (�ư�����DB�񤭹����ɲ�)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/09/12    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����    ����
      *  04.07.00    NACL-����  11/03/20  ���饤����Ȱ����б�
      *  04.07.00    NACL-����  13/04/26  ʣ�����б�
      *  05.00.00    NACL-����  17/06/30  MKPRT1�˽������ư
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       DATA                        DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
        LINKAGE                    SECTION.
           COPY  "CPORCSMKPRT.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSMKPRTAREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    "__SITE__"      TO  MKPRT-DEF
           CALL    "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA.
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .