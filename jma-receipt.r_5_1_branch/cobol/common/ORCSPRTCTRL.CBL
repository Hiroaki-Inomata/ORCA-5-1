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
       PROGRAM-ID.             ORCSPRTCTRL.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���饤����Ȱ�����������Ȥ�Ω��
      *  ����ݡ��ͥ��̾  : GROUP & UUID ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/03/10    NACL�����ġ�  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-����    14/08/06  4.8���饤����Ȱ����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       DATA                    DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE         SECTION.
      **************************************************************************
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRTCTRL.INC".
           COPY    "MCPAREA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               SPA-AREA
               ORCSPRTCTRLAREA
               MCPAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
         IF  ( ORCSPRTCTRL-BATCH-FLG NOT   =   1  )
               MOVE    1               TO  SPA-CLIENT-PRT-FLG
         END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .