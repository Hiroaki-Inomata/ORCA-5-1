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
      *----------------------------------------------------------*
      *    ������Ģ���ϥѥ�᥿          COPY (CPORCSCH62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSCH62AREA.
      *
      *    1:��ɼ�ֹ�ʱ���ˡ�2:��ɼ�ֹ�ʱ�����
      *    3:���ӽ�λ���ʱ����ˡ�4:���ӽ�λ���ʱ�����
      *    A:���������ѹ���B:��������ʱ�����
      *    C:��������ʱ���ˡ�D:��������������
           03  ORCSCH62-KBN             PIC X(01).
      *    1:�ãӣֽ��ϤΤ�
           03  ORCSCH62-KBN2            PIC X(01).
      *    ȯ�Բ��̶�ʬ��1:�����ǧ��2:���ӽ�λ��3���������
           03  ORCSCH62-PGKBN           PIC X(01).
      *
      *    ��ɼ�ֹ�
           03  ORCSCH62-DENPNUM         PIC 9(07).
      *
      *    �������ơ�A:���������ѹ���D:�������������ˤǻ��ѡ�
           03  ORCSCH62-DATA-REC.
               05  ORCSCH62-ZAI-MAX         PIC 9(03).
               05  ORCSCH62-REC-TBL         OCCURS   100.
                   07  ORCSCH62-ZAIKAIKEI       PIC 9(03).
                   07  ORCSCH62-SRYKBN          PIC X(02).
                   07  ORCSCH62-SRYSYUKBN       PIC X(03).
                   07  ORCSCH62-NAI-TBL         OCCURS   50.
                       09  ORCSCH62-SRYCD           PIC  X(09).
                       09  ORCSCH62-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCSCH62-SRYKAISU        PIC  9(03).
                       09  ORCSCH62-INPUTCOMENT     PIC  X(140).
                       09  ORCSCH62-KANSURYO        PIC  9(05)V9(05).
      *