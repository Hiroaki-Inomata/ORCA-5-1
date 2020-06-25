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
      *    �������޾���ѥ�᥿�����ѥ�      COPY (CPORCSCHN30.INC)
      *                                          (ORCSCHN30 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-¿��Ǽ  09/07/23  �������ɲ�
      *  05.00.00    NACL-¿��Ǽ  17/05/16  �����ֹ��ɲ�
      ************************************************************
      *
       01  ORCSCHN30AREA.
      *
      *    ������ʬ(N:�����������̡�
           03  ORCSHCN30-KBN             PIC X(01).
      *    ������
           03  ORCSHCN30-SRYYMD          PIC X(08).
      *    �ɥ�����
           03  ORCSHCN30-DRCD            PIC X(05).
      *    ����
           03  ORCSHCN30-BTUNAME         PIC X(20).
      *    �¼�
           03  ORCSHCN30-ROOMNO          PIC X(06).
      *H29.5
      *    �����ֹ�
           03  ORCSHCN30-BTUNUM          PIC X(02).
      *
      *    ��������
           03  ORCSHCN30-DATA-REC.
               05  ORCSHCN30-ZAI-MAX         PIC 9(03).
               05  ORCSHCN30-REC-TBL     OCCURS   100.
                   07  ORCSHCN30-ZAIKAIKEI       PIC 9(03).
                   07  ORCSHCN30-SRYKBN          PIC X(02).
                   07  ORCSHCN30-SRYSYUKBN       PIC X(03).
                   07  ORCSHCN30-NAI-TBL         OCCURS   50.
                       09  ORCSHCN30-SRYCD           PIC  X(09).
                       09  ORCSHCN30-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCSHCN30-KANSURYO        PIC  9(05)V9(05).
                       09  ORCSHCN30-SRYKAISU        PIC  9(03).
                       09  ORCSHCN30-ZAIKAISU        PIC  9(03).
      *R02.4           09  ORCSHCN30-INPUTCOMENT     PIC  X(80).
                       09  ORCSHCN30-INPUTCOMENT     PIC  X(140).
      *H28.11          ��ˡ�����ɿ���
                       09  ORCSHCN30-INPUTCHI-G.
      *R02.4               11  ORCSHCN30-INPUTCHI    PIC  X(08)
                           11  ORCSHCN30-INPUTCHI    PIC  X(10)
                                                     OCCURS   5.
      *