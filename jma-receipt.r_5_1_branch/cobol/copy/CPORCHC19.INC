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
      *    ����䵽��ϥѥ�᥿�ʣ£���COPY (CPORCHC19.INC)
      *                                          (ORCHC19 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  04/07/20  ��ɼ�ֹ��ɲ�
      *  01.00.02    NACL-¿��Ǽ  05/11/10  ������ʬ A��D �����������ɲ�
      *  05.00.00    NACL-¿��Ǽ  20/02/XX  �����ȡ������ͷ���ѹ�
      ************************************************************
      *
       01  ORCHC19AREA.
      *
      *    1:������������򡧺��������2:Ƭ��Τߡ�3:����ʬ
      *    4:���ӽ�λ���ʱ����ˡ�5:���ӽ�λ���ʱ����
      *    A:���������ѹ���B:��������ʱ�����
      *    C:��������ʱ���ˡ�D:��������������
           03  ORCHC19-KBN             PIC X(01).
      *
      *    �������Ϣ�֡ʳ��ꤷ�ʤ�����������
           03  ORCHC19-RENNUM          PIC 9(01).
      *
      *    ��ɼ�ֹ�
           03  ORCHC19-DENPNUM         PIC 9(07).
      *
      *    �������ơ�A:���������ѹ���D:�������������ˤǻ��ѡ�
           03  ORCHC19-DATA-REC.
               05  ORCHC19-ZAI-MAX         PIC 9(03).
               05  ORCHC19-REC-TBL         OCCURS   100.
                   07  ORCHC19-ZAIKAIKEI       PIC 9(03).
                   07  ORCHC19-ZAITENKEI       PIC 9(08).
                   07  ORCHC19-SRYKBN          PIC X(02).
                   07  ORCHC19-SRYSYUKBN       PIC X(03).
                   07  ORCHC19-NAI-TBL         OCCURS   50.
                       09  ORCHC19-SRYCD           PIC  X(09).
                       09  ORCHC19-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCHC19-SRYKAISU        PIC  9(03).
      *R02.4           09  ORCHC19-INPUTCOMENT     PIC  X(80).
                       09  ORCHC19-INPUTCOMENT     PIC  X(140).
      *H30.4           ������
                       09  ORCHC19-INPUTCHI-G.
      *R02.4               11  ORCHC19-INPUTCHI    PIC  X(08)
                           11  ORCHC19-INPUTCHI    PIC  X(10)
                                                        OCCURS  5.
      *