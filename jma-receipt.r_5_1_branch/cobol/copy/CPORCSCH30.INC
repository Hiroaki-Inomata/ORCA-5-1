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
      *    ���޾���ѥ�᥿�����ѥ�        COPY (CPORCSCH30.INC)
      *                                          (ORCSCH30 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/09  ������ʬ 5 �����ѹ��ɲ�
      *  04.05.00    NACL-¿��Ǽ  09/07/23  �������ɲ�
      *  04.05.00    NACL-¿��Ǽ  09/10/XX  �����Ϳ����ɲ�
      *  04.08.00    NACL-¿��Ǽ  16/10/XX  ��ˡ�����ɿ����ɲ�
      ************************************************************
      *
       01  ORCSCH30AREA.
      *
      *    ������ʬ(�������⡢����������3:���ӽ�λ���⡢4:���ӽ�λ������
      *              5:�����ѹ�
           03  ORCSCH30-KBN            PIC X(01).
      *
      *    ��ɼ�ֹ�
           03  ORCSCH30-DENPNUM        PIC 9(07).
      *
      *    �������ơ�C:���������ѹ��Τ߻��ѡ�
           03  ORCSCH30-DATA-REC.
               05  ORCSCH30-ZAI-MAX         PIC 9(03).
               05  ORCSCH30-REC-TBL     OCCURS   100.
                   07  ORCSCH30-ZAIKAIKEI       PIC 9(03).
                   07  ORCSCH30-SRYKBN          PIC X(02).
                   07  ORCSCH30-SRYSYUKBN       PIC X(03).
                   07  ORCSCH30-NAI-TBL         OCCURS   50.
                       09  ORCSCH30-SRYCD           PIC  X(09).
                       09  ORCSCH30-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCSCH30-KANSURYO        PIC  9(05)V9(05).
                       09  ORCSCH30-SRYKAISU        PIC  9(03).
                       09  ORCSCH30-ZAIKAISU        PIC  9(03).
                       09  ORCSCH30-INPUTCOMENT     PIC  X(140).
                       09  ORCSCH30-INPUTCHI-AREA.
                           11  ORCSCH30-INPUTCHI    PIC  X(10)
                                                    OCCURS  5.
      *