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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/09/24  ����б�
      *  01.00.02    ����         11/01/11  ��������������б�
      *  01.00.03    ����         19/01/11  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT3710.
      *����ǯ��
           03  SKT3710-SRYY                PIC  X(04).
           03  SKT3710-SRYM                PIC  X(04).
      *���ŵ��إ�����
           03  SKT3710-HOSPCDN             PIC  X(20).
      *���Ų�
           03  SKT3710-RECEKA              PIC  X(20).
      *����������
           03  SKT3710-TOKUBETU            PIC  X(10).
      *��¸��������ơ��֥�
           03  SKT3710-KZN-TBL OCCURS 2.
      *����������ơ��֥�
               05  SKT3710-NYUGAIKBN1   OCCURS  2.
      *���ܤε��ա����
                   07  SKT3710-KZN-KENSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT3710-KZN-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT3710-KZN-ITBFTN      PIC  X(09).
      *�������
           03  SKT3710-SEIKYU  OCCURS 9.
      *����ˡ���ֹ�
               05  SKT3710-HBTNUM              PIC  X(04).
      *�������̾
               05  SKT3710-KOHNAME             PIC  X(10).
      *����������ơ��֥�
               05  SKT3710-NYUGAIKBN2   OCCURS  2.
      *���ܤε��ա����
                   07  SKT3710-KENSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT3710-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT3710-ITBFTN      PIC  X(09).
