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
       01  SKT3700.
      *����ǯ��
           03  SKT3700-SRYY                PIC  X(04).
           03  SKT3700-SRYM                PIC  X(04).
      *͹���ֹ�
      *���ŵ��ؽ����
           03  SKT3700-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT3700-HOSPNAME            PIC  X(100).
      *���ŵ��������ֹ�
           03  SKT3700-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT3700-KAISETU             PIC  X(40).
      *���ŵ��إ�����
           03  SKT3700-HOSPCDN             PIC  X(20).
      *���Ų�
           03  SKT3700-RECEKA              PIC  X(20).
      *����������
           03  SKT3700-TOKUBETU            PIC  X(10).
      *�������
           03  SKT3700-SEIKYU  OCCURS 11.
      *����������ơ��֥�
               05  SKT3700-NYUGAIKBN   OCCURS  2.
      *���ܤε��ա����
                   07  SKT3700-KENSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT3700-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT3700-ITBFTN      PIC  X(09).
