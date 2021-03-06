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
       01  SKT3760.
      *    ����ǯ��
           03  SKT3760-SRYYM               PIC  X(16).
      *    �ݸ����ֹ�
           03  SKT3760-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT3760-HOSPCD              PIC  X(14).
      *    �����ȡ��߰���
           03  SKT3760-ZAI1                PIC  X(02).
           03  SKT3760-ZAI2                PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT3760-NO                  PIC  X(04).
      *    ���Ų�
           03  SKT3760-RECEKA              PIC  X(20).
      *    ����������
           03  SKT3760-TOKUBETU            PIC  X(10).
      *    Ϸ������ơ��֥�
           03       SKT3760-RSEIKYU-TBL OCCURS 2.
               05   SKT3760-RNYUGAI-TBL OCCURS 2.
      *    ���
                   07   SKT3760-KENSU   PIC X(05).
      *    ���ż�����
                   07   SKT3760-NISSU   PIC X(05).
      *    ����
                   07   SKT3760-TENSU   PIC X(09).
      *    ������ô��
                   07   SKT3760-ITBFTN  PIC X(09).
      *    ���������
               05   SKT3760-S-KENSU      PIC X(05).
      *    ����������
               05   SKT3760-S-NISSU      PIC X(05).
      *    ���������
               05   SKT3760-S-KINGK      PIC X(09).
      *    ������ɸ����ô��
               05   SKT3760-S-FTNGK      PIC X(09).
      *    ��������ơ��֥�
           03       SKT3760-KSEIKYU-TBL OCCURS 6.
      *    ����̾
               05   SKT3760-HBTNUM      PIC X(04).
               05   SKT3760-KOHNAME     PIC X(10).
               05   SKT3760-KNYUGAI-TBL OCCURS 2.
      *    ���
                   07   SKT3760-KKENSU   PIC X(05).
      *    ���ż�����
                   07   SKT3760-KNISSU   PIC X(05).
      *    ����
                   07   SKT3760-KTENSU   PIC X(09).
      *    ������ô��
                   07   SKT3760-KITBFTN  PIC X(09).
      *    ���������
               05   SKT3760-S-KKENSU      PIC X(05).
      *    ����������
               05   SKT3760-S-KNISSU      PIC X(05).
      *    ���������
               05   SKT3760-S-KKINGK      PIC X(09).
      *    ������ɸ����ô��
               05   SKT3760-S-KFTNGK      PIC X(09).
