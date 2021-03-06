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
       01  SKT3750.
      *    ����ǯ��
           03  SKT3750-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT3750-SEIYMD              PIC  X(22).
      *    ����
           03  SKT3750-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT3750-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT3750-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT3750-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT3750-HKNJANAME           PIC  X(24).
      *    �ݸ����ֹ�
           03  SKT3750-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT3750-HOSPCD              PIC  X(14).
      *    �����ǡ��߰���
           03  SKT3750-ZAI1                PIC  X(02).
           03  SKT3750-ZAI2                PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT3750-NO                  PIC  X(04).
      *    ���Ų�
           03  SKT3750-RECEKA              PIC  X(20).
      *    ����������
           03  SKT3750-TOKUBETU            PIC  X(10).
      *    ���
           03       SKT3750-KENSU-TBL OCCURS 9.
               05   SKT3750-KENSU     OCCURS 2 PIC X(05).
      *    ���ż�����
           03       SKT3750-NISSU-TBL OCCURS 9.
               05   SKT3750-NISSU     OCCURS 2 PIC X(05).
      *    ����
           03       SKT3750-TENSU-TBL OCCURS 9.
               05   SKT3750-TENSU     OCCURS 2 PIC X(09).
      *    ������ô��
           03       SKT3750-ITBFTN-TBL OCCURS 9.
               05   SKT3750-ITBFTN     OCCURS 2 PIC X(09).
      *    ���
           03       SKT3750-S-KENSU-TBL OCCURS 9.
               05   SKT3750-S-KENSU      PIC X(05).
      *    ���ż�����
           03       SKT3750-S-NISSU-TBL OCCURS 9.
               05   SKT3750-S-NISSU      PIC X(05).
      *    ���
           03       SKT3750-S-KINGK-TBL OCCURS 9.
               05   SKT3750-S-KINGK      PIC X(09).
      *    ������ô��
           03       SKT3750-S-FTNGK-TBL OCCURS 9.
               05   SKT3750-S-FTNGK     PIC X(09).
