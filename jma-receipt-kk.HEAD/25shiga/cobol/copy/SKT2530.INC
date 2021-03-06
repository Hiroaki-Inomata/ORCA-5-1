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
      * Maj/Min/Rev  ������   ����         ����
      *  01.00.01    ����     2009/06/10   �ݸ���̾�Х��ȿ�����
      *  01.00.02    ����     2010/11/01   ��������������б�
      *  01.00.03    ���     2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2530.
      *    NO
           03  SKT2530-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT2530-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT2530-SEIYMD              PIC  X(22).
      *    ����
           03  SKT2530-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT2530-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT2530-KAISETUNAME         PIC  X(80).
      *    �����ֹ�
           03  SKT2530-TEL                 PIC  X(15).
      *    �ݸ���̾
           03  SKT2530-HKNJANAME           PIC  X(40).
      *    �ݸ����ֹ�
           03  SKT2530-HKNJANUM            PIC  X(06).
      *    ���ŵ��إ�����
           03  SKT2530-HOSPCD              PIC  X(07).
      *    �����ǡ��߰���
           03  SKT2530-ZAISOU              PIC  X(02).
      *    ����������
           03  SKT2530-TOKUBETU            PIC  X(10).
      *    ���ճ��
           03  SKT2530-KYURATE-TBL  OCCURS  4.
               05  SKT2530-KYURATE         PIC  X(02).
      *    ����ơ��֥�
           03  SKT2530-SEIKYU-TBL  OCCURS  9.
      *    �����ơ��֥�
               05  SKT2530-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2530-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2530-NISSU       PIC  X(05).
      *    ����
                   07  SKT2530-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2530-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2530-SKENSU          PIC  X(05).
      *    �������ܡ�����
               05  SKT2530-SNISSU          PIC  X(05).
      *    �������ܡ����
               05  SKT2530-SKINGK          PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2530-SFTNGK          PIC  X(09).
