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
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������  ����      ����
      *  01.00.01    ����    09/02/06  ���߼�̾�Х��ȿ�����
      *  01.00.02    ����    11/01/11  ��������������б�
      *  01.00.03    ���    18/10/09  ���ץ����Ȥ���
      *****************************************************************
       01  SKT0810.
      *����ǯ��
           03  SKT0810-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT0810-SEIYMD              PIC  X(22).
      *���ŵ��إ�����
           03  SKT0810-HOSPCDN             PIC  X(20).
      *�ݸ����ֹ�
           03  SKT0810-HKNJANUM            PIC  X(12).
      *���ŵ��ؽ����
           03  SKT0810-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT0810-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT0810-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT0810-KAISETUNAME         PIC  X(60).
      *�ݸ���̾
           03  SKT0810-HKNJANAME           PIC  X(24).
      *�������߰���
           03  SKT0810-ZAI1                PIC  X(02).
           03  SKT0810-ZAI2                PIC  X(02).
      *�ڡ����ֹ�
           03  SKT0810-NO                  PIC  X(04).
      *    ����������
           03  SKT0810-TOKUBETU            PIC  X(10).
      *
      *�������
           03  SKT0810-SEIKYU  OCCURS 9.
      *����������ơ��֥�
               05  SKT0810-NYUGAIKBN   OCCURS  2.
      *���ܤε��ա����
                   07  SKT0810-KENSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT0810-NISSU       PIC  X(05).
      *���ܤε��ա�����
                   07  SKT0810-TENSU       PIC  X(09).
      *���ܤε��ա�������ô��
                   07  SKT0810-ITBFTN      PIC  X(09).
      *�������ܡ����
               05  SKT0810-S-KENSU          PIC  X(05).
      *�������ܡ�����
               05  SKT0810-S-NISSU          PIC  X(05).
      *�������ܡ����
               05  SKT0810-S-KINGK          PIC  X(09).
      *�������ܡ�ɸ����ô��
               05  SKT0810-S-FTNGK          PIC  X(09).
