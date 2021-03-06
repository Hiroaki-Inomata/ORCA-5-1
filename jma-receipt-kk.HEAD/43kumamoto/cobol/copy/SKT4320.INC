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
      *  01.00.01    ����         11/02/10  ��������������б�
      *  01.00.02    ���         18/09/05  ���ץ����Ȥ���
      ********************************************************************
       01  SKT4320.
      *�ڡ����ֹ�
           03  SKT4320-NO                  PIC  X(04).
      *����ǯ��
           03  SKT4320-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT4320-SEIYMD              PIC  X(22).
      *���ŵ��ؽ����
           03  SKT4320-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4320-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT4320-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT4320-KAISETU             PIC  X(40).
      *���ŵ��إ�����
           03  SKT4320-HOSPCDN             PIC  X(20).
      *�ݸ����ֹ�
           03  SKT4320-HKNJANUM            PIC  X(12).
      *�ݸ���̾
           03  SKT4320-HKNJANAME           PIC  X(24).
      *���̡������å�����
           03  SKT4320-ZAI                 PIC  X(32).
      *����������
           03  SKT4320-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT4320-SEIKYU  OCCURS  7.
               05  SKT4320-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4320-KENSU        PIC  X(05).
      *����
                   07 SKT4320-NISSU        PIC  X(05).
      *����
                   07 SKT4320-TENSU        PIC  X(09).
      *������ô��
                   07 SKT4320-ITBFTN       PIC  X(09).
      *�������
               05  SKT4320-SKENSU          PIC  X(05).
      *��������
               05  SKT4320-SNISSU          PIC  X(05).
      *����������
               05  SKT4320-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4320-SFTNGK          PIC  X(09).
      *����ơ��֥�
           03  SKT4320-KOHSEIKYU  OCCURS  3.
      *ˡ���ֹ�
               05  SKT4320-HBTNUM          PIC  X(02).
      *���������ơ��֥�
               05  SKT4320-K-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4320-K-KENSU      PIC  X(05).
      *����
                   07 SKT4320-K-NISSU      PIC  X(05).
      *����
                   07 SKT4320-K-TENSU      PIC  X(09).
      *������ô��
                   07 SKT4320-K-ITBFTN     PIC  X(09).
      *�������
               05  SKT4320-K-SKENSU        PIC  X(05).
      *��������
               05  SKT4320-K-SNISSU        PIC  X(05).
      *����������
               05  SKT4320-K-SKINGK        PIC  X(09).
      *ɸ����ô��
               05  SKT4320-K-SFTNGK        PIC  X(09).
