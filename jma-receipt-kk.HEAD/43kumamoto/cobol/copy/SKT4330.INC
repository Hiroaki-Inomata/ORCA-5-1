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
       01  SKT4330.
      *�ڡ����ֹ�
           03  SKT4330-NO                  PIC  X(04).
      *����ǯ��
           03  SKT4330-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT4330-SEIYMD              PIC  X(22).
      *���ŵ��ؽ����
           03  SKT4330-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4330-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT4330-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT4330-KAISETU             PIC  X(40).
      *���ŵ��إ�����
           03  SKT4330-HOSPCDN             PIC  X(20).
      *�ݸ����ֹ�
           03  SKT4330-HKNJANUM            PIC  X(12).
      *�ݸ���̾
           03  SKT4330-HKNJANAME           PIC  X(24).
      *���̡������å�����
           03  SKT4330-ZAI                 PIC  X(32).
      *����������
           03  SKT4330-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT4330-SEIKYU  OCCURS  2.
               05  SKT4330-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4330-KENSU        PIC  X(05).
      *����
                   07 SKT4330-NISSU        PIC  X(05).
      *����
                   07 SKT4330-TENSU        PIC  X(09).
      *������ô��
                   07 SKT4330-ITBFTN       PIC  X(09).
      *�������
               05  SKT4330-SKENSU          PIC  X(05).
      *��������
               05  SKT4330-SNISSU          PIC  X(05).
      *����������
               05  SKT4330-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4330-SFTNGK          PIC  X(09).
      *����ơ��֥�
           03  SKT4330-KOHSEIKYU  OCCURS  4.
      *ˡ���ֹ�
               05  SKT4330-HBTNUM          PIC  X(02).
      *����̾
      *         05  SKT4330-KOHNAME         PIC  X(10).
      *
               05  SKT4330-K-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4330-K-KENSU      PIC  X(05).
      *����
                   07 SKT4330-K-NISSU      PIC  X(05).
      *����
                   07 SKT4330-K-TENSU      PIC  X(09).
      *������ô��
                   07 SKT4330-K-ITBFTN     PIC  X(09).
      *�������
               05  SKT4330-K-SKENSU        PIC  X(05).
      *��������
               05  SKT4330-K-SNISSU        PIC  X(05).
      *����������
               05  SKT4330-K-SKINGK        PIC  X(09).
      *ɸ����ô��
               05  SKT4330-K-SFTNGK        PIC  X(09).
