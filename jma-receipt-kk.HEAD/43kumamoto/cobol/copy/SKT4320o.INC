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
       01  SKT4320o.
      *�ڡ����ֹ�
           03  SKT4320o-NO                  PIC  X(04).
      *����ǯ��
           03  SKT4320o-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT4320o-SEIYMD              PIC  X(22).
      *���ŵ��ؽ����
           03  SKT4320o-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4320o-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT4320o-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT4320o-KAISETU             PIC  X(40).
      *���ŵ��إ�����
           03  SKT4320o-HOSPCDN             PIC  X(20).
      *�ݸ����ֹ�
           03  SKT4320o-HKNJANUM            PIC  X(12).
      *�ݸ���̾
           03  SKT4320o-HKNJANAME           PIC  X(24).
      *���̡������å�����
           03  SKT4320o-ZAI                 PIC  X(32).
      *���ܤε��եơ��֥�
           03  SKT4320o-SEIKYU  OCCURS  9.
               05  SKT4320o-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4320o-KENSU        PIC  X(05).
      *����
                   07 SKT4320o-NISSU        PIC  X(05).
      *����
                   07 SKT4320o-TENSU        PIC  X(09).
      *������ô��
                   07 SKT4320o-ITBFTN       PIC  X(09).
      *�������
               05  SKT4320o-SKENSU          PIC  X(05).
      *��������
               05  SKT4320o-SNISSU          PIC  X(05).
      *����������
               05  SKT4320o-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4320o-SFTNGK          PIC  X(09).