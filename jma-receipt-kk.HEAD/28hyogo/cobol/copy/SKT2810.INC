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
      *  01.00.01    ����      2010/11/02   ��������������б�
      *  01.00.02    ����      2019/03/08   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2810.
      *����ǯ��
         03      SKT2810-SRYYM        PIC X(16).
      *���ŵ��ؽ���
         03      SKT2810-ADRS         PIC X(100).
      *���ŵ���̾��
         03      SKT2810-HOSPNAME     PIC X(100).
      *���߼�̾
         03      SKT2810-KAISETUNAME  PIC X(40).
      *�ݸ���̾
         03      SKT2810-HKNJANAME    PIC X(26).
      *�ݸ����ֹ�
         03      SKT2810-HKNJANUM     PIC X(12).
      *���ŵ��إ�����
         03      SKT2810-HOSPCD       PIC X(14).
      *����ǯ����
         03      SKT2810-SEIYMD       PIC X(22).
      *����
         03      SKT2810-ZAI1         PIC X(02).
         03      SKT2810-ZAI2         PIC X(02).
      *�ڡ����ֹ�
         03      SKT2810-NO           PIC X(04).
      *����������
         03      SKT2810-TOKUBETU     PIC  X(10).
      *��������ơ��֥�
         03      SKT2810-KOH-TBL      OCCURS 2.
      *������
           05    SKT2810-KOHKENSU     PIC X(05).
      *������ô��
           05    SKT2810-KOHITBFTN    PIC X(09).
      *����ơ��֥�
         03      SKT2810-SEIKYU-TBL   OCCURS 12.
      *�����ơ��֥�
           05    SKT2810-NYUGAI-TBL   OCCURS 2.
      *���
             07  SKT2810-KENSU        PIC X(05).
      *����
             07  SKT2810-NISSU        PIC X(05).
      *����
             07  SKT2810-TENSU        PIC X(09).
      *������ô��
             07  SKT2810-ITBFTN       PIC X(09).
      *�������
           05    SKT2810-SKENSU       PIC X(05).
      *��������
           05    SKT2810-SNISSU       PIC X(05).
      *���
           05    SKT2810-SKINGK       PIC X(09).
      *ɸ����ô��
           05    SKT2810-SFTNGK       PIC X(09).
      *�ޥ븶
         03      SKT2810-TOKUBETU1    PIC X(02) OCCURS 2.
      *�ޥ���
         03      SKT2810-TOKUBETU2    PIC X(02) OCCURS 2.
