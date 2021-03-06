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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    11/03/04    ��������������б�
      *  01.00.02    ���    19/02/04    �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4610.
      *�ڡ����ֹ�
           03  SKT4610-NO                  PIC  X(04).
      *��ɽ��ʬ
           03  SKT4610-TENHYOKBN           PIC  X(02)  OCCURS 2.
      *����ǯ��
           03  SKT4610-SRYYM               PIC  X(16).
      *�ݸ���̾
           03  SKT4610-HKNJANAME           PIC  X(30).
      *����ǯ����
           03  SKT4610-SEIYMD              PIC  X(22).
      *�ݸ����ֹ�
           03  SKT4610-HKNJANUM            PIC  X(12).
      *���ŵ��إ�����
           03  SKT4610-HOSPCD              PIC  X(14).
      *���ŵ��ؽ���
           03  SKT4610-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4610-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT4610-TEL                 PIC  X(15).
      *���߼�̾
           03  SKT4610-KAISETU             PIC  X(40).
      *����
           03  SKT4610-ZAI1                PIC  X(02).
           03  SKT4610-ZAI2                PIC  X(02).
      *����������
           03  SKT4610-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT4610-SEIKYU  OCCURS  9.
               05  SKT4610-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4610-KENSU        PIC  X(05).
      *����
                   07 SKT4610-NISSU        PIC  X(05).
      *����
                   07 SKT4610-TENSU        PIC  X(09).
      *������ô��
                   07 SKT4610-ITBFTN       PIC  X(09).
      *�������
               05  SKT4610-SKENSU          PIC  X(05).
      *��������
               05  SKT4610-SNISSU          PIC  X(05).
      *����������
               05  SKT4610-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4610-SFTNGK          PIC  X(09).
