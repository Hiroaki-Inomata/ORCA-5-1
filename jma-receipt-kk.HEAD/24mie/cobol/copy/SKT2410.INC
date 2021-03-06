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
      *  01.00.01    ����      2010/11/01   ��������������б�
      *  01.00.02    ����      2019/05/23   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2410.
      *����ǯ��
           03  SKT2410-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT2410-SEIYMD              PIC  X(22).
      *���ŵ��ؽ���
           03  SKT2410-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SKT2410-HOSPNAME            PIC  X(100).
      *���߼�̾
           03  SKT2410-KAISETU             PIC  X(40).
      *�����ֹ�
           03  SKT2410-TEL                 PIC  X(15).
      *�ݸ����ֹ�
           03  SKT2410-HKNJANUM            PIC  X(12).
      *���ŵ��إ�����
           03  SKT2410-HOSPCDN             PIC  X(20).
      *�ݸ���ʬ
           03  SKT2410-HKNKBN              PIC  X(02) OCCURS 3.
      *�ڡ����ֹ�
           03  SKT2410-NO                  PIC  X(04).
      *    ����������
           03  SKT2410-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT2410-SEIKYU  OCCURS  6.
               05  SKT2410-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT2410-KENSU        PIC  X(05).
      *����
                   07 SKT2410-NISSU        PIC  X(05).
      *����
                   07 SKT2410-TENSU        PIC  X(09).
      *������ô��
                   07 SKT2410-ITBFTN       PIC  X(09).
      *�������
               05  SKT2410-S-KENSU         PIC  X(05).
      *��������
               05  SKT2410-S-NISSU         PIC  X(05).
      *����������
               05  SKT2410-S-MONEY         PIC  X(09).
      *ɸ����ô��
               05  SKT2410-S-FTNGAKU       PIC  X(09).
      *������
           03  SKT2410-KOHI                PIC  X(05).
      *�������
           03  SKT2410-GENBAKU             PIC  X(05).
      *������
           03  SKT2410-ZAITAKU             PIC  X(05).
