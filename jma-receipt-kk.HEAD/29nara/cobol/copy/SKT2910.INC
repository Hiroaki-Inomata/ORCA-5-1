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
      *  01.00.01    ����      2010/12/08   ��������������б�
      *  01.00.02    ����      2012/06/04   �����ֹ��ɲ�
      *  01.00.03    ����      2019/04/25   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2910.
      *����ǯ��
           03  SKT2910-SRYYM               PIC  X(16).
      *�ݸ���̾
           03  SKT2910-HKNJANAME           PIC  X(40).
      *����ǯ����
           03  SKT2910-SEIYMD              PIC  X(22).
      *�ݸ����ֹ�
           03  SKT2910-HKNJANUM            PIC  X(12).
      *���ŵ��إ�����
           03  SKT2910-HOSPCDN             PIC  X(20).
      *���ŵ��ؽ���
           03  SKT2910-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SKT2910-HOSPNAME            PIC  X(100).
      *���ŵ��������ֹ�
           03  SKT2910-HOSPTEL             PIC  X(15).
      *���߼�̾
           03  SKT2910-KAISETU             PIC  X(40).
      *��ɽ��ʬ
           03  SKT2910-TENHYOKBN           PIC  X(02).
      *���ճ��
           03  SKT2910-WARIAI              PIC  X(02)  OCCURS 4.
      *����
           03  SKT2910-ZAITAKU1            PIC  X(02).
           03  SKT2910-ZAITAKU2            PIC  X(02).
      *����������
           03  SKT2910-TOKUBETU            PIC  X(10).
      *�ڡ����ֹ�
           03  SKT2910-NO                  PIC  X(04).
      *���ܤε��եơ��֥�
           03  SKT2910-SEIKYU  OCCURS  9.
               05  SKT2910-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT2910-KENSU        PIC  X(05).
      *����
                   07 SKT2910-NISSU        PIC  X(05).
      *����
                   07 SKT2910-TENSU        PIC  X(09).
      *������ô��
                   07 SKT2910-ITBFTN       PIC  X(09).
      *�������
               05  SKT2910-SKENSU          PIC  X(05).
      *��������
               05  SKT2910-SNISSU          PIC  X(05).
      *����������
               05  SKT2910-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT2910-SFTNGK          PIC  X(09).
      *����ε��եơ��֥�
           03  SKT2910-KOHI    OCCURS  4.
      *����̾
               05  SKT2910-KOHNAME         PIC  X(04).
               05  SKT2910-KNYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT2910-K-KENSU       PIC  X(05).
      *����
                   07 SKT2910-K-NISSU       PIC  X(05).
      *����
                   07 SKT2910-K-TENSU       PIC  X(09).
      *������ô��
                   07 SKT2910-K-ITBFTN      PIC  X(09).
