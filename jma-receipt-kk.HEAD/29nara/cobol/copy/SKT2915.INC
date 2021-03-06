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
      ********************************************************************
      *    �����������������ɸ���     COPY (SKT2915.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/14    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/07/11  �����ֹ��ɲ�
      *  01.00.02    ����    2012/06/04  �����ֹ��ɲ�
      *  01.00.03    ����    2019/04/25  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2915.
      *����ǯ��
           03  SKT2915-SRYYM               PIC  X(16).
      *�ݸ���̾
           03  SKT2915-HKNJANAME           PIC  X(20).
      *����ǯ����
           03  SKT2915-SEIYMD              PIC  X(22).
      *�ݸ����ֹ�
           03  SKT2915-HKNJANUM            PIC  X(12).
      *���ŵ��إ�����
           03  SKT2915-HOSPCDN             PIC  X(20).
      *���ŵ��ؽ���
           03  SKT2915-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT2915-HOSPNAME            PIC  X(100).
      *���ŵ��������ֹ�
           03  SKT2915-HOSPTEL             PIC  X(15).
      *���߼�̾
           03  SKT2915-KAISETUNAME         PIC  X(40).
      *��ɽ��ʬ
           03  SKT2915-TENHYOKBN           PIC  X(02).
      *����
           03  SKT2915-ZAI1                PIC  X(02).
           03  SKT2915-ZAI2                PIC  X(02).
      *�ڡ����ֹ�
           03  SKT2915-NO                  PIC  X(04).
      *���ܤε��եơ��֥�
           03  SKT2915-SEIKYU  OCCURS  2.
               05  SKT2915-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT2915-KENSU        PIC  X(05).
      *����
                   07 SKT2915-NISSU        PIC  X(05).
      *����
                   07 SKT2915-TENSU        PIC  X(09).
      *������ô��
                   07 SKT2915-ITBFTN       PIC  X(09).
      *�������
               05  SKT2915-S-KENSU         PIC  X(05).
      *��������
               05  SKT2915-S-NISSU         PIC  X(05).
      *����������
               05  SKT2915-S-KINGK         PIC  X(09).
      *ɸ����ô��
               05  SKT2915-S-FTNGK         PIC  X(09).
      *����ε��եơ��֥�
           03  SKT2915-KOHI    OCCURS  5.
      *����̾
               05  SKT2915-KOHNAME         PIC  X(10).
      *�����ֹ�
               05  SKT2915-HBTNUM          PIC  X(02).
               05  SKT2915-KNYUGAI-TBL    OCCURS  2.
      *���
                   07 SKT2915-KKENSU       PIC  X(05).
      *����
                   07 SKT2915-KNISSU       PIC  X(05).
      *����
                   07 SKT2915-KTENSU       PIC  X(09).
      *������ô��
                   07 SKT2915-KITBFTN      PIC  X(09).
      *Ĺ�������������
           03  SKT2915-TYOKI               PIC  X(05).
