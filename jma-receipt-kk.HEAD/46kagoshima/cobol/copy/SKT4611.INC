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
       01  SKT4611.
      *�ڡ����ֹ�
           03  SKT4611-NO                  PIC  X(04).
      *�ݸ����ֹ�
           03  SKT4611-HKNJANUM            PIC  X(12).
      *���ŵ��إ�����
           03  SKT4611-HOSPCD              PIC  X(14).
      *����
           03  SKT4611-ZAI1                PIC  X(02).
           03  SKT4611-ZAI2                PIC  X(02).
      *����������
           03  SKT4611-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT4611-ROJIN   OCCURS  2.
               05  SKT4611-R-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4611-KENSU      PIC  X(05).
      *����
                   07 SKT4611-NISSU      PIC  X(05).
      *����
                   07 SKT4611-TENSU      PIC  X(09).
      *������ô��
                   07 SKT4611-ITBFTN     PIC  X(09).
      *�������
               05  SKT4611-SKENSU        PIC  X(05).
      *��������
               05  SKT4611-SNISSU        PIC  X(05).
      *����������
               05  SKT4611-SKINGK        PIC  X(09).
      *ɸ����ô��
               05  SKT4611-SFTNGK        PIC  X(09).
      *���ܤε��եơ��֥�
           03  SKT4611-KOHI    OCCURS  7.
      *����̾
               05  SKT4611-HBTNUM          PIC  X(02).
               05  SKT4611-KOHNAME         PIC  X(10).
      *
               05  SKT4611-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT4611-K-KENSU        PIC  X(05).
      *����
                   07 SKT4611-K-NISSU        PIC  X(05).
      *����
                   07 SKT4611-K-TENSU        PIC  X(09).
      *������ô��
                   07 SKT4611-K-ITBFTN       PIC  X(09).
      *�������
               05  SKT4611-K-SKENSU          PIC  X(05).
      *��������
               05  SKT4611-K-SNISSU          PIC  X(05).
      *����������
               05  SKT4611-K-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4611-K-SFTNGK          PIC  X(09).
      *Ϸ��ʻ�ѥޥ븶
           03  SKT4611-MARUGEN.
      *�ޥ븶ʸ��
               05  SKT4611-MARUGEN1        PIC  X(06).
      *�ޥ븶���
               05  SKT4611-MARUGEN2-1      PIC  X(04).
               05  SKT4611-MARUGEN2-2      PIC  X(02).
