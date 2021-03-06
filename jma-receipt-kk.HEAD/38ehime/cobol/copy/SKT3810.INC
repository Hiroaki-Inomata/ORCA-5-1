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
      * Maj/Min/Rev  ������    ����        ����
      *  01.00.01    Ĺ��      08/04/29    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����      09/03/06    ��������������б�
      *  01.00.03    ����      12/04/11    Ģɼ�ͼ���ȼ������
      *  01.00.02    ���      18/09/05    ���ץ����Ȥ���
      *****************************************************************
       01  SKT3810.
      *����ǯ��
           03  SKT3810-SRYYM               PIC  X(16).
      *���ŵ��إ�����
           03  SKT3810-HOSPCD              PIC  X(14).
      *���ŵ��ؽ���
           03  SKT3810-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SKT3810-HOSPNAME            PIC  X(100).
      *����������
           03  SKT3810-TOKUBETU            PIC  X(10).
      *Ϸ�����ܤε��եơ��֥�
           03  SKT3810-SEIKYU  OCCURS  2.
               05  SKT3810-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT3810-KENSU        PIC  X(06).
      *����
                   07 SKT3810-TENSU        PIC  X(11).
      *������ô��
                   07 SKT3810-ITBFTN       PIC  X(10).
      *�������ܤε��եơ��֥�
           03  SKT3810-K-SEIKYU  OCCURS  2.
               05  SKT3810-K-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT3810-KKENSU        PIC  X(06).
      *����
                   07 SKT3810-KTENSU        PIC  X(11).
      *������ô��
                   07 SKT3810-KITBFTN       PIC  X(10).
      *Ϸ�Ϳ������ܥơ��֥�
           03  SKT3810-SHOKUJI-TBL OCCURS 2.
      *�������
               05  SKT3810-S-KENSU         PIC  X(06).
      *����������
               05  SKT3810-S-MONEY         PIC  X(11).
      *ɸ����ô��
               05  SKT3810-S-FTNGAKU       PIC  X(10).
      *���񿩻����ܥơ��֥�
           03  SKT3810-K-SHOKUJI-TBL OCCURS 2.
      *�������
               05  SKT3810-S-KKENSU         PIC  X(06).
      *����������
               05  SKT3810-S-KMONEY         PIC  X(11).
      *ɸ����ô��
               05  SKT3810-S-KFTNGAKU       PIC  X(10).
      *�����ݸ���
           03  SKT3810-KENNAI               PIC  X(02) OCCURS 22.
      *�츩���ݸ���
           03  SKT3810-KENNAI-OLD           PIC  X(120).
      *�����ݸ���̾
           03  SKT3810-KENGAI               PIC  X(350).
      *�����ݸ��Կ�
           03  SKT3810-KENNAISU             PIC  X(02).
      *�����ݸ��Կ�
           03  SKT3810-KENGAISU             PIC  X(02).
      *ʿ������
           03  SKT3810-AVG-HKN OCCURS 2.
               05  SKT3810-AVG-NYUGAI OCCURS 2.
                   07  SKT3810-AVG          PIC  X(07).
