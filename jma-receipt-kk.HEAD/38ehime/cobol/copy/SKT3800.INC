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
      *  01.00.03    ���      18/09/05    ���ץ����Ȥ���
      *****************************************************************
       01  SKT3800.
      *����ǯ��
           03  SKT3800-SRYYM               PIC  X(16).
      *���ŵ��إ�����
           03  SKT3800-HOSPCD              PIC  X(14).
      *����������
           03  SKT3800-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT3800-SEIKYU  OCCURS  7.
               05  SKT3800-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT3800-KENSU        PIC  X(06).
      *����
                   07 SKT3800-TENSU        PIC  X(10).
      *������ô��
                   07 SKT3800-ITBFTN       PIC  X(10).
      *�������ܥơ��֥�
           03  SKT3800-SHOKUJI-TBL OCCURS 7.
      *�������
               05  SKT3800-S-KENSU         PIC  X(06).
      *����������
               05  SKT3800-S-MONEY         PIC  X(10).
      *ɸ����ô��
               05  SKT3800-S-FTNGAKU       PIC  X(10).
