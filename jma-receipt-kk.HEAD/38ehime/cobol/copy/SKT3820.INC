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
      *  01.00.02    ����      11/01/12    ��������������б�
      *  01.00.03    ����      11/08/19    ���߼�̾�ΥХ��ȿ�����
      *  01.00.04    ���      18/09/05    ���ץ����Ȥ���
      *****************************************************************
       01  SKT3820.
      *�ڡ����ֹ�
           03  SKT3820-NO                  PIC  X(04).
      *����ǯ��
           03  SKT3820-SRYYM               PIC  X(16).
      *����ǯ����
           03  SKT3820-SEIYMD              PIC  X(22).
      *���ŵ��إ�����
           03  SKT3820-HOSPCD              PIC  X(14).
      *���ŵ��ؽ���
           03  SKT3820-HOSPADRS            PIC  X(100).
      *���ŵ���̾��
           03  SKT3820-HOSPNAME            PIC  X(100).
      *���߼�̾
           03  SKT3820-KAISETU             PIC  X(60).
      *�ݸ���̾��
           03  SKT3820-HKNJANAME           PIC  X(24).
      *�ݸ����ֹ�
           03  SKT3820-HKNJANUM            PIC  X(12).
      *�����ǡ��߰���
           03  SKT3820-ZAI                 PIC  X(02) OCCURS 2.
      *����������
           03  SKT3820-TOKUBETU            PIC  X(10).
      *���ܤε��եơ��֥�
           03  SKT3820-SEIKYU  OCCURS  7.
               05  SKT3820-NYUGAI-TBL  OCCURS  2.
      *���
                   07 SKT3820-KENSU        PIC  X(06).
      *����
                   07 SKT3820-NISSU        PIC  X(06).
      *����
                   07 SKT3820-TENSU        PIC  X(10).
      *������ô��
                   07 SKT3820-ITBFTN       PIC  X(09).
      *�������ܥơ��֥�
           03  SKT3820-SHOKUJI-TBL OCCURS 7.
      *�������
               05  SKT3820-S-KENSU         PIC  X(06).
      *��������
               05  SKT3820-S-NISSU         PIC  X(06).
      *����������
               05  SKT3820-S-MONEY         PIC  X(09).
      *ɸ����ô��
               05  SKT3820-S-FTNGAKU       PIC  X(09).
