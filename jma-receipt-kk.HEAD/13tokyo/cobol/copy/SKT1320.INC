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
      *  01.00.01    ����    2010/08/03  ��������������б�
      *  01.00.02    ���    2018/11/02  �������򥳥��Ⱥ��
      *  01.00.03    ���    2019/09/02  ���߼Ի�̾�ΰ���ʸ����������
      ********************************************************************
       01  SKT1320.
      *    ����ǯ��
           03  SKT1320-SRYYM             PIC  X(16).
      *    ���ŵ��ؽ���
           03  SKT1320-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT1320-HOSPNAME          PIC  X(100).
      *    �����ֹ�
           03  SKT1320-TEL               PIC  X(15).
      *    ���߼�̾
           03  SKT1320-KAISETUNAME       PIC  X(40).
      *    ����ǯ����
           03  SKT1320-SEIYMD            PIC  X(22).
      *    �ݸ����ֹ�
           03  SKT1320-HKNJANUM          PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT1320-HOSPCD            PIC  X(14).
      *    ���ʰ����ݸ��ԡ�
           03  SKT1320-KYURATE           PIC  X(02).
      *    ������
           03  SKT1320-ZAI1              PIC  X(02).
           03  SKT1320-ZAI2              PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT1320-NO                PIC  X(04).
      *    ����������
           03  SKT1320-TOKUBETU          PIC  X(10).
      *    ����ơ��֥�
           03  SKT1320-SEIKYU-TBL     OCCURS  12.
      *    ���
               05  SKT1320-KENSU         PIC  X(05).
      *    ������
               05  SKT1320-NISSU         PIC  X(05).
      *    ����
               05  SKT1320-TENSU         PIC  X(09).
      *    ������ô��
               05  SKT1320-ITBFTN        PIC  X(09).
      *    �������
               05  SKT1320-S-KENSU       PIC  X(05).
      *    ����������
               05  SKT1320-S-NISSU       PIC  X(05).
      *    ���
               05  SKT1320-S-KINGK       PIC  X(09).
      *    ɸ����ô��
               05  SKT1320-S-FTNGK       PIC  X(09).
      *    ����ơ��֥�
           03  SKT1320-KOH-TBL        OCCURS   2.
      *    ����̾
               05  SKT1320-K-KOHNAME     PIC  X(16).
      *    ���
               05  SKT1320-KKENSU         PIC  X(05).
      *    ������
               05  SKT1320-KNISSU         PIC  X(05).
      *    ����
               05  SKT1320-KTENSU         PIC  X(09).
      *    ������ô��
               05  SKT1320-KITBFTN        PIC  X(09).
      *    �������
               05  SKT1320-S-KKENSU       PIC  X(05).
      *    ����������
               05  SKT1320-S-KNISSU       PIC  X(05).
      *    ���
               05  SKT1320-S-KKINGK       PIC  X(09).
      *    ɸ����ô��
               05  SKT1320-S-KFTNGK       PIC  X(09).
      *    ���߼�̾��
           03  SKT1320-KAISETUNAME2      PIC  X(60).
