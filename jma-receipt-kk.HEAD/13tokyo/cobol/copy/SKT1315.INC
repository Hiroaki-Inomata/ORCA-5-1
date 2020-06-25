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
      *    �������������(����)������ԡ�     COPY (SKT1315.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����        ����
      *  01.00.01    ���         2019/09/02  ���߼Ի�̾�ΰ���ʸ����������
      ********************************************************************
       01  SKT1315.
      *    ����ǯ��
           03  SKT1315-SRYYM             PIC  X(16).
      *    ���ŵ��ؽ���
           03  SKT1315-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT1315-HOSPNAME          PIC  X(100).
      *    �����ֹ�
           03  SKT1315-TEL               PIC  X(15).
      *    ���߼�̾
           03  SKT1315-KAISETUNAME       PIC  X(40).
      *    ����ǯ����
           03  SKT1315-SEIYMD            PIC  X(22).
      *    �ݸ���̾
           03  SKT1315-HKNJANANME        PIC  X(100).
      *    �ݸ����ֹ�
           03  SKT1315-HKNJANUM          PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT1315-HOSPCD            PIC  X(14).
      *    ������
           03  SKT1315-ZAI1              PIC  X(02).
           03  SKT1315-ZAI2              PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT1315-NO                PIC  X(04).
      *    ����ơ��֥�
           03  SKT1315-SEIKYU-TBL     OCCURS  2.
      *    ���
               05  SKT1315-KENSU         PIC  X(05).
      *    ������
               05  SKT1315-NISSU         PIC  X(05).
      *    ����
               05  SKT1315-TENSU         PIC  X(09).
      *    ������ô��
               05  SKT1315-ITBFTN        PIC  X(09).
      *    �������
               05  SKT1315-S-KENSU       PIC  X(05).
      *    ����������
               05  SKT1315-S-NISSU       PIC  X(05).
      *    ���
               05  SKT1315-S-KINGK       PIC  X(09).
      *    ɸ����ô��
               05  SKT1315-S-FTNGK       PIC  X(09).
      *    ����̾
           03  SKT1315-KOHNAME           PIC  X(16).
      *    ���
           03  SKT1315-KKENSU            PIC  X(05).
      *    ������
           03  SKT1315-KNISSU            PIC  X(05).
      *    ����
           03  SKT1315-KTENSU            PIC  X(11).
      *    ������ô��
           03  SKT1315-KITBFTN           PIC  X(09).
      *    �������
           03  SKT1315-S-KKENSU          PIC  X(05).
      *    ����������
           03  SKT1315-S-KNISSU          PIC  X(05).
      *    ���
           03  SKT1315-S-KKINGK          PIC  X(09).
      *    ɸ����ô��
           03  SKT1315-S-KFTNGK          PIC  X(09).
      *    ���߼�̾��
           03  SKT1315-KAISETUNAME2      PIC  X(60).