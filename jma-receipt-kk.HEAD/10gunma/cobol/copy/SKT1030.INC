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
       01  SKT1030.
      *    ����ǯ��
           03  SKT1030-SRYYM             PIC  X(16).
      *    �ݸ���̾
           03  SKT1030-HKNJANAME         PIC  X(30).
      *    ����ǯ����
           03  SKT1030-SEIYMD            PIC  X(22).
      *    �߰���
           03  SKT1030-ZAI               PIC  X(02).
      *    ͹���ֹ�
      *     03  SKT1030-POST              PIC  X(18).
      *    ����
           03  SKT1030-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT1030-HOSPNAME          PIC  X(100).
      *    ���߼�̾
           03  SKT1030-KAISETUNAME       PIC  X(40).
      *    �����ֹ�
           03  SKT1030-TEL               PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT1030-HKNJANUM          PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT1030-HOSPCD            PIC  X(14).
      *    �ڡ����ֹ�
           03  SKT1030-NO                PIC  X(04).
      *    ����������
           03  SKT1030-TOKUBETU          PIC  X(10).
      *    ����ơ��֥�
           03  SKT1030-SEIKYU-TBL   OCCURS  3.
      *    �����ơ��֥�
               05  SKT1030-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT1030-KENSU     PIC  X(03).
      *    ������
                   07  SKT1030-KKENSU    PIC  X(03).
      *    ʡ����
                   07  SKT1030-FKENSU    PIC  X(03).
      *    ����
                   07  SKT1030-NISSU     PIC  X(05).
      *    ����
                   07  SKT1030-TENSU     PIC  X(09).
      *    ������ô��
                   07  SKT1030-ITBFTN    PIC  X(09).
      *    �������ܡ����
               05  SKT1030-S-KENSU       PIC  X(03).
      *    �������ܡ�����
               05  SKT1030-S-NISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT1030-S-KINGAKU     PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT1030-S-FTNGAKU     PIC  X(09).
