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
       01  SKT3410.
      *    NO
           03  SKT3410-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT3410-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT3410-SEIYMD              PIC  X(22).
      *    ����
           03  SKT3410-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT3410-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT3410-KAISETUNAME         PIC  X(40).
      *    �ݸ����ֹ�
           03  SKT3410-HKNJANAME           PIC  X(20).
           03  SKT3410-HKNJANUM            PIC  X(06).
      *    ���ŵ��إ�����
           03  SKT3410-HOSPCD              PIC  X(20).
      *    ����������
           03  SKT3410-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT3410-SEIKYU-TBL  OCCURS  9.
      *    �����ơ��֥�
               05  SKT3410-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3410-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3410-NISSU       PIC  X(05).
      *    ����
                   07  SKT3410-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3410-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT3410-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3410-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3410-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3410-S-FTNGK         PIC  X(09).
