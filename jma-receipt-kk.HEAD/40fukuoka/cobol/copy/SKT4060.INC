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
      ********************************************************************
          01  SKT4060.
      *    ����ǯ��
           03  SKT4060-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4060-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4060-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4060-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4060-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4060-TEL                 PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT4060-HOSPCD              PIC  X(20).
      *    �ݸ����ֹ�
           03  SKT4060-HKNJANUM            PIC  X(12).
      *    �����ǡ��߰���
           03  SKT4060-ZAISOU1             PIC  X(02).
           03  SKT4060-ZAISOU2             PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4060-NO                  PIC  X(04).
      *    ����ơ��֥�
           03  SKT4060-SEIKYU-TBL  OCCURS  5.
      *    �����ơ��֥�
               05  SKT4060-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4060-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4060-NISSU       PIC  X(05).
      *    ����
                   07  SKT4060-TENSU       PIC  X(10).
      *    �������ܡ����
               05  SKT4060-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4060-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4060-S-KINGK         PIC  X(10).
