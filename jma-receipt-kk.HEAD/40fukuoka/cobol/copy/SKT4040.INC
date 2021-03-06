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
      *  01.00.01    ����    2011/02/09  ��������������б�
      *  01.00.02    ���    2019/01/10  �������򥳥��Ⱥ��
      ********************************************************************
          01  SKT4040.
      *    ����ǯ��
           03  SKT4040-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4040-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4040-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4040-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4040-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4040-TEL                 PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT4040-HOSPCD              PIC  X(20).
      *    �ݸ����ֹ�
           03  SKT4040-HKNJANUM            PIC  X(12).
      *    �����ǡ��߰���
           03  SKT4040-ZAISOU1             PIC  X(02).
           03  SKT4040-ZAISOU2             PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4040-NO                  PIC  X(04).
      *    ����������
           03  SKT4040-TOKUBETU            PIC X(10).
      *    ��������ơ��֥�
           03  SKT4040-KOUHI-TBL  OCCURS  7.
      *    ����̾
               05  SKT4040-HBTNUM          PIC  X(02).
               05  SKT4040-KOHNAME         PIC  X(10).
      *    �����ơ��֥�
               05  SKT4040-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4040-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4040-KNISSU       PIC  X(05).
      *    ����
                   07  SKT4040-KTENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4040-KITBFTN      PIC  X(10).
      *    ������ô��
                   07  SKT4040-KPTFTN       PIC  X(10).
      *    �������ܡ����
               05  SKT4040-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4040-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4040-S-KKINGK         PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4040-S-KFTNGK         PIC  X(10).
