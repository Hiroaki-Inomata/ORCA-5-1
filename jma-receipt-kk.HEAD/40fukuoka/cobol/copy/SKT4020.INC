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
          01  SKT4020.
      *    ����ǯ��
           03  SKT4020-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4020-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4020-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4020-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4020-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4020-TEL                 PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT4020-HOSPCD              PIC  X(20).
      *    �ݸ����ֹ�
           03  SKT4020-HKNJANUM            PIC  X(12).
      *    �����ǡ��߰���
           03  SKT4020-ZAISOU1             PIC  X(02).
           03  SKT4020-ZAISOU2             PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4020-NO                  PIC  X(04).
      *    ����������
           03  SKT4020-TOKUBETU            PIC X(10).
      *    Ϸ������ơ��֥�
           03  SKT4020-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT4020-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4020-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4020-NISSU       PIC  X(05).
      *    ����
                   07  SKT4020-TENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4020-ITBFTN      PIC  X(10).
      *    �������ܡ����
               05  SKT4020-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4020-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4020-S-KINGK         PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4020-S-FTNGK         PIC  X(10).
      *    Ϸ�͸�������ơ��֥�
           03  SKT4020-GENBK-TBL.
      *    �����ơ��֥�
               05  SKT4020-GNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4020-GKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4020-GNISSU       PIC  X(05).
      *    ����
                   07  SKT4020-GTENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4020-GITBFTN      PIC  X(10).
      *    ������ô��
                   07  SKT4020-GPTFTN       PIC  X(10).
      *    �������ܡ����
               05  SKT4020-S-GKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4020-S-GNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4020-S-GKINGK         PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4020-S-GFTNGK         PIC  X(10).
      *
      *    ��������ơ��֥�
           03  SKT4020-KOUHI-TBL  OCCURS  4.
      *    ����̾
               05  SKT4020-HBTNUM          PIC  X(02).
               05  SKT4020-KOHNAME         PIC  X(10).
      *    �����ơ��֥�
               05  SKT4020-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4020-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4020-KNISSU       PIC  X(05).
      *    ����
                   07  SKT4020-KTENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4020-KITBFTN      PIC  X(10).
      *    ������ô��
                   07  SKT4020-KPTFTN       PIC  X(10).
      *    �������ܡ����
               05  SKT4020-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4020-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4020-S-KKINGK         PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4020-S-KFTNGK         PIC  X(10).
