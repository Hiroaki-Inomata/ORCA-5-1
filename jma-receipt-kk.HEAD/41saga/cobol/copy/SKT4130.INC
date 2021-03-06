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
       01  SKT4130.
      *    ����ǯ��
           03  SKT4130-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4130-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4130-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4130-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4130-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4130-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT4130-HKNJANUM            PIC  X(06).
      *    ���ŵ��إ�����
           03  SKT4130-HOSPCD              PIC  X(07).
      *    �ݸ���̾
           03  SKT4130-HOKENJANAME         PIC  X(20).
      *    �����ǡ��߰���
           03  SKT4130-ZAI1                PIC  X(02).
           03  SKT4130-ZAI2                PIC  X(02).
      *    ���ճ��
           03  SKT4130-KYURATE-TBL  OCCURS  4.
               05  SKT4130-KYURATE         PIC  X(02).
      *    Ϸ������ơ��֥�
           03  SKT4130-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT4130-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4130-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4130-NISSU       PIC  X(05).
      *    ����
                   07  SKT4130-TENSU       PIC  X(09).
      *    ���ް�����ô��
                   07  SKT4130-YKZFTN      PIC  X(07).
      *    ������ô��
                   07  SKT4130-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4130-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4130-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4130-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4130-S-FTNGK         PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT4130-KOUHI-TBL  OCCURS  5.
      *    ˡ���ֹ�
               05  SKT4130-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT4130-KOHNAME         PIC  X(10).
      *    �����ơ��֥�
               05  SKT4130-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4130-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4130-KNISSU       PIC  X(05).
      *    ����
                   07  SKT4130-KTENSU       PIC  X(09).
      *    ���ް�����ô��
                   07  SKT4130-KYKZFTN      PIC  X(07).
      *    ������ô��
                   07  SKT4130-KITBFTN      PIC  X(09).
      *    ������ô��
                   07  SKT4130-KKANFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4130-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4130-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4130-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4130-S-KFTNGK         PIC  X(09).
