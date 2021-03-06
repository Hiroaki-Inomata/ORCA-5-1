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
      *  01.00.01    ����    2011/06/15  ��������������б�
      *  01.00.02    ����    2011/11/09  �ݸ���̾�ΥХ��ȿ�����
      *  01.00.03    ���    2019/12/09  �������򥳥��Ⱥ��
      *  01.00.04    ���    2020/04/10  �̾���������ˤ����ư����������
      ********************************************************************
       01  SKT0410.
      *    ����ǯ��
           03  SKT0410-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT0410-SEIYMD              PIC  X(22).
      *    ����
           03  SKT0410-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT0410-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT0410-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT0410-TEL                 PIC  X(15).
      *    �ݸ���̾
           03  SKT0410-HKNJANAME           PIC  X(40).
      *    �ݸ����ֹ�
           03  SKT0410-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT0410-HOSPCD              PIC  X(14).
      *    ����
           03  SKT0410-ZAI1                PIC  X(02).
           03  SKT0410-ZAI2                PIC  X(02).
      *    ���ճ��
           03  SKT0410-WARI                PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT0410-NO                  PIC  X(04).
      *    ����������
           03  SKT0410-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT0410-SEIKYU-TBL  OCCURS  13.
      *    �����ơ��֥�
               05  SKT0410-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT0410-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT0410-NISSU       PIC  X(05).
      *    ����
                   07  SKT0410-TENSU       PIC  X(10).
      *    ������ô��
                   07  SKT0410-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT0410-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT0410-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT0410-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0410-S-FTNGK         PIC  X(09).
      *    ��������ơ��֥�
           03  SKT0410-KOHI-TBL  OCCURS  9.
      *    �����ơ��֥�
               05  SKT0410-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT0410-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT0410-KNISSU       PIC  X(05).
      *    ����
                   07  SKT0410-KTENSU       PIC  X(10).
      *    ������ô��
                   07  SKT0410-KITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT0410-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT0410-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT0410-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0410-S-KFTNGK         PIC  X(09).
