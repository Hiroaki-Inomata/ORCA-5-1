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
      *  01.00.01    ����    2011/01/12  ��������������б�
      *  01.00.02    ����    2017/04/14  �����η������
      *  01.00.03    ����    2019/02/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4110.
      *    �ڡ����ֹ�
           03  SKT4110-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT4110-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4110-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4110-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4110-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4110-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4110-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT4110-HKNJANUM            PIC  X(06).
      *    ���ŵ��إ�����
           03  SKT4110-HOSPCD              PIC  X(07).
      *    �ݸ���̾
           03  SKT4110-HOKENJANAME         PIC  X(20).
      *    �����ǡ��߰���
           03  SKT4110-ZAI1                PIC  X(02).
           03  SKT4110-ZAI2                PIC  X(02).
      *     ����������
           03  SKT4110-TOKUBETU            PIC  X(10).
      *    ���ճ��
           03  SKT4110-KYURATE-TBL  OCCURS  4.
               05  SKT4110-KYURATE         PIC  X(02).
      *    Ĺ�����ܷ��
           03  SKT4110-TYOKENSU            PIC  X(05).
      *    ����ơ��֥�
           03  SKT4110-SEIKYU-TBL  OCCURS  9.
      *    �����ơ��֥�
               05  SKT4110-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4110-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4110-NISSU       PIC  X(05).
      *    ����
                   07  SKT4110-TENSU       PIC  X(10).
      *    ���ް�����ô��
                   07  SKT4110-YKZFTN      PIC  X(07).
      *    ������ô��
                   07  SKT4110-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4110-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4110-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4110-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4110-S-FTNGK         PIC  X(09).
