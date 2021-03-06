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
      *    �������������Ĺ�긩��     COPY (SKT4215.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/10    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������     ����        ����
      *  01.00.01    ����       2008/04/29  ���ŵ��إ����ɥХ��ȿ�����
      *  01.00.02    ����       2008/05/21  ����̾�Х��ȿ�����
      *  01.00.03    ����       2019/02/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4215.
      *    ����ǯ��
           03  SKT4215-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4215-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4215-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4215-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4215-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4215-TEL                 PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT4215-HOSPCD              PIC  X(40).
      *    �ݸ���̾
           03  SKT4215-HKNJANAME           PIC  X(20).
      *    �ݸ����ֹ�
           03  SKT4215-HKNJANUM            PIC  X(12).
      *    �����ǡ��߰���
           03  SKT4215-ZAI1                PIC  X(02).
           03  SKT4215-ZAI2                PIC  X(02).
      *    Ĺ��
           03  SKT4215-TYOKI1              PIC  X(02).
           03  SKT4215-TYOKI2              PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4215-NO                  PIC  X(04).
      *    ����ơ��֥�
           03  SKT4215-SEIKYU-TBL  OCCURS  4.
      *    �����ơ��֥�
               05  SKT4215-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4215-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4215-NISSU       PIC  X(05).
      *    ����
                   07  SKT4215-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT4215-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4215-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4215-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4215-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4215-S-FTNGK         PIC  X(09).
      *    ����ơ��֥�
           03  SKT4215-KOH-TBL  OCCURS 4.
      *    ˡ���ֹ�
               05  SKT4215-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT4215-KOHNAME         PIC  X(10).
      *    �����ơ��֥�
               05  SKT4215-KNYUGAI-TBL   OCCURS  2.
      *    ���
                   07  SKT4215-KKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT4215-KNISSU      PIC  X(05).
      *    ����
                   07  SKT4215-KTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT4215-KITBFTN     PIC  X(09).
      *    �������ܡ����
               05  SKT4215-S-KKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT4215-S-KNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT4215-S-KKINGK        PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4215-S-KFTNGK        PIC  X(09).
