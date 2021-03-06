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
      *    ������������ʹ��θ���     COPY (SKT3925.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/09    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT3925.
      *    ����ǯ��
           03  SKT3925-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT3925-SEIYMD              PIC  X(22).
      *    ����
           03  SKT3925-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT3925-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT3925-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT3925-TEL                 PIC  X(15).
      *    �ݸ���̾
           03  SKT3925-HKNJANAME           PIC  X(100).
      *    �ݸ����ֹ�
           03  SKT3925-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT3925-HOSPCD              PIC  X(14).
      *    �����ǡ��߰���
           03  SKT3925-ZAI1                PIC  X(02).
           03  SKT3925-ZAI2                PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT3925-NO                  PIC  X(04).
      *    ������������ơ��֥�
           03  SKT3925-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT3925-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3925-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3925-NISSU       PIC  X(05).
      *    ����
                   07  SKT3925-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3925-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT3925-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3925-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3925-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3925-S-FTNGK         PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT3925-KOUHI-TBL  OCCURS  2.
      *    ˡ���ֹ�
               05  SKT3925-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT3925-KOHNAME         PIC  X(12).
      *    �����ơ��֥�
               05  SKT3925-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3925-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3925-KNISSU       PIC  X(05).
      *    ����
                   07  SKT3925-KTENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3925-KITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT3925-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3925-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3925-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3925-S-KFTNGK         PIC  X(09).
