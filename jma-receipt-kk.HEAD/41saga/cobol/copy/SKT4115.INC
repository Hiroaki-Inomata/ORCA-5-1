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
      *    ������������� �ʺ��츩��     COPY (SKT4115.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/05/02    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         17/04/14  �����η������
      *  01.00.02    ����         19/02/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4115.
      *    �ڡ����ֹ�
           03  SKT4115-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT4115-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4115-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4115-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4115-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4115-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4115-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT4115-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT4115-HOSPCD              PIC  X(14).
      *    �ݸ���̾
           03  SKT4115-HKNJANAME           PIC  X(100).
      *    �����ǡ��߰���
           03  SKT4115-ZAI1                PIC  X(02).
           03  SKT4115-ZAI2                PIC  X(02).
      *    ���ճ��
           03  SKT4115-KYURATE-TBL  OCCURS  4.
               05  SKT4115-KYURATE         PIC  X(02).
      *    ����ơ��֥�
           03  SKT4115-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT4115-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4115-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4115-NISSU       PIC  X(05).
      *    ����
                   07  SKT4115-TENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4115-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT4115-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4115-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4115-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4115-S-FTNGK         PIC  X(09).
      *    Ĺ�����
           03  SKT4115-TYOKI               PIC  X(03).
      *
      *    ��������ơ��֥�
           03  SKT4115-KOUHI-TBL  OCCURS  5.
      *    ˡ���ֹ�
               05  SKT4115-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT4115-KOHNAME         PIC  X(10).
      *    �����ơ��֥�
               05  SKT4115-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4115-KKENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4115-KNISSU       PIC  X(05).
      *    ����
                   07  SKT4115-KTENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4115-KITBFTN      PIC  X(09).
      *    ������ô��
                   07  SKT4115-KPTFTN       PIC  X(09).
      *    �������ܡ����
               05  SKT4115-S-KKENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4115-S-KNISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4115-S-KKINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT4115-S-KFTNGK         PIC  X(09).
