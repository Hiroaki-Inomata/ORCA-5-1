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
      *    ������������� �ʰ�븩��     COPY (SKT0815.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/22    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����      ����
      *  01.00.01    ����    09/02/06  ���߼�̾�Х��ȿ�����
      *  01.00.02    ���    18/10/09  ���ץ����Ȥ���
      ********************************************************************
       01  SKT0815.
      *    ����ǯ��
           03  SKT0815-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT0815-ADRS                PIC X(100).
      *    ���ŵ���̾��
           03  SKT0815-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT0815-KAISETUNAME         PIC X(60).
      *    ���ŵ��إ�����
           03  SKT0815-HOSPCD              PIC X(20).
      *    �ݸ���̾
           03  SKT0815-HKNJANAME           PIC X(20).
      *    ����ǯ����
           03  SKT0815-SEIYMD              PIC X(22).
      *    ����
           03  SKT0815-ZAI1                PIC X(02).
           03  SKT0815-ZAI2                PIC X(02).
      *    �ڡ����ֹ�
           03  SKT0815-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT0815-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT0815-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT0815-KENSU       PIC X(05).
      *    ����
                   07  SKT0815-NISSU       PIC X(05).
      *    ����
                   07  SKT0815-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT0815-ITBFTN      PIC X(09).
      *    �������
               05  SKT0815-S-KENSU         PIC X(05).
      *    ��������
               05  SKT0815-S-NISSU         PIC X(05).
      *    ���
               05  SKT0815-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT0815-S-FTNGK         PIC X(09).
      *    ��������ơ��֥�
           03   SKT0815-KOUHI-TBL  OCCURS 3.
      *    ˡ���ֹ�
               05  SKT0815-HBTNUM          PIC  X(02).
      *    ���������ơ��֥�
               05  SKT0815-KNYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT0815-KKENSU      PIC X(05).
      *    ��������
                   07  SKT0815-KNISSU      PIC X(05).
      *    ��������
                   07  SKT0815-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT0815-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT0815-S-KKENSU        PIC X(05).
      *    ���񿩻�����
               05  SKT0815-S-KNISSU        PIC X(05).
      *    ���񿩻����
               05  SKT0815-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT0815-S-KFTNGK        PIC X(09).
