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
      *  ������������ɽ�ʷ��ܸ���     COPY (SKT4325.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/18    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         12/03/14  �����ȶ�ۤη������
      *  01.00.02    ����         14/07/10  ���������η������
      *  01.00.03    ���         18/09/05  ���ץ����Ȥ���
      ********************************************************************
       01  SKT4325.
      *    �ڡ����ֹ�
           03  SKT4325-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT4325-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4325-SEIYMD              PIC  X(22).
      *    ���ŵ��ؽ����
           03  SKT4325-ADRS                PIC  X(100).
      *    ���ŵ���̾��
           03  SKT4325-HOSPNAME            PIC  X(100).
      *    �����ֹ�
           03  SKT4325-TEL                 PIC  X(15).
      *    ���߼Ի�̾
           03  SKT4325-KAISETUNAME         PIC  X(40).
      *    ���ŵ��إ�����
           03  SKT4325-HOSPCD              PIC  X(40).
      *    �ݸ���̾
           03  SKT4325-HKNJANAME           PIC  X(20).
      *    ���̡������å�����
           03  SKT4325-ZAI                 PIC  X(32).
      *    ���ܤε��եơ��֥�
           03  SKT4325-SEIKYU-TBL  OCCURS  2.
               05  SKT4325-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07 SKT4325-KENSU        PIC  X(05).
      *    ����
                   07 SKT4325-NISSU        PIC  X(05).
      *    ����
                   07 SKT4325-TENSU        PIC  X(10).
      *    ������ô��
                   07 SKT4325-ITBFTN       PIC  X(09).
      *    �������
               05  SKT4325-S-KENSU         PIC  X(05).
      *    ��������
               05  SKT4325-S-NISSU         PIC  X(06).
      *    ����������
               05  SKT4325-S-KINGK         PIC  X(10).
      *    ɸ����ô��
               05  SKT4325-S-FTNGK         PIC  X(09).
      *    ����ơ��֥�
           03  SKT4325-KOUHI-TBL  OCCURS  5.
      *    ˡ���ֹ�
               05  SKT4325-HBTNUM          PIC  X(02).
      *    ���������ơ��֥�
               05  SKT4325-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07 SKT4325-KKENSU       PIC  X(05).
      *    ����
                   07 SKT4325-KNISSU       PIC  X(05).
      *    ����
                   07 SKT4325-KTENSU       PIC  X(10).
      *    ������ô��
                   07 SKT4325-KITBFTN      PIC  X(09).
      *    �������
               05  SKT4325-S-KKENSU        PIC  X(05).
      *    ��������
               05  SKT4325-S-KNISSU        PIC  X(06).
      *    ����������
               05  SKT4325-S-KKINGK        PIC  X(10).
      *    ɸ����ô��
               05  SKT4325-S-KFTNGK        PIC  X(09).
