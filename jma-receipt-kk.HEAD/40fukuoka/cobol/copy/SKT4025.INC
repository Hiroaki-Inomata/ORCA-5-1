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
      *    ��������������ʡ������     COPY (SKT4025.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/06/10  ������ˡ���ֹ��ɲ�
      *  01.00.02    ���         19/01/10  �������򥳥��Ⱥ��
      ********************************************************************
          01  SKT4025.
      *    ����ǯ��
           03  SKT4025-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4025-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4025-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4025-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4025-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4025-TEL                 PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT4025-HOSPCD              PIC  X(20).
      *    �ݸ���̾
           03  SKT4025-HKNJANAME           PIC  X(100).
      *    �ݸ����ֹ�
           03  SKT4025-HKNJANUM            PIC  X(12).
      *    �����ǡ��߰���
           03  SKT4025-ZAI1                PIC  X(02).
           03  SKT4025-ZAI2                PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT4025-NO                  PIC  X(04).
      *    Ϸ������ơ��֥�
           03  SKT4025-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT4025-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4025-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT4025-NISSU       PIC  X(05).
      *    ����
                   07  SKT4025-TENSU       PIC  X(10).
      *    ������ô��
                   07  SKT4025-ITBFTN      PIC  X(10).
      *    �������ܡ����
               05  SKT4025-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT4025-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT4025-S-KINGK         PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4025-S-FTNGK         PIC  X(10).
      *    ��������ơ��֥�
           03  SKT4025-GENBK-TBL.
      *    ˡ���ֹ�
               05  SKT4025-GHBTNUM         PIC  X(02).
      *    �����ơ��֥�
               05  SKT4025-GNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4025-GKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT4025-GNISSU      PIC  X(05).
      *    ����
                   07  SKT4025-GTENSU      PIC  X(10).
      *    ������ô��
                   07  SKT4025-GITBFTN     PIC  X(10).
      *    ������ô��
                   07  SKT4025-GPTFTN      PIC  X(10).
      *    �������ܡ����
               05  SKT4025-S-GKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT4025-S-GNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT4025-S-GKINGK        PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4025-S-GFTNGK        PIC  X(10).
      *
      *    ��������ơ��֥�
           03  SKT4025-KOUHI-TBL  OCCURS  4.
      *    ����̾
               05  SKT4025-HBTNUM          PIC  X(02).
      *    �����ơ��֥�
               05  SKT4025-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT4025-KKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT4025-KNISSU      PIC  X(05).
      *    ����
                   07  SKT4025-KTENSU      PIC  X(10).
      *    ������ô��
                   07  SKT4025-KITBFTN     PIC  X(10).
      *    ������ô��
                   07  SKT4025-KPTFTN      PIC  X(10).
      *    �������ܡ����
               05  SKT4025-S-KKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT4025-S-KNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT4025-S-KKINGK        PIC  X(10).
      *    �������ܡ�ɸ����ô��
               05  SKT4025-S-KFTNGK        PIC  X(10).
