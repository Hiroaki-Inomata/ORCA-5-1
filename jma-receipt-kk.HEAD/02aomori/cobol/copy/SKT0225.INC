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
      *  ���������������Ŀ�����     COPY (SKT0225.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/17    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT0225.
      *    ����ǯ��
           03  SKT0225-SRYYM             PIC  X(16).
      *    ����ǯ����
           03  SKT0225-SEIYMD            PIC  X(22).
      *    �ݸ���̾
           03  SKT0225-HKNJANAME         PIC  X(20).
      *    ���ŵ��إ�����
           03  SKT0225-HOSPCD            PIC  X(40).
      *    ���ŵ��ؽ���
           03  SKT0225-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT0225-HOSPNAME          PIC  X(100).
      *    ���߼�̾
           03  SKT0225-KAISETUNAME       PIC  X(40).
      *    �����ֹ�
           03  SKT0225-TEL               PIC  X(15).
      *    ����
           03  SKT0225-ZAI1              PIC X(02).
           03  SKT0225-ZAI2              PIC X(02).
      *    �ڡ����ֹ�
           03  SKT0225-NO                PIC  X(04).
      *    ����ơ��֥�
           03  SKT0225-SEIKYU-TBL     OCCURS  2.
      *    �����ơ��֥�
               05  SKT0225-NYUGAI-TBL     OCCURS  2.
      *    ���
                   07  SKT0225-KENSU     PIC  X(05).
      *    ������
                   07  SKT0225-NISSU     PIC  X(05).
      *    ����
                   07  SKT0225-TENSU     PIC  X(09).
      *    ������ô��
                   07  SKT0225-ITBFTN    PIC  X(09).
      *    �������ܡ����
               05  SKT0225-S-KENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT0225-S-NISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT0225-S-KINGK       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0225-S-FTNGK       PIC  X(09).
      *    ����ơ��֥�
           03  SKT0225-KOH-TBL     OCCURS  5.
      *    ����̾
               05  SKT0225-KOHNAME       PIC  X(10).
      *    ˡ���ֹ�
               05  SKT0225-HBTNUM        PIC  X(02).
      *    �����ơ��֥�
               05  SKT0225-KNYUGAI-TBL      OCCURS  2.
      *    ���
                   07  SKT0225-KKENSU    PIC  X(05).
      *    ������
                   07  SKT0225-KNISSU    PIC  X(05).
      *    ����
                   07  SKT0225-KTENSU    PIC  X(09).
      *    ������ô��
                   07  SKT0225-KITBFTN   PIC  X(09).
      *    �������ܡ����
               05  SKT0225-S-KKENSU      PIC  X(05).
      *    �������ܡ�����
               05  SKT0225-S-KNISSU      PIC  X(05).
      *    �������ܡ����
               05  SKT0225-S-KKINGK      PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0225-S-KFTNGK      PIC  X(09).
