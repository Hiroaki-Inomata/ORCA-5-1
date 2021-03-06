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
      *    �����������ɽ�ʻ�������
      *
      *  ��������    ��ȼ�        ����
      *  08/04/25    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
        01  SKT0915.
      *    ����ǯ��
            03  SKT0915-SRYYM             PIC X(16).
      *    ����ǯ����
            03  SKT0915-SEIYMD            PIC X(22).
      *    ����
            03  SKT0915-ADRS              PIC X(100).
      *    ���ŵ���̾
            03  SKT0915-HOSPNAME          PIC X(100).
      *    ���߼�̾
            03  SKT0915-KAISETUNAME       PIC X(40).
      *    ���ŵ��إ�����
            03  SKT0915-HOSPCD            PIC X(14).
      *    �ݸ����ֹ�
            03  SKT0915-HKNJANUM          PIC X(16).
      *    �����ǡ��߰���
            03  SKT0915-ZAI1              PIC X(2).
            03  SKT0915-ZAI2              PIC X(2).
      *    �ڡ����ֹ�
            03  SKT0915-NO                PIC X(4).
      *    ����ơ��֥�
            03  SKT0915-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
              05  SKT0915-NYUGAI-TBL  OCCURS  2.
      *    ���
                07  SKT0915-KENSU         PIC X(5).
      *    ���ż�����
                07  SKT0915-NISSU         PIC X(5).
      *    ����
                07  SKT0915-TENSU         PIC X(9).
      *    ������ô��
                07  SKT0915-ITBFTN        PIC X(9).
      *    �������ܡ����
              05  SKT0915-S-KENSU         PIC X(5).
      *    �������ܡ�����
              05  SKT0915-S-NISSU         PIC X(5).
      *    �������ܡ����
              05  SKT0915-S-KINGK         PIC X(9).
      *    �������ܡ�ɸ����ô��
              05  SKT0915-S-FTNGK         PIC X(9).
      *    ��������ơ��֥�
            03  SKT0915-KOUHI-TBL  OCCURS  4.
      *    ����̾
              05  SKT0915-HBTNUM          PIC X(2).
              05  SKT0915-KOHNAME         PIC X(10).
      *    �����ơ��֥�
             05  SKT0915-KNYUGAI-TBL  OCCURS  2.
      *    ���
                07  SKT0915-KKENSU        PIC X(5).
      *    ���ż�����
                07  SKT0915-KNISSU        PIC X(5).
      *    ����
                07  SKT0915-KTENSU        PIC X(9).
      *    ������ô��
                07  SKT0915-KITBFTN       PIC X(9).
      *    �������ܡ����
              05  SKT0915-S-KKENSU        PIC X(5).
      *    �������ܡ�����
              05  SKT0915-S-KNISSU        PIC X(5).
      *    �������ܡ����
              05  SKT0915-S-KKINGK        PIC X(9).
      *    �������ܡ�ɸ����ô��
              05  SKT0915-S-KFTNGK        PIC X(9).
