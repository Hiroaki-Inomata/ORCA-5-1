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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/05/16  ����Ʒ����2�Ԥ���7�Ԥ˽���
      *  01.00.02    ����    2009/03/12  ����ɽ��ʬ�ɲ�
      *  01.00.03    ���    2018/10/12  �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT0615.
      *    ����ǯ��
            03  SKT0615-SRYYM              PIC X(16).
      *    ����ǯ����
            03  SKT0615-SEIYMD             PIC X(22).
      *    �ݸ���̾
            03  SKT0615-HKNJANAME          PIC X(100).
      *    ����
            03  SKT0615-ADRS               PIC X(100).
      *    ���ŵ���̾
            03  SKT0615-HOSPNAME           PIC X(100).
      *    ���߼�̾
            03  SKT0615-KAISETUNAME        PIC X(40).
      *    ���ŵ��إ�����
            03  SKT0615-HOSPCD             PIC X(20).
      *    ����ɽ��ʬ
            03  SKT0615-TENHYOKBN          PIC X(02) OCCURS 2.
      *    �����ǡ��߰���
            03  SKT0615-ZAI1               PIC X(2).
            03  SKT0615-ZAI2               PIC X(2).
      *    �ڡ����ֹ�
            03  SKT0615-NO                 PIC X(4).
      *    ����ơ��֥�
            03  SKT0615-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
              05  SKT0615-NYUGAI-TBL  OCCURS  2.
      *    ���
                07  SKT0615-KENSU          PIC X(5).
      *    ���ż�����
                07  SKT0615-NISSU          PIC X(5).
      *    ����
                07  SKT0615-TENSU          PIC X(10).
      *    ������ô��
                07  SKT0615-ITBFTN         PIC X(10).
      *    �������ܡ����
              05  SKT0615-S-KENSU          PIC X(5).
      *    �������ܡ�����
              05  SKT0615-S-NISSU          PIC X(5).
      *    �������ܡ����
              05  SKT0615-S-KINGK          PIC X(10).
      *    �������ܡ�ɸ����ô��
              05  SKT0615-S-FTNGK          PIC X(10).
      *    ��������ơ��֥�
            03  SKT0615-KOUHI-TBL  OCCURS  7.
      *    ����̾
              05  SKT0615-HBTNUM           PIC X(2).
              05  SKT0615-KOHNAME          PIC X(10).
      *    �����ơ��֥�
              05  SKT0615-KNYUGAI-TBL  OCCURS  2.
      *    ���
                07  SKT0615-KKENSU         PIC X(5).
      *    ���ż�����
                07  SKT0615-KNISSU         PIC X(5).
      *    ����
                07  SKT0615-KTENSU         PIC X(10).
      *    ������ô��
                07  SKT0615-KITBFTN        PIC X(10).
      *    �������ܡ����
              05  SKT0615-S-KKENSU         PIC X(5).
      *    �������ܡ�����
              05  SKT0615-S-KNISSU         PIC X(5).
      *    �������ܡ����
              05  SKT0615-S-KKINGK         PIC X(10).
      *    �������ܡ�ɸ����ô��
              05  SKT0615-S-KFTNGK         PIC X(10).
