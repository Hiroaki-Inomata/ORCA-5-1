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
      *    �����������ɽ��¾�ܸ�ʬ������ܡ�
      *
      *  ��������    ��ȼ�        ����
      *  08/04/24    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������   ����         ����
      * 01.00.01     ���     2018/12/03   �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT2715.
      *    ����ǯ��
            03  SKT2715-SRYYM             PIC X(16).
      *    ����ǯ����
            03  SKT2715-SEIYMD            PIC X(22).
      *    �ݸ���̾
            03  SKT2715-HKNJANAME         PIC X(20).
      *    ����
            03  SKT2715-ADRS              PIC X(100).
      *    ���ŵ���̾
            03  SKT2715-HOSPNAME          PIC X(100).
      *    ���߼�̾
            03  SKT2715-KAISETUNAME       PIC X(40).
      *    �����ֹ�
            03  SKT2715-TEL               PIC X(15).
      *    ���ŵ��إ�����
            03  SKT2715-HOSPCD            PIC X(20).
      *    �ݸ����ֹ�
            03  SKT2715-HKNJANUM          PIC X(16).
      *    �����ǡ��߰���
            03  SKT2715-ZAI1              PIC X(2).
            03  SKT2715-ZAI2              PIC X(2).
      *    �ڡ����ֹ�
            03  SKT2715-NO                PIC X(4).
      *    �Ƚ��������
            03  SKT2715-MENJO             PIC X(5).
      *    ����ơ��֥�
            03  SKT2715-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
              05  SKT2715-NYUGAI-TBL  OCCURS  2.
      *    ���
                07  SKT2715-KENSU         PIC X(5).
      *    ���ż�����
                07  SKT2715-NISSU         PIC X(5).
      *    ����
                07  SKT2715-TENSU         PIC X(10).
      *    ������ô��
                07  SKT2715-ITBFTN        PIC X(10).
      *    �������ܡ����
              05  SKT2715-S-KENSU         PIC X(5).
      *    �������ܡ�����
              05  SKT2715-S-NISSU         PIC X(5).
      *    �������ܡ����
              05  SKT2715-S-KINGK         PIC X(10).
      *    �������ܡ�ɸ����ô��
              05  SKT2715-S-FTNGK         PIC X(10).
      *    ��������ơ��֥�
            03  SKT2715-KOUHI-TBL  OCCURS  2.
      *    ����̾
              05  SKT2715-HBTNUM          PIC X(2).
              05  SKT2715-KOHNAME         PIC X(10).
      *    �����ơ��֥�
              05  SKT2715-KNYUGAI-TBL  OCCURS  2.
      *    ���
                07  SKT2715-KKENSU        PIC X(5).
      *    ���ż�����
                07  SKT2715-KNISSU        PIC X(5).
      *    ����
                07  SKT2715-KTENSU        PIC X(10).
      *    ������ô��
                07  SKT2715-KITBFTN       PIC X(10).
      *    �������ܡ����
              05  SKT2715-S-KKENSU        PIC X(5).
      *    �������ܡ�����
              05  SKT2715-S-KNISSU        PIC X(5).
      *    �������ܡ����
              05  SKT2715-S-KKINGK        PIC X(10).
      *    �������ܡ�ɸ����ô��
              05  SKT2715-S-KFTNGK        PIC X(10).
