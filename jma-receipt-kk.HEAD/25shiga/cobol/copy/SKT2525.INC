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
      *    �����������ɽ�ʼ��츩��     COPY (SKT2525.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/17    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������   ����         ����
      *  01.00.01    ����     2008/05/03   ����Ʒ���ˤ�ˡ���ֹ�����
      *  01.00.02    ����     2008/05/23   �������ΥХ��ȿ�����
      *  01.00.03    ���     2018/12/12   �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT2525.
      *    ����ǯ��
            03  SKT2525-SRYYM             PIC X(16).
      *    ����ǯ����
            03  SKT2525-SEIYMD            PIC X(22).
      *    �ݸ���̾
            03  SKT2525-HKNJANAME         PIC X(20).
      *    ����
            03  SKT2525-ADRS              PIC X(100).
      *    ���ŵ���̾
            03  SKT2525-HOSPNAME          PIC X(100).
      *    �����ֹ�
            03  SKT2525-TEL               PIC X(15).
      *    ���߼�̾
            03  SKT2525-KAISETUNAME       PIC X(40).
      *    ���ŵ��إ�����
            03  SKT2525-HOSPCDN            PIC X(14).
      *    �����ǡ��߰���
            03  SKT2525-ZAI1           PIC X(02).
            03  SKT2525-ZAI2           PIC X(02).
      *    �ڡ����ֹ�
            03  SKT2525-NO                PIC X(04).
      *    �ݸ����ֹ�
            03  SKT2525-SEIKYU-TBL  OCCURS  2 TIMES.
      *    �����ơ��֥�
              05  SKT2525-NYUGAI-TBL  OCCURS  2 TIMES.
      *    ���
                07  SKT2525-KENSU         PIC X(05).
      *    ���ż�����
                07  SKT2525-NISSU         PIC X(05).
      *    ����
                07  SKT2525-TENSU         PIC X(11).
      *    ������ô��
                07  SKT2525-ITBFTN        PIC X(11).
      *    �������ܡ����
              05  SKT2525-S-KENSU          PIC X(05).
      *    �������ܡ�����
              05  SKT2525-S-NISSU          PIC X(05).
      *    �������ܡ����
              05  SKT2525-S-KINGK          PIC X(11).
      *    �������ܡ�ɸ����ô��
              05  SKT2525-S-FTNGK          PIC X(11).
      *    ��������ơ��֥�
            03  SKT2525-KOUHI-TBL  OCCURS  4 TIMES.
      *    ����̾
              05  SKT2525-HBTNUM          PIC X(02).
      *    �����ơ��֥�
              05  SKT2525-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    ���
                07  SKT2525-KKENSU       PIC X(05).
      *    ���ż�����
                07  SKT2525-KNISSU       PIC X(05).
      *    ����
                07  SKT2525-KTENSU       PIC X(11).
      *    ������ô��
                07  SKT2525-KITBFTN      PIC X(11).
      *    �������ܡ����
              05  SKT2525-S-KKENSU        PIC X(05).
      *    �������ܡ�����
              05  SKT2525-S-KNISSU        PIC X(05).
      *    �������ܡ����
              05  SKT2525-S-KKINGK        PIC X(11).
      *    �������ܡ�ɸ����ô��
              05  SKT2525-S-KFTNGK        PIC X(11).
      *    Ĺ����ۼ��·��
            03  SKT2525-TYOKI-KENSU       PIC X(05).
