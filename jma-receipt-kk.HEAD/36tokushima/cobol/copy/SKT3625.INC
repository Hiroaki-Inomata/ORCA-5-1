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
      *    �����������������縩��     COPY (SKT3625.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/17    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/06/08  ��������ۡ���������ΥХ��ȿ�����
      *  01.00.02    ����    2015/02/12  ���߼Ի�̾�ΥХ��ȿ�����
      ********************************************************************
        01  SKT3625.
      *    ����ǯ��
            03  SKT3625-SRYYM   PIC X(16).
      *    ���ŵ��إ�����
            03  SKT3625-HOSPCDN PIC X(20).
      *    �ݸ����ֹ�
            03  SKT3625-HKNJANUM    PIC X(16).
      *    ����
            03  SKT3625-ADRS    PIC X(100).
      *    ���ŵ���̾
            03  SKT3625-HOSPNAME    PIC X(100).
      *    �����ֹ�
            03  SKT3625-TEL PIC X(15).
      *    ���߼�̾
            03  SKT3625-KAISETUNAME PIC X(60).
      *    �����ǡ��߰���
            03  SKT3625-ZAI1    PIC X(2).
            03  SKT3625-ZAI2    PIC X(2).
      *    �ڡ����ֹ�
            03  SKT3625-NO  PIC X(4).
      *    ����ǯ����
            03  SKT3625-SEIYMD    PIC X(22).
      *    ����ơ��֥�
            03  SKT3625-SEIKYU-TBL      OCCURS  2 TIMES.
      *    �����ơ��֥�
              05  SKT3625-NYUGAI-TBL      OCCURS  2 TIMES.
      *    ���
                07  SKT3625-KENSU   PIC X(5).
      *    ���ż�����
                07  SKT3625-NISSU   PIC X(5).
      *    ����
                07  SKT3625-TENSU   PIC X(10).
      *    ������ô��
                07  SKT3625-ITBFTN  PIC X(9).
      *    �������ܡ����
              05  SKT3625-S-KENSU PIC X(5).
      *    �������ܡ�����
              05  SKT3625-S-NISSU PIC X(6).
      *    �������ܡ����
              05  SKT3625-S-KINGK PIC X(10).
      *    �������ܡ�ɸ����ô��
              05  SKT3625-S-FTNGK PIC X(9).
      *    ��������ơ��֥�
            03  SKT3625-KOUHI-TBL       OCCURS  4 TIMES.
      *    �����ơ��֥�
              05  SKT3625-KNYUGAI-TBL     OCCURS  2 TIMES.
      *    ���
                07  SKT3625-KKENSU  PIC X(5).
      *    ���ż�����
                07  SKT3625-KNISSU  PIC X(5).
      *    ����
                07  SKT3625-KTENSU  PIC X(10).
      *    ������ô��
                07  SKT3625-KITBFTN PIC X(9).
      *    ����̾
              05  SKT3625-KOHNAME PIC X(10).
              05  SKT3625-HBTNUM  PIC X(4).
      *    �������ܡ����
              05  SKT3625-S-KKENSU    PIC X(5).
      *    �������ܡ�����
              05  SKT3625-S-KNISSU    PIC X(6).
      *    �������ܡ����
              05  SKT3625-S-KKINGK    PIC X(10).
      *    �������ܡ�ɸ����ô��
              05  SKT3625-S-KFTNGK    PIC X(9).
