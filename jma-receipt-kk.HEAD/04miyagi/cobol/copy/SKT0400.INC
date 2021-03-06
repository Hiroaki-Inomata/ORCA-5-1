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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2011/06/15  ��������������б�
      *  01.00.02    ����    2012/06/12  �����ֹ�����
      *  01.00.03    ���    2019/12/09  �������򥳥��Ⱥ��
      *  01.00.04    ����    2020/02/10  ���������η������
      *  01.00.05    ���    2020/04/10  �̾���������ˤ����ư����������
      ********************************************************************
       01  SKT0400.
      *    ����ǯ��
           03  SKT0400-SRYYM             PIC  X(16).
      *    ���ŵ��إ�����
           03  SKT0400-HOSPCDN           PIC  X(40).
      *    ���ŵ��ؽ���
           03  SKT0400-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT0400-HOSPNAME          PIC  X(100).
      *    ���ŵ��������ֹ�
           03  SKT0400-HOSPTEL           PIC  X(15).
      *    ���߼Ի�̾
           03  SKT0400-KAISETU           PIC  X(40).
      *    ����������
           03  SKT0400-TOKUBETU          PIC  X(10).
      *    ����ơ��֥�
           03  SKT0400-SEIKYU-TBL     OCCURS  17.
      *    �����ơ��֥�
               05  SKT0400-NYUGAI-TBL      OCCURS   2.
      *    ���
                   07  SKT0400-KENSU     PIC  X(05).
      *    ������
                   07  SKT0400-NISSU     PIC  X(05).
      *    ����
                   07  SKT0400-TENSU     PIC  X(10).
      *    �������ܡ����
               05  SKT0400-S-KENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT0400-S-NISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT0400-S-KINGK     PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0400-S-FTNGK     PIC  X(09).
      *    ��������ơ��֥�
           03  SKT0400-KSEIKYU-TBL.
      *    �����ơ��֥�
               05  SKT0400-KNYUGAI-TBL      OCCURS   2.
      *    ���
                   07  SKT0400-KKENSU     PIC  X(05).
      *    ������
                   07  SKT0400-KNISSU     PIC  X(05).
      *    ����
                   07  SKT0400-KTENSU     PIC  X(10).
      *    �������ܡ����
               05  SKT0400-S-KKENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT0400-S-KNISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT0400-S-KKINGK       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0400-S-KFTNGK       PIC  X(09).
      *    ��ץơ��֥�
           03  SKT0400-GSEIKYU-TBL.
      *    �����ơ��֥�
               05  SKT0400-GNYUGAI-TBL      OCCURS   2.
      *    ���
                   07  SKT0400-GKENSU     PIC  X(05).
      *    ������
                   07  SKT0400-GNISSU     PIC  X(05).
      *    ����
                   07  SKT0400-GTENSU     PIC  X(10).
      *    �������ܡ����
               05  SKT0400-S-GKENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT0400-S-GNISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT0400-S-GKINGK       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0400-S-GFTNGK       PIC  X(09).
      *    ʿ�ѥơ��֥�
           03  SKT0400-HEIKIN-TBL.
      *    ��ס�����ʿ������
               05  SKT0400-N-HEIKIN      PIC  X(07).
      *    ��סݳ���ʿ������
               05  SKT0400-G-HEIKIN      PIC  X(07).
