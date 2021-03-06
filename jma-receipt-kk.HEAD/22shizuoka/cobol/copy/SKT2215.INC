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
      *    ������������� ���Ų�����     COPY (SKT2215.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/25    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/11/19  �ݸ����ֹ�ΥХ��ȿ�����
      *  01.00.02    ����    2009/05/13  ��������ȶ�ۤΥХ��ȿ�����
      *  01.00.03    ����    2018/09/10  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2215.
      *    ����ǯ��
           03  SKT2215-SRYYM            PIC X(16).
      *    �ݸ����ֹ�
           03  SKT2215-HKNJANUM         PIC X(16).
      *    �ݸ���̾
           03  SKT2215-HKNJANAME        PIC X(20).
      *    ����
           03  SKT2215-ZAI1             PIC X(02).
           03  SKT2215-ZAI2             PIC X(02).
      *    ����ǯ����
           03  SKT2215-SEIYMD           PIC X(22).
      *    ���ŵ��إ�����
           03  SKT2215-HOSPCD           PIC X(07).
      *    ���ŵ��ؽ���
           03  SKT2215-ADRS             PIC X(100).
      *    ���ŵ���̾��
           03  SKT2215-HOSPNAME         PIC X(100).
      *    ���߼Ի�̾
           03  SKT2215-KAISETUNAME      PIC X(40).
      *    �ڡ����ֹ�
           03  SKT2215-NO               PIC X(04).
      *    ����ơ��֥�
           03  SKT2215-SEIKYU-TBL       OCCURS  2.
      *    �����ơ��֥�
               05  SKT2215-NYUGAI-TBL   OCCURS  2.
      *    ���
                   07  SKT2215-KENSU    PIC X(05).
      *    ���ż�����
                   07  SKT2215-NISSU    PIC X(05).
      *    ����
                   07  SKT2215-TENSU    PIC X(09).
      *    ������ô��
                   07  SKT2215-ITBFTN   PIC X(09).
      *    ���
               05  SKT2215-S-KENSU      PIC X(05).
      *    ���ż�����
               05  SKT2215-S-NISSU      PIC X(06).
      *    ���
               05  SKT2215-S-KINGK      PIC X(11).
      *    ɸ����ô��
               05  SKT2215-S-FTNGK      PIC X(09).
      *    ����ơ��֥�
           03  SKT2215-KOUHI-TBL        OCCURS  4.
      *    ����̾
               05  SKT2215-KOHNAME      PIC X(08).
      *    ˡ���ֹ�
               05  SKT2215-HBTNUM       PIC X(02).
      *    ���������ơ��֥�
               05  SKT2215-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2215-KKENSU   PIC X(05).
      *    ���ż�����
                   07  SKT2215-KNISSU   PIC X(05).
      *    ����
                   07  SKT2215-KTENSU   PIC X(09).
      *    ������ô��
                   07  SKT2215-KITBFTN  PIC X(09).
      *    ���
               05  SKT2215-S-KKENSU     PIC X(05).
      *    ���ż�����
               05  SKT2215-S-KNISSU     PIC X(06).
      *    ���
               05  SKT2215-S-KKINGK     PIC X(11).
      *    ɸ����ô��
               05  SKT2215-S-KFTNGK     PIC X(09).
      *    Ĺ��
           03  SKT2215-TYOKI            PIC X(07).
      *    ����
           03  SKT2215-GENBAKU          PIC X(07).
