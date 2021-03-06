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
      *  01.00.02    ���    2019/12/09  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0520.
      *    NO
           03  SKT0520-NO                PIC  X(04).
      *    ����ǯ��
           03  SKT0520-SRYYM             PIC  X(16).
      *    ����ǯ����
           03  SKT0520-SEIYMD            PIC  X(22).
      *    ����ɽ��ʬ
           03  SKT0520-TENHYOKBN-TBL     OCCURS  2.
               05  SKT0520-TENHYOKBN     PIC  X(02).
      *    ���Ų�̾
           03  SKT0520-SRYKANAME         PIC  X(20).
      *    ���ŵ��إ�����
           03  SKT0520-HOSPCDN           PIC  X(20).
      *    ���ŵ��ؽ���
           03  SKT0520-ADRS              PIC  X(100).
      *    ���ŵ���̾
           03  SKT0520-HOSPNAME          PIC  X(100).
      *    ���߼�̾
           03  SKT0520-KAISETU           PIC  X(80).
      *    �ݸ����ֹ�
           03  SKT0520-HKNJANUM          PIC  X(12).
      *    ������
           03  SKT0520-ZAI1              PIC  X(02).
           03  SKT0520-ZAI2              PIC  X(02).
      *    ����������
           03  SKT0520-TOKUBETU          PIC  X(10).
      *    ����ơ��֥�
           03  SKT0520-SEIKYU-TBL      OCCURS  11.
               05  SKT0520-NYUGAI-TBL  OCCURS 2.
      *    ���
                   07  SKT0520-KENSU     PIC  X(05).
      *    ������
                   07  SKT0520-NISSU     PIC  X(05).
      *    ����
                   07  SKT0520-TENSU     PIC  X(09).
      *    ������ô��
                   07  SKT0520-ITBFTN    PIC  X(09).
      *    �������
               05  SKT0520-SKENSU        PIC  X(05).
      *    ����������
               05  SKT0520-SNISSU        PIC  X(05).
      *    ���
               05  SKT0520-SKINGK        PIC  X(09).
      *    ɸ����ô��
               05  SKT0520-SFTNGK        PIC  X(09).
      *    ����ʡ��ơ��֥�
           03  SKT0520-KOHI-TBL.
               05  SKT0520-KNYUGAI-TBL  OCCURS 2.
      *    ���
                   07  SKT0520-K-KENSU     PIC  X(05).
      *    ������
                   07  SKT0520-K-NISSU     PIC  X(05).
      *    ����
                   07  SKT0520-K-TENSU     PIC  X(09).
      *    ������ô��
                   07  SKT0520-K-ITBFTN    PIC  X(09).
      *    �������
               05  SKT0520-K-SKENSU        PIC  X(05).
      *    ����������
               05  SKT0520-K-SNISSU        PIC  X(05).
      *    ���
               05  SKT0520-K-SKINGK        PIC  X(09).
      *    ɸ����ô��
               05  SKT0520-K-SFTNGK        PIC  X(09).
