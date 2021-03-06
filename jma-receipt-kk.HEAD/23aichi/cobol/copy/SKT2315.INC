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
      *  ������������ʰ��θ���     COPY (SKT2315.INC)               
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/05/20  �����������������ʤ��Զ�罤��
      *  01.00.02    ����    2008/06/24  ����̾�ѹ�
      *  01.00.03    ����    2013/11/05  �����ɲ�
      *  01.00.04    ����    2020/02/12  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2315.
      *    ����ǯ��
           03  SKT2315-SRYYM       PIC  X(16).
      *    ����ǯ����
           03  SKT2315-SEIYMD      PIC  X(22).
      *    �ݸ���̾
           03  SKT2315-HKNJANAME   PIC  X(100).
      *    �ݸ����ֹ�
           03  SKT2315-HKNJANUM    PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT2315-HOSPCD      PIC  X(14).
      *    ���ŵ��ؽ���
           03  SKT2315-ADRS        PIC  X(100).
      *    ���ŵ���̾��
           03  SKT2315-HOSPNAME    PIC  X(100).
      *    ���ŵ��������ֹ�
           03  SKT2315-TEL         PIC  X(15).
      *    ���ŵ��س��߼�̾
           03  SKT2315-KAISETUNAME PIC  X(40).
      *    ����
           03  SKT2315-ZAI1        PIC  X(02).
           03  SKT2315-ZAI2        PIC  X(02).
      *    �ڡ����ֹ�
           03  SKT2315-NO          PIC  X(04).
           03  SKT2315-KOHKENSU    PIC  X(09).
      *    ���ܤε��եơ��֥�
           03  SKT2315-SEIKYU-TBL OCCURS 2.
      *    �����ơ��֥�
               05  SKT2315-NYUGAI-TBL OCCURS 2.
      *    ���
                   07  SKT2315-KENSU      PIC X(05).
      *    ����
                   07  SKT2315-NISSU      PIC X(05).
      *    ����
                   07  SKT2315-TENSU      PIC X(09).
      *    ������ô��
                   07  SKT2315-ITBFTN     PIC X(09).
      *    �����ݷ��
               05  SKT2315-S-KENSU         PIC X(05).
      *    ����������
               05  SKT2315-S-NISSU         PIC X(05).
      *    �����ݶ��
               05  SKT2315-S-KINGK         PIC X(09).
      *    ������ɸ����ô��
               05  SKT2315-S-FTNGK         PIC X(09).
      *    ���٤���
           03  SKT2315-TUKIOKURE          PIC X(05).
      *    Ĺ�����
           03  SKT2315-TYOKI              PIC X(05).
      *    �����������
           03  SKT2315-KOGAKU             PIC X(05).
      *    ����������
           03  SKT2315-TOKUBETU           PIC X(10).
