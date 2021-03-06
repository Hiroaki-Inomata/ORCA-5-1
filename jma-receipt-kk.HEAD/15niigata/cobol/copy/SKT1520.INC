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
      *  01.00.01    ����    2010/08/04  ��������������б�
      *  01.00.02    ����    2018/11/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT1520.
      *    �ڡ����ֹ�
           03  SKT1520-NO                PIC  X(04).
      *    �ݸ����ֹ�
           03  SKT1520-HKNJANUM          PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT1520-HOSPCD            PIC  X(20).
      *    ������
           03  SKT1520-ZAI1              PIC  X(02).
           03  SKT1520-ZAI2              PIC  X(02).
      *    ����������
           03  SKT1520-TOKUBETU          PIC  X(10).
      *    ʡ������ơ��֥�
           03  SKT1520-F-SEIKYU-TBL     OCCURS  6.
      *    ���
               05  SKT1520-F-KENSU             PIC  X(05).
      *    ����  
               05  SKT1520-F-TENSU             PIC  X(09).
      *    �������
               05  SKT1520-F-SKENSU            PIC  X(05).
      *    �������
               05  SKT1520-F-SKINGK            PIC  X(09).
      *    ����ɸ����ô��
               05  SKT1520-F-SFTNGK            PIC  X(09).
      *    ��������ơ��֥�
           03  SKT1520-K-SEIKYU-TBL     OCCURS 2.
      *    ˡ���ֹ�
               05  SKT1520-K-HBTNUM            PIC  X(02).
      *    ����̾
               05  SKT1520-K-KOHNAME           PIC  X(08).
      *    �����ơ��֥�
               05  SKT1520-K-NYUGAI-TBL OCCURS 2.
      *    ���
                   07  SKT1520-K-KENSU         PIC  X(05).
      *    ����
                   07  SKT1520-K-NISSU         PIC  X(05).
      *    ����
                   07  SKT1520-K-TENSU         PIC  X(09).
      *    ������ô��
                   07  SKT1520-K-ITBFTN        PIC  X(09).
      *    �������
               05  SKT1520-K-SKENSU            PIC  X(05).
      *    ��������
               05  SKT1520-K-SNISSU            PIC  X(05).
      *    �������
               05  SKT1520-K-SKINGK            PIC  X(09).
      *    ����ɸ����ô��
               05  SKT1520-K-SFTNGK            PIC  X(09).
