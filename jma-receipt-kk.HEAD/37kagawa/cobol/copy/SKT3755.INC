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
      *    ������������� �ʹ����     COPY (SKT3755.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/23    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT3755.
      *    ����ǯ��
           03  SKT3755-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT3755-ADRS                PIC X(100).
      *    ���ŵ���TEL
           03  SKT3755-TEL                 PIC X(15).
      *    ���ŵ���̾��
           03  SKT3755-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT3755-KAISETUNAME         PIC X(40).
      *    ���ŵ��إ�����
           03  SKT3755-HOSPCD              PIC X(14).
      *    �ݸ���̾
           03  SKT3755-HKNJANAME           PIC X(20).
      *    ����ǯ����
           03  SKT3755-SEIYMD              PIC X(22).
      *    ����
           03  SKT3755-ZAI1                PIC X(02).
           03  SKT3755-ZAI2                PIC X(02).
      *    �ڡ����ֹ�
           03  SKT3755-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT3755-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT3755-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT3755-KENSU       PIC X(05).
      *    ����
                   07  SKT3755-NISSU       PIC X(05).
      *    ����
                   07  SKT3755-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT3755-ITBFTN      PIC X(09).
      *    �������
               05  SKT3755-S-KENSU         PIC X(05).
      *    ��������
               05  SKT3755-S-NISSU         PIC X(05).
      *    ���
               05  SKT3755-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT3755-S-FTNGK         PIC X(09).
      *    ��������ơ��֥�
           03   SKT3755-KOUHI-TBL  OCCURS 4.
      *    ˡ���ֹ�
               05  SKT3755-HBTNUM          PIC  X(04).
      *    ����̾
               05  SKT3755-KOHNAME         PIC  X(10).
      *    ���������ơ��֥�
               05  SKT3755-KNYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT3755-KKENSU      PIC X(05).
      *    ��������
                   07  SKT3755-KNISSU      PIC X(05).
      *    ��������
                   07  SKT3755-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT3755-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT3755-S-KKENSU        PIC X(05).
      *    ���񿩻�����
               05  SKT3755-S-KNISSU        PIC X(05).
      *    ���񿩻����
               05  SKT3755-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT3755-S-KFTNGK        PIC X(09).
