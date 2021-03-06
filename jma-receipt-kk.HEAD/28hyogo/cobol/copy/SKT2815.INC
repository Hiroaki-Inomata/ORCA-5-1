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
      *    ������������� ��ʼ�˸���     COPY (SKT2815.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/15    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/05/21  ­��ʤ������������˵���
      *  01.00.02    ����    2019/03/08  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2815.
      *    ����ǯ��
           03  SKT2815-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT2815-ADRS                PIC X(100).
      *    ���ŵ���̾��
           03  SKT2815-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT2815-KAISETUNAME         PIC X(40).
      *    �ݸ����ֹ�
           03  SKT2815-HKNJANUM            PIC X(12).
      *    ���ŵ��إ�����
           03  SKT2815-HOSPCD              PIC X(14).
      *    �ݸ���̾
           03  SKT2815-HKNJANAME           PIC X(100).
      *    ����ǯ����
           03  SKT2815-SEIYMD              PIC X(22).
      *    ����
           03  SKT2815-ZAI1                PIC X(02).
           03  SKT2815-ZAI2                PIC X(02).
      *    �ڡ����ֹ�
           03  SKT2815-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT2815-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT2815-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT2815-KENSU       PIC X(05).
      *    ����
                   07  SKT2815-NISSU       PIC X(05).
      *    ����
                   07  SKT2815-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT2815-ITBFTN      PIC X(09).
      *    �������
               05  SKT2815-S-KENSU         PIC X(05).
      *    ��������
               05  SKT2815-S-NISSU         PIC X(05).
      *    ���
               05  SKT2815-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT2815-S-FTNGK         PIC X(09).
      *    ��������ơ��֥�
           03   SKT2815-KOUHI-TBL  OCCURS 7.
      *    ˡ���ֹ�
               05  SKT2815-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT2815-KOHNAME         PIC  X(12).
      *    �����󵭺�������
               05  SKT2815-NYUIN           PIC  X(06).
      *    �����󵭺���������
               05  SKT2815-NYUINGAI        PIC  X(06).
      *    ���������ơ��֥�
               05  SKT2815-KNYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT2815-KKENSU      PIC X(05).
      *    ��������
                   07  SKT2815-KNISSU      PIC X(05).
      *    ��������
                   07  SKT2815-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT2815-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT2815-S-KKENSU        PIC X(05).
      *    ���񿩻�����
               05  SKT2815-S-KNISSU        PIC X(05).
      *    ���񿩻����
               05  SKT2815-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT2815-S-KFTNGK        PIC X(09).
