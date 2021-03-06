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
      *    ������������� �ʰ�ɲ����     COPY (SKT3825.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/28    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����         ����
      *  01.00.01    ����    2008/05/09   �奺�콤��
      *  01.00.02    ����    2011/08/19   ���߼�̾�ΥХ��ȿ�����
      *  01.00.03    ���    2018/09/05   ���ץ����Ȥ���
      ********************************************************************
       01  SKT3825.
      *    ����ǯ��
           03  SKT3825-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT3825-ADRS                PIC X(100).
      *    ���ŵ���̾��
           03  SKT3825-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT3825-KAISETUNAME         PIC X(60).
      *    �ݸ����ֹ�
           03  SKT3825-HKNJANUM            PIC X(16).
      *    ���ŵ��إ�����
           03  SKT3825-HOSPCD              PIC X(40).
      *    �ݸ���̾
           03  SKT3825-HKNJANAME           PIC X(100).
      *    ����ǯ����
           03  SKT3825-SEIYMD              PIC X(22).
      *    ����
           03  SKT3825-ZAI1                PIC X(02).
           03  SKT3825-ZAI2                PIC X(02).
      *    �ڡ����ֹ�
           03  SKT3825-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT3825-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT3825-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT3825-KENSU       PIC X(05).
      *    ����
                   07  SKT3825-NISSU       PIC X(05).
      *    ����
                   07  SKT3825-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT3825-ITBFTN      PIC X(09).
      *    �������
               05  SKT3825-S-KENSU         PIC X(05).
      *    ��������
               05  SKT3825-S-NISSU         PIC X(05).
      *    ���
               05  SKT3825-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT3825-S-FTNGK         PIC X(09).
      *    ��������ơ��֥�
           03   SKT3825-KOUHI-TBL.
      *    ���������ơ��֥�
               05  SKT3825-K-NYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT3825-KKENSU      PIC X(05).
      *    ��������
                   07  SKT3825-KNISSU      PIC X(05).
      *    ��������
                   07  SKT3825-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT3825-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT3825-S-KKENSU        PIC X(05).
      *    ���񿩻�����
               05  SKT3825-S-KNISSU        PIC X(05).
      *    ���񿩻����
               05  SKT3825-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT3825-S-KFTNGK        PIC X(09).
