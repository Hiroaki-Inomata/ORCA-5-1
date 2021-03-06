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
      *    ������������� ���̳�ƻ��     COPY (SKT0115.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/22    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/06/27  �ޥ�Ĺ���ɲ��б�
      *  01.00.02    ����    2008/07/23  ������ˡ�Ķ��ס�ɸ��װ����б�
      *                                  �ޥ�Ĺ�����������᤹
      *  01.00.03    ����    2018/09/10  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0115.
      *    ����ǯ��
           03  SKT0115-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT0115-ADRS                PIC X(100).
      *    ���ŵ���̾��
           03  SKT0115-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT0115-KAISETUNAME         PIC X(40).
      *    �ݸ����ֹ�
           03  SKT0115-HKNJANUM            PIC X(12).
      *    ���ŵ��إ�����
           03  SKT0115-HOSPCD              PIC X(14).
      *    �ݸ���̾
           03  SKT0115-HKNJANAME           PIC X(20).
      *    ����ǯ����
           03  SKT0115-SEIYMD              PIC X(22).
      *    ����
           03  SKT0115-ZAI1                PIC X(02).
           03  SKT0115-ZAI2                PIC X(02).
      *    �ڡ����ֹ�
           03  SKT0115-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT0115-SEIKYU-TBL   OCCURS  2.
      *    �����ơ��֥�
               05  SKT0115-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT0115-KENSU       PIC X(05).
      *    ����
                   07  SKT0115-NISSU       PIC X(05).
      *    ����
                   07  SKT0115-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT0115-ITBFTN      PIC X(09).
      *    �������
               05  SKT0115-S-KENSU         PIC X(05).
      *    ��������
               05  SKT0115-S-NISSU         PIC X(05).
      *    ���
               05  SKT0115-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT0115-S-FTNGK         PIC X(09).
      *    ��������ơ��֥�
           03   SKT0115-KOUHI-TBL   OCCURS  4.
      *    ˡ���ֹ�
               05  SKT0115-HBTNUM1         PIC X(02).
               05  SKT0115-HBTNUM2         PIC X(02).
      *    ���������ơ��֥�
               05  SKT0115-KNYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT0115-KKENSU      PIC X(05).
      *    ��������
                   07  SKT0115-KNISSU      PIC X(05).
      *    ��������
                   07  SKT0115-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT0115-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT0115-S-KKENSU        PIC X(05).
      *    ���񿩻�����
               05  SKT0115-S-KNISSU        PIC X(05).
      *    ���񿩻����
               05  SKT0115-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT0115-S-KFTNGK        PIC X(09).
      *    ���ͥơ��֥�
           03  SKT0115-BIKO-TBL.
      *    Ķ��
               05  SKT0115-TYOKA           PIC X(04).
               05  SKT0115-TYOKA-MARU      PIC X(02).
      *    ɸ��
               05  SKT0115-HYOKETU         PIC X(04).
               05  SKT0115-HYOKETU-MARU    PIC X(02).
