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
      *    ������������� �ʴ�긩��     COPY (SKT0315.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/23    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������   ����         ����
      * 01.00.01     ����     2008/05/03   ���ŵ��إ����ɥХ��ȿ�����
      * 01.00.02     ����     2012/04/19   Ģɼ�ͼ����ѹ�
      * 01.00.03     ����     2015/04/01   Ģɼ�ͼ����ѹ�
      ********************************************************************
       01  SKT0315.
      *    ����ǯ��
           03  SKT0315-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT0315-ADRS                PIC X(100).
      *    ���ŵ���TEL
           03  SKT0315-TEL                 PIC X(15).
      *    ���ŵ���̾��
           03  SKT0315-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT0315-KAISETUNAME         PIC X(40).
      *    �ݸ����ֹ�
           03  SKT0315-HKNJANUM            PIC X(12).
      *    ���ŵ��إ�����
           03  SKT0315-HOSPCD              PIC X(40).
      *    �ݸ���̾
           03  SKT0315-HKNJANAME           PIC X(100).
      *    ����ǯ����
           03  SKT0315-SEIYMD              PIC X(22).
      *    �ڡ����ֹ�
           03  SKT0315-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT0315-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT0315-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT0315-KENSU       PIC X(05).
      *    ����
                   07  SKT0315-NISSU       PIC X(05).
      *    ����
                   07  SKT0315-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT0315-ITBFTN      PIC X(09).
      *    Ĺ�����
                   07  SKT0315-TYOKI       PIC X(02).
      *    �������
               05  SKT0315-S-KENSU         PIC X(05).
      *    ��������
               05  SKT0315-S-NISSU         PIC X(05).
      *    ���
               05  SKT0315-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT0315-S-FTNGK         PIC X(09).
      *    ����ơ��֥�
           03  SKT0315-ZAI-TBL         OCCURS  2 TIMES.
      *    ���
               05  SKT0315-ZKENSU  PIC X(5).
      *    ����
               05  SKT0315-ZNISSU  PIC X(5).
      *    ����
               05  SKT0315-ZTENSU  PIC X(9).
      *    ������ô��
               05  SKT0315-ZITBFTN PIC X(9).
      *    Ĺ�����
               05  SKT0315-ZTYOKI  PIC X(2).
      *    �����������ơ��֥�
           03  SKT0315-KITEI-TBL  OCCURS  5.
      *    ������������ơ��֥�
               05  SKT0315-KINYUGAI-TBL OCCURS  2.
      *    ���������
                   07  SKT0315-KIKENSU     PIC X(05).
      *    �����������
                   07  SKT0315-KINISSU     PIC X(05).
      *    �����������
                   07  SKT0315-KITENSU     PIC X(09).
      *    ������������ô��
                   07  SKT0315-KIITBFTN    PIC X(09).
      *    ������񿩻����
               05  SKT0315-S-KIKENSU       PIC X(05).
      *    ������񿩻�����
               05  SKT0315-S-KINISSU       PIC X(05).
      *    ������񿩻����
               05  SKT0315-S-KIKINGK       PIC X(09).
      *    �������ɸ����ô��
               05  SKT0315-S-KIFTNGK       PIC X(09).
      *    ��������ơ��֥�
           03   SKT0315-KOUHI-TBL  OCCURS  3.
      *    ˡ���ֹ�
               05  SKT0315-HBTNUM          PIC X(02).
      *    ����̾
               05  SKT0315-KOHNAME         PIC X(10).
      *    ���������ơ��֥�
               05  SKT0315-KNYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT0315-KKENSU      PIC X(05).
      *    ��������
                   07  SKT0315-KNISSU      PIC X(05).
      *    ��������
                   07  SKT0315-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT0315-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT0315-S-KKENSU        PIC X(05).
      *    ���񿩻�����
               05  SKT0315-S-KNISSU        PIC X(05).
      *    ���񿩻����
               05  SKT0315-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT0315-S-KFTNGK        PIC X(09).
