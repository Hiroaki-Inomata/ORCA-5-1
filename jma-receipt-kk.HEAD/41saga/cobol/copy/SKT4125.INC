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
      *    ������������񡦺������ѡʺ��츩��     COPY (SKT4125.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/05/21    ����          ��������
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         17/04/14  �����η������
      *  01.00.02    ����         19/02/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4125.
      *    �ڡ����ֹ�
           03  SKT4125-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT4125-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT4125-SEIYMD              PIC  X(22).
      *    ����
           03  SKT4125-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT4125-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT4125-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT4125-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT4125-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT4125-HOSPCD              PIC  X(14).
      *    �ݸ���̾
           03  SKT4125-HKNJANAME           PIC  X(100).
      *    ���ճ��
           03  SKT4125-KYURATE-TBL  OCCURS  4.
               05  SKT4125-KYURATE         PIC  X(02).
      *    ����ơ��֥�
           03  SKT4125-SEIKYU-TBL  OCCURS  2.
      *    ���
               05  SKT4125-KENSU           PIC  X(05).
      *    ���ż�����
               05  SKT4125-NISSU           PIC  X(05).
      *    ����
               05  SKT4125-TENSU           PIC  X(10).
      *    ������ô��
               05  SKT4125-ITBFTN          PIC  X(09).
      *
      *    ��������ơ��֥�
           03  SKT4125-KOUHI-TBL   OCCURS  9.
      *    ˡ���ֹ�
               05  SKT4125-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT4125-KOHNAME         PIC  X(10).
      *    ���
               05  SKT4125-KKENSU          PIC  X(05).
      *    ���ż�����
               05  SKT4125-KNISSU          PIC  X(05).
      *    ����
               05  SKT4125-KTENSU          PIC  X(10).
      *    ������ô��
               05  SKT4125-KITBFTN         PIC  X(09).
      *    ������ô��
               05  SKT4125-KPTFTN          PIC  X(09).
