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
      *    ������������� �ʹ����     COPY (SKT3705.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/15    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT3705.
      *    ����ǯ��
           03  SKT3705-SRYYM               PIC X(16).
      *    ���ŵ��ؽ���
           03  SKT3705-ADRS                PIC X(100).
      *    ���ŵ���̾��
           03  SKT3705-HOSPNAME            PIC X(100).
      *    ���߼�̾
           03  SKT3705-KAISETUNAME         PIC X(40).
      *    ���ŵ��إ�����
           03  SKT3705-HOSPCD              PIC X(14).
      *    �����ǡ��߰���
           03  SKT3705-ZAI1                PIC X(02).
           03  SKT3705-ZAI2                PIC X(02).
      *    �ڡ����ֹ�
           03  SKT3705-NO                  PIC X(04).
      *    ����ơ��֥�
           03  SKT3705-SEIKYU-TBL   OCCURS 2.
      *    �����ơ��֥�
               05  SKT3705-NYUGAI-TBL   OCCURS 2.
      *    ���
                   07  SKT3705-KENSU       PIC X(05).
      *    ����
                   07  SKT3705-TENSU       PIC X(09).
      *    ������ô��
                   07  SKT3705-ITBFTN      PIC X(09).
      *    �������
               05  SKT3705-S-KENSU         PIC X(05).
      *    ���
               05  SKT3705-S-KINGK         PIC X(09).
      *    ɸ����ô��
               05  SKT3705-S-FTNGK         PIC X(09).
      *    ����ʴ����������˥ơ��֥�
           03  SKT3705-KZN-TBL   OCCURS 2.
      *    ���
               05  SKT3705-KZKENSU         PIC X(5).
      *    ����
               05  SKT3705-KZTENSU         PIC X(9).
      *    ������ô��
               05  SKT3705-KZITBFTN        PIC X(9).
      *    �������
               05  SKT3705-S-KZKENSU       PIC X(5).
      *    ���
               05  SKT3705-S-KZKINGK       PIC X(9).
      *    ɸ����ô��
               05  SKT3705-S-KZFTNGK       PIC X(9).
      *    ��������ơ��֥�
           03   SKT3705-KOUHI-TBL  OCCURS 4.
      *    ˡ���ֹ�
               05  SKT3705-HBTNUM          PIC  X(04).
      *    ����̾
               05  SKT3705-KOHNAME         PIC  X(10).
      *    ���������ơ��֥�
               05  SKT3705-KNYUGAI-TBL  OCCURS  2.
      *    ������
                   07  SKT3705-KKENSU      PIC X(05).
      *    ��������
                   07  SKT3705-KTENSU      PIC X(09).
      *    ���������ô��
                   07  SKT3705-KITBFTN     PIC X(09).
      *    ���񿩻����
               05  SKT3705-S-KKENSU        PIC X(05).
      *    ���񿩻����
               05  SKT3705-S-KKINGK        PIC X(09).
      *    ����ɸ����ô��
               05  SKT3705-S-KFTNGK        PIC X(09).
