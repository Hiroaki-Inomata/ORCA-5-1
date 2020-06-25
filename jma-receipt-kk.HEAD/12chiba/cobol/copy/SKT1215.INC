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
      *  �������������(����)�����ո���     COPY (SKT1215.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/16    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT1215.
      *    ����ǯ��
           03  SKT1215-SRYYM                PIC  X(16).
      *    ����ǯ����
           03  SKT1215-SEIYMD               PIC  X(22).
      *    ���ŵ��ؽ���
           03  SKT1215-ADRS                 PIC  X(100).
      *    ���ŵ���̾
           03  SKT1215-HOSPNAME             PIC  X(100).
      *    �����ֹ�
           03  SKT1215-TEL                  PIC  X(15).
      *    ���߼�̾
           03  SKT1215-KAISETUNAME          PIC  X(40).
      *    �ݸ����ֹ�
           03  SKT1215-HKNJANUM             PIC  X(4).
      *    ���ŵ��إ�����
           03  SKT1215-HOSPCD               PIC  X(14).
      *    �ڡ����ֹ�
           03  SKT1215-NO                   PIC  X(04).
      *    ����ơ��֥�
           03  SKT1215-SEIKYU-TBL     OCCURS  2.
      *    ���
               05  SKT1215-KENSU            PIC  X(05).
      *    ������
               05  SKT1215-NISSU            PIC  X(05).
      *    ����
               05  SKT1215-TENSU            PIC  X(09).
      *    ������ô��
               05  SKT1215-ITBFTN           PIC  X(09).
      *    �������
               05  SKT1215-S-KENSU          PIC  X(05).
      *    ����������
               05  SKT1215-S-NISSU          PIC  X(05).
      *    �������
               05  SKT1215-S-KINGK          PIC  X(09).
      *    ����ɸ����ô��
               05  SKT1215-S-FTNGK          PIC  X(09).
      *    ����ơ��֥�
           03  SKT1215-KOH-TBL       OCCURS 6.
      *    ˡ���ֹ�
               05  SKT1215-HBTNUM           PIC  X(02).
      *    ���
               05  SKT1215-KKENSU           PIC  X(05).
      *    ������
               05  SKT1215-KNISSU           PIC  X(05).
      *    ����
               05  SKT1215-KTENSU           PIC  X(09).
      *    ������ô��
               05  SKT1215-KITBFTN          PIC  X(09).
      *    �������
               05  SKT1215-S-KKENSU         PIC  X(05).
      *    ����������
               05  SKT1215-S-KNISSU         PIC  X(05).
      *    �������
               05  SKT1215-S-KKINGK         PIC  X(09).
      *    ����ɸ����ô��
               05  SKT1215-S-KFTNGK         PIC  X(09).
