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
      ************************************************************
      *    �����������     COPY (CPSKT0700TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         11/04/04  ����������ʿ�β��Ͽ��б�
      *  01.00.02    ����         18/10/10  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0700TMP-REC.
           03  SKT0700TMP-KEY.
      *���ŵ��إ�����
               05  SKT0700TMP-HOSPCD               PIC  X(20).
      *���ȶ�ʬ
               05  SKT0700TMP-GENMEN               PIC  X(01).
      *�ݸ��Կ�������
           03  SKT0700TMP-KENNAI-HKNJSU            PIC  9(06).
      *�ݸ��Կ�������
           03  SKT0700TMP-KENGAI-HKNJSU            PIC  9(06).
      *����ơ��֥�
           03  SKT0700TMP-SEIKYU-TBL               OCCURS  4.
      *�����ơ��֥�
               05  SKT0700TMP-NYUGAI-TBL           OCCURS  2.
      *���
                   07  SKT0700TMP-KENSU            PIC  9(11).
      *����
                   07  SKT0700TMP-NISSU            PIC  9(11).
      *����
                   07  SKT0700TMP-TENSU            PIC  9(14).
      *���������
               05  SKT0700TMP-SKENSU               PIC  9(06).
      *����������
               05  SKT0700TMP-SNISSU               PIC  9(06).
      *���������
               05  SKT0700TMP-SKINGK               PIC  9(11).
      *��ץơ��֥�
               05  SKT0700TMP-GOKEI-TBL.
      *��ס����
                   07  SKT0700TMP-G-KENSU          PIC  9(11).
      *��ס�����
                   07  SKT0700TMP-G-NISSU          PIC  9(11).
      *��ס�����
                   07  SKT0700TMP-G-TENSU          PIC  9(14).
      *��ס��������
                   07  SKT0700TMP-G-SKENSU         PIC  9(06).
      *��ס���������
                   07  SKT0700TMP-G-SNISSU         PIC  9(06).
      *��ס��������
                   07  SKT0700TMP-G-SKINGK         PIC  9(11).
      *�����ݸ��Կ�
           03  SKT0700TMP-KENNAI.
               05  SKT0700TMP-KENNAI-MAX    PIC 9(04).
               05  SKT0700TMP-KENNAI-HKNJA  PIC X(06) OCCURS 100.
      *�����ݸ��Կ�
           03  SKT0700TMP-KENGAI.
               05  SKT0700TMP-KENGAI-MAX    PIC 9(04).
               05  SKT0700TMP-KENGAI-HKNJA  PIC X(06) OCCURS 100.