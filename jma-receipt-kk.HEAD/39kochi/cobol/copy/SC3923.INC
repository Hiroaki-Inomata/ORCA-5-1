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
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SC23.
      *����ǯ��
           03  SC23-SRYYM                 PIC X(16).
      *��ץڡ���
           03  SC23-GPAGE                 PIC X(02).
      *�ڡ���
           03  SC23-PAGE                  PIC X(02).
      *���ŵ��ؽ���
           03  SC23-ADRS                  PIC X(100).
      *���ŵ���̾��
           03  SC23-HOSPNAME              PIC X(100).
      *���߼�̾
           03  SC23-KAISETU               PIC X(40).
      *���ŵ��إ�����
           03  SC23-HOSPCDN               PIC X(20).
      *����ơ��֥�
           03  SC23-SEIKYU-TBL            OCCURS 12 TIMES.
      *ˡ���ֹ�
               05  SC23-HBTNUM            PIC X(03).
      *�����ơ��֥�
               05  SC23-NYUGAI-TBL        OCCURS 2 TIMES.
      *���
                   07  SC23-KENSU         PIC X(05).
      *����
                   07  SC23-TENSU         PIC X(09).
      *���
                   07  SC23-SEIKYUGK      PIC X(09).