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
       01  SC03.
      *����ǯ��
           03  SC03-SRYYM              PIC  X(16).
      *����ǯ����
           03  SC03-SEIYMD             PIC  X(22).
      *���ŵ��إ�����
           03  SC03-HOSPCDN            PIC  X(20).
      *ñ����ʬ
           03  SC03-TANDOKUKBN         PIC  X(02) OCCURS 2.
      *���ŵ��ؽ����
           03  SC03-ADRS               PIC  X(100).
      *���ŵ���̾��
           03  SC03-HOSPNAME           PIC  X(100).
      *���߼�̾
           03  SC03-KAISETU            PIC  X(40).
      *�����ֹ�
           03  SC03-TEL                PIC  X(15).
      *����ɽ��ʬ
           03  SC03-TENSUHYO           PIC  X(02) OCCURS 4.
      *��Į¼̾��
           03  SC03-CITYNAME           PIC  X(20).
      *��Į¼�ֹ�
           03  SC03-CITYNUM            PIC  X(08).
      *����ơ��֥�
           03  SC03-NYUGAI-TBL  OCCURS 2.
               05  SC03-SEIKYU-TBL     OCCURS 4.
      *���
                   07  SC03-KENSU      PIC X(06).
      *����
                   07  SC03-TENSU      PIC X(10).
      *������ô��
                   07  SC03-ITBFTN     PIC X(09).
      *Ĺ�����
               05  SC03-TYOKI1         PIC X(06).
      *Ĺ����̷��
               05  SC03-TYOKI2         PIC X(06).
