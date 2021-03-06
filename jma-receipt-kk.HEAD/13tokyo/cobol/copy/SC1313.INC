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
       01  SC13.
      *���ŵ��إ�����
           03  SC13-HOSPCD             PIC X(14).
      *����ǯ��
           03  SC13-PRTYM              PIC X(16).
      *����ǯ����
           03  SC13-SEIYMD             PIC X(22).
      *������ʬɽ������� ������ ��Ĵ�� ���Ǹ�
           03  SC13-TENHYOKBN          PIC X(02) OCCURS 4.
      *����
           03  SC13-ADRS               PIC X(100).
      *���ŵ���̾
           03  SC13-HOSPNAME           PIC X(100).
      *�����ֹ�
           03  SC13-TEL                PIC X(15).
      *���߼�̾
           03  SC13-KAISETUNAME        PIC X(40).
      *NO
           03  SC13-NO                 PIC X(03).
      *��ô���ֹ�
           03  SC13-FTNJA-TBL.
               05  SC13-FTNJANUM       PIC X(08) OCCURS 10.
      *������ֹ�
           03  SC13-JKYSNUM-TBL.
               05  SC13-JKYSNUM        PIC X(14) OCCURS 10.
      *�ݸ����ֹ�
           03  SC13-HKNJANUM-TBL.
               05  SC13-HKNJANUM       PIC X(16) OCCURS 10.
      *���ԥ��ʻ�̾
           03  SC13-KANANAME-TBL.
               05  SC13-KANANAME       PIC X(20) OCCURS 10.
      *���Ի�̾
           03  SC13-NAME-TBL.
               05  SC13-NAME           PIC X(20) OCCURS 10.
      *��ô���
           03  SC13-FTNRATE-TBL.
               05  SC13-FTNRATE-G OCCURS 10.
                   07  SC13-FTNRATE    PIC X(02) OCCURS 3.
      *����������
           03  SC13-NYUGAI-TBL.
               05  SC13-NYUGAI-G OCCURS 10.
                   07  SC13-NYUGAIKBN  PIC X(02) OCCURS 2.
      *��������
           03  SC13-NISSU-TBL.
               05  SC13-NISSU          PIC X(02) OCCURS 10.
      *�����
           03  SC13-SEIKYUGK-TBL.
               05  SC13-SEIKYUGK       PIC X(07) OCCURS 10.
      *��������������ɸ����ô��
           03  SC13-SFTNGK-TBL.
               05  SC13-SFTNGK         PIC X(07) OCCURS 10.
      *�����������ʬ
           03  SC13-SKJKBN-TBL OCCURS 10.
               05  SC13-SKJKBN         PIC X(02) OCCURS 2.
      *����ʬ����
           03  SC13-TENSU-TBL.
               05  SC13-TENSU          PIC X(07) OCCURS 10.
      *������ô��������
           03  SC13-ITBFTN-TBL.
               05  SC13-ITBFTN         PIC X(07) OCCURS 10.
      *������
           03  SC13-BIKO-TBL OCCURS 10.
               05  SC13-BIKO1          PIC X(02) OCCURS 2.
               05  SC13-BIKO2          PIC X(02) OCCURS 2.
      *���
           03  SC13-GOKEI-TBL.
      *��׷��
               05  SC13-GKENSU         PIC X(02).
      *��������
               05  SC13-GSEIKYUGK      PIC X(09).
