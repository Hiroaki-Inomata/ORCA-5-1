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
       01  SC21.
      *��������
           03  SC21-SEIYMD            PIC  X(22).
      *���ŵ��إ�����
           03  SC21-HOSPCDN           PIC  X(20).
      *���ŵ��ؽ����
           03  SC21-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC21-HOSPNAME          PIC  X(100).
      *������̾
           03  SC21-KAISETU           PIC  X(40).
      *��Į¼̾
           03  SC21-CITYNAME          PIC  X(26).
      *��Į¼�ֹ�
           03  SC21-CITYNUM           PIC  X(16).
      *��������
           03  SC21-GSEIKYUGK         PIC  X(09).
      *���ᥨ�ꥢ
           03  SC21-SEIKYU-AREA OCCURS 5.
      *������ֹ�
               05  SC21-JKYSNUM       PIC  X(14).
      *��̾
               05  SC21-NAME          PIC  X(20).
      *������ǯ
               05  SC21-GENGO         PIC  X(02) OCCURS 3.
               05  SC21-TANJOYY       PIC  X(04).
      *����
               05  SC21-SEX           PIC  X(02) OCCURS 2.
      *����ǯ��
               05  SC21-SRYYY         PIC  X(04).
               05  SC21-SRYMM         PIC  X(04).
      *������ʬ
               05  SC21-NYUGAIKBN     PIC  X(02) OCCURS 4.
      *����
               05  SC21-NISSU         PIC  X(02).
      *�����
               05  SC21-SEIKYUGK      PIC  X(09).
