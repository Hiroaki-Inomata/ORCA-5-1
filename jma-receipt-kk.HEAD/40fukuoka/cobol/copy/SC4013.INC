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
       01  SC13.
      *��������
           03  SC13-SRYYM             PIC  X(16).
      *��������
           03  SC13-SEIYMD            PIC  X(22).
      *���ŵ��إ�����
           03  SC13-HOSPCDN           PIC  X(20).
      *���ŵ��ؽ����
           03  SC13-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC13-HOSPNAME          PIC  X(100).
      *���ŵ��������ֹ�
           03  SC13-TEL               PIC  X(15).
      *������̾
           03  SC13-KAISETU           PIC  X(40).
      *����ɽ��ʬ
           03  SC13-TENSUHYO          PIC  X(02) OCCURS 4.
      *�����㳲�ԡܣ���Ϸ�����ᥨ�ꥢ
           03  SC13-80-AREA.
      *���
               05  SC13-80-KENSU      PIC  X(05).
      *������ô���
               05  SC13-80-ITBFTN     PIC  X(10).
      *�����
               05  SC13-80-SEIKYUGK   PIC  X(10).
      *����Ϸ�����ᥨ�ꥢ
           03  SC13-41-AREA OCCURS 5.
      *���
               05  SC13-41-KENSU      PIC  X(05).
      *���ް�����ô��
               05  SC13-41-YKZFTN     PIC  X(09).
      *������ô��
               05  SC13-41-ITBFTN     PIC  X(09).
      *������
               05  SC13-41-SEIKYUGK   PIC  X(09).
