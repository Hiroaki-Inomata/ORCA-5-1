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
       01  SC19.
      *��������
           03  SC19-SRYYM             PIC  X(16).
      *��������
           03  SC19-SEIYMD            PIC  X(22).
      *���ŵ��إ�����
           03  SC19-HOSPCDN           PIC  X(20).
      *���ŵ��ؽ����
           03  SC19-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC19-HOSPNAME          PIC  X(100).
      *���ŵ��������ֹ�
           03  SC19-TEL               PIC  X(15).
      *������̾
           03  SC19-KAISETU           PIC  X(30).
      *����ɽ��ʬ
           03  SC19-TENSUHYO          PIC  X(02) OCCURS 4.
      *��Į¼̾
           03  SC19-CITYNAME          PIC  X(26).
      *���ᥨ�ꥢ
           03  SC19-SEIKYU-AREA.
      *���
               05  SC19-KENSU      PIC  X(06).
      *������ô���
               05  SC19-SEIKYUGK   PIC  X(11).
