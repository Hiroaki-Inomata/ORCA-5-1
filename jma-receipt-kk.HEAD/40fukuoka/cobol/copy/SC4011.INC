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
       01  SC11.
      *��������
           03  SC11-SRYYM             PIC  X(16).
      *��������
           03  SC11-SEIYMD            PIC  X(22).
      *���ŵ��إ�����
           03  SC11-HOSPCDN           PIC  X(20).
      *���ŵ��ؽ����
           03  SC11-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC11-HOSPNAME          PIC  X(100).
      *������̾
           03  SC11-KAISETU           PIC  X(80).
      *�ݸ����ֹ�
           03  SC11-HKNJANUM          PIC  X(12).
           03  SC11-HKNJANUM2         PIC  X(12).
      *�ݸ���ʬ
           03  SC11-HKNKBN            PIC  X(02) OCCURS 4.
      *���٥ơ��֥�
           03  SC11-MEISAI-TBL OCCURS 3.
      *���ճ��
               05  SC11-KYURATE       PIC  X(06).
      *����ơ��֥�
               05  SC11-SEIKYU-TBL OCCURS 3.
      *���
                   07  SC11-KENSU     PIC  X(05).
      *���ż�����
                   07  SC11-NISSU     PIC  X(05).
      *������
                   07  SC11-TENSU     PIC  X(09).
      *�����㡦����ŵ��ճ�����
                   07  SC11-KYUFUGAI  PIC  X(07).
      *����ʬ
                   07  SC11-YKZFTN    PIC  X(07).
      *������ôʬ
                   07  SC11-ITBFTN    PIC  X(07).
      *���
                   07  SC11-GOKEI     PIC  X(09).
