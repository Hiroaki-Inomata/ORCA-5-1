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
       01  SKT1040.
      *    ����
           03  SKT1040-ADRS              PIC  X(100).
      *    ���߼�̾
           03  SKT1040-KAISETUNAME       PIC  X(40).
      *    ���ŵ���̾
           03  SKT1040-HOSPNAME          PIC  X(100).
      *    ���ŵ��إ�����
           03  SKT1040-HOSPCD            PIC  X(14).
      *    ����ɽ��ʬ
           03  SKT1040-TENHYOKBN         PIC  X(02)  OCCURS  4.
      *    ����ǯ��
           03  SKT1040-SRYYM             PIC  X(16).
      *    ����ǯ����
           03  SKT1040-SEIYMD            PIC  X(22).
      *    ���μ���
           03  SKT1040-BAITAI            PIC  X(02)  OCCURS  3.