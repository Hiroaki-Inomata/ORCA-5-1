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
      *    ������������               COPY (CPACCESSKEY.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ACCESS-KEY-REC.
      *���ŵ��ؼ����ֹ�
           03  ACCESS-KEY-HOSPNUM      PIC  9(02).
      *������������
           03  ACCESS-KEY-1            PIC  X(128).
      *����ǯ����
           03  ACCESS-KEY-CREYMD       PIC  X(08).
      *��������
           03  ACCESS-KEY-UPHMS        PIC  X(06).
      *��Ưǯ��
           03  ACCESS-KEY-KADO-YM      PIC  X(06).
      *��Ư��٥�
           03  ACCESS-KEY-KADO-LEVEL   PIC  9(01).
      *�������
           03  ACCESS-KEY-CONTRACT-COND
                                       PIC  9(01).
      *������
           03  ACCESS-KEY-CONTRACT-YMD PIC  X(08).
      *�Ȳ���
           03  ACCESS-KEY-INQUIRE-YMD  PIC  X(08).