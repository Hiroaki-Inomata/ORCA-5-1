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
      *    ��ô��ʬ�����ɼ�������������COPY (CPORCSGETFTNKBN.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSGETFTNKBNAREA.
      *
      *���ŵ��أɣ�
           03  ORCSGETFK-HOSPNUM       PIC 9(02).
      *���ԣɣ�
           03  ORCSGETFK-PTID          PIC 9(10).
      *����ǯ��
           03  ORCSGETFK-SRYYM         PIC X(06).
      *�ݸ��ȹ礻�ֹ�
           03  ORCSGETFK-HKNCOMBINUM   PIC 9(04).
      *��ô��ʬ������
           03  ORCSGETFK-FTNKBN        PIC X(01).
      *�ݸ����ֹ�
           03  ORCSGETFK-HKNJANUM      PIC X(08).
      *��ô���ֹ�
           03  ORCSGETFK-FTNJANUM-G.
               05  ORCSGETFK-FTNJANUM  PIC X(08)
                                       OCCURS  4.
      *
