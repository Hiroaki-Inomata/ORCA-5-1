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
      *         �����ޥ����������������    COPY (CPTENSU-PRICE.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TENSU-PRICE-REC.
           03  TNSPR-KEY.
               05  TNSPR-HOSPNUM         PIC 9(2).
               05  TNSPR-SRYCD           PIC  X(09).
               05  TNSPR-YUKOSTYMD       PIC  X(08).
               05  TNSPR-YUKOEDYMD       PIC  X(08).
      *    ���
           03  TNSPR-PRICE               PIC  9(09)V9(02).
      *
           03  TNSPR-TERMID              PIC  X(16).
           03  TNSPR-OPID                PIC  X(16).
           03  TNSPR-CREYMD              PIC  X(08).
           03  TNSPR-UPYMD               PIC  X(08).
           03  TNSPR-UPHMS               PIC  X(06).
      *�����祭�������ѹ���
           03  TNSPR-UP-KEY.
               05  TNSPR-UP-HOSPNUM       PIC 9(2).
               05  TNSPR-UP-SRYCD         PIC  X(09).
               05  TNSPR-UP-YUKOSTYMD     PIC  X(08).
               05  TNSPR-UP-YUKOEDYMD     PIC  X(08).