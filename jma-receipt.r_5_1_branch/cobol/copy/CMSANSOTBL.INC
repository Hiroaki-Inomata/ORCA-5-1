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
      *----------------------------------------------------------------*
      *    ���ǥ����ɥơ��֥�   COPY (CMSANSOTBL.INC)                  *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *    ���ǥ����ɥơ��֥�
       01  TBL-SANSO-AREA.
      *    ���λ��ǡ����ּ�
           03  FILLER              PIC X(09)   VALUE   "739200000".
      *    ���λ��ǡ����̼�
           03  FILLER              PIC X(09)   VALUE   "739210000".
      *    ���ǥܥ���緿
           03  FILLER              PIC X(09)   VALUE   "739220000".
      *    ���ǥܥ�پ���
           03  FILLER              PIC X(09)   VALUE   "739230000".
      *    ���λ��ǡ����ּ���Υ���
           03  FILLER              PIC X(09)   VALUE   "739240000".
      *    ���λ��ǡ����̼���Υ���
           03  FILLER              PIC X(09)   VALUE   "739250000".
      *    ���ǥܥ���緿��Υ���
           03  FILLER              PIC X(09)   VALUE   "739260000".
      *    ���ǥܥ�پ�����Υ���
           03  FILLER              PIC X(09)   VALUE   "739270000".
      *
       01  TBL-SANSO-AREA-R        REDEFINES   TBL-SANSO-AREA.
           03  TBL-SANSO-OCC       OCCURS   8  INDEXED   TBL-SANSO.
               05  TBL-SANSO-SRYCD     PIC X(09).
      *    ���ǥե饰
       01  FLG-SANSO-OK                PIC 9(01).
      *
      *
