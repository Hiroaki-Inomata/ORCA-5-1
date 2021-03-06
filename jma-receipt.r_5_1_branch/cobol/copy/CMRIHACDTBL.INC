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
      *   ��ϥӥ�Ʊ�컻�ꥳ���ɥơ��֥� COPY (CMRIHACDTBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *    ��ϥӥ�Υ�����
       01  TBL-RIHABIRI-AREA.
      *    ����ɼ��¥�ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "1 1 180027410".
           03  FILLER              PIC X(13)   VALUE   "1 2 180027510".
      *    ����ɼ��¥�ϥӥ�ʣ�����Ķ��
           03  FILLER              PIC X(13)   VALUE   "121 180029110".
           03  FILLER              PIC X(13)   VALUE   "122 180029210".
      *    Ǿ��ɼ��¥�ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "2 1 180027610".
           03  FILLER              PIC X(13)   VALUE   "2 2 180027710".
      *    Ǿ��ɼ��¥�ϥӥ�ʣ�����Ķ��
           03  FILLER              PIC X(13)   VALUE   "221 180029310".
           03  FILLER              PIC X(13)   VALUE   "222 180029410".
      *    ��ư���ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "3 1 180027810".
           03  FILLER              PIC X(13)   VALUE   "3 2 180027910".
      *    ��ư���ϥӥ�ʣ�����Ķ��
           03  FILLER              PIC X(13)   VALUE   "321 180029510".
           03  FILLER              PIC X(13)   VALUE   "322 180029610".
      *    �Ƶ۴��ϥӥ�
           03  FILLER              PIC X(13)   VALUE   "4 1 180028010".
           03  FILLER              PIC X(13)   VALUE   "4 2 180028110".
      *    �Ƶ۴��ϥӥ�ʣ���Ķ��
           03  FILLER              PIC X(13)   VALUE   "421 180029710".
           03  FILLER              PIC X(13)   VALUE   "422 180029810".
      *�ȣ�����������(������������ѡ�
      *    Ǿ��ɼ��¥�ϥӥꣳ
           03  FILLER              PIC X(13)   VALUE   "2 3 180030810".
      *
       01  TBL-RIHABIRI-AREA-R     REDEFINES   TBL-RIHABIRI-AREA.
           03  TBL-RIHABIRI-OCC    OCCURS   17   INDEXED   TBL-RIH.
      *        ��ϥӥ����
               05  TBL-RIHA-KBN        PIC X(01).
               05  TBL-RIHA-GEN        PIC X(01).
               05  TBL-RIHA-KBN2       PIC X(01).
               05  TBL-RIHA-YOBO       PIC X(01).
               05  TBL-RIHA-SRYCD      PIC X(09).
      *
       01  FLG-RIHA-OK                 PIC 9(01).
      *
