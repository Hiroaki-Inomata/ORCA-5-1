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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ��ǼAPI���̥��ԡ���          COPY (CPAPI023COMMON.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *    ��������̾��
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(03)   VALUE   "A00".
           03  FILLER              PIC X(30)   VALUE   "�顦�ƿ���".
           03  FILLER              PIC X(03)   VALUE   "B00".
           03  FILLER              PIC X(30)   VALUE   "��ش�����".
           03  FILLER              PIC X(03)   VALUE   "C00".
           03  FILLER              PIC X(30)   VALUE   "��������".
           03  FILLER              PIC X(03)   VALUE   "F00".
           03  FILLER              PIC X(30)   VALUE   "����".
           03  FILLER              PIC X(03)   VALUE   "G00".
           03  FILLER              PIC X(30)   VALUE   "����".
           03  FILLER              PIC X(03)   VALUE   "J00".
           03  FILLER              PIC X(30)   VALUE   "����".
           03  FILLER              PIC X(03)   VALUE   "K00".
           03  FILLER              PIC X(30)   VALUE   "���".
           03  FILLER              PIC X(03)   VALUE   "L00".
           03  FILLER              PIC X(30)   VALUE   "���".
           03  FILLER              PIC X(03)   VALUE   "D00".
           03  FILLER              PIC X(30)   VALUE   "����".
           03  FILLER              PIC X(03)   VALUE   "E00".
           03  FILLER              PIC X(30)   VALUE   "��������".
           03  FILLER              PIC X(03)   VALUE   "H00".
           03  FILLER              PIC X(30)   VALUE   "��ϥӥ�".
           03  FILLER              PIC X(03)   VALUE   "I00".
           03  FILLER              PIC X(30)   VALUE   "����������".
           03  FILLER              PIC X(03)   VALUE   "M00".
           03  FILLER              PIC X(30)   VALUE   "����������".
           03  FILLER              PIC X(03)   VALUE   "N00".
           03  FILLER              PIC X(30)   VALUE   "��������".
           03  FILLER              PIC X(03)   VALUE   "A10".
           03  FILLER              PIC X(30)   VALUE   "������".
           03  FILLER              PIC X(03)   VALUE   "001".
           03  FILLER              PIC X(30)   VALUE   "����ô������".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI-OCC  OCCURS   16.
               05  TBL-HKNTEN-KBN  PIC X(03).
               05  TBL-HKNTEN-MEI  PIC X(30).
