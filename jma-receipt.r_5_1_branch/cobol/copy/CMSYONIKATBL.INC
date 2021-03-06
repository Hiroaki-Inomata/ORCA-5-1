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
      *    �����ʳ���ǻ����ơ��֥� COPY (CMSYONIKATBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       01  TBL-SYONIKA-AREA.
      *
           03  FILLER              PIC X(11)   VALUE   "  111010370".
           03  FILLER              PIC X(11)   VALUE   "3 111010470".
           03  FILLER              PIC X(11)   VALUE   "3 111010570".
           03  FILLER              PIC X(11)   VALUE   "3 111010670".
           03  FILLER              PIC X(11)   VALUE   "3 111010770".
           03  FILLER              PIC X(11)   VALUE   "  111010870".
           03  FILLER              PIC X(11)   VALUE   "  111010970".
           03  FILLER              PIC X(11)   VALUE   "  111011070".
           03  FILLER              PIC X(11)   VALUE   "  111011170".
           03  FILLER              PIC X(11)   VALUE   "4 112005770".
           03  FILLER              PIC X(11)   VALUE   "4 112005870".
           03  FILLER              PIC X(11)   VALUE   "4 112005970".
           03  FILLER              PIC X(11)   VALUE   "4 112006070".
           03  FILLER              PIC X(11)   VALUE   "1 113003510".
           03  FILLER              PIC X(11)   VALUE   "2 113003610".
           03  FILLER              PIC X(11)   VALUE   "1 113003710".
           03  FILLER              PIC X(11)   VALUE   "2 113003810".
           03  FILLER              PIC X(11)   VALUE   "  113005070".
           03  FILLER              PIC X(11)   VALUE   "  113005170".
           03  FILLER              PIC X(11)   VALUE   "4 113005270".
           03  FILLER              PIC X(11)   VALUE   "4 113005370".
           03  FILLER              PIC X(11)   VALUE   "4 113005470".
           03  FILLER              PIC X(11)   VALUE   "4 113005570".
      *    �������±���Ʊ��Ƴ���ʣ���
           03  FILLER              PIC X(11)   VALUE   "9 180010510".
      *    �ౡ����Ʊ��Ƴ�û�
           03  FILLER              PIC X(11)   VALUE   "9 180015870".
      *    �ϰ�Ϣ�Ⱦ�����֡�����������
           03  FILLER              PIC X(11)   VALUE   "9 113006710".
      *    �ȣ������������ɲ�
           03  FILLER              PIC X(11)   VALUE   "3 113007070".
           03  FILLER              PIC X(11)   VALUE   "3 113007170".
           03  FILLER              PIC X(11)   VALUE   "3 113007270".
           03  FILLER              PIC X(11)   VALUE   "4 113007370".
           03  FILLER              PIC X(11)   VALUE   "4 113007470".
           03  FILLER              PIC X(11)   VALUE   "4 113007570".
           03  FILLER              PIC X(11)   VALUE   "4 113007670".
           03  FILLER              PIC X(11)   VALUE   "4 113007770".
           03  FILLER              PIC X(11)   VALUE   "4 113007870".
      *    ���ֳ��û�
           03  FILLER              PIC X(11)   VALUE   "3 113009670".
           03  FILLER              PIC X(11)   VALUE   "4 113009770".
           03  FILLER              PIC X(11)   VALUE   "4 113009870".
      *    ����ô����������������
           03  FILLER              PIC X(11)   VALUE   "9 199000610".
      *    ���ž����������ʣȣ������������
           03  FILLER              PIC X(11)   VALUE   "9 113009510".
      *    �ϰ�Ϣ�Ⱦ�����֡�������������
           03  FILLER              PIC X(11)   VALUE   "9 113008210".
      *    ����ȥꥢ�����û�
           03  FILLER              PIC X(11)   VALUE   "9 113011570".
      *
       01  TBL-SYONIKA-AREA-R      REDEFINES   TBL-SYONIKA-AREA.
           03  TBL-SYONIKA-OCC     OCCURS  42  INDEXED   TBL-SYO.
      *        1:��ǡ�2:�ƿǡ�3:��ǻ��ֳ���4:�ƿǻ��ֳ�
      *        9:�����ʳ���ǻ����ʳ��Ǥ⻻���ǽ
               05  TBL-SYONIKA-KBN         PIC X(01).
               05  TBL-YOBI1               PIC X(01).
               05  TBL-SYONIKA-SRYCD       PIC X(09).
      *
