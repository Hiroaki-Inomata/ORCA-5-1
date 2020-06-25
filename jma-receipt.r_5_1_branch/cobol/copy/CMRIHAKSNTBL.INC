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
      *    ��ϥӥ�������ϲû��ơ��֥� COPY (CMRIHAKSNTBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  04/03/17  �ȣ������������б�
      *****************************************************************
      *
       01  TBL-RIHABIRI-AREA.
      *    ���������ϲû�
           03  FILLER              PIC X(11)   VALUE   "180022970 1".
           03  FILLER              PIC X(11)   VALUE   "180023070 2".
           03  FILLER              PIC X(11)   VALUE   "180023170 3".
           03  FILLER              PIC X(11)   VALUE   "180023270 1".
           03  FILLER              PIC X(11)   VALUE   "180023370 2".
           03  FILLER              PIC X(11)   VALUE   "180023470 3".
           03  FILLER              PIC X(11)   VALUE   "180023570  ".
      *    ��������ϲû�
           03  FILLER              PIC X(11)   VALUE   "180024070 1".
           03  FILLER              PIC X(11)   VALUE   "180024170 2".
           03  FILLER              PIC X(11)   VALUE   "180024270 3".
           03  FILLER              PIC X(11)   VALUE   "180024370 1".
           03  FILLER              PIC X(11)   VALUE   "180024470 2".
           03  FILLER              PIC X(11)   VALUE   "180024570 3".
           03  FILLER              PIC X(11)   VALUE   "180024670  ".
      *    ���������ϲû�(Ϸ�͡�
           03  FILLER              PIC X(11)   VALUE   "180707670 1".
           03  FILLER              PIC X(11)   VALUE   "180707770 2".
           03  FILLER              PIC X(11)   VALUE   "180707870 3".
           03  FILLER              PIC X(11)   VALUE   "180707970  ".
           03  FILLER              PIC X(11)   VALUE   "180708570 1".
           03  FILLER              PIC X(11)   VALUE   "180708670 2".
           03  FILLER              PIC X(11)   VALUE   "180708770 3".
           03  FILLER              PIC X(11)   VALUE   "180708870  ".
      *    �����ϥӥ�û���ϫ�ҡ�
           03  FILLER              PIC X(11)   VALUE   "101800040 1".
           03  FILLER              PIC X(11)   VALUE   "101800050 2".
           03  FILLER              PIC X(11)   VALUE   "101800060 3".
           03  FILLER              PIC X(11)   VALUE   "101800070  ".
      *    ������ˡ����ܹԲû�(Ϸ�͡�
           03  FILLER              PIC X(11)   VALUE   "180708070".
      *    �����ˡ����ܹԲû�(Ϸ�͡�
           03  FILLER              PIC X(11)   VALUE   "180708970".
      *
      *    ���������ϲû�
           03  FILLER              PIC X(11)   VALUE   "180025670 1".
           03  FILLER              PIC X(11)   VALUE   "180025770 2".
           03  FILLER              PIC X(11)   VALUE   "180025870 3".
      *    ���������ϲû��ʣ�����̤����
           03  FILLER              PIC X(11)   VALUE   "180025970 1".
           03  FILLER              PIC X(11)   VALUE   "180026070 2".
           03  FILLER              PIC X(11)   VALUE   "180026170 3".
      *
       01  TBL-RIHABIRI-AREA-R       REDEFINES   TBL-RIHABIRI-AREA.
      *****03  TBL-RIHABIRI-OCC      OCCURS   28  INDEXED   TBL-RIDX.
           03  TBL-RIHABIRI-OCC      OCCURS   34  INDEXED   TBL-RIDX.
               05  TBL-RIHA-SRYCD      PIC X(09).
               05  TBL-RIHA-F1         PIC X(01).
      *        1:�������ޤǡʥ��ˡ�2:���������飳�����ʥ���
      *        3:���������飹�����ʥϡ�
               05  TBL-RIHA-KBN        PIC X(01).
      *
      *ʿ������ǯ�����
       01  TBL-RIHABIRI-AREA02.
      *    ������ˡ����ܹԲû�(Ϸ�͡�
           03  FILLER              PIC X(09)   VALUE   "180708070".
      *    �����ˡ����ܹԲû�(Ϸ�͡�
           03  FILLER              PIC X(09)   VALUE   "180708970".
      *
       01  TBL-RIHABIRI-AREA02-R     REDEFINES   TBL-RIHABIRI-AREA02.
           03  TBL-RIHABIRI2-OCC     OCCURS   2  INDEXED   TBL-RIDX2.
               05  TBL-RIHA2-SRYCD     PIC X(09).
      *
      *
      *    �����ϥӥꤢ��
       01  FLG-SOUKI                   PIC 9(01).
      *