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
      *    ��л����������������������ɼ�������
      *                                COPY (CPORCSYAKINTOKU.INC)
      *                                ���ѥץ������ (ORCSYAKINTOKU)
      *    �������
      *      ������������
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSYAKINTOKU-AREA.
      *
      *����ǯ��
           03  LNK-YAKINTOKU-SANTEIYM              PIC  X(06).
      *�����ֹ�
           03  LNK-YAKINTOKU-BTU-NUM               PIC  X(02).
      *������������
           03  LNK-YAKINTOKU-SRYCD                 PIC  X(09).
      *�ֵѹ���
           03  LNK-YAKINTOKU-HENKYAKU-AREA.
      *�в�����������������
              05  LNK-YAKINTOKU-CHGSRYCD           PIC  X(09).
      *���������ɡʣ�����������������ֵѤ��ꡡ������������������ֵѤʤ���
           03  LNK-YAKINTOKU-RC                    PIC S9(9) BINARY.
      *