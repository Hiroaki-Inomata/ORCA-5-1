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
      *    ��л���Ķ�Ḻ�������������ɼ������ּ������֥ѥ�᥿����COPY (CPORCSYAKINGEN.INC)
      *                                ���ѥץ������ (ORCSYAKINGEN)
      *    �������
      *      ������������
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSYAKINGEN-AREA.
      *
      *����ǯ��
           03  LNK-YAKINGEN-SANTEIYM               PIC  X(06).
      *�����ֹ�
           03  LNK-YAKINGEN-BTU-NUM                PIC  X(02).
      *������������
           03  LNK-YAKINGEN-SRYCD                  PIC  X(09).
      *�ֵѹ���
           03  LNK-YAKINGEN-HENKYAKU-AREA.
      *�в�����������������
              05  LNK-YAKINGEN-CHGSRYCD            PIC  X(09).
      *���������ɡʣ�����и����������ֵѤ��ꡡ������и����������ֵѤʤ���
           03  LNK-YAKINGEN-RC                     PIC S9(9) BINARY.
      *
