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
      *    ���Ķ���������ѥ�᥿��COPY (CPORCSTEISU.INC)
      *                                ���ѥץ������ (ORCSTEISU.CBL)
      *    �������
      *      ����ǯ��
      *      ����������
      *      ������������ʬ
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTEISU-AREA.
      *
      *����ǯ��
           03  LNK-TEISU-SANTEIYM                 PIC  X(06).
      *����������
           03  LNK-TEISU-SANTEISRYCD              PIC  X(09).
      *������������ʬ�ʣ�������������������������������
           03  LNK-TEISU-TOKUTEINYUIN-KBN         PIC  X(01).
      *�ֵѹ���
           03  LNK-TEISU-HENKYAKU-AREA.
      *���Ķ���ֵѥơ��֥�
             04  LNK-TEISU-HENKYAKU-TBL           OCCURS 2.
      *���Ķ�ᥳ����
               05  LNK-TEISU-SRYCD                PIC  X(09).
      *���Ķ�ᳫ��ǯ��
               05  LNK-TEISU-STARTYM              PIC  X(06).
      *���Ķ�Ὢλǯ��
               05  LNK-TEISU-ENDYM                PIC  X(06).
      *���������ɡʣ�������ǯ��顼�����������ƥ��������̵��
      *            �������Ķ���оݳ���
           03  LNK-TEISU-RC                       PIC S9(9) BINARY.
      *