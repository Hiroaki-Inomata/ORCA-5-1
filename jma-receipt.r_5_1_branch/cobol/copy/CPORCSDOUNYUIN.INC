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
      *    Ʊ��������Ƚ��ѥ�᥿������COPY (CPORCSDOUNYUIN.INC)
      *                                ���ѥץ������ (ORCSDOUNYUIN)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ���������ֹ�
      *      ������������ž������
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSDOUNYUIN-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-DOUNYUIN-HOSPNUM               PIC  9(02).
      *���ԣɣ�
           03  LNK-DOUNYUIN-PTID                  PIC  9(10).
      *���������ֹ�
           03  LNK-DOUNYUIN-RRKNUM                PIC  9(03).
      *��������
           03  LNK-DOUNYUIN-SAINYUINYMD           PIC  X(08).
      *�ֵѥ��ꥢ
           03  LNK-DOUNYUIN-HENKYAKU-AREA.
      *���������곫���������������ɤ����λ��Τ��ֵѡ�
             05  LNK-DOUNYUIN-SANTEIYMD           PIC  X(08).
      *
      *����������
      *     �������򤫤��Ʊ���������ʳ�
      *     �������򤫤��Ʊ��������
      *     ������������̵��
      *     �������ԥޥ������������顼
           03  LNK-DOUNYUIN-RC                    PIC S9(9) BINARY.
      *