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
      *----------------------------------------------------------*
      *    ���Ź԰ٻ����Խ��ѥѥ�᥿��   COPY (CPORCSC90.INC)
      *                              (ORCSC90 �ǻ��ѡ�
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   01.06.11  ���ͱդ��Ѵ��ɲ�
      * 01.00.03     MCC-¿��Ǽ   01.08.09  �쥤�������ѹ���SPA���̡�
      ************************************************************
      *
       01  ORCSC90AREA.
      *    ������ʬ�ʣ������١�������ס�
           03  LNK-SC90-KBN            PIC X(01).
      *    �оݹԡ�ɬ�ܡ�
           03  LNK-SC90-IDX-STR        PIC 9(04).
      *    ��λ��
           03  LNK-SC90-IDX-END        PIC 9(04).
      *    �굻��
           03  LNK-SC90-IDX            PIC 9(04).
      *
      *****03  LNK-SC90-KAISU          PIC 9(03).
      *
      *    �����ʼ굻��
           03  LNK-SC90-TENSU          PIC 9(08).
      *    �������
           03  LNK-SC90-SUM-AREA.
      *        ������
               05  LNK-SC90-TENSUKEI       PIC 9(08).
      *        ��������
               05  LNK-SC90-YKZTEN         PIC 9(08).
      *        �������
               05  LNK-SC90-KIZTEN         PIC 9(08).
      *        ��������ʻ���������
               05  LNK-SC90-SANTEN         PIC 9(08).
      *        �������������������
               05  LNK-SC90-CHITEN         PIC 9(08).
      *        �ե��������
               05  LNK-SC90-FIRTEN         PIC 9(08).
      *