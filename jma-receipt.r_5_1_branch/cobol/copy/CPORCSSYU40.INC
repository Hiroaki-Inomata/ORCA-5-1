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
      *    ʡ�������������б������оݳ��Խ����֥ѥ�᥿
      *                                              (CPORCSSYU40.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSSYU40AREA.
      *
      *    ������ʬ�ʣ������Ź԰١�������Ǽ��
           03  ORCS40-KBN                  PIC X(01).
      *
      *    ���Ź԰٤ξ��
           03  ORCS40-KBN1-AREA.
      *        �оݿ��ť�����
               05  ORCS40-ZAI-TBL          OCCURS  5.
                   07  ORCS40-SRYKBN           PIC X(02).
                   07  ORCS40-ZAITEN           PIC 9(08).
                   07  ORCS40-SRYCD            PIC X(09)
                                               OCCURS  5.
      *
      *    ��Ǽ�ξ��
           03  ORCS40-KBN2-AREA.
      *        ��ɼ�ֹ�
               05  ORCS40-DENPNUM              PIC 9(08).
      *
