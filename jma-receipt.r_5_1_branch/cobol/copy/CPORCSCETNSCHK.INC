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
      *    ���Ź԰������š��Ż�����ɽ�����å��ѥ�᥿
      *                                      COPY (CPORCSCETNSCHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSCETNSCHKAREA.
      *    ������ʬ�ʣ����������å���������ȿ�����å���
           03  ORCSCETNSCHK-SYORI         PIC X(01).
      *    �У�̾
           03  ORCSCETNSCHK-PG            PIC X(06).
      *    �оݿ��ť�����
           03  ORCSCETNSCHK-SRYCD1        PIC X(09).
      *    ���Ͽ��ť�����
           03  ORCSCETNSCHK-SRYCD2        PIC X(09).
      *    ��������
           03  ORCSCETNSCHK-SRYYMD        PIC X(08).
      *    �ֵ��ΰ�
           03  ORCSCETNSCHK-OUTAREA.
      *    �����å����ơʣ����оݤʤ��������оݤ����
               05  ORCSCETNSCHK-CHKKBN        PIC 9(01).
      *        ���Υ�����
               05  ORCSCETNSCHK-KNT-SRYCD-G.
                   07  ORCSCETNSCHK-KNT-SRYCD     PIC X(09)
                                                  OCCURS   10.
      *
