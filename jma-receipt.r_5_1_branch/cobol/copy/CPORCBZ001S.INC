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
      *    ���������ֹ�����ѥ�᥿��   COPY (CPORCBZ001S.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCBZ001SAREA.
      *
      *    ��λ�ե饰 �ʣ�����λ��
           03  ORCBZ001S-ENDFLG        PIC 9(01).
      *    ������
           03  ORCBZ001S-MAX-CNT       PIC 9(04).
      *    �����ֹ�
           03  ORCBZ001S-PTNUM-AREA.
               05  ORCBZ001S-PTNUM-G       OCCURS   120.
                   07  ORCBZ001S-PTNUM     PIC X(20).
      *    �ǽ������ֹ�
           03  ORCBZ001S-LST-PTNUM     PIC X(20).
      *
      *    ���顼������ ��1:�оݳ�,9:���顼��
           03  ORCBZ001S-RC            PIC 9(04).
      *