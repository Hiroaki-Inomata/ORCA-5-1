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
      *    ��������ƽ��ϥѥ�᥿��     COPY (ORCHCN01 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCHCN01AREA.
      *    ������ʬ (1:Ƭ�񤭤Τ߰���/ 2:���٤ư����� ... ɬ��
           03  ORCHCN01-SYORIKBN           PIC X(01).
      *    Ģɼ��ʬ (1:����� 2:³�� 3:ξ��)<- ������ʬ=���ΤȤ��Τ�����
           03  ORCHCN01-PRTKBN             PIC X(01).
      *    �缣�� ... Ǥ��
           03  ORCHCN01-DR-TBL.
               05  ORCHCN01-DR-OCC         OCCURS  3.
                   07  ORCHCN01-DR         PIC X(05).
      *    ������ ... Ǥ��
           03  ORCHCN01-NYUINYMD           PIC X(08).
      *    �ౡ�� ... Ǥ��
           03  ORCHCN01-TAIINYMD           PIC X(08).