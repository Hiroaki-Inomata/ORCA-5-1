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
      *    ���襫��ƽ��ϥѥ�᥿��     COPY (ORCHC01 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  06/02/06  �������ɲ�
      ************************************************************
       01  ORCHC01AREA.
      *    ������ʬ (2:Ƭ�񤭤Τ߰���/ 1:���٤ư����� ... ɬ��
      *             (3:�Хå���������
           03  ORCHC01-SYORIKBN           PIC X(01).
      *
      *    �������̾�Խ���ʬ
           03  ORCHC01-BYOMEIFLG       PIC X(01).
      *    ���ѥ�᥿
           03  ORCHC01-PRTAREA         PIC X(25000).
      *�����ΰ�
      *    ���顼������
           03  ORCHC01-RC              PIC 9(02).
      *    ���������
           03  ORCHC01-CNT             PIC 9(04).