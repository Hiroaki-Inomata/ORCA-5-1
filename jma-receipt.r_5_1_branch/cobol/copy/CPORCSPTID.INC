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
      *    �����ֹ��Ѵ��ѥ�᥿                         *
      *                         COPY  (CPORCSPTID.INC)  *
      *
      *     SPTID-RC    : ZERO �����ｪλ��
      *                  1     (�ͥ��顼��
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSPTIDAREA.
      *������
           03  SPTID-HOSPNUM            PIC 9(2).
      *    �����ֹ湽�����󸡺���ʬ
           03  SPTID-SYS1009-KBN        PIC 9(1).
      *
           03  SPTID-PTNUM              PIC X(20).
           03  SPTID-PTID               PIC 9(10).
      *---- TAKE MOD ST 
           03  SPTID-DBSUBNAME          PIC X(20).
      *---- TAKE MOD ED 
      *
      *����������
           03  SPTID-RC                 PIC S9(9)   BINARY.
      *