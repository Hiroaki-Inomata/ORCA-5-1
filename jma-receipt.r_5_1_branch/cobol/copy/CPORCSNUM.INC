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
      *    �����Ѵ����֥ѥ�᥿                         *
      *                         COPY  (CPORCSNUM.INC)    *
      *
      *     ����������   : ZERO �����ｪλ��
      *                    1     (�����ͥ��顼��
      *                    3     (���ͣ�����ʾ����Υ��顼��
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNUMAREA.
      *������
           03  SNUM-INX              PIC X(26).
      *����������
           03  SNUM-OUTNUM           PIC S9(12)V9(06).
      *������
           03  SNUM-OUTEDT           PIC X(15).
      *���
           03  SNUM-KETSUU           PIC 9(02).
      *���������
           03  SNUM-SEISUU           PIC 9(02).
      *���������
           03  SNUM-SYOSUU           PIC 9(02).
      *����
           03  SNUM-MINKBN           PIC X(01).
      *������̵ͭ
           03  SNUM-SYOKBN           PIC X(01). 
      *
      *����������
           03  SNUM-RC               PIC S9(9)   BINARY.
      *