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
      *    �ݸ��ȹ礻̾���Խ��ѥ�᥿����COPY (CPORCSHKNMEI.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSHKNMEIAREA.
      *
      *    �ݸ��ȹ礻�ֹ�
           03  ORCSHKNMEI-HKNCOMBINUM          PIC X(04).
      *    ���ԣɣ�
           03  ORCSHKNMEI-PTID                 PIC 9(10).
      *    ����ǯ����
           03  ORCSHKNMEI-SRYYMD               PIC X(08).
      *
           03  ORCSHKNMEI-OUT-AREA.
      *        �ݸ��ȹ礻�Խ�̾��
               05  ORCSHKNMEI-HKNCOMBIMEI      PIC  X(100).
      *        Ŭ�Ѵ���
               05  ORCSHKNMEI-TEKSTYMD         PIC  X(08).
               05  ORCSHKNMEI-TEKEDYMD         PIC  X(08).
      *        �ݸ�����
               05  ORCSHKNMEI-HKNNUM           PIC  X(03).
      *        �ݸ���ʬ
               05  ORCSHKNMEI-HKNKBN           PIC  9(01).
      *        ϫ�ҡ��ݸ���ʬ
               05  ORCSHKNMEI-RSI-HKNKBN       PIC  X(01).
      *���顼������
           03  ORCSHKNMEI-ERRCD                PIC 9(01).
      *