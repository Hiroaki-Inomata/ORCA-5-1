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
      *    ����������                       (CPTEIKIKANRI.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TEIKIKANRI-REC.
           03  TEIKIKANRI-HOSPNUM          PIC 9(2).
      *�����ֹ�
           03  TEIKIKANRI-BTUNUM           PIC  X(02).
      *������ʬ�ʣ�����硢�������̡�
           03  TEIKIKANRI-SYOKBN           PIC  X(01).
      *������
           03  TEIKIKANRI-CREYMD           PIC  X(08).
      *��������
           03  TEIKIKANRI-CREHMS           PIC  X(06).
      *����ǯ��ʰ�硧����ǯ����̡������
           03  TEIKIKANRI-SRYYM            PIC  X(06).
      *�ڡ���
           03  TEIKIKANRI-PAGE             PIC  9(05).
      *�ڡ���
           03  TEIKIKANRI-KENSU            PIC  9(05).
      *ü���ɣ�
           03  TEIKIKANRI-TERMID           PIC  X(16).