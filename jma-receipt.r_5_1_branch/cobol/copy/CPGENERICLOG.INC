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
      *         ����̾���ܥ���       COPY (CPGENERICLOG.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  GENERICLOG-REC.
      *���ŵ��أɣ�
           03  GENERICLOG-HOSPNUM          PIC  9(02).
      *���ԣɣ�
           03  GENERICLOG-PTID             PIC  9(10).
      *������
           03  GENERICLOG-SRYYMD           PIC  X(08).
      *ȯ����
           03  GENERICLOG-PRINTYMD         PIC  X(08).
      *ȯ�Ի���
           03  GENERICLOG-PRINTHMS         PIC  X(06).
      *�ң��ֹ�
           03  GENERICLOG-RECIPENUM        PIC  9(02).
      *������ʬ
           03  GENERICLOG-SYORIKBN         PIC  9(01).
      *��ɼ�ֹ�
           03  GENERICLOG-DENPNUM          PIC  9(07).
      *���ֹ�
           03  GENERICLOG-ZAINUM           PIC  9(08).
      *���ť�����
           03  GENERICLOG-SRYCD            PIC  X(09).
      *������ॳ���ɡʰ���̾�����ɡ�
           03  GENERICLOG-YAKKAKJNCD       PIC  X(12).
      *����̾��������
           03  GENERICLOG-GENERICNAME      PIC  X(200).
      *
