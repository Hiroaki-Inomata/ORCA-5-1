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
      *    �����������                       (CPTEIKIRRK.INC)   *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TEIKIRRK-REC.
           03  TEIKIRRK-HOSPNUM             PIC 9(2).
      *����ǯ��
           03  TEIKIRRK-SRYYM              PIC  X(06).
      *����
      *    ���������ƽ�����
           03  TEIKIRRK-KBT                PIC  X(01).
      *ü���ɣ�
           03  TEIKIRRK-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  TEIKIRRK-OPID               PIC  X(16).
      *����ǯ����
           03  TEIKIRRK-CREYMD             PIC  X(08).
      *����ǯ����
           03  TEIKIRRK-UPYMD              PIC  X(08).
      *��������
           03  TEIKIRRK-UPHMS              PIC  X(06).