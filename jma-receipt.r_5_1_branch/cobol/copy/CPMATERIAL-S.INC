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
      *         ������Ŭ������  COPY (CPMATERIAL-S.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MATERIAL-S-REC.
           03  MATERS-KEY.
               05  MATERS-SRYCD           PIC  X(09).
               05  MATERS-RENNUM1         PIC  9(03).
               05  MATERS-RENNUM2         PIC  9(02).
      *    ����̾̾��
           03  MATERS-BYOMEI           PIC  X(200).
      *    ����̾������
           03  MATERS-BYOMEICD         PIC  X(07).
      *    Ŭ������̾��ʬ
           03  MATERS-BYOMEIKBN        PIC  9(01).
      *
           03  MATERS-CREYMD           PIC  X(08).
           03  MATERS-UPYMD            PIC  X(08).
           03  MATERS-UPHMS            PIC  X(06).
