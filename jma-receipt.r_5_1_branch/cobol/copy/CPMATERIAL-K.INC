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
      *         �������Ϣ�굻  COPY (CPMATERIAL-K.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MATERIAL-K-REC.
           03  MATERK-KEY.
      *        �����ॳ����
               05  MATERK-SRYCD           PIC  X(09).
      *        Ϣ�֣�
               05  MATERK-RENNUM1         PIC  9(03).
      *        Ϣ�֣�
               05  MATERK-RENNUM2         PIC  9(02).
      *    ���Ź԰٥�����
           03  MATERK-SCD              PIC  X(09).
      *
           03  MATERK-CREYMD           PIC  X(08).
           03  MATERK-UPYMD            PIC  X(08).
           03  MATERK-UPHMS            PIC  X(06).
