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
      *    �������Ź԰پȲ�ơ��֥�    COPY (CPNSRYSRH.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����     ����
      ************************************************************
      *01  NSRYSRH-REC.
           03  NSRYSRH-HOSPNUM         PIC 9(02).
           03  NSRYSRH-PTID            PIC 9(10).
           03  NSRYSRH-ZAINUM          PIC 9(09).
           03  NSRYSRH-SRYYM           PIC X(06).
           03  NSRYSRH-SRYKA           PIC X(02).
           03  NSRYSRH-HKNCOMBINUM     PIC X(04).
           03  NSRYSRH-SRYKBN          PIC X(02).
           03  NSRYSRH-SRYCD           PIC X(09).
           03  NSRYSRH-SRYSURYO        PIC 9(05)V9(05).
           03  NSRYSRH-BRMNUM          PIC X(08).
           03  NSRYSRH-RM-SAGAKU       PIC X(02).
           03  NSRYSRH-TERMID          PIC X(16).
           03  NSRYSRH-OPID            PIC X(16).
           03  NSRYSRH-CREYMD          PIC X(08).
           03  NSRYSRH-UPYMD           PIC X(08).
           03  NSRYSRH-UPHMS           PIC X(06).
      *    �ʲ� virtual
           03  NSRYSRH-SELRRKNUM       PIC 9(03).
           03  NSRYSRH-SELSRYYMD       PIC X(08).