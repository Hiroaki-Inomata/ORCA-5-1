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
      ******************************************************************
      *    �ǡ��������å�����ơ��֥�ʣ���    COPY  (CPDCSRY.INC)   *
      ******************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����     ����
      ************************************************************
      *01  DCSRY-REC.
           03  DCSRY-KEY.
               05  DCSRY-PTID          PIC 9(10).
               05  DCSRY-ZAINUM        PIC 9(08).
               05  DCSRY-ACCTKBN       PIC X(01).
               05  DCSRY-SRYRENNUM     PIC 9(03).
           03  DCSRY-SRYSYUKBN         PIC X(03).
           03  DCSRY-SRYCD             PIC X(09).
           03  DCSRY-SURYO             PIC 9(5)V9(05).
           03  DCSRY-MEISKYFLG         PIC X(01).
           03  DCSRY-058-TANICD        PIC X(03).
           03  DCSRY-058-SRYCD         PIC X(09).
           03  DCSRY-INPUTNUM          PIC 9(03).
           03  DCSRY-TERMID            PIC X(16).
           03  DCSRY-OPID              PIC X(16).
           03  DCSRY-CREYMD            PIC X(08).
           03  DCSRY-UPYMD             PIC X(08).
           03  DCSRY-UPHMS             PIC X(06).