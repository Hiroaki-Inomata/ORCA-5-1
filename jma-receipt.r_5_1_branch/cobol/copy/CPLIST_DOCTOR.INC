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
      *****************************************************************
      *  ���Ų���Υɥ���������              (CPLIST_DOCTOR.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  LIST-DOCTOR-REC.
      *
      *����
           03  LSDOCTOR-KEY.
      *���ŵ��أɣ�
               05  LSDOCTOR-HOSPNUM     PIC 9(02).
      *���Ų�
               05  LSDOCTOR-SRYKA       PIC X(02).
      *ɽ����
               05  LSDOCTOR-RENNUM      PIC 9(03).
      *�ɥ�����
           03  LSDOCTOR-DRCD            PIC X(05).
      *ü���ɣ�
           03  LSDOCTOR-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  LSDOCTOR-OPID            PIC  X(16).
      *����ǯ����
           03  LSDOCTOR-CREYMD          PIC  X(08).
      *����ǯ����
           03  LSDOCTOR-UPYMD           PIC  X(08).
      *��������
           03  LSDOCTOR-UPHMS           PIC  X(06).