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
      *    �����ϰ�Ϣ��                  COPY  (CPPTNUM-PUBLIC.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTPUBLIC-REC.
      *����
           03  PTPUBLIC-KEY.
      *���ŵ��أɣ�
               05  PTPUBLIC-HOSPNUM        PIC  9(2).
      *���ԣɣ�
               05  PTPUBLIC-PTID           PIC  9(10).
      *�ϰ�Ϣ�ȣɣ�
           03  PTPUBLIC-PATIENT-ID1        PIC  X(20).
      *������Ʊ��̵ͭ
           03  PTPUBLIC-AGREEMENT          PIC  X(01).
      *ü���ɣ�
           03  PTPUBLIC-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTPUBLIC-OPID               PIC  X(16).
      *����ǯ����
           03  PTPUBLIC-CREYMD             PIC  X(08).
      *����ǯ����
           03  PTPUBLIC-UPYMD              PIC  X(08).
      *��������
           03  PTPUBLIC-UPHMS              PIC  X(06).
      *
