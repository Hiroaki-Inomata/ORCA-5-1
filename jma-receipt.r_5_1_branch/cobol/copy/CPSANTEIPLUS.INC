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
      *    ���������ղþ���          COPY (CPSANTEIPLUS.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SANTEIPLUS-REC.
           03  SANTEIPLUS-KEY.
      *���ŵ��أɣ�
               05  SANTEIPLUS-HOSPNUM          PIC 9(2).
      *���ԣɣ�
               05  SANTEIPLUS-PTID             PIC  9(10).
      *����ǯ��
               05  SANTEIPLUS-SRYYM            PIC  X(06).
      *���ť�����
               05  SANTEIPLUS-SRYCD            PIC  X(09).
      *������ʬ
               05  SANTEIPLUS-NYUGAIKBN        PIC  X(01).
      *���Ų�
               05  SANTEIPLUS-SRYKA            PIC  X(02).
      *�ݸ��ȹ��ֹ�
               05  SANTEIPLUS-HKNCOMBINUM      PIC  9(04).
      *������
               05  SANTEIPLUS-DAYKEY           PIC  9(02).
      *Ϣ��
               05  SANTEIPLUS-RENNUM           PIC  9(02).
      *��ϥӥꥳ����
           03  SANTEIPLUS-COMMENT              PIC  X(80).
      *��λ����
           03  SANTEIPLUS-ENDYMD               PIC  X(08).
      *ü���ɣ�
           03  SANTEIPLUS-TERMID           PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SANTEIPLUS-OPID             PIC  X(16).
      *����ǯ����
           03  SANTEIPLUS-CREYMD           PIC  X(08).
      *����ǯ����
           03  SANTEIPLUS-UPYMD            PIC  X(08).
      *��������
           03  SANTEIPLUS-UPHMS            PIC  X(06).
      *
