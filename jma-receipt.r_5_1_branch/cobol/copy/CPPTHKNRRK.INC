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
      *    �����ݸ���ǧǯ��������    �� COPY  (CPPTHKNRRK.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTHKNRRK-REC.
      *����
           03  PTHKNRRK-KEY.
      *���ŵ��أɣ�
               05  PTHKNRRK-HOSPNUM           PIC 9(2).
      *���ԣɣ�
               05  PTHKNRRK-PTID              PIC  9(10).
      *�ݸ��ɣ�
               05  PTHKNRRK-HKNID             PIC  9(10).
      *Ϣ��
               05  PTHKNRRK-RENNUM            PIC  9(01).
      *��ǧǯ����
           03  PTHKNRRK-KAKUNINYMD            PIC  X(08).
      *ü���ɣ�
           03  PTHKNRRK-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTHKNRRK-OPID                  PIC  X(16).
      *����ǯ����
           03  PTHKNRRK-CREYMD                PIC  X(08).
      *����ǯ����
           03  PTHKNRRK-UPYMD                 PIC  X(08).
      *��������
           03  PTHKNRRK-UPHMS                 PIC  X(06).