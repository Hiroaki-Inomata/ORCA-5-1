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
      *    ���Ų�����  ������ COPY  (CPSRYKARRK.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SRYKARRK-REC.
      *��������
           03  KARRK-KEY.
      *  ���ŵ��أɣ�
               05  KARRK-HOSPNUM            PIC 9(2).
      *  ���ԣɣ�
               05  KARRK-PTID              PIC  9(10).
      *  ���Ų�
               05  KARRK-SRYKA             PIC  X(02).
      *
      *  �������
           03  KARRK-SYOSINYMD1            PIC  X(08).
      *  �������
           03  KARRK-SYOSINYMD2            PIC  X(08).
      *  �ǽ�������
           03  KARRK-LASTYMD               PIC  X(08).
      *ü���ɣ�
           03  KARRK-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  KARRK-OPID                  PIC  X(16).
      *����ǯ����
           03  KARRK-CREYMD                PIC  X(08).
      *����ǯ����
           03  KARRK-UPYMD                 PIC  X(08).
      *��������
           03  KARRK-UPHMS                 PIC  X(06).
