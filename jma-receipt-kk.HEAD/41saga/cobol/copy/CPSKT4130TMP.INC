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
      *    �����������     COPY (CPSKT4130TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *�������ô���ֹ�
               05  MF100-CITYNUM               PIC  X(06).
      *����ơ��֥�
           03  MF100-SEIKYU-TBL.
               05  MF100-NYUGAI-TBL  OCCURS 2.
      *���
                   07  MF100-KENSU             PIC  9(06).
      *����
                   07  MF100-NISSU             PIC  9(06).
      *����
                   07  MF100-TENSU             PIC  9(09).
      *������ô��
                   07  MF100-ITBFTN            PIC  9(09).
      *�������
               05  MF100-SKENSU                PIC  9(06).
      *��������
               05  MF100-SNISSU                PIC  9(06).
      *����������
               05  MF100-SKINGK                PIC  9(09).
      *����ɸ����ô��
               05  MF100-SFTNGK                PIC  9(09).
