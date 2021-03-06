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
      *  ����������                 (CPHOURS.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HOURS-REC.
      *
      *����
           03  HOURS-KEY.
               05  HOURS-HOSPNUM       PIC 9(2).
      *������
               05  HOURS-SRYYMD        PIC  X(08).
      *�������
               05  HOURS-TIME          PIC  X(04).
      *�ɣ�
           03  HOURS-ID                PIC  X(12).
      *��������ʬ
           03  HOURS-HOURSKBN          PIC  X(1).
      *ü���ɣ�
           03  HOURS-TERMID            PIC  X(32).
      *���ڡ��졼���ɣ�
           03  HOURS-OPID              PIC  X(16).
      *����ǯ����
           03  HOURS-CREYMD            PIC  X(08).
      *����ǯ����
           03  HOURS-UPYMD             PIC  X(08).
      *��������
           03  HOURS-UPHMS             PIC  X(06).
      *����������
           03  HOURS-STSRYYMD          PIC  X(08).
      *��������λ
           03  HOURS-EDSRYYMD          PIC  X(08).
      *LIMIT
           03  HOURS-LIMIT             PIC 9(5).
