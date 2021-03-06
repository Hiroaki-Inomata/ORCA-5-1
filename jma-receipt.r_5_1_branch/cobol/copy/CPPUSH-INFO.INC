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
      *  PUSH���ξ����Ǽ��       (CPPUSH-INFO.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PUSHINFO-REC.
           03  PUSH-KEY.
      *���ŵ��أɣ�
               05  PUSH-HOSPNUM        PIC 9(2).
      *UID
               05  PUSH-UID            PIC X(37).
      *
           03  PUSH-EVENT              PIC X(50).
      *
           03  PUSH-PATIENT-MODE       PIC X(10).
      *�����ֹ�
           03  PUSH-PTNUM              PIC X(20).
      *���ԣɣ�
           03  PUSH-PTID               PIC 9(10).
      *������ա��Խ���
           03  PUSH-INFORMATION-DATE   PIC X(10).
      *������֡��Խ���
           03  PUSH-INFORMATION-TIME   PIC X(08).
      *�������ա��Խ���
           03  PUSH-PERFORM-DATE       PIC X(10).
      *���ž���
           03  PUSH-PUSHINFO           PIC X(50).
      *��������
           03  PUSH-SRYYMD             PIC X(08).
      *���ڡ��졼���ɣ�
           03  PUSH-OPID               PIC  X(16).
      *����ǯ����
           03  PUSH-CREYMD             PIC  X(08).
      *��������
           03  PUSH-UPHMS              PIC  X(06).
      *
