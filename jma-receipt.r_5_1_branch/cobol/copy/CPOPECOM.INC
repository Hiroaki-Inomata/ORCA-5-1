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
      *  �����쥻�ץȥ����ȥޥ�����       (CPOPECOM.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  OPECOM-REC.
           03  OPECOM-KEY.
      *���ŵ��أɣ�
               05  OPECOM-HOSPNUM        PIC 9(2).
      *���ԣɣ�
               05  OPECOM-PTID          PIC 9(10).
      *���ǯ����
               05  OPECOM-SRYYMD        PIC X(08).
      *���ť�����
               05  OPECOM-SRYCD         PIC X(09).
      *Ϣ��
               05  OPECOM-RENNUM        PIC 9(03).
      *���̾
           03  OPECOM-OPENAME           PIC X(100).
      *������
           03  OPECOM-OPECOMMENT        PIC X(100).
      *������ʬ�ʣ���������������������
           03  OPECOM-NYUGAIKBN         PIC X(01).
      *���Ų�
           03  OPECOM-SRYKA             PIC X(02).
      *�ݸ��ֹ�
           03  OPECOM-HKNNUM            PIC X(03).
      *ϫ���ݸ���ʬ�ʣ���û�����ա���������ǯ�⡡�������ե���������
           03  OPECOM-HKNKBN            PIC X(01).
      *�ݸ��ɣ�
           03  OPECOM-HKNID             PIC 9(10).
      *�ݸ��ȹ礻
           03  OPECOM-HKNCOMBI          PIC 9(04).
      *ͭ������ǯ��
           03  OPECOM-TEKSTYM           PIC X(06).
      *ͭ����λǯ��
           03  OPECOM-TEKEDYM           PIC X(06).
      *ü���ɣ�
           03  OPECOM-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  OPECOM-OPID              PIC X(16).
      *����ǯ����
           03  OPECOM-CREYMD            PIC X(08).
      *����ǯ����
           03  OPECOM-UPYMD             PIC X(08).
      *��������
           03  OPECOM-UPHMS             PIC X(06).
