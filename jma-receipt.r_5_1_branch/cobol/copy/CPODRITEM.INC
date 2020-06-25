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
      *    ��������������              COPY (CPODRITEM.INC)           *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  ODRITEM-REC.
           03  ODRITEM-KEY.
               05  ODRITEM-HOSPNUM        PIC 9(2).
               05  ODRITEM-KARTE-KEY     PIC X(36).
               05  ODRITEM-ORDERNUM      PIC 9(02).
               05  ODRITEM-ITEMNUM       PIC 9(02).
           03  ODRITEM-ITEMID            PIC X(05).
           03  ODRITEM-ITEM              PIC X(10).
           03  ODRITEM-MEMO              PIC X(128).
           03  ODRITEM-ERRCD             PIC X(04).
           03  ODRITEM-STATUS            PIC 9(01).
           03  ODRITEM-TERMID            PIC X(16).
           03  ODRITEM-OPID              PIC X(16).
           03  ODRITEM-CREYMD            PIC X(08).
           03  ODRITEM-UPYMD             PIC X(08).
           03  ODRITEM-UPHMS             PIC X(06).