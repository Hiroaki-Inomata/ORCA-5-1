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
      *          ����������� COPY (SEI0311.INC) *
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����      ����
      ********************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
               05  MF100-KBN             PIC  X(02).
               05  MF100-CITYNUM         PIC  X(02).
      *
           03  MF100-SRYYM               PIC  X(06).
           03  MF100-CITYNAME            PIC  X(10).
           03  MF100-NYUGAI-TBL  OCCURS 2.
               07  MF100-KENSU           PIC  9(06).
               07  MF100-TENSU           PIC  9(09).
               07  MF100-FTNMONEY        PIC  9(09).
           03  MF100-TOKKI-TBL  OCCURS 15.
               05  MF100-TOKKI-NYUGAI-TBL  OCCURS 2.
                   07  MF100-TOKKIKENSU  PIC  9(03).
           03  MF100-KOHI-TBL   OCCURS 15.
               05  MF100-KOHI-NYUGAI-TBL   OCCURS 2.
                   07  MF100-KOHIKENSU   PIC  9(03).
