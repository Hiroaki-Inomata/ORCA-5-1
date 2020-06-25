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
      *    APIV3�ѥ�᥿��  COPY  (CPAPIPARA.INC)               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  APIPARA-REC.
           03  APIPARA-KEY.
               05  APIPARA-HOSPNUM           PIC 9(2).
      *��̳�ɣ�
               05  APIPARA-GYOUMUID          PIC X(08).
      *ü���ɣġ�ORCA-UID)
               05  APIPARA-TERMID            PIC X(64).
      *�ե�����̾
               05  APIPARA-FILEMEI           PIC X(20).
      *����
               05  APIPARA-EDANUM            PIC 9(03).
      *�����UID
           03  APIPARA-KARTE-UID             PIC X(36).
      *�����ֹ�
           03  APIPARA-PTNUM                 PIC X(20).
      *�ǡ����ΰ�
           03  APIPARA-DATA-REC              PIC X(5000).
      *����ǯ����
           03  APIPARA-UPYMD                 PIC X(08).
      *��������
           03  APIPARA-UPHMS                 PIC X(06).