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
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɥꥯ�����ȿ��Ź԰���Ͽ���� (���β�)
      *                                       COPY (CPMDCXMLV3REQ4.INC)
      *         ���Ź԰ٺ��
      ******************************************************************
       01  XML-MEDICALV3REQ4.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *        �ꥯ�������ֹ�
               05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *        UID���Żҥ���ơ�
               05  MDCREQ-KARTE-UID         PIC X(36).
      *        UID�ʥ��륫��
               05  MDCREQ-ORCA-UID          PIC X(36).
      *        �����ֹ�
               05  MDCREQ-PATIENTID         PIC X(20).
      *        ��������
               05  MDCREQ-PERFORM-DATE      PIC X(10).
      *        ������ʬ
               05  MDCREQ-PATIENT-MODE      PIC X(06).
      *        ��ɼ�ֹ�
               05  MDCREQ-INVOICD-NUMBER    PIC X(07).
      *
      *        ���Ų�
               05  MDCREQ-APPOINT-DEP-CODE  PIC X(2).
      *        �ݸ��ȹ礻�ֹ�
               05  MDCREQ-COMBINATION-NUMBER   PIC X(04).
      *        Ϣ��
               05  MDCREQ-NUMBER            PIC X(01).
      *
      *        ��ǧ��å���������
               05  MDCREQ-SELECT-ANSWER     PIC X(02).
      *
      *