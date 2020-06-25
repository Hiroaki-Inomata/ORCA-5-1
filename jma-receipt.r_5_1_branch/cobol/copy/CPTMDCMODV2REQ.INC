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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɥꥯ���������ӥǡ�������������� (xml2)
      *                                      COPY (CPTMDCMODV2REQ.INC)
      ******************************************************************
        01  XML-TMEDICALMODREQ.
            02  TMDCREQ-CONTEXT           PIC S9(9)   BINARY.
            02  TMDCREQ-OBJECT            PIC X(36).
            02  TMDCREQ-MODE              PIC S9(9)   BINARY.
            02  TMDCREQ-RECORDNAME        PIC X(128).
            02  TMDCREQ-TMEDICALREQ.
      *       �ꥯ�������ֹ�
              03  TMDCREQ-REQUEST-NUMBER    PIC X(02).
      *       �����ֹ�
              03  TMDCREQ-PATIENTID         PIC X(20).
      *       ��������
              03  TMDCREQ-PERFORM-DATE      PIC X(10).
      *       ������ʬ�ʣ���������
              03  TMDCREQ-OUTPATIENT-CLASS  PIC X(1).
      *       ���Ų�
              03  TMDCREQ-DEP-CODE          PIC X(02).
      *       �ݸ��ȹ礻�ֹ�
              03  TMDCREQ-COMBINATION-NUMBER   PIC X(04).
      *       Ʊ���������ʣ���Ʊ����������
              03  TMDCREQ-DOUJITSUKBN          PIC X(01).
      *       �Żҥ���ƥ��������ӥǡ����Σգɣġ�
              03  TMDCREQ-MEDICAL-UID          PIC X(36).
      *HAORI����
      *       Ÿ��������ʬ
              03  TMDCREQ-LOCK-MODE         PIC X(01).
      *       UID���Żҥ���ơ�
              03  TMDCREQ-KARTE-UID         PIC X(36).
      *       UID�ʥ��륫��
              03  TMDCREQ-ORCA-UID          PIC X(36).
      *