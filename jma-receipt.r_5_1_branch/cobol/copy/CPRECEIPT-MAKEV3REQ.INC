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
      *    ���Уɥ쥻�ץȾ���ʥ쥻�ץȺ�����(xml2)
      *                                   COPY (CPRECEIPT-MAKEV3REQ.INC)
      ******************************************************************
        01  XML-RECEIPT-MAKEREQ.
            02  RECEMAKEQ-CONTEXT          PIC S9(9)   BINARY.
            02  RECEMAKEQ-OBJECT           PIC X(36).
            02  RECEMAKEQ-MODE             PIC S9(9)   BINARY.
            02  RECEMAKEQ-RECORDNAME       PIC X(128).
            02  RECEMAKEQ-PATIENTREQ.
      *         �ꥯ�������ֹ�
                03  RECEMAKEQ-REQUEST-NUMBER PIC X(2).
      *         �����UID
                03  RECEMAKEQ-KARTE-UID    PIC X(36).
      *         ���륫UID
                03  RECEMAKEQ-ORCA-UID     PIC X(36).
      *         �»���
                03  RECEMAKEQ-PERFORM-DATE PIC X(10).
      *         ����ǯ��
                03  RECEMAKEQ-PERFORM-MONTH PIC X(7).
      *         ������ʬ
                03  RECEMAKEQ-INOUT        PIC X(1).
      *         ������ʬ
                03  RECEMAKEQ-RECEIPT-MODE PIC X(10).
      *         �����⡼��
                03  RECEMAKEQ-PRINT-MODE   PIC X(6).
      *         �����
                03  RECEMAKEQ-SUBMISSION-MODE  PIC X(2).
      *         �����оݴ��԰���
                03  RECEMAKEQ-PATIENT-INFORMATION  OCCURS  100.
      *             �����оݡݴ����ֹ�
                    05  RECEMAKEQ-PATIENT-ID   PIC X(20).
      *             �����оݡݿ���ǯ��
                    05  RECEMAKEQ-PATIENT-PERFORM-DATE  PIC X(7).
      *