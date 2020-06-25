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
      *    ���Уɥ쥻�ťǡ�������ʥ쥻�ťǡ���������(xml2)
      *                                   COPY (CPRECEIPTDATA-MAKEV3REQ.INC)
      ******************************************************************
        01  XML-RECEIPTDATA-MAKEREQ.
            02  RECEDMAKEQ-CONTEXT          PIC S9(9)   BINARY.
            02  RECEDMAKEQ-OBJECT           PIC X(36).
            02  RECEDMAKEQ-MODE             PIC S9(9)   BINARY.
            02  RECEDMAKEQ-RECORDNAME       PIC X(128).
            02  RECEDMAKEQ-PATIENTREQ.
      *         �ꥯ�������ֹ�
                03  RECEDMAKEQ-REQUEST-NUMBER
                                           PIC X(2).
      *         �����UID
                03  RECEDMAKEQ-KARTE-UID   PIC X(36).
      *         ���륫UID
                03  RECEDMAKEQ-ORCA-UID    PIC X(36).
      *         �»���
                03  RECEDMAKEQ-PERFORM-DATE
                                           PIC X(10).
      *         ����ǯ��
                03  RECEDMAKEQ-PERFORM-MONTH
                                           PIC X(7).
      *         ����ǯ����
                03  RECEDMAKEQ-AC-DATE     PIC X(10).
      *         ������ʬ(02:�쥻��)
                03  RECEDMAKEQ-RECEIPT-MODE PIC X(02).
      *         ������ʬ
                03  RECEDMAKEQ-INOUT       PIC X(2).
      *         �����⡼��(Check:������ Ckeck�ʳ�:�����)
                03  RECEDMAKEQ-CREATE-MODE PIC X(6).
      *         �����
                03  RECEDMAKEQ-SUBMISSION-MODE
                                           PIC X(2).
      *         �쥻�ťǡ��������å�(Yes:�����å����� Yes�ʳ�:�����å����ʤ�)
                03  RECEDMAKEQ-CHECK-MODE  PIC X(3).
      *         �ݸ����ֹ�(00000000:ľ�����᤹���ݸ��԰ʳ� 00000000�ʳ�:ľ�����᤹���ݸ��԰ʳ�)
                03  RECEDMAKEQ-HKNJANUM    PIC X(8).
      *         ���ֻ���
                03  RECEDMAKEQ-START-MONTH PIC X(7).
                03  RECEDMAKEQ-END-MONTH   PIC X(7).
      *