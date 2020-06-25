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
      *    ���Уɥ쥻�ťǡ�������ʥ����å��ѥ쥻�ťǡ���������(xml2)
      *                        COPY (CPRECEIPTDATA-CHECK-MAKEV3RES.INC)
      ******************************************************************
       01  XML-RECEIPTDATA-CHECK-MAKERES.
           02  RECEDMAKES-CONTEXT          PIC S9(9)   BINARY.
           02  RECEDMAKES-OBJECT           PIC X(36).
           02  RECEDMAKES-MODE             PIC S9(9)   BINARY.
           02  RECEDMAKES-RECORDNAME       PIC X(128).
           02  RECEDMAKES-RECEIPT-MAKE-RES.
      *        �ꥯ�������ֹ�
               03  RECEDMAKES-REQUEST-NUMBER   PIC X(2).
      *
               03  RECEDMAKES-RESPONS-NUMBER   PIC X(2).
      *        �����UID
               03  RECEDMAKES-KARTE-UID    PIC X(36).
      *        ���륫UID
               03  RECEDMAKES-ORCA-UID     PIC X(36).
      *        �»���
               03  RECEDMAKES-INFORMATION-DATE
                                           PIC X(10).
      *        �»ܻ���
               03  RECEDMAKES-INFORMATION-TIME
                                           PIC X(8).
      *        �ֵѥ�����
               03  RECEDMAKES-API-RESULT   PIC X(3).
      *        �ֵѥ�å�����
               03  RECEDMAKES-API-RESULT-MSG
                                           PIC X(100).
      *
               03  RECEDMAKES-RESKEY       PIC X(100).
      *
               03  RECEDMAKES-SUBMISSION-MODE
                                           PIC X(2).
               03  RECEDMAKES-PERFORM-MONTH
                                           PIC X(7).
               03  RECEDMAKES-START-MONTH  PIC X(7).
               03  RECEDMAKES-END-MONTH    PIC X(7).
               03  RECEDMAKES-HKNJANUM     PIC X(8).
      *
               03  RECEDMAKES-DATA-ID-INFORMATION  OCCURS  10.
                   05  RECEDMAKES-DATA-ID  PIC X(37).
                   05  RECEDMAKES-PRINT-ID PIC X(37).
      *
      *        ��˥�
               03  RECEDMAKES-API-WARNING-INFO     OCCURS  5.
                   05  RECEDMAKES-API-WARNING
                                           PIC X(3).
                   05  RECEDMAKES-API-WARNING-MSG
                                           PIC X(100).
      *        ���顼���԰���
               03  RECEDMAKES-ERR-INFORMATION      OCCURS  100.
                   05  RECEDMAKES-PATIENT-ID
                                           PIC X(20).
                   05  RECEDMAKES-PATIENT-MONTH
                                           PIC X(7).
                   05  RECEDMAKES-PATIENT-INOUT
                                           PIC X(2).
                   05  RECEDMAKES-ERROR-CODE
                                           PIC X(3).
                   05  RECEDMAKES-ERROR-MSG
                                           PIC X(100).
                   05  RECEDMAKES-DETAIL-INFORMATION   OCCURS  30.
      *                ���̥�����
                       07  RECEDMAKES-DETAIL-CODE
                                           PIC X(04).
      *                ����
                       07  RECEDMAKES-DETAIL-TYPE
                                           PIC X(10).
                       07  RECEDMAKES-DETAIL-INOUT
                                           PIC X(1).
      *                �ݸ�����
                       07  RECEDMAKES-MAIN-INSURANCE.
      *                    �ݸ����ֹ�
                           09  RECEDMAKES-INSURANCE-NUMBER
                                           PIC X(08).
      *                    ����
                           09  RECEDMAKES-MAIN-MARK
                                           PIC X(40).
      *                    �ֹ�
                           09  RECEDMAKES-MAIN-NUMBER
                                           PIC X(40).
      *                    �����ʬ
                           09  RECEDMAKES-MAIN-ASSISTANCE
                                           PIC X(01).
      *                �������
                       07  RECEDMAKES-PUBLIC-INSURANCE.
      *                    ��ô���ֹ�
                           09  RECEDMAKES-PROVIDER-NUMBER
                                           PIC X(08).
      *                    ������ֹ�
                           09  RECEDMAKES-RECIPIENT-NUMBER
                                           PIC X(20).
      *                ϫ�Ҿ���
                       07  RECEDMAKES-ACCIDENT.
      *****************���¤�����
      *****************    09  RECEDMAKES-DISEASE-LOCATION
      *****************                    PIC X(100).
      *                    ϫƯ�ݸ��ֹ�
                           09  RECEDMAKES-ACCIDENT-NUMBER
                                           PIC X(14).
      *                     ǯ��ڽ��ֹ�
                           09  RECEDMAKES-PENSIONCERTIFICATE
                                           PIC X(09).
      *****************�ҳ���ʬ
      *****************    09  RECEDMAKES-ACCIDENT-CLASS
      *****************                    PIC X(1).
      *                �ҳ�������̵ͭ
                       07  RECEDMAKES-ACCIDENT-TARGET
                                           PIC X(4).
      *                �������̵ͭ
                       07  RECEDMAKES-INCARE-CLASS
                                           PIC X(4).
      *
                       07  RECEDMAKES-DETAIL-ERROR-CODE
                                           PIC X(3).
                       07  RECEDMAKES-DETAIL-ERROR-MSG
                                           PIC X(100).
                       07  RECEDMAKES-DETAIL-WARNING
                                            PIC X(3).
                       07  RECEDMAKES-DETAIL-WARNING-MSG
                                           PIC X(100).
      *        ���ֻ���
               03  RECEDMAKES-PERIOD-INFO          OCCURS  10.
                   05  RECEDMAKES-START-MONTH-O
                                           PIC X(7).
                   05  RECEDMAKES-END-MONTH-O
                                           PIC X(7).
                   05  RECEDMAKES-SELECT-MODE-O
                                           PIC X(2).
      *        �ݸ����ֹ�(00000000:ľ�����᤹���ݸ��԰ʳ�)
               03  RECEDMAKES-HKNJA-OCC            OCCURS  10.
                   05  RECEDMAKES-HKNJANUM-O
                                           PIC X(8).
                   05  RECEDMAKES-HKNJANAME-O
                                           PIC X(30).
      *        ���쥻�ץȷ��
               03  RECEDMAKES-ALL-COUNT    PIC X(5).
      *        �����
               03  RECEDMAKES-ALL-NUMBER-OF-SHEETS
                                           PIC X(5).