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
      *    ���Уɥ쥻�ץȾ���ʥ쥻�ץȰ�����(xml2)
      *                                   COPY (CPRECEIPT-PRINTV3REQ.INC)
      ******************************************************************
        01  XML-RECEIPT-PRINTREQ.
            02  RECEPRINTQ-CONTEXT         PIC S9(9)   BINARY.
            02  RECEPRINTQ-OBJECT          PIC X(36).
            02  RECEPRINTQ-MODE            PIC S9(9)   BINARY.
            02  RECEPRINTQ-RECORDNAME      PIC X(128).
            02  RECEPRINTQ-PATIENTREQ.
      *         �ꥯ�������ֹ�
                03  RECEPRINTQ-REQUEST-NUMBER PIC X(2).
      *         �����UID
                03  RECEPRINTQ-KARTE-UID   PIC X(36).
      *         ���륫UID
                03  RECEPRINTQ-ORCA-UID    PIC X(36).
      *         �»���
                03  RECEPRINTQ-PERFORM-DATE PIC X(10).
      *         ����ǯ��
                03  RECEPRINTQ-PERFORM-MONTH PIC X(7).
      *         ������ʬ
                03  RECEPRINTQ-INOUT       PIC X(1).
      *         ������ʬ
                03  RECEPRINTQ-RECEIPT-MODE PIC X(10).
      *         �����⡼��
                03  RECEPRINTQ-PRINT-MODE  PIC X(5).
      *         �����
                03  RECEPRINTQ-SUBMISSION-MODE  PIC X(2).
      *         ���ٰ���
                03  RECEPRINTQ-RECEIPT-INFORMATION.
                   05  RECEPRINTQ-SUBMISSION OCCURS  19.
      *                Ϣ��
                       07  RECEPRINTQ-SUBMISSION-POSITION
                                               PIC X(5).
                       07  RECEPRINTQ-DETAIL-INFORMATION OCCURS  36.
      *                    Ϣ��
                           09  RECEPRINTQ-DETAIL-POSITION
                                                   PIC X(5).
      *                    �����ؼ�(Yes :���� ����ʳ� :�������ʤ�)
                           09  RECEPRINTQ-PRINT-INSTRUCT
                                                   PIC X(5).
      *                    �����
                           09  RECEPRINTQ-SUBMISSION-IDENT
                                               PIC X(2).
      *                    ������������
                           09  RECEPRINTQ-SUBMISSION-CODE
                                                   PIC X(10).
      *                    ����
                           09  RECEPRINTQ-SUBMISSION-TYPE
                                                   PIC X(100).
      *                    ���
                           09  RECEPRINTQ-COUNT    PIC X(5).
      *                    ���
                           09  RECEPRINTQ-NUMBER-OF-SHEETS
                                                   PIC X(5).