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
      *    ���Уɥꥯ�����ȣạ̃��ɣ������������� (xml2)
      *                                      COPY (CPCLAIMACCOUNTV02REQ.INC)
      ******************************************************************
       01  XML-CLAIM-ACCOUNTV2REQ.
           02  CLACREQ-CONTEXT           PIC S9(9)   BINARY.
           02  CLACREQ-OBJECT            PIC X(36).
           02  CLACREQ-MODE              PIC S9(9)   BINARY.
           02  CLACREQ-RECORDNAME        PIC X(128).
           02  CLACREQ-CLAIM-FRONT.
      *        �ꥯ�������ֹ�
               03  CLACREQ-REQUEST-NUMBER    PIC X(02).
      *        �����ѥ�����
               03  CLACREQ-SEND-CHARACTER-CODE   PIC X(01).
      *        �����ֹ�
               03  CLACREQ-PATIENTID         PIC X(20).
      *        ��������
               03  CLACREQ-PERFORM-DATE      PIC X(10).
      *        ���Ų�
               03  CLACREQ-DEP-CODE          PIC X(02).
      *        �ɥ�����
               03  CLACREQ-DR-CODE           PIC X(05).
      *        �ݸ��ȹ礻�ֹ�
               03  CLACREQ-COMBINATION-NUMBER      PIC X(04).
      *        ��ɼ�ֹ�
               03  CLACREQ-INVOICE-NUMBER    PIC X(07).
      *        ������ʬ�ʣ���������
               03  CLACREQ-UPDATE-CODE       PIC X(01).
      *
