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
      *    ���Уɥ쥻�ץȾ���ʥ쥻�ץȤ������������(xml2)
      *                                   COPY (CPRECEIPT-INF1V2RES.INC)
      ******************************************************************
       01  XML-RECEIPT-INFRES.
           02  RECEIPT-INFS-CONTEXT            PIC S9(9)   BINARY.
           02  RECEIPT-INFS-OBJECT             PIC X(36).
           02  RECEIPT-INFS-MODE               PIC S9(9)   BINARY.
           02  RECEIPT-INFS-RECORDNAME         PIC X(128).
           02  RECEIPT-INFS-RECEIPT-INF-RES.
      *        �»���
               03  RECEIPT-INFS-INFORMATION-DATE   PIC X(10).
      *        �»ܻ���
               03  RECEIPT-INFS-INFORMATION-TIME   PIC X(8).
      *        �ֵѥ�����
               03  RECEIPT-INFS-API-RESULT         PIC X(2).
      *        �ֵѥ�å�����
               03  RECEIPT-INFS-API-RESULT-MSG     PIC X(100).
               03  RECEIPT-INFS-RESKEY             PIC X(100).
      *
      *        �쥻�ץȾ���
               03  RECEIPT-INFS-INFO-G.
      *            ����ǯ��
                   05  RECEIPT-INFS-PERFORM-MONTH PIC X(7).
      *            �쥻�ץ������
                   05  RECEIPT-INFS-RECEIPT-COUNT PIC X(6).
      *            �쥻�ץ�������
                   05  RECEIPT-INFS-RECEIPT-POINT PIC X(10).