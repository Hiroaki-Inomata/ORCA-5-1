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
      ******************************************************************
      *    ���У���������װ������� (xml2)
      *                             COPY (CPSTATISTICSFORMV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  XML-LSTRES.
           03  LSTRES-CONTEXT              PIC S9(9)   BINARY.
           03  LSTRES-OBJECT               PIC X(36).
           03  LSTRES-MODE                 PIC S9(9)   BINARY.
           03  LSTRES-RECORDNAME           PIC X(128).
           03  LSTRES-MEDICALRES.
      *        �ꥯ�������ֹ�
               05  LSTRES-REQUEST-NUMBER   PIC X(2).
      *        �쥹�ݥ��ֹ�
               05  LSTRES-RESPONSE-NUMBER  PIC X(2).
      *        UID���Żҥ���ơ�
               05  LSTRES-KARTE-UID        PIC X(36).
      *        ���륫UID
               05  LSTRES-ORCA-UID         PIC X(36).
      *        �»���
               05  LSTRES-INFORMATION-DATE PIC X(10).
      *        �»ܻ���
               05  LSTRES-INFORMATION-TIME PIC X(8).
      *        �ֵѥ�����
               05  LSTRES-API-RESULT       PIC X(03).
      *        �ֵѥ�å�����
               05  LSTRES-API-RESULT-MSG   PIC X(100).
      *
               05  LSTRES-RESKEY           PIC X(100).
      *        ������ʬ
               05  LSTRES-STATISTICS-MODE  PIC X(7).
      *        ��������
               05  LSTRES-PROCESSING-G.
                   07  LSTRES-PROCESSING-INFO      OCCURS  100.
                       09   LSTRES-PROGRAM-NO      PIC X(3).
                       09   LSTRES-PROGRAM-NAME    PIC X(24).
                       09   LSTRES-PROGRAM-LABEL   PIC X(80).
                       09   LSTRES-PERFORM-CLASS   PIC X(05).
                       09   LSTRES-PARAMETER-G.
                           11   LSTRES-PARM-INFO   OCCURS  10.
                               13  LSTRES-PARM-NO  PIC X(2).
                               13  LSTRES-PARM-CLASS
                                                   PIC X(10).
                               13  LSTRES-PARM-LABEL
                                                   PIC X(20). 
                               13  LSTRES-PARM-REQUIRED-ITEM
                                                   PIC X(5).
                       09  LSTRES-PROGRAM-MSG      PIC X(100).
                       09  LSTRES-PROGRAM-OUTPUT-COUNT
                                                   PIC X(10).
      *
               05  LSTRES-PROCESSING-ERR-G.
                   07  LSTRES-PROCESSING-ERR-INFO  OCCURS  10.
                       09  LSTRES-ERR-PROGRAM-NO   PIC X(3).
                       09  LSTRES-ERR-PROGRAM-NAME PIC X(24).
                       09  LSTRES-ERR-PROGRAM-LABEL
                                                   PIC X(80).
                       09  LSTRES-ERR-PARAMETER-G.
                           11  LSTRES-ERR-PARAMETER-INFO   OCCURS  10.
                               13  LSTRES-ERR-PARM-NO  PIC X(2).
                               13  LSTRES-ERR-PARM-CLASS
                                                       PIC X(10).
                               13  LSTRES-ERR-PARM-LABEL
                                                       PIC X(20). 
                               13  LSTRES-ERR-PARM-REQUIRED-ITEM
                                                       PIC X(5).
                               13  LSTRES-ERR-PARM-VALUE
                                                       PIC X(20).
                       09  LSTRES-ERROR-CODE       PIC X(3).
                       09  LSTRES-ERROR-MSG        PIC X(100).
      *
      *        ���
               05  LSTRES-OUTPUT-COUNT     PIC X(10).
      *
              05  LSTRES-DATA-ID-INFORMATION   OCCURS  10.
                  07  LSTRES-DATA-ID       PIC X(37).
                  07  LSTRES-PRINT-ID      PIC X(37).