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
      *    ���Уɽ���ޥ���������� (xml2)
      *                             COPY (CPADDRESSMASTERLSTV3RES.INC)
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
      *        ���ֹ�
               05  LSTRES-PREFNUM-G.
                   07  LSTRES-PREFNUM-OCC      OCCURS  10.
                       10  LSTRES-PREFNUM  PIC X(2).
      *        ���
               05  LSTRES-OUTPUT-COUNT     PIC X(10).
      *
              05  LSTRES-DATA-ID-INFORMATION   OCCURS  10.
                  07  LSTRES-DATA-ID       PIC X(37).
                  07  LSTRES-PRINT-ID      PIC X(37).