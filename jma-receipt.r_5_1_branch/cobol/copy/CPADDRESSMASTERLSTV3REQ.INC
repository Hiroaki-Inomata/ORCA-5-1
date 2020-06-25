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
      *    ���Уɽ���ޥ���������� (xml2)
      *                             COPY (CPADDRESSMASTERLSTV3REQ.INC)
      ******************************************************************
       01  XML-LSTREQ.
           03  LSTREQ-CONTEXT              PIC S9(9)   BINARY.
           03  LSTREQ-OBJECT               PIC X(36).
           03  LSTREQ-MODE                 PIC S9(9)   BINARY.
           03  LSTREQ-RECORDNAME           PIC X(128).
           03  LSTREQ-MEDICALREQ.
      *        �ꥯ�������ֹ�
               05  LSTREQ-REQUEST-NUMBER   PIC X(2).
      *        UID���Żҥ���ơ�
               05  LSTREQ-KARTE-UID        PIC X(36).
      *        ���륫UID
               05  LSTREQ-ORCA-UID         PIC X(36).
      *
               05  LSTREQ-PREFNUM-G.
                   07  LSTREQ-PREFNUM-OCC      OCCURS  10.
                       09  LSTREQ-PREFNUM  PIC X(2).
      *