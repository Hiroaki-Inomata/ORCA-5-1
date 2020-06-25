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
      *    ���Уɥꥯ������ʻ�Ѷش����ޥ����å�����
      *                                    COPY (CPCONTRACHECKV2REQ.INC)
      ******************************************************************
       01  XML-CONTRA-CHECKV2REQ.
           03  CHKREQ-CONTEXT          PIC S9(9)   BINARY.
           03  CHKREQ-OBJECT           PIC X(36).
           03  CHKREQ-MODE             PIC S9(9)   BINARY.
           03  CHKREQ-RECORDNAME       PIC X(128).
           03  CHKREQ-CONTRACHKREQ.
      *       �ꥯ�������ֹ�
              05  CHKREQ-REQUEST-NUMBER    PIC X(2).
      *       �����ֹ�
              05  CHKREQ-PATIENTID         PIC X(20).
      *       ��������
              05  CHKREQ-PERFORM-YM        PIC X(07).
      *       �оݴ��ַ��
              05  CHKREQ-CHECK-TUKI        PIC X(02).
      *
      *       ���޾���
              05  CHKREQ-MDC-INFO-G.
                  07  CHKREQ-PRESCRIPTION-INFO    OCCURS   30.
      *               ���ޥ�����
                      09  CHKREQ-PRSCRPT-CODE          PIC X(9).
                      09  CHKREQ-PRSCRPT-NAME          PIC X(80).
      *