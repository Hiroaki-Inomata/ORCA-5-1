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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ******************************************************
       01  STMTREQ-AREA.
           03  STMTREQ-CONTEXT             PIC S9(9)   BINARY.
           03  STMTREQ-OBJECT              PIC X(36).
           03  STMTREQ-MODE                PIC S9(9)   BINARY.
           03  STMTREQ-RECORDNAME          PIC X(128).
           03  STMTREQ-DATA-AREA.
      *       �ꥯ�������ֹ�
              05  STMTREQ-REQNUM          PIC X(2).
      *       �����ֹ�
              05  STMTREQ-PTNUM           PIC X(20).
      *       �����⡼��
              05  STMTREQ-PRINT-MODE       PIC X(10).
      *       ��ɼ�ֹ�
              05  STMTREQ-DENPNUM         PIC X(7).