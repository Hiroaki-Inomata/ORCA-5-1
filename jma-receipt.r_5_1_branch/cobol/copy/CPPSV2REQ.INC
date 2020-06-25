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
       01  PSREQ-AREA.
           03  PSREQ-CONTEXT               PIC S9(9)   BINARY.
           03  PSREQ-OBJECT                PIC X(36).
           03  PSREQ-MODE                  PIC S9(9)   BINARY.
           03  PSREQ-RECORDNAME            PIC X(128).
           03  PSREQ-DATA-AREA.
      *       �ꥯ�������ֹ�
              05  PSREQ-REQNUM             PIC X(2).
      *       �����⡼��
              05  PSREQ-PRINT-MODE         PIC X(10).
      *       �����ֹ�
              05  PSREQ-PTNUM              PIC X(20).
      *       ��ɼ�ֹ�
              05  PSREQ-DENPNUM            PIC X(7).
      *       ������ʬ
              05  PSREQ-INGAIKBN           PIC X(5).