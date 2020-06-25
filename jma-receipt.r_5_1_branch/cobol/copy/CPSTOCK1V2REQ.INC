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
      *    ���Уɥꥯ�����Ⱥ߸˴������� (xml2)
      *                                        COPY (CPSTOK1V02REQ.INC)
      ******************************************************************
        01  XML-STOCKREQ.
            02  STOCKREQ-CONTEXT           PIC S9(9)   BINARY.
            02  STOCKREQ-OBJECT            PIC X(36).
            02  STOCKREQ-MODE              PIC S9(9)   BINARY.
            02  STOCKREQ-RECORDNAME        PIC X(128).
            02  STOCKREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              03  STOCKREQ-REQUEST-NUMBER    PIC X(2).
      *       UID���Żҥ���ơ�
              03  STOCKREQ-KARTE-UID         PIC X(36).
      *       ���륫UID
              03  STOCKREQ-ORCA-UID          PIC X(36).
      *       �����
              03  STOCKREQ-BASE-DATE         PIC X(10).
      *       ������ʬ�ʳ���Τߡ�
              03  STOCKREQ-INOUT             PIC X(01).
      *       �ƥ��ȴ��Զ�ʬ(True:�ޤ��
              03  STOCKREQ-CONTAIN-TESTPATIENT
                                             PIC X(04).
      *