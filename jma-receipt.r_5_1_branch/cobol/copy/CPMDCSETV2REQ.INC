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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɡ����åȹ������� (xml2)  COPY (CPMDCSETV2REQ.INC)
      ******************************************************************
        01  XML-MEDICALSETREQ.
            02  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
            02  MDCREQ-OBJECT           PIC X(36).
            02  MDCREQ-MODE             PIC S9(9)   BINARY.
            02  MDCREQ-RECORDNAME       PIC X(128).
            02  MDCREQ-MEDICALREQ.
      *        �ꥯ�������ֹ�
               03  MDCREQ-REQUEST-NUMBER   PIC X(2).
      *        �����
               03  MDCREQ-BASE-DATE        PIC X(10).
      *        ���åȥ�����
               03  MDCREQ-SETCD            PIC X(06).
      *        ���å�̾��
               03  MDCREQ-SETCD-NAME       PIC X(80).
      *        ͭ������ǯ����
               03  MDCREQ-START-DATE       PIC X(10).
      *        ͭ����λǯ����
               03  MDCREQ-END-DATE         PIC X(10).
      *        ������ʬ�ʣɡ�������
               03  MDCREQ-OUTPATIENT-CLASS PIC X(01).
      *        ���ž���
               03  MDCREQ-MDC-INFO-G.
                  05  MDCREQ-MDC-INFO          OCCURS   40.
      *             ���ż��̶�ʬ
                    09  MDCREQ-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCREQ-MDC-CLASS-NAME        PIC X(40).
      *             ���
                    09  MDCREQ-MDC-CLASS-NUMBER      PIC X(03).
      *
                    09  MDCREQ-PRESCRIPTION-INFO    OCCURS   40.
      *                 ���ť�����
                        11  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *                 ����̾��
                        11  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *                 ����
                        11  MDCREQ-PRSCRPT-NUMBER        PIC X(11).
      *                 ������
                        11  MDCREQ-PRSCRPT-MONEY         PIC X(07).
      *                 ��������������
                        11  MDCREQ-PRSCRPT-ATAI-G.
                            13  MDCREQ-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 ��³�����Ȼؼ���ʬ
                        11  MDCREQ-PRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        11  MDCREQ-PRSCRPT-NAIFKBN       PIC X(01).
      *