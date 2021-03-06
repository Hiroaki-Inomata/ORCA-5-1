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
      *    ���Уɥꥯ���������ӽ�λ���� (xml2)  COPY (CPMDCXMLV2REQ.INC)
      ******************************************************************
        01  XML-MEDICALREQ.
            02  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
            02  MDCREQ-OBJECT           PIC X(36).
            02  MDCREQ-MODE             PIC S9(9)   BINARY.
            02  MDCREQ-RECORDNAME       PIC X(128).
            02  MDCREQ-MEDICALREQ.
      *       ������ʬ�ʣ���������
              03  MDCREQ-OUTPATIENT-CLASS  PIC X(1).
      *       �����ֹ�
              03  MDCREQ-PATIENTID         PIC X(20).
      *       ��������
              03  MDCREQ-PERFORM-DATE      PIC X(10).
      *       ���Ż���
              03  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID
              03  MDCREQ-MEDICAL-UID       PIC X(36).
      *       ������
              03  MDCREQ-ADMISSION-DATE    PIC X(10).
      *
              03  MDCREQ-MEDICAL-INFO.
      *         ���Ų�
                04  MDCREQ-APPOINT-DEP-CODE PIC X(2).
      *         �ɥ�����������
                04  MDCREQ-APPOINT-DR-CODE  PIC X(5).
      *         �ݸ�����
                04  MDCREQ-INSURANCE-INFO.
      *           �ݸ��ȹ礻�ֹ�
                  05  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *           �ݸ��μ���
                  05  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           �ݸ����ֹ�
                  05  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           �ݸ��μ���̾��
                  05  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           ����
                  05  MDCREQ-MAIN-MARK               PIC X(80).
      *           �ֹ�
                  05  MDCREQ-MAIN-NUMBER             PIC X(80).
      *           ��³��ʬ
                  05  MDCREQ-MAIN-CONTINUATION       PIC X(01).
      *           �����ʬ
                  05  MDCREQ-MAIN-ASSISTANCE         PIC X(01).
      *           �ܿͲ�²��ʬ
                  05  MDCREQ-MAIN-FAMILY             PIC X(01).
      *           ���ݸ���̾
                  05  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           ͭ��������
                  05  MDCREQ-MAIN-START-DATE         PIC X(10).
      *           ͭ����λ��
                  05  MDCREQ-MAIN-END-DATE           PIC X(10).
      *           �������
                  05  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             ����μ���
                    06  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���̾��
                    06  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    06  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    06  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *             ͭ��������
                    06  MDCREQ-INSURANCE-START-DATE  PIC X(10).
      *             ͭ����λ��
                    06  MDCREQ-INSURANCE-END-DATE    PIC X(10).
      *         ��������
                04  MDCREQ-MDC-INFO         OCCURS  40 TIMES.
      *           ���ż��̶�ʬ
                  05  MDCREQ-MDC-CLASS               PIC X(3).
      *??
                  05  MDCREQ-MDC-CLASS-NAME          PIC X(50).
      *           ���
                  05  MDCREQ-MDC-CLASS-NUMBER        PIC X(50).
      *           ������
                  05  MDCREQ-PRESCRIPTION-INFO     OCCURS  40 TIMES.
      *             ���ť�����
                    06  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *             ����̾�Ρʥ����ȡ�
      *R02.4        06  MDCREQ-PRSCRPT-NAME          PIC X(80).
                    06  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *             ����
                    06  MDCREQ-PRSCRPT-NUMBER        PIC X(40).
      *             �����������̾����
                    06  MDCREQ-GENERIC-FLG           PIC X(03).
      *H29.4
      *             ��³�����Ȼؼ���ʬ
                    06  MDCREQ-CONTKBN               PIC X(01).
      *             �����������ʬ
                    06  MDCREQ-NAIFKBN               PIC X(01).
      *
      *         ��̾
                04  MDCREQ-DIAGNOSIS-INFO          OCCURS  50 TIMES.
                  05  MDCREQ-DIAGNOSIS-NUMBER        PIC X(50).
      *ver.4.7    ������ʬ
                  05  MDCREQ-DIAGNOSIS-INOUT         PIC X(01).
                  05  MDCREQ-DIAGNOSIS-NAME          PIC X(80).
                  05  MDCREQ-DIAGNOSIS-SINGLE      OCCURS  6  TIMES.
                    07 MDCREQ-DIAGNOSIS-SINGLE-NUMBER
                                                     PIC X(10). 
                    07 MDCREQ-DIAGNOSIS-SINGLE-NAME
                                                     PIC X(20). 
      *           ��­������
                  05  MDCREQ-DIAGNOSIS-SUPPLEMENT.
                      07  MDCREQ-DIAGNOSIS-SCODE1        PIC X(10).
                      07  MDCREQ-DIAGNOSIS-SCODE2        PIC X(10).
                      07  MDCREQ-DIAGNOSIS-SCODE3        PIC X(10).
                      07  MDCREQ-DIAGNOSIS-SNAME         PIC X(40).
      *
                  05  MDCREQ-DIAGNOSIS-CATEGORY      PIC X(2).
                  05  MDCREQ-DIAGNOSIS-SUSPECTEDFLAG PIC X(2).
                  05  MDCREQ-DIAGNOSIS-START-DATE    PIC X(10).
                  05  MDCREQ-DIAGNOSIS-END-DATE      PIC X(10).
                  05  MDCREQ-DIAGNOSIS-OUTCOME       PIC X(2).
