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
      *    ���Уɥ쥹�ݥ����ӥǡ����������(xml2)
      *                                      COPY (CPTMDCMODV2RES.INC)
      ******************************************************************
       01  XML-TMEDICALMODRES.
            03  TMDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  TMDCRES-OBJECT              PIC X(36).
            03  TMDCRES-MODE                PIC S9(9)   BINARY.
            03  TMDCRES-RECORDNAME          PIC X(128).
            03  TMDCRES-MEDICALRES.
      *        �»���
               05  TMDCRES-INFORMATION-DATE  PIC X(10).
      *        �»ܻ���
               05  TMDCRES-INFORMATION-TIME  PIC X(8).
      *        �ֵѥ�����
               05  TMDCRES-API-RESULT        PIC X(3).
      *        �ֵѥ�å�����
               05  TMDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  TMDCRES-RESKEY            PIC X(100).
      *
      *        ��������
               05  TMDCRES-PERFORM-DATE      PIC X(10).
      *        ���Ծ���
               05  TMDCRES-PATIENT-INFOMATION.
      *            �����ֹ�
                   07  TMDCRES-PATIENTID        PIC X(20).
      *            ���Ի�̾
                   07  TMDCRES-NAME             PIC X(100).
      *            ���ԥ��ʻ�̾
                   07  TMDCRES-KANANAME         PIC X(100).
      *            ������
                   07  TMDCRES-BIRTHDAY         PIC X(10).
      *            ����
                   07  TMDCRES-SEX              PIC X(1).
      *        ������ʬ
               05  TMDCRES-OUTPATIENT-CLASS     PIC X(1).
      *        Ʊ���������ʣ���Ʊ����������
               05  TMDCRES-DOUJITSUKBN          PIC X(01).
      *        ���Ų�
               05  TMDCRES-DEP-CODE             PIC X(02).
      *        ���Ų�̾��
               05  TMDCRES-DEP-NAME             PIC X(20).
      *        �ɥ�����������
               05  TMDCRES-DR-CODE              PIC X(5).
      *        �ɥ�������̾
               05  TMDCRES-DR-NAME              PIC X(80).
      *        �ݸ��ȹ礻����
               05  TMDCRES-INSURANCE-INFO.
      *            �ݸ��ȹ礻�ֹ�
                   07  TMDCRES-COMBINATION-NUMBER   PIC X(04).
      *            �ݸ��μ���
                   07  TMDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *            �ݸ��μ���̾��
                   07  TMDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *            �ݸ����ֹ�
                   07  TMDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *            ����
                   07  TMDCRES-MAIN-MARK              PIC X(80).
      *            �ֹ�
                   07  TMDCRES-MAIN-NUMBER            PIC X(80).
      *            �������
                   07  TMDCRES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               ����μ���
                      09  TMDCRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      09  TMDCRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      09  TMDCRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      09  TMDCRES-RECIPIENT-NUMBER        PIC X(20).
      *            �ݸ��ȹ礻�ֹ楨�顼
                   07  TMDCRES-COMBINATION-ERR            PIC X(01).
      *
      *        �Żҥ���ƥ����ʣգɣġ�
               05  TMDCRES-MEDICAL-UID         PIC X(36).
      *        Ÿ�����ʬ
               05  TMDCRES-MODE-FLG            PIC X(01).
      *HAORI����
      *        UID���Żҥ���ơ�
               05  TMDCRES-KARTE-UID         PIC X(36).
      *        UID�ʥ��륫��
               05  TMDCRES-ORCA-UID          PIC X(36).
      *
      *        ���������ֵ�
               05  TMDCRES-MEDICAL-INF-G.
                   07  TMDCRES-MDC-INFO          OCCURS   120.
      *                ���Ŷ�ʬ
                       09  TMDCRES-MDC-CLASS-KBN         PIC X(2).
      *                ���ż��̶�ʬ
                       09  TMDCRES-MDC-CLASS             PIC X(3).
      *                ���ż���̾��
                       09  TMDCRES-MDC-CLASS-NAME        PIC X(40).
      *                ���
                       09  TMDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *                ������������
                       09  TMDCRES-MDC-DAY-G.
                           11  TMDCRES-MDC-DAY           PIC X(01)
                                                      OCCURS   31.
      *
                       09  TMDCRES-PRSCRPT-INFO-G.
                           10  TMDCRES-PRSCRPT-INFO    OCCURS   50.
      *                        ���ť�����
                               11  TMDCRES-PRSCRPT-CODE    PIC X(9).
      *                        ����̾��
                               11  TMDCRES-PRSCRPT-NAME    PIC X(140).
      *                        ����
                               11  TMDCRES-PRSCRPT-NUMBER  PIC X(11).
      *                        ñ�̥�����
                               11  TMDCRES-PRSCRPT-TANICD  PIC X(03).
      *                        ñ��̾��
                               11  TMDCRES-PRSCRPT-TANICD-NAME
                                                           PIC X(24).
      *                        ������
                               11  TMDCRES-PRSCRPT-MONEY   PIC X(07).
      *                        ��������������
                               11  TMDCRES-PRSCRPT-ATAI-G.
                                   13  TMDCRES-PRSCRPT-ATAI  PIC X(10)
                                                         OCCURS   5.
      *                        �ե����ʬ���
                               11  TMDCRES-PRSCRPT-FILMNUM   PIC X(03).
      *                        ��³�ؼ���ʬ
                               11  TMDCRES-PRSCRPT-CONTKBN   PIC X(01).
      *                        ����������ؼ���ʬ
                               11  TMDCRES-PRSCRPT-NAIFKBN   PIC X(01).
      *