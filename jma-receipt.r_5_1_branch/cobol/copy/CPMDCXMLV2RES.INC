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
      *    ���Уɥ쥹�ݥ����ӽ�λ����(xml2)   COPY (CPMDCXMLV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-MEDICALRES.
            02  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            02  MDCRES-OBJECT              PIC X(36).
            02  MDCRES-MODE                PIC S9(9)   BINARY.
            02  MDCRES-RECORDNAME          PIC X(128).
            02  MDCRES-MEDICALRES.
      *       �»���
              03  MDCRES-INFORMATION-DATE  PIC X(10).
      *       �»ܻ���
              03  MDCRES-INFORMATION-TIME  PIC X(8).
      *       �ֵѥ�����
              03  MDCRES-API-RESULT        PIC X(2).
      *       �ֵѥ�å�����
              03  MDCRES-API-RESULT-MSG    PIC X(100).
      *
              03  MDCRES-RESKEY            PIC X(100).
      *       ��������
              03  MDCRES-PERFORM-DATE      PIC X(10).
              03  MDCRES-PERFORM-TIME      PIC X(8).
      *       UID
              03  MDCRES-MEDICAL-UID       PIC X(36).
      *       ������
              03  MDCRES-ADMISSION-DATE    PIC X(10).
      *
              03  MDCRES-APPOINT-DEP-CODE  PIC X(2).
              03  MDCRES-APPOINT-DEP-NAME  PIC X(80).
              03  MDCRES-APPOINT-DR-CODE   PIC X(5).
              03  MDCRES-APPOINT-DR-NAME   PIC X(80).
              03  MDCRES-PATIENT-INFOMATION.
                04  MDCRES-PATIENTID       PIC X(20).
                04  MDCRES-NAME            PIC X(100).
                04  MDCRES-KANANAME        PIC X(100).
                04  MDCRES-BIRTHDAY        PIC X(10).
                04  MDCRES-SEX             PIC X(1).
      *         �ݸ�����
                04  MDCRES-INSURANCE-G.
      *             �ݸ��ȹ礻�ֹ�
                    06  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *             �ݸ��μ���
                    06  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    06  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    06  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    06  MDCRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    06  MDCRES-MAIN-NUMBER            PIC X(80).
      *             ��³��ʬ
                    06  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    06  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *             �ܿͲ�²��ʬ
                    06  MDCRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    06  MDCRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    06  MDCRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    06  MDCRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    06  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      07  MDCRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      07  MDCRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      07  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      07  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      07  MDCRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      07  MDCRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      07  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      07  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      07  MDCRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      07  MDCRES-INSURANCE-END-DATE      PIC X(10).
      *H26.8
      *       ���ӥǡ�����å�����
              03  MDCRES-MEDICAL-MSG1-G.
      *           ���顼������
                  05  MDCRES-MEDI-RESULT        PIC X(2).
      *           ���顼��å�����
                  05  MDCRES-MEDI-RESULT-MSG    PIC X(100).
      *           ���ӥǡ�����˥󥰥�å�����
                  05  MDCRES-MEDI-WARNING-TBL   OCCURS  50.
      *               ��˥󥰥�����
                      07  MDCRES-MEDI-WARNING        PIC X(03).
      *               ��˥󥰥�å�����
                      07  MDCRES-MEDI-WARNING-MSG    PIC X(100).
      *               ��˥󥰺���
                      07  MDCRES-MEDI-POSITION       PIC X(02).
      *               ��˥󥰹��ֹ�
                      07  MDCRES-MEDI-ITEM-POSITION  PIC X(02).
      *               ��˥󥰥�����
                      07  MDCRES-MEDI-CODE           PIC X(09).
      *      ��̾��å�����
              03  MDCRES-DIAGNOSIS-MSG-G.
      *           ���顼������
                  05  MDCRES-DIAG-RESULT         PIC X(02).
      *           ���顼��å�����
                  05  MDCRES-DIAG-RESULT-MSG     PIC X(100).
      *           ��̾��˥󥰥�å�����
                  05  MDCRES-DIAG-WARNING-TBL    OCCURS  50.
      *               ��˥󥰥�����
                      07  MDCRES-DIAG-WARNING        PIC X(03).
      *               ��˥󥰥�å�����
                      07  MDCRES-DIAG-WARNING-MSG    PIC X(100).
      *               ���ֹ�
                      07  MDCRES-DIAG-ITEM-POSITION  PIC X(02).
      *               ��̾
                      07  MDCRES-DIAG-NAME           PIC X(80).
      *               ��̾������
                      07  MDCRES-DIAG-CODE           PIC X(50).
      *               �ܹ�(�ѻ�)
                      07  MDCRES-DIAG-CHANGE         PIC X(02).
      *!!!