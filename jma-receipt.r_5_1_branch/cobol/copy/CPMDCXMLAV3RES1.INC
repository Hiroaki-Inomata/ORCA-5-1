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
      *    ���Уɥ쥹�ݥ��������Ź԰����Ͼ��� (���β�) COPY (CPMDCXMLAV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-MEDICALAV3RES1.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *       �ꥯ�������ֹ�
              05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *       �쥹�ݥ��ֹ�
              05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *       UID���Żҥ���ơ�
              05  MDCRES-KARTE-UID         PIC X(36).
      *       UID�ʥ��륫��
              05  MDCRES-ORCA-UID          PIC X(36).
      *       �»���
              05  MDCRES-INFORMATION-DATE  PIC X(10).
      *       �»ܻ���
              05  MDCRES-INFORMATION-TIME  PIC X(8).
      *       �ֵѥ�����
              05  MDCRES-API-RESULT        PIC X(03).
      *       �ֵѥ�å�����
              05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
              05  MDCRES-RESKEY            PIC X(100).
      *       ��������
              05  MDCRES-PERFORM-DATE      PIC X(10).
              05  MDCRES-PERFORM-TIME      PIC X(8).
      *       ���ӥǡ���¸�߶�ʬ
              05  MDCRES-TEMP-MEDICAL-FLAG PIC X(05).
      *       ��ǻ������ʤ���ʬ
              05  MDCRES-DOCTORS-FEE-INITIAL   PIC X(05).
      *       ������
              05  MDCRES-NYUIN-DATE        PIC  X(10).
      *       �ౡ��
              05  MDCRES-TAIIN-DATE        PIC  X(10).
      *       Ʊ��������ʬ��Ϣ�֡�
              05  MDCRES-DOUJITSUKBN       PIC X(1).
      *       ���Ų�
              05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
              05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      ***     �ɥ���������������Υɥ����������ܡ�
      ***     05  MDCRES-APPOINT-DR-CODE   PIC X(5).
      ***     05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *
              05  MDCRES-PATIENT-INFOMATION.
      *         ���Ծ���
                07  MDCRES-PATIENTID       PIC X(20).
                07  MDCRES-NAME            PIC X(100).
                07  MDCRES-KANANAME        PIC X(100).
                07  MDCRES-BIRTHDAY        PIC X(10).
                07  MDCRES-SEX             PIC X(1).
      *         �ݸ�����
                07  MDCRES-INSURANCE-G.
      *             �ݸ��ȹ礻�ֹ�
                    09  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *             �ݸ��μ���
                    09  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    09  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    09  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    09  MDCRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    09  MDCRES-MAIN-NUMBER            PIC X(80).
      *             ��³��ʬ
                    09  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    09  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *             �����ʬ
                    09  MDCRES-MAIN-ASSI-NAME         PIC X(20).
      *             �ܿͲ�²��ʬ
                    09  MDCRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    09  MDCRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    09  MDCRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    09  MDCRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    09  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      11  MDCRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      11  MDCRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      11  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      11  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      11  MDCRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      11  MDCRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      11  MDCRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      11  MDCRES-INSURANCE-END-DATE      PIC X(10).
      *
              05  MDCRES-MEDICAL-MSG1-G.
      *           ���顼������
                  07  MDCRES-MEDI-RESULT        PIC X(3).
      *           ���顼��å�����
                  07  MDCRES-MEDI-RESULT-MSG    PIC X(100).
      *           ��˥󥰥�å�����
                  07  MDCRES-MEDI-WARNING-TBL   OCCURS  50.
      *               ��˥󥰥�����
                      09  MDCRES-MEDI-WARNING        PIC X(03).
      *               ��˥󥰥�å�����
                      09  MDCRES-MEDI-WARNING-MSG    PIC X(100).
      *               ��˥󥰥�����
                      09  MDCRES-MEDI-CODE           PIC X(09).
      *