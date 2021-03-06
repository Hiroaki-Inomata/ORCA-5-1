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
      ******************************************************************
      *    ���Уɥ쥹�ݥ�ͽ�����           COPY (CPACCPTXMLV2RES.INC)
      ******************************************************************
        01  XML-APPOINTRES.
            02  APNTRES-CONTEXT            PIC S9(9)   BINARY.
            02  APNTRES-OBJECT             PIC X(36).
            02  APNTRES-MODE               PIC S9(9)   BINARY.
            02  APNTRES-RECORDNAME         PIC X(128).
            02  APNTRES-APPOINTRES.
      *       �»���
              03  APNTRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  APNTRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  APNTRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  APNTRES-API-RESULT-MSG      PIC X(100).
      *       ��˥󥰥�å�����
              03  APNTRES-API-WARNING-MSG-G.
                  05  APNTRES-API-WARNING-MSG-TBL     OCCURS   5.
                      07  APNTRES-API-WARNING-MSG     PIC X(100).
      *
              03  APNTRES-RESKEY              PIC X(100).
      *       ͽ����
              03  APNTRES-APPOINT-DATE        PIC X(10).
      *       ͽ�����
              03  APNTRES-APPOINT-TIME        PIC X(8).
      *       ͽ��ɣ�
              03  APNTRES-APPOINT-ID          PIC X(5).
      *       ���Ų�
              03  APNTRES-APPOINT-DEP-CODE    PIC X(2).
      *       ���Ų�̾��
              03  APNTRES-APPOINT-DEP-NAME    PIC X(80).
      *       �ɥ�����������
              03  APNTRES-APPOINT-DR-CODE     PIC X(5).
      *       �ɥ�������̾
              03  APNTRES-APPOINT-DR-NAME     PIC X(80).
      *       �������ƶ�ʬ
              03  APNTRES-APPOINT-DEP-INFO    PIC X(2).
      *       ͽ���ʬ
              03  APNTRES-APPOINT-INFO        PIC X(2).
      *       ͽ����
              03  APNTRES-APPOINT-MEMO        PIC X(100).
      *       �������ƶ�ʬ
              03  APNTRES-PATIENT-INFOMATION.
      *         �����ֹ�
                04  APNTRES-PATIENTID         PIC X(20).
      *         ���Ի�̾
                04  APNTRES-NAME              PIC X(100).
      *         ���ԥ��ʻ�̾
                04  APNTRES-KANANAME          PIC X(100).
      *         ������
                04  APNTRES-BIRTHDAY          PIC X(10).
      *         ����
                04  APNTRES-SEX               PIC X(1).
      *         �������
                04  APNTRES-ADDRESS-INFO.
      *           ͹���ֹ�
                  05  APNTRES-HOME-ZIP-CODE   PIC X(7).
      *           ����
                  05  APNTRES-HOME-ADDRESSES  PIC X(100).
      *         �ݸ�����
                04  APNTRES-INSURANCE-G.
                  05  APNTRES-INSURANCE-INFO          OCCURS 20 TIMES.
      *             �ݸ��μ���
                    06  APNTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ����ֹ�
                    06  APNTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             �ݸ��μ���̾��
                    06  APNTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             ����
                    06  APNTRES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    06  APNTRES-MAIN-NUMBER            PIC X(80).
      *             ��³��ʬ
                    06  APNTRES-MAIN-CONTINUATION      PIC X(01).
      *             �����ʬ
                    06  APNTRES-MAIN-ASSISTANCE        PIC X(01).
      *             �ܿͲ�²��ʬ
                    06  APNTRES-MAIN-FAMILY            PIC X(01).
      *             ���ݸ���̾
                    06  APNTRES-MAIN-POLICYHOLDER      PIC X(100).
      *             ͭ��������
                    06  APNTRES-MAIN-START-DATE        PIC X(10).
      *             ͭ����λ��
                    06  APNTRES-MAIN-END-DATE          PIC X(10).
      *             �������
                    06  APNTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      07  APNTRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      07  APNTRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      07  APNTRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      07  APNTRES-RECIPIENT-NUMBER        PIC X(20).
      *               ��������ôΨ�ʳ��
                      07  APNTRES-RATE-ADMISSION          PIC X(04).
      *               �����ݸ����
                      07  APNTRES-MONEY-ADMISSION         PIC X(06).
      *               �������ôΨ�ʳ��
                      07  APNTRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      07  APNTRES-MONEY-OUTPATIENT        PIC X(06).
      *               ͭ��������
                      07  APNTRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      07  APNTRES-INSURANCE-END-DATE      PIC X(10).
      *
