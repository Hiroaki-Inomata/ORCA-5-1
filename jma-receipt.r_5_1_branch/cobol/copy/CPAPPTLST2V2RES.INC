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
      *    ���Уɥ쥹�ݥ󥹴���ͽ����� (xml2)
      *                                    COPY (CPACCPTLST2V2RES.INC)
      ******************************************************************
        01  XML-APPOINTLST2RES.
            02  APLST2RES-CONTEXT           PIC S9(9)   BINARY.
            02  APLST2RES-OBJECT            PIC X(36).
            02  APLST2RES-MODE              PIC S9(9)   BINARY.
            02  APLST2RES-RECORDNAME        PIC X(128).
            02  APLST2RES-APPOINTRES.
      *       �»���
              03  APLST2RES-INFORMATION-DATE  PIC X(10).
      *       �»ܻ���
              03  APLST2RES-INFORMATION-TIME  PIC X(8).
      *       �ֵѥ�����
              03  APLST2RES-API-RESULT        PIC X(2).
      *       �ֵѥ�å�����
              03  APLST2RES-API-RESULT-MSG    PIC X(100).
      *
              03  APLST2RES-RESKEY            PIC X(100).
      *
      *       �����
              03  APLST2RES-BASE-DATE         PIC X(10).
      *       ���Ծ���
              03  APLST2RES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  APLST2RES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  05  APLST2RES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  05  APLST2RES-KANANAME          PIC X(100).
      *           ������
                  05  APLST2RES-BIRTHDAY          PIC X(10).
      *           ����
                  05  APLST2RES-SEX               PIC X(1).
      *       ͽ������
              03  APLST2RES-APPOINT-LST-G.
                  05  APLST2RES-APPOINT-LST     OCCURS   50.
      *               ͽ����
                      07  APLST2RES-APPOINT-DATE        PIC X(10).
      *               ͽ�����
                      07  APLST2RES-APPOINT-TIME        PIC X(8).
      *               �������ƶ�ʬ
                      07  APLST2RES-APPOINT-MED-INFO    PIC X(2).
      *               ��������̾��
                      07  APLST2RES-APPOINT-MED-NAME    PIC X(80).
      *               ���Ų�
                      07  APLST2RES-APPOINT-DEP-CODE    PIC X(2).
      *               ���Ų�̾��
                      07  APLST2RES-APPOINT-DEP-NAME    PIC X(80).
      *               �ɥ�����������
                      07  APLST2RES-APPOINT-DR-CODE     PIC X(5).
      *               �ɥ�������̾
                      07  APLST2RES-APPOINT-DR-NAME     PIC X(80).
      *               �象�Ѷ�ʬ
                      07  APLST2RES-VISIT-INFO          PIC X(1).
      *H29.11         ͽ��ID
                      07  APLST2RES-APPOINT-ID          PIC X(2).
      *               ͽ����
                      07  APLST2RES-APPOINT-MEMO        PIC X(100).
      *
