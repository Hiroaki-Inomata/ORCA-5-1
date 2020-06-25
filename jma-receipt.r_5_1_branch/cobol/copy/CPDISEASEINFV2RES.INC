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
      *    ���Уɥ쥹�ݥ󥹴�����̾����(xml2)
      *                                     COPY (CPDISEASEINFV2REQ.INC)
      ******************************************************************
        01  XML-DISEASE-INFRES.
            02  DISERES-CONTEXT            PIC S9(9)   BINARY.
            02  DISERES-OBJECT             PIC X(36).
            02  DISERES-MODE               PIC S9(9)   BINARY.
            02  DISERES-RECORDNAME         PIC X(128).
            02  DISERES-DISEASE-INFRES.
      *       �»���
              03  DISERES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  DISERES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  DISERES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  DISERES-API-RESULT-MSG      PIC X(100).
      *
              03  DISERES-RESKEY              PIC X(100).
      *       ��̾���󥪡��С��ե饰
              03  DISERES-INFORMATION-OVERFLOW
                                              PIC X(5).
      *       ���Ծ���
              03  DISERES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  DISERES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  05  DISERES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  05  DISERES-KANANAME          PIC X(100).
      *           ������
                  05  DISERES-BIRTHDAY          PIC X(10).
      *           ����
                  05  DISERES-SEX               PIC X(1).
      *       ���ǯ��
              03  DISERES-BASE-DATE        PIC X(07).
      *       ��̾����
              03  DISERES-DISEASE-INF-G    OCCURS   200.
      *            ������ʬ
                   05  DISERES-NYUGAIKBN       PIC X(1).
      *            ���Ų�
                   05  DISERES-SRYKA           PIC X(02).
      *            �ݸ��ȹ礻�ֹ�
                   05  DISERES-HKNCOMBI        PIC X(04).
      *            �軰�Թ԰٥ե饰���軰�Թ԰٤��ݸ��ȹ礻�ΤȤ���
                   05  DISERES-THIRD-PARTY-MARK
                                               PIC X(04).
      *            ��̾
                   05  DISERES-DISEASE-NAME    PIC X(160).
      *            ��̾������
                   05  DISERES-DISEASE-SINGLE-G.
                       07  DISERES-DISEASE-SINGLE  OCCURS   21.
      *                    ��̾������
                           09  DISERES-SINGLE-CODE     PIC X(07).
      *                    ��̾
                           09  DISERES-SINGLE-NAME     PIC X(160).
      *                    ���֥�����
                           09  DISERES-SINGLE-FLG      PIC X(01).
      *            ����̾�ե饰
                   05  DISERES-CATEGORY            PIC X(02).
      *            �����ե饰
                   05  DISERES-SUSPECTEDFLAG       PIC X(02).
      *            ���ų�����
                   05  DISERES-START-DATE          PIC X(10).
      *            ž����
                   05  DISERES-END-DATE            PIC X(10).
      *            ž����ʬ
                   05  DISERES-OUTCOME             PIC X(02).
      *            ��­������
                   05  DISERES-SUPPLEMENT-NAME     PIC X(80).
      *            ��­�����ȥ�����
                   05  DISERES-SUPPLEMENT-SINGLE-G.
                       07  DISERES-SUPPLEMENT-SINGLE   OCCURS   3.
      *                    ��­�����ȥ�����
                           09  DISERES-SUPPLEMENT-SINGLE-CODE
                                                   PIC X(20).
      *                    ��­������
                           09  DISERES-SUPPLEMENT-SINGLE-NAME
                                                   PIC X(80).
      *            �������̾
                   05  DISERES-KARTE-NAME          PIC X(160).
      *            ������ʬ
                   05  DISERES-DISEASE-CLASS       PIC X(02).
      *            �쥻�ץ�ɽ���ե饰
                   05  DISERES-RECEIPT-PRINT       PIC X(01).
      *            �쥻�ץ�ɽ�����
                   05  DISERES-RECEIPT-PERIOD      PIC X(02).
      *            �ݸ���̾�ե饰
                   05  DISERES-INSURANCE-DISEASE   PIC X(05).
      *            ��������ʬ
                   05  DISERES-CLASSIFICATION-NUMBER-MASTER
                                                   PIC X(02).
      *            ��ʻ�ɶ�ʬ
                   05  DISERES-CLASSIFICATION-NUMBER-SERVANT
                                                   PIC X(02).
      *            �ౡ������
                   05  DISERES-DISCHARGEFLG        PIC X(1).