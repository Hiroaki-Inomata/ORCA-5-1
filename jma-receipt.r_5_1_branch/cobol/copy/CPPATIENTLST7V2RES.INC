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
      *    ���Уɥ쥹�ݥ󥹥�����Ƽ���(xml2)
      *                                   COPY (CPPATIENTLST7V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST7RES.
            02  PTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PTRES-OBJECT           PIC X(36).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-RECORDNAME       PIC X(128).
            02  PTRES-PATIENTINFORES.
      *       �»���
              03  PTRES-INFORMATION-DATE   PIC X(10).
      *       �»ܻ���
              03  PTRES-INFORMATION-TIME   PIC X(8).
      *       �ֵѥ�����
              03  PTRES-API-RESULT         PIC X(3).
      *       �ֵѥ�å�����
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *
              03  PTRES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  PTRES-PATIENTID          PIC X(20).
      *           ������̾
                  05  PTRES-NAME               PIC X(100).
      *           ���ʻ�̾
                  05  PTRES-KANANAME           PIC X(100).
      *           ��ǯ����
                  05  PTRES-BIRTHDAY           PIC X(10).
      *           ����
                  05  PTRES-SEX                PIC X(1).
      *        �������
               03  PTRES-BASE-DATE        PIC X(10).
      *        ������
               03  PTRES-MEMOINF-INFOMATION.
      *            ������
                   05  PTRES-MEMO-INFO     OCCURS  10 TIMES.
      *                ���Ų�
                       07  PTRES-SRYKA              PIC X(02).
      *                ���Ų�̾��
                       07  PTRES-SRYKA-MEI          PIC X(40).
      *                �������
                       07  PTRES-MEMO               PIC X(1000).
      *                ���վ���
                       07  PTRES-UKETUKE-INFO.
      *                    ���տ��Ųʡʿ��Ųʡᣰ���Τߡ�
                           09  PTRES-UKE-SRYKA          PIC X(02).
                           09  PTRES-UKE-SRYKA-MEI      PIC X(40).
      *                    �ݸ�����
                           09  PTRES-INSURANCE-INFO.
      *                        �ݸ��ȹ礻�ֹ�
                               11  PTRES-COMBINATION-NUMBER  PIC X(04).
      *                        ��ɽ����ʬ
                               11  PTRES-COMBINATION-HYOJI   PIC X(01).
      *                        �ݸ��μ���
                               11  PTRES-MAIN-INSURANCE-CLASS
                                                             PIC X(03).
      *                        �ݸ����ֹ�
                               11  PTRES-MAIN-INSURANCE-NUMBER
                                                             PIC X(08).
      *                        �ݸ��μ���̾��
                               11  PTRES-MAIN-INSURANCE-NAME
                                                             PIC X(20).
      *                        ����
                               11  PTRES-MAIN-MARK           PIC X(80).
      *                        �ֹ�
                               11  PTRES-MAIN-NUMBER         PIC X(80).
      *                        ��³��ʬ
                               11  PTRES-MAIN-CONTINUATION   PIC X(01).
      *                        �����ʬ
                               11  PTRES-MAIN-ASSISTANCE     PIC X(01).
      *                        �ܿͲ�²��ʬ
                               11  PTRES-MAIN-FAMILY         PIC X(01).
      *                        ���ݸ���̾
                               11  PTRES-MAIN-POLICYHOLDER   PIC X(100).
      *                        ͭ��������
                               11  PTRES-MAIN-START-DATE     PIC X(10).
      *                        ͭ����λ��
                               11  PTRES-MAIN-END-DATE       PIC X(10).
      *                        �������
                               11  PTRES-PUBLIC-INSURANCE-INFO
                                                  OCCURS  4 TIMES.
      *                            ����μ���
                                   13  PTRES-INSURANCE-CLASS
                                                             PIC X(03).
      *                            ����μ���̾��
                                   13  PTRES-INSURANCE-NAME  PIC X(20).
      *                            ��ô���ֹ�
                                   13  PTRES-PROVIDER-NUMBER
                                                             PIC X(08).
      *                            ������ֹ�
                                   13  PTRES-RECIPIENT-NUMBER
                                                             PIC X(20).
      *                            ͭ��������
                                   13  PTRES-INSURANCE-START-DATE
                                                             PIC X(10).
      *                            ͭ����λ��
                                   13  PTRES-INSURANCE-END-DATE
                                                             PIC X(10).
      *