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
      *    ���Уɥ쥹�ݥ󥹴��Ծ����������(xml2)
      *                                   COPY (CPPATIENTLST2V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST2RES.
            02  PATLSTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PATLSTRES-OBJECT           PIC X(36).
            02  PATLSTRES-MODE             PIC S9(9)   BINARY.
            02  PATLSTRES-RECORDNAME       PIC X(128).
            02  PATLSTRES-PATIENTINFORES.
      *       �»���
              03  PATLSTRES-INFORMATION-DATE   PIC X(10).
      *       �»ܻ���
              03  PATLSTRES-INFORMATION-TIME   PIC X(8).
      *       �ֵѥ�����
              03  PATLSTRES-API-RESULT         PIC X(2).
      *       �ֵѥ�å�����
              03  PATLSTRES-API-RESULT-MSG     PIC X(100).
              03  PATLSTRES-RESKEY             PIC X(100).
      *       �оݴ��Է��
              03  PATLSTRES-PATIENTID-CNT      PIC X(03).
      *       ���顼���Է��
              03  PATLSTRES-PATIENTID-ERR      PIC X(03).
      *       ���Ծ���
              03  PATLSTRES-PATIENT-INFOMATION    OCCURS   100.
      *           �����ֹ�
                  04  PATLSTRES-PATIENTID          PIC X(20).
      *           ������̾
                  04  PATLSTRES-NAME               PIC X(100).
      *           ���ʻ�̾
                  04  PATLSTRES-KANANAME           PIC X(100).
      *           ��ǯ����
                  04  PATLSTRES-BIRTHDAY           PIC X(10).
      *           ����
                  04  PATLSTRES-SEX                PIC X(1).
      *           ���Լ������
                  04  PATLSTRES-ADDRESS-INFO.
      *               ͹���ֹ�
                      06  PATLSTRES-HOME-ZIP-CODE      PIC X(7).
      *               ����
                      06  PATLSTRES-HOME-ADDRESSES1    PIC X(200).
      *               �����ֹ�
                      06  PATLSTRES-HOME-ADDRESSES2    PIC X(200).
      *               ���������ֹ�����
                      06  PATLSTRES-HOME-HPONE1        PIC X(15).
      *               ���������ֹ�����
                      06  PATLSTRES-HOME-HPONE2        PIC X(15).
      *           ������ʬ�ʣ����������
                  04  PATLSTRES-OUTPATIENT-CLASS       PIC X(01).
      *           �ݸ�����
                  04  PATLSTRES-INSURANCE-INFO     OCCURS  3 TIMES.
      *               �ݸ��μ���
                      06  PATLSTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ݸ��μ���̾��
                      06  PATLSTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               �ݸ����ֹ�
                      06  PATLSTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               ����
                      06  PATLSTRES-MAIN-MARK              PIC X(80).
      *               �ֹ�
                      06  PATLSTRES-MAIN-NUMBER            PIC X(80).
      *               ��³��ʬ
                      06  PATLSTRES-MAIN-CONTINUATION      PIC X(01).
      *               �����ʬ
                      06  PATLSTRES-MAIN-ASSISTANCE        PIC X(01).
      *               �ܿͲ�²��ʬ
                      06  PATLSTRES-MAIN-FAMILY            PIC X(01).
      *               ���ݸ���̾
                      06  PATLSTRES-MAIN-POLICYHOLDER      PIC X(100).
      *               ͭ��������
                      06  PATLSTRES-MAIN-START-DATE        PIC X(10).
      *               ͭ����λ��
                      06  PATLSTRES-MAIN-END-DATE          PIC X(10).
      *            �������
                   04  PATLSTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      06  PATLSTRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      06  PATLSTRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      06  PATLSTRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      06  PATLSTRES-RECIPIENT-NUMBER        PIC X(20).
      *               ͭ��������
                      06  PATLSTRES-INSURANCE-START-DATE    PIC X(10).
      *               ͭ����λ��
                      06  PATLSTRES-INSURANCE-END-DATE      PIC X(10).
      *