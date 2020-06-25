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
      *    ���Уɥ쥹�ݥ󥹴��Ծ������ (xml2)
      *                                    COPY (CPPATIENTXMLV2RES.INC)
      ******************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.07.00     NACL-¿��Ǽ  14/05/12  ����ݸ��������ɲ�
      * 04.07.00     NACL-¿��Ǽ  14/11/XX  ��󡦺ǽ��象��¾�ɲ�
      * 04.07.00     NACL-¿��Ǽ  15/01/XX  ϫ�Ҽ������Խ��ɲ�
      * 04.08.00     NACL-¿��Ǽ  16/10/XX  ��̳�褫�鵢�����ɲ�
      * 04.08.00     NACL-¿��Ǽ  17/04/XX  ���Ȥ���������ˡ�ɲ���
      *                                     �ݸ��ȹ礻�������
      * 04.08.00     NACL-¿��Ǽ  17/05/XX  �ƥ��ȴ��ԡ���˴��ʬ�ɲ�
      * 05.00.00     NACL-¿��Ǽ  18/10/XX  ���Ը��̾��󤫤�ش������ɲ�
      * 05.00.00     NACL-¿��Ǽ  19/06/XX  �ݸ���ʼ������ʤ��ɲ�
      ************************************************************
        01  XML-PATIENTINFORES.
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
              03  PTRES-API-RESULT         PIC X(2).
      *       �ֵѥ�å�����
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *       ���Ծ���
              03  PTRES-PATIENT-INFOMATION.
      *           �����ֹ�
                  04  PTRES-PATIENTID          PIC X(20).
      *           ������̾
                  04  PTRES-NAME               PIC X(100).
      *           ���ʻ�̾
                  04  PTRES-KANANAME           PIC X(100).
      *           ��ǯ����
                  04  PTRES-BIRTHDAY           PIC X(10).
      *           ����
                  04  PTRES-SEX                PIC X(1).
      *           ���Ӽ�̾
                  04  PTRES-SETAI-NAME         PIC X(100).
      *           ³��
                  04  PTRES-ZOKUGARA           PIC X(60).
      *           ���Լ������
                  04  PTRES-ADDRESS-INFO.
      *               ͹���ֹ�
                      06  PTRES-HOME-ZIP-CODE      PIC X(7).
      *               ����
                      06  PTRES-HOME-ADDRESSES1    PIC X(200).
      *               �����ֹ�
                      06  PTRES-HOME-ADDRESSES2    PIC X(200).
      *               ���������ֹ�����
                      06  PTRES-HOME-HPONE1        PIC X(15).
      *               ���������ֹ�����
                      06  PTRES-HOME-HPONE2        PIC X(15).
      *H28.10
      *           ��̳�����
                  04  PTRES-OFFICE-INFO.
      *               ��̳��̾
                      06  PTRES-OFFICE-NAME        PIC X(50).
      *               ͹���ֹ�
                      06  PTRES-OFFICE-ZIP-CODE    PIC X(7).
      *               ����
                      06  PTRES-OFFICE-ADDRESSES1  PIC X(100).
      *               �����ֹ�
                      06  PTRES-OFFICE-ADDRESSES2  PIC X(100).
      *               �����ֹ�
                      06  PTRES-OFFICE-HPONE        PIC X(15).
      *           Ϣ�������
                  04  PTRES-RENRAKU-INFO.
      *               Ϣ����̾
                      06  PTRES-RENRAKU-NAME        PIC X(50).
      *               Ϣ����³��
                      06  PTRES-RENRAKU-ZOKUGARA    PIC X(30).
      *               ͹���ֹ�
                      06  PTRES-RENRAKU-ZIP-CODE    PIC X(7).
      *               ����
                      06  PTRES-RENRAKU-ADDRESSES1  PIC X(100).
      *               �����ֹ�
                      06  PTRES-RENRAKU-ADDRESSES2  PIC X(100).
      *               �����ֹ桡��
                      06  PTRES-RENRAKU-HPONE1      PIC X(15).
      *               �����ֹ桡��
                      06  PTRES-RENRAKU-HPONE2      PIC X(15).
      *           ���������
                  04  PTRES-KISEI-INFO.
      *               ������̾
                      06  PTRES-KISEI-NAME          PIC X(50).
      *               ͹���ֹ�
                      06  PTRES-KISEI-ZIP-CODE      PIC X(7).
      *               ����
                      06  PTRES-KISEI-ADDRESSES1    PIC X(100).
      *               �����ֹ�
                      06  PTRES-KISEI-ADDRESSES2    PIC X(100).
      *               �����ֹ�
                      06  PTRES-KISEI-HPONE         PIC X(15).
      *
      *           �ش���
                  04  PTRES-TABOO-1              PIC  X(200).
      *           �ش���
                  04  PTRES-TABOO-2              PIC  X(200).
      *           ����륮����
                  04  PTRES-ALLERGY-1            PIC  X(200).
      *           ����륮����
                  04  PTRES-ALLERGY-2            PIC  X(200).
      *           �����ɣ�
                  04  PTRES-KANSENSYO-1          PIC  X(200).
      *           �����ɣ�
                  04  PTRES-KANSENSYO-2          PIC  X(200).
      *           �����ȣ�
                  04  PTRES-COMMENT-1            PIC  X(200).
      *           �����ȣ�
                  04  PTRES-COMMENT-2            PIC  X(200).
      *H29.5
      *           �ƥ��ȴ��Զ�ʬ
                  04  PTMODRES-TEST-PATIENT      PIC  X(01).
      *           ��˴��ʬ
                  04  PTMODRES-DEATHKBN          PIC  X(01).
      *H29.4
      *           ����
                  04  PTRES-JOB                PIC X(20).
      *           �̾�
                  04  PTRES-NICKNAME           PIC  X(50).
      *           ���������ֹ�
                  04  PTRES-KEITAI-TEL         PIC  X(15).
      *           �ƣ����ֹ�
                  04  PTRES-FAX                PIC  X(15).
      *           �Żҥ᡼�륢�ɥ쥹
                  04  PTRES-EMAIL              PIC  X(050).
      *           ���Ȼ�ͳ
                  04  PTRES-DISCOUNTKBN        PIC  X(02).
                  04  PTRES-DISCOUNTKBN-NAME   PIC  X(20).
      *           ���Ψ
                  04  PTRES-DISCOUNT           PIC  X(02).
                  04  PTRES-DISCOUNT-NAME      PIC  X(14).
      *           ���֣����飳
                  04  PTRES-CONDITION-KBN-G.
                      05  PTRES-CONDITION-OCC    OCCURS   3.
                          07  PTRES-CONDITION-KBN     PIC X(02).
                          07  PTRES-CONDITION-NAME    PIC X(20).
      *           ������ˡ��ʬ
                  04  PTRES-IC-CODE           PIC  X(02).
                  04  PTRES-IC-CODE-NAME      PIC  X(20).
      *H25.12
      *           �ϰ�Ϣ��ID
                  04  PTRES-COMMUNITY-CID        PIC  X(20).
      *           Ʊ�եե饰
                  04  PTRES-COMMUNITY-CID-AGREE  PIC  X(05).
      *H26.11
      *           ����象��
                  04  PTRES-FIRST-DATE           PIC  X(10).
      *           �ǽ��象�����ౡ����
                  04  PTRES-LAST-DATE            PIC  X(10).
      *           �������ʬ�ʣ����������
                  04  PTRES-OUTPATIENT-CLASS     PIC  X(01).
      *           ��������������Τߡ�
                  04  PTRES-ADMISSION-DATE       PIC  X(10).
      *           �ౡ���������ౡ���Τߡ�
                  04  PTRES-DISCHARGE-DATE       PIC  X(10).
      *
      *           �ݸ�����
                  04  PTRES-INSURANCE-INFO     OCCURS  30 TIMES.
      *               �ݸ��ȹ礻�ֹ�
                      06  PTRES-COMBINATION-NUMBER     PIC X(04).
      *H26.11
      *               �ݸ��ȹ礻����ô����������
                      06  PTRES-COMBINATION-RATE-ADM   PIC X(04).
      *               �ݸ��ȹ礻����ô���ʳ����
                      06  PTRES-COMBINATION-RATE-OUT   PIC X(04).
      *H29.04
      *               ��ɽ����ʬ
                      06  PTRES-COMBINATION-HYOJI      PIC X(01).
      *
      *               �ݸ��μ���
                      06  PTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               �ݸ����ֹ�
                      06  PTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               �ݸ��μ���̾��
                      06  PTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               ����
                      06  PTRES-MAIN-MARK              PIC X(80).
      *               �ֹ�
                      06  PTRES-MAIN-NUMBER            PIC X(80).
      *               ����
                      06  PTRES-MAIN-EDABAN            PIC X(02).
      *               ��³��ʬ
                      06  PTRES-MAIN-CONTINUATION      PIC X(01).
      *               �����ʬ
                      06  PTRES-MAIN-ASSISTANCE        PIC X(01).
      *               �����ʬ̾��
                      06  PTRES-MAIN-ASSISTANCE-NAME   PIC X(20).
      *               �ܿͲ�²��ʬ
                      06  PTRES-MAIN-FAMILY            PIC X(01).
      *               ���ݸ���̾
                      06  PTRES-MAIN-POLICYHOLDER      PIC X(100).
      *               ͭ��������
                      06  PTRES-MAIN-START-DATE        PIC X(10).
      *               ͭ����λ��
                      06  PTRES-MAIN-END-DATE          PIC X(10).
      *R01.06
      *               ��ʼ�����
                      06  PTRES-MAIN-GET-DATE          PIC X(10).
      *H26.11
      *               �ǽ���ǧ��
                      06  PTRES-MAIN-CHECK-DATE        PIC X(10).
      *
      *               �������
                      06  PTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 ����μ���
                        07  PTRES-INSURANCE-CLASS         PIC X(03).
      *                 ����μ���̾��
                        07  PTRES-INSURANCE-NAME          PIC X(20).
      *                 ��ô���ֹ�
                        07  PTRES-PROVIDER-NUMBER         PIC X(08).
      *                 ������ֹ�
                        07  PTRES-RECIPIENT-NUMBER        PIC X(20).
      *                 ��������ôΨ�ʳ��
                        07  PTRES-RATE-ADMISSION          PIC X(04).
      *                 �����ݸ����
                        07  PTRES-MONEY-ADMISSION         PIC X(06).
      *                 �������ôΨ�ʳ��
                        07  PTRES-RATE-OUTPATIENT         PIC X(04).
      *                 ����ݸ����
                        07  PTRES-MONEY-OUTPATIENT        PIC X(06).
      *                 ͭ��������
                        07  PTRES-INSURANCE-START-DATE    PIC X(10).
      *                 ͭ����λ��
                        07  PTRES-INSURANCE-END-DATE      PIC X(10).
      *R01.06
      *                 �ǽ���ǧ��
                        07  PTRES-INSURANCE-CHECK-DATE    PIC X(10).
      *H27.1
      *ϫ�Ҿ���
                     06  PTRES-ACCIDENT-INF-G.
      *                  ϫ���ݸ�̾�Ρʵ����Ʊ�����ơ�
                         07  PTRES-ACCIDENT-NAME           PIC X(50).
      *                  ���¤����̡��ֹ��Ʊ�����ơ�
                         07  PTRES-DISEASE-LOCATION        PIC X(100).
      *                  ����ǯ����
                         07  PTRES-DISEASE-DATE            PIC X(10).
      *ϫ��
      *                  ϫƯ�ݸ��ֹ�
                         07  PTRES-ACCIDENT-NUMBER         PIC X(14).
      *                  ǯ��ڽ��ֹ�
                         07  PTRES-PENSIONCERTIFICATE      PIC X(09).
      *                  �ҳ���ʬ��̾�Ρ�
                         07  PTRES-ACCIDENT-CLASS          PIC X(20).
      *                  ϫƯ�����Ľ𥳡���
                         07  PTRES-LABOR-ST-CODE           PIC X(05).
      *                  ϫƯ�����Ľ𥳡���
                         07  PTRES-LABOR-ST-NAME           PIC X(50).
      *                  ���Ƚ�̾��
                         07  PTRES-RSI-JIGYOUNAME          PIC X(80).
      *                  �������ƻ�ܸ�̾
                         07  PTRES-RSI-PREFNAME            PIC X(20).
      *                  �������ƻ�ܸ���ʬ
                         07  PTRES-RSI-PREFKBN             PIC X(01).
      *                  �������ƻ�ܸ���ʬ̾��
                         07  PTRES-RSI-PREFKBNNAME         PIC X(02).
      *                  ����Ϸ��Զ�̾
                         07  PTRES-RSI-CITYNAME            PIC X(80).
      *                  ����Ϸ��Զ��ʬ
                         07  PTRES-RSI-CITYKBN             PIC X(01).
      *                  ����Ϸ��Զ�̾��
                         07  PTRES-RSI-CITYKBNNAME         PIC X(02).
      *������
      *                  �������ݸ����̾
                         07  PTRES-RSI-HOKENNAME           PIC X(80).
      *���ե�������
      *                  �򹯴�����Ģ�ֹ�
                         07  PTRES-RSI-KANRITCHO-NUMBER    PIC X(13).
      *                  »����ʬ
                         07  PTRES-DAMAGE-CLASS            PIC X(03).
      *                  »����ʬ̾��
                         07  PTRES-DAMAGE-CLASS-NAME       PIC X(100).
      *
      *H26.5
      *           ������
                  04  PTRES-CARE-INF-G.
      *               ����ݸ�����
                      05  PTRES-CARE-INSURANCE-G    OCCURS   10.
      *                   �ݸ����ֹ�
                          07  PTRES-CARE-INSURANCE-NUMBER  PIC X(06).
      *                   ���ݸ����ֹ�
                          07  PTRES-CARE-POLICYHOLDER      PIC X(10).
      *                   ͭ��������
                          07  PTRES-CARE-START-DATE        PIC X(10).
      *                   ͭ����λ��
                          07  PTRES-CARE-END-DATE          PIC X(10).
      *               �ײ��ǧ�����
                      05  PTRES-CERTIFICATION-G    OCCURS   50.
      *                   �ײ����ֶ�ʬ
                          07  PTRES-NEED-CARE-STATE-CODE   PIC X(02).
      *                   �ײ�����̾��
                          07  PTRES-NEED-CARE-STATE        PIC X(20).
      *                   ǧ������
                          07  PTRES-NEED-DATE              PIC X(10).
      *                   ͭ��������
                          07  PTRES-NEED-START-DATE        PIC X(10).
      *                   ͭ����λ��
                          07  PTRES-NEED-END-DATE          PIC X(10).
      *           �ϰ��������оݼ���
                  04  PTRES-COMMUNITY-DISEASE.
                      05  PTRES-TARGET-DISEASE     PIC X(05)
                                                   OCCURS   4.
      *H30.4
      *           ���Ը��̾���
                  04  PTRES-PTCONF-INFO.
      *               ǥ������
                      05  PTRES-NINPUKBN             PIC X(05).
      *H30.10
      *               ǧ�ξ��ϰ�������
                      05  PTRES-NINTIHOKATU          PIC X(05).
      *               ����������Ĥ�����
                      05  PTRES-SYOUNIKAKARI         PIC X(05).
      *
      *           ���ԸĿ��ֹ����
                  04  PTRES-PTMYNUMBER-G.
                      05  PTRES-PTMYNUMBER-TBL     OCCURS   20.
                          07  PTRES-ID-KEY           PIC X(10).
                          07  PTRES-MY-NUMBER        PIC X(20).
                          07  PTRES-DESCRIPTION      PIC X(100).
      *H30.10
      *
      *           ����������ư����
                  04  PTRES-AUTO-SRYCD-G.
                      05  PTRES-AUTO-SRYCD-TBL     OCCURS   3.
                          07  PTRES-AUTO-SRYCD       PIC X(09).
                          07  PTRES-AUTO-NAME        PIC X(80).
                          07  PTRES-AUTO-ENDYMD      PIC X(10).
      *
      *           ���Զش�����
                  04  PTRES-KINKI-YKZAI-G.
                      05  PTRES-KINKI-YKZAI-TBL    OCCURS   100.
      *                   �ش����ޥ�����
                          07  PTRES-DRUGCD-CODE       PIC X(09).
                          07  PTRES-DRUGCD-NAME       PIC X(80).
      *                   ͭ����λ���ʥޥ�����λ����
                          07  PTRES-DRUGCD-END-DATE   PIC X(10).
      *                   �ش�������
                          07  PTRES-START-DATE        PIC X(10).
      *