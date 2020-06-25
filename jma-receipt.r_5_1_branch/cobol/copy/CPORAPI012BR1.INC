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
      *    ���Уɴ��Ծ�������� (xml2)
      *                                    COPY (CPORAPI012BR1.INC)
      ******************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTINFLST-REC.
      *    ���Ծ���
           03  PTINFLST-PATIENT-INFOMATION.
      *        �����ֹ�
               05  PTINFLST-PATIENTID          PIC X(20).
      *        ������̾
               05  PTINFLST-NAME               PIC X(100).
      *        ���ʻ�̾
               05  PTINFLST-KANANAME           PIC X(100).
      *        ��ǯ����
               05  PTINFLST-BIRTHDAY           PIC X(10).
      *        ����
               05  PTINFLST-SEX                PIC X(1).
      *        ���Ӽ�̾
               05  PTINFLST-SETAI-NAME         PIC X(100).
      *        ³��
               05  PTINFLST-ZOKUGARA           PIC X(60).
      *        ���Լ������
               05  PTINFLST-ADDRESS-INFO.
      *            ͹���ֹ�
                   07  PTINFLST-HOME-ZIP-CODE      PIC X(7).
      *            ����
                   07  PTINFLST-HOME-ADDRESSES1    PIC X(200).
      *            �����ֹ�
                   07  PTINFLST-HOME-ADDRESSES2    PIC X(200).
      *            ���������ֹ�����
                   07  PTINFLST-HOME-HPONE1        PIC X(15).
      *            ���������ֹ�����
                   07  PTINFLST-HOME-HPONE2        PIC X(15).
      *
      *        ��̳�����
               05  PTINFLST-OFFICE-INFO.
      *            ��̳��̾
                   07  PTINFLST-OFFICE-NAME        PIC X(50).
      *            ͹���ֹ�
                   07  PTINFLST-OFFICE-ZIP-CODE    PIC X(7).
      *            ����
                   07  PTINFLST-OFFICE-ADDRESSES1  PIC X(100).
      *            �����ֹ�
                   07  PTINFLST-OFFICE-ADDRESSES2  PIC X(100).
      *            �����ֹ�
                   07  PTINFLST-OFFICE-HPONE       PIC X(15).
      *        Ϣ�������
               05  PTINFLST-RENRAKU-INFO.
      *            Ϣ����̾
                   07  PTINFLST-RENRAKU-NAME        PIC X(50).
      *            Ϣ����³��
                   07  PTINFLST-RENRAKU-ZOKUGARA    PIC X(30).
      *            ͹���ֹ�
                   07  PTINFLST-RENRAKU-ZIP-CODE    PIC X(7).
      *            ����
                   07  PTINFLST-RENRAKU-ADDRESSES1  PIC X(100).
      *            �����ֹ�
                   07  PTINFLST-RENRAKU-ADDRESSES2  PIC X(100).
      *            �����ֹ桡��
                   07  PTINFLST-RENRAKU-HPONE1      PIC X(15).
      *            �����ֹ桡��
                   07  PTINFLST-RENRAKU-HPONE2      PIC X(15).
      *        ���������
               05  PTINFLST-KISEI-INFO.
      *            ������̾
                   07  PTINFLST-KISEI-NAME          PIC X(50).
      *            ͹���ֹ�
                   07  PTINFLST-KISEI-ZIP-CODE      PIC X(7).
      *            ����
                   07  PTINFLST-KISEI-ADDRESSES1    PIC X(100).
      *            �����ֹ�
                   07  PTINFLST-KISEI-ADDRESSES2    PIC X(100).
      *            �����ֹ�
                   07  PTINFLST-KISEI-HPONE         PIC X(15).
      *
      *        �ش���
               05  PTINFLST-TABOO-1            PIC  X(100).
      *        �ش���
               05  PTINFLST-TABOO-2            PIC  X(100).
      *        ����륮����
               05  PTINFLST-ALLERGY-1          PIC  X(100).
      *        ����륮����
               05  PTINFLST-ALLERGY-2          PIC  X(100).
      *        �����ɣ�
               05  PTINFLST-KANSENSYO-1        PIC  X(100).
      *        �����ɣ�
               05  PTINFLST-KANSENSYO-2        PIC  X(100).
      *        �����ȣ�
               05  PTINFLST-COMMENT-1          PIC  X(100).
      *        �����ȣ�
               05  PTINFLST-COMMENT-2          PIC  X(100).
      *        �ƥ��ȴ��Զ�ʬ
               05  PTMODRES-TEST-PATIENT       PIC  X(01).
      *        ��˴��ʬ
               05  PTMODRES-DEATHKBN           PIC  X(01).
      *        ����
               05  PTINFLST-JOB                PIC X(20).
      *        �̾�
               05  PTINFLST-NICKNAME           PIC  X(50).
      *        ���������ֹ�
               05  PTINFLST-KEITAI-TEL         PIC  X(15).
      *        �ƣ����ֹ�
               05  PTINFLST-FAX                PIC  X(15).
      *        �Żҥ᡼�륢�ɥ쥹
               05  PTINFLST-EMAIL              PIC  X(50).
      *        ���Ȼ�ͳ
               05  PTINFLST-DISCOUNTKBN        PIC  X(02).
               05  PTINFLST-DISCOUNTKBN-NAME   PIC  X(20).
      *        ���Ψ
               05  PTINFLST-DISCOUNT           PIC  X(02).
               05  PTINFLST-DISCOUNT-NAME      PIC  X(14).
      *        ���֣����飳
               05  PTINFLST-CONDITION-KBN-G.
                   07  PTINFLST-CONDITION-OCC    OCCURS   3.
                       09  PTINFLST-CONDITION-KBN     PIC X(02).
                       09  PTINFLST-CONDITION-NAME    PIC X(20).
      *        ������ˡ��ʬ
               05  PTINFLST-IC-CODE            PIC  X(02).
               05  PTINFLST-IC-CODE-NAME       PIC  X(20).
      *
      *        �ϰ�Ϣ��ID
               05  PTINFLST-COMMUNITY-CID        PIC  X(20).
      *        Ʊ�եե饰
               05  PTINFLST-COMMUNITY-CID-AGREE  PIC  X(05).
      *
      *        ����象��
               05  PTINFLST-FIRST-DATE           PIC  X(10).
      *        �ǽ��象�����ౡ����
               05  PTINFLST-LAST-DATE            PIC  X(10).
      *        �������ʬ�ʣ����������
               05  PTINFLST-OUTPATIENT-CLASS     PIC  X(01).
      *        ��������������Τߡ�
               05  PTINFLST-ADMISSION-DATE       PIC  X(10).
      *        �ౡ���������ౡ���Τߡ�
               05  PTINFLST-DISCHARGE-DATE       PIC  X(10).
      *
      *    �ݸ�����
           03  PTINFLST-INSURANCE-INFO     OCCURS  40 TIMES.
      *        �ݸ��ȹ礻�ֹ�
               05  PTINFLST-COMB-NUMBER            PIC X(04).
      *        �ݸ��ȹ礻��������
               05  PTINFLST-COMB-START-DATE        PIC X(10).
      *        �ݸ��ȹ礻����λ��
               05  PTINFLST-COMB-END-DATE          PIC X(10).
      *        �ݸ��ȹ礻����ô����������
               05  PTINFLST-COMB-RATE-ADM          PIC X(04).
      *        �ݸ��ȹ礻����ô���ʳ����
               05  PTINFLST-COMB-RATE-OUT          PIC X(04).
      *        ��ɽ����ʬ
               05  PTINFLST-COMB-HYOJI             PIC X(01).
      *
      *        �ݸ�����
               05  PTINFLST-MAIN-INSURANCE-INFO.
      *            �ݸ��μ���
                   07  PTINFLST-MAIN-CLASS             PIC X(03).
      *            �ݸ����ֹ�
                   07  PTINFLST-INSURANCE-NUMBER       PIC X(08).
      *            �ݸ��μ���̾��
                   07  PTINFLST-MAIN-NAME              PIC X(20).
      *            ����
                   07  PTINFLST-MAIN-MARK              PIC X(80).
      *            �ֹ�
                   07  PTINFLST-MAIN-NUMBER            PIC X(80).
      *            ��³��ʬ
                   07  PTINFLST-MAIN-CONTINUATION      PIC X(01).
      *            �����ʬ
                   07  PTINFLST-MAIN-ASSISTANCE        PIC X(01).
      *            �����ʬ̾��
                   07  PTINFLST-MAIN-ASSISTANCE-NAME   PIC X(20).
      *            �ܿͲ�²��ʬ
                   07  PTINFLST-MAIN-FAMILY            PIC X(01).
      *            ���ݸ���̾
                   07  PTINFLST-MAIN-POLICYHOLDER      PIC X(100).
      *            ͭ��������
                   07  PTINFLST-MAIN-START-DATE        PIC X(10).
      *            ͭ����λ��
                   07  PTINFLST-MAIN-END-DATE          PIC X(10).
      *            ��ʼ�����
                   07  PTINFLST-MAIN-GET-DATE          PIC X(10).
      *            �ǽ���ǧ��
                   07  PTINFLST-MAIN-CHECK-DATE        PIC X(10).
      *
      *            �������
                   07  PTINFLST-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *                ����μ���
                       09  PTINFLST-PUBLIC-CLASS           PIC X(03).
      *                ����μ���̾��
                       09  PTINFLST-PUBLIC-NAME            PIC X(20).
      *                ��ô���ֹ�
                       09  PTINFLST-PROVIDER-NUMBER        PIC X(08).
      *                ������ֹ�
                       09  PTINFLST-RECIPIENT-NUMBER       PIC X(20).
      *                ��������ôΨ�ʳ��
                       09  PTINFLST-RATE-ADMISSION         PIC X(04).
      *                �����ݸ����
                       09  PTINFLST-MONEY-ADMISSION        PIC X(06).
      *                �������ôΨ�ʳ��
                       09  PTINFLST-RATE-OUTPATIENT        PIC X(04).
      *                ����ݸ����
                       09  PTINFLST-MONEY-OUTPATIENT       PIC X(06).
      *                ͭ��������
                       09  PTINFLST-PUBLIC-START-DATE      PIC X(10).
      *                ͭ����λ��
                       09  PTINFLST-PUBLIC-END-DATE        PIC X(10).
      *R01.6
      *                �ǽ���ǧ��
                       09  PTINFLST-PUBLIC-CHECK-DATE      PIC X(10).
      *ϫ�Ҿ���
               05  PTINFLST-ACCIDENT-INF-G.
      *            ϫ���ݸ�̾�Ρʵ����Ʊ�����ơ�
                   07  PTINFLST-ACCIDENT-NAME           PIC X(50).
      *            ���¤����̡��ֹ��Ʊ�����ơ�
                   07  PTINFLST-DISEASE-LOCATION        PIC X(100).
      *            ����ǯ����
                   07  PTINFLST-DISEASE-DATE            PIC X(10).
      *ϫ��
      *            ϫƯ�ݸ��ֹ�
                   07  PTINFLST-ACCIDENT-NUMBER         PIC X(14).
      *            ǯ��ڽ��ֹ�
                   07  PTINFLST-PENSIONCERTIFICATE      PIC X(09).
      *            �ҳ���ʬ��̾�Ρ�
                   07  PTINFLST-ACCIDENT-CLASS          PIC X(20).
      *            ϫƯ�����Ľ𥳡���
                   07  PTINFLST-LABOR-ST-CODE           PIC X(05).
      *            ϫƯ�����Ľ𥳡���
                   07  PTINFLST-LABOR-ST-NAME           PIC X(50).
      *            ���Ƚ�̾��
                   07  PTINFLST-RSI-JIGYOUNAME          PIC X(80).
      *            �������ƻ�ܸ�̾
                   07  PTINFLST-RSI-PREFNAME            PIC X(20).
      *            �������ƻ�ܸ���ʬ
                   07  PTINFLST-RSI-PREFKBN             PIC X(01).
      *            �������ƻ�ܸ���ʬ̾��
                   07  PTINFLST-RSI-PREFKBNNAME         PIC X(02).
      *            ����Ϸ��Զ�̾
                   07  PTINFLST-RSI-CITYNAME            PIC X(80).
      *            ����Ϸ��Զ��ʬ
                   07  PTINFLST-RSI-CITYKBN             PIC X(01).
      *            ����Ϸ��Զ�̾��
                   07  PTINFLST-RSI-CITYKBNNAME         PIC X(02).
      *������
      *            �������ݸ����̾
                   07  PTINFLST-RSI-HOKENNAME           PIC X(80).
      *���ե�������
      *            �򹯴�����Ģ�ֹ�
                   07  PTINFLST-RSI-KANRITCHO-NUMBER    PIC X(13).
      *            »����ʬ
                   07  PTINFLST-DAMAGE-CLASS            PIC X(03).
      *            »����ʬ̾��
                   07  PTINFLST-DAMAGE-CLASS-NAME       PIC X(100).
      *
      *    ������
           03  PTINFLST-CARE-INF-G.
      *        ����ݸ�����
               05  PTINFLST-CARE-INSURANCE-G    OCCURS   10.
      *            �ݸ����ֹ�
                   07  PTINFLST-CARE-INSURANCE-NUMBER  PIC X(06).
      *            ���ݸ����ֹ�
                   07  PTINFLST-CARE-POLICYHOLDER      PIC X(10).
      *            ͭ��������
                   07  PTINFLST-CARE-START-DATE        PIC X(10).
      *            ͭ����λ��
                   07  PTINFLST-CARE-END-DATE          PIC X(10).
      *            �ײ��ǧ�����
               05  PTINFLST-CERTIFICATION-G    OCCURS   50.
      *            �ײ����ֶ�ʬ
                   07  PTINFLST-NEED-CARE-STATE-CODE   PIC X(02).
      *            �ײ�����̾��
                   07  PTINFLST-NEED-CARE-STATE        PIC X(20).
      *            ǧ������
                   07  PTINFLST-NEED-DATE              PIC X(10).
      *            ͭ��������
                   07  PTINFLST-NEED-START-DATE        PIC X(10).
      *            ͭ����λ��
                   07  PTINFLST-NEED-END-DATE          PIC X(10).
      *
      *    �ϰ��������оݼ���
           03  PTINFLST-COMMUNITY-DISEASE.
               05  PTINFLST-TARGET-DISEASE     PIC X(05)
                                                   OCCURS   4.
      *
      *    ���ԸĿ��ֹ����
           03  PTINFLST-PTMYNUMBER-G.
               05  PTINFLST-PTMYNUMBER-TBL     OCCURS   20.
      *            �ɣļ���
                   07  PTINFLST-ID-KEY           PIC X(10).
      *            �ɣ��ֹ�
                   07  PTINFLST-MY-NUMBER        PIC X(20).
      *            ����
                   07  PTINFLST-DESCRIPTION      PIC X(100).
      *
      *H30.10
      *    ���Ը��̾���
           03  PTINFLST-PTCONF-INFO.
      *        ǥ������
               05  PTINFLST-NINPUKBN             PIC X(05).
      *        ǧ�ξ��ϰ�������
               05  PTINFLST-NINTIHOKATU          PIC X(05).
      *        ����������Ĥ�����
               05  PTINFLST-SYOUNIKAKARI         PIC X(05).
      *
      *    ����������ư����
           03  PTINFLST-AUTO-SRYCD-G.
               05  PTINFLST-AUTO-SRYCD-TBL     OCCURS   3.
                   07  PTINFLST-AUTO-SRYCD       PIC X(09).
                   07  PTINFLST-AUTO-NAME        PIC X(80).
                   07  PTINFLST-AUTO-ENDYMD      PIC X(10).
      *
      *    ���Զش�����
           03  PTINFLST-KINKI-YKZAI-G.
               05  PTINFLST-KINKI-YKZAI-TBL    OCCURS   100.
      *            �ش����ޥ�����
                   07  PTINFLST-DRUGCD-CODE       PIC X(09).
                   07  PTINFLST-DRUGCD-NAME       PIC X(80).
      *            ͭ����λ���ʥޥ�����λ����
                   07  PTINFLST-DRUGCD-END-DATE   PIC X(10).
      *            �ش�������
                   07  PTINFLST-START-DATE        PIC X(10).
      *