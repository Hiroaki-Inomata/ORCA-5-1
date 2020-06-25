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
      *    ���Ծ�����ܾ����Խ�����
      *                                    COPY (CPORAPI012RSUB1.INC)
      ******************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  18/10/XX  ���Ը��̾��󤫤�ش������ɲ�
      ************************************************************
       01  ORAPI012RSUB1AREA.
      *
      *    ���ԣɣ�
           03  API12SUB-PTID               PIC 9(10).
      *
           03  API12SUB-PATIENT-INFOMATION.
      *        �ϰ�Ϣ��ID
               05  API12SUB-COMMUNITY-CID        PIC  X(20).
      *        Ʊ�եե饰
               05  API12SUB-COMMUNITY-CID-AGREE  PIC  X(05).
      *
      *        ����象��
               05  API12SUB-FIRST-DATE           PIC  X(10).
      *        �ǽ��象�����ౡ����
               05  API12SUB-LAST-DATE            PIC  X(10).
      *        �������ʬ�ʣ����������
               05  API12SUB-OUTPATIENT-CLASS     PIC  X(01).
      *        ��������������Τߡ�
               05  API12SUB-ADMISSION-DATE       PIC  X(10).
      *        �ౡ���������ౡ���Τߡ�
               05  API12SUB-DISCHARGE-DATE       PIC  X(10).
      *    ������
           03  API12SUB-CARE-INF-G.
      *        ����ݸ�����
               05  API12SUB-CARE-INSURANCE-G    OCCURS   10.
      *            �ݸ����ֹ�
                   07  API12SUB-CARE-INSURANCE-NUMBER  PIC X(06).
      *            ���ݸ����ֹ�
                   07  API12SUB-CARE-POLICYHOLDER      PIC X(10).
      *            ͭ��������
                   07  API12SUB-CARE-START-DATE        PIC X(10).
      *            ͭ����λ��
                   07  API12SUB-CARE-END-DATE          PIC X(10).
      *            �ײ��ǧ�����
               05  API12SUB-CERTIFICATION-G    OCCURS   50.
      *            �ײ����ֶ�ʬ
                   07  API12SUB-NEED-CARE-STATE-CODE   PIC X(02).
      *            �ײ�����̾��
                   07  API12SUB-NEED-CARE-STATE        PIC X(20).
      *            ǧ������
                   07  API12SUB-NEED-DATE              PIC X(10).
      *            ͭ��������
                   07  API12SUB-NEED-START-DATE        PIC X(10).
      *            ͭ����λ��
                   07  API12SUB-NEED-END-DATE          PIC X(10).
      *
      *    �ϰ��������оݼ���
           03  API12SUB-COMMUNITY-DISEASE.
               05  API12SUB-TARGET-DISEASE     PIC X(05)
                                                   OCCURS   4.
      *
      *    ���ԸĿ��ֹ����
           03  API12SUB-PTMYNUMBER-G.
               05  API12SUB-PTMYNUMBER-TBL     OCCURS   20.
      *            �ɣļ���
                   07  API12SUB-ID-KEY           PIC X(10).
      *            �ɣ��ֹ�
                   07  API12SUB-MY-NUMBER        PIC X(20).
      *            ����
                   07  API12SUB-DESCRIPTION      PIC X(100).
      *
      *H30.10
      *    ���Ը��̾���
           03  API12SUB-PTCONF-INFO.
      *        ǥ������
               05  API12SUB-NINPUKBN             PIC X(05).
      *        ǧ�ξ��ϰ�������
               05  API12SUB-NINTIHOKATU          PIC X(05).
      *        ����������Ĥ�����
               05  API12SUB-SYOUNIKAKARI         PIC X(05).
      *
      *    ����������ư����
           03  API12SUB-AUTO-SRYCD-G.
               05  API12SUB-AUTO-SRYCD-TBL     OCCURS   3.
                   07  API12SUB-AUTO-SRYCD       PIC X(09).
                   07  API12SUB-AUTO-NAME        PIC X(80).
                   07  API12SUB-AUTO-ENDYMD      PIC X(10).
      *
      *    ���Զش�����
           03  API12SUB-KINKI-YKZAI-G.
               05  API12SUB-KINKI-YKZAI-TBL    OCCURS   100.
      *            �ش����ޥ�����
                   07  API12SUB-DRUGCD-CODE       PIC X(09).
                   07  API12SUB-DRUGCD-NAME       PIC X(80).
      *            ͭ����λ���ʥޥ�����λ����
                   07  API12SUB-DRUGCD-END-DATE   PIC X(10).
      *            �ش�������
                   07  API12SUB-START-DATE        PIC X(10).
      *