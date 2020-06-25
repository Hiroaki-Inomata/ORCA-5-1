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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����Ͽ������ݸ���������(���β���V3)
      *                                   COPY (CPPATIENTMODV3REQ2.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID���Żҥ���ơ�
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       UID�ʥ��륫��
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       ������աʥ����ƥ����ա�
              03  PTMODREQ-BASE-DATE        PIC X(10).
      *       ��ǧ��å���������
              03  PTMODREQ-SELECT-ANSWER    PIC X(03).
      *       ��³��ʬ
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       ���Ծ���
              03  PTMODREQ-PTINF-INFO.
      *         �����ֹ�
                05  PTMODREQ-PATIENTID        PIC X(20).
      *         ���Ի�̾
                05  PTMODREQ-NAME             PIC X(100).
      *         ���ԥ��ʻ�̾
                05  PTMODREQ-KANANAME         PIC X(100).
      *         ��ǯ����
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         ����
                05  PTMODREQ-SEX              PIC X(01).
      *
      *       �ݸ�����
              03  PTMODREQ-INSURANCE-INFO-G.
                  05  PTMODREQ-INSURANCE-INFO     OCCURS   40.
      *             ������ʬ
                    07  PTMODREQ-MAIN-MODE               PIC X(06).
      *             �ݸ��ɣ�
                    07  PTMODREQ-MAIN-INSURANCE-ID       PIC X(10).
      *             �ݸ��μ���
                    07  PTMODREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *             �ݸ����ֹ�
                    07  PTMODREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *             �ݸ��μ���̾��
                    07  PTMODREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *             ����
                    07  PTMODREQ-MAIN-MARK               PIC X(80).
      *             �ֹ�
                    07  PTMODREQ-MAIN-NUMBER             PIC X(80).
      *             ����
                    07  PTMODREQ-MAIN-EDABAN             PIC X(02).
      *             ��³��ʬ
                    07  PTMODREQ-MAIN-CONTINUATION       PIC X(01).
      *             �����ʬ
                    07  PTMODREQ-MAIN-ASSISTANCE         PIC X(01).
      *             �ܿͲ�²��ʬ
                    07  PTMODREQ-MAIN-FAMILY             PIC X(01).
      *             ���ݸ���̾
                    07  PTMODREQ-MAIN-POLICYHOLDER       PIC X(100).
      *             ͭ��������
                    07  PTMODREQ-MAIN-START-DATE         PIC X(10).
      *             ͭ����λ��
                    07  PTMODREQ-MAIN-END-DATE           PIC X(10).
      *             ��ʼ�����
                    07  PTMODREQ-MAIN-GET-DATE           PIC X(10).
      *             ��ǧ����
                    07  PTMODREQ-MAIN-CHECK-DATE         PIC X(10).
      *             �������ô��ʬ
                    07  PTMODREQ-RATE-CLASS              PIC X(01).
      *       �������
              03  PTMODREQ-PUBLIC-INSURANCE-INFG.
                  05  PTMODREQ-PUBLIC-INSURANCE-INF  OCCURS  60 TIMES.
      *             ������ʬ
                    07  PTMODREQ-INSURANCE-MODE        PIC X(06).
      *             ����ɣ�
                    07  PTMODREQ-INSURANCE-ID          PIC X(10).
      *             ����μ���
                    07  PTMODREQ-INSURANCE-CLASS       PIC X(03).
      *             ����μ���̾��
                    07  PTMODREQ-INSURANCE-NAME        PIC X(20).
      *             ��ô���ֹ�
                    07  PTMODREQ-PROVIDER-NUMBER       PIC X(08).
      *             ������ֹ�
                    07  PTMODREQ-RECIPIENT-NUMBER      PIC X(20).
      *             ͭ��������
                    07  PTMODREQ-INSURANCE-START-DATE  PIC X(10).
      *             ͭ����λ��
                    07  PTMODREQ-INSURANCE-END-DATE    PIC X(10).
      *             ��ǧ����
                    07  PTMODREQ-INSURANCE-CHECK-DATE  PIC X(10).
      *