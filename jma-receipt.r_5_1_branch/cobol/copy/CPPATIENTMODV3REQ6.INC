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
      *    ���Уɴ�����Ͽ����ʲ������(���β���V3)
      *                                   COPY (CPPATIENTMODV3REQ6.INC)
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
      *       ����ݸ�����
              03  PTMODREQ-CARE-INFO-G.
      *          ������ʬ��MODIFY,DELETE)
                 05  PTMODREQ-CARE-MODE        PIC X(06).
      *          ����ݸ�����
                 05  PTMODREQ-CARE-INSURANCE-G     OCCURS   10.
      *              �ݸ����ֹ�
                     07  PTMODREQ-CARE-INSURANCE-NUMBER  PIC X(06).
      *              ���ݸ����ֹ�
                     07  PTMODREQ-CARE-POLICYHOLDER      PIC X(10).
      *              ͭ��������
                     07  PTMODREQ-CARE-START-DATE        PIC X(10).
      *              ͭ����λ��
                     07  PTMODREQ-CARE-END-DATE          PIC X(10).
      *
      *       �ײ��ǧ�����
              03  PTMODREQ-NEED-INFO-G.
      *          ������ʬ��MODIFY,DELETE)
                 05  PTMODREQ-NEED-MODE        PIC X(06).
      *          �ײ��ǧ�����
                 05  PTMODREQ-CERTIFICATION-G    OCCURS   50.
      *              �ײ����ֶ�ʬ
                     07  PTMODREQ-NEED-CARE-STATE-CODE   PIC X(02).
      *              ǧ������
                     07  PTMODREQ-NEED-DATE              PIC X(10).
      *              ͭ��������
                     07  PTMODREQ-NEED-START-DATE        PIC X(10).
      *              ͭ����λ��
                     07  PTMODREQ-NEED-END-DATE          PIC X(10).
      *