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
      *    ���Уɴ�����Ͽ����ʸ�����ô�ۡ�(���β���V3)
      *                                   COPY (CPPATIENTMODV3REQ5.INC)
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
      * �ꥯ�����ȡᣰ�����飰��
      *       �����������
              03  PTMODREQ-KOHFTN-SELECT-G.
      *          ����ɣ�
                 05  PTMODREQ-INSURANCE-ID         PIC X(10).
      *          ����μ���
                 05  PTMODREQ-INSURANCE-CLASS      PIC X(03).
      *          ������ֹ�
                 05  PTMODREQ-RECIPIENT-NUMBER      PIC X(20).
      *          ͭ��������
                 05  PTMODREQ-INSURANCE-START-DATE  PIC X(10).
      *
      * �ꥯ�����ȡᣰ��
      *       ������ô�۾���
              03  PTMODREQ-PTKOHFTN-INFO-G.
      *          ������ʬ��MODIFY,DELETE)
                 05  PTMODREQ-PTKOHFTN-MODE        PIC X(06).
                 05  PTMODREQ-PTKOHFTN-INFO        OCCURS   300.
      *            �Խ�����ʬ��DELETE,NEW)
                   07  PTMODREQ-PTKOHFTN-LMODE         PIC X(06).
      *            Ϣ�֡ʥ쥹�ݥ����ơ�
                   07  PTMODREQ-PTKOHFTN-RENNUM        PIC X(03).
      *            ����ǯ����
                   07  PTMODREQ-PTKOHFTN-TEKSTYMD      PIC X(10).
      *            ��λǯ����
                   07  PTMODREQ-PTKOHFTN-TEKEDYMD      PIC X(10).
      *            �����³�
                   07  PTMODREQ-PTKOHFTN-GAIJGNGAK     PIC X(07).
      *            ������³�
                   07  PTMODREQ-PTKOHFTN-NYUJGNGAK     PIC X(07).
      *
      * �ꥯ�����ȡᣰ��������
      *       ������ô���������
              03  PTMODREQ-FTNETC-SELECT-G.
      *           Ϣ��
                  05  PTMODREQ-FTN-SEL-RENNUM        PIC X(03).
      *           ����ǯ����
                  05  PTMODREQ-FTN-SEL-TEKSTYMD      PIC X(10).
      *
      * �ꥯ�����ȡᣰ��
      *       ¾������ô��
              03  PTMODREQ-FTNETC-INFO-G.
      *          ������ʬ��MODIFY(NEW),DELETE)
                 05  PTMODREQ-FTNETC-MODE        PIC X(06).
                 05  PTMODREQ-FTNETC-INFO        OCCURS   100.
      *              ǯ����
                     07  PTMODREQ-FTNETC-SRYYMD      PIC  X(10).
      *              ������ʬ
                     07  PTMODREQ-FTNETC-NYUGAIKBN   PIC  X(01).
      *              ��ô���
                     07  PTMODREQ-FTNETC-FTNMONEY    PIC  X(07).
      *
      *
