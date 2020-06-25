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
      *    ���Уɥꥯ�����ȼ��վ��󡡡�       COPY (CPACCPTXMLV2REQ.INC)
      ******************************************************************
        01  XML-ACCEPTREQ.
            02  ACCPTREQ-CONTEXT          PIC S9(9)   BINARY.
            02  ACCPTREQ-OBJECT           PIC X(36).
            02  ACCPTREQ-MODE             PIC S9(9)   BINARY.
            02  ACCPTREQ-RECORDNAME       PIC X(128).
            02  ACCPTREQ-REGIST.
      *       �ꥯ�������ֹ�
              03  ACCPTREQ-REQUEST-NUMBER  PIC X(2).
      *       �����ֹ�
              03  ACCPTREQ-PATIENTID       PIC X(20).
      *       ���Ի�̾
              03  ACCPTREQ-NAME            PIC X(50).
      *
      *       ������
              03  ACCPTREQ-REGIST-DATE  PIC X(10).
      *       ���ջ���
              03  ACCPTREQ-REGIST-TIME  PIC X(8).
      *       ���գɣ�
              03  ACCPTREQ-REGIST-ID    PIC X(5).
      *       ���Ų�
              03  ACCPTREQ-REGIST-DEP-CODE PIC X(2).
      *       �ɥ�����������
              03  ACCPTREQ-REGIST-DR-CODE  PIC X(5).
      *       �������ƥ�����
              03  ACCPTREQ-REGIST-DEP-INFO PIC X(2).
      *       �ݸ�����
              03  ACCPTREQ-INSURANCE-INFO.
      *         �ݸ��ȹ礻�ֹ�
                04  ACCPTREQ-COMBINATION-NUMBER     PIC X(04).
      *         �ݸ��μ���
                04  ACCPTREQ-MAIN-INSURANCE-CLASS   PIC X(03).
      *         �ݸ����ֹ�
                04  ACCPTREQ-MAIN-INSURANCE-NUMBER  PIC X(08).
      *         �ݸ��μ���̾��
                04  ACCPTREQ-MAIN-INSURANCE-NAME    PIC X(20).
      *         ����
                04  ACCPTREQ-MAIN-MARK              PIC X(80).
      *         �ֹ�
                04  ACCPTREQ-MAIN-NUMBER            PIC X(80).
      *         ��³��ʬ
                04  ACCPTREQ-MAIN-CONTINUATION      PIC X(01).
      *         �ݸ��μ���
                04  ACCPTREQ-MAIN-ASSISTANCE        PIC X(01).
      *         �ܿͲ�²��ʬ
                04  ACCPTREQ-MAIN-FAMILY            PIC X(01).
      *         ���ݸ���̾
                04  ACCPTREQ-MAIN-POLICYHOLDER      PIC X(100).
      *         ͭ��������
                04  ACCPTREQ-MAIN-START-DATE        PIC X(10).
      *         ͭ����λ��
                04  ACCPTREQ-MAIN-END-DATE          PIC X(10).
      *         �������
                04  ACCPTREQ-PUBLIC-INSURANCE-INFO
                                       OCCURS  4 TIMES.
      *           ����μ���
                  05  ACCPTREQ-INSURANCE-CLASS      PIC X(03).
      *           ����μ���̾��
                  05  ACCPTREQ-INSURANCE-NAME       PIC X(20).
      *           ��ô���ֹ�
                  05  ACCPTREQ-PROVIDER-NUMBER      PIC X(08).
      *           ������ֹ�
                  05  ACCPTREQ-RECIPIENT-NUMBER     PIC X(20).
      *           ͭ��������
                  05  ACCPTREQ-INSURANCE-START-DATE PIC X(10).
      *           ͭ����λ��
                  05  ACCPTREQ-INSURANCE-END-DATE   PIC X(10).