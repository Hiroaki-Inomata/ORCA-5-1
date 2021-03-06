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
      *    ���Уɴ�����Ͽ����ʸ�����ô�۾����(���β���V3)
      *                                   COPY (CPPATIENTMODV3RES5.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      �ꥯ�������ֹ�
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      �쥹�ݥ��ֹ�
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID���Żҥ���ơ�
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID�ʥ��륫��
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      ��³��ʬ
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      �»���
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      �»ܻ���
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      �ֵѥ�����
             03  PTMODRES-API-RESULT        PIC X(3).
      *      �ֵѥ�å�����
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY            PIC X(100).
      *      �����ֹ�
             03  PTMODRES-PATIENTID         PIC X(20).
      *      ���Ծ���
             03  PTMODRES-PTINF-INFO.
      *        ���Ի�̾
               05  PTMODRES-NAME             PIC X(100).
      *        ���ԥ��ʻ�̾
               05  PTMODRES-KANANAME         PIC X(100).
      *        ��ǯ����
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        ����
               05  PTMODRES-SEX              PIC X(01).
      *
      * �ꥯ�����ȡᣰ��
      *       �оݸ������
              03  PTMODRES-PUBLIC-INSURANCE-INFG.
                 05  PTMODRES-PUBLIC-INSURANCE-INF  OCCURS  60 TIMES.
      *            ����ɣ�
                   07  PTMODRES-INSURANCE-ID          PIC X(10).
      *            ����μ���
                   07  PTMODRES-INSURANCE-CLASS       PIC X(03).
      *            ����μ���̾��
                   07  PTMODRES-INSURANCE-NAME        PIC X(20).
      *            ������ֹ�
                   07  PTMODRES-RECIPIENT-NUMBER      PIC X(20).
      *            ͭ��������
                   07  PTMODRES-INSURANCE-START-DATE  PIC X(10).
      *            ͭ����λ��
                   07  PTMODRES-INSURANCE-END-DATE    PIC X(10).
      *
      * �ꥯ�����ȡᣰ��������
      *       ������ô�۾���
              03  PTMODRES-KOHFTN-INFO-G.
      *          ����ɣ�
                 05  PTMODRES-SEL-INSURANCE-ID      PIC X(10).
      *          ����μ���
                 05  PTMODRES-SEL-INSURANCE-CLASS   PIC X(03).
      *          ����μ���
                 05  PTMODRES-SEL-INSURANCE-NAME    PIC X(20).
      *          ������ֹ�
                 05  PTMODRES-SEL-RECIPIENT-NUMBER  PIC X(20).
      *          ͭ��������
                 05  PTMODRES-SEL-INSURANCE-START   PIC X(10).
      *          ͭ����λ��
                 05  PTMODRES-SEL-INSURANCE-END     PIC X(10).
      *          ��ô�ۣ�̾��
                 05  PTMODRES-SEL-KOHFTN-NAME1       PIC X(20).
      *          ��ô�ۣ�̾��
                 05  PTMODRES-SEL-KOHFTN-NAME2       PIC X(20).
      *          ������֡ʣ�����³ۡ�����¾������ôͭ��������ô����
                 05  PTMODRES-SEL-KOHFTN-CLASS       PIC X(01).
      *
                 05  PTMODRES-PTKOHFTN-INFO        OCCURS   300.
      *            �Խ�����ʬ
                   07  PTMODRES-PTKOHFTN-LMODE         PIC X(06).
      *            Ϣ��
                   07  PTMODRES-PTKOHFTN-RENNUM        PIC X(03).
      *            ����ǯ����
                   07  PTMODRES-PTKOHFTN-TEKSTYMD      PIC X(10).
      *            ��λǯ����
                   07  PTMODRES-PTKOHFTN-TEKEDYMD      PIC X(10).
      *            �����³�
                   07  PTMODRES-PTKOHFTN-GAIJGNGAK     PIC X(07).
      *            ������³�
                   07  PTMODRES-PTKOHFTN-NYUJGNGAK     PIC X(07).
      *            ����¾������ô������ͭ�ʣ���ͭ��
                   07  PTMODRES-PTKOHFTN-ETC-KBN       PIC X(01).
      *            ���顼������
                   07  PTMODRES-PTKOHFTN-ERRCD         PIC X(04).
                   07  PTMODRES-PTKOHFTN-ERRCD-MSG     PIC X(100).
      *
      * �ꥯ�����ȡᣰ��������
      *       ����¾������ô��
              03  PTMODRES-PTKOHFTNETC-INFO-G.
      *          ����Ϣ��
                 05  PTMODRES-FTN-SEL-RENNUM        PIC X(03).
      *          ���򳫻�ǯ����
                 05  PTMODRES-FTN-SEL-TEKSTYMD      PIC X(10).
      *
      *          ���ֳ���ǯ����
                 05  PTMODRES-FTNETC-STRYMD         PIC X(10).
      *          ���ֽ�λǯ����
                 05  PTMODRES-FTNETC-ENDYMD         PIC X(10).
      *          ������³�
                 05  PTMODRES-FTNETC-GAIJGNGAK     PIC X(07).
      *          ¾������ô���߷�
                 05  PTMODRES-FTNETC-NYUJGNGAK     PIC X(07).
      *          �������׳ۡʼ�Ǽ��׳ۡ�
                 05  PTMODRES-FTNETC-SYUTOTAL      PIC X(07).
      *
                 05  PTMODRES-FTNETC-INFO-G2.
      *          ¾�屡��ô��
                     07  PTMODRES-FTNETC-INFO     OCCURS   100.
      *                 ǯ����
                        09  PTMODRES-FTNETC-SRYYMD      PIC  X(10).
      *                 ������ʬ
                        09  PTMODRES-FTNETC-NYUGAIKBN   PIC  X(01).
      *                 ��ô���
                        09  PTMODRES-FTNETC-FTNMONEY    PIC  X(07).
      *                 ���顼������
                        09  PTMODRES-FTNETC-ERRCD       PIC X(04).
                        09  PTMODRES-FTNETC-ERRCD-MSG   PIC X(100).
      *
