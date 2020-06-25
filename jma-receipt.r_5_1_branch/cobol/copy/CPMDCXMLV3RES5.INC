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
      ******************************************************************
      *    ���Уɥ쥹�ݥ󥹥�ϥӥꥳ���������Ͼ��� (���β�)
      *                                          COPY (CPMDCXMLV3RES5.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  XML-MEDICALV3RES5.
           03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
           03  MDCRES-OBJECT              PIC X(36).
           03  MDCRES-MODE                PIC S9(9)   BINARY.
           03  MDCRES-RECORDNAME          PIC X(128).
           03  MDCRES-MEDICALRES.
      *        �ꥯ�������ֹ�
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        �쥹�ݥ��ֹ�
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID���Żҥ���ơ�
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID�ʥ��륫��
               05  MDCRES-ORCA-UID          PIC X(36).
      *        �»���
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        �»ܻ���
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        �ֵѥ�����
               05  MDCRES-API-RESULT        PIC X(03).
      *        �ֵѥ�å�����
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *
      *        ���Ծ���
               05  MDCRES-PATIENT-INFOMATION.
                   07  MDCRES-PATIENTID       PIC X(20).
                   07  MDCRES-NAME            PIC X(100).
                   07  MDCRES-KANANAME        PIC X(100).
                   07  MDCRES-BIRTHDAY        PIC X(10).
                   07  MDCRES-SEX             PIC X(1).
      *        ��ϥӥ�ơ���������������ʥꥯ�����ȡᣰ��)
               05  MDCRES-SANTEIRRK-TBLINF.
                   07  MDCRES-SANTEIRRK-TBL   OCCURS   100.
      *                ���������ť�����
                       09  MDCRES-TPRSCRPT-CODE          PIC X(9).
      *                ����̾��
                       09  MDCRES-TPRSCRPT-NAME          PIC X(80).
      *                ����ǯ��
                       09  MDCRES-TPERFORM-YM            PIC X(7).
      *                �ݸ��ȹ礻�ֹ�
                       09  MDCRES-TCOMBINATION-NUMBER    PIC X(04).
      *                �ݸ��ȹ礻�ֹ�̾��
                       09  MDCRES-TCOMBINATION-NAME      PIC X(100).
      *                ������
                       09  MDCRES-TPERFORM-DATE-G.
                           11  MDCRES-TPERFORM-DAY       PIC X(02)
                                                        OCCURS   31.
      *                ��Ͽ��ʬ(1�����Ź԰١�
                       09  MDCRES-TOROKU-MODE1           PIC X(01).
      *                �����ȶ�ʬ(1�������Ȥ����
                       09  MDCRES-TOROKU-MODE2           PIC X(01).
      *                ��λ��
                       09  MDCRES-TEND-DATE              PIC X(10).
      *                ��λ����ʬ(1�����Ź԰١�
                       09  MDCRES-TOROKU-MODE3           PIC X(01).
      *
      *        ����������ʥꥯ�����ȡᣰ������
               05  MDCRES-SANTEIRRK-INFO.
      *            ������ʬ
                   07  MDCRES-SANTEI-MODE           PIC X(6).
      *            ���������ť�����
                   07  MDCRES-PRSCRPT-CODE          PIC X(9).
      *            ����̾��
                   07  MDCRES-PRSCRPT-NAME          PIC X(80).
      *            ����ǯ��
                   07  MDCRES-PERFORM-YM            PIC X(07).
      *            �ݸ��ȹ礻�ֹ�
                   07  MDCRES-COMBINATION-NUMBER    PIC X(04).
      *            �ݸ��ȹ礻�ֹ�̾��
                   07  MDCRES-COMBINATION-NAME      PIC X(100).
      *            ������
                   07  MDCRES-PERFORM-DATE-G.
                       09  MDCRES-PERFORM-DAY       PIC X(02)
                                                   OCCURS   31.
      *            ���顼������
                   07  MDCRES-SAN-ERRCD-TBL        OCCURS   50.
                       09  MDCRES-SAN-ERRCD            PIC X(04).
                       09  MDCRES-SAN-ERRCD-MSG        PIC X(100).
      *
      *        ��ϥӥꥳ���ȡʥꥯ�����ȡᣰ������
               05  MDCRES-COMMNT-INFO.
      *            ������
                   07  MDCRES-COMMNT-DAY-TBL        OCCURS   31.
      *                ������
                       09  MDCRES-COMMNT-DAY            PIC X(02).
      *                ��λ��
                       09  MDCRES-END-DATE              PIC X(10).
      *                �����ȣ�
                       09  MDCRES-COMMNT-TBL-G.
                           11  MDCRES-COMMNT-TBL        OCCURS   50.
                               13  MDCRES-COMMNT        PIC X(80).
                               13  MDCRES-COM-ENDDATE   PIC X(10).
      *                ���顼������
                       09  MDCRES-COM-ERRCD            PIC X(04).
                       09  MDCRES-COM-ERRCD-MSG        PIC X(100).
      *
      *        ��ϥӥ�ơ��������������̤��Ͽ�����ɰ���
               05  MDCRES-STARTSRYCD-INF.
                   07  MDCRES-STARTSRYCD-TBL   OCCURS   100.
      *                ���������ť�����
                       09  MDCRES-STARTSRYCD-CODE          PIC X(9).
      *                ����̾��
                       09  MDCRES-STARTSRYCD-NAME          PIC X(80).
      *                Ŭ��������
                       09  MDCRES-STARTSRYCD-DATE          PIC X(10).
      *