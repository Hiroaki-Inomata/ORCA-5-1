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
      *    ���Уɥ쥹�ݥ󥹿��Ź԰پ�������ʼ���ǯ��������ơ�(xml2)
      *                                      COPY (CPMDCGET03V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-MEDICALGET03RES.
            02  MDC03RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC03RES-OBJECT            PIC X(36).
            02  MDC03RES-MODE              PIC S9(9)   BINARY.
            02  MDC03RES-RECORDNAME        PIC X(128).
            02  MDC03RES-MEDICALRES.
      *       �»���
              03  MDC03RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  MDC03RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  MDC03RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  MDC03RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC03RES-RESKEY              PIC X(100).
      *       ���Ծ���
              03  MDC03RES-PATIENT-INFOMATION.
      *           �����ֹ�
                  05  MDC03RES-PATIENTID         PIC X(20).
      *           ���Ի�̾
                  05  MDC03RES-NAME              PIC X(100).
      *           ���ԥ��ʻ�̾
                  05  MDC03RES-KANANAME          PIC X(100).
      *           ��ǯ����
                  05  MDC03RES-BIRTHDAY          PIC X(10).
      *           ����
                  05  MDC03RES-SEX               PIC X(1).
      *       ����ǯ��
              03  MDC03RES-PERFORM-DATE        PIC X(07).
      *       �������ƾ���
              03  MDC03RES-MEDICAL-INF         OCCURS   400.
      *           ���Ŷ�ʬ
                  05  MDC03RES-MDC-CLASS           PIC X(02).
      *           ���Ŷ�ʬ̾��
                  05  MDC03RES-MDC-CLASS-NAME      PIC X(50).
      *           ���ť�����
                  05  MDC03RES-PRSCRPT-CODE        PIC X(9).
      *           ����̾��
                  05  MDC03RES-PRSCRPT-NAME        PIC X(140).
      *           ñ��
                  05  MDC03RES-UNIT-CODE           PIC X(03).
      *           ñ��̾��
                  05  MDC03RES-UNIT-NAME           PIC X(24).
      *           ����ʬ
                  05  MDC03RES-MDC-CODE            PIC X(01).
      *           ����ʬ��True�����ޡ�
                  05  MDC03RES-INCLUSION-CLASS     PIC X(05).
      *           ���������
                  05  MDC03RES-MDC-CLASS-NUMBER    PIC X(02).
      *           ������
                  05  MDC03RES-MDC-DAY-INF         PIC X(31).
      *           ��������
                  05  MDC03RES-MDC-DAY-G           OCCURS   31.
      *               ������
                      07  MDC03RES-DAY                  PIC X(02).
      *               ����׻�����
                      07  MDC03RES-DAY-PRSCRPT-NUMBER   PIC X(11).
      *
