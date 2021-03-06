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
      *    API ��ǻ�������Ͽ����(xml2)   COPY (CPMDCXMLV2RES3.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  XML-MEDICALV2RES3.
           03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
           03  MDCRES-OBJECT              PIC X(36).
           03  MDCRES-MODE                PIC S9(9)   BINARY.
           03  MDCRES-RECORDNAME          PIC X(128).
           03  MDCRES-MEDICALRES.
      *        ����
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        �»ܻ���
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        �ֵѥ�����
               05  MDCRES-API-RESULT        PIC X(3).
      *        �ֵѥ�å�����
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        ���Ծ���
               05  MDCRES-PATIENT-INFOMATION.
      *            �����ֹ�
                   07  MDCRES-PATIENTID        PIC X(20).
      *            ���Ի�̾
                   07  MDCRES-NAME             PIC X(100).
                   07  MDCRES-KANANAME         PIC X(100).
                   07  MDCRES-BIRTHDAY         PIC X(10).
                   07  MDCRES-SEX              PIC X(1).
      *        ��ǻ�����
               05  MDCRES-FIRST-DATE       PIC X(10).
      *        �ǽ��象��
               05  MDCRES-LAST-DATE        PIC X(10).
      *        ���Ų�
               05  MDCRES-DEP-CODE         PIC X(2).
               05  MDCRES-DEP-NAME         PIC X(80).
      *        �������������ʥꥯ�����ȡ�00 �Τߡ�
               05  MDCRES-FIRST-VISIT-DATE  PIC X(10).
      *
