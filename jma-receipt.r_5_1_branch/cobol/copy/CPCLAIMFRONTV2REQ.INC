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
      *    ���Уɥꥯ�����ȣạ̃��ɣͼ����������� (xml2)
      *                                      COPY (CPCLAIMFRONTV02REQ.INC)
      ******************************************************************
       01  XML-CLAIM-FRONTV2REQ.
           02  CLFRREQ-CONTEXT           PIC S9(9)   BINARY.
           02  CLFRREQ-OBJECT            PIC X(36).
           02  CLFRREQ-MODE              PIC S9(9)   BINARY.
           02  CLFRREQ-RECORDNAME        PIC X(128).
           02  CLFRREQ-CLAIM-FRONT.
      *        �ꥯ�������ֹ�
               03  CLFRREQ-REQUEST-NUMBER    PIC X(02).
      *        �����ֹ�
               03  CLFRREQ-PATIENTID         PIC X(20).
      *        ��������
               03  CLFRREQ-ACCEPT-DATE       PIC X(10).
      *        ���ջ���
               03  CLFRREQ-ACCEPT-TIME       PIC X(8).
      *        ���Ų�
               03  CLFRREQ-DEP-CODE          PIC X(02).
      *        �ɥ�����
               03  CLFRREQ-DR-CODE           PIC X(05).
      *        �ݸ��ȹ礻�ֹ�
               03  CLFRREQ-COMBINATION-NUMBER      PIC X(04).
      *        ��������̾��
               03  CLFRREQ-MEDICAL-NAME       PIC X(16).
      *        ���������ʬ
               03  CLFRREQ-MEMO-CODE          PIC X(02).
      *        ���պѤ�������ʬ�ʥꥯ�������ֹ�ᣰ�����Τߡ�
               03  CLFRREQ-SEND-STATUS       PIC X(02).
      *