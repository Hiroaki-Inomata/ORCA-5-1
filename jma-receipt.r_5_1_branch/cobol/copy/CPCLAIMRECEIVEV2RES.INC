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
      *    ���Уɥ쥹�ݥ󥹣ạ̃��ɣ����ӥǡ��������� (xml2)
      *                                      COPY (CPCLAIMRECEIVEV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  XML-CLAIM-RECEIVEV2RES.
           02  CLRERES-CONTEXT           PIC S9(9)   BINARY.
           02  CLRERES-OBJECT            PIC X(36).
           02  CLRERES-MODE              PIC S9(9)   BINARY.
           02  CLRERES-RECORDNAME        PIC X(128).
           02  CLRERES-CLAIM-FRONT.
      *        �»���
               03  CLRERES-INFORMATION-DATE    PIC X(10).
      *        �»ܻ���
               03  CLRERES-INFORMATION-TIME    PIC X(8).
      *        �ֵѥ�����
               03  CLRERES-API-RESULT          PIC X(2).
      *        �ֵѥ�å�����
               03  CLRERES-API-RESULT-MSG      PIC X(100).
      *        �����ֹ�
               03  CLRERES-PATIENTID           PIC X(20).
      *        ������
               03  CLRERES-PERFORM-DATE        PIC X(19).
      *        ��å�����
               03  CLRERES-MESSAGE-G.
      *            ��å�����
                   05  CLRERES-WARNING-MSG-TBL   OCCURS  50.
      *                ��å�����������
                       07  CLRERES-WARNING        PIC X(03).
      *                ��å�����
                       07  CLRERES-WARNING-MSG    PIC X(100).