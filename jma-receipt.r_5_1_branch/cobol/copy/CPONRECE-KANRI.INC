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
      ************************************************************
      *    ����饤��쥻�Ŵ����ޥ���   COPY (CPONRECE-KANRI.INC)*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    16/06/27  ���ŵ��إ������ѹ����Υ쥻��
      *                                     �ǡ������κ����б�
      *                                      �ʣ���������ѹ��Τߡ�
      ************************************************************
      *01  ONRECE-KANRI-REC.
      *���ŵ����ֹ�
           03  ONRECE-K-HOSPNUM    PIC 9(2).
      *�����
           03  ONRECE-K-TEISYUTUSAKI
                                   PIC X(1).
      *���ŵ��إ�����
           03  ONRECE-K-HOSPCD     PIC X(7).
      *����ǯ��
           03  ONRECE-K-SRYYM      PIC X(6).
      *���ղ��
           03  ONRECE-K-UCNT       PIC 9(3).
      *������
           03  ONRECE-K-SCNT       PIC 9(3).
      *���̥쥻�ż»ܻ��β��(���� ���� 1)
           03  ONRECE-K-RENNUM     PIC 9(3).
      *����饤��쥻�ż»���
           03  ONRECE-K-SYORIYMD   PIC X(8).
      *
           03  ONRECE-K-UKEYMD     PIC X(8).
      *
           03  ONRECE-K-SKYYMD     PIC X(8).
      *���ջ�̳�����ե������Ǽ�ե饰�ʳ�Ǽ������
           03  ONRECE-K-RECV-FLG   PIC 9(1).
      *�������ե������Ǽ�ե饰�ʳ�Ǽ������
            03  ONRECE-K-AMOUNT-FLG PIC 9(1).
      *���顼�ʤ��������������
           03  ONRECE-K-SEND-PATIENT-CNT
                                   PIC 9(5).
      *���顼������������ʤ��ä����
           03  ONRECE-K-NOSEND-PATIENT-CNT
                                   PIC 9(5).
      *��׷��(�������)
           03  ONRECE-K-SEND-TOTAL-CNT 
                                   PIC 9(5).
      *ü���ɣ�
           03  ONRECE-K-TERMID     PIC X(64).
      *���ڡ��졼���ɣ�
           03  ONRECE-K-OPID       PIC X(16).
      *����ǯ����
           03  ONRECE-K-CREYMD     PIC X(08).
      *����ǯ����
           03  ONRECE-K-UPYMD      PIC X(08).
      *��������
           03  ONRECE-K-UPHMS      PIC X(06).
      *�����ѹ��� 
           03  ONRECE-K-VIRTUAL.
              05  ONRECE-K-UCNT-V  PIC 9(3).
              05  ONRECE-K-SCNT-V  PIC 9(3).