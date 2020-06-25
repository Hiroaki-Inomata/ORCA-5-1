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
      *    ����饤���������   COPY (CPHENREI-KANRI.INC)        *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/12/20  �ݸ��Ԥ���������б�
      *
      *  04.08.01    NACL-ƣ��    16/06/27  ���ŵ��إ������ѹ����Υ쥻��
      *                                     �ǡ������κ����б�
      *                                      �ʣ���������ѹ��Τߡ�
      ************************************************************
      *01  HENREI-KANRI-REC.
      *���ŵ����ֹ�
           03  HENREI-K-HOSPNUM    PIC 9(2).
      *�����
           03  HENREI-K-TEISYUTUSAKI
                                   PIC X(1).
      *�����
           03  HENREI-K-TEISYUTUSAKI2
                                   PIC X(1).
      *���ŵ��إ����ɡ�RE�쥳���ɤο���ǯ��˳�����
           03  HENREI-K-HOSPCD     PIC X(7).
      *����¦����������ǯ��(HI�쥳���ɤ�ǯ��)
           03  HENREI-K-SYORIYM    PIC X(6).
      *
           03  HENREI-K-RECV-TOTAL-CNT
                                   PIC 9(5).
      *
           03  HENREI-K-RECV-PATIENT-CNT
                                   PIC 9(5).
      *
           03  HENREI-K-RECV-GO-CNT 
                                   PIC 9(5).
      *
           03  HENREI-K-RECV-GO-TEN
                                   PIC 9(10).
      *ü���ɣ�
           03  HENREI-K-TERMID     PIC X(64).
      *���ڡ��졼���ɣ�
           03  HENREI-K-OPID       PIC X(16).
      *����ǯ����
           03  HENREI-K-CREYMD     PIC X(08).
      *����ǯ����
           03  HENREI-K-UPYMD      PIC X(08).
      *��������
           03  HENREI-K-UPHMS      PIC X(06).