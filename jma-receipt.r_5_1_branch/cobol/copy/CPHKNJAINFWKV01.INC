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
      *    ������ݸ���           COPY (CPHKNJAINFWK.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HKNJAINFWK-REC.
      *������ʬ
           03  HKNJAWK-SYORIKBN              PIC  X(01).
      *���ŵ��أɣ�
           03  HKNJAWK-HOSPID                PIC  X(24).
      *�ݸ����ֹ�
           03  HKNJAWK-HKNJANUM              PIC  X(08).
      *�ݸ���̾
           03  HKNJAWK-HKNJANAME             PIC  X(200).
      *�ݸ���̾ά�Σ�
           03  HKNJAWK-HKNJANAME-TAN1        PIC  X(100).
      *�ݸ���̾ά�Σ�
           03  HKNJAWK-HKNJANAME-TAN2        PIC  X(40).
      *�ݸ���̾ά�Σ�
           03  HKNJAWK-HKNJANAME-TAN3        PIC  X(40).
      *�ݸ��ֹ�
           03  HKNJAWK-HKNNUM                PIC  X(03).
      *���ճ����ܿ͡������
           03  HKNJAWK-HON-GAIKYURATE-G.
               05  HKNJAWK-HON-GAIKYURATE    PIC  9(03).
      *���ճ����ܿ͡�������
           03  HKNJAWK-HON-NYUKYURATE-G.
               05  HKNJAWK-HON-NYUKYURATE    PIC  9(03).
      *���ճ��ʲ�²�������
           03  HKNJAWK-KZK-GAIKYURATE-G.
               05  HKNJAWK-KZK-GAIKYURATE    PIC  9(03).
      *���ճ��ʲ�²��������
           03  HKNJAWK-KZK-NYUKYURATE-G.
               05  HKNJAWK-KZK-NYUKYURATE    PIC  9(03).
      *͹���ֹ�
           03  HKNJAWK-POST                  PIC  X(07).
      *����
           03  HKNJAWK-ADRS                  PIC  X(200).
      *��������
           03  HKNJAWK-BANTI                 PIC  X(200).
      *�����ֹ�
           03  HKNJAWK-TEL                   PIC  X(15).
      *����
           03  HKNJAWK-KIGO                  PIC  X(80).
      *��ư��ʬ
           03  HKNJAWK-IDOKBN                PIC  X(01).
      *��ưǯ����
           03  HKNJAWK-IDOYMD                PIC  X(08).
      *ü���ɣ�
           03  HKNJAWK-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  HKNJAWK-OPID                  PIC  X(16).
      *����ǯ����
           03  HKNJAWK-CREYMD                PIC  X(08).
      *����ǯ����
           03  HKNJAWK-UPYMD                 PIC  X(08).
      *��������
           03  HKNJAWK-UPHMS                 PIC  X(06).