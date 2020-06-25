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
      *    �ݸ���          COPY (CPHKNJA.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HKNJAINF-REC.
      *���ŵ��أɣ�
           03  HKNJA-HOSPNUM                  PIC 9(2).
      *�ݸ����ֹ�
           03  HKNJA-HKNJANUM                PIC  X(08).
      *�ݸ���̾
           03  HKNJA-HKNJANAME               PIC  X(200).
      *�ݸ���̾ά�Σ�
           03  HKNJA-HKNJANAME-TAN1          PIC  X(100).
      *�ݸ���̾ά�Σ�
           03  HKNJA-HKNJANAME-TAN2          PIC  X(40).
      *�ݸ���̾ά�Σ�
           03  HKNJA-HKNJANAME-TAN3          PIC  X(40).
      *�ݸ��ֹ�
           03  HKNJA-HKNNUM                  PIC  X(03).
      *���ճ����ܿ͡������
           03  HKNJA-HON-GAIKYURATE          PIC  9(03).
      *���ճ����ܿ͡�������
           03  HKNJA-HON-NYUKYURATE          PIC  9(03).
      *���ճ��ʲ�²�������
           03  HKNJA-KZK-GAIKYURATE          PIC  9(03).
      *���ճ��ʲ�²��������
           03  HKNJA-KZK-NYUKYURATE          PIC  9(03).
      *͹���ֹ�
           03  HKNJA-POST                    PIC  X(07).
      *����
           03  HKNJA-ADRS                    PIC  X(200).
      *��������
           03  HKNJA-BANTI                   PIC  X(200).
      *�����ֹ�
           03  HKNJA-TEL                     PIC  X(15).
      *����
           03  HKNJA-KIGO                    PIC  X(80).
      *��ư��ʬ
           03  HKNJA-IDOKBN                  PIC  X(01).
      *��ưǯ����
           03  HKNJA-IDOYMD                  PIC  X(08).
      *ü���ɣ�
           03  HKNJA-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  HKNJA-OPID                    PIC  X(16).
      *����ǯ����
           03  HKNJA-CREYMD                  PIC  X(08).
      *����ǯ����
           03  HKNJA-UPYMD                   PIC  X(08).
      *��������
           03  HKNJA-UPHMS                   PIC  X(06).