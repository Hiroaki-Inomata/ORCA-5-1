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
      *    �ݸ��ԡʲ���ݸ���      COPY (CPHKNJAINFCARE.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HKNJAINFCARE-REC.
      *�ݸ����ֹ�
           03  HKNJAC-HKNJANUM               PIC  X(08).
      *�ݸ���̾
           03  HKNJAC-HKNJANAME              PIC  X(200).
      *͹���ֹ�
           03  HKNJAC-POST                   PIC  X(07).
      *����
           03  HKNJAC-ADRS                   PIC  X(200).
      *��������
           03  HKNJAC-BANTI                  PIC  X(200).
      *�����ֹ�
           03  HKNJAC-TEL                    PIC  X(15).
      *��ư��ʬ
           03  HKNJAC-IDOKBN                 PIC  X(01).
      *��ưǯ����
           03  HKNJAC-IDOYMD                 PIC  X(08).
      *ü���ɣ�
           03  HKNJAC-TERMID                 PIC  X(16).
      *���ڡ��졼���ɣ�
           03  HKNJAC-OPID                   PIC  X(16).
      *����ǯ����
           03  HKNJAC-CREYMD                 PIC  X(08).
      *����ǯ����
           03  HKNJAC-UPYMD                  PIC  X(08).
      *��������
           03  HKNJAC-UPHMS                  PIC  X(06).