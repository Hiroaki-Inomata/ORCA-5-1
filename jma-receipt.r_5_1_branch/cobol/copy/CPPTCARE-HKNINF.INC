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
      *    ���Բ���ݸ�����         COPY  (CPPTCARE-HKNINF.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTCARE-HKNINF-REC.
      *����
           03  PTCRHKN-KEY.
      *���ŵ��أɣ�
               05  PTCRHKN-HOSPNUM         PIC  9(02).
      *���ԣɣ�
               05  PTCRHKN-PTID            PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  PTCRHKN-TEKSTYMD        PIC  X(08).
      *Ŭ�ѽ�λǯ����
           03  PTCRHKN-TEKEDYMD            PIC  X(08).
      *�ݸ����ֹ�
           03  PTCRHKN-HKNJANUM            PIC  X(06).
      *���ݸ���̾
           03  PTCRHKN-HIHKNJANUM          PIC  X(10).
      *ü���ɣ�
           03  PTCRHKN-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTCRHKN-OPID                PIC  X(16).
      *����ǯ����
           03  PTCRHKN-CREYMD              PIC  X(08).
      *����ǯ����
           03  PTCRHKN-UPYMD               PIC  X(08).
      *��������
           03  PTCRHKN-UPHMS               PIC  X(06).