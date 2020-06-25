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
      *    �����ݸ�����    �� COPY  (CPPTHKNINF.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     ¿��Ǽ       02/02/07  ��ʼ���ǯ�����ɲ�
      * 05.00.00     NACL-¿��Ǽ  20/04/07  �����ɲ�
      ************************************************************
      *01  PTHKNINF-REC.
      *����
           03  PTHKN-KEY.
      *���ŵ��أɣ�
               05  PTHKN-HOSPNUM            PIC 9(2).
      *���ԣɣ�
               05  PTHKN-PTID              PIC  9(10).
      *�ݸ��ɣ�
               05  PTHKN-HKNID             PIC  9(10).
      *�ݸ��ֹ�
           03  PTHKN-HKNNUM                PIC  X(03).
      *
           03  PTHKN-HKNJA-G.
      *�ݸ����ֹ�
               05  PTHKN-HKNJANUM          PIC  X(08).
      *���ݸ���̾
           03  PTHKN-HIHKNJANAME           PIC  X(100).
      *�ܿͲ�²��ʬ
           03  PTHKN-HONKZKKBN             PIC  X(01).
      *�����ʬ
           03  PTHKN-HOJOKBN               PIC  X(01).
      *��³��ʬ
           03  PTHKN-CONTKBN               PIC  X(01).
      *����
           03  PTHKN-KIGO                  PIC  X(80).
      *�ֹ�
           03  PTHKN-NUM                   PIC  X(80).
      *�ֹ����
           03  PTHKN-EDABAN                PIC  X(02).
      *��ʼ���ǯ����
           03  PTHKN-SKKGETYMD             PIC  X(08).
      *Ŭ�ѳ���ǯ����
           03  PTHKN-TEKSTYMD              PIC  X(08).
      *Ŭ�ѽ�λǯ����
           03  PTHKN-TEKEDYMD              PIC  X(08).
      *��ǧǯ����
           03  PTHKN-KAKUNINYMD            PIC  X(08).
      *�����ʬ
           03  PTHKN-SAKJOKBN              PIC  X(01).
      *ü���ɣ�
           03  PTHKN-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTHKN-OPID                  PIC  X(16).
      *����ǯ����
           03  PTHKN-CREYMD                PIC  X(08).
      *����ǯ����
           03  PTHKN-UPYMD                 PIC  X(08).
      *��������
           03  PTHKN-UPHMS                 PIC  X(06).