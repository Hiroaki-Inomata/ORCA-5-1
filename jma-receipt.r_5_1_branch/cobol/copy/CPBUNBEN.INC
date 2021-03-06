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
      *    ʬ�ڴ���                   COPY  (CPBUNBEN.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  BUNBEN-REC.
      *����
           03  BUNBEN-KEY.
      *���ŵ��أɣ�
               05  BUNBEN-HOSPNUM             PIC  9(02).
      *���ԣɣ�
               05  BUNBEN-PTID                PIC  9(10).
      *������ʬ
               05  BUNBEN-NYUGAIKBN           PIC  X(01).
      *����ǯ��
               05  BUNBEN-SKYYM               PIC  X(06).
      *���������ֹ�
               05  BUNBEN-RRKNUM              PIC  9(07).
      *�����ֹ�
           03  BUNBEN-PTNUM                   PIC  X(20).
      *�����ʬ
           03  BUNBEN-SKYKBN                  PIC  X(01).
      *ľ�ܻ�ʧ�������Ѷ�ʬ
           03  BUNBEN-RIYOUKBN                PIC  X(01).
      *ʬ�ڶ�ʬ
           03  BUNBEN-BUNBENKBN               PIC  9(01).
      *�ǡ���������ʬ
           03  BUNBEN-TEISYUTUSAKI            PIC  X(01).
      *�������ٶ�ʬ
           03  BUNBEN-KANYUKBN                PIC  X(01).
      *�ܿͲ�²��ʬ
           03  BUNBEN-HONKZKKBN               PIC  X(01).
      *�ݸ����ֹ�
           03  BUNBEN-HKNJANUM                PIC  X(08).
      *���ݸ��Ծڵ���
           03  BUNBEN-KIGO                    PIC  X(40).
      *���ݸ��Ծ��ֹ�
           03  BUNBEN-NUM                     PIC  X(40).
      *ǥ�ػ�̾�ʥ��ʻ�̾��
           03  BUNBEN-NAME                    PIC  X(40).
      *���۽���
           03  BUNBEN-ZAIWEEK                 PIC  9(02).
      *�л�ǯ����
           03  BUNBEN-BIRTHDAY                PIC  X(08).
      *�໺̵ͭ��ʬ
           03  BUNBEN-SIZANKBN                PIC  X(01).
      *�л���
           03  BUNBEN-BIRTHCNT                PIC  9(01).
      *��������
           03  BUNBEN-NYUIN-DAY               PIC  9(03).
      *���ʰ���������ٶ�ʬ
           03  BUNBEN-SANKAKBN                PIC  X(01).
      *���پ���
           03  BUNBEN-MEISAI-INF.
      *������
               05  BUNBEN-NYU-MONEY           PIC S9(08).
      *��������
               05  BUNBEN-RMSGK-MONEY         PIC S9(08).
      *ʬ�ڲ����
               05  BUNBEN-KAIJO-MONEY         PIC S9(08).
      *ʬ����
               05  BUNBEN-BUNBEN-MONEY        PIC S9(08).
      *�����������ݰ���
               05  BUNBEN-SHIN-MONEY          PIC S9(08).
      *������������
               05  BUNBEN-KNS-MONEY           PIC S9(08).
      *���֡�������
               05  BUNBEN-SYC-MONEY           PIC S9(08).
      *���ʰ����������
               05  BUNBEN-SNK-MONEY           PIC S9(08).
      *����¾
               05  BUNBEN-ETC-MONEY           PIC S9(08).
      *������ô����
               05  BUNBEN-ICBFTN-MONEY        PIC S9(08).
      *ǥ�ع����ô��
               05  BUNBEN-NINPUFTN-MONEY      PIC S9(08).
      *���������
               05  BUNBEN-DAIRI-MONEY         PIC S9(08).
      *���پ���ʺ������
           03  BUNBEN-MEISAI-R   REDEFINES  BUNBEN-MEISAI-INF.
               05  BUNBEN-MONEY               PIC S9(08)  OCCURS 12.
      *����
           03  BUNBEN-BIKOU                   PIC  X(60).
      *������ʬ
           03  BUNBEN-UPDKBN                  PIC  X(01).
      *�ݸ����ֹ�
           03  BUNBEN-HKNJANUM-S              PIC  X(08).
      *�������ʬ�ʣ������������ᡡ���������������
           03  BUNBEN-TEISYUTUBIKBN           PIC  X(01).
      *ü���ɣ�
           03  BUNBEN-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  BUNBEN-OPID                    PIC  X(16).
      *����ǯ����
           03  BUNBEN-CREYMD                  PIC  X(08).
      *����ǯ����
           03  BUNBEN-UPYMD                   PIC  X(08).
      *��������
           03  BUNBEN-UPHMS                   PIC  X(06).
