      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *���ŵ��أɣ�
               05  MF100-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  MF100-SRYYM           PIC  9(06).
      *������ʬ
               05  MF100-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  MF100-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  MF100-RECEKA          PIC  X(02).
      *������ʧ����
               05  MF100-TEISYUTUSAKI    PIC  9(01).
      *�쥻�ץȼ���
               05  MF100-RECESYUBETU     PIC  X(04).
      *�ݸ����ֹ�        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  MF100-HOJOKBN-KEY     PIC  X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
               05  MF100-KOHID-KEY       PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  MF100-TEKSTYMD        PIC  X(08). 
      *ʬ���ʬ
               05  MF100-RECEIPT-KBN     PIC  X(10).
      *�����ֹ�
           03  MF100-PTNUM               PIC  X(20).
      *������̾
           03  MF100-NAME                PIC  X(100).
      *���ʻ�̾
           03  MF100-KANANAME            PIC  X(100).
      *ǯ��
           03  MF100-AGE                 PIC  9(03).
      *��ǯ����
           03  MF100-BIRTHDAY            PIC  9(08).
      *����
           03  MF100-SEX                 PIC  X(01).
      *���ݸ�����
           03  MF100-HKN.
      *���ݸ����ݸ��ɣ�
               05  MF100-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  MF100-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  MF100-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  MF100-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  MF100-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  MF100-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  MF100-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  MF100-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF100-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF100-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  MF100-CERTNUM         PIC  X(03).
      *�������
           03  MF100-KOHINF.
      *������ݸ��ɣ�
               05  MF100-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  MF100-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  MF100-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  MF100-KOHFTNJANUM     PIC  X(20).
      *����ݼ�����ֹ�
               05  MF100-KOHJKYSNUM      PIC  X(20).
      *����������
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *�����������
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(09).
      *�������������
           03  MF100-SHOKUJIINF.
      *��������
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *����������
               05  MF100-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF100-SHOKUJIFTN      PIC  9(09).