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
      *          ������������ʡ��� COPY (CPRMF04.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF04-REC.
           03  MF04-KEY.
      *���ŵ��أɣ�
               05  MF04-HOSPID          PIC  X(24).
      *�������ô���ֹ�
               05  MF04-FTNJANUM        PIC  X(08).
      *���Ų�
               05  MF04-SRYKA           PIC  X(02).
      *�����ֹ�
               05  MF04-PTNUM           PIC  X(20).
      *�ݸ����ֹ�        
               05  MF04-HKNJANUM-KEY    PIC  X(08).
      *Ŭ�ѳ���ǯ����
               05  MF04-TEKSTYMD        PIC  X(08). 
      *������ʬ
               05  MF04-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
           03  MF04-PTID                PIC  9(10).
      *������̾
           03  MF04-NAME                PIC  X(100).
      *��ǯ����
           03  MF04-BIRTHDAY            PIC  9(08).
      *���ݸ�����
           03  MF04-HKN.
      *���ݸ����ݸ��ɣ�
               05  MF04-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  MF04-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  MF04-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  MF04-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  MF04-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  MF04-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  MF04-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  MF04-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF04-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF04-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  MF04-CERTNUM         PIC  X(03).
      *Ϸ���ݷ����
           03  MF04-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  MF04-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  MF04-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  MF04-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  MF04-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  MF04-RJNJKYSNUM      PIC  X(07).
      *�������
           03  MF04-KOHINF.
      *������ݸ��ɣ�
               05  MF04-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  MF04-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  MF04-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  MF04-KOHFTNJANUM     PIC  X(08).
      *����ݼ�����ֹ�
               05  MF04-KOHJKYSNUM      PIC  X(20).
      *�����Ǥ�յ��ն�ʬ
               05  MF04-KOHNINIKYUKBN   PIC  X(01).
      *����ݷ��
               05  MF04-KOHKENSU        PIC  9(03).
      *����ݥ쥻�ץ������ʬ
               05  MF04-RECESKYKBN      PIC  X(01).
      *����������
           03  MF04-JNISSUINF.
      *������
               05  MF04-JNISSU          PIC  9(02).
      *�����������
           03  MF04-TOTALTENINF.
      *�������
               05  MF04-TOTALTEN        PIC  9(08).
      *��ô��۾���
           03  MF04-FTNMONEYINF.
      *��ô���
               05  MF04-FTNMONEY        PIC  9(09).
      *���ް�����ô�����
           03  MF04-YKZFTNINF.
      *���ް�����ô��
               05  MF04-YKZFTN          PIC  9(05).
