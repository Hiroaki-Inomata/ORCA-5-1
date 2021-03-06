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
       01  SC05.
      *��������
           03  SC05-PRTYM              PIC  X(16).
      *���ŵ��إ�����
           03  SC05-HOSPCDN            PIC  X(40).
      *�ݸ���ʬ
           03  SC05-HKNKBN-TBL OCCURS 4.
               05  SC05-HKNKBN         PIC  X(02).
      *ñ��ʻ�Ѷ�ʬ
           03  SC05-HEIKBN-TBL OCCURS 2.
               05  SC05-HEIKBN         PIC  X(02).
      *�ܿͲ�²��ʬ
           03  SC05-HONKZKKBN-TBL OCCURS 5.
               05  SC05-HONKZKKBN      PIC  X(02).
      *Ϸ�ͻ�Į¼�ֹ�
           03  SC05-RJNFTNJANUM        PIC  X(16).
      *������ô���ֹ�
           03  SC05-FTNJANUM           PIC  X(16) OCCURS 2.
      *Ϸ�ͼ�����ֹ�
           03  SC05-RJNJKYSNUM         PIC  X(14).
      *���������ֹ�
           03  SC05-JKYSNUM            PIC  X(14) OCCURS 2.
      *�ݸ����ֹ�
           03  SC05-HKNJANUM           PIC  X(16).
      *���ݸ��Ծڵ����ֹ�
           03  SC05-KIGO               PIC  X(34).
           03  SC05-BANGO              PIC  X(34).
      *�����ֹ�
           03  SC05-PTNUM              PIC  X(20).
      *����Ի�̾
           03  SC05-KANANAME           PIC  X(40).
           03  SC05-NAME               PIC  X(30).
      *��ǯ����
           03  SC05-BIRTHDAY           PIC  X(08).
      *����
           03  SC05-SEX-TBL OCCURS 2.
               05  SC05-SEX            PIC  X(02).
      *��ǯ������ǯ��
           03  SC05-NENGO-TBL OCCURS 4.
               05  SC05-NENGO          PIC  X(02).
      *�õ�����
           03  SC05-TOKKI              PIC  X(08).
           03  SC05-ZAISOU             PIC  X(04).
           03  SC05-ZAIMARU            PIC  X(02).
      *���ŵ��ؽ����
           03  SC05-ADRS               PIC  X(100).
      *���ŵ���̾��
           03  SC05-HOSPNAME           PIC  X(100).
      *������̾
           03  SC05-KANRINAME          PIC  X(80).
      *����̾�ơ��֥�
           03  SC05-BYOMEI-TBL.
               05  SC05-BYOMEI   PIC X(54)   OCCURS  5 TIMES.
               05  SC05-SRYYMD   PIC X(24)   OCCURS  5 TIMES.
               05  SC05-TENKI1   PIC X(04)   OCCURS  5 TIMES.
               05  SC05-TENKI2   PIC X(04)   OCCURS  5 TIMES.
               05  SC05-TENKI3   PIC X(04)   OCCURS  5 TIMES.
               05  SC05-TEKIYO   PIC X(52)   OCCURS  35 TIMES.
      *���ż�����
           03  SC05-NISSU             PIC  X(02) OCCURS 3.
      *��������
           03  SC05-TENSU             PIC  X(07) OCCURS 3.
      *������ô��
           03  SC05-KOHFTN            PIC  X(07) OCCURS 3.
      *��������
           03  SC05-SHOKUJI-TBL OCCURS 3.
      *��������
               05  SC05-SNISSU         PIC  X(02).
      *������
               05  SC05-SKINGK         PIC  X(07).
      *ɸ����û��
               05  SC05-SFTNGK         PIC  X(07).
