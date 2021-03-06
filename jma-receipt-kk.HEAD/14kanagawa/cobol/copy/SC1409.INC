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
      * Maj/Min/Rev  ������   ����        ����
      *  01.00.01    ����     15/12/10    �õ��������ǿ��ɲ�
      ********************************************************************
       01  SC09.
      *��������
           03  SC09-PRTYM              PIC  X(16).
      *���ŵ��إ�����
           03  SC09-HOSPCDN            PIC  X(40).
      *�ݸ���ʬ
           03  SC09-HKNKBN-TBL OCCURS 4.
               05  SC09-HKNKBN         PIC  X(02).
      *ñ��ʻ�Ѷ�ʬ
           03  SC09-HEIKBN-TBL OCCURS 3.
               05  SC09-HEIKBN         PIC  X(02).
      *�ܿͲ�²��ʬ
           03  SC09-HONKZKKBN-TBL OCCURS 5.
               05  SC09-HONKZKKBN      PIC  X(02).
      *Ϸ�ͻ�Į¼�ֹ�
           03  SC09-RJNFTNJANUM        PIC  X(16).
      *������ô���ֹ�
           03  SC09-FTNJANUM           PIC  X(16) OCCURS 2.
      *Ϸ�ͼ�����ֹ�
           03  SC09-RJNJKYSNUM         PIC  X(14).
      *���������ֹ�
           03  SC09-JKYSNUM            PIC  X(14) OCCURS 2.
      *�ݸ����ֹ�
           03  SC09-HKNJANUM           PIC  X(16).
      *���ݸ��Ծڵ����ֹ�
           03  SC09-KIGO               PIC  X(34).
           03  SC09-BANGO              PIC  X(34).
      *�����ֹ�
           03  SC09-PTNUM              PIC  X(20).
      *����Ի�̾
           03  SC09-NAME               PIC  X(30).
      *��ǯ����
           03  SC09-BIRTHDAY           PIC  X(08).
      *����
           03  SC09-SEX-TBL OCCURS 2.
               05  SC09-SEX            PIC  X(02).
      *��ǯ������ǯ��
           03  SC09-NENGO-TBL OCCURS 4.
               05  SC09-NENGO          PIC  X(02).
      *�õ�����
      *=== �õ��������ǿ��ɲ� by hagino(2015/12/10) start
      *     03  SC09-TOKKI              PIC  X(08).
           03  SC09-TOKKI-TBL          OCCURS 3.
               05  SC09-TOKKI          PIC  X(08).
      *=== �õ��������ǿ��ɲ� by hagino(2015/12/10) end
           03  SC09-ZAISOU             PIC  X(04).
           03  SC09-ZAIMARU            PIC  X(02).
      *���ŵ��ؽ����
           03  SC09-ADRS               PIC  X(100).
      *���ŵ���̾��
           03  SC09-HOSPNAME           PIC  X(100).
      *������̾
           03  SC09-KAISETUNAME        PIC  X(80).
      *����̾
           03  SC09-BYOMEI-TBL.
               05  SC09-BYOMEI         PIC  X(54) OCCURS 5.
               05  SC09-SRYYMD         PIC  X(24) OCCURS 5.
               05  SC09-TEKIYO         PIC  X(52) OCCURS 35.
      *���ż�����
           03  SC09-NISSU              PIC  X(02).
      *��������
           03  SC09-TENSU              PIC  X(07).
      *���ް�����ô��
           03  SC09-YKZFTN             PIC  X(07).
      *������ô��
           03  SC09-ITBFTN             PIC  X(07).
      *������ô��ʸ����
           03  SC09-ITBFTN2            PIC  X(07).
      *��������(SC1410�ѥ��ꥢ)
           03  SC09-SHOKUJI-TBL.
      *��������
               05  SC09-SNISSU         PIC  X(02).
      *������
               05  SC09-SKINGK         PIC  X(07).
      *ɸ����û��
               05  SC09-SFTNGK         PIC  X(07).
