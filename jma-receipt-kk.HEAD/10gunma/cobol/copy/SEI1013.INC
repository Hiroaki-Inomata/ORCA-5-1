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
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         15/01/06  ʿ������ǯ��������б�
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *���ŵ��أɣ�
               05  MF100-HOSPNUM         PIC  9(02).
      *������ʬ
               05  MF100-NYUGAIKBN       PIC  X(01).
      *��Į¼�ֹ�
               05  MF100-CITYNUM         PIC  X(02).
      *�����ȥ���
               05  MF100-SORTKEY.
                   07  MF100-RATE        PIC  9(01).
                   07  MF100-KBN         PIC  9(01).
      *�ȹ礻��ʬ
               05  MF100-COMBIKBN        PIC  X(02).
      *����ݼ�����ֹ�
               05  MF100-JKYSNUM         PIC  X(20).
      *���ݸ��ݵ��ճ��
               05  MF100-KYURATE-KEY     PIC  9(03).
      *�������ô���ֹ�
               05  MF100-FTNJANUM        PIC  X(08).
      *�����ֹ�
               05  MF100-PTNUM           PIC  X(20).
      *���Ų�
               05  MF100-SRYKA           PIC  X(02).
      *�ݸ����ֹ�
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *Ŭ�ѳ���ǯ����
               05  MF100-TEKSTYMD        PIC  X(08). 
      *����ǯ��
               05  MF100-SRYYM           PIC  X(06).
      *���ԣɣ�
           03  MF100-PTID                PIC  9(10).
      *�쥻�ץȼ���
           03  MF100-RECESYUBETU         PIC  X(04).
      *������ʧ����
           03  MF100-TEISYUTUSAKI        PIC  9(01).
      *������̾
           03  MF100-NAME                PIC  X(100).
      *���ʻ�̾
           03  MF100-KANANAME            PIC  X(50).
      *��Į¼�ֹ棲
           03  MF100-CITYNUM2            PIC  X(06).
      *��ǯ����
           03  MF100-BIRTHDAY            PIC  9(08).
      *����
           03  MF100-SEX                 PIC  9(01).
      *ǯ��
           03  MF100-AGE                 PIC  9(03).
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
      *Ϸ���ݷ����
           03  MF100-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  MF100-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  MF100-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  MF100-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  MF100-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  MF100-RJNJKYSNUM      PIC  X(07).
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
      *�����Ǥ�յ��ն�ʬ
               05  MF100-KOHNINIKYUKBN   PIC  X(01).
      *����ݷ��
               05  MF100-KOHKENSU        PIC  9(03).
      *����ݥ쥻�ץ������ʬ
               05  MF100-RECESKYKBN      PIC  X(01).
      *����������
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *�����������
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *��ô��۾���
           03  MF100-FTNMONEYINF.
               05  MF100-FTNMONEY        PIC  9(08).
      *���ް�����ô�����
           03  MF100-YKZFTNINF.
               05  MF100-YKZFTN          PIC  9(05).
      *�������������
           03  MF100-SHOKUJIINF.
      *��������
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *����������
               05  FM100-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *����ʬ���ް�����ô��
           03  MF100-KOHYKZFTN           PIC  9(05).
      *����Ŭ�ѳ�����
           03  MF100-KOHTEKSTYMD         PIC  X(08).
      *Ϸ�Ͱ�����ô��
           03  MF100-RJNFTN              PIC  9(07).
      *������
           03  MF100-SEIKYUGK            PIC  9(09).
      *����ʬ
           03  MF100-WARIAIKBN           PIC  9(01).
      *Ĺ��
           03  MF100-TYOKI               PIC  9(01).
      *����
           03  MF100-ZAITAKU             PIC  9(01).
      *��������
           03  MF100-TOTALTEN2           PIC  9(09).
      *
           03  MF100-SEIKYU-FLG          PIC  9(01).
      *�в�����
           03  MF100-KEIKA               PIC  9(01).
      *�����
           03  MF100-TEISHO              PIC  9(01).
      *��������񣴲��ܰʹ�
           03  MF100-KOGAKU4             PIC  9(01).
      *��Ⱦ
           03  MF100-KOUHAN              PIC  9(01).
      *���󥳡���
           03  MF100-TOKURAN             PIC  X(02).
      *���ͥ�����
           03  MF100-BIKO                PIC  X(02).
      *�ݸ�����
           03  MF100-HKNINF.
               05  MF100-H-JNISSU        PIC  9(03).
               05  MF100-H-TOTALTEN      PIC  9(08).
               05  MF100-H-FTNMONEY      PIC  9(09).
               05  MF100-H-YKZFTN        PIC  9(05).
      *�ݸ���������
           03  MF100-H-SHOKUJIINF.
               05  MF100-H-SHOKUJINISSU    PIC  9(02).
               05  MF100-H-SHOKUJIRYOYOHI  PIC  9(09).
               05  MF100-H-SHOKUJIFTN      PIC  9(09).
      *����¾�θ������
           03  MF100-ETCKOHINF OCCURS 2.
               05  MF100-ENISSU          PIC  9(02).
               05  MF100-EJNISSU         PIC  9(02).
               05  MF100-EKOHNUM         PIC  X(03).
               05  MF100-EKOHFTNJANUM    PIC  X(08).
               05  MF100-EKOHJKYSNUM     PIC  X(07).
               05  MF100-ETENSU          PIC  9(08).
               05  MF100-ETOTALTEN       PIC  9(08).
               05  MF100-EITBFTN         PIC  9(08).
               05  MF100-EETCFTN         PIC  9(07).
               05  MF100-EYKZFTN         PIC  9(07).
               05  MF100-E-SHOKUJIINF.
                   07  MF100-E-SHOKUJINISSU    PIC  9(02).
                   07  MF100-E-SHOKUJIRYOYOHI  PIC  9(09).
                   07  MF100-E-SHOKUJIFTN      PIC  9(09).
      *���������
           03  MF100-KOGAKU              PIC  X(01).
      *������ܸ�ʪ���ճ���
           03  MF100-GENBUTU             PIC  9(01).
