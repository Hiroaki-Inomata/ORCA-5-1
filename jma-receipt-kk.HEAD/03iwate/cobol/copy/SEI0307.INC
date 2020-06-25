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
      ************************************************************
      *          ������������ʣ�̼��쥻�� COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    ����         16/04/14  �����������б�
      *  04.08.02    ����         16/10/07  �õ������ɲ�
      *  04.08.03    ����         17/03/08  ������ֹ楽�����ѥ����ɲ�
      *  04.08.04    ����         18/05/09  ������ݰ�����ô�����ִ��б�
      *                           18/05/10  �ݸ���ݰ�����ô�����ִ��б�
      *  04.08.05    ���         18/10/10  �������򥳥��Ⱥ��
      ************************************************************
      *01  MF101-REC.
           03  MF101-KEY.
      *���ŵ��أɣ�
      *         05  MF101-HOSPID          PIC  X(24).
               05  MF101-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  MF101-SRYYM           PIC  X(06).
      *���Ͻ�
               05  MF101-DSPSEQ          PIC  9(03).
      *�ݸ����ֹ�        
               05  MF101-HKNJANUM-KEY    PIC  X(08).
      *���ʻ�̾(��������)
               05  MF101-KANANAME-KEY    PIC  X(50).
      *�����ֹ�(��������)
               05  MF101-PTNUM-KEY       PIC  X(20).
      *������ֹ�(��������)
               05  MF101-KOHJKYSNUM-KEY  PIC  X(11).
      *������ʬ
               05  MF101-NYUGAIKBN       PIC  X(01).
      *����ݼ�����ֹ�
               05  MF101-JKYSNUM         PIC  X(08).
      *�����ֹ�
               05  MF101-PTNUM           PIC  X(20).
      *������ʧ����
               05  MF101-TEISYUTUSAKI    PIC  9(01).
      *Ŭ�ѳ���ǯ����
               05  MF101-TEKSTYMD        PIC  X(08). 
      *���Ų�
               05  MF101-SRYKA           PIC  X(02).
      *�쥻�ץȼ���
               05  MF101-RECESYUBETU     PIC  X(04).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  MF101-HOJOKBN-KEY     PIC  X(01).
      *���ԣɣ�
           03  MF101-PTID                PIC  9(10).
      *���ʻ�̾
           03  MF101-KANANAME            PIC  X(100).
      *������̾
           03  MF101-NAME                PIC  X(100).
      *ǯ��
           03  MF101-AGE                 PIC  9(03).
      *��ǯ����
           03  MF101-BIRTHDAY            PIC  9(08).
      *����
           03  MF101-SEX                 PIC  X(01).
      *���ݸ�����
           03  MF101-HKN.
      *���ݸ����ݸ��ɣ�
               05  MF101-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  MF101-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  MF101-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  MF101-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  MF101-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  MF101-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  MF101-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  MF101-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF101-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF101-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  MF101-CERTNUM         PIC  X(03).
      *Ϸ���ݷ����
           03  MF101-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  MF101-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  MF101-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  MF101-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  MF101-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  MF101-RJNJKYSNUM      PIC  X(07).
      *�������
           03  MF101-KOHINF.
      *������ݸ��ɣ�
               05  MF101-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  MF101-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  MF101-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  MF101-KOHFTNJANUM     PIC  X(20).
      *����ݼ�����ֹ�
               05  MF101-KOHJKYSNUM      PIC  X(20).
      *�����Ǥ�յ��ն�ʬ
               05  MF101-KOHNINIKYUKBN   PIC  X(01).
      *����ݷ��
               05  MF101-KOHKENSU        PIC  9(03).
      *����ݥ쥻�ץ������ʬ
               05  MF101-RECESKYKBN      PIC  X(01).
      *������ȹ礻��ʬ
               05  MF101-COMBIKBN        PIC  9(01).
      *������ȹ礻���
               05  MF101-COMBIKENSU      PIC  9(03).
      *������ȹ礻��ô��
               05  MF101-COMBIFTN        PIC  9(09).
      *����������
           03  MF101-JNISSUINF.
               05  MF101-JNISSU          PIC  9(02).
      *�����������
           03  MF101-TOTALTENINF.
               05  MF101-TOTALTEN        PIC  9(08).
      *
           03  MF101-FTNMONEYINF.
               05  MF101-FTNMONEY        PIC  9(09).
      *�������������
           03  MF101-SHOKUJIINF.
      *��������
               05  MF101-SHOKUJINISSU    PIC  9(02).
      *����������
               05  MF101-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF101-SHOKUJIFTN      PIC  9(09).
      *���ް�����ô�����
           03  MF101-YKZFTNINF.
               05  MF101-YKZFTN          PIC  9(05).
      *Ĺ���������
           03  MF101-TYOKI               PIC  9(01).
      *����ե饰�ʺ����ǤΤߡ�
           03  MF101-ZAITAKU             PIC  9(01).
      *�ե饰��̤���ػ����������֡�
           03  MF101-FLG-TBL  OCCURS  5.
               05  MF101-FLG             PIC  9(01).
      *����¾�θ������
           03  MF101-ETCKOHINF OCCURS 2.
               05  MF101-ENISSU          PIC  9(02).
               05  MF101-EJNISSU         PIC  9(02).
               05  MF101-EKOHNUM         PIC  X(03).
               05  MF101-EKOHFTNJANUM    PIC  X(08).
               05  MF101-EKOHJKYSNUM     PIC  X(07).
               05  MF101-ETENSU          PIC  9(08).
               05  MF101-ETOTALTEN       PIC  9(08).
               05  MF101-EITBFTN         PIC  9(08).
               05  MF101-ESHOKUJINISSU   PIC  9(02).
               05  MF101-ESHOKUJIRYOYOHI PIC  9(09).
               05  MF101-ESHOKUJIFTN     PIC  9(09).
           03  MF101-TOKKI-TBL  OCCURS  5.
               05  MF101-TOKKI           PIC  X(02).
      *���ݸ���ô�⡧���ִ����
           03  MF101-OKIKAE-HKN          PIC 9(07).
      *�֡ʺƷǲ��ˡ����ִ����
           03  MF101-OKIKAE-SAIKEI1      PIC 9(07).
      *�֡ʺƷǾ�ˡ����ִ����
           03  MF101-OKIKAE-SAIKEI2      PIC 9(07).
      *�֡ʺƷǣ��ˡ����ִ����
           03  MF101-OKIKAE-SAIKEI3      PIC 9(07).
      *�֡ʺƷǣ��ˡ����ִ����
           03  MF101-OKIKAE-SAIKEI4      PIC 9(07).
      *���������ô���ִ�
           03  MF101-OKIKAE-TBL          OCCURS 4.
      *������ô��
               05  MF101-OKIKAE-FTN      PIC 9(07).