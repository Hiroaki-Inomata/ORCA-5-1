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
      *  01.00.01    ����         10/02/23  �õ������ɲ�
      *  01.00.02    ����         12/02/10  ����Ŭ�ѳ���ǯ�����ɲ�
      *  01.00.03    ���         18/05/02  ������ݰ�����ô�����ִ��б�
      *                           18/05/10  �ݸ���ݰ�����ô�⵭���ִ��б�
      *  01.00.04    ���         19/02/01  �������򥳥��Ⱥ��
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *���ŵ��أɣ�
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *���Ͻ�
               05  MF100-DSPSEQ          PIC  9(03).
      *�ݸ����ֹ�        
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *���ʻ�̾(��������)
               05  MF100-KANANAME-KEY    PIC  X(50).
      *�����ֹ�(��������)
               05  MF100-PTNUM-KEY       PIC  X(20).
      *�����
               05  MF100-TEISYUTUSAKI    PIC  X(01).
      *��ô���ֹ�
               05  MF100-FTNJANUM        PIC  X(20).
      *���������ֹ�
               05  MF100-JKYSNUM         PIC  X(20).
      *�����ֹ�
               05  MF100-PTNUM           PIC  X(20).
      *����ǯ��
               05  MF100-SRYYM           PIC  X(06).
      *�ݸ����ֹ�KEY��
               05  MF100-HKNJANUM-KEY2   PIC  X(08).
      *���Ų�
               05  MF100-SRYKA           PIC  X(02).
      *������ʬ
               05  MF100-NYUGAIKBN       PIC  X(01).
      *�쥻�ץȼ���
               05  MF100-RECESYUBETU     PIC  X(04).
      *Ŭ�ѳ���ǯ����
               05  MF100-TEKSTYMD        PIC  X(08). 
      *���ԣɣ�
           03  MF100-PTID                PIC  9(10).
      *���ʻ�̾
           03  MF100-KANANAME            PIC  X(100).
      *������̾
           03  MF100-NAME                PIC  X(100).
      *��ǯ����
           03  MF100-BIRTHDAY            PIC  9(08).
      *����
           03  MF100-SEX                 PIC  X(01).
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
      *����������
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *�����������
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *������ô��
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
      *�����Ŭ�ѳ���ǯ����
           03  MF100-KOHTEKSTYMD         PIC  X(08).
      *�����
           03  MF100-TOTALTEN-G          PIC  9(09).
           03  MF100-JNISSU-G            PIC  9(09).
           03  MF100-FTNMONEY-G          PIC  9(09).
           03  MF100-SHOKUJIINF-G.
               05  MF100-SHOKUJINISSU-G      PIC  9(02).
               05  MF100-SHOKUJIRYOYOHI-G    PIC  9(09).
               05  MF100-SHOKUJIFTN-G        PIC  9(09).
      *¾ˡ����(����������������Į¼�ѹ��ǡ����ޤ�)
           03  MF100-T-KOHNUM            PIC  X(03).
           03  MF100-T-FTNJANUM          PIC  X(20).
           03  MF100-T-JKYSNUM           PIC  X(20).
           03  MF100-T-JNISSU            PIC  9(02).
           03  MF100-T-TOTALTEN          PIC  9(09).
           03  MF100-T-FTNMONEY          PIC  9(09).
           03  MF100-T-KOHTEKSTYMD       PIC  X(08).
      *
           03  MF100-T-SHOKUJIINF.
               05  MF100-T-SHOKUJINISSU      PIC  9(02).
               05  MF100-T-SHOKUJIRYOYOHI    PIC  9(09).
               05  MF100-T-SHOKUJIFTN        PIC  9(09).
      *�õ�����
           03  MF100-TOKKI                   PIC  X(02)  OCCURS 4.
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
           03  MF100-OKIKAE-TBL        OCCURS 4.
      *������ô��
               05  MF100-OKIKAE-FTN      PIC 9(07).
