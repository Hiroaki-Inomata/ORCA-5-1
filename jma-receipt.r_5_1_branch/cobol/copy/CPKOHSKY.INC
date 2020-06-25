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
      *    �������������     COPY (CPKOHSKY.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.02.01    NACL-ƣ��    09/03/12  �ե饰���ǡ������ꥢ�ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ� 
      ************************************************************
       01  KOHSKY-REC.
           03  KOHSKY-KEY.
      *���ŵ��أɣ�
               05  KOHSKY-HOSPNUM          PIC 9(2).
      *����ǯ��
               05  KOHSKY-SRYYM           PIC  9(06).
      *������ʬ
               05  KOHSKY-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  KOHSKY-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  KOHSKY-RECEKA          PIC  X(02).
      *������ʧ����
               05  KOHSKY-TEISYUTUSAKI    PIC  9(01).
      *�쥻�ץȼ���
               05  KOHSKY-RECESYUBETU     PIC  X(04).
      *�ݸ����ֹ�        
               05  KOHSKY-HKNJANUM-KEY    PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  KOHSKY-HOJOKBN-KEY     PIC  X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
               05  KOHSKY-KOHID-KEY       PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  KOHSKY-TEKSTYMD        PIC  X(08). 
      *ʬ���ʬ
               05  KOHSKY-RECEIPT-KBN     PIC  X(10).
      *�����ֹ�
           03  KOHSKY-PTNUM               PIC  X(20).
      *���ʻ�̾
           03  KOHSKY-KANANAME            PIC  X(50).
      *������̾
           03  KOHSKY-NAME                PIC  X(100).
      *����
           03  KOHSKY-SEX                 PIC  X(01).
      *��ǯ����
           03  KOHSKY-BIRTHDAY            PIC  9(08).
      *ǯ��
           03  KOHSKY-AGE                 PIC  9(03).
      *��˴��ʬ
           03  KOHSKY-DEATHKBN            PIC  X(01).
      *���Ųʾ���
           03  KOHSKY-SRYKAINF            OCCURS  20.
      *���Ų�
               05  KOHSKY-SRYKA           PIC  X(02).
      *���ݸ�����
           03  KOHSKY-HKN.
      *���ݸ����ݸ��ɣ�
               05  KOHSKY-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  KOHSKY-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  KOHSKY-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  KOHSKY-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  KOHSKY-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  KOHSKY-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  KOHSKY-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  KOHSKY-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  KOHSKY-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  KOHSKY-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  KOHSKY-CERTNUM         PIC  X(03).
      *Ϸ���ݷ����
           03  KOHSKY-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  KOHSKY-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  KOHSKY-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  KOHSKY-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  KOHSKY-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  KOHSKY-RJNJKYSNUM      PIC  X(07).
      *�������
           03  KOHSKY-KOHINF              OCCURS  4.
      *������ݸ��ɣ�
               05  KOHSKY-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  KOHSKY-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  KOHSKY-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  KOHSKY-KOHFTNJANUM     PIC  X(20).
      *����ݼ�����ֹ�
               05  KOHSKY-KOHJKYSNUM      PIC  X(20).
      *�ݸ��ȹ礻�ֹ����
           03  KOHSKY-HKNCOMBIINF         OCCURS  10.
      *�ݸ��ȹ礻�ֹ�
               05  KOHSKY-HKNCOMBI        PIC  9(04).
      *���ݸ��ֹ�
               05  KOHSKY-HKNNUMHC        PIC  X(03).
      *�ȹ礻�����ݸ��ֹ����
               05  KOHSKY-KOHNUMHCINF     OCCURS  4.
      *�ȹ礻�����ݸ��ֹ�
                   07  KOHSKY-KOHNUMHC    PIC  X(03).
      *���Ŷ�ʬ����������
           03  KOHSKY-SRYKBNINF           OCCURS  50.
      *��������
               05  KOHSKY-KHNTEN          PIC  9(08).
      *���
               05  KOHSKY-KAISU           PIC  9(05).
      *������������
               05  KOHSKY-TENSUINF        OCCURS  5.
      *��������
                   07  KOHSKY-TENSU       PIC S9(08).
      *����������
           03  KOHSKY-JNISSUINF           OCCURS  5.
      *������
               05  KOHSKY-JNISSU          PIC  9(02).
      *�����������
           03  KOHSKY-TOTALTENINF         OCCURS  5.
      *�������
               05  KOHSKY-TOTALTEN        PIC S9(08).
      *��ô��۾���
           03  KOHSKY-FTNMONEYINF         OCCURS  5.
      *��ô���
               05  KOHSKY-FTNMONEY        PIC S9(09).
      *���ް�����ô�����
           03  KOHSKY-YKZFTNINF           OCCURS  5.
      *���ް�����ô��
               05  KOHSKY-YKZFTN          PIC  9(05).
      *�������������
           03  KOHSKY-SHOKUJIINF          OCCURS  5.
      *��������
               05  KOHSKY-SHOKUJINISSU    PIC  9(02).
      *����������
               05  KOHSKY-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  KOHSKY-SHOKUJIFTN      PIC  9(09).
      *������ô�⡦����������ɸ����ô�۶�ʬ
           03  KOHSKY-SHOKUJIFTNKBN       PIC  X(01).
      *��ǻ��ֶ�ʬ
           03  KOHSKY-SHOSHINTIME         PIC  X(01).
      *�������硦���������ե饰
           03  KOHSKY-ZAITAKU             PIC  X(01).
      *�������硦��������������ô��Ʒ�
           03  KOHSKY-ZAITAKU-SAIKEI      PIC S9(09).
      *����ǯ��
           03  KOHSKY-SKYYM               PIC  X(06).
      *����ǯ��
           03  KOHSKY-HENREIYM            PIC  X(06).
      *�����ʬ
           03  KOHSKY-SKYKBN              PIC  X(01).
      *������ʬ
           03  KOHSKY-TEISEIKBN           PIC  X(01).
      *
      *�ե饰
           03  KOHSKY-FLGINF.
               05  KOHSKY-FLG             PIC  9(01)   OCCURS  5.
      *��Ͽǯ����
           03  KOHSKY-CREYMD              PIC  X(08).
      *����ǯ����
           03  KOHSKY-UPDYMD              PIC  X(08).
      *������ݸ��ֹ渡���ѹ���
           03  KOHSKY-XXKOHNUM-G.  
               05  KOHSKY-XXKOHNUM        PIC X(03)   OCCURS  4.  