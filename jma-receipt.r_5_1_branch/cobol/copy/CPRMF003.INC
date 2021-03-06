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
      *    ���ݡ�����ɽ�����ѥե�����  COPY (CPRMF003.INC)       *
      *          KEY �ʳ���CPRCF002 ��Ʊ��
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF03-REC.
           03  MF03-KEY.
      *�ݸ����ֹ�        
               05  MF03-HKNJANUM-KEY    PIC  X(08).
      *���ŵ��أɣ�
               05  MF03-HOSPID          PIC  X(24).
      *����ǯ��
               05  MF03-SRYYM           PIC  9(06).
      *������ʬ
               05  MF03-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  MF03-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  MF03-RECEKA          PIC  X(02).
      *������ʧ����
               05  MF03-TEISYUTUSAKI    PIC  9(01).
      *�쥻�ץȼ���
               05  MF03-RECESYUBETU     PIC  9(04).
      *Ŭ�ѳ���ǯ����
               05  MF03-TEKSTYMD        PIC  X(08). 
      *�����ֹ�
           03  MF03-PTNUM               PIC  X(20).
      *���Ųʾ���
           03  MF03-SRYKAINF            OCCURS  10.
      *���Ų�
               05  MF03-SRYKA           PIC  X(02).
      *���ʻ�̾
           03  MF03-KANANAME            PIC  X(100).
      *������̾
           03  MF03-NAME                PIC  X(100).
      *����
           03  MF03-SEX                 PIC  X(01).
      *��ǯ����
           03  MF03-BIRTHDAY            PIC  9(08).
      *��˴��ʬ
           03  MF03-DEATHKBN            PIC  X(01).
      *���ݸ�����
           03  MF03-HKN.
      *���ݸ����ݸ��ɣ�
               05  MF03-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  MF03-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  MF03-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  MF03-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  MF03-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  MF03-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  MF03-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  MF03-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF03-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF03-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  MF03-CERTNUM         PIC  X(03).
      *Ϸ���ݷ����
           03  MF03-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  MF03-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  MF03-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  MF03-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  MF03-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  MF03-RJNJKYSNUM      PIC  X(07).
      *�������
           03  MF03-KOHINF              OCCURS  4.
      *������ݸ��ɣ�
               05  MF03-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  MF03-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  MF03-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  MF03-KOHFTNJANUM     PIC  X(08).
      *����ݼ�����ֹ�
               05  MF03-KOHJKYSNUM      PIC  X(20).
      *�����Ǥ�յ��ն�ʬ
               05  MF03-KOHNINIKYUKBN   PIC  X(01).
      *����ݷ��
               05  MF03-KOHKENSU        PIC  9(03).
      *����ݥ쥻�ץ������ʬ
               05  MF03-RECESKYKBN      PIC  X(01).
      *�ݸ��ȹ礻�ֹ����
           03  MF03-HKNCOMBIINF         OCCURS  3.
      *�ݸ��ȹ礻�ֹ�
               05  MF03-HKNCOMBI        PIC  9(04).
      *���ݸ��ֹ�
               05  MF03-HKNNUMHC        PIC  X(03).
      *�ȹ礻�����ݸ��ֹ����
               05  MF03-KOHNUMHCINF     OCCURS  4.
      *�ȹ礻�����ݸ��ֹ�
                   07  MF03-KOHNUMHC    PIC  X(03).
      *�õ��������
           03  MF03-TOKKIINF            OCCURS  5.
      *�õ�����
               05  MF03-TOKKI           PIC  X(02).
      *�����ʬ����
           03  MF03-BYOTOKBNINF         OCCURS  4.
      *�����ʬ
               05  MF03-BYOTOKBN        PIC  X(02).
      *����������
           03  MF03-JNISSUINF           OCCURS  5.
      *������
               05  MF03-JNISSU          PIC  9(02).
      *���Ŷ�ʬ����������
           03  MF03-SRYKBNINF           OCCURS  50.
      *��������
               05  MF03-KHNTEN          PIC  9(08).
      *���
               05  MF03-KAISU           PIC  9(05).
      *������������
               05  MF03-TENSUINF        OCCURS  5.
      *��������
                   07  MF03-TENSU       PIC  9(08).
      *�����������
           03  MF03-TOTALTENINF         OCCURS  5.
      *�������
               05  MF03-TOTALTEN        PIC  9(08).
      *��ô��۾���
           03  MF03-FTNMONEYINF         OCCURS  5.
      *��ô���
               05  MF03-FTNMONEY        PIC  9(09).
      *Ϸ�͸��ȶ�ʬ
           03  MF03-RJNGENMENKBN        PIC  X(01).
      *Ϸ�ͳ�����
           03  MF03-RJNGAIFTN           PIC  9(09).
      *Ϸ���������
           03  MF03-RJNNYUFTN           PIC  9(09).
      *���ݸ��ȶ�ʬ
           03  MF03-KKHGENMENKBN        PIC  X(01).
      *���ݸ��۳��
           03  MF03-KKHGENGAKURATE      PIC  X(03).
      *���ݸ��۶��
           03  MF03-KKHGENGAKU          PIC  X(03).
      *�������ñ��
           03  MF03-DISCOUNTTENTNK      PIC  X(02).
      *�������������
           03  MF03-SHOKUJIINF          OCCURS  5.
      *��������
               05  MF03-SHOKUJINISSU    PIC  9(02).
      *����������
               05  MF03-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF03-SHOKUJIFTN      PIC  9(09).
      *ɸ����ô��ʬ
           03  MF03-SHOKUJIFTNKBN       PIC  X(01).
      *Ϸ�͸�����ô��۾���
           03  MF03-RJNKOHFTNINF        OCCURS  3.
      *Ϸ�ͳ��������ô��
               05  MF03-RJNGAIKOHFTN    PIC  9(09).
      *Ϸ������������ô��
               05  MF03-RJNNYUKOHFTN    PIC  9(09).
      *���ް�����ô�����
           03  MF03-YKZFTNINF           OCCURS  5.
      *���ް�����ô��
               05  MF03-YKZFTN          PIC  9(05).
      *
