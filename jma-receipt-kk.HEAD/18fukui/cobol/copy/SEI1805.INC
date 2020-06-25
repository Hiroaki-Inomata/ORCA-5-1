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
      *          ����������� COPY (SEI1805.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2007/10/05   ���쥻ver4.0�б�
      *  01.00.02    ����      2007/10/24   �����ѹ�
      *  01.00.03    ����      2008/02/26   �����ѹ�
      *  01.00.04    ����      2008/05/16   H20ǯ4������б�
      *  01.00.05    ����      2011/12/16   Ʊ����Ǽ�����ֹ椬�Ѥ�äƤ�
      *                                     Ģɼ��ȿ�Ǥ���ʤ��Τ���
      *  01.00.06    ����      2012/10/25   ���ٽ�ե�����򿷵������Τ�������ɲ�
      *  01.00.07    ����      2015/11/11   ���Ῡ����ô�ɲ�
      *  01.00.08    ����      2018/11/07   �������򥳥��Ⱥ��
      ************************************************************
      *01  MF110-REC.
           03  MF110-KEY.
      *���ŵ��أɣ�
               05  MF110-HOSPNUM         PIC  9(02).
      *����ɽ��ʬ
               05  MF110-TENHYOKBN       PIC  X(01).
      *��Į¼������
               05  MF110-CITYCD          PIC  X(02).
      *��Į¼̾��
               05  MF110-CITYNAME        PIC  X(10).
      *������ʬ
               05  MF110-NYUGAIKBN       PIC  X(01).
      *����ǯ��
               05  MF110-SKYYM           PIC  X(06).
      *����ݼ�����ֹ�
               05  MF110-JKYSNUM         PIC  X(20).
      *������ݸ��ֹ�
               05  MF110-KOHHKNNUM       PIC  X(03).
      *�������ô���ֹ�
               05  MF110-FTNJANUM        PIC  X(08).
      *�����ֹ�
               05  MF110-PTNUM           PIC  X(20).
      *����ǯ��
               05  MF110-SRYYM           PIC  X(06).
      *�ݸ����ֹ�        
               05  MF110-HKNJANUM-KEY    PIC  X(08).
      *Ŭ�ѳ���ǯ����
               05  MF110-TEKSTYMD        PIC  X(08). 
      *���Ų�
               05  MF110-SRYKA           PIC  X(02).
      *���ԣɣ�
               05  MF110-PTID            PIC  9(10).
      *Ĺ����ʬ
               05  MF110-CHOKIKBN        PIC  9(01).
      *������̾
           03  MF110-NAME                PIC  X(100).
      *���ʻ�̾
           03  MF110-KANANAME            PIC  X(50).
      *����
           03  MF110-SEX                 PIC  X(01).
      *���ݶ�ʬ
           03  MF110-HONHUYOKBN          PIC  X(01).
      *��ǯ����
           03  MF110-BIRTHDAY            PIC  9(08).
      *�쥻�ץ��ܿͲ�²��ʬ
           03  MF110-RECEHONKZKKBN       PIC  X(01).
      *���ݸ�����
           03  MF110-HKN.
      *���ݸ����ݸ��ɣ�
               05  MF110-HKNID           PIC  9(10).
      *���ݸ����ݸ��ֹ�
               05  MF110-HKNNUM          PIC  X(03).
      *���ݸ���ˡ���ֹ�
               05  MF110-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  MF110-HKNJANUM        PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  MF110-HONKZKKBN       PIC  X(01).
      *���ݸ��������ʬ
               05  MF110-HOJOKBN         PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  MF110-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  MF110-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  MF110-NUM             PIC  X(80).
      *���ݸ��ݵ��ճ��
               05  MF110-KYURATE         PIC  9(03).
      *���ݸ��ݾ������ֹ�
               05  MF110-CERTNUM         PIC  X(03).
      *Ϸ���ݷ����
           03  MF110-RJNHKN.
      *Ϸ���ݸ����ݸ��ɣ�
               05  MF110-RJNHKNID        PIC  9(10).
      *Ϸ���ݸ����ݸ��ֹ�
               05  MF110-RJNHKNNUM       PIC  X(03).
      *Ϸ���ݸ���ˡ���ֹ�
               05  MF110-RJNHBTNUM       PIC  X(02).
      *Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  MF110-RJNFTNJANUM     PIC  X(08).
      *Ϸ���ݸ��ݼ�����ֹ�
               05  MF110-RJNJKYSNUM      PIC  X(07).
      *�������
           03  MF110-KOHINF.
      *������ݸ��ɣ�
               05  MF110-KOHID           PIC  9(10).
      *������ݸ��ֹ�
               05  MF110-KOHNUM          PIC  X(03).
      *�����ˡ���ֹ�
               05  MF110-KOHHBTNUM       PIC  X(02).
      *�������ô���ֹ�
               05  MF110-KOHFTNJANUM     PIC  X(20).
      *����ݼ�����ֹ�
               05  MF110-KOHJKYSNUM      PIC  X(20).
      *�����Ǥ�յ��ն�ʬ
               05  MF110-KOHNINIKYUKBN   PIC  X(01).
      *����ݷ��
               05  MF110-KOHKENSU        PIC  9(03).
      *����ݥ쥻�ץ������ʬ
               05  MF110-RECESKYKBN      PIC  X(01).
      *����������
           03  MF110-JNISSUINF.
      *������
               05  MF110-JNISSU          PIC  9(02).
      *�����������
           03  MF110-TOTALTENINF.
      *�������
               05  MF110-TOTALTEN        PIC  9(08).
      *��ô��۾���
           03  MF110-FTNMONEYINF.
      *��ô���
               05  MF110-FTNMONEY        PIC  9(09).
      *����¾��ô���
           03  MF110-ETCFTNMONEYINF.
               05  MF110-ETCFTNSU        PIC  9(03).
               05  MF110-ETCFTNMONEY     PIC  9(09).
      *�������������
           03  MF110-SHOKUJIINF.
      *�������
               05  MF110-SHOKUJIKAISU    PIC  9(02).
      *����������
               05  MF110-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF110-SHOKUJIFTN      PIC  9(09).
      *���ް�����ô�����
           03  MF110-YKZFTNINF           OCCURS  2.
      *���ް�����ô��
               05  MF110-YKZFTN         PIC  9(05).
      *������ô��
           03  MF110-ITBFTN		PIC  9(06).
      *������ô��
           03  MF110-KOHFTN		PIC  9(06).
      *��ˡ���������
           03  MF110-KESE		PIC  9(06).
      *ʡ����Ű�����ô
           03  MF110-FKSFTN             PIC  9(06).
      *�񡡸������
           03  MF110-RECEKOHINF           OCCURS  2.
      *�񡡸�����ݸ��ɣ�
               05  MF110-RECEKOHID           PIC  9(10).
      *�񡡸�����ݸ��ֹ�
               05  MF110-RECEKOHNUM          PIC  X(03).
      *�񡡸����ˡ���ֹ�
               05  MF110-RECEKOHHBTNUM       PIC  X(02).
      *�񡡸������ô���ֹ�
               05  MF110-RECEKOHFTNJANUM     PIC  X(20).
      *�񡡸���ݼ�����ֹ�
               05  MF110-RECEKOHJKYSNUM      PIC  X(20).
      *�񡡸���ݷ��
               05  MF110-RECEKOHKENSU        PIC  9(03).
      *�񡡸���ݥ쥻�ץ������ʬ
               05  MF110-RECERECESKYKBN      PIC  X(01).
      *�񡡼���������
           03  MF110-RECEJNISSUINF           OCCURS  2.
      *�񡡼�����
               05  MF110-RECEJNISSU          PIC  9(02).
      *�񡡹����������
           03  MF110-RECETOTALTENINF           OCCURS  2.
      *�񡡹������
               05  MF110-RECETOTALTEN        PIC  9(08).
      *����ô��۾���
           03  MF110-RECEFTNMONEYINF           OCCURS  2.
      *����ô���
               05  MF110-RECEFTNMONEY        PIC  9(09).
      *�񡡿������������
           03  MF110-RECESHOKUJIINF           OCCURS  2.
      *�񡡿������
               05  MF110-RECESHOKUJIKAISU    PIC  9(02).
      *�񡡿���������
               05  MF110-RECESHOKUJIRYOYOHI  PIC  9(09).
      *��ɸ����ô��
               05  MF110-RECESHOKUJIFTN      PIC  9(09).
      *�ե饰����
           03  MF110-FLGINF                  OCCURS  5.
      *�ե饰
               05  MF110-FLG                 PIC  9(01).
      *���������
           03  MF110-SKY-SHOKUJIFTN          PIC  9(09).